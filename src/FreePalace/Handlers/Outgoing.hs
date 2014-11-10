{-# LANGUAGE DoRec #-}
module FreePalace.Handlers.Outgoing where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import qualified System.Log.Logger                                 as Log
import qualified FRP.Sodium                                        as FRP

import qualified FreePalace.Domain.Chat                            as Chat
import qualified FreePalace.Domain.GUI                             as GUI
import qualified FreePalace.Domain.Net                             as Net
import qualified FreePalace.Domain.State                           as State
import qualified FreePalace.Domain.User                            as User
import qualified FreePalace.Handlers.Incoming                      as Incoming
import qualified FreePalace.Media.Loader                           as MediaLoader
import qualified FreePalace.Messages.Inbound                       as InboundMessages
import qualified FreePalace.Messages.PalaceProtocol.InboundReader  as PalaceInbound
import qualified FreePalace.Messages.PalaceProtocol.OutboundWriter as PalaceOutbound
import qualified FreePalace.Net.PalaceProtocol.Connect             as Connect

data GUIEventHandlers = GUIEventHandlers {
  handleUserTextEntry :: IO ()
}

guiEventHandlers :: State.Connected -> GUIEventHandlers
guiEventHandlers clientState = GUIEventHandlers {
  handleUserTextEntry = speak clientState
}

bindHandlers :: State.Connected -> GUIEventHandlers -> IO ()
bindHandlers State.Connected { State.guiState = guiComponents } guiHandlers =
  do
    bindUserTextEntry guiComponents $ handleUserTextEntry guiHandlers
    return ()

bindUserTextEntry :: GUI.Components -> IO () -> IO ()
bindUserTextEntry guiComponents userTextEntryHandler =
  do
    let chatEntryField = GUI.chatEntry guiComponents
        chatSendButton = GUI.chatSend guiComponents
    GUI.onEnterKeyPress chatEntryField userTextEntryHandler
    GUI.onButtonClick chatSendButton userTextEntryHandler


  
-- TODO Once client is connected and disconnects, how does this function get the updated state for the next connection?
handleConnectRequested :: State.ClientState -> Net.Protocol -> Net.Hostname -> Net.PortId -> IO ()
handleConnectRequested clientState protocol host port =
  do
    disconnected <- disconnect clientState
    Log.debugM "Connection" $ "Connecting to " ++ host ++ ":" ++ port
    newState <- catch (State.ConnectedState <$> connect disconnected protocol host port)
                      (\(SomeException exception) ->
                        do
                          Log.errorM "Connection" $ show exception
                          return $ State.DisconnectedState disconnected)

    case newState of
     State.ConnectedState connectedState ->
       do
         stateFromMessageReceiverThread <- newEmptyMVar
         -- TODO Must bind disconnect and reconnect to something that will stop the forked process
         -- i.e., Control.Concurrent.killThread :: ThreadId -> IO ()
         -- TODO If this thread dies or an exception is thrown on it, need to handle the disconnect
         messageListenerThreadId <- forkIO $ Incoming.initializeMessageDispatcher stateFromMessageReceiverThread connectedState
         readyState <- takeMVar stateFromMessageReceiverThread
         bindHandlers readyState $ guiEventHandlers readyState         
         sendLogin readyState         
         GUI.closeDialog . GUI.connectDialog . State.guiState $ readyState
         return ()
     State.DisconnectedState disconnectedState ->
       do
         GUI.closeDialog . GUI.connectDialog . State.disconnectedGui $ disconnectedState  -- TODO Show connection error somehow or something in GUI indicating failure
         return ()

                         
outboundMain :: State.ClientState -> IO ()
outboundMain clientState protocol host port = do
  let guiComponents = case clientState of (State.DisconnectedState state) -> GUI.disconnectedGui state
                                          (State.ConnectedState    state) -> GUI.guiState state
  eGuiAction <- GUI.bindComponents guiComponents
  let eLogGui = fmap ApplicationLog.fromGuiAction eGuiAction
  sync $ FRP.listen eLogGui ApplicationLog.debugLog
    
  rec
    bState <- FRP.accum clientState eStateUpdate       -- Behavior State.ClientState. Only outbound events updating state are Disconnect and Connected.
                                                       -- WE SHOULD SEPARATE PROTOCOL STATE FROM THE REST. ONLY PROTOCOL MANIPULATED ON BOTH THREADS.
                                                       -- We read and manipulate GUI in this thread but don't maintain or manipulate its state.
    let eGuiActionWithState = snapshot (,) eGuiAction bState
        eDisconnected = handleGuiDisconnectSelected eGuiActionWithState
        eConnected = handleGuiConnectRequest eGuiActionWithState
        eMessageListenerStarted = handleStartingMessageListener eConnected

        eDisconnectedClientState = fmap (\disconnected -> State.DisconnectedState disconnected) eDisconnected
        eConnectedClientState = fmap (\connected -> State.ConnectedState connected) eConnected
        eStateUpdate = merge eDisconnectedState eConnectedState
        
    sync $ FRP.listen eMessageListenerStarted sendLogin  -- Need to pass userId        
    sync $ FRP.listen eStartingMessageListener startMessageReceiverThread  -- push the MVar in here. put event in MVar?

     -- timeoutEvent -- Do later. If disconnect/connect come back too late, throw them away. (Need an ID?)
    sync $ FRP.listen eStateUpdate [GUI...]

-- Log "connecting to host port"
-- Log "connected to host port"

-- For talk: (...userId..., ...selectedUserId..., ...messageText...)

                                              -- ConnectCancel event needs gui state
                                                     -- just needs to close GUI connect window
                                              -- Speak event needs connection, messageConverters, user, selected user, value in text box, 
                                              -- Login event needs connection, messageConverters, user

handleStartingMessageListener :: FRP.Event State.Connected -> FRP.Event State.Connected -- The resulting event has to have the userID that resulted from the handshake
handleStartingMessageListener eConnected =
  -- MessageListenerThreadId should go in the state
           messageListenerThreadId <- forkIO $ Incoming.initializeMessageDispatcher connectedState


handleGuiConnectRequest :: FRP.Event (GUI.GuiAction, State.ClientState) -> FRP.Event State.Connected
handleGuiConnectRequest eGuiActionWithState = do
  (eDisconnected, pushDisconnected) <- sync newEvent
  (eConnected, pushConnected) <- sync newEvent
  sync $ FRP.listen eDisconnectListenable disconnect
  sync $ FRP.listen eConnectListenable connect
  eConnected
  
  where eConnectRequest = FRP.filterE isConnectOkClicked eGuiActionWithState 
        eDisconnectListenable = fmap (\guiAction, clientState -> clientState, pushDisconnected) eConnectRequest 
        eConnectListenable = fmap (\disconnected -> disconnected, pushConnected) eDisconnected
        isConnectOkClicked (GUI.ConnectOkClicked, _) = True
        isConnectOkClicked _ = False
        
handleGuiDisconnectSelected :: FRP.Event (GUI.GuiAction, State.ClientState) -> FRP.Event State.Disconnected
handleGuiDisconnectSelected eGuiActionWithState = do
  (eDisconnected, pushDisconnected) <- sync newEvent
  sync $ FRP.listen eDisconnectListenable disconnect
  eDisconnected
  
  where eDisconnectRequest = FRP.filterE isDisconnectSelected eGuiActionWithState
        eDisconnectListenable = fmap (\guiAction, clientState -> clientState, pushDisconnected) eDisconnectRequest 
        isDisconnectSelected (GUI.DisconnectSelected, _) = True
        isDisconnectSelected _ = False
        
-- TODO State should include listener thread ID if available
disconnect :: (State.ClientState, (State.Disconnected -> FRP.Reactive ())) -> IO ()
disconnect (disconnected@(State.DisconnectedState _), pushDisconnected) = sync $ pushDisconnected disconnected
disconnect ((State.ConnectedState priorState), pushDisconnected) = do
  -- TODO end message processing thread if in the state
  case State.protocolState priorState of
    State.PalaceProtocolState connection _ -> Connect.disconnect connection -- This does not rethrow exceptions
  let disconnectedState = State.disconnectedStateFrom priorState
  sync $ pushDisconnected disconnectedState

connect :: (State.Disconnected, (State.Connected -> FRP.Reactive ()) -> IO () 
connect (priorState, pushConnected) =
  do
    let guiComponents = State.guiState priorState
    host <- textValue $ connectHostEntry guiComponents
    port <- textValue $ connectPortEntry guiComponents
    let protocol = State.protocol priorState host port
        newProtocolState = case protocol of
                             Net.PalaceProtocol -> do
                               connection <- Connect.connect host port
                               State.PalaceProtocolState connection Connect.defaultPalaceMessageConverters
        newState = State.initialConnectedState priorState newProtocolState host port
    sync $ pushConnected newState

sendLogin :: State.Connected -> IO ()
sendLogin clientState =
  do
    let userId = State.userId $ State.userState clientState
    Log.debugM "Outgoing.Login" "Sending login..."
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceOutbound.sendLogin connection messageConverters userId

-- TODO The handler has to see if 1) it's connected, 2) it's a client command, 3) it's a script call, 4) a user is selected (for whisper)
speak :: State.Connected -> IO ()
speak clientState =
  do
    let userId            = State.userId $ State.userState clientState
        selectedUser      = Nothing -- TODO - get selected user from ... ?
        textEntryField    = GUI.chatEntry $ State.guiState clientState
    messageText <- GUI.textValue textEntryField

    -- TODO check the initial character of the message for instructions
    let communication = Chat.Communication {
            Chat.speaker = userId
          , Chat.target = selectedUser
          , Chat.message = messageText
          , Chat.chatMode = Chat.Outbound
          }
    Log.debugM "Outgoing.Talk" (show communication)
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceOutbound.speak connection messageConverters communication
    GUI.clearTextEntry textEntryField
