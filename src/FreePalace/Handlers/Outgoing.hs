{-# LANGUAGE RecursiveDo #-}
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
import qualified FreePalace.Logger                                 as ApplicationLog

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

outboundMain :: State.ClientState ->  IO ()
outboundMain clientState = do
  rec
    let guiComponents = case clientState of (State.DisconnectedState state) -> State.disconnectedGui state
                                            (State.ConnectedState    state) -> State.guiState state
    eGuiAction <- GUI.bindComponents guiComponents
    let eLogGui = fmap ApplicationLog.fromGuiAction eGuiAction
    FRP.sync $ FRP.listen eLogGui ApplicationLog.debugLog
    
    bState <- FRP.sync $ FRP.accum clientState eStateUpdate -- Only outbound events updating state are Disconnect and Connected.
              -- WE SHOULD SEPARATE PROTOCOL STATE FROM THE REST. ONLY PROTOCOL MANIPULATED ON BOTH THREADS.
              -- We read and manipulate GUI in this thread but don't maintain or manipulate its state.
    let eGuiActionWithState = FRP.snapshot (,) eGuiAction bState
        eDisconnected = handleGuiDisconnectSelected eGuiActionWithState
        eConnecting = handleGuiConnectRequest eGuiActionWithState
        
    eIncomingMessageReceived <- handleStartingMessageListener bState eConnecting
    
    let eConnected = FRP.filterE firstMessageAfterConnecting $ FRP.snapshot (\newState oldState -> oldState) eIncomingMessageReceived bState 
        eDisconnectedClientState = fmap State.DisconnectedState eDisconnected
        eConnectingClientState = fmap State.ConnectingState eConnecting
        eMessageReceivedClientState = fmap State.ConnectedState eIncomingMessageReceived
        eStateUpdate = FRP.merge eDisconnectedClientState $ FRP.merge  eConnectingClientState eMessageReceivedClientState

        firstMessageAfterConnecting (State.ConnectingState _) = True
        firstMessageAfterConnecting _ = False
        closeConnectDialog clientState = GUI.closeDialog . GUI.connectDialog $ State.guiStateFrom clientState

  FRP.sync $ FRP.listen eConnected sendLogin  -- Need to pass userId        
     -- TODO Issue timeoutEvent -- If disconnect/connect come back too late, throw them away. (Need an ID?)
     -- TODO How to handle exceptions in all this? Need
    -- eConnectFailure
  io <- FRP.sync $ FRP.listen eConnected closeConnectDialog
  io
-- Log "connecting to host port"
-- Log "connected to host port"

-- For talk: (...userId..., ...selectedUserId..., ...messageText...)

      -- ConnectCancel event needs gui state
      -- just needs to close GUI connect window
      -- Speak event needs connection, messageConverters, user, selected user, value in text box, 
      -- Login event needs connection, messageConverters, user
             
handleGuiConnectRequest :: FRP.Event (GUI.GuiAction, State.ClientState) -> FRP.Event State.Connecting
handleGuiConnectRequest eGuiActionWithState = do
  (eDisconnected, pushDisconnected) <- FRP.sync FRP.newEvent
  (eConnecting, pushConnecting) <- FRP.sync FRP.newEvent
  let eConnectRequest = FRP.filterE isConnectOkClicked eGuiActionWithState 
      eDisconnectListenable = fmap (\(guiAction, clientState) -> (clientState, pushDisconnected)) eConnectRequest 
      eConnectListenable = fmap (\disconnected -> (disconnected, pushConnecting)) eDisconnected
  FRP.sync $ FRP.listen eDisconnectListenable disconnect
  FRP.sync $ FRP.listen eConnectListenable connect
  eConnecting
  where isConnectOkClicked (GUI.ConnectOkClicked, _) = True
        isConnectOkClicked _ = False

handleStartingMessageListener :: FRP.Behavior State.ClientState -> FRP.Event State.Connecting -> IO (FRP.Event State.Connected)
handleStartingMessageListener bState eConnecting = do
  (eIncomingMessage, pushIncomingMessage) <- FRP.sync FRP.newEvent --TODO Where does eIncomingMessage get dealt with?
  messageListenerThreadId <- forkIO $ Incoming.initializeMessageDispatcher bState pushIncomingMessage
  let initialConnectedState threadId connectingState =
        State.initialConnectedState connectingState threadId host port
        where (host, port) = State.connectingHostAddress connectingState
  return $ fmap (initialConnectedState messageListenerThreadId) eConnecting
        
handleGuiDisconnectSelected :: FRP.Event (GUI.GuiAction, State.ClientState) -> FRP.Event State.Disconnected
handleGuiDisconnectSelected eGuiActionWithState = do
  (eDisconnected, pushDisconnected) <- FRP.sync FRP.newEvent
  let eDisconnectRequest = FRP.filterE isDisconnectSelected eGuiActionWithState
      eDisconnectListenable = fmap (\(guiAction, clientState) -> (clientState, pushDisconnected)) eDisconnectRequest 
  FRP.sync $ FRP.listen eDisconnectListenable disconnect
  eDisconnected
  where isDisconnectSelected (GUI.DisconnectSelected, _) = True
        isDisconnectSelected _ = False
        
disconnect :: (State.ClientState, State.Disconnected -> FRP.Reactive ()) -> IO ()
disconnect (disconnected, pushDisconnected) =
  FRP.sync $ pushDisconnected (State.disconnectedStateFrom disconnected)
disconnect (connecting@(State.ConnectingState priorState), pushDisconnected) = do
  case State.connectingProtocolState priorState of
    State.PalaceProtocolState connection _ -> Connect.disconnect connection -- This does not rethrow exceptions
  FRP.sync $ pushDisconnected (State.disconnectedStateFrom connecting)
disconnect (connected@(State.ConnectedState priorState), pushDisconnected) = do
  -- TODO end message processing thread if in the state
  case State.protocolState priorState of
    State.PalaceProtocolState connection _ -> Connect.disconnect connection -- This does not rethrow exceptions
  FRP.sync $ pushDisconnected (State.disconnectedStateFrom connected)

connect :: (State.Disconnected, State.Connecting -> FRP.Reactive ()) -> IO () 
connect (priorState, pushConnected) = do
  let guiComponents = State.guiState priorState
  host <- GUI.textValue $ GUI.connectHostEntry guiComponents
  port <- GUI.textValue $ GUI.connectPortEntry guiComponents
  let protocol = State.protocolFor priorState host port
      newProtocolState = case protocol of
                           Net.PalaceProtocol -> do
                             connection <- Connect.connect host port
                             State.PalaceProtocolState connection Connect.defaultPalaceMessageConverters
      newState = State.connectingStateFor priorState newProtocolState host port
  FRP.sync $ pushConnected newState

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
