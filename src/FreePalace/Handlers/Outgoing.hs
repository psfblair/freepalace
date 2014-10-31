module FreePalace.Handlers.Outgoing where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import qualified System.Log.Logger                                 as Log

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
    disconnected <- disconnect clientState protocol
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

disconnect :: State.ClientState -> Net.Protocol -> IO State.Disconnected
disconnect (State.DisconnectedState disconnected) _ = return disconnected
disconnect (State.ConnectedState priorState) Net.PalaceProtocol =
  do
    case State.protocolState priorState of
      State.PalaceProtocolState connection _ -> Connect.disconnect connection -- This does not rethrow exceptions.
    return $ State.disconnectedStateFrom priorState

connect :: State.Disconnected -> Net.Protocol -> Net.Hostname -> Net.PortId -> IO State.Connected
connect priorState Net.PalaceProtocol host port =
  do
    connection <- Connect.connect host port
    let protocol = State.PalaceProtocolState connection Connect.defaultPalaceMessageConverters
    return $ State.initialConnectedState priorState protocol host port

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
