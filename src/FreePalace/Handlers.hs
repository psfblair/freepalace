module FreePalace.Handlers where

import System.IO
import qualified System.Log.Logger as Log
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent
import Control.Exception

import qualified FreePalace.State as State
import qualified FreePalace.Messages as Messages
import qualified FreePalace.Messages.PalaceProtocol.Outbound as PalaceOutbound
import qualified FreePalace.Messages.PalaceProtocol.Inbound as Inbound
import qualified FreePalace.Net.Receive as Recv
import qualified FreePalace.Net.Send as Send
import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Net as Netcom
import qualified FreePalace.Net.Types as Net
import qualified FreePalace.Handlers.PalaceProtocol as PalaceHandlers

data GUIEventHandlers = GUIEventHandlers {
  handleUserTextEntry :: IO ()
}

guiEventHandlers :: State.Connected -> GUIEventHandlers
guiEventHandlers clientState = GUIEventHandlers {
  handleUserTextEntry = speak clientState
}

bindHandlers :: State.Connected -> GUIEventHandlers -> IO ()
bindHandlers State.Connected { State.guiState = guiComponents }  guiEventHandlers =
  do
    bindUserTextEntry guiComponents $ handleUserTextEntry guiEventHandlers
    return ()

bindUserTextEntry :: GUI.Components -> IO () -> IO ()
bindUserTextEntry guiComponents userTextEntryHandler =
  do
    let chatEntryField = GUI.chatEntry guiComponents
        chatSendButton = GUI.chatSend guiComponents
    GUI.onEnterKeyPress chatEntryField userTextEntryHandler
    GUI.onButtonClick chatSendButton userTextEntryHandler

-- TODO Once client is connected and disconnects, how does this function get the updated state for the next connection?
handleConnectRequested :: State.ClientState -> State.Protocol -> Net.Hostname -> Net.PortId -> IO ()
handleConnectRequested clientState protocol host port =
  do
    let connectAttempt =
          do
            disconnectedState <- Netcom.disconnect clientState
            connectedState    <- Netcom.connect disconnectedState protocol host port 
            readyState        <- handleHandshake connectedState
            -- TODO Must bind disconnect and reconnect to something that will stop the forked process
            -- i.e., Control.Concurrent.killThread :: ThreadId -> IO ()
            -- TODO If this thread dies or an exception is thrown on it, need to handle the disconnect
            messageListenerThreadId <- forkIO $ dispatchIncomingMessages readyState
            bindHandlers readyState $ guiEventHandlers readyState
            sendLogin readyState
            GUI.closeDialog . GUI.connectDialog . State.guiState $ readyState
    catch connectAttempt (\(SomeException exception) ->
                           do 
                             Log.errorM "Connection" $ show exception
                             -- TODO Provide some way in the GUI to indicate connection failed.
                             -- TODO State monad - grab the actual current state, not where it started out
                             disconnectedState <- Netcom.disconnect clientState
                             GUI.closeDialog . GUI.connectDialog . State.disconnectedGui $ disconnectedState
                             return ())

handleHandshake :: State.Connected -> IO State.Connected
handleHandshake clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) =
  PalaceHandlers.handleHandshake clientState connection messageConverters

sendLogin :: State.Connected -> IO ()
sendLogin clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) =
  do
    let userId = State.userId $ State.userState clientState
    PalaceHandlers.sendLogin connection messageConverters userId

-- TODO The handler has to see if 1) it's connected, 2) it's a client command, 3) it's a script call, 4) a user is selected (for whisper)
speak :: State.Connected -> IO ()
speak clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) =
  do
    let userId            = State.userId $ State.userState clientState
        selectedUser      = Nothing -- TODO - get selected user from ... ?
        textEntryField    = GUI.chatEntry $ State.guiState clientState
    messageText <- GUI.textValue textEntryField
    PalaceHandlers.speak connection messageConverters userId messageText selectedUser
    GUI.clearTextEntry textEntryField

-- TODO Handle connection loss
dispatchIncomingMessages :: State.Connected -> IO ()
dispatchIncomingMessages clientState =
  do
    header <- readHeader clientState
    Log.debugM "Incoming.Message.Header" (show header)
    newState <- case Messages.messageType header of
      -- Logon sequence after handshake received and logon sent
      Messages.AlternateLogonReply -> handleAlternateLogonReply clientState
      Messages.ServerVersion -> handleServerVersion clientState header
      Messages.ServerInfo -> handleServerInfo clientState header
      Messages.UserStatus -> handleUserStatus clientState header
      Messages.UserLoggedOnAndMax -> handleUserLogonNotification clientState header
      Messages.GotHttpServerLocation -> handleMediaServerInfo clientState header >> return clientState -- TODO need State monad!
      Messages.GotRoomDescription -> handleRoomDescription clientState header
      Messages.GotUserList -> handleUserList clientState header
      Messages.RoomDescend -> return clientState -- this message just means we're done receiving the room description & user list
      Messages.UserNew -> handleNewUserNotification clientState header
      -- End logon sequence
      
      Messages.Talk -> handleTalk clientState header Messages.TalkAloud
      Messages.CrossRoomWhisper -> handleTalk clientState header Messages.Whispering
      Messages.Say -> handleEncodedTalk clientState header Messages.TalkAloud
      Messages.Whisper -> handleEncodedTalk clientState header Messages.Whispering
      Messages.Move -> handleMovement clientState header
      _ -> handleUnknownMessage clientState header
    dispatchIncomingMessages newState

readHeader :: State.Connected -> IO Messages.Header
readHeader State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters } =
  PalaceHandlers.readHeader connection messageConverters        


{- OpenPalace comments say:
  This is only sent when the server is running in "guests-are-members" mode.
  This is pointless... it's basically echoing back the logon packet that we sent to the server.
  The only reason we support this is so that certain silly servers can change our puid and ask
  us to reconnect "for security reasons."
-}
handleAlternateLogonReply :: State.Connected -> IO State.Connected  -- TODO return puidCrc and puidCounter when we need it
handleAlternateLogonReply clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) =
  PalaceHandlers.handleAlternateLogonReply clientState connection messageConverters        

handleServerVersion :: State.Connected -> Messages.Header -> IO State.Connected
handleServerVersion clientState header =
  do
    Log.debugM "Incoming.Message.ServerVersion" ("Server version: " ++ (show $ Messages.messageRefNumber header))
    return clientState -- TODO Add server version to host state

handleServerInfo :: State.Connected -> Messages.Header -> IO State.Connected
handleServerInfo clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleServerInfo clientState connection messageConverters header 
  
handleUserStatus :: State.Connected -> Messages.Header -> IO State.Connected
handleUserStatus clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleUserStatus clientState connection messageConverters header 

handleUserLogonNotification :: State.Connected -> Messages.Header -> IO State.Connected
handleUserLogonNotification clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  do
    let gui = State.guiState clientState
    (message, state) <- PalaceHandlers.handleUserLogonNotification clientState connection messageConverters refIdToUserIdMapping header
    GUI.appendMessage (GUI.logWindow gui) $ Messages.makeRoomAnnouncement message
    return state
    
handleMediaServerInfo :: State.Connected -> Messages.Header -> IO State.Connected
handleMediaServerInfo clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  do
    (serverInfo, state) <- PalaceHandlers.handleMediaServerInfo clientState connection header
    return $ State.withMediaServerInfo state serverInfo

 -- room name, background image, overlay images, props, hotspots, draw commands
handleRoomDescription :: State.Connected -> Messages.Header -> IO State.Connected
handleRoomDescription clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleRoomDescription clientState connection messageConverters header

handleUserList :: State.Connected -> Messages.Header -> IO State.Connected
handleUserList clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleUserList clientState connection header

handleNewUserNotification :: State.Connected -> Messages.Header -> IO State.Connected
handleNewUserNotification clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleNewUserNotification clientState connection header

handleTalk :: State.Connected -> Messages.Header -> Messages.ChatMode -> IO State.Connected
handleTalk clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header mode =
  do
    let gui = State.guiState clientState
    (chat, state) <- PalaceHandlers.handleTalk clientState connection refIdToUserIdMapping header mode  -- TODO get mapping out of global state
    GUI.appendMessage (GUI.logWindow gui) chat
    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    return state
    
handleEncodedTalk :: State.Connected -> Messages.Header -> Messages.ChatMode -> IO State.Connected
handleEncodedTalk clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header mode =
  do
    let gui = State.guiState clientState
    -- TODO get user mapping out of global state
    (chat, state) <- PalaceHandlers.handleEncodedTalk clientState connection messageConverters refIdToUserIdMapping header mode
    GUI.appendMessage (GUI.logWindow gui) chat
    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    return state
    
handleMovement :: State.Connected -> Messages.Header -> IO State.Connected
handleMovement clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleMovement clientState connection messageConverters refIdToUserIdMapping header

handleUnknownMessage :: State.Connected -> Messages.Header -> IO State.Connected
handleUnknownMessage  clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  PalaceHandlers.handleUnknownMessage clientState connection header



-- TODO This needs to live in the global state
refIdToUserIdMapping :: Map.Map Int Messages.UserId
refIdToUserIdMapping = Map.fromList [ (0, Messages.roomAnnouncementUserId) ]


type MediaServer = String
type BackgroundImage = String
loadRoomBackgroundImage :: MediaServer -> BackgroundImage -> IO ()
loadRoomBackgroundImage mediaServer backgroundImage =
  -- width="{backgroundImage.width}" height="{backgroundImage.height}"	minWidth="512" minHeight="384"
  -- HTTP request the image : judge success or failure -- try again if fail
  -- if success -- read the image in its native format 
  -- pass the result to the GUI which will have to know how to paint it from that format (Gtk-specific)
  -- NOTE if this is browser-based then we can just set the image source... can we abstract this?

  return ()
