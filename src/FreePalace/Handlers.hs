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
import qualified FreePalace.Media.Loader as MediaLoader

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
    Log.debugM "Incoming.Message.Await" $ "Awaiting messages with state: " ++ (show clientState)    
    header <- readHeader clientState
    Log.debugM "Incoming.Message.Header" (show header)
    newState <- case Messages.messageType header of
      -- Logon sequence after handshake received and logon sent
      Messages.AlternateLogonReply -> handleAlternateLogonReply clientState
      Messages.ServerVersion -> handleServerVersion clientState header
      Messages.ServerInfo -> handleServerInfo clientState header
      Messages.UserStatus -> handleUserStatus clientState header
      Messages.UserLoggedOnAndMax -> handleUserLogonNotification clientState header
      Messages.GotHttpServerLocation -> handleMediaServerInfo clientState header
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
    Log.debugM "Incoming.Message.Processed" $ "Message processed. New state: " ++ (show newState)
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
    let newState = State.withMediaServerInfo state serverInfo
    loadRoomBackgroundImage newState
    Log.debugM "Incoming.Message.HttpServerLocation.Processed" $ "New state: " ++ (show newState)
    return newState

 -- room name, background image, overlay images, props, hotspots, draw commands
handleRoomDescription :: State.Connected -> Messages.Header -> IO State.Connected
handleRoomDescription clientState@(State.Connected { State.protocolState = State.PalaceProtocolState connection messageConverters }) header =
  do
    (roomDescription, state) <- PalaceHandlers.handleRoomDescription clientState connection messageConverters header
    let newState = State.withRoomDescription state roomDescription
    loadRoomBackgroundImage newState
    Log.debugM "Incoming.Message.GotRoomDescription.Processed" $ "New state: " ++ (show newState)
    return newState
    
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

-- TODO Special types for media server URI? And Background image (we want various permutations of BG image for various media types)
-- TODO Do we want to reload this every time a new room description gets sent? How often does that happen?
loadRoomBackgroundImage :: State.Connected -> IO State.Connected
loadRoomBackgroundImage state =
  do
    let mediaServer = State.mediaServer $ State.hostState state
        roomState = State.currentRoomState . State.hostState $ state
        roomCanvas = GUI.roomCanvas $ State.guiState state
    Log.debugM "Load.BackgroundImage" $ "Media server url: " ++ (show mediaServer)
    Log.debugM "Load.BackgroundImage" $ "RoomState: " ++ (show roomState)
    case (mediaServer, roomState) of
     (Just mediaServerUrl, Just currentRoomState) ->
       do
         let imageName = State.roomBackgroundImageName currentRoomState
             host = State.hostname $ State.hostState state
             port = State.portId $ State.hostState state
         Log.debugM "Load.BackgroundImage" $ "Fetching background image " ++ imageName ++ " from " ++ mediaServerUrl
         possibleImagePath <- MediaLoader.fetchCachedBackgroundImagePath host port mediaServerUrl imageName
         case possibleImagePath of
           Just imagePath -> GUI.displayBackground roomCanvas imagePath
           Nothing -> return ()
         return state
     (_, _) -> return state 
