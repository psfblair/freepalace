module FreePalace.Handlers where

import           Control.Concurrent
import qualified Data.Map                           as Map
import qualified System.Log.Logger                  as Log

import qualified FreePalace.Domain                  as Domain
import qualified FreePalace.GUI.Types               as GUI
import qualified FreePalace.Handlers.PalaceProtocol as PalaceHandlers
import qualified FreePalace.Messages.PalaceProtocol.Inbound as PalaceInbound
import qualified FreePalace.Handlers.State          as StateHandlers
import qualified FreePalace.Handlers.Types          as HandlerTypes
import qualified FreePalace.Media.Loader            as MediaLoader
import qualified FreePalace.Messages                as Messages
import qualified FreePalace.Net                     as Net
import qualified FreePalace.State                   as State

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
    newState <- case protocol of
      Net.PalaceProtocol -> PalaceHandlers.handleConnectRequested clientState host port
    case newState of
     State.DisconnectedState disconnected ->
       do
         GUI.closeDialog . GUI.connectDialog . State.disconnectedGui $ disconnected  -- TODO Show connection error somehow or something in GUI indicating failure
         return ()
     State.ConnectedState connected ->
       do
         readyState <- handleHandshake connected
         -- TODO Must bind disconnect and reconnect to something that will stop the forked process
         -- i.e., Control.Concurrent.killThread :: ThreadId -> IO ()
         -- TODO If this thread dies or an exception is thrown on it, need to handle the disconnect
         messageListenerThreadId <- forkIO $ dispatchIncomingMessages readyState
         bindHandlers readyState $ guiEventHandlers readyState
         sendLogin readyState
         GUI.closeDialog . GUI.connectDialog . State.guiState $ readyState
         return ()

handleHandshake :: State.Connected -> IO State.Connected
handleHandshake clientState =
  do
    handshakeData <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readHandshake connection messageConverters
    Log.debugM "Incoming.Handshake" (show handshakeData)
    let currentUserState = State.userState clientState
        protocolInfo = HandlerTypes.protocolInfo handshakeData
        userRefId = HandlerTypes.userRefId handshakeData
        newState = StateHandlers.withUserRefId clientState userRefId
    return $ StateHandlers.withProtocol newState protocolInfo

sendLogin :: State.Connected -> IO ()
sendLogin clientState =
  do
    let userId = State.userId $ State.userState clientState
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceHandlers.sendLogin connection messageConverters userId

-- TODO The handler has to see if 1) it's connected, 2) it's a client command, 3) it's a script call, 4) a user is selected (for whisper)
speak :: State.Connected -> IO ()
speak clientState =
  do
    let userId            = State.userId $ State.userState clientState
        selectedUser      = Nothing -- TODO - get selected user from ... ?
        textEntryField    = GUI.chatEntry $ State.guiState clientState
    messageText <- GUI.textValue textEntryField
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceHandlers.speak connection messageConverters userId messageText selectedUser
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

      Messages.Talk -> handleTalk clientState header Domain.TalkAloud
      Messages.CrossRoomWhisper -> handleTalk clientState header Domain.Whispering
      Messages.Say -> handleEncodedTalk clientState header Domain.TalkAloud
      Messages.Whisper -> handleEncodedTalk clientState header Domain.Whispering
      Messages.Move -> handleMovement clientState header
      _ -> handleUnknownMessage clientState header
    Log.debugM "Incoming.Message.Processed" $ "Message processed. New state: " ++ (show newState)
    dispatchIncomingMessages newState

readHeader :: State.Connected -> IO Messages.Header
readHeader clientState =
  case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readHeader connection messageConverters

{- OpenPalace comments say:
  This is only sent when the server is running in "guests-are-members" mode.
  This is pointless... it's basically echoing back the logon packet that we sent to the server.
  The only reason we support this is so that certain silly servers can change our puid and ask
  us to reconnect "for security reasons."
-}
handleAlternateLogonReply :: State.Connected -> IO State.Connected  -- TODO return puidCrc and puidCounter when we need it
handleAlternateLogonReply clientState =
  do
    case State.protocolState clientState of
     State.PalaceProtocolState connection messageConverters -> PalaceInbound.readAlternateLogonReply connection messageConverters
    return clientState

handleServerVersion :: State.Connected -> Messages.Header -> IO State.Connected
handleServerVersion clientState header =
  do
    Log.debugM "Incoming.Message.ServerVersion" ("Server version: " ++ (show $ Messages.messageRefNumber header))
    return clientState -- TODO Add server version to host state

handleServerInfo :: State.Connected -> Messages.Header -> IO State.Connected
handleServerInfo clientState header =
  do
    (serverName, serverPermissions) <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readServerInfo connection messageConverters header
    Log.debugM "Incoming.Message.ServerInfo" $ "Server name: " ++ serverName
    return clientState

handleUserStatus :: State.Connected -> Messages.Header -> IO State.Connected
handleUserStatus clientState header =
  do
    userFlags <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readUserStatus connection messageConverters header
    Log.debugM "Incoming.Message.UserStatus" $ "User status -- User flags: " ++ (show userFlags)
    return clientState

handleUserLogonNotification :: State.Connected -> Messages.Header -> IO State.Connected
handleUserLogonNotification clientState header =
  do
    let gui = State.guiState clientState
    (userRefId, palaceUserCount) <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readUserLogonNotification connection messageConverters header
    -- TODO Update state
    let message = "User " ++ (Domain.userName $ userIdFrom header refIdToUserIdMapping) ++ " just arrived."
    GUI.appendMessage (GUI.logWindow gui) $ makeRoomAnnouncement message
    Log.debugM  "Incoming.Message.UserLoggedOnAndMax" $  message ++ "  Population: " ++ (show palaceUserCount)
    return clientState

handleMediaServerInfo :: State.Connected -> Messages.Header -> IO State.Connected
handleMediaServerInfo clientState header =
  do
    serverInfo <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readMediaServerInfo connection header
    Log.debugM "Incoming.Message.HttpServerLocation" $ "Media server: " ++ serverInfo
    let newState = StateHandlers.withMediaServerInfo clientState serverInfo
    _  <- loadRoomBackgroundImage newState
    Log.debugM "Incoming.Message.HttpServerLocation.Processed" $ "New state: " ++ (show newState)
    return newState

 -- room name, background image, overlay images, props, hotspots, draw commands
handleRoomDescription :: State.Connected -> Messages.Header -> IO State.Connected
handleRoomDescription clientState header =
  do
    roomDescription <- case State.protocolState clientState of
                        State.PalaceProtocolState connection messageConverters ->
                          PalaceInbound.readRoomDescription connection messageConverters header
    Log.debugM "Incoming.Message.GotRoomDescription" $ show roomDescription
    let newState = StateHandlers.withRoomDescription clientState roomDescription
    _ <- loadRoomBackgroundImage newState
    Log.debugM "Incoming.Message.GotRoomDescription.Processed" $ "New state: " ++ (show newState)
    return newState
  {- OpenPalace also does this when receiving these messages:
	clearStatusMessage currentRoom
	clearAlarms
	midiStop
     and after parsing the information:
        Dim room 100
        Room.showAvatars = true -- scripting can hide all avatars
        Dispatch room change event for scripting
    -}
    
handleUserList :: State.Connected -> Messages.Header -> IO State.Connected
handleUserList clientState header =
  do
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readUserList connection header
    Log.debugM "Incoming.Message.GotUserList" $ "Received user list."
    return clientState
    {- OpenPalace does:
         currentRoom.removeAllUsers();
    -}

handleNewUserNotification :: State.Connected -> Messages.Header -> IO State.Connected
handleNewUserNotification clientState header =
  do
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readNewUserNotification connection header
    Log.debugM "Incoming.Message.UserNew" $ "Received new user notification."
    return clientState
    {- OpenPalace does:
         PalaceSoundPlayer.getInstance().playConnectionPing();
         And if one's self entered:
         if (needToRunSignonHandlers) {  palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
					 needToRunSignonHandlers = false; }
         palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleTalk :: State.Connected -> Messages.Header -> Domain.ChatMode -> IO State.Connected
handleTalk clientState header mode =
  do
    let gui = State.guiState clientState
    chatData <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readTalk connection header
    let communication = communicationFromChatData chatData mode
    Log.debugM "Incoming.Message.UnencryptedTalk" (show communication)
    GUI.appendMessage (GUI.logWindow gui) communication
    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    -- TODO new state with chat log and fix below
    let newState = clientState
    return newState

handleEncodedTalk :: State.Connected -> Messages.Header -> Domain.ChatMode -> IO State.Connected
handleEncodedTalk clientState header mode =
  do
    let gui = State.guiState clientState
    chatData <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readEncodedTalk connection messageConverters header
    let communication = communicationFromChatData chatData mode
    Log.debugM "Incoming.Message.EncryptedTalk" (show communication)
    GUI.appendMessage (GUI.logWindow gui) communication
    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    -- TODO new state with chat log and fix below
    let newState = clientState
    return newState

handleMovement :: State.Connected -> Messages.Header -> IO State.Connected
handleMovement clientState header =
  do
    movementData <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readMovement connection messageConverters header
    Log.debugM "Incoming.Message.Movement" (show movementData)
    let (movement, newState) = StateHandlers.withMovementData clientState movementData
    -- TODO tell the GUI to move the user (in Handler.hs)
    -- TODO send action to script event handler when there is scripting?
    return newState

handleUnknownMessage :: State.Connected -> Messages.Header -> IO State.Connected
handleUnknownMessage clientState header =
  do
    case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readUnknownMessage connection header
    return clientState



userIdFrom :: Messages.Header -> Map.Map Domain.UserRefId Domain.UserId -> Domain.UserId
userIdFrom header userMap =
  let refNumber = Messages.messageRefNumber header
  in userIdFor userMap refNumber

userIdFor :: Map.Map Domain.UserRefId Domain.UserId -> Domain.UserRefId -> Domain.UserId
userIdFor userMap refId =
  let defaultUserId = Domain.UserId { Domain.userRef = refId, Domain.userName = "User #" ++ show refId }
  in Map.findWithDefault defaultUserId refId userMap

-- TODO This needs to live in the global state
refIdToUserIdMapping :: Map.Map Domain.UserRefId Domain.UserId
refIdToUserIdMapping = Map.fromList [ (0, roomAnnouncementUserId) ]

-- TODO Move to State? Or into ref map?
roomAnnouncementUserId = Domain.UserId { Domain.userRef = 0, Domain.userName = "Announcement" }

communicationFromChatData :: HandlerTypes.ChatData -> Domain.ChatMode -> Domain.Communication
communicationFromChatData chatData chatMode =
  Domain.Communication {
    Domain.speaker = userIdFor refIdToUserIdMapping $ HandlerTypes.chatSource chatData,
    Domain.target = fmap (userIdFor refIdToUserIdMapping) (HandlerTypes.chatRecipient chatData),
    Domain.message = HandlerTypes.chatMessage chatData,
    Domain.chatMode = chatMode
  }

makeRoomAnnouncement :: String -> Domain.Communication
makeRoomAnnouncement message =
  Domain.Communication {
    Domain.speaker = roomAnnouncementUserId
  , Domain.target = Nothing
  , Domain.message = message
  , Domain.chatMode = Domain.Announcement
  }



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
         Log.debugM "Load.BackgroundImage" $ "Fetching background image " ++ imageName ++ " from " ++ (show mediaServerUrl)
         possibleImagePath <- MediaLoader.fetchCachedBackgroundImagePath host port mediaServerUrl imageName
         case possibleImagePath of
           Just imagePath -> GUI.displayBackground roomCanvas imagePath
           Nothing -> return ()
         return state
     (_, _) -> return state
