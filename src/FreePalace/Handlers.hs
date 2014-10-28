module FreePalace.Handlers where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import qualified Data.Map                                          as Map
import qualified System.Log.Logger                                 as Log

import qualified FreePalace.Domain.Chat                            as Chat
import qualified FreePalace.Domain.GUI                             as GUI
import qualified FreePalace.Domain.Net                             as Net
import qualified FreePalace.Domain.State                           as State
import qualified FreePalace.Domain.User                            as User
import qualified FreePalace.Media.Loader                           as MediaLoader
import qualified FreePalace.Messages                               as Messages
import qualified FreePalace.Messages.Inbound                       as InboundEvents
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
         readyState <- handleHandshake connectedState
         -- TODO Must bind disconnect and reconnect to something that will stop the forked process
         -- i.e., Control.Concurrent.killThread :: ThreadId -> IO ()
         -- TODO If this thread dies or an exception is thrown on it, need to handle the disconnect
         messageListenerThreadId <- forkIO $ dispatchIncomingMessages readyState
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

handleHandshake :: State.Connected -> IO State.Connected
handleHandshake clientState =
  do
    (handshakeData, newState) <- handleProtocolUpdate clientState $ State.protocolState clientState
    Log.debugM "Incoming.Handshake" (show handshakeData)
    let userRefId = InboundEvents.userRefId handshakeData
    return $ State.withUserRefId newState userRefId

handleProtocolUpdate :: State.Connected -> State.ProtocolState ->  IO (InboundEvents.InboundHandshake, State.Connected)
handleProtocolUpdate clientState (State.PalaceProtocolState connection messageConverters) =
  do
    inboundHandshake <- PalaceInbound.readHandshake connection messageConverters
    let InboundEvents.PalaceProtocol _ endianness = InboundEvents.protocolInfo inboundHandshake
        newMessageConverters = Connect.messageConvertersFor endianness
        updatedState = State.withProtocol clientState (State.PalaceProtocolState connection newMessageConverters)
    return (inboundHandshake, updatedState)

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

      Messages.Talk -> handleTalk clientState header Chat.TalkAloud
      Messages.CrossRoomWhisper -> handleTalk clientState header Chat.Whispering
      Messages.Say -> handleEncodedTalk clientState header Chat.TalkAloud
      Messages.Whisper -> handleEncodedTalk clientState header Chat.Whispering
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
    serverInfo <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readServerInfo connection messageConverters header
    Log.debugM "Incoming.Message.ServerInfo" $ "Server info: " ++ (show serverInfo)
    return clientState

handleUserStatus :: State.Connected -> Messages.Header -> IO State.Connected
handleUserStatus clientState header =
  do
    userStatus <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readUserStatus connection messageConverters header
    Log.debugM "Incoming.Message.UserStatus" $ "User status -- User flags: " ++ (show userStatus)
    return clientState

handleUserLogonNotification :: State.Connected -> Messages.Header -> IO State.Connected
handleUserLogonNotification clientState header =
  do
    let gui = State.guiState clientState
    (InboundEvents.InboundUserLogonNotification userRefId palaceUserCount) <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readUserLogonNotification connection messageConverters header
    -- TODO Update state
    let message = "User " ++ (User.userName $ User.userIdFor User.refIdToUserIdMapping userRefId) ++ " just arrived."
    GUI.appendMessage (GUI.logWindow gui) $ Chat.makeRoomAnnouncement message
    Log.debugM  "Incoming.Message.UserLoggedOnAndMax" $  message ++ "  Population: " ++ (show palaceUserCount)
    return clientState

handleMediaServerInfo :: State.Connected -> Messages.Header -> IO State.Connected
handleMediaServerInfo clientState header =
  do
    serverInfo <- case State.protocolState clientState of
      State.PalaceProtocolState connection _ -> PalaceInbound.readMediaServerInfo connection header
    Log.debugM "Incoming.Message.HttpServerLocation" $ show serverInfo
    let newState = State.withMediaServerInfo clientState serverInfo
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
    let newState = State.withRoomDescription clientState roomDescription
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
    userList <- case State.protocolState clientState of
      State.PalaceProtocolState connection _ -> PalaceInbound.readUserList connection header
    Log.debugM "Incoming.Message.GotUserList" $ show userList
    return clientState
    {- OpenPalace does:
         currentRoom.removeAllUsers();
    -}

handleNewUserNotification :: State.Connected -> Messages.Header -> IO State.Connected
handleNewUserNotification clientState header =
  do
    userNotification <- case State.protocolState clientState of
      State.PalaceProtocolState connection _ -> PalaceInbound.readNewUserNotification connection header
    Log.debugM "Incoming.Message.UserNew" $ show userNotification
    return clientState
    {- OpenPalace does:
         PalaceSoundPlayer.getInstance().playConnectionPing();
         And if one's self entered:
         if (needToRunSignonHandlers) {  palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
					 needToRunSignonHandlers = false; }
         palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleTalk :: State.Connected -> Messages.Header -> Chat.ChatMode -> IO State.Connected
handleTalk clientState header mode =
  do
    let gui = State.guiState clientState
    chatData <- case State.protocolState clientState of
      State.PalaceProtocolState connection _ -> PalaceInbound.readTalk connection header
    let communication = Chat.fromChatData chatData mode
    Log.debugM "Incoming.Message.UnencryptedTalk" $ show communication
    GUI.appendMessage (GUI.logWindow gui) communication
    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    -- TODO new state with chat log and fix below
    let newState = clientState
    return newState

handleEncodedTalk :: State.Connected -> Messages.Header -> Chat.ChatMode -> IO State.Connected
handleEncodedTalk clientState header mode =
  do
    let gui = State.guiState clientState
    chatData <- case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readEncodedTalk connection messageConverters header
    let communication = Chat.fromChatData chatData mode
    Log.debugM "Incoming.Message.EncryptedTalk" $ show communication
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
    Log.debugM "Incoming.Message.Movement" $ show movementData
    let (_, newState) = State.withMovementData clientState movementData
    -- TODO tell the GUI to move the user (in Handler.hs)
    -- TODO send action to script event handler when there is scripting?
    return newState

handleUnknownMessage :: State.Connected -> Messages.Header -> IO State.Connected
handleUnknownMessage clientState header =
  do
    case State.protocolState clientState of
      State.PalaceProtocolState connection _ -> PalaceInbound.readUnknownMessage connection header
    return clientState





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
