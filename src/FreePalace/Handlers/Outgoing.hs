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
         messageListenerThreadId <- forkIO $ initializeMessageDispatcher stateFromMessageReceiverThread connectedState
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

-- TODO Time out if this takes too long, don't keep listening, tell the main thread.
-- TODO Make sure exceptions are caught so as not to block the main thread waiting on the MVar
initializeMessageDispatcher :: MVar State.Connected -> State.Connected -> IO ()
initializeMessageDispatcher conveyorOfStateBackToMainThread clientState =
  do
    Log.debugM "Incoming.Message.Await" $ "Awaiting initial handshake with state: " ++ (show clientState)
    header <- readHeader clientState
    Log.debugM "Incoming.Message.Header" (show header)
    message <- readMessage clientState header
    newState <- case message of
       InboundMessages.HandshakeMessage _ -> handleInboundEvent clientState message
       _ -> throwIO $ userError "Connection failed. No handshake."
    Log.debugM "Incoming.Message.Processed" $ "Message processed. New state: " ++ (show newState)
    putMVar conveyorOfStateBackToMainThread newState
    
    dispatchIncomingMessages newState
  
-- TODO Handle connection loss
dispatchIncomingMessages :: State.Connected -> IO ()
dispatchIncomingMessages clientState =
  do
    Log.debugM "Incoming.Message.Await" $ "Awaiting messages with state: " ++ (show clientState)
    header <- readHeader clientState
    Log.debugM "Incoming.Message.Header" (show header)
    message <- readMessage clientState header
    newState <- handleInboundEvent clientState message
    Log.debugM "Incoming.Message.Processed" $ "Message processed. New state: " ++ (show newState)
    dispatchIncomingMessages newState

readHeader :: State.Connected -> IO InboundMessages.Header
readHeader clientState =
  case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readHeader connection messageConverters

readMessage :: State.Connected -> InboundMessages.Header -> IO InboundMessages.InboundMessage
readMessage clientState header =
  case State.protocolState clientState of
      State.PalaceProtocolState connection messageConverters -> PalaceInbound.readMessage connection messageConverters header

-- TODO Right now these need to be in IO because of logging and GUI changes. Separate those out.
handleInboundEvent :: State.Connected -> InboundMessages.InboundMessage -> IO State.Connected
handleInboundEvent clientState (InboundMessages.HandshakeMessage handshakeData) = handleHandshake clientState handshakeData
handleInboundEvent clientState (InboundMessages.LogonReplyMessage logonReply) = handleLogonReply clientState logonReply
handleInboundEvent clientState (InboundMessages.ServerVersionMessage serverVersion) = handleServerVersion clientState serverVersion
handleInboundEvent clientState (InboundMessages.ServerInfoMessage serverInfo) = handleServerInfo clientState serverInfo
handleInboundEvent clientState (InboundMessages.UserStatusMessage userStatus) = handleUserStatus clientState userStatus
handleInboundEvent clientState (InboundMessages.UserLogonMessage userLogonNotification) = handleUserLogonNotification clientState userLogonNotification
handleInboundEvent clientState (InboundMessages.MediaServerMessage mediaServerInfo) = handleMediaServerInfo clientState mediaServerInfo
handleInboundEvent clientState (InboundMessages.RoomDescriptionMessage roomDescription) = handleRoomDescription clientState roomDescription
handleInboundEvent clientState (InboundMessages.UserListMessage userListing) = handleUserList clientState userListing
handleInboundEvent clientState (InboundMessages.NewUserMessage newUserNotification) = handleNewUserNotification clientState newUserNotification
handleInboundEvent clientState (InboundMessages.ChatMessage chat) = handleChat clientState chat
handleInboundEvent clientState (InboundMessages.MovementMessage movementNotification) = handleMovement clientState movementNotification
handleInboundEvent clientState (InboundMessages.NoOpMessage noOp) = handleNoOp clientState noOp

handleHandshake :: State.Connected -> InboundMessages.Handshake -> IO State.Connected
handleHandshake clientState handshake = 
  do
    Log.debugM "Incoming.Handshake" (show handshake)
    let newState = handleProtocolUpdate clientState (InboundMessages.protocolInfo handshake)
        userRefId = InboundMessages.userRefId handshake
    return $ State.withUserRefId newState userRefId

-- This is separate because it depends on the specific protocol
handleProtocolUpdate :: State.Connected -> InboundMessages.ProtocolInfo ->  State.Connected
handleProtocolUpdate clientState (InboundMessages.PalaceProtocol connection endianness) =
  State.withProtocol clientState (State.PalaceProtocolState connection newMessageConverters)
  where newMessageConverters = Connect.messageConvertersFor endianness

{- Re AlternateLogonReply: OpenPalace comments say:
  This is only sent when the server is running in "guests-are-members" mode.
  This is pointless... it's basically echoing back the logon packet that we sent to the server.
  The only reason we support this is so that certain silly servers can change our puid and ask
  us to reconnect "for security reasons."
-}
handleLogonReply :: State.Connected -> InboundMessages.LogonReply -> IO State.Connected
handleLogonReply clientState logonReply = 
  do
    Log.debugM "Incoming.Message.LogonReply" (show logonReply)
    return clientState   -- TODO return puidCrc and puidCounter when we need it
  
handleServerVersion :: State.Connected -> InboundMessages.ServerVersion -> IO State.Connected
handleServerVersion clientState serverVersion =
  do
    Log.debugM "Incoming.Message.ServerVersion" $ "Server version: " ++ (show serverVersion)
    return clientState -- TODO Add server version to host state

handleServerInfo :: State.Connected -> InboundMessages.ServerInfoNotification -> IO State.Connected
handleServerInfo clientState serverInfo =
  do
    Log.debugM "Incoming.Message.ServerInfo" $ "Server info: " ++ (show serverInfo)
    return clientState

handleUserStatus :: State.Connected -> InboundMessages.UserStatusNotification -> IO State.Connected
handleUserStatus clientState userStatus =
  do
    Log.debugM "Incoming.Message.UserStatus" $ "User status -- User flags: " ++ (show userStatus)
    return clientState

handleUserLogonNotification :: State.Connected -> InboundMessages.UserLogonNotification -> IO State.Connected
handleUserLogonNotification clientState (InboundMessages.UserLogonNotification userRefId palaceUserCount) =
  do
    let gui = State.guiState clientState
    let message = "User " ++ (User.userName $ User.userIdFor User.refIdToUserIdMapping userRefId) ++ " just arrived."
    -- TODO Update state
    GUI.appendMessage (GUI.logWindow gui) $ Chat.makeRoomAnnouncement message
    Log.debugM  "Incoming.Message.UserLogonNotification" $  message ++ "  Population: " ++ (show palaceUserCount)
    return clientState

handleMediaServerInfo :: State.Connected -> InboundMessages.MediaServerInfo -> IO State.Connected
handleMediaServerInfo clientState serverInfo =
  do
    Log.debugM "Incoming.Message.MediaServerInfo" $ show serverInfo
    let newState = State.withMediaServerInfo clientState serverInfo
    _  <- loadRoomBackgroundImage newState
    Log.debugM "Incoming.Message.MediaServerInfo.Processed" $ "New state: " ++ (show newState)
    return newState

 -- room name, background image, overlay images, props, hotspots, draw commands
handleRoomDescription :: State.Connected -> InboundMessages.RoomDescription -> IO State.Connected
handleRoomDescription clientState roomDescription =
  do
    Log.debugM "Incoming.Message.RoomDescription" $ show roomDescription
    let newState = State.withRoomDescription clientState roomDescription
    _ <- loadRoomBackgroundImage newState
    Log.debugM "Incoming.Message.RoomDescription.Processed" $ "New state: " ++ (show newState)
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

handleUserList :: State.Connected -> InboundMessages.UserListing -> IO State.Connected
handleUserList clientState userList =
  do
    Log.debugM "Incoming.Message.UserList" $ show userList
    return clientState
    {- OpenPalace does:
         currentRoom.removeAllUsers();
    -}

handleNewUserNotification :: State.Connected -> InboundMessages.NewUser -> IO State.Connected
handleNewUserNotification clientState userNotification =
  do
    Log.debugM "Incoming.Message.NewUser" $ show userNotification
    return clientState
    {- OpenPalace does:
         PalaceSoundPlayer.getInstance().playConnectionPing();
         And if one's self entered:
         if (needToRunSignonHandlers) {  palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
					 needToRunSignonHandlers = false; }
         palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleChat :: State.Connected -> InboundMessages.Chat -> IO State.Connected
handleChat clientState chatData =
  do
    let gui = State.guiState clientState
    let communication = Chat.fromChatData chatData
    Log.debugM "Incoming.Message.Chat" $ show communication
    GUI.appendMessage (GUI.logWindow gui) communication
    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    -- TODO new state with chat log and fix below
    let newState = clientState
    return newState

handleMovement :: State.Connected -> InboundMessages.MovementNotification -> IO State.Connected
handleMovement clientState movementData =
  do
    Log.debugM "Incoming.Message.Movement" $ show movementData
    let (_, newState) = State.withMovementData clientState movementData
    -- TODO tell the GUI to move the user (in Handler.hs)
    -- TODO send action to script event handler when there is scripting?
    return newState

handleNoOp :: State.Connected -> InboundMessages.NoOp -> IO State.Connected
handleNoOp clientState noOp =
  do
    Log.debugM "Incoming.Message.NoOp" $ show noOp
    return clientState


-- TODO Do we want to reload this every time a new room description gets sent? How often does that happen?
loadRoomBackgroundImage :: State.Connected -> IO State.Connected
loadRoomBackgroundImage state =
  do
    let possibleMediaServer = State.mediaServer $ State.hostState state
        possibleImageName = State.roomBackgroundImageName . State.currentRoomState . State.hostState $ state
        roomCanvas = GUI.roomCanvas $ State.guiState state
    Log.debugM "Load.BackgroundImage" $ "Media server url: " ++ (show possibleMediaServer)
    Log.debugM "Load.BackgroundImage" $ "Background image: " ++ (show possibleImageName)
    case (possibleMediaServer, possibleImageName) of
     (Just mediaServerUrl, Just imageName) ->
       do
         let host = State.hostname $ State.hostState state
             port = State.portId $ State.hostState state
         Log.debugM "Load.BackgroundImage" $ "Fetching background image " ++ imageName ++ " from " ++ (show mediaServerUrl)
         possibleImagePath <- MediaLoader.fetchCachedBackgroundImagePath host port mediaServerUrl imageName
         case possibleImagePath of
           Just imagePath -> GUI.displayBackground roomCanvas imagePath
           Nothing -> return ()
         return state
     (_, _) -> return state
