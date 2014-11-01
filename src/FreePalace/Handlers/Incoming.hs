module FreePalace.Handlers.Incoming where

import           Control.Concurrent
import           Control.Exception
import qualified System.Log.Logger                                 as Log

import qualified FreePalace.Domain.Chat                            as Chat
import qualified FreePalace.Domain.GUI                             as GUI
import qualified FreePalace.Domain.State                           as State
import qualified FreePalace.Domain.User                            as User
import qualified FreePalace.Media.Loader                           as MediaLoader
import qualified FreePalace.Messages.Inbound                       as InboundMessages
import qualified FreePalace.Messages.PalaceProtocol.InboundReader  as PalaceInbound
import qualified FreePalace.Net.PalaceProtocol.Connect             as Connect


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
handleInboundEvent clientState (InboundMessages.UserEnteredRoomMessage userEnteredRoom) = handleUserEnteredRoom clientState userEnteredRoom
handleInboundEvent clientState (InboundMessages.UserExitedRoomMessage userExitedRoom) = handleUserExitedRoom clientState userExitedRoom
handleInboundEvent clientState (InboundMessages.UserDisconnectedMessage userDisconnected) = handleUserDisconnected clientState userDisconnected
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

-- A user connects to this host
handleUserLogonNotification :: State.Connected -> InboundMessages.UserLogonNotification -> IO State.Connected
handleUserLogonNotification clientState logonNotification =
  do
    Log.debugM  "Incoming.Message.UserLogonNotification" $ (show logonNotification)
    return clientState
    {- OpenPalace does:
         Adds the user ID to  recentLogonUserIds; sets a timer to remove it in 15 seconds.
         This is for the Ping sound managed by the NewUserNotification.
    -}

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

-- List of users in the current room
handleUserList :: State.Connected -> InboundMessages.UserListing -> IO State.Connected
handleUserList clientState userList =
  do
    Log.debugM "Incoming.Message.UserList" $ show userList
    let newState = State.withRoomUsers clientState userList
    return newState
    {- OpenPalace - after creating each user:
         user.loadProps()
    -}

handleUserEnteredRoom :: State.Connected -> InboundMessages.UserEnteredRoom -> IO State.Connected
handleUserEnteredRoom clientState userNotification =
  do
    let newState = State.withRoomUsers clientState $ InboundMessages.UserListing [userNotification]
        gui = State.guiState clientState
        userWhoArrived = InboundMessages.userId userNotification
        message = (User.userName $ State.userIdFor newState userWhoArrived) ++ " entered the room."
    Log.debugM "Incoming.Message.NewUser" $ show userNotification
    GUI.appendMessage (GUI.logWindow gui) $ Chat.makeRoomAnnouncement message
    return newState
    {- OpenPalace does:
         Looks for this user in recentLogonIds (set in UserLogonNotification) - to see if the user has entered the palace within the last 15 sec.
         If so, remove it from the recentLogonIds and play the connection Ping:
            PalaceSoundPlayer.getInstance().playConnectionPing();
         If someone else entered and they were selected in the user list, they are selected (for whisper) in the room too.
         And if one's self entered:
            if (needToRunSignonHandlers) {  -- This flag is set to true when you first log on
                requestRoomList();
                requestUserList();
                palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
			    needToRunSignonHandlers = false; }
            palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleUserExitedRoom :: State.Connected -> InboundMessages.UserExitedRoom -> IO State.Connected
handleUserExitedRoom clientState exitNotification@(InboundMessages.UserExitedRoom userWhoLeft) =
  do
    let
        message = (User.userName $ State.userIdFor clientState userWhoLeft) ++ " left the room."
        gui = State.guiState clientState
        newState = State.withUserLeavingRoom clientState userWhoLeft
    Log.debugM "Incoming.Message.NewUser" $ show exitNotification
    GUI.appendMessage (GUI.logWindow gui) $ Chat.makeRoomAnnouncement message
    return newState

handleUserDisconnected :: State.Connected -> InboundMessages.UserDisconnected -> IO State.Connected
handleUserDisconnected clientState disconnectedNotification@(InboundMessages.UserDisconnected userWhoLeft population) =
  do
    let newState = State.withUserDisconnecting clientState userWhoLeft population
    Log.debugM "Incoming.Message.NewUser" $ show disconnectedNotification
    return newState
    {- OpenPalace does:
        If user is in the room, PalaceSoundPlayer.getInstance().playConnectionPing()
        If that user was the selected user for whisper (in the room or not), unset the selected user
    -}

handleChat :: State.Connected -> InboundMessages.Chat -> IO State.Connected
handleChat clientState chatData =
  do
    let gui = State.guiState clientState
        communication = State.communicationFromChatData clientState chatData
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
-- TODO This should happen on a separate thread.
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
