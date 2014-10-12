module FreePalace.Handlers where

import System.IO
import qualified System.Log.Logger as Log
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent

import qualified FreePalace.Messages as Messages
import qualified FreePalace.Messages.Outbound as Outbound
import qualified FreePalace.Messages.Inbound as Inbound
import qualified FreePalace.Net.Receive as Recv
import qualified FreePalace.Net.Send as Send
import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Net as Netcom
import qualified FreePalace.Net.Types as Net

data GUIEventHandlers = GUIEventHandlers {
  handleUserTextEntry :: IO ()
}

guiEventHandlers :: GUI.Components -> Net.Communicators -> Net.Translators -> Messages.UserId -> GUIEventHandlers
guiEventHandlers gui communicators translators userId = GUIEventHandlers {
  handleUserTextEntry = speak gui communicators translators userId
}

bindHandlers :: GUI.Components -> GUIEventHandlers -> IO ()
bindHandlers guiComponents guiEventHandlers =
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

handleConnectRequested :: GUI.Components -> Net.Connectors -> Net.Hostname -> Net.PortId -> IO ()
handleConnectRequested gui connectors host portString =
  do
    -- TODO If we are already connected, disconnect
    let connect = Net.connect connectors
    channels <- connect (host::Net.Hostname) (portString::Net.PortId) 
    handshake <- handleHandshake channels    -- Now in STATE_HANDSHAKING
    case handshake of
      Left msg -> return () -- TODO Provide some way to indicate connection failed. Don't close the connection window.
      Right (communicators, translators, userRefId) ->  -- Now in STATE_READY 
        do
          -- TODO Allow user to set user name
          let user = Messages.UserId { Messages.userRef = userRefId, Messages.userName = "Haskell Curry"}
          sendLogin communicators translators user   -- TODO Also could raise IO Error
          messageListenerThreadId <- forkIO $ dispatchIncomingMessage communicators gui
          bindHandlers gui $ guiEventHandlers gui communicators translators user  -- TODO allow rebinding if user name changes
          -- TODO Must bind disconnect and reconnect to something that will stop the forked process (Control.Concurrent -  killThread :: ThreadId -> IO () )
          GUI.closeDialog $ GUI.connectDialog gui

-- TODO Time out if this takes too long
-- TODO Use IOError instead of Either? Or at least don't wrap strings in Either
handleHandshake :: Net.Channels -> IO (Either String (Net.Communicators, Net.Translators, Int))
handleHandshake channels =
  do
    let byteSource = Net.source channels
    let byteSink = Net.sink channels
    let bigEndianCommunicators =  Netcom.bigEndianCommunicators byteSource byteSink -- default to big endian
    header <- Net.readHeader bigEndianCommunicators
    let msgType = Messages.messageType header
        userRefId = Messages.messageRefNumber header
    Log.debugM "Incoming.Handshake" (show msgType)
    case msgType of
      Messages.BigEndianServer    ->  return $ Right (bigEndianCommunicators, Outbound.bigEndianTranslators, userRefId)
      Messages.LittleEndianServer ->  return $ Right (Netcom.littleEndianCommunicators byteSource byteSink, Outbound.littleEndianTranslators, userRefId)
      Messages.UnknownServer      ->  return $ Left "Unknown server type"
      _                           ->  return $ Left "Unknown server type"

sendLogin :: Net.Communicators -> Net.Translators -> Messages.UserId -> IO ()
sendLogin communicators translators userId =
  do
    let sendBytes = Net.writeBytes communicators
        loginMessageBytes = Outbound.loginMessage translators userId
    sendBytes loginMessageBytes

-- TODO The handler has to see if 1) it's connected, 2) it's a client command, 3) it's a script call, 4) a user is selected (for whisper)
-- TODO ultimately for this to be able to allow the user to change name mid-stream, this needs to take a way to get the latest name from
--      the user mapping
speak :: GUI.Components -> Net.Communicators -> Net.Translators -> Messages.UserId -> IO ()
speak gui communicators translators userId =
  do
    let sendBytes = Net.writeBytes communicators
        textEntryField = GUI.chatEntry gui
        selectedUser = Nothing -- TODO - get selected user from ... ?
    messageText <- GUI.textValue textEntryField
    
    -- TODO check the initial character of the message for instructions
    let communication = Messages.Communication {
          Messages.speaker = userId,
          Messages.target = selectedUser,
          Messages.message = messageText,
          Messages.chatMode = Messages.Outbound
          }
        chatMessage = Outbound.chatMessage translators communication
    sendBytes chatMessage
    GUI.clearTextEntry textEntryField


-- TODO Handle connection loss
dispatchIncomingMessage :: Net.Communicators -> GUI.Components -> IO ()
dispatchIncomingMessage communicators gui =
  do
    header <- Net.readHeader communicators
    Log.debugM "Incoming.Message.Header" (show header)
    case Messages.messageType header of
      -- Logon sequence after handshake received and logon sent
      Messages.AlternateLogonReply -> handleAlternateLogonReply communicators
      Messages.ServerVersion -> handleServerVersion communicators header
      Messages.ServerInfo -> handleServerInfo communicators header
      Messages.UserStatus -> handleUserStatus communicators header
      Messages.UserLoggedOnAndMax -> handleUserLogonNotification communicators gui header
      Messages.GotHttpServerLocation -> handleMediaServerInfo communicators header
      Messages.GotRoomDescription -> handleRoomDescription communicators header
      Messages.GotUserList -> handleUserList communicators header
      Messages.RoomDescend -> return () -- this message just means we're done receiving the room description & user list
      Messages.UserNew -> handleNewUserNotification communicators header
      -- End logon sequence
      
      Messages.Talk -> handleTalk gui communicators header Messages.TalkAloud
      Messages.CrossRoomWhisper -> handleTalk gui communicators header Messages.Whispering
      Messages.Say -> handleEncodedTalk gui communicators header Messages.TalkAloud
      Messages.Whisper -> handleEncodedTalk gui communicators header Messages.Whispering
      Messages.Move -> handleMovement gui communicators header
      _ -> handleUnknownMessage communicators header
    dispatchIncomingMessage communicators gui

{- OpenPalace comments say:
  This is only sent when the server is running in "guests-are-members" mode.
  This is pointless... it's basically echoing back the logon packetthat we sent to the server.
  The only reason we support this is so that certain silly servers can change our puid and ask
  us to reconnect "for security reasons."
-}
handleAlternateLogonReply :: Net.Communicators -> IO ()  -- TODO return puidCrc and puidCounter when we need it
handleAlternateLogonReply communicators =
  do
    Inbound.readAlternateLogonReply communicators
    return ()

handleServerVersion :: Net.Communicators -> Messages.Header -> IO ()
handleServerVersion communicators header =
  do
    Log.debugM "Incoming.Message.ServerVersion" ("Server version: " ++ (show $ Messages.messageRefNumber header))
    return () -- TODO At some point probably something will need to happen here

handleServerInfo :: Net.Communicators -> Messages.Header -> IO ()
handleServerInfo communicators header =
  do
    (serverName, permissions) <- Inbound.readServerInfo communicators header --TODO probably need to do something with this server name
    Log.debugM "Incoming.Message.ServerInfo" $ "Server name: " ++ serverName
    return ()

handleUserStatus :: Net.Communicators -> Messages.Header -> IO () -- TODO probably need to do something with this info
handleUserStatus communicators header =
  do
    userFlags  <- Inbound.readUserStatus communicators header
    Log.debugM "Incoming.Message.UserStatus" $ "User status -- User flags: " ++ (show userFlags)
    return ()

handleUserLogonNotification :: Net.Communicators -> GUI.Components -> Messages.Header -> IO ()
handleUserLogonNotification communicators gui header =
  do
    (logonId, totalUserCount) <- Inbound.readUserLogonNotification communicators header
    let message = "User " ++ (Messages.userName $ Messages.userIdFrom header refIdToUserIdMapping) ++ " just logged on."
    Log.debugM  "Incoming.Message.UserLoggedOnAndMax" $  message ++ "  Population: " ++ (show totalUserCount)
    GUI.appendMessage (GUI.logWindow gui) $ makeRoomAnnouncement message
    return ()

handleMediaServerInfo :: Net.Communicators -> Messages.Header -> IO ()
handleMediaServerInfo communicators header =
  do
    serverInfo <- Inbound.readMediaServerInfo communicators header
    Log.debugM "Incoming.Message.HttpServerLocation" $ "Media server: " ++ serverInfo
    return ()

handleRoomDescription :: Net.Communicators -> Messages.Header -> IO ()
handleRoomDescription communicators header =
  do
    Inbound.readRoomDescription communicators header
    Log.debugM "Incoming.Message.GotRoomDescription" $ "Received room description."
    return ()
  {- This is where the name, background image, hotspots, etc. come in. We'll do that later.
     Also OpenPalace does this when receiving these messages:
			currentRoom.clearStatusMessage();
			palaceController.clearAlarms();
			palaceController.midiStop();

    and eventually emits a room change event for iptScrae
  -}

handleUserList :: Net.Communicators -> Messages.Header -> IO ()
handleUserList communicators header =
  do
    Inbound.readUserList communicators header
    Log.debugM "Incoming.Message.GotUserList" $ "Received user list."
    return ()
    {- OpenPalace does:
         currentRoom.removeAllUsers();
    -}

handleNewUserNotification :: Net.Communicators -> Messages.Header -> IO ()
handleNewUserNotification communicators header =
  do
    Inbound.readNewUserNotification communicators header
    Log.debugM "Incoming.Message.UserNew" $ "Received new user notification."
    return ()
    {- OpenPalace does:
         PalaceSoundPlayer.getInstance().playConnectionPing();
         And if one's self entered:
         if (needToRunSignonHandlers) {  palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
					 needToRunSignonHandlers = false; }
         palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleTalk :: GUI.Components -> Net.Communicators -> Messages.Header -> Messages.ChatMode -> IO ()
handleTalk gui communicators header mode =
  do
    chat <- Inbound.readTalk communicators refIdToUserIdMapping header mode
    Log.debugM "Incoming.Message.UnencryptedTalk" (show chat)
    GUI.appendMessage (GUI.logWindow gui) chat

    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    return ()

handleEncodedTalk :: GUI.Components -> Net.Communicators -> Messages.Header -> Messages.ChatMode -> IO ()
handleEncodedTalk gui communicators header mode =
  do
    chat <- Inbound.readEncodedTalk communicators refIdToUserIdMapping header mode
    Log.debugM "Incoming.Message.EncryptedTalk" (show chat)
    GUI.appendMessage (GUI.logWindow gui) chat

    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    return ()

handleMovement :: GUI.Components -> Net.Communicators -> Messages.Header -> IO ()
handleMovement gui communicators header = 
  do
    movement <- Inbound.readMovement communicators refIdToUserIdMapping header 
    Log.debugM "Incoming.Message.EncryptedTalk" (show movement)
    -- TODO tell the GUI to move the user
    -- TODO send action to script event handler when there is scripting?
    return ()

handleUnknownMessage :: Net.Communicators -> Messages.Header -> IO ()
handleUnknownMessage communicators header = Inbound.readUnknown communicators header

roomAnnouncementUserId = Messages.UserId { Messages.userRef = 0, Messages.userName = "Announcement" }
refIdToUserIdMapping = Map.fromList [ (0, roomAnnouncementUserId) ]

makeRoomAnnouncement :: String -> Messages.Communication
makeRoomAnnouncement message =
  Messages.Communication {
    Messages.speaker = roomAnnouncementUserId,
    Messages.target = Nothing,
    Messages.message = message,
    Messages.chatMode = Messages.Announcement
  }
