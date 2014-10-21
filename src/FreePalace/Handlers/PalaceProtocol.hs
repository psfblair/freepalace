module FreePalace.Handlers.PalaceProtocol where

import qualified System.Log.Logger as Log
import qualified Data.Map as Map
import Control.Exception

import qualified FreePalace.State as State
import qualified FreePalace.Messages as Messages
import qualified FreePalace.Messages.PalaceProtocol.Inbound as PalaceInbound
import qualified FreePalace.Messages.PalaceProtocol.Outbound as PalaceOutbound
import qualified FreePalace.Net as Netcom
import qualified FreePalace.Net.Receive as Receive
import qualified FreePalace.Net.Send as Send
import qualified FreePalace.GUI.Types as GUI

handleHandshake :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters -> IO State.Connected
handleHandshake clientState connection messageConverters =
  do
    let intReader  = State.palaceIntReader messageConverters
        byteSource = State.palaceByteSource connection
        readInt    = Receive.readIntFromNetwork intReader byteSource
    header <- Receive.readHeader readInt -- TODO Time out if this takes too long
    let msgType    = Messages.messageType header
        userRefId  = Messages.messageRefNumber header
        newState   = clientState { State.userState = State.UserState { State.userId = State.userIdFor userRefId }}
    Log.debugM "Incoming.Handshake" (show msgType)
    case msgType of
      Messages.BigEndianServer    -> return  $ newState
      Messages.LittleEndianServer -> return  $ newState { State.protocolState = State.PalaceProtocolState connection Netcom.littleEndianMessageConverters }
      Messages.UnknownServer      -> throwIO $ userError "Unknown server type"
      _                           -> throwIO $ userError "Invalid server type"

sendLogin :: State.PalaceConnection -> State.PalaceMessageConverters -> Messages.UserId -> IO ()
sendLogin State.PalaceConnection { State.palaceByteSink = byteSink } messageConverters userId =
  do
    let intsToBuilder     = Send.toIntByteStringBuilder   $ State.palaceIntWriter   messageConverters
        shortsToBuilder   = Send.toShortByteStringBuilder $ State.palaceShortWriter messageConverters
        loginMessageBytes = PalaceOutbound.loginMessage intsToBuilder shortsToBuilder userId
    Log.debugM "Outgoing.Login" "Sending login..."
    Send.writeBytesToSink byteSink loginMessageBytes     

-- TODO The handler has to see if 1) it's a client command, 2) it's a script call, 3) a user is selected (for whisper)
speak :: State.PalaceConnection -> State.PalaceMessageConverters -> Messages.UserId -> String -> Maybe Messages.UserId -> IO ()
speak State.PalaceConnection { State.palaceByteSink = byteSink } messageConverters userId messageText selectedUser =
  do
    let intsToBuilder     = Send.toIntByteStringBuilder   $ State.palaceIntWriter   messageConverters
        shortsToBuilder   = Send.toShortByteStringBuilder $ State.palaceShortWriter messageConverters
    
        -- TODO check the initial character of the message for instructions
        communication = Messages.Communication {
          Messages.speaker = userId,
          Messages.target = selectedUser,
          Messages.message = messageText,
          Messages.chatMode = Messages.Outbound
          }
                        
        chatMessageBytes = PalaceOutbound.chatMessage intsToBuilder shortsToBuilder communication
    Log.debugM "Outgoing.Talk" (show communication)
    Send.writeBytesToSink byteSink chatMessageBytes

readHeader :: State.PalaceConnection -> State.PalaceMessageConverters -> IO Messages.Header
readHeader connection messageConverters =
  do
    let byteSource = State.palaceByteSource connection
        intReader = State.palaceIntReader messageConverters
    PalaceInbound.readHeader byteSource intReader

handleAlternateLogonReply :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters -> IO State.Connected
handleAlternateLogonReply clientState connection messageConverters =
  do
    let byteSource = State.palaceByteSource connection
        intReader = State.palaceIntReader messageConverters
        shortReader = State.palaceShortReader messageConverters
    PalaceInbound.readAlternateLogonReply byteSource intReader shortReader
    return clientState

handleServerInfo :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters -> Messages.Header -> IO State.Connected
handleServerInfo clientState connection messageConverters header =
  do
    let byteSource = State.palaceByteSource connection
        intReader = State.palaceIntReader messageConverters
    (serverName, permissions) <- PalaceInbound.readServerInfo byteSource intReader header 
    Log.debugM "Incoming.Message.ServerInfo" $ "Server name: " ++ serverName
    return clientState --TODO add server name and permissions to host state
  
handleUserStatus :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters -> Messages.Header -> IO State.Connected
handleUserStatus clientState connection messageConverters header =  
  do
    let byteSource = State.palaceByteSource connection
        shortReader = State.palaceShortReader messageConverters
    userFlags  <- PalaceInbound.readUserStatus byteSource shortReader header
    Log.debugM "Incoming.Message.UserStatus" $ "User status -- User flags: " ++ (show userFlags)
    return clientState -- TODO probably need to do something with this info

handleUserLogonNotification :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters ->
                               Map.Map Int Messages.UserId -> Messages.Header -> IO (String, State.Connected)
handleUserLogonNotification clientState connection messageConverters userMap header =  
  do
    let byteSource = State.palaceByteSource connection
        intReader = State.palaceIntReader messageConverters
    (logonId, totalUserCount) <- PalaceInbound.readUserLogonNotification byteSource intReader header
    let message = "User " ++ (Messages.userName $ Messages.userIdFrom header userMap) ++ " just arrived."
    Log.debugM  "Incoming.Message.UserLoggedOnAndMax" $  message ++ "  Population: " ++ (show totalUserCount)
    return (message, clientState)

handleMediaServerInfo :: State.Connected -> State.PalaceConnection -> Messages.Header -> IO State.Connected
handleMediaServerInfo clientState connection header =  
  do
    let byteSource = State.palaceByteSource connection
    serverInfo <- PalaceInbound.readMediaServerInfo byteSource header
    Log.debugM "Incoming.Message.HttpServerLocation" $ "Media server: " ++ serverInfo
    -- TODO if we already have a room description, load the media
    return clientState 

 -- room name, background image, overlay images, props, hotspots, draw commands
handleRoomDescription :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters -> Messages.Header -> IO State.Connected
handleRoomDescription clientState connection messageConverters header =  
  do
    let byteSource = State.palaceByteSource connection
        intReader = State.palaceIntReader messageConverters
        shortReader = State.palaceShortReader messageConverters
    roomDescription <- PalaceInbound.readRoomDescription byteSource intReader shortReader header
    Log.debugM "Incoming.Message.GotRoomDescription" $ show roomDescription
    -- If we have received the media server info, load the background image from the mediaServer using various permutations of backgroundImageName:
       -- If the image name ends with .gif first try to use .png and then .jpg; otherwise use the original name

  {- OpenPalace also does this when receiving these messages:
	clearStatusMessage currentRoom
	clearAlarms
	midiStop
     and after parsing the information:
        Dim room 100
        Room.showAvatars = true -- scripting can hide all avatars
        Dispatch room change event for scripting
    -}
    return clientState

handleUserList :: State.Connected -> State.PalaceConnection -> Messages.Header -> IO State.Connected
handleUserList clientState connection header =
  do
    let byteSource = State.palaceByteSource connection
    PalaceInbound.readUserList byteSource header
    Log.debugM "Incoming.Message.GotUserList" $ "Received user list."
    return clientState
    {- OpenPalace does:
         currentRoom.removeAllUsers();
    -}

handleNewUserNotification :: State.Connected -> State.PalaceConnection -> Messages.Header -> IO State.Connected
handleNewUserNotification clientState connection header =
  do
    let byteSource = State.palaceByteSource connection    
    PalaceInbound.readNewUserNotification byteSource header
    Log.debugM "Incoming.Message.UserNew" $ "Received new user notification."
    return clientState
    {- OpenPalace does:
         PalaceSoundPlayer.getInstance().playConnectionPing();
         And if one's self entered:
         if (needToRunSignonHandlers) {  palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
					 needToRunSignonHandlers = false; }
         palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleTalk :: State.Connected -> State.PalaceConnection -> Map.Map Int Messages.UserId ->
              Messages.Header -> Messages.ChatMode -> IO (Messages.Communication, State.Connected)
handleTalk clientState connection userMap header mode =
  do
    let byteSource = State.palaceByteSource connection
    chat <- PalaceInbound.readTalk byteSource userMap header mode
    Log.debugM "Incoming.Message.UnencryptedTalk" (show chat)
    return (chat, clientState)

handleEncodedTalk :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters ->
                     Map.Map Int Messages.UserId -> Messages.Header -> Messages.ChatMode -> IO (Messages.Communication, State.Connected)
handleEncodedTalk clientState connection messageConverters userMap header mode =
  do
    let byteSource = State.palaceByteSource connection
        shortReader = State.palaceShortReader messageConverters
    chat <- PalaceInbound.readEncodedTalk byteSource shortReader userMap header mode
    Log.debugM "Incoming.Message.EncryptedTalk" (show chat)
    return (chat, clientState)

handleMovement :: State.Connected -> State.PalaceConnection -> State.PalaceMessageConverters ->
                  Map.Map Int Messages.UserId -> Messages.Header -> IO State.Connected
handleMovement clientState connection messageConverters userMap header =
  do
    let byteSource = State.palaceByteSource connection
        shortReader = State.palaceShortReader messageConverters
    movement <- PalaceInbound.readMovement byteSource shortReader userMap header
    Log.debugM "Incoming.Message.EncryptedTalk" (show movement)
    -- TODO tell the GUI to move the user (in Handler.hs)
    -- TODO send action to script event handler when there is scripting?
    return clientState

handleUnknownMessage :: State.Connected -> State.PalaceConnection -> Messages.Header -> IO State.Connected
handleUnknownMessage clientState connection header =
  do
    let byteSource = State.palaceByteSource connection
    PalaceInbound.readUnknown byteSource header
    return clientState
