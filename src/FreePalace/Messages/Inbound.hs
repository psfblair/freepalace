module FreePalace.Messages.Inbound where

import Control.Applicative
import qualified Data.Map as Map
import Data.Word

import FreePalace.Net.Types as Net
import FreePalace.Messages as Messages
import qualified FreePalace.Messages.Obfuscate as Illuminator

readAlternateLogonReply :: Net.Communicators -> IO (Int, Int)
readAlternateLogonReply communicators  =
  do
    let readInt = Net.readInt communicators
        readByte = Net.readByte communicators
        readText = Net.readText communicators
        readShort = Net.readShort communicators
      
    crc <- readInt
    counter <- readInt
    userNameLength <- readByte
    userName <- readText 31
    wizardPassword <- readText 32
    auxFlags <- readInt
 
    puidCounter <- readInt
    puidCrc <- readInt
 
    demoElapsed <- readInt
    totalElapsed <- readInt
    demoLimit <- readInt
    desiredRoom <- readShort
    reserved <- readText 6
    uploadRequestedProtocolVersion <- readInt
    uploadCapabilities <- readInt
    downloadCapabilities <- readInt
    upload2DEngineCapabilities <- readInt
    upload2DGraphicsCapabilities <- readInt
    upload3DEngineCapabilities <- readInt
        
    return (puidCounter, puidCrc)
			 
readServerInfo :: Net.Communicators -> Messages.Header -> IO (String, Int)
readServerInfo communicators header =
  do
    let readInt = Net.readInt communicators
        readByte = Net.readByte communicators
        readText = Net.readTextNoTerminator communicators

    permissions <- readInt
    size <- fromIntegral <$> readByte
    serverName <- readText size

    {- OpenPalace notes:
	Weird -- this message is supposed to include options and upload/download capabilities, but doesn't.
          options = socket.readUnsignedInt();
          uploadCapabilities = socket.readUnsignedInt();
          downloadCapabilities = socket.readUnsignedInt();
    -}
    
    return (serverName, permissions)

-- TODO Create a UserStatus type
readUserStatus :: Net.Communicators -> Messages.Header -> IO Word16
readUserStatus communicators header =
  do
    let readShort = Net.readShort communicators
        readBytes = Net.readBytes communicators
        trailingBytes = (Messages.messageSize header) - 2

    userFlags <- readShort
    readBytes trailingBytes

    return userFlags

readUserLogonNotification :: Net.Communicators -> Messages.Header -> IO (Int, Int)
readUserLogonNotification communicators header =
  do
    let readInt = Net.readInt communicators
        userWhoLoggedOn = Messages.messageRefNumber header
    population <- readInt
    return (userWhoLoggedOn, population)
    
readMediaServerInfo :: Net.Communicators -> Messages.Header -> IO String
readMediaServerInfo communicators header =
  Net.readTextNoTerminator communicators  $ Messages.messageSize header

-- TODO Finish this
readRoomDescription :: Net.Communicators -> Messages.Header -> IO ()
readRoomDescription communicators header =
  do
    Net.readBytes communicators $ Messages.messageSize header
    return ()

-- TODO Finish this
readUserList :: Net.Communicators -> Messages.Header -> IO ()
readUserList communicators header =
  do
    Net.readBytes communicators $ Messages.messageSize header
    return ()

-- TODO Finish this
readNewUserNotification :: Net.Communicators -> Messages.Header -> IO ()
readNewUserNotification communicators header =
  do
    Net.readBytes communicators $ Messages.messageSize header
    return ()

readMovement :: Net.Communicators -> Map.Map Int Messages.UserId -> Messages.Header -> IO Messages.Movement
readMovement communicators userMap header =
  do
    let readShort = Net.readShort communicators
        mover = Messages.userIdFrom header userMap
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    return Messages.Movement { Messages.x = x, Messages.y = y, Messages.userWhoMoved = mover }

readTalk :: Net.Communicators -> Map.Map Int Messages.UserId -> Messages.Header -> Messages.ChatMode -> IO Messages.Communication
readTalk communicators userMap header mode =
  do
    let speaking = Messages.userIdFrom header userMap
        chatMode = case mode of
          Messages.TalkAloud -> if Messages.userRef speaking == 0 then Messages.Announcement else Messages.TalkAloud
          otherMode -> otherMode
        readText = Net.readText communicators
        messageLength = Messages.messageSize header

    message <- truncateChatMessage <$> readText messageLength
    
    return Messages.Communication {
      Messages.speaker = speaking,
      Messages.target = Nothing,
      Messages.message = message,
      Messages.chatMode = chatMode
    }
 
readEncodedTalk :: Net.Communicators -> Map.Map Int Messages.UserId -> Messages.Header -> Messages.ChatMode -> IO Messages.Communication
readEncodedTalk communicators userMap header mode =
  -- header messageSize field is actually the message checksum + 1 ??
  do
    let readShort = Net.readShort communicators
        readBytes = Net.readBytes communicators
        speaking = Messages.userIdFrom header userMap

    length <- fromIntegral <$> readShort -- This is apparently the total length including these two bytes and the terminator
    let messageLength = length - 2
    obfuscated <- truncateChatMessage <$> init <$> readBytes messageLength
    let message = Illuminator.illuminate obfuscated
        
    return Messages.Communication {
      Messages.speaker = speaking,
      Messages.target = Nothing,
      Messages.message = message,
      Messages.chatMode = mode
    }

-- Chat string can't be > 254 characters long
-- OpenPalace carries along original message but unclear why it's needed
-- Even iptscrae shouldn't use what isn't displayed, no?
truncateChatMessage :: [a] -> [a]
truncateChatMessage message = take 254 message

readUnknown :: Net.Communicators -> Messages.Header -> IO ()
readUnknown communicators header =
  do
    Net.readBytes communicators $ Messages.messageSize header
    return ()
