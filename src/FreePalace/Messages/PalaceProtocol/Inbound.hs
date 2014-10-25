module FreePalace.Messages.PalaceProtocol.Inbound where

import Control.Applicative
import qualified Data.Binary.Get as Get
import qualified Data.Convertible.Base as Convert
import Data.Convertible.Instances.Num
import qualified Data.Map as Map
import Data.Word

import qualified FreePalace.Messages as Messages
import qualified FreePalace.Messages.PalaceProtocol.Obfuscate as Illuminator
import qualified FreePalace.Net.Receive as Receive
import qualified FreePalace.Net.Types as Net

readHeader :: Net.IncomingByteSource -> Get.Get Word32 -> IO Messages.Header
readHeader byteSource intReader = Receive.readHeader $ Receive.readIntFromNetwork intReader byteSource

readAlternateLogonReply :: Net.IncomingByteSource -> Get.Get Word32 -> Get.Get Word16 -> IO (Int, Int)
readAlternateLogonReply byteSource intReader shortReader  =
  do
    let readInt = Receive.readIntFromNetwork intReader byteSource
        readByte = Receive.readByteFromNetwork byteSource
        readText = Receive.readNullTerminatedTextFromNetwork byteSource
        readShort = Receive.readShortFromNetwork shortReader byteSource
      
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
			 
readServerInfo :: Net.IncomingByteSource -> Get.Get Word32 -> Messages.Header -> IO (String, Int)
readServerInfo byteSource intReader header =
  do
    let readInt = Receive.readIntFromNetwork intReader byteSource
        readByte = Receive.readByteFromNetwork byteSource
        readText = Receive.readTextFromNetwork byteSource

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
readUserStatus :: Net.IncomingByteSource -> Get.Get Word16 -> Messages.Header -> IO Word16
readUserStatus byteSource shortReader header =
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource
        readByte = Receive.readByteFromNetwork byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        trailingBytes = (Messages.messageSize header) - 2

    userFlags <- readShort
    readBytes trailingBytes

    return userFlags

readUserLogonNotification :: Net.IncomingByteSource -> Get.Get Word32 -> Messages.Header -> IO (Int, Int)
readUserLogonNotification byteSource intReader header =
  do
    let readInt = Receive.readIntFromNetwork intReader byteSource
        userWhoLoggedOn = Messages.messageRefNumber header
    population <- readInt
    return (userWhoLoggedOn, population)
    
readMediaServerInfo :: Net.IncomingByteSource -> Messages.Header -> IO String
readMediaServerInfo byteSource header =
  Receive.readNullTerminatedTextFromNetwork byteSource $ Messages.messageSize header

-- TODO Finish this
readRoomDescription :: Net.IncomingByteSource -> Get.Get Word32 -> Get.Get Word16 -> Messages.Header -> IO Messages.RoomDescription
readRoomDescription byteSource intReader shortReader header =
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource
        readInt = Receive.readIntFromNetwork intReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
    roomFlags <- readInt   -- unused
    face <- readInt        -- unused
    roomId <- fromIntegral <$> readShort
    roomNameOffset <- fromIntegral <$> readShort
    backgroundImageNameOffset <- fromIntegral <$> readShort
    artistNameOffset <- fromIntegral <$> readShort
    passwordOffset <- fromIntegral <$> readShort
    hotSpotCount <- fromIntegral <$> readShort
    hotSpotOffset <- fromIntegral <$> readShort
    overlayImageCount <- fromIntegral <$> readShort
    overlayImageOffset <- fromIntegral <$> readShort
    drawCommandsCount <- fromIntegral <$> readShort
    firstDrawCommand <- readShort
    peopleCount <- fromIntegral <$> readShort
    loosePropCount <- fromIntegral <$> readShort
    firstLooseProp <- readShort
    unused <- readShort
    roomDataLength <- fromIntegral <$> readShort
    roomData <- readBytes $ roomDataLength
    let paddingLength = (Messages.messageSize header) - roomDataLength - 40 -- We have used (roomDataLength + 40 bytes so far)
    padding <- readBytes paddingLength

    -- This message doesn't come in very often so we're not going to use arrays unless it really makes things slow.
    let roomNameLength = fromIntegral $ head $ drop roomNameOffset roomData
        roomName = map Convert.convert $ take roomNameLength $ drop (roomNameOffset + 1) roomData
        backgroundImageNameLength = fromIntegral $ head $ drop backgroundImageNameOffset roomData
        backgroundImageName = map Convert.convert $ take backgroundImageNameLength $ drop (backgroundImageNameOffset + 1) roomData

    return Messages.RoomDescription {
        Messages.roomId = roomId
      , Messages.roomName = roomName
      , Messages.roomBackgroundImageName = backgroundImageName
    }
{- Next, load:
     -- overlay images
     -- hotspots
     -- loose props
     -- draw commands

For each overlay image:
	refCon = imageBA.readInt(); --  unused
	id = imageBA.readShort();
	picNameOffset = imageBA.readShort(); 
	transparencyIndex = imageBA.readShort();
	readShort(); -- Reserved: padding for field alignment
        picNameLength <- readByte
        picName <- readBytes -- from picNameOffset using picNameLength.
                             -- This is the filename. May need URL escaping based on configuration

For each hotspot:
        scriptEventMask <- readInt
	flags <- readInt              -- unused? 
	secureInfo <- readInt
	refCon <- readInt             
	location.y <- readShort
	location.x <- readShort
	id <- readShort               -- unused?
	dest <- readShort
	numPoints <- readShort  -- local, used for loading vertices (below)
	pointsOffset <- readShort
	type <- readShort
	groupId <- readShort
	nbrScripts <- readShort
	scriptRecordOffset <- readShort
	state <- readShort
	numStates <- readShort
	var stateRecordOffset:int <- readShort
	var nameOffset:int <- readShort
	var scriptTextOffset:int <- readShort
	readShort                 -- unused?
        nameLength  
        name -- using nameOffset and nameLength

    Now look for script info using the scriptTextOffset:
        scriptString <- readBytes ... -- all the bytes to the end of the hotspot bytes (this then gets parsed and loaded)

    Now go back into the array and read the vertices of the points of the hotspot, starting with pointsOffset, loop numPoints times:
        y <- readShort
        x <- readShort

    Now go back into the array and read the hotspot states starting at stateOffset and looping numStates times. A state is 8 bytes:
        pictureId <- readShort
        readShort -- unused
        y <- readShort
        x <- readShort

For each loose prop -- this is 24 bytes:
	nextOffset <- readShort  -- This is the offset into the room data bytes for the next loose prop
	readShort
	id <- readUnsignedInt
	crc <- readUnsignedInt
	flags <- readUnsignedInt
	readInt
	y <- readShort
	x <- readShort

For each draw command -- has a 10-byte header and then a length specified by commandLength:
   Header:
	nextOffset <- readShort       -- offset into the room data bytes for the next draw command
	ba.readShort -- reserved, unused
	command <- readUnsignedShort  -- contains both flags and command, obtained by bit shifting/masking. Ignore CMD_DETONATE and CMD_DELETE (?)
	commandLength <- readUnsignedShort
	commandStart <- readShort     -- if it's 0, reset to 10 to start after header. (?)

   Starting at commandStart, read pen size and colors (add a preset alpha):
	penSize <- readShort
	numPoints <- readShort
	red <- readUnsignedByte
	readUnsignedByte 	-- Values are doubled; unclear why.
	green <- readUnsignedByte
	readUnsignedByte        -- doubled greem
	blue <- readUnsignedByte
	readUnsignedByte        -- doubled blue

   For each point in the polygon up to numPoints:
	y <- readShort
	x <- readShort

   If there are remaining bytes (if this fails, fall back to using pen color for everything - this means PalaceChat 3 style packets?)
	alphaInt <- readUnsignedByte  -- line color and alpha
	red <- readUnsignedByte
	green <- readUnsignedByte
	blue <- readUnsignedByte
	
	alphaInt <- readUnsignedByte  -- fill color and alpha
	red <- readUnsignedByte
	green <- readUnsignedByte
	blue <- readUnsignedByte
-}

-- TODO Finish this
readUserList :: Net.IncomingByteSource -> Messages.Header -> IO ()
readUserList byteSource header =
  do
    Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()

-- TODO Finish this
readNewUserNotification :: Net.IncomingByteSource -> Messages.Header -> IO ()
readNewUserNotification byteSource header =
  do
    Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()

readTalk :: Net.IncomingByteSource -> Map.Map Int Messages.UserId -> Messages.Header -> Messages.ChatMode -> IO Messages.Communication
readTalk byteSource userMap header mode =
  do
    let speaking = Messages.userIdFrom header userMap
        chatMode = case mode of
          Messages.TalkAloud -> if Messages.userRef speaking == 0 then Messages.Announcement else Messages.TalkAloud
          otherMode -> otherMode
        readText = Receive.readNullTerminatedTextFromNetwork byteSource
        messageLength = Messages.messageSize header

    message <- truncateChatMessage <$> readText messageLength
    
    return Messages.Communication {
      Messages.speaker = speaking,
      Messages.target = Nothing,
      Messages.message = message,
      Messages.chatMode = chatMode
    }
 
readEncodedTalk :: Net.IncomingByteSource -> Get.Get Word16 -> Map.Map Int Messages.UserId -> Messages.Header -> Messages.ChatMode -> IO Messages.Communication
readEncodedTalk byteSource shortReader userMap header mode =
  -- header messageSize field is actually the message checksum + 1 ??
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource    
        readBytes = Receive.readBytesFromNetwork byteSource
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

readMovement :: Net.IncomingByteSource -> Get.Get Word16 -> Map.Map Int Messages.UserId -> Messages.Header -> IO Messages.Movement
readMovement byteSource shortReader userMap header =
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource
        mover = Messages.userIdFrom header userMap
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    return Messages.Movement { Messages.x = x, Messages.y = y, Messages.userWhoMoved = mover }

-- Chat string can't be > 254 characters long
-- OpenPalace carries along original message but unclear why it's needed
-- Even iptscrae shouldn't use what isn't displayed, no?
truncateChatMessage :: [a] -> [a]
truncateChatMessage message = take 254 message

readUnknown :: Net.IncomingByteSource -> Messages.Header -> IO ()
readUnknown byteSource header =
  do
    Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()
