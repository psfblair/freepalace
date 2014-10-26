module FreePalace.Messages.PalaceProtocol.Inbound where

import           Control.Applicative
import qualified Data.Binary.Get                              as Get
import qualified Data.Convertible.Base                        as Convert
import           Data.Convertible.Instances.Num
import qualified Data.Map                                     as Map
import           Data.Word

import qualified FreePalace.Handlers.Types                    as Handlers
import qualified FreePalace.Messages                          as Messages
import qualified FreePalace.Messages.PalaceProtocol.Obfuscate as Illuminator
import qualified FreePalace.Net                               as Net
import qualified FreePalace.Net.Receive                       as Receive

readHeader :: Net.IncomingByteSource -> Get.Get Word32 -> IO Messages.Header
readHeader byteSource intReader =
  do
    let readNextInt = Receive.readIntFromNetwork intReader byteSource
    msgType <- Messages.idToMessageType <$> readNextInt
    size <- readNextInt
    referenceNumber <- readNextInt
    return Messages.Header {
      Messages.messageType = msgType,
      Messages.messageSize = size,
      Messages.messageRefNumber = referenceNumber
    }

readAlternateLogonReply :: Net.IncomingByteSource -> Get.Get Word32 -> Get.Get Word16 -> IO (Handlers.PuidCounter, Handlers.PuidCrc)
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

    return (puidCounter, puidCrc) -- TODO When do these get used?

readServerInfo :: Net.IncomingByteSource -> Get.Get Word32 -> Messages.Header -> IO (Handlers.PlaceName, Handlers.ServerPermissions)
readServerInfo byteSource intReader header =
  do
    let readInt = Receive.readIntFromNetwork intReader byteSource
        readByte = Receive.readByteFromNetwork byteSource
        readText = Receive.readTextFromNetwork byteSource
    permissions <- readInt
    size <- fromIntegral <$> readByte
    serverName <- readText size
    return (serverName, permissions)
    {- OpenPalace notes:
	Weird -- this message is supposed to include options and upload/download capabilities, but doesn't.
          options = socket.readUnsignedInt();
          uploadCapabilities = socket.readUnsignedInt();
          downloadCapabilities = socket.readUnsignedInt();
    -}

readUserStatus :: Net.IncomingByteSource -> Get.Get Word16 -> Messages.Header -> IO Handlers.UserFlags
readUserStatus byteSource shortReader header =
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        trailingBytes = (Messages.messageSize header) - 2
    userFlags <- readShort
    _ <- readBytes trailingBytes
    return userFlags

readUserLogonNotification :: Net.IncomingByteSource -> Get.Get Word32 -> Messages.Header -> IO (Handlers.UserRefId, Handlers.PalaceUserCount)
readUserLogonNotification byteSource intReader header =
  do
    let readInt = Receive.readIntFromNetwork intReader byteSource
        userWhoLoggedOn = Messages.messageRefNumber header
    population <- readInt
    return (userWhoLoggedOn, population)

readMediaServerInfo :: Net.IncomingByteSource -> Messages.Header -> IO Net.URL
readMediaServerInfo byteSource header =
  Receive.readNullTerminatedTextFromNetwork byteSource $ Messages.messageSize header

-- TODO Finish this
readRoomDescription :: Net.IncomingByteSource -> Get.Get Word32 -> Get.Get Word16 -> Messages.Header -> IO Handlers.RoomDescription
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

    return Handlers.RoomDescription {
        Handlers.roomDescId = roomId
      , Handlers.roomDescName = roomName
      , Handlers.roomDescBackground = backgroundImageName
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
    _ <- Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()

-- TODO Finish this
readNewUserNotification :: Net.IncomingByteSource -> Messages.Header -> IO ()
readNewUserNotification byteSource header =
  do
    _ <- Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()

readTalk :: Net.IncomingByteSource -> Messages.Header -> IO Handlers.ChatData
readTalk byteSource header =
  do
    let speaking = Messages.messageRefNumber header
        readText = Receive.readNullTerminatedTextFromNetwork byteSource
        messageLength = Messages.messageSize header

    message <- truncateChatMessage <$> readText messageLength

    return Handlers.ChatData {
        Handlers.chatSource = speaking
      , Handlers.chatRecipient = Nothing
      , Handlers.chatMessage = message
      }

readEncodedTalk :: Net.IncomingByteSource -> Get.Get Word16 -> Messages.Header -> IO Handlers.ChatData
readEncodedTalk byteSource shortReader header =
  -- header messageSize field is actually the message checksum + 1 ??
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        speaking = Messages.messageRefNumber header

    fieldLength <- fromIntegral <$> readShort -- This is apparently the total length including these two bytes and the terminator
    obfuscated <- truncateChatMessage <$> init <$> readBytes (fieldLength - 2)
    let message = Illuminator.illuminate obfuscated

    return Handlers.ChatData {
        Handlers.chatSource = speaking
      , Handlers.chatRecipient = Nothing
      , Handlers.chatMessage = message
      }

-- Chat string can't be > 254 characters long
-- OpenPalace carries along original message but unclear why it's needed
-- Even iptscrae shouldn't use what isn't displayed, no?
truncateChatMessage :: [a] -> [a]
truncateChatMessage message = take 254 message


readMovement :: Net.IncomingByteSource -> Get.Get Word16 -> Messages.Header -> IO Handlers.MovementData
readMovement byteSource shortReader header =
  do
    let readShort = Receive.readShortFromNetwork shortReader byteSource
        mover = Messages.messageRefNumber header
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    return Handlers.MovementData { Handlers.x = x, Handlers.y = y, Handlers.userWhoMoved = mover }

readUnknown :: Net.IncomingByteSource -> Messages.Header -> IO ()
readUnknown byteSource header =
  do
    _ <- Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()
