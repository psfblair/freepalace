module FreePalace.Messages.PalaceProtocol.Inbound where

import           Control.Applicative
import           Control.Exception
import qualified Data.Convertible.Base                        as Convert

import qualified FreePalace.Domain                            as Domain
import qualified FreePalace.Handlers.Types                    as HandlerTypes
import qualified FreePalace.Messages                          as Messages
import qualified FreePalace.Messages.PalaceProtocol.Obfuscate as Illuminator
import qualified FreePalace.Net                               as Net
import qualified FreePalace.Net.Receive                       as Receive

readHeader :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Messages.Header
readHeader connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        readNextInt = Receive.readIntFromNetwork intReader byteSource
    msgType <- Messages.idToMessageType <$> readNextInt
    size <- readNextInt
    referenceNumber <- readNextInt
    return Messages.Header {
      Messages.messageType = msgType,
      Messages.messageSize = size,
      Messages.messageRefNumber = referenceNumber
    }

readHandshake :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO HandlerTypes.HandshakeData
readHandshake connection messageConverters =
  do
    header <- readHeader connection messageConverters -- TODO Time out if this takes too long
    let msgType    = Messages.messageType header
        userRefId  = Messages.messageRefNumber header
    case msgType of
      Messages.BigEndianServer    -> return  $ HandlerTypes.HandshakeData userRefId (HandlerTypes.PalaceProtocol connection HandlerTypes.BigEndian)
      Messages.LittleEndianServer -> return  $ HandlerTypes.HandshakeData userRefId (HandlerTypes.PalaceProtocol connection HandlerTypes.LittleEndian)
      Messages.UnknownServer      -> throwIO $ userError "Unknown server type"
      _                           -> throwIO $ userError "Invalid server type"


readAlternateLogonReply :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO (HandlerTypes.PuidCounter, HandlerTypes.PuidCrc)
readAlternateLogonReply connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        shortReader = Net.palaceShortReader messageConverters
        readInt = Receive.readIntFromNetwork intReader byteSource
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


readServerInfo :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO (HandlerTypes.PlaceName, HandlerTypes.ServerPermissions)
readServerInfo connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        readInt = Receive.readIntFromNetwork intReader byteSource
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

readUserStatus :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.UserFlags
readUserStatus connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        trailingBytes = (Messages.messageSize header) - 2
    userFlags <- readShort
    _ <- readBytes trailingBytes
    return userFlags

readUserLogonNotification :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO (Domain.UserRefId, HandlerTypes.PalaceUserCount)
readUserLogonNotification connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        readInt = Receive.readIntFromNetwork intReader byteSource
        userWhoLoggedOn = Messages.messageRefNumber header
    population <- readInt
    return (userWhoLoggedOn, population)

readMediaServerInfo :: Net.PalaceConnection -> Messages.Header -> IO Net.URL
readMediaServerInfo connection header =
  do
    let byteSource = Net.palaceByteSource connection
    Receive.readNullTerminatedTextFromNetwork byteSource $ Messages.messageSize header

 -- room name, background image, overlay images, props, hotspots, draw commands
-- TODO Finish this
readRoomDescription :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.RoomDescription
readRoomDescription connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
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

    return HandlerTypes.RoomDescription {
        HandlerTypes.roomDescId = roomId
      , HandlerTypes.roomDescName = roomName
      , HandlerTypes.roomDescBackground = backgroundImageName
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
readUserList :: Net.PalaceConnection -> Messages.Header -> IO ()
readUserList connection header =
  do
    let byteSource = Net.palaceByteSource connection
    _ <- Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()

-- TODO Finish this
readNewUserNotification :: Net.PalaceConnection -> Messages.Header -> IO ()
readNewUserNotification connection header =
  do
    let byteSource = Net.palaceByteSource connection
    _ <- Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()


readTalk :: Net.PalaceConnection -> Messages.Header -> IO HandlerTypes.ChatData
readTalk connection header =
  do
    let byteSource = Net.palaceByteSource connection
        speaking = Messages.messageRefNumber header
        readText = Receive.readNullTerminatedTextFromNetwork byteSource
        messageLength = Messages.messageSize header

    message <- truncateChatMessage <$> readText messageLength

    return HandlerTypes.ChatData {
        HandlerTypes.chatSource = speaking
      , HandlerTypes.chatRecipient = Nothing
      , HandlerTypes.chatMessage = message
      }

readEncodedTalk :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.ChatData
readEncodedTalk connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        speaking = Messages.messageRefNumber header

    -- header messageSize field is actually the message checksum + 1 ??
    fieldLength <- fromIntegral <$> readShort -- This is apparently the total length including these two bytes and the terminator
    obfuscated <- truncateChatMessage <$> init <$> readBytes (fieldLength - 2)
    let message = Illuminator.illuminate obfuscated

    return HandlerTypes.ChatData {
        HandlerTypes.chatSource = speaking
      , HandlerTypes.chatRecipient = Nothing
      , HandlerTypes.chatMessage = message
      }

-- Chat string can't be > 254 characters long
-- OpenPalace carries along original message but unclear why it's needed
-- Even iptscrae shouldn't use what isn't displayed, no?
truncateChatMessage :: [a] -> [a]
truncateChatMessage message = take 254 message

readMovement :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.MovementData
readMovement connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        mover = Messages.messageRefNumber header
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    return HandlerTypes.MovementData { HandlerTypes.x = x, HandlerTypes.y = y, HandlerTypes.userWhoMoved = mover }

readUnknownMessage :: Net.PalaceConnection -> Messages.Header -> IO ()
readUnknownMessage connection header =
  do
    let byteSource = Net.palaceByteSource connection
    _ <- Receive.readBytesFromNetwork byteSource $ Messages.messageSize header
    return ()
