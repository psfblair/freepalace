module FreePalace.Messages.Inbound where

import Control.Applicative
import qualified Data.Convertible.Base as Convert
import Data.Convertible.Instances.Num
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
        readByte = Net.readByte communicators
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
readRoomDescription :: Net.Communicators -> Messages.Header -> IO (RoomDescription)
readRoomDescription communicators header =
  do
    let readShort = Net.readShort communicators
        readInt = Net.readInt communicators
        readBytes = Net.readBytes communicators
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
