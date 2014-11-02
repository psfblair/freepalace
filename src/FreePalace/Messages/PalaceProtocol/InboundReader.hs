module FreePalace.Messages.PalaceProtocol.InboundReader where

import           Control.Applicative
import           Control.Exception
import qualified Data.Convertible.Base                           as Convert
import           Data.Word

import qualified FreePalace.Domain.Net                           as Net
import qualified FreePalace.Messages.Inbound                     as Inbound
import qualified FreePalace.Messages.PalaceProtocol.MessageTypes as PalaceMsgTypes
import qualified FreePalace.Messages.PalaceProtocol.Obfuscate    as Illuminator
import qualified FreePalace.Net.Receive                          as Receive

readHeader :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Inbound.Header
readHeader connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        readNextInt = Receive.readIntFromNetwork intReader byteSource
    msgType <- PalaceMsgTypes.idToMessageType <$> readNextInt
    size <- readNextInt
    referenceNumber <- readNextInt
    return Inbound.PalaceHeader {
      Inbound.messageType = msgType,
      Inbound.messageSize = size,
      Inbound.messageRefNumber = referenceNumber
    }

readMessage :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> IO Inbound.InboundMessage
readMessage connection messageConverters header =
  let messageType = Inbound.messageType header
      byteSource = Net.palaceByteSource connection
      readByte = Receive.readByteFromNetwork byteSource
      readBytes = Receive.readBytesFromNetwork byteSource
      readShort = Receive.readShortFromNetwork (Net.palaceShortReader messageConverters) byteSource
      readInt = Receive.readIntFromNetwork (Net.palaceIntReader messageConverters) byteSource
      readText = Receive.readTextFromNetwork byteSource
      readNullTerminatedText = Receive.readNullTerminatedTextFromNetwork byteSource
      
  in case messageType of
    PalaceMsgTypes.BigEndianServer    -> readHandshake PalaceMsgTypes.BigEndianServer connection header
    PalaceMsgTypes.LittleEndianServer -> readHandshake PalaceMsgTypes.LittleEndianServer connection header
    PalaceMsgTypes.UnknownServer      -> readHandshake PalaceMsgTypes.UnknownServer connection header
    PalaceMsgTypes.AlternateLogonReply -> readAlternateLogonReply readByte readShort readInt readText
    PalaceMsgTypes.ServerVersion -> readServerVersion header
    PalaceMsgTypes.ServerInfo -> readServerInfo readByte readInt readText
    PalaceMsgTypes.UserStatus -> readUserStatus readBytes readShort header
    PalaceMsgTypes.UserLoggedOnAndMax -> readUserLogonNotification readInt header
    PalaceMsgTypes.GotHttpServerLocation -> readMediaServerInfo readNullTerminatedText header
    PalaceMsgTypes.GotRoomDescription -> readRoomDescription readBytes readShort readInt header
    PalaceMsgTypes.GotUserList -> readUserList readByte readBytes readShort readInt readText header
    PalaceMsgTypes.RoomDescend -> return $ Inbound.NoOpMessage (Inbound.NoOp $ show messageType)
    -- RoomDescend message just means we're done receiving the room description & user list
    PalaceMsgTypes.UserNew -> readUserEnteredRoomNotification readByte readBytes readShort readInt readText
    -- End logon sequence

    PalaceMsgTypes.Talk -> readTalk readNullTerminatedText header Inbound.PublicChat
    PalaceMsgTypes.CrossRoomWhisper -> readTalk readNullTerminatedText header Inbound.PrivateChat
    PalaceMsgTypes.Say -> readEncodedTalk readBytes readShort header Inbound.PublicChat
    PalaceMsgTypes.Whisper -> readEncodedTalk readBytes readShort header Inbound.PrivateChat
    PalaceMsgTypes.Move -> readMovement readShort header

    PalaceMsgTypes.UserExitRoom -> readUserExitedRoomNotification header
    PalaceMsgTypes.UserLeaving -> readUserDisconnectedNotification readInt header

    PalaceMsgTypes.Superuser -> readUnknownMessage readBytes header
    PalaceMsgTypes.SusrMsg -> readUnknownMessage readBytes header
    PalaceMsgTypes.GlobalMsg -> readUnknownMessage readBytes header
    PalaceMsgTypes.RoomMsg -> readUnknownMessage readBytes header

    PalaceMsgTypes.GotRoomDescriptionAlt -> readUnknownMessage readBytes header
    PalaceMsgTypes.RequestRoomList -> readUnknownMessage readBytes header
    PalaceMsgTypes.GotRoomList -> readUnknownMessage readBytes header
    PalaceMsgTypes.GotReplyOfAllRooms -> readUnknownMessage readBytes header
    PalaceMsgTypes.RequestUserList -> readUnknownMessage readBytes header
    PalaceMsgTypes.GotReplyOfAllUsers -> readUnknownMessage readBytes header

    PalaceMsgTypes.Logon -> readUnknownMessage readBytes header
    PalaceMsgTypes.Authenticate -> readUnknownMessage readBytes header
    PalaceMsgTypes.Authresponse -> readUnknownMessage readBytes header
    PalaceMsgTypes.Bye -> readUnknownMessage readBytes header

    PalaceMsgTypes.ChangeName -> readUnknownMessage readBytes header
    PalaceMsgTypes.UserRename -> readUnknownMessage readBytes header
    PalaceMsgTypes.UserDescription -> readUnknownMessage readBytes header
    PalaceMsgTypes.UserColor -> readUnknownMessage readBytes header
    PalaceMsgTypes.UserFace -> readUnknownMessage readBytes header
    PalaceMsgTypes.UserProp -> readUnknownMessage readBytes header
    PalaceMsgTypes.GotoRoom -> readUnknownMessage readBytes header
    PalaceMsgTypes.Blowthru -> readUnknownMessage readBytes header
    PalaceMsgTypes.NavError -> readUnknownMessage readBytes header

    PalaceMsgTypes.PropNew -> readUnknownMessage readBytes header
    PalaceMsgTypes.PropMove -> readUnknownMessage readBytes header
    PalaceMsgTypes.PropDelete -> readUnknownMessage readBytes header
    PalaceMsgTypes.PictMove -> readUnknownMessage readBytes header

    PalaceMsgTypes.IncomingFile -> readUnknownMessage readBytes header
    PalaceMsgTypes.RequestAsset -> readUnknownMessage readBytes header
    PalaceMsgTypes.AssetQuery -> readUnknownMessage readBytes header
    PalaceMsgTypes.AssetIncoming -> readUnknownMessage readBytes header
    PalaceMsgTypes.AssetRegi -> readUnknownMessage readBytes header

    PalaceMsgTypes.DoorLock -> readUnknownMessage readBytes header
    PalaceMsgTypes.DoorUnlock -> readUnknownMessage readBytes header
    PalaceMsgTypes.SpotState -> readUnknownMessage readBytes header
    PalaceMsgTypes.SpotMove -> readUnknownMessage readBytes header

    PalaceMsgTypes.Draw -> readUnknownMessage readBytes header
    PalaceMsgTypes.DrawCmd -> readUnknownMessage readBytes header
    
    PalaceMsgTypes.Pinged -> readUnknownMessage readBytes header
    PalaceMsgTypes.PingBack -> readUnknownMessage readBytes header
    PalaceMsgTypes.ServerDown -> readUnknownMessage readBytes header
    PalaceMsgTypes.ConnectionDied -> readUnknownMessage readBytes header

    PalaceMsgTypes.UnknownMessage -> readUnknownMessage readBytes header

    
readHandshake :: PalaceMsgTypes.MessageType -> Net.PalaceConnection -> Inbound.Header -> IO Inbound.InboundMessage
readHandshake PalaceMsgTypes.BigEndianServer connection header = do
    let userRefId  = Inbound.messageRefNumber header
        handshake = Inbound.Handshake userRefId (Inbound.PalaceProtocol connection Inbound.BigEndian)
    return $ Inbound.HandshakeMessage handshake
readHandshake PalaceMsgTypes.LittleEndianServer connection header = do
    let userRefId  = Inbound.messageRefNumber header
        handshake = Inbound.Handshake userRefId (Inbound.PalaceProtocol connection Inbound.LittleEndian)
    return $ Inbound.HandshakeMessage handshake
readHandshake PalaceMsgTypes.UnknownServer _ _ = throwIO $ userError "Unknown server type"
readHandshake _ _ _ =  throwIO $ userError "Invalid server type"

readAlternateLogonReply :: IO Word8 -> IO Word16 -> IO Int -> (Int -> IO String) -> IO Inbound.InboundMessage
readAlternateLogonReply readByte readShort readInt readText =
  do
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

    return . Inbound.LogonReplyMessage $ Inbound.LogonReply puidCounter puidCrc -- TODO When do these get used?

readServerInfo :: IO Word8 -> IO Int -> (Int -> IO String) -> IO Inbound.InboundMessage
readServerInfo readByte readInt readText =
  do
    permissions <- readInt
    size <- fromIntegral <$> readByte
    serverName <- readText size
    return . Inbound.ServerInfoMessage $ Inbound.ServerInfoNotification serverName permissions
    {- OpenPalace notes:
	Weird -- this message is supposed to include options and upload/download capabilities, but doesn't.
          options = socket.readUnsignedInt();
          uploadCapabilities = socket.readUnsignedInt();
          downloadCapabilities = socket.readUnsignedInt();
    -}

readServerVersion :: Inbound.Header -> IO Inbound.InboundMessage
readServerVersion header = return . Inbound.ServerVersionMessage $ Inbound.ServerVersion (Inbound.messageRefNumber header)

readUserStatus :: (Int -> IO [Word8]) -> IO Word16 -> Inbound.Header -> IO Inbound.InboundMessage
readUserStatus readBytes readShort header =
  do
    let trailingBytes = (Inbound.messageSize header) - 2
    userFlags <- readShort
    _ <- readBytes trailingBytes
    return . Inbound.UserStatusMessage $ Inbound.UserStatusNotification userFlags

readUserLogonNotification :: IO Int -> Inbound.Header -> IO Inbound.InboundMessage
readUserLogonNotification readInt header =
  do
    let userWhoLoggedOn = Inbound.messageRefNumber header
    population <- readInt
    return . Inbound.UserLogonMessage $ Inbound.UserLogonNotification userWhoLoggedOn population

readMediaServerInfo :: (Int -> IO String) -> Inbound.Header -> IO Inbound.InboundMessage
readMediaServerInfo readNullTerminatedText header =
  do
    url <- readNullTerminatedText $ Inbound.messageSize header
    return . Inbound.MediaServerMessage $ Inbound.MediaServerInfo url

-- room name, background image, overlay images, props, hotspots, draw commands
-- TODO Finish this
readRoomDescription :: (Int -> IO [Word8]) -> IO Word16 -> IO Int -> Inbound.Header -> IO Inbound.InboundMessage
readRoomDescription readBytes readShort readInt header =
  do
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
    let paddingLength = (Inbound.messageSize header) - roomDataLength - 40 -- We have used (roomDataLength + 40 bytes so far)
    padding <- readBytes paddingLength

    -- This message doesn't come in very often so we're not going to use arrays unless it really makes things slow.
    let roomNameLength = fromIntegral $ head $ drop roomNameOffset roomData
        roomName = map Convert.convert $ take roomNameLength $ drop (roomNameOffset + 1) roomData
        backgroundImageNameLength = fromIntegral $ head $ drop backgroundImageNameOffset roomData
        backgroundImageName = map Convert.convert $ take backgroundImageNameLength $ drop (backgroundImageNameOffset + 1) roomData
        roomDescription = Inbound.RoomDescription {
            Inbound.roomDescId = roomId
          , Inbound.roomDescName = roomName
          , Inbound.roomDescBackground = backgroundImageName
          }
    return $ Inbound.RoomDescriptionMessage roomDescription
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


-- List of users in the current room
readUserList :: IO Word8 -> (Int -> IO [Word8]) -> IO Word16 -> IO Int -> (Int -> IO String) -> Inbound.Header -> IO Inbound.InboundMessage
readUserList readByte readBytes readShort readInt readText header =
  do
    let numberOfUsers = Inbound.messageRefNumber header
    userList <- sequence $ replicate numberOfUsers (readSingleUser readByte readBytes readShort readInt readText)
    return . Inbound.UserListMessage $ Inbound.UserListing userList

readSingleUser :: IO Word8 -> (Int -> IO [Word8]) -> IO Word16 -> IO Int -> (Int -> IO String) -> IO Inbound.UserData
readSingleUser readByte readBytes readShort readInt readText =
  do
    let toPairs (propId:propCrc:xs) = (propId,propCrc) : toPairs xs
        toPairs _ = []
        
    userId <- readInt
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    propInfos <- sequence $ replicate 18 readInt  -- 9 slots for prop images - pairs of prop ID and propCrc (PalaceProtocol specific?)
    roomId <- fromIntegral <$> readShort
    face <- fromIntegral <$> readShort
    color <- fromIntegral <$> readShort
    _ <- readShort -- 0?
    _ <- readShort -- 0?
    numberOfProps <- fromIntegral <$> readShort
    userNameLength <- fromIntegral <$> readByte
    userName <- readText userNameLength
    _ <- readBytes $ 31 - userNameLength

    let propsList = toPairs propInfos
        propInfo = Inbound.PropInfo {
            Inbound.numberOfProps = numberOfProps
          , Inbound.props = take numberOfProps propsList
          }
    return $ Inbound.UserData { 
        Inbound.userId = userId
      , Inbound.userName = userName
      , Inbound.userRoomId = roomId
      , Inbound.userCoordinates = Inbound.CartesianCoordinates { Inbound.xPos = x, Inbound.yPos = y }
      , Inbound.userFaceInfo = Inbound.UserFaceInfo { Inbound.userFace = face, Inbound.userColor = color }
      , Inbound.userPropInfo = propInfo
      }

readUserEnteredRoomNotification :: IO Word8 -> (Int -> IO [Word8]) -> IO Word16 -> IO Int -> (Int -> IO String) -> IO Inbound.InboundMessage
readUserEnteredRoomNotification readByte readBytes readShort readInt readText =
  do
    userData <- readSingleUser readByte readBytes readShort readInt readText
    return $ Inbound.UserEnteredRoomMessage userData

readUserExitedRoomNotification :: Inbound.Header -> IO Inbound.InboundMessage
readUserExitedRoomNotification header =
   do
    let userId = Inbound.messageRefNumber header
    return . Inbound.UserExitedRoomMessage $ Inbound.UserExitedRoom userId

readUserDisconnectedNotification :: IO Int -> Inbound.Header -> IO Inbound.InboundMessage
readUserDisconnectedNotification readInt header =
  do
    population <- readInt
    return . Inbound.UserDisconnectedMessage $ Inbound.UserDisconnected population (Inbound.messageRefNumber header)

readTalk :: (Int -> IO String) -> Inbound.Header -> Inbound.ChatExposure -> IO Inbound.InboundMessage
readTalk readNullTerminatedText header exposure =
  do
    let speaking = Inbound.messageRefNumber header
        messageLength = Inbound.messageSize header

    message <- truncateChatMessage <$> readNullTerminatedText messageLength

    let chat = Inbound.Chat {
            Inbound.chatSpeaker = speaking
          , Inbound.chatRecipient = Nothing
          , Inbound.chatMessage = message
          , Inbound.chatExposure = exposure
          }
    return $ Inbound.ChatMessage chat

readEncodedTalk :: (Int -> IO [Word8]) -> IO Word16 -> Inbound.Header -> Inbound.ChatExposure -> IO Inbound.InboundMessage
readEncodedTalk readBytes readShort header exposure =
  do
    let speaking = Inbound.messageRefNumber header

    -- header messageSize field is actually the message checksum + 1 ??
    fieldLength <- fromIntegral <$> readShort -- This is apparently the total length including these two bytes and the terminator
    obfuscated <- truncateChatMessage <$> init <$> readBytes (fieldLength - 2)
    let message = Illuminator.illuminate obfuscated
        chat = Inbound.Chat {
            Inbound.chatSpeaker = speaking
          , Inbound.chatRecipient = Nothing
          , Inbound.chatMessage = message
          , Inbound.chatExposure = exposure
          }
    return $ Inbound.ChatMessage chat

-- Chat string can't be > 254 characters long
-- OpenPalace carries along original message but unclear why it's needed
-- Even iptscrae shouldn't use what isn't displayed, no?
truncateChatMessage :: [a] -> [a]
truncateChatMessage message = take 254 message

readMovement :: IO Word16 -> Inbound.Header -> IO Inbound.InboundMessage
readMovement readShort header =
  do
    let mover = Inbound.messageRefNumber header
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    let notification = Inbound.MovementNotification { Inbound.x = x, Inbound.y = y, Inbound.userWhoMoved = mover }
    return $ Inbound.MovementMessage notification

readUnknownMessage :: (Int -> IO [Word8]) -> Inbound.Header -> IO Inbound.InboundMessage
readUnknownMessage readBytes header =
  do
    _ <- readBytes $ Inbound.messageSize header
    let diagnosticMessage = "Unknown messsage: " ++ (show header)
    return $ Inbound.NoOpMessage (Inbound.NoOp diagnosticMessage)
