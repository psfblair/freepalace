module FreePalace.Messages.PalaceProtocol.InboundReader where

import           Control.Applicative
import           Control.Exception
import qualified Data.Convertible.Base                           as Convert

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
  in case messageType of
    PalaceMsgTypes.BigEndianServer    -> readHandshake PalaceMsgTypes.BigEndianServer connection header
    PalaceMsgTypes.LittleEndianServer -> readHandshake PalaceMsgTypes.LittleEndianServer connection header
    PalaceMsgTypes.UnknownServer      -> readHandshake PalaceMsgTypes.UnknownServer connection header
    PalaceMsgTypes.AlternateLogonReply -> readAlternateLogonReply connection messageConverters
    PalaceMsgTypes.ServerVersion -> readServerVersion header
    PalaceMsgTypes.ServerInfo -> readServerInfo connection messageConverters
    PalaceMsgTypes.UserStatus -> readUserStatus connection messageConverters header
    PalaceMsgTypes.UserLoggedOnAndMax -> readUserLogonNotification connection messageConverters header
    PalaceMsgTypes.GotHttpServerLocation -> readMediaServerInfo connection header
    PalaceMsgTypes.GotRoomDescription -> readRoomDescription connection messageConverters header
    PalaceMsgTypes.GotUserList -> readUserList connection messageConverters header
    PalaceMsgTypes.RoomDescend -> return $ Inbound.NoOpMessage (Inbound.NoOp $ show messageType)
    -- RoomDescend message just means we're done receiving the room description & user list
    PalaceMsgTypes.UserNew -> readNewUserNotification connection messageConverters
    -- End logon sequence

    PalaceMsgTypes.Talk -> readTalk connection header Inbound.PublicChat
    PalaceMsgTypes.CrossRoomWhisper -> readTalk connection header Inbound.PrivateChat
    PalaceMsgTypes.Say -> readEncodedTalk connection messageConverters header Inbound.PublicChat
    PalaceMsgTypes.Whisper -> readEncodedTalk connection messageConverters header Inbound.PrivateChat
    PalaceMsgTypes.Move -> readMovement connection messageConverters header

    PalaceMsgTypes.Superuser -> readUnknownMessage connection header
    PalaceMsgTypes.SusrMsg -> readUnknownMessage connection header
    PalaceMsgTypes.GlobalMsg -> readUnknownMessage connection header
    PalaceMsgTypes.RoomMsg -> readUnknownMessage connection header

    PalaceMsgTypes.GotRoomDescriptionAlt -> readUnknownMessage connection header
    PalaceMsgTypes.RequestRoomList -> readUnknownMessage connection header
    PalaceMsgTypes.GotRoomList -> readUnknownMessage connection header
    PalaceMsgTypes.GotReplyOfAllRooms -> readUnknownMessage connection header
    PalaceMsgTypes.RequestUserList -> readUnknownMessage connection header
    PalaceMsgTypes.GotReplyOfAllUsers -> readUnknownMessage connection header

    PalaceMsgTypes.Logon -> readUnknownMessage connection header
    PalaceMsgTypes.Authenticate -> readUnknownMessage connection header
    PalaceMsgTypes.Authresponse -> readUnknownMessage connection header
    PalaceMsgTypes.Bye -> readUnknownMessage connection header

    PalaceMsgTypes.ChangeName -> readUnknownMessage connection header
    PalaceMsgTypes.UserRename -> readUnknownMessage connection header
    PalaceMsgTypes.UserDescription -> readUnknownMessage connection header
    PalaceMsgTypes.UserColor -> readUnknownMessage connection header
    PalaceMsgTypes.UserFace -> readUnknownMessage connection header
    PalaceMsgTypes.UserProp -> readUnknownMessage connection header
    PalaceMsgTypes.UserExitRoom -> readUnknownMessage connection header
    PalaceMsgTypes.UserLeaving -> readUnknownMessage connection header
    PalaceMsgTypes.GotoRoom -> readUnknownMessage connection header
    PalaceMsgTypes.Blowthru -> readUnknownMessage connection header
    PalaceMsgTypes.NavError -> readUnknownMessage connection header

    PalaceMsgTypes.PropNew -> readUnknownMessage connection header
    PalaceMsgTypes.PropMove -> readUnknownMessage connection header
    PalaceMsgTypes.PropDelete -> readUnknownMessage connection header
    PalaceMsgTypes.PictMove -> readUnknownMessage connection header

    PalaceMsgTypes.IncomingFile -> readUnknownMessage connection header
    PalaceMsgTypes.RequestAsset -> readUnknownMessage connection header
    PalaceMsgTypes.AssetQuery -> readUnknownMessage connection header
    PalaceMsgTypes.AssetIncoming -> readUnknownMessage connection header
    PalaceMsgTypes.AssetRegi -> readUnknownMessage connection header

    PalaceMsgTypes.DoorLock -> readUnknownMessage connection header
    PalaceMsgTypes.DoorUnlock -> readUnknownMessage connection header
    PalaceMsgTypes.SpotState -> readUnknownMessage connection header
    PalaceMsgTypes.SpotMove -> readUnknownMessage connection header

    PalaceMsgTypes.Draw -> readUnknownMessage connection header
    PalaceMsgTypes.DrawCmd -> readUnknownMessage connection header
    
    PalaceMsgTypes.Pinged -> readUnknownMessage connection header
    PalaceMsgTypes.PingBack -> readUnknownMessage connection header
    PalaceMsgTypes.ServerDown -> readUnknownMessage connection header
    PalaceMsgTypes.ConnectionDied -> readUnknownMessage connection header

    PalaceMsgTypes.UnknownMessage -> readUnknownMessage connection header

    
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

readAlternateLogonReply :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Inbound.InboundMessage
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

    return . Inbound.LogonReplyMessage $ Inbound.LogonReply puidCounter puidCrc -- TODO When do these get used?

readServerInfo :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Inbound.InboundMessage
readServerInfo connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        readInt = Receive.readIntFromNetwork intReader byteSource
        readByte = Receive.readByteFromNetwork byteSource
        readText = Receive.readTextFromNetwork byteSource
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

readUserStatus :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> IO Inbound.InboundMessage
readUserStatus connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        trailingBytes = (Inbound.messageSize header) - 2
    userFlags <- readShort
    _ <- readBytes trailingBytes
    return . Inbound.UserStatusMessage $ Inbound.UserStatusNotification userFlags

readUserLogonNotification :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> IO Inbound.InboundMessage
readUserLogonNotification connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        readInt = Receive.readIntFromNetwork intReader byteSource
        userWhoLoggedOn = Inbound.messageRefNumber header
    population <- readInt
    return . Inbound.UserLogonMessage $ Inbound.UserLogonNotification userWhoLoggedOn population

readMediaServerInfo :: Net.PalaceConnection -> Inbound.Header -> IO Inbound.InboundMessage
readMediaServerInfo connection header =
  do
    let byteSource = Net.palaceByteSource connection
    url <- Receive.readNullTerminatedTextFromNetwork byteSource $ Inbound.messageSize header
    return . Inbound.MediaServerMessage $ Inbound.MediaServerInfo url

-- room name, background image, overlay images, props, hotspots, draw commands
-- TODO Finish this
readRoomDescription :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> IO Inbound.InboundMessage
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
readUserList :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> IO Inbound.InboundMessage
readUserList connection messageConverters header =
  do
    let numberOfUsers = Inbound.messageRefNumber header
    userList <- sequence $ replicate numberOfUsers (readSingleUser connection messageConverters)
    return . Inbound.UserListMessage $ Inbound.UserListing userList

readSingleUser :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Inbound.UserData
readSingleUser connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        readByte = Receive.readByteFromNetwork byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        readText = Receive.readTextFromNetwork byteSource
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        intReader = Net.palaceIntReader messageConverters
        readInt = Receive.readIntFromNetwork intReader byteSource
        
        toPairs (propId:propCrc:xs) = (propId,propCrc) : toPairs xs
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
    

readNewUserNotification :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Inbound.InboundMessage
readNewUserNotification connection messageConverters =
  do
    userData <- readSingleUser connection messageConverters
    return $ Inbound.NewUserMessage userData


readTalk :: Net.PalaceConnection -> Inbound.Header -> Inbound.ChatExposure -> IO Inbound.InboundMessage
readTalk connection header exposure =
  do
    let byteSource = Net.palaceByteSource connection
        speaking = Inbound.messageRefNumber header
        readText = Receive.readNullTerminatedTextFromNetwork byteSource
        messageLength = Inbound.messageSize header

    message <- truncateChatMessage <$> readText messageLength

    let chat = Inbound.Chat {
            Inbound.chatSpeaker = speaking
          , Inbound.chatRecipient = Nothing
          , Inbound.chatMessage = message
          , Inbound.chatExposure = exposure
          }
    return $ Inbound.ChatMessage chat

readEncodedTalk :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> Inbound.ChatExposure -> IO Inbound.InboundMessage
readEncodedTalk connection messageConverters header exposure =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        readBytes = Receive.readBytesFromNetwork byteSource
        speaking = Inbound.messageRefNumber header

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

readMovement :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Inbound.Header -> IO Inbound.InboundMessage
readMovement connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
        readShort = Receive.readShortFromNetwork shortReader byteSource
        mover = Inbound.messageRefNumber header
    y <- fromIntegral <$> readShort
    x <- fromIntegral <$> readShort
    let notification = Inbound.MovementNotification { Inbound.x = x, Inbound.y = y, Inbound.userWhoMoved = mover }
    return $ Inbound.MovementMessage notification

readUnknownMessage :: Net.PalaceConnection -> Inbound.Header -> IO Inbound.InboundMessage
readUnknownMessage connection header =
  do
    let byteSource = Net.palaceByteSource connection
    _ <- Receive.readBytesFromNetwork byteSource $ Inbound.messageSize header
    let diagnosticMessage = "Unknown messsage: " ++ (show header)
    return $ Inbound.NoOpMessage (Inbound.NoOp diagnosticMessage)
