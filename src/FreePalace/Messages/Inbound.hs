module FreePalace.Messages.Inbound where

import           Data.Word
import qualified FreePalace.Domain.Host as Host
import qualified FreePalace.Domain.Media as Media
import qualified FreePalace.Domain.Net as Net
import qualified FreePalace.Domain.User as User
import qualified FreePalace.Messages.PalaceProtocol.MessageTypes as PalaceMessages
-- TODO Would be nice not to have PalaceProtocol references in here

data Header = PalaceHeader {
    messageType      :: PalaceMessages.MessageType
  , messageSize      :: Int
  , messageRefNumber :: Int
} deriving Show


data InboundMessage = 
    HandshakeMessage Handshake
  | LogonReplyMessage LogonReply
  | ServerVersionMessage ServerVersion
  | ServerInfoMessage ServerInfoNotification
  | UserStatusMessage UserStatusNotification
  | UserLogonMessage UserLogonNotification    
  | MediaServerMessage MediaServerInfo
  | RoomDescriptionMessage RoomDescription
  | UserListMessage UserListing
  | NoOpMessage NoOp
  | NewUserMessage NewUser
  | ChatMessage Chat
  | MovementMessage MovementNotification


data Handshake = Handshake {
    userRefId :: User.UserRefId
  , protocolInfo :: ProtocolInfo
  } deriving Show
data  ProtocolInfo = PalaceProtocol Net.PalaceConnection Endianness
instance Show ProtocolInfo where
  show (PalaceProtocol _ _) = "Palace Protocol"
data  Endianness = BigEndian | LittleEndian deriving Show


data LogonReply = LogonReply {
    puidCounter :: PuidCounter
  , puidCrc :: PuidCrc
  } deriving Show
type PuidCounter = Int
type PuidCrc = Int


newtype ServerVersion = ServerVersion Int deriving Show


data ServerInfoNotification = ServerInfoNotification {
    serverName :: ServerName
  , serverPermissions :: ServerPermissions
  } deriving Show
type ServerName = String
type ServerPermissions = Int


newtype UserStatusNotification = UserStatusNotification UserFlags deriving Show
type UserFlags = Word16


data UserLogonNotification = UserLogonNotification {
    whoLoggedOn :: User.UserRefId
  , palaceUserCount :: PalaceUserCount
  } deriving Show
type PalaceUserCount = Int


newtype MediaServerInfo = MediaServerInfo Net.URL deriving Show


data RoomDescription = RoomDescription {
    roomDescId         :: Host.RoomId
  , roomDescName       :: Host.RoomName
  , roomDescBackground :: Media.ImageFilename
} deriving Show
{- Ultimately RoomDescription should also contain:
   -- overlay images - id, name, transpatency index
   -- hotspots - these come in layers - above avatars, above name tags, above all, above nothing.
           -- There is also a collection of all of them, and a hash of all of them by ID.
           -- Hotspots are read by count and initial offset. each record has a fixed length of 48 bytes.
           -- Hotspots also manage a collection of vertices and a collection of hotspot states
   -- loose props
   -- draw commands
-}

data UserListing = UserListing [ UserData ] deriving Show
data UserData = UserData {
    userId :: User.UserRefId
  , userName :: User.UserName
  , userRoomId :: Host.RoomId
  , userCoordinates :: Coordinates
  , userFaceInfo :: UserFaceInfo
  , userPropInfo :: PropInfo
  } deriving Show
data Coordinates = CartesianCoordinates { xPos :: Int, yPos :: Int } deriving Show
data PropInfo = PropInfo { numberOfProps :: Int, props :: [(PropId, PropCrc)] } deriving Show
type PropId = Int
type PropCrc = Int
data UserFaceInfo = UserFaceInfo { userFace :: UserFace, userColor :: UserColor } deriving Show
type UserFace = Int
type UserColor = Int

data NewUser = NewUser deriving Show


data Chat = Chat {
    chatSpeaker   :: User.UserRefId
  , chatRecipient :: Maybe User.UserRefId
  , chatMessage   :: ChatMessage
  , chatExposure  :: ChatExposure
  } deriving Show
type ChatMessage = String
data ChatExposure = PublicChat | PrivateChat deriving Show


data MovementNotification = MovementNotification { x :: Int, y :: Int, userWhoMoved :: User.UserRefId } deriving Show


newtype NoOp = NoOp String deriving Show

