module FreePalace.Messages.Inbound where

import           Data.Word
import qualified FreePalace.Domain as Domain
import qualified FreePalace.Net as Net

data InboundHandshake = InboundHandshake {
    userRefId :: Domain.UserRefId
  , protocolInfo :: ProtocolInfo
  } deriving Show

data ProtocolInfo = PalaceProtocol Net.PalaceConnection Endianness
instance Show ProtocolInfo where
  show (PalaceProtocol _ _) = "Palace Protocol"
data Endianness = BigEndian | LittleEndian deriving Show

data InboundLogonReply = InboundLogonReply {
    puidCounter :: PuidCounter
  , puidCrc :: PuidCrc
  } deriving Show
type PuidCounter = Int
type PuidCrc = Int

data InboundServerInfo = InboundServerInfo {
    serverName :: ServerName
  , serverPermissions :: ServerPermissions
  } deriving Show
type ServerName = String
type ServerPermissions = Int

data InboundUserStatus = InboundUserStatus UserFlags deriving Show
type UserFlags = Word16

data InboundUserLogonNotification = InboundUserLogonNotification {
    whoLoggedOn :: Domain.UserRefId
  , palaceUserCount :: PalaceUserCount
  } deriving Show
type PalaceUserCount = Int

data InboundMediaServerInfo = InboundMediaServerInfo Net.URL deriving Show

data InboundRoomDescription = InboundRoomDescription {
    roomDescId         :: Domain.RoomId
  , roomDescName       :: Domain.RoomName
  , roomDescBackground :: Domain.ImageFilename
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

data InboundUserList = InboundUserList deriving Show

data InboundNewUserNotification = InboundNewUserNotification deriving Show

data InboundChat = InboundChat {
    chatSpeaker   :: Domain.UserRefId
  , chatRecipient :: Maybe Domain.UserRefId
  , chatMessage   :: ChatMessage
  } deriving Show
type ChatMessage = String

data InboundMovement = InboundMovement { x :: Int, y :: Int, userWhoMoved :: Domain.UserRefId } deriving Show
