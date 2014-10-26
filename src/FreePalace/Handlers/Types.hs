module FreePalace.Handlers.Types where

import           Data.Word
import qualified FreePalace.Domain as Domain
import qualified FreePalace.Net    as Net

data HandlerUpdate = HandshakeUpdate { userRefId :: Domain.UserRefId, converters :: Net.PalaceMessageConverters }

type UserRefId = Int
type UserFlags = Word16
type PlaceName = String
type ServerPermissions = Int
type PalaceUserCount = Int
type ChatMessage = String
type RoomId = Int
type RoomName = String
type ImageName = String
type PuidCounter = Int
type PuidCrc = Int

data ChatData = ChatData {
    chatSource    :: UserRefId
  , chatRecipient :: Maybe UserRefId
  , chatMessage   :: String
  }

data MovementData = MovementData { x :: Int, y :: Int, userWhoMoved :: UserRefId } deriving Show

data RoomDescription = RoomDescription {
    roomDescId         :: RoomId
  , roomDescName       :: RoomName
  , roomDescBackground :: ImageName
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
