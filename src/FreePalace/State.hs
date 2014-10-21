module FreePalace.State where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Binary.Get as Get
import Data.Word
import Data.Int

import qualified FreePalace.Net.Types as Net
import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Messages as Messages

data ClientState =
    DisconnectedState Disconnected
  | ConnectedState Connected

data Disconnected = Disconnected {
    disconnectedGui :: GUI.Components
  }

data Connected = Connected {
    protocolState :: ProtocolState
  , guiState :: GUI.Components
  , hostState :: HostState
  , hostDirectoryState :: HostDirectoryState
  , userState :: UserState
  }

data Protocol = PalaceProtocol

data ProtocolState = PalaceProtocolState PalaceConnection PalaceMessageConverters

data PalaceConnection = PalaceConnection {
    palaceByteSource :: Net.IncomingByteSource
  , palaceByteSink :: Net.OutgoingByteSink
  }

data PalaceMessageConverters = PalaceMessageConverters {
    palaceShortWriter :: Word16 -> Builder.Builder
  , palaceIntWriter :: Int32 -> Builder.Builder
  , palaceShortReader :: Get.Get Word16
  , palaceIntReader :: Get.Get Word32
  }

data HostState = NoHost | HostState {
    hostname :: Net.Hostname
  , portId :: Net.PortId
  -- TODO , serverVersion :: ServerVersion
  , roomListState :: RoomListState
  , userListState :: UserListState
  , logState :: LogState
  , currentRoomState :: CurrentRoomState
  }


data RoomListState = EmptyRoomList
data UserListState = EmptyUserList
data LogState = EmptyLog | LogState {
  -- TODO logEntryTimestamp
  logEntry :: Messages.Communication
  }

data CurrentRoomState =  NoCurrentRoom | CurrentRoomState {
    roomId :: Int
  , roomName :: String
  , roomBackgroundImageName :: String
  -- TODO inhabitants ??
  -- TODO overlay images - id, name, transpatency index
  -- TODO hotspots - these come in layers - above avatars, above name tags, above all, above nothing.
           -- There is also a collection of all of them, and a hash of all of them by ID.
           -- Hotspots are read by count and initial offset. each record has a fixed length of 48 bytes.
           -- Hotspots also manage a collection of vertices and a collection of hotspot states
  -- TODO loose props
  -- TODO draw commands
  }

data HostDirectoryState = EmptyHostDirectory | HostDirectoryState

data UserState = NotLoggedIn | UserState {
  userId :: Messages.UserId
  -- TODO props
  -- TODO sounds
  -- TODO settings
  }

userIdFor :: Int -> Messages.UserId
userIdFor refId = Messages.UserId {
    Messages.userRef = refId
  , Messages.userName = "Haskell Curry" -- TODO allow user to set user name
  }

initialHostStateFor :: Net.Hostname -> Net.PortId -> HostState
initialHostStateFor hostName portid = HostState {
    hostname = hostName
  , portId = portid
  , roomListState = EmptyRoomList
  , userListState = EmptyUserList
  , logState = EmptyLog
  , currentRoomState = NoCurrentRoom
  }

