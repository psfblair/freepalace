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
  , disconnectedHostDirectory :: HostDirectory  
  }

data Connected = Connected {
    protocolState :: ProtocolState
  , guiState :: GUI.Components
  , hostState :: HostState
  , hostDirectory :: HostDirectory
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

data HostState = HostState {
    hostname :: Net.Hostname
  , portId :: Net.PortId
  , mediaServer :: Maybe Net.URI
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

data HostDirectory = HostDirectory

data UserState = NotLoggedIn { userName :: String }
               | LoggedIn    {
  userId :: Messages.UserId
  -- TODO props
  -- TODO sounds
  -- TODO settings
  }

userIdFor :: UserState -> Int -> Messages.UserId
userIdFor userState refId = Messages.UserId {
  Messages.userRef = refId
  , Messages.userName = case userState of
                         NotLoggedIn { userName = name } -> name
                         LoggedIn    { userId = Messages.UserId { Messages.userName = name }} -> name 
  }

defaultUserName :: String
defaultUserName = "Haskell Curry" -- TODO allow user to set user name

initialHostStateFor :: Net.Hostname -> Net.PortId -> HostState
initialHostStateFor hostName portid = HostState {
    hostname = hostName
  , portId = portid
  , mediaServer = Nothing
  , roomListState = EmptyRoomList
  , userListState = EmptyUserList
  , logState = EmptyLog
  , currentRoomState = NoCurrentRoom
  }

withMediaServerInfo :: Connected -> Net.URI -> Connected
withMediaServerInfo currentState mediaServerUri =
  currentState {
    hostState = (hostState currentState) {
       mediaServer = Just mediaServerUri
    }
  }
