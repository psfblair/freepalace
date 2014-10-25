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
  , disconnectedSettings :: Settings
  }

data Connected = Connected {
    protocolState :: ProtocolState
  , guiState :: GUI.Components
  , hostState :: HostState
  , hostDirectory :: HostDirectory
  , userState :: UserState
  , settings :: Settings
  } deriving Show

data HostDirectory = HostDirectory deriving Show

data Settings = Settings {
    thisUserName :: String
  -- TODO  maxCacheSize
  -- TODO  roomDimensions
  } deriving Show

data Protocol = PalaceProtocol

data ProtocolState = PalaceProtocolState PalaceConnection PalaceMessageConverters
instance Show ProtocolState where
   show _ = "ProtocolState"

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
  , mediaServer :: Maybe Net.URL
  -- TODO , serverVersion :: ServerVersion
  , roomList :: RoomList
  , userList :: UserList
  , chatLog :: ChatLog
  , currentRoomState :: Maybe CurrentRoomState
  } deriving Show


data RoomList = RoomList deriving Show
data UserList = UserList deriving Show
data ChatLog  = ChatLog {
  -- TODO logEntryTimestamp
  logEntries :: [ Messages.Communication ]
  } deriving Show

data CurrentRoomState = CurrentRoomState {
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
  } deriving Show

data UserState = NotLoggedIn { userName :: String }
               | LoggedIn    {
  userId :: Messages.UserId
  -- TODO props
  -- TODO sounds
  -- TODO settings
  } deriving Show

userIdFor :: UserState -> Int -> Messages.UserId
userIdFor userState refId = Messages.UserId {
  Messages.userRef = refId
  , Messages.userName = case userState of
                         NotLoggedIn { userName = name } -> name
                         LoggedIn    { userId = Messages.UserId { Messages.userName = name }} -> name 
  }

-- We will accept as legal characters A-Za-z0-9. - and _; any others will be encoded as url-encoded characters %20 etc. 
defaultSettings :: Settings
defaultSettings = Settings {
    thisUserName = "Haskell Curry"  -- TODO allow user to set user name
  }

initialHostStateFor :: Net.Hostname -> Net.PortId -> HostState
initialHostStateFor hostName portid = HostState {
    hostname = hostName
  , portId = portid
  , mediaServer = Nothing
  , roomList = RoomList
  , userList = UserList
  , chatLog = ChatLog []
  , currentRoomState = Nothing
  }

withMediaServerInfo :: Connected -> Net.URL -> Connected
withMediaServerInfo currentState mediaServerUri =
  currentState {
    hostState = (hostState currentState) {
       mediaServer = Just mediaServerUri
    }
  }

-- TODO This may get more complicated if/when the current room state is affected by more than a RoomDescription message
withRoomDescription :: Connected -> Messages.RoomDescription -> Connected
withRoomDescription currentState roomDescription =
  currentState {
    hostState = (hostState currentState) {
       currentRoomState = Just CurrentRoomState {
            roomId = Messages.roomId roomDescription
          , roomName = Messages.roomName roomDescription
          , roomBackgroundImageName = Messages.roomBackgroundImageName roomDescription
          }
       }
    }
