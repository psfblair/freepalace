module FreePalace.State where

import qualified FreePalace.Domain    as Domain
import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Net       as Net
import qualified Network.URI          as Network

data ClientState =
    DisconnectedState Disconnected
  | ConnectedState Connected

data Disconnected = Disconnected {
    disconnectedGui           :: GUI.Components
  , disconnectedHostDirectory :: Domain.HostDirectory
  , disconnectedSettings      :: Settings
  }

data Connected = Connected {
    settings      :: Settings
  , protocolState :: ProtocolState
  , guiState      :: GUI.Components
  , hostState     :: HostState
  , hostDirectory :: Domain.HostDirectory
  , userState     :: UserState
  } deriving Show

data Settings = Settings {
    thisUserName :: Domain.UserName
  -- TODO  maxCacheSize
  -- TODO  roomDimensions
  } deriving Show

data ProtocolState = PalaceProtocolState Net.PalaceConnection Net.PalaceMessageConverters
instance Show ProtocolState where
   show _ = "ProtocolState"

data HostState = HostState {
    hostname         :: Net.Hostname
  , portId           :: Net.PortId
  -- , serverName       :: Maybe Domain.ServerName
  -- TODO , serverVersion :: ServerVersion
  , mediaServer      :: Maybe Network.URI
  , roomList         :: Domain.RoomList
  , userList         :: Domain.UserList
  , chatLog          :: Domain.ChatLog
  , currentRoomState :: Maybe CurrentRoomState
  } deriving Show

data CurrentRoomState = CurrentRoomState {
    roomId                  :: Domain.RoomId
  , roomName                :: Domain.RoomName
  , roomBackgroundImageName :: Domain.ImageFilename
  -- TODO inhabitants ??
  -- TODO overlay images - id, name, transpatency index
  -- TODO hotspots - these come in layers - above avatars, above name tags, above all, above nothing.
           -- Thereb is also a collection of all of them, and a hash of all of them by ID.
           -- Hotspots are read by count and initial offset. each record has a fixed length of 48 bytes.
           -- Hotspots also manage a collection of vertices and a collection of hotspot states
  -- TODO loose props
  -- TODO draw commands
  } deriving Show

data UserState = NotLoggedIn { username :: Domain.UserName }
               | LoggedIn    {
  userId :: Domain.UserId
  -- TODO props
  -- TODO sounds
  -- TODO settings
  } deriving Show
