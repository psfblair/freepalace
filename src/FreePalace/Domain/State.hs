module FreePalace.Domain.State where

import qualified FreePalace.Domain.Chat  as Chat
import qualified FreePalace.Domain.GUI   as GUI
import qualified FreePalace.Domain.Host  as Host
import qualified FreePalace.Domain.Media as Media
import qualified FreePalace.Domain.Net   as Net
import qualified FreePalace.Domain.User  as User
import qualified Network.URI             as Network

data ClientState =
    DisconnectedState Disconnected
  | ConnectedState Connected

data Disconnected = Disconnected {
    disconnectedGui           :: GUI.Components
  , disconnectedHostDirectory :: Host.HostDirectory
  , disconnectedSettings      :: Settings
  }

data Connected = Connected {
    settings      :: Settings
  , protocolState :: ProtocolState
  , guiState      :: GUI.Components
  , hostState     :: HostState
  , hostDirectory :: Host.HostDirectory
  , userState     :: UserState
  } deriving Show

data Settings = Settings {
    thisUserName :: User.UserName
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
  , roomList         :: Host.RoomList
  , userList         :: User.UserList
  , chatLog          :: Chat.ChatLog
  , currentRoomState :: Maybe CurrentRoomState
  } deriving Show

data CurrentRoomState = CurrentRoomState {
    roomId                  :: Host.RoomId
  , roomName                :: Host.RoomName
  , roomBackgroundImageName :: Media.ImageFilename
  -- TODO inhabitants ??
  -- TODO overlay images - id, name, transpatency index
  -- TODO hotspots - these come in layers - above avatars, above name tags, above all, above nothing.
           -- Thereb is also a collection of all of them, and a hash of all of them by ID.
           -- Hotspots are read by count and initial offset. each record has a fixed length of 48 bytes.
           -- Hotspots also manage a collection of vertices and a collection of hotspot states
  -- TODO loose props
  -- TODO draw commands
  } deriving Show

data UserState = NotLoggedIn { username :: User.UserName }
               | LoggedIn    {
  userId :: User.UserId
  -- TODO props
  -- TODO sounds
  -- TODO settings
  } deriving Show
