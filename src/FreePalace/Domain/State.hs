module FreePalace.Domain.State where

import qualified Data.Map                    as Map

import qualified FreePalace.Domain.Chat      as Chat
import qualified FreePalace.Domain.GUI       as GUI
import qualified FreePalace.Domain.Host      as Host
import qualified FreePalace.Domain.Media     as Media
import qualified FreePalace.Domain.Net       as Net
import qualified FreePalace.Domain.User      as User
import qualified FreePalace.Messages.Inbound as InboundMessages
import qualified Network.URI                 as Network

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
  , userMap          :: User.UserMap
  , chatLog          :: Chat.ChatLog
  , currentRoomState :: CurrentRoomState
  } deriving Show

data CurrentRoomState = CurrentRoomState {
    roomId                  :: Maybe Host.RoomId
  , roomName                :: Maybe Host.RoomName
  , roomBackgroundImageName :: Maybe Media.ImageFilename
  , inhabitants             :: [User.UserId]
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


-- We will accept as legal characters A-Za-z0-9. - and _; any others will be encoded as url-encoded characters %20 etc.
defaultSettings :: Settings
defaultSettings = Settings {
    thisUserName = "Haskell Curry"  -- TODO allow user to set user name
  }

initialConnectedState :: Disconnected -> ProtocolState -> Net.Hostname -> Net.PortId -> Connected
initialConnectedState priorState protocol host port =
  case protocol of
   PalaceProtocolState _ _ -> Connected {
      protocolState = protocol
    , guiState = disconnectedGui priorState
    , hostState = initialHostStateFor host port
    , hostDirectory = Host.HostDirectory
    , userState = NotLoggedIn { username = thisUserName . disconnectedSettings $ priorState }
    , settings = disconnectedSettings priorState
    }

initialHostStateFor :: Net.Hostname -> Net.PortId -> HostState
initialHostStateFor hostName portid = HostState {
    hostname = hostName
  , portId = portid
  , mediaServer = Nothing
  , roomList = Host.RoomList
  , userMap = initialIdToUserIdMapping
  , chatLog = Chat.ChatLog []
  , currentRoomState = initialRoomState
  }

initialIdToUserIdMapping :: User.UserMap
initialIdToUserIdMapping = User.UserMap $ Map.fromList [ (0, User.roomAnnouncementUserId) ]

initialRoomState :: CurrentRoomState
initialRoomState = CurrentRoomState {
    roomId = Nothing
  , roomName = Nothing
  , roomBackgroundImageName = Nothing
  , inhabitants = []         
  }
  
disconnectedStateFrom :: Connected -> Disconnected
disconnectedStateFrom priorState =
    let gui = guiState priorState
        priorSettings = settings priorState
    in Disconnected gui Host.HostDirectory priorSettings

withUserRefId :: Connected -> User.UserRefId -> Connected
withUserRefId currentState refId =
  let currentUserName = case userState currentState of
        NotLoggedIn { username = name } -> name
        LoggedIn    { userId = theUserId } -> User.userName theUserId
  in currentState {
    userState = LoggedIn {
       userId = User.UserId {
            User.userRef = refId
          , User.userName = currentUserName
          }
       }
    }

withProtocol :: Connected -> ProtocolState -> Connected
withProtocol currentState updatedProtocol = currentState { protocolState = updatedProtocol }

withMediaServerInfo :: Connected -> InboundMessages.MediaServerInfo -> Connected
withMediaServerInfo currentState (InboundMessages.MediaServerInfo mediaServerUrl) =
  case Network.parseURI mediaServerUrl of
   Nothing -> currentState  -- If we can't parse it, we just won't update.
   Just uri ->
     currentState {
       hostState = (hostState currentState) {
          mediaServer = Just uri
       }
     }

withRoomDescription :: Connected -> InboundMessages.RoomDescription -> Connected
withRoomDescription currentState roomDescription =
  let oldRoomState = currentRoomState . hostState $ currentState
  in currentState {
    hostState = (hostState currentState) {
       currentRoomState = oldRoomState {
            roomId = Just $ InboundMessages.roomDescId roomDescription
          , roomName = Just $ InboundMessages.roomDescName roomDescription
          , roomBackgroundImageName = Just $ InboundMessages.roomDescBackground roomDescription
          }
       }
    }

withRoomUsers :: Connected -> InboundMessages.UserListing -> Connected
withRoomUsers currentState (InboundMessages.UserListing userData) =
  let roomUserList = map userFrom userData
      currentUserMap = userMap $ hostState currentState
      newUserMap = User.addUsers currentUserMap roomUserList
      oldRoomState = currentRoomState . hostState $ currentState
  in
   currentState {
     hostState = (hostState currentState) {
          userMap = newUserMap
        , currentRoomState = oldRoomState { inhabitants = roomUserList }
     }
   }

userFrom :: InboundMessages.UserData -> User.UserId
userFrom InboundMessages.UserData { InboundMessages.userId = ref, InboundMessages.userName = name } =
  User.UserId {
      User.userRef = ref
    , User.userName = name
  }


withMovementData :: Connected -> InboundMessages.MovementNotification -> (Chat.Movement, Connected)
withMovementData currentState movementData = (Chat.Movement, currentState)
