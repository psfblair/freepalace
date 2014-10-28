module FreePalace.Handlers.State where

import           FreePalace.Domain.Chat                as Chat
import           FreePalace.Domain.Host                as Host
import           FreePalace.Domain.Net                 as Net
import           FreePalace.Domain.State
import           FreePalace.Domain.User                as User
import           FreePalace.Messages.Inbound           as Events
import           FreePalace.Net.PalaceProtocol.Connect as Connect
import           Network.URI                           as Network

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
  , userList = User.UserList
  , chatLog = Chat.ChatLog []
  , currentRoomState = Nothing
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

withProtocol :: Connected -> Events.ProtocolInfo -> Connected
withProtocol currentState protocol =
    case protocol of
      Events.PalaceProtocol connection endianness ->
        case endianness of
          Events.BigEndian    -> currentState { protocolState = PalaceProtocolState connection Connect.bigEndianMessageConverters }
          Events.LittleEndian -> currentState { protocolState = PalaceProtocolState connection Connect.littleEndianMessageConverters }

withMediaServerInfo :: Connected -> Events.InboundMediaServerInfo -> Connected
withMediaServerInfo currentState (Events.InboundMediaServerInfo mediaServerUrl) =
  case Network.parseURI mediaServerUrl of
   Nothing -> currentState  -- If we can't parse it, we just won't update.
   Just uri ->
     currentState {
       hostState = (hostState currentState) {
          mediaServer = Just uri
       }
     }

-- TODO This may get more complicated if/when the current room state is affected by more than a RoomDescription message
withRoomDescription :: Connected -> Events.InboundRoomDescription -> Connected
withRoomDescription currentState roomDescription =
  currentState {
    hostState = (hostState currentState) {
       currentRoomState = Just CurrentRoomState {
            roomId = Events.roomDescId roomDescription
          , roomName = Events.roomDescName roomDescription
          , roomBackgroundImageName = Events.roomDescBackground roomDescription
          }
       }
    }

withMovementData :: Connected -> Events.InboundMovement -> (Chat.Movement, Connected)
withMovementData currentState movementData = (Chat.Movement, currentState)
