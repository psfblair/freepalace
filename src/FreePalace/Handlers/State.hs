module FreePalace.Handlers.State where

import           FreePalace.Domain         as Domain
import           FreePalace.Handlers.Types as HandlerTypes
import           FreePalace.Net            as Net
import           FreePalace.Net.PalaceProtocol.Connect    as Connect
import           FreePalace.State
import           Network.URI               as Network

-- We will accept as legal characters A-Za-z0-9. - and _; any others will be encoded as url-encoded characters %20 etc.
defaultSettings :: Settings
defaultSettings = Settings {
    thisUserName = "Haskell Curry"  -- TODO allow user to set user name
  }

withUserRefId :: Connected -> Domain.UserRefId -> Connected
withUserRefId currentState refId =
  let currentUserName = case userState currentState of
        NotLoggedIn { username = name } -> name
        LoggedIn    { userId = theUserId } -> Domain.userName theUserId
  in currentState {
    userState = LoggedIn {
       userId = Domain.UserId {
            Domain.userRef = refId
          , Domain.userName = currentUserName
          }
       }                  
    }

withProtocol :: Connected -> HandlerTypes.ProtocolInfo -> Connected
withProtocol currentState protocol =
    case protocol of
      HandlerTypes.PalaceProtocol connection endianness ->
        case endianness of 
          HandlerTypes.BigEndian    -> currentState { protocolState = PalaceProtocolState connection Connect.bigEndianMessageConverters }
          HandlerTypes.LittleEndian -> currentState { protocolState = PalaceProtocolState connection Connect.littleEndianMessageConverters }
  
initialHostStateFor :: Net.Hostname -> Net.PortId -> HostState
initialHostStateFor hostName portid = HostState {
    hostname = hostName
  , portId = portid
  , mediaServer = Nothing
  , roomList = Domain.RoomList
  , userList = Domain.UserList
  , chatLog = Domain.ChatLog []
  , currentRoomState = Nothing
  }

withMediaServerInfo :: Connected -> Net.URL -> Connected
withMediaServerInfo currentState mediaServerUrl =
  case Network.parseURI mediaServerUrl of
   Nothing -> currentState  -- If we can't parse it, we just won't update.
   Just uri ->
     currentState {
       hostState = (hostState currentState) {
          mediaServer = Just uri
       }
     }

-- TODO This may get more complicated if/when the current room state is affected by more than a RoomDescription message
withRoomDescription :: Connected -> HandlerTypes.RoomDescription -> Connected
withRoomDescription currentState roomDescription =
  currentState {
    hostState = (hostState currentState) {
       currentRoomState = Just CurrentRoomState {
            roomId = HandlerTypes.roomDescId roomDescription
          , roomName = HandlerTypes.roomDescName roomDescription
          , roomBackgroundImageName = HandlerTypes.roomDescBackground roomDescription
          }
       }
    }

withMovementData :: Connected -> HandlerTypes.MovementData -> (Domain.Movement, Connected)
withMovementData currentState movementData = (Domain.Movement, currentState)
