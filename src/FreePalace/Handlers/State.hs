module FreePalace.Handlers.State where

import           FreePalace.Domain         as Domain
import           FreePalace.Messages.Inbound as Events
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

withProtocol :: Connected -> Events.ProtocolInfo -> Connected
withProtocol currentState protocol =
    case protocol of
      Events.PalaceProtocol connection endianness ->
        case endianness of 
          Events.BigEndian    -> currentState { protocolState = PalaceProtocolState connection Connect.bigEndianMessageConverters }
          Events.LittleEndian -> currentState { protocolState = PalaceProtocolState connection Connect.littleEndianMessageConverters }
  
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

withMovementData :: Connected -> Events.InboundMovement -> (Domain.Movement, Connected)
withMovementData currentState movementData = (Domain.Movement, currentState)
