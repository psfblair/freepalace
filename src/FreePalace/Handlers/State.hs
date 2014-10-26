module FreePalace.Handlers.State where

import FreePalace.State
import FreePalace.Domain as Domain
import FreePalace.Net as Net
import FreePalace.Handlers.Types as Handlers
import Network.URI as Network

-- We will accept as legal characters A-Za-z0-9. - and _; any others will be encoded as url-encoded characters %20 etc.
defaultSettings :: Settings
defaultSettings = Settings {
    thisUserName = "Haskell Curry"  -- TODO allow user to set user name
  }

userIdFor :: UserState -> Int -> Domain.UserId
userIdFor stateOfUser refId = Domain.UserId {
    Domain.userRef = refId
  , Domain.userName = case stateOfUser of
                       NotLoggedIn { username = name } -> name
                       LoggedIn    { userId = Domain.UserId { Domain.userName = name }} -> name
  }

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
withRoomDescription :: Connected -> Handlers.RoomDescription -> Connected
withRoomDescription currentState roomDescription =
  currentState {
    hostState = (hostState currentState) {
       currentRoomState = Just CurrentRoomState {
            roomId = Handlers.roomDescId roomDescription
          , roomName = Handlers.roomDescName roomDescription
          , roomBackgroundImageName = Handlers.roomDescBackground roomDescription
          }
       }
    }

withMovementData :: Connected -> Handlers.MovementData -> (Domain.Movement, Connected)
withMovementData currentState movementData = (Domain.Movement, currentState)
