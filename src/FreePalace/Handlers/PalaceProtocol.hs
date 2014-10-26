module FreePalace.Handlers.PalaceProtocol where

import           Control.Exception
import Control.Applicative
import qualified FreePalace.Messages                         as Messages
import qualified FreePalace.Messages.PalaceProtocol.Inbound  as PalaceInbound
import qualified FreePalace.Messages.PalaceProtocol.Outbound as PalaceOutbound
import qualified FreePalace.Net.PalaceProtocol.Connect       as Connect
import qualified FreePalace.Net.Send                         as Send
import qualified FreePalace.Net                              as Net
import qualified FreePalace.State                            as State
import qualified System.Log.Logger                           as Log
import qualified FreePalace.Domain as Domain
import qualified FreePalace.Handlers.State as Handlers
import qualified FreePalace.Handlers.Types as HandlerTypes

handleConnectRequested :: State.ClientState -> Net.Hostname -> Net.PortId -> IO State.ClientState
handleConnectRequested clientState host port =
  do
    disconnected <- disconnect clientState
    catch
      (State.ConnectedState <$> connect disconnected host port)
      (\(SomeException exception) ->
        do
          Log.errorM "Connection" $ show exception
          return $ State.DisconnectedState disconnected)

disconnect :: State.ClientState -> IO State.Disconnected
disconnect (State.DisconnectedState disconnected) = return disconnected
disconnect (State.ConnectedState priorState@(State.Connected { State.protocolState = State.PalaceProtocolState connection _ })) =
  do
    Connect.disconnect connection -- This does not rethrow exceptions
    return $ State.Disconnected gui Domain.HostDirectory settings
    where gui = State.guiState priorState
          settings = State.settings priorState

connect :: State.Disconnected -> Net.Hostname -> Net.PortId -> IO State.Connected
connect priorState host port = 
  do
    connection <- Connect.connect host port
    return State.Connected {
      State.protocolState = State.PalaceProtocolState connection Connect.defaultPalaceMessageConverters
    , State.guiState = State.disconnectedGui priorState
    , State.hostState = Handlers.initialHostStateFor host port
    , State.hostDirectory = Domain.HostDirectory
    , State.userState = State.NotLoggedIn { State.username = State.thisUserName . State.disconnectedSettings $ priorState }
    , State.settings = State.disconnectedSettings priorState
    }
    
handleHandshake :: State.Connected -> Net.PalaceConnection -> Net.PalaceMessageConverters -> IO State.Connected
handleHandshake clientState connection messageConverters =
  do
    header <- readHeader connection messageConverters -- TODO Time out if this takes too long
    let msgType    = Messages.messageType header
        userRefId  = Messages.messageRefNumber header
        currentUserState = State.userState clientState
        newState   = clientState { State.userState = State.LoggedIn { State.userId = Handlers.userIdFor currentUserState userRefId }}
    Log.debugM "Incoming.Handshake" (show msgType)
    case msgType of
      Messages.BigEndianServer    -> return  $ newState
      Messages.LittleEndianServer -> return  $ newState { State.protocolState = State.PalaceProtocolState connection Connect.littleEndianMessageConverters }
      Messages.UnknownServer      -> throwIO $ userError "Unknown server type"
      _                           -> throwIO $ userError "Invalid server type"

sendLogin :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Domain.UserId -> IO ()
sendLogin Net.PalaceConnection { Net.palaceByteSink = byteSink } messageConverters userId =
  do
    let intsToBuilder     = Send.toIntByteStringBuilder   $ Net.palaceIntWriter   messageConverters
        shortsToBuilder   = Send.toShortByteStringBuilder $ Net.palaceShortWriter messageConverters
        loginMessageBytes = PalaceOutbound.loginMessage intsToBuilder shortsToBuilder userId
    Log.debugM "Outgoing.Login" "Sending login..."
    Send.writeBytesToSink byteSink loginMessageBytes

-- TODO The handler has to see if 1) it's a client command, 2) it's a script call, 3) a user is selected (for whisper)
speak :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Domain.UserId -> String -> Maybe Domain.UserId -> IO ()
speak Net.PalaceConnection { Net.palaceByteSink = byteSink } messageConverters userId messageText selectedUser =
  do
    let intsToBuilder     = Send.toIntByteStringBuilder   $ Net.palaceIntWriter   messageConverters
        shortsToBuilder   = Send.toShortByteStringBuilder $ Net.palaceShortWriter messageConverters

        -- TODO check the initial character of the message for instructions
        communication = Domain.Communication {
          Domain.speaker = userId,
          Domain.target = selectedUser,
          Domain.message = messageText,
          Domain.chatMode = Domain.Outbound
          }

        chatMessageBytes = PalaceOutbound.chatMessage intsToBuilder shortsToBuilder communication
    Log.debugM "Outgoing.Talk" (show communication)
    Send.writeBytesToSink byteSink chatMessageBytes

readHeader :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO Messages.Header
readHeader connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
    PalaceInbound.readHeader byteSource intReader

handleAlternateLogonReply :: Net.PalaceConnection -> Net.PalaceMessageConverters -> IO (HandlerTypes.PuidCounter, HandlerTypes.PuidCrc)
handleAlternateLogonReply connection messageConverters =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        shortReader = Net.palaceShortReader messageConverters
    PalaceInbound.readAlternateLogonReply byteSource intReader shortReader

handleServerInfo :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO (HandlerTypes.PlaceName, HandlerTypes.ServerPermissions)
handleServerInfo connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
    PalaceInbound.readServerInfo byteSource intReader header

handleUserStatus :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.UserFlags
handleUserStatus connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
    PalaceInbound.readUserStatus byteSource shortReader header

handleUserLogonNotification :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO (HandlerTypes.UserRefId, HandlerTypes.PalaceUserCount)
handleUserLogonNotification connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
    PalaceInbound.readUserLogonNotification byteSource intReader header

handleMediaServerInfo :: Net.PalaceConnection -> Messages.Header -> IO Net.URL
handleMediaServerInfo connection header =
  do
    let byteSource = Net.palaceByteSource connection
    PalaceInbound.readMediaServerInfo byteSource header

 -- room name, background image, overlay images, props, hotspots, draw commands
handleRoomDescription :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.RoomDescription
handleRoomDescription connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        intReader = Net.palaceIntReader messageConverters
        shortReader = Net.palaceShortReader messageConverters
    PalaceInbound.readRoomDescription byteSource intReader shortReader header
  {- OpenPalace also does this when receiving these messages:
	clearStatusMessage currentRoom
	clearAlarms
	midiStop
     and after parsing the information:
        Dim room 100
        Room.showAvatars = true -- scripting can hide all avatars
        Dispatch room change event for scripting
    -}

handleUserList :: Net.PalaceConnection -> Messages.Header -> IO ()
handleUserList connection header =
  do
    let byteSource = Net.palaceByteSource connection
    PalaceInbound.readUserList byteSource header
    {- OpenPalace does:
         currentRoom.removeAllUsers();
    -}

handleNewUserNotification :: Net.PalaceConnection -> Messages.Header -> IO ()
handleNewUserNotification connection header =
  do
    let byteSource = Net.palaceByteSource connection
    PalaceInbound.readNewUserNotification byteSource header
    {- OpenPalace does:
         PalaceSoundPlayer.getInstance().playConnectionPing();
         And if one's self entered:
         if (needToRunSignonHandlers) {  palaceController.triggerHotspotEvents(IptEventHandler.TYPE_SIGNON);
					 needToRunSignonHandlers = false; }
         palaceController.triggerHotspotEvents(IptEventHandler.TYPE_ENTER);
    -}

handleTalk :: Net.PalaceConnection -> Messages.Header -> IO HandlerTypes.ChatData
handleTalk connection header =
  do
    let byteSource = Net.palaceByteSource connection
    PalaceInbound.readTalk byteSource header

handleEncodedTalk :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.ChatData
handleEncodedTalk connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
    PalaceInbound.readEncodedTalk byteSource shortReader header

handleMovement :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Messages.Header -> IO HandlerTypes.MovementData
handleMovement connection messageConverters header =
  do
    let byteSource = Net.palaceByteSource connection
        shortReader = Net.palaceShortReader messageConverters
    PalaceInbound.readMovement byteSource shortReader header

handleUnknownMessage :: Net.PalaceConnection -> Messages.Header -> IO ()
handleUnknownMessage connection header =
  do
    let byteSource = Net.palaceByteSource connection
    PalaceInbound.readUnknown byteSource header
