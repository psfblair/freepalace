module FreePalace.Handlers.PalaceProtocol where

import           Control.Applicative
import           Control.Exception
import qualified FreePalace.Domain                           as Domain
import qualified FreePalace.Handlers.State                   as Handlers
import qualified FreePalace.Messages                         as Messages
import qualified FreePalace.Messages.PalaceProtocol.InboundReader  as PalaceInbound
import qualified FreePalace.Messages.PalaceProtocol.Outbound as PalaceOutbound
import qualified FreePalace.Net                              as Net
import qualified FreePalace.Net.PalaceProtocol.Connect       as Connect
import qualified FreePalace.Net.Send                         as Send
import qualified FreePalace.State                            as State
import qualified System.Log.Logger                           as Log

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

