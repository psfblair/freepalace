module FreePalace.Net where

import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom) -- I wish this module didn't have to know about sockets.
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Binary.Get as Get
import Data.Word
import Data.Int
import Control.Exception

import qualified FreePalace.Net.Types as Net
import qualified FreePalace.Messages as Messages
import qualified FreePalace.Net.Send as Send
import qualified FreePalace.Net.Receive as Receive
import qualified FreePalace.State as State

connect :: State.Disconnected -> State.Protocol -> Net.Hostname -> Net.PortId -> IO State.Connected
connect priorState State.PalaceProtocol hostname portId =
  do
    addrinfos <- Socket.getAddrInfo Nothing (Just hostname) (Just portId)
    let serveraddr = head addrinfos
    socket <- Socket.socket (Socket.addrFamily serveraddr) Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption socket Socket.KeepAlive 1
    Socket.connect socket (Socket.addrAddress serveraddr)
    let byteSource = Net.SocketByteSource socket
        byteSink = Net.SocketByteSink socket
    return $ justConnectedToPalaceState priorState byteSource byteSink hostname portId

disconnect :: State.ClientState -> IO State.Disconnected
disconnect (State.DisconnectedState state@(State.Disconnected _)) = return state
disconnect (State.ConnectedState State.Connected {
    State.guiState = gui
  , State.protocolState = State.PalaceProtocolState connection converters
  }) =
  do
    let (Net.SocketByteSource socket) = State.palaceByteSource connection
        disconnectAttempt = Socket.close socket
    catch disconnectAttempt (\(SomeException e) -> return ())  -- If we can't disconnect, we're going to be throwing away that connection state anyway.
    return $ State.Disconnected gui

justConnectedToPalaceState :: State.Disconnected -> Net.IncomingByteSource -> Net.OutgoingByteSink -> Net.Hostname -> Net.PortId -> State.Connected
justConnectedToPalaceState priorState byteSource byteSink hostname portId =
  let gui = State.disconnectedGui priorState
      connection  = State.PalaceConnection {
          State.palaceByteSource = byteSource
        , State.palaceByteSink = byteSink
        }
  in State.Connected {
      State.protocolState = State.PalaceProtocolState connection defaultPalaceMessageConverters
    , State.guiState = gui
    , State.hostState = State.initialHostStateFor hostname portId
    , State.hostDirectoryState = State.EmptyHostDirectory
    , State.userState = State.NotLoggedIn
    }

defaultPalaceMessageConverters :: State.PalaceMessageConverters
defaultPalaceMessageConverters = bigEndianMessageConverters

bigEndianMessageConverters :: State.PalaceMessageConverters
bigEndianMessageConverters = State.PalaceMessageConverters {
    State.palaceShortWriter = Builder.word16BE
  , State.palaceIntWriter = Builder.int32BE
  , State.palaceShortReader = Get.getWord16be
  , State.palaceIntReader = Get.getWord32be
  }

littleEndianMessageConverters :: State.PalaceMessageConverters
littleEndianMessageConverters = State.PalaceMessageConverters {
    State.palaceShortWriter = Builder.word16LE
  , State.palaceIntWriter = Builder.int32LE
  , State.palaceShortReader = Get.getWord16le
  , State.palaceIntReader = Get.getWord32le
  }
