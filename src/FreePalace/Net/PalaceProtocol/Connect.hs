module FreePalace.Net.PalaceProtocol.Connect where

import           Control.Exception
import qualified Data.Binary.Get              as Get
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified FreePalace.Domain.Net        as Net
import qualified Network.Socket               as Socket hiding (recv, recvFrom,
                                                         send, sendTo)

connect :: Net.Hostname -> Net.PortId -> IO Net.PalaceConnection
connect hostname portId =
  do
    addrinfos <- Socket.getAddrInfo Nothing (Just hostname) (Just portId)
    let serveraddr = head addrinfos
    socket <- Socket.socket (Socket.addrFamily serveraddr) Socket.Stream Socket.defaultProtocol
    Socket.setSocketOption socket Socket.KeepAlive 1
    Socket.connect socket (Socket.addrAddress serveraddr)
    let byteSource = Net.SocketByteSource socket
        byteSink = Net.SocketByteSink socket
        connection  = Net.PalaceConnection {
          Net.palaceByteSource = byteSource
        , Net.palaceByteSink = byteSink
        }
    return connection

disconnect :: Net.PalaceConnection -> IO ()
disconnect connection =
  do
    let (Net.SocketByteSource socket) = Net.palaceByteSource connection
        disconnectAttempt = Socket.close socket
    catch disconnectAttempt (\(SomeException _) -> return ())  -- If we can't disconnect, we're going to be throwing away that connection state anyway.

defaultPalaceMessageConverters :: Net.PalaceMessageConverters
defaultPalaceMessageConverters = bigEndianMessageConverters

bigEndianMessageConverters :: Net.PalaceMessageConverters
bigEndianMessageConverters = Net.PalaceMessageConverters {
    Net.palaceShortWriter = Builder.word16BE
  , Net.palaceIntWriter = Builder.int32BE
  , Net.palaceShortReader = Get.getWord16be
  , Net.palaceIntReader = Get.getWord32be
  }

littleEndianMessageConverters :: Net.PalaceMessageConverters
littleEndianMessageConverters = Net.PalaceMessageConverters {
    Net.palaceShortWriter = Builder.word16LE
  , Net.palaceIntWriter = Builder.int32LE
  , Net.palaceShortReader = Get.getWord16le
  , Net.palaceIntReader = Get.getWord32le
  }
