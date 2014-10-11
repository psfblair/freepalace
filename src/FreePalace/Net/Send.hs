module FreePalace.Net.Send where

import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString 
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString

import qualified FreePalace.Net.Types as Net 

writeBytesToSink :: Net.OutgoingByteSink -> LazyByteString.ByteString -> IO ()
writeBytesToSink (Net.SocketByteSink socket) byteString =
  do
    NetworkLazyByteString.send socket byteString
    return ()


