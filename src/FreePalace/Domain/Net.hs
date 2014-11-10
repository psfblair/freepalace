module FreePalace.Domain.Net where

import qualified Data.Binary.Get              as Get
import qualified Data.ByteString.Lazy.Builder as Builder
import           Data.Int
import           Data.Word
import qualified Network.Socket               as Socket (Socket)

type Hostname = String
type PortId   = String
type URL      = String

data IncomingByteSource = SocketByteSource Socket.Socket
data OutgoingByteSink   = SocketByteSink   Socket.Socket

data Protocol = PalaceProtocol deriving Show

data PalaceConnection = PalaceConnection {
    palaceByteSource :: IncomingByteSource
  , palaceByteSink   :: OutgoingByteSink
  }

data PalaceMessageConverters = PalaceMessageConverters {
    palaceShortWriter :: Word16 -> Builder.Builder
  , palaceIntWriter   :: Int32 -> Builder.Builder
  , palaceShortReader :: Get.Get Word16
  , palaceIntReader   :: Get.Get Word32
  }
