module FreePalace.Net.Types where

import qualified Network.Socket as Socket (Socket) -- I wish this module didn't have to know about sockets.
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Builder as Builder
import Data.Word

import qualified FreePalace.Messages as Messages

type Hostname = String
type PortId   = String

-- TODO - These may be better as typeclasses and get rid of dependence of this module on sockets
data IncomingByteSource = SocketByteSource Socket.Socket
data OutgoingByteSink   = SocketByteSink   Socket.Socket

data Channels = Channels { source :: IncomingByteSource, sink :: OutgoingByteSink }

data Connectors = Connectors {
  connect :: Hostname -> PortId -> IO Channels,
  disconnect :: IncomingByteSource -> IO ()
}

data Communicators = Communicators {
  readInt :: IO Int,
  readInts :: Int -> IO [Int],
  readByte :: IO Word8,
  readShort :: IO Word16,
  readHeader :: IO Messages.Header,
  readText :: Int -> IO String,
  readTextNoTerminator :: Int -> IO String,
  writeBytes :: LazyByteString.ByteString -> IO ()
}

data Translators = Translators {
  intsToByteStringBuilder :: [Int] -> Builder.Builder,
  shortsToByteStringBuilder :: [Word16] -> Builder.Builder
}

