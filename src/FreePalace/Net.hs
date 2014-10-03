{-# LANGUAGE ExistentialQuantification #-}
module FreePalace.Net where

import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom) -- I wish this module didn't have to know about sockets.
import qualified Network.Socket.ByteString as NetworkByteString
import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as Char8 --TODO replace readInt from this package with Data.Binary.Get
import System.IO (Handle)
import Data.Word
import Data.Char
import Control.Applicative hiding (empty)
import Control.Exception

type Hostname = String
type PortId   = String

data Channels = Channels { source :: IncomingByteSource, sink :: OutgoingByteSink }
data IncomingByteSource = SocketByteSource Socket.Socket
data OutgoingByteSink   = SocketByteSink   Socket.Socket

-- TODO Move these functions into the Socket module and make fields for them in Connectors (change the name of that data type)

-- TODO This probably should use Data.Binary.Get
readWord :: IncomingByteSource -> IO Int
readWord (SocketByteSource socket) =
  do
    possibleInt <- Char8.readInt <$> NetworkByteString.recv socket 1
    case possibleInt of
     Just (theInt, byteString) -> return theInt
     Nothing -> throwIO (userError "Unable to read integer from incoming bytestring.")

-- TODO This probably should use Data.Binary.Get
readWords :: Integral b => b -> IncomingByteSource -> IO [Int]
readWords numberOfWords (SocketByteSource socket) =
  readInts <$> NetworkByteString.recv socket (fromIntegral numberOfWords)
  where
    emptyByteString = LazyByteString.empty
    -- FIXME warning about overlapping pattern match
    readInts emptyByteString = []
    readInts byteStr = case Char8.readInt byteStr of
                        Just (theInt, byteString) -> theInt : (readInts byteString)
                        Nothing -> []

-- TODO Deal with IOError somewhere up the stack
writeBytes :: OutgoingByteSink -> LazyByteString.ByteString -> IO ()
writeBytes (SocketByteSink socket) byteString =
  do
    NetworkLazyByteString.send socket byteString
    return ()

data Connectors = Connectors {
  connector :: Hostname -> PortId -> IO Channels,
  disconnector :: IncomingByteSource -> IO ()
}
