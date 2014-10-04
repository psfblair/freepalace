{-# LANGUAGE ExistentialQuantification #-}
module FreePalace.Net where

import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom) -- I wish this module didn't have to know about sockets.
import qualified Network.Socket.ByteString as NetworkByteString
import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString 
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary.Get as Get
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

readWord :: IncomingByteSource -> IO Int
readWord (SocketByteSource socket) = fromIntegral <$> Get.runGet Get.getWord32le <$> NetworkLazyByteString.recv socket 4

readWords :: Int -> IncomingByteSource -> IO [Int]
readWords numberOfWords (SocketByteSource socket) =
  Get.runGet readInts <$> NetworkLazyByteString.recv socket ((fromIntegral numberOfWords) * 4)
  where
    readInts = do
      empty <- Get.isEmpty
      if empty
         then return []
         else do thisWord <- Get.getWord32le
                 rest <- readInts
                 return ((fromIntegral thisWord):rest)
    

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
