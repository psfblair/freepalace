{-# LANGUAGE ExistentialQuantification #-}
module FreePalace.Net (
  Hostname, PortId,
  Channels, source, sink,
  Connectors, connector, disconnector, socketConnectors,
  Communicators, readInt, readInts, intsToByteStringBuilder, shortsToByteStringBuilder, writeBytes,
  bigEndianCommunicators, littleEndianCommunicators) where

import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom) -- I wish this module didn't have to know about sockets.
import qualified Network.Socket.ByteString as NetworkByteString
import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString 
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Builder as Builder
import Data.Encoding
import Data.Encoding.CP1252
import Data.Word
import Data.Int

import Control.Applicative hiding (empty)
import Control.Exception
import Data.Monoid

import FreePalace.Net.Utils

type Hostname = String
type PortId   = String

data Channels = Channels { source :: IncomingByteSource, sink :: OutgoingByteSink }
data IncomingByteSource = SocketByteSource Socket.Socket
data OutgoingByteSink   = SocketByteSink   Socket.Socket


data Connectors = Connectors {
  connector :: Hostname -> PortId -> IO Channels,
  disconnector :: IncomingByteSource -> IO ()
}

socketConnectors = Connectors {
  connector = \hostname portNum -> 
                    do
                      addrinfos <- Socket.getAddrInfo Nothing (Just hostname) (Just portNum)
                      let serveraddr = head addrinfos
                      socket <- Socket.socket (Socket.addrFamily serveraddr) Socket.Stream Socket.defaultProtocol
                      Socket.setSocketOption socket Socket.KeepAlive 1
                      Socket.connect socket (Socket.addrAddress serveraddr)
                      return $ Channels { source = SocketByteSource socket, sink = SocketByteSink socket },

  disconnector = \(SocketByteSource socket) -> Socket.close socket
}


data Communicators = Communicators {
  readInt :: IO Int,
  readInts :: Int -> IO [Int],
  intsToByteStringBuilder :: [Int] -> Builder.Builder,
  shortsToByteStringBuilder :: [Word16] -> Builder.Builder,
  writeBytes :: LazyByteString.ByteString -> IO ()
}

-- We treat big endian as the "natural" order; the Linpal and OpenPalace clients treat it as the default.
bigEndianCommunicators byteSource byteSink = Communicators {
  readInt = readIntFromNetwork id byteSource,
  readInts = readIntsFromNetwork id byteSource,
  intsToByteStringBuilder = (toIntByteStringBuilder Builder.int32BE),
  shortsToByteStringBuilder = (toShortByteStringBuilder Builder.word16BE),
  writeBytes = writeBytesToSink byteSink
}

littleEndianCommunicators byteSource byteSink = Communicators {
  readInt = readIntFromNetwork LazyByteString.reverse byteSource,
  readInts = readIntsFromNetwork LazyByteString.reverse byteSource,
  intsToByteStringBuilder = toIntByteStringBuilder Builder.int32LE,
  shortsToByteStringBuilder = toShortByteStringBuilder Builder.word16LE,
  writeBytes = writeBytesToSink byteSink
}


-- TODO Deal with IOErrors somewhere up the stack
readIntsFromNetwork :: (LazyByteString.ByteString -> LazyByteString.ByteString) -> IncomingByteSource -> Int -> IO [Int]
readIntsFromNetwork endianConverter byteSource numberOfWords =
  sequence $ take numberOfWords $ repeat (readIntFromNetwork endianConverter byteSource)

toIntByteStringBuilder :: (Int32 -> Builder.Builder) -> [Int] -> Builder.Builder
toIntByteStringBuilder builderBuilder ints = foldr (.++) mempty builders
  where builders = builderBuilder <$> fromIntegral <$> ints

toShortByteStringBuilder :: (Word16 -> Builder.Builder) -> [Word16] -> Builder.Builder
toShortByteStringBuilder builderBuilder shorts = foldr (.++) mempty builders
  where builders = builderBuilder <$> shorts


{- IMPLEMENTATION-SPECIFIC -}
readIntFromNetwork :: (LazyByteString.ByteString -> LazyByteString.ByteString) -> IncomingByteSource -> IO Int
readIntFromNetwork endianConverter (SocketByteSource socket) =
  fromIntegral <$> Get.runGet Get.getWord32be <$> endianConverter <$> NetworkLazyByteString.recv socket 4

writeBytesToSink :: OutgoingByteSink -> LazyByteString.ByteString -> IO ()
writeBytesToSink (SocketByteSink socket) byteString =
  do
    NetworkLazyByteString.send socket byteString
    return ()

