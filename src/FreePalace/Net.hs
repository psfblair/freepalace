{-# LANGUAGE ExistentialQuantification #-}
module FreePalace.Net where

import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom) -- I wish this module didn't have to know about sockets.
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Builder as Builder

import Data.Word
import Data.Int

import qualified FreePalace.Net.Types as Net
import qualified FreePalace.Messages as Messages
import qualified FreePalace.Net.Send as Send
import qualified FreePalace.Net.Receive as Receive

socketConnectors = Net.Connectors {
    Net.connect = \hostname portNum -> 
                 do
                   addrinfos <- Socket.getAddrInfo Nothing (Just hostname) (Just portNum)
                   let serveraddr = head addrinfos
                   socket <- Socket.socket (Socket.addrFamily serveraddr) Socket.Stream Socket.defaultProtocol
                   Socket.setSocketOption socket Socket.KeepAlive 1
                   Socket.connect socket (Socket.addrAddress serveraddr)
                   return $ Net.Channels { Net.source = Net.SocketByteSource socket, Net.sink = Net.SocketByteSink socket }

  , Net.disconnect = \(Net.SocketByteSource socket) -> Socket.close socket
}

-- We treat big endian as the "natural" order; the Linpal and OpenPalace clients treat it as the default.
bigEndianCommunicators :: Net.IncomingByteSource -> Net.OutgoingByteSink -> Net.Communicators
bigEndianCommunicators byteSource byteSink = Net.Communicators {
    Net.readInt = Receive.readIntFromNetwork id byteSource
  , Net.readInts = Receive.readIntsFromNetwork id byteSource
  , Net.readByte = Receive.readByteFromNetwork byteSource
  , Net.readBytes = Receive.readBytesFromNetwork byteSource
  , Net.readShort = Receive.readShortFromNetwork id byteSource
  , Net.readText = Receive.readNullTerminatedTextFromNetwork id byteSource
  , Net.readTextNoTerminator = Receive.readTextFromNetwork id byteSource
  , Net.readHeader = Receive.readHeader $ Receive.readIntFromNetwork id byteSource
  , Net.writeBytes = Send.writeBytesToSink byteSink
}

littleEndianCommunicators :: Net.IncomingByteSource -> Net.OutgoingByteSink -> Net.Communicators
littleEndianCommunicators byteSource byteSink = Net.Communicators {
    Net.readInt = Receive.readIntFromNetwork LazyByteString.reverse byteSource
  , Net.readInts = Receive.readIntsFromNetwork LazyByteString.reverse byteSource
  , Net.readByte = Receive.readByteFromNetwork byteSource
  , Net.readBytes = Receive.readBytesFromNetwork byteSource
  , Net.readShort = Receive.readShortFromNetwork LazyByteString.reverse byteSource
  , Net.readText = Receive.readNullTerminatedTextFromNetwork LazyByteString.reverse byteSource
  , Net.readTextNoTerminator = Receive.readTextFromNetwork id byteSource
  , Net.readHeader = Receive.readHeader $ Receive.readIntFromNetwork LazyByteString.reverse byteSource
  , Net.writeBytes = Send.writeBytesToSink byteSink
}

bigEndianTranslators :: Net.Translators
bigEndianTranslators = Net.Translators {
    Net.intsToByteStringBuilder = Send.toIntByteStringBuilder Builder.int32BE
  , Net.shortsToByteStringBuilder = Send.toShortByteStringBuilder Builder.word16BE
  , Net.toWin1252ByteStringBuilder = Send.toWin1252ByteStringBuilder
  , Net.toSingleByteBuilder = Send.toSingleByteBuilder                         
}

littleEndianTranslators :: Net.Translators
littleEndianTranslators = Net.Translators {
    Net.intsToByteStringBuilder = Send.toIntByteStringBuilder Builder.int32LE
  , Net.shortsToByteStringBuilder = Send.toShortByteStringBuilder Builder.word16LE
  , Net.toWin1252ByteStringBuilder = Send.toWin1252ByteStringBuilder
  , Net.toSingleByteBuilder = Send.toSingleByteBuilder
}

