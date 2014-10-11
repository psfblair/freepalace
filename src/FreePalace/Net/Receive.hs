module FreePalace.Net.Receive where

import qualified System.Log.Logger as Log
import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString 
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary.Get as Get
import qualified Data.Convertible.Base as Convert
import Data.Convertible.Instances.Num
import Data.Word

import Control.Applicative

import qualified FreePalace.Net.Types as Net
import qualified FreePalace.Messages as Messages

readHeader :: IO Int -> IO Messages.Header
readHeader readNextInt = 
  do
    msgType <- Messages.idToMessageType <$> readNextInt
    size <- readNextInt
    referenceNumber <- readNextInt
    return Messages.Header {
      Messages.messageType = msgType,
      Messages.messageSize = size,
      Messages.messageRefNumber = referenceNumber
    }

-- Assume 2-byte Win-1252 characters. OpenPalace seems to let user select UTF-8 as well; not sure how that works.
readTextFromNetwork :: (LazyByteString.ByteString -> LazyByteString.ByteString) -> Net.IncomingByteSource -> Int -> IO String
readTextFromNetwork endianConverter byteSource numberOfCharacters =
  do
    chars <- sequence $ take (numberOfCharacters - 1) $ repeat (readCharFromNetwork byteSource)
    readByteFromNetwork byteSource  -- throw away last byte (null terminator)
    return chars
    
-- TODO Deal with IOErrors somewhere up the stack
readIntsFromNetwork :: (LazyByteString.ByteString -> LazyByteString.ByteString) -> Net.IncomingByteSource -> Int -> IO [Int]
readIntsFromNetwork endianConverter byteSource numberOfWords =
  sequence $ take numberOfWords $ repeat (readIntFromNetwork endianConverter byteSource)

{- IMPLEMENTATION-SPECIFIC -}
-- TODO FIX MAKE THIS USE getWord32le or be depending on endianness, and pass that in
readIntFromNetwork :: (LazyByteString.ByteString -> LazyByteString.ByteString) -> Net.IncomingByteSource -> IO Int
readIntFromNetwork endianConverter (Net.SocketByteSource socket) =
  fromIntegral <$> Get.runGet Get.getWord32be <$> endianConverter <$> NetworkLazyByteString.recv socket 4

readCharFromNetwork :: Net.IncomingByteSource -> IO Char
readCharFromNetwork (Net.SocketByteSource socket) =
  Convert.convert <$> LazyByteString.head <$> readByteFromNetwork (Net.SocketByteSource socket)
  
readByteFromNetwork :: Net.IncomingByteSource -> IO LazyByteString.ByteString
readByteFromNetwork (Net.SocketByteSource socket) = NetworkLazyByteString.recv socket 1
