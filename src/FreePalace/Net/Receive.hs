module FreePalace.Net.Receive where

import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString 
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Binary.Get as Get
import qualified Data.Convertible.Base as Convert
import Data.Word

import Control.Applicative

import qualified FreePalace.Net as Net
import qualified FreePalace.Messages as Messages


readNullTerminatedTextFromNetwork :: Net.IncomingByteSource -> Int -> IO String
readNullTerminatedTextFromNetwork byteSource numberOfCharacters =
  do
    chars <- readTextFromNetwork byteSource numberOfCharacters
    return $ init chars   -- throw away last byte (null terminator)
  
-- Assume Win-1252 characters. OpenPalace seems to let user select UTF-8 as well; not sure how that works.
readTextFromNetwork ::  Net.IncomingByteSource -> Int -> IO String
readTextFromNetwork byteSource numberOfCharacters =
  sequence $ take numberOfCharacters $ repeat $ readCharFromNetwork byteSource
    
-- TODO Deal with IOErrors somewhere up the stack
readIntsFromNetwork :: Get.Get Word32 -> Net.IncomingByteSource -> Int -> IO [Int]
readIntsFromNetwork endianConverter byteSource numberOfWords =
  sequence $ take numberOfWords $ repeat $ readIntFromNetwork endianConverter byteSource

readBytesFromNetwork :: Net.IncomingByteSource -> Int -> IO [Word8]
readBytesFromNetwork byteSource numberOfBytes =
  sequence $ take numberOfBytes $ repeat $ readByteFromNetwork byteSource

{- IMPLEMENTATION-SPECIFIC -}
-- TODO FIX MAKE THIS USE getWord32le or be depending on endianness, and pass that in
readIntFromNetwork :: Get.Get Word32 -> Net.IncomingByteSource -> IO Int
readIntFromNetwork endianConverter (Net.SocketByteSource socket) =
  fromIntegral <$> Get.runGet endianConverter <$> NetworkLazyByteString.recv socket 4

readShortFromNetwork :: Get.Get Word16 -> Net.IncomingByteSource -> IO Word16
readShortFromNetwork endianConverter (Net.SocketByteSource socket) =
  Get.runGet endianConverter <$> NetworkLazyByteString.recv socket 2
  
readCharFromNetwork :: Net.IncomingByteSource -> IO Char
readCharFromNetwork (Net.SocketByteSource socket) =
  Convert.convert <$> readByteFromNetwork (Net.SocketByteSource socket)
  
readByteFromNetwork :: Net.IncomingByteSource -> IO Word8
readByteFromNetwork (Net.SocketByteSource socket) = LazyByteString.head <$> NetworkLazyByteString.recv socket 1
