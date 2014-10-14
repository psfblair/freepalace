module FreePalace.Net.Send where

import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString 
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Word
import Data.Int
import Data.Encoding
import Data.Encoding.CP1252
import Data.Monoid
import Control.Applicative

import qualified FreePalace.Net.Types as Net 
import FreePalace.Net.Utils

writeBytesToSink :: Net.OutgoingByteSink -> LazyByteString.ByteString -> IO ()
writeBytesToSink (Net.SocketByteSink socket) byteString =
  do
    NetworkLazyByteString.send socket byteString
    return ()

toIntByteStringBuilder :: (Int32 -> Builder.Builder) -> [Int] -> Builder.Builder
toIntByteStringBuilder builderBuilder ints = foldr (.++) mempty builders
  where builders = builderBuilder <$> fromIntegral <$> ints

toShortByteStringBuilder :: (Word16 -> Builder.Builder) -> [Word16] -> Builder.Builder
toShortByteStringBuilder builderBuilder shorts = foldr (.++) mempty builders
  where builders = builderBuilder <$> shorts

toWin1252ByteStringBuilder :: String -> Builder.Builder
toWin1252ByteStringBuilder stringToEncode = Builder.lazyByteString $ encodeLazyByteString CP1252 stringToEncode

toSingleByteBuilder :: Int -> Builder.Builder
toSingleByteBuilder theInt = Builder.int8 $ (fromIntegral theInt :: Int8)

