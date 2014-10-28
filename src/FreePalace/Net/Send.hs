module FreePalace.Net.Send where

import           Control.Applicative
import qualified Data.ByteString.Lazy           as LazyByteString
import qualified Data.ByteString.Lazy.Builder   as Builder
import           Data.Encoding
import           Data.Encoding.CP1252
import           Data.Int
import           Data.Monoid
import           Data.Word
import qualified Network.Socket.ByteString.Lazy as NetworkLazyByteString

import qualified FreePalace.Domain.Net          as Net
import           FreePalace.Net.Utils

writeBytesToSink :: Net.OutgoingByteSink -> LazyByteString.ByteString -> IO ()
writeBytesToSink (Net.SocketByteSink socket) byteString =
  do
    _ <- NetworkLazyByteString.send socket byteString
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
