{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module FreePalace.Messages.PalaceProtocol.Obfuscate where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Convertible.Base as Convert
import Data.Convertible.Instances.Num
import Data.Encoding
import Data.Encoding.CP1252
import Data.List as List
import Data.Word
import Data.Bits

infixr  5 :<
pattern b :< bs <- (LazyByteString.uncons -> Just (b, bs))
pattern Empty   <- (LazyByteString.uncons -> Nothing)

obfuscate :: String -> LazyByteString.ByteString
obfuscate plaintext =
  -- TODO Someday may need other encodings
  let encodedWithCharset = encodeLazyByteString CP1252 plaintext
      reversed = LazyByteString.reverse encodedWithCharset
      initialPartiallyObfuscatedByte = 0
  in obfuscateRecursive obfuscationKeys initialPartiallyObfuscatedByte LazyByteString.empty reversed

obfuscateRecursive :: [Word8] -> Word8 -> LazyByteString.ByteString -> LazyByteString.ByteString -> LazyByteString.ByteString
obfuscateRecursive _ _ accumulated Empty = accumulated
obfuscateRecursive (key1:key2:remainingKeys) previousPartiallyObfuscatedByte obfuscatedSoFar (byteToObfuscate:<remainingBytes) =
  let obfuscatedByte = byteToObfuscate `xor` key1 `xor` previousPartiallyObfuscatedByte
      partiallyObfuscatedByte = obfuscatedByte `xor` key2
      newResults = LazyByteString.cons obfuscatedByte obfuscatedSoFar
  in obfuscateRecursive remainingKeys partiallyObfuscatedByte newResults remainingBytes

illuminate :: [Word8] -> String
illuminate obfuscated =
  let reversed = reverse obfuscated
      initialPartiallyIlluminatedByte = 0
      illuminated = illuminateRecursive obfuscationKeys initialPartiallyIlluminatedByte [] reversed
  in map Convert.convert illuminated

-- We use an accumulator, which gives the result the reverse order of what was passed in
-- so we don't have to reverse it again.
illuminateRecursive :: [Word8] -> Word8 -> [Word8] -> [Word8] -> [Word8]
illuminateRecursive _ _ accumulated [] = accumulated
illuminateRecursive (key1:key2:remainingKeys) previousPartiallyIlluminatedByte illuminatedSoFar (byteToIlluminate:remainingBytes)  =
  let illuminatedByte = byteToIlluminate `xor` key1 `xor` previousPartiallyIlluminatedByte
      partiallyIlluminatedByte = byteToIlluminate `xor` key2
      newResults = illuminatedByte : illuminatedSoFar
  in illuminateRecursive remainingKeys partiallyIlluminatedByte newResults remainingBytes 
illuminateRecursive _ _ accumulated _ = accumulated  -- Should never get here but if we do we'll provide what we have so far

-- An array of 512 bizarrely-concocted Word16s truncated to Word8s
obfuscationKeys :: [Word8]
obfuscationKeys = take 512 $ List.unfoldr nextKey 666666

nextKey :: Int -> Maybe (Word8, Int)
nextKey seed =
  let quotient = seed `quot` 127773
      remainder = seed  `rem` 127773
      possibleNewSeed = (16807 * remainder) - (2836 * quotient)
      newSeed = if possibleNewSeed > 0 then possibleNewSeed else possibleNewSeed + 0x7fffffff
      pseudoRandomDouble = (fromIntegral newSeed) / 2147483647.0
      pseudoRandomInt  = (truncate :: Double -> Int) $ pseudoRandomDouble * 256
      pseudoRandomWord8 = fromIntegral $ pseudoRandomInt .&. 0x000000FF
  in Just (pseudoRandomWord8, newSeed)
