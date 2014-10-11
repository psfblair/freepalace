module FreePalace.Messages.Outbound where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString

import Data.Encoding
import Data.Encoding.CP1252
import Data.Bits
import Data.Int
import Data.Word
import Data.Monoid

import Control.Applicative

import qualified FreePalace.Messages as Messages
import FreePalace.Net.Types as Net
import FreePalace.Net.Utils

bigEndianTranslators :: Net.Translators
bigEndianTranslators = Net.Translators {
  intsToByteStringBuilder = toIntByteStringBuilder Builder.int32BE,
  shortsToByteStringBuilder = toShortByteStringBuilder Builder.word16BE
}

littleEndianTranslators :: Net.Translators
littleEndianTranslators = Net.Translators {
  intsToByteStringBuilder = toIntByteStringBuilder Builder.int32LE,
  shortsToByteStringBuilder = toShortByteStringBuilder Builder.word16LE
}

loginMessage :: Net.Translators -> Messages.UserId -> LazyByteString.ByteString
loginMessage translators userId =
  let intsToBuilder = intsToByteStringBuilder translators
      shortsToBuilder = shortsToByteStringBuilder translators
      
      msgTypeId = Messages.messageTypeId Messages.Logon
      messageLength = 128
      referenceNumber = 0
      guestRegCodeCrc = 0x5905f923      -- TODO get this from Registration code if user is not a guest
      guestRegCodeCounter = 0xcf07309c  -- TODO get this from Registration code if user is not a guest
      userName = Messages.userName userId
      userNameLength = min 63 $ length userName 
      paddedUserName = toWin1252ByteStringBuilder $ ensureLength 63 '\0' userName --TODO check that the last character can be non-null
      auxFlags = flagAuthenticate .|. flagPlatformTypeWin32
      puidCounter = 0xf5dc385e
      puidCRC = 0xc144c580
      demoElapsed = 0  -- no longer used
      totalElapsed = 0 -- no longer used
      demoLimit = 0    -- no longer used
      desiredRoomId = 0 :: Word16 --  Later maybe get the initial desired room from somewhere
     
      reserved = toWin1252ByteStringBuilder "FREEPL" -- The protocol spec lists these as reserved, and says nothing should be put in them.
                 -- However, the server records these 6 bytes in its log file.  So we'll exploit that to identify the client type.
     
      uploadRequestedProtocolVersion = 0 -- ignored on server
      uploadCapabilities = uploadCapabilitiesAssetsPalace -- TODO This is a lie... for now (?)
     
      -- We have to lie about our capabilities so that servers don't reject this as a hacked client.
      downloadCapabilities = downloadCapabilitiesAssetsPalace .|. downloadCapabilitiesFilesPalace .|. downloadCapabilitiesFilesHttp
     
      upload2DEngineCapabilities = 0 -- Unused
      upload2dGraphicsCapabilities = 0 -- Unused
      upload3DEngineCapabilities = 0  -- Unused
      
      builder =     (intsToBuilder       (msgTypeId : messageLength : referenceNumber : guestRegCodeCrc : guestRegCodeCounter : []))
                  .++ (toSingleByteBuilder userNameLength)
                  .++ paddedUserName
                  .++ (intsToBuilder       (auxFlags : puidCounter : puidCRC : demoElapsed : totalElapsed : demoLimit : []))
                  .++ (shortsToBuilder     (desiredRoomId : [])) 
                  .++ reserved
                  .++ (intsToBuilder       (uploadRequestedProtocolVersion : uploadCapabilities : downloadCapabilities : upload2DEngineCapabilities :
                                            upload2dGraphicsCapabilities : upload3DEngineCapabilities : []))

  in Builder.toLazyByteString builder


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

ensureLength :: Int -> a -> [a] -> [a]
ensureLength limit padElement xs 
  | limit == (length xs)  = xs
  | limit < (length xs)   = take limit xs
  | otherwise             = pad limit padElement xs

pad :: Int -> a -> [a] -> [a]
pad limit padElement xs = xs ++ padding
  where padding = replicate n padElement
        n = limit - (length xs)


{-     ----------- CONSTANTS ------------     -}

-- Machine Types
flagPlatformTypeUnknown = 0
flagPlatformTypeMac68k  = 1
flagPlatformTypeMacPPC  = 2
flagPlatformTypeWin16   = 3
flagPlatformTypeWin32   = 4
flagPlatformTypeJava    = 5

flagAuthenticate        = 0x80000000

uploadCapabilitiesAssetsPalace    = 0X00000001
uploadCapabilitiesAssetsFtp       = 0X00000002
uploadCapabilitiesAssetsHttp      = 0X00000004
uploadCapabilitiesAssetsOther     = 0X00000008

uploadCapabilitiesFilesPalace     = 0X00000010
uploadCapabilitiesFilesFtp        = 0X00000020
uploadCapabilitiesFilesHttp       = 0X00000040
uploadCapabilitiesFilesOther      = 0X00000080

uploadCapabilitiesExtendPkt       = 0X00000100

downloadCapabilitiesAssetsPalace  = 0X00000001
downloadCapabilitiesAssetsFtp     = 0X00000002
downloadCapabilitiesAssetsHttp    = 0X00000004
downloadCapabilitiesAssetsOther   = 0X00000008

downloadCapabilitiesFilesPalace   = 0X00000010
downloadCapabilitiesFilesFtp      = 0X00000020
downloadCapabilitiesFilesHttp     = 0X00000040
downloadCapabilitiesFilesOther    = 0X00000080
downloadCapabilitiesFilesHttpsrvr = 0X00000100

downloadCapabilitiesExtendPkt     = 0X00000200
