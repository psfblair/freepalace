module FreePalace.Net.Send where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as ByteString
import Data.Encoding
import Data.Encoding.CP1252
import Data.Bits
import Data.Word
import Data.Monoid
import Control.Applicative

import qualified FreePalace.Net as Net
import FreePalace.Net.Messages

-- Machine Types
flagPlatformTypeUnknown = 0
flagPlatformTypeMac68k = 1
flagPlatformTypeMacPPC = 2
flagPlatformTypeWin16 = 3
flagPlatformTypeWin32 = 4
flagPlatformTypeJava  = 5

flagAuthenticate = 0x80000000

uploadCapabilitiesAssetsPalace = 0X00000001
uploadCapabilitiesAssetsFtp = 0X00000002
uploadCapabilitiesAssetsHttp = 0X00000004
uploadCapabilitiesAssetsOther = 0X00000008

uploadCapabilitiesFilesPalace = 0X00000010
uploadCapabilitiesFilesFtp = 0X00000020
uploadCapabilitiesFilesHttp = 0X00000040
uploadCapabilitiesFilesOther = 0X00000080

uploadCapabilitiesExtendPkt = 0X00000100

downloadCapabilitiesAssetsPalace = 0X00000001
downloadCapabilitiesAssetsFtp = 0X00000002
downloadCapabilitiesAssetsHttp = 0X00000004
downloadCapabilitiesAssetsOther = 0X00000008

downloadCapabilitiesFilesPalace = 0X00000010
downloadCapabilitiesFilesFtp = 0X00000020
downloadCapabilitiesFilesHttp =  0X00000040
downloadCapabilitiesFilesOther = 0X00000080
downloadCapabilitiesFilesHttpsrvr = 0X00000100

downloadCapabilitiesExtendPkt = 0X00000200


sendLogin :: Net.OutgoingByteSink -> UserId -> IO ()
sendLogin byteSink userId = 
  do
    let msgTypeId = messageTypeId Logon
    let messageLength = 128
    let referenceNumber = 0
    let guestRegCodeCrc = 0x5905f923      -- TODO get this from Registration code if user is not a guest
    let guestRegCodeCounter = 0xcf07309c  -- TODO get this from Registration code if user is not a guest
    let userNameLength = length $ userName userId
    let paddedUserName = toWin1252ByteString $ ensureLength 31 '\0' (userName userId)
    let auxFlags = flagAuthenticate .|. flagPlatformTypeWin32
    let puidCounter = 0xf5dc385e
    let puidCRC = 0xc144c580
    let demoElapsed = 0  -- no longer used
    let totalElapsed = 0 -- no longer used
    let demoLimit = 0    -- no longer used
    let desiredRoomId = 0 :: Word16 --  Later maybe get the initial desired room from somewhere

    let reserved = toWin1252ByteString "FREEPL" -- The protocol spec lists these as reserved, and says nothing should be put in them.
                   -- However, the server records these 6 bytes in its log file.  So we'll exploit that to identify the client type.

    let uploadRequestedProtocolVersion = 0 -- ignored on server
    let uploadCapabilities = uploadCapabilitiesAssetsPalace -- TODO This is a lie... for now (?)

    -- We have to lie about our capabilities so that servers don't reject this as a hacked client.
    let downloadCapabilities = downloadCapabilitiesAssetsPalace .|. downloadCapabilitiesFilesPalace .|. downloadCapabilitiesFilesHttp

    let upload2DEngineCapabilities = 0 -- Unused
    let upload2dGraphicsCapabilities = 0 -- Unused
    let upload3DEngineCapabilities = 0  -- Unused
    -- TODO - Accommodate big endian servers
    let builder =     (toByteStringBuilder   (msgTypeId : messageLength : referenceNumber : guestRegCodeCrc : guestRegCodeCounter : userNameLength : []))
                  .++ (Builder.byteString     paddedUserName)
                  .++ (toByteStringBuilder   (auxFlags : puidCounter : puidCRC : demoElapsed : totalElapsed : demoLimit : []))
                  .++ (Builder.word16LE       desiredRoomId) 
                  .++ (Builder.byteString     reserved)
                  .++ (toByteStringBuilder   (uploadRequestedProtocolVersion : uploadCapabilities : downloadCapabilities : upload2DEngineCapabilities :
                                            upload2dGraphicsCapabilities : upload3DEngineCapabilities : []))

    let message = Builder.toLazyByteString builder
    Net.writeBytes byteSink message


toWin1252ByteString :: String -> ByteString.ByteString
toWin1252ByteString = encodeStrictByteString CP1252

ensureLength :: Int -> a -> [a] -> [a]
ensureLength limit padElement xs 
  | limit == (length xs)  = xs
  | limit < (length xs)   = take limit xs
  | otherwise             = pad limit padElement xs

pad :: Int -> a -> [a] -> [a]
pad limit padElement xs = xs ++ padding
  where padding = replicate n padElement
        n = limit - (length xs)

infixr 4 .++
(.++) :: Monoid m => m -> m -> m
(.++) = mappend

-- (.++) :: Builder.Builder -> Builder.Builder -> Builder.Builder
-- (.++) = Builder.append

toByteStringBuilder :: [Int] -> Builder.Builder
toByteStringBuilder ints = foldr (.++) mempty builders
  where builders = Builder.int32LE <$> fromIntegral <$> ints

