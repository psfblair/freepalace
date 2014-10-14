module FreePalace.Messages.Outbound where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Bits
import Data.Word
import Control.Applicative

import qualified FreePalace.Messages as Messages
import qualified FreePalace.Messages.Obfuscate as Obfuscate
import FreePalace.Net.Types as Net
import FreePalace.Net.Utils

loginMessage :: Net.Translators -> Messages.UserId -> LazyByteString.ByteString
loginMessage translators userId =
  let intsToBuilder = Net.intsToByteStringBuilder translators
      shortsToBuilder = Net.shortsToByteStringBuilder translators
      stringBuilder = Net.toWin1252ByteStringBuilder translators
      byteBuilder = Net.toSingleByteBuilder translators
      
      msgTypeId = Messages.messageTypeId Messages.Logon
      messageLength = 128
      referenceNumber = 0
      guestRegCodeCrc = 0x5905f923      -- TODO get this from Registration code if user is not a guest
      guestRegCodeCounter = 0xcf07309c  -- TODO get this from Registration code if user is not a guest
      userName = Messages.userName userId
      userNameLength = min 31 $ length userName -- TODO move this constraint to the UserId itself
      -- TODO Wizard password goes in characters starting at 32
      paddedUserName = stringBuilder $ ensureLength 63 '\0' userName --TODO check that the last character can be non-null
      auxFlags = flagAuthenticate .|. flagPlatformTypeWin32
      puidCounter = 0xf5dc385e
      puidCrc = 0xc144c580
      demoElapsed = 0  -- no longer used
      totalElapsed = 0 -- no longer used
      demoLimit = 0    -- no longer used
      desiredRoomId = 0 :: Word16 --  Later maybe get the initial desired room from somewhere
     
      reserved = stringBuilder "OPNPAL" -- The protocol spec lists these as reserved, and says nothing should be put in them.
                 -- However, the server records these 6 bytes in its log file.  So we'll exploit that to identify the client type.
                 -- We have to pretend to be Open Palace because newer servers use this to identify the client
     
      uploadRequestedProtocolVersion = 0 -- ignored on server
      uploadCapabilities = uploadCapabilitiesAssetsPalace -- TODO This is a lie... for now (?)
     
      -- We have to lie about our capabilities so that servers don't reject this as a hacked client.
      downloadCapabilities = downloadCapabilitiesAssetsPalace .|. downloadCapabilitiesFilesPalace .|. downloadCapabilitiesFilesHttp
     
      upload2DEngineCapabilities = 0 -- Unused
      upload2dGraphicsCapabilities = 0 -- Unused
      upload3DEngineCapabilities = 0  -- Unused
      
      builder =       (intsToBuilder     (msgTypeId : messageLength : referenceNumber : guestRegCodeCrc : guestRegCodeCounter : []))
                  .++ (byteBuilder        userNameLength)
                  .++ paddedUserName
                  .++ (intsToBuilder     (auxFlags : puidCounter : puidCrc : demoElapsed : totalElapsed : demoLimit : []))
                  .++ (shortsToBuilder   (desiredRoomId : [])) 
                  .++ reserved
                  .++ (intsToBuilder     (uploadRequestedProtocolVersion : uploadCapabilities : downloadCapabilities : upload2DEngineCapabilities :
                                          upload2dGraphicsCapabilities : upload3DEngineCapabilities : []))

  in Builder.toLazyByteString builder

chatMessage :: Net.Translators -> Messages.Communication -> LazyByteString.ByteString
chatMessage translators communication =
  do
    let intsToBuilder = Net.intsToByteStringBuilder translators
        shortsToBuilder = Net.shortsToByteStringBuilder translators
        
        encoded = Obfuscate.obfuscate $ Messages.message communication
        messageLength = fromIntegral $ LazyByteString.length encoded
        userRefId = Messages.userRef $ Messages.speaker communication
        header = case Messages.target communication of
          Just target -> whisperHeader messageLength userRefId target
          Nothing -> talkHeader messageLength userRefId
          
        payloadLength = fromIntegral messageLength + 3 -- terminator plus two bytes for the length
        terminatedMessagePayload = LazyByteString.append encoded (LazyByteString.cons 0 LazyByteString.empty)
        leader = Builder.toLazyByteString $ (intsToBuilder header) .++ (shortsToBuilder [payloadLength])
      in LazyByteString.append leader terminatedMessagePayload
    
talkHeader :: Int -> Int-> [Int]
talkHeader messageLength userRefId  =
  let totalLength = messageLength + 3
      messageType = Messages.messageTypeId Messages.Say
  in messageType : totalLength : userRefId : []

-- This is actually a header plus a field for the target
whisperHeader :: Int -> Int -> Messages.UserId -> [Int]
whisperHeader messageLength userRefId target =
  let totalLength = messageLength + 7
      messageType = Messages.messageTypeId Messages.Whisper
      targetRefId = Messages.userRef target
  in messageType : totalLength : userRefId : targetRefId : []

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
