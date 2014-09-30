module FreePalace.Net.Messages where

import System.IO
import Data.Char
import qualified Data.Map as Map
import Control.Applicative

import FreePalace.Net as Net

readMessageType :: Net.IncomingByteSource -> IO IncomingMessage
readMessageType byteSource = idToMessageType <$> readWord byteSource

readHeader :: Net.IncomingByteSource -> IO Header
readHeader byteSource = 
  do
    msgType <- readMessageType byteSource
    size <- readWord byteSource
    referenceNumber <- readWord byteSource
    return Header {
      messageType = msgType,
      messageSize = size,
      messageRefNumber = referenceNumber
    }
    
readMessageBody :: Net.IncomingByteSource -> Header -> (Header -> [Int] -> IncomingMessage) -> IO IncomingMessage
readMessageBody byteSource header messageConstructor =
  do
    let length = messageSize header
    messageBytes <- readWords length byteSource
    return (messageConstructor header messageBytes)
  
data Header = Header {
  messageType :: IncomingMessage,
  messageSize :: Int,
  messageRefNumber :: Int
}

data IncomingMessage =  LittleEndianServer | BigEndianServer | UnknownServer | -- 68000 & early SPARC big-endian, PPC & later SPARC bi-endian.
                        AlternateLogonReply | Authenticate |                    
                        ServerVersion | ServerInfo |
                        UserStatus  | UserLoggedOnAndMax |
                        GotHttpServerLocation |
                        -- the following four are whenever you change rooms as well as login
                        GotRoomDescription | GotRoomDescriptionAlt | GotUserList | GotRoomList |                        
                        GotReplyOfAllRooms | GotReplyOfAllUsers |                     
                        RoomDescend | -- ?
                        Pinged |                   
                        XTalk | XWhisper | Talk  | Whisper | -- Xtalk and Xwhisper are encrypted; Talk and Whisper unencrypted
                        Movement |
                        UserNew | UserColor | UserFace | UserDescription | UserProp | UserRename | UserExitRoom | UserLeaving | -- UserLeaving ?
                        ConnectionDied |
                        IncomingFile | AssetIncoming | AssetQuery |
                        DoorLock | DoorUnlock | SpotState | SpotMove | PictMove |
                        DrawCmd | 
                        PropMove | PropDelete | PropNew | -- loose props
                        NavError |
                        ServerDown |
                        Blowthru |
                        UnknownMessage

idToMessageType :: Int -> IncomingMessage
idToMessageType messageTypeId =
  Map.findWithDefault UnknownMessage messageTypeId idsToMessages 
    where idsToMessages = Map.fromList
                            [(1920559476,LittleEndianServer)
                            ,(1953069426,BigEndianServer)
                            ,(1886610802,UnknownServer)
                            ,(1919250482,AlternateLogonReply)
                            ,(0x61757468,Authenticate)
                            ,(1986359923,ServerVersion)
                            ,(1936289382,ServerInfo)
                            ,(1968403553,UserStatus)
                            ,(1819240224,UserLoggedOnAndMax)
                            ,(1213486160,GotHttpServerLocation)
                            ,(1919905645,GotRoomDescription)
                            ,(1934782317,GotRoomDescriptionAlt)
                            ,(1919971955,GotUserList)
                            ,(1917612916,GotRoomList)
                            ,(1701733490,RoomDescend)
                            ,(1852863091,UserNew)
                            ,(1885957735,Pinged)
                            ,(0x78746c6b,XTalk)
                            ,(0x78776973,XWhisper)
                            ,(0x74616c6b,Talk)
                            ,(0x77686973,Whisper)
                            ,(1967943523,Movement)
                            ,(1970500163,UserColor)
                            ,(1970500166,UserFace)
                            ,(1970500164,UserDescription)
                            ,(1970500176,UserProp)
                            ,(1970500174,UserRename)
                            ,(1652122912,UserLeaving)
                            ,(1685026670,ConnectionDied)
                            ,(1933994348,IncomingFile)
                            ,(1933669236,AssetIncoming)
                            ,(1701868147,UserExitRoom)
                            ,(1917612916,GotReplyOfAllRooms)
                            ,(1967944564,GotReplyOfAllUsers)
                            ,(1819239275,DoorLock)
                            ,(1970170991,DoorUnlock)
                            ,(1934849121,SpotState)
                            ,(1668238451,SpotMove)
                            ,(1884057443,PictMove)
                            ,(1685217655,DrawCmd)
                            ,(1833988720,PropMove)
                            ,(1682993776,PropDelete)
                            ,(1850765936,PropNew)
                            ,(1900114804,AssetQuery)
                            ,(1933931122,NavError)
                            ,(1685026670,ServerDown)
                            ,(1651273591,Blowthru)]
