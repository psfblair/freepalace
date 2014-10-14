module FreePalace.Messages where

import System.IO
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Bits
import Data.Char
import Control.Applicative

data UserId = UserId { userRef :: Int, userName :: String } deriving Show -- TODO Limit to 31 characters

data Header = Header {
  messageType :: MessageType,
  messageSize :: Int,
  messageRefNumber :: Int
} deriving Show

data Communication = Communication {
  speaker :: UserId,
  target :: Maybe UserId,
  message :: String,
  chatMode :: ChatMode
} deriving Show

data ChatMode = TalkAloud | Whispering | Thought | Exclamation | Announcement | Outbound deriving Show

data Movement = Movement { x :: Int, y :: Int, userWhoMoved :: UserId } deriving Show

class Message msgType where
  messageTypeId :: msgType -> Int

data MessageType =  -- Bidirectional messages
                Say | Whisper |
                Move | 
                UserColor | UserFace | UserProp |
                SpotState | DoorLock | DoorUnlock |
                PropNew | PropMove | PropDelete |  -- loose props
                Blowthru |
                
                -- Incoming messages
                LittleEndianServer | BigEndianServer | UnknownServer | -- Not necessarily processor architecture-based
                AlternateLogonReply | Authenticate |                    
                ServerVersion | ServerInfo |
                UserStatus  | UserLoggedOnAndMax |
                GotHttpServerLocation |
                -- the following four are whenever you change rooms as well as login
                GotRoomDescription | GotRoomDescriptionAlt | GotUserList | GotRoomList |                        
                GotReplyOfAllRooms | GotReplyOfAllUsers |                     
                RoomDescend | -- ?
                Pinged |                   
                Talk | CrossRoomWhisper | -- Say and Whisper are encoded; Talk and CrossRoom aren't
                UserNew | UserDescription | UserRename | UserExitRoom | UserLeaving | -- UserLeaving ?
                ConnectionDied |
                IncomingFile | AssetIncoming | AssetQuery |
                SpotMove | PictMove |
                DrawCmd | 
                NavError |
                ServerDown |
                UnknownMessage |
                
                -- Outgoing messages
                Logon | Authresponse | PingBack | Bye |
                Superuser |
                RequestRoomList | GotoRoom |
                RequestUserList |                        
                ChangeName |                        
                GlobalMsg | RoomMsg | SusrMsg |
                RequestAsset |                 
                AssetRegi |                        
                Draw 
            deriving (Enum, Bounded, Show)

instance Message MessageType where  
    messageTypeId Say = 2020895851
    messageTypeId Whisper = 2021091699
    messageTypeId Move = 1967943523    
    messageTypeId UserColor = 1970500163
    messageTypeId UserFace = 1970500166
    messageTypeId UserProp = 1970500176
    messageTypeId SpotState = 1934849121
    messageTypeId DoorLock = 1819239275
    messageTypeId DoorUnlock = 1970170991
    messageTypeId PropNew = 1850765936
    messageTypeId PropMove = 1833988720
    messageTypeId PropDelete = 1682993776
    messageTypeId Blowthru = 1651273591
    
    messageTypeId LittleEndianServer = 1920559476 -- MSG_DIYIT highfirst = true
    messageTypeId BigEndianServer = 1953069426  -- MSG_TIYID   highfirst = false
    messageTypeId UnknownServer = 1886610802 -- MSG_TROPSER
    messageTypeId AlternateLogonReply = 1919250482
    messageTypeId Authenticate = 1635087464
    messageTypeId ServerVersion = 1986359923
    messageTypeId ServerInfo = 1936289382
    messageTypeId UserStatus = 1968403553
    messageTypeId UserLoggedOnAndMax = 1819240224
    messageTypeId GotHttpServerLocation = 1213486160
    messageTypeId GotRoomDescription = 1919905645
    messageTypeId GotRoomDescriptionAlt = 1934782317
    messageTypeId GotUserList = 1919971955
    messageTypeId GotRoomList = 1917612916
    messageTypeId GotReplyOfAllRooms = 1917612916
    messageTypeId GotReplyOfAllUsers = 1967944564    
    messageTypeId RoomDescend = 1701733490
    messageTypeId Pinged = 1885957735

    messageTypeId Talk = 1952541803
    messageTypeId CrossRoomWhisper = 0x77686973
    messageTypeId UserNew = 1852863091
    messageTypeId UserDescription = 1970500164
    messageTypeId UserRename = 1970500174
    messageTypeId UserExitRoom = 1701868147
    messageTypeId UserLeaving = 1652122912
    messageTypeId ConnectionDied = 1685026670
    messageTypeId IncomingFile = 1933994348
    messageTypeId AssetQuery = 1900114804
    messageTypeId AssetIncoming = 1933669236
    messageTypeId SpotMove = 1668238451
    messageTypeId PictMove = 1884057443
    messageTypeId DrawCmd = 1685217655
    messageTypeId NavError = 1933931122
    messageTypeId ServerDown = 1685026670
    messageTypeId Logon = 0X72656769
    messageTypeId Authresponse = 0X61757472
    messageTypeId PingBack = 0X706F6E67
    messageTypeId Bye = 0X62796520
    messageTypeId Superuser = 0X73757372
    messageTypeId RequestRoomList = 0X724C7374
    messageTypeId GotoRoom = 0X6E617652
    messageTypeId RequestUserList = 0X754C7374
    messageTypeId ChangeName = 0X7573724E

    messageTypeId GlobalMsg = 0X676D7367
    messageTypeId RoomMsg = 0X726D7367
    messageTypeId SusrMsg = 0X736D7367
    messageTypeId RequestAsset = 0X71417374
    messageTypeId AssetRegi = 0X72417374
    messageTypeId Draw = 0X64726177

    messageTypeId UnknownMessage = 0x00
                      
idToMessageType :: Int -> MessageType
idToMessageType messageTypeId =
  Map.findWithDefault UnknownMessage messageTypeId idsToMessageTypes 
    where messageTypes = [(minBound :: MessageType) ..]
          idsToMessageTypes = Map.fromList $ map messageTypeToIdTypePair messageTypes

messageTypeToIdTypePair :: MessageType -> (Int, MessageType)
messageTypeToIdTypePair msgType = (messageTypeId msgType, msgType)

userIdFrom :: Header -> Map.Map Int UserId -> UserId
userIdFrom header userMap =
  let refNumber = messageRefNumber header
      defaultUserId = UserId { userRef = refNumber , userName = "User #" ++ show refNumber }
  in Map.findWithDefault defaultUserId refNumber userMap 

-- Ultimately RoomDescription should also contain:
   -- overlay images - id, name, transpatency index
   -- hotspots - these come in layers - above avatars, above name tags, above all, above nothing.
           -- There is also a collection of all of them, and a hash of all of them by ID.
           -- Hotspots are read by count and initial offset. each record has a fixed length of 48 bytes.
           -- Hotspots also manage a collection of vertices and a collection of hotspot states
   -- loose props
   -- draw commands
data RoomDescription = RoomDescription {
    roomId :: Int
  , roomName :: String
  , roomBackgroundImageName :: String
} deriving Show

