module FreePalace.Domain where

type UserRefId = Int
type UserName = String

data UserId = UserId { userRef :: UserRefId, userName :: UserName } deriving Show -- TODO Limit to 31 characters

type RoomId = Int
type RoomName = String
type ImageFilename = String

data Communication = Communication {
  speaker  :: UserId,
  target   :: Maybe UserId,
  message  :: String,
  chatMode :: ChatMode
} deriving Show

data ChatMode = TalkAloud | Whispering | Thought | Exclamation | Announcement | Outbound deriving Show

data Movement = Movement

data HostDirectory = HostDirectory deriving Show
data RoomList = RoomList deriving Show
data UserList = UserList deriving Show
data ChatLog  = ChatLog {
  -- TODO logEntryTimestamp
  logEntries :: [ Communication ]
  } deriving Show
