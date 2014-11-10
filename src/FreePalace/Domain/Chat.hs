module FreePalace.Domain.Chat where

import qualified FreePalace.Domain.User as User

data Communication = Communication {
  speaker  :: User.UserId,
  target   :: Maybe User.UserId,
  message  :: ChatText,
  chatMode :: ChatMode
} deriving Show

data ChatMode = TalkAloud | Whispering | Thought | Exclamation | Notice | Announcement | Outbound deriving Show

type ChatText = String

data Movement = Movement

data ChatLog  = ChatLog {
  -- TODO logEntryTimestamp
  logEntries :: [ Communication ]
  } deriving Show

makeRoomAnnouncement :: String -> Communication
makeRoomAnnouncement announcement =
  Communication {
    speaker = User.roomAnnouncementUserId
  , target = Nothing
  , message = announcement
  , chatMode = Announcement
  }
