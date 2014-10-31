module FreePalace.Domain.Chat where

import qualified FreePalace.Domain.User as User
import qualified FreePalace.Messages.Inbound as Messages

data Communication = Communication {
  speaker  :: User.UserId,
  target   :: Maybe User.UserId,
  message  :: String,
  chatMode :: ChatMode
} deriving Show

data ChatMode = TalkAloud | Whispering | Thought | Exclamation | Notice | Announcement | Outbound deriving Show

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
