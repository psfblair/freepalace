module FreePalace.Domain.Chat where

import qualified FreePalace.Domain.User as User
import qualified FreePalace.Messages.Inbound as InboundEvents

data Communication = Communication {
  speaker  :: User.UserId,
  target   :: Maybe User.UserId,
  message  :: String,
  chatMode :: ChatMode
} deriving Show

data ChatMode = TalkAloud | Whispering | Thought | Exclamation | Announcement | Outbound deriving Show

data Movement = Movement

data ChatLog  = ChatLog {
  -- TODO logEntryTimestamp
  logEntries :: [ Communication ]
  } deriving Show


fromChatData :: InboundEvents.InboundChat -> ChatMode -> Communication
fromChatData chatData mode =
  Communication {
    speaker = User.userIdFor User.refIdToUserIdMapping $ InboundEvents.chatSpeaker chatData,
    target = fmap (User.userIdFor User.refIdToUserIdMapping) (InboundEvents.chatRecipient chatData),
    message = InboundEvents.chatMessage chatData,
    chatMode = mode
  }

makeRoomAnnouncement :: String -> Communication
makeRoomAnnouncement announcement =
  Communication {
    speaker = User.roomAnnouncementUserId
  , target = Nothing
  , message = announcement
  , chatMode = Announcement
  }
