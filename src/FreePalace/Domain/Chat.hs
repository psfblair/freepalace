module FreePalace.Domain.Chat where

import qualified FreePalace.Domain.User as User
import qualified FreePalace.Messages.Inbound as Messages

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


fromChatData :: Messages.Chat -> Communication
fromChatData Messages.Chat {   Messages.chatSpeaker = spkr
                             , Messages.chatRecipient = recvr
                             , Messages.chatMessage = msg
                             , Messages.chatExposure = exposure } =
  let mode = case exposure of
        Messages.PublicChat -> TalkAloud
        Messages.PrivateChat -> Whispering
  in Communication {
      speaker = User.userIdFor User.refIdToUserIdMapping spkr
    , target = fmap (User.userIdFor User.refIdToUserIdMapping) recvr
    , message = msg
    , chatMode = mode
    }

makeRoomAnnouncement :: String -> Communication
makeRoomAnnouncement announcement =
  Communication {
    speaker = User.roomAnnouncementUserId
  , target = Nothing
  , message = announcement
  , chatMode = Announcement
  }
