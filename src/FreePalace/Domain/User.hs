module FreePalace.Domain.User where

import Data.Map as Map

type UserRefId = Int
type UserName = String

data UserId = UserId { userRef :: UserRefId, userName :: UserName } deriving Show -- TODO Limit to 31 characters

data UserList = UserList deriving Show


userIdFor :: Map.Map UserRefId UserId -> UserRefId -> UserId
userIdFor userMap refId =
  let defaultUserId = UserId { userRef = refId, userName = "User #" ++ show refId }
  in Map.findWithDefault defaultUserId refId userMap

-- TODO This needs to live in the global state
refIdToUserIdMapping :: Map.Map UserRefId UserId
refIdToUserIdMapping = Map.fromList [ (0, roomAnnouncementUserId) ]

-- TODO Move to State? Or into ref map?
roomAnnouncementUserId :: UserId
roomAnnouncementUserId = UserId { userRef = 0, userName = "Announcement" }
