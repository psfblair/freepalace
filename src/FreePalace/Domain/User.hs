module FreePalace.Domain.User where

import Data.Map as Map

type UserRefId = Int
type UserName = String

{- Fields that OpenPalace has on a user:
				user.isSelf
				user.id
				user.name
				user.propCount
				user.x
				user.y
				user.propIds
				user.propCrcs
				user.face
				user.color
-}
data UserId = UserId { userRef :: UserRefId, userName :: UserName } deriving Show -- TODO Limit to 31 characters

newtype UserMap = UserMap (Map.Map UserRefId UserId) deriving Show

addUsers :: UserMap -> [UserId] -> UserMap
addUsers (UserMap userMap) additionalUsers =
  let addToUserMap = \accum user -> Map.insert (userRef user) user accum
      newMap = Prelude.foldl addToUserMap userMap additionalUsers
  in UserMap newMap

-- TODO Move to State? Or into ref map?
roomAnnouncementUserId :: UserId
roomAnnouncementUserId = UserId { userRef = 0, userName = "Announcement" }
