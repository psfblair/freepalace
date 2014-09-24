module FreePalace.Handlers where

data Handlers = Handlers

setUpHandlers :: Handlers
setUpHandlers = Handlers

class FreePalaceDialog a where
  handleCancel :: a -> IO ()
