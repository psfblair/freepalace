module FreePalace.GUI.Types where

-- TODO Need Log Window
-- TODO Need Disconnect menu item and Quit menu item

import qualified FreePalace.Messages as Messages

data MainWindow = MainWindow {
  quit :: IO (),
  showMainWindow :: IO (),
  closeMainWindow :: IO (),
  onMainWindowClose :: IO () -> IO ()
}

data LogWindow = LogWindow {
  showLogWindow :: IO (),
  closeLogWindow :: IO (),
  appendMessage :: Messages.Communication -> IO ()
}

data Dialog = Dialog {
  showDialog :: IO (),
  closeDialog :: IO ()
}

data Button = Button {
  onButtonClick :: IO () -> IO ()
}

data TextField = TextField {
  textValue :: IO String
}
                 
data Components = Components {
  mainWindow :: MainWindow,
  
  connectDialog :: Dialog,
  connectHostEntry :: TextField,
  connectPortEntry :: TextField,
  connectOk :: Button,
  connectCancel :: Button,

  logWindow :: LogWindow
}
