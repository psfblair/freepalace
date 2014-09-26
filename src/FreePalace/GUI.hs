module FreePalace.GUI where

import qualified FreePalace.Handlers as Handlers

data MainWindow = MainWindow {
  quit :: IO (),
  showWindow :: IO (),
  closeWindow :: IO (),
  onWindowClose :: IO () -> IO ()
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
  connectCancel :: Button
}

setUpGUI :: Components -> Handlers.GUIEventHandlers -> IO ()
setUpGUI guiComponents handlers =
  do
    setUpMainWindow guiComponents
    setUpConnectDialog guiComponents handlers
    showConnectDialog guiComponents

setUpMainWindow :: Components -> IO ()
setUpMainWindow guiComponents =
  do
    let mainWin = mainWindow guiComponents
        quitAction = quit mainWin
    onWindowClose mainWin $ quitAction

setUpConnectDialog :: Components -> Handlers.GUIEventHandlers -> IO ()
setUpConnectDialog guiComponents handlers =
  do
    let okButton = connectOk guiComponents
        okHandler = do
          let hostSource = textValue $ connectHostEntry guiComponents
              portSource = textValue $ connectPortEntry guiComponents
          Handlers.connectOkHandler handlers hostSource portSource
          closeDialog $ connectDialog guiComponents
    onButtonClick okButton $ okHandler
    
    let cancelButton = connectCancel guiComponents
        cancelHandler = closeDialog $ connectDialog guiComponents

    onButtonClick cancelButton cancelHandler
 
showConnectDialog :: Components -> IO ()
showConnectDialog guiComponents =
  do
    showDialog $ connectDialog guiComponents
