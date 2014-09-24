 module FreePalace.GUI where

import FreePalace.Handlers

class WindowClass a
class MainClass a
class DialogClass a
class ButtonClass a

data MainWindow = MainWindow {
  window :: (WindowClass a) => a, 
  main :: (MainClass a) => a,
  quit :: IO (),
  showWindow :: IO (),
  closeWindow :: IO ()
}

data Dialog = Dialog {
  dialog :: (DialogClass a) => a,
  showDialog :: IO (),
  closeDialog :: IO ()
}

data Components = Components {
  mainWindow :: MainWindow,
  connectDialog :: Dialog,
  connectOk :: (ButtonClass a) => a,
  connectCancel :: (ButtonClass a) => a
}

data Events = Events {
  onWindowClose :: WindowClass a => a -> IO () -> IO (), 
  onButtonClicked :: ButtonClass a => a -> IO () -> IO ()
}


setUpGUI :: Components -> Events -> Handlers -> IO ()
setUpGUI guiComponents guiEvents handlers =
  do
    setUpMainWindow guiComponents guiEvents
    setUpConnectDialog guiComponents guiEvents handlers
    showConnectDialog guiComponents

setUpMainWindow :: Components -> Events -> IO ()
setUpMainWindow guiComponents guiEvents =
  do
    -- on open, show connect dialog - why does doing it this way open
    -- the dialog behind the main window?
    -- afterShow (mainWindow gui) (openConnectDialog gui)
    let quitAction = quit . main $ mainWindow guiComponents
        mainWin = window $ mainWindow guiComponents
        onClose = onWindowClose guiEvents
    onClose mainWin quitAction

setUpConnectDialog :: Components -> Events -> Handlers -> IO ()
setUpConnectDialog guiComponents guiEvents handlers =
  do
    let onClick = onButtonClicked guiEvents
        okButton = connectOk guiComponents
        okHandler = closeDialog $ connectDialog guiComponents
    onClick okButton okHandler
    
    let cancelButton = connectCancel guiComponents
        cancelHandler = closeDialog $ connectDialog guiComponents

    onClick cancelButton cancelHandler
 
showConnectDialog :: Components -> IO ()
showConnectDialog guiComponents =
  do
    showDialog $ connectDialog guiComponents
