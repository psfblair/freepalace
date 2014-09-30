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

initializeGUI :: Components -> Handlers.ConnectRequestHandler -> IO ()
initializeGUI guiComponents handler =
  do
    setUpMainWindow guiComponents
    setUpConnectDialog guiComponents handler
    showConnectDialog guiComponents

setUpMainWindow :: Components -> IO ()
setUpMainWindow guiComponents =
  do
    let mainWin = mainWindow guiComponents
        quitAction = quit mainWin
    onWindowClose mainWin quitAction

setUpConnectDialog :: Components -> Handlers.ConnectRequestHandler -> IO ()
setUpConnectDialog guiComponents connectHandler =
  do
    let okButton = connectOk guiComponents
        okHandler = do
          host <- textValue $ connectHostEntry guiComponents
          port <- textValue $ connectPortEntry guiComponents
          guiEventHandlers <- connectHandler host port
          addHandlersToGUI guiComponents guiEventHandlers
          closeDialog $ connectDialog guiComponents
    onButtonClick okButton okHandler
    
    let cancelButton = connectCancel guiComponents
        cancelHandler = closeDialog $ connectDialog guiComponents

    onButtonClick cancelButton cancelHandler
 
showConnectDialog :: Components -> IO ()
showConnectDialog guiComponents = showDialog $ connectDialog guiComponents

addHandlersToGUI :: Components -> Handlers.GUIEventHandlers -> IO ()
addHandlersToGUI guiComponents guiEventHandlers = return ()
