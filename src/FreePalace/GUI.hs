module FreePalace.GUI where

import qualified FreePalace.State as State
import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.Net.Types as Net
import FreePalace.GUI.Types

initializeGUI :: Components -> (Net.Hostname -> Net.PortId -> IO ()) -> IO ()
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
    onMainWindowClose mainWin quitAction

setUpConnectDialog :: Components -> (Net.Hostname -> Net.PortId -> IO ()) -> IO ()
setUpConnectDialog guiComponents connectHandler =
  do
    let okButton = connectOk guiComponents 
        okHandler = do
          host <- textValue $ connectHostEntry guiComponents
          port <- textValue $ connectPortEntry guiComponents
          connectHandler host port


    onButtonClick okButton okHandler
    
    let cancelButton = connectCancel guiComponents
        cancelHandler = closeDialog $ connectDialog guiComponents

    onButtonClick cancelButton cancelHandler
 
showConnectDialog :: Components -> IO ()
showConnectDialog guiComponents = showDialog $ connectDialog guiComponents

