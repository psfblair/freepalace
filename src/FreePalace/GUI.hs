module FreePalace.GUI where

import FreePalace.Handlers

class Components a where
  mainWindow :: (Main b, Window b) => a -> b
  connectDialog :: Dialog b => a -> b
  connectOk :: Button b => a -> b
  connectCancel :: Button b => a -> b

class Events a where
  onWindowClose :: Window b => a -> b -> IO () -> IO () 
  onButtonClicked :: Button b => a -> b -> IO () -> IO ()

class Main a where
  quit :: a -> IO ()
  
class Window a where
  showWindow :: a -> IO ()
  closeWindow :: a -> IO ()
  
class Dialog a where
  showDialog :: a -> IO ()
  closeDialog :: a -> IO ()
  
class Button a
  

setUpGUI :: (Components a, Events b) => a -> b -> Handlers -> IO ()
setUpGUI guiComponents guiEvents handlers =
  do
    setUpMainWindow guiComponents guiEvents
    setUpConnectDialog guiComponents guiEvents handlers
    showConnectDialog guiComponents

setUpMainWindow :: (Components a, Events b) => a -> b -> IO ()
setUpMainWindow guiComponents guiEvents =
  -- on open, show connect dialog - why does doing it this way open
  -- the dialog behind the main window?
  -- afterShow (mainWindow gui) (openConnectDialog gui)
  let window = mainWindow guiComponents 
      onClose = onWindowClose guiEvents in
  onClose window (quit window)

setUpConnectDialog :: (Components a, Events b) => a -> b -> Handlers -> IO ()
setUpConnectDialog guiComponents guiEvents handlers =
    let okButton = connectOk guiComponents
        cancelButton = connectCancel guiComponents
        onClick = onButtonClicked guiEvents
        okHandler = closeDialog $ connectDialog guiComponents
        cancelHandler = closeDialog $ connectDialog guiComponents in
     onClick okButton okHandler
     onClick cancelButton cancelHandler
 
showConnectDialog :: Components a => a -> IO ()
showConnectDialog guiComponents =
  showDialog $ connectDialog guiComponents
