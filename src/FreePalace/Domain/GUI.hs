module FreePalace.Domain.GUI where

import qualified System.FilePath        as Path
import qualified FRP.Sodium             as FRP

import qualified FreePalace.Domain.Chat as Chat


data GuiAction = DisconnectSelected
     	       | ConnectSelected
     	       | QuitSelected

     	       | ConnectOkClicked 
     	       | ConnectCancelClicked

     	       | ChatSendClicked
     	       | ChatEntered  deriving Show

data MainWindow = MainWindow {
  quit              :: IO (),
  showMainWindow    :: IO (),
  closeMainWindow   :: IO (),
  onMainWindowClose :: IO () -> IO ()
}

data LogWindow = LogWindow {
  showLogWindow  :: IO (),
  closeLogWindow :: IO (),
  appendMessage  :: Chat.Communication -> IO ()
}

data Dialog = Dialog {
  showDialog  :: IO (),
  closeDialog :: IO ()
}

data Button = Button {
  onButtonClick :: IO () -> IO ()
}

data TextField = TextField {
  textValue       :: IO String,
  clearTextEntry  :: IO (),
  onEnterKeyPress :: IO () -> IO ()
}

data Canvas = Canvas {
  displayBackground :: Path.FilePath -> IO ()
}

data MenuItem = MenuItem {
  onMenuItemSelect :: IO () -> IO ()
}

data Components = Components {
    mainWindow       :: MainWindow

  , connectDialog    :: Dialog
  , connectHostEntry :: TextField
  , connectPortEntry :: TextField
  , connectOk        :: Button
  , connectCancel    :: Button

  , logWindow        :: LogWindow

  , chatEntry        :: TextField
  , chatSend         :: Button

  , roomCanvas       :: Canvas

  , connectMenuItem    :: MenuItem
  , disconnectMenuItem :: MenuItem
  , quitMenuItem       :: MenuItem
}

instance Show Components where
  show _ = "Components"

bindComponents :: Components -> IO (FRP.Event GuiAction)
bindComponents guiComponents =
  do  
    let connectOkButton = connectOk guiComponents
        connectCancelButton = connectCancel guiComponents
        chatEntryField = chatEntry guiComponents
        chatSendButton = chatSend guiComponents
        connectMenuOption = connectMenuItem guiComponents
        disconnectMenuOption = disconnectMenuItem guiComponents
        quitMenuOption = quitMenuItem guiComponents
        
    (eGuiAction, pushGuiAction) <- FRP.sync FRP.newEvent

    onButtonClick connectOkButton         (FRP.sync $ pushGuiAction ConnectOkClicked)
    onButtonClick connectCancelButton     (FRP.sync $ pushGuiAction ConnectCancelClicked)

    onEnterKeyPress chatEntryField        (FRP.sync $ pushGuiAction ChatEntered)
    onButtonClick chatSendButton          (FRP.sync $ pushGuiAction ChatSendClicked)

    onMenuItemSelect connectMenuOption    (FRP.sync $ pushGuiAction ConnectSelected)
    onMenuItemSelect disconnectMenuOption (FRP.sync $ pushGuiAction DisconnectSelected)
    onMenuItemSelect quitMenuOption       (FRP.sync $ pushGuiAction QuitSelected)

    return eGuiAction

