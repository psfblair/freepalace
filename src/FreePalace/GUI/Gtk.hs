module FreePalace.GUI.Gtk where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Control.Concurrent
import System.IO

import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.Messages as Messages

data GtkGui = GtkGui {
  mainWindow :: Window,

  connectDialog :: Dialog,
  connectHostEntry :: Entry,
  connectPortEntry :: Entry,
  connectOk :: Button,
  connectCancel :: Button,

  logWindow :: Window,
  logTextView :: TextView,
  logTextBuffer :: TextBuffer
}


init :: FilePath -> IO (GUI.Components)
init gladepath =
  do
    initGUI
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGladeComponents gladepath
    return $ wrapComponents gui

start :: IO ()
start = mainGUI

loadGladeComponents :: FilePath -> IO GtkGui
loadGladeComponents gladepath =
  do
    builder <- builderNew
    builderAddFromFile builder gladepath

    mainWindow <- builderGetObject builder castToWindow "mainWindow"

    connectDialog <- builderGetObject builder castToDialog "connectDialog"
    connectHostEntry <- builderGetObject builder castToEntry "hostEntry"
    connectPortEntry <- builderGetObject builder castToEntry "portEntry"
    connectOkButton <- builderGetObject builder castToButton "connectOk"
    connectCancelButton <- builderGetObject builder castToButton "connectCancel"

    logWindow <- builderGetObject builder castToWindow "logWindow"
    logTextView <- builderGetObject builder castToTextView "logTextView"
    logTextBuffer <- builderGetObject builder castToTextBuffer "logTextBuffer"

    return $ GtkGui mainWindow
      connectDialog connectHostEntry connectPortEntry connectOkButton connectCancelButton
      logWindow logTextView logTextBuffer

wrapComponents :: GtkGui -> GUI.Components
wrapComponents gui =
  GUI.Components {
    GUI.mainWindow = wrapMainWindow gui,
    
    GUI.connectDialog = wrapConnectDialog gui,
    GUI.connectHostEntry = wrapConnectHostEntry gui,
    GUI.connectPortEntry = wrapConnectPortEntry gui,
    GUI.connectOk = wrapConnectOkButton gui,
    GUI.connectCancel = wrapConnectCancelButton gui,

    GUI.logWindow = wrapLogWindow gui
  }

wrapMainWindow :: GtkGui -> GUI.MainWindow
wrapMainWindow gui =
  let mainWin = mainWindow gui in
   GUI.MainWindow {
     GUI.quit = mainQuit,
     GUI.showMainWindow = windowPresent mainWin,
     GUI.closeMainWindow = widgetHide mainWin,
     GUI.onMainWindowClose =
       \handler -> do
         onDestroy mainWin handler
         return ()
   }

wrapConnectDialog :: GtkGui -> GUI.Dialog
wrapConnectDialog gui =
  let dialog = connectDialog gui in
  GUI.Dialog {
    GUI.showDialog = windowPresent dialog,
    GUI.closeDialog = widgetHide dialog
  }

wrapConnectHostEntry :: GtkGui -> GUI.TextField
wrapConnectHostEntry gui =
  wrapEntry $ connectHostEntry gui

wrapConnectPortEntry :: GtkGui -> GUI.TextField
wrapConnectPortEntry gui =
  wrapEntry $ connectPortEntry gui

wrapEntry :: Entry -> GUI.TextField
wrapEntry entry =
  GUI.TextField {
    GUI.textValue = entryGetText entry
  }

wrapConnectOkButton :: GtkGui -> GUI.Button
wrapConnectOkButton gui =
  wrapButton $ connectOk gui

wrapConnectCancelButton :: GtkGui -> GUI.Button
wrapConnectCancelButton gui =
  wrapButton $ connectCancel gui

wrapButton :: Button -> GUI.Button
wrapButton button = 
  GUI.Button {
    GUI.onButtonClick =
       \handler -> do
         onClicked button handler
         return ()
  }

wrapLogWindow :: GtkGui -> GUI.LogWindow
wrapLogWindow gui =
  let logWin = logWindow gui
      textBuffer = logTextBuffer gui
      textView = logTextView gui
  in GUI.LogWindow {
    GUI.showLogWindow = windowPresent logWin,
    GUI.closeLogWindow = widgetHide logWin,
    GUI.appendMessage = \chat ->
                         do
                           -- TODO Handle different message types: whisper, room message, thought balloon, etc. using styled text
                           -- TODO - ultimately want this in 2-column format, with message wrapping only in right-hand column
                           -- TODO - want timestamp when we actually save the log, but not for display
                           let user = Messages.userName $ Messages.speaker chat
                               message = Messages.message chat
                               stringToLog = user ++ ":\t" ++ message ++ "\n"
                           iter <- textBufferGetEndIter textBuffer
                           textBufferInsert textBuffer iter stringToLog
                           textViewScrollToIter textView iter 0.0 Nothing  
                           return () -- TODO store in message log for saving later
  }
