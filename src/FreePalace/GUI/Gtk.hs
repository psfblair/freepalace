module FreePalace.GUI.Gtk where

import qualified FreePalace.GUI as GUI
import qualified FreePalace.Handlers as Handlers

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent

data GtkGui = GtkGui {
  mainWindow :: Window,
  connectDialog :: Dialog,
  connectHostEntry :: Entry,
  connectPortEntry :: Entry,
  connectOk :: Button,
  connectCancel :: Button
}


main :: FilePath -> Handlers.GUIEventHandlers -> IO ()
main gladepath handlers =
  do
    initGUI
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGladeComponents gladepath
    let components = wrapComponents gui
    GUI.setUpGUI components handlers 
    mainGUI

loadGladeComponents :: FilePath -> IO GtkGui
loadGladeComponents gladepath =
  do
    Just gladeXml <- xmlNew gladepath
    mainWindow <- xmlGetWidget gladeXml castToWindow "mainWindow"

    connectDialog <- xmlGetWidget gladeXml castToDialog "connectDialog"
    connectHostEntry <- xmlGetWidget gladeXml castToEntry "hostEntry"
    connectPortEntry <- xmlGetWidget gladeXml castToEntry "portEntry"
    connectOkButton <- xmlGetWidget gladeXml castToButton "connectOk"
    connectCancelButton <- xmlGetWidget gladeXml castToButton "connectCancel"

    return $ GtkGui mainWindow connectDialog connectHostEntry connectPortEntry connectOkButton connectCancelButton

wrapComponents :: GtkGui -> GUI.Components
wrapComponents gui =
  GUI.Components {
    GUI.mainWindow = wrapMainWindow gui,
    GUI.connectDialog = wrapConnectDialog gui,
    GUI.connectHostEntry = wrapConnectHostEntry gui,
    GUI.connectPortEntry = wrapConnectPortEntry gui,
    GUI.connectOk = wrapConnectOkButton gui,
    GUI.connectCancel = wrapConnectCancelButton gui
  }

wrapMainWindow gui =
  let mainWin = mainWindow gui in
   GUI.MainWindow {
     GUI.quit = mainQuit,
     GUI.showWindow = windowPresent mainWin,
     GUI.closeWindow = widgetHide mainWin,
     GUI.onWindowClose =
       \handler -> do
         onDestroy mainWin handler
         return ()
   }

wrapConnectDialog gui =
  let dialog = connectDialog gui in
  GUI.Dialog {
    GUI.showDialog = windowPresent dialog,
    GUI.closeDialog = widgetHide dialog
  }

wrapConnectHostEntry gui =
  wrapEntry $ connectHostEntry gui

wrapConnectPortEntry gui =
  wrapEntry $ connectPortEntry gui

wrapEntry entry =
  GUI.TextField {
    GUI.textValue = entryGetText entry
  }
  
wrapConnectOkButton gui =
  wrapButton $ connectOk gui

wrapConnectCancelButton gui =
  wrapButton $ connectCancel gui

wrapButton button = 
  GUI.Button {
    GUI.onButtonClick =
       \handler -> do
         onClicked button handler
         return ()
  }
