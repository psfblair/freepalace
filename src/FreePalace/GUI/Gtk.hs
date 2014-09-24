module FreePalace.GUI.Gtk where

import qualified FreePalace.GUI
import FreePalace.Handlers

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Concurrent

data GtkComponents = GtkComponents {
  mainWindow :: MainWindow,
  connectDialog :: Dialog,
  connectDialogLabel :: Label,
  connectOk :: Button,
  connectCancel :: Button
}

instance FreePalace.GUI.Components GtkComponents where
  FreePalace.GUI.mainWindow gui = mainWindow gui
  FreePalace.GUI.connectDialog gui  = connectDialog gui
  FreePalace.GUI.connectOk gui = connectOk gui
  FreePalace.GUI.connectCancel gui = connectCancel gui

data GtkGUIBuilders = GtkGUIBuilders

instance FreePalace.GUI.Builders GtkGUIBuilders where
  onWindowClose builders = onDestroy
  onButtonClicked builders = onClicked

data MainWindow = MainWindow Window  

instance FreePalace.GUI.Main MainWindow where
  quit window = mainQuit

instance FreePalace.GUI.Window MainWindow where
  show MainWindow window = windowPresent window

instance FreePalace.GUI.Dialog Dialog where
  close dialog = widgetHide dialog
  show  dialog = windowPresent dialog

instance FreePalace.GUI.Button Button

main :: FilePath -> Handlers -> IO ()
main gladepath handlers =
  do
    initGUI
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGladeComponents gladepath
    setUpGUI gui GtkGUIBuilders handlers GtkRenderers
    mainGUI

loadGladeComponents :: FilePath -> IO (GtkGUI)
loadGladeComponents gladepath =
  do
    Just gladeXml <- xmlNew gladepath
    mainWindow <- MainWindow $ xmlGetWidget gladeXml castToWindow "mainWindow"

    connectDialog <- xmlGetWidget gladeXml castToDialog "connectDialog"
    connectDialogLabel <- xmlGetWidget gladeXml castToLabel "connectDialogLabel"
    connectOkButton <- xmlGetWidget gladeXml castToButton "connectOk"
    connectCancelButton <- xmlGetWidget gladeXml castToButton "connectCancel"

    return $ GtkComponents mainWindow connectDialog connectDialogLabel connectOkButton connectCancelButton
