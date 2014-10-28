module FreePalace.GUI.Gtk where

import           Control.Concurrent
import           Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as Widget
import qualified Graphics.UI.Gtk.Display.Image   as Image
import qualified Graphics.UI.Gtk.Gdk.Events      as Events
import qualified Graphics.UI.Gtk.Gdk.Pixbuf      as Pixbuf

import qualified FreePalace.Domain.Chat          as Chat
import qualified FreePalace.Domain.GUI           as GUI
import qualified FreePalace.Domain.User          as User

data GtkGui = GtkGui {
    mainWindow       :: Window

  , connectDialog    :: Dialog
  , connectHostEntry :: Entry
  , connectPortEntry :: Entry
  , connectOk        :: Button
  , connectCancel    :: Button

  , logWindow        :: Window
  , logTextView      :: TextView
  , logTextBuffer    :: TextBuffer

  , chatEntry        :: Entry
  , chatButton       :: Button

  , roomImage        :: Image
}

init :: FilePath -> IO (GUI.Components)
init gladepath =
  do
    _ <- initGUI
    _ <- timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGladeComponents gladepath
    return $ wrapComponents gui

start :: IO ()
start = mainGUI

loadGladeComponents :: FilePath -> IO GtkGui
loadGladeComponents gladepath =
  do
    builder <- builderNew
    builderAddFromFile builder gladepath

    guiMainWindow <- builderGetObject builder castToWindow "mainWindow"
    windowMove guiMainWindow 322 50
    guiConnectDialog <- builderGetObject builder castToDialog "connectDialog"
    guiConnectHostEntry <- builderGetObject builder castToEntry "hostEntry"
    widgetGrabFocus guiConnectHostEntry
    guiConnectPortEntry <- builderGetObject builder castToEntry "portEntry"
    guiConnectOkButton <- builderGetObject builder castToButton "connectOk"
    guiConnectCancelButton <- builderGetObject builder castToButton "connectCancel"

    guiLogWindow <- builderGetObject builder castToWindow "logWindow"
    windowMove guiLogWindow 322 671
    guiLogTextView <- builderGetObject builder castToTextView "logTextView"
    widgetModifyBg guiLogTextView StateNormal (Color 0xFFFF 0xFFFF 0xFFFF)
    guiLogTextBuffer <- builderGetObject builder castToTextBuffer "logTextBuffer"

    guiChatEntry <- builderGetObject builder castToEntry "chatEntry"
    widgetGrabFocus guiChatEntry
    guiChatButton <- builderGetObject builder castToButton "chatButton"

    guiRoomImage <- builderGetObject builder castToImage "roomImage"

    return $ GtkGui guiMainWindow
      guiConnectDialog guiConnectHostEntry guiConnectPortEntry guiConnectOkButton guiConnectCancelButton
      guiLogWindow guiLogTextView guiLogTextBuffer
      guiChatEntry guiChatButton
      guiRoomImage

wrapComponents :: GtkGui -> GUI.Components
wrapComponents gui =
  GUI.Components {
      GUI.mainWindow = wrapMainWindow gui

    , GUI.connectDialog = wrapConnectDialog gui
    , GUI.connectHostEntry = wrapEntry $ connectHostEntry gui
    , GUI.connectPortEntry = wrapEntry $ connectPortEntry gui
    , GUI.connectOk = wrapButton $ connectOk gui
    , GUI.connectCancel = wrapButton $ connectCancel gui

    , GUI.logWindow = wrapLogWindow gui

    , GUI.chatEntry = wrapEntry $ chatEntry gui
    , GUI.chatSend = wrapButton $ chatButton gui

    , GUI.roomCanvas = wrapDrawingArea $ roomImage gui
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
         _ <- onDestroy mainWin handler
         return ()
   }

wrapConnectDialog :: GtkGui -> GUI.Dialog
wrapConnectDialog gui =
  let dialog = connectDialog gui in
  GUI.Dialog {
    GUI.showDialog = windowPresent dialog,
    GUI.closeDialog = widgetHide dialog
  }

wrapEntry :: Entry -> GUI.TextField
wrapEntry entry =
  GUI.TextField {
    GUI.textValue = entryGetText entry,
    GUI.clearTextEntry = entrySetText entry "",
    GUI.onEnterKeyPress =
      \handler -> do
        _ <- onEntryActivate entry handler
        return ()
  }

wrapButton :: Button -> GUI.Button
wrapButton button =
  GUI.Button {
    GUI.onButtonClick =
       \handler -> do
         _ <- onClicked button handler
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
                           let user = User.userName $ Chat.speaker chat
                               message = Chat.message chat
                               stringToLog = user ++ ":\t" ++ message ++ "\n"
                           iter <- textBufferGetEndIter textBuffer
                           textBufferInsert textBuffer iter stringToLog
                           _ <- textViewScrollToIter textView iter 0.0 Nothing
                           return () -- TODO store in message log for saving later
  }

-- TODO: Keep pixbuf around, redraw to scale when window is resized
wrapDrawingArea :: Image -> GUI.Canvas
wrapDrawingArea roomBgImage = GUI.Canvas {
  GUI.displayBackground = \imagePath ->
                           do
                             (Events.Rectangle _  _ width height) <- Widget.widgetGetAllocation roomBgImage
                             pixbuf <- Pixbuf.pixbufNewFromFileAtScale imagePath width height True
                             Image.imageSetFromPixbuf roomBgImage pixbuf
  }
