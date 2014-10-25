module FreePalace.GUI.Gtk where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import qualified Graphics.UI.Gtk.Display.Image as Image
import qualified Graphics.UI.Gtk.Abstract.Widget as Widget
import qualified Graphics.UI.Gtk.Gdk.Pixbuf as Pixbuf
import qualified Graphics.UI.Gtk.Gdk.Events as Events
-- import qualified Graphics.Rendering.Pango.Structs as PangoStructs (Rectangle)
import Foreign.Marshal.Array (newArray)
import Control.Concurrent
import System.IO

import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.Messages as Messages
import qualified FreePalace.Media.Loader as MediaLoader

data GtkGui = GtkGui {
    mainWindow :: Window

  , connectDialog :: Dialog
  , connectHostEntry :: Entry
  , connectPortEntry :: Entry
  , connectOk :: Button
  , connectCancel :: Button
   
  , logWindow :: Window
  , logTextView :: TextView
  , logTextBuffer :: TextBuffer
  
  , chatEntry :: Entry
  , chatButton :: Button

  , roomImage :: Image
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
    windowMove mainWindow 322 50
    connectDialog <- builderGetObject builder castToDialog "connectDialog"
    connectHostEntry <- builderGetObject builder castToEntry "hostEntry"
    widgetGrabFocus connectHostEntry
    connectPortEntry <- builderGetObject builder castToEntry "portEntry"
    connectOkButton <- builderGetObject builder castToButton "connectOk"
    connectCancelButton <- builderGetObject builder castToButton "connectCancel"

    logWindow <- builderGetObject builder castToWindow "logWindow"
    windowMove logWindow 322 671
    logTextView <- builderGetObject builder castToTextView "logTextView"
    widgetModifyBg logTextView StateNormal (Color 0xFFFF 0xFFFF 0xFFFF)
    logTextBuffer <- builderGetObject builder castToTextBuffer "logTextBuffer"

    chatEntry <- builderGetObject builder castToEntry "chatEntry"
    widgetGrabFocus chatEntry 
    chatButton <- builderGetObject builder castToButton "chatButton"

    roomImage <- builderGetObject builder castToImage "roomImage"
    
    return $ GtkGui mainWindow
      connectDialog connectHostEntry connectPortEntry connectOkButton connectCancelButton
      logWindow logTextView logTextBuffer
      chatEntry chatButton
      roomImage

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

wrapEntry :: Entry -> GUI.TextField
wrapEntry entry =
  GUI.TextField {
    GUI.textValue = entryGetText entry,
    GUI.clearTextEntry = entrySetText entry "",
    GUI.onEnterKeyPress =
      \handler -> do
        onEntryActivate entry handler
        return ()
  }

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


-- TODO: Problem with background: Will image resize when window is resized?
wrapDrawingArea :: Image -> GUI.Canvas
wrapDrawingArea roomImage = GUI.Canvas {
  GUI.displayBackground = \imagePath ->
                           do
                             -- Why TF doesn't this work (and not with Widget or anything else)
                             -- width <- widgetGetAllocatedWidth roomImage 
                             -- height <- widgetGetAllocatedHeight roomImage
                             (Events.Rectangle _  _ width height) <- Widget.widgetGetAllocation roomImage 
                             pixbuf <- Pixbuf.pixbufNewFromFileAtScale imagePath width height True
                             Image.imageSetFromPixbuf roomImage pixbuf
  }

{-                            
wrapDrawingArea :: Image -> GUI.Canvas
wrapDrawingArea roomImage = GUI.Canvas {
  GUI.displayBackground = \image ->
                           do
                             let width = 
                                 height = 
                                 hasAlpha = false
                                 bitsPerSample = 8
                                 rowOffset = width * 3 -- "row stride," i.e. the number of bytes between rows; I think that means the byte-width of a row
                             img_ptr <- newArray (map CUChar image)
                             -- Pixbuf only supports RGB right now
                             pixbuf <- pixbufNewFromData img_ptr ColorspaceRgb hasAlpha bitsPerSample width height rowOffset
                             Image.imageSetFromPixbuf roomImage pixbuf
  }

-}

{-
Using PixbufLoader (run gdk-pixbuf-query-loaders to see if e.g., JPEG is installed.):

    loader = gdk_pixbuf_loader_new ();
    gdk_pixbuf_loader_write (loader, buffer, length, NULL); //length is length of buffer containing JPEG
    pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
    Graphics.UI.Gtk.Gdk.Drawable.drawPixbuf
-}
{-
JuicyPixels-3.1.7.1
decodeImage :: ByteString -> Either String DynamicImage
-}
