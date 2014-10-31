module Main where

import           Network
import           System.Log.Logger

import qualified FreePalace.Domain.Host       as Host
import qualified FreePalace.Domain.Net        as Net
import qualified FreePalace.Domain.State      as State
import qualified FreePalace.GUI               as GUI
import qualified FreePalace.GUI.Gtk           as Gtk
import qualified FreePalace.Handlers.Outgoing as GuiHandlers

import           Paths_freepalace             (getDataFileName)

main :: IO ()
main = withSocketsDo $ do -- TODO abstract out withSocketsDo?
  setLoggingLevels
  guiDataFileName <- getDataFileName "freepalace.resources.glade"
  guiComponents <- Gtk.init guiDataFileName
  let initialState = State.DisconnectedState (State.Disconnected guiComponents Host.HostDirectory State.defaultSettings) -- TODO configs from config file
      connectionRequestHandler = GuiHandlers.handleConnectRequested initialState Net.PalaceProtocol
  GUI.initializeGUI guiComponents connectionRequestHandler
  Gtk.start

-- TODO Allow this to be set in configuration
setLoggingLevels :: IO ()
setLoggingLevels =
  do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    updateGlobalLogger "Incoming.Message.Await" (setLevel ERROR)
    updateGlobalLogger "Incoming.Message.Processed" (setLevel ERROR)
    updateGlobalLogger "Incoming.Message.HttpServerLocation.Processed" (setLevel ERROR)
    updateGlobalLogger "Incoming.Message.GotRoomDescription.Processed" (setLevel ERROR)
    return ()
