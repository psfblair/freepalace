module Main where

import Control.Applicative
import Network
import System.Log.Logger

import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.GUI.Gtk as Gtk
import qualified FreePalace.GUI as GUI
import qualified FreePalace.State as State

import Paths_freepalace(getDataFileName)

-- TODO abstract out withSocketsDo?
main = withSocketsDo $ do
  setLoggingLevels
  guiDataFileName <- getDataFileName "freepalace.resources.glade"
  guiComponents <- Gtk.init guiDataFileName
  let initialState = State.DisconnectedState (State.Disconnected guiComponents State.HostDirectory State.defaultSettings) -- TODO configs from config file 
      connectionRequestHandler = Handlers.handleConnectRequested initialState State.PalaceProtocol
  GUI.initializeGUI guiComponents connectionRequestHandler
  Gtk.start


setLoggingLevels =
  do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    updateGlobalLogger "Incoming.Message.Await" (setLevel ERROR)
    updateGlobalLogger "Incoming.Message.Processed" (setLevel ERROR)
    updateGlobalLogger "Incoming.Message.HttpServerLocation.Processed" (setLevel ERROR)
    updateGlobalLogger "Incoming.Message.GotRoomDescription.Processed" (setLevel ERROR)
    return ()
