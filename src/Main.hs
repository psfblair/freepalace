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
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  dataFileName <- getDataFileName "freepalace.resources.glade"
  guiComponents <- Gtk.init dataFileName
  let initialState = State.DisconnectedState (State.Disconnected guiComponents) 
      connectionRequestHandler = Handlers.handleConnectRequested initialState State.PalaceProtocol
  GUI.initializeGUI guiComponents connectionRequestHandler
  Gtk.start


