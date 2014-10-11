module Main where

import Control.Applicative
import Network
import System.Log.Logger

import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.GUI.Gtk as Gtk
import qualified FreePalace.GUI as GUI
import qualified FreePalace.Net as Net


import Paths_freepalace(getDataFileName)

-- TODO abstract out withSocketsDo?
main = withSocketsDo $ do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  dataFileName <- getDataFileName "freepalace.resources.glade"
  guiComponents <- Gtk.init dataFileName
  let connectionRequestHandler = Handlers.handleConnectRequested guiComponents Net.socketConnectors
  GUI.initializeGUI guiComponents connectionRequestHandler
  Gtk.start
