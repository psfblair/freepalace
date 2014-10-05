module Main where

import Network

import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.GUI.Gtk as Gtk
import qualified FreePalace.Net as Net
import System.Log.Logger

import Paths_freepalace(getDataFileName)

-- TODO abstract out withSocketsDo?
main = withSocketsDo $ do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  let connectionRequestHandler = Handlers.handleConnectRequested Net.socketConnectors
  gladeFilename <- getDataFileName "freepalace.resources.glade"
  Gtk.main gladeFilename connectionRequestHandler
