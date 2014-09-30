module Main where

import Network

import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.GUI.Gtk as Gtk
import qualified FreePalace.Net.Socket as Socket

import Paths_freepalace(getDataFileName)

-- TODO abstract out withSocketsDo?
main = withSocketsDo $ do
  let connectionRequestHandler = Handlers.handleConnectRequested Socket.connectors
  gladeFilename <- getDataFileName "freepalace.resources.glade"
  Gtk.main gladeFilename connectionRequestHandler
