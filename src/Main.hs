module Main where

import qualified FreePalace.Handlers as Handlers
import qualified FreePalace.GUI.Gtk as Gtk
import qualified FreePalace.Net.Socket as Socket

import Paths_freepalace(getDataFileName)

{-
guiEventHandlers =
serverEventHandlers =

main = Freepalace.GUI.Gtk.main guiEventHandlers serverEventHandlers "freepalace.resources.glade"
-}

main = do
  let transmitters = Socket.transmitters
      guiEventHandlers = Handlers.guiEventHandlers transmitters
  gladeFilename <- getDataFileName "freepalace.resources.glade"
  Gtk.main gladeFilename guiEventHandlers




