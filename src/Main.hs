module Main where

import FreePalace.Handlers
import qualified FreePalace.GUI.Gtk

import Paths_freepalace(getDataFileName)

{-
guiEventHandlers =
serverEventHandlers =

main = Freepalace.GUI.Gtk.main guiEventHandlers serverEventHandlers "freepalace.resources.glade"
-}

main = do
  let handlers =  setUpHandlers
  gladeFilename <- getDataFileName "freepalace.resources.glade"
  FreePalace.GUI.Gtk.main gladeFilename handlers renderers




