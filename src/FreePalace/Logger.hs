module FreePalace.Logger where

import qualified FreePalace.Domain.GUI as GUI

import qualified System.Log.Logger     as Log

data Loggable = Loggable LoggerName LogMessage
type LoggerName = String
type LogMessage = String

fromGuiEvent :: GUI.GuiEvent -> Loggable
fromGuiEvent event =
  Loggable ("Gui." ++ eventName) eventName
  where eventName = show event

debugLog :: Loggable -> IO ()
debugLog (Loggable loggerName logMessage) = Log.debugM loggerName logMessage

