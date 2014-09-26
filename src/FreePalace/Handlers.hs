module FreePalace.Handlers where

import qualified FreePalace.Net as Net

data GUIEventHandlers = GUIEventHandlers {
  connectOkHandler :: IO String -> IO String -> IO ()
}

guiEventHandlers :: Net.Transmitters -> GUIEventHandlers
guiEventHandlers transmitters  =
  let transmitter = Net.connectTransmitter transmitters in
  GUIEventHandlers {
    connectOkHandler = handleConnect transmitter
  }

handleConnect :: (String -> String -> IO()) -> IO String -> IO String -> IO ()
handleConnect requestTransmitter hostSource portSource =
  do
    host <- hostSource
    port <- portSource
    requestTransmitter host port -- Here we need the actual login
      -- information from configurations
