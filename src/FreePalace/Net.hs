module FreePalace.Net where

data Transmitters = Transmitters {
  connectTransmitter :: String -> String -> IO ()
}
