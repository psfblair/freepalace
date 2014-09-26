module FreePalace.Net.Socket where

import qualified FreePalace.Net as Net

transmitters :: Net.Transmitters
transmitters = Net.Transmitters {
  Net.connectTransmitter = transmitLogin
}

transmitLogin :: String -> String -> IO ()
transmitLogin host port = print $ "Transmitting to: " ++ host ++ ":" ++ port ++ " !"
