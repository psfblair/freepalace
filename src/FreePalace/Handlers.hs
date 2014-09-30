module FreePalace.Handlers where

import System.IO
import Control.Applicative
import qualified FreePalace.Net as Net
import qualified FreePalace.Net.Messages as Msg

type ConnectRequestHandler = String -> String -> IO GUIEventHandlers

handleConnectRequested :: Net.Connectors -> ConnectRequestHandler
handleConnectRequested connectors host portString =
  do
    let connector = Net.connector connectors
    connectionHandle <- connector (host::Net.Hostname) (portString::Net.PortId) 
    -- Now in STATE_HANDSHAKING
    handleHandshake connectionHandle  --TODO Time out if this takes too long
    
    -- Now in STATE_READY
    -- Fork off socket listening thread, passing in GUI?
    -- Bind event handlers to the IO Handle
    -- Bind GUI to the handlers - how?
    return GUIEventHandlers

handleHandshake :: Net.IncomingByteSource -> IO (Either String Net.IncomingByteSource)
handleHandshake connectionHandle =
  do
    msgType <- Msg.messageType <$> (Msg.readHeader connectionHandle)
    case msgType of
      Msg.LittleEndianServer ->  return $ Right connectionHandle -- TODO send logon
      Msg.BigEndianServer    ->  return $ Left "Connection with big endian servers not supported" -- TODO Use different readWords functions
      Msg.UnknownServer      ->  return $ Left "Unknown server type"
      _                      ->  return $ Left "Unknown server type"

data GUIEventHandlers = GUIEventHandlers {
  -- connectOkHandler :: String -> String -> IO Handle
}

-- Do we need this? Somehow we need to read data from the socket and dispatch, and also write to it
-- but neither should block the other.
data IncomingMessageHandlers = IncomingMessageHandlers {

}
