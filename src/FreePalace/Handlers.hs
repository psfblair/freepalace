module FreePalace.Handlers where

import System.IO
import Control.Applicative
import qualified FreePalace.Net as Net
import qualified FreePalace.Net.Messages as Msg
import qualified FreePalace.Net.Receive as Recv
import qualified FreePalace.Net.Send as Send

type ConnectRequestHandler = String -> String -> IO ()

handleConnectRequested :: Net.Connectors -> ConnectRequestHandler
handleConnectRequested connectors host portString =
  do
    let connector = Net.connector connectors
    channels <- connector (host::Net.Hostname) (portString::Net.PortId) 
    handshake <- handleHandshake $ Net.source channels    -- Now in STATE_HANDSHAKING
    case handshake of
      Left msg -> return () -- TODO Provide some way to indicate connection failed. Don't close the connection window.
      Right source ->  -- Now in STATE_READY 
        do
          Send.sendLogin (Net.sink channels) (Msg.UserId { Msg.userName = "Haskell Curry"} )  -- TODO Also could raise IO Error
        -- Send login message
        -- Fork off socket listening thread, passing in GUI?
        -- Bind event handlers to the IO Handle
        -- return GUIEventHandlers ?
        -- close connect dialog ?
          return ()

-- TODO Time out if this takes too long
-- TODO Use IOError instead of Either? Or at least don't wrap strings in Either
handleHandshake :: Net.IncomingByteSource -> IO (Either String Net.IncomingByteSource)
handleHandshake connectionHandle =
  do
    msgType <- Msg.messageType <$> (Recv.readHeader connectionHandle)
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
