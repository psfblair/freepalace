module FreePalace.Handlers where

import System.IO
import Control.Applicative
import qualified FreePalace.Net as Net
import qualified FreePalace.Net.Messages as Msg
import qualified FreePalace.Net.Receive as Recv
import qualified FreePalace.Net.Send as Send
import qualified System.Log.Logger as Log

type ConnectRequestHandler = String -> String -> IO ()

handleConnectRequested :: Net.Connectors -> ConnectRequestHandler
handleConnectRequested connectors host portString =
  do
    let connector = Net.connector connectors
    channels <- connector (host::Net.Hostname) (portString::Net.PortId) 
    handshake <- handleHandshake channels    -- Now in STATE_HANDSHAKING
    case handshake of
      Left msg -> return () -- TODO Provide some way to indicate connection failed. Don't close the connection window.
      Right communicators ->  -- Now in STATE_READY 
        do
          -- TODO Allow user to set user name
          Send.sendLogin communicators $ Msg.UserId { Msg.userName = "Haskell Curry"}  -- TODO Also could raise IO Error
        -- Fork off socket listening thread, passing in GUI?
        -- Bind event handlers to the IO Handle
        -- return GUIEventHandlers ?
        -- close connect dialog ?
          return ()

-- TODO Time out if this takes too long
-- TODO Use IOError instead of Either? Or at least don't wrap strings in Either
handleHandshake :: Net.Channels -> IO (Either String Net.Communicators)
handleHandshake channels =
  do
    let byteSource = Net.source channels
    let byteSink = Net.sink channels
    let bigEndianCommunicators =  Net.bigEndianCommunicators byteSource byteSink
    msgType <- Msg.messageType <$> Recv.readHeader bigEndianCommunicators -- default to big endian
    Log.debugM "Incoming.Handshake" (show msgType)
    case msgType of
      Msg.BigEndianServer    ->  return $ Right bigEndianCommunicators
      Msg.LittleEndianServer ->  return $ Right (Net.littleEndianCommunicators byteSource byteSink)
      Msg.UnknownServer      ->  return $ Left "Unknown server type"
      _                      ->  return $ Left "Unknown server type"

data GUIEventHandlers = GUIEventHandlers {
  -- connectOkHandler :: String -> String -> IO Handle
}

-- Do we need this? Somehow we need to read data from the socket and dispatch, and also write to it
-- but neither should block the other.
data IncomingMessageHandlers = IncomingMessageHandlers {

}
