module FreePalace.Handlers where

import System.IO
import qualified System.Log.Logger as Log

import Control.Applicative
import Control.Concurrent


import qualified FreePalace.Messages as Messages
import qualified FreePalace.Messages.Outbound as Outbound
import qualified FreePalace.Net.Receive as Recv
import qualified FreePalace.Net.Send as Send
import qualified FreePalace.GUI.Types as GUI
import qualified FreePalace.Net as Netcom
import qualified FreePalace.Net.Types as Net

data GUIEventHandlers = GUIEventHandlers {

}

guiEventHandlers = GUIEventHandlers {

}

handleConnectRequested :: GUI.Components -> Net.Connectors -> Net.Hostname -> Net.PortId -> IO ()
handleConnectRequested gui connectors host portString =
  do
    -- TODO If we are already connected, disconnect
    let connect = Net.connect connectors
    channels <- connect (host::Net.Hostname) (portString::Net.PortId) 
    handshake <- handleHandshake channels    -- Now in STATE_HANDSHAKING
    case handshake of
      Left msg -> return () -- TODO Provide some way to indicate connection failed. Don't close the connection window.
      Right (communicators, translators) ->  -- Now in STATE_READY 
        do
          -- TODO Allow user to set user name
          sendLogin communicators translators $ Messages.UserId { Messages.userRef = 5, Messages.userName = "Haskell Curry"}  -- TODO Also could raise IO Error
          messageListenerThreadId <- forkIO $ dispatchIncomingMessage communicators gui
          -- TODO bind GUIEventHandlers to communicators
          -- TODO Must bind disconnect and reconnect to something that will stop the forked process (Control.Concurrent -  killThread :: ThreadId -> IO () )
          GUI.closeDialog $ GUI.connectDialog gui

-- TODO Time out if this takes too long
-- TODO Use IOError instead of Either? Or at least don't wrap strings in Either
handleHandshake :: Net.Channels -> IO (Either String (Net.Communicators, Net.Translators))
handleHandshake channels =
  do
    let byteSource = Net.source channels
    let byteSink = Net.sink channels
    let bigEndianCommunicators =  Netcom.bigEndianCommunicators byteSource byteSink -- default to big endian
    msgType <- Messages.messageType <$> (Net.readHeader bigEndianCommunicators)
    Log.debugM "Incoming.Handshake" (show msgType)
    case msgType of
      Messages.BigEndianServer    ->  return $ Right (bigEndianCommunicators, Outbound.bigEndianTranslators)
      Messages.LittleEndianServer ->  return $ Right (Netcom.littleEndianCommunicators byteSource byteSink, Outbound.littleEndianTranslators)
      Messages.UnknownServer      ->  return $ Left "Unknown server type"
      _                           ->  return $ Left "Unknown server type"

sendLogin :: Net.Communicators -> Net.Translators -> Messages.UserId -> IO ()
sendLogin communicators translators userId =
  do
    let sendBytes = Net.writeBytes communicators
    let loginMessageBytes = Outbound.loginMessage translators userId
    sendBytes loginMessageBytes


-- TODO Handle connection loss
dispatchIncomingMessage :: Net.Communicators -> GUI.Components -> IO ()
dispatchIncomingMessage communicators gui =
  do
    header <- Net.readHeader communicators
    Log.debugM "Incoming.Message.Header" (show header)
    case Messages.messageType header of
      Messages.Talk -> handleTalk communicators gui header
      _ -> return ()
    dispatchIncomingMessage communicators gui

-- TODO room message if reference ID = 0
handleTalk :: Net.Communicators -> GUI.Components -> Messages.Header -> IO ()
handleTalk communicators gui header =
  do
    let userId = userIdFrom (Messages.messageRefNumber header)
    Log.debugM "Incoming.Message.Talk.User" (show userId)
    message <- (Net.readText communicators) (Messages.messageSize header)
    
    -- Chat string can't be > 254 characters long
    -- OpenPalace carries along original message but unclear why it's needed
    -- Even iptscrae shouldn't use what isn't displayed, no?
    let truncated = take 254 message
    Log.debugM "Incoming.Message.Talk.Text" (show truncated)
    
    -- TODO Handle different message types: whisper, room message? Or in a different handler?
    -- OpenPalace carries along original message; seems to be passed off to iptscrae. Should it be?
    GUI.appendMessage (GUI.logWindow gui) userId truncated

    -- TODO send talk and user (and message type) to chat balloon in GUI
    -- TODO send talk to script event handler when there is scripting
    return ()


userIdFrom :: Int -> Messages.UserId
userIdFrom refNumber =
   -- TODO Actually look up ID from some lookup table that we were sent from the server
  Messages.UserId { Messages.userRef = refNumber , Messages.userName = show refNumber }
