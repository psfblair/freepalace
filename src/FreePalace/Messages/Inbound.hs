module FreePalace.Messages.Inbound where

import Control.Applicative

import FreePalace.Net.Types as Net
import FreePalace.Messages as Messages

readTalk :: Net.Communicators -> Messages.Header -> IO String
readTalk communicators header = (Net.readText communicators) (Messages.messageSize header)

readAlternateLogonReply :: Net.Communicators -> IO (Int, Int)
readAlternateLogonReply communicators  =
  do
    let readInt = Net.readInt communicators
        readByte = Net.readByte communicators
        readText = Net.readText communicators
        readShort = Net.readShort communicators
      
    crc <- readInt
    counter <- readInt
    userNameLength <- readByte
    userName <- readText 31
    wizardPassword <- readText 32
    auxFlags <- readInt
 
    puidCounter <- readInt
    puidCrc <- readInt
 
    demoElapsed <- readInt
    totalElapsed <- readInt
    demoLimit <- readInt
    desiredRoom <- readShort
    reserved <- readText 6
    uploadRequestedProtocolVersion <- readInt
    uploadCapabilities <- readInt
    downloadCapabilities <- readInt
    upload2DEngineCapabilities <- readInt
    upload2DGraphicsCapabilities <- readInt
    upload3DEngineCapabilities <- readInt
        
    return (puidCounter, puidCrc)
			 
readServerInfo :: Net.Communicators -> Messages.Header -> IO (String, Int)
readServerInfo communicators header =
  do
    let readInt = Net.readInt communicators
        readByte = Net.readByte communicators
        readText = Net.readTextNoTerminator communicators

    permissions <- readInt
    size <- fromIntegral <$> readByte
    serverName <- readText size

    {- OpenPalace notes:
	Weird -- this message is supposed to include options and upload/download capabilities, but doesn't.
          options = socket.readUnsignedInt();
          uploadCapabilities = socket.readUnsignedInt();
          downloadCapabilities = socket.readUnsignedInt();
    -}
    
    return (serverName, permissions)

