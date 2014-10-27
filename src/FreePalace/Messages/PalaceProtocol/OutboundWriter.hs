module FreePalace.Messages.PalaceProtocol.OutboundWriter where

import FreePalace.Net as Net
import FreePalace.Domain as Domain
import FreePalace.Net.Send as Send
import FreePalace.Messages.PalaceProtocol.Outbound as PalaceOutbound

sendLogin :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Domain.UserId -> IO ()
sendLogin Net.PalaceConnection { Net.palaceByteSink = byteSink } messageConverters userId =
  do
    let intsToBuilder     = Send.toIntByteStringBuilder   $ Net.palaceIntWriter   messageConverters
        shortsToBuilder   = Send.toShortByteStringBuilder $ Net.palaceShortWriter messageConverters
        loginMessageBytes = PalaceOutbound.loginMessage intsToBuilder shortsToBuilder userId
    Send.writeBytesToSink byteSink loginMessageBytes

-- TODO The handler has to see if 1) it's a client command, 2) it's a script call, 3) a user is selected (for whisper)
speak :: Net.PalaceConnection -> Net.PalaceMessageConverters -> Domain.Communication -> IO ()
speak Net.PalaceConnection { Net.palaceByteSink = byteSink } messageConverters communication =
  do
    let intsToBuilder     = Send.toIntByteStringBuilder   $ Net.palaceIntWriter   messageConverters
        shortsToBuilder   = Send.toShortByteStringBuilder $ Net.palaceShortWriter messageConverters

        chatMessageBytes = PalaceOutbound.chatMessage intsToBuilder shortsToBuilder communication
    Send.writeBytesToSink byteSink chatMessageBytes
