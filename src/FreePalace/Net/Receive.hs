module FreePalace.Net.Receive where

import FreePalace.Net
import FreePalace.Net.Messages

import Control.Applicative

readMessageType :: IncomingByteSource -> IO MessageType
readMessageType byteSource = idToMessageType <$> readWord byteSource

readHeader :: IncomingByteSource -> IO Header
readHeader byteSource = 
  do
    msgType <- readMessageType byteSource
    size <- readWord byteSource
    referenceNumber <- readWord byteSource
    return Header {
      messageType = msgType,
      messageSize = size,
      messageRefNumber = referenceNumber
    }
    
readMessageBody :: IncomingByteSource -> Header -> (Header -> [Int] -> MessageType) -> IO MessageType
readMessageBody byteSource header messageConstructor =
  do
    let length = messageSize header
    messageBytes <- readWords length byteSource
    return (messageConstructor header messageBytes)
