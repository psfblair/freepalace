module FreePalace.Net.Receive where

import FreePalace.Net
import FreePalace.Net.Messages

import Control.Applicative

readHeader :: Communicators -> IO Header
readHeader communicators = 
  do
    let readNextInt = readInt communicators
    msgType <- idToMessageType <$> readNextInt
    size <- readNextInt
    referenceNumber <- readNextInt
    return Header {
      messageType = msgType,
      messageSize = size,
      messageRefNumber = referenceNumber
    }
    
readMessageBody :: Communicators -> Header -> (Header -> [Int] -> MessageType) -> IO MessageType
readMessageBody communicators header messageConstructor =
  do
    let length = messageSize header
    messageBytes <- readInts communicators $ length 
    return (messageConstructor header messageBytes)
