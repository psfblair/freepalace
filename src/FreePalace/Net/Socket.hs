module FreePalace.Net.Socket where

import qualified Network.Socket as Socket
import System.IO
import Data.Char
import Control.Applicative

import FreePalace.Net


connectors :: Connectors
connectors = Connectors {
  disconnector = \(SocketByteSource socket) -> Socket.close socket,
  connector = \hostname portNum -> 
                    do
                      addrinfos <- Socket.getAddrInfo Nothing (Just hostname) (Just portNum)
                      let serveraddr = head addrinfos
                      socket <- Socket.socket (Socket.addrFamily serveraddr) Socket.Stream Socket.defaultProtocol
                      Socket.setSocketOption socket Socket.KeepAlive 1
                      Socket.connect socket (Socket.addrAddress serveraddr)
                      return $ Channels { source = SocketByteSource socket, sink = SocketByteSink socket }
}
