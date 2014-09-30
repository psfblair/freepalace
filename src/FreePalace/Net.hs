{-# LANGUAGE ExistentialQuantification #-}
module FreePalace.Net where

import qualified Network.Socket as Socket -- Wish I didn't have to do this.
import System.IO (Handle)
import Data.Word
import Data.Char
import Control.Applicative

type Hostname = String
type PortId = String

data IncomingByteSource = SocketByteSource Socket.Socket

readWord :: IncomingByteSource -> IO Int
readWord (SocketByteSource socket) = ord <$> head <$> Socket.recv socket 1

readWords :: Integral b => b -> IncomingByteSource -> IO [Int]
readWords numberOfWords (SocketByteSource socket) = (map ord) <$> Socket.recv socket (fromIntegral numberOfWords)


data Connectors = Connectors {
  connector :: Hostname -> PortId -> IO IncomingByteSource,
  disconnector :: IncomingByteSource -> IO ()
}
