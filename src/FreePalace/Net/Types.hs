module FreePalace.Net.Types where

import qualified Network.Socket as Socket (Socket) -- I wish this module didn't have to know about sockets.

type Hostname = String
type PortId   = String

-- TODO - These may be better as typeclasses and get rid of dependence of this module on sockets
data IncomingByteSource = SocketByteSource Socket.Socket
data OutgoingByteSink   = SocketByteSink   Socket.Socket

