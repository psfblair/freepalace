module FreePalace.Net.Types where

import qualified Network.Socket as Socket (Socket) -- I wish this module didn't have to know about sockets.

type Hostname = String
type PortId   = String
type URL      = String

-- TODO - These may be better as typeclasses and get rid of dependence of this module on sockets
data IncomingByteSource = SocketByteSource Socket.Socket
data OutgoingByteSink   = SocketByteSink   Socket.Socket

