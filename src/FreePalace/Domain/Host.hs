module FreePalace.Domain.Host where

import qualified Data.Map as Map
import qualified FreePalace.Domain.Net as Net

data HostDirectory = HostDirectory (Map.Map (Net.Hostname, Net.PortId) Net.Protocol) deriving Show

protocolFor :: HostDirectory -> Net.Hostname -> Net.PortId -> Net.Protocol
protocolFor (HostDirectory hostDirectory) host port = Map.findWithDefault Net.PalaceProtocol (host, port) hostDirectory

type RoomId = Int
type RoomName = String
data RoomList = RoomList deriving Show

