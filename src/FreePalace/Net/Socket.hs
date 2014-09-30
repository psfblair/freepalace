module FreePalace.Net.Socket where

import qualified Network.Socket as Socket
import System.IO
import Data.Char
import Control.Applicative

import qualified FreePalace.Net as Net
  
connectors :: Net.Connectors
connectors = Net.Connectors {
  Net.disconnector = \(Net.SocketByteSource socket) -> Socket.close socket,
  Net.connector = \hostname portNum -> 
                    do
                      addrinfos <- Socket.getAddrInfo Nothing (Just hostname) (Just portNum)
                      let serveraddr = head addrinfos
                      socket <- Socket.socket (Socket.addrFamily serveraddr) Socket.Stream Socket.defaultProtocol
                      Socket.setSocketOption socket Socket.KeepAlive 1
                      Socket.connect socket (Socket.addrAddress serveraddr)
                      return $ Net.SocketByteSource socket
}


{- DIFFERENT LOGINS

OpenPalace:
  // After socket connection - onConnect sets state to STATE_HANDSHAKING. Socket data coming back in this state calls handshake():
  
    handshake():void {      
      messageID = socket.readInt();
      switch (messageID) {  // Can get UNKNOWN_SERVER, LITTLE_ENDIAN_SERVER, BIG_ENDIAN_SERVER or unexpected message.
                            // LITTLE_ENDIAN_SERVER and BIG_ENDIAN_SERVER set endianness of socket, then:
        size = socket.readInt();
        p = socket.readInt();
        logOn(size, p);
      }
    }
    
    logOn(size:int, referenceId:int):void {
      currentRoom.selfUserId = id = referenceId;
      socket.writeInt(OutgoingMessageTypes.LOGON);    // LOGON:int = 0x72656769;
      socket.writeInt(128);               // struct AuxRegistrationRec is 128 bytes
      socket.writeInt(0);                 // RefNum unused in LOGON message
      socket.writeInt(regCRC);            // Guest regCode crc =  0x5905f923 or obtained from Registration code
      socket.writeInt(regCounter);        // Guest regCode counter = 0xcf07309c or obtained from Registration code

      socket.writeByte(userName.length);  // Username has to be Windows-1252 and up to 31 characters. Limit it to 31
      socket.writeMultiByte(userName, 'Windows-1252');
      for (...) { socket.writeByte(0); }  // Pad the rest with zeroes     
  
      /* AUXFLAGS_UNKNOWN_MACHINE:uint = 0;
         AUXFLAGS_MAC68K:uint = 1;
         AUXFLAGS_MACPPC:uint = 2;
         AUXFLAGS_WIN16:uint  = 3;
         AUXFLAGS_WIN32:uint  = 4;
         AUXFLAGS_JAVA:uint   = 5; */
      socket.writeInt(AUXFLAGS_AUTHENTICATE | AUXFLAGS_WIN32);  // AUXFLAGS_AUTHENTICATE:uint = 0x80000000; AUXFLAGS_WIN32:uint = 4;
      socket.writeInt(puidCounter);        // puidCounter:uint = 0xf5dc385e;
      socket.writeInt(puidCRC);            // puidCRC:uint = 0xc144c580;
      socket.writeInt(0);                  // demoElapsed - no longer used
      socket.writeInt(0);                  // totalElapsed - no longer used
      socket.writeInt(0);                  // demoLimit - no longer used
      socket.writeShort(initialRoom);      // desired room id

      // Protocol spec lists these as reserved, and says there shouldn't
      // be anything put in them... but the server records these 6 bytes
      // in the log file.  So I'll exploit that.
      socket.writeMultiByte("OPNPAL", "iso-8859-1");
      socket.writeInt(0);                   // ulRequestedProtocolVersion -- ignored on server
      socket.writeInt(ULCAPS_ASSETS_PALACE);// ulUploadCaps - This is a lie... for now  ULCAPS_ASSETS_PALACE:uint = 0x00000001;

      // ulDownloadCaps - We have to lie about our capabilities so that servers don't reject OpenPalace as a Hacked client.
      socket.writeInt(
        DLCAPS_ASSETS_PALACE |              // DLCAPS_ASSETS_PALACE:uint = 0x00000001;
        DLCAPS_FILES_PALACE |               // This is a lie...  DLCAPS_FILES_PALACE:uint = 0x00000010;
        DLCAPS_FILES_HTTPSRVR               // DLCAPS_FILES_HTTPSRVR:uint = 0x00000100;
      );
      socket.writeInt(0);                   // ul2DEngineCaps -- Unused
      socket.writeInt(0);                   // ul2dGraphicsCaps -- Unused
      socket.writeInt(0);                   // ul3DEngineCaps -- Unused
      socket.flush();
      
      state = STATE_READY;
      connecting = false;
      dispatchEvent(new PalaceEvent(PalaceEvent.CONNECT_COMPLETE));   // Hide connect and directory window
    }
  

Java:
 this.size = 128;
 ndos.writeInt(this.regInts[0]);    //  1493563683
 ndos.writeInt(this.regInts[1]);    // -821612388
 ndos.writeByte(this.userName.length());
 ndos.writeBytes(this.userName)     // Pad to 64 characters with 0 bytes
 ndos.writeInt(this.auxFlags);      // 5
 ndos.writeInt(this.puidCtr);       // 0
 ndos.writeInt(this.puidCRC);       // 0
 ndos.writeInt(this.demoElapsed);   // 0
 ndos.writeInt(this.totalElapsed);  // 0
 ndos.writeInt(this.demoLimit);     // 0
 ndos.writeShort(this.desiredRoom); // 0
 String version = "J2.3.0";         // Max 6 characters     
 ndos.writeBytes(version);
 ndos.writeInt(0);
 ndos.writeInt(0);
 ndos.writeInt(0);
 ndos.writeInt(0);
 ndos.writeInt(0);
 ndos.writeInt(0);
 
Linpal: 
  RLogOn(int a, int b){
    // = a; a is validation
    id = b;
    WriteInt(1919248233);   // 0x72656769
    WriteInt(128);
    WriteInt(id); // client id/room number?
    WriteInt(0x5905f923); // b[0] ?
    WriteInt(0xcf07309c); // b[1] ?
  
    WriteByte(strlen(name));
    WriteBytes((unsigned char *)name, strlen(name)); // Again padded to 64 chars. name  or super.a?

    // WriteInt(5);  // 5 - this is commented out in favor of the hex below. 
    WriteInt(0x80000004);
  
    // WriteInt(0);  //  This is commented out in favor of the hex below. unset or d?
    WriteInt(0xf5dc385e);
    
    //WriteInt(0); // e?
    WriteInt(0xc144c580);
    
    //WriteInt(0); // f?
    WriteInt(0x00002a30);
    
    //WriteInt(0); // g?
    WriteInt(0x00021df9);
    
    //WriteInt(0); // h?
    WriteInt(0x00002a30);
          
    //WriteShort(1); // i? room id?
    WriteShort(0); // i? room id?
  
    /*  // version 
    WriteBytes((unsigned char *)"J2.0", 4); 
    WriteByte(0);
    WriteByte(0); */
    WriteBytes((unsigned char *)"350211", 6);
    
    WriteInt(0);

    //WriteInt(0);
    WriteInt(1);

    //WriteInt(0);
    WriteInt(0x00000111);

    //WriteInt(0);
    WriteInt(1);

    //WriteInt(0);
    WriteInt(1);

    WriteInt(0);
  }
  
  
  
-}