# PasSimpleSockets
Simple socket API wrapper for FreePascal.
Provides access to the socket API in a less C-ish way.

### Features:
Provides the base functionality of the berkley socket API in a more modern manner. Also transparently handles IP-address conversion and IPv6/IPv4 dual stack.

Functions (Delphi Notation):
```pascal
function TCPSocket(AType: TSocketType): TSocket; // Creates a new TCP socket for either IPv4, IPv6 or DualStack
function UDPSocket(AType: TSocketType): TSocket; // Creates a new UDP socket for either IPv4, IPv6 or DualStack

procedure CloseSocket(const ASocket: TSocket); // Closes a socket

procedure Bind(const ASocket: TSocket; const AAddress: TNetworkAddress; APort: Word); // Binds socket to IP/Port for usage as server
procedure TCPServerListen(const ASocket: TSocket; Backlog: Integer); // TCP server starts listening for connections

// Generic Receive and Sending of buffers
function TCPReceive(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): SizeInt;
function UDPReceive(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): TUDPResult;
function TCPSend(const ASocket: TSocket; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt;
function UDPSend(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress;
                  ReceiverPort: Word; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt;

// Receive and Sending of Strings
function TCPReceiveStr(const ASocket: TSocket; MaxLength: SizeInt; AFlags: Integer = 0): String; inline;
function UDPReceiveStr(const ASocket: TSocket; MaxLength: SizeInt = MaxUDPPackageSize; AFlags: Integer = 0): TUDPStringMessage; inline;
function TCPSendStr(const ASocket: TSocket; const AData: String; AFlags: Integer = 0): SizeInt; inline;
function UDPSendStr(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: String; AFlags: Integer = 0): SizeInt; inline;
 
 // Receive and sending of structured data
function TCPReceive<T>(const ASocket: TSocket; AFlags: Integer = 0): T; 
function UDPReceive<T>(const ASocket: TSocket; AFlags: Integer = 0): TUDPDataMessage<T>; 
function TCPSend<T>(const ASocket: TSocket; constref AData: T; AFlags: Integer = 0): SizeInt; 
function UDPSend<T>(const ASocket: TSocket; constref ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: T; AFlags: Integer = 0): SizeInt;

// Receive and sending of Arrays of structured data
function TCPReceiveArray<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): TArray<T>;
function UDPReceiveArray<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): TUDPDataMessage<TArray<T>>;
function TCPSendArray<T>(const ASocket: TSocket; const AData: TArray<T>; AFlags: Integer = 0): SizeInt;
function UDPSendArray<T>(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: TArray<T>; AFlags: Integer = 0): SizeInt;

function DataPending(const ASocket: TSocket; TimeOut: Integer = 0): Boolean; // Check if data is available, wait for up to Timeout ms
```

### Example
```pascal
program example;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

var
  Sock: TSocket;
  Msg: TUDPStringMessage;
begin
  Sock := UDPSocket(stDualStack);
  try
    Bind(Sock, '127.0.0.1', 1337);
    // Simple echo server: Receive message and answer with same message
    Msg := UDPReceiveStr(Sock);
    WriteLn('Received from Client at ', Msg.FromAddr.Address, ':', Msg.FromPort,' message: ', Msg.Data);
    UDPSendStr(Sock, Msg.FromAddr, Msg.FromPort, Msg.Data);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.
```

### DualStack
Sockets created with type `stDualStack` will support both IPv4 and IPv6 connections. The wrapper will transparently handle this and will convert IPv6 mapped IPv4 addresses to the common IPv4 notation
