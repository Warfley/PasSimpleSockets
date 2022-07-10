# PasSimpleSockets
Simple socket API wrapper for FreePascal.
Provides access to the socket API in a less C-ish way.

### Features:
Provides the base functionality of the berkley socket API in a more modern manner. Also transparently handles IP-address conversion and IPv6/IPv4 dual stack.

Functions (Delphi Notation):
```pascal
function TCPSocket(AType: TSocketType): TSocket; 
function UDPSocket(AType: TSocketType): TSocket; 

procedure CloseSocket(const ASocket: TSocket); 

procedure Bind(const ASocket: TSocket; const AAddress: TNetworkAddress; APort: Word);

procedure Listen(const ASocket: TSocket; Backlog: Integer); 
function AcceptConnection(const ASocket: TSocket): TSocketConnection; 

procedure Connect(const ASocket: TSocket; const AAddress: TNetworkAddress; APort: Word); 

function Receive(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): SizeInt; 
function ReceiveFrom(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): TReceiveFromResult;
function Send(const ASocket: TSocket; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt; 
function SendTo(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress;
                  ReceiverPort: Word; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt; 

function ReceiveStr(const ASocket: TSocket; MaxLength: SizeInt; AFlags: Integer = 0): String; 
function ReceiveStrFrom(const ASocket: TSocket; MaxLength: SizeInt = MaxUDPPackageSize; AFlags: Integer = 0): TReceiveFromStringMessage; 
function SendStr(const ASocket: TSocket; const AData: String; AFlags: Integer = 0): SizeInt; 
function SendStrTo(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: String; AFlags: Integer = 0): SizeInt; 
 
function Receive<T>(const ASocket: TSocket; AFlags: Integer = 0): T; 
function ReceiveFrom<T>(const ASocket: TSocket; AFlags: Integer = 0): TReceiveFromMessage<T>; 
function Send<T>(const ASocket: TSocket; constref AData: T; AFlags: Integer = 0): SizeInt; 
function SendTo<T>(const ASocket: TSocket; constref ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: T; AFlags: Integer = 0): SizeInt; 

function ReceiveArray<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): TArray<T>; 
function ReceiveArrayFrom<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): TReceiveFromMessage<TArray<T>>; 
function SendArray<T>(const ASocket: TSocket; const AData: TArray<T>; AFlags: Integer = 0): SizeInt; 
function SendArrayTo<T>(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: TArray<T>; AFlags: Integer = 0): SizeInt; 

// Timeout in MS
function DataAvailable(const SocketArray: TSocketArray; TimeOut: Integer = 0): TSocketArray; 
function DataAvailable(const ASocket: TSocket; TimeOut: Integer = 0): Boolean; 
function DataAvailable(const SocketArray: array of TSocket; TimeOut: Integer = 0): TSocketArray;  
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
    Msg := ReceiveStrFrom(Sock);
    WriteLn('Received from Client at ', Msg.FromAddr.Address, ':', Msg.FromPort,' message: ', Msg.Data);
    SendStrTo(Sock, Msg.FromAddr, Msg.FromPort, Msg.Data);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.
```

### DualStack
Sockets created with type `stDualStack` will support both IPv4 and IPv6 connections. The wrapper will transparently handle this and will convert IPv6 mapped IPv4 addresses to the common IPv4 notation
