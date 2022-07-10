unit SimpleSockets;

{$mode ObjFPC}{$H+}
{$TypedAddress on}

interface

uses
  SysUtils, Sockets;

type

  { Basic Socket Types }

  TSocketFD = Tsocket;
  TSocketType = (stIPv4, stIPv6, stDualStack);
  TSocket = record
    FD: TSocketFD;
    SocketType: TSocketType;
  end;

  TSocketArray = Array of TSocket;

  TAddressType = (atIN4, atIN6);
  TNetworkAddress = record
    Address: String;
    AddressType: TAddressType;
  end;

  TSocketConnection = record
    ClientAddress: TNetworkAddress;
    ClientPort: Word;
    Socket: TSocket;
  end;

  { ReceiveFrom Return Types }

  TReceiveFromResult = record
    FromAddr: TNetworkAddress;
    FromPort: Word;
    DataSize: SizeInt;
  end;

  generic TReceiveFromMessage<T> = record
    FromAddr: TNetworkAddress;
    FromPort: Word;
    Data: T;
  end;

  TReceiveFromStringMessage = specialize TReceiveFromMessage<String>;

  { Exceptions }

  EDualStackNotSupported = class(Exception);
  EUnsupportedAddress = class(Exception);

  { ESocketError }

  ESocketError = class(Exception)
  private
    FCode: Integer;
  public
    constructor Create(ACode: Integer; const FunName: String);

    property Code: Integer read FCode;
  end;

  EConnectionClosedException = class(Exception);
  EUDPFragmentationException = class(Exception);

const
  MaxUDPPackageSize = 512;

  { Address Management }

function IN4Address(const Address: String): TNetworkAddress; inline;
function IN6Address(const Address: String): TNetworkAddress; inline;
function IN4MappedIN6Address(const In4Address: String): TNetworkAddress; inline;
function INAddr(const Address: String): TNetworkAddress; inline;

function IsIPv4Mapped(const IPv6Addr: TNetworkAddress): Boolean; inline;
function ExtractIPv4Address(const IPv6Addr: TNetworkAddress): TNetworkAddress; inline;

function IN6Equal(const A, B: String): Boolean;
operator =(const A, B: TNetworkAddress): Boolean; inline;
operator :=(const AStr: String): TNetworkAddress; inline;

  { Socket Functions }

function TCPSocket(AType: TSocketType): TSocket; inline;
function UDPSocket(AType: TSocketType): TSocket; inline;

procedure CloseSocket(const ASocket: TSocket); inline;

procedure Bind(const ASocket: TSocket; const AAddress: TNetworkAddress; APort: Word);

procedure Listen(const ASocket: TSocket; Backlog: Integer); inline;
function AcceptConnection(const ASocket: TSocket): TSocketConnection; inline;

procedure Connect(const ASocket: TSocket; const AAddress: TNetworkAddress; APort: Word); inline;

function Receive(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): SizeInt; inline;
function ReceiveFrom(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt; AFlags: Integer = 0): TReceiveFromResult;
function Send(const ASocket: TSocket; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt; inline;
function SendTo(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress;
                  ReceiverPort: Word; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer = 0): SizeInt; inline;

function ReceiveStr(const ASocket: TSocket; MaxLength: SizeInt; AFlags: Integer = 0): String; inline;
function ReceiveStrFrom(const ASocket: TSocket; MaxLength: SizeInt = MaxUDPPackageSize; AFlags: Integer = 0): TReceiveFromStringMessage; inline;
function SendStr(const ASocket: TSocket; const AData: String; AFlags: Integer = 0): SizeInt; inline;
function SendStrTo(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: String; AFlags: Integer = 0): SizeInt; inline;
 
generic function Receive<T>(const ASocket: TSocket; AFlags: Integer = 0): T; inline;
generic function ReceiveFrom<T>(const ASocket: TSocket; AFlags: Integer = 0): specialize TReceiveFromMessage<T>; inline;
generic function Send<T>(const ASocket: TSocket; constref AData: T; AFlags: Integer = 0): SizeInt; inline;
generic function SendTo<T>(const ASocket: TSocket; constref ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: T; AFlags: Integer = 0): SizeInt; inline;

generic function ReceiveArray<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): specialize TArray<T>; inline;
generic function ReceiveArrayFrom<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): specialize TReceiveFromMessage<specialize TArray<T>>; inline;
generic function SendArray<T>(const ASocket: TSocket; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt; inline;
generic function SendArrayTo<T>(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt; inline;

// Timeout in MS
function DataAvailable(const SocketArray: TSocketArray; TimeOut: Integer = 0): TSocketArray; overload;
function DataAvailable(const ASocket: TSocket; TimeOut: Integer = 0): Boolean; overload; //inline;
function DataAvailable(const SocketArray: array of TSocket; TimeOut: Integer = 0): TSocketArray; overload; inline;

{ Helper }
// Must be in interface because of generic functions

type
  _PAddressUnion = ^_TAddressUnion;
  _TAddressUnion = record
  case Boolean of
  True: (In4Addr: sockaddr_in);
  False: (In6Addr: sockaddr_in6);
  end;

function CreateAddr(AAddress: TNetworkAddress; APort: Word; DualStack: Boolean): _TAddressUnion; inline;
procedure ReadAddr(constref Addr: _TAddressUnion; DualStack: Boolean; out AAddress: TNetworkAddress; out APort: Word);
implementation

uses
  {$IfDef WINDOWS}WinSock2{$Else}BaseUnix{$EndIf}, Math;

const
  IPPROTO_IPV6 = {$IfDef WINDOWS}41{$Else}41{$EndIf};
  IPV6_V6ONLY = {$IfDef WINDOWS}27{$Else}26{$EndIf};

function CreateAddr(AAddress: TNetworkAddress; APort: Word; DualStack: Boolean): _TAddressUnion;
begin
  if (AAddress.AddressType = atIN4) and DualStack then
    AAddress := IN4MappedIN6Address(AAddress.Address);
  if AAddress.AddressType = atIN4 then
  begin
    Result.In4Addr.sin_family := AF_INET;
    Result.In4Addr.sin_port := HToNS(APort);
    Result.In4Addr.sin_addr.s_addr := LongWord(StrToNetAddr(AAddress.Address));
  end
  else if AAddress.AddressType = atIN6 then
  begin
    Result.In6Addr.sin6_family := AF_INET6;
    Result.In6Addr.sin6_port := HToNS(APort);
    Result.In6Addr.sin6_addr := StrToHostAddr6(AAddress.Address);
    Result.In6Addr.sin6_flowinfo := 0;
    Result.In6Addr.sin6_scope_id := 0;
  end
  else
    raise EUnsupportedAddress.Create('Address type ' + ord(AAddress.AddressType).ToString + ' not supported');
end;

procedure ReadAddr(constref Addr: _TAddressUnion; DualStack: Boolean; out
  AAddress: TNetworkAddress; out APort: Word);
begin
  if Addr.In4Addr.sin_family = AF_INET then
  begin
    AAddress := IN4Address(NetAddrToStr(Addr.In4Addr.sin_addr));
    APort := NToHs(Addr.In4Addr.sin_port);
  end
  else if Addr.In6Addr.sin6_family = AF_INET6 then
  begin
    AAddress := IN6Address(HostAddrToStr6(Addr.In6Addr.sin6_addr));
    if DualStack and IsIPv4Mapped(AAddress.Address) then
      AAddress := ExtractIPv4Address(AAddress);
    APort := NToHs(Addr.In6Addr.sin6_port);
  end
  else
    raise EUnsupportedAddress.Create('Address Family ' + Addr.In4Addr.sin_family.ToString + ' not supported');
end;

function SocketInvalid(ASocket: TSocketFD): Boolean; inline;
begin
  {$IfDef Windows}
  Result := ASocket = TSocketFD(INVALID_SOCKET);
  {$Else}
  Result := ASocket < 0;
  {$EndIf}
end;

function CreateSocketFD(ADomain: TSocketType; AType: Integer; AProto: Integer): TSocketFD;
var
  AFam, v6Only: Integer;
begin
  if ADomain = stIPv4 then
    AFam := AF_INET
  else
    AFam := AF_INET6;
  Result := fpsocket(AFam, AType, AProto);
  if SocketInvalid(Result) then
    raise ESocketError.Create(socketerror, 'socket');

  if ADomain = stDualStack then
  begin
    v6Only := 0;
    if fpsetsockopt(Result, IPPROTO_IPV6, IPV6_V6ONLY, @v6Only, SizeOf(v6Only)) <> 0 then
      raise EDualStackNotSupported.Create('Dualstack option not supported on this system: ' + socketerror.ToString);
  end;
end;

function IN4Address(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atIN4;
end;

function IN6Address(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
 Result.Address := Address;
 Result.AddressType := atIN6;
end;

function IN4MappedIN6Address(const In4Address: String): TNetworkAddress;
var
  InAddr: TIn_addr;
begin
  InAddr := StrToNetAddr(In4Address);
  Result := IN6Address('::FFFF:%x:%x'.Format([(InAddr.s_bytes[1] shl 8) or InAddr.s_bytes[2],
                                              (InAddr.s_bytes[3] shl 8) or InAddr.s_bytes[4]]));
end;

function INAddr(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
  if Pos(':', Address) = 0 then
    Result.AddressType := atIN4
  else
    Result.AddressType := atIN6;
  Result.Address := Address;
end;

function IsIPv4Mapped(const IPv6Addr: TNetworkAddress): Boolean;
var
  In6Addr: Sockets.TIn6Addr;
begin
  if IPv6Addr.AddressType = atIN4 then
    Exit(True);
  if IPv6Addr.AddressType  <> atIN6 then
    raise EUnsupportedAddress.Create('Can only check IPv4 mapping for IPv6 addresses');
  IN6Addr := StrToHostAddr6(IPv6Addr.Address);
  Result := (IN6Addr.u6_addr16[0] = 0) and
            (IN6Addr.u6_addr16[1] = 0) and
            (IN6Addr.u6_addr16[2] = 0) and
            (IN6Addr.u6_addr16[3] = 0) and
            (IN6Addr.u6_addr16[4] = 0) and
            (IN6Addr.u6_addr16[5] = $FFFF);
end;

function ExtractIPv4Address(const IPv6Addr: TNetworkAddress): TNetworkAddress;
var
  In6Addr: Sockets.TIn6Addr;
begin
  if IPv6Addr.AddressType = atIN4 then
    Exit(IPv6Addr);
  if IPv6Addr.AddressType  <> atIN6 then
    raise EUnsupportedAddress.Create('Can only extract IPv4 mapping from IPv6 addresses');
  IN6Addr := StrToHostAddr6(IPv6Addr.Address);
  Result := IN4Address('%d.%d.%d.%d'.Format([IN6Addr.s6_addr8[12],
                                             IN6Addr.s6_addr8[13],
                                             IN6Addr.s6_addr8[14],
                                             IN6Addr.s6_addr8[15]]));
end;

function IN6Equal(const A, B: String): Boolean;
var
  AAddr, BAddr: Sockets.Tin6_addr;
begin
  AAddr := StrToHostAddr6(A);
  BAddr := StrToHostAddr6(B);
  Result := (AAddr.s6_addr32[0] = BAddr.s6_addr32[0]) and
            (AAddr.s6_addr32[1] = BAddr.s6_addr32[1]) and
            (AAddr.s6_addr32[2] = BAddr.s6_addr32[2]) and
            (AAddr.s6_addr32[3] = BAddr.s6_addr32[3]);
end;

operator=(const A, B: TNetworkAddress): Boolean;
begin
  Result := (A.AddressType = B.AddressType) and (
              ((A.AddressType = atIN4) and (A.Address = B.Address)) or // IPv4: simple string equality
              ((A.AddressType = atIN6) and IN6Equal(A.Address, B.Address)) // IPv6 check binary equality
            );
end;

operator:=(const AStr: String): TNetworkAddress;
begin
  Result := INAddr(AStr);
end;

function TCPSocket(AType: TSocketType): TSocket;
begin
  Result.SocketType := AType;
  Result.FD := CreateSocketFD(AType, SOCK_STREAM, 0);
end;

function UDPSocket(AType: TSocketType): TSocket;
begin
  Result.SocketType := AType;
  Result.FD := CreateSocketFD(AType, SOCK_DGRAM, 0);
end;

procedure CloseSocket(const ASocket: TSocket);
begin
  Sockets.CloseSocket(ASocket.FD);
end;

procedure Bind(const ASocket: TSocket; const AAddress: TNetworkAddress;
  APort: Word);
var
  addr: _TAddressUnion;
begin
  addr := CreateAddr(AAddress, APort, ASocket.SocketType = stDualStack);
  if fpbind(ASocket.FD, Sockets.PSockAddr(@addr), SizeOf(addr)) <> 0 then raise
    ESocketError.Create(socketerror, 'bind (%s:%d)'.Format([AAddress.Address, APort]));
end;

procedure Listen(const ASocket: TSocket; Backlog: Integer);
begin
  if fplisten(ASocket.FD, Backlog) <> 0 then raise
    ESocketError.Create(socketerror, 'listen');
end;

function AcceptConnection(const ASocket: TSocket): TSocketConnection;
var
  addr: _TAddressUnion;
  addrLen: SizeInt = SizeOf(addr);
begin
  Result.Socket.FD := fpaccept(ASocket.FD, Sockets.psockaddr(@addr), @addrLen);
  if SocketInvalid(Result.Socket.FD) then
    raise ESocketError.Create(socketerror, 'accept');
  Result.Socket.SocketType := ASocket.SocketType;
  ReadAddr(addr, ASocket.SocketType = stDualStack, Result.ClientAddress, Result.ClientPort);
end;

procedure Connect(const ASocket: TSocket;
  const AAddress: TNetworkAddress; APort: Word);
var
  addr: _TAddressUnion;
begin
  addr := CreateAddr(AAddress, APort, ASocket.SocketType = stDualStack);
  if fpconnect(ASocket.FD, Sockets.psockaddr(@addr), SizeOf(addr)) <> 0 then
    raise ESocketError.Create(socketerror, 'connect');
end;

function Receive(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt;
  AFlags: Integer): SizeInt;
begin
  Result := fprecv(ASocket.FD, ABuffer, MaxSize, AFlags);
  if Result = 0 then
    raise EConnectionClosedException.Create('The connection closed')
  else if Result < 0 then
    raise ESocketError.Create(socketerror, 'recv');
end;

function ReceiveFrom(const ASocket: TSocket; ABuffer: Pointer; MaxSize: SizeInt;
  AFlags: Integer): TReceiveFromResult;
var
  addr: _TAddressUnion;
  addrLen: SizeInt;
begin
  addrLen := SizeOf(_TAddressUnion);
  Result.DataSize := fprecvfrom(ASocket.FD, ABuffer, MaxSize, AFlags, Sockets.PSockAddr(@addr), @addrLen);
  if Result.DataSize < 0 then
    raise ESocketError.Create(socketerror, 'recvfrom');
  ReadAddr(addr, ASocket.SocketType = stDualStack, Result.FromAddr, Result.FromPort);
end;

function Send(const ASocket: TSocket; ABuffer: Pointer; ASize: SizeInt;
  AFlags: Integer): SizeInt;
begin
  Result := fpsend(ASocket.FD, ABuffer, ASize, AFlags);
  if Result < 0 then
    raise ESocketError.Create(socketerror, 'send');
end;

function SendTo(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress;
  ReceiverPort: Word; ABuffer: Pointer; ASize: SizeInt; AFlags: Integer
  ): SizeInt;
var
  addr: _TAddressUnion;
begin
  addr := CreateAddr(ReceiverAddr, ReceiverPort, ASocket.SocketType = stDualStack);
  Result := fpsendto(ASocket.FD, ABuffer, ASize, AFlags, Sockets.psockaddr(@addr), SizeOf(addr));
  if Result < 0 then
    raise ESocketError.Create(socketerror, 'sendto');
end;

function ReceiveStr(const ASocket: TSocket; MaxLength: SizeInt;
  AFlags: Integer): String;
var
  Len: SizeInt;
begin
  Result := '';
  SetLength(Result, MaxLength);
  Len := Receive(ASocket, @Result[1], MaxLength, AFlags);
  SetLength(Result, Len);
end;

function ReceiveStrFrom(const ASocket: TSocket; MaxLength: SizeInt;
  AFlags: Integer): TReceiveFromStringMessage;
var
  UdpMessage: TReceiveFromResult;
begin
  Result := Default(specialize TReceiveFromMessage<String>);
  SetLength(Result.Data, MaxLength);
  UdpMessage := ReceiveFrom(ASocket, @Result.Data[1], MaxLength, AFlags);
  SetLength(Result.Data, UdpMessage.DataSize);
  Result.FromAddr := UdpMessage.FromAddr;
  Result.FromPort := UdpMessage.FromPort;
end;

function SendStr(const ASocket: TSocket; const AData: String; AFlags: Integer
  ): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := Send(ASocket, @AData[1], Length(AData), AFlags);
end;

function SendStrTo(const ASocket: TSocket;
  const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: String; AFlags: Integer
  ): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := SendTo(ASocket, ReceiverAddr, ReceiverPort, @AData[1], Length(AData), AFlags);
end;

generic function Receive<T>(const ASocket: TSocket; AFlags: Integer = 0): T;
var
  Len: SizeInt;
begin
  Result := Default(T);
  Len := 0;
  while Len < SizeOf(Result) do
    Len += Receive(ASocket, @PByte(@Result)[Len], SizeOf(Result) - Len, AFlags);
end;

generic function ReceiveFrom<T>(const ASocket: TSocket; AFlags: Integer = 0): specialize TReceiveFromMessage<T>;
var
  UdpMessage: TReceiveFromResult;
begin
  Result := Default(T);
  UdpMessage := ReceiveFrom(ASocket, @Result, SizeOf(Result), AFlags);
  if UdpMessage.DataSize < SizeOf(T) then
    raise EUDPFragmentationException.Create('Receiving of fragmented data is not supported by typed receive');
  Result.FromAddr := UdpMessage.FromAddr;
  Result.FromPort := UdpMessage.FromPort;
end;

generic function Send<T>(const ASocket: TSocket; constref AData: T; AFlags: Integer = 0): SizeInt;
begin
  Result := Send(ASocket, @AData, SizeOf(T), AFlags);
end;

generic function SendTo<T>(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; constref AData: T; AFlags: Integer = 0): SizeInt;
begin
  Result := SendTo(ASocket, ReceiverAddr, ReceiverPort, @AData, SizeOf(T), AFlags);
end;

generic function ReceiveArray<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): specialize TArray<T>;
var
  Len: SizeInt;
begin     
  Result := nil;
  SetLength(Result, MaxCount);
  Len := 0;
  repeat
    Len += Receive(ASocket, @PByte(@Result)[Len], MaxCount * SizeOf(T) - Len, AFlags);
  until (Len mod SizeOf(T)) = 0;
  SetLength(Result, Len div SizeOf(T));
end;

generic function ReceiveArrayFrom<T>(const ASocket: TSocket; MaxCount: SizeInt; AFlags: Integer = 0): specialize TReceiveFromMessage<specialize TArray<T>>;
var
  UdpMessage: TReceiveFromResult;
begin
  Result.Data := nil;
  SetLength(Result.Data, MaxCount);
  UdpMessage := ReceiveFrom(ASocket, @Result.Data[0], MaxCount * SizeOf(T), AFlags);
  if UdpMessage.DataSize mod SizeOf(T) > 0 then
    raise EUDPFragmentationException.Create('Receiving of fragmented data is not supported by typed receive');
  SetLength(Result.Data, UdpMessage.DataSize div SizeOf(T)); 
  Result.FromAddr := UdpMessage.FromAddr;
  Result.FromPort := UdpMessage.FromPort;
end;

generic function SendArray<T>(const ASocket: TSocket; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := Send(ASocket, @AData[0], Length(AData) * SizeOf(T), AFlags);
end;

generic function SendArrayTo<T>(const ASocket: TSocket; const ReceiverAddr: TNetworkAddress; ReceiverPort: Word; const AData: specialize TArray<T>; AFlags: Integer = 0): SizeInt;
begin
  if Length(AData) = 0 then Exit(0);
  Result := SendTo(ASocket, ReceiverAddr, ReceiverPort, @AData[0], Length(AData) * SizeOf(T), AFlags);
end;

function DataAvailable(const SocketArray: TSocketArray; TimeOut: Integer
  ): TSocketArray;
var
  FDSet: TFDSet;
  MaxSock: TSocketFD;
  timeval: TTimeVal;
  Ret: LongInt;
  i, WriteHead: Integer;
begin
  Result := nil;
  MaxSock := 0;
  {$IfDef UNIX}fp{$endif}FD_ZERO(FDSet);
  for i:=0 to Length(SocketArray) - 1 do
  begin
    MaxSock := Max(MaxSock, SocketArray[i].FD);
    {$IfDef UNIX}fp{$endif}FD_SET(SocketArray[i].FD, FDSet);
  end;
  timeval.tv_sec := TimeOut div 1000;
  timeval.tv_usec := (TimeOut mod 1000) * 1000;
  Ret := {$IfDef UNIX}fp{$endif}select(MaxSock + 1, @FDSet, nil, nil, @timeval);
  if Ret < 0 then
    raise ESocketError.Create(socketerror, 'select');

  SetLength(Result, Ret);
  WriteHead := 0;
  for i:=0 to Length(SocketArray) - 1 do
    if FD_ISSET(SocketArray[i].FD, FDSet) then
    begin
      Result[WriteHead] := SocketArray[i];
      Inc(WriteHead);
    end;
end;

function DataAvailable(const ASocket: TSocket; TimeOut: Integer): Boolean;
var
  Arr: TSocketArray;
begin
  Arr := [ASocket];
  Result := Length(DataAvailable(Arr, TimeOut)) > 0;
end;

function DataAvailable(const SocketArray: array of TSocket; TimeOut: Integer
  ): TSocketArray;
var
  Arr: TSocketArray;
begin
  if Length(SocketArray) = 0 then Exit(nil);
  SetLength(Arr, Length(SocketArray));
  Move(SocketArray[0], Arr[0], Length(SocketArray) * SizeOf(SocketArray[0]));
  Result := DataAvailable(arr, TimeOut);
end;

{ ESocketError }

constructor ESocketError.Create(ACode: Integer; const FunName: String);
begin
  inherited CreateFmt('[Socket Error: %d] %s call failed',  [ACode, FunName]);
  FCode := ACode;
end;

end.

