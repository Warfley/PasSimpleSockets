program tcptest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fpsockets, ctypes;


const
{$if defined(win32)}
  LibName = 'msvcrt';
{$elseif defined(win64)}
  LibName = 'msvcrt';
{$elseif defined(wince)}
  LibName = 'coredll';
{$elseif defined(netware)}
  LibName = 'clib';
{$elseif defined(netwlibc)}
  LibName = 'libc';
{$elseif defined(macos)}
  LibName = 'StdCLib';
{$elseif defined(beos)}
  LibName = 'root';
{$else}
  LibName = 'c';
{$endif}

procedure CExit(status: cint); cdecl; external LibName name 'exit';

const
  HelloStr = 'Hello Server';
  ReplyStr = 'Hello Client!';

var ClientError, ServerError: String;

procedure IPv4TestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := ReceiveStr(Conn.Socket);
        sleep(500);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv4TestClient;
var
  sock: TFPSocket;
  Received: String;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      SendStr(sock, HelloStr);
      Received := ReceiveStr(sock, 16);
      if Received <> ReplyStr then
        ClientError := 'Unexpected response: ' + Received;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv6TestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv6);
    try
      Bind(sock, '::0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := ReceiveStr(Conn.Socket);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure IPv6TestClient;
var
  sock: TFPSocket;
  Received: String;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv6);
    try
      Connect(sock, '::1', 1337);
      SendStr(sock, HelloStr);
      Received := ReceiveStr(sock);
      if Received <> ReplyStr then
        ClientError := 'Unexpected response: ' + Received;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure DualStackTestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPDualStack);
    try
      Bind(sock, '::0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := ReceiveStr(Conn.Socket);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if not IsIPv4Mapped(Conn.ClientAddress) then
      ServerError := 'Expected IPv4 mapped Address, got ' + Conn.ClientAddress.Address;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure CloseTestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      CloseSocket(Conn.Socket);
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure CloseTestClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      Sleep(100);
      if not StreamClosed(sock) then
        ClientError := 'Should detect closed stream by server';
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure DataAvailableTestClient;
var
  sock: TFPSocket;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      SendStr(sock, HelloStr);
      Sleep(600);
      if not DataAvailable(sock) then
      begin
        ClientError := 'Should have data from the server pending';
        Exit;
      end;
      if BytesAvailable(sock) <> Length(ReplyStr) then
        ClientError := 'Unexpected data length';
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure ReceiveArrayTestServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: Array of Integer; // Hello Server = 12 chars = divisible by 4
  i:Integer;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := specialize ReceiveArray<Integer>(Conn.Socket);
        SendStr(Conn.Socket, ReplyStr);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Length(Received) * SizeOf(Integer) <> Length(HelloStr) then
    begin
      ServerError := 'Unexpected response length ' + Length(Received).ToString;
      Exit;
    end;
    for i:=0 to Length(ReplyStr) -1 do
      if PChar(@Received[0])[i]<>HelloStr[i+1] then
      begin
        ServerError := 'Unexpected response Char ' + PChar(@Received[0])[i] + '@' + i.ToString;;
        Exit;
      end;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure ReceiveArrayTestClient;
var
  sock: TFPSocket;
  Received: Array of Char;
  i:Integer;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      SendStr(sock, HelloStr);
      Received := specialize ReceiveArray<Char>(sock);
    finally
      CloseSocket(sock);
    end;
    if Length(Received) <> Length(ReplyStr) then
    begin
      ClientError := 'Unexpected response length ' + Length(Received).ToString;
      Exit;
    end;
    for i:=0 to Length(Received) -1 do
      if Received[i]<>ReplyStr[i+1] then
      begin
        ClientError := 'Unexpected response Char ' + Received[i] + '@' + i.ToString;;
        Exit;
      end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

procedure ChunkTestServer;
type
  TChunkString = String[16];
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received, toSend: TChunkString;
begin
  ServerError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Bind(sock, '0.0.0.0', 1337);
      Listen(sock, 0);
      Conn := AcceptConnection(sock);
      try
        Received := specialize Receive<TChunkString>(Conn.Socket);
        ToSend := ReplyStr;
        // Send in two halves with time delay (client must block until full chunk)
        Send(Conn.Socket, @toSend, SizeOf(toSend) div 2);
        Sleep(400);
        Send(Conn.Socket, PByte(@toSend) + SizeOf(toSend) div 2, SizeOf(toSend) - SizeOf(toSend) div 2);
      finally
        CloseSocket(Conn.Socket);
      end;
    finally
      CloseSocket(sock);
    end;
    if Received <> HelloStr then
      ServerError := 'Unexpected response: ' + Received;
  except on E: Exception do
    ServerError := 'Exception: ' + E.Message;
  end;
end;

procedure ChunkTestClient;
type
  TChunkString = String[16];
var
  sock: TFPSocket;
  Received: TChunkString;
begin
  ClientError := '';
  try
    sock := TCPSocket(stIPv4);
    try
      Connect(sock, '127.0.0.1', 1337);
      specialize Send<TChunkString>(sock, HelloStr);
      Received := specialize Receive<TChunkString>(sock);
      if Received <> ReplyStr then
        ClientError := 'Unexpected response: ' + Received;
    finally
      CloseSocket(sock);
    end;
  except on E: Exception do
    ClientError := 'Exception: ' + E.Message;
  end;
end;

type
  TTimeoutThread = class(TThread)
  protected
    procedure Execute;override;
  end;

procedure TTimeoutThread.Execute;
var
  i: Integer;
begin
  for i:=1 to 100 do
  begin
    if Terminated then
      Exit;
    Sleep(100);
  end; 
  if Terminated then
    Exit;
  WriteLn(' Timeout');
  // FPC Halt does not work with threads... so we just rawkill using libc
  cexit(1);
end;

procedure RunTest(const TestName: String; ASrv, ACli: TProcedure);
var
  Timeout, SrvThread, CliThread: TThread;
begin
  Write('Testing ', TestName, '...');
  SrvThread:=TThread.CreateAnonymousThread(ASrv);
  SrvThread.FreeOnTerminate := False;
  SrvThread.Start;
  CliThread:=TThread.CreateAnonymousThread(ACli);
  CliThread.FreeOnTerminate := False;
  CliThread.Start;
  Timeout:=TTimeoutThread.Create(false);
  SrvThread.WaitFor;
  if not ServerError.IsEmpty then
  begin
    WriteLn(LineEnding, '  Server Error: ', ServerError);
    Halt(1);
  end;
  CliThread.WaitFor;
  if not ClientError.IsEmpty then
  begin
    WriteLn(LineEnding, '  Client Error: ', ClientError);
    Halt(1);
  end;
  Timeout.Terminate;
  Timeout.Free;
  WriteLn(' Success!');
  CliThread.Free;
  SrvThread.Free;
  Sleep(200);
end;

begin
  RunTest('IPv4Test', @IPv4TestServer, @IPv4TestClient);
  RunTest('IPv6Test', @IPv6TestServer, @IPv6TestClient);
  RunTest('DualStackTest', @DualStackTestServer, @IPv4TestClient);
  RunTest('CloseTest', @CloseTestServer, @CloseTestClient);
  RunTest('DataAvailableTest', @IPv4TestServer, @DataAvailableTestClient);
  RunTest('ReceiveArrayTest', @ReceiveArrayTestServer, @ReceiveArrayTestClient);
  RunTest('ChunkTest', @ChunkTestServer, @ChunkTestClient);
end.
