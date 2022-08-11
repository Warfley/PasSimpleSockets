program tcpechoserver;

{$mode objfpc}{$H+}

uses
  SysUtils, SimpleSockets;

procedure HandleConnection(Connection: TSocketConnection);
var
  Msg: String;
begin
  WriteLn('Connection from: ', Connection.ClientAddress.Address, ':', Connection.ClientPort);
  While True do begin
  Msg := ReceiveStr(Connection.Socket, 1024);
  WriteLn('Received: ', Msg);
  sleep(2500);
  SendStr(Connection.Socket, 'Pong');
  end;
end;

var
  ServerSock: TSocket;
  Conn: TSocketConnection;
begin
  ServerSock := TCPSocket(stDualStack);
  try
     // bind to any addr (ipv6 addr 0) should allow any ipv6 and ipv4 connection due to dualstack
     Bind(ServerSock, '::0', 1337);
     Listen(ServerSock, 1);
     Conn := AcceptConnection(ServerSock);
     try
       HandleConnection(Conn);
     finally
       CloseSocket(Conn.Socket);
     end;
  finally
    CloseSocket(ServerSock);
  end;
  ReadLn;
end.

