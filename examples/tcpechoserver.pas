program tcpechoserver;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

procedure HandleConnection(Connection: TTCPConnection);
var
  Msg: String;
begin
  WriteLn('Connection from: ', Connection.ClientAddress.Address, ':', Connection.ClientPort);
  Msg := TCPReceiveStr(Connection.Socket, 1024);
  WriteLn('Received: ', Msg);
  TCPSendStr(Connection.Socket, Msg);
end;

var
  ServerSock: TSocket;
  Conn: TTCPConnection;
begin
  ServerSock := TCPSocket(stDualStack);
  try
     // bind to any addr (ipv6 addr 0) should allow any ipv6 and ipv4 connection due to dualstack
     Bind(ServerSock, '::0', 1337);
     TCPServerListen(ServerSock, 1);
     Conn := TCPServerAccept(ServerSock);
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

