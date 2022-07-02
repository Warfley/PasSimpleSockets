program tcpclient;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

var
  Sock: TSocket;
  Msg: String;
begin
  sock := TCPSocket(stIPv6);
  try
    TCPClientConnect(Sock, '::1', 1337);
    TCPSendStr(Sock, 'Hello Server');
    Msg := TCPReceiveStr(Sock, 32);
    WriteLn('Server answered: ', Msg);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

