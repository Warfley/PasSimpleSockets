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
    Connect(Sock, '::1', 1337);
    SendStr(Sock, 'Hello Server');
    Msg := ReceiveStr(Sock, 32);
    WriteLn('Server answered: ', Msg);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

