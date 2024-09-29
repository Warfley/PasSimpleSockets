program tcpclient;

{$mode objfpc}{$H+}

uses
  fpsockets;

var
  Sock: TFPSocket;
  Msg: String;
begin
  sock := TCPSocket(stIPv4);
  try
    Connect(Sock, '127.0.0.1', 1337);
    SendStr(Sock, 'Hello Server');
    Msg := ReceiveStr(Sock, 32);
    WriteLn('Server answered: ', Msg);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

