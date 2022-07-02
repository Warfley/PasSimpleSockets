program udpclient;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

var
  Sock: TSocket;
  Msg: TUDPStringMessage;
begin
  Sock := UDPSocket(stIPv4);
  try
    UDPSendStr(Sock, '127.0.0.1', 1337, 'Hello UDP');
    Msg := UDPReceiveStr(Sock);
    WriteLn('Server at ', Msg.FromAddr.Address, ':', Msg.FromPort, ' answered: ', Msg.Data);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

