program udpclient;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

var
  Sock: TPasSocket;
  Msg: TReceiveFromStringMessage;
begin
  Sock := UDPSocket(stIPv4);
  try
    SendStrTo(Sock, '127.0.0.1', 1337, 'Hello UDP');
    Msg := ReceiveStrFrom(Sock);
    WriteLn('Server at ', Msg.FromAddr.Address, ':', Msg.FromPort, ' answered: ', Msg.Data);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

