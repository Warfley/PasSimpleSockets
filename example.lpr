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
    Msg := UDPReceiveStr(Sock);
    WriteLn('Received from Client at ', Msg.FromAddr.Address, ':', Msg.FromPort,' message: ', Msg.Data);
    UDPSendStr(Sock, Msg.FromAddr, Msg.FromPort, Msg.Data);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

