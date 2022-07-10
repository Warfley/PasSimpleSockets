program example;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

var
  Sock: TSocket;
  Msg: TReceiveFromStringMessage;
begin
  Sock := UDPSocket(stDualStack);
  try
    // Binding to 0-IP (here in ipv6 format) binds to any address, with stDualStack both IPv4 and IPv6
    Bind(Sock, '::0', 1337);
    // Simple echo server: Receive message and answer with same message
    Msg := ReceiveStrFrom(Sock);
    WriteLn('Received from Client at ', Msg.FromAddr.Address, ':', Msg.FromPort,' message: ', Msg.Data);
    SendStrTo(Sock, Msg.FromAddr, Msg.FromPort, Msg.Data);
  finally
    CloseSocket(Sock);
  end;
  ReadLn;
end.

