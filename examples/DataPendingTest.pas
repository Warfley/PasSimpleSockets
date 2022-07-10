program DataPendingTest;

{$mode objfpc}{$H+}

uses
  SimpleSockets;

var
  Sock: TSocket;
  dots, i: Integer;
  Msg: TReceiveFromStringMessage;
begin
  Sock := UDPSocket(stDualStack);
  // Listen to any connection
  Bind(Sock, '::0', 1337);
  dots := 0;
  // DataAvailable will wait until data is available, or stop after 1 second
  while not DataAvailable(Sock, 1000) do
  begin
    // When no data available after 1 second show message
    Write(#13'Waiting for Data');
    for i := 0 to dots do
      Write('.');
    Write('  ');
    dots := (dots + 1) mod 3;
  end;
  Msg := ReceiveStrFrom(Sock);
  WriteLn;
  WriteLn('Data Received from ', Msg.FromAddr.Address, ':', Msg.FromPort, ': ', Msg.Data);
  SendStrTo(Sock, Msg.FromAddr, Msg.FromPort, Msg.Data);
  ReadLn;
end.

