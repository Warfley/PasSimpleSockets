program tcptest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fpsockets;

const
  HelloStr = 'Hello Server';
  ReplyArray: Array of Char = ('H', 'e', 'l', 'l', 'o');

var ClientOk, ServerOk: Boolean;

procedure RunServer;
var
  sock: TFPSocket;
  Conn: TFPSocketConnection;
  Received: String;
begin
  sock := TCPSocket(stDualStack);
  Bind(sock, '::0', 1337);
  Listen(sock, 0);
  Write('Server: Waiting for connection');
  Conn := AcceptConnection(sock);
  Received := ReceiveStr(Conn.Socket);
  specialize SendArray<Char>(Conn.Socket, ReplyArray);
  CloseSocket(Conn.Socket);
  CloseSocket(sock);
  if Received <> HelloStr then
  begin
    WriteLn('Server: Unexpected response: ', Received);
    Exit;
  end;
  ServerOk := True;
end;

procedure RunClient;
var
  sock: TFPSocket;
  Received: Array of Char;
  i: Integer;
begin
  sock := TCPSocket(stIPv4);
  Connect(sock, '127.0.0.1', 1337);
  SendStr(sock, HelloStr);
  Received := specialize ReceiveArray<Char>(sock);
  Sleep(100);
  try
    ReceiveStr(sock);
    // Server should have closed socket so exception should be fired
    ClientOk:=False;
    WriteLn('Client: Expected server to close connection');
  except on E: EConnectionClosedException do
    ClientOk:=True;
  end;
  CloseSocket(sock);
  if Length(Received) <> Length(ReplyArray) then
  begin
    WriteLn('Client: Unexpected response size: ', Length(Received));
    ClientOk := False;
    Exit;
  end;
  for i:=0 to Length(Received)-1 do
    if Received[i] <> ReplyArray[i] then
    begin
      WriteLn('Client: Unexpected response char at: ', i, ' ''', Received[i], '''');
      ClientOk := False;
      Exit;
    end;
end;

var
  SrvThread, CliThread: TThread;
begin
  SrvThread:=TThread.CreateAnonymousThread(@RunServer);
  SrvThread.Start;
  CliThread:=TThread.CreateAnonymousThread(@RunClient);
  CliThread.Start;
  SrvThread.WaitFor;
  CliThread.WaitFor;
  if not ServerOk or Not ClientOk then
  begin
    WriteLn('Error, see ouptut above');
    ExitCode:=1;
  end;
end.

