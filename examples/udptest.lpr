program udptest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, fpsockets;

const
  SendStr = 'Hello Server';
  ReplyStr = 'Hello Client';

var ClientOk, ServerOk: Boolean;

procedure RunServer;
var
  sock: TFPSocket;
  Received: TReceiveFromStringMessage;
begin
  sock := UDPSocket(stDualStack);
  Bind(sock, '::0', 1337);
  Write('Server: Waiting for data (Timeout 100ms)');
  while not DataAvailable(sock, 100) do
    Write('.');
  WriteLn;
  if BytesAvailable(sock)<>SendStr.Length then
    WriteLn('Unexpected response Length');
  Received := ReceiveStrFrom(sock);
  SendStrTo(sock, Received.FromAddr, Received.FromPort, ReplyStr);
  CloseSocket(sock);
  if Received.Data <> SendStr then
  begin
    WriteLn('Server: Unexpected response: ', Received.Data);
    Exit;
  end;
  ServerOk := True;
end;

procedure RunClient;
var
  sock: TFPSocket;
  Received: String;
begin
  sock := UDPSocket(stIPv4);
  SendStrTo(sock, '127.0.0.1', 1337, SendStr);
  Sleep(100);
  Received := ReceiveStr(sock);
  CloseSocket(sock);
  if Received <> ReplyStr then
  begin
    WriteLn('Client: Unexpected response: ', Received);
    Exit;
  end;
  ClientOk := True;
end;

var
  SrvThread, CliThread: TThread;
begin
  SrvThread:=TThread.CreateAnonymousThread(@RunServer);
  SrvThread.Start;
  Sleep(1000);
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

