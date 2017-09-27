unit Unit_Indy_Message;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdContext, IdMessage, IdTCPClient;

function SendMessage(AContext: TIdContext; AMessage: TidMessage): Boolean; overload;
function SendMessage(AClient: TIdTCPClient; AMessage: TIdMessage): Boolean; overload;
function ReceiveMessage(AContext: TIdContext; var AMessage: TIdMessage): Boolean; overload;
function ReceiveMessage(AClient: TIdTCPClient; var AMessage: TIdMessage): Boolean; overload;

implementation

uses IdAttachment, IdAttachmentMemory;

var
   Attach: TIdAttachmentMemory;

function SendMessage(AContext: TIdContext; AMessage: TidMessage): Boolean;
var
   AStream: TMemoryStream;
begin
  try
    Result := True;
    AStream:= TMemoryStream.Create;
    try
      AMessage.SaveToStream(AStream);
      AStream.Position:= 0;

      AContext.Connection.IOHandler.Write(LongInt(AStream.Size));
      AContext.Connection.IOHandler.WriteBufferOpen;
      AContext.Connection.IOHandler.Write(AStream, 0, False);
      AContext.Connection.IOHandler.WriteBufferFlush;
    finally
      AContext.Connection.IOHandler.WriteBufferClose;
      AStream.Free;
    end;
  except
    Result := False;
  end;
end;


function SendMessage(AClient: TIdTCPClient; AMessage: TIdMessage): Boolean;
var
   AStream : TMemoryStream;
begin
  try
    Result := True;
    AStream := TMemoryStream.Create;
    try
      AMessage.SaveToStream(AStream);
      AStream.Position:= 0;

      AClient.IOHandler.Write(LongInt(AStream.Size));
      AClient.IOHandler.WriteBufferOpen;
      AClient.IOHandler.Write(AStream, 0, False);
      AClient.IOHandler.WriteBufferFlush;
    finally
      AClient.IOHandler.WriteBufferClose;
      AStream.free;
    end;
  except
    Result := False;
  end;
end;

function ReceiveMessage(AContext: TIdContext; var AMessage: TIdMessage): Boolean;
var
   AStream : TMemoryStream;
begin
  Result := True;
  try
    AStream := TMemoryStream.Create;
    try
      AContext.Connection.IOHandler.ReadStream(AStream, AContext.Connection.IOHandler.ReadLongInt, False);

      AStream.Position:= 0;
      AMessage.LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  except
    Result := False;
  end;

end;

function ReceiveMessage(AClient: TIdTCPClient; var AMessage: TIdMessage): Boolean;
var
   AStream : TMemoryStream;
begin
  Result := True;
  try
    AStream := TMemoryStream.Create;
    try
      AClient.IOHandler.ReadStream(AStream, AClient.IOHandler.ReadLongInt, False);

      AStream.Position:= 0;
      AMessage.LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  except
    Result := False;
  end;
end;

end.

