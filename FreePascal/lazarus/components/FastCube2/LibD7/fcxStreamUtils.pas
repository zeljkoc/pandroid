{*******************************************************}
{                                                       }
{            FastCube 2 stream utilities unit           }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxStreamUtils;
{$INCLUDE fcx.inc}
interface

uses
{$IFDEF SQL_TYPES_EXTRA1}
  SqlTimSt,
  FMTBcd,
{$ENDIF}
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  Classes,
  SysUtils,
  fcxTypes;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}
interface

uses
{$IFDEF SQL_TYPES_EXTRA1}
  Data.SqlTimSt,
  Data.FMTBcd,
{$ENDIF}
  System.Variants, System.Classes, System.SysUtils,
  FMX.fcxTypes;
{$ENDIF FMX}

procedure WriteDouble(Stream: TStream; V: Double);
procedure WriteByte(Stream: TStream; V: Byte);
procedure WriteString(Stream: TStream; S: String);
procedure WriteWideString(Stream: TStream; S: WideString);
procedure WritefcString(Stream: TStream; S: TfcxString);
procedure WriteVariant(Stream: TStream; V: Variant; ft: integer); overload;
procedure WriteVariant(Stream: TStream; V: Variant); overload;
procedure WritePChar(Stream: TStream; P: PAnsiChar);
procedure WritePWideChar(Stream: TStream; P: PWideChar); 
procedure WriteBoolean(Stream: TStream; V: Boolean);
procedure WriteInteger(Stream: TStream; V: Integer);
procedure WriteInt64(Stream: TStream; V: Int64);
procedure WriteStream(Stream: TStream; Source: TStream);
{$IFDEF SQL_TYPES_EXTRA1}
procedure WriteSQLTimeStamp(Stream: TStream; V: TSQLTimeStamp);
procedure WriteFMTBcd(Stream: TStream; V: TBcd);
{$ENDIF}

function ReadDouble(Stream: TStream): Double;
function ReadByte(Stream: TStream): Byte;
function ReadString(Stream: TStream): String;
function ReadOldString(Stream: TStream): String;
function ReadWideString(Stream: TStream): WideString;
function ReadfcString(Stream: TStream): TfcxString;
function ReadVariant(Stream: TStream; ft: integer): Variant; overload;
function ReadOldVariant(Stream: TStream; ft: integer): Variant; overload;
function ReadVariant(Stream: TStream): Variant; overload;
function ReadPChar(Stream: TStream): PAnsiChar;
function ReadPWideChar(Stream: TStream): PWideChar;
function ReadBoolean(Stream: TStream): Boolean;
function ReadInteger(Stream: TStream): Integer;
function ReadInt64(Stream: TStream): Int64;
procedure ReadStream(Stream: TStream; Target: TStream);
procedure ReadStreamFromPosition(Stream: TStream; Target: TStream);
{$IFDEF SQL_TYPES_EXTRA1}
function ReadSQLTimeStamp(Stream: TStream): TSQLTimeStamp;
function ReadFMTBcd(Stream: TStream): TBcd;
{$ENDIF}

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxRes, fcxStringUtils, fcxUnicodeUtils;
{$ELSE FMX}
uses
  FMX.fcxRes, FMX.fcxStringUtils, FMX.fcxUnicodeUtils;
{$ENDIF FMX}

{$ifdef fpc}
  {$hints off} // too much unuseful hints
{$endif}
procedure WriteString(Stream: TStream; S: String);
var
  L: Word;
  p: PAnsiChar;
  AAnsiStr: AnsiString;
begin
  AAnsiStr := AnsiToUtf8(S);
  L := Length(AAnsiStr);
  Stream.Write(L, SizeOf(L));
  If L > 0 then
  begin
    GetMem(p, L + 1);
    StrPCopy(P, AAnsiStr);
    Stream.Write(p^, L);
    FreeMem(P);
  end;
end;

procedure WriteWideString(Stream: TStream; S: WideString);
var
  L: Word;
  p: PAnsiChar;
  AAnsiStr: AnsiString;
begin
  AAnsiStr := UTF8Encode(S);
  L := Length(AAnsiStr);
  Stream.Write(L, SizeOf(L));
  If L > 0 then
  begin
    GetMem(p, L + 1);
    StrPCopy(P, AAnsiStr);
    Stream.Write(p^, L);
    FreeMem(P);
  end;
end;

procedure WritefcString(Stream: TStream; S: TfcxString);
var
  L: Word;
  p: PAnsiChar;
  AAnsiStr: AnsiString;
begin
  AAnsiStr := UTF8Encode(S);
  L := Length(AAnsiStr);
  Stream.Write(L, SizeOf(L));
  If L > 0 then
  begin
    GetMem(p, L + 1);
    StrPCopy(p, AAnsiStr);
    Stream.Write(p^, L);
    FreeMem(p);
  end;
end;

procedure WritePChar(Stream: TStream; P: PAnsiChar);
var
  L: Word;
begin
  if P <> nil then
  begin
    L := StrLen(P);
    Stream.Write(L, SizeOf(L));
    Stream.Write(P^, L);
  end else
  begin
    L := 0;
    Stream.Write(L, SizeOf(L));
  end;
end;

procedure WritePWideChar(Stream: TStream; P: PWideChar);
var
  L: Word;
begin
  if P <> nil then
  begin
    L := Length(P);
    Stream.Write(L, SizeOf(L));
    Stream.Write(P^, L * SizeOf(WideChar));
  end else
  begin
    L := 0;
    Stream.Write(L, SizeOf(L));
  end;  
end;

function ReadPChar(Stream: TStream): PAnsiChar;
var
  L: Word;
begin
  Stream.Read(L, SizeOf(L));
  if L > 0 then
  begin
    GetMem(Result, L + 1);
    Stream.Read(Result^, L);
    Result[L] := #0;
  end
  else
    Result := nil;
end;

function ReadPWideChar(Stream: TStream): PWideChar;
var
  L: Word;
begin
  Stream.Read(L, SizeOf(L));
  if L > 0 then
  begin
    GetMem(Result, (L + 1) * SizeOf(WideChar));
    Stream.Read(Result^, L * SizeOf(WideChar));
    Result[L] := #0;
  end
  else
    Result := nil;
end;

function ReadString(Stream: TStream): String;
var
  L: Word;
  p: PAnsiChar;
begin
  Stream.Read(L, SizeOf(L));
  if L > 0 then
  begin
    GetMem(p, L + 1);
    Stream.Read(p^, L);
    p[L] := #0;
    Result := Utf8ToAnsi(p);
    FreeMem(p);
  end
  else
    Result := '';
end;

function ReadOldString(Stream: TStream): String;
var
  L: Word;
  p: PAnsiChar;
begin
  Stream.Read(L, SizeOf(L));
  if L > 0 then
  begin
    GetMem(p, L + 1);
    Stream.Read(p^, L);
    p[L] := #0;
    Result := String(StrPas(p));
    FreeMem(p, L + 1);
  end
  else
    Result := '';
end;

function ReadWideString(Stream: TStream): WideString;
var
  L: Word;
  p: PAnsiChar;
begin
  Stream.Read(L, SizeOf(L));
  if L > 0 then
  begin
    GetMem(p, L + 1);
    Stream.Read(p^, L);
    p[L] := #0;
    Result := Utf8ToAnsi(p);
    FreeMem(p);
  end
  else
    Result := '';
end;

function ReadfcString(Stream: TStream): TfcxString;
var
  L: Word;
  p: PAnsiChar;
begin
  Stream.Read(L, SizeOf(L));
  if L > 0 then
  begin
    GetMem(p, L + 1);
    Stream.Read(p^, L);
    p[L] := #0;
    Result := UTF8Decode(p);
    FreeMem(p);
  end
  else
    Result := '';
end;

function ReadSmallInt(Stream: TStream): SmallInt;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadInteger(Stream: TStream): Integer;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadInt64(Stream: TStream): Int64;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadSingle(Stream: TStream): Single;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadDouble(Stream: TStream): Double;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadCurrency(Stream: TStream): Currency;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadOleString(Stream: TStream): Variant;
var
  L: Word;
  WS: WideString;
begin
  Stream.Read(L, SizeOf(L));
  SetLength(WS, L);
  Stream.Read(WS[1], L * 2);
  Result := WS;
end;

function ReadBoolean(Stream: TStream): Boolean;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadByte(Stream: TStream): Byte;
begin
  Stream.Read(Result, SizeOf(Result));
end;

procedure WriteSmallInt(Stream: TStream; V: SmallInt);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteInteger(Stream: TStream; V: Integer);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteInt64(Stream: TStream; V: Int64);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteSingle(Stream: TStream; V: Single);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteDouble(Stream: TStream; V: Double);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteCurrency(Stream: TStream; V: Currency);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteBoolean(Stream: TStream; V: Boolean);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteByte(Stream: TStream; V: Byte);
begin
  Stream.Write(V, SizeOf(V));
end;

{$IFDEF SQL_TYPES_EXTRA1}
procedure WriteSQLTimeStamp(Stream: TStream; V: TSQLTimeStamp);
begin
  Stream.Write(V, SizeOf(V));
end;

procedure WriteFMTBcd(Stream: TStream; V: TBcd);
begin
  Stream.Write(V, SizeOf(V));
end;
{$ENDIF}

procedure WriteOleString(Stream: TStream; V: Variant);
var
  L: Word;
  WS: WideString;
begin
  WS := TVarData(V).VOleStr;
  L := Length(WS);
  Stream.Write(L, SizeOf(L));
  Stream.Write(WS[1], L * 2);
end;

procedure ReadStream(Stream: TStream; Target: TStream);
var
  p: PAnsiChar;
  sz: Integer;
begin
  sz := ReadInteger(Stream);
  GetMem(p, sz);
  Stream.Read(p^, sz);
  Target.Write(p^, sz);
  FreeMem(p, sz);
  Target.Position := 0;
end;

procedure ReadStreamFromPosition(Stream: TStream; Target: TStream);
var
  buf: array[0..8191] of AnsiChar;
  ACount: Integer;
begin
  repeat
    ACount := Stream.Read(buf, SizeOf(buf));
    Target.Write(buf, ACount);
  until ACount <> SizeOf(buf);
  Target.Position := 0;
end;

{$IFDEF SQL_TYPES_EXTRA1}
function ReadSQLTimeStamp(Stream: TStream): TSQLTimeStamp;
begin
  Stream.Read(Result, SizeOf(Result));
end;

function ReadFMTBcd(Stream: TStream): TBcd;
begin
  Stream.Read(Result, SizeOf(Result));
end;
{$ENDIF}


procedure WriteStream(Stream: TStream; Source: TStream);
var
  p: PAnsiChar;
  sz: Integer;
begin
  sz := Source.Size;
  WriteInteger(Stream, sz);
  GetMem(p, sz);
  Source.Read(p^, sz);
  Stream.Write(p^, sz);
  FreeMem(p, sz);
end;

function ReadVariant(Stream: TStream; ft: integer): Variant;
begin
  case ft of
    varSmallint    : Result := ReadSmallInt(Stream);
    varInteger     : Result := ReadInteger(Stream);
    varSingle      : Result := ReadSingle(Stream);
    varDate        : Result := TDateTime(ReadDouble(Stream));
    varDouble      : Result := ReadDouble(Stream);
    varCurrency    : Result := ReadCurrency(Stream);
    varOleStr      : Result := ReadOleString(Stream);
    varBoolean     : Result := ReadBoolean(Stream);
    varByte        : Result := ReadByte(Stream);
    varString      : Result := ReadString(Stream);
    varNull        : Result := Null;
    varEmpty       : Result := UnAssigned;
{$IFDEF DELPHI_6UP}
    varInt64       : Result := ReadInt64(Stream);
{$ENDIF}
  else
{$IFDEF SQL_TYPES_EXTRA1}
    if ft = varSQLTimeStamp then
      Result := VarSQLTimeStampCreate(ReadSQLTimeStamp(Stream))
    else
    if ft = varFMTBcd then
      Result := VarFMTBcdCreate(ReadFMTBcd(Stream))
    else
{$ENDIF}
      Result := Null;
  end;
end;

function ReadOldVariant(Stream: TStream; ft: integer): Variant;
begin
  case ft of
    varSmallint    : Result := ReadSmallInt(Stream);
    varInteger     : Result := ReadInteger(Stream);
    varSingle      : Result := ReadSingle(Stream);
    varDate        : Result := TDateTime(ReadDouble(Stream));
    varDouble      : Result := ReadDouble(Stream);
    varCurrency    : Result := ReadCurrency(Stream);
    varOleStr      : Result := ReadOleString(Stream);
    varBoolean     : Result := ReadBoolean(Stream);
    varByte        : Result := ReadByte(Stream);
    varString      : Result := ReadOldString(Stream);
    varNull        : Result := Null;
    varEmpty       : Result := UnAssigned;
{$IFDEF DELPHI_6UP}
    varInt64       : Result := ReadInt64(Stream);
{$ENDIF}
  else
{$IFDEF SQL_TYPES_EXTRA1}
    if ft = varSQLTimeStamp then
      Result := VarSQLTimeStampCreate(ReadSQLTimeStamp(Stream))
    else
    if ft = varFMTBcd then
      Result := VarFMTBcdCreate(ReadFMTBcd(Stream))
    else
{$ENDIF}
      Result := Null;
  end;
end;

function ReadVariant(Stream: TStream): Variant;
begin
  Result := ReadVariant (Stream, ReadInteger (Stream));
end;

procedure WriteVariant(Stream: TStream; V: Variant; ft: integer);
begin
  case ft of
    varSmallint    : WriteSmallInt(Stream, V);
    varInteger     : WriteInteger(Stream, V);
    varSingle      : WriteSingle(Stream, V);
    varDate        : WriteDouble(Stream, V);
    varDouble      : WriteDouble(Stream, V);
    varCurrency    : WriteCurrency(Stream, V);
    varOleStr      : WriteOleString(Stream, V);
    varBoolean     : WriteBoolean(Stream, V);
    varByte        : WriteByte(Stream, V);
    varString      : WriteString(Stream, V);
{$IFDEF DELPHI_6UP}
    varInt64       : WriteInt64(Stream, V);
{$ENDIF}
{$IFDEF SQL_TYPES_EXTRA1}
  else
    if ft = varSQLTimeStamp then
      WriteSQLTimeStamp(Stream, VarToSQLTimeStamp(V))
    else
    if ft = varFMTBcd then
      WriteFMTBcd(Stream, VarToBcd(V));
{$ENDIF}
  end;
end;

procedure WriteVariant(Stream: TStream; V: Variant);
var
  ft: integer;
begin
  ft := VarType(V);
  WriteInteger(Stream, VarType (V));
  WriteVariant(Stream, V, ft);
end;

end.
