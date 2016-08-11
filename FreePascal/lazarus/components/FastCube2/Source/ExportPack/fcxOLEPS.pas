
{******************************************}
{                                          }
{             FastReport v5.0              }
{            OLEPS Writing API             }
{                                          }
{         Copyright (c) 1998-2011          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

{ This module provides API for writing
  OLEPS streams. The format is documented
  in MSDN [MS-OLEPS] section. }

//VCL uses section
{$IFNDEF FMX}
unit fcxOLEPS;

interface

uses
  Classes, fcxUtils;
{$ELSE}
interface

uses
  System.Classes,
  FMX.fcxUtils;
{$ENDIF}

type

  TOlepsFmtId = array[1..4] of LongWord;
  TOlepsPropId = LongWord;
  TOlepsPropType = Word;
  TOlepsAccess = LongWord;

const

  { OleSi values represents a list of
    property id values for properties
    that are stored within a <05>SummaryInformation
    OLEPS stream. }

  OlepsSiCodepage   = $01;
  OlepsSiAuthor     = $04;
  OlepsSiLastAuthor = $08;
  OlepsSiAppName    = $12;
  OlepsSiAccess     = $13;
  OlepsSiComment    = $06;
  OlepsSiKeywords   = $05;
  OlepsSiSubject    = $03;
  OlepsSiTitle      = $02;
  OlepsSiRevision   = $09;
  OlepsSiCreation   = $0c;
  OlepsSiLastSave   = $0d;

  { OlepsAf values defines flags for access
    rights that are stored in the OlepsSiAccess
    property }

  OlepsAfAll        = 0;  // All access
  OlepsAfPassword   = 1;  // Password protected
  OlepsAfReadOnlyR  = 2;  // Read-only recommended
  OlepsAfReadOnlyF  = 4;  // Read-only enforced
  OlepsAfNoAnnot    = 8;  // Locked for annotations

  { OleDsi values represents a list of
    property id values for properties
    that are stored within a
    <05>DocumentSummaryInformation OLEPS stream }

  OlepsDsiCategory  = 2;
  OlepsDsiCompany   = $f;
  OlepsDsiManager   = $e;

  { Well known FMTID values.
    The abbreviations are:

      Si  = <05>SummaryInformation
      Dsi = <05>DocumentSummaryInformation }

  OlepsFmtIdSi: TOlepsFmtId = ($f29f85e0, $10684ff9, $000891ab, $d9b3272b);
  OlepsFmtIdDsi: TOlepsFmtId = ($d5cdd502, $101b2e9c, $00089793, $aef92c2b);

  { Property types }

  OlepsPtWord       = 2;
  OlepsPtInt        = 3;
  OlepsPtString     = $1e;
  OlepsPtUnicode    = $1f;
  OlepsPtTime       = $40;

type

  { TOlepsProp represents a single
    property in a property set. Such a
    property can define an author name
    or a document title.

    For the list of values for TOlepsPropId
    see OlepsSi and OlepsDsi values.

    For the list of values for TOlepsPropType
    see OlepsPt values. }

  TOlepsProp = class(TMemoryStream)
  public

    PropId: TOlepsPropId;
    PropType: TOlepsPropType;
    Offset: LongInt;

    procedure Flush(Stream: TStream);
    procedure WriteVal(Value: LongInt; BytesCount: LongInt);
    procedure WriteUCS(const Str: WideString);
    procedure WriteAnsi(const Str: AnsiString);

  end;

  { TOlepsPropSet represents a property set.
    Each property set is associated with a
    well known FMTID values. For an example,
    <05>SummaryInformation stream uses the
    [F29F85E0-4FF9-1068-AB91-08002B27B3D9] FMTID.
    For the list of values for TOlepsFmtId
    see OlepsFmtId values. }

  TOlepsPropSet = class
  public

    FmtId: TOlepsFmtId;
    Props: TList; // List of TOlepsProp
    Offset: LongInt;

    constructor Create;
    destructor Destroy; override;
    procedure Flush(Stream: TStream);
    function Add(PropId: TOlepsPropId; PropType: TOlepsPropType): TOlepsProp;

    { Adds a new non-empty string property.
      If the passed string is empty, no property
      will be added. The return value can be nil. }

    function AddUCS(PropId: TOlepsPropId; const Str: WideString): TOlepsProp;
    function AddAnsi(PropId: TOlepsPropId; const Str: AnsiString): TOlepsProp;
    function AddUnicode(PropId: TOlepsPropId; const Str: WideString): TOlepsProp;

    { Adds a property with type OlepsPtTime }

    function AddTime(PropId: TOlepsPropId; const t: TFileTime): TOlepsProp;

  end;

  { TOlepsStream represents the contents of
    a OLEPS stream. It contains of a few property sets.
    Normally, there one or two property sets
    in a stream.  }

  TOlepsStream = class
  public

    PropSets: TList; // List of TOlepsPropSet

    constructor Create;
    destructor Destroy; override;
    procedure Flush(Stream: TStream);
    function Add(const FmtId: TOlepsFmtId): TOlepsPropSet;

  end;

implementation

type

  TOlepsHeader = packed record

    ByteOrder:  Word;
    Version:    Word;
    System:     LongWord;
    CLSID:      array[1..4] of LongWord;
    PropCount:  LongWord;

  end;

{ Global routines }

procedure FreeList(l: TList);
var
  i: LongInt;
begin
  for i := 0 to l.Count - 1 do
    TObject(l[i]).Free;

  l.Free;
end;

{ TOlepsProp }

procedure TOlepsProp.Flush(Stream: TStream);
var
  x: LongInt;
begin
  Offset := Stream.Position;

  with Stream do
  begin
    Write(PropType, 2);
    x := 0;
    Write(x, 2);
  end;

  SaveToStream(Stream);
end;

procedure TOlepsProp.WriteVal(Value: LongInt; BytesCount: LongInt);
begin
  Write(Value, BytesCount);
end;

procedure TOlepsProp.WriteUCS(const Str: WideString);
var
  n, x: LongInt;
begin
  { write size of the string including
    the null terminator }

  n := Length(Str);

  if n = 0 then
  begin
    x := 0;
    Write(x, 4);
    Exit;
  end;

  x := 2 * n  + 2;
  Write(x, 4);

  { write the characters including
    the null terminator }

  Write(Str[1], 2 * n);
  x := 0;
  Write(x, 2);

  { make padding to 4-byte boundary }

  x := 0;
  n := Position;

  if n mod 4 <> 0 then
    Write(x, 4 - n mod 4);
end;

procedure TOlepsProp.WriteAnsi(const Str: AnsiString);
var
  n, x: LongInt;
begin
  { write size of the string including
    the null terminator }

  n := Length(Str);

  if n = 0 then
  begin
    x := 0;
    Write(x, 4);
    Exit;
  end;

  x := n + 1;
  Write(x, 4);

  { write the characters including
    the null terminator }

  Write(Str[1], n);
  x := 0;
  Write(x, 1);

  { make padding to 4-byte boundary }

  x := 0;
  n := Position;

  if n mod 4 <> 0 then
    Write(x, 4 - n mod 4);
end;

{ TOlepsPropSet }

constructor TOlepsPropSet.Create;
begin
  Props := TList.Create;
end;

destructor TOlepsPropSet.Destroy;
begin
  FreeList(Props);
end;

function TOlepsPropSet.Add(PropId: TOlepsPropId; PropType: TOlepsPropType): TOlepsProp;
var
  p: TOlepsProp;
begin
  p := TOlepsProp.Create;
  p.PropId := PropId;
  p.PropType := PropType;
  Props.Add(p);
  Result := p;
end;

function TOlepsPropSet.AddUCS(PropId: TOlepsPropId; const Str: WideString): TOlepsProp;
begin
  if Str = '' then
  begin
    Result := nil;
    Exit;
  end;

  Result := Add(PropId, OlepsPtString);
  Result.WriteUCS(Str);
end;

function TOlepsPropSet.AddUnicode(PropId: TOlepsPropId; const Str: WideString): TOlepsProp;
begin
  if Str = '' then
  begin
    Result := nil;
    Exit;
  end;

  Result := Add(PropId, OlepsPtUnicode);
  Result.WriteUCS(Str);
end;

function TOlepsPropSet.AddAnsi(PropId: TOlepsPropId; const Str: AnsiString): TOlepsProp;
begin
  if Str = '' then
  begin
    Result := nil;
    Exit;
  end;

  Result := Add(PropId, OlepsPtString);
  Result.WriteAnsi(Str);
end;

function TOlepsPropSet.AddTime(PropId: TOlepsPropId; const t: TFileTime): TOlepsProp;
begin
  Result := Add(PropId, OlepsPtTime);
  with Result do
  begin
    Write(t.dwLowDateTime, 4);
    Write(t.dwHighDateTime, 4);
  end;
end;

procedure TOlepsPropSet.Flush(Stream: TStream);
var
  s: TMemoryStream;
  i, x, n: LongInt;
  p: TOlepsProp;
begin
  Offset := Stream.Position;
  n := 8 + Props.Count * 8;
  s := TMemoryStream.Create;

  for i := 0 to Props.Count - 1 do
  begin
    p := TOlepsProp(Props[i]);
    x := s.Position;

    { this will make padding
      to 4-byte boundary }

    if x mod 4 <> 0 then
      s.Write(x, 4 - x mod 4);

    p.Flush(s);
  end;

  with Stream do
  begin
    x := n + s.Size;
    Write(x, 4);
    x := Props.Count;
    Write(x, 4);

    for i := 0 to Props.Count - 1 do
    begin
      p := TOlepsProp(Props[i]);
      Write(p.PropId, 4);
      x := n + p.Offset;
      Write(x, 4);
    end;
  end;

  s.SaveToStream(Stream);
  s.Free;
end;

{ TOlepsStream }

constructor TOlepsStream.Create;
begin
  PropSets := TList.Create;
end;

destructor TOlepsStream.Destroy;
begin
  FreeList(PropSets);
end;

function TOlepsStream.Add(const FmtId: TOlepsFmtId): TOlepsPropSet;
var
  p: TOlepsPropSet;
begin
  p := TOlepsPropSet.Create;
  p.FmtId := FmtId;
  PropSets.Add(p);
  Result := p;
end;

procedure TOlepsStream.Flush(Stream: TStream);
var
  h: TOlepsHeader;
  s: TMemoryStream;
  i, n, x: LongInt;
begin
  FillChar(h, SizeOf(h), 0);

  h.ByteOrder := $fffe;
  h.System    := $20001;
  h.PropCount := PropSets.Count;

  Stream.Write(h, SizeOf(h));

  n := SizeOf(h) + 20 * PropSets.Count;
  s := TMemoryStream.Create;

  for i := 0 to PropSets.Count - 1 do
    TOlepsPropSet(PropSets[i]).Flush(s);

  for i := 0 to PropSets.Count - 1 do
    with TOlepsPropSet(PropSets[i]) do
    begin
      Stream.Write(FmtId, 16);
      x := n + Offset;
      Stream.Write(x, 4);
    end;

  s.SaveToStream(Stream);
  s.Free;
end;

end.
