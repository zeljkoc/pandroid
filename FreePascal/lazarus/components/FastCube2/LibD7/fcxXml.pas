{******************************************}
{                                          }
{         FastCube 2 XML document          }
{                                          }
{         Copyright (c) 1998-2014          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{           Modified for FastCube          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}

unit fcxXml;

interface

{$i fcx.inc}

uses
{$IFNDEF CROSS_COMPILE}
  Windows, 
{$ENDIF}
{$IFNDEF DELPHI_6UP}
  FileCtrl,
{$ENDIF}
  fcxUnicodeUtils,
  Types, SysUtils, Classes;
//FMX uses
{$ELSE}
interface

{$i fcx.inc}

uses
  FMX.fcxUnicodeUtils,
  System.Types, System.SysUtils, System.Classes;

{$ENDIF}

type
  TfcxInvalidXMLException = class(Exception);

  TfcxXMLItem = class(TObject)
  private
    FData: Pointer;              { optional item data }
    FHiOffset: Byte;             { hi-part of the offset }
    FItems: TList;               { subitems }
    FLoaded: Boolean;            { item is loaded, no need to call LoadItem }
    FLoOffset: Integer;          { lo-part of the offset }
    FModified: Boolean;          { item is modified (used by preview designer) }
    FName: String;               { item name }
    FParent: TfcxXMLItem;        { item parent }
    FText: String;               { item attributes }
    FUnloadable: Boolean;
    FValue: String;              { item value <item>Value</item> }
    function GetCount: Integer;
    function GetItems(Index: Integer): TfcxXMLItem;
    function GetOffset: Int64;
    procedure SetOffset(const Value: Int64);
    function GetProp(Index: String): String;
    procedure SetProp(Index: String; const Value: String);
    function GetBoolProp(Index: String): Boolean;
    function GetDateProp(Index: String): TDateTime;
    function GetFloatProp(Index: String): Double;
    function GetIntProp(Index: String): Integer;
    procedure SetBoolProp(Index: String; const Value: Boolean);
    procedure SetDateProp(Index: String; const Value: TDateTime);
    procedure SetFloatProp(Index: String; const Value: Double);
    procedure SetIntProp(Index: String; const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(Item: TfcxXMLItem);
    procedure Clear;
    procedure InsertItem(Index: Integer; Item: TfcxXMLItem);

    function Add: TfcxXMLItem; overload;
    function Add(Name: string): TfcxXMLItem; overload;
    function Find(const Name: String): Integer;
    function FindItem(const Name: String): TfcxXMLItem;
    function IndexOf(Item: TfcxXMLItem): Integer;
    function PropExists(const Index: String): Boolean;
    function Root: TfcxXMLItem;
    procedure DeleteProp(const Index: String);

    property Count: Integer read GetCount;
    property Data: Pointer read FData write FData;
    property Items[Index: Integer]: TfcxXMLItem read GetItems; default;
    property Loaded: Boolean read FLoaded;
    property Modified: Boolean read FModified write FModified;
    property Name: String read FName write FName;
{ offset is the position of the item in the tempstream. This parameter is needed
  for dynamically loading large files. Items that can be loaded on-demand must
  have Unloadable = True (in run-time) or have 'ld="0"' parameter (in the file) }
    property Offset: Int64 read GetOffset write SetOffset;
    property Parent: TfcxXMLItem read FParent;
    property Prop[Index: String]: String read GetProp write SetProp;
    property IntProp[Index: String]: Integer read GetIntProp write SetIntProp;
    property DateProp[Index: String]: TDateTime read GetDateProp write SetDateProp;
    property FloatProp[Index: String]: Double read GetFloatProp write SetFloatProp;
    property BoolProp[Index : String]: Boolean read GetBoolProp write SetBoolProp;
    property Text: String read FText write FText;
    property Unloadable: Boolean read FUnloadable write FUnloadable;
    property Value: String read FValue write FValue;
  end;

  TfcxXMLDocument = class(TObject)
  private
    FAutoIndent: Boolean;        { use indents when writing document to a file }
    FRoot: TfcxXMLItem;          { root item }
    FTempDir: String;            { folder for temporary files }
    FTempFile: String;           { tempfile name }
    FTempStream: TStream;        { temp stream associated with tempfile }
    FTempFileCreated: Boolean;   { tempfile has been created - need to delete it }
    FOldVersion: Boolean;
    procedure CreateTempFile;
    procedure DeleteTempFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadItem(Item: TfcxXMLItem);
    procedure UnloadItem(Item: TfcxXMLItem);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream; AllowPartialLoading: Boolean = False);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);

    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
    property Root: TfcxXMLItem read FRoot;
    property TempDir: String read FTempDir write FTempDir;
    property OldVersion: Boolean read FOldVersion;
  end;

{ TfcxXMLReader and TfcxXMLWriter are doing actual read/write to the XML file.
  Read/write process is buffered. }

  TfcxXMLReader = class(TObject)
  private
    FBuffer: PAnsiChar;
    FBufPos: Integer;
    FBufEnd: Integer;
    FPosition: Int64;
    FSize: Int64;
    FStream: TStream;
    FOldFormat: Boolean;
    procedure SetPosition(const Value: Int64);
    procedure ReadBuffer;
    procedure ReadItem(out NameS, Text, Value: String);
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure RaiseException;
    procedure ReadHeader;
    procedure ReadRootItem(Item: TfcxXMLItem; ReadChildren: Boolean = True);
    property Position: Int64 read FPosition write SetPosition;
    property Size: Int64 read FSize;
  end;

  TfcxXMLWriter = class(TObject)
  private
    FAutoIndent: Boolean;
    FBuffer: AnsiString;
    FStream: TStream;
    FTempStream: TStream;
    procedure FlushBuffer;
    procedure WriteLn(const s: AnsiString);
    procedure WriteItem(Item: TfcxXMLItem; Level: Integer = 0);
  public
    constructor Create(Stream: TStream);
    procedure WriteHeader;
    procedure WriteRootItem(RootItem: TfcxXMLItem);
    property TempStream: TStream read FTempStream write FTempStream;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent;
  end;


{ fcStrToXML changes '<', '>', '"', cr, lf symbols to its ascii codes }
function fcStrToXML(const s: String): String;

{ fcValueToXML convert a value to the valid XML string }
function fcValueToXML(const Value: Variant): String;

{ fcXMLToStr is opposite to frcStrToXML function }
function fcXMLToStr(const s: String): String;

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxUtils;
//FMX uses
{$ELSE}
uses
  FMX.fcxUtils;
{$ENDIF}
  
{$IFNDEF FPC}
{$IFNDEF DELPHI_6UP}
var
  TrueBoolStrs: array of String;
  FalseBoolStrs: array of String;

const
  DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

  procedure VerifyBoolStrArray;
  begin
    if Length(TrueBoolStrs) = 0 then
    begin
      SetLength(TrueBoolStrs, 1);
      TrueBoolStrs[0] := DefaultTrueBoolStr;
    end;
    if Length(FalseBoolStrs) = 0 then
    begin
      SetLength(FalseBoolStrs, 1);
      FalseBoolStrs[0] := DefaultFalseBoolStr;
    end;
  end;

  function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): String;
  const
    cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
  begin
    if UseBoolStrs then
    begin
      VerifyBoolStrArray;
      if B then
        Result := TrueBoolStrs[0]
      else
        Result := FalseBoolStrs[0];
    end
    else
      Result := cSimpleBoolStrs[B];
  end;

  function TryStrToBool(const S: String; out Value: Boolean): Boolean;
    function CompareWith(const aArray: array of String): Boolean;
    var
      I: Integer;
    begin
      Result := False;
      for I := Low(aArray) to High(aArray) do
        if AnsiSameText(S, aArray[I]) then
        begin
          Result := True;
          Break;
        end;
    end;
  var
    LResult: Extended;
  begin
    Result := TextToFloat(PAnsiChar(S), LResult, fvExtended);
    if Result then
      Value := LResult <> 0
    else
    begin
      VerifyBoolStrArray;
      Result := CompareWith(TrueBoolStrs);
      if Result then
        Value := True
      else
      begin
        Result := CompareWith(FalseBoolStrs);
        if Result then
          Value := False;
      end;
    end;
  end;

  function StrToBool(const S: String): Boolean;
  resourcestring SInvalidBoolean = '''%s'' is not a valid boolean value';
  begin
    if not TryStrToBool(S, Result) then
      raise EConvertError.CreateResFmt(@SInvalidBoolean, [S])
  end;
{$ENDIF}
{$ENDIF}

function fcStrToXML(const s: String): String;
const
  SpecChars = ['<', '>', '"', #10, #13, '&'];
var
  i, lenRes, resI, ch: Integer;
  pRes: PChar;

  procedure ReplaceChars(var s: String; i: Integer);
  begin
    Insert('#' + IntToStr(Ord(s[i])) + ';', s, i + 1);
    s[i] := '&';
  end;

begin
  lenRes := Length(s);

  if lenRes < 32 then
  begin
    Result := s;
    for i := lenRes downto 1 do
{$IFDEF Delphi12}
      if CharInSet(s[i], SpecChars) then
{$ELSE}
      if s[i] in SpecChars then
{$ENDIF}
        if (s[i] = '&') and (Copy(s, i + 1, 5) = 'quot;') then
        begin
          Delete(Result, i, 6);
          Insert('&#34;', Result, i);
        end
        else
          ReplaceChars(Result, i);
    Exit;
  end;

  { speed optimized code }
  SetLength(Result, lenRes);
  pRes := PChar(Result) - 1;
  resI := 1;
  i := 1;

  while i <= Length(s) do
  begin
    if resI + 5 > lenRes then
    begin
      Inc(lenRes, 256);
      SetLength(Result, lenRes);
      pRes := PChar(Result) - 1;
    end;

{$IFDEF Delphi12}
    if CharInSet(s[i], SpecChars) then
{$ELSE}
    if s[i] in SpecChars then
{$ENDIF}
    begin
      if (s[i] = '&') and (i <= Length(s) - 5) and (s[i + 1] = 'q') and
        (s[i + 2] = 'u') and (s[i + 3] = 'o') and (s[i + 4] = 't') and (s[i + 5] = ';') then
      begin
        pRes[resI] := '&';
        pRes[resI + 1] := '#';
        pRes[resI + 2] := '3';
        pRes[resI + 3] := '4';
        pRes[resI + 4] := ';';
        Inc(resI, 4);
        Inc(i, 5);
      end
      else
      begin
        pRes[resI] := '&';
        pRes[resI + 1] := '#';

        ch := Ord(s[i]);
        if ch < 10 then
        begin
          pRes[resI + 2] := Char(Chr(ch + $30));
          Inc(resI, 3);
        end
        else if ch < 100 then
        begin
          pRes[resI + 2] := Char(Chr(ch div 10 + $30));
          pRes[resI + 3] := Char(Chr(ch mod 10 + $30));
          Inc(resI, 4);
        end
        else
        begin
          pRes[resI + 2] := Char(Chr(ch div 100 + $30));
          pRes[resI + 3] := Char(Chr(ch mod 100 div 10 + $30));
          pRes[resI + 4] := Char(Chr(ch mod 10 + $30));
          Inc(resI, 5);
        end;
        pRes[resI] := ';';
      end;
    end
    else
      pRes[resI] := s[i];
    Inc(resI);
    Inc(i);
  end;

  SetLength(Result, resI - 1);
end;

function fcXMLToStr(const s: String): String;
var
  i, j, h, n: Integer;
begin
  Result := s;
  i := 1;
  n := Length(s);
  while i < n do
  begin
    if Result[i] = '&' then
      if (i + 3 <= n) and (Result[i + 1] = '#') then
      begin
        j := i + 3;
        while Result[j] <> ';' do
          Inc(j);
        h := StrToInt(String(Copy(Result, i + 2, j - i - 2)));
        Delete(Result, i, j - i);
        Result[i] := Char(Chr(h));
        Dec(n, j - i);
      end
      else if Copy(Result, i + 1, 5) = 'quot;' then
      begin
        Delete(Result, i, 5);
        Result[i] := '"';
        Dec(n, 5);
      end
      else if Copy(Result, i + 1, 4) = 'amp;' then
      begin
        Delete(Result, i, 4);
        Result[i] := '&';
        Dec(n, 4);
      end
      else if Copy(Result, i + 1, 3) = 'lt;' then
      begin
        Delete(Result, i, 3);
        Result[i] := '<';
        Dec(n, 3);
      end
      else if Copy(Result, i + 1, 3) = 'gt;' then
      begin
        Delete(Result, i, 3);
        Result[i] := '>';
        Dec(n, 3);
      end;
    Inc(i);
  end;
end;

function fcValueToXML(const Value: Variant): String;
begin
  case TVarData(Value).VType of
    varSmallint, varInteger, varByte:
      Result := IntToStr(Value);

    varSingle, varDouble, varCurrency:
      Result := FloatToStr(Value);

    varDate:
      Result := DateToStr(Value);

    varOleStr, varString, varVariant{$IFDEF Delphi12}, varUString{$ENDIF}:
      Result := fcStrToXML(Value);

    varBoolean:
      if Value = True then Result := '1' else Result := '0';

    else
      Result := '';
  end;
end;

{ TfcxXMLItem }

constructor TfcxXMLItem.Create;
begin
  FLoaded := True;
end;

destructor TfcxXMLItem.Destroy;
begin
  Clear;
  if FParent <> nil then
    FParent.FItems.Remove(Self);
  inherited;
end;

procedure TfcxXMLItem.Clear;
begin
  if FItems <> nil then
  begin
    while FItems.Count > 0 do
      TfcxXMLItem(FItems[0]).Free;
    FItems.Free;
    FItems := nil;
  end;
  if FUnloadable then
    FLoaded := False;
end;

function TfcxXMLItem.GetIntProp(Index: String): Integer;
var
  Code: Integer;
begin
  Val(GetProp(Index), Result, Code);
end;

function TfcxXMLItem.GetItems(Index: Integer): TfcxXMLItem;
begin
  Result := TfcxXMLItem(FItems[Index]);
end;

function TfcxXMLItem.GetBoolProp(Index: String): Boolean;
begin
{$IFDEF DELPHI_6UP}
  Result := StrToBoolDef(GetProp(Index), False);
{$ELSE}
  try
    Result := StrToBool(GetProp(Index));
  except
    Result := False;
  end;
{$ENDIF}
end;

function TfcxXMLItem.GetCount: Integer;
begin
  if FItems = nil then
    Result := 0 else
    Result := FItems.Count;
end;

function TfcxXMLItem.GetDateProp(Index: String): TDateTime;
var
  StrDateTime: String;
{$IFDEF DELPHI7}
  Settings: TFormatSettings;
{$ENDIF}
begin
  StrDateTime := GetProp(Index);
{$IFDEF DELPHI7}
  Settings.DateSeparator := '.';
  Settings.TimeSeparator := ':';
  Settings.ShortDateFormat := 'DD.MM.YYYY';
  Settings.LongDateFormat := 'DD.MM.YYYY';
  Settings.ShortTimeFormat := 'hh:mm:ss:zzz';
  Settings.LongTimeFormat := 'hh:mm:ss:zzz';
  Settings.TwoDigitYearCenturyWindow := 50;
  Result := StrToDateTimeDef(StrDateTime, 0, Settings);
{$ELSE}
  // fpc and D6 doesnot support settings now
  {$ifndef fpc}
    {$ifdef DELPHI6}
      Result := StrToDateTimeDef(StrDateTime, 0);
    {$else}
      try
        Result := StrToDateTime(StrDateTime);
      except
        Result := 0;
      end;
    {$endif}
  {$else}
    try
      Result := StrToDateTime(StrDateTime);
    except
      Result := 0;
    end;
  {$endif}
{$ENDIF}
end;

function TfcxXMLItem.GetFloatProp(Index: String): Double;
var
{$IFDEF DELPHI7}
  Settings : TFormatSettings;
{$ENDIF}
  StrFloat : String;
{$IFNDEF DELPHI6}
  LResult : Extended;
{$ENDIF}
begin
  StrFloat := GetProp(Index);
{$IFDEF DELPHI7}
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := ' ';
  Result := StrToFloatDef(StrFloat, 0, Settings);
{$ELSE}
  {$IFDEF DELPHI6}
    Result := StrToFloatDef(StrFloat, 0);
  {$ELSE}
    if not TextToFloat(PChar(StrFloat), LResult, fvExtended) then
      Result := 0
    else
      Result := LResult;
  {$ENDIF}
{$ENDIF}
end;

function TfcxXMLItem.Add: TfcxXMLItem;
begin
  Result := TfcxXMLItem.Create;
  AddItem(Result);
end;

function TfcxXMLItem.Add(Name: string): TfcxXMLItem;
begin
  Result := Add;
  Result.Name := Name;
end;

procedure TfcxXMLItem.AddItem(Item: TfcxXMLItem);
begin
  if FItems = nil then
    FItems := TList.Create;

  FItems.Add(Item);
  if Item.FParent <> nil then
    Item.FParent.FItems.Remove(Item);
  Item.FParent := Self;
end;

procedure TfcxXMLItem.InsertItem(Index: Integer; Item: TfcxXMLItem);
begin
  AddItem(Item);
  FItems.Delete(FItems.Count - 1);
  FItems.Insert(Index, Item);
end;

function TfcxXMLItem.Find(const Name: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if AnsiCompareText(Items[i].Name, Name) = 0 then
    begin
      Result := i;
      break;
    end;
end;

function TfcxXMLItem.FindItem(const Name: String): TfcxXMLItem;
var
  i: Integer;
begin
  i := Find(Name);
  if i = -1 then
  begin
    Result := Add;
    Result.Name := Name;
  end
  else
    Result := Items[i];
end;

function TfcxXMLItem.GetOffset: Int64;
begin
  Result := Int64(FHiOffset) * $100000000 + Int64(FLoOffset);
end;

procedure TfcxXMLItem.SetBoolProp(Index: String; const Value: Boolean);
begin
{$ifndef fpc}
  SetProp(Index, BoolToStr(Value, False));
{$else}
  SetProp(Index, BoolToStr(Value));
{$endif}
end;

procedure TfcxXMLItem.SetDateProp(Index: String; const Value: TDateTime);
var
  StrDateTime: String;
{$IFDEF DELPHI_7UP}
  Settings: TFormatSettings;
{$ENDIF}
begin
{$IFDEF DELPHI_7UP}
  Settings.DateSeparator := '.';
  Settings.TimeSeparator := ':';
  Settings.ShortDateFormat := 'DD.MM.YYYY';
  Settings.LongDateFormat := 'DD.MM.YYYY';
  Settings.ShortTimeFormat := 'hh:mm:ss:zzz';
  Settings.LongTimeFormat := 'hh:mm:ss:zzz';
  Settings.TwoDigitYearCenturyWindow := 50;
  StrDateTime := DateTimeToStr(Value, Settings);
{$ELSE}
  StrDateTime := DateTimeToStr(Value);
{$ENDIF}
  SetProp(Index, StrDateTime);
end;

procedure TfcxXMLItem.SetFloatProp(Index: String; const Value: Double);
var
{$IFDEF DELPHI_7UP}
  Settings: TFormatSettings;
{$ENDIF}
  StrFloat: String;
begin
{$IFDEF DELPHI_7UP}
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := ' ';
  StrFloat := FloatToStr(Value, Settings);
{$ELSE}
  StrFloat := FloatToStr(Value);
{$ENDIF}
  SetProp(Index, StrFloat);
end;

procedure TfcxXMLItem.SetIntProp(Index: String; const Value: Integer);
begin
  SetProp(Index, IntToStr(Value));
end;

procedure TfcxXMLItem.SetOffset(const Value: Int64);
begin
  FHiOffset := Value div $100000000;
  FLoOffset := Value mod $100000000;
end;

function TfcxXMLItem.Root: TfcxXMLItem;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TfcxXMLItem.GetProp(Index: String): String;
var
  i: Integer;
begin
  i := Pos(' ' + AnsiUppercase(Index) + '="', AnsiUppercase(' ' + FText));
  if i <> 0 then
  begin
    Result := Copy(FText, i + Length(String(Index) + '="'), MaxInt);
    Result := fcXMLToStr(Copy(Result, 1, Pos('"', Result) - 1));
  end
  else
    Result := '';
end;

procedure TfcxXMLItem.SetProp(Index: String; const Value: String);
var
  i, j: Integer;
  s: String;
begin
  i := Pos(' ' + AnsiUppercase(Index) + '="', AnsiUppercase(' ' + FText));
  if i <> 0 then
  begin
    j := i + Length(Index + '="');
    while (j <= Length(FText)) and (FText[j] <> '"') do
      Inc(j);
    Delete(FText, i, j - i + 1);
  end
  else
    i := Length(FText) + 1;

  s := Index + '="' + fcStrToXML(Value) + '"';
  if (i > 1) and (FText[i - 1] <> ' ') then
    s := ' ' + s;
  Insert(s, FText, i);
end;

function TfcxXMLItem.PropExists(const Index: String): Boolean;
begin
  Result := Pos(' ' + AnsiUppercase(String(Index)) + '="', ' ' + AnsiUppercase(String(FText))) > 0;
end;

procedure TfcxXMLItem.DeleteProp(const Index: String);
var
  i: Integer;
begin
  i := Pos(' ' + AnsiUppercase(String(Index)) + '="', ' ' + AnsiUppercase(String(FText)));
  if i > 0 then
  begin
    SetProp(Index, '');
    Delete(FText, i, Length(Index) + 4);
  end;
end;

function TfcxXMLItem.IndexOf(Item: TfcxXMLItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

{ TfcxXMLDocument }

constructor TfcxXMLDocument.Create;
begin
  FRoot := TfcxXMLItem.Create;
  FOldVersion := False;
end;

destructor TfcxXMLDocument.Destroy;
begin
  DeleteTempFile;
  FRoot.Free;
  inherited;
end;

procedure TfcxXMLDocument.Clear;
begin
  FRoot.Clear;
  DeleteTempFile;
end;

procedure TfcxXMLDocument.CreateTempFile;
begin
  if FTempFileCreated then Exit;
  FTempFile := GetTempFileName(TempDir, 'fr');
  FTempStream := TFileStream.Create(FTempFile, fmOpenReadWrite);
  FTempFileCreated := True;
end;

procedure TfcxXMLDocument.DeleteTempFile;
begin
  if FTempFileCreated then
  begin
    FTempStream.Free;
    FTempStream := nil;
    DeleteFile(FTempFile);
    FTempFileCreated := False;
  end;
  if FTempStream <> nil then
    FTempStream.Free;
  FTempStream := nil;
end;

procedure TfcxXMLDocument.LoadItem(Item: TfcxXMLItem);
var
  rd: TfcxXMLReader;
  Text: String;
begin
  if (FTempStream = nil) or Item.FLoaded or not Item.FUnloadable then Exit;

  rd := TfcxXMLReader.Create(FTempStream);
  try
    rd.Position := Item.Offset;
    Text := Item.Text;
    rd.ReadRootItem(Item);
    Item.Text := Text;
    Item.FLoaded := True;
  finally
    rd.Free;
  end;
end;

procedure TfcxXMLDocument.UnloadItem(Item: TfcxXMLItem);
var
  wr: TfcxXMLWriter;
begin
  if not Item.FLoaded or not Item.FUnloadable then Exit;

  CreateTempFile;
  FTempStream.Position := FTempStream.Size;
  wr := TfcxXMLWriter.Create(FTempStream);
  try
    Item.Offset := FTempStream.Size;
    wr.WriteRootItem(Item);
    Item.Clear;
  finally
    wr.Free;
  end;
end;

procedure TfcxXMLDocument.LoadFromStream(Stream: TStream;
  AllowPartialLoading: Boolean = False);
var
  rd: TfcxXMLReader;
begin
  DeleteTempFile;

  rd := TfcxXMLReader.Create(Stream);
  try
    FRoot.Clear;
    FRoot.Offset := 0;
    rd.ReadHeader;
    FOldVersion := rd.FOldFormat;
    rd.ReadRootItem(FRoot, not AllowPartialLoading);
  finally
    rd.Free;
  end;

  if AllowPartialLoading then
    FTempStream := Stream else
    FTempStream := nil;
end;

procedure TfcxXMLDocument.SaveToStream(Stream: TStream);
var
  wr: TfcxXMLWriter;
begin
  wr := TfcxXMLWriter.Create(Stream);
  wr.TempStream := FTempStream;
  wr.FAutoIndent := FAutoIndent;

  try
    wr.WriteHeader;
    wr.WriteRootItem(FRoot);
  finally
    wr.Free;
  end;
end;

procedure TfcxXMLDocument.LoadFromFile(const FileName: String);
var
  s: TFileStream;
begin
  s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(s, True);
end;

procedure TfcxXMLDocument.SaveToFile(const FileName: String);
var
  s: TFileStream;
begin
  s := TFileStream.Create(FileName + '.tmp', fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;

  DeleteTempFile;
  DeleteFile(FileName);
  RenameFile(FileName + '.tmp', FileName);
  LoadFromFile(FileName);
end;


{ TfcxXMLReader }

constructor TfcxXMLReader.Create(Stream: TStream);
begin
  FStream := Stream;
  FSize := Stream.Size;
  FPosition := Stream.Position;
  GetMem(FBuffer, 4096);
end;

destructor TfcxXMLReader.Destroy;
begin
  FreeMem(FBuffer, 4096);
  FStream.Position := FPosition;
  inherited;
end;

procedure TfcxXMLReader.ReadBuffer;
begin
  FBufEnd := FStream.Read(FBuffer^, 4096);
  FBufPos := 0;
end;

procedure TfcxXMLReader.SetPosition(const Value: Int64);
begin
  FPosition := Value;
  FStream.Position := Value;
  FBufPos := 0;
  FBufEnd := 0;
end;

procedure TfcxXMLReader.RaiseException;
begin
  raise TfcxInvalidXMLException.Create('Invalid file format');
end;

procedure TfcxXMLReader.ReadHeader;
var
  s1, s2, s3: String;
  i: Integer;
  Ver: String;
begin
  ReadItem(s1, s2, s3);
  if Pos('?xml', s1) <> 1 then
    RaiseException;
  i := Pos('version=', s2);
  if i <> 0 then
    Ver := Copy(s2, i + 9, 3);
  i := Pos('standalone=', s2);
  FOldFormat := (Ver = '1.0') and (i = 0);
end;

procedure TfcxXMLReader.ReadItem(out NameS, Text, Value: String);
var
  c: Integer;
  curpos, len: Integer;
  state: (FindLeft, FindRight, FindComment, Done);
  i, comment: Integer;
  ps: PAnsiChar;
  Name: AnsiString;
begin
  Text := '';
  Value := '';
  comment := 0;
  state := FindLeft;
  curpos := 0;
  len := 4096;
  SetLength(Name, len);
  ps := @Name[1];

  while FPosition < FSize do
  begin
    if FBufPos = FBufEnd then
      ReadBuffer;
    c := Ord(FBuffer[FBufPos]);
    Inc(FBufPos);
    Inc(FPosition);

    if state = FindLeft then
    begin
      if c = Ord('<') then
        state := FindRight
      else if not (c in [13, 10]) then
        Value := Value + Chr(c);
    end
    else if state = FindRight then
    begin
      if c = Ord('>') then
      begin
        state := Done;
        break;
      end
      else if c = Ord('<') then
        RaiseException
      else
      begin
        ps[curpos] := AnsiChar(Chr(c));
        Inc(curpos);
        if (curpos = 3) and (Pos(AnsiString('!--'), Name) = 1) then
        begin
          state := FindComment;
          comment := 0;
          curpos := 0;
        end;
        if curpos >= len - 1 then
        begin
          Inc(len, 4096);
          SetLength(Name, len);
          ps := @Name[1];
        end;
      end;
    end
    else if State = FindComment then
    begin
      if comment = 2 then
      begin
        if c = Ord('>') then
          state := FindLeft
        else
          comment := 0;
      end
      else begin
        if c = Ord('-') then
          Inc(comment)
        else
          comment := 0;
      end;
    end;
  end;

  len := curpos;
  SetLength(Name, len);

  if state = FindRight then
    RaiseException;
  if (Name <> '') and (Name[len] = ' ') then
    SetLength(Name, len - 1);

  i := Pos(AnsiString(' '), Name);
  if i <> 0 then
  begin
    if FOldFormat then
      Text := String(Copy(Name, i + 1, len - i))
    else
      Text := UTF8Decode(Copy(Name, i + 1, len - i));
    Delete(Name, i, len - i + 1);
  end;
  NameS := String(Name);
  Value := Trim(Value);
end;

procedure TfcxXMLReader.ReadRootItem(Item: TfcxXMLItem; ReadChildren: Boolean = True);
var
  LastName: String;

  function DoRead(RootItem: TfcxXMLItem): Boolean;
  var
    n: Integer;
    ChildItem: TfcxXMLItem;
    Done: Boolean;
    CurPos: Int64;
  begin
    Result := False;
    CurPos := Position;
    ReadItem(RootItem.FName, RootItem.FText, RootItem.FValue);
    LastName := RootItem.FName;

    if (RootItem.Name = '') or (RootItem.Name[1] = '/') then
    begin
      Result := True;
      Exit;
    end;

    n := Length(RootItem.Name);
    if RootItem.Name[n] = '/' then
    begin
      SetLength(RootItem.FName, n - 1);
      Exit;
    end;

    n := Length(RootItem.Text);
    if (n > 0) and (RootItem.Text[n] = '/') then
    begin
      SetLength(RootItem.FText, n - 1);
      Exit;
    end;

    repeat
      ChildItem := TfcxXMLItem.Create;
      Done := DoRead(ChildItem);
      if not Done then
        RootItem.AddItem(ChildItem) else
        ChildItem.Free;
    until Done;

    if (LastName <> '') and (AnsiCompareText(LastName, '/' + RootItem.Name) <> 0) then
      RaiseException;

    n := Pos(' ld="0"', LowerCase(RootItem.Text));
    if n <> 0 then
      Delete(RootItem.FText, n, 7);
    if not ReadChildren and (n <> 0) then
    begin
      RootItem.Clear;
      RootItem.Offset := CurPos;
      RootItem.FUnloadable := True;
      RootItem.FLoaded := False;
    end;
  end;

begin
  DoRead(Item);
end;


{ TfcxXMLWriter }

constructor TfcxXMLWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TfcxXMLWriter.FlushBuffer;
begin
  if FBuffer <> '' then
    FStream.Write(FBuffer[1], Length(FBuffer));
  FBuffer := '';
end;

procedure TfcxXMLWriter.WriteLn(const s: AnsiString);
begin
  if not FAutoIndent then
    Insert(s, FBuffer, MaxInt) else
    Insert(s + #13#10, FBuffer, MaxInt);
  if Length(FBuffer) > 4096 then
    FlushBuffer;
end;

procedure TfcxXMLWriter.WriteHeader;
var
  OldAutoIndent: Boolean;
begin
  OldAutoIndent := AutoIndent;
  try
    AutoIndent := True;
    WriteLn('<?xml version="1.0" encoding="utf-8" standalone="yes"?>');
  finally
    AutoIndent := OldAutoIndent;
  end;
end;

function Dup(n: Integer): AnsiString;
begin
  SetLength(Result, n);
  FillChar(Result[1], n, ' ');
end;

procedure TfcxXMLWriter.WriteItem(Item: TfcxXMLItem; Level: Integer = 0);
var
  by_val: boolean;
  s, name: AnsiString;
begin
  by_val := False;
  if (Item.FText <> '') or (Item.FValue <> '') or Item.FUnloadable then
  begin
    if Item.FText <> '' then
    begin
      s := UTF8Encode(Item.FText);
      if (s = '') or (s[1] <> ' ') then
        s := ' ' + s;
      if Item.FUnloadable then
        s := s + 'ld="0"';
    end
    else
    if Item.FValue <> '' then
    begin
      s := UTF8Encode(Item.FValue);
      by_val := true;
    end;
  end
  else
    s := '';

  name := UTF8Encode(Item.Name);

  if by_val then
  begin
    s := '<' + name + '>' + s + '</' + name + '>';
    if FAutoIndent then
      s := Dup(Level) + s;
  end
  else
  begin
    if Item.Count = 0 then
    begin
      if Item.Value = '' then
        s := s + '/>'
      else
        s := s + '>' + UTF8Encode(Item.FValue) + '</' + name + '>'
    end
    else
      s := s + '>';
    if not FAutoIndent then
      s := '<' + name + s else
      s := Dup(Level) + '<' + name + s;
  end;    
  WriteLn(s);
end;

procedure TfcxXMLWriter.WriteRootItem(RootItem: TfcxXMLItem);

  procedure DoWrite(RootItem: TfcxXMLItem; Level: Integer = 0);
  var
    i: Integer;
    rd: TfcxXMLReader;
    NeedClear: Boolean;
  begin
    NeedClear := False;
    if not FAutoIndent then
      Level := 0;

    if (FTempStream <> nil) and RootItem.FUnloadable and not RootItem.FLoaded then
    begin
      rd := TfcxXMLReader.Create(FTempStream);
      try
        rd.Position := RootItem.Offset;
        rd.ReadRootItem(RootItem);
        NeedClear := True;
      finally
        rd.Free;
      end;
    end;

    WriteItem(RootItem, Level);
    for i := 0 to RootItem.Count - 1 do
      DoWrite(RootItem[i], Level + 2);
    if RootItem.Count > 0 then
      if not FAutoIndent then
        WriteLn('</' + AnsiString(RootItem.Name) + '>') else
        WriteLn(Dup(Level) + '</' + AnsiString(RootItem.Name) + '>');

    if NeedClear then
      RootItem.Clear;
  end;

begin
  DoWrite(RootItem);
  FlushBuffer;
end;

end.

