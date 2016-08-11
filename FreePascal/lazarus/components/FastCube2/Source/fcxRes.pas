{*******************************************************}
{                                                       }
{             FastCube 2 resources unit                 }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxRes;
{$I fcx.inc}
interface
uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLType, InterfaceBase,
{$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  fcxUtils,
  fcxUnicodeUtils,
  fcxTypes
{$IFDEF Delphi_10UP}
, WideStrings
{$ENDIF}
  ;
//FMX uses
{$ELSE FMX}
{$I fcx.inc}
interface
uses
  System.Classes, System.SysUtils, System.TypInfo, System.WideStrings,
  FMX.fcxUtils, FMX.fcxUnicodeUtils, FMX.fcxTypes;
{$ENDIF FMX}

type

  { TfcxResources }

  TfcxResources = class(TObject)
  private
    FNames: TStringList;
    FValues: TWideStrings;
    FLanguages: TStringList;
    FHelpFile: String;
    FCP: Cardinal;
    procedure BuildLanguagesList;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const StrName: String): String;
    function GetAnsi(const StrName: String): String;
    procedure Add(const Ref, Str: String);
    procedure AddW(const Ref: String; Str: WideString);
    procedure AddStrings(const Str: String);
    procedure AddXML(const Str: AnsiString);
    procedure Clear;
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure Help(Sender: TObject); overload;

    property Languages: TStringList read FLanguages;
    property HelpFile: String read FHelpFile write FHelpFile;
  end;

function fcxResources: TfcxResources;
function fcxGet(ID: Integer): String;
function IsUTF8Application: Boolean;
function StringToControl(const ASource: TfcxString): TfcxString;
function ControlToString(const ASource: TfcxString): TfcxString;

const
  FCX_VERSION = {$I fcxVersion.inc};

implementation

//VCL uses section
{$IFNDEF FMX}
uses
{$IFNDEF FPC}
  fcxChm,
{$ENDIF}
  fcxXml,
  fcxrcStrings,
  fcxrcDesgn,
  fcxrcExports;
//FMX uses
{$ELSE FMX}
uses
//  fcxChm,
  FMX.fcxXml, FMX.fcxrcStrings, FMX.fcxrcDesgn, FMX.fcxrcExports;
{$ENDIF FMX}

var
  FResources: TfcxResources = nil;

function IsUTF8Application: Boolean;
begin
  {$IFDEF FPC}
  Result := WidgetSet.IntfSendsUTF8KeyPress;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function StringToControl(const ASource: TfcxString): TfcxString;
begin
  if IsUTF8Application then
    Result := AnsiToUTF8(ASource)
  else
    Result := ASource;
  {$IFDEF FPC_HAS_CPSTRING}
  SetCodePage(RawByteString(Result), DefaultSystemCodePage, False);
  {$ENDIF}
end;

function ControlToString(const ASource: TfcxString): TfcxString;
begin
  if IsUTF8Application then
    Result := UTF8ToAnsi(ASource)
  else
    Result := ASource;
end;

{ TfrxResources }

constructor TfcxResources.Create;
begin
  inherited;

  FNames := TStringList.Create;
{$IFDEF Delphi_10UP}
  FValues := TfcxWideStrings.Create;
{$ELSE}
  FValues := TWideStrings.Create;
{$ENDIF}
  FNames.Sorted := True;
  FLanguages := TStringList.Create;
  HelpFile := 'FCUser.chm';
  FCP := 0;
  BuildLanguagesList;
end;

destructor TfcxResources.Destroy;
begin
  FLanguages.Free;
  FNames.Free;
  FValues.Free;
  inherited;
end;

procedure TfcxResources.AddW(const Ref: String; Str: WideString);
var
  i: Integer;
begin
  i := FNames.IndexOf(Ref);
  if i = -1 then
  begin
    FNames.AddObject(Ref, Pointer(FValues.Count));
    FValues.Add(Str);
  end
  else
    FValues[Integer(FNames.Objects[i])] := Str;
end;

procedure TfcxResources.Add(const Ref, Str: String);
begin
{$IFDEF Delphi12}
  AddW(Ref, Str);
{$ELSE}
  AddW(Ref, WideString(AnsiToUnicode(Str, DEFAULT_CHARSET, FCP)));
{$ENDIF}
end;

procedure TfcxResources.AddStrings(const Str: String);
var
  i: Integer;
  sl: TWideStrings;
  nm, vl: WideString;
begin
{$IFDEF Delphi_10UP}
  sl := TfcxWideStrings.Create;
{$ELSE}
  sl := TWideStrings.Create;
{$ENDIF}
  sl.Text := Str;
  for i := 0 to sl.Count - 1 do
  begin
    nm := sl[i];
    vl := Copy(nm, Pos('=', nm) + 1, MaxInt);
    nm := Copy(nm, 1, Pos('=', nm) - 1);
    if (nm <> '') and (vl <> '') then
      Add(nm, vl);
  end;
  sl.Free;
end;

procedure TfcxResources.AddXML(const Str: AnsiString);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create({$IFDEF Delphi12}'', TEncoding.UTF8{$ELSE}Str{$ENDIF});
{$IFDEF Delphi12}
  Stream.Write(Str[1], Length(Str));
  Stream.Position := 0;
{$ENDIF}
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TfcxResources.Clear;
begin
  FNames.Clear;
  FValues.Clear;
end;

function TfcxResources.Get(const StrName: String): String;
var
  i: Integer;
begin
  if IsUTF8Application then
  begin
    i := FNames.IndexOf(StrName);
    if i <> -1 then
      Result := UTF8Encode(FValues[Integer(FNames.Objects[i])])
    else
      Result := StrName;
  {$IFDEF FPC_HAS_CPSTRING}
    SetCodePage(RawByteString(Result), DefaultSystemCodePage, False);
  {$ENDIF}
    if (Result <> '') and (Result[1] = '!') then
      Delete(Result, 1, 1);
  end
  else
    Result := GetAnsi(StrName);
end;

function TfcxResources.GetAnsi(const StrName: String): String;
var
  i: Integer;
begin
  i := FNames.IndexOf(StrName);
  if i <> -1 then
{$IFDEF Delphi12}
    Result :=  FValues[Integer(FNames.Objects[i])] else //_UnicodeToAnsi(FValues[Integer(FNames.Objects[i])], DEFAULT_CHARSET, FCP) else
    Result := StrName;
{$ELSE}
    Result := _UnicodeToAnsi(FValues[Integer(FNames.Objects[i])], DEFAULT_CHARSET, FCP) else
    Result := StrName;
{$ENDIF}
  if (Result <> '') and (Result[1] = '!') then
    Delete(Result, 1, 1);
end;

procedure TfcxResources.LoadFromFile(const FileName: String);
var
  f: TFileStream;
begin
  if FileExists(FileName) then
  begin
    f := TFileStream.Create(FileName, fmOpenRead);
    try
      LoadFromStream(f);
    finally
      f.Free;
    end;
  end;
end;

procedure TfcxResources.LoadFromStream(Stream: TStream);
var
  FXMLRes: TfcxXMLDocument;
  idx: Integer;
begin
  FXMLRes := TfcxXMLDocument.Create;
  FXMLRes.LoadFromStream(Stream);
  try
    with FXMLRes.Root do
    begin
      if Name = 'Resources' then
      begin
        FCP := StrToInt(Prop['CodePage']);
        for idx := 0 to  Count - 1 do
          if Items[idx].Name = 'StrRes' then
          {$IFDEF DELPHI12_UP}
            if not FXMLRes.OldVersion then
              Self.AddW(Items[idx].Prop['Name'], fcXMLToStr(Items[idx].Prop['Text']))
            else
          {$ENDIF}
              Self.AddW(Items[idx].Prop['Name'], UTF8Decode(fcXMLToStr(Items[idx].Prop['Text'])));
      end;
    end;
  finally
    FXMLRes.Free;
  end;
end;

procedure TfcxResources.Help(Sender: TObject);
begin
  // TODO: help
end;

procedure TfcxResources.BuildLanguagesList;
var
  i: Integer;
  SRec: TSearchRec;
  Dir: String;
  s: String;
begin
  Dir := GetAppPath;
  FLanguages.Clear;
  i := FindFirst(Dir + '*.xml', faAnyFile, SRec);
  try
    while i = 0 do
    begin
      s := LowerCase(SRec.Name);
      s := UpperCase(Copy(s, 1, 1)) + Copy(s, 2, Length(s) - 1);
      s := StringReplace(s, '.xml', '', []);
      FLanguages.Add(s);
      i := FindNext(SRec);
    end;
    FLanguages.Sort;
  finally
    FindClose(Srec);
  end;
end;


function fcxResources: TfcxResources;
begin
  if FResources = nil then
    FResources := TfcxResources.Create;
  Result := FResources;
end;

function fcxGet(ID: Integer): String;
begin
  Result := fcxResources.Get(IntToStr(ID));
end;

initialization

finalization
  FreeAndNil(FResources);
end.
