{*******************************************************}
{                                                       }
{                FastCube 2 Code unit                   }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxCodeUtils;

interface

{$I fcx.inc}

uses
  Classes, SysUtils, TypInfo
{$IFDEF Delphi6}
, Variants
{$ENDIF};
//FMX uses
{$ELSE FMX}

interface

{$I fcx.inc}

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Variants;
{$ENDIF FMX}

procedure fcxGetEventHandlersList(Code: TStrings; const Language: String;
  EventType: PTypeInfo; List: TStrings);
function fcxLocateEventHandler(Code: TStrings; const Language,
  EventName: String; EventType: PTypeInfo): Integer;
function fcxLocateMainProc(Code: TStrings; const Language: String): Integer;
function fcxAddEvent(Code: TStrings; const Language: String;
  EventType: PTypeInfo; const EventName: String): Integer;
function fcxAddEventWCode(Code: TStrings; const Language: String;
  EventType: PTypeInfo; const EventName: String; const VarSection: String;
  const EventCode: String): Integer;
function fcxReplaceEvent(Code: TStrings; const Language: String;
  EventType: PTypeInfo; const EventName: String; const VarSection: String;
  const EventCode: String): Integer;
procedure fcxEmptyCode(Code: TStrings; const Language: String);
procedure fcxAddCodeRes;

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxRes;
//FMX uses
{$ELSE FMX}
uses
  FMX.fcxRes;
{$ENDIF FMX}

procedure fcxAddCodeRes;
begin
  with fcxResources do
  begin
    Add('PascalScript',
      'proc="procedure" begin="begin" end="end;" lastend="end."');
    Add('C++Script',
      'proc="void" begin="{" end="}" lastend="}"');
    Add('BasicScript',
      'proc="sub" begin="" end="end sub" lastend=""');
    Add('JScript',
      'proc="function" begin="{" end="}" lastend=""');

    Add('TfcxGetValue',
      'PascalScript=(var Result: Variant);' + #13#10 +
      'C++Script=(Variant &Result)' + #13#10 +
      'BasicScript=(Result)' + #13#10 +
      'JScript=(Result)');
    Add('TfcxGetValue2',
      'PascalScript=(var Result: Variant; const Final: Boolean);' + #13#10 +
      'C++Script=(Variant &Result, bool Final)' + #13#10 +
      'BasicScript=(Result, Final)' + #13#10 +
      'JScript=(Result, Final)');
  end;
end;

function GetLangParam(const Language, Param: String): String;
var
  s: String;
  i: Integer;
begin
  Result := '';
  s := fcxResources.Get(Language);
  if s = Language then Exit;

  i := Pos(AnsiUppercase(Param) + '="', AnsiUppercase(s));
  if (i <> 0) and ((i = 1) or (s[i - 1] = ' ')) then
  begin
    Result := Copy(s, i + Length(Param + '="'), MaxInt);
    Result := Copy(Result, 1, Pos('"', Result) - 1);
  end;
end;

function GetEventParams(EventType: PTypeInfo; const Language: String): String;
var
  s: String;
  sl: TStringList;
begin
  Result := '';
  s := fcxResources.Get(EventType.Name);
  if s = EventType.Name then Exit;

  sl := TStringList.Create;
  sl.Text := s;
  Result := sl.Values[Language];
  sl.Free;
end;

procedure fcxGetEventHandlersList(Code: TStrings; const Language: String;
  EventType: PTypeInfo; List: TStrings);
var
  i: Integer;
  s, EventName, EventWord, EventParams: String;
begin
  List.Clear;
  EventParams := GetEventParams(EventType, Language);
  EventWord := AnsiUppercase(GetLangParam(Language, 'proc'));

  for i := 0 to Code.Count - 1 do
  begin
    s := Code[i];
    if Pos(EventWord, AnsiUppercase(s)) = 1 then
    begin
      { delete the "procedure" word }
      Delete(s, 1, Length(EventWord));
      { extract the event name and params }
      EventName := Trim(Copy(s, 1, Pos('(', s) - 1));
      s := Trim(Copy(s, Pos('(', s), 255));
      { compare the params }
      if AnsiCompareText(s, EventParams) = 0 then
        List.Add(EventName);
    end;
  end;
end;

function fcxLocateEventHandler(Code: TStrings; const Language,
  EventName: String; EventType: PTypeInfo): Integer;
var
  i: Integer;
  s, EventWord, EventParams: String;
begin
  Result := -1;
  EventParams := GetEventParams(EventType, Language);
  EventWord := AnsiUppercase(GetLangParam(Language, 'proc') + ' ' + EventName + '(');

  for i := 0 to Code.Count - 1 do
  begin
    s := Code[i];
    if Pos(EventWord, AnsiUppercase(s)) = 1 then
    begin
      Delete(s, 1, Length(EventWord));
      if AnsiCompareText(Trim('('+s), EventParams) = 0 then
      begin
        Result := i;
        break;
      end
    end;
  end
end;

function fcxLocateMainProc(Code: TStrings; const Language: String): Integer;
var
  i, endCount: Integer;
  s, BeginStr, EndStr: String;
begin
  Result := -1;
  if (Code.Count = 0) or ((Code.Count = 1) and (Code[0] = '')) then
    fcxEmptyCode(Code, Language);
  BeginStr := GetLangParam(Language, 'begin');
  EndStr := GetLangParam(Language, 'lastend');
  if EndStr = '' then
  begin
    Result := Code.Count - 1;
    Exit;
  end;

  i := Code.Count - 1;
  while i >= 0 do
  begin
    s := AnsiUpperCase(Code[i]);
    Dec(i);
    if Pos(AnsiUpperCase(EndStr), s) <> 0 then
      break;
  end;

  if i < 0 then Exit;

  EndStr := GetLangParam(Language, 'end');
  endCount := 1;
  while (i >= 0) and (endCount <> 0) do
  begin
    s := AnsiUpperCase(Code[i]);
    if Pos(AnsiUpperCase(EndStr), s) <> 0 then
      Inc(endCount);
    if Pos(AnsiUpperCase(BeginStr), s) <> 0 then
      Dec(endCount);
    Dec(i);
  end;

  Result := i + 1;
end;

function fcxAddEvent(Code: TStrings; const Language: String;
  EventType: PTypeInfo; const EventName: String): Integer;
var
  MainProcIndex: Integer;
begin
  MainProcIndex := fcxLocateMainProc(Code, Language);
  if MainProcIndex = -1 then
    raise Exception.Create(fcxResources.Get('dsCantFindProc'));

  Code.Insert(MainProcIndex, GetLangParam(Language, 'proc') + ' ' + EventName +
    GetEventParams(EventType, Language));
  Code.Insert(MainProcIndex + 1, GetLangParam(Language, 'begin'));
  Code.Insert(MainProcIndex + 2, '');
  Code.Insert(MainProcIndex + 3, GetLangParam(Language, 'end'));
  Code.Insert(MainProcIndex + 4, '');
  Result := MainProcIndex + 3;
end;

function fcxAddEventWCode(Code: TStrings; const Language: String;
  EventType: PTypeInfo; const EventName: String; const VarSection: String;
  const EventCode: String): Integer;
var
  MainProcIndex, i, varcount: Integer;
  CodeStrList: TStringList;
begin
  MainProcIndex := fcxLocateMainProc(Code, Language);
  if MainProcIndex = -1 then
    raise Exception.Create(fcxResources.Get('dsCantFindProc'));
  CodeStrList := TStringList.Create;
  Code.Insert(MainProcIndex, GetLangParam(Language, 'proc') + ' ' + EventName +
    GetEventParams(EventType, Language));
  varcount := 0;
  if VarSection <> '' then
  begin
    CodeStrList.Text := VarSection;
    for i := 0 to CodeStrList.Count - 1 do
      Code.Insert(MainProcIndex + 1 + i, CodeStrList[i]);
    varcount := CodeStrList.Count;
  end;
  Code.Insert(MainProcIndex + 1 + varcount, GetLangParam(Language, 'begin'));
  CodeStrList.Text := EventCode;
  for i := 0 to CodeStrList.Count - 1 do
    Code.Insert(MainProcIndex + 2 + varcount + i, CodeStrList[i]);
  Code.Insert(MainProcIndex + 2 + varcount + CodeStrList.Count, GetLangParam(Language, 'end'));
  Code.Insert(MainProcIndex + 3 + varcount + CodeStrList.Count, '');
  Result := MainProcIndex + 2 + varcount + CodeStrList.Count;
  CodeStrList.Free;
end;

function fcxReplaceEvent(Code: TStrings; const Language: String;
  EventType: PTypeInfo; const EventName: String; const VarSection: String;
  const EventCode: String): Integer;
begin
{todo}
end;

procedure fcxEmptyCode(Code: TStrings; const Language: String);
begin
  Code.Clear;
  if GetLangParam(Language, 'lastend') <> '' then
  begin
    Code.Add(GetLangParam(Language, 'begin'));
    Code.Add('');
    Code.Add(GetLangParam(Language, 'lastend'));
  end;
end;



end.
