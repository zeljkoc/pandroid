{*******************************************************}
{                                                       }
{            FastCube 2 String utilities unit           }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxStringUtils;
{$INCLUDE fcx.inc}

interface
uses
  Classes, SysUtils
{$IFDEF Delphi10}
  , WideStrings
{$ENDIF}
{$IFDEF Delphi_9UP}
  , WideStrUtils
{$ENDIF}
{$IFDEF FPC}
  , LCLType
{$ELSE}
  , Windows
{$ENDIF}
, fcxTypes
  ;
//FMX uses
{$ELSE}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils, System.WideStrings, System.WideStrUtils, 
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  FMX.fcxTypes;
{$ENDIF}

Type
{$ifdef UseAnsiString}
  TfcxStringList = TStringList;
{$else}
  {$ifdef UseWideString}
    {$ifdef Delphi_10UP}
      TfcxStringList = TWideStringList;
    {$else}
      TfcxStringList = TWideStrings;
    {$endif}
  {$else}
    {$ifdef UseUnicodeString}
      TfcxStringList = TStringList;
    {$else}
      TfcxStringList = TStringList;
    {$endif}
  {$endif}
{$endif}

{$IFNDEF Delphi_9UP}
function WStrPCopy(ADest: PWideChar; const ASource: WideString): PWideChar;
{$ENDIF}
function fcStrPCopy(ADest: PfcxChar; const ASource: TfcxString): PfcxChar;
function fcStrCompare(const AStr1, AStr2: TfcxString): integer;
function fcStrUpper(const AStr: TfcxString): TfcxString;
function fcFindInfcStringListSorted(AList: TfcxStringList; const AStr: TfcxString; out AIndex: Integer): Boolean;

implementation

//VCL uses section
{$IFNDEF FMX}
{$IFDEF DELPHI_6UP}
uses
  Variants;
{$ENDIF}
//FMX uses
{$ELSE}
uses
  System.Variants;
{$ENDIF}

{$IFNDEF Delphi_9UP}
function WStrPCopy(ADest: PWideChar; const ASource: WideString): PWideChar;
var
  AWCSource: PWideChar;
  ALen: integer;
begin
  Result := ADest;
  ALen := Length(ASource);
  AWCSource := PWideChar(ASource);
  while (AWCSource^ <> #$00) and (ALen > 0) do
  begin
    ADest^ := AWCSource^;
    Inc(AWCSource);
    Inc(ADest);
    Dec(ALen);
  end;
  ADest^ := #$00;
end;
{$ENDIF}

function fcFindInfcStringListSorted(AList: TfcxStringList; const AStr: TfcxString; out AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := fcStrCompare(AList[I], AStr);//CompareStrings(FList^[I].FString, S);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  AIndex := L;
end;

function fcStrPCopy(ADest: PfcxChar; const ASource: TfcxString): PfcxChar;
begin
{$ifdef UseAnsiString}
  Result := StrPCopy(ADest, ASource);
{$else}
  {$ifdef UseWideString}
    Result := WStrPCopy(ADest, ASource);
  {$else}
    {$ifdef UseUnicodeString}
      Result := StrPCopy(ADest, ASource);
    {$else}
      Result := StrPCopy(ADest, ASource);
    {$endif}
  {$endif}
{$endif}
end;

function fcStrCompare(const AStr1, AStr2: TfcxString): integer;
{$IFNDEF FMX}
begin
{$ifdef UseAnsiString}
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PfcxChar(AStr1),
      -1, PfcxChar(AStr2), -1) - 2;
{$else}
  {$ifdef UseWideString}
    Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PfcxChar(AStr1),
      -1, PfcxChar(AStr2), -1) - 2
  {$else}
    {$ifdef UseUnicodeString}
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PfcxChar(AStr1),
          -1, PfcxChar(AStr2), -1) - 2;
    {$else}
      {$ifdef fpc}
        Result := AnsiCompareText(AStr1, AStr2);
      {$else}
        Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PfcxChar(AStr1),
            -1, PfcxChar(AStr2), -1) - 2;
      {$endif}
    {$endif}
  {$endif}
{$endif}
end;
{$ELSE FMX}
begin
  {$ifdef fpc}
    Result := AnsiCompareText(AStr1, AStr2);
  {$else}
    {$IFDEF MSWINDOWS}
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PfcxChar(AStr1),
        -1, PfcxChar(AStr2), -1) - 2;
    {$ELSE MSWINDOWS}
      Result := AnsiCompareText(AStr1, AStr2);
    {$ENDIF MSWINDOWS}
  {$endif}
end;
{$ENDIF FMX}

function fcStrUpper(const AStr: TfcxString): TfcxString;
{$IFNDEF FMX}
begin
{$ifdef UseAnsiString}
  Result := CharUpperA(PfcxChar(AStr));
{$else}
  {$ifdef UseWideString}
    Result := CharUpperW(PfcxChar(AStr));
  {$else}
    {$ifdef UseUnicodeString}
      Result := CharUpper(PfcxChar(AStr));
    {$else}
      {$ifdef fpc}
        Result := AnsiStrUpper(PfcxChar(AStr));
      {$else}
        Result := CharUpper(PfcxChar(AStr));
      {$endif}
    {$endif}
  {$endif}
{$endif}
end;
{$ELSE FMX}
begin
  {$ifdef fpc}
    Result := AnsiStrUpper(PfcxChar(AStr));
  {$else}
    {$IFDEF MSWINDOWS}
      Result := CharUpper(PfcxChar(AStr));
    {$ELSE MSWINDOWS}
      Result := AnsiStrUpper(PfcxChar(AStr));
    {$ENDIF MSWINDOWS}
  {$endif}
end;
{$ENDIF FMX}

end.
