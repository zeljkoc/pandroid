{******************************************}
{                                          }
{        FastCube 2 Chm help viewer        }
{                                          }
{         Copyright (c) 1998-2011          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxChm;

interface

{$I fcx.inc}

uses Windows;

procedure frxDisplayHHTopic(Handle: THandle; const topic: String);


implementation

const
  HH_DISPLAY_TOPIC  = $0000;
  HH_DISPLAY_TOC    = $0001;

type
  THtmlHelpA = function(hwndCaller: THandle; pszFile: PChar;
    uCommand: Cardinal; dwData: Longint): THandle; stdcall;

var
  HtmlHelpA: THtmlHelpA = nil;
  OCXHandle: THandle = 0;

function HtmlHelpInstalled: Boolean;
begin
  Result := (Assigned(HtmlHelpA));
end;

procedure frxDisplayHHTopic(Handle: THandle; const topic: String);
begin
  if (Assigned(HtmlHelpA)) then
    HtmlHelpA(Handle, PChar(topic), HH_DISPLAY_TOC, 0);
end;


initialization
  HtmlHelpA := nil;
  OCXHandle := LoadLibrary('HHCtrl.OCX');
  if (OCXHandle <> 0) then
    HtmlHelpA := GetProcAddress(OCXHandle, 'HtmlHelpA');

finalization
  if (OCXHandle <> 0) then
    FreeLibrary(OCXHandle);
end.
