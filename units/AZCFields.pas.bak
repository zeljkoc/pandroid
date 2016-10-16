{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 28-8-15
***********************************************************}
unit AZCFields;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses
  Androidr15;

type
   { ADBTextView }

   ADBTextView = class(AWTextView)
   private
    fCursor: ADCursor;
    fVisible: boolean;
    procedure SetVisible(aValue: boolean);
   public
    property Cursor: ADCursor read fCursor write fCursor;
    property Visible: boolean read fVisible write SetVisible;
   end;

   ADBEditText = class(AWEditText)

   end;

implementation


{ ADBTextView }

procedure ADBTextView.SetVisible(aValue: boolean);
begin
  fVisible := aValue;
  if fVisible then
   setVisibility(AVView.VISIBLE)
  else setVisibility(AVView.INVISIBLE);
end;

end.

