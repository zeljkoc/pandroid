{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit AZCScrollButons;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15, Rjava;

type

    { AZCScroolButton }

    AZCScroolButton = class(AWLinearLayout)
    private
      fContext: ACContext;
      fButtons: JUArrayList;
      fHorizontalScropllView : AWHorizontalScrollView;
      fLinearLayout: AWLinearLayout;
    private
      procedure SetButtons(AValue: JUArrayList);
    public
      constructor create(para1: ACContext; aButonWidth: integer = 66); overload;
      procedure RefreshView;
    public
      property Buttons: JUArrayList read fButtons write SetButtons;
    end;

implementation

uses AZCDialogs;

{ AZCScroolButton }

procedure AZCScroolButton.SetButtons(AValue: JUArrayList);
begin
  if fButtons=AValue then Exit;
  fButtons:=AValue;
end;

constructor AZCScroolButton.create(para1: ACContext; aButonWidth: integer = 66);
var
  Params:  AWLinearLayout.InnerLayoutParams;
begin
  fContext := para1;
  inherited Create(fContext);

  fLinearLayout:= AWLinearLayout.create(fContext);

  fHorizontalScropllView := AWHorizontalScrollView.Create(fContext);

  Params:=  AWLinearLayout.InnerLayoutParams.Create(
                AWLinearLayout.InnerLayoutParams.WRAP_CONTENT ,  aButonWidth);  //sirina button-a
  addView(fHorizontalScropllView, AVViewGroup_LayoutParams(params));

  fButtons := JUArrayList.create;
end;

procedure AZCScroolButton.RefreshView;
var
  i: integer;
  bt: AWButton;
begin
  fLinearLayout.removeAllViews;
  fHorizontalScropllView.removeAllViews;

  for i:=0 to fButtons.size - 1 do begin

    fLinearLayout.addView(AVView(fButtons.get(i)) );
  end;
  fHorizontalScropllView.addView(fLinearLayout);

end;

end.

