{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit StdCtrl;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.stdctrl}

interface

uses androidr15, Rjava, StdCtrls;

 

type

  { MainActivity }

  MainActivity = class(AAActivity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
  private
    bt: TButton;
    et: TEditText;
    procedure BtClickListener(aView: AVView);
    function BtLongClickListener(aView: AVView): boolean;
    procedure etChangeText(para1: JLObject);
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

    bt:= TButton.create(Self);
    bt.ID := 1;
    bt.Text := JLString('OK');
    bt.onClickListener := @btClickListener;
    bt.OnLongClickListener := @BtLongClickListener;
  layout.addView(bt);

    et:= TEditText.create(Self);
    et.Text := JLString('eeee');
    et.onChangeText := @etChangeText;
  layout.addView(et);

  setContentView(layout);    
end;

procedure MainActivity.BtClickListener(aView: AVView);
begin
   et.Text := JLString('Click button');
end;

function MainActivity.BtLongClickListener(aView: AVView): boolean;
begin
  et.Text := JLString('Long click button');
  Result := True;
end;

procedure MainActivity.etChangeText(para1: JLObject);
begin
  bt.Text := TEditText(para1).Text;
end;

end.
