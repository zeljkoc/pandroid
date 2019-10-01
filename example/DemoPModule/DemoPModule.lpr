{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit DemoPModule;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demopmodule}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity, StdCtrls, PandroidModule;

 

type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure btOnClick(aView: AVView);
  public
    bt: TButton;
    tv: TTextView;
    pm: ZCPandroidModule;
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout; 
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

    tv:= TTextView.create(Self);
  layout.addView(tv);

    bt:= TButton.create(self);
    bt.Text := JLString('OK');
    bt.onClickListener := @btOnClick;
  layout.addView(bt);

  setContentView(layout);

  pm:= ZCPandroidModule.create;
end;

procedure MainActivity.btOnClick(aView: AVView);
var
  ID: jlong;
begin
  ID := pm.CreateObject('TDataM');

  //set property value
  pm.SetPropertyValue(ID, JLString('DatabaseName'), JLString('192.168.10.50:01Zeljus'));

  //Get property value
  tv.Text := pm.GetPropertyValue(ID, JLString('DatabaseName'))

 { ID := pm.CreateObject('TDataM');
  pm.SetPropertyValue(ID, JLString('Test'), JLString('test00001'));
  tv.Text := pm.GetPropertyValue(ID, JLString('Test'))}
end;

end.
