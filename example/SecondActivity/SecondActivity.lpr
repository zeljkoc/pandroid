{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit SecondActivity;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.secondactivity}

interface

uses androidr15, Rjava, AActivity;

 

type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
  end;

implementation
uses Activity2;


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout;
  bt: AWButton;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

     bt:= AWButton.create(self);
     bt.setID(1);
     bt.setText(JLString('prvi'));
     bt.setOnClickListener(Self);
  layout.addView(bt);

     bt:= AWButton.create(self);
     bt.setID(2);
     bt.setText(JLString('drugi'));
     bt.setOnClickListener(Self);
  layout.addView(bt);

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);
var
  intent:ACIntent;
begin
  case aView.getId of
    1: begin
      intent:=ACIntent.Create(Self, JLClass(FormActivity));
      intent.putExtra(JLString('str1'), JLString('promjenljiva 1'));
      startActivity(intent);
    end;

    2: begin
      intent:=ACIntent.Create(Self, JLClass(FormActivity));
      intent.putExtra(JLString('str1'), JLString('promjenljiva 2'));
      startActivity(intent);
    end;
  end;
end;

end.
