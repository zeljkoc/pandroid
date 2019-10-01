{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit PascalScript;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.pascalscript}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity, APascalScript;

 

type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
  public
     Script: ZCPPascalScript;
     et: AWEditText;
     tv: AWTextView;
     mess: AWTextView;
  end;

implementation

const
  sc = 'program test; '+#10+
       'Var '+#10+
       '    i: integer; '+#10+
       'begin '+#10+
       ' for i:= 0 to 30 do  '+ #10 +
       '  Writeln(IntToStr(i)); '+#10+
       ' '+#10+
       'end. ';


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout;
  bt: AWButton;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

   bt:= AWButton.create(Self);
   bt.setId(1);
   bt.setText(JLString('Execute!'));
   bt.setOnClickListener(Self);
  layout.addView(bt);

   et:= AWEditText.Create(self);
   et.setText(JLString(sc));
  layout.addView(et);

   tv:= AWTextView.create(self);
  layout.addView(tv);

    mess:= AWTextView.create(self);
  layout.addView(mess);

   Script:= ZCPPascalScript.create();


  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);
begin
  case aView.getId of
     1: begin
          tv.setText(Script.Execute(et.getText.toString).toString);
          mess.setText(Script.Message.toString);
     end;
  end;

end;

end.
