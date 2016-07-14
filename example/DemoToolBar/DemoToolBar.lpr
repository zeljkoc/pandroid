{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoToolBar/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoToolBar;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demotoolbar}

interface

uses androidr15, Rjava, AActivity, AZCToolBar;

type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override; 

  public
    ToolBar : TZCToolBar;
  end;

implementation

uses AZCDialogs;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;

 bt: AWButton;
 et: AWEditText;
 i: integer;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  //----------------------------
   ToolBar:= TZCToolBar.Create(Self);
   with ToolBar do begin
     setVerticalSpacingGridView(10);
     setHorizontalSpacingGridView(10);
     setNumColumnsGridView(AWGridView.AUTO_FIT);
     setColumnWidthGridView(70);
   end;

   for i:=0 to 10 do begin
      bt:= AWButton.create(Self);
      bt.setId(i);
      bt.setText(JLString('Ok'));
      bt.setOnClickListener(self);
      ToolBar.add(bt);
   end;

   et := AWEditText.create(Self);
   et.setText(JLString('Proba'));
   ToolBar.add(et);


  layout.addView(ToolBar);


  setContentView(layout);
end;

procedure MainActivity.onClick(aView: AVView);  
begin
  ShowMessage(Self, JLInteger.toString(aView.getId));
end;


end.
