{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoScrollButton/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoScrollButton;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.scrollbutton}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity;

type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
  public

  end;

implementation

uses AZCDialogs;


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
   layout: AWLinearLayout;
   scrollView: AWScrollView;
   horizontalScropllView : AWHorizontalScrollView;
   layout1: AWLinearLayout;

   bt: AWButton;
   i : integer;
begin
 inherited onCreate(savedInstanceState);
 layout:= AWLinearLayout.create(Self);
 layout.setOrientation(AWLinearLayout.VERTICAL);
   scrollView:= AWScrollView.Create(Self);
     horizontalScropllView := AWHorizontalScrollView.Create(self);
       layout1:= AWLinearLayout.Create(Self);

         for i:=0 to 20 do begin
            bt:= AWButton.Create(Self);
            bt.setId(i);
            bt.setText(JLString('Button: ').concat(JLInteger.toString(i)));
            bt.setOnClickListener(Self);
          layout1.addView(bt);
         end;

     horizontalScropllView.addView(layout1);
   scrollView.addView(horizontalScropllView);
 layout.addView(scrollView);

 setContentView(layout);
end;

procedure MainActivity.onClick(aView: AVView);  
begin
 ShowMessage(Self, JLString('Button ').concat(JLInteger.toString(aView.getId)));
end;


end.
