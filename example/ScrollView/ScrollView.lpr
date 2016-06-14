{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/ScrollView/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit ScrollView;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.scrollview}

interface

uses androidr15, Rjava, AActivity;


type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
  public
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 scrollView: AWScrollView;
 horizontalScropllView : AWHorizontalScrollView;

 layout: AWLinearLayout;

 TableLayout: AWTableLayout;
 row: AWTableRow;
 lv : AWTextView;

 i, j: integer;
begin
  inherited onCreate(savedInstanceState);
  scrollView:= AWScrollView.Create(Self);
  horizontalScropllView := AWHorizontalScrollView.Create(self);
  layout:= AWLinearLayout.Create(Self);

  TableLayout:= AWTableLayout.create(Self);
  for i:= 0 to 10 do begin
    row:= AWTableRow.Create(Self);
     for j:=0 to 10 do begin
       lv := AWTextView.Create(Self);
       lv.setText(JLString(' |'+JLInteger.toString(i) +'-row-'+JLInteger.toString(j)+' '));
       row.addView(lv);
     end;


   TableLayout.addView(row);
  end;
  layout.addView(TableLayout);

  scrollView.addView(layout);
  horizontalScropllView.addView(scrollView);

  setContentView(horizontalScropllView);
end;


end.
