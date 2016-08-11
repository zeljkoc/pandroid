{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoToolBar2/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoToolBar2;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demotoolbar2}

interface

uses androidr15, Rjava, AActivity, AZCToolBar;

type

  { MainActivity }

  MainActivity = class(Activity)
  public
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;

  public
    ToolBar : TZCToolBar;
    function ObjectView(count: Jint): AVView;
  end;

implementation

uses AZCDialogs;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;

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
  //   setNumColumnsGridView(1); // AWGridView.AUTO_FIT);
   end;

   for i:=0 to 30 do begin
      ToolBar.add(ObjectView(i));
   end;

  layout.addView(ToolBar);


  setContentView(layout);
end;

procedure MainActivity.onClick(aView: AVView);
begin
  ShowMessage(Self, JLString('Edit text: ').concat( (AWLinearLayout(ToolBar.Items.get(aView.getId)).getChildAt(1) as AWEditText).getText.toString) );
end;

function MainActivity.ObjectView(count: Jint): AVView;
var
 layout: AWLinearLayout;    //AWLinearLayout Items.get ...

 tv: AWTextView;
 et: AWEditText;
 bt: AWButton;
begin
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.HORIZONTAL);

      tv := AWTextView.create(Self);        //child 0
      tv.setWidth(150);
      tv.setText(JLString('Line: ').concat(JLInteger.toString(count) ));
   layout.addView(tv);

      et := AWEditText.create(Self);      //child 1
      et.setWidth(200);
      et.setText(JLString('text: ').concat(JLInteger.toString(count)));
   layout.addView(et);
                                         //child 2
      bt:= AWButton.create(Self);
      bt.setId(count);
      bt.setText(JLString('...'));
      bt.setOnClickListener(self);
  layout.addView(bt);

  Result := layout;
end;


end.
