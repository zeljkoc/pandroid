{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/Demo2DDraw/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit Demo2DDraw;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demo2ddraw}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity;


type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
  public

  end;

  { MyView }

  MyView = class(AVView)
    constructor create(para1: ACContext); overload;
    procedure onDraw(canvas: AGCanvas); overload; override;
  end;


implementation

{ MyView }

constructor MyView.create(para1: ACContext);
begin
  inherited create(para1);
end;

procedure MyView.onDraw(canvas: AGCanvas);
var
  x,y, radius: integer;
 paint: AGPaint;
begin
  inherited onDraw(canvas);
  x := getWidth;
  y := getHeight;
  radius := 100;

  paint:= AGPaint.create();
  paint.setStyle(AGPaint.InnerStyle.fFILL);
  paint.setColor(AGColor.WHITE);
  canvas.drawPaint(paint);
  // Use Color.parseColor to define HTML colors
  paint.setColor(AGColor.parseColor(JLString('#CD5C5C')));
  canvas.drawCircle(x / 2, y / 2, radius, paint);

end;


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
begin
  inherited onCreate(savedInstanceState);
  setContentView(MyView.create(Self));
end;


end.
