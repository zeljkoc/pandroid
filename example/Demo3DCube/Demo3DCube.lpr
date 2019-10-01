{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/Demo3DCube/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit Demo3DCube;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demo3dcube}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity, MyView;


type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onResume; overload; override;
    procedure onPause; overload; override;
  private
     // glView: AOGLSurfaceView;
      glView: MyGLSurfaceView;
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
begin
  inherited onCreate(savedInstanceState);
   // Go fullscreen
 // requestWindowFeature(AVWindow.FEATURE_NO_TITLE);
 // getWindow().setFlags(AVWindowManager.InnerLayoutParams.FLAG_FULLSCREEN,
 // AVWindowManager.InnerLayoutParams.FLAG_FULLSCREEN);

 // glView := AOGLSurfaceView.create(Self);
 // glView.setRenderer(MyGLRenderer.Create(Self));
  glView := MyGLSurfaceView.create(Self);
  setContentView(glView);
end;

procedure MainActivity.onResume;
begin
  inherited onResume;
  glView.onResume();
end;

procedure MainActivity.onPause;
begin
  inherited onPause;
  glView.onPause();
end;



end.
