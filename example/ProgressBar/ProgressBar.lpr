{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/ProgressBar/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit ProgressBar;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.progressbar}

interface

uses androidr15, Rjava, AActivity;


type

  { MainActivity }

  MainActivity = class(Activity)
  public
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
    function handleMessage(eMessage: AOMessage): Boolean; override;
  public
    btnStartProgress: AWButton;
    mProgressDialog:AAProgressDialog;
    mProgress:jint;
    mProgressHandler:AOHandler;

  end;

const
  MAX_PROGRESS = 100;

implementation



procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
layout : AWLinearLayout;

begin
  inherited onCreate(savedInstanceState);
  layout :=  AWLinearLayout.create(Self);
  layout.setOrientation(AWLinearLayout.HORIZONTAL);

  mProgressHandler:=AOHandler.create(self);

    btnStartProgress:= AWButton.create(Self);
    btnStartProgress.setText(JLString('Progress bar'));
    btnStartProgress.setId(1);
    btnStartProgress.setOnClickListener(Self);
  layout.addView(btnStartProgress);



  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);
var
 builder: AAAlertDialog.InnerBuilder;
begin
 case aView.getId of
      1: begin
        mProgressDialog:= AAProgressDialog.create(self);
       // mProgressDialog.setIcon(R.drawable.alert_dialog_icon);
        mProgressDialog.setTitle(JLString('Dialog progress bar'));
        mProgressDialog.setProgressStyle(AAProgressDialog.STYLE_HORIZONTAL);
       // mProgressDialog.setProgressStyle(AAProgressDialog.STYLE_SPINNER);

        mProgressDialog.setMax(MAX_PROGRESS);
        mProgressDialog.setButton(JLString('OK'), self);
       // mProgressDialog.setButton2(JLString('Cancel'),self);
        mProgressDialog.show;

       mProgress:= 0;
       mProgressDialog.setProgress(0);
       mProgressHandler.sendEmptyMessage(0);
      end;
 end;
         
end;

function MainActivity.handleMessage(eMessage: AOMessage): Boolean;
begin
  if (mProgress >= MAX_PROGRESS) then
      mProgressDialog.dismiss()
  else begin
      mProgress:=+mProgress;
      mProgressDialog.incrementProgressBy(1);
      mProgressHandler.sendEmptyMessageDelayed(0, 100);
   end;
end;




end.
