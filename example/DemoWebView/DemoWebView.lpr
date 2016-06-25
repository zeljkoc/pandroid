{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoWebView/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoWebView;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demowebview}

interface

uses androidr15, Rjava, AActivity;


type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;
 wv : AWWebView;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  wv := AWWebView.Create(self);
  wv.loadData(JLString('<html><body><h1>Hello, WebView</h1>' +
    '<h1>Heading 1</h1><p>This is a sample paragraph.</p>' +
    '</body></html>'),JLString('text/html'), JLString('UTF-8'));
  wv.getSettings.setJavaScriptEnabled(true);

  wv.getSettings.setLoadWithOverviewMode(true);
  wv.getSettings.setUseWideViewPort(true);
  wv.setScrollBarStyle(AWWebView.SCROLLBARS_OUTSIDE_OVERLAY);
  wv.setScrollbarFadingEnabled(false);
  wv.getSettings.setBuiltInZoomControls(true);

  layout.addView(wv);


  setContentView(layout);    
end;


end.
