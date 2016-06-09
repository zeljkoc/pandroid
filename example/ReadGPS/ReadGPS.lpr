{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/ReadGPS/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit ReadGPS;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.readgps}

interface

uses androidr15, Rjava, AActivity;


type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
    procedure onBackPressed; overload; override;

          //
    procedure onResume; override;
    //ALLocationListener
    procedure onLocationChanged(loc: ALLocation); overload; override;
    procedure onStatusChanged(par1: JLString; par2: LongInt; par3: AOBundle); overload; override;
    procedure onProviderEnabled(par1: JLString); overload; override;
    procedure onProviderDisabled(par1: JLString);overload; override;
  public
    mWebView: AWWebView;
    lm: ALLocationManager;
    provider: JLString;
    location: ALLocation;

    tv: AWTextView;
  end;

implementation

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var layout: AWLinearLayout;
    bt: AWButton;

begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

    bt := AWButton.create(Self);
    bt.setId(1);
    bt.setOnClickListener(self);
    bt.setText(JLString('Set GPS'));
  layout.addView(bt);

    bt := AWButton.create(Self);
    bt.setId(2);
    bt.setOnClickListener(self);
    bt.setText(JLString('Locate'));
  layout.addView(bt);

     tv:= AWTextView.create(Self);
  layout.addView(tv);

    mWebView:= AWWebView.create(Self);
    mWebView.getSettings.setGeolocationEnabled(true);
     mWebView.getSettings.setBuiltInZoomControls(true);
    mWebView.setFilterTouchesWhenObscured(true);

   layout.addView(mWebView);

   lm := ALLocationManager(Self.getSystemService(LOCATION_SERVICE));

   provider := ALLocationManager.GPS_PROVIDER; // We want to use the GPS
   try
     // Initialize the location fields
     if lm.isProviderEnabled(provider) then begin
       location:= lm.getLastKnownLocation(provider);
     end;
  except
  end;

  setContentView(layout);
end;

procedure MainActivity.onClick(aView: AVView);
var
 intent: ACIntent;

 url : JLString;
begin
  case aView.getId of
    1: begin
         intent := ACIntent.Create(APSettings.ACTION_LOCATION_SOURCE_SETTINGS);
        startActivity(intent);
       end;

    2: begin
          url := JLString('http://maps.google.com/maps/api/staticmap?center=').
                concat(JLString.valueOf(location.getLatitude)).concat(', ').concat(JLString.valueOf(location.getLongitude)).
                concat('&zoom=15&size=512x512&maptype=roadmap&sensor=false');
          mWebView.loadUrl(url);
    end;
  end;
end;

procedure MainActivity.onBackPressed;
begin
  if mWebView.canGoBack then mWebView.goBack
  else  inherited onBackPressed;
end;

procedure MainActivity.onResume;
begin
  inherited onResume;
  lm.requestLocationUpdates(provider, 0, 0, self);
end;

procedure MainActivity.onLocationChanged(loc: ALLocation);
begin
  location := loc;
  tv.SetText(JLString(JLString.valueOf(location.getLatitude).concat(', ').concat(JLString.valueOf(location.getLongitude))));
end;

procedure MainActivity.onStatusChanged(par1: JLString; par2: LongInt; par3: AOBundle);
begin

end;

procedure MainActivity.onProviderEnabled(par1: JLString);
begin

end;

procedure MainActivity.onProviderDisabled(par1: JLString);
begin

end;



end.
