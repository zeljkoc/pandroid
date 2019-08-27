{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit PandroidRadiPlayer;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.pandroidradiplayer}

interface

//uses androidr15
{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity, StdCtrls;

 

type

   { TPhonePlayer }

   TPhonePlayer = class(ATPhoneStateListener)
     FContext : ACContext;
     procedure onCallStateChanged(para1: jint; para2: JLString); overload; override;
   public
     property Context: ACContext read FContext write FContext;
   public

  end;

  { MainActivity }

  MainActivity = class(Activity)
    FPlayer : AMMediaPlayer;
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
  protected
     procedure Play(aUrl: JLString);
     procedure StopPlay;
     procedure StartPlay;
  public
    TelephonyManager : ATTelephonyManager;
    PhonePlayer: TPhonePlayer;
    lastURL: JLString;
  end;

implementation

uses AZCForms;

{ TPhonePlayer }

procedure TPhonePlayer.onCallStateChanged(para1: jint; para2: JLString);
begin
 // inherited onCallStateChanged(para1, para2);
 if (FContext is MainActivity) then
   case para1 of
    ATTelephonyManager.CALL_STATE_RINGING,
    ATTelephonyManager.CALL_STATE_OFFHOOK: (FContext as MainActivity).StopPlay;

    ATTelephonyManager.CALL_STATE_IDLE: (FContext as MainActivity).StartPlay;
  end;

end;


procedure MainActivity.Play(aUrl: JLString);
begin
  try
    lastURL := aUrl;
    if FPlayer.isPlaying then FPlayer.stop;
    FPlayer.reset;
    FPlayer.setDataSource(aUrl);
    FPlayer.prepare;
    FPlayer.start;
  except
  end;
end;

procedure MainActivity.StartPlay;
begin
  try
   if (lastURL.length <> 0) then Play(LastUrl);
  except
  end;
end;

procedure MainActivity.StopPlay;
begin
    try
     if FPlayer.isPlaying then FPlayer.stop;
  except
  end;
end;


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AZCForm;
begin
  inherited onCreate(savedInstanceState);
  layout := AZCForm.create(Self, R.drawable.logo, 'Zeljko Cvijanovic & Mgr. Janusz Chmiel');
  layout.HTMLTemplate := '<font color="#ff0000"> <i> <#Button> </i> </font> ';

  layout.addButtonImage(Self, 12, 'Radiožurnál', R.drawable.notes);
  layout.addButtonImage(Self, 13, 'Dvojka', R.drawable.notes);
  layout.addButtonImage(Self, 14, 'Vltava', R.drawable.notes);
  layout.addButtonImage(Self, 15, 'Plus', R.drawable.notes);
  layout.addButtonImage(Self, 16, 'Radio Wave', R.drawable.notes);
  layout.addButtonImage(Self, 17, 'D-dur', R.drawable.notes);
  layout.addButtonImage(Self, 18, 'Jazz', R.drawable.notes);
  layout.addButtonImage(Self, 19, 'Rádio Junior', R.drawable.notes);
  layout.addButtonImage(Self, 20, 'Radio Retro', R.drawable.notes);
  layout.addButtonImage(Self, 21, 'Rádio Junior Písničky', R.drawable.notes);
  layout.addButtonImage(Self, 22, 'Radio Praha', R.drawable.notes);
  layout.addButtonImage(Self, 23, 'Sport', R.drawable.notes);
  layout.addButtonImage(Self, 24, 'Brno', R.drawable.notes);
  layout.addButtonImage(Self, 25, 'České Budějovice', R.drawable.notes);
  layout.addButtonImage(Self, 26, 'Hradec Králové', R.drawable.notes);
  layout.addButtonImage(Self, 27, 'Olomouc', R.drawable.notes);
  layout.addButtonImage(Self, 28, 'Ostrava', R.drawable.notes);
  layout.addButtonImage(Self, 29, 'Pardubice', R.drawable.notes);
  layout.addButtonImage(Self, 30, 'Plzeň', R.drawable.notes);
  layout.addButtonImage(Self, 31, 'Karlovy Vary', R.drawable.notes);
  layout.addButtonImage(Self, 32, 'Regina', R.drawable.notes);
  layout.addButtonImage(Self, 33, 'Sever', R.drawable.notes);
  layout.addButtonImage(Self, 34, 'Liberec', R.drawable.notes);
  layout.addButtonImage(Self, 35, 'Region Střední Čechy', R.drawable.notes);
  layout.addButtonImage(Self, 36, 'Region Vysočina', R.drawable.notes);

  layout.addLine;
  layout.addButtonImage(Self, 10, 'Stop', R.drawable.notes);
  layout.addButtonImage(Self, 11, 'Start', R.drawable.notes);
  layout.addLine;
  layout.addButtonImage(Self, 100, 'EXIT', R.drawable.exit);

  FPlayer := AMMediaPlayer.create;
  FPlayer.setAudioStreamType(AMAudioManager.STREAM_MUSIC);

  TelephonyManager :=  ATTelephonyManager(getSystemService(ACContext.TELEPHONY_SERVICE));
  PhonePlayer:= TPhonePlayer.create;
  PhonePlayer.Context := Self;
  TelephonyManager.listen(PhonePlayer, ATPhoneStateListener.LISTEN_CALL_STATE);

  setContentView(layout);
  // java example
  //https://www.codota.com/code/java/classes/android.media.MediaPlayer
end;

procedure MainActivity.onClick(aView: AVView);
begin
  case aView.getId of
        10: begin
             StopPlay;
        end;

        12: Play('http://icecast8.play.cz/cro1-128.mp3');
        13: Play('http://icecast6.play.cz/cro2-128.mp3');
        14: Play('http://icecast5.play.cz/cro3-128.mp3');
        15: Play('http://icecast5.play.cz/crowave-128.mp3');
        16: Play('http://icecast5.play.cz/croddur-128.mp3');
        17: Play('http://icecast1.play.cz/crojazz128.mp3');
        18: Play('http://icecast5.play.cz/crojuniormaxi128.mp3');
        19: Play('http://icecast5.play.cz/crojuniormaxi128.mp3');
        20: Play('http://icecast7.play.cz/croretro128.mp3');
        21: Play('http://icecast7.play.cz/crojuniormini128.mp3');
        22: Play('http://icecast2.play.cz/cro7-128.mp3');
        23: Play('http://icecast7.play.cz/crosport128.mp3');
        24: Play('http://icecast2.play.cz/crobrno128.mp3');
        25: Play('http://icecast2.play.cz/crocb128.mp3');
        26: Play('http://icecast2.play.cz/crohk128.mp3');
        27: Play('http://icecast2.play.cz/crool128.mp3');
        28: Play('http://icecast2.play.cz/croov128.mp3');
        29: Play('http://icecast2.play.cz/cropardubice128.mp3');
        30: Play('http://icecast2.play.cz/croplzen128.mp3');
        31: Play('http://icecast2.play.cz/crokv128.mp3');
        32: Play('http://icecast2.play.cz/croregina128.mp3');
        33: Play('http://icecast2.play.cz/crosever128.mp3');
        34: Play('http://icecast2.play.cz/croliberec128.mp3');
        35: Play('http://icecast2.play.cz/croregion128.mp3');
        36: Play('http://icecast2.play.cz/crovysocina128.mp3');

    100: begin
           StopPlay;
           Finish;
         end;
    end;
end;

end.
