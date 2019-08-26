{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit ATimer;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

//uses androidr15
{$include AndroidVersion.inc}
, StdCtrls;

type
   TOnTick = procedure (para1: jlong) of object;
   TOnFinish = procedure of object;
    { TTimer }

    TZCTimer = class (AOCountDownTimer) //JUTimerTask)
    private
      fOnTick : TOnTick;
      fOnFinish: TOnFinish;
    public
     constructor create(para1: jlong; para2: jlong); overload;
     procedure onTick(para1: jlong); overload; override;
     procedure onFinish(); overload; override;

     property onTickListener : TOnTick write fOnTick;
     property onFinishListener : TOnFinish write fOnFinish;
    end;

implementation




{ TTimer }

constructor TZCTimer.create(para1: jlong; para2: jlong);
begin
  inherited Create(para1, para2);
end;

procedure TZCTimer.onTick(para1: jlong);
begin
  if Assigned(fOnTick) then fOnTick(para1);
end;

procedure TZCTimer.onFinish();
begin
  if Assigned(fOnFinish) then fOnFinish;
end;


end.

