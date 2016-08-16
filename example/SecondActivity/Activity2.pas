{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
unit Activity2;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.secondactivity}

interface

uses androidr15, Rjava, AActivity;

type

  { FormActivity }

  FormActivity = class(Activity)
    public
      procedure onCreate(savedInstanceState:AOBundle);override;
  end;

implementation

{ FormActivity }

procedure FormActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  AWToast.makeText(self, getIntent.getStringExtra(JLString('str1')), AWToast.LENGTH_LONG).show();

  setContentView(layout);
end;

end.

