{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit #AppName#;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace #JavaPackageName#}

interface

uses androidr15, Rjava, AActivity;

 

type
  #ActivityName# = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;    
  end;

implementation


procedure #ActivityName#.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout; 
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  setContentView(layout);    
end;

end.
