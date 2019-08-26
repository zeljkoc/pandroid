{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit #AppName#;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace #JavaPackageName#}

interface
{$include /usr/local/pandroid/units/AndroidVersion.inc}  
, Rjava, AActivity, StdCtrls;

 

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
