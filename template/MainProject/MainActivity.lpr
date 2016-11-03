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

uses androidr15, Rjava, AActivity, IniFile;

var
   IniFileName : JLString;
   ini: ZCTIniFile;
   
   UserName: JLString;
 

type
  #ActivityName# = class(Activity)
  public 
    procedure onClick(aView: AVView); override; 
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    procedure onCreate(savedInstanceState: AOBundle); override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public
    procedure SetingsIniFile;
    procedure ReadIniFile;    
  end;

implementation

uses AZCForms, AZCDialogs, AZCEditFile;

procedure #ActivityName#.onCreate(savedInstanceState: AOBundle);
var
  layout : AZCForm; 
begin
  inherited onCreate(savedInstanceState);
   layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic');   
  
  //================= Initialize
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/inifiles.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile; 
  //=============================

  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);
  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);    
end;



procedure #ActivityName#.onClick(aView: AVView);
begin
 case aView.getId of
    10: Finish;
 end;
         
end;

procedure #ActivityName#.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin
  //inherited onClick(dInterface, p1);
  ReadIniFile;
end;


//============
function #ActivityName#.onCreateOptionsMenu(menu: AVMenu): JBoolean;
var
 MenuItem : AVMenuItem;
 SubMenu : AVSubMenu; 
begin
  inherited onCreateOptionsMenu(menu);

  SubMenu := menu.addSubMenu(0, 0, 0, JLString(string('Meni')));
  SubMenu.add(0, 1, 0, JLString('Edit ini file')).setIcon(R.drawable.ic_next);

  MenuItem := SubMenu.getItem;
  MenuItem.setIcon(r.drawable.ic_menu); 
  MenuItem.setShowAsAction(AVMenuItem.SHOW_AS_ACTION_ALWAYS);   

  Result := true;
end;

function #ActivityName#.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
   Result := true;
   case item.getItemID of
      1: begin
         With AEditFile.create(Self , IniFileName) do
          show;

      end else Result := false;
   end;
end;

procedure #ActivityName#.SetingsIniFile;
begin
    ini.setString(JLString('PDAsetings'), JLString('UserName'),
       ini.getString(JLString('PDAsetings'), JLString('UserName'), JLString('SYSDBA')) );
   ReadIniFile;
end;

procedure #ActivityName#.ReadIniFile;
begin
  UserName  := ini.getString(JLString('PDAsetings'), JLString('UserName'), JLString('SYSDBA'));
end;


end.
