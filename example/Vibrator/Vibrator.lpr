{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 8-2-16 07:26:57 
***********************************************************}  
{%BuildWorkingDir /usr/local/pandroid/example/Vibrator/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit Vibrator;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.vibrator}

interface

uses androidr15, Rjava, AActivity, IniFile, AZCEditFile, ADBDataBase;

var
   IniFileName : JLString;
   ini: ZCTIniFile;

   Server : JLString;
   Port : JLString;

   DataBase: JLString;
   UserName: JLString;

   IDRadnoMjesto: Integer; //= '10';
   IDPDA: Integer;    

type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override; 
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public
     db :  ASQLDatabase;
     v: AOVibrator;
  public
    procedure SetingsIniFile;
    procedure ReadIniFile;    
  end;

implementation

uses AZCForms;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
layout : AZCForm; 
begin
  inherited onCreate(savedInstanceState);
   layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic +387 65 458 401');   
  
  //================= Initialize
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/inifiles.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile; 
  //=============================

  layout.addButton(Self, 1, 'VIBRIRAJ');
  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);

 // Get instance of Vibrator from current Context
  v:= AOVibrator(getSystemService(ACContext.VIBRATOR_SERVICE));

end;

procedure MainActivity.onClick(aView: AVView);  
begin
 case aView.getId of
      1: v.vibrate(300); // Vibrate for 300 milliseconds
      10: Finish;
 end;
         
end;

procedure MainActivity.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin
  //inherited onClick(dInterface, p1);
  ReadIniFile;
end;

//-------------------------------------
function MainActivity.onCreateOptionsMenu(menu: AVMenu): JBoolean;
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

function MainActivity.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
var
  edit: AEditFile;
begin
   Result := true;
   case item.getItemID of
      1: begin
         edit := AEditFile.create(Self , IniFileName) ;
         edit.show;
      end else Result := false;
   end;
end;

procedure MainActivity.SetingsIniFile;
begin


   ReadIniFile;
end;

procedure MainActivity.ReadIniFile;
begin


end;  

end.
