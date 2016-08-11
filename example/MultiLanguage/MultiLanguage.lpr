{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/MultiLanguage/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit MultiLanguage;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.multilanguage}

interface

uses androidr15, Rjava, AActivity, IniFile, AZCEditFile;


var
   IniFileName : JLString;
   ini: ZCTIniFile;

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

  public
    procedure SetingsIniFile;
    procedure ReadIniFile;    
  end;

implementation

uses AZCForms;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
layout : AZCForm;
 tv : AWTextView;
begin
  inherited onCreate(savedInstanceState);

  layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic');   
  
  //================= Initialize
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/inifiles.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile; 
  //=============================

    tv := AWTextView.create(self);
    tv.setText(R.string_.rec);
  layout.setView(tv);

  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);  
begin
 case aView.getId of
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
    ini.setInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'),
       ini.getInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'), 1) );
    ini.setInt(JLString('PDAsetings'), JLString('IDPDA'),
       ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1 ));  

   ReadIniFile;
end;

procedure MainActivity.ReadIniFile;
begin
  IDRadnoMjesto := ini.getInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'), 1);
  IDPDA :=  ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1);
end;  

end.
