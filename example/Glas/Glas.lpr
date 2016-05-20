{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 7-2-16 13:31:38 
***********************************************************}  
{%BuildWorkingDir /usr/local/pandroid/example/Glas/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit Glas;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.glas}

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

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override; 
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    procedure onDestroy; overload; override;
    //procedure onInit(status: jint); overload;
    procedure onInit(status: jint); overload; override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public
     db :  ASQLDatabase;
     mTts:ASTTextToSpeech;
     text: AWEditText;
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

    text:= AWEditText.create(Self);
    text.setText(JLString('Ok'));
  layout.setView(text);

  layout.addButton(Self, 1, 'Reci: ');

  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);

  mTts:= ASTTextToSpeech.create(self, self);

end;

procedure MainActivity.onClick(aView: AVView);  
begin
 case aView.getId of
    1:  mTts.speak(text.getText.toString, ASTTextToSpeech.QUEUE_FLUSH, nil);

      10: Finish;
 end;
         
end;

procedure MainActivity.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin
  //inherited onClick(dInterface, p1);
  ReadIniFile;
end;

procedure MainActivity.onDestroy;
begin
    if (mTts <> nil) then begin
    mTts.stop();
    mTts.shutdown();
  end;
  inherited onDestroy;
end;

procedure MainActivity.onInit(status: jint);
begin
    if (status = ASTTextToSpeech.SUCCESS) then begin
       mTts.setLanguage(JULocale.fUS);   // engleski
      //  mTts.setLanguage(JULocale.fUK); // ukraina
    end;
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
