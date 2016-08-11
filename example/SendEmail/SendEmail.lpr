{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/SendEmail/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit SendEmail;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.sendemail}

interface

uses androidr15, Rjava, AActivity, IniFile, AZCEditFile;


var
   IniFileName : JLString;
   ini: ZCTIniFile;

   IDRadnoMjesto: Integer; //= '10';
   IDPDA: Integer;    

type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override; 
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
    //intent
    function onActivityResult(requestCode: LongInt; resultCode: LongInt; data: ACIntent): Boolean; override;
  public
   const
      INTENT_SEND_EMAIL = 1;
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

  layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic');   
  
  //================= Initialize
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/inifiles.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile; 
  //=============================

  layout.addButton(Self, 1, 'Send E-mail');

  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);
var
 email: ACIntent;
begin
 case aView.getId of
      1: begin
           Email:= ACIntent.Create(ACIntent.ACTION_SEND);
           email.putExtra(ACIntent.EXTRA_EMAIL, JLString('temp@gmail.com'){ to});
          // email.putExtra(ACIntent.EXTRA_CC, JLString(''){ to});
           //email.putExtra(ACIntent.EXTRA_BCC, new String[]{to});
           email.putExtra(ACIntent.EXTRA_SUBJECT, JLString('Predmet: poruka'){subject});
           email.putExtra(ACIntent.EXTRA_TEXT, JLString('Hello World!!!') {message});
           email.setType(JLString('message/rfc822'));

          // startService(email);
           startActivityForResult(email, INTENT_SEND_EMAIL);
      end;
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

function MainActivity.onActivityResult(requestCode: LongInt; resultCode: LongInt; data: ACIntent): Boolean;
begin
     //data.getData.  change -----
   case requestCode of
      INTENT_SEND_EMAIL: begin
       AWtoast.makeText(self,JLString('-----'),AWToast.LENGTH_SHORT).show();
     end;
  end;
   Result:=inherited onActivityResult(requestCode, resultCode, data);
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
