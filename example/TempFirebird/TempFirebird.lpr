{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 10-5-16 16:17:07 
***********************************************************}  
{%BuildWorkingDir /usr/local/pandroid/example/TempFirebird/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit TempFirebird;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.tempfirebird}

interface

uses androidr15, Rjava, AActivity, IniFile, AZCEditFile, zcfbclient;

var
   IniFileName : JLString;
   ini: ZCTIniFile;


   DataBaseName: JLString;
   CharSet:JLString;
   UserName: JLString;
   Password: JLstring;

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
    UIBDataBase: ZCFUIBDataBase;
    UIBDataSet: ZCFUIBDataSet;
    tv, tvError: AWTextView;

    select, execute: AWEditText;

  public
    procedure SetingsIniFile;
    procedure ReadIniFile;    
  end;

implementation

uses AZCForms, AZCDialogs;

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

    select:= AWEditText.create(Self);
   // select.setText(JLString('select "sJMjere"."JMjereID", "sJMjere"."JMjere" from "sJMjere" '));
    select.setText(JLString('select "sLice"."LiceID", "sLice"."Prezime"||"sLice"."Ime" from "sLice" '));
  layout.setView(select);

  layout.addButton(Self, 1, 'Select');

    execute:= AWEditText.create(Self);
    execute.setText(JLString('insert into "sJMjere" ("JMjereID", "JMjere") values (100, ''TONA'') '));
  layout.setView(execute);

  layout.addButton(Self, 2, 'executeSQL');
  layout.addButton(Self, 10, 'IZLAZ');

    tvError:= AWTextView.create(Self);
  layout.setView(tvError);

      tv:= AWTextView.create(Self);
  layout.setView(tv);

  setContentView(layout);

  UIBDataBase:= ZCFUIBDataBase.create;

 // UIBDataBase.Init(JLString('192.168.1.22/3070:01Zdravstvo25'), JLString('UTF8'), JLString('/system/lib/libfbclient.so.3.0.0'));
 // UIBDataBase.setUserNamePassword(JLString('SYSDBA'), JLString('jedan'));

  UIBDataBase.Init(DataBaseName, CharSet, JLString('/system/lib/libfbclient.so.3.0.0'));
  UIBDataBase.setUserNamePassword(UserName, Password);

  UIBDataSet:= ZCFUIBDataSet.create();

  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);

end;

procedure MainActivity.onClick(aView: AVView);
var
 i: Integer;
 text: JLstring;
begin
 case aView.getId of
      1: begin

          UIBDataSet.Init(select.getText.toString); // JLString('select "sJMjere"."JMjereID", "sJMjere"."JMjere" from "sJMjere" '));

         // tvError.setText(UIBDataSet.Open);
        text := UIBDataSet.Open.toString;
        if (text.toString = 'OK') then begin

          UIBDataSet.Last;
          tvError.setText(JLString('').concat(JLInteger.Create(UIBDataSet.FieldCount).toString).concat(' - ').concat(JLInteger.Create(UIBDataSet.RecordCount).toString));

          UIBDataSet.First;
          text := 'Read all records, max 20 views'+#10;
         // tv.setText(tv.getText.toString.concat('  nnnnn ').concat(JLInteger.Create(UIBDataSet.RecordCount).toString ) );
         for i:= 0 to {UIBDataSet.RecordCount -1} 20 do begin
            UIBDataSet.setRecNo(i);
            text := Text.concat('  ').concat(UIBDataSet.getAsString(0).toString ).concat(' = ')
                                                       .concat(UIBDataSet.getAsString(1).toString ).concat(#10) ;

         end;
         // showMessage(self, text);
         tv.setText(text);

         ZCFUIBDataBase.Disconnect;

       end else ShowMessage(Self, text.toString);

      end;

     2: begin

         UIBDataSet.Init(execute.getText.toString);
         text := UIBDataSet.ExecSQL.toString;
        if ( text.toString = 'OK')  then begin //UIBDataSet.ExecSQL;
          ZCFUIBDataBase.Disconnect;

         end else ShowMessage(Self, text.toString);
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

procedure MainActivity.SetingsIniFile;
begin

    ini.setString(JLString('PDAsetings'), JLString('DataBaseName'),
       ini.getString(JLString('PDAsetings'), JLString('DataBaseName'), JLString('192.168.1.22/3070:01Zdravstvo25')) );
    ini.setString(JLString('PDAsetings'), JLString('CharSet'),
       ini.getString(JLString('PDAsetings'), JLString('CharSet'), JLString('UTF8')) );
    ini.setString(JLString('PDAsetings'), JLString('UserName'),
       ini.getString(JLString('PDAsetings'), JLString('UserName'), JLString('SYSDBA')) );
    ini.setString(JLString('PDAsetings'), JLString('Password'),
       ini.getString(JLString('PDAsetings'), JLString('Password'), JLString('masterkey')) );

   ReadIniFile;
end;

procedure MainActivity.ReadIniFile;
begin
  DataBaseName := ini.getString(JLString('PDAsetings'), JLString('DataBaseName'), JLString('192.168.1.22/3070:01Zdravstvo25'));
  CharSet  := ini.getString(JLString('PDAsetings'), JLString('CharSet'), JLString('UTF8'));
  UserName  := ini.getString(JLString('PDAsetings'), JLString('UserName'), JLString('SYSDBA'));
  Password  := ini.getString(JLString('PDAsetings'), JLString('Password'), JLString('masterkey'));
end;

end.
