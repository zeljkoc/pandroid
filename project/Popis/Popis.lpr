{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 28-10-15 10:54:43 
***********************************************************}  
{%BuildWorkingDir /usr/local/pandroid/project/Popis/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit Popis;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.popis}

interface

uses androidr15, Rjava, AActivity, Data, fPopis, ADBDataBase, AZCDialogs,
  fMjestoPopisa, AZCScrollButons, IniFile;


var
   IniFileName : JLString;
   ini: ZCTIniFile;

   IDRadnoMjesto: Integer; //= '10';
   IDPDA: Integer;

   EMailTo: JLString;

type

  { MainActivity }

  MainActivity = class(Activity)
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
    //open file
      procedure onActivityResult(requestCode: LongInt; resultCode: LongInt; data: ACIntent);
  public
    db :  ASQLDatabase;

     tv: AWTextView;
  public
    procedure SetingsIniFile;
    procedure ReadIniFile;
    procedure ImportFile;
  end;

implementation

uses AZCEditFile,  AZCForms, AUtils;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout : AZCForm;

begin
  inherited onCreate(savedInstanceState);
  layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic');

  //----------------------- Inicijalizacija
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/IniFile.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile;

  db := initDataBase(self, db);

  //---------------------------------------


  layout.addButton(Self, 1, 'POPIS');
  layout.addButton(Self, 2, 'MATERIJAL');
  layout.addButton(Self, 3, 'MJESTO POPISA');
  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);
end;

procedure MainActivity.onClick(aView: AVView);
var
  intent:ACIntent;
  dPartner: AZCLookUpSQLDialog;
begin
   case aView.getId of
       1:  //popis
         begin
           intent:=ACIntent.Create(Self, JLClass(PopisActivity));
           startActivity(intent);
         end;

       2:  //sif materijal
         begin
            dPartner:= AZCLookUpSQLDialog.create(self, db, JLString('select  IDMaterijal, Materijal from sMaterijal '), 'Materijal', 'sMaterijal');
            dPartner.KeyFieldID := 0;    //return string
            dPartner.Caption := 'Materijal';
            dPartner.LookupKeyFields:='0';
           // dPartner.setOnDismissListener(Self);
            dPartner.Show;
         end;

       3:  //Mjesto popisa
         begin
           intent:=ACIntent.Create(Self, JLClass(MjestoPopisaActivity));
           startActivity(intent);
         end;


      10: Finish;
   end;
end;

procedure MainActivity.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin
  //inherited onClick(dInterface, p1);
  ReadIniFile;
end;

function MainActivity.onCreateOptionsMenu(menu: AVMenu): JBoolean;
var
 mi : AVMenuItem;
 sm : AVSubMenu;
begin
  inherited onCreateOptionsMenu(menu);
  sm := menu.addSubMenu(0, 0, 0, JLString(string('Meni')));

  sm.add(0, 3, 0, JLString('Import: SifMaterijal File')).setIcon(R.drawable.ic_next);
  mi := sm.getItem;

  sm.add(0, 4, 0, JLString('Send Popis Email')).setIcon(R.drawable.ic_next);
  mi := sm.getItem;

  sm.add(0, 5, 0, JLString('Edit ini file')).setIcon(R.drawable.ic_next);
  mi := sm.getItem;
  mi.setIcon(r.drawable.ic_menu);
  mi.setShowAsAction(AVMenuItem.SHOW_AS_ACTION_ALWAYS);

  Result := true;
end;

function MainActivity.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
var
  edit: AEditFile;
begin
   Result := true;
   case item.getItemID of
      0: begin end;

      3: ImportFile; //Import file


      4: begin   //send email
           try
              if generateCsvFile('/data/data/zeljus.com.popis/popis01.csv', db) then begin
              // fNameTo := 'popis01.csv';
               ExportPopis(self, CopyFile('/data/zeljus.com.popis/popis01.csv', 'popis01.csv' {fNameTo}), db);
              end;
           except
           end;
        end;

      5: begin   // Edit ini file
         edit := AEditFile.create(Self , IniFileName) ;
         edit.show;
      end else Result := false;
   end;
end;


procedure MainActivity.onActivityResult(requestCode: LongInt;
  resultCode: LongInt; data: ACIntent);
begin
    case requestCode of
    1 : if (resultCode = RESULT_OK) then begin
       FillMaterijal(self, data.getData.getPath.toString, db);
     end;
  end;
end;

procedure MainActivity.SetingsIniFile;
begin

    ini.setInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'),
       ini.getInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'), 1) );
    ini.setInt(JLString('PDAsetings'), JLString('IDPDA'),
       ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1 ));

    ini.setInt(JLString('PDAsetings'), JLString('IDMjestoPopisa'),
       ini.getInt(JLString('PDAsetings'), JLString('IDMjestoPopisa'), 1));

    ini.setString(JLString('PDAsetings'), JLString('EMailTo'),
       ini.getString(JLString('PDAsetings'), JLString('EMailTo'), JLString('cvzeljko@gmail.com')) );

   ReadIniFile;
end;

procedure MainActivity.ReadIniFile;
begin

  IDRadnoMjesto := ini.getInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'), 1);
  IDPDA :=  ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1);

  IDMjestoPopisa := ini.getInt(JLString('PDAsetings'), JLString('IDMjestoPopisa'), 1);

  EMailTo := ini.getString(JLString('PDAsetings'), JLString('EMailTo'), JLString('cvzeljko@gmail.com'));
end;

procedure MainActivity.ImportFile;
var
 intent : ACIntent;
 uri : ANUri;
 file1 : JIFile;
begin
  try
    intent := ACIntent.create(ACIntent.ACTION_GET_CONTENT);
    intent.setType(JLString('file/*'));
    uri := ANUri.fromFile(AOEnvironment.getExternalStorageDirectory());
    startActivityForResult(intent, 1);
  except
    ShowMessage(self, JLString('Please Insert SDCard'));
  end;
end;


end.
