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
, Rjava, AActivity, IniFile, Data, PandroidModule, Utils;

type

  { MainActivity }

  #ActivityName# = class(Activity)
  private
    procedure SetScale(aScale: jfloat); 
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
  public
    IniFileName : JLString;
    ini: ZCTIniFile;
    
    fHost: String;
    fPort: String;
    fScale: jFloat; 

    procedure RefreshSifrarnik;
  end;

implementation

uses fSifre,  AZCForms, Dialogs;

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
  dBase := initDataBase(self, dBase);

  PModule:= ZCPandroidModule.Create;
  IDPModule:= PModule.CreateObject('TDataM');

  if dBase.isopen then begin
          //set property value
          PModule.SetPropValue(IDPModule, JLString('Parameters'),
              JLString('SQLiteDatabaseName=').concat('/data/data/').concat(self.getApplicationContext.getPackageName).concat('/databases/').concat(DataBaseName).
               concat(';Host=').concat(fHost).
               concat(';Port=').concat(fPort).
               concat(';Identifikacija=').concat(Ident.toString.trim).
               concat(';AndroidID=').concat( APSettings.InnerSecure.getString(self.getContentResolver, APSettings.InnerSecure.ANDROID_ID ).toString.trim).
               concat(';IDPartner=').concat(IDPartner).
               concat(';IDPda=').concat(IdPDA)
              ); 

  end else Finish;  
  //=============================

  layout.addButtonImage(Self, 8, 'SIFRARNICI', R.drawable.notes);
  layout.addButtonImage(Self, 10, 'IZLAZ', R.drawable.exit);

  setContentView(layout);
  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);
end;



procedure #ActivityName#.onClick(aView: AVView);
var
  intent: ACIntent;
begin
 case aView.getId of
   8: begin
       intent:=ACIntent.Create(Self, JLClass(TFormSifre));
       startActivity(intent);
    end;

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
  SubMenu.add(0, 1, 0, JLString('Edit ini file')).setIcon(R.drawable.settings);
  SubMenu.add(0, 2, 0, JLString('Remote refresh!!!')).setIcon(R.drawable.remote_database);

  MenuItem := SubMenu.getItem;
  MenuItem.setIcon(r.drawable.tools);
  MenuItem.setShowAsAction(AVMenuItem.SHOW_AS_ACTION_ALWAYS);   

  Result := true;
end;

function #ActivityName#.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
   Result := true;
   case item.getItemID of
      1: begin
         With TEditFileDialog.create(Self , IniFileName) do
          show;
         end;
      2: begin //Refresh DataBase
           RefreshSifrarnik;
         end else
           Result := false;
   end;
end;


procedure #ActivityName#.SetingsIniFile;
begin
    ini.setDouble(JLString('PDAsetings'), JLString('Scale'), ini.getDouble(JLString('PDAsetings'), JLString('Scale'), 1) );
    ini.setString(JLString('PDAsetings'), JLString('Host'), ini.getString(JLString('PDAsetings'), JLString('Host'), JLString('desktop.zeljus.com')) );
    ini.setString(JLString('PDAsetings'), JLString('Port'), ini.getString(JLString('PDAsetings'), JLString('Port'), '7000') );
    ini.setString(JLString('PDAsetings'), JLString('IdPDA'),  ini.getString(JLString('PDAsetings'), JLString('IdPDA'), '100') );
    ini.setString(JLString('PDAsetings'), JLString('IDPartner'), ini.getString(JLString('PDAsetings'), JLString('IDPartner'), string('1')) );

    ini.setString(JLString('PDAsetings'), JLString('Ident'), ini.getString(JLString('PDAsetings'), JLString('Ident'), string('0000')) );

   ReadIniFile;
end;

procedure #ActivityName#.ReadIniFile;
begin
 fScale:= ini.getDouble(JLString('PDAsetings'), JLString('Scale'), 1);    
 fHost  := ini.getString(JLString('PDAsetings'), JLString('Host'), JLString('desktop.zeljus.com'));
 fPort  := ini.getString(JLString('PDAsetings'), JLString('Port'), '7000');

 IdPDA  := ini.getString(JLString('PDAsetings'), JLString('IdPDA'), '100');
 IDPartner := ini.getString(JLString('PDAsetings'), JLString('IDPartner'), string('1'));

 Ident  := ini.getString(JLString('PDAsetings'), JLString('Ident'), string('0000'));  
end;


procedure #ActivityName#.RefreshSifrarnik;
var
  query : TQuery;
begin
   query := TQuery.Create(PModule, IDPModule);
   ShowMessage(Self, JLString(query.Error));  
end;

procedure #ActivityName#.SetScale(aScale: jfloat);
var
 configuration: ACRConfiguration;
 metrics: AUDisplayMetrics;
begin
  configuration := getResources.getConfiguration;
  configuration.ffontScale := jfloat(aScale); //0.85 small size, 1 normal size, 1,15 big etc

 metrics := AUDisplayMetrics.create;
 getWindowManager.getDefaultDisplay.getMetrics(metrics);
 metrics.fscaledDensity := configuration.ffontScale * metrics.fdensity;
 getBaseContext.getResources.updateConfiguration(configuration, metrics);
end; 


end.
