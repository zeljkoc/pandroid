{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)                    *}
{******************************************}
unit Data;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

//uses androidr15, 
{$include /usr/local/pandroid/units/AndroidVersion.inc}
,PandroidModule, DB, DataBase;

const
  DataBaseName  = 'Finand03.ldb';

  var
    dBase: TDataBase;
    IdPDA : JLString;
    IDPartner: JLString;
    Ident: JLString; //Identifikacija
    AndroidID: JLString; //AndroidID
    
    PModule: ZCPandroidModule;
    IDPModule: jint;

  function initDataBase(aContext: ACContext; aDatabase: TDataBase): TDataBase;

  function SifrarniciListView(aContext: ACContext; aField: TFieldDef): AVView;
  function DnevnikView(aContext: ACContext; aField: TFieldDef): AVView;
type

  { TQuery }

  TQuery = class
    fModule: ZCPandroidModule;
    fIDPModule: jint;
    fError: String;
  private
    function GetAsString(aIndex: integer): String;
    function GetExecuteDirect(aSQL: String): String;
    function GetQueryToSQLite: String;
    function GetQueryToSQLiteRedniBroj: String;
    function GetRecordCount: integer;
    function GetRecordNo: integer;
    function GetReplicate: String;
    procedure SetFieldNo(AValue: integer);
    procedure SetRecordNo(AValue: integer);
  public
     constructor Create(aSQL: JLString; aModule: ZCPandroidModule; aIDPModule: jint );
     constructor Create(aModule: ZCPandroidModule; aIDPModule: jint);
  public
     property RecordNo: integer read GetRecordNo write SetRecordNo;
     property FieldNo: integer write SetFieldNo;
     property RecordCount: integer read GetRecordCount;
     property AsString[aIndex: integer]: String read GetAsString;

     property ExecuteDirect[aSQL: String]:String read GetExecuteDirect;

     property QueryToSQLite: String read GetQueryToSQLite;
     property QueryToSQLiteRedniBroj: String read GetQueryToSQLiteRedniBroj;

     property Error : String read fError;

     property Replicate: String read GetReplicate;

  end;


implementation

uses StdCtrls, Dialogs, Utils;

function initDataBase(aContext: ACContext; aDatabase: TDataBase): TDataBase;
begin
    aDatabase :=  TDatabase.Create(aContext, DataBaseName, nil, 1);

    //sifrarnici
    aDatabase.SQL.add(JLString('create table sPartner (' +
             'PartnerID integer primary key not null, '+
             'Partner text not null ) ' ));


    Result := aDatabase;
end;


function SifrarniciListView(aContext: ACContext; aField: TFieldDef): AVView;
var
   layout: AWLinearLayout;
   i: jint;
begin
       layout:= AWLinearLayout.Create(aContext);
       layout.setOrientation(AWLinearLayout.VERTICAL);


       for i:=0 to aField.FieldCount - 1 do begin
          layout.addView(TTextView.create(aContext));
          TTextView(layout.getChildAt(layout.getChildCount - 1)).Text :=
            JLString(aField.DisplayName[i].trim).concat(': ').concat(aField.Value[i].AsString.trim); //.concat(#10#13);
          TTextView(layout.getChildAt(layout.getChildCount - 1)).setGravity(AVGravity.RIGHT);

         if i = 0 then begin
          TTextView(layout.getChildAt(layout.getChildCount - 1)).setTypeface(nil, AGTypeface.BOLD);
          TTextView(layout.getChildAt(layout.getChildCount - 1)).setGravity(AVGravity.Left);
         end;
       end;

       //linija ispod
       layout.addView(TTextView.create(aContext));
       TTextView(layout.getChildAt(layout.getChildCount - 1)).setBackgroundColor(AGColor.RED);
       TTextView(layout.getChildAt(layout.getChildCount - 1)).Height := 1;

       Result := layout;
end;

function DnevnikView(aContext: ACContext; aField: TFieldDef): AVView;
var
    layout: AWLinearLayout;
    gd: AGDGradientDrawable;
begin
  layout:= AWLinearLayout.Create(aContext);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  gd:= AGDGradientDrawable.create;
  gd.setColor(0); // $FFFFFFFF); // Changes this drawbale to use a single color instead of a gradient
 // gd.setCornerRadius(20);
  //gd.setShape(AGDGradientDrawable.RADIAL_GRADIENT);
  gd.setStroke(1, $FF000000);
                                                         //PaketBroj, IDMaterijal, DuzinaCM, DebljinaMM, SirinaCM, SUM(Komada) as Komada, sum(0) as KolicinaM3
  layout.addView(TTextView.create(aContext));
    TTextView(layout.getChildAt(layout.getChildCount - 1)).Text :=  JLString('   ').concat(aField.Value[5].AsString)
                                                                   .concat(' DVal:').concat(aField.Value[6].AsString)
                                                                   .concat(' * ').concat(aField.Value[0].AsString)
                                                                   .concat(#10).concat('  ').concat(aField.Value[2].AsString)
                                                                   .concat(' ****** ')
                                                                   .concat( JTDecimalFormat.create('0.00').format(aField.Value[3].AsFloat - aField.Value[4].AsFloat));
//  TTextView(layout.getChildAt(layout.getChildCount - 1)).Height := 88; // NHeight; //70 ini file;
  TTextView(layout.getChildAt(layout.getChildCount - 1)).setBackgroundDrawable(gd);
  TTextView(layout.getChildAt(layout.getChildCount - 1)).setTypeface(nil, AGTypeface.BOLD);
 // TTextView(layout.getChildAt(layout.getChildCount - 1)).setGravity(AVGravity.CENTER_HORIZONTAL);
  TTextView(layout.getChildAt(layout.getChildCount - 1)).setGravity(AVGravity.LEFT);

  Result := layout;
end;

{ TQuery }

function TQuery.GetAsString(aIndex: integer): String;
begin
  fModule.SetPropValue(fIDPModule, JLString('FieldNo'), JLString(JLInteger.toString(aIndex)));
  Result := fModule.GetPropValue(fIDPModule, JLString('AsString'));
end;

function TQuery.GetExecuteDirect(aSQL: String): String;
begin
  fModule.SetPropValue(fIDPModule, JLString('RemoteSQL'), JLString(aSQL));
  fError:=fModule.GetPropValue(fIDPModule, JLString('RemoteExecuteSQL'));
end;

function TQuery.GetQueryToSQLite: String;
begin
  fError := '';
  fError:=fModule.GetPropValue(fIDPModule, JLString('QueryToSQLite'));
end;

function TQuery.GetQueryToSQLiteRedniBroj: String;
begin
 fError := '';
 Result:=fModule.GetPropValue(fIDPModule, JLString('QueryToSQLiteRedniBroj'));
end;

function TQuery.GetRecordCount: integer;
begin
  Result := JLInteger.create(fModule.GetPropValue(fIDPModule, JLString('RecordCount'))).IntValue;
end;

function TQuery.GetRecordNo: integer;
begin
  Result := JLInteger.create(fModule.GetPropValue(fIDPModule, JLString('FieldNo'))).IntValue;
end;

function TQuery.GetReplicate: String;
begin
  Result := fModule.GetPropValue(fIDPModule, JLString('Replicate'));
end;


procedure TQuery.SetFieldNo(AValue: integer);
begin
  fModule.SetPropValue(fIDPModule, JLString('FieldNo'), JLInteger.toString(aValue));
end;

procedure TQuery.SetRecordNo(AValue: integer);
begin
  fModule.SetPropValue(fIDPModule, JLString('RecordNo'), JLInteger.toString(aValue));
end;

constructor TQuery.Create(aSQL: JLString; aModule: ZCPandroidModule; aIDPModule: jint);
begin
  fModule := aModule;
  fIDPModule := aIDPModule;
  fModule.SetPropValue(fIDPModule, JLString('SelectQuery'), aSQL);
  fError:=fModule.GetPropValue(fIDPModule, JLString('RefreshQuery'));
end;


constructor TQuery.Create(aModule: ZCPandroidModule; aIDPModule: jint);
begin
 fError := '';
 fModule := aModule;
 fIDPModule := aIDPModule;
end;
end.

