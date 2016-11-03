{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 27-12-15 10:19:08 
***********************************************************}  
{%BuildWorkingDir /usr/local/pandroid/example/TestBarCode/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit TestBarCode;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.testbarcode}

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
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
    //
    procedure afterTextChanged(editable: ATEditable); override;
  public
     db :  ASQLDatabase;
     et: AWEditText;
     tv: AWTextView;
     function ReadBarCode(Code: JLString): JLString;
     function ParseXML(text: JLString): JLString;
  public
    procedure SetingsIniFile;
    procedure ReadIniFile;    
  end;

implementation

uses AZCForms, fbsqlite;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AZCForm;
  HederForm: AZCHorizontalForm;
begin
  inherited onCreate(savedInstanceState);
   layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic +387 65 458 401');
  
  //================= Initialize
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/inifiles.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile; 
  //=============================


     et:= AWEditText.create(self);
     et.addTextChangedListener(self);
     et.setMinWidth(300);
     // et.setInputType(ATInputType.TYPE_NUMBER_FLAG_DECIMAL );
     et.setVisibility(AVView.VISIBLE);  //AVView.GONE
     et.requestFocus;
  layout.setView(et);


  layout.addButton(Self, 10, 'IZLAZ');

     tv:= AWTextView.create(Self);
  layout.setView(tv);


  setContentView(layout);
  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);
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

procedure MainActivity.afterTextChanged(editable: ATEditable);
var
  lastCharacter: char;
  s: string;
begin
  if editable.length > 1 then begin
    lastCharacter := editable.charAt(editable.length - 1);

    if lastCharacter = #10 then begin
      s:= editable.subSequence(0, editable.length - 1).toString.trim;
      //ficd barcode and parse
        tv.setText(ReadBarCode(s));
       editable.clear;
    end;
  end;
end;

function MainActivity.ReadBarCode(Code: JLString): JLString;
var
  xml : JLString;
begin
     xml := JLString('  <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:gep="http://www.gepir.org/"> ')
  .concat('	<soapenv:Header>')
  .concat('		<gep:gepirRequestHeader>')
  .concat('			<!--Optional:-->')
  .concat('			<gep:requesterGln>').concat(code).concat('</gep:requesterGln>')
  .concat('			<!--Optional:-->')
  .concat('			<gep:cascade>1</gep:cascade>')
  .concat('		</gep:gepirRequestHeader>  ')
  .concat('	</soapenv:Header> ')
  .concat('	<soapenv:Body> ')
  .concat('		<gep:GetPartyByGTIN version="1.0"> ')
  .concat('			<!--Zero or more repetitions:--> ')
  .concat('			<gep:requestedGtin>').concat(code).concat('</gep:requestedGtin> ')
  .concat('			<!--Optional:--> ')
  .concat('			<gep:requestedLanguages> ')
  .concat('			<!--Zero or more repetitions:--> ')
  .concat('			<gep:language>0</gep:language> </gep:requestedLanguages> ')
  .concat('		</gep:GetPartyByGTIN> ')
  .concat('	</soapenv:Body> ')
  .concat('</soapenv:Envelope> ');


    try
      Result := ParseXML(ZCJjnireplicate.HTTPSOAPPost('http://52.70.203.173/v31/router.asmx', 'http://www.gepir.org/GetPartyByGTIN', xml));
    except
      Result := JLstring('Error!');
    end;
end;

function MainActivity.ParseXML(text: JLString): JLString;
var
  factory: OXVXmlPullParserFactory;
  xpp: OXVXmlPullParser;
  eventType: integer;
begin
 // Result := Text;
  Result := '';
  factory:= OXVXmlPullParserFactory.newInstance;
  factory.setNamespaceAware(true);
  xpp := factory.newPullParser;

  xpp.setInput(JIStringReader.create(text));
  eventType := xpp.getEventType;
  While (eventType <> OXVXmlPullParser.END_DOCUMENT) do begin
    { if eventType = OXVXmlPullParser.START_TAG then
        Result := Result.concat(xpp.getName).concat(': ')
     else }
     if eventType = OXVXmlPullParser.TEXT then
      Result := Result.concat(xpp.getText).concat(#10);
      { if(eventType == XmlPullParser.START_DOCUMENT) {
              System.out.println("Start document");
          } else if(eventType == XmlPullParser.END_DOCUMENT) {
              System.out.println("End document");
          } else if(eventType == XmlPullParser.START_TAG) {
              System.out.println("Start tag "+xpp.getName());
          } else if(eventType == XmlPullParser.END_TAG) {
              System.out.println("End tag "+xpp.getName());
          } else if(eventType == XmlPullParser.TEXT) {
              System.out.println("Text "+xpp.getText());
          }}

     eventType:= xpp.next;
  end;

end;


procedure MainActivity.SetingsIniFile;
begin

    ini.setInt(JLString('PDAsetings'), JLString('IDPDA'),
       ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1 ));

   ReadIniFile;
end;

procedure MainActivity.ReadIniFile;
begin

  IDPDA :=  ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1);

end;  

end.
