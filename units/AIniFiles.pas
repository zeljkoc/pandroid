{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit AIniFiles;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15, Rjava;

type

  { AIniFile }

  AIniFile = class(JLObject)
  private
    fContext: ACContext;
    fFileName: JLstring;
    fValues: ACContentValues;
    function getValue(aTag: JLString; aElement: OWDElement; aDefault: JLString): JLString;
    procedure setValue(aTag: JLString; aElement: OWDElement; aValue: JLString);
    procedure SaveInifile;
  public
    constructor Create(aContext: ACContext; aFileName: JLString);  overload;
    function ReadString(aSection, aIdent, aDefault: JLString): JLString;
    procedure WriteString(aSection, aIdent,aValue: JLString);
  end;

implementation

uses AZCDialogs;

{ AIniFile }

function AIniFile.getValue(aTag: JLString; aElement: OWDElement; aDefault: JLString): JLString;
begin

end;

procedure AIniFile.setValue(aTag: JLString; aElement: OWDElement; aValue: JLString);
begin

end;

procedure AIniFile.SaveInifile;

begin
 { String FILE_NAME = "file.txt";
  try {
      FileOutputStream fos = openFileOutput(FILE_NAME, Context.MODE_PRIVATE);
      fos.write(someText.toString().getBytes());
      fos.close();
  } catch (Exception e) {
      e.printStackTrace();
  }}
end;

constructor AIniFile.Create(aContext: ACContext; aFileName: JLString);
var
 input: JIInputStream;
 i: integer;
begin
  fContext := aContext;
  fFileName:= aFileName;

 // fValues:= ACContentValues.create;
  input:= fContext.getAssets.open(fFileName);


 {if input.available > 0 then
    fDoc:= dBuilder.parse(input)
  else fDoc :=  dBuilder.newDocument;}

end;

function AIniFile.ReadString(aSection, aIdent, aDefault: JLString): JLString;
begin
  Result := aDefault;
end;

procedure AIniFile.WriteString(aSection, aIdent, aValue: JLString);

begin

 // SaveInifile;
end;

end.

