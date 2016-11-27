{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit AUtils;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses
  androidr15;

type
   ADataType = (ftString, ftInteger, ftFloat, ftEmail);

//ok
function FormatFloat(aFormat: String; aValue: Real): Real;
function Format(aFormat: String; aValue: Real): String;
function IntToStr(aValue: jint): String;
function StrToInt(aValue: String): jint;
function LongToStr(aValue: jlong): String;
function StrToLong(aValue: String): jlong;
function FloatToStr(aValue: jfloat): String;
function StrToFloat(aValue: String): jfloat;

function LeftStr(Str: JLString; ch: jChar; Size: Integer): JLString;
function RightStr(Str: JLString; ch: jChar; Size: Integer): JLString;
function CharReplace(Str: JLString; ch: Char; ToCh: Char): String;
function StrToFloat(aStr: JLString): Real;
function FBDateTimeToString: JLString;  //Firebird Date time

function CopyFile(currentDBPath: JLString; backupDBFile: JLString): JLString;
//?????
function GetNextRedniBroj(PrevBroj:String; aFormat: string): JLString;
function isInputTest(aValue: JLString; aFieldType: ADataType): boolean;



implementation

function FormatFloat(aFormat: String; aValue: Real): Real;
var
  df: JTDecimalFormat;
  ds: JTDecimalFormatSymbols;
begin
   ds:= JTDecimalFormatSymbols.Create;
   try
     ds.setDecimalSeparator(JChar('.'));
     ds.setGroupingSeparator(JChar(','));
     df := JTDecimalFormat.create(aFormat, ds);
     Result := JLFloat.parseFloat(df.format(aValue).toString);
   except
      ds.setDecimalSeparator(JChar(','));
      ds.setGroupingSeparator(JChar('.'));
      df := JTDecimalFormat.create(aFormat, ds);
      Result := JLFloat.parseFloat(df.format(aValue).toString);
   end;
end;

function Format(aFormat: String; aValue: Real): String;     //aFormat = '#0.00'
var
  df: JTDecimalFormat;
  ds: JTDecimalFormatSymbols;
begin
   ds:= JTDecimalFormatSymbols.Create;
   try
     ds.setDecimalSeparator(JChar('.'));
     ds.setGroupingSeparator(JChar(','));
     df := JTDecimalFormat.create(aFormat, ds);
     Result := df.format(aValue).toString;
   except
      ds.setDecimalSeparator(JChar(','));
      ds.setGroupingSeparator(JChar('.'));
      df := JTDecimalFormat.create(aFormat, ds);
      Result :=df.format(aValue).toString;
   end;
end;

function LeftStr(Str: JLString; ch: jChar; Size: Integer): JLString;
var
  msg: JLStringBuilder;
  i: integer;
begin
   msg:= JLStringBuilder.Create(Str);
   for i:=0 to (Size - msg.length - 1) do
     msg.insert(0, ch);
   msg.SetLength(Size);
   Result := msg.toString;
end;

function RightStr(Str: JLString; ch: jChar; Size: Integer): JLString;
var
  msg: JLStringBuilder;
  i: integer;
begin
   msg:= JLStringBuilder.Create(Str);
   for i:=0 to (Size - msg.length - 1)  do
      msg.append(ch);
   msg.SetLength(Size);
   Result := msg.toString;
end;

function CharReplace(Str: JLString; ch: Char; ToCh: Char): String;
var
  msg: JLStringBuilder;
  i: integer;
begin
   msg:= JLStringBuilder.Create(Str);

   for i:= 0 to msg.length - 1 do
     if msg.charAt(i) = ch then
      msg.setCharAt(i, toCh);

   Result := msg.toString;
end;

function StrToFloat(aStr: JLString): Real;
var
   msg : array of jbyte;
   i: integer;
   bStr: JLString;
begin
   msg := aStr.getBytes;  bStr := '';
   for i:= 0 to aStr.length - 1 do begin
     if Msg[i] in  [46, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57] then
                    bStr := bStr.concat(JLString.create(Msg[i]));
   end;
   Result := JLFloat.valueOf(bStr.trim).floatValue;
end;

function FBDateTimeToString: JLString;
begin
   //'2015-12-07 16:46:40'; - Firebird format
  Result := JTSimpleDateFormat.create(JLString('yyyy-MM-dd HH:mm:ss')).format(JUDate.Create);
end;

function IntToStr(aValue: jint): String;
begin
   Result := JLInteger.toString(aValue);
end;

function StrToInt(aValue: String): jint;
begin
  Result := JLInteger.parseInt(aValue);
end;

function LongToStr(aValue: jlong): String;
begin
  Result := JLLong.toString(aValue);
end;

function StrToLong(aValue: String): jlong;
begin
  Result :=  JLLong.parseLong(aValue);
end;

function FloatToStr(aValue: jfloat): String;
begin
  Result := JLFloat.toString(aValue);
end;

function StrToFloat(aValue: String): jfloat;
begin
  Result := JLFloat.parseFloat(aValue);
end;

function CopyFile(currentDBPath: JLString; backupDBFile: JLString): JLString;
var
  sd: JIFile;
  data: JIFile;
  currentDB: JIFile;
  backupDB: JIFile;
  src: JNCFileChannel;
  dst: JNCFileChannel;
begin
  sd := AOEnvironment.getExternalStorageDirectory;
  data := AOEnvironment.getDataDirectory;

  currentDB := JIFile.create(data, currentDBPath);
  backupDB := JIFile.create(AOEnvironment.getExternalStoragePublicDirectory(
                     AOEnvironment.fDIRECTORY_DOWNLOADS), backupDBFile);
  //odredi putanju za slanje
  Result := backupDB.toString;

  if (currentDB.exists) then begin

   src := JIFileInputStream.Create(currentDB).getChannel;
   dst:=  JIFileOutputStream.Create(backupDB).getChannel;

   dst.transferFrom(JNCReadableByteChannel(src), 0, src.size);
   src.close;
   dst.close;
  end;

end;

function GetNextRedniBroj(PrevBroj: String; aFormat: string): JLString;
var
   RBroj: integer;
begin
  If Length(PrevBroj) < 5 then RBroj := 1 else
  try
    RBroj := JLInteger.ParseInt(copy(PrevBroj, 7, Length(PrevBroj))) + 1;
  except
     RBroj := 1;
  end;

  Result := '';
  Result :=  Result.concat(JTSimpleDateFormat.create(JLString('yy')).format(JUDate.Create)).concat('/').
                    concat(JTSimpleDateFormat.create(JLString('MM')).format(JUDate.Create)).concat('-');

  Result := Result.concat(JTDecimalFormat.create(aFormat).format(RBroj));
end;

function isInputTest(aValue: JLString; aFieldType: ADataType): boolean;
begin
  try
   case aFieldType of  //kontrola upisa
     ftInteger: JLInteger.parseInt(aValue.toString);
     ftFloat  : JLFloat.parseFloat(aValue.toString);
   end;
   Result := true;
  except
    Result := false;
  end;
end;



end.

