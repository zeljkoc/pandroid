{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit Utils;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

//uses androidr15;
{$include ../AndroidVersion.inc}
;

function checkExistsFile(aFileName: JLString): jboolean;
function DeleteFile(aFileName: JLString): jboolean;
//CopyFile('/data/zeljus.com.popis/popis01.csv', 'popis01.csv' {fNameTo})
function CopyFileExternal(currentDBPath: JLString; backupDBFile: JLString): JLString;
function CopyFile(currentDBPath: JLString; backupDBFile: JLString): JLString;
//Number Utisl
function RealToFormatString(aFormat: JLString; aNumber: Real): JLString;

//Date Utils
function FBDateTimeToString: JLString;  //Firebird Date time
function FBDateToString: JLString;  //Firebird Date
function FBTimeToString: JLString;  //Firebird Time
function DateStringToString(aDate, InFormat, OutFormat: String): JLString;

//Strings
function LeftStr(Str: JLString; ch: jChar; Size: Integer): JLString;
function RightStr(Str: JLString; ch: jChar; Size: Integer): JLString;
function CharReplace(Str: JLString; ch: Char; ToCh: Char): JLString;

function AsciToHex(Str: JLString): JLString;
function HexToAsci(Str: JLString): JLString;

//Test barcode
function EAN13CheckSum(aValue: JLString): integer;


//
function StrToIntDef(const s: String; Default: integer): integer;
//


implementation


function checkExistsFile(aFileName: JLString): jboolean;
begin
  Result := JIFile.Create(aFileName).exists;
end;


function DeleteFile(aFileName: JLString): jboolean;
var
  fn: JIFile;
begin
   Result := false;
   try
     fn := JIFile.create(aFileName);
     if  (fn.exists) then Result := fn.delete();
   except
   end;
   fn := nil;
end;

function CopyFileExternal(currentDBPath: JLString; backupDBFile: JLString): JLString;
var
  data: JIFile;
  currentDB: JIFile;
  backupDB: JIFile;
  src: JNCFileChannel;
  dst: JNCFileChannel;
begin
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

function CopyFile(currentDBPath: JLString; backupDBFile: JLString): JLString;
var
 // sd: JIFile;
  data: JIFile;
  currentDB: JIFile;
  backupDB: JIFile;
  src: JNCFileChannel;
  dst: JNCFileChannel;
begin
 // sd := AOEnvironment.getDownloadCacheDirectory();
  data := AOEnvironment.getRootDirectory();

  currentDB := JIFile.create(data, currentDBPath);
  backupDB := JIFile.create(AOEnvironment.getExternalStoragePublicDirectory(
                     AOEnvironment.fDIRECTORY_DOWNLOADS), backupDBFile);

  //odredi putanju za slanje
  Result :=  backupDB.toString;

  if (currentDB.exists) then begin

   src := JIFileInputStream.Create(currentDB).getChannel;
   dst:=  JIFileOutputStream.Create(backupDB).getChannel;

   dst.transferFrom(JNCReadableByteChannel(src), 0, src.size);
   src.close;
   dst.close;
  end;

end;

function RealToFormatString(aFormat: JLString; aNumber: Real): JLString;
begin
  Result := JTDecimalFormat.create(aFormat).format(aNumber); // -1235
  {a:= JTDecimalFormat.create('00').format(0); // 0
  a:= JTDecimalFormat.create('##00').format(0); // 00
  a:= JTDecimalFormat.create('.00').format(-.4567); // -.46
  a:= JTDecimalFormat.create('0.00').format(-.34567); // -0.346
  a:= JTDecimalFormat.create('#.######').format(-012.34567); // -12.34567
  a:= JTDecimalFormat.create('#.000000').format(-1234.567); // -1234.567000
  a:= JTDecimalFormat.create('#,###,###').format(-01234567.890); // -1 234 568
  a:= JTDecimalFormat.create('text'#').format(+1234.567); // text1235
  a:= JTDecimalFormat.create('00.00E0').format(-012345.67); // -12.35E2}
end;

function FBDateTimeToString: JLString;
begin
   //'2015-12-07 16:46:40'; - Firebird format
  Result := JTSimpleDateFormat.create(JLString('yyyy-dd-MM HH:mm:ss')).format(JUDate.Create);
//    Result := JTSimpleDateFormat.create(JLString('dd-MM-yyyy HH:mm:ss')).format(JUDate.Create);
end;

function FBDateToString: JLString;
begin
  //'2015-12-07 16:46:40'; - Firebird format
  Result := JTSimpleDateFormat.create(JLString('yyyy-dd-MM')).format(JUDate.Create);
end;

function FBTimeToString: JLString;
begin
  //'2015-12-07 16:46:40'; - Firebird format
  Result := JTSimpleDateFormat.create(JLString('HH:mm:ss')).format(JUDate.Create);
end;

function DateStringToString(aDate, InFormat, OutFormat: String): JLString;
begin
 Result := JTSimpleDateFormat.create(JLString(OutFormat)).format(
    JTSimpleDateFormat.create(JLString(InFormat)).parse(JLString(aDate)));
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

function CharReplace(Str: JLString; ch: Char; ToCh: Char): JLString;
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

function StrToIntDef(const s: String; Default: integer): integer;
var
  E: integer;
begin
   val(s, Result, E);
   if E <> 0 then Result := Default;
end;

function AsciToHex(Str: JLString): JLString;
var
   ch: array of jchar;
   builder: JLStringBuilder;
   i: integer;
begin
  Result := Str;
  if Str.length = 0 then Exit;
  setLength(ch, Str.length);
  ch := Str.toCharArray;

  builder := JLStringBuilder.Create;
  for i:=0 to Str.length - 1 do
    builder.append(JLInteger.toHexString(ord(ch[i])).toUpperCase);

  Result := builder.toString; // Str;
end;

function HexToAsci(Str: JLString): JLString;
var
  builder: JLStringBuilder;
  i: integer;
begin
  Result := Str;
  if Str.length = 0 then Exit;
  builder:= JLStringBuilder.Create;

  i:= 0;
  repeat
     builder.append(char(JLInteger.parseInt(Str.substring(i, (i + 2)), 16)));
    inc(i, 2);
  until i > Str.length - 1;

  Result := builder.toString;
end;

function EAN13CheckSum(aValue: JLString): integer;
var
  i, Digit: integer;
  Odd: boolean;
  Sum: integer;
begin
  Result := -1;
 // if (aValue.length <> 13) then Exit;
  aValue := aValue.CopyValueOf(aValue.toCharArray, 0, aValue.length - 1);
  Sum:=0; Odd := true;
  for i:= aValue.length downto 1 do begin
      Digit := StrToIntDef(string(aValue.charAt(i-1)), 0);
      if odd then
         sum := sum + digit * 3
      else
         Sum := Sum + Digit;
      Odd := not Odd;
  end;
  Result := Sum mod 10;
  if Result <> 0 then Result := 10 - Result;
end;





end.

