{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit ADBDataBase;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses
  androidr15, dataset;

type
   { ASQLDatabase }

   ASQLDatabase = class(ADSSQLiteOpenHelper)
   private
    fContext: ACContext;
    fSQL: JUList;
   public
    procedure onCreate(aDatabase: ADSSQLiteDatabase); override;
    procedure onUpgrade(aDatabase: ADSSQLiteDatabase; oldVersion: integer; newVersion: integer); override;
    procedure onOpen(aDatabase: ADSSqliteDatabase); override;
   public
     constructor Create(context: ACContext; DatabaseName: String;
      factory: ADSSQLiteDatabase.InnerCursorFactory; version: integer);

     function isopen: jboolean;
     procedure beginTransaction;
     procedure setTransactionSuccessful;
     procedure endTransaction;
     procedure beginTransactionNonExclusive;

     function insert(aTableName: JLString; para2: JLString; aValues: ACContentValues): jlong;
     function update(aTableName: JLString; aValues: ACContentValues; aWhere: JLString; para4: Arr1JLString): jint;
     function delete(aTableName: JLString; aWhere: JLString; para3: Arr1JLString): jint;

     function rawQuery(aSQL: JLString; para2: Arr1JLString): ADCursor;
     function query(para1: jboolean; para2: JLString; para3: Arr1JLString; para4: JLString; para5: Arr1JLString; para6: JLString; para7: JLString; para8: JLString; para9: JLString): ADCursor;
     procedure execSQL(aSQL: JLString);

     property SQL: JUList read FSQL write FSQL;
   end;

function checkExistsFile(aDataBase: JLString): boolean;
function CreateFields(aContext: ACContext; aDataBase: ASQLDatabase; aSQL: JLString): JUArrayList;
function CursorToFields(aCurosr: ADCursor): JUArrayList;
function InsertFields(aContext: ACContext; aDataBase: ASQLDatabase; aTable: JLString; aFields: JUList): boolean;
function DBEditFields(aContext: ACContext; aDataBase: ASQLDatabase; aTable: JLString; aFields: JUList): boolean;
function DBInsertFields(aContext: ACContext; aDataBase: ASQLDatabase; aTable: JLString; aFields: JUList): boolean;

implementation

uses AZCDialogs;


function checkExistsFile(aDataBase: JLString): boolean;
var
  aFile: JIFile;
begin
  aFile:= JIFile.Create(aDataBase);
  result := aFile.exists;
end;


function CreateFields(aContext: ACContext; aDataBase: ASQLDatabase; aSQL: JLString): JUArrayList;
var
  cur: ADCursor;
  tField: ZCField;
  i: Integer;
begin
   cur := aDataBase.rawQuery(aSQL.toString, nil);
   cur.moveToFirst;
   Result := JUArrayList.create;

   for i:=0 to Cur.getColumnCount - 1 do begin
        tField := ZCField.create;

        if Cur.getType(i) = 1 then  tField.fDataType := ZCField.InnerFDataType.ftIinteger
          else  if Cur.getType(i) = 2 then  tField.fDataType := ZCField.InnerFDataType.ftFloat
          else tField.fDataType := ZCField.InnerFDataType.ftString;

        tField.fName := cur.getColumnName(i).toString;
        tField.fOldValue := Cur.getString(i).toString.trim;
        tField.fValue := Cur.getString(i).toString.trim;


        Result.add(JLObject(tField));
   end;
end;

function CursorToFields(aCurosr: ADCursor): JUArrayList;
var
  tField: ZCField;
  i: Integer;
begin
  Result := JUArrayList.create;

   for i:=0 to aCurosr.getColumnCount - 1 do begin
        tField := ZCField.create;
        tField.fDataType := ZCField.InnerFDataType.ftString;   // za prazno kursor nemoze odrediti tip
        tField.fName := aCurosr.getColumnName(i).toString;
        tField.fCharCase := ZCField.InnerEditCharCase.fNormal;
        Result.add(JLObject(tField));
   end;
end;

function InsertFields(aContext: ACContext; aDataBase: ASQLDatabase; aTable: JLString; aFields: JUList): boolean;
var
  values: ACContentValues;
  i: integer;
begin
   Result := True;
   values := ACContentValues.Create(1);
   for i:=0 to aFields.size - 1 do begin
     values.put(ZCField(aFields.get(i)).fName, ZCField(aFields.get(i)).fValue);
   end;

  aDataBase.beginTransactionNonExclusive;
  try
    try
      aDataBase.insert(aTable, Nil, values);
      aDataBase.setTransactionSuccessful;
   finally
      aDataBase.endTransaction;
   end;
 except
   Result := False;
 end;
end;

function DBEditFields(aContext: ACContext; aDataBase: ASQLDatabase;  aTable: JLString; aFields: JUList): boolean;
var
  values: ACContentValues;
  i: integer;
  key: JLString;
begin
   Result := True;
   key := JLString(ZCField(aFields.get(0)).fName).concat(' = ').concat(ZCField(aFields.get(0)).fValue);

   values := ACContentValues.Create(1);
   for i:=1 to aFields.size - 1 do begin
      if (ZCField(aFields.get(i)).fChange) then begin
       values.put(ZCField(aFields.get(i)).fName, ZCField(aFields.get(i)).fValue);
      end;
   end;

  aDataBase.beginTransactionNonExclusive;
  try
    try
      aDataBase.update (aTable, values, key, nil);
      aDataBase.setTransactionSuccessful;
   finally
      aDataBase.endTransaction;
   end;
 except
   Result := False;
 end;

end;

function DBInsertFields(aContext: ACContext; aDataBase: ASQLDatabase; aTable: JLString; aFields: JUList): boolean;
var
  values: ACContentValues;
  i: integer;
begin
   Result := True;

   values := ACContentValues.Create(1);
   for i:=1 to aFields.size - 1 do begin
      if (ZCField(aFields.get(i)).fChange) then begin
       values.put(ZCField(aFields.get(i)).fName, ZCField(aFields.get(i)).fValue);
      end;
   end;

  aDataBase.beginTransactionNonExclusive;
  try
    try
      aDataBase.insert(aTable, nil, values);
      aDataBase.setTransactionSuccessful;
   finally
      aDataBase.endTransaction;
   end;
 except
   Result := False;
 end;

end;

{ ASQLDatabase }

constructor ASQLDatabase.Create(context: ACContext; DatabaseName: String;
  factory: ADSSQLiteDatabase.InnerCursorFactory; version: integer);
begin
  inherited Create(context, DatabaseName, factory, version);
  FContext := context;
  FSQL:= JUArrayList.Create;
end;

function ASQLDatabase.isopen: jboolean;
begin
  Result := getWritableDatabase.isopen;
end;

procedure ASQLDatabase.beginTransaction;
begin
  getWritableDatabase.beginTransaction;
end;

procedure ASQLDatabase.setTransactionSuccessful;
begin
  getWritableDatabase.setTransactionSuccessful;
end;

procedure ASQLDatabase.endTransaction;
begin
  getWritableDatabase.endTransaction;
end;

procedure ASQLDatabase.beginTransactionNonExclusive;
begin
  getWritableDatabase.beginTransactionNonExclusive;
end;

function ASQLDatabase.insert(aTableName: JLString; para2: JLString;
  aValues: ACContentValues): jlong;
begin
  getWritableDatabase.insert(aTableName, para2, aValues);
end;

function ASQLDatabase.update(aTableName: JLString; aValues: ACContentValues;
  aWhere: JLString; para4: Arr1JLString): jint;
begin
  getWritableDatabase.update(aTableName, aValues, aWhere, para4);
end;

function ASQLDatabase.delete(aTableName: JLString; aWhere: JLString; para3: Arr1JLString): jint;
begin
  Result := getWritableDatabase.delete(aTableName, aWhere, para3);
end;

function ASQLDatabase.rawQuery(aSQL: JLString; para2: Arr1JLString): ADCursor;
begin
  Result := getReadableDatabase.rawQuery(aSQL , para2);
end;

function ASQLDatabase.query(para1: jboolean; para2: JLString;
  para3: Arr1JLString; para4: JLString; para5: Arr1JLString; para6: JLString;
  para7: JLString; para8: JLString; para9: JLString): ADCursor;
begin
  Result := getReadableDatabase.query(para1, para2, para3, para4, para5, para6, para7, para8, para9);
end;

procedure ASQLDatabase.execSQL(aSQL: JLString);
begin
  execSQL(aSQL);
end;

procedure ASQLDatabase.onCreate(aDatabase: ADSSQLiteDatabase);
var
  i: integer;
begin
  try
    if aDatabase.isOpen then  begin
       for i:=0 to FSQL.size - 1 do
        aDatabase.execSQL(FSQL.get(i).toString);
    end;
  except
      on e: ADSQLException do
      AULog.d('onCreateDB', e.getMessage);
  end;
end;

procedure ASQLDatabase.onUpgrade(aDatabase: ADSSQLiteDatabase;
  oldVersion: integer; newVersion: integer);
begin
  // aDatabase.execSQL('DROP TABLE IF EXISTS scan_result');
  // onCreate(aDatabase);
end;

procedure ASQLDatabase.onOpen(aDatabase: ADSSqliteDatabase);
begin
  inherited onOpen(aDatabase);
  if not aDatabase.isReadOnly then
    aDatabase.ExecSQL('pragma foreign_keys=on;');
end;

end.

