{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit DataBase;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

//uses androidr15;
{$include ../AndroidVersion.inc}
;

type

  { TDataBase }

  TDataBase = class(ADSSQLiteOpenHelper)
   private
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
     procedure ExecuteDirect(aSQL: JLString);

     property SQL: JUList read FSQL write FSQL;
   end;


implementation


{ TDataBase }

procedure TDataBase.onCreate(aDatabase: ADSSQLiteDatabase);
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

procedure TDataBase.onUpgrade(aDatabase: ADSSQLiteDatabase;
  oldVersion: integer; newVersion: integer);
begin
    // aDatabase.execSQL('DROP TABLE IF EXISTS scan_result');
  // onCreate(aDatabase);
end;

procedure TDataBase.onOpen(aDatabase: ADSSqliteDatabase);
begin
  inherited onOpen(aDatabase);
  if not aDatabase.isReadOnly then
    aDatabase.ExecSQL('pragma foreign_keys=on;');
end;

constructor TDataBase.Create(context: ACContext; DatabaseName: String;
  factory: ADSSQLiteDatabase.InnerCursorFactory; version: integer);
begin
  inherited Create(context, DatabaseName, factory, version);
  FSQL:= JUArrayList.Create;
end;

function TDataBase.isopen: jboolean;
begin
  Result := getWritableDatabase.isopen;
end;

procedure TDataBase.beginTransaction;
begin
  getWritableDatabase.beginTransaction;
end;

procedure TDataBase.setTransactionSuccessful;
begin
  getWritableDatabase.setTransactionSuccessful;
end;

procedure TDataBase.endTransaction;
begin
  getWritableDatabase.endTransaction;
end;

procedure TDataBase.beginTransactionNonExclusive;
begin
  getWritableDatabase.beginTransactionNonExclusive;
end;

function TDataBase.insert(aTableName: JLString; para2: JLString;
  aValues: ACContentValues): jlong;
begin
  getWritableDatabase.insert(aTableName, para2, aValues);
end;

function TDataBase.update(aTableName: JLString; aValues: ACContentValues;
  aWhere: JLString; para4: Arr1JLString): jint;
begin
   getWritableDatabase.update(aTableName, aValues, aWhere, para4);
end;

function TDataBase.delete(aTableName: JLString; aWhere: JLString;
  para3: Arr1JLString): jint;
begin
  Result := getWritableDatabase.delete(aTableName, aWhere, para3);
end;

function TDataBase.rawQuery(aSQL: JLString; para2: Arr1JLString): ADCursor;
begin
  Result := getReadableDatabase.rawQuery(aSQL , para2);
end;

function TDataBase.query(para1: jboolean; para2: JLString; para3: Arr1JLString;
  para4: JLString; para5: Arr1JLString; para6: JLString; para7: JLString;
  para8: JLString; para9: JLString): ADCursor;
begin
  Result := getReadableDatabase.query(para1, para2, para3, para4, para5, para6, para7, para8, para9);
end;

procedure TDataBase.execSQL(aSQL: JLString);
begin
  getWritableDatabase.beginTransactionNonExclusive;
  try
    getWritableDatabase.execSQL(aSQL.toString);
    getWritableDatabase.setTransactionSuccessful;
    getWritableDatabase.endTransaction;
  except
    getWritableDatabase.endTransaction;
  end;
end;

procedure TDataBase.ExecuteDirect(aSQL: JLString);
begin
  getWritableDatabase.beginTransactionNonExclusive;
  try
   getWritableDatabase.compileStatement(aSQL).execute;
   getWritableDatabase.setTransactionSuccessful;
   getWritableDatabase.endTransaction;
  except
    getWritableDatabase.endTransaction;
  end;
end;

end.

