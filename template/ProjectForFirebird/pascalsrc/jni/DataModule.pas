unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, uib;

{ TDataM }

type
  TDataM = class(TDataModule)
    SQLite3Conn: TSQLite3Connection;
    SQLiteTransaction: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  protected
    procedure SetSQLiteDatabaseName(aValue: String);
    function SQLiteToFirebird(Source, Target: String): boolean;
    function FirebirdToSQLite(Source, Target: String): boolean;
  private
    { private declarations }
    procedure SetParameters(Value: String);
    function GetRefreshDatabase: String;
    function GetSendDatabase: String;
  public
    { public declarations }
    UIBDataBase: TUIBDataBase;
    UIBTransaction: TUIBTransaction;
  published
    property Parameters: String write SetParameters;
    property RefreshDatabase: String read GetRefreshDatabase;
    property SendDatabase: String read GetSendDatabase;
  end;

{var
  DataM: TDataM;  }

implementation

{$R *.lfm}

uses uiblib, uibdataset, EncodeDecodeCodePage;


{ TDataM }

function TDataM.GetRefreshDatabase: String;
begin
  try
    Result := 'NotOk';
    //sJMjere
  {  if SQLiteToFirebird('SELECT r."JMjereID", r."JMjere" FROM "sJMjere" r ',
      'insert OR REPLACE into "sJMjere" ("JMjereID", "JMjere") values (:JMjereID, :JMjere) ') then Result := 'OK';  }


  except on E: Exception do
    Result := E.Message;
  end;
  SQLite3Conn.Connected := False;
  UIBDataBase.Connected := false;
end;

function TDataM.GetSendDatabase: String;
begin
  try
    Result := 'NotOk';


  except on E: Exception do
    Result := E.Message;
  end;
  SQLite3Conn.Connected := False;
  UIBDataBase.Connected := false;
end;

//----------------------------------------------------------------

procedure TDataM.DataModuleCreate(Sender: TObject);
begin
  //onCrete
 UIBDataBase:= TUIBDataBase.Create(nil);
 UIBDataBase.CharacterSet :=  csWIN1250;
 UIBDataBase.LibraryName := '/system/lib/libfbclient.so.3.0.1';

 UIBTransaction:= TUIBTransaction.Create(nil);
 UIBTransaction.DataBase := UIBDataBase;
end;

procedure TDataM.DataModuleDestroy(Sender: TObject);
begin
  UIBTransaction.Free;
  UIBDataBase.Free;
end;

procedure TDataM.SetSQLiteDatabaseName(aValue: String);
begin
  SQLiteLibraryName := '/system/lib/libsqlite.so';
  SQLite3Conn.HostName := 'localhost';
 // SQLite3Conn.CharSet := 'UTF-8';
  SQLite3Conn.DatabaseName := aValue;
  SQLite3Conn.Transaction := SQLiteTransaction;

  SQLite3Conn.Connected := true;
end;

//----------------------------------------------

function TDataM.SQLiteToFirebird(Source, Target: String): boolean;
var
  sqlSource: TUIBDataSet;
  sqlTarget: TSQLQuery;
  i: integer;
begin
    if (not UIBDataBase.Connected)  then UIBDataBase.Connected := True;
   sqlSource:= TUIBDataSet.Create(nil);
   sqlTarget:= TSQLQuery.Create(nil);
   try
     sqlSource.Transaction := UIBTransaction;
     sqlSource.SQL.Add(Source);
     sqlSource.Open;

     sqlTarget.DataBase := SQLite3Conn;
     sqlTarget.Transaction := SQLiteTransaction;

     SQLite3Conn.Connected :=  True;
     sqlTarget.SQL.Clear;
     sqlTarget.SQL.Add(Target);
   try
     while not sqlSource.EOF do begin
       for i:=0 to sqlTarget.Params.Count -1 do
       sqlTarget.Params[i].Value := CP1250ToUTF8(sqlSource.Fields[i].Value);
      // sqlTarget.Params[i].Value := sqlSource.Fields[i].Value;
       sqlTarget.ExecSQL;
       SQLiteTransaction.Commit;
       sqlSource.Next;
     end;
     Result := true;
   except
      SQLiteTransaction.Rollback;
      Result := false;
   end;

   finally
     sqlTarget.Free;
     sqlSource.Free;
   end;
end;

function TDataM.FirebirdToSQLite(Source, Target: String): boolean;
var
  sqlSource: TSQLQuery;
  sqlTarget: TUIBDataSet;
  i: integer;
begin
   if (not UIBDataBase.Connected)  then UIBDataBase.Connected := True;
   sqlSource:= TSQLQuery.Create(nil);
   sqlTarget:= TUIBDataSet.Create(nil);
   try
       sqlSource.DataBase := SQLite3Conn;
       sqlSource.Transaction := SQLiteTransaction;
       SQLite3Conn.Connected :=  True;
       sqlSource.SQL.Clear;
       sqlSource.SQL.Add(Source);
       sqlSource.Open;

       UIBTransaction.AutoStart := true;
       UIBTransaction.AutoStop := true;
       sqlTarget.Database := UIBDataBase;
       sqlTarget.Transaction := UIBTransaction;
       sqlTarget.SQL.Add(Target);

       try
     		  while not sqlSource.EOF do begin
             for i:=0 to sqlTarget.Params.ParamCount -1 do
      	       sqlTarget.Params.AsVariant[i]  := UTF8ToCP1250(sqlSource.Fields[i].Value);
              // sqlTarget.Params.AsVariant[i] := sqlSource.Fields[i].Value;
             sqlTarget.ExecSQL;
             sqlSource.Next;
          end;
          Result := true;
       except
          Result := false;
       end;

   finally
     sqlTarget.Free;
     sqlSource.Free;
   end;
end;


procedure TDataM.SetParameters(Value: String);
var
  parametri: TStringList;
  i: integer;
begin
  parametri:= TStringList.Create;
  try
    parametri.StrictDelimiter := true;
    parametri.Delimiter := ';';
    parametri.DelimitedText := Value;

    for i:=0 to parametri.Count -1 do begin
      case parametri.Names[i] of
           'SQLiteDatabaseName': SetSQLiteDatabaseName(parametri.ValueFromIndex[i]);
           'Database': UIBDataBase.DatabaseName := parametri.ValueFromIndex[i];
           'UserName': UIBDataBase.UserName := parametri.ValueFromIndex[i];
           'Password': UIBDataBase.PassWord := parametri.ValueFromIndex[i];
      end;
    end;

  finally
    parametri.Free;
  end;

end; 

end.

