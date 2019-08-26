unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdTCPClient,  sqldb, SQLite3Conn, uReplicate;

{ TDataM }

type
  TDataM = class(TDataModule)
    Query: TSQLQuery;
    TCPClient: TIdTCPClient;
    procedure DataModuleCreate(Sender: TObject);
  protected
    function RemoteFirebirdQuery(sSelect: String): TSQLQuery;
    function ExecuteSQLRemoteFirebird(aSQL: String): String;
  private
    fRemoteSQL: String;
    fSelectQuery: String;
    fInsertQuery: String;
    fFieldNo: integer;
    fRecordNo: integer;
    fIdentifikacija: String;
    fAndroidID: String;
    //sql promjenljive
    fIDPartner: String;
    fIDPda: String;
    //-end sql promjenljive
    fSQLiteDataBaseName: string;
    function GetAsString: String;
    function GetFieldCount: integer;
    function GetQueryToSQLite: String;
    function GetQueryToSQLiteRedniBroj: String;
    function GetRecordCount: integer;
    function GetRecordNo: integer;
    function GetRefreshQuery: String;
    function GetRemoteExecuteSQL: String;
    function GetReplicate: String;
    function GetTCPClientDisconect: String;
    { private declarations }
    procedure SetParameters(Value: String);
    procedure SetRecordNo(AValue: integer);
  public
    { public declarations }
  published
    property Parameters: String write SetParameters;

    // Query Command
    property SelectQuery: String write fSelectQuery;
    property RefreshQuery: String read GetRefreshQuery;
    property RecordCount: integer read GetRecordCount;
    property FieldCount: integer read GetFieldCount;
    property FieldNo: integer write fFieldNo;
    property RecordNo: integer read GetRecordNo write SetRecordNo;
    property AsString: String read GetAsString;

    property RemoteSQL: String write fRemoteSQL;
    property RemoteExecuteSQL: String read GetRemoteExecuteSQL;

    property QueryToSQLite: String read GetQueryToSQLite;
    property QueryToSQLiteRedniBroj: String read GetQueryToSQLiteRedniBroj;

    property TCPClientDisconect: String read GetTCPClientDisconect;

    property Replicate: String read GetReplicate;
  end;

{var
  DataM: TDataM;  }

implementation

{$R *.lfm}

function SendStream(AClient: TIdTCPClient; AStream: TStream): Boolean; overload;
var
  StreamSize: LongInt;
begin
  try
    Result := True;
    try
      StreamSize := (AStream.Size);

      // AStream.Seek(0, soFromBeginning);
      // AClient.IOHandler.LargeStream := True;
      // AClient.IOHandler.SendBufferSize := 32768;

      AClient.IOHandler.Write(LongInt(StreamSize));
      AClient.IOHandler.WriteBufferOpen;
      AClient.IOHandler.Write(AStream, 0, False);
      AClient.IOHandler.WriteBufferFlush;
    finally
      AClient.IOHandler.WriteBufferClose;
    end;
  except
    Result := False;
  end;
end;


function ReceiveStream(AClient: TIdTCPClient; var AStream: TStream): Boolean; overload;
var
  LSize: LongInt;
begin
  Result := True;
  try
    LSize := AClient.IOHandler.ReadLongInt();
    AClient.IOHandler.ReadStream(AStream, LSize, False);
  except
    Result := False;
  end;
end;


{ TDataM }

procedure TDataM.SetRecordNo(AValue: integer);
begin
  if (aValue <= Query.RecordCount) and (aValue >= 0) then fRecordNo:= aValue;
end;

//----------------------------------------------------------------

procedure TDataM.DataModuleCreate(Sender: TObject);
begin
//
  SQLiteLibraryName := '/system/lib/libsqlite.so';
 // InitreplParams;
end;


//----------------------------------------------

function TDataM.RemoteFirebirdQuery(sSelect: String): TSQLQuery;
var
  Data: TMemoryStream;
  Komanda : String;
begin
     if not TCPClient.Connected then TCPClient.Connect;
    Data:= TMemoryStream.Create;
    try
        Komanda := 'SelectRemote';

        Data.Clear;
        Data.WriteAnsiString(Komanda);
        Data.WriteAnsiString(sSelect);

         if (SendStream(TCPClient, TStream(Data)) = False) then begin   // Slanje zahtjeva
               Exit;
          end;

          Data.Clear;
          if (ReceiveStream(TCPClient, TStream(Data) ) = False) then begin  // odgovor
            Exit;
          end else begin
         //   Query.Clear;
            Data.Position:=0;
            Query.LoadFromStream(Data);
            Result := Query;
          end;
    finally
    //  if TCPClient.Connected then  TCPClient.Disconnect;
      Data.Free;
    end;
end;

function TDataM.ExecuteSQLRemoteFirebird(aSQL: String): String;
var
  Data: TMemoryStream;
  Komanda: String;
begin
 if not TCPClient.Connected then TCPClient.Connect;

  Data:= TMemoryStream.Create;
  Komanda := 'ExecSQL';

  Data.WriteAnsiString(Komanda);
  aSQL:= 'Identifikacija='+fIdentifikacija +';AndroidID='+fAndroidID+';Query='+aSQL;         //zc
  Data.WriteAnsiString(aSQL);

  try
       if (SendStream(TCPClient, TStream(Data)) = False) then begin   // Slanje zahtjeva
          Exit;
        end;

        Data.Clear;
        if (ReceiveStream(TCPClient, TStream(Data) ) = False) then begin  // odgovor
          Exit;
        end else begin
          Data.Position:=0;
          Komanda := Data.ReadAnsiString;
          Result := Data.ReadAnsiString;
        end;
  finally
    Data.Free;
  //  TCPClient.Disconnect;
  end;
end;

function TDataM.GetAsString: String;
begin
  Query.RecNo:= fRecordNo;
  Result := Query.Fields[fFieldNo].AsString;
end;

function TDataM.GetFieldCount: integer;
begin
  Result := Query.FieldCount;
end;


function TDataM.GetQueryToSQLite: String;
var
  SQLite3Conn: TSQLite3Connection;
  SQLiteTrans: TSQLTransaction;
  SQLiteQuery: TSQLQuery;

  i, j: integer;
begin
  Result := 'Refresh Ok';

  SQLite3Conn:= TSQLite3Connection.Create(nil);
  SQLiteTrans:= TSQLTransaction.Create(nil);
  SQLiteQuery:= TSQLQuery.Create(nil);

  SQLite3Conn.HostName:= 'localhost';
  SQLite3Conn.DatabaseName:= fSQLiteDataBaseName; // := '/data/data/com.zeljus.replicate/databases/repl001.ldb';
  SQLite3Conn.Transaction := SQLiteTrans;
  SQLiteQuery.DataBase := SQLite3Conn;

  SQLite3Conn.Connected:=True;
  SQLiteTrans.Active:= True;

   try
        for i:= 0 to MReplReceive.BrojTabela - 1  do begin
                  if Query.Active then Query.Close;
                  Query := RemoteFirebirdQuery('Identifikacija='+fIdentifikacija +';AndroidID='+fAndroidID+';Query='+MReplReceive.SQLSelect[i]);
                  SQLiteQuery.SQL.Text := MReplReceive.SQLInsert[i]; // fInsertQuery;   //Update

                  fRecordNo:=0; fFieldNo:= 0;

                  if Query.RecordCount > 0 then begin
                       Query.First;
                       while not Query.EOF  do begin
                           try
                              if SQLiteQuery.Active then SQLiteQuery.Close;
                              for j:=0 to SQLiteQuery.Params.Count - 1 do
                                SQLiteQuery.Params[j].Value := Query.Fields[j].Value; //  Query.Fields[j].Value; // Query.FieldByName(SQLiteQuery.Params[j].Name).Value;

                              if not SQLiteTrans.Active then SQLiteTrans.StartTransaction;
                              SQLiteQuery.ExecSQL;
                              SQLiteTrans.Commit;
                           except on E: Exception do begin
                                      SQLiteTrans.Rollback;
                                      Result:= 'Error: ' + IntToStr(i)+' '+ E.Message;
                                      Exit;
                                  end;
                           end;

                         Query.Next;
                       end;
                  end;

            Sleep(200);
        end;
   finally
     if TCPClient.Connected then TCPClient.Disconnect;
     SQLite3Conn.Free;
     SQLiteTrans.Free;
     SQLiteQuery.Free;
   end;
end;

function TDataM.GetQueryToSQLiteRedniBroj: String;
var
  SQLite3Conn: TSQLite3Connection;
  SQLiteTrans: TSQLTransaction;
  SQLiteQuery: TSQLQuery;

  i, j: integer;

begin
  Result := 'Start';

  SQLite3Conn:= TSQLite3Connection.Create(nil);
  SQLiteTrans:= TSQLTransaction.Create(nil);
  SQLiteQuery:= TSQLQuery.Create(nil);

  SQLite3Conn.HostName:= 'localhost';
  SQLite3Conn.DatabaseName:= fSQLiteDataBaseName; // := '/data/data/com.zeljus.replicate/databases/repl001.ldb';
  SQLite3Conn.Transaction := SQLiteTrans;
  SQLiteQuery.DataBase := SQLite3Conn;

  SQLite3Conn.Connected:=True;
  SQLiteTrans.Active:= True;

   try
        for i:= 0 to MReplReceiveRedniBroj.BrojTabela - 1  do begin
                 SQLiteQuery.SQL.Text := MReplReceiveRedniBroj.SQLSelectLite[i];
                 SQLiteQuery.Open;

                if SQLiteQuery.RecordCount > 0 then begin
                          if Query.Active then Query.Close;
                          Query := RemoteFirebirdQuery('Identifikacija='+fIdentifikacija +';AndroidID='+fAndroidID+';Query='+
                          stringreplace(MReplReceiveRedniBroj.SQLSelect[i], ':RedniBroj', IntToStr(SQLiteQuery.RecordCount), [rfReplaceAll, rfIgnoreCase]  ));


                          fRecordNo:=0; fFieldNo:= 0;

                          if Query.RecordCount > 0 then begin
                               Query.First;

                               SQLiteQuery.First;  j:= query.Fields[0].AsInteger;
                               while not SQLiteQuery.EOF do begin
                                 SQLiteQuery.edit;
                                 SQLiteQuery.Fields[1].AsInteger:= j;
                                 SQLiteQuery.Post;
                                 SQLiteQuery.Next;
                                 j := j - 1;
                               end;

                               if SQLiteQuery.ChangeCount <> 0 then begin
                                    try
                                       if not SQLiteTrans.Active then SQLiteTrans.StartTransaction;
                                       SQLiteQuery.ApplyUpdates(-1);
                                       SQLiteTrans.Commit;
                                    except on E: Exception do begin
                                              SQLiteTrans.Rollback;
                                              Result:= 'Error: '+' '+ E.Message;
                                              Exit;
                                          end;
                                   end;
                               end;

                          end;
            end;

            Sleep(200);
        end;
   finally
     if TCPClient.Connected then TCPClient.Disconnect;
     SQLite3Conn.Free;
     SQLiteTrans.Free;
     SQLiteQuery.Free;
   end;
   Result := 'RBrOK';
end;

function TDataM.GetRecordCount: integer;
begin
  Result := Query.RecordCount;
end;

function TDataM.GetRecordNo: integer;
begin
  Result := Query.RecNo;
end;


function TDataM.GetRefreshQuery: String;
begin
  Result := 'Ok';
  try
    if Query.Active then Query.Close;
    Query := RemoteFirebirdQuery(fSelectQuery);
    fRecordNo:=0;
    fFieldNo:= 0;
  except on E: Exception do
      Result := 'Error'+ E.Message;
  end;
end;

function TDataM.GetRemoteExecuteSQL: String;
begin
     Result := ExecuteSQLRemoteFirebird(fRemoteSQL);
end;

function TDataM.GetReplicate: String;    //Slanje novi i izmjena
var
  SQLite3Conn: TSQLite3Connection;
  SQLiteTrans: TSQLTransaction;
  SQLiteRepl: TSQLQuery;
  SQLiteData: TSQLQuery;

  i: integer;
  Odgovor: String;


    Function ChangeParams(aValue: String): String;
    var
      j: integer;
    begin
      for j:= 0 to SQLiteData.FieldCount - 1 do
           aValue:= stringreplace(aValue, ':'+SQLiteData.Fields[j].FieldName,  SQLiteData.Fields[j].asString, [rfReplaceAll, rfIgnoreCase]);

      aValue:= stringreplace(aValue, ':IDPartner', fIDPartner, [rfReplaceAll, rfIgnoreCase]);
      aValue:= stringreplace(aValue, ':IDPda', fIDPda, [rfReplaceAll, rfIgnoreCase]);

      Result := aValue;
    end;

begin
   //  Result := ExecuteSQLRemoteFirebird('insert into "BarKod" ("BarKodID", "NazivProizvoda", "KolicinaPakovanja") values (100, ''Zeljus'', 1 )');

  Result := 'Work';

  SQLite3Conn:= TSQLite3Connection.Create(nil);
  SQLiteTrans:= TSQLTransaction.Create(nil);
  SQLiteRepl:= TSQLQuery.Create(nil);
  SQLiteData := TSQLQuery.Create(nil);

  SQLite3Conn.HostName:= 'localhost';
  SQLite3Conn.DatabaseName:= fSQLiteDataBaseName; //'/data/data/com.zeljus.replicate/databases/repl001.ldb';
  SQLite3Conn.Transaction := SQLiteTrans;
  SQLiteRepl.DataBase := SQLite3Conn;
  SQLiteData.DataBase := SQLite3Conn;

  SQLite3Conn.Connected:=True;
  SQLiteTrans.Active:= True;
 try
   SQLiteRepl.SQL.Text :=  'select ReplID, Promjena, Tabela, Kljuc, coalesce(Kljuc2, '''') as Kljuc2, Greska from REPL_LOG where (Greska = ''repl'') order by ReplID ';
   SQLiteRepl.Open;
   SQLiteRepl.First;
   while not SQLiteRepl.EOF do begin
       for i:=0 to MReplSend.BrojTabela - 1 do begin
         if SQLiteData.Active then SQLiteData.Close;
         SQLiteData.SQL.Clear;
         if (SQLiteRepl.FieldByName('Tabela').AsString = MReplSend.TableNameSource[i]) then begin
               Case SQLiteRepl.FieldByName('Promjena').asString of
                  'I':  SQLiteData.SQL.Add(MReplSend.SQLInsertSource[i]);
                  'U':  SQLiteData.SQL.Add(MReplSend.SQLUpdateSource[i]);
                  'D':  SQLiteData.SQL.Add(MReplSend.SQLDeleteSource[i]);
               end;
               if  ( (SQLiteRepl.FieldByName('Promjena').asString = 'I') and (MReplSend.SQLInsertSource[i] <> '')) or
                   ((SQLiteRepl.FieldByName('Promjena').asString = 'U') and (MReplSend.SQLUpdateSource[i] <> '') ) or
                   ((SQLiteRepl.FieldByName('Promjena').asString = 'D') and (MReplSend.SQLDeleteSource[i] <> '')) then begin
                       if SQLiteRepl.FieldByName('Kljuc2').Value = '' then
                         SQLiteData.ParamByName('Kljuc').Value:= SQLiteRepl.FieldByName('Kljuc').Value
                       else
                         SQLiteData.ParamByName('Kljuc2').Value:= SQLiteRepl.FieldByName('Kljuc2').Value;

                   SQLiteData.Active:= true;
               end;

               Case SQLiteRepl.FieldByName('Promjena').asString of
                  'I': Odgovor := ExecuteSQLRemoteFirebird(ChangeParams(MReplSend.SQLInsertTarget[i]));
                  'U': Odgovor := ExecuteSQLRemoteFirebird(ChangeParams(MReplSend.SQLUpdateTarget[i]));
                  'D': Odgovor := ExecuteSQLRemoteFirebird(stringreplace(ChangeParams(MReplSend.SQLDeleteTarget[i]), ':Kljuc', SQLiteRepl.FieldByName('Kljuc').Value, [rfReplaceAll, rfIgnoreCase]) );
               end;
            end;
            SQLiteRepl.Edit;
            SQLiteRepl.FieldByName('Greska').AsString:= Odgovor;
            SQLiteRepl.Post;
         end;

       SQLiteRepl.Next;
   end;
   //---------------
   try
     if not SQLiteTrans.Active then SQLiteTrans.StartTransaction;    //Snimi odgovore
     SQLiteRepl.ApplyUpdates(0);
     SQLiteTrans.Commit;
   except on E: Exception do begin
       Result := 'Error'+ E.Message;
       SQLiteTrans.Rollback;
     end;
   end;
   //----------------------
 finally
   SQLite3Conn.Free;
   SQLiteTrans.Free;
   SQLiteRepl.Free;
   SQLiteData.Free;
 end;

 Result := 'End';

end;

function TDataM.GetTCPClientDisconect: String;
begin
  Result := 'Restart TCPClient!!!';
  try
   if TCPClient.Connected then  TCPClient.Disconnect;
  except on E: Exception do
      Result := 'Error'+ E.Message;
  end;
end;

procedure TDataM.SetParameters(Value: String);
var
  parametri: TStringList;
  i: integer;
begin
 //init sql parametara
 fIDPartner:= '1';
 fIDPda:= '100';
 //

  parametri:= TStringList.Create;
  try
    parametri.StrictDelimiter := true;
    parametri.Delimiter := ';';
    parametri.DelimitedText := Value;

    for i:=0 to parametri.Count -1 do begin
      case parametri.Names[i] of
           'SQLiteDatabaseName': fSQLiteDataBaseName := parametri.ValueFromIndex[i];
           'Host'              : TCPClient.Host := parametri.ValueFromIndex[i];
           'Port'              : TCPClient.Port := StrToInt(parametri.ValueFromIndex[i]);
           'Identifikacija'    : fIdentifikacija:= parametri.ValueFromIndex[i];
           'AndroidID'         : fAndroidID     := parametri.ValueFromIndex[i];
           //SQL parametri
           'IDPartner'         : fIDPartner     := parametri.ValueFromIndex[i];
           'IDPda'             : fIDPda         := parametri.ValueFromIndex[i];
      end;
    end;

  finally
    parametri.Free;
  end;
  InitreplParams(fIDPda, fIDPartner);
end; 

end.

