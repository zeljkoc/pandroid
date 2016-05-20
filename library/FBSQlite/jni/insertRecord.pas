unit insertRecord;

{$mode objfpc}{$H+}

interface

uses jni, sqlite3conn, sqldb, sysutils, IdTCPClient, Classes;

var
  fDataBaseName: string;
  fUserName: String;
  fPassword: string;
  fSQLiteDataBaseName: string;

function IndyConnect(env: PJNIEnv; this: jobject; host,port: jstring): jString; cdecl;
function IndyConnected(env: PJNIEnv; this: jobject): jboolean; cdecl;
function IndyDisconnect(env: PJNIEnv; this: jobject): jString; cdecl;


function SendSql(env: PJNIEnv; this: jobject; sql: jstring): jstring; cdecl;
function GetSql(env: PJNIEnv; this: jobject;  sql: jstring): jstring; cdecl;

function SetDataBaseParams(env: PJNIEnv; this: jobject; aDataBaseName: jstring; aUserName: jString; aPassword: jstring; aSQLiteDataBaseName: jstring): jint; cdecl;
function InsertFBTable (env: PJNIEnv; this: jobject; aSQLiteSelectSql: jstring; aFBInsertSql: jstring; aFBUpdateSql: jstring): jstring; cdecl;
function SelectFBTable (env: PJNIEnv; this: jobject; aFBSelectSql: jstring; aSQLiteInsertSQL: jstring; aSQLiteUpdateSQL: jstring): jstring; cdecl;


//HTTP
function HTTPSOAPPost(env: PJNIEnv; this: jobject; aUrl: jstring; aSOAPAction: jString; aXml: jString): JString; cdecl;


//Opste
function JNI_JStringToString(env: PJNIEnv; JStr: JString): string;
function JNI_StringToJString(env: PJNIEnv; Str: string): jstring;

var
   TCPClient: TidTCPClient;

implementation

uses IdGlobal, HTTPSend;

function SendRecord(AClient: TIdTCPClient; aRecord: TStringList): Boolean;
var
  StreamSize: LongInt;
  AStream: TMemoryStream;
begin
  try
    Result := True;
     AStream:= TMemoryStream.Create;
    try
      aRecord.SaveToStream(AStream);
      AStream.Position:=0;
      //-----------------------------
      StreamSize := (AStream.Size);

      AClient.IOHandler.Write(LongInt(StreamSize));
      AClient.IOHandler.WriteBufferOpen;
      AClient.IOHandler.Write(AStream, 0, False);
      AClient.IOHandler.WriteBufferFlush;
    finally
      //AClient.IOHandler.WriteBufferClose;
      AClient.IOHandler.CloseGracefully;
      AClient.DisconnectNotifyPeer;
      AStream.Free;
    end;
  except
    Result := False;
  end;

end;

function ReceiveRecord(AClient: TIdTCPClient; var aRecord: TStringList): Boolean;
var
  LSize: LongInt;
  AStream: TMemoryStream;
begin
  Result := True;
  AStream:= TMemoryStream.Create;
  try
     try
        LSize := AClient.IOHandler.ReadLongInt;
        AClient.IOHandler.ReadStream(AStream, LSize, False);
        //-----------------------  Podatak u AStream
        AStream.Position:=0;
        aRecord.LoadFromStream(AStream);
     except
        Result := False;
     end;

  finally
    AStream.Free;
  end;
end;

function SendSQLQuery(AClient: TIdTCPClient; aRecord: TSQLQuery): Boolean;
var
  StreamSize: LongInt;
  AStream: TMemoryStream;
begin
  try
    Result := True;
    AStream:= TMemoryStream.Create;
    try
      aRecord.SaveToStream(AStream);
      AStream.Position:=0;
      //-----------------------------
      StreamSize := (AStream.Size);

      AClient.IOHandler.Write(LongInt(StreamSize));
      AClient.IOHandler.WriteBufferOpen;
      AClient.IOHandler.Write(AStream, 0, false);
      AClient.IOHandler.WriteBufferFlush;
    finally
      //AClient.IOHandler.WriteBufferClose;
      AClient.IOHandler.CloseGracefully;
      AClient.DisconnectNotifyPeer;
      AStream.Free;
    end;
  except
    Result := False;
  end;

end;

function ReceiveSQLQuery(AClient: TIdTCPClient; var aRecord: TSQLQuery): Boolean;
var
  LSize: LongInt;
  AStream: TMemoryStream;
begin
  Result := True;
  AStream:= TMemoryStream.Create;
  try
     try
        LSize := AClient.IOHandler.ReadLongInt;
        AClient.IOHandler.ReadStream(AStream, LSize, False);
       //-----------------------  Podatak u AStream
        AStream.Position:=0;
        aRecord.LoadFromStream(AStream);
        if not (aRecord is TSQLQuery) Then Result := false;
     except
        Result := False;
     end;

  finally
    AStream.Free;
  end;
end;

function IndyConnect(env: PJNIEnv; this: jobject; host, port: jstring ): jString; cdecl;
begin
   Result :=  JNI_StringToJString(env, 'OK');
   try
     if not Assigned(TCPClient) then
       TCPClient:=TIdTCPClient.Create;
     if TCPClient.Connected then TCPClient.Disconnect;

     TCPClient.IPVersion:=id_Ipv4;
     TCPClient.Host:=trim(JNI_JStringToString(env, host));
     TCPClient.Port:=StrToInt(trim(JNI_JStringToString(env, port)));
     TCPClient.Connect;
   except
      on E: Exception do begin
          Result:=JNI_StringToJString(env,'Error: ' + E.Message);
          Exit;
      end;
   end;
end;

function IndyConnected(env: PJNIEnv; this: jobject): jboolean; cdecl;
begin
   try
     Result := JBoolean(TCPClient.Connected);
   except
      Result := JBoolean(false);
   end;
end;

function IndyDisconnect(env: PJNIEnv; this: jobject): jString; cdecl;
begin
  Result :=  JNI_StringToJString(env, 'OK');
  try
    if Assigned(TCPClient) then
    if TCPClient.Connected then begin
      TCPClient.Disconnect;
      TCPClient.Free;
      TCPClient:=nil;
    end;

  except
    on E: Exception do begin
       Result:=JNI_StringToJString(env,'Error: ' + E.Message);
       Exit;
    end;
  end;
end;

function SetDataBaseParams(env: PJNIEnv; this: jobject; aDataBaseName: jstring; aUserName: jString; aPassword: jstring; aSQLiteDataBaseName: jstring): jint; cdecl;
begin
 Result := 1;
 try
    fDataBaseName        := trim(JNI_JStringToString(env, aDataBaseName));
    fUserName            := trim(JNI_JStringToString(env, aUserName));
    fPassword            := trim(JNI_JStringToString(env, aPassword));
    fSQLiteDataBaseName  := trim(JNI_JStringToString(env, aSQLiteDataBaseName));
    SQLiteLibraryName := '/system/lib/libsqlite.so';
 except
   Result := 0;
 end;
end;

function SendSql(env: PJNIEnv; this: jobject; sql: jstring): jstring; cdecl;
var
   Rec: TStringList;
   temp: String;
begin
  Result:=JNI_StringToJString(env,'200OK');
   Rec:= TStringList.Create;
  try
    try
     // comanda
      if Assigned(TCPClient) then
        if TCPClient.Connected then begin

          //--------- SLANJE / PRIJEM
          Rec.Insert(0, 'SendSql');
          temp := trim(JNI_JStringToString(env, sql));   Rec.Insert(1, temp);
          temp := fDataBaseName;                         Rec.Insert(2, temp);
          temp := fUserName;                             Rec.Insert(3, temp);
          temp := fPassword;                             Rec.Insert(4, temp);

           if (SendRecord(TCPClient, Rec) = false) then begin
             Result:=JNI_StringToJString(env, 'Error send');
             Exit;
           end else begin
               if (ReceiveRecord(TCPClient, Rec) = false) then begin
                  Result:=JNI_StringToJString(env, 'Error receive');
                  Exit;
               end else begin
                   Result:=JNI_StringToJString(env, Rec.Strings[1]);
               end;
           end;
         //--------- SLANJE / PRIJEM
        end;
   except
      on E: Exception do
       Result:=JNI_StringToJString(env,'Error: ' + E.Message);
   end;

  finally
    Rec.Free;
  end;
end;

function GetSql(env: PJNIEnv; this: jobject; sql: jstring): jstring; cdecl;
var
   Rec: TStringList;
    temp: String;
begin
      Result:=JNI_StringToJString(env,'200OK');
      Rec:= TStringList.Create;
     try
       try
         // comanda
         if Assigned(TCPClient) then
           if TCPClient.Connected then begin

              //------------- SLANJE / PRIJEM
              Rec.Insert(0, 'GetSql');
              temp := trim(JNI_JStringToString(env, sql));   Rec.Insert(1, temp);
              temp :=  fDataBaseName;                        Rec.Insert(2, temp);
              temp :=  fUserName;                            Rec.Insert(3, temp);
              temp :=  fPassword;                            Rec.Insert(4, temp);

              if (SendRecord(TCPClient, Rec) = false) then begin
                 Result:=JNI_StringToJString(env, 'Error send');
                 Exit;
              end else begin
                 if (ReceiveRecord(TCPClient, Rec) = false) then begin
                  Result:=JNI_StringToJString(env, 'Error receive');
                  exit;
                 end else begin
                  Result:=JNI_StringToJString(env, Rec.Strings[1]);
                 end;
             end;
           //------------- PRIJEM
           end;
       except
         on E: Exception do
           Result:=JNI_StringToJString(env,'Error: ' + E.Message);
       end;

     finally
       Rec.Free;
     end;
end;

//----------------- InsertFBTable

function FSQliteToFBQuery(SQLite3Conn: TSQLite3Connection; SQLiteTrans: TSQLTransaction; aQuery: TSQLQuery; aSQLiteSelectSQL: string): string;
var

  i, j: integer;
begin
     Result :=  '200OK';

      //Select
          try
            SQLite3Conn.HostName:= 'localhost';
            SQLite3Conn.DatabaseName:= fSQLiteDataBaseName; //'/data/data/com.zeljus.replicate/databases/repl001.ldb';
            SQLite3Conn.Transaction := SQLiteTrans;
            aQuery.DataBase := SQLite3Conn;

            SQLite3Conn.Connected:=True;
            SQLiteTrans.Active:= True;

            aQuery.SQL.Text :=  aSQLiteSelectSQL;  //aSQLiteInsertSQL;

            if not aQuery.Active then aQuery.Open;
          //  SQLiteTrans.Commit;
          except
           on E: Exception do begin
            //  SQLiteTrans.Rollback;
              Result:= 'Error: ' + E.Message;
              Exit;
           end;
         end;
end;

function InsertFBTable(env: PJNIEnv; this: jobject; aSQLiteSelectSql: jstring; aFBInsertSql: jstring; aFBUpdateSql: jstring): jstring; cdecl;
var
   Rec: TStringList;
    temp: String;
    i: integer;
   SQLQuery: TSQLQuery;
   SQLite3Conn: TSQLite3Connection;
   SQLiteTrans: TSQLTransaction;
begin
      Result:=JNI_StringToJString(env,'200 OK');
      Rec:= TStringList.Create;
      SQLite3Conn:= TSQLite3Connection.Create(nil);
      SQLiteTrans:= TSQLTransaction.Create(nil);
      SQLQuery:= TSQLQuery.Create(nil);
     try
       try
         // comanda
         if Assigned(TCPClient) then
           if TCPClient.Connected then begin
             // TCPClient.ReadTimeout:= 10000;
               //------------- SLANJE / PRIJEM
              Rec.Insert(0, 'FBQuery');
              temp :=  trim(JNI_JStringToString(env, aFBInsertSql));  Rec.Insert(1, temp);
              temp :=  fDataBaseName;                                 Rec.Insert(2, temp);
              temp :=  fUserName;                                     Rec.Insert(3, temp);
              temp :=  fPassword;                                     Rec.Insert(4, temp);
              temp :=  trim(JNI_JStringToString(env, aFBUpdateSql));  Rec.Insert(5, temp);

              if (SendRecord(TCPClient, Rec) = false) then begin     //Slanje komande
                 Result:=JNI_StringToJString(env, 'Error send');
                 Exit;
              end else begin
                 if (ReceiveRecord(TCPClient, Rec) = false) then begin  //mogucnost slanja
                  Result:=JNI_StringToJString(env, 'Error receive');
                  exit;
                 end else begin    //Slanje Query
                    if Rec.Strings[1] = '200 Ok' then
                       Result:= JNI_StringToJString(env, FSQliteToFBQuery(SQLite3Conn, SQLiteTrans, SQLQuery, trim(JNI_JStringToString(env, aSQLiteSelectSql))));

                     if (SQLQuery is TSQLQuery) and (SendSQLQuery(TCPClient, SQLQuery) = false) then begin
                        Result:=JNI_StringToJString(env, 'Error Send Query');
                        exit;
                     end else begin
                         if (ReceiveRecord(TCPClient, Rec) = false) then  begin  //mogucnost slanja
                           Result:=JNI_StringToJString(env, 'No');
                           exit;
                        end else begin
                           Result:=JNI_StringToJString(env, Rec.Strings[1]);
                        end;
                     end;
                 end;
             end;
           //------------- Slanje
           end;
       except
         on E: Exception do
           Result:=JNI_StringToJString(env,'Error: ' + E.Message);
       end;

     finally
       SQLQuery.Free;
       SQLiteTrans.free;
       SQLite3Conn.Free;
       Rec.Free;
     end;
end;

//--------------------  SelectFBTable

function FBQueryToSQlite(aQuery: TSQLQuery; aSQLiteInsertSQL: string;  aSQLiteUpdateSQL: string): string;
var
  SQLite3Conn: TSQLite3Connection;
  SQLiteTrans: TSQLTransaction;
  SQLiteQuery: TSQLQuery;

  i, j: integer;
begin
     Result :=  '200OK';
   try
      SQLite3Conn:= TSQLite3Connection.Create(nil);
      SQLiteTrans:= TSQLTransaction.Create(nil);
      SQLiteQuery:= TSQLQuery.Create(nil);

        SQLite3Conn.HostName:= 'localhost';
        SQLite3Conn.DatabaseName:= fSQLiteDataBaseName; //'/data/data/com.zeljus.replicate/databases/repl001.ldb';
        SQLite3Conn.Transaction := SQLiteTrans;
        SQLiteQuery.DataBase := SQLite3Conn;

        SQLite3Conn.Connected:=True;
        SQLiteTrans.Active:= True;

        aQuery.First;
        while not aQuery.EOF  do begin
         // if SQLiteQuery.Active then SQLiteQuery.Close;
          SQLiteQuery.SQL.Text :=  aSQLiteInsertSQL;       //Insert
          for j:=0 to SQLiteQuery.Params.Count - 1 do
             SQLiteQuery.Params[j].Value := aQuery.FieldByName(SQLiteQuery.Params[j].Name).Value;
          if not SQLiteTrans.Active then SQLiteTrans.StartTransaction;
          try
            SQLiteQuery.ExecSQL;
            SQLiteTrans.Commit;
          except
            on E: Exception do begin
                SQLiteTrans.Rollback;
                if aSQLiteUpdateSQL = '' then begin
                      Result:= 'Error: ' + E.Message;
                      Exit;
                      end else begin
                        try
                        // if SQLiteQuery.Active then SQLiteQuery.Close;
                          SQLiteQuery.SQL.Text :=  aSQLiteUpdateSQL;   //Update
                          for j:=0 to SQLiteQuery.Params.Count - 1 do
                            SQLiteQuery.Params[j].Value := aQuery.FieldByName(SQLiteQuery.Params[j].Name).Value;

                          if not SQLiteTrans.Active then SQLiteTrans.StartTransaction;
                             //Update

                            SQLiteQuery.ExecSQL;
                            SQLiteTrans.Commit;
                          except
                                on E: Exception do begin
                                  SQLiteTrans.Rollback;
                                  Result:= 'Error: ' + E.Message;
                                  Exit;
                                end;
                          end;
                      end;

            end;
          end;
          aQuery.Next;
        end;

   finally
     SQLite3Conn.Free;
     SQLiteTrans.Free;
     SQLiteQuery.Free;
   end;

end;


function SelectFBTable(env: PJNIEnv; this: jobject; aFBSelectSql: jstring; aSQLiteInsertSQL: jstring;  aSQLiteUpdateSQL: jstring): jstring; cdecl;
var
   Rec: TStringList;
    temp: String;
    i: integer;
   SQLQuery: TSQLQuery;
begin
      Result:=JNI_StringToJString(env,'200OK');
      Rec:= TStringList.Create;
      SQLQuery:= TSQLQuery.Create(nil);
     try
       try
         // comanda
         if Assigned(TCPClient) then
           if TCPClient.Connected then begin
             // TCPClient.ReadTimeout:= 10000;
               //------------- SLANJE / PRIJEM
              Rec.Insert(0, 'SelectFBQuery');
              temp := trim(JNI_JStringToString(env, aFBSelectSql));   Rec.Insert(1, temp);
              temp :=  fDataBaseName;                        Rec.Insert(2, temp);
              temp :=  fUserName;                            Rec.Insert(3, temp);
              temp :=  fPassword;                            Rec.Insert(4, temp);

              if (SendRecord(TCPClient, Rec) = false) then begin
                 Result:=JNI_StringToJString(env, 'Error send');
                 Exit;
              end else begin
                 if (ReceiveSQLQuery(TCPClient, SQLQuery) = false) then begin
                  Result:=JNI_StringToJString(env, 'Error receive');
                  exit;
                 end else begin    //otvoriti Query
                    Result:= JNI_StringToJString(env, FBQueryToSQlite(SQLQuery, trim(JNI_JStringToString(env, aSQLiteInsertSQL)), trim(JNI_JStringToString(env, aSQLiteUpdateSQL))));
                 end;
             end;
           //------------- PRIJEM
           end;
       except
         on E: Exception do
           Result:=JNI_StringToJString(env,'Error: ' + E.Message);
       end;

     finally
       SQLQuery.Free;
       Rec.Free;
     end;
end;

//===================================================================================
function HTTPSOAPPost(env: PJNIEnv; this: jobject; aUrl: jstring; aSOAPAction: jString; aXml: jString): JString; cdecl;
var
   HTTP : THTTPSend;
   Ret: String;
begin
   HTTP := THTTPSend.Create;
   try
    HTTP.Headers.Add('SOAPAction'+JNI_JStringToString(env, aSOAPAction));
    HTTP.Protocol:='1.1';
    HTTP.MimeType := 'text/xml;charset=UTF-8';


    Ret := JNI_JStringToString(env, aXml);
    HTTP.Document.Write(Ret[1], Length(Ret)); Ret := '';
    if HTTP.HTTPMethod('POST', JNI_JStringToString(env, aUrl)) then begin
          SetLength(Ret, HTTP.Document.Size);
          HTTP.Document.ReadBuffer(Ret[1], HTTP.Document.Size);
          Result := JNI_StringToJString(env, Ret);
    end else Result := JNI_StringToJString(env, 'Error!: ');
   finally
     HTTP.Free;
   end;
end;

{

function HTTPSOAPPost(env: PJNIEnv; this: jobject; aServer: jString; aUrl: jstring; aSOAPAction: jString; aXml: jString): JString; cdecl;
var
  postData : TMemoryStream;
  IdHTTPPost: TIdHTTP;
  s: TStringList;


begin
  postData := TMemoryStream.Create;
  s := TStringList.Create;
  s.Add(JNI_JStringToString(env, aXml));
  s.SaveToStream(postData);
  //SynEdit1.Lines.SaveToStream(postData);
   postData.Position:= 0;
   IdHTTPPost:= TIdHTTP.Create;

   try
      IdHTTPPost.HandleRedirects:= true;

     //  IdHTTPPost.Get(JNI_JStringToString(env, aUrl));
     // fill postData with SOAP 1.1 xml as needed...
       IdHTTPPost.Request.ContentType := 'application/soap+xml; charset=utf-8';
      // IdHTTPPost.Request.Charset := 'utf-8';
       IdHTTPPost.Request.CustomHeaders.Values['SOAPAction'] := JNI_JStringToString(env, aSOAPAction);
       try
        //  IdHTTPPost.Connect(JNI_JStringToString(env, aUrl));
         //Result := JNI_StringToJString(env, IdHTTPPost.Get(JNI_JStringToString(env, aUrl)));
        Result := JNI_StringToJString(env, IdHTTPPost.post(JNI_JStringToString(env, aUrl),  postData));
       except  on E: Exception do
         Result := JNI_StringToJString(env, 'Error!: '+ E.Message+'  '+JNI_JStringToString(env, aUrl));
       end;
 finally
   s.Free;
   postData.Free;
   IdHTTPPost.Free;
 end;
end;
 }
//===================================================================================

//Java String Convert to Pascal String
function JNI_JStringToString(env: PJNIEnv; JStr: JString): string;
var
  pAnsiCharTMP: pAnsiChar;
  pIsCopy: Byte;
begin
   // IsCopy := JNI_TRUE;

    if (JStr = nil) then begin
      Result := '';
      Exit;
    end;

     pAnsiCharTMP := env^^.GetStringUTFChars(env, JStr, @pIsCopy);

    if (pAnsiCharTMP = nil) then begin
      Result := '';
    end else begin
      Result := StrPas(pAnsiCharTMP); //Return the result;
       //Release the temp string
      Env^^.ReleaseStringUTFChars(env, JStr, pAnsiCharTMP);
    end;

end;

// Pascal String Convert to Java String
function JNI_StringToJString(env: PJNIEnv; Str: string): jstring;
begin
  Result := env^^.NewStringUTF(env, @Str[1]);
end;

end.

