{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
unit zcFirebird;

{$mode objfpc}{$H+}


interface

uses jni, Classes, sysutils,
     uib, uiblib, uibdataset,
     JNIUtils;


var
  ZCUIBDataBase: TUIBDataBase;
  ZCUIBTransaction: TUIBTransaction;
  ZCUIBDataSet: TUIBDataSet;

//UIBDataBase
procedure Java_zeljus_com_firebird_UIBDataBase_Init(env: PJNIEnv; this: jobject; DataBaseName: jString; CharSet: jString; LibraryName: jString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataBase_setUserNamePassword(env: PJNIEnv; this: jobject; UserName: jString; Password: jString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataBase_Connect(env: PJNIEnv; this: jobject): JString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataBase_Disconnect(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataBase_isConnected(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

//UIBDataSet
procedure Java_zeljus_com_firebird_UIBDataSet_Init(env: PJNIEnv; this: jobject; SQL: jString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_Open(env: PJNIEnv; this: jobject): JString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_Close(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_isActive(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_ExecSQL(env: PJNIEnv; this: jobject): JString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

procedure Java_zeljus_com_firebird_UIBDataSet_Edit(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_Post(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_Next(env: PJNIEnv; this: jobject);{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_Prior(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_EOF(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_BOF(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_First(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_Last(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_setRecNo(env: PJNIEnv; this: jobject; RecNo: jint); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_getRecNo(env: PJNIEnv; this: jobject): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

function  Java_zeljus_com_firebird_UIBDataSet_FieldCount(env: PJNIEnv; this: jobject): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_FieldName(env: PJNIEnv; this: jobject; FieldNo: jint): jString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}

function  Java_zeljus_com_firebird_UIBDataSet_RecordCount(env: PJNIEnv; this: jobject): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_getAsString(env: PJNIEnv; this: jobject; FieldNo: jint): jString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
function  Java_zeljus_com_firebird_UIBDataSet_getAsInteger(env: PJNIEnv; this: jobject; FieldNo: jint): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_setAsString(env: PJNIEnv; this: jobject; FieldNo: jint; Value: JString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
procedure Java_zeljus_com_firebird_UIBDataSet_setAsInteger(env: PJNIEnv; this: jobject; FieldNo: jint; Value: JInt); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}


implementation


//UIBDataBase
procedure Java_zeljus_com_firebird_UIBDataBase_Init(env: PJNIEnv; this: jobject;
               DataBaseName: jString; CharSet: jString; LibraryName: jString ); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
var
  CS : String;
begin
    try
      if not Assigned(ZCUIBDataBase) then begin
          ZCUIBDataBase:= TUIBDataBase.Create(nil);
          ZCUIBDataBase.DataBaseName:= JNI_JStringToString(env, DataBaseName);

          CS :=  JNI_JStringToString(env, CharSet);
          if  CS = 'WIN1250' then ZCUIBDataBase.CharacterSet:= csWIN1250 else
          if  CS = 'UTF8' then ZCUIBDataBase.CharacterSet:= csUTF8 else
          ZCUIBDataBase.CharacterSet := csNONE;

          ZCUIBDataBase.LibraryName:= JNI_JStringToString(env, LibraryName);
      end;

      if not Assigned(ZCUIBTransaction) then
        ZCUIBTransaction:= TUIBTransaction.Create(nil);

      ZCUIBTransaction.DataBase := ZCUIBDataBase;
    except
         Exit;
    end;
end;

procedure Java_zeljus_com_firebird_UIBDataBase_setUserNamePassword(
  env: PJNIEnv; this: jobject; UserName: jString; Password: jString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
   ZCUIBDataBase.UserName:= JNI_JStringToString(env, UserName);
   ZCUIBDataBase.PassWord:= JNI_JStringToString(env, Password);
end;

function Java_zeljus_com_firebird_UIBDataBase_Connect(env: PJNIEnv; this: jobject): JString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
   Result := JNI_StringToJString(env, 'OK');
   try
     if not ZCUIBDataBase.Connected then ZCUIBDataBase.Connected:= True;
   except on E: Exception do begin
          Result:=JNI_StringToJString(env,'Error: ' + E.Message);
          Exit;
      end;
   end;
end;

procedure Java_zeljus_com_firebird_UIBDataBase_Disconnect(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  if ZCUIBDataBase.Connected then ZCUIBDataBase.Connected:= False;
end;

function Java_zeljus_com_firebird_UIBDataBase_isConnected(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  if ZCUIBDataBase.Connected then Result := JNI_TRUE else Result := JNI_FALSE;;
end;

//UIBDataSet
procedure Java_zeljus_com_firebird_UIBDataSet_Init(env: PJNIEnv; this: jobject; SQL: jString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    try
      if not Assigned(ZCUIBDataSet) then  ZCUIBDataSet:= TUIBDataSet.Create(nil);

      ZCUIBDataSet.SQL.Text := JNI_JStringToString(env, SQL);

      ZCUIBDataSet.Database := ZCUIBDataBase;
      ZCUIBDataSet.Transaction := ZCUIBTransaction;
    except
         Exit;
    end;
end;

function Java_zeljus_com_firebird_UIBDataSet_Open(env: PJNIEnv; this: jobject): JString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    Result := JNI_StringToJString(env, 'OK');
    try
      if (not ZCUIBDataSet.Active) then ZCUIBDataSet.Open;
    except on E: Exception do begin
           Result:=JNI_StringToJString(env,'Error: ' + E.Message);
           Exit;
       end;
    end;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_Close(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  if ZCUIBDataSet.Active then ZCUIBDataSet.Close;
end;

function Java_zeljus_com_firebird_UIBDataSet_isActive(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
   if ZCUIBDataSet.Active then Result := JNI_TRUE else  Result := JNI_FALSE;
end;

function Java_zeljus_com_firebird_UIBDataSet_ExecSQL(env: PJNIEnv; this: jobject): JString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    Result := JNI_StringToJString(env, 'OK');
    try
      ZCUIBDataSet.ExecSQL;
    except on E: Exception do begin
           Result:=JNI_StringToJString(env,'Error: ' + E.Message);
           Exit;
       end;
    end;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_Edit(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.Edit;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_Post(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.Post;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_Next(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.Next;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_Prior(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.Prior;
end;

function Java_zeljus_com_firebird_UIBDataSet_EOF(env: PJNIEnv; this: jobject): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  if ZCUIBDataSet.EOF then Result := JNI_TRUE else Result := JNI_FALSE;
end;

function Java_zeljus_com_firebird_UIBDataSet_BOF(env: PJNIEnv; this: jobject ): jboolean; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    if ZCUIBDataSet.BOF then Result := JNI_TRUE else Result := JNI_FALSE;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_First(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    ZCUIBDataSet.First;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_Last(env: PJNIEnv; this: jobject); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    ZCUIBDataSet.Last;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_setRecNo(env: PJNIEnv; this: jobject; RecNo: jint); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.RecNo:= RecNo;
end;

function Java_zeljus_com_firebird_UIBDataSet_getRecNo(env: PJNIEnv; this: jobject): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  Result :=  ZCUIBDataSet.RecNo;
end;

function Java_zeljus_com_firebird_UIBDataSet_RecordCount(env: PJNIEnv; this: jobject): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  Result :=  ZCUIBDataSet.RecordCount;
end;

function Java_zeljus_com_firebird_UIBDataSet_FieldCount(env: PJNIEnv; this: jobject): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  Result := ZCUIBDataSet.FieldCount;
end;

function Java_zeljus_com_firebird_UIBDataSet_FieldName(env: PJNIEnv; this: jobject; FieldNo: jint): jString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
    Result := JNI_StringToJString(env,ZCUIBDataSet.Fields[FieldNo].FieldName );
end;

function Java_zeljus_com_firebird_UIBDataSet_getAsString(env: PJNIEnv; this: jobject; FieldNo: jint): jString; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  Result := JNI_StringToJString(env,ZCUIBDataSet.Fields[FieldNo].AsString );
end;

function Java_zeljus_com_firebird_UIBDataSet_getAsInteger(env: PJNIEnv; this: jobject; FieldNo: jint): jint; {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  Result :=  ZCUIBDataSet.Fields[FieldNo].AsInteger;
end;

procedure Java_zeljus_com_firebird_UIBDataSet_setAsString(env: PJNIEnv; this: jobject; FieldNo: jint; Value: JString); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.Fields[FieldNo].AsString:= JNI_JStringToString(env, Value);
end;

procedure Java_zeljus_com_firebird_UIBDataSet_setAsInteger(env: PJNIEnv; this: jobject; FieldNo: jint; Value: JInt); {$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
begin
  ZCUIBDataSet.Fields[FieldNo].AsInteger :=  Value;
end;





end.

