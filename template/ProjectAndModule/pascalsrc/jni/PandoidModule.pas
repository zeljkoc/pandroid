{***********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
unit PandoidModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, jni, JNIUtils, DataModule;

var
 Module : TStringList;

function Java_zeljus_com_PandroidModule_CreateObject(env: PJNIEnv; this: jobject; AClassName: jString): jlong; cdecl;
procedure Java_zeljus_com_PandroidModule_SetPropValue(env: PJNIEnv; this: jobject; AID: jlong; AProperty: jString; AValue: jString); cdecl;
function Java_zeljus_com_PandroidModule_GetPropValue(env: PJNIEnv; this: jobject; AID: jlong; AProperty: jString): jString; cdecl;
procedure Java_zeljus_com_PandroidModule_SetObjectProp(env: PJNIEnv; this: jobject; AID: jlong; AProperty: jString; AIDObject: jlong); cdecl;

procedure Java_zeljus_com_PandroidModule_Free(env: PJNIEnv; this: jobject; AID: jlong); cdecl;


implementation

var
   PropInfo: PPropInfo;

function Java_zeljus_com_PandroidModule_CreateObject(env: PJNIEnv; this: jobject; AClassName: jString): jlong; cdecl;
var
 ClassName : String;
begin
  ClassName := JNI_JStringToString(env, AClassName);
  try
    if CompareText(ClassName, 'TDataM') = 0 then
       Result := Module.AddObject(ClassName, TDataM.Create(nil))
    else
       Result := -1;

  finally

  end;
end;

procedure Java_zeljus_com_PandroidModule_SetPropValue(env: PJNIEnv; this: jobject; AID: jlong; AProperty: jString; AValue: jString); cdecl;
begin
  PropInfo := GetPropInfo(Module.Objects[AID].ClassInfo, JNI_JStringToString(env, AProperty));
  if Assigned(PropInfo) then
    SetPropValue(Module.Objects[AID], PropInfo, JNI_JStringToString(env, AValue));
end;

function Java_zeljus_com_PandroidModule_GetPropValue(env: PJNIEnv; this: jobject; AID: jlong; AProperty: jString): jString; cdecl;
begin
  try
    Result := JNI_StringToJString(env, GetPropValue(Module.Objects[AID], JNI_JStringToString(env, AProperty)) );
  except on E: Exception do
    Result := JNI_StringToJString(env, E.Message);
  end;
end;

procedure Java_zeljus_com_PandroidModule_SetObjectProp(env: PJNIEnv; this: jobject; AID: jlong; AProperty: jString; AIDObject: jlong); cdecl;
begin
  PropInfo := GetPropInfo(Module.Objects[AID].ClassInfo, JNI_JStringToString(env, AProperty));
  if Assigned(PropInfo) then
    SetObjectProp(Module.Objects[AID], PropInfo, Module.Objects[AIDObject]);
end;

procedure Java_zeljus_com_PandroidModule_Free(env: PJNIEnv; this: jobject; AID: jlong); cdecl;
begin
  Module.Objects[AID].Free;
end;

initialization
   Module := TStringList.Create;
finalization
   Module.Free;

end.

