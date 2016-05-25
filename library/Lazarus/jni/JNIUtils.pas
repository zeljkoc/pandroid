unit JNIUtils;

{$mode objfpc}{$H+}

interface

uses jni;

//Opste
function JNI_JStringToString(env: PJNIEnv; JStr: JString): string;
function JNI_StringToJString(env: PJNIEnv; Str: string): jstring;

function JNI_JStringToWideString(Env: PJNIEnv; JStr: JString): WideString;
function JNI_WideStringToJString(Env: PJNIEnv; const WStr: WideString): JString;


implementation

//Opste
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

function JNI_StringToJString(env: PJNIEnv; Str: string): jstring;
begin
    Result := env^^.NewStringUTF(env, @Str[1]);
end;

function JNI_JStringToWideString(Env: PJNIEnv; JStr: JString): WideString;
var
  IsCopy: Pjboolean;
  Chars: PChar;
begin
  if JStr = nil then begin
    Result := '';
    Exit;
  end;

  Chars:= Env^^.GetStringUTFChars(Env, JStr, IsCopy);
  if Chars = nil then
     Result := ''
  else begin
      Result := WideString(Chars);
      Env^^.ReleaseStringUTFChars(Env, JStr, Chars);
    end;
end;

function JNI_WideStringToJString(Env: PJNIEnv; const WStr: WideString): JString;
begin
  Result := Env^^.NewString(Env, Pointer(WStr), Length(WStr));
end;

end.

