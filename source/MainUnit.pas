unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
   SysUtils, Forms;


type
   TAProject = record
       gProjectDir: String;
       gJavaPackageName: string;
       gJavaProjectName: string;
       gAndroidSDKDir: string;
       gTarget: string;
       gAppName: string;
       gActivityName: String;
    end;

var
  AProject : TAProject;

  function Replace(S, Old, New: String): String;
  function StringNameReplace(S: String): String;

implementation

function Replace(S, Old, New: String): String;
var i: integer;
begin
  Result := '';
  for i:=1 to Length(S) do begin
     if S[i]=Old then Result := Result + New
     else Result := Result + S[i];
  end;
end;

function StringNameReplace(S: String): String;
begin
 S :=  StringReplace(S, '#ProjectDir#',      AProject.gProjectDir,      [rfReplaceAll]);
 S :=  StringReplace(S, '#JavaPackageName#', AProject.gJavaPackageName, [rfReplaceAll]);
 S :=  StringReplace(S, '#AndroidSDKDir#',   AProject.gAndroidSDKDir,   [rfReplaceAll]);
 S :=  StringReplace(S, '#Target#',          AProject.gTarget,          [rfReplaceAll]);
 S :=  StringReplace(S, '#AppName#',         AProject.gAppName,         [rfReplaceAll]);
 S :=  StringReplace(S, '#ActivityName#',    AProject.gActivityName,    [rfReplaceAll]);
 S :=  StringReplace(S, '#PANDROID#',        ExtractFileDir(Application.ExeName),    [rfReplaceAll]);
 S :=  StringReplace(S, '#DatumVreme#',      DateTimeToStr(now),        [rfReplaceAll]);
 Result := S;
end;

end.

