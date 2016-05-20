{*************************************************************
*                 Zeljko Cvijanovic
*                  2015 Teslic
*                  www.zeljus.com
*************************************************************}
unit FNewProject;

{$mode objfpc}{$H+}


interface
uses
  Classes, SysUtils, FileUtil, Forms, dialogs;

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

procedure CreateNewAndroidProject;
procedure BuildRJavaFiles(tAppName, tJavaPackageName, tRJava, tRJavaPAs: string);

function Replace(S, Old, New: String): String;

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
 S :=  StringReplace(S, '#JavaProjectName#', AProject.gJavaProjectName, [rfReplaceAll]);
 S :=  StringReplace(S, '#AndroidSDKDir#',   AProject.gAndroidSDKDir,   [rfReplaceAll]);
 S :=  StringReplace(S, '#Target#',          AProject.gTarget,          [rfReplaceAll]);
 S :=  StringReplace(S, '#AppName#',         AProject.gAppName,         [rfReplaceAll]);
 S :=  StringReplace(S, '#ActivityName#',    AProject.gActivityName,    [rfReplaceAll]);
 S :=  StringReplace(S, '#PANDROID#',        ExtractFileDir(Application.ExeName),    [rfReplaceAll]);
 S :=  StringReplace(S, '#DatumVreme#',      DateTimeToStr(now),        [rfReplaceAll]);
 Result := S;
end;

procedure AddJavaBuildFiles(); forward;
procedure AddPasBuildFiles(); forward;
procedure AddResFiles(); forward;
procedure AddBatchFile(); forward;


procedure CreateNewAndroidProject;
var
  Str: string;
begin
 try
  if DirectoryExists(AProject.gProjectDir + PathDelim+ 'android') then begin
    ShowMessage('Android project exist!');
    Exit;
  end;

   // Creates the directory structure
   ForceDirectories(AProject.gProjectDir + PathDelim+ 'android');
 //  ForceDirectories(AProject.gProjectDir + PathDelim+ 'android'+ PathDelim + 'jni');
 //  ForceDirectories(AProject.gProjectDir + PathDelim+ 'android'+ PathDelim + 'lib');
   ForceDirectories(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'libs' ); //+ PathDelim + 'armeabi');
   ForceDirectories(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'values');
 //  CreateDir(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable-ldpi');

   Str := StringReplace(AProject.gJavaPackageName, '.', PathDelim, [rfReplaceAll]);
   ForceDirectories(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'src' + PathDelim + Str);

   // Now add the Java build files
   AddJavaBuildFiles();

   AddPasBuildFiles;

   //BuildRJavaFiles;

   AddResFiles;

   AddBatchFile;

   ShowMessage('Create project!');

 Except
   ShowMessage('Error!');
 end;
end;

procedure AddJavaBuildFiles;
var
  lFile: TStringList;
  i: integer;
begin
  lFile := TStringList.Create;
  try

    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+'android'+PathDelim+ 'AndroidManifest.xml');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'AndroidManifest.xml');


    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+'android'+PathDelim+ 'build.xml');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'build.xml');

    lFile.Clear;
    lFile.Add('# Project target.');
    lFile.Add('target='+AProject.gTarget);
    lFile.Add('# SDK directory');
    {$ifdef linux}
     lFile.Add('sdk.dir='+ AProject.gAndroidSDKDir);
    {$else}
     lFile.Add('sdk.dir=' + Replace(AProject.gAndroidSDKDir, '\', '\\'));
    {$endif}
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'project.properties');

   lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+'android'+PathDelim+ 'ant.properties');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'ant.properties');

  finally
    lFile.Free;
  end;

end;

procedure AddPasBuildFiles;
var
  lFile: TStringList;
  i: integer;
begin
  lFile := TStringList.Create;
  try
    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+ 'MainActivity.lpr');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ AProject.gAppName + '.lpr');


    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+ 'MainActivity.lpi');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ AProject.gAppName + '.lpi');

    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+ 'MainActivity.lps');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ AProject.gAppName + '.lps');

    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+ 'Rjava.pas');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'Rjava' + '.pas');
  finally
    lFile.Free;
  end;
end;

procedure AddResFiles;
var
  lFile: TStringList;
  i: integer;
begin
  lFile := TStringList.Create;
  try
    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'values' + PathDelim + 'strings.xml');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'values' + PathDelim + 'strings.xml');

    lFile.Clear;
    lFile.LoadFromFile(ExtractFileDir(Application.ExeName)+PathDelim+'template'+PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'values' + PathDelim + 'styles.xml');
    for i:=0 to lFile.Count - 1 do begin
      lFile.Strings[i] := StringNameReplace(lFile.Strings[i]);
    end;
    lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'values' + PathDelim + 'styles.xml');
  finally
    lFile.Free;
  end;

  // Now the default icon
{  FileUtil.CopyFile(ExtractFileDir(Application.ExeName) + PathDelim+ 'template'+PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable-ldpi' + PathDelim + 'ic_launcher.png',
  AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable-ldpi' + PathDelim + 'ic_launcher.png');
 }
  FileUtil.CopyDirTree(ExtractFileDir(Application.ExeName) + PathDelim+ 'template'+PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable',
  AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable' , [cffCreateDestDirectory]);

  FileUtil.CopyDirTree(ExtractFileDir(Application.ExeName) + PathDelim+ 'template'+PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable-ldpi',
  AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'res' + PathDelim + 'drawable-ldpi' , [cffCreateDestDirectory]);

  FileUtil.CopyDirTree(ExtractFileDir(Application.ExeName) + PathDelim+ 'template'+PathDelim+ 'android' + PathDelim + 'libs',
  AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'libs' , [cffCreateDestDirectory]);

  FileUtil.CopyDirTree(ExtractFileDir(Application.ExeName) + PathDelim+ 'template'+PathDelim+ 'android' + PathDelim + 'assets',
  AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'assets' , [cffCreateDestDirectory]);
end;


procedure AddBatchFile;
var
  lFile: TStringList;
  Str: String;
begin
  lFile := TStringList.Create;
  try
    {$ifdef linux}
   {  lFile.Add('keytool -genkey -v -keystore android.keystore -alias myalias -storepass 111111 -keypass 111111 -keyalg RSA -validity 10000');
     lFile.SaveToFile(AProject.gProjectDir + PathDelim + 'android' + PathDelim + 'keygen.sh');  }

     lFile.Clear;
     lFile.Add('clear');
     lFile.Add('echo BUILD --------------------------------------------------------------');
     lFile.Add('PANDROID='+ExtractFileDir(Application.ExeName));
     lFile.Add('PROJECT='+AProject.gProjectDir);
     lFile.Add('SDK='+AProject.gAndroidSDKDir);
     lFile.Add('TYPHON=/usr/local/codetyphon');
     lFile.Add('');
    // lFile.Add('export JAVA_HOME='+AProject.gJava_Home);
     lFile.Add('');
     lFile.Add('cd $PROJECT' + PathDelim + 'android');
     lFile.Add('');
     lFile.Add('rm -rf bin');
     lFile.Add('rm -rf gen');
     lFile.Add('rm -rf Rjava.pas');
     lFile.Add('');
    // lFile.Add('sleep 5');
     lFile.Add('echo Deleted -----------------------------------');
     lFile.Add('mkdir -p bin');
     lFile.Add('mkdir -p gen');
     lFile.Add('mkdir -p bin/classes');
     lFile.Add('');

     lFile.Add('echo compile R.java -------------------------------------');
    { lFile.Add('$SDK' + PathDelim +'build-tools'+ PathDelim+ '19.1.0' +PathDelim +'aapt p -f -M AndroidManifest.xml -F bin'+PathDelim +AProject.gAppName+'.ap_ -I '+
               '$SDK' + PathDelim +'platforms'+ PathDelim +AProject.gTarget +PathDelim+'android.jar -S res -m -J gen');}

     lFile.Add('$SDK' + PathDelim +'build-tools'+ PathDelim+ '23.0.3' +PathDelim +'aapt package -m -J gen -M AndroidManifest.xml -S res -I '+
               '$SDK' + PathDelim +'platforms'+ PathDelim +AProject.gTarget +PathDelim+'android.jar -S res -m -J gen');

     lFile.Add('');
     lFile.Add('echo compile Rjava.pas ----------------------------------');
     Str := StringReplace(AProject.gJavaPackageName, '.', PathDelim, [rfReplaceAll]);
     ForceDirectories(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'gen' + PathDelim + Str);
     lFile.Add('$PANDROID'+ PathDelim+ ExtractFileName(Application.ExeName) +' R '+AProject.gAppName+ ' '+ AProject.gJavaPackageName+
                                         ' $PROJECT'+ PathDelim +'android' + PathDelim + 'gen' + PathDelim + Str+PathDelim+'R.java $PROJECT'+ PathDelim +'Rjava.pas');

     lFile.Add('');
     lFile.Add('echo ppcjvm ---------------------------------------------');

     {$ifdef linux}

       {$IFDEF CPU64}
           lFile.Add('$TYPHON/fpc/fpc64/bin/x86_64-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -FUbin/classes -djvm -dRELEASE -Us -Sg $TYPHON/fpcsrc/rtl/java/system.pp');
           lFile.Add('$TYPHON/fpc/fpc64/bin/x86_64-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -FUbin/classes -djvm -dRELEASE $TYPHON/fpcsrc/rtl/inc/uuchar.pp');
           lFile.Add('$TYPHON/fpc/fpc64/bin/x86_64-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -FUbin/classes -djvm -dRELEASE $TYPHON/fpcsrc/rtl/java/objpas.pp');
           lFile.Add('$TYPHON/fpc/fpc64/bin/x86_64-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -Fu$PANDROID/units -FUbin/classes -djvm -dRELEASE $PROJECT'+PathDelim+''+ AProject.gAppName + '.lpr');
       {$ELSE IFDEF CPU32}
           lFile.Add('$TYPHON/fpc/fpc32/bin/i386-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -FUbin/classes -djvm -dRELEASE -Us -Sg $TYPHON/fpcsrc/rtl/java/system.pp');
           lFile.Add('$TYPHON/fpc/fpc32/bin/i386-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -FUbin/classes -djvm -dRELEASE $TYPHON/fpcsrc/rtl/inc/uuchar.pp');
           lFile.Add('$TYPHON/fpc/fpc32/bin/i386-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -FUbin/classes -djvm -dRELEASE $TYPHON/fpcsrc/rtl/java/objpas.pp');
           lFile.Add('$TYPHON/fpc/fpc32/bin/i386-linux/ppcjvm @$TYPHON/fpcsrc/rtl/android/jvm/rtl.cfg -Ur -Tandroid -Pjvm -Ur -Xs -O2 -n -Fi$TYPHON/fpcsrc/rtl/inc'+
                     ' -Fi$TYPHON/fpcsrc/rtl/jvm -Fi$TYPHON/fpcsrc/rtl/java -FE. -Fi$TYPHON/fpcsrc/rtl/android/jvm -Fu$PANDROID/units -FUbin/classes -djvm -dRELEASE $PROJECT'+PathDelim+''+ AProject.gAppName + '.lpr');
        {$ENDIF}

     {$Elses}

     {$ENDIF}
     lFile.Add('');
     lFile.Add('#$PANDROID'+PathDelim+'compiler'+PathDelim+'ppcjvm -n -Tandroid '+
        '-Fu$PANDROID'+PathDelim+'units'+PathDelim+'typhon '+'-Fu$PANDROID'+PathDelim+'units -FEbin'+PathDelim+'classes $PROJECT'+PathDelim+''+ AProject.gAppName + '.lpr');

   {  {$IFDEF CPU64}
      lFile.Add('$PANDROID'+PathDelim+'compiler'+PathDelim+'ppcjvm -n -Tandroid '+
        '-Fu$PANDROID'+PathDelim+'units'+PathDelim+'typhon '+'-Fu$PANDROID'+PathDelim+'units -FEbin'+PathDelim+'classes $PROJECT'+PathDelim+''+ AProject.gAppName + '.lpr');

     {$ELSE IFDEF CPU32}


     {$ENDIF} }

     lFile.Add('');
     lFile.Add('echo ANT DEBUG ==========================================');
     lFile.Add('ant -verbose release');
     lFile.Add('');

     lFile.Add('jarsigner -verify -verbose -certs $PROJECT'+PathDelim+ AProject.gAppName+'.apk');

     lFile.Add('');
    // lFile.Add('sleep 1');
     lFile.Add('');
     lFile.Add('adb unistall '+AProject.gJavaPackageName);
     lFile.Add('adb install -r $PROJECT'+ PathDelim +AProject.gAppName+'.apk');
     lFile.Add('');
     lFile.Add('adb shell am start -n '+AProject.gJavaPackageName +PathDelim+ AProject.gJavaPackageName+'.MainActivity');
     lFile.Add('');
  //   lFile.Add('sleep 100');
     lFile.Add('echo delete bin and gen =====================');
     lFile.Add('rm -rf bin');
     lFile.Add('rm -rf gen');

     lFile.SaveToFile(AProject.gProjectDir+ PathDelim+'android' + PathDelim + 'build_debug_apk.sh');
    {$else}
     lFile.Add('SET PATH='+gPATHJava+';'+AProject.gApacheAnt);
     lFile.Add('SET APK_SDK_PLATFORM='+AProject.gSDKPlatform);
     lFile.Add('');
     lFile.Add('rmdir bin /s /q');
     lFile.Add('rmdir gen /s /q');
     lFile.Add('');
     lFile.Add('ant debug');
     lFile.SaveToFile(AProject.gProjectDir + PathDelim+ 'android' + PathDelim + 'build_debug_apk.bat');
    {$endif}

    lFile.Clear;
    {$ifdef linux}


    {$else}
    lFile.Add('SET PATH='+gAndroidSDKDir+'tools;'+gAndroidSDKDir+ 'platform-tools'+ PathDelim +';'+gPATHJava);
    lFile.Add('');
    //lFile.Add('keytool -genkeypair -dname "cn=Zeljko Cvijanovic, ou=ZELJUSszd, o=Sun, c=RS" -alias business -keypass 111111 -keystore LCLDebugKey.keystore -storepass 111111 -validity 180');
    //lFile.Add('move LCLDebugKey.keystore bin\LCLDebugKey.keystore');
    //lFile.Add('');
    //lFile.Add('');
    lFile.Add('adb uninstall '+gJavaPackageName);
    lFile.Add('adb -s emulator-5554 install bin'+ PathDelim +gJavaProjectName+'-debug.apk');
    lFile.SaveToFile(gProjectDir + PathDelim+ 'android' + PathDelim + 'send_emulator.bat');
    {$endif}

  finally
    lFile.Free;
  end;

end;

procedure BuildRJavaFiles(tAppName, tJavaPackageName, tRJava, tRJavaPAs: string);

var
  lFile: TStringList;
  InFile: TStringList;
  i, j: integer;

  InClass : Boolean;
  InConst : Boolean;
  InStr: String;
begin
  lFile := TStringList.Create;
  InFile := TStringList.Create;
  try
    lFile.Add('// AUTO-GENERATED FILE. DO NOT MODIFY.');
    lFile.Add('');
    lFile.Add('// This class was automatically generated by the ' + tAppName + ' tool');
    lFile.Add('// from R.java. It should not be modified by hand.');
    lFile.Add('');
    lFile.Add('unit Rjava;');
    lFile.Add('');
    lFile.Add('{$mode objfpc}{$H+}');
    lFile.Add('{$modeswitch unicodestrings}');
    lFile.Add('{$namespace ' + tJavaPackageName + '}');
    lFile.Add('');
    lFile.Add('interface');
    lFile.Add('');
    lFile.Add('type');
    lFile.Add('  R = class');
    lFile.Add('  public');
    lFile.Add('    type');

    InFile.LoadFromFile(tRjava); //lista files *.pas

    j:= 0;
    repeat
      InStr := Trim(InFile.Strings[j]);
      j := j + 1;
    until Copy(InStr, 1, 21) = 'public final class R ';
   InClass := False;
   InConst := False;

    for i:=j to InFile.Count - 1 do begin
      InStr := Trim(InFile.Strings[i]);

      if Copy(InStr, 1, 26) = 'public static final class ' then begin
        InStr := Copy(InStr, 27, MaxInt);
        if Pos('{', InStr) > 0 then
          InStr := Copy(InStr, 1, Pos('{', InStr)-1);
        InStr := Trim(InStr);

        if InStr = 'string' then InStr := 'string_';
        if InStr = 'array' then  InStr := 'array_';

        lFile.Add('      '+InStr+' = class');
        InClass := True;
        end
      else if InClass and (Copy(InStr, 1, 24) = 'public static final int ') then begin
        if not InConst then begin
            lFile.Add('      public');
            lFile.Add('        const');
            InConst := True;
          end;
        InStr := Copy(InStr, 25, MaxInt);
        InStr := StringReplace(InStr, '=0x', ' = $', []);
        lFile.Add('          '+InStr);
        end
      else if InClass and (InStr = '}') then begin
         lFile.Add('      end;');
         lFile.Add(' ');
         InClass := False;
         InConst := False;
        end;
   end;

   lFile.Add('  end;');
   lFile.Add('');
   lFile.Add('implementation');
   lFile.Add('');
   lFile.Add('end.');

    DeleteFile(AProject.gProjectDir + PathDelim + 'Rjava.pas');
   lFile.SaveToFile(tRJavaPas);

  finally
    lFile.Free;
    InFile.Free;
  end;
end;

end.

