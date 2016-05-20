{*************************************************************
*                 Zeljko Cvijanovic
*                    2015 Teslic
*                  www.zeljus.com
*************************************************************}
program pandroid;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,  mainform, FNewProject
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  if (Paramcount = 5) and (ParamStr(1) = 'R') then begin
                     //tAppName,  tJavaPackageName, tRJava, tRJavaPAs
     BuildRJavaFiles(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5));
  end else  Application.Run ;
end.

