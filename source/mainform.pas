{*************************************************************
*                 Zeljko Cvijanovic
*                  2015 Teslic
*                  www.zeljus.com
*************************************************************}
unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    buttonGenerateHeaders1: TButton;
    buttonGenerateHeaders2: TButton;
    buttonNewProject: TButton;
    eTarget: TComboBox;
    eAppName: TEdit;
    eAndroidSDKDir: TDirectoryEdit;
    eJavaPackageName: TEdit;
    eProjectDir: TDirectoryEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure buttonGenerateHeaders1Click(Sender: TObject);
    procedure buttonGenerateHeaders2Click(Sender: TObject);
    procedure buttonNewProjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure LoadIniFile;
    procedure SaveIniFile;
    procedure EditToAProject;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses FNewProject, Inifiles, process, MainUnit;

{ TForm1 }

procedure TForm1.buttonGenerateHeaders2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.buttonNewProjectClick(Sender: TObject);
begin
  EditToAProject;
  CreateNewAndroidProject;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   LoadIniFile;
end;

procedure TForm1.buttonGenerateHeaders1Click(Sender: TObject);
begin
    SaveIniFile;
end;

procedure TForm1.LoadIniFile;
var
   IniFile : TIniFile;
begin
   IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
   try
     eJavaPackageName.Text   := IniFile.ReadString('pandroid', 'JavaPackageName', 'zeljus.com.button');
     eAppName.Text           := IniFile.ReadString('pandroid', 'AppName', 'Button');
     eProjectDir.Text        := IniFile.ReadString('pandroid', 'ProjectDir', '/usr/local/pandroid/example');

     eAndroidSDKDir.Text     := IniFile.ReadString('pandroid', 'AndroidSDKDir', '/usr/local/pandroid/sdk');
     eTarget.Text            := IniFile.ReadString('pandroid', 'Target', 'android-15');


     AProject.gActivityName  := 'MainActivity';

   finally
     IniFile.Free;
   end;
end;

procedure TForm1.SaveIniFile;
var
   IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
     IniFile.WriteString('pandroid', 'JavaPackageName', eJavaPackageName.Text);
     IniFile.WriteString('pandroid', 'AppName',         eAppName.Text);
     IniFile.WriteString('pandroid', 'ProjectDir',      eProjectDir.Text);

     IniFile.WriteString('pandroid', 'AndroidSDKDir',   eAndroidSDKDir.Text);
     IniFile.WriteString('pandroid', 'Target',          eTarget.Text);
  finally
    IniFile.Free;
  end;
end;

procedure TForm1.EditToAProject;
begin
  with AProject do begin
    gJavaPackageName    := eJavaPackageName.Text;
    gAppName            := eAppName.Text;
    gProjectDir         := eProjectDir.Text;
    gAndroidSDKDir      := eAndroidSDKDir.Text;
    gTarget             := eTarget.Text;
  end;
end;

end.

