unit MainUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls,
  uPSComponent, uPSRuntime, uPSCompiler;

{ TForm1 }

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    Memo3: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
     PSScript: TPSScript;
     procedure PSScriptCompile(Sender: TPSScript);
     procedure PSScriptExecute(Sender: TPSScript);
  public
    { public declarations }
    procedure Execute(Script: WideString);
  end;

var
  Form1: TForm1;
  Rec: String;
  Message: String;

implementation

{$R *.lfm}

uses
   Variants,
   uPSComponent_Default,
   uPSComponent_DB,
   uPSC_strutils;

{ TForm1 }


procedure MyWriteln(const s: String);
begin
  Form1.Memo3.Lines.Add(s);
end;


procedure TForm1.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Execute(SynEdit1.Lines.Text);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: integer;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('s  = ');
  for i:=0 to SynEdit1.lines.Count - 2 do begin
    Memo1.Lines.Add('   '''+SynEdit1.Lines.Strings[i]+' ''+#10+' );
  end;
  Memo1.Lines.Add('   '''+SynEdit1.Lines.Strings[i+1]+' ''' );
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PSScript.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PSScript:= TPSScript.Create(nil);
  PSScript.OnCompile :=  PSScriptCompile;
  PSScript.OnExecute :=  PSScriptExecute;

  TPSPluginItem(PSScript.Plugins.Add).Plugin := TPSImport_Classes.Create(nil);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := TPSImport_DB.Create(nil);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := TPSImport_DateUtils.Create(nil);
  TPSPluginItem(PSScript.Plugins.Add).Plugin := TPSImport_StrUtils.Create(nil);

end;

procedure TForm1.PSScriptCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@MyWriteln, 'procedure Writeln(s: String);');
  Sender.AddRegisteredVariable('vars', 'Variant');
end;

procedure TForm1.PSScriptExecute(Sender: TPSScript);
begin
  PPSVariantVariant(PSScript.GetVariable('VARS'))^.Data := VarArrayCreate([0, 1], varShortInt);
end;

procedure TForm1.Execute(Script: WideString);
  procedure OutputMessages;
  var
    l: Longint;
    b: Boolean;
  begin
    b := False;

    for l := 0 to PSScript.CompilerMessageCount - 1 do
    begin
      Memo3.Lines.Add('Compiler: '+ PSScript.CompilerErrorToStr(l));
      if (not b) and (PSScript.CompilerMessages[l] is TIFPSPascalCompilerError) then
      begin
        b := True;
        SynEdit1.SelStart := PSScript.CompilerMessages[l].Pos;
      end;
    end;
  end;


begin

  Memo3.Lines.Clear;

  PSScript.Script.Assign(SynEdit1.Lines);
  Memo3.Lines.Add('Compiling');
  if PSScript.Compile then
  begin
    OutputMessages;
    Memo3.Lines.Add('Compiled successfully');
    if not PSScript.Execute then
    begin
      SynEdit1.SelStart := PSScript.ExecErrorPosition;
      Memo3.Lines.Add(PSScript.ExecErrorToString +' at '+Inttostr(PSScript.ExecErrorProcNo)+'.'+Inttostr(PSScript.ExecErrorByteCodePosition));
    end else Memo3.Lines.Add('Successfully executed');
  end else
  begin
    OutputMessages;
    Memo3.Lines.Add('Compiling failed');
  end;

end;

end.

