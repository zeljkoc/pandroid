{*******************************************************}
{                                                       }
{    FastCube 2 FastScript Formula Visual Editor unit   }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxfsFormulaEditor;

interface
{$INCLUDE fcx.inc}
uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, ToolWin,
  fcxComponent, fcxSlice, fs_tree, fs_synmemo, fs_iinterpreter,
  fs_ipascal, fs_icpp, fs_ibasic, fs_ijs,
  fs_itools, fcxInterpreter, fcxDataTree;

type
  TfcxfsFormulaEditor = class(TForm)
    Panel1: TPanel;
    Splitter: TSplitter;
    MainToolbar: TToolBar;
    btnCheckScript: TToolButton;
    btnOk: TToolButton;
    Panel2: TPanel;
    btnCancel: TToolButton;
    ScriptPages: TPageControl;
    LangCB: TComboBox;
    procedure btnCheckScriptSlick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ScriptPagesChange(Sender: TObject);
    procedure LangCBClick(Sender: TObject);
  private
    function GetSyntaxMemo: TfsSyntaxMemo;
    function GetErrorMsg: String;
    function GetScript: TfsScript;
  private
    FScripts: TList;
    FSources: TList;
    FComponent: TfcxComponent;
    FDataTree: TfcxDataTreeForm;
    FVariables: TStringList;

    // script objects
    FSFields: TPersistent;
    FMeasures: TPersistent;
    FDimensions: TPersistent;
    FCustomObject: TPersistent;

    procedure UpdateCaptions;
    procedure UpdateDataTree;
    procedure FillSliceItems;
    procedure OnDataTreeDblClick(Sender: TObject);
    function CheckScript: Boolean;
    procedure PrepareScript(AScript: TfsScript);
    procedure ClearScripts;
    function AddScript(Caption: String; SyntaxType: String; Script: TStrings; AParent: TfsScript): TfsScript;
    property fsSyntaxMemo: TfsSyntaxMemo read GetSyntaxMemo;
    property fsScript: TfsScript read GetScript;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Edit(const PositionTo: Integer = -1): Boolean;
    property ScriptComponent: TfcxComponent read FComponent write FComponent;
  end;

implementation
uses
  fcxRes,
  fcxGraphicRes,
  fcxTypes,
  fcxCodeUtils
{$IFDEF DELPHI_6UP}
  ,Variants
{$ENDIF};

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

function fsGetSyntaxType(SyntaxType: String): TSyntaxType;
begin
  // stPascal, stCpp, stJs, stVB, stSQL, stText
  if CompareText(SyntaxType, 'PascalScript') = 0 then
    Result := stPascal
  else
  if CompareText(SyntaxType, 'C++Script') = 0 then
    Result := stCpp
  else
  if CompareText(SyntaxType, 'JScript') = 0 then
    Result := stJs
  else
  if CompareText(SyntaxType, 'BasicScript') = 0 then
    Result := stVB
  else
    Result := stText;
end;

{ TFormulaEditor }

procedure TfcxfsFormulaEditor.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

function TfcxfsFormulaEditor.CheckScript: Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to FScripts.Count - 1 do
  begin
    TfsScript(FScripts[i]).Clear;
    PrepareScript(TfsScript(FScripts[i]));
    TfsScript(FScripts[i]).Lines.Assign(TfsSyntaxMemo(ScriptPages.Pages[i].Controls[0]).Lines);
    Result := TfsScript(FScripts[i]).Compile;
    if not Result then
      Exit;
  end;
end;

constructor TfcxfsFormulaEditor.Create(AOwner: TComponent);
begin
  inherited;

  fsGetLanguageList(LangCB.Items);
  FDimensions := CreateDimensions(nil);
  FMeasures := CreateMeasures(nil);
  FSFields := TfcxSliceFields.Create(nil);
  FCustomObject := CreateCustomObject(nil);

  FVariables := TStringList.Create;
  FVariables.Add('Dimensions');
  FVariables.Add('Measures');
  FVariables.Add('SliceFields');
  FVariables.Add('CustomObject');

  FScripts := TList.Create;
  FSources := TList.Create;
  FDataTree := TfcxDataTreeForm.Create(Self);
  FDataTree.OnDblClick := OnDataTreeDblClick;
  FDataTree.Parent := Panel2;
  FDataTree.Align := alClient;
  FDataTree.Visible := True;
  MainToolBar.Images := fcxGraphicResources.ToolImages;
  UpdateCaptions;
end;

destructor TfcxfsFormulaEditor.Destroy;
begin
  FVariables.Free;
  FScripts.Free;
  FSources.Free;
  FDimensions.Free;
  FMeasures.Free;
  FSFields.Free;
  FCustomObject.Free;
  inherited;
end;

function TfcxfsFormulaEditor.Edit(const PositionTo: Integer): Boolean;
var
  GlobalScript: TfsScript;
begin
  ClearScripts;
  GlobalScript := AddScript('common', ScriptComponent.CommonScriptLanguage, ScriptComponent.CommonScript, nil {fsGlobalUnit});
  AddScript(ScriptComponent.Name, ScriptComponent.ScriptLanguage, ScriptComponent.Script, GlobalScript);
  FDataTree.Script := GlobalScript {fsGlobalUnit}; //??
  FDataTree.SysVariables.Assign(FVariables);
  if ScriptComponent is TfcxSlice then
    FillSliceItems;
  UpdateDataTree;
  if PositionTo <> -1 then
    fsSyntaxMemo.SetPos(1, PositionTo);
  LangCB.ItemIndex := LangCB.Items.IndexOf(fsScript.SyntaxType);
  Result := ShowModal = mrOk;
end;

procedure TfcxfsFormulaEditor.OnDataTreeDblClick(Sender: TObject);
begin
  fsSyntaxMemo.SelText := FDataTree.GetFieldName;
  fsSyntaxMemo.SetFocus;
end;

procedure TfcxfsFormulaEditor.PrepareScript(AScript: TfsScript);
begin
  AScript.AddVariable('Dimensions', 'TDimensions', PtrUInt(FDimensions));
  AScript.AddVariable('Measures', 'TMeasures', PtrUInt(FMeasures));
  AScript.AddVariable('SliceFields', 'TfcxSliceFields', PtrUInt(FSFields));
  AScript.AddVariable('CustomObject', 'TfcxCustomObject', PtrUInt(FCustomObject));
//  if Assigned(Slice.OnInterpreterCreated) then
//    Slice.OnInterpreterCreated(Slice, fsScript, fcti_MeasureCalculation);
end;

procedure TfcxfsFormulaEditor.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfcxfsFormulaEditor.btnCheckScriptSlick(Sender: TObject);
begin
  if CheckScript then
    MessageDlg(fcxResources.Get('sNoErrors'), mtInformation, [mbOk], 0)
  else
    MessageDlg(GetErrorMsg, mtError, [mbOk], 0);
end;

procedure TfcxfsFormulaEditor.UpdateCaptions;
begin
  Caption := fcxResources.Get('sFormulaEditor');
  BtnOk.Hint := fcxResources.Get('sOkBtn');
  BtnCancel.Caption := fcxResources.Get('sCancelBtn');
  BtnCancel.Hint := fcxResources.Get('sCancelBtn');
  BtnCheckScript.Hint := fcxResources.Get('sCheckBtn');
end;

procedure TfcxfsFormulaEditor.UpdateDataTree;
begin
  FDataTree.UpdateItems;
end;

function TfcxfsFormulaEditor.AddScript(Caption: String; SyntaxType: String; Script: TStrings; AParent: TfsScript): TfsScript;
var
  Memo: TfsSyntaxMemo;
  Page: TTabSheet;
begin
  Result := TfsScript.Create(Self);
  Result.SyntaxType := SyntaxType;
  Result.Parent := AParent;
  if AParent = nil then
    fcxAddRTTI(Result);
  Result.Lines.Assign(Script);
  FSources.Add(Script);
  FScripts.Add(Result);
  Page := TTabSheet.Create(Self);
  Page.Caption := Caption;
  Page.PageControl := ScriptPages;
  Memo := TfsSyntaxMemo.Create(Self);
  Memo.Lines.Text := Script.Text;
  Memo.Parent := Page;
  Memo.Align := alClient;
  Memo.SyntaxType := fsGetSyntaxType(SyntaxType);
  ScriptPages.ActivePageIndex := Page.PageIndex;
  ActiveControl := Memo;
end;

function TfcxfsFormulaEditor.GetSyntaxMemo: TfsSyntaxMemo;
begin
  Result := TfsSyntaxMemo(ScriptPages.ActivePage.Controls[0])
end;

procedure TfcxfsFormulaEditor.ClearScripts;
var
  i: integer;
begin
  for i := 0 to FScripts.Count - 1 do
    TObject(FScripts[i]).Free;
  FScripts.Clear;
  for i := 0 to FSources.Count - 1 do
    TObject(FSources[i]).Free;
  FSources.Clear;
  for i := ScriptPages.PageCount - 1 downto 0 do
    ScriptPages.Pages[i].Free;
end;

function TfcxfsFormulaEditor.GetErrorMsg: String;
var
  i: integer;
begin
  for i := 0 to FScripts.Count - 1 do
    if TfsScript(FScripts[i]).ErrorMsg <> '' then
    begin
      Result := TfsScript(FScripts[i]).ErrorMsg;
      Exit;
    end;
  Result := '';
end;

procedure TfcxfsFormulaEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: integer;
  Script: TfsScript;
begin
  CanClose := (ModalResult = mrCancel) or ((ModalResult = mrOk) and (CheckScript or
    (MessageDlg(Format(fcxResources.Get('sScriptErrorsFound'), [GetErrorMsg]), mtConfirmation, [mbYes, mbNo], 0) = mrYes)));
  if CanClose and (ModalResult = mrOk) then
  begin
    for i := 0 to FSources.Count - 1 do
    begin
      Script := TfsScript(FScripts[i]);
      TStrings(FSources[i]).Assign(Script.Lines);
      case i of
        0: ScriptComponent.CommonScriptLanguage := Script.SyntaxType;
        1: ScriptComponent.ScriptLanguage := Script.SyntaxType;
      end;
    end;
  end;
end;

procedure TfcxfsFormulaEditor.ScriptPagesChange(Sender: TObject);
begin
  LangCB.ItemIndex := LangCB.Items.IndexOf(fsScript.SyntaxType);
end;

procedure TfcxfsFormulaEditor.LangCBClick(Sender: TObject);
begin
  if MessageDlg(fcxResources.Get('dsClearScript'), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
  begin
    LangCB.ItemIndex := LangCB.Items.IndexOf(fsScript.SyntaxType);
    Exit;
  end;

  fsScript.SyntaxType := LangCB.Text;
  fcxEmptyCode(fsSyntaxMemo.Lines, LangCB.Text);
  fsSyntaxMemo.SetFocus;
  fsSyntaxMemo.Invalidate;
end;

function TfcxfsFormulaEditor.GetScript: TfsScript;
begin
  Result := TfsScript(FScripts[ScriptPages.ActivePageIndex]);
end;

procedure TfcxfsFormulaEditor.FillSliceItems;
var
  AItems: TStringList;
  i: integer;
begin
  AItems := TStringList.Create;
  for i := 0 to TfcxSlice(ScriptComponent).MeasuresContainer.Count - 1 do
    AItems.Add(TfcxSlice(ScriptComponent).MeasuresContainer.Measures[i].Name + '=' + TfcxSlice(ScriptComponent).MeasuresContainer.Measures[i].Caption);
  FDataTree.Measures.Assign(AItems);
  AItems.Clear;
  with TfcxSlice(ScriptComponent).XAxisContainer do
    for i := 0 to LevelCount - 1 do
      if LevelInfoWOMeasures[i].LevelType = fcATLT_SubGroup then
        AItems.Add(LevelInfoWOMeasures[i].RegionField.Name + ' SubGroup=' + LevelInfoWOMeasures[i].RegionField.Caption)
      else
        AItems.Add(LevelInfoWOMeasures[i].RegionField.Name + '=' + LevelInfoWOMeasures[i].RegionField.Caption);
  with TfcxSlice(ScriptComponent).YAxisContainer do
    for i := 0 to LevelCount - 1 do
      if LevelInfoWOMeasures[i].LevelType = fcATLT_SubGroup then
        AItems.Add(LevelInfoWOMeasures[i].RegionField.Name + ' SubGroup=' + LevelInfoWOMeasures[i].RegionField.Caption)
      else
        AItems.Add(LevelInfoWOMeasures[i].RegionField.Name + '=' + LevelInfoWOMeasures[i].RegionField.Caption);
{
  for i := 0 to TfcxSlice(ScriptComponent).XAxisContainer.Fields.Count - 1 do
    AItems.Add(TfcxSlice(ScriptComponent).XAxisContainer.Fields[i].Name + '=' + TfcxSlice(ScriptComponent).XAxisContainer.Fields[i].Caption);
  for i := 0 to TfcxSlice(ScriptComponent).YAxisContainer.Fields.Count - 1 do
    AItems.Add(TfcxSlice(ScriptComponent).YAxisContainer.Fields[i].Name + '=' + TfcxSlice(ScriptComponent).YAxisContainer.Fields[i].Caption);
}
  FDataTree.Dimensions.Assign(AItems);
  AItems.Clear;
  for i := 0 to TfcxSlice(ScriptComponent).SliceFields.Count - 1 do
    AItems.Add(TfcxSlice(ScriptComponent).SliceFields[i].FieldName + '=' + TfcxSlice(ScriptComponent).SliceFields[i].Caption);
  FDataTree.SliceFields.Assign(AItems);
  AItems.Free;
end;

end.
