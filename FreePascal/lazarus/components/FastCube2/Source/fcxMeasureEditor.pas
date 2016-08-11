{*******************************************************}
{                                                       }
{            FastCube 2 measure editor unit             }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxMeasureEditor;

{$I fcx.inc}

interface

uses
{$IFDEF FPC}
  Types, LCLType,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  fcxTypes, fcxSlice, Spin, ExtCtrls, ToolWin, ActnList,
  fcxGridPainters, fcxFormatFrame, fcxStyleFrame, fcxHighlights, CheckLst;

type

  { TfcxMeasureEditorForm }

  TfcxMeasureEditorForm = class(TForm)
    Pages: TPageControl;
    PageGeneral: TTabSheet;
    PageTotals: TTabSheet;
    PageFiltering: TTabSheet;
    PageStyle: TTabSheet;
    OkBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    lblCaption: TLabel;
    edCaption: TEdit;
    lblAggregate: TLabel;
    cbAggregate: TComboBox;
    lblBaseField: TLabel;
    cbBaseField: TComboBox;
    cbScriptFunction: TComboBox;
    lblFunction: TLabel;
    lblOrder: TLabel;
    edOrder: TSpinEdit;
    lbStyles: TListBox;
    tbStyles: TToolBar;
    tbAdd: TToolButton;
    tbDelete: TToolButton;
    ToolButton3: TToolButton;
    tbUp: TToolButton;
    tbDown: TToolButton;
    PageFormat: TTabSheet;
    FormatFrame: TfcxFormatEditorFrame;
    ActionList1: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    hdStyles: THeaderControl;
    tbEdit: TToolButton;
    actEdit: TAction;
    lblFilterFunction: TLabel;
    cbFilterFunction: TComboBox;
    cbUseDifferentAggregateForTotals: TCheckBox;
    cbAggregateForTotals: TComboBox;
    lblAggregateTotal: TLabel;
    cbCalcTotalsOnTotals: TCheckBox;
    cbUseXAxisTotalsAsBase: TCheckBox;
    rbTotalsConflictResolving: TRadioGroup;
    lblDefaultTotalPosition: TLabel;
    cbDefaultTotalPosition: TComboBox;
    cbCalcAllCells: TCheckBox;
    lblFunctionTotal: TLabel;
    cbScriptFunctionTotal: TComboBox;
    lblOrderTotal: TLabel;
    edOrderTotal: TSpinEdit;
    cbCalcAllCellsTotal: TCheckBox;
    lblName: TLabel;
    edName: TEdit;
    cbDistinct: TCheckBox;
    lbDistinctField: TLabel;
    cbDistinctField: TComboBox;
    lblExtraField: TLabel;
    cbExtraField: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyBtnClick(Sender: TObject);
    procedure cbScriptFunctionDblClick(Sender: TObject);
    procedure cbScriptFunctionDropDown(Sender: TObject);
    procedure lbStylesClick(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actMoveUpUpdate(Sender: TObject);
    procedure actMoveDownUpdate(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure lbStylesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actEditUpdate(Sender: TObject);
    procedure lbStylesDblClick(Sender: TObject);
    procedure cbFilterFunctionDropDown(Sender: TObject);
    procedure cbFilterFunctionDblClick(Sender: TObject);
    procedure cbUseDifferentAggregateForTotalsClick(Sender: TObject);
    procedure cbCalcTotalsOnTotalsClick(Sender: TObject);
    procedure cbAggregateChange(Sender: TObject);
    procedure cbBaseFieldSelect(Sender: TObject);
    procedure cbDistinctClick(Sender: TObject);
  private
    FMeasure: TfcxMeasureField;
    FSlice: TfcxSlice;
    FHighlightRules: TfcxCustomHighlights;
    FSelectedHighlight: Integer;
    FBlockUpdate: Boolean;
    FBaseFieldIndex: Integer;
    procedure Localize;
    procedure SetSelectedHighlight(const Value: Integer);
    procedure UpdateStylesList;
  protected
    procedure DoStylesChange(Sender: TObject);
    procedure SetupGeneralPage;
    procedure SetupTotalsPage;
    procedure SetupFilteringPage;
    procedure SetupStylePage;
    procedure SetupFormatPage;

    procedure ApplyGeneralPage;
    procedure ApplyTotalsPage;
    procedure ApplyFilteringPage;
    procedure ApplyStylePage;
    procedure ApplyFormatPage;
    procedure Apply;

    property SelectedHighlight: Integer read FSelectedHighlight write SetSelectedHighlight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(ASlice: TfcxSlice; AMeasure: TfcxMeasureField; const PageIndex: Integer = 0): Boolean;
  end;

const
  fcxMeasureEditorGeneralPageIndex = 0;
  fcxMeasureEditorTotalsPageIndex = 1;
  fcxMeasureEditorFilteringPageIndex = 2;
  fcxMeasureEditorStylePageIndex = 3;
  fcxMeasureEditorFormatPageIndex = 4;

implementation

uses
  fcxRes,
  fcxGraphicRes,
  fcxCodeUtils,
  fcxHighlightRuleEditor,
  Math
{$ifdef INCLUDE_FAST_SCRIPT}
  , fcxfsFormulaEditor
{$ENDIF};

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{ TMeasureEditor }

procedure TfcxMeasureEditorForm.Apply;
begin
  FMeasure.BeginUpdate;
  ApplyGeneralPage;
  ApplyTotalsPage;
  ApplyFilteringPage;
  ApplyStylePage;
  ApplyFormatPage;
  FMeasure.EndUpdate;
end;

function TfcxMeasureEditorForm.Execute(ASlice: TfcxSlice; AMeasure: TfcxMeasureField; const PageIndex: Integer = 0): Boolean;
begin
  FSlice := ASlice;
  FMeasure := AMeasure;
  Localize;
  Pages.ActivePageIndex := PageIndex;
  SetupGeneralPage;
  SetupTotalsPage;
  SetupFilteringPage;
  SetupStylePage;
  SetupFormatPage;
  Result := ShowModal = mrOk;
  if Result then
    Apply;
end;

procedure TfcxMeasureEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfcxMeasureEditorForm.ApplyGeneralPage;
begin
  FMeasure.Name := ControlToString(edName.Text);
  FMeasure.Caption := ControlToString(edCaption.Text);
  FMeasure.AgrFunc := TfcxAgrFunc(cbAggregate.ItemIndex + 1);
  if FMeasure.AgrFunc in ScriptFuncs then
  begin
    FMeasure.SliceField := nil;
    FMeasure.ScriptFunction := cbScriptFunction.Text;
    FMeasure.ScriptOrder := edOrder.Value;
    if FMeasure.AgrFunc in CanCreateAllCellsFuncs then
      FMeasure.CalcAllCells := cbCalcAllCells.Checked
    else
      FMeasure.CalcAllCells := False;
  end
  else
  begin
    if cbBaseField.ItemIndex <> -1 then
      FMeasure.SliceField := FSlice.SliceField[cbBaseField.ItemIndex]
    else
      FMeasure.SliceField := nil;

    if cbExtraField.ItemIndex <> -1 then
      FMeasure.SliceFieldExtra := FSlice.SliceField[cbExtraField.ItemIndex]
    else
      FMeasure.SliceFieldExtra := FMeasure.SliceField;

    FMeasure.ScriptFunction := '';
    FMeasure.ScriptOrder := -1;
    FMeasure.CalcAllCells := False;
  end;
  FMeasure.Distinct := cbDistinct.Checked;
  if FMeasure.Distinct then
    FMeasure.SliceFieldDistinct := FSlice.SliceField[cbDistinctField.ItemIndex]
  else
    FMeasure.SliceFieldDistinct := FMeasure.SliceField;
end;

procedure TfcxMeasureEditorForm.SetupGeneralPage;
var
  i: Integer;
begin
  edName.Text := StringToControl(FMeasure.Name);
  edCaption.Text := StringToControl(FMeasure.Caption);
  cbAggregate.ItemIndex := Integer(FMeasure.AgrFunc) - 1;
  for i := 0 to FSlice.SliceFieldCount - 1 do
  begin
    cbBaseField.Items.AddObject(StringToControl(FSlice.SliceField[i].Caption), FSlice.SliceField[i]);
    cbDistinctField.Items.AddObject(StringToControl(FSlice.SliceField[i].Caption), FSlice.SliceField[i]);
    cbExtraField.Items.AddObject(StringToControl(FSlice.SliceField[i].Caption), FSlice.SliceField[i]);
  end;
  cbBaseField.ItemIndex := cbBaseField.Items.IndexOfObject(FMeasure.SliceField);
  FBaseFieldIndex := cbBaseField.ItemIndex;
  cbScriptFunction.Text := FMeasure.ScriptFunction;
  edOrder.Value := FMeasure.ScriptOrder;
  cbCalcAllCells.Checked := FMeasure.CalcAllCells;
  cbDistinct.Checked := FMeasure.Distinct;
  cbDistinctField.Enabled := cbDistinct.Checked;
  cbDistinctField.ItemIndex := cbDistinctField.Items.IndexOfObject(FMeasure.SliceFieldDistinct);
  cbExtraField.ItemIndex := cbExtraField.Items.IndexOfObject(FMeasure.SliceFieldExtra);
  cbAggregateChange(nil);
end;

procedure TfcxMeasureEditorForm.ApplyBtnClick(Sender: TObject);
begin
  Apply;
end;

procedure TfcxMeasureEditorForm.Localize;
const
  TotalsItems: array[TfcxTotalPosition] of String = (
 { fctp_Before } 'sBefore',
 { fctp_After  } 'sAfter',
 { fctp_Hide   } 'sHide'
  );
var
  AgrFunc: TfcxAgrFunc;
  TotalPos: TfcxTotalPosition;
  MeasureCaption: TfcxString;
begin
  MeasureCaption := FMeasure.Caption;
  if MeasureCaption = '' then
    MeasureCaption := FMeasure.Name;
  Caption := fcxResources.Get('sMeasureEditor') + Format(' [%s]', [StringToControl(MeasureCaption)]);
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
  ApplyBtn.Caption := fcxResources.Get('sApplyBtn');

  // general page
  PageGeneral.Caption := fcxResources.Get('sGeneral');
  lblName.Caption := fcxResources.Get('sName') + ':';
  lblCaption.Caption := fcxResources.Get('sCaption') + ':';
  lblAggregate.Caption := fcxResources.Get('sAggregate') + ':';

  for AgrFunc := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
    cbAggregate.Items.Add(fcxResources.Get(sFuncCaptions[AgrFunc]));

  lblBaseField.Caption := fcxResources.Get('sBaseField') + ':';
  lblExtraField.Caption := fcxResources.Get('sExtraField') + ':';
  lblFunction.Caption := fcxResources.Get('sFunction') + ':';
  lblOrder.Caption := fcxResources.Get('sOrder') + ':';
  cbCalcAllCells.Caption := fcxResources.Get('sCalcAllCells');

  lbDistinctField.Caption := fcxResources.Get('sDistinctField') + ':';
  cbDistinct.Caption := fcxResources.Get('sDistinct');

  // totals page
  PageTotals.Caption := fcxResources.Get('sTotals');
  cbUseDifferentAggregateForTotals.Caption := fcxResources.Get('sUseDifferentAggregateForTotals');
  lblAggregateTotal.Caption := fcxResources.Get('sAggregate') + ':';
  for AgrFunc := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
    cbAggregateForTotals.Items.Add(fcxResources.Get(sFuncCaptions[AgrFunc]));
  lblFunctionTotal.Caption := fcxResources.Get('sFunction') + ':';
  lblOrderTotal.Caption := fcxResources.Get('sOrder') + ':';
  cbCalcAllCellsTotal.Caption := fcxResources.Get('sCalcAllCells');

  cbCalcTotalsOnTotals.Caption := fcxResources.Get('sCalcTotalsOnTotals');
  cbUseXAxisTotalsAsBase.Caption := fcxResources.Get('sUseXAxisTotalsAsBase');
  rbTotalsConflictResolving.Caption := fcxResources.Get('sTotalsConflictResolving');
  rbTotalsConflictResolving.Items.Add(fcxResources.Get('sEmptyCell'));
  rbTotalsConflictResolving.Items.Add(fcxResources.Get('sUseXAxisTotal'));
  rbTotalsConflictResolving.Items.Add(fcxResources.Get('sUseYAxisTotal'));
  lblDefaultTotalPosition.Caption := fcxResources.Get('sTotalPosition') + ':';
  for TotalPos := Low(TfcxTotalPosition) to High(TfcxTotalPosition) do
    cbDefaultTotalPosition.Items.Add(fcxResources.Get(TotalsItems[TotalPos]));

  // filtering page
  PageFiltering.Caption := fcxResources.Get('sFiltering');
  lblFilterFunction.Caption := fcxResources.Get('sFunction') + ':';

  // style page
  PageStyle.Caption := fcxResources.Get('sDataMarker');
  hdStyles.Sections[0].Text := fcxResources.Get('sRule');
  hdStyles.Sections[1].Text := fcxResources.Get('sStyle');

  actAdd.Hint := fcxResources.Get('sAdd');
  actEdit.Hint := fcxResources.Get('sEditBtn');
  actDelete.Hint := fcxResources.Get('sDelete');
  actMoveUp.Hint := fcxResources.Get('sMoveUp');
  actMoveDown.Hint := fcxResources.Get('sMoveDown');
  tbStyles.Images := fcxGraphicResources.HighlightsImages;

  // format page
  PageFormat.Caption := fcxResources.Get('sDisplayFormat');
end;

procedure TfcxMeasureEditorForm.cbScriptFunctionDblClick(Sender: TObject);
{$IFDEF INCLUDE_FAST_SCRIPT}
var
  FuncName: String;
  LinePos: Integer;
  Combo: TCombobox absolute Sender;
{$ENDIF}
begin
{$IFDEF INCLUDE_FAST_SCRIPT}
  FuncName := Combo.Text;
  if FuncName = '' then
  begin
    if Combo = cbScriptFunction then
      FuncName := edName.Text + 'OnGetValue'
    else
    if Combo = cbScriptFunctionTotal then
      FuncName := edName.Text + 'OnGetTotalValue';
    Combo.Text := FuncName;
    if TfcxAgrFunc(cbAggregate.ItemIndex + 1) = af_FormulaDetail then
      LinePos := fcxLocateEventHandler(FSlice.Script, FSlice.ScriptLanguage, FuncName, TypeInfo(TfcxGetValue2))
    else
      LinePos := fcxLocateEventHandler(FSlice.Script, FSlice.ScriptLanguage, FuncName, TypeInfo(TfcxGetValue));
    if LinePos = -1 then
      if TfcxAgrFunc(cbAggregate.ItemIndex + 1) = af_FormulaDetail then
        LinePos := fcxAddEvent(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue2), FuncName)
      else
        LinePos := fcxAddEvent(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue), FuncName)
    else
      Inc(LinePos, 3);
  end
  else
  begin
    if TfcxAgrFunc(cbAggregate.ItemIndex + 1) = af_FormulaDetail then
      LinePos := fcxLocateEventHandler(FSlice.Script, FSlice.ScriptLanguage, FuncName, TypeInfo(TfcxGetValue2))
    else
      LinePos := fcxLocateEventHandler(FSlice.Script, FSlice.ScriptLanguage, FuncName, TypeInfo(TfcxGetValue));
    inc(LinePos, 3);
  end;
  with TfcxfsFormulaEditor.Create(Self) do
  begin
    ScriptComponent := FSlice;
    Edit(LinePos);
  end;
{$ENDIF}
end;

procedure TfcxMeasureEditorForm.cbScriptFunctionDropDown(Sender: TObject);
begin
  if TfcxAgrFunc(cbAggregate.ItemIndex + 1) = af_FormulaDetail then
    fcxGetEventHandlersList(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue2), (Sender as TCombobox).Items)
  else
    fcxGetEventHandlersList(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue), (Sender as TCombobox).Items);
end;

procedure TfcxMeasureEditorForm.ApplyStylePage;
begin
  FMeasure.Highlights.Assign(FHighlightRules);
end;

procedure TfcxMeasureEditorForm.SetupStylePage;
begin
  FHighlightRules.Assign(FMeasure.Highlights);
  UpdateStylesList;
  if FHighlightRules.Count > 0 then
    SelectedHighlight := 0
  else
    SelectedHighlight := -1;
end;

constructor TfcxMeasureEditorForm.Create(AOwner: TComponent);
begin
  inherited;
  FHighlightRules := TfcxCustomHighlights.Create(nil);
  FHighlightRules.OnChange := DoStylesChange;
  FBlockUpdate := False;
  FSelectedHighlight := -2;
end;

destructor TfcxMeasureEditorForm.Destroy;
begin
  FHighlightRules.Free;
  inherited;
end;

procedure TfcxMeasureEditorForm.lbStylesClick(Sender: TObject);
begin
  SelectedHighlight := lbStyles.ItemIndex;
end;

procedure TfcxMeasureEditorForm.SetSelectedHighlight(const Value: Integer);
begin
  if FSelectedHighlight <> Value then
  begin
    FBlockUpdate := True;
    FSelectedHighlight := Value;
    lbStyles.ItemIndex := Value;
    FBlockUpdate := False;
  end;
end;

procedure TfcxMeasureEditorForm.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled := SelectedHighlight >= 0;
end;

procedure TfcxMeasureEditorForm.actMoveUpUpdate(Sender: TObject);
begin
  actMoveUp.Enabled := SelectedHighlight > 0;
end;

procedure TfcxMeasureEditorForm.actMoveDownUpdate(Sender: TObject);
begin
  actMoveDown.Enabled := SelectedHighlight < FHighlightRules.Count - 1;
end;

procedure TfcxMeasureEditorForm.actMoveUpExecute(Sender: TObject);
begin
  with FHighlightRules[SelectedHighlight] do
    Index := Index - 1;
  UpdateStylesList;
  SelectedHighlight := SelectedHighlight - 1;
end;

procedure TfcxMeasureEditorForm.actMoveDownExecute(Sender: TObject);
begin
  with FHighlightRules[SelectedHighlight] do
    Index := Index + 1;
  UpdateStylesList;
  SelectedHighlight := SelectedHighlight + 1;
end;

procedure TfcxMeasureEditorForm.UpdateStylesList;
var
  i: integer;
begin
  lbStyles.Clear;
  for i := 0 to FHighlightRules.Count - 1 do
    lbStyles.Items.Add('');
end;

procedure TfcxMeasureEditorForm.actDeleteExecute(Sender: TObject);
begin
  if MessageDlg(fcxResources.Get('sConfirmDelete'), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FHighlightRules[SelectedHighlight].Free;
    UpdateStylesList;
    SelectedHighlight := SelectedHighlight - 1;
  end;
end;

procedure TfcxMeasureEditorForm.actAddExecute(Sender: TObject);
var
  Editor: TfcxHighlightRuleEditorDialog;
  Rule, NewRule: TfcxCustomHighlight;
begin
  Editor := TfcxHighlightRuleEditorDialog.Create(Self);
  Rule := TfcxCustomHighlight(fcxRegisteredHighlights.Items[0]^.ClassType.NewInstance);
  Rule.Create(nil);
  NewRule := Editor.Execute(Rule);
  if NewRule <> Rule then
  begin
    Rule.Free;
    Rule := FHighlightRules.Add(TfcxCustomHighlightClass(NewRule.ClassType));
    Rule.Assign(NewRule);
    UpdateStylesList;
    SelectedHighlight := FHighlightRules.Count - 1;
  end
  else
    Rule.Free;
end;

procedure TfcxMeasureEditorForm.lbStylesDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Highlight: TfcxCustomHighlight;
  Sz: TSize;
  S: String;
  R: Integer;
begin
  R := Rect.Right;
  Highlight := FHighlightRules[Index];
  S := Highlight.AsString;
  with lbStyles.Canvas do
  begin
    FillRect(Rect);
    Rect.Right := hdStyles.Sections[0].Width;
    Sz := TextExtent(S);
    TextOut((Rect.Left + Rect.Right - Sz.cx) div 2, (Rect.Top + Rect.Bottom - Sz.cy) div 2, S);
    Rect.Left := Rect.Right;
    Rect.Right := R;
    InflateRect(Rect, -5, -5);
    TfcxGraphicHighlight(Highlight).DrawExample(lbStyles.Canvas, Rect);
  end;
end;

procedure TfcxMeasureEditorForm.SetupFormatPage;
begin
  FormatFrame.Format := FMeasure.DisplayFormat;
end;

procedure TfcxMeasureEditorForm.ApplyFormatPage;
begin
  FMeasure.DisplayFormat := FormatFrame.Format;
end;

procedure TfcxMeasureEditorForm.DoStylesChange(Sender: TObject);
begin
  lbStyles.Invalidate;
end;

procedure TfcxMeasureEditorForm.actEditExecute(Sender: TObject);
var
  Editor: TfcxHighlightRuleEditorDialog;
  NewRule, Rule: TfcxCustomHighlight;
  Index: Integer;
begin
  Editor := TfcxHighlightRuleEditorDialog.Create(Self);
  Rule := FHighlightRules[SelectedHighlight];
  NewRule := Editor.Execute(Rule);
  if NewRule <> Rule then
  begin
    if NewRule.ClassType <> Rule.ClassType then
    begin
      Index := Rule.Index;
      Rule.Free;
      Rule := FHighlightRules.Add(TfcxCustomHighlightClass(NewRule.ClassType));
      Rule.Index := Index;
    end;
    Rule.Assign(NewRule);
    lbStyles.Invalidate;
  end;
end;

procedure TfcxMeasureEditorForm.actEditUpdate(Sender: TObject);
begin
  actEdit.Enabled := SelectedHighlight <> -1;
end;

procedure TfcxMeasureEditorForm.lbStylesDblClick(Sender: TObject);
begin
  actEdit.Execute;
end;

procedure TfcxMeasureEditorForm.cbFilterFunctionDropDown(Sender: TObject);
begin
  fcxGetEventHandlersList(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue), cbFilterFunction.Items);
end;

procedure TfcxMeasureEditorForm.cbFilterFunctionDblClick(Sender: TObject);
{$IFDEF INCLUDE_FAST_SCRIPT}
var
  FuncName: String;
  LinePos: Integer;
{$ENDIF}
begin
{$IFDEF INCLUDE_FAST_SCRIPT}
  FuncName := cbFilterFunction.Text;
  if FuncName = '' then
  begin
    FuncName := edName.Text + 'OnFilter';
    cbFilterFunction.Text := FuncName;
    LinePos := fcxLocateEventHandler(FSlice.Script, FSlice.ScriptLanguage, FuncName, TypeInfo(TfcxGetValue));
    if LinePos = -1 then
      LinePos := fcxAddEvent(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue), FuncName)
    else
      Inc(LinePos, 3);
  end
  else
  begin
    LinePos := fcxLocateEventHandler(FSlice.Script, FSlice.ScriptLanguage, FuncName, TypeInfo(TfcxGetValue));
    inc(LinePos, 3);
  end;
  with TfcxfsFormulaEditor.Create(Self) do
  begin
    ScriptComponent := FSlice;
    Edit(LinePos);
  end;
{$ENDIF}
end;

procedure TfcxMeasureEditorForm.ApplyFilteringPage;
begin
  FMeasure.FilterScriptFunction := cbFilterFunction.Text;
end;

procedure TfcxMeasureEditorForm.SetupFilteringPage;
begin
  cbFilterFunction.Text := FMeasure.FilterScriptFunction;
end;

procedure TfcxMeasureEditorForm.SetupTotalsPage;
begin
  cbUseDifferentAggregateForTotals.Checked := FMeasure.UseDifferentAggForTotals;
  cbAggregateForTotals.ItemIndex := Integer(FMeasure.AgrFuncForTotals) - 1;
  cbScriptFunctionTotal.Text := FMeasure.ScriptFunctionForTotals;
  edOrderTotal.Value := FMeasure.ScriptOrderForTotals;
  cbCalcAllCellsTotal.Checked := FMeasure.CalcAllCellsForTotals;
  cbCalcTotalsOnTotals.Checked := FMeasure.CalcTotalsOnTotals;
  cbUseXAxisTotalsAsBase.Checked := FMeasure.UseXAxisTotalsAsBase;
  cbUseXAxisTotalsAsBase.Enabled := cbCalcTotalsOnTotals.Checked;
  rbTotalsConflictResolving.ItemIndex := Ord(FMeasure.ConflictResolve);
  cbDefaultTotalPosition.ItemIndex := Ord(FMeasure.DefaultTotalPosition);
  cbUseDifferentAggregateForTotalsClick(nil);
{
    FAgrFuncForDimCount: TfcxSmallCount; // count items in FAgrFuncForDimensionArray
    FAgrFuncForDimensionArray: PfcxAgrFuncForDimensionArray;
    UseTotalPositionFromMeasure
}
end;

procedure TfcxMeasureEditorForm.ApplyTotalsPage;
begin
  FMeasure.UseDifferentAggForTotals := cbUseDifferentAggregateForTotals.Checked;
  FMeasure.AgrFuncForTotals := TfcxAgrFunc(cbAggregateForTotals.ItemIndex + 1);
  if FMeasure.AgrFuncForTotals in ScriptFuncs then
  begin
    FMeasure.ScriptFunctionForTotals := cbScriptFunctionTotal.Text;
    FMeasure.ScriptOrderForTotals := edOrderTotal.Value;
    if FMeasure.AgrFuncForTotals in CanCreateAllCellsFuncs then
      FMeasure.CalcAllCellsForTotals := cbCalcAllCellsTotal.Checked
    else
      FMeasure.CalcAllCellsForTotals := False;
  end
  else
  begin
    FMeasure.ScriptFunctionForTotals := '';
    FMeasure.ScriptOrderForTotals := -1;
    FMeasure.CalcAllCellsForTotals := False;
  end;
  FMeasure.CalcTotalsOnTotals := cbCalcTotalsOnTotals.Checked;
  if FMeasure.CalcTotalsOnTotals then
    FMeasure.UseXAxisTotalsAsBase := cbUseXAxisTotalsAsBase.Checked
  else
    FMeasure.UseXAxisTotalsAsBase := False;
  FMeasure.ConflictResolve := TfcxTotalsConflictResolve(rbTotalsConflictResolving.ItemIndex);
  FMeasure.DefaultTotalPosition := TfcxTotalPosition(cbDefaultTotalPosition.ItemIndex);
end;

procedure TfcxMeasureEditorForm.cbUseDifferentAggregateForTotalsClick(Sender: TObject);
var
  Calculated: Boolean;
begin
  Calculated := TfcxAgrFunc(cbAggregateForTotals.ItemIndex + 1) in ScriptFuncs;
  cbAggregateForTotals.Enabled := cbUseDifferentAggregateForTotals.Checked;
  lblFunctionTotal.Enabled := cbUseDifferentAggregateForTotals.Checked and Calculated;
  cbScriptFunctionTotal.Enabled := cbUseDifferentAggregateForTotals.Checked and Calculated;
  lblOrderTotal.Enabled := cbUseDifferentAggregateForTotals.Checked and Calculated;
  edOrderTotal.Enabled := cbUseDifferentAggregateForTotals.Checked and Calculated;
  cbCalcAllCellsTotal.Enabled := cbUseDifferentAggregateForTotals.Checked and (TfcxAgrFunc(cbAggregateForTotals.ItemIndex + 1) in CanCreateAllCellsFuncs);
end;

procedure TfcxMeasureEditorForm.cbCalcTotalsOnTotalsClick(Sender: TObject);
begin
  cbUseXAxisTotalsAsBase.Enabled := cbCalcTotalsOnTotals.Checked;
end;

procedure TfcxMeasureEditorForm.cbAggregateChange(Sender: TObject);
var
  Calculated, NeedsExtra: Boolean;
begin
  Calculated := TfcxAgrFunc(cbAggregate.ItemIndex + 1) in ScriptFuncs;
  NeedsExtra := TfcxAgrFunc(cbAggregate.ItemIndex + 1) in ExtraFuncs;
  lblBaseField.Enabled := not Calculated;
  cbBaseField.Enabled := not Calculated;
  lblFunction.Enabled := Calculated;
  cbScriptFunction.Enabled := Calculated;
  lblOrder.Enabled := Calculated;
  edOrder.Enabled := Calculated;
  cbCalcAllCells.Enabled := TfcxAgrFunc(cbAggregate.ItemIndex + 1) in CanCreateAllCellsFuncs;
  lblExtraField.Enabled := NeedsExtra;
  cbExtraField.Enabled := NeedsExtra;
end;

procedure TfcxMeasureEditorForm.cbBaseFieldSelect(Sender: TObject);
begin
  if FBaseFieldIndex = cbDistinctField.ItemIndex then
    cbDistinctField.ItemIndex := cbBaseField.ItemIndex;
  FBaseFieldIndex := cbBaseField.ItemIndex;
end;

procedure TfcxMeasureEditorForm.cbDistinctClick(Sender: TObject);
begin
  if not cbDistinct.Checked then
    cbDistinctField.ItemIndex := cbBaseField.ItemIndex;
  cbDistinctField.Enabled := cbDistinct.Checked;
end;

end.
