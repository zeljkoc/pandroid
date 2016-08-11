{*******************************************************}
{                                                       }
{             FastCube 2 axis editor unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxAxisEditor;

{$I fcx.inc}

interface

uses
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  fcxTypes, fcxSlice, Spin, ExtCtrls, CheckLst, ActnList,
  fcxFormatFrame;

type
  TfcxAxisEditorForm = class(TForm)
    Pages: TPageControl;
    PageGeneral: TTabSheet;
    OkBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    lblAdditionalTotals: TLabel;
    clbAdditionalTotals: TCheckListBox;
    lblTotalPosition: TLabel;
    cbTotalPosition: TComboBox;
    lblAxisType: TLabel;
    cbAxisType: TComboBox;
    lblSortType: TLabel;
    cbSortType: TComboBox;
    cbHideZeros: TCheckBox;
    lblFunctionTotal: TLabel;
    cbScriptFunctionTotal: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyBtnClick(Sender: TObject);
    procedure cbScriptFunctionTotalDropDown(Sender: TObject);
    procedure cbScriptFunctionTotalDblClick(Sender: TObject);
  private
    FSlice: TfcxSlice;
    FAxis: TfcxAxisContainer;
    procedure Localize;
  protected
    procedure SetupGeneralPage;

    procedure ApplyGeneralPage;
    procedure Apply;

  public
    function Execute(ASlice: TfcxSlice; AAxis: TfcxAxisContainer; const PageIndex: Integer = 0): Boolean;
  end;

const
  fcxAxisEditorGeneralPageIndex = 0;
  fcxAxisEditorFormatPageIndex = 1;

implementation

uses
  fcxCodeUtils,
  fcxRes
{$ifdef INCLUDE_FAST_SCRIPT}
  , fcxfsFormulaEditor
{$ENDIF};

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{ TAxisEditor }

procedure TfcxAxisEditorForm.Apply;
begin
//  FAxis.BeginUpdate;
  ApplyGeneralPage;
//  FAxis.EndUpdate;
end;

function TfcxAxisEditorForm.Execute(ASlice: TfcxSlice; AAxis: TfcxAxisContainer; const PageIndex: Integer = 0): Boolean;
begin
  FSlice := ASlice;
  FAxis := AAxis;
  Pages.ActivePageIndex := PageIndex;

  Localize;

  SetupGeneralPage;
  Result := ShowModal = mrOk;
  if Result then
    Apply;
end;

procedure TfcxAxisEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfcxAxisEditorForm.ApplyGeneralPage;
var
  TotalFunctions: TfcxSetAgrFunc;
  I: Integer;
begin
  FAxis.AxisType := TfcxAxisType(cbAxisType.ItemIndex);
  FAxis.DefaultTypeSort := TfcxTypeSortAxis(cbSortType.ItemIndex);

  if FSlice.XAxisContainer = FAxis then
    FSlice.HideColZeros := cbHideZeros.Checked
  else
    FSlice.HideRowZeros := cbHideZeros.Checked;

  FAxis.GrandTotalPosition := TfcxTotalPosition(cbTotalPosition.ItemIndex);
  FAxis.AdditionalGrandTotalScriptFunction := cbScriptFunctionTotal.Text;
  TotalFunctions := [];
  for I := 0 to clbAdditionalTotals.Items.Count - 1 do
    if clbAdditionalTotals.Checked[I] then
      Include(TotalFunctions, TfcxAgrFunc(PtrInt(clbAdditionalTotals.Items.Objects[I])));
  FAxis.AdditionalGrandTotalFunctions := TotalFunctions;
end;

procedure TfcxAxisEditorForm.SetupGeneralPage;
var
  I: Integer;
begin
  cbAxisType.ItemIndex := Ord(FAxis.AxisType);
  cbSortType.ItemIndex := Ord(FAxis.DefaultTypeSort);

  if FSlice.XAxisContainer = FAxis then
    cbHideZeros.Checked := FSlice.HideColZeros
  else
    cbHideZeros.Checked := FSlice.HideRowZeros;

  cbTotalPosition.ItemIndex := Ord(FAxis.GrandTotalPosition);
  cbScriptFunctionTotal.Text := FAxis.AdditionalGrandTotalScriptFunction;
  for I := 0 to clbAdditionalTotals.Items.Count - 1 do
    clbAdditionalTotals.Checked[I] := TfcxAgrFunc(PtrInt(clbAdditionalTotals.Items.Objects[I])) in FAxis.AdditionalGrandTotalFunctions;
end;

procedure TfcxAxisEditorForm.ApplyBtnClick(Sender: TObject);
begin
  Apply;
end;

procedure TfcxAxisEditorForm.Localize;
const
  TotalsItems: array[TfcxTotalPosition] of String = (
 { fctp_Before } 'sBefore',
 { fctp_After  } 'sAfter',
 { fctp_Hide   } 'sHide'
  );
  
  SortItems: array[TfcxTypeSortAxis] of String = (
 { md_tsa_ByAxisValue  } 'sSortByValues',
 { md_tsa_ByTotalValue } 'sSortByTotals',
 { md_tsa_BySelected   } 'sSortByXSelection'
  );

  AxisItems: array[TfcxAxisType] of String = (
 { at_Standard } 'sStandard',
 { at_Tree     } 'sTreeLike'
  );
var
  TotalPos: TfcxTotalPosition;
  AgrFunc: TfcxAgrFunc;
  AxisType: TfcxAxisType;
  SortType: TfcxTypeSortAxis;
begin
  Caption := fcxResources.Get('sAxisEditor');
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
  ApplyBtn.Caption := fcxResources.Get('sApplyBtn');

  // general page
  PageGeneral.Caption := fcxResources.Get('sGeneral');

  lblAxisType.Caption := fcxResources.Get('sAxisType') + ':';
  for AxisType := Low(TfcxAxisType) to High(TfcxAxisType) do
    cbAxisType.Items.Add(fcxResources.Get(AxisItems[AxisType]));

  if FAxis = FSlice.XAxisContainer then
  begin
    lblSortType.Caption := fcxResources.Get('sColSort') + ':';
    cbHideZeros.Caption := fcxResources.Get('sHideColZeros');
  end
  else
  begin
    lblSortType.Caption := fcxResources.Get('sRowSort') + ':';
    cbHideZeros.Caption := fcxResources.Get('sHideRowZeros');
  end;

  for SortType := Low(TfcxTypeSortAxis) to High(TfcxTypeSortAxis) do
    cbSortType.Items.Add(fcxResources.Get(SortItems[SortType]));

  if FAxis = FSlice.YAxisContainer then
    cbSortType.Items[2] := fcxResources.Get('sSortByYSelection');

  lblTotalPosition.Caption := fcxResources.Get('sGrandTotalPosition') + ':';
  for TotalPos := Low(TfcxTotalPosition) to High(TfcxTotalPosition) do
    cbTotalPosition.Items.Add(fcxResources.Get(TotalsItems[TotalPos]));

  lblFunctionTotal.Caption := fcxResources.Get('sFunction') + ':';
  lblAdditionalTotals.Caption := fcxResources.Get('sAdditionalGrandTotals') + ':';
  for AgrFunc := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
    clbAdditionalTotals.Items.AddObject(fcxResources.Get(sFuncCaptions[AgrFunc]), TObject(PtrInt(AgrFunc)));
end;

procedure TfcxAxisEditorForm.cbScriptFunctionTotalDropDown(
  Sender: TObject);
begin
  fcxGetEventHandlersList(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue), (Sender as TCombobox).Items);
end;

procedure TfcxAxisEditorForm.cbScriptFunctionTotalDblClick(
  Sender: TObject);
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
    FuncName := 'OnGetAddGrandTotalValue';
    Combo.Text := FuncName;
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

end.
