{*******************************************************}
{                                                       }
{           FastCube 2 dimension editor unit            }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxDimensionEditor;

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

  { TfcxDimensionEditorForm }

  TfcxDimensionEditorForm = class(TForm)
    Pages: TPageControl;
    PageGeneral: TTabSheet;
    OkBtn: TButton;
    CancelBtn: TButton;
    ApplyBtn: TButton;
    lblCaption: TLabel;
    edCaption: TEdit;
    PageFormat: TTabSheet;
    FormatFrame: TfcxFormatEditorFrame;
    lblTotalPosition: TLabel;
    cbTotalPosition: TComboBox;
    cbUseTotalPositionFromMeasure: TCheckBox;
    lblSortDirection: TLabel;
    cbSortDirection: TComboBox;
    lblAdditionalTotals: TLabel;
    clbAdditionalTotals: TCheckListBox;
    lblFunctionTotal: TLabel;
    cbScriptFunctionTotal: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyBtnClick(Sender: TObject);
    procedure cbScriptFunctionTotalDropDown(Sender: TObject);
    procedure cbScriptFunctionTotalDblClick(Sender: TObject);
  private
    FSlice: TfcxSlice;
    FDimension: TfcxAxisField;
    procedure Localize;
  protected
    procedure SetupGeneralPage;
    procedure SetupFormatPage;

    procedure ApplyGeneralPage;
    procedure ApplyFormatPage;
    procedure Apply;

  public
    function Execute(ASlice: TfcxSlice; ADimension: TfcxAxisField; const PageIndex: Integer = 0): Boolean;
  end;

const
  fcxDimensionEditorGeneralPageIndex = 0;
  fcxDimensionEditorFormatPageIndex = 1;

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

{ TDimensionEditor }

procedure TfcxDimensionEditorForm.Apply;
begin
  FDimension.BeginUpdate;
  ApplyGeneralPage;
  ApplyFormatPage;
  FDimension.EndUpdate;
end;

function TfcxDimensionEditorForm.Execute(ASlice: TfcxSlice; ADimension: TfcxAxisField; const PageIndex: Integer = 0): Boolean;
begin
  FSlice := ASlice;
  FDimension := ADimension;
  Pages.ActivePageIndex := PageIndex;

  Localize;

  SetupGeneralPage;
  SetupFormatPage;
  Result := ShowModal = mrOk;
  if Result then
    Apply;
end;

procedure TfcxDimensionEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfcxDimensionEditorForm.ApplyGeneralPage;
var
  TotalFunctions: TfcxSetAgrFunc;
  I: Integer;
begin
  FDimension.Caption := ControlToString(edCaption.Text);
  FDimension.TotalPosition := TfcxTotalPosition(cbTotalPosition.ItemIndex);
  FDimension.UseTotalPositionFromMeasure := cbUseTotalPositionFromMeasure.Checked;
  FDimension.SortDirection := TfcxSortDirection(cbSortDirection.ItemIndex);
  FDimension.AdditionalTotalScriptFunction := cbScriptFunctionTotal.Text;
  TotalFunctions := [];
  for I := 0 to clbAdditionalTotals.Items.Count - 1 do
    if clbAdditionalTotals.Checked[I] then
      Include(TotalFunctions, TfcxAgrFunc(PtrInt(clbAdditionalTotals.Items.Objects[I])));
  FDimension.AdditionalTotalFunctions := TotalFunctions;
end;

procedure TfcxDimensionEditorForm.SetupGeneralPage;
var
  I: Integer;
begin
  edCaption.Text := StringToControl(FDimension.Caption);
  cbTotalPosition.ItemIndex := Ord(FDimension.TotalPosition);
  cbUseTotalPositionFromMeasure.Checked := FDimension.UseTotalPositionFromMeasure;
  cbSortDirection.ItemIndex := Ord(FDimension.SortDirection);
  cbScriptFunctionTotal.Text := FDimension.AdditionalTotalScriptFunction;
  for I := 0 to clbAdditionalTotals.Items.Count - 1 do
    clbAdditionalTotals.Checked[I] := TfcxAgrFunc(PtrInt(clbAdditionalTotals.Items.Objects[I])) in FDimension.AdditionalTotalFunctions;
end;

procedure TfcxDimensionEditorForm.ApplyBtnClick(Sender: TObject);
begin
  Apply;
end;

procedure TfcxDimensionEditorForm.Localize;
const
  TotalsItems: array[TfcxTotalPosition] of String = (
 { fctp_Before } 'sBefore',
 { fctp_After  } 'sAfter',
 { fctp_Hide   } 'sHide'
  );
  SortItems: array[TfcxSortDirection] of String = (
  { fcsd_Asc  } 'sAscending',
  { fcsd_Desc } 'sDescending'
  );
var
  TotalPos: TfcxTotalPosition;
  SortDirection: TfcxSortDirection;
  AgrFunc: TfcxAgrFunc;
  DimensionCaption: TfcxString;
begin
  DimensionCaption := FDimension.Caption;
  if DimensionCaption = '' then
    DimensionCaption := FDimension.Name;
  Caption := fcxResources.Get('sDimensionEditor') + Format(' [%s]', [StringToControl(DimensionCaption)]);
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
  ApplyBtn.Caption := fcxResources.Get('sApplyBtn');

  // general page
  PageGeneral.Caption := fcxResources.Get('sGeneral');
  lblCaption.Caption := fcxResources.Get('sCaption') + ':';
  lblTotalPosition.Caption := fcxResources.Get('sTotalPosition') + ':';
  for TotalPos := Low(TfcxTotalPosition) to High(TfcxTotalPosition) do
    cbTotalPosition.Items.Add(fcxResources.Get(TotalsItems[TotalPos]));
  cbUseTotalPositionFromMeasure.Caption := fcxResources.Get('sUseTotalPositionFromMeasure');
  lblSortDirection.Caption := fcxResources.Get('sSortDirection') + ':';
  for SortDirection := Low(TfcxSortDirection) to High(TfcxSortDirection) do
    cbSortDirection.Items.Add(fcxResources.Get(SortItems[SortDirection]));
  lblFunctionTotal.Caption := fcxResources.Get('sFunction') + ':';
  lblAdditionalTotals.Caption := fcxResources.Get('sAdditionalTotals') + ':';
  for AgrFunc := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
    clbAdditionalTotals.Items.AddObject(fcxResources.Get(sFuncCaptions[AgrFunc]), TObject(PtrInt(AgrFunc)));

  // format page
  PageFormat.Caption := fcxResources.Get('sDisplayFormat');
end;

procedure TfcxDimensionEditorForm.SetupFormatPage;
begin
  FormatFrame.Format := FDimension.DisplayFormat;
end;

procedure TfcxDimensionEditorForm.ApplyFormatPage;
begin
  FDimension.DisplayFormat := FormatFrame.Format;
end;

procedure TfcxDimensionEditorForm.cbScriptFunctionTotalDropDown(
  Sender: TObject);
begin
  fcxGetEventHandlersList(FSlice.Script, FSlice.ScriptLanguage, TypeInfo(TfcxGetValue), (Sender as TCombobox).Items);
end;

procedure TfcxDimensionEditorForm.cbScriptFunctionTotalDblClick(
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
    FuncName := FDimension.Name + 'OnGetAddTotalValue';
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
