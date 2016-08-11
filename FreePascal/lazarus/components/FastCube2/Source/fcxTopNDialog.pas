{*******************************************************}
{                                                       }
{             FastCube 2 Top N dialog unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxTopNDialog;

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
  fcxTypes, fcxSlice, Spin;

type
  TfcxTopNDialogForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    lblDimension: TLabel;
    lblMeasure: TLabel;
    cbCreateOthers: TCheckBox;
    cbDimension: TComboBox;
    cbMeasure: TComboBox;
    lblShow: TLabel;
    SpinEdit1: TSpinEdit;
    cbTopType: TComboBox;
  private
    FAxisContainer: TfcxAxisContainer;
    procedure Localize;
    function GetDimension: TfcxAxisField;
    function GetMeasure: TfcxMeasureField;
    function GetTopCount: Integer;
    function GetTopType: TfcxTopType;
    function GetCreateOthers: Boolean;
  public
    function Execute(AAxisContainer: TfcxAxisContainer; ADimension: TfcxAxisField): Boolean;

    property Dimension: TfcxAxisField read GetDimension;
    property Measure: TfcxMeasureField read GetMeasure;
    property TopCount: Integer read GetTopCount;
    property TopType: TfcxTopType read GetTopType;
    property CreateOthers: Boolean read GetCreateOthers;
  end;

implementation

uses
  fcxRes;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

function TfcxTopNDialogForm.Execute(AAxisContainer: TfcxAxisContainer; ADimension: TfcxAxisField): Boolean;
var
  FieldsOfAxis: TfcxCommonFieldsOfRegion;
  i: integer;
begin
  FAxisContainer := AAxisContainer;

  Localize;

  FieldsOfAxis := AAxisContainer.Slice.FieldsOfRegion[AAxisContainer.Region];

  for i := 0 to FieldsOfAxis.Count - 1 do
    cbDimension.AddItem(StringToControl(FieldsOfAxis.Items[i].Caption), FieldsOfAxis.Items[i]);

  for i := 0 to AAxisContainer.Slice.MeasuresContainer.Count - 1 do
    cbMeasure.AddItem(StringToControl(AAxisContainer.Slice.MeasuresContainer.Measures[i].Caption), AAxisContainer.Slice.MeasuresContainer.Measures[i]);

  if cbDimension.Items.Count > 0 then
    cbDimension.ItemIndex := cbDimension.Items.IndexOfObject(ADimension);

  if cbMeasure.Items.Count > 0 then
    cbMeasure.ItemIndex := 0;

  cbTopType.ItemIndex := 0;

  Result := ShowModal = mrOk;
end;

function TfcxTopNDialogForm.GetCreateOthers: Boolean;
begin
  Result := cbCreateOthers.Checked;
end;

function TfcxTopNDialogForm.GetDimension: TfcxAxisField;
begin
  if cbDimension.ItemIndex >= 0 then
    Result := TfcxAxisField(cbDimension.Items.Objects[cbDimension.ItemIndex])
  else
    Result := nil;
end;

function TfcxTopNDialogForm.GetMeasure: TfcxMeasureField;
begin
  if cbMeasure.ItemIndex >= 0 then
    Result := TfcxMeasureField(cbMeasure.Items.Objects[cbMeasure.ItemIndex])
  else
    Result := nil;
end;

function TfcxTopNDialogForm.GetTopCount: Integer;
begin
  Result := SpinEdit1.Value;
end;

function TfcxTopNDialogForm.GetTopType: TfcxTopType;
begin
  Result := TfcxTopType(cbTopType.ItemIndex);
end;

procedure TfcxTopNDialogForm.Localize;
const
  sTopType: array[TfcxTopType] of String = (
 { ttMax } 'sTopMax',
 { ttMin } 'sTopMin'
  );
var
  TopType: TfcxTopType;
begin
  Caption := fcxResources.Get('sShowTopN');
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');

  lblDimension.Caption := fcxResources.Get('sDimension') + ':';
  lblMeasure.Caption := fcxResources.Get('sMeasure') + ':';
  lblShow.Caption := fcxResources.Get('sShow') + ':';
  cbCreateOthers.Caption := fcxResources.Get('sCreateOthers');

  for TopType := Low(TfcxTopType) to High(TfcxTopType) do
    cbTopType.Items.Add(fcxResources.Get(sTopType[TopType]));
end;

end.
