{*******************************************************}
{                                                       }
{             FastCube 2 ChartEdit unit                 }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxChartEditor;

interface

{$INCLUDE fcx.inc}
{$INCLUDE fcxTee.inc}

uses
{$IFDEF FPC}
  Types, 
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls,
  fcxTypes, fcxSlice, fcxChart, Spin;

type

  { TfcxChartEditor }

  TfcxChartEditor = class(TForm)
    OkBtn: TButton;
    GroupBox2: TGroupBox;
    ChartDataType: TComboBox;
    CancelBtn: TButton;
    RealTimeChange: TCheckBox;
    SeriesAxis: TRadioGroup;
    ValuesAxis: TRadioGroup;
    lbSeriesFieldCount: TLabel;
    lbValuesFieldCount: TLabel;
    lbMeasureFieldIndex: TLabel;
    SkipNullPoints: TCheckBox;
    BaseAxisDataType: TComboBox;
    lbBaseAxisDataType: TLabel;
    SeriesFieldCount: TSpinEdit;
    MeasureFieldIndex: TSpinEdit;
    ValuesFieldCount: TSpinEdit;
    procedure BaseAxisDataTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChartDataTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SeriesAxisClick(Sender: TObject);
    procedure SkipNullPointsClick(Sender: TObject);
    procedure ValuesAxisClick(Sender: TObject);
    procedure SeriesFieldCountChange(Sender: TObject);
    procedure MeasureFieldIndexChange(Sender: TObject);
    procedure ValuesFieldCountChange(Sender: TObject);
  private
    FLoadingData: Boolean;
    procedure Localize;
    procedure FromChart;
    procedure ChartDataTypeChanged;
  public
    Chart: TfcxCustomChart;
  end;

implementation

uses
  fcxRes;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TfcxChartEditor }

procedure TfcxChartEditor.Localize;
const
  TypeChartDataCaptions: array[0..2] of String = ('sChartByAxisAxis', 'sChartByAxisMeasures', 'sChartByMeasuresAxis');
  TypeAxisCaptions: array[0..1] of String = ('sAxisX', 'sAxisY');
  BaseAxisDataTypeCaptions: array[0..2] of String = ('sString', 'sNumeric', 'sDateTime');
var
  I: integer;
begin
  ChartDataType.Clear;
  for I := Low(TypeChartDataCaptions) to High(TypeChartDataCaptions) do
    ChartDataType.Items.Add(fcxResources.Get(TypeChartDataCaptions[i]));

  SeriesAxis.Items.Clear;
  ValuesAxis.Items.Clear;
  for I := Low(TypeAxisCaptions) to High(TypeAxisCaptions) do
  begin
    SeriesAxis.Items.Add(fcxResources.Get(TypeAxisCaptions[i]));
    ValuesAxis.Items.Add(fcxResources.Get(TypeAxisCaptions[i]));
  end;

  RealTimeChange.Caption := fcxResources.Get('sChartRealTimeChange');
  GroupBox2.Caption      := fcxResources.Get('sChartDataSource');
  SeriesAxis.Caption     := fcxResources.Get('sChartSeriesAxis');
  ValuesAxis.Caption     := fcxResources.Get('sChartValuesAxis');
  lbSeriesFieldCount.Caption  := fcxResources.Get('sChartSeriesFieldCount');
  lbValuesFieldCount.Caption  := fcxResources.Get('sChartValuesFieldCount');
  lbMeasureFieldIndex.Caption := fcxResources.Get('sChartMeasureFieldIndex');
  OkBtn.Caption     := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
  Caption           := fcxResources.Get('sChartDataProperty');
  SkipNullPoints.Caption := fcxResources.Get('sSkipNullPoints');
  lbBaseAxisDataType.Caption := fcxResources.Get('sBaseAxisDataType');
  BaseAxisDataType.Clear;
  for I := Low(BaseAxisDataTypeCaptions) to High(BaseAxisDataTypeCaptions) do
    BaseAxisDataType.Items.Add(fcxResources.Get(BaseAxisDataTypeCaptions[i]));
end;

procedure TfcxChartEditor.FormCreate(Sender: TObject);
begin
  FLoadingData := True;
  Localize;
end;

procedure TfcxChartEditor.ChartDataTypeChange(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.TypeChartData := TfcxTypeChartData(ChartDataType.ItemIndex);
  ChartDataTypeChanged;
end;

procedure TfcxChartEditor.FormShow(Sender: TObject);
begin
  if Assigned(Chart) then
    FromChart;
end;

procedure TfcxChartEditor.FromChart;
begin
  FLoadingData := True;
  ChartDataType.ItemIndex := ord(Chart.TypeChartData);
  SeriesAxis.ItemIndex := ord(Chart.SeriesAxis);
  ValuesAxis.ItemIndex := ord(Chart.CategoriesAxis);
  SeriesFieldCount.Value := Chart.SeriesFieldCount;
  ValuesFieldCount.Value := Chart.CategoriesFieldCount;
  MeasureFieldIndex.Value := Chart.MeasureFieldIndex;
  SkipNullPoints.Checked := Chart.SkipNullPoints;
  BaseAxisDataType.ItemIndex := ord(Chart.BaseAxisDataType);
  ChartDataTypeChanged;
  FLoadingData := False;
end;

procedure TfcxChartEditor.SeriesAxisClick(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.SeriesAxis := TfcxAxisRegion(SeriesAxis.ItemIndex);
end;

procedure TfcxChartEditor.ValuesAxisClick(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.CategoriesAxis := TfcxAxisRegion(ValuesAxis.ItemIndex);
end;

procedure TfcxChartEditor.ChartDataTypeChanged;
begin
  case ChartDataType.ItemIndex of
    0:
      begin
        SeriesAxis.Visible := true;
        ValuesAxis.Visible := true;
      end;
    1:
      begin
        SeriesAxis.Visible := false;
        ValuesAxis.Visible := true;
      end;
    2:
      begin
        SeriesAxis.Visible := true;
        ValuesAxis.Visible := false;
      end;
  end;
  lbSeriesFieldCount.Visible := SeriesAxis.Visible;
  SeriesFieldCount.Visible := SeriesAxis.Visible;
  lbValuesFieldCount.Visible := ValuesAxis.Visible;
  ValuesFieldCount.Visible := ValuesAxis.Visible;
  lbMeasureFieldIndex.Visible := ValuesAxis.Visible and SeriesAxis.Visible;
  MeasureFieldIndex.Visible := ValuesAxis.Visible and SeriesAxis.Visible;
end;

procedure TfcxChartEditor.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  if (ModalResult = mrOk) and (not RealTimeChange.Checked) and Assigned(Chart) then
  begin
    Chart.BeginUpdate;
    try
      Chart.TypeChartData := TfcxTypeChartData(ChartDataType.ItemIndex);
      Chart.MeasureFieldIndex := MeasureFieldIndex.Value;
      Chart.SeriesFieldCount := SeriesFieldCount.Value;
      Chart.CategoriesFieldCount := ValuesFieldCount.Value;
      Chart.CategoriesAxis := TfcxAxisRegion(ValuesAxis.ItemIndex);
      Chart.SeriesAxis := TfcxAxisRegion(SeriesAxis.ItemIndex);
      Chart.SkipNullPoints := SkipNullPoints.Checked;
      Chart.BaseAxisDataType := TfcxAxisDataType(BaseAxisDataType.ItemIndex);
    finally
      Chart.EndUpdate;
    end;
  end;
end;

procedure TfcxChartEditor.SkipNullPointsClick(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.SkipNullPoints := SkipNullPoints.Checked;
end;

procedure TfcxChartEditor.BaseAxisDataTypeChange(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.BaseAxisDataType := TfcxAxisDataType(BaseAxisDataType.ItemIndex);
end;

procedure TfcxChartEditor.SeriesFieldCountChange(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.SeriesFieldCount := SeriesFieldCount.Value;
end;

procedure TfcxChartEditor.MeasureFieldIndexChange(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.MeasureFieldIndex := MeasureFieldIndex.Value;
end;

procedure TfcxChartEditor.ValuesFieldCountChange(Sender: TObject);
begin
  if not FLoadingData and RealTimeChange.Checked and Assigned(Chart) then
    Chart.CategoriesFieldCount := ValuesFieldCount.Value;
end;

end.
