{*******************************************************}
{                                                       }
{             FastCube 2 information unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
unit fcxInfo;

interface
{$INCLUDE fcx.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, fcxSlice, fcxTypes, fcxCube;

type

  { TfcStatForm }

  TfcxInfoForm = class(TForm)
    GroupBox1: TGroupBox;
    lRowDimCount: TLabel;
    lColDimCount: TLabel;
    lFactsCount: TLabel;
    lFilterCount: TLabel;
    vRowDimCount: TLabel;
    vColDimCount: TLabel;
    vFactsCount: TLabel;
    vFilterCount: TLabel;
    Button1: TButton;
    GroupBox2: TGroupBox;
    lDBCloseTime: TLabel;
    lDBGetDataTime: TLabel;
    lDBOpenTime: TLabel;
    lOpenTime: TLabel;
    vDBCloseTime: TLabel;
    vDBGetDataTime: TLabel;
    vDBOpenTime: TLabel;
    vOpenTime: TLabel;
    lConvertTime: TLabel;
    vConvertTime: TLabel;
    lSortTime: TLabel;
    vSortTime: TLabel;
    lSourceRowCount: TLabel;
    vSourceRowCount: TLabel;
    lFullTime: TLabel;
    vFullTime: TLabel;
    lDBMoveTime: TLabel;
    vDBMoveTime: TLabel;
    FastCubeVer: TLabel;
    lRowCount: TLabel;
    vRowCount: TLabel;
    lColCount: TLabel;
    vColCount: TLabel;
    lDataBuildTime: TLabel;
    vDataBuildTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    procedure UpdateCaptions;
    procedure ShowInfo(Slice: TfcxSlice);
  end;

implementation
uses
  fcxRes;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}  
  {$R *.dfm}
{$ENDIF}

{ TfcxInfoForm }

procedure TfcxInfoForm.ShowInfo(Slice: TfcxSlice);

  procedure SetLabels(l: array of TLabel; v: array of Integer);
  var
    i : integer;
  begin
    for i := Low(l) to High(l) do
      l[i].Caption := IntToStr(v[i]);
  end;

  procedure SetTimeLabels(l: array of TLabel; v: array of Integer);
  var
    i : integer;
  begin
    for i := Low(l) to High(l) do
      l[i].Caption := Format('%.3f', [v[i] / 1000]);
  end;

begin
  SetLabels([vSourceRowCount, vRowDimCount, vColDimCount, vFactsCount, vFilterCount, vRowCount, vColCount],
            [Slice.Cube.SourceHolder.RecordsCount,
             Slice.FieldsOfRegion[rf_CapYAx].Count,
             Slice.FieldsOfRegion[rf_CapXAx].Count,
             Slice.FieldsOfRegion[rf_CapFacts].Count,
             Slice.FieldsOfRegion[rf_Page].Count,
             Slice.RowCount,
             Slice.ColCount]);

  SetTimeLabels([vFullTime, vDBOpenTime, vOpenTime, vDBMoveTime, vDBGetDataTime, vConvertTime, vDBCloseTime, vSortTime, vDataBuildTime],
                [Slice.Cube.TimeStat.FullTime,
                 Slice.Cube.TimeStat.DBOpenTime,
                 Slice.Cube.TimeStat.OpenTime,
                 Slice.Cube.TimeStat.DBMoveTime,
                 Slice.Cube.TimeStat.DBGetDataTime,
                 Slice.Cube.TimeStat.ConvertTime,
                 Slice.Cube.TimeStat.DBCloseTime,
                 Slice.Cube.TimeStat.SortTime,
                 Slice.TimeStat.DataBuildTime]);
  ShowModal;
end;

procedure TfcxInfoForm.UpdateCaptions;
begin
  Caption := fcxResources.Get('sInfoFormCaption');
  FastCubeVer.Caption := 'FastCube ver. ' + FCX_VERSION;
  GroupBox1.Caption := fcxResources.Get('sInfoFormSliceGeometry');
  lSourceRowCount.Caption := fcxResources.Get('sInfoFormSourceRowCount');
  lRowDimCount.Caption    := fcxResources.Get('sInfoFormRowDimCount');
  lColDimCount.Caption    := fcxResources.Get('sInfoFormColDimCount');
  lFactsCount.Caption     := fcxResources.Get('sInfoFormFactsCount');
  lFilterCount.Caption    := fcxResources.Get('sInfoFormFilterCount');
  lRowCount.Caption       := fcxResources.Get('sInfoFormRowCount');
  lColCount.Caption       := fcxResources.Get('sInfoFormColCount');

  GroupBox2.Caption := fcxResources.Get('sInfoFormTimeChar');
  lFullTime.Caption      := fcxResources.Get('sInfoFormFullTime');
  lDBOpenTime.Caption    := fcxResources.Get('sInfoFormDBOpenTime');
  lOpenTime.Caption      := fcxResources.Get('sInfoFormOpenTime');
  lDBMoveTime.Caption    := fcxResources.Get('sInfoFormDBMoveTime');
  lDBGetDataTime.Caption := fcxResources.Get('sInfoFormDBGetDataTime');
  lConvertTime.Caption   := fcxResources.Get('sInfoFormConvertTime');
  lDBCloseTime.Caption   := fcxResources.Get('sInfoFormDBCloseTime');
  lSortTime.Caption      := fcxResources.Get('sInfoFormSortTime');
  lDataBuildTime.Caption := fcxResources.Get('sInfoFormDataBuildTime');
//  Label11.Caption        := fcxResources.Get('sInfoFormTimeLinks');
//  Label12.Caption        := fcxResources.Get('sInfoFormTimeUniques');
  Button1.Caption := fcxResources.Get('sOkBtn');
end;

procedure TfcxInfoForm.FormCreate(Sender: TObject);
begin
  UpdateCaptions;
end;

procedure TfcxInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
