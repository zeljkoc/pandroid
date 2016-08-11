{******************************************}
{                                          }
{            FastReport v4.0               }
{         FastCube 2 Chart editor          }
{                                          }
{         Copyright (c) 1998-2014          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpChartEditor;

interface

{$I fcx.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ImgList, Menus, ComCtrls, Buttons, ToolWin, ExtCtrls, 
  frxDock, frxClass, frxCtrls, frxCustomEditors, 
  fcxpChart, fcxSliceGrid, fcxTypes, fcxCube,  fcxSlice, fcxChart, fcxSliceGridToolBar, 
  TeeProcs, TeEngine, Chart, fcxCustomToolbar, fcxComponent, fcxpChartComponents, fcxZone, fcxCustomGrid
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  TfcxpChartEditor = class(TfrxViewEditor)
  public
    function Edit: Boolean; override;
    function HasEditor: Boolean; override;
  end;

  TfcxpChartEditorForm = class(TForm)
    OkB: TButton;
    CancelB: TButton;
    CubeCB: TComboBox;
    CubeL: TLabel;
    PageControl1: TPageControl;
    StructureSheet: TTabSheet;
    fcxSliceGrid1: TfcxSliceGrid;
    froToolBar1: TfcxSliceGridToolBar;
    ChartSheet: TTabSheet;
    fcxChart1: TfcxChart;
    fcxChartToolBar1: TfcxChartToolBar;
    ChartL: TLabel;
    ChartCB: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CancelBClick(Sender: TObject);
    procedure ChartCBClick(Sender: TObject);
    procedure ChartCBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
        State: TOwnerDrawState);
    procedure OkBClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure CubeCBClick(Sender: TObject);
    procedure CubeCBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LBDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FChart: TfcxpChartView;
    FOldSlice: TMemoryStream;
    FOldChart: TMemoryStream;
    FImages: TImageList;
    procedure SetChart(const Value: TfcxpChartView);
    procedure FillGridItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Chart: TfcxpChartView read FChart write SetChart;
  end;


implementation

{$R *.DFM}

uses
  frxDsgnIntf,
  frxEditFormat,
  frxEditHighlight,
  frxEditMemo,
  frxEditFrame,
  frxDesgnCtrls,
  frxRes,
  frxUtils,
  fcxRes,
  fcxGraphicRes,
  fcxpComponents;

type
  THackSlice = class(TfcxSlice);
  
{ TfrxCrossEditor }

function TfcxpChartEditor.Edit: Boolean;
begin
  with TfcxpChartEditorForm.Create(Designer) do
  begin
//    TfcxChart(TfcxpChartView(Component).Chart).Parent := TabSheet1;
    try
//      TfcxChart(TfcxpChartView(Component).Chart).Align := alClient;
      Chart := TfcxpChartView(Component);
      Result := ShowModal = mrOk;
    finally
//      TfcxChart(Chart.Chart).Parent := nil;
    end;
    Free;
  end;
end;

function TfcxpChartEditor.HasEditor: Boolean;
begin
  Result := True;
end;


{ TfrxCrossEditorForm }

constructor TfcxpChartEditorForm.Create(AOwner: TComponent);
begin
  inherited;
  FImages := TImageList.Create(nil);
  FOldSlice := nil;
  FOldChart := nil;
end;

destructor TfcxpChartEditorForm.Destroy;
begin
  FImages.Free;
  FOldSlice.Free;
  FOldChart.Free;
  inherited;
end;

procedure TfcxpChartEditorForm.FillGridItems;
var
  I: Integer;
  Field: TfcxSliceField;
begin
  for I := 0 to fcxSliceGrid1.Slice.SliceFieldCount - 1 do
  begin
    Field := fcxSliceGrid1.Slice.SliceField[I];
    if Field.DataType in fcxNumericTypes then // add to measures
      fcxSliceGrid1.Slice.MeasuresContainer.AddMeasure(Field, Field.FieldName, Field.Caption, af_Sum)
    else // add to page
      THackSlice(fcxSliceGrid1.Slice).PageContainer.AddFilterField(Field);
  end;
end;

procedure TfcxpChartEditorForm.FormCreate(Sender: TObject);
begin
  Caption := fcxGet(4300);
  CubeL.Caption := 'Cube';//fcGet(4301);
  StructureSheet.Caption := fcxGet(4306);
  ChartSheet.Caption := fcxGet(4100);
  OkB.Caption := frxGet(1);
  CancelB.Caption := frxGet(2);
end;

procedure TfcxpChartEditorForm.FormShow(Sender: TObject);

  procedure SelectCube;
  begin
    CubeCB.ItemIndex := CubeCB.Items.IndexOfObject(FChart.Cube);
    if CubeCB.ItemIndex = -1 then
      CubeCB.ItemIndex := 0;
    CubeCBClick(nil);
  end;
  procedure SelectChart;
  begin
    ChartCB.ItemIndex := ChartCB.Items.IndexOfObject(FChart.fcxpChartProvider);
    ChartCBClick(nil);
  end;

begin
  fcxpGetfcxpCubeList(FChart.Report, CubeCB.Items);
  fcxpGetfcxpChartProviderList(ChartCB.Items);
  if FChart.fcxpChartProvider <> nil then
    SelectChart
  else
    SelectCube;
  CubeL.Visible := True;
end;

procedure TfcxpChartEditorForm.FormHide(Sender: TObject);
begin
  if ModalResult = mrCancel then
  begin
    FOldSlice.Position := 0;
    FChart.Slice.LoadFromStream(FOldSlice);
    FOldChart.Position := 0;
    TfcxChart(FChart.Chart).LoadFromStream(FOldChart);
  end;
end;

procedure TfcxpChartEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    frxResources.Help(Self);
end;

procedure TfcxpChartEditorForm.CubeCBClick(Sender: TObject);
var
  cube: TfcxpCube;
  AIsfcxpChartProvider: boolean;
begin
  if CubeCB.ItemIndex = -1 then Exit;
  if ChartCB.ItemIndex <> -1 then
  begin
    AIsfcxpChartProvider := True;
    ChartCB.ItemIndex := -1;
    ChartCBClick(Self);
  end
  else
    AIsfcxpChartProvider := False;
  cube := TfcxpCube(CubeCB.Items.Objects[CubeCB.ItemIndex]);
  Chart.Cube := cube;
  if not AIsfcxpChartProvider then
    if Assigned(fcxSliceGrid1.Slice) and Assigned(fcxSliceGrid1.Slice.Cube) then
      fcxSliceGrid1.Slice.Cube.Active := False;
  fcxSliceGrid1.Slice := Chart.Slice;
  if Assigned(fcxSliceGrid1.Slice) and Assigned(fcxSliceGrid1.Slice.Cube) then
    fcxSliceGrid1.Slice.Cube.Active := True;
  fcxChart1.Slice := fcxSliceGrid1.Slice;
  fcxChart1.Active := True;
  if FOldSlice <> nil then
  begin
    FOldSlice.Position := 0;
    fcxSliceGrid1.Slice.LoadFromStream(FOldSlice);
  end;
  if FOldChart <> nil then
  begin
    FOldChart.Position := 0;
    fcxChart1.LoadFromStream(FOldChart);
  end;
  if Assigned(fcxSliceGrid1.Slice) and fcxSliceGrid1.Slice.Active and not fcxSliceGrid1.Slice.HaveLayout then
    FillGridItems;
end;

procedure TfcxpChartEditorForm.LBDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListBox) and (TListBox(Source).Items.Count > 0);
end;

procedure TfcxpChartEditorForm.CancelBClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfcxpChartEditorForm.ChartCBClick(Sender: TObject);
var
  fcxpChartProvider: TfcxpChartProvider;
begin
  if ChartCB.ItemIndex = -1 then Exit;
  if CubeCB.ItemIndex <> -1 then
  begin
    CubeCB.ItemIndex := -1;
    CubeCBClick(Self);
  end;
  fcxpChartProvider := TfcxpChartProvider(ChartCB.Items.Objects[ChartCB.ItemIndex]);
  Chart.fcxpChartProvider := fcxpChartProvider;
  if Assigned(fcxSliceGrid1.Slice) and Assigned(fcxSliceGrid1.Slice.Cube) then
    fcxSliceGrid1.Slice.Cube.Active := False;
  fcxSliceGrid1.Slice := Chart.Slice;
  if Assigned(fcxSliceGrid1.Slice) and Assigned(fcxSliceGrid1.Slice.Cube) then
    fcxSliceGrid1.Slice.Cube.Active := True;
  fcxChart1.Slice := fcxSliceGrid1.Slice;
  fcxChart1.Active := True;
  FreeAndNil(FOldChart);
  FOldChart := TMemoryStream.Create;
  TfcxChart(Chart.Chart).SaveToStream(FOldChart);
  FOldChart.Position := 0;
  if fcxpChartProvider.UseFCChartEvents then
  begin
    fcxChart1.OnGetSeriesClass := TfcxChart(Chart.Chart).OnGetSeriesClass;
    fcxChart1.OnSeriesCreated := TfcxChart(Chart.Chart).OnSeriesCreated;
    fcxChart1.OnChartFilled := TfcxChart(Chart.Chart).OnChartFilled;
  end;
  fcxChart1.LoadFromStream(FOldChart);

  if not fcxSliceGrid1.Slice.HaveLayout then
    FillGridItems;
end;

procedure TfcxpChartEditorForm.ChartCBDrawItem(Control: TWinControl; Index:
    Integer; ARect: TRect; State: TOwnerDrawState);
begin
  ChartCB.Canvas.FillRect(ARect);
  fcxGraphicResources.FRImages.Draw(ChartCB.Canvas, ARect.Left, ARect.Top, 1);
  ChartCB.Canvas.TextOut(ARect.Left + 20, ARect.Top + 1, ChartCB.Items[Index]);
end;

procedure TfcxpChartEditorForm.OkBClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfcxpChartEditorForm.SetChart(const Value: TfcxpChartView);
begin
  FChart := Value;
  if FChart <> nil then
  begin
    fcxSliceGrid1.Slice := FChart.Slice;
    fcxChart1.Slice := fcxSliceGrid1.Slice;
    FreeAndNil(FOldSlice);
    FOldSlice := TMemoryStream.Create;
    FChart.Slice.SaveToStream(FOldSlice);
    FOldSlice.Position := 0;
    FreeAndNil(FOldChart);
    FOldChart := TMemoryStream.Create;
    TfcxChart(Chart.Chart).SaveToStream(FOldChart);
    FOldChart.Position := 0;
    fcxChart1.OnGetSeriesClass := TfcxChart(Chart.Chart).OnGetSeriesClass;
    fcxChart1.OnSeriesCreated := TfcxChart(Chart.Chart).OnSeriesCreated;
    fcxChart1.OnChartFilled := TfcxChart(Chart.Chart).OnChartFilled;
    fcxChart1.LoadFromStream(FOldChart);
//    FOldChart.Free;
  end else
  begin
    fcxChart1.Slice := nil;
    fcxSliceGrid1.Slice := nil;
    FreeAndNil(FOldSlice);
    FreeAndNil(FOldChart);
  end;
end;

procedure TfcxpChartEditorForm.CubeCBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  CubeCB.Canvas.FillRect(ARect);
  fcxGraphicResources.FRImages.Draw(CubeCB.Canvas, ARect.Left, ARect.Top, 1);
  CubeCB.Canvas.TextOut(ARect.Left + 20, ARect.Top + 1, CubeCB.Items[Index]);
end;

procedure TfcxpChartEditorForm.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  if ModalResult = mrOk then
  begin
    FreeAndNil(FOldChart);
    FOldChart := TMemoryStream.Create;
    fcxChart1.SaveToStream(FOldChart);
    FOldChart.Position := 0;
    TfcxChart(Chart.Chart).LoadFromStream(FOldChart);
    FreeAndNil(FOldChart);
  end;
end;

initialization
  frxComponentEditors.Register(TfcxpChartView, TfcxpChartEditor);

end.
