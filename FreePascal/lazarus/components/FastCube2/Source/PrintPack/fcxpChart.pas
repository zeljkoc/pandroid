{******************************************}
{                                          }
{             FastReport v4.0              }
{       FastCube 2 TeeChart Add-In Object  }
{                                          }
{         Copyright (c) 1998-2014          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpChart;

interface

{$I fcx.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Menus, Controls,
  frxClass,
{$IFDEF DELPHI_16UP}
  VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series, VCLTee.TeCanvas,
{$ELSE}
  TeeProcs, TeEngine, Chart, Series, TeCanvas,
{$ENDIF}
  fcxpComponents, fcxpChartComponents, fcxCube, fcxSlice, fcxChart, frxChart, fcxRes, fcxAlerts
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
//  TfcxpChartObject = class(TComponent);  // fake component
  Tfrx2ChartGetSeriesClassEvent = type String;
  Tfrx2ChartSeriesCreatedEvent = type String;
  Tfrx2ChartFilledEvent = type String;

  TfcxpChartView = class(TfrxChartView)
  private
    FSlice: TfcxSlice;
    FCube: TfcxpCube;
    FExternalSlice: TfcxSlice;
    FLoadSchema: TMemoryStream;
    FEMF: TMetafile;
    FfcxpChartProviderName: String;
    FfcxpChartProvider: TfcxpChartProvider;
    FOnChartFilled: Tfrx2ChartFilledEvent; { script event }
    FOnGetSeriesClass: Tfrx2ChartGetSeriesClassEvent; { script event }
    FOnSeriesCreated: Tfrx2ChartSeriesCreatedEvent; { script event }

    // property writers and readers
    procedure ReadSchema(Stream: TStream);
    procedure WriteSchema(Stream: TStream);

    procedure ReadChartProp(Stream: TStream);
    procedure WriteChartProp(Stream: TStream);

    procedure ReadEmf(Stream: TStream);
    procedure WriteEmf(Stream: TStream);

    procedure SetCube(const Value: TfcxpCube);
    procedure SetSlice(const Value: TfcxSlice);
    function GetfcxpChartProviderName: String;
    procedure SetfcxpChartProvider(const Value: TfcxpChartProvider);
    procedure SetfcxpChartProviderName(const Value: String);
    function GetSlice: TfcxSlice;

    procedure GetSeriesClassEvent(Sender: TfcxCustomChart;
      ASeriesIndex: integer; ASeriesTitle: string; var ASeriesClass: TChartSeriesClass);
    procedure SeriesCreatedEvent(Sender: TfcxCustomChart; AChartSeries: TChartSeries);
    procedure FilledEvent(Sender: TfcxCustomChart);

  protected
    class function GetChartClass: TChartClass; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateChart; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    procedure GetData; override;
    procedure Loaded; override;
    function ExecuteAction(Action : TBasicAction) : Boolean; override;
    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;
    procedure AfterPrint; override;
    property Slice: TfcxSlice read GetSlice write SetSlice;
  published
    property fcxpChartProvider: TfcxpChartProvider read FfcxpChartProvider write SetfcxpChartProvider;
    property fcxpChartProviderName: String read GetfcxpChartProviderName write SetfcxpChartProviderName;
    property Cube: TfcxpCube read FCube write SetCube;

    property OnGetSeriesClass: Tfrx2ChartGetSeriesClassEvent read FOnGetSeriesClass write FOnGetSeriesClass; { script event }
    property OnSeriesCreated: Tfrx2ChartSeriesCreatedEvent read FOnSeriesCreated write FOnSeriesCreated; { script event }
    property OnChartFilled: Tfrx2ChartFilledEvent read FOnChartFilled write FOnChartFilled; { script event }
  end;


implementation

uses
{$IFNDEF NO_EDITORS}
  fcxpChartEditor,
{$ENDIF}
  frxDsgnIntf,
  frxUtils,
  frxRes,
  fcxGraphicRes;

type
  TfcxChartAccess = class(TfcxChart)
  end;

{ TfcxpChartView }

procedure TfcxpChartView.AfterPrint;
begin
  inherited;
  FEMF.Free;
  FEMF := nil;
end;

constructor TfcxpChartView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // this objects are for load sequence only
  FSlice := TfcxSlice.Create(Self);
  FSlice.ListnersManager.AddListner(Self);
  FLoadSchema := nil;
  TfcxChart(Chart).Slice := Slice;

  TfcxChart(Chart).OnGetSeriesClass := GetSeriesClassEvent;
  TfcxChart(Chart).OnChartFilled := FilledEvent;
  TfcxChart(Chart).OnSeriesCreated := SeriesCreatedEvent;

  FEMF := nil;
end;

destructor TfcxpChartView.Destroy;
begin
  FLoadSchema.Free;
  FSlice.ListnersManager.RemoveListner(Self);
  FSlice.Free;
  FEMF.Free;
  inherited Destroy;
end;

procedure TfcxpChartView.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX,
  OffsetY: Extended);
begin
  if FEMF <> nil then
  begin
    BeginDraw(Canvas, ScaleX, ScaleY, OffsetX, OffsetY);
    DrawBackground;
    Canvas.StretchDraw(Rect(FX, FY, FX1, FY1), FEMF);
    DrawFrame;
  end else
    inherited;
end;

function TfcxpChartView.ExecuteAction(Action: TBasicAction): Boolean;
begin
  If (Action is TfcxAction) and (TfcxAction(Action).ChangeAlert is TfcxSliceChangeAlert) then
  begin
    TfcxChart(Chart).Active := True;
    Result := True;
  end else
    Result := False;
end;

procedure TfcxpChartView.GetData;
var AStream: TMemoryStream;
begin
  inherited;
  // we can render cross only if it have any usable layout
  if (Slice.Cube <> nil) and Slice.HaveLayout then
  begin
    Report.SetProgressMessage(frxResources.Get('crBuildMx'));
    TfcxChart(Chart).Slice := Slice;

      if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) and (FfcxpChartProvider.Chart.Slice <> nil) then
      begin
        AStream := TMemoryStream.Create;
        FfcxpChartProvider.Chart.SaveToStream(AStream);
        AStream.Position := 0;
        TfcxChart(Chart).OnGetSeriesClass := GetSeriesClassEvent;
        TfcxChart(Chart).OnChartFilled := FilledEvent;
        TfcxChart(Chart).OnSeriesCreated := SeriesCreatedEvent;
        TfcxChart(Chart).LoadFromStream(AStream);
        AStream.Free;
      end;

    TfcxChart(Chart).Active := False;
    TfcxChart(Chart).Active := True;

    if Color = clTransparent then
      Chart.Color := clWhite else
      Chart.Color := Color;
    Chart.BufferedDisplay := True;
    FEMF := Chart.TeeCreateMetafile(False, Rect(0, 0, Round(Width), Round(Height)));
  end;
end;

class function TfcxpChartView.GetDescription: String;
begin
  Result := fcxResources.Get('obfrcChartView');
end;

procedure TfcxpChartView.Loaded;
begin
  inherited;
  if (FLoadSchema <> nil) and (FCube <> nil) and (FCube.Active) then
  begin
    FSlice.LoadFromStream(FLoadSchema);
    TfcxChart(Chart).Slice := Slice;
    FreeAndNil(FLoadSchema);
  end
  else
  if Slice <> nil then
    TfcxChart(Chart).Slice := Slice;
end;

procedure TfcxpChartView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Cube) then
    Cube := nil;
end;

class function TfcxpChartView.GetChartClass: TChartClass;
begin
  Result := TfcxChart;
end;

procedure TfcxpChartView.CreateChart;
begin
  inherited;
  if Slice <> nil then
    TfcxChart(Chart).Slice := Slice;

  TfcxChart(Chart).OnGetSeriesClass := GetSeriesClassEvent;
  TfcxChart(Chart).OnChartFilled := FilledEvent;
  TfcxChart(Chart).OnSeriesCreated := SeriesCreatedEvent;

end;

procedure TfcxpChartView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Schema', ReadSchema, WriteSchema, True);
  Filer.DefineBinaryProperty('ChartProp', ReadChartProp, WriteChartProp, True);
  Filer.DefineBinaryProperty('EMF', ReadEMF, WriteEMF, True);
end;

procedure TfcxpChartView.ReadEmf(Stream: TStream);
begin
  if Stream.Size <> 0 then
  begin
    if FEMF = nil then
      FEMF := TMetafile.Create;
    FEMF.LoadFromStream(Stream);
  end;
end;

procedure TfcxpChartView.ReadSchema(Stream: TStream);
begin
  if (Stream.Size > 0) then
  begin
    if FSlice.Cube <> nil then
    begin
      // if we already have cube, we can load slice
      FSlice.LoadFromStream(Stream);
    end else
    begin
      // else we should create temprorary stream and wait until we get cube
      // we just can't apply schema to nothing
      FLoadSchema := TMemoryStream.Create;
      FLoadSchema.LoadFromStream(Stream);
      FLoadSchema.Position := 0;
    end;
  end;
end;

procedure TfcxpChartView.WriteEmf(Stream: TStream);
begin
  if FEMF <> nil then
    FEMF.SaveToStream(Stream);
end;

procedure TfcxpChartView.WriteSchema(Stream: TStream);
begin
  if (FSlice.Cube <> nil) and (FSlice.Active) then
    FSlice.SaveToStream(Stream);
end;

procedure TfcxpChartView.SetCube(const Value: TfcxpCube);
begin
  FCube := Value;
  if FCube <> nil then
  begin
    TfcxChart(Chart).Slice := FSlice;

    TfcxChart(Chart).OnGetSeriesClass := GetSeriesClassEvent;
    TfcxChart(Chart).OnChartFilled := FilledEvent;
    TfcxChart(Chart).OnSeriesCreated := SeriesCreatedEvent;

    FSlice.Cube := FCube.Cube;
    fcxpChartProvider := nil;
    FCube.FreeNotification(Self);
  end
  else
    FSlice.Cube := nil;

  // we get cube, so we can apply schema now and forget about it holding in stream
  if (FLoadSchema <> nil) and (FCube <> nil) and (FCube.Active) then
  begin
    FSlice.LoadFromStream(FLoadSchema);
    FreeAndNil(FLoadSchema);
  end;
end;

procedure TfcxpChartView.SetSlice(const Value: TfcxSlice);
begin
  FExternalSlice := Value;
end;

procedure TfcxpChartView.ReadChartProp(Stream: TStream);
var AStream: TMemoryStream;
begin
  if (Stream.Size > 0) then
  begin
    if Chart <> nil then
    begin
      if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) and (FfcxpChartProvider.Chart.Slice <> nil) then
      begin
        AStream := TMemoryStream.Create;
        FfcxpChartProvider.Chart.SaveToStream(AStream);
        AStream.Position := 0;
        if FfcxpChartProvider.UseFCChartEvents then
        begin
          TfcxChart(Chart).OnGetSeriesClass := FfcxpChartProvider.Chart.OnGetSeriesClass;
          TfcxChart(Chart).OnSeriesCreated := FfcxpChartProvider.Chart.OnSeriesCreated;
          TfcxChart(Chart).OnChartFilled := FfcxpChartProvider.Chart.OnChartFilled;
        end;
        TfcxChart(Chart).LoadFromStream(AStream);
        AStream.Free;
      end
      else
        TfcxChart(Chart).LoadFromStream(Stream);
    end;
  end;
end;

procedure TfcxpChartView.WriteChartProp(Stream: TStream);
begin
  if Chart <> nil then
    if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) and (FfcxpChartProvider.Chart.Slice <> nil) then
      FfcxpChartProvider.Chart.SaveToStream(Stream)
    else
      TfcxChart(Chart).SaveToStream(Stream);
end;

function TfcxpChartView.GetfcxpChartProviderName: String;
begin
  if FfcxpChartProvider = nil then
    Result := FfcxpChartProviderName else
    Result := FfcxpChartProvider.UserName;
end;

procedure TfcxpChartView.SetfcxpChartProvider(const Value: TfcxpChartProvider);
begin
  FfcxpChartProvider := Value;
  if FfcxpChartProvider = nil then
  begin
    FfcxpChartProviderName := '';
    TfcxChart(Chart).Slice := FSlice;
  end
  else
  begin
    Cube := nil;
    FfcxpChartProviderName := FfcxpChartProvider.UserName;
    if (FfcxpChartProvider.Chart <> nil) and (FfcxpChartProvider.Chart.Slice <> nil) then
    begin
      TfcxChart(Chart).Slice := FfcxpChartProvider.Chart.Slice;
    end
  end
end;

procedure TfcxpChartView.SetfcxpChartProviderName(const Value: String);
begin
  FfcxpChartProviderName := Value;
  FfcxpChartProvider := fcxpFindfcxpChartProvider(FfcxpChartProviderName);
  if FfcxpChartProvider = nil then
  begin
    TfcxChart(Chart).Slice := FSlice;
  end
  else
  begin
    Cube := nil;
    if (FfcxpChartProvider.Chart <> nil) and (FfcxpChartProvider.Chart.Slice <> nil) then
    begin
      TfcxChart(Chart).Slice := FfcxpChartProvider.Chart.Slice;
    end
  end
end;

function TfcxpChartView.GetSlice: TfcxSlice;
begin
  if FExternalSlice <> nil then
  begin
    Result := FExternalSlice;
  end
  else
  if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) and (FfcxpChartProvider.Chart.Slice <> nil) then
  begin
    Result := FfcxpChartProvider.Chart.Slice;
  end
  else
    Result := FSlice;
end;

procedure TfcxpChartView.FilledEvent(Sender: TfcxCustomChart);
var
  v: Variant;
begin
  if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) then
    if FfcxpChartProvider.UseFCChartEvents then
      if Assigned(FfcxpChartProvider.Chart.OnChartFilled) then
        FfcxpChartProvider.Chart.OnChartFilled(Sender);
  if FOnChartFilled <> '' then
  begin
    v := null;
    v := VarArrayOf([frxInteger(Sender)]);
    if Report <> nil then
      Report.DoParamEvent(FOnChartFilled, v);
  end;
end;

procedure TfcxpChartView.GetSeriesClassEvent(Sender: TfcxCustomChart;
  ASeriesIndex: integer; ASeriesTitle: string;
  var ASeriesClass: TChartSeriesClass);
var
  v: Variant;
  ASeriesType: TfcxSeriesType;
begin
  if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) then
    if FfcxpChartProvider.UseFCChartEvents then
      if Assigned(FfcxpChartProvider.Chart.OnGetSeriesClass) then
        FfcxpChartProvider.Chart.OnGetSeriesClass(Sender, ASeriesIndex, ASeriesTitle, ASeriesClass);
  if FOnGetSeriesClass <> '' then
  begin
    v := null;
    ASeriesType := fcxGetSeriesTypeBySeriesClass(ASeriesClass);
    v := VarArrayOf([frxInteger(Sender), ASeriesIndex, ASeriesTitle, ASeriesType]);
    if Report <> nil then
    begin
      Report.DoParamEvent(FOnGetSeriesClass, v);
      ASeriesType := v[3];
      ASeriesClass := fcxChart.ChartSeries[ord(ASeriesType)];
    end
  end;
end;

procedure TfcxpChartView.SeriesCreatedEvent(Sender: TfcxCustomChart;
  AChartSeries: TChartSeries);
var
  v: Variant;
begin
  if (FfcxpChartProvider <> nil) and (FfcxpChartProvider.Chart <> nil) then
    if FfcxpChartProvider.UseFCChartEvents then
      if Assigned(FfcxpChartProvider.Chart.OnSeriesCreated) then
        FfcxpChartProvider.Chart.OnSeriesCreated(Sender, AChartSeries);
  if FOnSeriesCreated <> '' then
  begin
    v := null;
    v := VarArrayOf([frxInteger(Sender), frxInteger(AChartSeries)]);
    if Report <> nil then
      Report.DoParamEvent(FOnSeriesCreated, v);
  end;
end;

initialization
  frxObjects.RegisterObject1(TfcxpChartView, fcxGraphicResources.GetFRBitmap(3), '', '', 0, -1);
  frxHideProperties(TfcxpChartView, 'fcxpChartProviderName');
  frxResources.Add('Tfrx2ChartGetSeriesClassEvent',
    'PascalScript=(Sender: TfcxCustomChart; ASeriesIndex: integer; ASeriesTitle: string; var ASeriesType: TfcxSeriesType);' + #13#10 +
    'C++Script=(TfcxCustomChart Sender, integer ASeriesIndex, string ASeriesTitle, TfcxSeriesType ASeriesType)' + #13#10 +
    'BasicScript=(Sender, ASeriesIndex, ASeriesTitle, ASeriesType)' + #13#10 +
    'JScript=(Sender, ASeriesIndex, ASeriesTitle, ASeriesType)');
  frxResources.Add('Tfrx2ChartSeriesCreatedEvent',
    'PascalScript=(Sender: TfcxCustomChart; AChartSeries: TChartSeries);' + #13#10 +
    'C++Script=(TfcxCustomChart Sender, TChartSeries AChartSeries)' + #13#10 +
    'BasicScript=(Sender, AChartSeries)' + #13#10 +
    'JScript=(Sender, AChartSeries)');
  frxResources.Add('Tfrx2ChartFilledEvent',
    'PascalScript=(Sender: TfcxCustomChart);' + #13#10 +
    'C++Script=(TfcxCustomChart Sender)' + #13#10 +
    'BasicScript=(Sender)' + #13#10 +
    'JScript=(Sender)');

finalization
  frxObjects.UnRegister(TfcxpChartView);

end.
