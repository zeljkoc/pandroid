{******************************************}
{                                          }
{             FastReport v4.0              }
{       FastCube 2 enduser components      }
{                                          }
{          Copyright (c) 2001-2014         }
{       by Oleg Pryalkov, Paul Ishenin,    }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxpComponents;
{$I frx.inc}
{$I fcx.inc}
interface

{$IFDEF DELPHI_12UP}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}
uses
  SysUtils, Classes, Dialogs, frxClass, frxDBSet, fcxDataSource,
  fcxCube, fcxSlice, fcxCustomGrid, fcxSliceGrid, fcxpCrossRTTI, fcxTypes
{$IFNDEF NO_CRITICAL_SECTION}
,  SyncObjs
{$ENDIF}
{$IFDEF Delphi6}
, Variants
{$ENDIF};
{$ELSE FMX}
{$INCLUDE fcx.inc}
{$INCLUDE frx.inc}
interface
uses
  // RTL
  System.Types, System.Classes, System.SysUtils, System.Variants, System.UITypes,
{$IFNDEF NO_CRITICAL_SECTION}
  System.SyncObjs,
{$ENDIF}
  // FMX
  FMX.Dialogs,
  // FC FMX
  FMX.fcxCube, FMX.fcxSliceGrid, FMX.fcxDataSource,
  // FR FMX
  FMX.frxClass, FMX.frxDBSet;
{$ENDIF}

type
  TfcxpCrossAutoSizeStyle = (
    ssDefault,              // use internal row/col sizes
    ssBySlice,              // use sizes from slice
    ssAutoColWidth,         // auto calc column width
    ssAutoColWidthRestrict, // auto calc column width no greater then ...
    ssAutoRowHeight,        // auto calc row height
    ssByMemoSize            // use sized of nested memos
  );

  TfcxpCrossPaintSizes = class(TPersistent)
  protected
    FCross: TfrxView;
    FAutoSizeStyle: TfcxpCrossAutoSizeStyle;
    FMaxColWidth: Integer;
    FDefaultRowHeight: integer;
    FDefaultColWidth: integer;
    procedure SetAutoSizeStyle(const Value: TfcxpCrossAutoSizeStyle);
    procedure SetDefaultColWidth(const Value: integer);
    procedure SetDefaultRowHeight(const Value: integer);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TfrxView);
  published
    property AutoSizeStyle: TfcxpCrossAutoSizeStyle read FAutoSizeStyle write SetAutoSizeStyle default ssDefault;
    property MaxColWidth: Integer read FMaxColWidth write FMaxColWidth default 80;
    property DefaultRowHeight: integer read FDefaultRowHeight write SetDefaultRowHeight default 18;
    property DefaultColWidth: integer read FDefaultColWidth write SetDefaultColWidth default 80;
  end;

  TfcxpComponents = class(TComponent)
  private
    FOldComponents: TfcxpComponents;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String;
  published
    property Version: String read GetVersion write SetVersion;
  end;

  TfcxpCube = class(TfrxDialogComponent)
  private
    FRunScriptInDesigner: Boolean;
    FDataSource: TfcxDataSource;
    FDBDataSet: TfcxDBDataSet;
    function GetCube: TfcxCube;
    function GetFileName: String;
    procedure SetFileName(const Value: String);
    procedure SetRunScriptInDesigner(const Value: Boolean);
    function GetCubeSource: TfcxCubeSource;
    procedure SetCubeSource(const Value: TfcxCubeSource);
  protected
    FDataset: TfrxDataset;
    FCube: TfcxCube;
    FExternalCube: TfcxCube;
    FLoadActive: Boolean;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetCube(const Value: TfcxCube);
    procedure SetDataset(const Value: TfrxDataset);
    function GetDataset: TfrxDataset;
  public
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    property Cube: TfcxCube read GetCube write SetCube;
  published
    property Dataset: TfrxDataset read GetDataset write SetDataset;
    property FileName: String read GetFileName write SetFileName;
    property Active: Boolean read GetActive write SetActive default False;
    property RunScriptInDesigner: Boolean read FRunScriptInDesigner write SetRunScriptInDesigner default False;
    property CubeSource: TfcxCubeSource read GetCubeSource write SetCubeSource default fccs_None;
  end;

  TfcxpSliceGridProvider = class;

  TfcxpGetStyles = procedure(Sender: TfcxpSliceGridProvider; var AStyles: TfrxStyles) of object;

  {$ifdef FPC}
  // FPC has Internal error 200610054 when setting it to TfcxSliceGrid
  TfcxSliceGridType = TfcxCustomGrid;
  {$else}
  TfcxSliceGridType = TfcxSliceGrid;
  {$endif}

  TfcxpSliceGridProvider = class(TfrxDialogComponent)
  private
    FEnabled: Boolean;
    FReportRef: TfrxReport;
    FUserName: String;
    FSliceGrid: TfcxSliceGridType;
    FPaintSizes: TfcxpCrossPaintSizes;
    FUseReportPaintSizes: Boolean;
    FonGetStyles: TfcxpGetStyles;
    procedure SetSliceGrid(const Value: TfcxSliceGridType);
    procedure SetPaintSizes(const Value: TfcxpCrossPaintSizes);
    function GetStyles: TfrxStyles;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    procedure SetName(const NewName: TComponentName); override;
    procedure SetUserName(const Value: String); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ReportRef: TfrxReport read FReportRef write FReportRef;
    property Styles: TfrxStyles read GetStyles;
  published
    property Version: String read GetVersion write SetVersion;
    property SliceGrid: TfcxSliceGridType read FSliceGrid write SetSliceGrid;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property UseReportPaintSizes: Boolean read FUseReportPaintSizes write FUseReportPaintSizes default False;
    property UserName: String read FUserName write SetUserName;
    property PaintSizes: TfcxpCrossPaintSizes read FPaintSizes write SetPaintSizes;
    property onGetStyles: TfcxpGetStyles read FonGetStyles write FonGetStyles;
  end;

  TfcxpGlobalProviderList = class(TList)
{$IFNDEF NO_CRITICAL_SECTION}
    FCriticalSection: TCriticalSection;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

var
  FCComponents: TfcxpComponents;

  procedure fcxpGetfcxpCubeList(Report: TfrxReport; List: TStrings);
  function fcxpFindSliceGridProvider(const Name: String): TfcxpSliceGridProvider;
  procedure fcxpGetSliceGridProviderList(List: TStrings);

implementation

{$IFNDEF FMX}
uses
  fcxpCross,
  frxDsgnIntf,
  frxRes,
  fcxRes,
  fcxGraphicRes;
{$ELSE}
uses
  FMX.fcxpCross,
  FMX.frxDsgnIntf,
  FMX.frxRes,
  FMX.fcxRes,
  FMX.fcxGraphicRes;
{$ENDIF}

var
  SliceGridProviderList: TfcxpGlobalProviderList;

type
  TfcxpCubeProperty = class(TfrxPropertyEditor)
  public
    function GetValue: String; override;
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
  end;

  TfcxpSliceGridProviderProperty = class(TfrxPropertyEditor)
  public
    function GetValue: String; override;
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
  end;

  TfcxpHackCrossView = class(TfcxpCrossView);

procedure fcxpGetfcxpCubeList(Report: TfrxReport; List: TStrings);
var
  i: Integer;
  Page: TfrxPage;
begin
  List.Clear;
  Page := Report.Pages[0]; // Data page
  for I := 0 to Page.Objects.Count - 1 do
    if TObject(Page.Objects[i]) is TfcxpCube then
      List.AddObject(TfcxpCube(Page.Objects[i]).Name, Page.Objects[i]);
end;

function fcxpFindSliceGridProvider(const Name: String): TfcxpSliceGridProvider;
var
  i: Integer;
  ds: TfcxpSliceGridProvider;
begin
  Result := nil;
  if Name = '' then
    Exit;
  if SliceGridProviderList = nil then
    Exit;
  SliceGridProviderList.Lock;
  for i := 0 to SliceGridProviderList.Count - 1 do
  begin
    ds := SliceGridProviderList[i];
    if AnsiCompareText(ds.UserName, Name) = 0 then
    begin
      Result := SliceGridProviderList[i];
      break;
    end;
  end;
  SliceGridProviderList.Unlock;
end;

procedure fcxpGetSliceGridProviderList(List: TStrings);
var
  i: Integer;
  ds: TfcxpSliceGridProvider;
begin
  if SliceGridProviderList = nil then
    Exit;
  SliceGridProviderList.Lock;
  List.Clear;
  for i := 0 to SliceGridProviderList.Count - 1 do
  begin
    ds := SliceGridProviderList[i];
    if (ds <> nil) and (ds.UserName <> '') and ds.Enabled then
      List.AddObject(ds.UserName, ds);
  end;
  SliceGridProviderList.Unlock;
end;

{ TfcxpCrossPaintSizes }

procedure TfcxpCrossPaintSizes.AssignTo(Dest: TPersistent);
var
  DestSizes: TfcxpCrossPaintSizes absolute Dest;
begin
  if Dest is TfcxpCrossPaintSizes then
  begin
    DestSizes.AutoSizeStyle := AutoSizeStyle;
    DestSizes.MaxColWidth := MaxColWidth;
    DestSizes.DefaultRowHeight := DefaultRowHeight;
    DestSizes.DefaultColWidth := DefaultColWidth;
  end else
    inherited;
end;

constructor TfcxpCrossPaintSizes.Create(AOwner: TfrxView);
begin
  FDefaultColWidth := 80;
  FDefaultRowHeight := 18;
  FMaxColWidth := DefaultColWidth;
  FAutoSizeStyle := ssDefault;

  FCross := AOwner;
end;

procedure TfcxpCrossPaintSizes.SetAutoSizeStyle(const Value: TfcxpCrossAutoSizeStyle);
begin
  FAutoSizeStyle := Value;

  // and auto apply to visual object
  if (FCross <> nil) and (AutoSizeStyle <> ssByMemoSize) then
  begin
    TfcxpHackCrossView(FCross).SetMemosPos;
    TfcxpHackCrossView(FCross).SetMemosBounds;
  end;
end;

procedure TfcxpCrossPaintSizes.SetDefaultColWidth(const Value: integer);
begin
  FDefaultColWidth := Value;

  // and auto apply to visual object
  if (FCross <> nil) and (AutoSizeStyle <> ssByMemoSize) then
  begin
    TfcxpHackCrossView(FCross).SetMemosPos;
    TfcxpHackCrossView(FCross).SetMemosBounds;
  end;
end;

procedure TfcxpCrossPaintSizes.SetDefaultRowHeight(const Value: integer);
begin
  FDefaultRowHeight := Value;

  // and auto apply to visual object
  if (FCross <> nil) and (AutoSizeStyle <> ssByMemoSize) then
  begin
    TfcxpHackCrossView(FCross).SetMemosPos;
    TfcxpHackCrossView(FCross).SetMemosBounds;
  end;
end;

{ TfcxpComponents }

constructor TfcxpComponents.Create(AOwner: TComponent);
begin
  inherited;
  FOldComponents := FCComponents;
  FCComponents := Self;
end;

destructor TfcxpComponents.Destroy;
begin
  if FCComponents = Self then
    FCComponents := FOldComponents;
  inherited;
end;

function TfcxpComponents.GetDescription: String;
begin
  Result := 'FastCube 2';
end;

function TfcxpComponents.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxpComponents.SetVersion(const Value: String);
begin
//
end;

{ TfcxpCube }

constructor TfcxpCube.Create(AOwner: TComponent);
begin
  inherited;
  FRunScriptInDesigner := False;
  FLoadActive := False;
  FExternalCube := nil;
  FCube := TfcxCube.Create(Self);
  FDataSource := TfcxDataSource.Create(Self);
  FDBDataSet := TfcxDBDataSet.Create(Self);
  FDataSource.DataSet := FDBDataSet;
  FCube.DataSource := FDataSource;
  Component := FCube;
end;

function TfcxpCube.GetDataset: TfrxDataset;
begin
  Result := FDataSet
end;

class function TfcxpCube.GetDescription: String;
begin
  Result := fcxResources.Get('obfrcCube');
end;

function TfcxpCube.GetFileName: String;
begin
  Result := FCube.CubeFile;
end;

destructor TfcxpCube.Destroy;
begin
  inherited;
end;

function TfcxpCube.GetActive: Boolean;
begin
  if FExternalCube <> nil then
    Result := FExternalCube.Active else
    Result := FCube.Active;
end;

function TfcxpCube.GetCube: TfcxCube;
begin
  if FExternalCube <> nil then
    Result := FExternalCube else
    Result := FCube;
end;

function TfcxpCube.GetCubeSource: TfcxCubeSource;
begin
  Result := FCube.CubeSource;
end;

procedure TfcxpCube.SetActive(const Value: Boolean);
begin
  if FLoadActive <> Value then
  begin
    FLoadActive := Value;
    if FRunScriptInDesigner and Value and (Report <> nil) then
      Report.PrepareScript;
    if ((FCube.CubeSource = fccs_DataSource) and (TfcxDBDataSet(FCube.DataSource.DataSet).DataSet <> nil)) or ((FCube.CubeSource = fccs_CubeFile) and (FileName <> '')) then
      FCube.Active := Value;
  end
end;

procedure TfcxpCube.SetCube(const Value: TfcxCube);
begin
  FExternalCube := Value;
end;

procedure TfcxpCube.SetCubeSource(const Value: TfcxCubeSource);
begin
  FCube.CubeSource := Value;
end;

procedure TfcxpCube.SetDataset(const Value: TfrxDataset);
begin
  if Value is TfrxDBDataset then
  begin
    FDataset := Value;
    FDBDataSet.DataSet := TfrxDBDataset(Value).DataSet;
    FDataSource.DeleteFields;
    FCube.CubeSource := fccs_DataSource;
//    FCube.Active := FLoadActive;
  end
  else
  if Value = Nil then
  begin
    FDataset := Value;
    FDataSource.DataSet := Nil;
    FDataSource.DeleteFields;
    FCube.CubeSource := fccs_None;
  end
  else
    MessageDlg('Can not assign non TfrxDBDataset to Cube!', {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtError, [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk], 0);
end;

procedure TfcxpCube.SetFileName(const Value: String);
begin
  FCube.CubeFile := Value;
  FCube.CubeSource := fccs_CubeFile;
end;

procedure TfcxpCube.Loaded;
begin
  inherited;
  FCube.Active := FLoadActive;
end;

procedure TfcxpCube.SetRunScriptInDesigner(const Value: Boolean);
begin
  FRunScriptInDesigner := Value;
  if Value then
    if Active and (Report <> nil) then
      Report.PrepareScript;
end;

{ TfcxpCubeProperty }

function TfcxpCubeProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

function TfcxpCubeProperty.GetValue: String;
var
  cube: TfcxpCube;
begin
  cube := TfcxpCube(GetOrdValue);
  if (cube <> nil) and (frComponent.Report <> nil) then
    Result := cube.Name else
    Result := frxResources.Get('prNotAssigned');
end;

procedure TfcxpCubeProperty.GetValues;
begin
  fcxpGetfcxpCubeList(frComponent.Report, Values);
end;

procedure TfcxpCubeProperty.SetValue(const Value: String);
var
  cube: TfcxpCube;
begin
  if Value = '' then
    SetOrdValue(0)
  else
  begin
    cube := TfcxpCube(frComponent.Report.FindObject(Value));
    if cube <> nil then
      SetOrdValue(frxInteger(cube)) else
      raise Exception.Create(frxResources.Get('prInvProp'));
  end;
end;

{ TfcxpSliceGridProvider }

constructor TfcxpSliceGridProvider.Create(AOwner: TComponent);
begin
  inherited;
  FPaintSizes := TfcxpCrossPaintSizes.Create(nil);
  FUseReportPaintSizes := False;
  Enabled := true;
  if SliceGridProviderList = nil then
    Exit;
  SliceGridProviderList.Lock;
  SliceGridProviderList.Add(Self);
  SliceGridProviderList.Unlock;
end;

destructor TfcxpSliceGridProvider.Destroy;
begin
  if SliceGridProviderList = nil then
  begin
    inherited;
    SliceGridProviderList.Unlock;
    FPaintSizes.Free;
  end
  else
  begin
    SliceGridProviderList.Lock;
    SliceGridProviderList.Remove(Self);
    inherited;
    SliceGridProviderList.Unlock;
    FPaintSizes.Free;
  end;
end;

procedure TfcxpSliceGridProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FSliceGrid then
      SliceGrid := nil
end;

procedure TfcxpSliceGridProvider.SetName(const NewName: TComponentName);
begin
  if NewName <> '' then
    if (FUserName = '') or (FUserName = Name) then
      UserName := NewName;
  inherited;
end;

procedure TfcxpSliceGridProvider.SetPaintSizes(const Value: TfcxpCrossPaintSizes);
begin
  FPaintSizes.Assign(Value);
end;

procedure TfcxpSliceGridProvider.SetSliceGrid(const Value: TfcxSliceGridType);
begin
  FSliceGrid := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TfcxpSliceGridProvider.SetUserName(const Value: String);
begin
  if Trim(Value) = '' then
    raise Exception.Create(frxResources.Get('prInvProp'));
  FUserName := Value;
end;

function TfcxpSliceGridProvider.GetStyles: TfrxStyles;
begin
  if Assigned(FonGetStyles) then
    FonGetStyles(Self, Result)
  else
    Result := nil;
end;

function TfcxpSliceGridProvider.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxpSliceGridProvider.SetVersion(const Value: String);
begin
//
end;

{ TfcxpGlobalProviderList }

constructor TfcxpGlobalProviderList.Create;
begin
{$IFNDEF NO_CRITICAL_SECTION}
  FCriticalSection := TCriticalSection.Create;
{$ENDIF}
  inherited;
end;

destructor TfcxpGlobalProviderList.Destroy;
begin
{$IFNDEF NO_CRITICAL_SECTION}
  FCriticalSection.Free;
  FCriticalSection := nil;
{$ENDIF}
  inherited;
end;

procedure TfcxpGlobalProviderList.Lock;
begin
{$IFNDEF NO_CRITICAL_SECTION}
  if FCriticalSection <> nil then
    FCriticalSection.Enter;
{$ENDIF}
end;

procedure TfcxpGlobalProviderList.Unlock;
begin
{$IFNDEF NO_CRITICAL_SECTION}
  if FCriticalSection <> nil then
    FCriticalSection.Leave;
{$ENDIF}
end;

{ TfcxpSliceGridProviderProperty }

function TfcxpSliceGridProviderProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

function TfcxpSliceGridProviderProperty.GetValue: String;
var
  SliceGridProvider: TfcxpSliceGridProvider;
begin
  SliceGridProvider := TfcxpSliceGridProvider(GetOrdValue);
  if (SliceGridProvider <> nil) and (frComponent.Report <> nil) then
    Result := SliceGridProvider.UserName
  else
    Result := frxResources.Get('prNotAssigned');
end;

procedure TfcxpSliceGridProviderProperty.GetValues;
begin
  fcxpGetSliceGridProviderList(Values);
end;

procedure TfcxpSliceGridProviderProperty.SetValue(const Value: String);
var
  SliceGridProvider: TfcxpSliceGridProvider;
begin
  if Value = '' then
    SetOrdValue(0)
  else
  begin
    SliceGridProvider := fcxpFindSliceGridProvider(Value);
    if SliceGridProvider <> nil then
      SetOrdValue(frxInteger(SliceGridProvider)) else
      raise Exception.Create(frxResources.Get('prInvProp'));
  end;
end;

initialization
  SliceGridProviderList := TfcxpGlobalProviderList.Create;
  frxObjects.RegisterCategory('FastCube 2', fcxGraphicResources.GetFRBitmap(0), 'FastCube 2 Components');
  frxObjects.RegisterObject1(TfcxpCube, fcxGraphicResources.GetFRBitmap(1), '', 'FastCube 2', 0, -1, [ctDialog, ctData]);
  frxPropertyEditors.Register(TypeInfo(TfcxpCube), nil, '', TfcxpCubeProperty);
  frxPropertyEditors.Register(TypeInfo(TfcxpSliceGridProvider), nil, '', TfcxpSliceGridProviderProperty);
  frxResources.Add('propFileName', fcxResources.Get('propFileName'));

finalization
  frxObjects.UnRegister(TfcxpCube);
  FreeAndNil(SliceGridProviderList);
end.
