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

unit fcxpChartComponents;

interface

{$I frx.inc}
{$I fcx.inc}
{$I fcxTee.inc}

uses
  Windows, Classes, Dialogs, SysUtils, frxClass, fcxCube, fcxSlice,
  fcxTypes,  fcxChart, fcxpChartRTTI
{$IFNDEF NO_CRITICAL_SECTION}
,  SyncObjs
{$ENDIF}
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type

  TfcxpChartComponents = class(TComponent)
  private
    FOldComponents: TfcxpChartComponents;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String;
  published
    property Version: String read GetVersion write SetVersion;
  end;

  TfcxpChartProvider = class(TfrxDialogComponent)
  private
    FEnabled: Boolean;
    FReportRef: TfrxReport;
    FUserName: String;
    FChart: TfcxChart;
    FUseFCChartEvents: boolean;
    procedure SetChart(const Value: TfcxChart);
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
  published
    property Version: String read GetVersion write SetVersion;
    property UseFCChartEvents: boolean read FUseFCChartEvents write FUseFCChartEvents default True;
    property Chart: TfcxChart read FChart write SetChart;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property UserName: String read FUserName write SetUserName;
  end;

  function fcxpFindfcxpChartProvider(const Name: String): TfcxpChartProvider;
  procedure fcxpGetfcxpChartProviderList(List: TStrings);

var
  FCChartComponents: TfcxpChartComponents;

implementation

uses
{$IFNDEF NO_EDITORS}
  //frxEditor,
{$ENDIF}
  fcxpChart,
  fcxpComponents,
  fcxpCross,
  frxDsgnIntf,
  frxRes,
  fcxRes;

var
  fcxpChartProviderList: TfcxpGlobalProviderList;

type
  TfcxpChartProviderProperty = class(TfrxPropertyEditor)
  public
    function GetValue: String; override;
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
  end;

function fcxpFindfcxpChartProvider(const Name: String): TfcxpChartProvider;
var
  i: Integer;
  ds: TfcxpChartProvider;
begin
  Result := nil;
  if Name = '' then
    Exit;
  fcxpChartProviderList.Lock;
  for i := 0 to fcxpChartProviderList.Count - 1 do
  begin
    ds := fcxpChartProviderList[i];
    if AnsiCompareText(ds.UserName, Name) = 0 then
    begin
      Result := fcxpChartProviderList[i];
      break;
    end;
  end;
  fcxpChartProviderList.Unlock;
end;

procedure fcxpGetfcxpChartProviderList(List: TStrings);
var
  i: Integer;
  ds: TfcxpChartProvider;
begin
  fcxpChartProviderList.Lock;
  List.Clear;
  for i := 0 to fcxpChartProviderList.Count - 1 do
  begin
    ds := fcxpChartProviderList[i];
    if (ds <> nil) and (ds.UserName <> '') and ds.Enabled then
      List.AddObject(ds.UserName, ds);
  end;
  fcxpChartProviderList.Unlock;
end;

{ TfcxpChartComponents }

constructor TfcxpChartComponents.Create(AOwner: TComponent);
begin
  inherited;
  FOldComponents := FCChartComponents;
  FCChartComponents := Self;
end;

destructor TfcxpChartComponents.Destroy;
begin
  if FCChartComponents = Self then
    FCChartComponents := FOldComponents;
  inherited;
end;

function TfcxpChartComponents.GetDescription: String;
begin
  Result := 'FastCube 2';
end;

function TfcxpChartComponents.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxpChartComponents.SetVersion(const Value: String);
begin
//
end;

{ TfcxpChartProvider }

constructor TfcxpChartProvider.Create(AOwner: TComponent);
begin
  inherited;
  FUseFCChartEvents := true;
  Enabled := true;
  if fcxpChartProviderList = nil then
    exit;
  fcxpChartProviderList.Lock;
  fcxpChartProviderList.Add(Self);
  fcxpChartProviderList.Unlock;
end;

destructor TfcxpChartProvider.Destroy;
begin
  if fcxpChartProviderList = nil then
  begin
    inherited;
  end
  else
  begin
    fcxpChartProviderList.Lock;
    fcxpChartProviderList.Remove(Self);
    inherited;
    fcxpChartProviderList.Unlock;
  end;
end;

function TfcxpChartProvider.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxpChartProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FChart then
      Chart := nil
end;

procedure TfcxpChartProvider.SetChart(const Value: TfcxChart);
begin
  FChart := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TfcxpChartProvider.SetName(const NewName: TComponentName);
begin
  if NewName <> '' then
    if (FUserName = '') or (FUserName = Name) then
      UserName := NewName;
  inherited;
end;

procedure TfcxpChartProvider.SetUserName(const Value: String);
begin
  if Trim(Value) = '' then
    raise Exception.Create(frxResources.Get('prInvProp'));
  FUserName := Value;
end;

procedure TfcxpChartProvider.SetVersion(const Value: String);
begin
//
end;

{ TfcxpChartProviderProperty }

function TfcxpChartProviderProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

function TfcxpChartProviderProperty.GetValue: String;
var
  Chart: TfcxpChartProvider;
begin
  Chart := TfcxpChartProvider(GetOrdValue);
  if (Chart <> nil) and (frComponent.Report <> nil) then
    Result := Chart.UserName else
    Result := frxResources.Get('prNotAssigned');
end;

procedure TfcxpChartProviderProperty.GetValues;
begin
  fcxpGetfcxpChartProviderList(Values);
end;

procedure TfcxpChartProviderProperty.SetValue(const Value: String);
var
  Chart: TfcxpChartProvider;
begin
  if Value = '' then
    SetOrdValue(0)
  else
  begin
    Chart := fcxpFindfcxpChartProvider(Value);
    if Chart <> nil then
      SetOrdValue(frxInteger(Chart)) else
      raise Exception.Create(frxResources.Get('prInvProp'));
  end;
end;

initialization
  fcxpChartProviderList := TfcxpGlobalProviderList.Create;
  frxPropertyEditors.Register(TypeInfo(TfcxpChartProvider), nil, '', TfcxpChartProviderProperty);

finalization
  fcxpChartProviderList.Free;
  fcxpChartProviderList := nil;
end.
