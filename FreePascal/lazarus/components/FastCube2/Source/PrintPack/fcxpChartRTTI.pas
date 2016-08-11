{******************************************}
{                                          }
{             FastReport v4.0              }
{              Cross-tab RTTI              }
{                                          }
{            Copyright (c) 2001-2014       }
{       by Oleg Pryalkov, Paul Ishenin,    }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpChartRTTI;

interface

{$I frx.inc}
{$I fcx.inc}
{$I fcxTee.inc}

implementation

uses
  Windows, Classes, SysUtils, Forms, fs_iinterpreter, frxClassRTTI, 
  fcxpChartComponents, fcxpChart, fcxChart
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TFunctions = class(TfsRTTIModule)
  private
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;


{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    AddEnum('TfcxSeriesType', 'stTBarSeries, stTLineSeries, stTPointSeries, stTAreaSeries, stTPieSeries, stTHorizBarSeries');
    AddClass(TfcxCustomChart, 'TCustomChart');
    AddClass(TfcxChart, 'TfcxCustomChart');
    AddClass(TfcxpChartProvider, 'TfrxDialogComponent');
    with AddClass(TfcxpChartView, 'TfrxChartView') do
    begin
      AddProperty('Chart', 'TfcxChart', GetProp, nil);
      AddProperty('fcxpChartProvider', 'TfcxpChartProvider', GetProp, nil);
    end;
  end;
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;
  if Instance = nil then
    exit;
  if ClassType = TfcxpChartView then
  begin
    if PropName = 'CHART' then
      Result := frxInteger(TfcxpChartView(Instance).Chart)
    else
    if PropName = 'FCXPCHARTPROVIDER' then
      Result := frxInteger(TfcxpChartView(Instance).fcxpChartProvider)
  end
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.
