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

unit fcxpCrossRTTI;

interface

{$I frx.inc}
{$I fcx.inc}

implementation

uses
  Classes, SysUtils, Forms, fs_iinterpreter, fcxpCross, frxClassRTTI, fcxpComponents,
  fcxComponent, fcxCustomGrid, fcxSliceGrid, fcxCube, fcxSlice
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
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
    AddClass(TfcxCustomGrid, 'TCustomControl');
    AddClass(TfcxSliceGrid, 'TfcxCustomGrid');
    AddClass(TfcxCube, 'TComponent');
    AddEnum('TfcxRegionOfField', 'rf_Page, rf_CapXAx, rf_CapYAx, rf_CapFacts, rf_None');

    with AddClass(TfcxCommonFieldsOfRegion, 'TObject') do
    begin
      AddProperty('Count', 'integer', GetProp, nil);
    end;
    AddClass(TfcxComponent, 'TComponent');
    AddClass(TfcxAbstractSlice, 'TfcxComponent');
    with AddClass(TfcxSlice, 'TfcAbstractSlice') do
    begin
      AddIndexProperty('FieldsOfRegion', 'TfcxRegionOfField', 'TfcxCommonFieldsOfRegion', CallMethod, True);
    end;
    AddClass(TfcxpCube, 'TfrxDialogComponent');
    AddClass(TfcxpSliceGridProvider, 'TfrxDialogComponent');
    with AddClass(TfcxpCrossView, 'TfrxView') do
    begin
      AddMethod('procedure Update', CallMethod);
      AddProperty('Slice', 'TfcxSlice', GetProp, nil);
      AddProperty('SliceGridProvider', 'TfcxpSliceGridProvider', GetProp, nil);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  CrossView: TfcxpCrossView;
begin
  Result := 0;
  if Instance = nil then
    exit;
  if ClassType = TfcxpCrossView then
  begin
    CrossView := TfcxpCrossView(Instance);
    if MethodName = 'UPDATE' then
      CrossView.Update
  end
  else
  if ClassType = TfcxSlice then
  begin
    if MethodName = 'FIELDSOFREGION.GET' then
      Result := frxInteger(TfcxSlice(Instance).FieldsOfRegion[Caller.Params[0]])
  end;
  if Result = 0 then
    Result := 0;
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;
  if Instance = nil then
    exit;
  if ClassType = TfcxpCrossView then
  begin
    if PropName = 'SLICE' then
      Result := frxInteger(TfcxpCrossView(Instance).Slice)
    else if PropName = 'SLICEGRIDPROVIDER' then
      Result := frxInteger(TfcxpCrossView(Instance).SliceGridProvider)
  end
  else
  if ClassType = TfcxCommonFieldsOfRegion then
  begin
    if PropName = 'COUNT' then
      Result := TfcxCommonFieldsOfRegion(Instance).Count
  end
end;

initialization
  fsRTTIModules.Add(TFunctions);

finalization
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TFunctions);

end.
