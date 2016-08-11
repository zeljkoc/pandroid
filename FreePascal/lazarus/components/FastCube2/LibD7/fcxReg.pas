{*******************************************************}
{                                                       }
{           FastCube 2 Registration unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxReg;
{$INCLUDE fcx.inc}

interface
uses
  Classes
  {$IFNDEF FPC}
    {$IFDEF DELPHI_16UP}
      ,Controls
    {$ENDIF}
    {$IFDEF DELPHI_6UP}
      ,DesignIntf
      ,DesignEditors
    {$ELSE}
      ,DsgnIntf
    {$ENDIF}
  {$ELSE}
    ,PropEdits
    ,LazarusPackageIntf
  {$ENDIF};

procedure Register;

implementation

{$R *.res}

uses
  fcxDefaultSettings,
  fcxDataSource,
  fcxUniqueValue,
  fcxCube,
  fcxTypes,
  fcxSlice,
  fcxFilters,
  fcxComponent,
  fcxControl,
  fcxCubeGrid,
  fcxCubeGridToolBar,
  fcxSliceGrid,
  fcxSliceGridToolBar
{$IFNDEF FPC}
  ,fcxPropertyEditors
{$ENDIF};

{$IFDEF FPC}
procedure RegisterUnitfcxReg;
{$ELSE}
procedure Register;
{$ENDIF}
begin
{$IFDEF DELPHI16}
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TfcxDefaultSettings, TControl);
  GroupDescendentsWith(TfcxDataSet, TControl);
  GroupDescendentsWith(TfcxDBDataSet, TControl);
  GroupDescendentsWith(TfcxUserDataSet, TControl);
  GroupDescendentsWith(TfcxDataSource, TControl);
  GroupDescendentsWith(TfcxComponent, TControl);
  GroupDescendentsWith(TfcxCube, TControl);
  GroupDescendentsWith(TfcxSlice, TControl);
  GroupDescendentsWith(TfcxFilterManager, TControl);
  GroupDescendentsWith(TfcxCubeGrid, TControl);
  GroupDescendentsWith(TfcxSliceGrid, TControl);
  GroupDescendentsWith(TfcxSliceGridToolBar, TControl);
  GroupDescendentsWith(TfcxCubeGridToolBar, TControl);
{$ENDIF}

  RegisterComponents('FastCube 2', [
    TfcxDefaultSettings, TfcxDBDataSet, TfcxUserDataSet, TfcxDataSource,
    TfcxCube, TfcxSlice, TfcxFilterManager,
    TfcxCubeGrid, TfcxSliceGrid, TfcxSliceGridToolBar, TfcxCubeGridToolBar]);
  
{$IFNDEF FPC}
  RegisterPropertyEditor(TypeInfo(string), TfcxReferenceDataField, 'DataFieldName', TfcxReferenceDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(TfcxString), TfcxSourceFieldProperties, 'CaptionSourceAttribute', TfcxAttributeProperty);
  RegisterPropertyEditor(TypeInfo(TfcxString), TfcxSourceFieldProperties, 'OrderSourceAttribute', TfcxAttributeProperty);
  RegisterPropertyEditor(TypeInfo(TfcxSourceFieldProperties), TfcxSourceField, 'SourceFieldProperties', TfcxAttributeTypeProperty);
  RegisterPropertyEditor(TypeInfo(TfcxDateTypes), TfcxSplitProperty, 'DateSplitPaths', TfcxSplitTypesSetProperty);
  RegisterPropertyEditor(TypeInfo(TfcxTimeTypes), TfcxSplitProperty, 'TimeSplitPaths', TfcxSplitTypesSetProperty);

//  RegisterPropertyEditor(TypeInfo(TfcxDataType), TfcxCustomSplitPath, 'DataType', TfcxDataTypeProperty);

  RegisterComponentEditor(TfcxDataSource, TfcxDataSourceEditor);
  RegisterComponentEditor(TfcxComponent, TfcxComponentEditor);
  RegisterComponentEditor(TfcxCustomControl, TfcxComponentEditor);
  RegisterComponentEditor(TfcxDefaultSettings, TfcxComponentEditor);
  RegisterPropertyEditor(TypeInfo(TfcxAttributeType), TfcxSourceField,'SourceFieldType',nil);
{$ENDIF}
end;

{$ifdef FPC}
procedure Register;
begin
  RegisterUnit('fcxReg', @RegisterUnitfcxReg);
end;
{$endif}

end.
