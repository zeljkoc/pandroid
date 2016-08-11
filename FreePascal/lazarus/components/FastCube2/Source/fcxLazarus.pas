{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fcxLazarus;

interface

uses
  fcxAlerts, fcxCodeUtils, fcxComponent, fcxCube, fcxCubeGrid, fcxCustomGrid, 
  fcxDataSource, fcxDefaultSettings, fcxError, fcxFields, fcxFilterPopup, 
  fcxFilters, fcxFormats, fcxGraphicUtils, fcxGridPainters, fcxInterpreter, 
  fcxList, fcxMeasureEditor, fcxPainters, fcxPopupWindow, fcxQSort, 
  fcxrcDesgn, fcxrcStrings, fcxReg, fcxRes, fcxSliceGrid, fcxSliceGridToolbar, 
  fcxSlice, fcxSort, fcxStreamUtils, fcxStringUtils, fcxTypes, fcxUniqueArray, 
  fcxUniqueValue, fcxZone, fcxDimensionEditor, fcxFormatFrame, 
  fcxContinuousHighlightFrame, fcxHighlightRuleEditor, fcxHighlights, 
  fcxImagesPopup, fcxRangeHighlightFrame, fcxStyleFrame, fcxStylesEditor, 
  fcxCustomToolbar, fcxUtils, fcxUnicodeUtils, fcxCustomExport, fcxrcExports, 
  fcxInfo, fcxCubeGridToolBar, fcxXml, fcxControl, fcxAxisEditor, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fcxReg', @fcxReg.Register);
end;

initialization
  RegisterPackage('fcxLazarus', @Register);
end.
