{*******************************************************}
{                                                       }
{              FastCube 2 Alerts unit                   }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxAlerts;
{$INCLUDE fcx.inc}

interface
uses
  Classes, SysUtils, fcxTypes;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils, FMX.fcxTypes;
{$ENDIF FMX}

type
  TfcxAlertType = (
    at_Cube,
    at_Filters,
    at_Slice
  );

// Changes in slice
  TfcxChangeInSlice = (
     chs_None,                 // Set Cube property
     chs_SetCube,              // Set Cube property
     chs_SetFilterManager,     // Set FilterManager property
     chs_CubeChanged,          // Global changes in Cube
     chs_FilterManagerChanged, // Global changes in FilterManager
     chs_FiltersChanged,       // Changes in Filters
     chs_ChangeAxis,           // Change X and Y axis
     chs_CapXAxis,             // Changed fields list in X axis
     chs_CapYAxis,             // Changed fields list in Y axis
     chs_CapPage,              // Changed fields list in Pages
     chs_CapMeasures,          // Changed fields list in Measures
     chs_MeasuresFieldPosition,// œÓÎÓÊÂÌËÂ ÔÓÎˇ œŒ ¿«¿“≈À»
     chs_NeedFillVisibleX,     // Need to run FillVisibleArray in X Axis
     chs_NeedFillVisibleY,     // Need to run FillVisibleArray in Y Axis
     chs_XAxis,                // Changed in X axis (to redraw)
     chs_YAxis,                // Changed in Y axis (to redraw)
     chs_Page,                 // Changed in Pages  (to redraw)
     chs_Fact,                 // Changed in Facts  (to redraw)
     chs_ExpandXGroup,         // Expanded/collapsed Group in X Axis
     chs_ExpandYGroup,         // Expanded/collapsed Group in Y Axis
     chs_ExpandXNode,          //
     chs_ExpandYNode,          //
     chs_ColWidth,             //
     chs_RowHeight,            //
     chs_XHeight,              //
     chs_YWidth,               //
     chs_CapXWidth,            //
     chs_CapYWidth,            //
     chs_CapPageWidth,         //
     chs_XSortDirection,       //
     chs_YSortDirection,       //
     chs_XTotalPosition,       //
     chs_YTotalPosition,       //
     chs_XTypeSort,            // Sort Type in X Axis
     chs_YTypeSort,            // Sort Type in Y Axis
     chs_XAxisType,            // Axis Type in X Axis
     chs_YAxisType,            // Axis Type in Y Axis
     chs_Load,                 // Slice has finished load from the stream
     chs_TopNChanged,          // Changes in TopN
     chs_DefaultColWidth,
     chs_DefaultRowHeight

{
     tch_CapFacts,       //  Need comments !!!
     tch_CapXAx,         //
     tch_CapYAx,         //
     tch_CapPage,        //
     tch_Filter,         //
     tch_MakeTotalXAx,   //
     tch_MakeTotalYAx,   //
     tch_BuildCube,      //
     tch_ExpandXAx,      //
     tch_ExpandYAx,      //
     tch_VisibleInData,  //
     tch_DisplayAs,      //
     tch_Clear,          //
     tch_OrderOfXAx,     //
     tch_OrderOfYAx,     //
     tch_TypeOrderOfXAx, //
     tch_TypeOrderOfYAx, //
     tch_UpdateListners  // nothing to do, just call update listners
}
  );

// Set changes in slice
  TfcxChangesInSlice = set of TfcxChangeInSlice;

  TfcxSliceChangeType = (
    scht_All,     // structure changed
    scht_Size,    // cells size changed
    scht_Region,  // region changed
    scht_Scroll   // first visible col or row changed
  );

  TfcxChangeAlert = class
  private
    FAlertType: TfcxAlertType;
  public
    constructor Create(AAlertType: TfcxAlertType); overload; virtual;
    property AlertType: TfcxAlertType read FAlertType;
  end;

  TfcxAction = class(TBasicAction)
  private
    FChangeAlert: TfcxChangeAlert;
    procedure SetChangeAlert(const Value: TfcxChangeAlert);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFCAction(AOwner: TComponent; AChangeAlert: TfcxChangeAlert); virtual;
    destructor Destroy; override;
    property ChangeAlert: TfcxChangeAlert read FChangeAlert write SetChangeAlert;
  end;

// Cube Alerts
// Changes in Cube
  TfcxChangeInCube =
  (
     chc_BeforeOpen,
     chc_AfterOpen,
     chc_BeforeClose,
     chc_AfterClose,
     chc_FieldsList,
     chc_AddedFields,   // need send field index
     chc_DeletedField,  // need send field index
     chc_GroupsChanged, // changes in groups in single field
     chc_ChangedSplits, // need send masterfield object
     chc_AddedOneSplit, // need send masterfield object and splitfield index
     chc_DeletingOneSplit, // need send masterfield object and splitfield index
     chc_DeletedOneSplit,  // need send masterfield object and old splitfield index
     chc_AppendedData,
     chc_Caption
  );

// Set changes in Cube
  TfcxChangesInCube = set of TfcxChangeInCube;

  TfcxCubeChangeType =
  (
    ccht_All,       // Global Cube changed
    ccht_FieldList, // List of Fields
    ccht_Field, // Changed one field
    ccht_NeedRedraw, // Redraw visual components
    ccht_OneSplit, // Changed one split in field
    ccht_ChangesInData, // Changed data in cube
    ccht_Nothing
  );

  TfcxCubeChangeAlert = class(TfcxChangeAlert)
  private
    FCubeChangeType: TfcxCubeChangeType;
    FChangesInCube: TfcxChangesInCube;
    FFieldIndex: integer;
  public
    constructor Create(ACubeChangeType: TfcxCubeChangeType; AChangesInCube: TfcxChangesInCube); overload;
    constructor Create(ACubeChangeType: TfcxCubeChangeType; AChangesInCube: TfcxChangesInCube; AFieldIndex: integer); overload;
    property CubeChangeType: TfcxCubeChangeType read FCubeChangeType;
    property ChangesInCube: TfcxChangesInCube read FChangesInCube;
    property FieldIndex: integer read FFieldIndex;
  end;

  TfcxCubeSplitsChangeAlert = class(TfcxCubeChangeAlert)
  private
    FMasterField: TObject;
    FSplitFieldIndex: integer;
  public
    constructor Create(ACubeChangeType: TfcxCubeChangeType; AChangesInCube: TfcxChangesInCube; AFieldIndex: integer; AMasterField: TObject; ASplitFieldIndex: integer); overload;
    property MasterField: TObject read FMasterField;
    property SplitFieldIndex: integer read FSplitFieldIndex;
  end;

// Filters Alerts
// Changes in filters
  TfcxChangeInFilters =
  (
     chf_SetCube,         // Set Cube property
     chf_Changed,         // filters changed
     chf_ActivedFilterChanged,   // Actived filter changed
     chf_NonActivedFilterChanged, // NonActived filter changed
     chf_UVFilterTypeChanged, // UVFilterType changed (need redraw)
     chf_Captions, //  Any Captions changed (need redraw)
     chf_ResetAllFilters // Reset All Filters
  );

// Set changes in Filters
  TfcxChangesInFilters = set of TfcxChangeInFilters;

  TfcxFiltersChangeType =
  (
    fcht_All,     // filters changed
    fcht_EnabledItems, // changed Enabled Items (Enabled filter, ...)
    fcht_DisabledItems, // changed Disabled Items (Disabled filter, ...)
    fcht_NeedRedraw // Redraw visual components
  );

  TfcxFiltersChangeAlert = class(TfcxChangeAlert)
  private
    FFiltersChangeType: TfcxFiltersChangeType;
    FChangesInFilters: TfcxChangesInFilters;
    FFilterIndex: integer;
  public
    constructor Create(AFiltersChangeType: TfcxFiltersChangeType; AChangesInFilters: TfcxChangesInFilters); overload;
    constructor Create(AFiltersChangeType: TfcxFiltersChangeType; AChangesInFilters: TfcxChangesInFilters; AFilterIndex: integer); overload;
    property FiltersChangeType: TfcxFiltersChangeType read FFiltersChangeType;
    property ChangesInFilters: TfcxChangesInFilters read FChangesInFilters;
  end;

// Slice Alerts
  TfcxSliceChangeAlert = class(TfcxChangeAlert)
  private
    FSliceChangeType: TfcxSliceChangeType;
    FChangesInSlice: TfcxChangesInSlice;
    FRegions: TfcxRegionsOfField;
  public
    constructor Create(ASliceChangeType: TfcxSliceChangeType; AChangesInSlice: TfcxChangesInSlice; ARegions: TfcxRegionsOfField); overload;
    property SliceChangeType: TfcxSliceChangeType read FSliceChangeType;
    property ChangesInSlice: TfcxChangesInSlice read FChangesInSlice;
    property Regions: TfcxRegionsOfField read FRegions;
  end;

  function CubeChangeTypeOfChanges(AChanges: TfcxChangesInCube): TfcxCubeChangeType;
  function FiltersChangeTypeOfChanges(AChanges: TfcxChangesInFilters): TfcxFiltersChangeType;
  function SliceChangeTypeOfChanges(AChanges: TfcxChangesInSlice): TfcxSliceChangeType;

const
  ChangesInCubeChangeType: Array[TfcxCubeChangeType] of TfcxChangesInCube =
  (
{ccht_All      } [chc_AfterOpen, chc_AfterClose, chc_FieldsList],
{ccht_FieldList} [chc_AddedFields, chc_DeletedField, chc_ChangedSplits],
{ccht_Field} [chc_GroupsChanged],
{ccht_NeedRedraw} [chc_Caption],
{ccht_OneSplit} [chc_AddedOneSplit, chc_DeletingOneSplit, chc_DeletedOneSplit],
{ccht_ChangesInData} [chc_AppendedData],
{ccht_Nothing} []
  );

  ChangesInFiltersChangeType: Array[TfcxFiltersChangeType] of TfcxChangesInFilters =
  (
{fcht_All}    [chf_SetCube, chf_Changed],
{fcht_EnabledItems} [chf_ActivedFilterChanged, chf_ResetAllFilters],
{fcht_DisabledItems} [chf_NonActivedFilterChanged],
{fcht_NeedRedraw} [chf_UVFilterTypeChanged, chf_Captions]
  );

  ChangesInSliceChangeType: Array[TfcxSliceChangeType] of TfcxChangesInSlice =
  (
{scht_All}    [chs_SetCube, chs_SetFilterManager, chs_CubeChanged , chs_FilterManagerChanged, chs_Load],
{scht_Size}   [chs_DefaultColWidth, chs_DefaultRowHeight],
{scht_Region} [chs_ChangeAxis, chs_CapXAxis, chs_CapYAxis, chs_CapPage, chs_CapMeasures,
               chs_NeedFillVisibleX, chs_NeedFillVisibleY, chs_XAxis, chs_YAxis, chs_Page, chs_Fact,
               chs_MeasuresFieldPosition, chs_ExpandXNode,
               chs_ExpandYNode, chs_ColWidth, chs_RowHeight, chs_XSortDirection,
               chs_YSortDirection, chs_XTotalPosition, chs_YTotalPosition,
               chs_XHeight, chs_YWidth, chs_CapXWidth, chs_CapYWidth, chs_CapPageWidth,
               chs_XTypeSort, chs_YTypeSort, chs_XAxisType, chs_YAxisType],
{scht_Scroll} []
  );

implementation

function CubeChangeTypeOfChanges(AChanges: TfcxChangesInCube): TfcxCubeChangeType;
var
  ACubeChangeType: TfcxCubeChangeType;
begin
  Result := ccht_All;
  if (AChanges * ChangesInCubeChangeType[ccht_All]) <> [] then
    exit;
  for ACubeChangeType := Succ(ccht_All) to High(TfcxCubeChangeType) do
  begin
    if (AChanges * ChangesInCubeChangeType[ACubeChangeType]) <> [] then
    begin
      if Result <> ccht_All then
      begin
        Result := ccht_All;
        exit;
      end;
      Result := ACubeChangeType;
    end;
  end;
end;

function FiltersChangeTypeOfChanges(AChanges: TfcxChangesInFilters): TfcxFiltersChangeType;
var
  AFiltersChangeType: TfcxFiltersChangeType;
  ANeedRedraw: Boolean;
begin
  Result := fcht_All;
  if (AChanges * ChangesInFiltersChangeType[fcht_All]) <> [] then
    exit;
  ANeedRedraw := False;
  for AFiltersChangeType := Succ(fcht_All) to High(TfcxFiltersChangeType) do
  begin
    if AFiltersChangeType = fcht_NeedRedraw then
      ANeedRedraw := (AChanges * ChangesInFiltersChangeType[AFiltersChangeType]) <> []
    else
    if (AChanges * ChangesInFiltersChangeType[AFiltersChangeType]) <> [] then
    begin
      if Result <> fcht_All then
      begin
        Result := fcht_All;
        exit;
      end;
      Result := AFiltersChangeType;
    end;
  end;
  if ANeedRedraw and (Result = fcht_All) then
    Result := fcht_NeedRedraw;
end;

function SliceChangeTypeOfChanges(AChanges: TfcxChangesInSlice): TfcxSliceChangeType;
var
  ASliceChangeType: TfcxSliceChangeType;
begin
  Result := scht_All;
  if (AChanges * ChangesInSliceChangeType[scht_All]) <> [] then
    exit;
  for ASliceChangeType := Succ(scht_All) to High(TfcxSliceChangeType) do
  begin
    if (AChanges * ChangesInSliceChangeType[ASliceChangeType]) <> [] then
    begin
      if Result <> scht_All then
      begin
        Result := scht_All;
        exit;
      end;
      Result := ASliceChangeType;
    end;
  end;
end;

{ TfcxChangeAlert }

constructor TfcxChangeAlert.Create(AAlertType: TfcxAlertType);
begin
  FAlertType := AAlertType;
end;

{ TfcxAction }

constructor TfcxAction.Create(AOwner: TComponent);
begin
  inherited;
  FChangeAlert := nil;
end;

constructor TfcxAction.CreateFCAction(AOwner: TComponent;
  AChangeAlert: TfcxChangeAlert);
begin
  Create(AOwner);
  FChangeAlert := AChangeAlert;
end;

destructor TfcxAction.Destroy;
begin
  FreeAndNil(FChangeAlert);
  inherited;
end;

procedure TfcxAction.SetChangeAlert(const Value: TfcxChangeAlert);
begin
  FChangeAlert := Value;
end;

{ TfcxFiltersChangeAlert }

constructor TfcxFiltersChangeAlert.Create(
  AFiltersChangeType: TfcxFiltersChangeType;
  AChangesInFilters: TfcxChangesInFilters);
begin
  inherited Create(at_Filters);
  FFiltersChangeType := AFiltersChangeType;
  FChangesInFilters := AChangesInFilters;
  FFilterIndex := -1;
end;

constructor TfcxFiltersChangeAlert.Create(
  AFiltersChangeType: TfcxFiltersChangeType;
  AChangesInFilters: TfcxChangesInFilters; AFilterIndex: integer);
begin
  Create(AFiltersChangeType, AChangesInFilters);
  FFilterIndex := AFilterIndex;
end;

{ TfcxCubeChangeAlert }

constructor TfcxCubeChangeAlert.Create(ACubeChangeType: TfcxCubeChangeType;
  AChangesInCube: TfcxChangesInCube);
begin
  inherited Create(at_Cube);
  FCubeChangeType := ACubeChangeType;
  FChangesInCube := AChangesInCube;
end;

constructor TfcxCubeChangeAlert.Create(ACubeChangeType: TfcxCubeChangeType;
  AChangesInCube: TfcxChangesInCube; AFieldIndex: integer);
begin
  Create(ACubeChangeType, AChangesInCube);
  FFieldIndex := AFieldIndex;
end;

{ TfcxSliceChangeAlert }

constructor TfcxSliceChangeAlert.Create(
  ASliceChangeType: TfcxSliceChangeType;
  AChangesInSlice: TfcxChangesInSlice; ARegions: TfcxRegionsOfField);
begin
  inherited Create(at_Slice);
  FSliceChangeType := ASliceChangeType;
  FChangesInSlice := AChangesInSlice;
  FRegions := ARegions
end;

{ TfcxCubeSplitsChangeAlert }

constructor TfcxCubeSplitsChangeAlert.Create(
  ACubeChangeType: TfcxCubeChangeType; AChangesInCube: TfcxChangesInCube;
  AFieldIndex: integer;  AMasterField: TObject; ASplitFieldIndex: integer);
begin
  Create(ACubeChangeType, AChangesInCube, AFieldIndex);
  FMasterField := AMasterField;
  FSplitFieldIndex := ASplitFieldIndex;
end;

end.
