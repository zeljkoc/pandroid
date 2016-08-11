{*******************************************************}
{                                                       }
{             FastCube 2 slice grid unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxSliceGrid;

interface

{$INCLUDE fcx.inc}

uses
  {$ifndef fpc}
  Windows,
  {$else}
  LCLType, LCLIntf,
  {$endif}
{$IFDEF DELPHI_6UP}
  Types,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Menus, Dialogs, ImgList,
  fcxDefaultSettings, fcxDataSource, fcxFilters, fcxCube,
  fcxTypes, fcxStringUtils, fcxComponent, fcxControl, fcxZone, fcxPainters,
  fcxStyles, fcxGridPainters, fcxGraphicUtils, fcxCustomGrid, fcxAlerts, fcxSlice,
  fcxFormats, fcxPopupWindow, fcxFields, fcxUniqueArray, fcxCustomExport;

const
  gsFirstSliceGridStyle = gsLastCustomGridStyle + 1;
  gsActiveDimension = gsFirstSliceGridStyle + 0;
  gsInactiveDimension = gsFirstSliceGridStyle + 1;
  gsMeasure = gsFirstSliceGridStyle + 2;
  gsDataCellsTotals = gsFirstSliceGridStyle + 3;
  gsFieldsItem = gsFirstSliceGridStyle + 4;
  gsLastSliceGridStyle = gsFieldsItem;

  fcxSliceGridStyleNames: array[gsFirstSliceGridStyle..gsLastSliceGridStyle] of String = (
    'sActiveDimension',
    'sInactiveDimension',
    'sMeasure',
    'sDataCellsTotals',
    'sFieldsItem'
  );

const
  curDrag = 4096;

type
  TfcxSliceGrid = class;
  TfcxSliceCustomAxisZone = class;
  TfcxSliceCustomItemsZone = class;

  TfcxSliceGridStyles = class(TfcxCustomGridStyles)
  private
    FStyles: array[gsFirstSliceGridStyle..gsLastSliceGridStyle] of TfcxCustomThemeStyle;
  protected
    function GetStyle(Index: Integer): TfcxCustomThemeStyle; override;
    function GetStyleName(Index: Integer): String; override; 
    procedure InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); override;
    procedure SetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); override;
    procedure SetDefaultValues; override;
    function GetLastStyleIndex: Integer; override;
  published
    property ParentFont;
    property CaptionArea;
    property HeaderArea;
    property HeaderCells;
    property HeaderCellsSelected;
    property DataArea;
    property DataCells;
    property DataCellsSelected;
    property ActiveDimension: TfcxCustomThemeStyle index gsActiveDimension read GetStyle write SetStyle;
    property InactiveDimension: TfcxCustomThemeStyle index gsInactiveDimension read GetStyle write SetStyle;
    property Measure: TfcxCustomThemeStyle index gsMeasure read GetStyle write SetStyle;
    property DataCellsTotals: TfcxCustomThemeStyle index gsDataCellsTotals read GetStyle write SetStyle;
    property FieldsItem: TfcxCustomThemeStyle index gsFieldsItem read GetStyle write SetStyle;
  end;

  TfcxSliceGridZone = class(TfcxGridZone)
  private
    function GetGrid: TfcxSliceGrid;
  public
    property Grid: TfcxSliceGrid read GetGrid;
  end;

  TfcxSliceCaptionZone = class(TfcxSliceGridZone)
  public
    procedure ClientPaint; override;
    procedure ContextPopup(X: Integer; Y: Integer; var Handled: Boolean); override;
    function CreatePopupMenu: TPopupMenu; override;
    procedure PreparePopupMenu;
    procedure PopupMenuItemClick(Sender: TObject);
  published
    property Visible;
  end;

  { TfcxSliceDataZone }

  TfcxSliceDataZone = class(TfcxCustomDataZone)
  private
    FPopupCell: TfcxMeasureCell;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FDrawStyle: TfcxCustomThemeStyle;
    FDetailColumns: TfcxCubeDataColumns;
    function GetGrid: TfcxSliceGrid;
    function GetSelectedMeasure: Integer;
    procedure SetImages(const Value: TCustomImageList);
    procedure ImageListChange(Sender: TObject);
  protected
    function GetColCount: Integer; override;
    function GetRowCount: Integer; override;
    function GetColWidth(AIndex: Integer): Integer; override;
    function GetRowHeight(AIndex: Integer): Integer; override;
    procedure SetColWidth(AIndex: Integer; AWidth: Integer); override;
    procedure SetRowHeight(AIndex: Integer; AHeight: Integer); override;
    function IsResizeableFrame(AFrame: TZoneFrame): Boolean; override;
    function CanUpdateScrolls: Boolean; override;

    function GetAsPlainText: AnsiString; override;
    function GetText(ACol, ARow: Integer): String; override;

    function IneritedGetHorzScrollMax: Integer;
    function IneritedGetVertScrollMax: Integer;
    function GetHorzScrollMax: Integer; override;
    function GetVertScrollMax: Integer; override;

    function CreatePopupMenu: TPopupMenu; override;
    procedure PreparePopupMenuFor(APoint: TfcxDataPoint);
    procedure ContextPopup(X: Integer; Y: Integer; var Handled: Boolean); override;
    procedure DblClick(X, Y: Integer); override;
    procedure StopSizing(var AInfo: TSizingInfo); override;

    procedure PopupItemClick(Sender: TObject);
    procedure AlignmentPopupItemClick(Sender: TObject);
    procedure DisplayAsPopupItemClick(Sender: TObject);
    procedure SelectPopupItemClick(Sender: TObject);
    procedure FullUpdate(AChanges: TfcxChangesInSlice);
    // virtual methods
    procedure DoDrawHighlights(AHookData: Pointer;
      APainter: TfcxCustomPainter; ACanvas: TCanvas; var ARect: TRect;
      var CanDrawImage, CanDrawText: Boolean); virtual;
    procedure DoDrawCell(const ACell: TfcxMeasureCell; AStates: TfcxThemeCellStates; ARect: TRect); virtual;
    procedure DoGetCellImageIndex(const ACell: TfcxMeasureCell; out ImageIndex: Integer); virtual;
    procedure DoGetCellStyle(const ACell: TfcxMeasureCell; States: TfcxThemeCellStates;
      Style: TfcxCustomThemeStyle); virtual;
    procedure DoGetCellText(const ACell: TfcxMeasureCell; out Text: String); virtual;
    procedure SelectionChanged(Sender: TZone);
  public
    constructor Create(AOwner: TZoneContainer); override;
    destructor Destroy; override;
    procedure ClientPaint; override;
    function AcceptDrag(DragItem: TfcxCustomDragItem): Boolean; override;
    procedure DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer); override;
    property Grid: TfcxSliceGrid read GetGrid;
    property SelectedMeasure: Integer read GetSelectedMeasure;
    property PopupCell: TfcxMeasureCell read FPopupCell;
  published
    property Images: TCustomImageList read FImages write SetImages;
  end;

  TfcxCellInfos = class;
  TfcxCellInfo = class
  private
    FOwner: TfcxCellInfos;
    FBoundingRect: TRect;
    FData: TfcxSliceDrawHeader;
    FTreeButtonPos: TPoint;
    FHierButtonPos: TPoint;
    FGraphicSize: TSize;
    FIsSelected: Boolean;
    procedure SetBoundingRect(const Value: TRect);
  public
    constructor Create(AOwner: TfcxCellInfos); overload;
    constructor Create(AOwner: TfcxCellInfos; AData: TfcxSliceDrawHeader); overload;
    procedure Assign(AInfo: TfcxCellInfo);
    property BoundingRect: TRect read FBoundingRect write SetBoundingRect;
    property GraphicSize: TSize read FGraphicSize;
    property Data: TfcxSliceDrawHeader read FData;
    property IsSelected: Boolean read FIsSelected;
    property TreeButtonPos: TPoint read FTreeButtonPos;
    property HierButtonPos: TPoint read FHierButtonPos;
  end;

  { TfcxCellInfos }

  TfcxCellInfos = class(TList)
  private
    FOwner: TfcxSliceCustomAxisZone;
    FValid: Boolean;
    FLastIndices: PfcxIntegerArray;
    FLastIndicesCount: Integer;
    FFirstLevel: Integer;
    FFirstCell: Integer;
    FLastLevel: Integer;
    FLastCell: Integer;
    FTreeLevelSpacing: Integer;
    FTreeMeasureSize: Integer;
    FTreeLevelSize: Integer;
    function GetItem(AIndex: Integer): TfcxCellInfo;
    procedure SetItem(AIndex: Integer; const Value: TfcxCellInfo);
    function GetItemByLastIndex(AIndex: Integer): TfcxCellInfo;
    procedure FixRects(const AInfo: TfcxCellInfo);
    function GetLevelSize(const AInfo: TfcxCellInfo): Integer;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetBoundingRect: TRect;
    function GetBoundingRgn: HRGN;
  public
    constructor Create(const AOwner: TfcxSliceCustomAxisZone);
    function AddCell(const ARec: TfcxSliceDrawHeader): TfcxCellInfo;
    procedure Clear; override;
    function FindByPosition(P: TPoint): TfcxCellInfo;
    function GetStartPointFor(const AInfo: TfcxCellInfo): TPoint;
    function GetSizeFor(const AInfo: TfcxCellInfo): TSize;
    procedure IncSize(Delta: Integer);
    property Items[AIndex: Integer]: TfcxCellInfo read GetItem write SetItem; default;
    property ItemByLastIndex[AIndex: Integer]: TfcxCellInfo read GetItemByLastIndex;
    property Valid: Boolean read FValid write FValid;
    property LastIndicesCount: Integer read FLastIndicesCount;

    property FirstLevel: Integer read FFirstLevel write FFirstLevel;
    property LastLevel: Integer read FLastLevel write FLastLevel;
    property FirstCell: Integer read FFirstCell write FFirstCell;
    property LastCell: Integer read FLastCell write FLastCell;

    property TreeLevelSize: Integer read FTreeLevelSize write FTreeLevelSize;
    property TreeLevelSpacing: Integer read FTreeLevelSpacing write FTreeLevelSpacing;
    property TreeMeasureSize: Integer read FTreeMeasureSize write FTreeMeasureSize;
  end;

  TfcxSlicAxisZoneClientPart = (
    scpNone,
    scpXSizing,
    scpYSizing,
    scpTreeButton,
    scpHierButton
  );

  { TfcxSliceCustomAxisZone }

  TfcxSliceCustomAxisZone = class(TfcxSliceGridZone)
  private
    FFirstVisibleItem: Integer;
    FFirstVisibleLevel: Integer;
    FActiveClientPart: TfcxSlicAxisZoneClientPart;
    FCells: TfcxCellInfos;
    FTempCells: TfcxCellInfos;
    FCurrentCellInfo: TfcxCellInfo;
    FLastCursorPos: TPoint;
    FPopupInfo: TfcxCellInfo;
    FAutoSize: Boolean;
    FAutoSizeConstraint: Integer;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FDrawStyle: TfcxCustomThemeStyle;
    procedure ImageListChange(Sender: TObject);
    procedure SetCurrentCellInfo(const Value: TfcxCellInfo);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetAutoSizeConstraint(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
  protected
    function FindItemFromCoordinate(AItemCoordinate: Integer): Integer; virtual;
    function GetItemSize(AItemIndex: Integer): Integer; virtual;
    function GetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart): Integer; virtual; abstract;
    procedure SetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart; const Value: Integer); virtual; abstract;
    procedure AssignTo(Dest: TPersistent); override;
    function CreatePopupMenu: TPopupMenu; override;
    procedure PreparePopupMenuFor(AInfo: TfcxCellInfo);
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer): Boolean; override;
    procedure MouseLeave; override;
    procedure DblClick(X: Integer; Y: Integer); override;
    procedure ContextPopup(X: Integer; Y: Integer; var Handled: Boolean); override;
    procedure PopupItemClick(Sender: TObject);
    procedure MoveToGroupClick(Sender: TObject);
    procedure TotalsPopupItemClick(Sender: TObject);
    procedure AxisTypePopupItemClick(Sender: TObject);

    procedure UpdateCursor; override;
    procedure UpdateActiveClientPart(CursorPos: TPoint);

    function ClientHitTest(P: TPoint): TfcxSlicAxisZoneClientPart; virtual;
    procedure HintInfo(P: TPoint; out HintStr: String; out HintRect: TRect); override;

    function DrawCell(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean; virtual; abstract;
    function CalcCellSize(Sender: TfcxAxisContainer; AInfo: TfcxCellInfo): TSize; virtual; abstract;
    function DrawCellForScroll(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean; virtual; abstract;

    procedure DrawCellInfo(Info: TfcxCellInfo);
    function MeasureCellSize(Info: TfcxCellInfo; GrowInHeight: Boolean): TSize;

    function GetAxisContainer: TfcxAxisContainer; virtual;
    function GetFirstVisibleLevel: Integer; virtual;
    function GetLastVisibleLevel: Integer; virtual;
    function GetFirstVisibleItem: Integer; virtual;
    function GetLastVisibleItem: Integer; virtual;
    procedure SetFirstVisibleItem(const Value: Integer); virtual;
    procedure SetFirstVisibleLevel(const Value: Integer); virtual;

    function GetScrollMax(const DefScrollMax: Integer): Integer; virtual;

    function GetRequiredSize: Integer; virtual;
    procedure FullUpdate(AChanges: TfcxChangesInSlice); virtual;
    procedure SetActiveClientPart(const Value: TfcxSlicAxisZoneClientPart); virtual;

    procedure StartSizing(var AInfo: TSizingInfo); override;
    procedure MoveSizing(var AInfo: TSizingInfo); override;
    procedure StopSizing(var AInfo: TSizingInfo); override;
    procedure UpdateSizingDrawInfo(var AInfo: TSizingInfo); override;

    procedure DrawEmptyText(ARect: TRect); virtual;

    function IsSelected(Info: TfcxCellInfo): Boolean; virtual;
    procedure SelectionChanged(Sender: TZone); virtual;
    procedure SelectData(Info: TfcxCellInfo); virtual;
    procedure InvalidateItem(Info: TfcxCellInfo);

    procedure DoDrawCell(AState: TfcxThemeState; const AInfo: TfcxCellInfo); virtual;
    function DoMeasureCell(AState: TfcxThemeState; const AInfo: TfcxCellInfo; GrowInHeight: Boolean): TSize; virtual;
    procedure DoGetCellImageIndex(const AInfo: TfcxCellInfo; out ImageIndex: Integer); virtual;
    procedure DoGetCellStyle(const AInfo: TfcxCellInfo; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle); virtual;
    procedure DoGetCellText(const AInfo: TfcxCellInfo; out Text: String); virtual;
  public
    constructor Create(AOwner: TZoneContainer); override;
    destructor Destroy; override;

    procedure ClientPaint; override;
    procedure Update; override;
    function AcceptDrag(DragItem: TfcxCustomDragItem): Boolean; override;
    procedure DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer); override;

    property ActiveClientPart: TfcxSlicAxisZoneClientPart read FActiveClientPart write SetActiveClientPart;
    property AxisContainer: TfcxAxisContainer read GetAxisContainer;
    property FirstVisibleLevel: Integer read GetFirstVisibleLevel write SetFirstVisibleLevel;
    property LastVisibleLevel: Integer read GetLastVisibleLevel;
    property FirstVisibleItem: Integer read GetFirstVisibleItem write SetFirstVisibleItem;
    property LastVisibleItem: Integer read GetLastVisibleItem;
    property RequiredSize: Integer read GetRequiredSize;
    property CurrentCellInfo: TfcxCellInfo read FCurrentCellInfo write SetCurrentCellInfo;
    property ItemSize[AItemIndex: Integer]: Integer read GetItemSize;
    property CellSize[ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart]: Integer read GetCellSize write SetCellSize;
    property Cells: TfcxCellInfos read FCells;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property AutoSizeConstraint: Integer read FAutoSizeConstraint write SetAutoSizeConstraint default 0;
    property Images: TCustomImageList read FImages write SetImages;
  end;

  { TfcxSliceXAxisZone }

  TfcxSliceXAxisZone = class(TfcxSliceCustomAxisZone)
  protected
    function FindItemFromCoordinate(AItemCoordinate: Integer): Integer; override;
    function GetItemSize(AItemIndex: Integer): Integer; override;
    function DrawCell(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean; override;
    function DrawCellForScroll(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean; override;
    function CalcCellSize(Sender: TfcxAxisContainer; AInfo: TfcxCellInfo): TSize; override;
    function GetAxisContainer: TfcxAxisContainer; override;
    function GetLastVisibleItem: Integer; override;
    function GetRequiredSize: Integer; override;
    function IsResizeableFrame(AFrame: TZoneFrame): Boolean; override;
    function IsSelected(Info: TfcxCellInfo): Boolean; override;
    procedure SelectData(Info: TfcxCellInfo); override;
    function GetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart): Integer; override;
    procedure SetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart; const Value: Integer); override;
    procedure SetBoundingRect(const Value: TRect); override;
  public
    constructor Create(AOwner: TZoneContainer); override;
  end;

  { TfcxSliceYAxisZone }

  TfcxSliceYAxisZone = class(TfcxSliceCustomAxisZone)
  protected
    function FindItemFromCoordinate(AItemCoordinate: Integer): Integer; override;
    function GetItemSize(AItemIndex: Integer): Integer; override;
    function DrawCell(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean; override;
    function DrawCellForScroll(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean; override;
    procedure DrawEmptyText(ARect: TRect); override;
    function CalcCellSize(Sender: TfcxAxisContainer; AInfo: TfcxCellInfo): TSize; override;
    function GetAxisContainer: TfcxAxisContainer; override;
    function GetLastVisibleItem: Integer; override;
    function GetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart): Integer; override;
    procedure SetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart; const Value: Integer); override;
    function GetRequiredSize: Integer; override;
    function IsResizeableFrame(AFrame: TZoneFrame): Boolean; override;
    function IsSelected(Info: TfcxCellInfo): Boolean; override;
    procedure SelectData(Info: TfcxCellInfo); override;
    procedure SetBoundingRect(const Value: TRect); override;
  public
    constructor Create(AOwner: TZoneContainer); override;
  end;

  TfcxSliceItemsZoneClientPart = (
    scpiNone,
    scpiDropDownButton,
    scpiXSizing
  );

  { TfcxSliceCustomItemsZone }

  TfcxSliceCustomItemsZone = class(TfcxSliceGridZone)
  private
    FActiveIndex: Integer;
    FActiveClientPart: TfcxSliceItemsZoneClientPart;
    FDownPos: TPoint;
    FDragIndex: Integer;
    FDropDownIndex: Integer;
    FLastCursorPos: TPoint;
    FPopupWindow: TfcxPopupWindow;
    FRequiredScrollers: TZoneScrollers;
    FFirstVisibleItem: Integer;
    FPopupIndex: Integer;
    FSpacing: Integer;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FDropDownFromWholeSurface: Boolean;
    procedure GetDragIndexAt(P: TPoint; out Item: Integer; out Position: TfcxDragPosition);
    function GetIndexAt(P: TPoint): Integer;
    function GetItemRect(AIndex: Integer): TRect;
    function ClientHitTest(P: TPoint): TfcxSliceItemsZoneClientPart;
    procedure UpdateActiveClientPart(CursorPos: TPoint);
    procedure SetActiveClientPart(const Value: TfcxSliceItemsZoneClientPart);
    procedure SetActiveIndex(const Value: Integer);
    procedure InvalidateItem(AIndex: Integer);
    procedure DoPopupDestroy(Sender: TObject);
    procedure SetFirstVisibleItem(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure ImageListChange(Sender: TObject);
  protected
    procedure SetSpacing(const Value: Integer); virtual;
    function GetItemCount: Integer; virtual;
    function GetItem(AIndex: Integer): TObject; virtual;
    function GetItemThemeOptions(Item: TObject): TfcxThemeButtonOptions; virtual;
    function GetItemStyle(Item: TObject): TfcxCustomThemeStyle; virtual;
    function GetItemWidth(AIndex: Integer): Integer; virtual;
    procedure SetItemWidth(AIndex: Integer; const Value: Integer); virtual;
    function GetItemText(Item: TObject): TfcxString; virtual;
    function GetItemSortType(Item: TObject): TfcxThemeSortType; virtual;
    procedure PreparePopupMenuFor(AIndex: Integer); virtual;
    function CreatePopupWindow(Item: TObject): TfcxPopupWindow; virtual;
    function GetAllowDrag: Boolean; virtual;
    function GetEmptyText: String; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave; override;
    procedure ContextPopup(X: Integer; Y: Integer; var Handled: Boolean); override;
    procedure UpdateCursor; override;
    procedure HintInfo(P: TPoint; out HintStr: String; out HintRect: TRect); override;

    function GetRequiredScrollers: TZoneScrollers; override;
    function GetRequiredSize: Integer; virtual;
    procedure Scroll(AScroller: TZoneScroller); override;

    procedure DblClick(X, Y: Integer); override;
    procedure InitDrag(DragItem: TfcxCustomDragItem);
    procedure DrawItem(AIndex: Integer; ACanvas: TCanvas; ARect: TRect; MergeWithRegion: HRGN);
    procedure DrawScroller(ARect: TRect; AScroller: TZoneScroller; AEnabled: Boolean); override;

    procedure StartSizing(var AInfo: TSizingInfo); override;
    procedure MoveSizing(var AInfo: TSizingInfo); override;
    procedure StopSizing(var AInfo: TSizingInfo); override;

    procedure DoGetItemImageIndex(const ItemIndex: Integer; out ImageIndex: Integer); virtual;
  public
    constructor Create(AOwner: TZoneContainer); override;
    destructor Destroy; override;

    procedure ClientPaint; override;
    procedure DropDown(AIndex: Integer);
    procedure Update; override;

    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
    property ActiveClientPart: TfcxSliceItemsZoneClientPart read FActiveClientPart write SetActiveClientPart;
    property AllowDrag: Boolean read GetAllowDrag;
    property FirstVisibleItem: Integer read FFirstVisibleItem write SetFirstVisibleItem;
    property RequiredSize: Integer read GetRequiredSize;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Images: TCustomImageList read FImages write SetImages;
    property ItemCount: Integer read GetItemCount;
    property Item[AIndex: Integer]: TObject read GetItem;
    property ItemWidth[AIndex: Integer]: Integer read GetItemWidth write SetItemWidth;
  end;

  TfcxSliceItemsZone = class(TfcxSliceCustomItemsZone)
  private
    FFields: TfcxCommonFieldsOfRegion;
    FRegion: TfcxRegionOfField;
    procedure SetFields;
    procedure SetRegion(const Value: TfcxRegionOfField);
  protected
    procedure DblClick(X, Y: Integer); override;
    function GetItemCount: Integer; override;
    function GetItem(AIndex: Integer): TObject; override;
    function GetItemThemeOptions(Item: TObject): TfcxThemeButtonOptions; override;
    function GetItemStyle(Item: TObject): TfcxCustomThemeStyle; override;
    function GetItemWidth(AIndex: Integer): Integer; override;
    procedure SetItemWidth(AIndex: Integer; const Value: Integer); override;
    function GetItemText(Item: TObject): TfcxString; override;
    function GetItemSortType(Item: TObject): TfcxThemeSortType; override;
    function CreatePopupWindow(Item: TObject): TfcxPopupWindow; override;
    function GetAllowDrag: Boolean; override;
    function GetEmptyText: String; override;

    function CreatePopupMenu: TPopupMenu; override;
    procedure PreparePopupMenuFor(AIndex: Integer); override;
    procedure PopupItemClick(Sender: TObject);
    procedure SortPopupItemClick(Sender: TObject);
    procedure TotalsPopupItemClick(Sender: TObject);
    procedure AlignmentPopupItemClick(Sender: TObject);
    procedure TimeSplitPopupItemClick(Sender: TObject);
    procedure DateSplitPopupItemClick(Sender: TObject);
    procedure DateAttributeClick(Sender: TObject);
    procedure TimeAttributeClick(Sender: TObject);
  public
    constructor Create(AOwner: TZoneContainer); override;
    function AcceptDrag(DragItem: TfcxCustomDragItem): Boolean; override;
    procedure DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer); override;
    procedure FullUpdate; virtual;

    property Region: TfcxRegionOfField read FRegion write SetRegion;
  published
    property Images;
  end;

  TfcxSliceFieldsZone = class(TfcxSliceCustomItemsZone)
  protected
    function GetItemCount: Integer; override;
    function GetItem(AIndex: Integer): TObject; override;
    function GetItemThemeOptions(Item: TObject): TfcxThemeButtonOptions; override;
    function GetItemWidth(AIndex: Integer): Integer; override;
    function GetItemStyle(Item: TObject): TfcxCustomThemeStyle; override;
    function GetItemText(Item: TObject): TfcxString; override;
    function CreatePopupWindow(Item: TObject): TfcxPopupWindow; override;
  public
    function AcceptDrag(DragItem: TfcxCustomDragItem): Boolean; override;
    procedure DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer); override;
    constructor Create(AOwner: TZoneContainer); override;
  published
    property Images;
  end;

  TfcxSliceStatusZoneClientPart = (
    szpNone,
    szpTopN,
    szpScaleButton,
    szpLessScale,
    szpSlider,
    szpMoreScale
  );

  TfcxSliceStatusZone = class(TfcxSliceGridZone)
  private
    FFuncWidth: array of Integer;
    FSectionRects: array of TRect;
    FAgrFuncs: TfcxSetAgrFunc;
    FFormat: TfcxFormat;
    FHasScaleSlider: Boolean;
    FHasScale: Boolean;
    FCurrentSection: Integer;
    FActiveClientPart: TfcxSliceStatusZoneClientPart;
    FPopupWindow: TfcxPopupWindow; 
    procedure PopupItemClick(Sender: TObject);
    procedure AgrFuncItemClick(Sender: TObject);
    procedure PreparePopupMenu;
    procedure SetAgrFuncs(const Value: TfcxSetAgrFunc);
    procedure SetHasScale(const Value: Boolean);
    procedure SetHasScaleSlider(const Value: Boolean);
    procedure SetCurrentSection(const Value: Integer);
    procedure SetActiveClientPart(const Value: TfcxSliceStatusZoneClientPart);

    procedure DropDown;
    procedure DoPopupDestroy(Sender: TObject);
    procedure DoAfterCloseUp(Sender: TObject; Cancel: Boolean);
    procedure DoGetCheckState(Sender: TObject; ANode, AData: Pointer);
    procedure DoSetCheckState(Sender: TObject; ANode: Pointer; AData: Pointer);
  protected
    function CreatePopupMenu: TPopupMenu; override;
    procedure ContextPopup(X: Integer; Y: Integer; var Handled: Boolean); override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave; override;

    function GetSectionCount: Integer;
    function GetSectionWidth(AIndex: Integer): Integer;
    procedure InvalidateSection(AIndex: Integer);
    procedure UpdateActiveClientPart(CursorPos: TPoint);
    function GetSectionAt(P: TPoint): Integer;
    function ClientHitTest(P: TPoint): TfcxSliceStatusZoneClientPart;

    procedure DrawSection(AIndex: Integer);
    procedure DrawTopN(R: TRect);
    procedure DrawArgFuncs(R: TRect);
    procedure DrawScaleControls(R: TRect);

    procedure ClientPaint; override;
    procedure SelectionChanged(Sender: TZone);
    property CurrentSection: Integer read FCurrentSection write SetCurrentSection;
    property ActiveClientPart: TfcxSliceStatusZoneClientPart read FActiveClientPart write SetActiveClientPart;
  public
    constructor Create(AOwner: TZoneContainer); override;
    destructor Destroy; override;
    procedure FullUpdate;
    property AgrFuncs: TfcxSetAgrFunc read FAgrFuncs write SetAgrFuncs;
    property HasScale: Boolean read FHasScale write SetHasScale;
    property HasScaleSlider: Boolean read FHasScaleSlider write SetHasScaleSlider;
  end;

  TfcxSliceGridPointer = class
  private
    FBitmaps: array[0..1] of TBitmap;
    FActive: Boolean;
    FFullRect: Boolean;
    FOwner: TWinControl;
    FPaintRect: TRect;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure BeginDrag(ARect: TRect);
    procedure DragTo(ACanvas: TCanvas; ARect: TRect; Position: TfcxDragPosition; FullRect: Boolean);
    procedure EndDrag;
    procedure Redraw(ACanvas: TCanvas);
    procedure Restore;
    property Active: Boolean read FActive;
  end;

  TfcxSliceGridDragItem = class(TfcxCustomDragItem)
  private
    FItemIndex: Integer;
    FItemsZone: TfcxSliceCustomItemsZone;
  public
    function GetItem: TObject; override;
    procedure InitDrag(ItemBitmap, ItemMask: TBitmap; AItemsZone: TfcxSliceCustomItemsZone; AItemIndex: Integer);
    property ItemsZone: TfcxSliceCustomItemsZone read FItemsZone;
    property ItemIndex: Integer read FItemIndex;
  end;

  // event types

  // for axis cells
  TfcxSliceGridDrawAxisCellEvent = procedure(Sender: TfcxSliceCustomAxisZone; Canvas: TCanvas; State: TfcxThemeState; const Info: TfcxCellInfo) of object;
  TfcxSliceGridMeasureAxisCellEvent = procedure(Sender: TfcxSliceCustomAxisZone; Canvas: TCanvas; State: TfcxThemeState; const Info: TfcxCellInfo; GrowInHeight: Boolean; var Size: TSize) of object;
  TfcxSliceGridGetAxisCellImageIndexEvent = procedure(Sender: TfcxSliceCustomAxisZone; const AInfo: TfcxCellInfo; var ImageIndex: Integer) of object;
  TfcxSliceGridGetAxisCellTextEvent = procedure(Sender: TfcxSliceCustomAxisZone; const AInfo: TfcxCellInfo; var Text: String) of object;
  TfcxSliceGridGetAxisCellStyleEvent = procedure(Sender: TfcxSliceCustomAxisZone; const AInfo: TfcxCellInfo; State: TfcxThemeState; Style: TfcxCustomThemeStyle) of object;

  // for data cells
  TfcxSliceGridDataDblClickEvent = procedure(Sender: TfcxSliceDataZone; XAxisIndex, YAxisIndex : Integer) of Object;
  TfcxSliceGridDrawCellEvent = procedure(Sender: TfcxSliceDataZone; Canvas: TCanvas; const Rect: TRect; States: TfcxThemeCellStates; const Cell: TfcxMeasureCell) of object;
  TfcxSliceGridGetCellImageIndexEvent = procedure(Sender: TfcxSliceDataZone; const Cell: TfcxMeasureCell; var ImageIndex: Integer) of object;
  TfcxSliceGridGetCellTextEvent = procedure(Sender: TfcxSliceDataZone; const Cell: TfcxMeasureCell; var Text: String) of object;
  TfcxSliceGridGetCellStyleEvent = procedure(Sender: TfcxSliceDataZone; const Cell: TfcxMeasureCell; States: TfcxThemeCellStates; Style: TfcxCustomThemeStyle) of object;

  // for items
  TfcxSliceGridGetItemItemImageIndexEvent = procedure(Sender: TfcxSliceCustomItemsZone; const ItemIndex: Integer; var ImageIndex: Integer) of object;

  TfcxSliceGrid = class(TfcxCustomGrid)
  private
    FCaptionZone: TZone;
    FDataZone: TfcxSliceDataZone;
    FXAxisZone: TfcxSliceXAxisZone;
    FYAxisZone: TfcxSliceYAxisZone;
    FXDimsZone: TfcxSliceItemsZone;
    FYDimsZone: TfcxSliceItemsZone;
    FPageDimsZone: TfcxSliceItemsZone;
    FStatusZone: TfcxSliceStatusZone;
    FFieldsZone: TfcxSliceFieldsZone;
    FInUpdate: Boolean;
    FSlice: TfcxSlice;
    FDragPointer: TfcxSliceGridPointer;
    FDragItem: TfcxSliceGridDragItem;
    FFieldsEditor: TfcxFieldEditor;
    FOnDrawAxisCell: TfcxSliceGridDrawAxisCellEvent;
    FOnDrawDataCell: TfcxSliceGridDrawCellEvent;
    FOnGetAxisCellImageIndex: TfcxSliceGridGetAxisCellImageIndexEvent;
    FOnGetAxisCellText: TfcxSliceGridGetAxisCellTextEvent;
    FOnGetDataCellImageIndex: TfcxSliceGridGetCellImageIndexEvent;
    FOnGetDataCellStyle: TfcxSliceGridGetCellStyleEvent;
    FOnGetDataCellText: TfcxSliceGridGetCellTextEvent;
    FOnMeasureAxisCell: TfcxSliceGridMeasureAxisCellEvent;
    FOnGetAxisCellStyle: TfcxSliceGridGetAxisCellStyleEvent;
    FScale: Integer;
    FOnGetItemImageIndex: TfcxSliceGridGetItemItemImageIndexEvent;
    FOnDataDblClick: TfcxSliceGridDataDblClickEvent;
    procedure SetSlice(const Value: TfcxSlice);
    procedure InvalidateEmptyArea;
    function GetStyles: TfcxSliceGridStyles;
    procedure SetStyles(const Value: TfcxSliceGridStyles); reintroduce;
    procedure SetCaptionZone(const Value: TZone);
    procedure SetDataZone(const Value: TfcxSliceDataZone);
    procedure SetPageDimsZone(const Value: TfcxSliceItemsZone);
    procedure SetStatusZone(const Value: TfcxSliceStatusZone);
    procedure SetXAxisZone(const Value: TfcxSliceXAxisZone);
    procedure SetXDimsZone(const Value: TfcxSliceItemsZone);
    procedure SetYAxisZone(const Value: TfcxSliceYAxisZone);
    procedure SetYDimsZone(const Value: TfcxSliceItemsZone);
    procedure SetFieldsZone(const Value: TfcxSliceFieldsZone);
    procedure SetScale(const Value: Integer);
  protected
    class function GetStylesClass: TfcxCustomGridStylesClass; override;
    procedure NotifyUpdate(Sender: TZone); override;
    procedure NotifyScroll(Sender: TZone; ScrollBar: TScrollBarKind; ScrollCode: TScrollCode; ScrollPos: Integer); override;
    procedure NotifySelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetPaintStyle(const Value: TfcxPaintStyle); override;

    procedure SliceChanged(AChangeAlert: TfcxChangeAlert);
    procedure FullUpdate(AChangeType: TfcxSliceChangeType; AChanges: TfcxChangesInSlice; ARegions: TfcxRegionsOfField);
    procedure SetDefaultColWidth(const Value: Integer); override;
    procedure SetDefaultRowHeight(const Value: Integer); override;
    procedure PreparePopup(APopup: TfcxPopupWindow);
    function GetSelection: TfcxGridSelection;

    // scroll
    function GetScrollBarFor(AZone: TZone): TControl;
    function GetHorzScrollMax: Integer;
    function GetVertScrollMax: Integer;
    procedure UpdateScrolls(UpdateHorizontal, UpdateVertical: Boolean);

    // drag and drop
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetClipboardText: TfcxString; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
    procedure Paint; override;
    function DoExport(AFilter: TfcxCustomExportFilter): Boolean;

    procedure ShowFieldsEditor;
    procedure HideFieldsEditor;

    function ExecuteAction(Action: TBasicAction) : Boolean; override;
    function Rescale(const Size: Integer): Integer;

    property Scale: Integer read FScale write SetScale default 100;
  published
    property Align;
    property Font;
    property PaintStyle;
    property ParentFont;
    property Slice: TfcxSlice read FSlice write SetSlice;
    property ShowHint;
    property Styles: TfcxSliceGridStyles read GetStyles write SetStyles;
    property TabStop;
    property TabOrder;
    property OnGetClipboardText;

    // zones
    property CaptionZone: TZone read FCaptionZone write SetCaptionZone;
    property DataZone: TfcxSliceDataZone read FDataZone write SetDataZone;
    property XAxisZone: TfcxSliceXAxisZone read FXAxisZone write SetXAxisZone;
    property YAxisZone: TfcxSliceYAxisZone read FYAxisZone write SetYAxisZone;
    property XDimsZone: TfcxSliceItemsZone read FXDimsZone write SetXDimsZone;
    property YDimsZone: TfcxSliceItemsZone read FYDimsZone write SetYDimsZone;
    property PageDimsZone: TfcxSliceItemsZone read FPageDimsZone write SetPageDimsZone;
    property StatusZone: TfcxSliceStatusZone read FStatusZone write SetStatusZone;
    property FieldsZone: TfcxSliceFieldsZone read FFieldsZone write SetFieldsZone;

    // events
    property OnDrawDataCell: TfcxSliceGridDrawCellEvent read FOnDrawDataCell write FOnDrawDataCell;
    property OnDataDblClick: TfcxSliceGridDataDblClickEvent read FOnDataDblClick write FOnDataDblClick;
    property OnGetDataCellImageIndex: TfcxSliceGridGetCellImageIndexEvent read FOnGetDataCellImageIndex write FOnGetDataCellImageIndex;
    property OnGetDataCellText: TfcxSliceGridGetCellTextEvent read FOnGetDataCellText write FOnGetDataCellText;
    property OnGetDataCellStyle: TfcxSliceGridGetCellStyleEvent read FOnGetDataCellStyle write FOnGetDataCellStyle;

    property OnDrawAxisCell: TfcxSliceGridDrawAxisCellEvent read FOnDrawAxisCell write FOnDrawAxisCell;
    property OnMeasureAxisCell: TfcxSliceGridMeasureAxisCellEvent read FOnMeasureAxisCell write FOnMeasureAxisCell;
    property OnGetAxisCellImageIndex: TfcxSliceGridGetAxisCellImageIndexEvent read FOnGetAxisCellImageIndex write FOnGetAxisCellImageIndex;
    property OnGetAxisCellText: TfcxSliceGridGetAxisCellTextEvent read FOnGetAxisCellText write FOnGetAxisCellText;
    property OnGetAxisCellStyle: TfcxSliceGridGetAxisCellStyleEvent read FOnGetAxisCellStyle write FOnGetAxisCellStyle;

    property OnGetItemImageIndex: TfcxSliceGridGetItemItemImageIndexEvent read FOnGetItemImageIndex write FOnGetItemImageIndex;
  end;

implementation

uses
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  Math,
  fcxRes,
  fcxGraphicRes,
  fcxRange,
  fcxHighlights,
  fcxMeasureEditor,
  fcxDimensionEditor,
  fcxAxisEditor,
  fcxTopNDialog,
  fcxScaleDialog,
  fcxFilterPopup,
  fcxFieldsPopup,
  fcxTopNPopup,
  fcxDetailGrid,
  fcxRangeFrame;

const
  sRegionCaptions: array[TfcxRegionOfField] of String = (
 { rf_Page     } 'sFiltersRegion',
 { rf_CapXAx   } 'sColsRegion',
 { rf_CapYAx   } 'sRowsRegion',
 { rf_CapFacts } 'sFactsRegion',
 { rf_None     } ''
  );
  
type
  TfcxSliceHack = class(TfcxSlice);
  TfcxFieldsContainerHack = class(TfcxFieldsContainer);

  PfcxHighlightHookRec = ^TfcxHighlightHookRec;
  TfcxHighlightHookRec = record
    Cell: TfcxMeasureCell;
    CustomDrawnHighlights: TList;
  end;

const
  AlignmentOrder: array[TAlignment] of Integer = (0,2,1);

{ TfcxSliceGrid }

constructor TfcxSliceGrid.Create(AOwner: TComponent);
begin
  inherited;

  FScale := 100;

  Screen.Cursors[curDrag] := LoadCursor(hInstance, 'fcxdrag');
  DragCursor := curDrag;

  FInUpdate := True;

  FCaptionZone := Zones.AddZone(TfcxSliceCaptionZone);
  with FCaptionZone do
  begin
    Options := [];
    Caption := 'Caption';
  end;

  FXAxisZone := TfcxSliceXAxisZone(Zones.AddZone(TfcxSliceXAxisZone));
  with FXAxisZone do
  begin
    Options := [zoCollapsable];
    Caption := 'X Axis';
  end;

  FYAxisZone := TfcxSliceYAxisZone(Zones.AddZone(TfcxSliceYAxisZone));
  with FYAxisZone do
  begin
    Options := [zoCollapsable];
    Caption := 'Y Axis';
  end;

  FDataZone := TfcxSliceDataZone(Zones.AddZone(TfcxSliceDataZone));
  with FDataZone do
  begin
    Options := [];
    Caption := 'Data';
  end;

  FXDimsZone := TfcxSliceItemsZone(Zones.AddZone(TfcxSliceItemsZone));
  with FXDimsZone do
  begin
    Options := [zoCollapsable, zoScrollable];
    Caption := 'X Dims';
    Region := rf_CapXAx;
  end;

  FYDimsZone := TfcxSliceItemsZone(Zones.AddZone(TfcxSliceItemsZone));
  with FYDimsZone do
  begin
    Options := [zoCollapsable, zoScrollable];
    Caption := 'Y Dims';
    Region := rf_CapYAx;
  end;

  FPageDimsZone := TfcxSliceItemsZone(Zones.AddZone(TfcxSliceItemsZone));
  with FPageDimsZone do
  begin
    Options := [zoCollapsable, zoScrollable];
    Caption := 'Page Dims';
    Region := rf_Page;
  end;

  FStatusZone := TfcxSliceStatusZone(Zones.AddZone(TfcxSliceStatusZone));
  with FStatusZone do
  begin
    Options := [];
    Caption := 'Status';
  end;

  FFieldsZone := TfcxSliceFieldsZone(Zones.AddZone(TfcxSliceFieldsZone));
  with FFieldsZone do
  begin
    Options := [];
    Caption := 'Fields';
  end;

  KeyBoardZone := FDataZone;

  FInUpdate := False;

  // drag and drop
  FDragPointer := TfcxSliceGridPointer.Create(Self);
  FDragItem := TfcxSliceGridDragItem.Create(Self);
end;

procedure TfcxSliceGrid.SliceChanged(AChangeAlert: TfcxChangeAlert);
var
  CubeAlert: TfcxCubeChangeAlert absolute AChangeAlert;
  SliceAlert: TfcxSliceChangeAlert absolute AChangeAlert;
begin
  case AChangeAlert.AlertType of
    at_Cube:
      begin
        if (AChangeAlert is TfcxCubeChangeAlert) then
        begin
          if chc_Caption in CubeAlert.ChangesInCube then
            FCaptionZone.Update;
        end;
      end;
    at_Slice:
      begin
        if (AChangeAlert is TfcxSliceChangeAlert) then
        begin
          if (chs_SetCube in SliceAlert.ChangesInSlice) and Assigned(Slice.Cube) then
            Slice.Cube.ListnersManager.AddListner(Self);
          FullUpdate(SliceAlert.SliceChangeType, SliceAlert.ChangesInSlice, SliceAlert.Regions);
        end;
      end;
  else
    FullUpdate(scht_All, [], []);
  end;
end;

destructor TfcxSliceGrid.Destroy;
begin
  Slice := nil;
  FDragPointer.Free;
  FDragItem.Free;
  inherited;
end;

function TfcxSliceGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if Action.Owner = Slice then
      SliceChanged(TfcxAction(Action).ChangeAlert)
    else
    if Assigned(Slice) and (Action.Owner = Slice.Cube) then
      SliceChanged(TfcxAction(Action).ChangeAlert);
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

procedure TfcxSliceGrid.FullUpdate(AChangeType: TfcxSliceChangeType; AChanges: TfcxChangesInSlice; ARegions: TfcxRegionsOfField);
begin
  case AChangeType of
    scht_Region:
      begin
        if rf_CapXAx in ARegions then
        begin
          FXDimsZone.FullUpdate;
          FXAxisZone.FullUpdate(AChanges);
        end;
        if rf_CapYAx in ARegions then
        begin
          FYDimsZone.FullUpdate;
          FYAxisZone.FullUpdate(AChanges);
        end;
        if rf_Page in ARegions then
        begin
          FPageDimsZone.FullUpdate;
        end;
        if rf_CapFacts in ARegions then
        begin
          FDataZone.FullUpdate(AChanges);
          case Slice.MeasuresContainer.Container.Region of
            rf_CapXAx: FXDimsZone.FullUpdate;
            rf_CapYAx: FYDimsZone.FullUpdate;
            rf_Page: FPageDimsZone.FullUpdate;
          end;
        end;
        if chs_TopNChanged in AChanges then
          FStatusZone.FullUpdate;
        InvalidateEmptyArea;
      end;
    scht_Size:
      begin
        if chs_DefaultColWidth in AChanges then
          DefaultColWidth := Slice.DefaultColWidth;
        if chs_DefaultRowHeight in AChanges then
          DefaultRowHeight := Slice.DefaultRowHeight;
      end;
    else
      begin
        // update Dims zone first - others depends on their Fields
        FPageDimsZone.FullUpdate;
        FXDimsZone.FullUpdate;
        FYDimsZone.FullUpdate;
        FXAxisZone.FullUpdate(AChanges);
        FYAxisZone.FullUpdate(AChanges);
        FDataZone.FullUpdate(AChanges);
        FStatusZone.FullUpdate;
        Invalidate;
      end;
  end;
  DoChange(TfcxChangeAlert.Create(at_Slice));
end;

procedure TfcxSliceGrid.NotifyScroll(Sender: TZone; ScrollBar: TScrollBarKind; ScrollCode: TScrollCode;
  ScrollPos: Integer);
begin
  inherited;
  if Sender = FDataZone then
  begin
    // change XAxis/YAxis depends on scrollbar
    case ScrollBar of
      sbHorizontal: FXAxisZone.FirstVisibleItem := ScrollPos;
      sbVertical: FYAxisZone.FirstVisibleItem := ScrollPos;
    end;
  end;
end;

procedure TfcxSliceGrid.NotifyUpdate(Sender: TZone);

  procedure UpdateXAxisZone; forward;
  procedure UpdateYAxisZone; forward;
  procedure UpdateXDimsZone; forward;
  procedure UpdateYDimsZone; forward;
  procedure UpdatePageDimsZone; forward;

  procedure UpdateCaptionZone;
  var
    ARect: TRect;
    Sz: Integer;
  begin
    ARect := FCaptionZone.BoundingRect;
    if FCaptionZone.Visible then
      Sz := 20
    else
      Sz := 0;
    FCaptionZone.BoundingRect := Rect(0, 0, Width, Sz);
    if not EqualRect(FCaptionZone.BoundingRect, ARect) then
      UpdatePageDimsZone;
  end;

  procedure UpdateFieldsZone;
  var
    ARect: TRect;
    Sz: TSize;
  begin
    ARect := FFieldsZone.BoundingRect;
    if FFieldsZone.Visible then
    begin
      Sz.cx := Min(DefaultColWidth, FYDimsZone.Width);
      Sz.cy := FFieldsZone.RequiredSize;
    end
    else
    begin
      Sz.cx := 0;
      Sz.cy := 0;
    end;
    FFieldsZone.BoundingRect := Rect(0, FPageDimsZone.BoundingRect.Bottom, Sz.cx, FPageDimsZone.BoundingRect.Bottom + Sz.cy);
    if not EqualRect(FFieldsZone.BoundingRect, ARect) then
    begin
      UpdateYDimsZone;
      UpdateXAxisZone;
    end;
  end;

  procedure UpdatePageDimsZone;
  var
    ARect: TRect;
    Sz: Integer;
  begin
    ARect := FPageDimsZone.BoundingRect;
    if FPageDimsZone.Visible then
      Sz := FPageDimsZone.RequiredSize
    else
      Sz := 0;
    FPageDimsZone.BoundingRect := Rect(0, FCaptionZone.BoundingRect.Bottom, Width, FCaptionZone.BoundingRect.Bottom + Sz);
    if not EqualRect(FPageDimsZone.BoundingRect, ARect) then
    begin
      UpdateXDimsZone;
      UpdateFieldsZone;
    end;
  end;

  procedure UpdateXDimsZone;
  var
    ARect: TRect;
    Sz: Integer;
  begin
    ARect := FXDimsZone.BoundingRect;
    if FXDimsZone.Visible then
      Sz := FXDimsZone.RequiredSize
    else
      Sz := 0;
    FXDimsZone.BoundingRect := Rect(FYDimsZone.Width, FPageDimsZone.BoundingRect.Bottom, FDataZone.BoundingRect.Right, FPageDimsZone.BoundingRect.Bottom + Sz);
    if not EqualRect(FXDimsZone.BoundingRect, ARect) then
    begin
      UpdateYDimsZone;
      UpdateXAxisZone;
    end;
  end;

  procedure UpdateYDimsZone;
  var
    ARect: TRect;
    Top, Sz: Integer;
  begin
    ARect := FYDimsZone.BoundingRect;
    Sz := FYDimsZone.RequiredSize;
    Top := FXAxisZone.BoundingRect.Bottom - Sz;
    if not FYDimsZone.Visible then
      Sz := 0;
    FYDimsZone.BoundingRect := Rect(0, Top, FYAxisZone.RequiredSize, Top + Sz);
    if not EqualRect(FYDimsZone.BoundingRect, ARect) then
    begin
      UpdateXDimsZone;
      UpdateYAxisZone;
      UpdateFieldsZone;
    end;
  end;

  procedure UpdateDataZone;
  var
    ARect: TRect;
    Pt: TPoint;
    Sz: TSize;
  begin
    ARect := FDataZone.BoundingRect;
    Pt.X := FYAxisZone.BoundingRect.Right;
    Pt.Y := FXAxisZone.BoundingRect.Bottom;
    if FDataZone.Visible then
    begin
      Sz.cx := Width - Pt.X;
      if Assigned(FDataZone.VertScroll) then
        Sz.cx := Sz.cx - FDataZone.VertScroll.Width;
      Sz.cy := FStatusZone.BoundingRect.Top - Pt.Y;
      if Assigned(FDataZone.HorzScroll) then
        Sz.cy := Sz.cy - FDataZone.HorzScroll.Height;
    end
    else
    begin
      Sz.cx := 0;
      Sz.cy := 0;
    end;
    FDataZone.BoundingRect := Rect(Pt.X, Pt.Y, Pt.X + Sz.cx, Pt.Y + Sz.cy);
    if not EqualRect(FDataZone.BoundingRect, ARect) then
    begin
      UpdateXAxisZone;
      UpdateYAxisZone;
    end;  
  end;

  procedure UpdateStatusZone;
  var
    ARect: TRect;
    Sz: Integer;
  begin
    ARect := FStatusZone.BoundingRect;
    if FStatusZone.Visible then
      Sz := 21
    else
      Sz := 0;
    FStatusZone.BoundingRect := Rect(0, Height - Sz, Width, Height);
    if not EqualRect(FStatusZone.BoundingRect, ARect) then
      UpdateDataZone;
   end;

  procedure UpdateXAxisZone;
  var
    ARect: TRect;
    Sz: Integer;
  begin
    ARect := FXAxisZone.BoundingRect;
    if FXAxisZone.Visible then
    begin
      Sz := FXAxisZone.RequiredSize;
      if FYDimsZone.Visible then
        Sz := Max(Sz, FYDimsZone.RequiredSize);
    end
    else
      Sz := 0;
    FXAxisZone.BoundingRect := Rect(FYAxisZone.Width, FXDimsZone.BoundingRect.Bottom, FDataZone.BoundingRect.Right, FXDimsZone.BoundingRect.Bottom + Sz);
    if not EqualRect(FXAxisZone.BoundingRect, ARect) then
    begin
      UpdateDataZone;
      UpdateYDimsZone;
    end;
  end;

  procedure UpdateYAxisZone;
  var
    ARect: TRect;
    Sz: TSize;
  begin
    ARect := FYAxisZone.BoundingRect;
    if FYAxisZone.Visible then
    begin
      Sz.cx := FYAxisZone.RequiredSize;
      if FDataZone.Visible then
        Sz.cy := FDataZone.BoundingRect.Bottom
      else
        Sz.cy := FStatusZone.BoundingRect.Top;
      Dec(Sz.cy, FYDimsZone.BoundingRect.Bottom);
    end
    else
    begin
      Sz.cx := 0;
      Sz.cy := 0;
    end;

    FYAxisZone.BoundingRect := Rect(0, FYDimsZone.BoundingRect.Bottom, Sz.cx, FYDimsZone.BoundingRect.Bottom + Sz.cy);
    if not EqualRect(FYAxisZone.BoundingRect, ARect) then
    begin
      UpdateDataZone;
      UpdateXAxisZone;
    end;
  end;

begin
  inherited;

  if FInUpdate then
    Exit;

  FInUpdate := True;

  if Sender = FCaptionZone then
  begin
    UpdateCaptionZone;
  end
  else
  if Sender = FStatusZone then
  begin
    UpdateStatusZone;
  end
  else
  if Sender = FFieldsZone then
  begin
    UpdateFieldsZone;
  end
  else
  if Sender = FPageDimsZone then
    UpdatePageDimsZone
  else
  if Sender = FXDimsZone then
    UpdateXDimsZone
  else
  if Sender = FYDimsZone then
    UpdateYDimsZone
  else
  if Sender = FXAxisZone then
    UpdateXAxisZone
  else
  if Sender = FYAxisZone then
    UpdateYAxisZone
  else
  if Sender = FDataZone then
    UpdateDataZone;

  FInUpdate := False;
end;

procedure TfcxSliceGrid.SetSlice(const Value: TfcxSlice);
begin
  if FSlice <> Value then
  begin
    if FSlice <> nil then
    begin
      FSlice.ListnersManager.RemoveListner(Self);
      FSlice.RemoveFreeNotification(Self);
      if Assigned(FSlice.Cube) then
        FSlice.Cube.ListnersManager.RemoveListner(Self);
    end;
    FSlice := Value;
    FullUpdate(scht_All, [], []);
    if FSlice <> nil then
    begin
      FSlice.FreeNotification(Self);
      FSlice.ListnersManager.AddListner(Self);
      if Assigned(FSlice.Cube) then
        FSlice.Cube.ListnersManager.AddListner(Self);
      TfcxSliceHack(FSlice).DefaultColWidth := DefaultColWidth;
      TfcxSliceHack(FSlice).DefaultRowHeight := DefaultRowHeight;
    end;
  end;
end;

procedure TfcxSliceGrid.SetDefaultColWidth(const Value: Integer);
begin
  inherited;
  if Assigned(FSlice) then
    if TfcxSliceHack(FSlice).DefaultColWidth <> DefaultColWidth then
      TfcxSliceHack(FSlice).DefaultColWidth := DefaultColWidth;
end;

procedure TfcxSliceGrid.SetDefaultRowHeight(const Value: Integer);
begin
  inherited;
  if Assigned(FSlice) then
    if TfcxSliceHack(FSlice).DefaultRowHeight <> DefaultRowHeight then
      TfcxSliceHack(FSlice).DefaultRowHeight := DefaultRowHeight;
end;

procedure TfcxSliceGrid.InvalidateEmptyArea;
var
  R: TRect;
begin
  // there are 5 areas which have no zones
  // 1 - between the page and y fiels zone, fields and x axis zone
  R.Top := FPageDimsZone.BoundingRect.Bottom;
  R.Left := FFieldsZone.BoundingRect.Right;
  R.Bottom := FYDimsZone.BoundingRect.Top;
  R.Right := FXDimsZone.BoundingRect.Left;
  InvalidateRect(Handle, @R, False);

  // 2 - between fields and x fields zone
  R.Top := FFieldsZone.BoundingRect.Bottom;
  R.Left := FYDimsZone.BoundingRect.Left;
  R.Bottom := FYDimsZone.BoundingRect.Top;
  R.Right := FDataZone.BoundingRect.Left;
  InvalidateRect(Handle, @R, False);

  // 3 - between the page and data zone
  R.Top := FPageDimsZone.BoundingRect.Bottom;
  R.Bottom := FDataZone.BoundingRect.Top;
  R.Left := FXAxisZone.BoundingRect.Right;
  R.Right := Width;
  InvalidateRect(Handle, @R, False);
  // 4 - under the y axis zone
  R.Top := FYAxisZone.BoundingRect.Bottom;
  R.Left := FYAxisZone.BoundingRect.Left;
  R.Right := FDataZone.BoundingRect.Left;
  R.Bottom := FStatusZone.BoundingRect.Top;
  InvalidateRect(Handle, @R, False);
  // 5 - between scrolls
  if Assigned(FDataZone.VertScroll) and Assigned(FDataZone.HorzScroll) then
  begin
    R.Left := FDataZone.HorzScroll.Left + FDataZone.HorzScroll.Width;
    R.Right := ClientWidth;
    R.Top := FDataZone.VertScroll.Top + FDataZone.VertScroll.Height;
    R.Bottom := ClientHeight;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TfcxSliceGrid.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  FDragPointer.EndDrag;
end;

procedure TfcxSliceGrid.DoStartDrag(var DragObject: TDragObject);
begin
  inherited DoStartDrag(DragObject);

  if LastMouseZone is TfcxSliceItemsZone then
  begin
    TfcxSliceItemsZone(LastMouseZone).InitDrag(FDragItem);
    DragObject := FDragItem;
  end;
end;

procedure TfcxSliceGrid.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Zone: TZone;
  R: TRect;
  P: TPoint;
  I: Integer;
  Position: TfcxDragPosition;
  FullRect: Boolean;
begin
  inherited;
  Zone := Zones.GetZoneAt(X, Y);
  Accept := (Source is TfcxCustomDragItem) and
    (Zone is TfcxGridZone) and TfcxGridZone(Zone).AcceptDrag(TfcxCustomDragItem(Source));
  if Accept then
  begin
    if Zone is TfcxSliceItemsZone then
    begin
      P := Point(X - Zone.Left, Y - Zone.Top);
      TfcxSliceItemsZone(Zone).GetDragIndexAt(P, I, Position);
      FullRect := I = -1;
      if not FullRect then
      begin
        R := TfcxSliceItemsZone(Zone).GetItemRect(I);
        OffsetRect(R, Zone.Left + Zone.FrameRect.Left, Zone.Top + Zone.FrameRect.Top);
      end
      else
        R := Zone.ClientBounds;
    end
    else
    begin
      R := Zone.ClientBounds;
      FullRect := True;
    end;
    if TfcxCustomDragItem(Source).Control <> Self then
      TfcxCustomDragItem(Source).EndDragNotification(Self);
    FDragPointer.DragTo(Canvas, R, Position, FullRect);
  end
  else
    FDragPointer.EndDrag;
end;

procedure TfcxSliceGrid.Paint;
begin
  if FDragItem.InDrag then
    FDragItem.GetDragImages.HideDragImage;
  Canvas.Font.Color := clBlack;
  Canvas.Brush.Color := clWindow;
  inherited;
  if (FDragPointer.Active) then
    FDragPointer.Redraw(Canvas);
  if FDragItem.InDrag then
    FDragItem.GetDragImages.ShowDragImage;
end;

procedure TfcxSliceGrid.DragDrop(Source: TObject; X, Y: Integer);
var
  Zone: TZone;
begin
  inherited;
  Zone := Zones.GetZoneAt(X, Y);
  if (Source is TfcxCustomDragItem) and (Zone is TfcxGridZone) then
    TfcxGridZone(Zone).DragDrop(TfcxCustomDragItem(Source), X - Zone.Left, Y - Zone.Top);
end;

class function TfcxSliceGrid.GetStylesClass: TfcxCustomGridStylesClass;
begin
  Result := TfcxSliceGridStyles;
end;

function TfcxSliceGrid.GetStyles: TfcxSliceGridStyles;
begin
  Result := TfcxSliceGridStyles(inherited Styles)
end;

procedure TfcxSliceGrid.SetStyles(const Value: TfcxSliceGridStyles);
begin
  inherited SetStyles(Value);
end;

procedure TfcxSliceGrid.PreparePopup(APopup: TfcxPopupWindow);
begin
  APopup.Font := Font;
  APopup.PaintStyle := PaintStyle;
  APopup.Styles.Footer := Styles.HeaderArea;
  APopup.Styles.DataItem := Styles.DataCells;
  APopup.Styles.DataItemSelected := Styles.DataCellsSelected;
  APopup.ShowHint := ShowHint;
end;

function TfcxSliceGrid.GetScrollBarFor(AZone: TZone): TControl;
begin
  if AZone = FXAxisZone then
    Result := FDataZone.HorzScroll
  else
  if AZone = FYAxisZone then
    Result := FDataZone.VertScroll
  else
    Result := nil
end;

function TfcxSliceGrid.GetSelection: TfcxGridSelection;
begin
  Result := FDataZone.FSelection;
end;

procedure TfcxSliceGrid.NotifySelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection);
begin
  inherited;
  FDataZone.SelectionChanged(Sender);
  FXAxisZone.SelectionChanged(Sender);
  FYAxisZone.SelectionChanged(Sender);
  FStatusZone.SelectionChanged(Sender);
end;

procedure TfcxSliceGrid.HideFieldsEditor;
begin
  if Assigned(FFieldsEditor) then
    FFieldsEditor.Hide;
end;

procedure TfcxSliceGrid.ShowFieldsEditor;
begin
  if not Assigned(FFieldsEditor) then
  begin
    FFieldsEditor := TfcxFieldEditor.Create(Self);
    FFieldsEditor.Slice := Slice;
  end;
  FFieldsEditor.Show;
end;

procedure TfcxSliceGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent = Slice then
      Slice := nil;
    if AComponent = DataZone.Images then
      DataZone.Images := nil;
    if AComponent = XAxisZone.Images then
      XAxisZone.Images := nil;
    if AComponent = YAxisZone.Images then
      YAxisZone.Images := nil;
  end;
end;

procedure TfcxSliceGrid.UpdateScrolls(UpdateHorizontal, UpdateVertical: Boolean);
begin
  DataZone.UpdateScrolls(UpdateVertical, UpdateHorizontal);
end;

procedure TfcxSliceGrid.SetCaptionZone(const Value: TZone);
begin
  FCaptionZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetDataZone(const Value: TfcxSliceDataZone);
begin
  FDataZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetPageDimsZone(const Value: TfcxSliceItemsZone);
begin
  FPageDimsZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetStatusZone(const Value: TfcxSliceStatusZone);
begin
  FStatusZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetXAxisZone(const Value: TfcxSliceXAxisZone);
begin
  FXAxisZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetXDimsZone(const Value: TfcxSliceItemsZone);
begin
  FXDimsZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetYAxisZone(const Value: TfcxSliceYAxisZone);
begin
  FYAxisZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetYDimsZone(const Value: TfcxSliceItemsZone);
begin
  FYDimsZone.Assign(Value);
end;

procedure TfcxSliceGrid.SetPaintStyle(const Value: TfcxPaintStyle);
begin
  inherited;
  FPageDimsZone.Update;
  FXDimsZone.Update;
  FYDimsZone.Update;
  FFieldsZone.Update;
end;

procedure TfcxSliceGrid.SetScale(const Value: Integer);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    XAxisZone.Update;
    YAxisZone.Update;
    DataZone.Update;
    Invalidate;
  end;
end;

procedure TfcxSliceGrid.SetFieldsZone(const Value: TfcxSliceFieldsZone);
begin
  FFieldsZone.Assign(Value);
end;

function TfcxSliceGrid.GetHorzScrollMax: Integer;
begin
  Result := XAxisZone.GetScrollMax(DataZone.IneritedGetHorzScrollMax);
end;

function TfcxSliceGrid.GetVertScrollMax: Integer;
begin
  Result := YAxisZone.GetScrollMax(DataZone.IneritedGetVertScrollMax);
end;

function TfcxSliceGrid.Rescale(const Size: Integer): Integer;
begin
  Result := MulDiv(Size, Scale, 100);
end;

function TfcxSliceGrid.DoExport(AFilter: TfcxCustomExportFilter): Boolean;
begin
  Result := False;

  if AFilter = nil then
    Exit;

  AFilter.Slice := Slice;
  AFilter.Styles := Styles;
  Result := AFilter.PerformExport;
end;

function TfcxSliceGrid.GetClipboardText: TfcxString;
begin
  with GetSelection do
    Result := Slice.GetAsPlainText(Rect.Top, Rect.Bottom, Rect.Left, Rect.Right, False);
end;

{ TfcxSliceDataZone }

procedure TfcxSliceDataZone.ClientPaint;
var
  Slice: TfcxSlice;
  ACol, ARow: integer;
  CR, ARect: TRect;
  CellStates: TfcxThemeCellStates;
  Cell: TfcxMeasureCell;
  RGN: HRGN;
  RgnType: Integer;

  function ExcludeRect(const ARect: TRect): Integer;
  var
    TmpRgn: HRGN;
  begin
    TmpRgn := CreateRectRgnIndirect(ARect);
    Result := CombineRgn(Rgn, Rgn, TmpRgn, RGN_DIFF);
    DeleteObject(TmpRgn);
  end;

begin
  CR := ClientRect;

  Slice := Grid.Slice;
  if Slice = nil then
  begin
    Grid.Painter.DrawBody(Canvas, CR, Grid.Styles.DataArea);
    Exit;
  end;

  Rgn := CreateRectRgnIndirect(CR);
  RgnType := SIMPLEREGION;
  ARect := CR;
  for ARow := FirstVisibleRow to LastVisibleRow do
  begin
    ARect.Left := CR.Left;
    ARect.Bottom := ARect.Top + RowHeight[ARow];
    for ACol := FirstVisibleCol to LastVisibleCol do
    begin
      ARect.Right := ARect.Left + ColWidth[ACol];
      Slice.GetMeasureCell(ACol, ARow, Cell);
      CellStates := [];
      if Cell.IsTotal then
        Include(CellStates, tcsTotal);
      if CellFocused(ACol, ARow) then
        Include(CellStates, tcsFocused);
      if CellSelected(ACol, ARow) then
        Include(CellStates, tcsSelected);
      DoDrawCell(Cell, CellStates, ARect);
      RgnType := ExcludeRect(ARect);
      if RgnType = NULLREGION then
        Break;
      ARect.Left := ARect.Right;
    end;
    ARect.Top := ARect.Bottom;
  end;
  if RgnType <> NULLREGION then
  begin
    with ClientBounds do
    begin
      OffsetRgn(Rgn, Left, Top);
      SelectClipRgn(Canvas.Handle, Rgn);
    end;
    Grid.Painter.DrawBody(Canvas, CR, Grid.Styles.DataArea);
    SelectClipRgn(Canvas.Handle, 0);
  end;
  DeleteObject(Rgn);
  if (Slice.MeasuresContainer.VisibleCount = 0) or not (Slice.MeasuresContainer.Container.Region in [rf_CapXAx, rf_CapYAx]) then
  begin
    Canvas.Font.Assign(Grid.Styles.HeaderArea.Font);
    Canvas.Font.Color := Grid.Styles.HeaderArea.TextColor;
    Grid.Painter.DrawText(Canvas, CR, fcxResources.Get(sRegionCaptions[rf_CapFacts]), DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

function TfcxSliceDataZone.GetColCount: Integer;
begin
  if Assigned(Grid.Slice) then
    Result := Grid.Slice.ColCount
  else
    Result := 0;
end;

function TfcxSliceDataZone.GetRowCount: Integer;
begin
  if Assigned(Grid.Slice) then
    Result := Grid.Slice.RowCount
  else
    Result := 0;
end;

function TfcxSliceDataZone.GetColWidth(AIndex: Integer): Integer;
begin
  if Assigned(Grid.FSlice) then
    Result := Grid.XAxisZone.ItemSize[AIndex]
  else
    Result := Grid.Rescale(Grid.DefaultColWidth)
end;

function TfcxSliceDataZone.GetRowHeight(AIndex: Integer): Integer;
begin
  if Assigned(Grid.FSlice) then
    Result := Grid.YAxisZone.ItemSize[AIndex]
  else
    Result := Grid.Rescale(Grid.DefaultRowHeight)
end;

procedure TfcxSliceDataZone.SetColWidth(AIndex: Integer; AWidth: Integer);
begin
  AWidth := MulDiv(AWidth, 100, Grid.Scale);
  if Assigned(Grid.FSlice) then
    Grid.FSlice.ColWidth[AIndex] := AWidth;
  inherited;
end;

procedure TfcxSliceDataZone.SetRowHeight(AIndex: Integer; AHeight: Integer);
begin
  AHeight := MulDiv(AHeight, 100, Grid.Scale);
  if Assigned(Grid.FSlice) then
    Grid.FSlice.RowHeight[AIndex] := AHeight;
  inherited;
end;

function TfcxSliceDataZone.IsResizeableFrame(AFrame: TZoneFrame): Boolean;
begin
  Result := AFrame in [zpLeftFrame, zpTopFrame];
end;

procedure TfcxSliceDataZone.FullUpdate(AChanges: TfcxChangesInSlice);

  procedure SwapDataPoint(var P: TfcxDataPoint);
  begin
    P.x := P.x xor P.y;
    P.y := P.x xor P.y;
    P.x := P.x xor P.y;
  end;

begin
  UpdateScrolls(True, True);

  Selection_BeginUpdate;
  if chs_Load in AChanges then
  begin
    // apply loaded selection
    // Grid.Slice.SelectedCol, Grid.Slice.SelectedRow ?
    SelectCell(TfcxSliceHack(Grid.Slice).SelectedCol, TfcxSliceHack(Grid.Slice).SelectedRow, True);
  end
  else
  if chs_ChangeAxis in AChanges then
  begin
    // swap selection
    SwapDataPoint(FSelection.Rect.LeftTop);
    SwapDataPoint(FSelection.Rect.BottomRight);
    SwapDataPoint(FSelection.Start);
    SwapDataPoint(FSelection.Last);
  end
  else
  begin
    FSelection.Rect.Bottom := Min(FSelection.Rect.Bottom, GetRowCount - 1);
    FSelection.Rect.Right := Min(FSelection.Rect.Right, GetColCount - 1);
  end;
  Selection_EndUpdate;

  Invalidate;
end;

procedure TfcxSliceDataZone.DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer);
var
  AItem: TObject;
begin
  if Assigned(Grid.FSlice) then
  begin
    AItem := DragItem.GetItem;
    if (AItem = CounterPtr) then
      Grid.Slice.MeasuresContainer.AddMeasure(nil, '', fcxResources.Get('SCounterItem'), af_Count)
    else
    begin
      if (AItem is TfcxMeasureField) then
        AItem := TfcxMeasureField(AItem).SliceField
      else
      if (AItem is TfcxAxisField) then
        AItem := TfcxAxisField(AItem).SliceField;
      if AItem is TfcxSliceField then
        Grid.Slice.MeasuresContainer.AddMeasure(TfcxSliceField(AItem), TfcxSliceField(AItem).FieldName, TfcxSliceField(AItem).Caption, af_Sum);
    end;
  end;
end;

function TfcxSliceDataZone.GetGrid: TfcxSliceGrid;
begin
  Result := TfcxSliceGrid(Owner)
end;

procedure TfcxSliceDataZone.ContextPopup(X: Integer; Y: Integer; var Handled: Boolean);
var
  Pt: TPoint;
begin
  Handled := True;
  PopupMenuNeeded;
  Pt := Point(X, Y);
  if (Pt.X = -1) and (Pt.Y = -1) then
  begin
    PreparePopupMenuFor(SelectedArea.Start);
    Pt := DataPointToRect(SelectedArea.Start).BottomRight;
  end
  else
    PreparePopupMenuFor(PointToDataPoint(Pt));
  with ClientToScreen(Pt) do
    PopupMenu.Popup(X, Y);
end;

function TfcxSliceDataZone.CreatePopupMenu: TPopupMenu;
const
  Items: array[0..10] of String = (
    'sAlignment',
    'sDisplayAs',
    'sSelect',
    cLineCaption,
    'sCopy',
    cLineCaption,
    'sSortByYSelection',
    'sSortByXSelection',
    cLineCaption,
    'sDeleteMeasure',
    'sProperties'
  );
  AlignmentItems: array[0..2] of String = (
  { taLeftJustify  } 'sLeftJustify',
  { taCenter       } 'sCenter',
  { taRightJustify } 'sRightJustify'
  );
  AlignmentIcons: array[0..2] of Integer = (14, 15, 16);
  SelectItems: array[0..2] of String = (
   'sSelectRow',
   'sSelectCol',
   'sSelectAll'
  );
  DisplayAsIcons: array[TfcxDisplayAs] of integer = (
 { da_Value             } 17,
 { da_RowPercentTotal   } 18,
 { da_ColPercentTotal   } 19,
 { da_RowPercentLevel   } 18,
 { da_ColPercentLevel   } 19,
 { da_GrandTotalPercent } 20,
 { da_RowRank           } 21,
 { da_ColRank           } 22
  );
  DisplayAsItems: array[TfcxDisplayAs] of String = (
 { da_Value             } 'sOriginalValue',
 { da_RowPercentTotal   } 'sRowPercentTotal',
 { da_ColPercentTotal   } 'sColPercentTotal',
 { da_RowPercentLevel   } 'sRowPercentGroup',
 { da_ColPercentLevel   } 'sColPercentGroup',
 { da_GrandTotalPercent } 'sGrandTotalPercent',
 { da_RowRank           } 'sRowRank',
 { da_ColRank           } 'sColRank'
  );
var
  I: Integer;
  DA: TfcxDisplayAs;
  Item: TMenuItem;
begin
  Result := inherited CreatePopupMenu;
  Result.Images := fcxGraphicResources.ToolImages;
  SetLength(FOwnMenuItems, Length(Items));
  for I := Low(Items) to High(Items) do
  begin
    FOwnMenuItems[I] := NewItem(fcxResources.Get(Items[I]), 0, False, True, PopupItemClick, 0, '');
    Result.Items.Add(FOwnMenuItems[I]);
  end;
  FOwnMenuItems[4].ImageIndex := 24;
  FOwnMenuItems[0].SubMenuImages := fcxGraphicResources.ToolImages;
  for I := Low(AlignmentItems) to High(AlignmentItems) do
  begin
    Item := NewItem(fcxResources.Get(AlignmentItems[I]), 0, False, True, AlignmentPopupItemClick, 0, '');
    Item.ImageIndex := AlignmentIcons[I];
    Item.RadioItem := True;
    FOwnMenuItems[0].Add(Item);
  end;
  Result.Items[1].SubMenuImages := fcxGraphicResources.ToolImages;
  for DA := Low(DisplayAsItems) to High(DisplayAsItems) do
  begin
    Item := NewItem(fcxResources.Get(DisplayAsItems[DA]), 0, False, True, DisplayAsPopupItemClick, 0, '');
    Item.ImageIndex := DisplayAsIcons[DA];
    Item.RadioItem := True;
    FOwnMenuItems[1].Add(Item);
  end;
  for I := Low(SelectItems) to High(SelectItems) do
  begin
    Item := NewItem(fcxResources.Get(SelectItems[I]), 0, False, True, SelectPopupItemClick, 0, '');
    FOwnMenuItems[2].Add(Item);
  end;
end;

procedure TfcxSliceDataZone.PreparePopupMenuFor(APoint: TfcxDataPoint);
var
  I, J: Integer;
begin
  if Assigned(Grid.Slice) then
    Grid.Slice.GetMeasureCell(APoint.x, APoint.y, FPopupCell)
  else
    FPopupCell.MeasureIndex := -1;
  for I := 0 to High(FOwnMenuItems) do
  begin
    for J := 0 to FOwnMenuItems[I].Count - 1 do
      FOwnMenuItems[I][J].Checked := False;
    FOwnMenuItems[I].Visible := PopupCell.MeasureIndex <> -1;
  end;
  if PopupCell.MeasureIndex <> -1 then
  begin
    FOwnMenuItems[0].Items[AlignmentOrder[Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex].Alignment]].Checked := True;
    FOwnMenuItems[1].Items[Ord(Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex].DisplayAs)].Checked := True;
  end;
end;

procedure TfcxSliceDataZone.PopupItemClick(Sender: TObject);
begin
  case GetOwnMenuItemIndex(Sender) of
    4: Grid.CopyToClipboard;
    6: Grid.Slice.YAxisContainer.DefaultTypeSort := md_tsa_BySelected;
    7: Grid.Slice.XAxisContainer.DefaultTypeSort := md_tsa_BySelected;
    9: Grid.Slice.MeasuresContainer.DeleteMeasure(PopupCell.MeasureIndex);
    10: with TfcxMeasureEditorForm.Create(Grid) do
         Execute(Grid.Slice, Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex]);
  end;
end;

procedure TfcxSliceDataZone.AlignmentPopupItemClick(Sender: TObject);
begin
  case (Sender as TMenuItem).MenuIndex of
    0: Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex].Alignment := taLeftJustify;
    1: Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex].Alignment := taCenter;
    2: Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex].Alignment := taRightJustify;
  end;
  Invalidate(False);
end;

procedure TfcxSliceDataZone.SelectPopupItemClick(Sender: TObject);
begin
  case (Sender as TMenuItem).MenuIndex of
    0: SelectRow(FSelection.Rect.Top);
    1: SelectCol(FSelection.Rect.Left);
    2: SelectAll;
  end;
end;

procedure TfcxSliceDataZone.DisplayAsPopupItemClick(Sender: TObject);
begin
  Grid.Slice.MeasuresContainer.Measures[PopupCell.MeasureIndex].DisplayAs := TfcxDisplayAs((Sender as TMenuItem).MenuIndex);
end;

function TfcxSliceDataZone.GetSelectedMeasure: Integer;
var
  Cell: TfcxMeasureCell;
begin
  Grid.Slice.GetMeasureCell(FSelection.Start.x, FSelection.Start.y, Cell);
  Result := Cell.MeasureIndex;
end;

function TfcxSliceDataZone.AcceptDrag(DragItem: TfcxCustomDragItem): Boolean;
var
  Item: TObject;
begin
  Item := DragItem.GetItem;
  Result :=
    (Item is TfcxSliceField) or
    (Item is TfcxAxisField) or
    (Item is TfcxMeasureField) or
    (Item = CounterPtr);
end;

procedure TfcxSliceDataZone.DoGetCellStyle(const ACell: TfcxMeasureCell; States: TfcxThemeCellStates; Style: TfcxCustomThemeStyle);
begin
{$IFDEF TRIAL}
  if ACell.IsTrial then
  begin
    if [tcsSelected, tcsFocused] * States = [tcsSelected] then
      Style.Assign(Grid.Styles.DataCellsSelected)
    else
    begin
      Style.Assign(Grid.Styles.DataCells);
      Style.FillColor := clWindow;
      Style.GradientDirection := tgdNone;
    end;
    Style.TextColor := clRed;
    Exit;
  end;
{$ENDIF}
  if [tcsSelected, tcsFocused] * States = [tcsSelected] then
    Style.Assign(Grid.Styles.DataCellsSelected)
  else
  begin
    if tcsTotal in States then
      Style.Assign(Grid.Styles.DataCellsTotals)
    else
      Style.Assign(Grid.Styles.DataCells);
  end;
  if Assigned(Grid.OnGetDataCellStyle) then
    Grid.OnGetDataCellStyle(Self, ACell, States, Style);
end;

procedure TfcxSliceDataZone.DoGetCellText(const ACell: TfcxMeasureCell; out Text: String);
begin
  Text := StringToControl(ACell.StrValue);
  if Assigned(Grid.OnGetDataCellText) then
    Grid.OnGetDataCellText(Self, ACell, Text);
end;

procedure TfcxSliceDataZone.DoGetCellImageIndex(const ACell: TfcxMeasureCell; out ImageIndex: Integer);
begin
  ImageIndex := -1;
  if Assigned(Grid.OnGetDataCellImageIndex) then
    Grid.OnGetDataCellImageIndex(Self, ACell, ImageIndex);
end;

procedure TfcxSliceDataZone.DoDrawHighlights(AHookData: Pointer; APainter: TfcxCustomPainter;
  ACanvas: TCanvas; var ARect: TRect; var CanDrawImage, CanDrawText: Boolean);
var
  HighlightHookRec: PfcxHighlightHookRec absolute AHookData;
  I: Integer;
begin
  if not Assigned(HighlightHookRec^.CustomDrawnHighlights) then
    Exit;
  for I := 0 to HighlightHookRec^.CustomDrawnHighlights.Count - 1 do
    if TVarData(HighlightHookRec.Cell.Value).VType > 1 then
      TfcxGraphicHighlight(HighlightHookRec^.CustomDrawnHighlights[I]).DrawValue(ACanvas, Grid.Scale, ARect, @HighlightHookRec^.Cell, CanDrawImage, CanDrawText);
end;

procedure TfcxSliceDataZone.DoDrawCell(const ACell: TfcxMeasureCell; AStates: TfcxThemeCellStates; ARect: TRect);
var
  Text: String;
  ImageIndex: Integer;
  ApplyTo: TfcxHighlightApplyToEnum;
  Highlights: TfcxCustomHighlights;
  Highlight: TfcxCustomHighlight;
  HighlightHookRec: TfcxHighlightHookRec;
  I: Integer;
begin
  if Assigned(Grid.OnDrawDataCell) then
    Grid.OnDrawDataCell(Self, Canvas, ARect, AStates, ACell)
  else
  begin
    HighlightHookRec.Cell := ACell;
    HighlightHookRec.CustomDrawnHighlights := nil;
    try
      DoGetCellText(ACell, Text);
      DoGetCellStyle(ACell, AStates, FDrawStyle);
      // merge styles with highlights and populate the custom drawn highligh list
      if ACell.MeasureIndex >= 0 then
      begin
        Highlights := Grid.Slice.MeasuresContainer.Measures[ACell.MeasureIndex].Highlights;
        if Highlights.Count > 0 then
        begin
          if ACell.IsGrandTotal then
            ApplyTo := hatGrandTotal
          else
          if ACell.IsTotal then
            ApplyTo := hatTotals
          else
            ApplyTo := hatCells;
          HighlightHookRec.CustomDrawnHighlights := TList.Create;
          for I := 0 to Highlights.Count - 1 do
          begin
            Highlight := Highlights[I];
            if Highlight.AcceptValue(ACell.Value) and (ApplyTo in Highlight.ApplyTo) then
            begin
              if Highlight.CustomDrawn then
                HighlightHookRec.CustomDrawnHighlights.Add(Highlight)
              else
                FDrawStyle.MergeWith(TfcxGraphicHighlight(Highlight).GetStyleFor(@ACell));
            end;
          end;
        end;
      end;
      DoGetCellImageIndex(ACell, ImageIndex);
      Grid.Painter.DrawDataCell(Canvas, ARect, Text, ACell.Alignment, AStates, FDrawStyle, Grid.Scale, Images, ImageIndex, DoDrawHighlights, @HighlightHookRec);
    finally
      HighlightHookRec.CustomDrawnHighlights.Free;
    end;
  end;
end;

procedure TfcxSliceDataZone.SetImages(const Value: TCustomImageList);
begin
  if Assigned(Images) then
    Images.UnRegisterChanges(FImagesChangeLink);
  FImages := Value;
  if Assigned(Images) then
  begin
    Images.RegisterChanges(FImagesChangeLink);
    Images.FreeNotification(Grid);
  end;
  Invalidate;
end;

constructor TfcxSliceDataZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;
  FDrawStyle := TfcxCustomThemeStyle.Create(nil);
end;

destructor TfcxSliceDataZone.Destroy;
begin
  FImagesChangeLink.Free;
  FDrawStyle.Free;
  FDetailColumns.Free;
  inherited;
end;

procedure TfcxSliceDataZone.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfcxSliceDataZone.DblClick(X, Y: Integer);
var
  DataPoint: TfcxDataPoint;
begin
  DataPoint := PointToDataPoint(Point(X, Y));
  if Assigned(Grid.OnDataDblClick) then
    Grid.OnDataDblClick(Self, DataPoint.x, DataPoint.y)
  else
  begin
    if not Assigned(FDetailColumns) then
      FDetailColumns := TfcxCubeDataColumns.Create;
    ShowDetails(Grid.Slice, Grid.PaintStyle, Point(DataPoint.X, DataPoint.Y), FDetailColumns);
  end;
  inherited;
end;

procedure TfcxSliceDataZone.StopSizing(var AInfo: TSizingInfo);
begin
  case AInfo.ActivePart of
    zpLeftFrame:
      Grid.YAxisZone.Cells.IncSize(AInfo.CurPoint.X - AInfo.StartPoint.X);
    zpTopFrame:
      Grid.XAxisZone.Cells.IncSize(AInfo.CurPoint.Y - AInfo.StartPoint.Y);
  else
    inherited StopSizing(AInfo);
  end;
end;

function TfcxSliceDataZone.GetHorzScrollMax: Integer;
begin
  if Assigned(Grid) then
    Result := Grid.GetHorzScrollMax
  else
    Result := inherited GetHorzScrollMax;
end;

function TfcxSliceDataZone.GetVertScrollMax: Integer;
begin
  if Assigned(Grid) then
    Result := Grid.GetVertScrollMax
  else
    Result := inherited GetVertScrollMax;
end;

function TfcxSliceDataZone.IneritedGetHorzScrollMax: Integer;
begin
  Result := inherited GetHorzScrollMax;
end;

function TfcxSliceDataZone.IneritedGetVertScrollMax: Integer;
begin
  Result := inherited GetVertScrollMax;
end;

procedure TfcxSliceDataZone.SelectionChanged(Sender: TZone);
begin
  if Assigned(Grid) and Assigned(Grid.Slice) then
  begin
    if FSelection.Start.x = -1 then
      Grid.Slice.InternalSetSelected(-1, -1, -1, -1, -1)
    else
      Grid.Slice.InternalSetSelectedVis(SelectedMeasure, FSelection.Start.x, FSelection.Start.y);
  end;
end;

function TfcxSliceDataZone.GetText(ACol, ARow: Integer): String;
var
  Cell: TfcxMeasureCell;
begin
  Grid.Slice.GetMeasureCell(ACol, ARow, Cell);
  DoGetCellText(Cell, Result);
end;

function TfcxSliceDataZone.CanUpdateScrolls: Boolean;
begin
  Result := False;
end;

function TfcxSliceDataZone.GetAsPlainText: AnsiString;
var
  I, J: Integer;
  L: TStringList;
  S: AnsiString;
begin
  L := TStringList.Create;
  try
    for I := FSelection.Rect.Top to FSelection.Rect.Bottom do
    begin
      S:= '';
      for J := FSelection.Rect.Left to FSelection.Rect.Right do
        S := S + GetText(J, I) + #9;
      Delete(S, Length(S), 1);
      L.Add(S);
    end;
    Result := L.Text;
  finally
    L.Free;
  end;
end;

{ TfcxSliceCaptionZone }

procedure TfcxSliceCaptionZone.ClientPaint;
var
  R: TRect;
begin
  R := ClientRect;
  Grid.Painter.DrawBody(Canvas, R, Grid.Styles.CaptionArea);
  Canvas.Font.Assign(Grid.Styles.CaptionArea.Font);
  Canvas.Font.Color := Grid.Styles.CaptionArea.TextColor;
  if (Grid.Slice <> nil) and (Grid.Slice.Cube <> nil) then
    Grid.Painter.DrawText(Canvas, R, StringToControl(Grid.Slice.Cube.Caption), DT_CENTER or DT_SINGLELINE or DT_VCENTER)
  else
    Grid.Painter.DrawText(Canvas, R, '<cube is not assigned>', DT_CENTER or DT_SINGLELINE or DT_VCENTER)
end;

procedure TfcxSliceCaptionZone.ContextPopup(X, Y: Integer; var Handled: Boolean);
begin
  Handled := True;
  PopupMenuNeeded;
  PreparePopupMenu;
  with ClientToScreen(Point(X, Y)) do
    PopupMenu.Popup(X, Y);
end;

function TfcxSliceCaptionZone.CreatePopupMenu: TPopupMenu;
begin
  Result := inherited CreatePopupMenu;
  SetLength(FOwnMenuItems, 1);
  FOwnMenuItems[0] := NewItem(fcxResources.Get('sFieldFormRename'), 0, False, True, PopupMenuItemClick, 0, '');
  Result.Items.Add(FOwnMenuItems[0]);
end;

procedure TfcxSliceCaptionZone.PopupMenuItemClick(Sender: TObject);
var
  S: TfcxString;
begin
  case GetOwnMenuItemIndex(Sender) of
    0:
      begin
        S := Grid.Slice.Cube.Caption;
        if InputQuery(fcxResources.Get('sCubeChangeCaption'), fcxResources.Get('sCubeChangePropmpt'), S) then
          Grid.Slice.Cube.Caption := S;
      end;
  end;
end;

procedure TfcxSliceCaptionZone.PreparePopupMenu;
begin
  FOwnMenuItems[0].Visible := Assigned(Grid.Slice) and Assigned(Grid.Slice.Cube);
end;

{ TfcxSliceXAxisZone }

function TfcxSliceXAxisZone.DrawCell(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  CellInfo: TfcxCellInfo;
  CR, ARect: TRect;
  Sz: TSize;
begin
  CellInfo := FCells.AddCell(ARec);

  if AutoSize then
    Sz := CalcCellSize(Sender, CellInfo)
  else
    Sz := CellInfo.GraphicSize;

  with ARect do
  begin
    TopLeft := FCells.GetStartPointFor(CellInfo);
    Right := Left + Sz.cx;
    Bottom := Top + Sz.cy;
  end;
  CellInfo.BoundingRect := ARect;

  // correct the rect we have
  CR := ClientRect;
  if (pca_LastLevel in CellInfo.Data.CellProperties) and (CellInfo.BoundingRect.Bottom < CR.Bottom) then
    CellInfo.FBoundingRect.Bottom := CR.Bottom;
  // decide whether to stop
  Result := (ARect.Right + 1) >= CR.Right;
end;

function TfcxSliceXAxisZone.GetAxisContainer: TfcxAxisContainer;
begin
  if Assigned(Grid.Slice) then
    Result := Grid.Slice.XAxisContainer
  else
    Result := nil;
end;

function TfcxSliceXAxisZone.GetLastVisibleItem: Integer;
begin
  if Assigned(FCells) and FCells.Valid then
    Result := FCells.LastIndicesCount - 1
  else
    Result := ClientWidth div Grid.DefaultColWidth;
  Result := Min(FirstVisibleItem + Result, inherited GetLastVisibleItem);
end;

function TfcxSliceXAxisZone.GetRequiredSize: Integer;
begin
  if Visible then
  begin
    Result := inherited GetRequiredSize;
    if Result = 0 then
      Result := Grid.Rescale(Grid.DefaultRowHeight);
  end
  else
    Result := 0;
end;

function TfcxSliceXAxisZone.IsResizeableFrame(AFrame: TZoneFrame): Boolean;
begin
  Result := AFrame = zpBottomFrame;
end;

function TfcxSliceXAxisZone.IsSelected(Info: TfcxCellInfo): Boolean;
var
  Selection: TfcxGridSelection;
  i: integer;
  AAxisContainer: TfcxAxisContainer;
begin
  Selection := Grid.GetSelection;
  AAxisContainer := AxisContainer;
  Result := False;
  if Selection.Rect.Left = -1 then
    Exit;
  if Info.Data.TreeRect.Level = LastVisibleLevel then
  begin
    for i := Selection.Rect.Left to Selection.Rect.Right do
    begin
      if (AAxisContainer.VisibleAxisNodes.Index[i] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
      begin
        if (AAxisContainer.MeasuresLevel >= 0) then
        begin
          if (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
          begin
            Result := True;
            Exit;
          end
        end
        else
        begin
          Result := True;
          Exit;
        end
      end
    end
  end
  else
  begin
    for i := Selection.Rect.Left to Selection.Rect.Right do
    begin
      if AAxisContainer.MeasuresLevel < 0 then
      begin
        if (Info.Data.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [] then
        begin
          if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
          begin
            Result := True;
            Exit;
          end
        end
        else
        if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) then
        begin
          Result := True;
          Exit;
        end
      end
      else
      begin
        if Info.Data.TreeRect.Level >= AAxisContainer.MeasuresLevel then
        begin
          if (Info.Data.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [] then
          begin
            if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level - 1] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
            begin
              if (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
              begin
                Result := True;
                Exit;
              end
            end
          end
          else
          if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level - 1] = Info.Data.Cell) then
          begin
            if (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
            begin
              Result := True;
              Exit;
            end
          end
        end
        else
        begin
          if (Info.Data.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [] then
          begin
            if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
            begin
              if (Info.Data.MeasureIndex = -1) or (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
              begin
                Result := True;
                Exit;
              end
            end
          end
          else
          if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) then
          begin
            if (Info.Data.MeasureIndex = -1) or (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
            begin
              Result := True;
              Exit;
            end
          end
        end
      end
    end;
  end;
end;

procedure TfcxSliceXAxisZone.SelectData(Info: TfcxCellInfo);

  procedure SelectAtLevel(ALevel: TfcxSmallCount; ACell, ASizeCell: Integer);
  begin
    Grid.DataZone.SelectCols(ACell + FirstVisibleItem, ACell + FirstVisibleItem + ASizeCell - 1);
  end;

begin
  if Assigned(Info) then
    SelectAtLevel(Info.Data.TreeRect.Level, Info.Data.TreeRect.Cell, Info.Data.TreeRect.SizeCell)
end;

function TfcxSliceXAxisZone.CalcCellSize(Sender: TfcxAxisContainer; AInfo: TfcxCellInfo): TSize;
begin
  Result := MeasureCellSize(AInfo, False);
  with Result do
  begin
    cx := Max(AInfo.GraphicSize.cx, cx);
    cy := Max(AInfo.GraphicSize.cy, cy);
  end;
end;

function TfcxSliceXAxisZone.GetItemSize(AItemIndex: Integer): Integer;
begin
  if Assigned(FCells) and FCells.Valid and (AItemIndex >= GetFirstVisibleItem) and (AItemIndex < (GetFirstVisibleItem + FCells.LastIndicesCount)) then
    with FCells.ItemByLastIndex[AItemIndex - GetFirstVisibleItem].BoundingRect do
      Result := Right - Left
  else
    Result := Grid.FSlice.ColWidth[AItemIndex];
end;

function TfcxSliceXAxisZone.FindItemFromCoordinate(AItemCoordinate: Integer): Integer;
var
  I: integer;
begin
  for I := 0 to FCells.LastIndicesCount - 1 do
  begin
    if FCells.ItemByLastIndex[I].BoundingRect.Right >= AItemCoordinate then
    begin
      Result := I + GetFirstVisibleItem;
      Exit;
    end;
  end;
  Result := -1;
end;

function TfcxSliceXAxisZone.DrawCellForScroll(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  CellInfo: TfcxCellInfo;
  ARect: TRect;
begin
  Result := False;

  CellInfo := FTempCells.AddCell(ARec);

  ARect.TopLeft := FTempCells.GetStartPointFor(CellInfo);
  with CalcCellSize(Sender, CellInfo) do
  begin
    ARect.Right := ARect.Left + cx;
    ARect.Bottom := ARect.Top + cy;
  end;
  CellInfo.BoundingRect := ARect;
end;

constructor TfcxSliceXAxisZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FCells.TreeLevelSpacing := 0;
  FCells.TreeLevelSize := Grid.DefaultRowHeight;
  FCells.TreeMeasureSize := Grid.DefaultRowHeight
end;

function TfcxSliceXAxisZone.GetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart): Integer;
begin
  case ASizeType of
    scpXSizing:
      Result := ItemSize[FindItemFromCoordinate(ACellInfo.BoundingRect.Right)];
    scpYSizing:
      if Assigned(AxisContainer) then
      case AxisContainer.AxisType of
        at_Standard:
          with ACellInfo.Data.TreeRect do
            if SizeLevel > 1 then
              Result := AxisContainer.VisibleLevelSize[Level + SizeLevel - 1]
            else
              Result := AxisContainer.VisibleLevelSize[Level];
        at_Tree:
          begin
            if pca_TreeMeasureCell in ACellInfo.Data.CellProperties then
              Result := FCells.TreeMeasureSize
            else
              Result := FCells.TreeLevelSize;
          end;
        else
          Result := 0;
      end
      else
        Result := 0;
    else
      Result := 0;
  end;
end;

procedure TfcxSliceXAxisZone.SetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart; const Value: Integer);
var
  I: Integer;
begin
  if Assigned(AxisContainer) then
  case ASizeType of
    scpXSizing:
      if Assigned(ACellInfo) then
        AxisContainer.Slice.ColWidth[FindItemFromCoordinate(ACellInfo.BoundingRect.Right)] := Value
      else
        for I := 0 to AxisContainer.Slice.ColCount - 1 do
          AxisContainer.Slice.ColWidth[I] := Value;
    scpYSizing:
      case AxisContainer.AxisType of
        at_Standard:
          if Assigned(ACellInfo) then
          begin
            with ACellInfo.Data.TreeRect do
              if SizeLevel > 1 then
                AxisContainer.VisibleLevelSize[Level + SizeLevel - 1] := Value
              else
                AxisContainer.VisibleLevelSize[Level] := Value;
          end
          else
            for I := 0 to AxisContainer.LevelCount - 1 do
              AxisContainer.VisibleLevelSize[I] := Value;
        at_Tree:
          if Assigned(ACellInfo) then
          begin
            if pca_TreeMeasureCell in ACellInfo.Data.CellProperties then
              if Value = 0 then
                FCells.TreeMeasureSize := Grid.DefaultRowHeight
              else
                FCells.TreeMeasureSize := Value
            else
              if Value = 0 then
                FCells.TreeLevelSize := Grid.DefaultRowHeight
              else
                FCells.TreeLevelSize := Value;
            Update;
          end;
      end;
  end;
end;

procedure TfcxSliceXAxisZone.SetBoundingRect(const Value: TRect);
var
  OldClientRect: TRect;
begin
  OldClientRect := ClientRect;
  inherited;
  if not EqualRect(ClientRect, OldClientRect) then
    Grid.UpdateScrolls(False, True);
end;

{ TfcxSliceYAxisZone }

function TfcxSliceYAxisZone.DrawCell(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  CellInfo: TfcxCellInfo;
  ARect: TRect;
  Sz: TSize;
begin
  CellInfo := FCells.AddCell(ARec);

  if AutoSize then
    Sz := CalcCellSize(Sender, CellInfo)
  else
    Sz := CellInfo.GraphicSize;

  with ARect do
  begin
    TopLeft := FCells.GetStartPointFor(CellInfo);
    Right := Left + Sz.cx;
    Bottom := Top + Sz.cy;
  end;

  CellInfo.BoundingRect := ARect;
  Result := (ARect.Bottom + 1) >= ClientRect.Bottom;
end;

function TfcxSliceYAxisZone.GetAxisContainer: TfcxAxisContainer;
begin
  if Assigned(Grid.Slice) then
    Result := Grid.Slice.YAxisContainer
  else
    Result := nil;
end;

function TfcxSliceYAxisZone.GetLastVisibleItem: Integer;
begin
  if Assigned(FCells) and FCells.Valid then
    Result := FCells.LastIndicesCount - 1
  else
    Result := ClientHeight div Grid.DefaultRowHeight;
  Result := Min(FirstVisibleItem + Result, inherited GetLastVisibleItem);
end;

function TfcxSliceYAxisZone.GetRequiredSize: Integer;
begin
  if Visible then
  begin
    Result := inherited GetRequiredSize;
    if Result = 0 then
      Result := Grid.Rescale(Grid.DefaultColWidth);
  end
  else
    Result := 0;
end;

function TfcxSliceYAxisZone.IsResizeableFrame(AFrame: TZoneFrame): Boolean;
begin
  Result := AFrame = zpRightFrame;
end;

function TfcxSliceYAxisZone.IsSelected(Info: TfcxCellInfo): Boolean;
var
  Selection: TfcxGridSelection;
  i: integer;
  AAxisContainer: TfcxAxisContainer;
begin
  Selection := Grid.GetSelection;
  AAxisContainer := AxisContainer;
  Result := False;
  if Selection.Rect.Top = -1 then
    Exit;
  if Info.Data.TreeRect.Level = LastVisibleLevel then
  begin
    for i := Selection.Rect.Top to Selection.Rect.Bottom do
    begin
      if (AAxisContainer.VisibleAxisNodes.Index[i] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
      begin
        if (AAxisContainer.MeasuresLevel >= 0) then
        begin
          if (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
          begin
            Result := True;
            Exit;
          end
        end
        else
        begin
          Result := (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex);
          Exit;
        end
      end
    end
  end
  else
  begin
    for i := Selection.Rect.Top to Selection.Rect.Bottom do
    begin
      if AAxisContainer.MeasuresLevel < 0 then
      begin
        if (Info.Data.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [] then
        begin
          if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
          begin
            Result := True;
            Exit;
          end
        end
        else
        if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) then
        begin
          Result := True;
          Exit;
        end
      end
      else
      begin
        if Info.Data.TreeRect.Level >= AAxisContainer.MeasuresLevel then
        begin
          if (Info.Data.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [] then
          begin
            if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level - 1] = Info.Data.Cell) and (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) then
            begin
              if (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
              begin
                Result := True;
                Exit;
              end
            end
          end
          else
          if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level - 1] = Info.Data.Cell) then
          begin
            if (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
            begin
              Result := True;
              Exit;
            end
          end
        end
        else
        begin
          if (Info.Data.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [] then
          begin
            if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) and (
              (AAxisContainer.VisibleAxisNodes.AdditionalTotalIndex[i] = Info.Data.TotalIndex) or (pca_GrandTotal in Info.Data.CellProperties)) then
            begin
              if (Info.Data.MeasureIndex = -1) or (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
              begin
                Result := True;
                Exit;
              end
            end
          end
          else
          if (AAxisContainer.IndexOfTotalInLevel[AAxisContainer.VisibleAxisNodes.Index[i], Info.Data.TreeRect.Level] = Info.Data.Cell) then
          begin
            if (Info.Data.MeasureIndex = -1) or (AAxisContainer.VisibleAxisNodes.MeasureIndex[i] = Info.Data.MeasureIndex) then
            begin
              Result := True;
              Exit;
            end
          end
        end
      end
    end;
  end;
end;

procedure TfcxSliceYAxisZone.SelectData(Info: TfcxCellInfo);

  procedure SelectAtLevel(ALevel: TfcxSmallCount; ACell, ASizeCell: Integer);
  begin
    Grid.DataZone.SelectRows(ACell + FirstVisibleItem, ACell + FirstVisibleItem + ASizeCell - 1);
  end;

begin
  if Assigned(Info) then
    SelectAtLevel(Info.Data.TreeRect.Level, Info.Data.TreeRect.Cell, Info.Data.TreeRect.SizeCell)
end;

function TfcxSliceYAxisZone.CalcCellSize(Sender: TfcxAxisContainer; AInfo: TfcxCellInfo): TSize;
begin
  Result := MeasureCellSize(AInfo, True);
  with Result do
  begin
    cx := Max(AInfo.GraphicSize.cx, cx);
    cy := Max(AInfo.GraphicSize.cy, cy);
  end;
end;

function TfcxSliceYAxisZone.GetItemSize(AItemIndex: Integer): Integer;
begin
  if Assigned(FCells) and FCells.Valid and
    (AItemIndex >= GetFirstVisibleItem) and (AItemIndex < (GetFirstVisibleItem + FCells.LastIndicesCount)) then
      with FCells.ItemByLastIndex[AItemIndex - GetFirstVisibleItem].BoundingRect do
        Result := Bottom - Top
  else
    Result := Grid.FSlice.RowHeight[AItemIndex];
end;

function TfcxSliceYAxisZone.FindItemFromCoordinate(AItemCoordinate: Integer): Integer;
var
  I: integer;
begin
  for i := 0 to FCells.LastIndicesCount - 1 do
  begin
    if FCells.ItemByLastIndex[I].BoundingRect.Bottom >= AItemCoordinate then
    begin
      Result := I + GetFirstVisibleItem;
      Exit;
    end;
  end;
  Result := -1;
end;

function TfcxSliceYAxisZone.DrawCellForScroll(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  CellInfo: TfcxCellInfo;
  ARect: TRect;
begin
  Result := False;
  CellInfo := FTempCells.AddCell(ARec);

  ARect.TopLeft := FTempCells.GetStartPointFor(CellInfo);

  with CalcCellSize(Sender, CellInfo) do
  begin
    ARect.Right := ARect.Left + cx;
    ARect.Bottom := ARect.Top + cy;
  end;

  CellInfo.BoundingRect := ARect;
end;

constructor TfcxSliceYAxisZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FCells.TreeLevelSpacing := 20;
  FCells.TreeLevelSize := Grid.DefaultColWidth;
  FCells.TreeMeasureSize := Grid.DefaultColWidth;
end;

function TfcxSliceYAxisZone.GetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart): Integer;
begin
  case ASizeType of
    scpXSizing:
      if Assigned(AxisContainer) then
      case AxisContainer.AxisType of
        at_Standard:
          with ACellInfo.Data.TreeRect do
            if SizeLevel > 1 then
              Result := AxisContainer.VisibleLevelSize[Level + SizeLevel - 1]
            else
              Result := AxisContainer.VisibleLevelSize[Level];
        at_Tree:
          begin
            if pca_TreeMeasureCell in ACellInfo.Data.CellProperties then
              Result := FCells.TreeMeasureSize
            else
              Result := FCells.TreeLevelSize;
          end;
        else
          Result := 0;
      end;
    scpYSizing:
      Result := ItemSize[FindItemFromCoordinate(ACellInfo.BoundingRect.Bottom)];
    else
      Result := 0;
  end;
end;

procedure TfcxSliceYAxisZone.SetCellSize(ACellInfo: TfcxCellInfo; ASizeType: TfcxSlicAxisZoneClientPart; const Value: Integer);
var
  I: Integer;
begin
  if Assigned(AxisContainer) then
  case ASizeType of
    scpXSizing:
      case AxisContainer.AxisType of
        at_Standard:
          if Assigned(ACellInfo) then
          begin
            with ACellInfo.Data.TreeRect do
              if SizeLevel > 1 then
                AxisContainer.VisibleLevelSize[Level + SizeLevel - 1] := Value
              else
                AxisContainer.VisibleLevelSize[Level] := Value;
          end
          else
            for I := 0 to AxisContainer.LevelCount - 1 do
              AxisContainer.VisibleLevelSize[I] := Value;
        at_Tree:
          if Assigned(ACellInfo) then
          begin
            if pca_TreeMeasureCell in ACellInfo.Data.CellProperties then
              if Value = 0 then
                FCells.TreeMeasureSize := Grid.DefaultColWidth
              else
                FCells.TreeMeasureSize := Value
            else
              if Value = 0 then
                FCells.TreeLevelSize := Grid.DefaultColWidth
              else
                FCells.TreeLevelSize := Value;
            Update;
          end;
      end;
    scpYSizing:
      if Assigned(AcellInfo) then
        AxisContainer.Slice.RowHeight[FindItemFromCoordinate(ACellInfo.BoundingRect.Bottom)] := Value
      else
        for I := 0 to AxisContainer.Slice.RowCount - 1 do
          AxisContainer.Slice.RowHeight[I] := Value;
  end;
end;

procedure TfcxSliceYAxisZone.DrawEmptyText(ARect: TRect);
var
  FontNew, FontOld: HFONT;
  Ht: Integer;
  TextExtent: TSize;
  Caption: String;
begin
  Canvas.Font.Assign(Grid.Styles.HeaderArea.Font);
  Canvas.Font.Color := Grid.Styles.HeaderArea.TextColor;
  FontNew := CreateRotatedFont(Canvas, 90);
  FontOld := SelectObject(Canvas.Handle, FontNew);
  Caption := fcxResources.Get(sRegionCaptions[rf_CapYAx]);
  with ARect do
  begin
    Ht := Bottom - Top;
    TextExtent := Canvas.TextExtent(Caption);
    inc(TextExtent.cx, 2);
    inc(TextExtent.cy, 2);
    Bottom := (Top + Bottom - TextExtent.cx) div 2;
    Top := Bottom + TextExtent.cx;
    Left := (Left + Right - TextExtent.cy) div 2;
    Right := Left + Ht;
  end;
  Grid.Painter.DrawText(Canvas, ARect, Caption, DT_SINGLELINE);
  DeleteObject(SelectObject(Canvas.Handle, FontOld));
end;

procedure TfcxSliceYAxisZone.SetBoundingRect(const Value: TRect);
var
  OldClientRect: TRect;
begin
  OldClientRect := ClientRect;
  inherited;
  if not EqualRect(ClientRect, OldClientRect) then
    Grid.UpdateScrolls(True, False);
end;

{ TfcxSliceCustomAxisZone }

procedure TfcxSliceCustomAxisZone.ClientPaint;
var
  CR: TRect;
  i: integer;
  Rgn, Rgn1: HRGN;
  RgnType: Integer;
begin
  if Assigned(AxisContainer) then
  begin
    if not FCells.Valid then
    begin
      FCurrentCellInfo := nil;
      FCells.Clear;
      FCells.FirstLevel := FirstVisibleLevel;
      FCells.LastLevel := LastVisibleLevel;
      FCells.FirstCell := FirstVisibleItem;
      FCells.LastCell := AxisContainer.VisibleNodeCount - 1;

      AxisContainer.TraverseAxis(FCells.FirstLevel, FCells.LastLevel, FCells.FirstCell, DrawCell);
      FCells.Valid := True;
    end;
    CR := ClientRect;
    for i := 0 to FCells.Count - 1 do
      DrawCellInfo(FCells[i]);
    Rgn := CreateRectRgnIndirect(CR);
    Rgn1 := FCells.GetBoundingRgn;
    RgnType := CombineRgn(Rgn, Rgn, Rgn1, RGN_DIFF);
    DeleteObject(Rgn1);
    // draw empty zones
    if RgnType <> NULLREGION then
    begin
      with ClientBounds do
      begin
        OffsetRgn(Rgn, Left, Top);
        SelectClipRgn(Canvas.Handle, Rgn);
      end;
      Grid.Painter.DrawBody(Canvas, CR, Grid.Styles.HeaderCells);
      if FCells.Count = 0 then
        DrawEmptyText(CR);
      SelectClipRgn(Canvas.Handle, 0);
    end;
    DeleteObject(Rgn);
    // update ActiveClientPart
    if Owner.Sizing.ActiveZone <> Self then
      UpdateActiveClientPart(FLastCursorPos);
  end
  else
  begin
    Canvas.Brush.Color := Grid.Styles.HeaderCells.FillColor;
    Canvas.Font.Assign(Grid.Styles.HeaderCells.Font);
    Canvas.Font.Color := Grid.Styles.HeaderCells.TextColor;
    inherited;
  end;
end;

function TfcxSliceCustomAxisZone.GetFirstVisibleItem: Integer;
begin
  Result := FFirstVisibleItem;
end;

function TfcxSliceCustomAxisZone.GetFirstVisibleLevel: Integer;
begin
  Result := FFirstVisibleLevel;
end;

function TfcxSliceCustomAxisZone.GetAxisContainer: TfcxAxisContainer;
begin
  Result := nil;
end;

function TfcxSliceCustomAxisZone.GetLastVisibleItem: Integer;
begin
  if Assigned(AxisContainer) then
    Result := AxisContainer.VisibleNodeCount - 1
  else
    Result := -1;
end;

function TfcxSliceCustomAxisZone.GetLastVisibleLevel: Integer;
begin
  if Assigned(AxisContainer) then
    Result := AxisContainer.VisibleLevelCount - 1
  else
    Result := -1;
end;

procedure TfcxSliceCustomAxisZone.SetFirstVisibleItem(const Value: Integer);
begin
  if FFirstVisibleItem <> Value then
  begin
    FFirstVisibleItem := Value;
    FCells.Valid := False;
    Invalidate;
  end;
end;

procedure TfcxSliceCustomAxisZone.SetFirstVisibleLevel(const Value: Integer);
begin
  if FFirstVisibleLevel <> Value then
  begin
    FFirstVisibleLevel := Value;
    Invalidate;
  end;
end;

function TfcxSliceCustomAxisZone.GetRequiredSize: Integer;
var
  i: integer;
  AAxisContainer: TfcxAxisContainer;
begin
  Result := 0;
  if not Visible then
    Exit;
  AAxisContainer := AxisContainer;
  if Assigned(AAxisContainer) then
    case AAxisContainer.AxisType of
      at_Standard:
        for i := 0 to AAxisContainer.VisibleLevelCount - 1 do
          inc(Result, Grid.Rescale(AAxisContainer.VisibleLevelSize[i]));
      at_Tree:
        begin
          if AAxisContainer.LevelCount > 0 then
            Result := Grid.Rescale(FCells.TreeLevelSize);
          if AAxisContainer.MeasuresLevel > -1 then
            inc(Result, Grid.Rescale(Max(AAxisContainer.LevelCount - 1, 0) * FCells.TreeLevelSpacing + FCells.TreeMeasureSize))
          else
            inc(Result, Grid.Rescale(Max(AAxisContainer.LevelCount - 1, 0) * FCells.TreeLevelSpacing));
        end;
    end;
  if Result <> 0 then
    with FrameRect do
      inc(Result, Left + Right);
end;

procedure TfcxSliceCustomAxisZone.FullUpdate(AChanges: TfcxChangesInSlice);
begin
  Update;
end;

procedure TfcxSliceCustomAxisZone.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if not HasCapture then
  begin
    UpdateActiveClientPart(Point(X, Y));
    InvalidateItem(FCurrentCellInfo);
    if (Button = mbLeft) then
      case ActiveClientPart of
        scpXSizing: Owner.StartSizing(Self, stHorzSizing, Point(X, Y));
        scpYSizing: Owner.StartSizing(Self, stVertSizing, Point(X, Y));
        scpNone: SelectData(FCurrentCellInfo);
      end;
  end;
end;

procedure TfcxSliceCustomAxisZone.MouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
end;

procedure TfcxSliceCustomAxisZone.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
  InvalidateItem(FCurrentCellInfo);
  if Button = mbLeft then
  begin
    case ActiveClientPart of
      scpTreeButton:
        begin
          with CurrentCellInfo do
            if pca_GrandTotal in Data.CellProperties then
              if ssCtrl in Shift then
                AxisContainer.SetExpandedLevel(-1, pca_Collapsed in Data.CellProperties)
              else
                AxisContainer.SetExpanded(-1, Data.NodeIndex, Data.MeasureIndex, pca_Collapsed in Data.CellProperties)
            else
            if ssCtrl in Shift then
              AxisContainer.SetExpandedLevel(Data.TreeRect.Level, pca_Collapsed in Data.CellProperties)
            else
              AxisContainer.SetExpanded(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, pca_Collapsed in Data.CellProperties);
        end;
      scpHierButton:
        begin
          with CurrentCellInfo do
            if pca_GExpanded in Data.CellProperties then
              AxisContainer.SetExpandedGroup(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, False)
            else
            if pca_GCollapsed in Data.CellProperties then
              AxisContainer.SetExpandedGroup(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, True)
        end;
    end;
  end;
end;

procedure TfcxSliceCustomAxisZone.SetActiveClientPart(const Value: TfcxSlicAxisZoneClientPart);
begin
  if FActiveClientPart <> Value then
  begin
    FActiveClientPart := Value;
    UpdateCursor;
  end;
end;

function TfcxSliceCustomAxisZone.ClientHitTest(P: TPoint): TfcxSlicAxisZoneClientPart;
var
  R: TRect;
  Info: TfcxCellInfo;
begin
  Result := scpNone;
  if not FCells.Valid then
  begin
    FCurrentCellInfo := nil;
    Exit;
  end;
  Info := FCells.FindByPosition(P);
  try
    if Info = nil then
      Exit;
    if (Info.BoundingRect.Right - P.X) <= SplitThreshold then
      Result := scpXSizing
    else
    if (P.X - Info.BoundingRect.Left) <= SplitThreshold then
    begin
      dec(P.X, SplitThreshold + 1);
      Info := FCells.FindByPosition(P);
      if Info = nil then
        Exit;
      Result := scpXSizing;
    end
    else
    if (Info.BoundingRect.Bottom - P.Y) <= SplitThreshold then
      Result := scpYSizing
    else
    if (P.Y - Info.BoundingRect.Top) <= SplitThreshold then
    begin
      dec(P.Y, SplitThreshold + 1);
      Info := FCells.FindByPosition(P);
      if Info = nil then
        Exit;
      Result := scpYSizing;
    end
    else
    begin
      if (Info.TreeButtonPos.X <> -1) then
      begin
        R.TopLeft := Info.TreeButtonPos;
        R.BottomRight := Info.TreeButtonPos;
        inc(R.Right, TreeButtonSize);
        inc(R.Bottom, TreeButtonSize);
        if PtInRect(R, P) then
        begin
          Result := scpTreeButton;
          Exit;
        end
      end;
      if (Info.HierButtonPos.X <> -1) then
      begin
        R.TopLeft := Info.HierButtonPos;
        R.BottomRight := Info.HierButtonPos;
        inc(R.Right, TreeButtonSize);
        inc(R.Bottom, TreeButtonSize);
        if PtInRect(R, P) then
          Result := scpHierButton;
      end;
    end;
  finally
    CurrentCellInfo := Info;
  end;
end;

constructor TfcxSliceCustomAxisZone.Create(AOwner: TZoneContainer);
begin
  inherited Create(AOwner);
  FCells := TfcxCellInfos.Create(Self);
  FCurrentCellInfo := nil;
  FAutoSize := True;
  FAutoSizeConstraint := 0;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;
  FDrawStyle := TfcxCustomThemeStyle.Create(nil);
end;

destructor TfcxSliceCustomAxisZone.Destroy;
begin
  FCells.Free;
  FImagesChangeLink.Free;
  FDrawStyle.Free;
  inherited;
end;

procedure TfcxSliceCustomAxisZone.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfcxSliceCustomAxisZone.UpdateActiveClientPart(CursorPos: TPoint);
begin
  FLastCursorPos := CursorPos;
  if ActivePart = zpBody then
    ActiveClientPart := ClientHitTest(PointToClientPoint(CursorPos))
  else
  begin
    CurrentCellInfo := nil;
    ActiveClientPart := scpNone;
  end;
end;

procedure TfcxSliceCustomAxisZone.StartSizing(var AInfo: TSizingInfo);
begin
  if AInfo.ActivePart = zpBody then
  begin
    AInfo.Data := TfcxCellInfo.Create(nil);
    TfcxCellInfo(AInfo.Data).Assign(CurrentCellInfo);
  end
  else
    inherited;
end;

procedure TfcxSliceCustomAxisZone.UpdateSizingDrawInfo(var AInfo: TSizingInfo);
begin
  inherited;
  if AInfo.ActivePart = zpBody then
    case AInfo.SizingType of
      stHorzSizing:
        begin
          if Grid.DataZone.Top > Top then
            inc(AInfo.DrawStop.Y, Grid.DataZone.Height);
        end;
      stVertSizing:
        begin
          if Grid.DataZone.Left > Left then
            inc(AInfo.DrawStop.X, Grid.DataZone.Width);
        end;
    end;
end;

procedure TfcxSliceCustomAxisZone.MoveSizing(var AInfo: TSizingInfo);
const
  MinSize = 1;
var
  d: integer;
begin
  if AInfo.ActivePart = zpBody then
    case AInfo.SizingType of
      stHorzSizing:
        begin
          d := TfcxCellInfo(AInfo.Data).BoundingRect.Right - TfcxCellInfo(AInfo.Data).BoundingRect.Left;
          if (AInfo.CurPoint.X - AInfo.StartPoint.X + d) < MinSize then
            AInfo.CurPoint.X := AInfo.StartPoint.X - d + MinSize;
        end;
      stVertSizing:
        begin
          d := TfcxCellInfo(AInfo.Data).BoundingRect.Bottom - TfcxCellInfo(AInfo.Data).BoundingRect.Top;
          if (AInfo.CurPoint.Y - AInfo.StartPoint.Y + d) < MinSize then
            AInfo.CurPoint.Y := AInfo.StartPoint.Y - d + MinSize;
        end;
    end
  else
    inherited;
end;

procedure TfcxSliceCustomAxisZone.StopSizing(var AInfo: TSizingInfo);
var
  NewSize: Integer;
begin
  case AInfo.ActivePart of
    zpBody:
      begin
        case AInfo.SizingType of
          stHorzSizing:
            begin
              NewSize := CellSize[AInfo.Data, scpXSizing] + MulDiv(AInfo.CurPoint.X - AInfo.StartPoint.X, 100, Grid.Scale);
              if AInfo.ResizeSibling then
                CellSize[nil, scpXSizing] := NewSize
              else
                CellSize[AInfo.Data, scpXSizing] := NewSize;
            end;
          stVertSizing:
            begin
              NewSize := CellSize[AInfo.Data, scpYSizing] + MulDiv(AInfo.CurPoint.Y - AInfo.StartPoint.Y, 100, Grid.Scale);
              if AInfo.ResizeSibling then
                CellSize[nil, scpYSizing] := NewSize
              else
                CellSize[AInfo.Data, scpYSizing] := NewSize;
            end;
        end;
        TfcxCellInfo(AInfo.Data).Free;
      end;
    zpRightFrame:
      FCells.IncSize(AInfo.CurPoint.X - AInfo.StartPoint.X);
    zpBottomFrame:
      FCells.IncSize(AInfo.CurPoint.Y - AInfo.StartPoint.Y);
    else
      inherited;
  end;
end;

function TfcxSliceCustomAxisZone.IsSelected(Info: TfcxCellInfo): Boolean;
begin
  Result := False;
end;

function TfcxSliceCustomAxisZone.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; X, Y: Integer): Boolean;
var
  ScrollBar: TControl;  
begin
  Result := inherited MouseWheel(Shift, WheelDelta, X, Y);
  if not Result then
  begin
    ScrollBar := Grid.GetScrollBarFor(Self);
    if not (Assigned(ScrollBar) and ScrollBar.Enabled) then
      Exit;
    Grid.SetScrollPos(Self, ScrollBar, Grid.GetScrollParam(Self, ScrollBar, spPosition) - Sign(WheelDelta) * Mouse.WheelScrollLines);
    Result := True;
  end;
end;

procedure TfcxSliceCustomAxisZone.SelectionChanged(Sender: TZone);
var
  i: integer;
  Info: TfcxCellInfo;
  NewSelected: Boolean;
begin
  for i := 0 to FCells.Count - 1 do
  begin
    Info := FCells[i];
    NewSelected := IsSelected(Info);
    if NewSelected <> Info.IsSelected then
      InvalidateItem(Info);
  end;
end;

procedure TfcxSliceCustomAxisZone.SelectData(Info: TfcxCellInfo);
begin
  // nothing here
end;

procedure TfcxSliceCustomAxisZone.SetImages(const Value: TCustomImageList);
begin
  if Assigned(Images) then
    Images.UnRegisterChanges(FImagesChangeLink);
  FImages := Value;
  if Assigned(Images) then
  begin
    Images.RegisterChanges(FImagesChangeLink);
    Images.FreeNotification(Grid);
  end;
  Invalidate;
end;

procedure TfcxSliceCustomAxisZone.DoGetCellStyle(const AInfo: TfcxCellInfo; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle);
begin
  if AInfo.IsSelected then
    AStyle.Assign(Grid.Styles.HeaderCellsSelected)
  else
    AStyle.Assign(Grid.Styles.HeaderCells);
  if Assigned(Grid.OnGetAxisCellStyle) then
    Grid.OnGetAxisCellStyle(Self, AInfo, AState, AStyle);
end;

procedure TfcxSliceCustomAxisZone.DoGetCellText(const AInfo: TfcxCellInfo; out Text: String);
begin
  Text := StringToControl(AInfo.Data.Text);
  if Assigned(Grid.OnGetAxisCellText) then
    Grid.OnGetAxisCellText(Self, AInfo, Text);
end;

procedure TfcxSliceCustomAxisZone.DoGetCellImageIndex(const AInfo: TfcxCellInfo; out ImageIndex: Integer);
begin
  ImageIndex := -1;
  if Assigned(Grid.OnGetAxisCellImageIndex) then
    Grid.OnGetAxisCellImageIndex(Self, AInfo, ImageIndex);
end;

function TfcxSliceCustomAxisZone.DoMeasureCell(AState: TfcxThemeState; const AInfo: TfcxCellInfo; GrowInHeight: Boolean): TSize;
var
  Text: String;
  ImageIndex: Integer;
begin
  Result := AInfo.GraphicSize;
  if Assigned(Grid.OnMeasureAxisCell) then
    Grid.OnMeasureAxisCell(Self, Canvas, AState, AInfo, GrowInHeight, Result)
  else
  begin
    DoGetCellText(AInfo, Text);
    DoGetCellStyle(AInfo, AState, FDrawStyle);
    DoGetCellImageIndex(AInfo, ImageIndex);
    Result := Grid.Painter.MeasureAxisCell(Canvas, AInfo.GraphicSize, Text, AInfo.Data.CellProperties,
        AInfo.Data.Alignment, AState, FDrawStyle, Grid.Scale, GrowInHeight, Images, ImageIndex);
  end;
end;

procedure TfcxSliceCustomAxisZone.DoDrawCell(AState: TfcxThemeState; const AInfo: TfcxCellInfo);
var
  Text: String;
  ImageIndex: Integer;
begin
  if Assigned(Grid.OnDrawAxisCell) then
    Grid.OnDrawAxisCell(Self, Canvas, AState, AInfo)
  else
  begin
    DoGetCellText(AInfo, Text);
    DoGetCellStyle(AInfo, AState, FDrawStyle);
    DoGetCellImageIndex(AInfo, ImageIndex);
    Grid.Painter.DrawAxisCell(Canvas, AInfo.BoundingRect, Text, AInfo.Data.CellProperties,
      AInfo.Data.Alignment, AState, FDrawStyle, Grid.Scale, AInfo.FTreeButtonPos, AInfo.FHierButtonPos, Images, ImageIndex, not AutoSize);
  end;
end;

procedure TfcxSliceCustomAxisZone.DrawCellInfo(Info: TfcxCellInfo);
var
  State: TfcxThemeState;
begin
  State := tsNormal;
  if CurrentCellInfo = Info then
    if GetKeyState(VK_LBUTTON) < -126 then
      State := tsPressed;

  Info.FIsSelected := IsSelected(Info);
  if Info.IsSelected and (State = tsNormal) then
    State := tsHot;

  DoDrawCell(State, Info);
end;

procedure TfcxSliceCustomAxisZone.InvalidateItem(Info: TfcxCellInfo);
var
  R: TRect;
begin
  if Assigned(Info) then
  begin
    R := Info.BoundingRect;
    Invalidate(False, @R);
  end;
end;

procedure TfcxSliceCustomAxisZone.DblClick(X: Integer; Y: Integer);
begin
  UpdateActiveClientPart(Point(X, Y));
  if Assigned(CurrentCellInfo) then
  begin
    InvalidateItem(CurrentCellInfo);
    case ActiveClientPart of
      scpNone:
        begin
          with CurrentCellInfo do
            if [pca_Expanded, pca_Collapsed] * Data.CellProperties <> [] then
            begin
              if pca_GrandTotal in Data.CellProperties then
                if GetKeyState(VK_CONTROL) < -126 then
                  AxisContainer.SetExpandedLevel(-1, pca_Collapsed in Data.CellProperties)
                else
                  AxisContainer.SetExpanded(-1, Data.NodeIndex, Data.MeasureIndex, pca_Collapsed in Data.CellProperties)
              else
              if GetKeyState(VK_CONTROL) < -126 then
                AxisContainer.SetExpandedLevel(Data.TreeRect.Level, pca_Collapsed in Data.CellProperties)
              else
                AxisContainer.SetExpanded(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, pca_Collapsed in Data.CellProperties);
            end;
        end;
      scpXSizing,
      scpYSizing:
        SetCellSize(CurrentCellInfo, ActiveClientPart, 0); // reset size
    end;
  end;
end;

procedure TfcxSliceCustomAxisZone.SetCurrentCellInfo(const Value: TfcxCellInfo);
var
  OldInfo: TfcxCellInfo;
begin
  if FCurrentCellInfo <> Value then
  begin
    OldInfo := FCurrentCellInfo;
    FCurrentCellInfo := Value;
    InvalidateItem(OldInfo);
    InvalidateItem(FCurrentCellInfo);
  end;
end;

procedure TfcxSliceCustomAxisZone.MouseLeave;
begin
  UpdateActiveClientPart(Point(-1, -1));
end;

procedure TfcxSliceCustomAxisZone.Update;
begin
  FCells.Valid := False;
  inherited;
end;

procedure TfcxSliceCustomAxisZone.ContextPopup(X: Integer; Y: Integer;
  var Handled: Boolean);
begin
  Handled := True;
  PopupMenuNeeded;
  PreparePopupMenuFor(FCells.FindByPosition(Point(X, Y)));
  with ClientToScreen(Point(X, Y)) do
    PopupMenu.Popup(X, Y);
end;

function TfcxSliceCustomAxisZone.CreatePopupMenu: TPopupMenu;
const
  Items: array[0..20] of String = (
    'sExpand',
    'sExpandGroup',
    cLineCaption,
    'sExpandAll',
    'sCollapseAll',
    cLineCaption,
    'sMoveToGroup',
    'sMoveFromGroup',
    'sDeleteGroup',
    'sRenameGroup',
    cLineCaption,
    'sFilterOutThisItem',
    'sFilterOutAllOtherItems',
    cLineCaption,
    'sHideNode',
    'sShowHiddenNodes',
    cLineCaption,
    'sGrandTotal',
    'sAxisType',
    'sAxisProperties',
    'sProperties'
  );
  TotalsItems: array[TfcxTotalPosition] of String = (
 { fctp_Before } 'sBefore',
 { fctp_After  } 'sAfter',
 { fctp_Hide   } 'sHide'
  );
  AxisItems: array[TfcxAxisType] of String = (
 { at_Standard } 'sStandard',
 { at_Tree     } 'sTreeLike'
  );
var
  I: Integer;
  TP: TfcxTotalPosition;
  AF: TfcxAgrFunc;
  AT: TfcxAxisType;
  Item: TMenuItem;
begin
  Result := inherited CreatePopupMenu;
  SetLength(FOwnMenuItems, Length(Items));
  for I := Low(Items) to High(Items) do
  begin
    FOwnMenuItems[I] := NewItem(fcxResources.Get(Items[I]), 0, False, True, PopupItemClick, 0, '');
    Result.Items.Add(FOwnMenuItems[I]);
  end;
  // totals
  // a) position
  for TP := Low(TotalsItems) to High(TotalsItems) do
  begin
    Item := NewItem(fcxResources.Get(TotalsItems[TP]), 0, False, True, TotalsPopupItemClick, 0, '');
    Item.RadioItem := True;
    FOwnMenuItems[17].Add(Item);
  end;
  // b) additional totals
  Item := NewLine;
  Result.Items[17].Add(Item);
  for AF := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
  begin
    Item := NewItem(fcxResources.Get(sFuncCaptions[AF]), 0, False, True, TotalsPopupItemClick, 0, '');
    FOwnMenuItems[17].Add(Item);
  end;
  
  for AT := Low(TfcxAxisType) to High(TfcxAxisType) do
  begin
    Item := NewItem(fcxResources.Get(AxisItems[AT]), 0, False, True, AxisTypePopupItemClick, 0, '');
    Item.RadioItem := True;
    FOwnMenuItems[18].Add(Item);
  end;
end;

procedure TfcxSliceCustomAxisZone.PreparePopupMenuFor(AInfo: TfcxCellInfo);
var
  LevelInfo: TfcxAxisLevelInfo;
  Item: TMenuItem;
  I, J: integer;
  AF: TfcxAgrFunc;  
begin
  FPopupInfo := AInfo;
  for I := 0 to High(FOwnMenuItems) do
    for J := 0 to FOwnMenuItems[I].Count - 1 do
      FOwnMenuItems[I][J].Checked := False;
  // set the visibility
  FOwnMenuItems[0].Visible := Assigned(AInfo) and (AInfo.TreeButtonPos.X <> -1);
  FOwnMenuItems[1].Visible := Assigned(AInfo) and (AInfo.HierButtonPos.X <> -1);
  FOwnMenuItems[2].Visible := FOwnMenuItems[0].Visible or FOwnMenuItems[1].Visible;
  FOwnMenuItems[3].Visible := FOwnMenuItems[0].Visible or FOwnMenuItems[1].Visible;
  FOwnMenuItems[4].Visible := FOwnMenuItems[0].Visible or FOwnMenuItems[1].Visible;
  FOwnMenuItems[5].Visible := FOwnMenuItems[3].Visible or FOwnMenuItems[4].Visible;
  // set captions
  if Assigned(AInfo) and (AInfo.TreeButtonPos.X <> -1) then
    if pca_Collapsed in AInfo.Data.CellProperties then
      FOwnMenuItems[0].Caption := fcxResources.Get('sExpand')
    else
      FOwnMenuItems[0].Caption := fcxResources.Get('sCollapse');
  if Assigned(AInfo) and (AInfo.HierButtonPos.X <> -1) then
    if pca_GCollapsed in AInfo.Data.CellProperties then
      FOwnMenuItems[1].Caption := fcxResources.Get('sExpandGroup')
    else
      FOwnMenuItems[1].Caption := fcxResources.Get('sCollapseGroup');

  // hide all groups related items
  FOwnMenuItems[6].Visible := False;
  FOwnMenuItems[7].Visible := False;
  FOwnMenuItems[8].Visible := False;
  FOwnMenuItems[9].Visible := False;
  FOwnMenuItems[10].Visible := False;

  // hide filter related items
  FOwnMenuItems[11].Visible := False;
  FOwnMenuItems[12].Visible := False;

  if Assigned(AInfo) then
  begin
    LevelInfo := AxisContainer.LevelInfo[AInfo.Data.TreeRect.Level];
    if [pca_GCollapsed, pca_GExpanded] * AInfo.Data.CellProperties <> [] then
    begin
      FOwnMenuItems[8].Visible := True;
      FOwnMenuItems[9].Visible := True;
      FOwnMenuItems[10].Visible := True;
    end
    else
    if ([pca_GrandTotal, pca_StartTotal, pca_MeasureCell] * AInfo.Data.CellProperties = []) and Assigned(LevelInfo.RegionField) then
    begin
      FOwnMenuItems[6].Visible := True;
      FOwnMenuItems[7].Visible := LevelInfo.LevelType = fcATLT_SubGroup;
      FOwnMenuItems[6].OnClick := nil;
      FOwnMenuItems[6].Clear;
      Item := NewItem(fcxResources.Get('sCreateNew') + cDialogSuffix, 0, False, True, MoveToGroupClick, 0, '');
      FOwnMenuItems[6].Add(Item);
      FOwnMenuItems[6].Add(NewLine);
      for I := 0 to LevelInfo.RegionField.GroupCount - 1 do
      begin
        Item := NewItem(LevelInfo.RegionField.GroupCaption[I], 0, False, True, MoveToGroupClick, 0, '');
        FOwnMenuItems[6].Add(Item);
      end;
      FOwnMenuItems[10].Visible := True;
    end;
    FOwnMenuItems[11].Visible := [pca_GrandTotal, pca_StartTotal, pca_Empty, pca_EmptySubGroup, pca_MeasureCell] * AInfo.Data.CellProperties = [];
    FOwnMenuItems[12].Visible := [pca_GrandTotal, pca_StartTotal, pca_Empty, pca_EmptySubGroup, pca_MeasureCell] * AInfo.Data.CellProperties = [];
  end;
  FOwnMenuItems[13].Visible := FOwnMenuItems[11].Visible or FOwnMenuItems[12].Visible;
  FOwnMenuItems[14].Visible := Assigned(AInfo);
  FOwnMenuItems[15].Visible := Assigned(AxisContainer);
  FOwnMenuItems[17].Visible := Assigned(AxisContainer);
  if FOwnMenuItems[17].Visible then
  begin
    FOwnMenuItems[17].Items[Ord(AxisContainer.GrandTotalPosition)].Checked := True;
    for AF := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
      FOwnMenuItems[17].Items[Ord(High(TfcxTotalPosition))+1+Ord(AF)].Checked := AF in AxisContainer.AdditionalGrandTotalFunctions;
  end;

  FOwnMenuItems[18].Visible := Assigned(AxisContainer);
  if FOwnMenuItems[18].Visible then
    FOwnMenuItems[18].Items[Ord(AxisContainer.AxisType)].Checked := True;
  FOwnMenuItems[19].Visible := Assigned(AxisContainer);
  FOwnMenuItems[20].Visible := Assigned(AInfo) and (AInfo.Data.NodeLevel > -1);
end;

procedure TfcxSliceCustomAxisZone.PopupItemClick(Sender: TObject);
var
  S: String;
  Info: TfcxAxisLevelInfo;
  I: Integer;
begin
  case GetOwnMenuItemIndex(Sender) of
    0:
      with FPopupInfo do
        if pca_GrandTotal in Data.CellProperties then
            AxisContainer.SetExpanded(-1, Data.NodeIndex, Data.MeasureIndex, pca_Collapsed in Data.CellProperties)
        else
          AxisContainer.SetExpanded(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, pca_Collapsed in Data.CellProperties);
    1:
      with FPopupInfo do
        if pca_GExpanded in Data.CellProperties then
          AxisContainer.SetExpandedGroup(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, False)
        else
        if pca_GCollapsed in Data.CellProperties then
          AxisContainer.SetExpandedGroup(Data.TreeRect.Level, Data.NodeIndex, Data.MeasureIndex, True);
    3:
      with FPopupInfo do
        if pca_GrandTotal in Data.CellProperties then
          AxisContainer.SetExpandedLevel(-1, True)
        else
          AxisContainer.SetExpandedLevel(Data.TreeRect.Level, True);
    4:
      with FPopupInfo do
        if pca_GrandTotal in Data.CellProperties then
          AxisContainer.SetExpandedLevel(-1, False)
        else
          AxisContainer.SetExpandedLevel(Data.TreeRect.Level, False);
    7: // move from group
      AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level].RegionField.SliceField.GroupManager.RemoveUVFromGroup(FPopupInfo.Data.ValueIndex);
    8:
      AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level].RegionField.SliceField.GroupManager.DeleteGroup(FPopupInfo.Data.ValueIndex);
    9:
      begin
        S := AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level].RegionField.SliceField.GroupCaption[FPopupInfo.Data.NodeIndex];
        if InputQuery('Group rename', 'Rename to:', S) then
          AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level].RegionField.SliceField.GroupCaption[FPopupInfo.Data.NodeIndex] := S
      end;
    11:
      begin
        // filter out this item
        Info := AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level];
        if Info.IsMeasure then
          AxisContainer.Slice.MeasuresContainer.Measures[FPopupInfo.Data.MeasureIndex].Visible := False
        else
        if [pca_GExpanded, pca_GCollapsed] * FPopupInfo.Data.CellProperties <> [] then
          Info.RegionField.SliceField.UVFilterStateOfGroup[FPopupInfo.Data.ValueIndex] := csUnchecked
        else
          Info.RegionField.SliceField.UVFilterOf[FPopupInfo.Data.ValueIndex] := False
      end;
    12:
      begin
        // filter out all other items
        Info := AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level];
        if Info.IsMeasure then
          for I := 0 to AxisContainer.Slice.MeasuresContainer.Count - 1 do
            AxisContainer.Slice.MeasuresContainer.Measures[I].Visible := I = FPopupInfo.Data.MeasureIndex
        else
        if [pca_GExpanded, pca_GCollapsed] * FPopupInfo.Data.CellProperties <> [] then
          Info.RegionField.SliceField.UVGroupSingleIndex := FPopupInfo.Data.ValueIndex
        else
          Info.RegionField.SliceField.UVSingleIndex := FPopupInfo.Data.ValueIndex
      end;
    14:
      begin
        with FPopupInfo do
          AxisContainer.SetHidden(Data.NodeLevel, Data.NodeIndex, Data.MeasureIndex, (pca_StartTotal in Data.CellProperties) or (pca_GrandTotal in Data.CellProperties), Data.TotalIndex);
      end;
    15:
      begin
        if Assigned(FPopupInfo) then
          AxisContainer.ShowAllHidden
        else
          AxisContainer.ShowAllHidden;
      end;
    19:
      begin
        with TfcxAxisEditorForm.Create(Grid) do
          Execute(Grid.Slice, AxisContainer);
      end;
    20:
      begin
        Info := AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level];
        if Info.IsMeasure then
          with TfcxMeasureEditorForm.Create(Grid) do
            Execute(Grid.Slice, AxisContainer.Slice.MeasuresContainer.Measures[FPopupInfo.Data.MeasureIndex])
        else
        begin
          Info := AxisContainer.LevelInfoWOMeasures[FPopupInfo.Data.NodeLevel];
          with TfcxDimensionEditorForm.Create(Grid) do
            Execute(Grid.Slice, Info.RegionField);
        end
      end;
  end;
end;

procedure TfcxSliceCustomAxisZone.MoveToGroupClick(Sender: TObject);
var
  Index, UVIndex, GroupIndex: Integer;
  GroupManager: TfcxCommonGroupManager;
  S: String;
begin
  Index := (Sender as TMenuItem).MenuIndex;
  AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level].RegionField.SliceField.CanGroup := True;
  GroupManager := AxisContainer.LevelInfo[FPopupInfo.Data.TreeRect.Level].RegionField.SliceField.GroupManager;
  if Index = 0 then
  begin
    // create a new group
    S := fcxResources.Get('sNewGroup');
    if InputQuery(fcxResources.Get('sCreateANewGroup'), fcxResources.Get('sEnterANewGroupName'), S) then
      GroupIndex := GroupManager.CreateGroup(S).Index
    else
      Exit;
  end
  else
  if Index > 1 then
    GroupIndex := Index - 2
  else
    Exit;

  UVIndex := FPopupInfo.Data.ValueIndex;
  GroupManager.RemoveUVFromGroup(UVIndex);
  GroupManager.AddUVInGroup(UVIndex, GroupIndex);
end;

procedure TfcxSliceCustomAxisZone.TotalsPopupItemClick(Sender: TObject);
var
  Index: Integer;
  AF: TfcxAgrFunc;
begin
  Index := (Sender  as TMenuItem).MenuIndex;
  if Index <= Ord(High(TfcxTotalPosition)) then
    AxisContainer.GrandTotalPosition := TfcxTotalPosition(Index)
  else
  begin
    Dec(Index, Ord(High(TfcxTotalPosition)) + 2);
    AF := TfcxAgrFunc(Succ(Index));
    if AF in AxisContainer.AdditionalGrandTotalFunctions then
      AxisContainer.AdditionalGrandTotalFunctions := AxisContainer.AdditionalGrandTotalFunctions - [AF]
    else
      AxisContainer.AdditionalGrandTotalFunctions := AxisContainer.AdditionalGrandTotalFunctions + [AF];
  end;
end;

procedure TfcxSliceCustomAxisZone.AxisTypePopupItemClick(Sender: TObject);
begin
  AxisContainer.AxisType := TfcxAxisType((Sender as TMenuItem).MenuIndex);
end;

procedure TfcxSliceCustomAxisZone.UpdateCursor;
begin
  if ActivePart = zpBody then
    case FActiveClientPart of
      scpNone: Owner.Cursor := crDefault;
      scpXSizing: Owner.Cursor := crHSplit;
      scpYSizing: Owner.Cursor := crVSplit;
      scpTreeButton,
      scpHierButton: Owner.Cursor := crHandPoint;
    end
  else
    inherited UpdateCursor;
end;

function TfcxSliceCustomAxisZone.MeasureCellSize(Info: TfcxCellInfo; GrowInHeight: Boolean): TSize;
begin
  Result := DoMeasureCell(tsNormal, Info, GrowInHeight);
end;

procedure TfcxSliceCustomAxisZone.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    invalidate;
  end;
end;

procedure TfcxSliceCustomAxisZone.SetAutoSizeConstraint(const Value: Integer);
begin
  if FAutoSizeConstraint <> Value then
  begin
    FAutoSizeConstraint := Value;
    if AutoSize then
      invalidate;
  end;
end;

procedure TfcxSliceCustomAxisZone.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxSliceCustomAxisZone then
  begin
    TfcxSliceCustomAxisZone(Dest).AutoSizeConstraint := AutoSizeConstraint;
    TfcxSliceCustomAxisZone(Dest).AutoSize := AutoSize;
  end
  else
    inherited;
end;

function TfcxSliceCustomAxisZone.GetItemSize(AItemIndex: Integer): Integer;
begin
  Result := 0;
end;

function TfcxSliceCustomAxisZone.FindItemFromCoordinate(AItemCoordinate: Integer): Integer;
begin
  Result := -1;
end;

function TfcxSliceCustomAxisZone.GetScrollMax(const DefScrollMax: Integer): Integer;
  function GetSize(const R: TRecT): Integer;
  begin
    case AxisContainer.Region of
      rf_CapXAx:
        with R do
          Result := Right - Left;
      rf_CapYAx:
        with R do
          Result := Bottom - Top;
      else
        Result := 0;
    end;
  end;

var
  MaxSize, CurSize: Integer;
begin
  if not AutoSize or
     not Assigned(AxisContainer) or
     not Assigned(FCells) or
     (FCells.Valid and (GetLastVisibleItem = AxisContainer.VisibleNodeCount - 1)) then
    Result := DefScrollMax
  else
  begin
    // calc using Traverse
    FTempCells := TfcxCellInfos.Create(Self);
    try
      Result := AxisContainer.VisibleNodeCount - 1;
      if (Result = -1) or (GetLastVisibleLevel < 0) then
      begin
        Result := DefScrollMax;
        Exit;
      end;
      FTempCells.FirstLevel := GetFirstVisibleLevel;
      FTempCells.LastLevel := GetLastVisibleLevel;
      FTempCells.FirstCell := Max(0, DefScrollMax);
      FTempCells.LastCell := Result;
      FTempCells.TreeLevelSize := FCells.TreeLevelSize;
      FTempCells.TreeLevelSpacing := FCells.TreeLevelSpacing;
      FTempCells.TreeMeasureSize := FCells.TreeMeasureSize;

      AxisContainer.TraverseAxis(FTempCells.FirstLevel, FTempCells.LastLevel, FTempCells.FirstCell, DrawCellForScroll);
      MaxSize := GetSize(ClientRect);
      CurSize := 0;
      while (CurSize <= MaxSize) and (Result >= FTempCells.FirstCell) and (Result - FTempCells.FirstCell < FTempCells.LastIndicesCount) do
      begin
        Inc(CurSize, GetSize(FTempCells.ItemByLastIndex[Result - FTempCells.FirstCell].BoundingRect));
        Dec(Result);
      end;
      if CurSize > MaxSize then
        Inc(Result);
    finally
      FreeAndNil(FTempCells);
    end;
  end;
end;

function TfcxSliceCustomAxisZone.AcceptDrag(DragItem: TfcxCustomDragItem): Boolean;
var
  Item: TObject;
begin
  Item := DragItem.GetItem;
  Result := (Item is TfcxMeasuresContainer) or
            (Item is TfcxAxisField) or
            (Item is TfcxSliceField) or
            ((Item is TfcxMeasureField) and Assigned(TfcxMeasureField(Item).SliceField));
end;

procedure TfcxSliceCustomAxisZone.DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer);
var
  Index: Integer;
  AItem: TObject;
begin
  UpdateActiveClientPart(Point(X, Y));
  Index := AxisContainer.Fields.Count;
  if Assigned(Grid.FSlice) then
  begin
    AItem := DragItem.GetItem;
    if AItem is TfcxMeasuresContainer then
    begin
      TfcxFieldsContainerHack(AxisContainer).InsertMeasuresFieldInPosition(Index);
    end
    else
    if AItem is TfcxAxisField then
      TfcxFieldsContainerHack(AxisContainer).InsertFieldInPosition(TfcxAxisField(AItem).SliceField, Index, TfcxAxisField(AItem).Name, TfcxAxisField(AItem).Caption)
    else
    if AItem is TfcxSliceField then
      TfcxFieldsContainerHack(AxisContainer).InsertFieldInPosition(TfcxSliceField(AItem), Index)
    else
    if AItem is TfcxMeasureField then
      if Assigned(TfcxMeasureField(AItem).SliceField) then
        TfcxFieldsContainerHack(AxisContainer).InsertFieldInPosition(TfcxMeasureField(AItem).SliceField, Index);
  end;
end;

procedure TfcxSliceCustomAxisZone.DrawEmptyText(ARect: TRect);
begin

end;

procedure TfcxSliceCustomAxisZone.HintInfo(P: TPoint; out HintStr: String; out HintRect: TRect);
var
  Part: TZonePart;
  Info: TfcxCellInfo;
begin
  inherited HintInfo(P, HintStr, HintRect);
  Part := HitTest(P);
  if Part = zpBody then
  begin
    if FCells.Valid then
    begin
      Info := FCells.FindByPosition(PointToClientPoint(P));
      if Assigned(Info) then
      begin
        HintStr := Info.Data.Text;
        HintRect := Info.BoundingRect;
        HintRect.TopLeft := ClientPointToPoint(Hintrect.TopLeft);
        HintRect.BottomRight := ClientPointToPoint(Hintrect.BottomRight);
      end;
    end;
  end;
end;

{ TfcxCellInfos }

function TfcxCellInfos.AddCell(const ARec: TfcxSliceDrawHeader): TfcxCellInfo;
begin
  Result := TfcxCellInfo.Create(Self, ARec);
  Result.FGraphicSize := GetSizeFor(Result);

  if pca_LastLevel in Result.Data.CellProperties then
  begin
    if Result.Data.TreeRect.Cell >= FLastIndicesCount then
    begin
      FLastIndicesCount := Result.Data.TreeRect.Cell + 1;
      ReallocMem(FLastIndices, FLastIndicesCount * SizeOf(Integer));
      FLastIndices[Result.Data.TreeRect.Cell] := Add(Result);
    end
  end
  else
    Add(Result);
end;

procedure TfcxCellInfos.Clear;
begin
  inherited;
  if Assigned(FLastIndices) then
  begin
    FreeMem(FLastIndices);
    FLastIndices := nil;
  end;  
  FLastIndicesCount := 0;
end;

constructor TfcxCellInfos.Create(const AOwner: TfcxSliceCustomAxisZone);
begin
  inherited Create;
  FOwner := AOwner;
  FValid := False;
  FLastIndices := nil;
  FLastIndicesCount := 0;
  FTreeLevelSize := 0;
  FTreeLevelSpacing := 0;
  FTreeMeasureSize := 0;
end;

function TfcxCellInfos.FindByPosition(P: TPoint): TfcxCellInfo;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if PtInRect(Items[i].BoundingRect, P) then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

procedure TfcxCellInfos.FixRects(const AInfo: TfcxCellInfo);

  procedure IncSize(ACellIndex: Integer; ADiff: Integer);
  var
    I: Integer;
    Item: TfcxCellInfo;
  begin
    case FOwner.AxisContainer.Region of
      rf_CapXAx:
        for I := 0 to Count - 1 do
        begin
          Item := Items[I];
          if Item = AInfo then
            Continue;
          if Item.Data.TreeRect.Cell = ACellIndex then
            inc(Item.FBoundingRect.Right, ADiff)
          else
          if Item.Data.TreeRect.Cell > ACellIndex then
            OffsetRect(Item.FBoundingRect, ADiff, 0)
          else
          if (Item.Data.TreeRect.Cell < ACellIndex) and ((Item.Data.TreeRect.Cell + Item.Data.TreeRect.SizeCell) > ACellIndex)  then
            inc(Item.FBoundingRect.Left, ADiff)
        end;
      rf_CapYAx:
        for I := 0 to Count - 1 do
        begin
          Item := Items[I];
          if Item = AInfo then
            Continue;
          if Item.Data.TreeRect.Cell = ACellIndex then
            inc(Item.FBoundingRect.Bottom, ADiff)
          else
          if Item.Data.TreeRect.Cell > ACellIndex then
            OffsetRect(Item.FBoundingRect, 0, ADiff)
          else
          if (Item.Data.TreeRect.Cell < ACellIndex) and ((Item.Data.TreeRect.Cell + Item.Data.TreeRect.SizeCell) > ACellIndex)  then
            inc(Item.FBoundingRect.Top, ADiff)
        end;
    end;
  end;

var
  I: Integer;
  sz, isz: Integer;
begin
  // 1. Fix level size
  sz := GetLevelSize(AInfo);
  case FOwner.AxisContainer.Region of
    rf_CapXAx: AInfo.FBoundingRect.Bottom := AInfo.FBoundingRect.Top + sz;
    rf_CapYAx: AInfo.FBoundingRect.Right := AInfo.FBoundingRect.Left + sz;
  end;
  if not (pca_LastLevel in AInfo.Data.CellProperties) then
  begin
    // 2. Fix cell size
    sz := 0;
    case FOwner.AxisContainer.Region of
      rf_CapXAx:
        begin
          for I := AInfo.Data.TreeRect.Cell to AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 1 do
            with ItemByLastIndex[I].BoundingRect do
              inc(sz, Right - Left);
          with AInfo.FBoundingRect do
            if sz >= (Right - Left) then // if info cell size >= the size we have then fix our size
              Right := Left + sz
            else
            begin // in other case increase child sizes
              // the diff
              sz := (Right - Left) - sz;
              isz := sz div AInfo.Data.TreeRect.SizeCell;
              for I := AInfo.Data.TreeRect.Cell to AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 2 do
              begin
                IncSize(I, isz);
                dec(sz, isz);
              end;
              IncSize(AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 1, sz);
            end;
        end;
      rf_CapYAx:
        begin
          for I := AInfo.Data.TreeRect.Cell to AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 1 do
            with ItemByLastIndex[I].BoundingRect do
              inc(sz, Bottom - Top);
          with AInfo.FBoundingRect do
            if sz >= (Bottom - Top) then
              Bottom := Top + sz
            else
            begin
              sz := (Bottom - Top) - sz;
              isz := sz div AInfo.Data.TreeRect.SizeCell;
              for I := AInfo.Data.TreeRect.Cell to AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 2 do
              begin
                IncSize(I, isz);
                dec(sz, isz);
              end;
              IncSize(AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 1, sz);
            end;
        end;
    end;
  end;
end;

function TfcxCellInfos.GetBoundingRect: TRect;
var
  i: integer;
  R: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  for i := 0 to Count - 1 do
  begin
    R := Items[i].BoundingRect;
    if R.Right > Result.Right then
      Result.Right := R.Right;
    if R.Bottom > Result.Bottom then
      Result.Bottom := R.Bottom;
  end;
end;

function TfcxCellInfos.GetBoundingRgn: HRGN;
var
  i: integer;
  R: HRGN;
begin
  Result := CreateRectRgnIndirect(Rect(0, 0, 0, 0));
  for i := 0 to Count - 1 do
  begin
    R := CreateRectRgnIndirect(Items[i].BoundingRect);
    CombineRgn(Result, Result, R, RGN_OR);
    DeleteObject(R);
  end;
end;

function TfcxCellInfos.GetItem(AIndex: Integer): TfcxCellInfo;
begin
  Result := TfcxCellInfo(inherited Get(AIndex));
end;

function TfcxCellInfos.GetItemByLastIndex(AIndex: Integer): TfcxCellInfo;
begin
  Result := Items[FLastIndices[AIndex]];
end;

function TfcxCellInfos.GetLevelSize(const AInfo: TfcxCellInfo): Integer;
var
  I: Integer;
begin
  Result := 0;
  case FOwner.AxisContainer.AxisType of
    at_Standard:
      begin
        for I := AInfo.Data.TreeRect.Level to AInfo.Data.TreeRect.Level + AInfo.Data.TreeRect.SizeLevel - 1 do
          inc(Result, FOwner.Grid.Rescale(FOwner.AxisContainer.VisibleLevelSize[I + FirstLevel]));
      end;
    at_Tree:
      if pca_TreeMeasureCell in AInfo.Data.CellProperties then
        Result := FOwner.Grid.Rescale(TreeMeasureSize)
      else
      if pca_TreeCellWithMeasure in AInfo.Data.CellProperties then
        if FOwner.AxisContainer.MeasuresLevel > -1 then
          if AInfo.Data.TreeRect.Level <= FOwner.AxisContainer.MeasuresLevel then
            Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - 1 - AInfo.Data.TreeRect.Level) * TreeLevelSpacing)
          else
            Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - AInfo.Data.TreeRect.Level) * TreeLevelSpacing)
        else
          Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - 1 - AInfo.Data.TreeRect.Level) * TreeLevelSpacing)
      else
        if FOwner.AxisContainer.MeasuresLevel > -1 then
          if AInfo.Data.TreeRect.Level < FOwner.AxisContainer.MeasuresLevel then
            Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - 1 - AInfo.Data.TreeRect.Level) * TreeLevelSpacing + TreeMeasureSize)
          else
            Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - AInfo.Data.TreeRect.Level) * TreeLevelSpacing + TreeMeasureSize)
        else
          Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - 1 - AInfo.Data.TreeRect.Level) * TreeLevelSpacing);
  end;
end;

function TfcxCellInfos.GetSizeFor(const AInfo: TfcxCellInfo): TSize;
var
  I: Integer;
begin
  Result.cx := 0;
  Result.cy := 0;
  case FOwner.AxisContainer.Region of
    rf_CapXAx:
      begin
        Result.cy := GetLevelSize(AInfo);

        for I := AInfo.Data.TreeRect.Cell to AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 1 do
          inc(Result.cx, FOwner.Grid.Rescale(FOwner.Grid.FSlice.ColWidth[I + FirstCell]));
      end;
    rf_CapYAx:
      begin
        Result.cx := GetLevelSize(AInfo);

        for I := AInfo.Data.TreeRect.Cell to AInfo.Data.TreeRect.Cell + AInfo.Data.TreeRect.SizeCell - 1 do
          inc(Result.cy, FOwner.Grid.Rescale(FOwner.Grid.FSlice.RowHeight[I + FirstCell]));
      end;
  end;
end;

procedure TfcxCellInfos.IncSize(Delta: Integer);
begin
  if FOwner.AxisContainer <> nil then
    case FOwner.AxisContainer.AxisType of
      at_Standard:
        FOwner.AxisContainer.VisibleLevelSize[LastLevel] :=
          FOwner.AxisContainer.VisibleLevelSize[LastLevel] + MulDiv(Delta, 100, FOwner.Grid.Scale);
      at_Tree:
        begin
          TreeLevelSize := TreeLevelSize + MulDiv(Delta, 100, FOwner.Grid.Scale);
          FOwner.Update;
        end;
    end;
end;

function TfcxCellInfos.GetStartPointFor(const AInfo: TfcxCellInfo): TPoint;

  function GetLevelStart: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    case FOwner.AxisContainer.AxisType of
      at_Standard:
        begin
          for I := 0 to AInfo.Data.TreeRect.Level - 1 do
            inc(Result, FOwner.Grid.Rescale(FOwner.AxisContainer.VisibleLevelSize[I + FirstLevel]));
        end;
      at_Tree:
        if pca_TreeMeasureCell in AInfo.Data.CellProperties then
          if FOwner.AxisContainer.LevelCount > 0 then
            Result := FOwner.Grid.Rescale(TreeLevelSize + (FOwner.AxisContainer.LevelCount - 1) * TreeLevelSpacing)
          else
            Result := 0
        else
          if FOwner.AxisContainer.MeasuresLevel > -1 then
            if AInfo.Data.TreeRect.Level <= FOwner.AxisContainer.MeasuresLevel then
              Result := FOwner.Grid.Rescale(AInfo.Data.TreeRect.Level * TreeLevelSpacing)
            else
              Result := FOwner.Grid.Rescale((AInfo.Data.TreeRect.Level - 1) * TreeLevelSpacing)
          else
            Result := FOwner.Grid.Rescale(AInfo.Data.TreeRect.Level * TreeLevelSpacing);
    end;
  end;

var
  I: Integer;
begin
  Result.X := 0;
  Result.Y := 0;

  case FOwner.AxisContainer.Region of
    rf_CapXAx:
      begin
        Result.Y := GetLevelStart;

        for I := 0 to AInfo.Data.TreeRect.Cell - 1 do
          with ItemByLastIndex[I].BoundingRect do
            inc(Result.X, Right - Left);
      end;
    rf_CapYAx:
      begin
        Result.X := GetLevelStart;

        for I := 0 to AInfo.Data.TreeRect.Cell - 1 do
          with ItemByLastIndex[I].BoundingRect do
            inc(Result.Y, Bottom - Top);
      end;
  end;
end;

procedure TfcxCellInfos.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TfcxCellInfo(Ptr).Free;
end;

procedure TfcxCellInfos.SetItem(AIndex: Integer; const Value: TfcxCellInfo);
begin
  inherited Put(AIndex, Value);
end;

{ TfcxSliceCustomItemsZone }

function TfcxSliceCustomItemsZone.ClientHitTest(P: TPoint): TfcxSliceItemsZoneClientPart;
var
  i: integer;
  Item: TObject;
  Options: TfcxThemeButtonOptions;
  CR, R: TRect;
begin
  Result := scpiNone;
  CR := ClientRect;
  R := CR;
  inc(R.Left, Spacing);
  inc(R.Top, Spacing);
  dec(R.Bottom, Spacing);
  for i := FirstVisibleItem to GetItemCount - 1 do
  begin
    R.Right := R.Left + ItemWidth[i] - Spacing;
    IntersectRect(R, R, CR);
    if Abs(R.Right - P.X) <= SplitThreshold then
    begin
      Result := scpiXSizing;
      Exit;
    end
    else
    if PtInRect(R, P) then
    begin
      Item := GetItem(i);
      Options := GetItemThemeOptions(Item);
      if tboHasDropDownButton in Options then
      begin
        R := Grid.Painter.GetDropDownButtonRect(R, Options);
        if PtInRect(R, P) then
        begin
          Result := scpiDropDownButton;
          Exit;
        end;
      end;
      Exit;
    end;
    R.Left := R.Right + Spacing;
  end;
end;

procedure TfcxSliceCustomItemsZone.ClientPaint;
var
  I: integer;
  CR, R: TRect;
  EmptyRgn, DrawnRgn: HRGN;
  Text: String;
begin
  CR := ClientRect;
  EmptyRgn := CreateRectRgnIndirect(CR);
  DrawnRgn := CreateRectRgnIndirect(Rect(0, 0, 0, 0));

  R := CR;
  inc(R.Left, Spacing);
  inc(R.Top, Spacing);
  dec(R.Bottom, Spacing);
  for I := FirstVisibleItem to ItemCount - 1 do
  begin
    R.Right := R.Left + ItemWidth[i] - Spacing;
    IntersectRect(R, R, CR);
    if IsRectEmpty(R) then
      break;
    DrawItem(i, Canvas, R, DrawnRgn);
    R.Left := R.Right + Spacing;
  end;
  CombineRgn(EmptyRgn, EmptyRgn, DrawnRgn, RGN_DIFF);
  DeleteObject(DrawnRgn);
  with ClientBounds do
  begin
    OffsetRgn(EmptyRgn, Left, Top);
    SelectClipRgn(Canvas.Handle, EmptyRgn);
  end;
  Grid.Painter.DrawBody(Canvas, CR, Grid.Styles.HeaderArea);
  if ItemCount = 0 then
  begin
    Text := GetEmptyText;
    if Text <> '' then
    begin
      Canvas.Font.Assign(Grid.Styles.HeaderArea.Font);
      Canvas.Font.Color := Grid.Styles.HeaderArea.TextColor;
      Grid.Painter.DrawText(Canvas, CR, Text, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;
  end;
  SelectClipRgn(Canvas.Handle, 0);
  DeleteObject(EmptyRgn);
end;

constructor TfcxSliceCustomItemsZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FDropDownFromWholeSurface := False;
  FDownPos.X := -1;
  FActiveIndex := -1;
  FDropDownIndex := -1;
  FActiveClientPart := scpiNone;
  FRequiredScrollers := [];
  FSpacing := 2;
  HasFrame := False;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;
end;

destructor TfcxSliceCustomItemsZone.Destroy;
begin
  FPopupWindow.Free;
  FImagesChangeLink.Free;
  inherited Destroy;
end;

procedure TfcxSliceCustomItemsZone.DblClick(X, Y: Integer);
begin
  UpdateActiveClientPart(Point(X, Y));
end;

procedure TfcxSliceCustomItemsZone.DrawItem(AIndex: Integer; ACanvas: TCanvas; ARect: TRect; MergeWithRegion: HRGN);
var
  S: TfcxString;
  Options: TfcxThemeButtonOptions;
  SortType: TfcxThemeSortType;
  State: TfcxThemeState;
  Style: TfcxCustomThemeStyle;
  Item: TObject;
  ImageIndex: Integer;
begin
  Item := GetItem(AIndex);
  Options := GetItemThemeOptions(Item);
  S := GetItemText(Item);
  SortType := GetItemSortType(Item);
  Style := GetItemStyle(Item);
  if ActiveIndex = AIndex then
    if (GetKeyState(VK_LBUTTON) < -126) and (ActiveClientPart <> scpiDropDownButton) then
      State := tsPressed
    else
      State := tsHot
  else
    State := tsNormal;
  DoGetItemImageIndex(AIndex, ImageIndex);
  Grid.Painter.DrawItemButton(ACanvas, ARect, StringToControl(S), Images, ImageIndex, Options, SortType, State, Style, MergeWithRegion);
  if (tboHasDropDownButton in Options) {and (ActiveIndex = AIndex)} then
  begin
    ARect := Grid.Painter.GetDropDownButtonRect(ARect, Options);
    if not IsRectEmpty(ARect) then
    begin
      if (AIndex = ActiveIndex) and (ActiveClientPart = scpiDropDownButton) then
        if GetKeyState(VK_LBUTTON) < -126 then
          State := tsPressed
        else
          State := tsHot
      else
        State := tsNormal;
      if (AIndex = FDropDownIndex) then
        State := tsPressed;
      Grid.Painter.DrawDropDownButton(ACanvas, Style, ARect, Options, SortType, State);
    end;
  end;
end;

procedure TfcxSliceCustomItemsZone.GetDragIndexAt(P: TPoint; out Item: Integer; out Position: TfcxDragPosition);
var
  i, L, W: integer;
begin
  L := ClientRect.Left;
  for i := FirstVisibleItem to GetItemCount - 1 do
  begin
    W := ItemWidth[i];
    if P.X - L - W div 2 <= 0 then
    begin
      Item := i;
      Position := fcdpBefore;
      Exit;
    end
    else
    if P.X - L - W <= 0 then
    begin
      Item := i;
      Position := fcdpAfter;
      Exit;
    end;
    inc(L, W);
  end;
  Item := GetItemCount - 1;
  Position := fcdpAfter;
end;

function TfcxSliceCustomItemsZone.GetIndexAt(P: TPoint): Integer;
var
  i: integer;
  R: TRect;
begin
  R := ClientRect;
  with GetFrameRect do
    OffsetRect(R, Left, Top);
  InflateRect(R, -Spacing, -Spacing);
  for i := FirstVisibleItem to GetItemCount - 1 do
  begin
    R.Right := R.Left + ItemWidth[i] - Spacing;
    if PtInRect(R, P) then
    begin
      Result := i;
      Exit;
    end;
    R.Left := R.Right + Spacing;
  end;
  Result := -1;
end;

function TfcxSliceCustomItemsZone.GetItemRect(AIndex: Integer): TRect;
var
  i: integer;
begin
  Result := ClientRect;
  InflateRect(Result, -Spacing, -Spacing);
  for i := FirstVisibleItem to AIndex do
  begin
    Result.Right := Result.Left + ItemWidth[i] - Spacing;
    if I = AIndex then
      break;
    Result.Left := Result.Right + Spacing;
  end;
end;

procedure TfcxSliceCustomItemsZone.InitDrag(DragItem: TfcxCustomDragItem);
var
  AIndex: Integer;
  Bitmap, Mask: TBitmap;
  Rgn: HRGN;
  R: TRect;
begin
  AIndex := FDragIndex;
  Bitmap := TBitmap.Create;
  Mask := TBitmap.Create;
  try
    Bitmap.Width := ItemWidth[AIndex];
    with ClientRect do
      Bitmap.Height := Bottom - Top - Spacing * 2;
    Rgn := CreateRectRgn(0, 0, 0, 0);
    R := Rect(0, 0, Bitmap.Width, Bitmap.Height);
    DrawItem(AIndex, Bitmap.Canvas, R, Rgn);
    // add a mask to remove unpainted parts from the image
    Mask.Monochrome := True;
    Mask.Width := Bitmap.Width;
    Mask.Height := Bitmap.Height;
    // exclude all the rectange
    FillRect(Mask.Canvas.Handle, R, GetStockObject(WHITE_BRUSH));
    // set the clip region according to paiting region
    SelectClipRgn(Mask.Canvas.Handle, RGN);
    // include painted region into the mask
    FillRect(Mask.Canvas.Handle, R, GetStockObject(BLACK_BRUSH));
    DeleteObject(Rgn);
    TfcxSliceGridDragItem(DragItem).InitDrag(Bitmap, Mask, Self, AIndex);
  finally
    Bitmap.Free;
    Mask.Free;
  end;
end;

procedure TfcxSliceCustomItemsZone.InvalidateItem(AIndex: Integer);
var
  R: TRect;
begin
  if (AIndex < FirstVisibleItem) or (AIndex >= GetItemCount) then
    Exit;
  R := GetItemRect(AIndex);
  Invalidate(False, @R);
end;

procedure TfcxSliceCustomItemsZone.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if not HasCapture then
  begin
    UpdateActiveClientPart(Point(X, Y));
    InvalidateItem(ActiveIndex);
    if Button = mbLeft then
    begin
      case ActiveClientPart of
        scpiDropDownButton:
          if FPopupWindow = nil then
            DropDown(ActiveIndex);
        scpiXSizing: Owner.StartSizing(Self, stHorzSizing, Point(X, Y));
      else
        if AllowDrag then
        begin
          FDownPos := FLastCursorPos;
          FDragIndex := ActiveIndex;
          SetCapture;
        end
        else
        if FDropDownFromWholeSurface and not Assigned(FPopupWindow) then
          DropDown(ActiveIndex);
      end;
    end;
  end;
end;

procedure TfcxSliceCustomItemsZone.MouseLeave;
begin
  inherited;
  ActiveIndex := -1;
  ActiveClientPart := scpiNone;
end;

procedure TfcxSliceCustomItemsZone.MouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
begin
  inherited;
  if (FDownPos.X <> -1) and (FDragIndex <> -1) and
     (
      (Abs(FDownPos.X - X) > Mouse.DragThreshold) or
       (Abs(FDownPos.Y - Y) > Mouse.DragThreshold)
     ) then
  begin
    FDownPos.X := -1;
    ReleaseCapture;
    Grid.InitiateDrag;
  end
  else
    UpdateActiveClientPart(Point(X, Y));
end;

procedure TfcxSliceCustomItemsZone.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if HasCapture then
  begin
    ReleaseCapture;
    FDownPos.X := -1;
  end;
  UpdateActiveClientPart(Point(X, Y));
  InvalidateItem(ActiveIndex);
end;

procedure TfcxSliceCustomItemsZone.SetActiveClientPart(const Value: TfcxSliceItemsZoneClientPart);
begin
  if FActiveClientPart <> Value then
  begin
    FActiveClientPart := Value;
    UpdateCursor;
    InvalidateItem(ActiveIndex);
  end;
end;

procedure TfcxSliceCustomItemsZone.SetActiveIndex(const Value: Integer);
begin
  if FActiveIndex <> Value then
  begin
    InvalidateItem(FActiveIndex);
    FActiveIndex := Value;
    InvalidateItem(Value);
  end;
end;

procedure TfcxSliceCustomItemsZone.UpdateActiveClientPart(CursorPos: TPoint);
begin
  FLastCursorPos := CursorPos;
  if ActivePart = zpBody then
  begin
    ActiveIndex := GetIndexAt(CursorPos);
    if ActiveIndex = -1 then
      ActiveClientPart := scpiNone
    else
      ActiveClientPart := ClientHitTest(PointToClientPoint(CursorPos));
  end
  else
  begin
    ActiveIndex := -1;
    ActiveClientPart := scpiNone;
  end;
end;

procedure TfcxSliceCustomItemsZone.DropDown(AIndex: Integer);
var
  R: TRect;
  Item: TObject;
begin
  if AIndex < 0 then
    Exit;
  FDropDownIndex := AIndex;
  R := GetItemRect(AIndex);
  R.TopLeft := ClientToScreen(R.TopLeft);
  R.BottomRight := ClientToScreen(R.BottomRight);

  Item := GetItem(AIndex);
  FPopupWindow := CreatePopupWindow(Item);
  if Assigned(FPopupWindow) then
  begin
    FPopupWindow.OnDestroy := DoPopupDestroy;
    Grid.PreparePopup(FPopupWindow);
    FPopupWindow.Options := [pwoShowFooter, pwoAllowResize];
    FPopupWindow.PopupAt(Point(R.Left, R.Bottom));
  end;
end;

procedure TfcxSliceCustomItemsZone.DoPopupDestroy(Sender: TObject);
begin
  FPopupWindow := nil;
  InvalidateItem(FDropDownIndex);
  FDropDownIndex := -1;
end;

function TfcxSliceCustomItemsZone.GetRequiredScrollers: TZoneScrollers;
begin
  Result := FRequiredScrollers;
end;

procedure TfcxSliceCustomItemsZone.Update;
var
  I, MaxWidth, RequiredWidth: Integer;
begin
  inherited;
  // calculate scroll requirements
  FRequiredScrollers := [];
  MaxWidth := BoundingRect.Right - BoundingRect.Left;
  RequiredWidth := 0;
  for I := FirstVisibleItem to GetItemCount - 1 do
    inc(RequiredWidth, ItemWidth[I]);
  if (RequiredWidth > MaxWidth) and (GetItemCount > 1) then
    include(FRequiredScrollers, zsRight);
  if FirstVisibleItem > 0 then
    include(FRequiredScrollers, zsLeft);
end;

procedure TfcxSliceCustomItemsZone.Scroll(AScroller: TZoneScroller);
begin
  if AScroller = zsLeft then
    FirstVisibleItem := FirstVisibleItem - 1
  else
    FirstVisibleItem := FirstVisibleItem + 1;
end;

procedure TfcxSliceCustomItemsZone.SetFirstVisibleItem(const Value: Integer);
begin
  if FFirstVisibleItem <> Value then
  begin
    FFirstVisibleItem := Value;
    Update;
  end;
end;

procedure TfcxSliceCustomItemsZone.ContextPopup(X: Integer; Y: Integer;
  var Handled: Boolean);
begin
  Handled := True;
  PopupMenuNeeded;
  PreparePopupMenuFor(GetIndexAt(Point(X, Y)));
  with ClientToScreen(Point(X, Y)) do
    PopupMenu.Popup(X, Y);
end;

procedure TfcxSliceCustomItemsZone.UpdateCursor;
begin
  if ActivePart = zpBody then
    case FActiveClientPart of
      scpiNone: Owner.Cursor := crDefault;
      scpiDropDownButton: Owner.Cursor := crHandPoint;
      scpiXSizing: Owner.Cursor := crHSplit;
    end
  else
    inherited UpdateCursor;
end;

function TfcxSliceCustomItemsZone.GetRequiredSize: Integer;
begin
  if Visible then
    Result := Grid.DefaultRowHeight + Spacing * 2 + Grid.Painter.GetItemButtonThemeSpacing.cy * 2
  else
    Result := 0;
end;

procedure TfcxSliceCustomItemsZone.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Update;
  end;
end;

function TfcxSliceCustomItemsZone.GetItem(AIndex: Integer): TObject;
begin
  Result := nil;
end;

function TfcxSliceCustomItemsZone.GetItemCount: Integer;
begin
  Result := 0;
end;

function TfcxSliceCustomItemsZone.GetItemStyle(Item: TObject): TfcxCustomThemeStyle;
begin
  Result := Grid.Styles.InactiveDimension;
end;

function TfcxSliceCustomItemsZone.GetItemThemeOptions(Item: TObject): TfcxThemeButtonOptions;
begin
  Result := [];
end;

function TfcxSliceCustomItemsZone.GetItemWidth(AIndex: Integer): Integer;
begin
  Result := 0;
end;

procedure TfcxSliceCustomItemsZone.PreparePopupMenuFor(AIndex: Integer);
begin
  //
end;

function TfcxSliceCustomItemsZone.GetItemText(Item: TObject): TfcxString;
begin
  Result := '?';
end;

function TfcxSliceCustomItemsZone.GetItemSortType(Item: TObject): TfcxThemeSortType;
begin
  Result := tstUnknown;
end;

function TfcxSliceCustomItemsZone.CreatePopupWindow(Item: TObject): TfcxPopupWindow;
begin
  Result := nil;
end;

function TfcxSliceCustomItemsZone.GetAllowDrag: Boolean;
begin
  Result := False;
end;

procedure TfcxSliceCustomItemsZone.SetItemWidth(AIndex: Integer;
  const Value: Integer);
begin
  //
end;

procedure TfcxSliceCustomItemsZone.MoveSizing(var AInfo: TSizingInfo);
const
  MinSize = 1;
var
  d: integer;
begin
  if ActivePart = zpBody then
    if AInfo.SizingType = stHorzSizing then
    begin
      d := ItemWidth[PtrInt(AInfo.Data)];
      if (AInfo.CurPoint.X - AInfo.StartPoint.X + d) < MinSize then
        AInfo.CurPoint.X := AInfo.StartPoint.X - d + MinSize;
    end
  else
    inherited;
end;

procedure TfcxSliceCustomItemsZone.StartSizing(var AInfo: TSizingInfo);
begin
  if AInfo.ActivePart = zpBody then
    AInfo.Data := Pointer(ActiveIndex)
  else
    inherited;
end;

procedure TfcxSliceCustomItemsZone.StopSizing(var AInfo: TSizingInfo);
begin
  if AInfo.ActivePart = zpBody then
    case AInfo.SizingType of
      stHorzSizing:
        ItemWidth[PtrInt(AInfo.Data)] := ItemWidth[PtrInt(AInfo.Data)] + MulDiv(AInfo.CurPoint.X - AInfo.StartPoint.X, 100, Grid.Scale);
    end
  else
    inherited;
end;

function TfcxSliceCustomItemsZone.GetEmptyText: String;
begin
  Result := '';
end;

procedure TfcxSliceCustomItemsZone.DrawScroller(ARect: TRect; AScroller: TZoneScroller; AEnabled: Boolean);
var
  R: TRect;
  Rgn, Tmp: HRGN;
  RgnType: Integer;
begin
  Rgn := CreateRectRgnIndirect(ARect);
  R := ARect;
  InflateRect(R, 0, -Spacing);
  Grid.Painter.DrawScrollButton(Canvas, R, ScrollerDirectionMap[AScroller], GetScrollerState(AScroller, AEnabled));
  Tmp := CreateRectRgnIndirect(R);
  RgnType := CombineRgn(Rgn, Rgn, Tmp, RGN_DIFF);
  DeleteObject(Tmp);
  if RgnType <> NULLREGION then
  begin
    with BoundingRect do
      OffsetRgn(Rgn, Left, Top);
    SelectClipRgn(Canvas.Handle, Rgn);
    Grid.Painter.DrawBody(Canvas, ARect, Grid.Styles.HeaderArea);
  end;
  SelectClipRgn(Canvas.Handle, 0);
  DeleteObject(Rgn);
end;

procedure TfcxSliceCustomItemsZone.SetImages(const Value: TCustomImageList);
begin
  if Assigned(Images) then
    Images.UnRegisterChanges(FImagesChangeLink);
  FImages := Value;
  if Assigned(Images) then
  begin
    Images.RegisterChanges(FImagesChangeLink);
    Images.FreeNotification(Grid);
  end;
  Invalidate;
end;

procedure TfcxSliceCustomItemsZone.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfcxSliceCustomItemsZone.DoGetItemImageIndex(const ItemIndex: Integer; out ImageIndex: Integer);
begin
  ImageIndex := -1;
  if Assigned(Grid.OnGetItemImageIndex) then
    Grid.OnGetItemImageIndex(Self, ItemIndex, ImageIndex);
end;

procedure TfcxSliceCustomItemsZone.HintInfo(P: TPoint; out HintStr: String; out HintRect: TRect);
var
  Part: TZonePart;
  Index: Integer;
begin
  Part := HitTest(P);
  if Part = zpBody then
  begin
    Index := GetIndexAt(PointToClientPoint(P));
    if Index <> -1 then
    begin
      HintStr := GetItemText(Item[Index]);
      HintRect := GetItemRect(Index);
      HintRect.TopLeft := ClientPointToPoint(HintRect.TopLeft);
      HintRect.BottomRight := ClientPointToPoint(HintRect.BottomRight);
    end
    else
      inherited HintInfo(P, HintStr, HintRect);
  end
  else
    inherited HintInfo(P, HintStr, HintRect);
end;

{ TfcxSliceItemsZone }

constructor TfcxSliceItemsZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FRegion := rf_None;
  FFields := nil;
end;

procedure TfcxSliceItemsZone.DblClick(X, Y: Integer);
const
  NextSortDirection: array[TfcxSortDirection] of TfcxSortDirection = (fcsd_Desc, fcsd_Asc);
var
  Item: TObject;
begin
  inherited;
  // change sort direction
  if (ActiveClientPart <> scpiDropDownButton) and (ActiveIndex <> -1) then
  begin
    Item := GetItem(ActiveIndex);
    if Item is TfcxAxisField then
    begin
      TfcxAxisField(Item).SortDirection := NextSortDirection[TfcxAxisField(Item).SortDirection];
      InvalidateItem(ActiveIndex);
    end;
  end;
end;

procedure TfcxSliceItemsZone.DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer);

  procedure AddDim(const AField: TfcxSliceField; const AName, ACaption: String; const Index: Integer);
  begin
    TfcxFieldsContainerHack(FFields.Container).InsertFieldInPosition(AField, Index, AName, ACaption);
  end;

var
  Index: Integer;
  AItem: TObject;
  Position: TfcxDragPosition;
begin
  UpdateActiveClientPart(Point(X, Y));
  GetDragIndexAt(Point(X, Y), Index, Position);
  if Position = fcdpAfter then
    inc(Index);
  if Assigned(Grid.FSlice) then
  begin
    AItem := DragItem.GetItem;
    if AItem is TfcxMeasuresContainer then
      TfcxFieldsContainerHack(FFields.Container).InsertMeasuresFieldInPosition(Index)
    else
    if AItem is TfcxAxisField then
      TfcxFieldsContainerHack(FFields.Container).InsertFieldInPosition(TfcxAxisField(AItem).SliceField, Index, TfcxAxisField(AItem).Name, TfcxAxisField(AItem).Caption)
    else
    if AItem is TfcxSliceField then
      TfcxFieldsContainerHack(FFields.Container).InsertFieldInPosition(TfcxSliceField(AItem), Index)
    else
    if AItem is TfcxMeasureField then
      TfcxFieldsContainerHack(FFields.Container).InsertFieldInPosition(TfcxMeasureField(AItem).SliceField, Index)
  end;
  // drag a field DragItem.ItemIndex from DragItem.ItemsZone to Index
end;

procedure TfcxSliceItemsZone.FullUpdate;
begin
  SetFields;
  Update;
end;

function TfcxSliceItemsZone.GetItem(AIndex: Integer): TObject;
var
  MeasureIndex: Integer;
begin
  if Assigned(Grid.Slice) and (Grid.Slice.MeasuresContainer.Container = FFields.Container) then
  begin
    MeasureIndex := TfcxAxisContainer(FFields.Container).MeasuresPosition;
    if AIndex > MeasureIndex then
      Result := FFields.Items[AIndex - 1]
    else
    if AIndex < MeasureIndex then
    begin
      if (AIndex < FFields.Count) and (AIndex >= 0) then
        Result := FFields.Items[AIndex]
      else
        Result := nil;
    end
    else
      Result := Grid.Slice.MeasuresContainer
  end
  else
  if Assigned(FFields) and (AIndex < FFields.Count) and (AIndex >= 0) then
    Result := FFields.Items[AIndex]
  else
    Result := nil;
end;

function TfcxSliceItemsZone.GetItemCount: Integer;
begin
  if Assigned(FFields) then
  begin
    Result := FFields.Count;
    if Grid.Slice.MeasuresContainer.Container = FFields.Container then
      inc(Result);
  end
  else
    Result := 0;
end;

function TfcxSliceItemsZone.GetItemThemeOptions(Item: TObject): TfcxThemeButtonOptions;
var
  I: Integer;
begin
  Result := [];
  if Item is TfcxAxisField then
  begin
    include(Result, tboHasSortArrow);
    if TfcxAxisField(Item).SliceField.UVFilterSupport then
    begin
      include(Result, tboHasDropDownButton);
      if TfcxAxisField(Item).SliceField.UVFilteredValuesCount > 0 then
        include(Result, tboHasFilteredValues);
    end;
  end
  else
  if Item is TfcxMeasuresContainer then
  begin
    //include(Result, tboHasSortArrow);
    include(Result, tboHasDropDownButton);
    for I := 0 to TfcxMeasuresContainer(Item).Count - 1 do
      if not TfcxMeasuresContainer(Item).Measures[I].Visible then
      begin
        include(Result, tboHasFilteredValues);
        break;
      end;
  end;
end;

function TfcxSliceItemsZone.GetItemWidth(AIndex: Integer): Integer;
var
  Item: TObject;
begin
  Item := GetItem(AIndex);
  if Item is TfcxCommonFieldOfRegion then
    Result := TfcxCommonFieldOfRegion(Item).CaptionWidth
  else
  if Item is TfcxMeasuresContainer then
    Result := TfcxMeasuresContainer(Item).CaptionWidth
  else
    Result := 0;
end;

procedure TfcxSliceItemsZone.SetFields;
begin
  if Assigned(Grid.Slice) then
    FFields := Grid.Slice.FieldsOfRegion[FRegion]
  else
    FFields := nil;
end;

function TfcxSliceItemsZone.GetItemStyle(Item: TObject): TfcxCustomThemeStyle;
begin
  if Item is TfcxCommonFieldOfRegion then
    if Region = rf_Page then
      Result := Grid.Styles.InactiveDimension
    else
      Result := Grid.Styles.ActiveDimension
  else
    Result := Grid.Styles.Measure
end;

procedure TfcxSliceItemsZone.SetRegion(const Value: TfcxRegionOfField);
begin
  if FRegion <> Value then
  begin
    FRegion := Value;
    SetFields;
  end;
end;

function TfcxSliceItemsZone.CreatePopupMenu: TPopupMenu;
const
  Items: array[0..18] of String = (
 {0}'sSort',
 {1}'sTotals',
 {2}'sAlignment',
 {3}'sRename',
 {4}'sMoveToPage',
    cLineCaption,
 {6}'sCustomFilter',
 {7}'sClearFilter',
    cLineCaption,
 {9}'sCollapse',
 {0}'sExpand',
    cLineCaption,
 {2}'sSplits',
 {3}'sCreateNewGroup',
 {4}'sCreateOtherGroup',
 {5}'sAddNewMeasure',
 {6}'sShowTopN',
 {7}'sClearTopN',
 {8}'sProperties'
  );
  SortItems: array[TfcxSortDirection] of String = (
  { fcsd_Asc  } 'sAscending',
  { fcsd_Desc } 'sDescending'
  );
  TotalsItems: array[TfcxTotalPosition] of String = (
 { fctp_Before } 'sBefore',
 { fctp_After  } 'sAfter',
 { fctp_Hide   } 'sHide'
  );
  AlignmentItems: array[0..2] of String = (
  { taLeftJustify  } 'sLeftJustify',
  { taCenter       } 'sCenter',
  { taRightJustify } 'sRightJustify'
  );
  AlignmentIcons: array[0..2] of Integer = (14, 15, 16);
  SplitItems: array[0..1] of String = (
    'sTimeSplits',
    'sDateSplits'
  );
  TimeSplitItems: array[TfcxTimeType] of String = (
 { ott_None        } 'sNone',
 { ott_Hour        } 'sHour',
 { ott_Minute      } 'sMinute',
 { ott_Second      } 'sSecond',
 { ott_Millisecond } 'sMillisecond'
  );
  DateSplitItems: array[TfcxDateType] of String = (
 { odt_None       } 'sNone',
 { odt_Year       } 'sYear',
 { odt_Month      } 'sMonth',
 { odt_Day        } 'sDay',
 { odt_DayOfWeek  } 'sDayOfWeek',
 { odt_Quarter    } 'sQuarter',
 { odt_WeekNumber } 'sWeekNumber',
 { odt_DayOfYear  } 'sDayOfYear'
  );
var
  I: Integer;
  SD: TfcxSortDirection;
  TP: TfcxTotalPosition;
  TT: TfcxTimeType;
  DT: TfcxDateType;
  Item: TMenuItem;
  AF: TfcxAgrFunc;
begin
  Result := inherited CreatePopupMenu;
  SetLength(FOwnMenuItems, Length(Items));
  for I := Low(Items) to High(Items) do
  begin
    FOwnMenuItems[I] := NewItem(fcxResources.Get(Items[I]), 0, False, True, PopupItemClick, 0, '');
    Result.Items.Add(FOwnMenuItems[I]);
  end;
  for SD := Low(SortItems) to High(SortItems) do
  begin
    Item := NewItem(fcxResources.Get(SortItems[SD]), 0, False, True, SortPopupItemClick, 0, '');
    Item.RadioItem := True;
    FOwnMenuItems[0].Add(Item);
  end;
  // totals:
  // a) total position
  for TP := Low(TotalsItems) to High(TotalsItems) do
  begin
    Item := NewItem(fcxResources.Get(TotalsItems[TP]), 0, False, True, TotalsPopupItemClick, 0, '');
    Item.RadioItem := True;
    FOwnMenuItems[1].Add(Item);
  end;
  // b) use position from measure
  Item := NewItem(fcxResources.Get('sFromMeasure'), 0, False, True, TotalsPopupItemClick, 0, '');
  FOwnMenuItems[1].Add(Item);
  // c) additional totals
  Item := NewLine;
  FOwnMenuItems[1].Add(Item);
  for AF := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
  begin
    Item := NewItem(fcxResources.Get(sFuncCaptions[AF]), 0, False, True, TotalsPopupItemClick, 0, '');
    FOwnMenuItems[1].Add(Item);
  end;

  FOwnMenuItems[2].SubMenuImages := fcxGraphicResources.ToolImages;
  for I := Low(AlignmentItems) to High(AlignmentItems) do
  begin
    Item := NewItem(fcxResources.Get(AlignmentItems[I]), 0, False, True, AlignmentPopupItemClick, 0, '');
    Item.ImageIndex := AlignmentIcons[I];
    Item.RadioItem := True;
    FOwnMenuItems[2].Add(Item);
  end;
  Item := NewItem(fcxResources.Get('sDateAttribute'), 0, False, True, DateAttributeClick, 0, '');
  FOwnMenuItems[12].Add(Item);
  Item := NewItem(fcxResources.Get('sTimeAttribute'), 0, False, True, TimeAttributeClick, 0, '');
  FOwnMenuItems[12].Add(Item);
  for I := Low(SplitItems) to High(SplitItems) do
  begin
    Item := NewItem(fcxResources.Get(SplitItems[I]), 0, False, True, nil, 0, '');
    FOwnMenuItems[12].Add(Item);
  end;
  for TT := Succ(Low(TimeSplitItems)) to High(TimeSplitItems) do
  begin
    Item := NewItem(fcxResources.Get(TimeSplitItems[TT]), 0, False, True, TimeSplitPopupItemClick, 0, '');
    FOwnMenuItems[12][2].Add(Item);
  end;
  for DT := Succ(Low(DateSplitItems)) to High(DateSplitItems) do
  begin
    Item := NewItem(fcxResources.Get(DateSplitItems[DT]), 0, False, True, DateSplitPopupItemClick, 0, '');
    FOwnMenuItems[12][3].Add(Item);
  end;
end;

procedure TfcxSliceItemsZone.PreparePopupMenuFor(AIndex: Integer);
var
  AItem: TObject;
  I, J: Integer;
  AF: TfcxAgrFunc;
begin
  FPopupIndex := AIndex;
  AItem := GetItem(FPopupIndex);
  // set visibility
  FOwnMenuItems[0].Visible := Assigned(AItem) and (Region <> rf_Page) and not (AItem is TfcxMeasuresContainer); // sort
  FOwnMenuItems[1].Visible := (AItem is TfcxAxisField) and (Region <> rf_Page); // totals
  FOwnMenuItems[2].Visible := Assigned(AItem); // alignment
  FOwnMenuItems[3].Visible := Assigned(AItem); // rename
  FOwnMenuItems[4].Visible := Assigned(AItem) and (Region <> rf_Page); // move to page

  FOwnMenuItems[6].Visible := Assigned(AItem); // custom filter
  FOwnMenuItems[7].Visible := Assigned(AItem); // clear filter
  FOwnMenuItems[5].Visible := FOwnMenuItems[6].Visible or FOwnMenuItems[7].Visible; // separator

  FOwnMenuItems[9].Visible := Assigned(AItem) and (Region <> rf_Page); // expand
  FOwnMenuItems[10].Visible := Assigned(AItem) and (Region <> rf_Page); // collapse
  FOwnMenuItems[8].Visible := FOwnMenuItems[9].Visible or FOwnMenuItems[10].Visible; // separator

  FOwnMenuItems[12].Visible := Assigned(AItem);
  FOwnMenuItems[13].Visible := (AItem is TfcxAxisField) and Assigned(TfcxAxisField(AItem).SliceField.UniqueValues);
  FOwnMenuItems[14].Visible := (AItem is TfcxAxisField) and Assigned(TfcxAxisField(AItem).SliceField.UniqueValues) and not (TfcxAxisField(AItem).SliceField.HasGroups and TfcxAxisField(AItem).SliceField.GroupManager.ExistsOther);
  FOwnMenuItems[15].Visible := (AItem is TfcxMeasuresContainer);
  FOwnMenuItems[16].Visible := (AItem is TfcxAxisField) and (Region <> rf_Page); // show topN
  FOwnMenuItems[17].Visible := (AItem is TfcxAxisField) and (Region <> rf_Page) and
    (Grid.Slice.TopNs.AsFilter or TfcxAxisField(AItem).TopNProcessor.Active); // clear topN
  FOwnMenuItems[18].Visible := (AItem is TfcxAxisField);
  FOwnMenuItems[11].Visible := FOwnMenuItems[12].Visible or FOwnMenuItems[13].Visible or
    FOwnMenuItems[14].Visible or FOwnMenuItems[15].Visible or FOwnMenuItems[16].Visible or
    FOwnMenuItems[17].Visible or FOwnMenuItems[18].Visible;

  // set checked
  for I := 0 to High(FOwnMenuItems) do
    for J := 0 to FOwnMenuItems[I].Count - 1 do
      FOwnMenuItems[I].Items[J].Checked := False;
  // hide splits
  for I := 0 to FOwnMenuItems[12].Count - 1 do
    FOwnMenuItems[12][I].Visible := False;
  FOwnMenuItems[12].Visible := False;

  if AItem is TfcxAxisField then
  begin
    FOwnMenuItems[0].Items[Ord(TfcxAxisField(AItem).SortDirection)].Checked := True;
    FOwnMenuItems[1].Items[Ord(TfcxAxisField(AItem).TotalPosition)].Checked := True;
    FOwnMenuItems[1].Items[Ord(High(TfcxTotalPosition))+1].Checked := TfcxAxisField(AItem).UseTotalPositionFromMeasure;
    for AF := Succ(Low(TfcxAgrFunc)) to High(TfcxAgrFunc) do
      FOwnMenuItems[1].Items[Ord(High(TfcxTotalPosition))+2+Ord(AF)].Checked := AF in TfcxAxisField(AItem).AdditionalTotalFunctions;
    FOwnMenuItems[2].Items[AlignmentOrder[TfcxAxisField(AItem).Alignment]].Checked := True;
    // prepare splits
    if Assigned(TfcxAxisField(AItem).SliceField.SplitManager) then
    begin
      if TfcxAxisField(AItem).SliceField.SplitManager.CanUseTimeSplit and
         TfcxAxisField(AItem).SliceField.SplitManager.CanUseDateSplit then
      begin
        FOwnMenuItems[12][0].Visible := True;
        FOwnMenuItems[12][0].Checked :=
            TfcxAxisField(AItem).SliceField.SplitManager.AttributesManager.ExistsDateAttribute;
        FOwnMenuItems[12][1].Visible := True;
        FOwnMenuItems[12][1].Checked :=
            TfcxAxisField(AItem).SliceField.SplitManager.AttributesManager.ExistsTimeAttribute;
        FOwnMenuItems[12].Visible := True;
      end;

      if TfcxAxisField(AItem).SliceField.SplitManager.CanUseTimeSplit then
      begin
        FOwnMenuItems[12][2].Visible := True;
        for I := 0 to FOwnMenuItems[12][2].Count - 1 do
          FOwnMenuItems[12][2][I].Checked :=
            TfcxAxisField(AItem).SliceField.SplitManager.UseTimeSplit and
            TfcxAxisField(AItem).SliceField.SplitManager.TimePathsManager.UseTimePath[TfcxTimeType(I + 1)];
        FOwnMenuItems[12].Visible := True;
      end;
      if TfcxAxisField(AItem).SliceField.SplitManager.CanUseDateSplit then
      begin
        FOwnMenuItems[12][3].Visible := True;
        for I := 0 to FOwnMenuItems[12][3].Count - 1 do
          FOwnMenuItems[12][3][I].Checked :=
            TfcxAxisField(AItem).SliceField.SplitManager.UseDateSplit and
            TfcxAxisField(AItem).SliceField.SplitManager.DatePathsManager.UseDatePath[TfcxDateType(I + 1)];
        FOwnMenuItems[12].Visible := True;
      end;
    end;
  end;
end;

procedure TfcxSliceItemsZone.PopupItemClick(Sender: TObject);
var
  AItem: TObject;
  Value: TfcxString;
  S: String;
  NewMeasure: TfcxMeasureField;
  Ranges: TfcxRanges;
begin
  AItem := GetItem(FPopupIndex);
  case GetOwnMenuItemIndex(Sender) of
    3:
      begin
        if AItem is TfcxMeasuresContainer then
          Value := TfcxMeasuresContainer(AItem).Caption
        else
        if AItem is TfcxAxisField then
          Value := TfcxAxisField(AItem).Caption
        else
          Value := '';

        if (Value <> '') and InputQuery(fcxResources.Get('sFieldLabelChangeCaption'), fcxResources.Get('sFieldLabelChangePropmpt'), Value) then
        begin
          Grid.Slice.BeginUpdate;
          if AItem is TfcxMeasuresContainer then
            TfcxMeasuresContainer(AItem).Caption := Value
          else
          if AItem is TfcxAxisField then
            TfcxAxisField(AItem).Caption := Value;
          Grid.Slice.EndUpdate;
          InvalidateItem(FPopupIndex);
        end;
      end;
    4: if AItem is TfcxMeasuresContainer then
         Grid.Slice.PageContainer.InsertMeasuresField(0)
       else
       if AItem is TfcxAxisField then
         Grid.Slice.PageContainer.AddFilterField(TfcxAxisField(AItem).SliceField, TfcxAxisField(AItem).Name, TfcxAxisField(AItem).Caption);
    6:
       begin
         Ranges := TfcxRanges.Create;
         try
           if AItem is TfcxAxisField then
           begin
             if TfcxAxisField(AItem).SliceField.DataType in fcxStringTypes then
               Ranges.DefaultCompareObject := rcoText
             else
             if TfcxAxisField(AItem).SliceField.DataType in fcxDateTypes then
               Ranges.DefaultCompareObject := rcoDate
             else
               Ranges.DefaultCompareObject := rcoValue;
             Ranges.Add;
             if TfcxRangesEditor.Edit(Ranges) then
               TfcxAxisField(AItem).SliceField.SetRangeFilter(Ranges)
           end;
         finally
           Ranges.Free;
         end;
       end;
    7: if AItem is TfcxMeasuresContainer then
         TfcxMeasuresContainer(AItem).SetAllVisible
       else
       if AItem is TfcxAxisField then
         TfcxAxisField(AItem).SliceField.SetAllFilter;
    9: if AItem is TfcxMeasuresContainer then
         TfcxAxisContainer(FFields.Container).SetExpandedLevel(TfcxAxisContainer(FFields.Container).MeasuresLevel, False)
       else
       if AItem is TfcxAxisField then
         TfcxAxisContainer(FFields.Container).SetExpandedLevel(TfcxAxisContainer(FFields.Container).LevelOfRegionField[FFields.IndexOf[TfcxAxisField(AItem)]], False);
    10: if AItem is TfcxMeasuresContainer then
         TfcxAxisContainer(FFields.Container).SetExpandedLevel(TfcxAxisContainer(FFields.Container).MeasuresLevel, True)
       else
       if AItem is TfcxAxisField then
         TfcxAxisContainer(FFields.Container).SetExpandedLevel(TfcxAxisContainer(FFields.Container).LevelOfRegionField[FFields.IndexOf[TfcxAxisField(AItem)]], True);
    13:
      if AItem is TfcxAxisField then
      begin
        // create a new group
        S := fcxResources.Get('sNewGroup');
        if InputQuery(fcxResources.Get('sCreateANewGroup'), fcxResources.Get('sEnterANewGroupName'), S) then
        begin
          TfcxAxisField(AItem).SliceField.CanGroup := True;
          TfcxAxisField(AItem).SliceField.GroupManager.CreateGroup(S);
        end
        else
          Exit;
      end;
    14:
      if AItem is TfcxAxisField then
      begin
        S := fcxResources.Get('sOther');
        if InputQuery(fcxResources.Get('sCreateOtherGroup'), fcxResources.Get('sEnterOtherGroupCaption'), S) then
        begin
          TfcxAxisField(AItem).SliceField.CanGroup := True;
          TfcxAxisField(AItem).SliceField.GroupManager.CreateOtherGroup;
          TfcxAxisField(AItem).SliceField.GroupManager.OtherCaption := S;
        end
      end;
    15:
      if AItem is TfcxMeasuresContainer then
      begin
        NewMeasure := TfcxMeasureField.Create(Grid.Slice.MeasuresContainer.MeasureFields, af_None, nil, '', '');
        with TfcxMeasureEditorForm.Create(Grid) do
          if Execute(Grid.Slice, NewMeasure) then
            Grid.Slice.MeasuresContainer.AddMeasure(NewMeasure)
          else
            NewMeasure.Free;
      end;
    16:
      begin
        with TfcxTopNDialogForm.Create(Grid) do
        begin
          if Execute(TfcxAxisContainer(FFields.Container), TfcxAxisField(AItem)) then
            Grid.Slice.TopNs.SetTopN(Dimension, Measure, TopType, TopCount, CreateOthers);
          Free;
        end;
      end;
    17:
      begin
        if Grid.Slice.TopNs.AsFilter then
          Grid.Slice.TopNs.ClearTopN
        else
          TfcxAxisField(AItem).TopNProcessor.Active := False;
      end;
    18:
      if AItem is TfcxAxisField then
      begin
        with TfcxDimensionEditorForm.Create(Grid) do
          Execute(Grid.Slice, TfcxAxisField(AItem));
      end;
  end;
end;

procedure TfcxSliceItemsZone.SortPopupItemClick(Sender: TObject);
var
  AItem: TObject;
  SD: TfcxSortDirection;
begin
  AItem := GetItem(FPopupIndex);
  SD := TfcxSortDirection((Sender as TMenuItem).MenuIndex);
  if AItem is TfcxAxisField then
  begin
    TfcxAxisField(AItem).SortDirection := SD;
    InvalidateItem(FPopupIndex);
  end;
end;

procedure TfcxSliceItemsZone.TotalsPopupItemClick(Sender: TObject);
var
  AItem: TObject;
  Index: Integer;
  AF: TfcxAgrFunc;
begin
  AItem := GetItem(FPopupIndex);
  if AItem is TfcxAxisField then
  begin
    Index := (Sender  as TMenuItem).MenuIndex;
    if Index <= Ord(High(TfcxTotalPosition)) then
      TfcxAxisField(AItem).TotalPosition := TfcxTotalPosition(Index)
    else
    if Index = Ord(High(TfcxTotalPosition)) + 1 then
      TfcxAxisField(AItem).UseTotalPositionFromMeasure := not TfcxAxisField(AItem).UseTotalPositionFromMeasure
    else
    begin
      Dec(Index, Ord(High(TfcxTotalPosition)) + 3);
      AF := TfcxAgrFunc(Succ(Index));
      if AF in TfcxAxisField(AItem).AdditionalTotalFunctions then
        TfcxAxisField(AItem).AdditionalTotalFunctions := TfcxAxisField(AItem).AdditionalTotalFunctions - [AF]
      else
        TfcxAxisField(AItem).AdditionalTotalFunctions := TfcxAxisField(AItem).AdditionalTotalFunctions + [AF];
    end;
  end;
end;

procedure TfcxSliceItemsZone.AlignmentPopupItemClick(Sender: TObject);
var
  AItem: TObject;
  AL: TAlignment;
begin
  AItem := GetItem(FPopupIndex);
  case (Sender as TMenuItem).MenuIndex of
    0: AL := taLeftJustify;
    1: AL := taCenter;
    2: AL := taRightJustify;
    else
      AL := taLeftJustify;
  end;
  if AItem is TfcxAxisField then
    TfcxAxisField(AItem).Alignment := AL
  else
  if AItem is TfcxMeasuresContainer then
    TfcxMeasuresContainer(AItem).Alignment := AL;
end;

function TfcxSliceItemsZone.AcceptDrag(DragItem: TfcxCustomDragItem): Boolean;
var
  Item: TObject;
begin
  Item := DragItem.GetItem;
  Result := (Item is TfcxMeasuresContainer) or
            (Item is TfcxAxisField) or
            (Item is TfcxSliceField) or
            ((Item is TfcxMeasureField) and Assigned(TfcxMeasureField(Item).SliceField));
end;

procedure TfcxSliceItemsZone.DateSplitPopupItemClick(Sender: TObject);
var
  DateSplit: TfcxDateType;
  AItem: TObject;
begin
  AItem := GetItem(FPopupIndex);
  DateSplit := TfcxDateType((Sender as TMenuItem).MenuIndex + 1);
  TfcxAxisField(AItem).SliceField.SplitManager.UseDateSplit := True;

  if TfcxAxisField(AItem).SliceField.SplitManager.DatePathsManager.UseDatePath[DateSplit] then
    Grid.Slice.Cube.RemoveDatePath(TfcxCommonSliceCubeField(TfcxAxisField(AItem).SliceField).CubeField, DateSplit)
  else
    Grid.Slice.Cube.AddDatePath(TfcxCommonSliceCubeField(TfcxAxisField(AItem).SliceField).CubeField, DateSplit);
end;

procedure TfcxSliceItemsZone.TimeSplitPopupItemClick(Sender: TObject);
var
  TimeSplit: TfcxTimeType;
  AItem: TObject;
begin
  AItem := GetItem(FPopupIndex);
  TimeSplit := TfcxTimeType((Sender as TMenuItem).MenuIndex + 1);
  TfcxAxisField(AItem).SliceField.SplitManager.UseTimeSplit := True;
  if TfcxAxisField(AItem).SliceField.SplitManager.TimePathsManager.UseTimePath[TimeSplit] then
    Grid.Slice.Cube.RemoveTimePath(TfcxCommonSliceCubeField(TfcxAxisField(AItem).SliceField).CubeField, TimeSplit)
  else
    Grid.Slice.Cube.AddTimePath(TfcxCommonSliceCubeField(TfcxAxisField(AItem).SliceField).CubeField, TimeSplit);
end;

function TfcxSliceItemsZone.GetItemText(Item: TObject): TfcxString;
begin
  if Item is TfcxCommonFieldOfRegion then
    Result := TfcxCommonFieldOfRegion(Item).Caption
  else
  if Item is TfcxMeasuresContainer then
    Result := TfcxMeasuresContainer(Item).Caption + Format(' (%d)', [TfcxMeasuresContainer(Item).Count])
  else
    Result := inherited GetItemText(Item);
end;

function TfcxSliceItemsZone.GetItemSortType(Item: TObject): TfcxThemeSortType;
const
  DirectionMap: array[TfcxSortDirection] of TfcxThemeSortType = (tstUp, tstDown);
begin
  if Item is TfcxAxisField then
    Result := DirectionMap[TfcxAxisField(Item).SortDirection]
  else
    Result := inherited GetItemSortType(Item);
end;

function TfcxSliceItemsZone.CreatePopupWindow(Item: TObject): TfcxPopupWindow;
begin
  if Item is TfcxAxisField then
  begin
    Result := TfcxFilterPopup.Create(Owner);
    TfcxFilterPopup(Result).Field := TfcxAxisField(Item).SliceField;
  end
  else
  if Item is TfcxMeasuresContainer then
  begin
    Result := TfcxMeasurePopup.Create(Owner);
    TfcxMeasurePopup(Result).Container := TfcxMeasuresContainer(Item);
  end
  else
    Result := inherited CreatePopupWindow(Item);
end;

function TfcxSliceItemsZone.GetAllowDrag: Boolean;
begin
  Result := True;
end;

procedure TfcxSliceItemsZone.SetItemWidth(AIndex: Integer;
  const Value: Integer);
var
  Item: TObject;
begin
  Item := GetItem(AIndex);
  if Item is TfcxCommonFieldOfRegion then
    TfcxCommonFieldOfRegion(Item).CaptionWidth := Value
  else
  if Item is TfcxMeasuresContainer then
    TfcxMeasuresContainer(Item).CaptionWidth := Value;
end;

function TfcxSliceItemsZone.GetEmptyText: String;
begin
  if Region in [rf_Page, rf_CapXAx] then
    Result := fcxResources.Get(sRegionCaptions[Region])
  else
    Result := inherited GetEmptyText;
end;

procedure TfcxSliceItemsZone.DateAttributeClick(Sender: TObject);
var
  AItem: TObject;
begin
  AItem := GetItem(FPopupIndex);
  TfcxAxisField(AItem).SliceField.SplitManager.AttributesManager.ExistsDateAttribute :=
    not TfcxAxisField(AItem).SliceField.SplitManager.AttributesManager.ExistsDateAttribute;
end;

procedure TfcxSliceItemsZone.TimeAttributeClick(Sender: TObject);
var
  AItem: TObject;
begin
  AItem := GetItem(FPopupIndex);
  TfcxAxisField(AItem).SliceField.SplitManager.AttributesManager.ExistsTimeAttribute :=
    not TfcxAxisField(AItem).SliceField.SplitManager.AttributesManager.ExistsTimeAttribute;
end;

{ TfcxSliceGridDragItem }

function TfcxSliceGridDragItem.GetItem: TObject;
begin
  Result := ItemsZone.GetItem(ItemIndex);
end;

procedure TfcxSliceGridDragItem.InitDrag(ItemBitmap, ItemMask: TBitmap;
  AItemsZone: TfcxSliceCustomItemsZone; AItemIndex: Integer);
begin
  if not InDrag then
  begin
    FItemsZone := AItemsZone;
    FItemIndex := AItemIndex;
    inherited InitDrag(ItemBitmap, ItemMask);
  end;
end;

{ TfcxSliceGridPointer }

procedure TfcxSliceGridPointer.BeginDrag(ARect: TRect);
begin
  if not Active then
  begin
    FPaintRect := Rect(0, 0, 0, 0);
    FActive := True;
  end;
end;

constructor TfcxSliceGridPointer.Create(AOwner: TWinControl);
var
  Tmp: TBitmap;
  i, dx: integer;
begin
  FOwner := AOwner;
  FActive := False;
  FPaintRect := Rect(0, 0, 0, 0);

  Tmp := LoadBitmap('FCXPOSER');
  Tmp.TransparentColor := Tmp.Canvas.pixels[0, 0];
  Tmp.Transparent := True;

  dx := Tmp.Height;
  for I := 0 to 1 do
  begin
    FBitmaps[i] := TBitmap.Create;
    FBitmaps[i].Width := dx;
    FBitmaps[i].Height := dx;
    FBitmaps[i].Transparent := True;
    FBitmaps[i].Canvas.CopyRect(Rect(0, 0, dx, dx), Tmp.Canvas, Rect(i*dx, 0, (i + 1)*dx, dx));
  {$IFDEF FPC}
    CorrectFPCBitmap(FBitmaps[i]);
    FBitmaps[i].TransparentColor := Tmp.TransparentColor;
  {$ENDIF}
  end;
  Tmp.Free;
end;

destructor TfcxSliceGridPointer.Destroy;
var
  i: integer;
begin
  for i := Low(FBitmaps) to High(FBitmaps) do
    FBitmaps[i].Free;
  inherited;
end;

procedure TfcxSliceGridPointer.DragTo(ACanvas: TCanvas; ARect: TRect;
  Position: TfcxDragPosition; FullRect: Boolean);
begin
  // Fix ARect

  if FullRect then
    InflateRect(ARect, 0, FBitmaps[0].Height)
  else
    with ARect do
    begin
      if Position = fcdpBefore then
        Left := Left - FBitmaps[0].Width shr 1 - 1
      else
        Left := Right - FBitmaps[0].Width shr 1 + 1;
      Right := Left + FBitmaps[0].Width;
      Top := Max(0, Top - FBitmaps[0].Height);
      Bottom := Bottom + FBitmaps[0].Height;
    end;

  if not Active then
    BeginDrag(ARect);

  FFullRect := FullRect;
  if not EqualRect(FPaintRect, ARect) then
  begin
    Restore;
    FPaintRect := ARect;
    InvalidateRect(FOwner.Handle, @FPaintRect, False);
  end;
end;

procedure TfcxSliceGridPointer.EndDrag;
begin
  if Active then
  begin
    FActive := False;
    Restore;
    FPaintRect := Rect(0, 0, 0, 0);
  end;
end;

procedure TfcxSliceGridPointer.Redraw(ACanvas: TCanvas);
var
  OldWidth: integer;
  ARect: TRect;
begin
  if not IsRectEmpty(FPaintRect) then
  begin
    OldWidth := ACanvas.Pen.Width;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Width := 2;
    if not FFullRect then
    begin
      ACanvas.Draw(FPaintRect.Left, FPaintRect.Top, FBitmaps[0]);
      ACanvas.Draw(FPaintRect.Left, FPaintRect.Bottom - FBitmaps[1].Height, FBitmaps[1]);
      ACanvas.MoveTo(FPaintRect.Left + FBitmaps[0].Width shr 1, FPaintRect.Top + FBitmaps[0].Height);
      ACanvas.LineTo(FPaintRect.Left + FBitmaps[0].Width shr 1, FPaintRect.Bottom- FBitmaps[0].Height);
    end else
    begin
      ACanvas.Draw((FPaintRect.Left + FPaintRect.Right - FBitmaps[0].Width) shr 1,
                   FPaintRect.Top, FBitmaps[0]);
      ACanvas.Draw((FPaintRect.Left + FPaintRect.Right - FBitmaps[1].Width) shr 1,
                   FPaintRect.Bottom - FBitmaps[1].Height, FBitmaps[1]);

      ARect := FPaintRect;
      InflateRect(ARect, -1, -FBitmaps[0].Height);
      ACanvas.Polyline(
        [ARect.TopLeft, Point(ARect.Right, ARect.Top),
         Point(ARect.Right, ARect.Bottom), Point(ARect.Left, ARect.Bottom),
         ARect.TopLeft]);
    end;
    ACanvas.Pen.Width := OldWidth;
  end;
end;

procedure TfcxSliceGridPointer.Restore;
begin
  if not IsRectEmpty(FPaintRect) then
    InvalidateRect(FOwner.Handle, @FPaintRect, False);
end;

{ TfcxCellInfo }

procedure TfcxCellInfo.Assign(AInfo: TfcxCellInfo);
begin
  FData := AInfo.FData; 
  FGraphicSize := AInfo.GraphicSize;
  FBoundingRect := AInfo.BoundingRect;
  FTreeButtonPos := AInfo.TreeButtonPos;
  FHierButtonPos := AInfo.HierButtonPos;
  FIsSelected := AInfo.IsSelected;
end;

constructor TfcxCellInfo.Create(AOwner: TfcxCellInfos; AData: TfcxSliceDrawHeader);
begin
  Create(AOwner);
  FData := AData;
end;

constructor TfcxCellInfo.Create(AOwner: TfcxCellInfos);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TfcxCellInfo.SetBoundingRect(const Value: TRect);
begin
  if not EqualRect(FBoundingRect, Value) then
  begin
    FBoundingRect := Value;
    if Assigned(FOwner) then
      FOwner.FixRects(Self);
  end;
end;

{ TfcxSliceGridStyles }

function TfcxSliceGridStyles.GetLastStyleIndex: Integer;
begin
  Result := gsLastSliceGridStyle;
end;

function TfcxSliceGridStyles.GetStyle(Index: Integer): TfcxCustomThemeStyle;
begin
  if Index < gsFirstSliceGridStyle then
    Result := inherited GetStyle(Index)
  else
    Result := FStyles[Index]
end;

function TfcxSliceGridStyles.GetStyleName(Index: Integer): String;
begin
  if (Index >= gsFirstSliceGridStyle) and (Index <= gsLastSliceGridStyle) then
    Result := fcxResources.Get(fcxSliceGridStyleNames[Index])
  else
    Result := inherited GetStyleName(Index);
end;

procedure TfcxSliceGridStyles.InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle);
begin
  if Index < gsFirstSliceGridStyle then
    inherited InternalSetStyle(Index, Value)
  else
  begin
    Value.Index := Index;
    FStyles[Index] := Value;
  end
end;

procedure TfcxSliceGridStyles.SetDefaultValues;
begin
  inherited;
  ActiveDimension.Update(clActiveCaption, clGradientActiveCaption, clCaptionText, tgdHorizontal);
  InactiveDimension.Update(clInactiveCaption, clGradientInactiveCaption, clInactiveCaptionText, tgdHorizontal);
  Measure.Update(clGreen, clMoneyGreen, clCaptionText, tgdHorizontal);
  DataCellsTotals.Update(clInfoBkStd, clWhite, clBlack);
  FieldsItem.Update(clMoneyGreen, clNone, clCaptionText);
end;

procedure TfcxSliceGridStyles.SetStyle(Index: Integer; const Value: TfcxCustomThemeStyle);
begin
  if Index < gsFirstSliceGridStyle then
    inherited SetStyle(Index, Value)
  else
    FStyles[Index].Assign(Value)
end;

{ TfcxSliceGridZone }

function TfcxSliceGridZone.GetGrid: TfcxSliceGrid;
begin
  Result := TfcxSliceGrid(Owner);
end;

{ TfcxSliceStatusZone }

procedure TfcxSliceStatusZone.AgrFuncItemClick(Sender: TObject);
var
  AgrFunc: TfcxSelAgrFunc;
begin
  AgrFunc := TfcxSelAgrFunc(Succ((Sender as TMenuItem).MenuIndex));
  if AgrFunc in FAgrFuncs then
    Exclude(FAgrFuncs, AgrFunc)
  else
    Include(FAgrFuncs, AgrFunc);
  InvalidateSection(0);
end;

function TfcxSliceStatusZone.ClientHitTest(P: TPoint): TfcxSliceStatusZoneClientPart;
var
  I: Integer;
begin
  for I := Low(FSectionRects) to High(FSectionRects) do
    if PtInRect(FSectionRects[I], P) then
    begin
      P.X := P.X - FSectionRects[I].Left;
      case I of
        0:
          begin
            Result := szpTopN;
            Exit;
          end;
        2:
          begin
            if P.X < 50 then
              Result := szpScaleButton
            else
              Result := szpNone;
            Exit;
          end;
      end;
    end;
  Result := szpNone;
end;

procedure TfcxSliceStatusZone.ClientPaint;
var
  CR, R: TRect;
  I, C, W, SW: Integer;
begin
  CR := ClientRect;
  Grid.Painter.DrawStatus(Canvas, CR, Grid.Styles.StatusArea);
  Canvas.Font.Assign(Grid.Styles.StatusArea.Font);
  Canvas.Font.Color := Grid.Styles.StatusArea.TextColor;
  C := GetSectionCount;
  SetLength(FSectionRects, C);
  W := CR.Right - CR.Left;
  for I := 0 to C - 1 do
  begin
    SW := GetSectionWidth(I);
    if SW <> - 1 then
      Dec(W, SW + 1);
  end;
  R := CR;
  for I := 0 to C - 1 do
  begin
    SW := GetSectionWidth(I);
    if SW = -1 then
      SW := W;
    R.Right := R.Left + SW;
    FSectionRects[I] := R;
    DrawSection(I);
    R.Left := R.Right + 1;
  end;
end;

procedure TfcxSliceStatusZone.ContextPopup(X, Y: Integer; var Handled: Boolean);
begin
  Handled := True;
  PopupMenuNeeded;
  PreparePopupMenu;
  with ClientToScreen(Point(X, Y)) do
    PopupMenu.Popup(X, Y);
end;

constructor TfcxSliceStatusZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  HasFrame := False;
  FCurrentSection := -1;
  FAgrFuncs := [af_Sum, af_Count, af_Avg];
  FHasScale := True;
  FHasScaleSlider := False;
  FFormat := TfcxFormat.Create;
  FFormat.TypeFormat.Kind := fkNumeric;
  FFormat.TypeFormat.FormatStr := '#0.##';
end;

function TfcxSliceStatusZone.CreatePopupMenu: TPopupMenu;
const
  Items: array[0..3] of String = (
    'sCalcForSelection',
    '-',
    'sScale',
    'sScaleSlider'
  );
var
  I: Integer;
  Item: TMenuItem;
  AgrFunc: TfcxSelAgrFunc;
begin
  Result := inherited CreatePopupMenu;
  SetLength(FOwnMenuItems, Length(Items));
  for I := Low(Items) to High(Items) do
  begin
    FOwnMenuItems[I] := NewItem(fcxResources.Get(Items[I]), 0, False, True, PopupItemClick, 0, '');
    Result.Items.Add(FOwnMenuItems[I]);
  end;
  for AgrFunc := Succ(Low(TfcxSelAgrFunc)) to High(TfcxSelAgrFunc) do
  begin
    Item := NewItem(fcxResources.Get(sFuncCaptions[AgrFunc]), 0, False, True, AgrFuncItemClick, 0, '');
    FOwnMenuItems[0].Add(Item);
  end;
end;

destructor TfcxSliceStatusZone.Destroy;
begin
  FFormat.Free;
  inherited;
end;

procedure TfcxSliceStatusZone.DoAfterCloseUp(Sender: TObject; Cancel: Boolean);
begin
  if not Cancel then
  begin
    TfcxTopNDataProvider(Sender).ForEach(DoSetCheckState);
    InvalidateSection(0);
  end;
end;

procedure TfcxSliceStatusZone.DoPopupDestroy;
begin
  FPopupWindow := nil;
  InvalidateSection(0);
end;

procedure TfcxSliceStatusZone.DoGetCheckState(Sender: TObject; ANode, AData: Pointer);
begin
  if TfcxTopNProcessor(AData).Active then
    TfcxTopNDataProvider(Sender).NodeState[ANode] := csChecked
  else
    TfcxTopNDataProvider(Sender).NodeState[ANode] := csUnchecked;
end;

procedure TfcxSliceStatusZone.DoSetCheckState(Sender: TObject; ANode, AData: Pointer);
begin
  if TfcxTopNProcessor(AData).Active <> (TfcxTopNDataProvider(Sender).NodeState[ANode] = csChecked) then
    TfcxTopNProcessor(AData).Active := TfcxTopNDataProvider(Sender).NodeState[ANode] = csChecked;
end;

procedure TfcxSliceStatusZone.DrawArgFuncs(R: TRect);
var
  Text: TfcxString;
  AgrFunc: TfcxSelAgrFunc;
  AValue: Variant;
  W: Integer;
begin
  for AgrFunc := Low(TfcxSelAgrFunc) to High(TfcxSelAgrFunc) do
    if AgrFunc in FAgrFuncs then
    begin
      if Assigned(Grid.Slice) then
        AValue := Grid.Slice.CalcFuncForSelectedArea(AgrFunc, Grid.GetSelection)
      else
        AValue := UnAssigned;
      if VarIsNumeric(AValue) then
        Text := fcxResources.Get(sFuncCaptions[AgrFunc]) + ': ' + FFormat.FormatData(AValue, nil)
      else
        Text := fcxResources.Get(sFuncCaptions[AgrFunc]) + ': ';
      if High(FFuncWidth) < Ord(AgrFunc) then
        SetLength(FFuncWidth, Ord(AgrFunc) + 1);
      W := Canvas.TextWidth(Text);
      if W > FFuncWidth[Ord(AgrFunc)] then
        FFuncWidth[Ord(AgrFunc)] := W
      else
        W := FFuncWidth[Ord(AgrFunc)];
      R.Left := R.Right - W - 6;
      Grid.Painter.DrawText(Canvas, R, Text, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
      R.Right := R.Left;
    end;
end;

procedure TfcxSliceStatusZone.DrawScaleControls(R: TRect);
var
  State: TfcxThemeState;
begin
  // draw a scale button
  if HasScale then
  begin
    R.Right := R.Left + 50;
    if (ActiveClientPart = szpScaleButton) then
      if (GetKeyState(VK_LBUTTON) < -126) then
        State := tsPressed
      else
        State := tsHot
    else
      State := tsNormal;
    Grid.Painter.DrawToolButton(Canvas, R, State);
    Grid.Painter.DrawText(Canvas, R, IntToStr(Grid.Scale)+'%', DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    R.Left := R.Right;
  end;
  if HasScaleSlider then
  begin
    State := tsNormal;
    R.Right := R.Left + Grid.Painter.GetScaleSliderButtonSize;
    Grid.Painter.DrawScaleSliderButton(Canvas, R, Grid.Styles.StatusArea, State)
  end;
end;

procedure TfcxSliceStatusZone.DrawSection(AIndex: Integer);
var
  R: TRect;
begin
  R := FSectionRects[AIndex];
  if AIndex < GetSectionCount - 1 then
    Grid.Painter.DrawStatusPane(Canvas, R);
  case AIndex of
    0: DrawTopN(R);
    1: DrawArgFuncs(R);
    2: DrawScaleControls(R);
  end;
end;

procedure TfcxSliceStatusZone.DrawTopN(R: TRect);
var
  State: TfcxThemeState;
  ActiveCount: Integer;
begin
  if (ActiveClientPart = szpTopN) then
    if (GetKeyState(VK_LBUTTON) < -126) then
      State := tsPressed
    else
      State := tsHot
  else
    State := tsNormal;
  if Assigned(FPopupWindow) then
    State := tsPressed;
  Grid.Painter.DrawToolButton(Canvas, R, State);
  InflateRect(R, -2, -2);
  if Assigned(Grid.Slice) then
    ActiveCount := Grid.Slice.TopNs.ActiveCount
  else
    ActiveCount := 0;
  Grid.Painter.DrawText(Canvas, R, Format(fcxResources.Get('sActiveTopN'), [ActiveCount]), DT_LEFT or DT_VCENTER or DT_SINGLELINE);
end;

procedure TfcxSliceStatusZone.DropDown;
var
  P: TPoint;
begin
  FPopupWindow := TfcxTopNPopup.Create(Owner);
  TfcxTopNPopup(FPopupWindow).Slice := Grid.Slice;
  Grid.PreparePopup(FPopupWindow);
  if Assigned(FPopupWindow) then
  begin
    FPopupWindow.OnDestroy := DoPopupDestroy;
    TfcxTopNPopup(FPopupWindow).DataProvider.OnAfterCloseUp := DoAfterCloseUp;
    TfcxTopNPopup(FPopupWindow).DataProvider.ForEach(DoGetCheckState);
    FPopupWindow.Options := [pwoShowFooter, pwoAllowResize];
    P := ZoneToScreen(FSectionRects[0].TopLeft);
    dec(P.Y, FPopupWindow.Height);
    FPopupWindow.PopupAt(P);
  end;
end;

function TfcxSliceStatusZone.GetSectionAt(P: TPoint): Integer;
var
  I: Integer;
begin
  for I := Low(FSectionRects) to High(FSectionRects) do
    if PtInRect(FSectionRects[I], P) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TfcxSliceStatusZone.GetSectionCount: Integer;
begin
  Result := 2;
  if HasScale or HasScaleSlider then
    inc(Result);
end;

function TfcxSliceStatusZone.GetSectionWidth(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := 150;
    1: Result := -1;
    2:
      begin
        Result := 0;
        if HasScale then
          inc(Result, 50);
        if HasScaleSlider then
          inc(Result, 150);
      end;
  else
    Result := -1;
  end;
end;

procedure TfcxSliceStatusZone.InvalidateSection(AIndex: Integer);
var
  R: TRect;
begin
  if (AIndex > High(FSectionRects)) or (AIndex < Low(FSectionRects)) then
    Exit;
  R := FSectionRects[AIndex];
  Invalidate(False, @R);
end;

procedure TfcxSliceStatusZone.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
  InvalidateSection(FCurrentSection);
  if Button = mbLeft then
  begin
    case ActiveClientPart of
      szpTopN:
        if FPopupWindow = nil then
          DropDown;
    end;
  end;
end;

procedure TfcxSliceStatusZone.MouseLeave;
begin
  inherited;
  UpdateActiveClientPart(Point(-1, -1));
end;

procedure TfcxSliceStatusZone.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
end;

procedure TfcxSliceStatusZone.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
  InvalidateSection(FCurrentSection);
  if Button = mbLeft then
  begin
    case ActiveClientPart of
      szpScaleButton:
        begin
          with TfcxScaleDialogForm.Create(Grid) do
          begin
            Scale := Grid.Scale;
            if Execute then
              Grid.Scale := Scale;
          end;
        end;
    end;
  end;
end;

procedure TfcxSliceStatusZone.PopupItemClick(Sender: TObject);
begin
  case GetOwnMenuItemIndex(Sender) of
    2: HasScale := not HasScale;
    3: HasScaleSlider := not HasScaleSlider;
  end;
end;

procedure TfcxSliceStatusZone.PreparePopupMenu;
var
  I: Integer;
begin
  for I := 0 to FOwnMenuItems[0].Count - 1 do
    FOwnMenuItems[0][I].Checked := TfcxSelAgrFunc(Succ(I)) in FAgrFuncs;
  FOwnMenuItems[2].Checked := HasScale;
  FOwnMenuItems[3].Checked := HasScaleSlider;
end;

procedure TfcxSliceStatusZone.SelectionChanged(Sender: TZone);
begin
  InvalidateSection(1);
end;

procedure TfcxSliceStatusZone.SetActiveClientPart(const Value: TfcxSliceStatusZoneClientPart);
begin
  if FActiveClientPart <> Value then
  begin
    FActiveClientPart := Value;
    InvalidateSection(CurrentSection);
  end;
end;

procedure TfcxSliceStatusZone.SetAgrFuncs(const Value: TfcxSetAgrFunc);
begin
  if FAgrFuncs <> Value then
  begin
    FAgrFuncs := Value;
    invalidate;
  end;
end;

procedure TfcxSliceStatusZone.SetCurrentSection(const Value: Integer);
begin
  if FCurrentSection <> Value then
  begin
    InvalidateSection(FCurrentSection);
    FCurrentSection := Value;
    InvalidateSection(FCurrentSection);
  end;
end;

procedure TfcxSliceStatusZone.SetHasScale(const Value: Boolean);
begin
  if FHasScale <> Value then
  begin
    FHasScale := Value;
    invalidate;
  end;
end;

procedure TfcxSliceStatusZone.SetHasScaleSlider(const Value: Boolean);
begin
  if FHasScaleSlider <> Value then
  begin
    FHasScaleSlider := Value;
    invalidate;
  end;
end;

procedure TfcxSliceStatusZone.UpdateActiveClientPart(CursorPos: TPoint);
begin
  if ActivePart = zpBody then
  begin
    CursorPos := PointToClientPoint(CursorPos);
    CurrentSection := GetSectionAt(CursorPos);
    if CurrentSection <> -1 then
      ActiveClientPart := ClientHitTest(CursorPos)
    else
      ActiveClientPart := szpNone;
  end
  else
  begin
    CurrentSection := -1;
    ActiveClientPart := szpNone;
  end;
end;

procedure TfcxSliceStatusZone.FullUpdate;
begin
  Update;
end;

{ TfcxSliceFieldsZone }

function TfcxSliceFieldsZone.AcceptDrag(DragItem: TfcxCustomDragItem): Boolean;
var
  Item: TObject;
begin
  Item := DragItem.GetItem;
  Result := (Item is TfcxAxisField) or (Item is TfcxMeasureField);
end;

constructor TfcxSliceFieldsZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FDropDownFromWholeSurface := True;
end;

function TfcxSliceFieldsZone.CreatePopupWindow(Item: TObject): TfcxPopupWindow;
begin
  Result := TfcxSliceFieldsPopup.Create(Owner);
  TfcxSliceFieldsPopup(Result).Slice := Grid.Slice;
end;

procedure TfcxSliceFieldsZone.DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer);
var
  AItem: TObject;
begin
  UpdateActiveClientPart(Point(X, Y));
  if Assigned(Grid.FSlice) then
  begin
    AItem := DragItem.GetItem;
    if AItem is TfcxAxisField then
      Grid.Slice.RemoveSliceFieldFromContainers(TfcxAxisField(AItem).SliceField)
    else
    if AItem is TfcxMeasureField then
      Grid.Slice.MeasuresContainer.DeleteMeasure(TfcxMeasureField(AItem).Index);
  end;
end;

function TfcxSliceFieldsZone.GetItem(AIndex: Integer): TObject;
begin
  if Assigned(Grid.Slice) then
    Result := Grid.Slice.SliceFields
  else
    Result := nil;
end;

function TfcxSliceFieldsZone.GetItemCount: Integer;
begin
  if Assigned(Grid.Slice) then
    Result := 1
  else
    Result := 0;
end;

function TfcxSliceFieldsZone.GetItemStyle(Item: TObject): TfcxCustomThemeStyle;
begin
  Result := Grid.Styles.FieldsItem;
end;

function TfcxSliceFieldsZone.GetItemText(Item: TObject): TfcxString;
begin
  Result := ControlToString(fcxResources.Get('sFieldList'));
end;

function TfcxSliceFieldsZone.GetItemThemeOptions(Item: TObject): TfcxThemeButtonOptions;
begin
  Result := [tboHasDropDownButton];
end;

function TfcxSliceFieldsZone.GetItemWidth(AIndex: Integer): Integer;
begin
  Result := Grid.DefaultColWidth;
end;

initialization
{$IFDEF DELPHI_16UP}
{
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TfcxDefaultSettings, TControl);
  GroupDescendentsWith(TfcxDBDataSet, TControl);
  GroupDescendentsWith(TfcxUserDataSet, TControl);
  GroupDescendentsWith(TfcxDataSource, TControl);
  GroupDescendentsWith(TfcxFilterManager, TControl);
  GroupDescendentsWith(TfcxCube, TControl);
  GroupDescendentsWith(TfcxSlice, TControl);
  GroupDescendentsWith(TfcxSliceGrid, TControl);
}
{$ENDIF}
end.
