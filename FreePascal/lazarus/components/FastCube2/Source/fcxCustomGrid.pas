{*******************************************************}
{                                                       }
{            FastCube 2 custom grid unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxCustomGrid;

interface

{$INCLUDE fcx.inc}

uses
  {$ifndef fpc}
  Windows,
  {$else}
  LCLType, LCLIntf, LMessages,
  {$endif}
{$IFDEF DELPHI_6UP}
  Types,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms, Menus,
  fcxComponent, fcxControl, fcxDefaultSettings, fcxTypes, fcxPainters,
  fcxStyles, fcxGridPainters, fcxZone;

const
// custom grid styles
  gsCaptionArea = 0;
  gsHeaderArea = 1;
  gsHeaderCells = 2;
  gsHeaderCellsSelected = 3;
  gsDataArea = 4;
  gsDataCells = 5;
  gsDataCellsSelected = 6;
  gsStatusArea = 7;
  gsFirstCustomGridStyle = gsCaptionArea;
  gsLastCustomGridStyle = gsStatusArea;

  fcxCustomGridStyleNames: array[gsFirstCustomGridStyle..gsLastCustomGridStyle] of String = (
    'sCaptionArea',
    'sHeaderArea',
    'sHeaderCells',
    'sHeaderCellsSelected',
    'sDataArea',
    'sDataCells',
    'sDataCellsSelected',
    'sStatusArea'
  );

type
  TfcxCustomGridStyles = class(TfcxCustomThemeStyles)
  private
    FStyles: array[gsFirstCustomGridStyle..gsLastCustomGridStyle] of TfcxCustomThemeStyle;
  protected
    function GetStyle(Index: Integer): TfcxCustomThemeStyle; override;
    function GetStyleName(Index: Integer): String; override;
    procedure InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); override;
    procedure SetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); override;
    procedure SetDefaultValues; override;
    function GetFirstStyleIndex: Integer; override;
    function GetLastStyleIndex: Integer; override;
  published
    property CaptionArea: TfcxCustomThemeStyle index gsCaptionArea read GetStyle write SetStyle;
    property HeaderArea: TfcxCustomThemeStyle index gsHeaderArea read GetStyle write SetStyle;
    property HeaderCells: TfcxCustomThemeStyle index gsHeaderCells read GetStyle write SetStyle;
    property HeaderCellsSelected: TfcxCustomThemeStyle index gsHeaderCellsSelected read GetStyle write SetStyle;
    property DataArea: TfcxCustomThemeStyle index gsDataArea read GetStyle write SetStyle;
    property DataCells: TfcxCustomThemeStyle index gsDataCells read GetStyle write SetStyle;
    property DataCellsSelected: TfcxCustomThemeStyle index gsDataCellsSelected read GetStyle write SetStyle;
    property StatusArea: TfcxCustomThemeStyle index gsStatusArea read GetStyle write SetStyle;
  end;
  TfcxCustomGridStylesClass = class of TfcxCustomGridStyles;

  TfcxCustomGrid = class;
  
  TfcxCustomGridGetClipboardTextEvent = procedure(Sender: TfcxCustomGrid; out ClipboardText: TfcxString) of object;
  
  TfcxCustomGrid = class(TZoneContainer)
  private
    FPainter: TfcxCustomGridPainter;
    FUpdateCount: Integer;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FStyles: TfcxCustomGridStyles;
    FOnGetClipboardText: TfcxCustomGridGetClipboardTextEvent;
  protected
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure PaintEmptySpace; override;
    procedure DrawSizingLine; override;

    class function GetZoneClass: TZoneClass; override;
    class function GetStylesClass: TfcxCustomGridStylesClass; virtual;
    function GetPaintStyle: TfcxPaintStyle;
    procedure SetPaintStyle(const Value: TfcxPaintStyle); virtual;
    procedure SetDefaultColWidth(const Value: Integer); virtual;
    procedure SetDefaultRowHeight(const Value: Integer); virtual;
    procedure SetStyles(const Value: TfcxCustomGridStyles); virtual;
    function CreateScrollBar(AZone: TZone; AScrollBarKind: TScrollBarKind): TControl;
    procedure UpdateScrollBar(AZone: TZone; AScrollBar: TControl);
    procedure ScrollChange(Sender: TObject); virtual;
    procedure NotifyScroll(Sender: TZone; ScrollBar: TScrollBarKind; ScrollCode: TScrollCode; ScrollPos: Integer); virtual;
    procedure NotifySelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection); virtual;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function InUpdate: Boolean;
  public
    procedure SetDefaults; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CopyToClipboard; virtual;
    function GetClipboardText: TfcxString; virtual;
    
    procedure SetScrollParams(AZone: TZone; AScrollBar: TControl; APosition, AMin, AMax, ASmallChange, ALargeChange: Integer); virtual;
    procedure SetScrollPos(AZone: TZone; AScrollBar: TControl; APosition: Integer); virtual;
    function GetScrollParam(AZone: TZone; AScrollBar: TControl; AParam: TfcxScrollParam): Integer; virtual;

    property Painter: TfcxCustomGridPainter read FPainter;
    property PaintStyle: TfcxPaintStyle read GetPaintStyle write SetPaintStyle default psDefault;
    property Styles: TfcxCustomGridStyles read FStyles write SetStyles;
    property OnGetClipboardText: TfcxCustomGridGetClipboardTextEvent read FOnGetClipboardText write FOnGetClipboardText;
  published
    property DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default cfcColWidth;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default cfcRowHeight;
    property OnZoneContextPopup;
    property OnZoneCreatePopupMenu;
  end;

  TfcxGridZone = class(TZone)
  private
    FHasFrame: Boolean;
    function GetGrid: TfcxCustomGrid;
  protected
    function GetBorderSize: TSize; override;
    function GetHeaderSize: TSize; override;
    function GetExpandSignRect(ARect: TRect): TRect; override;

    procedure DrawFrame; override;
    procedure DrawHeader(ARect: TRect); override;
    procedure ClientPaint; override;
    function GetScrollerWidth(AScroller: TZoneScroller): Integer; override;
    function GetScrollerState(AScroller: TZoneScroller; AEnabled: Boolean): TfcxThemeState; virtual;
    procedure DrawScroller(ARect: TRect; AScroller: TZoneScroller; AEnabled: Boolean); override;

    procedure SetHasFrame(const Value: Boolean); virtual;

    // scroll handlers
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;

    property Grid: TfcxCustomGrid read GetGrid;
  public
    constructor Create(AOwner: TZoneContainer); override;
    // drag drop
    function AcceptDrag(DragItem: TfcxCustomDragItem): Boolean; virtual;
    procedure DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer); virtual;
    property HasFrame: Boolean read FHasFrame write SetHasFrame;
  end;

  TfcxCustomDataZonePointInfo = record
    PointType: Byte;
    Index: Integer;
  end;

  { TfcxCustomDataZone }

  TfcxCustomDataZone = class(TfcxGridZone)
  private
    FInUpdate: Boolean;
    FFirstVisibleRow: Integer;
    FFirstVisibleCol: Integer;
    FHorzScroll: TControl;
    FVertScroll: TControl;
    procedure SetPointInfo(const Value: TfcxCustomDataZonePointInfo); 
  protected
    FSaveSelection, FSelection: TfcxGridSelection;
    FPointInfo: TfcxCustomDataZonePointInfo;

    // offset of the first column from the client rectangle
    function GetFirstCellOffset: TPoint; virtual;
    function GetLastVisibleCol: Integer; virtual;
    function GetLastVisibleRow: Integer; virtual;
    procedure SetFirstVisibleCol(const Value: Integer); virtual;
    procedure SetFirstVisibleRow(const Value: Integer); virtual;

    // the last row to show on top
    function GetHorzScrollMax: Integer; virtual;
    function GetVertScrollMax: Integer; virtual;

    procedure UpdateCursor; override;
    // key handlers
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    // mouse handlers
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave; override;

    procedure DblClick(X: Integer; Y: Integer); override;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer): Boolean; override;
    // don't use 'const' modifier here since records are passed by reference in this case
    procedure UpdatePointInfo(OldInfo, NewInfo: TfcxCustomDataZonePointInfo); virtual;

    procedure SetBoundingRect(const Value: TRect); override;
    function CanUpdateScrolls: Boolean; virtual;
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer); override;

    // size
    function GetColCount: Integer; virtual;
    function GetRowCount: Integer; virtual;
    function GetColWidth(AIndex: Integer): Integer; virtual;
    function GetRowHeight(AIndex: Integer): Integer; virtual;
    procedure SetColWidth(AIndex: Integer; AWidth: Integer); virtual;
    procedure SetRowHeight(AIndex: Integer; AHeight: Integer); virtual;

    function GetRowIndex(ARow: Integer): Integer; virtual;
    function GetText(ACol, ARow: Integer): String; virtual;

    // selection
    function Selection_LastVisibleCol(FullyVisible: Boolean = False): Integer; virtual;
    function Selection_LastVisibleRow(FullyVisible: Boolean = False): Integer; virtual;
    function Selection_LastPosibleCol: Integer; virtual;
    function Selection_LastPosibleRow: Integer; virtual;
    procedure Selection_BeginUpdate; virtual;
    procedure Selection_EndUpdate; virtual;
    function GetRectForSelection(const ASelection: TfcxGridSelection): TRect; virtual;
    function Selection_GetRect: TRect; 
    function Selection_GetUpdateRect(NewFirstCol, NewFirstRow: Integer): TRect; virtual;
    procedure ScrollIfAtBorder(P: TPoint);

    procedure StartSelection(P: TPoint);
    procedure ContinueSelection(P: TPoint);
    procedure EndSelection(P: TPoint);
    procedure SetSelection(P: TfcxDataPoint); overload;
    procedure SetSelection(P: TPoint); overload;
    procedure ClearSelection;
    procedure RestoreSelection;
    procedure StartSelectionAt(ACol, ARow: integer; AddCell: Boolean);
    function CellFocusedMove(const DeltaX, DeltaY: Integer; AddCell: Boolean): Boolean;
    function CellFocused(i, j: integer): Boolean;
    function CellSelected(i, j: integer): Boolean;
    function GetAsPlainText: AnsiString; virtual;

    procedure UpdateScrolls(UpdateHorizontal, UpdateVertical: Boolean); virtual;
    function GetPointInfo(APoint: TPoint): TfcxCustomDataZonePointInfo; virtual;

    procedure StartSizing(var AInfo: TSizingInfo); override;
    procedure MoveSizing(var AInfo: TSizingInfo); override;
    procedure StopSizing(var AInfo: TSizingInfo); override;

    property PointInfo: TfcxCustomDataZonePointInfo read FPointInfo write SetPointInfo;
  public
    constructor Create(AOwner: TZoneContainer); override;
    destructor Destroy; override;

    function PointToDataPoint(P: TPoint): TfcxDataPoint; virtual;
    function DataPointToRect(P: TfcxDataPoint): TRect; virtual;

    procedure EnsureVisible(const ACol, ARow: Integer);
    procedure SelectCol(const ACol: Integer);
    procedure SelectCols(const AStartCol, AStopCol: Integer);
    procedure SelectRow(const ARow: Integer);
    procedure SelectRows(const AStartRow, AStopRow: Integer);
    procedure SelectAll;
    procedure SelectCell(const ACol, ARow: integer; AMakeVisible: Boolean);

    property ColWidth[AIndex: Integer]: Integer read GetColWidth write SetColWidth;
    property RowHeight[AIndex: Integer]: Integer read GetRowHeight write SetRowHeight;

    property FirstVisibleCol: Integer read FFirstVisibleCol write SetFirstVisibleCol;
    property FirstVisibleRow: Integer read FFirstVisibleRow write SetFirstVisibleRow;
    property LastVisibleCol: Integer read GetLastVisibleCol;
    property LastVisibleRow: Integer read GetLastVisibleRow;
    property SelectedArea: TfcxGridSelection read FSelection;

    property HorzScroll: TControl read FHorzScroll;
    property VertScroll: TControl read FVertScroll;
  end;

// point types
const
  ptNone = 0;
  ptHorzSplit = 1;
  ptVertSplit = 2;

// threshold for capturing splitters
  SplitThreshold = 1;

  ScrollerDirectionMap: array[TZoneScroller] of TfcxThemeDirection = (
 { zsLeft  } tdRight,
 { zsRight } tdLeft
  );

implementation

uses
  ClipBrd,
  Math,
  fcxRes;

{$IFNDEF FPC}
type
  THackClipboard = class(TClipBoard)
  end;
{$ENDIF}

{ TfcxCustomGrid }

constructor TfcxCustomGrid.Create(AOwner: TComponent);
begin
  inherited;
  FPainter := TfcxDefaultGridPainter.Create;
  FStyles := GetStylesClass.Create(Self);
end;

destructor TfcxCustomGrid.Destroy;
begin
  FPainter.Free;
  FStyles.Free;
  inherited;
end;

class function TfcxCustomGrid.GetZoneClass: TZoneClass;
begin
  Result := TfcxGridZone;
end;

function TfcxCustomGrid.GetPaintStyle: TfcxPaintStyle;
begin
  Result := Painter.GetPaintStyle;
end;

procedure TfcxCustomGrid.SetPaintStyle(const Value: TfcxPaintStyle);
begin
  if PaintStyle <> Value then
  begin
    FPainter.Free;
    FPainter := GridPainterClass[Value].Create;
    invalidate;
  end;
end;

function TfcxCustomGrid.CreateScrollBar(AZone: TZone;
  AScrollBarKind: TScrollBarKind): TControl;
begin
  Result := TScrollBar.Create(Self);
  Result.ControlStyle := Result.ControlStyle - [csFramed];
  Result.Parent := Self;
  with Result as TScrollBar do
  begin
    TabStop := False;
    Kind := AScrollBarKind;
    OnScroll := TfcxGridZone(AZone).ScrollScroll;
    OnChange := ScrollChange;
  end;
end;

procedure TfcxCustomGrid.UpdateScrollBar(AZone: TZone; AScrollBar: TControl);
begin
  if TScrollBar(AScrollBar).Kind = sbVertical then
  begin
    AScrollBar.Left := AZone.Left + AZone.Width;
    AScrollBar.Top := AZone.Top;
    AScrollBar.Height := Max(0, AZone.Height);
  end
  else
  begin
    AScrollBar.Left := AZone.Left;
    AScrollBar.Top := AZone.Top + AZone.Height;
    AScrollBar.Width := Max(0, AZone.Width);
  end;
end;

procedure TfcxCustomGrid.ScrollChange(Sender: TObject);
var
  AScrollPos: Integer;
begin
  AScrollPos := TScrollBar(Sender).Position;
  if Assigned(TScrollBar(Sender).OnScroll) then
    TScrollBar(Sender).OnScroll(Sender, scPosition, AScrollPos);
end;

procedure TfcxCustomGrid.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TfcxCustomGrid.EndUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyUpdate(nil);
end;

function TfcxCustomGrid.InUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TfcxCustomGrid.SetScrollParams(AZone: TZone; AScrollBar: TControl;
  APosition, AMin, AMax, ASmallChange, ALargeChange: Integer);
var
  ScrollBar: TScrollBar absolute AScrollBar;
begin
  if AMax < AMin then
    AMax := AMin;
  ScrollBar.SetParams(APosition, AMin, AMax);
  ScrollBar.SmallChange := ASmallChange;
  ScrollBar.LargeChange := ALargeChange;
end;

procedure TfcxCustomGrid.SetScrollPos(AZone: TZone; AScrollBar: TControl;
  APosition: Integer);
var
  ScrollBar: TScrollBar absolute AScrollBar;
begin
  ScrollBar.Position := APosition;
end;

function TfcxCustomGrid.GetScrollParam(AZone: TZone; AScrollBar: TControl;
  AParam: TfcxScrollParam): Integer;
var
  ScrollBar: TScrollBar absolute AScrollBar;
begin
  case AParam of
    spMin: Result := ScrollBar.Min;
    spMax: Result := ScrollBar.Max;
    spPosition: Result := ScrollBar.Position;
    spSmallChange: Result := ScrollBar.SmallChange;
    spLargeChange: Result := ScrollBar.LargeChange;
  else
    Result := 0;
  end;
end;

procedure TfcxCustomGrid.NotifyScroll(Sender: TZone; ScrollBar: TScrollBarKind; ScrollCode: TScrollCode;
  ScrollPos: Integer);
begin
  // nothing here
end;

procedure TfcxCustomGrid.SetDefaultColWidth(const Value: Integer);
begin
  FDefaultColWidth := Value;
end;

procedure TfcxCustomGrid.SetDefaultRowHeight(const Value: Integer);
begin
  FDefaultRowHeight := Value;
end;

procedure TfcxCustomGrid.SetDefaults;
begin
  inherited;
  DefaultColWidth := fcDefaultSettingsStore.ColWidth;
  DefaultRowHeight:= fcDefaultSettingsStore.RowHeight;
end;

procedure TfcxCustomGrid.SetStyles(const Value: TfcxCustomGridStyles);
begin
  FStyles.Assign(Value);
  Invalidate;
end;

class function TfcxCustomGrid.GetStylesClass: TfcxCustomGridStylesClass;
begin
  Result := TfcxCustomGridStyles;
end;

procedure TfcxCustomGrid.NotifySelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection);
begin
  // nothing here
end;

procedure TfcxCustomGrid.CMFontChanged(var Message: TMessage);
begin
  Styles.UpdateFonts;
end;

procedure TfcxCustomGrid.PaintEmptySpace;
begin
  Painter.DrawBody(Canvas, ClientRect, Styles.HeaderArea);
end;

procedure TfcxCustomGrid.DrawSizingLine;
var
  R: TRect;
begin
  if Assigned(Sizing.ActiveZone) then
  begin
    R.TopLeft := Sizing.DrawStart;
    R.BottomRight := Sizing.DrawStop;
    with Sizing.ActiveZone.BoundingRect do
      OffsetRect(R, Left, Top);
    Painter.DrawSplitLine(Canvas, R.TopLeft, R.BottomRight);
  end;
end;

procedure TfcxCustomGrid.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TfcxCustomGrid.CopyToClipboard;
  function AddToClipboard(Format: Word; var Buffer; Size: Integer): Boolean;
  begin
  {$IFDEF FPC}
    Result := Clipboard.AddFormat(Format, Buffer, Size);
  {$ELSE}
  Result := True;
  try
    THackClipboard(Clipboard).SetBuffer(Format, Buffer, Size);
  except
    Result := False;
  end;
  {$ENDIF}
  end;

var
  S: TfcxString;
  A: AnsiString;
{$IFNDEF FPC}
  W: WideString;
{$ENDIF}
begin
  Clipboard.Open;
  try
    Clipboard.Clear;
    if Assigned(OnGetClipboardText) then
      OnGetClipboardText(Self, S)
    else
      S := GetClipboardText;
    // 1. Add plain text format
    A := S;
    AddToClipboard(CF_TEXT, A[1], Length(A) + 1);
    {$IFNDEF FPC}
    // 2. Add unicode text format
    W := S;
    AddToClipboard(CF_UNICODETEXT, W[1], Length(W) * 2 + 1);
    {$ENDIF}
  finally
    Clipboard.Close;
  end;
end;

function TfcxCustomGrid.GetClipboardText: TfcxString;
begin
  Result := '';
end;

{ TfcxGridZone }

function TfcxGridZone.AcceptDrag(DragItem: TfcxCustomDragItem): Boolean;
begin
  Result := False;
end;

procedure TfcxGridZone.ClientPaint;
begin
  Grid.Painter.DrawBody(Canvas, ClientRect, Grid.Styles.CaptionArea);  
end;

constructor TfcxGridZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FHasFrame := True;
end;

procedure TfcxGridZone.DragDrop(DragItem: TfcxCustomDragItem; X, Y: Integer);
begin
end;

procedure TfcxGridZone.DrawFrame;
begin
  if HasFrame then
    Grid.Painter.DrawBorder(Canvas, Rect(0, 0, Width, Height), tsNormal);
end;

procedure TfcxGridZone.DrawHeader(ARect: TRect);
begin
  Grid.Painter.DrawHeader(Canvas, ARect, Caption, tsNormal);
  if zoCollapsable in Options then
    Grid.Painter.DrawTreeButton(Canvas, GetExpandSignRect(ARect).TopLeft, CollapsedToTreeButtonKind[not Expanded], Grid.Styles.HeaderArea, False);
end;

procedure TfcxGridZone.DrawScroller(ARect: TRect; AScroller: TZoneScroller; AEnabled: Boolean);
begin
  Grid.Painter.DrawScrollButton(Canvas, ARect, ScrollerDirectionMap[AScroller], GetScrollerState(AScroller, AEnabled))
end;

function TfcxGridZone.GetBorderSize: TSize;
begin
  Result.cx := Ord(HasFrame);
  Result.cy := Result.cx;
end;

function TfcxGridZone.GetExpandSignRect(ARect: TRect): TRect;
begin
  Result.Left := ARect.Left + 2;
  Result.Top := (ARect.Top + ARect.Bottom - TreeButtonSize) div 2;
  Result.Right := Result.Left + TreeButtonSize;
  Result.Bottom := Result.Top + TreeButtonSize;
end;

function TfcxGridZone.GetGrid: TfcxCustomGrid;
begin
  Result := TfcxCustomGrid(Owner); 
end;

function TfcxGridZone.GetHeaderSize: TSize;
var
  Extent: TSize;
begin
  Result.cy := TreeButtonSize;
  if GetGrid.HandleAllocated then
  begin
    Extent := Canvas.TextExtent('Wj');
    Result.cy := Max(Result.cy, Extent.cy) + 4; // 2 = 1 + 1 margin
  end;  
  Result.cx := Result.cy;
end;

function TfcxGridZone.GetScrollerState(AScroller: TZoneScroller; AEnabled: Boolean): TfcxThemeState;
begin
  if not AEnabled then
    Result := tsDisabled
  else
    if ActivePart = ScrollerToPart[AScroller] then
    begin
      if GetKeyState(VK_LBUTTON) < -126 then
        Result := tsPressed
      else
        Result := tsHot
    end
    else
      Result := tsNormal;
end;

function TfcxGridZone.GetScrollerWidth(AScroller: TZoneScroller): Integer;
begin
  Result := Grid.Painter.GetScrollButtonSize;
end;

procedure TfcxGridZone.ScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  // virtual
end;

procedure TfcxGridZone.SetHasFrame(const Value: Boolean);
begin
  if FHasFrame <> Value then
  begin
    FHasFrame := Value;
    Invalidate;
  end;
end;

{ TfcxCustomDataZone }

function TfcxCustomDataZone.CellFocused(i, j: integer): Boolean;
begin
  Result := (FSelection.Start.x = i) and (FSelection.Start.y = j);
end;

function TfcxCustomDataZone.CellFocusedMove(const DeltaX, DeltaY: Integer; AddCell: Boolean): Boolean;
var
  DataPoint: TfcxDataPoint;
begin
  if AddCell then
    DataPoint := FSelection.Last
  else
    DataPoint := FSelection.Start;
  Result := False;
  if DeltaX <> 0 then
  begin
    DataPoint.x := Min(Selection_LastPosibleCol, Max(0, DataPoint.x + DeltaX));
    Result := Result or
              ((DataPoint.x > Selection_LastVisibleCol(True)) and (DeltaX > 0)) or
              ((DataPoint.x < FFirstVisibleCol) and (DeltaX < 0));
  end
  else
  if DataPoint.x = -1 then
    DataPoint.x := 0;

  if DeltaY <> 0 then
  begin
    DataPoint.y := Min(Selection_LastPosibleRow, Max(0, DataPoint.y + DeltaY));
    Result := Result or
              ((DataPoint.y > Selection_LastVisibleRow(True)) and (DeltaY > 0)) or
              ((DataPoint.y < FFirstVisibleRow) and (DeltaY < 0));
  end
  else
  if DataPoint.y = -1 then
    DataPoint.y := 0;

  if AddCell then
  begin
    FSelection.Fixed := False;
    SetSelection(DataPoint);
  end else
  begin
    FSelection.Fixed := True;
    FSelection.Start := DataPoint;
    FSelection.Rect.LeftTop := DataPoint;
    FSelection.Rect.BottomRight := DataPoint;
    FSelection.Last := DataPoint;
  end;
end;

function TfcxCustomDataZone.CellSelected(i, j: integer): Boolean;
begin
  Result := (FSelection.Rect.Left <> -1) and
            (
             (FSelection.Rect.Top <= j) and
             (FSelection.Rect.Bottom >= j) and
             (FSelection.Rect.Left <= i) and
             (FSelection.Rect.Right >= i)
            );
end;

procedure TfcxCustomDataZone.ClearSelection;
begin
  FSaveSelection := FSelection;
  FSelection.Fixed := True;
  FSelection.Rect := fcxDataRect(-1, -1, -1, -1);
  FSelection.Start := fcxDataPoint(-1, -1);
end;

procedure TfcxCustomDataZone.ContinueSelection(P: TPoint);
begin
  if FSelection.Rect.Left = -1 then
    StartSelection(P)
  else
  begin
    FSelection.Fixed := False;
    SetSelection(P);
  end;
end;

constructor TfcxCustomDataZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FInUpdate := False;
  FFirstVisibleCol := 0;
  FFirstVisibleRow := 0;
  ClearSelection;
  FSaveSelection := FSelection;
  FPointInfo.PointType := ptNone;

  FVertScroll := Grid.CreateScrollBar(Self, sbVertical);
  FVertScroll.Enabled := False;
  FHorzScroll := Grid.CreateScrollBar(Self, sbHorizontal);
  FHorzScroll.Enabled := False;
end;

function TfcxCustomDataZone.DataPointToRect(P: TfcxDataPoint): TRect;
var
  i: Integer;
begin
  Result := ClientRect;
  with GetFirstCellOffset do
  begin
    inc(Result.Left, X);
    inc(Result.Top, Y);
  end;

  for i := FirstVisibleCol to P.x - 1 do
    Inc(Result.Left, ColWidth[i]);

  for i := FirstVisibleRow to P.y - 1 do
    Inc(Result.Top, RowHeight[i]);

  Result.Right := Result.Left + ColWidth[P.x];
  Result.Bottom := Result.Top + RowHeight[P.y];
end;

procedure TfcxCustomDataZone.DblClick(X: Integer; Y: Integer);
begin
  ReleaseCapture;
  PostMessage(Owner.Handle, WM_LBUTTONUP, 0, Integer($FFFFFFFF));
  inherited;
end;

destructor TfcxCustomDataZone.Destroy;
begin
  FVertScroll.Free;
  FHorzScroll.Free;

  inherited;
end;

procedure TfcxCustomDataZone.EndSelection(P: TPoint);
begin
  SetSelection(P);
  FSelection.Fixed := True;
end;

function TfcxCustomDataZone.GetColCount: Integer;
begin
  Result := 0;
end;

function TfcxCustomDataZone.GetColWidth(AIndex: Integer): Integer;
begin
  Result := Grid.DefaultColWidth;
end;

function TfcxCustomDataZone.GetHorzScrollMax: Integer;
var
  MaxSize, CurSize: Integer;
begin
  Result := GetColCount - 1;
  MaxSize := ClientWidth - GetFirstCellOffset.X;
  CurSize := 0;
  while (CurSize <= MaxSize) and (Result >= 0) do
  begin
    Inc(CurSize, ColWidth[Result]);
    Dec(Result);
  end;
  if CurSize > MaxSize then
    Inc(Result);
end;

function TfcxCustomDataZone.GetVertScrollMax: Integer;
var
  MaxSize, CurSize: Integer;
begin
  MaxSize := ClientHeight - GetFirstCellOffset.Y;
  Result := GetRowCount - 1;
  CurSize := 0;
  while (CurSize <= MaxSize) and (Result >= 0) do
  begin
    Inc(CurSize, RowHeight[Result]);
    Dec(Result);
  end;
  if CurSize > MaxSize then
    Inc(Result);
end;

procedure TfcxCustomDataZone.UpdateCursor;
begin
  if ActivePart in [zpHeader, zpBody] then
    case PointInfo.PointType of
      ptHorzSplit: Owner.Cursor := crHSplit;
      ptVertSplit: Owner.Cursor := crVSplit;
    else
      Owner.Cursor := crDefault;
    end
  else
    inherited UpdateCursor;
end;

function TfcxCustomDataZone.GetLastVisibleCol: Integer;
var
  ATempWidth : Integer;
  MaxIndex, MaxWidth : Integer;
begin
  MaxIndex := GetColCount - 1;
  if MaxIndex < 0 then
  begin
    Result := -1;
    Exit;
  end;
  ATempWidth := 0;
  Result := FirstVisibleCol;
  if Result > MaxIndex then
  begin
    Result := MaxIndex;
    Exit;
  end;
  if Result >= 0 then
  begin
    MaxWidth := ClientWidth - GetFirstCellOffset.X;
    while (ATempWidth < MaxWidth) and (Result < MaxIndex) do
    begin
      Inc(ATempWidth, ColWidth[Result]);
      Inc(Result);
    end;
    if ATempWidth > MaxWidth then
      Dec(Result);
  end;
end;

function TfcxCustomDataZone.GetLastVisibleRow: Integer;
var
  ATempHeight: Integer;
  MaxIndex, MaxHeight: Integer;
begin
  MaxIndex := GetRowCount - 1;
  if (MaxIndex < 0) then
  begin
    Result := -1;
    Exit;
  end;
  ATempHeight := 0;
  Result := FirstVisibleRow;
  if Result >= 0 then
  begin
    MaxHeight := ClientHeight - GetFirstCellOffset.Y;
    while (ATempHeight < MaxHeight) and (Result < MaxIndex) do
    begin
      Inc(ATempHeight, RowHeight[Result]);
      Inc(Result);
    end;
    if ATempHeight > MaxHeight then
      Dec(Result);
  end;
end;

function TfcxCustomDataZone.GetPointInfo(APoint: TPoint): TfcxCustomDataZonePointInfo;
var
  Cur, i: Integer;
begin
  with FrameRect do
  begin
    Dec(APoint.X, Left);
    Dec(APoint.Y, Top);
  end;

  Result.PointType := ptNone;
  Result.Index := -1;

  // check if we are on horizontal splitter
  Cur := GetFirstCellOffset.X;
  for i := FirstVisibleCol to LastVisibleCol do
  begin
    inc(Cur, ColWidth[i]);
    if Abs(APoint.X - Cur) <= SplitThreshold then
    begin
      Result.PointType := ptHorzSplit;
      Result.Index := i;
      Exit;
    end;
    if Cur > APoint.X then
      Break;
  end;

  Cur := GetFirstCellOffset.Y;
  for i := FirstVisibleRow to LastVisibleRow do
  begin
    inc(Cur, RowHeight[i]);
    if Abs(APoint.Y - Cur) <= SplitThreshold then
    begin
      Result.PointType := ptVertSplit;
      Result.Index := i;
      Exit;
    end;
    if Cur > APoint.Y then
      Break;
  end;
end;

function TfcxCustomDataZone.GetRowCount: Integer;
begin
  Result := 0;
end;

function TfcxCustomDataZone.GetRowHeight(AIndex: Integer): Integer;
begin
  Result := Grid.DefaultRowHeight;
end;

function TfcxCustomDataZone.GetRowIndex(ARow: Integer): Integer;
begin
  Result := ARow;
end;

procedure TfcxCustomDataZone.KeyDown(var Key: Word; Shift: TShiftState);
var
  NeedInvalidate: Boolean;
  NewCol, NewRow: Integer;
  ARect: TRect;

  procedure SetScroll;
  begin
    FirstVisibleRow := NewRow;
    FirstVisibleCol := NewCol;
  end;

begin
  inherited KeyDown(Key, Shift);

// Если ничего в гриде не отображается, то реакции на
// кнопки нет => выход
  if (Selection_LastPosibleCol < 0) or
     (Selection_LastPosibleRow < 0) then
    Exit;

  NeedInvalidate := True;
  NewCol := FirstVisibleCol;
  NewRow := FirstVisibleRow;
  if ssCtrl in Shift then
    case Key of
      VK_HOME:
        begin
          Selection_BeginUpdate;
          StartSelectionAt(0, 0, ssShift in Shift);
          NewRow := 0;
          NewCol := 0;
        end;
      VK_END:
        begin
          Selection_BeginUpdate;
          StartSelectionAt(Selection_LastPosibleCol, Selection_LastPosibleRow, ssShift in Shift);
          with Grid do
          begin
            NewRow := GetScrollParam(Self, VertScroll, spMax);
            NewCol := GetScrollParam(Self, HorzScroll, spMax);
          end;
        end;
      VK_RIGHT:
        begin
          Selection_BeginUpdate;
          with Grid do
            if CellFocusedMove(GetScrollParam(Self, HorzScroll, spLargeChange), 0, ssShift in Shift) then
              NewCol := Min(NewCol + GetScrollParam(Self, HorzScroll, spLargeChange), GetScrollParam(Self, HorzScroll, spMax));
        end;
      VK_LEFT:
        begin
          Selection_BeginUpdate;
          with Grid do
            if CellFocusedMove(-GetScrollParam(Self, HorzScroll, spLargeChange), 0, ssShift in Shift) then
              NewCol := Max(NewCol - GetScrollParam(Self, HorzScroll, spLargeChange), 0);
        end;
      Byte('A'):
        begin
          SelectAll;
          NeedInvalidate := False;
        end;
      VK_INSERT,
      Byte('C'):
        Grid.CopyToClipboard;
      else
        NeedInvalidate := False;
    end
  else
  begin
    case Key of
      VK_HOME:
        begin
          Selection_BeginUpdate;
          StartSelectionAt(0, FSelection.Start.y, ssShift in Shift);
          NewCol := 0;
        end;
      VK_END:
        begin
          Selection_BeginUpdate;
          StartSelectionAt(Selection_LastPosibleCol, FSelection.Start.y, ssShift in Shift);
          NewCol := Grid.GetScrollParam(Self, HorzScroll, spMax);
        end;
      VK_UP:
        begin
          Selection_BeginUpdate;
          if CellFocusedMove(0, -1, ssShift in Shift) then
            Dec(NewRow);
        end;
      VK_DOWN:
        begin
          Selection_BeginUpdate;
          if CellFocusedMove(0, +1, ssShift in Shift) then
            Inc(NewRow);
        end;
      VK_LEFT:
        begin
          Selection_BeginUpdate;
          if CellFocusedMove(-1, 0, ssShift in Shift) then
            Dec(NewCol);
        end;
      VK_RIGHT:
        begin
          Selection_BeginUpdate;
          if CellFocusedMove(+1, 0, ssShift in Shift) then
            Inc(NewCol);
        end;
      VK_NEXT:
        begin
          Selection_BeginUpdate;
          with Grid do
            if CellFocusedMove(0, GetScrollParam(Self, VertScroll, spLargeChange), ssShift in Shift) then
              NewRow := Min(GetScrollParam(Self, VertScroll, spMax), NewRow + GetScrollParam(Self, VertScroll, spLargeChange));
        end;
      VK_PRIOR:
        begin
          Selection_BeginUpdate;
          with Grid do
            if CellFocusedMove(0, -GetScrollParam(Self, VertScroll, spLargeChange), ssShift in Shift) then
              NewRow := Max(0, NewRow  - GetScrollParam(Self, VertScroll, spLargeChange));
        end;
      else
        NeedInvalidate := False;
    end;
  end;
  if HorzScroll.Enabled then
    NewCol := Max(0, Min(NewCol, Grid.GetScrollParam(Self, HorzScroll, spMax)))
  else
    NewCol := 0;
  if VertScroll.Enabled then
    NewRow := Max(0, Min(NewRow, Grid.GetScrollParam(Self, VertScroll, spMax)))
  else
    NewRow := 0;
  NeedInvalidate := NeedInvalidate and ((NewCol <> FirstVisibleCol) or (NewRow <> FirstVisibleRow));
  if NeedInvalidate then
  begin
    SetScroll;
    ARect := Selection_GetUpdateRect(NewCol, NewRow);
    Invalidate(False, @ARect);
  end;
  if not (ssShift in Shift) then
    EnsureVisible(FSelection.Start.x, FSelection.Start.y);
  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FSelection.Fixed := True;
  inherited KeyUp(Key, Shift);
end;

procedure TfcxCustomDataZone.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if not HasCapture then
  begin
    PointInfo := GetPointInfo(Point(X, Y));
    if (Button = mbLeft) then
    begin
      SetCapture;
      case FPointInfo.PointType of
        ptNone:
          begin
            if ssShift in Shift then
              ContinueSelection(Point(X, Y))
            else
              StartSelection(Point(X, Y));
          end;
        ptHorzSplit:
          Owner.StartSizing(Self, stHorzSizing, Point(X, Y));
        ptVertSplit:
          Owner.StartSizing(Self, stVertSizing, Point(X, Y));
      end;
    end;
  end;
end;

procedure TfcxCustomDataZone.MouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
begin
  PointInfo := GetPointInfo(Point(X, Y));
  if HasCapture then
  begin
    if FPointInfo.PointType in [ptNone, ptHorzSplit, ptVertSplit] then
      SetSelection(Point(X, Y));
    if not FSelection.Fixed then
      ScrollIfAtBorder(Point(X, Y));
  end;
  inherited;
end;

procedure TfcxCustomDataZone.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  ReleaseCapture;
  PointInfo := GetPointInfo(Point(X, Y));
  inherited;
end;

function TfcxCustomDataZone.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  X, Y: Integer): Boolean;
begin
  Result := inherited MouseWheel(Shift, WheelDelta, X, Y);
  if not Result and VertScroll.Enabled then
  begin
    with Grid do
      SetScrollPos(Self, VertScroll, GetScrollParam(Self, VertScroll, spPosition) - Sign(WheelDelta) * Mouse.WheelScrollLines);
    Result := True;
  end;
end;

procedure TfcxCustomDataZone.MoveSizing(var AInfo: TSizingInfo);
const
  MinSize = 1;
begin
  if AInfo.ActivePart in [zpHeader, zpBody] then
    case AInfo.SizingType of
      stHorzSizing:
        begin
          if (AInfo.CurPoint.X - AInfo.StartPoint.X + ColWidth[PtrInt(AInfo.Data)]) < MinSize then
            AInfo.CurPoint.X := AInfo.StartPoint.X - ColWidth[PtrInt(AInfo.Data)] + MinSize;
          if AInfo.CurPoint.X > ClientWidth + FrameRect.Left then
            AInfo.CurPoint.X := ClientWidth + FrameRect.Left;
        end;
       stVertSizing:
        begin
          if (AInfo.CurPoint.Y - AInfo.StartPoint.Y + RowHeight[PtrInt(AInfo.Data)]) < MinSize then
            AInfo.CurPoint.Y := AInfo.StartPoint.Y - RowHeight[PtrInt(AInfo.Data)] + MinSize;
          if AInfo.CurPoint.Y > ClientHeight + FrameRect.Top then
            AInfo.CurPoint.Y := ClientHeight + FrameRect.Top;
        end;
    end
  else
    inherited;
end;

function TfcxCustomDataZone.PointToDataPoint(P: TPoint): TfcxDataPoint;
var
  Cur, i: Integer;
begin
  with FrameRect do
  begin
    Dec(P.X, Left);
    Dec(P.Y, Top);
  end;
  if LastVisibleRow >= 0 then
  begin
    Result.Y := FirstVisibleRow;
    Cur := GetFirstCellOffset.Y;
    for i := FirstVisibleRow to LastVisibleRow do
    begin
      Result.Y := i;
      inc(Cur, RowHeight[i]);
      if Cur > P.Y then
        Break;
    end;
  end
  else
    Result.Y := -1;
  if LastVisibleCol >= 0 then
  begin
    Result.X := FirstVisibleCol;
    Cur := GetFirstCellOffset.X;
    for i := FirstVisibleCol to LastVisibleCol do
    begin
      Result.X := i;
      inc(Cur, ColWidth[i]);
      if Cur > P.X then
        Break;
    end;
  end
  else
    Result.X := -1;
end;

procedure TfcxCustomDataZone.RestoreSelection;
begin
  FSelection := FSaveSelection;
end;

procedure TfcxCustomDataZone.ScrollIfAtBorder(P: TPoint);

  procedure MoveScroll(AScrollBar: TControl; AForward: Boolean);
  begin
    if not Assigned(AScrollBar) or not AScrollBar.Enabled then
      Exit;
    with Grid do
      if AForward then
        SetScrollPos(Self, AScrollBar,
          GetScrollParam(Self, AScrollBar, spPosition) + GetScrollParam(Self, AScrollBar, spSmallChange))
      else
        SetScrollPos(Self, AScrollBar,
          GetScrollParam(Self, AScrollBar, spPosition) - GetScrollParam(Self, AScrollBar, spSmallChange))
  end;

const
  delta: integer = 5;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  if P.x <= Rect.Left + delta then // move scroll left
    MoveScroll(HorzScroll, False);
  if P.x >= Rect.Right - delta then // move scroll right
    MoveScroll(HorzScroll, True);

  if P.y <= Rect.Top + delta then // move scroll top
    MoveScroll(VertScroll, False);
  if P.y >= Rect.Bottom + delta then // move scroll bottom
    MoveScroll(VertScroll, True);
end;

procedure TfcxCustomDataZone.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Sender = FHorzScroll then
  begin
    if FFirstVisibleCol <> ScrollPos then
    begin
      FFirstVisibleCol := ScrollPos;
      Invalidate(False);
    end;
    Grid.NotifyScroll(Self, sbHorizontal, ScrollCode, ScrollPos);
  end else
  begin
    if FFirstVisibleRow <> ScrollPos then
    begin
      FFirstVisibleRow := ScrollPos;
      Invalidate(False);
    end;
    Grid.NotifyScroll(Self, sbVertical, ScrollCode, ScrollPos);
  end;
end;

procedure TfcxCustomDataZone.SelectAll;
begin
  Selection_BeginUpdate;
  FSelection.Fixed := True;

  FSelection.Start := fcxDataPoint(0, 0);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := fcxDataPoint(Selection_LastPosibleCol, Selection_LastPosibleRow);

  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.SelectCell(const ACol, ARow: Integer; AMakeVisible: Boolean);
begin
  Selection_BeginUpdate;
  FSelection.Fixed := True;
  FSelection.Start.x := min(ACol, Selection_LastPosibleCol);
  FSelection.Start.y := min(ARow, Selection_LastPosibleRow);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := FSelection.Start;
  FSelection.Last := FSelection.Start;
  Selection_EndUpdate;
  if AMakeVisible then
    EnsureVisible(FSelection.Start.x, FSelection.Start.y);
end;

procedure TfcxCustomDataZone.SelectCol(const ACol: Integer);
begin
  Selection_BeginUpdate;
  FSelection.Fixed := True;

  FSelection.Start := fcxDataPoint(ACol, 0);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := fcxDataPoint(ACol, Selection_LastPosibleRow);

  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.SelectCols(const AStartCol,
  AStopCol: Integer);
begin
  Selection_BeginUpdate;
  FSelection.Fixed := True;

  FSelection.Start := fcxDataPoint(AStartCol, 0);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := fcxDataPoint(AStopCol, Selection_LastPosibleRow);

  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.Selection_BeginUpdate;
var
  Changed: Boolean;
begin
  Changed := False;
  if FSelection.Start.x > Selection_LastPosibleCol then
  begin
    FSelection.Start.x := Selection_LastPosibleCol;
    Changed := True;
  end;
  if FSelection.Start.y > Selection_LastPosibleRow then
  begin
    FSelection.Start.y := Selection_LastPosibleRow;
    Changed := True;
  end;
  if FSelection.Last.x > Selection_LastPosibleCol then
  begin
    FSelection.Last.x := Selection_LastPosibleCol;
    Changed := True;
  end;
  if FSelection.Last.y > Selection_LastPosibleRow then
  begin
    FSelection.Last.y := Selection_LastPosibleRow;
    Changed := True;
  end;
  if Changed then
  begin
    FSelection.Fixed := True;
    FSelection.Rect.LeftTop := FSelection.Start;
    FSelection.Rect.BottomRight := FSelection.Start;
    FSelection.Last := FSelection.Start;
  end;
end;

procedure TfcxCustomDataZone.Selection_EndUpdate;
var
  ARect, OldRect: TRect;
begin
  OldRect := GetRectForSelection(FSaveSelection);
  ARect := Selection_GetRect;
  if not EqualRect(OldRect, ARect) or
     not fcxEqualDataPoint(FSaveSelection.Start, FSelection.Start) then
  begin
//    OutputDebugString(PAnsiChar(Format('%d.%d - %d.%d', [FSelectionRect.Left, FSelectionRect.Top, FSelectionRect.Right, FSelectionRect.Bottom])));
    Invalidate(False, @OldRect);
    if not EqualRect(OldRect, ARect) then
      Invalidate(False, @ARect);
//    OutputDebugString(PAnsiChar(Format('%d.%d - %d.%d', [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom])));
    Grid.NotifySelectionChanged(Self, FSaveSelection);
    FSaveSelection := FSelection;
  end;
end;

function TfcxCustomDataZone.Selection_GetRect: TRect;
begin
  Result := GetRectForSelection(FSelection);
end;

function TfcxCustomDataZone.Selection_GetUpdateRect(NewFirstCol,
  NewFirstRow: Integer): TRect;
begin
  Result := ClientRect;
end;

function TfcxCustomDataZone.Selection_LastPosibleCol: Integer;
begin
  Result := GetColCount - 1;
end;

function TfcxCustomDataZone.Selection_LastPosibleRow: Integer;
begin
  Result := GetRowCount - 1;
end;

function TfcxCustomDataZone.Selection_LastVisibleCol(FullyVisible: Boolean = False): Integer;
var
  ATempWidth, AColCount: Integer;
  MaxIndex, MaxWidth: Integer;
begin
  AColCount := GetColCount;
  Result := FirstVisibleCol - Byte(AColCount = 0);
  if Result >= 0 then
  begin
    ATempWidth := ColWidth[Result];
    MaxIndex := AColCount - 1;
    MaxWidth := ClientWidth - GetFirstCellOffset.X;
    while (ATempWidth < MaxWidth) and (Result < MaxIndex) do
    begin
      inc(Result);
      inc(ATempWidth, ColWidth[Result]);
    end;
    if FullyVisible and (ATempWidth > MaxWidth) then
      Dec(Result);
  end;
end;

function TfcxCustomDataZone.Selection_LastVisibleRow(FullyVisible: Boolean = False): Integer;
var
  ATempHeight, ARowCount: Integer;
  MaxIndex, MaxHeight: Integer;
begin
  ARowCount := GetRowCount;
  Result := FirstVisibleRow - Byte(ARowCount = 0);
  if Result >= 0 then
  begin
    ATempHeight := RowHeight[Result];
    MaxIndex := ARowCount - 1;
    MaxHeight := ClientHeight - GetFirstCellOffset.Y;
    while (ATempHeight < MaxHeight) and (Result < MaxIndex) do
    begin
      inc(Result);
      inc(ATempHeight, RowHeight[Result]);
    end;
    if FullyVisible and (ATempHeight > MaxHeight) then
      Dec(Result);
  end;
end;

procedure TfcxCustomDataZone.SelectRow(const ARow: Integer);
begin
  Selection_BeginUpdate;
  FSelection.Fixed := True;

  FSelection.Start := fcxDataPoint(0, ARow);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := fcxDataPoint(Selection_LastPosibleCol, ARow);

  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.SelectRows(const AStartRow,
  AStopRow: Integer);
begin
  Selection_BeginUpdate;
  FSelection.Fixed := True;

  FSelection.Start := fcxDataPoint(0, AStartRow);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := fcxDataPoint(Selection_LastPosibleCol, AStopRow);

  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.SetColWidth(AIndex: Integer; AWidth: Integer);
begin
  Invalidate;
end;

procedure TfcxCustomDataZone.SetFirstVisibleCol(const Value: Integer);
begin
  if FFirstVisibleCol <> Value then
  begin
    Grid.SetScrollPos(Self, HorzScroll, Value);
    // don't update value here because it happens on scroll change handler
    // FFirstVisibleCol := Value;
  end;
end;

procedure TfcxCustomDataZone.SetFirstVisibleRow(const Value: Integer);
begin
  if FFirstVisibleRow <> Value then
  begin
    Grid.SetScrollPos(Self, VertScroll, Value);
    // don't update value here because it happens on scroll change handler
    // FFirstVisibleRow := Value;
  end;
end;

procedure TfcxCustomDataZone.SetRowHeight(AIndex: Integer; AHeight: Integer);
begin
  Invalidate;
end;

procedure TfcxCustomDataZone.SetSelection(P: TPoint);
var
  DataPoint: TfcxDataPoint;
begin
  if not FSelection.Fixed then
  begin
    // add a point to the currect selection
    DataPoint := PointToDataPoint(P);
    Selection_BeginUpdate;
    SetSelection(DataPoint);
    Selection_EndUpdate;
  end;
end;

procedure TfcxCustomDataZone.SetSelection(P: TfcxDataPoint);
begin
  FSelection.Rect.Top := min(FSelection.Start.y , P.y);
  FSelection.Rect.Bottom := max(FSelection.Start.y , P.y);
  FSelection.Rect.Left := min(FSelection.Start.x , P.x);
  FSelection.Rect.Right := max(FSelection.Start.x , P.x);

  FSelection.Last := P;
end;

procedure TfcxCustomDataZone.StartSelection(P: TPoint);
begin
  Selection_BeginUpdate;
  FSelection.Fixed := False;

  FSelection.Start := PointToDataPoint(P);
  FSelection.Rect.LeftTop := FSelection.Start;
  FSelection.Rect.BottomRight := FSelection.Start;
  FSelection.Last := FSelection.Start;
  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.StartSelectionAt(ACol, ARow: Integer; AddCell: Boolean);
var
  DataPoint: TfcxDataPoint;
begin
  Selection_BeginUpdate;

  FSelection.Fixed := not AddCell;

  if FSelection.Fixed then
  begin
    FSelection.Start.x := ACol;
    FSelection.Start.y := ARow;
    FSelection.Rect.LeftTop := FSelection.Start;
    FSelection.Rect.BottomRight := FSelection.Start;
    FSelection.Last := FSelection.Start;
  end else
  begin
    DataPoint.x := ACol;
    DataPoint.y := ARow;
    SetSelection(DataPoint);
  end;

  Selection_EndUpdate;
end;

procedure TfcxCustomDataZone.StartSizing(var AInfo: TSizingInfo);
begin
  if AInfo.ActivePart in [zpHeader, zpBody] then
    AInfo.Data := Pointer(FPointInfo.Index)
  else
    inherited;
end;

procedure TfcxCustomDataZone.StopSizing(var AInfo: TSizingInfo);
var
  I, NewSize: Integer;
begin
  if AInfo.ActivePart in [zpHeader, zpBody] then
    case AInfo.SizingType of
      stHorzSizing:
        begin
          NewSize := AInfo.CurPoint.X - AInfo.StartPoint.X + ColWidth[PtrInt(AInfo.Data)];
          if AInfo.ResizeSibling then
            for I := 0 to GetColCount - 1 do
              ColWidth[I] := NewSize
          else
            ColWidth[PtrInt(AInfo.Data)] := NewSize;
        end;
      stVertSizing:
        begin
          NewSize := AInfo.CurPoint.Y - AInfo.StartPoint.Y + RowHeight[PtrInt(AInfo.Data)];
          if AInfo.ResizeSibling then
            for I := 0 to GetRowCount - 1 do
              RowHeight[I] := NewSize
          else
            RowHeight[PtrInt(AInfo.Data)] := NewSize;
        end;
    end
  else
    inherited;
end;

procedure TfcxCustomDataZone.UpdatePointInfo(OldInfo, NewInfo: TfcxCustomDataZonePointInfo);
begin
  FPointInfo := NewInfo;
  UpdateCursor;
end;

procedure TfcxCustomDataZone.UpdateScrolls(UpdateHorizontal, UpdateVertical: Boolean);
var
  MaxPos, CurPos, LastVisibleItem: Integer;
begin
  // perform calculations for scrollbars
  if UpdateVertical then
  begin
    MaxPos := GetVertScrollMax;
    VertScroll.Enabled := MaxPos >= 0;

    if VertScroll.Enabled then
    begin
      if FFirstVisibleRow > MaxPos + 1 then
      begin
        FFirstVisibleRow := MaxPos + 1;
        CurPos := MaxPos;
      end
      else
        CurPos := Grid.GetScrollParam(Self, VertScroll, spPosition);
      LastVisibleItem := LastVisibleRow;
      if CurPos > LastVisibleItem then
        CurPos := LastVisibleItem;
      Grid.SetScrollParams(Self, VertScroll,
        CurPos,                          // position
        0,                               // min
        Max(MaxPos + 1, CurPos),         // max
        1,                               // small change
        LastVisibleItem - FirstVisibleRow // large change
      );
    end
    else
    begin
      Grid.SetScrollPos(Self, VertScroll, 0);
      VertScroll.Enabled := False; // disable it again since it can be enabled in VCL code
    end;
  end;

  if UpdateHorizontal then
  begin
    MaxPos := GetHorzScrollMax;
    HorzScroll.Enabled := MaxPos >= 0;

    if HorzScroll.Enabled then
    begin
      if FFirstVisibleCol > MaxPos + 1 then
      begin
        FFirstVisibleCol := MaxPos + 1;
        CurPos := MaxPos;
      end
      else
        CurPos := Grid.GetScrollParam(Self, HorzScroll, spPosition);
      LastVisibleItem := LastVisibleCol;
      if CurPos > LastVisibleItem then
        CurPos := LastVisibleItem;
      Grid.SetScrollParams(Self, HorzScroll,
        CurPos,                          // position
        0,                               // min
        max(MaxPos + 1, CurPos),         // max
        1,                               // small change
        LastVisibleItem - FirstVisibleCol // large change
      );
    end
    else
    begin
      Grid.SetScrollPos(Self, HorzScroll, 0);
      HorzScroll.Enabled := False; // disable it again since it can be enabled in VCL code
    end;
  end;
end;

procedure TfcxCustomDataZone.EnsureVisible(const ACol, ARow: Integer);
begin
  if ACol < FirstVisibleCol then
    FirstVisibleCol := ACol
  else
  if ACol > LastVisibleCol then
    FirstVisibleCol := FirstVisibleCol + (ACol - LastVisibleCol);
  if ARow < FirstVisibleRow then
    FirstVisibleRow := ARow
  else
  if ARow > LastVisibleRow then
    FirstVisibleRow := FirstVisibleRow + (ARow - LastVisibleRow);
end;

function TfcxCustomDataZone.GetFirstCellOffset: TPoint;
begin
  Result := Point(0, 0);
end;

function TfcxCustomDataZone.GetRectForSelection(const ASelection: TfcxGridSelection): TRect;
var
  i: integer;
begin
  if (ASelection.Rect.Left = -1) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  Result := ClientRect;
  with GetFirstCellOffset do
  begin
    inc(Result.Left, X);
    inc(Result.Top, Y);
  end;

  Result.Right := Result.Left;
  Result.Bottom := Result.Top;

  // get vertical bounds
  for i := FirstVisibleRow to ASelection.Rect.Bottom do
    Inc(Result.Bottom, RowHeight[i]);
  for i := FirstVisibleRow to ASelection.Rect.Top - 1 do
    Inc(Result.Top, RowHeight[i]);

  // get horizontal bounds
  for i := FirstVisibleCol to ASelection.Rect.Right do
    Inc(Result.Right, ColWidth[i]);
  for i := FirstVisibleCol to ASelection.Rect.Left - 1 do
    Inc(Result.Left, ColWidth[i]);
end;

procedure TfcxCustomDataZone.SetPointInfo(const Value: TfcxCustomDataZonePointInfo);
begin
  if (FPointInfo.PointType <> Value.PointType) or
     (FPointInfo.Index <> Value.Index) then
  begin
    UpdatePointInfo(FPointInfo, Value);
  end;
end;

procedure TfcxCustomDataZone.MouseLeave;
begin
  inherited;
  PointInfo := GetPointInfo(Point(-1, -1));
end;

function TfcxCustomDataZone.GetAsPlainText: AnsiString;
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
        S := S + GetText(J, GetRowIndex(I)) + #9;
      Delete(S, Length(S), 1);
      L.Add(S);
    end;
    Result := L.Text;
  finally
    L.Free;
  end;
end;

function TfcxCustomDataZone.GetText(ACol, ARow: Integer): TfcxString;
begin
  Result := '';
end;

procedure TfcxCustomDataZone.SetBoundingRect(const Value: TRect);
var
  OldClientRect: TRect;
begin
  OldClientRect := ClientRect;
  inherited;
  if not EqualRect(ClientRect, OldClientRect) then
  begin
    with Grid do
    begin
      UpdateScrollBar(Self, FVertScroll);
      UpdateScrollBar(Self, FHorzScroll);
    end;
    if CanUpdateScrolls then
      UpdateScrolls(True, True);
  end;
end;

function TfcxCustomDataZone.CanUpdateScrolls: Boolean;
begin
  Result := True;
end;

{ TfcxCustomGridStyles }

function TfcxCustomGridStyles.GetFirstStyleIndex: Integer;
begin
  Result := gsFirstCustomGridStyle;
end;

function TfcxCustomGridStyles.GetLastStyleIndex: Integer;
begin
  Result := gsLastCustomGridStyle;
end;

function TfcxCustomGridStyles.GetStyle(Index: Integer): TfcxCustomThemeStyle;
begin
  Result := FStyles[Index]
end;

function TfcxCustomGridStyles.GetStyleName(Index: Integer): String;
begin
  if (Index >= gsFirstCustomGridStyle) and (Index <= gsLastCustomGridStyle) then
    Result := fcxResources.Get(fcxCustomGridStyleNames[Index])
  else
    Result := inherited GetStyleName(Index);
end;

procedure TfcxCustomGridStyles.InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle);
begin
  inherited;
  FStyles[Index] := Value;
end;

procedure TfcxCustomGridStyles.SetDefaultValues;
begin
  CaptionArea.Update(clInfoBkStd, clWhite, clBlack, tgdHorizontal);
  HeaderArea.Update(clBtnFace, clNone, clBlack);
  HeaderCells.Update(clBtnFace, clNone, clBlack);
  HeaderCellsSelected.Update(clBtnShadow, clNone, clBtnText);
  DataArea.Update(clWhite, clNone, clGray);
  DataCells.Update(clWhite, clNone, clBlack);
  DataCellsSelected.Update(clHighlight, clNone, clHighlightText);
  StatusArea.Update(clBtnFace, clNone, clBlack);
end;

procedure TfcxCustomGridStyles.SetStyle(Index: Integer;
  const Value: TfcxCustomThemeStyle);
begin
  FStyles[Index].Assign(Value)
end;

end.
