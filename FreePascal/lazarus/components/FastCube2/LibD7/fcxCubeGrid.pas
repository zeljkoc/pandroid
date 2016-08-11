{*******************************************************}
{                                                       }
{             FastCube 2 source grid unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxCubeGrid;

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
  SysUtils, Classes, Graphics, Controls, StdCtrls, Menus,
  fcxTypes, fcxRes, fcxZone, fcxStyles, fcxPainters,
  fcxGridPainters, fcxCustomGrid,
  fcxCube, fcxAlerts, fcxPopupProvider, fcxPopupWindow, fcxCustomExport;

type
  TfcxCubeCaptionZone = class(TfcxGridZone)
  public
    procedure ClientPaint; override;
  end;

  TfcxCubeGrid = class;

  TfcxCubeDataZone = class(TfcxCustomDataZone)
  private
    FOrderedRecordSetProvider: TfcxOrderedRecordSetProvider;
    FPopupWindow: TfcxPopupWindow;
    procedure DoPopupDestroy(Sender: TObject);
    procedure DoGetNodeKind(Sender: TObject; ANode: Pointer; var NodeKind: TfcxNodeKind);
    procedure DoGetCheckState(Sender: TObject; ANode: Pointer; AData: Pointer);
    procedure DoSetCheckState(Sender: TObject; ANode: Pointer; AData: Pointer);
    procedure DoAfterCloseUp(Sender: TObject; Cancel: Boolean);
    function GetGrid: TfcxCubeGrid;
    procedure SetColumns(const Value: TfcxCubeDataColumns);
    function GetColumns: TfcxCubeDataColumns;
  protected
    FRowHeights: array of Integer;
    function ValidData: Boolean; virtual;
    function GetText(ACol, ARowIndex: Integer): String; override;
    function GetRowIndex(ARow: Integer): Integer; override;

    function GetHeaderSize: TSize; override;
    function GetIndicatorSize: TSize;
    function GetFirstCellOffset: TPoint; override;
    procedure DrawHeader(ARect: TRect); override;
    function GetHeaderCellRect(const Index: Integer): TRect;

    function GetColCount: Integer; override;
    function GetRowCount: Integer; override;
    function GetColWidth(AIndex: Integer): Integer; override;
    function GetRowHeight(AIndex: Integer): Integer; override;
    procedure SetColWidth(AIndex: Integer; AWidth: Integer); override;
    procedure SetRowHeight(AIndex: Integer; AHeight: Integer); override;

    procedure FullUpdate; virtual;
    procedure SelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    function GetPointInfo(APoint: TPoint): TfcxCustomDataZonePointInfo; override;
    procedure UpdatePointInfo(OldInfo, NewInfo: TfcxCustomDataZonePointInfo); override;

    function CreatePopupMenu: TPopupMenu; override;
    procedure PreparePopupMenuFor(APoint: TfcxDataPoint);
    procedure ContextPopup(X: Integer; Y: Integer; var Handled: Boolean); override;
    procedure PopupItemClick(Sender: TObject);

    // invalidates
    procedure InvalidateHeaderIndicator;
    procedure InvalidateHeaderCell(const Index: Integer);
    procedure InvalidateRowIndicator(const Index: Integer);
    function CreateOrderedRecordSetProvider: TfcxOrderedRecordSetProvider; virtual;
    property Grid: TfcxCubeGrid read GetGrid;
    procedure ColumnSort(const Index: Integer; const Add: Boolean);
  public
    constructor Create(AOwner: TZoneContainer); override;
    destructor Destroy; override;
    procedure ClientPaint; override;
    procedure DropDown;
    property Columns: TfcxCubeDataColumns read GetColumns write SetColumns;
    property OrderedRecordSetProvider: TfcxOrderedRecordSetProvider read FOrderedRecordSetProvider;
  end;

  TfcxCubeDataZoneClass = class of TfcxCubeDataZone;

  TfcxCubeStatusZoneClientPart = (
    szpNone,
    szpPositionInfo
  );

  TfcxCubeStatusZone = class(TfcxGridZone)
  private
    FSectionRects: array of TRect;
    FCurrentSection: Integer;
    FActiveClientPart: TfcxCubeStatusZoneClientPart;
    procedure SetCurrentSection(const Value: Integer);
    procedure SetActiveClientPart(const Value: TfcxCubeStatusZoneClientPart);
    procedure DrawPosition(R: TRect);
  protected
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseLeave; override;

    function GetSectionCount: Integer;
    function GetSectionWidth(AIndex: Integer): Integer;
    procedure DrawSection(AIndex: Integer);
    procedure InvalidateSection(AIndex: Integer);
    procedure UpdateActiveClientPart(CursorPos: TPoint);
    function GetSectionAt(P: TPoint): Integer;
    function ClientHitTest(P: TPoint): TfcxCubeStatusZoneClientPart;

    procedure ClientPaint; override;
    procedure SelectionChanged(Sender: TZone);
    property CurrentSection: Integer read FCurrentSection write SetCurrentSection;
    property ActiveClientPart: TfcxCubeStatusZoneClientPart read FActiveClientPart write SetActiveClientPart;
  public
    constructor Create(AOwner: TZoneContainer); override;
  end;

  { TfcxCubeGrid }

  TfcxCubeGrid = class(TfcxCustomGrid)
  private
    FCaptionZone: TZone;
    FDataZone: TfcxCubeDataZone;
    FStatusZone: TfcxCubeStatusZone;
    FInUpdate: Boolean;
    FCube: TfcxCube;
    procedure SetCube(const Value: TfcxCube);
    procedure SetCaptionZone(const Value: TZone);
    procedure SetDataZone(const Value: TfcxCubeDataZone);
    procedure SetStatusZone(const Value: TfcxCubeStatusZone);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyUpdate(Sender: TZone); override;
    procedure CubeChanged(Sender: TfcxCube; AChangeAlert: TfcxChangeAlert);
    procedure FullUpdate;
    function GetDataZoneClass: TfcxCubeDataZoneClass; virtual;
    procedure DoGetRowCount(Sender: TObject; var Value: Integer);
    procedure DoGetRowIndex(Sender: TObject; ARow: Integer; var Value: Integer); virtual;
    procedure DoGetColumns(Sender: TObject; Value: TfcxCubeDataColumns);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetClipboardText: TfcxString; override;
    function ExecuteAction(Action : TBasicAction) : Boolean; override;
    procedure NotifySelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection); override;
    function DoExport(AFilter: TfcxCustomExportFilter): Boolean;
  published
    property Align;
    property Cube: TfcxCube read FCube write SetCube;
    property PaintStyle;
    property Styles;
    property OnGetClipboardText;

    property CaptionZone: TZone read FCaptionZone write SetCaptionZone;
    property DataZone: TfcxCubeDataZone read FDataZone write SetDataZone;
    property StatusZone: TfcxCubeStatusZone read FStatusZone write SetStatusZone;
  end;

// point types
const
  ptHeaderIndicator = 100;
  ptHeaderCell = 101;
  ptRowIndicator = 102;
  
implementation

uses
  Math,
  ClipBrd,
  fcxGraphicRes,
  fcxFieldsPopup;

{ TfcxCubeGrid }

constructor TfcxCubeGrid.Create(AOwner: TComponent);
begin
  inherited;
  FInUpdate := True;

  FCaptionZone := Zones.AddZone(TfcxCubeCaptionZone);
  with FCaptionZone do
  begin
    Options := [];
    Caption := 'The cube caption!';
  end;

  FDataZone := TfcxCubeDataZone(Zones.AddZone(GetDataZoneClass));
  with FDataZone do
  begin
    Options := [zoShowHeader];
    Caption := 'Data';
  end;

  FStatusZone := TfcxCubeStatusZone(Zones.AddZone(TfcxCubeStatusZone));
  with FStatusZone do
  begin
    Options := [];
    Caption := 'The cube caption!';
  end;

  KeyBoardZone := FDataZone;

  FInUpdate := False;
end;

procedure TfcxCubeGrid.CubeChanged(Sender: TfcxCube; AChangeAlert: TfcxChangeAlert);
begin
  FullUpdate;
end;

destructor TfcxCubeGrid.Destroy;
begin
  Cube := nil;
  inherited;
end;

function TfcxCubeGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if Action.Owner is TfcxCube then
      CubeChanged(TfcxCube(Action.Owner), TfcxAction(Action).ChangeAlert);
    Result := True;
  end
  else
    Result := False;
end;

procedure TfcxCubeGrid.FullUpdate;
begin
  FDataZone.FullUpdate;
  Invalidate;
  DoChange(TfcxChangeAlert.Create(at_Cube));
end;

function TfcxCubeGrid.GetDataZoneClass: TfcxCubeDataZoneClass;
begin
  Result := TfcxCubeDataZone;
end;

procedure TfcxCubeGrid.NotifySelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection);
begin
  inherited;
  FDataZone.SelectionChanged(Sender, OldSelection);
  FStatusZone.SelectionChanged(Sender);
end;

procedure TfcxCubeGrid.NotifyUpdate(Sender: TZone);

  procedure UpdateDataZone;
  var
    ARect: TRect;
    Top: Integer;
  begin
    if FCaptionZone.Visible then
      Top := FCaptionZone.BoundingRect.Bottom
    else
      Top := 0;
    ARect := Rect(0, Top, Width, Height);
    ARect.Bottom := FStatusZone.Top;
    if Assigned(FDataZone.VertScroll) then
      ARect.Right := ARect.Right - FDataZone.VertScroll.Width;
    if Assigned(FDataZone.HorzScroll) then
      ARect.Bottom := ARect.Bottom - FDataZone.HorzScroll.Height;
    FDataZone.BoundingRect := ARect;
  end;

begin
  inherited;
  if FInUpdate then
    Exit;

  FInUpdate := True;

  if Sender = FCaptionZone then
  begin
    Sender.BoundingRect := Rect(0, 0, Width, 20);
  end
  else
  if Sender = FStatusZone then
  begin
    Sender.BoundingRect := Rect(0, Height - 21, Width, Height);
    UpdateDataZone;
  end
  else
  if Sender = FDataZone then
  begin
    UpdateDataZone;
  end;
  FInUpdate := False;
end;

procedure TfcxCubeGrid.SetCube(const Value: TfcxCube);
begin
  if FCube <> Value then
  begin
    if Assigned(FCube) then
    begin
      FCube.ListnersManager.RemoveListner(Self);
      FCube.RemoveFreeNotification(Self);
    end;
    FCube := Value;
    FullUpdate;
    if Assigned(FCube) then
    begin
      FCube.FreeNotification(Self);
      FCube.ListnersManager.AddListner(Self);
    end;
  end;
end;

procedure TfcxCubeGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent = Cube then
      Cube := nil;
  end;
end;

function TfcxCubeGrid.DoExport(AFilter: TfcxCustomExportFilter): Boolean;
begin
  Result := False;

  if AFilter = nil then
    Exit;

  AFilter.Slice := nil;
  AFilter.Cube := Cube;
  AFilter.Styles := Styles;
  AFilter.OnGetCubeRowCount := DoGetRowCount;
  AFilter.OnGetCubeRowIndex := DoGetRowIndex;
  AFilter.OnGetCubeColumns := DoGetColumns;
  Result := AFilter.PerformExport;
end;

procedure TfcxCubeGrid.DoGetRowCount(Sender: TObject; var Value: Integer);
begin
  Value := FDataZone.GetRowCount;
end;

procedure TfcxCubeGrid.DoGetRowIndex(Sender: TObject; ARow: Integer; var Value: Integer);
begin
  Value := FDataZone.GetRowIndex(ARow);
end;

procedure TfcxCubeGrid.DoGetColumns(Sender: TObject; Value: TfcxCubeDataColumns);
begin
  Value.Assign(FDataZone.Columns);
end;

procedure TfcxCubeGrid.SetCaptionZone(const Value: TZone);
begin
  FCaptionZone.Assign(Value);
end;

procedure TfcxCubeGrid.SetDataZone(const Value: TfcxCubeDataZone);
begin
  FDataZone.Assign(Value);
end;

procedure TfcxCubeGrid.SetStatusZone(const Value: TfcxCubeStatusZone);
begin
  FStatusZone.Assign(Value);
end;

function TfcxCubeGrid.GetClipboardText: TfcxString;
begin
  Result := DataZone.GetAsPlainText;
end;

{ TfcxCubeCaptionZone }

procedure TfcxCubeCaptionZone.ClientPaint;
var
  R: TRect;
begin
  R := ClientRect;
  Grid.Painter.DrawBody(Canvas, R, Grid.Styles.CaptionArea);
  Canvas.Font.Assign(Grid.Styles.CaptionArea.Font);
  if TfcxCubeGrid(Grid).Cube <> nil then
    Grid.Painter.DrawText(Canvas, R, StringToControl(TfcxCubeGrid(Grid).Cube.Caption), DT_CENTER or DT_SINGLELINE or DT_VCENTER)
  else
    Grid.Painter.DrawText(Canvas, R, '<cube is not assigned>', DT_CENTER or DT_SINGLELINE or DT_VCENTER)
end;

{ TfcxDataZone }

procedure TfcxCubeDataZone.ClientPaint;
var
  Cube: TfcxCube;
  I, J, RowIndex: Integer;
  CR, ARect: TRect;
  CellStates: TfcxThemeCellStates;
  Style: TfcxCustomThemeStyle;
  Rgn: HRGN;
  RgnType: Integer;

  function GetCellAlignment(Field: TfcxCommonField): TAlignment;
  begin
    case Field.DataType of
      fcdt_String, fcdt_WideString: Result := taLeftJustify;
    else
      Result := taRightJustify;
    end;
  end;

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

  Cube := TfcxCubeGrid(Grid).Cube;
  if Cube = nil then
  begin
    with Grid do
      Painter.DrawBody(Self.Canvas, CR, Styles.DataArea);
    Exit;
  end;

  Rgn := CreateRectRgnIndirect(CR);
  RgnType := SIMPLEREGION;
  ARect := CR;
  for J := FirstVisibleRow to LastVisibleRow do
  begin
    ARect.Left := CR.Left;
    ARect.Bottom := ARect.Top + RowHeight[J];
    ARect.Right := ARect.Left + GetIndicatorSize.cx;
    // indicator
    with Grid do
    begin
      if (J >= FSelection.Rect.Top) and (J <= FSelection.Rect.Bottom) then
        Painter.DrawHeaderCell(Self.Canvas, ARect, '', tsHot, Styles.HeaderCellsSelected)
      else
        Painter.DrawHeaderCell(Self.Canvas, ARect, '', tsNormal, Styles.HeaderCells);
      if FSelection.Start.y = J then
        Painter.DrawRowIndicator(Self.Canvas, ARect, Styles.HeaderCellsSelected);
    end;
    RgnType := ExcludeRect(ARect);

    if RgnType = NULLREGION then
      Break;

    ARect.Left := ARect.Right;
    RowIndex := GetRowIndex(J);

    for I := FirstVisibleCol to LastVisibleCol do
    begin
      ARect.Right := ARect.Left + ColWidth[i];
      CellStates := [];
      if CellFocused(I, J) then
        Include(CellStates, tcsFocused);
      if CellSelected(I, J) then
        Include(CellStates, tcsSelected);
      if [tcsSelected, tcsFocused] * CellStates = [tcsSelected] then
        Style := Grid.Styles.DataCellsSelected
      else
        Style := Grid.Styles.DataCells;
      Grid.Painter.DrawDataCell(Canvas, ARect, GetText(I, RowIndex), GetCellAlignment(Columns.VisItems[I].Field), CellStates, Style, 100);
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
    with Grid do
      Painter.DrawBody(Self.Canvas, CR, Styles.DataArea);
    SelectClipRgn(Canvas.Handle, 0);
  end;
  DeleteObject(Rgn);
end;

function TfcxCubeDataZone.GetColCount: Integer;
begin
  Result := Columns.VisibleCount;
end;

function TfcxCubeDataZone.GetColumns: TfcxCubeDataColumns;
begin
  Result := OrderedRecordSetProvider.Columns;
end;

function TfcxCubeDataZone.GetRowCount: Integer;
begin
  Result := OrderedRecordSetProvider.RecordCount;
end;

procedure TfcxCubeDataZone.DrawHeader(ARect: TRect);
const
  DirectionMap: array[TfcxSortDirection] of TfcxThemeSortType = (tstUp, tstDown);
var
  Cube: TfcxCube;
  I: integer;
  TmpRect: TRect;
  S: String;
  State: TfcxThemeState;
  Style: TfcxCustomThemeStyle;
  Rgn: HRGN;
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
  Cube := TfcxCubeGrid(Grid).Cube;
  if Cube = nil then
  begin
    inherited;
    Exit;
  end;

  Rgn := CreateRectRgnIndirect(ARect);
  TmpRect := ARect;
  TmpRect.Bottom := TmpRect.Top + Grid.DefaultRowHeight;

  // indicator

  TmpRect.Right := TmpRect.Left + GetIndicatorSize.cx;
  if (PointInfo.PointType = ptHeaderIndicator) then
  begin
    if GetKeyState(VK_LBUTTON) < -126 then
      State := tsPressed
    else
      State := tsHot;
    Style := Grid.Styles.HeaderCellsSelected;
  end
  else
  begin
    State := tsNormal;
    Style := Grid.Styles.HeaderCells;
  end;
  Grid.Painter.DrawHeaderCell(Canvas, TmpRect, '', State, Style, False, tstUnknown, fcxGraphicResources.ToolImages, 12);
  RgnType := ExcludeRect(TmpRect);
  TmpRect.Left := TmpRect.Right;

  for I := FirstVisibleCol to LastVisibleCol do
  begin
    TmpRect.Right := TmpRect.Left + ColWidth[i];
    S := Columns.VisItems[I].Field.CubeFieldDisplayLabel;
    IntersectRect(TmpRect, TmpRect, ARect);

    State := tsNormal;
    if (PointInfo.PointType = ptHeaderCell) and (PointInfo.Index = I) then
      if GetKeyState(VK_LBUTTON) < -126 then
        State := tsPressed;
    if (I >= FSelection.Rect.Left) and (I <= FSelection.Rect.Right) then
    begin
      if State = tsNormal then
        State := tsHot;
      Style := Grid.Styles.HeaderCellsSelected;
    end
    else
      Style := Grid.Styles.HeaderCells;

    Grid.Painter.DrawHeaderCell(Canvas, TmpRect, StringToControl(S), State,
      Style, Columns.VisItems[I].SortIndex <> -1, DirectionMap[Columns.VisItems[I].SortDirection]);

    RgnType := ExcludeRect(TmpRect);
    if RgnType = NULLREGION then
      Break;
    TmpRect.Left := TmpRect.Right;
  end;
  if RgnType <> NULLREGION then
  begin
    with BoundingRect do
    begin
      OffsetRgn(Rgn, Left, Top);
      SelectClipRgn(Canvas.Handle, Rgn);
    end;
    with Grid do
      Painter.DrawBody(Self.Canvas, ARect, Styles.HeaderArea);
    SelectClipRgn(Canvas.Handle, 0);
  end;
  DeleteObject(Rgn);
end;

function TfcxCubeDataZone.GetHeaderSize: TSize;
begin
  Result := inherited GetHeaderSize;
  if Assigned(Grid) then
    Result.cy := Grid.DefaultRowHeight
end;

function TfcxCubeDataZone.GetColWidth(AIndex: Integer): Integer;
begin
  if (AIndex >= Columns.VisibleCount) or (AIndex < 0) then
    Result := Grid.DefaultColWidth
  else
  begin
    Result := Columns.VisItems[AIndex].Width;
    if Result = 0 then
      Result := Grid.DefaultColWidth;
  end;
end;

function TfcxCubeDataZone.GetRowHeight(AIndex: Integer): Integer;
begin
  if (Length(FRowHeights) <= AIndex) or (AIndex < 0) then
    Result := Grid.DefaultRowHeight
  else
  begin
    Result := FRowHeights[AIndex];
    if Result = 0 then
      Result := Grid.DefaultRowHeight;
  end;
end;

function TfcxCubeDataZone.GetRowIndex(ARow: Integer): Integer;
begin
  Result := OrderedRecordSetProvider.GetRowIndex(ARow);
end;

procedure TfcxCubeDataZone.SetColWidth(AIndex, AWidth: Integer);
begin
  Columns.VisItems[AIndex].Width := AWidth;
  inherited;
end;

procedure TfcxCubeDataZone.SetRowHeight(AIndex, AHeight: Integer);
begin
  FRowHeights[AIndex] := AHeight;
  inherited;
end;

procedure TfcxCubeDataZone.FullUpdate;
begin
  if ValidData then
  begin
    TfcxCubeOrderedRecordSetProvider(OrderedRecordSetProvider).SetCube(Grid.Cube);
    SetLength(FRowHeights, GetRowCount);
  end
  else
  begin
    TfcxCubeOrderedRecordSetProvider(OrderedRecordSetProvider).SetCube(nil);
    SetLength(FRowHeights, 0);
  end;

  UpdateScrolls(True, True);
end;

constructor TfcxCubeDataZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  FOrderedRecordSetProvider :=  CreateOrderedRecordSetProvider;
end;

function TfcxCubeDataZone.CreateOrderedRecordSetProvider: TfcxOrderedRecordSetProvider;
begin
  Result := TfcxCubeOrderedRecordSetProvider.Create(Grid.Cube);
end;

destructor TfcxCubeDataZone.Destroy;
begin
  FOrderedRecordSetProvider.Free;
  inherited;
end;

procedure TfcxCubeDataZone.DoAfterCloseUp(Sender: TObject; Cancel: Boolean);
begin
  if not Cancel then
  begin
    TfcxCubeFieldsDataProvider(Sender).ForEach(DoSetCheckState);
    UpdateScrolls(True, True);
    Invalidate;
  end;
end;

procedure TfcxCubeDataZone.DoGetCheckState(Sender: TObject; ANode, AData: Pointer);
begin
  if Columns.FieldItems[TfcxCommonField(AData)].Visible then
    TfcxCubeFieldsDataProvider(Sender).NodeState[ANode] := csChecked
  else
    TfcxCubeFieldsDataProvider(Sender).NodeState[ANode] := csUnChecked;
end;

procedure TfcxCubeDataZone.DoGetNodeKind(Sender: TObject; ANode: Pointer; var NodeKind: TfcxNodeKind);
begin
  NodeKind := nkCheck;
end;

procedure TfcxCubeDataZone.DoPopupDestroy(Sender: TObject);
begin
  FPopupWindow := nil;
  InvalidateHeaderIndicator;
end;

procedure TfcxCubeDataZone.DoSetCheckState(Sender: TObject; ANode, AData: Pointer);
begin
  Columns.FieldItems[TfcxCommonField(AData)].Visible := TfcxCubeFieldsDataProvider(Sender).NodeState[ANode] = csChecked;
end;

procedure TfcxCubeDataZone.DropDown;
var
  R: TRect;
begin
  R := GetHeaderRect;
  R.TopLeft := ZoneToScreen(R.TopLeft);
  R.BottomRight := ZoneToScreen(R.BottomRight);

  FPopupWindow := TfcxCubeFieldsPopup.Create(Owner);
  TfcxCubeFieldsPopup(FPopupWindow).Cube := Grid.Cube;
  if Assigned(FPopupWindow) then
  begin
    FPopupWindow.OnDestroy := DoPopupDestroy;
    TfcxCubeFieldsPopup(FPopupWindow).DataProvider.OnGetNodeKind := DoGetNodeKind;
    TfcxCubeFieldsPopup(FPopupWindow).DataProvider.OnAfterCloseUp := DoAfterCloseUp;
    TfcxCubeFieldsPopup(FPopupWindow).DataProvider.ForEach(DoGetCheckState);
    FPopupWindow.Options := [pwoShowFooter, pwoAllowResize];
    FPopupWindow.PopupAt(Point(R.Left, R.Bottom));
  end;
end;

function TfcxCubeDataZone.GetFirstCellOffset: TPoint;
begin
  Result.X := GetIndicatorSize.cx;
  Result.Y := 0;
end;

function TfcxCubeDataZone.GetGrid: TfcxCubeGrid;
begin
  Result := TfcxCubeGrid(Owner);
end;

function TfcxCubeDataZone.GetHeaderCellRect(const Index: Integer): TRect;
var
  I: Integer;
begin
  Result := GetHeaderRect;
  Inc(Result.Left, GetIndicatorSize.cx);
  for I := 0 to Index - 1 do
    inc(Result.Left, ColWidth[I]);
  Result.Right := Result.Left + ColWidth[Index];
end;

function TfcxCubeDataZone.GetIndicatorSize: TSize;
begin
  Result.cx := fcxGraphicResources.ToolImages.Width + 2;
  Result.cy := fcxGraphicResources.ToolImages.Height + 2;
end;

function TfcxCubeDataZone.GetPointInfo(APoint: TPoint): TfcxCustomDataZonePointInfo;
var
  I, Cur: Integer;
  R: TRect;
  P: TPoint;
begin
  R := GetHeaderRect;
  if PtInRect(R, APoint) then
  begin
    // header click
    Cur := GetFirstCellOffset.X;
    if APoint.X < Cur then
    begin
      Result.PointType := ptHeaderIndicator;
      Exit;
    end;
    for I := FirstVisibleCol to LastVisibleCol do
    begin
      Inc(Cur, ColWidth[I]);
      if Abs(APoint.X - Cur) <= SplitThreshold then
      begin
        Result.PointType := ptHorzSplit;
        Result.Index := I;
        Exit;
      end;
      if APoint.X <= Cur then
      begin
        Result.PointType := ptHeaderCell;
        Result.Index := I;
        Exit;
      end;
    end;
  end
  else
  begin
    P := APoint;
    with FrameRect do
    begin
      Dec(P.X, Left);
      Dec(P.Y, Top);
    end;
    if P.X < GetFirstCellOffset.X then
    begin
      // row indicator click
      Cur := GetFirstCellOffset.Y;
      for I := FirstVisibleRow to LastVisibleRow do
      begin
        Inc(Cur, RowHeight[I]);
        if Abs(P.Y - Cur) <= SplitThreshold then
        begin
          Result.PointType := ptVertSplit;
          Result.Index := I;
          Exit;
        end;
        if P.Y <= Cur then
        begin
          Result.PointType := ptRowIndicator;
          Result.Index := I;
          Exit;
        end;
      end;
      Result.PointType := ptNone;
    end
    else
      Result := inherited GetPointInfo(APoint);
  end;
end;

procedure TfcxCubeDataZone.InvalidateHeaderCell(const Index: Integer);
var
  R: TRect;
begin
  R := GetHeaderCellRect(Index);
  Invalidate(False, @R, False);
end;

procedure TfcxCubeDataZone.InvalidateHeaderIndicator;
var
  R: TRect;
begin
  R := GetHeaderRect;
  R.Right := R.Left + GetIndicatorSize.cx;
  Invalidate(False, @R, False);
end;

procedure TfcxCubeDataZone.InvalidateRowIndicator(const Index: Integer);
var
  R: TRect;
  I: Integer;
begin
  R := ClientRect;
  R.Right := R.Left + GetIndicatorSize.cx;
  for I := 0 to Index - 1 do
    inc(R.Top, RowHeight[I]);
  R.Bottom := R.Top + RowHeight[Index];
end;

procedure TfcxCubeDataZone.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    case PointInfo.PointType of
      ptHeaderIndicator:
        begin
          if FPopupWindow = nil then
            DropDown;
          InvalidateHeaderIndicator;
        end;
      ptHeaderCell:
        begin
          InvalidateHeaderCell(PointInfo.Index);
        end;
      ptRowIndicator:
        begin
          InvalidateRowIndicator(PointInfo.Index);
          SelectRow(PointInfo.Index);
        end;
    end;
  end;
end;

procedure TfcxCubeDataZone.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  case PointInfo.PointType of
    ptHeaderIndicator: InvalidateHeaderIndicator;
    ptHeaderCell: ColumnSort(PointInfo.Index, ssShift in Shift);
    ptRowIndicator: InvalidateRowIndicator(PointInfo.Index);
  end;
end;

procedure TfcxCubeDataZone.SelectionChanged(Sender: TZone; const OldSelection: TfcxGridSelection);
var
  R1, R2, R, RH, CR: TRect;
begin
  CR := ClientRect;
  // invalidate indicator
  R1 := GetRectForSelection(OldSelection);
  R2 := GetRectForSelection(FSelection);
  if not EqualRect(R1, R2) then
  begin
    R := R1;
    RH := R;
    R.Left := CR.Left;
    R.Right := R.Left + GetIndicatorSize.cx;
    Invalidate(False, @R);

    R := RH;
    R.Top := CR.Top;
    R.Bottom := R.Top + GetHeaderSize.cy;
    Invalidate(False, @R, False);

    R := R2;
    RH := R;
    R.Left := CR.Left;
    R.Right := R.Left + GetIndicatorSize.cx;
    Invalidate(False, @R);

    R := RH;
    R.Top := CR.Top;
    R.Bottom := R.Top + GetHeaderSize.cy;
    Invalidate(False, @R, False);
  end;
end;

procedure TfcxCubeDataZone.UpdatePointInfo(OldInfo, NewInfo: TfcxCustomDataZonePointInfo);
begin
  inherited;
  case OldInfo.PointType of
    ptHeaderIndicator: InvalidateHeaderIndicator;
    ptHeaderCell: InvalidateHeaderCell(OldInfo.Index);
    ptRowIndicator: InvalidateRowIndicator(OldInfo.Index);
  end;

  case NewInfo.PointType of
    ptHeaderIndicator: InvalidateHeaderIndicator;
    ptHeaderCell: InvalidateHeaderCell(NewInfo.Index);
    ptRowIndicator: InvalidateRowIndicator(NewInfo.Index);
  end;
end;

function TfcxCubeDataZone.ValidData: Boolean;
begin
  Result := Assigned(Grid.Cube);
end;

function TfcxCubeDataZone.GetText(ACol, ARowIndex: Integer): TfcxString;
begin
  Result := OrderedRecordSetProvider.Text[ACol, ARowIndex];
end;

procedure TfcxCubeDataZone.ContextPopup(X, Y: Integer; var Handled: Boolean);
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

function TfcxCubeDataZone.CreatePopupMenu: TPopupMenu;
const
  Items: array[0..0] of String = (
    'sCopy'
  );
  Icons: array[0..0] of Integer = (
    24
  );
var
  I: Integer;
begin
  Result := inherited CreatePopupMenu;
  Result.Images := fcxGraphicResources.ToolImages;
  SetLength(FOwnMenuItems, Length(Items));
  for I := Low(Items) to High(Items) do
  begin
    FOwnMenuItems[I] := NewItem(fcxResources.Get(Items[I]), 0, False, True, PopupItemClick, 0, '');
    FOwnMenuItems[I].ImageIndex := Icons[I];
    Result.Items.Add(FOwnMenuItems[I]);
  end;
end;

procedure TfcxCubeDataZone.PreparePopupMenuFor(APoint: TfcxDataPoint);
begin
  // nothing here
end;

procedure TfcxCubeDataZone.PopupItemClick(Sender: TObject);
begin
  case GetOwnMenuItemIndex(Sender) of
    0: Grid.CopyToClipboard;
  end;
end;

{ TfcxCubeStatusZone }

function TfcxCubeStatusZone.ClientHitTest(P: TPoint): TfcxCubeStatusZoneClientPart;
var
  I: Integer;
begin
  for I := Low(FSectionRects) to High(FSectionRects) do
    if PtInRect(FSectionRects[I], P) then
    begin
      P.X := P.X - FSectionRects[I].Left;
      if I = 1 then
      begin
        if P.X < 100 then
          Result := szpPositionInfo
        else
          Result := szpNone;
        Exit;
      end;
    end;
  Result := szpNone;
end;

procedure TfcxCubeStatusZone.ClientPaint;
var
  CR, R: TRect;
  I, C, W, SW: Integer;
begin
  CR := ClientRect;
  with Grid do
  begin
    Painter.DrawStatus(Self.Canvas, CR, Styles.StatusArea);
    Self.Canvas.Font.Assign(Styles.StatusArea.Font);
    Self.Canvas.Font.Color := Styles.StatusArea.TextColor;
  end;
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

constructor TfcxCubeStatusZone.Create(AOwner: TZoneContainer);
begin
  inherited;
  HasFrame := False;
  FCurrentSection := -1;
end;

procedure TfcxCubeStatusZone.DrawPosition(R: TRect);
const
  FormatStr = '%d : %d';
begin
  Grid.Painter.DrawText(Canvas, R, Format(FormatStr, [TfcxCubeGrid(Grid).DataZone.FSelection.Start.y + 1, TfcxCubeGrid(Grid).DataZone.GetRowCount]), DT_CENTER or DT_SINGLELINE or DT_VCENTER);
end;

procedure TfcxCubeStatusZone.DrawSection(AIndex: Integer);
var
  R: TRect;
begin
  R := FSectionRects[AIndex];
  if AIndex < GetSectionCount - 1 then
    Grid.Painter.DrawStatusPane(Canvas, R);
  if AIndex = 1 then
    DrawPosition(R)
end;

function TfcxCubeStatusZone.GetSectionAt(P: TPoint): Integer;
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

function TfcxCubeStatusZone.GetSectionCount: Integer;
begin
  Result := 2;
end;

function TfcxCubeStatusZone.GetSectionWidth(AIndex: Integer): Integer;
begin
  if AIndex = 0 then
    Result := -1
  else
    Result := 100;
end;

procedure TfcxCubeStatusZone.InvalidateSection(AIndex: Integer);
var
  R: TRect;
begin
  if (AIndex > High(FSectionRects)) or (AIndex < Low(FSectionRects)) then
    Exit;
  R := FSectionRects[AIndex];
  Invalidate(False, @R);
end;

procedure TfcxCubeStatusZone.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
  InvalidateSection(FCurrentSection);
  if CurrentSection = 1 then
  begin
    // scale section
  end;
end;

procedure TfcxCubeStatusZone.MouseLeave;
begin
  inherited;
  UpdateActiveClientPart(Point(-1, -1));
end;

procedure TfcxCubeStatusZone.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
end;

procedure TfcxCubeStatusZone.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateActiveClientPart(Point(X, Y));
  InvalidateSection(FCurrentSection);
  if Button = mbLeft then
  begin
{    case ActiveClientPart of
    end;}
  end;
end;

procedure TfcxCubeStatusZone.SelectionChanged(Sender: TZone);
begin
  InvalidateSection(1);
end;

procedure TfcxCubeStatusZone.SetActiveClientPart(const Value: TfcxCubeStatusZoneClientPart);
begin
  if FActiveClientPart <> Value then
  begin
    FActiveClientPart := Value;
    InvalidateSection(CurrentSection);
  end;
end;

procedure TfcxCubeStatusZone.SetCurrentSection(const Value: Integer);
begin
  if FCurrentSection <> Value then
  begin
    InvalidateSection(FCurrentSection);
    FCurrentSection := Value;
    InvalidateSection(FCurrentSection);
  end;
end;

procedure TfcxCubeStatusZone.UpdateActiveClientPart(CursorPos: TPoint);
begin
  if ActivePart = zpBody then
  begin
    with FrameRect do
      CursorPos := Point(CursorPos.X - Left, CursorPos.Y - Top);
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

procedure TfcxCubeDataZone.SetColumns(const Value: TfcxCubeDataColumns);
begin
  Columns.Assign(Value);
  FullUpdate;
end;

procedure TfcxCubeDataZone.ColumnSort(const Index: Integer; const Add: Boolean);
var
  I, MaxIndex: Integer;
begin
  if not Add then
  begin
    for I := 0 to Columns.VisibleCount - 1 do
      if I <> Index then
        Columns.VisItems[I].SortIndex := -1;
    if Columns.VisItems[Index].SortIndex = -1 then
    begin
      Columns.VisItems[Index].SortIndex := 0;
      Columns.VisItems[Index].SortDirection := fcsd_Asc;
    end
    else
    if Columns.VisItems[Index].SortDirection = fcsd_Asc then
    begin
      Columns.VisItems[Index].SortIndex := 0;
      Columns.VisItems[Index].SortDirection := fcsd_Desc
    end
    else
      Columns.VisItems[Index].SortIndex := -1;
  end
  else
  begin
    if Columns.VisItems[Index].SortIndex = -1 then
    begin
      MaxIndex := -1;
      for I := 0 to Columns.VisibleCount - 1 do
        MaxIndex := Max(MaxIndex, Columns.VisItems[I].SortIndex);
      Columns.VisItems[Index].SortIndex := MaxIndex + 1;
      Columns.VisItems[Index].SortDirection := fcsd_Asc;
    end
    else
    if Columns.VisItems[Index].SortDirection = fcsd_Asc then
      Columns.VisItems[Index].SortDirection := fcsd_Desc
    else
    begin
      MaxIndex := Columns.VisItems[Index].SortIndex;
      Columns.VisItems[Index].SortIndex := -1;
      for I := 0 to Columns.VisibleCount - 1 do
        if Columns.VisItems[I].SortIndex > MaxIndex then
          dec(Columns.VisItems[I].SortIndex);
    end;
  end;
  FOrderedRecordSetProvider.Update;
  Invalidate;
end;

initialization
{$IFDEF DELPHI_16UP}
{
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TfcxCubeGrid, TControl);
}
{$ENDIF}
end.
