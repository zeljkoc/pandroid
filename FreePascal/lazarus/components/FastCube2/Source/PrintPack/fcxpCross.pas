{******************************************}
{                                          }
{             FastReport v4.0              }
{         FastCube 2 Cross object          }
{                                          }
{         Copyright (c) 2001-2014          }
{       by Oleg Pryalkov, Paul Ishenin,    }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpCross;

interface

{$I frx.inc}
{$I fcx.inc}

uses
  SysUtils, Types, Classes, Controls, Graphics, Forms,
  frxClass, fcxpComponents, fcxSlice, fcxCube, fcxSliceGrid,
  fcxTypes, fcxStyles, fcxHighlights, frxCross, fcxAlerts
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  _extended_cell = array[0..0] of Extended;
  PextendedCell = ^_extended_cell;

  TfcxpMemoView = class(TfrxMemoView)
  private
    FGradientDirection: TfcxThemeGradientDirection;
    FGradientColor: TColor;
    FHighlights: TList;
    FCell: TfcxMeasureCell;
  protected
    procedure DrawBackground; override;
    procedure AddHighlight(AHighlight: TfcxCustomHighlight);
    procedure SetHighlights(const Value: String);
    function GetHighlights: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Diff(AComponent: TfrxComponent): String; override;
  published
    property GradientDirection: TfcxThemeGradientDirection read FGradientDirection write FGradientDirection default tgdNone;
    property GradientColor: TColor read FGradientColor write FGradientColor default clNone;
    property Highlights: String read GetHighlights write SetHighlights;
  end;

  TfcxpCrossView = class(TfrxView)
  private
    FMaxHeight: Extended;
    FCube: TfcxpCube;
    FSlice: TfcxSlice;
    FExternalSlice: TfcxSlice;

    FLoadSchema: TMemoryStream;
    FLoadMemos: TMemoryStream;
    FLoadSizes: TMemoryStream;

    FDotMatrix: Boolean;
    FColumnBands: TfrxCutBands;
    FRowBands: TfrxCutBands;
    FDownThenAcross: Boolean;
    FRepeatRowHeaders: Boolean;
    FRepeatColumnHeaders: Boolean;
    FShowColumnHeader: Boolean;
    FShowRowHeader: Boolean;
    FShowNames: Boolean;
    FBorder: Boolean;
    LargeBand: TfrxNullBand;

    FAllMemos: TList;
    FCellMemos: TList;
    FCellHeaderMemos: TList;
    FColumnMemos: TList;
    FColumnTotalMemos: TList;
    FCornerMemos: TList;
    FRowMemos: TList;
    FRowTotalMemos: TList;
    FGridX: TfrxGridLines;
    FGridY: TfrxGridLines;
    FGridUsed: TfrxGridLines;
    FFirstMousePos: TPoint;
    FLastMousePos: TPoint;
    FMovingObjects: Integer;
    FMouseDown: Boolean;

    FOnBeforePrintRowHeader: TfrxOnPrintHeaderEvent;
    FGapY: Integer;
    FGapX: Integer;     { Delphi event }
    FMeasurePlace: Integer;
    FMeasureIndex: Integer;
    FGridStyles: TfcxSliceGridStyles;
    FUseGridColors: Boolean;
    FUseGridAlign: Boolean;
    FNextCrossGap: Extended;
    FNextCross: TfcxpCrossView;
    FKeepTogether: Boolean;

    FHeadColSize: array of TfrxPoint;
    FHeadRowSize: array of TfrxPoint;
    FColSize: array of TfrxPoint;
    FRowSize: array of TfrxPoint;

    FCrossTraversRowBottom: PextendedCell;
    FCrossTreeRowBottom: array[Boolean] of Extended;

    FCrossHeadColHeight: PextendedCell;
    FCrossHeadRowWidth: PextendedCell;
    FCrossColWidth: PextendedCell;
    FCrossRowHeight: PextendedCell;
    FCalcMemo: TfrxCustomMemoView;
    FCalcStyle: TfcxCustomThemeStyle;
    FMinYNamesHeight: Extended;

    FOnPrintCell: TfrxPrintCellEvent;                    { script event }
    FOnPrintColumnHeader: TfrxPrintHeaderEvent;          { script event }
    FOnPrintRowHeader: TfrxPrintHeaderEvent;             { script event }
    FOnBeforePrintCell: TfrxOnPrintCellEvent;            { Delphi event }
    FOnBeforePrintColumnHeader: TfrxOnPrintHeaderEvent;  { Delphi event }
    FPaintSizes: TfcxpCrossPaintSizes;
    FSliceGridProvider: TfcxpSliceGridProvider;
    FSliceGridProviderName: String;
    procedure SetCube(const Value: TfcxpCube);
    procedure RenderMatrix;
    procedure BuildColumnBands;
    procedure BuildRowBands;
    procedure SetupOriginalComponent(Obj1, Obj2: TfrxComponent);
    procedure CorrectDMPBounds(Memo: TfrxCustomMemoView);
    function CreateMemo(Parent: TfrxComponent): TfrxCustomMemoView;
    procedure SetSlice(const Value: TfcxSlice);
// for TraverseAxis
    function XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function XAxisDrawProcCalc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function YAxisDrawProcCalc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function GetXAxisColWidth(ACol: Integer): Integer;
    function GetYAxisColWidth(ACol: Integer): Integer;
    function GetXAxisRowHeight(ARow: Integer): Integer;
    function GetYAxisRowHeight(ARow: Integer): Integer;

    procedure ClearMemos;
    function GetCellHeaderMemos(Index: Integer): TfrxCustomMemoView;
    function GetCellMemos(Index: Integer): TfrxCustomMemoView;
    function GetColumnMemos(Index: Integer): TfrxCustomMemoView;
    function GetColumnTotalMemos(Index: Integer): TfrxCustomMemoView;
    function GetCornerMemos(Index: Integer): TfrxCustomMemoView;
    function GetRowMemos(Index: Integer): TfrxCustomMemoView;
    function GetRowTotalMemos(Index: Integer): TfrxCustomMemoView;
    function GetCellLevels: Integer;
    function GetColumnLevels: Integer;
    function GetRowLevels: Integer;
    procedure CreateCellHeaderMemos(NewCount: Integer);
    procedure CreateCellMemos(NewCount: Integer);
    procedure CreateColumnMemos(NewCount: Integer);
    procedure CreateCornerMemos(NewCount: Integer);
    procedure CreateRowMemos(NewCount: Integer);
    procedure InitMemos(AddToScript: Boolean);
    function GetNestedObjects: TList;
    procedure SetDotMatrix(const Value: Boolean);
    procedure SliceChanged;
    procedure CreateMemos;
    function IsCrossValid: Boolean;
    function GetDefaultColWidthInternal: integer;
    function GetDefaultRowHeightInternal: integer;
    function GetColWidth(ACol: Integer): integer;
    function GetRowHeight(ARow: Integer): integer;
    procedure SetColWidth(ACol: Integer; const Value: integer);
    procedure SetRowHeight(ARow: Integer; const Value: integer);
    procedure SetShowColumnHeader(const Value: Boolean);
    procedure SetShowNames(const Value: Boolean);
    procedure SetShowRowHeader(const Value: Boolean);
    procedure DoCalcSizes;
    function GetSliceGridProviderName: String;
    procedure SetSliceGridProvider(const Value: TfcxpSliceGridProvider);
    procedure SetSliceGridProviderName(const Value: String);
    procedure SetNextCross(const Value: TfcxpCrossView);
    procedure ApplyCellStyle(ACell: TfcxMeasureCell; Style: TfcxCustomThemeStyle);
    procedure ApplyMemoStyle(AMemo: TfrxCustomMemoView; Style: TfcxCustomThemeStyle);
    function GetYAxisLevelSize: Integer;
    function GetYAxisMeasureSize: Integer;
  protected
    procedure SetMemosBounds;
    procedure SetMemosPos;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    // property writers and readers
    procedure ReadSchema(Stream: TStream);
    procedure WriteSchema(Stream: TStream);
    procedure ReadMemos(Stream: TStream);
    procedure WriteMemos(Stream: TStream);
    procedure ReadSizes(Stream: TStream);
    procedure WriteSizes(Stream: TStream);
    // slice helpers
    function ColCount: Integer;
    function RowCount: Integer;
    function GetContainerObjects: TList; override;
    function AutoSizeMemo: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    procedure AddSourceObjects; override;
    procedure Loaded; override;
    // style work
    procedure ApplyStyle(Style: TfrxStyles);
    procedure GetStyle(Style: TfrxStyles);

    function DrawCross(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended): TfrxPoint;
    procedure Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended); override;
    procedure GetData; override;
// Only for RenderMatrix
    function RowHeaderWidth: Extended;
// Only for RenderMatrix
    function ColumnHeaderHeight: Extended;
    function ColumnHeaderHeightWOLast: Extended;
// Only for RenderMatrix
    function NamesHeight: Extended;
    function ExecuteAction(Action : TBasicAction) : Boolean; override;
    function ContainerAdd(Obj: TfrxComponent): Boolean; override;
    function ContainerMouseDown(Sender: TObject; X, Y: Integer): Boolean; override;
    procedure ContainerMouseMove(Sender: TObject; X, Y: Integer); override;
    procedure ContainerMouseUp(Sender: TObject; X, Y: Integer); override;
    procedure BeforeStartReport; override;
    procedure ImportColorFromGrid(ASliceGrid: TfcxSliceGrid);
    procedure ClearColorFromGrid;

    property Slice: TfcxSlice read FSlice write SetSlice;
    property DotMatrix: Boolean read FDotMatrix;
    property OnBeforePrintCell: TfrxOnPrintCellEvent read FOnBeforePrintCell write FOnBeforePrintCell;
    property OnBeforePrintColumnHeader: TfrxOnPrintHeaderEvent read FOnBeforePrintColumnHeader write FOnBeforePrintColumnHeader;
    property OnBeforePrintRowHeader: TfrxOnPrintHeaderEvent read FOnBeforePrintRowHeader write FOnBeforePrintRowHeader;
    property CellMemos[Index: Integer]: TfrxCustomMemoView read GetCellMemos;
    property CellHeaderMemos[Index: Integer]: TfrxCustomMemoView read GetCellHeaderMemos;
    property ColumnMemos[Index: Integer]: TfrxCustomMemoView read GetColumnMemos;
    property ColumnTotalMemos[Index: Integer]: TfrxCustomMemoView read GetColumnTotalMemos;
    property CornerMemos[Index: Integer]: TfrxCustomMemoView read GetCornerMemos;
    property RowMemos[Index: Integer]: TfrxCustomMemoView read GetRowMemos;
    property RowTotalMemos[Index: Integer]: TfrxCustomMemoView read GetRowTotalMemos;
    property RowLevels: Integer read GetRowLevels;
    property ColumnLevels: Integer read GetColumnLevels;
    property CellLevels: Integer read GetCellLevels;
    property DefaultColWidthInternal: integer read GetDefaultColWidthInternal;
    property DefaultRowHeightInternal: integer read GetDefaultRowHeightInternal;
    property ColWidth[ACol: Integer]: integer read GetColWidth write SetColWidth;
    property RowHeight[ARow: Integer]: integer read GetRowHeight write SetRowHeight;
  published
    procedure Update;
    property GapX: Integer read FGapX write FGapX default 3;
    property GapY: Integer read FGapY write FGapY default 3;
    property Border: Boolean read FBorder write FBorder default True;
    property Cube: TfcxpCube read FCube write SetCube;
    property SliceGridProvider: TfcxpSliceGridProvider read FSliceGridProvider write SetSliceGridProvider;
    property SliceGridProviderName: String read GetSliceGridProviderName write SetSliceGridProviderName;
    property DownThenAcross: Boolean read FDownThenAcross write FDownThenAcross default False;
    property RepeatRowHeaders: Boolean read FRepeatRowHeaders write FRepeatRowHeaders default True;
    property RepeatColumnHeaders: Boolean read FRepeatColumnHeaders write FRepeatColumnHeaders default True;
    property ShowColumnHeader: Boolean read FShowColumnHeader write SetShowColumnHeader default True;
    property ShowRowHeader: Boolean read FShowRowHeader write SetShowRowHeader default True;
    property ShowNames: Boolean read FShowNames write SetShowNames default True;
    property OnPrintCell: TfrxPrintCellEvent read FOnPrintCell write FOnPrintCell;
    property OnPrintColumnHeader: TfrxPrintHeaderEvent read FOnPrintColumnHeader write FOnPrintColumnHeader;
    property OnPrintRowHeader: TfrxPrintHeaderEvent read FOnPrintRowHeader write FOnPrintRowHeader;
    property NextCross: TfcxpCrossView read FNextCross write SetNextCross;
    property NextCrossGap: Extended read FNextCrossGap write FNextCrossGap;
    property KeepTogether: Boolean read FKeepTogether write FKeepTogether default False;
    property PaintSizes: TfcxpCrossPaintSizes read FPaintSizes;
  end;

implementation
uses
{$IFNDEF NO_EDITORS}
  fcxpCrossEditor,
{$ENDIF}
  Math,
  frxDsgnIntf,
  frxRes,
  fcxRes,
  fcxGraphicRes,
  frxDMPClass,
  frxXML,
  frxXMLSerializer,
  frxVariables,
  frxUtils,
  fcxGraphicUtils;

type
  THackComponent = class(TfrxComponent);
  THackMemoView = class(TfrxCustomMemoView);
  THackSlice = class(TfcxSlice);

const                  //IsColTotal, IsRowTotal, IsColGrand, IsRowGrand
  CellStylesNames: array[boolean, boolean, boolean, boolean] of String =
  ((
     (('cell', 'cellrowgrand'),('cellcolgrand', 'cellfullgrand')),
     (('cellrowtotal', 'error'),('cellrowtotalcolgrand', 'error'))
   ),
   (
     (('cellcoltotal', 'cellrowgrandcoltotal'),('error', 'error')),
     (('cellfulltotal', 'error'),('error', 'error'))
   ));
  cAlignment2HAlign: array [TAlignment] of TfrxHAlign = (haLeft, haRight, haCenter);

  YAxisLevelSpacing = 20;
  XAxisLevelSpacing = 0;

function CalcSize(m: TfrxCustomMemoView): TfrxPoint;
var
  e, SaveHeight: Extended;
begin
  SaveHeight := m.Height;
  m.Height := 10000;

  Result.X := m.CalcWidth;
  Result.Y := m.CalcHeight;

  if m is TfrxDMPMemoView then
  begin
    Result.X := Result.X + fr1CharX;
    Result.Y := Result.Y + fr1CharY;
  end;

  if (m.Rotation = 90) or (m.Rotation = 270) then
  begin
    e := Result.X;
    Result.X := Result.Y;
    Result.Y := e;
  end;

  m.Height := SaveHeight;
end;

function CalcHeight(m: TfrxCustomMemoView; Wrap: Boolean): TfrxPoint;
var
  SaveHeight: Extended;
begin
  SaveHeight := m.Height;
  m.Height   := 10000;
  m.AutoWidth := False;
  m.WordWrap := Wrap;
  m.StretchMode := smActualHeight;
  Result.Y := m.CalcHeight;
  if m is TfrxDMPMemoView then
  begin
    Result.Y := Result.Y + fr1CharY;
  end;
  m.Height := SaveHeight;
end;

function CalcWidthWithHeigth(m: TfrxCustomMemoView; AHeight: Extended): TfrxPoint;
var
  SaveHeight: Extended;
begin
  SaveHeight := m.Height;
  m.Height := AHeight;

  m.WordWrap    := False;
  m.AutoWidth   := True;
  m.StretchMode := smDontStretch;

  Result.X := m.CalcWidth;
  Result.Y := AHeight;

  if m is TfrxDMPMemoView then
  begin
    Result.X := Result.X + fr1CharX;
  end;

  m.Height := SaveHeight;
end;

function CalcHeightWithWidth(m: TfrxCustomMemoView; AWidth: Extended): TfrxPoint;
var
  SaveHeight: Extended;
begin
  SaveHeight := m.Height;
  m.Height   := 10000;
  m.Width     := AWidth;
  m.WordWrap  := True;
  m.AutoWidth := False;
  m.StretchMode := smActualHeight;
  Result.X := AWidth;
  Result.Y := m.CalcHeight;
  if m is TfrxDMPMemoView then
  begin
    Result.Y := Result.Y + fr1CharY;
  end;
  m.Height := SaveHeight;
end;

{ TfcxpCrossView }

constructor TfcxpCrossView.Create(AOwner: TComponent);
begin
  inherited;
  FPaintSizes := TfcxpCrossPaintSizes.Create(Self);
  SetLength(FHeadColSize, 0);
  SetLength(FHeadRowSize, 0);
  SetLength(FColSize, 0);
  SetLength(FRowSize, 0);

  FCrossHeadColHeight := nil;
  FCrossHeadRowWidth := nil;
  FCrossTraversRowBottom := nil;
  FCrossColWidth := nil;
  FCrossRowHeight := nil;
  FMinYNamesHeight := DefaultRowHeightInternal;

  Color := clWhite;
  Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];

  // we don't want show cross in Preview form - there should be only memos
  frComponentStyle := frComponentStyle - [csPreviewVisible] + [csContainer];

  FAllMemos := TList.Create;
  FCellMemos := TList.Create;
  FCellHeaderMemos := TList.Create;
  FColumnMemos := TList.Create;
  FColumnTotalMemos := TList.Create;
  FCornerMemos := TList.Create;
  FRowMemos := TList.Create;
  FRowTotalMemos := TList.Create;

  FColumnBands := TfrxCutBands.Create;
  FRowBands    := TfrxCutBands.Create;
  FGridX := TfrxGridLines.Create;
  FGridY := TfrxGridLines.Create;

  FDownThenAcross := False;
  FShowColumnHeader := True;
  FShowRowHeader := True;
  FRepeatRowHeaders := True;
  FRepeatColumnHeaders := True;
  FShowNames := True;
  FBorder := True;
  FGapX := 3;
  FGapY := 3;
  SetDotMatrix(Page is TfrxDMPPage);
  FNextCrossGap := 0;

  FCalcMemo := CreateMemo(nil);
  FCalcMemo.Name := 'FCalcMemo';

  FCalcStyle := TfcxCustomThemeStyle.Create(nil);

  FUseGridColors := False;
  FUseGridAlign := False;
  // create internal slice and add us as listeners
  FSlice := TfcxSlice.Create(Self);
  FSlice.ListnersManager.AddListner(Self);
  CreateMemos;

  // this objects are for load sequence only
  FLoadSchema := nil;
  FLoadMemos := nil;
  FLoadSizes := nil;
end;

destructor TfcxpCrossView.Destroy;
begin
  FreeMem(FCrossHeadColHeight, SizeOf(_Extended_cell) * (ColumnLevels + 1));
  FreeMem(FCrossHeadRowWidth, SizeOf(_Extended_cell) * RowLevels);
  FreeMem(FCrossTraversRowBottom);
  FreeMem(FCrossColWidth, SizeOf(_Extended_cell) * ColCount);
  FreeMem(FCrossRowHeight, SizeOf(_Extended_cell) * RowCount);
  FCrossHeadColHeight := nil;
  FCrossHeadRowWidth := nil;
  FCrossTraversRowBottom := nil;
  FCrossColWidth := nil;
  FCrossRowHeight := nil;

  FCalcStyle.Free;
  FCalcMemo.Free;
  FLoadSchema.Free;
  FLoadMemos.Free;
  FLoadSizes.Free;

  FColumnBands.Free;
  FRowBands.Free;
  ClearMemos;
  FAllMemos.Free;
  FCellMemos.Free;
  FCellHeaderMemos.Free;
  FColumnMemos.Free;
  FColumnTotalMemos.Free;
  FCornerMemos.Free;
  FRowMemos.Free;
  FRowTotalMemos.Free;
  FGridX.Free;
  FGridY.Free;
  // Deleting internal slice
  // this should be done last
  FSlice.ListnersManager.RemoveListner(Self);
  FSlice.Free;
  PaintSizes.Free;
  inherited;
end;

procedure TfcxpCrossView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Cube) then
    Cube := nil;
  if (Operation = opRemove) and (AComponent = FNextCross) then
    FNextCross := nil;
end;

procedure TfcxpCrossView.SetCube(const Value: TfcxpCube);
begin
  FCube := Value;
  if FCube <> nil then
  begin
    FSlice.Cube := FCube.Cube;
    SliceGridProvider := nil;
    FCube.FreeNotification(Self);
  end
  else
    FSlice.Cube := nil;

  // we get cube, so we can apply schema now and forget about it holding in stream
  if (FLoadSchema <> nil) and (FCube <> nil) and (FCube.Active) then
  begin
    //FSlice.Recreate;
    FSlice.LoadFromStream(FLoadSchema);
    FreeAndNil(FLoadSchema);

    if FLoadMemos <> nil then
    begin
      ReadMemos(FLoadMemos);
      FreeAndNil(FLoadMemos);
    end;

    if FLoadSizes <> nil then
    begin
      ReadSizes(FLoadSizes);
      FreeAndNil(FLoadSizes);
    end;
  end;
end;

procedure TfcxpCrossView.SetSlice(const Value: TfcxSlice);
begin
  // we should set external slice here, because internal slice cannot be changed
  // interfnal slice used inside FR
  // external - in main application, but outside FR
  FExternalSlice := Value;
end;

procedure TfcxpCrossView.DefineProperties(Filer: TFiler);
begin
  inherited;
  // Schema - we should hold cross layout inside object
  // Memos - this is visual style of our cross-tab
  Filer.DefineBinaryProperty('Schema', ReadSchema, WriteSchema, True);
  Filer.DefineBinaryProperty('Memos', ReadMemos, WriteMemos, True);
  Filer.DefineBinaryProperty('MemoSizes', ReadSizes, WriteSizes, True);
end;

procedure TfcxpCrossView.ReadSchema(Stream: TStream);
begin
  if (Stream.Size > 0) then
  begin
    if FSlice.Cube <> nil then
    begin
      // if we already have cube, we can load slice
      FSlice.LoadFromStream(Stream);
    end else
    begin
      // else we should create temprorary stream and wait until we get cube
      // we just can't apply schema to nothing
      FLoadSchema := TMemoryStream.Create;
      FLoadSchema.LoadFromStream(Stream);
      FLoadSchema.Position := 0;
    end;
  end;
end;

procedure TfcxpCrossView.WriteSizes(Stream: TStream);
var
  x: TfrxXMLDocument;
  Item: TfrxXMLItem;
  i: Integer;

  procedure AddItem(p: TfrxPoint; Parent: TfrxXMLItem; const Name: String);
  var
    Item: TfrxXMLItem;
  begin
    Item := Parent.Add;
    Item.Name := Name;
    Item.Prop['X'] := FloatToStr(p.X);
    Item.Prop['Y'] := FloatToStr(p.Y);
  end;

begin
  x := TfrxXMLDocument.Create;
  x.Root.Name := 'memo_sizes';
  try
    Item := x.Root.Add;
    Item.Name := 'head_col_sizes';
    for i := 0 to High(FHeadColSize) do
      AddItem(FHeadColSize[i], Item, 'head_col_size');

    Item := x.Root.Add;
    Item.Name := 'head_row_sizes';
    for i := 0 to High(FHeadRowSize) do
      AddItem(FHeadRowSize[i], Item, 'head_row_size');

    Item := x.Root.Add;
    Item.Name := 'col_sizes';
    for i := 0 to High(FColSize) do
      AddItem(FColSize[i], Item, 'col_size');

    Item := x.Root.Add;
    Item.Name := 'row_sizes';
    for i := 0 to High(FRowSize) do
      AddItem(FRowSize[i], Item, 'row_size');

    x.SaveToStream(Stream);
  finally
    x.Free;
  end;
end;

procedure TfcxpCrossView.ReadSizes(Stream: TStream);
var
  x: TfrxXMLDocument;
  Item: TfrxXMLItem;
  i: Integer;

  procedure GetItem(var p: TfrxPoint; Parent: TfrxXMLItem; const Index: Integer);
  var
    Item: TfrxXMLItem;
  begin
    if Index >= Parent.Count then
      Exit;    
    Item := Parent.Items[Index];
    p.X := StrToFloat(Item.Prop['X']);
    p.Y := StrToFloat(Item.Prop['Y']);
  end;

begin
  // if we have no Cube, we just creating temporary stream and wait until we get it
  if (Stream.Size > 0) and (FSlice.Cube = nil) then
  begin
    FLoadSizes := TMemoryStream.Create;
    FLoadSizes.LoadFromStream(Stream);
    FLoadSizes.Position := 0;
    Exit;
  end;

  x := TfrxXMLDocument.Create;

  try
    x.LoadFromStream(Stream);
    Item := x.Root.FindItem('head_col_sizes');
    for i := 0 to High(FHeadColSize) do
      GetItem(FHeadColSize[i], Item, i);

    Item := x.Root.FindItem('head_row_sizes');
    for i := 0 to High(FHeadRowSize) do
      GetItem(FHeadRowSize[i], Item, i);

    Item := x.Root.FindItem('col_sizes');
    for i := 0 to High(FColSize) do
      GetItem(FColSize[i], Item, i);

    Item := x.Root.FindItem('row_sizes');
    for i := 0 to High(FRowSize) do
      GetItem(FRowSize[i], Item, i);
  finally
    x.Free;
  end;
end;

procedure TfcxpCrossView.WriteSchema(Stream: TStream);
begin
  if FSlice.Active then
    FSlice.SaveToStream(Stream);
end;


procedure TfcxpCrossView.Draw(Canvas: TCanvas; ScaleX, ScaleY, OffsetX, OffsetY: Extended);
var
  size, Offset: TfrxPoint;
begin
  size := DrawCross(nil, ScaleX, ScaleY, AbsLeft, AbsTop);
  if (size.X > 0) and (size.Y > 0) then
  begin
    Width := size.X + 40;
    Height := size.Y + 40;
  end;

  Color := clWhite;
  Frame.Style := fsDot;
  inherited;

  Offset := frxPoint(20, 20);
  if FDotMatrix then
    Offset := frxPoint(fr1CharX * 2, fr1CharY);
  DrawCross(Canvas, ScaleX, ScaleY, AbsLeft + Offset.X, AbsTop + Offset.Y);
  fcxGraphicResources.FRImages.Draw(Canvas, FX + 2, FY + 2, 2);
end;

function TfcxpCrossView.DrawCross(Canvas: TCanvas; ScaleX, ScaleY,
  OffsetX, OffsetY: Extended): TfrxPoint;

  procedure DrawLine(x, y, dx, dy: Extended);
  begin
    Canvas.MoveTo(Round(x * ScaleX), Round(y * ScaleY));
    Canvas.LineTo(Round((x + dx) * ScaleX), Round((y + dy) * ScaleY));
  end;

  procedure DrawScriptSign(c: TfrxReportComponent);
  begin
    if (Canvas <> nil) and (c.OnBeforePrint <> '') then
      with c, Canvas do
      begin
        Pen.Style := psSolid;
        Pen.Color := clRed;
        Pen.Width := 1;
        DrawLine(AbsLeft + 2, AbsTop + 1, 0, 7);
        DrawLine(AbsLeft + 3, AbsTop + 2, 0, 5);
        DrawLine(AbsLeft + 4, AbsTop + 3, 0, 3);
        DrawLine(AbsLeft + 5, AbsTop + 4, 0, 1);
      end;
  end;

  procedure DrawObj(Obj: TfrxReportComponent; Child: Boolean = False);
  var
    i: Integer;
  begin
    { don't let a child move outside parent }
    if Child then
    begin
      if Obj.Left < 0 then
        Obj.Left := 0;
      if Obj.Left + Obj.Width > Obj.Parent.Width then
        Obj.Left := Obj.Parent.Width - Obj.Width;
      if Obj.Top < 0 then
        Obj.Top := 0;
      if Obj.Top + Obj.Height > Obj.Parent.Height then
        Obj.Top := Obj.Parent.Height - Obj.Height;
    end;

    if Canvas <> nil then
      Obj.Draw(Canvas, ScaleX, ScaleY, 0, 0);
    DrawScriptSign(Obj);
    if not Child then
    begin
      FGridX.Add(Obj, Obj.AbsLeft + Obj.Width);
      FGridY.Add(Obj, Obj.AbsTop + Obj.Height);
    end;
    FAllMemos.Add(Obj);
    for i := 0 to Obj.Objects.Count - 1 do
      DrawObj(Obj.Objects[i], True);
  end;

  procedure DrawHeader(Header: String; p: TfrxPoint);
  var
    i: Integer;
    Items: TList;
    m: TfrxCustomMemoView;
    SaveWidth, SaveHeight: Extended; // for dot-matrix
    s: String;
  begin
    Items := TList.Create;
    if Header = 'Corner' then
    begin
      if ShowColumnHeader and ShowRowHeader then
        Items.Add(FCornerMemos[0]);

      if ShowColumnHeader and ShowNames then
        Items.Add(FCornerMemos[1]);
      if ShowRowHeader and ShowColumnHeader and ShowNames then
        Items.Add(FCornerMemos[2]);
    end
    else
    if Header = 'ColumnHeader' then
    begin
      for i := 0 to FColumnMemos.Count - 1 do
        Items.Add(FColumnMemos[i]);
      for i := 0 to FColumnTotalMemos.Count - 1 do
        Items.Add(FColumnTotalMemos[i]);
    end
    else
    if Header = 'RowHeader' then
    begin
      for i := 0 to FRowMemos.Count - 1 do
        Items.Add(FRowMemos[i]);
      for i := 0 to FRowTotalMemos.Count - 1 do
        Items.Add(FRowTotalMemos[i]);
    end;

    for i := 0 to Items.Count - 1 do
    begin
      m := Items[i];
      s := m.Text;
      m.SetBounds(m.Left + p.X, m.Top + p.Y, m.Width, m.Height);
      SaveWidth := m.Width;
      SaveHeight := m.Height;
      CorrectDMPBounds(m);
      if m.Left + m.Width > Result.X then
        Result.X := m.Left + m.Width;
      if m.Top + m.Height > Result.Y then
        Result.Y := m.Top + m.Height;

      if m.Visible then
        DrawObj(m);

      m.Text := s;
      if m is TfrxDMPMemoView then
        TfrxDMPMemoView(m).SetBoundsDirect(m.Left - fr1CharX / 2,
          m.Top - fr1CharY / 2, SaveWidth, SaveHeight);
    end;

    Items.Free;
  end;

  procedure DrawCell(p: TfrxPoint);
  var
    i, j: Integer;
    m: TfrxCustomMemoView;
    SaveWidth, SaveHeight: Extended; // for dot-matrix
    s: String;
    NCol, NRow : integer;
  begin
    if (ShowColumnHeader and (FMeasurePlace = 0)) or (ShowRowHeader and (FMeasurePlace = 1)) then
      for i := 0 to FCellHeaderMemos.Count - 1 do
      begin
        m := FCellHeaderMemos[i];
        s := m.Text;
        m.SetBounds(m.Left + p.X, m.Top + p.Y, m.Width, m.Height);
        SaveWidth := m.Width;
        SaveHeight := m.Height;
        CorrectDMPBounds(m);
        if m.Left + m.Width > Result.X then
          Result.X := m.Left + m.Width;
        if m.Top + m.Height > Result.Y then
          Result.Y := m.Top + m.Height;

        if m.Visible then
          DrawObj(m);

        m.Text := s;
        if m is TfrxDMPMemoView then
          TfrxDMPMemoView(m).SetBoundsDirect(m.Left - fr1CharX / 2,
            m.Top - fr1CharY / 2, SaveWidth, SaveHeight);
      end;
    if FMeasurePlace = 0 then
      NCol := ColumnLevels * CellLevels
    else
      NCol := ColumnLevels + 1;
    if FMeasurePlace = 1 then
      NRow := RowLevels * CellLevels
    else
      NRow := RowLevels + 1;
    for i := 0 to NRow - 1 do
    begin
      for j := 0 to NCol - 1 do
      begin
        m := CellMemos[i * NCol + j];
        m.Visible := True;
        m.Restrictions := [rfDontDelete, rfDontEdit];
        m.Text := m.FormatData(CellMemos[i * NCol + j].Value, CellMemos[i * NCol + j].DisplayFormat);
        m.Left := m.Left + p.X;
        m.Top := m.Top + p.Y;
        SaveWidth := m.Width;
        SaveHeight := m.Height;
        CorrectDMPBounds(m);
        if m.Left + m.Width > Result.X then
          Result.X := m.Left + m.Width;
        if m.Top + m.Height > Result.Y then
          Result.Y := m.Top + m.Height;

        DrawObj(m);
        if m is TfrxDMPMemoView then
          TfrxDMPMemoView(m).SetBoundsDirect(m.Left - fr1CharX / 2,
            m.Top - fr1CharY / 2, SaveWidth, SaveHeight);

      end;
    end;
  end;

begin
  Result := frxPoint(0, 0);
  FGridX.Clear;
  FGridY.Clear;
  FAllMemos.Clear;

  if IsCrossValid then
  begin
    InitMemos(False);

    DrawHeader('Corner', frxPoint(OffsetX, OffsetY));
    if ShowColumnHeader then
      DrawHeader('ColumnHeader', frxPoint(OffsetX, OffsetY));
    if ShowRowHeader then
      DrawHeader('RowHeader', frxPoint(OffsetX, OffsetY));

    DrawCell(frxPoint(OffsetX, OffsetY));
  end;

  Result.X := Result.X - OffsetX;
  Result.Y := Result.Y - OffsetY;
end;

procedure TfcxpCrossView.GetData;
var
  OldSlice: TfcxSlice;
  AStyles: TfrxStyles;
begin
  inherited;
  OldSlice := FSlice;

  // if we have external slice, then we've got it from ouside FR, so we should
  // use it inspite of internal
  FUseGridAlign := False;
  if FExternalSlice <> nil then
  begin
    FSlice := FExternalSlice;
    SliceChanged;
  end
  else
  if Assigned(FSliceGridProvider) and Assigned(FSliceGridProvider.SliceGrid) and Assigned(TfcxSliceGrid(FSliceGridProvider.SliceGrid).Slice) then
  begin
    FUseGridAlign := True;
    FSlice := TfcxSliceGrid(FSliceGridProvider.SliceGrid).Slice;
    AStyles := FSliceGridProvider.Styles;
    if AStyles = nil then
      ImportColorFromGrid(TfcxSliceGrid(FSliceGridProvider.SliceGrid));
    if not FSliceGridProvider.UseReportPaintSizes then
      PaintSizes.Assign(FSliceGridProvider.PaintSizes);
    SliceChanged;
    if AStyles <> nil then
      ApplyStyle(AStyles);
  end;
  try
    // we can render cross only if it have any usable layout
    if Assigned(FSlice.Cube) and FSlice.HaveLayout then
    begin
      Report.SetProgressMessage(frxResources.Get('crBuildMx'));
      RenderMatrix;
    end;
  finally
    FSlice := OldSlice;
  end;
end;

procedure TfcxpCrossView.ApplyCellStyle(ACell: TfcxMeasureCell; Style: TfcxCustomThemeStyle);
begin
{$IFDEF TRIAL}
  if ACell.IsTrial then
  begin
    if FUseGridColors then
      Style.Assign(FGridStyles.DataCells);
    Style.FillColor := clWindow;
    Style.GradientDirection := tgdNone;
    Style.TextColor := clRed;
    Exit;
  end;
{$ENDIF}
  if FUseGridColors then
  begin
    if ACell.IsTotal then
      Style.Assign(FGridStyles.DataCellsTotals)
    else
      Style.Assign(FGridStyles.DataCells);
  end;
end;

procedure TfcxpCrossView.RenderMatrix;
var
  i, j, Page, SavePage: Integer;
  CurY, SaveCurY, AddWidth, MaxX: Extended;
  Band: TfrxBand;

  VarRowIndex, VarColumnIndex: TfrxVariable;

  function GetCellBand(RowIndex, ColumnIndex: Integer): TfrxBand;
  var
    I, J, iFrom, iTo: Integer;
    AMeasureIndex, ATotX, ATotY : integer;
    CellSize, CellOffs : Extended;
    m, Memo: TfrxCustomMemoView;
    c, c1: TfrxReportComponent;

    MeasureCell: TfcxMeasureCell;
    Highlights: TfcxCustomHighlights;
    Highlight: TfcxCustomHighlight;
    ApplyTo: TfcxHighlightApplyToEnum;
  begin
    Result := TfrxNullBand.Create(Report);
    Result.Height := RowHeight[RowIndex];

    iFrom := FColumnBands[ColumnIndex].FromIndex;
    iTo := FColumnBands[ColumnIndex].ToIndex;
    CellOffs  := 0;

    for I := iFrom to iTo do
    begin
      ATotX := ColumnLevels - 1 - Slice.XAxisContainer.VisibleAxisNodes.LevelOf[i];
      ATotY := RowLevels - 1 - Slice.YAxisContainer.VisibleAxisNodes.LevelOf[RowIndex];

      if FMeasurePlace = 0 then
      begin
        if ATotX > (ColumnLevels - FMeasureIndex - 1) then
          dec(ATotX);

        AMeasureIndex := Slice.MeasuresContainer.VisibleIndex[Slice.XAxisContainer.VisibleAxisNodes.MeasureIndex[i]];
        m := CellMemos[ATotY * CellLevels * ColumnLevels + ATotX * CellLevels + AMeasureIndex];
      end
      else
      if FMeasurePlace = 1 then
      begin
        if ATotY > (RowLevels - FMeasureIndex - 1) then
          dec(ATotY);
        AMeasureIndex := Slice.MeasuresContainer.VisibleIndex[Slice.YAxisContainer.VisibleAxisNodes.MeasureIndex[RowIndex]];
        m := CellMemos[(ATotY * CellLevels + AMeasureIndex) * (ColumnLevels + 1) + ATotX];
      end
      else
        m := nil;
      Memo := CreateMemo(Result);
      if FMeasurePlace <> -1 then
      begin
        Memo.Assign(m);
        SetupOriginalComponent(Memo, m);
        Memo.AutoWidth := False;

        Slice.GetMeasureCell(i, RowIndex, MeasureCell);

        if Memo is TfcxpMemoView then
          TfcxpMemoView(Memo).FCell := MeasureCell;
          
        THackMemoView(Memo).Value := MeasureCell.Value;

        if VarIsEmpty(THackMemoView(Memo).Value) then
          THackMemoView(Memo).Value := 0;

        Memo.Text := MeasureCell.StrValue;

        ApplyCellStyle(MeasureCell, FCalcStyle);
        Highlights := Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].Highlights;
        if Highlights.Count > 0 then
        begin
          if MeasureCell.IsGrandTotal then
            ApplyTo := hatGrandTotal
          else
          if MeasureCell.IsTotal then
            ApplyTo := hatTotals
          else
            ApplyTo := hatCells;
          for J := 0 to Highlights.Count - 1 do
          begin
            Highlight := Highlights[J];
            if Highlight.AcceptValue(MeasureCell.Value) and (ApplyTo in Highlight.ApplyTo) then
            begin
              if Highlight.CustomDrawn then
              begin
                if Memo is TfcxpMemoView then
                begin
                  TfcxpMemoView(Memo).AddHighlight(Highlight);
                  if Highlight.HideValue then
                    Memo.Text := '';
                end;
              end
              else
                FCalcStyle.MergeWith(TfcxGraphicHighlight(Highlight).GetStyleFor(@MeasureCell));
            end;
          end;
        end;
        if FUseGridAlign then
          Memo.HAlign := cAlignment2HAlign[MeasureCell.Alignment]
        else
          Memo.HAlign := haRight;
        if FUseGridColors then
          ApplyMemoStyle(Memo, FCalcStyle)
        else
          Memo.Font.Assign(m.Font);
      end;

      Memo.Rotation := 0;

      CellSize := ColWidth[i];
      Memo.SetBounds(CellOffs + AddWidth,
        0,  //          RowIndex * DefaultRowHeightInternal - TopMargin,
        CellSize, RowHeight[RowIndex]{DefaultRowHeightInternal}); // need Height of Real Row
        CellOffs := CellOffs + CellSize;
      CorrectDMPBounds(Memo);
      if Memo.AbsLeft + Memo.Width > MaxX then
        MaxX := Memo.AbsLeft + Memo.Width;
      Memo.Visible := (Memo.Width <> 0) and (Memo.Height <> 0);
      if FMeasurePlace = -1 then
        Memo.Frame.Typ := []
      else
      if Border then
      begin
        if RowIndex = 0 then
          Memo.Frame.Typ := Memo.Frame.Typ + [ftTop];
        if (i = 0) then
          Memo.Frame.Typ := Memo.Frame.Typ + [ftLeft];
        if (i = ColCount - 1) then
          Memo.Frame.Typ := Memo.Frame.Typ + [ftRight];
        if RowIndex = RowCount - 1 then
          Memo.Frame.Typ := Memo.Frame.Typ + [ftBottom];
      end;
      VarRowIndex.Value := RowIndex;
      VarColumnIndex.Value := i;
      Report.LocalValue := THackMemoView(Memo).Value;
      Report.CurObject := Memo.Name;
      Report.DoBeforePrint(Memo);

      { process memo children if any }
      if FMeasurePlace <> -1 then
        for j := 0 to m.Objects.Count - 1 do
        begin
          c := m.Objects[j];
          c1 := TfrxReportComponent(c.NewInstance);
          c1.Create(Result);
          c1.Assign(c);
          c1.Left := c1.Left + Memo.Left;
          c1.Top := c1.Top + Memo.Top;
          Report.CurObject := c.Name;
          Report.DoBeforePrint(c1);
        end;
    end;
  end;

  procedure DrawNames(AXAxis: Boolean; Offset: TfrxPoint; ARight: Extended = 0);
  var
    m: TfrxCustomMemoView;
  begin
    if AXAxis then
    begin
      if not ((FMeasurePlace = -1) and (CornerMemos[1].Text = '')) then
      begin
        m := CornerMemos[1];
        m.BeforePrint;
        if ARight = 0 then
          m.SetBounds(Offset.X + RowHeaderWidth, Offset.Y, DefaultColWidthInternal, NamesHeight) // need Height of Real Row
        else
          m.SetBounds(Offset.X + RowHeaderWidth, Offset.Y, ARight - Offset.X, NamesHeight); // need Height of Real Row
        CorrectDMPBounds(m);
        Report.PreviewPages.AddObject(m);
        m.AfterPrint;
      end
    end
    else
    begin
      if not ((FMeasurePlace = -1) and (CornerMemos[2].Text = '')) then
      begin
        m := CornerMemos[2];
        m.BeforePrint;
        m.SetBounds(Offset.X, Offset.Y + NamesHeight, RowHeaderWidth, ColumnHeaderHeight - NamesHeight); // need Height of Real Row
        CorrectDMPBounds(m);
        Report.PreviewPages.AddObject(m);
        m.AfterPrint;
      end;
    end;
  end;

  procedure DrawCorner(Offset: TfrxPoint);
  var
    m: TfrxCustomMemoView;
  begin
    m := CornerMemos[0];
    m.BeforePrint;
    m.SetBounds(Offset.X, Offset.Y, RowHeaderWidth, NamesHeight); // need Height of Real Row
    CorrectDMPBounds(m);
    Report.PreviewPages.AddObject(m);
    m.AfterPrint;
  end;

  procedure DoPagination(i, j: Integer);
  var
    k, kFrom, kTo: Integer;
    ARight: Extended;
    AColumnHeaderHeight: Extended;
  begin
    AColumnHeaderHeight := ColumnHeaderHeight;
    if ShowColumnHeader and (FRepeatColumnHeaders or (i = 0)) then
    begin
      Band := FColumnBands[j].Band;
      Band.Top := CurY;
      Report.Engine.ShowBand(Band);
      if ShowNames then
      begin
        ARight := 0;
        for k := 0 to Band.Objects.Count - 1 do
        begin
          if ARight < (TfrxReportComponent(Band.Objects[k]).Left + TfrxReportComponent(Band.Objects[k]).Width) then
            ARight := (TfrxReportComponent(Band.Objects[k]).Left + TfrxReportComponent(Band.Objects[k]).Width);
        end;
        if j = 0 then
        begin
          DrawNames(True, frxPoint(Left, Band.Top), ARight);
        end
        else
        begin
          if FRepeatRowHeaders then
          begin
            DrawNames(True, frxPoint(0, Band.Top), ARight);
          end
          else
          begin
            DrawNames(True, frxPoint(0 - RowHeaderWidth, Band.Top), ARight - RowHeaderWidth);
          end;
        end;
      end
    end;

    if ShowRowHeader and (FRepeatRowHeaders or (j = 0)) then
    begin
      Band := FRowBands[i].Band;
      if j = 0 then
        Band.Left := Left
      else
        Band.Left := 0;
      Band.Top := Band.Top + CurY;
      Report.Engine.ShowBand(Band);
      Band.Top := Band.Top - CurY;

      if ShowNames and ShowColumnHeader and (FRepeatColumnHeaders or (i = 0)) then
        DrawNames(False, frxPoint(Band.Left, Band.Top + CurY - AColumnHeaderHeight));
      if ShowColumnHeader and (FRepeatColumnHeaders or (i = 0)) then
        DrawCorner(frxPoint(Band.Left, Band.Top + CurY - AColumnHeaderHeight))
    end;


    if FRepeatColumnHeaders or (i = 0) then
      Report.Engine.CurY := CurY + AColumnHeaderHeight else
      Report.Engine.CurY := CurY;
    if FRepeatRowHeaders or (j = 0) then
    begin
      AddWidth := RowHeaderWidth;
      if j = 0 then
        AddWidth := AddWidth + Left;
    end
    else
      AddWidth := 0;

    kFrom := FRowBands[i].FromIndex;
    kTo := FRowBands[i].ToIndex;

    if FMeasurePlace <> -1 then
      for k := kFrom to kTo do
      begin
        Band := GetCellBand(k, j);
        Band.Top := Report.Engine.CurY;
        Report.Engine.ShowBand(Band);
        Band.Free;
      end
    else
      for k := kFrom to kTo do
      begin
        Band := GetCellBand(k, j);
        Band.Top := Report.Engine.CurY;
        Report.Engine.ShowBand(Band);
        Band.Free;
      end;
  end;

begin

  DoCalcSizes;
  AddSourceObjects;
  BuildColumnBands;
  BuildRowBands;

  SavePage := Report.PreviewPages.CurPage;
  Page := SavePage;
  SaveCurY := Report.Engine.CurY;
  CurY := SaveCurY;
  MaxX := 0;

  frxGlobalVariables['RowIndex'] := 0;
  frxGlobalVariables['ColumnIndex'] := 0;
  VarRowIndex := frxGlobalVariables.Items[frxGlobalVariables.IndexOf('RowIndex')];
  VarColumnIndex := frxGlobalVariables.Items[frxGlobalVariables.IndexOf('ColumnIndex')];

  if FDownThenAcross then
    for i := 0 to FColumnBands.Count - 1 do
    begin
      for j := 0 to FRowBands.Count - 1 do
      begin
        Report.PreviewPages.CurPage := Page + j;
        DoPagination(j, i);
        if j <> FRowBands.Count - 1 then
          Report.Engine.NewPage;
      end;

      if i <> FColumnBands.Count - 1 then
        Report.Engine.NewPage;
      CurY := Report.Engine.CurY;
      Inc(Page, FRowBands.Count);

      Application.ProcessMessages;
      if Report.Terminated then break;
    end
  else
    for i := 0 to FRowBands.Count - 1 do
    begin
      for j := 0 to FColumnBands.Count - 1 do
      begin
        Report.PreviewPages.CurPage := Page + j;
        MaxX := 0;
        DoPagination(i, j);
        if j <> FColumnBands.Count - 1 then
        begin
          Report.PreviewPages.AddPageAction := apWriteOver;
          Report.Engine.NewPage;
        end
        else if NextCross <> nil then
          NextCross.Left := MaxX + NextCrossGap;
      end;

      if i <> FRowBands.Count - 1 then
      begin
        Report.PreviewPages.AddPageAction := apAdd;
        Report.Engine.NewPage;
        Page := Report.PreviewPages.CurPage;
      end
      else
        Inc(Page, FColumnBands.Count);
      CurY := Report.Engine.CurY;

      Application.ProcessMessages;
      if Report.Terminated then break;
    end;

  if Parent is TfrxBand then
    CurY := CurY - Height;
  { print last page footers }
  if FColumnBands.Count > 1 then
    Report.Engine.EndPage;

  if NextCross <> nil then
  begin
    { position to last column, first row page }
    Report.PreviewPages.CurPage := SavePage + FColumnBands.Count - 1;
    Report.PreviewPages.AddPageAction := apAdd;
    Report.Engine.CurY := SaveCurY;
  end
  else
  begin
    { position to last row, first column page }
    Report.PreviewPages.CurPage := Page - FColumnBands.Count;
    Report.PreviewPages.AddPageAction := apAdd;
    Report.Engine.CurY := CurY;
  end;

  FColumnBands.Clear;
  FRowBands.Clear;

end;

function TfcxpCrossView.ColCount: Integer;
begin
  Result := Slice.ColCount;
end;

function TfcxpCrossView.RowCount: Integer;
begin
  Result := Slice.RowCount;
end;

procedure TfcxpCrossView.AddSourceObjects;
var
  i: Integer;
begin
  for i := 0 to FCellHeaderMemos.Count - 1 do
    Report.PreviewPages.AddToSourcePage(CellHeaderMemos[i]);
  for i := 0 to FCellMemos.Count - 1 do
    Report.PreviewPages.AddToSourcePage(CellMemos[i]);
  for i := 0 to FColumnMemos.Count - 1 do
  begin
    Report.PreviewPages.AddToSourcePage(ColumnMemos[i]);
    Report.PreviewPages.AddToSourcePage(ColumnTotalMemos[i]);
  end;
  for i := 0 to FCornerMemos.Count - 1 do
    Report.PreviewPages.AddToSourcePage(CornerMemos[i]);
  for i := 0 to FRowMemos.Count - 1 do
  begin
    Report.PreviewPages.AddToSourcePage(RowMemos[i]);
    Report.PreviewPages.AddToSourcePage(RowTotalMemos[i]);
  end;
end;

procedure TfcxpCrossView.BuildColumnBands;
var
  i, LeftIndex, RightIndex: Integer;
  CurWidth, AddWidth, LeftMargin, RightMargin: Extended;
  CellSize, CellOffs : Extended;

  procedure CreateBand;
  var
    i: Integer;
    Band: TfrxNullBand;
    Memo, CutMemo: TfrxCustomMemoView;
    CutSize: Extended;
  begin
    Band := TfrxNullBand.Create(Report);
    Band.Left := AddWidth;

    { move in-bounds memos to the new band }
    i := 0;
    while i < LargeBand.Objects.Count do
    begin
      Memo := LargeBand.Objects[i];
      if Memo.Left < RightMargin then
      begin
        if Memo.Left + Memo.Width <= RightMargin + 5 then
        begin
          Memo.Parent := Band;
          Memo.Visible := Memo.Width > 0;
          Dec(i);
        end
        else { cut off the memo }
        begin
          CutSize := RightMargin - Memo.Left;
          CutMemo := CreateMemo(Band);
          CutMemo.AssignAll(Memo);
          CutMemo.Width := CutSize;
          if CutMemo.CalcWidth > CutSize then
            CutMemo.Text := '';

          SetupOriginalComponent(CutMemo, Memo);
          Memo.Width := Memo.Width - CutSize;
          Memo.Left := Memo.Left + CutSize;
          if Memo is TfrxDMPMemoView then
          begin
            Memo.Left := Memo.Left + fr1CharX;
            Memo.Width := Memo.Width - fr1CharX;
          end;
          CutMemo.Frame.Typ := CutMemo.Frame.Typ - [ftRight];
          Memo.Frame.Typ := Memo.Frame.Typ - [ftLeft];

          if Memo.CalcWidth > Memo.Width then
            if CutMemo.Text = '' then
            begin
// need cut text !!!
              if CutMemo.Width >= Memo.Width then
              begin
                CutMemo.Text := Memo.Text;
                Memo.Text := '';
              end
            end
            else
              Memo.Text := '';
          Memo := CutMemo;
        end;

        Memo.Left := Memo.Left - LeftMargin;
      end;
      Inc(i);
    end;

    FColumnBands.Add(Band, LeftIndex, RightIndex);
  end;

begin
  FColumnBands.Clear;
  { create one large band }
  LargeBand := TfrxNullBand.Create(nil);

  { add memos to band }
  Slice.XAxisContainer.TraverseAxis(0, Slice.XAxisContainer.VisibleLevelCount - 1, 0, XAxisDrawProc);

  { cut it to small bands for each page }
  AddWidth := RowHeaderWidth;
  CurWidth := Report.Engine.PageWidth - AddWidth;
  LeftMargin  := -Left;
  RightMargin := LeftMargin + CurWidth;
  LeftIndex   := 0;
  RightIndex  := ColCount - 1;
  CellOffs := 0;
  if not TfrxReportPage(Page).EndlessWidth then
    for i := 0 to ColCount - 1 do
    begin
      { find right terminal item }
      CellSize := ColWidth[i];
      if CellSize = 0 then
        CellSize := DefaultColWidthInternal;
      if CellOffs + CellSize - LeftMargin > CurWidth then
      begin
        RightMargin := CellOffs;
        RightIndex := i - 1;
        CreateBand;
        LeftMargin := RightMargin;
        if FRepeatRowHeaders then
          AddWidth := RowHeaderWidth else
          AddWidth := 0;
        CurWidth := Report.Engine.PageWidth - AddWidth;
        RightMargin := LeftMargin + CurWidth;
        LeftIndex := RightIndex + 1;
        RightIndex := ColCount - 1;
      end;
      CellOffs := CellOffs + CellSize;
    end
  else
  begin
    for i := 0 to ColCount - 1 do
    begin
      { find right terminal item }
      CellSize := ColWidth[i];
      if CellSize = 0 then
        CellSize := DefaultColWidthInternal;
      CellOffs := CellOffs + CellSize;
    end;
    CurWidth := CellOffs - LeftMargin + AddWidth;
    if Report.Engine.PageWidth < CurWidth then
      Report.Engine.PageWidth := CurWidth;
    RightMargin := 1e+6;
  end;

//  add last band
  CreateBand;

  LargeBand.Free;
end;

procedure TfcxpCrossView.BuildRowBands;
var
  i, TopIndex, BottomIndex: Integer;
  CurHeight, AddHeight, TopMargin, BottomMargin: Extended;
  OffsetY : integer;
  AColumnHeaderHeight: Extended;

  procedure CreateBand;
  var
    i: Integer;
    Band: TfrxNullBand;
    Memo, CutMemo: TfrxCustomMemoView;
    CutSize: Extended;
  begin
    Band := TfrxNullBand.Create(Report);
    Band.Top := AddHeight;

    { move in-bounds memos to the new band }
    i := 0;
    while i < LargeBand.Objects.Count do
    begin
      Memo := LargeBand.Objects[i];
      if Memo.Top < BottomMargin then
      begin
        if Memo.Top + Memo.Height <= BottomMargin + 5 then
        begin
          Memo.Parent := Band;
          Dec(i);
        end
        else { cut off the memo }
        begin
          CutSize := BottomMargin - Memo.Top;
          CutMemo := CreateMemo(Band);
          CutMemo.AssignAll(Memo);
          CutMemo.Height := CutSize;
          SetupOriginalComponent(CutMemo, Memo);
          Memo.Height := Memo.Height - CutSize;
          Memo.Top := Memo.Top + CutSize;
          if Memo is TfrxDMPMemoView then
          begin
            Memo.Top := Memo.Top + fr1CharY;
            Memo.Height := Memo.Height - fr1CharY;
          end;
          CutMemo.Frame.Typ := CutMemo.Frame.Typ - [ftBottom];
          Memo.Frame.Typ := Memo.Frame.Typ - [ftTop];
          Memo := CutMemo;
        end;

        Memo.Top := Memo.Top - TopMargin;
      end;
      Inc(i);
    end;

    FRowBands.Add(Band, TopIndex, BottomIndex);
  end;

begin
  FRowBands.Clear;
  LargeBand := TfrxNullBand.Create(nil);
  FMaxHeight := 0;

  { create one large band }
  for i := 0 to RowLevels - 1 do
    FCrossTraversRowBottom[i] := 0;
  Slice.YAxisContainer.TraverseAxis(0, Slice.YAxisContainer.VisibleLevelCount - 1, 0, YAxisDrawProc);
  AColumnHeaderHeight := ColumnHeaderHeight;
  { cut it to small bands for each page }
  AddHeight := AColumnHeaderHeight;
  CurHeight := Report.Engine.FreeSpace - AddHeight;
  if (FMaxHeight > CurHeight) and KeepTogether then
  begin
    Report.Engine.NewPage;
    AddHeight := AColumnHeaderHeight;
    CurHeight := Report.Engine.FreeSpace - AddHeight;
  end;

  TopMargin := 0;
  BottomMargin := TopMargin + CurHeight;
  TopIndex  := 0;
  BottomIndex := RowCount - 1;
  OffsetY := 0;
  for i := 0 to RowCount - 1 do
  begin
    { find right terminal item }
    OffsetY := OffsetY + RowHeight[i];
    if OffsetY - TopMargin > CurHeight then
    begin
      BottomMargin := OffsetY - RowHeight[i];
      BottomIndex := i - 1;
      CreateBand;
      TopMargin := BottomMargin;
      if FRepeatColumnHeaders then
        AddHeight := AColumnHeaderHeight else
        AddHeight := 0;
      CurHeight := Report.Engine.PageHeight - Report.Engine.HeaderHeight -
        Report.Engine.FooterHeight - AddHeight;
      BottomMargin := TopMargin + CurHeight;
      TopIndex := BottomIndex + 1;
      BottomIndex := RowCount - 1;
    end;
  end;

  CreateBand;

  LargeBand.Free;
end;

function TfcxpCrossView.CreateMemo(Parent: TfrxComponent): TfrxCustomMemoView;
begin
  // create default memo for printing
  if FDotMatrix then
    Result := TfrxDMPMemoView.Create(Parent)
  else
    Result := TfcxpMemoView.Create(Parent);
  Result.AutoWidth   := False;
  Result.StretchMode := smDontStretch;
  Result.WordWrap    := True;
  Result.Frame.Typ   := [ftBottom, ftLeft, ftRight, ftTop];
  Result.Frame.Style := fsSolid;
  Result.AllowExpressions := False;
end;

procedure TfcxpCrossView.SetupOriginalComponent(Obj1, Obj2: TfrxComponent);
begin
  THackComponent(Obj1).FOriginalComponent := THackComponent(Obj2).FOriginalComponent;
  THackComponent(Obj1).FAliasName := THackComponent(Obj2).FAliasName;
end;

procedure TfcxpCrossView.CorrectDMPBounds(Memo: TfrxCustomMemoView);
begin
  if Memo is TfrxDMPMemoView then
  begin
    Memo.Left := Memo.Left + fr1CharX;
    Memo.Top := Memo.Top + fr1CharY;
    Memo.Width := Memo.Width - fr1CharX;
    Memo.Height := Memo.Height - fr1CharY;
  end;
end;

function TfcxpCrossView.ColumnHeaderHeight: Extended;
var
  I: integer;
begin
  if ShowColumnHeader then
  begin
    Result := NamesHeight;
    if FSlice.XAxisContainer.VisibleLevelCount = 0 then
      Result := Result + DefaultRowHeightInternal
    else
      for I := 0 to FSlice.XAxisContainer.VisibleLevelCount - 1 do
        Result := Result + GetXAxisRowHeight(I);
  end
  else
    Result := 0;
end;

function TfcxpCrossView.RowHeaderWidth: Extended;
var
  I: integer;
begin
  Result := 0;
  if ShowRowHeader then
  begin
    case FSlice.YAxisContainer.AxisType of
      at_Standard:
      begin
        for i := 0 to FSlice.YAxisContainer.VisibleLevelCount - 1 do
          Result := Result +  GetYAxisColWidth(i);
      end;
      at_Tree:
      begin
        if FSlice.YAxisContainer.LevelCount > 0 then
          Result := GetYAxisLevelSize;
        if FSlice.YAxisContainer.MeasuresLevel > -1 then
          Result := Result + Max(FSlice.YAxisContainer.LevelCount - 1, 0) * YAxisLevelSpacing + GetYAxisMeasureSize
        else
          Result := Result + Max(FSlice.YAxisContainer.LevelCount - 1, 0) * YAxisLevelSpacing;
      end;
    end;
  end;
end;

procedure TfcxpCrossView.ApplyStyle(Style: TfrxStyles);
var
  i: Integer;
  s: String;
  IsColTotal, IsRowTotal, IsColGrand, IsRowGrand : Boolean;
  NCol, INCol, NRow, INRow : Integer;
begin
  for i := 0 to FCellHeaderMemos.Count - 1 do
    CellHeaderMemos[i].ApplyStyle(Style.Find('cellheader'));

  if FMeasurePlace = 0 then
    NCol := ColumnLevels * CellLevels
  else
    NCol := ColumnLevels + 1;
  if FMeasurePlace = 1 then
    NRow := RowLevels * CellLevels
  else
    NRow := RowLevels + 1;

  for i := 0 to FCellMemos.Count - 1 do
  begin
    INCol := i mod NCol;
    INRow := i div NCol;
    IsColGrand := False;
    IsColTotal := False;
    IsRowGrand := False;
    IsRowTotal := False;
    if FMeasurePlace = 0 then
    begin
      if INCol >= CellLevels then
        if INCol >= (NCol - CellLevels - 1) then
          IsColGrand := True
        else
          IsColTotal := True;
    end
    else
    begin
      if INCol >= 1 then
        if INCol = (NCol - 1) then
          IsColGrand := True
        else
          IsColTotal := True;
    end;
    if FMeasurePlace = 1 then
    begin
      if INRow >= CellLevels then
        if INRow >= (NRow - CellLevels - 1) then
          IsRowGrand := True
        else
          IsRowTotal := True;
    end
    else
    begin
      if INRow >= 1 then
        if INRow = (NRow - 1) then
          IsRowGrand := True
        else
          IsRowTotal := True;
    end;
    CellMemos[i].ApplyStyle(Style.Find(CellStylesNames[IsColTotal, IsRowTotal, IsColGrand, IsRowGrand]));
  end;


  for i := 0 to FColumnMemos.Count - 1 do
  begin
    ColumnMemos[i].ApplyStyle(Style.Find('column'));
    if i = 0 then
      s := 'colgrand'
    else
      s := 'coltotal';
    ColumnTotalMemos[i].ApplyStyle(Style.Find(s));
  end;

  for i := 0 to FRowMemos.Count - 1 do
  begin
    RowMemos[i].ApplyStyle(Style.Find('row'));
    if i = 0 then
      s := 'rowgrand'
    else
      s := 'rowtotal';
    RowTotalMemos[i].ApplyStyle(Style.Find(s));
  end;

  for i := 0 to FCornerMemos.Count - 1 do
    CornerMemos[i].ApplyStyle(Style.Find('corner'));
end;

function TfcxpCrossView.AutoSizeMemo: Boolean;
begin
  Result := PaintSizes.AutoSizeStyle <> ssByMemoSize;
end;

procedure TfcxpCrossView.GetStyle(Style: TfrxStyles);
  procedure DoStyle(m: TfrxCustomMemoView; const s: String);
  var
    stItem: TfrxStyleItem;
  begin
    stItem := Style.Find(s);
    if stItem = nil then
      stItem := Style.Add;
    stItem.Name := s;
    stItem.Color := m.Color;
    stItem.Font := m.Font;
    stItem.Frame := m.Frame;
  end;
var
  IsColTotal, IsRowTotal, IsColGrand, IsRowGrand : Boolean;
  i, NCol, INCol, NRow, INRow : Integer;
begin
  if FCellHeaderMemos.Count > 0 then
    DoStyle(CellHeaderMemos[0], 'cellheader');


  if FCellMemos.Count > 0 then
  begin
    if FMeasurePlace = 0 then
      NCol := ColumnLevels * CellLevels
    else
      NCol := ColumnLevels + 1;
    if FMeasurePlace = 1 then
      NRow := RowLevels * CellLevels
    else
      NRow := RowLevels + 1;
    for i := 0 to FCellMemos.Count - 1 do
    begin
      INCol := i mod NCol;
      INRow := i div NCol;
      IsColGrand := False;
      IsColTotal := False;
      IsRowGrand := False;
      IsRowTotal := False;
      if FMeasurePlace = 0 then
      begin
        if INCol >= CellLevels then
          if INCol >= (NCol - CellLevels - 1) then
            IsColGrand := True
          else
            IsColTotal := True;
      end
      else
      begin
        if INCol >= 1 then
          if INCol = (NCol - 1) then
            IsColGrand := True
          else
            IsColTotal := True;
      end;
      if FMeasurePlace = 1 then
      begin
        if INRow >= CellLevels then
          if INRow >= (NRow - CellLevels - 1) then
            IsRowGrand := True
          else
            IsRowTotal := True;
      end
      else
      begin
        if INRow >= 1 then
          if INRow = (NRow - 1) then
            IsRowGrand := True
          else
            IsRowTotal := True;
      end;
      DoStyle(CellMemos[i], CellStylesNames[IsColTotal, IsRowTotal, IsColGrand, IsRowGrand]);
    end;
  end;

  if FColumnMemos.Count > 0 then
  begin
    DoStyle(ColumnMemos[0], 'column');
    DoStyle(ColumnTotalMemos[0], 'colgrand');
    if FColumnTotalMemos.Count > 1 then
      DoStyle(ColumnTotalMemos[1], 'coltotal');
  end;

  if FRowMemos.Count > 0 then
  begin
    DoStyle(RowMemos[0], 'row');
    DoStyle(RowTotalMemos[0], 'rowgrand');
    if FRowTotalMemos.Count > 1 then
      DoStyle(RowTotalMemos[1], 'rowtotal');
  end;

  if FCornerMemos.Count > 0 then
    DoStyle(CornerMemos[0], 'corner');

end;

function TfcxpCrossView.XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  m, Memo: TfrxCustomMemoView;
  i, ATotX: Integer;
  c: TfrxReportComponent;
  ATop, AHeight, ALeft, AWidth: Extended;
  AIsTotal, AIsMeasure: Boolean;
begin
// this is event from slice. We start the draw loop. Here we create memos
  Memo := CreateMemo(LargeBand);
  AIsTotal := (ARec.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [];
  AIsMeasure := (FMeasurePlace = 0) and (ARec.TreeRect.Level = FMeasureIndex);
  m := nil;
  if FMeasurePlace = 0 then
  begin
    ATotX := ColumnLevels - ARec.TreeRect.Level - 1;
    if AIsMeasure then
      m := CellHeaderMemos[Slice.MeasuresContainer.VisibleIndex[ARec.MeasureIndex] + CellLevels * ATotX]
    else
    if AIsTotal then // Total or Collapsed
      m := ColumnTotalMemos[ARec.Level]
    else
      m := ColumnMemos[ARec.Level];
  end
  else
  begin
    if CornerMemos[1].Text = '' then
      Memo.Frame.Typ := []
    else
    if AIsTotal then // Total or Collapsed
      m := ColumnTotalMemos[ARec.Level]
    else
      m := ColumnMemos[ARec.Level];
  end;
  if Assigned(m) then
  begin
    Memo.AssignAll(m);
    SetupOriginalComponent(Memo, m);
  end;
  Memo.AutoWidth := False;

  if CornerMemos[1].Text <> '' then
    Memo.Text := ARec.Text;

  ATop := NamesHeight;
  case Sender.AxisType of
    at_Standard:
      begin
        for i := 0 to ARec.TreeRect.Level - 1 do
          ATop := ATop + GetXAxisRowHeight(i);
        AHeight := 0;
        for i := ARec.TreeRect.Level to ARec.TreeRect.Level + ARec.TreeRect.SizeLevel - 1 do
          AHeight := AHeight + GetXAxisRowHeight(i);

        ALeft := 0;
        for i := 0 to ARec.TreeRect.Cell - 1 do
          ALeft := ALeft + GetXAxisColWidth(i);
        AWidth := 0;
        for i := ARec.TreeRect.Cell to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
          AWidth := AWidth + GetXAxisColWidth(i);

        Memo.SetBounds(ALeft,
                       ATop,
                       AWidth,
                       AHeight
                       ); // need Height of Real Row
      end;
    at_Tree:
      begin
        // TODO:
      end;
  end;

  Memo.Highlight.Condition := '';
  CorrectDMPBounds(Memo);
  Memo.Visible := (Memo.Width <> 0) and (Memo.Height <> 0);
  Report.LocalValue := THackMemoView(Memo).Value;
  Report.CurObject  := Memo.Name;
  Report.DoBeforePrint(Memo);

//  process memo children if any
  for i := 0 to Memo.Objects.Count - 1 do
  begin
    c := Memo.Objects[i];
    Report.CurObject := c.Name;
    Report.DoBeforePrint(c);
  end;
  Result := False;
end;

function TfcxpCrossView.YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  m, Memo: TfrxCustomMemoView;
  i, j, ATotY: integer;
  c: TfrxReportComponent;
  ALeft, AWidth, AHeight: Extended;
  AIsTotal, AIsMeasure: Boolean;
begin
// this is event from slice. We start the draw loop. Here we create memos
  Memo := CreateMemo(LargeBand);
  AIsTotal := (ARec.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [];
  AIsMeasure := (FMeasurePlace = 1) and (ARec.TreeRect.Level = FMeasureIndex);
  m := nil;
  if FMeasurePlace = 1 then
  begin
    ATotY := RowLevels - ARec.TreeRect.Level - 1;
    if AIsMeasure then
      m := CellHeaderMemos[Slice.MeasuresContainer.VisibleIndex[ARec.MeasureIndex] + CellLevels * ATotY]
    else
    if AIsTotal then // Total or Collapsed
      m := RowTotalMemos[ARec.Level]
    else
      m := RowMemos[ARec.Level];
  end
  else
  begin
    if CornerMemos[2].Text = '' then
      Memo.Frame.Typ := []
    else
    if AIsTotal then // Total or Collapsed
      m := RowTotalMemos[ARec.Level]
    else
      m := RowMemos[ARec.Level];
  end;
  if Assigned(m) then
  begin
    Memo.AssignAll(m);
    SetupOriginalComponent(Memo, m);
  end;
  Memo.AutoWidth := False;

//  Memo.Text := Memo.FormatData(Rec.Value);
  if CornerMemos[2].Text <> '' then
    Memo.Text := ARec.Text;

  Memo.Highlight.Condition := '';

  ALeft := 0;

  case Sender.AxisType of
    at_Standard:
      begin
        for j := 0 to ARec.TreeRect.Level - 1 do
          ALeft := ALeft + GetYAxisColWidth(j);
        AWidth := 0;
        for j := ARec.TreeRect.Level to ARec.TreeRect.Level + ARec.TreeRect.SizeLevel - 1 do
          AWidth := AWidth + GetYAxisColWidth(j);

        if pca_LastLevel in ARec.CellProperties then
          AHeight := GetYAxisRowHeight(ARec.TreeRect.Cell)
        else
          AHeight := FCrossTraversRowBottom[ARec.TreeRect.Level+1]-FCrossTraversRowBottom[ARec.TreeRect.Level];
        Memo.SetBounds(ALeft,
                       FCrossTraversRowBottom[ARec.TreeRect.Level],
                       AWidth,
                       AHeight);
        FCrossTraversRowBottom[ARec.TreeRect.Level] := FCrossTraversRowBottom[ARec.TreeRect.Level] + AHeight;
        if pca_LastLevel in ARec.CellProperties then
          for i := ARec.TreeRect.Level + 1 to RowLevels - 1 do
            FCrossTraversRowBottom[i] := FCrossTraversRowBottom[ARec.TreeRect.Level];
      end;
    at_Tree:
      begin
        if pca_TreeMeasureCell in ARec.CellProperties then
        begin
          if Sender.LevelCount > 0 then
            ALeft := ALeft + GetYAxisLevelSize + (Sender.LevelCount - 1) * YAxisLevelSpacing;
          AWidth := GetYAxisMeasureSize;
        end
        else
        begin
          if Sender.MeasuresLevel > -1 then
            if ARec.TreeRect.Level <= Sender.MeasuresLevel then
              ALeft := ALeft + ARec.TreeRect.Level * YAxisLevelSpacing
            else
              ALeft := ALeft + (ARec.TreeRect.Level - 1) * YAxisLevelSpacing
          else
            ALeft := ALeft + ARec.TreeRect.Level * YAxisLevelSpacing;

          if pca_TreeCellWithMeasure in ARec.CellProperties then
            if Sender.MeasuresLevel > -1 then
              if ARec.TreeRect.Level <= Sender.MeasuresLevel then
                AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing
              else
                AWidth := GetYAxisLevelSize + (Sender.LevelCount - ARec.TreeRect.Level) * YAxisLevelSpacing
            else
              AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing
          else
            if Sender.MeasuresLevel > -1 then
              if ARec.TreeRect.Level < Sender.MeasuresLevel then
                AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing + GetYAxisMeasureSize
              else
                AWidth := GetYAxisLevelSize + (Sender.LevelCount - ARec.TreeRect.Level) * YAxisLevelSpacing + GetYAxisMeasureSize
            else
              AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing;
        end;
        if ARec.TreeRect.SizeCell = 1 then
          AHeight := GetYAxisRowHeight(ARec.TreeRect.Cell)
        else
          AHeight := FCrossTreeRowBottom[not (pca_LastLevel in ARec.CellProperties)] - FCrossTreeRowBottom[pca_LastLevel in ARec.CellProperties];
        Memo.SetBounds(ALeft,
                       FCrossTreeRowBottom[pca_LastLevel in ARec.CellProperties],
                       AWidth,
                       AHeight);
        FCrossTreeRowBottom[pca_LastLevel in ARec.CellProperties] := FCrossTreeRowBottom[pca_LastLevel in ARec.CellProperties] + AHeight;
        if ([pca_LastLevel, pca_TreeMeasureCell] * ARec.CellProperties = [pca_LastLevel]) then
          FCrossTreeRowBottom[False] := FCrossTreeRowBottom[pca_LastLevel in ARec.CellProperties];
      end;
  end;

  if Memo.Top + Memo.Height > FMaxHeight then
    FMaxHeight := Memo.Top + Memo.Height;
  CorrectDMPBounds(Memo);
  Memo.Visible := (Memo.Width <> 0) and (Memo.Height <> 0);

  Report.LocalValue := THackMemoView(Memo).Value;
  Report.CurObject  := Memo.Name;
  Report.DoBeforePrint(Memo);

//  process memo children if any
  for j := 0 to Memo.Objects.Count - 1 do
  begin
    c := Memo.Objects[j];
    Report.CurObject := c.Name;
    Report.DoBeforePrint(c);
  end;
  Result := False;
end;

procedure TfcxpCrossView.ClearMemos;
begin
  while FCellHeaderMemos.Count > 0 do
  begin
    CellHeaderMemos[0].Free;
    FCellHeaderMemos.Delete(0);
  end;
  while FCellMemos.Count > 0 do
  begin
    CellMemos[0].Free;
    FCellMemos.Delete(0);
  end;
  while FColumnMemos.Count > 0 do
  begin
    ColumnMemos[0].Free;
    FColumnMemos.Delete(0);
    ColumnTotalMemos[0].Free;
    FColumnTotalMemos.Delete(0);
  end;
  while FRowMemos.Count > 0 do
  begin
    RowMemos[0].Free;
    FRowMemos.Delete(0);
    RowTotalMemos[0].Free;
    FRowTotalMemos.Delete(0);
  end;
  while FCornerMemos.Count > 0 do
  begin
    CornerMemos[0].Free;
    FCornerMemos.Delete(0);
  end;
end;

function TfcxpCrossView.GetCellHeaderMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FCellHeaderMemos[Index];
end;

function TfcxpCrossView.GetCellMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FCellMemos[Index];
end;

function TfcxpCrossView.GetColumnMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FColumnMemos[Index];
end;

function TfcxpCrossView.GetColumnTotalMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FColumnTotalMemos[Index];
end;

function TfcxpCrossView.GetCornerMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FCornerMemos[Index];
end;

function TfcxpCrossView.GetRowMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FRowMemos[Index];
end;

function TfcxpCrossView.GetRowTotalMemos(Index: Integer): TfrxCustomMemoView;
begin
  Result := FRowTotalMemos[Index];
end;

procedure TfcxpCrossView.ReadMemos(Stream: TStream);
var
  x: TfrxXMLDocument;
  i: Integer;

  procedure GetItem(m: TfrxCustomMemoView; const Name: String; Index: Integer);
  var
    xs: TfrxXMLSerializer;
    Item: TfrxXMLItem;
  begin
    Item := x.Root.FindItem(Name);
    if Index >= Item.Count then Exit;
    Item := Item[Index];

    xs := TfrxXMLSerializer.Create(nil);
    m.Color := clNone;
    m.Frame.Color := clBlack;
    m.Frame.Width := 1;
    m.Frame.Typ := [];
    m.Font.Style := [];
    m.HAlign := haLeft;
    m.VAlign := vaTop;
    xs.ReadRootComponent(m, Item);
    xs.Free;
  end;

begin
  // if we have no Cube, we just creating temporary stream and wait until we get it
  if (Stream.Size > 0) and (FSlice.Cube = nil) then
  begin
    FLoadMemos := TMemoryStream.Create;
    FLoadMemos.LoadFromStream(Stream);
    FLoadMemos.Position := 0;
    Exit;
  end;
  
  x := TfrxXMLDocument.Create;
  try
    x.LoadFromStream(Stream);

    for i := 0 to FCellHeaderMemos.Count - 1 do
      GetItem(CellHeaderMemos[i], 'cellheadermemos', i);

    for i := 0 to FCellMemos.Count - 1 do
      GetItem(CellMemos[i], 'cellmemos', i);

    for i := 0 to FColumnMemos.Count - 1 do
    begin
      GetItem(ColumnMemos[i], 'columnmemos', i);
      GetItem(ColumnTotalMemos[i], 'columntotalmemos', i);
    end;

    for i := 0 to FRowMemos.Count - 1 do
    begin
      GetItem(RowMemos[i], 'rowmemos', i);
      GetItem(RowTotalMemos[i], 'rowtotalmemos', i);
    end;

    for i := 0 to FCornerMemos.Count - 1 do
      GetItem(CornerMemos[i], 'cornermemos', i);

  finally
    x.Free;
  end;
end;

procedure TfcxpCrossView.WriteMemos(Stream: TStream);
var
  x: TfrxXMLDocument;
  i: Integer;

  procedure AddItem(m: TfrxCustomMemoView; const Name: String);
  var
    xs: TfrxXMLSerializer;
  begin
    xs := TfrxXMLSerializer.Create(nil);
    xs.WriteRootComponent(m, True, x.Root.FindItem(Name).Add);
    xs.Free;
  end;

begin
  x := TfrxXMLDocument.Create;
  x.Root.Name := 'fccross';

  try
    x.Root.Add.Name := 'cellmemos';
    x.Root.Add.Name := 'cellheadermemos';
    x.Root.Add.Name := 'columnmemos';
    x.Root.Add.Name := 'columntotalmemos';
    x.Root.Add.Name := 'cornermemos';
    x.Root.Add.Name := 'rowmemos';
    x.Root.Add.Name := 'rowtotalmemos';

    for i := 0 to FCellHeaderMemos.Count - 1 do
      AddItem(CellHeaderMemos[i], 'cellheadermemos');

    for i := 0 to FCellMemos.Count - 1 do
      AddItem(CellMemos[i], 'cellmemos');

    for i := 0 to FColumnMemos.Count - 1 {FColumnLevels - 1} do
    begin
      AddItem(ColumnMemos[i], 'columnmemos');
      AddItem(ColumnTotalMemos[i], 'columntotalmemos');
    end;

    for i := 0 to FRowMemos.Count - 1 {FRowLevels - 1} do
    begin
      AddItem(RowMemos[i], 'rowmemos');
      AddItem(RowTotalMemos[i], 'rowtotalmemos');
    end;

    for i := 0 to FCornerMemos.Count - 1 do
      AddItem(CornerMemos[i], 'cornermemos');

    x.SaveToStream(Stream);
  finally
    x.Free;
  end;
end;

function TfcxpCrossView.GetCellLevels: Integer;
begin
  Result := 0;
  if (Slice <> nil) and Slice.Active then
  begin
    if Slice.MeasuresContainer.InAxis then
      Result := Slice.MeasuresContainer.VisibleCount
    else
      Result := 0;
  end;
end;

function TfcxpCrossView.GetColumnLevels: Integer;
begin
  Result := 0;
  if Assigned(Slice) and Slice.Active then
    Result := Slice.XAxisContainer.VisibleLevelCount;
end;

function TfcxpCrossView.GetRowLevels: Integer;
begin
  Result := 0;
  if Assigned(Slice) and Slice.Active then
    Result := Slice.YAxisContainer.VisibleLevelCount;
end;

procedure TfcxpCrossView.CreateCellHeaderMemos(NewCount: Integer);
var
  i: Integer;
  m: TfrxCustomMemoView;
begin
  for i := FCellHeaderMemos.Count to NewCount - 1 do
  begin
    m := CreateMemo(nil);
    FCellHeaderMemos.Add(m);
    m.Restrictions := [rfDontDelete];
    m.Text := 'CellHeader' + IntToStr(i);
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  end;
  while FCellHeaderMemos.Count > NewCount do
  begin
    CellHeaderMemos[FCellHeaderMemos.Count - 1].Free;
    FCellHeaderMemos.Delete(FCellHeaderMemos.Count - 1);
  end;
end;

procedure TfcxpCrossView.CreateCellMemos(NewCount: Integer);
var
  i: Integer;
  m: TfrxCustomMemoView;
begin
  for i := FCellMemos.Count to NewCount - 1 do
  begin
    m := CreateMemo(nil);
    FCellMemos.Add(m);
    m.Restrictions := [rfDontDelete];
    m.HAlign := haRight;
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  end;
  while FCellMemos.Count > NewCount do
  begin
    CellMemos[FCellMemos.Count - 1].Free;
    FCellMemos.Delete(FCellMemos.Count - 1);
  end;
end;

procedure TfcxpCrossView.CreateColumnMemos(NewCount: Integer);
var
  i: Integer;
  m: TfrxCustomMemoView;
begin
  if FMeasurePlace = 0 then
    NewCount := NewCount - 1;
  for i := FColumnMemos.Count to NewCount - 1 do
  begin
    m := CreateMemo(nil);
    FColumnMemos.Add(m);
    m.Restrictions := [rfDontDelete, rfDontEdit];
    m.Text := 'Column' + IntToStr(i);
    m.HAlign := haCenter;
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];

    m := CreateMemo(nil);
    FColumnTotalMemos.Add(m);
    m.Restrictions := [rfDontDelete];
    if i = 0 then
      m.Text := 'Grand Total'
    else
      m.Text := 'Total' + IntToStr(i);
    m.Font.Style := [fsBold];
    m.HAlign := haCenter;
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  end;
  while FColumnMemos.Count > Max(0, NewCount) do
  begin
    ColumnMemos[FColumnMemos.Count - 1].Free;
    FColumnMemos.Delete(FColumnMemos.Count - 1);
    ColumnTotalMemos[FColumnTotalMemos.Count - 1].Free;
    FColumnTotalMemos.Delete(FColumnTotalMemos.Count - 1);
  end;
end;

procedure TfcxpCrossView.CreateRowMemos(NewCount: Integer);
var
  i: Integer;
  m: TfrxCustomMemoView;
begin
  if FMeasurePlace = 1 then
    NewCount := NewCount - 1;
  for i := FRowMemos.Count to NewCount - 1 do
  begin
    m := CreateMemo(nil);
    FRowMemos.Add(m);
    m.Restrictions := [rfDontDelete, rfDontEdit];
    m.Text := 'Row' + IntToStr(i);
    m.HAlign := haCenter;
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];

    m := CreateMemo(nil);
    FRowTotalMemos.Add(m);
    m.Restrictions := [rfDontDelete];
    if i = 0 then
      m.Text := 'Grand Total'
    else
      m.Text := 'Total' + IntToStr(i);
    m.Font.Style := [fsBold];
    m.HAlign := haCenter;
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  end;
  while FRowMemos.Count > NewCount do
  begin
    RowMemos[FRowMemos.Count - 1].Free;
    FRowMemos.Delete(FRowMemos.Count - 1);
    RowTotalMemos[FRowTotalMemos.Count - 1].Free;
    FRowTotalMemos.Delete(FRowTotalMemos.Count - 1);
  end;
end;

procedure TfcxpCrossView.CreateCornerMemos(NewCount: Integer);
var
  i, j: Integer;
  m: TfrxCustomMemoView;
  ANames: string;
begin
  for i := FCornerMemos.Count to NewCount - 1 do
  begin
    m := CreateMemo(nil);
    FCornerMemos.Add(m);
    m.Restrictions := [rfDontDelete];
    m.HAlign := haCenter;
    m.VAlign := vaCenter;
    m.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  end;
  while FCornerMemos.Count > NewCount do
  begin
    CornerMemos[FCornerMemos.Count - 1].Free;
    FCornerMemos.Delete(FCornerMemos.Count - 1);
  end;
  for i := 0 to FCornerMemos.Count - 1 do
  begin
    case i of
      0:
        CornerMemos[i].Text := '';
      1:
        if Slice.Active then
        begin
          ANames := '';
          for j := 0 to Slice.XAxisContainer.VisibleLevelCount - 1 do
          begin
            if j <> 0 then
              ANames := ANames + ', ';
            if Slice.XAxisContainer.VisibleLevelInfo[j].IsMeasure then
              ANames := ANames + Slice.MeasuresContainer.Caption
            else
              ANames := ANames + Slice.XAxisContainer.VisibleLevelInfo[j].RegionField.Caption;
          end;
          CornerMemos[i].Text := ANames;
        end;
      2:
        if Slice.Active then
        begin
          ANames := '';
          for j := 0 to Slice.YAxisContainer.VisibleLevelCount - 1 do
          begin
            if j <> 0 then
              ANames := ANames + ', ';
            if Slice.YAxisContainer.VisibleLevelInfo[j].IsMeasure then
              ANames := ANames + Slice.MeasuresContainer.Caption
            else
              ANames := ANames + Slice.YAxisContainer.VisibleLevelInfo[j].RegionField.Caption;
          end;
          CornerMemos[i].Text := ANames;
        end;
      else
       CornerMemos[i].Text := '';
    end;
  end;

end;

procedure TfcxpCrossView.InitMemos(AddToScript: Boolean);
var
  i, j: Integer;
  m: TfrxCustomMemoView;
  NCol, INCol, INRow: integer;
  NestedObjects: TList;
  ANames: String;
begin
  if FMeasurePlace = 0 then
    NCol := ColumnLevels * CellLevels
  else
    NCol := ColumnLevels + 1;

  for i := 0 to FCellHeaderMemos.Count - 1 do
  begin
    m := CellHeaderMemos[i];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'CellHeader' + IntToStr(i);
    m.Text := '[' + Slice.MeasuresContainer.VisibleMeasures[i mod CellLevels].Caption + ']';
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  for i := 0 to FCellMemos.Count - 1 do
  begin
    m := CellMemos[i];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    INCol := i mod NCol;
    INRow := i div NCol;

    m.Name  := Name + 'Cell' + IntToStr(INRow) + '_' + IntToStr(INCol);
    m.Value := 0;

    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  for i := 0 to FColumnMemos.Count - 1 do
  begin
    m := ColumnMemos[i];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'Column' + IntToStr(i);

    if FMeasurePlace = 0 then
    begin
      m.Text := '[' + Slice.XAxisContainer.LevelInfoWOMeasures[i].RegionField.Caption + ']';
    end
    else
    begin
      if Slice.XAxisContainer.VisibleLevelCount > 0 then
        m.Text := '[' + Slice.XAxisContainer.LevelInfo[i].RegionField.Caption + ']'
      else
        m.Text := '';
    end;
    if AddToScript then
      Report.Script.AddObject(m.Name, m);

    m := ColumnTotalMemos[i];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'ColumnTotal' + IntToStr(i);

    if FMeasurePlace = 0 then
    begin
      m.Text := 'Total(' + Slice.XAxisContainer.LevelInfoWOMeasures[i].RegionField.Caption + ')';
    end
    else
    begin
      if Slice.XAxisContainer.VisibleLevelCount > 0 then
        m.Text := 'Total(' + Slice.XAxisContainer.LevelInfo[i].RegionField.Caption + ')'
      else
        m.Text := '';
    end;
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  for i := 0 to FRowMemos.Count - 1 do
  begin
    m := RowMemos[i];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'Row' + IntToStr(i);

    if FMeasurePlace = 1 then
    begin
      m.Text := '[' + Slice.YAxisContainer.LevelInfoWOMeasures[i].RegionField.Caption + ']';
    end
    else
    begin
      if Slice.YAxisContainer.VisibleLevelCount > 0 then
        m.Text := '[' + Slice.YAxisContainer.LevelInfo[i].RegionField.Caption + ']'
      else
        m.Text := '';
    end;
    if AddToScript then
      Report.Script.AddObject(m.Name, m);

    m := RowTotalMemos[i];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'RowTotal' + IntToStr(i);

    if FMeasurePlace = 1 then
    begin
      m.Text := 'Total(' + Slice.YAxisContainer.LevelInfoWOMeasures[i].RegionField.Caption + ')';
    end
    else
    begin
      if Slice.YAxisContainer.VisibleLevelCount > 0 then
        m.Text := 'Total(' + Slice.YAxisContainer.LevelInfo[i].RegionField.Caption + ')'
      else
        m.Text := '';
    end;
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  if ShowColumnHeader and ShowRowHeader then
  begin
    m := CornerMemos[0];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'Corner0';
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  if ShowColumnHeader and ShowNames then
  begin
    m := CornerMemos[1];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'Corner1';

    ANames := '';
    for j := 0 to Slice.XAxisContainer.VisibleLevelCount - 1 do
    begin
      if j <> 0 then
        ANames := ANames + ', ';
      if Slice.XAxisContainer.VisibleLevelInfo[j].IsMeasure then
        ANames := ANames + Slice.MeasuresContainer.Caption
      else
        ANames := ANames + Slice.XAxisContainer.VisibleLevelInfo[j].RegionField.Caption;
    end;
    CornerMemos[1].Text := ANames;
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;


  if ShowRowHeader and ShowColumnHeader and ShowNames then
  begin
    m := CornerMemos[2];
    m.GapX := FGapX;
    m.GapY := FGapY;
    m.AllowExpressions := False;
    m.Name := Name + 'Corner2';

    ANames := '';
    for j := 0 to Slice.YAxisContainer.VisibleLevelCount - 1 do
    begin
      if j <> 0 then
        ANames := ANames + ', ';
      if Slice.YAxisContainer.VisibleLevelInfo[j].IsMeasure then
        ANames := ANames + Slice.MeasuresContainer.Caption
      else
        ANames := ANames + Slice.YAxisContainer.VisibleLevelInfo[j].RegionField.Caption;
    end;
    CornerMemos[2].Text := ANames;
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  SetMemosBounds; // ???

  NestedObjects := GetNestedObjects;

  for i := 0 to NestedObjects.Count - 1 do
  begin
    m := NestedObjects[i];
    m.Name := Name + 'Object' + IntToStr(m.Tag);
    if AddToScript then
      Report.Script.AddObject(m.Name, m);
  end;

  NestedObjects.Free;

end;

function TfcxpCrossView.GetNestedObjects: TList;
var
  i: Integer;
  NestedObjects: TList;

  procedure DoNested(Memo: TfrxCustomMemoView);
  var
    i: Integer;
    c: TfrxComponent;
  begin
    for i := 0 to Memo.Objects.Count - 1 do
    begin
      c := Memo.Objects[i];
      NestedObjects.Add(c);
    end;
  end;

begin
  NestedObjects := TList.Create;

  for i := 0 to FCellHeaderMemos.Count - 1 do
    DoNested(CellHeaderMemos[i]);

  for i := 0 to FCellMemos.Count - 1 do
    DoNested(CellMemos[i]);

  for i := 0 to FColumnMemos.Count - 1 do
  begin
    DoNested(ColumnMemos[i]);
    DoNested(ColumnTotalMemos[i]);
  end;

  for i := 0 to FRowMemos.Count - 1 do
  begin
    DoNested(RowMemos[i]);
    DoNested(RowTotalMemos[i]);
  end;

  for i := 0 to FCornerMemos.Count - 1 do
    DoNested(CornerMemos[i]);

  Result := NestedObjects;
end;

procedure TfcxpCrossView.SetDotMatrix(const Value: Boolean);
begin
  FDotMatrix := Value;
  if FDotMatrix then
  begin
    FGapX := 0;
    FGapY := 0;
  end;
end;

function TfcxpCrossView.ExecuteAction(Action: TBasicAction): Boolean;
begin
  If (Action is TfcxAction) and (TfcxAction(Action).ChangeAlert is TfcxSliceChangeAlert) then
  begin
    SliceChanged;
    Result := True;
  end else
    Result := False;
end;

procedure TfcxpCrossView.SliceChanged;
begin
  CreateMemos;
end;

procedure TfcxpCrossView.CreateMemos;
var
  lvl: Integer;
  i, NCol, NRow: Integer;
begin
  FMeasurePlace := -1;
  FMeasureIndex := -1;
  if Slice.MeasuresContainer.Container = Slice.XAxisContainer then
  begin
    FMeasurePlace := 0;
    FMeasureIndex := Slice.XAxisContainer.MeasuresLevelVisibleIndex;
  end
  else
  if Slice.MeasuresContainer.Container = Slice.YAxisContainer then
  begin
    FMeasurePlace := 1;
    FMeasureIndex := Slice.YAxisContainer.MeasuresLevelVisibleIndex;
  end;
// It is necessary to save and restore properties
  lvl := ColumnLevels;
  CreateColumnMemos(lvl);
  lvl := RowLevels;
  CreateRowMemos(lvl);
  CreateCornerMemos(3);
  if FMeasurePlace = 0 then
    NCol := ColumnLevels * CellLevels
  else
    NCol := ColumnLevels + 1;
  if FMeasurePlace = 1 then
    NRow := RowLevels * CellLevels
  else
    NRow := RowLevels + 1;
  CreateCellMemos(NCol * NRow);
  if FMeasurePlace < 0 then
    CreateCellHeaderMemos(0)
  else
  if FMeasurePlace = 1 then
    CreateCellHeaderMemos(CellLevels * (RowLevels))
  else
    CreateCellHeaderMemos(CellLevels * (ColumnLevels));
  if FUseGridColors then
  begin
    for i := 0 to FCornerMemos.Count - 1 do
    begin
      case i of
        0,1:
          begin
            ApplyMemoStyle(CornerMemos[i], FGridStyles.CaptionArea);
            CornerMemos[i].HAlign := haLeft;
          end
      else
        if FMeasurePlace = 1 then
        begin
          if FMeasureIndex = (i - 2) then
          begin
            ApplyMemoStyle(CornerMemos[i], FGridStyles.Measure);
            CornerMemos[i].HAlign := haLeft;
          end
          else
          begin
            ApplyMemoStyle(CornerMemos[i], FGridStyles.ActiveDimension);
            CornerMemos[i].HAlign := haLeft;
          end;
        end
        else
        begin
          ApplyMemoStyle(CornerMemos[i], FGridStyles.ActiveDimension);
          CornerMemos[i].HAlign := haLeft;
        end;
      end;
    end;

    for i := 0 to FCellHeaderMemos.Count - 1 do
    begin
      ApplyMemoStyle(CellHeaderMemos[i], FGridStyles.HeaderCells);
      CellHeaderMemos[i].HAlign := haCenter;
    end;
  end;
  if FUseGridColors or FUseGridAlign then
  begin
    for i := 0 to FColumnMemos.Count - 1 do
    begin
      if FUseGridColors then
        ApplyMemoStyle(ColumnMemos[i], FGridStyles.HeaderCells);
      if FUseGridAlign and not Slice.XAxisContainer.VisibleLevelInfo[i].IsMeasure then
        ColumnMemos[i].HAlign := cAlignment2HAlign[Slice.XAxisContainer.VisibleLevelInfo[i].RegionField.Alignment];
    end;

    for i := 0 to FRowMemos.Count - 1 do
    begin
      if FUseGridColors then
        ApplyMemoStyle(RowMemos[i], FGridStyles.HeaderCells);
      if FUseGridAlign and not Slice.YAxisContainer.VisibleLevelInfo[i].IsMeasure then
        RowMemos[i].HAlign := cAlignment2HAlign[Slice.YAxisContainer.VisibleLevelInfo[i].RegionField.Alignment];
    end;

    for i := 0 to FColumnTotalMemos.Count - 1 do
    begin
      if FUseGridColors then
        ApplyMemoStyle(ColumnTotalMemos[i], FGridStyles.HeaderCells);
      if FUseGridAlign and not Slice.XAxisContainer.VisibleLevelInfo[i].IsMeasure then
        ColumnTotalMemos[i].HAlign := cAlignment2HAlign[Slice.XAxisContainer.VisibleLevelInfo[i].RegionField.Alignment];
    end;

    for i := 0 to FRowTotalMemos.Count - 1 do
    begin
      if FUseGridColors then
        ApplyMemoStyle(RowTotalMemos[i], FGridStyles.HeaderCells);
      if FUseGridAlign and not Slice.YAxisContainer.VisibleLevelInfo[i].IsMeasure then
        RowTotalMemos[i].HAlign := cAlignment2HAlign[Slice.YAxisContainer.VisibleLevelInfo[i].RegionField.Alignment];
    end;
  end;

  SetLength(FHeadColSize, ColumnLevels + 1);
  SetLength(FHeadRowSize, RowLevels);
  SetLength(FColSize, NCol);
  SetLength(FRowSize, NRow);

  SetMemosPos;
  SetMemosBounds;
end;

function TfcxpCrossView.IsCrossValid: Boolean;
begin
  Result := (Slice <> nil)
end;

function TfcxpCrossView.GetContainerObjects: TList;
begin
  Result := FAllMemos;
end;

function TfcxpCrossView.ContainerAdd(Obj: TfrxComponent): Boolean;
var
  i, j, n: Integer;
  c: TfrxComponent;
  Offset: TfrxPoint;
  NestedObjects: TList;
  Found: Boolean;
begin
  Result := False;
  if (Obj is TfcxpCrossView) or (Obj is TfrxSubreport) then Exit;

  Offset := frxPoint(20, 20);
  if FDotMatrix then
    Offset := frxPoint(fr1CharX * 2, fr1CharY);
  { call DrawCross to calc visible memos and their bounds }
  DrawCross(nil, FScaleX, FScaleY, AbsLeft + Offset.X, AbsTop + Offset.Y);

  { find parent memo for added object }
  for i := 0 to FAllMemos.Count - 1 do
  begin
    c := FAllMemos[i];
    if (Obj.Left >= c.Left) and (Obj.Top >= c.Top) and
      (Obj.Left <= c.Left + c.Width) and
      (Obj.Top <= c.Top + c.Height) then
    begin
      Obj.Left := Obj.Left - c.Left;
      Obj.Top := Obj.Top - c.Top;
      Obj.Owner.RemoveComponent(Obj);
      Obj.Parent := c;

      { create unique tag for it - it will be used for name creation }
      NestedObjects := GetNestedObjects;
      n := 0;
      while True do
      begin
        Inc(n);
        Found := False;
        for j := 0 to NestedObjects.Count - 1 do
          if TfrxComponent(NestedObjects[j]).Tag = n then
          begin
            Found := True;
            break;
          end;
        if not Found then
        begin
          Obj.Tag := n;
          Obj.Name := Name + 'Object' + IntToStr(n);
          break;
        end;
      end;

      NestedObjects.Free;
      Result := True;
      break;
    end;
  end;
end;

function TfcxpCrossView.ContainerMouseDown(Sender: TObject; X,
  Y: Integer): Boolean;
var
  i, j: Integer;
  c: TfrxComponent;
  Offset: TfrxPoint;
begin
  Result := False;
  if AutoSizeMemo then
    exit;
  Offset := frxPoint(20, 20);
  if FDotMatrix then
    Offset := frxPoint(fr1CharX * 2, fr1CharY);
  DrawCross(nil, FScaleX, FScaleY, AbsLeft + Offset.X, AbsTop + Offset.Y);
  FGridUsed := nil;
  FFirstMousePos := Point(X, Y);
  FLastMousePos := Point(X, Y);

  for i := 0 to FGridX.Count - 1 do
    for j := 0 to FGridX[i].Objects.Count - 1 do
    begin
      c := FGridX[i].Objects[j];
      if (Abs(c.AbsLeft + c.Width - X / FScaleX) < 2) and
        (Y / FScaleY >= c.AbsTop) and (Y / FScaleY <= c.AbsTop + c.Height) then
      begin
        FGridUsed := FGridX;
        FMovingObjects := i;
        Result := True;
        break;
      end;
    end;

  for i := 0 to FGridY.Count - 1 do
    for j := 0 to FGridY[i].Objects.Count - 1 do
    begin
      c := FGridY[i].Objects[j];
      if (Abs(c.AbsTop + c.Height - Y / FScaleY) < 2) and
        (X / FScaleX >= c.AbsLeft) and (X / FScaleX <= c.AbsLeft + c.Width) then
      begin
        FGridUsed := FGridY;
        FMovingObjects := i;
        Result := True;
        break;
      end;
    end;

  FMouseDown := Result;
end;

procedure TfcxpCrossView.ContainerMouseMove(Sender: TObject; X,
  Y: Integer);
var
  i, j: Integer;
  c: TfrxComponent;
  Offset: TfrxPoint;
  ACol : integer;
begin
  if AutoSizeMemo then exit;
  if (FScaleX = 0) or (FScaleY = 0) then Exit;

  if not FMouseDown then
  begin
    Offset := frxPoint(20, 20);
    if FDotMatrix then
      Offset := frxPoint(fr1CharX * 2, fr1CharY);
    DrawCross(nil, FScaleX, FScaleY, AbsLeft + Offset.X, AbsTop + Offset.Y);

    for i := 0 to FGridX.Count - 1 do
      for j := 0 to FGridX[i].Objects.Count - 1 do
      begin
        c := FGridX[i].Objects[j];
        if (Abs(c.AbsLeft + c.Width - X / FScaleX) < 2) and
          (Y / FScaleY >= c.AbsTop) and (Y / FScaleY <= c.AbsTop + c.Height) then
        begin
          TWinControl(Sender).Cursor := crHSplit;
          break;
        end;
      end;

    for i := 0 to FGridY.Count - 1 do
      for j := 0 to FGridY[i].Objects.Count - 1 do
      begin
        c := FGridY[i].Objects[j];
        if (Abs(c.AbsTop + c.Height - Y / FScaleY) < 2) and
          (X / FScaleX >= c.AbsLeft) and (X / FScaleX <= c.AbsLeft + c.Width) then
        begin
          TWinControl(Sender).Cursor := crVSplit;
          break;
        end;
      end;
  end
  else
  begin
    Offset := frxPoint(20, 20);
    if FGridUsed = FGridX then
    begin

      if FGridX[FMovingObjects].Objects.Count > 0 then
      begin
        ACol := -1;
        for i := 0 to High(FHeadRowSize) do
        begin
          if round(FHeadRowSize[i].X + Left + Offset.X) = round(TfrxComponent(FGridX[FMovingObjects].Objects[0]).Left + TfrxComponent(FGridX[FMovingObjects].Objects[0]).Width) then
          begin
            ACol := i;
            Break;
          end;
        end;
        if ACol = -1 then
        begin
          for i := 0 to High(FColSize) do
          begin
            if round(FColSize[i].X) = round(TfrxComponent(FGridX[FMovingObjects].Objects[0]).Left - Left - Offset.X + TfrxComponent(FGridX[FMovingObjects].Objects[0]).Width) then
            begin
              ACol := i;
              Break;
            end;
          end;
          if ACol = -1 then
          begin
            if round(FColSize[High(FColSize)].X + FColSize[High(FColSize)].Y) = round(TfrxComponent(FGridX[FMovingObjects].Objects[0]).Left - Left - Offset.X + TfrxComponent(FGridX[FMovingObjects].Objects[0]).Width) then
              ACol := High(FColSize) + 1;
          end;
          if ACol <> -1 then
          begin
            if ACol = 0 then
            begin
              if FHeadRowSize[High(FHeadRowSize)].Y + (X - FLastMousePos.X) < 20 then exit;
              FHeadRowSize[High(FHeadRowSize)].Y := FHeadRowSize[High(FHeadRowSize)].Y + (X - FLastMousePos.X);
            end
            else
            begin
              if FColSize[ACol - 1].Y + (X - FLastMousePos.X) < 20 then exit;
              FColSize[ACol - 1].Y := FColSize[ACol - 1].Y + (X - FLastMousePos.X);
            end;
            for i := ACol to High(FColSize) do
            begin
              FColSize[i].X := FColSize[i].X + (X - FLastMousePos.X);
            end;
          end
        end
        else
        begin
          if ACol > 0 then
          begin
            if FHeadRowSize[ACol - 1].Y + (X - FLastMousePos.X) < 20 then exit;
            FHeadRowSize[ACol - 1].Y := FHeadRowSize[ACol - 1].Y + (X - FLastMousePos.X);
            for i := ACol to High(FHeadRowSize) do
            begin
              FHeadRowSize[i].X := FHeadRowSize[i].X + (X - FLastMousePos.X);
            end;
            for i := 0 to High(FColSize) do
            begin
              FColSize[i].X := FColSize[i].X + (X - FLastMousePos.X);
            end;
          end;
        end;

      end;

    end
    else if FGridUsed = FGridY then
    begin

      if FGridY[FMovingObjects].Objects.Count > 0 then
      begin
        ACol := -1;
        for i := 0 to High(FHeadColSize) do
        begin
          if round(FHeadColSize[i].X ) = round(TfrxComponent(FGridY[FMovingObjects].Objects[0]).Top - Top - Offset.Y + TfrxComponent(FGridY[FMovingObjects].Objects[0]).Height) then
          begin
            ACol := i;
            Break;
          end;
        end;
        if ACol = -1 then
        begin
          for i := 0 to High(FRowSize) do
          begin
            if round(FRowSize[i].X) = round(TfrxComponent(FGridY[FMovingObjects].Objects[0]).Top - Top - Offset.Y + TfrxComponent(FGridY[FMovingObjects].Objects[0]).Height) then
            begin
              ACol := i;
              Break;
            end;
          end;
          if ACol = -1 then
          begin
            if round(FRowSize[High(FRowSize)].X + FRowSize[High(FRowSize)].Y) = round((TfrxComponent(FGridY[FMovingObjects].Objects[0]).Top - Top - Offset.Y + TfrxComponent(FGridY[FMovingObjects].Objects[0]).Height)) then
              ACol := High(FRowSize) + 1;
          end;
          if ACol <> -1 then
          begin
            if ACol = 0 then
            begin
              if FHeadColSize[High(FHeadColSize)].Y + (Y - FLastMousePos.Y) < 10 then exit;
              FHeadColSize[High(FHeadColSize)].Y := FHeadColSize[High(FHeadColSize)].Y + (Y - FLastMousePos.Y);
            end
            else
            begin
              if FRowSize[ACol - 1].Y + (Y - FLastMousePos.Y) < 10 then exit;
              FRowSize[ACol - 1].Y := FRowSize[ACol - 1].Y + (Y - FLastMousePos.Y);
            end;
            for i := ACol to High(FRowSize) do
            begin
              FRowSize[i].X := FRowSize[i].X + (Y - FLastMousePos.Y);
            end;
          end
        end
        else
        begin
          if ACol > 0 then
          begin
            if FHeadColSize[ACol - 1].Y + (Y - FLastMousePos.Y) < 10 then exit;
            FHeadColSize[ACol - 1].Y := FHeadColSize[ACol - 1].Y + (Y - FLastMousePos.Y);
            for i := ACol to High(FHeadColSize) do
            begin
              FHeadColSize[i].X := FHeadColSize[i].X + (Y - FLastMousePos.Y);
            end;
            for i := 0 to High(FRowSize) do
            begin
              FRowSize[i].X := FRowSize[i].X + (Y - FLastMousePos.Y);
            end;
          end;
        end;

      end;
    end;
    FLastMousePos := Point(X, Y);
  end;
end;

procedure TfcxpCrossView.ContainerMouseUp(Sender: TObject; X, Y: Integer);
begin
  if AutoSizeMemo then exit;
  FMouseDown := False;
  if AutoSizeMemo and ((Abs(X - FFirstMousePos.X) > 5) or (Abs(Y - FFirstMousePos.Y) > 5)) then
    frxInfoMsg(frxResources.Get('crResize'));
end;

procedure TfcxpCrossView.BeforeStartReport;
begin
  inherited;
  InitMemos(True);
end;

function TfcxpCrossView.GetDefaultColWidthInternal: integer;
begin
  if Slice <> nil then
    if PaintSizes.AutoSizeStyle <> ssBySlice then
      Result := PaintSizes.DefaultColWidth
    else
      Result := THackSlice(Slice).DefaultColWidth
  else
    Result := PaintSizes.DefaultColWidth;
end;

procedure TfcxpCrossView.ImportColorFromGrid(ASliceGrid: TfcxSliceGrid);
begin
  FGridStyles := ASliceGrid.Styles;
  FUseGridColors := True;
end;

procedure TfcxpCrossView.ClearColorFromGrid;
begin
  FUseGridColors := False;
end;

function TfcxpCrossView.GetDefaultRowHeightInternal: integer;
begin
  if Slice <> nil then
    if PaintSizes.AutoSizeStyle <> ssBySlice then
      Result := PaintSizes.DefaultRowHeight
    else
      Result := THackSlice(Slice).DefaultRowHeight
  else
    Result := PaintSizes.DefaultRowHeight;
end;

function TfcxpCrossView.NamesHeight: Extended;
begin
  if ShowNames and ShowColumnHeader then
    Result := FCrossHeadColHeight[0]
  else
    Result := 0;
end;

function TfcxpCrossView.GetColWidth(ACol: Integer): integer;
var
  AMeasureIndex, ATotX: Integer;
begin
  Result := PaintSizes.DefaultColWidth;
  case PaintSizes.AutoSizeStyle of
    ssBySlice:
      if Slice <> nil then
        Result := Slice.ColWidth[ACol];
    ssByMemoSize:
      begin
// RunTime version
// Find memo for ACol
        AMeasureIndex := Slice.MeasuresContainer.VisibleIndex[Slice.XAxisContainer.VisibleAxisNodes.MeasureIndex[ACol]];
        ATotX := ColumnLevels - 1 - Slice.XAxisContainer.VisibleAxisNodes.LevelOf[ACol];
        if FMeasurePlace = 0 then
        begin
          if ATotX > (ColumnLevels - FMeasureIndex - 1) then
            dec(ATotX);
          Result := Round(CellMemos[ATotX * CellLevels + AMeasureIndex].Width);
        end
        else
        begin
          Result := Round(CellMemos[ATotX].Width);
        end;
      end;
    ssAutoColWidth, ssAutoColWidthRestrict, ssAutoRowHeight:
      begin
        Result := Round(FCrossColWidth[ACol]);
      end;
  end;
end;

function TfcxpCrossView.GetRowHeight(ARow: Integer): integer;
var
  AMeasureIndex, ATotY: Integer;
begin
  Result := PaintSizes.DefaultRowHeight;
  case PaintSizes.AutoSizeStyle of
    ssBySlice:
      if Slice <> nil then
        Result := Slice.RowHeight[ARow];
    ssByMemoSize:
      begin
// RunTime version
// Find memo for ARow
        AMeasureIndex := Slice.MeasuresContainer.VisibleIndex[Slice.YAxisContainer.VisibleAxisNodes.MeasureIndex[ARow]];
        ATotY := RowLevels - 1 - Slice.YAxisContainer.VisibleAxisNodes.LevelOf[ARow];

        if FMeasurePlace = 0 then
        begin
          Result := Round(CellMemos[ATotY * CellLevels * ColumnLevels].Height);
        end
        else
        if FMeasurePlace = 1 then
        begin
          if ATotY > (RowLevels - FMeasureIndex - 1) then
            dec(ATotY);
          Result := Round(CellMemos[(ATotY * CellLevels + AMeasureIndex)* (ColumnLevels + 1)].Height);
        end
        else
          Result := Round(CellMemos[ATotY * CellLevels * ColumnLevels].Height);
      end;
    ssAutoColWidth, ssAutoColWidthRestrict, ssAutoRowHeight:
      begin
        Result := Round(FCrossRowHeight[ARow]);
      end;
  end;
end;

procedure TfcxpCrossView.SetColWidth(ACol: Integer; const Value: integer);
begin

end;

procedure TfcxpCrossView.SetRowHeight(ARow: Integer;
  const Value: integer);
begin

end;

procedure TfcxpCrossView.SetMemosBounds;
var
  i: Integer;
  m: TfrxCustomMemoView;
  NCol, INCol, INRow : integer;
  AYOffset2, AXOffset, AYOffset : Extended;
begin
  if ShowColumnHeader and ShowNames then
    AYOffset2 := FHeadColSize[0].Y
  else
    AYOffset2 := 0;
  if ShowColumnHeader then
    AYOffset := FHeadColSize[High(FHeadColSize)].Y + FHeadColSize[High(FHeadColSize)].X
  else
    AYOffset := 0;
  if Length(FHeadRowSize) > 0 then
    if ShowRowHeader then
      AXOffset := FHeadRowSize[High(FHeadRowSize)].Y + FHeadRowSize[High(FHeadRowSize)].X
    else
      AXOffset := 0
  else
    AXOffset := 0;
  for i := 0 to FCellHeaderMemos.Count - 1 do
  begin
    m := CellHeaderMemos[i];
// HeadCells in last row(column) of axis
    if FMeasurePlace = 0 then
    begin
      m.SetBounds(FColSize[i].X,
                  FHeadColSize[High(FHeadColSize)].X,
                  FColSize[i].Y,
                  FHeadColSize[High(FHeadColSize)].Y)
    end
    else
    begin
      m.SetBounds(FHeadRowSize[High(FHeadRowSize)].X,
                  FRowSize[i].X,
                  FHeadRowSize[High(FHeadRowSize)].Y,
                  FRowSize[i].Y)
    end;
  end;

  if FMeasurePlace = 0 then
    NCol := ColumnLevels * CellLevels
  else
    NCol := ColumnLevels + 1;

  for i := 0 to FCellMemos.Count - 1 do
  begin
    m := CellMemos[i];
    INCol := i mod NCol;
    INRow := i div NCol;
    m.SetBounds(FColSize[INCol].X,
                FRowSize[INRow].X,
                FColSize[INCol].y,
                FRowSize[INRow].y);
  end;

  for i := 0 to FColumnMemos.Count - 1 do
  begin

    m := ColumnMemos[i];
    if FMeasurePlace = 0 then
    begin
      m.SetBounds(AXOffset,
                  FHeadColSize[i + 1].X,
                  FColSize[(FColumnMemos.Count - i) * CellLevels].X - AXOffset,
                  FHeadColSize[i + 1].Y);
    end
    else
    begin
      m.SetBounds(AXOffset,
                  FHeadColSize[i + 1].X,
                  FColSize[(FColumnMemos.Count - i)].X - AXOffset,
                  FHeadColSize[i + 1].Y);
    end;
    m := ColumnTotalMemos[i];
    if FMeasurePlace = 0 then
    begin
      m.SetBounds(ColumnMemos[i].Left + ColumnMemos[i].Width,
                  ColumnMemos[i].Top,
                  FColSize[(FColumnMemos.Count - i) * CellLevels + CellLevels - 1].X  + FColSize[(FColumnMemos.Count - i) * CellLevels + CellLevels - 1].Y - (ColumnMemos[i].Left + ColumnMemos[i].Width),
                  FHeadColSize[High(FHeadColSize)].X - ColumnMemos[i].Top);
    end
    else
    begin
      m.SetBounds(ColumnMemos[i].Left + ColumnMemos[i].Width,
                  ColumnMemos[i].Top,
                  FColSize[(FColumnMemos.Count - i)].Y,
                  FHeadColSize[High(FHeadColSize)].X + FHeadColSize[High(FHeadColSize)].Y - ColumnMemos[i].Top);
    end;
  end;

  for i := 0 to FRowMemos.Count - 1 do
  begin
    m := RowMemos[i];
    if FMeasurePlace = 1 then
    begin
      m.SetBounds(FHeadRowSize[i].X,
                  AYOffset,
                  FHeadRowSize[i].Y,
                  FRowSize[(FRowMemos.Count - i) * CellLevels].X - AYOffset);
    end
    else
    begin
      m.SetBounds(FHeadRowSize[i].X,
                  AYOffset,
                  FHeadRowSize[i].Y,
                  FRowSize[(FRowMemos.Count - i)].X - AYOffset);
    end;
    m := RowTotalMemos[i];

    if FMeasurePlace = 1 then
    begin
      m.SetBounds(RowMemos[i].Left,
                  RowMemos[i].Top + RowMemos[i].Height,
                  FHeadRowSize[High(FHeadRowSize)].X - RowMemos[i].Left,
                  FRowSize[(FRowMemos.Count - i) * CellLevels + CellLevels - 1].X  + FRowSize[(FRowMemos.Count - i) * CellLevels + CellLevels - 1].Y - (RowMemos[i].Top + RowMemos[i].Height));
    end
    else
    begin
      m.SetBounds(RowMemos[i].Left,
                  RowMemos[i].Top + RowMemos[i].Height,
                  FHeadRowSize[High(FHeadRowSize)].X + FHeadRowSize[High(FHeadRowSize)].Y - RowMemos[i].Left,
                  FRowSize[(FRowMemos.Count - i)].Y);
    end;
  end;

  if ShowColumnHeader and ShowRowHeader then
  begin
    m := CornerMemos[0];
    m.SetBounds(0, 0, AXOffset, AYOffset2); // need Height of Real Row
  end;

  if ShowColumnHeader and ShowNames and (Length(FColSize) > 0) then
  begin
    m := CornerMemos[1];
    m.SetBounds(AXOffset, 0, FColSize[High(FColSize)].X + FColSize[High(FColSize)].Y - AXOffset, FHeadColSize[0].Y);
  end;

  if ShowRowHeader and ShowColumnHeader and ShowNames then
  begin
    m := CornerMemos[2];
    m.SetBounds(0, CornerMemos[0].Top + CornerMemos[0].Height, AXOffset, AYOffset - CornerMemos[0].Top - CornerMemos[0].Height);
  end;
end;

procedure TfcxpCrossView.SetMemosPos;
var
  i: Integer;
  AYOffset2, AXOffset, AYOffset : Extended;
begin
  if (FHeadColSize[0].Y = 0) or AutoSizeMemo then
    FHeadColSize[0].Y := PaintSizes.DefaultRowHeight;
  FHeadColSize[0].X := 0;

  if ShowColumnHeader and ShowNames then
    AYOffset2 := FHeadColSize[0].Y
  else
    AYOffset2 := 0;

  if ShowColumnHeader then
  begin
    AYOffset := AYOffset2;
    for i := 1 to High(FHeadColSize) do
    begin
      if (FHeadColSize[i].Y = 0) or AutoSizeMemo then
        FHeadColSize[i].Y := PaintSizes.DefaultRowHeight;
      FHeadColSize[i].X := AYOffset;
      AYOffset := AYOffset + FHeadColSize[i].Y;
    end;
  end
  else
  begin
    AYOffset := 0;
    for i := 1 to High(FHeadColSize) do
    begin
      if (FHeadColSize[i].Y = 0) or AutoSizeMemo then
        FHeadColSize[i].Y := PaintSizes.DefaultRowHeight;
      FHeadColSize[i].X := AYOffset;
    end;
  end;
  AXOffset := 0;
  if ShowRowHeader then
  begin
    for i := 0 to High(FHeadRowSize) do
    begin
      if (FHeadRowSize[i].Y = 0) or AutoSizeMemo then
        FHeadRowSize[i].Y := PaintSizes.DefaultColWidth;
      FHeadRowSize[i].X := AXOffset;
      AXOffset := AXOffset + FHeadRowSize[i].Y;
    end;
  end
  else
  begin
    for i := 1 to High(FHeadRowSize) do
    begin
      if (FHeadRowSize[i].Y = 0) or AutoSizeMemo then
        FHeadRowSize[i].Y := PaintSizes.DefaultColWidth;
      FHeadRowSize[i].X := AXOffset;
    end;
  end;

  for i := 0 to High(FColSize) do
  begin
    FColSize[i].X := AXOffset;
    if (FColSize[i].Y = 0) or AutoSizeMemo then
      FColSize[i].Y := PaintSizes.DefaultColWidth;
    AXOffset := AXOffset + FColSize[i].Y;
  end;

  for i := 0 to High(FRowSize) do
  begin
    FRowSize[i].X := AYOffset;
    if (FRowSize[i].Y = 0) or AutoSizeMemo then
      FRowSize[i].Y := PaintSizes.DefaultRowHeight;
    AYOffset := AYOffset + FRowSize[i].Y;
  end;
end;

procedure TfcxpCrossView.SetShowColumnHeader(const Value: Boolean);
begin
  FShowColumnHeader := Value;
  SetMemosPos;
end;

procedure TfcxpCrossView.SetShowNames(const Value: Boolean);
begin
  FShowNames := Value;
  SetMemosPos;
end;

procedure TfcxpCrossView.SetShowRowHeader(const Value: Boolean);
begin
  FShowRowHeader := Value;
  SetMemosPos;
end;

procedure TfcxpCrossView.DoCalcSizes;
var
  i, j: Integer;
  AMeasureIndex, ATotX, ATotY, AColLevels, ARowLevels, ACellLevels: Integer;
  m: TfrxCustomMemoView;
  AMemoRec: TfrxPoint;
  MeasureCell: TfcxMeasureCell;
begin
// create
  ReallocMem(FCrossHeadColHeight, (ColumnLevels + 1) * SizeOf(_Extended_cell));
  ReallocMem(FCrossHeadRowWidth, RowLevels * SizeOf(_Extended_cell));
  ReallocMem(FCrossTraversRowBottom, RowLevels * SizeOf(_Extended_cell));
  ReallocMem(FCrossColWidth, ColCount * SizeOf(_Extended_cell));
  ReallocMem(FCrossRowHeight, RowCount * SizeOf(_Extended_cell));
// Set Default
  case PaintSizes.AutoSizeStyle of
    ssAutoColWidth, ssAutoColWidthRestrict:
      begin
        for i := 0 to ColumnLevels do
          FCrossHeadColHeight[i] := -1;
        for i := 0 to RowLevels - 1 do
        begin
          FCrossHeadRowWidth[i] := -1;
          FCrossTraversRowBottom[i] := 0;
        end;
        for i := 0 to ColCount - 1 do
          FCrossColWidth[i] := -1;
        for i := 0 to RowCount - 1 do
          FCrossRowHeight[i] := -1;
      end;
    ssAutoRowHeight:
      begin
        for i := 0 to ColumnLevels do
          FCrossHeadColHeight[i] := -1;
        for i := 0 to RowLevels - 1 do
        begin
          FCrossHeadRowWidth[i] := DefaultColWidthInternal;
          FCrossTraversRowBottom[i] := 0;
        end;
        for i := 0 to ColCount - 1 do
          FCrossColWidth[i] := DefaultColWidthInternal;
        for i := 0 to RowCount - 1 do
          FCrossRowHeight[i] := -1;
      end;
  else
    for i := 0 to ColumnLevels do
      FCrossHeadColHeight[i] := DefaultRowHeightInternal;
    for i := 0 to RowLevels - 1 do
    begin
      FCrossHeadRowWidth[i] := DefaultColWidthInternal;
      FCrossTraversRowBottom[i] := 0;
    end;
    for i := 0 to ColCount - 1 do
      FCrossColWidth[i] := DefaultColWidthInternal;
    for i := 0 to RowCount - 1 do
      FCrossRowHeight[i] := DefaultRowHeightInternal;
  end;

  if PaintSizes.AutoSizeStyle in [ssAutoColWidth, ssAutoColWidthRestrict, ssAutoRowHeight] then
  begin
    FCalcMemo.Assign(CornerMemos[1]);
    FCalcMemo.Font.Assign(CornerMemos[1].Font);
    FCalcMemo.Text := CornerMemos[1].Text;
    AMemoRec := CalcHeight(FCalcMemo, False);
    if AMemoRec.Y > FCrossHeadColHeight[0] then
      FCrossHeadColHeight[0] := AMemoRec.Y;
    if FCrossHeadColHeight[0] < DefaultRowHeightInternal then
      FCrossHeadColHeight[0] := DefaultRowHeightInternal;
  end;

  if PaintSizes.AutoSizeStyle = ssBySlice then
  begin
    FMinYNamesHeight := DefaultRowHeightInternal;
    FCalcMemo.Assign(CornerMemos[2]);
    FCalcMemo.Font.Assign(CornerMemos[2].Font);
    FCalcMemo.Text := CornerMemos[2].Text;
    FCalcMemo.Width := RowHeaderWidth;
    AMemoRec := CalcHeight(FCalcMemo, True);
    if AMemoRec.Y > FMinYNamesHeight then
      FMinYNamesHeight := AMemoRec.Y;
  end;

  // use traverse for calc sizes
  if PaintSizes.AutoSizeStyle in [ssAutoColWidth, ssAutoColWidthRestrict, ssAutoRowHeight] then
  begin
    Slice.XAxisContainer.TraverseAxis(0, Slice.XAxisContainer.VisibleLevelCount - 1, 0, XAxisDrawProcCalc);
    Slice.YAxisContainer.TraverseAxis(0, Slice.YAxisContainer.VisibleLevelCount - 1, 0, YAxisDrawProcCalc);
    if Slice.MeasuresContainer.InAxis then
    begin
      // use this variables to speedup slice access a bit
      AColLevels := ColumnLevels;
      ARowLevels := RowLevels;
      ACellLevels := CellLevels;
      for i := 0 to ColCount - 1 do
      begin
        ATotX := AColLevels - 1 - Slice.XAxisContainer.VisibleAxisNodes.LevelOf[i];
        for j := 0 to RowCount - 1 do
        begin
          ATotY := ARowLevels - 1 - Slice.YAxisContainer.VisibleAxisNodes.LevelOf[j];

          if FMeasurePlace = 0 then
          begin
            if ATotX > (AColLevels - FMeasureIndex - 1) then
              dec(ATotX);
            AMeasureIndex := Slice.MeasuresContainer.VisibleIndex[Slice.XAxisContainer.VisibleAxisNodes.MeasureIndex[i]];
            m := CellMemos[ATotY * ACellLevels * AColLevels + ATotX * ACellLevels + AMeasureIndex];
          end
          else
          if FMeasurePlace = 1 then
          begin
            if ATotY > (ARowLevels - FMeasureIndex - 1) then
              dec(ATotY);
            AMeasureIndex := Slice.MeasuresContainer.VisibleIndex[Slice.YAxisContainer.VisibleAxisNodes.MeasureIndex[j]];
            m := CellMemos[(ATotY * ACellLevels + AMeasureIndex) * (AColLevels + 1) + ATotX];
          end
          else
            m := nil;

          Slice.GetMeasureCell(i, j, MeasureCell);

          FCalcMemo.Assign(m);
          if FUseGridColors then
          begin
            ApplyCellStyle(MeasureCell, FCalcStyle);
            FCalcMemo.Font.Assign(FCalcStyle.Font);
          end
          else
            FCalcMemo.Font.Assign(m.Font);
          // Format of cell value from grid
          FCalcMemo.Text := MeasureCell.StrValue;
          case PaintSizes.AutoSizeStyle of
            ssAutoColWidth:
              begin
                AMemoRec := CalcHeight(FCalcMemo, False);
                AMemoRec := CalcWidthWithHeigth(FCalcMemo, AMemoRec.Y);
                if FCrossColWidth[i] < AMemoRec.X then
                  FCrossColWidth[i] := AMemoRec.X;
                if FCrossRowHeight[j] < AMemoRec.Y then
                  FCrossRowHeight[j] := AMemoRec.Y;
              end;
            ssAutoColWidthRestrict:
              begin
                AMemoRec := CalcHeight(FCalcMemo, False);
                AMemoRec := CalcWidthWithHeigth(FCalcMemo, AMemoRec.Y);
                if AMemoRec.X > PaintSizes.MaxColWidth then
                  AMemoRec.X := PaintSizes.MaxColWidth;
                AMemoRec := CalcHeightWithWidth(FCalcMemo, AMemoRec.X);
                if FCrossColWidth[i] < AMemoRec.X then
                  FCrossColWidth[i] := AMemoRec.X;
                if FCrossRowHeight[j] < AMemoRec.Y then
                  FCrossRowHeight[j] := AMemoRec.Y;
              end;
            ssAutoRowHeight:
              begin
                AMemoRec := CalcHeightWithWidth(FCalcMemo, FCrossColWidth[i]);
                if FCrossRowHeight[j] < AMemoRec.Y then
                  FCrossRowHeight[j] := AMemoRec.Y;
              end;
          end;
        end;
      end;
    end;
  end;

  if PaintSizes.AutoSizeStyle = ssByMemoSize then
  begin
    for i := 0 to ColCount - 1 do
      FCrossColWidth[i]  := ColWidth[i];
    for i := 0 to RowCount - 1 do
      FCrossRowHeight[i] := RowHeight[i];
    FCrossHeadColHeight[0] := CornerMemos[1].Height;
    for i := 0 to ColumnLevels - 1 do
    begin
      if FMeasurePlace = 0 then
      begin
        if FMeasureIndex = i then
          FCrossHeadColHeight[i + 1] := CellHeaderMemos[0].Height
        else
        if FMeasureIndex < i then
          FCrossHeadColHeight[i + 1] := ColumnMemos[i - 1].Height
        else
          FCrossHeadColHeight[i + 1] := ColumnMemos[i].Height;
      end
      else
        FCrossHeadColHeight[i + 1] := ColumnMemos[i].Height;
    end;
    for i := 0 to RowLevels - 1 do
    begin
      if FMeasurePlace = 1 then
      begin
        if FMeasureIndex = i then
          FCrossHeadRowWidth[i] := CellHeaderMemos[0].Width
        else
        if FMeasureIndex < i then
          FCrossHeadRowWidth[i] := RowMemos[i - 1].Width
        else
          FCrossHeadRowWidth[i] := RowMemos[i].Width;
      end
      else
      begin
        FCrossHeadRowWidth[i] := RowMemos[i].Width;
      end;
    end;
  end;
end;

function TfcxpCrossView.XAxisDrawProcCalc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;

  procedure ApplyColWidth(FirstCol, ColCount: Integer; const AWidth: Extended);
  var
    TotalWidth, DW: Extended;
    I: Integer;
  begin
    if ColCount = 1 then
    begin
      if FCrossColWidth[FirstCol] < AWidth then
        FCrossColWidth[FirstCol] := AWidth;
    end
    else
    begin
      // split height between rows
      TotalWidth := 0;
      for I := FirstCol to FirstCol + ColCount - 1 do
        TotalWidth := TotalWidth + FCrossColWidth[I];
      if AWidth > TotalWidth then
      begin
        DW := (AWidth - TotalWidth) / ColCount;
        for I := FirstCol to FirstCol + ColCount - 1 do
          FCrossColWidth[I] := FCrossColWidth[I] + DW;
      end;
    end;
  end;

var
  m: TfrxCustomMemoView;
  I, ATotX: Integer;
  AMemoRec: TfrxPoint;
  AIsTotal, AIsMeasure: Boolean;
  AWidth: Extended;
begin
  // this is event from slice. We start the draw loop. Here we create memos
  AIsTotal := (ARec.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [];
  AIsMeasure := (FMeasurePlace = 0) and (ARec.TreeRect.Level = FMeasureIndex);
  if FMeasurePlace = 0 then
  begin
    ATotX := ColumnLevels - ARec.TreeRect.Level - 1;
    if AIsMeasure then // Measure in this axis
      m := CellHeaderMemos[Slice.MeasuresContainer.VisibleIndex[ARec.MeasureIndex] + CellLevels * ATotX]
    else
    if AIsTotal then // Total or Collapsed
      m := ColumnTotalMemos[ARec.Level]
    else
      m := ColumnMemos[ARec.Level];
  end
  else
  begin
    if CornerMemos[1].Text = '' then
      m := nil
    else
    if AIsTotal then // Total or Collapsed
      m := ColumnTotalMemos[ARec.Level]
    else
      m := ColumnMemos[ARec.Level];
  end;
  if Assigned(m) then
  begin
    FCalcMemo.Assign(m);
    FCalcMemo.Font.Assign(m.Font);
    FCalcMemo.Text := ARec.Text;
  end;
  case PaintSizes.AutoSizeStyle of
    ssAutoColWidth:
      begin
        // calculate size as usual
        AMemoRec := CalcHeight(FCalcMemo, False);
        AMemoRec := CalcWidthWithHeigth(FCalcMemo, AMemoRec.Y);

        // save sizes
        if FCrossHeadColHeight[ARec.TreeRect.Level + 1] < AMemoRec.Y then
          FCrossHeadColHeight[ARec.TreeRect.Level + 1] := AMemoRec.Y;

        ApplyColWidth(ARec.TreeRect.Cell, ARec.TreeRect.SizeCell, AMemoRec.X);  
      end;
    ssAutoColWidthRestrict:
      begin
        // calculate size as usual
        AMemoRec := CalcHeight(FCalcMemo, False);
        AMemoRec := CalcWidthWithHeigth(FCalcMemo, AMemoRec.Y);

        // get the maximum width for this cell
        AWidth := FPaintSizes.MaxColWidth;
        for I := ARec.TreeRect.Cell + 1 to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
          AWidth := AWidth + GetXAxisColWidth(I);

        // recalculate size with fixing the width
        if AMemoRec.X > AWidth then
          AMemoRec.X := AWidth;
        AMemoRec := CalcHeightWithWidth(FCalcMemo, AMemoRec.X);

        // save sizes
        if FCrossHeadColHeight[ARec.TreeRect.Level + 1] < AMemoRec.Y then
          FCrossHeadColHeight[ARec.TreeRect.Level + 1] := AMemoRec.Y;

        ApplyColWidth(ARec.TreeRect.Cell, ARec.TreeRect.SizeCell, AMemoRec.X);  
      end;
    ssAutoRowHeight:
      begin
        // get current cell width 
        AWidth := 0;
        for I := ARec.TreeRect.Cell to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
          AWidth := AWidth + GetXAxisColWidth(I);

        // save size with fixing the width
        AMemoRec := CalcHeightWithWidth(FCalcMemo, AWidth);
        if FCrossHeadColHeight[ARec.TreeRect.Level + 1] < AMemoRec.Y then
          FCrossHeadColHeight[ARec.TreeRect.Level + 1] := AMemoRec.Y;
      end;
  end;
  Result := False;
end;

function TfcxpCrossView.YAxisDrawProcCalc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;

  procedure ApplyRowHeight(FirstRow, RowCount: Integer; const AHeight: Extended);
  var
    TotalHeight, DH: Extended;
    I: Integer;
  begin
    if RowCount = 1 then
    begin
      if FCrossRowHeight[FirstRow] < AHeight then
        FCrossRowHeight[FirstRow] := AHeight;
    end
    else
    begin
      // split height between rows
      TotalHeight := 0;
      for I := FirstRow to FirstRow + RowCount - 1 do
        TotalHeight := TotalHeight + FCrossRowHeight[I];
      if AHeight > TotalHeight then
      begin
        DH := (AHeight - TotalHeight) / RowCount;
        for I := FirstRow to FirstRow + RowCount - 1 do
          FCrossRowHeight[I] := FCrossRowHeight[I] + DH;
      end;
    end;
  end;

var
  m: TfrxCustomMemoView;
  I, ATotY: Integer;
  AMemoRec: TfrxPoint;
  AIsTotal, AIsMeasure: Boolean;
  AWidth: Extended;
begin
// this is event from slice. We start the draw loop. Here we create memos
  AIsTotal := (ARec.CellProperties * [pca_GrandTotal, pca_StartTotal]) <> [];
  AIsMeasure := (FMeasurePlace = 1) and (ARec.TreeRect.Level = FMeasureIndex);
  if FMeasurePlace = 1 then
  begin
    ATotY := RowLevels - ARec.TreeRect.Level - 1;
    if AIsMeasure then
      m := CellHeaderMemos[Slice.MeasuresContainer.VisibleIndex[ARec.MeasureIndex] + CellLevels * ATotY]
    else
    if AIsTotal then // Total or Collapsed
      m := RowTotalMemos[ARec.Level]
    else
      m := RowMemos[ARec.Level];
  end
  else
  begin
    if CornerMemos[2].Text = '' then
      m := nil // ??
    else
    if AIsTotal then // Total or Collapsed
      m := RowTotalMemos[ARec.Level]
    else
      m := RowMemos[ARec.Level];
  end;
  if Assigned(m) then
  begin
    FCalcMemo.Assign(m);
    FCalcMemo.Font.Assign(m.Font);
    FCalcMemo.Text := ARec.Text;
  end;
  case PaintSizes.AutoSizeStyle of
    ssAutoColWidth:
      begin
        // calculate sizes as usual
        AMemoRec := CalcHeight(FCalcMemo, False);
        AMemoRec := CalcWidthWithHeigth(FCalcMemo, AMemoRec.Y);

        // save sizes
        ApplyRowHeight(ARec.TreeRect.Cell, ARec.TreeRect.SizeCell, AMemoRec.Y);
        if FCrossHeadRowWidth[ARec.TreeRect.Level] < AMemoRec.X then
          FCrossHeadRowWidth[ARec.TreeRect.Level] := AMemoRec.X
      end;
    ssAutoColWidthRestrict:
      begin
        // calculate sizes as usual
        AMemoRec := CalcHeight(FCalcMemo, False);
        AMemoRec := CalcWidthWithHeigth(FCalcMemo, AMemoRec.Y);

        // get the maximum width for this cell
        AWidth := FPaintSizes.MaxColWidth;
        for I := ARec.TreeRect.Level + 1 to ARec.TreeRect.Level + ARec.TreeRect.SizeLevel - 1 do
          AWidth := AWidth + GetYAxisColWidth(I);

        // recalculate size with fixing the width
        if AMemoRec.X > AWidth then
          AMemoRec.X := AWidth;
        AMemoRec := CalcHeightWithWidth(FCalcMemo, AMemoRec.X);

        // save sizes
        ApplyRowHeight(ARec.TreeRect.Cell, ARec.TreeRect.SizeCell, AMemoRec.Y);
        if FCrossHeadRowWidth[ARec.TreeRect.Level] < AMemoRec.X then
          FCrossHeadRowWidth[ARec.TreeRect.Level] := AMemoRec.X
      end;
    ssAutoRowHeight:
      begin
        // calculate width for this cell
        AWidth := 0;
        case Sender.AxisType of
          at_Standard:
            begin
              for I := ARec.TreeRect.Level to ARec.TreeRect.Level + ARec.TreeRect.SizeLevel - 1 do
                AWidth := AWidth + GetYAxisColWidth(I);
            end;
          at_Tree:
            begin
              if pca_TreeCellWithMeasure in ARec.CellProperties then
                if Sender.MeasuresLevel > -1 then
                  if ARec.TreeRect.Level <= Sender.MeasuresLevel then
                    AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing
                  else
                    AWidth := GetYAxisLevelSize + (Sender.LevelCount - ARec.TreeRect.Level) * YAxisLevelSpacing
                else
                  AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing
              else
                if Sender.MeasuresLevel > -1 then
                  if ARec.TreeRect.Level < Sender.MeasuresLevel then
                    AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing + GetYAxisMeasureSize
                  else
                    AWidth := GetYAxisLevelSize + (Sender.LevelCount - ARec.TreeRect.Level) * YAxisLevelSpacing + GetYAxisMeasureSize
                else
                  AWidth := GetYAxisLevelSize + (Sender.LevelCount - 1 - ARec.TreeRect.Level) * YAxisLevelSpacing;
            end;
        end;
        // save size with fixing the width
        AMemoRec := CalcHeightWithWidth(FCalcMemo, AWidth);
        ApplyRowHeight(ARec.TreeRect.Cell, ARec.TreeRect.SizeCell, AMemoRec.Y);
      end;
  end;
  Result := False;
end;

function TfcxpCrossView.GetXAxisColWidth(ACol: Integer): Integer;
begin
  case PaintSizes.AutoSizeStyle of
    ssDefault:
      Result := PaintSizes.DefaultColWidth;
    ssBySlice:
      Result := Slice.ColWidth[ACol];
  else
    Result := Round(FCrossColWidth[ACol]);
  end;
end;

function TfcxpCrossView.GetXAxisRowHeight(ARow: Integer): Integer;
begin
  case PaintSizes.AutoSizeStyle of
    ssDefault:
      Result := PaintSizes.DefaultRowHeight;
    ssBySlice:
      if ARow = (Slice.XAxisContainer.VisibleLevelCount - 1) then
      begin
        Result := Slice.XAxisContainer.VisibleLevelSize[ARow];
        if (ColumnHeaderHeightWOLast + Result - NamesHeight) < FMinYNamesHeight then
          Result := trunc(FMinYNamesHeight + NamesHeight - ColumnHeaderHeightWOLast);
      end
      else
        Result := Slice.XAxisContainer.VisibleLevelSize[ARow];
  else
    Result := Round(FCrossHeadColHeight[ARow + 1])
  end;
end;

function TfcxpCrossView.GetYAxisColWidth(ACol: Integer): Integer;
begin
  case PaintSizes.AutoSizeStyle of
    ssDefault:
      Result := PaintSizes.DefaultColWidth;
    ssBySlice:
      Result := Slice.YAxisContainer.VisibleLevelSize[ACol];
  else
    Result := Round(FCrossHeadRowWidth[ACol]);
  end;
end;

function TfcxpCrossView.GetYAxisLevelSize: Integer;
begin
  case PaintSizes.AutoSizeStyle of
    ssDefault:
      Result := PaintSizes.DefaultColWidth;
    ssBySlice:
      if Assigned(FSliceGridProvider) and Assigned(FSliceGridProvider.SliceGrid) then
        Result := TfcxSliceGrid(FSliceGridProvider.SliceGrid).YAxisZone.Cells.TreeLevelSize
      else
        Result := PaintSizes.DefaultColWidth;
  else
    Result := Round(FCrossHeadRowWidth[0]);
  end;
end;

function TfcxpCrossView.GetYAxisMeasureSize: Integer;
begin
  case PaintSizes.AutoSizeStyle of
    ssDefault:
      Result := PaintSizes.DefaultColWidth;
    ssBySlice:
      if Assigned(FSliceGridProvider) and Assigned(FSliceGridProvider.SliceGrid) then
        Result := TfcxSliceGrid(FSliceGridProvider.SliceGrid).YAxisZone.Cells.TreeMeasureSize
      else
        Result := PaintSizes.DefaultColWidth;
  else
    Result := Round(FCrossHeadRowWidth[0]);
  end;
end;

function TfcxpCrossView.GetYAxisRowHeight(ARow: Integer): Integer;
begin
  case PaintSizes.AutoSizeStyle of
    ssDefault:
      Result := PaintSizes.DefaultRowHeight;
    ssBySlice:
      Result := Slice.RowHeight[ARow];
  else
    Result := Round(FCrossRowHeight[ARow]);
  end;
end;

procedure TfcxpCrossView.Loaded;
begin
  inherited;
  if (FLoadSchema <> nil) and (FCube <> nil) and (FCube.Active) then
  begin
    FSlice.LoadFromStream(FLoadSchema);
    FreeAndNil(FLoadSchema);

    if FLoadMemos <> nil then
    begin
      ReadMemos(FLoadMemos);
      FreeAndNil(FLoadMemos);
    end;

    if FLoadSizes <> nil then
    begin
      ReadSizes(FLoadSizes);
      FreeAndNil(FLoadSizes);
    end;
  end;
end;

function TfcxpCrossView.GetSliceGridProviderName: String;
begin
  if FSliceGridProvider = nil then
    Result := FSliceGridProviderName else
    Result := FSliceGridProvider.UserName;
end;

procedure TfcxpCrossView.SetSliceGridProvider(const Value: TfcxpSliceGridProvider);
begin
  FSliceGridProvider := Value;
  if FSliceGridProvider = nil then
    FSliceGridProviderName := ''
  else
  begin
    Cube := nil;
    FSliceGridProviderName := FSliceGridProvider.UserName;
  end
end;

procedure TfcxpCrossView.SetSliceGridProviderName(const Value: String);
begin
  FSliceGridProviderName := Value;
  FSliceGridProvider := fcxpFindSliceGridProvider(FSliceGridProviderName);
  if FSliceGridProvider <> nil then
    Cube := nil;
end;

procedure TfcxpCrossView.SetNextCross(const Value: TfcxpCrossView);
begin
  FNextCross := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TfcxpCrossView.Update;
var
  FTempLoadSchema: TMemoryStream;
  FTempLoadMemos: TMemoryStream;
  FTempLoadSizes: TMemoryStream;
begin
  if Assigned(Cube) then
  begin
    FTempLoadSchema := TMemoryStream.Create;
    WriteSchema(FTempLoadSchema);
    FTempLoadSchema.Position := 0;
    FTempLoadMemos := TMemoryStream.Create;
    WriteMemos(FTempLoadMemos);
    FTempLoadMemos.Position := 0;
    FTempLoadSizes := TMemoryStream.Create;
    WriteSizes(FTempLoadSizes);
    FTempLoadSizes.Position := 0;
    Cube.Active := False;
    Cube.Active := True;
    FSlice.LoadFromStream(FTempLoadSchema);
    ReadMemos(FTempLoadMemos);
    ReadSizes(FTempLoadSizes);
    FTempLoadSchema.Free;
    FTempLoadMemos.Free;
    FTempLoadSizes.Free;
  end
end;

function TfcxpCrossView.ColumnHeaderHeightWOLast: Extended;
var
  i: integer;
begin
  if ShowColumnHeader then
  begin
    Result := NamesHeight;
    for i := 0 to FSlice.XAxisContainer.VisibleLevelCount - 2 do
      Result := Result + GetXAxisRowHeight(i);
  end
  else
    Result := 0;
end;

class function TfcxpCrossView.GetDescription: String;
begin
  Result := fcxResources.Get('obfrcCrossView');
end;

procedure TfcxpCrossView.ApplyMemoStyle(AMemo: TfrxCustomMemoView; Style: TfcxCustomThemeStyle);
begin
  AMemo.Color := Style.FillColor;
  AMemo.Font.Assign(Style.Font);
  AMemo.Font.Color := Style.TextColor;
  if AMemo is TfcxpMemoView then
  begin
    TfcxpMemoView(AMemo).GradientDirection := Style.GradientDirection;
    TfcxpMemoView(AMemo).GradientColor := Style.GradientColor;
  end;
end;

{ TfcxpMemoView }

procedure TfcxpMemoView.AddHighlight(AHighlight: TfcxCustomHighlight);
begin
  if not Assigned(FHighlights) then
    FHighlights := TList.Create;
  FHighlights.Add(AHighlight)
end;

constructor TfcxpMemoView.Create(AOwner: TComponent);
begin
  inherited;
  FHighlights := nil;
end;

destructor TfcxpMemoView.Destroy;
begin
  FHighlights.Free;
  inherited;
end;

function TfcxpMemoView.Diff(AComponent: TfrxComponent): String;
var
  v: TfcxpMemoView;
begin
  Result := inherited Diff(AComponent);
  v := TfcxpMemoView(AComponent);

  if FGradientDirection <> v.FGradientDirection then
    Result := Result + ' GradientDirection="' + frxValueToXML(FGradientDirection) + '"';
  if FGradientColor <> v.FGradientColor then
    Result := Result + ' GradientColor="' + IntToStr(FGradientColor) + '"';
  if FHighlights <> v.FHighlights then
    Result := Result + ' Highlights="' + frxStrToXML(Highlights) + '"';
end;

procedure TfcxpMemoView.DrawBackground;
var
  I: Integer;
  R: TRect;
  CanDrawImage, CanDrawText: Boolean;
begin
  R := Rect(FX, FY, FX1, FY1);
  if GradientDirection = tgdNone then
    inherited DrawBackGround
  else
    FillGradient(FCanvas, R, ColorToRGB(Color), ColorToRGB(GradientColor), Ord(Pred(GradientDirection)));
  if Assigned(FHighlights) then
  begin
    CanDrawText := True;
    CanDrawImage := True;
    for I := 0 to FHighlights.Count - 1 do
      if TVarData(FCell.Value).VType > 1 then
        TfcxGraphicHighlight(FHighlights[I]).DrawValue(FCanvas, 100, R, @FCell, CanDrawImage, CanDrawText);
  end;
end;

procedure TfcxpMemoView.SetHighlights(const Value: String);
var
  x: TfrxXMLDocument;
  Item: TfrxXMLItem;
  i: Integer;
  Stream: TStringStream;
begin
  if Value = '' then
  begin
    FreeAndNil(FHighlights);
    Exit;
  end;
  // Highlights are internal objects which belongs to Slice. We should not store
  // them as text, just pointers
  Stream := TStringStream.Create(Value);
  x := TfrxXMLDocument.Create;
  try
    x.LoadFromStream(Stream);
    Item := x.Root;
    if Item.Prop['Value'] = '' then
      FCell.Value := Null
    else
      FCell.Value := StrToFloatDef(Item.Prop['Value'], 0);
    FCell.StrValue := Item.Prop['StrValue'];
    FCell.BaseIndex := StrToInt(Item.Prop['BaseIndex']);
    FCell.SecondIndex := StrToInt(Item.Prop['SecondIndex']);
    FCell.MeasureIndex := StrToInt(Item.Prop['MeasureIndex']);
    FCell.Alignment := TAlignment(StrToInt(Item.Prop['Alignment']));
    FCell.IsTotal := StrToBool(Item.Prop['IsTotal']);
    FCell.IsGrandTotal := StrToBool(Item.Prop['IsGrandTotal']);

    FHighlights := TList.Create;
    for i := 0 to Item.Count - 1 do
      FHighlights.Add(Pointer(PtrInt(StrToInt(Item.Items[i].Prop['ptr']))));
  finally
    Stream.Free;
    x.Free;
  end;
end;

function TfcxpMemoView.GetHighlights: String;
var
  x: TfrxXMLDocument;
  Item: TfrxXMLItem;
  i: Integer;
  Stream: TStringStream;
begin
  if not Assigned(FHighlights) or (FHighlights.Count = 0) then
  begin
    Result := '';
    Exit;
  end;
  // Highlights are internal objects which belongs to Slice. We should not store
  // them as text, just pointers
  x := TfrxXMLDocument.Create;
  x.Root.Name := 'highlights';
  try
    Item := x.Root;
    Item.Prop['Value'] := frxValueToXML(FCell.Value);
    Item.Prop['StrValue'] := FCell.StrValue;
    Item.Prop['BaseIndex'] := IntToStr(FCell.BaseIndex);
    Item.Prop['SecondIndex'] := IntToStr(FCell.SecondIndex);
    Item.Prop['MeasureIndex'] := IntToStr(FCell.MeasureIndex);
    Item.Prop['Alignment'] := IntToStr(Ord(FCell.Alignment));
    Item.Prop['IsTotal'] := BoolToStr(FCell.IsTotal);
    Item.Prop['IsGrandTotal'] := BoolToStr(FCell.IsGrandTotal);
    for i := 0 to FHighlights.Count - 1 do
    begin
      with Item.Add do
      begin
        Name := 'highlight';
        Prop['ptr'] := IntToStr(PtrInt(FHighlights[i]));
      end;
    end;
    Stream := TStringStream.Create('');
    try
      x.SaveToStream(Stream);
      Result := Stream.DataString;
    finally
      Stream.Free;
    end;
  finally
    x.Free;
  end;
end;

initialization
  frxObjects.RegisterObject1(TfcxpMemoView, nil, '', '', 0, 2, [ctReport]);
  frxObjects.RegisterObject1(TfcxpCrossView, fcxGraphicResources.GetFRBitmap(2), '', '', 0, -1, [ctReport, ctDMP]);
  frxHideProperties(TfcxpCrossView, 'SliceGridProviderName');
  frxResources.Add('TfrxPrintCellEvent',
    'PascalScript=(Memo: TfrxMemoView; RowIndex, ColumnIndex, CellIndex: Integer; RowValues, ColumnValues, Value: Variant);' + #13#10 +
    'C++Script=(TfrxMemoView Memo, int RowIndex, int ColumnIndex, int CellIndex, variant RowValues, variant ColumnValues, variant Value)' + #13#10 +
    'BasicScript=(Memo, RowIndex, ColumnIndex, CellIndex, RowValues, ColumnValues, Value)' + #13#10 +
    'JScript=(Memo, RowIndex, ColumnIndex, CellIndex, RowValues, ColumnValues, Value)');
  frxResources.Add('TfrxPrintHeaderEvent',
    'PascalScript=(Memo: TfrxMemoView; HeaderIndexes, HeaderValues, Value: Variant);' + #13#10 +
    'C++Script=(TfrxMemoView Memo, variant HeaderIndexes, variant HeaderValues, variant Value)' + #13#10 +
    'BasicScript=(Memo, HeaderIndexes, HeaderValues, Value)' + #13#10 +
    'JScript=(Memo, HeaderIndexes, HeaderValues, Value)');
  frxResources.Add('TfrxCalcWidthEvent',
    'PascalScript=(ColumnIndex: Integer; ColumnValues: Variant; var Width: Extended);' + #13#10 +
    'C++Script=(int ColumnIndex, variant ColumnValues, float &Width)' + #13#10 +
    'BasicScript=(ColumnIndex, ColumnValues, byref Width)' + #13#10 +
    'JScript=(ColumnIndex, ColumnValues, &Width)');
  frxResources.Add('TfrxCalcHeightEvent',
    'PascalScript=(RowIndex: Integer; RowValues: Variant; var Height: Extended);' + #13#10 +
    'C++Script=(int RowIndex, variant RowValues, float &Height)' + #13#10 +
    'BasicScript=(RowIndex, RowValues, byref Height)' + #13#10 +
    'JScript=(RowIndex, RowValues, &Height)');

  // add properties description
  frxResources.Add('propCube', fcxResources.Get('propCube'));
  frxResources.Add('propPaintSizes', fcxResources.Get('propPaintSizes'));
  frxResources.Add('propShowNames', fcxResources.Get('propShowNames'));
  frxResources.Add('propAutoSizeStyle', fcxResources.Get('propAutoSizeStyle'));
  frxResources.Add('propDefaultColWidth', fcxResources.Get('propDefaultColWidth'));
  frxResources.Add('propDefaultRowHeight', fcxResources.Get('propDefaultRowHeight'));
  frxResources.Add('propMaxColWidth', fcxResources.Get('propMaxColWidth'));

finalization
  frxObjects.UnRegister(TfcxpCrossView);
end.
