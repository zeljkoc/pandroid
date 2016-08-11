{*******************************************************}
{                                                       }
{           FastCube 2  grid painters unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxGridPainters;

interface

{$INCLUDE fcx.inc}
uses
{$IFDEF FPC}
  LResources, LCLType, LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF USE_THEMES}
  Themes,
  {$IFNDEF FPC}
  UxTheme,
  {$ENDIF}
{$ENDIF}
{$IFDEF Delphi_6UP}
  GraphUtil,
{$ENDIF}
{$IFDEF Delphi_18UP}
  System.UITypes,
{$ENDIF}
  Types, Classes, SysUtils, Controls, Forms, StdCtrls, Graphics, ImgList,
  fcxTypes, fcxGraphicUtils, fcxPainters, fcxRes, fcxXml, fcxStyles;

const
  clInfoBkStd = $EBFFFF;

const
  // bitmaps
  bmpPlusBtn = 0;
  bmpMinusBtn = 1;
  bmpTreePlusBtn = 2;
  bmpTreeMinusBtn = 3;
  bmpSortArrowUp = 4;
  bmpSortArrowDown = 5;
  bmpSortArrowUpDown = 6;
  bmpDropDown = 7;
  bmpFilterDown = 8;
  bmpGridFirst = bmpPlusBtn;
  bmpGridLast = bmpFilterDown;

type
  TDrawDataCellHookProc = procedure(AHookData: Pointer; APainter: TfcxCustomPainter; ACanvas: TCanvas;
    var ARect: TRect; var CanDrawImage, CanDrawText: Boolean) of object;

  { TfcxCustomGridPainter }

  TfcxCustomGridPainter = class(TfcxCustomPainter)
  protected
    FBitmaps: array[bmpGridFirst..bmpGridLast] of TBitmap;
    FUtilImages: TImageList;
    function GetUtilImages: TImageList;
    class function GetPostfix: String; virtual;
    class function SetClipRegion(ADC: HDC; ARect: TRect): HRGN;
    property UtilImages: TImageList read GetUtilImages;
    procedure RescaleFont(ACanvas: TCanvas; AScale: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    // metrics
    function GetDropDownButtonRect(ARect: TRect; Options: TfcxThemeButtonOptions): TRect; virtual;
    function GetItemButtonThemeSpacing: TSize; virtual;
    function GetScrollButtonSize: Integer; virtual;
    function GetScaleSliderButtonSize: Integer; virtual;

    // zone + common
    procedure DrawFocusRect(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); virtual; abstract;
    procedure DrawBody(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle); virtual;
    procedure DrawHeader(ACanvas: TCanvas; ARect: TRect; AText: TfcxString; AState: TfcxThemeState); virtual; abstract;
    class procedure DrawStyledBackground(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle);
    procedure DrawTreeButton(ACanvas: TCanvas; APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean); virtual; abstract;
    procedure DrawDottedHLine(ACanvas: TCanvas; Y, X1, X2: Integer); virtual;
    procedure DrawDottedVLine(ACanvas: TCanvas; X, Y1, Y2: Integer); virtual;

    class function DrawText(ACanvas: TCanvas; var ARect: TRect; AText: TfcxString;
      AFormat: DWord; StyleModifiers: TFontStyles = []): Integer; virtual;

    class procedure DrawStyledText(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AFormat: DWord; AStyle: TfcxCustomThemeStyle; StyleModifiers: TFontStyles = [];
      ASpacing: Integer = 0; ASelStart: Integer = 0; ASelLength: Integer = 0); virtual;


    // grid
    procedure DrawAxisItem(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle); virtual; abstract;
    function MeasureAxisItem(ACanvas: TCanvas; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize; virtual; abstract;

    procedure DrawDropDownButton(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; ARect: TRect; Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState); virtual; abstract;
    procedure DrawItemButton(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AImageList: TCustomImageList; AImageIndex: Integer;
      Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState;
      AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN); virtual; abstract;
    procedure DrawSortArrow(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType); virtual; abstract;
    procedure DrawSplitLine(ACanvas: TCanvas; AStartPos, AEndPos: TPoint); virtual;
    procedure DrawDataCell(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AAlignment: TAlignment; AStates: TfcxThemeCellStates;
      AStyle: TfcxCustomThemeStyle; AScale: Integer;
      AImages: TCustomImageList = nil; AImageIndex: Integer = -1;
      HookProc: TDrawDataCellHookProc = nil; HookData: Pointer = nil); virtual;
    procedure DrawDataCellFrame(ACanvas: TCanvas; ARect: TRect); virtual;

    procedure DrawAxisCell(ACanvas: TCanvas; ARect: TRect; AText: TfcxString; ACellProperties: TfcxPropertiesOfCellAxis;
      AAlignment: TAlignment; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle; AScale: Integer;
      out TreeButtonPos, HierButtonPos: TPoint; AImages: TCustomImageList = nil; AImageIndex: Integer = -1;
      ShowEllipsis: Boolean = True); virtual;
    function MeasureAxisCell(ACanvas: TCanvas; ASize: TSize; AText: TfcxString;
      ACellProperties: TfcxPropertiesOfCellAxis;
      AAlignment: TAlignment; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle;
      AScale: Integer; GrowInHeight: Boolean;
      AImages: TCustomImageList = nil; AImageIndex: Integer = -1): TSize; virtual;

    procedure DrawHeaderCell(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle;
      AHasSortArrow: Boolean = False; ASortType: TfcxThemeSortType = tstUnknown;
      AImages: TCustomImageList = nil; AImageIndex: Integer = -1);
    procedure DrawScrollArrow(ACanvas: TCanvas; ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState); virtual;
    procedure DrawScrollButton(ACanvas: TCanvas; ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState); virtual;
    procedure DrawRowIndicator(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle); virtual;

    // for popup window
    procedure DrawStatus(ACanvas: TCanvas; var ARect: TRect; AStyle: TfcxCustomThemeStyle); virtual;
    procedure DrawStatusPane(ACanvas: TCanvas; ARect: TRect); virtual;
    function GetSizeGripRect(ARect: TRect): TRect; virtual;
    procedure DrawSizeGrip(ACanvas: TCanvas; ARect: TRect); virtual;
    function GetCheckRect(ARect: TRect): TRect; virtual;
    procedure DrawCheck(ACanvas: TCanvas; ARect: TRect; ACheckState: TCheckBoxState; AState: TfcxThemeState); virtual;
    procedure DrawRadio(ACanvas: TCanvas; ARect: TRect; AChecked: Boolean; AState: TfcxThemeState); virtual;
    procedure DrawButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); virtual;
    procedure DrawToolButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); virtual;
    function GetButtonContent(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState): TRect; virtual;
    procedure DrawImageCell(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); virtual;
    procedure DrawScaleSliderButton(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle; AState: TfcxThemeState); virtual;

    function GetBitmap(AIndex: Integer): TBitmap;
  end;

  TfcxStandardGridPainter = class(TfcxCustomGridPainter)
  protected
    class function GetPostfix: String; override;
  public
    class function GetPaintStyle: TfcxPaintStyle; override;

    procedure DrawAxisItem(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle); override;
    function MeasureAxisItem(ACanvas: TCanvas; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize; override;

    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); override;
    procedure DrawDropDownButton(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; ARect: TRect; Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState); override;
    procedure DrawHeader(ACanvas: TCanvas; ARect: TRect; AText: TfcxString; AState: TfcxThemeState); override;
    procedure DrawItemButton(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AImageList: TCustomImageList; AImageIndex: Integer;
      Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState;
      AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN); override;
    procedure DrawSortArrow(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType); override;
    procedure DrawTreeButton(ACanvas: TCanvas; APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean); override;
  end;

  TfcxFlatGridPainter = class(TfcxCustomGridPainter)
  protected
    class function GetPostfix: String; override;
  public
    class function GetPaintStyle: TfcxPaintStyle; override;

    procedure DrawAxisItem(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle); override;
    function MeasureAxisItem(ACanvas: TCanvas; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize; override;

    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); override;
    procedure DrawDropDownButton(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; ARect: TRect; Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState); override;
    procedure DrawHeader(ACanvas: TCanvas; ARect: TRect; AText: TfcxString; AState: TfcxThemeState); override;
    procedure DrawItemButton(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AImageList: TCustomImageList; AImageIndex: Integer;
      Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState;
      AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN); override;
    procedure DrawSortArrow(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType); override;
    procedure DrawTreeButton(ACanvas: TCanvas; APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean); override;
    procedure DrawButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); override;
    procedure DrawScrollButton(ACanvas: TCanvas; ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState); override;
  end;

  TfcxXPGridPainter = class(TfcxCustomGridPainter)
  private
    FStdPainter: TfcxStandardGridPainter;
    FThemesEnabled: Boolean;
    FSystemStyle: Boolean;
  protected
    class function GetSystemStyle: Boolean;
    class function GetThemeEnabled: Boolean;
    class function GetPostfix: String; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetPaintStyle: TfcxPaintStyle; override;

    // metrics
    function GetDropDownButtonRect(ARect: TRect; Options: TfcxThemeButtonOptions): TRect; override;
    function GetScrollButtonSize: Integer; override;
    function GetItemButtonThemeSpacing: TSize; override;

    procedure DrawAxisItem(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle); override;
    function MeasureAxisItem(ACanvas: TCanvas; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize; override;

    procedure DrawBorder(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); override;
    procedure DrawDropDownButton(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; ARect: TRect; Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState); override;
    procedure DrawHeader(ACanvas: TCanvas; ARect: TRect; AText: TfcxString; AState: TfcxThemeState); override;
    procedure DrawItemButton(ACanvas: TCanvas; ARect: TRect; AText: TfcxString;
      AImageList: TCustomImageList; AImageIndex: Integer;
      Options: TfcxThemeButtonOptions;
      ASortType: TfcxThemeSortType; AState: TfcxThemeState;
      AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN); override;
    procedure DrawSortArrow(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType); override;
    procedure DrawDataCellFrame(ACanvas: TCanvas; ARect: TRect); override;

    procedure DrawStatus(ACanvas: TCanvas; var ARect: TRect; AStyle: TfcxCustomThemeStyle); override;
    procedure DrawStatusPane(ACanvas: TCanvas; ARect: TRect); override;
    function GetSizeGripRect(ARect: TRect): TRect; override;
    procedure DrawSizeGrip(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawTreeButton(ACanvas: TCanvas; APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean); override;
    procedure DrawCheck(ACanvas: TCanvas; ARect: TRect; ACheckState: TCheckBoxState; AState: TfcxThemeState); override;
    procedure DrawRadio(ACanvas: TCanvas; ARect: TRect; AChecked: Boolean; AState: TfcxThemeState); override;
    procedure DrawButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); override;
    procedure DrawToolButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState); override;
    procedure DrawScrollButton(ACanvas: TCanvas; ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState); override;
  end;

  TfcxDefaultGridPainter = class(TfcxXPGridPainter)
  public
    class function GetPaintStyle: TfcxPaintStyle; override;
  end;

  TfcxGridPainterClass = class of TfcxCustomGridPainter;

const
  GridPainterClass: array[TfcxPaintStyle] of TfcxGridPainterClass =
  (
{psDefault } TfcxDefaultGridPainter,
{psStandard} TfcxStandardGridPainter,
{psFlat    } TfcxFlatGridPainter,
{psXP      } TfcxXPGridPainter
  );
  CollapsedToTreeButtonKind: array[Boolean] of TfcxTreeButtonKind = (tbkMinusButton, tbkPlusButton);
  CollapsedToHierButtonKind: array[Boolean] of TfcxTreeButtonKind = (tbkTreeButtonMinus, tbkTreeButtonPlus);

  TreeButtonSize = 9;
  SortArrowSize = 9;
  
implementation

uses
  Math, TypInfo;

const
  AlignmentFormat: array[TAlignment] of DWORD = (
    DT_LEFT,
    DT_RIGHT,
    DT_CENTER
  );
  EllipsisFormat: array[Boolean] of DWORD = (
    0,
    DT_END_ELLIPSIS or DT_MODIFYSTRING
  );

  ThemeStateToFrameControlState: array[TfcxThemeState] of DWORD = (
    0,
    DFCS_HOT,
    DFCS_PUSHED,
    DFCS_INACTIVE
  );

  ItemButtonMinTextRect = 46;

function fcxDrawText(ADC: HDC; P: PfcxChar; Len: Integer; var ARect: TRect; AFormat: Cardinal): Integer;
begin
  {$IFDEF FPC}
    Result := LCLIntf.DrawText(ADC, P, Len, ARect, AFormat);
  {$ELSE}
    {$IFDEF UseAnsiString}
      Result := DrawTextExA(ADC, P, Len, ARect, AFormat, nil)
    {$ELSE}
      {$IFDEF UseWideString}
        Result := DrawTextExW(ADC, P, Len, ARect, AFormat, nil);
      {$ELSE}
        Result := DrawTextEx(ADC, P, Len, ARect, AFormat, nil);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;
 
// a helper routine to draw a simple arrow
procedure DoDrawArrow(const ACanvas: TCanvas; const P: TPoint; const AArrowType: TfcxThemeSortType);
var
  tdx, bdx: integer;
begin
  case AArrowType of
    tstUp:
      begin
        tdx := 1;
        bdx := -2;
      end;
    tstDown:
      begin
        tdx := -2;
        bdx := 1;
      end;
    else
      Exit;
  end;
  ACanvas.Polygon([Point(p.x - 3, p.y + tdx),
                   Point(p.x + 3, p.y + tdx),
                   Point(p.x + 0, p.y + bdx)]);
end;

{ TfcxCustomGridPainter }

constructor TfcxCustomGridPainter.Create;
begin
  inherited Create;
  FUtilImages:= TImageList.Create(nil);
end;

destructor TfcxCustomGridPainter.Destroy;
begin
  FUtilImages.Free;
  inherited;
end;

procedure TfcxCustomGridPainter.DrawAxisCell(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; ACellProperties: TfcxPropertiesOfCellAxis;
  AAlignment: TAlignment; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle; AScale: Integer;
  out TreeButtonPos, HierButtonPos: TPoint; AImages: TCustomImageList = nil; AImageIndex: Integer = -1;
  ShowEllipsis: Boolean = True);

const
  Spacing = 3;

  function HasSpaceForTreeButton(const ARect: TRect): Boolean;
  begin
    Result := (ARect.Bottom - ARect.Top - Spacing * 2 >= TreeButtonSize) and
              (ARect.Right - ARect.Left - Spacing * 2 >= TreeButtonSize);
  end;

var
  uFormat: Cardinal;
  R: TRect;
begin
  TreeButtonPos := Point(-1, -1);
  HierButtonPos := Point(-1, -1);
  if not IsRectEmpty(ARect) and ((ARect.Bottom - ARect.Top) > 2)
     and ((ARect.Right - ARect.Left) > Spacing) then
  begin
    DrawAxisItem(ACanvas, ARect, AState, AStyle);
    ACanvas.Font.Assign(AStyle.Font);
    ACanvas.Font.Color := AStyle.TextColor;
    RescaleFont(ACanvas, AScale);
    InflateRect(ARect, -1, -1);

    inc(ARect.Left, Spacing);
    if (ACellProperties * [pca_Expanded, pca_Collapsed]) <> [] then // Draw plus/minus
    begin
      if HasSpaceForTreeButton(ARect) then
      begin
        TreeButtonPos := Point(ARect.Left, ARect.Top + 3);
        DrawTreeButton(ACanvas, TreeButtonPos, CollapsedToTreeButtonKind[pca_Collapsed in ACellProperties], AStyle, False);
      end;
      inc(ARect.Left, TreeButtonSize + Spacing);
    end;

    if (ACellProperties * [pca_HExpanded, pca_HCollapsed, pca_GExpanded, pca_GCollapsed]) <> [] then // Draw hierarchi plus/minus
    begin
      if HasSpaceForTreeButton(ARect) then
      begin
        HierButtonPos := Point(ARect.Left, ARect.Top + 3);
        DrawTreeButton(ACanvas, HierButtonPos, CollapsedToHierButtonKind[[pca_HCollapsed , pca_GCollapsed] * ACellProperties <> []], AStyle, False);
      end;
      inc(ARect.Left, TreeButtonSize + Spacing);
    end;

    if (pca_Sort in ACellProperties) then
    begin
      // this can be only a last cell with standard height
      DrawSortArrow(ACanvas, AStyle, Point(ARect.Left, ARect.Top + 3), tstUnknown);
      inc(ARect.Left, SortArrowSize + Spacing);
    end;

    dec(ARect.Right, Spacing);

    inc(ARect.Top);

    if Assigned(AImages) and (AImageIndex <> -1) then
    begin
      R := ARect;
      R.Right := R.Left + MulDiv(AImages.Width, AScale, 100);
      R.Bottom := R.Top + MulDiv(AImages.Height, AScale, 100);
      ImageListScaleDraw(ACanvas, AImages, AImageIndex, R);
      inc(ARect.Left, R.Right - R.Left + Spacing);
    end;

    uFormat := DT_NOPREFIX or DT_WORDBREAK or EllipsisFormat[ShowEllipsis] or AlignmentFormat[AAlignment];
    AText := Trim(AText);
    DrawText(ACanvas, ARect, AText, uFormat);
  end;
end;

procedure TfcxCustomGridPainter.DrawBody(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle);
begin
  DrawStyledBackground(ACanvas, ARect, AStyle);
end;

procedure TfcxCustomGridPainter.DrawButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState);
begin
  DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON,
    DFCS_BUTTONPUSH or ThemeStateToFrameControlState[AState]);
end;

procedure TfcxCustomGridPainter.DrawCheck(ACanvas: TCanvas; ARect: TRect; ACheckState: TCheckBoxState; AState: TfcxThemeState);
var
  OldBrushColor: TColor;
begin
  with ACanvas do
  begin
    OldBrushColor := Brush.Color;
    case AState of
      tsHot: Brush.Color := GetHighLightColor(clHighlight  {$IFDEF DELPHI_7UP}, 38 {$ENDIF});
      tsPressed: Brush.Color := GetHighLightColor(clHighlight);
      tsDisabled: Brush.Color := clBtnFace;
    end;
    Pen.Color := clBlack;
    Rectangle(ARect);
    if ACheckState = cbChecked then
    begin
      InflateRect(ARect, -2, -2); // 2 is a border between cross and rect
      PenPos := ARect.TopLeft;
      LineTo(ARect.Right, ARect.Bottom);
      PenPos := Point(ARect.Left, ARect.Bottom - 1);
      LineTo(ARect.Right, ARect.Top - 1);
    end
    else
    if ACheckState = cbGrayed then
    begin
      InflateRect(ARect, -2, -2);
      Brush.Color := clSilver;
      FillRect(ARect);
    end;
    Brush.Color := OldBrushColor;
  end;
end;

procedure TfcxCustomGridPainter.DrawDataCell(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AAlignment: TAlignment; AStates: TfcxThemeCellStates;
  AStyle: TfcxCustomThemeStyle; AScale: Integer;
  AImages: TCustomImageList = nil; AImageIndex: Integer = -1;
  HookProc: TDrawDataCellHookProc = nil; HookData: Pointer = nil);
var
  R, FocusRect: TRect;
  Flags: DWord;
  CanDrawText, CanDrawImage: Boolean;
begin
  DrawStyledBackground(ACanvas, ARect, AStyle);
  ACanvas.Font.Assign(AStyle.Font);
  ACanvas.Font.Color := AStyle.TextColor;
  RescaleFont(ACanvas, AScale);
  DrawDataCellFrame(ACanvas, ARect);

  FocusRect := ARect;
  InflateRect(ARect, -1, -1);

  CanDrawText := True;
  CanDrawImage := True;

  if Assigned(HookProc) then
    HookProc(HookData, Self, ACanvas, ARect, CanDrawImage, CanDrawText);

  if tcsFocused in AStates then
    DrawFocusRect(ACanvas, FocusRect);

  // data margins
  Inc(ARect.Left, 2);
  Dec(ARect.Right, 2);

  if CanDrawImage and Assigned(AImages) and (AImageIndex <> -1) then
  begin
    R.Left := ARect.Left;
    R.Bottom := MulDiv(AImages.Height, AScale, 100);
    R.Top := (ARect.Top + ARect.Bottom - R.Bottom) div 2;
    R.Right := R.Left + MulDiv(AImages.Width, AScale, 100);
    R.Bottom := R.Top + R.Bottom;
    ImageListScaleDraw(ACanvas, AImages, AImageIndex, R);
    inc(ARect.Left, (R.Right - R.Left) + 2);
  end;

  if CanDrawText then
  begin
    with ACanvas do
    begin
      Flags := DT_END_ELLIPSIS or DT_SINGLELINE or DT_EXPANDTABS or DT_LEFT;
      R := ARect;
      with R do
      begin
        case AAlignment of
          taLeftJustify : inc(Left);
          taRightJustify : Left := max(Right - TextWidth(AText) - 1, Left);
          taCenter : Left := Max((Left + Right - TextWidth(AText)) shr 1, Left);
        end;
        Top := (Top + Bottom - TextHeight(Atext)) shr 1;
        Bottom := Top + TextHeight(Atext);
      end;

      IntersectRect(R, R, ARect);
      DrawText(ACanvas, R, AText, Flags);
    end
  end;
  //Canvas.Font.Assign(FOldFont);
end;

procedure TfcxCustomGridPainter.DrawDataCellFrame(ACanvas: TCanvas; ARect: TRect);
var
  FrameColor: TColor;
begin
  FrameColor := GetShadowColor(ColorToRGB(clBtnHighlight));
  BevelLine(ACanvas, FrameColor, ARect.Left, ARect.Bottom - 1, ARect.Right, ARect.Bottom - 1);
  BevelLine(ACanvas, FrameColor, ARect.Right - 1, ARect.Top, ARect.Right - 1, ARect.Bottom);
end;

procedure TfcxCustomGridPainter.DrawDottedHLine(ACanvas: TCanvas; Y, X1, X2: Integer);
begin
  ACanvas.Pen.Color := clBtnShadow;
  while X1 < X2 do
  begin
    ACanvas.Pixels[X1, Y] := ACanvas.Pen.Color;
    inc(X1, 2);
  end;
end;

procedure TfcxCustomGridPainter.DrawDottedVLine(ACanvas: TCanvas; X, Y1, Y2: Integer);
begin
  ACanvas.Pen.Color := clBtnShadow;
  while Y1 < Y2 do
  begin
    ACanvas.Pixels[X, Y1] := ACanvas.Pen.Color;
    inc(Y1, 2);
  end;
end;

procedure TfcxCustomGridPainter.DrawFocusRect(ACanvas: TCanvas; ARect: TRect);
begin
  {$IFNDEF FPC}Windows.{$ELSE}LCLIntf.{$ENDIF}DrawFocusRect(ACanvas.Handle, ARect);
end;

procedure TfcxCustomGridPainter.DrawHeaderCell(ACanvas: TCanvas;
  ARect: TRect; AText: TfcxString; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle;
  AHasSortArrow: Boolean = False; ASortType: TfcxThemeSortType = tstUnknown;
  AImages: TCustomImageList = nil; AImageIndex: Integer = -1);

const
  Spacing = 2;
  
var
  R: TRect;
  P: TPoint;  
begin
  DrawAxisItem(ACanvas, ARect, AState, AStyle);
  InflateRect(ARect, -1, -1);

  if Assigned(AImages) and (AImageIndex <> -1) then
  begin
    R := ARect;
    R.Right := R.Left + AImages.Width;
    R.Bottom := R.Top + AImages.Height;
    ImageListScaleDraw(ACanvas, AImages, AImageIndex, R);
    inc(ARect.Left, R.Right - R.Left + Spacing);
  end;

  if AHasSortArrow then
  begin
    P.X := ARect.Right - SortArrowSize - Spacing;
    P.Y := (ARect.Bottom + ARect.Top - SortArrowSize) div 2 + Ord(Odd(SortArrowSize));
    DrawSortArrow(ACanvas, AStyle, P, ASortType);
    ARect.Right := P.X - Spacing;
  end;

  if AText <> '' then
  begin
    ACanvas.Font.Assign(AStyle.Font);
    ACanvas.Font.Color := AStyle.TextColor;
    DrawText(ACanvas, ARect, AText, DT_END_ELLIPSIS or DT_SINGLELINE or DT_EXPANDTABS or DT_VCENTER or DT_CENTER);
  end;
end;

procedure TfcxCustomGridPainter.DrawImageCell(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState);
const
  BgColors: array[TfcxThemeState] of TColor = (
  { tsNormal   } 0,
  { tsHot      } $C2EEFF,
  { tsPressed  } $6FC0FF,
  { tsDisabled } clGray
  );
  FrColors: array[TfcxThemeState] of TColor = (
  { tsNormal   } 0,
  { tsHot      } $4DD1F1,
  { tsPressed  } $3FABFF,
  { tsDisabled } clGray
  );
begin
  with ACanvas do
  begin
    if AState <> tsNormal then
      Brush.Color := BGColors[AState];
    if AState <> tsNormal then
      Pen.Color := FrColors[AState]
    else
      Pen.Color := Brush.Color;
    Rectangle(ARect);
  end;
end;

procedure TfcxCustomGridPainter.DrawRadio(ACanvas: TCanvas; ARect: TRect;
  AChecked: Boolean; AState: TfcxThemeState);
var
  OldBrushColor: TColor;
begin
  InflateRect(ARect, -1, -1);
  with ACanvas do
  begin
    OldBrushColor := Brush.Color;
    case AState of
      tsHot: Brush.Color := GetHighLightColor(clHighlight {$IFDEF DELPHI_7UP}, 38 {$ENDIF});
      tsPressed: Brush.Color := GetHighLightColor(clHighlight);
      tsDisabled: Brush.Color := clBtnFace;
    end;
    Pen.Color := clBlack;
    Ellipse(ARect);
    if AChecked then
    begin
      InflateRect(ARect, -3, -3); // 3 is a border between inner circle and outer
      Brush.Color := clBlack;
      Ellipse(ARect);
    end;
    Brush.Color := OldBrushColor;
  end;
end;

procedure TfcxCustomGridPainter.DrawRowIndicator(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle);
var
  P: TPoint;
  OldPenColor, OldBrushColor: TColor;
begin
  OldPenColor := ACanvas.Pen.Color;
  OldBrushColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := GetShadowColor(AStyle.FillColor);
  ACanvas.Pen.Color := ACanvas.Brush.Color;

  P.X := (ARect.Left + ARect.Right) div 2;
  P.Y := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.Polygon([Point(p.x - 2, p.y - 3),
                   Point(p.x - 2, p.y + 3),
                   Point(p.x + 1, p.y + 0)]);

  ACanvas.Pen.Color := OldPenColor;
  ACanvas.Brush.Color := OldBrushColor;
end;

procedure TfcxCustomGridPainter.DrawScaleSliderButton(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle; AState: TfcxThemeState);
const
  SignSize = 10;
var
  APosition: TPoint;
  Bmp: TBitmap;
begin
  InflateRect(ARect, -2, -2);
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := (ARect.Right - ARect.Left) * 2;
    Bmp.Height := (ARect.Bottom - ARect.Top) * 2;
    Bmp.Canvas.Brush.Color := clGreen;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    case AState of
      tsNormal:
        begin
          Bmp.Canvas.Pen.Color := clBtnShadow;
          Bmp.Canvas.Brush.Style := bsClear;
        end;
      tsHot:
        begin
          Bmp.Canvas.Pen.Color := clBlack;
          Bmp.Canvas.Brush.Style := bsClear;
        end;
      tsPressed:
        begin
          Bmp.Canvas.Pen.Color := clBtnHighlight;
          Bmp.Canvas.Brush.Color := clBtnShadow;
        end;
    end;
    Bmp.Canvas.Ellipse(0, 0, Bmp.Width, Bmp.Height);
    Bmp.TransparentColor := clGreen;
    Bmp.Transparent := True;
    SetStretchBltMode(ACanvas.HAndle, HALFTONE);
    ACanvas.StretchDraw(ARect, Bmp);
  finally
    Bmp.Free;
  end;
  
  APosition.X := (ARect.Left + ARect.Right) div 2;
  APosition.Y := (ARect.Top + ARect.Bottom) div 2;

  ACanvas.MoveTo(APosition.X - SignSize div 2, APosition.Y);
  ACanvas.LineTo(APosition.X + SignSize div 2, APosition.Y);

//  if AKind = tbkPlusButton then
  begin
    ACanvas.MoveTo(APosition.X, APosition.Y - SignSize div 2);
    ACanvas.LineTo(APosition.X, APosition.Y + SignSize div 2);
  end;
end;

procedure TfcxCustomGridPainter.DrawScrollArrow(ACanvas: TCanvas;
  ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState);
var
  P: TPoint;
  I: Integer;
  XDirection, YDirection: Integer;
begin
  if AState = tsDisabled then
    ACanvas.Pen.Color := clGray
  else
    ACanvas.Pen.Color := clBlack;
  XDirection := 0;
  YDirection := 0;
  case ADirection of
    tdLeft: XDirection := -1;
    tdRight: XDirection := 1;
    tdUp: YDirection := -1;
    tdDown: YDirection := 1;
  end;
  // find center point
  P.X := (ARect.Left + ARect.Right) div 2 + XDirection;
  P.Y := (ARect.Top + ARect.Bottom) div 2 + YDirection;
  if ADirection in [tdLeft, tdRight] then
    for I := 0 to 2 do
    begin
      ACanvas.MoveTo(P.X - XDirection * I, P.Y - (2 - I));
      ACanvas.LineTo(P.X - XDirection * I, P.Y + (2 - I) + 1);
    end
  else
    for I := 0 to 2 do
    begin
      ACanvas.MoveTo(P.X - (2 - I), P.Y - YDirection);
      ACanvas.LineTo(P.X + (2 - I) + 1, P.Y - YDirection);
    end
end;

procedure TfcxCustomGridPainter.DrawScrollButton(ACanvas: TCanvas;
  ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState);
begin
  DrawButton(ACanvas, ARect, AState);
  DrawScrollArrow(ACanvas, ARect, ADirection, AState);
end;

procedure TfcxCustomGridPainter.DrawSizeGrip(ACanvas: TCanvas; ARect: TRect);

  procedure DrawCorner(StartPoint: TPoint; AColor: TColor);
  var
    i: integer;
  begin
    ACanvas.Pen.Color := AColor;
    ACanvas.Brush.Color := AColor;
    for i := 2 downto 0 do
    begin
      ACanvas.MoveTo(StartPoint.X - (2-i) * 4 - 1, ARect.Bottom);
      // 1px pen needs to be drawn to the end and 2px pen should not draw the last point 
      ACanvas.LineTo(ARect.Right + 2 - ACanvas.Pen.Width, StartPoint.Y - (2-i) * 4 - 1 - 2 + ACanvas.Pen.Width);
    end;
  end;
var
  StartPoint: TPoint;
begin
  StartPoint := ARect.BottomRight;
  dec(StartPoint.X, 1);
  dec(StartPoint.Y, 1);
  ACanvas.Pen.Width := 2;
  DrawCorner(StartPoint, clBtnShadow);
  dec(StartPoint.X, 1);
  dec(StartPoint.Y, 1);
  ACanvas.Pen.Width := 1;
  DrawCorner(StartPoint, clBtnHighlight);
end;

procedure TfcxCustomGridPainter.DrawSplitLine(ACanvas: TCanvas; AStartPos, AEndPos: TPoint);
var
  OldPen: TPen;
begin
  OldPen := TPen.Create;
  try
    with ACanvas do
    begin
      OldPen.Assign(Pen);
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Width := 1;
      try
        MoveTo(AStartPos.X, AStartPos.Y);
        LineTo(AEndPos.X, AEndPos.Y);
      finally
        Pen.Assign(OldPen);
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TfcxCustomGridPainter.DrawStatus(ACanvas: TCanvas; var ARect: TRect; AStyle: TfcxCustomThemeStyle);
begin
  BevelLine(ACanvas, clBtnShadow, ARect.Left, ARect.Top, ARect.Right, ARect.Top);
  inc(ARect.Top);
  DrawStyledBackground(ACanvas, ARect, AStyle)
end;

procedure TfcxCustomGridPainter.DrawStatusPane(ACanvas: TCanvas; ARect: TRect);
begin
  with ARect do
    BevelLine(ACanvas, clBtnShadow, Right, Top, Right, Bottom);
end;

class procedure TfcxCustomGridPainter.DrawStyledBackground(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle);
begin
  AStyle.DrawBackground(ACanvas, ARect);
end;

class procedure TfcxCustomGridPainter.DrawStyledText(ACanvas: TCanvas;
  ARect: TRect; AText: TfcxString; AFormat: DWord;
  AStyle: TfcxCustomThemeStyle; StyleModifiers: TFontStyles;
  ASpacing, ASelStart, ASelLength: Integer);
var
  OldTextColor, OldBrushColor: TColor;
  SelectionText, AfterText: String;
  SelectionRect: TRect;
begin
  if (DT_CALCRECT and AFormat) <> 0 then
    Exit;

  DrawStyledBackground(ACanvas, ARect, AStyle);
  InflateRect(ARect, -ASpacing, -ASpacing);
  ACanvas.Font.Assign(AStyle.Font);
  if (ASelLength > 0) then
  begin
    SelectionText := Copy(AText, ASelStart, ASelLength);
    AfterText := Copy(AText, ASelStart + ASelLength, Length(AText));
    Delete(AText, ASelStart, Length(AText));
  end
  else
  begin
    SelectionText := '';
    AfterText := '';
  end;

  OldTextColor := ACanvas.Font.Color;

  if AText <> '' then
  begin
    ACanvas.Font.Color := AStyle.TextColor;
    DrawText(ACanvas, ARect, AText, AFormat, StyleModifiers);
  end;

  if SelectionText <> '' then
  begin
    SelectionRect := ARect;
    SelectionRect.Left := SelectionRect.Left + ACanvas.TextWidth(AText);
    SelectionRect.Right := SelectionRect.Left + ACanvas.TextWidth(SelectionText);
    InflateRect(SelectionRect, ASpacing div 2, ASpacing div 2);

    OldBrushColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := AStyle.TextColor;
    ACanvas.FillRect(SelectionRect);
    ACanvas.Brush.Color := OldBrushColor;
    ACanvas.Font.Color := AStyle.FillColor;
    InflateRect(SelectionRect, -ASpacing div 2, -ASpacing div 2);
    DrawText(ACanvas, SelectionRect, SelectionText, AFormat, StyleModifiers);
    ARect.Left := SelectionRect.Right;
  end;

  if AfterText <> '' then
  begin
    ACanvas.Font.Color := AStyle.TextColor;
    DrawText(ACanvas, ARect, AfterText, AFormat, StyleModifiers);
  end;
  ACanvas.Font.Color := OldTextColor;
end;

class function TfcxCustomGridPainter.DrawText(ACanvas: TCanvas; var ARect: TRect;
  AText: TfcxString; AFormat: DWord; StyleModifiers: TFontStyles = []): Integer;
var
  P: PfcxChar;
  OldStyle: TFontStyles;
  OldBkMode: Integer;
begin
  if (DT_CALCRECT and AFormat) = 0 then
    OldBkMode := SetBkMode(ACanvas.Handle, TRANSPARENT)
  else
    OldBkMode := TRANSPARENT; // to reduce warnings
  OldStyle := ACanvas.Font.Style;
  ACanvas.Font.Style := ACanvas.Font.Style + StyleModifiers;
  GetMem(P, (Length(AText) + 5) * SizeOf(TfcxChar)); // +4 because of DrawText + DT_CALCRECT requirement
  Move(PfcxChar(AText)^, P^, (Length(AText) + 1) * SizeOf(TfcxChar));
  Result := fcxDrawText(ACanvas.Handle, P, Length(AText), ARect, AFormat);
  FreeMem(P);
  ACanvas.Font.Style := OldStyle;
  if (DT_CALCRECT and AFormat) = 0 then
    SetBkMode(ACanvas.Handle, OldBkMode);
end;

procedure TfcxCustomGridPainter.DrawToolButton(ACanvas: TCanvas;
  ARect: TRect; AState: TfcxThemeState);
begin
  if AState <> tsNormal then
    DrawButton(ACanvas, ARect, AState);
end;

function TfcxCustomGridPainter.GetBitmap(AIndex: Integer): TBitmap;
begin
  if FBitmaps[AIndex] = nil then
    FBitmaps[AIndex] := LoadBitmapRes('fcxGRIDBmp_' + GetPostfix + '_' + IntToStr(AIndex));
  Result := FBitmaps[AIndex];
end;

function TfcxCustomGridPainter.GetButtonContent(ACanvas: TCanvas;
  ARect: TRect; AState: TfcxThemeState): TRect;
begin
  Result := ARect;
  InflateRect(Result, -2, -2);
end;

function TfcxCustomGridPainter.GetCheckRect(ARect: TRect): TRect;
begin
  Result := ARect;
  InflateRect(Result, -2, -2);
  Result.Right := Result.Left + Result.Bottom - Result.Top;
end;

function TfcxCustomGridPainter.GetDropDownButtonRect(ARect: TRect; Options: TfcxThemeButtonOptions): TRect;
const
  MaxSize = 16;
begin
  Result := ARect;
  if Result.Right - Result.Left < ItemButtonMinTextRect then
  begin
    Result := Rect(-1, -1, -1, -1);
    Exit;
  end;
  InflateRect(Result, -1, -1);
  if Result.Bottom - Result.Top > MaxSize then
  begin
    Result.Top := (Result.Top + Result.Bottom - MaxSize) div 2;
    Result.Bottom := Result.Top + MaxSize;
  end;
  Result.Left := Result.Right - Result.Bottom + Result.Top;
end;

function TfcxCustomGridPainter.GetItemButtonThemeSpacing: TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
end;

class function TfcxCustomGridPainter.GetPostfix: String;
begin
  Result := ''
end;

function TfcxCustomGridPainter.GetScaleSliderButtonSize: Integer;
begin
  Result := 20;
end;

function TfcxCustomGridPainter.GetScrollButtonSize: Integer;
begin
  Result := 10;
end;

function TfcxCustomGridPainter.GetSizeGripRect(ARect: TRect): TRect;
begin
  Result.BottomRight := ARect.BottomRight;
  dec(Result.Bottom, 2);
  dec(Result.Right, 2);
  Result.TopLeft := Result.BottomRight;
  dec(Result.Top, 11);
  dec(Result.Left, 11);
end;

function TfcxCustomGridPainter.GetUtilImages: TImageList;
begin
  if FUtilImages.Count = 0 then
    LoadHiResImages(FUtilImages, 10, 'FCXUTILS_' + GetPostfix);

  Result := FUtilImages;
end;

function TfcxCustomGridPainter.MeasureAxisCell(ACanvas: TCanvas;
  ASize: TSize; AText: TfcxString; ACellProperties: TfcxPropertiesOfCellAxis;
  AAlignment: TAlignment; AState: TfcxThemeState;
  AStyle: TfcxCustomThemeStyle; AScale: Integer; GrowInHeight: Boolean; AImages: TCustomImageList;
  AImageIndex: Integer): TSize;
const
  Spacing = 3;  
var
  R: TRect;
  AWidth, AHeight, ALineHeight: integer;
begin
  // Result := MeasureAxisItem(ACanvas, AState, AStyle);
  Result.cx := 0;
  Result.cy := 0;
  ACanvas.Font.Assign(AStyle.Font);
  ACanvas.Font.Color := AStyle.TextColor;
  RescaleFont(ACanvas, AScale);

  // add border spacing
  inc(Result.cx, Spacing);

  // add text spacing
  inc(Result.cx, Spacing * 2);
  if (ACellProperties * [pca_Expanded, pca_Collapsed]) <> [] then // has plus/minus
    inc(Result.cx, TreeButtonSize + Spacing);

  if (ACellProperties * [pca_HExpanded, pca_HCollapsed, pca_GExpanded, pca_GCollapsed]) <> [] then // has hierarchi plus/minus
    inc(Result.cx, TreeButtonSize + Spacing);

  if (pca_Sort in ACellProperties) then
    inc(Result.cx, SortArrowSize + Spacing);

  if Assigned(AImages) and (AImageIndex <> -1) then
  begin
    inc(Result.cx, MulDiv(AImages.Width, AScale, 100) + Spacing);
    Result.cy := MulDiv(AImages.Height, AScale, 100);
  end;

  if GrowInHeight then
  begin
    R := Rect(0, 0, ASize.cx - Result.cx, ASize.cy);
    if Trim(AText) = '' then
      AText := 'X';
    DrawText(ACanvas, R, Trim(AText), DT_NOPREFIX or DT_WORDBREAK or AlignmentFormat[AAlignment] or DT_CALCRECT or DT_NOCLIP);
  end
  else
  begin
    R := Rect(0, 0, ASize.cx - Result.cx, ASize.cy);
// todo надо что-то придумать, что-бы ширина считалась с учётом доступной для переноса высоты
// если DrawText не позволяет это сделать напрямую, значит надо делать подбором...
// сперва проверим, может хватит имеющейся ширины
    DrawText(ACanvas, R, Trim(AText), DT_NOPREFIX or DT_WORDBREAK or AlignmentFormat[AAlignment] or DT_CALCRECT or DT_NOCLIP);
    if Max(R.Bottom - R.Top, Result.cy) > ASize.cy then
    begin
// не хватило. увеличим ширину в зависимоcти от увеличения высоты.
// узнаем высоту строки
      AHeight := Max(R.Bottom - R.Top, Result.cy);
      AWidth := R.Right - R.Left;
      R := Rect(0, 0, Screen.Width, Screen.Height);
      ALineHeight := DrawText(ACanvas, R, Trim(AText), DT_NOPREFIX or AlignmentFormat[AAlignment] or DT_CALCRECT or DT_NOCLIP or DT_SINGLELINE	);
      R := Rect(0, 0, Trunc(AWidth * AHeight / ALineHeight), ASize.cy);
      DrawText(ACanvas, R, Trim(AText), DT_NOPREFIX or DT_WORDBREAK or AlignmentFormat[AAlignment] or DT_CALCRECT or DT_NOCLIP);
    end;
  end;

  inc(Result.cx, R.Right - R.Left);
  Result.cy := Max(R.Bottom - R.Top, Result.cy) + Spacing * 2; // 4 = border spacing + text spacing
end;

procedure TfcxCustomGridPainter.RescaleFont(ACanvas: TCanvas; AScale: Integer);
begin
  ACanvas.Font.Height := MulDiv(ACanvas.Font.Height, AScale, 100); 
end;

class function TfcxCustomGridPainter.SetClipRegion(ADC: HDC; ARect: TRect): HRGN;
var
  Offset: TPoint;
begin
  GetWindowOrgEx(ADC, Offset);
  OffsetRect(ARect, -Offset.x, -Offset.y);
  Result := CreateRectRgnIndirect(ARect);
  SelectClipRgn(ADC, Result);
end;

{ TfcxStandardGridPainter }

procedure TfcxStandardGridPainter.DrawAxisItem(ACanvas: TCanvas;
  ARect: TRect; AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle);
const
  ButtonStyle: array[TfcxThemeState] of DWord = (
 { tsNormal   } 0,
 { tsHot      } DFCS_HOT,
 { tsPressed  } DFCS_PUSHED,
 { tsDisabled } DFCS_INACTIVE
  );
begin
  DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH or ButtonStyle[AState]);
  InflateRect(ARect, -2, -2);
  DrawStyledBackground(ACanvas, ARect, AStyle);
end;

procedure TfcxStandardGridPainter.DrawBorder(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState);
{$IFNDEF FPC}
var
  TopColor, BottomColor: TColor;
{$ENDIF}
begin
{$IFNDEF FPC}
  TopColor := clBtnShadow;
  BottomColor := clBtnFace;
  Frame3D(ACanvas, ARect, TopColor, BottomColor, 1);
{$ELSE}
  ACanvas.Frame3d(ARect, 1, bvLowered);
{$ENDIF}
end;

procedure TfcxStandardGridPainter.DrawDropDownButton(ACanvas: TCanvas;
  AStyle: TfcxCustomThemeStyle; ARect: TRect;
  Options: TfcxThemeButtonOptions; ASortType: TfcxThemeSortType; AState: TfcxThemeState);
var
  P: TPoint;
begin
  if AState <> tsPressed then
    Frame3D(ACanvas, ARect, clBtnHighlight, clBtnShadow, 1)
  else
    Frame3D(ACanvas, ARect, clBtnShadow, clBtnShadow, 1);
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := clBlack;

  if AState > tsNormal then
  begin
    if AState = tsPressed then
      ACanvas.Brush.Color := clBtnFace
    else
      ACanvas.Brush.Color := clBtnHighlight;

    ACanvas.FillRect(Arect);
    ACanvas.Pen.Color := clBlack;
    ACanvas.Brush.Color := clBlack;
  end;

  if AState = tsPressed then
  begin
    inc(ARect.Left);
    inc(ARect.Top);
  end;

  p.x := (ARect.Left + ARect.Right) div 2;
  p.y := (ARect.Top + ARect.Bottom) div 2;

  ACanvas.Polygon([Point(p.x - 3, p.y - 2),
                   Point(p.x + 3, p.y - 2),
                   Point(p.x + 0, p.y + 1)]);
end;

procedure TfcxStandardGridPainter.DrawHeader(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AState: TfcxThemeState);
var
  Rgn: HRgn;
  OldColor: TColor;
begin
  DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH);
  InflateRect(ARect, -1, -1);
  OldColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(ARect);

  RGN := SetClipRegion(ACanvas.Handle, ARect);

  DrawText(ACanvas, ARect, AText, DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_MODIFYSTRING or DT_END_ELLIPSIS);
  SelectClipRgn(ACanvas.Handle, 0);
  DeleteObject(Rgn);
  ACanvas.Brush.Color := OldColor;
end;

procedure TfcxStandardGridPainter.DrawItemButton(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AImageList: TCustomImageList; AImageIndex: Integer;
  Options: TfcxThemeButtonOptions; ASortType: TfcxThemeSortType;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN);
var
  Rgn: HRgn;
  ATextRect, ADropRect: TRect;
  Style: TFontStyles;
  P: TPoint;
begin
  if tboHasDropDownButton in Options then
    ADropRect := GetDropDownButtonRect(ARect, Options);
    
  DrawFrameControl(ACanvas.Handle, ARect, DFC_BUTTON, DFCS_BUTTONPUSH or ThemeStateToFrameControlState[AState]);
  if MergeWithRegion <> 0 then
  begin
    Rgn := CreateRectRgnIndirect(ARect);
    CombineRgn(MergeWithRegion, MergeWithRegion, Rgn, RGN_OR);
    DeleteObject(Rgn);
  end;
  InflateRect(ARect, -1, -1);
  DrawStyledBackground(ACanvas, ARect, AStyle);
  ACanvas.Font.Assign(AStyle.Font);
  ACanvas.Font.Color := AStyle.TextColor;

  Rgn := SetClipRegion(ACanvas.Handle, ARect);

  ATextRect := ARect;
  inflateRect(ATextRect, -1, -1);
  inc(ATextRect.Left);

  if (AImageIndex <> -1) and (AImageList <> nil) then
  begin
    AImageList.Draw(ACanvas, ATextRect.Left,
      Max(0, ATextRect.Top + ATextRect.Bottom - AImageList.Height) div 2,
      AImageIndex);
    inc(ATextRect.Left, AImageList.Width + 2);
  end;

  if (tboHasDropDownButton in Options) and not IsRectEmpty(ADropRect) then
    ATextRect.Right := ADropRect.Left - 1;
  
  if (tboHasSortArrow in Options) and ((ATextRect.Right - ATextRect.Left) > ItemButtonMinTextRect) then
  begin
    P.X := (ATextRect.Right - SortArrowSize);
    P.Y := (ATextRect.Bottom + ATextRect.Top - SortArrowSize) div 2 + Ord(Odd(SortArrowSize));
    DrawSortArrow(ACanvas, AStyle, P, ASortType);
    ATextRect.Right := P.X;
  end;

  if AText <> '' then
  begin
    if tboHasFilteredValues in Options then
      Style := [fsItalic]
    else
      Style := [];
    DrawText(ACanvas, ATextRect, AText, DT_VCENTER or DT_LEFT or DT_SINGLELINE or DT_MODIFYSTRING or DT_END_ELLIPSIS, Style);
  end;

  SelectClipRgn(ACanvas.Handle, 0);
  DeleteObject(Rgn);
end;

procedure TfcxStandardGridPainter.DrawSortArrow(ACanvas: TCanvas;
  AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType);
var
  p: TPoint;
  OldPenColor, OldBrushColor: TColor;
begin
  p.x := APosition.X + SortArrowSize div 2;
  p.y := APosition.Y + SortArrowSize div 2;

  OldPenColor := ACanvas.Pen.Color;
  OldBrushColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := GetShadowColor(AStyle.FillColor);
  ACanvas.Pen.Color := ACanvas.Brush.Color;

  if AArrowType in [tstUp, tstDown] then
    DoDrawArrow(ACanvas, P, AArrowType)
  else
  begin
    dec(p.y, 1);
    DoDrawArrow(ACanvas, P, tstUp);
    inc(p.y, 4);
    DoDrawArrow(ACanvas, P, tstDown);
  end;

  ACanvas.Pen.Color := OldPenColor;
  ACanvas.Brush.Color := OldBrushColor;
end;

procedure TfcxStandardGridPainter.DrawTreeButton(ACanvas: TCanvas;
  APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean);
var
  OldPenColor, OldBrushColor: TColor;  
begin
  OldPenColor := ACanvas.Pen.Color;
  OldBrushColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := AStyle.FillColor;
  if ASelected then
    ACanvas.Pen.Color := GetHighlightColor(AStyle.FillColor {$IFDEF DELPHI_7UP}, 38 {$ENDIF})
  else
    ACanvas.Pen.Color := GetShadowColor(AStyle.FillColor {$IFDEF DELPHI_7UP}, -100 {$ENDIF});
  ACanvas.Rectangle(APosition.X, APosition.Y, APosition.X + TreeButtonSize, APosition.Y + TreeButtonSize);
  if AKind in [tbkPlusButton, tbkMinusButton] then
  begin
    ACanvas.MoveTo(APosition.X + 2, APosition.Y + TreeButtonSize div 2);
    ACanvas.LineTo(APosition.X + TreeButtonSize - 2, APosition.Y + TreeButtonSize div 2);
  end;  
  if AKind = tbkPlusButton then
  begin
    ACanvas.MoveTo(APosition.X + TreeButtonSize div 2, APosition.Y + 2);
    ACanvas.LineTo(APosition.X + TreeButtonSize div 2, APosition.Y + TreeButtonSize - 2);
  end;
  if AKind in [tbkTreeButtonPlus, tbkTreeButtonMinus] then
  begin
    ACanvas.MoveTo(APosition.X + 2, APosition.Y + 2);
    ACanvas.LineTo(APosition.X + 2, APosition.Y + TreeButtonSize - 2);
    if AKind = tbkTreeButtonPlus then
    begin
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + 3);
      ACanvas.LineTo(APosition.X + 5, APosition.Y + 3);
      ACanvas.Rectangle(APosition.X + 5, APosition.Y + 2, APosition.X + 7, APosition.Y + 4);
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + TreeButtonSize - 3);
      ACanvas.LineTo(APosition.X + 4, APosition.Y + TreeButtonSize - 3);
      ACanvas.Rectangle(APosition.X + 4, APosition.Y + TreeButtonSize - 4, APosition.X + 6, APosition.Y + TreeButtonSize - 2);
    end
    else
    begin
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + 3);
      ACanvas.LineTo(APosition.X + 4, APosition.Y + 3);
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + TreeButtonSize - 3);
      ACanvas.LineTo(APosition.X + 4, APosition.Y + TreeButtonSize - 3);

      ACanvas.MoveTo(APosition.X + 5, APosition.Y + 3);
      ACanvas.LineTo(APosition.X + 7, APosition.Y + 3);
      ACanvas.MoveTo(APosition.X + 5, APosition.Y + TreeButtonSize - 3);
      ACanvas.LineTo(APosition.X + 7, APosition.Y + TreeButtonSize - 3);
    end;
  end;
  ACanvas.Pen.Color := OldPenColor;
  ACanvas.Brush.Color := OldBrushColor;
end;

class function TfcxStandardGridPainter.GetPaintStyle: TfcxPaintStyle;
begin
  Result := psStandard;
end;

class function TfcxStandardGridPainter.GetPostfix: String;
begin
  Result := 'STD';
end;

function TfcxStandardGridPainter.MeasureAxisItem(ACanvas: TCanvas;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize;
begin
  Result.cx := 2;
  Result.cy := 2;
end;

{ TfcxXPGridPainter }

constructor TfcxXPGridPainter.Create;
begin
  inherited;
  FThemesEnabled := GetThemeEnabled;
  FSystemStyle := GetSystemStyle;
  FStdPainter := TfcxStandardGridPainter.Create
end;

destructor TfcxXPGridPainter.Destroy;
begin
  FStdPainter.Free;
  inherited;
end;

procedure TfcxXPGridPainter.DrawAxisItem(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle);
{$IFDEF USE_THEMES}
var
  Detail: TThemedHeader;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := thHeaderItemNormal;
    if AState <> tsDisabled then
      inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    FStdPainter.DrawAxisItem(ACanvas, ARect, AState, AStyle);
end;

procedure TfcxXPGridPainter.DrawBorder(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState);
{$IFDEF USE_THEMES}
var
  Details: TThemedElementDetails;
  SavedDC: Integer;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    SavedDC := SaveDC(ACanvas.Handle);
    with ARect do
      ExcludeClipRect(ACanvas.Handle, Left + 1, Top + 1, Right - 1, Bottom - 1);
    Details := ThemeServices.GetElementDetails(teEditTextNormal);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
    RestoreDC(ACanvas.Handle, SavedDC);
  end
  else
{$ENDIF}
    FStdPainter.DrawBorder(ACanvas, ARect, AState);
end;

procedure TfcxXPGridPainter.DrawButton(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState);
{$IFDEF USE_THEMES}
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := tbPushButtonNormal;
    inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    inherited;
end;

procedure TfcxXPGridPainter.DrawCheck(ACanvas: TCanvas; ARect: TRect;
  ACheckState: TCheckBoxState; AState: TfcxThemeState);
{$IFDEF USE_THEMES}
const
  CheckStateToTheme: array[TCheckBoxState] of TThemedButton = (
 { cbUnchecked } tbCheckBoxUncheckedNormal,
 { cbChecked   } tbCheckBoxCheckedNormal,
 { cbGrayed    } tbCheckBoxMixedNormal
  );
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := CheckStateToTheme[ACheckState];
    inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    FStdPainter.DrawCheck(ACanvas, ARect, ACheckState, AState);
end;

procedure TfcxXPGridPainter.DrawDropDownButton(ACanvas: TCanvas;
  AStyle: TfcxCustomThemeStyle; ARect: TRect;
  Options: TfcxThemeButtonOptions; ASortType: TfcxThemeSortType; AState: TfcxThemeState);
{$IFDEF USE_THEMES}
const
  MainIndex: array[Boolean] of Integer = (bmpDropDown, bmpFilterDown);
  Spacing = 2;
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
  X, Y: Integer;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    if tboHasDropDownButton in Options then
    begin
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(ARect.Left, ARect.Top + 3);
      ACanvas.LineTo(ARect.Left, ARect.Bottom - 3);
      Detail := tbPushButtonNormal;
      inc(Detail, Ord(AState));
      Details := ThemeServices.GetElementDetails(Detail);
      ARect := ThemeServices.ContentRect(ACanvas.Handle, Details, ARect);
      // draw icons
      if tboHasSortArrow in Options then
      begin
        X := ARect.Right - UtilImages.Width - 2;
        Y := (ARect.Top + ARect.Bottom - UtilImages.Height) div 2;
        UtilImages.Draw(ACanvas, X, Y, MainIndex[tboHasFilteredValues in Options]);
        DrawSortArrow(ACanvas, AStyle, Point(X - UtilImages.Width + 1, Y), ASortType);
      end
      else
      begin
        X := (ARect.Left + ARect.Right - UtilImages.Width) div 2;
        Y := (ARect.Top + ARect.Bottom - UtilImages.Height) div 2;
        UtilImages.Draw(ACanvas, X, Y, MainIndex[tboHasFilteredValues in Options]);
      end;
    end;
  end
  else
{$ENDIF}
    FStdPainter.DrawDropDownButton(ACanvas, AStyle, ARect, Options, ASortType, AState);
end;

procedure TfcxXPGridPainter.DrawHeader(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AState: TfcxThemeState);
{$IFDEF USE_THEMES}
var
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tebHeaderBackgroundNormal);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
    if AText <> '' then
      ThemeServices.DrawText(ACanvas.Handle, Details, AText,
        ARect, DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_MODIFYSTRING or DT_END_ELLIPSIS, 0);
  end
  else
{$ENDIF}
    FStdPainter.DrawHeader(ACanvas, ARect, AText, AState);
end;

procedure TfcxXPGridPainter.DrawItemButton(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AImageList: TCustomImageList; AImageIndex: Integer;
  Options: TfcxThemeButtonOptions; ASortType: TfcxThemeSortType;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN);
{$IFDEF USE_THEMES}
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
  R: TRect;
  ATextRect, ADropRect: TRect;
  Rgn: HRGN;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    if Options <> [] then
      ADropRect := GetDropDownButtonRect(ARect, Options);
    Detail := tbPushButtonNormal;
    inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);

    if MergeWithRegion <> 0 then
    begin
      {$ifndef fpc}
      {$ifdef Delphi_16UP}
      if not StyleServices.GetElementRegion(ACanvas.Handle, Details, ARect, Rgn) then
        Rgn := 0;
      {$else}
      Rgn := 0;
      GetThemeBackgroundRegion(ThemeServices.Theme[Details.Element], ACanvas.Handle, Details.Part, Details.State, ARect, Rgn);
      {$endif}
      {$else}
      Rgn := ThemeServices.GetDetailRegion(ACanvas.Handle, Details, ARect);
      {$endif}
      if Rgn = 0 then
        with ARect do
          Rgn := CreateRectRgn(Left, Top, Right, Bottom);
      CombineRgn(MergeWithRegion, MergeWithRegion, Rgn, RGN_OR);
      DeleteObject(Rgn);
    end;

    ARect := ThemeServices.ContentRect(ACanvas.Handle, Details, ARect);

    ATextRect := ARect;
    inflateRect(ATextRect, -1, -1);
    inc(ATextRect.Left);

    if (AImageIndex <> -1) and (AImageList <> nil) then
    begin
      R.Left := ATextRect.Left;
      R.Top := Max(0, ATextRect.Top + ATextRect.Bottom - AImageList.Height) div 2;
      R.Right := R.Left + AImageList.Width;
      R.Bottom := R.Top + AImageList.Height;
      ThemeServices.DrawIcon(ACanvas.Handle, Details, R, AImageList.Handle, AImageIndex);
      inc(ATextRect.Left, AImageList.Width + 2);
    end;

    if (Options <> []) and not IsRectEmpty(ADropRect)  then
      ATextRect.Right := ADropRect.Left - 1;

    if AText <> '' then
    begin
      ACanvas.Font.Assign(AStyle.Font);
      ACanvas.Font.Color := clBtnText;//AStyle.TextColor;
      DrawText(ACanvas, ATextRect, AText, DT_VCENTER or DT_LEFT or DT_SINGLELINE or DT_MODIFYSTRING or DT_END_ELLIPSIS, []);
    end;
  end
  else
{$ENDIF}
    FStdPainter.DrawItemButton(ACanvas, ARect, AText, AImageList, AImageIndex, Options, ASortType, AState, AStyle, MergeWithRegion);
end;

procedure TfcxXPGridPainter.DrawRadio(ACanvas: TCanvas; ARect: TRect;
  AChecked: Boolean; AState: TfcxThemeState);
{$IFDEF USE_THEMES}
const
  CheckedToTheme: array[Boolean] of TThemedButton = (
 { cbUnchecked } tbRadioButtonUncheckedNormal,
 { cbChecked   } tbRadioButtonCheckedNormal
  );
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := CheckedToTheme[AChecked];
    inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    FStdPainter.DrawRadio(ACanvas, ARect, AChecked, AState);
end;

procedure TfcxXPGridPainter.DrawStatus(ACanvas: TCanvas; var ARect: TRect; AStyle: TfcxCustomThemeStyle);
{$IFDEF USE_THEMES}
var
  Details: TThemedElementDetails;
  Result: TRect;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tsStatusRoot);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
    Result := ThemeServices.ContentRect(ACanvas.Handle, Details, ARect);
    if not IsRectEmpty(Result) then
      ARect := Result;
  end
  else
{$ENDIF}
    inherited DrawStatus(ACanvas, ARect, AStyle);
end;

function TfcxXPGridPainter.GetSizeGripRect(ARect: TRect): TRect;
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    // TODO: use theme metrics
    Result := ARect;
    with Result do
    begin
      Dec(Right);
      Left := Right - 14; // -16 but we report less to draw more compact
    end;
  end
  else
{$ENDIF}
    Result := inherited GetSizeGripRect(ARect);
end;

class function TfcxXPGridPainter.GetSystemStyle: Boolean;
begin
{$IFDEF USE_THEMES}
  {$IFDEF Delphi_16UP}
    Result := ThemeServices.IsSystemStyle;
  {$ELSE}
    Result := True;
  {$ENDIF}
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TfcxXPGridPainter.DrawSizeGrip(ACanvas: TCanvas; ARect: TRect);
{$IFDEF USE_THEMES}
var
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tsGripper);
    ARect.Left := ARect.Left - 2;
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    inherited DrawSizeGrip(ACanvas, ARect);
end;

procedure TfcxXPGridPainter.DrawSortArrow(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType);
const
  ArrowIndex : array[TfcxThemeSortType] of Integer = (bmpSortArrowUp, bmpSortArrowDown, bmpSortArrowUpDown);
begin
  if FThemesEnabled then
    UtilImages.Draw(ACanvas, APosition.X, APosition.Y, ArrowIndex[AArrowType])
  else
    FStdPainter.DrawSortArrow(ACanvas, AStyle, APosition, AArrowType);
end;

procedure TfcxXPGridPainter.DrawTreeButton(ACanvas: TCanvas;
  APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean);
{$IFDEF USE_THEMES}
const
  ExpandIndex : array[TfcxTreeButtonKind] of TThemedTreeview = (ttGlyphClosed, ttGlyphOpened, ttGlyphClosed, ttGlyphOpened);
  ImageIndex: array[TfcxTreeButtonKind] of Integer = (1, 0, 2, 3);
var
  Details: TThemedElementDetails;
  R: TRect;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    if FSystemStyle and (AKind in [tbkPlusButton, tbkMinusButton]) then
    begin
      R.TopLeft := APosition;
      R.BottomRight := Point(APosition.X + TreeButtonSize, APosition.Y + TreeButtonSize);
      Details := ThemeServices.GetElementDetails(ExpandIndex[AKind]);
      ThemeServices.DrawElement(ACanvas.Handle, Details, R);
    end
    else
      UtilImages.Draw(ACanvas, APosition.X, APosition.Y, ImageIndex[AKind]);
  end
  else
{$ENDIF}
    FStdPainter.DrawTreeButton(ACanvas, APosition, AKind, AStyle, ASelected);
end;

class function TfcxXPGridPainter.GetPaintStyle: TfcxPaintStyle;
begin
  Result := psXP;
end;

class function TfcxXPGridPainter.GetPostfix: String;
begin
  if GetThemeEnabled then
    Result := 'XP'
  else
    Result := 'STD';
end;

class function TfcxXPGridPainter.GetThemeEnabled: Boolean;
begin
{$IFDEF USE_THEMES}
  Result := ThemeServices.ThemesEnabled;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TfcxXPGridPainter.GetScrollButtonSize: Integer;
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
    Result := 10
  else
{$ENDIF}
    Result := inherited GetScrollButtonSize;
end;

procedure TfcxXPGridPainter.DrawScrollButton(ACanvas: TCanvas; ARect: TRect;
  ADirection: TfcxThemeDirection; AState: TfcxThemeState);
{$IFDEF USE_THEMES}
const
  DirectionToDetail: array[TfcxThemeDirection] of TThemedSpin = (
 { tdLeft  } tsUpHorzNormal,
 { tdRight } tsDownHorzNormal,
 { tdUp    } tsUpNormal,
 { tdDown  } tsDownNormal
  );
var
  Detail: TThemedSpin;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := DirectionToDetail[ADirection];
    inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    if ThemeServices.HasTransparentParts(Details) then
      ACanvas.FillRect(ARect);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    inherited;
end;

function TfcxXPGridPainter.MeasureAxisItem(ACanvas: TCanvas;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize;
{$IFDEF USE_THEMES}
var
  Detail: TThemedHeader;
  Details: TThemedElementDetails;
  R: TRect;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := thHeaderItemNormal;
    if AState <> tsDisabled then
      inc(Detail, Ord(AState));
    R := Rect(0, 0, 20, 20);
    Details := ThemeServices.GetElementDetails(Detail);
    with Result, ThemeServices.ContentRect(ACanvas.Handle, Details, R) do
    begin
      cx := Right - Left;
      cy := Bottom - Top;
    end;
  end
  else
{$ENDIF}
    FStdPainter.MeasureAxisItem(ACanvas, AState, AStyle);
end;

function TfcxXPGridPainter.GetDropDownButtonRect(ARect: TRect; Options: TfcxThemeButtonOptions): TRect;
const
  MaxSize = 20;
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Result := ARect;
    if Result.Right - Result.Left < ItemButtonMinTextRect then
    begin
      Result := Rect(-1, -1, -1, -1);
      Exit;
    end;
    if Result.Bottom - Result.Top > MaxSize then
    begin
      Result.Top := (Result.Top + Result.Bottom - MaxSize) div 2;
      Result.Bottom := Result.Top + MaxSize;
    end;
    Result.Left := Result.Right - UtilImages.Width * (1 + Ord(tboHasSortArrow in Options)) - GetItemButtonThemeSpacing.cx * 2 - Ord(not (tboHasSortArrow in Options));
  end
  else
{$ENDIF}
    Result := FStdPainter.GetDropDownButtonRect(ARect, Options);
end;

function TfcxXPGridPainter.GetItemButtonThemeSpacing: TSize;
begin
  Result.cx := 2;
  Result.cy := 2;
end;

procedure TfcxXPGridPainter.DrawDataCellFrame(ACanvas: TCanvas; ARect: TRect);
const
  BevelColor = $E5D7D0;
begin
  BevelLine(ACanvas, BevelColor, ARect.Left, ARect.Bottom - 1, ARect.Right, ARect.Bottom - 1);
  BevelLine(ACanvas, BevelColor, ARect.Right - 1, ARect.Top, ARect.Right - 1, ARect.Bottom);
end;

procedure TfcxXPGridPainter.DrawStatusPane(ACanvas: TCanvas; ARect: TRect);
{$IFDEF USE_THEMES}
var
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Details := ThemeServices.GetElementDetails(tsPane);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    inherited DrawStatusPane(ACanvas, ARect);
end;

procedure TfcxXPGridPainter.DrawToolButton(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState);
{$IFDEF USE_THEMES}
var
  Detail: TThemedToolBar;
  Details: TThemedElementDetails;
{$ENDIF}
begin
{$IFDEF USE_THEMES}
  if FThemesEnabled then
  begin
    Detail := ttbButtonNormal;
    inc(Detail, Ord(AState));
    Details := ThemeServices.GetElementDetails(Detail);
    ThemeServices.DrawElement(ACanvas.Handle, Details, ARect);
  end
  else
{$ENDIF}
    inherited;
end;

{ TfcxFlatGridPainter }

procedure TfcxFlatGridPainter.DrawAxisItem(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle);
begin
  DrawEdge(ACanvas.Handle, ARect, EDGE_RAISED, BF_RECT or BF_SOFT);
//  DrawEdge(ACanvas.Handle, ARect, EDGE_RAISED, BF_RECT);
  InflateRect(ARect, -1, -1);
  DrawStyledBackground(ACanvas, ARect, AStyle);
end;

procedure TfcxFlatGridPainter.DrawBorder(ACanvas: TCanvas; ARect: TRect;
  AState: TfcxThemeState);
begin
  Frame3D(ACanvas, ARect, clBtnShadow, clBtnShadow, 1);
end;

procedure TfcxFlatGridPainter.DrawButton(ACanvas: TCanvas; ARect: TRect; AState: TfcxThemeState);
begin
  case AState of
    tsHot:
      DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
    tsPressed:
      DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT);
  end;
end;

procedure TfcxFlatGridPainter.DrawDropDownButton(ACanvas: TCanvas;
  AStyle: TfcxCustomThemeStyle; ARect: TRect;
  Options: TfcxThemeButtonOptions; ASortType: TfcxThemeSortType; AState: TfcxThemeState);
var
  P: TPoint;
begin
  // draw frame
  if AState in [tsHot,tsPressed] then
  begin
    Frame3D(ACanvas, ARect, clGray, clGray, 1);
    ACanvas.FillRect(ARect);
  end;

  // draw arrow
  ACanvas.Pen.Color := clWhite;
  ACanvas.Brush.Color := clWhite;

  if AState > tsNormal then
  begin
    if AState = tsPressed then
      ACanvas.Brush.Color := clGray
    else
      ACanvas.Brush.Color := clSilver;

    ACanvas.FillRect(Arect);
    if AState = tsPressed then
    begin
      ACanvas.Pen.Color := clSilver;
      ACanvas.Brush.Color := clSilver;
    end
    else
    begin
      ACanvas.Pen.Color := clWhite;
      ACanvas.Brush.Color := clWhite;
    end;
  end;

  p.x := (ARect.Left + ARect.Right) div 2;
  p.y := (ARect.Top + ARect.Bottom) div 2;

  ACanvas.Polygon([Point(p.x - 3, p.y - 2),
                   Point(p.x + 3, p.y - 2),
                   Point(p.x + 0, p.y + 1)]);
end;

procedure TfcxFlatGridPainter.DrawHeader(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AState: TfcxThemeState);
var
  Rgn: HRgn;
  OldColor: TColor;
begin
  DrawEdge(ACanvas.Handle, ARect, EDGE_BUMP, BF_FLAT or BF_RECT);
  InflateRect(ARect, -1, -1);
  OldColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := clBtnShadow;
  ACanvas.FillRect(ARect);

  RGN := SetClipRegion(ACanvas.Handle, ARect);
  DrawText(ACanvas, ARect, AText, DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_MODIFYSTRING or DT_END_ELLIPSIS);
  SelectClipRgn(ACanvas.Handle, 0);
  DeleteObject(Rgn);
  ACanvas.Brush.Color := OldColor;
end;

procedure TfcxFlatGridPainter.DrawItemButton(ACanvas: TCanvas; ARect: TRect;
  AText: TfcxString; AImageList: TCustomImageList; AImageIndex: Integer;
  Options: TfcxThemeButtonOptions; ASortType: TfcxThemeSortType;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle; MergeWithRegion: HRGN);
var
  Rgn: HRgn;
  ATextRect, ADropRect: TRect;
  P: TPoint;
  Style: TFontStyles;
begin
  if tboHasDropDownButton in Options then
    ADropRect := GetDropDownButtonRect(ARect, Options);
  DrawEdge(ACanvas.Handle, ARect, EDGE_BUMP, BF_FLAT or BF_RECT);
  if MergeWithRegion <> 0 then
  begin
    Rgn := CreateRectRgnIndirect(ARect);
    CombineRgn(MergeWithRegion, MergeWithRegion, Rgn, RGN_OR);
    DeleteObject(Rgn);
  end;
  InflateRect(ARect, -1, -1);
  DrawStyledBackground(ACanvas, ARect, AStyle);
  ACanvas.Font.Assign(AStyle.Font);
  ACanvas.Font.Color := AStyle.TextColor;

  RGN := SetClipRegion(ACanvas.Handle, ARect);

  ATextRect := ARect;
  inflateRect(ATextRect, -1, -1);
  inc(ATextRect.Left);

  if (AImageIndex <> -1) and (AImageList <> nil) then
  begin
    AImageList.Draw(ACanvas, ATextRect.Left,
      Max(0, ATextRect.Top + ATextRect.Bottom - AImageList.Height) div 2,
      AImageIndex);
    inc(ATextRect.Left, AImageList.Width + 2);
  end;

  if (tboHasDropDownButton in Options) and not IsRectEmpty(ADropRect) then
    ATextRect.Right := ADropRect.Left - 1;

  if (tboHasSortArrow in Options) and ((ATextRect.Right - ATextRect.Left) > ItemButtonMinTextRect) then
  begin
    P.X := (ATextRect.Right - 3 - SortArrowSize);
    P.Y := (ATextRect.Bottom + ATextRect.Top - SortArrowSize) div 2 + Ord(Odd(SortArrowSize));
    DrawSortArrow(ACanvas, AStyle, P, ASortType);
    ATextRect.Right := P.X;
  end;
  
  if AText <> '' then
  begin
    if tboHasFilteredValues in Options then
      Style := [fsItalic]
    else
      Style := [];
    DrawText(ACanvas, ATextRect, AText, DT_VCENTER or DT_LEFT or DT_SINGLELINE or DT_MODIFYSTRING or DT_END_ELLIPSIS, Style);
  end;
  SelectClipRgn(ACanvas.Handle, 0);
  DeleteObject(Rgn);
end;

procedure TfcxFlatGridPainter.DrawScrollButton(ACanvas: TCanvas;
  ARect: TRect; ADirection: TfcxThemeDirection; AState: TfcxThemeState);
const
  StateToColor: array[TfcxThemeState] of TColor = (
    clBtnFace,
    clSilver,
    clGray,
    clBtnFace
  );
begin
  ACanvas.Brush.Color := StateToColor[AState];
  Frame3D(ACanvas, ARect, clGray, clGray, 1);
  ACanvas.FillRect(ARect);
  DrawScrollArrow(ACanvas, ARect, ADirection, AState);
end;

procedure TfcxFlatGridPainter.DrawSortArrow(ACanvas: TCanvas; AStyle: TfcxCustomThemeStyle; APosition: TPoint; AArrowType: TfcxThemeSortType);
var
  p: TPoint;
  OldPenColor, OldBrushColor: TColor;
begin
  p.x := APosition.X + SortArrowSize div 2;
  p.y := APosition.Y + SortArrowSize div 2;

  OldPenColor := ACanvas.Pen.Color;
  OldBrushColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := GetShadowColor(AStyle.FillColor);
  ACanvas.Pen.Color := ACanvas.Brush.Color;

  if AArrowType in [tstUp, tstDown] then
    DoDrawArrow(ACanvas, P, AArrowType)
  else
  begin
    dec(p.y, 1);
    DoDrawArrow(ACanvas, P, tstUp);
    inc(p.y, 4);
    DoDrawArrow(ACanvas, P, tstDown);
  end;

  ACanvas.Pen.Color := OldPenColor;
  ACanvas.Brush.Color := OldBrushColor;
end;

procedure TfcxFlatGridPainter.DrawTreeButton(ACanvas: TCanvas;
  APosition: TPoint; AKind: TfcxTreeButtonKind; AStyle: TfcxCustomThemeStyle; ASelected: Boolean);
var
  OldPenColor, OldBrushColor: TColor;  
begin
  OldPenColor := ACanvas.Pen.Color;
  OldBrushColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := AStyle.FillColor;
  if ASelected then
    ACanvas.Pen.Color := GetHighlightColor(AStyle.FillColor)
  else
    ACanvas.Pen.Color := GetShadowColor(AStyle.FillColor);
  ACanvas.Rectangle(APosition.X, APosition.Y, APosition.X + TreeButtonSize, APosition.Y + TreeButtonSize);
  if AKind in [tbkPlusButton, tbkMinusButton] then
  begin
    ACanvas.MoveTo(APosition.X + 2, APosition.Y + TreeButtonSize div 2);
    ACanvas.LineTo(APosition.X + TreeButtonSize - 2, APosition.Y + TreeButtonSize div 2);
  end;  
  if AKind = tbkPlusButton then
  begin
    ACanvas.MoveTo(APosition.X + TreeButtonSize div 2, APosition.Y + 2);
    ACanvas.LineTo(APosition.X + TreeButtonSize div 2, APosition.Y + TreeButtonSize - 2);
  end;
  if AKind in [tbkTreeButtonPlus, tbkTreeButtonMinus] then
  begin
    ACanvas.MoveTo(APosition.X + 2, APosition.Y + 2);
    ACanvas.LineTo(APosition.X + 2, APosition.Y + TreeButtonSize - 2);
    if AKind = tbkTreeButtonPlus then
    begin
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + 3);
      ACanvas.LineTo(APosition.X + 5, APosition.Y + 3);
      ACanvas.Rectangle(APosition.X + 5, APosition.Y + 2, APosition.X + 7, APosition.Y + 4);
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + TreeButtonSize - 3);
      ACanvas.LineTo(APosition.X + 4, APosition.Y + TreeButtonSize - 3);
      ACanvas.Rectangle(APosition.X + 4, APosition.Y + TreeButtonSize - 4, APosition.X + 6, APosition.Y + TreeButtonSize - 2);
    end
    else
    begin
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + 3);
      ACanvas.LineTo(APosition.X + 4, APosition.Y + 3);
      ACanvas.MoveTo(APosition.X + 2, APosition.Y + TreeButtonSize - 3);
      ACanvas.LineTo(APosition.X + 4, APosition.Y + TreeButtonSize - 3);

      ACanvas.MoveTo(APosition.X + 5, APosition.Y + 3);
      ACanvas.LineTo(APosition.X + 7, APosition.Y + 3);
      ACanvas.MoveTo(APosition.X + 5, APosition.Y + TreeButtonSize - 3);
      ACanvas.LineTo(APosition.X + 7, APosition.Y + TreeButtonSize - 3);
    end;
  end;
  ACanvas.Pen.Color := OldPenColor;
  ACanvas.Brush.Color := OldBrushColor;
end;

class function TfcxFlatGridPainter.GetPaintStyle: TfcxPaintStyle;
begin
  Result := psFlat;
end;

class function TfcxFlatGridPainter.GetPostfix: String;
begin
  Result := 'FLAT';
end;

function TfcxFlatGridPainter.MeasureAxisItem(ACanvas: TCanvas;
  AState: TfcxThemeState; AStyle: TfcxCustomThemeStyle): TSize;
begin
  Result.cx := 1;
  Result.cy := 1;
end;

{ TfcxDefaultGridPainter }

class function TfcxDefaultGridPainter.GetPaintStyle: TfcxPaintStyle;
begin
  Result := psDefault;
end;

end.
