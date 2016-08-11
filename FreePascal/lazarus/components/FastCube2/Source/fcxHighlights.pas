{*******************************************************}
{                                                       }
{          FastCube 2 highlights core classes           }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxHighlights;

{$INCLUDE fcx.inc}

interface

uses
{$IFDEF FPC}
  Types, LCLType, LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  SysUtils, Classes,
  fcxTypes, fcxXml, fcxRange, fcxStyles, fcxSlice,
  Graphics, Forms, ImgList;

type
  TfcxCustomHighlightEditorFrame = class;
  TfcxCustomHighlightEditorFrameClass = class of TfcxCustomHighlightEditorFrame;

  TfcxGraphicHighlight = class(TfcxCustomHighlight)
  private
    FStyle: TfcxCustomThemeStyle;
    procedure SetStyle(const Value: TfcxCustomThemeStyle);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoStyleChange(Sender: TObject); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure DrawExample(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DrawValue(ACanvas: TCanvas; AScale: Integer; var ARect: TRect; AValue: Pointer; var CanDrawImage, CanDrawText: Boolean); virtual;
    function GetStyleFor(AValue: Pointer): TfcxCustomThemeStyle; virtual;

    procedure LoadFromXML(AItem: TfcxXMLItem); override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;

    property Style: TfcxCustomThemeStyle read FStyle write SetStyle;
  end;

  TfcxGraphicHighlightClass = class of TfcxGraphicHighlight;

  TfcxRangeHighlight = class(TfcxGraphicHighlight)
  private
    FRange: TfcxRange;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoRangeChange(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function AsString: String; override;
    function AcceptValue(AValue: Variant): Boolean; override;

    class function GetCaptionForEditor: String; override;

    procedure LoadFromXML(AItem: TfcxXMLItem); override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;

    procedure LoadRangeXML(AItem: TfcxXMLItem); override;
    procedure LoadRange(ARange: TfcxLoadedRange); override;

    property Range: TfcxRange read FRange;
  end;


  TfcxContinuousHighlightKind = (
    chkTwoColorScale,
    chkThreeColorScale,
    chkBarChart,
    chkIconSet
  );

  TfcxContinuousHighlightValue = (
    chvBoundByRow,
    chvBoundByCol,
    chvNumber,
    chvPercentByRow,
    chvPercentByCol,
    chvPercentileByRow,
    chvPercentileByCol
    // todo: script value
  );

  TfcxContinuousHighlightIconSet = array[0..65535] of Integer;
  PfcxContinuousHighlightIconSet = ^TfcxContinuousHighlightIconSet;

  TfcxContinuousHighlightIconSets = class(TList)
  private
    function GetItem(AIndex: Integer): PfcxContinuousHighlightIconSet;
    procedure SetItem(AIndex: Integer; const Value: PfcxContinuousHighlightIconSet);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Item[AIndex: Integer]: PfcxContinuousHighlightIconSet read GetItem write SetItem; default;
  end;

  TfcxIconRule = record
    ImageIndex: Integer;
    GreaterEqual: Boolean;
    Value: Double;
    ValueType: TfcxContinuousHighlightValue;
  end;
  TfcxIconRuleArray = array of TfcxIconRule;

  TfcxContinuousHighlight = class(TfcxGraphicHighlight)
  private
    FKind: TfcxContinuousHighlightKind;
    FMidValue: Double;
    FMaxValue: Double;
    FMinValue: Double;
    FMinValueType: TfcxContinuousHighlightValue;
    FMaxValueType: TfcxContinuousHighlightValue;
    FMidValueType: TfcxContinuousHighlightValue;
    FMidValueColor: TColor;
    FMinValueColor: TColor;
    FMaxValueColor: TColor;
    FShowCellValue: Boolean;
    FGradientDraw: Boolean;
    FFrameColor: TColor;
    FSolidBrush: HBrush;
    FFrameBrush: HBrush;
    FIconSet: Integer;
    FImageCount: Integer;
    FIcons: TfcxIconRuleArray;
    procedure SetKind(const Value: TfcxContinuousHighlightKind);
    procedure SetMaxValue(const Value: Double);
    procedure SetMidValue(const Value: Double);
    procedure SetMinValue(const Value: Double);
    procedure SetMaxValueType(const Value: TfcxContinuousHighlightValue);
    procedure SetMidValueType(const Value: TfcxContinuousHighlightValue);
    procedure SetMinValueType(const Value: TfcxContinuousHighlightValue);
    procedure SetMaxValueColor(const Value: TColor);
    procedure SetMidValueColor(const Value: TColor);
    procedure SetMinValueColor(const Value: TColor);
    function GetImageCount: Integer;
    function GetImageList: TCustomImageList;
    procedure SetShowCellValue(const Value: Boolean);
    procedure SetGradientDraw(const Value: Boolean);
    procedure SetFrameColor(const Value: TColor);
    procedure SetIconSet(const Value: Integer);
    procedure InternalSetIconSet(const Value: Integer);
    procedure InternalSetImageCount(const Value: Integer);
    function GetImageIndex(AIcon: Integer): Integer;
    function GetImageValue(AIcon: Integer): Double;
    function GetImageValueGE(AIcon: Integer): Boolean;
    function GetImageValueType(AIcon: Integer): TfcxContinuousHighlightValue;
    procedure SetImageIndex(AIcon: Integer; const Value: Integer);
    procedure SetImageValue(AIcon: Integer; const Value: Double);
    procedure SetImageValueGE(AIcon: Integer; const Value: Boolean);
    procedure SetImageValueType(AIcon: Integer;
      const Value: TfcxContinuousHighlightValue);
    procedure CheckIconIndex(AIcon: Integer);
    function GetImageInfo(AIcon: Integer): String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoStyleChange(Sender: TObject); override;

    function GetMinValue(AValue: Pointer): Double;
    function GetMaxValue(AValue: Pointer): Double;
    function GetMidValue(AValue: Pointer): Double;
    function GetValue(AValue: Pointer): Double;

    function GetSolidBrush: HBrush;
    function GetFrameBrush: HBrush;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function AsString: String; override;
    function AcceptValue(AValue: Variant): Boolean; override;
    procedure DrawExample(ACanvas: TCanvas; ARect: TRect); override;
    procedure DrawValue(ACanvas: TCanvas; AScale: Integer; var ARect: TRect; AValue: Pointer; var CanDrawImage, CanDrawText: Boolean); override;
    procedure DrawIconSet(ACanvas: TCanvas; var ARect: TRect; AIconSet: Integer);

    function GetStyleFor(AValue: Pointer): TfcxCustomThemeStyle; override;

    class function GetCaptionForEditor: String; override;
    function CustomDrawn: Boolean; override;
    function HideValue: Boolean; override;

    procedure LoadFromXML(AItem: TfcxXMLItem); override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;

    property Kind: TfcxContinuousHighlightKind read FKind write SetKind;

    // for scales and bar chart
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MidValue: Double read FMidValue write SetMidValue;
    property MinValueType: TfcxContinuousHighlightValue read FMinValueType write SetMinValueType;
    property MaxValueType: TfcxContinuousHighlightValue read FMaxValueType write SetMaxValueType;
    property MidValueType: TfcxContinuousHighlightValue read FMidValueType write SetMidValueType;
    property MinValueColor: TColor read FMinValueColor write SetMinValueColor;
    property MaxValueColor: TColor read FMaxValueColor write SetMaxValueColor;
    property MidValueColor: TColor read FMidValueColor write SetMidValueColor;

    // for bar chart
    property GradientDraw: Boolean read FGradientDraw write SetGradientDraw;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    // for custom drawn
    property ShowCellValue: Boolean read FShowCellValue write SetShowCellValue;

    // for icons
    property IconSet: Integer read FIconSet write SetIconSet;

    property ImageList: TCustomImageList read GetImageList;
    property ImageIndex[AIcon: Integer]: Integer read GetImageIndex write SetImageIndex;
    property ImageInfo[AIcon: Integer]: String read GetImageInfo;
    property ImageValue[AIcon: Integer]: Double read GetImageValue write SetImageValue;
    property ImageValueType[AIcon: Integer]: TfcxContinuousHighlightValue read GetImageValueType write SetImageValueType;
    property ImageValueGE[AIcon: Integer]: Boolean read GetImageValueGE write SetImageValueGE;
    property ImageCount: Integer read GetImageCount;
  end;

  TfcxCustomHighlightEditorFrame = class(TFrame)
  private
    FHighlight: TfcxGraphicHighlight;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure SetHighlight(const Value: TfcxGraphicHighlight); virtual;
    class function GetHighlightClass: TfcxGraphicHighlightClass; virtual;
    procedure DoHighlightChange(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Prepare; virtual;

    property Highlight: TfcxGraphicHighlight read FHighlight write SetHighlight;
  end;
  
const
  KindToStr: array[TfcxContinuousHighlightKind] of String = (
 { chkTwoColorScale   } 'sTwoColorScale',
 { chkThreeColorScale } 'sThreeColorScale',
 { chkBarChart        } 'sBarChart',
 { chkIconSet         } 'sIconSet'
  );

var
  fcxRegisteredIconSets: TfcxContinuousHighlightIconSets;

procedure fcxRegisterIconSet(AIconSet: array of Integer);

implementation

uses
  typinfo,
  Math,
  StrUtils,
  RTLConsts,
  fcxRes,
  fcxGraphicRes,
  GraphUtil,
  fcxGraphicUtils;

// this is required to load old range info
const
  MaxReal: Double = +1.7e+308;
  MinReal: Double = -1.7e+308;  

procedure fcxRegisterHighlightClass(AClass: TfcxCustomHighlightClass);
begin
  if not Assigned(fcxRegisteredHighlights.FindItem(AClass)) then
    fcxRegisteredHighlights.Add(AClass, nil)
end;

procedure fcxRegisterHighlightEditor(AClass: TfcxCustomHighlightClass; AFrame: TfcxCustomHighlightEditorFrameClass);
var
  Rec: PfcxRegisteredHighlight;
begin
  Rec := fcxRegisteredHighlights.FindItem(AClass);
  if Assigned(Rec) then
    Rec^.FrameType := AFrame
  else
    fcxRegisteredHighlights.Add(AClass, AFrame);
end;

procedure fcxRegisterIconSet(AIconSet: array of Integer);
var
  NewItem: PfcxContinuousHighlightIconSet;
begin
  GetMem(NewItem, (Length(AIconSet) + 1) * SizeOf(Integer));
  NewItem[0] := Length(AIconSet);
  Move(AIconSet[0], NewItem[1], NewItem[0] * SizeOf(Integer));
  fcxRegisteredIconSets.Add(NewItem);
end;

{ TfcxCustomHighlight }

procedure TfcxGraphicHighlight.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxGraphicHighlight then
    TfcxGraphicHighlight(Dest).FStyle.Assign(FStyle);
  inherited;
end;

constructor TfcxGraphicHighlight.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStyle := TfcxCustomThemeStyle.Create(nil);
  FStyle.OnChange := DoStyleChange;
end;

destructor TfcxGraphicHighlight.Destroy;
begin
  FreeAndNil(FStyle);
  inherited;
end;

procedure TfcxGraphicHighlight.DoStyleChange(Sender: TObject);
begin
  DoChange;
end;

procedure TfcxGraphicHighlight.SetStyle(const Value: TfcxCustomThemeStyle);
begin
  FStyle.Assign(Value);
end;

{ TfcxHighlight }

procedure TfcxRangeHighlight.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxRangeHighlight then
  begin
    TfcxRangeHighlight(Dest).Range.Assign(Range);
    inherited AssignTo(Dest);
  end
  else
    inherited;
end;

constructor TfcxRangeHighlight.Create(Collection: TCollection);
begin
  inherited;
  FRange := TfcxRange.Create(nil);
  FRange.OnChange := DoRangeChange;
end;

function TfcxRangeHighlight.AsString: String;
begin
  Result := FRange.AsString;
end;

procedure TfcxGraphicHighlight.DrawExample(ACanvas: TCanvas; ARect: TRect);
var
  OldColor: TColor;
  OldStyle: TBrushStyle;
  S: String;
  Sz: TSize;
begin
  if (Style.GradientDirection <> tgdNone) or (Style.FillColor <> clNone) then
    Style.DrawBackground(ACanvas, ARect)
  else
  begin
    OldColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := clWindow;
    ACanvas.FillRect(ARect);
    ACanvas.Brush.Color := OldColor;
  end;
  
  S := fcxResources.Get('sExample');
  Sz := ACanvas.TextExtent(S);
  OldStyle := ACanvas.Brush.Style;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := Style.Font;
  ACanvas.Font.Color := Style.TextColor;
  with ARect, Sz do
    ACanvas.TextOut((Left + Right - cx) div 2, (Top + Bottom - cy) div 2, S);
  ACanvas.Brush.Style := OldStyle;
end;

function TfcxGraphicHighlight.GetStyleFor(AValue: Pointer): TfcxCustomThemeStyle;
begin
  Result := Style;
end;

procedure TfcxGraphicHighlight.DrawValue(ACanvas: TCanvas; AScale: Integer; var ARect: TRect; AValue: Pointer; var CanDrawImage, CanDrawText: Boolean);
begin
  // nothing here
end;

procedure TfcxGraphicHighlight.LoadFromXML(AItem: TfcxXMLItem);
begin
  inherited;
  Style.LoadFromXML(AItem.FindItem('STYLE'));
end;

procedure TfcxGraphicHighlight.SaveToXML(AItem: TfcxXMLItem);
begin
  inherited;
  Style.SaveToXML(AItem.Add);
end;

{ TfcxCustomHighlightEditorFrame }

constructor TfcxCustomHighlightEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FHighlight := GetHighlightClass.Create(nil);
  FHighlight.OnChange := DoHighlightChange;
end;

destructor TfcxCustomHighlightEditorFrame.Destroy;
begin
  FHighlight.Free;
  inherited;
end;

procedure TfcxCustomHighlightEditorFrame.DoHighlightChange(Sender: TObject);
begin
  // nothing here
end;

class function TfcxCustomHighlightEditorFrame.GetHighlightClass: TfcxGraphicHighlightClass;
begin
  Result := TfcxGraphicHighlight;
end;

procedure TfcxCustomHighlightEditorFrame.PaintWindow(DC: HDC);
begin
  // this is required to fix D7 bug
  {$ifdef fpc}
  inherited PaintWindow(DC);
  {$endif}
end;

procedure TfcxCustomHighlightEditorFrame.Prepare;
begin
  HandleNeeded;
  DoHighlightChange(nil);
end;

procedure TfcxCustomHighlightEditorFrame.SetHighlight(const Value: TfcxGraphicHighlight);
begin
  FHighlight.Assign(Value);
end;

{ TfcxContinuousHighlight }

function TfcxContinuousHighlight.AcceptValue(AValue: Variant): Boolean;
begin
  Result := TVarData(AValue).VType > 1;
end;

procedure TfcxContinuousHighlight.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TfcxContinuousHighlight then
  begin
    TfcxContinuousHighlight(Dest).FKind := FKind;
    TfcxContinuousHighlight(Dest).FMinValueType := FMinValueType;
    TfcxContinuousHighlight(Dest).FMaxValueType := FMaxValueType;
    TfcxContinuousHighlight(Dest).FMidValueType := FMidValueType;
    TfcxContinuousHighlight(Dest).FMinValue := FMinValue;
    TfcxContinuousHighlight(Dest).FMaxValue := FMaxValue;
    TfcxContinuousHighlight(Dest).FMidValue := FMidValue;
    TfcxContinuousHighlight(Dest).FMinValueColor := FMinValueColor;
    TfcxContinuousHighlight(Dest).FMaxValueColor := FMaxValueColor;
    TfcxContinuousHighlight(Dest).FMidValueColor := FMidValueColor;
    TfcxContinuousHighlight(Dest).FShowCellValue := FShowCellValue;
    TfcxContinuousHighlight(Dest).FGradientDraw := FGradientDraw;
    TfcxContinuousHighlight(Dest).FFrameColor := FFrameColor;
    TfcxContinuousHighlight(Dest).FIconSet := FIconSet;
    TfcxContinuousHighlight(Dest).InternalSetImageCount(ImageCount);
    for I := 0 to ImageCount - 1 do
      TfcxContinuousHighlight(Dest).FIcons[I] := FIcons[I];
    inherited AssignTo(Dest);
  end
  else
    inherited;
end;

function TfcxContinuousHighlight.AsString: String;
begin
  Result := fcxResources.Get(KindToStr[Kind]);
end;

constructor TfcxContinuousHighlight.Create(Collection: TCollection);
begin
  inherited;
  FKind := chkTwoColorScale;
  FMinValue := 0;
  FMaxValue := 0;
  FMidValue := 50;
  FMinValueType := chvBoundByRow;
  FMaxValueType := chvBoundByRow;
  FMidValueType := chvPercentileByRow;
  FMinValueColor := clRed;
  FMaxValueColor := clGreen;
  FMidValueColor := clYellow;
  FShowCellValue := True;
  FGradientDraw := True;
  FFrameColor := FMinValueColor;
  InternalSetIconSet(0);
  FSolidBrush := 0;
  FFrameBrush := 0;
end;

function TfcxContinuousHighlight.CustomDrawn: Boolean;
begin
  Result := Kind in [chkBarChart, chkIconSet];
end;

destructor TfcxContinuousHighlight.Destroy;
begin
  if FSolidBrush <> 0 then
    DeleteObject(FSolidBrush);
  if FFrameBrush <> 0 then
    DeleteObject(FFrameBrush);
  Finalize(FIcons);
  inherited;
end;

procedure TfcxContinuousHighlight.DoStyleChange(Sender: TObject);
begin
  // this should not create any change because style is not used
end;

procedure TfcxContinuousHighlight.DrawExample(ACanvas: TCanvas; ARect: TRect);
var
  R: TRect;
  I, dx: Integer;
  AImageList: TCustomImageList;
  H, L, S: Word;
  StartColor, EndColor: TColor;
begin
  case Kind of
    chkTwoColorScale:
      FillGradient(ACanvas, ARect, MinValueColor, MaxValueColor, 0);
    chkThreeColorScale:
      begin
        R := ARect;
        R.Left := (R.Left + R.Right) div 2;
        ARect.Right := R.Left;
        FillGradient(ACanvas, ARect, MinValueColor, MidValueColor, 0);
        FillGradient(ACanvas, R, MidValueColor, MaxValueColor, 0);
      end;
    chkBarChart:
      begin
        FrameRect(ACanvas.Handle, ARect, GetStockObject(GRAY_BRUSH));
        InflateRect(ARect, -2, -2);

        if FrameColor <> clNone then
        begin
          FrameRect(ACanvas.Handle, ARect, GetFrameBrush);
          InflateRect(ARect, -1, -1);
        end;

        if GradientDraw then
        begin
          StartColor := ColorToRGB(MinValueColor);
          ColorRGBToHLS(StartColor, H, L, S);
          EndColor := ColorHLSToRGB(H, 255, S);
          FillGradient(ACanvas, ARect, StartColor, EndColor, 0);
        end
        else
          FillRect(ACanvas.Handle, ARect, GetSolidBrush);
      end;
    chkIconSet:
      begin
        AImageList := ImageList;
        if AImageList = nil then
          Exit;
        dx := 0;
        for I := 0 to ImageCount - 1 do
        begin
          AImageList.Draw(ACanvas, ARect.Left + dx, (ARect.Top + ARect.Bottom - AImageList.Height) div 2, ImageIndex[I]);
          dx := dx + AImageList.Width + 3;
        end;
      end;
  end;
end;

procedure TfcxContinuousHighlight.DrawValue(ACanvas: TCanvas; AScale: Integer; var ARect: TRect; AValue: Pointer; var CanDrawImage, CanDrawText: Boolean);

  function MatchIcon(AIndex: Integer; CellValue: Double): Boolean;
  var
    RuleValue: Double;
  begin
    case ImageValueType[AIndex] of
      chvBoundByRow: RuleValue := TfcxCustomHighlights(Collection).RequestMinMax(AValue, True, False);
      chvBoundByCol: RuleValue := TfcxCustomHighlights(Collection).RequestMinMax(AValue, True, True);
      chvPercentByRow: RuleValue := TfcxCustomHighlights(Collection).RequestPercent(AValue, ImageValue[AIndex], False);
      chvPercentByCol: RuleValue := TfcxCustomHighlights(Collection).RequestPercent(AValue, ImageValue[AIndex], True);
      chvPercentileByRow: RuleValue := TfcxCustomHighlights(Collection).RequestPercentile(AValue, ImageValue[AIndex], False);
      chvPercentileByCol: RuleValue := TfcxCustomHighlights(Collection).RequestPercentile(AValue, ImageValue[AIndex], True);
    else
      RuleValue := ImageValue[AIndex];
    end;
    if ImageValueGE[AIndex] then
      Result := CellValue >= RuleValue
    else
      Result := CellValue > RuleValue;
  end;

var
  I, Len, MinL, Index: Integer;
  MinValue, MaxValue, Value: Double;
  AImageList: TCustomImageList;
  R: TRect;
  StartColor, EndColor: TColor;
  H, L, S: Word;
begin
  case Kind of
    chkBarChart:
      begin
        R := ARect;
        // get min, max, value
        MinValue := GetMinValue(AValue);
        MaxValue := GetMaxValue(AValue);
        Value := GetValue(AValue);

        with R do
          Len := Right - Left;

        MinL := Len div 10;
        Len := Len - MinL;

        if Value <= MinValue then
          Len := 0
        else
        if Value < MaxValue then
          Len := Round(Len * (Value - MinValue) / (MaxValue - MinValue));
        R.Right := R.Left + Len + MinL;

        if FrameColor <> clNone then
        begin
          FrameRect(ACanvas.Handle, R, GetFrameBrush);
          InflateRect(R, -1, -1);
        end;

        if GradientDraw then
        begin
          StartColor := ColorToRGB(MinValueColor);
          ColorRGBToHLS(StartColor, H, L, S);
          EndColor := ColorHLSToRGB(H, 255, S);
          FillGradient(ACanvas, R, StartColor, EndColor, 0);
        end
        else
          FillRect(ACanvas.Handle, R, GetSolidBrush);
      end;
    chkIconSet:
      begin
        AImageList := ImageList;
        if AImageList = nil then
          Exit;
        Value := GetValue(AValue);
        Index := 0;
        for I := ImageCount - 1 downto 1 do
          if MatchIcon(I, Value) then
          begin
            Index := I;
            Break;
          end;
        if AScale = 100 then
        begin
          AImageList.Draw(ACanvas, ARect.Left, (ARect.Top + ARect.Bottom - AImageList.Height) div 2, ImageIndex[Index]);
          ARect.Left := ARect.Left + AImageList.Width;
        end
        else
        begin
          R.Left := ARect.Left;
          R.Bottom := MulDiv(AImageList.Height, AScale, 100);
          R.Top := (ARect.Top + ARect.Bottom - R.Bottom) div 2;
          R.Right := R.Left + MulDiv(AImageList.Width, AScale, 100);
          R.Bottom := R.Top + R.Bottom;
          ImageListScaleDraw(ACanvas, ImageList, ImageIndex[Index], R);
          ARect.Left := ARect.Left + R.Right - R.Left;
        end
      end;
  end;
  CanDrawImage := CanDrawImage and ShowCellValue;
  CanDrawText := CanDrawText and ShowCellValue;
end;

class function TfcxContinuousHighlight.GetCaptionForEditor: String;
begin
  Result := fcxResources.Get('sContinuousHighlightCaption');
end;

function TfcxContinuousHighlight.GetFrameBrush: HBrush;
begin
  if FFrameBrush = 0 then
    FFrameBrush := CreateSolidBrush(ColorToRGB(FrameColor));
  Result := FFrameBrush;
end;

function TfcxContinuousHighlight.GetImageCount: Integer;
begin
  Result := FImageCount;
end;

function TfcxContinuousHighlight.GetImageIndex(AIcon: Integer): Integer;
begin
  Result := FIcons[AIcon].ImageIndex;
end;

function TfcxContinuousHighlight.GetImageList: TCustomImageList;
begin
  Result := fcxGraphicResources.ContinuousHighlightsImages;
end;

function TfcxContinuousHighlight.GetMaxValue(AValue: Pointer): Double;
begin
  case MaxValueType of
    chvBoundByRow: Result := TfcxCustomHighlights(Collection).RequestMinMax(AValue, True, False);
    chvBoundByCol: Result := TfcxCustomHighlights(Collection).RequestMinMax(AValue, True, True);
    chvPercentByRow: Result := TfcxCustomHighlights(Collection).RequestPercent(AValue, MaxValue, False);
    chvPercentByCol: Result := TfcxCustomHighlights(Collection).RequestPercent(AValue, MaxValue, True);
    chvPercentileByRow: Result := TfcxCustomHighlights(Collection).RequestPercentile(AValue, MaxValue, False);
    chvPercentileByCol: Result := TfcxCustomHighlights(Collection).RequestPercentile(AValue, MaxValue, True);
  else
    Result := MaxValue;
  end;
end;

function TfcxContinuousHighlight.GetMidValue(AValue: Pointer): Double;
begin
  case MidValueType of
    chvPercentByRow: Result := TfcxCustomHighlights(Collection).RequestPercent(AValue, MidValue, False);
    chvPercentByCol: Result := TfcxCustomHighlights(Collection).RequestPercent(AValue, MidValue, True);
    chvPercentileByRow: Result := TfcxCustomHighlights(Collection).RequestPercentile(AValue, MidValue, False);
    chvPercentileByCol: Result := TfcxCustomHighlights(Collection).RequestPercentile(AValue, MidValue, True);
  else
    Result := MidValue;
  end;
end;

function TfcxContinuousHighlight.GetMinValue(AValue: Pointer): Double;
begin
  case MinValueType of
    chvBoundByRow: Result := TfcxCustomHighlights(Collection).RequestMinMax(AValue, False, False);
    chvBoundByCol: Result := TfcxCustomHighlights(Collection).RequestMinMax(AValue, False, True);
    chvPercentByRow: Result := TfcxCustomHighlights(Collection).RequestPercent(AValue, MinValue, False);
    chvPercentByCol: Result := TfcxCustomHighlights(Collection).RequestPercent(AValue, MinValue, True);
    chvPercentileByRow: Result := TfcxCustomHighlights(Collection).RequestPercentile(AValue, MinValue, False);
    chvPercentileByCol: Result := TfcxCustomHighlights(Collection).RequestPercentile(AValue, MinValue, True);
  else
    Result := MinValue;  
  end;
end;

function TfcxContinuousHighlight.GetSolidBrush: HBrush;
begin
  if FSolidBrush = 0 then
    FSolidBrush := CreateSolidBrush(ColorToRGB(MinValueColor));
  Result := FSolidBrush;
end;

function TfcxContinuousHighlight.GetStyleFor(AValue: Pointer): TfcxCustomThemeStyle;
var
  MinValue, MaxValue, MidValue, Value, F: Double;
  R1, R2, G1, G2, B1, B2: Byte;
  C1, C2: TColor;
begin
  case Kind of
    chkTwoColorScale:
      begin
        Result := Style;
        // get min, max, value
        MinValue := GetMinValue(AValue);
        MaxValue := GetMaxValue(AValue);
        Value := GetValue(AValue);

        if Value >= MaxValue then
          Result.FillColor := MaxValueColor
        else
        if Value <= MinValue then
          Result.FillColor := MinValueColor
        else
        begin
          C1 := ColorToRGB(MinValueColor);
          C2 := ColorToRGB(MaxValueColor);

          GetRGBValues(C1, R1, G1, B1);
          GetRGBValues(C2, R2, G2, B2);

          F := (Value - MinValue) / (MaxValue - MinValue);
          R1 := R1 + Round((R2 - R1) * F);
          G1 := G1 + Round((G2 - G1) * F);
          B1 := B1 + Round((B2 - B1) * F);

          Result.FillColor := RGB(R1, G1, B1);
        end;
      end;
    chkThreeColorScale:
      begin
        Result := Style;
        // get min, max, value
        MinValue := GetMinValue(AValue);
        MidValue := GetMidValue(AValue);
        MaxValue := GetMaxValue(AValue);
        Value := GetValue(AValue);

        if Value >= MaxValue then
          Result.FillColor := MaxValueColor
        else
        if Value <= MinValue then
          Result.FillColor := MinValueColor
        else
        if Value = MidValue then
          Result.FillColor := MidValueColor
        else
        if Value < MidValue then
        begin
          C1 := ColorToRGB(MinValueColor);
          C2 := ColorToRGB(MidValueColor);

          GetRGBValues(C1, R1, G1, B1);
          GetRGBValues(C2, R2, G2, B2);

          F := (Value - MinValue) / (MidValue - MinValue);
          R1 := R1 + Round((R2 - R1) * F);
          G1 := G1 + Round((G2 - G1) * F);
          B1 := B1 + Round((B2 - B1) * F);

          Result.FillColor := RGB(R1, G1, B1);
        end
        else
        begin
          C1 := ColorToRGB(MidValueColor);
          C2 := ColorToRGB(MaxValueColor);

          GetRGBValues(C1, R1, G1, B1);
          GetRGBValues(C2, R2, G2, B2);

          F := (Value - MidValue) / (MaxValue - MidValue);
          R1 := R1 + Round((R2 - R1) * F);
          G1 := G1 + Round((G2 - G1) * F);
          B1 := B1 + Round((B2 - B1) * F);

          Result.FillColor := RGB(R1, G1, B1);
        end;
      end;
  else
    Result := Style;
  end;
end;

function TfcxContinuousHighlight.GetValue(AValue: Pointer): Double;
begin
  Result := TfcxCustomHighlights(Collection).RequestValue(AValue);
end;

procedure TfcxContinuousHighlight.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    if FFrameBrush <> 0 then
    begin
      DeleteObject(FFrameBrush);
      FFrameBrush := 0;
    end;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetGradientDraw(const Value: Boolean);
begin
  if FGradientDraw <> Value then
  begin
    FGradientDraw := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.InternalSetImageCount(const Value: Integer);
begin
  FImageCount := Value;
  SetLength(FIcons, Value);
end;

procedure TfcxContinuousHighlight.InternalSetIconSet(const Value: Integer);
var
  I: Integer;
begin
  FIconSet := Value;
  InternalSetImageCount(fcxRegisteredIconSets[FIconSet][0]);
  for I := 0 to FImageCount - 1 do
  begin
    FIcons[I].ImageIndex := fcxRegisteredIconSets[FIconSet][I + 1];
    FIcons[I].ValueType := chvPercentByRow;
    FIcons[I].GreaterEqual := True;
    FIcons[I].Value := 100 div FImageCount * I;
  end;
end;

procedure TfcxContinuousHighlight.SetIconSet(const Value: Integer);
begin
  if FIconSet <> Value then
  begin
    InternalSetIconSet(Value);
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.CheckIconIndex(AIcon: Integer);
begin
  if (AIcon < 0) or (AIcon >= FImageCount) then
    raise EListError.CreateFmt(SListIndexError, [AIcon]);
end;

procedure TfcxContinuousHighlight.SetImageIndex(AIcon: Integer; const Value: Integer);
begin
  CheckIconIndex(AIcon);
  if FIcons[AIcon].ImageIndex <> Value then
  begin
    FIcons[AIcon].ImageIndex := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetKind(const Value: TfcxContinuousHighlightKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMaxValue(const Value: Double);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMaxValueColor(const Value: TColor);
begin
  if FMaxValueColor <> Value then
  begin
    FMaxValueColor := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMaxValueType(const Value: TfcxContinuousHighlightValue);
begin
  if FMaxValueType <> Value then
  begin
    FMaxValueType := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMidValue(const Value: Double);
begin
  if FMidValue <> Value then
  begin
    FMidValue := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMidValueColor(const Value: TColor);
begin
  if FMidValueColor <> Value then
  begin
    FMidValueColor := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMidValueType(const Value: TfcxContinuousHighlightValue);
begin
  if FMidValueType <> Value then
  begin
    FMidValueType := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMinValue(const Value: Double);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMinValueColor(const Value: TColor);
begin
  if FMinValueColor <> Value then
  begin
    FMinValueColor := Value;
    if FSolidBrush <> 0 then
    begin
      DeleteObject(FSolidBrush);
      FSolidBrush := 0;
    end;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetMinValueType(const Value: TfcxContinuousHighlightValue);
begin
  if FMinValueType <> Value then
  begin
    FMinValueType := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetShowCellValue(const Value: Boolean);
begin
  if FShowCellValue <> Value then
  begin
    FShowCellValue := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.DrawIconSet(ACanvas: TCanvas; var ARect: TRect; AIconSet: Integer);
var
  AImageList: TCustomImageList;
  dx, I: Integer;
begin
  AImageList := ImageList;
  if AImageList = nil then
    Exit;
  dx := 5;
  for I := 0 to fcxRegisteredIconSets[AIconSet][0] - 1 do
  begin
    AImageList.Draw(ACanvas, ARect.Left + dx, (ARect.Top + ARect.Bottom - AImageList.Height) div 2, fcxRegisteredIconSets[AIconSet][I + 1]);
    dx := dx + AImageList.Width + 3;
  end;
end;

function TfcxContinuousHighlight.GetImageValue(AIcon: Integer): Double;
begin
  Result := FIcons[AIcon].Value;
end;

function TfcxContinuousHighlight.GetImageValueGE(AIcon: Integer): Boolean;
begin
  Result := FIcons[AIcon].GreaterEqual;
end;

function TfcxContinuousHighlight.GetImageValueType(AIcon: Integer): TfcxContinuousHighlightValue;
begin
  Result := FIcons[AIcon].ValueType;
end;

procedure TfcxContinuousHighlight.SetImageValue(AIcon: Integer; const Value: Double);
begin
  CheckIconIndex(AIcon);
  if FIcons[AIcon].Value <> Value then
  begin
    FIcons[AIcon].Value := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetImageValueGE(AIcon: Integer; const Value: Boolean);
begin
  CheckIconIndex(AIcon);
  if FIcons[AIcon].GreaterEqual <> Value then
  begin
    FIcons[AIcon].GreaterEqual := Value;
    DoChange;
  end;
end;

procedure TfcxContinuousHighlight.SetImageValueType(AIcon: Integer; const Value: TfcxContinuousHighlightValue);
begin
  CheckIconIndex(AIcon);
  if FIcons[AIcon].ValueType <> Value then
  begin
    FIcons[AIcon].ValueType := Value;
    DoChange;
  end;
end;

function TfcxContinuousHighlight.GetImageInfo(AIcon: Integer): String;
const
  GEToStr: array[Boolean] of String = ('<=', '<');
begin
  if AIcon = ImageCount - 1 then
    Result := fcxResources.Get('sIfValue')
  else
  begin
    Result := Format(fcxResources.Get('sIfValue1'), [GEToStr[ImageValueGE[AIcon + 1]], ImageValue[AIcon + 1]]);
    if AIcon <> 0 then
      Result := Result + ' ' + fcxResources.Get('sAnd');
  end;
end;

procedure TfcxContinuousHighlight.LoadFromXML(AItem: TfcxXMLItem);

  procedure LoadImageInfo(AIcon: Integer; AItem: TfcxXMLItem);
  begin
    if AItem.Name <> 'ICON' then
      Exit;
    ImageIndex[AIcon] := AItem.IntProp['ImageIndex'];
    ImageValue[AIcon] := AItem.FloatProp['ImageValue'];
    ImageValueType[AIcon] := TfcxContinuousHighlightValue(GetEnumValue(TypeInfo(TfcxContinuousHighlightValue), AItem.Prop['ImageValueType']));
    ImageValueGE[AIcon] := AItem.BoolProp['ImageValueGE'];
  end;

var
  I: Integer;
begin
  inherited;
  Kind := TfcxContinuousHighlightKind(GetEnumValue(TypeInfo(TfcxContinuousHighlightKind), AItem.Prop['Kind']));
  // for scales and bar chart
  MinValue := AItem.FloatProp['MinValue'];
  MaxValue := AItem.FloatProp['MaxValue'];
  MidValue := AItem.FloatProp['MidValue'];
  MinValueType := TfcxContinuousHighlightValue(GetEnumValue(TypeInfo(TfcxContinuousHighlightValue), AItem.Prop['MinValueType']));
  MaxValueType := TfcxContinuousHighlightValue(GetEnumValue(TypeInfo(TfcxContinuousHighlightValue), AItem.Prop['MaxValueType']));
  MidValueType := TfcxContinuousHighlightValue(GetEnumValue(TypeInfo(TfcxContinuousHighlightValue), AItem.Prop['MidValueType']));
  MinValueColor := AItem.IntProp['MinValueColor'];
  MaxValueColor := AItem.IntProp['MaxValueColor'];
  MidValueColor := AItem.IntProp['MidValueColor'];
  // for bar chart
  GradientDraw := AItem.BoolProp['GradientDraw'];
  FrameColor := AItem.IntProp['FrameColor'];
  // for custom drawn
  ShowCellValue := AItem.BoolProp['ShowCellValue'];

  // for icons
  AItem := AItem.FindItem('ICONSET');
  IconSet := AItem.IntProp['Value'];
  for I := 0 to ImageCount - 1 do
    LoadImageInfo(I, AItem[I]);
end;

procedure TfcxContinuousHighlight.SaveToXML(AItem: TfcxXMLItem);

  procedure SaveImageInfo(AIcon: Integer; AItem: TfcxXMLItem);
  begin
    AItem.Name := 'ICON';
    AItem.IntProp['ImageIndex'] := ImageIndex[AIcon];
    AItem.FloatProp['ImageValue'] := ImageValue[AIcon];
    AItem.Prop['ImageValueType'] := GetEnumName(TypeInfo(TfcxContinuousHighlightValue), Ord(ImageValueType[AIcon]));
    AItem.BoolProp['ImageValueGE'] := ImageValueGE[AIcon];
  end;

var
  I: Integer;
begin
  inherited;
  AItem.Prop['Kind'] := GetEnumName(TypeInfo(TfcxContinuousHighlightKind), Ord(Kind));
  // for scales and bar chart
  AItem.FloatProp['MinValue']:= MinValue;
  AItem.FloatProp['MaxValue']:= MaxValue;
  AItem.FloatProp['MidValue']:= MidValue;
  AItem.Prop['MinValueType'] := GetEnumName(TypeInfo(TfcxContinuousHighlightValue), Ord(MinValueType));
  AItem.Prop['MaxValueType'] := GetEnumName(TypeInfo(TfcxContinuousHighlightValue), Ord(MaxValueType));
  AItem.Prop['MidValueType'] := GetEnumName(TypeInfo(TfcxContinuousHighlightValue), Ord(MidValueType));
  AItem.IntProp['MinValueColor'] := MinValueColor;
  AItem.IntProp['MaxValueColor'] := MaxValueColor;
  AItem.IntProp['MidValueColor'] := MidValueColor;
  // for bar chart
  AItem.BoolProp['GradientDraw'] := GradientDraw;
  AItem.IntProp['FrameColor'] := FrameColor;
  // for custom drawn
  AItem.BoolProp['ShowCellValue'] := ShowCellValue;

  // for icons
  AItem := AItem.Add;
  AItem.Name := 'ICONSET';
  AItem.IntProp['Value'] := IconSet;
  for I := 0 to ImageCount - 1 do
    SaveImageInfo(I, AItem.Add);
end;

function TfcxContinuousHighlight.HideValue: Boolean;
begin
  Result := not ShowCellValue;
end;

{ TfcxContinuousHighlightIconSets }

function TfcxContinuousHighlightIconSets.GetItem(AIndex: Integer): PfcxContinuousHighlightIconSet;
begin
  Result := inherited Get(AIndex);
end;

procedure TfcxContinuousHighlightIconSets.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    FreeMem(PfcxContinuousHighlightIconSet(Ptr));
  inherited;
end;

procedure TfcxContinuousHighlightIconSets.SetItem(AIndex: Integer; const Value: PfcxContinuousHighlightIconSet);
begin
  inherited Put(AIndex, Value);
end;

procedure TfcxRangeHighlight.LoadFromXML(AItem: TfcxXMLItem);
begin
  inherited;
  Range.LoadFromXML(AItem);
end;

procedure TfcxRangeHighlight.SaveToXML(AItem: TfcxXMLItem);
begin
  inherited;
  Range.SaveToXML(AItem);
end;

procedure TfcxRangeHighlight.DoRangeChange(Sender: TObject);
begin
  DoChange;
end;

destructor TfcxRangeHighlight.Destroy;
begin
  FRange.Free;
  inherited;
end;

function TfcxRangeHighlight.AcceptValue(AValue: Variant): Boolean;
begin
  Result := FRange.Match(AValue);
end;

class function TfcxRangeHighlight.GetCaptionForEditor: String;
begin
  Result := fcxResources.Get('sRangeHighlightCaption');
end;

procedure TfcxRangeHighlight.LoadRangeXML(AItem: TfcxXMLItem);
var
  D: Double;
begin
  with AItem do
  begin
    D := FloatProp['low'];
    if D = MinReal then
      Range.LowRange := NegInfinity
    else
      Range.LowRange := D;
    D := FloatProp['high'];
    if D = MaxReal then
      Range.HighRange := Infinity
    else
      Range.HighRange := D;
    Style.FillColor := IntProp['fill_color'];
    Style.TextColor := IntProp['text_color'];
  end;
end;

procedure TfcxRangeHighlight.LoadRange(ARange: TfcxLoadedRange);
begin
  if ARange.Low = MinReal then
    Range.LowRange := NegInfinity
  else
    Range.LowRange := ARange.Low;
  if ARange.High = MaxReal then
    Range.HighRange := Infinity
  else
    Range.HighRange := ARange.High;
  Style.FillColor := ARange.FillColor;
  Style.TextColor := ARange.TextColor;
end;

initialization
  fcxRegisterHighlightClass(TfcxContinuousHighlight);
  fcxRegisterHighlightClass(TfcxRangeHighlight);

  fcxRegisteredIconSets := TfcxContinuousHighlightIconSets.Create;

  // 3 icons
  fcxRegisterIconSet([01, 02, 00]); // 3 color arrows
  fcxRegisterIconSet([06, 07, 05]); // 3 gray arrows
  fcxRegisterIconSet([36, 35, 34]); // 3 flags
  fcxRegisterIconSet([30, 29, 28]); // 3 lights
  fcxRegisterIconSet([39, 38, 37]); // 3 lights with frame
  fcxRegisterIconSet([33, 32, 28]); // 3 icons
  fcxRegisterIconSet([42, 41, 40]); // 3 symbols in circle
  fcxRegisterIconSet([45, 44, 43]); // 3 symbols with no circle
  fcxRegisterIconSet([48, 47, 46]); // 3 stars
  fcxRegisterIconSet([51, 50, 49]); // 3 triangles

  // 4 icons
  fcxRegisterIconSet([01, 04, 03, 00]); // 4 color arrows
  fcxRegisterIconSet([06, 09, 08, 05]); // 4 gray arrows
  fcxRegisterIconSet([13, 12, 11, 10]); // from red to black
  fcxRegisterIconSet([21, 20, 19, 18]); // 4 marks
  fcxRegisterIconSet([31, 30, 29, 28]); // 4 lights

  // 5 icons
  fcxRegisterIconSet([01, 04, 02, 03, 00]); // 5 arrows
  fcxRegisterIconSet([06, 09, 07, 08, 05]); // 5 gray arrows
  fcxRegisterIconSet([22, 21, 20, 19, 18]); // 5 marks
  fcxRegisterIconSet([17, 16, 15, 14, 13]); // 5 quarters
  fcxRegisterIconSet([27, 26, 25, 24, 23]); // 5 squares

finalization
  fcxRegisteredIconSets.Free;

end.
