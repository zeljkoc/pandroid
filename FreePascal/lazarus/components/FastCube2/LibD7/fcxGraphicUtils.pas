{*******************************************************}
{                                                       }
{         FastCube 2 graphic helper routines            }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxGraphicUtils;
interface
{$INCLUDE fcx.inc}

uses
{$IFNDEF FPC}
  Windows, CommCtrl,
{$ELSE}
  LCLType, LCLIntf,
{$ENDIF}
{$IFDEF Delphi_18UP}
  System.UITypes,
{$ENDIF}
  Classes, SysUtils, ImgList, Graphics, Controls, ExtCtrls;

function InverseColor(Color : TColor): TColor;
procedure AdjustColors(Bevel: TPanelBevel; out TopColor, BottomColor: TColor);
function CreateRotatedFont(ACanvas: TCanvas; AAngle: integer): hFont;
procedure BevelLine(ACanvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor; Width: Integer);
function FillGradient(ACanvas: TCanvas; ARect: TRect; AStartColor, AEndColor: Cardinal; ADirection: Byte): Boolean;
procedure ImageListScaleDraw(ACanvas: TCanvas; AImageList: TCustomImageList; AImageIndex: Integer; R: TRect);
function LoadBitmap(ResourceName: String): TBitmap; overload;
procedure LoadBitmap(ABitmap: TBitmap; ResourceName: String); overload;
procedure LoadHiResImages(ImageList: TImageList; Size: Integer; ResName: String; MaskColor: TColor = clFuchsia; NumAsOne: Integer = 1);
procedure CorrectFPCBitmap(ABitmap: TBitmap);
{$IFNDEF FPC}
function CreateAlphaBlendedBitmap(Source: TBitmap; R: TRect; Alpha, AColor: DWord): TBitmap;
{$ENDIF}

{$IFNDEF DELPHI_6UP}
function GetHighLightColor(const Color: TColor; Luminance: Integer = 19): TColor;
function GetShadowColor(const Color: TColor; Luminance: Integer = -50): TColor;
procedure ColorRGBToHLS(clrRGB: COLORREF; var Hue, Luminance, Saturation: Word);
function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;
function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;
{$ENDIF}

function GetRValue(rgb: DWORD): Byte;
{$EXTERNALSYM GetRValue}
function GetGValue(rgb: DWORD): Byte;
{$EXTERNALSYM GetGValue}
function GetBValue(rgb: DWORD): Byte;
{$EXTERNALSYM GetBValue}

procedure GetRGBValues(rgb: DWord; out R, G, B: Byte);
function HTMLRGBColor(Color: TColor): string;

const
  clInfoBkStd = $EBFFFF;

implementation

uses
  Math;
type
  TBGRA = array[0..3] of byte;

function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

procedure GetRGBValues(rgb: DWord; out R, G, B: Byte);
begin
  R := RGB and $ff;
  G := (RGB shr 8) and $ff;
  B := (RGB shr 16) and $ff;
end;

function InverseColor(Color : TColor): TColor;
begin
  Result := $FFFFFF - ColorToRGB(Color);
end;

procedure AdjustColors(Bevel: TPanelBevel; out TopColor, BottomColor: TColor);
begin
  TopColor := clBtnHighlight;
  if Bevel = bvLowered then
    TopColor := clBtnShadow;
  BottomColor := clBtnShadow;
  if Bevel = bvLowered then
    BottomColor := clBtnHighlight;
end;

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

// 0 = horizontal
// 1 = vertical
// 2 = horizontal center
// 3 = vertical center
// 4 = diagonal 1
// 5 = diagonal 2
// 6 = diagonal 1 center
// 7 = diagonal 2 center
// 8..11 = from corner
// 12 = from center
function FillGradient(ACanvas: TCanvas; ARect: TRect; AStartColor, AEndColor: Cardinal; ADirection: Byte): Boolean;
{$ifndef fpc}
type
  TTriVertex = record
    x: Longint;
    y: Longint;
    Red: Word;
    Green: Word;
    Blue: Word;
    Alpha: Word;
  end;
{$endif}
type
  TGradientTriangleArray = array[0..3] of TGradientTriangle;

const
  FirstCornerMode = 8;
  DirectionToMode: array[0..12] of LongInt = (
    GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V,
    GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V,
    GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE,
    GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE,
    GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE,
    GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE,
    GRADIENT_FILL_TRIANGLE);
  DirectionToVertexCount: array[0..12] of LongInt = (2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5);
  DirectionToMeshCount: array[0..12] of LongInt = (1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4);
  DefGradientTriangle: TGradientTriangleArray =
   ((Vertex1: 0; Vertex2: 1; Vertex3: 2),
   (Vertex1: 0; Vertex2: 1; Vertex3: 3),
   (Vertex1: 1; Vertex2: 2; Vertex3: 4),
   (Vertex1: 3; Vertex2: 2; Vertex3: 4));

var
  TriVertexes: array[0..4] of TTriVertex;
  GradientTriangle: TGradientTriangleArray;
  I: Integer;
{$ifndef fpc}
  WinTriVertex: array[0..3] of Windows.TTriVertex absolute TriVertexes; // windows.pas has a bug regards COLOR16 ShortInt insted of Word in D7
{$endif}
begin
  GradientTriangle := DefGradientTriangle;
  if ADirection in [0..1, 4..5] then
  begin
    TriVertexes[0].x := ARect.Left;
    TriVertexes[0].y := ARect.Top;
    TriVertexes[0].Red := GetRValue(AStartColor) shl 8;
    TriVertexes[0].Blue := GetBValue(AStartColor) shl 8;
    TriVertexes[0].Green := GetGValue(AStartColor) shl 8;
    TriVertexes[0].Alpha := 0;

    TriVertexes[1].x := ARect.Right;
    TriVertexes[1].y := ARect.Bottom;
    TriVertexes[1].Red := GetRValue(AEndColor) shl 8;
    TriVertexes[1].Blue := GetBValue(AEndColor) shl 8;
    TriVertexes[1].Green := GetGValue(AEndColor) shl 8;
    TriVertexes[1].Alpha := 0;

    if ADirection in [4..5] then
    begin
      TriVertexes[2].y := ARect.Bottom;
      TriVertexes[2].Red := ((GetRValue(AStartColor) + GetRValue(AEndColor)) div 2) shl 8;
      TriVertexes[2].Blue := ((GetBValue(AStartColor) + GetBValue(AEndColor)) div 2) shl 8;
      TriVertexes[2].Green := ((GetGValue(AStartColor) + GetGValue(AEndColor)) div 2) shl 8;
      TriVertexes[2].Alpha := 0;

      TriVertexes[3].y := ARect.Top;
      TriVertexes[3].Red := TriVertexes[2].Red;
      TriVertexes[3].Blue := TriVertexes[2].Blue;
      TriVertexes[3].Green := TriVertexes[2].Green;
      TriVertexes[3].Alpha := 0;

      if ADirection = 5 then
      begin
        TriVertexes[0].x := ARect.Right;
        TriVertexes[1].x := ARect.Left;
        TriVertexes[2].x := ARect.Right;
        TriVertexes[3].x := ARect.Left;
      end
      else
      begin
        TriVertexes[2].x := ARect.Left;
        TriVertexes[3].x := ARect.Right;
      end;
    end;
  end
  else
  if ADirection in [2..3] then
  begin
    TriVertexes[0].x := ARect.Left;
    TriVertexes[0].y := ARect.Top;
    TriVertexes[0].Red := GetRValue(AStartColor) shl 8;
    TriVertexes[0].Blue := GetBValue(AStartColor) shl 8;
    TriVertexes[0].Green := GetGValue(AStartColor) shl 8;
    TriVertexes[0].Alpha := 0;

    if ADirection = 2 then
    begin
      TriVertexes[1].x := (Arect.Left + ARect.Right) div 2;
      TriVertexes[1].y := ARect.Bottom;
      TriVertexes[2].x := TriVertexes[1].x;
      TriVertexes[2].y := ARect.Top;
    end
    else
    begin
      TriVertexes[1].x := ARect.Right;
      TriVertexes[1].y := (ARect.Top + ARect.Bottom) div 2;
      TriVertexes[2].x := ARect.Left;
      TriVertexes[2].y := TriVertexes[1].y;
    end;

    TriVertexes[1].Red := GetRValue(AEndColor) shl 8;
    TriVertexes[1].Blue := GetBValue(AEndColor) shl 8;
    TriVertexes[1].Green := GetGValue(AEndColor) shl 8;
    TriVertexes[1].Alpha := 0;

    TriVertexes[2].Red := TriVertexes[1].Red;
    TriVertexes[2].Blue := TriVertexes[1].Blue;
    TriVertexes[2].Green := TriVertexes[1].Green;
    TriVertexes[2].Alpha := 0;

    TriVertexes[3].x := ARect.Right;
    TriVertexes[3].y := ARect.Bottom;
    TriVertexes[3].Red := TriVertexes[0].Red;
    TriVertexes[3].Blue := TriVertexes[0].Blue;
    TriVertexes[3].Green := TriVertexes[0].Green;
    TriVertexes[3].Alpha := 0;

    GradientTriangle[1].Vertex1 := 3;
  end
  else
  if ADirection in [6..7] then
  begin
    TriVertexes[0].x := ARect.Left;
    TriVertexes[0].y := ARect.Top;
    TriVertexes[0].Red := GetRValue(AStartColor) shl 8;
    TriVertexes[0].Blue := GetBValue(AStartColor) shl 8;
    TriVertexes[0].Green := GetGValue(AStartColor) shl 8;
    TriVertexes[0].Alpha := 0;

    TriVertexes[1].x := ARect.Right;
    TriVertexes[1].y := ARect.Bottom;
    TriVertexes[1].Red := TriVertexes[0].Red;
    TriVertexes[1].Blue := TriVertexes[0].Blue;
    TriVertexes[1].Green := TriVertexes[0].Green;
    TriVertexes[1].Alpha := 0;

    TriVertexes[2].y := ARect.Bottom;
    TriVertexes[2].Red := GetRValue(AEndColor) shl 8;
    TriVertexes[2].Blue := GetBValue(AEndColor) shl 8;
    TriVertexes[2].Green := GetGValue(AEndColor) shl 8;
    TriVertexes[2].Alpha := 0;

    TriVertexes[3].y := ARect.Top;
    TriVertexes[3].Red := TriVertexes[2].Red;
    TriVertexes[3].Blue := TriVertexes[2].Blue;
    TriVertexes[3].Green := TriVertexes[2].Green;
    TriVertexes[3].Alpha := 0;

    if ADirection = 7 then
    begin
      TriVertexes[0].x := ARect.Right;
      TriVertexes[1].x := ARect.Left;
      TriVertexes[2].x := ARect.Right;
      TriVertexes[3].x := ARect.Left;
    end
    else
    begin
      TriVertexes[2].x := ARect.Left;
      TriVertexes[3].x := ARect.Right;
    end;
  end
  else
  if ADirection in [FirstCornerMode..FirstCornerMode+3] then
  begin
    for I := 0 to 3 do
      if (I <> ADirection - FirstCornerMode) then
        with TriVertexes[I] do
        begin
          Red := GetRValue(AEndColor) shl 8;
          Blue := GetBValue(AEndColor) shl 8;
          Green := GetGValue(AEndColor) shl 8;
          Alpha := 0;
        end;
    with TriVertexes[ADirection - FirstCornerMode] do
    begin
      Red := GetRValue(AStartColor) shl 8;
      Blue := GetBValue(AStartColor) shl 8;
      Green := GetGValue(AStartColor) shl 8;
      Alpha := 0;
    end;
    with ARect do
    begin
      TriVertexes[0].x := Left;
      TriVertexes[0].y := Top;

      TriVertexes[1].x := Right;
      TriVertexes[1].y := Top;

      TriVertexes[2].x := Right;
      TriVertexes[2].y := Bottom;

      TriVertexes[3].x := Left;
      TriVertexes[3].y := Bottom;
    end;

    GradientTriangle[0].Vertex1 := ADirection - FirstCornerMode;
    GradientTriangle[0].Vertex2 := (ADirection - FirstCornerMode + 1) mod 4;
    GradientTriangle[0].Vertex3 := (ADirection - FirstCornerMode + 2) mod 4;
    GradientTriangle[1].Vertex1 := ADirection - FirstCornerMode;
    GradientTriangle[1].Vertex2 := (ADirection - FirstCornerMode + 3) mod 4;
    GradientTriangle[1].Vertex3 := (ADirection - FirstCornerMode + 2) mod 4;
  end
  else
  if ADirection = 12 then
  begin
    for I := 0 to 3 do
      with TriVertexes[I] do
      begin
        Red := GetRValue(AStartColor) shl 8;
        Blue := GetBValue(AStartColor) shl 8;
        Green := GetGValue(AStartColor) shl 8;
        Alpha := 0;
      end;

    TriVertexes[0].x := ARect.Left;
    TriVertexes[0].y := ARect.Top;

    TriVertexes[1].x := ARect.Right;
    TriVertexes[1].y := ARect.Top;

    TriVertexes[2].x := ARect.Right;
    TriVertexes[2].y := ARect.Bottom;

    TriVertexes[3].x := ARect.Left;
    TriVertexes[3].y := ARect.Bottom;

    TriVertexes[4].x := (ARect.Left + ARect.Right) div 2;
    TriVertexes[4].y := (ARect.Top + ARect.Bottom) div 2;
    TriVertexes[4].Red := GetRValue(AEndColor) shl 8;
    TriVertexes[4].Blue := GetBValue(AEndColor) shl 8;
    TriVertexes[4].Green := GetGValue(AEndColor) shl 8;
    TriVertexes[4].Alpha := 0;

    GradientTriangle[0].Vertex3 := 4;
    GradientTriangle[1].Vertex2 := 4;
  end;

  {$ifndef fpc}
  {$ifndef DELPHI_9UP}
  Result := Windows.GradientFill(ACanvas.Handle, WinTriVertex[0],
    DirectionToVertexCount[ADirection], @GradientTriangle[0],
    DirectionToMeshCount[ADirection], DirectionToMode[ADirection]);
  {$else}
  Result := Windows.GradientFill(ACanvas.Handle, @WinTriVertex,
    DirectionToVertexCount[ADirection], @GradientTriangle[0],
    DirectionToMeshCount[ADirection], DirectionToMode[ADirection]);
  {$endif}
  {$else}
  Result := LCLIntf.GradientFill(ACanvas.Handle, @TriVertexes,
    DirectionToVertexCount[ADirection], @GradientTriangle[0],
    DirectionToMeshCount[ADirection], DirectionToMode[ADirection]);
  {$endif}
end;

procedure ImageListScaleDraw(ACanvas: TCanvas; AImageList: TCustomImageList; AImageIndex: Integer; R: TRect);
var
  Icon: TIcon;
begin
  if (R.Right - R.Left = AImageList.Width) and
     (R.Bottom - R.Top = AImageList.Height) then
    AImageList.Draw(ACanvas, R.Left, R.Top, AImageIndex)
  else
  begin
    Icon := TIcon.Create;
    try
      AImageList.GetIcon(AImageIndex, Icon);
      {$ifndef fpc}
      DrawIconEx(ACanvas.Handle, R.Left, R.Top, Icon.Handle, R.Right - R.Left, R.Bottom - R.Top, 0, 0, DI_NORMAL);
      {$else}
      ACanvas.StretchDraw(R, Icon);
      {$endif}
    finally
      Icon.Free;
    end;
  end;
end;

procedure CorrectFPCBitmap(ABitmap: TBitmap);
var
  Stream: TStream;
begin
  // in order to get fully correct TBitmap we should save it and load
  Stream := TMemoryStream.Create;
  ABitmap.SaveToStream(Stream);
  Stream.Position := 0;
  ABitmap.LoadFromStream(Stream);
  Stream.Free;
end;

function LoadBitmap(ResourceName: String): TBitmap;
begin
  Result := TBitmap.Create;
  Result.TransparentColor := clFuchsia;
  LoadBitmap(Result, ResourceName);
end;

procedure LoadBitmap(ABitmap: TBitmap; ResourceName: String);
begin
{$IFDEF FPC}
  ABitmap.LoadFromResourceName(hInstance, ResourceName);
{$ELSE}
  ABitmap.Handle := Windows.LoadBitmap(hInstance, PChar(ResourceName));
{$ENDIF}
end;

procedure LoadImageList(IL: TImageList; const ImageName: String; MaskColor: TColor = clNone; NumAsOne: Integer = 1);
var
  Images,
  OneImage: TBitmap;
  I: Integer;
  Source,
  Dest: TRect;
begin
  try
    // Since we want the image list appearing in the correct system colors, we have to remap its colors.
    Images := LoadBitmap(ImageName);
    OneImage := TBitmap.Create;
    OneImage.TransparentMode := tmFixed;

    try
      if IL.Count = 0 then
      begin
        IL.Height := Images.Height;
        IL.Width := Images.Height * NumAsOne;
      end;
      OneImage.Width := IL.Width;
      OneImage.Height := IL.Height;

      if MaskColor = clNone then
        MaskColor := Images.Canvas.Pixels[0, 0]; // this is usually clFuchsia
      Dest := Rect(0, 0, IL.Width, IL.Height);
      for I := 0 to ((Images.Width div NumAsOne) div Images.Height) - 1 do
      begin
        Source := Rect(I * IL.Width, 0, (I + 1) * IL.Width, IL.Height);
        OneImage.Canvas.CopyRect(Dest, Images.Canvas, Source);
        IL.AddMasked(OneImage, MaskColor);
      end;
    finally
      Images.Free;
      OneImage.Free;
    end;
  finally
  end;
end;

procedure LoadHiResImages(ImageList: TImageList; Size: Integer; ResName: String; MaskColor: TColor = clFuchsia; NumAsOne: Integer = 1);
{$IFNDEF FPC}
var
  Flags: Cardinal;
{$ENDIF}
begin
{$IFNDEF FPC}
  if ImageList.Count = 0 then // recreate hi res image list
  begin
    if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
      Flags := ILC_COLOR32 or ILC_MASK
    else
      Flags := ILC_COLOR16 or ILC_MASK;
    ImageList.Handle := ImageList_Create(Size * NumAsOne, Size, Flags, 0, 4);
  end;
{$ENDIF}
  LoadImageList(ImageList, ResName, MaskColor, NumAsOne);
end;

procedure BevelLine(ACanvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
var
  AOldCol: TColor;
begin
  AOldCol := ACanvas.Pen.Color;
  ACanvas.Pen.Color := C;
  MoveToEx(ACanvas.Handle, X1, Y1, nil);
  LineTo(ACanvas.Handle, X2, Y2);
  ACanvas.Pen.Color := AOldCol;
end;

function CreateRotatedFont(ACanvas: TCanvas; AAngle: integer): HFONT;
var
  lf: TLogFont;
begin
  {Создаем описание для нового шрифта.}
  with lf, ACanvas.Font do
  begin
  {Устанавливаем текущие для объекта Font параметры, кроме углов.}
    lfHeight := Height;
    lfWidth := 0;
    lfEscapement := AAngle*10;  {Угол наклона строки в 0.1 градуса}
    lfOrientation := AAngle*10; {Угол наклона символов в строке в 0.1 градуса}
    if fsBold in Style then
      lfWeight := FW_BOLD else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in Style);
    lfUnderline := Byte(fsUnderline in Style);
    lfStrikeOut := Byte(fsStrikeOut in Style);
    lfCharSet := Charset;

    if AnsiCompareText(Name, 'Default') = 0 then
      StrPCopy(lfFaceName, DefFontData.Name)
    else
      StrPCopy(lfFaceName, Name);
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_TT_ONLY_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  {Создаем новый шрифт}
  Result := CreateFontIndirect(lf);
end;

function HTMLRGBColor(Color: TColor): string;
var
  TheRgbValue: TColor;
begin
  TheRgbValue := ColorToRGB(Color);
  Result := '#' + Format('%.2x%.2x%.2x', [GetRValue(TheRGBValue), GetGValue(TheRGBValue), GetBValue(TheRGBValue)]);
end;

{$IFNDEF FPC}
function CreateAlphaBlendedBitmap(Source: TBitmap; R: TRect; Alpha, AColor: DWord): TBitmap;

  procedure AlphaBlendLine(ScanLine: Pointer; Width: Integer; Alpha, Color: DWord);
  //target = (alpha * color + (256 - alpha) * target) / 256
  type
    TBGRA = array[0..3] of byte;
    PBGRA = ^TBGRA;
  var
    i: integer;
    C1, C2: DWord;
    Run: PBGRA;
    BGRA1: TBGRA absolute Color;
  begin
    BGRA1[0] := BGRA1[0] xor BGRA1[2];
    BGRA1[2] := BGRA1[0] xor BGRA1[2];
    BGRA1[0] := BGRA1[0] xor BGRA1[2];
    C1 := Alpha * Color;
    C2 := (256 - Alpha);
    Run := ScanLine;
    for i := 0 to Width - 1 do
    begin
      Run[0] := (C1 + C2 * Run[0]) shr 8;
      Run[1] := (C1 + C2 * Run[1]) shr 8;
      Run[2] := (C1 + C2 * Run[2]) shr 8;
      inc(Run);
    end;
  end;

var
  Y: Integer;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32Bit;
  Result.Width := R.Right - R.Left;
  Result.Height := R.Bottom - R.Top;

  with Result do
    BitBlt(Canvas.Handle, 0, 0, Width, Height, Source.Canvas.Handle, 0,
      0, SRCCOPY);

  if not IsRectEmpty(R) then
  begin
    for Y := R.Top to R.Bottom - 1 do
    begin
      AlphaBlendLine(Result.ScanLine[Y], R.Right - R.Left, Alpha, AColor);
    end;
  end;
end;
{$ENDIF}

{$IFNDEF DELPHI_6UP}
threadvar
  CachedRGBToHLSclrRGB: COLORREF;
  CachedRGBToHLSHue: WORD;
  CachedRGBToHLSLum: WORD;
  CachedRGBToHLSSat: WORD;
threadvar
  CachedHighlightLum: Integer;
  CachedHighlightColor,
  CachedHighlight: TColor;
  CachedShadowLum: Integer;
  CachedShadowColor,
  CachedShadow: TColor;
  CachedColorValue: Integer;
  CachedLumValue: Integer;
  CachedColorAdjustLuma: TColor;
const
  HLSMAX = 240;            // H,L, and S vary over 0-HLSMAX
  RGBMAX = 255;            // R,G, and B vary over 0-RGBMAX
                           // HLSMAX BEST IF DIVISIBLE BY 6
                           // RGBMAX, HLSMAX must each fit in a byte.
  HLSUndefined = (HLSMAX*2/3);

function HueToRGB(Lum, Sat, Hue: Double): Integer;
var
  ResultEx: Double;
begin
  { range check: note values passed add/subtract thirds of range }
  if (hue < 0) then
     hue := hue + HLSMAX;

  if (hue > HLSMAX) then
     hue := hue - HLSMAX;

  { return r,g, or b value from this tridrant }
  if (hue < (HLSMAX/6)) then
    ResultEx := Lum + (((Sat-Lum)*hue+(HLSMAX/12))/(HLSMAX/6))
  else if (hue < (HLSMAX/2)) then
    ResultEx := Sat
  else if (hue < ((HLSMAX*2)/3)) then
    ResultEx := Lum + (((Sat-Lum)*(((HLSMAX*2)/3)-hue)+(HLSMAX/12))/(HLSMAX/6))
  else
    ResultEx := Lum;
  Result := Round(ResultEx);
end;
  
function GetHighLightColor(const Color: TColor; Luminance: Integer): TColor;
var
  H, L, S: Word;
  Clr: Cardinal;
begin
  if (Color = CachedHighlightColor) and (Luminance = CachedHighlightLum) then
    Result := CachedHighlight
  else
  begin
    // Case for default luminance
    if (Color = clBtnFace) and (Luminance = 19) then
      Result := clBtnHighlight
    else
    begin
      Clr := ColorToRGB(Color);
      ColorRGBToHLS(Clr, H, L, S);
      if S > 220 then
        Result := ColorHLSToRGB(H, L - Luminance, S)
      else
        Result := TColor(ColorAdjustLuma(Clr, Luminance, False));
      CachedHighlightLum := Luminance;
      CachedHighlightColor := Color;
      CachedHighlight := Result;
    end;
  end;
end;

function GetShadowColor(const Color: TColor; Luminance: Integer): TColor;
var
  H, L, S: Word;
  Clr: Cardinal;
begin
  if (Color = CachedShadowColor) and (Luminance = CachedShadowLum) then
    Result := CachedShadow
  else
  begin
    // Case for default luminance
    if (Color = clBtnFace) and (Luminance = -50) then
      Result := clBtnShadow
    else
    begin
      Clr := ColorToRGB(Color);
      ColorRGBToHLS(Clr, H, L, S);
      if S >= 160 then
        Result := ColorHLSToRGB(H, L + Luminance, S)
      else
        Result := TColor(ColorAdjustLuma(Clr, Luminance, False));
    end;
    CachedShadowLum := Luminance;
    CachedShadowColor := Color;
    CachedShadow := Result;
  end;
end;

procedure ColorRGBToHLS(clrRGB: COLORREF; var Hue, Luminance, Saturation: Word);
var
  H, L, S: Double;
  R, G, B: Word;
  cMax, cMin: Double;
  Rdelta, Gdelta, Bdelta: Extended; { intermediate value: % of spread from max }
begin
  if clrRGB = CachedRGBToHLSclrRGB then
  begin
    Hue := CachedRGBToHLSHue;
    Luminance := CachedRGBToHLSLum;
    Saturation := CachedRGBToHLSSat;
    exit;
  end;
  R := GetRValue(clrRGB);
  G := GetGValue(clrRGB);
  B := GetBValue(clrRGB);

  { calculate lightness }
  cMax := Math.Max(Math.Max(R, G), B);
  cMin := Math.Min(Math.Min(R, G), B);
  L := ( ((cMax + cMin) * HLSMAX) + RGBMAX ) / ( 2 * RGBMAX);
  if cMax = cMin then  { r=g=b --> achromatic case }
  begin                { saturation }
    Hue := Round(HLSUndefined);
//    pwHue := 160;      { MS's ColorRGBToHLS always defaults to 160 in this case }
    Luminance := Round(L);
    Saturation := 0;
  end
  else                 { chromatic case }
  begin
    { saturation }
    if L <= HLSMAX/2 then
      S := ( ((cMax-cMin)*HLSMAX) + ((cMax+cMin)/2) ) / (cMax+cMin)
    else
      S := ( ((cMax-cMin)*HLSMAX) + ((2*RGBMAX-cMax-cMin)/2) ) / (2*RGBMAX-cMax-cMin);

    { hue }
    Rdelta := ( ((cMax-R)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
    Gdelta := ( ((cMax-G)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);
    Bdelta := ( ((cMax-B)*(HLSMAX/6)) + ((cMax-cMin)/2) ) / (cMax-cMin);

    if (R = cMax) then
      H := Bdelta - Gdelta
    else if (G = cMax) then
      H := (HLSMAX/3) + Rdelta - Bdelta
    else // B == cMax
      H := ((2 * HLSMAX) / 3) + Gdelta - Rdelta;

    if (H < 0) then
      H := H + HLSMAX;
    if (H > HLSMAX) then
      H := H - HLSMAX;
    Hue := Round(H);
    Luminance := Round(L);
    Saturation := Round(S);
  end;
  CachedRGBToHLSclrRGB := clrRGB;
  CachedRGBToHLSHue := Hue;
  CachedRGBToHLSLum := Luminance;
  CachedRGBToHLSSat := Saturation;
end;

function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;

  function RoundColor(Value: Double): Integer;
  begin
    if Value > 255 then
      Result := 255
    else
      Result := Round(Value);
  end;

var
  R,G,B: Double;               { RGB component values }
  Magic1,Magic2: Double;       { calculated magic numbers (really!) }
begin
  if (Saturation = 0) then
  begin            { achromatic case }
     R := (Luminance * RGBMAX)/HLSMAX;
     G := R;
     B := R;
     if (Hue <> HLSUndefined) then
       ;{ ERROR }
  end
  else
  begin            { chromatic case }
     { set up magic numbers }
     if (Luminance <= (HLSMAX/2)) then
        Magic2 := (Luminance * (HLSMAX + Saturation) + (HLSMAX/2)) / HLSMAX
     else
        Magic2 := Luminance + Saturation - ((Luminance * Saturation) + (HLSMAX/2)) / HLSMAX;
     Magic1 := 2 * Luminance - Magic2;

     { get RGB, change units from HLSMAX to RGBMAX }
     R := (HueToRGB(Magic1,Magic2,Hue+(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX;
     G := (HueToRGB(Magic1,Magic2,Hue)*RGBMAX + (HLSMAX/2)) / HLSMAX;
     B := (HueToRGB(Magic1,Magic2,Hue-(HLSMAX/3))*RGBMAX + (HLSMAX/2))/HLSMAX;
  end;
  Result := RGB(RoundColor(R), RoundColor(G), RoundColor(B));
end;

function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;
var
  H, L, S: Word;
begin
  if (clrRGB = CachedColorValue) and (n = CachedLumValue) then
    Result := CachedColorAdjustLuma
  else
  begin
    ColorRGBToHLS(ColorToRGB(clrRGB), H, L, S);
    Result := TColor(ColorHLSToRGB(H, L + n, S));
    CachedColorValue := clrRGB;
    CachedLumValue := n;
    CachedColorAdjustLuma := Result;
  end;
end;

initialization
  CachedHighlightLum := 0;
  CachedHighlightColor := 0;
  CachedHighlight := 0;
  CachedShadowLum := 0;
  CachedShadowColor := 0;
  CachedShadow := 0;
  CachedColorValue := 0;
  CachedLumValue := 0;
  CachedColorAdjustLuma := 0;

{$ENDIF}

end.




