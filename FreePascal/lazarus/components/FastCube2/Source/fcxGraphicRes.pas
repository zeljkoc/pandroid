{*******************************************************}
{                                                       }
{             FastCube 2 resources unit                 }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxGraphicRes;
{$I fcx.inc}
interface
uses
{$IFNDEF FPC}
  Windows,
  CommCtrl
{$ELSE}
  LCLType,
  InterfaceBase
{$ENDIF},
  ImgList,
  Classes,
  SysUtils,
  Controls,
  Graphics;

type

  TfcxImageList = TCustomImageList;

  { TfcxResources }

  TfcxGraphicResources = class(TObject)
  private
    FFRBitmaps: array[0..3] of TBitmap;
    FDisabledButtonImages: TImageList;
    FMainButtonImages: TImageList;
    FScriptImages: TImageList;
    FToolmages: TImageList;
    FChartImages: TImageList;
    FPopupUtilImages: TImageList;
    FFRImages: TImageList;
    FFieldsImages: TImageList;
    FHighlightsImages: TImageList;
    FContinuousHighlightsImages: TImageList;

    function GetMainButtonImages: TImageList;
    function GetToolImages: TImageList;
    function GetChartImages: TImageList;
    function GetFRImages: TImageList;
    function GetPopupUtilImages: TImageList;
    function GetFieldsImages: TImageList;
    function GetHighlightsImages: TImageList;
    function GetContinuousHighlightsImages: TImageList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetButtonImages(Images: TBitmap; AClear: Boolean = False);
    procedure SetObjectImages(Images: TBitmap; AClear: Boolean = False);
    function GetFRBitmap(BitmapIndex: Integer): TBitmap;

    property DisabledButtonImages: TImageList read FDisabledButtonImages;
    property MainButtonImages: TImageList read GetMainButtonImages;
    property ToolImages: TImageList read GetToolImages;
    property ChartImages: TImageList read GetChartImages;
    property PopupUtilImages: TImageList read GetPopupUtilImages;
    property FRImages: TImageList read GetFRImages;
    property FieldsImages: TImageList read GetFieldsImages;
    property HighlightsImages: TImageList read GetHighlightsImages;
    property ContinuousHighlightsImages: TImageList read GetContinuousHighlightsImages;
  end;

function fcxGraphicResources: TfcxGraphicResources;

implementation

{$R *.res}

uses
  fcxGraphicUtils;

var
  FGraphicResources: TfcxGraphicResources = nil;

procedure frxAssignImages(Bitmap: TBitmap; dx, dy: Integer;
  ImgList1: TImageList; ImgList2: TImageList = nil);
var
  b: TBitmap;
  x, y: Integer;
  Done: Boolean;
begin
  b := TBitmap.Create;
  b.Width := dx;
  b.Height := dy;

  x := 0; y := 0;

  repeat
    b.Canvas.CopyRect(Rect(0, 0, dx, dy), Bitmap.Canvas, Rect(x, y, x + dx, y + dy));
    Done := y > Bitmap.Height;

    if not Done then
    begin
      ImgList1.AddMasked(b, b.TransparentColor);
      if ImgList2 <> nil then
      begin
        Inc(x, dx);
        b.Canvas.CopyRect(Rect(0, 0, dx, dy), Bitmap.Canvas, Rect(x, y, x + dx, y + dy));
        ImgList2.AddMasked(b, b.TransparentColor);
      end;
    end;

    Inc(x, dx);
    if x >= Bitmap.Width then
    begin
      x := 0;
      Inc(y, dy);
    end;
  until Done;

  b.Free;
end;


{ TfrxResources }

constructor TfcxGraphicResources.Create;
var
  i: Integer;
begin
  inherited;
  for i := Low(FFRBitmaps) to High(FFRBitmaps) do
    FFRBitmaps[i] := nil;

  FDisabledButtonImages := TImageList.Create(nil);
  FMainButtonImages := TImageList.Create(nil);
  FToolmages := TImageList.Create(nil);
  FChartImages := TImageList.Create(nil);
  FFRImages := TImageList.Create(nil);
  FFieldsImages := TImageList.Create(nil);
  FHighlightsImages := TImageList.Create(nil);
  FContinuousHighlightsImages := TImageList.Create(nil);
  FPopupUtilImages := TImageList.Create(nil);
end;

destructor TfcxGraphicResources.Destroy;
var
  i: Integer;
begin
  FDisabledButtonImages.Free;
  FMainButtonImages.Free;
  FToolmages.Free;
  FChartImages.Free;
  FPopupUtilImages.Free;
  for i := Low(FFRBitmaps) to High(FFRBitmaps) do
    FFRBitmaps[i].Free;
  FFRImages.Free;
  FFieldsImages.Free;
  FHighlightsImages.Free;
  FContinuousHighlightsImages.Free;
  inherited;
end;

function TfcxGraphicResources.GetChartImages: TImageList;
begin
  if FChartImages.Count = 0 then
    LoadHiResImages(FChartImages, 16, 'FCXCHART');

  Result := FChartImages;
end;

function TfcxGraphicResources.GetFieldsImages: TImageList;
begin
  if FFieldsImages.Count = 0 then
    LoadHiResImages(FFieldsImages, 16, 'FCXFIELDS');

  Result := FFieldsImages;
end;

function TfcxGraphicResources.GetFRBitmap(BitmapIndex: Integer): TBitmap;
begin
  if FFRBitmaps[BitmapIndex] = nil then
  begin
    FFRBitmaps[BitmapIndex] := TBitmap.Create;
    FFRBitmaps[BitmapIndex].Transparent := True;
    FRImages.GetBitmap(BitmapIndex, FFRBitmaps[BitmapIndex]);
  end;
  Result := FFRBitmaps[BitmapIndex];
end;

function TfcxGraphicResources.GetFRImages: TImageList;
begin
  if FFRImages.Count = 0 then
    LoadHiResImages(FFRImages, 16, 'FCXFRIMAGES');

  Result := FFRImages;
end;

function TfcxGraphicResources.GetPopupUtilImages: TImageList;
begin
  if FPopupUtilImages.Count = 0 then
    LoadHiResImages(FPopupUtilImages, 12, 'fcxPopupUtils', clFuchsia);

  Result := FPopupUtilImages;
end;

function TfcxGraphicResources.GetToolImages: TImageList;
begin
  if FToolmages.Count = 0 then
    LoadHiResImages(FToolmages, 16, 'FCXGRID');

  Result := FToolmages;
end;

procedure TfcxGraphicResources.SetObjectImages(Images: TBitmap; AClear: Boolean = False);
begin
  if AClear then
    FScriptImages.Clear;
  frxAssignImages(Images, 16, 16, FScriptImages);
end;

function fcxGraphicResources: TfcxGraphicResources;
begin
  if FGraphicResources = nil then
    FGraphicResources := TfcxGraphicResources.Create;
  Result := FGraphicResources;
end;

function TfcxGraphicResources.GetHighlightsImages: TImageList;
begin
  if FHighlightsImages.Count = 0 then
    LoadHiResImages(FHighlightsImages, 16, 'FCXHIGHLIGHTS');

  Result := FHighlightsImages;
end;

function TfcxGraphicResources.GetContinuousHighlightsImages: TImageList;
begin
  if FContinuousHighlightsImages.Count = 0 then
    LoadHiResImages(FContinuousHighlightsImages, 16, 'FCXIMAGEHIGHLIGHT');

  Result := FContinuousHighlightsImages;
end;

function TfcxGraphicResources.GetMainButtonImages: TImageList;
var
  Images: TBitmap;
begin
  if FMainButtonImages.Count = 0 then
  begin
    Images := TBitmap.Create;
    try
      Images.LoadFromResourceName(hInstance, 'FCXBUTTONS');
      SetButtonImages(Images);
    finally
      Images.Free;
    end;
  end;

  Result := FMainButtonImages;
end;

procedure TfcxGraphicResources.SetButtonImages(Images: TBitmap; AClear: Boolean);
begin
  if AClear then
  begin
    FMainButtonImages.Clear;
    FDisabledButtonImages.Clear;
  end;
  frxAssignImages(Images, 16, 16, FMainButtonImages, FDisabledButtonImages);
end;

initialization
finalization
  FreeAndNil(FGraphicResources);
end.
