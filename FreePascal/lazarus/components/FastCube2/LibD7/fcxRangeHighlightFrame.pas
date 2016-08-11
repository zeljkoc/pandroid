{*******************************************************}
{                                                       }
{      FastCube 2 Range highlight editor unit           }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
unit fcxRangeHighlightFrame;

{$INCLUDE fcx.inc}

interface

uses
{$IFDEF FPC}
  Types, LCLType, ColorBox,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  fcxTypes, fcxRange, fcxSlice, fcxHighlights, fcxStyles, fcxGridPainters, fcxRangeFrame;

type
  TfcxRangeHighlightEditorFrame = class(TfcxCustomHighlightEditorFrame)
    Bevel1: TBevel;
    PaintBox1: TPaintBox;
    lblExample: TLabel;
    btnEditStyle: TButton;
    RangeFrame: TfcxRangeEditorFrame;
    procedure PaintBox1Paint(Sender: TObject);
    procedure btnEditStyleClick(Sender: TObject);
  private
    FLocalized: Boolean;
    procedure Localize;
  protected
    class function GetHighlightClass: TfcxGraphicHighlightClass; override;
    procedure DoHighlightChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare; override;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  fcxRes,
  fcxStyleFrame;

{ TfcxRangeHighlightEditorFrame }

constructor TfcxRangeHighlightEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FLocalized := False;
end;

procedure TfcxRangeHighlightEditorFrame.DoHighlightChange(Sender: TObject);
begin
  inherited;
  RangeFrame.Range := TfcxRangeHighlight(Highlight).Range;
end;

class function TfcxRangeHighlightEditorFrame.GetHighlightClass: TfcxGraphicHighlightClass;
begin
  Result := TfcxRangeHighlight;
end;

procedure TfcxRangeHighlightEditorFrame.Localize;
begin
  if FLocalized then
    Exit;
  FLocalized := True;
  lblExample.Caption := fcxResources.Get('sExample');
  btnEditStyle.Caption := fcxResources.Get('sEditStyle');
  RangeFrame.Localize;
end;

procedure TfcxRangeHighlightEditorFrame.PaintBox1Paint(Sender: TObject);

  procedure PaintStyle(ACanvas: TCanvas; ARect: TRect; AStyle: TfcxCustomThemeStyle);
  var
    OldStyle: TBrushStyle;
    OldColor: TColor;
  begin
    OldStyle := ACanvas.Brush.Style;
    OldColor := ACanvas.Brush.Color;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    ACanvas.Brush.Style := OldStyle;
    ACanvas.Brush.Color := OldColor;
    if (AStyle.GradientDirection <> tgdNone) or (AStyle.FillColor <> clNone) then
      AStyle.DrawBackground(ACanvas, ARect);
  end;

var
  R: TRect;
begin
  R := PaintBox1.ClientRect;
  PaintStyle(PaintBox1.Canvas, R, Highlight.Style);
  PaintBox1.Canvas.Font := Highlight.Style.Font;
  PaintBox1.Canvas.Font.Color := Highlight.Style.TextColor;
  if PaintBox1.Canvas.Font.Color = clNone then
    PaintBox1.Canvas.Font.Color := clWindowText;
  TfcxGridPainterClass.DrawText(PaintBox1.Canvas, R, fcxResources.Get('sExampleString'), DT_CENTER or DT_VCENTER or DT_SINGLELINE, Highlight.Style.Font.Style);
end;

procedure TfcxRangeHighlightEditorFrame.btnEditStyleClick(Sender: TObject);
begin
  if TfcxStyleEditorFrame.Edit(Highlight.Style) then
    PaintBox1.Invalidate;
end;

procedure TfcxRangeHighlightEditorFrame.Prepare;
begin
  inherited;
  Localize;
end;

initialization
  fcxRegisterHighlightEditor(TfcxRangeHighlight, TfcxRangeHighlightEditorFrame);

end.
