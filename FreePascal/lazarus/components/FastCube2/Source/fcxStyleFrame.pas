{*******************************************************}
{                                                       }
{            FastCube 2 Style editor unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
unit fcxStyleFrame;

interface

{$I fcx.inc}

uses
{$IFDEF FPC}
  Types, LCLType, ColorBox,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, CheckLst, ExtCtrls,
  fcxTypes, fcxStyles, fcxGridPainters;

type
  TfcxStyleEditorFrame = class(TFrame)
    lblFillStyle: TLabel;
    cbFillStyle: TComboBox;
    lblFillColor1: TLabel;
    cbFillColor1: TColorBox;
    lblFillColor2: TLabel;
    cbFillColor2: TColorBox;
    lblTextColor: TLabel;
    cbTextColor: TColorBox;
    PaintBox1: TPaintBox;
    lblExample: TLabel;
    cbTextStyle: TCheckListBox;
    lblTextStyle: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure cbFillStyleChange(Sender: TObject);
    procedure PaintBox1DblClick(Sender: TObject);
  private
    FStyle: TfcxCustomThemeStyle;
    FLocalized: Boolean;
    function GetStyle: TfcxCustomThemeStyle;
    procedure SetStyle(const Value: TfcxCustomThemeStyle);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Localize;
    class function Edit(AStyle: TfcxCustomThemeStyle): Boolean;
    property Style: TfcxCustomThemeStyle read GetStyle write SetStyle;
  end;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}  
{$ENDIF}

uses
  fcxRes;

{ TfcxStyleEditorFrame }

constructor TfcxStyleEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FLocalized := False;
end;

function TfcxStyleEditorFrame.GetStyle: TfcxCustomThemeStyle;
begin
  Result := FStyle;
end;

procedure TfcxStyleEditorFrame.Localize;
const
  sFontStyle: array[TFontStyle] of TfcxString = (
 { fsBold      } 'sBold',
 { fsItalic    } 'sItalic',
 { fsUnderline } 'sUnderline',
 { fsStrikeOut } 'sStrikeOut'
  );
  sGradientStyle: array[TfcxThemeGradientDirection] of TfcxString = (
 { tgdNone             } 'sSolidColor',
 { tgdHorizontal       } 'sHorzGradient',
 { tgdVertical         } 'sVertGradient',
 { tgdHorizontalCenter } 'sHorzCenterGradient',
 { tgdVerticalCenter   } 'sVertCenterGradient',
 { tgdDiagonal1        } 'sDiag1Gradient',
 { tgdDiagonal2        } 'sDiag2Gradient',
 { tgdDiagonal1Center  } 'sDiag1CenterGradient',
 { tgdDiagonal2Center  } 'sDiag2CenterGradient',
 { tgdCorner1          } 'sCorner1Gradient',
 { tgdCorner2          } 'sCorner2Gradient',
 { tgdCorner3          } 'sCorner3Gradient',
 { tgdCorner4          } 'sCorner4Gradient',
 { tgdCenter           } 'sCenterGradient'
  );
var
  fs: TFontStyle;
  gs: TfcxThemeGradientDirection;
begin
  if FLocalized then
    Exit;

  FLocalized := True;

  cbFillColor1.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames];
  cbFillColor2.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames];
  cbTextColor.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames];

  lblFillStyle.Caption := fcxResources.Get('sFillStyle') + ':';
  lblFillColor1.Caption := Format(fcxResources.Get('sFillColor'), [1]) + ':';
  lblFillColor2.Caption := Format(fcxResources.Get('sFillColor'), [2]) + ':';
  lblTextStyle.Caption := fcxResources.Get('sTextStyle') + ':';
  lblTextColor.Caption := fcxResources.Get('sTextColor') + ':';
  lblExample.Caption := fcxResources.Get('sExample') + ':';

  for gs := Low(gs) to High(gs) do
    cbFillStyle.Items.Add(fcxResources.Get(sGradientStyle[gs]));

  for fs := Low(fs) to High(fs) do
    cbTextStyle.Items.Add(fcxResources.Get(sFontStyle[fs]));
end;

procedure TfcxStyleEditorFrame.SetStyle(const Value: TfcxCustomThemeStyle);
var
  fs: TFontStyle;
begin
  FStyle := Value;
  cbFillStyle.ItemIndex := Ord(Style.GradientDirection);
  cbFillColor1.Selected := Style.FillColor;
  cbFillColor2.Selected := Style.GradientColor;
  cbTextColor.Selected := Style.TextColor;
  for fs := Low(fs) to High(fs) do
    cbTextStyle.Checked[Ord(fs)] := fs in Style.Font.Style;
  Changed;
  PaintBox1.Invalidate;
end;

procedure TfcxStyleEditorFrame.PaintBox1Paint(Sender: TObject);

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
  if not Assigned(Style) then
    Exit;
  R := PaintBox1.ClientRect;
  PaintStyle(PaintBox1.Canvas, R, Style);
  PaintBox1.Canvas.Font := Style.Font;
  PaintBox1.Canvas.Font.Color := Style.TextColor;
  if PaintBox1.Canvas.Font.Color = clNone then
    PaintBox1.Canvas.Font.Color := clWindowText;
  TfcxGridPainterClass.DrawText(PaintBox1.Canvas, R, fcxResources.Get('sExampleString'), DT_CENTER or DT_VCENTER or DT_SINGLELINE, Style.Font.Style);
end;

procedure TfcxStyleEditorFrame.cbFillStyleChange(Sender: TObject);
var
  fs: TFontStyle;
  fss: TFontStyles;
begin
  if not Assigned(Style) then
    Exit;
    
  Style.GradientDirection := TfcxThemeGradientDirection(cbFillStyle.ItemIndex);
  Style.FillColor := cbFillColor1.Selected;
  Style.GradientColor := cbFillColor2.Selected;
  Style.TextColor := cbTextColor.Selected;
  fss := [];
  for fs := Low(fs) to High(fs) do
    if cbTextStyle.Checked[Ord(fs)] then
      include(fss, fs);
  Style.Font.Style := fss;
  PaintBox1.Invalidate;
end;

procedure TfcxStyleEditorFrame.PaintBox1DblClick(Sender: TObject);
var
  C: TColor;
begin
  if not Assigned(Style) then
    Exit;
    
  C := Style.FillColor;
  Style.FillColor := Style.GradientColor;
  Style.GradientColor := C;
  
  cbFillColor1.Selected := Style.FillColor;
  cbFillColor2.Selected := Style.GradientColor;
  PaintBox1.Invalidate;
end;

class function TfcxStyleEditorFrame.Edit(AStyle: TfcxCustomThemeStyle): Boolean;
var
  F: TForm;
  Frame: TfcxStyleEditorFrame;
begin
  F := TForm.Create(Application);
  F.Caption := fcxResources.Get('sStyleEditor');
  F.Position := poScreenCenter;
  F.BorderStyle := bsDialog;

  Frame := TfcxStyleEditorFrame.Create(F);
  Frame.Parent := F;
  Frame.Top := 6;
  Frame.Left := 6;
  Frame.Localize;
  Frame.Style := AStyle;

  F.ClientWidth := Frame.Width + 12;

  with TButton.Create(F) do
  begin
    Caption := fcxResources.Get('sCancelBtn');
    Cancel := True;
    ModalResult := mrCancel;
    Left := F.ClientWidth - Width - 6;
    Top := Frame.Top + Frame.Height + 6;
    F.ClientHeight := Top + Height + 6;
    Parent := F;
  end;

  with TButton.Create(F) do
  begin
    Caption := fcxResources.Get('sOkBtn');
    Default := True;
    ModalResult := mrOk;
    Left := F.ClientWidth - Width * 2 - 12;
    Top := Frame.Top + Frame.Height + 6;
    Parent := F;
  end;

  Result := F.ShowModal = mrOk
end;

end.
