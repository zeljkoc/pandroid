{*******************************************************}
{                                                       }
{        FastCube 2 Continuous highlight editor         }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxContinuousHighlightFrame;
{$INCLUDE fcx.inc}

interface

uses
{$IFDEF FPC}
  Types, LCLType, ColorBox,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  fcxSlice, fcxHighlights, StdCtrls, ExtCtrls, fcxGraphicUtils, fcxImagesPopup;

type
  TfcxIconRuleControl = record
    Icon: TfcxImageCombo;
    Info: TLabel;
    Rule: TCombobox;
    Value: TEdit;
    ValueType: TCombobox;
  end;

  TfcxIconRuleControls = record
    lblIcon: TLabel;
    lblValue: TLabel;
    lblValueType: TLabel;
    Rows: array of TfcxIconRuleControl;
  end;

  TfcxContinuousHighlightEditorFrame = class(TfcxCustomHighlightEditorFrame)
    lblHighlightKind: TLabel;
    cbHighlightKind: TComboBox;
    lblType: TLabel;
    lblValue: TLabel;
    lblColor: TLabel;
    cbMinValueType: TComboBox;
    cbMidValueType: TComboBox;
    cbMaxValueType: TComboBox;
    lblMinValue: TLabel;
    lblMidValue: TLabel;
    lblMaxValue: TLabel;
    edMinValue: TEdit;
    edMidValue: TEdit;
    edMaxValue: TEdit;
    cbMinValueColor: TColorBox;
    cbMidValueColor: TColorBox;
    cbMaxValueColor: TColorBox;
    pbScale: TPaintBox;
    lblExample: TLabel;
    cbShowCellValue: TCheckBox;
    cbGradientDraw: TCheckBox;
    lblFrameColor: TLabel;
    cbFrameColor: TColorBox;
    lblIconSet: TLabel;
    cbIconSet: TComboBox;
    btnReverseOrder: TButton;
    procedure cbHighlightKindChange(Sender: TObject);
    procedure cbMinValueTypeChange(Sender: TObject);
    procedure cbMidValueTypeChange(Sender: TObject);
    procedure cbMaxValueTypeChange(Sender: TObject);
    procedure pbScalePaint(Sender: TObject);
    procedure cbMinValueColorChange(Sender: TObject);
    procedure cbMidValueColorChange(Sender: TObject);
    procedure cbMaxValueColorChange(Sender: TObject);
    procedure edMinValueChange(Sender: TObject);
    procedure edMidValueChange(Sender: TObject);
    procedure edMaxValueChange(Sender: TObject);
    procedure cbShowCellValueClick(Sender: TObject);
    procedure cbGradientDrawClick(Sender: TObject);
    procedure cbFrameColorChange(Sender: TObject);
    procedure cbIconSetDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbIconSetChange(Sender: TObject);
    procedure btnReverseOrderClick(Sender: TObject);
  private
    FIconRuleControls: TfcxIconRuleControls;
    FLocalized: Boolean;
    procedure Localize;

    procedure CreateIconRuleControls(ACount: Integer);
    procedure RemoveIconRuleControl(AIndex: Integer);
    procedure RemoveIconRuleControls;
    procedure RuleIconChange(Sender: TObject);
    procedure RuleRuleChange(Sender: TObject);
    procedure RuleValueChange(Sender: TObject);
    procedure RuleValueTypeChange(Sender: TObject);
  protected
    class function GetHighlightClass: TfcxGraphicHighlightClass; override;
    procedure DoHighlightChange(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare; override;
  end;

implementation

uses
  fcxRes;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

const
  ValueTypeToStr: array[TfcxContinuousHighlightValue] of String = (
  { chvBoundByRow      } '',
  { chvBoundByCol      } '',
  { chvNumber          } 'sNumber',
  { chvPercentByRow    } 'sPercentByRow',
  { chvPercentByCol    } 'sPercentByCol',
  { chvPercentileByRow } 'sPercentileByRow',
  { chvPercentileByCol } 'sPercentileByCol'
  );
  
{ TfcxContinousHighlightEditorFrame }

constructor TfcxContinuousHighlightEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FLocalized := False;
end;

procedure TfcxContinuousHighlightEditorFrame.DoHighlightChange(Sender: TObject);
begin
  if not HandleAllocated then
    Exit;
    
  cbHighlightKind.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).Kind);
  cbHighlightKindChange(nil);
  if pbScale.Visible then
    pbScale.Invalidate;
end;

class function TfcxContinuousHighlightEditorFrame.GetHighlightClass: TfcxGraphicHighlightClass;
begin
  Result := TfcxContinuousHighlight;
end;

procedure TfcxContinuousHighlightEditorFrame.Localize;
var
  Kind: TfcxContinuousHighlightKind;
  ValueType: TfcxContinuousHighlightValue;
  I: Integer;
begin
  if FLocalized then
    Exit;

  FLocalized := True;  
  lblHighlightKind.Caption := fcxResources.Get('sHighlightKind') + ':';
  for Kind := Low(TfcxContinuousHighlightKind) to High(TfcxContinuousHighlightKind) do
    cbHighlightKind.Items.Add(fcxResources.Get(KindToStr[Kind]));

  lblMinValue.Caption := fcxResources.Get('sMinValue') + ':';
  lblMidValue.Caption := fcxResources.Get('sMidValue') + ':';
  lblMaxValue.Caption := fcxResources.Get('sMaxValue') + ':';
  lblType.Caption := fcxResources.Get('sType') + ':';
  lblValue.Caption := fcxResources.Get('sValue') + ':';
  lblColor.Caption := fcxResources.Get('sColor') + ':';
  lblFrameColor.Caption := fcxResources.Get('sFrameColor') + ':';
  lblExample.Caption := fcxResources.Get('sExample') + ':';

  cbMinValueType.Items.Add(fcxResources.Get('sMinValueByRow'));
  cbMinValueType.Items.Add(fcxResources.Get('sMinValueByCol'));
  cbMaxValueType.Items.Add(fcxResources.Get('sMaxValueByRow'));
  cbMaxValueType.Items.Add(fcxResources.Get('sMaxValueByCol'));
  for ValueType := chvNumber to High(TfcxContinuousHighlightValue) do
  begin
    cbMinValueType.Items.Add(fcxResources.Get(ValueTypeToStr[ValueType]));
    cbMidValueType.Items.Add(fcxResources.Get(ValueTypeToStr[ValueType]));
    cbMaxValueType.Items.Add(fcxResources.Get(ValueTypeToStr[ValueType]));
  end;

  cbShowCellValue.Caption := fcxResources.Get('sShowCellValue');
  cbGradientDraw.Caption := fcxResources.Get('sGradientDraw');
  btnReverseOrder.Caption := fcxResources.Get('sReverseOrder');

  lblIconSet.Caption := fcxResources.Get('sIconSet') + ':';
  for I := 0 to fcxRegisteredIconSets.Count - 1 do
    cbIconSet.Items.Add('');
end;

procedure TfcxContinuousHighlightEditorFrame.cbHighlightKindChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).Kind := TfcxContinuousHighlightKind(cbHighlightKind.ItemIndex);
  with TfcxContinuousHighlight(Highlight) do
  begin
    lblMinValue.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    cbMinValueType.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    edMinValue.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    cbMinValueColor.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];

    lblMidValue.Visible := Kind in [chkThreeColorScale];
    cbMidValueType.Visible := Kind in [chkThreeColorScale];
    edMidValue.Visible := Kind in [chkThreeColorScale];
    cbMidValueColor.Visible := Kind = chkThreeColorScale;

    lblMaxValue.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    cbMaxValueType.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    edMaxValue.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    cbMaxValueColor.Visible := Kind in [chkTwoColorScale, chkThreeColorScale];

    lblType.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    lblValue.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    lblColor.Visible := Kind in [chkTwoColorScale, chkThreeColorScale, chkBarChart];
    lblFrameColor.Visible := Kind = chkBarChart;
    cbFrameColor.Visible := Kind = chkBarChart;

    cbShowCellValue.Visible := Kind in [chkBarChart, chkIconSet];
    cbGradientDraw.Visible := Kind = chkBarChart;

    lblIconSet.Visible := Kind = chkIconSet;
    cbIconSet.Visible := Kind = chkIconSet;
    btnReverseOrder.Visible := Kind = chkIconSet;
  end;

  case TfcxContinuousHighlight(Highlight).Kind of
    chkTwoColorScale,
    chkThreeColorScale:
      begin
        RemoveIconRuleControls;
        lblColor.Caption := fcxResources.Get('sColor') + ':';
        lblMinValue.Caption := fcxResources.Get('sMinValue') + ':';
        lblMaxValue.Caption := fcxResources.Get('sMaxValue') + ':';
        cbMinValueType.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).MinValueType);
        cbMinValueTypeChange(nil);
        cbMidValueType.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).MidValueType) - 2;
        cbMidValueTypeChange(nil);
        cbMaxValueType.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).MaxValueType);
        cbMaxValueTypeChange(nil);

        cbMinValueColor.Selected := TfcxContinuousHighlight(Highlight).MinValueColor;
        cbMinValueColorChange(nil);
        cbMidValueColor.Selected := TfcxContinuousHighlight(Highlight).MidValueColor;
        cbMidValueColorChange(nil);
        cbMaxValueColor.Selected := TfcxContinuousHighlight(Highlight).MaxValueColor;
        cbMaxValueColorChange(nil);
      end;
    chkBarChart:
      begin
        RemoveIconRuleControls;
        lblColor.Caption := fcxResources.Get('sBarColor') + ':';
        lblMinValue.Caption := fcxResources.Get('sTheShortestBar') + ':';
        lblMaxValue.Caption := fcxResources.Get('sTheLongestBar') + ':';

        cbMinValueType.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).MinValueType);
        cbMinValueTypeChange(nil);
        cbMaxValueType.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).MaxValueType);
        cbMaxValueTypeChange(nil);
        cbMinValueColor.Selected := TfcxContinuousHighlight(Highlight).MinValueColor;
        cbMinValueColorChange(nil);
        cbFrameColor.Selected := TfcxContinuousHighlight(Highlight).FrameColor;
        cbFrameColorChange(nil);

        cbShowCellValue.Checked := TfcxContinuousHighlight(Highlight).ShowCellValue;
        cbShowCellValueClick(nil);

        cbGradientDraw.Checked := TfcxContinuousHighlight(Highlight).GradientDraw;
        cbGradientDrawClick(nil);
      end;
    chkIconSet:
      begin
        cbShowCellValue.Checked := TfcxContinuousHighlight(Highlight).ShowCellValue;
        cbShowCellValueClick(nil);

        cbIconSet.ItemIndex := TfcxContinuousHighlight(Highlight).IconSet;
        cbIconSetChange(nil);
      end;
  end;

  case TfcxContinuousHighlight(Highlight).Kind of
    chkTwoColorScale, chkBarChart:
      begin
        lblMaxValue.Left := lblMidValue.Left;
        cbMaxValueType.Left := lblMaxValue.Left;
        edMaxValue.Left := lblMaxValue.Left;
        cbMaxValueColor.Left := lblMaxValue.Left;
        if TfcxContinuousHighlight(Highlight).Kind = chkTwoColorScale then
          pbScale.Top := cbMinValueColor.Top + cbMinValueColor.Height + 6
        else
          pbScale.Top := cbFrameColor.Top + cbFrameColor.Height + 6;
        lblExample.Top := (pbScale.Top * 2 + pbScale.Height - lblExample.Height) div 2;          
        SetBounds(Left, Top, cbMaxValueType.Left + cbMaxValueType.Width + 6, pbScale.Top + pbScale.Height + 6);
        pbScale.Width := ClientWidth - pbScale.Left - 6;
      end;
    chkThreeColorScale:
      begin
        lblMaxValue.Left := cbMidValueType.Left + cbMidValueType.Width + 6;
        cbMaxValueType.Left := lblMaxValue.Left;
        edMaxValue.Left := lblMaxValue.Left;
        cbMaxValueColor.Left := lblMaxValue.Left;
        pbScale.Top := cbMinValueColor.Top + cbMinValueColor.Height + 6;
        lblExample.Top := (pbScale.Top * 2 + pbScale.Height - lblExample.Height) div 2;
        SetBounds(Left, Top, cbMaxValueType.Left + cbMaxValueType.Width + 6, pbScale.Top + pbScale.Height + 6);
        pbScale.Width := ClientWidth - pbScale.Left - 6;
      end;
    chkIconSet:
      begin
        with FIconRuleControls.Rows[0].Icon do
          pbScale.Top := Top + Height + 6;
        lblExample.Top := (pbScale.Top * 2 + pbScale.Height - lblExample.Height) div 2;
        SetBounds(Left, Top,
          FIconRuleControls.Rows[1].ValueType.Left + FIconRuleControls.Rows[1].ValueType.Width + 6,
          pbScale.Top + pbScale.Height + 6);
        pbScale.Width := ClientWidth - pbScale.Left - 6;
      end;
  end;  
end;

procedure TfcxContinuousHighlightEditorFrame.cbMinValueTypeChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MinValueType := TfcxContinuousHighlightValue(cbMinValueType.ItemIndex);
  if TfcxContinuousHighlight(Highlight).MinValueType in [chvBoundByRow, chvBoundByCol] then
  begin
    edMinValue.Text := fcxResources.Get('sMinValue');
    edMinValue.Enabled := False;
  end
  else
  begin
    edMinValue.Text := FloatToStr(TfcxContinuousHighlight(Highlight).MinValue);
    edMinValue.Enabled := True;
  end;
end;

procedure TfcxContinuousHighlightEditorFrame.cbMidValueTypeChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MidValueType := TfcxContinuousHighlightValue(cbMidValueType.ItemIndex + 2);
  edMidValue.Text := FloatToStr(TfcxContinuousHighlight(Highlight).MidValue);
end;

procedure TfcxContinuousHighlightEditorFrame.cbMaxValueTypeChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MaxValueType := TfcxContinuousHighlightValue(cbMaxValueType.ItemIndex);
  if TfcxContinuousHighlight(Highlight).MaxValueType in [chvBoundByRow, chvBoundByCol] then
  begin
    edMaxValue.Text := fcxResources.Get('sMaxValue');
    edMaxValue.Enabled := False;
  end
  else
  begin
    edMaxValue.Text := FloatToStr(TfcxContinuousHighlight(Highlight).MaxValue);
    edMaxValue.Enabled := True;
  end;
end;

procedure TfcxContinuousHighlightEditorFrame.pbScalePaint(Sender: TObject);

  procedure PaintRect(ACanvas: TCanvas; ARect: TRect);
  var
    OldStyle: TBrushStyle;
    OldColor: TColor;
  begin
    OldStyle := ACanvas.Brush.Style;
    OldColor := ACanvas.Brush.Color;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Brush.Style := OldStyle;
    ACanvas.Brush.Color := OldColor;
  end;

var
  R: TRect;
begin
  R := pbScale.ClientRect;
  pbScale.Canvas.Rectangle(R);
  inflateRect(R, -2, -2);
  Highlight.DrawExample(pbScale.Canvas, R);
end;

procedure TfcxContinuousHighlightEditorFrame.cbMinValueColorChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MinValueColor := cbMinValueColor.Selected;
end;

procedure TfcxContinuousHighlightEditorFrame.cbMidValueColorChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MidValueColor := cbMidValueColor.Selected;
end;

procedure TfcxContinuousHighlightEditorFrame.cbMaxValueColorChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MaxValueColor := cbMaxValueColor.Selected;
end;

procedure TfcxContinuousHighlightEditorFrame.edMinValueChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MinValue := StrToFloatDef(edMinValue.Text, 0);
end;

procedure TfcxContinuousHighlightEditorFrame.edMidValueChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MidValue := StrToFloatDef(edMidValue.Text, 0);
end;

procedure TfcxContinuousHighlightEditorFrame.edMaxValueChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).MaxValue := StrToFloatDef(edMaxValue.Text, 0);
end;

procedure TfcxContinuousHighlightEditorFrame.Prepare;
begin
  HandleNeeded;

  Localize;
  // set style here because on frame create they start handle creation which lead to error
  cbMinValueColor.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames];
  cbMidValueColor.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames];
  cbMaxValueColor.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor, cbPrettyNames];

  cbMinValueColor.HandleNeeded;
  cbMaxValueColor.HandleNeeded;
  cbMidValueColor.HandleNeeded;
  cbFrameColor.HandleNeeded;
  DoHighlightChange(nil);
end;

procedure TfcxContinuousHighlightEditorFrame.cbShowCellValueClick(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).ShowCellValue := cbShowCellValue.Checked;
end;

procedure TfcxContinuousHighlightEditorFrame.cbGradientDrawClick(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).GradientDraw := cbGradientDraw.Checked;
end;

procedure TfcxContinuousHighlightEditorFrame.cbFrameColorChange(Sender: TObject);
begin
  TfcxContinuousHighlight(Highlight).FrameColor := cbFrameColor.Selected;
end;

procedure TfcxContinuousHighlightEditorFrame.cbIconSetDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  with cbIconSet.Canvas do
    FillRect(Rect);
  if Index = -1 then
    Index := TfcxContinuousHighlight(Highlight).IconSet;
  TfcxContinuousHighlight(Highlight).DrawIconSet(cbIconSet.Canvas, Rect, Index);
end;

procedure TfcxContinuousHighlightEditorFrame.cbIconSetChange(Sender: TObject);
const
  BoolToRuleIndex: array[Boolean] of Integer = (1, 0);
var
  I: Integer;
begin
  TfcxContinuousHighlight(Highlight).IconSet := cbIconSet.ItemIndex;
  CreateIconRuleControls(TfcxContinuousHighlight(Highlight).ImageCount);
  for I := 0 to TfcxContinuousHighlight(Highlight).ImageCount - 1 do
  begin
    FIconRuleControls.Rows[I].Icon.ImageIndex := TfcxContinuousHighlight(Highlight).ImageIndex[I];
    FIconRuleControls.Rows[I].Icon.OnChange(FIconRuleControls.Rows[I].Icon);

    FIconRuleControls.Rows[I].Info.Caption := TfcxContinuousHighlight(Highlight).ImageInfo[I];

    if Assigned(FIconRuleControls.Rows[I].Rule) then
    begin
      FIconRuleControls.Rows[I].Rule.ItemIndex := BoolToRuleIndex[TfcxContinuousHighlight(Highlight).ImageValueGE[I]];
      FIconRuleControls.Rows[I].Rule.OnChange(FIconRuleControls.Rows[I].Rule);
    end;
    if Assigned(FIconRuleControls.Rows[I].Value) then
    begin
      FIconRuleControls.Rows[I].Value.Text := FloatToStr(TfcxContinuousHighlight(Highlight).ImageValue[I]);
      FIconRuleControls.Rows[I].Value.OnChange(FIconRuleControls.Rows[I].Value);
    end;
    if Assigned(FIconRuleControls.Rows[I].ValueType) then
    begin
      FIconRuleControls.Rows[I].ValueType.ItemIndex := Ord(TfcxContinuousHighlight(Highlight).ImageValueType[I]) - 2;
      FIconRuleControls.Rows[I].ValueType.OnChange(FIconRuleControls.Rows[I].ValueType);
    end;
  end;
end;

procedure TfcxContinuousHighlightEditorFrame.CreateIconRuleControls(ACount: Integer);
var
  I, Y, OldCount: Integer;
  ValueType: TfcxContinuousHighlightValue;
begin
  for I := Length(FIconRuleControls.Rows) - 1 downto ACount do
    RemoveIconRuleControl(I);
  OldCount := Length(FIconRuleControls.Rows);
  SetLength(FIconRuleControls.Rows, ACount);
  if OldCount < ACount then
  begin
    Move(FIconRuleControls.Rows[0], FIconRuleControls.Rows[ACount - OldCount], OldCount * SizeOf(TfcxIconRuleControl));
    FillChar(FIconRuleControls.Rows[0], (ACount - OldCount) * SizeOf(TfcxIconRuleControl), 0);
  end;
  Y := cbIconSet.Top + cbIconSet.Height + 6;

  if not Assigned(FIconRuleControls.lblIcon) then
  begin
    FIconRuleControls.lblIcon := TLabel.Create(Self);
    FIconRuleControls.lblIcon.Left := 6;
    FIconRuleControls.lblIcon.Top := Y;
    FIconRuleControls.lblIcon.Caption := fcxResources.Get('sIcon') + ':';
    FIconRuleControls.lblIcon.Parent := Self;

    FIconRuleControls.lblValue := TLabel.Create(Self);
    FIconRuleControls.lblValue.Left := 6;
    FIconRuleControls.lblValue.Top := Y;
    FIconRuleControls.lblValue.Caption := fcxResources.Get('sValue') + ':';
    FIconRuleControls.lblValue.Parent := Self;

    FIconRuleControls.lblValueType := TLabel.Create(Self);
    FIconRuleControls.lblValueType.Left := 6;
    FIconRuleControls.lblValueType.Top := Y;
    FIconRuleControls.lblValueType.Caption := fcxResources.Get('sType') + ':';
    FIconRuleControls.lblValueType.Parent := Self;
  end;
  inc(Y, FIconRuleControls.lblIcon.Height + 3);

  for I := ACount - 1 downto 0 do
  begin
    // create icon combo
    if not Assigned(FIconRuleControls.Rows[I].Icon) then
    begin
      FIconRuleControls.Rows[I].Icon := TfcxImageCombo.Create(Self);
      FIconRuleControls.Rows[I].Icon.Width := 100;
      FIconRuleControls.Rows[I].Icon.Images := TfcxContinuousHighlight(Highlight).ImageList;
      FIconRuleControls.Rows[I].Icon.NoImageText := fcxResources.Get('sNoIcon');
      FIconRuleControls.Rows[I].Icon.Parent := Self;
    end;
    FIconRuleControls.Rows[I].Icon.Tag := I;
    FIconRuleControls.Rows[I].Icon.Top := Y;
    FIconRuleControls.Rows[I].Icon.Left := 6;
    FIconRuleControls.Rows[I].Icon.OnChange := RuleIconChange;
    // create info label
    if not Assigned(FIconRuleControls.Rows[I].Info) then
    begin
      FIconRuleControls.Rows[I].Info := TLabel.Create(Self);
      FIconRuleControls.Rows[I].Info.Parent := Self;
    end;
    FIconRuleControls.Rows[I].Info.Tag := I;
    FIconRuleControls.Rows[I].Info.Top := Y + (FIconRuleControls.Rows[I].Icon.Height - FIconRuleControls.Rows[I].Info.Height) div 2;
    FIconRuleControls.Rows[I].Info.Left := FIconRuleControls.Rows[I].Icon.Left + FIconRuleControls.Rows[I].Icon.Width + 6;
    FIconRuleControls.Rows[I].Info.Caption := 'if value';
    if I > 0 then
    begin
      // create rule combo
      if not Assigned(FIconRuleControls.Rows[I].Rule) then
      begin
        FIconRuleControls.Rows[I].Rule := TCombobox.Create(Self);
        FIconRuleControls.Rows[I].Rule.Width := 50;
        FIconRuleControls.Rows[I].Rule.Parent := Self;
        FIconRuleControls.Rows[I].Rule.Items.Add('>=');
        FIconRuleControls.Rows[I].Rule.Items.Add('>');
      end;
      FIconRuleControls.Rows[I].Rule.Tag := I;
      FIconRuleControls.Rows[I].Rule.Top := Y;
      FIconRuleControls.Rows[I].Rule.Left := FIconRuleControls.Rows[I].Icon.Left + FIconRuleControls.Rows[I].Icon.Width + 106;
      FIconRuleControls.Rows[I].Rule.Style := csDropDownList;
      FIconRuleControls.Rows[I].Rule.OnChange := RuleRuleChange;
      // create value edit box
      if not Assigned(FIconRuleControls.Rows[I].Value) then
      begin
        FIconRuleControls.Rows[I].Value := TEdit.Create(Self);
        FIconRuleControls.Rows[I].Value.Width := 100;
        FIconRuleControls.Rows[I].Value.Parent := Self;
      end;
      FIconRuleControls.Rows[I].Value.Tag := I;
      FIconRuleControls.Rows[I].Value.Top := Y;
      FIconRuleControls.Rows[I].Value.Left := FIconRuleControls.Rows[I].Rule.Left + FIconRuleControls.Rows[I].Rule.Width + 6;
      FIconRuleControls.Rows[I].Value.OnChange := RuleValueChange;
      // create value type combo
      if not Assigned(FIconRuleControls.Rows[I].ValueType) then
      begin
        FIconRuleControls.Rows[I].ValueType := TCombobox.Create(Self);
        FIconRuleControls.Rows[I].ValueType.Width := 145;
        FIconRuleControls.Rows[I].ValueType.Parent := Self;
        for ValueType := chvNumber to High(TfcxContinuousHighlightValue) do
          FIconRuleControls.Rows[I].ValueType.Items.Add(fcxResources.Get(ValueTypeToStr[ValueType]));
      end;
      FIconRuleControls.Rows[I].ValueType.Tag := I;
      FIconRuleControls.Rows[I].ValueType.Top := Y;
      FIconRuleControls.Rows[I].ValueType.Left := FIconRuleControls.Rows[I].Value.Left + FIconRuleControls.Rows[I].Value.Width + 6;
      FIconRuleControls.Rows[I].ValueType.Style := csDropDownList;
      FIconRuleControls.Rows[I].ValueType.OnChange := RuleValueTypeChange;
    end
    else
    begin
      FreeAndNil(FIconRuleControls.Rows[I].Rule);
      FreeAndNil(FIconRuleControls.Rows[I].Value);
      FreeAndNil(FIconRuleControls.Rows[I].ValueType);
    end;

    inc(Y, FIconRuleControls.Rows[I].Icon.Height + 6);
  end;
  FIconRuleControls.lblValue.Left := FIconRuleControls.Rows[1].Value.Left;
  FIconRuleControls.lblValueType.Left := FIconRuleControls.Rows[1].ValueType.Left;
end;

procedure TfcxContinuousHighlightEditorFrame.RemoveIconRuleControl(AIndex: Integer);
begin
  FIconRuleControls.Rows[AIndex].Icon.Free;
  FIconRuleControls.Rows[AIndex].Info.Free;
  FIconRuleControls.Rows[AIndex].Rule.Free;
  FIconRuleControls.Rows[AIndex].Value.Free;
  FIconRuleControls.Rows[AIndex].ValueType.Free;
end;

procedure TfcxContinuousHighlightEditorFrame.RemoveIconRuleControls;
var
  I: Integer;
begin
  for I := 0 to High(FIconRuleControls.Rows) do
    RemoveIconRuleControl(I);
  SetLength(FIconRuleControls.Rows, 0);
  FreeAndNil(FIconRuleControls.lblIcon);
  FreeAndNil(FIconRuleControls.lblValue);
  FreeAndNil(FIconRuleControls.lblValueType);
end;

procedure TfcxContinuousHighlightEditorFrame.RuleIconChange(Sender: TObject);
var
  Combo: TfcxImageCombo absolute Sender;
begin
  TfcxContinuousHighlight(Highlight).ImageIndex[Combo.Tag] := Combo.ImageIndex;
end;

procedure TfcxContinuousHighlightEditorFrame.RuleRuleChange(Sender: TObject);
var
  Combo: TCombobox absolute Sender;
begin
  TfcxContinuousHighlight(Highlight).ImageValueGE[Combo.Tag] := Combo.ItemIndex = 0;
end;

procedure TfcxContinuousHighlightEditorFrame.RuleValueChange(Sender: TObject);
var
  Edit: TEdit absolute Sender;
begin
  TfcxContinuousHighlight(Highlight).ImageValue[Edit.Tag] := StrToFloatDef(Edit.Text, 0);
end;

procedure TfcxContinuousHighlightEditorFrame.RuleValueTypeChange(Sender: TObject);
var
  Combo: TCombobox absolute Sender;
begin
  TfcxContinuousHighlight(Highlight).ImageValueType[Combo.Tag] := TfcxContinuousHighlightValue(Combo.ItemIndex + 2);
end;

procedure TfcxContinuousHighlightEditorFrame.btnReverseOrderClick(Sender: TObject);
var
  I, C: Integer;
begin
  with TfcxContinuousHighlight(Highlight) do
  begin
    BeginUpdate;
    C := ImageCount - 1;
    for I := 0 to C div 2 do
    begin
      if I = C - I then
        break;
      ImageIndex[I] := ImageIndex[I] xor ImageIndex[C - I];
      ImageIndex[C - I] := ImageIndex[I] xor ImageIndex[C - I];
      ImageIndex[I] := ImageIndex[I] xor ImageIndex[C - I];
    end;
    EndUpdate;
  end;
end;

initialization
  fcxRegisterHighlightEditor(TfcxContinuousHighlight, TfcxContinuousHighlightEditorFrame);

end.
