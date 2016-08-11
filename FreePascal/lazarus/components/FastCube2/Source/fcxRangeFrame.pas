{*******************************************************}
{                                                       }
{            FastCube 2 Range editor unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
unit fcxRangeFrame;

interface

{$include fcx.inc}

uses
{$IFDEF FPC}
  Types, LCLType, LCLIntf, EditBtn,
{$ELSE}
  Windows, ComCtrls,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, fcxRange;
  
const
  UM_OPERATOR_DELETE = $8000 + 1; // WM_APP
type
  {$ifndef FPC}
  TDateEdit = TDateTimePicker;
  {$endif}

  TfcxRangeEditorFrame = class(TFrame)
    cbCompareObject: TComboBox;
    cbCondition: TComboBox;
    edRangeLow: TEdit;
    lblAnd: TLabel;
    edRangeHigh: TEdit;
    procedure cbCompareObjectChange(Sender: TObject);
    procedure cbConditionChange(Sender: TObject);
    procedure edRangeLowChange(Sender: TObject);
    procedure edRangeHighChange(Sender: TObject);
    procedure edDateRangeLowChange(Sender: TObject);
    procedure edDateRangeHighChange(Sender: TObject);
  private
    FRange: TfcxRange;
    edDateRangeLow, edDateRangeHigh: TDateEdit;
    FLocalized: Boolean;
    procedure SetRange(const Value: TfcxRange);
    procedure DoRangeChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Localize;
    class function Edit(ARange: TfcxRange): Boolean;
    property Range: TfcxRange read FRange write SetRange;
  end;

  TfcxRangeOperatorEditor = class
  private
    FRadioAnd: TRadioButton;
    FRadioOr: TRadioButton;
    FDelete: TButton;
    FPlaceHolder: TPanel;
    FIndex: Integer;
    FOnChange: TNotifyEvent;
    FBlockChange: Boolean;
    FOnDelete: TNotifyEvent;
    function GetValue: TfcxRangeBinaryOperator;
    procedure SetValue(const Value: TfcxRangeBinaryOperator);
    function GetHeight: Integer;
    procedure DoChange(Sender: TObject);
    procedure DoDeleteClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure PlaceTo(const Value: TWinControl; const Left, Top: Integer);
    property Value: TfcxRangeBinaryOperator read GetValue write SetValue;
    property Index: Integer read FIndex write FIndex;
    property Height: Integer read GetHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
  end;

  TfcxRangesEditor = class(TForm)
  private
    FRanges: TfcxRanges;
    FFrames: array of TfcxRangeEditorFrame;
    FOperators: array of TfcxRangeOperatorEditor;
    FButtons: array of TButton;
    procedure DoRangesChange(Sender: TObject);
    procedure DoOperatorChange(Sender: TObject);
    procedure DoOperatorDelete(Sender: TObject);
    procedure SetRanges(const Value: TfcxRanges);
    procedure AddButtonClick(Sender: TObject);
    procedure UMOperatorDelete(var Message: TMessage); message UM_OPERATOR_DELETE;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    class function Edit(ARange: TfcxRanges): Boolean;
    property Ranges: TfcxRanges read FRanges write SetRanges;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Math,
  fcxRes;


{ TTfcxRangeFrame }

constructor TfcxRangeEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FLocalized := False;
  edDateRangeLow := TDateEdit.Create(Self);
  edDateRangeLow.Parent := Self;
  edDateRangeLow.Left := edRangeLow.Left;
  edDateRangeLow.Top := edRangeLow.Top;
  {$ifndef fpc}
  edDateRangeLow.Width := edRangeLow.Width;
  edDateRangeLow.ShowCheckbox := True;
  {$else}
  edDateRangeLow.Width := edRangeLow.Width - 21;
  {$endif}
  edDateRangeLow.Visible := False;
  edDateRangeLow.OnChange := edDateRangeLowChange;

  edDateRangeHigh := TDateEdit.Create(Self);
  edDateRangeHigh.Parent := Self;
  edDateRangeHigh.Left := edRangeHigh.Left;
  edDateRangeHigh.Top := edRangeHigh.Top;
  {$ifndef fpc}
  edDateRangeHigh.Width := edRangeHigh.Width;
  edDateRangeHigh.ShowCheckbox := True;
  {$else}
  edDateRangeHigh.Width := edRangeHigh.Width - 21;
  {$endif}
  edDateRangeHigh.Visible := False;
  edDateRangeHigh.OnChange := edDateRangeHighChange;
end;

procedure TfcxRangeEditorFrame.Localize;
const
  CompareObjectToStr: array[TfcxRangeCompareObject] of String = (
 { rcoValue   } 'sCellValue',
 { rcoText    } 'sCellText',
 { rcoDate    } 'sCellDate',
 { rcoNull    } 'sCellNull',
 { rcoNotNull } 'sCellNotNull'
  );
var
  rco: TfcxRangeCompareObject;
begin
  if FLocalized then
    Exit;
  FLocalized := True;  
  lblAnd.Caption := fcxResources.Get('sAnd');
  for rco := Low(TfcxRangeCompareObject) to High(TfcxRangeCompareObject) do
    cbCompareObject.Items.Add(fcxResources.Get(CompareObjectToStr[rco]));
end;

procedure TfcxRangeEditorFrame.SetRange(const Value: TfcxRange);
begin
  FRange := Value;
  DoRangeChange(nil);
end;

procedure TfcxRangeEditorFrame.cbCompareObjectChange(Sender: TObject);

const
  ValueConditionToStr: array[TfcxRangeValueCondition] of String = (
  { rvcBetween        } 'sBetween',
  { rvcBeyond         } 'sBeyond',
  { rvcEqual          } 'sEqual',
  { rvcNotEqual       } 'sNotEqual',
  { rvcGreater        } 'sGreater',
  { rvcLess           } 'sLess',
  { rvcGreaterOrEqual } 'sGreaterOrEqual',
  { rvcLessOrEqual    } 'sLessOrEqual'
  );

  TextConditionToStr: array[TfcxRangeTextCondition] of String = (
 { rtcContains    } 'sContains',
 { rtcNotContains } 'sNotContains',
 { rtcStartsWith  } 'sStartsWith',
 { rtcEndsWith    } 'sEndsWith'
  );

var
  rvo: TfcxRangeValueCondition;
  rto: TfcxRangeTextCondition;
begin
  Range.CompareObject := TfcxRangeCompareObject(cbCompareObject.ItemIndex);
  cbCondition.Items.Clear;
  case Range.CompareObject of
    rcoValue, rcoDate:
      begin
        for rvo := Low(TfcxRangeValueCondition) to High(TfcxRangeValueCondition) do
          cbCondition.Items.Add(fcxResources.Get(ValueConditionToStr[rvo]));
        cbCondition.ItemIndex := Ord(Range.ValueCondition);
      end;
    rcoText:
      begin
        for rto := Low(TfcxRangeTextCondition) to High(TfcxRangeTextCondition) do
          cbCondition.Items.Add(fcxResources.Get(TextConditionToStr[rto]));
        cbCondition.ItemIndex := Ord(Range.TextCondition);
      end;
  end;
  cbCondition.Visible := Range.CompareObject in [rcoValue, rcoText, rcoDate];
  cbConditionChange(nil);
end;

procedure TfcxRangeEditorFrame.cbConditionChange(Sender: TObject);
begin
  if cbCondition.Visible then
  begin
    case Range.CompareObject of
      rcoValue:
        begin
          Range.ValueCondition := TfcxRangeValueCondition(cbCondition.ItemIndex);
          edDateRangeLow.Visible := False;
          edDateRangeHigh.Visible := False;
          edRangeLow.Visible := True;
          if (edRangeLow.Text = '') then
          begin
            if Range.LowRange <> NegInfinity then
              edRangeLow.Text := FloatToStr(Range.LowRange)
          end;
          edRangeHigh.Visible := Range.ValueCondition in [rvcBetween, rvcBeyond];
          if (edRangeHigh.Text = '') then
          begin
            if Range.HighRange <> Infinity then
              edRangeHigh.Text := FloatToStr(Range.HighRange)
          end;
        end;
      rcoText:
        begin
          Range.TextCondition := TfcxRangeTextCondition(cbCondition.ItemIndex);
          edDateRangeLow.Visible := False;
          edDateRangeHigh.Visible := False;
          edRangeLow.Visible := True;
          edRangeHigh.Visible := False;
          edRangeLow.Text := Range.Text;
        end;
      rcoDate:
        begin
          Range.ValueCondition := TfcxRangeValueCondition(cbCondition.ItemIndex);
          edRangeLow.Visible := False;
          edRangeHigh.Visible := False;
          edDateRangeLow.Visible := True;
          if Range.LowRange <> NegInfinity then
          begin
            edDateRangeLow.Date := Range.LowRange;
            {$ifndef fpc}
            edDateRangeLow.Checked := True;
            {$endif}
          end
          else
          begin
            {$ifndef fpc}
            edDateRangeLow.Date := Now;
            edDateRangeLow.Checked := False;
            {$else}
            edDateRangeLow.Text := '';
            {$endif}
          end;
          edDateRangeHigh.Visible := Range.ValueCondition in [rvcBetween, rvcBeyond];
          if Range.HighRange <> Infinity then
          begin
            edDateRangeHigh.Date := Range.HighRange;
            {$ifndef fpc}
            edDateRangeHigh.Checked := True;
            {$endif}
          end
          else
          begin
            {$ifndef fpc}
            edDateRangeHigh.Date := Now;
            edDateRangeHigh.Checked := False;
            {$else}
            edDateRangeHigh.Text := '';
            {$endif}
          end;
        end;
    end;
  end
  else
  begin
    edRangeLow.Visible := False;
    edRangeHigh.Visible := False;
    edDateRangeLow.Visible := False;
    edDateRangeHigh.Visible := False;
  end;
  lblAnd.Visible := edRangeHigh.Visible or edDateRangeHigh.Visible;
end;

procedure TfcxRangeEditorFrame.edRangeLowChange(Sender: TObject);
begin
  Range.BeginUpdate;
  Range.LowRange := StrToFloatDef(edRangeLow.Text, NegInfinity);
  Range.Text := edRangeLow.Text;
  Range.EndUpdate;
end;

procedure TfcxRangeEditorFrame.edRangeHighChange(Sender: TObject);
begin
  Range.HighRange := StrToFloatDef(edRangeHigh.Text, Infinity);
end;

procedure TfcxRangeEditorFrame.DoRangeChange;
begin
  if Assigned(Range) then
    cbCompareObject.ItemIndex := Ord(Range.CompareObject)
  else
    cbCompareObject.ItemIndex := -1;
  cbCompareObjectChange(nil);
end;

class function TfcxRangeEditorFrame.Edit(ARange: TfcxRange): Boolean;
var
  F: TForm;
  Frame: TfcxRangeEditorFrame;
begin
  F := TForm.Create(Application);
  F.Caption := fcxResources.Get('sRangeEditor');
  F.Position := poScreenCenter;
  F.BorderStyle := bsDialog;
  F.BorderWidth := 6;

  Frame := TfcxRangeEditorFrame.Create(F);
  Frame.Range := ARange;
  Frame.Parent := F;

  F.ClientWidth := Frame.Width;

  with TButton.Create(F) do
  begin
    Caption := fcxResources.Get('sCancelBtn');
    Cancel := True;
    ModalResult := mrCancel;
    Left := F.ClientWidth - Width;
    Top := Frame.Top + Frame.Height + 6;
    F.ClientHeight := Top + Height;
    Parent := F;
  end;

  with TButton.Create(F) do
  begin
    Caption := fcxResources.Get('sOkBtn');
    Default := True;
    ModalResult := mrOk;
    Left := F.ClientWidth - Width * 2 - 6;
    Top := Frame.Top + Frame.Height + 6;
    Parent := F;
  end;

  Result := F.ShowModal = mrOk
end;

procedure TfcxRangeEditorFrame.edDateRangeLowChange(Sender: TObject);
begin
  Range.BeginUpdate;
  if {$ifndef fpc}edDateRangeLow.Checked{$else}edDateRangeLow.Text <> ''{$endif} then
    Range.LowRange := edDateRangeLow.Date
  else
    Range.LowRange := NegInfinity;
  Range.Text := DateToStr(edDateRangeLow.Date);
  Range.EndUpdate;
end;

procedure TfcxRangeEditorFrame.edDateRangeHighChange(Sender: TObject);
begin
  if {$ifndef fpc}edDateRangeHigh.Checked{$else}edDateRangeHigh.Text <> ''{$endif} then
    Range.HighRange := edDateRangeHigh.Date
  else
    Range.HighRange := Infinity;
end;

{ TfcxRangesEditor }

procedure TfcxRangesEditor.AddButtonClick(Sender: TObject);
begin
  Ranges.Add;
end;

class function TfcxRangesEditor.Edit(ARange: TfcxRanges): Boolean;
var
  F: TfcxRangesEditor;
begin
  F := TfcxRangesEditor.CreateNew(Application);
  F.Ranges := ARange;
  Result := F.ShowModal = mrOk;
  F.Free;
end;

procedure TfcxRangesEditor.SetRanges(const Value: TfcxRanges);
begin
  FRanges := Value;
  FRanges.OnChange := DoRangesChange;
  DoRangesChange(FRanges);
end;

procedure TfcxRangesEditor.DoRangesChange(Sender: TObject);
var
  I: Integer;
begin
  if Ranges.Count > Length(FFrames) then
  begin
    for I := Length(FFrames) to Ranges.Count - 1 do
    begin
      if I > 0 then
      begin
        SetLength(FOperators, I);
        FOperators[pred(I)] := TfcxRangeOperatorEditor.Create(Self);
        if I > 1 then
          FOperators[pred(I)].PlaceTo(Self, 30, (FFrames[0].Height * I) + FOperators[0].Height * pred(I))
        else
          FOperators[pred(I)].PlaceTo(Self, 30, FFrames[0].Height * I);
        FOperators[pred(I)].OnChange := DoOperatorChange;
        FOperators[pred(I)].OnDelete := DoOperatorDelete;
      end;
      SetLength(FFrames, I + 1);
      FFrames[I] := TfcxRangeEditorFrame.Create(Self);
      FFrames[I].Name := '';
      if I > 0 then
        FFrames[I].Top := (FFrames[0].Height + FOperators[0].Height) * I
      else
        FFrames[I].Top := 0;
      FFrames[I].Left := 0;
      FFrames[I].Parent := Self;
      FFrames[I].Localize;
    end;
  end
  else
  if Ranges.Count < Length(FFrames) then
  begin
    for I := Length(FFrames) - 1 downto Ranges.Count do
    begin
      if I > 0 then
        FOperators[pred(I)].Free;
      FFrames[I].Free;
    end;
    SetLength(FFrames, Ranges.Count);
    SetLength(FOperators, Ranges.Count - 1);
  end;
  if Length(FFrames) > 0 then
  begin
    ClientWidth := FFrames[0].Width;
    if Length(FOperators) > 0 then
      ClientHeight := FFrames[0].Height * Length(FFrames) + FOperators[0].Height * Length(FOperators) + FButtons[0].Height + 6
    else
      ClientHeight := FFrames[0].Height * Length(FFrames) + FButtons[0].Height + 6;
    for I := 0 to Length(FFrames) - 1 do
      FFrames[I].Range := Ranges[I];

    for I := 0 to Length(FOperators) - 1 do
    begin
      FOperators[I].Index := I;
      FOperators[I].Value := Ranges.Operator[I];
    end;
  end;
end;

constructor TfcxRangesEditor.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  Caption := fcxResources.Get('sRangeEditor');
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  Height := 100;
  Width := 200;

  SetLength(FButtons, 3);
  FButtons[0] := TButton.Create(Self);
  with FButtons[0] do
  begin
    Caption := fcxResources.Get('sAdd');
    Left := 6;
    Top := Self.ClientHeight - Height - 6;
    Anchors := [akLeft, akBottom];
    Parent := Self;
    OnClick := AddButtonClick;
  end;

  FButtons[1] := TButton.Create(Self);
  with FButtons[1] do
  begin
    Caption := fcxResources.Get('sCancelBtn');
    Cancel := True;
    ModalResult := mrCancel;
    Left := Self.ClientWidth - Width - 6;
    Top := Self.ClientHeight - Height - 6;
    Anchors := [akRight, akBottom];
    Parent := Self;
  end;

  FButtons[2] := TButton.Create(Self);
  with FButtons[2] do
  begin
    Caption := fcxResources.Get('sOkBtn');
    Default := True;
    ModalResult := mrOk;
    Left := Self.ClientWidth - Width * 2 - 12;
    Top := Self.ClientHeight - Height - 6;
    Anchors := [akRight, akBottom];
    Parent := Self;
  end;
end;

procedure TfcxRangesEditor.DoOperatorChange(Sender: TObject);
begin
  FRanges.Operator[TfcxRangeOperatorEditor(Sender).Index] := TfcxRangeOperatorEditor(Sender).Value;
end;

procedure TfcxRangesEditor.DoOperatorDelete(Sender: TObject);
begin
  PostMessage(Handle, UM_OPERATOR_DELETE, TfcxRangeOperatorEditor(Sender).Index + 1, 0);
end;

procedure TfcxRangesEditor.UMOperatorDelete(var Message: TMessage);
begin
  FRanges.Delete(Message.WParam);
end;

destructor TfcxRangesEditor.Destroy;
var
  I: Integer;
begin
  // destroy operators explicitly
  for I := Low(FOperators) to High(FOperators) do
    FOperators[I].Free;
  SetLength(FOperators, 0);
  inherited;
end;

{ TfcxRangeOperatorEditor }

constructor TfcxRangeOperatorEditor.Create(AOwner: TComponent);
begin
  FBlockChange := False;
  // create a dummy parent to correct work of checked
  FPlaceHolder := TPanel.Create(AOwner);
  FPlaceHolder.BevelInner := bvNone;
  FPlaceHolder.BevelOuter := bvNone;
  FPlaceHolder.Caption := '';
  FRadioAnd := TRadioButton.Create(AOwner);
  FRadioAnd.Caption := fcxResources.Get('sAnd');
  FRadioOr := TRadioButton.Create(AOwner);
  FRadioOr.Caption := fcxResources.Get('sOr');
  FDelete := TButton.Create(AOwner);
  FDelete.Caption := fcxResources.Get('sDelete');
end;

destructor TfcxRangeOperatorEditor.Destroy;
begin
  FRadioAnd.Free;
  FRadioOr.Free;
  FDelete.Free;
  FPlaceHolder.Free;
  inherited;
end;

procedure TfcxRangeOperatorEditor.DoChange(Sender: TObject);
begin
  if Assigned(OnChange) and not FBlockChange then
    OnChange(Self);
end;

procedure TfcxRangeOperatorEditor.DoDeleteClick(Sender: TObject);
begin
  if Assigned(OnDelete) then
    OnDelete(Self);
end;

function TfcxRangeOperatorEditor.GetHeight: Integer;
begin
  Result := FRadioAnd.Height;
end;

function TfcxRangeOperatorEditor.GetValue: TfcxRangeBinaryOperator;
begin
  if FRadioAnd.Checked then
    Result := rboAnd
  else
    Result := rboOr;
end;

procedure TfcxRangeOperatorEditor.PlaceTo(const Value: TWinControl; const Left, Top: Integer);
begin
  FPlaceHolder.Left := Left;
  FPlaceHolder.Top := Top;
  FPlaceHolder.Parent := Value;
  FRadioAnd.Left := 0;
  FRadioAnd.Top := 0;
  FRadioAnd.Parent := FPlaceHolder;
  FRadioAnd.OnClick := DoChange;
  FRadioOr.Left := FRadioAnd.Width + 3;
  FRadioOr.Top := 0;
  FRadioOr.Parent := FPlaceHolder;
  FRadioOr.OnClick := DoChange;

  FDelete.Top := 0;
  FDelete.Left := FRadioOr.Left + FRadioOr.Width + 3;
  FDelete.Height := Height;
  FDelete.Parent := FPlaceHolder;
  FDelete.OnClick := DoDeleteClick;
  FPlaceHolder.Height := Height;
  FPlaceHolder.Width := FDelete.Left + FDelete.Width;
end;

procedure TfcxRangeOperatorEditor.SetValue(const Value: TfcxRangeBinaryOperator);
begin
  FBlockChange := True;
  case Value of
    rboAnd: FRadioAnd.Checked := True;
    rboOr: FRadioOr.Checked := True;
  end;
  FBlockChange := False;
end;

end.
