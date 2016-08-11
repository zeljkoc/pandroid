{*******************************************************}
{                                                       }
{             FastCube 2 highlights editor              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxHighlightRuleEditor;

{$INCLUDE fcx.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  fcxTypes, fcxSlice, fcxHighlights, CheckLst, ExtCtrls;

type
  TRuleRec = record
    RuleClass: TfcxCustomHighlightClass;
    Radio: TRadioButton;
    TabSheet: TTabSheet;
    Frame: TfcxCustomHighlightEditorFrame;
  end;

  TfcxHighlightRuleEditorDialog = class(TForm)
    Pages: TPageControl;
    OkBtn: TButton;
    CancelBtn: TButton;
    pApplyTo: TPanel;
    clbApplyTo: TCheckListBox;
    hdApplyTo: THeaderControl;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRules: array of TRuleRec;
    procedure Localize;
    procedure SelectRuleClick(Sender: TObject);
    procedure DoFrameResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute(ARule: TfcxCustomHighlight): TfcxCustomHighlight;
  end;

implementation

uses
  fcxRes,
  fcxRangeHighlightFrame,
  fcxContinuousHighlightFrame;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{ TfcxHighlightRuleEditorDialog }

constructor TfcxHighlightRuleEditorDialog.Create(AOwner: TComponent);

  procedure AddRuleClass(AValue: PfcxRegisteredHighlight);
  var
    Index: Integer;
  begin
    Index := Length(FRules);
    SetLength(FRules, Index + 1);
    FRules[Index].RuleClass := AValue^.ClassType;
    // 1. Add a radio button
    FRules[Index].Radio := TRadioButton.Create(Self);
    with FRules[Index].Radio do
    begin
      Top := Pages.Top - 1;
      Align := alTop;
      Caption := AValue^.ClassType.GetCaptionForEditor;
      Parent := Self;
      Pages.Top := Pages.Top + Height;
      Pages.Height := Pages.Height - Height;
      Tag := Index;
      OnClick := SelectRuleClick;
    end;
    // 2. Add a page
    FRules[Index].TabSheet := TTabSheet.Create(Self);
    with FRules[Index].TabSheet do
    begin
      {$ifndef fpc}
      TabVisible := False;
      {$endif}
      PageControl := Pages;
    end;
    // 3. add a frame
    FRules[Index].Frame := TfcxCustomHighlightEditorFrame(AValue^.FrameType.NewInstance);
    FRules[Index].Frame.Create(Self);
    with FRules[Index].Frame do
    begin
      Left := 0;
      Top := 0;
      Parent := FRules[Index].TabSheet;
    end;
    pApplyTo.Top := Pages.Top;
    pApplyTo.Height := Pages.Height;
  end;

var
  I: Integer;

begin
  inherited;
  Localize;

  for I := 0 to fcxRegisteredHighlights.Count - 1 do
    AddRuleClass(fcxRegisteredHighlights[I]);
end;

function TfcxHighlightRuleEditorDialog.Execute(ARule: TfcxCustomHighlight): TfcxCustomHighlight;
var
  i: integer;
  ApplyTo: TfcxHighlightApplyToEnum;
  AppliesTo: TfcxHighlightApplyTo;
begin
  // select the right radio button
  for i := Low(FRules) to High(FRules) do
    if FRules[i].RuleClass = ARule.ClassType then
    begin
      FRules[i].Radio.Checked := True;
      FRules[i].Frame.Highlight := TfcxGraphicHighlight(ARule);
    end;

  for ApplyTo := Low(TfcxHighlightApplyToEnum) to High(TfcxHighlightApplyToEnum) do
    clbApplyTo.Checked[Ord(ApplyTo)] := ApplyTo in ARule.ApplyTo;

  if ShowModal = mrOk then
  begin
    Result := FRules[Pages.ActivePageIndex].Frame.Highlight;
    AppliesTo := [];
    for ApplyTo := Low(TfcxHighlightApplyToEnum) to High(TfcxHighlightApplyToEnum) do
      if clbApplyTo.Checked[Ord(ApplyTo)] then
        include(AppliesTo, ApplyTo);
    Result.ApplyTo := AppliesTo;
  end
  else
    Result := ARule;
end;

procedure TfcxHighlightRuleEditorDialog.Localize;
const
  ApplyToStr: array[TfcxHighlightApplyToEnum] of String = (
 { hatCells      } 'sToCells',
 { hatTotals     } 'sToTotals',
 { hatGrandTotal } 'sToGrandTotal'
  );
var
  ApplyTo: TfcxHighlightApplyToEnum;
begin
  Caption := fcxResources.Get('sHighlightRuleEditor');
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
  for ApplyTo := Low(TfcxHighlightApplyToEnum) to High(TfcxHighlightApplyToEnum) do
    clbApplyTo.Items.Add(fcxResources.Get(ApplyToStr[ApplyTo]));
  hdApplyTo.Sections[0].Text := fcxResources.Get('sApplyTo');
end;

procedure TfcxHighlightRuleEditorDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfcxHighlightRuleEditorDialog.SelectRuleClick(Sender: TObject);
begin
  if Sender is TRadioButton then
  begin
    Pages.ActivePageIndex := TRadioButton(Sender).Tag;
    FRules[Pages.ActivePageIndex].Frame.OnResize := DoFrameResize;
    FRules[Pages.ActivePageIndex].Frame.Prepare;
    DoFrameResize(nil);
  end;
end;

procedure TfcxHighlightRuleEditorDialog.DoFrameResize(Sender: TObject);
begin
  Width := Width - Pages.ActivePage.ClientWidth + FRules[Pages.ActivePageIndex].Frame.Width;
  Height := Height - Pages.ActivePage.ClientHeight + FRules[Pages.ActivePageIndex].Frame.Height;
end;

end.
