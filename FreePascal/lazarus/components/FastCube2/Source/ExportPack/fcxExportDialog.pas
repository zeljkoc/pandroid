{*******************************************************}
{                                                       }
{        FastCube 2 export dialog build unit            }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxExportDialog;
interface

{$INCLUDE fcx.inc}

uses
  Classes,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  fcxTypes;

type
  TfcxExportDialog = class(TForm)
  private
    FPages: TPageControl;
    FButtonPanel: TPanel;
    FOkB: TButton;
    FCancelB: TButton;
    FSaveDialog: TSaveDialog;
    function GetTopForPage(APage: TWinControl): Integer;
    function GetRequiredHeight: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function AddPage(const ACaption: String): TComponent;
    function AddCheckBox(APage: TComponent; const ACaption: String): TCheckBox;
    function AddComboBox(APage: TComponent; const AList: array of String): TComboBox;
    function AddEdit(APage: TComponent): TEdit;
    function AddMemo(APage: TComponent): TMemo;
    function AddLabel(APage: TComponent; const ACaption: String): TLabel;
    function AddLabelFor(AComponent: TComponent; const ACaption: String; AtLeft: Boolean = True): TComponent;
    property SaveDialog: TSaveDialog read FSaveDialog;
    procedure AfterConstruction; override;
  end;

implementation

uses
  Math,
  fcxRes;

{ TfcxExportDialog }

function TfcxExportDialog.AddCheckBox(APage: TComponent; const ACaption: String): TCheckBox;
begin
  Result := TCheckBox.Create(Self);
  Result.Caption := ACaption;
  Result.Top := GetTopForPage(TWinControl(APage));
  Result.Width := TWinControl(APage).ClientWidth;
  Result.Parent := TWinControl(APage);
end;

function TfcxExportDialog.AddComboBox(APage: TComponent; const AList: array of String): TComboBox;
var
  I: Integer;
begin
  Result := TComboBox.Create(Self);
  Result.Style := csDropDownList;
  Result.Top := GetTopForPage(TWinControl(APage));
  Result.Width := TWinControl(APage).ClientWidth;
  Result.Parent := TWinControl(APage);
  for I := Low(AList) to High(AList) do
    Result.Items.Add(AList[I]);
end;

function TfcxExportDialog.AddEdit(APage: TComponent): TEdit;
begin
  Result := TEdit.Create(Self);
  Result.Top := GetTopForPage(TWinControl(APage));
  Result.Width := TWinControl(APage).ClientWidth;
  Result.Parent := TWinControl(APage);
end;

function TfcxExportDialog.AddMemo(APage: TComponent): TMemo;
begin
  Result := TMemo.Create(Self);
  Result.Top := GetTopForPage(TWinControl(APage));
  Result.Width := TWinControl(APage).ClientWidth;
  Result.Parent := TWinControl(APage);
end;

function TfcxExportDialog.AddLabel(APage: TComponent; const ACaption: String): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.AutoSize := True;
  Result.WordWrap := True;
  Result.Caption := ACaption;
  Result.Top := GetTopForPage(TWinControl(APage));
  Result.Width := TWinControl(APage).ClientWidth;
  Result.Parent := TWinControl(APage);
end;

function TfcxExportDialog.AddLabelFor(AComponent: TComponent; const ACaption: String; AtLeft: Boolean): TComponent;
begin
  Result := TLabel.Create(Self);
  TLabel(Result).Caption := ACaption;
  TLabel(Result).Top := TWinControl(AComponent).Top;
  if AtLeft then
  begin
    TWinControl(AComponent).Left := 120;
    TWinControl(AComponent).Width := TWinControl(AComponent).Parent.ClientWidth - 120;
  end
  else
  begin
    TLabel(Result).Width := TWinControl(AComponent).Parent.ClientWidth;
    TWinControl(AComponent).Top := TLabel(Result).Top + TLabel(Result).Height + 3;
  end;
  TLabel(Result).Parent := TWinControl(AComponent).Parent;
end;

function TfcxExportDialog.AddPage(const ACaption: String): TComponent;
begin
  Result := TTabSheet.Create(Self);
  TTabSheet(Result).BorderWidth := 7;
  TTabSheet(Result).Caption := ACaption;
  TTabSheet(Result).PageControl := FPages;
end;

procedure TfcxExportDialog.AfterConstruction;
begin
  inherited;
  ClientHeight := GetRequiredHeight;
  if UseRightToLeftAlignment then
    FlipChildren(True);
end;

constructor TfcxExportDialog.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  BorderStyle := bsDialog;
  BorderWidth := 7;
  Position := poScreenCenter;
  FPages := TPageControl.Create(Self);
  FPages.Align := alClient;
  FPages.Parent := Self;
  FButtonPanel := TPanel.Create(Self);
  FButtonPanel.Align := alBottom;
  FButtonPanel.Parent := Self;
  FButtonPanel.Height := 32;
  FButtonPanel.BevelInner := bvNone;
  FButtonPanel.BevelOuter := bvNone;
  FCancelB := TButton.Create(Self);
  FCancelB.Caption := fcxResources.Get('sCancelBtn');
  FCancelB.Cancel := True;
  FCancelB.ModalResult := mrCancel;
  FCancelB.Top := 7;
  FCancelB.Left := FButtonPanel.ClientWidth - FCancelB.Width;
  FCancelB.Parent := FButtonPanel;
  FOkB := TButton.Create(Self);
  FOkB.Caption := fcxResources.Get('sOkBtn');
  FOkB.Default := True;
  FOkB.ModalResult := mrOk;
  FOkB.Top := 7;
  FOkB.Left := FCancelB.Left - FOkB.Width - 7;
  FOkB.Parent := FButtonPanel;

  FSaveDialog := TSaveDialog.Create(Self);
end;

function TfcxExportDialog.GetRequiredHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FPages.PageCount - 1 do
    Result := Max(Result, GetTopForPage(FPages.Pages[I]));
  Result := Result + FPages.Height + FButtonPanel.Height;
  if FPages.PageCount > 0 then
    Result := Result - FPages.Pages[0].ClientHeight;
end;

function TfcxExportDialog.GetTopForPage(APage: TWinControl): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to APage.ControlCount - 1 do
    if APage.Controls[I].Top + APage.Controls[I].Height > Result then
      Result := APage.Controls[I].Top + APage.Controls[I].Height;
  if Result > 0 then
    Result := Result + 7;
end;

end.
