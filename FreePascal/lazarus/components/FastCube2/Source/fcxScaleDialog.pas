{*******************************************************}
{                                                       }
{             FastCube 2 scale dialog unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxScaleDialog;

{$I fcx.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Spin;

type
  TfcxScaleDialogForm = class(TForm)
    rgScale: TRadioGroup;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure rgScaleClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    spCustomScale: TSpinEdit;
    FBlockChange: Boolean;
    procedure Localize;
    function GetScale: Integer;
    procedure SetScale(const Value: Integer);
    function GetScaleFrom(S: String): Integer;
    procedure DoCustomScaleChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property Scale: Integer read GetScale write SetScale;
  end;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  fcxRes;

{ TfrxScaleDialogForm }

constructor TfcxScaleDialogForm.Create(AOwner: TComponent);
begin
  inherited;
  FBlockChange := False;
  spCustomScale := TSpinEdit.Create(Self);
  spCustomScale.MinValue := 10;
  spCustomScale.MaxValue := 400;
  spCustomScale.Width := 50;
  spCustomScale.Parent := rgScale;
  spCustomScale.Top := rgScale.ClientHeight - spCustomScale.Height - 6;
  spCustomScale.Left := rgScale.ClientWidth - spCustomScale.Width - 6;
  spCustomScale.BringToFront;
  spCustomScale.OnChange := DoCustomScaleChange;
  Localize;
end;

function TfcxScaleDialogForm.Execute: Boolean;
begin
  Result := ShowModal = mrOk;
end;

function TfcxScaleDialogForm.GetScaleFrom(S: String): Integer;
begin
  if Pos('%', S) = Length(S) then
  begin
    Delete(S, Length(S), 1);
    Result := StrToIntDef(S, 0);
  end
  else
    Result := 0;
end;

function TfcxScaleDialogForm.GetScale: Integer;
begin
  if rgScale.ItemIndex = rgScale.Items.Count - 1 then
    Result := spCustomScale.Value
  else
    Result := GetScaleFrom(rgScale.Items[rgScale.ItemIndex]);
end;

procedure TfcxScaleDialogForm.Localize;
var
  I: integer;
begin
  Caption := fcxResources.Get('sScale');
  rgScale.Caption := fcxResources.Get('sScale') + ':';
  for I := 0 to rgScale.Items.Count - 1 do
    rgScale.Items[I] := fcxResources.Get(rgScale.Items[I]);
  rgScale.Items[rgScale.Items.Count - 1] := rgScale.Items[rgScale.Items.Count - 1] + ':'; 
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
end;

procedure TfcxScaleDialogForm.SetScale(const Value: Integer);
var
  I: Integer;
begin
  FBlockChange := True;
  spCustomScale.Value := Value;
  FBlockChange := False;
  for I := 0 to rgScale.Items.Count - 2 do
    if GetScaleFrom(rgScale.Items[I]) = Value then
    begin
      rgScale.ItemIndex := I;
      Exit;
    end;
  rgScale.ItemIndex := rgScale.Items.Count - 1;
end;

procedure TfcxScaleDialogForm.rgScaleClick(Sender: TObject);
begin
  if rgScale.ItemIndex < rgScale.Items.Count - 1 then
  begin
    FBlockChange := True;
    spCustomScale.Value := GetScaleFrom(rgScale.Items[rgScale.ItemIndex]);
    FBlockChange := False;
  end;
end;

procedure TfcxScaleDialogForm.DoCustomScaleChange(Sender: TObject);
begin
  if FBlockChange then
    Exit;
  rgScale.ItemIndex := rgScale.Items.Count - 1;
end;

procedure TfcxScaleDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
