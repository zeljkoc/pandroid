{*******************************************************}
{                                                       }
{           FastCube 2 Styles editor unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
unit fcxStylesEditor;

interface

{$I fcx.inc}

uses
  Classes, Controls, Forms, StdCtrls,
  fcxStyleFrame, fcxStyles;

type
  TfcxStylesEditorDialog = class(TForm)
    lbStyles: TListBox;
    StyleFrame: TfcxStyleEditorFrame;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure lbStylesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FStyles: TfcxCustomThemeStyles;
    procedure Localize;
    procedure UpdateStyles;
    procedure UpdateStyle;
    procedure SetStyles(const Value: TfcxCustomThemeStyles);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;
    property Styles: TfcxCustomThemeStyles read FStyles write SetStyles;
  end;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}  
{$ENDIF}

uses
  fcxRes;

{ TfcxStylesEditorDialog }

constructor TfcxStylesEditorDialog.Create(AOwner: TComponent);
begin
  inherited;
  Localize;
  FStyles := nil;
end;

function TfcxStylesEditorDialog.Execute: Boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfcxStylesEditorDialog.Localize;
begin
  StyleFrame.Localize;
  Caption := fcxResources.Get('sStylesEditor');
  // buttons
  OkBtn.Caption := fcxResources.Get('sOkBtn');
  CancelBtn.Caption := fcxResources.Get('sCancelBtn');
end;

procedure TfcxStylesEditorDialog.SetStyles(const Value: TfcxCustomThemeStyles);
begin
  FStyles.Free;
  if Assigned(Value) then
  begin
    FStyles := TfcxCustomThemeStyles(Value.ClassType.NewInstance);
    {$warnings off}
    FStyles.Create(nil);
    {$warnings on}
    FStyles.Assign(Value);
  end;
  UpdateStyles;
end;

procedure TfcxStylesEditorDialog.UpdateStyles;
var
  i: integer;
begin
  lbStyles.Clear;
  if not Assigned(Styles) then
    Exit;

  for i := Styles.FirstStyleIndex to Styles.LastStyleIndex do
    lbStyles.Items.AddObject(Styles.StyleName[i], Styles[i]);
  if lbStyles.Items.Count > 0 then
    lbStyles.ItemIndex := 0
  else
    lbStyles.ItemIndex := -1;
  UpdateStyle;
end;

procedure TfcxStylesEditorDialog.lbStylesClick(Sender: TObject);
begin
  UpdateStyle;
end;

procedure TfcxStylesEditorDialog.UpdateStyle;
begin
  StyleFrame.Enabled := lbStyles.ItemIndex <> -1;
  if StyleFrame.Enabled then
    StyleFrame.Style := TfcxCustomThemeStyle(lbStyles.Items.Objects[lbStyles.ItemIndex]);
end;

procedure TfcxStylesEditorDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

destructor TfcxStylesEditorDialog.Destroy;
begin
  FStyles.Free;
  inherited;
end;

end.
