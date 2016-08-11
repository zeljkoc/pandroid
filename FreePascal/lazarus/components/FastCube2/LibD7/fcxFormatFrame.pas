
{******************************************}
{                                          }
{             FastReport v4.0              }
{           DisplayFormat editor           }
{                                          }
{         Copyright (c) 1998-2010          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxFormatFrame;

interface

{$I fcx.inc}

uses
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, fcxFormats
{$IFDEF Delphi_6UP}
, Variants
{$ENDIF}
;


type
  TfcxFormatEditorFrame = class(TFrame)
    CategoryL: TGroupBox;
    CategoryLB: TListBox;
    FormatL: TGroupBox;
    FormatLB: TListBox;
    GroupBox1: TGroupBox;
    FormatStrL: TLabel;
    SeparatorL: TLabel;
    FormatE: TEdit;
    SeparatorE: TEdit;
    procedure CategoryLBClick(Sender: TObject);
    procedure FormatLBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormatLBClick(Sender: TObject);
  private
    { Private declarations }
    FFormat: TfcxFormat;
    FAllowNoFormat: Boolean;
    function GetFormat: TfcxFormat;
    procedure SetFormat(const Value: TfcxFormat);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Localize;
    property Format: TfcxFormat read GetFormat write SetFormat;
  end;


implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}  
{$ENDIF}

uses
  fcxRes,
  fcxCustomFormat;

const
  CategoryNames: array[0..5] of String =
    ('fkText', 'fkNumber', 'fkDateTime', 'fkBoolean', 'fkCustom', 'fkNone');

constructor TfcxFormatEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFormat := TfcxFormat.Create;
  FAllowNoFormat := False;
  Localize;
end;

destructor TfcxFormatEditorFrame.Destroy;
begin
  FFormat.Free;
  inherited;
end;

procedure TfcxFormatEditorFrame.CategoryLBClick(Sender: TObject);
var
  i, n: Integer;
  s: String;
  procedure FindFormat;
  var
    i: Integer;
    s: String;
  begin
    if CategoryLB.ItemIndex = 4 then
    begin
      FormatLB.ItemIndex := fcCustomFormats.FindFormat(FormatE.Text)
    end
    else
      for i := 0 to FormatLB.Items.Count - 1 do
      begin
        s := FormatLB.Items[i];
        if Copy(s, Pos(';', s) + 1, 255) = FormatE.Text then
          FormatLB.ItemIndex := i;
      end;
  end;
begin
  FormatLB.Items.Clear;
  n := CategoryLB.ItemIndex;
  SeparatorE.Enabled := n in [1, 4];
  SeparatorL.Enabled := n in [1, 4];
  FormatE.ReadOnly := False;
  if (n = 0) or (n = 5) or (n = -1) then
    Exit;
  if n <> 4 then
    for i := 1 to 10 do
    begin
      s := fcxResources.Get(CategoryNames[n] + IntToStr(i));
      if Pos('fk', s) = 0 then
        FormatLB.Items.Add(s);
    end
  else
  if n = 4 then
  begin
    for i := 0 to fcCustomFormats.Count - 1 do
    begin
      s := fcCustomFormats[i].Caption;
      FormatLB.Items.Add(s);
    end;
    FormatE.ReadOnly := True;
  end;
  FindFormat;
end;

procedure TfcxFormatEditorFrame.FormatLBClick(Sender: TObject);
var
  s: String;
begin
  if FormatLB.ItemIndex <> -1 then
  begin
    if CategoryLB.ItemIndex = 4 then
    begin
      FormatE.Text := fcCustomFormats.Format[FormatLB.ItemIndex].Name;
      SeparatorE.Text := fcCustomFormats.Format[FormatLB.ItemIndex].DecSeparator;
    end
    else
    begin
      s := FormatLB.Items[FormatLB.ItemIndex];
      FormatE.Text := Copy(s, Pos(';', s) + 1, 255);
    end
  end;
end;

procedure TfcxFormatEditorFrame.FormatLBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  s: String;
begin
  with FormatLB do
  begin
    Canvas.FillRect(ARect);
    s := Items[Index];
    if Pos(';', s) <> 0 then
      s := Copy(s, 1, Pos(';', s) - 1);
    Canvas.TextOut(ARect.Left + 2, ARect.Top + 1, s);
  end;
end;

procedure TfcxFormatEditorFrame.Localize;
begin
  CategoryL.Caption := fcxGet(4501);
  FormatL.Caption := fcxGet(4502);
  FormatStrL.Caption := fcxGet(4503);
  SeparatorL.Caption := fcxGet(4504);
end;

function TfcxFormatEditorFrame.GetFormat: TfcxFormat;
var
  s: String;
begin
  Result := FFormat;
  if (CategoryLB.ItemIndex = 4) and (FormatLB.ItemIndex < 0) then
    exit;
  FFormat.TypeFormat.Kind := TfcxFormatKind(CategoryLB.ItemIndex);
  FFormat.TypeFormat.FormatStr := FormatE.Text;
  s := SeparatorE.Text;
  if (s = '') and (FFormat.TypeFormat.Kind <> fkCustom) then
    s := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  if (s <> '') then
    FFormat.TypeFormat.DecSeparator := s[1]
  else
    FFormat.TypeFormat.DecSeparator := '';
end;

procedure TfcxFormatEditorFrame.SetFormat(const Value: TfcxFormat);
  procedure FillCategory;
  var
    i: Integer;
  begin
    for i := 0 to 4 do
      CategoryLB.Items.Add(fcxResources.Get(CategoryNames[i]));
  end;
begin
  FFormat.Assign(Value);
  
  FillCategory;
  CategoryLB.ItemIndex := Integer(FFormat.TypeFormat.Kind);
  FormatE.Text := FFormat.TypeFormat.FormatStr;
  SeparatorE.Text := FFormat.TypeFormat.DecSeparator;
  CategoryLBClick(Self);
end;

end.
