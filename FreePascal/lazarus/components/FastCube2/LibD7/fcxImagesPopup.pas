{*******************************************************}
{                                                       }
{             FastCube 2 Images Popup unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxImagesPopup;

interface

{$INCLUDE fcx.inc}

uses
  {$IFNDEF FPC}
  Windows,
  {$ELSE}
  LCLType, LCLIntf, LMessages,
  {$ENDIF}
{$IFDEF DELPHI_6UP}
  Types,
{$ENDIF}
  Messages, Classes, Graphics, ImgList, Controls,
  fcxPopupWindow, fcxTypes, fcxPainters, fcxGridPainters;

type
  TfcxImagesPopup = class(TfcxPopupWindow)
  private
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FColCount: Integer;
    FHSpacing: Integer;
    FVSpacing: Integer;
    FImageIndex: Integer;
    FNoImageText: String;
    FActiveIndex: Integer;
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender: TObject);
    procedure ReloadImages;
    procedure SetColCount(const Value: Integer);
    procedure SetHSpacing(const Value: Integer);
    procedure SetVSpacing(const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    procedure SetNoImageText(const Value: String);
    procedure SetActiveIndex(const Value: Integer);
    function GetIndexAt(X, Y: Integer): Integer;
    function GetIndexRect(AIndex: Integer): TRect;
    procedure InvalidateIndex(AIndex: Integer);
  protected
    function GetFrameSize: Integer; override;
    procedure NonClientPaint; override;
    procedure ClientPaint; override;
    procedure ClientMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ClientMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ClientMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ClientMouseLeave; override;

    procedure UpdateActiveIndex(X, Y: Integer);
    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ColCount: Integer read FColCount write SetColCount default 8;
    property HSpacing: Integer read FHSpacing write SetHSpacing default 4;
    property VSpacing: Integer read FVSpacing write SetVSpacing default 3;
    property NoImageText: String read FNoImageText write SetNoImageText;
  end;

  TfcxImageCombo = class(TCustomControl)
  private
    FImageIndex: Integer;
    FNoImageText: String;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FPainter: TfcxCustomGridPainter;
    FState: TfcxThemeState;
    FPopup: TfcxImagesPopup;
    FOnChange: TNotifyEvent;
    procedure SetImageIndex(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure DoImagesChange(Sender: TObject);
    procedure SetNoImageText(const Value: String);
    procedure DoPopupDestroy(Sender: TObject);
    procedure SetState(const Value: TfcxThemeState);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    property State: TfcxThemeState read FState write SetState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property NoImageText: String read FNoImageText write SetNoImageText;
    property Painter: TfcxCustomGridPainter read FPainter;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  
implementation

uses
  Math, Forms;

{ TfcxImagesPopup }

procedure TfcxImagesPopup.ClientMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateActiveIndex(X, Y);
  if Button in [mbLeft, mbRight] then
  begin
    if ActiveIndex <> -2 then
    begin
      ImageIndex := ActiveIndex;
      CloseUp(False);
    end;
  end;
end;

procedure TfcxImagesPopup.ClientMouseLeave;
begin
  ActiveIndex := -2;
end;

procedure TfcxImagesPopup.ClientMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  UpdateActiveIndex(X, Y);
end;

procedure TfcxImagesPopup.ClientMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  UpdateActiveIndex(X, Y);
end;

procedure TfcxImagesPopup.ClientPaint;
var
  CR, R: TRect;
  I: Integer;
  State: TfcxThemeState;
begin
  CR := ClientRect;
  if not Assigned(Images) then
    Exit;
  Canvas.Brush.Color := Styles.DataItem.FillColor;
  if ImageIndex = -1 then
    State := tsPressed
  else
  if ActiveIndex = -1 then
    State := tsHot
  else
    State := tsNormal;
  R := GetIndexRect(-1);
  Painter.DrawImageCell(Canvas, R, State);
  Painter.DrawText(Canvas, R, NoImageText, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  for I := 0 to Images.Count - 1 do
  begin
    if ImageIndex = I then
      State := tsPressed
    else
    if ActiveIndex = I then
      State := tsHot
    else
      State := tsNormal;
    R := GetIndexRect(I);
    Canvas.Brush.Color := Styles.DataItem.FillColor;
    Painter.DrawImageCell(Canvas, R, State);
    Images.Draw(Canvas, R.Left + HSpacing, R.Top + VSpacing, I, True);
  end;
  if (R.Right < CR.Right) then
  begin
    Canvas.Brush.Color := Styles.DataItem.FillColor;
    R.Left := R.Right;
    R.Right := CR.Right;
    Canvas.FillRect(R);
  end;
end;

constructor TfcxImagesPopup.Create(AOwner: TComponent);
begin
  inherited;
  FActiveIndex := -2;
  FImageIndex := -1;
  FColCount := 8;
  FVSpacing := 3;
  FHSpacing := 4;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := DoImagesChange;
end;

procedure TfcxImagesPopup.DoImagesChange(Sender: TObject);
begin
  ReloadImages;
end;

procedure TfcxImagesPopup.ReloadImages;
var
  FrameSize: Integer;
begin
  if Assigned(Images) then
  begin
    FrameSize := GetFrameSize;
    Width := (Images.Width + HSpacing * 2) * Min(ColCount, Images.Count) + FrameSize * 2;
    Height := (Images.Width + VSpacing * 2) * ((Images.Count div ColCount) + Ord(Images.Count mod ColCount <> 0) + 1) + FrameSize * 2;
  end;
end;

procedure TfcxImagesPopup.SetActiveIndex(const Value: Integer);
begin
  if FActiveIndex <> Value then
  begin
    InvalidateIndex(FActiveIndex);
    FActiveIndex := Value;
    InvalidateIndex(FActiveIndex);
  end;
end;

procedure TfcxImagesPopup.SetColCount(const Value: Integer);
begin
  if FColCount <> Value then
  begin
    FColCount := Value;
    ReloadImages;
  end;
end;

procedure TfcxImagesPopup.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    InvalidateIndex(FImageIndex);
    FImageIndex := Value;
    InvalidateIndex(FImageIndex);
  end;
end;

procedure TfcxImagesPopup.SetImages(const Value: TCustomImageList);
begin
  if Assigned(Images) then
    Images.UnRegisterChanges(FImagesChangeLink);
  FImages := Value;
  if Assigned(Images) then
  begin
    Images.RegisterChanges(FImagesChangeLink);
    Images.FreeNotification(Self);
  end;
  ReloadImages;
end;

procedure TfcxImagesPopup.SetNoImageText(const Value: String);
begin
  if FNoImageText <> Value then
  begin
    FNoImageText := Value;
    invalidate;
  end;
end;

procedure TfcxImagesPopup.SetHSpacing(const Value: Integer);
begin
  if FHSpacing <> Value then
  begin
    FHSpacing := Value;
    ReloadImages;
  end;
end;

procedure TfcxImagesPopup.SetVSpacing(const Value: Integer);
begin
  if FVSpacing <> Value then
  begin
    FVSpacing := Value;
    ReloadImages;
  end;
end;

function TfcxImagesPopup.GetIndexAt(X, Y: Integer): Integer;
var
  Row, Col: Integer;
begin
  Row := Y div (Images.Height + VSpacing * 2);
  Col := X div (Images.Width + HSpacing * 2);
  if Row = 0 then
    Result := -1
  else
    Result := (Row - 1) * ColCount + Col;
  if Result > Images.Count then
    Result := -2;
end;

procedure TfcxImagesPopup.UpdateActiveIndex(X, Y: Integer);
begin
  ActiveIndex := GetIndexAt(X, Y);
end;

function TfcxImagesPopup.GetIndexRect(AIndex: Integer): TRect;
var
  RowHeight, ColWidth: Integer;
begin
  Result := ClientRect;
  RowHeight := Images.Height + VSpacing * 2;
  ColWidth := Images.Width + HSpacing * 2;
  if AIndex = -1 then
    Result.Bottom := RowHeight
  else
  begin
    Result.Top := Result.Top + ((AIndex div ColCount) + 1) * RowHeight;
    Result.Left := Result.Left + (AIndex mod ColCount) * ColWidth;
    Result.Bottom := Result.Top + RowHeight;
    Result.Right := Result.Left + ColWidth;
  end;
end;

procedure TfcxImagesPopup.InvalidateIndex(AIndex: Integer);
begin
  if AIndex = -2 then
    Exit;
  InvalidateClientRect(GetIndexRect(AIndex));
end;

destructor TfcxImagesPopup.Destroy;
begin
  FImagesChangeLink.Free;
  inherited;
end;

function TfcxImagesPopup.GetFrameSize: Integer;
begin
  Result := inherited GetFrameSize + HSpacing;
end;

procedure TfcxImagesPopup.NonClientPaint;
var
  R: TRect;
  RgnOuter, RgnInner: HRGN;
  Frame: Integer;
begin
  inherited;
  Frame := inherited GetFrameSize;
  R := Rect(0, 0, Width, Height);
  inflateRect(R, -Frame, -Frame);
  RgnOuter := CreateRectRgnIndirect(R);
  inflateRect(R, -HSpacing, -HSpacing);
  RgnInner := CreateRectRgnIndirect(R);
  CombineRgn(RgnOuter, RgnOuter, RgnInner, RGN_DIFF);
  DeleteObject(RgnInner);
  SelectClipRgn(Canvas.Handle, RgnOuter);
  inflateRect(R, HSpacing, HSpacing);
  Painter.DrawBody(Canvas, R, Styles.DataItem);
  SelectClipRgn(Canvas.Handle, 0);
  DeleteObject(RgnOuter);
end;

{ TfcxImageCombo }

procedure TfcxImageCombo.CMMouseEnter(var Message: TMessage);
begin
  if Enabled then
    State := tsHot;
end;

procedure TfcxImageCombo.CMMouseLeave(var Message: TMessage);
begin
  if Enabled then
    State := tsNormal
  else
    State := tsDisabled;
end;

constructor TfcxImageCombo.Create(AOwner: TComponent);
begin
  inherited;
  Width := 75;
  Height := 25;
  FPainter := TfcxDefaultGridPainter.Create;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := DoImagesChange;
  FImageIndex := -1;
  FState := tsNormal;
end;

destructor TfcxImageCombo.Destroy;
begin
  FImagesChangeLink.Free;
  FPainter.Free;
  inherited;
end;

procedure TfcxImageCombo.DoImagesChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfcxImageCombo.DoPopupDestroy(Sender: TObject);
begin
  ImageIndex := FPopup.ImageIndex;
  FPopup := nil;
end;

procedure TfcxImageCombo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;
  State := tsPressed;
  if not Assigned(FPopup) then
  begin
    FPopup := TfcxImagesPopup.Create(Self);
    FPopup.Images := Images;
    FPopup.NoImageText := NoImageText;
    FPopup.ImageIndex := ImageIndex;
    P := ClientToScreen(Point(0, Height));
    FPopup.OnDestroy := DoPopupDestroy;
    FPopup.PopupAt(P);
  end
  else
    FPopup.CloseUp(True);
end;

procedure TfcxImageCombo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  State := tsHot;
end;

procedure TfcxImageCombo.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Painter.DrawButton(Canvas, R, FState);
  if ImageIndex = -1 then
    Painter.DrawText(Canvas, R, NoImageText, DT_CENTER or DT_VCENTER or DT_SINGLELINE)
  else
  if Assigned(Images) then
    with R do
      Images.Draw(Canvas, (Right + Left - Images.Width) div 2, (Bottom + Top - Images.Height) div 2, ImageIndex, FState <> tsDisabled);
end;

procedure TfcxImageCombo.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
    if Assigned(FOnChange) then
      OnChange(Self);
  end;
end;

procedure TfcxImageCombo.SetImages(const Value: TCustomImageList);
begin
  if Assigned(Images) then
    Images.UnRegisterChanges(FImagesChangeLink);
  FImages := Value;
  if Assigned(Images) then
  begin
    Images.RegisterChanges(FImagesChangeLink);
    Images.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TfcxImageCombo.SetNoImageText(const Value: String);
begin
  if FNoImageText <> Value then
  begin
    FNoImageText := Value;
    Invalidate;
  end;
end;

procedure TfcxImageCombo.SetState(const Value: TfcxThemeState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Invalidate;
  end;
end;

end.
