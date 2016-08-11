{*******************************************************}
{                                                       }
{        FastCube 2 styles definition unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxStyles;

interface

{$INCLUDE fcx.inc}
uses
{$IFDEF Delphi_18UP}
  System.UITypes,
{$ENDIF}
  Controls, Graphics, fcxGraphicUtils,
  Types, Classes, SysUtils, fcxXml, fcxTypes;

type
  { TfcxCustomThemeStyle }

  TfcxCustomThemeStyles = class;
  TfcxCustomThemeStyle = class(TPersistent)
  private
    FOwner: TfcxCustomThemeStyles;
    FGradientDirection: TfcxThemeGradientDirection;
    FOnChange: TNotifyEvent;
    FIndex: Integer;
    FFont: TFont;
    FFillColor: TColor;
    FGradientColor: TColor;
    FTextColor: TColor;
  protected
    procedure SetFillColor(const Value: TColor); virtual;
    procedure SetTextColor(const Value: TColor);
    procedure SetGradientColor(const Value: TColor); virtual;
    procedure SetFont(const Value: TFont); virtual;
    procedure SetGradientDirection(const Value: TfcxThemeGradientDirection);
    procedure Change; virtual;
    function SameStyle(AStyle: TfcxCustomThemeStyle): boolean; virtual;
    function SameFont(AFont: TFont): boolean;
  public
    constructor Create(AOwner: TfcxCustomThemeStyles); virtual;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure MergeWith(AStyle: TfcxCustomThemeStyle); virtual;
    procedure Update(AFillColor, AGradientColor, AFontColor: TColor; AGradientDirection: TfcxThemeGradientDirection = tgdNone; ResetFont: Boolean = True);
    procedure DrawBackground(ACanvas: TCanvas; ARect: TRect);
    procedure SaveToXML(AItem: TfcxXMLItem); virtual;
    procedure LoadFromXML(AItem: TfcxXMLItem); virtual;
    property Index: Integer read FIndex write FIndex;
  published
    property TextColor: TColor read FTextColor write SetTextColor default clNone;
    property FillColor: TColor read FFillColor write SetFillColor default clNone; // in case of gradient the start color
    property GradientColor: TColor read FGradientColor write SetGradientColor default clNone; // in case of gradient the end color
    property Font: TFont read FFont write SetFont;
    property GradientDirection: TfcxThemeGradientDirection read FGradientDirection write SetGradientDirection default tgdNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  TfcxCustomThemeStyleClass = class of TfcxCustomThemeStyle;

  TfcxCustomThemeStyles = class(TPersistent)
  private
    FOwner: TComponent;
    FParentFont: Boolean;
    procedure SetParentFont(const Value: Boolean);
  protected
    function GetStyle(Index: Integer): TfcxCustomThemeStyle; virtual; abstract;
    function GetStyleName(Index: Integer): String; virtual; 
    procedure InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); virtual;
    procedure SetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); virtual; abstract;
    procedure ChangeStyle(AStyle: TfcxCustomThemeStyle); virtual;
    procedure SetDefaultValues; virtual; 
    function GetFirstStyleIndex: Integer; virtual;
    function GetLastStyleIndex: Integer; virtual; 
    class function GetStyleClass: TfcxCustomThemeStyleClass; virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateFonts;
    property FirstStyleIndex: Integer read GetFirstStyleIndex;
    property LastStyleIndex: Integer read GetLastStyleIndex;
    property Style[Index : Integer]: TfcxCustomThemeStyle read GetStyle write SetStyle; default;
    property StyleName[Index : Integer]: String read GetStyleName;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
  end;

implementation

type
  THackWinControl = class(TWinControl);

{ TfcxCustomThemeStyle }

procedure TfcxCustomThemeStyle.AssignTo(Dest: TPersistent);
var
  DestStyle: TfcxCustomThemeStyle absolute Dest;
begin
  if Dest is TfcxCustomThemeStyle then
  begin
    DestStyle.FFillColor := FFillColor;
    DestStyle.FTextColor := FTextColor;
    DestStyle.FGradientColor := FGradientColor;
    DestStyle.FGradientDirection := FGradientDirection;
    DestStyle.FFont.Assign(FFont);
    DestStyle.Change;
  end
  else
    inherited;
end;

procedure TfcxCustomThemeStyle.DrawBackground(ACanvas: TCanvas; ARect: TRect);
var
  OldColor: TColor;  
begin
  if GradientDirection = tgdNone then
  begin
    OldColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := FillColor;
    ACanvas.FillRect(ARect);
    ACanvas.Brush.Color := OldColor;
  end
  else
    FillGradient(ACanvas, ARect, ColorToRGB(FillColor), ColorToRGB(GradientColor), Ord(Pred(GradientDirection)));
end;

function TfcxCustomThemeStyle.SameFont(AFont: TFont): boolean;
begin
  Result :=
    (AFont.Name = Font.Name) and
    (AFont.Size = Font.Size) and
    (AFont.Style = Font.Style) and
    (AFont.Height = Font.Height) and
    (AFont.Pitch = Font.Pitch) and
    (AFont.Charset = Font.Charset);
end;

function TfcxCustomThemeStyle.SameStyle(AStyle: TfcxCustomThemeStyle): boolean;
begin
  Result :=
    (AStyle.FillColor = FillColor) and
    (AStyle.TextColor = TextColor) and
    (AStyle.GradientColor = GradientColor) and
    (AStyle.GradientDirection = GradientDirection) and
    SameFont(AStyle.Font);
end;

constructor TfcxCustomThemeStyle.Create(AOwner: TfcxCustomThemeStyles);
begin
  inherited Create;
  FIndex := -1;
  FOwner := AOwner;
  FFont := TFont.Create;
  FTextColor := clNone;
  FFillColor := clNone;
  FGradientColor := clNone;
  FGradientDirection := tgdNone;
end;

destructor TfcxCustomThemeStyle.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TfcxCustomThemeStyle.SetFillColor(const Value: TColor);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Change;
  end;
end;

procedure TfcxCustomThemeStyle.SetFont(const Value: TFont);
begin
  if SameFont(Value) then Exit;
  FFont.Assign(Value);
  if Assigned(FOwner) then
    FOwner.FParentFont := False;
  Change;
end;

procedure TfcxCustomThemeStyle.Change;
begin
  if FOwner <> nil then
    FOwner.ChangeStyle(Self);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TfcxCustomThemeStyle.Update(AFillColor, AGradientColor, AFontColor: TColor;
  AGradientDirection: TfcxThemeGradientDirection = tgdNone; ResetFont: Boolean = True);
begin
  FFillColor := AFillColor;
  FTextColor := AFontColor;
  FGradientColor := AGradientColor;
  FGradientDirection := AGradientDirection;
  if ResetFont and Assigned(FOwner.FOwner) then
    FFont.Assign(THackWinControl(FOwner.FOwner).Font);
end;

procedure TfcxCustomThemeStyle.SetGradientColor(const Value: TColor);
begin
  if FGradientColor <> Value then
  begin
    FGradientColor := Value;
    Change;
  end;
end;

procedure TfcxCustomThemeStyle.SetGradientDirection(
  const Value: TfcxThemeGradientDirection);
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Change;
  end;
end;

procedure TfcxCustomThemeStyle.MergeWith(AStyle: TfcxCustomThemeStyle);
var
  HasChange: Boolean;
begin
  HasChange := False;
  if AStyle.FillColor <> clNone then
  begin
    FFillColor := AStyle.FillColor;
    HasChange := True;
  end;
  if AStyle.TextColor <> clNone then
  begin
    FTextColor := AStyle.TextColor;
    HasChange := True;
  end;
  if AStyle.GradientColor <> clNone then
  begin
    FGradientColor := AStyle.GradientColor;
    HasChange := True;
  end;
  if AStyle.GradientDirection <> tgdNone then
  begin
    FGradientDirection := AStyle.GradientDirection;
    HasChange := True;
  end;
  if AStyle.Font.Style <> [] then
  begin
    Font.Style := Font.Style + AStyle.Font.Style;
    HasChange := True;
  end;

  if HasChange then
    Change;
end;

procedure TfcxCustomThemeStyle.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Change;
  end;
end;

procedure TfcxCustomThemeStyle.LoadFromXML(AItem: TfcxXMLItem);

  procedure LoadFontFromXML(AItem: TfcxXMLItem; AFont: TFont);
  begin
    if AItem.Name <> 'FONT' then
      Exit;
    AFont.Charset := AItem.IntProp['Charset'];
    AFont.Color := AItem.IntProp['Color'];
    AFont.Height := AItem.IntProp['Height'];
    AFont.Name := AItem.Prop['Name'];
    AFont.Pitch := TFontPitch(AItem.IntProp['Pitch']);
    AFont.Size := AItem.IntProp['Size'];
    {$IFDEF LCL}
    AFont.Style := TFontStyles(LongInt(AItem.IntProp['Style']));
    {$ELSE}
    AFont.Style := TFontStyles(Byte(AItem.IntProp['Style']));
    {$ENDIF}
  end;

begin
  if AItem.Name <> 'STYLE' then
    Exit;
  TextColor := AItem.IntProp['TextColor'];
  FillColor := AItem.IntProp['FillColor'];
  GradientColor := AItem.IntProp['GradientColor'];
  GradientDirection := TfcxThemeGradientDirection(AItem.IntProp['GradientDirection']);
  LoadFontFromXML(AItem.FindItem('FONT'), Font)
end;

procedure TfcxCustomThemeStyle.SaveToXML(AItem: TfcxXMLItem);

  procedure SaveFontToXML(AItem: TfcxXMLItem; AFont: TFont);
  begin
    AItem.Name := 'FONT';
    AItem.IntProp['Charset'] := AFont.Charset;
    AItem.IntProp['Color'] := AFont.Color;
    AItem.IntProp['Height'] := AFont.Height;
    AItem.Prop['Name'] := AFont.Name;
    AItem.IntProp['Pitch'] := Ord(AFont.Pitch);
    AItem.IntProp['Size'] := AFont.Size;
    {$IFDEF LCL}
    AItem.IntProp['Style'] := LongInt(AFont.Style);
    {$ELSE}
    AItem.IntProp['Style'] := Byte(AFont.Style);
    {$ENDIF}
  end;

begin
  AItem.Name := 'STYLE';
  AItem.IntProp['TextColor'] := TextColor;
  AItem.IntProp['FillColor'] := FillColor;
  AItem.IntProp['GradientColor'] := GradientColor;
  AItem.IntProp['GradientDirection'] := Ord(GradientDirection);
  SaveFontToXML(AItem.Add, Font)
end;

{ TfcxCustomThemeStyles }

procedure TfcxCustomThemeStyles.AssignTo(Dest: TPersistent);
var
  i: integer;
  DestStyles: TfcxCustomThemeStyles absolute Dest;
begin
  if Dest is TfcxCustomThemeStyles then
  begin
    for i := GetFirstStyleIndex to GetLastStyleIndex do
      DestStyles[i].Assign(Style[i]);
  end
  else
    inherited;
end;

procedure TfcxCustomThemeStyles.ChangeStyle(AStyle: TfcxCustomThemeStyle);
begin
  if Assigned(FOwner) and not (csLoading in FOwner.ComponentState) and TWinControl(FOwner).HandleAllocated then
    TWinControl(FOwner).Invalidate;
end;

constructor TfcxCustomThemeStyles.Create(AOwner: TComponent);
var
  I: integer;
begin
  inherited Create;
  FOwner := AOwner;
  FParentFont := True;
  for I := GetFirstStyleIndex to GetLastStyleIndex do
    InternalSetStyle(I, GetStyleClass.Create(Self));
  SetDefaultValues;
end;

destructor TfcxCustomThemeStyles.Destroy;
var
  i: integer;
begin
  for i := GetFirstStyleIndex to GetLastStyleIndex do
    Style[i].Free;
  inherited;
end;

function TfcxCustomThemeStyles.GetFirstStyleIndex: Integer;
begin
  Result := 0;
end;

function TfcxCustomThemeStyles.GetLastStyleIndex: Integer;
begin
  Result := -1;
end;

class function TfcxCustomThemeStyles.GetStyleClass: TfcxCustomThemeStyleClass;
begin
  Result := TfcxCustomThemeStyle;
end;

function TfcxCustomThemeStyles.GetStyleName(Index: Integer): String;
begin
  Result := IntToStr(Index);
end;

procedure TfcxCustomThemeStyles.InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle);
begin
  Value.FIndex := Index;
end;

procedure TfcxCustomThemeStyles.SetDefaultValues;
begin

end;

procedure TfcxCustomThemeStyles.SetParentFont(const Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    UpdateFonts;
  end;
end;

procedure TfcxCustomThemeStyles.UpdateFonts;
var
  i: Integer;
begin
  if ParentFont then
  begin
    for i := GetFirstStyleIndex to GetLastStyleIndex do
      Style[i].Font := THackWinControl(FOwner).Font;
    FParentFont := True;
  end;
end;

end.
