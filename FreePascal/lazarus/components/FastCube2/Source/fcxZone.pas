{*******************************************************}
{                                                       }
{            FastCube 2 zone controls unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
unit fcxZone;

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
  Messages, Classes, Graphics, Forms, Menus, Controls,
  fcxTypes, fcxControl, fcxAlerts;

// TODO:
// 1. Expand/Collapse handling
// 2. Mouse over events
// 3. Similar to splitter resizing

type
  TZone = class;
  TZoneContainer = class;

  TZoneOption = (
    zoShowHeader,  // zone has visible header
    zoCollapsable, // can be collapsed/expanded
    zoScrollable   // items can be scrolled
  );
  TZoneOptions = set of TZoneOption;

  TZonePart = (
    zpNone,          // undefined
    zpLeftFrame,     // frame around
    zpTopFrame,
    zpRightFrame,
    zpBottomFrame,
    zpHeader,        // header
    zpExpandSign,    // expand sign on header
    zpLeftScroller,  // left scroller
    zpRightScroller, // right scroller
    zpBody           // body
  );

  TZoneFrame = zpLeftFrame..zpBottomFrame;

  TZoneScroller = (
    zsLeft,
    zsRight
  );
  TZoneScrollers = set of TZoneScroller;

  TSizingType = (
    stHorzSizing,
    stVertSizing
  );

  TSizingInfo = record
    ActiveZone: TZone;
    ActivePart: TZonePart;
    SizingType: TSizingType;
    StartPoint: TPoint;
    CurPoint: TPoint;
    DrawStart: TPoint;
    DrawStop: TPoint;
    ResizeSibling: Boolean;
    Data: Pointer;
  end;

  { TZone }

  TZone = class(TPersistent)
  private
    FOptions: TZoneOptions;
    FOwner: TZoneContainer;
    FBoundingRect: TRect;
    FSavedBounds: TRect;
    FExpanded: Boolean;
    FCaption: String;
    FActivePart: TZonePart;
    FVisible: Boolean;
    FPopupMenu: TPopupMenu;
    procedure SetOptions(const Value: TZoneOptions);
    function GetBoundingRect: TRect;
    function GetCanvas: TCanvas;
    function GetClientBounds: TRect;
    function GetClientRect: TRect;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetExpanded(const Value: Boolean);
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetCaption(const Value: String);
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    procedure SetVisible(const Value: Boolean);
  protected
    // own menu items
    FOwnMenuItems: array of TMenuItem;
    function GetOwnMenuItemIndex(MenuItem: TObject): Integer;
  
    procedure SetActivePart(const Value: TZonePart); virtual;
    procedure SetBoundingRect(const Value: TRect); virtual;

    // frame rect and parts
    function GetBorderSize: TSize; virtual;
    function GetHeaderSize: TSize; virtual;
    function GetFrameRect: TRect; virtual;
    function GetHeaderRect: TRect;
    function GetExpandSignRect(ARect: TRect): TRect; virtual;
    function HitTest(APoint: TPoint): TZonePart; virtual;
    procedure UpdateCursor; virtual;
    function IsResizeableFrame(AFrame: TZoneFrame): Boolean; virtual;
    procedure HintInfo(P: TPoint; out HintStr: String; out HintRect: TRect); virtual;

    function ScreenToClient(const P: TPoint): TPoint;
    function ScreenToZone(P: TPoint): TPoint;
    function ClientToScreen(const P: TPoint): TPoint;
    function ZoneToScreen(P: TPoint): TPoint;
    function PointToClientPoint(P: TPoint): TPoint;
    function ClientPointToPoint(P: TPoint): TPoint;

    function GetClientMousePos: TPoint;

    function GetScrollerRect(AScroller: TZoneScroller): TRect;
    function GetScrollerWidth(AScroller: TZoneScroller): Integer; virtual;
    function GetRequiredScrollers: TZoneScrollers; virtual;
    procedure Scroll(AScroller: TZoneScroller); virtual;

    // non client parts
    procedure DrawFrame; virtual;
    procedure DrawHeader(ARect: TRect); virtual;
    procedure DrawScrollers; virtual;
    procedure DrawScroller(ARect: TRect; AScroller: TZoneScroller; AEnabled: Boolean); virtual;

    // 2 paint methods
    procedure NonClientPaint; virtual;
    procedure ClientPaint; virtual;

    // mouse handlers
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      X, Y: Integer): Boolean; virtual;
    procedure DblClick(X, Y: Integer); virtual;

    function GetPopupMenu: TPopupMenu;
    procedure PopupMenuNeeded;
    function CreatePopupMenu: TPopupMenu; virtual;
    procedure ContextPopup(X, Y: Integer; var Handled: Boolean); virtual;

    procedure SetCapture;
    procedure ReleaseCapture;
    function HasCapture: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;

    // sizing
    procedure StartSizing(var AInfo: TSizingInfo); virtual;
    procedure MoveSizing(var AInfo: TSizingInfo); virtual;
    procedure StopSizing(var AInfo: TSizingInfo); virtual;
    procedure UpdateSizingDrawInfo(var AInfo: TSizingInfo); virtual;

    // key handlers
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    property ActivePart: TZonePart read FActivePart write SetActivePart;
    property Canvas: TCanvas read GetCanvas;
    property Owner: TZoneContainer read FOwner;
    property PopupMenu: TPopupMenu read GetPopupMenu;
  public
    constructor Create(AOwner: TZoneContainer); virtual;
    destructor Destroy; override;

    procedure Invalidate(Erase: Boolean = True; ARect: PRect = nil; Client: Boolean = True); virtual;
    procedure Paint; virtual;
    procedure Update; virtual; // whole update + redrawing

    property BoundingRect: TRect read GetBoundingRect write SetBoundingRect;
    property Caption: String read FCaption write SetCaption;
    property ClientBounds: TRect read GetClientBounds;
    property ClientRect: TRect read GetClientRect;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property FrameRect: TRect read GetFrameRect; // frame sizes
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property Options: TZoneOptions read FOptions write SetOptions;
    property Top: Integer read GetTop write SetTop;
    property Visible: Boolean read FVisible write SetVisible;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TZoneClass = class of TZone;

  { TZoneList }

  TZoneList = class(TList)
  private
    FOwner: TZoneContainer;
    FZoneClass: TZoneClass;
    function GetZone(AIndex: Integer): TZone;
    procedure SetZone(AIndex: Integer; const Value: TZone);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
  public
    constructor Create(AOwner: TZoneContainer; AZoneClass: TZoneClass);
    function AddZone(AZoneClass: TZoneClass = nil): TZone;
    function GetBoundingRgn: HRGN;
    function GetZoneAt(X, Y: Integer): TZone;
    procedure Paint;
    procedure Update;

    property Items[AIndex: Integer]: TZone read GetZone write SetZone; default;
  end;

  TZoneContextPopupEvent = procedure(Sender: TZoneContainer; AZone: TZone; X, Y: Integer; var Handled: Boolean) of object;
  TZoneCreatePopupMenuEvent = procedure(Sender: TZoneContainer; AZone: TZone; PopupMenu: TPopupMenu) of object;

  { TZoneContainer }

  TZoneContainer = class(TfcxCustomControl)
  private
    FZones: TZoneList;
    FKeyBoardZone: TZone;
    FCaptureZone: TZone;
    FLastMouseZone: TZone;
    FSizing: TSizingInfo;
    FOnZoneContextPopup: TZoneContextPopupEvent;
    FOnZoneCreatePopupMenu: TZoneCreatePopupMenuEvent;
    procedure UpdateLastMouseZone(X, Y: Integer);
  protected
    class function GetZoneClass: TZoneClass; virtual;

    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Msg: TCMHINTSHOW); message CM_HINTSHOW;

    procedure PaintEmptySpace; virtual;
    procedure DrawSizingLine; virtual;
    procedure InvalidateSizingLine;

    procedure SetLastMouseZone(const Value: TZone);
    // mouse events
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

    procedure DoZoneContextPopup(AZone: TZone; X, Y: Integer; var Handled: Boolean); virtual;
    procedure DoCreatePopupMenu(AZone: TZone; PopupMenu: TPopupMenu); virtual;

    procedure SetCapture(AZone: TZone);
    procedure ReleaseCapture;
    function GetCapture: TZone;
    procedure InitiateDrag;

    // key events
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure NotifyUpdate(Sender: TZone); virtual;
    property KeyBoardZone: TZone read FKeyBoardZone write FKeyBoardZone;

    property OnZoneContextPopup: TZoneContextPopupEvent read FOnZoneContextPopup write FOnZoneContextPopup;
    property OnZoneCreatePopupMenu: TZoneCreatePopupMenuEvent read FOnZoneCreatePopupMenu write FOnZoneCreatePopupMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure StartSizing(AZone: TZone; const ASizingType: TSizingType; const APoint: TPoint);
    procedure StopSizing;

    property Zones: TZoneList read FZones;
    property LastMouseZone: TZone read FLastMouseZone write SetLastMouseZone;
    property Sizing: TSizingInfo read FSizing;
  end;

const
  ScrollerToPart: array[TZoneScroller] of TZonePart = (
 { zsLeft  } zpLeftScroller,
 { zsRight } zpRightScroller
  );
  
implementation

uses
  TypInfo, SysUtils;

{ TZoneList }

function TZoneList.AddZone(AZoneClass: TZoneClass = nil): TZone;
begin
  if Assigned(AZoneClass) then
    Result := AZoneClass.Create(FOwner)
  else
    Result := FZoneClass.Create(FOwner);
  inherited Add(Result);
end;

constructor TZoneList.Create(AOwner: TZoneContainer; AZoneClass: TZoneClass);
begin
  inherited Create;
  FOwner := AOwner;
  FZoneClass := AZoneClass;
end;

function TZoneList.GetBoundingRgn: HRGN;
var
  i: integer;
  R: HRGN;
begin
  Result := CreateRectRgnIndirect(Rect(0, 0, 0, 0));
  for i := 0 to Count - 1 do
  begin
    R := CreateRectRgnIndirect(Items[i].BoundingRect);
    CombineRgn(Result, Result, R, RGN_OR);
    DeleteObject(R);
  end;
end;

function TZoneList.GetZone(AIndex: Integer): TZone;
begin
  Result := inherited Get(AIndex);
end;

function TZoneList.GetZoneAt(X, Y: Integer): TZone;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if not Result.Visible then
      Continue;
    if PtInRect(Result.BoundingRect, Point(X, Y)) then
      Exit;
  end;
  Result := nil;
end;

procedure TZoneList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TZone(Ptr).Free;
end;

procedure TZoneList.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Items[i].Notification(AComponent, Operation);
end;

procedure TZoneList.Paint;
var
  i: integer;
  OldBrushColor: TColor;
begin
  OldBrushColor := FOwner.Canvas.Brush.Color;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Visible then
    begin
      Items[i].Paint;
      FOwner.Canvas.Brush.Color := OldBrushColor;
    end;
  end;
end;

procedure TZoneList.SetZone(AIndex: Integer; const Value: TZone);
begin
  inherited Put(AIndex, Value);
end;

procedure TZoneList.Update;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Update;
end;

{ TZone }

procedure TZone.ClientPaint;
begin
  Canvas.Brush.Color := clMoneyGreen;
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Color := clWindow;
end;

constructor TZone.Create(AOwner: TZoneContainer);
begin
  inherited Create;
  FOwner := AOwner;
  FOptions := [];
  FVisible := True;
  FBoundingRect := Rect(0, 0, 0, 0);
  FSavedBounds := FBoundingRect;
  FExpanded := True;
  FCaption := '';
end;

procedure TZone.DrawFrame;
begin
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  with FrameRect do
    Canvas.Rectangle(Rect(Left, Top, Self.Width - Right, Self.Height - Bottom));
end;

procedure TZone.DrawHeader(ARect: TRect);

 procedure DrawTreeButton(ARect: TRect; Expanded: Boolean);
 begin
   Canvas.Brush.Color := clWhite;
   Canvas.FillRect(ARect);
   Canvas.Rectangle(ARect);
   Canvas.MoveTo(ARect.Left + 2, (ARect.Top + ARect.Bottom) div 2);
   Canvas.LineTo(ARect.Right - 2, (ARect.Top + ARect.Bottom) div 2);
   if not Expanded then
   begin
     Canvas.MoveTo((ARect.Left + ARect.Right) div 2, ARect.Top + 2);
     Canvas.LineTo((ARect.Left + ARect.Right) div 2, ARect.Bottom - 2);
   end;
 end;

var
  ASize: TSize;
begin
  Canvas.Brush.Color := clGray;
  Canvas.Font.Color := clWhite;
  ASize := Canvas.TextExtent(Caption);
  Canvas.TextRect(ARect,
    (ARect.Left + ARect.Right - ASize.cx) div 2,
    (ARect.Top + ARect.Bottom - ASize.cy) div 2,
    Caption);
  if zoCollapsable in Options then
  begin
    ARect := GetExpandSignRect(ARect);
    DrawTreeButton(ARect, Expanded);
  end;
end;

procedure TZone.DrawScrollers;
var
  Scroller: TZoneScroller;
  Scrollers: TZoneScrollers;
begin
  Scrollers := GetRequiredScrollers;
  if Scrollers = [] then
    Exit;
    
  for Scroller := Low(TZoneScroller) to High(TZoneScroller) do
    DrawScroller(GetScrollerRect(Scroller), Scroller, Scroller in Scrollers);
end;

function TZone.GetBorderSize: TSize;
begin
  Result.cx := 3;
  Result.cy := 3;
end;

function TZone.GetBoundingRect: TRect;
begin
  Result := FBoundingRect;
end;

function TZone.GetCanvas: TCanvas;
begin
  Result := FOwner.Canvas;
end;

procedure TZone.SetCapture;
begin
  FOwner.SetCapture(Self);
end;

procedure TZone.ReleaseCapture;
begin
  FOwner.ReleaseCapture;
end;

function TZone.GetClientBounds: TRect;
begin
  Result := FBoundingRect;
  // - frame
  with FrameRect do
  begin
    if Result.Right - Result.Left > 1 then
    begin
      inc(Result.Left, Left);
      dec(Result.Right, Right);
    end;  
    if Result.Bottom - Result.Top > 1 then
    begin
      inc(Result.Top, Top);
      dec(Result.Bottom, Bottom);
    end;  
  end;
end;

function TZone.GetClientHeight: Integer;
begin
  with GetClientBounds do
    Result := Bottom - Top;
end;

function TZone.GetClientRect: TRect;
begin
  Result := GetClientBounds;
  OffsetRect(Result, -Result.Left, -Result.Top);
end;

function TZone.GetClientWidth: Integer;
begin
  with GetClientBounds do
    Result := Right - Left;
end;

function TZone.GetExpandSignRect(ARect: TRect): TRect;
begin
  Result.Left := ARect.Left + 2;
  Result.Top := ARect.Top + 2;
  Result.Bottom := ARect.Bottom - 2;
  Result.Right := Result.Left + Result.Bottom - Result.Top;
end;

function TZone.GetFrameRect: TRect;
var
  BorderSize: TSize;
begin
  BorderSize := GetBorderSize;
  Result := Rect(BorderSize.cx, BorderSize.cy, BorderSize.cx, BorderSize.cy);
  if zoShowHeader in Options then
  begin
    BorderSize := GetHeaderSize;
    inc(Result.Top, BorderSize.cy);
  end;
  if (zoScrollable in Options) and (GetRequiredScrollers <> []) then
  begin
    inc(Result.Right, GetScrollerWidth(zsLeft));
    inc(Result.Right, GetScrollerWidth(zsRight));
  end;
end;

function TZone.GetHeaderSize: TSize;
begin
  Result.cx := 15;
  Result.cy := 15;
end;

function TZone.GetHeight: Integer;
begin
  with BoundingRect do
    Result := Bottom - Top;
end;

function TZone.GetLeft: Integer;
begin
  Result := FBoundingRect.Left;
end;

function TZone.GetTop: Integer;
begin
  Result := FBoundingRect.Top;
end;

function TZone.GetWidth: Integer;
begin
  with BoundingRect do
    Result := Right - Left;
end;

function TZone.HitTest(APoint: TPoint): TZonePart;
var
  ARect: TRect;
  BorderSize: TSize;
begin
  Result := zpNone;
  ARect := BoundingRect;
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  if not PtInRect(ARect, APoint) then
    Exit;

  // point is inside zone => check where
  ARect := ClientBounds;
  OffsetRect(ARect, -Left, -Top);
  if PtInRect(ARect, APoint) then
  begin
    Result := zpBody;
    Exit;
  end;

  // point is not in the body => either header or frame
  BorderSize := GetBorderSize;
  if (APoint.X <= BorderSize.cx) then
  begin
    Result := zpLeftFrame;
    Exit;
  end
  else
  if (APoint.Y <= BorderSize.cy) then
  begin
    Result := zpTopFrame;
    Exit;
  end
  else
  if (APoint.X >= Width - BorderSize.cx) then
  begin
    Result := zpRightFrame;
    Exit;
  end
  else
  if (APoint.Y >= Height - BorderSize.cy) then
  begin
    Result := zpBottomFrame;
    Exit;
  end;

  if zoScrollable in Options then
  begin
    if PtInRect(GetScrollerRect(zsLeft), APoint) then
    begin
      Result := zpLeftScroller;
      Exit;
    end;
    if PtInRect(GetScrollerRect(zsRight), APoint) then
    begin
      Result := zpRightScroller;
      Exit;
    end;
  end;

  if not (zoShowHeader in Options) then
    Exit;
   
  // we are in the header => check if we at button
  Result := zpHeader;

  if not (zoCollapsable in Options) then
    Exit;

  with FrameRect do
    ARect := GetExpandSignRect(Rect(Left, Bottom, Self.Width - Right, Top - 1));

  if PtInRect(ARect, APoint) then
    Result := zpExpandSign;
end;

procedure TZone.UpdateCursor;
begin
  case ActivePart of
    zpExpandSign:
      Owner.Cursor := crHandPoint;
    zpLeftFrame, zpRightFrame:
      if IsResizeableFrame(ActivePart) then
        Owner.Cursor := crHSplit;
    zpTopFrame, zpBottomFrame:
      if IsResizeableFrame(ActivePart) then
        Owner.Cursor := crVSplit;
  else
    Owner.Cursor := crDefault;
  end;
end;

function TZone.IsResizeableFrame(AFrame: TZoneFrame): Boolean;
begin
  Result := False;
end;

procedure TZone.Invalidate(Erase: Boolean = True; ARect: PRect = nil; Client: Boolean = True);
var
  AUpdateRect: TRect;
begin
  if FOwner.HandleAllocated then
  begin
    if ARect = nil then
      AUpdateRect := FBoundingRect
    else
    begin
      AUpdateRect := ARect^;
      with FBoundingRect do
        OffsetRect(AUpdateRect, Left, Top);
      if Client then
        with FrameRect do
          OffsetRect(AUpdateRect, Left, Top);
      IntersectRect(AUpdateRect, AUpdateRect, FBoundingRect);
    end;
    InvalidateRect(FOwner.Handle, @AUpdateRect, Erase);
  end;
end;

procedure TZone.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // nothing
end;

procedure TZone.KeyUp(var Key: Word; Shift: TShiftState);
begin
  // nothing
end;

procedure TZone.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  ActivePart := HitTest(Point(X, Y));
  case ActivePart of
    zpLeftScroller, zpRightScroller:
      Invalidate(False);
    zpLeftFrame, zpRightFrame:
      if IsResizeableFrame(ActivePart) then
        Owner.StartSizing(Self, stHorzSizing, Point(X, Y));
    zpTopFrame, zpBottomFrame:
      if IsResizeableFrame(ActivePart) then
        Owner.StartSizing(Self, stVertSizing, Point(X, Y));
  end;
end;

procedure TZone.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  ActivePart := HitTest(Point(X, Y));
end;

procedure TZone.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  ActivePart := HitTest(Point(X, Y));
  case ActivePart of
    zpExpandSign: Expanded := not Expanded;
    zpLeftScroller: if zsLeft in GetRequiredScrollers then Scroll(zsLeft);
    zpRightScroller: if zsRight in GetRequiredScrollers then Scroll(zsRight);
  end;
end;

function TZone.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer): Boolean;
begin
  ActivePart := HitTest(Point(X, Y));
  Result := False;
end;

procedure TZone.NonClientPaint;
begin
  // draw frame around
  DrawFrame;
  // draw header with collapse/expand button
  if zoShowHeader in Options then
    DrawHeader(GetHeaderRect);
  // draw scrollers
  if (zoScrollable in Options) then
    DrawScrollers;
end;

procedure TZone.Paint;
var
  ACanvas: TCanvas;
  Origin: TPoint;
  DC: HDC;
  RGN: HRGN;
begin
  ACanvas := Canvas; // to reduce function calls
  // change (0, 0) point
  DC := ACanvas.Handle;
  with BoundingRect do
  begin
    GetWindowOrgEx(DC, Origin);
    SetWindowOrgEx(DC, Origin.X - Left, Origin.Y - Top, nil);
  end;

  NonClientPaint;

  if Expanded then
  begin
    SetWindowOrgEx(DC, Origin.X, Origin.Y, nil);
    with ClientBounds do
      RGN := CreateRectRgn(Left, Top, Right, Bottom);
    OffsetRGN(RGN, -Origin.X, -Origin.Y);
    SelectClipRgn(DC, RGN);
    with ClientBounds do
      SetWindowOrgEx(DC, Origin.X - Left, Origin.Y - Top, nil);
    try
      ClientPaint;
    finally
      SelectClipRgn(DC, 0);
      DeleteObject(RGN);
    end;
  end;

  // restore (0, 0) point
  SetWindowOrgEx(DC, Origin.X, Origin.Y, nil);
end;

procedure TZone.SetActivePart(const Value: TZonePart);
const
  Controllers = [zpExpandSign, zpLeftScroller, zpRightScroller];
var
  OldActivePart: TZonePart;
begin
  if FActivePart <> Value then
  begin
    OldActivePart := FActivePart;
    FActivePart := Value;
    UpdateCursor;
    if (OldActivePart in Controllers) or (ActivePart in Controllers) then
      Invalidate(False);
  end;
end;

procedure TZone.SetBoundingRect(const Value: TRect);
begin
  if EqualRect(Value, FBoundingRect) then
    Exit;
  FBoundingRect := Value;
  Update;
end;

procedure TZone.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    invalidate;
  end;
end;

procedure TZone.SetExpanded(const Value: Boolean);
begin
  if (FExpanded <> Value) and
     (zoCollapsable in Options) then
  begin
    FExpanded := Value;
    Update;
  end;
end;

procedure TZone.SetHeight(const Value: Integer);
begin
  with BoundingRect do
    BoundingRect := Rect(Left, Top, Right, Top + Value);
end;

procedure TZone.SetLeft(const Value: Integer);
begin
  with BoundingRect do
    BoundingRect := Rect(Value, Top, Right - Left + Value, Bottom);
end;

procedure TZone.SetOptions(const Value: TZoneOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Update;
  end;
end;

procedure TZone.SetTop(const Value: Integer);
begin
  with BoundingRect do
    BoundingRect := Rect(Left, Value, Right, Bottom - Top + Value);
end;

procedure TZone.SetWidth(const Value: Integer);
begin
  with BoundingRect do
    BoundingRect := Rect(Left, Top, Left + Value, Bottom);
end;

procedure TZone.Update;
var
  NewBounds: TRect;
begin
  // recalculate sizes
  if not Expanded and IsRectEmpty(FSavedBounds) then
  begin
    FSavedBounds := BoundingRect;
    NewBounds := BoundingRect;
    with GetHeaderSize do
      NewBounds.Bottom := NewBounds.Top + cy;
    with GetBorderSize do
      inc(NewBounds.Bottom, cy);
    BoundingRect := NewBounds;
  end
  else
  if Expanded and not IsRectEmpty(FSavedBounds) then
  begin
    BoundingRect := FSavedBounds;
    FSavedBounds := Rect(0, 0, 0, 0);
  end;
  Owner.NotifyUpdate(Self);
  // update painting
  invalidate;
end;

procedure TZone.DblClick(X, Y: Integer);
begin
  //
end;

procedure TZone.MouseEnter;
begin

end;

procedure TZone.MouseLeave;
begin
  ActivePart := zpNone;
end;

function TZone.GetClientMousePos: TPoint;
begin
  Result := ScreenToClient(Mouse.CursorPos)
end;

function TZone.HasCapture: Boolean;
begin
  Result := Owner.GetCapture = Self;
end;

procedure TZone.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FPopupMenu) then
    FPopupMenu := nil;
end;

function TZone.ClientToScreen(const P: TPoint): TPoint;
begin
  Result := Owner.ClientToScreen(P);
  with ClientBounds, Result do
  begin
    inc(X, Left);
    inc(Y, Top);
  end;
end;

function TZone.ScreenToClient(const P: TPoint): TPoint;
begin
  Result := Owner.ScreenToClient(P);
  with ClientBounds, Result do
  begin
    dec(X, Left);
    dec(Y, Top);
  end;
end;

function TZone.GetRequiredScrollers: TZoneScrollers;
begin
  Result := [];
end;

function TZone.GetScrollerWidth(AScroller: TZoneScroller): Integer;
begin
  Result := 10;
end;

procedure TZone.DrawScroller(ARect: TRect; AScroller: TZoneScroller; AEnabled: Boolean);

  procedure DrawArrow;
  var
    P: TPoint;
    I: Integer;
    XDirection: Integer;
  begin
    // find center point
    case AScroller of
     zsLeft: XDirection := 1;
     zsRight: XDirection := -1;
    else
      XDirection := 0;
    end;
    P.X := (ARect.Left + ARect.Right) div 2 + XDirection;
    P.Y := (ARect.Top + ARect.Bottom) div 2;
    for I := 0 to 2 do
    begin
      Canvas.MoveTo(P.X - XDirection * I, P.Y - (2 - I));
      Canvas.LineTo(P.X - XDirection * I, P.Y + (2 - I) + 1);
    end;
  end;

var
  State: DWord;
begin
  State := DFCS_BUTTONPUSH;
  if not AEnabled then
    State := DFCS_INACTIVE
  else
    if ActivePart = ScrollerToPart[AScroller] then
    begin
      if GetKeyState(VK_LBUTTON) < -126 then
        State := State or DFCS_PUSHED
      else
        State := State or DFCS_HOT;
    end
    else
      State := State or DFCS_FLAT;
  DrawFrameControl(Canvas.Handle, ARect, DFC_BUTTON, State);
  DrawArrow;
end;

function TZone.GetScrollerRect(AScroller: TZoneScroller): TRect;
begin
  Result := Rect(0, 0, Width, Height);
  with FrameRect do
  begin
    inc(Result.Top, Top);
    dec(Result.Bottom, Bottom);
    if AScroller = zsLeft then
    begin
      Result.Left := Result.Right - Right;
      Result.Right := Result.Left + GetScrollerWidth(zsLeft);
    end
    else
    if AScroller = zsRight then
    begin
      Result.Left := Result.Right - Right + GetScrollerWidth(zsLeft);
      Result.Right := Result.Left + GetScrollerWidth(zsRight);
    end;
  end;
end;

procedure TZone.Scroll(AScroller: TZoneScroller);
begin
  Invalidate(False);
end;

procedure TZone.ContextPopup(X, Y: Integer; var Handled: Boolean);
begin
end;

function TZone.GetHeaderRect: TRect;
begin
  with FrameRect do
    Result := Rect(Left, Bottom, Self.Width - Right, Top);
end;

function TZone.ScreenToZone(P: TPoint): TPoint;
begin
  Result := Owner.ScreenToClient(P);
  with BoundingRect, Result do
  begin
    dec(X, Left);
    dec(Y, Top);
  end;
end;

function TZone.ZoneToScreen(P: TPoint): TPoint;
begin
  Result := Owner.ClientToScreen(P);
  with BoundingRect, Result do
  begin
    inc(X, Left);
    inc(Y, Top);
  end;
end;

procedure TZone.MoveSizing(var AInfo: TSizingInfo);
begin

end;

procedure TZone.StartSizing(var AInfo: TSizingInfo);
begin

end;

procedure TZone.StopSizing(var AInfo: TSizingInfo);
begin

end;

procedure TZone.UpdateSizingDrawInfo(var AInfo: TSizingInfo);
begin
  case AInfo.SizingType of
    stHorzSizing:
      begin
        AInfo.DrawStart := AInfo.CurPoint;
        Dec(AInfo.DrawStart.X, FrameRect.Left);
        AInfo.DrawStart.Y := 0;
        AInfo.DrawStop := AInfo.DrawStart;
        AInfo.DrawStop.Y := ClientHeight;
      end;
    stVertSizing:
      begin
        AInfo.DrawStart := AInfo.CurPoint;
        Dec(AInfo.DrawStart.Y, FrameRect.Top);
        AInfo.DrawStart.X := 0;
        AInfo.DrawStop := AInfo.DrawStart;
        AInfo.DrawStop.X := ClientWidth;
      end;
  end;
end;

procedure TZone.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

procedure TZone.HintInfo(P: TPoint; out HintStr: String; out HintRect: TRect); 
begin
  HintStr := '';
  HintRect := Rect(0, 0, 0, 0);
end;

function TZone.PointToClientPoint(P: TPoint): TPoint;
begin
  Result := P;
  with FrameRect, Result do
  begin
    dec(X, Left);
    dec(Y, Top);
  end;
end;

function TZone.ClientPointToPoint(P: TPoint): TPoint;
begin
  Result := P;
  with FrameRect, Result do
  begin
    inc(X, Left);
    inc(Y, Top);
  end;
end;

function TZone.GetPopupMenu: TPopupMenu;
begin
  PopupMenuNeeded;
  Result := FPopupMenu;
end;

function TZone.CreatePopupMenu: TPopupMenu;
var
  TopOwner: TComponent;
begin
  TopOwner := Owner;
  // Delphi XE7 has a bug with PopupMenus not owned by forms
  if not (csDesigning in Owner.ComponentState) then
  begin
    while not (TopOwner is TCustomForm) and Assigned(TopOwner.Owner) do
      TopOwner := TopOwner.Owner;
  end;
  Result := TPopupMenu.Create(TopOwner);
  Result.FreeNotification(Owner);
end;

destructor TZone.Destroy;
begin
  FreeAndNil(FPopupMenu);
  inherited;
end;

function TZone.GetOwnMenuItemIndex(MenuItem: TObject): Integer;
var
  I: Integer;
begin
  if MenuItem is TMenuItem then
  begin
    for I := 0 to High(FOwnMenuItems) do
      if FOwnMenuItems[I] = MenuItem then
      begin
        Result := I;
        Exit;
      end;
  end;
  Result := -1;
end;

procedure TZone.PopupMenuNeeded;
begin
  if not Assigned(FPopupMenu) then
  begin
    FPopupMenu := CreatePopupMenu;
    Owner.DoCreatePopupMenu(Self, FPopupMenu);
  end;
end;

{ TZoneContainer }

constructor TZoneContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csCaptureMouse, csOpaque, csDisplayDragImage];
{$IFDEF DELPHI_20UP}
  ParentDoubleBuffered := False;
{$ENDIF}
  DoubleBuffered := False;
  FZones := TZoneList.Create(Self, GetZoneClass);
  FLastMouseZone := nil;
  FKeyBoardZone := nil;
  FCaptureZone := nil;
  FSizing.ActiveZone := nil;

  Width := 700;
  Height := 400;
end;

destructor TZoneContainer.Destroy;
begin
  FZones.Free;
  inherited;
end;

function TZoneContainer.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  Pos: TPoint;
begin
  {$ifndef fpc}
  Pos := ScreenToClient(MousePos);
  {$else}
  Pos := MousePos;
  {$endif}
  with Pos do
    UpdateLastMouseZone(X, Y);
  if Assigned(LastMouseZone) then
    Result := LastMouseZone.MouseWheel(Shift, WheelDelta, Pos.X - LastMouseZone.Left, Pos.Y - LastMouseZone.Top)
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

class function TZoneContainer.GetZoneClass: TZoneClass;
begin
  Result := TZone;
end;

procedure TZoneContainer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(KeyBoardZone) then
    KeyBoardZone.KeyDown(Key, Shift);
end;

procedure TZoneContainer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Assigned(KeyBoardZone) then
    KeyBoardZone.KeyUp(Key, Shift);
end;

procedure TZoneContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Assigned(Zones) then
    Zones.Notification(AComponent, Operation);
end;

procedure TZoneContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  UpdateLastMouseZone(X, Y);
  if Assigned(LastMouseZone) then
    LastMouseZone.MouseDown(Button, Shift, X - LastMouseZone.Left, Y - LastMouseZone.Top);
end;

procedure TZoneContainer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  if Assigned(FSizing.ActiveZone) then
  begin
    InvalidateSizingLine;
    FSizing.CurPoint := Point(X - FSizing.ActiveZone.Left, Y - FSizing.ActiveZone.Top);
    FSizing.ActiveZone.MoveSizing(FSizing);
    FSizing.ActiveZone.UpdateSizingDrawInfo(FSizing);
    InvalidateSizingLine;
    Exit;
  end;

  UpdateLastMouseZone(X, Y);
  if Assigned(LastMouseZone) then
    LastMouseZone.MouseMove(Shift, X - LastMouseZone.Left, Y - LastMouseZone.Top);
end;

procedure TZoneContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Assigned(FSizing.ActiveZone) then
  begin
    InvalidateSizingLine;
    if (X <> -1) then
    begin
      FSizing.CurPoint := Point(X - FSizing.ActiveZone.Left, Y - FSizing.ActiveZone.Top);
      FSizing.ActiveZone.MoveSizing(FSizing);
      FSizing.ResizeSibling := ssCtrl in Shift;
    end;
    StopSizing;
    Exit;
  end;

  UpdateLastMouseZone(X, Y);
  if Assigned(LastMouseZone) then
    LastMouseZone.MouseUp(Button, Shift, X - LastMouseZone.Left, Y - LastMouseZone.Top);
end;

procedure TZoneContainer.NotifyUpdate(Sender: TZone);
begin
  // should be overriden in descendant
end;

procedure TZoneContainer.Paint;
var
  R, Rgn, Rgn1: HRGN;
  RgnType: Integer;
  I: Integer;
begin
  Rgn := CreateRectRgnIndirect(ClientRect);
  // skip zones
  Rgn1 := Zones.GetBoundingRgn;
  // skip own controls
  for I := 0 to ControlCount - 1 do
    if Controls[I].Visible then
    begin
      R := CreateRectRgnIndirect(Controls[I].BoundsRect);
      CombineRgn(Rgn1, Rgn1, R, RGN_OR);
      DeleteObject(R);
    end;

  RgnType := CombineRgn(Rgn, Rgn, Rgn1, RGN_DIFF);
  DeleteObject(Rgn1);
  // draw empty zones
  if RgnType <> NULLREGION then
  begin
    SelectClipRgn(Canvas.Handle, Rgn);
    PaintEmptySpace;
    SelectClipRgn(Canvas.Handle, 0);
  end;
  DeleteObject(Rgn);
  Zones.Paint;
  DrawSizingLine;  
end;

function TZoneContainer.GetCapture: TZone;
begin
  Result := FCaptureZone;
end;

procedure TZoneContainer.SetCapture(AZone: TZone);
begin
  SetCaptureControl(Self);
  FCaptureZone := AZone;
end;

procedure TZoneContainer.ReleaseCapture;
begin
  if GetCaptureControl = Self then
    SetCaptureControl(nil);
  FCaptureZone := nil;
end;

procedure TZoneContainer.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TZoneContainer.DblClick;
var
  MousePos: TPoint;
begin
  MousePos := ScreenToClient(Mouse.CursorPos);
  with MousePos do
    UpdateLastMouseZone(X, Y);
  if Assigned(LastMouseZone) then
    LastMouseZone.DblClick(MousePos.X - LastMouseZone.Left, MousePos.Y - LastMouseZone.Top)
  else
    inherited DblClick;
end;

procedure TZoneContainer.PaintEmptySpace;
begin
  Canvas.FillRect(ClientRect);
end;

procedure TZoneContainer.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  DefaultHandler(Message);
end;

procedure TZoneContainer.SetLastMouseZone(const Value: TZone);
begin
  if FLastMouseZone <> Value then
  begin
    if Assigned(FLastMouseZone) then
      FLastMouseZone.MouseLeave;
    FLastMouseZone := Value;
    if Assigned(FLastMouseZone) then
      FLastMouseZone.MouseEnter;
  end;
end;

procedure TZoneContainer.CMMouseLeave(var Message: TMessage);
begin
  LastMouseZone := nil;
end;

procedure TZoneContainer.UpdateLastMouseZone(X, Y: Integer);
var
  NewZone: TZone;
begin
  NewZone := GetCapture;
  if NewZone = nil then
    NewZone := Zones.GetZoneAt(X, Y);
  LastMouseZone := NewZone;
end;

procedure TZoneContainer.InitiateDrag;
var
  OldCaptureZone: TZone;
begin
  // the problem is that VCL BeginDrag send a mouse up message to the control
  // if zone has not capture and mouse up is happens then another zone will get
  // it and also LastMouseZone will not be equal to the zone which initiated the
  // drag. This trick prevents this unwanted behavior
  OldCaptureZone := GetCapture;
  FCaptureZone := LastMouseZone;
  try
    BeginDrag(True);
  finally
    FCaptureZone := OldCaptureZone;
  end;
end;

procedure TZoneContainer.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  with MousePos do
    if (X = -1) and (Y = -1) then
    begin
      if Assigned(KeyBoardZone) then
        DoZoneContextPopup(KeyBoardZone, -1, -1, Handled);
    end
    else
      UpdateLastMouseZone(X, Y);
  if Assigned(LastMouseZone) then
    DoZoneContextPopup(LastMouseZone, MousePos.X - LastMouseZone.Left, MousePos.Y - LastMouseZone.Top, Handled)
  else
    inherited DoContextPopup(MousePos, Handled);
end;

procedure TZoneContainer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Zones.Update;
end;

procedure TZoneContainer.StartSizing(AZone: TZone; const ASizingType: TSizingType; const APoint: TPoint);
begin
  if Assigned(FSizing.ActiveZone) then
    StopSizing;

  AZone.SetCapture;

  FSizing.ActiveZone := AZone;
  FSizing.ActivePart := AZone.ActivePart;
  FSizing.SizingType := ASizingType;
  FSizing.StartPoint := APoint;
  FSizing.CurPoint := APoint;
  FSizing.ResizeSibling := False;
  AZone.StartSizing(FSizing);
  AZone.UpdateSizingDrawInfo(FSizing);

  InvalidateSizingLine;
end;

procedure TZoneContainer.StopSizing;
begin
  if Assigned(FSizing.ActiveZone) then
  begin
    FSizing.ActiveZone.StopSizing(FSizing);
    FSizing.ActiveZone.ReleaseCapture;
    FSizing.ActiveZone := nil;
  end;
end;

procedure TZoneContainer.InvalidateSizingLine;
var
  ARect: TRect;
begin
  if not Assigned(FSizing.ActiveZone) then
    Exit;
  ARect.TopLeft := FSizing.DrawStart;
  ARect.BottomRight := FSizing.DrawStop;
  if ARect.Left = ARect.Right then
  begin
    Dec(ARect.Left);
    Inc(ARect.Right);
  end
  else
  if ARect.Top = ARect.Bottom then
  begin
    Dec(ARect.Top);
    Inc(ARect.Bottom);
  end;
  with FSizing.ActiveZone.BoundingRect do
    OffsetRect(ARect, Left, Top);
  InvalidateRect(Handle, @ARect, False);
end;

procedure TZoneContainer.DrawSizingLine;
begin
end;

procedure TZoneContainer.CMHintShow(var Msg: TCMHINTSHOW);
var
  Pos: TPoint;
  Zone: TZone;
  HintRect: TRect;
  HintStr: String;
begin
  Pos := Msg.HintInfo^.CursorPos;
  Zone := Zones.GetZoneAt(Pos.X, Pos.Y);
  if Assigned(Zone) then
  begin
    Zone.HintInfo(Point(Pos.X - Zone.Left, Pos.Y - Zone.Top), HintStr, HintRect);
    Msg.HintInfo^.HintStr := HintStr;
    with Zone.BoundingRect do
      OffsetRect(HintRect, Left, Top);
    Msg.HintInfo^.CursorRect := HintRect;
    Msg.HintInfo^.HintMaxWidth := Zone.ClientWidth;
    Dec(Pos.Y, GetSystemMetrics(SM_CYCURSOR));
    Msg.HintInfo^.HintPos := ClientToScreen(Pos);
    Msg.Result := 0; 
  end
  else
    inherited;
end;

procedure TZoneContainer.DoZoneContextPopup(AZone: TZone; X, Y: Integer; var Handled: Boolean);
begin
  Handled := False;
  if Assigned(OnZoneContextPopup) then
    OnZoneContextPopup(Self, AZone, X, Y, Handled);
  if not Handled then
    AZone.ContextPopup(X, Y, Handled);
end;

procedure TZoneContainer.DoCreatePopupMenu(AZone: TZone; PopupMenu: TPopupMenu);
begin
  if Assigned(OnZoneCreatePopupMenu) then
    OnZoneCreatePopupMenu(Self, AZone, PopupMenu);
end;

end.
