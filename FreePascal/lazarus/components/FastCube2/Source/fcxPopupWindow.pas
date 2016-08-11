{*******************************************************}
{                                                       }
{            FastCube 2 Popup Window unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxPopupWindow;

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
  SysUtils, Contnrs, Messages, Classes, Controls, StdCtrls, Graphics, Forms, Menus,
  fcxTypes, fcxStyles, fcxPainters, fcxGridPainters, fcxRes, fcxGraphicRes,
  fcxComponent, fcxControl, fcxPopupProvider;

const
// popup styles
  psFooter = 0;
  psDataItem = 1;
  psDataItemSelected = 2;
  psFirstPopupStyle = psFooter;
  psLastPopupStyle = psDataItemSelected;

// messages
  WM_DO_POPUP = WM_USER + 1;
{$IFDEF FPC}
  WM_ACTIVATE = LM_ACTIVATE;
{$ENDIF}

type
{$IFDEF FPC}
  TWMActivate = TLMActivate;
{$ENDIF}
  TfcxPopupStyles = class(TfcxCustomThemeStyles)
  private
    FStyles: array[psFirstPopupStyle..psLastPopupStyle] of TfcxCustomThemeStyle;
  protected
    function GetStyle(Index: Integer): TfcxCustomThemeStyle; override;
    procedure InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); override;
    procedure SetStyle(Index: Integer; const Value: TfcxCustomThemeStyle); override;
    procedure SetDefaultValues; override;
    function GetFirstStyleIndex: Integer; override;
    function GetLastStyleIndex: Integer; override;
  published
    property Footer: TfcxCustomThemeStyle index psFooter read GetStyle write SetStyle;
    property DataItem: TfcxCustomThemeStyle index psDataItem read GetStyle write SetStyle;
    property DataItemSelected: TfcxCustomThemeStyle index psDataItemSelected read GetStyle write SetStyle;
  end;
  TfcxPopupStylesClass = class of TfcxPopupStyles;

  TfcxPopupWindowOption = (
    pwoShowFooter,  // has footer at the bottom of the window
    pwoAllowResize, // allow to resize the window (only if footer is available)
    pwoVertScroll   // add vertical scroller (for internal use only)
  );
  TfcxPopupWindowOptions = set of TfcxPopupWindowOption;

  TfcxPopupWindowPart = (
    pwpNone,
    pwpFrame,
    pwpFooter,
    pwpResizeCorner,
    pwpFooterControl,
    pwpBody
  );

  TfcxPopupWindow = class;

  TfcxPopupControl = class
  private
    FOwner: TfcxPopupWindow;
    FCursor: TCursor;
    FHint: String;
    FAnchorToLeft: Boolean;
  protected
    procedure SetHint(const Value: String);
    procedure Draw(ACanvas: TCanvas; ARect: TRect; IsHot: Boolean); virtual; abstract;
  public
    constructor Create(AOwner: TfcxPopupWindow); virtual;
    property Cursor: TCursor read FCursor write FCursor;
    property Hint: String read FHint write SetHint;
    property AnchorToLeft: Boolean read FAnchorToLeft write FAnchorToLeft;
    property Owner: TfcxPopupWindow read FOwner;
  end;

  TfcxPopupButton = class(TfcxPopupControl)
  private
    FImageIndex: Integer;
    FOnClick: TNotifyEvent;
    FDown: Boolean;
    procedure SetImageIndex(const Value: Integer);
    procedure SetDown(const Value: Boolean);
  protected
    procedure DoClick;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; IsHot: Boolean); override;
  public
    constructor Create(AOwner: TfcxPopupWindow); override;
    property Down: Boolean read FDown write SetDown;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TfcxPopupHitTestInfo = record
    Part: TfcxPopupWindowPart;
    Index: Integer;
  end;

  { TfcxPopupWindow }

  TfcxPopupWindow = class(TCustomForm)
  private
    FPainter: TfcxCustomGridPainter;
    FOnDestroy: TNotifyEvent;
    FOptions: TfcxPopupWindowOptions;
    FActivePart: TfcxPopupWindowPart;
    FInResize: Boolean;
    FStartMousePos: TPoint;
    FStartSize: TSize;
    FScrollBar: TControl;
    FStyles: TfcxPopupStyles;
    FFooterControls: TObjectList;
    FActiveFooterControl: Integer;
    function GetPaintStyle: TfcxPaintStyle;
    procedure SetPaintStyle(const Value: TfcxPaintStyle);
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMMouseleave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMDoPopup(var Message: TMessage); message WM_DO_POPUP;
    procedure CMHintShow(var Msg: TCMHINTSHOW); message CM_HINTSHOW;
    procedure SetOptions(const Value: TfcxPopupWindowOptions);
    procedure SetActivePart(const Value: TfcxPopupWindowPart);
    procedure CreateScroll;
    procedure DestroyScroll;
    procedure SetStyles(const Value: TfcxPopupStyles);
    procedure SetActiveFooterControl(const Value: Integer);
    procedure UpdateActivePart(X, Y: Integer);
  protected
    class function GetStylesClass: TfcxPopupStylesClass; virtual;
    procedure Deactivate; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure UpdateConstraints; virtual;
    procedure InvalidateClientRect(R: TRect; Erase: Boolean = False); virtual;
    procedure InvalidateFooterControl(AIndex: Integer);
    function AddFooterButton: TfcxPopupButton;

    // mouse
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

    function MouseHitTest(X, Y: Integer): TfcxPopupHitTestInfo; virtual;
    procedure ClientMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure ClientMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure ClientMouseMove(Shift: TShiftState; X: Integer; Y: Integer); virtual;
    function ClientMouseWheel(Shift: TShiftState; WheelDelta: Integer; X: Integer; Y: Integer): Boolean; virtual;
    procedure ClientMouseLeave; virtual;
    procedure ClientDblClick(X: Integer; Y: Integer); virtual;
    procedure ClientContextPopup(X, Y: Integer; var Handled: Boolean); virtual;
    procedure ClientHintInfo(X, Y: Integer; out HintStr: String; out HintRect: TRect); virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    // scroll
    procedure ScrollChange(Sender: TObject); virtual;
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;
    procedure SetScrollParams(AScrollBar: TControl; APosition, AMin, AMax, ASmallChange, ALargeChange: Integer); virtual;
    procedure SetScrollPos(AScrollBar: TControl; APosition: Integer); virtual;
    function GetScrollParam(AScrollBar: TControl; AParam: TfcxScrollParam): Integer; virtual;

    function GetFrameSize: Integer; virtual;
    function GetFooterSize: Integer;
    function GetFooterOrigin: TPoint;
    function GetFooterClientRect: TRect;
    function GetClientRect: TRect; override;
    function GetClientOrigin: TPoint; override;
    function GetFooterControlRect(AIndex: Integer): TRect;
    function GetFooterMinWidth: Integer;

    procedure NonClientPaint; virtual;
    procedure ClientPaint; virtual;
    procedure FooterPaint; virtual;

    property ActivePart: TfcxPopupWindowPart read FActivePart write SetActivePart;
    property ActiveFooterControl: Integer read FActiveFooterControl write SetActiveFooterControl;
    property ScrollBar: TControl read FScrollBar;
    property FooterControls: TObjectList read FFooterControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PopupAt(P: TPoint); virtual;
    procedure CloseUp(Cancel: Boolean); virtual;

    property Options: TfcxPopupWindowOptions read FOptions write SetOptions;
    property Painter: TfcxCustomGridPainter read FPainter;
    property PaintStyle: TfcxPaintStyle read GetPaintStyle write SetPaintStyle default psDefault;
    property Styles: TfcxPopupStyles read FStyles write SetStyles;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TfcxNodeInfo = class
    Node: Pointer;
    BoundingRect: TRect;
  end;

  // node cache
  TfcxPopupNodeCache = class(TList)
  private
    FValid: Boolean;
    function GetItem(AIndex: Integer): TfcxNodeInfo;
    procedure SetItem(AIndex: Integer; const Value: TfcxNodeInfo);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
    function AddNodeInfo: TfcxNodeInfo;
    function FindByPosition(P: TPoint): TfcxNodeInfo;
    property Items[AIndex: Integer]: TfcxNodeInfo read GetItem write SetItem; default;
    property Valid: Boolean read FValid write FValid;
  end;

  TfcxDragControl = class(TControl)
  protected
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer); override;
  end;
  
  TfcxNodePopupDragItem = class(TfcxCustomDragItem)
  public
    function GetItem: TObject; override;
  end;

  TfcxNodePopupActivePart = (
    fppNone,
    fppItem,
    fppCheck,
    fppTreeButton
  );

  TfcxCustomNodePopup = class(TfcxPopupWindow)
  private
    FFirstVisibleRow: Integer;
    FDropDownCount: Integer;
    FFocusedNode: Pointer;
    FActiveNode: Pointer;
    FAllowResizeScrollChange: Boolean;
    FDataProvider: TfcxCustomNodeProvider;
    FNodeCache: TfcxPopupNodeCache;
    FDownPos: TPoint;
    FDragNode: Pointer;
    FDragItem: TfcxNodePopupDragItem;
    FActiveClientPart: TfcxNodePopupActivePart;
    FSearchString: String;
    function GetLastVisibleRow: Integer;
    procedure SetFirstVisibleRow(const Value: Integer);
    procedure SetDropDownCount(const Value: Integer);
    procedure SetFocusedNode(const Value: Pointer);
  protected
    FLastCursorPos: TPoint;
    procedure SetActiveNode(const Value: Pointer); virtual;
    procedure SetActiveClientPart(const Value: TfcxNodePopupActivePart); virtual;
    procedure SetDataProvider(const Value: TfcxCustomNodeProvider); virtual;
    procedure Resize; override;
    procedure ClientPaint; override;
    procedure InitDrag;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer); override;

    function GetDragItem: TObject;

    procedure DoExpandChange(Sender: TObject); virtual;

    function GetRowCount: Integer; virtual;
    function CreatePopupMenu: TPopupMenu; virtual;
    procedure PreparePopupMenuFor(ANode: Pointer); virtual;

    function GetNodeAt(X, Y: Integer): Pointer;
    function GetNodeRect(ANode: Pointer): TRect;

    function GetVisibleRowCount(FullyVisible: Boolean = True): Integer;
    function GetRowHeight: Integer;
    function GetRowRect(AIndex: Integer): TRect;
    procedure UpdateConstraints; override;
    procedure UpdateNeedScroll;
    procedure UpdateActiveIndex(X, Y: Integer); virtual;

    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure ClientMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ClientMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure ClientMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    function ClientMouseWheel(Shift: TShiftState; WheelDelta: Integer; X: Integer; Y: Integer): Boolean; override;
    procedure ClientMouseLeave; override;
    procedure ClientDblClick(X: Integer; Y: Integer); override;
    procedure ClientContextPopup(X, Y: Integer; var Handled: Boolean); override;
    procedure ClientHintInfo(X, Y: Integer; out HintStr: String; out HintRect: TRect); override;

    function GetTreeRect(R: TRect): TRect;
    procedure DrawNode(ANode: Pointer; R: TRect); virtual;
    procedure InvalidateNode(ANode: Pointer);
    procedure DoStateChange(Sender: TObject; ANode: Pointer); virtual;
    procedure ClickNodeCheck(ANode: Pointer; ForceSingleCheck: Boolean = False); virtual;
    function IncSearch(const NewSearchString: String): Boolean;

    property FirstVisibleRow: Integer read FFirstVisibleRow write SetFirstVisibleRow;
    property LastVisibleRow: Integer read GetLastVisibleRow;
    property FocusedNode: Pointer read FFocusedNode write SetFocusedNode;
    property ActiveNode: Pointer read FActiveNode write SetActiveNode;
    property ActiveClientPart: TfcxNodePopupActivePart read FActiveClientPart write SetActiveClientPart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;
    procedure PopupAt(P: TPoint); override;
    procedure CloseUp(Cancel: Boolean); override;

    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 10;
    property DataProvider: TfcxCustomNodeProvider read FDataProvider write SetDataProvider;
  end;

implementation

uses
  Math;

const
  ControlSpacing = 1;

{ TfcxPopupWindow }

procedure TfcxPopupWindow.ClientMouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
end;

procedure TfcxPopupWindow.ClientMouseLeave;
begin
end;

procedure TfcxPopupWindow.ClientMouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
begin
end;

procedure TfcxPopupWindow.ClientMouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
end;

function TfcxPopupWindow.ClientMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; X: Integer; Y: Integer): Boolean;
begin
  Result := False;
end;

procedure TfcxPopupWindow.ClientPaint;
begin
  Painter.DrawStyledBackground(Canvas, ClientRect, Styles.DataItem);
end;

procedure TfcxPopupWindow.CloseUp(Cancel: Boolean);
begin
  Close;
end;

constructor TfcxPopupWindow.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  FFooterControls := TObjectList.Create(True);
  FPainter := TfcxDefaultGridPainter.Create;
  FStyles := GetStylesClass.Create(Self);
  FActivePart := pwpNone;
  FActiveFooterControl := -1;
  BorderStyle := bsNone;
  // D7- only solution. For LCL and D7+ use PopupParent
  {$IF DEFINED(DELPHI_9UP) OR DEFINED(FPC)}
    PopupMode := pmAuto;
  {$ELSE}
    FormStyle := fsStayOnTop;
  {$IFEND}
  Width := 180;
end;

procedure TfcxPopupWindow.CreateScroll;
begin
  FScrollBar := TScrollBar.Create(Self);
  FScrollBar.ControlStyle := FScrollBar.ControlStyle - [csFramed];
  with FScrollBar as TScrollBar do
  begin
    Kind := sbVertical;
    TabStop := False;
    OnScroll := ScrollScroll;
    OnChange := ScrollChange;
  end;
  FScrollBar.Parent := Self;
  FScrollBar.Anchors := [akTop, akRight, akBottom];
  FScrollBar.Left := Width - FScrollBar.Width - GetFrameSize;
  FScrollBar.Top := GetFrameSize;
  FScrollBar.Height := ClientHeight;
end;

procedure TfcxPopupWindow.Deactivate;
begin
  // window is deactivated by another window => close it
  inherited;
  if Visible then
    CloseUp(False);
end;

destructor TfcxPopupWindow.Destroy;
begin
  FFooterControls.Free;
  FPainter.Free;
  FStyles.Free;
  if Assigned(FOnDestroy) then
    OnDestroy(Self);
  inherited;
end;

procedure TfcxPopupWindow.DestroyScroll;
begin
  FScrollBar.Free;
end;

procedure TfcxPopupWindow.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
end;

function TfcxPopupWindow.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  {$ifndef fpc}
  with ScreenToClient(MousePos) do
  {$else}
  with MousePos do
  {$endif}
  begin
    UpdateActivePart(X, Y);
    if ActivePart = pwpBody then
      Result := ClientMouseWheel(Shift, WheelDelta, X - GetFrameSize, Y - GetFrameSize);
  end;
end;

procedure TfcxPopupWindow.FooterPaint;
var
  R, BR: TRect;
  I: Integer;
begin
  R := GetFooterClientRect;
  Painter.DrawStatus(Canvas, R, Styles.Footer);
  for I := 0 to FooterControls.Count - 1 do
  begin
    BR := GetFooterControlRect(I);
    TfcxPopupControl(FooterControls[I]).Draw(Canvas, BR, (ActivePart = pwpFooterControl) and (ActiveFooterControl = I));
  end;
  if pwoAllowResize in Options then
    Painter.DrawSizeGrip(Canvas, Painter.GetSizeGripRect(R));
end;

function TfcxPopupWindow.GetClientOrigin: TPoint;
begin
  Result := inherited GetClientOrigin;
  inc(Result.X, GetFrameSize);
  inc(Result.Y, GetFrameSize);
end;

function TfcxPopupWindow.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  dec(Result.Right, GetFrameSize * 2);
  dec(Result.Bottom, GetFrameSize * 2);
  if pwoShowFooter in Options then
    dec(Result.Bottom, GetFooterSize);
end;

function TfcxPopupWindow.GetFooterClientRect: TRect;
begin
  Result := Rect(0, 0, ClientWidth, GetFooterSize);
end;

function TfcxPopupWindow.GetFooterSize: Integer;
begin
  if pwoShowFooter in Options then
    Result := 21
  else
    Result := 0;
end;

function TfcxPopupWindow.GetFrameSize: Integer;
begin
  Result := 1;
end;

function TfcxPopupWindow.GetPaintStyle: TfcxPaintStyle;
begin
  Result := Painter.GetPaintStyle;
end;

function TfcxPopupWindow.GetScrollParam(AScrollBar: TControl; AParam: TfcxScrollParam): Integer;
var
  ScrollBar: TScrollBar absolute AScrollBar;
begin
  case AParam of
    spMin: Result := ScrollBar.Min;
    spMax: Result := ScrollBar.Max;
    spPosition: Result := ScrollBar.Position;
    spSmallChange: Result := ScrollBar.SmallChange;
    spLargeChange: Result := ScrollBar.LargeChange;
  else
    Result := 0;
  end;
end;

class function TfcxPopupWindow.GetStylesClass: TfcxPopupStylesClass;
begin
  Result := TfcxPopupStyles;
end;

procedure TfcxPopupWindow.InvalidateClientRect(R: TRect; Erase: Boolean);
begin
  OffsetRect(R, GetFrameSize, GetFrameSize);
  InvalidateRect(Handle, @R, Erase);
end;

procedure TfcxPopupWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited;
  UpdateActivePart(X, Y);
  if (Button = mbLeft) and (ActivePart = pwpResizeCorner) then
  begin
    FStartMousePos := Point(X, Y);
    FStartSize.cx := Width;
    FStartSize.cy := Height;
    FInResize := True;
    SetCaptureControl(Self);
  end
  else
  if ActivePart = pwpFooterControl then
  begin
    if ActiveFooterControl <> -1 then
      InvalidateFooterControl(ActiveFooterControl);
  end
  else
  if ActivePart = pwpBody then
    ClientMouseDown(Button, Shift, X - GetFrameSize, Y - GetFrameSize)
end;

function TfcxPopupWindow.MouseHitTest(X, Y: Integer): TfcxPopupHitTestInfo;
var
  FrameSize: Integer;
  R: TRect;
  P: TPoint;
  I: Integer;
begin
  Result.Part := pwpNone;
  Result.Index := -1;
  if (X < 0) or (Y < 0) or
     (X > Width) or (Y > Height) then
    Exit;

  FrameSize := GetFrameSize;
  if (X <= FrameSize) or (Y <= FrameSize) or
     (X >= Width - FrameSize) or (Y >= Height - FrameSize) then
  begin
    Result.Part := pwpFrame;
    Exit;
  end;
  P := GetFooterOrigin;
  if (pwoShowFooter in Options) and (Y >= P.Y) and (Y < Height) then
  begin
    Result.Part := pwpFooter;

    // check button
    for I := 0 to FooterControls.Count - 1 do
    begin
      R := GetFooterControlRect(I);
      OffsetRect(R, P.X, P.Y);
      if PtInRect(R, Point(X, Y)) then
      begin
        Result.Part := pwpFooterControl;
        Result.Index := I;
        Exit;
      end;
    end;

    if (pwoAllowResize in Options) then
    begin
      R := Rect(P.X, P.Y, Width - FrameSize, Height - FrameSize);
      R := Painter.GetSizeGripRect(R);
      if PtInRect(R, Point(X, Y)) then
        Result.Part := pwpResizeCorner;
    end;
    Exit;
  end;
  Result.Part := pwpBody;
end;

procedure TfcxPopupWindow.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  if FInResize then
    SetBounds(Left, Top, FStartSize.cx + (X - FStartMousePos.X), FStartSize.cy + (Y - FStartMousePos.Y))
  else
  begin
    UpdateActivePart(X, Y);
    if ActivePart = pwpBody then
      ClientMouseMove(Shift, X - GetFrameSize, Y - GetFrameSize);
  end;
end;

procedure TfcxPopupWindow.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  inherited;
  if FInResize then
  begin
    SetBounds(Left, Top, FStartSize.cx + (X - FStartMousePos.X), FStartSize.cy + (Y - FStartMousePos.Y));
    Invalidate;
    FInResize := False;
    if GetCaptureControl = Self then
      SetCaptureControl(nil);
  end;
  UpdateActivePart(X, Y);
  if ActivePart = pwpFooterControl then
  begin
    if (Button = mbLeft) and (ActiveFooterControl <> -1) then
    begin
      InvalidateFooterControl(ActiveFooterControl);
      TfcxPopupButton(FooterControls[ActiveFooterControl]).DoClick;
    end;
  end
  else
  if ActivePart = pwpBody then
    ClientMouseUp(Button, Shift, X - GetFrameSize, Y - GetFrameSize)
end;

procedure TfcxPopupWindow.NonClientPaint;
begin
  Painter.DrawBorder(Canvas, Rect(0, 0, Width, Height), tsNormal);
end;

procedure TfcxPopupWindow.Paint;
var
  ACanvas: TCanvas;
  OldWindowOrgEx: TPoint;
begin
  ACanvas := Canvas; // to reduce function calls
  GetWindowOrgEx(ACanvas.Handle, OldWindowOrgEx);

  NonClientPaint;
  if pwoShowFooter in Options then
  begin
    with GetFooterOrigin do
      SetWindowOrgEx(ACanvas.Handle, -X, -Y, nil);
    FooterPaint;
  end;

  SetWindowOrgEx(ACanvas.Handle, -GetFrameSize, -GetFrameSize, nil);
  ClientPaint;

  // restore (0, 0) point
  SetWindowOrgEx(ACanvas.Handle, OldWindowOrgEx.x, OldWindowOrgEx.y, nil);
end;

procedure TfcxPopupWindow.PopupAt(P: TPoint);
begin
  Left := P.X;
  Top := P.Y;
  // postpone Show. This is needed for Lazarus on Carbon.
  PostMessage(Handle, WM_DO_POPUP, 0, 0);
end;

procedure TfcxPopupWindow.ScrollChange(Sender: TObject);
var
  AScrollPos: Integer;
begin
  AScrollPos := TScrollBar(Sender).Position;
  if Assigned(TScrollBar(Sender).OnScroll) then
    TScrollBar(Sender).OnScroll(Sender, scPosition, AScrollPos);
end;

procedure TfcxPopupWindow.ScrollScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  //
end;

procedure TfcxPopupWindow.SetActivePart(const Value: TfcxPopupWindowPart);
var
  R: TRect;
begin
  if FActivePart <> Value then
  begin
    if FActivePart = pwpBody then
      ClientMouseLeave;
    if FActivePart in [pwpFooter, pwpFooterControl] then
    begin
      R := GetFooterClientRect;
      with GetFooterOrigin do
        OffsetRect(R, X, Y);
      InvalidateRect(Handle, @R, False);
    end;
    FActivePart := Value;
    if ActivePart <> pwpFooterControl then
      ActiveFooterControl := -1;
  end;
end;

procedure TfcxPopupWindow.SetOptions(const Value: TfcxPopupWindowOptions);
begin
  if FOptions <> Value then
  begin
    if pwoVertScroll in (FOptions - Value) then
      DestroyScroll
    else
    if pwoVertScroll in (Value - FOptions) then
      CreateScroll;
    FOptions := Value;
    UpdateConstraints;
  end;
end;

procedure TfcxPopupWindow.SetPaintStyle(const Value: TfcxPaintStyle);
begin
  if PaintStyle <> Value then
  begin
    FPainter.Free;
    FPainter := GridPainterClass[Value].Create;
    invalidate;
  end;
end;

procedure TfcxPopupWindow.SetScrollParams(AScrollBar: TControl; APosition,
  AMin, AMax, ASmallChange, ALargeChange: Integer);
var
  ScrollBar: TScrollBar absolute AScrollBar;
begin
  if AMax < AMin then
    AMax := AMin;
  ScrollBar.SetParams(APosition, AMin, AMax);
  ScrollBar.SmallChange := ASmallChange;
  ScrollBar.LargeChange := ALargeChange;
end;

procedure TfcxPopupWindow.SetScrollPos(AScrollBar: TControl; APosition: Integer);
var
  ScrollBar: TScrollBar absolute AScrollBar;
begin
  ScrollBar.Position := APosition;
end;

procedure TfcxPopupWindow.SetStyles(const Value: TfcxPopupStyles);
begin
  FStyles.Assign(Value);
  Invalidate;
end;

procedure TfcxPopupWindow.UpdateConstraints;
begin
end;

procedure TfcxPopupWindow.WMActivate(var Message: TWMActivate);
begin
  // our popup window is a top level window and therefore when it becomes active
  // the previos window start to show that it is inactive. to prevent this we
  // can send WM_NCACTIVATE message to the previosly active window to change it
  // visual style (windows only solution)
  inherited;
  {$IFDEF MSWindows}
  if (Message.Active <> WA_INACTIVE) and (Message.ActiveWindow <> 0) then
    SendMessage(Message.ActiveWindow, WM_NCACTIVATE, 1, 0);
  {$ENDIF}
end;

procedure TfcxPopupWindow.WMEraseBkgnd(var Message: TMessage);
begin
  DefaultHandler(Message);
end;

procedure TfcxPopupWindow.WMMouseleave(var Message: TMessage);
begin
  inherited;
  ActivePart := pwpNone;
end;

procedure TfcxPopupWindow.WMDoPopup(var Message: TMessage);
begin
  Show;
end;

function TfcxPopupWindow.AddFooterButton: TfcxPopupButton;
begin
  Result := TfcxPopupButton.Create(Self);
  FooterControls.Add(Result);
end;

function TfcxPopupWindow.GetFooterControlRect(AIndex: Integer): TRect;
var
  i, APos: Integer;
begin
  Result := GetFooterClientRect;
  inc(Result.Top);
  if pwoAllowResize in Options then
    Result.Right := Painter.GetSizeGripRect(Result).Left;
  InflateRect(Result, -ControlSpacing, -ControlSpacing);
  APos := 0;
  if TfcxPopupControl(FooterControls[AIndex]).AnchorToLeft then
  begin
    for i := 0 to AIndex - 1 do
      if TfcxPopupControl(FooterControls[i]).AnchorToLeft then
        inc(APos);
    Inc(Result.Left, APos * (Result.Bottom - Result.Top + ControlSpacing));
    Result.Right := Result.Left + Result.Bottom - Result.Top;
  end
  else
  begin
    for i := AIndex + 1 to FooterControls.Count - 1 do
      if not TfcxPopupControl(FooterControls[i]).AnchorToLeft then
        inc(APos);
    Dec(Result.Right, APos * (Result.Bottom - Result.Top + ControlSpacing));
    Result.Left := Result.Right - (Result.Bottom - Result.Top);
  end;
end;

procedure TfcxPopupWindow.SetActiveFooterControl(const Value: Integer);
begin
  if FActiveFooterControl <> Value then
  begin
    if FActiveFooterControl <> -1 then
      InvalidateFooterControl(FActiveFooterControl);
    FActiveFooterControl := Value;
    if Value <> -1 then
      InvalidateFooterControl(Value);
  end;
end;

function TfcxPopupWindow.GetFooterOrigin: TPoint;
begin
  Result := Point(GetFrameSize, Height - GetFrameSize - GetFooterSize);
end;

procedure TfcxPopupWindow.InvalidateFooterControl(AIndex: Integer);
var
  R: TRect;
begin
  R := GetFooterControlRect(AIndex);
  with GetFooterOrigin do
    OffsetRect(R, X, Y);
  InvalidateRect(Handle, @R, False);
end;

procedure TfcxPopupWindow.UpdateActivePart(X, Y: Integer);
var
  Info: TfcxPopupHitTestInfo;
begin
  Info := MouseHitTest(X, Y);
  ActivePart := Info.Part;
  if ActivePart = pwpFooterControl then
    ActiveFooterControl := Info.Index
  else
    ActiveFooterControl := -1;
  case ActivePart of
    pwpResizeCorner:
      Cursor := crSizeNWSE;
    pwpFooterControl:
      Cursor := TfcxPopupControl(FooterControls[ActiveFooterControl]).Cursor;
    else
      Cursor := crDefault;
  end;
end;

function TfcxPopupWindow.GetFooterMinWidth: Integer;
begin
  Result := FooterControls.Count * (GetFooterSize + ControlSpacing) + 2 * ControlSpacing;
  if pwoAllowResize in Options then
    with Painter.GetSizeGripRect(Rect(0, 0, GetFooterSize, GetFooterSize)) do
      inc(Result, Right - Left);
end;

procedure TfcxPopupWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
    CloseUp(True);
end;

procedure TfcxPopupWindow.Resize;
begin
  inherited;
  Invalidate;
end;

procedure TfcxPopupWindow.DblClick;
begin
  inherited;
  with ScreenToClient(Mouse.CursorPos) do
  begin
    UpdateActivePart(X, Y);
    if ActivePart = pwpBody then
      ClientDblClick(X - GetFrameSize, Y - GetFrameSize);
  end;
end;

procedure TfcxPopupWindow.ClientDblClick(X: Integer; Y: Integer);
begin
end;

procedure TfcxPopupWindow.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if ActivePart = pwpBody then
    if (MousePos.X = -1) and (MousePos.Y = -1) then
      ClientContextPopup(-1, -1, Handled)
    else
      ClientContextPopup(MousePos.X - GetFrameSize, MousePos.Y - GetFrameSize, Handled)
  else
    inherited DoContextPopup(MousePos, Handled);
end;

procedure TfcxPopupWindow.ClientContextPopup(X, Y: Integer; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TfcxPopupWindow.CMHintShow(var Msg: TCMHINTSHOW);
var
  Pos: TPoint;
  ItemRect: TRect;
  HintStr: String;
  Sz: Integer;
begin
  Pos := Msg.HintInfo^.CursorPos;
  case ActivePart of
    pwpFooterControl:
      begin
        HintStr := TfcxPopupControl(FooterControls[ActiveFooterControl]).Hint;
        ItemRect := GetFooterControlRect(ActiveFooterControl);
        with GetFooterOrigin do
          OffsetRect(ItemRect, X, Y);
      end;
    pwpBody:
      begin
        Sz := GetFrameSize;
        ClientHintInfo(Pos.X - Sz, Pos.Y - Sz, HintStr, ItemRect);
        OffsetRect(ItemRect, Sz, Sz);
      end;
    else
      HintStr := '';
  end;
  if HintStr <> '' then
  begin
    Msg.HintInfo^.HintStr := HintStr;
    Msg.HintInfo^.CursorRect := ItemRect;
    Msg.HintInfo^.HintMaxWidth := ClientWidth;
    Dec(Pos.Y, GetSystemMetrics(SM_CYCURSOR));
    Msg.HintInfo^.HintPos := ClientToScreen(Pos);
    Msg.Result := 0; 
  end
  else
    inherited;
end;

procedure TfcxPopupWindow.ClientHintInfo(X, Y: Integer;
  out HintStr: String; out HintRect: TRect);
begin
  HintStr := '';
  HintRect := Rect(0, 0, 0, 0);
end;

{ TfcxPopupStyles }

function TfcxPopupStyles.GetFirstStyleIndex: Integer;
begin
  Result := psFirstPopupStyle;
end;

function TfcxPopupStyles.GetLastStyleIndex: Integer;
begin
  Result := psLastPopupStyle;
end;

function TfcxPopupStyles.GetStyle(Index: Integer): TfcxCustomThemeStyle;
begin
  Result := FStyles[Index]
end;

procedure TfcxPopupStyles.InternalSetStyle(Index: Integer; const Value: TfcxCustomThemeStyle);
begin
  FStyles[Index] := Value;
end;

procedure TfcxPopupStyles.SetDefaultValues;
begin
  Footer.Update(clBtnFace, clNone, clBlack);
  DataItem.Update(clWhite, clNone, clBlack);
  DataItemSelected.Update(clHighlight, clNone, clHighlightText);
end;

procedure TfcxPopupStyles.SetStyle(Index: Integer; const Value: TfcxCustomThemeStyle);
begin
  FStyles[Index].Assign(Value);
end;

{ TfcxPopupButton }

constructor TfcxPopupButton.Create(AOwner: TfcxPopupWindow);
begin
  inherited;
  FCursor := crHandPoint;
end;

procedure TfcxPopupButton.DoClick;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TfcxPopupButton.Draw(ACanvas: TCanvas; ARect: TRect; IsHot: Boolean);
var
  State: TfcxThemeState;
begin
  if Down then
    State := tsPressed
  else
  if IsHot then
    if GetKeyState(VK_LBUTTON) < -126 then
      State := tsPressed
    else
      State := tsHot
  else
    State := tsNormal;
  Owner.Painter.DrawButton(ACanvas, ARect, State);
  if ImageIndex <> -1 then
  begin
    ARect := Owner.Painter.GetButtonContent(ACanvas, ARect, State);
    fcxGraphicResources.PopupUtilImages.Draw(ACanvas,
      (ARect.Left + ARect.Right - fcxGraphicResources.PopupUtilImages.Width) div 2,
      (ARect.Top + ARect.Bottom - fcxGraphicResources.PopupUtilImages.Height) div 2,
      ImageIndex);
  end;
end;

procedure TfcxPopupButton.SetDown(const Value: Boolean);
begin
  FDown := Value;
end;

procedure TfcxPopupButton.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

{ TfcxPopupControl }

constructor TfcxPopupControl.Create(AOwner: TfcxPopupWindow);
begin
  inherited Create;
  FOwner := AOwner;
  FCursor := crDefault;
  FAnchorToLeft := True;
end;

procedure TfcxPopupControl.SetHint(const Value: String);
begin
  FHint := Value;
end;

{ TfcxPopupNodeCache }

function TfcxPopupNodeCache.AddNodeInfo: TfcxNodeInfo;
begin
  Result := TfcxNodeInfo.Create;
  Add(Result);
end;

constructor TfcxPopupNodeCache.Create;
begin
  inherited Create;
  FValid := False;
end;

function TfcxPopupNodeCache.FindByPosition(P: TPoint): TfcxNodeInfo;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if PtInRect(Items[i].BoundingRect, P) then
    begin
      Result := Items[i];
      Exit;
    end;
  Result := nil;
end;

function TfcxPopupNodeCache.GetItem(AIndex: Integer): TfcxNodeInfo;
begin
  Result := TfcxNodeInfo(inherited Get(AIndex));
end;

procedure TfcxPopupNodeCache.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TfcxNodeInfo(Ptr).Free;
end;

procedure TfcxPopupNodeCache.SetItem(AIndex: Integer; const Value: TfcxNodeInfo);
begin
  inherited Put(AIndex, Value);
end;

{ TfcxDragControl }

procedure TfcxDragControl.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  TfcxCustomNodePopup(Owner).DoEndDrag(Target, X, Y);
  Free;
end;

procedure TfcxDragControl.DoStartDrag(var DragObject: TDragObject);
begin
  TfcxCustomNodePopup(Owner).DoStartDrag(DragObject);
end;

{ TfcxFilterPopupDragItem }

function TfcxNodePopupDragItem.GetItem: TObject;
begin
  Result := TfcxCustomNodePopup(Control).GetDragItem;
end;

{ TfcxCustomNodePopup }

procedure TfcxCustomNodePopup.ClientContextPopup(X, Y: Integer; var Handled: Boolean);
var
  Pt: TPoint;
begin
  Handled := True;
  Pt := Point(X, Y);
  if (Pt.X = -1) and (Pt.Y = -1) then
  begin
    PreparePopupMenuFor(ActiveNode);
    Pt := GetNodeRect(ActiveNode).BottomRight;
  end
  else
    PreparePopupMenuFor(GetNodeAt(X, Y));
  with ClientToScreen(Pt) do
    PopupMenu.Popup(X, Y);
end;

procedure TfcxCustomNodePopup.ClientDblClick(X, Y: Integer);
begin
  inherited;
  UpdateActiveIndex(X, Y);
end;

procedure TfcxCustomNodePopup.ClientMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // find the row and set ItemIndex
  IncSearch('');
  UpdateActiveIndex(X, Y);
  if Button in [mbLeft, mbRight] then
  begin
    if ActiveNode <> nil then
      FocusedNode := ActiveNode;
  end;
  if (Button = mbLeft) and not (ssDouble in Shift) and Assigned(ActiveNode) and DataProvider.AllowDrag then
  begin
    FDownPos := FLastCursorPos;
    FDragNode := ActiveNode;
    SetCaptureControl(Self);
  end;
end;

procedure TfcxCustomNodePopup.ClientMouseLeave;
begin
  ActiveNode := nil;
end;

procedure TfcxCustomNodePopup.ClientMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (FDownPos.X <> -1) and Assigned(FDragNode) and
     (
      (Abs(FDownPos.X - X) > Mouse.DragThreshold) or
       (Abs(FDownPos.Y - Y) > Mouse.DragThreshold)
     ) then
  begin
    FDownPos.X := -1;
    if GetCaptureControl = Self then
      ReleaseCapture;
    InitDrag;
  end
  else
    UpdateActiveIndex(X, Y);
end;

procedure TfcxCustomNodePopup.ClientMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FDownPos.X <> -1) and Assigned(FDragNode) then
  begin
    if GetCaptureControl = Self then
      ReleaseCapture;
    FDownPos.X := -1;
  end;
  UpdateActiveIndex(X, Y);
  if Button = mbLeft then
  begin
    if (ActiveNode <> nil) then
      case ActiveClientPart of
        fppCheck: ClickNodeCheck(ActiveNode, ssCtrl in Shift);
        fppTreeButton: DataProvider.NodeExpanded[ActiveNode] := not DataProvider.NodeExpanded[ActiveNode];
      end;
  end;
end;

function TfcxCustomNodePopup.ClientMouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer): Boolean;
begin
  UpdateActiveIndex(X, Y);
  if pwoVertScroll in Options then
  begin
    SetScrollPos(ScrollBar, GetScrollParam(ScrollBar, spPosition) - Sign(WheelDelta) * Mouse.WheelScrollLines);
    Result := True;
  end
  else
    Result := False;
end;

procedure TfcxCustomNodePopup.ClientPaint;
var
  i, CurRow: integer;
  CR, R: TRect;
  RGN: HRGN;

  procedure Traverse(ANode: Pointer);
  begin
    while Assigned(ANode) and (R.Top < CR.Bottom) do
    begin
      if CurRow >= FirstVisibleRow then
      begin
        R.Bottom := R.Top + GetRowHeight;
        with FNodeCache.AddNodeInfo do
        begin
          Node := ANode;
          BoundingRect := R;
        end;
        DrawNode(ANode, R);
        R.Top := R.Bottom;
      end;
      inc(CurRow);

      if DataProvider.NodeExpanded[ANode] then
        Traverse(DataProvider.GetFirstChild(ANode));
      ANode := DataProvider.GetNextSibling(ANode);
    end;
  end;

begin
  CR := ClientRect;
  if pwoVertScroll in Options then
    CR.Right := CR.Right - ScrollBar.Width;
  R := CR;
  with CR do
    RGN := CreateRectRgn(Left + GetFrameSize, Top + GetFrameSize, Right + GetFrameSize, Bottom + GetFrameSize);
  try
    SelectClipRgn(Canvas.Handle, RGN);
    if FNodeCache.Valid then
    begin
      for i := 0 to FNodeCache.Count - 1 do
      begin
        R := FNodeCache[i].BoundingRect;
        DrawNode(FNodeCache[i].Node, R);
      end;
    end
    else
    begin
      FNodeCache.Clear;
      R.Bottom := R.Top;
      CurRow := 0;
      Traverse(DataProvider.GetFirstNode);
      FNodeCache.Valid := True;
    end;
    if (R.Bottom < CR.Bottom) then
      Painter.DrawStyledBackground(Canvas, Rect(CR.Left, R.Bottom, CR.Right, CR.Bottom), Styles.DataItem);
  finally
    SelectClipRgn(Canvas.Handle, 0);
    DeleteObject(RGN);
  end;
end;

procedure TfcxCustomNodePopup.CloseUp(Cancel: Boolean);
begin
  inherited;
  DataProvider.AfterCloseUp(Cancel);
end;

constructor TfcxCustomNodePopup.Create(AOwner: TComponent);
begin
  inherited;
  FActiveClientPart := fppNone;
  FNodeCache := TfcxPopupNodeCache.Create;
  FDataProvider := nil;
  FFirstVisibleRow := 0;
  FDropDownCount := 10;
  FFocusedNode := nil;
  FActiveNode := nil;
  FDownPos := Point(-1, -1);
  FAllowResizeScrollChange := False;
  UpdateConstraints;
  Height := Constraints.MinHeight;
  Width := Constraints.MinWidth;
  PopupMenu := CreatePopupMenu;
  FDragItem := TfcxNodePopupDragItem.Create(Self);
  FSearchString := '';
end;

function TfcxCustomNodePopup.CreatePopupMenu: TPopupMenu;
begin
  Result := TPopupMenu.Create(Self);
end;

destructor TfcxCustomNodePopup.Destroy;
begin
  FDragItem.Free;
  FNodeCache.Free;
  DataProvider.Free;
  PopupMenu.Free;
  inherited;
end;

procedure TfcxCustomNodePopup.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  FNodeCache.Valid := False;
  InvalidateClientRect(GetClientRect, False);
  UpdateNeedScroll;
end;

procedure TfcxCustomNodePopup.DoExpandChange(Sender: TObject);
begin
  FNodeCache.Valid := False;
  InvalidateClientRect(GetClientRect, False);
end;

procedure TfcxCustomNodePopup.DoStartDrag(var DragObject: TDragObject);
begin
  DragObject := FDragItem;
end;

procedure TfcxCustomNodePopup.DrawNode(ANode: Pointer; R: TRect);
const
  TextFlags = DT_LEFT or DT_VCENTER or DT_SINGLELINE;
  TreeButtonKind: array[Boolean] of TfcxTreeButtonKind = (tbkPlusButton, tbkMinusButton);

  function GetParentAtLevel(const ALevel: Integer): Pointer;
  var
    I: Integer;
  begin
    Result := ANode;
    for I := DataProvider.GetNodeLevel(ANode) downto ALevel + 1 do
      Result := DataProvider.GetParent(ANode);
  end;

var
  TextRect, CheckRect, TreeRect: TRect;
  State: TfcxThemeState;
  Style: TfcxCustomThemeStyle;
  i, Level, RowHeight: Integer;
  NodeKind: TfcxNodeKind;
  Text, SelectionText: String;
begin
  Level := DataProvider.GetNodeLevel(ANode);
  RowHeight := R.Bottom - R.Top;

  Style := Styles.DataItem;

  if DataProvider.IsTreeLike then
  begin
    Painter.DrawStyledBackground(Canvas, Rect(R.Left, R.Top, R.Left + Level * RowHeight, R.Bottom), Style);
    for i := 0 to Level - 1 do
    begin
      if Assigned(DataProvider.GetNextSibling(GetParentAtLevel(i))) then
        Painter.DrawDottedVLine(Canvas, R.Left + RowHeight shr 1, R.Top, R.Bottom);
      inc(R.Left, RowHeight);
    end;
    
    TreeRect := GetTreeRect(R);
    Painter.DrawStyledBackground(Canvas, TreeRect, Style);
    R.Left := TreeRect.Right;

    Dec(TreeRect.Right, RowHeight shr 2);
    inflateRect(TreeRect, -3, -3);

    // draw expand/collapse button
    if Assigned(DataProvider.GetFirstChild(ANode)) then
    begin
      Painter.DrawDottedHLine(Canvas, (R.Top + R.Bottom) shr 1, TreeRect.Right + 1, TreeRect.Right + RowHeight shr 2 + 1);
      Painter.DrawTreeButton(Canvas, TreeRect.TopLeft, TreeButtonKind[DataProvider.NodeExpanded[ANode]], Styles.DataItem, False);
    end
    else
    begin
      if ANode <> DataProvider.GetFirstNode then
        Painter.DrawDottedVLine(Canvas, (TreeRect.Left + TreeRect.Right) shr 1, TreeRect.Top - 3, (TreeRect.Bottom + TreeRect.Top) shr 1);

      if Assigned(DataProvider.GetNextSibling(ANode)) then
        Painter.DrawDottedVLine(Canvas, (TreeRect.Left + TreeRect.Right) shr 1, (TreeRect.Bottom + TreeRect.Top) shr 1, TreeRect.Top + RowHeight shr 1 + 6);

      Painter.DrawDottedHLine(Canvas, (TreeRect.Bottom + TreeRect.Top) shr 1, TreeRect.Right - RowHeight shr 1 + 2, TreeRect.Right + RowHeight shr 2 + 1);
    end;
  end;

  TextRect := R;
  NodeKind := DataProvider.GetNodeKind(ANode);
  if NodeKind in [nkCheck, nkRadio] then
  begin
    CheckRect := Painter.GetCheckRect(R);
    TextRect.Left := CheckRect.Right + (CheckRect.Left - R.Left);
    R.Right := TextRect.Left;
    Painter.DrawStyledBackground(Canvas, R, Style);

    if (ANode = ActiveNode) and (ActiveClientPart = fppCheck) then
    begin
      if GetKeyState(VK_LBUTTON) < -126 then
        State := tsPressed
      else
        State := tsHot
    end
    else
      State := tsNormal;

    //OutputDebugString(PChar(Format('State for: %d = %d node rect %d.%d - %d.%d', [PtrInt(ANode), Ord(State), CheckRect.Left, CheckRect.Top, CheckRect.Right, CheckRect.Bottom])));

    case NodeKind of
      nkRadio: Painter.DrawRadio(Canvas, CheckRect, DataProvider.NodeState[ANode] = csChecked, State);
      nkCheck: Painter.DrawCheck(Canvas, CheckRect, TCheckBoxState(DataProvider.NodeState[ANode]), State);
    end;
  end;

  if ANode = FocusedNode then
    Style := Styles.DataItemSelected
  else
    Style := Styles.DataItem;

  Text := StringToControl(DataProvider.GetNodeText(ANode));
  if (ANode = FocusedNode) and (FSearchString <> '') then
    SelectionText := StringToControl(FSearchString)
  else
    SelectionText := '';

  TreeRect := TextRect;
  R := TextRect;
  R.Right := R.Left + 4;
  inc(R.Right, Canvas.TextWidth(Text));

  IntersectRect(R, R, TextRect);
  Painter.DrawStyledText(Canvas, R, Text, TextFlags, Style, [], 2, 1, Length(SelectionText));
  if ANode = FocusedNode then
    Painter.DrawFocusRect(Canvas, R);

  if R.Right < TreeRect.Right then
  begin
    TreeRect.Left := R.Right;
    Painter.DrawStyledBackground(Canvas, TreeRect, Styles.DataItem)
  end;
end;

function TfcxCustomNodePopup.GetDragItem: TObject;
begin
  Result := DataProvider.GetDragItem(FDragNode);
end;

function TfcxCustomNodePopup.GetLastVisibleRow: Integer;
begin
  Result := Min(FirstVisibleRow + GetVisibleRowCount(False) - 1, GetRowCount - 1);
end;

function TfcxCustomNodePopup.GetNodeAt(X, Y: Integer): Pointer;
var
  Info: TfcxNodeInfo;
begin
  Info := FNodeCache.FindByPosition(Point(X, Y));
  if Assigned(Info) then
    Result := Info.Node
  else
    Result := nil;
end;

function TfcxCustomNodePopup.GetNodeRect(ANode: Pointer): TRect;
var
  AIndex: Integer;
begin
  AIndex := DataProvider.GetVisibleIndex(ANode);
  if (AIndex >= FirstVisibleRow) and (AIndex <= LastVisibleRow) then
    Result := Rect(0, (AIndex - FirstVisibleRow) * GetRowHeight, ClientWidth, (AIndex - FirstVisibleRow + 1) * GetRowHeight)
  else
    Result := Rect(-1, -1, -1, -1);
end;

function TfcxCustomNodePopup.GetRowCount: Integer;
begin
  if Assigned(DataProvider) then
    Result := DataProvider.GetVisibleNodeCount
  else
    Result := 0;
end;

function TfcxCustomNodePopup.GetRowHeight: Integer;
begin
  Result := Canvas.TextHeight('Wj') + 4;
end;

function TfcxCustomNodePopup.GetRowRect(AIndex: Integer): TRect;
begin
  Result := Rect(0, AIndex * GetRowHeight, ClientWidth, (AIndex + 1) * GetRowHeight);
  if pwoVertScroll in Options then
    dec(Result.Right, ScrollBar.Width);
  IntersectRect(Result, Result, GetClientRect);
end;

function TfcxCustomNodePopup.GetTreeRect(R: TRect): TRect;
begin
  Result := Rect(R.Left, R.Top, R.Left + (R.Bottom - R.Top) + (R.Bottom - R.Top) shr 2, R.Bottom);
end;

function TfcxCustomNodePopup.GetVisibleRowCount(FullyVisible: Boolean): Integer;
begin
  Result := ClientHeight div GetRowHeight;
  if not FullyVisible then
    Inc(Result, Ord((ClientHeight mod GetRowHeight) > 0));
end;

procedure TfcxCustomNodePopup.InitDrag;
begin
  // TCustomForm can't start drag because of exception in Controls.pas
  // create a dummy control for this
  with TfcxDragControl.Create(Self) do
    BeginDrag(True);
end;

procedure TfcxCustomNodePopup.Invalidate;
begin
  if Assigned(FNodeCache) then
    FNodeCache.Valid := False;
  inherited;
end;

procedure TfcxCustomNodePopup.InvalidateNode(ANode: Pointer);
var
  R: TRect;
begin
  R := GetNodeRect(ANode);
  // OutputDebugString(PChar(Format('invalidate: %d node rect %d.%d - %d.%d', [PtrInt(ANode), R.Left, R.Top, R.Right, R.Bottom])));
  if R.Bottom <> -1 then
    InvalidateClientRect(R, False);
end;

procedure TfcxCustomNodePopup.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewNode: Pointer;
begin
  inherited;
  if Key = 0 then
    Exit;
  NewNode := FocusedNode;
  if ssCtrl in Shift then
    case Key of
      VK_HOME: NewNode := DataProvider.GetFirstNode;
      VK_END: NewNode := DataProvider.GetVisibleByIndex(GetRowCount - 1);
      VK_SPACE: if DataProvider.GetNodeKind(FocusedNode) in [nkCheck, nkRadio] then ClickNodeCheck(FocusedNode, True);
    end
  else
  begin
    case Key of
      VK_UP:
        NewNode := DataProvider.GetVisibleByIndex(Max(0, DataProvider.GetVisibleIndex(FocusedNode) - 1));
      VK_DOWN:
        NewNode := DataProvider.GetVisibleByIndex(Min(GetRowCount - 1, DataProvider.GetVisibleIndex(FocusedNode) + 1));
      VK_LEFT:
        DataProvider.NodeExpanded[FocusedNode] := False;
      VK_RIGHT:
        DataProvider.NodeExpanded[FocusedNode] := True;
      VK_NEXT:
        NewNode := DataProvider.GetVisibleByIndex(Min(GetRowCount - 1, DataProvider.GetVisibleIndex(FocusedNode) + DropDownCount - 1));
      VK_PRIOR:
        NewNode := DataProvider.GetVisibleByIndex(Max(0, DataProvider.GetVisibleIndex(FocusedNode) - DropDownCount + 1));
    end;
  end;
  if FocusedNode <> NewNode then
  begin
    IncSearch('');
    FocusedNode := NewNode;
  end;
end;

procedure TfcxCustomNodePopup.PopupAt(P: TPoint);
begin
  DataProvider.BeforePopup;
  inherited PopupAt(P);
  UpdateNeedScroll;
  FAllowResizeScrollChange := True;
end;

procedure TfcxCustomNodePopup.PreparePopupMenuFor(ANode: Pointer);
begin
  //
end;

procedure TfcxCustomNodePopup.Resize;
begin
  inherited;
  if FAllowResizeScrollChange then
    UpdateNeedScroll;
end;

procedure TfcxCustomNodePopup.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  FirstVisibleRow := ScrollPos;
end;

procedure TfcxCustomNodePopup.SetActiveNode(const Value: Pointer);
begin
  // OutputDebugString(PChar('SetActiveNode: ' + IntToStr(PtrInt(Value))));
  if FActiveNode <> Value then
  begin
    InvalidateNode(FActiveNode);
    FActiveNode := Value;
    if FActiveNode = nil then
      FActiveClientPart := fppNone;
    InvalidateNode(Value);
  end;
end;

procedure TfcxCustomNodePopup.SetActiveClientPart(const Value: TfcxNodePopupActivePart);
begin
  if FActiveClientPart <> Value then
  begin
    FActiveClientPart := Value;
    InvalidateNode(ActiveNode);
  end;
end;

procedure TfcxCustomNodePopup.SetDataProvider(const Value: TfcxCustomNodeProvider);
begin
  if FDataProvider <> Value then
  begin
    FDataProvider := Value;
    FDataProvider.OnExpandChange := DoExpandChange;
    FDataProvider.OnStateChange := DoStateChange;
    invalidate;
  end;
end;

procedure TfcxCustomNodePopup.SetDropDownCount(const Value: Integer);
begin
  if FDropDownCount <> Value then
  begin
    FDropDownCount := Value;
    UpdateConstraints;
  end;
end;

procedure TfcxCustomNodePopup.SetFirstVisibleRow(const Value: Integer);
begin
  if FFirstVisibleRow <> Value then
  begin
    FFirstVisibleRow := Value;
    SetScrollPos(ScrollBar, Value);
    FNodeCache.Valid := False;
    InvalidateClientRect(GetClientRect, False);
  end;
end;

procedure TfcxCustomNodePopup.SetFocusedNode(const Value: Pointer);
begin
  if FFocusedNode <> Value then
  begin
    InvalidateNode(FFocusedNode);
    FFocusedNode := Value;
    InvalidateNode(Value);
    if DataProvider.GetVisibleIndex(FocusedNode) < FirstVisibleRow then
      FirstVisibleRow := DataProvider.GetVisibleIndex(FocusedNode)
    else
    if DataProvider.GetVisibleIndex(FocusedNode) > FirstVisibleRow + GetVisibleRowCount(True) - 1 then
      FirstVisibleRow := DataProvider.GetVisibleIndex(FocusedNode) - GetVisibleRowCount(True) + 1
  end;
end;

procedure TfcxCustomNodePopup.UpdateActiveIndex(X, Y: Integer);
var
  R, TR: TRect;
begin
  FLastCursorPos := Point(X, Y);
  ActiveNode := GetNodeAt(X, Y);
  if Assigned(ActiveNode) then
  begin
    R := GetRowRect(DataProvider.GetVisibleIndex(ActiveNode) - FirstVisibleRow);
    if DataProvider.IsTreeLike then
    begin
      Inc(R.Left, DataProvider.GetNodeLevel(ActiveNode) * (R.Bottom - R.Top));
      TR := GetTreeRect(R);
      if PtInRect(TR, FLastCursorPos) then
      begin
        ActiveClientPart := fppTreeButton;
        Exit;
      end;
      R.Left := TR.Right;
    end;
    if (DataProvider.GetNodeKind(ActiveNode) in [nkCheck, nkRadio]) and PtInRect(Painter.GetCheckRect(R), FLastCursorPos) then
      ActiveClientPart := fppCheck
    else
      ActiveClientPart := fppItem
  end
  else
    ActiveClientPart := fppNone;
end;

procedure TfcxCustomNodePopup.UpdateConstraints;
begin
  Constraints.MinHeight := GetRowHeight * DropDownCount + (Height - ClientHeight);
  if Height < Constraints.MinHeight then
    Height := Constraints.MinHeight;
  Constraints.MinWidth := Max(GetFooterMinWidth, 180);
  if Width < Constraints.MinWidth then
    Width := Constraints.MinWidth;
end;

procedure TfcxCustomNodePopup.UpdateNeedScroll;
var
  Max: Integer;
begin
  Max := GetRowCount - GetVisibleRowCount;
  if Max <= 0 then
    Options := Options - [pwoVertScroll]
  else
  begin
    Options := Options + [pwoVertScroll];
    SetScrollParams(ScrollBar, FirstVisibleRow, 0, Max, 1, GetVisibleRowCount);
  end;
end;

procedure TfcxCustomNodePopup.ClickNodeCheck(ANode: Pointer; ForceSingleCheck: Boolean);
begin
  if DataProvider.GetNodeKind(ANode) = nkRadio then
    DataProvider.NodeState[ANode] := csChecked
  else
  if ForceSingleCheck then
  begin
    DataProvider.SetSingleCheck(ANode);
    InvalidateClientRect(ClientRect);
  end
  else
  begin
    if DataProvider.NodeState[ANode] = csChecked then
      DataProvider.NodeState[ANode] := csUnchecked
    else
      DataProvider.NodeState[ANode] := csChecked;
  end;
end;

procedure TfcxCustomNodePopup.DoStateChange(Sender: TObject; ANode: Pointer);
begin
  InvalidateNode(ANode);
end;

procedure TfcxCustomNodePopup.KeyPress(var Key: Char);
begin
  inherited;
  case Key of
    #8: IncSearch(Copy(FSearchString, 1, Length(FSearchString) - 1));
    #32: if not IncSearch(FSearchString + Key) and
            (DataProvider.GetNodeKind(FocusedNode) in [nkCheck, nkRadio]) then
           ClickNodeCheck(FocusedNode);
    #33..#255: IncSearch(FSearchString + Key);
  end;
end;

function TfcxCustomNodePopup.IncSearch(const NewSearchString: String): Boolean;
var
  ParentNode, NewNode: PfcxTreeNode;
begin
  if NewSearchString = '' then
  begin
    FSearchString := NewSearchString;
    if Assigned(FocusedNode) then
      InvalidateNode(FocusedNode);
    Result := False;
    Exit;
  end;
  NewNode := DataProvider.SearchNode(NewSearchString);
  Result := Assigned(NewNode);
  if Result then
  begin
    FSearchString := NewSearchString;
    if NewNode <> FocusedNode then
    begin
      ParentNode := NewNode^.Parent;
      while Assigned(ParentNode) do
      begin
        DataProvider.NodeExpanded[ParentNode] := True;
        ParentNode := ParentNode^.Parent;
      end;
      FocusedNode := NewNode;
    end
    else
      InvalidateNode(NewNode);
  end;
end;

procedure TfcxCustomNodePopup.ClientHintInfo(X, Y: Integer;
  out HintStr: String; out HintRect: TRect);
var
  Node: Pointer;
begin
  Node := GetNodeAt(X, Y);
  if Assigned(Node) then
  begin
    HintStr := StringToControl(DataProvider.GetNodeText(Node));
    HintRect := GetNodeRect(Node); 
  end
  else
    inherited;
end;

end.
