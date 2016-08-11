{*******************************************************}
{                                                       }
{               FastCube 2 Controls unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxControl;
{$INCLUDE fcx.inc}

interface
uses
{$IFDEF FPC}
  LCLType, LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, Graphics, Controls, SysUtils, fcxTypes, fcxAlerts, fcxComponent;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils, 
  FMX.Controls, FMX.fcxTypes, FMX.fcxAlerts, FMX.fcxComponent;
{$ENDIF FMX}

type
  TfcxListnersManagerControl = class(TfcxListnersManager)
  public
    procedure AddListner(AComponent: TComponent); override;
    procedure RemoveListner(AComponent: TComponent); override;
  end;

{$IFNDEF FMX}
  TfcxCustomDragItem = class(TDragControlObject)
  private
    FDragImages: TDragImageList;
    FInDrag: Boolean;
    FShowDragImage: Boolean;
    FEndDragNotifiers: TList;
  protected
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure EndDragNotification(AControl: TControl);

    function GetItem: TObject; virtual;
    procedure InitDrag(ItemBitmap, ItemMask: TBitmap);
    property InDrag: Boolean read FInDrag;
  end;
{$ENDIF FMX}

{$IFNDEF FMX}
  TfcxCustomControl = class(TCustomControl)
{$ELSE FMX}
  TfcxCustomControl = class(TStyledControl)
{$ENDIF FMX}
  private
    FListnersManager: TfcxListnersManager;
    FOnChanged: TfcxChanged;
    FOnStartChange: TNotifyEvent;
    FOnStopChange: TNotifyEvent;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
// Count of changes in Cube
    FChangeCount: Integer;
// Lock process of changes when >0
    FChangeSemaphore: Integer;
// run after changed
    procedure DoChange(AChangeAlert: TfcxChangeAlert); virtual;
// run start changed
    procedure StartChange; virtual;
// run when new listner added
    procedure ListnerAdded(AComponent : TComponent); virtual;
// run when listner removed
    procedure ListnerRemoved(AComponent : TComponent); virtual;
  public
    procedure SetDefaults; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
// check listner
    function AllowAddListner(AComponent : TComponent): boolean; virtual;
// listners Manager
    property ListnersManager: TfcxListnersManager read FListnersManager;
  published
    property Version: String read GetVersion write SetVersion;
// events of changes
    property OnStartChange: TNotifyEvent read FOnStartChange write FOnStartChange;
    property OnStopChange: TNotifyEvent read FOnStopChange write FOnStopChange;
    property OnChanged: TfcxChanged read FOnChanged write FOnChanged;
  end;

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxRes;
//FMX uses
{$ELSE FMX}
uses
  FMX.fcxRes;
{$ENDIF FMX}

type
  TControlAccess = class(TControl);

{ TfcxCustomControl }

function TfcxCustomControl.AllowAddListner(AComponent: TComponent): boolean;
begin
  Result := AComponent <> nil;
end;

constructor TfcxCustomControl.Create(AOwner: TComponent);
begin
  FListnersManager := TfcxListnersManagerControl.Create(Self);
  FChangeCount := 0;
  FChangeSemaphore := 0;
  inherited;
  SetDefaults;
end;

destructor TfcxCustomControl.Destroy;
begin
  inherited;
  FListnersManager.Free;
end;

procedure TfcxCustomControl.DoChange(AChangeAlert: TfcxChangeAlert);
var
  AAction: TfcxAction;
begin
  AAction := TfcxAction.CreateFCAction(Self, AChangeAlert);
  ListnersManager.SendAction(AAction);
  if Assigned(FOnChanged) then
    FOnChanged(Self, AChangeAlert);
  AAction.Free;
end;

function TfcxCustomControl.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxCustomControl.ListnerAdded(AComponent: TComponent);
begin
//
end;

procedure TfcxCustomControl.ListnerRemoved(AComponent: TComponent);
begin
//
end;

procedure TfcxCustomControl.SetDefaults;
begin
//
end;

procedure TfcxCustomControl.SetVersion(const Value: String);
begin
//
end;

procedure TfcxCustomControl.StartChange;
begin
  if FChangeSemaphore > 0 then
    Exit;
  inc(FChangeCount);
  if (FChangeCount = 1) and Assigned(OnStartChange) then
    OnStartChange(Self);
end;

{ TfcxListnersManagerControl }

procedure TfcxListnersManagerControl.AddListner(AComponent: TComponent);
begin
  if (FListners.IndexOf(AComponent) = -1) then
    if TfcxCustomControl(FOwner).AllowAddListner(AComponent) then
    begin
      FOwner.FreeNotification(AComponent);
      FListners.Add(AComponent);
      TfcxCustomControl(FOwner).ListnerAdded(AComponent);
    end
end;

procedure TfcxListnersManagerControl.RemoveListner(AComponent: TComponent);
var
  i: integer;
begin
  i := FListners.IndexOf(AComponent);
  if i >= 0 then
  begin
    FListners.Delete(i);
    FOwner.RemoveFreeNotification(AComponent);
    TfcxCustomControl(FOwner).ListnerRemoved(AComponent);
  end
end;

{$IFNDEF FMX}
{ TfcxCustomDragItem }

constructor TfcxCustomDragItem.Create(AControl: TControl);
begin
  inherited;
  FInDrag := False;
  FDragImages := TDragImageList.Create(AControl);
  FShowDragImage := False;
  FEndDragNotifiers := TList.Create;
{$IFDEF Delphi_10UP}
  AlwaysShowDragImages := True;
{$ELSE}
  {$IFDEF FPC}
  AlwaysShowDragImages := True;
  {$ENDIF}
{$ENDIF}
end;

destructor TfcxCustomDragItem.Destroy;
begin
  FDragImages.Free;
  FEndDragNotifiers.Free;
  inherited;
end;

procedure TfcxCustomDragItem.EndDrag(Target: TObject; X, Y: Integer);
var
  I: Integer;
begin
  FInDrag := False;
  for I := 0 to FEndDragNotifiers.Count - 1 do
    TControlAccess(FEndDragNotifiers[I]).DoEndDrag(Target, X, Y);
  inherited;
end;

procedure TfcxCustomDragItem.EndDragNotification(AControl: TControl);
begin
  if FEndDragNotifiers.IndexOf(AControl) = -1 then
    FEndDragNotifiers.Add(AControl);
end;

function TfcxCustomDragItem.GetDragImages: TDragImageList;
begin
  Result := FDragImages
end;

function TfcxCustomDragItem.GetItem: TObject;
begin
  Result := nil;
end;

procedure TfcxCustomDragItem.InitDrag(ItemBitmap, ItemMask: TBitmap);
begin
  if not FInDrag then
  begin
    FDragImages.Clear;
    if Assigned(ItemBitmap) then
    begin
      FDragImages.Width := ItemBitmap.Width;
      FDragImages.Height := ItemBitmap.Height;
      FDragImages.Add(ItemBitmap, ItemMask);
    end;  
    FInDrag := True;
  end;  
end;
{$ENDIF FMX}

end.
