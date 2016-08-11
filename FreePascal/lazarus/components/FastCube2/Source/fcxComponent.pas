{*******************************************************}
{                                                       }
{              FastCube 2 Components unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxComponent;
{$INCLUDE fcx.inc}

interface
uses
{$IFDEF FPC}
  LCLType, LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, fcxTypes, fcxAlerts;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils, FMX.fcxTypes, FMX.fcxAlerts;
{$ENDIF FMX}

type

  TfcxListnerProc = procedure(AComponent: TComponent) of object;
  TfcxChanged = procedure(Sender: TObject; AChangeAlert: TfcxChangeAlert) of Object;

  TfcxScriptStringList = class(TStringList)
  private
    FIsChanged: boolean;
    FChangedTick: Cardinal;
    procedure SetIsChanged(const Value: boolean);
  protected
    procedure Changed; override;
  public
    constructor Create;
    property IsChanged: boolean read FIsChanged write SetIsChanged;
    property ChangedTick: Cardinal read FChangedTick;
  end;
  
  TfcxListnersManager = class
  private
    function GetCount: integer;
    function GetListner(AIndex: Integer): TComponent;
    function GetListnerByClass(AIndex: Integer; AClass: TClass): TComponent;
    function GetListnerByClassCount(AClass: TClass): Integer;
    function GetListnerByInterface(AIndex: Integer; const IID: TGUID): TComponent;
    function GetListnerByInterfaceCount(const IID: TGUID): Integer;
  protected
    FOwner: TComponent;
    FListners: TList;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
// to add new listner
    procedure AddListner(AComponent : TComponent); virtual; abstract;
// to remove listner
    procedure RemoveListner(AComponent : TComponent); virtual; abstract;
// to process all listners
    procedure Listners_Proc(AListnerProc: TfcxListnerProc);
// to send message to all listners
    procedure SendAction(AfcAction: TfcxAction);
// count of listners
    property Count: integer read GetCount;
// listner by index
    property Listner[AIndex: Integer]: TComponent read GetListner; default;
// listner by index and class
    property ListnerByClass[AIndex: Integer; AClass: TClass]: TComponent read GetListnerByClass;
    // count of listeners with class
    property ListnerByClassCount[AClass: TClass]: Integer read GetListnerByClassCount;
// listner by index and interface
    property ListnerByInterface[AIndex: Integer; const IID: TGUID]: TComponent read GetListnerByInterface;
    // count of listeners with interface
    property ListnerByInterfaceCount[const IID: TGUID]: Integer read GetListnerByInterfaceCount;
  end;

  TfcxListnersManagerComponent = class(TfcxListnersManager)
  public
    procedure AddListner(AComponent: TComponent); override;
    procedure RemoveListner(AComponent: TComponent); override;
  end;

  TfcxComponent = class(TComponent)
  private
    FListnersManager: TfcxListnersManager;
    FOnChanged: TfcxChanged;
    FOnStartChange: TNotifyEvent;
    FOnStopChange: TNotifyEvent;
    FScriptLanguage: String;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
// Count of changes in Cube
    FChangeCount: Integer;
// Lock process of changes when >0
    FChangeSemaphore: integer;
// run after changed
    procedure DoChange(AChangeAlert: TfcxChangeAlert); virtual;
// run start changed
    procedure StartChange; virtual;
// run when new listner added
    procedure ListnerAdded(AComponent : TComponent); virtual;
// run when listner removed
    procedure ListnerRemoved(AComponent : TComponent); virtual;
// Common script (from cube)
    function GetCommonScript: TStrings; virtual;
    function GetCommonScriptLanguage: String; virtual;
    function GetScript: TStrings; virtual;
    function GetScriptLanguage: String; virtual;
    procedure SetCommonScript(const Value: TStrings); virtual;
    procedure SetCommonScriptLanguage(const Value: String); virtual;
    procedure SetScript(const Value: TStrings); virtual;
    procedure SetScriptLanguage(const Value: String); virtual;
  public
    procedure SetDefaults; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
// check listner
    function AllowAddListner(AComponent : TComponent): boolean; virtual;
// listners Manager
    property ListnersManager: TfcxListnersManager read FListnersManager;
// Init Interpreter (add variables, ...)
    procedure InitInterpreter; virtual;
// To check Common script Changing
    function GetCommonScriptChangedTick: Cardinal; virtual;

    property CommonScript: TStrings read GetCommonScript write SetCommonScript;
    property CommonScriptLanguage: String read GetCommonScriptLanguage write SetCommonScriptLanguage;
    property Script: TStrings read GetScript write SetScript;
    property ScriptLanguage: String read GetScriptLanguage write SetScriptLanguage;
  published
    property Version: String read GetVersion write SetVersion;
// events of changes
    property OnStartChange: TNotifyEvent read FOnStartChange write FOnStartChange;
    property OnStopChange: TNotifyEvent read FOnStopChange write FOnStopChange;
    property OnChanged: TfcxChanged read FOnChanged write FOnChanged;
  end;

  TfcxAbstractSlice = class(TfcxComponent)
  public
    procedure InternalSetSelected(ASelectedMeasure, ASelectedCol, ASelectedRow, ASelectedColAdditionalTotal, ASelectedRowAdditionalTotal: Integer); virtual; abstract;
    procedure SaveToStream(ASliceStream: TStream; AStoreItems: TfcxItemsForStoreWithSlice = []); virtual; abstract;
    function LoadFromStream(ASliceStream: TStream): Boolean; virtual; abstract;
    function CubeIsEqual(ACube: TObject): boolean; virtual; abstract;
    procedure UpdateDisplayLabel(ACubeField: TObject; const Value: TfcxString); virtual; abstract;
    procedure ResetDisplayLabelOfFields; virtual; abstract;
    procedure ClearFilterManager; virtual; abstract;
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

{ TfcxListnersManager }

constructor TfcxListnersManager.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FListners := TList.Create;
end;

destructor TfcxListnersManager.Destroy;
begin
  FreeAndNil(FListners);
  inherited;
end;

procedure TfcxListnersManager.SendAction(AfcAction: TfcxAction);
var
  i: integer;
begin
  for i := 0 to FListners.Count - 1 do
    TComponent(FListners[i]).ExecuteAction(AfcAction);
end;

function TfcxListnersManager.GetCount: integer;
begin
  Result := FListners.Count;
end;

function TfcxListnersManager.GetListner(AIndex: Integer): TComponent;
begin
  Result := TComponent(FListners[AIndex])
end;

function TfcxListnersManager.GetListnerByClass(AIndex: Integer;
  AClass: TClass): TComponent;
var
  i, AIndexTemp: integer;
begin
  Result := nil;
  AIndexTemp := 0;
  for i := 0 to FListners.Count - 1 do
  begin
    if TObject(FListners[i]) is AClass then
    begin
      if AIndexTemp = AIndex then
      begin
        Result := TComponent(FListners[i]);
        exit;
      end;
      inc(AIndexTemp);
    end
  end;
end;

procedure TfcxListnersManager.Listners_Proc(AListnerProc: TfcxListnerProc);
var
  i: integer;
begin
  for i := FListners.Count - 1 downto 0 do
    AListnerProc(FListners[i]);
end;

function TfcxListnersManager.GetListnerByClassCount(AClass: TClass): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FListners.Count - 1 do
  begin
    if TObject(FListners[i]) is AClass then
      inc(Result);
  end;
end;

function TfcxListnersManager.GetListnerByInterface(AIndex: Integer;
  const IID: TGUID): TComponent;
var
  i, AIndexTemp: integer;
begin
  Result := nil;
  AIndexTemp := 0;
  for i := 0 to FListners.Count - 1 do
  begin
    if Supports(TObject(FListners[i]), IID) then
    begin
      if AIndexTemp = AIndex then
      begin
        Result := TComponent(FListners[i]);
        exit;
      end;
      inc(AIndexTemp);
    end
  end;
end;

function TfcxListnersManager.GetListnerByInterfaceCount(
  const IID: TGUID): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FListners.Count - 1 do
  begin
    if Supports(TObject(FListners[i]), IID) then
      inc(Result);
  end;
end;

{ TfcxComponent }

function TfcxComponent.AllowAddListner(AComponent: TComponent): boolean;
begin
  Result := Assigned(AComponent);
end;

constructor TfcxComponent.Create(AOwner: TComponent);
begin
  FListnersManager := TfcxListnersManagerComponent.Create(Self);
  FScriptLanguage := 'PascalScript';
  inherited;
  SetDefaults;
end;

destructor TfcxComponent.Destroy;
begin
  inherited;
  FreeAndNil(FListnersManager);
end;

procedure TfcxComponent.DoChange(AChangeAlert: TfcxChangeAlert);
var
  AAction: TfcxAction;
begin
  AAction := TfcxAction.CreateFCAction(Self, AChangeAlert);
  ListnersManager.SendAction(AAction);
  if Assigned(FOnChanged) then
    FOnChanged(Self, AChangeAlert);
  AAction.Free;
end;

function TfcxComponent.GetCommonScript: TStrings;
begin
  Result := nil;
end;

function TfcxComponent.GetCommonScriptChangedTick: Cardinal;
begin
  Result := 0;
end;

function TfcxComponent.GetCommonScriptLanguage: String;
begin
  Result := 'PascalScript';
end;

function TfcxComponent.GetScript: TStrings;
begin
  Result := nil;
end;

function TfcxComponent.GetScriptLanguage: String;
begin
  Result := FScriptLanguage;
end;

function TfcxComponent.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxComponent.InitInterpreter;
begin
//
end;

procedure TfcxComponent.ListnerAdded(AComponent: TComponent);
begin
//
end;

procedure TfcxComponent.ListnerRemoved(AComponent: TComponent);
begin
//
end;

procedure TfcxComponent.SetCommonScript(const Value: TStrings);
begin

end;

procedure TfcxComponent.SetCommonScriptLanguage(const Value: String);
begin

end;

procedure TfcxComponent.SetDefaults;
begin
//
end;

procedure TfcxComponent.SetScript(const Value: TStrings);
begin

end;

procedure TfcxComponent.SetScriptLanguage(const Value: String);
begin
  FScriptLanguage := Value;
end;

procedure TfcxComponent.SetVersion(const Value: String);
begin
//
end;

procedure TfcxComponent.StartChange;
begin
  if FChangeSemaphore > 0 then
    Exit;
  inc(FChangeCount);
  if (FChangeCount = 1) and Assigned(OnStartChange) then
    OnStartChange(Self);
end;

{ TfcxListnersManagerComponent }

procedure TfcxListnersManagerComponent.AddListner(AComponent: TComponent);
begin
  if (FListners.IndexOf(AComponent) = -1) then
    if TfcxComponent(FOwner).AllowAddListner(AComponent) then
    begin
      FOwner.FreeNotification(AComponent);
      FListners.Add(AComponent);
      TfcxComponent(FOwner).ListnerAdded(AComponent);
    end
end;

procedure TfcxListnersManagerComponent.RemoveListner(AComponent: TComponent);
var
  i: integer;
begin
  i := FListners.IndexOf(AComponent);
  if i >= 0 then
  begin
    FListners.Delete(i);
    FOwner.RemoveFreeNotification(AComponent);
    TfcxComponent(FOwner).ListnerRemoved(AComponent);
  end
end;

{ TfcxScriptStringList }

procedure TfcxScriptStringList.Changed;
begin
  inherited;
  FIsChanged := True;
  FChangedTick := fcxGetTickCount;
end;

constructor TfcxScriptStringList.Create;
begin
  FIsChanged := False;
end;

procedure TfcxScriptStringList.SetIsChanged(const Value: boolean);
begin
  FIsChanged := Value;
end;

end.
