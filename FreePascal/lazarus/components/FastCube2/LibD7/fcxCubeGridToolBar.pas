{*******************************************************}
{                                                       }
{        FastCube 2.0 cube grid toolbar unit            }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxCubeGridToolBar;

interface
{$INCLUDE fcx.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, ImgList,
  fcxTypes, fcxRes, fcxAlerts, fcxCube, fcxCubeGrid, fcxCustomToolBar,
  fcxGraphicRes,  fcxCustomExport;
//FMX uses
{$ELSE FMX}
interface
{$INCLUDE fcx.inc}

uses
  // RTL
  System.SysUtils, System.Classes,
  // FMX
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Menus,
  // FC FMX
  FMX.fcxTypes, FMX.fcxRes, FMX.fcxAlerts, FMX.fcxCube, FMX.fcxCubeGrid,
  FMX.fcxCustomToolBar, FMX.fcxCustomExport,
  FMX.fcxGraphicUtils, FMX.fcxGraphicRes;
{$ENDIF FMX}

type

  TfcxCubeGridToolbarItem = class(TfcxToolbarItem)
  protected
    class function GetEnabled(Sender: TfcxCustomToolbar): Boolean; override;
  end;

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxCubeGridToolbar = class(TfcxCustomToolbar)
  private
    FCubeGrid: TfcxCubeGrid;
    procedure SetCubeGrid(const Value: TfcxCubeGrid);
  protected
    function GetItems: TfcxToolbarItems; override;
    function GetImageList: TfcxImageList; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction) : Boolean; override;
  published
    property CubeGrid: TfcxCubeGrid read FCubeGrid write SetCubeGrid;
  end;

  // standard buttons

  TfcxExportToCubeToolbarItem = class(TfcxCubeGridToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
    class procedure MenuPopup(Sender: TObject);
  protected
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  // end of standard items

  procedure fcxRegisterCubeGridToolbarItem(AItem: TfcxToolbarItemClass);
  procedure fcxUnRegisterCubeGridToolbarItem(AItem: TfcxToolbarItemClass);

implementation

var
  CubeGridToolbarItems: TfcxToolbarItems = nil;

procedure fcxRegisterCubeGridToolbarItem(AItem: TfcxToolbarItemClass);
begin
  if CubeGridToolbarItems.IndexOf(AItem) = -1 then
    CubeGridToolbarItems.Add(AItem);
end;

procedure fcxUnRegisterCubeGridToolbarItem(AItem: TfcxToolbarItemClass);
var
  AIndex: Integer;
begin
  AIndex := CubeGridToolbarItems.IndexOf(AItem);
  if AIndex <> -1 then
    CubeGridToolbarItems.Delete(AIndex);
end;


{ TfcxCubeGridToolbar }

procedure TfcxCubeGridToolbar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FCubeGrid) then
    CubeGrid := nil;
  inherited;
end;

procedure TfcxCubeGridToolbar.SetCubeGrid(const Value: TfcxCubeGrid);
begin
  if FCubeGrid <> Value then
  begin
    if Assigned(FCubeGrid) then
    begin
      FCubeGrid.ListnersManager.RemoveListner(Self);
      FCubeGrid.RemoveFreeNotification(Self);
    end;
    FCubeGrid := Value;
    if Assigned(FCubeGrid) then
    begin
      FCubeGrid.FreeNotification(Self);
      FCubeGrid.ListnersManager.AddListner(Self);
    end;

    UpdateState;
  end;
end;

function TfcxCubeGridToolbar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if Action.Owner is TfcxCubeGrid then
      UpdateState;
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

destructor TfcxCubeGridToolbar.Destroy;
begin
  CubeGrid := nil;
  inherited;
end;

function TfcxCubeGridToolbar.GetItems: TfcxToolbarItems;
begin
  Result := CubeGridToolbarItems;
end;

function TfcxCubeGridToolbar.GetImageList: TfcxImageList;
begin
  Result := fcxGraphicResources.ToolImages;
end;

{ TfcxExportToSliceToolbarItem }

class procedure TfcxExportToCubeToolbarItem.DoClick(Sender: TfcxToolButton);
begin
// not need in FMX !
{$IFNDEF FMX}
  if fcxExportFilters.Count = 1 then
    TfcxCubeGridToolbar(Sender.Owner).CubeGrid.DoExport(fcxExportFilters[0].Filter)
  else
{$ENDIF FMX}
    inherited;
end;

class function TfcxExportToCubeToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
begin
  Result := inherited GetDropDownMenu(Owner);
{$IFNDEF FMX}
  Result.Images := fcxGraphicResources.ToolImages;
{$ENDIF FMX}
  SetMenuPopupProc(Result, MenuPopup);
end;

class function TfcxExportToCubeToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sExportTo');
end;

class function TfcxExportToCubeToolbarItem.GetImageIndex: Integer;
begin
  Result := 3;
end;

class function TfcxExportToCubeToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioStartGroup, ioHasDropDown];
end;

class procedure TfcxExportToCubeToolbarItem.MenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
  UseDefault: boolean;
  Toolbar: TfcxCubeGridToolbar;
begin
  Toolbar := TfcxCubeGridToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar);
  if Assigned(Toolbar.OnMenuItemClick) then
  begin
    UseDefault := False;
    Toolbar.OnMenuItemClick(Toolbar, TfcxPopupMenu(MenuItem.Owner).ToolButton.Item, TfcxPopupMenuProcessor.ItemIndex(MenuItem), UseDefault);
    if not UseDefault then
      Exit;
  end;
  Toolbar.CubeGrid.DoExport(fcxExportFilters[MenuItem.Tag].Filter);
end;

class procedure TfcxExportToCubeToolbarItem.MenuPopup(Sender: TObject);
var
  PopupMenu: TfcxPopupMenu absolute Sender;
  i: integer;
  Item: TMenuItem;
begin
  // create items here because export creation may be postponed
  TfcxPopupMenuProcessor.Clear(PopupMenu);

  for i := 0 to fcxExportFilters.Count - 1 do
  begin
    Item := TMenuItem.Create(PopupMenu);
    with Item do
    begin
      Tag := i;
      TfcxPopupMenuProcessor.SetItemCaption(Item, fcxExportFilters[i].Filter.GetDescription + '...');
      OnClick := MenuItemClick;
    end;
    TfcxPopupMenuProcessor.AddItem(PopupMenu, Item);
  end;
end;

{ TfcxCubeGridToolbarItem }

class function TfcxCubeGridToolbarItem.GetEnabled(Sender: TfcxCustomToolbar): Boolean;
var
  ToolBar: TfcxCubeGridToolbar absolute Sender;
begin
 Result := Assigned(ToolBar.CubeGrid) and
   Assigned(ToolBar.CubeGrid.Cube);
end;

initialization
{$IFDEF DELPHI_16UP}
  {$IFDEF FMX}
{
  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(TfcxCubeGridToolBar, TFmxObject);
}
  {$ELSE}
{
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TfcxCubeGridToolBar, TControl);
}
  {$ENDIF}
{$ENDIF}
  CubeGridToolbarItems := TfcxToolbarItems.Create;
  fcxRegisterCubeGridToolbarItem(TfcxExportToCubeToolbarItem);

finalization
  CubeGridToolbarItems.Free;
end.
