{*******************************************************}
{                                                       }
{        FastCube 2.0 slice grid toolbar unit           }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxSliceGridToolbar;

interface
{$INCLUDE fcx.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls,
  ImgList, fcxTypes, fcxSlice, fcxSliceGrid, fcxAlerts, fcxInfo,
  fcxCustomToolBar, fcxMeasureEditor, fcxRes, fcxGraphicRes, fcxCustomExport;

//FMX uses
{$ELSE FMX}
interface
{$INCLUDE fcx.inc}

uses
  // RTL
  System.SysUtils, System.Classes,  System.UITypes,
  // FMX
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Menus,
  // FC FMX
  FMX.fcxTypes, FMX.fcxSlice, FMX.fcxSliceGrid, FMX.fcxAlerts, FMX.fcxInfo,
  FMX.fcxCustomToolBar, FMX.fcxMeasureEditor,
  FMX.Dialogs, FMX.fcxRes, FMX.fcxGraphicRes, FMX.fcxCustomExport,
  FMX.fcxGraphicUtils;
{$ENDIF FMX}

type
  TfcxSliceGridToolbarItem = class(TfcxToolbarItem)
  protected
    class function GetEnabled(Sender: TfcxCustomToolbar): Boolean; override;
  end;

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxSliceGridToolbar = class(TfcxCustomToolbar)
  private
    FSliceGrid: TfcxSliceGrid;
    FDialogsDefaultPath: String;
    procedure SetSliceGrid(const Value: TfcxSliceGrid);
  protected
    function GetItems: TfcxToolbarItems; override;
    function GetImageList: TfcxImageList; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction) : Boolean; override;
  published
    property DialogsDefaultPath: String read FDialogsDefaultPath write FDialogsDefaultPath;
    property SliceGrid: TfcxSliceGrid read FSliceGrid write SetSliceGrid;
  end;

  // standard buttons

  TfcxSaveSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
  protected
    class procedure DoClick(Sender: TfcxToolButton); override;
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxOpenSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
  protected
    class procedure DoClick(Sender: TfcxToolButton); override;
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxClearSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  protected
    class function GetImageIndex: Integer; override;
    class function GetHint: String; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxExportToSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
    class procedure MenuPopup(Sender: TObject);
  protected
    class function GetEnabled(Sender: TfcxCustomToolbar): Boolean; override;
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxTransposeSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxHideRowZerosSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
    class function GetChecked(Sender: TfcxCustomToolbar): Boolean; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxHideColZerosSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
    class function GetChecked(Sender: TfcxCustomToolbar): Boolean; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxRowSortSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
    class procedure MenuPopup(Sender: TObject);
  protected
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxColSortSliceToolbarItem = class(TfcxSliceGridToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
    class procedure MenuPopup(Sender: TObject);
  protected
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxEditMeasuresSliceToolbarItems = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxDataHighlightSliceToolbarItems = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxDisplayFormatSliceToolbarItems = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxFieldListSliceToolbarItems = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxInformationSliceToolbarItems = class(TfcxSliceGridToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  // end of standard items

  procedure fcxRegisterSliceGridToolBarItem(AItem: TfcxToolbarItemClass);
  procedure fcxUnRegisterSliceGridToolBarItem(AItem: TfcxToolbarItemClass);

implementation

var
  SliceGridToolbarItems: TfcxToolbarItems = nil;

procedure fcxRegisterSliceGridToolBarItem(AItem: TfcxToolbarItemClass);
begin
  if SliceGridToolbarItems.IndexOf(AItem) = -1 then
    SliceGridToolbarItems.Add(AItem);
end;

procedure fcxUnRegisterSliceGridToolBarItem(AItem: TfcxToolbarItemClass);
var
  AIndex: Integer;
begin
  AIndex := SliceGridToolbarItems.IndexOf(AItem);
  if AIndex <> -1 then
    SliceGridToolbarItems.Delete(AIndex);
end;


{ TfcxSliceGridToolbar }

procedure TfcxSliceGridToolbar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSliceGrid) then
    SliceGrid := nil;
  inherited;
end;

procedure TfcxSliceGridToolbar.SetSliceGrid(const Value: TfcxSliceGrid);
begin
  if FSliceGrid <> Value then
  begin
    if Assigned(FSliceGrid) then
    begin
      FSliceGrid.ListnersManager.RemoveListner(Self);
      FSliceGrid.RemoveFreeNotification(Self);
    end;
    FSliceGrid := Value;
    if Assigned(FSliceGrid) then
    begin
      FSliceGrid.FreeNotification(Self);
      FSliceGrid.ListnersManager.AddListner(Self);
    end;

    UpdateState;
  end;
end;

function TfcxSliceGridToolbar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if Action.Owner is TfcxSliceGrid then
      UpdateState;
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

destructor TfcxSliceGridToolbar.Destroy;
begin
  SliceGrid := nil;
  inherited;
end;

function TfcxSliceGridToolbar.GetItems: TfcxToolbarItems;
begin
  Result := SliceGridToolbarItems;
end;

function TfcxSliceGridToolbar.GetImageList: TfcxImageList;
begin
  Result := fcxGraphicResources.ToolImages;
end;

{ TfcxOpenCubeToolbarItem }

class function TfcxOpenSliceToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
const
  LoadSubMenuCaptions: array[0..3] of String  = ('sCubeLoad', 'sCubeLoadAdditional', '-', 'sSchemaLoad');
var
  i: integer;
  Item: TMenuItem;
begin
  Result := inherited GetDropDownMenu(Owner);
{$IFNDEF FMX}
  Result.Images := fcxGraphicResources.ToolImages;
{$ENDIF FMX}
  for i := Low(LoadSubMenuCaptions) to High(LoadSubMenuCaptions) do
  begin
    Item := TMenuItem.Create(Result);
    with Item do
    begin
      TfcxPopupMenuProcessor.SetItemCaption(Item, fcxResources.Get(LoadSubMenuCaptions[i]));
      OnClick := MenuItemClick;
    end;
    TfcxPopupMenuProcessor.AddItem(Result, Item);
  end;
end;

class function TfcxOpenSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sLoad');
end;

class function TfcxOpenSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 1;
end;

class function TfcxOpenSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioHasDropDown];
end;

class procedure TfcxOpenSliceToolbarItem.MenuItemClick(Sender: TObject);
const
  DefaultExts: array[0..3] of String = ('mdc', 'mdc', '', 'mds');
  Filters: array[0..3] of String = ('sCubeFilter', 'sCubeFilter', '', 'sSchemaFilter');
var
  MenuItem: TMenuItem absolute Sender;
  UseDefault: boolean;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  if Assigned(Toolbar.OnMenuItemClick) then
  begin
    UseDefault := False;
    Toolbar.OnMenuItemClick(Toolbar, TfcxPopupMenu(MenuItem.Owner).ToolButton.Item, TfcxPopupMenuProcessor.ItemIndex(MenuItem), UseDefault);
    if not UseDefault then
      Exit;
  end;
  with TOpenDialog.Create(Application) do
  begin
    DefaultExt := DefaultExts[TfcxPopupMenuProcessor.ItemIndex(MenuItem)];
    Filter := fcxResources.Get(Filters[TfcxPopupMenuProcessor.ItemIndex(MenuItem)]);
    if Toolbar.DialogsDefaultPath <> '' then
      InitialDir := Toolbar.DialogsDefaultPath
    else
      InitialDir := ExtractFileDir(fcxExeName);
    if Execute then
    begin
      case TfcxPopupMenuProcessor.ItemIndex(MenuItem) of
        0: Toolbar.SliceGrid.Slice.Cube.LoadFromFile(FileName);
        1: Toolbar.SliceGrid.Slice.Cube.AppendFromFile(FileName);
        3: Toolbar.SliceGrid.Slice.LoadFromFile(FileName);
      end;
      Toolbar.DialogsDefaultPath := ExtractFileDir(FileName);
    end;
    Free;
  end;
end;

class procedure TfcxOpenSliceToolbarItem.DoClick(Sender: TfcxToolButton);
// not need in FMX !
{$IFNDEF FMX}
var
  Ext: String;
begin
  with TOpenDialog.Create(Application) do
  begin
    DefaultExt := '';
    Filter := fcxResources.Get('sCubeFilter') + '|' + fcxResources.Get('sSchemaFilter');
    if TfcxSliceGridToolBar(Sender.Toolbar).DialogsDefaultPath <> '' then
      InitialDir := TfcxSliceGridToolBar(Sender.Toolbar).DialogsDefaultPath
    else
      InitialDir := ExtractFileDir(fcxExeName);
    if Execute then
    begin
      Ext := ExtractFileExt(FileName);
      if Ext = '.mdc' then
        TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.Cube.LoadFromFile(FileName)
      else
      if Ext = '.mds' then
        TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.LoadFromFile(FileName);
      TfcxSliceGridToolBar(Sender.Toolbar).DialogsDefaultPath := ExtractFileDir(FileName);
    end;
    Free;
  end;
{$ELSE FMX}
begin
  inherited;
{$ENDIF FMX}
end;

{ TfcxSaveCubeToolbarItem }

class procedure TfcxSaveSliceToolbarItem.DoClick(Sender: TfcxToolButton);
begin
// not need in FMX !
{$IFNDEF FMX}
  with TSaveDialog.Create(Application) do
  begin
    Options := Options + [ofOverwritePrompt];
    DefaultExt := '';
    Filter := fcxResources.Get('sCubeFilter') + '|' + fcxResources.Get('sSchemaFilter');
    if TfcxSliceGridToolbar(Sender.Toolbar).DialogsDefaultPath <> '' then
      InitialDir := TfcxSliceGridToolbar(Sender.Toolbar).DialogsDefaultPath
    else
      InitialDir := ExtractFileDir(fcxExeName);
    if Execute then
    begin
      case FilterIndex of
        1: TfcxSliceGridToolbar(Sender.Toolbar).SliceGrid.Slice.Cube.SaveToFile(ChangeFileExt(FileName, '.mdc'));
        2: TfcxSliceGridToolbar(Sender.Toolbar).SliceGrid.Slice.SaveToFile(ChangeFileExt(FileName, '.mds'), [fcxiss_Groups, fcxiss_Filters, fcxiss_Charts]);
      end;
      TfcxSliceGridToolBar(Sender.Toolbar).DialogsDefaultPath := ExtractFileDir(FileName);
    end;
    Free;
  end;
{$ELSE FMX}
  inherited;
{$ENDIF FMX}
end;

class function TfcxSaveSliceToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
const
  SaveSubMenuCaptions: array[0..1] of String  = ('sCubeSave', 'sSchemaSave');
var
  i: integer;
  Item: TMenuItem;
begin
  Result := inherited GetDropDownMenu(Owner);
{$IFNDEF FMX}
  Result.Images := fcxGraphicResources.ToolImages;
{$ENDIF FMX}
  for i := Low(SaveSubMenuCaptions) to High(SaveSubMenuCaptions) do
  begin
    Item := TMenuItem.Create(Result);
    with Item do
    begin
      TfcxPopupMenuProcessor.SetItemCaption(Item, fcxResources.Get(SaveSubMenuCaptions[i]));
      OnClick := MenuItemClick;
    end;
    TfcxPopupMenuProcessor.AddItem(Result, Item);
  end;
end;

class function TfcxSaveSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sSave');
end;

class function TfcxSaveSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 0;
end;

class function TfcxSaveSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioHasDropDown];
end;

class procedure TfcxSaveSliceToolbarItem.MenuItemClick(Sender: TObject);
const
  DefaultExts: array[0..1] of String = ('mdc', 'mds');
  Filters: array[0..1] of String = ('sCubeFilter', 'sSchemaFilter');
var
  MenuItem: TMenuItem absolute Sender;
  UseDefault: boolean;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  if Assigned(Toolbar.OnMenuItemClick) then
  begin
    UseDefault := False;
    Toolbar.OnMenuItemClick(Toolbar, TfcxPopupMenu(MenuItem.Owner).ToolButton.Item, TfcxPopupMenuProcessor.ItemIndex(MenuItem), UseDefault);
    if not UseDefault then
      Exit;
  end;
  with TSaveDialog.Create(Application) do
  begin
    Options := Options + [{$IFDEF FMX}TOpenOption.{$ENDIF}ofOverwritePrompt];
    DefaultExt := DefaultExts[TfcxPopupMenuProcessor.ItemIndex(MenuItem)];
    Filter := fcxResources.Get(Filters[TfcxPopupMenuProcessor.ItemIndex(MenuItem)]);
    if Toolbar.DialogsDefaultPath <> '' then
      InitialDir := Toolbar.DialogsDefaultPath
    else
      InitialDir := ExtractFileDir(fcxExeName);
    if Execute then
    begin
      case TfcxPopupMenuProcessor.ItemIndex(MenuItem) of
        0: Toolbar.SliceGrid.Slice.Cube.SaveToFile(ChangeFileExt(FileName, '.mdc'));
        1: Toolbar.SliceGrid.Slice.SaveToFile(ChangeFileExt(FileName, '.mds'), [fcxiss_Groups, fcxiss_Filters, fcxiss_Charts]);
      end;
      Toolbar.DialogsDefaultPath := ExtractFileDir(FileName);
    end;
    Free;
  end;
end;

constructor TfcxSliceGridToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FDialogsDefaultPath := '';
end;

{ TfcxClearSliceToolbarItem }

class procedure TfcxClearSliceToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.Clear;
end;

class function TfcxClearSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sClearGrid');
end;

class function TfcxClearSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 2;
end;

class function TfcxClearSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioStartGroup];
end;

{ TfcxExportToSliceToolbarItem }

class procedure TfcxExportToSliceToolbarItem.DoClick(Sender: TfcxToolButton);
begin
// not need in FMX !
{$IFNDEF FMX}
  if fcxExportFilters.Count = 1 then
    TfcxSliceGridToolbar(Sender.Toolbar).SliceGrid.DoExport(fcxExportFilters[0].Filter)
  else
{$ENDIF FMX}
    inherited;
end;

class function TfcxExportToSliceToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
begin
  Result := inherited GetDropDownMenu(Owner);
{$IFNDEF FMX}
  Result.Images := fcxGraphicResources.ToolImages;
{$ENDIF FMX}
  SetMenuPopupProc(Result, MenuPopup);
end;

class function TfcxExportToSliceToolbarItem.GetEnabled(
  Sender: TfcxCustomToolbar): Boolean;
begin
  Result := inherited GetEnabled(Sender);
  if Result then
    Result := fcxExportFilters.Count > 0;
end;

class function TfcxExportToSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sExportTo');
end;

class function TfcxExportToSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 3;
end;

class function TfcxExportToSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioStartGroup, ioHasDropDown];
end;

class procedure TfcxExportToSliceToolbarItem.MenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
  UseDefault: boolean;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  if Assigned(Toolbar.OnMenuItemClick) then
  begin
    UseDefault := False;
    Toolbar.OnMenuItemClick(Toolbar, TfcxPopupMenu(MenuItem.Owner).ToolButton.Item, TfcxPopupMenuProcessor.ItemIndex(MenuItem), UseDefault);
    if not UseDefault then
      Exit;
  end;
  Toolbar.SliceGrid.DoExport(fcxExportFilters[MenuItem.Tag].Filter);
end;

class procedure TfcxExportToSliceToolbarItem.MenuPopup(Sender: TObject);
var
  PopupMenu: TPopupMenu absolute Sender;
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

{ TfcxFieldListSliceToolbarItems }

class procedure TfcxFieldListSliceToolbarItems.DoClick(Sender: TfcxToolButton);
begin
  TfcxSliceGridToolBar(Sender.ToolBar).SliceGrid.ShowFieldsEditor;
end;

class function TfcxFieldListSliceToolbarItems.GetHint: String;
begin
  Result := fcxResources.Get('sFieldList');
end;

class function TfcxFieldListSliceToolbarItems.GetImageIndex: Integer;
begin
  Result := 12;
end;

{ TfcxHideColZerosSliceToolbarItem }

class procedure TfcxHideColZerosSliceToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.HideColZeros := not TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.HideColZeros;
end;

class function TfcxHideColZerosSliceToolbarItem.GetChecked(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := TfcxSliceGridToolBar(Sender).SliceGrid.Slice.HideColZeros;
end;

class function TfcxHideColZerosSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sHideColZeros');
end;

class function TfcxHideColZerosSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 6;
end;

class function TfcxHideColZerosSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioCheck];
end;

{ TfcxHideRowZerosSliceToolbarItem }

class procedure TfcxHideRowZerosSliceToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.HideRowZeros := not TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.HideRowZeros;
end;

class function TfcxHideRowZerosSliceToolbarItem.GetChecked(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := TfcxSliceGridToolBar(Sender).SliceGrid.Slice.HideRowZeros;
end;

class function TfcxHideRowZerosSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sHideRowZeros');
end;

class function TfcxHideRowZerosSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 5;
end;

class function TfcxHideRowZerosSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioCheck];
end;

{ TfcxRowSortSliceToolbarItem }

class function TfcxRowSortSliceToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
const
  RowSortSubMenuCaptions: array[TfcxTypeSortAxis] of String  = (
    'sSortByValues',
    'sSortByTotals',
    'sSortByYSelection'
  );
var
  SortType: TfcxTypeSortAxis;
  Item: TMenuItem;
begin
  Result := inherited GetDropDownMenu(Owner);
  for SortType := Low(RowSortSubMenuCaptions) to High(RowSortSubMenuCaptions) do
  begin
    Item := TMenuItem.Create(Result);
    with Item do
    begin
      RadioItem := True;
      TfcxPopupMenuProcessor.SetItemCaption(Item, fcxResources.Get(RowSortSubMenuCaptions[SortType]));
      OnClick := MenuItemClick;
    end;
    TfcxPopupMenuProcessor.AddItem(Result, Item);
  end;
  SetMenuPopupProc(Result, MenuPopup);
end;

class function TfcxRowSortSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sRowSort');
end;

class function TfcxRowSortSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 7;
end;

class function TfcxRowSortSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioHasDropDown];
end;

class procedure TfcxRowSortSliceToolbarItem.MenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
  UseDefault: boolean;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  if Assigned(Toolbar.OnMenuItemClick) then
  begin
    UseDefault := False;
    Toolbar.OnMenuItemClick(Toolbar, TfcxPopupMenu(MenuItem.Owner).ToolButton.Item, TfcxPopupMenuProcessor.ItemIndex(MenuItem), UseDefault);
    if not UseDefault then
      Exit;
  end;
  Toolbar.SliceGrid.Slice.YAxisContainer.DefaultTypeSort := TfcxTypeSortAxis(TfcxPopupMenuProcessor.ItemIndex(MenuItem));
end;

class procedure TfcxRowSortSliceToolbarItem.MenuPopup(Sender: TObject);
var
  PopupMenu: TfcxPopupMenu absolute Sender;
  I: Integer;
begin
  for I := 0 to TfcxPopupMenuProcessor.ItemsCount(PopupMenu) - 1 do
    TfcxPopupMenuProcessor.SetItemChecked(PopupMenu.Items[I], False);
  TfcxPopupMenuProcessor.SetItemChecked(PopupMenu.Items[Ord(TfcxSliceGridToolbar(PopupMenu.ToolButton.Toolbar).SliceGrid.Slice.YAxisContainer.DefaultTypeSort)], True);
end;

{ TfcxColSortSliceToolbarItem }

class function TfcxColSortSliceToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
const
  ColSortSubMenuCaptions: array[TfcxTypeSortAxis] of String  = (
    'sSortByValues',
    'sSortByTotals',
    'sSortByXSelection'
  );
var
  SortType: TfcxTypeSortAxis;
  Item: TMenuItem;
begin
  Result := inherited GetDropDownMenu(Owner);
  for SortType := Low(ColSortSubMenuCaptions) to High(ColSortSubMenuCaptions) do
  begin
    Item := TMenuItem.Create(Result);
    with Item do
    begin
      RadioItem := True;
      TfcxPopupMenuProcessor.SetItemCaption(Item, fcxResources.Get(ColSortSubMenuCaptions[SortType]));
      OnClick := MenuItemClick;
    end;
    TfcxPopupMenuProcessor.AddItem(Result, Item);
  end;
  SetMenuPopupProc(Result, MenuPopup);
end;

class function TfcxColSortSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sColSort');
end;

class function TfcxColSortSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 8;
end;

class function TfcxColSortSliceToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioHasDropDown];
end;

class procedure TfcxColSortSliceToolbarItem.MenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
  UseDefault: boolean;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar);
  if not Assigned(Toolbar) then
    Exit;
  if Assigned(Toolbar.OnMenuItemClick) then
  begin
    UseDefault := False;
    Toolbar.OnMenuItemClick(Toolbar, TfcxPopupMenu(MenuItem.Owner).ToolButton.Item, TfcxPopupMenuProcessor.ItemIndex(MenuItem), UseDefault);
    if not UseDefault then
      Exit;
  end;
  Toolbar.SliceGrid.Slice.XAxisContainer.DefaultTypeSort := TfcxTypeSortAxis(TfcxPopupMenuProcessor.ItemIndex(MenuItem));
end;

class procedure TfcxColSortSliceToolbarItem.MenuPopup(Sender: TObject);
var
  PopupMenu: TfcxPopupMenu absolute Sender;
  I: Integer;
begin
  for I := 0 to TfcxPopupMenuProcessor.ItemsCount(PopupMenu) - 1 do
    TfcxPopupMenuProcessor.SetItemChecked(PopupMenu.Items[I], False);
  TfcxPopupMenuProcessor.SetItemChecked(PopupMenu.Items[Ord(TfcxSliceGridToolbar(PopupMenu.ToolButton.Toolbar).SliceGrid.Slice.XAxisContainer.DefaultTypeSort)], True);
end;

{ TfcxEditMeasuresSliceToolbarItems }

class procedure TfcxEditMeasuresSliceToolbarItems.DoClick(Sender: TfcxToolButton);
var
  MeasureIndex: Integer;
  Measure: TfcxMeasureField;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(Sender.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  MeasureIndex := Toolbar.SliceGrid.DataZone.SelectedMeasure;
  if MeasureIndex <> -1 then
  begin
    Measure := Toolbar.SliceGrid.Slice.MeasuresContainer.Measures[MeasureIndex];
    with TfcxMeasureEditorForm.Create(Toolbar.SliceGrid) do
      Execute(Toolbar.SliceGrid.Slice, Measure);
  end;
end;

class function TfcxEditMeasuresSliceToolbarItems.GetHint: String;
begin
  Result := fcxResources.Get('sEditMeasures');
end;

class function TfcxEditMeasuresSliceToolbarItems.GetImageIndex: Integer;
begin
  Result := 9;
end;

{ TfcxDataMarkerSliceToolbarItems }

class procedure TfcxDataHighlightSliceToolbarItems.DoClick(Sender: TfcxToolButton);
var
  MeasureIndex: Integer;
  Measure: TfcxMeasureField;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(Sender.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  MeasureIndex := Toolbar.SliceGrid.DataZone.SelectedMeasure;
  if MeasureIndex <> -1 then
  begin
    Measure := Toolbar.SliceGrid.Slice.MeasuresContainer.Measures[MeasureIndex];
    with TfcxMeasureEditorForm.Create(Toolbar.SliceGrid) do
      Execute(Toolbar.SliceGrid.Slice, Measure, fcxMeasureEditorStylePageIndex);
  end;
end;

class function TfcxDataHighlightSliceToolbarItems.GetHint: String;
begin
  Result := fcxResources.Get('sDataMarker');
end;

class function TfcxDataHighlightSliceToolbarItems.GetImageIndex: Integer;
begin
  Result := 10;
end;

{ TfcxDisplayFormatSliceToolbarItems }

class procedure TfcxDisplayFormatSliceToolbarItems.DoClick(Sender: TfcxToolButton);
var
  MeasureIndex: Integer;
  Measure: TfcxMeasureField;
  Toolbar: TfcxSliceGridToolbar;
begin
  Toolbar := TfcxSliceGridToolbar(Sender.Toolbar);
  if Not Assigned(Toolbar) then
    Exit;
  MeasureIndex := Toolbar.SliceGrid.DataZone.SelectedMeasure;
  if MeasureIndex <> -1 then
  begin
    Measure := Toolbar.SliceGrid.Slice.MeasuresContainer.Measures[MeasureIndex];
    with TfcxMeasureEditorForm.Create(Toolbar.SliceGrid) do
      Execute(Toolbar.SliceGrid.Slice, Measure, fcxMeasureEditorFormatPageIndex);
  end;
end;

class function TfcxDisplayFormatSliceToolbarItems.GetHint: String;
begin
  Result := fcxResources.Get('sDisplayFormat');
end;

class function TfcxDisplayFormatSliceToolbarItems.GetImageIndex: Integer;
begin
  Result := 11;
end;

{ TfcxInformationSliceToolbarItems }

class procedure TfcxInformationSliceToolbarItems.DoClick(Sender: TfcxToolButton);
var
  InfoForm: TfcxInfoForm;
begin
  InfoForm := TfcxInfoForm.Create(Application);
  InfoForm.ShowInfo(TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice)
end;

class function TfcxInformationSliceToolbarItems.GetHint: String;
begin
  Result := fcxResources.Get('sInformation');
end;

class function TfcxInformationSliceToolbarItems.GetImageIndex: Integer;
begin
  Result := 13;
end;

{ TfcxTransposeSliceToolbarItem }

class procedure TfcxTransposeSliceToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxSliceGridToolBar(Sender.Toolbar).SliceGrid.Slice.Transpose;
end;

class function TfcxTransposeSliceToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sTranspose');
end;

class function TfcxTransposeSliceToolbarItem.GetImageIndex: Integer;
begin
  Result := 4;
end;

{ TfcxSliceGridToolbarItem }

class function TfcxSliceGridToolbarItem.GetEnabled(Sender: TfcxCustomToolbar): Boolean;
var
  ToolBar: TfcxSliceGridToolbar absolute Sender;
begin
 Result := Assigned(ToolBar.SliceGrid) and
   Assigned(ToolBar.SliceGrid.Slice) and
   Assigned(ToolBar.SliceGrid.Slice.Cube);
end;

initialization
{$IFDEF DELPHI_16UP}
  {$IFDEF FMX}
{
  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(TfcxSliceGridToolBar, TFmxObject);
}
  {$ELSE}
{
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TfcxSliceGridToolBar, TControl);
}
  {$ENDIF}
{$ENDIF}
  SliceGridToolbarItems := TfcxToolbarItems.Create;
  fcxRegisterSliceGridToolBarItem(TfcxSaveSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxOpenSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxClearSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxExportToSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxTransposeSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxHideRowZerosSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxHideColZerosSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxRowSortSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxColSortSliceToolbarItem);
  fcxRegisterSliceGridToolBarItem(TfcxEditMeasuresSliceToolbarItems);
  fcxRegisterSliceGridToolBarItem(TfcxDataHighlightSliceToolbarItems);
  fcxRegisterSliceGridToolBarItem(TfcxDisplayFormatSliceToolbarItems);
  fcxRegisterSliceGridToolBarItem(TfcxFieldListSliceToolbarItems);
  fcxRegisterSliceGridToolBarItem(TfcxInformationSliceToolbarItems);

finalization
  SliceGridToolbarItems.Free;
end.
