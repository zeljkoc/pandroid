{*******************************************************}
{                                                       }
{          FastCube 2.0 custom toolbar unit             }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxCustomToolbar;

interface
{$INCLUDE fcx.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, ImgList;

type
  TfcxCustomToolbar = class;
  TfcxToolButton = class;
  TfcxPopupMenu = class;

  TfcxToolbarItemOption = (
    ioCheck,
    ioHasDropDown,        // has a drop down menu
    ioStartGroup          // place a separator before this item
  );
  TfcxToolbarItemOptions = set of TfcxToolbarItemOption;

  TfcxToolbarItem = class
  protected
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; virtual;
    class function GetImageIndex: Integer; virtual;
    class function GetOptions: TfcxToolbarItemOptions; virtual;
    class function GetEnabled(Sender: TfcxCustomToolbar): Boolean; virtual;
    class function GetHint: String; virtual;
    class function GetChecked(Sender: TfcxCustomToolbar): Boolean; virtual;
    class procedure DoClick(Sender: TfcxToolButton); virtual;
    class procedure SetMenuPopupProc(APopupMenu: TPopupMenu; AProc: TNotifyEvent);
  end;

  TfcxToolbarItemClass = class of TfcxToolbarItem;

  TfcxToolbarItems = class(TList)
  private
    function GetItem(AIndex: Integer): TfcxToolbarItemClass;
    procedure SetItem(AIndex: Integer; const Value: TfcxToolbarItemClass);
  public
    property Items[AIndex: Integer]: TfcxToolbarItemClass read GetItem write SetItem; default;
  end;

  TfcxToolButton = class(TToolButton)
  private
    FItem: TfcxToolbarItemClass;
    FGroupSeparator: TfcxToolButton;
    function GetToolbar: TfcxCustomToolbar;
  protected
    function CheckVisible: Boolean;
    procedure UpdateState;
  public
    destructor Destroy; override;
    property Toolbar: TfcxCustomToolbar read GetToolbar;
    property GroupSeparator: TfcxToolButton read FGroupSeparator write FGroupSeparator;
    property Item: TfcxToolbarItemClass read FItem write FItem;
  end;

  TfcxCustomToolbarGetButtonVisibleEvent = procedure(Sender: TfcxCustomToolbar; Button: TfcxToolbarItemClass; var Visible: Boolean) of Object;
  TfcxCustomToolbarButtonClickEvent = procedure(Sender: TfcxCustomToolbar; Button: TfcxToolbarItemClass; var UseDefault: Boolean) of Object;
  TfcxCustomToolbarMenuItemClickEvent = procedure(Sender: TfcxCustomToolbar; Button: TfcxToolbarItemClass; MenuIndex: integer; var UseDefault: Boolean) of Object;

  TfcxCustomToolbar = class(TToolBar)
  private
    FGetButtonVisible: TfcxCustomToolbarGetButtonVisibleEvent;
    FOnButtonClick: TfcxCustomToolbarButtonClickEvent;
    FOnMenuItemClick: TfcxCustomToolbarMenuItemClickEvent;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    function GetItems: TfcxToolbarItems; virtual;
    function GetImageList: TCustomImageList; virtual;
    procedure CreateChildren;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateState;
    procedure DoButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Items: TfcxToolbarItems read GetItems;
  published
    property Version: String read GetVersion write SetVersion;
    property OnGetButtonVisible: TfcxCustomToolbarGetButtonVisibleEvent read FGetButtonVisible write FGetButtonVisible;
    property OnButtonClick: TfcxCustomToolbarButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnMenuItemClick: TfcxCustomToolbarMenuItemClickEvent read FOnMenuItemClick write FOnMenuItemClick;
  end;

  TfcxPopupMenu = class(TPopupMenu)
  private
    FToolButton: TfcxToolButton;
  public
    property ToolButton: TfcxToolButton read FToolButton write FToolButton;
  end;

  TfcxPopupMenuProcessor = class
  public
    class procedure Clear(Menu: TPopupMenu);
    class function ItemsCount(Menu: TPopupMenu): integer;
    class procedure AddItem(Menu: TPopupMenu; Item: TMenuItem);
    class procedure SetItemCaption(Item: TMenuItem; Caption: String);
    class function GetItemChecked(Item: TMenuItem): boolean;
    class procedure SetItemChecked(Item: TMenuItem; Value: boolean);
    class function ItemIndex(Item: TMenuItem): integer;
  end;

implementation

uses 
  fcxRes;

{TfcxPopupMenuProcessor}

class function TfcxPopupMenuProcessor.ItemsCount(Menu: TPopupMenu): integer;
begin
  Result := Menu.Items.Count;
end;

class procedure TfcxPopupMenuProcessor.Clear(Menu: TPopupMenu);
begin
  Menu.Items.Clear;
end;

class procedure TfcxPopupMenuProcessor.AddItem(Menu: TPopupMenu; Item: TMenuItem);
begin
  Menu.Items.Add(Item);
end;

class procedure TfcxPopupMenuProcessor.SetItemCaption(Item: TMenuItem; Caption: String);
begin
  Item.Caption := Caption;
end;

class procedure TfcxPopupMenuProcessor.SetItemChecked(Item: TMenuItem; Value: boolean);
begin
  Item.Checked := Value;
end;

class function TfcxPopupMenuProcessor.GetItemChecked(Item: TMenuItem): boolean;
begin
  Result := Item.Checked;
end;

class function TfcxPopupMenuProcessor.ItemIndex(Item: TMenuItem): integer;
begin
  Result := Item.MenuIndex;
end;

{ TfcxToolbarItems }

function TfcxToolbarItems.GetItem(AIndex: Integer): TfcxToolbarItemClass;
begin
  Result := TfcxToolbarItemClass(inherited Get(AIndex));
end;

procedure TfcxToolbarItems.SetItem(AIndex: Integer; const Value: TfcxToolbarItemClass);
begin
  inherited Put(AIndex, Value);
end;

{ TfcxToolbarItem }

class procedure TfcxToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  if ioHasDropDown in GetOptions then
    Sender.CheckMenuDropdown
end;

class function TfcxToolbarItem.GetChecked(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := False;
end;

class function TfcxToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
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
  Result := TfcxPopupMenu.Create(TopOwner);
  TfcxPopupMenu(Result).ToolButton := Owner;
end;

class function TfcxToolbarItem.GetEnabled(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := False;
end;

class function TfcxToolbarItem.GetHint: String;
begin
  Result := '';
end;

class function TfcxToolbarItem.GetImageIndex: Integer;
begin
  Result := -1;
end;

class function TfcxToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [];
end;

class procedure TfcxToolbarItem.SetMenuPopupProc(APopupMenu: TPopupMenu; AProc: TNotifyEvent);
begin
  APopupMenu.OnPopup := AProc;
end;

{ TfcxCustomToolbar }

constructor TfcxCustomToolbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateChildren;
end;

procedure TfcxCustomToolbar.CreateChildren;
var
  i: integer;
  Item: TfcxToolbarItemClass;
  Sep, B: TfcxToolButton;
begin
  for i := 0 to Items.Count - 1 do
  begin
    Item := Items[i];
    if (ioStartGroup in Item.GetOptions) and (i > 0) then
    begin
      Sep := TfcxToolButton.Create(Self);
      Sep.Style := tbsDivider;
      Sep.Width := 4;
    end
    else
      Sep := nil;
    B := TfcxToolButton.Create(Self);
    B.Item := Item;
    B.GroupSeparator := Sep;
    if (ioHasDropDown in Item.GetOptions) then
    begin
      B.Style := tbsDropDown;
      B.DropdownMenu := Item.GetDropDownMenu(B);
    end
    else
    if (ioCheck in Item.GetOptions) then
    begin
      B.Style := tbsCheck;
      B.AllowAllUp := True;
    end;
    B.ImageIndex := Item.GetImageIndex;
    B.Hint := Item.GetHint;
    B.OnClick := DoButtonClick;
  end;
end;

procedure TfcxCustomToolbar.DoButtonClick(Sender: TObject);
var
  UseDefault: Boolean;
begin
  if Sender is TfcxToolButton then
  begin
    if Assigned(OnButtonClick) then
    begin
      UseDefault := False;
      OnButtonClick(Self, TfcxToolButton(Sender).Item, UseDefault);
      if not UseDefault then
        Exit;
    end;
    TfcxToolButton(Sender).Item.DoClick(TfcxToolButton(Sender));
    if TfcxToolButton(Sender).Style = tbsCheck then
      TfcxToolButton(Sender).Down := TfcxToolButton(Sender).Item.GetChecked(Self);
  end;
end;

procedure TfcxCustomToolbar.GetChildren(Proc: TGetChildProc; Root: TComponent);

  function FindButton(AControl: TControl): Boolean;
  var
    i: integer;
  begin
    for i := 0 to ButtonCount - 1 do
      if Buttons[i] = AControl then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

var
  I: Integer;
  Control: TControl;
begin
{$IFNDEF FPC}
{$IFDEF DELPHI_6UP}
  if Assigned(Menu) then exit;
{$ENDIF}
{$ENDIF}
  for I := 0 to ButtonCount - 1 do
    if not (Buttons[i] is TfcxToolButton) then Proc(Buttons[I]);
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and not FindButton(Control) then Proc(Control);
  end;
end;

function TfcxCustomToolbar.GetImageList: TCustomImageList;
begin
  Result := nil;
end;

function TfcxCustomToolbar.GetItems: TfcxToolbarItems;
begin
  Result := nil;
end;

function TfcxCustomToolbar.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxCustomToolbar.SetParent(AParent: TWinControl);
var
  i, x: integer;
  OldParent: TWinControl;
begin
  OldParent := Parent;
  inherited;
  if (Parent <> nil) and (OldParent <> Parent) then
  begin
    Images := GetImageList;
    x := 0;
    for i := 0 to ComponentCount - 1 do
    begin
      if Components[i] is TToolButton then
      begin
        TToolButton(Components[i]).Left := x;
        TToolButton(Components[i]).Parent := Self;
        inc(x, TToolButton(Components[i]).Width);
      end;
    end;
    AutoSize := True;
    UpdateState;
  end;
end;

procedure TfcxCustomToolbar.SetVersion(const Value: String);
begin
//
end;

procedure TfcxCustomToolbar.UpdateState;
var
  i: integer;
  Btn: TToolButton;
begin
  if csDestroying in ComponentState then
    Exit;
  for i := 0 to ButtonCount - 1 do
  begin
    Btn := Buttons[i];
    if (Btn is TfcxToolButton) then
      TfcxToolButton(Btn).UpdateState;
  end;
end;

{ TfcxToolButton }

function TfcxToolButton.CheckVisible: Boolean;
begin
  Result := True;
  if Assigned(Item) and Assigned(TfcxCustomToolbar(FToolBar).OnGetButtonVisible) then
    TfcxCustomToolbar(FToolBar).OnGetButtonVisible(TfcxCustomToolbar(FToolBar), Item, Result);
end;

destructor TfcxToolButton.Destroy;
begin
  DropdownMenu.Free;
  inherited;
end;

function TfcxToolButton.GetToolbar: TfcxCustomToolbar;
begin
  Result := TfcxCustomToolBar(FToolBar);
end;

procedure TfcxToolButton.UpdateState;
begin
  Visible := CheckVisible;
  if Assigned(GroupSeparator) then
    GroupSeparator.Visible := Visible;
  Enabled := Assigned(Item) and Item.GetEnabled(TfcxCustomToolBar(FToolBar));
  if Enabled and (Style = tbsCheck) then
     Down := Item.GetChecked(TfcxCustomToolBar(FToolBar));
end;

end.
