{*******************************************************}
{                                                       }
{             FastCube 2 Fields Popup unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxFieldsPopup;

interface

{$INCLUDE fcx.inc}

uses
  {$IFNDEF FPC}
  Windows,
  {$ELSE}
  LCLType, LCLIntf,
  {$ENDIF}
{$IFDEF DELPHI_6UP}
  Types,
{$ENDIF}
  Classes, Controls, StdCtrls, Graphics, Menus,
  fcxSlice, fcxCube, fcxPopupWindow, fcxPopupProvider,
  fcxTypes, fcxPainters, fcxGridPainters;

type
  TfcxSliceFieldsDataProvider = class(TfcxCustomTreeProvider)
  private
    FSlice: TfcxSlice;
  protected
    function GetAllowDrag: Boolean; override;
  public
    function GetDragItem(ANode: Pointer): TObject; override;
    function GetNodeText(ANode: Pointer): TfcxString; override;

    // own members
    constructor Create(ASlice: TfcxSlice);
    destructor Destroy; override;
  end;

  TfcxCubeFieldsDataProvider = class(TfcxCustomTreeProvider)
  private
    FCube: TfcxCube;
  public
    function GetNodeText(ANode: Pointer): TfcxString; override;

    // own members
    constructor Create(ACube: TfcxCube);
    destructor Destroy; override;
  end;

  TfcxSliceFieldsPopup = class(TfcxCustomNodePopup)
  private
    procedure SetSlice(const Value: TfcxSlice);
  public
    constructor Create(AOwner: TComponent); override;
    property Slice: TfcxSlice write SetSlice;
  end;

  TfcxCubeFieldsPopup = class(TfcxCustomNodePopup)
  private
    procedure SetCube(const Value: TfcxCube);
    function GetDataProvider: TfcxCubeFieldsDataProvider;
    procedure SetDataProvider(const Value: TfcxCubeFieldsDataProvider); reintroduce;
  protected
    procedure UpdateButtons;
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure ClearAllClick(Sender: TObject);
    procedure InverseClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property Cube: TfcxCube write SetCube;
    property DataProvider: TfcxCubeFieldsDataProvider read GetDataProvider write SetDataProvider;
  end;

const
  CounterPtr = nil;

implementation

uses
  fcxRes;

{ TfcxSliceFieldsPopup }

constructor TfcxSliceFieldsPopup.Create(AOwner: TComponent);
begin
  inherited;
  Width := 160;
end;

procedure TfcxSliceFieldsPopup.SetSlice(const Value: TfcxSlice);
begin
  DataProvider.Free;
  DataProvider := TfcxSliceFieldsDataProvider.Create(Value);
//  Width := Field.PopUpWidth;
//  UpdateButtons;
//  UpdateConstraints;
end;

{ TfcxSliceFieldsDataProvider }

constructor TfcxSliceFieldsDataProvider.Create(ASlice: TfcxSlice);
var
  CounterNode: PfcxTreeNode;
begin
  inherited Create;
  FSlice := ASlice;
  FTree := FSlice.SliceFields.BuildTree;
  if Assigned(FTree.Root.FirstChild) then
  begin
    CounterNode := FTree.InsertNode(FTree.Root.FirstChild, True);
    CounterNode.Data := CounterPtr;
    SetAllNodeExpanded(True);
  end;
end;

function TfcxSliceFieldsDataProvider.GetNodeText(ANode: Pointer): TfcxString;
begin
  if PfcxTreeNode(ANode)^.Data = CounterPtr then
    Result := ControlToString(fcxResources.Get('SCounterItem'))
  else
    Result := TfcxSliceField(PfcxTreeNode(ANode)^.Data).Caption
end;

destructor TfcxSliceFieldsDataProvider.Destroy;
begin
  FTree.Free;
  inherited;
end;

function TfcxSliceFieldsDataProvider.GetAllowDrag: Boolean;
begin
  Result := True;
end;

function TfcxSliceFieldsDataProvider.GetDragItem(ANode: Pointer): TObject;
begin
  Result := PfcxTreeNode(ANode)^.Data;
end;

{ TfcxCubeFieldsDataProvider }

constructor TfcxCubeFieldsDataProvider.Create(ACube: TfcxCube);
begin
  inherited Create;
  FCube := ACube;
  FTree := FCube.Fields.BuildTree;
  SetAllNodeExpanded(True);
end;

destructor TfcxCubeFieldsDataProvider.Destroy;
begin
  FTree.Free;
  inherited;
end;

function TfcxCubeFieldsDataProvider.GetNodeText(ANode: Pointer): TfcxString;
begin
  Result := TfcxCommonField(PfcxTreeNode(ANode)^.Data).CubeFieldDisplayLabel
end;

{ TfcxCubeFieldsPopup }

procedure TfcxCubeFieldsPopup.CancelClick(Sender: TObject);
begin
  CloseUp(True);
end;

procedure TfcxCubeFieldsPopup.ClearAllClick(Sender: TObject);
begin
  DataProvider.SetAllNodeState(csUnchecked);
end;

constructor TfcxCubeFieldsPopup.Create(AOwner: TComponent);
begin
  inherited;
  Width := 160;
end;

function TfcxCubeFieldsPopup.GetDataProvider: TfcxCubeFieldsDataProvider;
begin
  Result := TfcxCubeFieldsDataProvider(inherited DataProvider);
end;

procedure TfcxCubeFieldsPopup.InverseClick(Sender: TObject);
begin
  DataProvider.InverseNodeState;
end;

procedure TfcxCubeFieldsPopup.OkClick(Sender: TObject);
begin
  CloseUp(False);
end;

procedure TfcxCubeFieldsPopup.SelectAllClick(Sender: TObject);
begin
  DataProvider.SetAllNodeState(csChecked);
end;

procedure TfcxCubeFieldsPopup.SetCube(const Value: TfcxCube);
begin
  DataProvider.Free;
  DataProvider := TfcxCubeFieldsDataProvider.Create(Value);
  UpdateButtons;
end;

procedure TfcxCubeFieldsPopup.SetDataProvider(const Value: TfcxCubeFieldsDataProvider);
begin
  inherited SetDataProvider(Value);
end;

procedure TfcxCubeFieldsPopup.UpdateButtons;
begin
  FooterControls.Clear;
  with AddFooterButton do
  begin
    Hint := fcxResources.Get('sSelectAll');
    ImageIndex := 0;
    OnClick := SelectAllClick;
  end;
  with AddFooterButton do
  begin
    Hint := fcxResources.Get('sClearAll');
    ImageIndex := 1;
    OnClick := ClearAllClick;
  end;
  with AddFooterButton do
  begin
    Hint := fcxResources.Get('sInverse');
    ImageIndex := 2;
    OnClick := InverseClick;
  end;
  with AddFooterButton do
  begin
    AnchorToLeft := False;
    Hint := fcxResources.Get('sOkBtn');
    ImageIndex := 3;
    OnClick := OkClick;
  end;
  with AddFooterButton do
  begin
    AnchorToLeft := False;
    Hint := fcxResources.Get('sCancelBtn');
    ImageIndex := 4;
    OnClick := CancelClick;
  end;
end;

end.
