{*******************************************************}
{                                                       }
{             FastCube 2 Filter Popup unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxFilterPopup;

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
  fcxSlice, fcxPopupProvider, fcxPopupWindow, fcxTypes, fcxPainters, fcxGridPainters;

type
  TfcxSliceFieldDataProvider = class(TfcxCustomTreeProvider)
  private
    FField: TfcxSliceField;
  public
    function IsTreeLike: Boolean; override;
    function GetNodeKind(ANode: Pointer): TfcxNodeKind; override;

    function GetNodeText(ANode: Pointer): TfcxString; override;
    function GetNodeState(ANode: Pointer): TfcxCheckState; override;
    procedure SetNodeState(ANode: Pointer; AState: TfcxCheckState); override;
    procedure SetSingleCheck(ANode: Pointer); override;

    procedure BeforePopup; override;
    procedure AfterCloseup(const Cancel: Boolean); override;

    // own members
    constructor Create(AField: TfcxSliceField);
    destructor Destroy; override;
  end;

  TfcxSliceMeasureDataProvider = class(TfcxCustomNodeProvider)
  private
    FContainer: TfcxMeasuresContainer;
  protected
    function GetAllowDrag: Boolean; override;
    function GetMeasure(ANode: Pointer): TfcxMeasureField;
  public
    // overrides
    function GetDragItem(ANode: Pointer): TObject; override;
    function GetFirstNode: Pointer; override;
    function GetNextSibling(ANode: Pointer): Pointer; override;
    function GetPrevSibling(ANode: Pointer): Pointer; override;
    function GetFirstChild(ANode: Pointer): Pointer; override;
    function GetParent(ANode: Pointer): Pointer; override;
    function GetVisibleByIndex(AIndex: Integer): Pointer; override;
    function GetVisibleIndex(ANode: Pointer): Integer; override;
    function GetNodeExpanded(ANode: Pointer): Boolean; override;
    function GetNodeLevel(ANode: Pointer): Integer; override;
    function GetVisibleNodeCount: Integer; override;

    function GetNodeKind(ANode: Pointer): TfcxNodeKind; override;
    function IsTreeLike: Boolean; override;

    function GetNodeText(ANode: Pointer): TfcxString; override;
    function GetNodeState(ANode: Pointer): TfcxCheckState; override;
    procedure SetSingleCheck(ANode: Pointer); override;
    procedure SetNodeState(ANode: Pointer; AState: TfcxCheckState); override;
    procedure SetNodeExpanded(ANode: Pointer; AValue: Boolean); override;
    procedure PerformDefaultNodeAction(ANode: Pointer); override;
    // own members
    constructor Create(AContainer: TfcxMeasuresContainer);
  end;

  TfcxCustomFilterPopup = class(TfcxCustomNodePopup)
  protected
    procedure ClientDblClick(X: Integer; Y: Integer); override;

    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  end;

  TfcxFilterPopup = class(TfcxCustomFilterPopup)
  private
    procedure SetField(const Value: TfcxSliceField);
    function GetField: TfcxSliceField;
  protected
    procedure UpdateButtons;
    // clicks
    procedure SelectAllClick(Sender: TObject);
    procedure ClearAllClick(Sender: TObject);
    procedure InverseClick(Sender: TObject);
    procedure ShowAvailableClick(Sender: TObject);
  public
    procedure CloseUp(Cancel: Boolean); override;
    property Field: TfcxSliceField read GetField write SetField;
  end;

  TfcxMeasurePopup = class(TfcxCustomFilterPopup)
  private
    FPopupNode: Pointer;
    procedure SetContainer(const Value: TfcxMeasuresContainer);
    function GetContainer: TfcxMeasuresContainer;
    function GetMeasure(ANode: Pointer): TfcxMeasureField;
    procedure PopupItemClick(Sender: TObject);
    procedure AlignmentPopupItemClick(Sender: TObject);
    procedure DisplayAsPopupItemClick(Sender: TObject);
  protected
    procedure UpdateButtons;
    // clicks
    procedure SelectAllClick(Sender: TObject);
    procedure ClearAllClick(Sender: TObject);
    procedure InverseClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    function CreatePopupMenu: TPopupMenu; override;
    procedure PreparePopupMenuFor(ANode: Pointer); override;
  public
    property Container: TfcxMeasuresContainer read GetContainer write SetContainer;
  end;

implementation

uses
  fcxRes,
  fcxGraphicRes,
  fcxCube,
  fcxUniqueArray,
  fcxMeasureEditor;

const
  BoolToNodeState: array[Boolean] of TfcxCheckState = (csUnchecked, csChecked);
   
{ TfcxCustomFilterPopup }

procedure TfcxCustomFilterPopup.CancelClick(Sender: TObject);
begin
  CloseUp(True);
end;

procedure TfcxCustomFilterPopup.OkClick(Sender: TObject);
begin
  CloseUp(False);
end;

procedure TfcxCustomFilterPopup.ClientDblClick(X, Y: Integer);
begin
  inherited;
  if (ActiveNode <> nil) and (ActiveClientPart = fppItem) then
    DataProvider.PerformDefaultNodeAction(ActiveNode);
end;

{ TfcxFilterPopup }

procedure TfcxFilterPopup.SetField(const Value: TfcxSliceField);
begin
  DataProvider.Free;
  DataProvider := TfcxSliceFieldDataProvider.Create(Value);
  Width := Field.PopUpWidth;
  UpdateButtons;
  UpdateConstraints;
end;

procedure TfcxFilterPopup.ClearAllClick(Sender: TObject);
begin
  Field.SetNoneFilter;
  InvalidateClientRect(GetClientRect);
end;

procedure TfcxFilterPopup.InverseClick(Sender: TObject);
begin
  Field.InverseFilter;
  InvalidateClientRect(GetClientRect);
end;

procedure TfcxFilterPopup.SelectAllClick(Sender: TObject);
begin
  Field.SetAllFilter;
  InvalidateClientRect(GetClientRect);
end;

procedure TfcxFilterPopup.UpdateButtons;
begin
  FooterControls.Clear;
  if Assigned(Field) and (Field.UVFilterType = uvft_Set) then
  begin
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
    if Field.Slice.FilterManager.OnlyAvailableSupport then
      with AddFooterButton do
      begin
        Hint := fcxResources.Get('sShowAvailable');
        ImageIndex := 7;
        Down := Field.PopupShowAvailable;
        OnClick := ShowAvailableClick;
      end;
  end;
  with AddFooterButton do
  begin
    AnchorToLeft := False;
    Hint := fcxResources.Get('sOkBtn');
    ImageIndex := 3;
    OnClick := OkClick;
  end;
  if Assigned(Field) and not Field.Slice.AutoUVFilter then
    with AddFooterButton do
    begin
      AnchorToLeft := False;
      Hint := fcxResources.Get('sCancelBtn');
      ImageIndex := 4;
      OnClick := CancelClick;
    end;
end;

procedure TfcxFilterPopup.CloseUp(Cancel: Boolean);
begin
  inherited;
  if Assigned(Field) then
    Field.PopUpWidth := Width;
end;

procedure TfcxFilterPopup.ShowAvailableClick(Sender: TObject);
var
  AField: TfcxSliceField;
begin
  AField := Field;
  AField.PopupShowAvailable := not AField.PopupShowAvailable;
  TfcxPopupButton(FooterControls[3]).Down := AField.PopupShowAvailable;
  DataProvider.Free;
  DataProvider := TfcxSliceFieldDataProvider.Create(AField);
  Invalidate;
end;

{ TfcxMeasurePopup }

procedure TfcxMeasurePopup.SetContainer(const Value: TfcxMeasuresContainer);
begin
  DataProvider.Free;
  DataProvider := TfcxSliceMeasureDataProvider.Create(Value);
  UpdateButtons;
  UpdateConstraints;
end;

procedure TfcxMeasurePopup.UpdateButtons;
begin
  FooterControls.Clear;
  if Assigned(Container) then
  begin
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
      Hint := fcxResources.Get('sMoveUp');
      ImageIndex := 5;
      OnClick := MoveUpClick;
    end;
    with AddFooterButton do
    begin
      Hint := fcxResources.Get('sMoveDown');
      ImageIndex := 6;
      OnClick := MoveDownClick;
    end;
  end;
  with AddFooterButton do
  begin
    AnchorToLeft := False;
    Hint := fcxResources.Get('sOkBtn');
    ImageIndex := 3;
    OnClick := OkClick;
  end;
end;

procedure TfcxMeasurePopup.SelectAllClick(Sender: TObject);
begin
  Container.SetAllVisible;
  Invalidate;
end;

procedure TfcxMeasurePopup.ClearAllClick(Sender: TObject);
begin
  Container.SetNoneVisible;
  Invalidate;
end;

procedure TfcxMeasurePopup.InverseClick(Sender: TObject);
begin
  Container.InverseVisible;
  Invalidate;
end;

procedure TfcxMeasurePopup.MoveDownClick(Sender: TObject);
begin
  if Container.MoveMeasure(DataProvider.GetVisibleIndex(FocusedNode), DataProvider.GetVisibleIndex(FocusedNode) + 1) then
    FocusedNode := DataProvider.GetNextSibling(FocusedNode);
end;

procedure TfcxMeasurePopup.MoveUpClick(Sender: TObject);
begin
  if Container.MoveMeasure(DataProvider.GetVisibleIndex(FocusedNode), DataProvider.GetVisibleIndex(FocusedNode) - 1) then
    FocusedNode := DataProvider.GetPrevSibling(FocusedNode);
end;

function TfcxFilterPopup.GetField: TfcxSliceField;
begin
  if Assigned(DataProvider) then
    Result := TfcxSliceFieldDataProvider(DataProvider).FField
  else
    Result := nil;
end;

function TfcxMeasurePopup.GetContainer: TfcxMeasuresContainer;
begin
  if Assigned(DataProvider) then
    Result := TfcxSliceMeasureDataProvider(DataProvider).FContainer
  else
    Result := nil;
end;

function TfcxMeasurePopup.CreatePopupMenu: TPopupMenu;
const
  Items: array[0..4] of String = (
    'sAlignment',
    'sDisplayAs',
    cLineCaption,
    'sDeleteMeasure',
    'sProperties'
  );
  AlignmentItems: array[TAlignment] of String = (
  { taLeftJustify  } 'sLeftJustify',
  { taRightJustify } 'sRightJustify',
  { taCenter       } 'sCenter'
  );
  AlignmentIcons: array[TAlignment] of Integer = (14, 15, 16);
  DisplayAsIcons: array[TfcxDisplayAs] of integer = (
 { da_Value             } 17,
 { da_RowPercentTotal   } 18,
 { da_ColPercentTotal   } 19,
 { da_RowPercentLevel   } 18,
 { da_ColPercentLevel   } 19,
 { da_GrandTotalPercent } 20,
 { da_RowRank           } 21,
 { da_ColRank           } 22
  );
  DisplayAsItems: array[TfcxDisplayAs] of String = (
 { da_Value             } 'sOriginalValue',
 { da_RowPercentTotal   } 'sRowPercentTotal',
 { da_ColPercentTotal   } 'sColPercentTotal',
 { da_RowPercentLevel   } 'sRowPercentGroup',
 { da_ColPercentLevel   } 'sColPercentGroup',
 { da_GrandTotalPercent } 'sGrandTotalPercent',
 { da_RowRank           } 'sRowRank',
 { da_ColRank           } 'sColRank'
  );
var
  I: Integer;
  AL: TAlignment;
  DA: TfcxDisplayAs;
  Item: TMenuItem;
begin
  Result := inherited CreatePopupMenu;
  for I := Low(Items) to High(Items) do
  begin
    Item := NewItem(fcxResources.Get(Items[I]), 0, False, True, PopupItemClick, 0, '');
    Result.Items.Add(Item);
  end;
  Result.Items[0].SubMenuImages := fcxGraphicResources.ToolImages;
  for AL := Low(AlignmentItems) to High(AlignmentItems) do
  begin
    Item := NewItem(fcxResources.Get(AlignmentItems[AL]), 0, False, True, AlignmentPopupItemClick, 0, '');
    Item.ImageIndex := AlignmentIcons[AL];
    Item.RadioItem := True;
    Result.Items[0].Add(Item);
  end;
  Result.Items[1].SubMenuImages := fcxGraphicResources.ToolImages;
  for DA := Low(DisplayAsItems) to High(DisplayAsItems) do
  begin
    Item := NewItem(fcxResources.Get(DisplayAsItems[DA]), 0, False, True, DisplayAsPopupItemClick, 0, '');
    Item.ImageIndex := DisplayAsIcons[DA];
    Item.RadioItem := True;
    Result.Items[1].Add(Item);
  end;
end;

procedure TfcxMeasurePopup.AlignmentPopupItemClick(Sender: TObject);
begin
  GetMeasure(FPopupNode).Alignment := TAlignment((Sender as TMenuItem).MenuIndex);
end;

procedure TfcxMeasurePopup.DisplayAsPopupItemClick(Sender: TObject);
begin
  GetMeasure(FPopupNode).DisplayAs := TfcxDisplayAs((Sender as TMenuItem).MenuIndex);
end;

procedure TfcxMeasurePopup.PopupItemClick(Sender: TObject);
begin
  case (Sender as TMenuItem).MenuIndex of
    3:
      begin
        Container.DeleteMeasure(GetMeasure(FPopupNode).Index);
        Invalidate;
      end;
    4: DataProvider.PerformDefaultNodeAction(FPopupNode);
  end;
end;

function TfcxMeasurePopup.GetMeasure(ANode: Pointer): TfcxMeasureField;
begin
  if Assigned(DataProvider) and Assigned(ANode) then
    Result := TfcxSliceMeasureDataProvider(DataProvider).GetMeasure(ANode)
  else
    Result := nil;
end;

procedure TfcxMeasurePopup.PreparePopupMenuFor(ANode: Pointer);
var
  I, J: Integer;
  Measure: TfcxMeasureField;
begin
  FPopupNode := ANode;    
  with PopupMenu do
  begin
    for I := 0 to Items.Count - 1 do
      for J := 0 to Items[I].Count - 1 do
        Items[I][J].Checked := False;
    Measure := GetMeasure(FPopupNode);
    for I := 0 to Items.Count - 1 do
      Items[I].Visible := Assigned(Measure);
    if Assigned(Measure) then
    begin
      Items[0].Items[Ord(Measure.Alignment)].Checked := True;
      Items[1].Items[Ord(Measure.DisplayAs)].Checked := True;
    end;
  end;
end;

{ TfcxSliceFieldDataProvider }

constructor TfcxSliceFieldDataProvider.Create(AField: TfcxSliceField);
begin
  inherited Create;
  FField := AField;
  FTree := AField.BuildUVTree(AField.PopupShowAvailable);
end;

function TfcxSliceFieldDataProvider.GetNodeState(ANode: Pointer): TfcxCheckState;
var
  Index, GroupIndex: PtrInt;
begin
  Index := PtrInt(PfcxTreeNode(ANode)^.Data);
  case PfcxTreeNode(ANode)^.Tag and $FF00 of
    uvtUV:
      begin
        if FField.UVFilterType = uvft_Set then
          Result := BoolToNodeState[FField.UVFilterOf[Index]]
        else
          Result := BoolToNodeState[FField.UVSingleIndex = Index]
      end;
    uvtGroup:
      begin
        Result := TfcxCheckState(FField.UVFilterStateOfGroup[Index])
      end;
    uvtGroupUV:
      begin
        GroupIndex := PtrInt(PfcxTreeNode(ANode).Parent^.Data);
        if FField.UVFilterType = uvft_Set then
          Result := BoolToNodeState[FField.UVFilterOf[FField.GroupUVIndexByOrder[GroupIndex, Index]]]
        else
          Result := BoolToNodeState[FField.UVSingleIndex = FField.GroupUVIndexByOrder[GroupIndex, Index]]
      end;
    uvtNonGroupUV:
      begin
        if FField.UVFilterType = uvft_Set then
          Result := BoolToNodeState[FField.UVFilterOf[FField.NonGroupUVIndexByOrder[Index]]]
        else
          Result := BoolToNodeState[FField.UVSingleIndex = FField.NonGroupUVIndexByOrder[Index]]
      end;
    else
      Result := csChecked;
  end;
end;

function TfcxSliceFieldDataProvider.GetNodeText(ANode: Pointer): TfcxString;
var
  Index: PtrInt;
begin
  Index := PtrInt(PfcxTreeNode(ANode)^.Data);
  case PfcxTreeNode(ANode)^.Tag and $FF00 of
    uvtUV: Result := FField.UVCaption[Index];
    uvtGroup: Result := FField.GroupCaption[Index];
    uvtGroupUV: Result := FField.UVCaptionInGroup[PtrInt(PfcxTreeNode(ANode)^.Parent^.Data), Index];
    uvtNonGroupUV: Result := FField.UVCaptionInNonGroups[Index];
  end;
end;

function TfcxSliceFieldDataProvider.GetNodeKind(ANode: Pointer): TfcxNodeKind;
begin
  if FField.UVFilterType = uvft_Single then
    Result := nkRadio
  else
    Result := nkCheck;
  if Assigned(OnGetNodeKind) then
    OnGetNodeKind(Self, ANode, Result);
end;

procedure TfcxSliceFieldDataProvider.SetNodeState(ANode: Pointer; AState: TfcxCheckState);
var
  Index: PtrInt;
  Child: PfcxTreeNode;
begin
  Index := PtrInt(PfcxTreeNode(ANode)^.Data);
  case PfcxTreeNode(ANode)^.Tag and $FF00 of
    uvtUV:
      begin
        if FField.UVFilterType = uvft_Single then
        begin
          if Index <> FField.UVSingleIndex then
          begin
            NodeStateChanged(FTree.FindByData(Pointer(FField.UVSingleIndex)));
            FField.UVSingleIndex := Index;
            NodeStateChanged(ANode);
          end
        end
        else
        begin
          FField.UVFilterOf[Index] := AState = csChecked;
          NodeStateChanged(ANode);
        end;
      end;
    uvtGroup:
      begin
        FField.UVFilterStateOfGroup[Index] := TfcxCheckState(AState);
        NodeStateChanged(ANode);
        // update all children
        Child := PfcxTreeNode(ANode)^.FirstChild;
        while Assigned(Child) do
        begin
          NodeStateChanged(Child);
          Child := Child^.NextSibling;
        end;
      end;
    uvtGroupUV:
      begin
        FField.UVFilterOf[FField.GroupUVIndexByOrder[PtrInt(PfcxTreeNode(ANode)^.Parent^.Data), Index]] := AState = csChecked;
        NodeStateChanged(ANode);
        NodeStateChanged(PfcxTreeNode(ANode).Parent);
      end;
    uvtNonGroupUV:
      begin
        FField.UVFilterOf[FField.NonGroupUVIndexByOrder[Index]] := AState = csChecked;
        NodeStateChanged(ANode);
      end;
  end;
end;

procedure TfcxSliceFieldDataProvider.SetSingleCheck(ANode: Pointer);
var
  Index: PtrInt;
begin
  Index := PtrInt(PfcxTreeNode(ANode)^.Data);
  case PfcxTreeNode(ANode)^.Tag and $FF00 of
    uvtUV:
      FField.UVSingleIndex := Index;
    uvtGroup:
      FField.UVGroupSingleIndex := FField.GroupIndexByOrder[Index];
    uvtGroupUV:
      FField.UVSingleIndex := FField.GroupUVIndexByOrder[PtrInt(PfcxTreeNode(ANode)^.Parent^.Data), Index];
    uvtNonGroupUV:
      FField.UVSingleIndex := FField.NonGroupUVIndexByOrder[Index];
  end;
end;

procedure TfcxSliceFieldDataProvider.BeforePopup;
begin
  if Assigned(FField) and not FField.Slice.AutoUVFilter then
    FField.BeginUpdateFieldFilter;
end;

procedure TfcxSliceFieldDataProvider.AfterCloseup(const Cancel: Boolean);
begin
  if Assigned(FField) and not FField.Slice.AutoUVFilter then
  begin
    if Cancel then
      FField.RollBackFieldFilter
    else
      FField.EndUpdateFieldFilter;
  end;
  inherited;
end;

function TfcxSliceFieldDataProvider.IsTreeLike: Boolean;
begin
  Result := FField.HasGroups;
end;

destructor TfcxSliceFieldDataProvider.Destroy;
begin
  FTree.Free;
  inherited;
end;

{ TfcxSliceMeasureDataProvider }

constructor TfcxSliceMeasureDataProvider.Create(AContainer: TfcxMeasuresContainer);
begin
  inherited Create;
  FContainer := AContainer;
end;

function TfcxSliceMeasureDataProvider.GetFirstChild(ANode: Pointer): Pointer;
begin
  Result := nil;
end;

function TfcxSliceMeasureDataProvider.GetFirstNode: Pointer;
begin
  Result := GetVisibleByIndex(0);
end;

function TfcxSliceMeasureDataProvider.GetNextSibling(ANode: Pointer): Pointer;
begin
  Result := Pointer(PtrInt(ANode) + 1);
  if PtrInt(Result) > FContainer.Count then
    Result := nil;
end;

function TfcxSliceMeasureDataProvider.GetNodeState(ANode: Pointer): TfcxCheckState;
begin
  Result := BoolToNodeState[GetMeasure(ANode).Visible];
end;

function TfcxSliceMeasureDataProvider.GetNodeExpanded(ANode: Pointer): Boolean;
begin
  Result := True;
end;

function TfcxSliceMeasureDataProvider.GetNodeLevel(ANode: Pointer): Integer;
begin
  Result := 0;
end;

function TfcxSliceMeasureDataProvider.GetNodeText(ANode: Pointer): TfcxString;
begin
  Result := GetMeasure(ANode).Caption;
end;

function TfcxSliceMeasureDataProvider.GetPrevSibling(ANode: Pointer): Pointer;
begin
  if ANode <> nil then
    Result := Pointer(PtrInt(ANode) - 1)
  else
    Result := nil;
end;

function TfcxSliceMeasureDataProvider.GetVisibleNodeCount: Integer;
begin
  if Assigned(FContainer) then
    Result := FContainer.Count
  else
    Result := 0;
end;

function TfcxSliceMeasureDataProvider.GetVisibleByIndex(AIndex: Integer): Pointer;
begin
  if (AIndex >= 0) and (AIndex < FContainer.Count) then
    Result := Pointer(AIndex + 1)
  else
    Result := nil;
end;

function TfcxSliceMeasureDataProvider.GetVisibleIndex(ANode: Pointer): Integer;
begin
  Result := PtrInt(ANode) - 1;
end;

function TfcxSliceMeasureDataProvider.GetNodeKind(ANode: Pointer): TfcxNodeKind;
begin
  Result := nkCheck;
  if Assigned(OnGetNodeKind) then
    OnGetNodeKind(Self, ANode, Result);
end;

function TfcxSliceMeasureDataProvider.IsTreeLike: Boolean;
begin
  Result := False;
end;

procedure TfcxSliceMeasureDataProvider.SetNodeState(ANode: Pointer; AState: TfcxCheckState);
begin
  GetMeasure(ANode).Visible := AState = csChecked;
  NodeStateChanged(ANode);
end;

procedure TfcxSliceMeasureDataProvider.SetNodeExpanded(ANode: Pointer; AValue: Boolean);
begin
  // nothing this is not supported
end;

function TfcxSliceMeasureDataProvider.GetParent(ANode: Pointer): Pointer;
begin
  Result := nil;
end;

procedure TfcxSliceMeasureDataProvider.PerformDefaultNodeAction(ANode: Pointer);
begin
  with TfcxMeasureEditorForm.Create(nil) do
  begin
    Execute(FContainer.Slice, GetMeasure(ANode));
    Free;
  end;
end;

procedure TfcxSliceMeasureDataProvider.SetSingleCheck(ANode: Pointer);
var
  i: integer;
begin
  FContainer.Slice.BeginUpdate;
  for i := 0 to FContainer.Count - 1 do
    FContainer.Measures[i].Visible := i = PtrInt(ANode) - 1;
  FContainer.Slice.EndUpdate;
end;

function TfcxSliceMeasureDataProvider.GetMeasure(ANode: Pointer): TfcxMeasureField;
begin
  Result := FContainer.Measures[PtrInt(ANode) - 1];
end;

function TfcxSliceMeasureDataProvider.GetAllowDrag: Boolean;
begin
  Result := True;
end;

function TfcxSliceMeasureDataProvider.GetDragItem(ANode: Pointer): TObject;
begin
  Result := GetMeasure(ANode);
end;

end.
