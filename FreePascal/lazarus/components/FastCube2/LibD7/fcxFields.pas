{*******************************************************}
{                                                       }
{     FastCube 2 fields selection and editing form      }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxFields;

interface

{$INCLUDE fcx.inc}
uses
{$IFDEF FPC}
  LCLType, LResources,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus,
  fcxTypes, fcxControl, fcxAlerts, fcxSlice;

type
  TfcxNodeType = (
    ntCube,
    ntField,
    ntSubField
  );

  TfcxNodeData = record
    NodeType: TfcxNodeType;
    Field: TfcxSliceField;
  end;
  PfcxNodeData = ^TfcxNodeData;

  TfcxFieldEditor = class;
  TfcxFieldForm = class(TForm)
    FieldTree: TTreeView;
    Panel1: TPanel;
    Panel2: TPanel;
    AddBtn: TButton;
    AreaList: TComboBox;
    TreeMenu: TPopupMenu;
    ItemRename: TMenuItem;
    procedure FieldTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure AreaListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FieldTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FieldTreeChange(Sender: TObject; Node: TTreeNode);
    procedure AddBtnClick(Sender: TObject);
    procedure FieldTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure ItemRenameClick(Sender: TObject);
    procedure TreeMenuPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDragObject: TDragObject;
    procedure DoChange;
  public
    Editor: TfcxFieldEditor;
    procedure UpdateCaptions;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TfcxFieldEditor = class(TComponent)
  private
    FForm: TfcxFieldForm;
    FSlice: TfcxSlice;
    procedure SetSlice(const Value: TfcxSlice);
    procedure Update;
    function GetFieldName: String;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SliceChanged(Alert: TfcxChangeAlert);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure Show;
    procedure Hide;
    property FieldName: String read GetFieldName;
  published
    property Slice: TfcxSlice read FSlice write SetSlice;
  end;

implementation

uses
  fcxRes,
  fcxGraphicRes;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

type
  TfcxFieldDragItem = class(TfcxCustomDragItem)
  private
    FItem: TObject;
  public
    function GetItem: TObject; override;
    procedure InitDrag(AItem: TObject);
  end;

{ TfcxFieldEditor }

constructor TfcxFieldEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSlice := nil;
  FForm := TfcxFieldForm.Create(nil);
  FForm.Editor := Self;
  FForm.Visible := False;
end;

destructor TfcxFieldEditor.Destroy;
begin
  Slice := nil;
  FForm.Free;
  inherited;
end;

function TfcxFieldEditor.GetFieldName: String;
begin
  if (FForm.FieldTree.Selected <> nil) and (FForm.FieldTree.Selected.Level = 2) then
    Result := StrPas(PChar(FForm.FieldTree.Selected.Data))
  else
    Result := '';
end;

procedure TfcxFieldEditor.SliceChanged(Alert: TfcxChangeAlert);
begin
  if Assigned(FForm) and FForm.Visible then
    Update;
end;

procedure TfcxFieldEditor.SetSlice(const Value: TfcxSlice);
begin
  if FSlice <> Value then
  begin
    if Slice <> nil then
    begin
      Slice.RemoveFreeNotification(Self);
      Slice.ListnersManager.RemoveListner(Self);
    end;
    FSlice := Value;
    if Slice <> nil then
    begin
      Slice.FreeNotification(Self);
      Slice.ListnersManager.AddListner(Self);
    end;
    Update;
  end;
end;

procedure TfcxFieldEditor.Show;
begin
  if not FForm.Visible then
  begin
    Update;
    FForm.Show;
  end;
  FForm.BringToFront;
end;

procedure TfcxFieldEditor.Update;

  function NewData(ANodeType: TfcxNodeType): Pointer;
  var
    NodeData: PfcxNodeData;
  begin
    New(NodeData);
    NodeData^.NodeType := ANodeType;
    Result := NodeData;
  end;

  function AddField(AField: TfcxSliceField; AParent: TTreeNode): TTreeNode;
  begin
    Result := FForm.FieldTree.Items.AddChild(AParent, StringToControl(AField.Caption));
    Result.ImageIndex := 2;
    Result.SelectedIndex := 2;
    Result.Data := NewData(ntField);
    PfcxNodeData(Result.Data)^.Field := AField;
  end;

  procedure TraverseAdd(ANode: PfcxTreeNode; AParent: TTreeNode);
  var
    Node: TTreeNode;
  begin
    if Assigned(ANode) then
    begin
      Node := AddField(ANode^.Data, AParent);
      TraverseAdd(ANode^.FirstChild, Node);
      TraverseAdd(ANode^.NextSibling, AParent);
    end;
  end;

var
  Node: TTreeNode;
  Tree: TfcxTree;
begin
  FForm.FieldTree.Items.Clear;
  FForm.FieldTree.Items.BeginUpdate;

  // create the root node - it has the same caption as cube
  if Assigned(Slice) and Assigned(Slice.Cube) and (Slice.Cube.Caption <> '') then
    Node := FForm.FieldTree.Items.AddChild(nil, StringToControl(Slice.Cube.Caption))
  else
    Node := FForm.FieldTree.Items.AddChild(nil, fcxResources.Get('sDefCubeCaption'));
  Node.ImageIndex    := 0;
  Node.SelectedIndex := 0;
  Node.Data := NewData(ntCube);

  // add children
  if Assigned(Slice) then
  begin
    Tree := Slice.SliceFields.BuildTree;
    try
      TraverseAdd(Tree.First, Node);
    finally
      Tree.Free;
    end;
  end;

  if FForm.AreaList.ItemIndex < 0 then
    FForm.AreaList.ItemIndex := 0;

  // expend the root node (cube)  
  Node.Expand(True);
  FForm.FieldTree.Items.EndUpdate;
  FForm.DoChange;
end;

procedure TfcxFieldForm.FieldTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
var
  Data: PfcxNodeData;
begin
  Data := PfcxNodeData(FieldTree.Selected.Data);
  if not Assigned(Data.Field) then
    Abort;

  if FDragObject = nil then
    FDragObject := TfcxFieldDragItem.Create(Self);
  TfcxFieldDragItem(FDragObject).InitDrag(Data.Field);
  DragObject := FDragObject;
end;

procedure TfcxFieldForm.AreaListDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  S: String;
begin
  S := AreaList.Items[Index];
  with (Control as TCombobox) do
  begin
    Canvas.FillRect(Rect);
    if odSelected in State then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := clWindowText;
    inc(Rect.Left, 2);                           
    if S <> '' then
      Canvas.TextRect(Rect, Rect.Left, (Rect.Bottom + Rect.Top - Canvas.TextHeight(S)) shr 1, S);
  end;
end;

constructor TfcxFieldForm.Create(AOwner: TComponent);
begin
  inherited;
  FDragObject := nil;
end;

procedure TfcxFieldForm.FieldTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FieldTree.Selected <> nil) and (PfcxNodeData(FieldTree.Selected.Data)^.NodeType = ntField) then
    FieldTree.BeginDrag(False) else
  if (Button = mbRight) then
    FieldTree.Selected := FieldTree.GetNodeAt(X, Y);
end;

procedure TfcxFieldForm.FieldTreeChange(Sender: TObject; Node: TTreeNode);
begin
  DoChange;
end;

destructor TfcxFieldForm.Destroy;
begin
  FDragObject.Free;
  inherited;
end;

procedure TfcxFieldForm.DoChange;
begin
  AddBtn.Enabled := (FieldTree.Selected <> nil) and (PfcxNodeData(FieldTree.Selected.Data)^.NodeType = ntField);
end;

procedure TfcxFieldForm.AddBtnClick(Sender: TObject);
var
  Data: PfcxNodeData;
begin
  if (FieldTree.Selected = nil) then
    Exit;
  Data := PfcxNodeData(FieldTree.Selected.Data);
  if Data^.NodeType <> ntField then
    Exit;
    
  case AreaList.ItemIndex of
    0: Editor.FSlice.PageContainer.AddFilterField(Data^.Field, Data^.Field.FieldName, Data^.Field.Caption);
    1: Editor.FSlice.YAxisContainer.AddDimension(Data^.Field, Data^.Field.FieldName, Data^.Field.Caption);
    2: Editor.FSlice.XAxisContainer.AddDimension(Data^.Field, Data^.Field.FieldName, Data^.Field.Caption);
    3: Editor.FSlice.MeasuresContainer.AddMeasure(Data^.Field, Data^.Field.FieldName, Data^.Field.Caption, af_Sum);
  end;
end;

procedure TfcxFieldForm.FieldTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
    Dispose(PfcxNodeData(Node.Data));
end;

procedure TfcxFieldForm.ItemRenameClick(Sender: TObject);
var
  Value: String;
  Data: PfcxNodeData;
begin
  Data := PfcxNodeData(FieldTree.Selected.Data);
  case Data^.NodeType of
    ntCube:
      begin
        Value := Editor.FSlice.Cube.Caption;
        if InputQuery(fcxResources.Get('sCubeChangeCaption'), fcxResources.Get('sCubeChangePropmpt'), Value) then
          Editor.FSlice.Cube.Caption := String(Value);
      end;
    ntField:
      begin
        Value := Data^.Field.Caption;
        if InputQuery(fcxResources.Get('sFieldLabelChangeCaption'), fcxResources.Get('sFieldLabelChangePropmpt'), Value) then
        begin
          Editor.FSlice.BeginUpdate;
          Data^.Field.Caption := Value;
          Editor.FSlice.EndUpdate;
        end;
      end;
  end;
end;

procedure TfcxFieldForm.TreeMenuPopup(Sender: TObject);
begin
  ItemRename.Enabled := (FieldTree.Selected <> nil);
end;

procedure TfcxFieldForm.UpdateCaptions;
begin
  Caption := fcxResources.Get('sFieldFormCaption');
  Panel2.Caption := fcxResources.Get('sFieldFormDragLabel');
  AddBtn.Caption := fcxResources.Get('sFieldFormAddBtn');
  AreaList.Items.Text := fcxResources.Get('sFieldFormAreaText');
  AreaList.Items.Text := StringReplace(AreaList.Items.Text, '''#$D#$A''', #13#10, [rfReplaceAll, rfIgnoreCase]);
  ItemRename.Caption := fcxResources.Get('sFieldFormRename');
  FieldTree.Images := fcxGraphicResources.FieldsImages;
end;

procedure TfcxFieldForm.FormCreate(Sender: TObject);
begin
  UpdateCaptions;
end;

procedure TfcxFieldEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FSlice) then
  begin
    FSlice := nil;
    Update;
  end;
end;

function TfcxFieldEditor.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if Action.Owner is TfcxSlice then
      SliceChanged(TfcxAction(Action).ChangeAlert);
    Result := True;
  end
  else
    Result := False;
end;

procedure TfcxFieldEditor.Hide;
begin
  FForm.Hide;
end;

{ TfcxFieldDragItem }

function TfcxFieldDragItem.GetItem: TObject;
begin
  Result := FItem;
end;

procedure TfcxFieldDragItem.InitDrag(AItem: TObject);
begin
  if not InDrag then
  begin
    FItem := AItem;
    inherited InitDrag(nil, nil);
  end;
end;

end.
