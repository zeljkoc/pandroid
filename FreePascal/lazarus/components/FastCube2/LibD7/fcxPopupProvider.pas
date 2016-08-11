{*******************************************************}
{                                                       }
{          FastCube 2 Popup Providers unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
//VCL uses section
{$IFNDEF FMX}
unit fcxPopupProvider;
{$INCLUDE fcx.inc}

interface
uses
  SysUtils, Classes,  
  fcxTypes;
//FMX uses
{$ELSE}
{$INCLUDE fcx.inc}

interface
uses 
  System.SysUtils, System.Classes,
  FMX.fcxTypes;
{$ENDIF}

type  
  // abstract classes to implement tree-like popup
  
  TfcxNodeKind = (
    nkSimple,
    nkCheck,
    nkRadio
  );

  TfcxNotifyNodeEvent = procedure(Sender: TObject; ANode: Pointer) of Object;
  TfcxGetNodeKindEvent = procedure(Sender: TObject; ANode: Pointer; var NodeKind: TfcxNodeKind) of Object;
  TfcxNodeHandler = procedure(Sender: TObject; ANode: Pointer; AData: Pointer) of Object;
  TfcxAfterCloseUpEvent = procedure(Sender: TObject; Cancel: Boolean) of Object;

  TfcxCustomNodeProvider = class
  private
    FOnExpandChange: TNotifyEvent;
    FOnStateChange: TfcxNotifyNodeEvent;
    FOnGetNodeKind: TfcxGetNodeKindEvent;
    FOnAfterCloseUp: TfcxAfterCloseUpEvent;
  protected
    procedure NodeStateChanged(ANode: Pointer);
    function GetAllowDrag: Boolean; virtual;
  public
    function GetDragItem(ANode: Pointer): TObject; virtual;
    function GetFirstNode: Pointer; virtual; abstract;
    function GetNextSibling(ANode: Pointer): Pointer; virtual; abstract;
    function GetPrevSibling(ANode: Pointer): Pointer; virtual; abstract;
    function GetFirstChild(ANode: Pointer): Pointer; virtual; abstract;
    function GetParent(ANode: Pointer): Pointer; virtual; abstract;
    function GetVisibleByIndex(AIndex: Integer): Pointer; virtual; abstract;
    function GetVisibleIndex(ANode: Pointer): Integer; virtual; abstract;
    function GetNodeExpanded(ANode: Pointer): Boolean; virtual; abstract;
    function GetNodeLevel(ANode: Pointer): Integer; virtual; abstract;
    function GetVisibleNodeCount: Integer; virtual; abstract;

    function IsTreeLike: Boolean; virtual; abstract;
    function GetNodeText(ANode: Pointer): TfcxString; virtual; abstract;
    procedure SetNodeExpanded(ANode: Pointer; AValue: Boolean); virtual; abstract;

    // check support
    function GetNodeKind(ANode: Pointer): TfcxNodeKind; virtual;
    function GetNodeState(ANode: Pointer): TfcxCheckState; virtual; abstract;
    procedure SetNodeState(ANode: Pointer; AState: TfcxCheckState); virtual; abstract;
    procedure SetSingleCheck(ANode: Pointer); virtual; abstract;

    // misc
    procedure BeforePopup; virtual;
    procedure AfterCloseup(const Cancel: Boolean); virtual;
    procedure PerformDefaultNodeAction(ANode: Pointer); virtual;
    function SearchNode(StartNodeText: String): Pointer; virtual;

    property AllowDrag: Boolean read GetAllowDrag;
    property NodeExpanded[ANode: Pointer]: Boolean read GetNodeExpanded write SetNodeExpanded;
    property NodeState[ANode: Pointer]: TfcxCheckState read GetNodeState write SetNodeState;

    property OnExpandChange: TNotifyEvent read FOnExpandChange write FOnExpandChange;
    property OnStateChange: TfcxNotifyNodeEvent read FOnStateChange write FOnStateChange;
    property OnGetNodeKind: TfcxGetNodeKindEvent read FOnGetNodeKind write FOnGetNodeKind;
    property OnAfterCloseUp: TfcxAfterCloseUpEvent read FOnAfterCloseUp write FOnAfterCloseUp;
  end;

  TfcxCustomTreeProvider = class(TfcxCustomNodeProvider)
  protected
    // this tree needs to be assigned from descendent
    FTree: TfcxTree;
  public
    // overrides
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
    function GetNodeState(ANode: Pointer): TfcxCheckState; override;

    function IsTreeLike: Boolean; override;
    procedure SetNodeExpanded(ANode: Pointer; AValue: Boolean); override;
    procedure SetNodeState(ANode: Pointer; AState: TfcxCheckState); override;
    procedure SetSingleCheck(ANode: Pointer); override;

    procedure SetAllNodeExpanded(AValue: Boolean);
    procedure SetAllNodeState(AState: TfcxCheckState);
    procedure InverseNodeState;
    procedure ForEach(Handler: TfcxNodeHandler);
  end;

const
  // node states
  stExpanded = 1;
  stChecked  = 2;
  stGrayed   = 4;

implementation

{ TfcxCustomNodeProvider }

procedure TfcxCustomNodeProvider.AfterCloseup(const Cancel: Boolean);
begin
  if Assigned(OnAfterCloseUp) then
    OnAfterCloseUp(Self, Cancel);
end;

procedure TfcxCustomNodeProvider.BeforePopup;
begin
end;

function TfcxCustomNodeProvider.GetAllowDrag: Boolean;
begin
  Result := False;
end;

function TfcxCustomNodeProvider.GetDragItem(ANode: Pointer): TObject;
begin
  Result := nil;
end;

function TfcxCustomNodeProvider.GetNodeKind(ANode: Pointer): TfcxNodeKind;
begin
  Result := nkSimple;
  if Assigned(OnGetNodeKind) then
    OnGetNodeKind(Self, ANode, Result);
end;

procedure TfcxCustomNodeProvider.NodeStateChanged(ANode: Pointer);
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self, ANode)
end;

procedure TfcxCustomNodeProvider.PerformDefaultNodeAction(ANode: Pointer);
begin
end;

function TfcxCustomNodeProvider.SearchNode(StartNodeText: String): Pointer;

  function Traverse(Start: Pointer): Pointer;
  begin
    Result := nil;
    if Start = nil then
      Exit;
    if Pos(StartNodeText, AnsiLowerCase(GetNodeText(Start))) = 1 then
    begin
      Result := Start;
      Exit;
    end;
    Result := Traverse(GetFirstChild(Start));
    if Result = nil then
      Result := Traverse(GetNextSibling(Start));
  end;

begin
  StartNodeText := AnsiLowerCase(StartNodeText);
  Result := Traverse(GetFirstNode);
end;

{ TfcxCustomTreeProvider }

function TfcxCustomTreeProvider.GetFirstChild(ANode: Pointer): Pointer;
begin
  Result := PfcxTreeNode(ANode)^.FirstChild;
end;

function TfcxCustomTreeProvider.GetFirstNode: Pointer;
begin
  if Assigned(FTree) then
    Result := FTree.First
  else
    Result := nil;
end;

function TfcxCustomTreeProvider.GetNextSibling(ANode: Pointer): Pointer;
begin
  Result := PfcxTreeNode(ANode)^.NextSibling;
end;

function TfcxCustomTreeProvider.GetPrevSibling(ANode: Pointer): Pointer;
begin
  Result := PfcxTreeNode(ANode)^.PrevSibling;
end;

function TfcxCustomTreeProvider.GetVisibleByIndex(AIndex: Integer): Pointer;

  function Traverse(ANode: PfcxTreeNode): Pointer;
  begin
    Result := nil;
    if Assigned(ANode) then
    begin
      if AIndex = 0 then
        Result := ANode;
      if Result = nil then
      begin
        dec(AIndex);
        if NodeExpanded[ANode] then
          Result := Traverse(Anode^.FirstChild);
        if Result = nil then
          Result := Traverse(ANode^.NextSibling);
      end;
    end;
  end;

begin
  Result := Traverse(FTree.First);
end;

function TfcxCustomTreeProvider.GetVisibleIndex(ANode: Pointer): Integer;

var
  Found: Boolean;

  procedure Traverse(Node: PfcxTreeNode);
  begin
    if Assigned(Node) then
    begin
      if ANode = Node then
        Found := True;
      if not Found then
      begin
        inc(Result);
        if NodeExpanded[Node] then
          Traverse(Node^.FirstChild);
        if not Found then
          Traverse(Node^.NextSibling);
      end;
    end;
  end;

begin
  if Assigned(ANode) then
  begin
    Result := 0;
    Found := False;
    Traverse(FTree.First);
  end
  else
    Result := -1;
end;

function TfcxCustomTreeProvider.GetNodeExpanded(ANode: Pointer): Boolean;
begin
  Result := (PfcxTreeNode(ANode)^.Tag and stExpanded) <> 0;
end;

procedure TfcxCustomTreeProvider.SetNodeExpanded(ANode: Pointer; AValue: Boolean);
const
  BoolToNodeState: array[Boolean] of Integer = (
    0,
    stExpanded
  );
begin
  if GetNodeExpanded(ANode) <> AValue then
  begin
    PfcxTreeNode(ANode)^.Tag := (PfcxTreeNode(ANode)^.Tag and not stExpanded) or BoolToNodeState[AValue];
    if Assigned(OnExpandChange) then
      OnExpandChange(Self);
  end;
end;

function TfcxCustomTreeProvider.GetNodeLevel(ANode: Pointer): Integer;
begin
  Result := 0;
  while PfcxTreeNode(ANode)^.Parent <> FTree.Root do
  begin
    ANode := PfcxTreeNode(ANode)^.Parent;
    inc(Result);
  end;
end;

function TfcxCustomTreeProvider.IsTreeLike: Boolean;
begin
  Result := True;
end;

function TfcxCustomTreeProvider.GetVisibleNodeCount: Integer;
  procedure Traverse(ANode: PfcxTreeNode);
  var
    Node: PfcxTreeNode;
  begin
    Node := ANode.FirstChild;
    while Assigned(Node) do
    begin
      inc(Result);
      if NodeExpanded[Node] and Assigned(Node.FirstChild) then
        Traverse(Node);
      Node := Node.NextSibling;
    end;
  end;
begin
  Result := 0;
  if Assigned(FTree) then
    Traverse(FTree.Root);
end;

function TfcxCustomTreeProvider.GetParent(ANode: Pointer): Pointer;
begin
  Result := PfcxTreeNode(ANode)^.Parent;
end;

function TfcxCustomTreeProvider.GetNodeState(ANode: Pointer): TfcxCheckState;
begin
  if (PfcxTreeNode(ANode)^.Tag and stGrayed) <> 0 then
    Result := csGrayed
  else
  if (PfcxTreeNode(ANode)^.Tag and stChecked) <> 0 then
    Result := csChecked
  else
    Result := csUnchecked;
end;

procedure TfcxCustomTreeProvider.SetNodeState(ANode: Pointer; AState: TfcxCheckState);
const
  CheckStateToNodeState: array[TfcxCheckState] of Integer = (
 { cbUnchecked } 0,
 { cbChecked   } stChecked,
 { cbGrayed    } stGrayed
  );
begin
  if GetNodeState(ANode) <> AState then
  begin
    PfcxTreeNode(ANode)^.Tag := (PfcxTreeNode(ANode)^.Tag and not (stGrayed or stChecked)) or CheckStateToNodeState[AState];
    NodeStateChanged(ANode);
  end;
end;

procedure TfcxCustomTreeProvider.SetAllNodeExpanded(AValue: Boolean);

  procedure Expand(ANode: Pointer);
  begin
    if Assigned(ANode) then
    begin
      SetNodeExpanded(ANode, AValue);
      Expand(GetFirstChild(ANode));
      Expand(GetNextSibling(ANode));
    end;
  end;

begin
  Expand(GetFirstNode);
end;

procedure TfcxCustomTreeProvider.SetSingleCheck(ANode: Pointer);

  procedure Check(Cur: Pointer);
  begin
    if Assigned(Cur) then
    begin
      if ANode = Cur then
        NodeState[Cur] := csChecked
      else
        NodeState[Cur] := csUnchecked;
      Check(GetFirstChild(Cur));
      Check(GetNextSibling(Cur))
    end;
  end;

begin
  Check(GetFirstNode);
end;

procedure TfcxCustomTreeProvider.SetAllNodeState(AState: TfcxCheckState);

  procedure SetState(ANode: Pointer);
  begin
    if Assigned(ANode) then
    begin
      SetNodeState(ANode, AState);
      SetState(GetFirstChild(ANode));
      SetState(GetNextSibling(ANode));
    end;
  end;

begin
  SetState(GetFirstNode);
end;

procedure TfcxCustomTreeProvider.InverseNodeState;

const
  InverseMap: array[TfcxCheckState] of TfcxCheckState = (
  { cbUnchecked } csChecked,
  { cbChecked   } csUnchecked,
  { cbGrayed    } csGrayed
  );

  procedure SetState(ANode: Pointer);
  begin
    if Assigned(ANode) then
    begin
      NodeState[ANode] := InverseMap[NodeState[ANode]];
      SetState(GetFirstChild(ANode));
      SetState(GetNextSibling(ANode));
    end;
  end;

begin
  SetState(GetFirstNode);
end;

procedure TfcxCustomTreeProvider.ForEach(Handler: TfcxNodeHandler);

  procedure Run(ANode: Pointer);
  begin
    if Assigned(ANode) then
    begin
      Handler(Self, ANode, PfcxTreeNode(ANode)^.Data);
      Run(GetFirstChild(ANode));
      Run(GetNextSibling(ANode));
    end;
  end;

begin
  Run(GetFirstNode);
end;

end.
