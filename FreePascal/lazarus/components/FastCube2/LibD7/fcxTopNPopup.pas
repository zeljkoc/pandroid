{*******************************************************}
{                                                       }
{              FastCube 2 topN Popup unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxTopNPopup;

interface

{$INCLUDE fcx.inc}

uses
  Classes, Controls, StdCtrls, Graphics,
  fcxSlice, fcxPopupProvider, fcxPopupWindow,
  fcxTypes, fcxPainters, fcxGridPainters;

type
  TfcxTopNDataProvider = class(TfcxCustomTreeProvider)
  private
    FSlice: TfcxSlice;
  public
    function GetNodeKind(ANode: Pointer): TfcxNodeKind; override;
    function GetNodeText(ANode: Pointer): TfcxString; override;
    function IsTreeLike: Boolean; override;

    // own members
    constructor Create(ASlice: TfcxSlice);
    destructor Destroy; override;
  end;

  TfcxTopNPopup = class(TfcxCustomNodePopup)
  private
    procedure SetSlice(const Value: TfcxSlice);
    function GetDataProvider: TfcxTopNDataProvider;
    procedure SetDataProvider(const Value: TfcxTopNDataProvider); reintroduce;
  protected
    procedure CancelClick(Sender: TObject);
    procedure ClearAllClick(Sender: TObject);
    procedure InverseClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure SelectAllClick(Sender: TObject);
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
    property Slice: TfcxSlice write SetSlice;
    property DataProvider: TfcxTopNDataProvider read GetDataProvider write SetDataProvider;
  end;

implementation

uses
  fcxRes;

{ TfcxCustomFilterPopup }

constructor TfcxTopNPopup.Create(AOwner: TComponent);
begin
  inherited;
  Width := 160;
end;

procedure TfcxTopNPopup.SetSlice(const Value: TfcxSlice);
begin
  DataProvider.Free;
  DataProvider := TfcxTopNDataProvider.Create(Value);
  UpdateButtons;
end;

{ TfcxSliceFieldsDataProvider }

constructor TfcxTopNDataProvider.Create(ASlice: TfcxSlice);
begin
  inherited Create;
  FSlice := ASlice;
  if Assigned(FSlice) then
  begin
    FTree := FSlice.TopNs.BuildTree;
    SetAllNodeExpanded(True);
  end;
end;

function TfcxTopNDataProvider.GetNodeText(ANode: Pointer): TfcxString;
begin
  Result := TfcxTopNProcessor(PfcxTreeNode(ANode)^.Data).AsString;
end;

destructor TfcxTopNDataProvider.Destroy;
begin
  FTree.Free;
  inherited;
end;

procedure TfcxTopNPopup.CancelClick(Sender: TObject);
begin
  CloseUp(True);
end;

procedure TfcxTopNPopup.ClearAllClick(Sender: TObject);
begin
  DataProvider.SetAllNodeState(csUnchecked);
end;

procedure TfcxTopNPopup.InverseClick(Sender: TObject);
begin
  DataProvider.InverseNodeState;
end;

procedure TfcxTopNPopup.OkClick(Sender: TObject);
begin
  CloseUp(False);
end;

procedure TfcxTopNPopup.SelectAllClick(Sender: TObject);
begin
  DataProvider.SetAllNodeState(csChecked);
end;

procedure TfcxTopNPopup.UpdateButtons;
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

function TfcxTopNPopup.GetDataProvider: TfcxTopNDataProvider;
begin
  Result := TfcxTopNDataProvider(inherited DataProvider)
end;

procedure TfcxTopNPopup.SetDataProvider(const Value: TfcxTopNDataProvider);
begin
  inherited SetDataProvider(Value);
end;

function TfcxTopNDataProvider.IsTreeLike: Boolean;
begin
  Result := False;
end;

function TfcxTopNDataProvider.GetNodeKind(ANode: Pointer): TfcxNodeKind;
begin
  Result := nkCheck;
end;

end.
