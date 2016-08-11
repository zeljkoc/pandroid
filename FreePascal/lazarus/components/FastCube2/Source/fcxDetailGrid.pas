{*******************************************************}
{                                                       }
{         FastCube 2 slice detail grid unit             }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxDetailGrid;

interface

{$INCLUDE fcx.inc}

uses
  {$ifndef fpc}
  Windows,
  {$else}
  LCLType, LCLIntf,
  {$endif}
{$IFDEF DELPHI_6UP}
  Types,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  fcxTypes, fcxRes, fcxZone, fcxPainters, fcxGridPainters, fcxCustomGrid,
  fcxSlice, fcxCube, fcxCubeGrid, fcxPopupWindow, fcxCubeGridToolBar;

type
  TfcxDetailGrid = class;

  TfcxDetailDataZone = class(TfcxCubeDataZone)
  private
    function GetGrid: TfcxDetailGrid;
  protected
    procedure FullUpdate; override;
    function ValidData: Boolean; override;
    function CreateOrderedRecordSetProvider: TfcxOrderedRecordSetProvider; override;
    property Grid: TfcxDetailGrid read GetGrid;
  end;

  TfcxDetailGrid = class(TfcxCubeGrid)
  private
    FSlice: TfcxSlice;
    FPosition: TPoint;
    procedure SetSlice(const Value: TfcxSlice);
    procedure SetPosition(const Value: TPoint);
  protected
    function GetDataZoneClass: TfcxCubeDataZoneClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Position: TPoint read FPosition write SetPosition;
  published
    property Align;
    property Slice: TfcxSlice read FSlice write SetSlice;
    property PaintStyle;
    property Styles;
    property TabStop;
    property TabOrder;
  end;

  procedure ShowDetails(ASlice: TfcxSlice; APaintStyle: TfcxPaintStyle; P: TPoint; AColumns: TfcxCubeDataColumns = nil);

implementation

uses
  fcxFieldsPopup;

procedure ShowDetails(ASlice: TfcxSlice; APaintStyle: TfcxPaintStyle; P: TPoint; AColumns: TfcxCubeDataColumns = nil);
var
  F: TForm;
  G: TfcxDetailGrid;
  Tb: TfcxCubeGridToolbar;
begin
  F := TForm.Create(Application);
  F.Caption := fcxResources.Get('sDetailGridCaption');
  F.Position := poOwnerFormCenter;
  F.Width := 500;

  // create a toolbar with only one button
  Tb := TfcxCubeGridToolbar.Create(F);
  Tb.Parent := F;
  Tb.Align := alTop;
  Tb.AutoSize := True;
  Tb.ShowHint := True;

  // create a detail grid
  G := TfcxDetailGrid.Create(F);
  G.Slice := ASlice;
  G.Position := P;
  G.PaintStyle := APaintStyle;
  G.Parent := F;
  G.Align := alClient;

  if Assigned(AColumns) then
    G.DataZone.Columns := AColumns;

  Tb.CubeGrid := G;

  F.Constraints.MinHeight := 70;
  F.Constraints.MinWidth := 30;
  F.ActiveControl := G;
  F.ShowModal;
  if Assigned(AColumns) then
    AColumns.Assign(G.DataZone.Columns);
  F.Free;
end;

{ TfcxDetailGrid }

constructor TfcxDetailGrid.Create(AOwner: TComponent);
begin
  FPosition := Point(-1, -1);
  inherited;
end;

destructor TfcxDetailGrid.Destroy;
begin
  Slice := nil;
  inherited;
end;

function TfcxDetailGrid.GetDataZoneClass: TfcxCubeDataZoneClass;
begin
  Result := TfcxDetailDataZone;
end;

procedure TfcxDetailGrid.SetPosition(const Value: TPoint);
begin
  if (FPosition.X <> Value.X) or
     (FPosition.Y <> Value.Y) then
  begin
    FPosition := Value;
    FullUpdate;
  end;
end;

procedure TfcxDetailGrid.SetSlice(const Value: TfcxSlice);
begin
  if FSlice <> Value then
  begin
    if Assigned(Slice) then
      Slice.ListnersManager.RemoveListner(Self);
    FSlice := Value;
    if Assigned(FSlice) then
    begin
      Cube := FSlice.Cube;
      FSlice.ListnersManager.AddListner(Self);
    end
    else
      Cube := nil;
  end;
end;

{ TfcxDetailDataZone }

function TfcxDetailDataZone.CreateOrderedRecordSetProvider: TfcxOrderedRecordSetProvider;
begin
  Result := TfcxDetailOrderedRecordSetProvider.Create(Grid.Slice);
  TfcxDetailOrderedRecordSetProvider(Result).SetPosition(Grid.Position);
end;

procedure TfcxDetailDataZone.FullUpdate;
begin
  if ValidData then
  begin
    TfcxDetailOrderedRecordSetProvider(OrderedRecordSetProvider).SetSlice(Grid.Slice);
    TfcxDetailOrderedRecordSetProvider(OrderedRecordSetProvider).SetPosition(Grid.Position);
    SetLength(FRowHeights, GetRowCount);
  end
  else
  begin
    TfcxDetailOrderedRecordSetProvider(OrderedRecordSetProvider).SetSlice(nil);
    SetLength(FRowHeights, 0);
  end;

  UpdateScrolls(True, True);
end;

function TfcxDetailDataZone.GetGrid: TfcxDetailGrid;
begin
  Result := TfcxDetailGrid(Owner);
end;

function TfcxDetailDataZone.ValidData: Boolean;
begin
  with Grid do
    Result := Assigned(Slice) and (Position.X <> -1) and (Position.Y <> -1);
end;

end.
