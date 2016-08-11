{*******************************************************}
{                                                       }
{            FastCube 2 debug axis preview form         }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxAxis;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ComCtrls, ToolWin, db, ImgList, fcxSlice;

type
  TfcxAxisShow = class(TForm)
    ToolBar1: TToolBar;
    btnExit: TToolButton;
    StatusBar1: TStatusBar;
    HierTree: TVirtualStringTree;
    ImageList1: TImageList;
    Images: TImageList;
    procedure btnExitClick(Sender: TObject);
    procedure HierTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure HierTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormCreate(Sender: TObject);
    procedure HierTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FAxis: TfcxAxisContainer;
    procedure MakeTree;
  public
    procedure ShowAxis(ACaption : String; AAxis: TfcxAxisContainer);
  end;

  THierRecord = record
    Caption  : String;
    ImageIndex : Byte;
  end;
  PHierRecord = ^THierRecord;

  procedure ShowAxis(ACaption : String; AAxis : TfcxAxisContainer);

implementation

{$R *.DFM}
uses
  math;


procedure TfcxAxisShow.MakeTree;
var
  i, ALevel: integer;

function DopInfo(Level, CurIndex: Integer; var Index : byte) : String;
const
  ResTemplate = '(%s, P = %d, F = %d, L = %d)';
var
  State: byte;
  strState: String;
  AxisValue: PfcxAxisTreeNodeOne;
begin
  AxisValue := @(FAxis.AxisTree[Level].nodes[CurIndex]);
//  State := AxisValue.State;
  strState := '';
  Index := 0;
  if FAxis.AxisTree[Level].MultiLevel then
    strState := strState + 'M'
  else
    strState := strState + 'O';
{
  if (State and stTotal) <> 0 then
  begin
    strState := strState + 'T';
    Index := Index or $01;
  end;
  if (State and stExpand) <> 0 then
  begin
    strState := strState + 'E';
    Index := Index or $02;
  end;
  if (State and stVisible) <> 0 then
  begin
    strState := strState + 'V';
    Index := Index or $04;
  end;
  if (State and stLExpand) <> 0 then
  begin
    strState := strState + 'L';
    Index := Index or $08;
  end;
  if (State and stBExpand) <> 0 then
  begin
    strState := strState + 'B';
    Index := Index or $10;
  end;
  if (State and stZero) <> 0 then
  begin
    strState := strState + 'Z';
    Index := Index or $20;
  end;
}
  Result := Format(ResTemplate, [strState, AxisValue.IndParent, AxisValue.IndFirst, AxisValue.IndLast]);
end;

procedure Obhod(Level, CurIndex : Integer; CurParent : PVirtualNode);
var
  Node : PVirtualNode;
  i: integer;
begin

  Node := HierTree.AddChild(CurParent);
  Node.States := Node.States + [vsInitialUserData];
  with PHierRecord(HierTree.GetNodeData(Node))^ do
    Caption := IntToStr(CurIndex) + ' : ' + FAxis.TESTGetDisplayValue(level, CurIndex){inttostr(FAxis.FAxisTree[level].nodes[CurIndex].Value)}{FAxis.DisplayValue[Level, CurIndex]} + ' ' + DopInfo(Level, CurIndex, ImageIndex);

  if (Level = (FAxis.LevelCount - 1)) then
    Exit;
  for i := FAxis.AxisTree[Level].Nodes[CurIndex].IndFirst to FAxis.AxisTree[Level].Nodes[CurIndex].IndLast do
  begin
    Obhod(Level + 1, i, Node);
  end;
end;

begin
  HierTree.NodeDataSize := SizeOf(THierRecord);
  HierTree.BeginUpdate;

  ALevel := 0;
  for i := 0 to FAxis.AxisTree[ALevel].Count - 1 do
    Obhod(0, i, nil);
  HierTree.EndUpdate;
end;

procedure TfcxAxisShow.HierTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  CellText := PHierRecord(Sender.GetNodeData(Node))^.Caption;
end;

procedure TfcxAxisShow.ShowAxis(ACaption : String; AAxis: TfcxAxisContainer);
begin
  Caption := ACaption;
  FAxis := AAxis;

  MakeTree;
  ShowModal;
end;

procedure TfcxAxisShow.btnExitClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure ShowAxis(ACaption : String; AAxis : TfcxAxisContainer);
var
  fcxAxisShow : TfcxAxisShow;
begin
  fcxAxisShow := TfcxAxisShow.Create(Application);
  fcxAxisShow.ShowAxis(ACaption, AAxis);
  fcxAxisShow.Free;
end;

procedure TfcxAxisShow.HierTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PHierRecord;
begin
  if Node <> nil then
  begin
    Data := Sender.GetNodeDatA(Node);
    Data^.Caption := '';
  end;
end;

procedure TfcxAxisShow.HierTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  IIndex : Byte;
begin
  IIndex := PHierRecord(Sender.GetNodeData(Node))^.ImageIndex;
  Ghosted := (IIndex and $04) = 0;
  case Kind of
    ikNormal, ikSelected : ImageIndex := IIndex;
    ikState, ikOverlay : ImageIndex := -1;
  end;
end;

procedure TfcxAxisShow.FormCreate(Sender: TObject);
const FixedColors : array[0..7] of TColor = (clRed, 255 or (200 shl 8), clYellow, clGreen, clAqua, clBlue, clPurple, clBlack);
var Bitmap : TBitmap;
    i, w, h : integer;
begin
  // Заполняем ImageList картинками
  w := Images.Width;
  h := Images.Height;

  randomize;

  for i := 0 to 40 do
  begin
    Bitmap := TBitmap.Create;
    Bitmap.Width  := w;
    Bitmap.Height := h;

    Bitmap.Canvas.Pen.Color := clBlack;
    Bitmap.Canvas.MoveTo(5, 3);
    Bitmap.Canvas.LineTo(10, 8);
    Bitmap.Canvas.LineTo(5, 13);
    Bitmap.Canvas.LineTo(0, 8);
    Bitmap.Canvas.LineTo(5, 3);
    if i <= 7 then
      Bitmap.Canvas.Brush.Color := FixedColors[i] else
      Bitmap.Canvas.Brush.Color := RGB(Random(255), Random(255), Random(255));
    Bitmap.Canvas.FloodFill(5, 8, clBlack, fsBorder);
    Bitmap.Canvas.Brush.Color := clFuchsia;
    Bitmap.Canvas.FloodFill(1, 1, clBlack, fsBorder);
    Bitmap.Canvas.Pixels[4, 8] := clWhite;
    Bitmap.Canvas.Pixels[6, 8] := clWhite;
    Bitmap.Canvas.Pixels[5, 7] := clWhite;
    Bitmap.Canvas.Pixels[5, 9] := clWhite;
    Images.AddMasked(Bitmap, clFuchsia);
    Bitmap.Free;
  end;
end;

end.
