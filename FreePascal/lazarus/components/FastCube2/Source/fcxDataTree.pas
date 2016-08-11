{******************************************}
{                                          }
{               FastReport                 }
{          Data Tree tool window           }
{                                          }
{         Copyright (c) 1998-2011          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxDataTree;

interface

{$I fcx.inc}
{$R fcxDataTree.res}
uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, fs_iinterpreter, fs_xml
{$IFDEF Delphi6}
, Variants
{$ENDIF}
;


type
  TfcxDataTreeForm = class(TForm)
    Tabs: TPageControl;
    VariablesTS: TTabSheet;
    VariablesTree: TTreeView;
    FunctionsTS: TTabSheet;
    ClassesTS: TTabSheet;
    FunctionsTree: TTreeView;
    ClassesTree: TTreeView;
    HintPanel: TPanel;
    Splitter1: TSplitter;
    FunctionDescL: TLabel;
    FunctionNameL: TLabel;
    procedure FormResize(Sender: TObject);
    procedure DataTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FunctionsTreeChange(Sender: TObject; Node: TTreeNode);
    procedure DataTreeDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FunctionsTSShow(Sender: TObject);
    procedure ClassesTSShow(Sender: TObject);
    procedure ClassesTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure ClassesTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
    FUpdating: Boolean;
    FFirstTime: Boolean;
    FScript: TfsScript;
    FSysVariables: TStringList;
    FMeasures: TStringList;
    FDimensions: TStringList;
    FSliceFields: TStringList;
    FXML: TfsXMLDocument;
    procedure FillFunctionsTree;
    procedure FillVariablesTree;
    procedure FillClassesTree;
    procedure SetScript(const Value: TfsScript);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColor(Color: TColor);
    procedure UpdateItems;
    function GetFieldName: String;
    property Script: TfsScript read FScript write SetScript;
    property SysVariables: TStringList read FSysVariables;
    property Measures: TStringList read FMeasures;
    property Dimensions: TStringList read FDimensions;
    property SliceFields: TStringList read FSliceFields;
  end;


implementation
{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  fcxRes,
  fcxGraphicRes,
  fs_itools;

procedure SetImageIndex(Node: TTreeNode; Index: Integer);
begin
  Node.ImageIndex := Index;
  Node.StateIndex := Index;
  Node.SelectedIndex := Index;
end;


{ TfrxDataTreeForm }

procedure TfcxDataTreeForm.ClassesTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
    Sender.Canvas.Font.Style := [fsBold];
end;

procedure TfcxDataTreeForm.ClassesTreeExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  i: Integer;
  xi: TfsXMLItem;
  s: String;
  n: TTreeNode;
begin
  if (Node.Level = 1) and (Node.Data <> nil) then
  begin
    FUpdating := True;
    ClassesTree.Items.BeginUpdate;

    Node.DeleteChildren;
    xi := TfsXMLItem(Node.Data);
    Node.Data := nil;

    for i := 0 to xi.Count - 1 do
    begin
      s := xi[i].Prop['text'];
      n := ClassesTree.Items.AddChild(Node, s);
      if Pos('property', s) = 1 then
        SetImageIndex(n, 73)
      else if Pos('event', s) = 1 then
        SetImageIndex(n, 79)
      else
        SetImageIndex(n, 74);
    end;

    ClassesTree.Items.EndUpdate;
  end;
end;

procedure TfcxDataTreeForm.ClassesTSShow(Sender: TObject);
begin
  if (Sender = ClassesTS) and (ClassesTree.Items.Count = 0) then
    FillClassesTree;
end;

constructor TfcxDataTreeForm.Create(AOwner: TComponent);
begin
  inherited;
  FXML := TfsXMLDocument.Create;

  FSysVariables := TStringList.Create;
  FMeasures := TStringList.Create;
  FDimensions := TStringList.Create;
  FSliceFields := TStringList.Create;

  VariablesTree.Images := fcxGraphicResources.MainButtonImages;
  FunctionsTree.Images := fcxGraphicResources.MainButtonImages;
  ClassesTree.Images := fcxGraphicResources.MainButtonImages;
  FFirstTime := True;
end;

destructor TfcxDataTreeForm.Destroy;
begin
  FUpdating := True;
  FSysVariables.Free;
  FMeasures.Free;
  FDimensions.Free;
  FSliceFields.Free;
  FXML.Free;
  inherited;
end;

procedure TfcxDataTreeForm.FillVariablesTree;
var
//  CategoriesList, VariablesList: TStrings;
  Root: TTreeNode;

  function AddNode(ParentNode: TTreeNode; const s: String): TTreeNode;
  begin
    Result := VariablesTree.Items.AddChild(ParentNode, s);
    SetImageIndex(Result, 80);
  end;

  procedure AddSysVariables;
  var
    Node: TTreeNode;
    i: integer;
  begin
    Node := VariablesTree.Items.AddChild(Root, fcxResources.Get('dtSysVar'));
    SetImageIndex(Node, 66);
    for i := 0 to SysVariables.Count - 1 do
      AddNode(Node, SysVariables[i]);
  end;

  procedure AddItems(AItems: TStringList; ACaption: String);
  var
    Node: TTreeNode;
    i: integer;
  begin
    Node := VariablesTree.Items.AddChild(Root, ACaption);
    SetImageIndex(Node, 66);
    for i := 0 to AItems.Count - 1 do
      {$IFDEF DELPHI7_UP}
      AddNode(AddNode(Node, AItems.Names[i]), AItems.ValueFromIndex[i])
      {$ELSE}
      AddNode(AddNode(Node, AItems.Names[i]), Copy(AItems[i], Length(AItems.Names[i]) + 2, MaxInt))
      {$ENDIF}
  end;

begin
//  CategoriesList := TStringList.Create;
//  VariablesList := TStringList.Create;
  // IP: добавляем поля !!!

  VariablesTree.Items.BeginUpdate;
  VariablesTree.Items.Clear;
  Root := VariablesTree.Items.AddChild(nil, fcxResources.Get('dtVar'));
  SetImageIndex(Root, 66);

  AddSysVariables;
  AddItems(FMeasures, fcxResources.Get('dtMeasures'));
  AddItems(FDimensions, fcxResources.Get('dtDimensions'));
  AddItems(FSliceFields, fcxResources.Get('dtSliceFields'));

  VariablesTree.FullExpand;
  VariablesTree.TopItem := Root;
  VariablesTree.Items.EndUpdate;
//  CategoriesList.Free;
//  VariablesList.Free;
end;

procedure TfcxDataTreeForm.FillFunctionsTree;

  procedure AddFunctions(xi: TfsXMLItem; Root: TTreeNode);
  var
    i: Integer;
    Node: TTreeNode;
    s: String;
  begin
    s := xi.Prop['text'];
    if xi.Count = 0 then
      s := Copy(s, Pos(' ', s) + 1, 255) else  { function }
      s := fcxResources.Get(s);                { category }

    if CompareText(s, 'hidden') = 0 then Exit;
    Node := FunctionsTree.Items.AddChild(Root, s);
    if xi.Count = 0 then
      Node.Data := xi;
    if Root = nil then
      Node.Text := fcxResources.Get('dtFunc');
    if xi.Count = 0 then
      SetImageIndex(Node, 80) else
      SetImageIndex(Node, 66);

    for i := 0 to xi.Count - 1 do
      AddFunctions(xi[i], Node);
  end;
begin
  FUpdating := True;

  FunctionsTree.Items.BeginUpdate;
  FunctionsTree.Items.Clear;
  if (FXML.Root.Count = 0) and (FScript <> nil) then
  begin
//    fcxAddRTTI(FScript);
    GenerateXMLContents(FScript, FXML.Root);
  end;

  AddFunctions(FXML.Root.FindItem('Functions'), nil);

  FunctionsTree.FullExpand;
  FunctionsTree.TopItem := FunctionsTree.Items[0];
  FunctionsTree.Items.EndUpdate;
  FUpdating := False;
end;

procedure TfcxDataTreeForm.FillClassesTree;

  procedure AddClasses(xi: TfsXMLItem; Root: TTreeNode);
  var
    i: Integer;
    Node: TTreeNode;
    s: String;
  begin
    s := xi.Prop['text'];

    Node := ClassesTree.Items.AddChild(Root, s);
    Node.Data := xi;
    if Root = nil then
    begin
      Node.Text := fcxResources.Get('2106');
      SetImageIndex(Node, 66);
    end
    else
      SetImageIndex(Node, 78);

    if Root = nil then
    begin
      for i := 0 to xi.Count - 1 do
        AddClasses(xi[i], Node);
    end
    else
      ClassesTree.Items.AddChild(Node, 'more...');  // do not localize
  end;

begin
  FUpdating := True;

  ClassesTree.Items.BeginUpdate;
  ClassesTree.Items.Clear;
  AddClasses(FXML.Root.FindItem('Classes'), nil);

  ClassesTree.TopItem := ClassesTree.Items[0];
  ClassesTree.TopItem.Expand(False);
  ClassesTree.Items.EndUpdate;
  FUpdating := False;
end;

function TfcxDataTreeForm.GetFieldName: String;
var
  i, n: Integer;
  s: String;
  Node: TTreeNode;
begin
  Result := '';
  if Tabs.ActivePage = VariablesTS then
  begin
    Node := VariablesTree.Selected;
    if (Node <> nil) and (Node.Count = 0) then
        Result := Node.Text;
  end
  else if Tabs.ActivePage = FunctionsTS then
  begin
    if (FunctionsTree.Selected <> nil) and (FunctionsTree.Selected.Count = 0) then
    begin
      s := FunctionsTree.Selected.Text;
      if Pos('(', s) <> 0 then
        n := 1 else
        n := 0;
      for i := 1 to Length(s) do
        {$IFDEF DELPHI12}
        if CharInSet(s[i], [',', ';']) then
        {$ELSE}
        if s[i] in [',', ';'] then
        {$ENDIF}
          Inc(n);

      if n = 0 then
        s := Copy(s, 1, Pos(':', s) - 1)
      else
      begin
        s := Copy(s, 1, Pos('(', s));
        for i := 1 to n - 1 do
          s := s + ',';
        s := s + ')';
      end;
      Result := s;
    end;
  end;
end;

procedure TfcxDataTreeForm.UpdateItems;
begin
  FillVariablesTree;
  FFirstTime := False;
end;

procedure TfcxDataTreeForm.SetColor(Color: TColor);
begin
  VariablesTree.Color := Color;
  FunctionsTree.Color := Color;
  ClassesTree.Color := Color;
end;

procedure TfcxDataTreeForm.SetScript(const Value: TfsScript);
begin
  FScript := Value;
  FillFunctionsTree;
  FillVariablesTree;
  FillClassesTree;
end;

procedure TfcxDataTreeForm.FormResize(Sender: TObject);
begin
  AutoScroll := False;
  Tabs.SetBounds(-4, 0, ClientWidth + 8, ClientHeight + 4);
end;

procedure TfcxDataTreeForm.DataTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Count <> 0 then
    Sender.Canvas.Font.Style := [fsBold];
end;

procedure TfcxDataTreeForm.FunctionsTreeChange(Sender: TObject;
  Node: TTreeNode);
var
  xi: TfsXMLItem;
begin
  if FUpdating then Exit;
  Node := FunctionsTree.Selected;
  if (Node = nil) or (Node.Data = nil) then
  begin
    FunctionNameL.Caption := '';
    FunctionDescL.Caption := '';
    Exit;
  end
  else
  begin
    xi := Node.Data;
    FunctionNameL.Caption := xi.Prop['text'];
    FunctionDescL.Caption := fcxResources.Get(xi.Prop['description']);
    FunctionNameL.AutoSize := True;
  end;
end;

procedure TfcxDataTreeForm.DataTreeDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Sender);
end;

procedure TfcxDataTreeForm.FormCreate(Sender: TObject);
begin
  Caption := fcxGet(2100);
  VariablesTS.Caption := fcxGet(2102);
  FunctionsTS.Caption := fcxGet(2103);
  ClassesTS.Caption := fcxGet(2106);
end;

procedure TfcxDataTreeForm.FunctionsTSShow(Sender: TObject);
begin
  if (Sender = FunctionsTS) and (FunctionsTree.Items.Count = 0) then
    FillFunctionsTree;
end;

end.
