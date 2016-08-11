{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
unit AZCToolBar;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15;

type

  { AZCArrayAddapter }

  TZCArrayAddapter = class(AWArrayAdapter)
    FView: JUArrayList;
  public
    constructor create(aContext: ACContext; para2: jint; aView: JUArrayList); overload;
    function getView(para1: jint; aView: AVView; aViewGroup: AVViewGroup): AVView;  override;
  end;

  { TZCToolBar }

  TZCToolBar = class(AWLinearLayout)
    FAdapter: TZCArrayAddapter;
    FItems: JUArrayList;
    FGridView: AWGridView;
  public
    function GetNumColumnsGridView: jint;
    function getStretchModeGridView: jint;
    procedure setColumnWidthGridView(AValue: jint);
    procedure setGravityGridView(AValue: jint);
    procedure setHorizontalSpacingGridView(AValue: jint);
    procedure SetNumColumnsGridView(AValue: jint);
    procedure setStretchModeGridView(AValue: jint);
    procedure setVerticalSpacingGridView(AValue: jint);
  public
    constructor create(para1: ACContext); overload;
    function add(para1: JLObject): jboolean; overload; virtual;
    procedure Clear;
  public
    property Items: JUArrayList read FItems;
  end;

implementation

{ TZCToolBar }

function TZCToolBar.GetNumColumnsGridView: jint;
begin
  Result := FGridView.getNumColumns;
end;

function TZCToolBar.getStretchModeGridView: jint;
begin
  Result := FGridView.getStretchMode;
end;

procedure TZCToolBar.setColumnWidthGridView(AValue: jint);
begin
  FGridView.setColumnWidth(AValue);
end;

procedure TZCToolBar.setGravityGridView(AValue: jint);
begin
  FGridView.setGravity(AValue);
end;

procedure TZCToolBar.setHorizontalSpacingGridView(AValue: jint);
begin
  FGridView.setHorizontalSpacing(AValue);
end;

procedure TZCToolBar.SetNumColumnsGridView(AValue: jint);
begin
  FGridView.setNumColumns(AValue);
end;

procedure TZCToolBar.setStretchModeGridView(AValue: jint);
begin
  FGridView.setStretchMode(AValue);
end;

procedure TZCToolBar.setVerticalSpacingGridView(AValue: jint);
begin
  FGridView.setVerticalSpacing(AValue);
end;

constructor TZCToolBar.create(para1: ACContext);
begin
  inherited create(para1);
  FItems:= JUArrayList.Create;

  FGridView:= AWGridView.create(para1);

    FAdapter := TZCArrayAddapter.create(para1, AR.innerLayout.simple_list_item_1, FItems);
  FGridView.setAdapter(FAdapter);
  inherited addView(FGridView)
end;

function TZCToolBar.add(para1: JLObject): jboolean;
begin
  FItems.add(para1);
end;

procedure TZCToolBar.Clear;
begin
  FItems.clear;
  FAdapter.clear;
end;


{ AZCArrayAddapter }

constructor TZCArrayAddapter.create(aContext: ACContext; para2: jint; aView: JUArrayList);
begin
 FView := aView;
 inherited create(aContext,  para2, FView);
end;

function TZCArrayAddapter.getView(para1: jint; aView: AVView; aViewGroup: AVViewGroup): AVView;
begin
  Result:=  AVView(FView.get(para1));
end;

end.

