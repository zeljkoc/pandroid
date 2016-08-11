{*******************************************************}
{                                                       }
{         FastCube chart data manipulation form         }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxChartDataManager;

interface

{$INCLUDE fcx.inc}
{$INCLUDE fcxTee.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  fcxTypes, fcxSlice, fcxSliceGrid;

type
  TfcxChartDataManager = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    Grid: TfcxSliceGrid;
    function GetSlice: TfcxSlice;
    procedure SetSlice(const Value: TfcxSlice);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure Localize;
    procedure Execute(OnlyPageEdit : Boolean = False);
    property Slice : TfcxSlice read GetSlice write SetSlice;
  end;

implementation

{$ifdef fpc}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

uses
  fcxRes;

{ TfcxChartDataManager }

constructor TfcxChartDataManager.Create(AOwner: TComponent);
begin
  inherited;
  Grid := TfcxSliceGrid.Create(Self);
  Grid.Parent := Self;
  Grid.Align := alClient;
end;

procedure TfcxChartDataManager.Execute(OnlyPageEdit: Boolean = False);
begin
{  if OnlyPageEdit then
    Grid.Options := Grid.Options + [mdgoDisableXAxisChange, mdgoDisableYAxisChange, mdgoDisableDataChange];
}    
  ShowModal;
end;

function TfcxChartDataManager.GetSlice: TfcxSlice;
begin
  Result := Grid.Slice;
end;

procedure TfcxChartDataManager.Localize;
begin
  Caption := fcxResources.Get('sChartDataManagerCaption');
end;

procedure TfcxChartDataManager.SetSlice(const Value: TfcxSlice);
begin
  Grid.Slice := Value;
end;

procedure TfcxChartDataManager.FormCreate(Sender: TObject);
begin
  Localize;
end;

end.
