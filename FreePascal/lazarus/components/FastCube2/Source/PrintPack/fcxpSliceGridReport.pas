{******************************************}
{                                          }
{             FastReport v4.0              }
{   FastCube 2 SliceGridReport components  }
{                                          }
{          Copyright (c) 2001-2010         }
{       by Oleg Pryalkov, Paul Ishenin,    }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpSliceGridReport;

interface

{$I fcx.inc}

uses
  Classes, SysUtils, frxClass, fcxRes, fcxSliceGrid, fcxpCross, fcxpComponents;

type
  TfcxpSliceGridReport = class(TComponent)
  private
    FGrid: TfcxSliceGrid;
    FReport: TfrxReport;
    FPaintSizes: TfcxpCrossPaintSizes;
    FBottomMargin: Extended;
    FTopMargin: Extended;
    FLeftMargin: Extended;
    FRightMargin: Extended;
    procedure SetPaintSizes(const Value: TfcxpCrossPaintSizes);
    procedure SetGrid(const Value: TfcxSliceGrid);
  protected
    procedure Prepare;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure PrintPreview;
    function Print: Boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Grid: TfcxSliceGrid read FGrid write SetGrid;
    property PaintSizes: TfcxpCrossPaintSizes read FPaintSizes write SetPaintSizes;
    // margins
    property LeftMargin: Extended read FLeftMargin write FLeftMargin;
    property RightMargin: Extended read FRightMargin write FRightMargin;
    property TopMargin: Extended read FTopMargin write FTopMargin;
    property BottomMargin: Extended read FBottomMargin write FBottomMargin;
  end;

implementation

{ TfcxpSliceGridReport }

constructor TfcxpSliceGridReport.Create(AOwner: TComponent);
begin
  inherited;
  FReport := TfrxReport.Create(Self);
  FPaintSizes := TfcxpCrossPaintSizes.Create(nil);
  FLeftMargin := 10;
  FRightMargin := 10;
  FTopMargin := 10;
  FBottomMargin := 10;
end;

destructor TfcxpSliceGridReport.Destroy;
begin
  FReport.Free;
  FPaintSizes.Free;
  inherited;
end;

procedure TfcxpSliceGridReport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FGrid <> nil) and
     (AComponent = FGrid) then
  begin
    FGrid := nil;
  end;
end;

procedure TfcxpSliceGridReport.Prepare;
var
  frxCross: TfcxpCrossView;
  frxPage: TfrxReportPage;
begin
  // preview in fr
  FReport.Clear;
  // create report page
  frxPage := TfrxReportPage.Create(FReport);
  frxPage.Parent := FReport;
  frxPage.LeftMargin := LeftMargin;
  frxPage.TopMargin := TopMargin;
  frxPage.RightMargin := RightMargin;
  frxPage.BottomMargin := BottomMargin;
  // create cross view and place it to page
  if Assigned(Grid) then
  begin
    if Assigned(Grid.Slice) then
    begin
      frxCross := TfcxpCrossView.Create(frxPage);
      frxCross.Slice := Grid.Slice;
      frxCross.CreateUniqueName;
      frxCross.Left := 0;
      frxCross.Top := 0;
      frxCross.ImportColorFromGrid(Grid);
      frxCross.PaintSizes.Assign(FPaintSizes);
    end;
  end;
end;

function TfcxpSliceGridReport.Print: Boolean;
begin
  Prepare;
  Result := FReport.Print;
end;

procedure TfcxpSliceGridReport.PrintPreview;
begin
  Prepare;
  try
    FReport.ShowReport;
  finally
    FReport.Clear;
  end;
end;

procedure TfcxpSliceGridReport.SetGrid(const Value: TfcxSliceGrid);
begin
  FGrid := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TfcxpSliceGridReport.SetPaintSizes(const Value: TfcxpCrossPaintSizes);
begin
  FPaintSizes.Assign(Value);
end;

end.
