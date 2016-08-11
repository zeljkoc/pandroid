{*******************************************************}
{                                                       }
{              FastCube 2 Export interface unit         }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
//VCL uses section
{$IFNDEF FMX}
unit fcxCustomExport;
{$INCLUDE fcx.inc}

interface

uses
  SysUtils, Classes, Controls,
  Forms, Dialogs,
  fcxTypes, fcxRes, fcxCube, fcxSlice, fcxStyles;
{$ELSE FMX}
interface

uses
  System.UITypes, System.SysUtils, System.Classes,
  FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.fcxRes, FMX.fcxTypes, FMX.fcxCube, FMX.fcxSlice, FMX.fcxStyles;
{$ENDIF FMX}

type
  TfcxExportTitle = ( // Titles
    rtReportHeader,   // Report title
    rtWorksheet,      // Worksheet title
    rtDataHeader      // Data title
  );

  TfcxExportTitleEvent = procedure(Sender: TObject; ExportTitle: TfcxExportTitle; var Value: String) of Object;
  TfcxExportGetCubeRowCountEvent = procedure(Sender: TObject; var Value: Integer) of Object;
  TfcxExportGetCubeRowIndexEvent = procedure(Sender: TObject; ARow: Integer; var Value: Integer) of Object;
  TfcxExportGetCubeColumnsEvent = procedure(Sender: TObject; Value: TfcxCubeDataColumns) of Object;
  TfcxPrepareSaveDialogEvent = procedure(Sender: TObject; ADialog: TSaveDialog) of Object;

  TfcxCustomExportFilter = class(TComponent)
  private
    FSlice: TfcxSlice;
    FCube: TfcxCube;
    FStyles: TfcxCustomThemeStyles;

    FName: String;
    FNoRegister: Boolean;
    FShowDialog: Boolean;
    FStream: TStream;
    FDefaultPath: String;
    FShowProgress: Boolean;
    FDefaultExt: String;
    FFilterDesc: String;
    FOverwritePrompt: Boolean;
    FOnBeginExport: TNotifyEvent;
    FCreationTime: TDateTime;
    FCubeDataColumns: TfcxCubeDataColumns;
    FOnGetTitle: TfcxExportTitleEvent;
    FOnGetCubeRowCount: TfcxExportGetCubeRowCountEvent;
    FOnGetCubeRowIndex: TfcxExportGetCubeRowIndexEvent;
    FOnGetCubeColumns: TfcxExportGetCubeColumnsEvent;
    FOnPrepareSaveDialog: TfcxPrepareSaveDialogEvent;
    procedure SetStyles(const Value: TfcxCustomThemeStyles);
    procedure SetSlice(const Value: TfcxSlice);
    procedure SetCube(const Value: TfcxCube);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    function GetTitle(AExportTitle: TfcxExportTitle): String;
    procedure PrepareSaveDialog(ADialog: TSaveDialog);
    function DoGetCubeRowCount: Integer;
    function DoGetCubeRowIndex(ARow: Integer): Integer;
    function DoGetCubeCols: TfcxCubeDataColumns;
    procedure DoProgress(Progress: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNoRegister;
    destructor Destroy; override;
    class function GetDescription: String; virtual;
    function PerformExport: Boolean;
    function ShowModal: TModalResult; virtual;
    function Start: Boolean; virtual;
    procedure Run; virtual;
    procedure Finish; virtual;

    property Cube: TfcxCube read FCube write SetCube;
    property Slice: TfcxSlice read FSlice write SetSlice;
    property Styles: TfcxCustomThemeStyles read FStyles write SetStyles;
    property Stream: TStream read FStream write FStream;
    property DefaultExt: String read FDefaultExt write FDefaultExt;
    property FilterDesc: String read FFilterDesc write FFilterDesc;
  published
    property Version: String read GetVersion write SetVersion;
    property ShowDialog: Boolean read FShowDialog write FShowDialog default True;
    property FileName: String read FName write FName;
    property DefaultPath: String read FDefaultPath write FDefaultPath;
    property ShowProgress: Boolean read FShowProgress write FShowProgress default True;
    property OverwritePrompt: Boolean read FOverwritePrompt write FOverwritePrompt default False;
    property CreationTime: TDateTime read FCreationTime write FCreationTime;

    property OnBeginExport: TNotifyEvent read FOnBeginExport write FOnBeginExport;
    property OnPrepareSaveDialog: TfcxPrepareSaveDialogEvent read FOnPrepareSaveDialog write FOnPrepareSaveDialog;
    property OnGetTitle: TfcxExportTitleEvent read FOnGetTitle write FOnGetTitle;
    property OnGetCubeRowCount: TfcxExportGetCubeRowCountEvent read FOnGetCubeRowCount write FOnGetCubeRowCount;
    property OnGetCubeRowIndex: TfcxExportGetCubeRowIndexEvent read FOnGetCubeRowIndex write FOnGetCubeRowIndex;
    property OnGetCubeColumns: TfcxExportGetCubeColumnsEvent read FOnGetCubeColumns write FOnGetCubeColumns;
  end;

  TfcxExportFilterItem = class(TCollectionItem)
  public
    Filter: TfcxCustomExportFilter;
  end;

  TfcxExportFilterCollection = class(TCollection)
  private
    function GetExportFilterItem(Index: Integer): TfcxExportFilterItem;
  public
    constructor Create;
    procedure Register(Filter: TfcxCustomExportFilter);
    procedure Unregister(Filter: TfcxCustomExportFilter);
    property Items[Index: Integer]: TfcxExportFilterItem
      read GetExportFilterItem; default;
  end;


function fcxExportFilters: TfcxExportFilterCollection;

implementation
var
  FExportFilters: TfcxExportFilterCollection = nil;

{ TfcxCustomExportFilter }

constructor TfcxCustomExportFilter.Create(AOwner: TComponent);
begin
  inherited;
  if not FNoRegister then
    fcxExportFilters.Register(Self);
  FShowDialog := True;
  FDefaultPath := '';
  FShowProgress := True;
  FOverwritePrompt := False;
  {$IFDEF FMX}
  FStyles := TfcxStyles.Create(nil);
  {$ELSE}
    {$WARNINGS OFF}
    FStyles := TfcxCustomThemeStyles.Create(nil);
    {$WARNINGS ON}
  {$ENDIF}
  FCubeDataColumns := TfcxCubeDataColumns.Create;
end;

constructor TfcxCustomExportFilter.CreateNoRegister;
begin
  FNoRegister := True;
  Create(nil);
end;

destructor TfcxCustomExportFilter.Destroy;
begin
  FCubeDataColumns.Free;
  FStyles.Free;
  if not FNoRegister then
    fcxExportFilters.Unregister(Self);
  inherited;
end;

class function TfcxCustomExportFilter.GetDescription: String;
begin
  Result := '';
end;

procedure TfcxCustomExportFilter.Finish;
begin
//
end;

function TfcxCustomExportFilter.ShowModal: TModalResult;
begin
  Result := mrOk;
end;

function TfcxCustomExportFilter.Start: Boolean;
begin
  Result := True;
end;

{ TcrxExportFilterCollection }

constructor TfcxExportFilterCollection.Create;
begin
  inherited Create(TfcxExportFilterItem);
end;

function TfcxExportFilterCollection.GetExportFilterItem(
  Index: Integer): TfcxExportFilterItem;
begin
  Result := TfcxExportFilterItem(inherited Items[Index]);
end;

procedure TfcxExportFilterCollection.Register(Filter: TfcxCustomExportFilter);
var
  i: Integer;
  Item: TfcxExportFilterItem;
begin
  if Filter = nil then Exit;
  for i := 0 to Count - 1 do
    if Items[i].Filter = Filter then
      Exit;

  Item := TfcxExportFilterItem(Add);
  Item.Filter := Filter;
end;

procedure TfcxExportFilterCollection.UnRegister(Filter: TfcxCustomExportFilter);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    if Items[i].Filter = Filter then
      Items[i].Free else
      Inc(i);
  end;
end;

function fcxExportFilters: TfcxExportFilterCollection;
begin
  if FExportFilters = nil then
    FExportFilters := TfcxExportFilterCollection.Create;
  Result := FExportFilters;
end;

function TfcxCustomExportFilter.PerformExport: Boolean;
var
  Filter: TfcxCustomExportFilter;

  procedure DoExport;
  begin
    if Assigned(OnBeginExport) then
      OnBeginExport(Filter);

    if Start then
      try
        if ShowProgress then
          Cube.InternalOnProgressStart(fcxpExporting);
        Run;
      finally
        if ShowProgress then
          Cube.InternalOnProgressStop(fcxpExporting);
        Finish;
      end;

  end;

begin
  Result := False;

  CreationTime := Now;
  if (ShowDialog and (ShowModal <> mrOk)) then
    Exit;

  Result := True;

  try
    DoExport;
  finally
  end;
end;

procedure TfcxCustomExportFilter.SetStyles(const Value: TfcxCustomThemeStyles);
begin
  if FStyles.ClassType <> Value.ClassType then
  begin
    FStyles.Free;
    FStyles := TfcxCustomThemeStyles(Value.ClassType.NewInstance);
    {$WARNINGS OFF}
    TfcxCustomThemeStyles(FStyles).Create(nil);
    {$WARNINGS ON}
  end;
  FStyles.Assign(Value);
end;

function TfcxCustomExportFilter.GetTitle(AExportTitle: TfcxExportTitle): String;
begin
  Result := '';
  case AExportTitle of
    rtReportHeader:
      Result := 'FastCube Export';
    rtWorksheet:
      begin
        Result := Cube.Caption;
        if Result = '' then
          Result := 'Cube data';
      end;
    rtDataHeader:
      begin
        Result := Cube.Caption;
        if Result = '' then
          Result := 'Report Data';
      end;
  end;
  if Assigned(OnGetTitle) then
    OnGetTitle(Self, AExportTitle, Result);
end;

procedure TfcxCustomExportFilter.PrepareSaveDialog(ADialog: TSaveDialog);
begin
  if OverwritePrompt then
    ADialog.Options := ADialog.Options + [{$IFDEF FMX}TOpenOption.{$ENDIF}ofOverwritePrompt];
  ADialog.FileName := FileName;
  ADialog.Filter := FilterDesc;
  ADialog.DefaultExt := DefaultExt;
  if DefaultPath <> '' then
    ADialog.InitialDir := DefaultPath;
  if Assigned(OnPrepareSaveDialog) then
    OnPrepareSaveDialog(Self, ADialog);
end;

procedure TfcxCustomExportFilter.SetSlice(const Value: TfcxSlice);
begin
  FSlice := Value;
  if Assigned(Slice) then
    Cube := Slice.Cube
  else
    Cube := nil;
end;

function TfcxCustomExportFilter.DoGetCubeRowCount: Integer;
begin
  Result := Cube.SourceHolder.RecordsCount;
  if Assigned(OnGetCubeRowCount) then
    OnGetCubeRowCount(Self, Result);
end;

function TfcxCustomExportFilter.DoGetCubeRowIndex(ARow: Integer): Integer;
begin
  Result := ARow;
  if Assigned(OnGetCubeRowIndex) then
    OnGetCubeRowIndex(Self, ARow, Result);
end;

function TfcxCustomExportFilter.DoGetCubeCols: TfcxCubeDataColumns;
begin
  if Assigned(OnGetCubeColumns) then
    OnGetCubeColumns(Self, FCubeDataColumns)
  else
  if Assigned(Cube) then
    FCubeDataColumns.Update(Cube);
  Result := FCubeDataColumns;
end;

procedure TfcxCustomExportFilter.SetCube(const Value: TfcxCube);
begin
  FCube := Value;
  FCubeDataColumns.Clear;
end;

procedure TfcxCustomExportFilter.Run;
begin
//
end;

procedure TfcxCustomExportFilter.DoProgress(Progress: Integer);
begin
  if ShowProgress then
    Cube.InternalOnProgress(fcxpExporting, Progress);
end;

function TfcxCustomExportFilter.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxCustomExportFilter.SetVersion(const Value: String);
begin
//
end;

initialization

finalization
  FExportFilters.Free;

end.
