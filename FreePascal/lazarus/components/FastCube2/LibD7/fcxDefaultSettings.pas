{*******************************************************}
{                                                       }
{          FastCube 2 Default Settings unit             }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxDefaultSettings;
{$INCLUDE fcx.inc}

interface
uses
  Classes, SysUtils, TypInfo,
  fcxTypes, fcxFormats
{$IFDEF FPC}
  , LCLType
{$ELSE}
  , Windows
{$ENDIF}
  ;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils, System.TypInfo,
  FMX.fcxTypes, FMX.fcxFormats;
{$ENDIF FMX}

const
  cfcRowHeight = 18;
  cfcColWidth = 100;

type
  TfcxDefaultFormat = class(TPersistent)
  private
    FDatePathFormat: TfcxDatePathFormat;
    FDateFormat: TfcxFormat;
    FDateTimeFormat: TfcxFormat;
    FTimeFormat: TfcxFormat;
    FFloatFormat: TfcxFormat;
    FCurrencyFormat: TfcxFormat;
    FIntegerFormat: TfcxFormat;
    FBooleanFormat: TfcxFormat;
    FTextFormat: TfcxFormat;
    FPercentFormat: TfcxFormat;
    procedure DoFormatChange(Sender: TObject);
    procedure DoDatePathFormatChange(Sender: TObject);
    procedure SetBooleanFormat(const Value: TfcxTypeFormat);
    procedure SetCurrencyFormat(const Value: TfcxTypeFormat);
    procedure SetDateFormat(const Value: TfcxTypeFormat);
    procedure SetDatePathFormat(const Value: TfcxDatePathFormat);
    procedure SetDateTimeFormat(const Value: TfcxTypeFormat);
    procedure SetFloatFormat(const Value: TfcxTypeFormat);
    procedure SetIntegerFormat(const Value: TfcxTypeFormat);
    procedure SetPercentFormat(const Value: TfcxTypeFormat);
    procedure SetTextFormat(const Value: TfcxTypeFormat);
    procedure SetTimeFormat(const Value: TfcxTypeFormat);
    function GetBooleanFormat: TfcxTypeFormat;
    function GetCurrencyFormat: TfcxTypeFormat;
    function GetDateFormat: TfcxTypeFormat;
    function GetDatePathFormat: TfcxDatePathFormat;
    function GetDateTimeFormat: TfcxTypeFormat;
    function GetFloatFormat: TfcxTypeFormat;
    function GetIntegerFormat: TfcxTypeFormat;
    function GetPercentFormat: TfcxTypeFormat;
    function GetTextFormat: TfcxTypeFormat;
    function GetTimeFormat: TfcxTypeFormat;
  protected
  public
    procedure SetDefaults;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    function DefaultFormatByType(ADataType: TfcxDataType): TfcxFormat;
  published
    property BooleanFormat: TfcxTypeFormat read GetBooleanFormat write SetBooleanFormat;
    property DateFormat: TfcxTypeFormat read GetDateFormat write SetDateFormat;
    property TimeFormat: TfcxTypeFormat read GetTimeFormat write SetTimeFormat;
    property DateTimeFormat: TfcxTypeFormat read GetDateTimeFormat write SetDateTimeFormat;
    property FloatFormat: TfcxTypeFormat read GetFloatFormat write SetFloatFormat;
    property CurrencyFormat: TfcxTypeFormat read GetCurrencyFormat write SetCurrencyFormat;
    property IntegerFormat: TfcxTypeFormat read GetIntegerFormat write SetIntegerFormat;
    property TextFormat: TfcxTypeFormat read GetTextFormat write SetTextFormat;
    property PercentFormat: TfcxTypeFormat read GetPercentFormat write SetPercentFormat;
    property DatePathFormat: TfcxDatePathFormat read GetDatePathFormat write SetDatePathFormat;
  end;

  TfcxAxisFieldDefaultSettings = class(TPersistent)
  private
    FTotalPosition: TfcxTotalPosition;
    FSortDirection: TfcxSortDirection;
    procedure SetTotalPosition(const Value: TfcxTotalPosition);
    procedure SetSortDirection(const Value: TfcxSortDirection);
  protected
  public
    procedure SetDefaults;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property SortDirection: TfcxSortDirection read FSortDirection write SetSortDirection default fcsd_Asc;
    property TotalPosition: TfcxTotalPosition read FTotalPosition write SetTotalPosition default fctp_Before;
  end;

  TfcxAxisDefaultSettings = class(TPersistent)
  private
    FGrandTotalPosition: TfcxTotalPosition;
    FShowTotalAs: TfcxShowTotalAs;
    procedure SetGrandTotalPosition(const Value: TfcxTotalPosition);
    procedure SetShowTotalAs(const Value: TfcxShowTotalAs);
  protected
  public
    procedure SetDefaults;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property GrandTotalPosition: TfcxTotalPosition read FGrandTotalPosition write SetGrandTotalPosition default fctp_Before;
    property ShowTotalAs: TfcxShowTotalAs read FShowTotalAs write SetShowTotalAs default sta_AsTotal;
  end;

  TfcxSliceDefaultSettings = class(TPersistent)
  private
    FYAxisSettings: TfcxAxisDefaultSettings;
    FXAxisSettings: TfcxAxisDefaultSettings;
    FAxisFieldSettings: TfcxAxisFieldDefaultSettings;
    procedure SetXAxisSettings(const Value: TfcxAxisDefaultSettings);
    procedure SetYAxisSettings(const Value: TfcxAxisDefaultSettings);
    procedure SetAxisFieldSettings(
      const Value: TfcxAxisFieldDefaultSettings);
  protected

  public
    procedure SetDefaults;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property XAxisSettings: TfcxAxisDefaultSettings read FXAxisSettings write SetXAxisSettings;
    property YAxisSettings: TfcxAxisDefaultSettings read FYAxisSettings write SetYAxisSettings;
    property AxisFieldSettings: TfcxAxisFieldDefaultSettings read FAxisFieldSettings write SetAxisFieldSettings;
  end;

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxDefaultSettings = class(TComponent)
  private
    procedure SetFormat(const Value: TfcxDefaultFormat);
    function GetFormat: TfcxDefaultFormat;
    function GetColWidth: Integer;
    function GetRowHeight: Integer;
    procedure SetColWidth(const Value: Integer);
    procedure SetRowHeight(const Value: Integer);
    function GetSliceSettings: TfcxSliceDefaultSettings;
    procedure SetSliceSettings(const Value: TfcxSliceDefaultSettings);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
  public
    procedure SetDefaults;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Version: String read GetVersion write SetVersion;
    // Default Format for data types
    property Format: TfcxDefaultFormat read GetFormat write SetFormat;
    property SliceSettings: TfcxSliceDefaultSettings read GetSliceSettings write SetSliceSettings;
    property RowHeight: Integer read GetRowHeight write SetRowHeight default cfcRowHeight;
    property ColWidth: Integer read GetColWidth write SetColWidth default cfcColWidth;
  end;

  TfcxDefaultSettingsStore = class
  private
    FFormat: TfcxDefaultFormat;
    FRowHeight: Integer;
    FColWidth: Integer;
    FSliceSettings: TfcxSliceDefaultSettings;
    procedure SetFormat(const Value: TfcxDefaultFormat);
    procedure SetSliceSettings(const Value: TfcxSliceDefaultSettings);
  protected
  public
    procedure SetDefaults;
    constructor Create;
    destructor Destroy; override;
    property Format: TfcxDefaultFormat read FFormat write SetFormat;
    property SliceSettings: TfcxSliceDefaultSettings read FSliceSettings write SetSliceSettings;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property ColWidth: Integer read FColWidth write FColWidth;
  end;

function fcDefaultSettingsStore: TfcxDefaultSettingsStore;

implementation

//VCL uses section
{$IFNDEF FMX}
uses fcxRes;
//FMX uses
{$ELSE FMX}
uses FMX.fcxRes;
{$ENDIF FMX}

var
  FfcxDefaultSettingsStore: TfcxDefaultSettingsStore = nil;


function fcDefaultSettingsStore: TfcxDefaultSettingsStore;
begin
  if FfcxDefaultSettingsStore = nil then
    FfcxDefaultSettingsStore := TfcxDefaultSettingsStore.Create;
  Result := FfcxDefaultSettingsStore;
end;

{ TfcxDefaultFormat }

procedure TfcxDefaultFormat.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxDefaultFormat then
    with TfcxDefaultFormat(Dest) do
    begin
      DateFormat := Self.DateFormat;
      DateTimeFormat := Self.DateTimeFormat;
      TimeFormat := Self.TimeFormat;
      FloatFormat := Self.FloatFormat;
      CurrencyFormat := Self.CurrencyFormat;
      IntegerFormat := Self.IntegerFormat;
      BooleanFormat := Self.BooleanFormat;
      TextFormat := Self.TextFormat;
      PercentFormat := Self.PercentFormat;
      DatePathFormat.Assign(Self.DatePathFormat);
    end
  else inherited AssignTo(Dest);
end;

Type TfcxFormatHack = class(TfcxFormat);

constructor TfcxDefaultFormat.Create;
begin
  FDatePathFormat := TfcxDatePathFormat.Create;

  FDateFormat := TfcxFormat.Create;
  FDateTimeFormat := TfcxFormat.Create;
  FTimeFormat := TfcxFormat.Create;
  FFloatFormat := TfcxFormat.Create;
  FCurrencyFormat := TfcxFormat.Create;
  FIntegerFormat := TfcxFormat.Create;
  FBooleanFormat := TfcxFormat.Create;
  FTextFormat := TfcxFormat.Create;
  FPercentFormat := TfcxFormat.Create;

  TfcxFormatHack(FDateFormat).FDatePathFormat.Free;
  TfcxFormatHack(FDateFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FDateTimeFormat).FDatePathFormat.Free;
  TfcxFormatHack(FDateTimeFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FTimeFormat).FDatePathFormat.Free;
  TfcxFormatHack(FTimeFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FFloatFormat).FDatePathFormat.Free;
  TfcxFormatHack(FFloatFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FCurrencyFormat).FDatePathFormat.Free;
  TfcxFormatHack(FCurrencyFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FIntegerFormat).FDatePathFormat.Free;
  TfcxFormatHack(FIntegerFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FBooleanFormat).FDatePathFormat.Free;
  TfcxFormatHack(FBooleanFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FTextFormat).FDatePathFormat.Free;
  TfcxFormatHack(FTextFormat).FDatePathFormat := FDatePathFormat;
  TfcxFormatHack(FPercentFormat).FDatePathFormat.Free;
  TfcxFormatHack(FPercentFormat).FDatePathFormat := FDatePathFormat;
  SetDefaults;
  FDatePathFormat.OnChange := DoDatePathFormatChange;
  DateFormat.OnChange := DoFormatChange;
  DateTimeFormat.OnChange := DoFormatChange;
  TimeFormat.OnChange := DoFormatChange;
  FloatFormat.OnChange := DoFormatChange;
  CurrencyFormat.OnChange := DoFormatChange;
  IntegerFormat.OnChange := DoFormatChange;
  BooleanFormat.OnChange := DoFormatChange;
  TextFormat.OnChange := DoFormatChange;
  PercentFormat.OnChange := DoFormatChange;
end;

function TfcxDefaultFormat.DefaultFormatByType(
  ADataType: TfcxDataType): TfcxFormat;
begin
  case ADataType of
    fcdt_Integer, fcdt_Byte, fcdt_Word, fcdt_SmallInteger, fcdt_LargeInteger:
      Result := FIntegerFormat;
    fcdt_DateTime:
      Result := FDateTimeFormat;
    fcdt_Date:
      Result := FDateFormat;
    fcdt_Time:
      Result := FTimeFormat;
    fcdt_Double, fcdt_BCD:
      Result := FFloatFormat;
    fcdt_String, fcdt_WideString:
      Result := FTextFormat;
    fcdt_Boolean:
      Result := FBooleanFormat;
    fcdt_Currency:
      Result := FCurrencyFormat;
  else
    Result := FTextFormat;
  end;
//  Result.DatePathFormat := FDatePathFormat;
end;

destructor TfcxDefaultFormat.Destroy;
begin
  FDatePathFormat.Free;

  TfcxFormatHack(FDateFormat).FDatePathFormat := nil;
  TfcxFormatHack(FDateTimeFormat).FDatePathFormat := nil;
  TfcxFormatHack(FTimeFormat).FDatePathFormat := nil;
  TfcxFormatHack(FFloatFormat).FDatePathFormat := nil;
  TfcxFormatHack(FCurrencyFormat).FDatePathFormat := nil;
  TfcxFormatHack(FIntegerFormat).FDatePathFormat := nil;
  TfcxFormatHack(FBooleanFormat).FDatePathFormat := nil;
  TfcxFormatHack(FTextFormat).FDatePathFormat := nil;
  TfcxFormatHack(FPercentFormat).FDatePathFormat := nil;

  FDateFormat.Free;
  FTimeFormat.Free;
  FDateTimeFormat.Free;
  FFloatFormat.Free;
  FCurrencyFormat.Free;
  FIntegerFormat.Free;
  FBooleanFormat.Free;
  FTextFormat.Free;
  FPercentFormat.Free;

  inherited;
end;

procedure TfcxDefaultFormat.DoDatePathFormatChange(Sender: TObject);
begin

end;

procedure TfcxDefaultFormat.DoFormatChange(Sender: TObject);
begin

end;

function TfcxDefaultFormat.GetBooleanFormat: TfcxTypeFormat;
begin
  Result := FBooleanFormat.TypeFormat
end;

function TfcxDefaultFormat.GetCurrencyFormat: TfcxTypeFormat;
begin
  Result := FCurrencyFormat.TypeFormat
end;

function TfcxDefaultFormat.GetDateFormat: TfcxTypeFormat;
begin
  Result := FDateFormat.TypeFormat
end;

function TfcxDefaultFormat.GetDatePathFormat: TfcxDatePathFormat;
begin
  Result := FDatePathFormat
end;

function TfcxDefaultFormat.GetDateTimeFormat: TfcxTypeFormat;
begin
  Result := FDateTimeFormat.TypeFormat
end;

function TfcxDefaultFormat.GetFloatFormat: TfcxTypeFormat;
begin
  Result := FFloatFormat.TypeFormat
end;

function TfcxDefaultFormat.GetIntegerFormat: TfcxTypeFormat;
begin
  Result := FIntegerFormat.TypeFormat
end;

function TfcxDefaultFormat.GetPercentFormat: TfcxTypeFormat;
begin
  Result := FPercentFormat.TypeFormat
end;

function TfcxDefaultFormat.GetTextFormat: TfcxTypeFormat;
begin
  Result := FTextFormat.TypeFormat
end;

function TfcxDefaultFormat.GetTimeFormat: TfcxTypeFormat;
begin
  Result := FTimeFormat.TypeFormat
end;

procedure TfcxDefaultFormat.SetBooleanFormat(const Value: TfcxTypeFormat);
begin
  FBooleanFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetCurrencyFormat(const Value: TfcxTypeFormat);
begin
  FCurrencyFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetDateFormat(const Value: TfcxTypeFormat);
begin
  FDateFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetDatePathFormat(
  const Value: TfcxDatePathFormat);
begin
  FDatePathFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetDateTimeFormat(const Value: TfcxTypeFormat);
begin
  FDateTimeFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetDefaults;
begin
  FDateFormat.TypeFormat.Kind := fkDateTime;
  FDateFormat.TypeFormat.FormatStr := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  FDateFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FTimeFormat.TypeFormat.Kind := fkDateTime;
  FTimeFormat.TypeFormat.FormatStr := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortTimeFormat;
  FTimeFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FDateTimeFormat.TypeFormat.Kind := fkDateTime;
  FDateTimeFormat.TypeFormat.FormatStr := {ShortTimeFormat + ' ' + }{$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDateFormat;
  FDateTimeFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FFloatFormat.TypeFormat.Kind := fkNumeric;
  FFloatFormat.TypeFormat.FormatStr := '%2.2n';
  FFloatFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FCurrencyFormat.TypeFormat.Kind := fkNumeric;
  FCurrencyFormat.TypeFormat.FormatStr := '%2.2m';
  FCurrencyFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FIntegerFormat.TypeFormat.Kind := fkNumeric;
  FIntegerFormat.TypeFormat.FormatStr := '%g';
  FIntegerFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FBooleanFormat.TypeFormat.Kind := fkBoolean;
  FBooleanFormat.TypeFormat.FormatStr := 'False,True';
  FBooleanFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
(* TODO:
  FBooleanFormat.FormatStr := fcxResources.GetAnsi('sFalse') + ',' + fcxResources.GetAnsi('sTrue');
*)
  FTextFormat.TypeFormat.Kind := fkText;
  FTextFormat.TypeFormat.FormatStr := '';
  FTextFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  FPercentFormat.TypeFormat.Kind := fkNumeric;
  FPercentFormat.TypeFormat.FormatStr := '%2.2n';
  FPercentFormat.TypeFormat.DecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
end;

procedure TfcxDefaultFormat.SetFloatFormat(const Value: TfcxTypeFormat);
begin
  FFloatFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetIntegerFormat(const Value: TfcxTypeFormat);
begin
  FIntegerFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetPercentFormat(const Value: TfcxTypeFormat);
begin
  FPercentFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetTextFormat(const Value: TfcxTypeFormat);
begin
  FTextFormat.TypeFormat.Assign(Value);
end;

procedure TfcxDefaultFormat.SetTimeFormat(const Value: TfcxTypeFormat);
begin
  FTimeFormat.TypeFormat.Assign(Value);
end;

{ TfcxDefaultSetting }

constructor TfcxDefaultSettings.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TfcxDefaultSettings.Destroy;
begin
  inherited;
end;

function TfcxDefaultSettings.GetColWidth: Integer;
begin
  Result := fcDefaultSettingsStore.ColWidth;
end;

function TfcxDefaultSettings.GetFormat: TfcxDefaultFormat;
begin
  Result := fcDefaultSettingsStore.Format;
end;

function TfcxDefaultSettings.GetRowHeight: Integer;
begin
  Result := fcDefaultSettingsStore.RowHeight;
end;

function TfcxDefaultSettings.GetSliceSettings: TfcxSliceDefaultSettings;
begin
  Result := fcDefaultSettingsStore.SliceSettings;
end;

function TfcxDefaultSettings.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxDefaultSettings.SetColWidth(const Value: Integer);
begin
  fcDefaultSettingsStore.ColWidth := Value;
end;

procedure TfcxDefaultSettings.SetDefaults;
begin
  fcDefaultSettingsStore.SetDefaults;
end;

procedure TfcxDefaultSettings.SetFormat(const Value: TfcxDefaultFormat);
begin
  fcDefaultSettingsStore.Format := Value;
end;

procedure TfcxDefaultSettings.SetRowHeight(const Value: Integer);
begin
  fcDefaultSettingsStore.RowHeight := Value;
end;

procedure TfcxDefaultSettings.SetSliceSettings(
  const Value: TfcxSliceDefaultSettings);
begin
  fcDefaultSettingsStore.SliceSettings := Value;
end;

{ TfcxDefaultSettingsStore }

constructor TfcxDefaultSettingsStore.Create;
begin
  FFormat := TfcxDefaultFormat.Create;
  FSliceSettings := TfcxSliceDefaultSettings.Create;
  SetDefaults;
end;

destructor TfcxDefaultSettingsStore.Destroy;
begin
  FSliceSettings.Free;
  FFormat.Free;
  inherited;
end;

procedure TfcxDefaultSettingsStore.SetDefaults;
begin
  FFormat.SetDefaults;
  FSliceSettings.SetDefaults;
  FRowHeight := cfcRowHeight;
  FColWidth := cfcColWidth;
end;

procedure TfcxDefaultSettingsStore.SetFormat(const Value: TfcxDefaultFormat);
begin
  FFormat.Assign(Value);
end;

procedure TfcxDefaultSettingsStore.SetSliceSettings(
  const Value: TfcxSliceDefaultSettings);
begin
  FSliceSettings.Assign(Value);
end;

{ TfcxAxisDefaultSettings }

procedure TfcxAxisDefaultSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxAxisDefaultSettings then
    with TfcxAxisDefaultSettings(Dest) do
    begin
      GrandTotalPosition := Self.GrandTotalPosition;
      ShowTotalAs := Self.ShowTotalAs;
    end
  else inherited AssignTo(Dest);
end;

constructor TfcxAxisDefaultSettings.Create;
begin
  FGrandTotalPosition := fctp_Before;
  FShowTotalAs := sta_AsTotal;
end;

destructor TfcxAxisDefaultSettings.Destroy;
begin
  inherited;
end;

procedure TfcxAxisDefaultSettings.SetDefaults;
begin
  GrandTotalPosition := fctp_Before;
  ShowTotalAs := sta_AsTotal;
end;

procedure TfcxAxisDefaultSettings.SetGrandTotalPosition(
  const Value: TfcxTotalPosition);
begin
  FGrandTotalPosition := Value;
end;

procedure TfcxAxisDefaultSettings.SetShowTotalAs(
  const Value: TfcxShowTotalAs);
begin
  FShowTotalAs := Value;
end;

{ TfcxSliceDefaultSettings }

procedure TfcxSliceDefaultSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxSliceDefaultSettings then
    with TfcxSliceDefaultSettings(Dest) do
    begin
      Self.XAxisSettings.AssignTo(XAxisSettings);
      Self.YAxisSettings.AssignTo(YAxisSettings);
      Self.AxisFieldSettings.AssignTo(AxisFieldSettings);
    end
  else inherited AssignTo(Dest);
end;

constructor TfcxSliceDefaultSettings.Create;
begin
  FXAxisSettings := TfcxAxisDefaultSettings.Create;
  FYAxisSettings := TfcxAxisDefaultSettings.Create;
  FAxisFieldSettings := TfcxAxisFieldDefaultSettings.Create;
end;

destructor TfcxSliceDefaultSettings.Destroy;
begin
  FAxisFieldSettings.Free;
  FYAxisSettings.Free;
  FXAxisSettings.Free;
  inherited;
end;

procedure TfcxSliceDefaultSettings.SetAxisFieldSettings(
  const Value: TfcxAxisFieldDefaultSettings);
begin
  FAxisFieldSettings.Assign(Value);
end;

procedure TfcxSliceDefaultSettings.SetDefaults;
begin
  FXAxisSettings.SetDefaults;
  FYAxisSettings.SetDefaults;
  FAxisFieldSettings.SetDefaults;
end;

procedure TfcxSliceDefaultSettings.SetXAxisSettings(
  const Value: TfcxAxisDefaultSettings);
begin
  FXAxisSettings.Assign(Value);
end;

procedure TfcxSliceDefaultSettings.SetYAxisSettings(
  const Value: TfcxAxisDefaultSettings);
begin
  FYAxisSettings.Assign(Value);
end;

{ TfcxAxisFieldDefaultSettings }

procedure TfcxAxisFieldDefaultSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxAxisFieldDefaultSettings then
    with TfcxAxisFieldDefaultSettings(Dest) do
    begin
      TotalPosition := Self.TotalPosition;
      SortDirection := Self.SortDirection;
    end
  else inherited AssignTo(Dest);
end;

constructor TfcxAxisFieldDefaultSettings.Create;
begin
  FTotalPosition := fctp_Before;
  FSortDirection := fcsd_Asc;
end;

destructor TfcxAxisFieldDefaultSettings.Destroy;
begin
  inherited;
end;

procedure TfcxAxisFieldDefaultSettings.SetDefaults;
begin
  TotalPosition := fctp_Before;
  SortDirection := fcsd_Asc;
end;

procedure TfcxAxisFieldDefaultSettings.SetSortDirection(
  const Value: TfcxSortDirection);
begin
  FSortDirection := Value;
end;

procedure TfcxAxisFieldDefaultSettings.SetTotalPosition(
  const Value: TfcxTotalPosition);
begin
  FTotalPosition := Value;
end;

procedure TfcxDefaultSettings.SetVersion(const Value: String);
begin
//
end;

initialization

finalization
  if FfcxDefaultSettingsStore <> nil then
    FfcxDefaultSettingsStore.Free;
  FfcxDefaultSettingsStore := nil;

end.
