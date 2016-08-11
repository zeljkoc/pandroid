{*******************************************************}
{                                                       }
{              FastCube 2 Formats unit                  }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxFormats;
{$INCLUDE fcx.inc}

interface
uses
  Classes, SysUtils, TypInfo,
  fcxXML, fcxTypes
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
  FMX.fcxXML, FMX.fcxTypes;
{$ENDIF FMX}

type

  TfcxFormatKind = (
    fkText,
    fkNumeric,
    fkDateTime,
    fkBoolean,
    fkCustom
  );

  TfcxTypeFormat = class(TPersistent)
  private
    FKind: TfcxFormatKind;
    FFormatStr: TfcxString;
    FDecimalSeparator: TfcxString;
    FOnChange: TNotifyEvent;
    procedure SetDecimalSeparator(const Value: TfcxString);
    procedure SetFormatStr(const Value: TfcxString);
    procedure SetKind(const Value: TfcxFormatKind);
  protected
    procedure Changed;
  public
    constructor Create; virtual;
    function Equal(ATypeFormat: TfcxTypeFormat): Boolean; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ToXML(AItem: TfcxXMLItem);
    procedure FromXML(AItem: TfcxXMLItem);
  published
    property DecSeparator: TfcxString read FDecimalSeparator write SetDecimalSeparator;
    property FormatStr: TfcxString read FFormatStr write SetFormatStr;
    property Kind: TfcxFormatKind read FKind write SetKind default fkText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TfcxDatePathFormat = class(TPersistent)
  private
    FQuarterDisplayFormat: TfcxQuarterDisplayFormat;
    FWeekDayDisplayFormat: TfcxWeekDayDisplayFormat;
    FWeekNumberDisplayFormat: TfcxWeekNumberDisplayFormat;
    FMonthDisplayFormat: TfcxMonthDisplayFormat;
    FOnChange: TNotifyEvent;
    procedure SetMonthDisplayFormat(const Value: TfcxMonthDisplayFormat);
    procedure SetQuarterDisplayFormat(
      const Value: TfcxQuarterDisplayFormat);
    procedure SetWeekDayDisplayFormat(
      const Value: TfcxWeekDayDisplayFormat);
    procedure SetWeekNumberDisplayFormat(
      const Value: TfcxWeekNumberDisplayFormat);
  protected
    procedure Changed;
  public
    procedure SetDefaults;
    constructor Create; virtual;
    function Equal(ADatePathFormat: TfcxDatePathFormat): Boolean; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ToXML(AItem: TfcxXMLItem);
    procedure FromXML(AItem: TfcxXMLItem);
  published
    property MonthDisplayFormat: TfcxMonthDisplayFormat read FMonthDisplayFormat write SetMonthDisplayFormat;
    property WeekDayDisplayFormat: TfcxWeekDayDisplayFormat read FWeekDayDisplayFormat write SetWeekDayDisplayFormat;
    property QuarterDisplayFormat: TfcxQuarterDisplayFormat read FQuarterDisplayFormat write SetQuarterDisplayFormat;
    property WeekNumberDisplayFormat: TfcxWeekNumberDisplayFormat read FWeekNumberDisplayFormat write SetWeekNumberDisplayFormat;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TfcxFormat = class(TPersistent)
  private
    FUniqueArray: TObject;
    FOnChange: TNotifyEvent;
    procedure SetTypeFormat(const Value: TfcxTypeFormat);
    procedure SetDatePathFormat(const Value: TfcxDatePathFormat);
  protected
    FTypeFormat: TfcxTypeFormat;
    FDatePathFormat: TfcxDatePathFormat;
    procedure DoFormatChange(Sender: TObject);
    procedure Changed;
  public
    constructor Create; overload; virtual;
    constructor Create(AUniqueArray: TObject); overload; virtual;
(*
    constructor Create(AUniqueArray: TObject; AfcFormatRec: TfcxFormatRec); overload; virtual;
*)
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    function Equal(AFormat: TfcxFormat): Boolean; virtual;
    function FormatData(AVarValue: Variant; ADataTypeProcessor: TObject {TfcxDataTypeProcessor}): TfcxString;
    function FormatDatePath(AValue: Word; ADateType: TfcxDateType; ADateTimeConsts: TfcxDateTimeConsts): TfcxString;
    function FormatTimePath(AValue: Word; ATimeType: TfcxTimeType): TfcxString;
    procedure ToXML(AItem: TfcxXMLItem);
    procedure FromXML(AItem: TfcxXMLItem);
    property TypeFormat: TfcxTypeFormat read FTypeFormat write SetTypeFormat;
    property DatePathFormat: TfcxDatePathFormat read FDatePathFormat write SetDatePathFormat;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
//VCL uses section
{$IFNDEF FMX}
uses
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  fcxRes,
  fcxCustomFormat,
  fcxUniqueValue,
  fcxUniqueArray,
  fcxCube;
//FMX uses
{$ELSE FMX}
uses
  System.Variants,
  FMX.fcxRes,
  FMX.fcxCustomFormat,
  FMX.fcxUniqueValue,
  FMX.fcxUniqueArray,
  FMX.fcxCube;
{$ENDIF FMX}

{ TfcxTypeFormat }

procedure TfcxTypeFormat.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxTypeFormat then
    with TfcxTypeFormat(Dest) do
    begin
      FKind := Self.Kind;
      FFormatStr := Self.FormatStr;
      FDecimalSeparator := Self.DecSeparator;
      Changed;
    end
  else inherited AssignTo(Dest);
end;

procedure TfcxTypeFormat.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TfcxTypeFormat.Equal(ATypeFormat: TfcxTypeFormat): Boolean;
begin
  Result := (FKind = ATypeFormat.Kind)
            and (FFormatStr = ATypeFormat.FormatStr)
            and (FDecimalSeparator = ATypeFormat.DecSeparator);
end;

constructor TfcxTypeFormat.Create;
begin
  FKind := fkText;
  FFormatStr := '';
  FDecimalSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
end;

procedure TfcxTypeFormat.FromXML(AItem: TfcxXMLItem);
begin
  FDecimalSeparator := AItem.Prop['decimal_separator'];
  FFormatStr := AItem.Prop['format_str'];
  FKind := TfcxFormatKind(GetEnumValue(TypeInfo(TfcxFormatKind), AItem.Prop['kind']));
  Changed;
end;

procedure TfcxTypeFormat.SetDecimalSeparator(const Value: TfcxString);
begin
  if FDecimalSeparator <> Value then
  begin
    FDecimalSeparator := Value;
    Changed;
  end;
end;

procedure TfcxTypeFormat.SetFormatStr(const Value: TfcxString);
begin
  if FFormatStr <> Value then
  begin
    FFormatStr := Value;
    Changed;
  end;
end;

procedure TfcxTypeFormat.SetKind(const Value: TfcxFormatKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TfcxTypeFormat.ToXML(AItem: TfcxXMLItem);
begin
  AItem.Prop['decimal_separator'] := FDecimalSeparator;
  AItem.Prop['format_str'] := FFormatStr;
  AItem.Prop['kind'] := GetEnumName(TypeInfo(TfcxFormatKind), Ord(FKind));
end;

{ TfcxDatePathFormat }

procedure TfcxDatePathFormat.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxDatePathFormat then
    with TfcxDatePathFormat(Dest) do
    begin
      FQuarterDisplayFormat := Self.QuarterDisplayFormat;
      FWeekDayDisplayFormat := Self.WeekDayDisplayFormat;
      FWeekNumberDisplayFormat := Self.WeekNumberDisplayFormat;
      FMonthDisplayFormat := Self.MonthDisplayFormat;
      Changed;
    end
  else inherited AssignTo(Dest);
end;

procedure TfcxDatePathFormat.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TfcxDatePathFormat.Equal(
  ADatePathFormat: TfcxDatePathFormat): Boolean;
begin
  Result := (FQuarterDisplayFormat = ADatePathFormat.QuarterDisplayFormat)
            and (FWeekDayDisplayFormat = ADatePathFormat.WeekDayDisplayFormat)
            and (FWeekNumberDisplayFormat = ADatePathFormat.WeekNumberDisplayFormat)
            and (FMonthDisplayFormat = ADatePathFormat.MonthDisplayFormat);
end;

constructor TfcxDatePathFormat.Create;
begin
  SetDefaults;
end;

procedure TfcxDatePathFormat.FromXML(AItem: TfcxXMLItem);
begin
  FQuarterDisplayFormat := TfcxQuarterDisplayFormat(GetEnumValue(TypeInfo(TfcxQuarterDisplayFormat), AItem.Prop['QuarterDisplayFormat']));
  FWeekDayDisplayFormat := TfcxWeekDayDisplayFormat(GetEnumValue(TypeInfo(TfcxWeekDayDisplayFormat), AItem.Prop['WeekDayDisplayFormat']));
  FWeekNumberDisplayFormat := TfcxWeekNumberDisplayFormat(GetEnumValue(TypeInfo(TfcxWeekNumberDisplayFormat), AItem.Prop['WeekNumberDisplayFormat']));
  FMonthDisplayFormat := TfcxMonthDisplayFormat(GetEnumValue(TypeInfo(TfcxMonthDisplayFormat), AItem.Prop['MonthDisplayFormat']));
end;

procedure TfcxDatePathFormat.SetDefaults;
begin
  FMonthDisplayFormat := mdf_Long;
  FQuarterDisplayFormat := qdf_System;
  FWeekDayDisplayFormat := wddf_Long;
  FWeekNumberDisplayFormat := wndf_System;
  Changed;
end;

procedure TfcxDatePathFormat.SetMonthDisplayFormat(
  const Value: TfcxMonthDisplayFormat);
begin
  if FMonthDisplayFormat <> Value then
  begin
    FMonthDisplayFormat := Value;
    Changed;
  end;
end;

procedure TfcxDatePathFormat.SetQuarterDisplayFormat(
  const Value: TfcxQuarterDisplayFormat);
begin
  if FQuarterDisplayFormat <> Value then
  begin
    FQuarterDisplayFormat := Value;
    Changed;
  end;
end;

procedure TfcxDatePathFormat.SetWeekDayDisplayFormat(
  const Value: TfcxWeekDayDisplayFormat);
begin
  if FWeekDayDisplayFormat <> Value then
  begin
    FWeekDayDisplayFormat := Value;
    Changed;
  end;
end;

procedure TfcxDatePathFormat.SetWeekNumberDisplayFormat(
  const Value: TfcxWeekNumberDisplayFormat);
begin
  if FWeekNumberDisplayFormat <> Value then
  begin
    FWeekNumberDisplayFormat := Value;
    Changed;
  end;
end;

procedure TfcxDatePathFormat.ToXML(AItem: TfcxXMLItem);
begin
  AItem.Prop['QuarterDisplayFormat'] := GetEnumName(TypeInfo(TfcxQuarterDisplayFormat), Ord(FQuarterDisplayFormat));
  AItem.Prop['WeekDayDisplayFormat'] := GetEnumName(TypeInfo(TfcxWeekDayDisplayFormat), Ord(FWeekDayDisplayFormat));
  AItem.Prop['WeekNumberDisplayFormat'] := GetEnumName(TypeInfo(TfcxWeekNumberDisplayFormat), Ord(FWeekNumberDisplayFormat));
  AItem.Prop['MonthDisplayFormat'] := GetEnumName(TypeInfo(TfcxMonthDisplayFormat), Ord(FMonthDisplayFormat));
end;

{ TfcxFormat }

procedure TfcxFormat.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxFormat then
    with TfcxFormat(Dest) do
    begin
      TypeFormat.Assign(Self.TypeFormat);
      DatePathFormat.Assign(Self.DatePathFormat);
    end
  else inherited AssignTo(Dest);
end;

procedure TfcxFormat.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TfcxFormat.Equal(AFormat: TfcxFormat): Boolean;
begin
  if Assigned(AFormat) then
    Result := FTypeFormat.Equal(AFormat.FTypeFormat) and FDatePathFormat.Equal(AFormat.FDatePathFormat)
  else
    Result :=  False;  
end;

(*
constructor TfcxFormat.Create(AUniqueArray: TObject; AfcFormatRec: TfcxFormatRec);
begin
  FTypeFormat := TfcxTypeFormat.Create;
  FTypeFormat.OnChange := DoFormatChange;
  FDatePathFormat := TfcxDatePathFormat.Create;
  FDatePathFormat.OnChange := DoFormatChange;
  if AUniqueArray is TfcxBaseUniqueValues then
    FUniqueArray := AUniqueArray
  else
    FUniqueArray := nil;
  if AfcFormatRec.TypeFormat <> nil then
    FTypeFormat.Assign(AfcFormatRec.TypeFormat);
  if AfcFormatRec.DatePathFormat <> nil then
    FDatePathFormat.Assign(AfcFormatRec.DatePathFormat);
end;
*)

constructor TfcxFormat.Create;
(*
var
 AFormatRec : TfcxFormatRec;
*)
begin
(*
  AFormatRec.DatePathFormat := nil;
  AFormatRec.TypeFormat := nil;
*)
  Create(nil(*, AFormatRec*));
end;

constructor TfcxFormat.Create(AUniqueArray: TObject);
(*
var
 AFormatRec : TfcxFormatRec;
*)
begin
(*
  AFormatRec.DatePathFormat := nil;
  AFormatRec.TypeFormat := nil;
  Create(AUniqueArray, AFormatRec);
*)
  FTypeFormat := TfcxTypeFormat.Create;
  FTypeFormat.OnChange := DoFormatChange;
  FDatePathFormat := TfcxDatePathFormat.Create;
  FDatePathFormat.OnChange := DoFormatChange;
  if AUniqueArray is TfcxBaseUniqueValues then
    FUniqueArray := AUniqueArray
  else
    FUniqueArray := nil;
end;

destructor TfcxFormat.Destroy;
begin
  FreeAndNil(FTypeFormat);
  FreeAndNil(FDatePathFormat);
  inherited;
end;

procedure TfcxFormat.DoFormatChange(Sender: TObject);
begin
  Changed;
end;

function TfcxFormat.FormatData(AVarValue: Variant; ADataTypeProcessor: TObject {TfcxDataTypeProcessor}): TfcxString;
var
  DataTypeProcessor: TfcxDataTypeProcessor absolute ADataTypeProcessor;

  i, ACustomFormatIndex: integer;
  function WithIntType(FormatStr: TfcxString): boolean;
  var
    i: integer;
    AIsFormat: boolean;
  begin
    Result := False;
    AIsFormat := False;
    for i := 1 to Length(FormatStr) do
    begin
      if FormatStr[i] = '%' then
      begin
        AIsFormat := True;
        Continue;
      end;
      if not AIsFormat then
        Continue;
{$IFDEF DELPHI_12UP}
      if CharInSet(FormatStr[i], ['D', 'U', 'd', 'u']) then
{$ELSE}
      if FormatStr[i] in ['D', 'U', 'd', 'u'] then
{$ENDIF}
      begin
        Result := True;
        Exit;
      end;
{$IFDEF DELPHI_12UP}
      if CharInSet(FormatStr[i], ['E', 'F', 'G', 'N', 'M', 'P', 'S', 'X', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x']) then
{$ELSE}
      if FormatStr[i] in ['E', 'F', 'G', 'N', 'M', 'P', 'S', 'X', 'e', 'f', 'g', 'n', 'm', 'p', 's', 'x'] then
{$ENDIF}
      begin
        AIsFormat := False;
        Continue;
      end;
{$IFDEF DELPHI_12UP}
      if CharInSet(FormatStr[i], ['*', ',', '-', '.', '0' .. '9', ':']) then
{$ELSE}
      if FormatStr[i] in ['*', ',', '-', '.', '0' .. '9', ':'] then
{$ENDIF}
        Continue;
    end;
  end;
begin
  if VarIsNull(AVarValue) then
    if DataTypeProcessor = nil then
      Result := ''
    else
      Result := DataTypeProcessor.NullCaption
  else
  if (TypeFormat.Kind = fkText) or (TVarData(AVarValue).VType = varOleStr) or (TVarData(AVarValue).VType = varString) then
    Result := fcxVarToStr(AVarValue)
  else
  try
    case TypeFormat.Kind of
      fkNumeric:
        begin
          if (TypeFormat.FormatStr <> '') and (TypeFormat.FormatStr[1] = '#') then
            Result := FormatFloat(TypeFormat.FormatStr, Extended(AVarValue))
          else if WithIntType(TypeFormat.FormatStr) then
            Result := Format(TypeFormat.FormatStr, [Integer(AVarValue)])
          else
            Result := Format(TypeFormat.FormatStr, [Extended(AVarValue)]);
          if (Length(TypeFormat.DecSeparator) = 1) and
            ({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> Char(TypeFormat.DecSeparator[1])) then
            for i := 1 to Length(Result) do
              if Result[i] = TfcxChar({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then
                Result[i] := TfcxChar(TypeFormat.DecSeparator[1]);
        end;

      fkDateTime:
        Result := FormatDateTime(TypeFormat.FormatStr, AVarValue);

      fkBoolean:
        if AVarValue = True then
          Result := Copy(TypeFormat.FormatStr, Pos(',', TypeFormat.FormatStr) + 1, 255) else
          Result := Copy(TypeFormat.FormatStr, 1, Pos(',', TypeFormat.FormatStr) - 1);
      fkCustom:
        begin
          ACustomFormatIndex := fcCustomFormats.FindFormat(TypeFormat.FormatStr);
          if ACustomFormatIndex = -1 then
            Result := fcxVarToStr(AVarValue)
          else
          begin
            Result := fcCustomFormats[ACustomFormatIndex].FormatData(AVarValue, TypeFormat.DecSeparator)
          end;
        end;
      else
        Result := fcxVarToStr(AVarValue)
    end;
  except
    Result := fcxVarToStr(AVarValue);
  end;
end;

function TfcxFormat.FormatDatePath(AValue: Word; ADateType: TfcxDateType; ADateTimeConsts: TfcxDateTimeConsts): TfcxString;
begin
  case ADateType of
    odt_Month:
      case FDatePathFormat.MonthDisplayFormat of
        mdf_Long:
          Result := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongMonthNames[AValue];
        mdf_Short:
          Result := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortMonthNames[AValue];
        mdf_Custom:
          if (FUniqueArray <> nil) and (FUniqueArray is TfcxCommonDatePathField) then
            Result := TfcxCommonDatePathField(FUniqueArray).CustomMonthNames(AValue)
          else
{ TODO -cНеобходимо : Перенести в ресурсы.}
            Result := 'Custom format not assigned';
      end;
    odt_DayOfWeek:
      begin
        if ADateTimeConsts.DayOfWeekISO8601 then
          AValue := AValue mod 7 + 1;
        case FDatePathFormat.WeekDayDisplayFormat of
          wddf_Long:
            Result := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongDayNames[AValue];
          wddf_Short:
            Result := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDayNames[AValue];
          wddf_Custom:
            if (FUniqueArray <> nil) and (FUniqueArray is TfcxCommonDatePathField) then
              Result := TfcxCommonDatePathField(FUniqueArray).CustomWeekDayNames(AValue)
            else
{ TODO -cНеобходимо : Перенести в ресурсы.}
              Result := 'Custom format not assigned';
        end;
      end;
    odt_Quarter:
      case FDatePathFormat.QuarterDisplayFormat of
        qdf_System:
          Result := fcxResources.GetAnsi('SQuarter_' + IntToStr(AValue));
        qdf_Custom:
          if (FUniqueArray <> nil) and (FUniqueArray is TfcxCommonDatePathField) then
            Result := TfcxCommonDatePathField(FUniqueArray).CustomQuarterNames(AValue)
          else
{ TODO -cНеобходимо : Перенести в ресурсы.}
            Result := 'Custom format not assigned';
      end;
    odt_WeekNumber:
      case FDatePathFormat.WeekNumberDisplayFormat of
        wndf_System:
          if AValue > 53 then
          begin
            if AValue > 199 then
{ TODO -cНеобходимо : Перенести в ресурсы.}
              Result := '1' + ' (Next Year)'
            else
              Result := IntToStr(AValue - 100)
          end
          else
{ TODO -cНеобходимо : Перенести в ресурсы.}
            Result := IntToStr(AValue) + ' (Prev Year)';
        wndf_Custom:
          if (FUniqueArray <> nil) and (FUniqueArray is TfcxCommonDatePathField) then
            if AValue > 53 then
            begin
              if AValue > 199 then
                Result := TfcxCommonDatePathField(FUniqueArray).CustomWeekNumberNames(1, 1)
              else
                Result := TfcxCommonDatePathField(FUniqueArray).CustomWeekNumberNames(AValue - 100, 0)
            end
            else
              Result := TfcxCommonDatePathField(FUniqueArray).CustomWeekNumberNames(AValue, -1)
          else
{ TODO -cНеобходимо : Перенести в ресурсы.}
            Result := 'Custom format not assigned';
      end;
  else
    Result := IntToStr(AValue);
  end;
end;

function TfcxFormat.FormatTimePath(AValue: Word;
  ATimeType: TfcxTimeType): TfcxString;
begin
  Result := IntToStr(AValue);
end;

procedure TfcxFormat.FromXML(AItem: TfcxXMLItem);
begin
  FTypeFormat.FromXML(AItem);
  FDatePathFormat.FromXML(AItem);
end;

procedure TfcxFormat.SetDatePathFormat(const Value: TfcxDatePathFormat);
begin
  FDatePathFormat.Assign(Value);
end;

procedure TfcxFormat.SetTypeFormat(const Value: TfcxTypeFormat);
begin
  FTypeFormat.Assign(Value);
end;

procedure TfcxFormat.ToXML(AItem: TfcxXMLItem);
begin
  FTypeFormat.ToXML(AItem);
  FDatePathFormat.ToXML(AItem);
end;

end.
