{*******************************************************}
{                                                       }
{               FastCube 2 Filters unit                 }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxFilters;
{$INCLUDE fcx.inc}

interface
uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLType, LCLIntf,
{$ENDIF}
  TypInfo, Classes, SysUtils,
  fcxError, fcxList, fcxTypes, fcxCube, fcxComponent, fcxAlerts, fcxXML, fcxRange;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.TypInfo, System.Classes, System.SysUtils,
  FMX.fcxError, FMX.fcxList, FMX.fcxTypes, FMX.fcxCube, 
  FMX.fcxComponent, FMX.fcxAlerts, FMX.fcxXML, FMX.fcxRange;
{$ENDIF FMX}

type

// функции фильтра
// 1. управление фильтрами
// 2. хранение отфильтрованного исходного набора
// фильтры: УЗ, составные (скрипт, выражение, event)
  TfcxFilter = class;
  TfcxFilters = class;
  TfcxUVFilter = class;
  TfcxFilterManager = class;

  TfcxFilterType =
  (
    ft_ByUniqueValue,       // simple by Unique Value
    ft_ByEvent,             // simple by Event
    ft_ByScript,            // simple by Script
    ft_BySimpleExpression   // simple by Expression
  );

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxFilterManager = class(TfcxComponent)
  private
    FCube: TfcxCube;
    FRecCount: integer;
    FGoodRecCount: integer;
// Indexes of records in good list. -1 for not admitted records
    FGoodIndexes: PfcxIntegerArray;
// Indexes of good records in full list.
    FAbsIndexes: PfcxIntegerArray;
    FFilterRecCount: integer;
    FFilterArray: PfcxByteArray;
// Список обработчиков фильтров (фильтр уникальных и т.д.)
    FFilters: TfcxFilters;
    FFieldCount: Integer;
// Set of changes in slice
    FChanges: TfcxChangesInFilters;
// update transaction started!
    FUpdating: boolean;
    FStoreEnabledValues: boolean;
    FOpened: Boolean;
    FOnlyAvailableSupport: boolean;
    procedure ResetListner(AComponent: TComponent);
    procedure SetCube(const Value: TfcxCube);
    function GetCube: TfcxCube;
    procedure Open;
    procedure Close;
    procedure FieldsAdded;
    procedure FieldDeleted(AFieldIndex: integer);
    procedure FieldSplitChanged(AFieldIndex: integer);
    procedure FieldSplitAdded(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
    procedure FieldSplitDeleted(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
    procedure DataChanged;

    procedure StopChange(AChanges: TfcxChangesInFilters = []; AIndex: integer = -1);
    procedure CubeChanged(Sender: TfcxCube; AChangeAlert: TfcxChangeAlert; Action: TBasicAction);
    function GetRecFilter(ARecIndex: integer): boolean;
    function GetFilter(AIndex: Integer): TfcxFilter;
    function GetUVFilter: TfcxUVFilter;
    function GetActiveFilter(AIndex: Integer): boolean;
    procedure SetActiveFilter(AIndex: Integer; const Value: boolean);
    function GetFilterByName(AName: TfcxString): TfcxFilter;
    function GetFilterIndexByName(AName: TfcxString): integer;
    procedure SetStoreEnabledValues(const Value: boolean);
    procedure AfterResetAllFilters;
    function GetGoodIndex(ARecIndex: integer): integer;
    function GetAbsIndex(AGoodIndex: integer): integer;
    procedure SetOnlyAvailableSupport(const Value: boolean);
    procedure SetAvailablesFlags(ARecIndex: integer; AFilteredByOtherFilters: Boolean);
  protected
    procedure StartChangeExt;
    procedure StopChangeExt(AChanges: TfcxChangesInFilters = []; AIndex: integer = -1);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action : TBasicAction) : Boolean; override;

    function GetCommonScript: TStrings; override;
    function GetCommonScriptChangedTick: Cardinal; override;

    procedure CalcFilter;

    function CreateEventFilter(AName: TfcxString; ACaption: TfcxString{; AEventProc: TfcxFilterEvent = nil}): integer;
    function CreateScriptFilter(AName: TfcxString; ACaption: TfcxString; AScript: TfcxString): integer;
    function CreateExpressionFilter(AName: TfcxString; ACaption: TfcxString; AExpression: TfcxString): integer;
    procedure DeleteFilter(AName: TfcxString);

    procedure ResetAllFilters;

// for non auto filters
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RollBack;

    function LoadFromStream(AStream: TStream): Boolean;
    procedure SaveToStream(AStream: TStream);

    function LoadFromFile(AFileName: String): Boolean;
    procedure SaveToFile(AFileName: String);

    function LoadFromXML(AXMLDoc: TfcxXMLDocument): Boolean;
    procedure SaveToXML(AXMLDoc: TfcxXMLDocument);

    function LoadFromXMLItem(AItem: TfcxXMLItem): Boolean;
    procedure SaveToXMLItem(AItem: TfcxXMLItem);


{ TODO -cНеобходимо : сделать опции.}
    property GoodRecCount: integer read FGoodRecCount;
    property GoodIndex[ARecIndex: integer]: integer read GetGoodIndex;
    property AbsIndex[AGoodIndex: integer]: integer read GetAbsIndex;
    property FilterRecCount: integer read FFilterRecCount;
// if True then record is not necessary to process !!!
    property RecFilter[ARecIndex: integer]: boolean read GetRecFilter;
    property Filter[AIndex: Integer]: TfcxFilter read GetFilter;
    property FilterByName[AName: TfcxString]: TfcxFilter read GetFilterByName;
    property FilterIndexByName[AName: TfcxString]: integer read GetFilterIndexByName;
    property UVFilter: TfcxUVFilter read GetUVFilter;
    property ActiveFilter[AIndex: Integer]: boolean read GetActiveFilter write SetActiveFilter;
  published
    property StoreEnabledValues: boolean read FStoreEnabledValues write SetStoreEnabledValues default False;
    property OnlyAvailableSupport: boolean read FOnlyAvailableSupport write SetOnlyAvailableSupport default True;
    property Cube: TfcxCube read GetCube write SetCube;
  end;

  TfcxFilters = class(TfcxList)
  private
    FFilterManager: TfcxFilterManager;
  protected
  public
    constructor Create(AFilterManager: TfcxFilterManager); overload;
    destructor  Destroy; override;
  end;

// базовый класс для обработчиков фильтра
  TfcxFilter = class
  private
(*
    FFilterArray: PfcxByteArray;
    FSaveRecCount: integer;
*)
    FFilterRecCount: integer;
    FFilterType: TfcxFilterType;
    FFilterManager: TfcxFilterManager;
// Active or Disable filter
    FActive: boolean;
    FCaption: TfcxString;
    FName: TfcxString;
    procedure Open; virtual;
    procedure Close; virtual;
    function StartCalcFilter: boolean;
    procedure StopCalcFilter;
// True if ARecIndex does not pass through Filter !!!
    function CalcFilterOnRec(ARecIndex: integer): boolean;
// True if ARecIndex passes through Filter !!!
    function TestRecByFilter(ARecIndex: Integer): Boolean; virtual; abstract;
    procedure SetActive(const Value: boolean);
// for non auto filters
    procedure BeginUpdate; virtual;
    function EndUpdate: boolean; virtual;
    procedure RollBack; virtual;
    function GetDescription: TfcxString; virtual;
    procedure SetName(const Value: TfcxString);
    function GetHasFilteredValues: boolean; virtual;
  protected
  public
    constructor Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString); virtual;
    destructor  Destroy; override;
    procedure ResetFilter; virtual; abstract;
    procedure SaveToXML(AItem: TfcxXMLItem); virtual; abstract;
    procedure LoadFromXML(AItem: TfcxXMLItem); virtual; abstract;
    // CalcFilter
    property FilterType: TfcxFilterType read FFilterType;
    property Active: boolean read FActive write SetActive;
    property Name: TfcxString read FName write SetName;
    property Caption: TfcxString read FCaption write FCaption;
    property Description: TfcxString read GetDescription;
  end;

  TfcxCubeFieldFilter = class;

  TfcxNewFilterArray = class
  private
    FCubeFieldFilter: TfcxCubeFieldFilter;
    FFilters: PfcxByteArray;
    FOnlyAvailableSupport: boolean;
    FAvailableFlags: PfcxByteArray;
    FFilterCount: integer;
// Тип фильтра уникальных значений
    FUVFilterType: TfcxUVFilterType;

    // to save old filters
    FSaveFilters: PfcxByteArray;
    FSaveFilterCount: integer;
    FSaveSingleIndex: Integer;

    FLockClearSave: boolean;
    FUVSingleIndex: Integer;
    FInternalSelfResult: Boolean;
    function TestRecByFilter(ARecIndex: Integer): Boolean; virtual;
    procedure Clear; virtual;
    procedure ClearSave; virtual;
    function GetFilter(AUVIndex: Integer): Boolean;
    procedure SetFilter(AUVIndex: Integer; const Value: Boolean);
// for non auto filters
    procedure BeginUpdate; virtual;
    function EndUpdate: boolean; virtual;
    procedure RollBack; virtual;
    procedure BeginUpdateField; virtual;
    function EndUpdateField: boolean; virtual;
    procedure RollBackField; virtual;
    function UniqueValuesCount: integer; virtual;
    procedure SetUVFilterType(const Value: TfcxUVFilterType);
    procedure SetFilterCount(const Value: Integer);
    procedure SetUVSingleIndex(const Value: Integer);
    function GetFilterByValue(AUVVarValue: Variant): Boolean;
    procedure SetFilterByValue(AUVVarValue: Variant;
      const Value: Boolean);
    function GetUVIndex(AUVVarValue: Variant): Integer; virtual;
    function GetUValue(AUVIndex: Integer): Variant; virtual;
    function IsNull(AUVIndex: Integer): Boolean; virtual;
    procedure ResetFilter; virtual;
    procedure SaveToXML(AItem: TfcxXMLItem); virtual;
    procedure LoadFromXML(AItem: TfcxXMLItem); virtual;
    procedure SetAllFilter;  // Check all UV
    procedure InverseFilter; // Inverse
    procedure SetNoneFilter; // Uncheck all UV
    procedure SetRangeFilter(ARange: TfcxRanges);
    procedure SetOnlyAvailableSupport(const Value: boolean);
    procedure StartCalcOnlyAvailable(ANoFilters: Boolean = False);
    procedure SetAvailablesFlags(ARecIndex: integer; AFilteredByOtherFilters: Boolean);
    procedure SetAvailable(AUVIndex: Integer);
    function GetAVailable(AUVIndex: Integer): Boolean;
// Тип фильтра уникальных значений
    property UVFilterType: TfcxUVFilterType read FUVFilterType write SetUVFilterType;
    property FilterCount: Integer read FFilterCount write SetFilterCount;
    property Filter[AUVIndex: Integer]: Boolean read GetFilter write SetFilter;
    property FilterByValue[AUVVarValue: Variant]: Boolean read GetFilterByValue write SetFilterByValue;
    property UVSingleIndex: Integer read FUVSingleIndex write SetUVSingleIndex;
    property UVCount: Integer read UniqueValuesCount;
    property Available[AUVIndex: Integer]: Boolean read GetAvailable;
  public
    constructor Create(ACubeFieldFilter: TfcxCubeFieldFilter); virtual;
    destructor  Destroy; override;
  end;

  TfcxNewUVFilterArray = class(TfcxNewFilterArray)
  private
    function UniqueValuesCount: integer; override;
    function GetUVIndex(AUVVarValue: Variant): Integer; override;
    function GetUValue(AUVIndex: Integer): Variant; override;
    function IsNull(AUVIndex: Integer): Boolean; override;
  protected
  public
  end;

  TfcxNewStdSplitFilterArray = class(TfcxNewFilterArray)
  private
    function UniqueValuesCount: integer; override;
    function GetUVIndex(AUVVarValue: Variant): Integer; override;
    function GetUValue(AUVIndex: Integer): Variant; override;
    function IsNull(AUVIndex: Integer): Boolean; override;
  protected
  public
  end;

  TfcxCubeFieldFilters = class;

// обработчик фильтра на основе уникальных значений
  TfcxUVFilter = class(TfcxFilter)
  private
    FCubeFieldFilters: TfcxCubeFieldFilters;
    FCountFilteredFields: Integer;
    FInternalCountFilteredFieldsForRec: Integer;
    FInternalResultForRec: Boolean;
    function CalcFilterOnRecWithAvailable(ARecIndex: integer): boolean;
    function TestRecByFilter(ARecIndex: Integer): Boolean; override;
    function TestRecByFilterWithAvailable(ARecIndex: Integer): Boolean;
    procedure Open; override;
    procedure Close; override;
    procedure FieldsAdded;
    procedure FieldDeleted(AFieldIndex: integer);
    procedure FieldSplitChanged(AFieldIndex: integer);
    procedure FieldSplitAdded(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
    procedure FieldSplitDeleted(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);

    procedure Clear;
    function GetHasFilteredValues: boolean; override;
// for non auto filters
    procedure BeginUpdate; override;
    function EndUpdate: boolean; override;
    procedure RollBack; override;
    function GetDescription: TfcxString; override;
    function GetCubeFieldFilter(
      AField: TfcxCommonField): TfcxCubeFieldFilter;
    procedure SetOnlyAvailableSupport(const Value: boolean);
    procedure StartCalcOnlyAvailable(ANoFilters: Boolean = False);
    procedure SetAvailablesFlags(ARecIndex: integer; AFilteredByOtherFilters: Boolean);
  protected
    property CountFilteredFields: Integer read FCountFilteredFields write FCountFilteredFields;
  public
    constructor Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString); override;
    destructor  Destroy; override;
    procedure ResetFilter; override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;
    procedure LoadFromXML(AItem: TfcxXMLItem); override;
    property CubeFieldFilters: TfcxCubeFieldFilters read FCubeFieldFilters;
    property HasFilteredValues: boolean read GetHasFilteredValues;
    property CubeFieldFilter[AField: TfcxCommonField]: TfcxCubeFieldFilter read GetCubeFieldFilter;
  end;

  TfcxCubeFieldFilter = class
  private
    FUVFilter: TfcxUVFilter;
    FMasterCubeFieldFilter: TfcxCubeFieldFilter;
    FField: TfcxCommonField;
    FCubeFieldFilters: TfcxCubeFieldFilters;
    FNewFilterArray: TfcxNewFilterArray;
    FCountFilteredFields: Integer;
    FUVAvailableCount: integer;
    procedure SetCountFilteredFields(const Value: Integer);
    function TestRecByFilter(ARecIndex: Integer): Boolean; virtual;
    function TestRecByFilterWithAvailable(ARecIndex: Integer): Boolean; virtual;
    procedure Clear;
    function GetFilter(AUVIndex: Integer): Boolean;
    procedure SetFilter(AUVIndex: Integer;
      const Value: Boolean);
    function GetHasFilteredValues: boolean; virtual;
// for non auto filters
    procedure BeginUpdate; virtual;
    function EndUpdate: boolean; virtual;
    procedure RollBack; virtual;
    procedure ResetFilter; virtual;
//    function GetDescription: TfcxString; override;
    function GetFilterByValue(AUVVarValue: Variant): Boolean;
    procedure SetFilterByValue(AUVVarValue: Variant;
      const Value: Boolean);
    function GetUVCount: integer;
    function GetFilteredValuesCount: integer;
    function GetUVFilterType: TfcxUVFilterType;
    procedure SetUVFilterType(const Value: TfcxUVFilterType);
    function GetUVSingleIndex: Integer;
    procedure SetUVSingleIndex(const Value: Integer);
    procedure FieldSplitChanged;
    function FieldSplitAdded(ACubeField: TfcxCommonField; ASplitFieldIndex: integer): Boolean;
    function FieldSplitDeleted(ACubeField: TfcxCommonField; ASplitFieldIndex: integer): Boolean;
    procedure SetOnlyAvailableSupport(const Value: boolean);
    procedure StartCalcOnlyAvailable(ANoFilters: Boolean = False);
    procedure SetAvailablesFlags(ARecIndex: integer; AFilteredByOtherFilters: Boolean);
    function GetAvailable(AUVIndex: Integer): Boolean;
    function GetUVAvailableCount: integer;
  protected
    property CountFilteredFields: Integer read FCountFilteredFields write SetCountFilteredFields;
  public
    constructor Create(AUVFilter: TfcxUVFilter; AMasterCubeFieldFilter: TfcxCubeFieldFilter;
      AField: TfcxCommonField); virtual;
    destructor Destroy; override;
    procedure SaveToXML(AItem: TfcxXMLItem); virtual;
    procedure LoadFromXML(AItem: TfcxXMLItem); virtual;
    procedure SetAllFilter;
    procedure InverseFilter;
    procedure SetNoneFilter;
    procedure SetRangeFilter(ARange: TfcxRanges);
    procedure BeginUpdateField; virtual;
    function EndUpdateField: boolean; virtual;
    procedure RollBackField; virtual;
    property Filter[AUVIndex: Integer]: Boolean read GetFilter write SetFilter;
    property Available[AUVIndex: Integer]: Boolean read GetAvailable;
    property FilterByValue[AUVVarValue: Variant]: Boolean read GetFilterByValue write SetFilterByValue;
    property UVFilter: TfcxUVFilter read FUVFilter;
    property Field: TfcxCommonField read FField;
    property MasterCubeFieldFilter: TfcxCubeFieldFilter read FMasterCubeFieldFilter;
    property HasFilteredValues: boolean read GetHasFilteredValues;
    property UVCount: integer read GetUVCount;
    property UVAvailableCount: integer read GetUVAvailableCount;
    property FilteredValuesCount: integer read GetFilteredValuesCount;
    property UVFilterType: TfcxUVFilterType read GetUVFilterType write SetUVFilterType;
    property UVSingleIndex: Integer read GetUVSingleIndex write SetUVSingleIndex;
  end;

  TfcxCubeFieldFilters = class(TfcxList)
  private
    function GetItem(Index: Integer): TfcxCubeFieldFilter;
    function FieldIndex(AFieldName, AType: TfcxString): integer;
  protected
  public
    destructor Destroy; override;
    property Items[Index: Integer]: TfcxCubeFieldFilter read GetItem; default;
  end;

  TfcxEventFilter = class(TfcxFilter)
  private
    function TestRecByFilter(ARecIndex: Integer): Boolean; override;
  protected
  public
    procedure ResetFilter; override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;
    procedure LoadFromXML(AItem: TfcxXMLItem); override;
    constructor Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString); override;
  end;

  TfcxScriptFilter = class(TfcxFilter)
  private
    function TestRecByFilter(ARecIndex: Integer): Boolean; override;
  protected
  public
    constructor Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString); override;
    procedure ResetFilter; override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;
    procedure LoadFromXML(AItem: TfcxXMLItem); override;
  end;

  TfcxExpFilter = class(TfcxFilter)
  private
    function TestRecByFilter(ARecIndex: Integer): Boolean; override;
  protected
  public
    constructor Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString); override;
    procedure ResetFilter; override;
    procedure SaveToXML(AItem: TfcxXMLItem); override;
    procedure LoadFromXML(AItem: TfcxXMLItem); override;
  end;

implementation
//VCL uses section
{$IFNDEF FMX}
uses
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  fcxStringUtils, fcxUniqueArray, fcxUniqueValue;
//FMX uses
{$ELSE FMX}
uses
  System.Variants,
  FMX.fcxStringUtils, FMX.fcxUniqueArray, FMX.fcxUniqueValue;
{$ENDIF FMX}

const
  SplitPathCount: integer = 4;

{ TfcxFilterManager }

procedure TfcxFilterManager.BeginUpdate;
var
  i: integer;
begin
//  if mdsoAutoFilter in Options then Exit;
  if FCube <> nil then
  begin
    if FUpdating then
      RaisefcError(exfcTransactionAlreadyStarted, []);
    StartChange;
    FUpdating := True;
    for i := 0 to FFilters.Count - 1 do
      TfcxFilter(FFilters[i]).BeginUpdate;
  end;
end;

procedure TfcxFilterManager.CalcFilter;
// True if ARecIndex does not pass through some Filter !!!
  function TestRecByFilters(ARecIndex: Integer): Boolean;
  var i: integer;
  begin
    Result := False;
    for i := 0 to FFilters.Count - 1 do
      if TfcxFilter(FFilters[i]).Active then
      begin
        Result := Result or TfcxFilter(FFilters[i]).CalcFilterOnRec(ARecIndex);
        if Result then
          Break;
      end
  end;

  function TestRecByFiltersWithOnlyAvailable(ARecIndex: Integer): Boolean;
  var
    i: integer;
    AFilteredByOtherFilters: Boolean;
  begin
    if UVFilter.Active then
      Result := UVFilter.CalcFilterOnRecWithAvailable(ARecIndex)
    else
      Result := False;
    AFilteredByOtherFilters := False;
    for i := 1 to FFilters.Count - 1 do
      if TfcxFilter(FFilters[i]).Active then
      begin
        AFilteredByOtherFilters := AFilteredByOtherFilters or TfcxFilter(FFilters[i]).CalcFilterOnRec(ARecIndex);
        if AFilteredByOtherFilters then
          Break;
      end;
    SetAvailablesFlags(ARecIndex, AFilteredByOtherFilters);
    Result := Result or AFilteredByOtherFilters;
  end;
var
  i, j: integer;
  AUseFilter: boolean;
begin
  AUseFilter := False;
  for i := 0 to FFilters.Count - 1 do
    AUseFilter := TfcxFilter(FFilters[i]).StartCalcFilter or AUseFilter;
  if FFilterArray <> nil then
    FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1), 0);
  FFilterRecCount := 0;
  UVFilter.StartCalcOnlyAvailable(not AUseFilter);
  if AUseFilter then
  begin
    if FOnlyAvailableSupport then
      for i := 0 to FRecCount - 1 do
      begin
        if TestRecByFiltersWithOnlyAvailable(i) then
        begin
          if FFilterArray = nil then
          begin
            GetMem(FFilterArray, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1));
            FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1), 0);
          end;
          FFilterArray[i shr 3] := FFilterArray[i shr 3] or BitmapSet[i mod 8];
          if FGoodIndexes = nil then
            GetMem(FGoodIndexes, FRecCount * SizeOf(Integer));
          if FFilterRecCount = 0 then
            for j := 0 to i - 1 do
              FGoodIndexes[j] := j;
          FGoodIndexes[i] := -1;
          inc(FFilterRecCount);
        end
        else
        if FGoodIndexes <> nil then
          FGoodIndexes[i] := i - FFilterRecCount;
      end
    else
      for i := 0 to FRecCount - 1 do
      begin
        if TestRecByFilters(i) then
        begin
          if FFilterArray = nil then
          begin
            GetMem(FFilterArray, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1));
            FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1), 0);
          end;
          FFilterArray[i shr 3] := FFilterArray[i shr 3] or BitmapSet[i mod 8];
          if FGoodIndexes = nil then
            GetMem(FGoodIndexes, FRecCount * SizeOf(Integer));
          if FFilterRecCount = 0 then
            for j := 0 to i - 1 do
              FGoodIndexes[j] := j;
          FGoodIndexes[i] := -1;
          inc(FFilterRecCount);
        end
        else
        if FGoodIndexes <> nil then
          FGoodIndexes[i] := i - FFilterRecCount;
      end;
  end;
  for i := 0 to FFilters.Count - 1 do
    TfcxFilter(FFilters[i]).StopCalcFilter;
  FGoodRecCount := FRecCount - FFilterRecCount;
  if FFilterRecCount = 0 then
  begin
    FreeMem(FFilterArray);
    FFilterArray := nil;
    FreeMem(FGoodIndexes);
    FGoodIndexes := nil;
    FreeMem(FAbsIndexes);
    FAbsIndexes := nil;
  end
  else
  if FGoodRecCount = 0 then
  begin
    FreeMem(FAbsIndexes);
    FAbsIndexes := nil;
  end
  else
  begin
    ReallocMem(FAbsIndexes, FGoodRecCount * SizeOf(Integer));
    for i := 0 to FRecCount - 1 do
      if FGoodIndexes[i] > -1 then
        FAbsIndexes[FGoodIndexes[i]] := i;
  end;
end;

constructor TfcxFilterManager.Create(AOwner: TComponent);
begin
  inherited;
  FOnlyAvailableSupport := True;
  FOpened := False;
  FUpdating := False;
  FCube := nil;
//  FFields := nil;
  FFieldCount := 0;
  FFilterRecCount := 0;
  FFilterArray := nil;
  FChanges := [];
  FRecCount := 0;
  FGoodRecCount := 0;
  FGoodIndexes := nil;
  FAbsIndexes := nil;
  FFilters := TfcxFilters.Create(Self);
{ TODO -cНеобходимо : Надо использовать ресурсы.}
  FFilters.Add(TfcxUVFilter.Create(Self, '#UVFilter', 'Unique values filter'));
end;

function TfcxFilterManager.CreateEventFilter(AName,
  ACaption: TfcxString{; AEventProc: TfcxFilterEvent = nil}): integer;
begin
  if FilterByName[AName] = nil then
  begin
    StartChange;
    Result := FFilters.Add(TfcxEventFilter.Create(Self, AName, ACaption));
{
    if AEventProc = nil then
      Result.Event := FFilterEvent
    else
      Result.Event := AEventProc;
}
    if Result <> -1 then
      StopChange([chf_ActivedFilterChanged])
    else
      StopChange([]);
  end
  else
    Result := -1;
end;

function TfcxFilterManager.CreateExpressionFilter(AName, ACaption,
  AExpression: TfcxString): integer;
begin
  if FilterByName[AName] = nil then
  begin
    StartChange;
    Result := FFilters.Add(TfcxExpFilter.Create(Self, AName, ACaption));
{
    Result.Expresion := AExpression
}
    if Result <> -1 then
      StopChange([chf_ActivedFilterChanged])
    else
      StopChange([]);
  end
  else
    Result := -1;
end;

function TfcxFilterManager.CreateScriptFilter(AName, ACaption,
  AScript: TfcxString): integer;
begin
  if FilterByName[AName] = nil then
  begin
    StartChange;
    Result := FFilters.Add(TfcxScriptFilter.Create(Self, AName, ACaption));
{
    Result.Script := AScript
}
    if Result <> -1 then
      StopChange([chf_ActivedFilterChanged])
    else
      StopChange([]);
  end
  else
    Result := -1;
end;

procedure TfcxFilterManager.CubeChanged(Sender: TfcxCube; AChangeAlert: TfcxChangeAlert; Action: TBasicAction);
begin
  if AChangeAlert is TfcxCubeSplitsChangeAlert then
  begin
    if TfcxCubeChangeAlert(AChangeAlert).CubeChangeType = ccht_OneSplit then
    begin
      if chc_AddedOneSplit in TfcxCubeChangeAlert(AChangeAlert).ChangesInCube then
        FieldSplitAdded(TfcxCommonField(TfcxCubeSplitsChangeAlert(AChangeAlert).MasterField), TfcxCubeSplitsChangeAlert(AChangeAlert).SplitFieldIndex)
      else
      if chc_DeletedOneSplit in TfcxCubeChangeAlert(AChangeAlert).ChangesInCube then
        FieldSplitDeleted(TfcxCommonField(TfcxCubeSplitsChangeAlert(AChangeAlert).MasterField), TfcxCubeSplitsChangeAlert(AChangeAlert).SplitFieldIndex);
      ListnersManager.SendAction(TfcxAction(Action));
    end
  end
  else
  if AChangeAlert is TfcxCubeChangeAlert then
    if TfcxCubeChangeAlert(AChangeAlert).CubeChangeType = ccht_All then
    begin
      StartChange;
      if FCube = nil then
      begin
        Close;
      end
      else
      begin
        Close;
        Open;
      end;
      StopChange([chf_Changed]);
    end
    else
    if TfcxCubeChangeAlert(AChangeAlert).CubeChangeType = ccht_FieldList then
    begin
      StartChange;
      if chc_AddedFields in TfcxCubeChangeAlert(AChangeAlert).ChangesInCube then
        FieldsAdded
      else
      if chc_DeletedField in TfcxCubeChangeAlert(AChangeAlert).ChangesInCube then
        FieldDeleted(TfcxCubeChangeAlert(AChangeAlert).FieldIndex)
      else
      if chc_ChangedSplits in TfcxCubeChangeAlert(AChangeAlert).ChangesInCube then
        FieldSplitChanged(TfcxCubeChangeAlert(AChangeAlert).FieldIndex);
      StopChange([chf_Changed]);
    end
    else
    if TfcxCubeChangeAlert(AChangeAlert).CubeChangeType = ccht_ChangesInData then
    begin
      StartChange;
        Close;
        Open;
//      DataChanged;
      StopChange([chf_Changed]);
    end
    else
    begin
// send cube alerts to Listners
      ListnersManager.SendAction(TfcxAction(Action));
//      DoChange(AChangeAlert);
//      StartChange;
//      StopChange([chf_Captions]);
    end;
end;

procedure TfcxFilterManager.DeleteFilter(AName: TfcxString);
var
  AIndex: integer;
  AActived: boolean;
begin
  AIndex := FilterIndexByName[AName];
  if AIndex <> -1 then
  begin
    AActived := ActiveFilter[AIndex];
    StartChange;
    FFilters.Delete(AIndex);
    if AActived then
      StopChange([chf_ActivedFilterChanged])
    else
      StopChange([chf_NonActivedFilterChanged]);
  end
end;

destructor TfcxFilterManager.Destroy;
begin
  FreeAndNil(FFilters);
  FreeMem(FFilterArray);
  FFilterArray := nil;
  FRecCount := 0;
  FGoodRecCount := 0;
  FreeMem(FGoodIndexes);
  FGoodIndexes := nil;
  FreeMem(FAbsIndexes);
  FAbsIndexes := nil;
  FFilterRecCount := 0;
  FFieldCount := 0;
  Cube := nil;
  inherited;
end;

procedure TfcxFilterManager.EndUpdate;
var
  i: integer;
begin
//  if mdsoAutoFilter in Options then
//    Exit;
  if FCube <> nil then
  begin
    if not FUpdating then
      Exit;
    for i := 0 to FFilters.Count - 1 do
      if not TfcxFilter(FFilters[i]).EndUpdate then
      begin
        RollBack;
        Exit;
      end;
    FUpdating := False;
    StopChange([]);
  end
end;

function TfcxFilterManager.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if TfcxAction(Action).Owner is TfcxCube then
      CubeChanged(TfcxCube(Action.Owner), TfcxAction(Action).ChangeAlert, Action);
    Result := True;
  end else
    Result := False;
end;

procedure TfcxFilterManager.FieldDeleted(AFieldIndex: integer);
begin
  if not FOpened then
    Exit;
  FFieldCount := FFieldCount - 1;
  TfcxUVFilter(FFilters[0]).FieldDeleted(AFieldIndex);
end;

procedure TfcxFilterManager.FieldsAdded;
begin
  if not FOpened then
    Exit;
  FFieldCount := FCube.Fields.Count;
  TfcxUVFilter(FFilters[0]).FieldsAdded;
end;

procedure TfcxFilterManager.FieldSplitChanged(AFieldIndex: integer);
begin
  TfcxUVFilter(FFilters[0]).FieldSplitChanged(AFieldIndex);
end;

function TfcxFilterManager.GetCube: TfcxCube;
begin
  Result := FCube;
end;

function TfcxFilterManager.GetActiveFilter(AIndex: Integer): boolean;
begin
  Result := Filter[AIndex].Active;
end;

function TfcxFilterManager.GetFilter(AIndex: Integer): TfcxFilter;
begin
  Result := TfcxFilter(FFilters[AIndex]);
end;

function TfcxFilterManager.GetFilterByName(AName: TfcxString): TfcxFilter;
var
  i: integer;
begin
  Result := nil;
  i := FilterIndexByName[AName];
  if i <> -1 then
    Result := Filter[i];
end;

function TfcxFilterManager.GetFilterIndexByName(AName: TfcxString): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FFilters.Count - 1 do
  begin
    if fcStrCompare(Filter[i].Name, AName) = 0 then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function TfcxFilterManager.GetRecFilter(ARecIndex: integer): boolean;
begin
  if FFilterArray = nil then
    Result := False
  else
    Result := (FFilterArray[ARecIndex shr 3] and BitmapSet[ARecIndex mod 8]) <> 0;
end;

function TfcxFilterManager.GetUVFilter: TfcxUVFilter;
begin
  Result := TfcxUVFilter(FFilters[0]);
end;

procedure TfcxFilterManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if Assigned(FCube) and (AComponent = FCube) then
      Cube := nil;
end;

procedure TfcxFilterManager.ResetListner(AComponent: TComponent);
begin
  if AComponent is TfcxAbstractSlice then
    if not TfcxAbstractSlice(AComponent).CubeIsEqual(FCube) then
      TfcxAbstractSlice(AComponent).ClearFilterManager;
end;

procedure TfcxFilterManager.RollBack;
var
  i: integer;
begin
//  if mdsoAutoFilter in Options then
//    Exit;
  if FCube <> nil then
  begin
    if not FUpdating then
      Exit;
    for i := 0 to FFilters.Count - 1 do
      TfcxFilter(FFilters[i]).RollBack;
//    Exclude(FChanges, chf_Changed);
    FUpdating := False;
    FChanges := FChanges - [chf_Changed, chf_NonActivedFilterChanged, chf_ActivedFilterChanged];
    StopChange([]);
  end;
end;

procedure TfcxFilterManager.SetCube(const Value: TfcxCube);
begin
  if FCube <> Value then
  begin
    if (Owner is TfcxAbstractSlice) and not TfcxAbstractSlice(Owner).CubeIsEqual(Value) then
    begin
      if Assigned(FCube) and Assigned(FCube.ListnersManager) then
        FCube.ListnersManager.RemoveListner(Self);
      exit;
    end;
    if not (csDestroying in ComponentState) then
      StartChange;
    if Assigned(FCube) then
      FCube.ListnersManager.RemoveListner(Self);
    FCube := Value;
    ListnersManager.Listners_Proc(ResetListner);
    if Assigned(FCube) then
      FCube.ListnersManager.AddListner(Self);
    if not (csDestroying in ComponentState) then
      StopChange([chf_SetCube]);
  end;
end;

procedure TfcxFilterManager.SetActiveFilter(AIndex: Integer;
  const Value: boolean);
begin
  Filter[AIndex].Active := Value;
end;

procedure TfcxFilterManager.StopChange(AChanges: TfcxChangesInFilters = []; AIndex: integer = -1);
begin
  if FChangeSemaphore > 0 then
    Exit;
  FChanges := FChanges + AChanges;
  dec(FChangeCount);
  if FChangeCount = 0 then
  begin
{ TODO -cНеобходимо : ТУТ ОБРАБАТЫВАЕМ ИЗМЕНЕНИЯ.}
    if FChanges = [] then
      exit;
    if (chf_SetCube in FChanges) then
    begin
      if FCube = nil then
      begin
        Close;
      end
      else
      begin
        Close;
        Open;
      end;
      FChanges := FChanges + [chf_Changed];
    end;
    if (FChanges - [chf_ResetAllFilters]) = [] then
      AfterResetAllFilters
    else
    if (FChanges - [chf_NonActivedFilterChanged]) <> [] then
      CalcFilter;
// это строка оповещения слушателей об изменении в компоненте. надо решать где её использовать
    DoChange(TfcxFiltersChangeAlert.Create(FiltersChangeTypeOfChanges(FChanges), FChanges));

    FChanges:=[];
    if Assigned(OnStopChange) then
      OnStopChange(Self);
  end;
end;

function TfcxFilterManager.GetCommonScript: TStrings;
begin
  if FCube <> nil then
    Result := FCube.GetCommonScript
  else
    Result := nil;
end;

function TfcxFilterManager.GetCommonScriptChangedTick: Cardinal;
begin
  if FCube <> nil then
    Result := FCube.GetCommonScriptChangedTick
  else
    Result := 0;
end;

procedure TfcxFilterManager.StartChangeExt;
begin
  StartChange;
end;

procedure TfcxFilterManager.StopChangeExt(AChanges: TfcxChangesInFilters;
  AIndex: integer);
begin
  StopChange(AChanges, AIndex);
end;

procedure TfcxFilterManager.FieldSplitAdded(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
begin
  TfcxUVFilter(FFilters[0]).FieldSplitAdded(ACubeField, ASplitFieldIndex);
end;

procedure TfcxFilterManager.FieldSplitDeleted(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
begin
  TfcxUVFilter(FFilters[0]).FieldSplitDeleted(ACubeField, ASplitFieldIndex);
end;

procedure TfcxFilterManager.ResetAllFilters;
var
  i: integer;
begin
  StartChange;
  for i := 0 to FFilters.Count - 1 do
  begin
    TfcxFilter(FFilters[i]).ResetFilter;
  end;
  StopChange([chf_ResetAllFilters]);
end;

function TfcxFilterManager.LoadFromFile(AFileName: String): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TfcxFilterManager.LoadFromStream(AStream: TStream): Boolean;
const
  XMLSignature: array[0..1] of AnsiChar = '<?';
var
  Version: Word;
  XMLDoc: TfcxXMLDocument;
begin
  Result := False;
  if AStream.Read(Version, SizeOf(Word)) < SizeOf(Word) then
    Exit;
  if Version = word(XMLSignature) then
  begin
    XMLDoc := TfcxXMLDocument.Create;
    try
      AStream.Position := AStream.Position - 2;
      XMLDoc.LoadFromStream(AStream);
      Result := LoadFromXML(XMLDoc);
    finally
      XMLDoc.Free;
    end;
  end
end;

function TfcxFilterManager.LoadFromXML(AXMLDoc: TfcxXMLDocument): Boolean;
begin
  Result := LoadFromXMLItem(AXMLDoc.Root);
end;

function TfcxFilterManager.LoadFromXMLItem(AItem: TfcxXMLItem): Boolean;
var
  i: integer;
begin
  Result := False;
  if AItem.Name = 'filters' then
    if AItem.IntProp['version'] = 2 then
    begin
      StartChange;
      ResetAllFilters;
      for i := 0 to AItem.Count - 1 do
      begin
        if FilterByName[AItem[i].Prop['name']] <> nil then
          FilterByName[AItem[i].Prop['name']].LoadFromXML(AItem[i])
      end;
      StopChange([]);
      Result := True;
    end
end;

procedure TfcxFilterManager.SaveToFile(AFileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TfcxFilterManager.SaveToStream(AStream: TStream);
var
  Doc: TfcxXMLDocument;
begin
  Doc := TfcxXMLDocument.Create;
  Doc.AutoIndent := True;
  try
    SaveToXML(Doc);
    Doc.SaveToStream(AStream);
  finally
    Doc.Free;
  end;
end;

procedure TfcxFilterManager.SaveToXML(AXMLDoc: TfcxXMLDocument);
begin
  SaveToXMLItem(AXMLDoc.Root);
end;

procedure TfcxFilterManager.SaveToXMLItem(AItem: TfcxXMLItem);
var
  i: integer;
begin
  AItem.Name := 'filters';
  AItem.IntProp['version'] := 2;
  AItem.Prop['timestamp'] := DateTimeToStr(Now);
  for i := 0 to FFilters.Count - 1 do
    TfcxFilter(FFilters[i]).SaveToXML(AItem)
end;

procedure TfcxFilterManager.SetStoreEnabledValues(const Value: boolean);
begin
  FStoreEnabledValues := Value;
end;

procedure TfcxFilterManager.Close;
var
  i: integer;
begin
  if not FOpened then
    Exit;
  for i := 0 to FFilters.Count - 1 do
    TfcxFilter(FFilters[i]).Close;
  FreeMem(FFilterArray);
  FFilterArray := nil;
  FRecCount := 0;
  FFieldCount := 0;
  FGoodRecCount := 0;
  FreeMem(FGoodIndexes);
  FGoodIndexes := nil;
  FreeMem(FAbsIndexes);
  FAbsIndexes := nil;
  FOpened := False;
end;

procedure TfcxFilterManager.Open;
var
  i: integer;
begin
  if FOpened then
    Exit;
  if FCube = nil then
    Exit;
  if not FCube.Active then
    Exit;
  FreeMem(FFilterArray);
  FFilterArray := nil;
  FRecCount := FCube.SourceHolder.RecordsCount;
  FFieldCount := FCube.Fields.Count;
  FreeMem(FGoodIndexes);
  FGoodIndexes := nil;
  FreeMem(FAbsIndexes);
  FAbsIndexes := nil;
(*
  if FRecCount > 0 then
  begin
    GetMem(FFilterArray, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1));
    FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FRecCount - 1) shr 3 + 1), 0);
  end;
*)
  FGoodRecCount := FRecCount;
  for i := 0 to FFilters.Count - 1 do
    TfcxFilter(FFilters[i]).Open;
  FOpened := True;
end;

procedure TfcxFilterManager.AfterResetAllFilters;
begin
  FFilterRecCount := 0;
  FGoodRecCount := FRecCount;
  FreeMem(FGoodIndexes);
  FGoodIndexes := nil;
  FreeMem(FAbsIndexes);
  FAbsIndexes := nil;
end;

function TfcxFilterManager.GetGoodIndex(ARecIndex: integer): integer;
begin
  if (FGoodIndexes = nil) then
    Result := ARecIndex
  else
  if (ARecIndex >= FRecCount) or (ARecIndex < 0) then
    Result := -1
  else
    Result := FGoodIndexes[ARecIndex];
end;

function TfcxFilterManager.GetAbsIndex(AGoodIndex: integer): integer;
begin
  if (FAbsIndexes = nil) then
    if FGoodRecCount = 0 then
      Result := -1
    else
      Result := AGoodIndex
  else
  if (AGoodIndex >= FGoodRecCount) or (AGoodIndex < 0) then
    Result := -1
  else
    Result := FAbsIndexes[AGoodIndex];
end;

procedure TfcxFilterManager.SetOnlyAvailableSupport(const Value: boolean);
begin
  if FOnlyAvailableSupport <> Value then
  begin
    FOnlyAvailableSupport := Value;
    UVFilter.SetOnlyAvailableSupport(Value);
    if FOnlyAvailableSupport then
      CalcFilter;
// todo Реакция на изменение. Создание/удаление структур и пересчёт фильтра.
  end
end;

procedure TfcxFilterManager.SetAvailablesFlags(ARecIndex: integer;
  AFilteredByOtherFilters: Boolean);
begin
  UVFilter.SetAvailablesFlags(ARecIndex, AFilteredByOtherFilters);
end;

procedure TfcxFilterManager.DataChanged;
begin
//  TfcxUVFilter(FFilters[0]).DataChanged;
end;

{ TfcxFilters }

constructor TfcxFilters.Create(AFilterManager: TfcxFilterManager);
begin
  inherited Create;
  FFilterManager := AFilterManager;
  AutoFree := True;
end;

destructor TfcxFilters.Destroy;
begin
  Clear;
  FFilterManager := nil;
  inherited;
end;

{ TfcxFilter }

procedure TfcxFilter.BeginUpdate;
begin
//
end;

function TfcxFilter.CalcFilterOnRec(ARecIndex: integer): boolean;
begin
  Result := not TestRecByFilter(ARecIndex);
  if Result then
  begin
(*
    if FFilterArray = nil then
    begin
      FSaveRecCount := FFilterManager.FRecCount;
      GetMem(FFilterArray, SizeOf(_fcxByteArray) * ((FSaveRecCount - 1) shr 3 + 1));
      FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FSaveRecCount - 1) shr 3 + 1), 0);
    end;
    FFilterArray[ARecIndex shr 3] :=  FFilterArray[ARecIndex shr 3] or BitmapSet[ARecIndex mod 8];
*)
    inc(FFilterRecCount);
  end;
end;

procedure TfcxFilter.Close;
begin
(*
  FreeMem(FFilterArray);
  FFilterArray := nil;
  FSaveRecCount := 0;
*)
end;

constructor TfcxFilter.Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString);
begin
  FFilterManager := AFilterManager;
(*
  FFilterArray := nil;
  FSaveRecCount := 0;
*)
  FFilterRecCount := 0;
  FName := AName;
  FCaption := ACaption;
  FActive := True;
end;

destructor TfcxFilter.Destroy;
begin
(*
  FreeMem(FFilterArray);
  FSaveRecCount := 0;
  FFilterArray := nil;
*)
  FFilterRecCount := 0;
  inherited;
end;

function TfcxFilter.EndUpdate: boolean;
begin
  Result := True;
end;

function TfcxFilter.GetDescription: TfcxString;
begin
  Result := '';
end;

function TfcxFilter.GetHasFilteredValues: boolean;
begin
  Result := True;
end;

procedure TfcxFilter.Open;
begin
(*
  if FFilterManager.FRecCount > 0 then
  begin
    ReallocMem(FFilterArray, SizeOf(_fcxByteArray) * ((FFilterManager.FRecCount - 1) shr 3 + 1));
    FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FFilterManager.FRecCount - 1) shr 3 + 1), 0);
  end;
*)
end;

procedure TfcxFilter.RollBack;
begin
//
end;

procedure TfcxFilter.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FFilterManager.StartChange;
    FActive := Value;
    FFilterManager.StopChange([chf_Changed]);
  end;
end;

procedure TfcxFilter.SetName(const Value: TfcxString);
begin
  if FName <> Value then
    if FFilterManager.FilterByName[Value] = nil then
      FName := Value;
end;

function TfcxFilter.StartCalcFilter: boolean;
begin
(*
  if FFilterArray <> nil then
    if FSaveRecCount = FFilterManager.FRecCount then
      FillChar(FFilterArray^, SizeOf(_fcxByteArray) * ((FFilterManager.FRecCount - 1) shr 3 + 1), 0)
    else
    begin
      FreeMem(FFilterArray);
      FFilterArray := nil;
      FSaveRecCount := 0;
    end;
*)
  FFilterRecCount := 0;
  Result := Active and GetHasFilteredValues;
end;

procedure TfcxFilter.StopCalcFilter;
begin
(*
  if FFilterRecCount = 0 then
  begin
    FreeMem(FFilterArray);
    FFilterArray := nil;
  end;
*)
end;

{ TfcxUVFilter }

procedure TfcxUVFilter.BeginUpdate;
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].BeginUpdate
end;

procedure TfcxUVFilter.Clear;
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].Clear
end;

constructor TfcxUVFilter.Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString);
begin
  inherited;
  FCountFilteredFields := 0;
  FFilterType := ft_ByUniqueValue;
  FCubeFieldFilters := TfcxCubeFieldFilters.Create;
end;

destructor TfcxUVFilter.Destroy;
begin
  Clear;
  FreeAndNil(FCubeFieldFilters);
  inherited;
end;

function TfcxUVFilter.EndUpdate: boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    Result := Result and FCubeFieldFilters[i].EndUpdate;
end;

procedure TfcxUVFilter.FieldDeleted(AFieldIndex: integer);
begin
  CountFilteredFields := CountFilteredFields - FCubeFieldFilters[AFieldIndex].CountFilteredFields;
  FCubeFieldFilters[AFieldIndex].Free;
  FCubeFieldFilters.Delete(AFieldIndex);
end;

procedure TfcxUVFilter.FieldsAdded;
var
  i: integer;
begin
  for i := FCubeFieldFilters.Count to FFilterManager.FFieldCount - 1 do
    FCubeFieldFilters.Add(TfcxCubeFieldFilter.Create(Self, nil, FFilterManager.FCube.Fields.Items[i]));
end;

procedure TfcxUVFilter.FieldSplitChanged(AFieldIndex: integer);
begin
  FCubeFieldFilters[AFieldIndex].FieldSplitChanged;
end;

procedure TfcxUVFilter.FieldSplitAdded(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    if FCubeFieldFilters[i].FieldSplitAdded(ACubeField, ASplitFieldIndex) then
      Exit;
end;

procedure TfcxUVFilter.FieldSplitDeleted(ACubeField: TfcxCommonField; ASplitFieldIndex: integer);
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    if FCubeFieldFilters[i].FieldSplitDeleted(ACubeField, ASplitFieldIndex) then
      Exit;
end;

function TfcxUVFilter.GetHasFilteredValues: boolean;
begin
  Result := FCountFilteredFields > 0;
end;

function TfcxUVFilter.GetDescription: TfcxString;
{
var
  i, j: integer;
}
begin
{
  if FFilterAllUVCount <> 0 then
  begin
    Result := '';
    for i := 0 to FFieldCount - 1 do
      if FFilterUVCounts[i] > 0 then
      begin
        if Result <> '' then
          Result := Result + #13#10 + #13#10;
        Result := Result + FFilterManager.FCube.Fields[i].CubeFieldDisplayLabel + ' not in (';
        for j := 0 to FFilterManager.FCube.Fields[i].UniqueValues.Count - 1 do
          if not Filter[i, j] then
          begin
            if RightStr(Result, 1) <> '(' then
              Result := Result + ', ';
            Result := Result + FFilterManager.FCube.Fields[i].UniqueValues.GetCaption(j);
          end;
        Result := Result + ')';
      end;
  end;
}
  if Result = '' then
    Result := 'No filter';
end;

procedure TfcxUVFilter.RollBack;
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].RollBack;
end;

function TfcxUVFilter.TestRecByFilter(ARecIndex: Integer): Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    if FCubeFieldFilters[i].FCountFilteredFields > 0 then
    begin
      Result := FCubeFieldFilters[i].TestRecByFilter(ARecIndex);
      if not Result then
        Exit;
    end;
end;

procedure TfcxUVFilter.ResetFilter;
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].ResetFilter;
end;

procedure TfcxUVFilter.SaveToXML(AItem: TfcxXMLItem);
var
  AItem1: TfcxXMLItem;
  i: integer;
begin
  if HasFilteredValues then
  begin
    AItem1 := AItem.Add;
    AItem1.Name := 'filter';
    AItem1.Prop['name'] := '#UVFilter';
    for i := 0 to FCubeFieldFilters.Count - 1 do
      FCubeFieldFilters[i].SaveToXML(AItem1);
  end
end;

procedure TfcxUVFilter.LoadFromXML(AItem: TfcxXMLItem);
var
  i, AFieldIndex: integer;
begin
  for i := 0 to AItem.Count - 1 do
  begin
    AFieldIndex := FCubeFieldFilters.FieldIndex(AItem[i].Prop['name'], AItem[i].Prop['type']);
    if AFieldIndex >= 0 then
      FCubeFieldFilters[AFieldIndex].LoadFromXML(AItem[i]);
  end;
end;

function TfcxUVFilter.GetCubeFieldFilter(
  AField: TfcxCommonField): TfcxCubeFieldFilter;
  function FindIn(ACubeFieldFilters: TfcxCubeFieldFilters; var ACubeFieldFilter: TfcxCubeFieldFilter): Boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to ACubeFieldFilters.Count - 1 do
    begin
      Result := (ACubeFieldFilters[i].FField = AField);
      if Result then
      begin
        ACubeFieldFilter := ACubeFieldFilters[i];
        Exit;
      end;
      Result := FindIn(ACubeFieldFilters[i].FCubeFieldFilters, ACubeFieldFilter);
      if Result then
        Exit;
    end;
  end;
begin
  Result := Nil;
  FindIn(FCubeFieldFilters, Result);
end;

procedure TfcxUVFilter.Close;
var
  i: integer;
begin
  inherited;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].Free;
  FCubeFieldFilters.Clear;
end;

procedure TfcxUVFilter.Open;
var
  i: integer;
begin
  inherited;
  for i := 0 to FFilterManager.FFieldCount - 1 do
    FCubeFieldFilters.Add(TfcxCubeFieldFilter.Create(Self, nil, FFilterManager.FCube.Fields.Items[i]));
end;

procedure TfcxUVFilter.SetOnlyAvailableSupport(const Value: boolean);
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].SetOnlyAvailableSupport(Value);
end;

procedure TfcxUVFilter.StartCalcOnlyAvailable(ANoFilters: Boolean = False);
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].StartCalcOnlyAvailable(ANoFilters);
  FInternalResultForRec := True;
  FInternalCountFilteredFieldsForRec := 0;
end;

function TfcxUVFilter.TestRecByFilterWithAvailable(
  ARecIndex: Integer): Boolean;
var
  i: integer;
begin
  Result := True;
  FInternalResultForRec := True;
  FInternalCountFilteredFieldsForRec := 0;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    if FCubeFieldFilters[i].FCountFilteredFields > 0 then
    begin
      FCubeFieldFilters[i].TestRecByFilterWithAvailable(ARecIndex);
      if FInternalCountFilteredFieldsForRec > 1 then
      begin
        Result := False;
        FInternalResultForRec := Result;
        Exit;
      end;
    end;
  if FInternalCountFilteredFieldsForRec > 0 then
  begin
    Result := False;
    FInternalResultForRec := Result;
  end;
end;

function TfcxUVFilter.CalcFilterOnRecWithAvailable(
  ARecIndex: integer): boolean;
begin
  Result := not TestRecByFilterWithAvailable(ARecIndex);
  if Result then
  begin
    inc(FFilterRecCount);
  end;
end;

procedure TfcxUVFilter.SetAvailablesFlags(ARecIndex: integer;
  AFilteredByOtherFilters: Boolean);
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].SetAvailablesFlags(ARecIndex, AFilteredByOtherFilters);
end;

{ TfcxEventFilter }

constructor TfcxEventFilter.Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString);
begin
  inherited;
  FFilterType := ft_ByEvent;
end;

procedure TfcxEventFilter.LoadFromXML(AItem: TfcxXMLItem);
begin
//
end;

procedure TfcxEventFilter.ResetFilter;
begin
//
end;

procedure TfcxEventFilter.SaveToXML(AItem: TfcxXMLItem);
begin
//
end;

function TfcxEventFilter.TestRecByFilter(ARecIndex: Integer): Boolean;
begin
  Result := True;
end;

{ TfcxScriptFilter }

constructor TfcxScriptFilter.Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString);
begin
  inherited;
  FFilterType := ft_ByScript;
end;

procedure TfcxScriptFilter.LoadFromXML(AItem: TfcxXMLItem);
begin
//
end;

procedure TfcxScriptFilter.ResetFilter;
begin
//
end;

procedure TfcxScriptFilter.SaveToXML(AItem: TfcxXMLItem);
begin
//
end;

function TfcxScriptFilter.TestRecByFilter(ARecIndex: Integer): Boolean;
begin
  Result := True;
end;

{ TfcxExpFilter }

constructor TfcxExpFilter.Create(AFilterManager: TfcxFilterManager; AName: TfcxString; ACaption: TfcxString);
begin
  inherited;
  FFilterType := ft_BySimpleExpression;
end;

procedure TfcxExpFilter.LoadFromXML(AItem: TfcxXMLItem);
begin
//
end;

procedure TfcxExpFilter.ResetFilter;
begin
//
end;

procedure TfcxExpFilter.SaveToXML(AItem: TfcxXMLItem);
begin
//
end;

function TfcxExpFilter.TestRecByFilter(ARecIndex: Integer): Boolean;
begin
  Result := True;
end;

{ TfcxCubeFieldFilter }

procedure TfcxCubeFieldFilter.BeginUpdate;
var
  i: integer;
begin
  FNewFilterArray.BeginUpdate;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].BeginUpdate;
end;

procedure TfcxCubeFieldFilter.BeginUpdateField;
begin
  FNewFilterArray.BeginUpdateField
end;

procedure TfcxCubeFieldFilter.Clear;
var
  i: integer;
begin
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].Clear;
  FNewFilterArray.Clear;
end;

constructor TfcxCubeFieldFilter.Create(AUVFilter: TfcxUVFilter;
  AMasterCubeFieldFilter: TfcxCubeFieldFilter; AField: TfcxCommonField);
var
  i: integer;
begin
  FField := AField;
  FMasterCubeFieldFilter := AMasterCubeFieldFilter;
  FUVFilter := AUVFilter;
  FCubeFieldFilters := TfcxCubeFieldFilters.Create;
  FUVAvailableCount := 0;
  if FField is TfcxCommonUVField then
    FNewFilterArray := TfcxNewUVFilterArray.Create(Self)
  else
    FNewFilterArray := TfcxNewStdSplitFilterArray.Create(Self);
  if FField.Fields <> nil then
    for i := 0 to FField.Fields.Count - 1 do
      FCubeFieldFilters.Add(TfcxCubeFieldFilter.Create(FUVFilter, Self, FField.Fields.Items[i]));
end;

destructor TfcxCubeFieldFilter.Destroy;
begin
  FreeAndNil(FCubeFieldFilters);
  FreeAndNil(FNewFilterArray);
  inherited;
end;

function TfcxCubeFieldFilter.EndUpdate: boolean;
var
  i: integer;
begin
  Result := FNewFilterArray.EndUpdate;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    Result := Result and FCubeFieldFilters[i].EndUpdate;
end;

function TfcxCubeFieldFilter.EndUpdateField: boolean;
begin
  Result := FNewFilterArray.EndUpdateField
end;

function TfcxCubeFieldFilter.GetFilter(AUVIndex: Integer): Boolean;
begin
  Result := FNewFilterArray.Filter[AUVIndex]
end;

function TfcxCubeFieldFilter.GetFilterByValue(
  AUVVarValue: Variant): Boolean;
begin
  Result := FNewFilterArray.FilterByValue[AUVVarValue]
end;

function TfcxCubeFieldFilter.GetFilteredValuesCount: integer;
begin
  Result := FNewFilterArray.FilterCount;
end;

function TfcxCubeFieldFilter.GetHasFilteredValues: boolean;
begin
  Result := FCountFilteredFields > 0;
end;

function TfcxCubeFieldFilter.GetUVCount: integer;
begin
  Result := FNewFilterArray.UVCount;
end;

function TfcxCubeFieldFilter.GetUVFilterType: TfcxUVFilterType;
begin
  Result := FNewFilterArray.UVFilterType
end;

function TfcxCubeFieldFilter.GetUVSingleIndex: Integer;
begin
  Result := FNewFilterArray.UVSingleIndex;
end;

procedure TfcxCubeFieldFilter.InverseFilter;
begin
  FNewFilterArray.InverseFilter;
end;

procedure TfcxCubeFieldFilter.LoadFromXML(AItem: TfcxXMLItem);
var
  i, j, AFieldIndex: integer;
begin
  for i := 0 to AItem.Count - 1 do
    if AItem[i].Name = 'values' then
      FNewFilterArray.LoadFromXML(AItem[i])
    else
    if AItem[i].Name = 'attributes' then
      for j := 0 to AItem[i].Count - 1 do
      begin
        AFieldIndex := FCubeFieldFilters.FieldIndex(AItem[i][j].Prop['name'], AItem[i][j].Prop['type']);
        if AFieldIndex >= 0 then
          FCubeFieldFilters[AFieldIndex].LoadFromXML(AItem[i][j]);
      end;
end;

procedure TfcxCubeFieldFilter.ResetFilter;
var
  i: integer;
begin
  FNewFilterArray.ResetFilter;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].ResetFilter;
end;

procedure TfcxCubeFieldFilter.RollBack;
var
  i: integer;
begin
  FNewFilterArray.RollBack;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].RollBack;
end;

procedure TfcxCubeFieldFilter.RollBackField;
begin
  FNewFilterArray.RollBackField
end;

procedure TfcxCubeFieldFilter.SaveToXML(AItem: TfcxXMLItem);
var
  i: integer;
  AItem1, AItem2: TfcxXMLItem;
begin
  AItem1 := AItem.Add;
  AItem1.Name := 'field';
  AItem1.Prop['name'] := FField.CubeFieldName;
  if FField is TfcxCommonDatePathField then
    AItem1.Prop['type'] := 'datepathfield'
  else
  if FField is TfcxCommonTimePathField then
    AItem1.Prop['type'] := 'timepathfield'
  else
    AItem1.Prop['type'] := 'uvfield';
  FNewFilterArray.SaveToXML(AItem1);
  if FCubeFieldFilters.Count > 0 then
  begin
    AItem2 := AItem1.Add;
    AItem2.Name := 'attributes';
    for i := 0 to FCubeFieldFilters.Count - 1 do
      FCubeFieldFilters[i].SaveToXML(AItem2);
    if AItem2.Count = 0 then
      AItem2.Free;
  end;
  if AItem1.Count = 0 then
    AItem1.Free;
end;

procedure TfcxCubeFieldFilter.SetAllFilter;
begin
  FNewFilterArray.SetAllFilter;
end;

procedure TfcxCubeFieldFilter.SetFilter(AUVIndex: Integer;
  const Value: Boolean);
begin
  FNewFilterArray.Filter[AUVIndex] := Value;
end;

procedure TfcxCubeFieldFilter.SetCountFilteredFields(const Value: Integer);
begin
  if FCountFilteredFields <> Value then
  begin
    if FMasterCubeFieldFilter <> nil then
      FMasterCubeFieldFilter.CountFilteredFields := FMasterCubeFieldFilter.CountFilteredFields - FCountFilteredFields + Value
    else
      FUVFilter.CountFilteredFields := FUVFilter.CountFilteredFields - FCountFilteredFields + Value;
    FCountFilteredFields := Value;
  end
end;

procedure TfcxCubeFieldFilter.SetFilterByValue(AUVVarValue: Variant;
  const Value: Boolean);
begin
  FNewFilterArray.FilterByValue[AUVVarValue] := Value;
end;

procedure TfcxCubeFieldFilter.SetNoneFilter;
begin
  FNewFilterArray.SetNoneFilter;
end;

procedure TfcxCubeFieldFilter.SetUVFilterType(
  const Value: TfcxUVFilterType);
begin
  FNewFilterArray.UVFilterType := Value
end;

procedure TfcxCubeFieldFilter.SetUVSingleIndex(const Value: Integer);
begin
  FNewFilterArray.UVSingleIndex := Value;
end;

function TfcxCubeFieldFilter.TestRecByFilter(ARecIndex: Integer): Boolean;
var
  i: integer;
begin
  if FNewFilterArray.FFilterCount > 0 then
  begin
    Result := FNewFilterArray.TestRecByFilter(ARecIndex);
    if not Result then
      Exit;
  end;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    if FCubeFieldFilters[i].FCountFilteredFields > 0 then
    begin
      Result := FCubeFieldFilters[i].TestRecByFilter(ARecIndex);
      if not Result then
        Exit;
    end;
  Result := True;
end;

procedure TfcxCubeFieldFilter.FieldSplitChanged;
var
  i: integer;
begin
  if FNewFilterArray.FFilterCount > 0 then
    CountFilteredFields := 1
  else
    CountFilteredFields := 0;
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].Free;
  FCubeFieldFilters.Clear;
  if FField.Fields <> nil then
    for i := 0 to FField.Fields.Count - 1 do
      FCubeFieldFilters.Add(TfcxCubeFieldFilter.Create(FUVFilter, Self, FField.Fields.Items[i]));
end;

function TfcxCubeFieldFilter.FieldSplitAdded(ACubeField: TfcxCommonField;
  ASplitFieldIndex: integer): Boolean;
var
  i: integer;
begin
  Result := False;
  if Self.FField = ACubeField then
  begin
    FCubeFieldFilters.Add(TfcxCubeFieldFilter.Create(FUVFilter, Self, FField.Fields.Items[ASplitFieldIndex]));
    Result := True;
  end
  else
    for i := 0 to FCubeFieldFilters.Count - 1 do
      if FCubeFieldFilters[i].FieldSplitAdded(ACubeField, ASplitFieldIndex) then
      begin
        Result := True;
        Exit;
      end;
end;

function TfcxCubeFieldFilter.FieldSplitDeleted(ACubeField: TfcxCommonField;
  ASplitFieldIndex: integer): Boolean;
var
  i: integer;
begin
  Result := False;
  if Self.FField = ACubeField then
  begin
    FCubeFieldFilters[ASplitFieldIndex].Free;
    FCubeFieldFilters.Delete(ASplitFieldIndex);
    Result := True;
  end
  else
    for i := 0 to FCubeFieldFilters.Count - 1 do
      if FCubeFieldFilters[i].FieldSplitDeleted(ACubeField, ASplitFieldIndex) then
      begin
        Result := True;
        Exit;
      end;
end;

procedure TfcxCubeFieldFilter.SetRangeFilter(ARange: TfcxRanges);
begin
  FNewFilterArray.SetRangeFilter(ARange);
end;

procedure TfcxCubeFieldFilter.SetOnlyAvailableSupport(
  const Value: boolean);
var
  i: integer;
begin
  FNewFilterArray.SetOnlyAvailableSupport(Value);
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].SetOnlyAvailableSupport(Value);
end;

procedure TfcxCubeFieldFilter.StartCalcOnlyAvailable(ANoFilters: Boolean = False);
var
  i: integer;
begin
  if ANoFilters then
    FUVAvailableCount := UVCount
  else
    FUVAvailableCount := 0;
  FNewFilterArray.StartCalcOnlyAvailable(ANoFilters);
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].StartCalcOnlyAvailable(ANoFilters);
end;

function TfcxCubeFieldFilter.TestRecByFilterWithAvailable(
  ARecIndex: Integer): Boolean;
var
  i: integer;
  ATempResult: Boolean;
begin
  Result := FNewFilterArray.TestRecByFilter(ARecIndex);
  if not Result then
  begin
    inc(FUVFilter.FInternalCountFilteredFieldsForRec);
    if FUVFilter.FInternalCountFilteredFieldsForRec > 1 then
      Exit;
  end;
  ATempResult := True;
  for i := 0 to FCubeFieldFilters.Count - 1 do
  begin
    ATempResult := FCubeFieldFilters[i].TestRecByFilter(ARecIndex);
    if not ATempResult then
    begin
      inc(FUVFilter.FInternalCountFilteredFieldsForRec);
      if FUVFilter.FInternalCountFilteredFieldsForRec > 1 then
      begin
        Result := False;
        Exit;
      end
    end;
  end;
  Result := Result and ATempResult;
end;

procedure TfcxCubeFieldFilter.SetAvailablesFlags(ARecIndex: integer;
  AFilteredByOtherFilters: Boolean);
var
  i: integer;
begin
  FNewFilterArray.SetAvailablesFlags(ARecIndex, AFilteredByOtherFilters);
  for i := 0 to FCubeFieldFilters.Count - 1 do
    FCubeFieldFilters[i].SetAvailablesFlags(ARecIndex, AFilteredByOtherFilters);
end;

function TfcxCubeFieldFilter.GetAvailable(AUVIndex: Integer): Boolean;
begin
  Result := FNewFilterArray.Available[AUVIndex]
end;

function TfcxCubeFieldFilter.GetUVAvailableCount: integer;
begin
  if FUVFilter.FFilterManager.FOnlyAvailableSupport then
    Result := FUVAvailableCount
  else
    Result := UVCount;
end;

{ TfcxCubeFieldFilters }

destructor TfcxCubeFieldFilters.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;
end;

function TfcxCubeFieldFilters.FieldIndex(AFieldName,
  AType: TfcxString): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].FField.CubeFieldName = AFieldName then
      if ((Items[i].FField is TfcxCommonDatePathField) and (AType = 'datepathfield')) or
         ((Items[i].FField is TfcxCommonTimePathField) and (AType = 'timepathfield')) or
         ((Items[i].FField is TfcxCommonUVField) and (AType = 'uvfield')) then
      begin
        Result := i;
        Exit;
      end;
end;

function TfcxCubeFieldFilters.GetItem(Index: Integer): TfcxCubeFieldFilter;
begin
  Result := List[Index];
end;

{ TfcxNewFilterArray }

procedure TfcxNewFilterArray.BeginUpdate;
var
  ACount: integer;
begin
  if (FUVFilterType = uvft_Single) or (FFilterCount = 0) then
  begin
    FSaveFilters := nil;
  end
  else
  begin
    ACount := (UniqueValuesCount shr 3 + 1);
    GetMem(FSaveFilters, SizeOf(_fcxByteArray) * ACount);
    Move(FFilters[0], FSaveFilters[0], SizeOf(_fcxByteArray) * ACount);
  end;
  FSaveFilterCount := FFilterCount;
  FSaveSingleIndex := FUVSingleIndex;
end;

procedure TfcxNewFilterArray.BeginUpdateField;
var
  ACount: integer;
begin
  if FCubeFieldFilter.UVFilter.FFilterManager.FUpdating then
    RaisefcError(exfcTransactionAlreadyStarted, []);
  FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
  FCubeFieldFilter.UVFilter.FFilterManager.FUpdating := True;
  if (FUVFilterType = uvft_Single) or (FFilterCount = 0) then
  begin
    FSaveFilters := nil;
  end
  else
  begin
    ACount := (UniqueValuesCount shr 3 + 1);
    GetMem(FSaveFilters, SizeOf(_fcxByteArray) * ACount);
    Move(FFilters[0], FSaveFilters[0], SizeOf(_fcxByteArray) * ACount);
  end;
  FSaveFilterCount := FFilterCount;
  FSaveSingleIndex := FUVSingleIndex;
end;

procedure TfcxNewFilterArray.Clear;
begin
  ClearSave;
  FreeMem(FFilters);
  FFilters := nil;
  FreeMem(FAvailableFlags);
  FAvailableFlags := nil;
end;

procedure TfcxNewFilterArray.ClearSave;
begin
  if FLockClearSave then
    Exit;
  FreeMem(FSaveFilters);
  FSaveFilters := nil;
  FSaveFilterCount := 0;
  FSaveSingleIndex := 0;
end;

constructor TfcxNewFilterArray.Create(
  ACubeFieldFilter: TfcxCubeFieldFilter);
var
  ACount: integer;
begin
  FCubeFieldFilter := ACubeFieldFilter;
  FLockClearSave := False;
  FFilterCount := 0;
  FSaveFilters := nil;
  FSaveFilterCount := 0;
  FSaveSingleIndex := 0;
// GetMem only when need!!
  FFilters := nil;
  FOnlyAvailableSupport := FCubeFieldFilter.FUVFilter.FFilterManager.FOnlyAvailableSupport;
  if FOnlyAvailableSupport then
  begin
    ACount := UniqueValuesCount shr 3 + 1;
    GetMem(FAvailableFlags, SizeOf(_fcxByteArray) * ACount);
    FillChar(FAvailableFlags^, SizeOf(_fcxByteArray) * ACount, 255);
  end
  else
    FAvailableFlags := nil;
{ TODO -cНеобходимо : Брать начальное значение UVFilterTypes из поля куба.}
  FUVFilterType := uvft_Set;
  FUVSingleIndex := 0;
end;

destructor TfcxNewFilterArray.Destroy;
begin
  Clear;
  inherited;
end;

function TfcxNewFilterArray.EndUpdate: boolean;
begin
  ClearSave;
  Result := True;
end;

function TfcxNewFilterArray.EndUpdateField: boolean;
begin
  Result := False;
  if not FCubeFieldFilter.UVFilter.FFilterManager.FUpdating then
    Exit;
  ClearSave;
  FCubeFieldFilter.UVFilter.FFilterManager.FUpdating := False;
  FCubeFieldFilter.UVFilter.FFilterManager.StopChange([]);
  Result := True;
end;

function TfcxNewFilterArray.GetFilter(AUVIndex: Integer): Boolean;
begin
  if FUVFilterType = uvft_Single then
    Result := (AUVIndex = FUVSingleIndex)
  else
  if FFilters = nil then
    Result := True
  else
    Result := (FFilters[AUVIndex shr 3] and BitmapSet[AUVIndex mod 8]) = 0;
end;

function TfcxNewFilterArray.GetFilterByValue(
  AUVVarValue: Variant): Boolean;
var
  AUVIndex: integer;
begin
  AUVIndex := GetUVIndex(AUVVarValue);
  if (AUVIndex > -1) and (AUVIndex < UVCount) then
    Result := Filter[AUVIndex]
  else
    Result := False;
end;

function TfcxNewFilterArray.GetAvailable(AUVIndex: Integer): Boolean;
begin
  if Assigned(FAvailableFlags) then
    Result := (FAvailableFlags[AUVIndex shr 3] and BitmapSet[AUVIndex mod 8]) <> 0
  else
    Result := True;
end;

function TfcxNewFilterArray.GetUValue(AUVIndex: Integer): Variant;
begin
  Result := Unassigned
end;

function TfcxNewFilterArray.GetUVIndex(AUVVarValue: Variant): Integer;
begin
  Result := -1;
end;

procedure TfcxNewFilterArray.InverseFilter;
var
  i, AUVCount, ACount: integer;
begin
  if FUVFilterType = uvft_Single then
    Exit;
  FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
  AUVCount := UniqueValuesCount;
  if FilterCount = AUVCount then
  begin
    FreeMem(FFilters);
    FFilters := nil;
    FilterCount := 0;
  end
  else
  begin
    ACount := AUVCount shr 3 + 1;
    if FFilters = nil then
    begin
      GetMem(FFilters, SizeOf(_fcxByteArray) * ACount);
      FillChar(FFilters^, SizeOf(_fcxByteArray) * ACount, 255);
    end
    else
      for i := 0 to ACount - 1 do
        FFilters[i] := not FFilters[i];
    FilterCount := AUVCount - FilterCount;
  end;
  if FCubeFieldFilter.UVFilter.FActive then
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_ActivedFilterChanged])
  else
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_NonActivedFilterChanged]);
end;

function TfcxNewFilterArray.IsNull(AUVIndex: Integer): Boolean;
begin
  Result := False;
end;

procedure TfcxNewFilterArray.LoadFromXML(AItem: TfcxXMLItem);
var
  AfcxVarType: TfcxVarType;
  AVariant: Variant;
  i: integer;
  AStoreEnabledValues: boolean;
begin
  AfcxVarType := TfcxVarType(GetEnumValue(TypeInfo(TfcxVarType), AItem.Parent.Prop['fcxVarType']));
  AStoreEnabledValues := AItem.Parent.BoolProp['StoreEnabledValues'];
  if AStoreEnabledValues then
    InverseFilter;
  for i := 0 to AItem.Count - 1 do
  begin
    if AItem[i].PropExists('nullvalue') then
      AVariant := Null
    else
      case AfcxVarType of
        fcvtDate:
          AVariant := AItem[i].DateProp['value'];
        fcvtOrdinal:
          AVariant := AItem[i].IntProp['value'];
        fcvtFloat:
          AVariant := AItem[i].FloatProp['value'];
      else
        AVariant := AItem[i].Prop['value'];
      end;
    FilterByValue[AVariant] := AStoreEnabledValues;
  end;
end;

procedure TfcxNewFilterArray.ResetFilter;
begin
  SetAllFilter;
end;

procedure TfcxNewFilterArray.RollBack;
var
  ACount: integer;
begin
  if FSaveFilters = nil then
  begin
    FreeMem(FFilters);
    FFilters := nil;
  end
  else
  begin
    ACount := (UniqueValuesCount shr 3 + 1);
    if FFilters = nil then
      GetMem(FFilters, SizeOf(_fcxByteArray) * ACount);
    Move(FSaveFilters[0], FFilters[0], SizeOf(_fcxByteArray) * ACount);
  end;
  FilterCount := FSaveFilterCount;
  FUVSingleIndex := FSaveSingleIndex;
  ClearSave;
end;

procedure TfcxNewFilterArray.RollBackField;
var
  ACount: integer;
begin
  if not FCubeFieldFilter.UVFilter.FFilterManager.FUpdating then
    Exit;
  if FSaveFilters = nil then
  begin
    FreeMem(FFilters);
    FFilters := nil;
  end
  else
  begin
    ACount := (UniqueValuesCount shr 3 + 1);
    if FFilters = nil then
      GetMem(FFilters, SizeOf(_fcxByteArray) * ACount);
    Move(FSaveFilters[0], FFilters[0], SizeOf(_fcxByteArray) * (UniqueValuesCount shr 3 + 1));
  end;
  FilterCount := FSaveFilterCount;
  FUVSingleIndex := FSaveSingleIndex;
  ClearSave;
  FCubeFieldFilter.UVFilter.FFilterManager.FUpdating := False;
  FCubeFieldFilter.UVFilter.FFilterManager.FChanges := FCubeFieldFilter.UVFilter.FFilterManager.FChanges - [chf_Changed, chf_NonActivedFilterChanged, chf_ActivedFilterChanged];
  FCubeFieldFilter.UVFilter.FFilterManager.StopChange([]);
end;

procedure TfcxNewFilterArray.SaveToXML(AItem: TfcxXMLItem);
var
  ASubItem, ASubItem1: TfcxXMLItem;
  AfcxVarType: TfcxVarType;
  AVariant: Variant;
  i: integer;
begin
  AfcxVarType := FCubeFieldFilter.FField.fcxVarType;
  AItem.Prop['fcxVarType'] := GetEnumName(TypeInfo(TfcxVarType), Ord(AfcxVarType));
  AItem.BoolProp['StoreEnabledValues'] := FCubeFieldFilter.UVFilter.FFilterManager.StoreEnabledValues;
  if FFilterCount > 0 then
  begin
// SAVE IT
    ASubItem := AItem.Add;
    ASubItem.Name := 'values';
    for i := 0 to UVCount - 1 do
      if not (Filter[i] xor FCubeFieldFilter.UVFilter.FFilterManager.StoreEnabledValues) then
      begin
        ASubItem1 := ASubItem.Add;
        ASubItem1.Name := 'value';
        if IsNull(i) then
          ASubItem1.Prop['nullvalue'] := ''
        else
        begin
          AVariant := GetUValue(i);
          case AfcxVarType of
            fcvtDate:
              ASubItem1.DateProp['value'] := AVariant;
            fcvtOrdinal:
              ASubItem1.IntProp['value'] := AVariant;
            fcvtFloat:
              ASubItem1.FloatProp['value'] := AVariant;
          else
            ASubItem1.Prop['value'] := AVariant;
          end;
        end
      end
  end;
end;

procedure TfcxNewFilterArray.SetAllFilter;
begin
  if FUVFilterType = uvft_Single then
    Exit;
  FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
  if FFilters <> nil then
  begin
    FreeMem(FFilters);
    FFilters := nil;
  end;
  FilterCount := 0;
  if FCubeFieldFilter.UVFilter.FActive then
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_ActivedFilterChanged])
  else
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_NonActivedFilterChanged]);
end;

procedure TfcxNewFilterArray.SetAvailablesFlags(ARecIndex: integer;
  AFilteredByOtherFilters: Boolean);
begin
  if FCubeFieldFilter.UVFilter.FInternalResultForRec then
// Row VISIBLE. set FLAG for current UV
    SetAvailable(FCubeFieldFilter.UVFilter.FFilterManager.FCube.SourceHolder.UniqueValueIndex[ARecIndex, FCubeFieldFilter.FField])
  else
  if (not FInternalSelfResult) and (not AFilteredByOtherFilters or (FCubeFieldFilter.UVFilter.FInternalCountFilteredFieldsForRec > 1)) then
// Row HIDDEN by current UV. set FLAG for current UV
    SetAvailable(FCubeFieldFilter.UVFilter.FFilterManager.FCube.SourceHolder.UniqueValueIndex[ARecIndex, FCubeFieldFilter.FField]);
end;

procedure TfcxNewFilterArray.SetFilter(AUVIndex: Integer;
  const Value: Boolean);
var
  ACount: integer;
begin
  if (FUVFilterType <> uvft_Single) and (FFilters = nil) and not Value then
  begin
    ACount := (UniqueValuesCount shr 3 + 1);
    GetMem(FFilters, SizeOf(_fcxByteArray) * ACount);
    FillChar(FFilters^, SizeOf(_fcxByteArray) * ACount, 0);
  end;
  if (Filter[AUVIndex] <> Value) then
  begin
    if FUVFilterType = uvft_Single then
// проверка
      if (FUVSingleIndex = AUVIndex) or not Value then
// нельзя сбросить фильтр в режиме uvft_Single
        Exit;

    FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
    if FUVFilterType = uvft_Single then
      FUVSingleIndex := AUVIndex
    else
    begin
      if Value then
      begin
        FFilters[AUVIndex shr 3] := FFilters[AUVIndex shr 3] and BitmapUnSet[AUVIndex mod 8];
        FilterCount := FilterCount - 1;
        if FilterCount = 0 then
        begin
          FreeMem(FFilters);
          FFilters := Nil;
        end;
      end
      else
      begin
        FFilters[AUVIndex shr 3] := FFilters[AUVIndex shr 3] or BitmapSet[AUVIndex mod 8];
        FilterCount := FilterCount + 1;
      end;
    end;
    if FCubeFieldFilter.UVFilter.FActive then
      FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_ActivedFilterChanged])
    else
      FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_NonActivedFilterChanged]);
  end;
end;

procedure TfcxNewFilterArray.SetFilterByValue(AUVVarValue: Variant;
  const Value: Boolean);
var
  AUVIndex: integer;
begin
  AUVIndex := GetUVIndex(AUVVarValue);
  if (AUVIndex > -1) and (AUVIndex < UVCount) then
    Filter[AUVIndex] := Value;
end;

procedure TfcxNewFilterArray.SetFilterCount(const Value: Integer);
begin
  if FFilterCount <> Value then
  begin
    if FFilterCount = 0 then
      FCubeFieldFilter.CountFilteredFields := FCubeFieldFilter.CountFilteredFields + 1
    else
    if Value = 0 then
      FCubeFieldFilter.CountFilteredFields := FCubeFieldFilter.CountFilteredFields - 1;
    FFilterCount := Value;
  end
end;

procedure TfcxNewFilterArray.SetAvailable(AUVIndex: Integer);
begin
  if (FAvailableFlags[AUVIndex shr 3] and BitmapSet[AUVIndex mod 8]) = 0 then
  begin
    FAvailableFlags[AUVIndex shr 3] := FAvailableFlags[AUVIndex shr 3] or BitmapSet[AUVIndex mod 8];
    inc(FCubeFieldFilter.FUVAvailableCount);
  end
end;

procedure TfcxNewFilterArray.SetNoneFilter;
var
  AUVCount, ACount: integer;
begin
  if FUVFilterType = uvft_Single then
    Exit;
  FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
  AUVCount := UniqueValuesCount;
  ACount := (AUVCount shr 3 + 1);
  if FFilters = nil then
    GetMem(FFilters, SizeOf(_fcxByteArray) * ACount);
  FillChar(FFilters^, SizeOf(_fcxByteArray) * ACount, 255);
  FilterCount := AUVCount;
  if FCubeFieldFilter.UVFilter.FActive then
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_ActivedFilterChanged])
  else
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_NonActivedFilterChanged]);
end;

procedure TfcxNewFilterArray.SetOnlyAvailableSupport(const Value: boolean);
var
  ACount: integer;
begin
  if FOnlyAvailableSupport <> Value then
  begin
    FOnlyAvailableSupport := Value;
    if FOnlyAvailableSupport then
    begin
      ACount := UniqueValuesCount shr 3 + 1;
      GetMem(FAvailableFlags, SizeOf(_fcxByteArray) * ACount);
      FillChar(FAvailableFlags^, SizeOf(_fcxByteArray) * ACount, 255);
    end
    else
    begin
      FreeMem(FAvailableFlags);
      FAvailableFlags := nil;
    end
  end
end;

procedure TfcxNewFilterArray.SetRangeFilter(ARange: TfcxRanges);
var
  i, AUVCount: integer;
begin
  if FUVFilterType = uvft_Single then
    Exit;
  FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
  AUVCount := UniqueValuesCount;
  for i := 0 to AUVCount - 1 do
    SetFilter(i, ARange.Match(GetUValue(i)));
  if FCubeFieldFilter.UVFilter.FActive then
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_ActivedFilterChanged])
  else
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_NonActivedFilterChanged]);
end;

procedure TfcxNewFilterArray.SetUVFilterType(
  const Value: TfcxUVFilterType);
var
  AUVCount, i: integer;
begin
  if FUVFilterType <> Value then
  begin
    if Value = uvft_None then
      Exit;
// нужна реакция!
    if Value = uvft_Single then
    begin
      AUVCount := UniqueValuesCount;
// надо сбросить все значения, оставив только одно.
      FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
      FUVFilterType := uvft_Set;
      if FFilterCount <> 0 then
      begin
        for i := 0 to AUVCount - 1 do
        begin
          if Filter[i] then
          begin
            FUVSingleIndex := i;
            break;
          end;
        end;
      end
      else
        FUVSingleIndex := 0;
      FreeMem(FFilters);
      FFilters := Nil;
      FilterCount := 1;
      FUVFilterType := Value;
      FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_UVFilterTypeChanged]);
    end
    else
    begin
      FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
      FUVFilterType := Value;
      FilterCount := 0;
      Filter[FUVSingleIndex] := True;
      FUVSingleIndex := -1;
      FCubeFieldFilter.UVFilter.FFilterManager.StopChange([chf_UVFilterTypeChanged]);
    end;
  end;
end;

procedure TfcxNewFilterArray.SetUVSingleIndex(const Value: Integer);
begin
  if FUVFilterType = uvft_Single then
    Filter[Value] := True
  else
  begin
    FCubeFieldFilter.UVFilter.FFilterManager.StartChange;
    SetNoneFilter;
    Filter[Value] := True;
    FCubeFieldFilter.UVFilter.FFilterManager.StopChange;
  end
end;

procedure TfcxNewFilterArray.StartCalcOnlyAvailable(ANoFilters: Boolean = False);
var
  ACount: integer;
begin
  if FOnlyAvailableSupport then
  begin
    ACount := UniqueValuesCount shr 3 + 1;
    if ANoFilters then
      FillChar(FAvailableFlags^, SizeOf(_fcxByteArray) * ACount, 255)
    else
      FillChar(FAvailableFlags^, SizeOf(_fcxByteArray) * ACount, 0);
  end;
  FInternalSelfResult := True;
end;

function TfcxNewFilterArray.TestRecByFilter(ARecIndex: Integer): Boolean;
begin
  if (FFilters = nil) and (FUVFilterType <> uvft_Single) then
    Result := True
  else
    Result := Filter[FCubeFieldFilter.UVFilter.FFilterManager.FCube.SourceHolder.UniqueValueIndex[ARecIndex, FCubeFieldFilter.FField]];
  FInternalSelfResult := Result;
end;

function TfcxNewFilterArray.UniqueValuesCount: integer;
begin
  Result := 0;
end;

{ TfcxNewUVFilterArray }

function TfcxNewUVFilterArray.GetUValue(AUVIndex: Integer): Variant;
begin
  Result := TfcxCommonUVField(FCubeFieldFilter.FField).UniqueValues.ValueAsVariantByIndex[AUVIndex]
end;

function TfcxNewUVFilterArray.GetUVIndex(AUVVarValue: Variant): Integer;
begin
  Result := TfcxCommonUVField(FCubeFieldFilter.FField).UniqueValues.UVIndex[AUVVarValue];
end;

function TfcxNewUVFilterArray.IsNull(AUVIndex: Integer): Boolean;
begin
  Result := TfcxCommonUVField(FCubeFieldFilter.FField).UniqueValues.IsNull[AUVIndex];
end;

function TfcxNewUVFilterArray.UniqueValuesCount: integer;
begin
  Result := TfcxCommonUVField(FCubeFieldFilter.FField).UniqueValues.Count;
end;

{ TfcxNewDateSplitFilterArray }

function TfcxNewStdSplitFilterArray.GetUValue(AUVIndex: Integer): Variant;
begin
  Result := TfcxCommonStdPathField(FCubeFieldFilter.FField).StdPathUniqueValues.DataTypeProcessor.ValueAtIndex[AUVIndex];
end;

function TfcxNewStdSplitFilterArray.GetUVIndex(
  AUVVarValue: Variant): Integer;
begin
  Result := TfcxCommonStdPathField(FCubeFieldFilter.FField).StdPathUniqueValues.UVIndex[AUVVarValue];
end;

function TfcxNewStdSplitFilterArray.IsNull(AUVIndex: Integer): Boolean;
begin
  Result := TfcxCommonStdPathField(FCubeFieldFilter.FField).StdPathUniqueValues.DataTypeProcessor.IsNullAtIndex[AUVIndex];
end;

function TfcxNewStdSplitFilterArray.UniqueValuesCount: integer;
begin
  Result := TfcxCommonStdPathField(FCubeFieldFilter.FField).StdPathUniqueValues.DataTypeProcessor.CountUV
end;

end.
