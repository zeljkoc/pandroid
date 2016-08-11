{*******************************************************}
{                                                       }
{             FastCube 2 DataSource unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxDataSource;
{$INCLUDE fcx.inc}

interface
uses
  Classes, db, SysUtils,
  fcxTypes, fcxUniqueValue, fcxStringUtils
{$IFDEF FPC}
  , LCLType, LCLIntf
{$ELSE}
  , Windows
{$ENDIF}
  ;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, Data.db, System.SysUtils,
  FMX.fcxTypes, FMX.fcxUniqueValue, FMX.fcxStringUtils;
{$ENDIF FMX}

type
  // forward declarations
  TfcxDataSet = class;
  TfcxSourceFields = class;
  TfcxSourceField = class;
  TfcxSplitProperty = class;
//  TfcxAttribute = class;


  TfcxDataFieldPropery = record
    FieldName: String;
    DisplayLabel: String;
    Index: integer;
    Size: integer;
    Visible: Boolean;
    FieldType: TFieldType;
  end;

  TfcxGetCustomAttributeValue = function(Sender : TObject; AMasterFieldName, ASplitPathName: TfcxString; AMasterValue: Variant; var ASplitPathValue: Variant): boolean of object;
  TfcxGetCustomValue = function(Sender : TObject; ACubeFieldName: TfcxString; var AValue: Variant): boolean of object;
  TfcxGetDataSet = function: TfcxDataSet of object;
  TfcxBooleanEvent = function(Sender : TObject): Boolean of object;
  TfcxGetDataFieldPropertyAtEvent = procedure(Sender : TObject; AFieldIndex: integer; var ADataFieldPropery: TfcxDataFieldPropery) of object;
  TfcxGetVarDataEvent = function(Sender : TObject; AFieldIndex: Integer; out AValue: Variant): Boolean of object;
  TfcxGetData = function(var Buffer: Pointer): Boolean of object;

  TfcxDataSource = class;

  TfcxDataField = class(TPersistent)
  private
    FConvert: boolean;
    FGetDataSet: TfcxGetDataSet;
    FCubeFieldSize: integer;
    FCubeFieldName: TfcxString;
    FCubeFieldDisplayLabel: TfcxString;
    FCubeFieldType: TfcxDataType;
    FNullStr: TfcxString;
    FCaseSensitive: TfcxCaseSensitive;
    FInitialized: Boolean;
    procedure SetCubeFieldDisplayLabel(const Value: TfcxString);
    procedure SetCubeFieldName(const Value: TfcxString);
    procedure SetCubeFieldSize(const Value: integer);
    procedure SetCubeFieldType(const Value: TfcxDataType);
    procedure SetNullStr(const Value: TfcxString);
    procedure SetCaseSensitive(const Value: TfcxCaseSensitive);
    function GetCubeFieldDisplayLabel: TfcxString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDataNil(var Buffer: Pointer): Boolean;
  public
    GetData: TfcxGetData;
    constructor Create(AGetDataSet: TfcxGetDataSet); virtual;
    destructor Destroy; override;
// Name of Field in Cube
    property CubeFieldName: TfcxString read FCubeFieldName write SetCubeFieldName;
// Type of Field in Cube
    property CubeFieldType: TfcxDataType read FCubeFieldType write SetCubeFieldType;
// Size of Field in Cube
    property CubeFieldSize: integer read FCubeFieldSize write SetCubeFieldSize;
// Display label of Field in Cube
    property CubeFieldDisplayLabel: TfcxString read GetCubeFieldDisplayLabel write SetCubeFieldDisplayLabel;
// Display value for Null
    property NullStr: TfcxString read FNullStr write SetNullStr;
// CaseSensitive for compare and sort
    property CaseSensitive: TfcxCaseSensitive read FCaseSensitive write SetCaseSensitive default cs_Default;
// Field Initialized
    property Initialized: Boolean read FInitialized;
  published
  end;

  TfcxReferenceDataField = class(TfcxDataField)
  private
    FDataFieldIndex: integer;
    FDataFieldSize: integer;
    FDataFieldName: string;
    FDataFieldDisplayLabel: string;
    FDataFieldType: TfcxDataType;
    FUseNativeFormat: Boolean;
//DB
    FDBField: TField;
    FTestOk: Boolean;
{$IFDEF Delphi_17UP}
    FVBuffer: TValueBuffer;
{$ENDIF}
    procedure SetConvert(const Value: boolean);
    function GetDataSet: TfcxDataSet;
// DB
    function GetDBDataSet: TDataSet;
    procedure SetDataFieldName(const Value: string);
    function GetConvert: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDataDB(var Buffer: Pointer): Boolean;
    function GetDataDBValue(var Buffer: Pointer): Boolean;
    function GetDataEvents(var Buffer: Pointer): Boolean;

// DB
    property DBDataSet: TDataSet read GetDBDataSet;
    property DBField: TField read FDBField;
  public
    constructor Create(AGetDataSet: TfcxGetDataSet); override;
    destructor Destroy; override;
// Setting properties from DataSet
    function Init: boolean; virtual;
// Test properties
    function Test(APrefix: String; var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean;
// Get data from dataset

    property DataSet: TfcxDataSet read GetDataSet;
// Index of Field in DataSet
    property DataFieldIndex: integer read FDataFieldIndex;
// Type of Field in DataSet
    property DataFieldType: TfcxDataType read FDataFieldType;
// Size of Field in DataSet
    property DataFieldSize: integer read FDataFieldSize;
// Display label of Field in DataSet
    property DataFieldDisplayLabel: string read FDataFieldDisplayLabel;
// Convert value to new type
    property Convert: boolean read GetConvert write SetConvert;
// Name of Field in DataSet
    property DataFieldName: string read FDataFieldName write SetDataFieldName;
// Use Native Format for Field in DataSet
    property UseNativeFormat: Boolean read FUseNativeFormat;
// Field pass test
    property TestOk: Boolean read FTestOk;
  published
  end;

  TfcxMainCustomDataField = class(TfcxDataField)
  private
    FDataSource: TfcxDataSource;
  protected
    function GetDataEvents(var Buffer: Pointer): Boolean;
  public
    function Init: boolean; virtual;
  published
    property CubeFieldName;
    property CubeFieldType;
    property CubeFieldSize;
    property CubeFieldDisplayLabel;
    property NullStr;
    property CaseSensitive;
  end;

  TfcxMainDateDataField = class(TfcxDataField)
  public
    constructor Create(AGetDataSet: TfcxGetDataSet); override;
  published
    property CubeFieldName;
    property CubeFieldDisplayLabel;
    property NullStr;
  end;

  TfcxMainTimeDataField = class(TfcxDataField)
  public
    constructor Create(AGetDataSet: TfcxGetDataSet); override;
  published
    property CubeFieldName;
    property CubeFieldDisplayLabel;
    property NullStr;
  end;

  TfcxMainReferenceDataField = class(TfcxReferenceDataField)
  private
    FLoadAllValues: Boolean;
  public
    constructor Create(AGetDataSet: TfcxGetDataSet); override;
  published
    property Convert;
    property DataFieldName;
    property CubeFieldName;
    property CubeFieldType;
    property CubeFieldSize;
    property CubeFieldDisplayLabel;
    property NullStr;
    property CaseSensitive;
    property LoadAllValues: Boolean read FLoadAllValues write FLoadAllValues default False;
  end;

  TfcxAddonReferenceDataField = class(TfcxReferenceDataField)
  published
    property DataFieldName;
  end;

  TfcxDataSet = class(TComponent)
  private
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  public
    procedure GetFieldNames(List: TStrings); virtual; abstract;
    function FieldExists(ADataFieldName: string): boolean; virtual; abstract;
    function AssignedDataSet: boolean; virtual; abstract;

    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    function Eof: boolean; virtual; abstract;
    function Active: boolean; virtual; abstract;
    function FieldCount: integer; virtual;
    function DataFieldAt(AFieldIndex: integer): TfcxDataFieldPropery; virtual; abstract;
    function DataFieldByName(AFieldName: string): TfcxDataFieldPropery; virtual; abstract;
    function DataFieldClassCategoryAt(AFieldIndex: integer): TfcxFieldClassCategory; virtual; abstract;
    function DataFieldClassCategoryByName(AFieldName: string): TfcxFieldClassCategory; virtual; abstract;
  published
    property Version: String read GetVersion write SetVersion;
  end;

  TfcxDBDataSet = class;

  TfcxGetNextDatasetEvent = function(Sender: TfcxDBDataSet; var ADataSet: TDataSet): Boolean of object;

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxDBDataSet = class(TfcxDataSet)
  private
    FFirstDataSet: TDataSet;
    FDataSet: TDataSet;
    FUseGetDataInInheritedClasses: boolean;
    FUseMultiLoad: Boolean;
    FOnGetNextDataset: TfcxGetNextDatasetEvent;
    FOldDataSetActive: Boolean;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetUseMultiLoad(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetFieldNames(List: TStrings); override;
    function FieldExists(ADataFieldName: string): boolean; override;
    function AssignedDataSet: boolean; override;

    procedure Open; override;
    procedure Close; override;
    procedure First; override;
    procedure Next; override;
    function Eof: boolean; override;
    function Active: boolean; override;
    function FieldCount: integer; override;
    function DataFieldAt(AFieldIndex: integer): TfcxDataFieldPropery; override;
    function DataFieldByName(AFieldName: string): TfcxDataFieldPropery; override;
    function DataFieldClassCategoryAt(AFieldIndex: integer): TfcxFieldClassCategory; override;
    function DataFieldClassCategoryByName(AFieldName: string): TfcxFieldClassCategory; override;
  published
    property UseMultiLoad: Boolean read FUseMultiLoad write SetUseMultiLoad default False;
    property OnGetNextDataset: TfcxGetNextDatasetEvent read FOnGetNextDataset write FOnGetNextDataset;
    property UseGetDataInInheritedClasses: boolean read FUseGetDataInInheritedClasses write FUseGetDataInInheritedClasses default False;
    property DataSet: TDataSet read FDataSet write SetDataSet;
  end;

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxUserDataSet = class(TfcxDataSet)
  private
    FOnEof: TfcxBooleanEvent;
    FOnActive: TfcxBooleanEvent;
    FOnDataFieldAt: TfcxGetDataFieldPropertyAtEvent;
    FOnClose: TNotifyEvent;
    FOnFirst: TNotifyEvent;
    FOnNext: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FOnGetVarData: TfcxGetVarDataEvent;
    FFields: TStrings;
    procedure SetFields(const Value: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetFieldNames(List: TStrings); override;
    function FieldExists(ADataFieldName: string): boolean; override;
    function AssignedDataSet: boolean; override;
    procedure Open; override;
    procedure Close; override;
    procedure First; override;
    procedure Next; override;
    function Eof: boolean; override;
    function Active: boolean; override;
    function FieldCount: integer; override;
    function DataFieldAt(AFieldIndex: integer): TfcxDataFieldPropery; override;
    function DataFieldByName(AFieldName: string): TfcxDataFieldPropery; override;
    function DataFieldClassCategoryAt(AFieldIndex: integer): TfcxFieldClassCategory; override;
    function DataFieldClassCategoryByName(AFieldName: string): TfcxFieldClassCategory; override;
    function GetVarData(AFieldIndex: Integer; out AValue: Variant): Boolean;
  published
{ TODO -cНеобходимо : Пока Fields: TStrings, но нужен редактор полей и хранение свойств.}
    property Fields: TStrings read FFields write SetFields;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnFirst: TNotifyEvent read FOnFirst write FOnFirst;
    property OnNext: TNotifyEvent read FOnNext write FOnNext;
    property OnEof: TfcxBooleanEvent read FOnEof write FOnEof;
    property OnActive: TfcxBooleanEvent read FOnActive write FOnActive;
    property OnDataFieldAt: TfcxGetDataFieldPropertyAtEvent read FOnDataFieldAt write FOnDataFieldAt;
    property OnGetVarData: TfcxGetVarDataEvent read FOnGetVarData write FOnGetVarData;
  end;

{$IFNDEF DELPHI_6UP}
  TfcxCollection = class(TCollection)
  public
    function Owner: TPersistent;
  end;
{$ELSE}
  TfcxCollection = TCollection;
{$ENDIF}

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxDataSource = class(TComponent)
  private
    FDBOpenTime: Cardinal;
    FOpenTime: Cardinal;
    FDBCloseTime: Cardinal;
    FDataSet: TfcxDataSet;
    FFields: TfcxSourceFields;
    FDataSetList: TList;
    FDataSetActived: array of boolean;
    FOnGetCustomAttributeValue: TfcxGetCustomAttributeValue;
    FOnGetCustomValue: TfcxGetCustomValue;
    FDefaultDateSplitPaths: TfcxDateTypes;
    FDefaultTimeSplitPaths: TfcxTimeTypes;
    FUseOnlyVisibleFields: Boolean;
    procedure SetDataSet(const Value: TfcxDataSet);
    procedure SetFields(const Value: TfcxSourceFields);
    procedure SetDefaultDateSplitPaths(const Value: TfcxDateTypes);
    procedure SetDefaultTimeSplitPaths(const Value: TfcxTimeTypes);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetCustomAttributeValue(AMasterFieldName, ASplitPathName: TfcxString; AMasterValue: Variant; var ASplitPathValue: Variant): boolean;
    function GetCustomValue(ACubeFieldName: TfcxString; var AValue: Variant): boolean;
    function TestDataSet(var AErrorMessage: String; AList: TList): boolean;
    function Test(var AErrorMessage: TfcxString; ASkipFieldsWithError: Boolean): boolean;
    procedure DeleteFields;
    procedure AddFields;
    procedure InitFields(ALoaded: boolean = False);
    function Open: boolean;
    procedure Close;
    property DBOpenTime: Cardinal read FDBOpenTime;
    property OpenTime: Cardinal read FOpenTime;
    property DBCloseTime: Cardinal read FDBCloseTime;
  published
    property Version: String read GetVersion write SetVersion;
    property DataSet: TfcxDataSet read FDataSet write SetDataSet;
    property Fields: TfcxSourceFields read FFields write SetFields;
    property DefaultDateSplitPaths: TfcxDateTypes read FDefaultDateSplitPaths write SetDefaultDateSplitPaths default [];
    property DefaultTimeSplitPaths: TfcxTimeTypes read FDefaultTimeSplitPaths write SetDefaultTimeSplitPaths default [];
    property UseOnlyVisibleFields: Boolean read FUseOnlyVisibleFields write FUseOnlyVisibleFields default False;
    property OnGetCustomAttributeValue: TfcxGetCustomAttributeValue read FOnGetCustomAttributeValue write FOnGetCustomAttributeValue;
    property OnGetCustomValue: TfcxGetCustomValue read FOnGetCustomValue write FOnGetCustomValue;
  end;

  TfcxSplitProperty = class(TPersistent)
  private
    FField: TfcxSourceField;
    FDateSplitPaths: TfcxDateTypes;
    FTimeSplitPaths: TfcxTimeTypes;
    FAttributes: TfcxSourceFields;
    procedure SetDateSplitPaths(const Value: TfcxDateTypes);
    procedure SetTimeSplitPaths(const Value: TfcxTimeTypes);
    procedure SetAttributes(const Value: TfcxSourceFields);
    function GetParentComponent: TComponent;
    function GetDataSource: TfcxDataSource;
  protected
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AField: TfcxSourceField); virtual;
    destructor Destroy; override;
    function Test(var AErrorMessage: String): boolean; virtual;
  published
    property DateSplitPaths: TfcxDateTypes read FDateSplitPaths write SetDateSplitPaths default [];
    property TimeSplitPaths: TfcxTimeTypes read FTimeSplitPaths write SetTimeSplitPaths default [];
    property Attributes: TfcxSourceFields read FAttributes write SetAttributes;
    property ParentComponent: TComponent read GetParentComponent;
  end;

  TfcxSourceFieldProperties = class(TPersistent)
  private
    FSaved: Boolean;
    FSourceField: TfcxSourceField;
    FWithCustomCaption: Boolean;
    FSplitProperty: TfcxSplitProperty;
    FCaptionSourceAttribute: TfcxString;
    FOrderSourceAttribute: TfcxString;
    function GetSourceFieldName: TfcxString; virtual; abstract;
    procedure SetWithCustomCaption(const Value: Boolean);
    procedure SetSplitProperty(const Value: TfcxSplitProperty);
    procedure SetCaptionSourceAttribute(const Value: TfcxString);
    procedure SetOrderSourceAttribute(const Value: TfcxString);
    procedure InitField; virtual;
    function GetParentComponent: TComponent;
    procedure SetSaved(const Value: Boolean); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASourceField: TfcxSourceField); virtual;
    destructor Destroy; override;
    function Test(var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean; virtual;
    property SourceFieldName: TfcxString read GetSourceFieldName;
    property ParentComponent: TComponent read GetParentComponent;
    property Saved: Boolean read FSaved write SetSaved default True;
  published
    property SplitProperty: TfcxSplitProperty read FSplitProperty write SetSplitProperty;
    property WithCustomCaption: Boolean read FWithCustomCaption write SetWithCustomCaption default False;
    property CaptionSourceAttribute: TfcxString read FCaptionSourceAttribute write SetCaptionSourceAttribute;
    property OrderSourceAttribute: TfcxString read FOrderSourceAttribute write SetOrderSourceAttribute;
  end;

  TfcxCustomSourceFieldProperties = class(TfcxSourceFieldProperties)
  private
    FDataField: TfcxMainCustomDataField;
    FCalculateAfterAll: Boolean;
    function GetSourceFieldName: TfcxString; override;
    procedure InitField; override;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASourceField: TfcxSourceField); override;
    destructor Destroy; override;
  published
    property DataField: TfcxMainCustomDataField read FDataField write FDataField;
    property CalculateAfterAll: Boolean read FCalculateAfterAll write FCalculateAfterAll default False;
    property Saved;
  end;

  TfcxDateSourceFieldProperties = class(TfcxSourceFieldProperties)
  private
    FDataField: TfcxMainDateDataField;
    function GetSourceFieldName: TfcxString; override;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASourceField: TfcxSourceField); override;
    destructor Destroy; override;
  published
    property DataField: TfcxMainDateDataField read FDataField write FDataField;
    property Saved;
  end;

  TfcxTimeSourceFieldProperties = class(TfcxSourceFieldProperties)
  private
    FDataField: TfcxMainTimeDataField;
    function GetSourceFieldName: TfcxString; override;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASourceField: TfcxSourceField); override;
    destructor Destroy; override;
  published
    property DataField: TfcxMainTimeDataField read FDataField write FDataField;
    property Saved;
  end;

  TfcxReferenceSourceFieldProperties = class(TfcxSourceFieldProperties)
  private
    FDataField: TfcxMainReferenceDataField;
    function GetSourceFieldName: TfcxString; override;
//    FWithGroup: Boolean;
    function GetDataSet: TfcxDataSet; virtual;
    procedure SetDataSet(const Value: TfcxDataSet); virtual;
//    procedure SetWithGroup(const Value: Boolean);
    procedure InitField; override;
    procedure SetSaved(const Value: Boolean); override;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASourceField: TfcxSourceField); override;
    destructor Destroy; override;
    property DataSet: TfcxDataSet read GetDataSet  write SetDataSet;
    function Test(var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean; override;
  published
    property DataField: TfcxMainReferenceDataField read FDataField write FDataField;
//    property WithGroup: Boolean read FWithGroup write SetWithGroup;
  end;

  TfcxReferenceAttributeSFProperties = class(TfcxReferenceSourceFieldProperties)
  private
    FDataSet: TfcxDataSet;
    FIdField: TfcxAddonReferenceDataField;
    procedure SetDataSet(const Value: TfcxDataSet); override;
    function GetDataSet: TfcxDataSet; override;
    procedure InitField; override;
    procedure SetSaved(const Value: Boolean); override;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASourceField: TfcxSourceField); override;
    destructor Destroy; override;
    function Test(var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean; override;
  published
    property IdField: TfcxAddonReferenceDataField read FIdField write FIdField;
    property DataSet;
  end;


  TfcxSourceField = class(TCollectionItem)
  private
    FSourceFieldProperties: TfcxSourceFieldProperties;
    FSourceFieldType: TfcxAttributeType;
    procedure SetSourceFieldProperties(const Value: TfcxSourceFieldProperties);
    procedure SetSourceFieldType(const Value: TfcxAttributeType);
    function GetDataField: TfcxDataField;
    function GetCaptionSourceAttribute: TfcxString;
    function GetOrderSourceAttribute: TfcxString;
    function GetSplitProperty: TfcxSplitProperty;
    function GetWithCustomCaption: Boolean;
    function GetParentComponent: TComponent;
    function TestDataSet(var AErrorMessage: String; ADataSetList: TfcxStringList; AfcDataSetList: TfcxStringList): boolean;
    function GetDataSource: TfcxDataSource;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property DataField: TfcxDataField read GetDataField;
    property SplitProperty: TfcxSplitProperty read GetSplitProperty;
    property WithCustomCaption: Boolean read GetWithCustomCaption;
    property CaptionSourceAttribute: TfcxString read GetCaptionSourceAttribute;
    property OrderSourceAttribute: TfcxString read GetOrderSourceAttribute;
    property ParentComponent: TComponent read GetParentComponent;
  published
    property SourceFieldType: TfcxAttributeType read FSourceFieldType write SetSourceFieldType default fcxsft_Custom;
    property SourceFieldProperties: TfcxSourceFieldProperties read FSourceFieldProperties write SetSourceFieldProperties;
  end;

  TfcxSourceFields = class(TfcxCollection)
  private
    FDataSource: TfcxDataSource;
    FSplitProperty: TfcxSplitProperty;
    function GetDataSet: TfcxDataSet;
    function GetField(AIndex: integer): TfcxSourceField;
    property DataSet: TfcxDataSet read GetDataSet;
    function GetParentComponent: TComponent;
    procedure InitField;
    function GetFieldByName(AFieldName: TfcxString): TfcxSourceField;
    function GetDataSource: TfcxDataSource;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TObject);
    property Field[AIndex: integer]: TfcxSourceField read GetField; default;
    property FieldByName[AFieldName: TfcxString]: TfcxSourceField read GetFieldByName;
    property ParentComponent: TComponent read GetParentComponent;
  published
  end;

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxRes;
//FMX uses
{$ELSE FMX}
uses
  FMX.fcxRes;
{$ENDIF FMX}

{ TfcxDataSource }

procedure TfcxDataSource.AddFields;
var
  i: integer;
  ADataField: TfcxDataFieldPropery;
  AField: TfcxSourceField;
begin
  DeleteFields;
  if DataSet <> nil then
    if DataSet.AssignedDataSet then
      for i := 0 to DataSet.FieldCount - 1 do
        if (not FUseOnlyVisibleFields) or DataSet.DataFieldAt(i).Visible then
          if DataSet.DataFieldClassCategoryAt(i) <> fcfcc_Error then
          begin
            ADataField := DataSet.DataFieldAt(i);
            AField := TfcxSourceField(FFields.Add);
            AField.SourceFieldType := fcxsft_Reference;
            TfcxReferenceSourceFieldProperties(AField.SourceFieldProperties).DataField.DataFieldName := ADataField.FieldName;
          end;
end;

procedure TfcxDataSource.Close;
var
  i: integer;
  ATickCount: Cardinal;
begin
  try
    FDBCloseTime := 0;
    for i := 0 to FDataSetList.Count - 1 do
    begin
      if not FDataSetActived[i] then
      begin
        ATickCount := fcxGetTickCount;
        TfcxDataSet(FDataSetList.Items[i]).Close;
        FDBCloseTime := FDBCloseTime + fcxGetTickCount - ATickCount;
      end
    end;
  finally
    SetLength(FDataSetActived, 0);
    FreeAndNil(FDataSetList);
  end;
end;

constructor TfcxDataSource.Create(AOwner: TComponent);
begin
  inherited;
  FDataSetList := nil;
  FDefaultDateSplitPaths := [];
  FDefaultTimeSplitPaths := [];
  FUseOnlyVisibleFields := False;
  FFields := TfcxSourceFields.Create(Self);
end;

procedure TfcxDataSource.DeleteFields;
begin
  FFields.Clear;
end;

destructor TfcxDataSource.Destroy;
begin
  FFields.Free;
  FDataSetList.Free;
  inherited;
end;

function TfcxDataSource.GetCustomAttributeValue(AMasterFieldName,
  ASplitPathName: TfcxString; AMasterValue: variant;
  var ASplitPathValue: variant): boolean;
begin
  if Assigned(FOnGetCustomAttributeValue) then
    Result := FOnGetCustomAttributeValue(Self, AMasterFieldName, ASplitPathName, AMasterValue, ASplitPathValue)
  else
    Result := False;
end;

function TfcxDataSource.GetCustomValue(ACubeFieldName: TfcxString;
  var AValue: Variant): boolean;
begin
  if Assigned(FOnGetCustomValue) then
    Result := FOnGetCustomValue(Self, ACubeFieldName, AValue)
  else
    Result := False;
end;

function TfcxDataSource.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxDataSource.InitFields(ALoaded: boolean = False);
var
  i: integer;
begin
// only after Test !!!
  if DataSet <> nil then
    if DataSet.AssignedDataSet then
    begin
      for i := 0 to Fields.Count - 1 do
        case FFields[i].SourceFieldType of
          fcxsft_Custom:
            begin
//              if TfcxCustomSourceFieldProperties(FFields[i].SourceFieldProperties).DataField.TestOk or ALoaded then
                FFields[i].SourceFieldProperties.InitField;
            end;
          fcxsft_Reference:
            begin
              if TfcxReferenceSourceFieldProperties(FFields[i].SourceFieldProperties).DataField.TestOk or ALoaded then
                FFields[i].SourceFieldProperties.InitField;
            end;
        else
          FFields[i].SourceFieldProperties.InitField;
        end;
    end
end;

procedure TfcxDataSource.Loaded;
begin
  inherited;
  InitFields;
end;

procedure TfcxDataSource.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i, j: integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TfcxDataSet) then
    if (FDataSet <> nil) then
    begin
      if (AComponent = FDataSet) then
        DataSet := nil;
      for i := 0 to FFields.Count - 1 do
      begin
        for j := 0 to FFields[i].SplitProperty.Attributes.Count - 1 do
          if FFields[i].SplitProperty.Attributes[j].SourceFieldType = fcxsft_Reference then
            if TfcxReferenceAttributeSFProperties(FFields[i].SplitProperty.Attributes[j].SourceFieldProperties).DataSet = AComponent then
              TfcxReferenceAttributeSFProperties(FFields[i].SplitProperty.Attributes[j].SourceFieldProperties).DataSet := nil;
      end;
    end;
end;

function TfcxDataSource.Open: boolean;
var
  AMessage: TfcxString;
  i: integer;
  ATickCount, ATickCount2: Cardinal;
begin
  FOpenTime := 0;
  ATickCount := fcxGetTickCount;
  try
    FDBOpenTime := 0;
    if FDataSetList <> nil then
      Close;
    FDataSetList := TList.Create;
    AMessage := '';
    Result := TestDataSet(AMessage, FDataSetList);
    if Result then
    begin
      SetLength(FDataSetActived, FDataSetList.Count);
      for i := 0 to FDataSetList.Count - 1 do
      begin
        FDataSetActived[i] := TfcxDataSet(FDataSetList.Items[i]).Active;
        FOpenTime := FOpenTime + fcxGetTickCount - ATickCount;
        if not FDataSetActived[i] then
        begin
          ATickCount2 := fcxGetTickCount;
          TfcxDataSet(FDataSetList.Items[i]).Open;
          FDBOpenTime := FDBOpenTime + fcxGetTickCount - ATickCount2;
        end;
        ATickCount := fcxGetTickCount;
      end;
    end;
  except
    Result := False;
  end;
  FOpenTime := FOpenTime + fcxGetTickCount - ATickCount;
end;

procedure TfcxDataSource.SetDataSet(const Value: TfcxDataSet);
begin
  if FDataSet <> Value then
    if Value <> nil then
      Value.FreeNotification(self);
  FDataSet := Value;
end;

procedure TfcxDataSource.SetDefaultDateSplitPaths(
  const Value: TfcxDateTypes);
begin
  FDefaultDateSplitPaths := Value;
  if odt_None in FDefaultDateSplitPaths then
    FDefaultDateSplitPaths := FDefaultDateSplitPaths - [odt_None];
end;

procedure TfcxDataSource.SetDefaultTimeSplitPaths(
  const Value: TfcxTimeTypes);
begin
  FDefaultTimeSplitPaths := Value;
  if ott_None in FDefaultTimeSplitPaths then
    FDefaultTimeSplitPaths := FDefaultTimeSplitPaths - [ott_None];
end;

procedure TfcxDataSource.SetFields(const Value: TfcxSourceFields);
begin
  FFields.Assign(Value);
end;

procedure TfcxDataSource.SetVersion(const Value: String);
begin
//
end;

function TfcxDataSource.Test(var AErrorMessage: TfcxString; ASkipFieldsWithError: Boolean): boolean;
var
  I, AIndex: integer;
  AFieldNameList: TfcxStringList;
begin
  AErrorMessage := '';
{ TODO -cНеобходимо : Сделать проверку датасетов.}
// collect info about DataSet
  Result := TestDataSet(AErrorMessage, nil);
// check field name duplicate
  if Result then
  begin
    AFieldNameList := TfcxStringList.Create;
    try
      for I := 0 to FFields.Count - 1 do
        if FFields[i].DataField.CubeFieldName <> '' then
          if fcFindInfcStringListSorted(AFieldNameList, FFields[i].DataField.CubeFieldName, AIndex) then
          begin
            AErrorMessage := AErrorMessage + 'Duplicate Field Name: ' + FFields[i].DataField.CubeFieldName + ';'+#13#10;
            Result := False;
          end
          else
            AFieldNameList.Insert(AIndex, FFields[i].DataField.CubeFieldName);
    finally
      AFieldNameList.Free;
    end;
    AFieldNameList := TfcxStringList.Create;
    try
      for I := 0 to FFields.Count - 1 do
        if FFields[i].DataField.CubeFieldDisplayLabel <> '' then
          if fcFindInfcStringListSorted(AFieldNameList, FFields[i].DataField.CubeFieldDisplayLabel, AIndex) then
          begin
            AErrorMessage := AErrorMessage + 'Duplicate Field Label: ' + FFields[i].DataField.CubeFieldDisplayLabel + ';'+#13#10;
            Result := False;
          end
          else
            AFieldNameList.Insert(AIndex, FFields[i].DataField.CubeFieldDisplayLabel);
    finally
      AFieldNameList.Free;
    end;
  end;
// check fields
  if Result then
    for I := 0 to FFields.Count - 1 do
      Result := Result and FFields[i].SourceFieldProperties.Test(AErrorMessage, ASkipFieldsWithError);

  if not Result then
    for I := 0 to FFields.Count - 1 do
    begin
        case FFields[i].SourceFieldType of
          fcxsft_Custom:
            begin
//              TfcxCustomSourceFieldProperties(FFields[i].SourceFieldProperties).DataField.FTestOk := False;
//              TfcxCustomSourceFieldProperties(FFields[i].SourceFieldProperties).DataField.FInitialized := False;
            end;
          fcxsft_Reference:
            begin
              TfcxReferenceSourceFieldProperties(FFields[i].SourceFieldProperties).DataField.FTestOk := False;
              TfcxReferenceSourceFieldProperties(FFields[i].SourceFieldProperties).DataField.FInitialized := False;
            end;
        else;
        end;
    end;
end;

function TfcxDataSource.TestDataSet(var AErrorMessage: String; AList: TList): boolean;
var
  i: integer;
  ADataSetList: TfcxStringList;
  AfcDataSetList: TfcxStringList;

  function FindInfcDataSetList(AObject: TObject): integer;
  begin
    for Result := 0 to AfcDataSetList.Count - 1 do
      if AfcDataSetList.Objects[Result] = AObject then
        Exit;
    Result := -1;
  end;

  function FindInDataSetList(AObject: TObject): integer;
  begin
    for Result := 0 to ADataSetList.Count - 1 do
      if ADataSetList.Objects[Result] = AObject then
        Exit;
    Result := -1;
  end;

begin
  Result := True;
  ADataSetList := TfcxStringList.Create;
  AfcDataSetList := TfcxStringList.Create;
  if AList <> nil then
    AList.Clear;
  try
    if (DataSet = nil) then
    begin
      AErrorMessage := AErrorMessage + 'Main DataSet is not assigned;'+#13#10;
      Result := False;
    end
    else
    begin
      if not DataSet.AssignedDataSet then
      begin
        AErrorMessage := AErrorMessage + 'Main DataSet is not assigned;'+#13#10;
        Result := False;
      end
      else
      begin
        if DataSet is TfcxDBDataSet then
        begin
          ADataSetList.AddObject('Fact table', TfcxDBDataSet(DataSet).DataSet);
          AfcDataSetList.AddObject('Fact table', DataSet);
        end
        else
        begin
// TfcxUserDataSet
          ADataSetList.AddObject('Fact table', nil);
          AfcDataSetList.AddObject('Fact table', DataSet);
        end;
      end;
      for I := 0 to FFields.Count - 1 do
// check reference dataset
        Result := Result and FFields[i].TestDataSet(AErrorMessage, ADataSetList, AfcDataSetList);
    end;
    if result and (AList <> nil) then
      for i := 0 to AfcDataSetList.Count - 1 do
        AList.Add(AfcDataSetList.Objects[i]);
  finally
    ADataSetList.Free;
    AfcDataSetList.Free;
  end;
end;

{ TfcxSourceFields }

constructor TfcxSourceFields.Create(AOwner: TObject);
begin
  if AOwner is TfcxDataSource then
  begin
    FDataSource := TfcxDataSource(AOwner);
    FSplitProperty := nil;
  end
  else
  begin
    FDataSource := nil;
    FSplitProperty := TfcxSplitProperty(AOwner);
  end;
  inherited Create(TfcxSourceField);
end;

function TfcxSourceFields.GetDataSet: TfcxDataSet;
begin
  if FDataSource <> nil then
    Result := FDataSource.DataSet
  else
  if FSplitProperty.FField.SourceFieldProperties is TfcxReferenceSourceFieldProperties then
    Result := TfcxReferenceSourceFieldProperties(FSplitProperty.FField.SourceFieldProperties).DataSet
  else
    Result := Nil;
end;

function TfcxSourceFields.GetDataSource: TfcxDataSource;
begin
  if FDataSource <> nil then
    Result := FDataSource
  else
    Result := FSplitProperty.GetDataSource;
end;

function TfcxSourceFields.GetField(AIndex: integer): TfcxSourceField;
begin
  Result := TfcxSourceField(Items[AIndex]);
end;

function TfcxSourceFields.GetFieldByName(
  AFieldName: TfcxString): TfcxSourceField;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if fcStrCompare(Field[i].SourceFieldProperties.SourceFieldName, AFieldName) = 0 then
    begin
      Result := Field[i];
      Exit;
    end;
  Result := nil;
end;

function TfcxSourceFields.GetOwner: TPersistent;
begin
  if FDataSource <> nil then
    Result := FDataSource
  else
    Result := FSplitProperty;
end;

function TfcxSourceFields.GetParentComponent: TComponent;
begin
  if FDataSource <> nil then
    Result := FDataSource
  else
    Result := FSplitProperty.ParentComponent;
end;

procedure TfcxSourceFields.InitField;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Field[i].SourceFieldType = fcxsft_Reference then
      TfcxReferenceAttributeSFProperties(Field[i].SourceFieldProperties).InitField;
end;

{ TfcxReferenceSourceFieldProperties }

procedure TfcxReferenceSourceFieldProperties.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TfcxReferenceSourceFieldProperties then
    with TfcxReferenceSourceFieldProperties(Dest) do
    begin
      DataField.Assign(Self.DataField);
//      WithGroup := Self.WithGroup;
    end
end;

constructor TfcxReferenceSourceFieldProperties.Create(ASourceField: TfcxSourceField);
begin
  inherited;
  FDataField := TfcxMainReferenceDataField.Create(GetDataSet);
end;

destructor TfcxReferenceSourceFieldProperties.Destroy;
begin
  FreeAndNil(FDataField);
  inherited;
end;

function TfcxReferenceSourceFieldProperties.GetDataSet: TfcxDataSet;
begin
  Result := TfcxSourceFields(FSourceField.Collection).DataSet;
end;

{
procedure TfcxReferenceSourceFieldProperties.SetWithGroup(const Value: Boolean);
begin
  FWithGroup := Value;
end;
}

function TfcxReferenceSourceFieldProperties.Test(var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean;
begin
  Result := DataField.Test('', AErrorMessage, ASkipFieldsWithError);
  Result := Result and inherited Test(AErrorMessage, ASkipFieldsWithError);
end;

procedure TfcxReferenceSourceFieldProperties.InitField;
begin
  if DataField.Init then
  begin
    inherited;
  end;
// возможно и др свойства
end;

function TfcxReferenceSourceFieldProperties.GetSourceFieldName: TfcxString;
begin
  Result := FDataField.DataFieldName
end;

procedure TfcxReferenceSourceFieldProperties.SetDataSet(
  const Value: TfcxDataSet);
begin
//
end;

procedure TfcxReferenceSourceFieldProperties.SetSaved(
  const Value: Boolean);
begin
  FSaved := True;
end;

{ TfcxSourceField }

procedure TfcxSourceField.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxSourceField then
    with TfcxSourceField(Dest) do
    begin
      SourceFieldType := Self.SourceFieldType;
      SourceFieldProperties.Assign(Self.SourceFieldProperties);
    end
  else inherited AssignTo(Dest);
end;

constructor TfcxSourceField.Create(Collection: TCollection);
begin
  inherited;
  FSourceFieldType := fcxsft_Custom;
  FSourceFieldProperties := TfcxCustomSourceFieldProperties.Create(Self);
end;

destructor TfcxSourceField.Destroy;
begin
  FSourceFieldProperties.Free;
  inherited;
end;

function TfcxSourceField.GetCaptionSourceAttribute: TfcxString;
begin
  Result := SourceFieldProperties.CaptionSourceAttribute;
end;

function TfcxSourceField.GetDataField: TfcxDataField;
begin
  case FSourceFieldType of
    fcxsft_Custom: Result := TfcxCustomSourceFieldProperties(SourceFieldProperties).DataField;
    fcxsft_Reference: Result := TfcxReferenceSourceFieldProperties(SourceFieldProperties).DataField;
    fcxsft_Date: Result := TfcxDateSourceFieldProperties(SourceFieldProperties).DataField;
    fcxsft_Time: Result := TfcxTimeSourceFieldProperties(SourceFieldProperties).DataField;
  else
    Result := nil;
  end;
end;

function TfcxSourceField.GetDataSource: TfcxDataSource;
begin
  Result := TfcxSourceFields(Collection).GetDataSource;
end;

function TfcxSourceField.GetDisplayName: string;
begin
  Result := FSourceFieldProperties.SourceFieldName;
  case FSourceFieldType of
    fcxsft_Custom:
      Result := Result + ' (Custom)';
    fcxsft_Reference:
      Result := Result + ' (Reference)';
    fcxsft_Date:
      Result := Result + ' (Date)';
    fcxsft_Time:
      Result := Result + ' (Time)';
  end;
end;

function TfcxSourceField.GetOrderSourceAttribute: TfcxString;
begin
  Result := SourceFieldProperties.OrderSourceAttribute;
end;

function TfcxSourceField.GetParentComponent: TComponent;
begin
  Result := TfcxSourceFields(Collection).ParentComponent;
end;

function TfcxSourceField.GetSplitProperty: TfcxSplitProperty;
begin
  Result := SourceFieldProperties.SplitProperty;
end;

function TfcxSourceField.GetWithCustomCaption: Boolean;
begin
  Result := SourceFieldProperties.WithCustomCaption;
end;

procedure TfcxSourceField.SetSourceFieldProperties(
  const Value: TfcxSourceFieldProperties);
begin
  FSourceFieldProperties.Assign(Value);
end;

procedure TfcxSourceField.SetSourceFieldType(
  const Value: TfcxAttributeType);
begin
  if FSourceFieldType <> Value then
  begin
    if (Collection.Owner is TfcxDataSource) and (Value in [fcxsft_Date, fcxsft_Time]) then
      Exit;
    FreeAndNil(FSourceFieldProperties);
    case Value of
      fcxsft_Custom:
        FSourceFieldProperties := TfcxCustomSourceFieldProperties.Create(Self);
      fcxsft_Reference:
        if Collection.Owner is TfcxDataSource then
          FSourceFieldProperties := TfcxReferenceSourceFieldProperties.Create(Self)
        else
          FSourceFieldProperties := TfcxReferenceAttributeSFProperties.Create(Self);
      fcxsft_Date:
        FSourceFieldProperties := TfcxDateSourceFieldProperties.Create(Self);
      fcxsft_Time:
        FSourceFieldProperties := TfcxTimeSourceFieldProperties.Create(Self);
    end;
    FSourceFieldType := Value;
  end;
end;

function TfcxSourceField.TestDataSet(var AErrorMessage: String; ADataSetList: TfcxStringList; AfcDataSetList: TfcxStringList): boolean;
var
  j: integer;
begin
  Result := True;
{TODO: check reference dataset}
  for j := 0 to SplitProperty.Attributes.Count - 1 do
  begin
    if SplitProperty.Attributes[j].SourceFieldType = fcxsft_Reference then
    begin
      if TfcxReferenceAttributeSFProperties(SplitProperty.Attributes[j].SourceFieldProperties).DataSet = nil then
      begin
        AErrorMessage := AErrorMessage + 'Reference DataSet for field ' + SourceFieldProperties.SourceFieldName + ' is not assigned;'+#13#10;
        Result := False;
      end
      else
      begin
// тут нужно проверять кучу всего, но пока делаем попростому.
        if TfcxReferenceAttributeSFProperties(SplitProperty.Attributes[j].SourceFieldProperties).DataSet.AssignedDataSet then
        begin
          if TfcxReferenceAttributeSFProperties(SplitProperty.Attributes[j].SourceFieldProperties).DataSet is TfcxDBDataSet then
          begin
            ADataSetList.AddObject('Reference DataSet for field ' + SourceFieldProperties.SourceFieldName, TfcxDBDataSet(TfcxReferenceAttributeSFProperties(SplitProperty.Attributes[j].SourceFieldProperties).DataSet).DataSet);
            AfcDataSetList.AddObject('Reference DataSet for field ' + SourceFieldProperties.SourceFieldName, TfcxReferenceAttributeSFProperties(SplitProperty.Attributes[j].SourceFieldProperties).DataSet);
          end
          else
          begin
            ADataSetList.AddObject('Reference DataSet for field ' + SourceFieldProperties.SourceFieldName, nil);
            AfcDataSetList.AddObject('Reference DataSet for field ' + SourceFieldProperties.SourceFieldName, TfcxReferenceAttributeSFProperties(SplitProperty.Attributes[j].SourceFieldProperties).DataSet);
          end
        end
        else
        begin
          AErrorMessage := AErrorMessage + 'Reference DataSet for field ' + SourceFieldProperties.SourceFieldName + ' is not assigned;'+#13#10;
          Result := False;
        end;
      end
    end;
// check reference dataset
    Result := Result and SplitProperty.Attributes[j].TestDataSet(AErrorMessage, ADataSetList, AfcDataSetList);
  end;
end;

{ TfcxDBDataSet }

function TfcxDBDataSet.Active: boolean;
begin
  Result := FDataSet.Active;
end;

function TfcxDBDataSet.AssignedDataSet: boolean;
begin
  Result := (FDataSet <> nil);
end;

procedure TfcxDBDataSet.Close;
begin
  if Assigned(FFirstDataSet) and (FFirstDataSet <> FDataSet) then
  begin
    FDataSet.Active := FOldDataSetActive;
    FDataSet := FFirstDataSet;
  end;
  FDataSet.Close;
end;

constructor TfcxDBDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FDataSet := nil;
  FUseGetDataInInheritedClasses := False;
  FUseMultiLoad := False;
end;

function TfcxDBDataSet.DataFieldAt(AFieldIndex: integer): TfcxDataFieldPropery;
begin
  Result.DisplayLabel := FDataSet.Fields[AFieldIndex].DisplayLabel;
  Result.FieldName := FDataSet.Fields[AFieldIndex].FieldName;
  Result.FieldType := FDataSet.Fields[AFieldIndex].DataType;
  Result.Index := FDataSet.Fields[AFieldIndex].Index;
  Result.Size := FDataSet.Fields[AFieldIndex].Size;
  Result.Visible := FDataSet.Fields[AFieldIndex].Visible;
end;

function TfcxDBDataSet.DataFieldByName(AFieldName: string): TfcxDataFieldPropery;
var
  AField: TField;
begin
  AField := FDataSet.FindField(AFieldName);
  if AField <> nil then
    Result := DataFieldAt(AField.Index)
  else
  begin
    Result.DisplayLabel := '';
    Result.FieldName := '';
    Result.FieldType := ftUnknown;
    Result.Index := -1;
    Result.Size := 0;
    Result.Visible := False;
  end;
end;

destructor TfcxDBDataSet.Destroy;
begin
  inherited;
end;

function TfcxDBDataSet.Eof: boolean;
var
  ANextDataSet: TDataSet;
  AOldDataSetActive, AState: Boolean;
begin
  Result := FDataSet.Eof;
  if Result and FUseMultiLoad and Assigned(FOnGetNextDataset) then
  begin
    ANextDataSet := nil;
    if FOnGetNextDataset(Self, ANextDataSet) and Assigned(ANextDataSet) then
    begin
      if FFirstDataSet <> FDataSet then
        FDataSet.Active := FOldDataSetActive;
      AOldDataSetActive := ANextDataSet.Active;
      AState := False;
      try
        ANextDataSet.Open;
        AState := True;
      except
      end;
      if AState then
      begin
        FDataSet := ANextDataSet;
        FDataSet.First;
        FOldDataSetActive := AOldDataSetActive;
        Result := False;
      end
    end;
  end
end;


function TfcxDBDataSet.FieldCount: integer;
begin
  Result := FDataSet.FieldCount;
end;

procedure TfcxDBDataSet.First;
begin
  FDataSet.First;
end;

procedure TfcxDBDataSet.GetFieldNames(List: TStrings);
begin
  if FDataSet <> nil then
    FDataSet.GetFieldNames(List);
end;

function TfcxDBDataSet.FieldExists(ADataFieldName: string): boolean;
begin
  if FDataSet <> nil then
    Result := (FDataSet.FindField(ADataFieldName) <> nil)
  else
    Result := False;
end;

procedure TfcxDBDataSet.Next;
begin
  FDataSet.MoveBy(1);// Next;
end;

procedure TfcxDBDataSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TDataSet) then
    if (FDataSet <> nil) and (AComponent = FDataSet) then
      DataSet := nil;
end;

procedure TfcxDBDataSet.Open;
begin
  FFirstDataSet := FDataSet;
  FDataSet.Open;
end;

procedure TfcxDBDataSet.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
    if Value <> nil then
      Value.FreeNotification(self);
  FDataSet := Value;
end;

function TfcxDBDataSet.DataFieldClassCategoryAt(
  AFieldIndex: integer): TfcxFieldClassCategory;
begin
  Result := fcFieldClassCategory(FDataSet.Fields[AFieldIndex], FUseGetDataInInheritedClasses)
end;

function TfcxDBDataSet.DataFieldClassCategoryByName(
  AFieldName: string): TfcxFieldClassCategory;
var
  AField: TField;
begin
  AField := FDataSet.FindField(AFieldName);
  if AField <> nil then
    Result := DataFieldClassCategoryAt(AField.Index)
  else
    Result := fcfcc_Error;
end;

procedure TfcxDBDataSet.SetUseMultiLoad(const Value: Boolean);
begin
  FUseMultiLoad := Value;
end;

{ TfcxSplitProperty }

procedure TfcxSplitProperty.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxSplitProperty then
    with TfcxSplitProperty(Dest) do
    begin
      DateSplitPaths := Self.DateSplitPaths;
      TimeSplitPaths := Self.TimeSplitPaths;
      Attributes.Assign(Self.Attributes);
    end
  else inherited AssignTo(Dest);
end;

constructor TfcxSplitProperty.Create(AField: TfcxSourceField);
begin
  FField := AField;
  FTimeSplitPaths := GetDataSource.DefaultTimeSplitPaths;
  FDateSplitPaths := GetDataSource.DefaultDateSplitPaths;
  FAttributes := TfcxSourceFields.Create(Self);
end;

destructor TfcxSplitProperty.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

function TfcxSplitProperty.GetDataSource: TfcxDataSource;
begin
  Result := FField.GetDataSource;
end;

function TfcxSplitProperty.GetOwner: TPersistent;
begin
  Result := FField;
end;

function TfcxSplitProperty.GetParentComponent: TComponent;
begin
  Result := FField.ParentComponent;
end;

procedure TfcxSplitProperty.SetAttributes(const Value: TfcxSourceFields);
begin
  FAttributes.Assign(Value);
end;

procedure TfcxSplitProperty.SetDateSplitPaths(const Value: TfcxDateTypes);
begin
  FDateSplitPaths := Value;
  if odt_None in FDateSplitPaths then
    FDateSplitPaths := FDateSplitPaths - [odt_None];
end;

procedure TfcxSplitProperty.SetTimeSplitPaths(const Value: TfcxTimeTypes);
begin
  FTimeSplitPaths := Value;
  if ott_None in FTimeSplitPaths then
    FTimeSplitPaths := FTimeSplitPaths - [ott_None];
end;

function TfcxSplitProperty.Test(var AErrorMessage: String): boolean;
begin
  Result := True;
(*
  if UseReferenceSplit then
    Result := TfcxReferenceSplit(ReferenceSplit).Test(AMessage);
*)
{TODO -Тестировать атрибуты}
end;

{ TfcxDataSet }

function TfcxDataSet.FieldCount: integer;
begin
  Result := 0;
end;

function TfcxDataSet.GetVersion: String;
begin
  Result := FCX_VERSION
end;

procedure TfcxDataSet.SetVersion(const Value: String);
begin
//
end;

{ TfcxDataField }

procedure TfcxDataField.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxDataField then
    with TfcxDataField(Dest) do
    begin
      CubeFieldName := Self.CubeFieldName;
      CubeFieldDisplayLabel := Self.CubeFieldDisplayLabel;
      CubeFieldSize := Self.CubeFieldSize;
      CubeFieldType := Self.CubeFieldType;
      NullStr := Self.NullStr;
      CaseSensitive := Self.CaseSensitive;
    end;
  inherited AssignTo(Dest);
end;

constructor TfcxDataField.Create(AGetDataSet: TfcxGetDataSet);
begin
  FGetDataSet := AGetDataSet;
  FCubeFieldDisplayLabel := '';
  FCubeFieldName := '';
  FCubeFieldSize := 0;
  FCubeFieldType := fcdt_NotImplemented;
  FNullStr := '';
  FCaseSensitive := cs_Default;
  FConvert := True;
  FInitialized := True;
end;

destructor TfcxDataField.Destroy;
begin
  inherited;
end;

function TfcxDataField.GetCubeFieldDisplayLabel: TfcxString;
begin
  if FCubeFieldDisplayLabel <> '' then
    Result := FCubeFieldDisplayLabel
  else
    Result := FCubeFieldName;
end;

function TfcxDataField.GetDataNil(var Buffer: Pointer): Boolean;
begin
  Buffer := nil;
  Result := True;
end;

procedure TfcxDataField.SetCaseSensitive(const Value: TfcxCaseSensitive);
begin
  FCaseSensitive := Value;
end;

procedure TfcxDataField.SetCubeFieldDisplayLabel(const Value: TfcxString);
begin
  if Value = FCubeFieldName then
    FCubeFieldDisplayLabel := ''
  else
    FCubeFieldDisplayLabel := Value;
end;

procedure TfcxDataField.SetCubeFieldName(const Value: TfcxString);
begin
  FCubeFieldName := Value;
end;

procedure TfcxDataField.SetCubeFieldSize(const Value: integer);
begin
  if FConvert then
    FCubeFieldSize := Value;
end;

procedure TfcxDataField.SetCubeFieldType(const Value: TfcxDataType);
begin
  if FConvert then
    FCubeFieldType := Value;
end;

procedure TfcxDataField.SetNullStr(const Value: TfcxString);
begin
  FNullStr := Value;
end;

{ TfcxReferenceDataField }

procedure TfcxReferenceDataField.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxReferenceDataField then
    with TfcxReferenceDataField(Dest) do
    begin
//      FGetDataSet := Self.FGetDataSet;
      Convert := Self.Convert;
      DataFieldName := Self.DataFieldName;
    end;
  inherited AssignTo(Dest);
end;

constructor TfcxReferenceDataField.Create(AGetDataSet: TfcxGetDataSet);
begin
  inherited Create(AGetDataSet);
  FInitialized := False;
  FTestOk := False;
// DB
  FDBField := nil;
  FUseNativeFormat := True;
  FDataFieldName := '';
  FConvert := False;
  Init;
end;

destructor TfcxReferenceDataField.Destroy;
begin
  inherited;
end;

function TfcxReferenceDataField.GetConvert: boolean;
begin
  Result := FConvert
end;

function TfcxReferenceDataField.GetDataDB(var Buffer: Pointer): Boolean;
{$IFDEF WideStringFieldIsPWideString}
var
  ASize: integer;
  WS: WideString;
{$ENDIF}
begin
{$IFDEF WideStringFieldIsPWideString}
  if FDBField.DataType = ftWideString then
  begin
    Result := FDBField.GetData(@WS, FUseNativeFormat);
    if Result then
    begin
      ASize := (Length(WS) + 1) * SizeOf(WideChar);
      GetMem(Buffer, ASize);
      Move(PWideChar(WS)^, Buffer^, ASize);
    end
    else
      Buffer := nil;
  end
  else
{$ENDIF}
  begin
{$IFDEF Delphi_17UP}
    if dsMaxStringSize >= FDBField.DataSize then
    begin
      Result := FDBField.GetData(FVBuffer, FUseNativeFormat);
      if Result then
      begin
        GetMem(Buffer, FDBField.DataSize);
        Move(FVBuffer[0], Buffer^, FDBField.DataSize);
      end;
    end
    else
      Result := False;
{$ELSE}
    if FDBField.ClassNameIs('TIBBCDField') then
      GetMem(Buffer, 34)
    else
      GetMem(Buffer, FDBField.DataSize);
    Result := FDBField.GetData(Buffer, FUseNativeFormat);
{$ENDIF}
    if not Result then
    begin
      FreeMem(Buffer);
      Buffer := nil;
    end
    else
      cfcProcessorMap[FDataFieldType].CorrectBuffer(Buffer);
  end
end;

function TfcxReferenceDataField.GetDataDBValue(var Buffer: Pointer): Boolean;
begin
  Result := not FDBField.IsNull;
  if Result then
    Buffer := cfcProcessorMap[FDataFieldType].ToPointer(FDBField.Value)
  else
    Buffer := nil
end;

function TfcxReferenceDataField.GetDataEvents(var Buffer: Pointer): Boolean;
var
  AVarValue: Variant;
begin
  Result := TfcxUserDataSet(DataSet).GetVarData(FDataFieldIndex, AVarValue);
  if Result then
  begin
    Buffer := cfcProcessorMap[FCubeFieldType].ToPointer(AVarValue);
    Result := Assigned(Buffer);
  end
  else
    Buffer := nil;
end;

function TfcxReferenceDataField.GetDataSet: TfcxDataSet;
begin
  Result := FGetDataSet;
end;

function TfcxReferenceDataField.GetDBDataSet: TDataSet;
var
  ADataSet: TfcxDataSet;
begin
  ADataSet := DataSet;
  if (ADataSet <> nil) and (ADataSet is TfcxDBDataSet) then
    Result := TfcxDBDataSet(ADataSet).DataSet
  else
    Result := nil;
end;

function TfcxReferenceDataField.Init: boolean;
var
  ADBDataSet: TDataSet;
  ADataFieldPropery: TfcxDataFieldPropery;
begin
  Result := (FDataFieldName <> '');
  if Result then
  begin
    if (DataSet <> nil) then
    begin
      if (DataSet is TfcxDBDataSet) then
      begin
        ADBDataSet := DBDataSet;
        if (ADBDataSet <> nil) then
        begin
{$IFDEF Delphi_17UP}
          SetLength(FVBuffer, dsMaxStringSize);
{$ENDIF}
          FDBField := ADBDataSet.FindField(FDataFieldName);
          if FDBField <> nil then
          begin
            FDataFieldDisplayLabel := FDBField.DisplayLabel;
            if (FCubeFieldName = FDataFieldName) and ((FCubeFieldDisplayLabel = '') or (FCubeFieldName = FCubeFieldDisplayLabel)) then
              FCubeFieldDisplayLabel := FDataFieldDisplayLabel;
            FDataFieldSize := FDBField.Size;
            FDataFieldIndex := FDBField.Index;
            FDataFieldType := cfcFieldTypeToDataType[FDBField.DataType];
            FUseNativeFormat := not (FDBField is TWideStringField);
            if not FConvert then
            begin
              FCubeFieldType := FDataFieldType;
              FCubeFieldSize := FDataFieldSize;
            end;
            case fcFieldClassCategory(FDBField, TfcxDBDataSet(DataSet).FUseGetDataInInheritedClasses) of
              fcfcc_GetData:
                begin
                  Result := True;
                  GetData := GetDataDB;
                end;
              fcfcc_Value:
                begin
                  Result := True;
                  GetData := GetDataDBValue;
                end;
            else
              Result := False;
            end;
          end
          else
            Result := False;
        end
        else
          Result := False;
      end
      else
      if (DataSet is TfcxUserDataSet) then
      begin
        ADataFieldPropery := DataSet.DataFieldByName(FDataFieldName);
        FDBField := nil;
        if ADataFieldPropery.Index >= 0 then
        begin
          FDataFieldDisplayLabel := ADataFieldPropery.DisplayLabel;
          if (FCubeFieldName = FDataFieldName) and ((FCubeFieldDisplayLabel = '') or (FCubeFieldName = FCubeFieldDisplayLabel)) then
            FCubeFieldDisplayLabel := FDataFieldDisplayLabel;
          FDataFieldSize := ADataFieldPropery.Size;
          FDataFieldIndex := ADataFieldPropery.Index;
          FDataFieldType := cfcFieldTypeToDataType[ADataFieldPropery.FieldType];
          FUseNativeFormat := True;
          if not FConvert then
          begin
            FCubeFieldType := FDataFieldType;
            FCubeFieldSize := FDataFieldSize;
          end;
          Result := True;
          GetData := GetDataEvents;
        end
        else
          Result := False;
      end
      else
        Result := False;
    end
    else
      Result := False;
  end
  else
    Result := False;
  FInitialized := Result;
  if not Result then
  begin
    FDBField := nil;
    FDataFieldDisplayLabel := '';
    FDataFieldSize := 0;
    FDataFieldIndex := -1;
    FDataFieldType := fcdt_NotImplemented;
    FUseNativeFormat := True;
    GetData := GetDataNil;
  end;
end;

procedure TfcxReferenceDataField.SetConvert(const Value: boolean);
begin
  FConvert := Value;
  if not FConvert then
    Init;
end;

procedure TfcxReferenceDataField.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
    FCubeFieldName := Value;
  FDataFieldName := Value;
  if Init then
    CubeFieldDisplayLabel := FDataFieldDisplayLabel
  else
    CubeFieldDisplayLabel := FDataFieldName;
end;

function TfcxReferenceDataField.Test(APrefix: String; var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean;
begin
  FInitialized := False;
  Result := True;
  if APrefix <> '' then
    APrefix := APrefix + ' ';
  if FDataFieldName = '' then
  begin
    AErrorMessage := APrefix + 'DataFieldName is empty;'+#13#10;
    Result := False;
  end
  else
    AErrorMessage := '';
  if FCubeFieldName = '' then
  begin
    AErrorMessage := AErrorMessage + APrefix + 'CubeFieldName is empty;'+#13#10;
    Result := False;
  end;
  if not DataSet.FieldExists(FDataFieldName) then
  begin
    AErrorMessage := AErrorMessage + APrefix + 'Field ' + FDataFieldName + ' not exists in ' + APrefix + 'DataSet ' + DataSet.Name + ';'+#13#10;
    Result := False;
  end;
  FTestOk := Result;
  if ASkipFieldsWithError then
  begin
    AErrorMessage := '';
    Result := True;
  end;
end;

{ TfcxUserDataSet }

function TfcxUserDataSet.Active: boolean;
begin
  if Assigned(FOnActive) then
    Result := FOnActive(Self)
  else
    Result := False;
end;

function TfcxUserDataSet.AssignedDataSet: boolean;
begin
  Result :=
    (FFields.Count > 0) and
    Assigned(FOnEof) and
    Assigned(FOnActive) and
    Assigned(FOnDataFieldAt) and
    Assigned(FOnClose) and
    Assigned(FOnFirst) and
    Assigned(FOnNext) and
    Assigned(FOnOpen) and
    Assigned(FOnGetVarData);
end;

procedure TfcxUserDataSet.Close;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

constructor TfcxUserDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FFields := TStringList.Create;
end;

function TfcxUserDataSet.DataFieldAt(
  AFieldIndex: integer): TfcxDataFieldPropery;
begin
  Result.Index := AFieldIndex;
  Result.DisplayLabel := '';
  Result.FieldType := ftUnknown;
  Result.FieldName := '';
  Result.Size := 0;
  Result.Visible := False;
  if Assigned(FOnDataFieldAt) then
    FOnDataFieldAt(Self, AFieldIndex, Result);
end;

function TfcxUserDataSet.DataFieldByName(
  AFieldName: string): TfcxDataFieldPropery;
begin
  Result.Index := FFields.IndexOf(AFieldName);
  Result.DisplayLabel := AFieldName;
  Result.FieldType := ftUnknown;
  Result.FieldName := AFieldName;
  Result.Size := 0;
  Result.Visible := False;
  if (Result.Index >= 0) and Assigned(FOnDataFieldAt) then
    FOnDataFieldAt(Self, Result.Index, Result);
end;

function TfcxUserDataSet.DataFieldClassCategoryAt(
  AFieldIndex: integer): TfcxFieldClassCategory;
begin
  if fcFieldTypeRealized(DataFieldAt(AFieldIndex).FieldType) then
    Result := fcfcc_GetData //??
  else
    Result := fcfcc_Error;
end;

function TfcxUserDataSet.DataFieldClassCategoryByName(
  AFieldName: string): TfcxFieldClassCategory;
begin
  if fcFieldTypeRealized(DataFieldByName(AFieldName).FieldType) then
    Result := fcfcc_GetData //??
  else
    Result := fcfcc_Error;
end;

destructor TfcxUserDataSet.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TfcxUserDataSet.Eof: boolean;
begin
  if Assigned(FOnEof) then
    Result := FOnEof(Self)
  else
    Result := True;
end;

function TfcxUserDataSet.FieldCount: integer;
begin
  Result := FFields.Count
end;

function TfcxUserDataSet.FieldExists(ADataFieldName: string): boolean;
begin
  Result := (FFields.IndexOf(ADataFieldName) >= 0);
end;

procedure TfcxUserDataSet.First;
begin
  if Assigned(FOnFirst) then
    FOnFirst(Self);
end;

procedure TfcxUserDataSet.GetFieldNames(List: TStrings);
begin
  List.Clear;
  List.AddStrings(FFields);
end;

function TfcxUserDataSet.GetVarData(AFieldIndex: Integer;
  out AValue: Variant): Boolean;
begin
  if Assigned(FOnGetVarData) then
    Result := FOnGetVarData(Self, AFieldIndex, AValue)
  else
  begin
    AValue := varNull;
    Result := False;
  end
end;

procedure TfcxUserDataSet.Next;
begin
  if Assigned(FOnNext) then
    FOnNext(Self);
end;

procedure TfcxUserDataSet.Open;
begin
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end;

procedure TfcxUserDataSet.SetFields(const Value: TStrings);
begin
  FFields.Assign(Value);
end;

{$IFNDEF DELPHI_6UP}
{ TfcxCollection }
function TfcxCollection.Owner: TPersistent;
begin
  Result := GetOwner;
end;
{$ENDIF}

{ TfcxSourceFieldProperties }

constructor TfcxSourceFieldProperties.Create(
  ASourceField: TfcxSourceField);
begin
  FSourceField := ASourceField;
  FSplitProperty := TfcxSplitProperty.Create(Self.FSourceField);
  FCaptionSourceAttribute := '';
  FOrderSourceAttribute := '';
  FSaved := True;
  FWithCustomCaption := False;
end;

procedure TfcxSourceFieldProperties.SetWithCustomCaption(const Value: Boolean);
begin
  FWithCustomCaption := Value;
end;

procedure TfcxSourceFieldProperties.SetSplitProperty(const Value: TfcxSplitProperty);
begin
  FSplitProperty.Assign(Value);
end;

procedure TfcxSourceFieldProperties.SetCaptionSourceAttribute(
  const Value: TfcxString);
begin
  FCaptionSourceAttribute := Value;
end;

procedure TfcxSourceFieldProperties.SetOrderSourceAttribute(const Value: TfcxString);
begin
  FOrderSourceAttribute := Value;
end;

function TfcxSourceFieldProperties.Test(var AErrorMessage: String; ASkipFieldsWithError: boolean): boolean;
begin
  Result := SplitProperty.Test(AErrorMessage);
end;

destructor TfcxSourceFieldProperties.Destroy;
begin
  FreeAndNil(FSplitProperty);
  inherited;
end;

procedure TfcxSourceFieldProperties.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TfcxSourceFieldProperties then
    with TfcxSourceFieldProperties(Dest) do
    begin
      SplitProperty.Assign(Self.SplitProperty);
      WithCustomCaption := Self.WithCustomCaption;
    end;
end;

procedure TfcxSourceFieldProperties.InitField;
begin
  SplitProperty.Attributes.InitField;
end;

function TfcxSourceFieldProperties.GetParentComponent: TComponent;
begin
  Result := FSourceField.ParentComponent;
end;

procedure TfcxSourceFieldProperties.SetSaved(const Value: Boolean);
begin
  FSaved := Value;
end;

{ TfcxCustomSourceFieldProperties }

procedure TfcxCustomSourceFieldProperties.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TfcxCustomSourceFieldProperties then
    with TfcxCustomSourceFieldProperties(Dest) do
    begin
      DataField.Assign(Self.DataField);
      Saved := Self.Saved;
    end;
end;

constructor TfcxCustomSourceFieldProperties.Create(
  ASourceField: TfcxSourceField);
begin
  inherited;
  FDataField := TfcxMainCustomDataField.Create(nil);
  FDataField.FDataSource := TfcxDataSource(FSourceField.Collection.Owner);
  FSaved := False;
  FCalculateAfterAll := False;
end;

destructor TfcxCustomSourceFieldProperties.Destroy;
begin
  FDataField.Free;
  inherited;
end;

function TfcxCustomSourceFieldProperties.GetSourceFieldName: TfcxString;
begin
  Result := DataField.CubeFieldName;
end;

procedure TfcxCustomSourceFieldProperties.InitField;
begin
  if DataField.Init then
  begin
    inherited;
  end;
// возможно и др свойства
end;

{ TfcxReferenceAttributeSFProperties }

procedure TfcxReferenceAttributeSFProperties.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxReferenceAttributeSFProperties then
    with TfcxReferenceAttributeSFProperties(Dest) do
    begin
      DataSet := Self.DataSet;
      IdField.Assign(Self.IdField);
    end;
  inherited;
end;

constructor TfcxReferenceAttributeSFProperties.Create(
  ASourceField: TfcxSourceField);
begin
  inherited;
  FIdField := TfcxAddonReferenceDataField.Create(GetDataSet);
end;

destructor TfcxReferenceAttributeSFProperties.Destroy;
begin
  FreeAndNil(FIDField);
  inherited;
end;

function TfcxReferenceAttributeSFProperties.GetDataSet: TfcxDataSet;
begin
  Result := FDataSet
end;

procedure TfcxReferenceAttributeSFProperties.InitField;
begin
  if IdField.Init or (DataSet = TfcxSourceFields(FSourceField.Collection).DataSet) then
  begin
    inherited;
  end;
end;

procedure TfcxReferenceAttributeSFProperties.SetDataSet(
  const Value: TfcxDataSet);
begin
  if FDataSet <> Value then
    if Value <> nil then
      Value.FreeNotification(ParentComponent);
  FDataSet := Value;
end;

procedure TfcxReferenceAttributeSFProperties.SetSaved(
  const Value: Boolean);
begin
  FSaved := True;
end;

function TfcxReferenceAttributeSFProperties.Test(var AErrorMessage: TfcxString; ASkipFieldsWithError: boolean): boolean;
begin
  Result := True;
end;

{ TfcxMainDateDataField }

constructor TfcxMainDateDataField.Create(AGetDataSet: TfcxGetDataSet);
begin
  inherited;
  FCubeFieldType := fcdt_Date;
end;

{ TfcxMainTimeDataField }

constructor TfcxMainTimeDataField.Create(AGetDataSet: TfcxGetDataSet);
begin
  inherited;
  FCubeFieldType := fcdt_Time;
end;

{ TfcxDateSourceFieldProperties }

procedure TfcxDateSourceFieldProperties.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TfcxDateSourceFieldProperties then
    with TfcxDateSourceFieldProperties(Dest) do
    begin
      DataField.Assign(Self.DataField);
    end;
end;

constructor TfcxDateSourceFieldProperties.Create(
  ASourceField: TfcxSourceField);
begin
  inherited;
  FDataField := TfcxMainDateDataField.Create(nil);
  FSaved := False;
end;

destructor TfcxDateSourceFieldProperties.Destroy;
begin
  FDataField.Free;
  inherited;
end;

function TfcxDateSourceFieldProperties.GetSourceFieldName: TfcxString;
begin
  Result := DataField.CubeFieldName;
end;

{ TfcxTimeSourceFieldProperties }

procedure TfcxTimeSourceFieldProperties.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TfcxTimeSourceFieldProperties then
    with TfcxTimeSourceFieldProperties(Dest) do
    begin
      DataField.Assign(Self.DataField);
    end;
end;

constructor TfcxTimeSourceFieldProperties.Create(
  ASourceField: TfcxSourceField);
begin
  inherited;
  FDataField := TfcxMainTimeDataField.Create(nil);
  FSaved := False;
end;

destructor TfcxTimeSourceFieldProperties.Destroy;
begin
  FDataField.Free;
  inherited;
end;

function TfcxTimeSourceFieldProperties.GetSourceFieldName: TfcxString;
begin
  Result := DataField.CubeFieldName;
end;

{ TfcxMainReferenceDataField }

constructor TfcxMainReferenceDataField.Create(AGetDataSet: TfcxGetDataSet);
begin
  inherited;
  FLoadAllValues := False;
end;

{ TfcxMainCustomDataField }

function TfcxMainCustomDataField.GetDataEvents(
  var Buffer: Pointer): Boolean;
var
  AVarValue: Variant;
begin
  Result := FDataSource.GetCustomValue(FCubeFieldName, AVarValue);
  if Result then
    Buffer := cfcProcessorMap[FCubeFieldType].ToPointer(AVarValue)
  else
    Buffer := nil;
end;

function TfcxMainCustomDataField.Init: boolean;
begin
  Result := True;
  GetData := GetDataEvents;
end;

initialization
{
Проверки, которые надо сделать:
-Датасет атрибута не должен совпадать с датасетом предка большего, чем непосредственный родитель.
}
end.
