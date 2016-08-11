{*******************************************************}
{                                                       }
{                FastCube 2 Cube unit                   }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxCube;
{$INCLUDE fcx.inc}

interface
uses
  Classes, db, SysUtils, Math, 
  fcxRes, fcxDefaultSettings, fcxList, fcxTypes, fcxFormats, fcxUniqueArray,
  fcxUniqueValue, fcxDataSource, fcxComponent, fcxXML, fcxAlerts
{$IFDEF SQL_TYPES_EXTRA1}
  , SqlTimSt
  , FMTBcd
{$ENDIF}
{$IFDEF FPC}
  , LCLType, LCLIntf, zstream
{$ELSE}
  , zlib, Windows
{$ENDIF}
  ;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, Data.db, System.SysUtils, System.Math, 
  FMX.fcxRes, FMX.fcxDefaultSettings, FMX.fcxList, FMX.fcxTypes, 
  FMX.fcxFormats, FMX.fcxUniqueArray, FMX.fcxUniqueValue, 
  FMX.fcxDataSource, FMX.fcxComponent, FMX.fcxXML, FMX.fcxAlerts
{$IFDEF SQL_TYPES_EXTRA1}
  , Data.SqlTimSt
  , Data.FMTBcd
{$ENDIF}
  , System.zlib
  ;
{$ENDIF FMX}

type

{ TODO -cНеобходимо : Перегнать в fcTypes.}

  TfcxCubeTimeStat = class
    FullTime: cardinal;
    DBOpenTime: cardinal;
    DBCloseTime: cardinal;
    OpenTime: cardinal;
    DBMoveTime: cardinal;
    DBGetDataTime: cardinal;
//    DBOtherTime: cardinal;
    ConvertTime: cardinal;
    SortTime: cardinal;
//    UVsTime: cardinal;
  public
    procedure Clear;
  end;

  TfcxCubeSource =
  (
    fccs_None,            // None
    fccs_DataSource,      // load from fcxDataSource
    fccs_CubeFile,        // load from file
    fccs_CubeStream       // load from Stream
  );

  TfcxCubeState =
  (
    fcst_NotActive,      // Cube closed (no data)
//    fcst_FieldsCreated,
    fcst_LoadingData,    // Loading data from source
    fcst_AppendingData,  // Appending data from source
    fcst_Active          // Cube opened (with data)
  );

// Type of cube field
  TfcxCFieldType =
  (
    fcft_Simple,        // simple cube field
    fcft_Event,         // calc by event
    fcft_Script,        // calc by script
    fcft_Expression     // calc by Expression
  );

// old format field properties
  TfcxOldSavedCubeField = record
    FFieldName: String;
    FCaption: String;
    FPrecision: SmallInt;
    FDataType: TFieldType;
    FDateType: TfcxDateType;
    FTimeType: TfcxTimeType;
    FDataSize: Integer;
    FDecSeparator: String;
    FFormatStr: String;
    FFormatKind: TfcxFormatKind;
    FIndexInDataset: Integer;
    FUseDefaultFormat: Boolean;
    FUseFormatStr: Boolean;
  end;

  TfcxOldSavedCubeFieldPropeties = record
    VarType: integer;
    DateType: TfcxDateType;
    TimeType: TfcxTimeType;
  end;

  TfcxCFieldProperties = record
    FieldProperties: TfcxFieldProperties;
    CFieldType: TfcxCFieldType;
    CalcString: TfcxString;
  end;

  TfcxCube = class;
  TfcxCommonField = class;
  TfcxCommonUVField = class;

  TfcxSourceRecord = class
  private
    FCube: TfcxCube;
    FRecNo: integer;
    FFieldCount: integer;
    function GetIndexOf(AFieldName: TfcxString): Integer;
    function GetValue(AFieldName: TfcxString): Variant;
    function GetValueAt(AFieldIndex: integer): Variant;
  public
    constructor Create(ACube: TfcxCube; AFieldCount: integer);
    property Value[AFieldName: TfcxString]: Variant read GetValue;
    property ValueAt[AFieldIndex: integer]: Variant read GetValueAt;
    property IndexOf[AFieldName: TfcxString]: Integer read GetIndexOf;
    property FieldCount: integer read FFieldCount;
    property RecNo: integer read FRecNo;
  end;

  TfcxSlicesManagerOnNeedSlice = procedure(Sender: TObject; AIndex: integer) of object;
  TfcxSlicesManager = class // срезы
  private
    FCube: TfcxCube;
    FOnNeedSlice: TfcxSlicesManagerOnNeedSlice;
    FStreamFromCube: TStream;
    FMajorVersion, FMinorVersion: integer;
    FSM, FSC, FSR: Integer;
    FOldVersion: Boolean;
    function GetSlice(AIndex: Integer): TfcxAbstractSlice;
    function GetCount: Integer;
    procedure LoadFromStream; overload;
    procedure SaveToStream(AStream: TStream; AStoreItems: TfcxItemsForStoreWithSlice = []);
//    function GetExistSlice(AIndex: Integer): TfcxAbstractSlice;
//    function GetPersistentCount: Integer;
  public
    constructor Create(ACube: TfcxCube);
    destructor Destroy; override;
    procedure NeedSlice(AIndex: Integer);
    property Count: Integer read GetCount;
    property Slice[AIndex: Integer]: TfcxAbstractSlice read GetSlice; default;
    property OnNeedSlice: TfcxSlicesManagerOnNeedSlice read FOnNeedSlice write FOnNeedSlice;
//    destructor Destroy; override;
//    procedure Creating;
//    procedure DestroyingSlice;
//    procedure Recreate;
//    procedure Build; // build cube
//    property PersistentCount: Integer read GetPersistentCount;
  end;

  TfcxMasterFields = class;
  TfcxSourceHolder = class;

  TfcxGetCalcValue = function(Sender: TfcxCube; AFieldName: TfcxString; ASourceRecord: TfcxSourceRecord; var AValue: variant): boolean of object;
  TfcxProgressEvent = procedure(Sender: TfcxCube; ProgressType: TfcxProgressType; Progress: Integer) of object;

  {$IFDEF DELPHI_16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF}
  TfcxCube = class(TfcxComponent)
  private
    FDataSource: TfcxDataSource;
    FFields: TfcxMasterFields;
    FSourceHolder: TfcxSourceHolder;
    FSlicesManager: TfcxSlicesManager;
    FSaveFilter: TObject;

    FTimeStat: TfcxCubeTimeStat;
    FTimeStatStart: TfcxCubeTimeStat;
    FCubeSource: TfcxCubeSource;
    FCubeFile: string;
    FCubeStream: TStream;
    FCubeState: TfcxCubeState;
    FDescription: TfcxString;
    FCaption: TfcxString;
    FLoadActive: Boolean;
    FAfterClose: TNotifyEvent;
    FBeforeOpen: TNotifyEvent;
    FBeforeClose: TNotifyEvent;
    FAfterOpen: TNotifyEvent;
    FOnGetCalcValue: TfcxGetCalcValue;
    FOnGetCustomAttributeValue: TfcxGetCustomAttributeValue;
    FSkipFieldsWithError: boolean;
    FCubeLock: Boolean;
    FDayOfWeekISO8601: Boolean;
    FWeekNumberISO8601: Boolean;

// Set of changes in Cube
    FChanges: TfcxChangesInCube;
    FFormat: TfcxDefaultFormat;

    FCommonScript: TfcxScriptStringList;
    FCubeFileCompressionLevel: TCompressionLevel;
    FOnProgress: TfcxProgressEvent;
    FOnProgressStart: TfcxProgressEvent;
    FOnProgressStop: TfcxProgressEvent;

    procedure SetOnGetCustomAttributeValue(
      const Value: TfcxGetCustomAttributeValue);
    procedure SetCubeFile(const Value: string);
    procedure SetDataSource(const Value: TfcxDataSource);
    procedure SetCaption(const Value: TfcxString);
    procedure SetDescription(const Value: TfcxString);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetOnGetCalcValue(const Value: TfcxGetCalcValue);
    function OpenSource: boolean;
    procedure CloseSource;
    procedure CreateFields;
    function LinkFields: Boolean;
    procedure LoadData;
    procedure LoadAppendingData;
    procedure InternalAppendDataFromDataSource;
    procedure InternalAppendCubeFromFile(AFileName: String);
    procedure InternalAppendCubeFromStream(ACubeStream: TStream);
    procedure InternalAppendCubeFromDeCompStream(ACubeStream: TStream);
    procedure InternalLoadFromDataSource;
    procedure InternalLoadCubeFromFile(AFileName: String);
    procedure InternalLoadCubeFromStream(ACubeStream: TStream);
    procedure InternalLoadCubeFromDeCompStream(ACubeStream: TStream);
    procedure InternalSaveToStream(ACubeStream: TStream; AFilter: TObject = nil);
    procedure SaveGroupsForNotSavedFieldsToStream(ACubeStream: TStream);
    procedure LoadGroupsForNotSavedFieldsFromStream(ACubeStream: TStream);
    procedure SetCubeLock(const Value: Boolean);
    function GetCubeStream: TStream;

    procedure SetFormat(const Value: TfcxDefaultFormat);
  protected
    procedure StopChange(AChanges: TfcxChangesInCube = []; AIndex: integer = -1; AMasterField: TfcxCommonUVField = nil; ASplitFieldIndex: integer = -1);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCustomAttributeValue(AMasterFieldName, AAttributeName: TfcxString; AMasterValue: variant; var AAttributeValue: variant): boolean;
    procedure DoBeforeOpen;
    procedure DoAfterOpen;
    procedure DoBeforeClose;
    procedure DoAfterClose;
  public
    procedure SetDefaults; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCommonScript: TStrings; override;
    function GetCommonScriptChangedTick: Cardinal; override;

    procedure Open;
    procedure AppendData;
    procedure Close;
    procedure Refresh(SaveFilters: Boolean = False);
    procedure ClearGroups;

    // progress
    procedure InternalOnProgressStart(ProgressType: TfcxProgressType);
    procedure InternalOnProgress(ProgressType: TfcxProgressType; Progress: Integer);
    procedure InternalOnProgressStop(ProgressType: TfcxProgressType);


    procedure SaveGroupsToXML(AXMLDoc: TfcxXMLDocument);
    procedure SaveGroupsToXMLItem(AItem: TfcxXMLItem; AOnlyForNotSavedFields: boolean = False);
    function LoadGroupsFromXML(AXMLDoc: TfcxXMLDocument): Boolean;
    function LoadGroupsFromXMLItem(AItem: TfcxXMLItem; AClearGroups: boolean=True): Boolean;
    procedure SaveGroupsToFile(AGroupsFileName: String);
    function LoadGroupsFromFile(AGroupsFileName: String): Boolean;
    procedure SaveGroupsToStream(AStream: TStream);
    function LoadGroupsFromStream(AStream: TStream): Boolean;

    procedure SaveToStream(ACubeStream: TStream; ACompressionLevel: TCompressionLevel = clMax; AFilter: TObject = nil);
    function LoadFromStream(ACubeStream: TStream): Boolean;
    function AppendFromStream(ACubeStream: TStream): Boolean;

    procedure SaveToFile(ACubeFileName: String; AFilter: TObject = nil);
    function LoadFromFile(ACubeFileName: String): Boolean;
    function AppendFromFile(ACubeFileName: String): Boolean;

    procedure AddCFields(ACFieldsProperties: Array of TfcxCFieldProperties);
    procedure DeleteField(AFieldName: TfcxString);
    procedure DeleteFieldAt(AFieldIndex: Integer);

// For using in Active cube
// Date Path
    procedure AddDatePath(AField: TfcxCommonField; ADatePath: TfcxDateType);
    procedure SetDatePaths(AFieldIndex: integer; ADatePaths: TfcxDateTypes);
    procedure RemoveDatePath(AField: TfcxCommonField; ADatePath: TfcxDateType);
// Time Path
    procedure AddTimePath(AField: TfcxCommonField; ATimePath: TfcxTimeType);
    procedure SetTimePaths(AFieldIndex: integer; ATimePaths: TfcxTimeTypes);
    procedure RemoveTimePath(AField: TfcxCommonField; ATimePath: TfcxTimeType);
// Custom Attribute
    procedure AddAttribute(AField: TfcxCommonField; AFieldProperties: TfcxFieldProperties);
    procedure RemoveAttribute(AField: TfcxCommonField; ACustomPathName: TfcxString); overload;
    procedure RemoveAttribute(AField: TfcxCommonField; AAttributeIndex: integer); overload;

    function DateTimeConsts: TfcxDateTimeConsts;

    // fields of cube
    property Fields: TfcxMasterFields read FFields;
    // holder of source records
    property SourceHolder: TfcxSourceHolder read FSourceHolder;
    // source for fccs_CubeStream (load from Stream)
    property CubeStream: TStream read GetCubeStream;

    // for internal use only - dont change anything
    // time metrics
    property TimeStat: TfcxCubeTimeStat read FTimeStat write FTimeStat;
    property TimeStatStart: TfcxCubeTimeStat read FTimeStatStart write FTimeStatStart;

    // cube state
    property CubeState: TfcxCubeState read FCubeState;
    // cube lock
    property CubeLock: Boolean read FCubeLock write SetCubeLock;
    property SlicesManager: TfcxSlicesManager read FSlicesManager;
  published
    property Active: Boolean read GetActive write SetActive default False;
    // source for fccs_DataSource (load from TfcxDataSet)
    property DataSource: TfcxDataSource read FDataSource write SetDataSource;
    // source for fccs_CubeFile (load from file)
    property CubeFile: string read FCubeFile write SetCubeFile;
    // Type of data source
    property CubeSource: TfcxCubeSource read FCubeSource write FCubeSource default fccs_None;
    // caption of cube - shown in grid
    property Caption: TfcxString read FCaption write SetCaption;
    // description of cube - any string
    property Description: TfcxString read FDescription write SetDescription;
    // To ignore fields which have not passed the test
    property SkipFieldsWithError: boolean read FSkipFieldsWithError write FSkipFieldsWithError default False;
    property CubeFileCompressionLevel: TCompressionLevel read FCubeFileCompressionLevel write FCubeFileCompressionLevel default clMax;

    property DayOfWeekISO8601: Boolean read FDayOfWeekISO8601 write FDayOfWeekISO8601 default True;
    property WeekNumberISO8601: Boolean read FWeekNumberISO8601 write FWeekNumberISO8601 default True;

// Default Format for data types
    property Formats: TfcxDefaultFormat read FFormat write SetFormat;

// events
    property OnGetCustomAttributeValue: TfcxGetCustomAttributeValue read FOnGetCustomAttributeValue write SetOnGetCustomAttributeValue;
    property OnGetCalcValue: TfcxGetCalcValue read FOnGetCalcValue write SetOnGetCalcValue;
    property OnProgressStart: TfcxProgressEvent read FOnProgressStart write FOnProgressStart;
    property OnProgress: TfcxProgressEvent read FOnProgress write FOnProgress;
    property OnProgressStop: TfcxProgressEvent read FOnProgressStop write FOnProgressStop;

    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TNotifyEvent read FAfterClose write FAfterClose;
  end;

  TfcxCubeFieldInIndex = record
    CubeField: TfcxCommonField;
    SortDirection: TfcxSortDirection;
  end;
  PfcxCubeFieldInIndex = ^TfcxCubeFieldInIndex;

  TfcxCubeFieldsInIndex = class(TObject)
  private
    FFields: array of TfcxCubeFieldInIndex;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetFieldItem(AIndex: TfcxCommonField): PfcxCubeFieldInIndex;
    function GetItem(AIndex: Integer): PfcxCubeFieldInIndex;
    function GetFieldItemIndex(AIndex: TfcxCommonField): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(ASource: TfcxCubeFieldsInIndex);

    property Items[AIndex: Integer]: PfcxCubeFieldInIndex read GetItem;
    property FieldItems[AIndex: TfcxCommonField]: PfcxCubeFieldInIndex read GetFieldItem;
    property FieldItemsIndex[AIndex: TfcxCommonField]: integer read GetFieldItemIndex;

    property Count: Integer read GetCount;
  end;

  TfcxCubeDataColumn = record
    Width: Integer;
    Field: TfcxCommonField;
    Visible: Boolean;
    SortIndex: Integer;
    SortDirection: TfcxSortDirection;
  end;
  PfcxCubeDataColumn = ^TfcxCubeDataColumn;

  TfcxCubeDataColumns = class(TObject)
  private
    FCols: array of TfcxCubeDataColumn;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    function GetVisibleCount: Integer;
    function GetVisItem(AIndex: Integer): PfcxCubeDataColumn;
    function GetFieldItem(AIndex: TfcxCommonField): PfcxCubeDataColumn;
    function GetAbsItem(AIndex: Integer): PfcxCubeDataColumn;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Update(Cube: TfcxCube);
    procedure Assign(ASource: TfcxCubeDataColumns);

    property AbsItems[AIndex: Integer]: PfcxCubeDataColumn read GetAbsItem;
    property VisItems[AIndex: Integer]: PfcxCubeDataColumn read GetVisItem; default;
    property FieldItems[AIndex: TfcxCommonField]: PfcxCubeDataColumn read GetFieldItem;

    property Count: Integer read GetCount;
    property VisibleCount: Integer read GetVisibleCount;
  end;

  TfcxGetRowIndex = function(ARow: Integer): Integer of object;

  TfcxOrderedRecordSetProvider = class
  private
    FRecordCount: integer;
    FCubeFieldsInIndex: TfcxCubeFieldsInIndex;
    FCubeDataColumns: TfcxCubeDataColumns;
    procedure SetCubeDataColumns(const Value: TfcxCubeDataColumns);
    procedure SetCubeFieldsInIndex(const Value: TfcxCubeFieldsInIndex);
  protected
    FRecordIndex: PfcxIntegerArray;
    function GetRowIndexDefault(ARow: Integer): Integer; virtual;
    function GetRowIndexOrder(ARow: Integer): Integer; virtual;
    function GetRecordCount: Integer; virtual;
    procedure FillRecordIndex; virtual;
    procedure Sort; virtual;
    function CompareUVIndexes(AIndex1, AIndex2: integer): integer; virtual;
    property CubeFieldsInIndex: TfcxCubeFieldsInIndex read FCubeFieldsInIndex write SetCubeFieldsInIndex;
    function GetText(ACol, ARowIndex: Integer): String; virtual;
  public
    GetRowIndex: TfcxGetRowIndex;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearIndex;
    procedure CreateIndex;
    procedure Update;
    function ValidProperties: Boolean; virtual;
    property Columns: TfcxCubeDataColumns read FCubeDataColumns write SetCubeDataColumns;
    property RecordCount: integer read FRecordCount;
    property Text[ACol, ARowIndex: Integer]: String read GetText;
  end;

  TfcxCubeOrderedRecordSetProvider = class(TfcxOrderedRecordSetProvider)
  private
    FCube: TfcxCube;
  protected
    function GetRecordCount: Integer; override;
    function CompareUVIndexes(AIndex1, AIndex2: integer): integer; override;
    function GetText(ACol, ARowIndex: Integer): String; override;
  public
    constructor Create(ACube: TfcxCube); reintroduce; virtual;
    function ValidProperties: Boolean; override;
    procedure SetCube(ACube: TfcxCube);
  end;

  TfcxSourceHolder = class
  private
    FBaseFieldsCount: Integer;
    FRecordsCount: Integer;
    FCapacity: Integer;
    FCube: TfcxCube;
    procedure SaveToStream(ACubeStream: TStream);
    procedure LoadFromStream(ACubeStream: TStream);
    procedure AppendFromStream(ACubeStream: TStream);
    function GetSourceRec(ARow: integer): PfcxCUVArray;
    function GetUniqueValueIndex(ARow: integer;
      AField: TfcxCommonField): integer;
    function GetUniqueValueCaption(ARow: integer;
      AField: TfcxCommonField): TfcxString;
    function GetUniqueValueAsVariant(ARow: integer;
      AField: TfcxCommonField): Variant;
  protected
    FSourceList: PfcxArrCUVArray;
    property SourceRec[ARow: integer]: PfcxCUVArray read GetSourceRec;
  public
    constructor Create(ACube: TfcxCube);
    destructor Destroy; override;
    function AddRecord: PfcxCUVArray;
    procedure AddRecords(ACount: integer);
    procedure AddField(AIndex: integer);
    procedure AddFields(ACount: integer);
    procedure DeleteField(AIndex: integer);
    procedure Recapacity;
    function GetUniqueValueIndexAndAsVariant(ARow: integer; AField: TfcxCommonField; var AVariantValue: Variant): Integer;
//    procedure SourceRecsCopy(Dest: Pointer);
// Count of records
    property RecordsCount: Integer read FRecordsCount;
    property BaseFieldsCount: Integer read FBaseFieldsCount;
    property UniqueValueIndex[ARow: integer; AField: TfcxCommonField]: Integer read GetUniqueValueIndex;
    property UniqueValueCaption[ARow: integer; AField: TfcxCommonField]: TfcxString read GetUniqueValueCaption;
    property UniqueValueAsVariant[ARow: integer; AField: TfcxCommonField]: Variant read GetUniqueValueAsVariant;
  end;

  TfcxFields = class(TfcxList)
  private
    FCube: TfcxCube;
    function GetItem(Index: Integer): TfcxCommonField; overload;
    function GetItemAtName(Index: TfcxString): TfcxCommonField; overload;
//    function AddDataSourceField(ASourceField: TfcxSourceField): Integer;
    function AddCField(ACFieldProperties: TfcxCFieldProperties): Integer;
    function CheckCField(ACFieldProperties: TfcxCFieldProperties; AFieldIndex: integer): Boolean;
    function AddStreamField(AFieldProperties: TfcxFieldProperties): Integer;
    function CheckStreamField(AFieldProperties: TfcxFieldProperties; AFieldIndex: integer): Boolean;
    procedure DeleteField(AFieldName: TfcxString); overload;
    procedure DeleteField(AFieldIndex: integer); overload;
    procedure SaveToXMLItem(AItem: TfcxXMLItem);
    procedure LoadFromXMLItem(AItem: TfcxXMLItem);
    function CheckFromXMLItem(AItem: TfcxXMLItem): Boolean;
    procedure LoadFieldFromXMLItem(AItem: TfcxXMLItem);
    procedure LoadFieldFromXMLItemStep2(AItem: TfcxXMLItem);
    function CheckFieldFromXMLItem(AItem: TfcxXMLItem; AFieldIndex: integer): Boolean;
    procedure SaveUVsToStream(ACubeStream: TStream);
    procedure LoadUVsFromStream(ACubeStream: TStream);
    procedure AppendUVsFromStream(ACubeStream: TStream);
  protected
  public
    function ExistsNotSaved(ANotCheckChildren: Boolean = False): Boolean;
    procedure ReNum;
    procedure DeleteLoaders;
//    function AddMeasuresField: Integer;
    constructor Create(ACube: TfcxCube); overload;
    function FieldIndex(AFieldName: TfcxString): Integer;
    property Items[Index: Integer]: TfcxCommonField read GetItem;
    property ItemsAtName[Index: TfcxString]: TfcxCommonField read GetItemAtName; default;
    property Cube: TfcxCube read FCube;
  end;

  TfcxMasterFields = class(TfcxFields)
  private
//    function GetItem(Index: Integer): TfcxCommonUVField;
    function AddDataSourceField(ASourceField: TfcxSourceField): Integer;
    function LinkDataSourceField(ASourceField: TfcxSourceField): Integer;
    procedure SaveToXMLDoc(XMLDoc: TfcxXMLDocument);
    procedure SaveFieldsToStream(AStream: TStream);
    procedure LoadFromXMLDoc(XMLDoc: TfcxXMLDocument);
    function CheckFromXMLDoc(XMLDoc: TfcxXMLDocument): boolean;
    procedure LoadFieldsFromStream(AStream: TStream);
    function CheckFieldsFromStream(AStream: TStream): boolean;
    procedure LoadFromOldStream(ACubeStream: TStream; AMajorVersion, AMinorVersion: integer);
    procedure LoadFieldFromOldStream(ACubeStream: TStream; AMajorVersion, AMinorVersion: integer);
  protected
  public
//    property Items[Index: Integer]: TfcxCommonUVField read GetItem; default;
    function BuildTree: TfcxTree;
  end;

  TfcxAttributeFields = class(TfcxFields)
  private
    FMasterField: TfcxCommonUVField;
  protected
  public
    function AddAttributeField(ASourceField: TfcxSourceField; AAttributeIndex: Integer; AAttributeType: TfcxAttributeType): Integer;
    function AddAttributeStreamField(AFieldProperties: TfcxFieldProperties; AAttributeType: TfcxAttributeType): Integer;
    function AddDatePathField(AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType): Integer;
    function AddTimePathField(AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType): Integer;
    function CheckAttributeStreamField(AFieldProperties: TfcxFieldProperties; AAttributeType: TfcxAttributeType; AFieldIndex: Integer): Boolean;
    function CheckDatePathField(AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType; AFieldIndex: Integer): Boolean;
    function CheckTimePathField(AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType; AFieldIndex: Integer): Boolean;
    procedure DeleteField(AField: TfcxCommonField);
    constructor Create(AMasterField: TfcxCommonUVField); overload;
  end;

  _fcxCommonUVFieldArray = array[0..0] of TfcxCommonUVField;
  PfcxCommonUVFieldArray = ^_fcxCommonUVFieldArray;

  TfcxDataLoaderType =
  (
    dlt_None,
    dlt_DataSource,
    dlt_Stream,
    dlt_Calculation
  );
  TfcxCommonDataLoader = class // Loader of field data
  private
    FField: TfcxCommonUVField;
// Create UniqueValues for field
    procedure CreateUVs; virtual;
    function CreateUniqueValues: TfcxBaseUniqueValues; virtual; abstract;
// Add new Value from source
    procedure AddValue(var AUValue:  PfcxCommonUV; ASourceRecord: TfcxSourceRecord); virtual; abstract;
    procedure LinkAttributes; virtual; abstract;
  protected
  public
    constructor Create(AField: TfcxCommonUVField); virtual;
  end;

  TfcxDSDataLoader = class(TfcxCommonDataLoader) // Loader from DataSource
  private
    FSourceField: TfcxSourceField;
    procedure CreateUVs; override;
    function CreateUniqueValues: TfcxBaseUniqueValues; override;
    procedure AddValue(var AUValue: PfcxCommonUV; ASourceRecord: TfcxSourceRecord); override;
    procedure SetSourceField(const Value: TfcxSourceField);
    procedure LinkAttributes; override;
  protected
  public
    property SourceField: TfcxSourceField read FSourceField write SetSourceField;
  end;

  TfcxStreamDataLoader = class(TfcxCommonDataLoader) // Loader from Stream
  private
    function CreateUniqueValues: TfcxBaseUniqueValues; override;
    procedure AddValue(var AUValue: PfcxCommonUV; ASourceRecord: TfcxSourceRecord); override;
    procedure LinkAttributes; override;
  protected
  public
  end;

  TfcxCalcDataLoader = class(TfcxCommonDataLoader) // Loader for Calculation fields
  private
    function CreateUniqueValues: TfcxBaseUniqueValues; override;
    procedure AddValue(var AUValue: PfcxCommonUV; ASourceRecord: TfcxSourceRecord); override;
    procedure LinkAttributes; override;
  protected
  public
  end;

  TfcxCommonField = class // field in Fact Table
  private
    FCube: TfcxCube;
    FMasterField: TfcxCommonUVField;
    FMasterFieldsMap: PfcxPointerArray; //PfcxCommonUVFieldArray
    FFields: TfcxAttributeFields;

    FCaseSensitive: boolean;
    FNullStr: TfcxString;
    FCubeFieldDisplayLabel: TfcxString;
    FCubeFieldName: TfcxString;
    FDataType: TfcxDataType;
    FIndex: Integer;
    FAttributeIndex: Integer;
    FLevel: Integer;
    FSaved: Boolean;
    FDataLoaderType: TfcxDataLoaderType;
    FDataLoader: TfcxCommonDataLoader;
    procedure SetCaseSensitive(const Value: boolean);
    procedure SetNullStr(const Value: TfcxString);
    procedure SetCubeFieldDisplayLabel(const Value: TfcxString);

{
// Create UniqueValues for field
    procedure CreateUVs; virtual; abstract;
// Add new Value from source
    procedure AddValue(var AUValue:  PfcxCommonUV; ASourceRecord: TfcxSourceRecord); virtual; abstract;
}
    class function ReadOldFieldBaseProps(ACubeStream: TStream; AMajorVersion, AMinorVersion: integer): TfcxOldSavedCubeField;
    class function LoadPropsFromXMLItem(AItem: TfcxXMLItem): TfcxFieldProperties;
    function GetDataTypeProcessor: TfcxCommonDataTypeProcessor; virtual; abstract;
    function GetfcxVarType: TfcxVarType;
    function GetAttribute(Index: TfcxString): TfcxCommonField;
    function GetCaptionValueAtIndex(Index: integer): TfcxString; virtual;
    function GetValueAsVariantAtIndex(Index: integer): Variant; virtual;

    function GetCanGroup: Boolean; virtual;
    function GetGroupCaption(AGroupIndex: integer): TfcxString;
    function GetGroupCount: Integer;
    function GetGroupIndexByOrder(AOrder: integer): integer;
    function GetGroupManager: TfcxCommonGroupManager; virtual;
    function GetGroupUVCount(AGroupIndex: integer): integer;
    function GetGroupUVIndexByOrder(AGroupIndex, AOrder: integer): integer;
    function GetHasGroups: Boolean;
    function GetNonGroupUVCount: integer;
    function GetNonGroupUVIndexByOrder(AOrder: integer): integer;
    function GetUVCaptionInGroup(AGroupIndex, AOrder: integer): TfcxString;
    function GetUVCaptionInNonGroups(AOrder: integer): TfcxString;
    procedure SetCanGroup(const Value: Boolean); virtual;
    procedure SetGroupCaption(AGroupIndex: integer;
      const Value: TfcxString);
//    function GetIndex: Integer;
    procedure SaveToXMLItem(AItem: TfcxXMLItem); virtual;
    procedure LoadFromXMLItem(AItem: TfcxXMLItem); virtual;
    procedure LoadFromXMLItemStep2(AItem: TfcxXMLItem); virtual;
    procedure SaveUVsToStream(ACubeStream: TStream); virtual; abstract;
    procedure LoadUVsFromStream(ACubeStream: TStream); virtual; abstract;
    procedure AppendUVsFromStream(ACubeStream: TStream); virtual; abstract;
    function GetDisplayFormat: TfcxFormat; virtual;
    procedure SetDisplayFormat(const Value: TfcxFormat); virtual;
    procedure SetSaved(const Value: Boolean); virtual;
    procedure SetDataLoaderType(const Value: TfcxDataLoaderType);
  public
    OldSavedCubeField: ^TfcxOldSavedCubeFieldPropeties;
    constructor Create(ACube: TfcxCube); overload; virtual;
    destructor Destroy; override;

    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
    property NullStr: TfcxString read FNullStr write SetNullStr;
    property CubeFieldName: TfcxString read FCubeFieldName;
    property CubeFieldDisplayLabel: TfcxString read FCubeFieldDisplayLabel write SetCubeFieldDisplayLabel;
    property DataType: TfcxDataType read FDataType;
    property Index: Integer read FIndex;
    property Level: Integer read FLevel;
    property Saved: Boolean read FSaved write SetSaved;
    property DataTypeProcessor: TfcxCommonDataTypeProcessor read GetDataTypeProcessor;
    property Cube: TfcxCube read FCube;
    property MasterField: TfcxCommonUVField read FMasterField;
    property fcxVarType: TfcxVarType read GetfcxVarType;
    property Fields: TfcxAttributeFields read FFields;
    property Attribute[Index: TfcxString]: TfcxCommonField read GetAttribute; default;
    property AttributeIndex: Integer read FAttributeIndex write FAttributeIndex;
    property CaptionValueAtIndex[Index: integer]: TfcxString read GetCaptionValueAtIndex;
    property ValueAsVariantAtIndex[Index: integer]: Variant read GetValueAsVariantAtIndex;
// groups
// наличие возможности группировать
    property CanGroup: Boolean read GetCanGroup write SetCanGroup;
// наличие непустых групп
    property HasGroups: Boolean read GetHasGroups;
    property GroupManager: TfcxCommonGroupManager read GetGroupManager;
// число групп
    property GroupCount: Integer read GetGroupCount;
// отображаемое значение uheggs
    property GroupCaption[AGroupIndex: integer]: TfcxString read GetGroupCaption write SetGroupCaption;
// уникальный код по порядку сортировки
    property GroupIndexByOrder[AOrder: integer]: integer read GetGroupIndexByOrder;
// количество уникальных значений в группе
    property GroupUVCount[AGroupIndex: integer]: integer read GetGroupUVCount;
// отображаемое значение уникальных значений в группе (пока по прямому индексу вхождения в группу)
    property UVCaptionInGroup[AGroupIndex, AOrder: integer]: TfcxString read GetUVCaptionInGroup;
// уникальный код по порядку сортировки (пока по прямому индексу вхождения в группу)
    property GroupUVIndexByOrder[AGroupIndex, AOrder: integer]: integer read GetGroupUVIndexByOrder;
// количество уникальных значений вне групп
    property NonGroupUVCount: integer read GetNonGroupUVCount;
// отображаемое значение уникальных значений вне групп (пока по прямому индексу оставшихся без группы + неоптимально в менеджере групп ??)
    property UVCaptionInNonGroups[AOrder: integer]: TfcxString read GetUVCaptionInNonGroups;
// уникальный код по порядку сортировки (пока по прямому индексу оставшихся без группы + неоптимально в менеджере групп ??)
    property NonGroupUVIndexByOrder[AOrder: integer]: integer read GetNonGroupUVIndexByOrder;
    property DisplayFormat: TfcxFormat read GetDisplayFormat write SetDisplayFormat;
    property DataLoaderType: TfcxDataLoaderType read FDataLoaderType write SetDataLoaderType;
    property DataLoader: TfcxCommonDataLoader read FDataLoader;
  end;

  TfcxCommonStdPathField = class(TfcxCommonField)
  private
    FStdPathUniqueValues: TfcxStandardPathUniqueValues;
    function GetDataTypeProcessor: TfcxCommonDataTypeProcessor; override;
    function GetCaptionValueAtIndex(Index: integer): TfcxString; override;
    function GetValueAsVariantAtIndex(Index: integer): Variant; override;
    function GetCanGroup: Boolean; override;
    function GetGroupManager: TfcxCommonGroupManager; override;
    procedure SetCanGroup(const Value: Boolean); override;
    procedure SaveUVsToStream(ACubeStream: TStream); override;
    procedure LoadUVsFromStream(ACubeStream: TStream); override;
    procedure AppendUVsFromStream(ACubeStream: TStream); override;
  public
    destructor Destroy; override;
    property StdPathUniqueValues: TfcxStandardPathUniqueValues read FStdPathUniqueValues;
  end;

  TfcxCommonDatePathField = class(TfcxCommonStdPathField)
  private
    FDateType: TfcxDateType;
//    FDatePathUniqueValues: TfcxDatePathUniqueValues;
    function GetDatePathProcessor: TfcxCommonDatePathDTP;
    function GetDatePathUniqueValues: TfcxDatePathUniqueValues;
    procedure SaveToXMLItem(AItem: TfcxXMLItem); override;
    procedure LoadFromXMLItem(AItem: TfcxXMLItem); override;
    function CheckField(AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType): Boolean;
  public
    constructor Create(AMasterField: TfcxCommonUVField;
      AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType); overload;
    function CustomMonthNames(AMonth: word): TfcxString;
    function CustomWeekDayNames(AWeekDay: word): TfcxString;
    function CustomQuarterNames(AQuarter: word): TfcxString;
    function CustomWeekNumberNames(AWeekNumber: word; AYearDelta: integer): TfcxString;
    property DatePathProcessor: TfcxCommonDatePathDTP read GetDatePathProcessor;
    property DatePathUniqueValues: TfcxDatePathUniqueValues read GetDatePathUniqueValues;
    property DateType: TfcxDateType read FDateType;
  end;

  TfcxCommonTimePathField = class(TfcxCommonStdPathField)
  private
    FTimeType: TfcxTimeType;
//    FTimePathUniqueValues: TfcxTimePathUniqueValues;
    function GetTimePathProcessor: TfcxCommonTimePathDTP;
    function GetTimePathUniqueValues: TfcxTimePathUniqueValues;
    procedure SaveToXMLItem(AItem: TfcxXMLItem); override;
    procedure LoadFromXMLItem(AItem: TfcxXMLItem); override;
    function CheckField(AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType): Boolean;
  protected
  public
    constructor Create(AMasterField: TfcxCommonUVField;
      AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType); overload;
    property TimePathProcessor: TfcxCommonTimePathDTP read GetTimePathProcessor;
    property TimePathUniqueValues: TfcxTimePathUniqueValues read GetTimePathUniqueValues;
    property TimeType: TfcxTimeType read FTimeType;
  end;

  TfcxCommonUVField = class(TfcxCommonField)
  private
    FUniqueValues: TfcxBaseUniqueValues;
(*
    FSourceField: TfcxSourceField;
*)
    FCalculateAfterAll: Boolean;
    function GetDataTypeProcessor: TfcxCommonDataTypeProcessor; override;

    function GetCaptionValueAtIndex(Index: integer): TfcxString; override;
    function GetValueAsVariantAtIndex(Index: integer): Variant; override;
    function GetCanGroup: Boolean; override;
    function GetGroupManager: TfcxCommonGroupManager; override;
    procedure SetCanGroup(const Value: Boolean); override;
    procedure SaveToXMLItem(AItem: TfcxXMLItem); override;
    procedure LoadFromXMLItem(AItem: TfcxXMLItem); override;
    procedure LoadFromXMLItemStep2(AItem: TfcxXMLItem); override;
    procedure SaveUVsToStream(ACubeStream: TStream); override;
    procedure LoadUVsFromStream(ACubeStream: TStream); override;
    procedure AppendUVsFromStream(ACubeStream: TStream); override;
    procedure SetSourceField(const Value: TfcxSourceField);
  protected
  public
    destructor Destroy; override;
    property UniqueValues: TfcxBaseUniqueValues read FUniqueValues;  // List of unique values
(*
    property SourceField: TfcxSourceField read FSourceField write SetSourceField;
*)
    property CalculateAfterAll: Boolean read FCalculateAfterAll;
  end;

  TfcxUVField = class(TfcxCommonUVField)
  private
    function CheckField(AFieldProperties: TfcxFieldProperties): Boolean;
  protected
  public
    constructor Create(ACube: TfcxCube; ASourceField: TfcxSourceField); overload; // with TfcxDSDataLoader
    constructor Create(ACube: TfcxCube; AFieldProperties: TfcxFieldProperties); overload; // with TfcxStreamDataLoader
  end;

  TfcxAttributeField = class(TfcxUVField) // Attribute field
  private
    FAttributeType: TfcxAttributeType;
    FFromMasterSource: Boolean;
    FLoadAllValues: Boolean;
    procedure SaveToXMLItem(AItem: TfcxXMLItem); override;
    procedure SetSaved(const Value: Boolean); override;
    function CheckField(AFieldProperties: TfcxFieldProperties; AAttributeType: TfcxAttributeType): Boolean; overload;
  protected
  public
    constructor Create(AMasterField: TfcxCommonUVField;
      ASourceField: TfcxSourceField; AAttributeIndex: integer; AAttributeType: TfcxAttributeType); overload;
    constructor Create(AMasterField: TfcxCommonUVField;
      AFieldProperties: TfcxFieldProperties; AAttributeType: TfcxAttributeType); overload;
    procedure SetIndex(AIndex: integer);
    procedure SetUniqueValues(AUniqueValues: TfcxBaseUniqueValues);
    property AttributeType: TfcxAttributeType read FAttributeType write FAttributeType;
    property FromMasterSource: Boolean read FFromMasterSource;
    property LoadAllValues: Boolean read FLoadAllValues;
  end;

  TfcxGetDataForRecord = function(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean of object;

  TfcxCalcField = class(TfcxCommonUVField) // field Created after loading
  private
    FCFieldType: TfcxCFieldType;
    FSystemText: TfcxString;
    GetData: TfcxGetDataForRecord;
    function GetDataEvent(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
//    function GetDataSimple(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
    function GetDataScript(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
    function GetDataExpression(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
    procedure SaveToXMLItem(AItem: TfcxXMLItem); override;
    function CheckField(ACFieldProperties: TfcxCFieldProperties): Boolean;
  protected
  public
    constructor Create(ACube: TfcxCube;
      ACFieldProperties: TfcxCFieldProperties); overload;
    property CFieldType: TfcxCFieldType read FCFieldType;
  end;

const
  DefaultColValue: TfcxCubeDataColumn = (Width: 0; Field: nil; Visible: True; SortIndex: -1; SortDirection: fcsd_Asc);
  DefaultFieldInIndexValue: TfcxCubeFieldInIndex = (CubeField: nil; SortDirection: fcsd_Asc);
var
  TrialString: string;
  
implementation
//VCL uses section
{$IFNDEF FMX}
uses
  fcxError,
  fcxStreamUtils,
  fcxSort,
  fcxStringUtils,
  fcxFilters,
  fcxCodeUtils,
  TypInfo
{$IFDEF DELPHI_6UP}
  ,Variants, RTLConsts
{$ELSE}
  ,consts
{$ENDIF}
  ;
//FMX uses
{$ELSE FMX}
uses
  FMX.fcxError, FMX.fcxStreamUtils, FMX.fcxSort, FMX.fcxStringUtils,
  FMX.fcxFilters, FMX.fcxCodeUtils, 
  System.TypInfo, System.Variants, System.RTLConsts;
{$ENDIF FMX}
type
  TfcxHackBaseUniqueValues = class(TfcxBaseUniqueValues);
  TfcxHackDatePathsManager = class(TfcxDatePathsManager);
  TfcxHackTimePathsManager = class(TfcxTimePathsManager);
  TfcxHackAttributesManager = class(TfcxAttributesManager);
  TfcxHackAttributeUniqueValues = class(TfcxAttributeUniqueValues);
const
  CubeFileOldSignature: AnsiString = 'MDC1';
  CubeFileSignature: AnsiString = 'MDC2';
  CubeStreamOldSignature: AnsiString = 'MDCube ';
  CubeStreamSignature: AnsiString = 'MDCUB2 ';
  CubeFormatMajorVersion = 1;
// Временно!
  CubeFormatMinorVersion = 2;

// for load FastCube 1.*
  FieldTypeToVarType: array[TFieldType] of Integer =
    (varUnknown, varString, varSmallint, varInteger, varInteger, varBoolean,
      varDouble,
    varCurrency, varDouble, varDate, varDate, varDate, varError, varError,
    varInteger, varError, varError, varError, varError, varError,
    varError, varError, varError, varString, varOleStr,
{$IFNDEF DELPHI_6UP}
    varError,
{$ELSE}
    varInt64,
{$ENDIF}
    varError, varError, varError, varError, varError,
    varError, varString, varError, varError, varError
{$IFDEF SQL_TYPES_EXTRA0}
    ,  varDate {varSQLTimeStamp},  varDouble {varFMTBcd}
{$ENDIF}
{$IFDEF DELPHI_10UP}
    , varString, varString, varDate, varError
  {$IFDEF DELPHI_12UP}
    , varLongWord,  varShortInt, varByte, varError, varError, varError, varError
  {$ENDIF}
  {$IFDEF DELPHI_14UP}
    , varError, varError, varError
  {$ENDIF}
{$ELSE}
  {$IFDEF FPC}
  // fpc 2.2
    , varString, varString
  {$ENDIF}
{$ENDIF}
    );

// for load FastCube 1.*
function GetVarType(ft: TFieldType): Integer;
begin
  Result := FieldTypeToVarType[ft];
{$IFDEF SQL_TYPES_EXTRA1}
  if ft = ftTimeStamp then
    Result := varSQLTimeStamp
  else
  if ft = ftFMTBcd then
    Result := varFMTBcd;
{$ENDIF}
end;

{ TfcxCube }

procedure TfcxCube.AddCFields(
  ACFieldsProperties: Array of TfcxCFieldProperties);
var
  i, AFieldIndex: integer;
  ASourceRecord: TfcxSourceRecord;
  AIndexs: Array of integer;
begin
  if Length(ACFieldsProperties) = 0 then
    exit;
  StartChange;
  SetLength(AIndexs, Length(ACFieldsProperties));
  for AFieldIndex := 0 to High(ACFieldsProperties) do
    AIndexs[AFieldIndex] := FFields.AddCField(ACFieldsProperties[AFieldIndex]);
  FSourceHolder.AddFields(Length(ACFieldsProperties));
  for AFieldIndex := 0 to High(ACFieldsProperties) do
    TfcxCommonUVField(FFields.Items[AIndexs[AFieldIndex]]).FDataLoader.CreateUVs;
  ASourceRecord := TfcxSourceRecord.Create(Self, FFields.Count);
  for i := 0 to FSourceHolder.RecordsCount - 1 do
  begin
    ASourceRecord.FRecNo := i;
    for AFieldIndex := 0 to High(ACFieldsProperties) do
      TfcxCommonUVField(FFields.Items[AIndexs[AFieldIndex]]).FDataLoader.AddValue(FSourceHolder.FSourceList[i, AIndexs[AFieldIndex]], ASourceRecord);
  end;
  ASourceRecord.Free;
  StopChange([chc_AddedFields]);
end;

procedure TfcxCube.AddDatePath(AField: TfcxCommonField;
  ADatePath: TfcxDateType);
var
  ASplitIndex: integer;
begin
  if not TfcxCommonUVField(AField).UniqueValues.SplitManager.UseDateSplit then
    exit;
  if TfcxCommonUVField(AField).UniqueValues.SplitManager.DatePathsManager.UseDatePath[ADatePath] then
    exit;
  StartChange;
  ASplitIndex := TfcxHackDatePathsManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.DatePathsManager).AddDatePath(ADatePath);
  TfcxHackBaseUniqueValues(TfcxCommonUVField(AField).UniqueValues).LoadAddons([],[ADatePath],[]);
  StopChange([chc_AddedOneSplit], AField.Index, TfcxCommonUVField(AField), ASplitIndex);
end;

procedure TfcxCube.AddTimePath(AField: TfcxCommonField;
  ATimePath: TfcxTimeType);
var
  ASplitIndex: integer;
begin
  if not TfcxCommonUVField(AField).UniqueValues.SplitManager.UseTimeSplit then
    exit;
  if TfcxCommonUVField(AField).UniqueValues.SplitManager.TimePathsManager.UseTimePath[ATimePath] then
    exit;
  StartChange;
  ASplitIndex := TfcxHackTimePathsManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.TimePathsManager).AddTimePath(ATimePath);
  TfcxHackBaseUniqueValues(TfcxCommonUVField(AField).UniqueValues).LoadAddons([],[],[ATimePath]);
  StopChange([chc_AddedOneSplit], AField.Index, TfcxCommonUVField(AField), ASplitIndex);
end;

procedure TfcxCube.AddAttribute(AField: TfcxCommonField;
  AFieldProperties: TfcxFieldProperties);
begin
  if TfcxCommonUVField(AField).UniqueValues.SplitManager.AttributesManager.AttributeByName[AFieldProperties.CubeFieldName] <> nil then
    exit;
  StartChange;
// временно 12-2011  TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues).LoadAddons([TfcxHackAttributesManager(TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues.SplitManager.AttributesManager).AddCustomAttribute(AFieldProperties)],[],[]);
  StopChange([chc_AddedOneSplit], AField.Index);
end;

procedure TfcxCube.Close;
begin
  if Active then
  begin
    try
      DoBeforeClose;
      FSourceHolder.Free;
      FFields.free;
      FFields := TfcxMasterFields.Create(Self);
      FSourceHolder := TfcxSourceHolder.Create(Self);
      FCubeState := fcst_NotActive;
      DoAfterClose;
    finally
      FCubeState := fcst_NotActive;
    end;
  end;
end;

procedure TfcxCube.CloseSource;
begin
  FTimeStatStart.DBCloseTime := fcxGetTickCount;
  FDataSource.Close;
  FTimeStat.DBCloseTime := FTimeStat.DBCloseTime + fcxGetTickCount - FTimeStatStart.DBCloseTime;
end;

constructor TfcxCube.Create(AOwner: TComponent);
begin
  FDayOfWeekISO8601 := True;
  FWeekNumberISO8601 := True;
  FFormat := TfcxDefaultFormat.Create;
  inherited;
  FCommonScript := TfcxScriptStringList.Create;
  fcxEmptyCode(FCommonScript, ScriptLanguage);
  FSlicesManager := TfcxSlicesManager.Create(Self);
  FCubeStream := nil;
  FCubeLock := False;
  FSkipFieldsWithError := False;
  FLoadActive := False;
  FCubeState := fcst_NotActive;
  FCubeSource := fccs_None;
  FDataSource := nil;
  FCubeFile := '';
  FCubeFileCompressionLevel := clMax;

  FChanges := [];

  FTimeStat := TfcxCubeTimeStat.Create;
  FTimeStatStart := TfcxCubeTimeStat.Create;
  FFields := TfcxMasterFields.Create(Self);
  FSourceHolder := TfcxSourceHolder.Create(Self);
end;

procedure TfcxCube.CreateFields;
var
  i: integer;
begin
  for i := 0 to FDataSource.Fields.Count - 1 do
    if FDataSource.Fields[i].DataField.Initialized then
      FFields.AddDataSourceField(FDataSource.Fields[i]);
  if FSourceHolder = nil then
    FSourceHolder := TfcxSourceHolder.Create(Self);
end;

function TfcxCube.DateTimeConsts: TfcxDateTimeConsts;
begin
  Result.DayOfWeekISO8601 := DayOfWeekISO8601;
  Result.WeekNumberISO8601 := WeekNumberISO8601;
end;

procedure TfcxCube.DeleteField(AFieldName: TfcxString);
begin
  DeleteFieldAt(FFields.FieldIndex(AFieldName));
end;

procedure TfcxCube.DeleteFieldAt(AFieldIndex: Integer);
begin
  if (AFieldIndex = -1) or (AFieldIndex >= FFields.Count) then
    exit;
  StartChange;
  FFields.DeleteField(AFieldIndex);
  FSourceHolder.DeleteField(AFieldIndex);
  StopChange([chc_DeletedField], AFieldIndex);
end;

destructor TfcxCube.Destroy;
begin
  FSourceHolder.Free;
  FFields.Free;
  FTimeStat.Free;
  FTimeStatStart.Free;
  FSlicesManager.Free;
  FCommonScript.Free;
  FFormat.Free;
  inherited;
end;

procedure TfcxCube.DoAfterClose;
begin
  DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_AfterClose]), [chc_AfterClose]));
  if Assigned(FAfterClose) then
    AfterClose(Self);
end;

procedure TfcxCube.DoAfterOpen;
begin
  DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_AfterOpen]), [chc_AfterOpen]));
  if Assigned(FAfterOpen) then
    AfterOpen(Self);
//УДАЛИТЬ  FSourceHolder.FOrderedRecordSetProvider.AddFieldToIndex(FFields.Items[0], fcsd_Asc);
//УДАЛИТЬ  FSourceHolder.FOrderedRecordSetProvider.AddFieldToIndex(FFields.Items[1], fcsd_Asc);
end;

procedure TfcxCube.DoBeforeClose;
begin
//  DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_BeforeClose]), [chc_BeforeClose]));
  if Assigned(FBeforeClose) then
    BeforeClose(Self);
end;

procedure TfcxCube.DoBeforeOpen;
begin
//  DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_BeforeClose]), [chc_BeforeClose]));
  if Assigned(FBeforeOpen) then
    BeforeOpen(Self);
end;

function TfcxCube.GetActive: Boolean;
begin
  Result := FCubeState = fcst_Active;
end;

function TfcxCube.GetCommonScript: TStrings;
begin
  Result := FCommonScript;
end;

function TfcxCube.GetCommonScriptChangedTick: Cardinal;
begin
  Result := FCommonScript.ChangedTick;
end;

function TfcxCube.GetCubeStream: TStream;
begin
  Result := nil;
end;

function TfcxCube.GetCustomAttributeValue(AMasterFieldName,
  AAttributeName: TfcxString; AMasterValue: variant;
  var AAttributeValue: variant): boolean;
begin
  if Assigned(FDataSource) then
    Result := FDataSource.GetCustomAttributeValue(AMasterFieldName, AAttributeName, AMasterValue, AAttributeValue)
  else
    Result := False;
  if not Result then
    if Assigned(FOnGetCustomAttributeValue) then
      Result := FOnGetCustomAttributeValue(Self, AMasterFieldName, AAttributeName, AMasterValue, AAttributeValue)
    else
      Result := False;
end;

procedure TfcxCube.InternalLoadCubeFromDeCompStream(ACubeStream: TStream);
var
  AVersion: array[0..6] of AnsiChar;
  ATempStr: array[0..1] of AnsiChar;
  ATempChar: AnsiChar;
  AMajorVersion, AMinorVersion: byte;
  i, j, AMasterFieldIndex: integer;
  AOldVersion, ANeedLoadAddon: boolean;
  AMasterFieldName: TfcxString;
  ASourceRecord: TfcxSourceRecord;
begin
  ANeedLoadAddon := False;
  ACubeStream.Read(AVersion, Length(CubeStreamSignature)); // Read cube version
  if (AVersion = CubeStreamSignature) then
  begin
    ACubeStream.Read(ATempStr, 2);
    Val(ATempStr, AMajorVersion, i);
    ACubeStream.Read(ATempChar, 1);
    ACubeStream.Read(ATempStr, 2);
    Val(ATempStr, AMinorVersion, i);
    AOldVersion := False;
  end
  else if (AVersion = CubeStreamOldSignature) then
  begin
    ACubeStream.Read(ATempChar, 1);
    Val(ATempChar, AMajorVersion, i);
    ACubeStream.Read(ATempChar, 1);
    ACubeStream.Read(ATempChar, 1);
    Val(ATempChar, AMinorVersion, i);
    AOldVersion := True;
  end
  else
    RaisefcError(exfcError, [fcxResources.GetAnsi('SUnknownCubeFormat')]);
    
  if AOldVersion then
  begin
    if AMajorVersion < 1 then
      Exit;
    if (AMajorVersion > 1) or (AMinorVersion >= 4) then
      FCaption := ReadOldString(ACubeStream) else
      FCaption := '';

    if (AMajorVersion > 2) or ((AMajorVersion = 2) and (AMinorVersion >= 3)) then
    begin
    // version 2.4
      if (AMajorVersion > 2) or (AMinorVersion >= 4) then
        FDescription := ReadOldString(ACubeStream);
      if (AMajorVersion > 3) or ((AMajorVersion = 3) and (AMinorVersion >= 5)) then
        ReadBoolean(ACubeStream);
      if (AMajorVersion > 3) or ((AMajorVersion = 3) and (AMinorVersion >= 6)) then
        ReadBoolean(ACubeStream);

  // Read hierarchies
//      Hierarchies.Clear;
      ACubeStream.Read(j, SizeOf(j)); // version 2.3
      if j > 0 then
        Exit;
{
      for i := 0 to j - 1 do
      begin
        Hierarchi := TfcxCubeHierarchi.Create(Hierarchies);
        TempStream := TMemoryStream.Create;
        ReadStream(Stream, TempStream);
        Hierarchi.LoadFromStream(TempStream);
        TempStream.Free;
      end;
      Hierarchies.Open;
}
    end
    else
    begin
      FDescription := '';
    end;
// Fields
    Fields.LoadFromOldStream(ACubeStream, AMajorVersion, AMinorVersion);
// Unique Values
    for i := 0 to FFields.Count - 1 do
      TfcxBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).LoadFromOldStream(ACubeStream, AMajorVersion, AMinorVersion);
// Source Holder
    if FSourceHolder = nil then
      FSourceHolder := TfcxSourceHolder.Create(Self);
    FSourceHolder.LoadFromStream(ACubeStream);
    for i := FFields.Count - 1 downto 0 do
      if FFields.Items[i].FCubeFieldName[1] = '#' then
      begin
        AMasterFieldName := copy(FFields.Items[i].FCubeFieldName, pos('_', FFields.Items[i].FCubeFieldName) + 1, Length(FFields.Items[i].FCubeFieldName));
        AMasterFieldIndex := FFields.FieldIndex(AMasterFieldName);
        if AMasterFieldIndex > -1 then
        begin
          if FFields.Items[i].OldSavedCubeField^.DateType <> odt_None then
          begin
            TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.UseDateSplit := True;
            TfcxHackDatePathsManager(TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.DatePathsManager).AddDatePath(FFields.Items[i].OldSavedCubeField^.DateType);
            TfcxCommonField(TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.DatePathsManager.DatePathField[FFields.Items[i].OldSavedCubeField^.DateType]).FCubeFieldName := FFields.Items[i].FCubeFieldName;
            TfcxCommonField(TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.DatePathsManager.DatePathField[FFields.Items[i].OldSavedCubeField^.DateType]).FCubeFieldDisplayLabel := FFields.Items[i].FCubeFieldDisplayLabel;
          end;
          if FFields.Items[i].OldSavedCubeField^.TimeType <> ott_None then
          begin
            TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.UseTimeSplit := True;
            TfcxHackTimePathsManager(TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.TimePathsManager).AddTimePath(FFields.Items[i].OldSavedCubeField^.TimeType);
            TfcxCommonField(TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.TimePathsManager.TimePathField[FFields.Items[i].OldSavedCubeField^.TimeType]).FCubeFieldName := FFields.Items[i].FCubeFieldName;
            TfcxCommonField(TfcxCommonUVField(FFields.Items[AMasterFieldIndex]).UniqueValues.SplitManager.TimePathsManager.TimePathField[FFields.Items[i].OldSavedCubeField^.TimeType]).FCubeFieldDisplayLabel := FFields.Items[i].FCubeFieldDisplayLabel;
          end;
          ANeedLoadAddon := True;
          inc(FChangeSemaphore);
          DeleteFieldAt(i);
          dec(FChangeSemaphore);
        end;
      end;
    if ANeedLoadAddon then
    begin
// Load addon
      for i := 0 to FFields.Count - 1 do
        TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).LoadAttributes(False);
    end;
    FSlicesManager.FOldVersion := True;
    if (AMajorVersion = 1) and (AMinorVersion > 2) then
    begin
      ACubeStream.Read(FSlicesManager.FSM, SizeOf(Integer));
      ACubeStream.Read(FSlicesManager.FSC, SizeOf(Integer));
      ACubeStream.Read(FSlicesManager.FSR, SizeOf(Integer));
    end;

    FSlicesManager.FStreamFromCube := TMemoryStream.Create;
    FSlicesManager.FMajorVersion := AMajorVersion;
    FSlicesManager.FMinorVersion := AMinorVersion;
    ReadStreamFromPosition(ACubeStream, FSlicesManager.FStreamFromCube);
  end
  else
  begin
// current version
    FCaption := ReadfcString(ACubeStream);
    FDescription := ReadfcString(ACubeStream);
// Fields
    Fields.LoadFieldsFromStream(ACubeStream);
// Unique Values
    inc(FChangeSemaphore);

    Fields.LoadUVsFromStream(ACubeStream);

    dec(FChangeSemaphore);
// Source Holder
    if FSourceHolder = nil then
      FSourceHolder := TfcxSourceHolder.Create(Self);
    FSourceHolder.LoadFromStream(ACubeStream);

    if Fields.ExistsNotSaved then
    begin
// Create UVs for not Saved fields
      inc(FChangeSemaphore);
      if Fields.ExistsNotSaved(True) then
      begin
        for i := 0 to FFields.Count - 1 do
          with TfcxCommonUVField(FFields.Items[i]) do
            if not CalculateAfterAll and not Saved then
              UniqueValues.Loading := True;
        ASourceRecord := TfcxSourceRecord.Create(Self, FFields.Count);
        for j := 0 to FSourceHolder.RecordsCount - 1 do
        begin
          ASourceRecord.FRecNo := j;
          for i := 0 to FFields.Count - 1 do
            with TfcxCommonUVField(FFields.Items[i]) do
              if not CalculateAfterAll and not Saved then
                FDataLoader.AddValue(FSourceHolder.SourceRec[j][i], ASourceRecord);
        end;
        ASourceRecord.Free;
        for i := 0 to FFields.Count - 1 do
          with TfcxCommonUVField(FFields.Items[i]) do
            if not CalculateAfterAll and not Saved then
            begin
              UniqueValues.Sort(True);
              UniqueValues.SetIndex;
              UniqueValues.Loading := False;
              TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
              UniqueValues.Recapacity;
            end;
      end;
      for i := 0 to FFields.Count - 1 do
        TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).FillNonSavedUVs;
// Fields CalculateAfterAll
      if Fields.ExistsNotSaved(True) then
      begin
        for i := 0 to FFields.Count - 1 do
          with TfcxCommonUVField(FFields.Items[i]) do
            if CalculateAfterAll and not Saved then
              UniqueValues.Loading := True;
        ASourceRecord := TfcxSourceRecord.Create(Self, FFields.Count);
        for j := 0 to FSourceHolder.RecordsCount - 1 do
        begin
          ASourceRecord.FRecNo := j;
          for i := 0 to FFields.Count - 1 do
            with TfcxCommonUVField(FFields.Items[i]) do
              if CalculateAfterAll and not Saved then
                FDataLoader.AddValue(FSourceHolder.SourceRec[j][i], ASourceRecord);
        end;
        ASourceRecord.Free;
        for i := 0 to FFields.Count - 1 do
          with TfcxCommonUVField(FFields.Items[i]) do
            if CalculateAfterAll and not Saved then
            begin
              UniqueValues.Sort(True);
              UniqueValues.SetIndex;
              UniqueValues.Loading := False;
              TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
              UniqueValues.Recapacity;
            end;
      end;
      dec(FChangeSemaphore);
    end;
// Load Groups for not Saved fields
    if AMinorVersion >= 2 then
      LoadGroupsForNotSavedFieldsFromStream(ACubeStream);

    FSlicesManager.FOldVersion := False;
    FSlicesManager.FStreamFromCube := TMemoryStream.Create;
    FSlicesManager.FMajorVersion := AMajorVersion;
    FSlicesManager.FMinorVersion := AMinorVersion;
    ReadStreamFromPosition(ACubeStream, FSlicesManager.FStreamFromCube);
  end;
  FSourceHolder.Recapacity;
  FFields.DeleteLoaders;
  FCubeState := fcst_Active;
end;

procedure TfcxCube.InternalLoadCubeFromFile(AFileName: String);
function IsRelativePath(S: String): Boolean;
begin
  Result :=
    (Length(S) > 0) and
    (
      (S[1] = '.') or     // relative path '.' or '..'
      (
        (S[1] <> '\') and // not network drive
        (S[2] <> ':')     // nor drive letter
      )
    );
end;

function ExpandFileName(S, CurrentDir: String): String;
var
  RealCurrentDir: String;
begin
  RealCurrentDir := GetCurrentDir;
  if RealCurrentDir <> CurrentDir then
    SetCurrentDir(CurrentDir);
{$IFNDEF FMX}
  Result := SysUtils.ExpandFileName(S);
{$ELSE}
  Result := System.SysUtils.ExpandFileName(S);
{$ENDIF}
  if RealCurrentDir <> CurrentDir then
    SetCurrentDir(RealCurrentDir);
end;

var
  AFullFileName: String;
  ACubeStream: TStream;
begin
  AFullFileName := AFileName;
  if IsRelativePath(ExtractFileDir(AFullFileName)) then
    AFullFileName := ExpandFileName(AFullFileName, ExtractFileDir(ParamStr(0)));
  try
    ACubeStream := TFileStream.Create(AFullFileName, fmOpenRead or fmShareDenyNone);
    try
      InternalLoadCubeFromStream(ACubeStream);
    finally
      FreeAndNil(ACubeStream);
    end;
  except
// file is not exists
    FCubeState := fcst_NotActive;
  end;

end;

procedure TfcxCube.InternalLoadCubeFromStream(ACubeStream: TStream);
var
  ADeCompStream: TDecompressionStream;
  AVersion: array[0..6] of AnsiChar;
  ASavePosition: integer;
  P: PAnsiChar;
begin
  GetMem(P, Length(CubeFileSignature) + 1);
  try
    ACubeStream.Read(P^, Length(CubeFileSignature));
    P[4] := #0;
    if (AnsiString(P) = CubeFileSignature) or (AnsiString(P) = CubeFileOldSignature) then
    begin
      FTimeStat.Clear;
      FTimeStatStart.FullTime := fcxGetTickCount;
      FCubeState := fcst_LoadingData;
      try
        FreeAndNil(FSourceHolder);
        FFields.Clear;
        if ACubeStream.Position < ACubeStream.Size then
        begin
    // Check Compression
          ASavePosition := ACubeStream.Position;
          ACubeStream.Read(AVersion, Length(CubeStreamSignature)); // Read cube version
          ACubeStream.Position := ASavePosition;
          if (AVersion = CubeStreamSignature) or (AVersion = CubeStreamOldSignature) then
          begin
            InternalLoadCubeFromDeCompStream(ACubeStream);
          end
          else
          begin
            ADeCompStream := TDecompressionStream.Create(ACubeStream);
            try
              InternalLoadCubeFromDeCompStream(ADeCompStream);
            finally
              ADeCompStream.Free;
            end; (*try*)
          end
        end;
        if FSourceHolder = nil then
          FSourceHolder := TfcxSourceHolder.Create(Self);
      finally
        if FCubeState = fcst_LoadingData then
          FCubeState := fcst_NotActive;
        FTimeStat.FullTime := FTimeStat.FullTime + fcxGetTickCount - FTimeStatStart.FullTime;
      end;
    end;
  finally
    FreeMem(P);
  end;
end;

procedure TfcxCube.InternalLoadFromDataSource;
var
  AErrorMessage: String;
begin
  FTimeStat.Clear;
  FTimeStatStart.FullTime := fcxGetTickCount;
  FCubeState := fcst_LoadingData;
  try
    FreeAndNil(FSourceHolder);
    FFields.Clear;
    if Assigned(FDataSource) then
      if OpenSource then
      begin
        try
          if not FDataSource.Test(AErrorMessage, FSkipFieldsWithError) then
            RaisefcError(exfcError, [AErrorMessage]);
          if FDataSource.Fields.Count = 0 then
            FDataSource.AddFields;
          FDataSource.InitFields;
          CreateFields;
          LoadData;
        finally
          CloseSource;
        end;
      end;
    if FSourceHolder = nil then
      FSourceHolder := TfcxSourceHolder.Create(Self);
  finally
    if FCubeState = fcst_LoadingData then
      FCubeState := fcst_NotActive;
    FTimeStat.FullTime := FTimeStat.FullTime + fcxGetTickCount - FTimeStatStart.FullTime;
  end;
end;

procedure TfcxCube.InternalSaveToStream(ACubeStream: TStream; AFilter: TObject = nil);
var
  VersionString: AnsiString;
begin
  CubeLock := True;
  try
    FSaveFilter := AFilter;
    VersionString := AnsiString(CubeStreamSignature + Format('%.2d.%.2d', [CubeFormatMajorVersion, CubeFormatMinorVersion]));
    ACubeStream.Write(VersionString[1], Length(VersionString)); // Версия куба
    WritefcString(ACubeStream, Caption);
    WritefcString(ACubeStream, Description);
// Fields
    Fields.SaveFieldsToStream(ACubeStream);
// Unique Values
    Fields.SaveUVsToStream(ACubeStream);
// Source Holder
    FSourceHolder.SaveToStream(ACubeStream);

    SaveGroupsForNotSavedFieldsToStream(ACubeStream);
// Slices
    FSlicesManager.SaveToStream(ACubeStream);
  finally
    FSaveFilter := nil;
    CubeLock := False;
  end;
end;

procedure TfcxCube.LoadData;
var
  i, j, RecNo: integer;
//  AOpened: Boolean;
  ASourceRec: PfcxCUVArray;
//  AMessage: TfcxString;
  ASourceRecord: TfcxSourceRecord;
begin
//  if not DataSource.Test(AMessage, FSkipFieldsWithError) then
//    exit;
//  FTimeStatStart.DBOtherTime := fcxGetTickCount;
//  AOpened := DataSource.DataSet.Active;
//  DataSource.DataSet.Open;
//  DataSource.InitFields;
//  FTimeStat.DBOtherTime := FTimeStat.DBOtherTime + fcxGetTickCount - FTimeStatStart.DBOtherTime;

  for i := 0 to FFields.Count - 1 do
  begin
//    TfcxCommonUVField(FFields.Items[i]).UniqueValues.Clear;
    with TfcxCommonUVField(FFields.Items[i]) do
      if not CalculateAfterAll then
        UniqueValues.Loading := True;
  end;

  FTimeStatStart.DBMoveTime := fcxGetTickCount;
  DataSource.DataSet.First;
  InternalOnProgressStart(fcxpFetchingData);
  RecNo := 0;
  while not DataSource.DataSet.Eof do
  begin
    InternalOnProgress(fcxpFetchingData, RecNo);
    FTimeStat.DBMoveTime := FTimeStat.DBMoveTime + fcxGetTickCount - FTimeStatStart.DBMoveTime;
    ASourceRec := FSourceHolder.AddRecord;
    for i := 0 to FFields.Count - 1 do
// добавить УЗ и его атрибуты из основного источника
      with TfcxCommonUVField(FFields.Items[i]) do
        if not CalculateAfterAll then
          FDataLoader.AddValue(ASourceRec[i], nil);
    FTimeStatStart.DBMoveTime := fcxGetTickCount;
    DataSource.DataSet.Next;
    inc(RecNo);
  end;
  InternalOnProgressStop(fcxpFetchingData);
  FTimeStat.DBMoveTime := FTimeStat.DBMoveTime + fcxGetTickCount - FTimeStatStart.DBMoveTime;
//  FTimeStatStart.DBOtherTime := fcxGetTickCount;
//  if not AOpened then
//    DataSource.DataSet.Close;
//  FTimeStat.DBOtherTime := FTimeStat.DBOtherTime + fcxGetTickCount - FTimeStatStart.DBOtherTime;
  for i := 0 to FFields.Count - 1 do
  begin
    with TfcxCommonUVField(FFields.Items[i]) do
      if not CalculateAfterAll then
      begin
        UniqueValues.Sort(True);
        UniqueValues.SetIndex;
        UniqueValues.Loading := False;
        TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
        TfcxHackBaseUniqueValues(UniqueValues).LoadAttributes(False);
        UniqueValues.Recapacity;
        if UniqueValues.Sort(False) then
          UniqueValues.SetIndex;
      end;
  end;
// Fields CalculateAfterAll
  for i := 0 to FFields.Count - 1 do
    with TfcxCommonUVField(FFields.Items[i]) do
      if CalculateAfterAll then
        UniqueValues.Loading := True;
  ASourceRecord := TfcxSourceRecord.Create(Self, FFields.Count);
  for j := 0 to FSourceHolder.RecordsCount - 1 do
  begin
    ASourceRecord.FRecNo := j;
    for i := 0 to FFields.Count - 1 do
      with TfcxCommonUVField(FFields.Items[i]) do
        if CalculateAfterAll then
          FDataLoader.AddValue(FSourceHolder.SourceRec[j][i], ASourceRecord);
  end;
  ASourceRecord.Free;
  for i := 0 to FFields.Count - 1 do
    with TfcxCommonUVField(FFields.Items[i]) do
      if CalculateAfterAll then
      begin
        UniqueValues.Sort(True);
        UniqueValues.SetIndex;
        UniqueValues.Loading := False;
        TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
        UniqueValues.Recapacity;
      end;
  FSourceHolder.Recapacity;
  FFields.DeleteLoaders;
  FCubeState := fcst_Active;
// порвать все связи с TfcxDataSource (TfcxDataField и TfcxSourceField)
end;

procedure TfcxCube.Loaded;
begin
  inherited;
  SetActive(FLoadActive);
end;

function TfcxCube.LoadFromFile(ACubeFileName: String): Boolean;
var
  AOldCubeSource: TfcxCubeSource;
  AOldCubeFile: String;
begin
  AOldCubeSource := CubeSource;
  AOldCubeFile := CubeFile;
  try
    SetActive(False);
    CubeSource := fccs_CubeFile;
    CubeFile := ACubeFileName;
    SetActive(True);
  finally
    CubeSource := AOldCubeSource;
    CubeFile := AOldCubeFile;
  end;
  Result := Active;
end;

function TfcxCube.LoadFromStream(ACubeStream: TStream): Boolean;
var
  AOldCubeSource: TfcxCubeSource;
begin
  AOldCubeSource := CubeSource;
  try
    SetActive(False);
    CubeSource := fccs_CubeStream;
    FCubeStream := ACubeStream;
    SetActive(True);
  finally
    CubeSource := AOldCubeSource;
    FCubeStream := Nil;
  end;
  Result := Active;
end;

procedure TfcxCube.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if AComponent is TfcxDataSource then
    begin
      if AComponent = FDataSource then
        DataSource := nil;
    end
end;

procedure TfcxCube.Open;
begin
  if FCubeState = fcst_NotActive then
  begin
    if CubeSource = fccs_None then
    begin
{ TODO -cНеобходимо : Сделать запрос на источник или решить самостоятельно.}
//      raise Exception.Create('Не указан Источник Данных');
      exit;
    end;
    DoBeforeOpen;
    case CubeSource of
      fccs_DataSource:
        InternalLoadFromDataSource;
      fccs_CubeFile:
        InternalLoadCubeFromFile(CubeFile);
      fccs_CubeStream:
        if FCubeStream <> nil then
          InternalLoadCubeFromStream(FCubeStream)
        else
          InternalLoadCubeFromStream(CubeStream);
    end;
    if Active then
    begin
//!!    FSlices.Build;
      DoAfterOpen;
      FSlicesManager.LoadFromStream;
    end;
  end;
end;

Function TfcxCube.OpenSource: boolean;
begin
  try
    Result := FDataSource.Open;
  finally
    FTimeStat.DBOpenTime := FTimeStat.DBOpenTime + FDataSource.DBOpenTime;
    FTimeStat.OpenTime := FTimeStat.OpenTime + FDataSource.OpenTime;
  end;
  if not Result then
  begin
    FDataSource.Close;
    FTimeStat.DBCloseTime := FTimeStat.DBCloseTime + FDataSource.DBCloseTime;
  end
end;

procedure TfcxCube.RemoveAttribute(AField: TfcxCommonField;
  ACustomPathName: TfcxString);
var
  AAttributeIndex: integer;
begin
  AAttributeIndex := TfcxHackAttributesManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.AttributesManager).FindAttribute(ACustomPathName);
  if AAttributeIndex < 0 then
    exit;
  StartChange;
  TfcxHackAttributesManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.AttributesManager).RemoveAttribute(AAttributeIndex);
  StopChange([chc_ChangedSplits], AField.Index, TfcxCommonUVField(AField), AAttributeIndex);
end;

procedure TfcxCube.RemoveTimePath(AField: TfcxCommonField;
  ATimePath: TfcxTimeType);
var
  ASplitFieldIndex: integer;
begin
  if not TfcxCommonUVField(AField).UniqueValues.SplitManager.UseTimeSplit then
    exit;
  if not TfcxCommonUVField(AField).UniqueValues.SplitManager.TimePathsManager.UseTimePath[ATimePath] then
    exit;
  ASplitFieldIndex := TfcxCommonTimePathField(TfcxCommonUVField(AField).UniqueValues.SplitManager.TimePathsManager.TimePathField[ATimePath]).Index;
  StartChange;
// need notify slices!
  StopChange([chc_DeletingOneSplit], AField.Index, TfcxCommonUVField(AField), ASplitFieldIndex);
  StartChange;
  TfcxHackTimePathsManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.TimePathsManager).RemoveTimePath(ATimePath);
  StopChange([chc_DeletedOneSplit], AField.Index, TfcxCommonUVField(AField), ASplitFieldIndex);
end;

procedure TfcxCube.RemoveDatePath(AField: TfcxCommonField;
  ADatePath: TfcxDateType);
var
  ASplitFieldIndex: integer;
begin
  if not TfcxCommonUVField(AField).UniqueValues.SplitManager.UseDateSplit then
    exit;
  if not TfcxCommonUVField(AField).UniqueValues.SplitManager.DatePathsManager.UseDatePath[ADatePath] then
    exit;
  ASplitFieldIndex := TfcxCommonDatePathField(TfcxCommonUVField(AField).UniqueValues.SplitManager.DatePathsManager.DatePathField[ADatePath]).Index;
  StartChange;
// need notify slices!
  StopChange([chc_DeletingOneSplit], AField.Index, TfcxCommonUVField(AField), ASplitFieldIndex);
  StartChange;
  TfcxHackDatePathsManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.DatePathsManager).RemoveDatePath(ADatePath);
  StopChange([chc_DeletedOneSplit], AField.Index, TfcxCommonUVField(AField), ASplitFieldIndex);
end;

procedure TfcxCube.RemoveAttribute(AField: TfcxCommonField;
  AAttributeIndex: integer);
begin
  StartChange;
  TfcxHackAttributesManager(TfcxCommonUVField(AField).UniqueValues.SplitManager.AttributesManager).RemoveAttribute(AAttributeIndex);
  StopChange([chc_ChangedSplits], AField.Index, TfcxCommonUVField(AField), AAttributeIndex);
end;

procedure TfcxCube.SaveToFile(ACubeFileName: String; AFilter: TObject = nil);
var
  ACubeStream: TStream;
begin
  ACubeStream := TFileStream.Create(ACubeFileName, fmCreate);
  try
    SaveToStream(ACubeStream, FCubeFileCompressionLevel, AFilter);
  finally
    ACubeStream.Free;
  end;
end;

procedure TfcxCube.SaveToStream(ACubeStream: TStream;
  ACompressionLevel: TCompressionLevel = clMax; AFilter: TObject = nil);
var
  ACompStream: TCompressionStream;
begin
  try
    ACubeStream.Write(CubeFileSignature[1], Length(CubeFileSignature));
    if ACompressionLevel = clNone then
    begin
      InternalSaveToStream(ACubeStream, AFilter);
    end
    else
    begin
      ACompStream := TCompressionStream.Create(ACompressionLevel, ACubeStream);
      try
        InternalSaveToStream(ACompStream, AFilter);
      finally
        ACompStream.Free;
      end; (*try*)
    end
  finally
  end;
end;

procedure TfcxCube.SetActive(const Value: Boolean);
begin
  if csReading in ComponentState then
  begin
    FLoadActive := Value;
    Exit;
  end;
  if (Active <> Value) then
  begin
    if Value then
      Open else
      Close;
  end;
end;

procedure TfcxCube.SetCaption(const Value: TfcxString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_Caption]), [chc_Caption]));
  end;
end;

procedure TfcxCube.SetCubeFile(const Value: string);
begin
  FCubeFile := Value;
end;

procedure TfcxCube.SetCubeLock(const Value: Boolean);
begin
  FCubeLock := Value;
end;

procedure TfcxCube.SetDataSource(const Value: TfcxDataSource);
begin
  if Assigned(FDataSource) then
    FDataSource.RemoveFreeNotification(Self);
  FDataSource := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TfcxCube.SetDefaults;
begin
  inherited;
  FFormat.Assign(fcDefaultSettingsStore.Format);
end;

procedure TfcxCube.SetDescription(const Value: TfcxString);
begin
  FDescription := Value;
end;

procedure TfcxCube.SetFormat(const Value: TfcxDefaultFormat);
begin
  FFormat.Assign(Value);
end;

procedure TfcxCube.SetOnGetCalcValue(const Value: TfcxGetCalcValue);
begin
  FOnGetCalcValue := Value;
end;

procedure TfcxCube.SetOnGetCustomAttributeValue(
  const Value: TfcxGetCustomAttributeValue);
begin
  FOnGetCustomAttributeValue := Value;
end;

procedure TfcxCube.SetTimePaths(AFieldIndex: integer;
  ATimePaths: TfcxTimeTypes);
begin
  if not TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues.SplitManager.UseTimeSplit then
    exit;
  StartChange;
  TfcxHackTimePathsManager(TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues.SplitManager.TimePathsManager).SetTimePaths(ATimePaths);
  TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues).LoadAddons;
  StopChange([chc_ChangedSplits], AFieldIndex);
end;

procedure TfcxCube.SetDatePaths(AFieldIndex: integer;
  ADatePaths: TfcxDateTypes);
begin
  if not TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues.SplitManager.UseDateSplit then
    exit;
  StartChange;
  TfcxHackDatePathsManager(TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues.SplitManager.DatePathsManager).SetDatePaths(ADatePaths);
  TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[AFieldIndex]).UniqueValues).LoadAddons;
  StopChange([chc_ChangedSplits], AFieldIndex);
end;

procedure TfcxCube.StopChange(AChanges: TfcxChangesInCube = []; AIndex: integer = -1;
  AMasterField: TfcxCommonUVField = nil; ASplitFieldIndex: integer = -1);
var
  AFIndex, AFSplitFieldIndex: integer;
  AFMasterField: TfcxCommonUVField;
begin
  if FChangeSemaphore > 0 then
    Exit;
  FChanges := FChanges + AChanges;
  dec(FChangeCount);
  if FChangeCount <= 0 then
  begin
    AFIndex := -1;
    FChangeCount := 0;
    AFSplitFieldIndex := -1;
    AFMasterField := nil;
{ TODO -cНеобходимо : ТУТ ОБРАБАТЫВАЕМ ИЗМЕНЕНИЯ.}
    if not (chc_FieldsList in FChanges) then
      if chc_AddedFields in FChanges then
      begin
        if (FChanges - [chc_AddedFields]) <> [] then
          FChanges := FChanges + [chc_FieldsList];
      end
      else
      if chc_DeletedField in FChanges then
      begin
        if (FChanges - [chc_DeletedField]) <> [] then
          FChanges := FChanges + [chc_FieldsList]
        else
          AFIndex := AIndex;
      end
      else
      if chc_ChangedSplits in FChanges then
      begin
        if (FChanges - [chc_ChangedSplits]) <> [] then
          FChanges := FChanges + [chc_FieldsList]
        else
        begin
          AFIndex := AIndex;
          AFMasterField := AMasterField;
        end
      end
      else
      if chc_GroupsChanged in FChanges then
      begin
        if (FChanges - [chc_GroupsChanged]) <> [] then
          FChanges := FChanges + [chc_FieldsList]
        else
          AFIndex := AIndex;
      end
      else
      if chc_AddedOneSplit in FChanges then
      begin
        if (FChanges - [chc_AddedOneSplit]) <> [] then
          FChanges := FChanges + [chc_FieldsList]
        else
        begin
          AFIndex := AIndex;
          AFMasterField := AMasterField;
          AFSplitFieldIndex := ASplitFieldIndex;
        end
      end
      else
      if chc_DeletedOneSplit in FChanges then
      begin
        if (FChanges - [chc_DeletedOneSplit]) <> [] then
          FChanges := FChanges + [chc_FieldsList]
        else
        begin
          AFIndex := AIndex;
          AFMasterField := AMasterField;
          AFSplitFieldIndex := ASplitFieldIndex;
        end
      end
      else
      if chc_DeletingOneSplit in FChanges then
      begin
        if (FChanges - [chc_DeletingOneSplit]) <> [] then
          FChanges := FChanges + [chc_FieldsList]
        else
        begin
          AFIndex := AIndex;
          AFMasterField := AMasterField;
          AFSplitFieldIndex := ASplitFieldIndex;
        end
      end;

// это строка оповещения слушателей об изменении в компоненте. надо решать где её использовать
    if (FChanges - [chc_AddedOneSplit, chc_DeletedOneSplit, chc_DeletingOneSplit, chc_ChangedSplits]) = [] then
      DoChange(TfcxCubeSplitsChangeAlert.Create(CubeChangeTypeOfChanges(FChanges), FChanges, AFIndex, AFMasterField, AFSplitFieldIndex))
    else
      DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges(FChanges), FChanges, AFIndex));
    FChanges:=[];
    if Assigned(OnStopChange) then
      OnStopChange(Self);
  end;
end;

procedure TfcxCube.SaveGroupsToXML(AXMLDoc: TfcxXMLDocument);
begin
  SaveGroupsToXMLItem(AXMLDoc.Root);
end;

procedure TfcxCube.SaveGroupsToFile(AGroupsFileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AGroupsFileName, fmCreate);
  try
    SaveGroupsToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TfcxCube.LoadGroupsFromFile(AGroupsFileName: String): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AGroupsFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadGroupsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TfcxCube.LoadGroupsFromXML(AXMLDoc: TfcxXMLDocument): Boolean;
begin
  Result := LoadGroupsFromXMLItem(AXMLDoc.Root);
end;

function TfcxCube.LoadGroupsFromXMLItem(AItem: TfcxXMLItem; AClearGroups: boolean=True): Boolean;
var
  i, AFieldIndex: integer;
begin
  Result := False;
  if AItem.Name <> 'groups' then
    exit;
  StartChange;
  if AClearGroups then
    ClearGroups;
  for i := 0 to AItem.Count - 1 do
  begin
    AFieldIndex := Fields.FieldIndex(AItem[i].Prop['name']);
    if AFieldIndex <> -1 then
      TfcxCommonUVField(Fields.Items[AFieldIndex]).FUniqueValues.LoadGroupsFromXML(AItem[i]);
  end;
  StopChange([]);
  Result := True;
end;

procedure TfcxCube.SaveGroupsToXMLItem(AItem: TfcxXMLItem; AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  AItem.Name := 'groups';
  AItem.IntProp['version'] := 2;
  AItem.Prop['timestamp'] := DateTimeToStr(Now);
  for i := 0 to Fields.Count - 1 do
    TfcxCommonUVField(Fields.Items[i]).UniqueValues.SaveGroupsToXML(AItem, AOnlyForNotSavedFields);
end;

procedure TfcxCube.ClearGroups;
var
  i: integer;
begin
  for i := 0 to Fields.Count - 1 do
    TfcxCommonUVField(Fields.Items[i]).UniqueValues.ClearGroups;
end;

procedure TfcxCube.SaveGroupsForNotSavedFieldsToStream(
  ACubeStream: TStream);
var
  ABool: Boolean;
  Doc: TfcxXMLDocument;
  AMemoryStream: TMemoryStream;
begin
  Doc := TfcxXMLDocument.Create;
  Doc.AutoIndent := False;
  AMemoryStream := TMemoryStream.Create;
  try
    SaveGroupsToXMLItem(Doc.Root, True);
    if Doc.Root.Count > 0 then
    begin
      ABool := True;
      ACubeStream.Write(ABool, SizeOf(Boolean));
      Doc.SaveToStream(AMemoryStream);
      AMemoryStream.Position := 0;
      WriteStream(ACubeStream, AMemoryStream);
    end
    else
    begin
      ABool := False;
      ACubeStream.Write(ABool, SizeOf(Boolean));
    end
  finally
    AMemoryStream.Free;
    Doc.Free;
  end;
end;

procedure TfcxCube.LoadGroupsForNotSavedFieldsFromStream(
  ACubeStream: TStream);
var
  XMLDoc: TfcxXMLDocument;
  AMemoryStream: TMemoryStream;
  AExistsGroups: Boolean;
begin
  ACubeStream.Read(AExistsGroups, SizeOf(Boolean));
  if AExistsGroups then
  begin
    AMemoryStream := TMemoryStream.Create;
    XMLDoc := TfcxXMLDocument.Create;
    try
      ReadStream(ACubeStream, AMemoryStream);
      XMLDoc.LoadFromStream(AMemoryStream);
      LoadGroupsFromXMLItem(XMLDoc.Root, False);
    finally
      XMLDoc.Free;
      AMemoryStream.Free;
    end;
  end;
end;

procedure TfcxCube.Refresh(SaveFilters: Boolean);
var
  Stream: TStream;
begin
  if not Active then
    exit;
  Stream := TMemoryStream.Create;
  try
    if SaveFilters then
      FSlicesManager.SaveToStream(Stream, [fcxiss_Charts, fcxiss_Filters, fcxiss_Groups])
    else
      FSlicesManager.SaveToStream(Stream, [fcxiss_Charts, fcxiss_Groups]);
    Close;
    Open;
    FSlicesManager.FStreamFromCube := Stream;
    FSlicesManager.FStreamFromCube.Position := 0;
    FSlicesManager.FOldVersion := False;
    FSlicesManager.LoadFromStream;
  finally
    FreeAndNil(FSlicesManager.FStreamFromCube);
  end;
end;

procedure TfcxCube.AppendData;
begin
  if FCubeState = fcst_Active then
  begin
    if CubeSource = fccs_None then
    begin
{ TODO -cНеобходимо : Сделать запрос на источник или решить самостоятельно.}
//      raise Exception.Create('Не указан Источник Данных');
      exit;
    end;
//    DoBeforeAppend;
    case CubeSource of
      fccs_DataSource:
        InternalAppendDataFromDataSource;
      fccs_CubeFile:
        InternalAppendCubeFromFile(CubeFile);
      fccs_CubeStream:
        if FCubeStream <> nil then
          InternalAppendCubeFromStream(FCubeStream)
        else
          InternalAppendCubeFromStream(CubeStream);
    end;
    if Active then
    begin
//      DoAfterAppend;
//      FSlicesManager.LoadFromStream;
    end;
  end;
end;

procedure TfcxCube.InternalAppendDataFromDataSource;
var
  AErrorMessage: String;
begin
  FTimeStat.Clear;
  FTimeStatStart.FullTime := fcxGetTickCount;
  FCubeState := fcst_AppendingData;
  try
    if Assigned(FDataSource) then
      if OpenSource then
      begin
        try
          if not FDataSource.Test(AErrorMessage, FSkipFieldsWithError) then
            RaisefcError(exfcError, [AErrorMessage]);
          if FDataSource.Fields.Count = 0 then
            FDataSource.AddFields;
          FDataSource.InitFields;
          if LinkFields then
            LoadAppendingData;
          FFields.DeleteLoaders;
        finally
          CloseSource;
        end;
      end;
  finally
    if FCubeState = fcst_AppendingData then
      FCubeState := fcst_NotActive;
    FTimeStat.FullTime := FTimeStat.FullTime + fcxGetTickCount - FTimeStatStart.FullTime;
    if FCubeState = fcst_Active then
      DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_AppendedData]), [chc_AppendedData]));
  end;
end;

function TfcxCube.LinkFields: Boolean;
var
  i: integer;
  AGoodFieldsCount: integer;
begin
  AGoodFieldsCount := 0;
  for i := 0 to FDataSource.Fields.Count - 1 do
    if FDataSource.Fields[i].DataField.Initialized then
      if FFields.LinkDataSourceField(FDataSource.Fields[i]) >= 0 then
        inc(AGoodFieldsCount);
  Result := AGoodFieldsCount > 0;
end;

procedure TfcxCube.LoadAppendingData;
var
  ASourceRec: PfcxCUVArray;
  i, RecNo: integer;
begin
  FTimeStatStart.DBMoveTime := fcxGetTickCount;
  DataSource.DataSet.First;
  InternalOnProgressStart(fcxpFetchingData);
  RecNo := 0;
  while not DataSource.DataSet.Eof do
  begin
    InternalOnProgress(fcxpFetchingData, RecNo);
    FTimeStat.DBMoveTime := FTimeStat.DBMoveTime + fcxGetTickCount - FTimeStatStart.DBMoveTime;
    ASourceRec := FSourceHolder.AddRecord;
    for i := 0 to FFields.Count - 1 do
      with TfcxCommonUVField(FFields.Items[i]) do
        if not CalculateAfterAll and Assigned(FDataLoader) then
          FDataLoader.AddValue(ASourceRec[i], nil);
    FTimeStatStart.DBMoveTime := fcxGetTickCount;
    DataSource.DataSet.Next;
    inc(RecNo);
  end;
  InternalOnProgressStop(fcxpFetchingData);
  FTimeStat.DBMoveTime := FTimeStat.DBMoveTime + fcxGetTickCount - FTimeStatStart.DBMoveTime;

  for i := 0 to FFields.Count - 1 do
  begin
    with TfcxCommonUVField(FFields.Items[i]) do
      if not CalculateAfterAll then
      begin
//        TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
        TfcxHackBaseUniqueValues(UniqueValues).AppendAttributes(False);
        UniqueValues.Recapacity;
        if UniqueValues.Sort(False) then
          UniqueValues.SetIndex;
      end;
  end;

  FSourceHolder.Recapacity;
  FCubeState := fcst_Active;
end;

procedure TfcxCube.InternalAppendCubeFromFile(AFileName: String);
function IsRelativePath(S: String): Boolean;
begin
  Result :=
    (Length(S) > 0) and
    (
      (S[1] = '.') or     // relative path '.' or '..'
      (
        (S[1] <> '\') and // not network drive
        (S[2] <> ':')     // nor drive letter
      )
    );
end;

function ExpandFileName(S, CurrentDir: String): String;
var
  RealCurrentDir: String;
begin
  RealCurrentDir := GetCurrentDir;
  if RealCurrentDir <> CurrentDir then
    SetCurrentDir(CurrentDir);
{$IFNDEF FMX}
  Result := SysUtils.ExpandFileName(S);
{$ELSE}
  Result := System.SysUtils.ExpandFileName(S);
{$ENDIF}
  if RealCurrentDir <> CurrentDir then
    SetCurrentDir(RealCurrentDir);
end;

var
  AFullFileName: String;
  ACubeStream: TStream;
begin
  AFullFileName := AFileName;
  if IsRelativePath(ExtractFileDir(AFullFileName)) then
    AFullFileName := ExpandFileName(AFullFileName, ExtractFileDir(ParamStr(0)));
  try
    ACubeStream := TFileStream.Create(AFullFileName, fmOpenRead or fmShareDenyNone);
    try
      InternalAppendCubeFromStream(ACubeStream);
    finally
      FreeAndNil(ACubeStream);
    end;
  except
// file is not exists
    FCubeState := fcst_NotActive;
  end;
end;

procedure TfcxCube.InternalAppendCubeFromDeCompStream(
  ACubeStream: TStream);
var
  AVersion: array[0..6] of AnsiChar;
  ATempStr: array[0..1] of AnsiChar;
  ATempChar: AnsiChar;
//  AMajorVersion, AMinorVersion: byte;
  i, j, {AMasterFieldIndex, }AInitRecordsCount: integer;
//  ANeedLoadAddon: boolean;
//  AMasterFieldName: TfcxString;
  ASourceRecord: TfcxSourceRecord;
begin
//  ANeedLoadAddon := False;
  ACubeStream.Read(AVersion, Length(CubeStreamSignature)); // Read cube version
  if (AVersion = CubeStreamSignature) then
  begin
// only current version allowed
    ACubeStream.Read(ATempStr, 2);
//    Val(ATempStr, AMajorVersion, i);
    ACubeStream.Read(ATempChar, 1);
    ACubeStream.Read(ATempStr, 2);
//    Val(ATempStr, AMinorVersion, i);
  end
  else
  begin
    Exit;
  end;
  {FCaption := }ReadfcString(ACubeStream);
  {FDescription := }ReadfcString(ACubeStream);
// Check Fields
  if not Fields.CheckFieldsFromStream(ACubeStream) then
    Exit;
    

// Unique Values
  inc(FChangeSemaphore);

  Fields.AppendUVsFromStream(ACubeStream);

  dec(FChangeSemaphore);

// Source Holder
  AInitRecordsCount := FSourceHolder.RecordsCount;
  FSourceHolder.AppendFromStream(ACubeStream);

  for i := 0 to FFields.Count - 1 do
  begin
    with TfcxCommonUVField(FFields.Items[i]) do
      if not CalculateAfterAll then
      begin
        if UniqueValues.Sort(False) then
          UniqueValues.SetIndex;
        UniqueValues.Recapacity;
      end;
  end;

  if Fields.ExistsNotSaved then
  begin
// Create UVs for not Saved fields
    inc(FChangeSemaphore);
    if Fields.ExistsNotSaved(True) then
    begin
{
      for i := 0 to FFields.Count - 1 do
        with TfcxCommonUVField(FFields.Items[i]) do
          if not CalculateAfterAll and not Saved then
            UniqueValues.Loading := True;
}
      ASourceRecord := TfcxSourceRecord.Create(Self, FFields.Count);
      for j := AInitRecordsCount to FSourceHolder.RecordsCount - 1 do
      begin
        ASourceRecord.FRecNo := j;
        for i := 0 to FFields.Count - 1 do
          with TfcxCommonUVField(FFields.Items[i]) do
            if not CalculateAfterAll and not Saved then
              FDataLoader.AddValue(FSourceHolder.SourceRec[j][i], ASourceRecord);
      end;
      ASourceRecord.Free;

      for i := 0 to FFields.Count - 1 do
        with TfcxCommonUVField(FFields.Items[i]) do
          if not CalculateAfterAll and not Saved then
          begin
{??
            UniqueValues.Sort(True);
            UniqueValues.SetIndex;
            UniqueValues.Loading := False;
            TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
}
            UniqueValues.Recapacity;
          end;
    end;
{??
    for i := 0 to FFields.Count - 1 do
      TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).FillNonSavedUVs;
}
// Fields CalculateAfterAll
    if Fields.ExistsNotSaved(True) then
    begin
{
      for i := 0 to FFields.Count - 1 do
        with TfcxCommonUVField(FFields.Items[i]) do
          if CalculateAfterAll and not Saved then
            UniqueValues.Loading := True;
}
      ASourceRecord := TfcxSourceRecord.Create(Self, FFields.Count);
      for j := AInitRecordsCount to FSourceHolder.RecordsCount - 1 do
      begin
        ASourceRecord.FRecNo := j;
        for i := 0 to FFields.Count - 1 do
          with TfcxCommonUVField(FFields.Items[i]) do
            if CalculateAfterAll and not Saved then
              FDataLoader.AddValue(FSourceHolder.SourceRec[j][i], ASourceRecord);
      end;
      ASourceRecord.Free;

      for i := 0 to FFields.Count - 1 do
        with TfcxCommonUVField(FFields.Items[i]) do
          if CalculateAfterAll and not Saved then
          begin
{
            UniqueValues.Sort(True);
            UniqueValues.SetIndex;
            UniqueValues.Loading := False;
            TfcxHackBaseUniqueValues(UniqueValues).SetCaptionAndGroupCapacity;
}
            UniqueValues.Recapacity;
          end;

    end;
    dec(FChangeSemaphore);
  end;
{
// Load Groups for not Saved fields
  if AMinorVersion >= 2 then
    LoadGroupsForNotSavedFieldsFromStream(ACubeStream);

  FSlicesManager.FOldVersion := False;
  FSlicesManager.FStreamFromCube := TMemoryStream.Create;
  FSlicesManager.FMajorVersion := AMajorVersion;
  FSlicesManager.FMinorVersion := AMinorVersion;
  ReadStreamFromPosition(ACubeStream, FSlicesManager.FStreamFromCube);
}
  Fields.DeleteLoaders;
  FSourceHolder.Recapacity;
  FCubeState := fcst_Active;
end;

procedure TfcxCube.InternalAppendCubeFromStream(ACubeStream: TStream);
var
  ADeCompStream: TDecompressionStream;
  AVersion: array[0..6] of AnsiChar;
  ASavePosition: integer;
  P: PAnsiChar;
begin
  GetMem(P, Length(CubeFileSignature) + 1);
  try
    ACubeStream.Read(P^, Length(CubeFileSignature));
    P[4] := #0;
    if (AnsiString(P) = CubeFileSignature) or (AnsiString(P) = CubeFileOldSignature) then
    begin
      FTimeStat.Clear;
      FTimeStatStart.FullTime := fcxGetTickCount;
      FCubeState := fcst_AppendingData;
      try
    //    FreeAndNil(FSourceHolder);
    //    FFields.Clear;
        if ACubeStream.Position < ACubeStream.Size then
        begin
    // Check Compression
          ASavePosition := ACubeStream.Position;
          ACubeStream.Read(AVersion, Length(CubeStreamSignature)); // Read cube version
          ACubeStream.Position := ASavePosition;
          if (AVersion = CubeStreamSignature) or (AVersion = CubeStreamOldSignature) then
          begin
            InternalAppendCubeFromDeCompStream(ACubeStream);
          end
          else
          begin
            ADeCompStream := TDecompressionStream.Create(ACubeStream);
            try
              InternalAppendCubeFromDeCompStream(ADeCompStream);
            finally
              ADeCompStream.Free;
            end; (*try*)
          end
        end;
        if FSourceHolder = nil then
          FSourceHolder := TfcxSourceHolder.Create(Self);
      finally
        if FCubeState = fcst_AppendingData then
          FCubeState := fcst_NotActive;
        FTimeStat.FullTime := FTimeStat.FullTime + fcxGetTickCount - FTimeStatStart.FullTime;
        if FCubeState = fcst_Active then
          DoChange(TfcxCubeChangeAlert.Create(CubeChangeTypeOfChanges([chc_AppendedData]), [chc_AppendedData]));
      end;
    end;
  finally
    FreeMem(P);
  end;
end;

function TfcxCube.AppendFromFile(ACubeFileName: String): Boolean;
var
  AOldCubeSource: TfcxCubeSource;
  AOldCubeFile: String;
begin
  AOldCubeSource := CubeSource;
  AOldCubeFile := CubeFile;
  try
//    SetActive(False);
    CubeSource := fccs_CubeFile;
    CubeFile := ACubeFileName;
    AppendData;
//    SetActive(True);
  finally
    CubeSource := AOldCubeSource;
    CubeFile := AOldCubeFile;
  end;
  Result := Active;
end;

function TfcxCube.AppendFromStream(ACubeStream: TStream): Boolean;
var
  AOldCubeSource: TfcxCubeSource;
begin
  AOldCubeSource := CubeSource;
  try
//    SetActive(False);
    CubeSource := fccs_CubeStream;
    FCubeStream := ACubeStream;
    AppendData;
//    SetActive(True);
  finally
    CubeSource := AOldCubeSource;
    FCubeStream := Nil;
  end;
  Result := Active;
end;

procedure TfcxCube.InternalOnProgress(ProgressType: TfcxProgressType; Progress: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, ProgressType, Progress);
end;

procedure TfcxCube.InternalOnProgressStart(ProgressType: TfcxProgressType);
begin
  if Assigned(FOnProgressStart) then
    FOnProgressStart(Self, ProgressType, 0);
end;

procedure TfcxCube.InternalOnProgressStop(ProgressType: TfcxProgressType);
begin
  if Assigned(FOnProgressStop) then
    FOnProgressStop(Self, ProgressType, 0);
end;

function TfcxCube.LoadGroupsFromStream(AStream: TStream): Boolean;
const
  XMLSignature: array[0..1] of AnsiChar = '<?';
var
  AVersion: Word;
  AXMLDoc: TfcxXMLDocument;
begin
  Result := False;
  if AStream.Read(AVersion, SizeOf(Word)) < SizeOf(Word) then
    Exit;
  if AVersion = word(XMLSignature) then
  begin
    // this is XML and should be read throug LoadFromXML
    AXMLDoc := TfcxXMLDocument.Create;
    try
      AStream.Position := AStream.Position - 2;
      AXMLDoc.LoadFromStream(AStream);
      Result := LoadGroupsFromXML(AXMLDoc);
    finally
      AXMLDoc.Free;
    end;
  end
end;

procedure TfcxCube.SaveGroupsToStream(AStream: TStream);
var
  Doc: TfcxXMLDocument;
begin
  Doc := TfcxXMLDocument.Create;
  Doc.AutoIndent := True;
  try
    SaveGroupsToXML(Doc);
    Doc.SaveToStream(AStream);
  finally
    Doc.Free;
  end;
end;

{ TfcxFields }

function TfcxFields.AddCField(
  ACFieldProperties: TfcxCFieldProperties): Integer;
begin
  Result := Count;
//  if not Find(AField.FieldName, Result) then
  Insert(Result, TfcxCalcField.Create(FCube, ACFieldProperties));
  Items[Result].FIndex := Result;
end;

function TfcxFields.AddStreamField(
  AFieldProperties: TfcxFieldProperties): Integer;
begin
  Result := Count;
//  if not Find(AField.FieldName, Result) then
  Insert(Result, TfcxUVField.Create(FCube, AFieldProperties));
  Items[Result].FIndex := Result;
end;

constructor TfcxFields.Create(ACube: TfcxCube);
begin
  inherited Create;
  FCube := ACube;
  AutoFree := True;
end;

procedure TfcxFields.DeleteField(AFieldName: TfcxString);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if fcStrCompare(Items[i].CubeFieldName, AFieldName) = 0 then
    begin
      DeleteField(i);
      exit;
    end;
end;

procedure TfcxFields.DeleteField(AFieldIndex: integer);
begin
  Delete(AFieldIndex);
  ReNum;
end;

function TfcxFields.ExistsNotSaved(ANotCheckChildren: Boolean = False): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if not Items[i].Saved then
    begin
      Result := True;
      Exit;
    end;
    if not ANotCheckChildren and Assigned(Items[i].Fields) and Items[i].Fields.ExistsNotSaved then
    begin
      Result := True;
      Exit;
    end;
  end
end;

function TfcxFields.FieldIndex(AFieldName: TfcxString): Integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if fcStrCompare(Items[i].CubeFieldName, AFieldName) = 0 then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TfcxFields.GetItem(Index: Integer): TfcxCommonField;
begin
  Result := List[Index];
end;

function TfcxFields.GetItemAtName(Index: TfcxString): TfcxCommonField;
var
  AIndex: integer;
begin
  AIndex := FieldIndex(Index);
  if AIndex = -1 then
    Result := nil
  else
    Result := List[AIndex];
end;

procedure TfcxFields.LoadFieldFromXMLItem(AItem: TfcxXMLItem);
var
  AFieldIndex: integer;
  ACFieldsProperties: TfcxCFieldProperties;
begin
  ACFieldsProperties.FieldProperties := TfcxCommonField.LoadPropsFromXMLItem(AItem);
  if AItem.PropExists('CalcType') then
  begin
    ACFieldsProperties.CFieldType := TfcxCFieldType(GetEnumValue(TypeInfo(TfcxCFieldType), AItem.Prop['CalcType']));
    ACFieldsProperties.CalcString := AItem.Prop['SystemText'];
    AFieldIndex := AddCField(ACFieldsProperties)
  end
  else
  if Self is TfcxAttributeFields then
    if AItem.PropExists('DateType') then
      AFieldIndex := TfcxAttributeFields(Self).AddDatePathField(ACFieldsProperties.FieldProperties, TfcxDateType(GetEnumValue(TypeInfo(TfcxDateType), AItem.Prop['DateType'])))
    else
    if AItem.PropExists('TimeType') then
      AFieldIndex := TfcxAttributeFields(Self).AddTimePathField(ACFieldsProperties.FieldProperties, TfcxTimeType(GetEnumValue(TypeInfo(TfcxTimeType), AItem.Prop['TimeType'])))
    else
    if AItem.PropExists('AttributeType') then
      AFieldIndex := TfcxAttributeFields(Self).AddAttributeStreamField(ACFieldsProperties.FieldProperties, TfcxAttributeType(GetEnumValue(TypeInfo(TfcxAttributeType), AItem.Prop['AttributeType'])))
    else
      AFieldIndex := TfcxAttributeFields(Self).AddAttributeStreamField(ACFieldsProperties.FieldProperties, fcxsft_Reference)
  else
    AFieldIndex := AddStreamField(ACFieldsProperties.FieldProperties);
  Items[AFieldIndex].LoadFromXMLItem(AItem);
end;

procedure TfcxFields.LoadFromXMLItem(AItem: TfcxXMLItem);
var
  i: integer;
begin
  for i := 0 to AItem.Count - 1 do
  begin
    LoadFieldFromXMLItem(AItem[i]);
    if AItem[i].Count > 0 then
      Items[i].Fields.LoadFromXMLItem(AItem[i][0]);
    LoadFieldFromXMLItemStep2(AItem[i]);
  end;
end;

procedure TfcxFields.LoadUVsFromStream(ACubeStream: TStream);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].LoadUVsFromStream(ACubeStream);
end;

procedure TfcxFields.DeleteLoaders;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].DataLoaderType := dlt_None;
    if Items[i] is TfcxUVField then
    begin
      TfcxUVField(Items[i]).UniqueValues.RemoveLinksToDataSource;
      FreeMem(TfcxHackBaseUniqueValues(TfcxUVField(Items[i]).UniqueValues).FTempList);
      TfcxHackBaseUniqueValues(TfcxUVField(Items[i]).UniqueValues).FTempList := nil;
      TfcxHackBaseUniqueValues(TfcxUVField(Items[i]).UniqueValues).FTempCount := 0;
      TfcxHackBaseUniqueValues(TfcxUVField(Items[i]).UniqueValues).FAddedUValues := False;
      FreeMem(TfcxHackBaseUniqueValues(TfcxUVField(Items[i]).UniqueValues).FIndexList);
      TfcxHackBaseUniqueValues(TfcxUVField(Items[i]).UniqueValues).FIndexList := nil;
    end;
    if Assigned(Items[i].Fields) then
      Items[i].Fields.DeleteLoaders;
  end;
end;

procedure TfcxFields.ReNum;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FIndex := i;
end;

procedure TfcxFields.SaveToXMLItem(AItem: TfcxXMLItem);
var
  I: Integer;
  AItem2: TfcxXMLItem;
begin
  AItem.Name := 'Fields';
  if Count = 0 then
    AItem.Free
  else
    for I := 0 to Count - 1 do
    begin
      AItem2 := AItem.Add;
      Items[I].SaveToXMLItem(AItem2);
      if Items[I].Fields <> nil then
        Items[I].Fields.SaveToXMLItem(AItem2.Add);
    end;
end;

procedure TfcxFields.SaveUVsToStream(ACubeStream: TStream);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].SaveUVsToStream(ACubeStream);
end;

function TfcxFields.CheckFieldFromXMLItem(AItem: TfcxXMLItem; AFieldIndex: integer): Boolean;
var
  ACFieldsProperties: TfcxCFieldProperties;
begin
  ACFieldsProperties.FieldProperties := TfcxCommonField.LoadPropsFromXMLItem(AItem);
  if AItem.PropExists('CalcType') then
  begin
    ACFieldsProperties.CFieldType := TfcxCFieldType(GetEnumValue(TypeInfo(TfcxCFieldType), AItem.Prop['CalcType']));
    ACFieldsProperties.CalcString := AItem.Prop['SystemText'];
    Result := CheckCField(ACFieldsProperties, AFieldIndex)
  end
  else
  if Self is TfcxAttributeFields then
    if AItem.PropExists('DateType') then
      Result := TfcxAttributeFields(Self).CheckDatePathField(ACFieldsProperties.FieldProperties, TfcxDateType(GetEnumValue(TypeInfo(TfcxDateType), AItem.Prop['DateType'])), AFieldIndex)
    else
    if AItem.PropExists('TimeType') then
      Result := TfcxAttributeFields(Self).CheckTimePathField(ACFieldsProperties.FieldProperties, TfcxTimeType(GetEnumValue(TypeInfo(TfcxTimeType), AItem.Prop['TimeType'])), AFieldIndex)
    else
    if AItem.PropExists('AttributeType') then
      Result := TfcxAttributeFields(Self).CheckAttributeStreamField(ACFieldsProperties.FieldProperties, TfcxAttributeType(GetEnumValue(TypeInfo(TfcxAttributeType), AItem.Prop['AttributeType'])), AFieldIndex)
    else
      Result := TfcxAttributeFields(Self).CheckAttributeStreamField(ACFieldsProperties.FieldProperties, fcxsft_Reference, AFieldIndex)
  else
    Result := CheckStreamField(ACFieldsProperties.FieldProperties, AFieldIndex);
//??  Items[AFieldIndex].LoadFromXMLItem(AItem);
end;

function TfcxFields.CheckFromXMLItem(AItem: TfcxXMLItem): Boolean;
var
  i: integer;
begin
  Result := AItem.Count = Count;
  if Result then
    for i := 0 to AItem.Count - 1 do
    begin
      Result := CheckFieldFromXMLItem(AItem[i], i);
      if not Result then
        exit;
      if AItem[i].Count > 0 then
      begin
        Result := Items[i].Fields.CheckFromXMLItem(AItem[i][0]);
        if not Result then
          exit;
      end
    end;
end;

function TfcxFields.CheckCField(ACFieldProperties: TfcxCFieldProperties;
  AFieldIndex: integer): Boolean;
begin
  Result := (Items[AFieldIndex] is TfcxCalcField) and TfcxCalcField(Items[AFieldIndex]).CheckField(ACFieldProperties);
end;

function TfcxFields.CheckStreamField(AFieldProperties: TfcxFieldProperties;
  AFieldIndex: integer): Boolean;
begin
  Result := (Items[AFieldIndex] is TfcxUVField) and TfcxUVField(Items[AFieldIndex]).CheckField(AFieldProperties);
end;

procedure TfcxFields.AppendUVsFromStream(ACubeStream: TStream);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AppendUVsFromStream(ACubeStream);
end;

procedure TfcxFields.LoadFieldFromXMLItemStep2(AItem: TfcxXMLItem);
begin
  Items[Count - 1].LoadFromXMLItemStep2(AItem);
end;

{ TfcxSourceHolder }

function TfcxSourceHolder.AddRecord: PfcxCUVArray;
begin
  if FRecordsCount = FCapacity then
  begin
    inc(FCapacity, 10000); // change to make this faster
    ReallocMem(FSourceList, FCapacity * fcPointerSize);
  end;
  Result := AllocMem(FBaseFieldsCount * fcPointerSize);
  FSourceList[FRecordsCount] := Result;
  inc(FRecordsCount);
end;

procedure TfcxSourceHolder.AddField(AIndex: integer);
var
  i: integer;
begin
  if AIndex > FBaseFieldsCount then
    AIndex := FBaseFieldsCount;
  inc(FBaseFieldsCount);
  for i := 0 to FRecordsCount - 1 do
  begin
    ReallocMem(FSourceList[i], FBaseFieldsCount * fcPointerSize);
    System.Move(FSourceList[i][AIndex], FSourceList[i][AIndex + 1],
      (FBaseFieldsCount - AIndex - 1) * fcPointerSize);
    FSourceList[i][AIndex] := nil;
  end;
end;

procedure TfcxSourceHolder.AddFields(ACount: integer);
var
  i, j: integer;
begin
  if ACount <= 0 then
    Exit;
  FBaseFieldsCount := FBaseFieldsCount + ACount;
  for i := 0 to FRecordsCount - 1 do
  begin
    ReallocMem(FSourceList[i], FBaseFieldsCount * fcPointerSize);
    for j := FBaseFieldsCount - ACount to FBaseFieldsCount - 1 do
      FSourceList[i][j] := nil;
  end;
end;

procedure TfcxSourceHolder.AddRecords(ACount: integer);
var
  i: integer;
begin
  if FRecordsCount = FCapacity then
  begin
    inc(FCapacity, max(10000, ACount)); // change to make this faster
    ReallocMem(FSourceList, FCapacity * fcPointerSize);
  end;
  for i := 0 to ACount - 1 do
  begin
    FSourceList[FRecordsCount + i] := AllocMem(FBaseFieldsCount * fcPointerSize);
  end;
  FRecordsCount := FRecordsCount + ACount;
end;

constructor TfcxSourceHolder.Create(ACube: TfcxCube);
begin
  FCube := ACube;
  FBaseFieldsCount := FCube.FFields.Count;
  FCapacity := 0;
  FRecordsCount := 0;
  FSourceList := nil;
end;

procedure TfcxSourceHolder.DeleteField(AIndex: integer);
var
  i: integer;
begin
  if AIndex >= FBaseFieldsCount then
    exit;
  dec(FBaseFieldsCount);
  for i := 0 to FRecordsCount - 1 do
  begin
    System.Move(FSourceList[i][AIndex + 1], FSourceList[i][AIndex],
      (FBaseFieldsCount - AIndex) * fcPointerSize);
    ReallocMem(FSourceList[i], FBaseFieldsCount * fcPointerSize);
  end;
end;

destructor TfcxSourceHolder.Destroy;
var
  i: integer;
begin
  for i := 0 to FRecordsCount - 1 do
    FreeMem(FSourceList[i]);
  FreeMem(FSourceList);
  inherited;
end;

function TfcxSourceHolder.GetSourceRec(ARow: integer): PfcxCUVArray;
begin
  Result := FSourceList[ARow];
end;

function TfcxSourceHolder.GetUniqueValueAsVariant(ARow: integer;
  AField: TfcxCommonField): Variant;
begin
  Result := AField.ValueAsVariantAtIndex[UniqueValueIndex[ARow, AField]]
end;

function TfcxSourceHolder.GetUniqueValueCaption(ARow: integer; AField: TfcxCommonField): TfcxString;
begin
{$IFDEF TRIAL}
  if Random(3) = 1 then
    Result := TrialString
  else
{$ENDIF}
  Result := AField.CaptionValueAtIndex[UniqueValueIndex[ARow, AField]]
end;

function TfcxSourceHolder.GetUniqueValueIndex(ARow: integer;
  AField: TfcxCommonField): integer;
var
  i: integer;
begin
  if AField.Level = 0 then
    Result := FSourceList[ARow][AField.Index].Index
  else
  begin
    Result := FSourceList[ARow][PfcxCommonUVFieldArray(AField.FMasterFieldsMap)[0].Index].Index;
    for i := 1 to AField.Level - 1 do
      Result := PfcxCommonUVFieldArray(AField.FMasterFieldsMap)[i].MasterField.UniqueValues.SplitManager.AttributesManager.AttributeUVIndex[PfcxCommonUVFieldArray(AField.FMasterFieldsMap)[i].AttributeIndex, Result];
    if AField is TfcxCommonUVField then
      Result := AField.MasterField.UniqueValues.SplitManager.AttributesManager.AttributeUVIndex[AField.AttributeIndex, Result]
    else
    if AField is TfcxCommonDatePathField then
      Result := TfcxCommonDatePathField(AField).DatePathProcessor.IndexAtBaseUVIndex[Result]
    else
    if AField is TfcxCommonTimePathField then
      Result := TfcxCommonTimePathField(AField).TimePathProcessor.IndexAtBaseUVIndex[Result];
  end
end;

function TfcxSourceHolder.GetUniqueValueIndexAndAsVariant(ARow: integer;
  AField: TfcxCommonField; var AVariantValue: Variant): Integer;
begin
  Result := UniqueValueIndex[ARow, AField];
  AVariantValue := AField.ValueAsVariantAtIndex[Result];
end;

procedure TfcxSourceHolder.LoadFromStream(ACubeStream: TStream);
var
  ACount, i, j, l: integer;
  AIntArray: PfcxIntegerArray;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  AddRecords(ACount);
  l := SizeOf(Integer) * FBaseFieldsCount;
  GetMem(AIntArray, l);
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(AIntArray[0], l);
    for j := 0 to FBaseFieldsCount - 1 do
      FSourceList[i][j] := TfcxBaseUniqueValues(TfcxCommonUVField(FCube.FFields.Items[j]).UniqueValues).UValue[AIntArray[j]];
  end;
  FreeMem(AIntArray);
end;

procedure TfcxSourceHolder.Recapacity;
begin
  FCapacity := FRecordsCount;
  ReallocMem(FSourceList, FCapacity * fcPointerSize);
end;

procedure TfcxSourceHolder.SaveToStream(ACubeStream: TStream);
var
  i, j: integer;
  AIntArray: PfcxIntegerArray;
begin
  GetMem(AIntArray, SizeOf(Integer) * FBaseFieldsCount);
  if Assigned(FCube.FSaveFilter) then
  begin
    ACubeStream.Write(TfcxFilterManager(FCube.FSaveFilter).GoodRecCount, SizeOf(integer));
    for i := 0 to FRecordsCount - 1 do
    begin
      if not TfcxFilterManager(FCube.FSaveFilter).RecFilter[i] then
      begin
        for j := 0 to FBaseFieldsCount - 1 do
          AIntArray[j] := FSourceList[i][j].Index;
        ACubeStream.Write(AIntArray[0], SizeOf(Integer) * FBaseFieldsCount);
      end;
    end;
  end
  else
  begin
    ACubeStream.Write(FRecordsCount, SizeOf(integer));
    for i := 0 to FRecordsCount - 1 do
    begin
      for j := 0 to FBaseFieldsCount - 1 do
        AIntArray[j] := FSourceList[i][j].Index;
      ACubeStream.Write(AIntArray[0], SizeOf(Integer) * FBaseFieldsCount);
    end;
  end;
  FreeMem(AIntArray);
end;

procedure TfcxSourceHolder.AppendFromStream(ACubeStream: TStream);
var
  ACount, i, j, l, AInitCount: integer;
  AIntArray: PfcxIntegerArray;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  AInitCount := FRecordsCount;
  AddRecords(ACount);
  l := SizeOf(Integer) * FBaseFieldsCount;
  GetMem(AIntArray, l);
  for i := AInitCount to AInitCount + ACount - 1 do
  begin
    ACubeStream.Read(AIntArray[0], l);

    for j := 0 to FBaseFieldsCount - 1 do
      FSourceList[i][j] := TfcxHackBaseUniqueValues(TfcxCommonUVField(FCube.FFields.Items[j]).UniqueValues).FTempList[AIntArray[j]];
  end;
  FreeMem(AIntArray);
end;

{ TfcxCommonField }

constructor TfcxCommonField.Create(ACube: TfcxCube);
begin
  if FMasterField = nil then
  begin
    FMasterFieldsMap := nil;
    FLevel := 0;
  end;
  FCube := ACube;
  FFields := nil;
  FCubeFieldName := '';
  FCubeFieldDisplayLabel := '';
  FDataType := fcdt_Error;
  FSaved := False;
  DataLoaderType := dlt_None;
{ TODO -cНеобходимо : Брать значения по умолчании из куба. Некоторые значения зависят от типа данных.}
  FCaseSensitive := True;
  FNullStr := fcxResources.Get('sNull');
end;

destructor TfcxCommonField.Destroy;
begin
  FreeMem(OldSavedCubeField);
  FreeMem(FMasterFieldsMap);
  FDataLoader.Free;
  inherited;
end;

{ TODO -cНеобходимо : Надо переделать наверно на хранение индекса, иначе устанет постоянно искать.}
{
function TfcxCommonField.GetIndex: Integer;
begin
  Result := FCube.Fields.FieldIndex(Self);
end;
}

function TfcxCommonField.GetCaptionValueAtIndex(Index: integer): TfcxString;
begin
  Result := '';
end;

function TfcxCommonField.GetAttribute(Index: TfcxString): TfcxCommonField;
begin
  Result := FFields.ItemsAtName[Index];
end;

function TfcxCommonField.GetfcxVarType: TfcxVarType;
begin
  case DataTypeToVarType[FDataType] of
    varSmallInt, varInteger, varBoolean, {$IFDEF DELPHI_6UP} varShortInt, varInt64, varLongWord, varWord, {$ENDIF}
    varByte:
      Result := fcvtOrdinal;
    varDate:
      Result := fcvtDate;
    varSingle, varDouble, varCurrency:
      Result := fcvtFloat;
  else
    Result := fcvtString;
  end;
end;

class function TfcxCommonField.ReadOldFieldBaseProps(
  ACubeStream: TStream; AMajorVersion, AMinorVersion: integer): TfcxOldSavedCubeField;
var
  B: Boolean;
  Bt: Byte;
begin
  Result.FFormatKind := fkText; // default

  Result.FFieldName := ReadOldString(ACubeStream);
  Result.FCaption := ReadOldString(ACubeStream);
  if (AMajorVersion > 1) or (AMinorVersion >= 5) then
  begin
    ACubeStream.Read(Result.FPrecision, SizeOf(Integer));
  end
  else
  begin
    ReadOldString(ACubeStream);
    Result.FPrecision := 2;
  end;
  // FPC compability. SizeOf(TFieldType) = 1 in delphy and 4 in fpc
  ACubeStream.Read(Bt, SizeOf(Bt));
  Result.FDataType := TFieldType(Bt);
  if (AMajorVersion = 1) and (AMinorVersion <= 5) then
    ACubeStream.Read(B, SizeOf(B));
  ACubeStream.Read(Bt, SizeOf(Bt));
  Result.FDateType := TfcxDateType(Bt);

  if (AMajorVersion > 3) or ((AMajorVersion = 3) and (AMinorVersion > 2)) then
  begin
    ACubeStream.Read(Bt, SizeOf(Bt));
    Result.FTimeType := TfcxTimeType(Bt);
  end
  else
    Result.FTimeType := ott_None;
  //ACubeStream.Read(Result.FDateType, SizeOf(Result.FDateType));
  if ((AMajorVersion = 1) and (AMinorVersion > 1)) or
    ((AMajorVersion = 2) and (AMinorVersion < 1)) then
    ReadOldString(ACubeStream);
  if (AMajorVersion > 2) or ((AMajorVersion = 2) and (AMinorVersion > 1)) then
    ACubeStream.Read(Result.FDataSize, SizeOf(Result.FDataSize));
  if (AMajorVersion > 3) or  ((AMajorVersion = 3) and (AMinorVersion > 3))  then
    ACubeStream.Read(Result.FIndexInDataset, SizeOf(Integer))
  else
    Result.FIndexInDataset := -1;
  Result.FUseDefaultFormat := True;
  Result.FUseFormatStr := False;
  if (AMajorVersion > 3) or ((AMajorVersion = 3) and (AMinorVersion > 0)) then
  begin
    Result.FFormatStr := ReadOldString(ACubeStream);
    Result.FUseFormatStr := True;
  end
  else
  if Result.FDataType in [ftFloat, ftCurrency, ftBCD{$IFDEF SQL_TYPES_EXTRA0}, ftFMTBcd{$ENDIF}] then
  begin
{
    // guess format
    Result.FFormatKind := fkNumeric;
    Result.FDecSeparator := DecimalSeparator;
    Result.FFormatStr := '#,##0';
    if Result.FPrecision > 0 then
      Result.FFormatStr := Result.FFormatStr + '.';
    for i := 0 to Result.FPrecision - 1 do
      Result.FFormatStr := Result.FFormatStr + '0';
    Result.FUseDefaultFormat := False;
}
  end;
  if (AMajorVersion > 3) or ((AMajorVersion = 3) and (AMinorVersion > 1)) then
  begin
    Result.FFormatKind := TfcxFormatKind(ReadByte(ACubeStream));
    Result.FDecSeparator := ReadOldString(ACubeStream);
    Result.FUseDefaultFormat := False;
(*
    if mdcoLoadWithDefaultFormat in Options  then
    begin
      case Result.FDataType of
        ftAutoInc, ftInteger, ftWord, ftSmallInt,
        ftBoolean{$IFDEF SQL_TYPES_EXTRA0}, ftLargeInt{$ENDIF}:
          begin
            Result.FFormatKind := DefaultFormat.IntegerFormat.Kind;
            Result.FFormatStr := DefaultFormat.IntegerFormat.FormatStr;
            Result.FDecSeparator := DefaultFormat.IntegerFormat.DecSeparator;
          end;
        ftFloat, ftBCD{$IFDEF SQL_TYPES_EXTRA0}, ftFMTBcd{$ENDIF}:
          begin
            Result.FFormatKind := DefaultFormat.FloatFormat.Kind;
            Result.FFormatStr := DefaultFormat.FloatFormat.FormatStr;
            Result.FDecSeparator := DefaultFormat.FloatFormat.DecSeparator;
          end;
        ftCurrency:
          begin
            Result.FFormatKind := DefaultFormat.CurrencyFormat.Kind;
            Result.FFormatStr := DefaultFormat.CurrencyFormat.FormatStr;
            Result.FDecSeparator := DefaultFormat.CurrencyFormat.DecSeparator;
          end;
        ftDate:
          begin
            Result.FFormatKind := DefaultFormat.DateFormat.Kind;
            Result.FDecSeparator := DefaultFormat.DateFormat.DecSeparator;
            Result.FFormatStr := DefaultFormat.DateFormat.FormatStr;
          end;
        ftTime:
          begin
            Result.FFormatKind := DefaultFormat.TimeFormat.Kind;
            Result.FDecSeparator := DefaultFormat.TimeFormat.DecSeparator;
            Result.FFormatStr := DefaultFormat.TimeFormat.FormatStr;
          end;
        ftDateTime{$IFDEF SQL_TYPES_EXTRA0}, ftTimeStamp{$ENDIF}:
          begin
            Result.FFormatKind := DefaultFormat.DateTimeFormat.Kind;
            Result.FDecSeparator := DefaultFormat.DateTimeFormat.DecSeparator;
            Result.FFormatStr := DefaultFormat.DateTimeFormat.FormatStr;
          end;
      end;
    end
*)
  end
  else
  if Result.FFormatKind = fkText then
  begin
(*
    case Result.FDataType of
      ftAutoInc, ftInteger, ftWord, ftSmallInt,
      ftBoolean{$IFDEF SQL_TYPES_EXTRA0}, ftLargeInt{$ENDIF}:
        begin
          Result.FFormatKind := DefaultFormat.IntegerFormat.Kind;
          Result.FFormatStr := DefaultFormat.IntegerFormat.FormatStr;
          Result.FDecSeparator := DefaultFormat.IntegerFormat.DecSeparator;
        end;
      ftFloat, ftBCD{$IFDEF SQL_TYPES_EXTRA0}, ftFMTBcd{$ENDIF}:
        begin
          Result.FFormatKind := DefaultFormat.FloatFormat.Kind;
          if Result.FFormatStr = '' then
            Result.FFormatStr := DefaultFormat.FloatFormat.FormatStr;
          if Result.FDecSeparator = '' then
            Result.FDecSeparator := DefaultFormat.FloatFormat.DecSeparator;
        end;
      ftCurrency:
        begin
          Result.FFormatKind := DefaultFormat.CurrencyFormat.Kind;
          if Result.FFormatStr = '' then
            Result.FFormatStr := DefaultFormat.CurrencyFormat.FormatStr;
          if Result.FDecSeparator = '' then
            Result.FDecSeparator := DefaultFormat.CurrencyFormat.DecSeparator;
        end;
      ftDate:
        begin
          Result.FFormatKind := DefaultFormat.DateFormat.Kind;
          Result.FDecSeparator := DefaultFormat.DateFormat.DecSeparator;
//          if Result.FFormatStr = '' then
            Result.FFormatStr := DefaultFormat.DateFormat.FormatStr;
        end;
      ftTime:
        begin
          Result.FFormatKind := DefaultFormat.TimeFormat.Kind;
          Result.FDecSeparator := DefaultFormat.TimeFormat.DecSeparator;
//          if Result.FFormatStr = '' then
            Result.FFormatStr := DefaultFormat.TimeFormat.FormatStr;
        end;
      ftDateTime{$IFDEF SQL_TYPES_EXTRA0}, ftTimeStamp{$ENDIF}:
        begin
          Result.FFormatKind := DefaultFormat.DateTimeFormat.Kind;
          Result.FDecSeparator := DefaultFormat.DateTimeFormat.DecSeparator;
//          if Result.FFormatStr = '' then
            Result.FFormatStr := DefaultFormat.DateTimeFormat.FormatStr;
        end;
    end;
*)
  end;
end;

procedure TfcxCommonField.SetCaseSensitive(const Value: boolean);
begin
  FCaseSensitive := Value;
end;

procedure TfcxCommonField.SetCubeFieldDisplayLabel(const Value: TfcxString);
var
  i: integer;
begin
  if FCubeFieldDisplayLabel <> Value then
  begin
    Cube.StartChange;
    for i := 0 to Cube.SlicesManager.Count - 1 do
      Cube.SlicesManager.Slice[i].UpdateDisplayLabel(Self, Value);
    FCubeFieldDisplayLabel := Value;
    Cube.StopChange([]);
  end;
end;

procedure TfcxCommonField.SetNullStr(const Value: TfcxString);
begin
  FNullStr := Value;
end;

function TfcxCommonField.GetCanGroup: Boolean;
begin
  Result := False;
end;

function TfcxCommonField.GetGroupCaption(AGroupIndex: integer): TfcxString;
begin
  if CanGroup then
    Result := GroupManager.Caption[AGroupIndex]
  else
    Result := '';
end;

function TfcxCommonField.GetGroupCount: Integer;
begin
  if CanGroup then
    Result := GroupManager.GroupCount
  else
    Result := 0;
end;

function TfcxCommonField.GetGroupIndexByOrder(AOrder: integer): integer;
begin
  Result := AOrder;
end;

function TfcxCommonField.GetGroupManager: TfcxCommonGroupManager;
begin
  Result := nil;
end;

function TfcxCommonField.GetGroupUVCount(AGroupIndex: integer): integer;
begin
  if CanGroup then
    Result := GroupManager.CountInGroup[AGroupIndex]
  else
    Result := 0;
end;

function TfcxCommonField.GetGroupUVIndexByOrder(AGroupIndex,
  AOrder: integer): integer;
begin
  if CanGroup then
    Result := GroupManager.UVIndexInGroup[AGroupIndex, AOrder]
  else
    Result := -1;
end;

function TfcxCommonField.GetHasGroups: Boolean;
begin
  if CanGroup then
  begin
{ TODO -cНеобходимо : Надо сделать проверку на пустоту групп}
    Result := GroupManager.GroupCount > 0;
  end
  else
    Result := False;
end;

function TfcxCommonField.GetNonGroupUVCount: integer;
begin
  if CanGroup then
    Result := GroupManager.CountInNonGroups
  else
    Result := 0;
end;

function TfcxCommonField.GetNonGroupUVIndexByOrder(
  AOrder: integer): integer;
begin
  if CanGroup then
    Result := GroupManager.UVIndexInNonGroups[AOrder]
  else
    Result := -1;
end;

function TfcxCommonField.GetUVCaptionInGroup(AGroupIndex,
  AOrder: integer): TfcxString;
begin
  if CanGroup then
    Result := GroupManager.CaptionInGroup[AGroupIndex, AOrder]
  else
    Result := '';
end;

function TfcxCommonField.GetUVCaptionInNonGroups(
  AOrder: integer): TfcxString;
begin
  if CanGroup then
    Result := GroupManager.CaptionInNonGroups[AOrder]
  else
    Result := '';
end;

procedure TfcxCommonField.SetCanGroup(const Value: Boolean);
begin
//
end;

procedure TfcxCommonField.SetGroupCaption(AGroupIndex: integer;
  const Value: TfcxString);
begin
  if CanGroup then
    GroupManager.Caption[AGroupIndex] := Value
end;

function TfcxCommonField.GetValueAsVariantAtIndex(Index: integer): Variant;
begin
  Result := Unassigned;
end;

procedure TfcxCommonField.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  AItem.Name := 'Field';
  AItem.Prop['CubeFieldName'] := FCubeFieldName;
  AItem.Prop['CubeFieldDisplayLabel'] := FCubeFieldDisplayLabel;
  AItem.Prop['NullStr'] := FNullStr;
  AItem.BoolProp['CaseSensitive'] := FCaseSensitive;
  AItem.Prop['DataType'] := GetEnumName(TypeInfo(TfcxDataType), Ord(FDataType));
  AItem.BoolProp['Saved'] := FSaved;
end;

class function TfcxCommonField.LoadPropsFromXMLItem(
  AItem: TfcxXMLItem): TfcxFieldProperties;
begin
  Result.CubeFieldName := AItem.Prop['CubeFieldName'];
  Result.CubeFieldDisplayLabel := AItem.Prop['CubeFieldDisplayLabel'];
  Result.NullStr := AItem.Prop['NullStr'];
// !!! УБРАТЬ
  if AItem.PropExists('CaseSensitive') then
    Result.CaseSensitive := AItem.BoolProp['CaseSensitive']
  else
    Result.CaseSensitive := AItem.BoolProp['SaseSensitive'];
  Result.DataType := TfcxDataType(GetEnumValue(TypeInfo(TfcxDataType), AItem.Prop['DataType']));
// !!! УБРАТЬ
  if AItem.PropExists('Saved') then
    Result.Saved := AItem.BoolProp['Saved']
  else
    Result.Saved := True;
  if AItem.PropExists('CalculateAfterAll') then
    Result.CalculateAfterAll := AItem.BoolProp['CalculateAfterAll']
  else
    Result.CalculateAfterAll := False;
end;

procedure TfcxCommonField.LoadFromXMLItem(AItem: TfcxXMLItem);
begin
//
end;

procedure TfcxCommonField.LoadFromXMLItemStep2(AItem: TfcxXMLItem);
begin
//
end;

function TfcxCommonField.GetDisplayFormat: TfcxFormat;
begin
  Result := DataTypeProcessor.DisplayFormat;
end;

procedure TfcxCommonField.SetDisplayFormat(const Value: TfcxFormat);
begin
  if not DataTypeProcessor.DisplayFormat.Equal(Value) then
    DataTypeProcessor.DisplayFormat.Assign(Value);
end;

procedure TfcxCommonField.SetSaved(const Value: Boolean);
begin
  FSaved := True;
end;

procedure TfcxCommonField.SetDataLoaderType(
  const Value: TfcxDataLoaderType);
var
  AOldDataLoader: TfcxCommonDataLoader;
begin
  if FDataLoaderType <> Value then
  begin
    AOldDataLoader := FDataLoader;
    if Self is TfcxCommonUVField then
    begin
      case Value of
        dlt_None: FDataLoader := nil;
        dlt_DataSource: FDataLoader := TfcxDSDataLoader.Create(TfcxCommonUVField(Self));
        dlt_Stream: FDataLoader := TfcxStreamDataLoader.Create(TfcxCommonUVField(Self));
        dlt_Calculation: FDataLoader := TfcxCalcDataLoader.Create(TfcxCommonUVField(Self));
      end;
      FDataLoaderType := Value;
    end
    else
    begin
      FDataLoader := nil;
      FDataLoaderType := dlt_None;
    end;
    AOldDataLoader.Free;
  end
end;

{ TfcxCalcField }

(*
procedure TfcxCalcField.AddValue(var AUValue: PfcxCommonUV; ASourceRecord: TfcxSourceRecord);
var
  ATempPointer: Pointer;
  not_IsNull: Boolean;
begin
  not_IsNull := GetData(ATempPointer, ASourceRecord);
  if not_IsNull then
    FUniqueValues.AddNewValue(ATempPointer, AUValue)
  else
    FUniqueValues.AddNewValue(nil, AUValue);
end;
*)

function TfcxCalcField.CheckField(
  ACFieldProperties: TfcxCFieldProperties): Boolean;
begin
  Result := (FCubeFieldName = ACFieldProperties.FieldProperties.CubeFieldName) and
            (FDataType = ACFieldProperties.FieldProperties.DataType) and
            (FCFieldType = ACFieldProperties.CFieldType);
//  FCubeFieldDisplayLabel := ACFieldProperties.FieldProperties.CubeFieldDisplayLabel;
//  FCaseSensitive := ACFieldProperties.FieldProperties.CaseSensitive;
//  FNullStr := ACFieldProperties.FieldProperties.NullStr;
//  FSystemText := ACFieldProperties.CalcString;
//  FSaved := ACFieldProperties.FieldProperties.Saved;
  if not Result then
    exit;
  DataLoaderType := dlt_Calculation;
  case FCFieldType of
    fcft_Event:
      GetData := GetDataEvent;
    fcft_Expression:
      GetData := GetDataExpression;
    fcft_Script:
      GetData := GetDataScript;
//    fcft_Simple:
//      GetData := GetDataSimple;
  end;
end;

constructor TfcxCalcField.Create(ACube: TfcxCube;
  ACFieldProperties: TfcxCFieldProperties);
begin
  inherited Create(ACube);
// !!! временно
  DataLoaderType := dlt_Calculation;
  FCFieldType := ACFieldProperties.CFieldType;
  case FCFieldType of
    fcft_Event:
      GetData := GetDataEvent;
    fcft_Expression:
      GetData := GetDataExpression;
    fcft_Script:
      GetData := GetDataScript;
//    fcft_Simple:
//      GetData := GetDataSimple;
  end;
  FCubeFieldName := ACFieldProperties.FieldProperties.CubeFieldName;
  FCubeFieldDisplayLabel := ACFieldProperties.FieldProperties.CubeFieldDisplayLabel;
  FDataType := ACFieldProperties.FieldProperties.DataType;
  FCaseSensitive := ACFieldProperties.FieldProperties.CaseSensitive;
  FNullStr := ACFieldProperties.FieldProperties.NullStr;
  FSystemText := ACFieldProperties.CalcString;
  FSaved := ACFieldProperties.FieldProperties.Saved;
  FDataLoader.CreateUVs;
end;

(*
procedure TfcxCalcField.CreateUVs;
begin
  FreeAndNil(FUniqueValues);
  FFields.free;
  FFields := TfcxAttributeFields.Create(Self);
  if FDataType <> fcdt_Error then
  begin
    FUniqueValues := TfcxBaseUniqueValues.Create(Self, False);
    FUniqueValues.WithGroup := False;
    FUniqueValues.CaptionSourceAttribute := '';
    FUniqueValues.OrderSourceAttribute := '';
  end
end;
*)

function TfcxCalcField.GetDataEvent(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
var
  AVarValue: Variant;
begin
{ TODO -cНеобходимо : Получить значение вычисляемое в событии на основании значений других полей.}
  if Assigned(FCube.FOnGetCalcValue) then
    Result := FCube.FOnGetCalcValue(FCube, FCubeFieldName, ASourceRecord, AVarValue)
  else
    Result := False;
  if Result then
    Buffer := cfcProcessorMap[FDataType].ToPointer(AVarValue)
  else
    Buffer := nil;
end;

function TfcxCalcField.GetDataExpression(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
begin
{ TODO -cНеобходимо : Получить значение вычисляемое в выражении на основании значений других полей.}
  result := False;
  Buffer := nil;
end;

function TfcxCalcField.GetDataScript(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
//var
//  AVarValue: Variant;
begin
{ TODO -cНеобходимо : Получить значение вычисляемое в скрипте на основании значений других полей.}
  result := False;
  Buffer := nil;
//  FCube.TimeStatStart.DBGetDataTime := fcxGetTickCount;
//  result := Получить значение вычисляемое в скрипте на основании значений других полей
//  AVarValue := varNull;
//  if result then
//    Buffer := cfcProcessorMap[FDataType].ToPointer(AVarValue)
//  else
//    Buffer := nil;
//  FCube.FTimeStat.DBGetDataTime := FCube.FTimeStat.DBGetDataTime + fcxGetTickCount - FCube.FTimeStatStart.DBGetDataTime;
end;

{ TODO -cНеобходимо : Получать значение из потока.}
{
function TfcxCalcField.GetDataSimple(var Buffer: Pointer; ASourceRecord: TfcxSourceRecord): Boolean;
begin
  result := False;
  Buffer := nil;
end;
}

procedure TfcxCalcField.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  if False {not SaveCalcAsData} then
  begin
    AItem.Prop['CalcType'] := GetEnumName(TypeInfo(TfcxCFieldType), Ord(FCFieldType));
    AItem.Prop['SystemText'] := FSystemText;
  end
end;

{ TfcxSourceRecord }

constructor TfcxSourceRecord.Create(ACube: TfcxCube;
  AFieldCount: integer);
begin
  FCube := ACube;
  FFieldCount := AFieldCount;
end;

function TfcxSourceRecord.GetIndexOf(AFieldName: TfcxString): Integer;
begin
  Result := FCube.FFields.FieldIndex(AFieldName);
end;

function TfcxSourceRecord.GetValue(AFieldName: TfcxString): Variant;
begin
  Result := GetValueAt(FCube.FFields.FieldIndex(AFieldName));
end;

function TfcxSourceRecord.GetValueAt(AFieldIndex: integer): Variant;
var
  AUVValue: PfcxCommonUV;
begin
  if (AFieldIndex >= 0) and (AFieldIndex >= 0) then
  begin
    AUVValue := FCube.FSourceHolder.FCube.FSourceHolder.FSourceList[FRecNo, AFieldIndex];
    if AUVValue = nil then
      Result := varNull
    else
      Result := TfcxCommonUVField(FCube.FFields.Items[AFieldIndex]).FUniqueValues.ValueAsVariant[AUVValue];
  end
  else
    Result := varNull
end;

{ TfcxMasterFields }

function TfcxMasterFields.AddDataSourceField(ASourceField: TfcxSourceField): Integer;
begin
  Result := Count;
//  if not Find(AField.FieldName, Result) then
  Insert(Result, TfcxUVField.Create(FCube, ASourceField));
  ReNum;
end;

procedure TfcxMasterFields.LoadFieldFromOldStream(ACubeStream: TStream; AMajorVersion, AMinorVersion: integer);
var
  AFieldIndex: integer;
  ACFieldsProperties: TfcxCFieldProperties;
  AOldSavedCubeField: TfcxOldSavedCubeField;
begin
  AOldSavedCubeField := TfcxCommonField.ReadOldFieldBaseProps(ACubeStream, AMajorVersion, AMinorVersion);
  ACFieldsProperties.FieldProperties.CubeFieldName := AOldSavedCubeField.FFieldName;
  ACFieldsProperties.FieldProperties.CubeFieldDisplayLabel := AOldSavedCubeField.FCaption;
  ACFieldsProperties.FieldProperties.NullStr := fcxResources.Get('sNULL');
  ACFieldsProperties.FieldProperties.CaseSensitive := False;
  ACFieldsProperties.FieldProperties.DataType := cfcFieldTypeToDataType[AOldSavedCubeField.FDataType];
  ACFieldsProperties.FieldProperties.Saved := True;
  ACFieldsProperties.FieldProperties.CalculateAfterAll := False;
  ACFieldsProperties.CFieldType := fcft_Simple;
  ACFieldsProperties.CalcString := '';
  AFieldIndex := AddStreamField(ACFieldsProperties.FieldProperties);
  GetMem(Items[AFieldIndex].OldSavedCubeField, SizeOf(TfcxOldSavedCubeFieldPropeties));
  Items[AFieldIndex].OldSavedCubeField^.DateType := AOldSavedCubeField.FDateType;
  Items[AFieldIndex].OldSavedCubeField^.TimeType := AOldSavedCubeField.FTimeType;
  Items[AFieldIndex].OldSavedCubeField^.VarType := GetVarType(AOldSavedCubeField.FDataType);
  if not AOldSavedCubeField.FUseDefaultFormat then
  begin
    Items[AFieldIndex].DisplayFormat.TypeFormat.Kind := AOldSavedCubeField.FFormatKind;
    Items[AFieldIndex].DisplayFormat.TypeFormat.FormatStr := AOldSavedCubeField.FFormatStr;
    Items[AFieldIndex].DisplayFormat.TypeFormat.DecSeparator := AOldSavedCubeField.FDecSeparator;
  end
  else
  if AOldSavedCubeField.FUseFormatStr then
    Items[AFieldIndex].DisplayFormat.TypeFormat.FormatStr := AOldSavedCubeField.FFormatStr;
end;

procedure TfcxMasterFields.LoadFromOldStream(ACubeStream: TStream; AMajorVersion, AMinorVersion: integer);
var
  i, AFieldCount: integer;
begin
  ACubeStream.Read(AFieldCount, SizeOf(integer));
  for i := 0 to AFieldCount - 1 do
    LoadFieldFromOldStream(ACubeStream, AMajorVersion, AMinorVersion);
end;

function TfcxMasterFields.BuildTree: TfcxTree;
  procedure FillNode(ANode: PfcxTreeNode);
  var
    i: integer;
    Field: TfcxCommonField;
    Node: PfcxTreeNode;
  begin
    for i := 0 to TfcxCommonUVField(ANode^.Data).Fields.Count - 1 do
    begin
      Field := TfcxCommonUVField(ANode^.Data).Fields.Items[i];
      Node := Result.AddChild(ANode);
      Node^.Data := Field;
      if (Field is TfcxCommonUVField) and (TfcxCommonUVField(Field).Fields <> nil) and (TfcxCommonUVField(Field).Fields.Count > 0) then
        FillNode(Node);
    end;
  end;
var
  i: integer;
  Field: TfcxCommonUVField;
  Node: PfcxTreeNode;
begin
  Result := TfcxTree.Create;
  for i := 0 to Count - 1 do
  begin
    Field := TfcxCommonUVField(Items[i]);
    Node := Result.AddChild(nil);
    Node^.Data := Field;
    if (TfcxCommonUVField(Node^.Data).Fields <> nil) and (TfcxCommonUVField(Node^.Data).Fields.Count > 0) then
      FillNode(Node);
  end;
end;

procedure TfcxMasterFields.SaveToXMLDoc(XMLDoc: TfcxXMLDocument);
begin
  XMLDoc.Root.Name := 'CubeFields';
  XMLDoc.Root.IntProp['version'] := 2;
  XMLDoc.Root.Prop['timestamp'] := DateTimeToStr(Now);
  SaveToXMLItem(XMLDoc.Root.Add);
end;

procedure TfcxMasterFields.SaveFieldsToStream(AStream: TStream);
var
  Doc: TfcxXMLDocument;
  AMemoryStream: TMemoryStream;
begin
  Doc := TfcxXMLDocument.Create;
  Doc.AutoIndent := True;
  AMemoryStream := TMemoryStream.Create;
  try
    SaveToXMLDoc(Doc);
    Doc.SaveToStream(AMemoryStream);
    AMemoryStream.Position := 0;
    WriteStream(AStream, AMemoryStream);
  finally
    AMemoryStream.Free;
    Doc.Free;
  end;
end;

procedure TfcxMasterFields.LoadFieldsFromStream(AStream: TStream);
var
  XMLDoc: TfcxXMLDocument;
  AMemoryStream: TMemoryStream;
begin
  AMemoryStream := TMemoryStream.Create;
  XMLDoc := TfcxXMLDocument.Create;
  try
    ReadStream(AStream, AMemoryStream);
    XMLDoc.LoadFromStream(AMemoryStream);
    LoadFromXMLDoc(XMLDoc);
  finally
    XMLDoc.Free;
    AMemoryStream.Free;
  end;
end;

procedure TfcxMasterFields.LoadFromXMLDoc(XMLDoc: TfcxXMLDocument);
begin
  LoadFromXMLItem(XMLDoc.Root[0]);
end;

function TfcxMasterFields.LinkDataSourceField(
  ASourceField: TfcxSourceField): Integer;
begin
  Result := FieldIndex(ASourceField.DataField.CubeFieldName);
  if Result = -1 then
    exit;
  if Items[Result] is TfcxCommonUVField then
  begin
// Compare type
    if Items[Result].FDataType <> ASourceField.DataField.CubeFieldType then
    begin
      Result := -1;
      exit;
    end;
// may be check another properties?
    with TfcxCommonUVField(Items[Result]) do
    begin
      FSaved := True;
      SetSourceField(ASourceField);
      if FUniqueValues.OrderSourceAttribute <> '' then
      begin
        TfcxHackBaseUniqueValues(FUniqueValues).SortIndexList;
      end;
  // LinkAttributes
      FDataLoader.LinkAttributes;
    end
  end
   else
   begin
// ?
   end;
//  Insert(Result, TfcxDataSourceField.Create(FCube, ASourceField));
end;

function TfcxMasterFields.CheckFieldsFromStream(AStream: TStream): boolean;
var
  XMLDoc: TfcxXMLDocument;
  AMemoryStream: TMemoryStream;
begin
  AMemoryStream := TMemoryStream.Create;
  XMLDoc := TfcxXMLDocument.Create;
  try
    ReadStream(AStream, AMemoryStream);
    XMLDoc.LoadFromStream(AMemoryStream);
    Result := CheckFromXMLDoc(XMLDoc);
  finally
    XMLDoc.Free;
    AMemoryStream.Free;
  end;
end;

function TfcxMasterFields.CheckFromXMLDoc(
  XMLDoc: TfcxXMLDocument): boolean;
begin
  Result := CheckFromXMLItem(XMLDoc.Root[0]);
end;

{ TfcxAttributeField }

constructor TfcxAttributeField.Create(AMasterField: TfcxCommonUVField;
  ASourceField: TfcxSourceField; AAttributeIndex: integer; AAttributeType: TfcxAttributeType);
begin
  FMasterField := AMasterField;
  FLevel := FMasterField.FLevel + 1;
  FAttributeIndex := AAttributeIndex;
  FAttributeType := AAttributeType;
  if ASourceField.SourceFieldProperties is TfcxCustomSourceFieldProperties then
    FCalculateAfterAll := TfcxCustomSourceFieldProperties(ASourceField.SourceFieldProperties).CalculateAfterAll
  else
    FCalculateAfterAll := False;
  GetMem(FMasterFieldsMap, SizeOf(Pointer) * FLevel);
  if FLevel > 1 then
    Move(AMasterField.FMasterFieldsMap[0], FMasterFieldsMap[0], SizeOf(Pointer) * (FLevel - 1));
  FMasterFieldsMap[FLevel - 1] := FMasterField;
  if FAttributeType = fcxsft_Reference then
  begin
    FLoadAllValues := TfcxMainReferenceDataField(ASourceField.DataField).LoadAllValues;
    if (AMasterField.DataLoaderType = dlt_DataSource) and
       (TfcxDSDataLoader(AMasterField.DataLoader).SourceField.DataField is TfcxReferenceDataField) and
       (TfcxReferenceDataField(TfcxDSDataLoader(AMasterField.DataLoader).SourceField.DataField).DataSet = TfcxMainReferenceDataField(ASourceField.DataField).DataSet) then
      FFromMasterSource := True
    else
      FFromMasterSource := False;
  end
  else
  begin
    FLoadAllValues := False;
    FFromMasterSource := False;
  end;
  inherited Create(AMasterField.Cube, ASourceField);
  FSaved := ASourceField.SourceFieldProperties.Saved;
end;

function TfcxAttributeField.CheckField(
  AFieldProperties: TfcxFieldProperties;
  AAttributeType: TfcxAttributeType): Boolean;
begin
  Result := (FAttributeType = AAttributeType) and CheckField(AFieldProperties);
end;

constructor TfcxAttributeField.Create(AMasterField: TfcxCommonUVField;
  AFieldProperties: TfcxFieldProperties;
  AAttributeType: TfcxAttributeType);
begin
  FMasterField := AMasterField;
  FLevel := FMasterField.FLevel + 1;
  FAttributeIndex := 0; // ???
  FAttributeType := AAttributeType;
  GetMem(FMasterFieldsMap, SizeOf(Pointer) * FLevel);
  if FLevel > 1 then
    Move(AMasterField.FMasterFieldsMap[0], FMasterFieldsMap[0], SizeOf(Pointer) * (FLevel - 1));
  FMasterFieldsMap[FLevel - 1] := FMasterField;
  inherited Create(AMasterField.Cube, AFieldProperties);
end;

procedure TfcxAttributeField.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  AItem.Prop['AttributeType'] := GetEnumName(TypeInfo(TfcxAttributeType), Ord(FAttributeType));
end;

procedure TfcxAttributeField.SetIndex(AIndex: integer);
begin
  FIndex := AIndex;
end;

procedure TfcxAttributeField.SetSaved(const Value: Boolean);
begin
  if AttributeType <> fcxsft_Reference then
    FSaved := Value
  else
    FSaved := True;
end;

procedure TfcxAttributeField.SetUniqueValues(
  AUniqueValues: TfcxBaseUniqueValues);
begin
  FUniqueValues := AUniqueValues;
end;

{ TfcxAttributeFields }

function TfcxAttributeFields.AddDatePathField(
  AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType): Integer;
begin
  Result := Count;
  Insert(Result, TfcxCommonDatePathField.Create(FMasterField, AFieldProperties, ADateType));
  Items[Result].FIndex := Result;
end;

function TfcxAttributeFields.AddAttributeField(
  ASourceField: TfcxSourceField; AAttributeIndex: Integer; AAttributeType: TfcxAttributeType): Integer;
begin
  Result := Count;
  Insert(Result, TfcxAttributeField.Create(FMasterField, ASourceField, AAttributeIndex, AAttributeType));
  Items[Result].FIndex := Result;
end;

function TfcxAttributeFields.AddAttributeStreamField(AFieldProperties: TfcxFieldProperties;
  AAttributeType: TfcxAttributeType): Integer;
begin
  Result := Count;
//  if not Find(AField.FieldName, Result) then
  Insert(Result, TfcxAttributeField.Create(FMasterField, AFieldProperties, AAttributeType));
  Items[Result].FIndex := Result;
end;

function TfcxAttributeFields.AddTimePathField(
  AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType): Integer;
begin
  Result := Count;
  Insert(Result, TfcxCommonTimePathField.Create(FMasterField, AFieldProperties, ATimeType));
  Items[Result].FIndex := Result;
end;

constructor TfcxAttributeFields.Create(AMasterField: TfcxCommonUVField);
begin
  inherited Create;
  FCube := AMasterField.Cube;
  FMasterField := AMasterField;
  AutoFree := True;
end;

procedure TfcxAttributeFields.DeleteField(AField: TfcxCommonField);
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    if FList[i] = AField then
    begin
      Delete(i);
      ReNum;
      Exit;
    end;
end;

function TfcxAttributeFields.CheckAttributeStreamField(
  AFieldProperties: TfcxFieldProperties; AAttributeType: TfcxAttributeType;
  AFieldIndex: Integer): Boolean;
begin
  Result := (Items[AFieldIndex] is TfcxAttributeField) and TfcxAttributeField(Items[AFieldIndex]).CheckField(AFieldProperties, AAttributeType);
end;

function TfcxAttributeFields.CheckDatePathField(
  AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType;
  AFieldIndex: Integer): Boolean;
begin
  Result := (Items[AFieldIndex] is TfcxCommonDatePathField) and TfcxCommonDatePathField(Items[AFieldIndex]).CheckField(AFieldProperties, ADateType);
end;

function TfcxAttributeFields.CheckTimePathField(
  AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType;
  AFieldIndex: Integer): Boolean;
begin
  Result := (Items[AFieldIndex] is TfcxCommonTimePathField) and TfcxCommonTimePathField(Items[AFieldIndex]).CheckField(AFieldProperties, ATimeType);
end;

{ TfcxSlicesManager }

constructor TfcxSlicesManager.Create(ACube: TfcxCube);
begin
  FCube := ACube;
  FStreamFromCube := nil;
end;

destructor TfcxSlicesManager.Destroy;
begin
  FStreamFromCube.Free;
  FStreamFromCube := Nil;
  inherited;
end;

function TfcxSlicesManager.GetCount: Integer;
var
  i, AFiltersCount: Integer;
begin
  Result := 0;
  AFiltersCount := FCube.ListnersManager.ListnerByClassCount[TfcxFilterManager];
  for i := 0 to AFiltersCount - 1 do
    Result := Result + TfcxFilterManager(FCube.ListnersManager.ListnerByClass[i, TfcxFilterManager]).ListnersManager.ListnerByClassCount[TfcxAbstractSlice];
end;

function TfcxSlicesManager.GetSlice(AIndex: Integer): TfcxAbstractSlice;
var
  i, AFiltersCount, ASlicesCount, ASlicesCount2: Integer;
begin
  AFiltersCount := FCube.ListnersManager.ListnerByClassCount[TfcxFilterManager];
  ASlicesCount2 := 0;
  Result := Nil;
  for i := 0 to AFiltersCount - 1 do
  begin
    ASlicesCount := TfcxFilterManager(FCube.ListnersManager.ListnerByClass[i, TfcxFilterManager]).ListnersManager.ListnerByClassCount[TfcxAbstractSlice];
    if (ASlicesCount2 + ASlicesCount) > AIndex then
    begin
      Result := TfcxAbstractSlice(TfcxFilterManager(FCube.ListnersManager.ListnerByClass[i, TfcxFilterManager]).ListnersManager.ListnerByClass[AIndex - ASlicesCount2, TfcxAbstractSlice]);
      Exit;
    end;
    ASlicesCount2 := ASlicesCount2 + ASlicesCount;
  end
end;

procedure TfcxSlicesManager.LoadFromStream;
var
  ASlicesCount, i, j: integer;
  XMLStream: TStream;
begin
  if FStreamFromCube <> nil then
  begin
    if FOldVersion then
    begin
      ASlicesCount := Count;
      if FMajorVersion >= 2 then
      begin
        FStreamFromCube.Read(j, SizeOf(j));
        if j > ASlicesCount then
        begin
          NeedSlice(j);
          ASlicesCount := Count;
        end; (*if*)
        if j <= ASlicesCount then
          for i := 0 to j - 1 do
          begin
            if FMajorVersion >= 3 then
            begin
              XMLStream := TMemoryStream.Create;
              ReadStream(FStreamFromCube, XMLStream);
              try
                Slice[i].LoadFromStream(XMLStream);
              finally
                XMLStream.Free;
              end;
            end
            else
              Slice[i].LoadFromStream(FStreamFromCube);
          end;
      end
      else
      begin
        if ASlicesCount = 0 then
        begin
          NeedSlice(1);
          ASlicesCount := Count;
        end; (*if*)
        if ASlicesCount >= 1 then
        begin
          Slice[0].InternalSetSelected(FSM, FSC, FSR, -1, -1);
          Slice[0].LoadFromStream(FStreamFromCube);
        end; (*if*)
      end;
      FreeAndNil(FStreamFromCube);
    end
    else
    begin
      ASlicesCount := Count;
      FStreamFromCube.Read(j, SizeOf(j));
      if j > ASlicesCount then
      begin
        NeedSlice(j);
        ASlicesCount := Count;
      end; (*if*)
      if j <= ASlicesCount then
        for i := 0 to j - 1 do
        begin
          XMLStream := TMemoryStream.Create;
          ReadStream(FStreamFromCube, XMLStream);
          try
            Slice[i].LoadFromStream(XMLStream);
          finally
            XMLStream.Free;
          end;
        end;
      FreeAndNil(FStreamFromCube);
    end;
  end;
end;

procedure TfcxSlicesManager.NeedSlice(AIndex: Integer);
begin
  if Assigned(OnNeedSlice) then
    OnNeedSlice(Self, AIndex);
end;

procedure TfcxSlicesManager.SaveToStream(AStream: TStream; AStoreItems: TfcxItemsForStoreWithSlice = []);
var
  ASlicesCount, i: integer;
  XmlStream: TStream;
begin
  ASlicesCount := Count;
  AStream.Write(ASlicesCount, SizeOf(integer));
  for i := 0 to ASlicesCount - 1 do
  begin
    XmlStream := TMemoryStream.Create;
    Slice[i].SaveToStream(XmlStream, AStoreItems);
    XmlStream.Position := 0;
    WriteStream(AStream, XmlStream);
    XmlStream.Free;
  end;
end;

{ TfcxCommonUVField }

destructor TfcxCommonUVField.Destroy;
begin
  FreeAndNil(FUniqueValues);
  FreeAndNil(FFields);
  inherited;
end;

function TfcxCommonUVField.GetDataTypeProcessor: TfcxCommonDataTypeProcessor;
begin
  Result := FUniqueValues.DataTypeProcessor
end;

function TfcxCommonUVField.GetCaptionValueAtIndex(
  Index: integer): TfcxString;
begin
  Result := UniqueValues.GetCaption(Index)
end;

function TfcxCommonUVField.GetCanGroup: Boolean;
begin
  Result := UniqueValues.WithGroup;
end;

function TfcxCommonUVField.GetGroupManager: TfcxCommonGroupManager;
begin
  Result := UniqueValues.GroupManager;
end;

procedure TfcxCommonUVField.SetCanGroup(const Value: Boolean);
begin
  UniqueValues.WithGroup := Value;
end;

function TfcxCommonUVField.GetValueAsVariantAtIndex(
  Index: integer): Variant;
begin
  Result := UniqueValues.ValueAsVariantByIndex[Index]
end;

procedure TfcxCommonUVField.SaveUVsToStream(ACubeStream: TStream);
begin
  if Saved then
    UniqueValues.SaveToStream(ACubeStream);
  Fields.SaveUVsToStream(ACubeStream);
end;

procedure TfcxCommonUVField.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  AItem.Prop['CaptionSourceAttribute'] := FUniqueValues.CaptionSourceAttribute;
  AItem.Prop['OrderSourceAttribute'] := FUniqueValues.OrderSourceAttribute;
  AItem.BoolProp['CalculateAfterAll'] := FCalculateAfterAll;
end;

procedure TfcxCommonUVField.LoadFromXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  if FMasterField <> nil then
    TfcxHackAttributesManager(FMasterField.UniqueValues.SplitManager.AttributesManager).AddAttributeSaved(Self);
//      case TfcxAttributeType(Bt) of
//        fcxsft_Custom:
// временно 12-2011        TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).AddCustomAttribute(ReadFieldBaseProps(ACubeStream))
//         ;
//        fcxsft_Reference:
//      end;
end;

procedure TfcxCommonUVField.LoadUVsFromStream(ACubeStream: TStream);
begin
  if Saved then
    UniqueValues.LoadFromStream(ACubeStream);
  Fields.LoadUVsFromStream(ACubeStream);
end;

procedure TfcxCommonUVField.SetSourceField(const Value: TfcxSourceField);
begin
  if Value <> nil then
  begin
    DataLoaderType := dlt_DataSource;
    with TfcxDSDataLoader(DataLoader) do
    begin
      SourceField := Value;
      FDataType := FSourceField.DataField.CubeFieldType;
      FCubeFieldName := FSourceField.DataField.CubeFieldName;
      FCubeFieldDisplayLabel := FSourceField.DataField.CubeFieldDisplayLabel;
      if FSourceField.SourceFieldProperties is TfcxCustomSourceFieldProperties then
        FCalculateAfterAll := TfcxCustomSourceFieldProperties(FSourceField.SourceFieldProperties).CalculateAfterAll
      else
        FCalculateAfterAll := False;
      if FSourceField.DataField.CaseSensitive = cs_Default then
{ TODO -cНеобходимо : Брать значения по умолчании из куба. Некоторые значения зависят от типа данных.}
        FCaseSensitive := True
      else
        FCaseSensitive := (FSourceField.DataField.CaseSensitive = cs_Sensitive);
      if FSourceField.DataField.NullStr = '' then
        FNullStr := fcxResources.Get('sNull')
      else
        FNullStr := FSourceField.DataField.NullStr;
    end;
{
  end
  else
  begin
    FDataType := fcdt_Error;
    FCubeFieldName := '';
    FCubeFieldDisplayLabel := '';
    FCalculateAfterAll := False;
    FCaseSensitive := True;
    FNullStr := fcxResources.Get('sNull');
}
  end;
end;

procedure TfcxCommonUVField.AppendUVsFromStream(ACubeStream: TStream);
begin
  if Saved then
    UniqueValues.AppendFromStream(ACubeStream);
  Fields.AppendUVsFromStream(ACubeStream);
end;

procedure TfcxCommonUVField.LoadFromXMLItemStep2(AItem: TfcxXMLItem);
begin
  inherited;
  FUniqueValues.CaptionSourceAttribute := AItem.Prop['CaptionSourceAttribute'];
  FUniqueValues.OrderSourceAttribute := AItem.Prop['OrderSourceAttribute'];
end;

{ TfcxCommonDatePathField }

function TfcxCommonDatePathField.CheckField(
  AFieldProperties: TfcxFieldProperties; ADateType: TfcxDateType): Boolean;
begin
  Result := (FDateType = ADateType) and
            (FCubeFieldName = AFieldProperties.CubeFieldName) and
            (FDataType = AFieldProperties.DataType);
end;

constructor TfcxCommonDatePathField.Create(
  AMasterField: TfcxCommonUVField; AFieldProperties: TfcxFieldProperties;
  ADateType: TfcxDateType);
begin
  inherited Create(AMasterField.Cube);
  FDateType := ADateType;
  FMasterField := AMasterField;
  FLevel := FMasterField.FLevel + 1;
//(* !!! временно
  FSaved := AFieldProperties.Saved;
//*)
  GetMem(FMasterFieldsMap, SizeOf(Pointer) * FLevel);
  if FLevel > 1 then
    Move(AMasterField.FMasterFieldsMap[0], FMasterFieldsMap[0], SizeOf(Pointer) * (FLevel - 1));
  FMasterFieldsMap[FLevel - 1] := FMasterField;
  FCubeFieldName := AFieldProperties.CubeFieldName;
  FCubeFieldDisplayLabel := AFieldProperties.CubeFieldDisplayLabel;
  FDataType := AFieldProperties.DataType;
  FCaseSensitive := AFieldProperties.CaseSensitive;
  FNullStr := AFieldProperties.NullStr;
  FStdPathUniqueValues := TfcxDatePathUniqueValues.Create(Self, ADateType);
end;

function TfcxCommonDatePathField.CustomMonthNames(AMonth: word): TfcxString;
begin
{ TODO -cНеобходимо : Нужно сделать в кубе событие, убрать комментарий и перенести строку в ресурсы }
//  if Assigned(fCube.OnGetMonthName) then
//    result := fCube.OnGetMonthName(Self, AMonth)
//  else
    result := 'OnGetMonthName not assigned';
end;

function TfcxCommonDatePathField.CustomQuarterNames(AQuarter: word): TfcxString;
begin
{ TODO -cНеобходимо : Нужно сделать в кубе событие, убрать комментарий и перенести строку в ресурсы }
//  if Assigned(fCube.OnGetQuarterName) then
//    result := fCube.OnGetQuarterName(Self, AQuarter)
//  else
    result := 'OnGetQuarterName not assigned';
end;

function TfcxCommonDatePathField.CustomWeekDayNames(AWeekDay: word): TfcxString;
begin
{ TODO -cНеобходимо : Нужно сделать в кубе событие, убрать комментарий и перенести строку в ресурсы }
//  if Assigned(fCube.OnGetWeekDayName) then
//    result := fCube.OnGetWeekDayName(Self, AWeekDay)
//  else
    result := 'OnGetWeekDayName not assigned';
end;

function TfcxCommonDatePathField.CustomWeekNumberNames(AWeekNumber: word; AYearDelta: integer): TfcxString;
begin
{ TODO -cНеобходимо : Нужно сделать в кубе событие, убрать комментарий и перенести строку в ресурсы }
//  if Assigned(fCube.OnGetWeekNumberName) then
//    result := fCube.OnGetWeekNumberName(Self, AWeekNumber, AYearDelta)
//  else
    result := 'OnGetWeekNumberName not assigned';
end;

function TfcxCommonDatePathField.GetDatePathProcessor: TfcxCommonDatePathDTP;
begin
  Result := TfcxCommonDatePathDTP(FStdPathUniqueValues.DataTypeProcessor);
end;

function TfcxCommonDatePathField.GetDatePathUniqueValues: TfcxDatePathUniqueValues;
begin
  Result := TfcxDatePathUniqueValues(FStdPathUniqueValues)
end;

procedure TfcxCommonDatePathField.LoadFromXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  if not FMasterField.UniqueValues.SplitManager.UseDateSplit then
    FMasterField.UniqueValues.SplitManager.UseDateSplit := True;
  TfcxHackDatePathsManager(FMasterField.UniqueValues.SplitManager.DatePathsManager).InternalAddDatePathSaved(Self);
end;

procedure TfcxCommonDatePathField.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  AItem.Prop['DateType'] := GetEnumName(TypeInfo(TfcxDateType), Ord(FDateType));
end;

{ TfcxCommonTimePathField }

function TfcxCommonTimePathField.CheckField(
  AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType): Boolean;
begin
  Result := (FTimeType = ATimeType) and
            (FCubeFieldName = AFieldProperties.CubeFieldName) and
            (FDataType = AFieldProperties.DataType);
end;

constructor TfcxCommonTimePathField.Create(AMasterField: TfcxCommonUVField;
  AFieldProperties: TfcxFieldProperties; ATimeType: TfcxTimeType);
begin
  inherited Create(AMasterField.Cube);
  FTimeType := ATimeType;
  FMasterField := AMasterField;
  FLevel := FMasterField.FLevel + 1;
//(* !!! временно
  FSaved := AFieldProperties.Saved;
//*)
  GetMem(FMasterFieldsMap, SizeOf(Pointer) * FLevel);
  if FLevel > 1 then
    Move(AMasterField.FMasterFieldsMap[0], FMasterFieldsMap[0], SizeOf(Pointer) * (FLevel - 1));
  FMasterFieldsMap[FLevel - 1] := FMasterField;
  FCubeFieldName := AFieldProperties.CubeFieldName;
  FCubeFieldDisplayLabel := AFieldProperties.CubeFieldDisplayLabel;
  FDataType := AFieldProperties.DataType;
  FCaseSensitive := AFieldProperties.CaseSensitive;
  FNullStr := AFieldProperties.NullStr;
  FStdPathUniqueValues := TfcxTimePathUniqueValues.Create(Self, ATimeType);
end;

function TfcxCommonTimePathField.GetTimePathProcessor: TfcxCommonTimePathDTP;
begin
  Result := TfcxCommonTimePathDTP(FStdPathUniqueValues.DataTypeProcessor);
end;

function TfcxCommonTimePathField.GetTimePathUniqueValues: TfcxTimePathUniqueValues;
begin
  Result := TfcxTimePathUniqueValues(FStdPathUniqueValues)
end;

procedure TfcxCommonTimePathField.LoadFromXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  if not FMasterField.UniqueValues.SplitManager.UseTimeSplit then
    FMasterField.UniqueValues.SplitManager.UseTimeSplit := True;
  TfcxHackTimePathsManager(FMasterField.UniqueValues.SplitManager.TimePathsManager).InternalAddTimePathSaved(Self);
end;

procedure TfcxCommonTimePathField.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  inherited;
  AItem.Prop['TimeType'] := GetEnumName(TypeInfo(TfcxTimeType), Ord(FTimeType));
end;

{ TfcxCommonStdPathField }

procedure TfcxCommonStdPathField.AppendUVsFromStream(ACubeStream: TStream);
begin
  if Saved then
    StdPathUniqueValues.DataTypeProcessor.AppendFromStream(ACubeStream);
end;

destructor TfcxCommonStdPathField.Destroy;
begin
  FreeAndNil(FStdPathUniqueValues);
  inherited;
end;

function TfcxCommonStdPathField.GetCanGroup: Boolean;
begin
  Result := StdPathUniqueValues.WithGroup;
end;

function TfcxCommonStdPathField.GetCaptionValueAtIndex(
  Index: integer): TfcxString;
begin
  Result := FStdPathUniqueValues.DataTypeProcessor.CaptionAtIndex[Index]
end;

function TfcxCommonStdPathField.GetDataTypeProcessor: TfcxCommonDataTypeProcessor;
begin
  Result := FStdPathUniqueValues.DataTypeProcessor
end;

function TfcxCommonStdPathField.GetGroupManager: TfcxCommonGroupManager;
begin
  Result := StdPathUniqueValues.GroupManager
end;

function TfcxCommonStdPathField.GetValueAsVariantAtIndex(
  Index: integer): Variant;
begin
  Result := FStdPathUniqueValues.DataTypeProcessor.ValueAtIndex[Index]
end;

procedure TfcxCommonStdPathField.LoadUVsFromStream(ACubeStream: TStream);
begin
  if Saved then
    StdPathUniqueValues.DataTypeProcessor.LoadFromStream(ACubeStream);
end;

procedure TfcxCommonStdPathField.SaveUVsToStream(ACubeStream: TStream);
begin
  if Saved then
    StdPathUniqueValues.DataTypeProcessor.SaveToStream(ACubeStream);
end;

procedure TfcxCommonStdPathField.SetCanGroup(const Value: Boolean);
begin
  StdPathUniqueValues.WithGroup := Value;
end;

{ TfcxCommonDataLoader }

constructor TfcxCommonDataLoader.Create(AField: TfcxCommonUVField);
begin
  FField := AField
end;

procedure TfcxCommonDataLoader.CreateUVs;
begin
  with FField do
  begin
    FreeAndNil(FUniqueValues);
    FFields.free;
    FFields := TfcxAttributeFields.Create(FField);
    if FDataType <> fcdt_Error then
    begin
      FUniqueValues := CreateUniqueValues;
      FUniqueValues.WithGroup := False;
      FUniqueValues.CaptionSourceAttribute := '';
      FUniqueValues.OrderSourceAttribute := '';
    end;
  end;
end;

{ TfcxDSDataLoader }

procedure TfcxDSDataLoader.AddValue(var AUValue: PfcxCommonUV;
  ASourceRecord: TfcxSourceRecord);
var
  ATempPointer, ATempPointer2: Pointer;
  not_IsNull: Boolean;
begin
  with FField do
  begin
    ATempPointer := nil;
    FCube.TimeStatStart.DBGetDataTime := fcxGetTickCount;
    not_IsNull := SourceField.DataField.GetData(ATempPointer);
    FCube.FTimeStat.DBGetDataTime := FCube.FTimeStat.DBGetDataTime + fcxGetTickCount - FCube.FTimeStatStart.DBGetDataTime;
    if not_IsNull then
    begin
      if (FSourceField.SourceFieldType = fcxsft_Reference) and TfcxReferenceSourceFieldProperties(FSourceField.SourceFieldProperties).DataField.Convert then
      begin
  { TODO -cНеобходимо : Первично сделанное конвертирование. Возможно надо переделать.}
        FCube.TimeStatStart.ConvertTime := fcxGetTickCount;
        ATempPointer2 := ATempPointer;
        ATempPointer := cfcProcessorMap[FSourceField.DataField.CubeFieldType].ToPointer(cfcProcessorMap[TfcxReferenceSourceFieldProperties(FSourceField.SourceFieldProperties).DataField.DataFieldType].PointerAsVariant(ATempPointer2));
        FreeMem(ATempPointer2);
        FCube.FTimeStat.ConvertTime := FCube.FTimeStat.ConvertTime + fcxGetTickCount - FCube.FTimeStatStart.ConvertTime;
      end;
      FUniqueValues.AddNewValue(ATempPointer, AUValue);
    end
    else
    begin
      FUniqueValues.AddNewValue(nil, AUValue);
    end;
  end;
end;

function TfcxDSDataLoader.CreateUniqueValues: TfcxBaseUniqueValues;
begin
  if FField is TfcxAttributeField then
  begin
    with TfcxAttributeField(FField) do
      case AttributeType of
        fcxsft_Reference:
          begin
            Result := TfcxAttributeUniqueValues.CreateReferenceAttribute(FField, TfcxReferenceDataField(FSourceField.DataField),
               TfcxReferenceAttributeSFProperties(FSourceField.SourceFieldProperties).IdField,
               MasterField.UniqueValues, False,
               TfcxMainReferenceDataField(FSourceField.DataField).LoadAllValues)
          end;
      else
        Result := TfcxAttributeUniqueValues.CreateAttribute(FField,
          MasterField.UniqueValues, False, AttributeType);
      end;
  end
  else
    Result := TfcxBaseUniqueValues.Create(FField, FSourceField.WithCustomCaption);
end;

procedure TfcxDSDataLoader.CreateUVs;
var
  i: integer;
begin
  inherited;
  with FField do
  begin
    if FDataType <> fcdt_Error then
    begin
      if FDataType in fcxDateTypes then
      begin
        try
          FUniqueValues.SplitManager.UseDateSplit := FSourceField.SplitProperty.DateSplitPaths <> [];
        except
        end;
        if FUniqueValues.SplitManager.UseDateSplit then
          TfcxHackDatePathsManager(FUniqueValues.SplitManager.DatePathsManager).SetDatePaths(FSourceField.SplitProperty.DateSplitPaths);
      end;
      if FDataType in fcxTimeTypes then
      begin
        try
          FUniqueValues.SplitManager.UseTimeSplit := FSourceField.SplitProperty.TimeSplitPaths <> [];
        except
        end;
        if FUniqueValues.SplitManager.UseTimeSplit then
          TfcxHackTimePathsManager(FUniqueValues.SplitManager.TimePathsManager).SetTimePaths(FSourceField.SplitProperty.TimeSplitPaths);
      end;    
      for i := 0 to FSourceField.SplitProperty.Attributes.Count - 1 do
        TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).AddAttribute(FSourceField.SplitProperty.Attributes[i]);

      FUniqueValues.CaptionSourceAttribute := FSourceField.CaptionSourceAttribute;
      FUniqueValues.OrderSourceAttribute := FSourceField.OrderSourceAttribute;
    end
  end;
end;

procedure TfcxDSDataLoader.LinkAttributes;
var
  i, AAttrubuteIndex: integer;
begin
  with FField do
  begin
    if FDataType <> fcdt_Error then
    begin
      for i := 0 to FSourceField.SplitProperty.Attributes.Count - 1 do
      begin
//        TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).AddAttribute(FSourceField.SplitProperty.Attributes[i]);
        AAttrubuteIndex := TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).FindAttribute(FSourceField.SplitProperty.Attributes[i].DataField.CubeFieldName);
        if AAttrubuteIndex <> -1 then
        begin
          if TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).Attribute[AAttrubuteIndex].fcField is TfcxCommonUVField then
          begin
// Compare type
            if TfcxCommonUVField(TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).Attribute[AAttrubuteIndex].fcField).FDataType <> FSourceField.SplitProperty.Attributes[i].DataField.CubeFieldType then
              continue;
            with TfcxCommonUVField(TfcxHackAttributesManager(FUniqueValues.SplitManager.AttributesManager).Attribute[AAttrubuteIndex].fcField) do
            begin
              SetSourceField(FSourceField.SplitProperty.Attributes[i]);
              TfcxHackAttributeUniqueValues(FUniqueValues).SetDataFields(TfcxReferenceDataField(FSourceField.SplitProperty.Attributes[i].DataField), TfcxReferenceAttributeSFProperties(FSourceField.SplitProperty.Attributes[i].SourceFieldProperties).IdField);
              if FUniqueValues.OrderSourceAttribute <> '' then
              begin
                TfcxHackBaseUniqueValues(FUniqueValues).SortIndexList;
              end;
// LinkAttributes
              FDataLoader.LinkAttributes;
            end
          end
          else
          begin
// ?
          end
        end
      end
    end
  end;
end;

procedure TfcxDSDataLoader.SetSourceField(const Value: TfcxSourceField);
begin
  FSourceField := Value;
end;

{ TfcxStreamDataLoader }

procedure TfcxStreamDataLoader.AddValue(var AUValue: PfcxCommonUV;
  ASourceRecord: TfcxSourceRecord);
begin
//
end;

function TfcxStreamDataLoader.CreateUniqueValues: TfcxBaseUniqueValues;
begin
  if FField is TfcxAttributeField then
    with TfcxAttributeField(FField) do
      Result := TfcxAttributeUniqueValues.CreateAttribute(FField,
         MasterField.UniqueValues, False, FAttributeType)
  else
    Result := TfcxBaseUniqueValues.Create(FField, False);
end;

procedure TfcxStreamDataLoader.LinkAttributes;
begin
//
end;

{ TfcxCalcDataLoader }

procedure TfcxCalcDataLoader.AddValue(var AUValue: PfcxCommonUV;
  ASourceRecord: TfcxSourceRecord);
var
  ATempPointer: Pointer;
  not_IsNull: Boolean;
begin
// !!! временно
  with TfcxCalcField(FField) do
  begin
    not_IsNull := GetData(ATempPointer, ASourceRecord);
    if not_IsNull then
      FUniqueValues.AddNewValue(ATempPointer, AUValue)
    else
      FUniqueValues.AddNewValue(nil, AUValue);
  end;
end;

function TfcxCalcDataLoader.CreateUniqueValues: TfcxBaseUniqueValues;
begin
  Result := TfcxBaseUniqueValues.Create(FField, False)
end;

procedure TfcxCalcDataLoader.LinkAttributes;
begin
//
end;

{ TfcxUVField }

constructor TfcxUVField.Create(ACube: TfcxCube;
  ASourceField: TfcxSourceField);
begin
  inherited Create(ACube);
  FSaved := True;
  SetSourceField(ASourceField);
  FDataLoader.CreateUVs;
end;

function TfcxUVField.CheckField(
  AFieldProperties: TfcxFieldProperties): Boolean;
begin
  Result := (FDataType = AFieldProperties.DataType) and
            (FCubeFieldName = AFieldProperties.CubeFieldName);
  if Result then
  begin
    DataLoaderType := dlt_Stream;
    if FUniqueValues.OrderSourceAttribute <> '' then
    begin
      TfcxHackBaseUniqueValues(FUniqueValues).SortIndexList;
    end;
  end
end;

constructor TfcxUVField.Create(ACube: TfcxCube;
  AFieldProperties: TfcxFieldProperties);
begin
  inherited Create(ACube);
  DataLoaderType := dlt_Stream;
  FDataType := AFieldProperties.DataType;
  FCubeFieldName := AFieldProperties.CubeFieldName;
  FCubeFieldDisplayLabel := AFieldProperties.CubeFieldDisplayLabel;
  FCaseSensitive := AFieldProperties.CaseSensitive;
  FNullStr := AFieldProperties.NullStr;
  FSaved := AFieldProperties.Saved;
  FCalculateAfterAll := AFieldProperties.CalculateAfterAll;
  FDataLoader.CreateUVs;
end;

{ TfcxCubeDataColumns }

procedure TfcxCubeDataColumns.Assign(ASource: TfcxCubeDataColumns);
begin
  FCols := ASource.FCols;
end;

procedure TfcxCubeDataColumns.Clear;
begin
  SetLength(FCols, 0);
end;

constructor TfcxCubeDataColumns.Create;
begin
  FCols := nil;
end;

destructor TfcxCubeDataColumns.Destroy;
begin
  Clear;
  inherited;
end;

function TfcxCubeDataColumns.GetAbsItem(AIndex: Integer): PfcxCubeDataColumn;
begin
  Result := @FCols[AIndex]
end;

function TfcxCubeDataColumns.GetCount: Integer;
begin
  Result := Length(FCols);
end;

function TfcxCubeDataColumns.GetFieldItem(AIndex: TfcxCommonField): PfcxCubeDataColumn;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if FCols[I].Field = AIndex then
    begin
      Result := @FCols[I];
      Exit;
    end;
  Result := nil;
end;

function TfcxCubeDataColumns.GetVisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FCols) do
    if FCols[I].Visible then
      Inc(Result);
end;

function TfcxCubeDataColumns.GetVisItem(AIndex: Integer): PfcxCubeDataColumn;
var
  I: Integer;
begin
  for I := 0 to High(FCols) do
  begin
    if FCols[I].Visible then
      Dec(AIndex);
    if AIndex < 0 then
    begin
      Result := @FCols[I];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TfcxCubeDataColumns.SetCount(const Value: Integer);
var
  I, OldLen: Integer;
begin
  OldLen := Length(FCols);
  SetLength(FCols, Value);
  if OldLen < Value then
    for I := OldLen to Value - 1 do
      FCols[I] := DefaultColValue;
end;

procedure TfcxCubeDataColumns.Update(Cube: TfcxCube);
var
  OldHidden: TList;
  OldSorted: array of TfcxCubeDataColumn;

  procedure Add(AField: TfcxCommonField);
  var
    i: integer;
  begin
    SetCount(Count + 1);
    FCols[Count - 1].Field := AField;
    FCols[Count - 1].Visible := OldHidden.IndexOf(AField) = -1;
    for i := 0 to High(OldSorted) do
      if OldSorted[i].Field = AField then
      begin
        FCols[Count - 1].SortIndex := OldSorted[i].SortIndex;
        FCols[Count - 1].SortDirection := OldSorted[i].SortDirection;
        Break;
      end;
  end;

  procedure TraverseAdd(ANode: Pointer);
  begin
    if Assigned(ANode) then
    begin
      Add(TfcxCommonField(PfcxTreeNode(ANode)^.Data));
      TraverseAdd(PfcxTreeNode(ANode).FirstChild);
      TraverseAdd(PfcxTreeNode(ANode).NextSibling);
    end;
  end;

  procedure AddOldSorted(ACubeDataColumn: TfcxCubeDataColumn);
  begin
    SetLength(OldSorted, Length(OldSorted) + 1);
    OldSorted[High(OldSorted)] := ACubeDataColumn;
  end;

var
  Tree: TfcxTree;
  I: Integer;
begin
  OldHidden := TList.Create;
  SetLength(OldSorted, 0);
  try
    for I := 0 to Count - 1 do
    begin
      if not FCols[I].Visible then
        OldHidden.Add(FCols[I].Field);
      if FCols[I].SortIndex <> -1 then
        AddOldSorted(FCols[I]);
    end;
    SetCount(0);
    if Assigned(Cube) then
    begin
      Tree := Cube.Fields.BuildTree;
      try
        TraverseAdd(Tree.First);
      finally
        Tree.Free;
      end;
    end;
  finally
    SetLength(OldSorted, 0);
    OldHidden.Free;
  end;
end;

{ TfcxCubeTimeStat }

procedure TfcxCubeTimeStat.Clear;
begin
  FullTime := 0;
  DBOpenTime := 0;
  DBCloseTime := 0;
  OpenTime := 0;
  DBGetDataTime := 0;
  DBMoveTime := 0;
//  DBOtherTime := 0;
  SortTime := 0;
  ConvertTime := 0;
//  UVsTime := 0;
end;

{ TfcxOrderedRecordSetProvider }

procedure TfcxOrderedRecordSetProvider.ClearIndex;
begin
  GetRowIndex := GetRowIndexDefault;
  FCubeFieldsInIndex.Clear;
  FreeMem(FRecordIndex);
  FRecordIndex := nil;
  FRecordCount := GetRecordCount;
end;

function TfcxOrderedRecordSetProvider.CompareUVIndexes(AIndex1, AIndex2: integer): integer;
begin
  Result := 0;
end;

constructor TfcxOrderedRecordSetProvider.Create;
begin
  FRecordIndex := nil;
  FCubeFieldsInIndex := TfcxCubeFieldsInIndex.Create;
  FCubeDataColumns := TfcxCubeDataColumns.Create;
  GetRowIndex := GetRowIndexDefault;
end;

procedure TfcxOrderedRecordSetProvider.CreateIndex;
begin
  GetRowIndex := GetRowIndexDefault;
  FreeMem(FRecordIndex);
  FRecordCount := GetRecordCount;
  GetMem(FRecordIndex, SizeOf(Integer) * FRecordCount);
  FillRecordIndex;
  Sort;
  GetRowIndex := GetRowIndexOrder;
end;

destructor TfcxOrderedRecordSetProvider.Destroy;
begin
  FreeMem(FRecordIndex);
  FRecordIndex := nil;
  FCubeFieldsInIndex.Free;
  FCubeDataColumns.Free;
  inherited;
end;

procedure TfcxOrderedRecordSetProvider.FillRecordIndex;
var
  i: integer;
begin
  for i := 0 to FRecordCount - 1 do
    FRecordIndex[i] := i;
end;

function TfcxOrderedRecordSetProvider.GetRecordCount: Integer;
begin
  Result := 0;
end;

function TfcxOrderedRecordSetProvider.GetRowIndexDefault(ARow: Integer): Integer;
begin
  Result := ARow;
end;

function TfcxOrderedRecordSetProvider.GetRowIndexOrder(ARow: Integer): Integer;
begin
  Result := FRecordIndex[ARow];
end;

function TfcxOrderedRecordSetProvider.GetText(ACol, ARowIndex: Integer): String;
begin
  Result := '';
end;

procedure TfcxOrderedRecordSetProvider.SetCubeDataColumns(const Value: TfcxCubeDataColumns);
begin
  FCubeDataColumns.Assign(Value);
end;

procedure TfcxOrderedRecordSetProvider.SetCubeFieldsInIndex(const Value: TfcxCubeFieldsInIndex);
begin
  FCubeFieldsInIndex.Assign(Value);
  Update;
end;

procedure TfcxOrderedRecordSetProvider.Sort;
// !!! НАДО ПОДУМАТЬ О СОРТИРОВКЕ ПО СОСТАВНОМУ КЛЮЧУ!!!!
// ПОКА ПО ПРОСТОМУ
  procedure QuickSortOL(L, R: Integer);
  var
    i, j, p: integer;
    v, b: integer;

    procedure Insert(const l1, n1: integer);
    var
      i1, j1, v1: integer;
      x1: integer;

    begin
      for i1 := l1 + 1 to n1 do
      begin
        x1 := FRecordIndex[i1];
        v1 := l1;
        for j1 := i1 - 1 downto l1 do
        begin
          if CompareUVIndexes(FRecordIndex[j1], x1) <= 0 then
          begin
            v1 := j1 + 1;
            break;
          end;
          FRecordIndex[j1 + 1] := FRecordIndex[j1];

        end;
        FRecordIndex[v1] := x1;
      end;
    end;

  begin
    while (l < r) do
    begin
      if r - l <= 12 then
      begin
        Insert(l, r);
        exit;
      end;

      p := (l + r) shr 1;
// вопрос поиска середины - не ипользовать ли random?
      V := FRecordIndex[p];

      FRecordIndex[p] := FRecordIndex[l];

      //     сортируем l+1..r относительно центра
      i := succ(l);
      j := r;
      while true do
      begin
        while (i < j) and (CompareUVIndexes(v, FRecordIndex[i]) > 0) do
          inc(i);
        while (j >= i) and (CompareUVIndexes(FRecordIndex[j], v) > 0) do
          dec(j);
        if (i >= j) then
          break;
        b := FRecordIndex[i];

        FRecordIndex[i] := FRecordIndex[j];
        FRecordIndex[j] := b;

        dec(j);
        inc(i);
      end;
      //     центр в a[j]
      FRecordIndex[l] := FRecordIndex[j];
      FRecordIndex[j] := v;
      if ((j - l) <= (r - j)) then
      begin
        QuickSortOL(l, j - 1);
        l := succ(j);
      end
      else
      begin
        QuickSortOL(j + 1, r);
        r := pred(J);
      end;
    end;
  end;
  
begin
  if ValidProperties then
    QuickSortOL(0, FRecordCount-1);
end;

procedure TfcxOrderedRecordSetProvider.Update;
var
  I: Integer;
  Column: PfcxCubeDataColumn;
begin
  FCubeFieldsInIndex.Clear;
  for I := 0 to Columns.VisibleCount - 1 do
  begin
    Column := Columns.VisItems[I];
    if Column.SortIndex <> -1 then
    begin
      FCubeFieldsInIndex.SetCount(Max(FCubeFieldsInIndex.Count, Column.SortIndex + 1));
      FCubeFieldsInIndex.Items[Column.SortIndex].CubeField := Column.Field;
      FCubeFieldsInIndex.Items[Column.SortIndex].SortDirection := Column.SortDirection;
    end;
  end;
  if FCubeFieldsInIndex.Count = 0 then
    ClearIndex
  else
    CreateIndex;
end;

function TfcxOrderedRecordSetProvider.ValidProperties: Boolean;
begin
  Result := True;
end;

{ TfcxCubeOrderedRecordSetProvider }

function TfcxCubeOrderedRecordSetProvider.CompareUVIndexes(AIndex1, AIndex2: integer): integer;
var
  AFieldIndex, AUVIndex1, AUVIndex2: integer;
begin
  Result := 0;
  for AFieldIndex := 0 to CubeFieldsInIndex.Count - 1 do
  begin
    AUVIndex1 := FCube.SourceHolder.GetUniqueValueIndex(AIndex1, FCubeFieldsInIndex.Items[AFieldIndex].CubeField);
    AUVIndex2 := FCube.SourceHolder.GetUniqueValueIndex(AIndex2, FCubeFieldsInIndex.Items[AFieldIndex].CubeField);
    if AUVIndex1 = AUVIndex2 then
      Result := 0
    else
    begin
      Result := fcxCompareHelperDirection[CubeFieldsInIndex.Items[AFieldIndex].SortDirection, AUVIndex1 > AUVIndex2];
      break;
    end;
  end;
end;

constructor TfcxCubeOrderedRecordSetProvider.Create(ACube: TfcxCube);
begin
  inherited Create;
  FCube := ACube;
end;

function TfcxCubeOrderedRecordSetProvider.GetRecordCount: Integer;
begin
  if ValidProperties then
    Result := FCube.SourceHolder.FRecordsCount
  else
    Result := 0;
end;

function TfcxCubeOrderedRecordSetProvider.GetText(ACol,
  ARowIndex: Integer): String;
begin
  if ValidProperties then
    Result := StringToControl(FCube.SourceHolder.UniqueValueCaption[ARowIndex, Columns.VisItems[ACol].Field])
  else
    Result := '';
end;

procedure TfcxCubeOrderedRecordSetProvider.SetCube(ACube: TfcxCube);
begin
  if FCube <> ACube then
    CubeFieldsInIndex.Clear;
  FCube := ACube;
  Columns.Update(FCube);
  Update;
end;

function TfcxCubeOrderedRecordSetProvider.ValidProperties: Boolean;
begin
  Result := Assigned(FCube);
end;

{ TfcxCubeFieldsInIndex }

procedure TfcxCubeFieldsInIndex.Assign(ASource: TfcxCubeFieldsInIndex);
begin
  FFields := ASource.FFields;
end;

procedure TfcxCubeFieldsInIndex.Clear;
begin
  SetLength(FFields, 0);
end;

constructor TfcxCubeFieldsInIndex.Create;
begin
  FFields := nil;
end;

destructor TfcxCubeFieldsInIndex.Destroy;
begin
  Clear;
  inherited;
end;

function TfcxCubeFieldsInIndex.GetCount: Integer;
begin
  Result := Length(FFields);
end;

function TfcxCubeFieldsInIndex.GetFieldItem(
  AIndex: TfcxCommonField): PfcxCubeFieldInIndex;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if FFields[I].CubeField = AIndex then
    begin
      Result := @FFields[I];
      Exit;
    end;
  Result := nil;
end;

function TfcxCubeFieldsInIndex.GetFieldItemIndex(
  AIndex: TfcxCommonField): integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if FFields[I].CubeField = AIndex then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TfcxCubeFieldsInIndex.GetItem(AIndex: Integer): PfcxCubeFieldInIndex;
begin
  Result := @FFields[AIndex]
end;

procedure TfcxCubeFieldsInIndex.SetCount(const Value: Integer);
var
  I, OldLen: Integer;
begin
  OldLen := Length(FFields);
  SetLength(FFields, Value);
  if OldLen < Value then
    for I := OldLen to Value - 1 do
      FFields[I] := DefaultFieldInIndexValue;
end;

initialization
  TrialString := Chr(84) + Chr(114) + Chr(105) + Chr(97) + Chr(108); // 'Trial'
{
Поля сами грузят себя.
Поля содержат Списки Уникальных Значений (СУЗ)
Различаем Поля по способу загрузки:
- Из датасета (TfcxCommonUVField - TfcxDataSourceField, TfcxCommonUVField - TfcxDataSourceField - TfcxAttributeField )
- Из потока (TfcxCommonUVField - TfcxStreamField, TfcxCommonUVField - TfcxStreamField - TfcxAttributeStreamField)
- Сплиты (части поля Даты и Времени) (TfcxCommonStdPathField - TfcxCommonDatePathField, TfcxCommonStdPathField - TfcxCommonTimePathField)
- Вычисляемые (Стандартные (дата и время поля ДатаТайм), кастомы и т.п.) (TfcxCommonUVField - TfcxCalcField)
Также СУЗ (кроме сплита) может содержать дочерние СУЗы (атрибуты и сплиты).

Идеалогия меняется.
Поле куба должно быть одного типа, но оно содержит "загрузчик". И вот загрузчик будет разного типа!

Два режима загрузки данных в куб:
1. Из БД
2. Из потока

Порядок действий:
1. Из БД.
  InternalLoadFromDataSource
    Открытие датасетов - OpenSource
    Создание полей в датасоурсе при их отсутствии - FDataSource.AddFields
    Создание полей в кубе - FDataSource.InitFields; CreateFields;
    Загрузка - LoadData;
      Установка для корневых полей (кроме "вычисляемых после всех") признака процесса загрузки - TfcxCommonUVField(FFields.Items[i]).UniqueValues.Loading := True;
        В дочерних СУЗах устанавливаем признак процесса загрузки (для загружаемых из родительского датасета) ....SplitManager.AttributesManager.StartLoadMainReference
      Обходим основной датасет
        Для каждой записи датасета:
          Создаем строку исходного набора  - ASourceRec := FSourceHolder.AddRecord;
          Для каждого корневого поля (кроме "вычисляемых после всех") вызываем добавление значений из текущей записи - TfcxCommonUVField(FFields.Items[i]).FDataLoader.AddValue(ASourceRec[i], nil);
            Получаем значение - SourceField.DataField.GetData(ATempPointer)
            Конвертируем при необходимости
            Добавляем значение в СУЗ - FUniqueValues.AddNewValue(ATempPointer, AUValue)
1:<
              Создаём УЗ и добавляем его в список - FUVHash.FHashTable[bucket, L] := AddUValue(FTypeProcessor.NewUValue(AValue));
                Изменяем Capacity при необходимости (там что-то присходит, но не в процессе загрузки)
                Вызываем FSplitManager.AttributesManager.ChangeCapacity(True); (увеличиваем размер FListUVofPaths[i] для атрибутов, загружаемых из родительского датасета)
              Обрабатываем атрибуты, загружаемые из родительского датасета - FSplitManager.AddUValueMainReference(AUValue, FUVHash.FHashTable[bucket, L]);
                Именно атрибуты - FAttributesManager.AddUValueMainReference(AUValue, AUVIndex);
                  Грузим атрибуты, загружаемые из родительского датасета (Attribute[i].AttributeManager.FFromMasterSource) - FListUVofPaths[i][AUVIndex] := FListUValues[i].FAttributeManager.AddAttributeFrom(AUValue);
                    Получаем значение атрибута - GetFieldAttributeValue(APointer);
                      Получаем значение - Result := FDataField.GetData(APointer);
                      Конвертируем при необходимости
                    Добавляем значение в СУЗ - FAttributeUniqueValues.AddNewValue(APointer, Result);
                      Создаём УЗ и добавляем его в список - FUVHash.FHashTable[bucket, L] := AddUValue(FTypeProcessor.NewUValue(AValue));
                        Изменяем Capacity при необходимости (там что-то присходит, но не в процессе загрузки)
  >
              Закончили обрабатывать атрибуты
      Закончили проход основного датасета.
      Для каждого поля основного списка, кроме "вычисляемых после всех":
        Сортируем СУЗ (первичная сортировка по значению) - TfcxCommonUVField(FFields.Items[i]).UniqueValues.Sort(True);
          Сортируем атрибуты, загружаемые из родительского датасета - FSplitManager.SortMainReference
        Перенумерация СУЗ - TfcxCommonUVField(FFields.Items[i]).UniqueValues.SetIndex;
          Перенумеруем атрибуты, загружаемые из родительского датасета - FSplitManager.SetIndexMainReference
        Сбрасываем признак процесса загрузки - TfcxCommonUVField(FFields.Items[i]).UniqueValues.Loading := False;
          В дочерних СУЗах сбрасываем признак процесса загрузки (для загружаемых из родительского датасета) ....SplitManager.AttributesManager.StopLoadMainReference
        Установка Capacity для зависимых - TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).SetCaptionAndGroupCapacity;
          Пользовательское отображаемое значение - FCustomCaptionManager.SetCapacity(True);
          Менеджер групп - FGroupManager.SetCapacity(True);

        Вызываем загрузку атрибутов поля - TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).LoadAttributes(False);
2:<
          Устанавливаем Capacity для атрибутов (у списков для связи) - FSplitManager.SetCapacity(True, AOnlyForNotSavedFields);
            Части даты - FDatePathsManager.SetCapacity(ASetAllToNil);
            Части времени - FTimePathsManager.SetCapacity(ASetAllToNil);
            Атрибуты - FAttributesManager.SetCapacity(ASetAllToNil, AOnlyForNotSavedFields);
          Установка признака процесса загрузки у атрибутов, не загружаемых из родительского датасета - FSplitManager.StartLoad;
            Установка признака процесса загрузки - FListUValues[i].Loading := True;
              В дочерних СУЗах устанавливаем признак процесса загрузки (для загружаемых из родительского датасета) ....SplitManager.AttributesManager.StartLoadMainReference
          Грузим Reference атрибуты, загружаемые не из родительского датасета.
          Группируем их по совпадению датасетов
          Загружаем Reference атрибуты отдельно для каждого датасета - FSplitManager.FAttributesManager.LoadReferenceSource(FfcField, ADataSets[i], AAttributeManagerArr[i]);
            Для каждой записи датасета:
              Загрузка значений атрибутов - AddReferenceValue(ADataSet, AAttributeManagerArr);
                Получаем значение ключевого поля - FListUValues[AAttributeManagerArr[0]].AttributeManager.GetIDFieldAttributeValue(APointerId);
                Ищем базовое УЗ if FBaseUniqueValues.FindUValue(APointerId, AUValue, AUVIndex) then
                  Если нашли, то для каждого атрибута с этим датасетом:
                    Получаем значение поля атрибута - FListUValues[AAttributeManagerArr[i]].AttributeManager.GetFieldAttributeValue(APointerValue);
                    Добавляем новое УЗ (смотри 1:)
                      FListUValues[AAttributeManagerArr[i]].AttributeManager.AddNull
                      или
                      FListUValues[AAttributeManagerArr[i]].AddNewValue(APointerValue, ASplitUValue);
                  если не нашли, но надо грузить весь справочник, то для каждого атрибута с этим датасетом :
                    Получаем значение поля атрибута - FListUValues[AAttributeManagerArr[i]].AttributeManager.GetFieldAttributeValue(APointerValue);
                    Добавляем новое УЗ (смотри 1:)
                      FListUValues[AAttributeManagerArr[i]].AttributeManager.AddNull
                      или
                      FListUValues[AAttributeManagerArr[i]].AddNewValue(APointerValue, ASplitUValue);
              Устанавливаем ссылку на Null атрибут для несвязанных - SetNullForUnReferenced
          Закончили грузить Reference атрибуты

          Заполняем остальные атрибуты, если надо - FSplitManager.AddUValue(flist[i], i);
            Части даты - FDatePathsManager.AddUValue(AUValue, AUVIndex);
            Части времени - FTimePathsManager.AddUValue(AUValue, AUVIndex);
            Атрибуты - FAttributesManager.AddUValue(AUValue, AUVIndex);
          Создаём структуры для частей даты - FSplitManager.DatePathsManager.CreatePathUVsArray;
          Создаём структуры для частей времени - FSplitManager.TimePathsManager.CreatePathUVsArray;
          Вызов первичной сортировки атрибутов - FSplitManager.Sort(True);
            Сортируем атрибуты, не загружаемые из родительского датасета
          Вызов перенумерации атрибутов - FSplitManager.SetIndex;
            Перенумеруем атрибуты, не загружаемые из родительского датасета
          Сброс признака процесса загрузки у атрибутов, не загружаемых из родительского датасета - FSplitManager.StopLoad;
            У атрибутов, не загружаемых из родительского датасета
          Установка Capacity для зависимых - FSplitManager.SetCaptionAndGroupCapacity;
            Пользовательское отображаемое значение - FCustomCaptionManager.SetCapacity(True);
            Менеджер групп - FGroupManager.SetCapacity(True);

          Вызываем загрузку дочерних атрибутов у атрибутов - FSplitManager.LoadAttributes(AOnlyForNotSavedFields);
            Смотри 2:

          Вызов вторичной сортировки атрибутов (по значению атрибута) - FSplitManager.Sort(False);
            Сортируем атрибуты, не загружаемые из родительского датасета
              Перенумерация СУЗ при необходимости
          Заполняем и обрабатываем "вычисляемые после всех" атрибутов.
>
        Закончена загрузка атрибутов

      Для каждого поля основного списка, кроме "вычисляемых после всех":
        Сжатие - TfcxCommonUVField(FFields.Items[i]).UniqueValues.Recapacity;
          В том числе и всех атрибутов и их атрибутов и т.д....
        Сортируем СУЗ (вторичная сортировка по значению атрибута) - if TfcxCommonUVField(FFields.Items[i]).UniqueValues.Sort(False) then
          Перенумерация СУЗ - TfcxCommonUVField(FFields.Items[i]).UniqueValues.SetIndex;
      Заполняем и обрабатываем "вычисляемые после всех" поля основного списка
      Мы должны порвать все связи с TfcxDataSource FFields.DeleteLoaders;
    Закрытие датасетов - CloseSource;

2. Из потока
  InternalLoadCubeFromDeCompStream
  старый куб

  новый куб
    Создаём поля и атрибуты - Fields.LoadFieldsFromStream(ACubeStream);
    Грузим сохранённые УЗ полей и атрибутов - Fields.LoadUVsFromStream(ACubeStream);
    Заполняем несохранённые УЗ полей и атрибутов -     TfcxHackBaseUniqueValues(TfcxCommonUVField(FFields.Items[i]).UniqueValues).FillNonSavedUVs;
// Load Groups for not Saved fields
    end;

    dec(FChangeSemaphore);
// Source Holder
    if FSourceHolder = nil then
      FSourceHolder := TfcxSourceHolder.Create(Self);
    FSourceHolder.LoadFromStream(ACubeStream);

----------------------
Подгрузка данных.
-Из БД, когда загрузка ещё не финализирована (мультизагрузка)
  Реализуем на уровне TfcxDataSource
-Из БД, когда загрузка уже финализирована
  надо:
    сохранить схему? фильтры?
    сверить поля - связать поля в кубе с полями в датасоурсе (рекурсия в атрибуты)
    выполнить подгрузку
      дополнить уникальные + SourceHolder
      уникальные - ищем и добавляем если не нашли.
    восстановить схему? фильтры?
-Из куба

}
end.
