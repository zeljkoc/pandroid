{*******************************************************}
{                                                       }
{            FastCube 2 Unique Array unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxUniqueArray;
{$INCLUDE fcx.inc}

interface
uses
  Classes, db, SysUtils, Math, 
  fcxRes, fcxTypes,
  fcxList, fcxUniqueValue, fcxError, fcxDataSource,
  fcxXML, fcxAlerts
{$IFDEF FPC}
  , LCLType, LCLIntf
{$ELSE}
  , Windows
{$ENDIF}
{$IFDEF SQL_TYPES_EXTRA1}
  , SqlTimSt
{$ENDIF}
  ;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, Data.db, System.SysUtils, System.Math, 
  FMX.fcxRes, FMX.fcxTypes, FMX.fcxList, FMX.fcxUniqueValue, 
  FMX.fcxError, FMX.fcxDataSource, FMX.fcxXML, FMX.fcxAlerts
{$IFDEF SQL_TYPES_EXTRA1}
  , Data.SqlTimSt
{$ENDIF}
  ;
{$ENDIF FMX}

type

  TfcxBaseUniqueValues = class;
  TfcxAttributeUniqueValues = class;
  _fcxAttributeUVsArray = array[0..0] of TfcxAttributeUniqueValues;
  PfcxAttributeUVsArray = ^_fcxAttributeUVsArray;

// для управления хранением отображаемых значений, устанавливаемых пользователем
  TfcxCustomCaptionManager = class
  private
    FCaptionList: PfcxPfcxCharArray; // array of caption of unique values
    FUniqueValues: TfcxBaseUniqueValues;
    procedure AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer);
    procedure Clear;
    procedure Delete(AUVIndex: integer);
    procedure Insert(AUVIndex: integer);
    procedure SetCapacity(ASetAllToNil: boolean = False);
    procedure SetCaption(AUVIndex: Integer; const Value: TfcxString);
    procedure SaveToStream(ACubeStream: TStream);
    procedure LoadFromStream(ACubeStream: TStream);
    procedure AppendFromStream(ACubeStream: TStream);
  protected
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues); virtual;
    destructor Destroy; override;
  end;

  TfcxSplitManager = class;

// общие методы и свойства для управления хранением частей даты и времени
  TfcxStandardPathsManager = class // для частей даты и времени
  private
// UniqueValues родительского поля
    FUniqueValues: TfcxBaseUniqueValues;
    FPathsCount: integer; // число возможных частей
    FPathFields: PfcxPointerArray; // поля частей сплита
    FCount: Integer; // число активных частей
    procedure SaveGroupsToXML(AItem: TfcxXMLItem;
      AOnlyForNotSavedFields: boolean); virtual; abstract;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); virtual; abstract;
(*
    procedure SaveToStream(ACubeStream: TStream); virtual; abstract;
    procedure LoadFromStream(ACubeStream: TStream); virtual; abstract;
*)
    procedure ClearGroups; virtual; abstract;
  protected
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues); virtual;
    destructor Destroy; override;
    property Count: Integer read FCount;
    property PathsCount: Integer read FPathsCount;
//    property SSplitPathType: TfcxStandardSplitPathType read FSSplitPathType;
  end;

// для управления хранением частей даты
  TfcxDatePathsManager = class(TfcxStandardPathsManager) // части даты
  private
    function GetUseDatePath(ADatePath: TfcxDateType): Boolean;
    function GetDatePathField(ADatePath: TfcxDateType): Pointer;
    procedure AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      ADateTypeSet: TfcxDateTypes = []; AOnlyForNotSavedFields: boolean = False);
    procedure InsertUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      ADateTypeSet: TfcxDateTypes = []; AOnlyForNotSavedFields: boolean = False);
    procedure Delete(AUVIndex: integer);
    procedure Insert(AUVIndex: integer);
    procedure SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
    procedure Recapacity(AOnlyForNotSavedFields: boolean = False);
    procedure SaveGroupsToXML(AItem: TfcxXMLItem;
      AOnlyForNotSavedFields: boolean); override;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); override;
(*
    procedure SaveToStream(ACubeStream: TStream); override;
    procedure LoadFromStream(ACubeStream: TStream); override;
*)
    procedure ClearGroups; override;
    function GetCaptionAtIndex(ADatePath: TfcxDateType;
      AIndex: integer): TfcxString;
    function GetCaptionAtValue(ADatePath: TfcxDateType;
      AValue: Word): TfcxString;
  protected
    function AddDatePath(ADatePath: TfcxDateType): integer;
    procedure SetDatePaths(ADatePaths: TfcxDateTypes);
    procedure RemoveDatePath(ADatePath: TfcxDateType);
    procedure InternalAddDatePathSaved(ADatePathField: Pointer);
    procedure CreatePathUVsArray(AOnlyForNotSavedFields: boolean = False);
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues); override;
    property UseDatePath[ADatePath: TfcxDateType]: Boolean read GetUseDatePath;
    property DatePathField[ADatePath: TfcxDateType]: Pointer read GetDatePathField;
    property CaptionAtIndex[ADatePath: TfcxDateType; AIndex: integer]: TfcxString read GetCaptionAtIndex;
    property CaptionAtValue[ADatePath: TfcxDateType; AValue: Word]: TfcxString read GetCaptionAtValue;
  end;

// для управления хранением частей времени
  TfcxTimePathsManager = class(TfcxStandardPathsManager) // части времени
  private
    function GetUseTimePath(ATimePath: TfcxTimeType): Boolean;
    function GetTimePathField(ATimePath: TfcxTimeType): Pointer;
    procedure AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      ATimeTypeSet: TfcxTimeTypes = []; AOnlyForNotSavedFields: boolean = False);
    procedure InsertUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      ATimeTypeSet: TfcxTimeTypes = []; AOnlyForNotSavedFields: boolean = False);
    procedure Delete(AUVIndex: integer);
    procedure Insert(AUVIndex: integer);
    procedure SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
    procedure Recapacity(AOnlyForNotSavedFields: boolean = False);
    procedure SaveGroupsToXML(AItem: TfcxXMLItem;
      AOnlyForNotSavedFields: boolean); override;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); override;
(*
    procedure SaveToStream(ACubeStream: TStream); override;
    procedure LoadFromStream(ACubeStream: TStream); override;
*)
    procedure ClearGroups; override;
    function GetCaptionAtIndex(ATimePath: TfcxTimeType;
      AIndex: integer): TfcxString;
    function GetCaptionAtValue(ATimePath: TfcxTimeType;
      AValue: integer): TfcxString;
  protected
    function AddTimePath(ATimePath: TfcxTimeType): integer;
    procedure SetTimePaths(ATimePaths: TfcxTimeTypes);
    procedure RemoveTimePath(ATimePath: TfcxTimeType);
    procedure InternalAddTimePathSaved(ATimePathField: Pointer);
    procedure CreatePathUVsArray(AOnlyForNotSavedFields: boolean = False);
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues); override;
    property UseTimePath[ATimePath: TfcxTimeType]: Boolean read GetUseTimePath;
    property TimePathField[ATimePath: TfcxTimeType]: Pointer read GetTimePathField;
    property CaptionAtIndex[ATimePath: TfcxTimeType; AIndex: integer]: TfcxString read GetCaptionAtIndex;
    property CaptionAtValue[ATimePath: TfcxTimeType; AValue: integer]: TfcxString read GetCaptionAtValue;
  end;

  TfcxAttributesManager = class
  private
    FAttributesCount: integer;
    FBaseUniqueValues: TfcxBaseUniqueValues;
// массив значений атрибутов, соответствующих УЗ базового поля
    FListUVofPaths: PfcxArrCUVArray; // List of arrays of pointers to UV of Split Paths
// массив СУЗ-ов атрибутов
    FListUValues: PfcxAttributeUVsArray;
    procedure Clear;
    procedure ClearGroups;
    procedure Delete(AUVIndex: integer);
    procedure Insert(AUVIndex: integer);
// первичная установка Capacity
    procedure SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
// изменение Capacity для Reference атрибутов
    procedure ChangeCapacity;
    procedure Recapacity(AOnlyForNotSavedFields: boolean = False);
    procedure SetIndex(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure SetIndexMainReference;
    procedure Sort(AFirstStep: boolean; ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure SortMainReference;
    procedure StartLoad(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure StopLoad(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure LoadAttributes(AOnlyForNotSavedFields: boolean);
    procedure FillNonSavedUVs;
    procedure StartLoadMainReference;
    procedure StopLoadMainReference;
    function GetAttribute(AAttributeIndex: integer): TfcxAttributeUniqueValues;
    function GetAttributeUVIndex(AAttributeIndex,
      AMasterUVIndex: integer): Integer;
    procedure SaveGroupsToXML(AItem: TfcxXMLItem;
      AOnlyForNotSavedFields: boolean); virtual;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); virtual;
    function GetAttributeByName(
      AAttributeName: TfcxString): TfcxAttributeUniqueValues;
    procedure AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False); overload;
    procedure AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      AAttributeUniqueValues: TfcxAttributeUniqueValues); overload;
    procedure AddUValueNonReference(AUValue: PfcxCommonUV; AUVIndex: integer);
    procedure AddUValueMainReference(AUValue: PfcxCommonUV; AUVIndex: integer);
    procedure AddUValueNull(AUVIndex: integer; ACalculateAfterAll: Boolean;
      AOnlyForNotSavedFields: boolean = False); overload;
    procedure AddUValueNull(AUVIndex: integer; AAttributeUniqueValues: TfcxAttributeUniqueValues); overload;
    procedure AddUValueNullNonReference(AUVIndex: integer);
    procedure AddUValueNullReference(AUVIndex: integer);
    procedure AddUValueNullMainReference(AUVIndex: integer);
    procedure LoadReferenceSource(AfcField: TObject; ADataSet: TfcxDataSet; AAttributeManagerArr: Array of integer);
    procedure AppendReferenceSource(AfcField: TObject; ADataSet: TfcxDataSet; AAttributeManagerArr: Array of integer);
    procedure SetNullForUnReferenced;
    function GetExistsDateAttribute: boolean;
    function GetExistsTimeAttribute: boolean;
    procedure SetExistsDateAttribute(const Value: boolean);
    procedure SetExistsTimeAttribute(const Value: boolean);
  protected
    function AddAttribute(ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
    function AddAttributeInRunTime(AField: Pointer; AFieldProperties: TfcxFieldProperties;
        AAttributeType: TfcxAttributeType): TfcxAttributeUniqueValues;
(*
    function AddDateAttribute(ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
    function AddTimeAttribute(ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
    function AddReferenceAttribute(ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
*)
    procedure AddAttributeSaved(AField: Pointer);
    procedure RemoveAttribute(AAttributeName: TfcxString); overload;
    procedure RemoveAttribute(AAttributeIndex: integer); overload;
    procedure AddReferenceValue(ADataSet: TfcxDataSet; AAttributeManagerArr: Array of integer);
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues); virtual;
    destructor Destroy; override;
    function FindAttribute(AAttributeName: TfcxString): integer;
    property AttributesCount: Integer read FAttributesCount;
    property BaseUniqueValues: TfcxBaseUniqueValues read FBaseUniqueValues;
    property AttributeUVIndex[AAttributeIndex: integer; AMasterUVIndex: integer]: Integer read GetAttributeUVIndex;
    property Attribute[AAttributeIndex: integer]: TfcxAttributeUniqueValues read GetAttribute; default;
    property AttributeByName[AAttributeName: TfcxString]: TfcxAttributeUniqueValues read GetAttributeByName;
    property ExistsDateAttribute: boolean read GetExistsDateAttribute write SetExistsDateAttribute;
    property ExistsTimeAttribute: boolean read GetExistsTimeAttribute write SetExistsTimeAttribute;
  end;

// для управления хранением сплитов всех типов
  TfcxSplitManager = class
  private
    FUniqueValues: TfcxBaseUniqueValues;
    FCanUseDateSplit: boolean;
    FUseDateSplit: boolean;
    FDatePathsManager: TfcxDatePathsManager;
    FCanUseTimeSplit: boolean;
    FUseTimeSplit: boolean;
    FTimePathsManager: TfcxTimePathsManager;
    FAttributesManager: TfcxAttributesManager;
{
    FCreateTime: boolean;
    FCreateDate: boolean;
}
    procedure SetUseDateSplit(const Value: boolean);
    function GetDatePathsManager: TfcxDatePathsManager;
    procedure SetUseTimeSplit(const Value: boolean);
    function GetTimePathsManager: TfcxTimePathsManager;
    procedure AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
      ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure AddUValueMainReference(AUValue: PfcxCommonUV; AUVIndex: integer);
    procedure Clear;
    procedure ClearGroups;
    procedure Delete(AUVIndex: integer);
    procedure Insert(AUVIndex: integer);
    procedure SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
    procedure SetIndex(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure SetIndexMainReference;
    procedure Recapacity(AOnlyForNotSavedFields: boolean = False);
    procedure Sort(AFirstStep: boolean; ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure SortMainReference;
    procedure StartLoad(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure StopLoad(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
    procedure LoadAttributes(AOnlyForNotSavedFields: boolean);
    procedure FillNonSavedUVs;
    procedure SetCaptionAndGroupCapacity(ACalculateAfterAll: boolean);
    function GetDatePathsCount: integer;
    function GetTimePathsCount: integer;
    procedure SaveGroupsToXML(AItem: TfcxXMLItem;
      AOnlyForNotSavedFields: boolean);
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem);
    function GetAttributesCount: integer;
    function GetAttributeDisplayValue(AUVIndex,
      AAttributeIndex: integer): TfcxString;
  protected
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues); virtual;
    destructor Destroy; override;

    property CanUseDateSplit: boolean read FCanUseDateSplit;
    property UseDateSplit: boolean read FUseDateSplit write SetUseDateSplit;
    property DatePathsCount: integer read GetDatePathsCount;
    property DatePathsManager: TfcxDatePathsManager read GetDatePathsManager;

    property CanUseTimeSplit: boolean read FCanUseTimeSplit;
    property UseTimeSplit: boolean read FUseTimeSplit write SetUseTimeSplit;
    property TimePathsCount: integer read GetTimePathsCount;
    property TimePathsManager: TfcxTimePathsManager read GetTimePathsManager;

    property AttributesCount: integer read GetAttributesCount;
    property AttributesManager: TfcxAttributesManager read FAttributesManager;
    property AttributeDisplayValue[AUVIndex: Integer; AAttributeIndex: integer]: TfcxString read GetAttributeDisplayValue;
  end;

  TfcxUVHash = class
  private
    FTypeProcessor: TfcxDataTypeProcessor;
    FHashTable: PfcxArrIntegerArray;
    FHashCount: PfcxIntegerArray;
    procedure ClearHash;
    procedure CreateHash;
  protected
  public
    constructor Create(ATypeProcessor: TfcxDataTypeProcessor); virtual;
    destructor Destroy; override;
  end;

// базовый класс для управления группами
  TfcxCommonGroupManager = class
  private
    FUniqueValues: TfcxBaseUniqueValues;
    FCount: Integer;
    FGroupParentList: PfcxPGVArray; // array of group values for each Unique Value (групповые значения, соответствующие уникальному)
    FListGV: PfcxPGVArray;  // group values array
    FOtherGV: PfcxGroupValue;
    FOtherCaption: TfcxString;
    FExistsOther: Boolean;
    FCountInAllGroups: integer;
    function IndexUVInGroup(AUVIndex: integer; var AIndexOfUV: integer; AGValue: PfcxGroupValue = nil): boolean;
    procedure SetOtherCaption(const Value: TfcxString);
    function GetCaptionByGV(AGValue: PfcxGroupValue): TfcxString;
    procedure SetCaptionByGV(AGValue: PfcxGroupValue; const Value: TfcxString);
    procedure AddInGroupInternal(AUValue: PfcxCommonUV; AGValue: PfcxGroupValue);
    procedure RemoveFromGroupInternal(AUValue: PfcxCommonUV; ATransfer: Boolean = False);
    function GetCaption(AGroupIndex: integer): TfcxString;
    procedure SetCaption(AGroupIndex: integer; const Value: TfcxString);
    function GetCountInGroup(AGroupIndex: integer): Integer;
    function GetCaptionInGroup(AGroupIndex,
      AIndexInGroup: Integer): TfcxString; virtual; abstract;
    function GetUVIndexInGroup(AGroupIndex,
      AIndexInGroup: Integer): Integer; virtual; abstract;
    function GetUVIndexOfOther(AIndexInGroup: integer): integer;
    function GetOtherGroupIndex: Integer;

    procedure SetCapacity(ASetAllToNil: boolean = False);
    procedure Delete(AUVIndex: integer);
    procedure Insert(AUVIndex: integer);
//    function AddUValue(AUVIndex: integer): Integer; virtual;
    function UVSCount: integer; virtual; abstract;
    function UVSCapacity: integer; virtual; abstract;
    procedure InitOtherGroup; virtual; abstract;

    procedure FreeGV(AGValue: PfcxGroupValue);
    function NewGV(const ACaption: TfcxString; AIsOther: boolean): PfcxGroupValue;
    procedure DeleteByIndex(AIndex: integer);

    procedure AddInGroup(AUValue: PfcxCommonUV; AGValue: PfcxGroupValue);
    procedure Clear;
    function CreateGV(const ACaption: TfcxString; AIsOther: Boolean = False): PfcxGroupValue;
    procedure RemoveFromGroup(AUValue: PfcxCommonUV);
    property CaptionByGV[AGValue: PfcxGroupValue]: TfcxString read GetCaptionByGV write SetCaptionByGV;
    function GetCountInNonGroups: Integer;
    function GetCaptionInNonGroups(AIndexInGroup: Integer): TfcxString; virtual; abstract;
    function GetUVIndexInNonGroups(AIndexInGroup: Integer): Integer; virtual; abstract;
    procedure StartChange; virtual; abstract;
    procedure StopChange(AChanges: TfcxChangesInCube = []; AIndex: integer = -1); virtual; abstract;
    function GetIndexFromUV(AUValue: PfcxCommonUV): integer; virtual;
    function GetGroupIndex(AGroupCaption: TfcxString): integer;
  protected
  public
    procedure SaveGroupsToXML(AItem: TfcxXMLItem); virtual;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); virtual;
    procedure SaveToStream(ACubeStream: TStream); virtual;
    procedure LoadFromStream(ACubeStream: TStream); virtual;
    procedure AppendFromStream(ACubeStream: TStream); virtual;

    constructor Create(AUniqueValues: TfcxBaseUniqueValues); overload; virtual;
    destructor Destroy; override;

    procedure DeleteGroup(AGroupIndex: integer);
    function CreateGroup(const AGroupCaption: TfcxString): PfcxGroupValue;
    function CreateOtherGroup: PfcxGroupValue;
    procedure AddUVInGroup(AUVIndex, AGroupIndex: integer); virtual; abstract;
    procedure AddUVValueInGroup(AUVValue: Variant; AGroupIndex: integer); virtual; abstract;
    procedure RemoveUVFromGroup(AUVIndex: integer); virtual; abstract;

    property GroupIndex[AGroupCaption: TfcxString]: integer read GetGroupIndex;
    property Caption[AGroupIndex: integer]: TfcxString read GetCaption write SetCaption;
    property CaptionInGroup[AGroupIndex, AIndexInGroup: Integer]: TfcxString read GetCaptionInGroup;
    property UVIndexInGroup[AGroupIndex, AIndexInGroup: Integer]: Integer read GetUVIndexInGroup;
    property CountInGroup[AGroupIndex: integer]: Integer read GetCountInGroup;
    property CountInNonGroups: Integer read GetCountInNonGroups;
    property CaptionInNonGroups[AIndexInGroup: Integer]: TfcxString read GetCaptionInNonGroups;
    property UVIndexInNonGroups[AIndexInGroup: Integer]: Integer read GetUVIndexInNonGroups;
    property ExistsOther: Boolean read FExistsOther;
    property GroupCount: Integer read FCount;
    property UVIndexOfOther[AIndexInGroup: integer]: Integer read GetUVIndexOfOther;
    property OtherGroupIndex: Integer read GetOtherGroupIndex;
    property OtherGV: PfcxGroupValue read FOtherGV;
    property OtherCaption: TfcxString read FOtherCaption write SetOtherCaption;
  end;

// класс для управления группами в полях на основе UniqueValues (основные поля куба)
  TfcxUVSGroupManager = class(TfcxCommonGroupManager)
  private
    function UVSCount: integer; override;
    function UVSCapacity: integer; override;
    procedure InitOtherGroup; override;
    function GetCaptionInGroup(AGroupIndex,
      AIndexInGroup: Integer): TfcxString; override;
    function GetUVIndexInGroup(AGroupIndex,
      AIndexInGroup: Integer): Integer; override;
    function GetCaptionInNonGroups(AIndexInGroup: Integer): TfcxString; override;
    function GetUVIndexInNonGroups(AIndexInGroup: Integer): Integer; override;
    procedure StartChange; override;
    procedure StopChange(AChanges: TfcxChangesInCube = []; AIndex: integer = -1); override;
  protected
  public
    procedure AddUVInGroup(AUVIndex, AGroupIndex: integer); override;
    procedure AddUVValueInGroup(AUVValue: Variant; AGroupIndex: integer); override;
    procedure RemoveUVFromGroup(AUVIndex: integer); override;
  end;

// класс для управления группами в полях сплитов даты
  TfcxDatePathGroupManager = class(TfcxCommonGroupManager)
  private
    FDatePath: TfcxDateType;
    FDatePathProcessor: TfcxCommonDatePathDTP;
    function UVSCount: integer; override;
    function UVSCapacity: integer; override;
    procedure InitOtherGroup; override;
    function GetCaptionInGroup(AGroupIndex,
      AIndexInGroup: Integer): TfcxString; override;
    function GetUVIndexInGroup(AGroupIndex,
      AIndexInGroup: Integer): Integer; override;
    function GetCaptionInNonGroups(AIndexInGroup: Integer): TfcxString; override;
    function GetUVIndexInNonGroups(AIndexInGroup: Integer): Integer; override;
    procedure StartChange; override;
    procedure StopChange(AChanges: TfcxChangesInCube = []; AIndex: integer = -1); override;
    function GetIndexFromUV(AUValue: PfcxCommonUV): integer; override;
  protected
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues; ADatePath: TfcxDateType); overload; virtual;
    procedure AddUVInGroup(AUVIndex, AGroupIndex: integer); override;
    procedure AddUVValueInGroup(AUVValue: Variant; AGroupIndex: integer); override;
    procedure RemoveUVFromGroup(AUVIndex: integer); override;
  end;

// класс для управления группами в полях сплитов времени
  TfcxTimePathGroupManager = class(TfcxCommonGroupManager)
  private
    FTimePath: TfcxTimeType;
    FTimePathProcessor: TfcxCommonTimePathDTP;
    function UVSCount: integer; override;
    function UVSCapacity: integer; override;
    procedure InitOtherGroup; override;
    function GetCaptionInGroup(AGroupIndex,
      AIndexInGroup: Integer): TfcxString; override;
    function GetUVIndexInGroup(AGroupIndex,
      AIndexInGroup: Integer): Integer; override;
    function GetCaptionInNonGroups(AIndexInGroup: Integer): TfcxString; override;
    function GetUVIndexInNonGroups(AIndexInGroup: Integer): Integer; override;
    procedure StartChange; override;
    procedure StopChange(AChanges: TfcxChangesInCube = []; AIndex: integer = -1); override;
    function GetIndexFromUV(AUValue: PfcxCommonUV): integer; override;
  protected
  public
    constructor Create(AUniqueValues: TfcxBaseUniqueValues; ATimePath: TfcxTimeType); overload; virtual;
    procedure AddUVInGroup(AUVIndex, AGroupIndex: integer); override;
    procedure AddUVValueInGroup(AUVValue: Variant; AGroupIndex: integer); override;
    procedure RemoveUVFromGroup(AUVIndex: integer); override;
  end;

// Базовый класс списка уникальных значений частей даты и времени
  TfcxStandardPathUniqueValues = class
  private
    FfcField: TObject; //TfcxCommonField;
//Groups Менеджер группировок
    FGroupManager: TfcxCommonGroupManager;
    FWithGroup: Boolean;
// Обработчик и хранитель данных
    FDataTypeProcessor: TfcxCommonStdPathDTP;
    function GetGroupIndexOfUV(AUVIndex: Integer): Integer;
    function GetGroupManager: TfcxCommonGroupManager;
    function GetUVInGroup(AUVIndex: Integer): Boolean;
    procedure SetWithGroup(const Value: Boolean);
    function CreateGroupManager: TfcxCommonGroupManager; virtual; abstract;
    function GetUVIndex(AUVVarValue: Variant): Integer;
  public
    constructor Create(AfcField: TObject); overload; virtual;
    destructor Destroy; override;
//Groups
    property GroupManager: TfcxCommonGroupManager read GetGroupManager; // group manager
    property UVInGroup[AUVIndex: Integer]: Boolean read GetUVInGroup;
    property GroupIndexOfUV[AUVIndex: Integer]: Integer read GetGroupIndexOfUV;
    property WithGroup: Boolean read FWithGroup write SetWithGroup;
    property DataTypeProcessor: TfcxCommonStdPathDTP read FDataTypeProcessor;
    property UVIndex[AUVVarValue: Variant]: Integer read GetUVIndex;
  end;


// Класс списка уникальных значений частей даты
  TfcxDatePathUniqueValues = class(TfcxStandardPathUniqueValues)
  private
    FDateType: TfcxDateType;
    function CreateGroupManager: TfcxCommonGroupManager; override;
    function GetDatePathProcessor: TfcxCommonDatePathDTP;
  public
    constructor Create(AfcField: TObject; ADateType: TfcxDateType); overload; virtual;
    property DatePathProcessor: TfcxCommonDatePathDTP read GetDatePathProcessor;
    property DateType: TfcxDateType read FDateType;
  end;

// Класс списка уникальных значений частей времени
  TfcxTimePathUniqueValues = class(TfcxStandardPathUniqueValues)
  private
    FTimeType: TfcxTimeType;
    function CreateGroupManager: TfcxCommonGroupManager; override;
    function GetTimePathProcessor: TfcxCommonTimePathDTP;
  public
    constructor Create(AfcField: TObject; ATimeType: TfcxTimeType); overload; virtual;
    property TimePathProcessor: TfcxCommonTimePathDTP read GetTimePathProcessor;
    property TimeType: TfcxTimeType read FTimeType;
  end;

  TfcxGetCaptionFunction = function(AUVIndex: Integer): TfcxString of object;

// Класс с общими свойствами списка уникальных значений:
  TfcxBaseUniqueValues = class
  private
    FLoadingFromStream: boolean;
    FList: PfcxCUVArray; // array of unique values
// поле со значениями
    FfcField: TObject; //TfcxCommonField;
    FCapacity: Integer;
    FCount: Integer;
// наличие NULL значения в списке
    FHasNull: Boolean;
// указатель на NULL значение
    FNullValue: PfcxCommonUV;
//Captions Менеджер отображаемых значений
    FCustomCaptionManager: TfcxCustomCaptionManager;
    FWithCustomCaption: boolean;
//Splits Менеджер сплитов
    FSplitManager: TfcxSplitManager;
// признак процесса заполнения СУЗ
    FLoading: boolean;
// тип данных в СУЗ
    FDataType: TfcxDataType;
// Variant тип данных
    FVarType: Word;
// Обработчик данных
    FTypeProcessor: TfcxDataTypeProcessor;
//Groups Менеджер группировок
    FGroupManager: TfcxCommonGroupManager;
    FWithGroup: Boolean;
//Hash
    FUVHash: TfcxUVHash;
//Order
    FOrderSourceAttribute: TfcxString;
    FOrderSourceAttributeIndex: integer;
//Captions
    FCaptionSourceAttribute: TfcxString;
    FCaptionSourceAttributeIndex: integer;
    procedure SetWithCustomCaption(const Value: Boolean); virtual;
    function GetCaptionCustomAttribute(AUVIndex: Integer): TfcxString;
    function GetCaptionAttribute(AUVIndex: Integer): TfcxString;
    procedure SetCaptionSourceAttribute(const Value: TfcxString);
    procedure SetOrderSourceAttribute(const Value: TfcxString);
//Отображаемое значение формируется из самого значения
    function GetCaptionSimple(AUVIndex: Integer): TfcxString;
//Отображаемое значение может хранится отдельно
    function GetCaptionCustom(AUVIndex: Integer): TfcxString;
//Groups
    function GetGroupManager: TfcxCommonGroupManager;
    function GetUVInGroup(AUVIndex: Integer): Boolean;
    function GetGroupIndexOfUV(AUVIndex: Integer): Integer;
    procedure SetWithGroup(const Value: Boolean);

    procedure SetLoading(const Value: boolean); virtual;
    procedure SetCapacity(const Value: Integer); virtual;
    procedure SetCount(const Value: Integer);
    function GetIgnoreCase: Boolean;
    function GetIsNull(AIndex: Integer): Boolean;
    function GetUValue(AIndex: Integer): PfcxCommonUV;
    function GetValueAsVariant(AUValue: PfcxCommonUV): Variant;
    function GetValueAsVariantByIndex(AUIndex: integer): Variant;
    function GetNullCaption: TfcxString;
    procedure SetNullCaption(const Value: TfcxString);
    function GetFieldIndex: Integer;

    procedure CreateTypeProcessor(ADataType: TfcxDataType; ADataTypeProcessorClass: TfcxDataTypeProcessorClass = nil);
// only for loading time
    function AddUValue(AUValue: PfcxCommonUV): Integer; virtual;
// only for work time
    function Insert(AValue: Pointer; AUVIndex: integer): PfcxCommonUV; virtual;
    function GetUVIndex(AUVVarValue: Variant): Integer;
  protected
    FTempCount: Integer;
    FTempList: PfcxCUVArray; // array of unique values
    FIndexList: PfcxCUVArray; // array of unique values sorted by value (for fields with OrderSourceAttribute)
    FAddedUValues: Boolean;
// only to use in opened cube!!! for date, time and custom attribute !!! ???
    procedure LoadAddons(AAttributeUniqueValuesArr: Array of TfcxAttributeUniqueValues; ADateTypeSet: TfcxDateTypes; ATimeTypeSet: TfcxTimeTypes); overload; virtual;
// only to use in opened cube!!! for date and time im attributes !!! ???
//    procedure LoadAddonsForAttributes(ADateTypeSet: TfcxDateTypes; ATimeTypeSet: TfcxTimeTypes); virtual; abstract;
// only for global loading after open and load from stream
    procedure LoadAttributes(AOnlyForNotSavedFields: boolean);
    procedure AppendAttributes(AOnlyForNotSavedFields: boolean);
    procedure FillNonSavedUVs;
    procedure LoadAddons; overload; virtual;
    procedure SetCaptionAndGroupCapacity;
    procedure SortIndexList;
  public
// активная процедура получения отображаемого значения
    GetCaption: TfcxGetCaptionFunction;
// присвоение отображаемого значения
    procedure SetCaption(AUVIndex: Integer; const Value: TfcxString);
    constructor Create(AfcField: TObject; AWithCustomCaption: boolean; ADataTypeProcessorClass: TfcxDataTypeProcessorClass = nil); overload; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure ClearGroups; virtual;
    procedure RemoveLinksToDataSource; virtual;
// добавление нового значения (дан указатель на значение) в СУЗ
    function AddNewValue(AValue: Pointer; var AUValue: PfcxCommonUV): boolean;
// добавление нового значения (дано значение тип Variant) в СУЗ
    function AddNewVarValue(AVarValue: Variant; var AUValue: PfcxCommonUV): boolean;
// поиск в СУЗ значения (дан указатель на значение). Если AFreeValue = True, то AValue уничтожается.
    function FindUValue(AValue: Pointer; var AUValue: PfcxCommonUV; var AUVIndex: integer; AFreeValue: boolean = False): boolean;
// Удаление значения с индексом AUVIndex из СУЗ
    procedure Delete(AUVIndex: integer); virtual;
// Выделение памяти под значение AVariant
    function VarValueToPointer(AVariant: Variant): pointer;
    procedure SaveGroupsToXML(AItem: TfcxXMLItem;
      AOnlyForNotSavedFields: boolean); virtual;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); virtual;
// запись СУЗ в поток версии 2
    procedure SaveToStream(ACubeStream: TStream); virtual;
// чтение СУЗ из потока версии 2
    procedure LoadFromStream(ACubeStream: TStream); virtual;
    procedure AppendFromStream(ACubeStream: TStream); virtual;
// чтение СУЗ из потока версии 1
    procedure LoadFromOldStream(ACubeStream: TStream; AMajorVersion, AMinorVersion: byte); virtual;
// сортировка СУЗ и связанных с ним массивов
    function Sort(AFirstStep: boolean): boolean; virtual;
// простановка уникального индекса значений СУЗ
    procedure SetIndex; virtual;
    procedure Recapacity; virtual;
//Splits
    property SplitManager: TfcxSplitManager read FSplitManager; // Split manager

    property Loading: boolean read FLoading write SetLoading;
    property fcField: TObject read FfcField;
    property DataType: TfcxDataType read FDataType;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property IsNull[AIndex: Integer]: Boolean read GetIsNull;
    property HasNull: Boolean read FHasNull;
    property NullValue: PfcxCommonUV read FNullValue;
//Groups
    property GroupManager: TfcxCommonGroupManager read GetGroupManager; // group manager
    property UVInGroup[AUVIndex: Integer]: Boolean read GetUVInGroup;
    property GroupIndexOfUV[AUVIndex: Integer]: Integer read GetGroupIndexOfUV;
    property WithGroup: Boolean read FWithGroup write SetWithGroup;

    property UValue[AIndex: Integer]: PfcxCommonUV read GetUValue; default;
    property UVIndex[AUVVarValue: Variant]: Integer read GetUVIndex;
    property ValueAsVariant[AUValue: PfcxCommonUV]: Variant read GetValueAsVariant;
    property ValueAsVariantByIndex[AUIndex: integer]: Variant read GetValueAsVariantByIndex;
    property IgnoreCase: Boolean read GetIgnoreCase;
    property NullCaption: TfcxString read GetNullCaption write SetNullCaption;
    property FieldIndex: Integer read GetFieldIndex;
//Captions
//    property Caption[AUVIndex: Integer]: string read GetCaption write SetCaption;
    property WithCustomCaption: Boolean read FWithCustomCaption write SetWithCustomCaption;
    property CaptionSourceAttribute: TfcxString read FCaptionSourceAttribute write SetCaptionSourceAttribute;
    property OrderSourceAttribute: TfcxString read FOrderSourceAttribute write SetOrderSourceAttribute;

    property DataTypeProcessor: TfcxDataTypeProcessor read FTypeProcessor;
  end;

// СУЗ для атрибутов
  TfcxAttributeUniqueValues = class(TfcxBaseUniqueValues)
  private
    FBaseUniqueValues: TfcxBaseUniqueValues;
    FFromMasterSource: Boolean;
    FLoadAllValues: Boolean;
    FAttributeType: TfcxAttributeType;
    FDataField: TfcxReferenceDataField;
    FIDDataField: TfcxReferenceDataField;
    FCalculateAfterAll: Boolean;
    function GetAttributeDisplayLabel: TfcxString;
    function GetAttributeName: TfcxString;
    Function AddNull: PfcxCommonUV;
    Function AddAttributeFrom(AUValue: PfcxCommonUV): PfcxCommonUV;
//    procedure SetCapacity(const Value: Integer); override;
  protected
    function GetAttributeValue(AMasterValue: Variant; var AAttributeValue: Variant): Boolean;
    function GetFieldAttributeValue(var APointer: Pointer): Boolean;
    function GetIDFieldAttributeValue(var APointer: Pointer): Boolean;
    procedure SetDataFields(ADataField, AIDDataField: TfcxReferenceDataField);
  public
// Reference and Custom Path field, Caption and Order field
    constructor CreateAttribute(AfcField: TObject; ABaseUniqueValues: TfcxBaseUniqueValues; AWithCustomCaption: boolean; AAttributeType: TfcxAttributeType);
    constructor CreateReferenceAttribute(AfcField: TObject;
      ADataField: TfcxReferenceDataField; AIDDataField: TfcxAddonReferenceDataField; ABaseUniqueValues: TfcxBaseUniqueValues;
      AWithCustomCaption: boolean; ALoadAllValues: Boolean);
    procedure RemoveLinksToDataSource; override;
    property FromMasterSource: Boolean read FFromMasterSource;
    property LoadAllValues: Boolean read FLoadAllValues;
    property AttributeType: TfcxAttributeType read FAttributeType;
    property AttributeName: TfcxString read GetAttributeName;
    property AttributeDisplayLabel: TfcxString read GetAttributeDisplayLabel;
    property CalculateAfterAll: Boolean read FCalculateAfterAll;
  end;

var
  fcDateTypeLength, fcTimeTypeLength, fcPointerSize: integer;

implementation
//VCL uses section
{$IFNDEF FMX}
uses
  TypInfo, StrUtils
{$IFDEF SQL_TYPES_EXTRA1}
//{$IFNDEF fpc}
  ,FMTBcd
{$ENDIF}
{$IFDEF DELPHI_6UP}
  ,Variants, RTLConsts
{$ELSE}
  ,consts
{$ENDIF}
{$IFDEF Delphi_9UP}
  ,WideStrUtils
{$ENDIF}
  ,fcxSort, fcxCube, fcxStringUtils, fcxStreamUtils;
//FMX uses
{$ELSE FMX}
uses
  System.TypInfo, System.StrUtils
{$IFDEF SQL_TYPES_EXTRA1}
  ,Data.FMTBcd
{$ENDIF}
  ,System.Variants, System.RTLConsts ,System.WideStrUtils
  ,FMX.fcxSort, FMX.fcxCube, FMX.fcxStringUtils, FMX.fcxStreamUtils;
{$ENDIF FMX}

Type
  _fcxCommonDatePathFieldArray = array[odt_None..odt_None] of TfcxCommonDatePathField;
  PfcxCommonDatePathFieldArray = ^_fcxCommonDatePathFieldArray;
  _fcxCommonTimePathFieldArray = array[ott_None..ott_None] of TfcxCommonTimePathField;
  PfcxCommonTimePathFieldArray = ^_fcxCommonTimePathFieldArray;

{ TODO -cНеобходимо : Перенести в открытый модуль (например fcTypes).}
{$IFNDEF DELPHI_6UP}
procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;
{$ENDIF}


function fcDecodeDate(const ATSDateTime: integer; ACalcWeek: boolean; var ADatePaths: TfcxDatePaths; ADateTimeConsts: TfcxDateTimeConsts): boolean; overload;
const
  D1 = 365; // days in year
  D4 = D1 * 4 + 1; // days in 4 years
  D100 = D4 * 25 - 1; // days in 100 years
  D400 = D100 * 4 + 1; // days in 400 years
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;

  LStartDayOfWeek, LEndDayOfWeek: integer;
  LDayOfYear: Integer;
  AWeekOfYear: integer;

  AIsLeap2: boolean;
begin
  T := ATSDateTime;
  if T <= 0 then
  begin
    ADatePaths.Year := 0;
    ADatePaths.Month := 0;
    ADatePaths.Day := 0;
    ADatePaths.DayOfWeek := 0;
    ADatePaths.DayOfYear := 0;
    ADatePaths.WeekNumber := 0;
    ADatePaths.Quarter := 0;
    ADatePaths.YearOfWeek := 0;
    Result := False;
  end else
  begin
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    Result := IsLeapYear(Y);
    DayTable := @MonthDays[Result];
    M := 1;
    ADatePaths.DayOfYear := D + 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    ADatePaths.Year := Y;
    ADatePaths.Month := M;
    ADatePaths.Day := D + 1;
    ADatePaths.Quarter := (ADatePaths.Month - 1) div 3 + 1;
    if ADateTimeConsts.DayOfWeekISO8601 then
      ADatePaths.DayOfWeek := (ATSDateTime - 1) mod 7 + 1
    else
      ADatePaths.DayOfWeek := ATSDateTime mod 7 + 1;
    if not ACalcWeek then
    begin
      ADatePaths.WeekNumber := 0;
      ADatePaths.YearOfWeek := 0;
    end
    else
    begin
      LDayOfYear := ADatePaths.DayOfYear;
      LStartDayOfWeek := (ATSDateTime - ADatePaths.DayOfYear) mod 7 + 1;
      if LStartDayOfWeek > 4 then
        Dec(LDayOfYear, 8 - LStartDayOfWeek)
      else
        Inc(LDayOfYear, LStartDayOfWeek - 1);
      ADatePaths.YearOfWeek := ADatePaths.Year;
      if LDayOfYear <= 0 then
      begin
        if ADateTimeConsts.WeekNumberISO8601 then
        begin
          AIsLeap2 := IsLeapYear(ADatePaths.Year - 1);
          if AIsLeap2 then
            LDayOfYear := 366
          else
            LDayOfYear := 365;
          LStartDayOfWeek := (ATSDateTime - ADatePaths.DayOfYear - LDayOfYear) mod 7 + 1;
          if LStartDayOfWeek > 4 then
            Dec(LDayOfYear, 8 - LStartDayOfWeek)
          else
            Inc(LDayOfYear, LStartDayOfWeek - 1);
          AWeekOfYear := LDayOfYear div 7;
          if LDayOfYear mod 7 <> 0 then
            Inc(AWeekOfYear);
          if AWeekOfYear >= 52 then
          begin
            LEndDayOfWeek := LStartDayOfWeek;
            if AIsLeap2 then
            begin
              if LEndDayOfWeek = 7 then
                LEndDayOfWeek := 1
              else
                Inc(LEndDayOfWeek);
            end;
            if LEndDayOfWeek < 4 then
            begin
              AWeekOfYear := 1;
            end
            else
              ADatePaths.YearOfWeek := ADatePaths.Year - 1;
          end
        end
        else
        begin
          AWeekOfYear := 1;
        end;
      end
      else
      begin
        AWeekOfYear := LDayOfYear div 7;
        if LDayOfYear mod 7 <> 0 then
          Inc(AWeekOfYear);
        if AWeekOfYear > 52 then
        begin
          if ADateTimeConsts.WeekNumberISO8601 then
          begin
            LEndDayOfWeek := LStartDayOfWeek;
            if Result then
            begin
              if LEndDayOfWeek = 7 then
                LEndDayOfWeek := 1
              else
                Inc(LEndDayOfWeek);
            end;
            if LEndDayOfWeek < 4 then
            begin
              ADatePaths.YearOfWeek := ADatePaths.Year + 1;
              AWeekOfYear := 1;
            end;
          end
        end;
      end;
      ADatePaths.WeekNumber := AWeekOfYear;
    end;
  end;
end;

function fcDecodeDate(const ADateTime: TDateTime; ACalcWeek: boolean; var ADatePaths: TfcxDatePaths; ADateTimeConsts: TfcxDateTimeConsts): boolean; overload;
var
  ATSDateTime: integer;
begin
  ATSDateTime := DateTimeToTimeStamp(ADateTime).Date;
  Result := fcDecodeDate(ATSDateTime, ACalcWeek, ADatePaths, ADateTimeConsts);
end;

procedure fcDecodeTime(const ATime: Integer; var ATimePaths: TfcxTimePaths); overload;
var
  AMinCount, AMSecCount, AHour, AMin, ASec, AMSec: Word;
begin
  DivMod(ATime, 60 * 1000, AMinCount, AMSecCount);
  DivMod(AMinCount, 60, AHour, AMin);
  DivMod(AMSecCount, 1000, ASec, AMSec);
  ATimePaths.Hour := AHour;
  ATimePaths.Minute := AMin;
  ATimePaths.Second := ASec;
  ATimePaths.Millisecond := AMSec;
end;

procedure fcDecodeTime(const ADateTime: TDateTime; var ATimePaths: TfcxTimePaths); overload;
var
  ATSDateTime: integer;
begin
  ATSDateTime := DateTimeToTimeStamp(ADateTime).Time;
  fcDecodeTime(ATSDateTime, ATimePaths);
end;

{$IFDEF SQL_TYPES_EXTRA1}
function fcDecodeDate(const ADateTime: TSQLTimeStamp; ACalcWeek: boolean; var ADatePaths: TfcxDatePaths; ADateTimeConsts: TfcxDateTimeConsts): boolean; overload;
var
  ATSDateTime: integer;
begin
  ATSDateTime := DateTimeToTimeStamp(SQLTimeStampToDateTime(ADateTime)).Date;
  Result := fcDecodeDate(ATSDateTime, ACalcWeek, ADatePaths, ADateTimeConsts);
end;

procedure fcDecodeTime(const ADateTime: TSQLTimeStamp; var ATimePaths: TfcxTimePaths); overload;
begin
  ATimePaths.Hour := ADateTime.Hour;
  ATimePaths.Minute := ADateTime.Minute;
  ATimePaths.Second := ADateTime.Second;
  ATimePaths.Millisecond := ADateTime.Fractions;
end;
{$ENDIF}

type TfcxHackCube = class(TfcxCube);

{ TfcxCommonGroupManager }

procedure TfcxCommonGroupManager.AddInGroup(AUValue: PfcxCommonUV;
  AGValue: PfcxGroupValue);
begin
  if FGroupParentList[GetIndexFromUV(AUValue)] = AGValue then
    exit;
  StartChange;
  if AGValue.IsOther then
    RaisefcError(exfcAddInOtherGroup, []);
  RemoveFromGroupInternal(AUValue, True);
  AddInGroupInternal(AUValue, AGValue);
  StopChange([chc_GroupsChanged], -2);
end;

procedure TfcxCommonGroupManager.AddInGroupInternal(AUValue: PfcxCommonUV;
  AGValue: PfcxGroupValue);
var
  AIndexUVInGroup: integer;
begin
  IndexUVInGroup(GetIndexFromUV(AUValue), AIndexUVInGroup, AGValue);
  inc(AGValue.CountUV);
  if not AGValue.IsOther then
    inc(FCountInAllGroups);
  ReallocMem(AGValue.ListUV, AGValue.CountUV * fcPointerSize);
  if AIndexUVInGroup < (AGValue.CountUV - 1) then
    System.Move(AGValue.ListUV^[AIndexUVInGroup], AGValue.ListUV^[AIndexUVInGroup + 1],
      (AGValue.CountUV - AIndexUVInGroup - 1) * fcPointerSize);
  AGValue.ListUV[AIndexUVInGroup] := AUValue;
  FGroupParentList[GetIndexFromUV(AUValue)] := AGValue;
//  AUValue.Properties := (AUValue.Properties or puvValueInGroup);
end;

procedure TfcxCommonGroupManager.Clear;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    if not FListGV[i].IsOther then
    begin
      FreeMem(FListGV[i].ListUV);
      FreeMem(FListGV[i].Caption);
      FreeMem(FListGV[i])
    end;
  FCount := 0;
  FCountInAllGroups := 0;
  FExistsOther := False;
  if FOtherGV <> nil then
  begin
    FreeMem(FOtherGV.ListUV);
    FreeMem(FOtherGV.Caption);
    FreeMem(FOtherGV);
    FOtherGV := nil;
  end;
  FreeMem(FListGV);
  FListGV := nil;
  FreeMem(FGroupParentList);
  FGroupParentList := nil;
end;

function TfcxCommonGroupManager.CreateGroup(const AGroupCaption: TfcxString): PfcxGroupValue;
begin
  Result := CreateGV(AGroupCaption);
end;

function TfcxCommonGroupManager.CreateGV(const ACaption: TfcxString; AIsOther: Boolean = False): PfcxGroupValue;
var
  AGroupIndex: integer;
begin
  if AIsOther and ExistsOther then
  begin
    Result := FOtherGV;
    exit;
  end;
  AGroupIndex := GroupIndex[ACaption];
  if AGroupIndex <> -1 then
  begin
    Result := FListGV[AGroupIndex];
    exit;
  end;
  StartChange;
  inc(FCount);
  ReallocMem(FListGV, FCount * fcPointerSize);
  if AIsOther then
  begin
    Result := FOtherGV;
    FListGV[FCount - 1] := Result;
    Result.Index := FCount - 1;
    FExistsOther := True;
  end
  else
  begin
    Result := NewGV(ACaption, AIsOther);
    if ExistsOther then
    begin
      FListGV[FCount - 1] := FOtherGV;
      FOtherGV.Index := FCount - 1;
      FListGV[FCount - 2] := Result;
      Result.Index := FCount - 2;
    end
    else
    begin
      FListGV[FCount - 1] := Result;
      Result.Index := FCount - 1;
    end;
  end;
  StopChange([chc_GroupsChanged], -2);
end;

function TfcxCommonGroupManager.CreateOtherGroup: PfcxGroupValue;
begin
  Result := CreateGV(FOtherCaption, True);
end;

procedure TfcxCommonGroupManager.DeleteByIndex(AIndex: integer);
var
  i: integer;
begin
  StartChange;
  if FListGV[AIndex].IsOther then
  begin
//    FOtherGV := nil;
    FExistsOther := False;
  end
  else
    FreeGV(FListGV[AIndex]);
  Dec(FCount);
  if AIndex < FCount then
    System.Move(FListGV^[AIndex + 1], FListGV^[AIndex],
      (FCount - AIndex) * fcPointerSize);
  ReallocMem(FListGV, FCount * fcPointerSize);
  for i := AIndex to FCount - 1 do
    FListGV[i].Index := i;
  StopChange([chc_GroupsChanged], -2);
end;

destructor TfcxCommonGroupManager.Destroy;
begin
  Clear;
  inherited;
end;

procedure TfcxCommonGroupManager.FreeGV(AGValue: PfcxGroupValue);
var
  i, AIndexUVInGroup: integer;
begin
  if not AGValue.IsOther then
  begin
    ReallocMem(FOtherGV.ListUV, (FOtherGV.CountUV + AGValue.CountUV) * fcPointerSize);
    for i := 0 to AGValue.CountUV - 1 do
    begin
      IndexUVInGroup(GetIndexFromUV(AGValue.ListUV[i]), AIndexUVInGroup, FOtherGV);
      inc(FOtherGV.CountUV);
//      ReallocMem(FOtherGV.ListUV, FOtherGV.CountUV * fcPointerSize);
      if AIndexUVInGroup < (FOtherGV.CountUV - 1) then
        System.Move(FOtherGV.ListUV^[AIndexUVInGroup], FOtherGV.ListUV^[AIndexUVInGroup + 1],
          (FOtherGV.CountUV - AIndexUVInGroup - 1) * fcPointerSize);
      FOtherGV.ListUV[AIndexUVInGroup] := AGValue.ListUV[i];
      FGroupParentList[GetIndexFromUV(AGValue.ListUV[i])] := FOtherGV;
    end;
    dec(FCountInAllGroups, AGValue.CountUV);
    AGValue.CountUV := 0;
    FreeMem(AGValue.ListUV);
    FreeMem(AGValue.Caption);
    FreeMem(AGValue)
  end;
end;

function TfcxCommonGroupManager.GetCaption(AGroupIndex: integer): TfcxString;
begin
  Result := CaptionByGV[FListGV[AGroupIndex]]
end;

function TfcxCommonGroupManager.GetCaptionByGV(AGValue: PfcxGroupValue): TfcxString;
begin
  if AGValue.IsOther then
    result := OtherCaption
  else
  if AGValue.Caption = nil then
    result := 'No Caption'
  else
    result := AGValue.Caption;
end;

function TfcxCommonGroupManager.GetCountInGroup(AGroupIndex: integer): Integer;
begin
{
  if FListGV[AGroupIndex].IsOther then
    Result := UVSCount - FCountInAllGroups
  else
}
    Result := FListGV[AGroupIndex].CountUV;
end;

function TfcxCommonGroupManager.IndexUVInGroup(AUVIndex: integer; var AIndexOfUV: integer; AGValue: PfcxGroupValue = nil): boolean;
var
  L, H, I, C: Integer;
begin
  if AGValue = nil then
    AGValue := FGroupParentList[AUVIndex];
  Result := False;
  if AGValue = nil then
  begin
    AIndexOfUV := -1;
    exit;
  end;
  L := 0;
  H := AGValue.CountUV - 1;
  while L <= H do
  begin
    I := (L + H) shr 1; // середина
    if GetIndexFromUV(AGValue.ListUV[I]) > AUVIndex then
      C := 1
    else if GetIndexFromUV(AGValue.ListUV[I]) < AUVIndex then
      C := -1
    else
      C := 0;
    if C < 0 then
      L := I + 1
    else if C > 0 then
      H := I - 1
    else
    begin // Ну вот и нашли
      Result := True;
      AIndexOfUV := I;
      Exit;
    end;
  end;
  AIndexOfUV := L;
end;

procedure TfcxCommonGroupManager.Insert(AUVIndex: integer);
begin
  if AUVIndex < (UVSCount - 1) then
    System.Move(FGroupParentList^[AUVIndex], FGroupParentList^[succ(AUVIndex)],
      (UVSCount - 1 - AUVIndex) * fcPointerSize);
  FGroupParentList[AUVIndex] := FOtherGV {nil};
end;

function TfcxCommonGroupManager.NewGV(const ACaption: TfcxString; AIsOther: boolean): PfcxGroupValue;
begin
  GetMem(Result, SizeOf(TfcxGroupValue));
  Result.CountUV := 0;
  Result.IsOther := AIsOther;
  Result.ListUV := nil;
  GetMem(Result.Caption, (Length(ACaption) + 1) *  SizeOf(TfcxChar));
  fcStrPCopy(Result.Caption, ACaption);
end;

procedure TfcxCommonGroupManager.RemoveFromGroup(AUValue: PfcxCommonUV);
begin
  if not FGroupParentList[GetIndexFromUV(AUValue)].IsOther then
    RemoveFromGroupInternal(AUValue);
end;

procedure TfcxCommonGroupManager.RemoveFromGroupInternal(
  AUValue: PfcxCommonUV; ATransfer: Boolean = False);
var
  AIndexUVInGroup: integer;
  AGValue: PfcxGroupValue;
begin
  if not IndexUVInGroup(GetIndexFromUV(AUValue), AIndexUVInGroup) then
    exit;
  if not ATransfer then
    StartChange;
  AGValue := FGroupParentList[GetIndexFromUV(AUValue)];
  dec(AGValue.CountUV);
  if not AGValue.IsOther then
    dec(FCountInAllGroups);
  if AIndexUVInGroup < AGValue.CountUV then
    System.Move(AGValue.ListUV^[AIndexUVInGroup + 1], AGValue.ListUV^[AIndexUVInGroup],
      (AGValue.CountUV - AIndexUVInGroup) * fcPointerSize);
  ReallocMem(AGValue.ListUV, AGValue.CountUV * fcPointerSize);
  FGroupParentList[GetIndexFromUV(AUValue)] := nil;
  if not AGValue.IsOther and not ATransfer then
    AddInGroupInternal(AUValue, FOtherGV);
  if not ATransfer then
    StopChange([chc_GroupsChanged], -2);
end;

procedure TfcxCommonGroupManager.SetCaption(AGroupIndex: integer;
  const Value: TfcxString);
begin
  StartChange;
  CaptionByGV[FListGV[AGroupIndex]] := Value;
  StopChange([chc_Caption]);
end;

procedure TfcxCommonGroupManager.SetCaptionByGV(AGValue: PfcxGroupValue;
  const Value: TfcxString);
begin
  if AGValue.IsOther then
    OtherCaption := Value
  else
  begin
    if CaptionByGV[AGValue] = Value then
      exit;
    if AGValue.Caption <> nil then
      FreeMem(AGValue.Caption);
    GetMem(AGValue.Caption, (Length(Value) + 1) *  SizeOf(TfcxChar));
    fcStrPCopy(AGValue.Caption, Value);
  end;
end;

procedure TfcxCommonGroupManager.SetOtherCaption(const Value: TfcxString);
begin
  if FOtherCaption = Value then
    exit;
  FOtherCaption := Value;
  if FExistsOther then
    CaptionByGV[FOtherGV] := Value;
end;

function TfcxCommonGroupManager.GetUVIndexOfOther(
  AIndexInGroup: integer): integer;
begin
// неоптимально !!! но создавать список для OTHER тоже не хочется
  for Result := 0 to UVSCount - 1 do
  begin
    if FGroupParentList[Result] = nil then
      dec(AIndexInGroup);
    if AIndexInGroup < 0 then
      exit;
  end;
  Result := -1;
end;

procedure TfcxCommonGroupManager.DeleteGroup(AGroupIndex: integer);
begin
  DeleteByIndex(AGroupIndex);
end;

function TfcxCommonGroupManager.GetOtherGroupIndex: Integer;
begin
  if ExistsOther then
    Result := FOtherGV.Index
  else
    Result := -1;
end;

procedure TfcxCommonGroupManager.SetCapacity(ASetAllToNil: boolean = False);
begin
  ReallocMem(FGroupParentList, UVSCapacity * fcPointerSize);
  if ASetAllToNil and (UVSCount > 0) then
    FillChar(FGroupParentList^, UVSCount *  fcPointerSize, 0);
end;

procedure TfcxCommonGroupManager.Delete(AUVIndex: integer);
begin
//  if ((FUniqueValues.FList[AUVIndex].Properties and puvValueInGroup) = puvValueInGroup) then
  if FGroupParentList[AUVIndex] <> nil then
    RemoveUVFromGroup(AUVIndex);
  if AUVIndex < (UVSCount - 1) then
    System.Move(FGroupParentList^[AUVIndex + 1], FGroupParentList^[AUVIndex],
      (UVSCount - 1 - AUVIndex) * fcPointerSize);
end;

{
function TfcxCommonGroupManager.AddUValue(AUVIndex: integer): Integer;
begin
  FGroupParentList[AUVIndex] := nil;
end;
}
function TfcxCommonGroupManager.GetCountInNonGroups: Integer;
begin
  if ExistsOther then
    Result := 0
  else
    Result := UVSCount - FCountInAllGroups
end;

constructor TfcxCommonGroupManager.Create(
  AUniqueValues: TfcxBaseUniqueValues);
begin
  FUniqueValues := AUniqueValues;
  FOtherCaption := 'Other';
  FCount := 0;
  FCountInAllGroups := 0;
  FExistsOther := False;
  FOtherGV := nil;
  FListGV := nil;
  FGroupParentList := nil;
  SetCapacity;
  FillChar(FGroupParentList^, UVSCount * fcPointerSize, 0);
  InitOtherGroup;
end;

function TfcxCommonGroupManager.GetIndexFromUV(
  AUValue: PfcxCommonUV): integer;
begin
  Result := AUValue.Index;
end;

procedure TfcxCommonGroupManager.LoadFromStream(ACubeStream: TStream);
var
  ACount, i, AOtherGroupIndex, AUVIndex, AGroupIndex: integer;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  ACubeStream.Read(AOtherGroupIndex, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    if i = AOtherGroupIndex then
    begin
      OtherCaption := ReadfcString(ACubeStream);

    end
    else
      CreateGroup(ReadfcString(ACubeStream));
  end;
  ACubeStream.Read(ACount, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(AUVIndex, SizeOf(integer));
    ACubeStream.Read(AGroupIndex, SizeOf(integer));
    AddUVInGroup(AUVIndex, AGroupIndex);
  end;
end;

procedure TfcxCommonGroupManager.SaveToStream(ACubeStream: TStream);
var
  i, AOtherGroupIndex: integer;
begin
  ACubeStream.Write(FCount, SizeOf(integer));
  AOtherGroupIndex := GetOtherGroupIndex;
  ACubeStream.Write(AOtherGroupIndex, SizeOf(integer));
  for i := 0 to FCount - 1 do
    WritefcString(ACubeStream, Caption[i]);
  ACubeStream.Write(FCountInAllGroups, SizeOf(integer));
  for i := 0 to UVSCount - 1 do
    if FGroupParentList[i] <> FOtherGV then
    begin
      ACubeStream.Write(i, SizeOf(integer));
      ACubeStream.Write(FGroupParentList[i].Index, SizeOf(integer));
    end;
end;

procedure TfcxCommonGroupManager.SaveGroupsToXML(AItem: TfcxXMLItem);
var
  AItem1, AItem2, AItem3: TfcxXMLItem;
  i, j: integer;
  AfcxVarType: TfcxVarType;
  AVariant: Variant;
begin
  AItem.BoolProp['ExistsOther'] := FExistsOther;
  AItem.Prop['OtherCaption'] := FOtherCaption;
  if Self is TfcxUVSGroupManager then
    AfcxVarType := TfcxCommonField(FUniqueValues.FfcField).fcxVarType
  else
    AfcxVarType := fcvtOrdinal;
  AItem.Prop['fcxVarType'] := GetEnumName(TypeInfo(TfcxVarType), Ord(AfcxVarType));
  AItem1 := AItem.Add;
  AItem1.Name := 'groups';
  for i := 0 to FCount - 1 do
  begin
    AItem2 := AItem1.Add;
    AItem2.Name := 'group';
    AItem2.Prop['name'] := Caption[i];
    if not FListGV[i].IsOther then
    begin
      for j := 0 to FListGV[i].CountUV - 1 do
      begin
        AItem3 := AItem2.Add;
        AItem3.Name := 'member';
        if Self is TfcxUVSGroupManager then
          AVariant := FUniqueValues.GetValueAsVariant(FListGV[i].ListUV[j])
        else
          AVariant := Integer(FListGV[i].ListUV[j]);
        if AVariant = null then
          AItem3.Prop['nullvalue'] := ''
        else
          case AfcxVarType of
            fcvtDate:
              AItem3.DateProp['value'] := AVariant;
            fcvtOrdinal:
              AItem3.IntProp['value'] := AVariant;
            fcvtFloat:
              AItem3.FloatProp['value'] := AVariant;
          else
            AItem3.Prop['value'] := AVariant;
          end;
      end;
    end
  end;
end;

procedure TfcxCommonGroupManager.LoadGroupsFromXML(AItem: TfcxXMLItem);
var
  i, j, AGroupIndex, AUVIndex: integer;
  AfcxVarType: TfcxVarType;
  AVarValue: Variant;
  AItem1: TfcxXMLItem;
  APointer: pointer;
  AUValue: PfcxCommonUV;
begin
  if AItem.BoolProp['ExistsOther'] then
    CreateOtherGroup;
  OtherCaption := AItem.Prop['OtherCaption'];
  AfcxVarType := TfcxVarType(GetEnumValue(TypeInfo(TfcxVarType), AItem.Prop['fcxVarType']));
  for i := 0 to AItem[AItem.Count - 1].Count - 1 do
  begin
    AItem1 := AItem[AItem.Count - 1].Items[i];
    AGroupIndex := CreateGroup(AItem1.Prop['name']).Index;
    for j := 0 to AItem1.Count - 1 do
    begin
      if AItem1[j].PropExists('nullvalue') then
        AVarValue := Null
      else
        case AfcxVarType of
          fcvtDate:
            AVarValue := AItem1[j].DateProp['value'];
          fcvtOrdinal:
            AVarValue := AItem1[j].IntProp['value'];
          fcvtFloat:
            AVarValue := AItem1[j].FloatProp['value'];
        else
          AVarValue := AItem1[j].Prop['value'];
        end;
        if Self is TfcxUVSGroupManager then
        begin
          if AVarValue = Null then
            APointer := nil
          else
            APointer := FUniqueValues.VarValueToPointer(AVarValue);
          if FUniqueValues.FindUValue(APointer, AUValue, AUVIndex) then
            AddInGroup(AUValue, FListGV[AGroupIndex]);
          FreeMem(APointer);
        end
        else
        if Self is TfcxDatePathGroupManager then
        begin
          if AVarValue = Null then
          begin
            if TfcxDatePathGroupManager(Self).FDatePathProcessor.FindIndexAtValue(0, AUVIndex) then
              AddUVInGroup(AUVIndex, AGroupIndex);
          end
          else
            if TfcxDatePathGroupManager(Self).FDatePathProcessor.FindIndexAtValue(AVarValue, AUVIndex) then
              AddUVInGroup(AUVIndex, AGroupIndex);
        end
        else
        if Self is TfcxTimePathGroupManager then
        begin
          if AVarValue = Null then
          begin
            if TfcxTimePathGroupManager(Self).FTimePathProcessor.FindIndexAtValue(-1, AUVIndex) then
              AddUVInGroup(AUVIndex, AGroupIndex);
          end
          else
            if TfcxTimePathGroupManager(Self).FTimePathProcessor.FindIndexAtValue(AVarValue, AUVIndex) then
              AddUVInGroup(AUVIndex, AGroupIndex);
        end;
    end;

  end;
end;

procedure TfcxCommonGroupManager.AppendFromStream(ACubeStream: TStream);
var
  ACount, i, AOtherGroupIndex, AUVIndex, AGroupIndex: integer;
  ATempIntArray: Array of integer;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  ACubeStream.Read(AOtherGroupIndex, SizeOf(integer));
  SetLength(ATempIntArray, ACount);
  for i := 0 to ACount - 1 do
  begin
    if i = AOtherGroupIndex then
    begin
      OtherCaption := ReadfcString(ACubeStream);
    end
    else
      ATempIntArray[i] := CreateGroup(ReadfcString(ACubeStream)).Index;
  end;
  ACubeStream.Read(ACount, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(AUVIndex, SizeOf(integer));
    ACubeStream.Read(AGroupIndex, SizeOf(integer));
    AddUVInGroup(FUniqueValues.FTempList[AUVIndex].Index, ATempIntArray[AGroupIndex]);
  end;
  SetLength(ATempIntArray, 0);
end;

function TfcxCommonGroupManager.GetGroupIndex(
  AGroupCaption: TfcxString): integer;
var
 i: integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if CaptionByGV[FListGV[i]] = AGroupCaption then
    begin
      Result := i;
      Exit;
    end
end;

{ TfcxSplitManager }

procedure TfcxSplitManager.AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
  ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
begin
  if not ACalculateAfterAll then
  begin
    if FUseDateSplit then
      FDatePathsManager.AddUValue(AUValue, AUVIndex, [], AOnlyForNotSavedFields);
    if FUseTimeSplit then
      FTimePathsManager.AddUValue(AUValue, AUVIndex, [], AOnlyForNotSavedFields);
  end;
  if (AttributesManager.AttributesCount > 0) then
    FAttributesManager.AddUValue(AUValue, AUVIndex, ACalculateAfterAll, AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.Clear;
begin
  FUseDateSplit := False;
  FreeAndNil(FDatePathsManager);

  FUseTimeSplit := False;
  FreeAndNil(FTimePathsManager);

  FreeAndNil(FAttributesManager);
end;

constructor TfcxSplitManager.Create(AUniqueValues: TfcxBaseUniqueValues);
begin
  FUniqueValues := AUniqueValues;

  FCanUseDateSplit := FUniqueValues.FDataType in fcxDateTypes;
  FUseDateSplit := False;
  FDatePathsManager := nil;

  FCanUseTimeSplit := FUniqueValues.FDataType in fcxTimeTypes;
  FUseTimeSplit := False;
  FTimePathsManager := nil;

  FAttributesManager := TfcxAttributesManager.Create(FUniqueValues);

(*
  SetCapacity;
*)
end;

procedure TfcxSplitManager.Delete(AUVIndex: integer);
begin
  if FUseDateSplit then
    FDatePathsManager.Delete(AUVIndex);
  if FUseTimeSplit then
    FTimePathsManager.Delete(AUVIndex);
  FAttributesManager.Delete(AUVIndex);
end;

destructor TfcxSplitManager.Destroy;
begin
  Clear;
  FreeAndNil(FDatePathsManager);
  FreeAndNil(FTimePathsManager);
  FreeAndNil(FAttributesManager);
  inherited;
end;

procedure TfcxSplitManager.Insert(AUVIndex: integer);
begin
  if FUseDateSplit then
  begin
    FDatePathsManager.Insert(AUVIndex);
    FDatePathsManager.InsertUValue(FUniqueValues.FList[AUVIndex], AUVIndex);
  end;
  if FUseTimeSplit then
  begin
    FTimePathsManager.Insert(AUVIndex);
{ TODO -cНеобходимо : Исправить, так как битовый массив уже убит !!!.}
    FTimePathsManager.InsertUValue(FUniqueValues.FList[AUVIndex], AUVIndex);
  end;
  if AttributesManager.AttributesCount > 0 then
  begin
    FAttributesManager.Insert(AUVIndex);
    FAttributesManager.AddUValueNonReference(FUniqueValues.FList[AUVIndex], AUVIndex);
// because DataSet closed !!!
//??    FAttributesManager.AddUValueNullReference(AUVIndex);
  end;
end;

procedure TfcxSplitManager.SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
begin
  If FUseDateSplit then
    FDatePathsManager.SetCapacity(ASetAllToNil, AOnlyForNotSavedFields);
  If FUseTimeSplit then
    FTimePathsManager.SetCapacity(ASetAllToNil, AOnlyForNotSavedFields);
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.SetCapacity(ASetAllToNil, AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.SetIndex(ACalculateAfterAll: Boolean;
  AOnlyForNotSavedFields: boolean = False);
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.SetIndex(ACalculateAfterAll, AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.SetUseDateSplit(const Value: boolean);
begin
  if FUseDateSplit <> Value then
  begin
    if Value then
    begin
      if not FCanUseDateSplit then
        RaisefcError(exfcTypeCanNotSplit, [cfcDataTypeNames[FUniqueValues.FDataType], 'Date']);
      FDatePathsManager := TfcxDatePathsManager.Create(FUniqueValues);
    end
    else
    begin
      FreeAndNil(FDatePathsManager);
    end;
    FUseDateSplit := Value;
  end;
end;

procedure TfcxSplitManager.SetUseTimeSplit(const Value: boolean);
begin
  if FUseTimeSplit <> Value then
  begin
    if Value then
    begin
      if not FCanUseTimeSplit then
        RaisefcError(exfcTypeCanNotSplit, [cfcDataTypeNames[FUniqueValues.FDataType], 'Time']);
      FTimePathsManager := TfcxTimePathsManager.Create(FUniqueValues);
    end
    else
    begin
      FreeAndNil(FTimePathsManager);
    end;
    FUseTimeSplit := Value;
  end;
end;

procedure TfcxSplitManager.Sort(AFirstStep: boolean; ACalculateAfterAll: Boolean;
  AOnlyForNotSavedFields: boolean = False);
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.Sort(AFirstStep, ACalculateAfterAll, AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.StartLoad(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.StartLoad(ACalculateAfterAll, AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.StopLoad(ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.StopLoad(ACalculateAfterAll, AOnlyForNotSavedFields);
end;

function TfcxSplitManager.GetDatePathsManager: TfcxDatePathsManager;
begin
  if not FUseDateSplit then
    RaisefcError(exfcDateSplitNotSupported, []);
  Result := FDatePathsManager;
end;

function TfcxSplitManager.GetTimePathsManager: TfcxTimePathsManager;
begin
  if not FUseTimeSplit then
    RaisefcError(exfcTimeSplitNotSupported, []);
  Result := FTimePathsManager;
end;

procedure TfcxSplitManager.Recapacity(AOnlyForNotSavedFields: boolean = False);
begin
  If FUseDateSplit then
    FDatePathsManager.Recapacity(AOnlyForNotSavedFields);
  If FUseTimeSplit then
    FTimePathsManager.Recapacity(AOnlyForNotSavedFields);
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.Recapacity(AOnlyForNotSavedFields);
end;

function TfcxSplitManager.GetDatePathsCount: integer;
begin
  if UseDateSplit then
    result := DatePathsManager.Count
  else
    result := 0;
end;

function TfcxSplitManager.GetTimePathsCount: integer;
begin
  if UseTimeSplit then
    result := TimePathsManager.Count
  else
    result := 0;
end;

function TfcxSplitManager.GetAttributesCount: integer;
begin
  Result := FAttributesManager.AttributesCount;
end;

function TfcxSplitManager.GetAttributeDisplayValue(AUVIndex,
  AAttributeIndex: integer): TfcxString;
begin
  Result := FAttributesManager.Attribute[AAttributeIndex].FTypeProcessor.Caption[FAttributesManager.FListUVofPaths[AAttributeIndex, AUVIndex]];
end;

procedure TfcxSplitManager.AddUValueMainReference(AUValue: PfcxCommonUV;
  AUVIndex: integer);
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.AddUValueMainReference(AUValue, AUVIndex);
end;

procedure TfcxSplitManager.SortMainReference;
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.SortMainReference;
end;

procedure TfcxSplitManager.SetIndexMainReference;
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.SetIndexMainReference;
end;

procedure TfcxSplitManager.SaveGroupsToXML(AItem: TfcxXMLItem;
  AOnlyForNotSavedFields: boolean);
begin
// Date Split
  if UseDateSplit then
    DatePathsManager.SaveGroupsToXML(AItem, AOnlyForNotSavedFields);
// Time Split
  if UseTimeSplit then
    TimePathsManager.SaveGroupsToXML(AItem, AOnlyForNotSavedFields);
  FAttributesManager.SaveGroupsToXML(AItem, AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.LoadGroupsFromXML(AItem: TfcxXMLItem);
var
  i: integer;
begin
  for i := 0 to AItem.Count - 1 do
  begin
    if AItem[i].Name = 'DatePaths' then
    begin
      if UseDateSplit then
        DatePathsManager.LoadGroupsFromXML(AItem[i]);
    end
    else
    if AItem[i].Name = 'TimePaths' then
    begin
      if UseTimeSplit then
        TimePathsManager.LoadGroupsFromXML(AItem[i]);
    end
    else
    if AItem[i].Name = 'Attributes' then
    begin
      FAttributesManager.LoadGroupsFromXML(AItem[i]);
    end
  end;
end;

procedure TfcxSplitManager.ClearGroups;
begin
  if UseDateSplit then
    FDatePathsManager.ClearGroups;
  if UseTimeSplit then
    FTimePathsManager.ClearGroups;
  FAttributesManager.ClearGroups;
end;

procedure TfcxSplitManager.LoadAttributes(AOnlyForNotSavedFields: boolean);
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.LoadAttributes(AOnlyForNotSavedFields);
end;

procedure TfcxSplitManager.SetCaptionAndGroupCapacity(ACalculateAfterAll: boolean);
var
  i: integer;
begin
(* ???
  if UseDateSplit then
    FDatePathsManager.;
  if UseTimeSplit then
    FTimePathsManager.;
*)
  for i := 0 to AttributesManager.AttributesCount - 1 do
    if not (ACalculateAfterAll xor FAttributesManager.Attribute[i].CalculateAfterAll) then
      FAttributesManager.Attribute[i].SetCaptionAndGroupCapacity;
end;

procedure TfcxSplitManager.FillNonSavedUVs;
begin
  if AttributesManager.AttributesCount > 0 then
    FAttributesManager.FillNonSavedUVs;
end;

{ TfcxCustomCaptionManager }

procedure TfcxCustomCaptionManager.AddUValue(AUValue: PfcxCommonUV;
  AUVIndex: integer);
begin
  FCaptionList[FUniqueValues.FCount] := nil;
end;

procedure TfcxCustomCaptionManager.AppendFromStream(ACubeStream: TStream);
var
  i, AUVIndex, ACount: integer;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(AUVIndex, SizeOf(integer));
    SetCaption(FUniqueValues.FTempList[AUVIndex].Index, ReadfcString(ACubeStream));
  end;
end;

procedure TfcxCustomCaptionManager.Clear;
var
  i: Integer;
begin
  for i := 0 to FUniqueValues.FCount - 1 do
    FreeMem(FCaptionList[i]);
  FreeMem(FCaptionList);
  FCaptionList := nil;
end;

constructor TfcxCustomCaptionManager.Create(AUniqueValues: TfcxBaseUniqueValues);
begin
  FUniqueValues := AUniqueValues;
  FCaptionList := nil;
  SetCapacity;
  FillChar(FCaptionList^, FUniqueValues.Count *  fcPointerSize, 0);
end;

procedure TfcxCustomCaptionManager.Delete(AUVIndex: integer);
begin
  FreeMem(FCaptionList[AUVIndex]);
  if AUVIndex < (FUniqueValues.FCount - 1) then
    System.Move(FCaptionList^[AUVIndex + 1], FCaptionList^[AUVIndex],
      (FUniqueValues.FCount - 1 - AUVIndex) * fcPointerSize);
end;

destructor TfcxCustomCaptionManager.Destroy;
begin
  Clear;
  inherited;
end;

procedure TfcxCustomCaptionManager.Insert(AUVIndex: integer);
begin
  if AUVIndex < (FUniqueValues.FCount - 1) then
    System.Move(FCaptionList^[AUVIndex], FCaptionList^[succ(AUVIndex)],
      (FUniqueValues.FCount - 1 - AUVIndex) * fcPointerSize);
  AddUValue(FUniqueValues.FList[AUVIndex], AUVIndex);
end;

procedure TfcxCustomCaptionManager.LoadFromStream(ACubeStream: TStream);
var
  i, AUVIndex, ACount: integer;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(AUVIndex, SizeOf(integer));
    SetCaption(AUVIndex, ReadfcString(ACubeStream));
  end;
end;

procedure TfcxCustomCaptionManager.SaveToStream(ACubeStream: TStream);
var
  i, ACapCount, ACapCount2: integer;
begin
  ACapCount := 0;
  for i := 0 to FUniqueValues.Count - 1 do
    if FCaptionList[i] <> nil then
      inc(ACapCount);
  ACubeStream.Write(ACapCount, SizeOf(integer));
  ACapCount2 := 0;
  for i := 0 to FUniqueValues.Count - 1 do
  begin
    if FCaptionList[i] <> nil then
    begin
      ACubeStream.Write(i, SizeOf(integer));
      WritefcString(ACubeStream, FCaptionList[i]);
      inc(ACapCount2);
      if ACapCount2 > ACapCount then
        break;
    end;
  end;
end;

procedure TfcxCustomCaptionManager.SetCapacity(ASetAllToNil: boolean = False);
begin
  ReallocMem(FCaptionList, FUniqueValues.FCapacity * fcPointerSize);
  if ASetAllToNil and (FUniqueValues.Count > 0) then
    FillChar(FCaptionList^, FUniqueValues.Count *  fcPointerSize, 0);
end;

procedure TfcxCustomCaptionManager.SetCaption(AUVIndex: Integer;
  const Value: TfcxString);
begin
  if FCaptionList[AUVIndex] <> nil then
    FreeMem(FCaptionList[AUVIndex]);
  GetMem(FCaptionList[AUVIndex], (Length(Value) + 1) *  SizeOf(TfcxChar));
  fcStrPCopy(FCaptionList[AUVIndex], Value);
end;

{ TfcxUVHash }

procedure TfcxUVHash.ClearHash;
var
  i: integer;
begin
  if FHashTable <> nil then
  begin
    for i := 0 to FTypeProcessor.HashBucketSize - 1 do
      FreeMem(FHashTable[i]);
    FreeMem(FHashTable);
    FreeMem(FHashCount);
    FHashTable := nil;
    FHashCount := nil;
  end;
end;

constructor TfcxUVHash.Create(ATypeProcessor: TfcxDataTypeProcessor);
begin
  FTypeProcessor := ATypeProcessor;
  FHashTable := nil;
  FHashCount := nil;
end;

procedure TfcxUVHash.CreateHash;
begin
  GetMem(FHashTable, FTypeProcessor.HashBucketSize *  fcPointerSize);
  GetMem(FHashCount, FTypeProcessor.HashBucketSize *  SizeOf(Integer));
  FillChar(FHashTable^, FTypeProcessor.HashBucketSize *  fcPointerSize, 0);
  FillChar(FHashCount^, FTypeProcessor.HashBucketSize *  SizeOf(Integer), 0);
end;

destructor TfcxUVHash.Destroy;
begin
  ClearHash;
  inherited;
end;

{ TfcxStandardPathsManager }

constructor TfcxStandardPathsManager.Create(AUniqueValues: TfcxBaseUniqueValues);
begin
  FUniqueValues := AUniqueValues;
  GetMem(FPathFields, FPathsCount * SizeOf(Pointer));
  FillChar(FPathFields^, FPathsCount * SizeOf(Pointer), 0);
end;

destructor TfcxStandardPathsManager.Destroy;
begin
  FreeMem(FPathFields);
  inherited;
end;

{ TfcxDatePathsManager }

function TfcxDatePathsManager.AddDatePath(ADatePath: TfcxDateType): integer;
var
  AFieldProperties: TfcxFieldProperties;
begin
  Result := -1;
  if PfcxCommonDatePathFieldArray(FPathFields)[ADatePath] = nil then
  begin
{ TODO -cНеобходимо : Откуда брать эти значения, из базового поля или из значений по умолчании или добавить свойства а TfcxCustomSplitPath в или еще откуда-то?}
    AFieldProperties.CaseSensitive := not FUniqueValues.IgnoreCase;
    AFieldProperties.NullStr := FUniqueValues.NullCaption;
    AFieldProperties.CubeFieldName := DateNamePrefixTable[ADatePath] + TfcxCommonField(FUniqueValues.fcField).CubeFieldName;
    AFieldProperties.CubeFieldDisplayLabel := fcxResources.Get(DateLabelPrefixTable[ADatePath]) + TfcxCommonField(FUniqueValues.fcField).CubeFieldDisplayLabel;
    AFieldProperties.DataType := fcdt_Word;
    AFieldProperties.Saved := False;
    PfcxCommonDatePathFieldArray(FPathFields)[ADatePath] := TfcxCommonDatePathField(TfcxCommonUVField(FUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FUniqueValues.fcField).Fields.AddDatePathField(AFieldProperties, ADatePath)]);
    Result := PfcxCommonDatePathFieldArray(FPathFields)[ADatePath].Index;
    inc(FCount);
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end;

end;

procedure TfcxDatePathsManager.AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
  ADateTypeSet: TfcxDateTypes = []; AOnlyForNotSavedFields: boolean = False);
var
  ADatePaths: TfcxDatePaths;
  ADateType: TfcxDateType;
  ANeedExit: Boolean;
begin
  ANeedExit := True;
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
        if (ADateTypeSet = []) or (ADateType in ADateTypeSet) then
        begin
          ANeedExit := False;
          Break;
        end;
  if ANeedExit then
    Exit;
  if FUniqueValues.FHasNull and (AUValue = FUniqueValues.FNullValue) then
  begin
// по новому. пока так.
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
      if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
        if (ADateTypeSet = []) or (ADateType in ADateTypeSet) then
          if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
            PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(0, AUVIndex);
  end
  else
  begin
    ADateType := odt_WeekNumber;
    case FUniqueValues.FDataType of
      fcdt_DateTime:
        fcDecodeDate(PfcxDateTimeUV(AUValue).Value, PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil, ADatePaths, TfcxCommonField(FUniqueValues.fcField).Cube.DateTimeConsts);
      fcdt_Date:
        fcDecodeDate(PfcxIntegerUV(AUValue).Value, PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil, ADatePaths, TfcxCommonField(FUniqueValues.fcField).Cube.DateTimeConsts);
    {$IFDEF SQL_TYPES_EXTRA1}
      fcdt_TimeStamp:
        fcDecodeDate(PfcxTimeStampUV(AUValue).Value, PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil, ADatePaths, TfcxCommonField(FUniqueValues.fcField).Cube.DateTimeConsts);
    {$ENDIF}  
    end;
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    begin
      if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
        if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
          if (ADateTypeSet = []) or (ADateType in ADateTypeSet) then
          begin
            case ADateType of
              odt_Year:
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.Year, AUVIndex);
              odt_Month:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.Month, AUVIndex);
                end;
              odt_Day:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.Day, AUVIndex);
                end;
              odt_DayOfWeek:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.DayOfWeek, AUVIndex);
                end;
              odt_Quarter:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.Quarter, AUVIndex);
                end;
              odt_WeekNumber:
                begin
                  if ADatePaths.YearOfWeek = ADatePaths.Year then
                    PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.WeekNumber + 100, AUVIndex)
                  else
                  if ADatePaths.YearOfWeek < ADatePaths.Year then
                    PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.WeekNumber, AUVIndex)
                  else
                    PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(200, AUVIndex);
                end;
              odt_DayOfYear:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.AddPathValue(ADatePaths.DayOfYear, AUVIndex);
                end;
            end;
          end;
    end;
  end;
end;

procedure TfcxDatePathsManager.ClearGroups;
var
  ADateType: TfcxDateType;
begin
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.ClearGroups;
end;

constructor TfcxDatePathsManager.Create(AUniqueValues: TfcxBaseUniqueValues);
begin
//  FSSplitPathType := fcsspt_Date;
  FPathsCount := fcDateTypeLength + 1;
  inherited;
end;

procedure TfcxDatePathsManager.CreatePathUVsArray(AOnlyForNotSavedFields: boolean = False);
var
  ADateType: TfcxDateType;
begin
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
        PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.CreatePathUVsArray;
end;

procedure TfcxDatePathsManager.Delete(AUVIndex: integer);
var
  ADateType: TfcxDateType;
begin
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.DeleteValue(AUVIndex);
end;

function TfcxDatePathsManager.GetCaptionAtIndex(ADatePath: TfcxDateType;
  AIndex: integer): TfcxString;
begin
  Result := PfcxCommonDatePathFieldArray(FPathFields)[ADatePath].DatePathProcessor.Caption[PfcxCommonDatePathFieldArray(FPathFields)[ADatePath].DatePathProcessor.ValueAtIndex[AIndex]];
end;

function TfcxDatePathsManager.GetCaptionAtValue(ADatePath: TfcxDateType;
  AValue: Word): TfcxString;
begin
  Result := PfcxCommonDatePathFieldArray(FPathFields)[ADatePath].DatePathProcessor.Caption[AValue];
end;

function TfcxDatePathsManager.GetDatePathField(
  ADatePath: TfcxDateType): Pointer;
begin
  Result := PfcxCommonDatePathFieldArray(FPathFields)[ADatePath];
end;

function TfcxDatePathsManager.GetUseDatePath(
  ADatePath: TfcxDateType): Boolean;
begin
  Result := PfcxCommonDatePathFieldArray(FPathFields)[ADatePath] <> nil;
end;

procedure TfcxDatePathsManager.Insert(AUVIndex: integer);
var
  ADateType: TfcxDateType;
begin
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertValue(AUVIndex);
end;

procedure TfcxDatePathsManager.InsertUValue(AUValue: PfcxCommonUV;
  AUVIndex: integer; ADateTypeSet: TfcxDateTypes;
  AOnlyForNotSavedFields: boolean);
var
  ADatePaths: TfcxDatePaths;
  ADateType: TfcxDateType;
  ANeedExit: Boolean;
begin
  ANeedExit := True;
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
        if (ADateTypeSet = []) or (ADateType in ADateTypeSet) then
        begin
          ANeedExit := False;
          Break;
        end;
  if ANeedExit then
    Exit;
  if FUniqueValues.FHasNull and (AUValue = FUniqueValues.FNullValue) then
  begin
// по новому. пока так.
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
      if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
        if (ADateTypeSet = []) or (ADateType in ADateTypeSet) then
          if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
            PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(0, AUVIndex);
  end
  else
  begin
    ADateType := odt_WeekNumber;
    case FUniqueValues.FDataType of
      fcdt_DateTime:
        fcDecodeDate(PfcxDateTimeUV(AUValue).Value, PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil, ADatePaths, TfcxCommonField(FUniqueValues.fcField).Cube.DateTimeConsts);
      fcdt_Date:
        fcDecodeDate(PfcxIntegerUV(AUValue).Value, PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil, ADatePaths, TfcxCommonField(FUniqueValues.fcField).Cube.DateTimeConsts);
    {$IFDEF SQL_TYPES_EXTRA1}
      fcdt_TimeStamp:
        fcDecodeDate(PfcxTimeStampUV(AUValue).Value, PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil, ADatePaths, TfcxCommonField(FUniqueValues.fcField).Cube.DateTimeConsts);
    {$ENDIF}
    end;
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    begin
      if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
        if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
          if (ADateTypeSet = []) or (ADateType in ADateTypeSet) then
          begin
            case ADateType of
              odt_Year:
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.Year, AUVIndex);
              odt_Month:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.Month, AUVIndex);
                end;
              odt_Day:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.Day, AUVIndex);
                end;
              odt_DayOfWeek:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.DayOfWeek, AUVIndex);
                end;
              odt_Quarter:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.Quarter, AUVIndex);
                end;
              odt_WeekNumber:
                begin
                  if ADatePaths.YearOfWeek = ADatePaths.Year then
                    PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.WeekNumber + 100, AUVIndex)
                  else
                  if ADatePaths.YearOfWeek < ADatePaths.Year then
                    PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.WeekNumber, AUVIndex)
                  else
                    PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(200, AUVIndex);
                end;
              odt_DayOfYear:
                begin
                  PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.InsertPathValue(ADatePaths.DayOfYear, AUVIndex);
                end;
            end;
          end;
    end;
  end;
end;

procedure TfcxDatePathsManager.InternalAddDatePathSaved(ADatePathField: Pointer);
begin
  if PfcxCommonDatePathFieldArray(FPathFields)[TfcxCommonDatePathField(ADatePathField).DateType] = nil then
  begin
    PfcxCommonDatePathFieldArray(FPathFields)[TfcxCommonDatePathField(ADatePathField).DateType] := TfcxCommonDatePathField(ADatePathField);
    inc(FCount);
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end;
end;

(*
procedure TfcxDatePathsManager.LoadFromStream(ACubeStream: TStream);
var
  ABoolValue: Boolean;
  ACount, i: integer;
  ADateType: TfcxDateType;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(ABoolValue, SizeOf(Boolean));
    if ABoolValue then
    begin
      ADateType := TfcxDateType(i);
      if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
        PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.LoadFromStream(ACubeStream);
    end;
  end;
end;
*)

procedure TfcxDatePathsManager.LoadGroupsFromXML(AItem: TfcxXMLItem);
var
  i: integer;
  ADateType: TfcxDateType;
begin
  for i := 0 to AItem.Count - 1 do
  begin
    ADateType := TfcxDateType(GetEnumValue(TypeInfo(TfcxDateType), AItem[i].Prop['DateType']));
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.LoadGroupsFromXML(AItem[i]);
  end;
end;

procedure TfcxDatePathsManager.Recapacity(AOnlyForNotSavedFields: boolean = False);
var
  ADateType: TfcxDateType;
begin
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
        PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.Recapacity;
end;

procedure TfcxDatePathsManager.RemoveDatePath(ADatePath: TfcxDateType);
begin
  if PfcxCommonDatePathFieldArray(FPathFields)[ADatePath] <> nil then
  begin
    dec(FCount);
    TfcxCommonUVField(FUniqueValues.FfcField).Fields.DeleteField(PfcxCommonDatePathFieldArray(FPathFields)[ADatePath]);
//    PfcxCommonDatePathFieldArray(FPathFields)[ADatePath].Free;
    PfcxCommonDatePathFieldArray(FPathFields)[ADatePath] := nil;
{ TODO -cПроверить : Не забыли ли чего?}
  end;
end;

procedure TfcxDatePathsManager.SaveGroupsToXML(AItem: TfcxXMLItem;
  AOnlyForNotSavedFields: boolean);
var
  AItem1: TfcxXMLItem;
  ADateType: TfcxDateType;
begin
  if Count > 0 then
  begin
    AItem1 := AItem.Add;
    AItem1.Name := 'DatePaths';
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
      if (PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil) and
        (not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved or not AOnlyForNotSavedFields) then
        PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.SaveGroupsToXML(AItem1);
    if AItem1.Count = 0 then
      AItem1.Free;
  end;
end;

(*
procedure TfcxDatePathsManager.SaveToStream(ACubeStream: TStream);
var
  ABoolValue: Boolean;
  ADateType: TfcxDateType;
begin
  ACubeStream.Write(PathsCount, SizeOf(Integer));
  ABoolValue := False;
  ACubeStream.Write(ABoolValue, SizeOf(Boolean));
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
  begin
    ABoolValue := PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil;
    ACubeStream.Write(ABoolValue, SizeOf(Boolean));
    if ABoolValue then
// сохранение уз сплита и значения части основного УЗ
      PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.SaveToStream(ACubeStream);
  end;
end;
*)

procedure TfcxDatePathsManager.SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
var
  ADateType: TfcxDateType;
begin
  for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if PfcxCommonDatePathFieldArray(FPathFields)[ADateType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonDatePathFieldArray(FPathFields)[ADateType].Saved then
        PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.SetCapacity(ASetAllToNil);
end;

procedure TfcxDatePathsManager.SetDatePaths(ADatePaths: TfcxDateTypes);
var
  ADatePath: TfcxDateType;
begin
  for ADatePath := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
    if ADatePath in ADatePaths then
      AddDatePath(ADatePath)
    else
      RemoveDatePath(ADatePath);
end;

{ TfcxTimePathsManager }

function TfcxTimePathsManager.AddTimePath(ATimePath: TfcxTimeType): integer;
var
  AFieldProperties: TfcxFieldProperties;
begin
  Result := -1;
  if PfcxCommonTimePathFieldArray(FPathFields)[ATimePath] = nil then
  begin
{ TODO -cНеобходимо : Откуда брать эти значения, из базового поля или из значений по умолчании или добавить свойства а TfcxCustomSplitPath в или еще откуда-то?}
    AFieldProperties.CaseSensitive := not FUniqueValues.IgnoreCase;
    AFieldProperties.NullStr := FUniqueValues.NullCaption;
    AFieldProperties.CubeFieldName := TimeNamePrefixTable[ATimePath] + TfcxCommonField(FUniqueValues.fcField).CubeFieldName;
    AFieldProperties.CubeFieldDisplayLabel := fcxResources.Get(TimeLabelPrefixTable[ATimePath]) + TfcxCommonField(FUniqueValues.fcField).CubeFieldDisplayLabel;
    AFieldProperties.DataType := fcdt_Word;
    AFieldProperties.Saved := False;
    PfcxCommonTimePathFieldArray(FPathFields)[ATimePath] := TfcxCommonTimePathField(TfcxCommonUVField(FUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FUniqueValues.fcField).Fields.AddTimePathField(AFieldProperties, ATimePath)]);
    Result := PfcxCommonTimePathFieldArray(FPathFields)[ATimePath].Index;
    inc(FCount);
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end;

end;

procedure TfcxTimePathsManager.AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
  ATimeTypeSet: TfcxTimeTypes = []; AOnlyForNotSavedFields: boolean = False);
var
  ATimePaths: TfcxTimePaths;
  ATimeType: TfcxTimeType;
  ANeedExit: Boolean;
begin
  ANeedExit := True;
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
        if (ATimeTypeSet = []) or (ATimeType in ATimeTypeSet) then
        begin
          ANeedExit := False;
          Break;
        end;
  if ANeedExit then
    Exit;
  if FUniqueValues.FHasNull and (AUValue = FUniqueValues.FNullValue) then
  begin
// по новому. пока так.
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    begin
      if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
        if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
          if (ATimeTypeSet = []) or (ATimeType in ATimeTypeSet) then
            PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.AddPathValue(-1, AUVIndex);
    end;
  end
  else
  begin
    case FUniqueValues.FDataType of
      fcdt_DateTime:
        fcDecodeTime(PfcxDateTimeUV(AUValue).Value, ATimePaths);
      fcdt_Time:
        fcDecodeTime(PfcxIntegerUV(AUValue).Value, ATimePaths);
    {$IFDEF SQL_TYPES_EXTRA1}
      fcdt_TimeStamp:
        fcDecodeTime(PfcxTimeStampUV(AUValue).Value, ATimePaths);
    {$ENDIF}
    end;
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    begin
      if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
        if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
          if (ATimeTypeSet = []) or (ATimeType in ATimeTypeSet) then
          begin
            case ATimeType of
              ott_Hour:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.AddPathValue(ATimePaths.Hour, AUVIndex);
                end;
              ott_Minute:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.AddPathValue(ATimePaths.Minute, AUVIndex);
                end;
              ott_Second:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.AddPathValue(ATimePaths.Second, AUVIndex);
                end;
              ott_Millisecond:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.AddPathValue(ATimePaths.Millisecond, AUVIndex);
                end;
            end;
          end;
    end;
  end;
end;

procedure TfcxTimePathsManager.ClearGroups;
var
  ATimeType: TfcxTimeType;
begin
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.ClearGroups;
end;

constructor TfcxTimePathsManager.Create(AUniqueValues: TfcxBaseUniqueValues);
begin
//  FSSplitPathType := fcsspt_Time;
  FPathsCount := fcTimeTypeLength + 1;
  inherited;
end;

procedure TfcxTimePathsManager.CreatePathUVsArray(AOnlyForNotSavedFields: boolean = False);
var
  ATimeType: TfcxTimeType;
begin
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
        PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.CreatePathUVsArray;
end;

procedure TfcxTimePathsManager.Delete(AUVIndex: integer);
var
  ATimeType: TfcxTimeType;
begin
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.DeleteValue(AUVIndex);
end;

function TfcxTimePathsManager.GetCaptionAtIndex(ATimePath: TfcxTimeType;
  AIndex: integer): TfcxString;
begin
  Result := PfcxCommonTimePathFieldArray(FPathFields)[ATimePath].TimePathProcessor.Caption[PfcxCommonTimePathFieldArray(FPathFields)[ATimePath].TimePathProcessor.ValueAtIndex[AIndex]];
end;

function TfcxTimePathsManager.GetCaptionAtValue(ATimePath: TfcxTimeType;
  AValue: integer): TfcxString;
begin
  Result := PfcxCommonTimePathFieldArray(FPathFields)[ATimePath].TimePathProcessor.Caption[AValue];
end;

function TfcxTimePathsManager.GetTimePathField(
  ATimePath: TfcxTimeType): Pointer;
begin
  Result := PfcxCommonTimePathFieldArray(FPathFields)[ATimePath];
end;

function TfcxTimePathsManager.GetUseTimePath(
  ATimePath: TfcxTimeType): Boolean;
begin
  Result := PfcxCommonTimePathFieldArray(FPathFields)[ATimePath] <> nil;
end;

procedure TfcxTimePathsManager.Insert(AUVIndex: integer);
var
  ATimeType: TfcxTimeType;
begin
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.InsertValue(AUVIndex);
end;

procedure TfcxTimePathsManager.InsertUValue(AUValue: PfcxCommonUV;
  AUVIndex: integer; ATimeTypeSet: TfcxTimeTypes;
  AOnlyForNotSavedFields: boolean);
var
  ATimePaths: TfcxTimePaths;
  ATimeType: TfcxTimeType;
  ANeedExit: Boolean;
begin
  ANeedExit := True;
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
        if (ATimeTypeSet = []) or (ATimeType in ATimeTypeSet) then
        begin
          ANeedExit := False;
          Break;
        end;
  if ANeedExit then
    Exit;
  if FUniqueValues.FHasNull and (AUValue = FUniqueValues.FNullValue) then
  begin
// по новому. пока так.
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    begin
      if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
        if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
          if (ATimeTypeSet = []) or (ATimeType in ATimeTypeSet) then
            PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.InsertPathValue(-1, AUVIndex);
    end;
  end
  else
  begin
    case FUniqueValues.FDataType of
      fcdt_DateTime:
        fcDecodeTime(PfcxDateTimeUV(AUValue).Value, ATimePaths);
      fcdt_Time:
       fcDecodeTime(PfcxIntegerUV(AUValue).Value, ATimePaths);
    {$IFDEF SQL_TYPES_EXTRA1}
      fcdt_TimeStamp:
        fcDecodeTime(PfcxTimeStampUV(AUValue).Value, ATimePaths);
    {$ENDIF}
    end;   
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    begin
      if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
        if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
          if (ATimeTypeSet = []) or (ATimeType in ATimeTypeSet) then
          begin
            case ATimeType of
              ott_Hour:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.InsertPathValue(ATimePaths.Hour, AUVIndex);
                end;
              ott_Minute:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.InsertPathValue(ATimePaths.Minute, AUVIndex);
                end;
              ott_Second:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.InsertPathValue(ATimePaths.Second, AUVIndex);
                end;
              ott_Millisecond:
                begin
                  PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.InsertPathValue(ATimePaths.Millisecond, AUVIndex);
                end;
            end;
          end;
    end;
  end;
end;

procedure TfcxTimePathsManager.InternalAddTimePathSaved(ATimePathField: Pointer);
begin
  if PfcxCommonTimePathFieldArray(FPathFields)[TfcxCommonTimePathField(ATimePathField).TimeType] = nil then
  begin
    PfcxCommonTimePathFieldArray(FPathFields)[TfcxCommonTimePathField(ATimePathField).TimeType] := TfcxCommonTimePathField(ATimePathField);
    inc(FCount);
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end;
end;

(*
procedure TfcxTimePathsManager.LoadFromStream(ACubeStream: TStream);
var
  ABoolValue: Boolean;
  ACount, i: integer;
  ATimeType: TfcxTimeType;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  for i := 0 to ACount - 1 do
  begin
    ACubeStream.Read(ABoolValue, SizeOf(Boolean));
    if ABoolValue then
    begin
      ATimeType := TfcxTimeType(i);
      if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
        PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.LoadFromStream(ACubeStream);
    end;
  end;
end;
*)
procedure TfcxTimePathsManager.LoadGroupsFromXML(AItem: TfcxXMLItem);
var
  i: integer;
  ATimeType: TfcxTimeType;
begin
  for i := 0 to AItem.Count - 1 do
  begin
    ATimeType := TfcxTimeType(GetEnumValue(TypeInfo(TfcxTimeType), AItem[i].Prop['TimeType']));
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.LoadGroupsFromXML(AItem[i]);
  end;
end;

procedure TfcxTimePathsManager.Recapacity(AOnlyForNotSavedFields: boolean = False);
var
  ATimeType: TfcxTimeType;
begin
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
        PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.Recapacity;
end;

procedure TfcxTimePathsManager.RemoveTimePath(ATimePath: TfcxTimeType);
begin
  if PfcxCommonTimePathFieldArray(FPathFields)[ATimePath] <> nil then
  begin
    dec(FCount);
    TfcxCommonUVField(FUniqueValues.FfcField).Fields.DeleteField(PfcxCommonTimePathFieldArray(FPathFields)[ATimePath]);
    PfcxCommonTimePathFieldArray(FPathFields)[ATimePath].Free;
    PfcxCommonTimePathFieldArray(FPathFields)[ATimePath] := nil;
{ TODO -cПроверить : Не забыли ли чего?}
  end;
end;

procedure TfcxTimePathsManager.SaveGroupsToXML(AItem: TfcxXMLItem;
  AOnlyForNotSavedFields: boolean);
var
  AItem1: TfcxXMLItem;
  ATimeType: TfcxTimeType;
begin
  if Count > 0 then
  begin
    AItem1 := AItem.Add;
    AItem1.Name := 'TimePaths';
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
      if (PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil) and
        (not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved or not AOnlyForNotSavedFields) then
        PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.SaveGroupsToXML(AItem1);
    if AItem1.Count = 0 then
      AItem1.Free;
  end;
end;

(*
procedure TfcxTimePathsManager.SaveToStream(ACubeStream: TStream);
var
  ABoolValue: Boolean;
  ATimeType: TfcxTimeType;
begin
  ACubeStream.Write(PathsCount, SizeOf(Integer));
  ABoolValue := False;
  ACubeStream.Write(ABoolValue, SizeOf(Boolean));
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
  begin
    ABoolValue := PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil;
    ACubeStream.Write(ABoolValue, SizeOf(Boolean));
    if ABoolValue then
// сохранение уз сплита и значения части основного УЗ
      PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.SaveToStream(ACubeStream);
  end;
end;
*)
procedure TfcxTimePathsManager.SetCapacity(ASetAllToNil: boolean; AOnlyForNotSavedFields: boolean = False);
var
  ATimeType: TfcxTimeType;
begin
  for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if PfcxCommonTimePathFieldArray(FPathFields)[ATimeType] <> nil then
      if not AOnlyForNotSavedFields or not PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].Saved then
        PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.SetCapacity(ASetAllToNil);
end;

procedure TfcxTimePathsManager.SetTimePaths(ATimePaths: TfcxTimeTypes);
var
  ATimePath: TfcxTimeType;
begin
  for ATimePath := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
    if ATimePath in ATimePaths then
      AddTimePath(ATimePath)
    else
      RemoveTimePath(ATimePath);
end;

{ TfcxBaseUniqueValues }

function TfcxBaseUniqueValues.AddNewValue(AValue: Pointer;
  var AUValue: PfcxCommonUV): boolean;
var
  bucket: Word;
  LenPodHash, i, R, c, L: integer;
begin
  Result := False;
  if FLoading then
// процесс загрузки из источника
  begin
// сперва проверим на наличие
    L := 0;
    if AValue = nil then
    begin
      if FHasNull then // Ну вот и нашли
      begin
        AUValue := FNullValue;
        Exit;
      end;
      bucket := 0;
      LenPodHash := FUVHash.FHashCount[bucket];
    end
    else
    begin
// ищем в корзине bucket
      bucket := FTypeProcessor.Hash(AValue);
      LenPodHash := FUVHash.FHashCount[bucket];
      R := LenPodHash - 1;
      if (bucket = 0) and (LenPodHash > 0) and FHasNull and (FList[FUVHash.FHashTable[bucket, L]] = FNullValue) then
        L := 1;
      while L <= R do
      begin
        I := (L + R) shr 1; // middle
        c := FTypeProcessor.CompareForInsert(FList[FUVHash.FHashTable[bucket, I]], AValue);
        if C < 0 then
          L := I + 1
        else if C > 0 then
          R := I - 1
        else
        begin // Ну вот и нашли
          AUValue := FList[FUVHash.FHashTable[bucket, I]];
          FreeMem(AValue);
          Exit;
        end;
      end;
    end;
// не нашли. L - место в корзине bucket
    Inc(FUVHash.FHashCount[bucket]);
    ReallocMem(FUVHash.FHashTable[bucket], FUVHash.FHashCount[bucket] * SizeOf(integer));
    if L < LenPodHash then
      System.Move(FUVHash.FHashTable[bucket][L], FUVHash.FHashTable[bucket][succ(L)],
        (LenPodHash - L) * SizeOf(integer));
// создаём УЗ и добавляем в корзину bucket
    FUVHash.FHashTable[bucket, L] := AddUValue(FTypeProcessor.NewUValue(AValue));
    AUValue := FList[FUVHash.FHashTable[bucket, L]];
    if AValue = nil then
    begin
      FHasNull := True;
      FNullValue := AUValue;
    end;
    Result := True; // добавили
// Обработать атрибуты, принадлежащие основному источнику.
(* ?? выяснить необходимость !!
    if not (Self is TfcxAttributeUniqueValues) then
      FSplitManager.AddUValueMainReference(AUValue, FUVHash.FHashTable[bucket, L])
    else
    if not TfcxAttributeUniqueValues(Self).FromMasterSource then
*)
      FSplitManager.AddUValueMainReference(AUValue, FUVHash.FHashTable[bucket, L]);
  end
  else
  begin
// рабочий режим
// проверяем на наличие такого УЗ
    if FindUValue(AValue, AUValue, L, True) then
      exit;
    if AValue = nil then
    begin
      FHasNull := True;
      FNullValue := AUValue;
    end;
// вставляем на найденное место в список
    AUValue := Insert(AValue, L);
    FAddedUValues := True;
    Result := True; // добавили
  end;
end;

function TfcxBaseUniqueValues.AddNewVarValue(AVarValue: Variant;
  var AUValue: PfcxCommonUV): boolean;
var
  APointer: Pointer;
begin
  APointer := VarValueToPointer(AVarValue);
  Result := AddNewValue(APointer, AUValue);
end;

function TfcxBaseUniqueValues.AddUValue(AUValue: PfcxCommonUV): Integer;
begin
  if FCount = FCapacity then
  begin
    Capacity := Capacity + 10000; // change to make this faster
(* ?? выяснить необходимость !!
    if not (Self is TfcxAttributeUniqueValues) then
      FSplitManager.AttributesManager.ChangeCapacity
    else
    if not TfcxAttributeUniqueValues(Self).FromMasterSource then
*)
      FSplitManager.AttributesManager.ChangeCapacity;
  end;
  FList[FCount] := AUValue;
  Result := FCount;
  inc(FCount);
end;

procedure TfcxBaseUniqueValues.Clear;
var
  i: Integer;
begin
  if FWithGroup then
    WithGroup := False;
  if FWithCustomCaption then
    WithCustomCaption := False;
  for i := 0 to FCount - 1 do
    FTypeProcessor.FreeUValue(FList[i]);
  FreeMem(FIndexList);
  FIndexList := nil;
  FreeMem(FList);
  FList := nil;
  FreeAndNil(FSplitManager);
  FreeAndNil(FUVHash);
  FreeAndNil(FTypeProcessor);
  FCapacity := 0;
  FCount := 0;
  FHasNull := False;
  FNullValue := nil;
  CaptionSourceAttribute := '';
  OrderSourceAttribute := '';
end;

procedure TfcxBaseUniqueValues.ClearGroups;
begin
  if FWithGroup then
    WithGroup := False;
  FSplitManager.ClearGroups;
end;

constructor TfcxBaseUniqueValues.Create(AfcField: TObject; AWithCustomCaption: boolean; ADataTypeProcessorClass: TfcxDataTypeProcessorClass = nil);
begin
  FLoadingFromStream := False;
  FCapacity := 0;
  FCount := 0;
  FList := nil;
  FIndexList := nil;
  FLoading := False;
  FHasNull := False;
  FNullValue := nil;
  FfcField := AfcField;
  FDataType := TfcxCommonField(AfcField).DataType;//ADataType;
  FVarType := DataTypeToVarType[FDataType];
  FGroupManager := nil;
  FSplitManager := TfcxSplitManager.Create(Self);
  WithCustomCaption := AWithCustomCaption;
  if WithCustomCaption then
    GetCaption := GetCaptionCustom
  else
    GetCaption := GetCaptionSimple;
  CreateTypeProcessor(FDataType, ADataTypeProcessorClass);
  FUVHash := TfcxUVHash.Create(FTypeProcessor);
  FCaptionSourceAttribute := '';
  FOrderSourceAttribute := '';
  FCaptionSourceAttributeIndex := -1;
  FOrderSourceAttributeIndex := -1;
  FAddedUValues := False;
  FTempCount := 0;
end;

procedure TfcxBaseUniqueValues.CreateTypeProcessor(ADataType: TfcxDataType;
  ADataTypeProcessorClass: TfcxDataTypeProcessorClass);
begin
  if ADataTypeProcessorClass = nil then
    FTypeProcessor := cfcProcessorMap[FDataType].Create(Self)
  else
    FTypeProcessor := ADataTypeProcessorClass.Create(Self);
end;

procedure TfcxBaseUniqueValues.Delete(AUVIndex: integer);
begin
  if WithCustomCaption then
    FCustomCaptionManager.Delete(AUVIndex);
  if WithGroup then
    FGroupManager.Delete(AUVIndex);
  FSplitManager.Delete(AUVIndex);
  if FHasNull and (FList[AUVIndex] = FNullValue) then
  begin
    FHasNull := False;
    FNullValue := nil;
  end;
  FTypeProcessor.FreeUValue(FList[AUVIndex]);
  Dec(FCount);
  if AUVIndex < FCount then
    System.Move(FList^[AUVIndex + 1], FList^[AUVIndex],
      (FCount - AUVIndex) * fcPointerSize);
{ TODO -cНеобходимо : Нужно не забыть потом проставить заново индексы !!! }
end;

destructor TfcxBaseUniqueValues.Destroy;
begin
  Clear;
//  FreeAndNil(FCustomCaptionManager);
//  FreeAndNil(FGroupManager);
//  FreeAndNil(FSplitManager);
//  FreeAndNil(FUVHash);
//  FreeAndNil(FTypeProcessor);
  inherited;
end;

function TfcxBaseUniqueValues.FindUValue(AValue: Pointer;
  var AUValue: PfcxCommonUV; var AUVIndex: integer;
  AFreeValue: boolean): boolean;
var
  R, I, c: integer;
  AList: PfcxCUVArray;
begin
  if FIndexList = nil then
    AList := FList
  else
    AList := FIndexList;
  AUVIndex := 0;
  if AValue = nil then
  begin
    if (Count > 0) and FHasNull and (AList[AUVIndex] = FNullValue) then
    begin
      Result := True; // Ну вот и нашли
      AUValue := FNullValue;
      Exit;
    end;
  end
  else
  begin
    R := Count - 1;
    if (Count > 0) and FHasNull and (AList[AUVIndex] = FNullValue) then
      AUVIndex := 1;
    while AUVIndex <= R do
    begin
      I := (AUVIndex + R) shr 1; // middle
      c := FTypeProcessor.CompareForInsert(AList[I], AValue);
      if C < 0 then
        AUVIndex := I + 1
      else if C > 0 then
        R := I - 1
      else
      begin
        Result := True; // Ну вот и нашли
        AUValue := AList[I];
        AUVIndex := I;
        if AFreeValue then // освободим память
          FreeMem(AValue);
        Exit;
      end;
    end;
  end;
  Result := False; // Не нашли
end;

function TfcxBaseUniqueValues.GetCaptionAttribute(
  AUVIndex: Integer): TfcxString;
begin
  Result := FSplitManager.FAttributesManager.FListUValues[FCaptionSourceAttributeIndex].FTypeProcessor.Caption[FSplitManager.FAttributesManager.FListUVofPaths[FCaptionSourceAttributeIndex][AUVIndex]];
end;

function TfcxBaseUniqueValues.GetCaptionCustom(AUVIndex: Integer): TfcxString;
begin
  if (FCustomCaptionManager.FCaptionList[AUVIndex] <> nil) then
    Result := FCustomCaptionManager.FCaptionList[AUVIndex]
  else
    Result := FTypeProcessor.Caption[FList[AUVIndex]];
end;

function TfcxBaseUniqueValues.GetCaptionCustomAttribute(
  AUVIndex: Integer): TfcxString;
begin
  if (FCustomCaptionManager.FCaptionList[AUVIndex] <> nil) then
    Result := FCustomCaptionManager.FCaptionList[AUVIndex]
  else
    Result := FSplitManager.FAttributesManager.FListUValues[FCaptionSourceAttributeIndex].FTypeProcessor.Caption[FSplitManager.FAttributesManager.FListUVofPaths[FCaptionSourceAttributeIndex][AUVIndex]];
end;

function TfcxBaseUniqueValues.GetCaptionSimple(
  AUVIndex: Integer): TfcxString;
begin
  Result := FTypeProcessor.Caption[FList[AUVIndex]];
end;

function TfcxBaseUniqueValues.GetFieldIndex: Integer;
begin
  Result := TfcxCommonField(FfcField).Index;
end;

function TfcxBaseUniqueValues.GetGroupIndexOfUV(AUVIndex: Integer): Integer;
begin
  if not FWithGroup then
    Result := -1
  else
    if FGroupManager.FGroupParentList[AUVIndex].IsOther and not FGroupManager.ExistsOther then
      Result := -1
    else
      Result := FGroupManager.FGroupParentList[AUVIndex].Index
{
    if FGroupManager.FGroupParentList[AUVIndex] <> nil then
      Result := FGroupManager.FGroupParentList[AUVIndex].Index
    else
      Result := -1
}
end;

function TfcxBaseUniqueValues.GetGroupManager: TfcxCommonGroupManager;
begin
  if not FWithGroup then
    RaisefcError(exfcGroupNotSupported, []);
  Result := FGroupManager;
end;

function TfcxBaseUniqueValues.GetIgnoreCase: Boolean;
begin
  Result := not TfcxCommonField(FfcField).CaseSensitive;
end;

function TfcxBaseUniqueValues.GetIsNull(AIndex: Integer): Boolean;
begin
  Result := FHasNull and (FList[AIndex] = FNullValue);
end;

function TfcxBaseUniqueValues.GetNullCaption: TfcxString;
begin
  Result := TfcxCommonField(FfcField).NullStr;
end;

function TfcxBaseUniqueValues.GetUValue(AIndex: Integer): PfcxCommonUV;
begin
  Result := FList[AIndex];
end;

function TfcxBaseUniqueValues.GetUVInGroup(AUVIndex: Integer): Boolean;
begin
  if not FWithGroup then
    Result := False
  else
    Result := not FGroupManager.FGroupParentList[AUVIndex].IsOther or FGroupManager.ExistsOther;
end;

function TfcxBaseUniqueValues.GetValueAsVariant(
  AUValue: PfcxCommonUV): Variant;
begin
  Result := FTypeProcessor.AsVariant(AUValue);
end;

function TfcxBaseUniqueValues.GetValueAsVariantByIndex(
  AUIndex: integer): Variant;
begin
  Result := FTypeProcessor.AsVariant(FList[AUIndex]);
end;

function TfcxBaseUniqueValues.Insert(AValue: Pointer;
  AUVIndex: integer): PfcxCommonUV;
var
  I: integer;  
begin
  Result := FTypeProcessor.NewUValue(AValue);
  Count := Count + 1;
  if FIndexList <> nil then
  begin
    Result.Index := Count - 1;
    if AUVIndex < (Count - 1) then
      System.Move(FIndexList[AUVIndex], FIndexList[succ(AUVIndex)],
        (Count - 1 - AUVIndex) * fcPointerSize);
    FIndexList[AUVIndex] := Result;
    FList[Count - 1] := Result;
//    SetIndex;
  { TODO -cНеобходимо : Нужно решить SetIndex или этот кусок кода. }
  //  for I := AUVIndex + 1 to Count - 1 do
  //    FList[I].Index := I;

    if WithGroup then
      FGroupManager.Insert(Count - 1);
    FSplitManager.Insert(Count - 1);
    if WithCustomCaption then
      FCustomCaptionManager.Insert(Count - 1);
  end
  else
  begin
    Result.Index := AUVIndex;
    if AUVIndex < (Count - 1) then
      System.Move(FList[AUVIndex], FList[succ(AUVIndex)],
        (Count - 1 - AUVIndex) * fcPointerSize);
    FList[AUVIndex] := Result;
//    SetIndex;
{ TODO -cНеобходимо : Нужно решить SetIndex или этот кусок кода. }
    for I := AUVIndex + 1 to Count - 1 do
      FList[I].Index := I;

    if WithGroup then
      FGroupManager.Insert(AUVIndex);
    FSplitManager.Insert(AUVIndex);
    if WithCustomCaption then
      FCustomCaptionManager.Insert(AUVIndex);
  end;
end;

procedure TfcxBaseUniqueValues.LoadAttributes(AOnlyForNotSavedFields: boolean);
var
  i, j: integer;
  AExists: boolean;
  ADataSets: array of TfcxDataSet;
  AAttributeManagerArr: Array of Array of Integer;
//  AArrCustomSplitManager: array of TfcxCustomSplitManager;
begin
  if FLoading then
    RaisefcError(exfcNotSupportedDuringLoading, []);
  if not (FSplitManager.FUseDateSplit or FSplitManager.FUseTimeSplit or (FSplitManager.AttributesCount > 0)) then
    Exit;
(*
  if not AOnlyForNotSavedFields and WithCustomCaption then
    FCustomCaptionManager.SetCapacity(True);
  if not AOnlyForNotSavedFields and WithGroup then
    FGroupManager.SetCapacity(True);
*)

  SetLength(ADataSets, 0);
  SetLength(AAttributeManagerArr, 0);
  if not AOnlyForNotSavedFields then
    for i := 0 to FSplitManager.AttributesCount - 1 do
    begin
      if (FSplitManager.AttributesManager[i].AttributeType = fcxsft_Reference) and
         not FSplitManager.AttributesManager[i].FromMasterSource then
      begin
        AExists := False;
        for j := 0 to High(ADataSets) do
          if ADataSets[j] = FSplitManager.AttributesManager[i].FDataField.DataSet then
          begin
            SetLength(AAttributeManagerArr[High(AAttributeManagerArr)], Length(AAttributeManagerArr[High(AAttributeManagerArr)]) + 1);
            AAttributeManagerArr[High(AAttributeManagerArr)][High(AAttributeManagerArr[High(AAttributeManagerArr)])] := i;
            AExists := True;
            Break;
          end;
        if not AExists then
        begin
          SetLength(ADataSets, Length(ADataSets) + 1);
          SetLength(AAttributeManagerArr, Length(AAttributeManagerArr) + 1);
          SetLength(AAttributeManagerArr[High(AAttributeManagerArr)], 1);
          ADataSets[High(ADataSets)] := FSplitManager.AttributesManager[i].FDataField.DataSet;
          AAttributeManagerArr[High(AAttributeManagerArr)][0] := i;
        end
      end
    end;

  FSplitManager.SetCapacity(True, AOnlyForNotSavedFields);
  if Count > 0 then
  begin
    FSplitManager.StartLoad(False);
    if not AOnlyForNotSavedFields then
      for i := 0 to High(ADataSets) do
      begin
// загружаем Reference атрибуты
        FSplitManager.FAttributesManager.LoadReferenceSource(FfcField, ADataSets[i], AAttributeManagerArr[i]);
      end;
// заполняем остальные атрибуты
    if FSplitManager.FUseDateSplit or FSplitManager.FUseTimeSplit or (FSplitManager.AttributesCount > 0) then
      for i := 0 to Count - 1 do
        FSplitManager.AddUValue(flist[i], i, False);
    if FSplitManager.FUseDateSplit then
      FSplitManager.DatePathsManager.CreatePathUVsArray;
    if FSplitManager.FUseTimeSplit then
      FSplitManager.TimePathsManager.CreatePathUVsArray;

    FSplitManager.Sort(True, False);
    FSplitManager.SetIndex(False);
    FSplitManager.StopLoad(False);
    FSplitManager.SetCaptionAndGroupCapacity(False);
    FSplitManager.LoadAttributes(AOnlyForNotSavedFields);
    FSplitManager.Sort(False, False);

    if FSplitManager.AttributesCount > 0 then
    begin
// CalculateAfterAll
      FSplitManager.StartLoad(True);
      for i := 0 to Count - 1 do
        FSplitManager.AddUValue(flist[i], i, True);
      FSplitManager.Sort(True, True);
      FSplitManager.SetIndex(True);
      FSplitManager.StopLoad(True);
      FSplitManager.SetCaptionAndGroupCapacity(True);
      FSplitManager.Sort(False, True);
    end;
  end;
end;

procedure TfcxBaseUniqueValues.FillNonSavedUVs;
var
  i: integer;
begin
  if FLoading then
    RaisefcError(exfcNotSupportedDuringLoading, []);
  if Count > 0 then
  begin
    if TfcxCommonField(FfcField).Fields.ExistsNotSaved(True) then
    begin
      FSplitManager.SetCapacity(True, True);
      FSplitManager.StartLoad(False, True);
      if FSplitManager.FUseDateSplit or FSplitManager.FUseTimeSplit or (FSplitManager.AttributesCount > 0) then
        for i := 0 to Count - 1 do
          FSplitManager.AddUValue(flist[i], i, False, True);
      if FSplitManager.FUseDateSplit then
        FSplitManager.DatePathsManager.CreatePathUVsArray(True);
      if FSplitManager.FUseTimeSplit then
        FSplitManager.TimePathsManager.CreatePathUVsArray(True);

      FSplitManager.Sort(True, False, True);
      FSplitManager.SetIndex(False, True);
      FSplitManager.StopLoad(False, True);
    end;
    FSplitManager.FillNonSavedUVs;
    FSplitManager.Sort(False, False, True);
// CalculateAfterAll
    if TfcxCommonField(FfcField).Fields.ExistsNotSaved(True) then
    begin
      FSplitManager.StartLoad(True, True);
      if FSplitManager.AttributesCount > 0 then
        for i := 0 to Count - 1 do
          FSplitManager.AddUValue(flist[i], i, True, True);
      FSplitManager.Sort(True, True, True);
      FSplitManager.SetIndex(True, True);
      FSplitManager.StopLoad(True, True);
    end;
    FSplitManager.Recapacity(True);
  end;
end;

procedure TfcxBaseUniqueValues.LoadAddons;
var
  i: integer;
begin
  if FLoading then
    RaisefcError(exfcNotSupportedDuringLoading, []);
  FSplitManager.SetCapacity(True);
  FSplitManager.StartLoad(False);
  if FSplitManager.FUseDateSplit or FSplitManager.FUseTimeSplit or (FSplitManager.AttributesCount > 0) then
    if Count > 0 then
      for i := 0 to Count - 1 do
        FSplitManager.AddUValue(flist[i], i, False);
  FSplitManager.Sort(True, False);
  FSplitManager.SetIndex(False);
  if FSplitManager.FUseDateSplit then
    FSplitManager.DatePathsManager.CreatePathUVsArray;
  if FSplitManager.FUseTimeSplit then
    FSplitManager.TimePathsManager.CreatePathUVsArray;
  FSplitManager.StopLoad(False);
end;

procedure TfcxBaseUniqueValues.LoadAddons(
  AAttributeUniqueValuesArr: array of TfcxAttributeUniqueValues;
  ADateTypeSet: TfcxDateTypes; ATimeTypeSet: TfcxTimeTypes);
var
  i, j: integer;
  ADateType: TfcxDateType;
  ATimeType: TfcxTimeType;
begin
  if FLoading then
    RaisefcError(exfcNotSupportedDuringLoading, []);
  for j := 0 to High(AAttributeUniqueValuesArr) do
  begin
// not need?    FSplitManager.SetCapacity(True);
    if not AAttributeUniqueValuesArr[j].FromMasterSource then
      AAttributeUniqueValuesArr[j].Loading := True;
  end;
  if ADateTypeSet <> [] then
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
      if ADateType in ADateTypeSet then
        TfcxCommonDatePathField(FSplitManager.DatePathsManager.DatePathField[ADateType]).DatePathProcessor.CreatePathValuesArray;
  if ATimeTypeSet <> [] then
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
      if ATimeType in ATimeTypeSet then
        TfcxCommonTimePathField(FSplitManager.TimePathsManager.TimePathField[ATimeType]).TimePathProcessor.CreatePathValuesArray;

  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      for j := 0 to High(AAttributeUniqueValuesArr) do
        case AAttributeUniqueValuesArr[j].AttributeType of
          fcxsft_Custom, fcxsft_Date, fcxsft_Time:
            FSplitManager.AttributesManager.AddUValue(flist[i], i, AAttributeUniqueValuesArr[j]);
        end;
      if ADateTypeSet <> [] then
        FSplitManager.DatePathsManager.AddUValue(flist[i], i, ADateTypeSet);
      if ATimeTypeSet <> [] then
        FSplitManager.TimePathsManager.AddUValue(flist[i], i, ATimeTypeSet);
    end;

  if ADateTypeSet <> [] then
    for ADateType := Succ(Low(TfcxDateType)) to High(TfcxDateType) do
      if ADateType in ADateTypeSet then
        TfcxCommonDatePathField(FSplitManager.DatePathsManager.DatePathField[ADateType]).DatePathProcessor.CreatePathUVsArray;
  if ATimeTypeSet <> [] then
    for ATimeType := Succ(Low(TfcxTimeType)) to High(TfcxTimeType) do
      if ATimeType in ATimeTypeSet then
        TfcxCommonTimePathField(FSplitManager.TimePathsManager.TimePathField[ATimeType]).TimePathProcessor.CreatePathUVsArray;
  for j := 0 to High(AAttributeUniqueValuesArr) do
    if not AAttributeUniqueValuesArr[j].FromMasterSource then
    begin
// not need?    FSplitManager.SetCapacity(True);
      AAttributeUniqueValuesArr[j].Sort(True);
      AAttributeUniqueValuesArr[j].SetIndex;
      AAttributeUniqueValuesArr[j].Loading := False;
    end;
end;

procedure TfcxBaseUniqueValues.LoadFromOldStream(ACubeStream: TStream;
  AMajorVersion, AMinorVersion: byte);
var
  i, ACount: integer;
  AHasNull: boolean;
  ACaption: Pointer;
  AVarValue: Variant;
begin
  ACubeStream.Read(ACount, SizeOf(integer));
  Count := ACount;
  WithCustomCaption := (AMajorVersion > 1) or (AMinorVersion <> 1);
  ACubeStream.Read(AHasNull, SizeOf(Boolean));
  if Count > 0 then
    if WithCustomCaption then
    begin
      FList[0] := FTypeProcessor.LoadFromOldStream(ACubeStream);
      if not AHasNull then
      begin
        AVarValue := ReadOldVariant(ACubeStream, TfcxCommonField(FfcField).OldSavedCubeField^.VarType);
        FTypeProcessor.SetVariantValue(FList[0], AVarValue);
      end;
      ACaption := PAnsiCharToPChar(ReadPChar(ACubeStream));
      if ACaption <> nil then
        FCustomCaptionManager.FCaptionList[0] := ACaption;
      for i := 1 to Count - 1 do
      begin
        FList[i] := FTypeProcessor.LoadFromOldStream(ACubeStream);
        AVarValue := ReadOldVariant(ACubeStream, TfcxCommonField(FfcField).OldSavedCubeField^.VarType);
        FTypeProcessor.SetVariantValue(FList[i], AVarValue);
        ACaption := PAnsiCharToPChar(ReadPChar(ACubeStream));
        if ACaption <> nil then
          FCustomCaptionManager.FCaptionList[i] := ACaption;
      end;
    end
    else
    begin
      FList[0] := FTypeProcessor.LoadFromOldStream(ACubeStream);
      if not AHasNull then
      begin
        AVarValue := ReadOldVariant(ACubeStream, TfcxCommonField(FfcField).OldSavedCubeField^.VarType);
        FTypeProcessor.SetVariantValue(FList[0], AVarValue);
      end;
      for i := 1 to Count - 1 do
      begin
        FList[i] := FTypeProcessor.LoadFromOldStream(ACubeStream);
        AVarValue := ReadOldVariant(ACubeStream, TfcxCommonField(FfcField).OldSavedCubeField^.VarType);
        FTypeProcessor.SetVariantValue(FList[i], AVarValue);
      end;
    end;
  if AHasNull then
  begin
    FHasNull := True;
    FNullValue := FList[0];
  end;
// недоделано ???
  SetIndex;
end;

procedure TfcxBaseUniqueValues.LoadFromStream(ACubeStream: TStream);
var
  i, ACount: integer;
  AHasNull, ABoolValue: boolean;
  AIntArray: PfcxIntegerArray;
begin
  FLoadingFromStream := True;
  ACubeStream.Read(ACount, SizeOf(integer));
  Count := ACount;
  ACubeStream.Read(AHasNull, SizeOf(Boolean));
  for i := 0 to Count - 1 do
  begin
    FList[i] := FTypeProcessor.LoadFromStream(ACubeStream);
  end;
  if AHasNull then
  begin
    FHasNull := True;
    FNullValue := FList[0];
  end;

  if TfcxCommonField(fcField).MasterField <> nil then
  begin

    GetMem(AIntArray, SizeOf(Integer) * TfcxCommonField(fcField).MasterField.UniqueValues.Count);
    ACubeStream.Read(AIntArray[0], SizeOf(Integer) * TfcxCommonField(fcField).MasterField.UniqueValues.Count);

    for i := 0 to TfcxCommonField(fcField).MasterField.UniqueValues.Count - 1 do
      TfcxCommonField(fcField).MasterField.UniqueValues.SplitManager.AttributesManager.FListUVofPaths[TfcxCommonField(fcField).AttributeIndex][i] := UValue[AIntArray[i]];

    FreeMem(AIntArray);
  end;

// Custom Caption
  ACubeStream.Read(ABoolValue, SizeOf(Boolean));
  if ABoolValue then
  begin
    WithCustomCaption := ABoolValue;
    FCustomCaptionManager.LoadFromStream(ACubeStream);
  end;
//  if Self is TfcxBaseUniqueValues then
  SetIndex;
// Groups
  ACubeStream.Read(ABoolValue, SizeOf(Boolean));
  if ABoolValue then
  begin
    WithGroup := ABoolValue;
    FGroupManager.LoadFromStream(ACubeStream);
  end;
  FLoadingFromStream := False;
end;

procedure TfcxBaseUniqueValues.LoadGroupsFromXML(AItem: TfcxXMLItem);
begin
  if AItem[0].Name <> 'groups' then
    SplitManager.LoadGroupsFromXML(AItem);
  if AItem[AItem.Count - 1].Name = 'groups' then
  begin
    WithGroup := True;
    FGroupManager.LoadGroupsFromXML(AItem);
  end;
end;

procedure TfcxBaseUniqueValues.Recapacity;
begin
  Capacity := FCount;
  FSplitManager.Recapacity;
end;

procedure TfcxBaseUniqueValues.SaveGroupsToXML(AItem: TfcxXMLItem;
  AOnlyForNotSavedFields: boolean);
var
  AItem1: TfcxXMLItem;
begin
// For Splits
  AItem1 := AItem.Add;
  AItem1.Name := 'Field';
  AItem1.Prop['name'] := TfcxCommonField(FfcField).CubeFieldName;
  SplitManager.SaveGroupsToXML(AItem1, AOnlyForNotSavedFields);
  if FWithGroup and (FGroupManager.GroupCount > 0) then
  begin
    if not AOnlyForNotSavedFields or not TfcxCommonField(FfcField).Saved then
      FGroupManager.SaveGroupsToXML(AItem1);
  end;
  if AItem1.Count = 0 then
    AItem1.Free;
end;

procedure TfcxBaseUniqueValues.SaveToStream(ACubeStream: TStream);
var
  i: integer;
  ABool: Boolean;
  AIntArray: PfcxIntegerArray;
begin
  ACubeStream.Write(Count, SizeOf(integer));
  ACubeStream.Write(HasNull, SizeOf(Boolean));
//  if ACommonField.FUniqueValues.HasNull then
  for i := 0 to Count - 1 do
    FTypeProcessor.SaveToStream(FList[i], ACubeStream);

  if Self is TfcxAttributeUniqueValues then
  begin
    GetMem(AIntArray, SizeOf(Integer) * TfcxAttributeUniqueValues(Self).FBaseUniqueValues.Count);
    for i := 0 to TfcxAttributeUniqueValues(Self).FBaseUniqueValues.Count - 1 do
      AIntArray[i] := TfcxAttributeUniqueValues(Self).FBaseUniqueValues.SplitManager.AttributesManager.FListUVofPaths[TfcxCommonField(fcField).AttributeIndex][i].Index;
    ACubeStream.Write(AIntArray[0], SizeOf(Integer) * TfcxAttributeUniqueValues(Self).FBaseUniqueValues.Count);
    FreeMem(AIntArray);
  end;
// Custom Caption
  ACubeStream.Write(WithCustomCaption, SizeOf(Boolean));
  if WithCustomCaption then
    FCustomCaptionManager.SaveToStream(ACubeStream);
  ABool := False;
  if FWithGroup and (FGroupManager.GroupCount > 0) then
  begin
    ACubeStream.Write(FWithGroup, SizeOf(Boolean));
    FGroupManager.SaveToStream(ACubeStream)
  end
  else
    ACubeStream.Write(ABool, SizeOf(Boolean));
end;

procedure TfcxBaseUniqueValues.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;
    ReallocMem(FList, FCapacity * fcPointerSize);
    if FIndexList <> nil then
      ReallocMem(FIndexList, FCapacity * fcPointerSize);
    if not FLoading then
    begin
      if WithGroup then
        FGroupManager.SetCapacity;
      FSplitManager.SetCapacity(False);
      if WithCustomCaption then
        FCustomCaptionManager.SetCapacity;
    end;
  end;
end;

procedure TfcxBaseUniqueValues.SetCaption(AUVIndex: Integer;
  const Value: TfcxString);
begin
  if not WithCustomCaption then
    RaisefcError(exfcCustomCaptionNotSupported, []);
  if GetCaption(AUVIndex) = Value then
    exit;
  FCustomCaptionManager.SetCaption(AUVIndex, Value);
end;

procedure TfcxBaseUniqueValues.SetCaptionSourceAttribute(
  const Value: TfcxString);
var
  AIndex: integer;
begin
  if FCaptionSourceAttribute <> Value then
  begin
    if Value = '' then
    begin
      FCaptionSourceAttributeIndex := -1;
      FCaptionSourceAttribute := Value;
    end
    else
    begin
      AIndex := FSplitManager.FAttributesManager.FindAttribute(Value);
      if AIndex <> -1 then
      begin
        FCaptionSourceAttributeIndex := AIndex;
        FCaptionSourceAttribute := Value;
      end
    end
  end;
  if FCaptionSourceAttributeIndex = -1 then
    if FWithCustomCaption then
      GetCaption := GetCaptionCustom
    else
      GetCaption := GetCaptionSimple
  else
    if FWithCustomCaption then
      GetCaption := GetCaptionCustomAttribute
    else
      GetCaption := GetCaptionAttribute;
{ TODO -cНеобходимо : Не забыть про обработку изменений в атрибутах.}
end;

procedure TfcxBaseUniqueValues.SetCount(const Value: Integer);
begin
  if Value > FCapacity then
    if TfcxCommonField(FfcField).Cube.CubeState = fcst_AppendingData then
      SetCapacity(Max(Value, FCapacity + 1000))
    else
      SetCapacity(Value);
  FCount := Value;
end;

procedure TfcxBaseUniqueValues.SetIndex;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do
    FList[i].Index := i;
  if not FLoadingFromStream then
    FSplitManager.SetIndexMainReference;
end;

procedure TfcxBaseUniqueValues.SetLoading(const Value: boolean);
begin
  if FLoading <> Value then
  begin
    if Value then
    begin
      FUVHash.CreateHash;
      SplitManager.AttributesManager.StartLoadMainReference;
    end
    else
    begin
      SplitManager.AttributesManager.StopLoadMainReference;
      FUVHash.ClearHash;
    end
  end;
  FLoading := Value;
end;

procedure TfcxBaseUniqueValues.SetNullCaption(const Value: TfcxString);
begin
  TfcxCommonField(FfcField).NullStr := Value;
end;

procedure TfcxBaseUniqueValues.SetOrderSourceAttribute(
  const Value: TfcxString);
var
  AIndex: integer;
begin
  if FOrderSourceAttribute <> Value then
  begin
    if Value = '' then
    begin
      FOrderSourceAttributeIndex := -1;
      FOrderSourceAttribute := Value;
    end
    else
    begin
      AIndex := FSplitManager.FAttributesManager.FindAttribute(Value);
      if AIndex <> -1 then
      begin
        FOrderSourceAttributeIndex := AIndex;
        FOrderSourceAttribute := Value;
      end
    end
  end;
{ TODO -cНеобходимо : Не забыть про обработку изменений в атрибутах.}
end;

procedure TfcxBaseUniqueValues.SetWithCustomCaption(const Value: Boolean);
begin
  if WithCustomCaption = Value then
    Exit;
  if Value then
    FCustomCaptionManager := TfcxCustomCaptionManager.Create(Self)
  else
    FreeAndNil(FCustomCaptionManager);
  FWithCustomCaption := Value;
  if FWithCustomCaption then
    GetCaption := GetCaptionCustom
  else
    GetCaption := GetCaptionSimple;
  if FCaptionSourceAttributeIndex = -1 then
    if FWithCustomCaption then
      GetCaption := GetCaptionCustom
    else
      GetCaption := GetCaptionSimple
  else
    if FWithCustomCaption then
      GetCaption := GetCaptionCustomAttribute
    else
      GetCaption := GetCaptionAttribute;
end;

procedure TfcxBaseUniqueValues.SetWithGroup(const Value: Boolean);
begin
  if FWithGroup = Value then
    Exit;
  if Value then
    FGroupManager := TfcxUVSGroupManager.Create(Self)
  else
    FreeAndNil(FGroupManager);
  FWithGroup := Value;
end;


function TfcxBaseUniqueValues.Sort(AFirstStep: boolean): boolean;
var
  AExtraList: PfcxArrPointerArray;
  AExtraCount: integer;
  AOrderIndex, i: integer;
begin
  Result := False;
  if (FList = nil) or (FCount = 0) then
    exit;
  if AFirstStep then
  begin
    FreeMem(FIndexList);
    FIndexList := nil;
// первичная сортировка. ключ - само значение
    if not FLoading then
      RaisefcError(exfcOnlyForLoading, []);
    TfcxCommonField(FfcField).Cube.TimeStatStart.SortTime := fcxGetTickCount;
    AOrderIndex := -1;
    AExtraList := nil;
    AExtraCount := 0;
    for i := 0 to FSplitManager.AttributesCount - 1 do
    begin
      if FSplitManager.FAttributesManager.Attribute[i].FromMasterSource then
      begin
        Inc(AExtraCount);
        ReallocMem(AExtraList, fcPointerSize * AExtraCount);
        AExtraList[AExtraCount - 1] := PfcxPointerArray(FSplitManager.FAttributesManager.FListUVofPaths[i]);
      end;
    end;
    SortUniqueList(0, FCount - 1, FList, FTypeProcessor.Compare, FNullValue, AExtraList, AExtraCount, AOrderIndex);
    FreeMem(AExtraList);
    with TfcxCommonField(FfcField).Cube do
      TimeStat.SortTime := TimeStat.SortTime + fcxGetTickCount - TimeStatStart.SortTime;
(* ?? выяснить необходимость !!
    if not (Self is TfcxAttributeUniqueValues) then
      FSplitManager.SortMainReference
    else
    if not TfcxAttributeUniqueValues(Self).FromMasterSource then
*)
      FSplitManager.SortMainReference;
  end
  else
  begin
    if FOrderSourceAttribute <> '' then
    begin
// финальная сортировка. ключ - значение атрибута
// save system index
      TfcxCommonField(FfcField).Cube.TimeStatStart.SortTime := fcxGetTickCount;
      AExtraList := nil;
      AOrderIndex := FOrderSourceAttributeIndex;
      AExtraCount := FSplitManager.AttributesCount;
      ReallocMem(AExtraList, fcPointerSize * AExtraCount);
      for i := 0 to FSplitManager.AttributesCount - 1 do
      begin
        AExtraList[i] := PfcxPointerArray(FSplitManager.FAttributesManager.FListUVofPaths[i]);
      end;
{// а надо ли сортировать?
      if FSplitManager.UseDateSplit then
        for i := 0 to FSplitManager.FDatePathsManager.PathsCount - 1 do
          if FSplitManager.FDatePathsManager.FListUValues[i] <> nil then
          begin
            inc(AExtraCount);
            ReallocMem(AExtraList, fcPointerSize * AExtraCount);
            AExtraList[AExtraCount - 1] := PfcxPointerArray(FSplitManager.FDatePathsManager.FListUVofPaths[i]);
          end;
      if FSplitManager.UseTimeSplit then
        for i := 0 to FSplitManager.FTimePathsManager.PathsCount - 1 do
          if FSplitManager.FTimePathsManager.FListUValues[i] <> nil then
          begin
            inc(AExtraCount);
            ReallocMem(AExtraList, fcPointerSize * AExtraCount);
            AExtraList[AExtraCount - 1] := PfcxPointerArray(FSplitManager.FTimePathsManager.FListUVofPaths[i]);
          end;
}
      SortUniqueList(0, FCount - 1, FList, FTypeProcessor.Compare, FNullValue, AExtraList, AExtraCount, AOrderIndex);
      FreeMem(AExtraList);
      Result := True;
      with TfcxCommonField(FfcField).Cube do
        TimeStat.SortTime := TimeStat.SortTime + fcxGetTickCount - TimeStatStart.SortTime;
    end;
  end;
end;

function TfcxBaseUniqueValues.VarValueToPointer(
  AVariant: Variant): pointer;
begin
  Result := FTypeProcessor.ToPointer(AVariant);
end;

procedure TfcxBaseUniqueValues.SetCaptionAndGroupCapacity;
begin
  if WithCustomCaption then
    FCustomCaptionManager.SetCapacity(True);
  if WithGroup then
    FGroupManager.SetCapacity(True);
end;

procedure TfcxBaseUniqueValues.RemoveLinksToDataSource;
begin
//
end;

procedure TfcxBaseUniqueValues.AppendFromStream(ACubeStream: TStream);
var
  i: integer;
  AHasNull, ABoolValue: boolean;
  AIntArray: PfcxIntegerArray;
  AValue: Pointer;
begin
//  FLoadingFromStream := True;
  ACubeStream.Read(FTempCount, SizeOf(integer));
  ReallocMem(FTempList, FTempCount * fcPointerSize);
  SetCapacity(Capacity + FTempCount); // to load faster
//  Count := ACount;
  ACubeStream.Read(AHasNull, SizeOf(Boolean));
  for i := 0 to FTempCount - 1 do
  begin
    AValue := FTypeProcessor.LoadValueFromStream(ACubeStream);
    if (i = 0) and AHasNull then
    begin
      AddNewValue(nil, FTempList[i]);
      FreeMem(AValue);
    end
    else
      AddNewValue(AValue, FTempList[i]);
  end;

  if TfcxCommonField(fcField).MasterField <> nil then
  begin

    GetMem(AIntArray, SizeOf(Integer) * TfcxCommonField(fcField).MasterField.UniqueValues.FTempCount);
    ACubeStream.Read(AIntArray[0], SizeOf(Integer) * TfcxCommonField(fcField).MasterField.UniqueValues.FTempCount);

    for i := 0 to TfcxCommonField(fcField).MasterField.UniqueValues.FTempCount - 1 do
      TfcxCommonField(fcField).MasterField.UniqueValues.SplitManager.AttributesManager.FListUVofPaths[TfcxCommonField(fcField).AttributeIndex][TfcxCommonField(fcField).MasterField.UniqueValues.FTempList[i].Index] := FTempList[AIntArray[i]];

    FreeMem(AIntArray);
  end;

// Custom Caption
  ACubeStream.Read(ABoolValue, SizeOf(Boolean));
  if ABoolValue then
  begin
    WithCustomCaption := ABoolValue;
    FCustomCaptionManager.AppendFromStream(ACubeStream);
  end;
//  if Self is TfcxBaseUniqueValues then
//  SetIndex;
// Groups
  ACubeStream.Read(ABoolValue, SizeOf(Boolean));
  if ABoolValue then
  begin
    WithGroup := ABoolValue;
    FGroupManager.AppendFromStream(ACubeStream);
  end;
//  FLoadingFromStream := False;
end;

procedure TfcxBaseUniqueValues.AppendAttributes(
  AOnlyForNotSavedFields: boolean);
var
  i, j: integer;
  AExists: boolean;
  ADataSets: array of TfcxDataSet;
  AAttributeManagerArr: Array of Array of Integer;
//  AArrCustomSplitManager: array of TfcxCustomSplitManager;
begin
  if FLoading then
    RaisefcError(exfcNotSupportedDuringLoading, []);
  if not (FSplitManager.FUseDateSplit or FSplitManager.FUseTimeSplit or (FSplitManager.AttributesCount > 0)) then
    Exit;
  SetLength(ADataSets, 0);
  SetLength(AAttributeManagerArr, 0);
  if not AOnlyForNotSavedFields then
    for i := 0 to FSplitManager.AttributesCount - 1 do
    begin
      if (FSplitManager.AttributesManager[i].AttributeType = fcxsft_Reference) and
         not FSplitManager.AttributesManager[i].FromMasterSource then
      begin
        AExists := False;
        for j := 0 to High(ADataSets) do
          if ADataSets[j] = FSplitManager.AttributesManager[i].FDataField.DataSet then
          begin
            SetLength(AAttributeManagerArr[High(AAttributeManagerArr)], Length(AAttributeManagerArr[High(AAttributeManagerArr)]) + 1);
            AAttributeManagerArr[High(AAttributeManagerArr)][High(AAttributeManagerArr[High(AAttributeManagerArr)])] := i;
            AExists := True;
            Break;
          end;
        if not AExists then
        begin
          SetLength(ADataSets, Length(ADataSets) + 1);
          SetLength(AAttributeManagerArr, Length(AAttributeManagerArr) + 1);
          SetLength(AAttributeManagerArr[High(AAttributeManagerArr)], 1);
          ADataSets[High(ADataSets)] := FSplitManager.AttributesManager[i].FDataField.DataSet;
          AAttributeManagerArr[High(AAttributeManagerArr)][0] := i;
        end
      end
    end;

//  FSplitManager.SetCapacity(True, AOnlyForNotSavedFields);
  if FAddedUValues then
  begin
//    FSplitManager.StartLoad(False);
    if not AOnlyForNotSavedFields then
      for i := 0 to High(ADataSets) do
      begin
// загружаем Reference атрибуты
        FSplitManager.FAttributesManager.AppendReferenceSource(FfcField, ADataSets[i], AAttributeManagerArr[i]);
      end;
// заполняем остальные атрибуты
{
    if FSplitManager.FUseDateSplit or FSplitManager.FUseTimeSplit or (FSplitManager.AttributesCount > 0) then
      for i := 0 to Count - 1 do
        FSplitManager.AddUValue(flist[i], i, False);
    if FSplitManager.FUseDateSplit then
      FSplitManager.DatePathsManager.CreatePathUVsArray;
    if FSplitManager.FUseTimeSplit then
      FSplitManager.TimePathsManager.CreatePathUVsArray;

    FSplitManager.Sort(True, False);
    FSplitManager.SetIndex(False);

    FSplitManager.StopLoad(False);
    FSplitManager.SetCaptionAndGroupCapacity(False);
    FSplitManager.LoadAttributes(AOnlyForNotSavedFields);
}
    FSplitManager.Sort(False, False);

{
    if FSplitManager.AttributesCount > 0 then
    begin
// CalculateAfterAll
      FSplitManager.StartLoad(True);
      for i := 0 to Count - 1 do
        FSplitManager.AddUValue(flist[i], i, True);
      FSplitManager.Sort(True, True);
      FSplitManager.SetIndex(True);
      FSplitManager.StopLoad(True);
      FSplitManager.SetCaptionAndGroupCapacity(True);
      FSplitManager.Sort(False, True);
    end;
}
  end;
end;

procedure TfcxBaseUniqueValues.SortIndexList;
begin
  ReallocMem(FIndexList, FCount * fcPointerSize);
  System.Move(FList^, FIndexList^, FCount * fcPointerSize);
  SortUniqueList(0, FCount - 1, FIndexList, FTypeProcessor.Compare, FNullValue, Nil, 0, -1);
end;

function TfcxBaseUniqueValues.GetUVIndex(AUVVarValue: Variant): Integer;
var
  AUValue: PfcxCommonUV;
  APointer: Pointer;
begin
  if (AUVVarValue = Unassigned) or (AUVVarValue = Null) then
    APointer := nil
  else
    APointer := VarValueToPointer(AUVVarValue);
  if not FindUValue(APointer, AUValue, Result) then
    Result := -1;
  FreeMem(APointer);
end;

{ TfcxUVSGroupManager }

procedure TfcxUVSGroupManager.AddUVInGroup(AUVIndex, AGroupIndex: integer);
begin
  AddInGroup(FUniqueValues.FList[AUVIndex], FListGV[AGroupIndex]);
end;

procedure TfcxUVSGroupManager.AddUVValueInGroup(AUVValue: Variant;
  AGroupIndex: integer);
var
  AUVIndex: integer;
begin
  AUVIndex := FUniqueValues.UVIndex[AUVValue];
  if AUVIndex >= 0 then
    AddUVInGroup(AUVIndex, AGroupIndex);
end;

function TfcxUVSGroupManager.GetCaptionInGroup(AGroupIndex,
  AIndexInGroup: Integer): TfcxString;
var
  AUVIndex: integer;
begin
  AUVIndex := FListGV[AGroupIndex].ListUV[AIndexInGroup].Index;
  Result := FUniqueValues.GetCaption(AUVIndex);
end;

function TfcxUVSGroupManager.GetCaptionInNonGroups(
  AIndexInGroup: Integer): TfcxString;
var
  AUVIndex: integer;
begin
  if not ExistsOther then
  begin
    AUVIndex := FOtherGV.ListUV[AIndexInGroup].Index;{ GetUVIndexOfOther(AIndexInGroup)};
    Result := FUniqueValues.GetCaption(AUVIndex);
  end
  else
    Result := '';
end;

function TfcxUVSGroupManager.GetUVIndexInGroup(AGroupIndex,
  AIndexInGroup: Integer): Integer;
begin
  Result := FListGV[AGroupIndex].ListUV[AIndexInGroup].Index;
end;

function TfcxUVSGroupManager.GetUVIndexInNonGroups(
  AIndexInGroup: Integer): Integer;
begin
  if not ExistsOther then
    Result := FOtherGV.ListUV[AIndexInGroup].Index
  else
    Result := -1;
end;

procedure TfcxUVSGroupManager.InitOtherGroup;
var
  i: integer;
begin
  FOtherGV := NewGV(FOtherCaption, True);
// add all values
  FOtherGV.CountUV := FUniqueValues.Count;
  ReallocMem(FOtherGV.ListUV, FOtherGV.CountUV * fcPointerSize);
  for i := 0 to FOtherGV.CountUV - 1 do
  begin
    FOtherGV.ListUV[i] := FUniqueValues.UValue[i];
    FGroupParentList[FOtherGV.ListUV[i].Index] := FOtherGV;
  end;
end;

procedure TfcxUVSGroupManager.RemoveUVFromGroup(AUVIndex: integer);
begin
  RemoveFromGroup(FUniqueValues.FList[AUVIndex]);
end;

procedure TfcxUVSGroupManager.StartChange;
begin
  TfcxHackCube(TfcxCommonField(FUniqueValues.FfcField).Cube).StartChange;
end;

procedure TfcxUVSGroupManager.StopChange(AChanges: TfcxChangesInCube;
  AIndex: integer);
begin
  if AIndex = -2 then
    AIndex := TfcxCommonField(FUniqueValues.FfcField).Index;
  TfcxHackCube(TfcxCommonField(FUniqueValues.FfcField).Cube).StopChange(AChanges, AIndex);
end;

function TfcxUVSGroupManager.UVSCapacity: integer;
begin
  Result := FUniqueValues.Capacity;
end;

function TfcxUVSGroupManager.UVSCount: integer;
begin
  Result := FUniqueValues.Count;
end;

{ TfcxDatePathGroupManager }

procedure TfcxDatePathGroupManager.AddUVInGroup(AUVIndex,
  AGroupIndex: integer);
begin
  AddInGroup(pointer(FDatePathProcessor.ValueAtIndex[AUVIndex]), FListGV[AGroupIndex]);
end;

procedure TfcxDatePathGroupManager.AddUVValueInGroup(AUVValue: Variant;
  AGroupIndex: integer);
var
  AUVIndex: integer;
begin
  AUVIndex := FDatePathProcessor.GetUVIndex(AUVValue);
  if AUVIndex >= 0 then
    AddUVInGroup(AUVIndex, AGroupIndex);
end;

constructor TfcxDatePathGroupManager.Create(
  AUniqueValues: TfcxBaseUniqueValues; ADatePath: TfcxDateType);
begin
  FDatePath := ADatePath;
  FDatePathProcessor := TfcxCommonDatePathField(AUniqueValues.SplitManager.DatePathsManager.DatePathField[FDatePath]).DatePathProcessor;
  inherited Create(AUniqueValues);
end;

function TfcxDatePathGroupManager.GetCaptionInGroup(AGroupIndex,
  AIndexInGroup: Integer): TfcxString;
var
  AUVIndex: integer;
begin
  AUVIndex := Integer(FDatePathProcessor.IndexAtValue[integer(FListGV[AGroupIndex].ListUV[AIndexInGroup])]);
  Result := FDatePathProcessor.CaptionAtIndex[AUVIndex];
end;

function TfcxDatePathGroupManager.GetCaptionInNonGroups(
  AIndexInGroup: Integer): TfcxString;
var
  AUVIndex: integer;
begin
  if not ExistsOther then
  begin
    AUVIndex := Integer(FDatePathProcessor.IndexAtValue[integer(FOtherGV.ListUV[AIndexInGroup])]);
    Result := FDatePathProcessor.CaptionAtIndex[AUVIndex];
  end
  else
    Result := '';
end;

function TfcxDatePathGroupManager.GetIndexFromUV(
  AUValue: PfcxCommonUV): integer;
begin
  Result := Integer(FDatePathProcessor.IndexAtValue[integer(AUValue)]);
end;

function TfcxDatePathGroupManager.GetUVIndexInGroup(AGroupIndex,
  AIndexInGroup: Integer): Integer;
begin
  Result := Integer(FDatePathProcessor.IndexAtValue[integer(FListGV[AGroupIndex].ListUV[AIndexInGroup])]);
end;

function TfcxDatePathGroupManager.GetUVIndexInNonGroups(
  AIndexInGroup: Integer): Integer;
begin
  if not ExistsOther then
    Result := Integer(FDatePathProcessor.IndexAtValue[integer(FOtherGV.ListUV[AIndexInGroup])])
  else
    Result := -1;
end;

procedure TfcxDatePathGroupManager.InitOtherGroup;
var
  i: integer;
begin
  FOtherGV := NewGV(FOtherCaption, True);
// add all values
  FOtherGV.CountUV := FDatePathProcessor.CountUV;
  ReallocMem(FOtherGV.ListUV, FOtherGV.CountUV * fcPointerSize);
  for i := 0 to FOtherGV.CountUV - 1 do
  begin
    FOtherGV.ListUV[i] := pointer(FDatePathProcessor.ValueAtIndex[i]);
    FGroupParentList[i] := FOtherGV;
  end;
end;

procedure TfcxDatePathGroupManager.RemoveUVFromGroup(AUVIndex: integer);
begin
  RemoveFromGroup(pointer(FDatePathProcessor.ValueAtIndex[AUVIndex]));
end;

procedure TfcxDatePathGroupManager.StartChange;
begin
  TfcxHackCube(TfcxCommonField(FUniqueValues.FfcField).Cube).StartChange;
end;

procedure TfcxDatePathGroupManager.StopChange(AChanges: TfcxChangesInCube;
  AIndex: integer);
begin
// ??? передать индекс поля сплита?
  if AIndex = -2 then
    AIndex := TfcxCommonField(FUniqueValues.FfcField).Index;
  TfcxHackCube(TfcxCommonField(FUniqueValues.FfcField).Cube).StopChange(AChanges, AIndex);
end;

function TfcxDatePathGroupManager.UVSCapacity: integer;
begin
  Result := FDatePathProcessor.CountUV;
end;

function TfcxDatePathGroupManager.UVSCount: integer;
begin
  Result := FDatePathProcessor.CountUV;
end;

{ TfcxDatePathUniqueValues }

constructor TfcxDatePathUniqueValues.Create(AfcField: TObject;
  ADateType: TfcxDateType);
begin
  inherited Create(AfcField);
  FDateType := ADateType;
  FDataTypeProcessor := cfcDatePathProcessorMap[FDateType].Create(AfcField);
end;

function TfcxDatePathUniqueValues.CreateGroupManager: TfcxCommonGroupManager;
begin
  Result := TfcxDatePathGroupManager.Create(TfcxCommonDatePathField(FfcField).MasterField.UniqueValues, FDateType);
end;

function TfcxDatePathUniqueValues.GetDatePathProcessor: TfcxCommonDatePathDTP;
begin
  Result := TfcxCommonDatePathDTP(FDataTypeProcessor);
end;

{ TfcxTimePathUniqueValues }

constructor TfcxTimePathUniqueValues.Create(AfcField: TObject;
  ATimeType: TfcxTimeType);
begin
  inherited Create(AfcField);
  FTimeType := ATimeType;
  FDataTypeProcessor := cfcTimePathProcessorMap[FTimeType].Create(AfcField);
end;

function TfcxTimePathUniqueValues.CreateGroupManager: TfcxCommonGroupManager;
begin
  Result := TfcxTimePathGroupManager.Create(TfcxCommonTimePathField(FfcField).MasterField.UniqueValues, FTimeType);
end;

function TfcxTimePathUniqueValues.GetTimePathProcessor: TfcxCommonTimePathDTP;
begin
  Result := TfcxCommonTimePathDTP(FDataTypeProcessor);
end;

{ TfcxTimePathGroupManager }

procedure TfcxTimePathGroupManager.AddUVInGroup(AUVIndex,
  AGroupIndex: integer);
begin
  AddInGroup(pointer(FTimePathProcessor.ValueAtIndex[AUVIndex]), FListGV[AGroupIndex]);
end;

procedure TfcxTimePathGroupManager.AddUVValueInGroup(AUVValue: Variant;
  AGroupIndex: integer);
var
  AUVIndex: integer;
begin
  AUVIndex := FTimePathProcessor.GetUVIndex(AUVValue);
  if AUVIndex >= 0 then
    AddUVInGroup(AUVIndex, AGroupIndex);
end;

constructor TfcxTimePathGroupManager.Create(
  AUniqueValues: TfcxBaseUniqueValues; ATimePath: TfcxTimeType);
begin
  FTimePath := ATimePath;
  FTimePathProcessor := TfcxCommonTimePathField(AUniqueValues.SplitManager.TimePathsManager.TimePathField[FTimePath]).TimePathProcessor;
  inherited Create(AUniqueValues);
end;

function TfcxTimePathGroupManager.GetCaptionInGroup(AGroupIndex,
  AIndexInGroup: Integer): TfcxString;
var
  AUVIndex: integer;
begin
  AUVIndex := Integer(FTimePathProcessor.IndexAtValue[integer(FListGV[AGroupIndex].ListUV[AIndexInGroup])]);
  Result := FTimePathProcessor.CaptionAtIndex[AUVIndex];
end;

function TfcxTimePathGroupManager.GetCaptionInNonGroups(
  AIndexInGroup: Integer): TfcxString;
var
  AUVIndex: integer;
begin
  if not ExistsOther then
  begin
    AUVIndex := Integer(FTimePathProcessor.IndexAtValue[integer(FOtherGV.ListUV[AIndexInGroup])]);
    Result := FTimePathProcessor.CaptionAtIndex[AUVIndex];
  end
  else
    Result := '';
end;

function TfcxTimePathGroupManager.GetIndexFromUV(
  AUValue: PfcxCommonUV): integer;
begin
  Result := Integer(FTimePathProcessor.IndexAtValue[integer(AUValue)]);
end;

function TfcxTimePathGroupManager.GetUVIndexInGroup(AGroupIndex,
  AIndexInGroup: Integer): Integer;
begin
  Result := Integer(FTimePathProcessor.IndexAtValue[integer(FListGV[AGroupIndex].ListUV[AIndexInGroup])]);
end;

function TfcxTimePathGroupManager.GetUVIndexInNonGroups(
  AIndexInGroup: Integer): Integer;
begin
  if not ExistsOther then
    Result := Integer(FTimePathProcessor.IndexAtValue[integer(FOtherGV.ListUV[AIndexInGroup])])
  else
    Result := -1;
end;

procedure TfcxTimePathGroupManager.InitOtherGroup;
var
  i: integer;
begin
  FOtherGV := NewGV(FOtherCaption, True);
// add all values
  FOtherGV.CountUV := FTimePathProcessor.CountUV;
  ReallocMem(FOtherGV.ListUV, FOtherGV.CountUV * fcPointerSize);
  for i := 0 to FOtherGV.CountUV - 1 do
  begin
    FOtherGV.ListUV[i] := pointer(FTimePathProcessor.ValueAtIndex[i]);
    FGroupParentList[i] := FOtherGV;
  end;
end;

procedure TfcxTimePathGroupManager.RemoveUVFromGroup(AUVIndex: integer);
begin
  RemoveFromGroup(pointer(FTimePathProcessor.ValueAtIndex[AUVIndex]));
end;

procedure TfcxTimePathGroupManager.StartChange;
begin
  TfcxHackCube(TfcxCommonField(FUniqueValues.FfcField).Cube).StartChange;
end;

procedure TfcxTimePathGroupManager.StopChange(AChanges: TfcxChangesInCube;
  AIndex: integer);
begin
// ??? передать индекс поля сплита?
  if AIndex = -2 then
    AIndex := TfcxCommonField(FUniqueValues.FfcField).Index;
  TfcxHackCube(TfcxCommonField(FUniqueValues.FfcField).Cube).StopChange(AChanges, AIndex);
end;

function TfcxTimePathGroupManager.UVSCapacity: integer;
begin
  Result := FTimePathProcessor.CountUV;
end;

function TfcxTimePathGroupManager.UVSCount: integer;
begin
  Result := FTimePathProcessor.CountUV;
end;

{ TfcxStandardPathUniqueValues }

constructor TfcxStandardPathUniqueValues.Create(AfcField: TObject);
begin
  FfcField := AfcField;
  FDataTypeProcessor := nil;
end;

destructor TfcxStandardPathUniqueValues.Destroy;
begin
  FreeAndNil(FGroupManager);
  FreeAndNil(FDataTypeProcessor);
  inherited;
end;

function TfcxStandardPathUniqueValues.GetGroupIndexOfUV(
  AUVIndex: Integer): Integer;
begin
  if not FWithGroup then
    Result := -1
  else
    if FGroupManager.FGroupParentList[AUVIndex].IsOther and not FGroupManager.ExistsOther then
      Result := -1
    else
      Result := FGroupManager.FGroupParentList[AUVIndex].Index
end;

function TfcxStandardPathUniqueValues.GetGroupManager: TfcxCommonGroupManager;
begin
  if not FWithGroup then
    RaisefcError(exfcGroupNotSupported, []);
  Result := FGroupManager;
end;

function TfcxStandardPathUniqueValues.GetUVIndex(
  AUVVarValue: Variant): Integer;
begin
  Result := DataTypeProcessor.GetUVIndex(AUVVarValue);
end;

function TfcxStandardPathUniqueValues.GetUVInGroup(
  AUVIndex: Integer): Boolean;
begin
  if not FWithGroup then
    Result := False
  else
    Result := not FGroupManager.FGroupParentList[AUVIndex].IsOther or FGroupManager.ExistsOther;
end;

procedure TfcxStandardPathUniqueValues.SetWithGroup(const Value: Boolean);
begin
  if FWithGroup = Value then
    Exit;
  if Value then
    FGroupManager := CreateGroupManager
  else
    FreeAndNil(FGroupManager);
  FWithGroup := Value;
end;

{ TfcxAttributesManager }

function TfcxAttributesManager.AddAttribute(
  ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
var
  AIndex: integer;
begin
{ TODO -cНеобходимо :  Нужно добавить проверку на совпадение имени части сплита !!!}
  AIndex := FindAttribute(ASourceField.DataField.CubeFieldName);
  if AIndex = -1 then
  begin
    Inc(FAttributesCount);
    ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
    ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
    GetMem(FListUVofPaths[FAttributesCount - 1], FBaseUniqueValues.FCapacity * fcPointerSize);
    if FListUVofPaths[FAttributesCount - 1] <> nil then
      FillChar(FListUVofPaths[FAttributesCount - 1]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
// Create TfcxUniqueValues for Custom Attribute
    FListUValues[FAttributesCount - 1] := TfcxAttributeUniqueValues(TfcxCommonUVField(TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.AddAttributeField(ASourceField, FAttributesCount - 1, ASourceField.SourceFieldType)]).UniqueValues);
(*
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetUniqueValues(FListUValues[FAttributesCount - 1]);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetIndex(FAttributesCount - 1);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).AttributeType := fcxsft_Custom;
*)
    Result := FListUValues[FAttributesCount - 1];
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end
  else
    Result := FListUValues[AIndex];
end;

(*
function TfcxAttributesManager.AddReferenceAttribute(
  ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
var
  AIndex: integer;
begin
  AIndex := FindAttribute(ASourceField.DataField.CubeFieldName);
  if AIndex = -1 then
  begin
    Inc(FAttributesCount);
    ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
    ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
    GetMem(FListUVofPaths[FAttributesCount - 1], FBaseUniqueValues.FCapacity * fcPointerSize);
    if FListUVofPaths[FAttributesCount - 1] <> nil then
      FillChar(FListUVofPaths[FAttributesCount - 1]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
// Create TfcxUniqueValues for Reference Path
    FListUValues[FAttributesCount - 1] := TfcxAttributeUniqueValues(TfcxCommonUVField(TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.AddSplitField(ASourceField, FAttributesCount - 1, fcxsft_Reference)]).UniqueValues);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetUniqueValues(FListUValues[FAttributesCount - 1]);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetIndex(FAttributesCount - 1);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).AttributeType := fcxsft_Reference;
    Result := FListUValues[FAttributesCount - 1];
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end
  else
    Result := FListUValues[AIndex];
end;
*)

procedure TfcxAttributesManager.AddAttributeSaved(AField: Pointer);
begin
  Inc(FAttributesCount);
  ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
  ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
  FListUVofPaths[FAttributesCount - 1] := nil;
(*
  GetMem(FListUVofPaths[FAttributesCount - 1], FBaseUniqueValues.FCapacity * fcPointerSize);
  if FListUVofPaths[FAttributesCount - 1] <> nil then
    FillChar(FListUVofPaths[FAttributesCount - 1]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
*)
  FListUValues[FAttributesCount - 1] := TfcxAttributeUniqueValues(TfcxCommonUVField(AField).UniqueValues);
//  TfcxAttributeStreamField(AField).SetIndex(FAttributesCount - 1);
//  TfcxAttributeStreamField(AField).AttributeType := fcxsft_Reference;
  TfcxAttributeField(AField).AttributeIndex := FAttributesCount - 1;
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
end;

procedure TfcxAttributesManager.AddUValue(AUValue: PfcxCommonUV;
  AUVIndex: integer; AAttributeUniqueValues: TfcxAttributeUniqueValues);
var
  i: integer;
begin
  if FBaseUniqueValues.FHasNull and (AUValue = FBaseUniqueValues.FNullValue) then
    AddUValueNull(AUVIndex, AAttributeUniqueValues)
  else
    for i := 0 to FAttributesCount - 1 do
      if Attribute[i] = AAttributeUniqueValues then
      begin
        FListUVofPaths[i][AUVIndex] := FListUValues[i].AddAttributeFrom(AUValue);
        exit;
      end
end;

procedure TfcxAttributesManager.AddUValueNull(AUVIndex: integer;
  AAttributeUniqueValues: TfcxAttributeUniqueValues);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i] = AAttributeUniqueValues then
    begin
      FListUVofPaths[i][AUVIndex] := FListUValues[i].AddNull;
      exit;
    end
end;

procedure TfcxAttributesManager.AddUValueMainReference(AUValue: PfcxCommonUV;
  AUVIndex: integer);
var
  i: integer;
begin
  if FBaseUniqueValues.FHasNull and (AUValue = FBaseUniqueValues.FNullValue) then
    AddUValueNullMainReference(AUVIndex)
  else
    for i := 0 to FAttributesCount - 1 do
      if Attribute[i].FromMasterSource then
        FListUVofPaths[i][AUVIndex] := FListUValues[i].AddAttributeFrom(AUValue);
end;

procedure TfcxAttributesManager.ChangeCapacity;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].FromMasterSource then
    begin
      ReallocMem(FListUVofPaths[i], FBaseUniqueValues.FCapacity * fcPointerSize);
      FillChar(FListUVofPaths[i][FBaseUniqueValues.Count], (FBaseUniqueValues.FCapacity - FBaseUniqueValues.Count) * fcPointerSize, 0);
    end;
end;

procedure TfcxAttributesManager.Clear;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
  begin
    FreeMem(FListUVofPaths[i]);
    FListUVofPaths[i] := nil;
//    TfcxAttributeField(FListUValues[i].fcField).SetUniqueValues(nil);
    TfcxCommonUVField(FBaseUniqueValues.FfcField).Fields.DeleteField(TfcxCommonField(FListUValues[i].fcField));
// Destroy TfcxUniqueValues
//    FreeAndNil(FListUValues[i]);
  end;
  FreeMem(FListUVofPaths);
  FListUVofPaths := nil;
  FreeMem(FListUValues);
  FListUValues := nil;
  FAttributesCount := 0;
end;

constructor TfcxAttributesManager.Create(
  AUniqueValues: TfcxBaseUniqueValues);
begin
  FAttributesCount := 0;
  FBaseUniqueValues := AUniqueValues
end;

procedure TfcxAttributesManager.Delete(AUVIndex: integer);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
  begin
//    FListUValues[i].FAttributeManager.DecOwnersCount(FListUVofPaths[i][AUVIndex].Index);
    if AUVIndex < (FBaseUniqueValues.FCount - 1) then
      System.Move(FListUVofPaths[i]^[AUVIndex + 1], FListUVofPaths[i]^[AUVIndex],
        (FBaseUniqueValues.FCount - 1 - AUVIndex) * fcPointerSize);
    ReallocMem(FListUVofPaths[i], FBaseUniqueValues.FCapacity * fcPointerSize);
  end;
end;

destructor TfcxAttributesManager.Destroy;
begin
  Clear;
  inherited;
end;

function TfcxAttributesManager.FindAttribute(
  AAttributeName: TfcxString): integer;
begin
  for Result := 0 to FAttributesCount - 1 do
    if fcStrCompare(FListUValues[Result].AttributeName, AAttributeName) = 0 then
      exit;
  Result := -1;
end;

function TfcxAttributesManager.GetAttribute(
  AAttributeIndex: integer): TfcxAttributeUniqueValues;
begin
  Result := FListUValues[AAttributeIndex];
end;

function TfcxAttributesManager.GetAttributeByName(
  AAttributeName: TfcxString): TfcxAttributeUniqueValues;
var
  AIndex: integer;
begin
  AIndex := FindAttribute(AAttributeName);
  if AIndex <> -1 then
    Result := FListUValues[AIndex]
  else
    Result := nil;
end;

function TfcxAttributesManager.GetAttributeUVIndex(AAttributeIndex,
  AMasterUVIndex: integer): Integer;
begin
  result := FListUVofPaths[AAttributeIndex, AMasterUVIndex].Index
end;

procedure TfcxAttributesManager.Insert(AUVIndex: integer);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if AUVIndex < (FBaseUniqueValues.FCount - 1) then
      System.Move(FListUVofPaths[i]^[AUVIndex], FListUVofPaths[i]^[succ(AUVIndex)],
        (FBaseUniqueValues.FCount - 1 - AUVIndex) * fcPointerSize);
end;

procedure TfcxAttributesManager.LoadReferenceSource(AfcField: TObject; ADataSet: TfcxDataSet; AAttributeManagerArr: Array of integer);
var
  RecNo: integer;
(*
  AfcField1: TfcxDataSourceField absolute AfcField;
*)
begin
//  AfcField1.LoadReferenceData(Self, ADataSet, AAttributeManagerArr);
  with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
  begin
    TimeStatStart.DBMoveTime := fcxGetTickCount;
    ADataSet.First;
    InternalOnProgressStart(fcxpFetchingData);
    RecNo := 0;
    TimeStat.DBMoveTime := TimeStat.DBMoveTime + fcxGetTickCount - TimeStatStart.DBMoveTime;
  end;
  TfcxCommonField(FBaseUniqueValues.fcField).Cube.TimeStatStart.DBMoveTime := fcxGetTickCount;
  while not ADataSet.Eof do
  begin
    with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
    begin
      InternalOnProgress(fcxpFetchingData, RecNo);
      TimeStat.DBMoveTime := TimeStat.DBMoveTime + fcxGetTickCount - TimeStatStart.DBMoveTime;
    end;
    AddReferenceValue(ADataSet, {AfcField1.SourceField.DataField, }AAttributeManagerArr);
    TfcxCommonField(FBaseUniqueValues.fcField).Cube.TimeStatStart.DBMoveTime := fcxGetTickCount;
    ADataSet.Next;
    inc(RecNo);
  end;
  with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
  begin
    InternalOnProgressStop(fcxpFetchingData);
    TimeStat.DBMoveTime := TimeStat.DBMoveTime + fcxGetTickCount - TimeStatStart.DBMoveTime;
  end;
  SetNullForUnReferenced;
end;

procedure TfcxAttributesManager.Recapacity(AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
      FListUValues[i].Recapacity;
end;

procedure TfcxAttributesManager.RemoveAttribute(
  AAttributeName: TfcxString);
var
  AAttributeIndex: integer;
begin
{ TODO -cНеобходимо : Нужно сделать поиск индекса части по названию !!! }
  AAttributeIndex := FindAttribute(AAttributeName);
  if AAttributeIndex >= 0 then
    RemoveAttribute(AAttributeIndex);
end;

procedure TfcxAttributesManager.RemoveAttribute(AAttributeIndex: integer);
var
  i: integer;
begin
  FreeMem(FListUVofPaths[AAttributeIndex]);
  if FListUValues[AAttributeIndex].fcField is TfcxAttributeField then
    TfcxAttributeField(FListUValues[AAttributeIndex].fcField).SetUniqueValues(nil)
  else
    TfcxAttributeField(FListUValues[AAttributeIndex].fcField).SetUniqueValues(nil);
  TfcxCommonUVField(FBaseUniqueValues.FfcField).Fields.DeleteField(TfcxCommonField(FListUValues[AAttributeIndex].fcField));
// Destroy TfcxUniqueValues
  FreeAndNil(FListUValues[AAttributeIndex]);
  Dec(FAttributesCount);
  if AAttributeIndex < FAttributesCount then
  begin
    System.Move(FListUVofPaths^[AAttributeIndex + 1], FListUVofPaths^[AAttributeIndex],
      (FAttributesCount - AAttributeIndex) * fcPointerSize);
    System.Move(FListUValues^[AAttributeIndex + 1], FListUValues^[AAttributeIndex],
      (FAttributesCount - AAttributeIndex) * fcPointerSize);
  end;
  ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
  ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
  for i := AAttributeIndex to FAttributesCount - 1 do
    TfcxCommonField(FListUValues[i].fcField).AttributeIndex := i;
(*
    if FListUValues[i].fcField is TfcxAttributeField then
      TfcxAttributeField(FListUValues[i].fcField).SetIndex(i)
    else
      TfcxAttributeStreamField(FListUValues[i].fcField).SetIndex(i);
*)
{ TODO -cПроверить : Не забыли ли чего? }
end;

procedure TfcxAttributesManager.SetCapacity(ASetAllToNil: boolean;
  AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  if AOnlyForNotSavedFields then
    for i := 0 to FAttributesCount - 1 do
    begin
      if TfcxCommonField(Attribute[i].fcField).Saved then
      begin
        ReallocMem(FListUVofPaths[i], FBaseUniqueValues.FCapacity * fcPointerSize);
        if ASetAllToNil and (FBaseUniqueValues.Count > 0) then
          FillChar(FListUVofPaths[i]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
      end
    end
  else
    for i := 0 to FAttributesCount - 1 do
    begin
      ReallocMem(FListUVofPaths[i], FBaseUniqueValues.FCapacity * fcPointerSize);
      if ASetAllToNil and (FBaseUniqueValues.Count > 0) and
        ((Attribute[i].AttributeType in [fcxsft_Custom, fcxsft_Date, fcxsft_Time]) or
        (not Attribute[i].FromMasterSource and not AOnlyForNotSavedFields)) then
        FillChar(FListUVofPaths[i]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
    end;
end;

procedure TfcxAttributesManager.SetIndex(ACalculateAfterAll: Boolean;
  AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if not Attribute[i].FromMasterSource then
      if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
        if not (ACalculateAfterAll xor Attribute[i].CalculateAfterAll) then
          FListUValues[i].SetIndex;
end;

procedure TfcxAttributesManager.SetNullForUnReferenced;
var
  AUVIndex, i: integer;
begin
// for UV without reference we need to add null child
  for AUVIndex := 0 to FBaseUniqueValues.Count - 1 do
  begin
    for i := 0 to FAttributesCount - 1 do
      if FListUVofPaths[i][AUVIndex] = nil then
      begin
        if FListUValues[i].HasNull then
          FListUVofPaths[i][AUVIndex] := FListUValues[i].FNullValue
        else
          FListUVofPaths[i][AUVIndex] := FListUValues[i].AddNull;
      end;
  end;
end;

procedure TfcxAttributesManager.Sort(AFirstStep: boolean; ACalculateAfterAll: Boolean;
  AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
  ANeedSetIndex: Boolean;
begin
  for i := 0 to FAttributesCount - 1 do
    if not Attribute[i].FromMasterSource then
      if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
        if not (ACalculateAfterAll xor Attribute[i].CalculateAfterAll) then
          with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
          begin
            ANeedSetIndex := FListUValues[i].Sort(AFirstStep);
            if ANeedSetIndex and not AFirstStep then
              FListUValues[i].SetIndex;
          end;
end;

procedure TfcxAttributesManager.StartLoad(ACalculateAfterAll: Boolean;
  AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if not Attribute[i].FromMasterSource then
      if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
        if not (ACalculateAfterAll xor Attribute[i].CalculateAfterAll) then
          FListUValues[i].Loading := True;
end;

procedure TfcxAttributesManager.StopLoad(ACalculateAfterAll: Boolean;
  AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if not Attribute[i].FromMasterSource then
      if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
        if not (ACalculateAfterAll xor Attribute[i].CalculateAfterAll) then
          FListUValues[i].Loading := False;
end;

procedure TfcxAttributesManager.SortMainReference;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].FromMasterSource then
      FListUValues[i].Sort(True);
end;

procedure TfcxAttributesManager.SetIndexMainReference;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].FromMasterSource then
      FListUValues[i].SetIndex;
end;

procedure TfcxAttributesManager.StartLoadMainReference;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].FromMasterSource then
      FListUValues[i].Loading := True;
end;

procedure TfcxAttributesManager.StopLoadMainReference;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].FromMasterSource then
      FListUValues[i].Loading := False;
end;

procedure TfcxAttributesManager.AddReferenceValue(ADataSet: TfcxDataSet;
  AAttributeManagerArr: Array of integer);
var
  APointerId, APointerValue: Pointer;
  AUValue, ASplitUValue: PfcxCommonUV;
  AUVIndex, i: integer;
begin
  APointerId := nil;
  FListUValues[AAttributeManagerArr[0]].GetIDFieldAttributeValue(APointerId);
  if FBaseUniqueValues.FindUValue(APointerId, AUValue, AUVIndex) then
  begin
// Finded base value
    if FBaseUniqueValues.FIndexList <> nil then
      AUVIndex := FBaseUniqueValues.FIndexList[AUVIndex].Index;
    for i := 0 to High(AAttributeManagerArr) do
    begin
      APointerValue := nil;
      FListUValues[AAttributeManagerArr[i]].GetFieldAttributeValue(APointerValue);
      if APointerValue = nil then
        FListUVofPaths[AAttributeManagerArr[i]][AUVIndex] := FListUValues[AAttributeManagerArr[i]].AddNull
      else
      begin
        FListUValues[AAttributeManagerArr[i]].AddNewValue(APointerValue, ASplitUValue);
        FListUVofPaths[AAttributeManagerArr[i]][AUVIndex] := ASplitUValue;
      end;
    end;
  end
  else
  begin
// вариант, когда надо загрузить все значения справочника
    for i := 0 to High(AAttributeManagerArr) do
      if FListUValues[AAttributeManagerArr[i]].LoadAllValues then
      begin
        APointerValue := nil;
        FListUValues[AAttributeManagerArr[i]].GetFieldAttributeValue(APointerValue);
        if APointerValue = nil then
          FListUValues[AAttributeManagerArr[i]].AddNull
        else
          FListUValues[AAttributeManagerArr[i]].AddNewValue(APointerValue, ASplitUValue);
        end
  end;
  FreeMem(APointerId);
end;

procedure TfcxAttributesManager.AddUValueNullMainReference(
  AUVIndex: integer);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].FromMasterSource then
      FListUVofPaths[i][AUVIndex] := FListUValues[i].AddNull;
end;

procedure TfcxAttributesManager.AddUValueNonReference(
  AUValue: PfcxCommonUV; AUVIndex: integer);
var
  i: integer;
begin
  if FBaseUniqueValues.FHasNull and (AUValue = FBaseUniqueValues.FNullValue) then
    AddUValueNull(AUVIndex, False)
  else
    for i := 0 to FAttributesCount - 1 do
      if Attribute[i].AttributeType in [fcxsft_Custom, fcxsft_Date, fcxsft_Time] then
        FListUVofPaths[i][AUVIndex] := FListUValues[i].AddAttributeFrom(AUValue);
end;

procedure TfcxAttributesManager.AddUValueNullNonReference(AUVIndex: integer);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].AttributeType in [fcxsft_Custom, fcxsft_Date, fcxsft_Time] then
      FListUVofPaths[i][AUVIndex] := FListUValues[i].AddNull;
end;

procedure TfcxAttributesManager.AddUValueNull(AUVIndex: integer;
  ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if not Attribute[i].FromMasterSource then
      if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
        if not (ACalculateAfterAll xor Attribute[i].CalculateAfterAll) then
          FListUVofPaths[i][AUVIndex] := FListUValues[i].AddNull;
end;

procedure TfcxAttributesManager.AddUValue(AUValue: PfcxCommonUV; AUVIndex: integer;
  ACalculateAfterAll: Boolean; AOnlyForNotSavedFields: boolean = False);
var
  i: integer;
begin
  if FBaseUniqueValues.FHasNull and (AUValue = FBaseUniqueValues.FNullValue) then
    AddUValueNull(AUVIndex, ACalculateAfterAll, AOnlyForNotSavedFields)
  else
    for i := 0 to FAttributesCount - 1 do
      if (Attribute[i].AttributeType <> fcxsft_Reference) then
        if not AOnlyForNotSavedFields or not TfcxCommonField(Attribute[i].fcField).Saved then
          if not (ACalculateAfterAll xor Attribute[i].CalculateAfterAll) then
            FListUVofPaths[i][AUVIndex] := FListUValues[i].AddAttributeFrom(AUValue);
end;

procedure TfcxAttributesManager.AddUValueNullReference(AUVIndex: integer);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    if Attribute[i].AttributeType = fcxsft_Reference then
      FListUVofPaths[i][AUVIndex] := FListUValues[i].AddNull;
end;

procedure TfcxAttributesManager.SaveGroupsToXML(AItem: TfcxXMLItem;
  AOnlyForNotSavedFields: boolean);
var
  AItem1: TfcxXMLItem;
  i: integer;
begin
  if FAttributesCount > 0 then
  begin
    AItem1 := AItem.Add;
    AItem1.Name := 'Attributes';
    for i := 0 to FAttributesCount - 1 do
      FListUValues[i].SaveGroupsToXML(AItem1, AOnlyForNotSavedFields);
    if AItem1.Count = 0 then
      AItem1.Free;
  end;
end;

procedure TfcxAttributesManager.LoadGroupsFromXML(AItem: TfcxXMLItem);
var
  i, AAttrIndex: integer;
begin
  for i := 0 to AItem.Count - 1 do
  begin
    AAttrIndex := FindAttribute(AItem[i].Prop['name']);
    if AAttrIndex <> -1 then
      FListUValues[AAttrIndex].LoadGroupsFromXML(AItem[i]);
  end;
end;

procedure TfcxAttributesManager.ClearGroups;
var
  i: integer;
begin
  if FAttributesCount > 0 then
    for i := 0 to FAttributesCount - 1 do
      FListUValues[i].ClearGroups;
end;

procedure TfcxAttributesManager.LoadAttributes(AOnlyForNotSavedFields: boolean);
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
//    if not Attribute[i].FromMasterSource then
    FListUValues[i].LoadAttributes(AOnlyForNotSavedFields);
end;

(*
function TfcxAttributesManager.AddDateAttribute(
  ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
var
  AIndex: integer;
begin
{ TODO -cНеобходимо :  Нужно добавить проверку на совпадение имени части сплита !!!}
  AIndex := FindAttribute(ASourceField.DataField.CubeFieldName);
  if AIndex = -1 then
  begin
    Inc(FAttributesCount);
    ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
    ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
    GetMem(FListUVofPaths[FAttributesCount - 1], FBaseUniqueValues.FCapacity * fcPointerSize);
    if FListUVofPaths[FAttributesCount - 1] <> nil then
      FillChar(FListUVofPaths[FAttributesCount - 1]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
// Create TfcxUniqueValues for Custom Attribute
    FListUValues[FAttributesCount - 1] := TfcxAttributeUniqueValues(TfcxCommonUVField(TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.AddSplitField(ASourceField, FAttributesCount - 1, fcxsft_Date)]).UniqueValues);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetUniqueValues(FListUValues[FAttributesCount - 1]);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetIndex(FAttributesCount - 1);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).AttributeType := fcxsft_Date;
    Result := FListUValues[FAttributesCount - 1];
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end
  else
    Result := FListUValues[AIndex];
end;

function TfcxAttributesManager.AddTimeAttribute(
  ASourceField: TfcxSourceField): TfcxAttributeUniqueValues;
var
  AIndex: integer;
begin
{ TODO -cНеобходимо :  Нужно добавить проверку на совпадение имени части сплита !!!}
  AIndex := FindAttribute(ASourceField.DataField.CubeFieldName);
  if AIndex = -1 then
  begin
    Inc(FAttributesCount);
    ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
    ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
    GetMem(FListUVofPaths[FAttributesCount - 1], FBaseUniqueValues.FCapacity * fcPointerSize);
    if FListUVofPaths[FAttributesCount - 1] <> nil then
      FillChar(FListUVofPaths[FAttributesCount - 1]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
// Create TfcxUniqueValues for Custom Attribute
    FListUValues[FAttributesCount - 1] := TfcxAttributeUniqueValues(TfcxCommonUVField(TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.AddSplitField(ASourceField, FAttributesCount - 1, fcxsft_Time)]).UniqueValues);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetUniqueValues(FListUValues[FAttributesCount - 1]);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).SetIndex(FAttributesCount - 1);
    TfcxAttributeField(FListUValues[FAttributesCount - 1].fcField).AttributeType := fcxsft_Time;
    Result := FListUValues[FAttributesCount - 1];
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end
  else
    Result := FListUValues[AIndex];
end;
*)

procedure TfcxAttributesManager.FillNonSavedUVs;
var
  i: integer;
begin
  for i := 0 to FAttributesCount - 1 do
    FListUValues[i].FillNonSavedUVs;
end;

function TfcxAttributesManager.GetExistsDateAttribute: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FAttributesCount - 1 do
    if FListUValues[i].AttributeType = fcxsft_Date then
    begin
      Result := True;
      Exit;
    end;
end;

function TfcxAttributesManager.GetExistsTimeAttribute: boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FAttributesCount - 1 do
    if FListUValues[i].AttributeType = fcxsft_Time then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TfcxAttributesManager.SetExistsDateAttribute(
  const Value: boolean);
var
  AFieldProperties: TfcxFieldProperties;
  AAttributeUniqueValues: TfcxAttributeUniqueValues;
  AFieldIndex, i: integer;
begin
  if ExistsDateAttribute <> Value then
  begin
    if Value then
    begin
// Add
{ TODO -cНеобходимо : Откуда брать эти значения, из базового поля или из значений по умолчании или добавить свойства а TfcxCustomSplitPath в или еще откуда-то?}
      AFieldProperties.CaseSensitive := not FBaseUniqueValues.IgnoreCase;
      AFieldProperties.NullStr := FBaseUniqueValues.NullCaption;
      AFieldProperties.CubeFieldName := DateAttributeNamePrefix + TfcxCommonField(FBaseUniqueValues.fcField).CubeFieldName;
      AFieldProperties.CubeFieldDisplayLabel := fcxResources.Get('sDateAttributeLabelPrefix') + TfcxCommonField(FBaseUniqueValues.fcField).CubeFieldDisplayLabel;
      AFieldProperties.DataType := fcdt_Date;
      AFieldProperties.Saved := False;
      AFieldProperties.CalculateAfterAll := False;
      TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StartChange;
      AAttributeUniqueValues := AddAttributeInRunTime(FBaseUniqueValues.fcField, AFieldProperties,
        fcxsft_Date);
// Fill UVs
      AAttributeUniqueValues.Loading := True;

      for i := 0 to FBaseUniqueValues.Count - 1 do
        FListUVofPaths[FAttributesCount - 1][i] := AAttributeUniqueValues.AddAttributeFrom(FBaseUniqueValues.FList[i]);
      AAttributeUniqueValues.Sort(True);
      AAttributeUniqueValues.SetIndex;
      AAttributeUniqueValues.Loading := False;
      TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StopChange([chc_AddedOneSplit], TfcxCommonUVField(FBaseUniqueValues.fcField).Index, TfcxCommonUVField(FBaseUniqueValues.fcField), TfcxCommonUVField(AAttributeUniqueValues.fcField).Index);
    end
    else
    begin
// Delete
      for i := 0 to FAttributesCount - 1 do
        if FListUValues[i].AttributeType = fcxsft_Date then
        begin
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StartChange;
// need notify slices!
          AFieldIndex := TfcxCommonUVField(FListUValues[i].fcField).Index;
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StopChange([chc_DeletingOneSplit], TfcxCommonUVField(FBaseUniqueValues.fcField).Index, TfcxCommonUVField(FBaseUniqueValues.fcField), AFieldIndex);
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StartChange;
          RemoveAttribute(TfcxCommonUVField(FListUValues[i].fcField).AttributeIndex);
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StopChange([chc_DeletedOneSplit], TfcxCommonUVField(FBaseUniqueValues.fcField).Index, TfcxCommonUVField(FBaseUniqueValues.fcField), AFieldIndex);
          Exit;
        end;
    end
  end;
end;

procedure TfcxAttributesManager.SetExistsTimeAttribute(
  const Value: boolean);
var
  AFieldProperties: TfcxFieldProperties;
  AAttributeUniqueValues: TfcxAttributeUniqueValues;
  AFieldIndex, i: integer;
begin
  if ExistsTimeAttribute <> Value then
  begin
    if Value then
    begin
// Add
{ TODO -cНеобходимо : Откуда брать эти значения, из базового поля или из значений по умолчании или добавить свойства а TfcxCustomSplitPath в или еще откуда-то?}
      AFieldProperties.CaseSensitive := not FBaseUniqueValues.IgnoreCase;
      AFieldProperties.NullStr := FBaseUniqueValues.NullCaption;
      AFieldProperties.CubeFieldName := TimeAttributeNamePrefix + TfcxCommonField(FBaseUniqueValues.fcField).CubeFieldName;
      AFieldProperties.CubeFieldDisplayLabel := fcxResources.Get('sTimeAttributeLabelPrefix') + TfcxCommonField(FBaseUniqueValues.fcField).CubeFieldDisplayLabel;
      AFieldProperties.DataType := fcdt_Time;
      AFieldProperties.Saved := False;
      AFieldProperties.CalculateAfterAll := False;
      TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StartChange;
      AAttributeUniqueValues := AddAttributeInRunTime(FBaseUniqueValues.fcField, AFieldProperties,
        fcxsft_Time);
// Fill UVs
      AAttributeUniqueValues.Loading := True;

      for i := 0 to FBaseUniqueValues.Count - 1 do
        FListUVofPaths[FAttributesCount - 1][i] := AAttributeUniqueValues.AddAttributeFrom(FBaseUniqueValues.FList[i]);
      AAttributeUniqueValues.Sort(True);
      AAttributeUniqueValues.SetIndex;
      AAttributeUniqueValues.Loading := False;
      TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StopChange([chc_AddedOneSplit], TfcxCommonUVField(FBaseUniqueValues.fcField).Index, TfcxCommonUVField(FBaseUniqueValues.fcField), TfcxCommonUVField(AAttributeUniqueValues.fcField).Index);
    end
    else
    begin
// Delete
      for i := 0 to FAttributesCount - 1 do
        if FListUValues[i].AttributeType = fcxsft_Time then
        begin
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StartChange;
// need notify slices!
          AFieldIndex := TfcxCommonUVField(FListUValues[i].fcField).Index;
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StopChange([chc_DeletingOneSplit], TfcxCommonUVField(FBaseUniqueValues.fcField).Index, TfcxCommonUVField(FBaseUniqueValues.fcField), AFieldIndex);
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StartChange;
          RemoveAttribute(TfcxCommonUVField(FListUValues[i].fcField).AttributeIndex);
          TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).StopChange([chc_DeletedOneSplit], TfcxCommonUVField(FBaseUniqueValues.fcField).Index, TfcxCommonUVField(FBaseUniqueValues.fcField), AFieldIndex);
          Exit;
        end;
    end
  end;
end;

function TfcxAttributesManager.AddAttributeInRunTime(AField: Pointer;
  AFieldProperties: TfcxFieldProperties;
  AAttributeType: TfcxAttributeType): TfcxAttributeUniqueValues;
var
  AIndex: integer;
begin
{ TODO -cНеобходимо :  Нужно добавить проверку на совпадение имени части сплита !!!}
  AIndex := FindAttribute(AFieldProperties.CubeFieldName);
  if AIndex = -1 then
  begin
    Inc(FAttributesCount);
    ReallocMem(FListUVofPaths, FAttributesCount * fcPointerSize);
    ReallocMem(FListUValues, FAttributesCount * fcPointerSize);
    GetMem(FListUVofPaths[FAttributesCount - 1], FBaseUniqueValues.FCapacity * fcPointerSize);
    if FListUVofPaths[FAttributesCount - 1] <> nil then
      FillChar(FListUVofPaths[FAttributesCount - 1]^, FBaseUniqueValues.Count *  fcPointerSize, 0);
// Create TfcxUniqueValues for Custom Attribute
    FListUValues[FAttributesCount - 1] := TfcxAttributeUniqueValues(TfcxCommonUVField(TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.Items[TfcxCommonUVField(FBaseUniqueValues.fcField).Fields.AddAttributeStreamField(AFieldProperties, AAttributeType)]).UniqueValues);
    Result := FListUValues[FAttributesCount - 1];
    TfcxAttributeField(Result.fcField).AttributeIndex := FAttributesCount - 1;
{ TODO -cНеобходимо : Запуск заполнения части сплита. А в перспективе заполнение д.б. пакетное, т.е. нужно устанавливать флаг, а потом его обрабатывать!!! }
{ TODO -cПроверить : Не забыли ли чего !!! }
  end
  else
    Result := FListUValues[AIndex];
end;

procedure TfcxAttributesManager.AppendReferenceSource(AfcField: TObject;
  ADataSet: TfcxDataSet; AAttributeManagerArr: array of integer);
var
  RecNo: integer;
begin
  with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
  begin
    TimeStatStart.DBMoveTime := fcxGetTickCount;
    ADataSet.First;
    InternalOnProgressStart(fcxpFetchingData);
    RecNo := 0;
    TimeStat.DBMoveTime := TimeStat.DBMoveTime + fcxGetTickCount - TimeStatStart.DBMoveTime;
  end;
  TfcxCommonField(FBaseUniqueValues.fcField).Cube.TimeStatStart.DBMoveTime := fcxGetTickCount;
  while not ADataSet.Eof do
  begin
    with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
    begin
      InternalOnProgress(fcxpFetchingData, RecNo);
      TimeStat.DBMoveTime := TimeStat.DBMoveTime + fcxGetTickCount - TimeStatStart.DBMoveTime;
    end;
    AddReferenceValue(ADataSet, {AfcField1.SourceField.DataField, }AAttributeManagerArr);
    TfcxCommonField(FBaseUniqueValues.fcField).Cube.TimeStatStart.DBMoveTime := fcxGetTickCount;
    ADataSet.Next;
    inc(RecNo);
  end;
  with TfcxCommonField(FBaseUniqueValues.fcField).Cube do
  begin
    InternalOnProgressStop(fcxpFetchingData);
    TimeStat.DBMoveTime := TimeStat.DBMoveTime + fcxGetTickCount - TimeStatStart.DBMoveTime;
  end;
//  SetNullForUnReferenced;
end;

{ TfcxAttributeUniqueValues }

function TfcxAttributeUniqueValues.AddAttributeFrom(
  AUValue: PfcxCommonUV): PfcxCommonUV;
var
  APointer: Pointer;
  AVariant: Variant;
begin
  APointer := nil;
  case AttributeType of
    fcxsft_Reference:
      GetFieldAttributeValue(APointer);
    fcxsft_Custom:
      if GetAttributeValue(FBaseUniqueValues.ValueAsVariant[AUValue], AVariant) then
        APointer := VarValueToPointer(AVariant);
    fcxsft_Date, fcxsft_Time:
      APointer := cfcProcessorMap[DataType].ToPointer(FBaseUniqueValues.ValueAsVariant[AUValue]);
  end;
  AddNewValue(APointer, Result);
end;

function TfcxAttributeUniqueValues.AddNull: PfcxCommonUV;
begin
  AddNewValue(nil, Result);
end;

constructor TfcxAttributeUniqueValues.CreateAttribute(AfcField: TObject; ABaseUniqueValues: TfcxBaseUniqueValues;
  AWithCustomCaption: boolean; AAttributeType: TfcxAttributeType);
begin
  Create(AfcField, AWithCustomCaption);
  FFromMasterSource := False;
  FLoadAllValues := False;
  FDataField := nil;
  FIDDataField := nil;
  if AfcField is TfcxCommonUVField then
    FCalculateAfterAll := TfcxCommonUVField(AfcField).CalculateAfterAll
  else
    FCalculateAfterAll := False;
  FAttributeType := AAttributeType;
  FBaseUniqueValues := ABaseUniqueValues;
end;

constructor TfcxAttributeUniqueValues.CreateReferenceAttribute(AfcField: TObject; ADataField: TfcxReferenceDataField;
  AIDDataField: TfcxAddonReferenceDataField; ABaseUniqueValues: TfcxBaseUniqueValues; AWithCustomCaption: boolean; ALoadAllValues: Boolean);
begin
  inherited Create(AfcField, AWithCustomCaption);
  if fcField is TfcxAttributeField then
  begin
    FFromMasterSource := TfcxAttributeField(fcField).FromMasterSource;
    FLoadAllValues := TfcxAttributeField(fcField).LoadAllValues;
  end
  else
  begin
    FFromMasterSource := False;
    FLoadAllValues := False;
  end;
  FDataField := ADataField;
  FIDDataField := AIDDataField;
  FAttributeType := fcxsft_Reference;
  FBaseUniqueValues := ABaseUniqueValues;
end;

function TfcxAttributeUniqueValues.GetAttributeDisplayLabel: TfcxString;
begin
  Result := TfcxCommonField(fcField).CubeFieldDisplayLabel;
end;

function TfcxAttributeUniqueValues.GetAttributeName: TfcxString;
begin
  Result := TfcxCommonField(fcField).CubeFieldName;
end;

function TfcxAttributeUniqueValues.GetAttributeValue(AMasterValue: Variant;
  var AAttributeValue: Variant): Boolean;
begin
  Result := TfcxHackCube(TfcxCommonField(FBaseUniqueValues.FfcField).Cube).GetCustomAttributeValue(TfcxCommonField(FBaseUniqueValues.FfcField).CubeFieldName, AttributeName, AMasterValue, AAttributeValue);
end;

function TfcxAttributeUniqueValues.GetFieldAttributeValue(
  var APointer: Pointer): Boolean;
var
  ATempPointer2: Pointer;
begin
  TfcxCommonField(FfcField).Cube.TimeStatStart.DBGetDataTime := fcxGetTickCount;
  Result := FDataField.GetData(APointer);
  TfcxCommonField(FfcField).Cube.TimeStat.DBGetDataTime := TfcxCommonField(FfcField).Cube.TimeStat.DBGetDataTime + fcxGetTickCount - TfcxCommonField(FfcField).Cube.TimeStatStart.DBGetDataTime;
  if Result and FDataField.Convert then
  begin
{ TODO -cНеобходимо : Первично сделанное конвертирование. Возможно надо переделать.}
    TfcxCommonField(FfcField).Cube.TimeStatStart.ConvertTime := fcxGetTickCount;
    ATempPointer2 := APointer;
    APointer := cfcProcessorMap[FDataField.CubeFieldType].ToPointer(cfcProcessorMap[FDataField.DataFieldType].PointerAsVariant(ATempPointer2));
    FreeMem(ATempPointer2);
    TfcxCommonField(FfcField).Cube.TimeStat.ConvertTime := TfcxCommonField(FfcField).Cube.TimeStat.ConvertTime + fcxGetTickCount - TfcxCommonField(FfcField).Cube.TimeStatStart.ConvertTime;
    Result := (APointer <> nil);
  end;
end;

function TfcxAttributeUniqueValues.GetIDFieldAttributeValue(
  var APointer: Pointer): Boolean;
var
  ATempPointer2: Pointer;
begin
  if FIDDataField = nil then
  begin
    Result := False;
    APointer := nil;
    exit;
  end;
  TfcxCommonField(FfcField).Cube.TimeStatStart.DBGetDataTime := fcxGetTickCount;
  Result := FIDDataField.GetData(APointer);
  TfcxCommonField(FfcField).Cube.TimeStat.DBGetDataTime := TfcxCommonField(FfcField).Cube.TimeStat.DBGetDataTime + fcxGetTickCount - TfcxCommonField(FfcField).Cube.TimeStatStart.DBGetDataTime;
  if Result and FIDDataField.Convert then
  begin
{ TODO -cНеобходимо : Первично сделанное конвертирование. Возможно надо переделать.}
    TfcxCommonField(FfcField).Cube.TimeStatStart.ConvertTime := fcxGetTickCount;
    ATempPointer2 := APointer;
    APointer := cfcProcessorMap[FIDDataField.CubeFieldType].ToPointer(cfcProcessorMap[FIDDataField.DataFieldType].PointerAsVariant(ATempPointer2));
    FreeMem(ATempPointer2);
    TfcxCommonField(FfcField).Cube.TimeStat.ConvertTime := TfcxCommonField(FfcField).Cube.TimeStat.ConvertTime + fcxGetTickCount - TfcxCommonField(FfcField).Cube.TimeStatStart.ConvertTime;
    Result := (APointer <> nil);
  end;
end;

procedure TfcxAttributeUniqueValues.RemoveLinksToDataSource;
begin
  inherited;
  FDataField := nil;
  FIDDataField := nil;
end;

procedure TfcxAttributeUniqueValues.SetDataFields(ADataField,
  AIDDataField: TfcxReferenceDataField);
begin
  FDataField := ADataField;
  FIDDataField := AIDDataField;
end;

initialization
  fcDateTypeLength := ord(High(TfcxDateType));
  fcTimeTypeLength := ord(High(TfcxTimeType));
  fcPointerSize := SizeOf(Pointer);
{
рассмотрим массивы, размер которых зависит от числа уникальных значений.
1. Это:
один массив (для пользовательских названий значение)
  (TfcxBaseUniqueValues).(TfcxCustomCaptionManager)FCustomCaptionManager.(PfcxPfcxCharArray)FCaptionList // array of caption of unique values
один массив (для указания принадлежности к группе)
  (TfcxBaseUniqueValues).(TfcxCommonGroupManager)FGroupManager.(PfcxPGVArray)FGroupParentList // array of group values for each Unique Value (групповые значения, соответствующие уникальному)
один массив в каждом PathProcessor (для связки частей дат с базовым значением)
  (TfcxBaseUniqueValues).(TfcxSplitManager)FSplitManager.(TfcxDatePathsManager)FDatePathsManager.PfcxCommonDatePathFieldArray(FPathFields)[ADateType].DatePathProcessor.FPathValues
один массив в каждом PathProcessor (для связки частей времени с базовым значением)
  (TfcxBaseUniqueValues).(TfcxSplitManager)FSplitManager.(TfcxTimePathsManager)FTimePathsManager.PfcxCommonTimePathFieldArray(FPathFields)[ATimeType].TimePathProcessor.FPathValues
число массивов по числу атрибутов (для связки значений атрибутов с базовым значением)
  (TfcxBaseUniqueValues).(TfcxSplitManager)FSplitManager.(TfcxAttributesManager)FAttributesManager.(PfcxArrCUVArray)FListUVofPaths// List of arrays of pointers to UV of Split Paths

2. Когда устанавливается/меняется их размер?
Тогда, когда устанавливается/меняется число уникальных значений:
- Загрузка значений из Базы
При проходе основного источника устанавливается размер для атрибутов из того-же источника
По завершении прохода устанавливаем размер для  FCustomCaptionManager и FGroupManager - SetCaptionAndGroupCapacity
Затем идет запуск загрузки остальных атрибутов

  - Поля
- Загрузка значений из сохранённого куба
}
end.
