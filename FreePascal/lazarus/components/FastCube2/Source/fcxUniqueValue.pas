{*******************************************************}
{                                                       }
{             FastCube 2 Unique Value unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxUniqueValue;
{$INCLUDE fcx.inc}

interface
uses
  Classes, SysUtils, db
{$IFDEF SQL_TYPES_EXTRA0}
  , FMTBcd
{$ENDIF}
{$IFDEF SQL_TYPES_EXTRA1}
  , SqlTimSt
{$ENDIF}
{$IFDEF FPC}
  , LCLType
{$ELSE}
  , Windows
{$ENDIF}
  , fcxTypes, fcxXML, fcxFormats;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, System.SysUtils, Data.db
{$IFDEF SQL_TYPES_EXTRA0}
  , Data.FMTBcd
{$ENDIF}
{$IFDEF SQL_TYPES_EXTRA1}
  , Data.SqlTimSt
{$ENDIF}
{$IFDEF MSWINDOWS}
  , Winapi.Windows
{$ENDIF MSWINDOWS}
  , FMX.fcxTypes, FMX.fcxXML, FMX.fcxFormats;
{$ENDIF FMX}

type
  TfcxFieldClassCategory = (fcfcc_Error, fcfcc_Value, fcfcc_GetData);

  TfcxDataTypeProcessor = class;

  PfcxCommonUV = ^TfcxCommonUV;
  TfcxCommonUV = record // Common Unique Value
    Index: Integer; // Internal Index
//    Properties: byte; // Properties
  end;

  _fcxCUVArray = array[0..0] of PfcxCommonUV;
  PfcxCUVArray = ^_fcxCUVArray;

  _fcxArrCUVArray = array[0..0] of PfcxCUVArray;
  PfcxArrCUVArray = ^_fcxArrCUVArray;

  PfcxGroupValue = ^TfcxGroupValue;
  TfcxGroupValue = packed record // GroupValue
    Index: Integer; // Internal Index  код УЗ ГРУППЫ
    Caption: PfcxChar; // String Caption
    ListUV: PfcxCUVArray; // List of UV список УЗ, входящих в группу
    CountUV: integer; // Count of UV число УЗ, входящих в группу
    IsOther: Boolean; // GV is "other"
  end;
  _fcxPGVArray = array[0..0] of PfcxGroupValue;
  PfcxPGVArray = ^_fcxPGVArray;

  PfcxPointerUV = ^TfcxPointerUV;
  TfcxPointerUV = packed record // unique Value with Pointer
    Common: TfcxCommonUV;
    Value: Pointer;
  end;

  PfcxNotImplementedUV = ^TfcxNotImplementedUV;
  TfcxNotImplementedUV = packed record // NotImplemented Unique Value
    Common: TfcxCommonUV;
    Value: byte; // Pointer Value
  end;

  PfcxIntegerUV = ^TfcxIntegerUV;
  TfcxIntegerUV = packed record // Integer Unique Value
    Common: TfcxCommonUV;
    Value: Integer; // Integer Value
  end;

  PfcxByteUV = ^TfcxByteUV;
  TfcxByteUV = packed record // Byte Unique Value
    Common: TfcxCommonUV;
    Value: Byte; // Byte Value
  end;

  PfcxShortIntUV = ^TfcxShortIntUV;
  TfcxShortIntUV = packed record // ShortInt Unique Value
    Common: TfcxCommonUV;
    Value: ShortInt; // ShortInt Value
  end;

  PfcxWordUV = ^TfcxWordUV;
  TfcxWordUV = packed record // Word Unique Value
    Common: TfcxCommonUV;
    Value: Word; // Word Value
  end;

  PfcxDateTimeUV = ^TfcxDateTimeUV;
  TfcxDateTimeUV = packed record // DateTime Unique Value
    Common: TfcxCommonUV;
    Value: TDateTime; // TDateTime Value
  end;

  PfcxSmallIntUV = ^TfcxSmallIntUV;
  TfcxSmallIntUV = packed record // SmallInt Unique Value
    Common: TfcxCommonUV;
    Value: SmallInt; // SmallInt Value
  end;

  PfcxLargeIntUV = ^TfcxLargeIntUV;
  TfcxLargeIntUV = packed record // LargeInt Unique Value
    Common: TfcxCommonUV;
    Value: Int64; // Int64 Value
  end;

  PfcxDoubleUV = ^TfcxDoubleUV;
  TfcxDoubleUV = packed record // Double Unique Value
    Common: TfcxCommonUV;
    Value: Double; // Double Value
  end;

  PfcxSingleUV = ^TfcxDoubleUV;
  TfcxSingleUV = packed record // Double Unique Value
    Common: TfcxCommonUV;
    Value: Single; // Single Value
  end;

{$IFDEF SQL_TYPES_EXTRA0}
  PfcxBCDUV = ^TfcxBCDUV;
  TfcxBCDUV = packed record // BCD Unique Value
    Common: TfcxCommonUV;
    Value: TBCD; // BCD Value
  end;
{$ENDIF}

{$IFDEF SQL_TYPES_EXTRA1}
  PfcxTimeStampUV = ^TfcxTimeStampUV;
  TfcxTimeStampUV = packed record // TimeStamp Unique Value
    Common: TfcxCommonUV;
    Value: TSQLTimeStamp; // TimeStamp Value
  end;
{$ENDIF}

  PfcxStringUV = ^TfcxStringUV;
  TfcxStringUV = packed record // String unique Value
    Common: TfcxCommonUV;
    Value: PAnsiChar;
  end;

  PfcxWideStringUV = ^TfcxWideStringUV;
  TfcxWideStringUV = packed record // WideString unique Value
    Common: TfcxCommonUV;
    Value: PWideChar;
  end;

  PfcxBoolUV = ^TfcxBoolUV;
  TfcxBoolUV = packed record // Boolean Unique Value
    Common: TfcxCommonUV;
    Value: WordBool; // WordBool Value
  end;

  TfcxDataTypeProcessorClass = class of TfcxDataTypeProcessor;
  TfcxDatePathProcessorClass = class of TfcxCommonDatePathDTP;
  TfcxTimePathProcessorClass = class of TfcxCommonTimePathDTP;

  TfcxCommonDataTypeProcessor = class
  private
    FOwner: TObject;
    FHashBucketSize: word;
    FDisplayFormat: TfcxFormat;
    function GetSize: Integer; virtual; abstract;
    function GetDisplayFormat: TfcxFormat;
    procedure SetDisplayFormat(const Value: TfcxFormat);
    function GetDefaultDisplayFormat: TfcxFormat; virtual; abstract;
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;
    property HashBucketSize: word read FHashBucketSize;
    property DisplayFormat: TfcxFormat read GetDisplayFormat write SetDisplayFormat;
    property DefaultDisplayFormat: TfcxFormat read GetDefaultDisplayFormat;
  end;

  TfcxCommonStdPathDTP = class(TfcxCommonDataTypeProcessor)
  private
// FOwner - TfcxCommonStdPathField
    FPathValues: Pointer;
    FUValues: Pointer;
    FHashTable: PfcxByteArray;
    FCount: Integer;
    FMinNotNull: Integer;
    FSize: Integer;
    FHasNull: Boolean;
    function GetValueAtIndex(AIndex: integer): integer; virtual; abstract;
    function GetValueAtBaseUVIndex(ABaseUVIndex: integer): integer; virtual; abstract;
    function GetIndexAtBaseUVIndex(ABaseUVIndex: integer): integer; virtual;
    function GetIndexAtValue(AValue: integer): integer; virtual;
    function GetCaption(AValue: integer): TfcxString; virtual; abstract;
    function GetCaptionAtIndex(AIndex: integer): TfcxString; virtual;
    function GetNullCaption: TfcxString;
    function GetCaptionAtBaseUVIndex(ABaseUVIndex: Integer): TfcxString;
    function GetIsNullAtIndex(AIndex: integer): boolean; virtual; abstract;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
// создаем массив значений части даты размером, равным числу УЗ в основном поле
    procedure CreatePathValuesArray;
// создаем массив УЗ части даты на основе хэша
    procedure CreatePathUVsArray; virtual;
// удаление родительского уз
    procedure DeleteValue(AUVIndex: integer); virtual; abstract;
// вставка нового родительского уз
    procedure InsertValue(AUVIndex: integer); virtual; abstract;
// добавление нового значения
    procedure AddPathValue(AValue: integer; AUVIndex: integer); virtual; abstract;
// вставка нового значения в заполненный список
    procedure InsertPathValue(AValue: integer; AUVIndex: integer); virtual; abstract;
    function FindIndexAtValue(AValue: integer; var AIndex: integer): Boolean; virtual;
    function GetUVIndex(AUVVarValue: Variant): integer; virtual; abstract;
    procedure Recapacity;
    procedure SetCapacity(ASetAllToNil: boolean);
    procedure SaveGroupsToXML(AItem: TfcxXMLItem); virtual;
    procedure LoadGroupsFromXML(AItem: TfcxXMLItem); virtual;

    procedure SaveToStream(ACubeStream: TStream); virtual;

    procedure LoadFromStream(ACubeStream: TStream); virtual;
    procedure AppendFromStream(ACubeStream: TStream); virtual;
    procedure ClearGroups;
    function AsVariant(AValue: Integer): Variant; virtual; abstract;

    property IndexAtBaseUVIndex[ABaseUVIndex: integer]: integer read GetIndexAtBaseUVIndex;
    property IndexAtValue[AValue: integer]: integer read GetIndexAtValue;
    property ValueAtIndex[AIndex: integer]: integer read GetValueAtIndex;
    property ValueAtBaseUVIndex[ABaseUVIndex: integer]: integer read GetValueAtBaseUVIndex;
    property Caption[AValue: Integer]: TfcxString read GetCaption;
    property CaptionAtIndex[AIndex: Integer]: TfcxString read GetCaptionAtIndex;
    property CaptionAtBaseUVIndex[ABaseUVIndex: Integer]: TfcxString read GetCaptionAtBaseUVIndex;
    property NullCaption: TfcxString read GetNullCaption;
    property CountUV: Integer read FCount;
    property HasNull: Boolean read FHasNull;
    property IsNullAtIndex[AIndex: integer]: boolean read GetIsNullAtIndex;
  end;

  TfcxCommonDatePathDTP = class(TfcxCommonStdPathDTP)
  private
// FOwner - TfcxCommonDatePathField !
    FDateType: TfcxDateType;
    function GetSize: Integer; override;
    function GetCaption(AValue: Integer): TfcxString; override;
    function GetDefaultDisplayFormat: TfcxFormat; override;
    function GetIsNullAtIndex(AIndex: integer): boolean; override;
  public
    constructor Create(AOwner: TObject); override;
    function GetUVIndex(AUVVarValue: Variant): integer; override;
// добавление нового значения
    procedure AddPathValue(AValue: integer; AUVIndex: integer); override;
    function AsVariant(AValue: Integer): Variant; override;
  end;

  TfcxDatePathByteDTP = class(TfcxCommonDatePathDTP)
  private
    function GetValueAtIndex(AIndex: integer): integer; override;
    function GetValueAtBaseUVIndex(ABaseUVIndex: integer): integer; override;
  public
    function FindIndexAtValue(AValue: integer; var AIndex: integer): Boolean; override;
// удаление родительского уз
    procedure DeleteValue(AUVIndex: integer); override;
// вставка нового родительского уз
    procedure InsertValue(AUVIndex: integer); override;
    procedure AddPathValue(AValue: integer; AUVIndex: integer); override;
    procedure InsertPathValue(AValue: integer; AUVIndex: integer); override;
    procedure CreatePathUVsArray; override;
  end;

  TfcxDatePathWordDTP = class(TfcxCommonDatePathDTP)
  private
    function GetValueAtIndex(AIndex: integer): integer; override;
    function GetValueAtBaseUVIndex(ABaseUVIndex: integer): integer; override;
  public
    function FindIndexAtValue(AValue: integer; var AIndex: integer): Boolean; override;
// удаление родительского уз
    procedure DeleteValue(AUVIndex: integer); override;
// вставка нового родительского уз
    procedure InsertValue(AUVIndex: integer); override;
    procedure AddPathValue(AValue: integer; AUVIndex: integer); override;
    procedure InsertPathValue(AValue: integer; AUVIndex: integer); override;
    procedure CreatePathUVsArray; override;
  end;

  TfcxCommonTimePathDTP = class(TfcxCommonStdPathDTP)
  private
// FOwner - TfcxCommonTimePathField !
    FTimeType: TfcxTimeType;
    function GetSize: Integer; override;
    function GetCaption(AValue: Integer): TfcxString; override;
    function GetDefaultDisplayFormat: TfcxFormat; override;
    function GetIsNullAtIndex(AIndex: integer): boolean; override;
  public
    constructor Create(AOwner: TObject); override;
    function GetUVIndex(AUVVarValue: Variant): integer; override;
// добавление нового значения
    procedure AddPathValue(AValue: integer; AUVIndex: integer); override;
    function AsVariant(AValue: Integer): Variant; override;
  end;

  TfcxTimePathShortintDTP = class(TfcxCommonTimePathDTP)
  private
    function GetValueAtIndex(AIndex: integer): Integer; override;
    function GetValueAtBaseUVIndex(ABaseUVIndex: integer): Integer; override;
  public
    function FindIndexAtValue(AValue: Integer; var AIndex: integer): Boolean; override;
// удаление родительского уз
    procedure DeleteValue(AUVIndex: integer); override;
// вставка нового родительского уз
    procedure InsertValue(AUVIndex: integer); override;
    procedure AddPathValue(AValue: Integer; AUVIndex: integer); override;
    procedure InsertPathValue(AValue: integer; AUVIndex: integer); override;
    procedure CreatePathUVsArray; override;
  end;

  TfcxTimePathSmallintDTP = class(TfcxCommonTimePathDTP)
  private
    function GetValueAtIndex(AIndex: integer): Integer; override;
    function GetValueAtBaseUVIndex(ABaseUVIndex: integer): Integer; override;
  public
    function FindIndexAtValue(AValue: Integer; var AIndex: integer): Boolean; override;
// удаление родительского уз
    procedure DeleteValue(AUVIndex: integer); override;
// вставка нового родительского уз
    procedure InsertValue(AUVIndex: integer); override;
    procedure AddPathValue(AValue: Integer; AUVIndex: integer); override;
    procedure InsertPathValue(AValue: integer; AUVIndex: integer); override;
    procedure CreatePathUVsArray; override;
  end;

  TfcxDataTypeProcessor = class(TfcxCommonDataTypeProcessor)
  private
// FOwner - TfcxUniqueValues !
    function GetCaption(AUValue: pointer): TfcxString; virtual;
    function GetIndex(AUValue: pointer): integer; virtual;
    procedure SetIndex(AUValue: pointer; const Value: integer); virtual;
    function GetNullCaption: TfcxString;
    function GetDefaultDisplayFormat: TfcxFormat; override;
  public
    function IsNull(AUValue: pointer): boolean; virtual;
    class function DataType: TfcxDataType; virtual;
    function AsVariant(AUValue: pointer): Variant; virtual;
    class function ToPointer(AValue: Variant): pointer; virtual;
    class function PointerAsVariant(APointer: pointer): Variant; virtual;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); virtual;
    class procedure CorrectBuffer(ABuffer: pointer); virtual;
// Return pointer (PfcxCommonUV) to UValue
    function LoadFromStream(ACubeStream: TStream): pointer; virtual;
// Return pointer to Value
    function LoadValueFromStream(ACubeStream: TStream): pointer; virtual;
    function LoadFromOldStream(ACubeStream: TStream): pointer; virtual;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); virtual; abstract;

    constructor Create(AOwner: TObject); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; virtual; abstract;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; virtual; abstract;
    procedure FreeUValue(AUValue: pointer); virtual;
    function NewUValue(AValue: pointer): pointer; virtual;
    function Hash(AValue: Pointer): cardinal; virtual; abstract;
    property Caption[AUValue: pointer]: TfcxString read GetCaption;
    property Index[AUValue: pointer]: integer read GetIndex write SetIndex;
    property NullCaption: TfcxString read GetNullCaption;
  end;

  TfcxNotImplementedDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
    function GetCaption(AUValue: pointer): TfcxString; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxIntegerDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxByteDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxWordDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxDateTimeDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    class procedure CorrectBuffer(ABuffer: pointer); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxDateDTP = class(TfcxIntegerDTP)
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
  end;

  TfcxTimeDTP = class(TfcxIntegerDTP)
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
  end;

  TfcxShortIntegerDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxSmallIntDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxLargeIntDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxDoubleDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxCurrencyDTP = class(TfcxDoubleDTP)
  public
    class function DataType: TfcxDataType; override;
  end;

{$IFDEF SQL_TYPES_EXTRA0}
  TfcxBCDDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;
{$ENDIF}

{$IFDEF SQL_TYPES_EXTRA1}
  TfcxTimeStampDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;
{$ENDIF}

  TfcxPointerDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    procedure FreeUValue(AUValue: pointer); override;
    function NewUValue(AValue: pointer): pointer; override;
  end;

  TfcxStringDTP = class(TfcxPointerDTP)
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function Hash(AValue: Pointer): cardinal; override;
    function NewUValue(AValue: pointer): pointer; override;
  end;

  TfcxWideStringDTP = class(TfcxPointerDTP)
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function Hash(AValue: Pointer): cardinal; override;
    function NewUValue(AValue: pointer): pointer; override;
  end;

  TfcxBooleanDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;

  TfcxSingleDTP = class(TfcxDataTypeProcessor)
  private
    function GetSize: Integer; override;
  public
    class function DataType: TfcxDataType; override;
    function AsVariant(AUValue: pointer): Variant; override;
    class function ToPointer(AValue: Variant): pointer; override;
    class function PointerAsVariant(APointer: pointer): Variant; override;
    class procedure SaveToStream(AUValue: pointer; ACubeStream: TStream); override;
    function LoadFromStream(ACubeStream: TStream): pointer; override;
    function LoadValueFromStream(ACubeStream: TStream): pointer; override;
    procedure SetVariantValue(AUValue: pointer; AValue: Variant); override;
    function Compare(const AUValue1, AUValue2: Pointer): Integer; override;
    function CompareForInsert(const AUValue1, AValue2: Pointer): Integer; override;
    function NewUValue(AValue: pointer): pointer; override;
    function Hash(AValue: Pointer): cardinal; override;
  end;


const
  DataTypeToVarType: array[TfcxDataType] of Word = (
 { fcdt_Error          } varError,
 { fcdt_NotImplemented } varError,
 { fcdt_Integer        } varInteger,
 { fcdt_Byte           } varByte,
 { fcdt_Word           } {$IFDEF DELPHI_6UP} varWord {$ELSE} $12 {$ENDIF},
 { fcdt_DateTime       } varDate,
 { fcdt_Date           } varDate,
 { fcdt_Time           } varDate,
 { fcdt_SmallInteger   } varSmallint,
 { fcdt_LargeInteger   } {$IFDEF DELPHI_6UP} varInt64 {$ELSE} $14 {$ENDIF},
 { fcdt_Double         } varDouble,
 { fcdt_BCD            } varDouble,
 { fcdt_String         } varString,
 { fcdt_WideString     } varOleStr,
 { fcdt_Boolean        } varBoolean,
 { fcdt_Currency       } varDouble,
 { fcdt_TimeStamp      } varDate,
 { fcdt_Single         } varSingle,
 { fcdt_ShortInteger   } varShortInt
  );

  cfcDataTypeNames: array[TfcxDataType] of TfcxString = (
 { fcdt_Error          } 'Error',
 { fcdt_NotImplemented } 'NotImplemented',
 { fcdt_Integer        } 'Integer',
 { fcdt_Byte           } 'Byte',
 { fcdt_Word           } 'Word',
 { fcdt_DateTime       } 'DateTime',
 { fcdt_Date           } 'Date',
 { fcdt_Time           } 'Time',
 { fcdt_SmallInteger   } 'SmallInteger',
 { fcdt_LargeInteger   } 'LargeInteger',
 { fcdt_Double         } 'Double',
 { fcdt_BCD            } 'BCD',
 { fcdt_String         } 'String',
 { fcdt_WideString     } 'WideString',
 { fcdt_Boolean        } 'Boolean',
 { fcdt_Currency       } 'Currency',
 { fcdt_TimeStamp      } 'TimeStamp',
 { fcdt_Single         } 'Single',
 { fcdt_ShortInteger   } 'ShortInteger'
 );

  cfcFieldTypeToDataType: array[TFieldType] of TfcxDataType = (
 { ftUnknown       } fcdt_NotImplemented,
 { ftString        } fcdt_String,
 { ftSmallint      } fcdt_SmallInteger,
 { ftInteger       } fcdt_Integer,
 { ftWord          } fcdt_Word,
 { ftBoolean       } fcdt_Boolean,
 { ftFloat         } fcdt_Double,
 { ftCurrency      } fcdt_Currency,
 { ftBCD           } fcdt_BCD,
 { ftDate          } fcdt_Date,
 { ftTime          } fcdt_Time,
 { ftDateTime      } fcdt_DateTime,
 { ftBytes         } fcdt_NotImplemented,
 { ftVarBytes      } fcdt_NotImplemented,
 { ftAutoInc       } fcdt_Integer,
 { ftBlob          } fcdt_NotImplemented,
 { ftMemo          } fcdt_String, //?
 { ftGraphic       } fcdt_NotImplemented,
 { ftFmtMemo       } fcdt_String, //?
 { ftParadoxOle    } fcdt_NotImplemented,
 { ftDBaseOle      } fcdt_NotImplemented,
 { ftTypedBinary   } fcdt_NotImplemented,
 { ftCursor        } fcdt_NotImplemented,
 { ftFixedChar     } fcdt_String,
 { ftWideString    } fcdt_WideString,
 { ftLargeint      } {$IFDEF SQL_TYPES_EXTRA0} fcdt_LargeInteger, {$ELSE} fcdt_NotImplemented, {$ENDIF}
 { ftADT           } fcdt_NotImplemented,
 { ftArray         } fcdt_NotImplemented,
 { ftReference     } fcdt_NotImplemented,
 { ftDataSet       } fcdt_NotImplemented,
 { ftOraBlob       } fcdt_NotImplemented,
 { ftOraClob       } fcdt_NotImplemented,
 { ftVariant       } fcdt_NotImplemented,
 { ftInterface     } fcdt_NotImplemented,
 { ftIDispatch     } fcdt_NotImplemented,
 { ftGuid          } fcdt_NotImplemented
{$IFDEF SQL_TYPES_EXTRA0}
 { ftTimeStamp     } , {$IFDEF SQL_TYPES_EXTRA1} fcdt_TimeStamp {$ELSE} fcdt_DateTime {$ENDIF}
 { ftFMTBcd        } , fcdt_BCD
{$ENDIF}
{$IFDEF FPC}
 { ftFixedWideChar } , fcdt_NotImplemented
 { ftWideMemo      } , fcdt_NotImplemented
{$ENDIF}
{$IFDEF DELPHI_10UP}
 { ftFixedWideChar } , fcdt_NotImplemented
 { ftWideMemo      } , fcdt_NotImplemented
 { ftOraTimeStamp  } , fcdt_NotImplemented
 { ftOraInterval   } , fcdt_NotImplemented
{$ENDIF}
{$IFDEF DELPHI_12UP}
 { ftLongWord      } , fcdt_NotImplemented
 { ftShortint      } , fcdt_ShortInteger
 { ftByte          } , fcdt_Byte
 { ftExtended      } , fcdt_NotImplemented
 { ftConnection    } , fcdt_NotImplemented
 { ftParams        } , fcdt_NotImplemented
 { ftStream        } , fcdt_NotImplemented
{$ENDIF}
{$IFDEF DELPHI_14UP}
 {ftTimeStampOffset} , fcdt_NotImplemented
 { ftObject        } , fcdt_NotImplemented
 { ftSingle        } , fcdt_Single
{$ENDIF}
    );

  cfcProcessorMap: array[TfcxDataType] of TfcxDataTypeProcessorClass = (
 { fcdt_Error          } TfcxNotImplementedDTP,
 { fcdt_NotImplemented } TfcxNotImplementedDTP,
 { fcdt_Integer        } TfcxIntegerDTP,
 { fcdt_Byte           } TfcxByteDTP,
 { fcdt_Word           } TfcxWordDTP,
 { fcdt_DateTime       } TfcxDateTimeDTP,
 { fcdt_Date           } TfcxDateDTP,
 { fcdt_Time           } TfcxTimeDTP,
 { fcdt_SmallInteger   } TfcxSmallIntDTP,
 { fcdt_LargeInteger   } TfcxLargeIntDTP,
 { fcdt_Double         } TfcxDoubleDTP,
 { fcdt_BCD            } {$IFDEF SQL_TYPES_EXTRA0}TfcxBCDDTP{$ELSE}TfcxNotImplementedDTP{$ENDIF},
 { fcdt_String         } TfcxStringDTP,
 { fcdt_WideString     } TfcxWideStringDTP,
 { fcdt_Boolean        } TfcxBooleanDTP,
 { fcdt_Currency       } TfcxCurrencyDTP,
 { fcdt_TimeStamp      } {$IFDEF SQL_TYPES_EXTRA1}TfcxTimeStampDTP{$ELSE}TfcxNotImplementedDTP{$ENDIF},
 { fcdt_Single         } TfcxSingleDTP,
 { fcdt_ShortInteger   } TfcxShortIntegerDTP
  );

  cfcDatePathProcessorMap: array[TfcxDateType] of TfcxDatePathProcessorClass = (
  { odt_None        }  TfcxDatePathByteDTP,
  { odt_Year        }  TfcxDatePathWordDTP,
  { odt_Month       }  TfcxDatePathByteDTP,
  { odt_Day         }  TfcxDatePathByteDTP,
  { odt_DayOfWeek   }  TfcxDatePathByteDTP,
  { odt_Quarter     }  TfcxDatePathByteDTP,
  { odt_WeekNumber  }  TfcxDatePathByteDTP,
  { odt_DayOfYear   }  TfcxDatePathWordDTP
  );

  cfcTimePathProcessorMap: array[TfcxTimeType] of TfcxTimePathProcessorClass = (
  { ott_None,         }  TfcxTimePathShortintDTP,
  { ott_Hour,         }  TfcxTimePathShortintDTP,
  { ott_Minute,       }  TfcxTimePathShortintDTP,
  { ott_Second        }  TfcxTimePathShortintDTP,
  { ott_Millisecond   }  TfcxTimePathSmallintDTP
  );

// Properties flags
  puvNone: Byte = $00;  // empty
  puvValueInGroup: Byte = $01; // Value In Group
  puvNullValue: Byte = $80; // Null Value

  function fcFieldTypeRealized(AFieldType: TFieldType): boolean;
  function fcFieldClassCategory(AField: TField; AUseGetDataInInheritedClasses: boolean = false): TfcxFieldClassCategory;

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  Math,
{$IFDEF Delphi_9UP}
  WideStrUtils,
{$ENDIF}
{$IFDEF DELPHI_6UP}
  Variants,
{$ENDIF}
  TypInfo,
  fcxUniqueArray, fcxCube, fcxStringUtils, fcxStreamUtils;
//FMX uses
{$ELSE FMX}
uses
  System.Math,
  System.WideStrUtils,
  System.Variants,
  System.TypInfo,
  FMX.fcxUniqueArray, FMX.fcxCube, FMX.fcxStringUtils, FMX.fcxStreamUtils;
{$ENDIF FMX}

type
  TfcxHackBaseUniqueValues = class(TfcxBaseUniqueValues);
const
  cHashBucketSize: array[TfcxDataType] of Word = (
 { fcdt_Error          } 251,
 { fcdt_NotImplemented } 251,
 { fcdt_Integer        } 65521,
 { fcdt_Byte           } 256,
 { fcdt_Word           } 65521,
 { fcdt_DateTime       } 65521,
 { fcdt_Date           } 65521,
 { fcdt_Time           } 65521,
 { fcdt_SmallInteger   } 65521,
 { fcdt_LargeInteger   } 65521,
 { fcdt_Double         } 65521,
 { fcdt_BCD            } 65521,
 { fcdt_String         } 65521,
 { fcdt_WideString     } 65521,
 { fcdt_Boolean        } 65521,
 { fcdt_Currency       } 65521,
 { fcdt_TimeStamp      } 251,
 { fcdt_Single         } 65521,
 { fcdt_ShortInteger   } 256
  );

function fcFieldTypeRealized(AFieldType: TFieldType): boolean;
begin
  Result := cfcFieldTypeToDataType[AFieldType] <> fcdt_NotImplemented;
end;

function fcFieldClassCanGetData(AClassName: String): boolean;
begin
  AClassName := UpperCase(AClassName);
  result :=
  // some special checks for IBObjects
    (AClassName = 'TIBOFLOATFIELD')
    or
    (AClassName = 'TIBOBCDFIELD')
    or
    (AClassName = 'TIBOLARGEINTFIELD')
  // some special checks for Fib+
    or
    (AClassName = 'TFIBINTEGERFIELD')
    or
    (AClassName = 'TFIBSMALLINTFIELD')
    or
    (AClassName = 'TFIBTIMEFIELD')
    or
    (AClassName = 'TFIBDATETIMEFIELD')
    or
    (AClassName = 'TFIBDATEFIELD')
    or
  // some special checks for InterBase Express
    (AClassName = 'TIBBCDFIELD')
    or
    (AClassName = 'TIBSTRINGFIELD')
    ;
end;

function fcFieldClassCategory(AField: TField; AUseGetDataInInheritedClasses: boolean = false): TfcxFieldClassCategory;
begin
  if (DefaultFieldClasses[AField.DataType] = nil) or not fcFieldTypeRealized(AField.DataType) or AField.InheritsFrom(TBlobField) then
  begin
    Result := fcfcc_Error;
    exit;
  end
  else
  if (DefaultFieldClasses[AField.DataType] = AField.ClassType) or (UpperCase(DefaultFieldClasses[AField.DataType].ClassName) = UpperCase(AField.ClassName)) then
  begin
    Result := fcfcc_GetData;
    exit;
  end
  else
  if fcFieldClassCanGetData(AField.ClassName) then
  begin
    Result := fcfcc_GetData;
    exit;
  end
  else
  if AField.InheritsFrom(DefaultFieldClasses[AField.DataType]) then
  begin
    if AUseGetDataInInheritedClasses then
      Result := fcfcc_GetData
    else
      Result := fcfcc_Value;
    exit;
  end
  else
    Result := fcfcc_Error;
end;

function DateToDateTime(Value: Integer): TDateTime;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp.Date := Value;
  TimeStamp.Time := 0;
  Result := TimeStampToDateTime(TimeStamp);
end;

function TimeToDateTime(Value: Integer): TDateTime;
//var
//  TimeStamp: TTimeStamp;
begin
(*
  TimeStamp.Date := DateTimeToTimeStamp(now).Date;
  TimeStamp.Time := Value;
  Result := TimeStampToDateTime(TimeStamp);
  Result := Result - trunc(Result);
*)
  Result := Value / (24000*3600);
end;

function GetTimeStampDate(Value: TDateTime): Integer;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Value);
  Result := TimeStamp.Date
end;

function GetTimeStampTime(Value: TDateTime): Integer;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Value);
  Result := TimeStamp.Time;
end;

{ TfcxDataTypeProcessor }

class function TfcxDataTypeProcessor.ToPointer(AValue: Variant): pointer;
begin
  Result := Nil;
end;

function TfcxDataTypeProcessor.AsVariant(AUValue: pointer): Variant;
begin
  Result := Null;
end;

class procedure TfcxDataTypeProcessor.CorrectBuffer(ABuffer: pointer);
begin
//
end;

constructor TfcxDataTypeProcessor.Create(AOwner: TObject);
begin
  inherited;
  FHashBucketSize := cHashBucketSize[DataType];
//  FDisplayFormat := TfcxFormat.Create(FOwner, TfcxCommonField(TfcxBaseUniqueValues(FOwner).fcField).Cube.Formats.DefaultFormatByType(DataType));
end;

class function TfcxDataTypeProcessor.DataType: TfcxDataType;
begin
  Result := fcdt_Error;
end;

procedure TfcxDataTypeProcessor.FreeUValue(AUValue: pointer);
begin
  FreeMem(AUValue);
end;

function TfcxDataTypeProcessor.GetCaption(AUValue: pointer): TfcxString;
begin
  Result := DisplayFormat.FormatData(AsVariant(AUValue), Self);
end;

function TfcxDataTypeProcessor.GetDefaultDisplayFormat: TfcxFormat;
begin
  Result := TfcxCommonField(TfcxBaseUniqueValues(FOwner).fcField).Cube.Formats.DefaultFormatByType(DataType);
end;

function TfcxDataTypeProcessor.GetIndex(AUValue: pointer): integer;
begin
  Result := PfcxCommonUV(AUValue).Index;
end;

function TfcxDataTypeProcessor.GetNullCaption: TfcxString;
begin
  Result := TfcxBaseUniqueValues(FOwner).NullCaption;
end;

function TfcxDataTypeProcessor.IsNull(AUValue: pointer): boolean;
begin
  Result := TfcxBaseUniqueValues(FOwner).HasNull and (AUValue = TfcxBaseUniqueValues(FOwner).NullValue)
end;

function TfcxDataTypeProcessor.LoadFromOldStream(ACubeStream: TStream): pointer;
begin
  Result := NewUValue(nil);
end;

function TfcxDataTypeProcessor.LoadFromStream(
  ACubeStream: TStream): pointer;
begin
  Result := NewUValue(nil);
end;

function TfcxDataTypeProcessor.LoadValueFromStream(
  ACubeStream: TStream): pointer;
begin
  Result := nil;
end;

function TfcxDataTypeProcessor.NewUValue(AValue: pointer): pointer;
begin
  GetMem(Result, GetSize);
  PfcxCommonUV(Result).Index := 0;
{
  if AValue <> nil then
    PfcxCommonUV(Result).Properties := 0
  else
    PfcxCommonUV(Result).Properties := puvNullValue;
}
end;

class function TfcxDataTypeProcessor.PointerAsVariant(
  APointer: pointer): Variant;
begin
  Result := Null;
end;

class procedure TfcxDataTypeProcessor.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
//  ACubeStream.Write(PfcxCommonUV(AUValue).Index, SizeOf(Integer));
//  ACubeStream.Write(PfcxCommonUV(AUValue).Properties, SizeOf(Byte));
end;

procedure TfcxDataTypeProcessor.SetIndex(AUValue: pointer;
  const Value: integer);
begin
  PfcxCommonUV(AUValue).Index := Value;
end;

{ TfcxNotImplementedDTP }

function TfcxNotImplementedDTP.AsVariant(AUValue: pointer): Variant;
begin
  Result := Unassigned;
end;

function TfcxNotImplementedDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  Result := 0;
end;

function TfcxNotImplementedDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  Result := 0;
end;

class function TfcxNotImplementedDTP.DataType: TfcxDataType;
begin
  Result := fcdt_NotImplemented;
end;

function TfcxNotImplementedDTP.GetCaption(
  AUValue: pointer): TfcxString;
begin
  if IsNull(AUValue) then
    Result := NullCaption
  else
    Result := 'NotImplementedUniqueValue'
end;

function TfcxNotImplementedDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxNotImplementedUV);
end;

function TfcxNotImplementedDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := 0;
end;

function TfcxNotImplementedDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  PfcxNotImplementedUV(Result).Value := 0;
  FreeMem(AValue);
end;

class function TfcxNotImplementedDTP.PointerAsVariant(
  APointer: pointer): Variant;
begin
  Result := Unassigned;
end;

procedure TfcxNotImplementedDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  PfcxNotImplementedUV(AUValue).Value := 0;
end;

class function TfcxNotImplementedDTP.ToPointer(AValue: Variant): pointer;
begin
  Result := nil;
end;

{ TfcxIntegerDTP }

function TfcxIntegerDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxIntegerUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxIntegerDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
// AUValue1 and AUValue2 can not be null
  if PfcxIntegerUV(AUValue1).Value = PfcxIntegerUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxIntegerUV(AUValue1).Value > PfcxIntegerUV(AUValue2).Value];
end;

function TfcxIntegerDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
// AUValue1 and AValue2 can not be null
  if PfcxIntegerUV(AUValue1).Value = PInteger(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxIntegerUV(AUValue1).Value > PInteger(AValue2)^];
end;

class function TfcxIntegerDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Integer;
end;

function TfcxIntegerDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxIntegerUV)
end;

function TfcxIntegerDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := PCardinal(AValue)^ mod FHashBucketSize;
end;

function TfcxIntegerDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxIntegerUV(result).Value, SizeOf(Integer));
end;

function TfcxIntegerDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Integer));
  ACubeStream.Read(Result^, SizeOf(Integer));
end;

function TfcxIntegerDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxIntegerUV(Result).Value := 0
  else
    PfcxIntegerUV(Result).Value := PInteger(AValue)^;
  FreeMem(AValue);
end;

class function TfcxIntegerDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PInteger(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxIntegerDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxIntegerUV(AUValue).Value, SizeOf(Integer));
end;

procedure TfcxIntegerDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxIntegerUV(AUValue).Value := AValue
    except
      PfcxIntegerUV(AUValue).Value := 0;
    end
  else
    PfcxIntegerUV(AUValue).Value := 0;
end;

class function TfcxIntegerDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(Integer));
    try
      PInteger(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxDateTimeDTP }

function TfcxDateTimeDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxDateTimeUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxDateTimeDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
// AUValue1 and AUValue2 can not be null
  if PfcxDateTimeUV(AUValue1).Value = PfcxDateTimeUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxDateTimeUV(AUValue1).Value > PfcxDateTimeUV(AUValue2).Value];
end;

function TfcxDateTimeDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
// AUValue1 and AValue2 can not be null
  if PfcxDateTimeUV(AUValue1).Value = PDateTime(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxDateTimeUV(AUValue1).Value > PDateTime(AValue2)^];
end;

class procedure TfcxDateTimeDTP.CorrectBuffer(ABuffer: pointer);
var
  T: TTimeStamp;
begin
  T := MSecsToTimeStamp(PDouble(ABuffer)^);
  PDouble(ABuffer)^ := TimeStampToDateTime(T);
end;

class function TfcxDateTimeDTP.DataType: TfcxDataType;
begin
  Result := fcdt_DateTime;
end;

function TfcxDateTimeDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxDateTimeUV)
end;

function TfcxDateTimeDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := Abs(PInt64(AValue)^ mod FHashBucketSize);
end;

function TfcxDateTimeDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxDateTimeUV(result).Value, SizeOf(TDateTime));
end;

function TfcxDateTimeDTP.LoadValueFromStream(
  ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(TDateTime));
  ACubeStream.Read(Result^, SizeOf(TDateTime));
end;

function TfcxDateTimeDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxDateTimeUV(Result).Value := 0
  else
    PfcxDateTimeUV(Result).Value := PDateTime(AValue)^;
  FreeMem(AValue);
end;

class function TfcxDateTimeDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PDateTime(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxDateTimeDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxDateTimeUV(AUValue).Value, SizeOf(TDateTime));
end;

procedure TfcxDateTimeDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
var
  D: TDateTime;
begin
  if TVarData(AValue).VType > varNull then
    try
      D := VarToDateTime(AValue);
      PfcxDateTimeUV(AUValue).Value := D
    except
      PfcxDateTimeUV(AUValue).Value := 0;
    end
  else
    PfcxDateTimeUV(AUValue).Value := 0;
end;

class function TfcxDateTimeDTP.ToPointer(AValue: Variant): pointer;
var
  D: TDateTime;
begin
  if TVarData(AValue).VType > varNull then
  begin
    try
      D := VarToDateTime(AValue);
    except
      Result := nil;
      exit;
    end;
    GetMem(Result, SizeOf(TDateTime));
    try
      PDateTime(Result)^ := D;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxWordDTP }

function TfcxWordDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxWordUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxWordDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
// AUValue1 and AUValue2 can not be null
  if PfcxWordUV(AUValue1).Value = PfcxWordUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxWordUV(AUValue1).Value > PfcxWordUV(AUValue2).Value];
end;

function TfcxWordDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
// AUValue1 and AValue2 can not be null
  if PfcxWordUV(AUValue1).Value = PWord(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxWordUV(AUValue1).Value > PWord(AValue2)^];
end;

class function TfcxWordDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Word;
end;

function TfcxWordDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxWordUV)
end;

function TfcxWordDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := PWord(AValue)^ mod FHashBucketSize;
end;

function TfcxWordDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxWordUV(result).Value, SizeOf(Word));
end;

function TfcxWordDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Word));
  ACubeStream.Read(Result^, SizeOf(Word));
end;

function TfcxWordDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxWordUV(Result).Value := 0
  else
    PfcxWordUV(Result).Value := PWord(AValue)^;
  FreeMem(AValue);
end;

class function TfcxWordDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PWord(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxWordDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxWordUV(AUValue).Value, SizeOf(Word));
end;

procedure TfcxWordDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxWordUV(AUValue).Value := AValue
    except
      PfcxWordUV(AUValue).Value := 0;
    end
  else
    PfcxWordUV(AUValue).Value := 0;
end;

class function TfcxWordDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(Word));
    try
      PWord(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxByteDTP }

function TfcxByteDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxByteUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxByteDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
// AUValue1 and AUValue2 can not be null
  if PfcxByteUV(AUValue1).Value = PfcxByteUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxByteUV(AUValue1).Value > PfcxByteUV(AUValue2).Value];
end;

function TfcxByteDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
// AUValue1 and AValue2 can not be null
  if PfcxByteUV(AUValue1).Value = PByte(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxByteUV(AUValue1).Value > PByte(AValue2)^];
end;

class function TfcxByteDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Byte;
end;

function TfcxByteDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxByteUV)
end;

function TfcxByteDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := PByte(AValue)^ mod FHashBucketSize;
end;

function TfcxByteDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxByteUV(result).Value, SizeOf(Byte));
end;

function TfcxByteDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Byte));
  ACubeStream.Read(Result^, SizeOf(Byte));
end;

function TfcxByteDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxByteUV(Result).Value := 0
  else
    PfcxByteUV(Result).Value := PByte(AValue)^;
  FreeMem(AValue);
end;

class function TfcxByteDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PByte(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxByteDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxByteUV(AUValue).Value, SizeOf(Byte));
end;

procedure TfcxByteDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxByteUV(AUValue).Value := AValue
    except
      PfcxByteUV(AUValue).Value := 0;
    end
  else
    PfcxByteUV(AUValue).Value := 0;
end;

class function TfcxByteDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(Byte));
    try
      PByte(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxStringDTP }

function TfcxStringDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
  begin
    Result := String(PfcxStringUV(AUValue).Value);
    try
      Result := VarAsType(Result, DataTypeToVarType[DataType]);
    except
      Result := Null
    end;
  end;
end;

function TfcxStringDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
var
  StrVal1: PfcxStringUV absolute AUValue1;
  StrVal2: PfcxStringUV absolute AUValue2;
begin
// AUValue1 and AUValue2 can not be null
{$IFNDEF fpc}
  {$IFNDEF FMX}
    if TfcxBaseUniqueValues(FOwner).IgnoreCase then
      Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
        -1, StrVal2.Value, -1) - 2
    else
      Result := CompareStringA(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
        -1, StrVal2.Value, -1) - 2;
  {$ELSE FMX}
    {$IFDEF MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
        Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
         -1, StrVal2.Value, -1) - 2
      else
        Result := CompareStringA(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
          -1, StrVal2.Value, -1) - 2;
    {$ELSE MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
         Result := AnsiCompareText(StrPas(StrVal1.Value), StrPas(StrVal2.Value))
      else
         Result := AnsiCompareStr(StrPas(StrVal1.Value), StrPas(StrVal2.Value));
    {$ENDIF MSWINDOWS}
  {$ENDIF FMX}
{$ELSE}
  if TfcxBaseUniqueValues(FOwner).IgnoreCase then
    Result := AnsiCompareText(StrPas(StrVal1.Value), StrPas(StrVal2.Value))
  else
    Result := AnsiCompareStr(StrPas(StrVal1.Value), StrPas(StrVal2.Value));
{$ENDIF}
end;

function TfcxStringDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
var
  StrVal1: PfcxStringUV absolute AUValue1;
begin
// AUValue1 and AValue2 can not be null
{$IFNDEF fpc}
  {$IFNDEF FMX}
    if TfcxBaseUniqueValues(FOwner).IgnoreCase then
      Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
        -1, AValue2, -1) - 2
    else
      Result := CompareStringA(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
        -1, AValue2, -1) - 2;
  {$ELSE FMX}
    {$IFDEF MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
        Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
          -1, AValue2, -1) - 2
      else
        Result := CompareStringA(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
          -1, AValue2, -1) - 2;
    {$ELSE MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
         Result := AnsiCompareText(StrPas(StrVal1.Value), StrPas(PAnsiChar(AValue2)))
      else
         Result := AnsiCompareStr(StrPas(StrVal1.Value), StrPas(PAnsiChar(AValue2)));
    {$ENDIF MSWINDOWS}
  {$ENDIF FMX}
{$ELSE}
  if TfcxBaseUniqueValues(FOwner).IgnoreCase then
    Result := AnsiCompareText(StrPas(StrVal1.Value), StrPas(AValue2))
  else
    Result := AnsiCompareStr(StrPas(StrVal1.Value), StrPas(AValue2));
{$ENDIF}
end;

class function TfcxStringDTP.DataType: TfcxDataType;
begin
  Result := fcdt_String;
end;

function TfcxStringDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := 0;
  {$IfOpt Q+}
    {$DEFINE FCXQNO}
    {$Q-}
  {$Else}
    {$UNDEF FCXQNO}
  {$EndIf}
  // this hash function is rather fast and effective
  if TfcxBaseUniqueValues(FOwner).IgnoreCase then
    while PAnsiChar(AValue)^ <> #0 do
    begin
      {$IFNDEF fpc}
{$IFNDEF FMX}
      Result := 31 * Result + Byte(CharUpperA(PAnsiChar(PAnsiChar(AValue)^)));
{$ELSE FMX}
    {$IFDEF MSWINDOWS}
      Result := 31 * Result + Byte(CharUpperA(PAnsiChar(PAnsiChar(AValue)^)));
    {$ELSE MSWINDOWS}
      Result := 31 * Result + Byte(AnsiStrUpper(PAnsiChar(PAnsiChar(AValue)^)));
    {$ENDIF MSWINDOWS}
{$ENDIF FMX}
      {$else}
      Result := 31 * Result + Byte(AnsiStrUpper(PAnsiChar(PAnsiChar(AValue)^)));
      {$endif}
      AValue := PAnsiChar(AValue) + 1;
    end
  else
    while PAnsiChar(AValue)^ <> #0 do
    begin
      Result := 31 * Result + Byte(PAnsiChar(AValue)^);
      AValue := PAnsiChar(AValue) + 1;
    end;
  {$IFDEF FCXQNO}
    {$Q+}
  {$EndIf}
  Result := Result mod FHashBucketSize;
end;

function TfcxStringDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  PfcxStringUV(result).Value := ReadPChar(ACubeStream);
end;

function TfcxStringDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  Result := ReadPChar(ACubeStream);
end;

function TfcxStringDTP.NewUValue(AValue: pointer): pointer;
begin
// вот эта строка позволит сократить расходы на память
  if AValue <> nil then
    ReallocMem(AValue, strlen(PAnsiChar(AValue)) + 1);
  Result := inherited NewUValue(AValue)
end;

class function TfcxStringDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := String(PAnsiChar(APointer));
    try
      Result := VarAsType(Result, DataTypeToVarType[DataType]);
    except
      Result := Null
    end;
end;

class procedure TfcxStringDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
{ TODO -cНеобходимо : Проверить правильность!!!}
  WritePChar(ACubeStream, PfcxStringUV(AUValue).Value);
end;

procedure TfcxStringDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
var
  S: ansistring;
begin
  if TVarData(AValue).VType > varNull then
  begin
    try
      S := ansistring(AValue);
    except
      PfcxStringUV(AUValue).Value := nil;
      exit;
    end;
    GetMem(PfcxStringUV(AUValue).Value, (Length(S) + 1) * SizeOf(AnsiChar));
    try
      StrPCopy(PfcxStringUV(AUValue).Value, S);
    except
      FreeMem(PfcxStringUV(AUValue).Value);
      PfcxStringUV(AUValue).Value := nil;
    end;
  end
  else
    PfcxStringUV(AUValue).Value := nil;
end;

class function TfcxStringDTP.ToPointer(AValue: Variant): pointer;
var
  S: ansistring;
begin
  if TVarData(AValue).VType > varNull then
  begin
    try
      S := ansistring(AValue);
    except
      Result := nil;
      exit;
    end;
    GetMem(Result, (Length(S) + 1) * SizeOf(AnsiChar));
    try
      StrPCopy(Result, S);
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxWideStringDTP }

function TfcxWideStringDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
  begin
    Result := WideString(PfcxWideStringUV(AUValue).Value);
    try
      Result := VarAsType(Result, DataTypeToVarType[DataType]);
    except
      Result := Null
    end;
  end;
end;

function TfcxWideStringDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
var
  StrVal1: PfcxWideStringUV absolute AUValue1;
  StrVal2: PfcxWideStringUV absolute AUValue2;
begin
// AUValue1 and AUValue2 can not be null
{$IFNDEF fpc}
  {$IFNDEF FMX}
    if TfcxBaseUniqueValues(FOwner).IgnoreCase then
      Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
        -1, StrVal2.Value, -1) - 2
    else
      Result := CompareStringW(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
        -1, StrVal2.Value, -1) - 2
  {$ELSE FMX}
    {$IFDEF MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
        Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
          -1, StrVal2.Value, -1) - 2
      else
        Result := CompareStringW(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
          -1, StrVal2.Value, -1) - 2
    {$ELSE MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
        Result := WideCompareText(StrVal1.Value, StrVal2.Value)
      else
        Result := WideCompareStr(StrVal1.Value, StrVal2.Value);
    {$ENDIF MSWINDOWS}
  {$ENDIF FMX}
{$ELSE}
  if TfcxBaseUniqueValues(FOwner).IgnoreCase then
    Result := WideCompareText(StrVal1.Value, StrVal2.Value)
  else
    Result := WideCompareStr(StrVal1.Value, StrVal2.Value);
{$ENDIF}
end;

function TfcxWideStringDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
var
  StrVal1: PfcxWideStringUV absolute AUValue1;
begin
// AUValue1 and AValue2 can not be null
{$IFNDEF fpc}
  {$IFNDEF FMX}
    if TfcxBaseUniqueValues(FOwner).IgnoreCase then
      Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
        -1, AValue2, -1) - 2
    else
      Result := CompareStringW(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
        -1, AValue2, -1) - 2;
  {$ELSE FMX}
    {$IFDEF MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
        Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, StrVal1.Value,
          -1, AValue2, -1) - 2
      else
        Result := CompareStringW(LOCALE_USER_DEFAULT, 0 + SORT_STRINGSORT, StrVal1.Value,
          -1, AValue2, -1) - 2;
    {$ELSE MSWINDOWS}
      if TfcxBaseUniqueValues(FOwner).IgnoreCase then
        Result := WideCompareText(StrVal1.Value, PWideChar(AValue2))
      else
        Result := WideCompareStr(StrVal1.Value, PWideChar(AValue2));
    {$ENDIF MSWINDOWS}
  {$ENDIF FMX}
{$ELSE}
  if TfcxBaseUniqueValues(FOwner).IgnoreCase then
    Result := WideCompareText(StrVal1.Value, PWideChar(AValue2))
  else
    Result := WideCompareStr(StrVal1.Value, PWideChar(AValue2));
{$ENDIF}
end;

class function TfcxWideStringDTP.DataType: TfcxDataType;
begin
  Result := fcdt_WideString;
end;

function TfcxWideStringDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := 0;
  {$IfOpt Q+}
    {$DEFINE FCXQNO}
    {$Q-}
  {$Else}
    {$UNDEF FCXQNO}
  {$EndIf}
  // this hash function is rather fast and effective
  if TfcxBaseUniqueValues(FOwner).IgnoreCase then
    while PWideChar(AValue)^ <> #0 do
    begin
      {$IFNDEF fpc}
{$IFNDEF FMX}
      Result := 31 * Result + Word(CharUpperW(PWideChar(PWideChar(AValue)^)));
{$ELSE FMX}
    {$IFDEF MSWINDOWS}
      Result := 31 * Result + Word(CharUpperW(PWideChar(PWideChar(AValue)^)));
    {$ELSE MSWINDOWS}
      Result := 31 * Result + Word(WideUpperCase(PWideChar(PWideChar(AValue)^))[1]);
    {$ENDIF MSWINDOWS}
{$ENDIF FMX}
      {$else}
      Result := 31 * Result + Word(WideUpperCase(PWideChar(PWideChar(AValue)^))[1]);
      {$endif}
      AValue := PWideChar(AValue) + 1;
    end
  else
    while PWideChar(AValue)^ <> #0 do
    begin
      Result := 31 * Result + Word(PWideChar(AValue)^);
      AValue := PWideChar(AValue) + 1;
    end;
  {$IFDEF FCXQNO}
    {$Q+}
  {$EndIf}
  Result := Result mod FHashBucketSize;
end;

function TfcxWideStringDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  PfcxWideStringUV(result).Value := ReadPWideChar(ACubeStream);
end;

function TfcxWideStringDTP.LoadValueFromStream(
  ACubeStream: TStream): pointer;
begin
  Result := ReadPWideChar(ACubeStream);
end;

function TfcxWideStringDTP.NewUValue(AValue: pointer): pointer;
begin
// вот эта строка позволит сократить расходы на память
{
  if AValue <> nil then
    ReallocMem(AValue, strlen(PWideChar(AValue)) + 1);
}
  Result := inherited NewUValue(AValue)
end;

class function TfcxWideStringDTP.PointerAsVariant(
  APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := WideString(PWideChar(APointer));
    try
      Result := VarAsType(Result, DataTypeToVarType[DataType]);
    except
      Result := Null
    end;
end;

class procedure TfcxWideStringDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  WritePWideChar(ACubeStream, PfcxWideStringUV(AUValue).Value);
end;

procedure TfcxWideStringDTP.SetVariantValue(AUValue: pointer;
  AValue: Variant);
var
  S: widestring;
begin
  if TVarData(AValue).VType > varNull then
  begin
    try
      S := widestring(AValue);
    except
      PfcxWideStringUV(AUValue).Value := nil;
      exit;
    end;
    GetMem(PfcxWideStringUV(AUValue).Value, (Length(S) + 1) * SizeOf(WideChar));
    try
      WStrPCopy(PfcxWideStringUV(AUValue).Value, S);
    except
      FreeMem(PfcxWideStringUV(AUValue).Value);
      PfcxWideStringUV(AUValue).Value := nil;
    end;
  end
  else
    PfcxStringUV(AUValue).Value := nil;
end;

class function TfcxWideStringDTP.ToPointer(AValue: Variant): pointer;
var
  S: widestring;
begin
  if TVarData(AValue).VType > varNull then
  begin
    try
      S := widestring(AValue);
    except
      Result := nil;
      exit;
    end;
    GetMem(Result, (Length(S) + 1) * SizeOf(WideChar));
    try
      WStrPCopy(Result, S);
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxBooleanDTP }

function TfcxBooleanDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxBoolUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxBooleanDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  if PfcxBoolUV(AUValue1).Value = PfcxBoolUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxBoolUV(AUValue1).Value > PfcxBoolUV(AUValue2).Value];
end;

function TfcxBooleanDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  if PfcxBoolUV(AUValue1).Value = PWordBool(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxBoolUV(AUValue1).Value > PWordBool(AValue2)^];
end;

class function TfcxBooleanDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Boolean;
end;

function TfcxBooleanDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxBoolUV)
end;

function TfcxBooleanDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := PWord(AValue)^ mod FHashBucketSize;
end;

function TfcxBooleanDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxBoolUV(result).Value, SizeOf(Boolean));
end;

function TfcxBooleanDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Boolean));
  ACubeStream.Read(Result^, SizeOf(Boolean));
end;

function TfcxBooleanDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxBoolUV(Result).Value := False
  else
    PfcxBoolUV(Result).Value := PWordBool(AValue)^;
  FreeMem(AValue);
end;

class function TfcxBooleanDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PWordBool(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxBooleanDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxBoolUV(AUValue).Value, SizeOf(Boolean));
end;

procedure TfcxBooleanDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxBoolUV(AUValue).Value := AValue
    except
      PfcxBoolUV(AUValue).Value := False;
    end
  else
    PfcxBoolUV(AUValue).Value := False;
end;

class function TfcxBooleanDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(WordBool));
    try
      PWordBool(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxPointerDTP }

procedure TfcxPointerDTP.FreeUValue(AUValue: pointer);
begin
  FreeMem(PfcxPointerUV(AUValue).Value);
  inherited;
end;

function TfcxPointerDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxPointerUV);
end;

function TfcxPointerDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxPointerUV(Result).Value := nil
  else
    PfcxPointerUV(Result).Value := AValue;
end;

{ TfcxDateDTP }

function TfcxDateDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := DateToDateTime(PfcxIntegerUV(AUValue).Value);
end;

class function TfcxDateDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Date;
end;

class function TfcxDateDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := DateToDateTime(PInteger(APointer)^);
end;

procedure TfcxDateDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxIntegerUV(AUValue).Value := GetTimeStampDate(AValue)
    except
      PfcxIntegerUV(AUValue).Value := 0;
    end
  else
    PfcxIntegerUV(AUValue).Value := 0;
end;

class function TfcxDateDTP.ToPointer(AValue: Variant): pointer;
begin
  GetMem(Result, SizeOf(Integer));
  PInteger(Result)^ := GetTimeStampDate(AValue);
end;

{ TfcxTimeDTP }

function TfcxTimeDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := null
  else
    Result := TimeToDateTime(PfcxIntegerUV(AUValue).Value);
end;

class function TfcxTimeDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Time;
end;

class function TfcxTimeDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := null
  else
    Result := TimeToDateTime(PInteger(APointer)^);
end;

procedure TfcxTimeDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxIntegerUV(AUValue).Value := GetTimeStampTime(AValue)
    except
      PfcxIntegerUV(AUValue).Value := 0;
    end
  else
    PfcxIntegerUV(AUValue).Value := 0;
end;

class function TfcxTimeDTP.ToPointer(AValue: Variant): pointer;
begin
  GetMem(Result, SizeOf(Integer));
  PInteger(Result)^ := GetTimeStampTime(AValue);
end;

{ TfcxShortIntegerDTP }

function TfcxShortIntegerDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxShortIntUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxShortIntegerDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  if PfcxShortIntUV(AUValue1).Value = PfcxShortIntUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxShortIntUV(AUValue1).Value > PfcxShortIntUV(AUValue2).Value];
end;

function TfcxShortIntegerDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  if PfcxShortIntUV(AUValue1).Value = PShortInt(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxShortIntUV(AUValue1).Value > PShortInt(AValue2)^];
end;

class function TfcxShortIntegerDTP.DataType: TfcxDataType;
begin
  Result := fcdt_ShortInteger;
end;

function TfcxShortIntegerDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxShortIntUV)
end;

function TfcxShortIntegerDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := abs(PShortInt(AValue)^ mod FHashBucketSize);
end;

function TfcxShortIntegerDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxShortIntUV(result).Value, SizeOf(ShortInt));
end;

function TfcxShortIntegerDTP.LoadValueFromStream(
  ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(ShortInt));
  ACubeStream.Read(Result^, SizeOf(ShortInt));
end;

function TfcxShortIntegerDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxShortIntUV(Result).Value := 0
  else
    PfcxShortIntUV(Result).Value := PShortInt(AValue)^;
  FreeMem(AValue);
end;

class function TfcxShortIntegerDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PShortInt(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxShortIntegerDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxShortIntUV(AUValue).Value, SizeOf(ShortInt));
end;

procedure TfcxShortIntegerDTP.SetVariantValue(AUValue: pointer;
  AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxShortIntUV(AUValue).Value := AValue
    except
      PfcxShortIntUV(AUValue).Value := 0;
    end
  else
    PfcxShortIntUV(AUValue).Value := 0;
end;

class function TfcxShortIntegerDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(ShortInt));
    try
      PShortInt(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxSmallIntDTP }

function TfcxSmallIntDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxSmallIntUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxSmallIntDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  if PfcxSmallIntUV(AUValue1).Value = PfcxSmallIntUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxSmallIntUV(AUValue1).Value > PfcxSmallIntUV(AUValue2).Value];
end;

function TfcxSmallIntDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  if PfcxSmallIntUV(AUValue1).Value = PSmallInt(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxSmallIntUV(AUValue1).Value > PSmallInt(AValue2)^];
end;

class function TfcxSmallIntDTP.DataType: TfcxDataType;
begin
  Result := fcdt_SmallInteger;
end;

function TfcxSmallIntDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxSmallIntUV)
end;

function TfcxSmallIntDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := abs(PSmallInt(AValue)^ mod FHashBucketSize);
end;

function TfcxSmallIntDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxSmallIntUV(result).Value, SizeOf(SmallInt));
end;

function TfcxSmallIntDTP.LoadValueFromStream(
  ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(SmallInt));
  ACubeStream.Read(Result^, SizeOf(SmallInt));
end;

function TfcxSmallIntDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxSmallIntUV(Result).Value := 0
  else
    PfcxSmallIntUV(Result).Value := PSmallInt(AValue)^;
  FreeMem(AValue);
end;

class function TfcxSmallIntDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PSmallInt(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxSmallIntDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxSmallIntUV(AUValue).Value, SizeOf(SmallInt));
end;

procedure TfcxSmallIntDTP.SetVariantValue(AUValue: pointer;
  AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxSmallIntUV(AUValue).Value := AValue
    except
      PfcxSmallIntUV(AUValue).Value := 0;
    end
  else
    PfcxSmallIntUV(AUValue).Value := 0;
end;

class function TfcxSmallIntDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(SmallInt));
    try
      PSmallInt(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxLargeIntDTP }

function TfcxLargeIntDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
{$IFDEF DELPHI_6UP}
    Result := VarAsType(PfcxLargeIntUV(AUValue).Value, DataTypeToVarType[DataType]);
{$ELSE}
    Result := Null;
{$ENDIF}
end;

function TfcxLargeIntDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  if PfcxLargeIntUV(AUValue1).Value = PfcxLargeIntUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxLargeIntUV(AUValue1).Value > PfcxLargeIntUV(AUValue2).Value];
end;

function TfcxLargeIntDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  if PfcxLargeIntUV(AUValue1).Value = PInt64(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxLargeIntUV(AUValue1).Value > PInt64(AValue2)^];
end;

class function TfcxLargeIntDTP.DataType: TfcxDataType;
begin
  Result := fcdt_LargeInteger;
end;

function TfcxLargeIntDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxLargeIntUV)
end;

function TfcxLargeIntDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := Abs(PInt64(AValue)^ mod FHashBucketSize);
end;

function TfcxLargeIntDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxLargeIntUV(result).Value, SizeOf(Int64));
end;

function TfcxLargeIntDTP.LoadValueFromStream(
  ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Int64));
  ACubeStream.Read(Result^, SizeOf(Int64));
end;

function TfcxLargeIntDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxLargeIntUV(Result).Value := 0
  else
    PfcxLargeIntUV(Result).Value := PInt64(AValue)^;
  FreeMem(AValue);
end;

class function TfcxLargeIntDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
{$IFDEF DELPHI_6UP}
    Result := VarAsType(PInt64(APointer)^, DataTypeToVarType[DataType]);
{$ELSE}
    Result := Null;
{$ENDIF}
end;

class procedure TfcxLargeIntDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxLargeIntUV(AUValue).Value, SizeOf(Int64));
end;

procedure TfcxLargeIntDTP.SetVariantValue(AUValue: pointer;
  AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
{$IFDEF DELPHI_6UP}
      PfcxLargeIntUV(AUValue).Value := AValue
{$ELSE}
      PfcxLargeIntUV(AUValue).Value := 0
{$ENDIF}
    except
      PfcxLargeIntUV(AUValue).Value := 0;
    end
  else
    PfcxLargeIntUV(AUValue).Value := 0;
end;

class function TfcxLargeIntDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
{$IFDEF DELPHI_6UP}
    GetMem(Result, SizeOf(Int64));
    try
      PInt64(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
{$ELSE}
    Result := nil;
{$ENDIF}
  end
  else
    Result := nil;
end;

{ TfcxDoubleDTP }

function TfcxDoubleDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxDoubleUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxDoubleDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  if PfcxDoubleUV(AUValue1).Value = PfcxDoubleUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxDoubleUV(AUValue1).Value > PfcxDoubleUV(AUValue2).Value];
end;

function TfcxDoubleDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  if PfcxDoubleUV(AUValue1).Value = PDouble(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxDoubleUV(AUValue1).Value > PDouble(AValue2)^];
end;

class function TfcxDoubleDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Double;
end;

function TfcxDoubleDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxDoubleUV)
end;

function TfcxDoubleDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := Abs(PInt64(AValue)^ mod FHashBucketSize);
end;

function TfcxDoubleDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxDoubleUV(result).Value, SizeOf(Double));
end;

function TfcxDoubleDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Double));
  ACubeStream.Read(Result^, SizeOf(Double));
end;

function TfcxDoubleDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxDoubleUV(Result).Value := 0
  else
    PfcxDoubleUV(Result).Value := PDouble(AValue)^;
  FreeMem(AValue);
end;

class function TfcxDoubleDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PDouble(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxDoubleDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxDoubleUV(AUValue).Value, SizeOf(Double));
end;

procedure TfcxDoubleDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxDoubleUV(AUValue).Value := AValue
    except
      PfcxDoubleUV(AUValue).Value := 0;
    end
  else
    PfcxDoubleUV(AUValue).Value := 0;
end;

class function TfcxDoubleDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(Double));
    try
      PDouble(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{ TfcxSingleDTP }

function TfcxSingleDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := VarAsType(PfcxSingleUV(AUValue).Value, DataTypeToVarType[DataType]);
end;

function TfcxSingleDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  if PfcxSingleUV(AUValue1).Value = PfcxSingleUV(AUValue2).Value then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxSingleUV(AUValue1).Value > PfcxSingleUV(AUValue2).Value];
end;

function TfcxSingleDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  if PfcxSingleUV(AUValue1).Value = PSingle(AValue2)^ then
    Result := 0
  else
    Result := fcxCompareHelper[PfcxSingleUV(AUValue1).Value > PSingle(AValue2)^];
end;

class function TfcxSingleDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Single;
end;

function TfcxSingleDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxSingleUV)
end;

function TfcxSingleDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := Abs(PInt64(AValue)^ mod FHashBucketSize);
end;

function TfcxSingleDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxSingleUV(result).Value, SizeOf(Single));
end;

function TfcxSingleDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(Single));
  ACubeStream.Read(Result^, SizeOf(Single));
end;

function TfcxSingleDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxSingleUV(Result).Value := 0
  else
    PfcxSingleUV(Result).Value := PSingle(AValue)^;
  FreeMem(AValue);
end;

class function TfcxSingleDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := VarAsType(PSingle(APointer)^, DataTypeToVarType[DataType]);
end;

class procedure TfcxSingleDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxSingleUV(AUValue).Value, SizeOf(Single));
end;

procedure TfcxSingleDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxSingleUV(AUValue).Value := AValue
    except
      PfcxSingleUV(AUValue).Value := 0;
    end
  else
    PfcxSingleUV(AUValue).Value := 0;
end;

class function TfcxSingleDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(Single));
    try
      PSingle(Result)^ := AValue;
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{$IFDEF SQL_TYPES_EXTRA0}
{ TfcxBCDDTP }

function TfcxBCDDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
{
  if DataTypeToVarType[DataType] = varFMTBcd then
    Result := varFMTBcdCreate(PfcxBCDUV(AUValue).Value)
  else
}
    Result := BcdToDouble(PfcxBCDUV(AUValue).Value);
end;

function TfcxBCDDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  Result := BcdCompare(PfcxBCDUV(AUValue1).Value, PfcxBCDUV(AUValue2).Value);
end;

function TfcxBCDDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  Result := BcdCompare(PfcxBCDUV(AUValue1).Value, PBCD(AValue2)^);
end;

class function TfcxBCDDTP.DataType: TfcxDataType;
begin
  Result := fcdt_BCD;
end;

function TfcxBCDDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxBCDUV)
end;

function TfcxBCDDTP.Hash(AValue: Pointer): cardinal;
var
  D: Double;
begin
  D := BCDToDouble(PBCD(AValue)^);
  Result := Abs(PInt64(@D)^ mod FHashBucketSize);
end;

function TfcxBCDDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxBCDUV(result).Value, SizeOf(TBCD));
end;

function TfcxBCDDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(TBCD));
  ACubeStream.Read(Result^, SizeOf(TBCD));
end;

function TfcxBCDDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxBCDUV(Result).Value := VarToBcd(0)
  else
    PfcxBCDUV(Result).Value := PBCD(AValue)^;
  FreeMem(AValue);
end;

class function TfcxBCDDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := BcdToDouble(PBCD(APointer)^);
end;

class procedure TfcxBCDDTP.SaveToStream(AUValue: pointer;
  ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxBCDUV(AUValue).Value, SizeOf(TBCD));
end;

procedure TfcxBCDDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
    try
      PfcxBCDUV(AUValue).Value := VarToBcd(AValue)
    except
      PfcxBCDUV(AUValue).Value := VarToBcd(0);
    end
  else
    PfcxBCDUV(AUValue).Value := VarToBcd(0);
end;

class function TfcxBCDDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(TBCD));
    try
      PBCD(Result)^ := VarToBcd(AValue);
    except
      FreeMem(Result);
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

{$ENDIF}

{$IFDEF SQL_TYPES_EXTRA1}
{ TfcxTimeStampDTP }

function CompareSQLTimeStamp(const Value1, Value2: TSQLTimeStamp): integer;
var
  Status: Integer;
begin
  Status := Value1.Year - Value2.Year;
  if Status = 0 then
  begin
    Status := Value1.Month - Value2.Month;
    if Status = 0 then
    begin
      Status := Value1.Day - Value2.Day;
      if Status = 0 then
      begin
        Status := Value1.Hour - Value2.Hour;
        if Status = 0 then
        begin
          Status := Value1.Minute - Value2.Minute;
          if Status = 0 then
          begin
            Status := Value1.Second - Value2.Second;
            if Status = 0 then
            begin
              Status := Value1.Fractions - Value2.Fractions;
            end;
          end;
        end;
      end;
    end;
  end;
  if Status = 0 then
    Result := 0
  else
  if Status > 0 then
    Result := 1
  else
    Result := -1;
end;

function TfcxTimeStampDTP.AsVariant(AUValue: pointer): Variant;
begin
  if IsNull(AUValue) then
    Result := Null
  else
    Result := varSQLTimeStampCreate(PfcxTimeStampUV(AUValue).Value);
end;

function TfcxTimeStampDTP.Compare(const AUValue1, AUValue2: Pointer): Integer;
begin
  Result := CompareSQLTimeStamp(PfcxTimeStampUV(AUValue1).Value, PfcxTimeStampUV(AUValue2).Value);
end;

function TfcxTimeStampDTP.CompareForInsert(const AUValue1, AValue2: Pointer): Integer;
begin
  Result := CompareSQLTimeStamp(PfcxTimeStampUV(AUValue1).Value, PSQLTimeStamp(AValue2)^);
end;

class function TfcxTimeStampDTP.DataType: TfcxDataType;
begin
  Result := fcdt_TimeStamp;
end;

function TfcxTimeStampDTP.GetSize: Integer;
begin
  Result := SizeOf(TfcxTimeStampUV)
end;

function TfcxTimeStampDTP.Hash(AValue: Pointer): cardinal;
begin
  Result := (PSQLTimeStamp(AValue)^.Year * 12 * 31 + PSQLTimeStamp(AValue)^.Month * 31 + PSQLTimeStamp(AValue)^.Day) mod FHashBucketSize;
end;

function TfcxTimeStampDTP.LoadFromStream(ACubeStream: TStream): pointer;
begin
  Result := inherited LoadFromStream(ACubeStream);
  ACubeStream.Read(PfcxTimeStampUV(result).Value, SizeOf(TSQLTimeStamp));
end;

function TfcxTimeStampDTP.LoadValueFromStream(ACubeStream: TStream): pointer;
begin
  GetMem(Result, SizeOf(TSQLTimeStamp));
  ACubeStream.Read(Result^, SizeOf(TSQLTimeStamp));
end;

function TfcxTimeStampDTP.NewUValue(AValue: pointer): pointer;
begin
  Result := inherited NewUValue(AValue);
  if AValue = nil then
    PfcxTimeStampUV(Result).Value := NullSQLTimeStamp
  else
    PfcxTimeStampUV(Result).Value := PSQLTimeStamp(AValue)^;
  FreeMem(AValue);
end;

class function TfcxTimeStampDTP.PointerAsVariant(APointer: pointer): Variant;
begin
  if APointer = nil then
    Result := Null
  else
    Result := varSQLTimeStampCreate(PSQLTimeStamp(APointer)^);
end;

class procedure TfcxTimeStampDTP.SaveToStream(AUValue: pointer; ACubeStream: TStream);
begin
  inherited;
  ACubeStream.Write(PfcxTimeStampUV(AUValue).Value, SizeOf(TSQLTimeStamp));
end;

procedure TfcxTimeStampDTP.SetVariantValue(AUValue: pointer; AValue: Variant);
begin
  if TVarData(AValue).VType > varNull then
  begin
    try
      PfcxTimeStampUV(AUValue).Value := VarToSQLTimeStamp(AValue);
    except
      PfcxTimeStampUV(AUValue).Value := NullSQLTimeStamp;
    end;
  end
  else
    PfcxTimeStampUV(AUValue).Value := NullSQLTimeStamp;
end;

class function TfcxTimeStampDTP.ToPointer(AValue: Variant): pointer;
begin
  if TVarData(AValue).VType > varNull then
  begin
    GetMem(Result, SizeOf(TSQLTimeStamp));
    PSQLTimeStamp(Result)^ := VarToSQLTimeStamp(AValue);
  end
  else
    Result := nil;
end;
{$ENDIF}

{ TfcxCurrencyDTP }

class function TfcxCurrencyDTP.DataType: TfcxDataType;
begin
  Result := fcdt_Currency;
end;

{ TfcxCommonDataTypeProcessor }

constructor TfcxCommonDataTypeProcessor.Create(AOwner: TObject);
begin
  FOwner := AOwner;
  FDisplayFormat:= nil;
end;

destructor TfcxCommonDataTypeProcessor.Destroy;
begin
  FDisplayFormat.Free;
  inherited;
end;

function TfcxCommonDataTypeProcessor.GetDisplayFormat: TfcxFormat;
begin
  if FDisplayFormat <> nil then
    Result := FDisplayFormat
  else
    Result := DefaultDisplayFormat;
end;

procedure TfcxCommonDataTypeProcessor.SetDisplayFormat(
  const Value: TfcxFormat);
begin
  if DisplayFormat.Equal(Value) then
    Exit;
  if FDisplayFormat = nil then
  begin
    FDisplayFormat := TfcxFormat.Create;
    FDisplayFormat.Assign(Value);
  end
  else
  begin
    if DefaultDisplayFormat.Equal(Value) then
    begin
      FDisplayFormat.Free;
      FDisplayFormat := nil;
    end
    else
    begin
      FDisplayFormat.Assign(Value);
    end
  end
end;

{ TfcxCommonDatePathDTP }

procedure TfcxCommonDatePathDTP.AddPathValue(AValue, AUVIndex: integer);
begin
  if (FHashTable[AValue shr 3] and BitmapSet[AValue mod 8]) = 0 then
  begin
    inc(FCount);
    FHashTable[AValue shr 3] := FHashTable[AValue shr 3] or BitmapSet[AValue mod 8];
    if (FMinNotNull > AValue) or (FMinNotNull <= 0) then
      FMinNotNull := AValue;
    if AValue = 0 then // null value
    begin
      if not FHasNull then
      begin
        inc(FCount);
        FHasNull := True;
      end
    end
  end;
end;

function TfcxCommonDatePathDTP.AsVariant(AValue: Integer): Variant;
begin
  if AValue = 0 then
    Result := Unassigned
  else
    Result := AValue;
end;

constructor TfcxCommonDatePathDTP.Create(AOwner: TObject);
begin
  FDateType := TfcxCommonDatePathField(AOwner).DateType;
  FHashBucketSize := cfcHashSizeOfDatePath[FDateType];
  inherited;
//  FDisplayFormat := TfcxFormat.Create(FOwner, TfcxCommonDatePathField(FOwner).MasterField.Cube.Formats.DefaultFormatByType(fcdt_Word));
end;

function TfcxCommonDatePathDTP.GetCaption(AValue: Integer): TfcxString;
begin
  if AValue = 0 then
    Result := NullCaption
  else
  begin
    Result := DisplayFormat.FormatDatePath(Word(AValue), FDateType, TfcxCommonDatePathField(FOwner).MasterField.Cube.DateTimeConsts);
  end
end;

function TfcxCommonDatePathDTP.GetDefaultDisplayFormat: TfcxFormat;
begin
  Result := TfcxCommonDatePathField(FOwner).MasterField.Cube.Formats.DefaultFormatByType(fcdt_Word);
end;

function TfcxCommonDatePathDTP.GetIsNullAtIndex(AIndex: integer): boolean;
begin
  Result := (ValueAtIndex[AIndex] = 0);
end;

function TfcxCommonDatePathDTP.GetSize: Integer;
begin
  Result := cfcSizeOfDatePath[FDateType];
end;

function TfcxCommonDatePathDTP.GetUVIndex(AUVVarValue: Variant): integer;
begin
  if (AUVVarValue = Unassigned) or (AUVVarValue = Null) then
    AUVVarValue := 0;
  if not FindIndexAtValue(AUVVarValue, Result) then
    Result := -1;
end;

{ TfcxDatePathWordDTP }

procedure TfcxDatePathWordDTP.AddPathValue(AValue: integer;
  AUVIndex: integer);
begin
  inherited;
  PfcxWordArray(FPathValues)[AUVIndex] := Word(AValue);
end;

procedure TfcxDatePathWordDTP.CreatePathUVsArray;
var
  i, i1, j, AStartOctet, ADelta: Integer;
begin
  GetMem(FUValues, FCount * FSize);
  j := 0;
  if j = FCount then // нашли все значения
  begin
    inherited CreatePathUVsArray;
    Exit;
  end;
  AStartOctet := FMinNotNull shr 3;
  if AStartOctet = 0 then
    AStartOctet := 1;
  i := 0;
// сперва проверка октета с элементом NULL
  if FHashTable[i] > 0 then // обработаем только ненулевые октеты
  begin
    for i1 := 0 to 7 do
    begin
      if (FHashTable[i] and BitmapSet[i1]) <> 0 then
      begin
        PfcxWordArray(FUValues)[j] := word(i1);
        inc(j);
        if j = FCount then // нашли все значения
        begin
          inherited CreatePathUVsArray;
          Exit;
        end;
      end;
    end;
  end;
  for i := AStartOctet to cfcHashSizeOfDatePath[FDateType] - 1 do
  begin
    if FHashTable[i] > 0 then // обработаем только ненулевые октеты
    begin
      ADelta := i shl 3;
      for i1 := 0 to 7 do
      begin
        if (FHashTable[i] and BitmapSet[i1]) <> 0 then
        begin
          PfcxWordArray(FUValues)[j] := word(ADelta + i1);
          inc(j);
          if j = FCount then // нашли все значения
          begin
            inherited CreatePathUVsArray;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited CreatePathUVsArray;
end;

procedure TfcxDatePathWordDTP.DeleteValue(AUVIndex: integer);
begin
// если делать счетчик ссылок, то тогда надо будет убирать и само УЗ части
  if AUVIndex < (TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxWordArray(FPathValues)[AUVIndex + 1], PfcxWordArray(FPathValues)[AUVIndex],
      (TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
//   ReallocMem(FPathValues, GetSize * Cardinal(TfcxCommonUVField(TfcxCommonDatePathField(FOwner).MasterField).UniqueValues.Capacity));
end;

function TfcxDatePathWordDTP.FindIndexAtValue(AValue: integer;
  var AIndex: integer): Boolean;
var
  R, I: integer;
begin
  AIndex := 0;
  Result := False;
  if (FCount = 0) then
    Exit;
  if AValue = 0 then
  begin
    if (PfcxWordArray(FUValues)[0] = 0) then
    begin
      // Ну вот и нашли
      Result := True;
      Exit;
    end;
  end
  else
  begin
    R := FCount - 1;
    if (PfcxWordArray(FUValues)[0] = 0) then
      AIndex := 1;
    while AIndex <= R do
    begin
      I := (AIndex + R) shr 1; // middle
      if PfcxWordArray(FUValues)[I] < AValue then
        AIndex := I + 1
      else if PfcxWordArray(FUValues)[I] > AValue then
        R := I - 1
      else
      begin
        Result := True; // Ну вот и нашли
        AIndex := I;
        Exit;
      end;
    end;
  end;
end;

function TfcxDatePathWordDTP.GetValueAtBaseUVIndex(
  ABaseUVIndex: integer): integer;
begin
  Result := PfcxWordArray(FPathValues)[ABaseUVIndex];
end;

function TfcxDatePathWordDTP.GetValueAtIndex(AIndex: integer): integer;
begin
  Result := PfcxWordArray(FUValues)[AIndex];
end;

procedure TfcxDatePathWordDTP.InsertPathValue(AValue, AUVIndex: integer);
var
  AIndex: integer;
begin
  PfcxWordArray(FPathValues)[AUVIndex] := Word(AValue);
  if not FindIndexAtValue(AValue, AIndex) then
  begin
    if AValue = 0 then // null value
      FHasNull := True;
    inc(FCount);
    ReallocMem(FUValues, FCount * FSize);
    if AIndex < (FCount - 1) then
      System.Move(PfcxWordArray(FUValues)[AIndex], PfcxWordArray(FUValues)[AIndex + 1],
        (FCount - 1 - AIndex) * FSize);
    PfcxWordArray(FUValues)[AIndex] := Word(AValue);
  end;
end;

procedure TfcxDatePathWordDTP.InsertValue(AUVIndex: integer);
begin
  if AUVIndex < (TfcxCommonUVField(TfcxCommonDatePathField(FOwner).MasterField).UniqueValues.Count - 1) then
    System.Move(PfcxWordArray(FPathValues)[AUVIndex], PfcxWordArray(FPathValues)[AUVIndex + 1],
      (TfcxCommonUVField(TfcxCommonDatePathField(FOwner).MasterField).UniqueValues.Count - 1 - AUVIndex) * FSize);
end;

{ TfcxDatePathByteDTP }

procedure TfcxDatePathByteDTP.AddPathValue(AValue: integer; AUVIndex: integer);
begin
  inherited;
  PfcxByteArray(FPathValues)[AUVIndex] := Byte(AValue);
end;

procedure TfcxDatePathByteDTP.CreatePathUVsArray;
var
  i, i1, j, AStartOctet, ADelta: Integer;
begin
  GetMem(FUValues, FCount * FSize);
  j := 0;
  if j = FCount then // нашли все значения
  begin
    inherited CreatePathUVsArray;
    Exit;
  end;
  AStartOctet := FMinNotNull shr 3;
  if AStartOctet = 0 then
    AStartOctet := 1;
  i := 0;
// сперва проверка октета с элементом NULL
  if FHashTable[i] > 0 then // обработаем только ненулевые октеты
  begin
    for i1 := 0 to 7 do
    begin
      if (FHashTable[i] and BitmapSet[i1]) <> 0 then
      begin
        PfcxByteArray(FUValues)[j] := Byte(i1);
        inc(j);
        if j = FCount then // нашли все значения
        begin
          inherited CreatePathUVsArray;
          Exit;
        end;
      end;
    end;
  end;
  for i := AStartOctet to cfcHashSizeOfDatePath[FDateType] - 1 do
  begin
    if FHashTable[i] > 0 then // обработаем только ненулевые октеты
    begin
      ADelta := i shl 3;
      for i1 := 0 to 7 do
      begin
        if (FHashTable[i] and BitmapSet[i1]) <> 0 then
        begin
          PfcxByteArray(FUValues)[j] := Byte(ADelta + i1);
          inc(j);
          if j = FCount then // нашли все значения
          begin
            inherited CreatePathUVsArray;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited CreatePathUVsArray;
end;

procedure TfcxDatePathByteDTP.DeleteValue(AUVIndex: integer);
begin
// если делать счетчик ссылок, то тогда надо будет убирать и само УЗ части
  if AUVIndex < (TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxByteArray(FPathValues)[AUVIndex + 1], PfcxByteArray(FPathValues)[AUVIndex],
      Integer(TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
//   ReallocMem(FPathValues, GetSize * Cardinal(TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Capacity));
end;

function TfcxDatePathByteDTP.FindIndexAtValue(AValue: integer;
  var AIndex: integer): Boolean;
var
  R, I: integer;
begin
  AIndex := 0;
  Result := False;
  if (FCount = 0) then
    Exit;
  if AValue = 0 then
  begin
    if (PfcxByteArray(FUValues)[0] = 0) then
    begin
      // Ну вот и нашли
      Result := True;
      Exit;
    end;
  end
  else
  begin
    R := FCount - 1;
    if (PfcxByteArray(FUValues)[0] = 0) then
      AIndex := 1;
    while AIndex <= R do
    begin
      I := (AIndex + R) shr 1; // middle
      if PfcxByteArray(FUValues)[I] < AValue then
        AIndex := I + 1
      else if PfcxByteArray(FUValues)[I] > AValue then
        R := I - 1
      else
      begin
        Result := True; // Ну вот и нашли
        AIndex := I;
        Exit;
      end;
    end;
  end;
end;

function TfcxDatePathByteDTP.GetValueAtBaseUVIndex(
  ABaseUVIndex: integer): integer;
begin
  Result := PfcxByteArray(FPathValues)[ABaseUVIndex];
end;

function TfcxDatePathByteDTP.GetValueAtIndex(AIndex: integer): integer;
begin
  Result := PfcxByteArray(FUValues)[AIndex];
end;

procedure TfcxDatePathByteDTP.InsertPathValue(AValue, AUVIndex: integer);
var
  AIndex: integer;
begin
  PfcxByteArray(FPathValues)[AUVIndex] := Byte(AValue);
  if not FindIndexAtValue(AValue, AIndex) then
  begin
    if AValue = 0 then // null value
      FHasNull := True;
    inc(FCount);
    ReallocMem(FUValues, FCount * FSize);
    if AIndex < (FCount - 1) then
      System.Move(PfcxByteArray(FUValues)[AIndex], PfcxByteArray(FUValues)[AIndex + 1],
        (FCount - 1 - AIndex) * FSize);
    PfcxByteArray(FUValues)[AIndex] := Byte(AValue);
  end;
end;

procedure TfcxDatePathByteDTP.InsertValue(AUVIndex: integer);
begin
  if AUVIndex < (TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxByteArray(FPathValues)[AUVIndex], PfcxByteArray(FPathValues)[AUVIndex + 1],
      Integer(TfcxCommonDatePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
end;

{ TfcxCommonTimePathDTP }

procedure TfcxCommonTimePathDTP.AddPathValue(AValue, AUVIndex: integer);
begin
  if AValue = -1 then // null value
  begin
    if not FHasNull then
    begin
      inc(FCount);
      FHasNull := True;
    end
  end
  else
  if (FHashTable[AValue shr 3] and BitmapSet[AValue mod 8]) = 0 then
  begin
    inc(FCount);
    FHashTable[AValue shr 3] := FHashTable[AValue shr 3] or BitmapSet[AValue mod 8];
    if (FMinNotNull > AValue) or (FMinNotNull = -1) then
      FMinNotNull := AValue;
  end;
end;

function TfcxCommonTimePathDTP.AsVariant(AValue: Integer): Variant;
begin
  if AValue = -1 then
    Result := Unassigned
  else
    Result := AValue;
end;

constructor TfcxCommonTimePathDTP.Create(AOwner: TObject);
begin
  FTimeType := TfcxCommonTimePathField(AOwner).TimeType;
  FHashBucketSize := cfcHashSizeOfTimePath[FTimeType];
  inherited;
//  FDisplayFormat := TfcxFormat.Create(FOwner, TfcxCommonTimePathField(FOwner).MasterField.Cube.Formats.DefaultFormatByType(fcdt_Word));
end;

function TfcxCommonTimePathDTP.GetCaption(AValue: Integer): TfcxString;
begin
  if AValue = -1 then
    Result := NullCaption
  else
  begin
    Result := DisplayFormat.FormatTimePath(Word(AValue), FTimeType);
  end
end;

function TfcxCommonTimePathDTP.GetDefaultDisplayFormat: TfcxFormat;
begin
  Result := TfcxCommonTimePathField(FOwner).MasterField.Cube.Formats.DefaultFormatByType(fcdt_Word);
end;

function TfcxCommonTimePathDTP.GetIsNullAtIndex(AIndex: integer): boolean;
begin
  Result := (ValueAtIndex[AIndex] = -1);
end;

function TfcxCommonTimePathDTP.GetSize: Integer;
begin
  Result := cfcSizeOfTimePath[FTimeType];
end;

{ TfcxTimePathWordDTP }

procedure TfcxTimePathSmallintDTP.AddPathValue(AValue: Integer;
  AUVIndex: integer);
begin
  inherited;
  PfcxSmallintArray(FPathValues)[AUVIndex] := Smallint(AValue);
end;

procedure TfcxTimePathSmallintDTP.CreatePathUVsArray;
var
  i, i1, j, AStartOctet, ADelta: Integer;
begin
  GetMem(FUValues, FCount * FSize);
  j := 0;
  if j = FCount then // нашли все значения
  begin
    inherited CreatePathUVsArray;
    Exit;
  end;
// сперва проверка октета с элементом NULL
  if FHasNull then
  begin
    PfcxSmallintArray(FUValues)[j] := -1;
    inc(j);
    if j = FCount then // нашли все значения
    begin
      inherited CreatePathUVsArray;
      Exit;
    end;
  end;
  AStartOctet := FMinNotNull shr 3;
  for i := AStartOctet to cfcHashSizeOfTimePath[FTimeType] - 1 do
  begin
    if FHashTable[i] > 0 then // обработаем только ненулевые октеты
    begin
      ADelta := i shl 3;
      for i1 := 0 to 7 do
      begin
        if (FHashTable[i] and BitmapSet[i1]) <> 0 then
        begin
          PfcxSmallintArray(FUValues)[j] := Smallint(ADelta + i1);
          inc(j);
          if j = FCount then // нашли все значения
          begin
            inherited CreatePathUVsArray;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited CreatePathUVsArray;
end;

procedure TfcxTimePathSmallintDTP.DeleteValue(AUVIndex: integer);
begin
// если делать счетчик ссылок, то тогда надо будет убирать и само УЗ части
  if AUVIndex < (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxSmallintArray(FPathValues)[AUVIndex + 1], PfcxSmallintArray(FPathValues)[AUVIndex],
      (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
//   ReallocMem(FPathValues, GetSize * Cardinal(TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Capacity));
end;

function TfcxTimePathSmallintDTP.FindIndexAtValue(AValue: Integer;
  var AIndex: integer): Boolean;
var
  R, I: integer;
begin
  AIndex := 0;
  Result := False;
  if (FCount = 0) then
    Exit;
  if AValue = -1 then
  begin
    if (PfcxSmallintArray(FUValues)[0] = -1) then
    begin
      // Ну вот и нашли
      Result := True;
      Exit;
    end;
  end
  else
  begin
    R := FCount - 1;
    if (PfcxSmallintArray(FUValues)[0] = -1) then
      AIndex := 1;
    while AIndex <= R do
    begin
      I := (AIndex + R) shr 1; // middle
      if PfcxSmallintArray(FUValues)[I] < AValue then
        AIndex := I + 1
      else if PfcxSmallintArray(FUValues)[I] > AValue then
        R := I - 1
      else
      begin
        Result := True; // Ну вот и нашли
        AIndex := I;
        Exit;
      end;
    end;
  end;
end;

function TfcxTimePathSmallintDTP.GetValueAtBaseUVIndex(
  ABaseUVIndex: integer): Integer;
begin
  Result := PfcxSmallintArray(FPathValues)[ABaseUVIndex];
end;

function TfcxTimePathSmallintDTP.GetValueAtIndex(AIndex: integer): Integer;
begin
  Result := PfcxSmallintArray(FUValues)[AIndex];
end;

procedure TfcxTimePathSmallintDTP.InsertPathValue(AValue,
  AUVIndex: integer);
var
  AIndex: integer;
begin
  PfcxSmallintArray(FPathValues)[AUVIndex] := Smallint(AValue);
  if not FindIndexAtValue(AValue, AIndex) then
  begin
    if AValue = -1 then // null value
      FHasNull := True;
    inc(FCount);
    ReallocMem(FUValues, FCount * FSize);
    if AIndex < (FCount - 1) then
      System.Move(PfcxSmallintArray(FUValues)[AIndex], PfcxSmallintArray(FUValues)[AIndex + 1],
        (FCount - 1 - AIndex) * FSize);
    PfcxSmallintArray(FUValues)[AIndex] := Smallint(AValue);
  end;
end;

procedure TfcxTimePathSmallintDTP.InsertValue(AUVIndex: integer);
begin
  if AUVIndex < (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxSmallintArray(FPathValues)[AUVIndex], PfcxSmallintArray(FPathValues)[AUVIndex + 1],
      (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
end;

{ TfcxTimePathByteDTP }

procedure TfcxTimePathShortintDTP.AddPathValue(AValue: Integer; AUVIndex: integer);
begin
  inherited;
  PfcxShortintArray(FPathValues)[AUVIndex] := Shortint(AValue);
end;

procedure TfcxTimePathShortintDTP.CreatePathUVsArray;
var
  i, i1, j, AStartOctet, ADelta: Integer;
begin
  GetMem(FUValues, FCount * FSize);
  j := 0;
  if j = FCount then // нашли все значения
  begin
    inherited CreatePathUVsArray;
    Exit;
  end;
  if FHasNull then
  begin
    PfcxShortintArray(FUValues)[j] := -1;
    inc(j);
    if j = FCount then // нашли все значения
    begin
      inherited CreatePathUVsArray;
      Exit;
    end;
  end;
  AStartOctet := FMinNotNull shr 3;
  for i := AStartOctet to cfcHashSizeOfTimePath[FTimeType] - 1 do
  begin
    if FHashTable[i] > 0 then // обработаем только ненулевые октеты
    begin
      ADelta := i shl 3;
      for i1 := 0 to 7 do
      begin
        if (FHashTable[i] and BitmapSet[i1]) <> 0 then
        begin
          PfcxShortintArray(FUValues)[j] := Shortint(ADelta + i1);
          inc(j);
          if j = FCount then // нашли все значения
          begin
            inherited CreatePathUVsArray;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited CreatePathUVsArray;
end;

procedure TfcxTimePathShortintDTP.DeleteValue(AUVIndex: integer);
begin
// если делать счетчик ссылок, то тогда надо будет убирать и само УЗ части
  if AUVIndex < (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxShortintArray(FPathValues)[AUVIndex + 1], PfcxShortintArray(FPathValues)[AUVIndex],
      (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
//   ReallocMem(FPathValues, GetSize * Cardinal(TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Capacity));
end;

function TfcxTimePathShortintDTP.FindIndexAtValue(AValue: Integer;
  var AIndex: integer): Boolean;
var
  R, I: integer;
begin
  AIndex := 0;
  Result := False;
  if (FCount = 0) then
    Exit;
  if AValue = -1 then
  begin
    if (PfcxShortintArray(FUValues)[0] = -1) then
    begin
      // Ну вот и нашли
      Result := True;
      Exit;
    end;
  end
  else
  begin
    R := FCount - 1;
    if (PfcxShortintArray(FUValues)[0] = -1) then
      AIndex := 1;
    while AIndex <= R do
    begin
      I := (AIndex + R) shr 1; // middle
      if PfcxShortintArray(FUValues)[I] < AValue then
        AIndex := I + 1
      else if PfcxShortintArray(FUValues)[I] > AValue then
        R := I - 1
      else
      begin
        Result := True; // Ну вот и нашли
        AIndex := I;
        Exit;
      end;
    end;
  end;
end;

function TfcxTimePathShortintDTP.GetValueAtBaseUVIndex(
  ABaseUVIndex: integer): Integer;
begin
  Result := PfcxShortintArray(FPathValues)[ABaseUVIndex];
end;

function TfcxTimePathShortintDTP.GetValueAtIndex(AIndex: integer): Integer;
begin
  Result := PfcxShortintArray(FUValues)[AIndex];
end;

procedure TfcxTimePathShortintDTP.InsertPathValue(AValue,
  AUVIndex: integer);
var
  AIndex: integer;
begin
  PfcxShortintArray(FPathValues)[AUVIndex] := Shortint(AValue);
  if not FindIndexAtValue(AValue, AIndex) then
  begin
    if AValue = -1 then // null value
      FHasNull := True;
    inc(FCount);
    ReallocMem(FUValues, FCount * FSize);
    if AIndex < (FCount - 1) then
      System.Move(PfcxShortintArray(FUValues)[AIndex], PfcxShortintArray(FUValues)[AIndex + 1],
        (FCount - 1 - AIndex) * FSize);
    PfcxShortintArray(FUValues)[AIndex] := Shortint(AValue);
  end;
end;

procedure TfcxTimePathShortintDTP.InsertValue(AUVIndex: integer);
begin
  if AUVIndex < (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1) then
    System.Move(PfcxShortintArray(FPathValues)[AUVIndex], PfcxShortintArray(FPathValues)[AUVIndex + 1],
      (TfcxCommonTimePathField(FOwner).MasterField.UniqueValues.Count - 1 - AUVIndex) * FSize);
end;

function TfcxCommonTimePathDTP.GetUVIndex(AUVVarValue: Variant): integer;
begin
  if (AUVVarValue = Unassigned) or (AUVVarValue = Null) then
    AUVVarValue := -1;
  if not FindIndexAtValue(AUVVarValue, Result) then
    Result := -1;
end;

{ TfcxCommonStdPathDTP }

procedure TfcxCommonStdPathDTP.AppendFromStream(ACubeStream: TStream);
var
  ACount: Integer;
  ABoolValue: Boolean;
  AUValues: Pointer;
begin
//!! недоделано
  ACubeStream.Read(ACount, SizeOf(Integer));
  if ACount > 0 then
  begin
    GetMem(AUValues, FSize * ACount);
    ACubeStream.Read(AUValues^, FSize * ACount);
    FreeMem(AUValues);
//    FCount := ACount;
    if TfcxHackBaseUniqueValues(TfcxCommonStdPathField(FOwner).MasterField.UniqueValues).FTempCount > 0 then
    begin
// не надо, т.к. был SetCapacity      GetMem(FPathValues, FSize * cardinal(TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count));
      GetMem(AUValues, FSize * TfcxHackBaseUniqueValues(TfcxCommonStdPathField(FOwner).MasterField.UniqueValues).FTempCount);

      ACubeStream.Read(AUValues^, FSize * TfcxHackBaseUniqueValues(TfcxCommonStdPathField(FOwner).MasterField.UniqueValues).FTempCount);
      FreeMem(AUValues);
//      ACubeStream.Read(FPathValues^, FSize * cardinal(TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count));
// Groups
      ACubeStream.Read(ABoolValue, SizeOf(Boolean));
      if ABoolValue then
      begin
        TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup := ABoolValue;
        TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.AppendFromStream(ACubeStream);
      end;
    end;
  end;
end;

procedure TfcxCommonStdPathDTP.ClearGroups;
begin
  if FCount > 0 then
    TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup := False;
end;

constructor TfcxCommonStdPathDTP.Create(AOwner: TObject);
begin
  FMinNotNull := -1;
  inherited;
  FSize := GetSize;
  GetMem(FHashTable, FHashBucketSize);
  FillChar(FHashTable^, FHashBucketSize, 0);
  FHasNull := False;
end;

procedure TfcxCommonStdPathDTP.CreatePathUVsArray;
begin
// тут делаем список уникальных на основе хэша
// затем освобождаем временные переменные
  FreeMem(FHashTable);
  FHashTable := nil;
end;

procedure TfcxCommonStdPathDTP.CreatePathValuesArray;
begin
  GetMem(FPathValues, FSize * TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Capacity);
end;

destructor TfcxCommonStdPathDTP.Destroy;
begin
  FreeMem(FHashTable);
  FreeMem(FPathValues);
  FreeMem(FUValues);
  inherited;
end;

function TfcxCommonStdPathDTP.FindIndexAtValue(AValue: integer;
  var AIndex: integer): Boolean;
begin
  Result := False;
  AIndex := 0;
end;

function TfcxCommonStdPathDTP.GetCaptionAtBaseUVIndex(
  ABaseUVIndex: Integer): TfcxString;
begin
  Result := Caption[ValueAtBaseUVIndex[ABaseUVIndex]];
end;

function TfcxCommonStdPathDTP.GetCaptionAtIndex(
  AIndex: integer): TfcxString;
begin
  Result := Caption[GetValueAtIndex(AIndex)];
end;

function TfcxCommonStdPathDTP.GetIndexAtBaseUVIndex(
  ABaseUVIndex: integer): integer;
begin
  FindIndexAtValue(GetValueAtBaseUVIndex(ABaseUVIndex), Result);
end;

function TfcxCommonStdPathDTP.GetIndexAtValue(AValue: integer): integer;
begin
  FindIndexAtValue(AValue, Result);
end;

function TfcxCommonStdPathDTP.GetNullCaption: TfcxString;
begin
  Result := TfcxCommonStdPathField(FOwner).NullStr;
end;

procedure TfcxCommonStdPathDTP.LoadFromStream(ACubeStream: TStream);
var

  ACount: Integer;

  ABoolValue: Boolean;
begin
  ACubeStream.Read(ACount, SizeOf(Integer));
  if ACount > 0 then
  begin
    GetMem(FUValues, FSize * ACount);
    ACubeStream.Read(FUValues^, FSize * ACount);
    FCount := ACount;
    if TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count > 0 then
    begin
// не надо, т.к. был SetCapacity      GetMem(FPathValues, FSize * cardinal(TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count));
      ACubeStream.Read(FPathValues^, FSize * TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count);
// Groups
      ACubeStream.Read(ABoolValue, SizeOf(Boolean));
      if ABoolValue then
      begin
        TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup := ABoolValue;
        TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.LoadFromStream(ACubeStream);
      end;
    end;
  end;
end;

procedure TfcxCommonStdPathDTP.LoadGroupsFromXML(AItem: TfcxXMLItem);
begin
  TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup := True;
  TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.LoadGroupsFromXML(AItem);
end;

procedure TfcxCommonStdPathDTP.Recapacity;
begin
  ReallocMem(FPathValues, FSize * TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count);
end;

procedure TfcxCommonStdPathDTP.SaveGroupsToXML(AItem: TfcxXMLItem);
var
  AItem1: TfcxXMLItem;
begin
  if FCount > 0 then
    if TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup and (TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.GroupCount > 0) then
    begin
      AItem1 := AItem.Add;
      AItem1.Name := 'field';
      AItem1.Prop['name'] := TfcxCommonStdPathField(FOwner).CubeFieldName;
      if Self is TfcxCommonDatePathDTP then
        AItem1.Prop['DateType'] := GetEnumName(TypeInfo(TfcxDateType), Ord(TfcxCommonDatePathDTP(Self).FDateType))
      else
      if Self is TfcxCommonTimePathDTP then
        AItem1.Prop['TimeType'] := GetEnumName(TypeInfo(TfcxTimeType), Ord(TfcxCommonTimePathDTP(Self).FTimeType));
      TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.SaveGroupsToXML(AItem1)
    end
end;

procedure TfcxCommonStdPathDTP.SaveToStream(ACubeStream: TStream);
var
  ABool: Boolean;
begin
  ACubeStream.Write(FCount, SizeOf(integer));
  if FCount > 0 then
  begin
    ACubeStream.Write(FUValues^, FSize * FCount);
    if TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count > 0 then
      ACubeStream.Write(FPathValues^, FSize * TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count);
    ABool := False;
    if TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup and (TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.GroupCount > 0) then
    begin
      ACubeStream.Write(TfcxCommonStdPathField(FOwner).StdPathUniqueValues.WithGroup, SizeOf(Boolean));
      TfcxCommonStdPathField(FOwner).StdPathUniqueValues.GroupManager.SaveToStream(ACubeStream)
    end
    else
      ACubeStream.Write(ABool, SizeOf(Boolean));
  end;
end;

procedure TfcxCommonStdPathDTP.SetCapacity(ASetAllToNil: boolean);
begin
  ReallocMem(FPathValues, FSize * TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Capacity);
  if ASetAllToNil and (TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count > 0) then
    FillChar(FPathValues^, TfcxCommonStdPathField(FOwner).MasterField.UniqueValues.Count * FSize, 0);
end;
end.
