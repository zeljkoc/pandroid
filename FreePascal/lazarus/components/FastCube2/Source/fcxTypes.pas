{*****************************************************}
{                                                     }
{                FastCube 2 Types unit                }
{                                                     }
{               Copyright (c) 2001-2014               }
{           by Oleg Pryalkov, Paul Ishenin            }
{                  Fast Reports Inc.                  }
{*****************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxTypes;
{$INCLUDE fcx.inc}

interface
uses
{$IFDEF FPC}
  LCLType, LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, TypInfo, Forms,
  fcxXml;

//FMX uses
{$ELSE}

{$INCLUDE fcx.inc}
interface
uses
  System.Classes, System.SysUtils, System.TypInfo,
  FMX.fcxXml;

{$ENDIF}

type
  TfcxSmallCount = Smallint;

// from ver 1

{$IFNDEF FPC}
  {$IFNDEF DELPHI_6UP}
    UTF8String = type string;
    PCardinal = ^Cardinal;
  {$ENDIF}
  {$IFNDEF DELPHI_7UP} 
    UInt64 = Longint; 
  {$ENDIF}  
  {$IFNDEF Delphi_15UP}
    PtrUInt = Cardinal;
    PtrInt = Integer;
  {$ELSE}
    PtrUInt = NativeUInt;
    PtrInt = NativeInt;
  {$ENDIF}
{$ENDIF}

// define a string type to use
{$ifdef UseAnsiString}
  TfcxString = AnsiString;
  TfcxChar = AnsiChar;
  PfcxChar = PAnsiChar;
{$else}
  {$ifdef UseWideString}
    TfcxString = WideString;
    TfcxChar = WideChar;
    PfcxChar = PWideChar;
  {$else}
    {$ifdef UseUnicodeString}
      TfcxString = UnicodeString;
      TfcxChar = WideChar;
      PfcxChar = PWideChar;
    {$else}
      TfcxString = String;
      TfcxChar = Char;
      PfcxChar = PChar;
    {$endif}
  {$endif}
{$endif}

  TfcxVarType = (
    fcvtString,
    fcvtOrdinal,
    fcvtFloat,
    fcvtDate
  );

// ver 2

  TfcxGetValue = procedure(var Result: Variant) of object;
  TfcxGetValue2 = procedure(var Result: Variant; const Final: Boolean) of object;

  IfcInterpreter = interface
    ['{E9185DD5-D5EB-4C68-B931-0D701E4D86E9}']
    // getters/setters
    function GetErrorMsg: String;
    function GetCompiled: Boolean;
    //
    procedure AddVariable(const Name, Typ: String; const Value: Variant);
    function CallFunction(const Func: Pointer; const Params: Variant): Variant;
    function Compile: Boolean;
    function GetFunctionPointer(AFunctionName: String): Pointer;
    function GetObject: TObject;
    function GetScript: TStrings;
    procedure SetScript(const AScript: TStrings);
    function GetScriptLanguage: String;
    procedure SetScriptLanguage(const Value: String);
    procedure Clear;

    property Compiled: Boolean read GetCompiled;
    property ErrorMsg: String read GetErrorMsg;
    property Script: TStrings read GetScript write SetScript;
    property ScriptLanguage: String read GetScriptLanguage write SetScriptLanguage;
  end;

  IfcxChart = interface
    ['{23E8B92A-2106-4496-A0CD-A6406212FD73}']
    procedure SaveToXMLItem(AItem: TfcxXMLItem);
    function LoadFromXMLItem(AItem: TfcxXMLItem): Boolean;
  end;


  PfcxTreeNode = ^TfcxTreeNode;
  TfcxTreeNode = record
    Parent: PfcxTreeNode;
    FirstChild: PfcxTreeNode;
    LastChild: PfcxTreeNode;
    NextSibling: PfcxTreeNode;
    PrevSibling: PfcxTreeNode;
    Data: Pointer;
    Tag: Integer;
  end;

  TfcxTree = class
  private
    FRoot: PfcxTreeNode;
    function GetFirst: PfcxTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddChild(AParent: PfcxTreeNode): PfcxTreeNode;
    function InsertNode(ANode: PfcxTreeNode; ABefore: Boolean): PfcxTreeNode;
    function FindByData(AData: Pointer): PfcxTreeNode;
    property First: PfcxTreeNode read GetFirst;
    property Root: PfcxTreeNode read FRoot;
  end;

// one dim arrays
  _fcxPointerArray = array[0..0] of Pointer;
  PfcxPointerArray = ^_fcxPointerArray;

  _fcxPointerArrayMinus = array[-1..-1] of Pointer;
  PfcxPointerArrayMinus = ^_fcxPointerArrayMinus;

  _fcxPfcxCharArray = array[0..0] of PfcxChar;
  PfcxPfcxCharArray = ^_fcxPfcxCharArray;

  _fcxBooleanArray = array[0..0] of Boolean;
  PfcxBooleanArray = ^_fcxBooleanArray;

  _fcxIntegerArray = array[0..0] of Integer;
  PfcxIntegerArray = ^_fcxIntegerArray;

  TfcxRecPointerArray = record
    PointerArray: PfcxPointerArray;
    Count: Integer;
  end;

  _fcxArrRecPointerArray = array[0..0] of TfcxRecPointerArray;
  PfcxArrRecPointerArray = ^_fcxArrRecPointerArray;

{
  _fcxArrRecIntegerArray = array[0..0] of PfcxIntegerArray;
  PfcxArrRecIntegerArray = ^_fcxArrRecIntegerArray;
}
  _fcxDoubleArray = array[0..0] of Double;
  PfcxDoubleArray = ^_fcxDoubleArray;

  _fcxIntegerArrayMinus = array[-1..-1] of Integer;
  PfcxIntegerArrayMinus = ^_fcxIntegerArrayMinus;

  _fcxByteArray = array[0..0] of Byte;
  PfcxByteArray = ^_fcxByteArray;

  _fcxWordArray = array[0..0] of Word;
  PfcxWordArray = ^_fcxWordArray;

  _fcxSmallintArray = array[0..0] of Smallint;
  PfcxSmallintArray = ^_fcxSmallintArray;

  _fcxShortintArray = array[0..0] of Shortint;
  PfcxShortintArray = ^_fcxShortintArray;

  _fcxSmallCountArray = array[0..0] of TfcxSmallCount;
  PfcxSmallCountArray = ^_fcxSmallCountArray;

  _fcxSmallCountArrayMinus = array[-1..-1] of TfcxSmallCount;
  PfcxSmallCountArrayMinus = ^_fcxSmallCountArrayMinus;

// two dims arrays
  _fcxArrPointerArray = array[0..0] of PfcxPointerArray;
  PfcxArrPointerArray = ^_fcxArrPointerArray;

  _fcxArrPointerArrayMinus = array[0..0] of PfcxPointerArrayMinus;
  PfcxArrPointerArrayMinus = ^_fcxArrPointerArrayMinus;

  _fcxArrMinusPointerArrayMinus = array[-1..-1] of PfcxPointerArrayMinus;
  PfcxArrMinusPointerArrayMinus = ^_fcxArrMinusPointerArrayMinus;

  _fcxArrIntegerArray = array[0..0] of PfcxIntegerArray;
  PfcxArrIntegerArray = ^_fcxArrIntegerArray;

  _fcxArrDoubleArray = array[0..0] of PfcxDoubleArray;
  PfcxArrDoubleArray = ^_fcxArrDoubleArray;

  _fcxArrByteArray = array[0..0] of PfcxByteArray;
  PfcxArrByteArray = ^_fcxArrByteArray;

// 3 dims arrays

  _fcxArr3IntegerArray = array[0..0] of PfcxArrIntegerArray;
  PfcxArr3IntegerArray = ^_fcxArr3IntegerArray;

  _fcxArr2ByteArray = array[0..0] of PfcxArrByteArray;
  PfcxArr2ByteArray = ^_fcxArr2ByteArray;

// Date split paths
  TfcxDateType = (  // в сумме
    odt_None,       // ??
    odt_Year,       // (0-4095) 12 bit или 2 byte  0 = null        массив 512 байт
    odt_Month,      // (1-12) 4 bit или 1 byte                     массив 2 байта
    odt_Day,        // (1-31) 5 bit или 1 byte                     массив 4 байта
    odt_DayOfWeek,  // (1-7) 3 bit или 1 byte                      массив 1 байт
    odt_Quarter,    // (1-4) 3 bit или 1 byte                      массив 1 байт
    odt_WeekNumber, // (1-55) 4(8) bit или 1 byte (дополнительные недели: 1 прошлого года и 53 следующего года (удлинним до 200)) массив 7 (26) байт
    odt_DayOfYear   // (1-366) 9 bit или 2 byte                    массив 46 байт
  );

// Time split paths
  TfcxTimeType = (
    ott_None,        // ???
    ott_Hour,        // (0-23) 5 bit или 1 byte -1 = null              массив 4 байта
    ott_Minute,      // (0-59) 6 bit или 1 byte                        массив 8 байт
    ott_Second,      // (0-59) 6 bit или 1 byte                        массив 8 байт
    ott_Millisecond  // (0-999) 10 bit или 2 byte                      массив 125 байт
  );

  TfcxDateTypes = set of TfcxDateType;
  TfcxTimeTypes = set of TfcxTimeType;

  TfcxMonthDisplayFormat = (
    mdf_Long,      // long month like 'January'
    mdf_Short,     // short month like 'Jan'
    mdf_Custom
  );

  TfcxWeekDayDisplayFormat = (
    wddf_Long,     // long day of week like 'Sunday'
    wddf_Short,    // short day of weel like 'Sun'
    wddf_Custom
  );

  TfcxQuarterDisplayFormat = (
    qdf_System,    // use resource string to display quarter like 'Q1'
    qdf_Custom
  );

  TfcxWeekNumberDisplayFormat = (
    wndf_System,   // use predefined format to display week number: usually number but can contain a resource string 'Prev Year', 'Next Year'
    wndf_Custom
  );

  TfcxDataType = (
    fcdt_Error,
    fcdt_NotImplemented, // field type is not implemented/supported by FastCube
    fcdt_Integer,
    fcdt_Byte,
    fcdt_Word,
    fcdt_DateTime,
    fcdt_Date,
    fcdt_Time,
    fcdt_SmallInteger,
    fcdt_LargeInteger,
    fcdt_Double,
    fcdt_BCD,
    fcdt_String,
    fcdt_WideString,
    fcdt_Boolean,
    fcdt_Currency,
    fcdt_TimeStamp,
    fcdt_Single,
    fcdt_ShortInteger
  );

  TfcxSortDirection = (fcsd_Asc, fcsd_Desc);
  TfcxTotalPosition = (
    fctp_Before, // Show totals before values
    fctp_After,  // Show totals after values
    fctp_Hide    // Hide totals
    );
  TfcxShowTotalAs = (
    sta_AsTotal,
    sta_AsParentValue,
    sta_AsTotalWithParentValue
  );

  TfcxCheckState = (
    csUnchecked,
    csChecked,
    csGrayed
  );

  TfcxDataPoint = record
    x: integer;
    y: integer;
  end;

  TfcxDataRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Integer);
      1: (LeftTop, BottomRight: TfcxDataPoint);
  end;

  TfcxGridSelection = record // selected area (in visible coordinate)
    Rect: TfcxDataRect;    // area as TDataRect
    Start: TfcxDataPoint;  // start area point
    Last: TfcxDataPoint;   // stop area point
    Fixed: Boolean;     // fixed area for (MouseUp, KeyUp)
  end;

const
  fcxIntegerTypes = [fcdt_Integer, fcdt_Byte, fcdt_Word, fcdt_SmallInteger, fcdt_LargeInteger];
  fcxFloatTypes = [fcdt_Double, fcdt_BCD, fcdt_Currency, fcdt_Single];
  fcxNumericTypes = fcxIntegerTypes + fcxFloatTypes;
//  fcxDateTypes = [fcdt_DateTime, fcdt_Date, fcdt_TimeStamp];
  fcxDateTypes: set of TfcxDataType = [
   fcdt_DateTime,
   fcdt_Date,
   fcdt_TimeStamp
  ];

  fcxTimeTypes: set of TfcxDataType = [
   fcdt_DateTime,
   fcdt_Time,
   fcdt_TimeStamp
  ];


  fcxStringTypes = [fcdt_String, fcdt_WideString];

type
  TfcxDragPosition = (
    fcdpBefore,
    fcdpAfter
  );

  TfcxDatePaths = record
    Year, Month, Day, DayOfWeek, DayOfYear, WeekNumber, Quarter, YearOfWeek: Word
  end;
  TfcxTimePaths = record
    Hour, Minute, Second, Millisecond: Word
  end;
  TfcxDateTimeConsts = record
    DayOfWeekISO8601, WeekNumberISO8601: Boolean;
  end;

  TfcxAttributeType = (
    fcxsft_Reference,
    fcxsft_Custom,
    fcxsft_Date,
    fcxsft_Time
  );

  TfcxCaseSensitive = (
    cs_Default,     // Take it from cube. Sensitivity can depend on data type
    cs_Sensitive,   // Explicitly set case sensitivity
    cs_InSensitive  // Explicitly set case insensitivity
  );

  TfcxFieldProperties = record
    CubeFieldName: TfcxString;
    CubeFieldDisplayLabel: TfcxString;
    DataType: TfcxDataType;
    CaseSensitive: boolean;
    NullStr: TfcxString;
    Saved: Boolean;
    CalculateAfterAll: Boolean;
  end;

// Type of field regions in cube
  TfcxRegionOfField = (
    rf_Page,
    rf_CapXAx,
    rf_CapYAx,
    rf_CapFacts,
    rf_None
  );

// Set of Types of field regions in cube
  TfcxRegionsOfField = set of TfcxRegionOfField;

// Fields List Order :
  TfcxFieldsOrder = (
    fcfloByDataSet,           // by order from DataSet
    fcfloByFieldName,         // by Field Name
    fcfloByFieldDisplayLabel, // by Field Display Label
    fcfloCustom               // by OnGetFieldOrder
  );

  TfcxIntegerCompare = function(const AIndex1, AIndex2: Integer): Integer of Object;

  TfcxCalcScriptType = (
    cst_MeasureFormula,
    cst_MeasureFormulaDetail,
    cst_MeasureFilter
  );

  TfcxOnScriptError = procedure(const AScriptType: TfcxCalcScriptType; const AErrorMessage: TfcxString) of Object;

// Type of agr functions
  TfcxAgrFunc = (
    af_None,
    af_Sum,      // Sum
    af_Count,    // Count
    af_Min,      // Min
    af_Max,      // Max
    af_Avg,      // Avg
    af_Mul,      // Mul
    af_Variance, // Несмещенная дисперсия
    af_StdDev,   // Несмещенное средне-квадратичное отклонение
    af_VarianceS,// Смещенная дисперсия
    af_StdDevS,  // Смещенное средне-квадратичное отклонение
    af_Formula,  // For calculating based on measures and dimetions
    af_CountOfUnique, // For Count Of Unique Values
    af_FirstValue, // First value
    af_ListOfUnique, // For List Of Unique Values
    af_LastValue, // Last value
    af_FormulaDetail,   // For calculating based on detail
    af_Median, // Median
    af_WeightedMean // weighted mean (2 fields)
  );
  TfcxSetAgrFunc = set of TfcxAgrFunc; // Set of types of agr functions

  TfcxTotalsConflictResolve = (
    tcrEmptyValue,
    tcrUseXAxis,
    tcrUseYAxis
  );

// Type of agr functions for selected area
  TfcxSelAgrFunc = af_None..af_Avg;

  _fcxAgrFuncArray = array[0..0] of TfcxAgrFunc;
  PfcxAgrFuncArray = ^_fcxAgrFuncArray;

  _fcxArrAgrFuncArrayMinus = array[-1..-1] of PfcxAgrFuncArray;
  PfcxArrAgrFuncArrayMinus = ^_fcxArrAgrFuncArrayMinus;

{
  TfcxTypeOfCellAxis =
  (
//    tca_Invisible,                   //0. ячейка невидима
    tca_SimpleInLastLevel,           //1. простая ячейка в последнем столбце - рисуем ячейку - заканчиваем
    tca_TotalStart,                  //2. итоговая ячейка начало - рисуем в право до конца  - заканчиваем
    tca_SimpleExpandedNotLastLevel,  //3. простая ячейка не в последнем столбце (с детьми)
                                     //    Развернута  - прорисовываем видимых детей - рисуем по высоте всех видимых детей - заканчиваем
    tca_SimpleCollapsedNotLastLevel, //4. простая ячейка не в последнем столбце (с детьми)
                                     //    Свернута - рисуем по высоте одной строки в право до конца  - заканчиваем
    tca_TotalNotStart,               //5. итоговая ячейка не начало - не рисуем
    tca_SimpleNotLastLevel,          //6. простая ячейка не в последнем столбце (с детьми) но без возможности сворачиваться
                                     //    прорисовываем видимых детей - рисуем по высоте всех видимых детей - заканчиваем
    tca_GrandTotalExpanded,          //7. общий итог развернутый
    tca_GrandTotalCollapsed          //8. общий итог свернут
//    tca_BlockExpandNotLastLevel,     // ячейка в непоследней строке с блокировкой развернутости
//    tca_Hierarchi1                   // more comments :)
  );
}
  TfcxTypeOfCellInAxis = (
    tca_GrandTotalExpanded,  // "Начало общего итога"
    tca_GrandTotalCollapsed, // "Начало общего итога"
    tca_GrandTotal, // "Начало несворачиваемого общего итога"
    tca_StartTotal, // "Начало итога"
    tca_Empty, // "Пустая (нерисуемая) обычная"
    tca_EmptySubGroup, // "Пустая (нерисуемая) подгрупная"
    tca_GroupExpanded, // "Развёрнутая группа"
    tca_Collapsed_GroupCollapsed, // "Свернутая ячейка Свёрнутой группы"
    tca_Expanded_GroupCollapsed, // "Развернутая ячейка Свёрнутой группы"
    tca_CollapsedSingle, // "Свернутая единичная ячейка"
    tca_ExpandedSingle, // "Развёрнутая единичная ячейка"
    tca_CollapsedSimple, // "Свёрнутая обычная"
    tca_ExpandedSimple // "Развёрнутая обычная"
//"Значимая ячейка последнего уровня"
  );

  _fcxTypeOfCellInAxisArray = array[0..0] of TfcxTypeOfCellInAxis;
  PfcxTypeOfCellInAxisArray = ^_fcxTypeOfCellInAxisArray;

  TfcxPropertyOfCellAxis = (
    pca_Simple,              //простая ячейка
//    pca_Total,               //итоговая ячейка
    pca_GrandTotal,          //общий итог
    pca_GroupValue,          //группирующая ячейка ??
//    pca_ValueInGroup,        //ячейка, входящаю в группу
    pca_LastLevel,           //крайняя ячейка (захватывает последний уровень)
    pca_StartTotal,          //начальная ячейка (для тотала)
    pca_Expanded,            //Развернута на след. уровень
    pca_Collapsed,           //Свернута след. уровень
    pca_HExpanded,           //Иерархия Развернута
    pca_HCollapsed,          //Иерархия Свернута
    pca_GExpanded,           //Группа Развернута
    pca_GCollapsed,          //Группа Свернута
    pca_GSingle,             //Простое значение в уровне с группой
    pca_Empty,               //пустая (нерисуемая) ячейка, потомок тотала или простой ячейки в уровне SubGroup или свёрнутой групповой ячейки в уровне SubGroup
    pca_EmptySubGroup,       //пустая (нерисуемая) ячейка, потомок простой ячейки в уровне SubGroup или свёрнутой групповой ячейки в уровне SubGroup
    pca_Sort,                //ячейка, участвующая в сортировке по значению показателя
    pca_TreeMeasureCell,     //дополнительная ячейка показателя в древовидно оси
    pca_TreeCellWithMeasure, //ячейка, у которой есть дополнительная ячейка показателя в древовидно оси
    pca_Other,               //Others for TopN
    pca_MeasureCell          //ячейка показателя
  );

  TfcxPropertiesOfCellAxis = set of TfcxPropertyOfCellAxis;

  TfcxScrollParam = (
    spMin,
    spMax,
    spPosition,
    spSmallChange,
    spLargeChange
  );

// Type of unique values filter
  TfcxUVFilterType = (
    uvft_None,    // disabled or not supported
    uvft_Single,  // can select only one value
    uvft_Set      // can select any set of values
  );

  _fcxUVFilterTypeArray = array[0..0] of TfcxUVFilterType;
  PfcxUVFilterTypeArray = ^_fcxUVFilterTypeArray;

// Type of TopN
  TfcxTopType = (
    ttMax, // use Max
    ttMin  // use Min
  );

// Items to save in XML
  TfcxItemForStoreWithSlice = (
    fcxiss_Groups,
    fcxiss_Filters,
    fcxiss_Charts
  );

  TfcxItemsForStoreWithSlice = set of TfcxItemForStoreWithSlice;

  TfcxProgressType = (
    fcxpExporting,
    fcxpFetchingData
  );

// common VCL and FMX theme enums

  TfcxThemeCellState = (
    tcsFocused,
    tcsSelected,
    tcsTotal
  );
  TfcxThemeCellStates = set of TfcxThemeCellState;

  TfcxThemeGradientDirection = (
    tgdNone,
    tgdHorizontal,
    tgdVertical,
    tgdHorizontalCenter,
    tgdVerticalCenter,
    tgdDiagonal1,
    tgdDiagonal2,
    tgdDiagonal1Center,
    tgdDiagonal2Center,
    tgdCorner1,
    tgdCorner2,
    tgdCorner3,
    tgdCorner4,
    tgdCenter
  );

  TfcxThemeSortType = (
    tstUp,
    tstDown,
    tstUnknown
  );

  TfcxThemeButtonOption = (
    tboHasSortArrow,
    tboHasDropDownButton,
    tboHasFilteredValues
  );
  TfcxThemeButtonOptions = set of TfcxThemeButtonOption;

  TfcxTreeButtonKind = (
    tbkPlusButton,
    tbkMinusButton,
    tbkTreeButtonPlus,
    tbkTreeButtonMinus
  );

  TfcxHighlightApplyToEnum = (
    hatCells,      // use highlight for cells
    hatTotals,     // use highlight for totals
    hatGrandTotal  // use highlight for grand total
  );

  TfcxHighlightApplyTo = set of TfcxHighlightApplyToEnum;

{$IFNDEF DELPHI_6UP}
type
  PWordBool     = ^WordBool;
  TValueSign = -1..1;

const
  NegativeValue = Low(TValueSign);
  ZeroValue = 0;
  PositiveValue = High(TValueSign);

function Sign(const AValue: Integer): TValueSign; overload;
function Sign(const AValue: Int64): TValueSign; overload;
function Sign(const AValue: Double): TValueSign; overload;
{$ENDIF}

const

{$IFNDEF DELPHI_6UP}
  clSystemColor = $FF000000;
  clMoneyGreen = TColor($C0DCC0);
  clGradientActiveCaption = TColor(clSystemColor or COLOR_GRADIENTACTIVECAPTION);
  clGradientInactiveCaption = TColor(clSystemColor or COLOR_GRADIENTINACTIVECAPTION);
{$ENDIF}
// Node State flags
  stDefault      = $00; // empty state (not visible and expanded to next dimension)
  stDimCollapsed = $01; // collapsed value to next dimension
  stVisible      = $02; // visible value
  stGExpanded    = $04; // Expanded group value to sublevel
  stZero         = $08; // Hidden zero value
  stHidden       = $10; // Hidden by user

  stZeroOrHidden = stZero or stHidden;
//  stTotal     = $01; // total value
//  stExpand    = $02; // expanded value
//  stZero      = $04; // zero value
//  stVisible   = $08; // visible value
//  stLExpand   = $10; // expanded level (HierLevel Expand)
//  stBExpand   = $20; // expand blocked  (for hierarchi)
// to speedup some operations
//  stVisExpand = stExpand or stVisible;
//  stNotZero   = $0B; // not zero
//

  BitmapSet: array[0..7] of byte = (1, 2, 4, 8, 16, 32, 64, 128);
  BitmapUnSet: array[0..7] of byte = (254, 253, 251, 247, 239, 223, 191, 127);

  cfcSizeOfDatePath: array[TfcxDateType] of byte = (0,2,1,1,1,1,1,2);
  cfcSizeOfTimePath: array[TfcxTimeType] of byte = (0,1,1,1,2);
  cfcHashSizeOfDatePath: array[TfcxDateType] of Word = (0,512,2,4,1,1,26,46);
  cfcHashSizeOfTimePath: array[TfcxTimeType] of Word = (0,4,8,8,125);

{ TODO -cНеобходимо : Перенести в отдельный модуль.}
  DateLabelPrefixTable: Array[TfcxDateType] of TfcxString = ('', 'SYearPrefix', 'SMonthPrefix', 'SDayPrefix', 'SDayOfWeekPrefix', 'SQuarterPrefix', 'SWeekNumberPrefix', 'SDayOfYearPrefix');
  DateNamePrefixTable : Array[TfcxDateType] of TfcxString = ('', '#Year_', '#Month_', '#Day_', '#DayOfWeeek_', '#Quarter_', '#WeekNumber_', '#DayOfYear_');
  DateAttributeNamePrefix = '#Date_';
  TimeLabelPrefixTable: Array[TfcxTimeType] of TfcxString = ('', 'SHourPrefix', 'SMinutePrefix', 'SSecondPrefix', 'SMillisecondPrefix');
  TimeNamePrefixTable : Array[TfcxTimeType] of TfcxString = ('', '#Hour_', '#Minute_', '#Second_', '#Millisecond_');
  TimeAttributeNamePrefix = '#Time_';
  sFuncCaptions       : Array[TfcxAgrFunc] of TfcxString  = ('', 'sSumCaption', 'sCountCaption', 'sMinCaption', 'sMaxCaption', 'sAvgCaption', 'sMulCaption', 'sVarianceCaption', 'sStdDevCaption', 'sVarianceSCaption', 'sStdDevSCaption', 'sFuncCaption', 'sCountOfUniqueCaption', 'sFirstValue', 'sListOfUniqueCaption', 'sLastValue', 'sFuncDetCaption', 'sMedianCaption', 'sWeightedMeanCaption');
  sFuncLabels         : Array[TfcxAgrFunc] of TfcxString  = ('', 'sSumPrefix', 'sCountPrefix', 'sMinPrefix', 'sMaxPrefix', 'sAvgPrefix', 'sMulPrefix', 'sVariancePrefix', 'sStdDevPrefix', 'sVarianceSPrefix', 'sStdDevSPrefix', 'sFuncPrefix', 'sCountOfUniquePrefix', 'sFirstValuePrefix', 'sListOfUniquePrefix', 'sLastValuePrefix', 'sFuncDetPrefix', 'sMedianPrefix', 'sWeightedMeanPrefix');

  // can be used for compare functions:
  // Result := fcxCompareHelper[A > B]
  fcxCompareHelper: array[Boolean] of Integer = (-1, 1);
  fcxCompareHelperDirection: array[TfcxSortDirection, Boolean] of Integer = ((-1, 1), (1, -1));
  VarianceFuncs: set of TfcxAgrFunc = [af_Variance, af_StdDev, af_VarianceS, af_StdDevS];
  FinalFuncs: set of TfcxAgrFunc = [af_CountOfUnique, af_ListOfUnique, af_Median];
  RestrucFuncs: set of TfcxAgrFunc = [af_Avg, af_Variance, af_StdDev, af_VarianceS, af_StdDevS, af_CountOfUnique, af_ListOfUnique, af_FormulaDetail, af_Median, af_WeightedMean];
  ScriptFuncs: set of TfcxAgrFunc = [af_Formula, af_FormulaDetail];
  CanDistinctFuncs: set of TfcxAgrFunc = [af_Sum, af_Count, af_Avg, af_Mul, af_Variance, af_StdDev, af_VarianceS, af_StdDevS, af_WeightedMean];
  CanCreateAllCellsFuncs: set of TfcxAgrFunc = [af_Formula];
// required 2 fields
  ExtraFuncs: set of TfcxAgrFunc = [af_WeightedMean];

function fcxVarToStr(V: Variant): TfcxString;
function PAnsiCharToPChar(P: PAnsiChar): PChar;
function fcxIntCompare(const AIndex1, AIndex2: Integer): Integer;
function CheckFireDacDataSet(ADataSet: TObject): Boolean;

function fcxDataRect(x1, y1, x2, y2: Integer): TfcxDataRect;
function fcxDataPoint(x1, y1: Integer): TfcxDataPoint;
function fcxEqualDataPoint(const P1, P2: TfcxDataPoint): Boolean;
function fcxEqualDataRect(const R1, R2: TfcxDataRect): Boolean;

function fcxGetTickCount: Cardinal;
function fcxExeName: string;

implementation

function fcxVarToStr(V: Variant): TfcxString;
begin
  Result := V;
end;

function PAnsiCharToPChar(P: PAnsiChar): PChar;
begin
  {$IFDEF Delphi_12UP}
    if P <> nil then
    begin
      GetMem(Result, (StrLen(P) + 1) * SizeOf(Result^));
      StrPCopy(Result, String(AnsiString(P)));
      FreeMem(P);
    end
    else
      Result := nil;
  {$ELSE}
    Result := P;
  {$ENDIF}
end;

function fcxIntCompare(const AIndex1, AIndex2: Integer): Integer;
begin
  if AIndex1 = AIndex2 then
    Result := 0
  else
    Result := fcxCompareHelper[AIndex1 > AIndex2];
end;

function CheckFireDacDataSet(ADataSet: TObject): Boolean;
var
  AClass: TClass;
begin
  Result := False;
  AClass := ADataSet.ClassType;
  while (AClass <> nil) and not (AClass = TObject) do
  begin
    if AClass.ClassNameIs('TADDATASET') or AClass.ClassNameIs('TFDDATASET') then
    begin
      Result := True;
      Exit;
    end;
    AClass := AClass.ClassParent;
  end;
end;

{$IFNDEF DELPHI_6UP}
function Sign(const AValue: Integer): TValueSign;
begin
  Result := ZeroValue;
  if AValue < 0 then
    Result := NegativeValue
  else if AValue > 0 then
    Result := PositiveValue;
end;

function Sign(const AValue: Int64): TValueSign;
begin
  Result := ZeroValue;
  if AValue < 0 then
    Result := NegativeValue
  else if AValue > 0 then
    Result := PositiveValue;
end;

function Sign(const AValue: Double): TValueSign;
begin
  if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000) then
    Result := ZeroValue
  else if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000) then
    Result := NegativeValue
  else
    Result := PositiveValue;
end;
{$ENDIF}

function fcxDataRect(x1, y1, x2, y2: Integer): TfcxDataRect;
begin
  with Result do
  begin
    Left := x1;
    Top := y1;
    Right := x2;
    Bottom := y2;
  end;
end;

function fcxDataPoint(x1, y1: Integer): TfcxDataPoint;
begin
  with Result do
  begin
    x := x1;
    y := y1;
  end;
end;

function fcxEqualDataPoint(const P1, P2: TfcxDataPoint): Boolean;
begin
  Result := (P1.x = P2.x) and (P1.y = P2.y);
end;

function fcxEqualDataRect(const R1, R2: TfcxDataRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Top = R2.Top) and 
            (R1.Right = R2.Right) and (R1.Bottom = R2.Bottom);
end;

{ TfcxTree }

function TfcxTree.AddChild(AParent: PfcxTreeNode): PfcxTreeNode;
begin
  if AParent = nil then
    AParent := FRoot;
  New(Result);
  Result^.Parent := AParent;
  Result^.FirstChild := nil;
  Result^.LastChild := nil;
  Result^.NextSibling := nil;
  Result^.Data := nil;
  Result^.Tag := 0;

  if Assigned(AParent^.LastChild) then
  begin
    AParent^.LastChild^.NextSibling := Result;
    Result^.PrevSibling := AParent^.LastChild;
    AParent^.LastChild := Result;
  end
  else
  begin
    AParent^.FirstChild := Result;
    AParent^.LastChild := Result;
    Result^.PrevSibling := nil;
  end;
end;

procedure TfcxTree.Clear;
var
  Temp: PfcxTreeNode;
  
  procedure Traverse(ANode: PfcxTreeNode);
  begin
    while Assigned(ANode) do
    begin
      Traverse(ANode^.FirstChild);
      Temp := ANode^.NextSibling;
      Dispose(ANode);
      ANode := Temp;
    end;
  end;
begin
  Traverse(FRoot^.FirstChild);
  FRoot^.FirstChild := nil;
  FRoot^.LastChild := nil;
end;

constructor TfcxTree.Create;
begin
  inherited Create;
  New(FRoot);
  FRoot^.FirstChild := nil;
  FRoot^.LastChild := nil;
  FRoot^.Parent := nil;
end;

destructor TfcxTree.Destroy;
begin
  Clear;
  Dispose(FRoot);
  inherited;
end;

function TfcxTree.FindByData(AData: Pointer): PfcxTreeNode;

  function Traverse(ANode: PfcxTreeNode): PfcxTreeNode;
  begin
    if Assigned(ANode) then
    begin
      if ANode^.Data = AData then
      begin
        Result := ANode;
        Exit;
      end;
      Result := Traverse(ANode^.FirstChild);
      if not Assigned(Result) then
        Result := Traverse(ANode^.NextSibling);
    end
    else
      Result := nil;
  end;

begin
  Result := Traverse(FRoot^.FirstChild);
end;

function TfcxTree.GetFirst: PfcxTreeNode;
begin
  Result := FRoot^.FirstChild;
end;

function TfcxTree.InsertNode(ANode: PfcxTreeNode; ABefore: Boolean): PfcxTreeNode;
begin
  New(Result);
  Result^.FirstChild := nil;
  Result^.LastChild := nil;
  Result^.Data := nil;
  Result^.Tag := 0;

  if not Assigned(ANode) then
  begin
    Result^.Parent := FRoot;
    Exit;
  end;

  Result^.Parent := ANode^.Parent;

  if ABefore then
  begin
    Result^.NextSibling := ANode;
    Result^.PrevSibling := ANode^.PrevSibling;
    ANode^.PrevSibling := Result;
    if not Assigned(Result^.PrevSibling) then
      Result^.Parent^.FirstChild := Result;
  end
  else
  begin
    Result^.PrevSibling := ANode;
    Result^.NextSibling := ANode^.NextSibling;
    ANode^.NextSibling := Result;
    if not Assigned(Result^.NextSibling) then
      Result^.Parent^.LastChild := Result;
  end;
end;

function fcxExeName: string;
begin
{$IFNDEF FMX}
  Result := Application.ExeName;
{$ELSE}
  Result := ParamStr(0);
{$ENDIF}
end;

function fcxGetTickCount: Cardinal;
begin
{$IFNDEF FMX}
  Result := GetTickCount;
{$ELSE}
  Result := TThread.GetTickCount;
{$ENDIF}
end;

end.


