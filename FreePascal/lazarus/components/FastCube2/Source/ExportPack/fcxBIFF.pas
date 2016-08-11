
{******************************************}
{                                          }
{             FastReport v4.0              }
{            BIFF8 Writing API             }
{                                          }
{         Copyright (c) 1998-2011          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

{ This module provides API for writing BIFF
  streams. This format is documented in
  MSDN [MS-XLS] section.

  The only way to store BIFF2-BIFF4 documents is
  to write data to a simple stream file. Since BIFF5
  the documents can be stored as compound document files:
  see frxCBFF.pas }

//VCL uses section
{$IFNDEF FMX}
unit fcxBIFF;

{$INCLUDE fcx.inc}

interface

uses
  {$ifndef fpc}
  Windows,
  {$else}
  Types, LCLType, LCLIntf,
  {$endif}
  Graphics,
  SysUtils,
  Classes,
  Math,
  fcxUtils,
  fcxStorage,
  fcxEscher,
  fcxCrypto;
{$ELSE}
{$INCLUDE fcx.inc}

interface

uses
  // RTL
  System.Types, System.UITypes, System.SysUtils, System.Classes, System.Math,
  // FMX
  FMX.Types, FMX.TextLayout,
  // FC FMX
  FMX.fcxUtils, FMX.fcxStorage, FMX.fcxEscher, FMX.fcxCrypto;

{$ENDIF}

type
  TBiffPaperSize = Word;
  TBiffFormatIndex = LongInt;
  TBiffStreamKind = LongInt;    // See bk*** values
  TBiffRecId = Word;            // BiffId*** values

const

  { BIFF8 allows the maximium record
  length of 8228 bytes including the record
  header (4 bytes) }

  BiffMaxRecLen       = $2020;

  { The maximum number of blocks in a MERGEDCELLS record.
    Required by [MS-XLS] 2.4.168 }

  BiffMaxMrgCellsNum  = 1026;

  //
  // Rows in BIFF8 are grouped into blocks.
  //

  BiffRowBlockSz      = 32;

  //
  // BIFF8 palette size.
  // This size includes the first 8 built-in colors.
  //

  BiffPaletteSize     = $40;

  { RRD table contains unique identifiers
    for all sheets in a workbook. If the
    count of sheets is greater that BiffRrdMaxCount
    then the RRD table is not emitted. }

  BiffRrdMaxCount     = $1010;

  //
  // BIFF8 allows cells with coordinates within
  // the following bounds:
  //
  //  row = 0 .. BiffMaxRow
  //  col = 0 .. BiffMaxCol
  //

  BiffMaxRow          = $fffe;
  BiffMaxCol          = $fe;

  //
  // Number formats are stored in a table.
  // The first 164 items are reserved and
  // define special widely used formats.
  // The first user format can begin from 164.
  //

  BiffUserFormat      = 164;

  //
  // BOF records have a field which specifies
  // a type of a stream which they start.
  // TBiffStreamKind lists all possible stream types.
  //

  bkWBGlobals         = 5;
  bkVBModule          = 6;
  bkSheet             = $10;
  bkChart             = $20;
  bkMacro             = $40;
  bkWorkspace         = $100;

  //
  // Font options
  //

  foBold              = $1;
  foItalic            = $2;
  foUnderline         = $4;
  foStruckOut         = $8;
  foOutline           = $10;
  foShadow            = $20;
  foCondense          = $40;
  foExtended          = $80;

  //
  // Font underline values
  //

  fuNone              = 0;
  fuSingle            = 1;
  fuDouble            = 2;
  fuSingleAcc         = $21;
  fuDoubleAcc         = $22;

  //
  // Normally, font weight value lies
  // in range 100..1000
  //

  fwNormal            = 400;
  fwBold              = 700;

  //
  // XF cell protection
  //

  xftpCellLocked      = $1;
  xftpHidden          = $2;
  xftpStyle           = $4;

  //
  // XF used attributes flags
  //

  BiffXfuaNumber      = $01;
  BiffXfuaFont        = $02;
  BiffXfuaText        = $04;
  BiffXfuaBorders     = $08;
  BiffXfuaBg          = $10;
  BiffXfuaCellProt    = $20;
  BiffXfuaAll         = $3f;

  //
  // Pattern style
  //

  psNone              = 0;
  psSolid             = 1;
  psChess             = 2;
  psHorThick          = 5;
  psVerThick          = 6;
  psChessThick        = 9;
  psHor               = $B;
  psVer               = $C;
  psDiagBack          = $D;
  psDiag              = $E;
  psCross             = $F;
  psCrossDiag         = $10;

  //
  // Window options
  //

  woFormula           = $1;
  woGrid              = $2;
  woHeaders           = $4;
  woFreezePanes       = $8;
  woShowZeros         = $10;
  woAutoGridCol       = $20;
  woColRTL            = $40;
  woOutline           = $80;
  woNoSplits          = $100;
  woSelected          = $200;
  woActive            = $400;
  woPageBreak         = $800;

  //
  // Sheet type
  //

  skWorksheet         = 0;
  skChart             = 2;
  skVB                = 6;

  //
  // BIFF keeps all format strings in a single list.
  // Several entries in this list are reserved.
  // A few indexes to these reserved formats are listed here.
  //

  BiffFmtGeneral      = 0;
  BiffFmtFixedPoint   = 2;      // 0.00
  BiffFmtThSep        = 4;      // #,##0.00
  BiffFmtCurrency     = 5;      // "$"#,##0_);("$"#,##0)
  BiffFmtPercentInt   = 9;      // 0%
  BiffFmtPercentFixed = 10;     // 0.00%
  BiffFmtDateTime     = 2232;   // M/D/YY h:mm

  //
  // Flags for options of WINDOW2 object
  //

  BiffWoFormulas      = 1;      // show formulas; clear = show results of formulas
  BiffWoGridLines     = 2;      // show grid lines
  BiffWoHeaders       = 4;      // show sheet headers
  BiffWoFrozen        = 8;      // panes are frozen
  BiffWoZeros         = $10;    // show zeros; clear = show zeros as empty cells
  BiffWoAutoGridColor = $20;    // automatic grid line color; clear = manual
  BiffWoColumnsRTL    = $40;    // columns from right to left; clear = left to right
  BiffWoOutline       = $80;    // show outline symbols
  BiffWoNoSplits      = $100;   // remove splits if pane freeze is removed
  BiffWoSelected      = $200;   // sheet selected
  BiffWoActive        = $400;   // sheet active
  BiffWoPageBreak     = $800;   // show in page break preview; clear = normal view

  { List of named paper sizes. }

  BiffPsUnknown       = 0;
  BiffPsA4            = 9;
  BiffPsReservedMin   = 118;
  BiffPsReservedMax   = 255;
  BiffPsCustomMin     = 256;

  { Flags for BOF record as specified
    in [MS-XLS] section 2.4.21. }

  BiffBoffWin         = 1;
  BiffBoffRisc        = 2;
  BiffBoffBeta        = 4;
  BiffBoffWinAny      = 8;
  BiffBoffMacAny      = $10;
  BiffBoffBetaAny     = $20;
  BiffBoffRiscAny     = $100;
  BiffBoffFontLim     = $2000;

  { Flags for WSBOOL record.
    See [MS-XLS] section 2.4.351 }

  BiffWsbShowBreaks   = $0001;  // show page breaks that added automatically
  BiffWsbDialog       = $0010;  // sheet is dialog
  BiffWsbOutline      = $0020;
  BiffWsbRowSums      = $0040;
  BiffWsbColSums      = $0080;
  BiffWsbFitPage      = $0100;  // fit the sheet to a print page
  BiffWsbHSync        = $1000;
  BiffWsbVSync        = $2000;
  BiffWsbAltExpr      = $4000;
  BiffWsbAltFormulas  = $8000;

type

  { BIFF stream.

    BIFF stream is a sequence of small
    data blocks named BIFF records. Each record
    has a header (2 byte Id and 2 byte Size field)
    and an optional data block. Normally, a BIFF stream
    starts with a BOF record and ends with a EOF record. }

  TBiffStream = class;

  TBiffRecord = class
  private

    FOwner: TBiffStream;
    FOffset: Cardinal;

    function  GetRecId: TBiffRecId;
    function  GetSize: Cardinal;
    procedure SetSize(n: Cardinal);

  public

    constructor Create(Owner: TBiffStream; Offset: Cardinal);

    { These methods append data to the record }

    procedure Write(const Buffer; Count: Cardinal);
    procedure WriteConst(Value, Size: Cardinal);
    procedure WriteZeros(Size: Cardinal);
    procedure WriteStream(Stream: TStream);

    { Allows to write data at arbitrary place inside the record }

    procedure WriteBytes(Offset, Data, Count: Cardinal);

    { Saves and loads contents of the record }

    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    { Each BIFF record has an Id value that
      defines a type of the contents }

    property Id: TBiffRecId read GetRecId;

    { Size of the record's contents. This size doesn't include
      the 4-byte record header. }

    property Size: LongWord read GetSize write SetSize;

    { Each BIFF record is stored within a BIFF stream.
      This value contains an offset in bytes from the
      beginning of the BIFF stream to this BIFF record. }

    property Offset: LongWord read FOffset;

  end;

  TBiffStream = class
  private

    FStream: TStream;
    FRecords: TListCache;   // List of TBiffRecord
    FLastRec: TBiffRecord;  // Last added record

    function  ReadBytes(Offset, Count: Cardinal): Cardinal;
    procedure WriteBytes(Offset, Data, Count: Cardinal);

    procedure Append(Data: Cardinal; Count: Cardinal);
    procedure AppendData(const Data; Count: Cardinal);
    procedure AppendRecord(Rec: TBiffRecord; const Data; DataSize: Cardinal);

    function GetRecord(Index: Integer): TBiffRecord;
    function GetRecCount: Integer;
    function GetSize: Cardinal;

  public

    constructor Create(Cached: Boolean = False);
    destructor Destroy; override;

    function Add(Id: TBiffRecId): TBiffRecord;
    function AddBOF(k: TBiffStreamKind): TBiffRecord;
    function AddEOF: TBiffRecord;

    procedure SaveToStream(Stream: TStream);

    property Records[Index: LongInt]: TBiffRecord read GetRecord; default;
    property Count: Integer read GetRecCount;
    property Size: Cardinal read GetSize;

  end;

  { BIFF object.

    BIFF object is anything that has
    representation in a BIFF stream.
    BIFF objects are: workbook, worksheet,
    text cell, unicode string and so on. }

  TBiffObject = class

    { Every BIFF object must be capable to
      serialize itself in a BIFF stream. }

    procedure Flush(Stream: TBiffStream); virtual; abstract;

    function GetHashCode: LongInt; reintroduce; virtual;
    function Equals(s: TBiffObject): Boolean; reintroduce; virtual;

    { This method calls Flush method for each BIFF object
      in the list. }

    class procedure FlushList(list: TObjList; Stream: TBiffStream);

  end;

  //
  // BIFF8 unicode string
  //

  TBiffUCS = class(TBiffObject)
  private

    FData:      WideString;
    FRuns:      TMemoryStream;
    FHash:      LongInt;
    FCompress:  Boolean;

    procedure SetData(const Value: WideString);

    { UCS16 string is compressible if it contains only
      symbols in range 0..255, i.e. the high byte of each symbol
      is zero. }

    function IsCompressible: Boolean;

    procedure Init;

  public

    Len16:      Boolean;        // set, if the length of this string must occupy two bytes
    Tag:        LongInt;        // not used by this class
    SstIndex:   Integer;        // not used by this class

    constructor Create; overload;
    constructor Create(const S: WideString; UCS16: Boolean); overload;
    destructor Destroy; override;

    procedure AddFormat(Position, Font: LongInt);
    procedure Flush(Stream: TBiffStream); override;

    { Compares itself with a TBiffUCS using
      a created hash. }

    function Equals(s: TBiffObject): Boolean; override;

    { Returns a hash code for this string.
      The created hash code is cached. }

    function GetHashCode: LongInt; override;

    property Data: WideString read FData write SetData;
    property Compress: Boolean read FCompress write FCompress;

  end;

  //
  // STYLE
  //

  TBiffBuiltinStyleId = (
    bsiNormal,
    bsiRowLevel,
    bsiColLevel,
    bsiCOmma,
    bsiCurrency,
    bsiPercent,
    bsiComma0,
    bsiCurrency0,
    bsiHyperlink,
    bsiFHyperlink);

  TBiffStyle = class(TBiffObject)
  public

    XF:       LongInt;
    StyleId:  LongInt;
    Level:    LongInt;
    Name:     WideString;

    constructor Create;
    procedure Flush(Stream: TBiffStream); override;

  end;

  //
  // BLANK
  //

  TBiffCell = class(TBiffObject)
  public

    Row:    LongInt;
    Col:    LongInt;
    XF:     LongInt;

    constructor Create;
    procedure Flush(Stream: TBiffStream); override;

  end;

  //
  // Link Table (EXTRENALBOOK, EXTERNALSHEET, etc.)
  //

  TBiffLinkTable = class(TBiffObject)
  private

    { i-th REF is associated with a sheet named FIntSheetNames[i],
      which has index FIntSheetIndex[i] in the workbook }

    FIntSheetNames: array of string;
    FIntSheetIndex: array of Integer;
    FSheetsCount: Integer;

    function FindSheet(const Name: string): Integer;

    function GetRefsCount: Integer;
    procedure SetRefsCount(n: Integer);
  public

    { The Link Table contains a mapping from internal sheet names to
      indices to REF records. Formulas need this mapping: in expression
      like SheetABC!G8 the sheet name SheetABC is stored indirectly,
      via an associated with it an index to its REF record.  }

    function GetInternalSheetRef(const SheetName: string): Integer;

    { Links a sheet name with a corresponding sheet index. Sheet indices are known
      by TBiffWorkbook, so this method is called by the workbook. }

    procedure SetSheetIndex(const SheetName: string; Index: Integer);

    { Writes itself as a series of record. A link table is repersented with several
      records, unlike the most of other objects, which are represented by one record. }

    procedure Flush(Stream: TBiffStream); override;

    property RefsCount: Integer read GetRefsCount write SetRefsCount;
    property SheetsCount: Integer read FSheetsCount write FSheetsCount;
  end;

  //
  // FORMULA
  //

  TBiffFormulaCell = class(TBiffCell)
  private
    FInst: TStream;
  public
    constructor Create(Instructions: TStream);
    destructor Destroy; override;

    procedure Flush(Stream: TBiffStream); override;
  end;

  { Formulas lexer }

  TBiffFormulaLexemKind =
  (
    flkVoid,      // indicates that a lexem does not exist
    flkSpace,     // several characters that are considered to be whitespaces
    flkName,      // a sequence of letters and numbers: ABS, LOG, but not LOG10, A11
    flkInt,       // an unsigned integer
    flkString,    // a string
    flkOp,        // an operator, like < > <= >= <> & * + -
    flkSymbol     // some special symbol of unrecognized meaning
  );

  TBiffFormulaLexem = record
    Kind: TBiffFormulaLexemKind;
    Text: string;
  end;

  TBiffFormulaLexer = class
  private
    FText: string;
    FPos: Integer;
    FLexems: array of TBiffFormulaLexem;

    function IsAlpha(c: Char): Boolean;
    function IsDigit(c: Char): Boolean;

    function Read(Len: Integer): string;
    function GetChar: Char;
    function NextChar: Char;
    procedure SkipChar;
    function SubStr(Pos, Len: Integer): string;

    procedure Add(const Lex: TBiffFormulaLexem); overload;
    procedure Add(LexKind: TBiffFormulaLexemKind; const LexText: string); overload;

    function AddSpace: Boolean;
    function AddNumber: Boolean;
    function AddSymbol: Boolean;
    function AddName: Boolean;
    function AddString(Quote: Char): Boolean;
    function AddOp: Boolean;

    function GetLexemsCount: Integer;
    function GetLexem(i: Integer): TBiffFormulaLexem;

    procedure Analyse(const Formula: string);
  public
    property Formula: string read FText write Analyse;
    property Count: Integer read GetLexemsCount;
    property Lexems[i: Integer]: TBiffFormulaLexem read GetLexem; default;
  end;

  { Formulas RPN stack }

  TBiffFormulaTokenKind =
  (
    ftkVoid,
    ftkArg,
    ftkOp         // pops a number of args and pushes one result back; Flags = the number of args
  );

  TBiffFormulaOperatorKind =
  (
    fokVoid,
    fokPush,      // pushes to stack one argument

    fokDiv100,    // %
    fokNeg,       // -

    fokAdd,       // +
    fokSub,       // -
    fokDiv,       // /
    fokMul,       // *
    fokPow,       // ^

    fokArea,      // : creates a cell area (A2:G6)
    fokColon,     // : creates a cell area like fokArea, but more general (A2:INDIRECT("G6"))
    fokIsect,     // intersects two ranges, e.g. A1:G5 A2:H7; denoted by one space character
    fokExt,       // ! is used to refer to an external cell; e.g. "Sheet 10"!G6
    fokJoin,      // & joins two strings; e.g. "123" & "abc"

    fokL,         // <
    fokG,         // >
    fokE,         // =
    fokNE,        // <>
    fokLE,        // <=
    fokGE,        // >=

    fokNull,      // null value; it's used for representing a missing argument: SUM(A12,,7)
    fokNumber,    // a floating point number
    fokBool,      // a boolean value; Flags = 1 means "true", Flags = 0 means "false"
    fokString,    // a string
    fokCell,      // a cell reference; Flags: bit 0 = row is relative; bit 1 = column is relative
    fokFunc,      // a function call
    fokId         // the identity operator; pops one arg and pushes it back unmodified
  );

  TBiffFormulaToken = record
    Kind:   TBiffFormulaTokenKind;
    Op:     TBiffFormulaOperatorKind;
    Text:   string;
    Flags:  Integer;
  end;

  TBiffFormulaTokenArray = array of TBiffFormulaToken;

  TBiffFormulaRPNStack = class
  private
    FNumArgs: Integer;                // number of arguments in FCode
    FCode: TBiffFormulaTokenArray;    // resulting program
    FStack: TBiffFormulaTokenArray;   // stack of operators awaiting to being unrolled
    FFrame: TBiffFormulaTokenArray;   // stack of function calls

    procedure Error(const Msg: string);
    procedure Ensure(b: Boolean; const Msg: string = 'RPN stack failed');

    procedure Push(var Res: TBiffFormulaTokenArray; const t: TBiffFormulaToken);
    function  Pop(var a: TBiffFormulaTokenArray): TBiffFormulaToken;
    function  Top(const a: TBiffFormulaTokenArray): TBiffFormulaToken;

    procedure PushOp(Op: TBiffFormulaToken); overload; // pushes an arith op recognizing the precedence order
    procedure PopOp; // moves one operator from FStack to FCode

    procedure Unroll; // unrolls one frame
    function  Joinable(Op: TBiffFormulaOperatorKind): Boolean; // whether Op can be added on top of FStack

    function  OpPriority(Kind: TBiffFormulaOperatorKind): Integer;

    function  GetCount: Integer;
    function  GetInstruction(i: Integer): TBiffFormulaToken;
    function  GetFrameArgs: Integer; // number of available arguments in the current frame
    function  GetFrameOps: Integer; // number of operators in the current frame
  public
    procedure PushArg(Op: TBiffFormulaOperatorKind; const Text: string; Flags: Integer = 0);
    procedure PushOp(Op: TBiffFormulaOperatorKind; const Text: string; NumArgs: Integer = 0); overload;

    procedure PushFrame(const Func: string = ''); // pushes a function call
    procedure PopFrame; // it's called whenever the closing parenthesis occurs

    property Count: Integer read GetCount;
    property Instructions[i: Integer]: TBiffFormulaToken read GetInstruction; default;
  end;

  EBiffFormulaRPNStackError = class(Exception);

  { Formulas parser

    BNF of accepted formulas:

    formula   ::= ppterm (binop ppterm)*
    ppterm    :=  [prefop] term [postop]
    term      ::= cell | area | string | number | func | name | extcell | extarea
    func      ::= [name [int]] "(" [formula] ("," formula)* ")"
    number    ::= int [ "." int ]
    cell      ::= ["$"] name ["$"] int
    extcell   ::= sheet "!" cell
    extarea   ::= sheet "!" area
    area      ::= cell ":" cell
    sheet     ::= string | name
    binop     ::= "+" | "-" | "*" | "/" | "^" | etc.
    prefop    ::= "-" | "+"
    postop    ::= "%"

    (string, int, name - indivisible lexems; they are emitted by the lexer)

    After a formula analysed, the parser builds a sequence of tokens (instructions)
    available through its Tokens property. Each token represents an instruction or an argument
    for an instruction. This a token can have one of two kinds:

      ftkOp     A token of this kind represents an instruction that must be executed
                by a virtual processor. The Flags field defines the number of arguments
                taken.

      ftkArg    A token of this kind represents an argument, which can be a number, a string,
                a cell, the null arguments and so on. This token is not an instruction and
                its only purpose is to be an argument for a token of kind ftkOp.

    Here is an example of tokens produced from the formula "SUM(A3:B4 G5) + SheetABC!$G$8^2":

      cell(A3)        This is an argument token. It represents a cell.
      cell(B4)
      area:2          This is an operator token. It takes two previous tokens, makes an area (A3:B4) and
                      pushes the area to the stack.
      cell(G5)
      push:1          This is an operator token. It takes on previous token (G5) and pushes it to the stack.
      isect:2         This operator pops two values from the stack, intersects them and pushes the result back.
      call(SUM):1     This operator pops one value from the stack, calls SUM and pushes the result to the stack.
      str(SheetABC)   This is an argument token. It represents a string.
      cell(G8):3      Here "3" is the Flags field. It has bits 0 and 1 set. This means that
                      the row and the column are absolute, not relative.
      extcell:2       This operator takes 2 previous tokens ("SheetABC", "$G$8"), makes an external cell
                      reference from them (SheetABC!$G$8) and pushes it onto the stack.
      num(2)          This is an argument.
      pow:2           This operator takes two values from the stack (SheetABC!$G$8 and 2), does exponentiation
                      and pushes the result (SheetABC!$G$8^2) onto the stack.
      add:2           Like as pow:2, but does addition. }

  TBiffFormulaParser = class
  private
    FLexems: array of TBiffFormulaLexem;
    FPos: Integer; // current lexem
    FSavedPos: array of Integer;
    FRPN: TBiffFormulaRPNStack;
    FCode: TBiffFormulaTokenArray;

    function  CreateArgToken(Kind: TBiffFormulaOperatorKind;
      const Text: string = ''; Flags: Integer = 0): TBiffFormulaToken;

    { emits two tokens: arg and push }

    procedure Push(Arg: TBiffFormulaOperatorKind; const Text: string = ''; Flags: Integer = 0);

    { errors reporing }

    procedure Error(const ErrorMsg: string);
    procedure Ensure(b: Boolean; const ErrorMsg: string = 'Invalid formula');

    { lexems traversing }

    function  Lexem(i: Integer = 0): TBiffFormulaLexem; // returns (FPos + i) lexem or flkVoid
    procedure SkipLexem;
    function  SkipLexemIf(const Text: string): Boolean; overload;
    function  SkipLexemIf(Kind: TBiffFormulaLexemKind; out Text: string): Boolean; overload;
    function  IsOp(const Lex: TBiffFormulaLexem; const Text: string): Boolean;
    function  IsArgSep(const Lex: TBiffFormulaLexem): Boolean;

    { parser's state saving/restoring }

    procedure Save;     // saves the parser's state
    procedure Load;     // loads the parser's state
    procedure Discard;  // discards the last saved state

    { operators propeties }

    function  OpKind(const s: string): TBiffFormulaOperatorKind;

    { parsing methods }

    procedure Parse(const s: string);
    procedure BuildLexems(const s: string);
    procedure CleanLexems;
    procedure CopyCodeFromRPNStack;

    function  ReadSym(const s: string): Boolean;
    function  ReadString(out s: string): Boolean;
    function  ReadNumber(out s: string): Boolean;
    function  ReadName(out s: string): Boolean;
    function  ReadOp(out s: string): Boolean;
    function  ReadSheet(out s: string): Boolean;
    function  ReadCell(out t: TBiffFormulaToken): Boolean;

    function  ParseFormula: Boolean;
    function  ParsePPTerm: Boolean;
    function  ParsePrefOp: Boolean;
    function  ParsePostOp: Boolean;
    function  ParseTerm: Boolean;
    function  ParseString: Boolean;
    function  ParseNumber: Boolean;
    function  ParseCell: Boolean;
    function  ParseArea: Boolean;
    function  ParseBinOp: Boolean;
    function  ParseFuncCall: Boolean;
    function  ParseNameConst: Boolean;
    function  ParseExtCell: Boolean;
    function  ParseExtArea: Boolean;

    { property getters/setters }

    function  GetToken(i: Integer): TBiffFormulaToken;
    function  GetTokensCount: Integer;
  public
    property Formula: string write Parse;
    property Tokens[i: Integer]: TBiffFormulaToken read GetToken; default;
    property Count: Integer read GetTokensCount;
  end;

  EBiffFormulaParserError = class(Exception);

  { Formulas code emitter }

  TBiffFormulaCellRef = record
    Row: Integer;     // zero based row index
    Col: Integer;     // zero based column index
    AbsRow: Boolean;  // row index is absolute; in $G7 the index is absolute; in G7 it's relative
    AbsCol: Boolean;  // column index is absolute; in G$7 the index is absolute; in G7 it's relative
  end;

  TBiffFormulaRetType = (frtVoid, frtRef, frtVal, frtArray);

  TBiffFormulaCodeEmitter = class
  private
    FInst: TStream;
    FRetTypeMode: TBiffFormulaRetType;

    procedure Error(const ErrorMsg: string);
    procedure Ensure(b: Boolean; const ErrorMsg: string = 'Cannot emit opcode');
    procedure EnsureCellRange(const Cell: TBiffFormulaCellRef);

    function RelFlags(RelRow, RelCol: Boolean): Byte;

    procedure WriteOpCode(Op: Byte);
    procedure Write(Inst: Cardinal; Len: Cardinal = 1);
    procedure WriteCellRef(const Cell: TBiffFormulaCellRef);
    procedure WriteAreaRef(const Cell1, Cell2: TBiffFormulaCellRef);
  public

    { commands that push a value on the stack }

    procedure Push(Value: Integer); overload;
    procedure Push(Value: Double); overload;
    procedure Push(b: Boolean); overload;
    procedure Push(const s: string); overload;
    procedure PushNull;
    procedure PushCell(const Cell: TBiffFormulaCellRef);
    procedure PushArea(const Cell1, Cell2: TBiffFormulaCellRef);
    procedure PushExtCell(SheetRefId: Integer; const Cell: TBiffFormulaCellRef);
    procedure PushExtArea(SheetRefId: Integer; const Cell1, Cell2: TBiffFormulaCellRef);

    { function calls }

    procedure Call(Func: Cardinal); overload;
    procedure Call(Func, NumArgs: Cardinal); overload;
    procedure CallId; // identity function; emits parethesis for display purposes

    { unary operators }

    procedure Neg;      // unary -
    procedure Div100;   // %

    { binary arithmetic operators }

    procedure Add;      // +
    procedure Sub;      // -
    procedure Mul;      // *
    procedure Divide;   // /
    procedure Pow;      // ^

    { binary comparsion operators }

    procedure CmpL;     // <
    procedure CmpG;     // >
    procedure CmpLE;    // <=
    procedure CmpGE;    // >=
    procedure CmpE;     // =
    procedure CmpNE;    // <>

    { cell operators }

    procedure Intersect; // intersects two areas
    procedure Range;     // :

    { string operators }

    procedure Join;     // &

    property Output: TStream read FInst write FInst;
    property RetMode: TBiffFormulaRetType read FRetTypeMode write FRetTypeMode;
  end;

  EBiffFormulaCodeEmitterError = class(Exception);

  { Functions list.

    todo -cOptimisation: This clumsy class represents a map which stores items
    sorted by key. It can be represented by AVL, red-black or b-tree,
    but because of lack of generics in old Delphi versions,
    there's no easy way adapt existing classes in frxStorage for
    string keys. In far future this class should be replaced with something like
    TAVLTree<string, TBiffFormulaFunc>. }

  TBiffFormulaFunc = record
    Name:     string;
    Id:       Integer;
    MinArgs:  Integer;
    MaxArgs:  Integer;
    Volatile: Boolean;
    RetType:  Char;
    ArgTypes: string;
  end;

  TBiffFormulaFuncArray = array of TBiffFormulaFunc;

  TBiffFormulaFuncList = class
  private
    class procedure Init;
    class procedure Add(const f: TBiffFormulaFunc); overload;
    class function Find(Name: string): Integer;
    class function GetCount: Integer;
    class function GetFunc(i: Integer): TBiffFormulaFunc;
    class procedure SetFunc(i: Integer; const f: TBiffFormulaFunc);
    class procedure SetCount(n: Integer);
  public

    { [MS-XLS] Section 2.5.198.17

      Name      The function name. Letters case doesn't matter - this method upcases all letters.
      Id        The function id. It can be found in the documentation.
      MinArgs   The minimum number of arguments the function takes.
      MaxArgs   The maximum number of arguments the function takes.
      RetType   The result type. See note below.
      ArgTypes  Arguments types. The actual length of this array can be less than MaxArgs. In this
                case all missing entries are assumed to be equal to the last entry; e.g.
                SUM takes up to 30 args and all of them are references, so ArgTypes can be set to
                "r" or "rr" or "rrr" - all these strings are equally assumed to be equal to the string
                with 30 r's - "rrr....rrr".

      Result type of arguments types take one of three values: 'R' (reference), 'V' (value) and 'A' (array).
      See docs for meaning of these types. }

    class procedure Add(Id: Integer; Name: string; MinArgs, MaxArgs: Integer;
      RetType: Char; ArgTypes: string; Volatile: Boolean = False); overload;

    class function Exists(const Name: string): Boolean;
    class function Get(const Name: string): TBiffFormulaFunc;
    class function GetArgType(const Name: string; i: Integer): Char;
  end;

  { Formulas compiler }

  TBiffFormulaCompiler = class
  private
    FParser: TBiffFormulaParser;
    FPos: Integer; // current token
    FEmitter: TBiffFormulaCodeEmitter;
    FCode: TStream;
    FLinkTable: TBiffLinkTable;
    FRetTypes: array of TBiffFormulaRetType;

    function  GetLinkTable: TBiffLinkTable;

    function  Token(i: Integer = 0): TBiffFormulaToken;
    procedure SkipToken;
    procedure SelectToken(i: Integer);

    procedure Error(const Fmt: string; const Args: array of const);
    procedure Ensure(b: Boolean; const ErrorMsg: string = 'Cannot compile formula'); overload;
    procedure Ensure(b: Boolean; const Fmt: string; const Args: array of const); overload;

    function  IsCell(const t: TBiffFormulaToken): Boolean;
    function  IsStr(const t: TBiffFormulaToken): Boolean;

    function  GetCellPos(const t: TBiffFormulaToken): TBiffFormulaCellRef;

    { Excel doesn't perform types coercion. This method finds for each token
      what type it should return. }

    procedure CalcRetTypes;

    procedure Compile(const s: string);
    procedure CompileToken;

    procedure EmitNum(Num: Double);
    procedure EmitOp(Kind: TBiffFormulaOperatorKind; NumArgs: Integer);
    procedure EmitFunc(Name: string; NumArgs: Integer);
    procedure EmitIdFunc(NumArgs: Integer);
    procedure EmitArea(const Cell1, Cell2: TBiffFormulaToken);
    procedure EmitExtCell(const Sheet, Cell: TBiffFormulaToken);
    procedure EmitExtArea(const Sheet, Cell1, Cell2: TBiffFormulaToken);
    procedure EmitPush(const t: TBiffFormulaToken);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToStream(Stream: TStream);

    property Formula: string write Compile;

    { Link table is needed to compile external cell references: SheetABC!G8.
      If the formula doesn't contain such expressions, then LinkTable can be left nil. }

    property LinkTable: TBiffLinkTable read GetLinkTable write FLinkTable;
  end;

  EBiffFormulaCompilerError = class(Exception);

  //
  // LABELSST
  //

  TBiffTextCell = class(TBiffCell)
  public

    SST:    LongInt;

    constructor Create(SST: LongInt);
    procedure Flush(Stream: TBiffStream); override;

  end;

  //
  // NUMBER
  //

  TBiffNumberCell = class(TBiffCell)
  public

    Value:  Double; // IEEE 754 floating-point value (64-bit double precision)

    constructor Create(Value: Double);
    procedure Flush(Stream: TBiffStream); override;

  end;

  //
  // Palette.
  //
  // BIFF document can contain a PALETTE record
  // which specifies an array of used colors.
  // To refer to a color in this array, a color index
  // is used. There're several special built-in
  // colors. Their indexes are defined below.
  //

  TBiffColorIndex = (
    ciBlack,
    ciWhite,
    ciRed,
    ciGreen,
    ciBlue,
    ciYellow,
    ciMagenta,
    ciCyan
 );

  //
  // Font.
  //

  TBiffFontOptions = LongInt;   // See fo*** values

  TBiffFontFamily = (
    ffNone,
    ffRoman,
    ffSwiss,
    ffModern,
    ffScript,
    ffDecorative);

  TBiffFontEscapement = (
    feNone,
    feSuperScript,
    feSubScript);

  TBiffFontUnderline = LongInt; // See fu*** values
  TBiffFontWeight    = LongInt; // See fw*** values

  TBiffFontData = record

    Height:     LongInt;        // Font height in twips = 1/20 of a point
    Options:    Word;           // See TBiffFontOptions
    Color:      Word;           // Index to a color
    Weight:     Word;
    Esc:        TBiffFontEscapement;
    Underline:  TBiffFontUnderline;
    Family:     TBiffFontFamily;
    Charset:    Byte;           // For byte strings

  end;

  TBiffFont = class(TBiffObject)
  public

    Data: TBiffFontData;
    Name: WideString;
    Hash: Integer;
    FontIndex: Integer;         // not used by this class

    constructor Create;
    procedure Flush(Stream: TBiffStream); override;
    function Equals(Font: TBiffObject): Boolean; override;
    function GetHashCode: Integer; override;

    //
    // Returns width in points of a specified string as though
    // it's drawed with this font. On failure, returns 0.
    //
    // This function is complicated and its results
    // should be cached to increase performance.
    //

    function StrWidth(const Str: WideString): LongInt;

  end;

  //
  // XF.
  //
  // Extended cell formatting.
  //

  TBiffXFTypeProt = LongInt;  // See xftp** values
  TBiffXFUsedAttrib = LongInt;// See BiffXfua*** values
  TBiffPatternStyle = LongInt;// See ps*** values

  TBiffXFHAlign = (
    xfhaGeneral,
    xfhaLeft,
    xfhaCentered,
    xfhaRight,
    xfhaFilled,
    xfhaJustified,
    xfhaCAS,                    // Centred across selection
    xfhaDistributed);

  TBiffXFVAlign = (
    xfvaTop,
    xfvaCentered,
    xfvaBottom,
    xfvaJustified,
    xfvaDistributed);

  TBiffXFTextDir = (
    xftdAuto,
    xftdLTR,
    xftdRTL);

  TBiffXFOrientation = (
    xfoNone,
    xfoTop,                     // Letters are stacked top-to-bottom, but not rotated
    xfo90CCW,                   // 90 degrees counterclockwise
    xfo90CW);                  // 90 degrees clockwise

  TBiffLineStyle = (
    lsNone,
    lsThin,
    lsMedium,
    lsDashed,
    lsDotted,
    lsThick,
    lsDouble,
    lsHair,
    lsMediumDashed,
    lsThinDashDotted,
    lsMediumDashDotted,
    lsThinDashDotDotted,
    lsMediumDashDotDotted,
    lsSlantedMediumDashDotted
 );

  TBiffLine = record

    Style:    TBiffLineStyle;
    Color:    Byte;             // Index to a color

  end;

  TBiffXfData = record

    Font:       LongInt;        // Index to FONT record
    Format:     LongInt;        // Index to FORMAT record
    Prot:       Byte;           // See TBiffXFTypeProt
    Parent:     LongInt;        // Index to parent XF. Always -1 in style XFs.
    HAlign:     TBiffXFHAlign;
    WordWrap:   Boolean;
    VAlign:     TBiffXFVAlign;
    Justify:    Boolean;        // Justify last line in justified or distibuted text
    Rotation:   Byte;

    //
    // Indent level is measured in units,
    // each of them equals to length of three
    // blank characters.
    //

    Indent:     Byte;

    Shrink:     Boolean;        // Shrink content to fit into cell
    Direction:  TBiffXFTextDir;
    UsedAttrs:  Byte;           // See TBiffXFUsedAttrib

    L, T, R, B: TBiffLine;      // Left, top, right and bottom lines
    D:          TBiffLine;      // Diagonal line
    LTRB:       Boolean;        // Diagonal line from left-top to right-bottom
    LBRT:       Boolean;        // Diagonal line from left-bottom to right-top

    Patt:       Byte;           // See TBiffPatternStyle and docs
    PattColor:  Word;           // Index to a color
    PattBgColor:Word;           // Index to a color

  end;

  TBiffXF = class(TBiffObject)
  public

    XfIndex:    Integer;        // Index to XF table. Not used by this class.
    Data:       TBiffXfData;
    Hash:       Integer;

    constructor Create;
    procedure Flush(Stream: TBiffStream); override;
    function Equals(XF: TBiffObject): Boolean; override;
    function GetHashCode: Integer; override;

  end;

  //
  // COLINFO
  // Defines formatting for several consequent columns.
  //

  TBiffColInfo = class(TBiffObject)
  public

    First:      LongInt;        // Index to the first column
    Last:       LongInt;        // Index to the last column
    XF:         LongInt;        // Index to an XF record
    Hidden:     Boolean;
    Collapsed:  Boolean;
    Outline:    Byte;           // Must be in 0..7 range
    Width:      LongInt;        // In 1/256 of the zero character of the 0-th font

    constructor Create(Column, XF, Width: LongInt);
    procedure Flush(Stream: TBiffStream); override;

  end;

  //
  // WINDOW1
  //

  TBiffWindowOptions = LongInt; // See wo*** values

  TBiffWindow = class
  public

    HPos, VPos:     LongInt;    // In twips = 1/20 of a point
    Width, Height:  LongInt;    // In twips = 1/20 of a point
    ActiveSheet:    LongInt;
    FirstTab:       LongInt;
    SelSheet:       LongInt;
    TabWidth:       LongInt;    // Worksheet tab bar. 1/1000 of window width

    Visible:        Boolean;    // Window
    Open:           Boolean;    // Open/minimized
    HSBVisible:     Boolean;    // Hor. scroll bar
    VSBVisible:     Boolean;    // Vert. scroll bar
    TabVisible:     Boolean;    // Worksheet tab

    constructor Create;
    procedure Flush(Stream: TBiffStream);

  end;

  //
  // ROW
  //

  TBiffRow = class(TBiffObject)
  public

    Row:      LongInt;        // Index to this row
    Cells:    TObjList;       // List of TBiffCell
    FirstCol: LongInt;        // Index to column of the first cell
    LastCol:  LongInt;        // Index to column of the last cell
    Height:   LongInt;        // Height in twips
    Outline:  Byte;           // Valid outline levels are 0..7
    XF:       LongInt;        // Index to an XF record
    Hidden:   Boolean;

    { These two fields are used by TBiffSheet, not
      by this class.

        - FirstCell   - stream position of the first cell of this row
        - Offset      - stream position of this row }

    FirstCell: LongWord;
    Offset:   LongWord;

    constructor Create;
    destructor Destroy; override;
    procedure Flush(Stream: TBiffStream); override;

  end;

  //
  // PAGESETUP
  //

  TBiffPageOrientation = (
    bpoLandscape,
    bpoPortrait
  );

  TBiffPageSetup = class(TBiffObject)
  public

    Size:       TBiffPaperSize;
    Orient:     TBiffPageOrientation;
    Copies:     LongInt;
    Colored:    Boolean;      // Print in color or black and white
    Xdpi:       Word;
    Ydpi:       Word;
    PrintInRows:Boolean;
    Draft:      Boolean;      // Draft print quality
    CellNotes:  Boolean;      // Print cell notes
    SheetNotes: Boolean;      // Print notes at the end of the sheet

    constructor Create;
    procedure Flush(Stream: TBiffStream); override;

  end;

  { FILEPASS

    TBiffRC4 allows to encrypt excel workbook data using the RC4 cipher.
    This class is serialized into the FILEPASS record. }

  TBiffRC4 = class(TBiffObject)
  private

    { Specific data to be written to the output
      FILEPASS record }

    FSalt:      array[1..16] of Byte;   // Random data
    FVerifier:  array[1..16] of Byte;   // Encrypted verifier (that is random data)
    FHash:      array[1..16] of Byte;   // Encrypted MD5 of the verifier

    { Partial hash required for creating
      the encryption key. }

    FTruncHash: array[1..5] of Byte;

    { RC4 cipher }

    FCipher:    TCryptoRC4;

    { Positioon of the currently encrypted byte }

    FTail:      LongInt;
    FBlockId:   LongInt;

    procedure EncryptEx(Data: Pointer; Size: LongInt);
    procedure MD5(out Hash; const Data; Size: Integer);

  public

    { RC4 cipher performs a self test. If the test has failed,
      an exception is raised. }

    constructor Create;
    destructor Destroy; override;

    { Creates a partial hash as specified in [MS-OFFCRYPTO] section 2.3.6.2. }

    procedure Prepare(const Password; PassLen: LongInt);

    { Creates the FILEPASS record in a BIFF stream.
      This routine must be called after the Prepare routine. }

    procedure Flush(Stream: TBiffStream); override;

    { Initializes the RC4 cipher with a specified key }

    procedure Init(Key: Pointer; Len: LongInt);

    { Initializes the RC4 cipher as specified in [MS-OFFCRYPTO] section 2.3.6.2.
      This routine must be called after the Prepare routine, because
      Initialize uses the partial hash that's generated by Prepare }

    procedure Initialize(BlockId: LongWord);

    { Encrypts a data block "in place".
      The RC4 cipher generates a crypto stream (based on the password),
      that is combined with the original data with XOR operation.
      If Data is nil, the crypto stream is generated but is written
      nowhere. }

    procedure Encrypt(Data: Pointer; Size: LongInt); cdecl;

    { This routine encrypts a BIFF stream  as specified in
      [MS-XLS] section 2.2.10. }

    procedure EncryptStream(Stream: TBiffStream);
  end;

  TBiffBounds = record

    FR:       LongInt;        // First row
    LR:       LongInt;        // Last row
    FC:       LongInt;        // First column
    LC:       LongInt;        // Last column

  end;

  //
  // WINDOW2
  //

  TBiffWindow2 = class(TBiffObject)
  public

    Options:  Word;           // See BiffWo*** values
    FirstRow: Word;           // First visible row
    FirstCol: Word;           // First visible column
    GridCol:  Word;           // Grid line color
    MFPBP:    Word;           // Cached magnification factor in page break preview
    CMFNV:    Word;           // Cached magnification factor in normal view

    constructor Create;
    procedure Flush(Stream: TBiffStream); override;

  end;

  { BIFF sheet.

    Any XLS BIFF document consists of a few sheets.
    A sheet is represented by class TBiffSheet.
    An instance of this class can be accessed by a single
    thread at a time. }

  TBiffSheetKind = LongInt;   // See sk*** values

  TBiffSheetVisibility = (
    svVisible,
    svHidden,
    svVeryHidden);            // Such a sheet can only be shown/hidden by VB macro

  TBiffMargin = record

    Left:   Double;           // Inches
    Top:    Double;
    Right:  Double;
    Bottom: Double;

  end;

  TBiffWorkbook = class;

  TBiffSheet = class(TBiffObject)
  private

    FRows:      TObjList;         // List of TBiffRow. Sorted by the row index.
    FCols:      TObjList;         // List of TBiffColInfo
    FMrgCells:  array of TRect;   // rectangles of merged cells
    FDGroup:    TEscherGroup;     // Drawing group. This class doesn't delete DGroup.
    FOwner:     TBiffWorkbook;
    FKind:      TBiffSheetKind;
    FVis:       TBiffSheetVisibility;
    FPB:        TList;            // List of LongInt (indexes to row where a page break occurs)
    FStrCount:  LongInt;          // number of text cells

    FLastReadRow: Integer;

    procedure SetColWidth(i: LongInt; w: LongInt);
    procedure SetRowHeight(i: LongInt; h: LongInt);
    function GetColWidth(i: LongInt): LongInt;
    function GetRowHeight(i: LongInt): LongInt;
    function GetRow(Index: LongInt): TBiffRow; // optimised for sequential access

  public

    Name:       WideString;
    Margin:     TBiffMargin;
    PageSetup:  TBiffPageSetup;
    View:       TBiffWindow2;
    Bounds:     TBiffBounds;      // Bounding rectangle of all cells

    { Index to the corresponding SHEET
      record which is placed in the Workbook
      globals stream. This index is formed by
      TBiffSheet.Flush method and used by
      TBiffWorkbook.Flush method to alter the
      SHEET record after all sheets are written
      to a BIFF stream. }

    RecIndex: Integer;

    constructor Create(Owner: TBiffWorkbook);
    destructor Destroy; override;

    //
    // Adda a new cell to the list of cells.
    //

    procedure AddCell(Cell: TBiffCell);

    { Merges a rectangle area of cells }

    procedure MergeCells(Rect: TRect);

    //
    // Adds a colinfo block to the list of colinfos
    //

    procedure AddColInfo(Info: TBiffColInfo);

    { Adds a new drawing to the sheet }

    function AddDrawing: TEscherShape;

    //
    // This function is to be called at the very end
    // to create a BIFF8 sheet substream.
    //

    procedure Flush(Stream: TBiffStream); override;

    { Adds a page break }

    procedure AddPageBreak(Row: LongInt);

    { Returns the index to the last row in this sheet.
      If there're no rows, -1 is returned. }

    function LastRowIndex: LongInt;

    { Column width is measured in 1/256 of width of the '0'
      character written by the 0-th font specified in
      the appropriate workbook. Row height is measured in twips.

        1 point = 20 twips

      When trying to read a non-existing column or a row, zero is
      returned. When trying to change size of an inaccesible column
      or a row (negative indexes, etc.), nothing is changed. }

    property ColWidth[i: LongInt]: LongInt read GetColWidth write SetColWidth;
    property RowHeight[i: LongInt]: LongInt read GetRowHeight write SetRowHeight;

    property TextCellsCount: Integer read FStrCount;
    property Visibility: TBiffSheetVisibility read FVis;
    property Kind: TBiffSheetKind read FKind;

  end;

  { BIFF workbook.

    The workbook is an Excel document containing some information
    and several sheets. The workbook is represented by class TBiffWorkbook.
    An instance of this class can be accessed by multiple threads at a moment.
    This is achieved via usage of critical section objects. }

  TBiffWorkbook = class(TBiffObject)
  private

    { Shared Strings Table }

    SST:      TObjList;         // List of TBiffUCS
    SSTHash:  TListHashTable;

    Win:      TBiffWindow;
    Sheets:   TObjList;         // List of TBiffSheet
    Styles:   TObjList;         // List of TBiffStyle
    FFormats: TObjList;         // List of TBiffUCS
    Escher:   TEscherStorage;   // Global Escher storage
    FLinkTbl: TBiffLinkTable;   // Contains external references; created on demand

    { Shared Fonts Table

      First four FONT records are reserved
      and have special meaning }

    Fonts:    TObjList;         // List of TBiffFont
    FontHash: TListHashTable;

    { Shared XFs Table

      Any BIFF document must contain
      at least 16 XF records used specially. }

    XFs:      TObjList;         // List of TBiffXF
    XFHash:   TListHashTable;

    { Shared Colors Table

      When anywhere a color is needed, this color
      is placed to PALETTE record and index to
      the color in the palette is used. Colors with
      indexes 0..7 are built-in, colors with indexes
      8..$3F are user defined.

      PALETTE record may be omitted and then the
      default Excel palette is used.

      To add a color to the palette
      use TboffWorkbook.AddColor method. }

    Palette:    array[0..BiffPaletteSize-1] of LongWord;
    PalUsed:    LongWord;

    { Optionally, the workbook can be encrypted }

    FCipher:    TBiffRC4;

    { The following critical section objects are used for
      mutual access to different parts of a workbook. }

    {$IFNDEF FMX}
    FCsStrings:   TRTLCriticalSection;
    FCsFonts:     TRTLCriticalSection;
    FCsXFs:       TRTLCriticalSection;
    FCsSheets:    TRTLCriticalSection;
    FCsStyles:    TRTLCriticalSection;
    FCsColors:    TRTLCriticalSection;
    FCsPictures:  TRTLCriticalSection;
    FCsFormats:   TRTLCriticalSection;
    {$ELSE}
    FCsStrings:   TObject;
    FCsFonts:     TObject;
    FCsXFs:       TObject;
    FCsSheets:    TObject;
    FCsStyles:    TObject;
    FCsColors:    TObject;
    FCsPictures:  TObject;
    FCsFormats:   TObject;
    {$ENDIF}

    { TBiffWorkbook can be accesses by a few threads.
      In this case, threads must synchronize access to
      the shared workbook object. Here is a typical example
      of use these functions:

        • thread wants to add a string to a workbook
        • thread calls TBiffWorkbook.LockSst
        • thread adds a string via TBiffWorkbook.AddString
        • thread calls TBiffWorkbook.UnlockSst

      TBiffWorkbook performs these locks automatically, i.e.
      client threads may not know about these. }

    procedure LockSst;
    procedure UnlockSst;

    procedure LockFonts;
    procedure UnlockFonts;

    procedure LockXfs;
    procedure UnlockXfs;

    procedure LockSheets;
    procedure UnlockSheets;

    procedure LockStyles;
    procedure UnlockStyles;

    procedure LockColors;
    procedure UnlockColors;

    procedure LockPictures;
    procedure UnlockPictures;

    procedure LockFormats;
    procedure UnlockFormats;

    { Internal  methods }

    function GetLinkTable: TBiffLinkTable;
    function GetSheetsCount: LongInt;
    function GetSheet(Index: LongInt): TBiffSheet;
    function GetFontsCount: LongInt;
    function GetFont(Index: LongInt): TBiffFont;
    procedure SetFormat(i: TBiffFormatIndex; const s: WideString);
    function AddBlip(Blip: TEscherPicture): LongInt;
    function AddColorInternal(C: LongWord; ANotFind: boolean): LongInt;

  public

    constructor Create;
    destructor Destroy; override;

    { Workbook can be protected with a password.
      The password can contain up to 255 unicode characters. }

    procedure SetPassword(const s: WideString);

    { BIFF8 contains all used strings in the SST table,
      all cells with strings refers to the SST table.
      If a cell needs to use a string, this function must be called.

      Returns an SST index.
      Multithread safe. }

    function AddString(S: TBiffUCS): LongWord; overload;
    function AddString(S: WideString): LongWord; overload;

    { In BIFF8 fonts have specific indexing: the 4-th FONT record
      is omitted. Use this function to add a new font and receive
      its valid index.

      Returns a font index.
      Multithread safe. }

    function AddFont(F: TBiffFont): LongWord;

    { These routines add a specified kind of object to the workbook.
      They are multithread safe. }

    function AddXF(X: TBiffXF): LongWord;
    function AddSheet(S: TBiffSheet): LongWord;
    function AddStyle(S: TBiffStyle): LongInt;
    function AddFormat(const FormatStr: WideString;
      FormatIndex: TBiffFormatIndex = -1): TBiffFormatIndex;

    { Adds a color to the palette and returns an index to the
      added color. If the color already exists, the index to
      the existing color is returned. If the palette is full
      and the color cannot be added, an index to the nearest color
      is returned.

      Byte structure of the color: RR GG BB AA
      Multithread safe. }

    function AddColor(C: LongWord; ANotFind: boolean = False): LongInt;

    { Adds a picture to the workbook.
      AddBitmap is applicable for BMP, PNG, JPEG, TIFF, etc.
      Multithread safe. }

    function AddBitmap(Kind: TEscherBlipKind; Contents: TStream): LongInt;
    {$ifndef FMX}
    {$ifndef fpc}
    function AddMetafile(Metafile: Graphics.TMetafile): LongInt;
    {$endif}
    {$endif}

    { Returns a count of strings occured in the workbook.
      Identical strings are counted as much as they occur.

      Multithread safe. }

    function StringsCount: LongInt;

    { Writes the workbook to data stream in BIFF8 format. }

    procedure Flush(Stream: TBiffStream); override;

    procedure AddDefPalette(ANotFind: Boolean);

    { The folowing properties are multithread safe. }

    property SheetsCount: LongInt read GetSheetsCount;
    property Sheet[Index: LongInt]: TBiffSheet read GetSheet;

    property FontsCount: LongInt read GetFontsCount;
    property Font[Index: LongInt]: TBiffFont read GetFont;

    property Format[i: TBiffFormatIndex]: WideString write SetFormat;

    property LinkTable: TBiffLinkTable read GetLinkTable;

  end;


{ MS Excel treats only #10 character as
  a line break, other characters (e.g. #13)
  it displays as squares (unknown symbols).

  This function deletes #13 chracaters.
  Returns a new length of the string.

  You must not use this function for an empty strings. }

procedure ValidateLineBreaks(var s: WideString);

{ Finds the nearest color within an array of colors. }

function NearestColor(Colors: Pointer; Count, Color: LongWord): LongWord;

implementation

{ This global variable is actually a static veriable of TBiffFormulaFuncList.
  Delphi4 doesn't allow writing "class var FuncArray: ..." - that's why this var
  is declared as global. However, when Delphi4 is left behind, this var should be
  moved to the owner class. }

var
  FuncArray: TBiffFormulaFuncArray;

//
// The following types describes structure
// of several specific BIFF records.
//

type

  //
  // BOF
  //

  TBiffrBOF = packed record

    Version:  Word;
    Kind:     Word;
    Build:    Word;
    Year:     Word;
    Flags1:   LongWord;         // See BiffBoff values
    Flags2:   LongWord;

  end;

  //
  // WINDOW1
  // 16 bytes
  //

  TBiffrWindow = packed record

    HPos:     Word;
    VPos:     Word;
    Width:    Word;
    Height:   Word;
    Flags:    Word;
    Active:   Word;             // Index to active worksheet
    First:    Word;             // Index of first visible tab
    Sel:      Word;             // Number of selected worksheets
    TabW:     Word;             // Width of the tabbar in 1/1000 of window width

  end;

  //
  // FONT
  // 14+ bytes
  // See docs for complete information
  //

  TBiffrFont = packed record

    Height:   Word;             // Height in 1/20 point
    Opts:     Word;             // Option flags, see TBiffFontOptions
    Color:    Word;             // Color index
    Weight:   Word;             // Weight in range 100..1000.
    Esc:      Word;             // Escapement, see TBiffFontEscapement
    Under:    Byte;             // Underline, see TBiffFontUnderline
    Family:   Byte;             // Font family, see TBiffFontFamily
    CharSet:  Byte;
    NotUsed:  Byte;

    {
    Name:     Unicode 16-bit length string
    }

  end;

  //
  // XF
  // 20 bytes
  //

  TBiffrXF = packed record

    Font:     Word;
    Format:   Word;
    Style:    Word;           // For bits 0-2 see TBiffXFTypeProt
    Align:    Byte;           // See TBiffXFHAlign and TBiffXFVAlign
    Rotation: Byte;
    Indent:   Byte;
    UsedAttrs:Byte;           // See TBiffXFUsedAttrib
    BStyle1:  LongWord;       // 0-3 bytes of border style
    BStyle2:  LongWord;       // 4-7 bytes of border style
    Pattern:  Word;

  end;

  TBiffrBuiltinStyle = packed record

    XF:       Word;             // 15-th bit is 1
    Id:       Byte;             // See TBiffBuiltinStyleId
    Level:    Byte;

  end;

  //
  // SHEET
  // 6+ bytes
  // See docs for complete information
  //

  TBiffrSheet = packed record

    Offset:   LongWord;         // Offset to BOF record
    Vis:      Byte;             // See TBiffSheetVisibility
    Kind:     Byte;             // See TBiffSheetKind

    {
    Name:     Unicode 8-bit length string
    }

  end;

  //
  // DIMENSION
  // 14 bytes
  //

  TBiffrDimension = packed record

    FirstRow: LongWord;
    LastRow:  LongWord;         // Increased by 1
    FirstCol: Word;
    LastCol:  Word;             // Increased by 1
    NotUsed:  Word;

  end;

  //
  // WINDOW2
  // 18 bytes
  //

  TBiffrWindow2 = packed record

    Options:  Word;             // See TBiffWindowOptions
    FirstRow: Word;             // First visible row
    FirstCol: Word;             // First visible column
    GridCol:  Word;             // Color index of grid lines
    NotUsed1: Word;
    CMFPBV:   Word;             // Can be 0
    CMFNV:    Word;             // Can be 0
    NotUsed2: LongWord;

  end;

  //
  // ROW
  // 16 bytes
  //

  TBiffrRow = packed record

    Row:      Word;
    FirstCol: Word;
    LastCol:  Word;             // Increased by 1
    Height:   Word;             // See docs
    NotUsed:  Word;
    NotUsed2: Word;
    Format:   LongWord;         // See docs

  end;

  //
  // SST
  // 8+ bytes
  //

  TBiffrSST = packed record

    WBSCount: LongWord;
    SSTCount: LongWord;

    {
    UCS16 strings
    }

  end;

  //
  // BLANK
  //

  TBiffrBlank = packed record

    Row:      Word;
    Column:   Word;
    XF:       Word;

  end;

  PBiffrBlank = ^TBiffrBlank;

  //
  // FORMULA
  // [MS-XLS] 2.4.127
  //

  TBiffrFormula = packed record

    Cell:     TBiffrBlank;
    Value:    Double;
    Flags:    Word;             // set to 0
    Cache:    Cardinal;         // set to 0
    InstLen:  Word;             // size in bytes of formula's program

  end;

  //
  // LABELSST
  //

  TBiffrLabelSST = packed record

    Row:      Word;
    Column:   Word;
    XF:       Word;
    SSTIndex: LongWord;

  end;

  PBiffrLabelSST = ^TBiffrLabelSST;

  //
  // NUMBER
  //

  TBiffrNumber = packed record

    Row:      Word;
    Column:   Word;
    XF:       Word;
    Value:    Double;           // IEEE 754 floating-point value (64-bit double precision)

  end;

  PBiffrNumber = ^TBiffrNumber;

  //
  // RK
  //

  TBiffrRK = packed record

    Row:      Word;
    Column:   Word;
    XF:       Word;
    RK:       LongWord;

  end;

  PBiffrRK = ^TBiffrRK;

  //
  // COLINFO
  // 12 bytes
  //

  TBiffrColInfo = packed record

    First:    Word;
    Last:     Word;
    Width:    Word;
    XF:       Word;
    Options:  Word;
    Reserved: Word;

  end;

  //
  // Cells block
  // 8 bytes
  // Used by MERGEDCELLS record
  //

  TBiffrCellsBlock = packed record

    FR:       Word;             // Index to first row
    LR:       Word;             // Index to last row
    FC:       Word;             // Index to first column
    LC:       Word;             // Index to last column

  end;

  //
  // PAGESETUP
  // 34 bytes
  //

  TBiffrPageSetup = packed record

    Size:     Word;             // See docs for the list of available values
    Scaling:  Word;             // In percents
    StartNum: Word;             // Start page number
    Width:    Word;             // 0 = use default
    Height:   Word;             // 0 = use default
    Options:  Word;
    Xdpi:     Word;
    Ydpi:     Word;
    Header:   Double;           // Header margin in inches
    Footer:   Double;           // Footer margin in inches
    Copies:   Word;             // Number of copies to print

  end;

  TPackedRect = packed record

    Left:     LongInt;
    Top:      LongInt;
    Right:    LongInt;
    Bottom:   LongInt;

  end;

  //
  // Common Object Record
  // 22 bytes
  //

  TBiffrCOR = packed record

    Kind:     Word;               // See BiffGo values
    Id:       Word;               // Object Id
    Opts:     Word;
    Unused1:  array[1..16] of Byte;

  end;

  { RC4 encryption header
    52 bytes }

  TBiffrRC4 = packed record

    Version:  LongWord;           // Must be $00010001
    Salt:     array[1..16] of Byte;
    Verifier: array[1..16] of Byte;
    Hash:     array[1..16] of Byte;

  end;

const

  //
  // Graphics object types
  //

  BiffGoPicture         = 8;

  //
  // The following constants are BIFF record types,
  // i.e. TBiffRecord.Id value.
  //

  BiffIdFormula         = $0006;  // FORMULA
  BiffIdRString         = $00d6;  // RSTRING
  BiffIdRK              = $027E;  // RK
  BiffIdNumber          = $0203;  // NUMBER
  BiffIdMulRK           = $00Bd;  // MULRK
  BiffIdMulBlank        = $00BE;  // MULBLANK
  BiffIdLabelSST        = $00Fd;  // LABELSST
  BiffIdBOF             = $0809;  // BOF
  BiffIdWindow          = $003d;  // WINDOW1
  BiffIdFont            = $0031;  // FONT
  BiffIdXF              = $00E0;  // XF
  BiffIdStyle           = $0293;  // STYLE
  BiffIdSheet           = $0085;  // SHEET
  BiffIdEOF             = $000A;  // EOF
  BiffIdDim             = $0200;  // DIMENSION
  BiffIdWindow2         = $023E;  // WINDOW2
  BiffIdRow             = $0208;  // ROW
  BiffIdDBCell          = $00d7;  // DBCELL
  BiffIdContinue        = $003C;  // CONTINUE
  BiffIdBlank           = $0201;  // BLANK
  BiffIdBoolErr         = $0205;  // BOOLERR
  BiffIdLabel           = $0204;  // LABEL
  BiffIdSST             = $00FC;  // SST
  BiffIdPalette         = $0092;  // PALETTE
  BiffIdColInfo         = $007d;  // COLINFO
  BiffIdMergedCells     = $00E5;  // MERGEDCELLS
  BiffIdLeftMargin      = $0026;  // LEFTMARGIN
  BiffIdRightMargin     = $0027;  // RIGHTMARGIN
  BiffIdTopMargin       = $0028;  // TOPMARGIN
  BiffIdBottomMargin    = $0029;  // BOTTOMMARGIN
  BiffIdPageSetup       = $00a1;  // PAGESETUP
  BiffIdFormat          = $041e;  // FORMAT
  BiffIdEscher          = $00eb;  // ESCHER
  BiffIdDrawing         = $00ec;  // DRAWING
  BiffIdGObj            = $005d;  // GOBJ
  BiffIdCOR             = $0015;  // Common Object Record
  BiffIdHorPageBreak    = $001b;  // HORIZONTALPAGEBREAK
  BiffIdFilePass        = $002f;  // FILEPASS
  BiffIdUserExcl        = $0194;  // USEREXCL
  BiffIdFileLock        = $0195;  // FILELOCK
  BiffIdInterfaceHdr    = $00e1;  // INTERFACEHDR
  BiffIdRRDInfo         = $0196;  // RRDINFO
  BiffIdRRDHead         = $0138;  // RRDHEAD
  BiffIdInterfaceEnd    = $00e2;  // INTERFACEEND
  BiffIdMms             = $00c1;  // MMS
  BiffIdWriteAccess     = $005c;  // WRITEACCESS
  BiffIdCodepage        = $0042;  // CODEPAGE
  BiffIdDsf             = $0161;  // DSF
  BiffIdRrd             = $013d;  // RRD
  BiffIdWinProt         = $0019;  // WINPROT
  BiffIdProt            = $0012;  // PROTECT
  BiffIdPassword        = $0013;  // PASSWORD
  BiffIdProtRev         = $01af;  // PROTREV
  BiffIdProtRevPass     = $01bc;  // PROTREVPASS
  BiffIdBackup          = $0040;  // BACKUP
  BiffIdHideObj         = $008d;  // HIDEOBJ
  BiffIdDate1904        = $0022;  // DATE1904
  BiffIdCalcPrec        = $000e;  // CALCPRECISION
  BiffIdRefreshAll      = $01b7;  // REFRESHALL
  BiffIdBookBool        = $00da;  // BOOKBOOL
  BiffIdUserElf         = $0160;  // USERELF
  BiffIdCountry         = $008c;  // COUNTRY
  BiffIdExtSst          = $00ff;  // EXTSST
  BiffIdCalcMode        = $000d;  // CALCMODE
  BiffIdCalcCount       = $000c;  // CALCCOUNT
  BiffIdCalcRefMode     = $000f;  // CALCREFMODE
  BiffIdCalcIter        = $0011;  // CALCITER
  BiffIdCalcDelta       = $0010;  // CALCDELTA
  BiffIdSaveRecalc      = $005f;  // SAVERECALC
  BiffIdPrintRowCol     = $002a;  // PRINTROWCOL
  BiffIdPrintGrid       = $002b;  // PRINTGRID
  BiffIdGridSet         = $0082;  // GRIDSET
  BiffIdGuts            = $0080;  // GUTS
  BiffIdDefRowHeight    = $0225;  // DEFROWHEIGHT
  BiffIdWsBool          = $0081;  // WSBOOL
  BiffIdHeader          = $0014;  // HEADER
  BiffIdFooter          = $0015;  // FOOTER
  BiffIdHCenter         = $0083;  // HCENTER
  BiffIdVCenter         = $0084;  // VCENTER
  BiffIdDefColWidth     = $0055;  // DEFCOLWIDTH
  BiffIdExtBook         = $01ae;  // EXTERNALBOOK
  BiffIdExtSheet        = $0017;  // EXTERNALSHEET

function NearestColor(Colors: Pointer; Count, Color: LongWord): LongWord;

  function Dist(i, c: LongWord): LongWord;
  var
    j: Integer;
  begin
    c := c xor PLongWord(LongWord(Colors) + i*SizeOf(Color))^;
    Result := 0;

    for j := 1 to 4 do
    begin
      Inc(Result, c and $ff);
      c := c shr 8;
    end;
  end;

var
  i: Integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do
    if Dist(i, Color) < Dist(Result, Color) then
      Result := i;
end;

{ Sets or resets the N-th bit of a variable }

procedure SetBit(P: Pointer; N: Cardinal; V: Boolean);
var
  B: PByte;
  M: Byte;
begin
  B := PByte(Cardinal(P) + N div 8);
  M := 1 shl (N mod 8);

  if V then
    B^ := B^ or M
  else
    B^ := B^ and not M
end;

procedure ValidateLineBreaks(var s: WideString);
var
  i, j: Cardinal;
begin
  j := 1;

  for i := 1 to Length(s) do
    if s[i] <> #13 then
    begin
      s[j] := s[i];
      Inc(j);
    end;

  SetLength(s, j - 1);
end;

{ TBiffObject }

class procedure TBiffObject.FlushList(list: TObjList; Stream: TBiffStream);
var
  i: LongInt;
begin
  for i := 0 to list.Count - 1 do
    TBiffObject(list[i]).Flush(Stream);
end;

function TBiffObject.GetHashCode: LongInt;
begin
  Result := 0;
end;

function TBiffObject.Equals(s: TBiffObject): Boolean;
begin
  Result := Self = s;
end;

{ TBiffRC4 }

constructor TBiffRC4.Create;
var
  r: array[1..4] of LongWord;
  i: LongInt;
  g: TCryptoCMWC;
begin
  FCipher := TCryptoRC4.Create;
  g := TCryptoCMWC.Create;

  for i := 1 to 4 do
    r[i] := g.Next;

  Move(r, FSalt, 16);

  for i := 1 to 4 do
    r[i] := g.Next;

  Move(r, FVerifier, 16);

  g.Free;
end;

destructor TBiffRC4.Destroy;
begin
  FCipher.Free;
end;

procedure TBiffRC4.MD5(out Hash; const Data; Size: Integer);
begin
  TCryptoHash.Hash('MD5', Hash, 16, Data, Size);
end;

procedure TBiffRC4.Prepare(const Password; PassLen: Integer);
var
  h: array[1..16] of Byte;
  b: array[1..336] of Byte;
  i: LongInt;
begin
  MD5(h, Password, PassLen);

  Move(h, b, 5);
  Move(FSalt, b[6], 16);

  for i := 1 to 15 do
    Move(b, b[1 + 21 * i], 21);

  MD5(h, b, 336);
  Move(h, FTruncHash, 5);

  Initialize(0);
  MD5(FHash, FVerifier, 16);
  Encrypt(@FVerifier, 16);
  Encrypt(@FHash, 16);
end;

procedure TBiffRC4.Init(Key: Pointer; Len: LongInt);
begin
  FCipher.Init(Key^, Len);
end;

procedure TBiffRC4.Initialize(BlockId: LongWord);
var
  h: array[1..9] of Byte;
  k: array[1..16] of Byte;
begin
  Move(FTruncHash, h, 5);
  Move(BlockId, h[6], 4);
  MD5(k, h, 9);
  Init(@k, 16);

  FTail := 1024;
  FBlockId := BlockId;
end;

procedure TBiffRC4.Encrypt(Data: Pointer; Size: LongInt); cdecl;
begin
  FCipher.Encrypt(Data, Size);
end;

procedure TBiffRC4.EncryptEx(Data: Pointer; Size: LongInt);
var
  n: LongInt;
begin
  { If the data block fits to the current 1024-bytes block,
    encrypt the data and exit. }

  if Size <= FTail then
  begin
    Encrypt(Data, Size);
    Dec(FTail, Size);
    if FTail = 0 then
      Initialize(FBlockId + 1);

    Exit;
  end;

  { If the data block does not fit the current 1024-bytes block,
    it must be encrypted by parts. Firstly, encrypt a part of
    the data that fits the current 1024-bytes block. }

  Encrypt(Data, FTail);
  if Data <> nil then Data := Pointer(LongInt(Data) + FTail);
  Dec(Size, FTail);
  Initialize(FBlockId + 1);

  { Split the data into 1024-bytes blocks and encrypt them
    consequently. At the beginning of each 1024-bytes block,
    the RC4 cipher must be reinitialized using the index to
    the current 1024-bytes block. }

  while Size > 0 do
  begin
    n := Size;
    if n > FTail then n := FTail;
    Encrypt(Data, n);
    Dec(Size, n);
    Dec(FTail, n);

    if Data <> nil then
      Data := Pointer(LongInt(Data) + n);

    if FTail = 0 then
      Initialize(FBlockId + 1);
  end;
end;

procedure TBiffRC4.EncryptStream(Stream: TBiffStream);

  function GetSkipSize(RecId: TBiffRecId; RecSize: Cardinal): Cardinal;
  begin
    case RecId of
      BiffIdFilePass,
      BiffIdUserExcl,
      BiffIdFileLock,
      BiffIdInterfaceHdr,
      BiffIdRRDInfo,
      BiffIdRRDHead,
      BiffIdBOF:
        Result := RecSize;

      BiffIdSheet:
        Result := 4;

      else
        Result := 0;
    end;
  end;

var
  i: LongInt;
  n: LongWord;
  RecData: TMemoryStream;
begin
  Initialize(0);
  RecData := TMemoryStream.Create;

  try
    for i := 0 to Stream.Count - 1 do
      with Stream[i] do
      begin
        n := GetSkipSize(Id, Size);

        RecData.Position := 0;
        SaveToStream(RecData);

        EncryptEx(nil, 4 + n); // 4 byte header and a few first bytes are not encrypted
        EncryptEx(Pointer(Cardinal(RecData.Memory) + n), Size - n);

        RecData.Position := 0;
        LoadFromStream(RecData);
      end;
  finally
    RecData.Free;
  end;
end;

procedure TBiffRC4.Flush(Stream: TBiffStream);
var
  r: TBiffrRC4;
begin
  FillChar(r, SizeOf(r), 0);

  r.Version := $00010001;
  Move(FSalt, r.Salt, 16);
  Move(FVerifier, r.Verifier, 16);
  Move(FHash, r.Hash, 16);

  with Stream.Add(BiffIdFilePass) do
  begin
    WriteConst(1, 2);
    Write(r, SizeOf(r));
  end;
end;

{ TBiffWindow2 }

constructor TBiffWindow2.Create;
begin
  Options := BiffWoGridLines or BiffWoHeaders or BiffWoZeros or
    BiffWoAutoGridColor or BiffWoOutline or BiffWoSelected or
    BiffWoActive;

  { There are only 64 colors in the palette.
    The following assignment emphasizes the fact
    that the grid color is selected automatically }

  GridCol := 64;
end;

procedure TBiffWindow2.Flush(Stream: TBiffStream);
var
  r: TBiffrWindow2;
begin
  FillChar(r, SizeOf(r), 0);

  r.Options   := Options;
  r.FirstRow  := FirstRow;
  r.FirstCol  := FirstCol;
  r.GridCol   := GridCol;
  r.CMFPBV    := MFPBP;
  r.CMFNV     := CMFNV;

  Stream.Add(BiffIdWindow2).Write(r, SizeOf(r));
end;

{ TBiffPageSetup }

constructor TBiffPageSetup.Create;
begin
  Size    := BiffPsA4;
  Orient  := bpoPortrait;
  Colored := True;
  Xdpi    := 300;
  Ydpi    := 300;
  Copies  := 1;
end;

procedure TBiffPageSetup.Flush(Stream: TBiffStream);
var
  r: TBiffrPageSetup;
  o: PWord;
begin
  FillChar(r, SizeOf(r), 0);

  r.Scaling   := 100;
  r.Size      := Size;
  r.StartNum  := 0;
  r.Width     := 1;
  r.Height    := 1;
  r.Xdpi      := Xdpi;
  r.Ydpi      := Ydpi;
  r.Header    := 0.0;
  r.Footer    := 0.0;
  r.Copies    := Word(Copies);
  r.Options   := 0;

  o := @r.Options;

  SetBit(o, 0, PrintInRows);
  SetBit(o, 1, Orient = bpoPortrait);
  SetBit(o, 3, not Colored);
  SetBit(o, 4, Draft);
  SetBit(o, 5, CellNotes);
  SetBit(o, 7, True);
  SetBit(o, 9, SheetNotes);
  SetBit(o, 10, True);        // Do not print errors

  Stream.Add(BiffIdPageSetup).Write(r, SizeOf(r));
end;

{ TBiffNumberCell }

procedure TBiffNumberCell.Flush(Stream: TBiffStream);
var
  r: TBiffrNumber;
begin
  FillChar(r, SizeOf(r), 0);

  r.Row     := Row;
  r.Column  := Col;
  r.XF      := XF;
  r.Value   := Value;

  Stream.Add(BiffIdNumber).Write(r, SizeOf(r));
end;

constructor TBiffNumberCell.Create(Value: Double);
begin
  Self.Value := Value;
end;

{ TBiffStyle }

constructor TBiffStyle.Create;
begin
  Level := -1;
end;

procedure TBiffStyle.Flush(Stream: TBiffStream);
var
  bs:   TBiffrBuiltinStyle;
  ucs:  TBiffUCS;
begin
  FillChar(bs, SizeOf(bs), 0);

  //
  // Built-in style
  //

  if Name = '' then
  begin
    bs.XF     := XF or $8000;
    bs.Id     := StyleId;
    bs.Level  := Byte(Level);

    Stream.Add(BiffIdStyle).Write(bs, SizeOf(bs));
    Exit;
  end;

  //
  // User defined style
  //

  Stream.Add(BiffIdStyle).Write(XF, 2);

  ucs := TBiffucs.Create(Name, True);
  ucs.Flush(Stream);
  ucs.Free;
end;

{ TBiffColInfo }

constructor TBiffColInfo.Create(Column, XF, Width: LongInt);
begin
  First     := Column;
  Last      := Column;
  Self.XF   := XF;
  Self.Width:= Width;
  Hidden    := False;
  Collapsed := False;
  Outline   := 0;
end;

procedure TBiffColInfo.Flush(Stream: TBiffStream);
var
  R: TBiffrColInfo;
begin
  FillChar(R, SizeOf(R), 0);

  R.First   := First;
  R.Last    := Last;
  R.Width   := Width;
  R.XF      := XF;
  R.Options := 0;

  SetBit(@R.Options, 0, Hidden);
  SetBit(@R.Options, 12, Collapsed);
  R.Options := R.Options and ((7 and Outline) shl 8);

  Stream.Add(BiffIdColInfo).Write(R, SizeOf(R));
end;

{ TBiffWindow }

constructor TBiffWindow.Create;
begin
  HPos        := 0;
  VPos        := 0;
  Width       := 15600;
  Height      := 8190;
  ActiveSheet := 0;
  FirstTab    := 0;
  SelSheet    := 1;
  TabWidth    := 400;

  Visible     := True;
  Open        := True;
  HSBVisible  := True;
  VSBVisible  := True;
  TabVisible  := True;
end;

procedure TBiffWindow.Flush(Stream: TBiffStream);
var
  R: TBiffrWindow;
  F: Word;
begin
  FillChar(R, SizeOf(R), 0);

  R.HPos      := HPos;
  R.VPos      := VPos;
  R.Width     := Width;
  R.Height    := Height;
  R.Active    := ActiveSheet;
  R.First     := FirstTab;
  R.Sel       := SelSheet;
  R.TabW      := TabWidth;

  F := 0;

  SetBit(@F, 0, not Visible);
  SetBit(@F, 1, not Open);
  SetBit(@F, 3, HSBVisible);
  SetBit(@F, 4, VSBVisible);
  SetBit(@F, 5, TabVisible);

  F := 56;
  R.Flags := F;
  Stream.Add(BiffIdWindow).Write(R, SizeOf(R));
end;

{ TBiffCell }

constructor TBiffCell.Create;
begin
  XF := 15; // default XF for cells
end;

procedure TBiffCell.Flush(Stream: TBiffStream);
var
  R: TBiffrBlank;
begin
  FillChar(R, SizeOf(R), 0);

  R.Row       := Row;
  R.Column    := Col;
  R.XF        := XF;

  with Stream.Add(BiffIdBlank) do
    Write(R, SizeOf(R));
end;

{ TBiffTextCell }

constructor TBiffTextCell.Create(SST: LongInt);
begin
  inherited Create;
  Self.SST := SST;
end;

procedure TBiffTextCell.Flush(Stream: TBiffStream);
var
  R: TBiffrLabelSST;
begin
  FillChar(R, SizeOf(R), 0);

  R.Row       := Row;
  R.Column    := Col;
  R.XF        := XF;
  R.SSTIndex  := SST;

  Stream.Add(BiffIdLabelSST).Write(R, SizeOf(R));
end;

{ TBiffFont }

constructor TBiffFont.Create;
begin
  with Data do
  begin
    Weight      := Word(fwNormal);
    Name        := 'Arial';
    Color       := Word(ciBlack);
    Height      := 12*20;
  end;
end;

procedure TBiffFont.Flush(Stream: TBiffStream);
var
  R: TBiffrFont;
begin
  FillChar(R, SizeOf(R), 0);

  with Data do
  begin
    { Font height must be zero or in bounds [20, 8191].
      Required by [MS-XLS] Section 2.4.122. }

    if Height < 20 then
      R.Height := 0
    else if Height > 8191 then
      R.Height := 8191
    else
      R.Height := Height;

    R.Opts      := Options;
    R.Color     := Color;
    R.Weight    := Weight;
    R.Esc       := Word(Esc);
    R.Under     := Byte(Underline);
    R.Family    := Byte(Family);
    R.CharSet   := Charset;
  end;

  Stream.Add(BiffIdFont).Write(R, SizeOf(R));

  with TBiffUCS.Create(Name, False) do
  try
    Compress := False;
    Flush(Stream);
  finally
    Free;
  end;
end;

function TBiffFont.Equals(Font: TBiffObject): Boolean;
begin
  Result := GetHashCode = Font.GetHashCode;
end;

function TBiffFont.GetHashCode: Integer;
var
  H: TCryptoHash;
begin
  if Hash = 0 then
  begin
    H := TCryptoJenkins.Create;

    try
      H.Push(Data, SizeOf(Data));

      if Name <> '' then
        H.Push(Name[1], SizeOf(Name[1]) * Length(Name));

      H.GetDigest(Hash, SizeOf(Hash));
    finally
      H.Free;
    end;
  end;

  Result := Hash;
end;

{$ifndef fpc}
{$ifndef FMX}
function TBiffFont.StrWidth(const Str: WideString): LongInt;
var
  dc: HDC;
  font,
  OldFont: HFONT;
  fam:    LongWord; // Font family
  sz:     SIZE;     // String size
begin
  with Data do
  begin
    Result := 0;

    dc := CreateDC('DISPLAY', nil, nil, nil);
    if dc = 0 then
      Exit;

    case Family of
      ffRoman:      fam := FF_ROMAN;
      ffSwiss:      fam := FF_SWISS;
      ffModern:     fam := FF_MODERN;
      ffScript:     fam := FF_SCRIPT;
      ffDecorative: fam := FF_DECORATIVE;
      else          fam := FF_DONTCARE;
    end;

    font := CreateFontW(
      Height div 10,            // Height in logical units
      0,                        // Width in logical units
      0,                        // Escapement in 1/10 of a degree
      0,                        // Orientation in 1/10 of a degree
      Weight,                   // Font weight

      //
      // Flags: italic, underline, struck out
      //

      Word(foItalic) and Options,
      LongWord(Underline <> fuNone),
      Word(foStruckOut) and Options,

      Charset,                  // Font charset
      OUT_DEFAULT_PRECIS,       // Output precision
      CLIP_DEFAULT_PRECIS,      // Clip precision
      DEFAULT_QUALITY,          // Output quality
      fam,                      // Font family
      PWideChar(Name));         // Font name

    if font = 0 then
    begin
      DeleteDC(dc);
      Exit;
    end;

    OldFont := SelectObject(dc, font);

    if GetTextExtentPoint32W(dc, PWideChar(Str), Length(Str), sz) then
      Result := sz.cx;

    DeleteObject(SelectObject(dc, OldFont));
    DeleteDC(dc);
  end;
end;
{$else}
function TBiffFont.StrWidth(const Str: WideString): LongInt;
var
  Layout: TTextLayout;
  Styles: TFontStyles;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    Layout.WordWrap := False;
    Layout.Trimming := TTextTrimming.ttNone;
    Layout.Font.Size := Data.Height * 72 / 960;
    Styles := [];
    if Word(foItalic) and Data.Options <> 0 then
      Styles := Styles + [TFontStyle.fsItalic];
    if Data.Underline <> fuNone then
      Styles := Styles + [TFontStyle.fsUnderline];
    if Word(foStruckOut) and Data.Options <> 0 then
      Styles := Styles + [TFontStyle.fsStrikeOut];
    Layout.Font.Style := Styles;
    Layout.Font.Family := Name;
    Layout.Text := Str;
    Layout.EndUpdate;
    Result := Ceil(Layout.TextRect.Width);
  finally
    Layout.Free;
  end;
end;
{$endif}
{$else}
function TBiffFont.StrWidth(const Str: WideString): LongInt;
var
  dc: HDC;
  font,
  OldFont: HFONT;
  fam:    LongWord; // Font family
  sz:     TSize;    // String size
  LF:     TLogFont;
begin
  with Data do
  begin
    Result := 0;

    dc := GetDC(0);
    if dc = 0 then
      Exit;

    case Family of
      ffRoman:      fam := FF_ROMAN;
      ffSwiss:      fam := FF_SWISS;
      ffModern:     fam := FF_MODERN;
      ffScript:     fam := FF_SCRIPT;
      ffDecorative: fam := FF_DECORATIVE;
      else          fam := FF_DONTCARE;
    end;

    LF.lfHeight := Height div 10;            // Height in logical units
    LF.lfWidth := 0;                         // Width in logical units
    LF.lfEscapement := 0;                    // Escapement in 1/10 of a degree
    LF.lfOrientation := 0;                   // Orientation in 1/10 of a degree
    LF.lfWeight := Weight;                   // Font weight

    //
    // Flags: italic, underline, struck out
    //

    if Word(foItalic) and Options <> 0 then
      LF.lfItalic := 1
    else
      LF.lfItalic:= 0;
    if Underline <> fuNone then
      LF.lfUnderline := 1
    else
      LF.lfUnderline := 0;
    if Word(foStruckOut) and Options <> 0 then
     LF.lfStrikeOut := 1
    else
      LF.lfStrikeOut := 0;

    LF.lfCharSet := Charset;                    // Font charset
    LF.lfOutPrecision := OUT_DEFAULT_PRECIS;    // Output precision
    LF.lfClipPrecision := CLIP_DEFAULT_PRECIS;  // Clip precision
    LF.lfQuality := DEFAULT_QUALITY;            // Output quality
    LF.lfPitchAndFamily := fam;                 // Font family
    LF.lfFaceName := UTF8Encode(Name);          // Font name

    font := CreateFontIndirect(LF);

    if font = 0 then
    begin
      ReleaseDC(0, dc);
      Exit;
    end;

    OldFont := SelectObject(dc, font);

    if GetTextExtentPoint32(dc, PChar(UTF8Encode(Str)), Length(Str), sz) then
      Result := sz.cx;

    DeleteObject(SelectObject(dc, OldFont));
    ReleaseDC(0, dc);
  end;
end;
{$endif}

{ TBiffXF }

constructor TBiffXF.Create;
begin
  Data.Parent   := $FFF;
  Data.HAlign   := xfhaLeft;
  Data.VAlign   := xfvaTop;
  Data.WordWrap := True;
  Data.Prot     := Byte(xftpCellLocked);
end;

procedure TBiffXF.Flush(Stream: TBiffStream);
var
  X: TBiffrXF;
begin
  FillChar(X, SizeOf(X), 0);

  with Data do
  begin
    X.Font      := Font;
    X.Format    := Format;
    X.Style     := Prot and 7 or (Parent shl 4);
    X.Rotation  := Rotation;
    X.UsedAttrs := Byte(UsedAttrs shl 2);

    if (Prot and xftpStyle) <> 0 then
      X.UsedAttrs := not X.UsedAttrs;

    X.Align     :=
      $03 and Byte(HAlign) or
      $08 and (Byte(WordWrap) shl 3) or
      $70 and (Byte(VAlign) shl 4) or
      $80 and (Byte(Justify) shl 7);

    X.Indent    :=
      $0f and Indent or
      $10 and (Byte(Shrink) shl 4) or
      $c0 and (Byte(Direction) shl 6);

    X.BStyle1   :=
      $0000000f and LongWord(L.Style) or
      $000000f0 and (LongWord(R.Style) shl 4) or
      $00000f00 and (LongWord(T.Style) shl 8) or
      $0000f000 and (LongWord(B.Style) shl 12) or
      $007f0000 and (LongWord(L.Color) shl 16) or
      $3f800000 and (LongWord(R.Color) shl 23) or
      $40000000 and (LongWord(LTRB) shl 30) or
      $80000000 and (LongWord(LBRT) shl 31);

    X.BStyle2   :=
      $0000007f and LongWord(T.Color) or
      $00003f80 and (LongWord(B.Color) shl 7) or
      $001fc000 and (LongWord(D.Color) shl 14) or
      $01e00000 and (LongWord(D.Style) shl 21) or
      $fc000000 and (LongWord(Patt) shl 26);

    X.Pattern   :=
      $007f and Word(PattColor) or
      $3f80 and (Word(PattBgColor) shl 7);
  end;

  Stream.Add(BiffIdXF).Write(X, SizeOf(X));
end;

function TBiffXF.Equals(XF: TBiffObject): Boolean;
begin
  Result := GetHashCode = XF.GetHashCode;
end;

function TBiffXF.GetHashCode: Integer;
begin
  if Hash = 0 then
    TCryptoHash.Hash('Jenkins', Hash, SizeOf(Hash), Data, SizeOf(Data));

  Result := Hash;
end;

{ TBiffRow }

constructor TBiffRow.Create;
begin
  Cells := TObjList.Create;
  XF := 15;
end;

destructor TBiffRow.Destroy;
begin
  Cells.Free;
end;

procedure TBiffRow.Flush(Stream: TBiffStream);

  function GetHeight: Word;
  begin
    Result := $8000;
    if Height > 0 then Result := Height and $7FFF;
  end;

  function GetFormat: LongWord;
  var
    f: LongWord;
  begin
    f :=
      $00000007 and Outline or
      $0FFF0000 and (XF shl 16);

    SetBit(@f, 5, Hidden);
    SetBit(@f, 6, Height > 0);
    //SetBit(@f, 7, XF >= 0);
    SetBit(@f, 8, True);

    Result := f;
  end;

var
  Rec: TBiffrRow;
begin
  FillChar(Rec, SizeOf(Rec), 0);

  Rec.Row       := Row;
  Rec.FirstCol  := FirstCol;
  Rec.LastCol   := LastCol + 1;
  Rec.Height    := GetHeight;
  Rec.Format    := GetFormat;

  Stream.Add(BiffIdRow).Write(Rec, SizeOf(Rec));
end;

{ TBiffWorkbook }

constructor TBiffWorkbook.Create;
var
  i: LongInt;
  x: TBiffXF;
begin
  Sheets  := TObjList.Create;
  Fonts   := TObjList.Create;
  XFs     := TObjList.Create;
  SST     := TObjList.Create;
  Styles  := TObjList.Create;
  FFormats := TObjList.Create;
  Win     := TBiffWindow.Create;
  Escher  := TEscherStorage.Create;

  { Hashes }

  XFHash  := TListHashTable.Create;
  SSTHash := TListHashTable.Create;
  FontHash := TListHashTable.Create;

  { Critical section objects }

  {$ifdef FMX}
  FCsStrings := TObject.Create;
  FCsFonts := TObject.Create;
  FCsXFs := TObject.Create;
  FCsSheets := TObject.Create;
  FCsStyles := TObject.Create;
  FCsColors := TObject.Create;
  FCsPictures := TObject.Create;
  FCsFormats := TObject.Create;
  {$else}
    {$ifndef fpc}
    InitializeCriticalSection(FCsStrings);
    InitializeCriticalSection(FCsFonts);
    InitializeCriticalSection(FCsXFs);
    InitializeCriticalSection(FCsSheets);
    InitializeCriticalSection(FCsStyles);
    InitializeCriticalSection(FCsColors);
    InitializeCriticalSection(FCsPictures);
    InitializeCriticalSection(FCsFormats);
    {$else}
    InitCriticalSection(FCsStrings);
    InitCriticalSection(FCsFonts);
    InitCriticalSection(FCsXFs);
    InitCriticalSection(FCsSheets);
    InitCriticalSection(FCsStyles);
    InitCriticalSection(FCsColors);
    InitCriticalSection(FCsPictures);
    InitCriticalSection(FCsFormats);
    {$endif}
  {$endif}

  { 8 built-in colors }

  PalUsed := 0;

  AddColor($000000);          // black
  AddColor($FFFFFF);          // white
  AddColor($0000FF);          // red
  AddColor($00FF00);          // green
  AddColor($FF0000);          // blue
  AddColor($00FFFF);          // yellow
  AddColor($FF00FF);          // magenta
  AddColor($FFFF00);          // cyan

// add main colors  
  AddColor($000000, True);          // black
  AddColor($FFFFFF, True);          // white
  AddColor($0000FF, True);          // red
  AddColor($00FF00, True);          // green
  AddColor($FF0000, True);          // blue
  AddColor($00FFFF, True);          // yellow
  AddColor($FF00FF, True);          // magenta
  AddColor($FFFF00, True);          // cyan

  { 16 built-in XFs }

  for i := 0 to 15 do
  begin
    x := TBiffXF.Create;

    with x.Data do
    begin
      Prot      := Prot or Byte(xftpStyle);
      UsedAttrs := UsedAttrs or Byte(BiffXfuaFont);
    end;

    XFs.Add(x);
  end;

  with TBiffXF(XFs[0]).Data do
  begin
    UsedAttrs := $ff;
  end;

  with TBiffXF(XFs[15]).Data do
  begin
    Parent    := 0;
    Prot      := Byte(xftpCellLocked);
    UsedAttrs := 0;
  end;

  { 1 built-in style for 0-th XF }

  AddStyle(TBiffStyle.Create);

  { 4 built-in fonts }

  Fonts.Add(TBiffFont.Create);
  Fonts.Add(TBiffFont.Create);
  Fonts.Add(TBiffFont.Create);
  Fonts.Add(TBiffFont.Create);
end;

destructor TBiffWorkbook.Destroy;
begin
  Sheets.Free;
  Fonts.Free;
  XFs.Free;
  SST.Free;
  Styles.Free;
  Win.Free;
  FFormats.Free;
  Escher.Free;
  FCipher.Free;
  SSTHash.Free;
  XFHash.Free;
  FontHash.Free;
  FLinkTbl.Free;

  {$ifdef FMX}
  FCsStrings.Free;
  FCsFonts.Free;
  FCsXFs.Free;
  FCsSheets.Free;
  FCsStyles.Free;
  FCsColors.Free;
  FCsPictures.Free;
  FCsFormats.Free;
  {$else}
    {$ifdef fpc}
    DoneCriticalsection(FCsStrings);
    DoneCriticalsection(FCsFonts);
    DoneCriticalsection(FCsXFs);
    DoneCriticalsection(FCsSheets);
    DoneCriticalsection(FCsStyles);
    DoneCriticalsection(FCsColors);
    DoneCriticalsection(FCsPictures);
    DoneCriticalsection(FCsFormats);
    {$else}
    DeleteCriticalsection(FCsStrings);
    DeleteCriticalsection(FCsFonts);
    DeleteCriticalsection(FCsXFs);
    DeleteCriticalsection(FCsSheets);
    DeleteCriticalsection(FCsStyles);
    DeleteCriticalsection(FCsColors);
    DeleteCriticalsection(FCsPictures);
    DeleteCriticalsection(FCsFormats);
    {$endif}
  {$endif}
end;

procedure TBiffWorkbook.LockSst;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsStrings);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsStrings);
  {$endif}
end;

procedure TBiffWorkbook.UnlockSst;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsStrings);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsStrings);
  {$endif}
end;

procedure TBiffWorkbook.LockSheets;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsSheets);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsSheets);
  {$endif}
end;

procedure TBiffWorkbook.UnlockSheets;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsSheets);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsSheets);
  {$endif}
end;

procedure TBiffWorkbook.LockXfs;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsXfs);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsXfs);
  {$endif}
end;

procedure TBiffWorkbook.UnlockXfs;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsXfs);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsXfs);
  {$endif}
end;

procedure TBiffWorkbook.LockColors;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsColors);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsColors);
  {$endif}
end;

procedure TBiffWorkbook.UnlockColors;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsColors);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsColors);
  {$endif}
end;

procedure TBiffWorkbook.LockStyles;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsStyles);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsStyles);
  {$endif}
end;

procedure TBiffWorkbook.UnlockStyles;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsStyles);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsStyles);
  {$endif}
end;

procedure TBiffWorkbook.LockPictures;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsPictures);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsPictures);
  {$endif}
end;

procedure TBiffWorkbook.UnlockPictures;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsPictures);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsPictures);
  {$endif}
end;

procedure TBiffWorkbook.LockFonts;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsFonts);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsFonts);
  {$endif}
end;

procedure TBiffWorkbook.UnlockFonts;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsFonts);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsFonts);
  {$endif}
end;

procedure TBiffWorkbook.LockFormats;
begin
  {$ifdef FMX}
  TMonitor.Enter(FCsFormats);
  {$else}
    {$ifdef fpc}System.{$endif}EnterCriticalSection(FCsFormats);
  {$endif}
end;

procedure TBiffWorkbook.UnlockFormats;
begin
  {$ifdef FMX}
  TMonitor.Exit(FCsFormats);
  {$else}
    {$ifdef fpc}System.{$endif}LeaveCriticalSection(FCsFormats);
  {$endif}
end;

procedure TBiffWorkbook.SetPassword(const s: WideString);
begin
  if s = '' then
  begin
    FCipher.Free;
    FCipher := nil;
    Exit;
  end;

  if FCipher = nil then
    FCipher := TBiffRC4.Create;

  with FCipher do
    Prepare(s[1], 2 * Length(s));
end;

function TBiffWorkbook.AddSheet(S: TBiffSheet): LongWord;
begin
  Assert(S <> nil);
  LockSheets;
  Result := Sheets.Add(S);
  UnlockSheets;
end;

function TBiffWorkbook.AddXF(X: TBiffXF): LongWord;
var
  p: TBiffXF;
begin
  LockXfs;

  p := TBiffXF(XFHash.SetValue(X.GetHashCode, X));

  { The XF is new for this workbook }

  if p = nil then
  begin
    Result := XFs.Add(X);
    X.XfIndex := Result;
  end

  { The XF already exists in the workbook }

  else
  begin
    Result := p.XfIndex;
    X.Free;
  end;

  UnlockXfs;
end;

function TBiffWorkbook.AddFont(F: TBiffFont): LongWord;
var
  p: TBiffFont;
begin
  LockFonts;

  p := TBiffFont(FontHash.SetValue(f.GetHashCode, Pointer(f)));

  { The font is already in the workbook }

  if p = nil then
  begin
    Result := Fonts.Add(f);
    f.FontIndex := Result;
  end
  else

  { The font is new for the workbook }

  begin
    Result := p.FontIndex;
    f.Free;
  end;

  if Result > 3 then
    Inc(Result);

  UnlockFonts;
end;

procedure TBiffWorkbook.SetFormat(i: TBiffFormatIndex; const s: WideString);
begin
  AddFormat(s, i);
end;

function TBiffWorkbook.AddFormat(const FormatStr: WideString;
  FormatIndex: TBiffFormatIndex): TBiffFormatIndex;
var
  i: LongInt;
  s: TBiffUCS;
begin
  if FormatStr = '' then
  begin

    { This is the general format.
      See the documentation for
      the complete list of 164
      predefined format values }

    Result := 0;
    Exit;
  end;

  LockFormats;

  for i := 0 to FFormats.Count - 1 do
  begin
    s := TBiffUCS(FFormats[i]);

    if (FormatIndex < 0) and (s.Tag = FormatIndex) then
    begin
      UnlockFormats;
      raise Exception.CreateFmt('Format #%d already exists',
        [FormatIndex]);
    end;

    if s.Data = FormatStr then
    begin
      UnlockFormats;
      Result := s.Tag;
      Exit;
    end;
  end;

  s := TBiffUCS.Create(FormatStr, True);
  s.Tag := FormatIndex;
  LockFormats;

  if s.Tag < 0 then
    s.Tag := BiffUserFormat + FFormats.Count;

  FFormats.Add(s);
  UnlockFormats;
  Result := s.Tag;
end;

function TBiffWorkbook.AddBlip(Blip: TEscherPicture): LongInt;
begin
  LockPictures;

  with Escher.AddImage(Blip) do
  begin
    Inc(RefCount);
    Result := Index;
  end;

  UnlockPictures;
end;

function TBiffWorkbook.AddBitmap(Kind: TEscherBlipKind; Contents: TStream): LongInt;
var
  p: TEscherBitmap;
begin
  p := TEscherBitmap.Create;
  p.Kind := Kind;
  p.CopyFrom(Contents, 0);
  Result := AddBlip(p);
end;

{$ifndef FMX}
{$ifndef fpc}
function TBiffWorkbook.AddMetafile(Metafile: Graphics.TMetafile): LongInt;
var
  p: TEscherMetafile;
  fx, fy: Extended;
begin
  p := TEscherMetafile.Create;
  p.Kind := EscherBkEMF;
  p.Compr := False;
  Metafile.Enhanced := True;
  Metafile.SaveToStream(p);

  with p.Bounds do
  begin
    Left    := 0;
    Top     := 0;
    Right   := Metafile.Width;
    Bottom  := Metafile.Height;
  end;

  fx := 2.54 * 360000 / 96;
  fy := 2.54 * 360000 / 96;

  with p.MFSize do
  begin
    X := Round(Metafile.Width * fx);
    Y := Round(Metafile.Height * fy);
  end;

  Result := AddBlip(p);
end;
{$endif}
{$endif}

function TBiffWorkbook.GetFontsCount: LongInt;
begin
  Result := Fonts.Count;
end;

function TBiffWorkbook.GetLinkTable: TBiffLinkTable;
begin
  if FLinkTbl = nil then
    FLinkTbl := TBiffLinkTable.Create;

  Result := FLinkTbl;
  Assert(Result <> nil);
end;

function TBiffWorkbook.GetFont(Index: LongInt): TBiffFont;
begin
  if (Index < 0) or (Index >= Fonts.Count) then
  begin
    Result := nil;
    Exit;
  end;

  LockFonts;
  Result := TBiffFont(Fonts[Index]);
  UnlockFonts;
end;

function TBiffWorkbook.GetSheetsCount: LongInt;
begin
  Result := Sheets.Count;
end;

function TBiffWorkbook.GetSheet(Index: LongInt): TBiffSheet;
begin
  if (Index < 0) or (Index >= Sheets.Count) then
  begin
    Result := nil;
    Exit;
  end;

  LockSheets;
  Result := TBiffSheet(Sheets[Index]);
  UnlockSheets;
end;

function TBiffWorkbook.AddString(S: WideString): LongWord;
begin
  Result := AddString(TBiffUCS.Create(S, True));
end;

function TBiffWorkbook.AddString(S: TBiffUCS): LongWord;
var
  p: TBiffUCS;
begin
  LockSst;

  p := TBIffUCS(SSTHash.SetValue(S.GetHashCode, S));

  { If the string aleready exists }

  if p <> nil then
  begin
    S.Free;
    Result := p.SstIndex;
  end
  else

  { If the string is new for the workbook }

  begin
    Result := SST.Add(S);
    S.SstIndex := Result;
  end;

  UnlockSst;
end;

function TBiffWorkbook.AddStyle(S: TBiffStyle): LongInt;
begin
  LockStyles;
  Result := Styles.Add(S);
  UnlockStyles;
end;

function TBiffWorkbook.AddColor(C: LongWord; ANotFind: boolean = False): LongInt;
begin
  LockColors;
  Result := AddColorInternal(C, ANotFind);
  UnlockColors;
end;

function TBiffWorkbook.AddColorInternal(C: LongWord; ANotFind: boolean): LongInt;
var
  i: LongInt;
begin
  C := {$ifndef FMX}ColorToRGB(Integer(C)){$else}C{$endif} and $ffffff;

  { Find the nearest color.

    Note that the new color is found starting from the Palette[7],
    not from Palette[0], as may be expected. Excel don't like
    colors with indexes 0..6, that's why these palette indexes are
    ignored in this search. }

  if PalUsed < 8 then
    i := -1
  else
    if ANotFind then
      i := PalUsed
    else
      i := 7 + NearestColor(@Palette[7], PalUsed - 7, C);

  //
  // If an exact match found, return the index.
  //
  if not ANotFind then
  begin

    if (i >= 0) and (Palette[i] = C) then
    begin
      Result := i;
      Exit;
    end;

  //
  // If no exact match found but palette is full,
  // return an index to the nearest color.
  //

    if (i >= 0) and (PalUsed = BiffPaletteSize) then
    begin
      Result := i;
      Exit;
    end;
  end;

  //
  // If no exact match found and the palette is not full,
  // add the new color.
  //

  Palette[PalUsed] := C;
  Result := PalUsed;
  Inc(PalUsed);
end;

function TBiffWorkbook.StringsCount: LongInt;
var
  i: LongInt;
begin
  LockSst;
  Result := 0;

  for i := 0 to Sheets.Count - 1 do
    Inc(Result, TBiffSheet(Sheets[i]).TextCellsCount);

  UnlockSst;
end;

procedure TBiffWorkbook.Flush(Stream: TBiffStream);

  procedure WriteInt(RecId: TBiffRecId; Value, Size: LongInt);
  begin
    Stream.Add(RecId).Write(Value, Size);
  end;

  procedure WriteFonts;
  begin
    TBiffObject.FlushList(Fonts, Stream);
  end;

  procedure WriteXFs;
  begin
    TBiffObject.FlushList(XFs, Stream);
  end;

  procedure WriteSheets;
  var
    S:    TBiffSheet;
    R:    TBiffrSheet;
    UCS:  TBiffUCS;
    i:    LongInt;
  begin
    for i := 0 to Sheets.Count - 1 do
    begin
      S := TBiffSheet(Sheets[i]);

      FillChar(R, SizeOf(R), 0);

      R.Offset  := 0; // Anything here. This will be overwritten. See docs.
      R.Vis     := Byte(S.Visibility);
      R.Kind    := Byte(S.Kind);

      S.RecIndex := Stream.Count;
      Stream.Add(BiffIdSheet).Write(R, SizeOf(R));

      UCS := TBiffUCS.Create(S.Name, False);
      UCS.Flush(Stream);
      UCS.Free;
    end;
  end;

  procedure WriteSST;
  var
    R:    TBiffrSST;
  begin
    if SST.Count = 0 then
      Exit;

    FillChar(R, SizeOf(R), 0);

    R.WBSCount := StringsCount;
    R.SSTCount := SST.Count;

    Stream.Add(BiffIdSST).Write(R, SizeOf(R));
    TBiffObject.FlushList(SST, Stream);
  end;

  procedure WritePalette;
  var
    n: Integer;
  begin
    with Stream.Add(BiffIdPalette) do
    begin
      n := System.Length(Palette) - 8;
      Assert(n = 56); // required by [MS-XLS] Section 2.4.188

      WriteConst(n, 2);
      Write(Palette[8], 4 * n);
    end;
  end;

  procedure WriteStyles;
  begin
    TBiffObject.FlushList(Styles, Stream);
  end;

  procedure WriteFormats;
  var
    ucs:  TBiffUCS;
    i:    LongInt;
  begin
    for i := 0 to FFormats.Count - 1 do
    begin
      ucs := FFormats[i];
      Stream.Add(BiffIdFormat).Write(ucs.Tag, 2);
      ucs.Flush(Stream);
    end;
  end;

  procedure WriteEscher;
  var
    es:   TEscherStream;
    ms:   TMemoryStream;
  begin
    if Escher.Empty then
      Exit;

    es := TEscherStream.Create;
    ms := TMemoryStream.Create;

    Escher.Flush(es);
    es.Flush(ms);

    Stream.Add(BiffIdEscher).Write(ms.Memory^, ms.Size);

    ms.Free;
    es.Free;
  end;

  procedure WriteFilePass;
  begin
    if FCipher <> nil then
      FCipher.Flush(Stream);
  end;

  procedure WriteInterface;
  begin
    with Stream do
    begin
      Add(BiffIdInterfaceHdr).WriteConst(1200, 2);
      Add(BiffIdMms).WriteConst(0, 2);
      Add(BiffIdInterfaceEnd);
    end;
  end;

  { [MS-XLS] 2.4.349 }

  procedure WriteAccess;
  begin
    with Stream.Add(BiffIdWriteAccess) do
    begin
      WriteConst($02, 2);
      WriteConst($00, 2);
      WriteCOnst($20, 1);
      WriteConst($20, 1);

      while Size < 112 do
        WriteConst($20, 1);
    end;
  end;

  procedure WriteRRD;
  var
    i: LongInt;
  begin
    if SheetsCount > BiffRrdMaxCount then
      Exit;

    with Stream.Add(BiffIdRrd) do
      for i := 0 to SheetsCount - 1 do
        WriteConst(i + 1, 2);
  end;

  procedure WriteProt;
  begin
    with Stream do
    begin
      Add(BiffIdWinProt).WriteConst(0, 2);
      Add(BiffIdProt).WriteConst(0, 2);
      Add(BiffIdPassword).WriteConst(0, 2);
      Add(BiffIdProtRev).WriteConst(0, 2);
      Add(BiffIdProtRevPass).WriteConst(0, 2);
    end;
  end;

  procedure WriteExtSST;
  begin
    { todo: EXTSST not emitted yet }
  end;

  procedure WriteLinkTable;
  var
    i: Integer;
  begin
    if FLinkTbl = nil then
      Exit;

    FLinkTbl.SheetsCount := SheetsCount;

    for i := 0 to Sheets.Count - 1 do
      FLinkTbl.SetSheetIndex(TBiffSheet(Sheets[i]).Name, i);

    FLinkTbl.Flush(Stream);
  end;

var
  i: LongInt;
begin
  Stream.AddBOF(bkWBGlobals);         // BOF
  WriteFilePass;                      // FILEPASS
  WriteInterface;                     // INTERFACEHDR MMS INTERFACEEND
  WriteAccess;                        // WRITEACCESS
  WriteInt(BiffIdCodepage, 1200, 2);  // CODEPAGE
  WriteInt(BiffIdDsf, 0, 2);          // DSF
  WriteRRD;                           // RRD
  WriteProt;                          // WINPROTECT PROTECT PASSWORD PROTREV PROTREVPASS
  Win.Flush(Stream);                  // WINDOW1
  WriteInt(BiffIdBackup, 0, 2);       // BACKUP
  WriteInt(BiffIdHideObj, 0, 2);      // HIDEOBJ
  WriteInt(BiffIdDate1904, 0, 2);     // DATE1904
  WriteInt(BiffIdCalcPrec, 1, 2);     // CALCPRECISION
  WriteInt(BiffIdRefreshAll, 0, 2);   // REFRESHALL
  WriteInt(BiffIdBookBool, 0, 2);     // BOOKBOOL
  WriteFonts;                         // 1*510 FONT
  WriteFormats;                       // 8*218 FORMAT
  WriteXFs;                           // 16* XF
  WriteStyles;                        // 1* STYLE
  WritePalette;                       // PALETTE
  WriteInt(BiffIdUserElf, 0, 2);      // USERELF
  WriteSheets;                        // SHEET
  WriteInt(BiffIdCountry, $70007, 4); // COUNTRY
  WriteEscher;                        // *ESCHER
  WriteSST;                           // SST
  WriteExtSST;                        // EXTSST
  WriteLinkTable;                     // EXTRENALBOOK, EXTERNALSHEET, etc.
  Stream.AddEOF;                      // EOF

  { Write sheets }

  for i := 0 to Sheets.Count - 1 do
    with TBiffSheet(Sheets[i]) do
    begin
      Stream[RecIndex].WriteBytes(0, Stream.Size, 4);
      Flush(Stream);
    end;

  { Encrypt the workbook if it needs }

  if FCipher <> nil then
    FCipher.EncryptStream(Stream);
end;

procedure TBiffWorkbook.AddDefPalette(ANotFind: Boolean);
begin
  AddColor($000080, ANotFind);
  AddColor($008000, ANotFind);
  AddColor($800000, ANotFind);
  AddColor($008080, ANotFind);
  AddColor($800080, ANotFind);
  AddColor($808000, ANotFind);
  AddColor($C0C0C0, ANotFind);
  AddColor($808080, ANotFind);
  AddColor($FF9999, ANotFind);
  AddColor($663399, ANotFind);
  AddColor($CCFFFF, ANotFind);
  AddColor($FFFFCC, ANotFind);
  AddColor($660066, ANotFind);
  AddColor($8080FF, ANotFind);
  AddColor($CC6600, ANotFind);
  AddColor($FFCCCC, ANotFind);
  AddColor($800000, ANotFind);
  AddColor($FF00FF, ANotFind);
  AddColor($00FFFF, ANotFind);
  AddColor($FFFF00, ANotFind);
  AddColor($800080, ANotFind);
  AddColor($000080, ANotFind);
  AddColor($808000, ANotFind);
  AddColor($FF0000, ANotFind);
  AddColor($FFCC00, ANotFind);
  AddColor($FFFFCC, ANotFind);
  AddColor($CCFFCC, ANotFind);
  AddColor($99FFFF, ANotFind);
  AddColor($FFCC99, ANotFind);
  AddColor($CC99FF, ANotFind);
  AddColor($FF99CC, ANotFind);
  AddColor($99CCFF, ANotFind);
  AddColor($FF6633, ANotFind);
  AddColor($CCCC33, ANotFind);
  AddColor($00CC99, ANotFind);
  AddColor($00CCFF, ANotFind);
  AddColor($0099FF, ANotFind);
  AddColor($0066FF, ANotFind);
  AddColor($996666, ANotFind);
  AddColor($969696, ANotFind);
  AddColor($003300, ANotFind);
  AddColor($669933, ANotFind);
  AddColor($003300, ANotFind);
  AddColor($003333, ANotFind);
  AddColor($003399, ANotFind);
  AddColor($663399, ANotFind);
  AddColor($993333, ANotFind);
  AddColor($333333, ANotFind);
end;

{ TBiffSheet }

constructor TBiffSheet.Create(Owner: TBiffWorkbook);
begin
  FOwner := Owner;

  FRows  := TObjList.Create;
  FCols  := TObjList.Create;

  PageSetup := TBiffPageSetup.Create;
  View      := TBiffWindow2.Create;

  Bounds.FR := -1;
  Bounds.LR := -1;
  Bounds.FC := -1;
  Bounds.LC := -1;

  FStrCount := 0;
  Name      := 'Sheet';
  FKind     := skWorksheet;
  FVis      := svVisible;
  FPB       := TList.Create;
end;

destructor TBiffSheet.Destroy;
begin
  FRows.Free;
  FCols.Free;
  PageSetup.Free;
  View.Free;
  FPB.Free;
end;

procedure TBiffSheet.SetColWidth(i: LongInt; w: LongInt);
begin
  if (i >= 0) and (i <= BiffMaxCol) then
    AddColInfo(TBiffColInfo.Create(i, 15, w));
end;

procedure TBiffSheet.SetRowHeight(i: LongInt; h: LongInt);
var
  r: TBiffRow;
  c: TBiffCell;
begin
  if (i < 0) or (i > BiffMaxRow) then
    Exit;

  r := GetRow(i);

  if r = nil then
  begin
    c := TBiffCell.Create;
    with c do
    begin
      Row   := i;
      Col   := 0;
    end;

    AddCell(c);
    r := GetRow(i);
  end;

  if r <> nil then
    r.Height := h;
end;

function TBiffSheet.GetColWidth(i: LongInt): LongInt;
var
  j: LongInt;
begin
  for j := FCols.Count - 1 downto 0 do
    with TBiffColInfo(FCols[j]) do
      if (First <= i) and (i <= Last) then
      begin
        Result := Width;
        Exit;
      end;

  Result := 0;
end;

function TBiffSheet.GetRowHeight(i: LongInt): LongInt;
var
  r: TBiffRow;
begin
  r := GetRow(i);
  if r = nil then
    Result := 0
  else
    Result := r.Height;
end;

function TBiffSheet.LastRowIndex: LongInt;
begin
  Result := Bounds.LR;
end;

procedure TBiffSheet.MergeCells(Rect: TRect);
begin
  Assert((Rect.Left <= Rect.Right) and (Rect.Top <= Rect.Bottom));

  { There's no need to merge a 1x1 rectangle of cells }

  if (Rect.Left = Rect.Right) and (Rect.Top = Rect.Bottom) then
    Exit;

  { todo: Rect should be checked for intersections with all existing
    merged blocks, but it'd be slow or it'd require a complex algorithm }

  SetLength(FMrgCells, Length(FMrgCells) + 1);
  FMrgCells[Length(FMrgCells) - 1] := Rect;
end;

procedure TBiffSheet.AddPageBreak(Row: LongInt);
begin
  FPB.Add(Pointer(Row));
end;

function TBiffSheet.AddDrawing: TEscherShape;
begin
  if FDGroup = nil then
    FDGroup := FOwner.Escher.AddGroup;

  Result := FDGRoup.Add;
end;

procedure TBiffSheet.AddColInfo(Info: TBiffColInfo);
begin
  FCols.Add(Info);
end;

function TBiffSheet.GetRow(Index: LongInt): TBiffRow;
var
  i, r: Longint;
begin
  { In most cases, only one iteration of this loop is executed,
    because this function is optimised for sequential access. }

  for i := 0 to FRows.Count - 1 do
  begin
    r := (FLastReadRow + i) mod FRows.Count;

    if TBiffRow(FRows[r]).Row = Index then
    begin
      Result := TBiffRow(FRows[r]);
      FLastReadRow := r + 1;
      Exit;
    end;
  end;

  Result := nil;
end;

//
// There is a list of rows, where each row contains
// a list of cells. The rows in the list are ordered
// by the row index by ascending. This function
// looks for a place in the list of rows and adds to there
// a cell with the specified coordinates.
//

procedure TBiffSheet.AddCell(Cell: TBiffCell);

  function CreateRow(Cell: TBiffCell): TBiffRow;
  var
    R: TBiffRow;
  begin
    R := TBiffRow.Create;
    R.Row := Cell.Row;
    R.Cells.Add(Cell);

    R.FirstCol  := Cell.Col;
    R.LastCol   := Cell.Col;

    Result := R;
  end;

  procedure AttachToRow(R: TBiffRow; Cell: TBiffCell);
  begin
    R.Cells.Add(Cell);

    if Cell.Col < R.FirstCol then
      R.FirstCol := Cell.Col;

    if Cell.Col > R.LastCol then
      R.LastCol := Cell.Col;
  end;

  procedure InsertRow(Cell: TBiffCell);
  var
    i: Integer;
  begin
    i := FRows.Count - 1;

    while (i >= 0) and (TBiffRow(FRows[i]).Row > Cell.Row) do
      Dec(i);

    if (i >= 0) and (TBiffRow(FRows[i]).Row = Cell.Row) then
      AttachToRow(TBiffRow(FRows[i]), Cell)
    else
      FRows.Insert(i + 1, CreateRow(Cell));
  end;

  procedure AdjustBounds(r, c: LongInt; var b: TBiffBounds);
  begin
    with b do
    begin
      if (r < FR) or (FR < 0) then
        FR := r;

      if (r > LR) or (LR < 0) then
        LR := r;

      if (c < FC) or (FC < 0) then
        FC := c;

      if (c > LC) or (LC < 0) then
        LC := c;
    end;
  end;

begin
  InsertRow(Cell);
  AdjustBounds(Cell.Row, Cell.Col, Bounds);

  if Cell is TBiffTextCell then
    Inc(FStrCount);
end;

procedure TBiffSheet.Flush(Stream: TBiffStream);

  procedure WriteInt(RecId: TBiffRecId; Value, Size: LongInt);
  begin
    Stream.Add(RecId).Write(Value, Size);
  end;

  procedure WriteF8(RecId: TBiffRecId; Value: Double);
  begin
    Stream.Add(RecId).Write(Value, 8);
  end;

  procedure WriteDIMENSION;
  var
    Dim: TBiffrDimension;
  begin
    FillChar(Dim, SizeOf(Dim), 0);

    with Bounds do
    begin
      { If FR or FC values are negative, then
        this sheet contains no cells }

      if FR < 0 then
        FR := 0;

      if FC < 0 then
        FC := 0;

      Dim.FirstRow  := Bounds.FR;
      Dim.LastRow   := Bounds.LR + 1;
      Dim.FirstCol  := Bounds.FC;
      Dim.LastCol   := Bounds.LC + 1;
    end;

    Stream.Add(BiffIdDim).Write(Dim, Sizeof(Dim));
  end;

  //
  // todo
  //

  procedure WriteCells(R: TBiffRow);
  begin
    TBiffObject.FlushList(R.Cells, Stream);
  end;

  procedure WriteCOLINFO;
  begin
    TBiffObject.FlushList(FCols, Stream);
  end;

  //
  // BIFF8 requires that a MERGEDCELLS record
  // is not continued by a CONTINUE record, so
  // merged blocks will be grouped by 1024 items
  // and written in different MERGEDCELLS records.
  //

  procedure WriteMergedCells;

    procedure WriteBlock(First, Last: Integer);
    var
      m: TBiffrCellsBlock;
      i: Integer;
    begin
      with Stream.Add(BiffIdMergedCells) do
      begin
        WriteConst(Last - First + 1, 2);

        for i := First to Last do
        begin
          with m, FMrgCells[i] do
          begin
            FR := Top;
            LR := Bottom;
            FC := Left;
            LC := Right;
          end;

          Write(m, SizeOf(m));
        end;
      end;
    end;

  var
    i, n: Integer;
  begin
    n := Min(BiffMaxMrgCellsNum, (BiffMaxRecLen - 2) div 8);
    i := 0;

    while i < Length(FMrgCells) do
    begin
      WriteBlock(i, Min(i + n - 1, High(FMrgCells)));
      Inc(i, n);
    end;
  end;

  procedure WriteMargins;

    procedure WM(Id: LongInt; Value: Double);
    begin
      Stream.Add(Id).Write(Value, 8);
    end;

  begin
    with Margin do
    begin
      WM(BiffIdLeftMargin,   Left);
      WM(BiffIdTopMargin,    Top);
      WM(BiffIdRightMargin,  Right);
      WM(BiffIdBottomMargin, Bottom);
    end;
  end;

  procedure WriteDrawings;
  var
    es:   TEscherStream;
    ms:   TMemoryStream;

  begin
    if FDGroup = nil then
      Exit;

    es := TEscherStream.Create;
    ms := TMemoryStream.Create;

    FDGroup.Flush(es);
    es.Flush(ms);

    Stream.Add(BiffIdDrawing).Write(ms.Memory^, ms.Size);

    ms.Free;
    es.Free;
  end;

  procedure WriteGOBJ;
  var
    i: Integer;
  begin
    if FDGroup = nil then
      Exit;

    // [MS-XLS] 2.4.181

    for i := 0 to FDGroup.Count - 1 do
      with Stream.Add(BiffIdGObj) do
      begin
        // [MS-XLS] 2.5.143

        WriteConst($15, 2); // record id
        WriteConst($12, 2); // record len
        WriteConst($08, 2);
        WriteZeros(16);

        // [MS-XLS] 2.5.142

        WriteConst($07, 2); // record id
        WriteConst($02, 2); // record len
        WriteConst($ffff, 2);

        // [MS-XLS} 2.5.151

        WriteConst($08, 2); // record id
        WriteConst($02, 2); // record len
        WriteConst($00, 2);
      end;
  end;

  procedure WritePageBreak;
  var
    i: LongInt;
  begin
    if FPB.Count = 0 then Exit;
    with Stream.Add(BiffIdHorPageBreak) do
    begin
      Write(FPB.Count, 2);

      for i := 0 to FPB.Count - 1 do
      begin
        WriteConst(LongInt(FPB[i]), 2);
        WriteConst(0, 2);
        WriteConst(BiffMaxCol, 2);
      end;
    end;
  end;

  procedure WriteIndex;
  begin
    { todo: INDEX not emitted yet }
  end;

  procedure WriteGuts;
  begin
    with Stream.Add(BiffIdGuts) do
    begin
      WriteConst(0, 4);
      WriteConst(0, 4);
    end;
  end;

  { Actual format of DEFROWHEIGHT see
    in [MS-XLS] section 2.4.87 }

  procedure WriteDefRowHeight;
  begin
    with Stream.Add(BiffIdDefRowHeight) do
    begin
      WriteConst(0, 2);
      WriteConst(300, 2);
    end;
  end;

  procedure WriteWsBool;
  begin
    WriteInt(
      BiffIdWsBool,

      //BiffWsbFitPage or
      BiffWsbShowBreaks or
      BiffWsbRowSums or
      BiffWsbColSums,

      2);
  end;

var
  i, j, fi, li, mi: LongInt;
  r, fr: TBiffRow;

begin
  Stream.AddBOF(bkSheet);             // BOF
  WriteIndex;                         // INDEX
  WriteInt(BiffIdCalcMode, 1, 2);     // CALCMODE
  WriteInt(BiffIdCalcCount, 100, 2);  // CALCCOUNT
  WriteInt(BiffIdCalcRefMode, 1, 2);  // CALCREFMODE
  WriteInt(BiffIdCalcIter, 0, 2);     // CALCITER
  WriteF8(BiffIdCalcDelta, 0.001);    // CALCDELTA
  WriteInt(BiffIdSaveRecalc, 1, 2);   // SAVERECALC
  WriteInt(BiffIdPrintRowCol, 0, 2);  // PRINTROWCOL
  WriteInt(BiffIdPrintGrid, 0, 2);    // PRINTGRID
  WriteInt(BiffIdGridSet, 1, 2);      // GRIDSET
  WriteGuts;                          // GUTS
  WriteDefRowHeight;                  // DEFROWHEIGHT
  WriteWsBool;                        // WSBOOL
  WritePageBreak;                     // HPAGEBREAK
  Stream.Add(BiffIdHeader);           // HEADER
  Stream.Add(BiffIdFooter);           // FOOTER
  WriteInt(BiffIdHCenter, 0, 2);      // HCENTER
  WriteInt(BiffIdVCenter, 0, 2);      // VCENTER
  WriteMargins;                       // LEFTMARGIN TOPMARGIN RIGHTMARGIN BOTTOMMARGIN
  PageSetup.Flush(Stream);            // PAGESETUP
  WriteInt(BiffIdDefColWidth, 8, 2);  // DEFCOLWIDTH
  WriteCOLINFO;                       // COLINFO
  WriteDIMENSION;                     // DIMENSION

  fi := 0;

  if FRows.Count > 0 then
  repeat
    //
    // MS Excel allows up to 32 rows written
    // consequently. Each block of 32 rows must be ended
    // with a DBCELL record.
    //

    fr  := TBiffRow(FRows[fi]);
    mi  := fr.Row + BiffRowBlockSz - 1;

    //
    // Write ROW records
    //

    i := fi;

    repeat
      r := TBiffRow(FRows[i]);
      if r.Row > mi then
        Break;

      r.Offset := Stream.Size;
      r.Flush(Stream);
      Inc(i);
    until i = FRows.Count;

    li := i - 1;

    //
    // Write cells
    //

    for i := fi to li do
    begin
      r := TBiffRow(FRows[i]);
      if r.Cells.Count = 0 then
        Continue;

      r.FirstCell := Stream.Size;
      TBiffObject(r.Cells[0]).Flush(Stream);

      for j := 1 to r.Cells.Count - 1 do
        TBiffObject(r.Cells[j]).Flush(Stream);
    end;

    //
    // Write DBCELL
    //

    with Stream.Add(BiffIdDBCell) do
    begin
      WriteConst(Offset - fr.Offset, 4);
      for i := fi to li do
      begin
        r := TBiffRow(FRows[i]);
        WriteConst(r.FirstCell - r.Offset - 4 - SizeOf(TBiffrRow), 2);
      end;
    end;

    fi := li + 1;
  until fi = FRows.Count;

  WriteDrawings;                  // DRAWING
  WriteGOBJ;                      // GOBJ
  View.Flush(Stream);             // WINDOW2
  WriteMergedCells;               // MERGEDCELLS

  Stream.AddEOF;                  // EOF
end;

{ TBiffUCS }

constructor TBiffUCS.Create;
begin
  Init;
end;

constructor TBiffUCS.Create(const S: WideString; UCS16: Boolean);
begin
  Init;
  Data := S;
  Len16 := UCS16;
end;

destructor TBiffUCS.Destroy;
begin
  FRuns.Free;
end;

procedure TBiffUCS.Init;
begin
  FRuns := TMemoryStream.Create;
  FCompress := True;
end;

procedure TBiffUCS.AddFormat(Position, Font: LongInt);
begin
  FRuns.Write(Position, 2);
  FRuns.Write(Font, 2);
end;

function TBiffUCS.IsCompressible: Boolean;
var
  i: Integer;
begin
  if not FCompress then
  begin
    Result := False;
    Exit;
  end;

  for i := 1 to Length(Data) do
    if Word(Ord(Data[i])) > 255 then
    begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

procedure TBiffUCS.Flush(Stream: TBiffStream);

  procedure WriteStr(s: PWideChar; len: LongWord; flags: Byte);
  var
    n:      LongWord;
    buffer: array[0..BiffMaxRecLen - 2] of AnsiChar;

    //
    // Prepares up to "count" bytes from the string
    // to be written to a record
    //

    procedure Prepare(count: LongWord);
    label
      copychar;
    var
      x, i: LongWord;
    begin
      if (flags and 1) = 1 then
      begin
        x := count shr 1;
        if x > len then
          x := len;

        n := x*2;
        Move(s^, buffer[0], n);

        s := PWideChar(LongWord(s) + n);
        Dec(len, x);
        Exit;
      end;

      n := count;

      if n > len then
        n := len;

      for i := 0 to n - 1 do
        buffer[i] := PAnsiChar(LongWord(s) + 2*i)^;

      Dec(len, n);
      s := PWideChar(LongWord(s) + 2*n);
    end;

  begin
    n := BiffMaxRecLen - Stream[Stream.Count - 1].Size;

    if n > 0 then
    begin
      Prepare(n);
      Stream[Stream.Count - 1].Write(buffer[0], n);
    end;

    while len > 0 do
    begin
      Prepare(BiffMaxRecLen - 1);
      with Stream.Add(BiffIdContinue) do
      begin
        Write(flags, 1);
        Write(buffer[0], n);
      end;
    end;
  end;

var
  hSize:  LongWord;     // header size with the first character
  f:      Byte;         // flags
  r:      TBiffRecord;  // first available record
  compr:  Boolean;      // set if the string is compressible
  frn:    LongWord;     // count of formatting runs

begin
  compr := IsCompressible;
  frn   := FRuns.Size shr 2;

  //
  // Calculate the size of the header.
  //

  hSize := 2;

  if Len16 then
    Inc(hSize);

  if frn > 0 then
    Inc(hSize, 2);

  if Data <> '' then
    if compr then
      Inc(hSize, 1)
    else
      Inc(hSize, 2);

  f := 0;

  if not compr then
    f := 1; // compression flag

  if frn > 0 then
    f := f or 8; // formatting runs flag

  //
  // Find the first available record
  // that capable to keep the header
  // and write the header.
  //

  r := Stream[Stream.Count - 1];
  if (r = nil) or (r.Size + hSize > BiffMaxRecLen) then
    r := Stream.Add(BiffIdContinue);

  if Len16 then
    r.WriteConst(Length(Data), 2)
  else
    r.WriteConst(Length(Data), 1);

  r.Write(f, 1);
  if frn > 0 then
    r.Write(frn, 2);

  if Data <> '' then
  begin
    if compr then
      r.Write(Data[1], 1)
    else
      r.Write(Data[1], 2);

    //
    // Write characters
    // and formatting runs.
    //

    if Length(Data) > 1 then
      WriteStr(@Data[2], Length(Data) - 1, f and 1);

    if frn > 0 then
      Stream[Stream.Count - 1].Write(FRuns.Memory^, FRuns.Size);
  end;
end;

procedure TBiffUCS.SetData(const Value: WideString);
begin
  FData := Value;
  FHash := 0;
end;

function TBiffUCS.Equals(s: TBiffObject): Boolean;
begin
  if s.GetHashCode = GetHashCode then
    Result := TBiffUCS(s).Data = Data
  else
    Result := False;
end;

function TBiffUCS.GetHashCode: LongInt;
begin
  if FHash <> 0 then
  begin
    Result := FHash;
    Exit;
  end;

  if FData <> '' then
    TCryptoHash.Hash('Jenkins', FHash, SizeOf(FHash),
      FData[1], Length(FData) * SizeOf(FData[1]))
  else
    FHash := 0;

  Result := FHash;
end;

{ TBiffRecord }

constructor TBiffRecord.Create(Owner: TBiffStream; Offset: Cardinal);
begin
  FOwner := Owner;
  FOffset := Offset;
end;

function TBiffRecord.GetRecId: TBiffRecId;
begin
  Result := TBiffRecId(FOwner.ReadBytes(Offset, 2))
end;

function TBiffRecord.GetSize: Cardinal;
begin
  Result := FOwner.ReadBytes(Offset + 2, 2)
end;

procedure TBiffRecord.SetSize(n: Cardinal);
begin
  Assert(n <= BiffMaxRecLen);
  FOwner.WriteBytes(Offset + 2, n, 2);
end;

procedure TBiffRecord.Write(const Buffer; Count: Cardinal);
begin
  FOwner.AppendRecord(Self, Buffer, Count)
end;

procedure TBiffRecord.WriteConst(Value, Size: Cardinal);
begin
  Assert((Size > 0) and (Size <= SizeOf(Value)));
  Write(Value, Size);
end;

procedure TBiffRecord.WriteStream(Stream: TStream);
var
  Buffer: array[0..63] of Byte;
  n: Integer;
begin
  Stream.Position := 0;

  repeat
    n := Stream.Read(Buffer[0], SizeOf(Buffer));

    if n > 0 then
      Write(Buffer[0], n);
  until n = 0;
end;

procedure TBiffRecord.WriteZeros(Size: Cardinal);
begin
  Assert(Size > 0);

  while Size > SizeOf(Integer) do
  begin
    WriteConst(0, SizeOf(Integer));
    Dec(Size, SizeOf(Integer));
  end;

  if Size > 0 then
    WriteConst(0, Size);
end;

procedure TBiffRecord.WriteBytes(Offset, Data, Count: Cardinal);
begin
  Assert(Count in [1..SizeOf(Data)]);
  Assert(Offset + Count <= Size);

  FOwner.FStream.Position := FOffset + 4 + Offset;
  FOwner.FStream.WriteBuffer(Data, Count);
end;

procedure TBiffRecord.LoadFromStream(Stream: TStream);
begin
  if Size > 0 then
  begin
    FOwner.FStream.Position := Offset + 4;
    FOwner.FStream.CopyFrom(Stream, Size);
  end;
end;

procedure TBiffRecord.SaveToStream(Stream: TStream);
begin
  if Size > 0 then
  begin
    FOwner.FStream.Position := Offset + 4;
    Stream.CopyFrom(FOwner.FStream, Size);
  end;
end;

procedure SerialiseBiffRecord(Stream: TStream; Rec: TObject);
begin
  with Rec as TBiffRecord do
  begin
    Stream.WriteBuffer(FOwner, SizeOf(FOwner));
    Stream.WriteBuffer(FOffset, SizeOf(FOffset));
  end;
end;

function DeserialiseBiffRecord(Stream: TStream): TObject;
var
  FOwner: TBiffStream;
  FOffset: Cardinal;
begin
  with Stream do
  begin
    ReadBuffer(FOwner, SizeOf(FOwner));
    ReadBuffer(FOffset, SizeOf(FOffset));
  end;

  Result := TBiffRecord.Create(FOwner, FOffset);
end;

{ TBiffStream }

constructor TBiffStream.Create(Cached: Boolean);
begin
  if Cached then
  begin
    FStream := TFileStream.Create(GetTempFile, fmCreate);
    FRecords := TListCache.Create(GetTempFile, GetTempFile);
    FRecords.WriteObj := SerialiseBiffRecord;
    FRecords.ReadObj := DeserialiseBiffRecord;
  end
  else
  begin
    FStream := TMemoryStream.Create;
    FRecords := TListCache.Create;
  end;
end;

destructor TBiffStream.Destroy;
begin
  FStream.Free;
  FRecords.Free;
end;

function TBiffStream.GetRecCount: Integer;
begin
  Result := FRecords.Count
end;

function TBiffStream.GetRecord(Index: LongInt): TBiffRecord;
begin
  Result := TBiffRecord(FRecords[Index])
end;

function TBiffStream.GetSize: Cardinal;
begin
  Result := FStream.Size
end;

procedure TBiffStream.SaveToStream(Stream: TStream);
begin
  Stream.CopyFrom(FStream, 0)
end;

function TBiffStream.ReadBytes(Offset, Count: Cardinal): Cardinal;
begin
  Assert((Count in [1..SizeOf(Result)]) and (Offset + Count <= Size));
  Result := 0;
  FStream.Position := Offset;
  FStream.ReadBuffer(Result, Count);
end;

procedure TBiffStream.WriteBytes(Offset, Data, Count: Cardinal);
begin
  Assert((Count in [1..SizeOf(Data)]) and (Offset + Count <= Size));
  FStream.Position := Offset;
  FStream.WriteBuffer(Data, Count);
end;

function TBiffStream.Add(Id: TBiffRecId): TBiffRecord;
begin
  Result := TBiffRecord.Create(Self, Size);
  FRecords.Add(Result);

  if Id <> BiffIdContinue then
    FLastRec := Result;

  Append(Id, 2);  // record id
  Append(0, 2);   // record size (will be updated)
end;

function TBiffStream.AddBOF(K: TBiffStreamKind): TBiffRecord;
var
  r: TBiffrBOF;
begin
  FillChar(r, SizeOf(r), 0);

  r.Version   := $600;
  r.Year      := 1997;
  r.Kind      := K;
  r.Build     := 8211;
  r.Flags1    := BiffBoffWin or (3 shl 14);
  r.Flags1    := r.Flags1 or BiffBoffWinAny;
  r.Flags2    := 6 or (3 shl 8);

  Result := Add(BiffIdBOF);
  Result.Write(r, SizeOf(r));
end;

function TBiffStream.AddEOF: TBiffRecord;
begin
  Result := Add(BiffIdEOF)
end;

procedure TBiffStream.AppendData(const Data; Count: Cardinal);
begin
  Assert(@Data <> nil);
  Assert(Count > 0);
  FStream.Seek(0, soFromEnd);
  FStream.WriteBuffer(Data, Count);
end;

procedure TBiffStream.Append(Data, Count: Cardinal);
begin
  Assert(Count in [1..SizeOf(Data)]);
  AppendData(Data, Count)
end;

procedure TBiffStream.AppendRecord(Rec: TBiffRecord; const Data; DataSize: Cardinal);

  procedure MoveData(var Src: Pointer; var Size: Cardinal; Count: Cardinal);
  begin
    Assert(Count <= Size);

    if Count > 0 then
    begin
      AppendData(Src^, Count);
      Dec(Size, Count);
      Src := Pointer(Cardinal(Src) + Count);
    end;
  end;

var
  n: Cardinal;
  DataPtr: Pointer;
begin
  Assert(Count > 0);
  Assert(@Data <> nil);
  Assert((Rec = FLastRec) or (Rec = Records[Count - 1]), 'Only the last record can be modified');

  DataPtr := @Data;
  n := Min(DataSize, BiffMaxRecLen - Records[Count - 1].Size);
  MoveData(DataPtr, DataSize, n);

  with Records[Count - 1] do
    Size := Size + n;

  { The data chunk is too big: CONTINUE records are needed.
    [MS-XLS] 2.4.58 }

  while DataSize > 0 do
  begin
    FRecords.Add(TBiffRecord.Create(Self, Size));
    n := Min(BiffMaxRecLen, DataSize);

    Append(BiffIdContinue, 2);
    Append(n, 2);
    MoveData(DataPtr, DataSize, n);
  end;
end;

{ TBiffLinkTable }

function TBiffLinkTable.GetInternalSheetRef(const SheetName: string): Integer;
var
  i: Integer;
begin
  i := FindSheet(SheetName);

  if i = RefsCount then
  begin
    RefsCount := RefsCount + 1;
    FIntSheetNames[RefsCount - 1] := SheetName;
  end;

  Result := i;
  Assert(FIntSheetNames[i] = SheetName);
end;

function TBiffLinkTable.GetRefsCount: Integer;
begin
  Result := Length(FIntSheetNames)
end;

procedure TBiffLinkTable.SetRefsCount(n: Integer);
begin
  SetLength(FIntSheetNames, n);
  SetLength(FIntSheetIndex, n);
end;

procedure TBiffLinkTable.SetSheetIndex(const SheetName: string; Index: Integer);
var
  i: Integer;
begin
  i := FindSheet(SheetName);

  if i < RefsCount then
    FIntSheetIndex[i] := Index;
end;

function TBiffLinkTable.FindSheet(const Name: string): Integer;
begin
  for Result := 0 to RefsCount - 1 do
    if FIntSheetNames[Result] = Name then
      Exit;

  Result := RefsCount;
end;

procedure TBiffLinkTable.Flush(Stream: TBiffStream);
var
  i: Integer;
begin
  with Stream.Add(BiffIdExtBook) do
  begin
    WriteConst(SheetsCount, 2);
    WriteConst(1, 1);
    WriteConst(4, 1);
  end;

  with Stream.Add(BiffIdExtSheet) do
  begin
    WriteConst(RefsCount, 2);

    for i := 0 to RefsCount - 1 do
    begin
      WriteConst(0, 2); // index to EXTBOOK
      WriteConst(FIntSheetIndex[i], 2); // index to first sheet
      WriteConst(FIntSheetIndex[i], 2); // index to last sheet
    end;
  end;
end;

{ TBiffFormulaCell }

constructor TBiffFormulaCell.Create(Instructions: TStream);
begin
  FInst := Instructions;
end;

destructor TBiffFormulaCell.Destroy;
begin
  FInst.Free;
  inherited;
end;

procedure TBiffFormulaCell.Flush(Stream: TBiffStream);
var
  f: TBiffrFormula;
begin
  FillChar(f, SizeOf(f), 0);

  with f do
  begin
    Cell.Row    := Row;
    Cell.Column := Col;
    Cell.XF     := XF;

    Value       := 0;
    InstLen     := FInst.Size;
  end;

  with Stream.Add(BiffIdFormula) do
  begin
    Write(f, Sizeof(f));
    WriteStream(FInst);
  end;
end;

{ TBiffFormulaCodeEmitter }

procedure TBiffFormulaCodeEmitter.Error(const ErrorMsg: string);
begin
  raise EBiffFormulaCodeEmitterError.Create(ErrorMsg)
end;

procedure TBiffFormulaCodeEmitter.Ensure(b: Boolean; const ErrorMsg: string);
begin
  if not b then
    Error(ErrorMsg)
end;

procedure TBiffFormulaCodeEmitter.EnsureCellRange(const Cell: TBiffFormulaCellRef);
begin
  Ensure((Cell.Row >= 0) and (Cell.Row < $10000), 'Row index is out of bounds');
  Ensure((Cell.Col >= 0) and (Cell.Col < $100), 'Column index is out of bounds');
end;

procedure TBiffFormulaCodeEmitter.Write(Inst, Len: Cardinal);
begin
  Assert((Len > 0) and (Len <= SizeOf(Inst)));
  Assert(FInst <> nil);
  FInst.WriteBuffer(Inst, Len);
end;

procedure TBiffFormulaCodeEmitter.WriteAreaRef(const Cell1, Cell2: TBiffFormulaCellRef);
begin
  EnsureCellRange(Cell1);
  EnsureCellRange(Cell2);

  Write(Cell1.Row, 2);
  Write(Cell2.Row, 2);
  Write(Cell1.Col, 1);
  Write(RelFlags(not Cell1.AbsRow, not Cell1.AbsCol));
  Write(Cell2.Col, 1);
  Write(RelFlags(not Cell2.AbsRow, not Cell2.AbsCol));
end;

procedure TBiffFormulaCodeEmitter.WriteCellRef(const Cell: TBiffFormulaCellRef);
begin
  EnsureCellRange(Cell);

  Write(Cell.Row, 2);
  Write(Cell.Col, 1);
  Write(RelFlags(not Cell.AbsRow, not Cell.AbsCol));
end;

function TBiffFormulaCodeEmitter.RelFlags(RelRow, RelCol: Boolean): Byte;
begin
  Result := 0;

  if RelCol then
    Result := Result or $40;

  if RelRow then
    Result := Result or $80;
end;

procedure TBiffFormulaCodeEmitter.WriteOpCode(Op: Byte);
begin
  Assert(Op and $e0 = 0);

  case RetMode of
    frtRef:   Op := Op or $20;
    frtVal:   Op := Op or $40;
    frtArray: Op := Op or $60;

    else Error('Invalid RetType mode');
  end;

  Write(Op, 1)
end;

procedure TBiffFormulaCodeEmitter.Range;
begin
  Write($11)
end;

procedure TBiffFormulaCodeEmitter.Intersect;
begin
  Write($0f)
end;

procedure TBiffFormulaCodeEmitter.Join;
begin
  Write($08)
end;

procedure TBiffFormulaCodeEmitter.CallId;
begin
  Write($15)
end;

procedure TBiffFormulaCodeEmitter.CmpE;
begin
  Write($0b)
end;

procedure TBiffFormulaCodeEmitter.CmpG;
begin
  Write($0d)
end;

procedure TBiffFormulaCodeEmitter.CmpGE;
begin
  Write($0c)
end;

procedure TBiffFormulaCodeEmitter.CmpL;
begin
  Write($09)
end;

procedure TBiffFormulaCodeEmitter.CmpLE;
begin
  Write($0a)
end;

procedure TBiffFormulaCodeEmitter.CmpNE;
begin
  Write($0e)
end;

procedure TBiffFormulaCodeEmitter.Div100;
begin
  Write($14)
end;

procedure TBiffFormulaCodeEmitter.Divide;
begin
  Write($06)
end;

procedure TBiffFormulaCodeEmitter.Mul;
begin
  Write($05)
end;

procedure TBiffFormulaCodeEmitter.Neg;
begin
  Write($13)
end;

procedure TBiffFormulaCodeEmitter.Add;
begin
  Write($03)
end;

procedure TBiffFormulaCodeEmitter.Sub;
begin
  Write($04)
end;

procedure TBiffFormulaCodeEmitter.Pow;
begin
  Write($07);
end;

procedure TBiffFormulaCodeEmitter.Push(Value: Integer);
begin
  Ensure((Value >= 0) and (Value < $10000));
  Write($1e);
  Write(Value, 2);
end;

procedure TBiffFormulaCodeEmitter.Push(Value: Double);
begin
  Write($1f);
  FInst.WriteBuffer(Value, 8);
end;

procedure TBiffFormulaCodeEmitter.PushNull;
begin
  Write($16);
end;

procedure TBiffFormulaCodeEmitter.Push(const s: string);
begin
  Ensure(Length(s) < $100, 'String is too long');

  Write($17);
  Write(Length(s));

  if SizeOf(s[1]) = 1 then
    Write(0)
  else
    Write(1);

  if s <> '' then
    FInst.WriteBuffer(s[1], Length(s) * SizeOf(s[1]));
end;

procedure TBiffFormulaCodeEmitter.Push(b: Boolean);
begin
  Write($1d);

  if b then
    Write(1)
  else
    Write(0);
end;

procedure TBiffFormulaCodeEmitter.Call(Func: Cardinal);
begin
  Ensure(Func < $10000);

  WriteOpCode($01);
  Write(Func, 2);
end;

procedure TBiffFormulaCodeEmitter.Call(Func, NumArgs: Cardinal);
begin
  Ensure(Func < $8000);
  Ensure(NumArgs < $80);

  WriteOpCode($02);
  Write(NumArgs and $7f);
  Write(Func and $7fff, 2);
end;

procedure TBiffFormulaCodeEmitter.PushArea(const Cell1, Cell2: TBiffFormulaCellRef);
begin
  WriteOpCode($05);
  WriteAreaRef(Cell1, Cell2);
end;

procedure TBiffFormulaCodeEmitter.PushCell(const Cell: TBiffFormulaCellRef);
begin
  WriteOpCode($04);
  WriteCellRef(Cell);
end;

procedure TBiffFormulaCodeEmitter.PushExtArea(SheetRefId: Integer; const Cell1, Cell2: TBiffFormulaCellRef);
begin
  Ensure((SheetRefId >= 0) and (SheetRefId < $10000));

  WriteOpCode($1b);
  Write(SheetRefId, 2);
  WriteAreaRef(Cell1, Cell2);
end;

procedure TBiffFormulaCodeEmitter.PushExtCell(SheetRefId: Integer; const Cell: TBiffFormulaCellRef);
begin
  Ensure((SheetRefId >= 0) and (SheetRefId < $10000));

  WriteOpCode($1a);
  Write(SheetRefId, 2);
  WriteCellRef(Cell);
end;

{ TBiffFormulaLexer }

procedure TBiffFormulaLexer.Add(const Lex: TBiffFormulaLexem);
begin
  SetLength(FLexems, Length(FLexems) + 1);
  FLexems[High(FLexems)] := Lex;
end;

procedure TBiffFormulaLexer.Add(LexKind: TBiffFormulaLexemKind; const LexText: string);
var
  Lex: TBiffFormulaLexem;
begin
  Lex.Kind := LexKind;
  Lex.Text := LexText;
  Add(Lex);
end;

function TBiffFormulaLexer.GetLexemsCount: Integer;
begin
  Result := Length(FLexems)
end;

function TBiffFormulaLexer.IsAlpha(c: Char): Boolean;
begin
  Result := AnsiChar(c) in ['a'..'z', 'A'..'Z']
end;

function TBiffFormulaLexer.IsDigit(c: Char): Boolean;
begin
  Result := AnsiChar(c) in ['0'..'9']
end;

function TBiffFormulaLexer.NextChar: Char;
begin
  Assert(FPos <= Length(FText));
  Result := FText[FPos + 1];
end;

function TBiffFormulaLexer.Read(Len: Integer): string;
begin
  Assert(FPos + Len <= Length(FText) + 1);
  Result := Copy(FText, FPos, Len);
  Inc(FPos, Len);
end;

procedure TBiffFormulaLexer.SkipChar;
begin
  Assert(FPos <= Length(FText));
  Inc(FPos);
end;

function TBiffFormulaLexer.SubStr(Pos, Len: Integer): string;
begin
  Assert(Pos + Len <= Length(FText) + 1);
  Result := Copy(FText, Pos, Len);
end;

function TBiffFormulaLexer.GetChar: Char;
begin
  Assert((FPos >= 0) and (FPos <= Length(FText) + 1));

  if FPos = Length(FText) + 1 then
    Result := #0
  else
    Result := FText[FPos];
end;

function TBiffFormulaLexer.GetLexem(i: Integer): TBiffFormulaLexem;
begin
  Assert((i >= 0) and (i < Count));
  Result := FLexems[i];
end;

procedure TBiffFormulaLexer.Analyse(const Formula: string);
begin
  SetLength(FLexems, 0);
  FText := Formula;
  FPos := 1;

  while FPos <= Length(FText) do
    if not AddSpace then
    if not AddNumber then
    if not AddName then
    if not AddString('"') then
    if not AddString('''') then
    if not AddOp then
    if not AddSymbol then
      Inc(FPos);

  Assert(FPos = Length(FText) + 1);
end;

function TBiffFormulaLexer.AddName: Boolean;
var
  i: Integer;
begin
  i := FPos;

  if IsAlpha(GetChar) then
    while IsAlpha(GetChar) do
      SkipChar;

  Result := i < FPos;

  if Result then
    Add(flkName, SubStr(i, FPos - i));
end;

function TBiffFormulaLexer.AddNumber: Boolean;
var
  i: Integer;
begin
  i := FPos;

  if IsDigit(GetChar) then
    while IsDigit(GetChar) do
      SkipChar;

  Result := i < FPos;

  if Result then
    Add(flkInt, SubStr(i, FPos - i));
end;

function TBiffFormulaLexer.AddOp: Boolean;
var
  i: Integer;
begin
  i := FPos;

  case GetChar of
    '+', '-', '*', '/', '%', '^', '&', ':', '!', '=':
      Add(flkOp, Read(1));
    '<':
      if AnsiChar(NextChar) in ['>', '='] then
        Add(flkOp, Read(2))
      else
        Add(flkOp, Read(1));
    '>':
      if NextChar = '=' then
        Add(flkOp, Read(2))
      else
        Add(flkOp, Read(1));
  end;

  Result := i < FPos;
end;

function TBiffFormulaLexer.AddSpace: Boolean;
var
  i: Integer;
begin
  i := FPos;

  while AnsiChar(GetChar) in [#1..#32] do
    SkipChar;

  Result := i < FPos;

  if Result then
    Add(flkSpace, '');
end;

function TBiffFormulaLexer.AddString(Quote: Char): Boolean;

  function Clean(const s: string): string;
  begin
    Result := StringReplace(s, '""', '"', [rfReplaceAll])
  end;

var
  i: Integer;
begin
  i := FPos;

  if GetChar = Quote then
  begin
    SkipChar;

    while GetChar <> #0 do
      if GetChar <> Quote then
        SkipChar
      else if NextChar <> Quote then
        Break
      else
      begin
        SkipChar;
        SkipChar;
      end;

    if GetChar = Quote then
      SkipChar
    else
      FPos := i;
  end;

  Result := i < FPos;

  if Result then
    Add(flkString, Clean(SubStr(i + 1, FPos - i - 2)))
end;

function TBiffFormulaLexer.AddSymbol: Boolean;
begin
  Add(flkSymbol, Read(1));
  Result := True;
end;

{ TBiffFormulaRPNStack }

function TBiffFormulaRPNStack.Top(const a: TBiffFormulaTokenArray): TBiffFormulaToken;
begin
  Assert(Length(a) > 0);
  Result := a[High(a)];
end;

function TBiffFormulaRPNStack.Pop(var a: TBiffFormulaTokenArray): TBiffFormulaToken;
begin
  Result := Top(a);
  SetLength(a, Length(a) - 1);
end;

procedure TBiffFormulaRPNStack.Push(var Res: TBiffFormulaTokenArray; const t: TBiffFormulaToken);
begin
  SetLength(Res, Length(Res) + 1);
  Res[High(Res)] := t;
end;

procedure TBiffFormulaRPNStack.PushFrame(const Func: string);
var
  t: TBiffFormulaToken;
begin
  t.Kind  := ftkOp;
  t.Op    := fokFunc;
  t.Text  := Func;
  t.Flags := Length(FStack) shl 8 or Byte(FNumArgs);

  Push(FFrame, t);
end;

procedure TBiffFormulaRPNStack.PopFrame;
var
  Func: TBiffFormulaToken;
begin
  Unroll;
  Func := Top(FFrame);

  if Func.Text = '' then
    Pop(FFrame) // Func is the identity function
  else
  begin
    Func.Flags := GetFrameArgs;
    Dec(FNumArgs, GetFrameArgs - 1);
    Pop(FFrame);
    Push(FCode, Func);
  end;
end;

procedure TBiffFormulaRPNStack.PushOp(Op: TBiffFormulaOperatorKind; const Text: string; NumArgs: Integer);
var
  t: TBiffFormulaToken;
begin
  t.Kind  := ftkOp;
  t.Op    := Op;
  t.Text  := Text;
  t.Flags := NumArgs;

  PushOp(t);
end;

procedure TBiffFormulaRPNStack.PushOp(Op: TBiffFormulaToken);
begin
  if Joinable(Op.Op) then
    Push(FStack, Op)
  else
  begin
    PopOp;
    PushOp(Op); // recusrion
  end;
end;

procedure TBiffFormulaRPNStack.PopOp;
var
  Op: TBiffFormulaToken;
begin
  Op := Pop(FStack);
  Push(FCode, Op);
  Dec(FNumArgs, Op.Flags - 1);
end;

procedure TBiffFormulaRPNStack.PushArg(Op: TBiffFormulaOperatorKind; const Text: string; Flags: Integer);
var
  t: TBiffFormulaToken;
begin
  t.Kind  := ftkArg;
  t.Op    := Op;
  t.Text  := Text;
  t.Flags := Flags;

  Push(FCode, t);
  Inc(FNumArgs);
end;

procedure TBiffFormulaRPNStack.Ensure(b: Boolean; const Msg: string);
begin
  if not b then
    Error(Msg)
end;

procedure TBiffFormulaRPNStack.Error(const Msg: string);
begin
  raise EBiffFormulaRPNStackError.Create(Msg)
end;

function TBiffFormulaRPNStack.GetFrameArgs: Integer;
begin
  if Length(FFrame) = 0 then
    Result := FNumArgs
  else
    Result := FNumArgs - (Top(FFrame).Flags and $ff);

  Assert(Result >= 0);
end;

function TBiffFormulaRPNStack.GetFrameOps: Integer;
begin
  if Length(FFrame) = 0 then
    Result := Length(FStack)
  else
    Result := Length(FStack) - (Top(FFrame).Flags shr 8);

  Assert(Result >= 0);
end;

function TBiffFormulaRPNStack.OpPriority(Kind: TBiffFormulaOperatorKind): Integer;
begin
  case Kind of
    fokPush:
      Result := 8;

    fokColon, fokArea:
      Result := 7;

    fokExt:
      Result := 6;

    fokNeg, fokDiv100:
      Result := 5;

    fokPow:
      Result := 4;

    fokDiv, fokMul, fokIsect:
      Result := 3;

    fokAdd, fokSub:
      Result := 2;

    fokL, fokG, fokE, fokNE, fokLE, fokGE:
      Result := 1;

    else
      Result := 0; // unrecognized operators have the lowest priority
  end;
end;

function TBiffFormulaRPNStack.GetCount: Integer;
begin
  Result := Length(FCode)
end;

function TBiffFormulaRPNStack.GetInstruction(i: Integer): TBiffFormulaToken;
begin
  Assert((i >= 0) and (i < Count));
  Result := FCode[i];
end;

procedure TBiffFormulaRPNStack.Unroll;
var
  Op: TBiffFormulaToken;
begin
  while GetFrameOps > 0 do
  begin
    Op := Pop(FStack);
    Ensure(GetFrameArgs >= Op.Flags);
    Push(FCode, Op);
    Dec(FNumArgs, Op.Flags - 1);
  end;
end;

function TBiffFormulaRPNStack.Joinable(Op: TBiffFormulaOperatorKind): Boolean;
var
  a, b: TBiffFormulaOperatorKind;
begin
  if GetFrameOps = 0 then
    Result := True // this is the first op in the current frame
  else
  begin
    a := Top(FStack).Op;
    b := Op;

    { Given an expression (X a Y b Z) where a and b are operators,
      this function must return True if (Y b Z) must be calculated first;
      and return False if (X a Y) must be calculated first. }

    if OpPriority(a) < OpPriority(b) then
      Result := True

    else if OpPriority(a) > OpPriority(b) then
      Result := False

    else if (a = fokPow) and (b = fokPow) then
      Result := True

    else
      Result := False;
  end;
end;

{ TBiffFormulaParser }

procedure TBiffFormulaParser.BuildLexems(const s: string);
var
  Lex: TBiffFormulaLexer;
  i: Integer;
begin
  Lex := TBiffFormulaLexer.Create;

  try
    Lex.Formula := s;
    SetLength(FLexems, Lex.Count);

    for i := 0 to Lex.Count - 1 do
      FLexems[i] := Lex[i];
  finally
    Lex.Free;
  end;
end;

procedure TBiffFormulaParser.CleanLexems;
var
  n: Integer;

  procedure Move(i: Integer);
  begin
    Assert(i >= n);

    if i > n then
      FLexems[n] := FLexems[i];

    Inc(n);
  end;

  function Lex(i: Integer): TBiffFormulaLexem;
  begin
    if (i < 0) or (i > High(FLexems)) then
      Result.Kind := flkVoid
    else
      Result := FLexems[i]
  end;

  function CanIgnoreSpace(i: Integer): Boolean;
  var
    a, b: TBiffFormulaLexem;
  begin
    a := Lex(i - 1);
    b := Lex(i + 1);

    Result :=
      (a.Kind = flkVoid) or (b.Kind = flkVoid) or
      (a.Kind = flkOp) or (b.Kind = flkOp) or
      (a.Text = '(') or (b.Text = ')') or
      IsArgSep(a) or IsArgSep(b)
  end;

var
  i: Integer;
begin
  n := 0;

  for i := 0 to High(FLexems) do
  with FLexems[i] do
    case Kind of
      flkSpace:
        if not CanIgnoreSpace(i) then
          Move(i);

      else
        Move(i);
    end;

  SetLength(FLexems, n);
end;

function TBiffFormulaParser.IsArgSep(const Lex: TBiffFormulaLexem): Boolean;
begin
  Result := (Lex.Kind = flkSymbol) and ((Lex.Text = ',') or (Lex.Text = ';'))
end;

function TBiffFormulaParser.IsOp(const Lex: TBiffFormulaLexem; const Text: string): Boolean;
begin
  Result := (Lex.Kind = flkOp) and (Lex.Text = Text)
end;

procedure TBiffFormulaParser.Ensure(b: Boolean; const ErrorMsg: string);
begin
  if not b then
    Error(ErrorMsg)
end;

procedure TBiffFormulaParser.Error(const ErrorMsg: string);
begin
  raise EBiffFormulaParserError.Create(ErrorMsg)
end;

function TBiffFormulaParser.GetToken(i: Integer): TBiffFormulaToken;
begin
  Assert((i >= 0) and (i < Length(FCode)));
  Result := FCode[i];
end;

function TBiffFormulaParser.GetTokensCount: Integer;
begin
  Result := Length(FCode)
end;

function TBiffFormulaParser.CreateArgToken(Kind: TBiffFormulaOperatorKind;
  const Text: string; Flags: Integer): TBiffFormulaToken;
begin
  Result.Kind := ftkArg;
  Result.Op := Kind;
  Result.Text := Text;
  Result.Flags := Flags;
end;

function TBiffFormulaParser.Lexem(i: Integer): TBiffFormulaLexem;

  function GetLexKind(i: Integer): TBiffFormulaLexemKind;
  begin
    if (i >= 0) and (i < Length(FLexems)) then
      Result := FLexems[i].Kind
    else
      Result := flkVoid
  end;

begin
  case GetLexKind(FPos + i) of
    flkVoid:
      Result.Kind := flkVoid;

    flkSpace:
      begin
        Result.Kind := flkOp;
        Result.Text := ' ';
      end;

    else
      Result := FLexems[FPos + i];
  end;
end;

procedure TBiffFormulaParser.SkipLexem;
begin
  Assert(FPos < Length(FLexems));
  Inc(FPos);
end;

function TBiffFormulaParser.SkipLexemIf(Kind: TBiffFormulaLexemKind; out Text: string): Boolean;
begin
  Result := Lexem.Kind = Kind;

  if Result then
  begin
    Text := Lexem.Text;
    SkipLexem;
  end;
end;

function TBiffFormulaParser.SkipLexemIf(const Text: string): Boolean;
begin
  Result := Lexem.Text = Text;

  if Result then
    SkipLexem;
end;

procedure TBiffFormulaParser.Save;
begin
  SetLength(FSavedPos, Length(FSavedPos) + 1);
  FSavedPos[High(FSavedPos)] := FPos;
end;

procedure TBiffFormulaParser.Load;
begin
  Assert(Length(FSavedPos) > 0);
  FPos := FSavedPos[High(FSavedPos)];
  SetLength(FSavedPos, Length(FSavedPos) - 1);
end;

procedure TBiffFormulaParser.Discard;
begin
  Assert(Length(FSavedPos) > 0);
  SetLength(FSavedPos, Length(FSavedPos) - 1);
end;

function TBiffFormulaParser.OpKind(const s: string): TBiffFormulaOperatorKind;
begin
  Assert(s <> '');
  Result := fokVoid;

  case Length(s) of
    1:
      case s[1] of
        '+': Result := fokAdd;
        '-': Result := fokSub;
        '*': Result := fokMul;
        '/': Result := fokDiv;
        '^': Result := fokPow;
        '<': Result := fokL;
        '>': Result := fokG;
        '=': Result := fokE;
        ':': Result := fokColon;
        ' ': Result := fokIsect;
        '!': Result := fokExt;
        '&': Result := fokJoin;
      end;

    2:
      if s = '<=' then
        Result := fokLE

      else if s = '>=' then
        Result := fokGE

      else if s = '<>' then
        Result := fokNE;
  end;

  Ensure(Result <> fokVoid, Format('Operator "%s" is unknown', [s]));
end;

procedure TBiffFormulaParser.Push(Arg: TBiffFormulaOperatorKind; const Text: string; Flags: Integer);
begin
  FRPN.PushArg(Arg, Text, Flags);
  FRPN.PushOp(fokPush, 'push', 1);
end;

procedure TBiffFormulaParser.CopyCodeFromRPNStack;
var
  i: Integer;
begin
  SetLength(FCode, FRPN.Count);

  for i := 0 to FRPN.Count - 1 do
    FCode[i] := FRPN[i];
end;

procedure TBiffFormulaParser.Parse(const s: string);
begin
  BuildLexems(s);
  CleanLexems;
  FPos := 0;

  try
    FRPN := TBiffFormulaRPNStack.Create;
    Ensure(ParseFormula);
    CopyCodeFromRPNStack;
  finally
    FRPN.Free;
    FRPN := nil;
  end;
end;

function TBiffFormulaParser.ReadCell(out t: TBiffFormulaToken): Boolean;
var
  AbsCol, AbsRow: Boolean;
  ColName, RowName: string;
  Flags: Integer;
begin
  Result := False;
  Save;

  AbsCol := SkipLexemIf('$');

  if not SkipLexemIf(flkName, ColName) then
  begin
    Load;
    Exit;
  end;

  AbsRow := SkipLexemIf('$');

  if not SkipLexemIf(flkInt, RowName) then
  begin
    Load;
    Exit;
  end;

  Flags := 0;

  if AbsRow then
    Flags := Flags or 1;

  if AbsCol then
    Flags := Flags or 2;

  Discard;
  t := CreateArgToken(fokCell, ColName + RowName, Flags);
  Result := True;
end;

function TBiffFormulaParser.ReadName(out s: string): Boolean;
var
  Name: string;
begin
  if Lexem.Kind <> flkName then
    Result := False
  else
  begin
    Name := Lexem.Text;
    SkipLexem;

    if Lexem.Kind = flkInt then
    begin
      Name := Name + Lexem.Text;
      SkipLexem;
    end;

    s := Name;
    Result := True;
  end;
end;

function TBiffFormulaParser.ReadNumber(out s: string): Boolean;
var
  Num: string;
begin
  if Lexem.Kind <> flkInt then
    Result := False
  else
  begin
    Num := Lexem.Text;
    SkipLexem;

    if Lexem.Text = '.' then
    begin
      Num := Num + '.';
      SkipLexem;

      if Lexem.Kind = flkInt then
      begin
        Num := Num + Lexem.Text;
        SkipLexem;
      end
      else
        Num := Num + '0';
    end;

    s := Num;
    Result := True;
  end;
end;

function TBiffFormulaParser.ReadOp(out s: string): Boolean;
begin
  Result := Lexem.Kind = flkOp;

  if Result then
  begin
    s := Lexem.Text;
    SkipLexem;
  end;
end;

function TBiffFormulaParser.ReadSheet(out s: string): Boolean;
begin
  Result := ReadString(s) or ReadName(s)
end;

function TBiffFormulaParser.ReadString(out s: string): Boolean;
begin
  Result := Lexem.Kind = flkString;

  if Result then
  begin
    s := Lexem.Text;
    SkipLexem;
  end;
end;

function TBiffFormulaParser.ReadSym(const s: string): Boolean;
begin
  Result := (Lexem.Kind <> flkVoid) and (Lexem.Text = s);

  if Result then
    SkipLexem;
end;

function TBiffFormulaParser.ParseTerm: Boolean;
begin
  Result :=
    ParseExtArea or
    ParseExtCell or
    ParseFuncCall or
    ParseString or
    ParseNumber or
    ParseNameConst or
    ParseArea or
    ParseCell
end;

function TBiffFormulaParser.ParseFormula: Boolean;
begin
  FRPN.PushFrame;
  Ensure(ParsePPTerm);

  while ParseBinOp do
    Ensure(ParsePPTerm, 'Expression cannot end with an operator');

  FRPN.PopFrame;
  Result := True;
end;

function TBiffFormulaParser.ParseNameConst: Boolean;
var
  s: string;
begin
  Save;
  Result := ReadName(s);

  if Result then
  begin
    s := AnsiUpperCase(s);

    if s = 'TRUE' then
      Push(fokBool, s, 1)

    else if s = 'FALSE' then
      Push(fokBool, s, 0)

    else
      Result := False;
  end;

  if Result then
    Discard
  else
    Load
end;

function TBiffFormulaParser.ParseNumber: Boolean;
var
  s: string;
begin
  Result := ReadNumber(s);

  if Result then
    Push(fokNumber, s);
end;

function TBiffFormulaParser.ParsePostOp: Boolean;
begin
  if IsOp(Lexem, '%') then
  begin
    FRPN.PushOp(fokDiv100, '%', 1);
    SkipLexem;
    Result := True;
  end
  else
    Result := False
end;

function TBiffFormulaParser.ParsePPTerm: Boolean;
begin
  ParsePrefOp;
  Ensure(ParseTerm);
  ParsePostOp;
  Result := True;
end;

function TBiffFormulaParser.ParsePrefOp: Boolean;
begin
  if IsOp(Lexem, '-') then
  begin
    FRPN.PushOp(fokNeg, '-', 1);
    SkipLexem;
    Result := True;
  end else if IsOp(Lexem, '+') then
  begin
    SkipLexem;
    Result := True;
  end
  else
    Result := False
end;

function TBiffFormulaParser.ParseCell: Boolean;
var
  t: TBiffFormulaToken;
begin
  Result := ReadCell(t);

  if Result then
    Push(fokCell, t.Text, t.Flags);
end;

function TBiffFormulaParser.ParseArea: Boolean;
var
  c1, c2: TBiffFormulaToken;
begin
  Save;
  Result := ReadCell(c1) and ReadSym(':') and ReadCell(c2);

  if Result then
  begin
    Discard;

    FRPN.PushArg(fokCell, c1.Text, c1.Flags);
    FRPN.PushArg(fokCell, c2.Text, c2.Flags);
    FRPN.PushOp(fokArea, ':', 2);
  end
  else
    Load;
end;

function TBiffFormulaParser.ParseExtArea: Boolean;
var
  s: string;
  c1, c2: TBiffFormulaToken;
begin
  Save;
  Result := ReadSheet(s) and ReadSym('!') and ReadCell(c1) and ReadSym(':') and ReadCell(c2);

  if Result then
  begin
    Discard;

    FRPN.PushArg(fokString, s);
    FRPN.PushArg(fokCell, c1.Text, c1.Flags);
    FRPN.PushArg(fokCell, c2.Text, c2.Flags);
    FRPN.PushOp(fokExt, '!', 3);
  end
  else
    Load;
end;

function TBiffFormulaParser.ParseExtCell: Boolean;
var
  s: string;
  c: TBiffFormulaToken;
begin
  Save;
  Result := ReadSheet(s) and ReadSym('!') and ReadCell(c);

  if Result then
  begin
    Discard;

    FRPN.PushArg(fokString, s);
    FRPN.PushArg(fokCell, c.Text, c.Flags);
    FRPN.PushOp(fokExt, '!', 2);
  end
  else
    Load;
end;

function TBiffFormulaParser.ParseString: Boolean;
var
  s: string;
begin
  Result := ReadString(s);

  if Result then
    Push(fokString, s);
end;

function TBiffFormulaParser.ParseBinOp: Boolean;
var
  s: string;
begin
  Result := ReadOp(s);

  if Result then
    FRPN.PushOp(OpKind(s), s, 2);
end;

function TBiffFormulaParser.ParseFuncCall: Boolean;
var
  s: string;
begin
  Save;

  if not ReadName(s) then
    s := '';

  Result := ReadSym('(');

  if not Result then
  begin
    Load;
    Exit;
  end;

  FRPN.PushFrame(s);

  if Lexem.Text <> ')' then
  begin
    Ensure(ParseFormula, 'Argument expected after "("');

    while IsArgSep(Lexem) do
    begin
      SkipLexem;

      if IsArgSep(Lexem) then
        Push(fokNull, 'void')
      else
        Ensure(ParseFormula, 'Argument expected after ","');
    end;
  end;

  Ensure(ReadSym(')'));

  if s = '' then
    FRPN.PushOp(fokId, 'id', 1);

  FRPN.PopFrame;
  Discard;
  Result := True;
end;

{ TBiffFormulaCompiler }

constructor TBiffFormulaCompiler.Create;
begin
  FParser := TBiffFormulaParser.Create;
  FEmitter := TBiffFormulaCodeEmitter.Create;
  FCode := TMemoryStream.Create;

  FEmitter.Output := FCode;
end;

destructor TBiffFormulaCompiler.Destroy;
begin
  FParser.Free;
  FEmitter.Free;
  FCode.Free;
  inherited;
end;

procedure TBiffFormulaCompiler.Ensure(b: Boolean; const Fmt: string; const Args: array of const);
begin
  Ensure(b, Format(Fmt, Args))
end;

procedure TBiffFormulaCompiler.Ensure(b: Boolean; const ErrorMsg: string);
begin
  if not b then
    Error('%s', [ErrorMsg])
end;

procedure TBiffFormulaCompiler.Error(const Fmt: string; const Args: array of const);
begin
  raise EBiffFormulaCompilerError.CreateFmt(Fmt, Args)
end;

procedure TBiffFormulaCompiler.EmitArea(const Cell1, Cell2: TBiffFormulaToken);
begin
  Ensure(IsCell(Cell1) and IsCell(Cell2));
  FEmitter.PushArea(GetCellPos(Cell1), GetCellPos(Cell2));
end;

procedure TBiffFormulaCompiler.EmitExtArea(const Sheet, Cell1, Cell2: TBiffFormulaToken);
begin
  Ensure(IsStr(Sheet) and IsCell(Cell1) and IsCell(Cell2));
  FEmitter.PushExtArea(LinkTable.GetInternalSheetRef(Sheet.Text), GetCellPos(Cell1), GetCellPos(Cell2));
end;

procedure TBiffFormulaCompiler.EmitExtCell(const Sheet, Cell: TBiffFormulaToken);
begin
  Ensure(IsStr(Sheet) and IsCell(Cell));
  FEmitter.PushExtCell(LinkTable.GetInternalSheetRef(Sheet.Text), GetCellPos(Cell));
end;

procedure TBiffFormulaCompiler.EmitFunc(Name: string; NumArgs: Integer);
var
  Func: TBiffFormulaFunc;
begin
  Assert(Name <> '');
  Name := AnsiUpperCase(Name);
  Ensure(TBiffFormulaFuncList.Exists(Name), 'Function %s is not supported', [Name]);

  Func := TBiffFormulaFuncList.Get(Name);
  Assert(Func.Name = Name);

  Ensure((Func.MinArgs <= NumArgs) and (NumArgs <= Func.MaxArgs),
    'Function %s cannot be called with %d arguments', [Name, NumArgs]);

  if Func.MinArgs = Func.MaxArgs then
    FEmitter.Call(Func.Id)
  else
    FEmitter.Call(Func.Id, NumArgs);
end;

procedure TBiffFormulaCompiler.EmitIdFunc(NumArgs: Integer);
begin
  if NumArgs = 1 then
    FEmitter.CallId
end;

procedure TBiffFormulaCompiler.EmitNum(Num: Double);
begin

  { A number can be compiled in three ways:

      double(Num)     [9 bytes]
      int(Num)        [5 bytes]
      int(Num) neg    [6 bytes]

    This method chooses a way that yields the smallest code. }

  if (Num <> Ceil(Num)) or (Num > $ffff) or (Num < -$ffff) then
    FEmitter.Push(Num)
  else if Num >= 0 then
    FEmitter.Push(Ceil(Num))
  else
  begin
    FEmitter.Push(Ceil(-Num));
    FEmitter.Neg;
  end;
end;

procedure TBiffFormulaCompiler.EmitOp(Kind: TBiffFormulaOperatorKind; NumArgs: Integer);
begin
  case NumArgs of
    1:
      case Kind of
        fokNeg:     FEmitter.Neg;
        fokDiv100:  FEmitter.Div100;

        else Error('Unsupported unary operator %d', [Integer(Kind)]);
      end;

    2:
      case Kind of
        fokAdd:       FEmitter.Add;
        fokSub:       FEmitter.Sub;
        fokDiv:       FEmitter.Divide;
        fokMul:       FEmitter.Mul;
        fokPow:       FEmitter.Pow;
        fokL:         FEmitter.CmpL;
        fokG:         FEmitter.CmpG;
        fokE:         FEmitter.CmpE;
        fokNE:        FEmitter.CmpNE;
        fokLE:        FEmitter.CmpLE;
        fokGE:        FEmitter.CmpGE;
        fokIsect:     FEmitter.Intersect;
        fokJoin:      FEmitter.Join;
        fokColon:     FEmitter.Range;

        else Error('Unsupported binary operator %d', [Integer(Kind)]);
      end;

    else Error('Unsupported %d-ary operator %d', [NumArgs, Integer(Kind)]);
  end;
end;

procedure TBiffFormulaCompiler.EmitPush(const t: TBiffFormulaToken);

  { StrToFloat gets decimal separator from locale settings.
    Here this separator is always the dot. }

  function StrToDouble(const s: string): Double;
  var
    p, n: Integer;
  begin
    Assert(s <> '');

    n := Length(s);
    p := n;

    while (p > 0) and (s[p] <> '.') do
      Dec(p);

    if p = 0 then
      Result := StrToInt(s)
    else if p = n then
      Result := StrToInt(Copy(s, 1, n - 1))
    else if p = 1 then
      Result := StrToDouble('0' + s)
    else
      Result := StrToInt(Copy(s, 1, p - 1)) + StrToInt(Copy(s, p + 1, n - p))/Power(10, n - p)
  end;

begin
  case t.Op of
    fokNumber:
      EmitNum(StrToDouble(t.Text));

    fokNull:
      FEmitter.PushNull;

    fokCell:
      FEmitter.PushCell(GetCellPos(t));

    fokString:
      FEmitter.Push(t.Text);

    fokBool:
      FEmitter.Push(t.Flags <> 0);

    else
      Error('Cannot push value (%s) of type %d', [t.Text, Integer(t.Op)]);
  end;
end;

function TBiffFormulaCompiler.GetCellPos(const t: TBiffFormulaToken): TBiffFormulaCellRef;

  { Converts a column name like GH into a zero based column index }

  function GetColIndex(const s: string): Integer;
  var
    i, n: Integer;
  begin
    Assert(s <> '');

    n := 1;
    Result := 0;

    for i := Length(s) downto 1 do
    begin
      Assert(AnsiChar(s[i]) in ['A'..'Z']);
      Inc(Result, n * (Ord(s[i]) - Ord('A') + 1));
      n := n * 26;
    end;

    Dec(Result);
  end;

var
  i: Integer;
  Ref, ColName, RowName: string;
begin
  Assert(IsCell(t));

  Ref := AnsiUpperCase(t.Text);
  i := 1;

  while AnsiChar(Ref[i]) in ['A'..'Z'] do
    Inc(i);

  Assert((i > 1) and (i <= Length(Ref)));

  ColName := Copy(Ref, 1, i - 1);
  RowName := Copy(Ref, i, Length(Ref) - i + 1);

  with Result do
  begin
    Row := StrToInt(RowName) - 1;
    Col := GetColIndex(ColName);
    AbsRow := t.Flags and 1 > 0;
    AbsCol := t.Flags and 2 > 0;
  end;
end;

function TBiffFormulaCompiler.GetLinkTable: TBiffLinkTable;
begin
  Result := FLinkTable;
  Ensure(Result <> nil, 'Link table is not assigned');
end;

function TBiffFormulaCompiler.IsCell(const t: TBiffFormulaToken): Boolean;
begin
  Result := (t.Kind = ftkArg) and (t.Op = fokCell) and (Length(t.Text) > 1)
end;

function TBiffFormulaCompiler.IsStr(const t: TBiffFormulaToken): Boolean;
begin
  Result := (t.Kind = ftkArg) and (t.Op = fokString)
end;

procedure TBiffFormulaCompiler.SaveToStream(Stream: TStream);
begin
  Stream.CopyFrom(FCode, 0)
end;

procedure TBiffFormulaCompiler.SelectToken(i: Integer);
begin
  Assert((i >= 0) and (i <= FParser.Count));

  if i < FParser.Count then
    FEmitter.RetMode := FRetTypes[i];

  FPos := i;
end;

procedure TBiffFormulaCompiler.SkipToken;
begin
  SelectToken(FPos + 1);
end;

function TBiffFormulaCompiler.Token(i: Integer): TBiffFormulaToken;
begin
  if (FPos + i >= 0) and (FPos + i < FParser.Count) then
    Result := FParser[FPos + i]
  else
    Result.Kind := ftkVoid
end;

procedure TBiffFormulaCompiler.CalcRetTypes;
var
  Stack: array of Integer;
  StackTop: Integer;

  procedure Push(Id: Integer; RepNum: Integer = 1);
  begin
    Assert(RepNum >= 1);

    for RepNum := RepNum downto 1 do
    begin
      Assert(StackTop < Length(Stack));
      Stack[StackTop] := Id;
      Inc(StackTop);
    end;
  end;

  procedure Pop(n: Integer);
  begin
    Assert(StackTop >= n);
    Dec(StackTop, n);
  end;

  function Top(i: Integer): Integer;
  begin
    Assert(StackTop - i - 1 >= 0);
    Result := Stack[StackTop - i - 1];
  end;

  procedure SetRetType(i: Integer; t: TBiffFormulaRetType);
  begin
    Assert(FRetTypes[i] = frtVoid, 'Ret type is already set');
    FRetTypes[i] := t;
  end;

  function GetArgType(const t: TBiffFormulaToken; i: Integer): TBiffFormulaRetType;
  begin
    Assert(t.Kind = ftkOp);
    Result := frtVoid;

    case t.Op of
      fokColon: Result := frtRef;
      fokIsect: Result := frtRef;
      fokFunc:
        case TBiffFormulaFuncList.GetArgType(t.Text, i) of
          'R': Result := frtRef;
          'V': Result := frtVal;
          'A': Result := frtArray;
        end;

      else Result := frtVal;
    end;
  end;

  procedure AssignArgTypes(const t: TBiffFormulaToken);
  var
    i: Integer;
  begin
    Assert(StackTop >= t.Flags);

    for i := 1 to t.Flags do
      SetRetType(Top(t.Flags - i), GetArgType(t, i - 1))
  end;

var
  i, n: Integer;
  t: TBiffFormulaToken;
begin
  n := FParser.Count;

  SetLength(FRetTypes, n);
  SetLength(Stack, n);
  StackTop := 0;

  for i := 0 to n - 1 do
  begin
    t := FParser[i];

    case t.Kind of
      ftkArg:
        Push(i);

      ftkOp:
        begin
          AssignArgTypes(t);
          Pop(t.Flags);
          Push(i);
        end;
     end;
  end;

  SetRetType(Top(0), frtVal);
  Pop(1);
  Ensure(StackTop = 0, 'Not all tokens produced by parser are supported by compiler');
end;

procedure TBiffFormulaCompiler.Compile(const s: string);
begin
  FParser.Formula := s;
  CalcRetTypes;
  SelectToken(0);

  while FPos < FParser.Count do
    CompileToken
end;

procedure TBiffFormulaCompiler.CompileToken;
begin
  if Token.Kind = ftkOp then
    case Token.Op of
      fokPush:
        EmitPush(Token(-1));

      fokId:
        EmitIdFunc(Token.Flags);

      fokFunc:
        EmitFunc(Token.Text, Token.Flags);

      fokArea:
        EmitArea(Token(-2), Token(-1));

      fokExt:
        case Token.Flags of
          2: EmitExtCell(Token(-2), Token(-1));
          3: EmitExtArea(Token(-3), Token(-2), Token(-1));

          else Error('Operator "!" does not support %d arguments', [Token.Flags]);
        end;

      fokAdd, fokSub, fokMul, fokDiv, fokPow,
      fokL, fokG, fokLE, fokGE, fokNE, fokE,
      fokIsect, fokJoin, fokColon, fokDiv100, fokNeg:
        EmitOp(Token.Op, Token.Flags);

      else
        Error('Operator (%s) %d not supported', [Token.Text, Integer(Token.Op)]);
    end;

  SkipToken;
end;

{ TBiffFormulaFuncList }

class function TBiffFormulaFuncList.GetCount: Integer;
begin
  Result := Length(FuncArray)
end;

class function TBiffFormulaFuncList.GetFunc(i: Integer): TBiffFormulaFunc;
begin
  Result := FuncArray[i]
end;

class procedure TBiffFormulaFuncList.SetCount(n: Integer);
begin
  SetLength(FuncArray, n)
end;

class procedure TBiffFormulaFuncList.SetFunc(i: Integer; const f: TBiffFormulaFunc);
begin
  FuncArray[i] := f
end;

class procedure TBiffFormulaFuncList.Add(Id: Integer; Name: string; MinArgs,
  MaxArgs: Integer; RetType: Char; ArgTypes: string; Volatile: Boolean);

  function IsValid(const s: string): Boolean;
  var
    i: Integer;
  begin
    for i := 1 to Length(s) do
      if not (AnsiChar(s[i]) in ['r', 'v', 'a']) then
      begin
        Result := False;
        Exit;
      end;

    Result := True;
  end;

var
  f: TBiffFormulaFunc;
begin
  Assert(Name <> '');
  Assert((MinArgs >= 0) and (MaxArgs >= MinArgs));
  Assert(IsValid(RetType));
  Assert(IsValid(ArgTypes));

  if MaxArgs > 0 then
    Assert(ArgTypes <> '');

  f.Name := AnsiUpperCase(Name);
  f.Id := Id;
  f.MinArgs := MinArgs;
  f.MaxArgs := MaxArgs;
  f.Volatile := Volatile;
  f.RetType := RetType;
  f.ArgTypes := ArgTypes;

  Add(f);
end;

class function TBiffFormulaFuncList.Find(Name: string): Integer;
begin
  Name := AnsiUpperCase(Name);

  for Result := 0 to GetCount - 1 do
    if GetFunc(Result).Name >= Name then
      Exit;

  Result := GetCount;
end;

class procedure TBiffFormulaFuncList.Add(const f: TBiffFormulaFunc);
var
  i, j: Integer;
begin
  i := Find(f.Name);
  Assert((i = GetCount) or (GetFunc(i).Name > f.Name), f.Name + ' already added');
  SetCount(GetCount + 1);

  for j := GetCount - 1 downto i + 1 do
    SetFunc(j, GetFunc(j - 1));

  SetFunc(i, f);
end;

class function TBiffFormulaFuncList.Get(const Name: string): TBiffFormulaFunc;
begin
  Init;
  Result := GetFunc(Find(Name))
end;

class function TBiffFormulaFuncList.GetArgType(const Name: string; i: Integer): Char;
begin
  with Get(Name) do
  begin
    Assert((i >= 0) and (i < MaxArgs));

    if i + 1 <= Length(ArgTypes) then
      Result := ArgTypes[i + 1]
    else
      Result := ArgTypes[Length(ArgTypes)];

    Result := UpperCase(Result)[1];
    Assert(AnsiChar(Result) in ['R', 'V', 'A']);
  end;
end;

class function TBiffFormulaFuncList.Exists(const Name: string): Boolean;
var
  i: Integer;
begin
  Init;
  i := Find(Name);

  Result := (i < GetCount) and (GetFunc(i).Name = Name)
end;

class procedure TBiffFormulaFuncList.Init;
begin
  if GetCount > 0 then
    Exit;

  { http://sc.openoffice.org/excelfileformat.pdf
    http://msdn.microsoft.com/en-us/library/dd904817.aspx }

  Add(0,    'count',        1,  30, 'v', 'r');
  Add(1,    'if',           2,  3,  'r', 'vr');
  Add(2,    'isna',         1,  1,  'v', 'v');
  Add(3,    'iserror',      1,  1,  'v', 'v');
  Add(4,    'sum',          1,  30, 'v', 'r');
  Add(5,    'average',      1,  30, 'v', 'r');
  Add(6,    'min',          1,  30, 'v', 'r');
  Add(7,    'max',          1,  30, 'v', 'r');
  Add(8,    'row',          0,  1,  'v', 'r');
  Add(9,    'column',       0,  1,  'v', 'r');
  Add(10,   'na',           0,  0,  'v', '');

  Add(13,   'dollar',       1,  2,  'v', 'v');
  Add(14,   'fixed',        2,  2,  'v', 'v');

  Add(20,   'sqrt',         1,  1,  'v', 'v');
  Add(21,   'exp',          1,  1,  'v', 'v');
  Add(22,   'ln',           1,  1,  'v', 'v');
  Add(23,   'log10',        1,  1,  'v', 'v');
  Add(24,   'abs',          1,  1,  'v', 'v');
  Add(25,   'int',          1,  1,  'v', 'v');
  Add(26,   'sign',         1,  1,  'v', 'v');
  Add(27,   'round',        2,  2,  'v', 'v');
  Add(28,   'lookup',       2,  3,  'v', 'vr');
  Add(29,   'index',        2,  4,  'r', 'rv');
  Add(30,   'rept',         2,  2,  'v', 'v');
  Add(31,   'mid',          3,  3,  'v', 'v');
  Add(32,   'len',          1,  1,  'v', 'v');
  Add(33,   'value',        1,  1,  'v', 'v');
  Add(34,   'true',         0,  0,  'v', '');
  Add(35,   'false',        0,  0,  'v', '');
  Add(36,   'and',          1,  30, 'v', 'r');
  Add(37,   'or',           1,  30, 'v', 'r');
  Add(38,   'not',          1,  1,  'v', 'v');
  Add(39,   'mod',          2,  2,  'v', 'v');

  Add(48,   'text',         2,  2,  'v', 'v');

  Add(63,   'rand',         0,  0,  'v', '');
  Add(64,   'match',        2,  3,  'v', 'vr');
  Add(65,   'date',         3,  3,  'v', 'v');
  Add(66,   'time',         3,  3,  'v', 'v');
  Add(67,   'day',          1,  1,  'v', 'v');
  Add(68,   'month',        1,  1,  'v', 'v');
  Add(69,   'year',         1,  1,  'v', 'v');
  Add(70,   'weekday',      1,  2,  'v', 'v');
  Add(71,   'hour',         1,  1,  'v', 'v');
  Add(72,   'minute',       1,  1,  'v', 'v');
  Add(73,   'second',       1,  1,  'v', 'v');
  Add(74,   'now',          0,  0,  'v', '');
  Add(75,   'areas',        1,  1,  'v', 'r');
  Add(76,   'rows',         1,  1,  'v', 'r');
  Add(77,   'columns',      1,  1,  'v', 'r');
  Add(78,   'offset',       3,  5,  'r', 'rv');
  Add(79,   'absref',       2,  2,  'r', 'vr');
  Add(80,   'relref',       2,  2,  'r', 'rr');

  Add(82,   'search',       2,  3,  'v', 'v');
  Add(83,   'transpose',    1,  1,  'a', 'a');
  Add(84,   'error',        0,  2,  'v', 'v');

  Add(86,   'type',         1,  1,  'v', 'v');
  Add($5a,  'deref',        1,  1,  'v', 'r');

  Add(100,  'choose',       2,  30, 'r', 'vr');
  Add(101,  'hlookup',      3,  4,  'v', 'vrrv');
  Add(102,  'vlookup',      3,  4,  'v', 'vrrv');

  Add(105,  'isref',        1,  1,  'v', 'r');
  Add(109,  'log',          1,  2,  'v', 'v');

  Add(111,  'char',         1,  1,  'v', 'v');
  Add(112,  'lower',        1,  1,  'v', 'v');
  Add(113,  'upper',        1,  1,  'v', 'v');

  Add(115,  'left',         1,  2,  'v', 'v');
  Add(116,  'right',        1,  2,  'v', 'v');
  Add(117,  'exact',        2,  2,  'v', 'v');
  Add(118,  'trim',         1,  2,  'v', 'v');
  Add(119,  'replace',      4,  4,  'v', 'v');
  Add(120,  'substitute',   3,  4,  'v', 'v');
  Add(121,  'code',         1,  1,  'v', 'v');

  Add(124,  'find',         2,  3,  'v', 'v');
  Add(125,  'cell',         1,  2,  'v', 'vr', True);
  Add(126,  'iserr',        1,  1,  'v', 'v');
  Add(127,  'istext',       1,  1,  'v', 'v');
  Add(128,  'isnumber',     1,  1,  'v', 'v');
  Add(129,  'isblank',      1,  1,  'v', 'v');
  Add(130,  't',            1,  1,  'v', 'r');
  Add(131,  'n',            1,  1,  'v', 'r');

  Add(140,  'datevalue',    1,  1,  'v', 'v');
  Add(141,  'timevalue',    1,  1,  'v', 'v');

  Add(147,  'textref',      1,  2,  'r', 'v');
  Add(148,  'indirect',     1,  2,  'r', 'v');

  Add(162,  'clean',        1,  1,  'v', 'v');
  Add(169,  'counta',       0,  30, 'v', 'r');
  Add(190,  'isnontext',    1,  1,  'v', 'v');

  Add(197,  'trunc',        1,  2,  'v', 'v');
  Add(198,  'islogical',    1,  1,  'v', 'v');

  Add(205,  'findb',        2,  3,  'v', 'v');
  Add(206,  'searchb',      2,  3,  'v', 'v');
  Add(207,  'replaceb',     4,  4,  'v', 'v');
  Add(208,  'leftb',        1,  2,  'v', 'v');
  Add(209,  'rightb',       1,  2,  'v', 'v');
  Add(210,  'midb',         3,  3,  'v', 'v');
  Add(211,  'lenb',         1,  1,  'v', 'v');
  Add(212,  'roundup',      2,  2,  'v', 'v');
  Add(213,  'rounddown',    2,  2,  'v', 'v');

  Add(219,  'address',      2,  5,  'v', 'v');
  Add(221,  'today',        0,  0,  'v', '', True);
  Add(257,  'evaluate',     1,  1,  'v', 'v');
  Add(261,  'error.type',   1,  1,  'v', 'v');
  Add(277,  'confidence',   3,  3,  'v', 'v');
  Add(279,  'even',         1,  1,  'v', 'v');
  Add(285,  'floor',        2,  2,  'v', 'v');
  Add(288,  'ceiling',      2,  2,  'v', 'v');
  Add(298,  'odd',          1,  1,  'v', 'v');

  Add(336,  'concatenate',  0,  30, 'v', 'v');
  Add(337,  'power',        2,  2,  'v', 'v');

  Add(344,  'subtotal',     2,  30, 'v', 'vr');
  Add(345,  'sumif',        2,  3,  'v', 'rvr');
  Add(346,  'countif',      2,  2,  'v', 'rv');
  Add(347,  'countblank',   1,  1,  'v', 'r');

  Add(350,  'ispmt',        4,  4,  'v', 'v');
  Add(351,  'dateif',       3,  3,  'v', 'v');
  Add(352,  'datestring',   1,  1,  'v', 'v');
  Add(353,  'numberstring', 2,  2,  'v', 'v');

  Add(359,  'hyperlink',    1,  2,  'v', 'v');

  Add(362,  'maxa',         1,  30, 'v', 'r');
  Add(363,  'mina',         1,  30, 'v', 'r');
end;

end.
