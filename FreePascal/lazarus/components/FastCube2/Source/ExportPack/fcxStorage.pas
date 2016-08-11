
{******************************************}
{                                          }
{             FastReport v4.0              }
{             Data Structures              }
{                                          }
{         Copyright (c) 1998-2011          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxStorage;

{$I fcx.inc}

interface

uses
  Classes,
  SysUtils,
  fcxHeap;
{$ELSE}
{$INCLUDE fcx.inc}

interface

uses
  System.SysUtils, System.Classes,
  FMX.fcxHeap;

{$ENDIF}

type

  { Old Delphi versions don't declare these classes }

  ENotImplemented = class(Exception);
  ENotSupportedException = class(Exception);

  { Provides the TStream interface to a piece of a stream }

  TProxyStream = class(TStream)
  private
    FStream: TStream;
    FOffset: Longint;
    FPos: Longint;
    FSize: Longint;

    procedure Init(Stream: TStream; Offset, Size: Longint);
    function AdjustRange(var Len: Integer): Boolean;
  public
    constructor Create(Stream: TStream; Offset, Size: Longint);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property Offset: Longint read FOffset;
    property BaseStream: TStream read FStream;
  end;

  { Provides additional functions for writing
    to an already existing stream. }

  TFmtStream = class(TStream)
  private
    FOutput: TStream;
    FOwn: Boolean;
    FFormatted: Boolean;
    FIndent: Integer;
  public
    constructor Create(Output: TStream; Own: Boolean);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    procedure PutsRawA(const s: AnsiString); overload;
    procedure PutsRaw(const s: string); overload;
    procedure PutsRawA(const Fmt: AnsiString; const Args: array of const); overload;
    procedure PutsRaw(const Fmt: string; const Args: array of const); overload;

    procedure PutsA(const s: AnsiString); overload;
    procedure Puts(const s: string = ''); overload;
    procedure PutsA(const Fmt: AnsiString; const Args: array of const); overload;
    procedure Puts(const Fmt: string; const Args: array of const); overload;

    procedure IncIndent(Step: Integer);

    property Formatted: Boolean read FFormatted write FFormatted;
    property Indent: Integer read FIndent write FIndent;
  end;

  { List of objects.

    This class assumes that it contains a list
    of TObject instances. When it's destroyed,
    it also destroys all contained objects. }

  TObjList = class(TList)
    procedure Clear; override;
  end;

  { Encodes an input stream and writes the result to an output stream.
    After sending the whole input stream to the encoder, it must be
    destroyed in order to force the encoder to write special ending bytes
    to the output stream. Example:

      ms := TMemoryStream.Create;

      s := 'data:image/jpeg;base64,';
      ms.Write(s[1], Length(s));

      with TBase64Encoder.Create(ms) do
      try
        Picture.SaveToStream(This)
      finally
        Free
      end; }

  TBase64Encoder = class(TStream)
  private

    FOutput: TStream;
    FCache, FUsed: Integer;
    FMap: array[0..63] of Byte;

  protected

    procedure InitMap;
    procedure Encode(a, n: Integer);
    procedure Finalise;

  public

    constructor Create(Output: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    function This: TBase64Encoder;

  end;

  { Performs simple hex encoding: each input byte is replaced with a 2-byte
    string containing the hexadecimal value of that byte. }

  THexEncoder = class(TStream)
  private
    FOutput: TStream;
  public
    constructor Create(Output: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { Writes the end-of-line char after every specified number of bytes }

  TLineSplitter = class(TStream)
  private
    FSep: AnsiString;
    FOutput: TStream;
    FLength: Integer;
    FWritten: Integer;
  public
    constructor Create(Output: TStream; Length: Integer; Sep: AnsiString = #13#10);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  { Block stream.

    This class virtually divides all data into
    equally sized blocks, except for the last
    block that can has less size. }

  TBlockStream = class(TMemoryStream)
  private

    FBlockShift: LongInt;

    function GetBlocksCount: LongInt;
    function GetBlockData(i: LongInt): Pointer;
    function GetBlockSize(i: LongInt): LongInt;
    function GetCurrentBlock: LongInt;

  public

    constructor Create(BlockSizeShift: LongInt = 4);

    { Writes a specified count of bytes from a
      specified value and returns a pointer to
      the written data. }

    function Imm(Value, Count: LongInt): Pointer;

    { Fills a specified count of bytes with a specified
      value. }

    procedure Fill(Value: Byte; Count: LongInt);

    { Fills the remaining space of the current block
      with a specified value. }

    procedure EndBlock(Value: Byte);

    { The data of the stream is virtually split into
      equally sized blocks (except for the last block,
      that can have less size). }

    property BlocksCount: LongInt read GetBlocksCount;
    property BlockData[i: LongInt]: Pointer read GetBlockData;
    property BlockSize[i: LongInt]: LongInt read GetBlockSize;

    { Returns an index to a block where the next byte
      will be written. }

    property CurrentBlock: LongInt read GetCurrentBlock;

    property BlockShift: LongInt read FBlockShift;

  end;

  { Cached stream.

    This class is a layer on another (file)stream and
    implements the interface of a write-only stream.

    It uses an intermediate small cache in physical memory
    to avoid calling the underlying stream for writing
    small data blocks. }

  TCachedStream = class(TStream)
  private

    FStream: TStream;
    FDeleteOnDestroy: Boolean;
    FChunk: array of Byte;
    FUsed: Integer;

    function GetCacheSize: Integer;
    procedure SetCacheSize(Size: Integer);

  protected

    procedure FlushCache;

  public

    constructor Create(Stream: TStream; DeleteOnDestroy: Boolean);
    destructor Destroy; override;

    property CacheSize: Integer read GetCacheSize write SetCacheSize;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  { Pointer-to-Pointer map interface }

  TMap = class
  protected
    procedure SetItemExistence(Key: Pointer; b: Boolean); virtual; abstract;
    function  ItemExists(Key: Pointer): Boolean; virtual; abstract;
    function  GetItem(Key: Pointer): Pointer; virtual; abstract;
    procedure SetItem(Key: Pointer; Value: Pointer); virtual; abstract;
  public
    property Exists[Key: Pointer]: Boolean read ItemExists write SetItemExistence;
    property Items[Key: Pointer]: Pointer read GetItem write SetItem; default;
  end;

  TOrderedMap = class(TMap)
  public
    { Gets all key-value pairs. Keys will be sorted by ascending.
      Either Keys or Values can be nil - in this case the corresponding information
      will not be gathered. }

    procedure GetKeysAndValues(Keys, Values: TList); virtual; abstract;
  end;

  { B-tree is a balanced search tree which can insert, delete and find a key
    for O(log n) time. Its keys are always sorted, so it can return all key value
    pairs sorted by key.

    The order of a b-tree is the maximum number of subnodes each node can have. }

  TBTreeNode = class;

  TBTree = class(TOrderedMap)
  private
    FOrder: Integer;
    FRoot: TBTreeNode;

    function  CreateNode(LNode: TBTreeNode = nil): TBTreeNode;
    function  IsFull(Node: TBTreeNode): Boolean;
    procedure InsertKey(Node: TBTreeNode; i: Integer; Key, Value: Pointer; LNode, RNode: TBTreeNode);
    procedure SplitTree(Node: TBTreeNode; i: Integer);
    function  FindIndex(Node: TBTreeNode; Key: Pointer): Integer;
    function  FindKey(Key: Pointer; out KeyIndex: Integer): TBTreeNode;
    function  GetHeight: Integer;
    function  GetKeysCount: Integer;
  protected
    { Returns True if the key was inserted. Otherwise, the key was already there. }
    function  Insert(Key: Pointer; Value: Pointer): Boolean;
    function  ItemExists(Key: Pointer): Boolean; override;
    function  GetItem(Key: Pointer): Pointer; override;
    procedure SetItem(Key: Pointer; Value: Pointer); override;
    procedure SetItemExistence(Key: Pointer; b: Boolean); override;

    property Root: TBTreeNode read FRoot;
    property Height: Integer read GetHeight;
    property KeysCount: Integer read GetKeysCount;
    property Order: Integer read FOrder;
  public
    constructor Create(Order: Integer = 4);
    destructor Destroy; override;

    procedure GetKeysAndValues(Keys, Values: TList); override;
  end;

  TBTreeNode = class
  private
    FLen: Integer;
    FKeys: array of Pointer;
    FRefs: array of TBTreeNode;
    FVals: array of Pointer;

    procedure SetLen(n: Integer);

    function GetKey(i: Integer): Pointer;
    function GetRef(i: Integer): TBTreeNode;
    function GetVal(i: Integer): Pointer;
    procedure SetKey(i: Integer; k: Pointer);
    procedure SetRef(i: Integer; r: TBTreeNode);
    procedure SetVal(i: Integer; v: Pointer);
  public
    constructor Create(MaxLen: Integer);

    property Len: Integer read FLen write SetLen;

    property Keys[i: Integer]: Pointer read GetKey write SetKey;
    property Refs[i: Integer]: TBTreeNode read GetRef write SetRef;
    property Vals[i: Integer]: Pointer read GetVal write SetVal;
  end;

  { AVL tree is a balanced search tree which can insert, delete and find a key
    for O(log n) time. Its keys are always sorted, so it can return all key value
    pairs sorted by key.

    AVL tree inserts new keys slower than B-tree, but performs lookup faster. }

  TAVLTreeNode = class;

  TAVLTree = class(TMap)
  private
    FRoot: TAVLTreeNode;

    function  FindKey(Key: Pointer; out Node: TAVLTreeNode): Boolean;
    procedure Insert(Parent, Node: TAVLTreeNode; Key, Value: Pointer);
    procedure LiftChild(R, N: TAVLTreeNode; Leaf: Boolean);
  protected
    procedure SetItemExistence(Key: Pointer; b: Boolean); override;
    function  ItemExists(Key: Pointer): Boolean; override;
    function  GetItem(Key: Pointer): Pointer; override;
    procedure SetItem(Key: Pointer; Value: Pointer); override;

    property Root: TAVLTreeNode read FRoot;
  public
    destructor Destroy; override;
  end;

  TAVLTreeNode = class
  private
    FKey, FValue: Pointer;
    FHeight: Byte;
    FLeafs: array[Boolean] of TAVLTreeNode;

    function  GetChild(Leaf: Boolean): TAVLTreeNode;
    procedure SetChild(Leaf: Boolean; Node: TAVLTreeNode);
    function  GetHeight: Byte;
  public
    constructor Create(Key, Value: Pointer);
    destructor Destroy; override;

    procedure UpdateHeight;

    property Key: Pointer read FKey;
    property Value: Pointer read FValue write FValue;
    property Height: Byte read GetHeight write FHeight;
    property Child[Leaf: Boolean]: TAVLTreeNode read GetChild write SetChild; default;
  end;

  { Hash table

    This is a base hash table class }

  THashTable = class
  protected

    procedure SetValueInternal(Key:Integer; Value: Pointer);

  public

    { Returns a value by a specified key.
      Returns zero if the value doesn't exist. }

    function GetValue(Key: Integer): Pointer; virtual; abstract;

    { Sets a value corresponding to a specified key.
      If the value for this key already exists, the routine
      doesn't change the previous value and returns it.
      If the value doesn't exist, the routine changes the value for
      the key and returns zero. }

    function SetValue(Key: Integer; Value: Pointer): Pointer; virtual; abstract;

    { Performs a self test.
      Arguments:

        • RS  - randomly choosen value
        • n   - a count of iterations }

    function SelfTest(RS, n: Integer): Boolean;

    property Items[Key: Integer]: Pointer read GetValue write SetValueInternal; default;

  end;

  { Hash table

    This is a "classic" implementation of a hash table. }

  TLhtEntry = packed record

    Next: Pointer;
    Key:  Integer;
    Data: Pointer;

  end;

  PLhtEntry = ^TLhtEntry;

  TListHashTable = class(THashTable)
  private

    FBushes: Pointer; // List of pointers to TLhtEntry
    FHeap: TfcxHeap;

    function CreateEntry(Key: Integer): PLhtEntry;

  public

    { Returns a value by a specified key.
      Returns zero if the value doesn't exist. }

    function GetValue(Key: Integer): Pointer; override;

    { Sets a value corresponding to a specified key. }

    function SetValue(Key: Integer; Value: Pointer): Pointer; override;

    constructor Create;
    destructor Destroy; override;

  end;

  { Cache.

    This class provides caching features for storing
    a big list of objects. The cache provides a transparent
    access to objects by indexes. There're two requirements
    that objects must satisfy if they are kept in the cache:

      • Any object can save itself to a stream and can load
        itself from a stream

      • The cache must not contain two identical references
        at a time }

  TLcWriteObj = procedure(Stream: TStream; Obj: TObject);
  TLcReadObj = function(Stream: TStream): TObject;

  TLcObject = record

    Offset: Int64;
    Size:   Int64;

  end;

  TLcCreateParams = record

    CacheStream:    TStream;
    ObjectsStream:  TStream;
    QueueSize:      Integer;

  end;

  TLcLoadedObject = record

    Reference: TObject;
    Index: Integer;

  end;

  TListCache = class
  private

    { TListCache can work as a usual list without
      caching. In this mode the objects are stored
      in this list. }

    FObjects: TObjList;

    { This stream contains a sequence of serialized objects.
      Normally, this stream is a disk file. }

    FCacheStream: TStream;

    { This stream contains TLcObject structures.
      The i-th structure describes the object at the i-th
      index.

        • TLcObject.Offset

          The index to the first byte in FCacheStream where
          the object is stored.

        • TLcObject.Size

          The number of bytes occupied in FCacheStream by the
          stored object. }

    FObjectsStream: TStream;

    { This list contains indexes to recently loaded objects.
      The first item of this list refers to the most recent object.
      Access to this queue must be performed via PushObject. }

    FCacheQueue: array of TLcLoadedObject;

    { If enabled, objects that are unloaded will be written to the
      cache stream even if they were already written to the stream
      in past. }

    FUpdate: Boolean;

    { Protects objects from overwriting }

    FProtect: Boolean;

    { If False, unloaded objects are written to the end of the
      cache stream. This greatly increases size of the stream,
      but this has to be done for objects with dynamic size. }

    FStaticSize: Boolean;

    { If these names of files are specified, then the destructor
      must close the two streams and delete the files. }

    FCacheFile: string;
    FObjFile:   string;

  protected

    function IsCaching: Boolean;

    { Calls destructor for all objects in the queue and
      then makes the queue empty. }

    procedure ClearQueue;

    { Access to list of objects must be peformed via
      two routines:

        • GetObject( Index )

          Finds a specified object. If the object is presented
          in physical memory, its returned. If the object is
          not in physical memory its loaded from a cache stream
          and returned.

        • SetObject( Index, Object )

          Writes a specified object to a specified location.
          Maybe, the routine moves some objects from physical memory
          to a cache stream. }

    function GetObject(Index: Integer): TObject;
    procedure SetObject(Index: Integer; Obj: TObject);

    { Adds a specified object to the head of the queue. If the queue
      is full, then an object in the tail of the queue is unloaded
      to the cache stream. }

    procedure PushObject(Obj: TObject; Index: Integer);

    procedure UnloadObject(Obj: TObject; Index: Integer);

    function GetObjectsCount: Integer;
    procedure SetObjectsCount(NewCount: Integer);

    { This routine is called by constructors for initialization }

    procedure Initialize(Params: TLcCreateParams);

  public

    WriteObj: TLcWriteObj;
    ReadObj: TLcReadObj;

    { This constructor creates TListCache that works
      as a usual TObjList without caching. }

    constructor Create; overload;

    { Creates a cache.
      Arguments:

        • Params.CacheStream

          A stream that will contain temporarily
          unloaded objects.

        • Params.QueueSize

          A number of objects that can be presented in
          physical memory at a time.

        • Params.ObjectsStream

          This stream will contain a list of TLcObject structures.
          The number of these structures equals the number of objects. }

    constructor Create(Params: TLcCreateParams); overload;

    { This constructor creates two files with the specified names
      and uses them as CacheStream and ObjectsStream. When the destructor
      is called, these files are closed and deleted. }

    constructor Create(CacheFile, ObjFile: string; QueueSize: Integer = 4); overload;

    destructor Destroy; override;

    procedure Clear;
    procedure Exchange(Index1, Index2: Integer);
    function First: TObject;
    function Last: TObject;
    function Add(Obj: TObject): Integer;

    { Access to objects by indexes.

      When a caller attempts to write an object at a non existing
      index, the internal list is extended and the object is correctly
      written.

      When a caller attempts to overwrite an existing object,
      and exception is raised. }

    property Objects[Index: Integer]: TObject read GetObject write SetObject; default;

    { Returns a number of references to objects.
      Some references can be zero. }

    property Count: Integer read GetObjectsCount write SetObjectsCount;

    { The cache keeps only a few objects in memory at a time.
      When a caller attempts to load an object that is not in memory,
      it's loaded into memory. After that the cache may have to
      unload another object back to a cache stream. The object that is
      going to be unloaded can differ from its copy in the cache stream.
      In this case the object must be written to the cache stream again.
      This property controls the behaviour of the cache in this situation:

        • Option enabled

          The object that's goigng to be unloaded will be written to
          the cache stream, even if it already has a copy in the cache stream.
          This option must be enabled if the object has been changed
          since a time when it was written to the cache stream.

        • Option disabled

          The object that's going to be unloaded will be removed from
          memory and will not be written to the cache stream. This option
          must be disabled if the object has not been changed since the last
          time when it was written to the cache stream. }

    property UpdateWhenUnload: Boolean read FUpdate write FUpdate; {default True}

    { When a caller attempts to put an object to a specified location,
      like as follows:

        Objects[777] := Obj

      it can turn out that the specified location is already occupied by another
      object. When this option is enabled, the cache will raise an exception
      for these cases. }

    property Protect: Boolean read FProtect write FProtect; {default False}

    { If WriteObj routine can write different number of bytes for the
      same object, then this property must be set to False. }

    property StaticSize: Boolean read FStaticSize write FStaticSize; {default False}

  end;

  { Data block }

  TDataBlock = class
  public

    Data: Pointer;
    Size: Integer;

    constructor Create(Size: Integer; Zero: Boolean = True);
    destructor Destroy; override;

  end;

  { This function returns a negative integer when x < y and
    returns a non-negative integer in other cases. }

  TSimpleCompareFunction = function(const x, y): Integer;

  { Array interface }

  TArray = class
  protected

    function GetZero: Boolean; virtual; abstract;
    procedure SetZero(f: Boolean); virtual; abstract;

    function GetItemSize: Integer; virtual; abstract;

    function GetCount: Integer; virtual; abstract;
    procedure SetCount(Value: Integer); virtual; abstract;

    function GetItemData(Index: Integer): Pointer; virtual; abstract;

    procedure GetItem(Index: Integer; var Item);
    procedure SetItem(Index: Integer; const Item);

    procedure Insert(Index: Integer; const Value);
    procedure Append(const Value);

    procedure VerifyIndex(Index: Integer);
    procedure ResetItems(Min, Max: Integer);

    property ItemData[Index: Integer]: Pointer read GetItemData;
    property ItemSize: Integer read GetItemSize;

  public

    procedure Clear;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Sort(Compare: TSimpleCompareFunction; Min, Max: Integer); virtual;

    property Count: Integer read GetCount write SetCount;

    { If set, all newly created items are filled with zeros.
      If not set, bits of all new blocks are set to 1. }

    property Zero: Boolean read GetZero write SetZero;

  end;

  { Cached array of equal sized structures }

  TCachedArray = class(TArray)
  private

    FCount: Integer;  // number of items
    FBlock: Integer;  // number of items in a block
    FStorage: TListCache;
    FItemSize: Integer;
    FZero: Boolean;

    function CreateBlock: TDataBlock;
    procedure Initialize;

  protected

    function GetZero: Boolean; override;
    procedure SetZero(f: Boolean); override;
    function GetItemSize: Integer; override;
    function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    function GetItemData(Index: Integer): Pointer; override;

  public

    { TCachedArray is based on TListCache, so its constructor
      has similar arguments.

        • DataStream

          This stream is used as a temporary storage for array elements.
          The size of the stream is quite equal to the size of the array.
          This stream is created and deleted by a client, this class only uses it
          and doesn't delete.

        • ServStream

          This stream is used for some service information required by TListCache.
          The size of this stream is rather small.
          It's created and deleted by a client, this class doesn't delete it.

        • DataFile

          This is the name of a file that will be created and used as DataStream.
          The destructor of this class deletes the created file.

        • ServFile

          This is the name of a file that will be created and used as ServStream.
          The destructor of this class deletes the created file.

        • Block

          All the elements of the array will be grouped into blocks of this size.
          Any block can either be in physical memory or in the DataStream.
          This value means the number of bytes in each block. }

    constructor Create(DataStream, ServStream: TStream; Block, ItemSize: Integer); overload;
    constructor Create(DataFile, ServFile: string; Block, ItemSize: Integer); overload;

    destructor Destroy; override;

    procedure Sort(Compare: TSimpleCompareFunction; Min, Max: Integer); override;

  end;

  { Abstract array of Integers }

  TIntArrayBase = class(TArray)
  protected

    function GetItem(Index: Integer): Integer; virtual; abstract;
    procedure SetItem(Index: Integer; Value: Integer); virtual; abstract;

    function GetItemSize: Integer; override;

  public

    procedure Insert(Index: Integer; Value: Integer);
    procedure Append(Value: Integer);

    property Items[Index: Integer]: Integer read GetItem write SetItem; default;

  end;

  { Array of Integers }

  TIntArray = class(TIntArrayBase)
  private

    FArray: array of Integer;
    FZero: Boolean;

  protected

    function GetZero: Boolean; override;
    procedure SetZero(f: Boolean); override;

    function GetItem(Index: Integer): Integer; override;
    procedure SetItem(Index: Integer; Value: Integer); override;

    function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    function GetItemData(Index: Integer): Pointer; override;

  end;

  { Cached array of Integers }

  TCachedIntArray = class(TIntArrayBase)
  private

    FArray: TCachedArray;

  protected

    function GetZero: Boolean; override;
    procedure SetZero(f: Boolean); override;

    function GetItem(Index: Integer): Integer; override;
    procedure SetItem(Index: Integer; Value: Integer); override;

    function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    function GetItemData(Index: Integer): Pointer; override;

  public

    constructor Create(DataStream, ServStream: TStream; Block: Integer); overload;
    constructor Create(DataFile, ServFile: string; Block: Integer); overload;

    destructor Destroy; override;

  end;

  { Abstract array of Integers }

  TExtArrayBase = class(TArray)
  protected

    function GetItem(Index: Integer): Extended; virtual; abstract;
    procedure SetItem(Index: Integer; Value: Extended); virtual; abstract;

    function GetItemSize: Integer; override;

  public

    procedure Insert(Index: Integer; Value: Extended);
    procedure Append(Value: Extended);

    property Items[Index: Integer]: Extended read GetItem write SetItem; default;

  end;

  { Array of Integers }

  TExtArray = class(TExtArrayBase)
  private

    FArray: array of Extended;
    FZero: Boolean;

  protected

    function GetZero: Boolean; override;
    procedure SetZero(f: Boolean); override;

    function GetItem(Index: Integer): Extended; override;
    procedure SetItem(Index: Integer; Value: Extended); override;

    function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    function GetItemData(Index: Integer): Pointer; override;

  end;

  { Cached array of Integers }

  TCachedExtArray = class(TExtArrayBase)
  private

    FArray: TCachedArray;

  protected

    function GetZero: Boolean; override;
    procedure SetZero(f: Boolean); override;

    function GetItem(Index: Integer): Extended; override;
    procedure SetItem(Index: Integer; Value: Extended); override;

    function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    function GetItemData(Index: Integer): Pointer; override;

  public

    constructor Create(DataStream, ServStream: TStream; Block: Integer); overload;
    constructor Create(DataFile, ServFile: string; Block: Integer); overload;

    destructor Destroy; override;

  end;

  { This class writes and reads various data types
    to an arbitrary stream }

  TStreamRW = class
  private

    FStream: TStream;
    FSecure: Boolean;

    function GetBoolType: Integer;
    function GetIntType: Integer;
    function GetExtType: Integer;
    function GetStrType: Integer;

    procedure WriteType(t: Integer);
    procedure ReadType(t: Integer);

  public

    { If Secure = True then any value will be written right after
      a 4-byte code that describe its type. When a value is to be read,
      the leading 4-byte code is read and compared with the type of
      the value and if the two 4-byte codes don't match, an
      exception is raised. }

    constructor Create(Stream: TStream; Secure: Boolean = False);

    procedure WriteBool(x: Boolean);
    procedure WriteInt(x: Integer);
    procedure WriteExt(x: Extended);
    procedure WriteStr(x: WideString);

    function ReadBool: Boolean;
    function ReadInt: Integer;
    function ReadExt: Extended;
    function ReadStr: WideString;

  end;

  { Array of bits }

  TBitArray = class
  private

    FLength: Integer;
    FData: array of Byte;

    function GetBit(Index: Integer): Boolean;
    procedure SetBit(Index: Integer; Value: Boolean);
    procedure SetBitsLength(NewLength: Integer);
    procedure Trunc;

  public

    function Clone: TBitArray;

    procedure ResetBits;  // make all bits equal 0
    procedure SetBits;    // make all bits equal 1

    procedure SaveToStream(Stream: TStream);

    function GetNumOfSetBits: Integer;
    function GetRightmostBitIndex: Integer;

    procedure BitOr(Src: TBitArray);  // Self := Self or Src
    procedure BitAnd(Src: TBitArray); // Self := Self and Src
    procedure BitXor(Src: TBitArray); // Self := Self xor Src

    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Length: Integer read FLength write SetBitsLength;

  end;

{ Finds an entry with a specified key.
  If there's no such an entry, the routine
  returns the last entry in the list. }

function LhtFindEntry(e: PLhtEntry; Key: Integer): PLhtEntry; cdecl;

{ Get/set an entry by its index. }

function LhtGetEntry(Base: Pointer; Index: Integer): PLhtEntry; cdecl;
procedure LhtSetEntry(Base: Pointer; Index: Integer; Entry: PLhtEntry); cdecl;

{ Serializing and deserializing routines for TDataBlock }

function DbRead(Stream: TStream): TObject;
procedure DbWrite(Stream: TStream; Block: TObject);

implementation

{$IFNDEF FMX}
uses
  Math;
{$ELSE}
uses
  System.Math;
{$ENDIF}

var
  NumSetBits: array[0..255] of Byte; // NumSetBits[b] = the number of "1" bits in b

procedure InitNumSetBits;

  function GetNSB(b: Byte): Byte;
  begin
    Result := 0;

    while b > 0 do
    begin
      Inc(Result);
      b := b and (b - 1);
    end;
  end;

var
  b: Byte;
begin
  for b := 0 to 255 do
    NumSetBits[b] := GetNSB(b)
end;

procedure LhtSetEntry(Base: Pointer; Index: Integer; Entry: PLhtEntry);
begin
{$IFDEF WIN64}
  Move(Entry, Pointer(Int64(Base) + Index*SizeOf(PLhtEntry))^, SizeOf(PLhtEntry));
{$ELSE}
  Move(Entry, Pointer(Integer(Base) + Index*SizeOf(PLhtEntry))^, SizeOf(PLhtEntry));
{$ENDIF}
end;

function LhtGetEntry(Base: Pointer; Index: Integer): PLhtEntry;
begin
{$IFDEF WIN64}
  Move(Pointer(Int64(Base) + Index*SizeOf(PLhtEntry))^, Result, SizeOf(PLhtEntry));
{$ELSE}
  Move(Pointer(Integer(Base) + Index*SizeOf(PLhtEntry))^, Result, SizeOf(PLhtEntry));
{$ENDIF}
end;

function LhtFindEntry(e: PLhtEntry; Key: Integer): PLhtEntry;
begin
  while (e <> nil) and (e^.Key <> Key) do
    e := e^.Next;

  Result := e;
end;

procedure CheckBounds(i, Min, Max: LongInt);
begin
  if (i < Min) or (i > Max) then
    raise Exception.CreateFmt('%d is out of bounds [%d, %d]', [i, Min, Max])
end;

{ TObjList }

procedure TObjList.Clear;
var
  i: LongInt;
begin
  for i := 0 to Count - 1 do
    TObject(Items[i]).Free;

  inherited;
end;

{ TBlockStream }

constructor TBlockStream.Create(BlockSizeShift: LongInt);
begin
  inherited Create;
  CheckBounds(BlockSizeShift, 1, 12);
  FBlockShift := BlockSizeShift;
end;

function TBlockStream.GetCurrentBlock: LongInt;
begin
  Result := Size shr FBlockShift;
end;

procedure TBlockStream.EndBlock(Value: Byte);
var
  n, m: LongInt;
begin
  n := 1 shl FBlockShift;
  m := Size and (n - 1);
  if m > 0 then
    Fill(Value, n - m);
end;

procedure TBlockStream.Fill(Value: Byte; Count: LongInt);
begin
  CheckBounds(Count, 1, MaxLongint);

  { todo: an efficient implementation
    can be done using a buffer in the thread stack. }

  while Count > 0 do
  begin
    Write(Value, 1);
    Dec(Count);
  end;
end;

function TBlockStream.GetBlocksCount: LongInt;
begin
  Result := Size shr FBlockShift;
  if Size and (1 shl FBlockShift - 1) <> 0 then
    Inc(Result);
end;

function TBlockStream.GetBlockData(i: LongInt): Pointer;
begin
  CheckBounds(i, 0, BlocksCount - 1);
  Result := Pointer(LongInt(Memory) + i shl FBlockShift);
end;

function TBlockStream.GetBlockSize(i: LongInt): LongInt;
begin
  CheckBounds(i, 0, BlocksCount - 1);

  Result := Size - i shl FBlockShift;
  if Result < 1 shl FBlockShift then
    Exit;

  Result := 1 shl FBlockShift;
end;

function TBlockStream.Imm(Value, Count: LongInt): Pointer;
begin
  CheckBounds(Count, 1, SizeOf(Value));
  Write(Value, Count);
  Result := Pointer(LongInt(Memory) + Size - Count);
end;

{ THashTable }

function THashTable.SelfTest(RS, n: Integer): Boolean;
var
  i: Integer;
  x: Pointer;
begin
  RandSeed := RS;
  for i := 1 to n do
  begin
    x := Pointer(Random($7fffffff));
    Items[Integer(x)] := x;

    if Items[Integer(x)] <> x then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure THashTable.SetValueInternal(Key:Integer; Value: Pointer);
begin
  SetValue(Key, Value);
end;

{ TListHashTable }

constructor TListHashTable.Create;
begin
  FBushes := AllocMem($10000 * SizeOf(Pointer));
  FHeap := TfcxHeap.Create(SizeOf(TLhtEntry));

  if FBushes = nil then
    raise Exception.Create('Failed to allocate memory');
end;

destructor TListHashTable.Destroy;
begin
  FreeMem(FBushes);
  FHeap.Free;
end;

function TListHashTable.CreateEntry(Key: Integer): PLhtEntry;
begin
  Result := FHeap.AllocMem(SizeOf(TLhtEntry));
  Result^.Key := Key;
  Result^.Next := nil;
end;

function TListHashTable.GetValue(Key: Integer): Pointer;
var
  e: PLhtEntry;
begin
  e := LhtGetEntry(FBushes, Key and $ffff);
  e := LhtFindEntry(e, Key);

  if (e = nil) or (e^.Key <> Key) then
    Result := nil
  else
    Result := e^.Data;
end;

function TListHashTable.SetValue(Key: Integer; Value: Pointer): Pointer;
var
  e, e2: PLhtEntry;
begin
  e := LhtGetEntry(FBushes, Key and $ffff);
  e := LhtFindEntry(e, Key);

  if (e <> nil) and (e^.Key = Key) then
  begin
    Result := e^.Data;
    Exit;
  end;

  e2 := CreateEntry(Key);
  e2^.Data := Pointer(Value);

  if e = nil then
    LhtSetEntry(FBushes, Key and $ffff, e2)
  else
    e^.Next := e2;

  Result := nil;
end;

{ TListCache }

constructor TListCache.Create;
begin
  FObjects := TObjList.Create;
end;

constructor TListCache.Create(Params: TLcCreateParams);
begin
  Initialize(Params);
end;

constructor TListCache.Create(CacheFile, ObjFile: string; QueueSize: Integer);
var
  p: TLcCreateParams;
begin
  FCacheFile := CacheFile;
  FObjFile := ObjFile;

  p.CacheStream := TFileStream.Create(CacheFile, fmCreate);
  p.ObjectsStream := TFileStream.Create(ObjFile, fmCreate);
  p.QueueSize := QueueSize;

  Initialize(p);
end;

destructor TListCache.Destroy;
begin
  if FObjFile <> '' then
  begin
    FObjectsStream.Free;
    DeleteFile(FObjFile);
  end;

  if FCacheFile <> '' then
  begin
    FCacheStream.Free;
    DeleteFile(FCacheFile);
  end;

  FObjects.Free;
  ClearQueue;
end;

procedure TListCache.Initialize(Params: TLcCreateParams);
var
  i: Integer;
begin
  if Params.CacheStream = Params.ObjectsStream then
    raise Exception.Create('Caching stream and objects stream must be different streams');

  FUpdate := True;
  FCacheStream := Params.CacheStream;
  FObjectsStream := Params.ObjectsStream;
  SetLength(FCacheQueue, Params.QueueSize);

  for i := 0 to Length(FCacheQueue) - 1 do
    FCacheQueue[i].Reference := nil;
end;

procedure TListCache.Clear;
begin
  if not IsCaching then
  begin
    FObjects.Clear;
    Exit;
  end;

  FCacheStream.Size := 0;
  FObjectsStream.Size := 0;

  ClearQueue;
end;

procedure TListCache.ClearQueue;
var
  i: Integer;
begin
  for i := 0 to Length(FCacheQueue) - 1 do
  with FCacheQueue[i] do
  begin
    Reference.Free;
    Reference := nil;
  end;
end;

procedure TListCache.Exchange(Index1, Index2: Integer);
var
  i, j: Integer;
  r1, r2: TLcObject;
begin
  if not IsCaching then
  begin
    FObjects.Exchange(Index1, Index2);
    Exit;
  end;

  with FObjectsStream do
  begin
    Seek(Index1 * SizeOf(TLcObject), soFromBeginning);
    Read(r1, SizeOf(TLcObject));

    Seek(Index2 * SizeOf(TLcObject), soFromBeginning);
    Read(r2, SizeOf(TLcObject));

    Seek(Index2 * SizeOf(TLcObject), soFromBeginning);
    Write(r1, SizeOf(TLcObject));

    Seek(Index1 * SizeOf(TLcObject), soFromBeginning);
    Write(r2, SizeOf(TLcObject));
  end;

  for i := 0 to Length(FCacheQueue) - 1 do
  begin
    j := FCacheQueue[i].Index;

    if j = Index1 then
      FCacheQueue[i].Index := Index2;

    if j = Index2 then
      FCacheQueue[i].Index := Index1;
  end;
end;

function TListCache.First: TObject;
begin
  Result := Objects[0];
end;

function TListCache.Last: TObject;
begin
  Result := Objects[Count - 1];
end;

function TListCache.Add(Obj: TObject): Integer;
begin
  Result := Count;
  Objects[Count] := Obj;
end;

function TListCache.IsCaching: Boolean;
begin
  Result := FCacheStream <> nil;
end;

function TListCache.GetObjectsCount: Integer;
begin
  if IsCaching then
    Result := FObjectsStream.Size div SizeOf(TLcObject)
  else
    Result := FObjects.Count;
end;

procedure TListCache.SetObjectsCount(NewCount: Integer);
var
  n, i: Integer;
begin
  n := Count;

  { The case when the new size is greater than the current }

  if NewCount > n then
  begin
    SetObject(NewCount - 1, nil);
    Exit;
  end;

  { If the new size is smaller, remove the tail items from the queue }

  for i := 0 to Length(FCacheQueue) - 1 do
    with FCacheQueue[i] do
      if Index >= NewCount then
      begin
        Reference.Free;
        Reference := nil;
      end;

  { Remove descriptions of the tail items }

  FObjectsStream.Size := NewCount * SizeOf(TLcObject);
end;

function TListCache.GetObject(Index: Integer): TObject;
var
  i: Integer;
  r: TLcLoadedObject;
  x: TLcObject;
begin
  if (Index < 0) or (Index >= Count) then
  begin
    Result := nil;
    Exit;
  end;

  if not IsCaching then
  begin
    Result := FObjects[Index];
    Exit;
  end;

  { Check whether the object is in memory }

  for i := 0 to Length(FCacheQueue) - 1 do
  begin
    r := FCacheQueue[i];

    if (r.Index = Index) and (r.Reference <> nil) then
    begin
      Result := r.Reference;
      Exit;
    end;
  end;

  { If the required object is not in memory,
    load it from the cache stream. }

  FObjectsStream.Seek(Index * SizeOf(TLcObject), soFromBeginning);
  FObjectsStream.Read(x, SizeOf(TLcObject));

  { Some objects stored by a client via SetObject can
    be nil. For these objects zero TLcObject entry corresponds. }

  if x.Size = 0 then
  begin
    Result := nil;
    Exit;
  end;

  { If the object is not nil, load it. }

  FCacheStream.Seek(x.Offset, soFromBeginning);
  Result := ReadObj(FCacheStream);

  { This exception occurs when an object was stored to the
    cache stream some time before and now it's been read, but
    the reading routine reads another number of bytes that
    the writing routine wrote.

    This exception indicates that the read object
    is possibly garbage. }

  if FCacheStream.Position <> x.Offset + x.Size then
    raise Exception.Create('The read object does not match the written object');

  { Put the loaded object to the queue of objects which
    are located in the physical memory. }

  PushObject(Result, Index);
end;

procedure TListCache.SetObject(Index: Integer; Obj: TObject);
var
  i: Integer;
  x: TLcObject;
begin   
  if Index < 0 then
    raise Exception.Create('Cannot store an object an a negative index');

  if not IsCaching then
  begin
    if Index >= Count then
      FObjects.Count := Index + 1;

    FObjects[Index] := Obj;
    Exit;
  end;

  { Prevent overwriting of objects }

  if FProtect and (Index < Count) then
  begin
    FObjectsStream.Seek(Index * SizeOf(TLcObject), soFromBeginning);
    FObjectsStream.Read(x, SizeOf(TLcObject));

    if x.Size > 0 then
      raise Exception.Create('Attempt to overwrite an existing object');
  end;

  { If the index is out of bounds, expand the storage of objects }

  if Index >= Count then
  begin
    i := Index - Count + 1;
    FillChar(x, SizeOf(x), 0);
    FObjectsStream.Seek(0, soFromEnd);

    while i > 0 do
    begin
      FObjectsStream.Write(x, SizeOf(x));
      Dec(i);
    end;
  end;

  { Notify the system that the object is in memory }

  if Obj <> nil then
    PushObject(Obj, Index);
end;

procedure TListCache.UnloadObject(Obj: TObject; Index: Integer);
var
  x: TLcObject;
begin
  FObjectsStream.Seek(Index * SizeOf(x), soFromBeginning);
  FObjectsStream.Read(x, SizeOf(x));

  { If an object has not been cached yet or
    size of objects can change dynamically,
    it must be cached now or again }

  if not FStaticSize or (x.Size = 0) then
  begin
    x.Offset := FCacheStream.Seek(0, soFromEnd);
    WriteObj(FCacheStream, Obj);
    x.Size := FCacheStream.Position - x.Offset;
    FObjectsStream.Seek(Index * SizeOf(x), soFromBeginning);
    FObjectsStream.Write(x, SizeOf(x));
  end

  { If an object has been cached, it either can be
    cached again or just discarded }

  else if FUpdate then
  begin
    FCacheStream.Seek(x.Offset, soFromBeginning);
    WriteObj(FCacheStream, Obj);

    { This exception occurs when the writing routine attempts
      to write data over bounds that were assigned with the
      object before. The writing routine must write the same
      number of bytes for the same object every time.

      This exception may lead to corruption of consequent objects. }

    if x.Offset + x.Size <> FCacheStream.Position then
      raise Exception.Create('The written object corrupted the consequent object');
  end;

  { Remove the object from memory }

  Obj.Free;
end;

procedure TListCache.PushObject(Obj: TObject; Index: Integer);
var
  i: Integer;
  Tail: TLcLoadedObject;
begin

  { If the queue is full, serialize tail
    objects and remove them from memory }

  Tail := FCacheQueue[Length(FCacheQueue) - 1];
  if Tail.Reference <> nil then
    UnloadObject(Tail.Reference, Tail.Index);

  { Remove the tail from the queue and
    insert the new object index to the head
    of the queue }

  for i := Length(FCacheQueue) - 1 downto 1 do
    FCacheQueue[i] := FCacheQueue[i - 1];

  FCacheQueue[0].Reference := Obj;
  FCacheQueue[0].Index := Index;
end;

{ TArray }

procedure TArray.Append(const Value);
begin
  Count := Count + 1;
  SetItem(Count - 1, Value)
end;

procedure TArray.Clear;
begin
  Count := 0
end;

procedure TArray.Delete(Index: Integer);
var
  i: Integer;
begin
  VerifyIndex(Index);

  for i := Index to Count - 2 do
    Move(ItemData[i + 1]^, ItemData[i]^, ItemSize);

  Count := Count - 1;
end;

procedure TArray.Exchange(Index1, Index2: Integer);
var
  p0, p1, p2: Pointer;
begin
  p1 := ItemData[Index1];
  p2 := ItemData[Index2];

  GetMem(p0, ItemSize);

  try
    Move(p1^, p0^, ItemSize);
    Move(p2^, p1^, ItemSize);
    Move(p0^, p2^, ItemSize);
  finally
    FreeMem(p0, ItemSize)
  end;
end;

procedure TArray.Insert(Index: Integer; const Value);
var
  i: Integer;
begin
  VerifyIndex(Index);
  Count := Count + 1;

  for i := Count - 2 downto Index do
    Move(ItemData[i]^, ItemData[i + 1]^, ItemSize);

  Move(Value, ItemData[Index]^, ItemSize);
end;

procedure TArray.GetItem(Index: Integer; var Item);
begin
  VerifyIndex(Index);
  Move(GetItemData(Index)^, Item, ItemSize);
end;

procedure TArray.SetItem(Index: Integer; const Item);
begin
  VerifyIndex(Index);
  Move(Item, GetItemData(Index)^, ItemSize);
end;

procedure TArray.Sort(Compare: TSimpleCompareFunction; min, max: Integer);
var
  x: array of Byte;
  i, j: Integer;
begin
  if min >= max then
    Exit;

  System.SetLength(x, ItemSize);
  GetItem((min + max) div 2, x[0]);

  i := min;
  j := max;

  while i <= j do
  begin
    while Compare(GetItemData(i)^, x[0]) < 0 do
      Inc(i);

    while Compare(x[0], GetItemData(j)^) < 0 do
      Dec(j);

    if i <= j then
    begin
      Exchange(i, j);
      Inc(i);
      Dec(j);
    end;
  end;

  if min < j then
    Sort(Compare, min, j);

  if i < max then
    Sort(Compare, i, max);
end;

procedure TArray.VerifyIndex(Index: Integer);
begin
  Assert((Index >= 0) and (Index < Count), 'Index out of bounds')
end;

procedure TArray.ResetItems(Min, Max: Integer);
var
  i: Integer;
begin
  if Zero then
    for i := Min to Max do
      FillChar(ItemData[i]^, ItemSize, 0)
  else
    for i := Min to Max do
      FillChar(ItemData[i]^, ItemSize, $ff)
end;

{ TCachedArray }

constructor TCachedArray.Create(DataStream, ServStream: TStream; Block, ItemSize: Integer);
var
  p: TLcCreateParams;
begin
  FBlock := Block;
  FItemSize := ItemSize;

  p.CacheStream := DataStream;
  p.ObjectsStream := ServStream;
  p.QueueSize := 8;

  FStorage := TListCache.Create(p);

  Initialize;
end;

constructor TCachedArray.Create(DataFile, ServFile: string; Block, ItemSize: Integer);
begin
  FBlock := Block;
  FItemSize := ItemSize;

  FStorage := TListCache.Create(DataFile, ServFile, 8);

  Initialize;
end;

destructor TCachedArray.Destroy;
begin
  FStorage.Free
end;

procedure TCachedArray.Initialize;
begin
  FStorage.ReadObj := DbRead;
  FStorage.WriteObj := DbWrite;
  FStorage.StaticSize := True;

  FZero := True;
end;

procedure TCachedArray.Sort(Compare: TSimpleCompareFunction; Min, Max: Integer);

  procedure GetBlockBounds(Index: Integer; var min, max: Integer);
  begin
    min := Index * FBlock;
    max := min + FBlock - 1;

    if max >= Count then
      max := Count - 1;
  end;

  procedure SortBlock(Index: Integer);
  var
    min, max, i, j: Integer;
    x, y: Pointer;
  begin
    GetBlockBounds(Index, min, max);

    for i := min to max do
      for j := i + 1 to max do
      begin
        x := GetItemData(i);
        y := GetItemData(j);

        if Compare(x^, y^) >= 0 then
          Exchange(i, j);
      end;
  end;

  function BinSearch(const x; min, max: Integer): Integer;
  var
    i: Integer;
  begin
    if min = max then
    begin
      Result := min;
      Exit;
    end;

    i := (min + max) div 2;

    if Compare(x, GetItemData(i)^) < 0 then
      Result := BinSearch(x, min, i)
    else if i <> min then
      Result := BinSearch(x, i, max)
    else
      Result := i;
  end;

  procedure MergeBlock(Index: Integer);
  var
    i, j, min, max: Integer;
    x, y: array of Byte;
  begin
    GetBlockBounds(Index, min, max);
    System.SetLength(x, ItemSize);
    System.SetLength(y, ItemSize);
    GetItem(min, y[0]);

    for i := BinSearch(y[0], 0, min - 1) to min - 1 do
    begin
      GetItem(i, x[0]);

      if Compare(x[0], y[0]) < 0 then
        Continue;

      SetItem(i, y[0]);
      j := min + 1;

      while j <= max do
      begin
        GetItem(j, y[0]);

        if Compare(x[0], y[0]) < 0 then
          Break;

        SetItem(j - 1, y[0]);
        Inc(j);
      end;

      SetItem(j - 1, x[0]);
      GetItem(min, y[0]);
    end;
  end;

var
  i: Integer;
begin
  if (Min <> 0) or (Max <> Count - 1) then
  begin
    inherited Sort(Compare, 0, Count - 1);
    Exit;
  end;

  if FStorage.Count < 1 then
    Exit;

  SortBlock(0);

  for i := 1 to FStorage.Count - 1 do
  begin
    SortBlock(i);
    MergeBlock(i);
  end;
end;

function TCachedArray.GetCount: Integer;
begin
  Result := FCount
end;

procedure TCachedArray.SetCount(Value: Integer);
var
  i, n: Integer;
begin
  n := (Value + FBlock - 1) div FBlock;

  if Value > Count then
    for i := FStorage.Count to n - 1 do
      FStorage[i] := CreateBlock
  else
    FStorage.Count := n;

  FCount := Value;
end;

procedure TCachedArray.SetZero(f: Boolean);
begin
  FZero := f
end;

function TCachedArray.GetItemData(Index: Integer): Pointer;
var
  b: TDataBlock;
begin
  b := FStorage[Index div FBlock] as TDataBlock;
  Result := Pointer(Integer(b.Data) + (Index mod FBlock) * ItemSize);
end;

function TCachedArray.GetItemSize: Integer;
begin
  Result := FItemSize
end;

function TCachedArray.GetZero: Boolean;
begin
  Result := FZero
end;

function TCachedArray.CreateBlock: TDataBlock;
begin
  Result := TDataBlock.Create(FBlock * ItemSize, Zero)
end;

{ TDataBlock }

constructor TDataBlock.Create(Size: Integer; Zero: Boolean);
begin
  if Size = 0 then
    Exit;

  GetMem(Data, Size);

  if Zero then
    FillChar(Data^, Size, 0)
  else
    FillChar(Data^, Size, $ff);

  Self.Size := Size;
end;

destructor TDataBlock.Destroy;
begin
  FreeMem(Data, Size)
end;

function DbRead(Stream: TStream): TObject;
var
  n: Integer;
  b: TDataBlock;
begin
  Stream.ReadBuffer(n, SizeOf(n));
  b := TDataBlock.Create(n);
  Stream.ReadBuffer(b.Data^, n);
  Result := b;
end;

procedure DbWrite(Stream: TStream; Block: TObject);
var
  b: TDataBlock;
begin
  b := Block as TDataBlock;

  Stream.WriteBuffer(b.Size, SizeOf(b.Size));

  if b.Size > 0 then
    Stream.WriteBuffer(b.Data^, b.Size)
end;

{ TIntArrayBase }

procedure TIntArrayBase.Append(Value: Integer);
begin
  Count := Count + 1;
  Items[Count - 1] := Value;
end;

function TIntArrayBase.GetItemSize: Integer;
begin
  Result := SizeOf(Items[0])
end;

procedure TIntArrayBase.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Value)
end;

{ TIntArray }

function TIntArray.GetCount: Integer;
begin
  Result := Length(FArray)
end;

function TIntArray.GetItem(Index: Integer): Integer;
begin
  Result := FArray[Index]
end;

procedure TIntArray.SetItem(Index: Integer; Value: Integer);
begin
  FArray[Index] := Value
end;

procedure TIntArray.SetZero(f: Boolean);
begin
  FZero := f
end;

function TIntArray.GetItemData(Index: Integer): Pointer;
begin
  Result := @FArray[Index]
end;

function TIntArray.GetZero: Boolean;
begin
  Result := FZero
end;

procedure TIntArray.SetCount(Value: Integer);
var
  n: Integer;
begin
  n := Count;
  SetLength(FArray, Value);
  ResetItems(n, Value - 1);
end;

{ TCachedIntArray }

constructor TCachedIntArray.Create(DataStream, ServStream: TStream; Block: Integer);
begin
  FArray := TCachedArray.Create(DataStream, ServStream, Block, ItemSize)
end;

constructor TCachedIntArray.Create(DataFile, ServFile: string; Block: Integer);
begin
  FArray := TCachedArray.Create(DataFile, ServFile, Block, ItemSize)
end;

destructor TCachedIntArray.Destroy;
begin
  FArray.Free;
  inherited;
end;

function TCachedIntArray.GetCount: Integer;
begin
  Result := FArray.Count
end;

function TCachedIntArray.GetItem(Index: Integer): Integer;
begin
  FArray.GetItem(Index, Result)
end;

function TCachedIntArray.GetItemData(Index: Integer): Pointer;
begin
  Result := FArray.ItemData[Index]
end;

function TCachedIntArray.GetZero: Boolean;
begin
  Result := FArray.Zero
end;

procedure TCachedIntArray.SetCount(Value: Integer);
begin
  FArray.Count := Value
end;

procedure TCachedIntArray.SetItem(Index, Value: Integer);
begin
  FArray.SetItem(Index, Value)
end;

procedure TCachedIntArray.SetZero(f: Boolean);
begin
  FArray.Zero := f
end;

{ TExtArrayBase }

procedure TExtArrayBase.Append(Value: Extended);
begin
  Count := Count + 1;
  Items[Count - 1] := Value;
end;

function TExtArrayBase.GetItemSize: Integer;
begin
  Result := SizeOf(Items[0])
end;

procedure TExtArrayBase.Insert(Index: Integer; Value: Extended);
begin
  inherited Insert(Index, Value)
end;

{ TExtArray }

function TExtArray.GetCount: Integer;
begin
  Result := Length(FArray)
end;

function TExtArray.GetItem(Index: Integer): Extended;
begin
  Result := FArray[Index]
end;

procedure TExtArray.SetItem(Index: Integer; Value: Extended);
begin
  FArray[Index] := Value
end;

procedure TExtArray.SetZero(f: Boolean);
begin
  FZero := f
end;

function TExtArray.GetItemData(Index: Integer): Pointer;
begin
  Result := @FArray[Index]
end;

function TExtArray.GetZero: Boolean;
begin
  Result := FZero
end;

procedure TExtArray.SetCount(Value: Integer);
var
  n: Integer;
begin
  n := Count;
  SetLength(FArray, Value);
  ResetItems(n, Value - 1);
end;

{ TCachedExtArray }

constructor TCachedExtArray.Create(DataStream, ServStream: TStream; Block: Integer);
begin
  FArray := TCachedArray.Create(DataStream, ServStream, Block, ItemSize)
end;

constructor TCachedExtArray.Create(DataFile, ServFile: string; Block: Integer);
begin
  FArray := TCachedArray.Create(DataFile, ServFile, Block, ItemSize)
end;

destructor TCachedExtArray.Destroy;
begin
  FArray.Free;
  inherited;
end;

function TCachedExtArray.GetCount: Integer;
begin
  Result := FArray.Count
end;

function TCachedExtArray.GetItem(Index: Integer): Extended;
begin
  FArray.GetItem(Index, Result)
end;

function TCachedExtArray.GetItemData(Index: Integer): Pointer;
begin
  Result := FArray.ItemData[Index]
end;

function TCachedExtArray.GetZero: Boolean;
begin
  Result := FArray.Zero
end;

procedure TCachedExtArray.SetCount(Value: Integer);
begin
  FArray.Count := Value
end;

procedure TCachedExtArray.SetItem(Index: Integer; Value: Extended);
begin
  FArray.SetItem(Index, Value)
end;

procedure TCachedExtArray.SetZero(f: Boolean);
begin
  FArray.Zero := f
end;

{ TStreamRW }

constructor TStreamRW.Create(Stream: TStream; Secure: Boolean);
begin
  if Stream = nil then
    raise Exception.Create('Invalid stream');

  FStream := Stream;
  FSecure := Secure;
end;

function TStreamRW.GetBoolType: Integer;
begin
  Result := $31927381;
end;

function TStreamRW.GetExtType: Integer;
begin
  Result := $11a328b2;
end;

function TStreamRW.GetIntType: Integer;
begin
  Result := -$218a9320;
end;

function TStreamRW.GetStrType: Integer;
begin
  Result := $71bcf389;
end;

function TStreamRW.ReadBool: Boolean;
begin
  ReadType(GetBoolType);
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TStreamRW.ReadExt: Extended;
begin
  ReadType(GetExtType);
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TStreamRW.ReadInt: Integer;
begin
  ReadType(GetIntType);
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TStreamRW.ReadStr: WideString;
var
  n: Integer;
begin
  ReadType(GetStrType);
  n := 0;
  FStream.ReadBuffer(n, 2);

  if n = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, n);
  FStream.ReadBuffer(Result[1], n * SizeOf(Result[1]));
end;

procedure TStreamRW.ReadType(t: Integer);
var
  x: Integer;
begin
  if not FSecure then
    Exit;

  FStream.ReadBuffer(x, SizeOf(x));

  if x <> t then
    raise Exception.Create('Incorrect data type');
end;

procedure TStreamRW.WriteBool(x: Boolean);
begin
  WriteType(GetBoolType);
  FStream.WriteBuffer(x, SizeOf(x));
end;

procedure TStreamRW.WriteExt(x: Extended);
begin
  WriteType(GetExtType);
  FStream.WriteBuffer(x, SizeOf(x));
end;

procedure TStreamRW.WriteInt(x: Integer);
begin
  WriteType(GetIntType);
  FStream.WriteBuffer(x, SizeOf(x));
end;

procedure TStreamRW.WriteStr(x: WideString);
var
  n: Integer;
begin
  WriteType(GetStrType);
  n := Length(x);
  FStream.WriteBuffer(n, 2);

  if n > 0 then
    FStream.WriteBuffer(x[1], n * SizeOf(x[1]));
end;

procedure TStreamRW.WriteType(t: Integer);
begin
  if FSecure then
    FStream.WriteBuffer(t, SizeOf(t));
end;

{ TBitArray }

procedure TBitArray.BitAnd(Src: TBitArray);
var
  i: Integer;
begin
  Assert(Length <= Src.Length);

  for i := 0 to High(FData) do
    FData[i] := FData[i] and Src.FData[i];

  Trunc;
end;

procedure TBitArray.BitOr(Src: TBitArray);
var
  i: Integer;
begin
  Assert(Length <= Src.Length);

  for i := 0 to High(FData) do
    FData[i] := FData[i] or Src.FData[i];

  Trunc;
end;

procedure TBitArray.BitXor(Src: TBitArray);
var
  i: Integer;
begin
  Assert(Length <= Src.Length);

  for i := 0 to High(FData) do
    FData[i] := FData[i] xor Src.FData[i];

  Trunc;
end;

function TBitArray.Clone: TBitArray;
begin
  Result := TBitArray.Create;

  try
    Result.Length := Length;

    if Length > 0 then
      Move(FData[0], Result.FData[0], System.Length(FData));

    Result.Trunc;
  except
    Result.Free;
    Result := nil;
  end;
end;

function TBitArray.GetNumOfSetBits: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to High(FData) do
    Inc(Result, NumSetBits[FData[i]]);
end;

function TBitArray.GetRightmostBitIndex: Integer;
begin
  for Result := Length - 1 downto 0 do
    if Bits[Result] then
      Exit;

  Result := -1;
end;

procedure TBitArray.ResetBits;
begin
  if System.Length(FData) > 0 then
    FillChar(FData[0], System.Length(FData), 0)
end;

procedure TBitArray.SaveToStream(Stream: TStream);
begin
  if System.Length(FData) > 0 then
    Stream.WriteBuffer(FData[0], System.Length(FData))
end;

procedure TBitArray.SetBit(Index: Integer; Value: Boolean);
var
  i: Integer;
begin
  Assert((Index >= 0) and (Index < Length));
  i := Index shr 3;

  if Value then
    FData[i] := FData[i] or (1 shl (Index and 7))
  else
    FData[i] := FData[i] and not (1 shl (Index and 7))
end;

function TBitArray.GetBit(Index: Integer): Boolean;
begin
  Assert((Index >= 0) and (Index < Length));
  Result := (FData[Index shr 3] and (1 shl (Index and 7))) > 0
end;

procedure TBitArray.SetBits;
begin
  if System.Length(FData) > 0 then
    FillChar(FData[0], System.Length(FData), -1);

  Trunc;
end;

procedure TBitArray.SetBitsLength(NewLength: Integer);
begin
  SetLength(FData, (NewLength + 7) shr 3);
  FLength := NewLength;
  Trunc;
end;

procedure TBitArray.Trunc;
begin
  if (Length and 7) > 0 then
    FData[High(FData)] := FData[High(FData)] and ((1 shl (Length and 7)) - 1);
end;

{ TCachedStream }

constructor TCachedStream.Create(Stream: TStream; DeleteOnDestroy: Boolean);
begin
  FStream := Stream;
  FDeleteOnDestroy := DeleteOnDestroy;
  CacheSize := 4096; // typical size of one cluster
end;

destructor TCachedStream.Destroy;
begin
  FlushCache;

  if FDeleteOnDestroy then
    FStream.Free;

  inherited
end;

procedure TCachedStream.FlushCache;
begin
  if FUsed <> FStream.Write(FChunk[0], FUsed) then
  begin
    {$IFDEF STORAGE_DEBUG}
    DbgPrint('TCachedStream.FlushCache failed'#10);
    FStream.Write(FChunk[0], FUsed);
    {$ENDIF}
  end;

  FUsed := 0;
end;

function TCachedStream.GetCacheSize: Integer;
begin
  Result := Length(FChunk)
end;

function TCachedStream.Read(var Buffer; Count: Integer): Longint;
begin
  raise Exception.CreateFmt('%s does not support reading', [ClassName])
end;

function TCachedStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  FlushCache;
  Result := FStream.Seek(Offset, Origin);
end;

procedure TCachedStream.SetCacheSize(Size: Integer);
begin
  if FUsed > 0 then
    raise Exception.Create('Changing cache size is allowed only when the cache is empty');

  SetLength(FChunk, Size)
end;

function TCachedStream.Write(const Buffer; Count: Integer): Longint;
var
  n, p: Integer;
begin
  Result := Count;
  p := 0;

  while Count > 0 do
  begin
    n := Min(Count, Length(FChunk) - FUsed);
    Move(Pointer(Integer(@Buffer) + p)^, FChunk[FUsed], n);

    Inc(p, n);
    Inc(FUsed, n);
    Dec(Count, n);

    if FUsed = Length(FChunk) then
      FlushCache
  end;
end;

{ TBase64Encoder }

constructor TBase64Encoder.Create(Output: TStream);
begin
  FOutput := Output;
  InitMap;
end;

destructor TBase64Encoder.Destroy;
begin
  Finalise;
  inherited;
end;

procedure TBase64Encoder.Encode(a, n: Integer);
var
  b: Integer;
begin
  b :=
    FMap[a shr 2 and 63] or
    FMap[a shr 12 and 15 or a and 3 shl 4 and 63] shl 8 or
    FMap[a shr 22 and 3 or a shr 8 and 15 shl 2 and 63] shl 16 or
    FMap[a shr 16 and 63] shl 24;

  case n of
    1: FOutput.Write(b, 2);
    2: FOutput.Write(b, 3);
    3: FOutput.Write(b, 4);
  end;
end;

procedure TBase64Encoder.Finalise;
var
  b: Byte;
begin
  Encode(FCache, FUsed);
  b := Byte('=');

  while (FUsed > 0) and (FUsed < 3) do
  begin
    FOutput.Write(b, 1);
    Inc(FUsed);
  end;
end;

procedure TBase64Encoder.InitMap;

  procedure Map(Dest, Src, Count: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      FMap[Dest + i] := Src + i
  end;

begin
  Map(0,  Byte('A'), 26); // A..Z
  Map(26, Byte('a'), 26); // a..z
  Map(52, Byte('0'), 10); // 0..9

  FMap[62] := Byte('+');  // +
  FMap[63] := Byte('/');  // /
end;

function TBase64Encoder.Read(var Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('This stream is write-only')
end;

function TBase64Encoder.This: TBase64Encoder;
begin
  Result := Self
end;

function TBase64Encoder.Write(const Buffer; Count: Integer): Longint;

  function BitMask(n: Integer): Integer;
  begin
    Result := (1 shl (8*n)) - 1
  end;

var
  n: Integer;
begin
  if Count = 0 then
    {do nothing}
  else if FUsed > 0 then
  begin
    n := Min(3 - FUsed, Count);
    FCache := FCache or ((PInteger(@Buffer)^ and BitMask(n)) shl (8*FUsed));
    Inc(FUsed, n);

    if FUsed = 3 then
    begin
      Encode(FCache, 3);
      FUsed := 0;
    end;

    Write(Pointer(Integer(@Buffer) + n)^, Count - n);
  end
  else
  begin
    n := 0;

    while n + 3 <= Count do
    begin
      Encode(PInteger(Integer(@Buffer) + n)^, 3);
      Inc(n, 3);
    end;

    if n < Count then
    begin
      FCache := PInteger(Integer(@Buffer) + n)^ and BitMask(Count - n);
      FUsed := Count - n;
    end;
  end;

  Result := Count
end;

{ TFmtStream }

constructor TFmtStream.Create(Output: TStream; Own: Boolean);
begin
  FOutput := Output;
  FOwn := Own;
end;

destructor TFmtStream.Destroy;
begin
  if FOwn then
    FOutput.Free;

  inherited;
end;

procedure TFmtStream.IncIndent(Step: Integer);
begin
  Inc(FIndent, Step)
end;

procedure TFmtStream.PutsA(const Fmt: AnsiString; const Args: array of const);
begin
  Puts(string(Fmt), Args)
end;

procedure TFmtStream.PutsA(const s: AnsiString);
var
  i: Integer;
begin
  if Formatted then
    for i := 1 to Indent do
      PutsRawA(' ');

  PutsRawA(s);

  if Formatted then
    PutsRawA(AnsiString(#13#10))
end;

procedure TFmtStream.Puts(const Fmt: string; const Args: array of const);
begin
  Puts(Format(Fmt, Args))
end;

procedure TFmtStream.PutsRaw(const s: string);
begin
  PutsRawA(AnsiString(s))
end;

procedure TFmtStream.PutsRawA(const s: AnsiString);
begin
  if s <> '' then
    Write(s[1], Length(s))
end;

procedure TFmtStream.PutsRaw(const Fmt: string; const Args: array of const);
begin
  PutsRaw(Format(Fmt, Args))
end;

procedure TFmtStream.PutsRawA(const Fmt: AnsiString; const Args: array of const);
begin
  PutsRaw(string(Fmt), Args)
end;

procedure TFmtStream.Puts(const s: string);
begin
  PutsA(AnsiString(s))
end;

function TFmtStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FOutput.Read(Buffer, Count)
end;

function TFmtStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FOutput.Write(Buffer, Count)
end;

{ TProxyStream }

constructor TProxyStream.Create(Stream: TStream; Offset, Size: Integer);
begin
  Init(Stream, Offset, Size)
end;

procedure TProxyStream.Init(Stream: TStream; Offset, Size: Integer);
begin
  if Stream is TProxyStream then
    Init(TProxyStream(Stream).FStream, Offset + TProxyStream(Stream).FOffset, Size)
  else
  begin
    FStream := Stream;
    FOffset := Offset;
    FSize := Size
  end;
end;

function TProxyStream.AdjustRange(var Len: Integer): Boolean;
begin
  Result := (FPos >= 0) and (FPos < FSize);

  if FPos + Len > FSize then
    Len := FSize - FPos;
end;

function TProxyStream.Read(var Buffer; Count: Integer): Longint;
var
  p: Longint;
begin
  Result := 0;

  if not AdjustRange(Count) then
    Exit;

  p := FStream.Position;
  FStream.Position := FOffset + FPos;
  Result := FStream.Read(Buffer, Count);
  FStream.Position := p;
  FPos := FPos + Result;
end;

function TProxyStream.Write(const Buffer; Count: Integer): Longint;
var
  p: Longint;
begin
  Result := 0;

  if not AdjustRange(Count) then
    Exit;

  p := FStream.Position;
  FStream.Position := FOffset + FPos;
  Result := FStream.Write(Buffer, Count);
  FStream.Position := p;
  FPos := FPos + Result;
end;

function TProxyStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPos := Offset;
    soFromCurrent:   FPos := FPos + Offset;
    soFromEnd:       FPos := FSize + Offset;
  end;

  Result := FPos;
end;

{ THexEncoder }

constructor THexEncoder.Create(Output: TStream);
begin
  FOutput := Output
end;

function THexEncoder.Read(var Buffer; Count: Integer): Longint;
begin
  raise ENotImplemented.Create('This stream is read-only')
end;

function THexEncoder.Write(const Buffer; Count: Integer): Longint;
const
  h: AnsiString = '0123456789abcdef';
var
  i: Integer;
  b: Byte;
  s: array[0..1] of AnsiChar;
begin
  for i := 0 to Count - 1 do
  begin
    b := PByte(Integer(@Buffer) + i)^;

    s[0] := h[1 + (b shr 4)];
    s[1] := h[1 + (b and $f)];

    FOutput.WriteBuffer(s[0], 2);
  end;

  Result := Count;
end;

{ TLineSplitter }

constructor TLineSplitter.Create(Output: TStream; Length: Integer; Sep: AnsiString);
begin
  Assert(Sep <> '', 'This class is useless for empty separators');

  inherited Create;

  FOutput := Output;
  FLength := Length;
  FSep := Sep;
end;

function TLineSplitter.Read(var Buffer; Count: Integer): Longint;
begin
  raise ENotSupportedException.Create('The stream is read-only')
end;

function TLineSplitter.Write(const Buffer; Count: Integer): Longint;
var
  n: Integer;
  p: Pointer;
begin
  Result := Count;
  p := @Buffer;

  while Count > 0 do
  begin
    n := Min(FLength - FWritten, Count);
    FOutput.WriteBuffer(p^, n);
    p := Pointer(Integer(p) + n);
    Inc(FWritten, n);
    Dec(Count, n);

    if FWritten = FLength then
    begin
      FOutput.WriteBuffer(FSep[1], Length(FSep));
      FWritten := 0;
    end;
  end;
end;

{ TBTreeNode }

constructor TBTreeNode.Create(MaxLen: Integer);
begin
  Assert(MaxLen > 0);
  SetLength(FKeys, MaxLen);
  SetLength(FVals, MaxLen);
  SetLength(FRefs, MaxLen + 1);
end;

function TBTreeNode.GetKey(i: Integer): Pointer;
begin
  Assert((i >= 0) and (i < Len));
  Result := FKeys[i]
end;

function TBTreeNode.GetRef(i: Integer): TBTreeNode;
begin
  Assert((i >= 0) and (i < Len + 1));
  Result := FRefs[i]
end;

function TBTreeNode.GetVal(i: Integer): Pointer;
begin
  Assert((i >= 0) and (i < Len));
  Result := FVals[i]
end;

procedure TBTreeNode.SetKey(i: Integer; k: Pointer);
begin
  Assert((i >= 0) and (i < Len));
  FKeys[i] := k
end;

procedure TBTreeNode.SetLen(n: Integer);
begin
  Assert((n >= 0) and (n <= Length(FKeys)));
  FLen := n;
end;

procedure TBTreeNode.SetRef(i: Integer; r: TBTreeNode);
begin
  Assert((i >= 0) and (i < Len +1));
  FRefs[i] := r
end;

procedure TBTreeNode.SetVal(i: Integer; v: Pointer);
begin
  Assert((i >= 0) and (i < Len));
  FVals[i] := v
end;

{ TBTree }

constructor TBTree.Create(Order: Integer);
begin
  Assert(Order >= 4);
  Assert(Order mod 2 = 0);
  FOrder := Order;
end;

destructor TBTree.Destroy;

  procedure DeleteNode(Node: TBTreeNode);
  var
    i: Integer;
  begin
    if Node = nil then
      Exit;

    for i := 0 to Node.Len do
      DeleteNode(Node.Refs[i]);

    Node.Free;
  end;

begin
  DeleteNode(FRoot);
  inherited;
end;

function TBTree.FindIndex(Node: TBTreeNode; Key: Pointer): Integer;

  function LinSearch(Node: TBTreeNode; Key: Pointer): Integer;
  var
    i: Integer;
  begin
    i := 0;

    with Node do
      while (i < Len) and (Cardinal(Keys[i]) < Cardinal(Key)) do
        Inc(i);

    Result := i;
  end;

  function BinSearch(Node: TBTreeNode; Key: Pointer): Integer;
  var
    i, j, m: Integer;
    k: Pointer;
  begin
    if Node.Len = 0 then
      Result := 0
    else
    with Node do
    begin
      i := 0;
      j := Len - 1;

      repeat
        m := (i + j) div 2; // due to i and j are small, the expression i + (j - i) div 2 can be avoided
        k := Keys[m];

        if Cardinal(Key) < Cardinal(k) then
          j := m - 1
        else if Cardinal(Key) > Cardinal(k) then
          i := m + 1
        else
          Break;
      until i > j;

      if Cardinal(k) < Cardinal(Key) then
        Result := m + 1
      else
        Result := m
    end;
  end;

begin
  Assert(BinSearch(Node, Key) = LinSearch(Node, Key));

  if Node.Len < 10 then
    Result := LinSearch(Node, Key)
  else
    Result := BinSearch(Node, Key)
end;

function TBTree.FindKey(Key: Pointer; out KeyIndex: Integer): TBTreeNode;
var
  Node: TBTreeNode;
  i: Integer;
begin
  if FRoot = nil then
  begin
    Result := nil;
    Exit;
  end;

  Node := FRoot;

  repeat
    Assert(Node <> nil);

    with Node do
    begin
      i := FindIndex(Node, Key);

      if (i < Len) and (Key = Keys[i]) then
      begin
        KeyIndex := i;
        Result := Node;
        Exit;
      end;

      Node := Refs[i];
    end;
  until Node = nil;

  Result := nil;
end;

function TBTree.CreateNode(LNode: TBTreeNode): TBTreeNode;
begin
  Result := TBTreeNode.Create(FOrder - 1);
  Result.Refs[0] := LNode;
end;

function TBTree.IsFull(Node: TBTreeNode): Boolean;
begin
  Result := Node.Len + 1 = FOrder
end;

function TBTree.GetHeight: Integer;
var
  Node: TBTreeNode;
begin
  Result := 0;
  Node := FRoot;

  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.Refs[0];
  end;
end;

function TBTree.GetItem(Key: Pointer): Pointer;
var
  Node: TBTreeNode;
  i: Integer;
begin
  Node := FindKey(Key, i);

  if Node = nil then
    Result := nil
  else
    Result := Node.Vals[i]
end;

procedure TBTree.GetKeysAndValues(Keys, Values: TList);

  procedure Visit(Node: TBTreeNode);
  var
    i: Integer;
  begin
    if Node = nil then
      Exit;

    Visit(Node.Refs[0]);

    for i := 0 to Node.Len - 1 do
    begin
      if Keys <> nil then
        Keys.Add(Node.Keys[i]);

      if Values <> nil then
        Values.Add(Node.Vals[i]);

      Visit(Node.Refs[i + 1]);
    end;
  end;

var
  n: Integer;
begin
  n := KeysCount;

  if Keys <> nil then
    Keys.Capacity := n;

  if Values <> nil then
    Values.Capacity := n;

  if (Keys <> nil) or (Values <> nil) then
    Visit(FRoot);
end;

function TBTree.GetKeysCount: Integer;

  function Count(Node: TBTreeNode): Integer;
  var
    i: Integer;
  begin
    if Node = nil then
      Result := 0
    else
    begin
      Result := Node.Len;

      for i := 0 to Node.Len do
        Inc(Result, Count(Node.Refs[i]));
    end;
  end;

begin
  Result := Count(FRoot)
end;

procedure TBTree.SetItem(Key, Value: Pointer);
begin
  Insert(Key, Value);
end;

procedure TBTree.SetItemExistence(Key: Pointer; b: Boolean);
begin
  if b then
    Insert(Key, nil)
  else
    raise ENotImplemented.Create('Removing keys not implemented')
end;

function TBTree.ItemExists(Key: Pointer): Boolean;
var
  i: Integer;
begin
  Result := FindKey(Key, i) <> nil
end;

procedure TBTree.InsertKey(Node: TBTreeNode; i: Integer; Key, Value: Pointer; LNode, RNode: TBTreeNode);
var
  j: Integer;
begin
  Assert(not IsFull(Node));
  Assert((i >= 0) and (i <= Node.Len));

  with Node do
  begin
    Len := Len + 1;

    for j := Len - 1 downto i + 1 do
      Keys[j] := Keys[j - 1];

    for j := Len - 1 downto i + 1 do
      Vals[j] := Vals[j - 1];

    for j := Len downto i + 2 do
      Refs[j] := Refs[j - 1];

    Keys[i] := Key;
    Vals[i] := Value;
    Refs[i] := LNode;
    Refs[i + 1] := RNode;
  end;
end;

procedure TBTree.SplitTree(Node: TBTreeNode; i: Integer);
var
  LNode, RNode: TBTreeNode;
  Key, Value: Pointer;
  j, m: Integer;
begin
  Assert(not IsFull(Node));
  Assert(Node.Refs[i] <> nil);
  Assert(IsFull(Node.Refs[i]));

  LNode := Node.Refs[i];
  RNode := CreateNode;

  with LNode do
  begin
    m := Len div 2;

    Key := Keys[m];
    Value := Vals[m];

    RNode.Len := m;

    for j := 0 to m - 1 do
      RNode.Keys[j] := Keys[m + 1 + j];

    for j := 0 to m - 1 do
      RNode.Vals[j] := Vals[m + 1 + j];

    for j := 0 to m do
      RNode.Refs[j] := Refs[m + 1 + j];

    Len := m;
  end;

  with Node do
  begin
    Assert(Refs[i] = LNode);
    InsertKey(Node, i, Key, Value, LNode, RNode);
  end;
end;

function TBTree.Insert(Key: Pointer; Value: Pointer): Boolean;
var
  Node: TBTreeNode;
  i: Integer;
begin
  if FRoot = nil then
  begin
    FRoot := CreateNode;
    FRoot.Len := 1;
    FRoot.Keys[0] := Key;
    FRoot.Vals[0] := Value;
    Result := True;
    Exit;
  end;

  if IsFull(FRoot) then
  begin
    FRoot := CreateNode(FRoot);
    SplitTree(FRoot, 0);
  end;

  Result := False;
  Node := FRoot;

  repeat
    Assert(Node <> nil);

    with Node do
    begin
      i := FindIndex(Node, Key);

      if (i < Len) and (Key = Keys[i]) then
        Break;

      if (Refs[i] = nil) or not IsFull(Refs[i]) then
      begin
        if Refs[i] = nil then
        begin
          InsertKey(Node, i, Key, Value, nil, nil);
          Result := True;
        end;

        Node := Refs[i]
      end
      else
      begin
        SplitTree(Node, i);

        if Key = Keys[i] then
          Break;

        if Cardinal(Key) < Cardinal(Keys[i]) then
          Node := Refs[i]
        else
          Node := Refs[i + 1];
      end;
    end;
  until Node = nil;

  if Node <> nil then
    Node.Vals[i] := Value;
end;

{ TAVLTree }

procedure TAVLTree.LiftChild(R, N: TAVLTreeNode; Leaf: Boolean);
var
  A, B: TAVLTreeNode;
begin
  Assert(N <> nil);
  Assert(N[Leaf] <> nil);

  A := N[Leaf];
  B := A[not Leaf];

  A[not Leaf] := N;
  N[Leaf] := B;

  if R = nil then
    FRoot := A
  else
    R[R[True] = N] := A;

  N.UpdateHeight;
  A.UpdateHeight;
end;

procedure TAVLTree.Insert(Parent, Node: TAVLTreeNode; Key, Value: Pointer);
var
  Leaf: Boolean;
begin
  Assert(Node <> nil);

  if Node.Key = Key then
  begin
    Node.Value := Value;
    Exit;
  end;

  Leaf := Cardinal(Key) < Cardinal(Node.Key);

  if Node[Leaf] = nil then
  begin
    Node[Leaf] := TAVLTreeNode.Create(Key, Value);
    Node.Height := 2;
    Exit;
  end;

  Insert(Node, Node[Leaf], Key, Value);
  Node.UpdateHeight;

  if Node[Leaf].Height < 2 + Node[not Leaf].Height then
    Exit;

  if Node[Leaf][not Leaf].Height > Node[Leaf][Leaf].Height then
    LiftChild(Node, Node[Leaf], not Leaf);

  LiftChild(Parent, Node, Leaf);
end;

function TAVLTree.GetItem(Key: Pointer): Pointer;
var
  Node: TAVLTreeNode;
begin
  if FindKey(Key, Node) then
    Result := Node.Value
  else
    Result := nil
end;

procedure TAVLTree.SetItem(Key, Value: Pointer);
begin
  if FRoot = nil then
    FRoot := TAVLTreeNode.Create(Key, Value)
  else
    Insert(nil, FRoot, Key, Value)
end;

function TAVLTree.ItemExists(Key: Pointer): Boolean;
var
  Node: TAVLTreeNode;
begin
  Result := FindKey(Key, Node)
end;

procedure TAVLTree.SetItemExistence(Key: Pointer; b: Boolean);
begin
  if b then
    SetItem(Key, nil)
  else
    raise ENotImplemented.Create('Item cannot be removed')
end;

destructor TAVLTree.Destroy;
begin
  Root.Free;
  inherited;
end;

function TAVLTree.FindKey(Key: Pointer; out Node: TAVLTreeNode): Boolean;
var
  N: TAVLTreeNode;
begin
  N := FRoot;

  while (N <> nil) and (N.Key <> Key) do
    N := N[Cardinal(Key) < Cardinal(N.Key)];

  Node := N;
  Result := N <> nil;
end;

{ TAVLTreeNode }

constructor TAVLTreeNode.Create(Key, Value: Pointer);
begin
  FKey := Key;
  FValue := Value;
  FHeight := 1;
end;

destructor TAVLTreeNode.Destroy;
begin
  FLeafs[True].Free;
  FLeafs[False].Free;
  inherited;
end;

function TAVLTreeNode.GetChild(Leaf: Boolean): TAVLTreeNode;
begin
  Result := FLeafs[Leaf]
end;

function TAVLTreeNode.GetHeight: Byte;
begin
  if Self = nil then
    Result := 0
  else
    Result := FHeight
end;

procedure TAVLTreeNode.SetChild(Leaf: Boolean; Node: TAVLTreeNode);
begin
  FLeafs[Leaf] := Node;
end;

procedure TAVLTreeNode.UpdateHeight;
begin
  FHeight := Max(Child[True].Height, Child[False].Height) + 1
end;

initialization

InitNumSetBits;

end.
