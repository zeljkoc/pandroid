
{******************************************}
{                                          }
{             FastReport v4.0              }
{        Escher Stream Writing API         }
{                                          }
{         Copyright (c) 1998-2011          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

{ This module provides API for writing Escher
  streams. This format is documented in MSDN
  [MS-ODRAW] section. }

//VCL uses section
{$IFNDEF FMX}
unit fcxEscher;

{$include fcx.inc}

interface

uses
  Types, Classes;
{$ELSE}
{$include fcx.inc}

interface

uses
  System.Types, System.Classes;
{$ENDIF}

const

  //
  // Record types
  //

  EscherRkBse     = $f007;
  EscherRkBlip    = $f018;
  EscherRkDggc    = $f000;
  EscherRkDgg     = $f006;
  EscherRkBstore  = $f001;
  EscherRkSpCont  = $f004;
  EscherRkSp      = $f00a;
  EscherRkOpts    = $f00b;
  EscherRkAnchor  = $f00e;
  EscherRkDgCont  = $f002;
  EscherRkDg      = $f008;
  EscherRkSpgrCont= $f003;
  EscherRkSpgr    = $f009;
  EscherRkSMC     = $f11e;
  EscherRkCData   = $f011;
  EscherRkCAnchor = $f010;

  //
  // BLIP kinds
  //

  EscherBkEMF     = 2;
  EscherBkWMF     = 3;
  EscherBkJPEG    = 5;
  EscherBkPNG     = 6;
  EscherBkDIB     = 7;
  EscherBkTIFF    = 8;

  //
  // BLIP signatures
  // Actually these values are stored
  // in 12-bit fields.
  //

  EscherBsUnknown = $000;
  EscherBsWMF     = $216;
  EscherBsEMF     = $3d4;
  EscherBsPNG     = $6e0;
  EscherBsJPEG    = $46a;
  EscherBsDIB     = $7a8;
  EscherBsTIFF    = $6e4;

  //
  // BLIP usage
  //

  EscherBuDefault = 0;
  EscherBuTexture = 1;

  //
  // Metafile compression
  //

  EscherMcDeflate = 0;
  EscherMcNone    = $fe;

  //
  // Shape flags.
  // They are specified in Sp record.
  //

  EscherSfGroup   = $001;
  EscherSfChild   = $002;
  EscherSfRoot    = $004;
  EscherSfDeleted = $008;
  EscherSfOle     = $010;
  EscherSfMaster  = $020;
  EscherSfFlipHor = $040;
  EscherSfFlipVer = $080;
  EscherSfConn    = $100;
  EscherSfAnchor  = $200;
  EscherSfBg      = $400;
  EscherSfShape   = $800;

  //
  // Shape types.
  //
  // These values are written in 
  // the instance field of the Sp 
  // record.
  //
  // The complete list of 204
  // values can be found in the
  // documentation.
  //

  EscherStNone          = 0;
  EscherStPictureFrame  = 75;

  //
  // Each shape in an Escher stream has an
  // unique id, named "shape id". Shapes are
  // grouped into "drawing groups". Each drawing
  // group can contain up to a limited number
  // of shapes. This is not a restriction of
  // the Escher format, but this limit allow
  // to simplify generating shape id values.
  //

  EscherGroupLimit      = 1024;

type

  { Escher stream consists of consequent records.
    Each record has a fixed header and a variable
    length body. The body of an Escher record can
    be another Escher stream.

    When TEscherStream serializes its data,
    it looks over all its TEscherRec items and
    serializes them consequently.

    When TEscherRec serializes its data,
    it does that in three steps:

      - it writes the fixed header of 8 bytes

      - it serializes the own data, due to it is
        derived from TMemoryStream

      - it serializes all subrecords, due to TEscherRec
        is derived from TEscherStream }

  TEscherRec = class;

  TEscherStream = class(TMemoryStream)
  private

    Records:  TList;              // List of TEscherRec

  public

    constructor Create;
    destructor Destroy; override;

    procedure Flush(Stream: TStream); virtual;
    procedure WriteVal(Value: LongWord; Count: LongWord);

    function Add: TEscherRec; overload;
    function Add(Version: Byte; Instance: Word; Kind: Word): TEscherRec; overload;
    function AddCont(Instance: Word; Kind: Word): TEscherRec; overload;

  end;

  TEscherRec = class(TEscherStream)
  public

    Version:  Byte;
    Instance: Word;
    Kind:     Word;

    procedure Flush(Stream: TStream); override;
    function GetESize: LongWord;

  end;

  { There are two general kinds of drawings in Escher stream:

      - Metafiles (they are: EMF, WMF and PICT)
      - Bitmaps (they are: PNG, JPEG and DIB)

    The only difference is that metafiles start with one header
    and bitmaps start with another header. The actual data of images
    is not interesting for the Escher stream }

  TEscherBlipKind = Byte;         // See EscherBk values
  TEscherBlipSign = Word;         // See EscherBs values. Actually it's 12-bit length.

  TEscherPicture = class(TMemoryStream)
  public

    Kind: TEscherBlipKind;

    function GetESize: LongWord; virtual; abstract;
    procedure Flush(Stream: TStream); virtual; abstract;

  end;

  TEscherBitmap = class(TEscherPicture)
  public

    function GetESize: LongWord; override;
    procedure Flush(Stream: TStream); override;

  end;

  TEscherMetafile = class(TEscherPicture)
  public

    Bounds:   TRect;              // Boundary for metafile commands
    MFSize:   TPoint;             // Size of the metafile in EMUs (360000 EMU = 1 cm)
    Compr:    Boolean;            // Entire metafile can be LZ compressed

    function GetESize: LongWord; override;
    procedure Flush(Stream: TStream); override;

  end;

  TEscherBlipHash = array[1..16] of Byte;

  TEscherBlip = class
  public

    Hash:       TEscherBlipHash;
    RefCount:   LongWord;
    Pict:       TEscherPicture;
    Index:      LongWord;         // Index to this BLIP in the array of images

    constructor Create(Pict: TEscherPicture);
    destructor Destroy; override;
    procedure Flush(Stream: TEscherStream);

  end;

  { Properties of drawings are stored in a separate
    record. The contents of this record is an array
    of fixed size entries followed by an array of
    variable length elements such as Unicode strings }

  TEscherProp = class
  public

    Id:         Word;
    Blip:       Boolean;
    Complex:    Boolean;
    Value:      LongInt;

    constructor Create(Id: Word);

  end;

  TEscherPropList = class
  private

    FS: TList;                    // List of TEscherProp

  public

    constructor Create;
    destructor Destroy; override;

    function Add(Id: Word): TEscherProp;
    procedure Flush(Stream: TEscherStream);

  end;

  { Shape location.
    Location of a shape in an excel sheet can be
    specified relative to data cells. The left side
    of the sahpe is specified with two parameters:

      - Left - index to an excel data cell where
        the left side of the shape is located

      - LeftOffset - a distance between the left side
        of the data cell and the left side of the shape
        (this distance is measured in excel-specific
        units of length)

    The same rule is applicable to the other three
    sides of the shape. }

  TEscherShapePos = class
  public

    Left:   LongInt;
    Top:    LongInt;
    Right:  LongInt;
    Bottom: LongInt;

    LeftOffset:   LongInt;
    TopOffset:    LongInt;
    RightOffset:  LongInt;
    BottomOffset: LongInt;

    procedure Flush(Stream: TEscherStream);

  end;

  { Drawing group is a list of shapes.
    Each shape has:

      - shape id that is unique within a document
      - reference to an image object, such as a metafile
      - some options, such as a placemenet }

  TEscherShape = class
  public

    Id:       LongWord;           // Shape Id that's unique within a document
    Flags:    LongWord;           // Combination of EsherSf flags
    Image:    LongWord;           // Reference to an image object.
    Pos:      TEscherShapePos;

    constructor Create;
    destructor Destroy; override;
    procedure Flush(Stream: TEscherStream);

  end;

  TEscherStorage = class;

  TEscherGroup = class
  private

    SId:    LongWord;             // Used for generating unique Shape Ids.
    Shapes: TList;                // List of TEscherShape

    function GetShape(Index: LongInt): TEscherShape;
    function GetSId: LongWord;
    function GetRId: LongWord;

  public

    Id:     LongWord;             // Drawing Group Id that is unique within a document

    constructor Create;
    destructor Destroy; override;

    procedure Flush(Stream: TEscherStream);
    function Count: LongWord;
    function Add: TEscherShape;
    function GetMaxSId: LongWord;

    property Items[Index: LongInt]: TEscherShape read GetShape; default;

  end;

  { Escher Storage is a global per a document Escher stream
    that contains images. The storage consists of:

      - array of drawing groups (TEscherGroup)
      - array of images (TEscherBlip)
      - array of properties (TEscherPropList) }

  TEscherStorage = class
  private

    Groups:   TList;              // List of TEscherGroup
    Images:   TList;              // List of TEscherBlip

    function GetGroup(Index: LongInt): TEscherGroup;
    function GetImage(Index: LongInt): TEscherBlip;

  public

    Props: TEscherPropList;

    constructor Create;
    destructor Destroy; override;

    function AddGroup: TEscherGroup;
    function AddImage(Pict: TEscherPicture): TEscherBlip;

    function Empty: Boolean;

    procedure Flush(Stream: TEscherStream);

    property Image[Index: LongInt]: TEscherBlip read GetImage;
    property Group[Index: LongInt]: TEscherGroup read GetGroup;

  end;

{ Returns a BLIP signature corresponding to a BLIP type }

function EscherGetBlipSign(Kind: TEscherBlipKind): TEscherBlipSign;

{ Computes a 16-bytes hash value for a drawing }

procedure EscherGetBlipHash(out Hash: TEscherBlipHash; Blip: TMemoryStream);

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxCrypto;
{$ELSE FMX}
uses
  FMX.fcxCrypto;
{$ENDIF FMX}

type

  { Shape placement
    18 bytes }

  TEscherPlacement = packed record

    Unknown:    Word;             // Content of this field is unknown
    LCol:       Word;
    LOff:       Word;
    TRow:       Word;
    TOff:       Word;
    RCol:       Word;
    ROff:       Word;
    BRow:       Word;
    BOff:       Word;

  end;

  { Types.TRect is declared without "packed"
    modifier }

  TPackedRect = packed record

    Left:       LongInt;
    Top:        LongInt;
    Right:      LongInt;
    Bottom:     LongInt;

  end;

  { Windows.TSize is declared without "packed"
    modifier }

  TPackedSize = packed record

    Width:      LongWord;
    Height:     LongWord;

  end;

  { Escher record header
    8 bytes
    Starts each record in an Escher stream }

  TEscherRecHeader = packed record

    VerInst:    Word;
    Kind:       Word;
    Length:     LongWord;

  end;

  { BLIP header
    36 bytes
    Followed by an Escher stream with the BLIP data }

  TEscherBlipHeader = packed record

    Win32:      Byte;             // BLIP type. See EscherBk values.
    MacOS:      Byte;             // BLIP type. See EscherBk values.
    Id:         TGUID;            // Unique identifier.
    Unused1:    Word;
    Size:       LongWord;         // Picture size in the stream.
    RefCount:   LongWord;         // Number of references to this BLIP.
    Offset:     LongWord;         // Offset to the delay stream. Can be zero.
    Usage:      Byte;             // See EscherBu values.
    NameLen:    Byte;             // Length of a Unicode name of this BLIP. Can be zero.
    Unused2:    Word;

  end;

  { Metafile header
    34 bytes
    Followed by metafile data, probably LZ compressed }

  TEscherMetafileHeader = packed record

    MFSize:     LongWord;         // Metafile size
    Bounds:     TPackedRect;      // Bounds for metafile commands
    Size:       TPackedSize;      // Width and height of the metafile in EMUs
    PackedSize: LongWord;         // Size of packed data that follows this header
    Compr:      Byte;             // See EscherMc values
    Filter:     Byte;             // Always $fe

  end;

  { Property header
    6 bytes }

  TEscherPropHeader = packed record

    Id:         Word;
    Value:      LongInt;

  end;

procedure DestroyList(List: TList);
var
  i: LongInt;
begin
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;

  List.Free;
end;

function EscherGetBlipSign(Kind: TEscherBlipKind): TEscherBlipSign;
begin
  case Kind of
    EscherBkEMF:  Result := EscherBsEMF;
    EscherBkWMF:  Result := EscherBsWMF;
    EscherBkJPEG: Result := EscherBsJPEG;
    EscherBkPNG:  Result := EscherBsPNG;
    EscherBkDIB:  Result := EscherBsDIB;
    EscherBkTIFF: Result := EscherBsTIFF;
    else          Result := EscherBsUnknown;
  end;
end;

procedure EscherGetBlipHash(out Hash: TEscherBlipHash; Blip: TMemoryStream);
begin
  TCryptoHash.Hash('MD5', Hash, Length(Hash), Blip.Memory^, Blip.Size);
end;

//
// TEscherRec
//

function TEscherRec.GetESize: LongWord;
var
  i: LongInt;
begin
  Result := Size;
  for i := 0 to Records.Count - 1 do
    Inc(Result, 8 + TEscherRec(Records[i]).GetESize);
end;

procedure TEscherRec.Flush(Stream: TStream);
var
  h: TEscherRecHeader;
begin
  //
  // 1. Fixed size header
  //

  FillChar(h, SizeOf(h), 0);

  h.VerInst :=
    (Version and $f) or
    (Instance and $fff) shl 4;

  h.Kind    := Kind;
  h.Length  := GetESize;

  Stream.Write(h, Sizeof(h));

  //
  // 2. Own data
  //

  SaveToStream(Stream);

  //
  // 3. All subrecords
  //

  inherited Flush(Stream);
end;

//
// TEscherStream
//

constructor TEscherStream.Create;
begin
  Records := TList.Create;
end;

destructor TEscherStream.Destroy;
begin
  DestroyList(Records);
  inherited;
end;

procedure TEscherStream.Flush(Stream: TStream);
var
  i: LongInt;
begin
  for i := 0 to Records.Count - 1 do
    TEscherRec(Records[i]).Flush(Stream);
end;

function TEscherStream.Add: TEscherRec;
begin
  Result := TEscherRec.Create;
  Records.Add(Result);
end;

function TEscherStream.AddCont(Instance: Word; Kind: Word): TEscherRec;
begin
  Result := Add($f, Instance, Kind);
end;

function TEscherStream.Add(Version: Byte; Instance: Word; Kind: Word): TEscherRec;
begin
  Result := Add;

  Result.Version  := Version;
  Result.Instance := Instance;
  Result.Kind     := Kind;
end;

procedure TEscherStream.WriteVal(Value: LongWord; Count: LongWord);
begin
  Write(Value, Count);
end;

//
// TEscherBitmap
//

procedure TEscherBitmap.Flush(Stream: TStream);
var
  Tag: Byte;
begin
  Tag := $ff;
  Stream.Write(Tag, 1);
  SaveToStream(Stream);
end;

function TEscherBitmap.GetESize: LongWord;
begin
  Result := 1 + Size;
end;

//
// TEscherMetafile
//

procedure TEscherMetafile.Flush(Stream: TStream);
var
  h: TEscherMetafileHeader;
begin
  // todo -cFeature: TEscherMetafile doesn't support compression.

  FillChar(h, SizeOf(h), 0);

  h.MFSize      := Size;
  h.PackedSize  := Size;
  h.Filter      := $fe;
  h.Compr       := EscherMcNone;

  with h.Size do
  begin
    Width   := MFSize.X;
    Height  := MFSize.Y;
  end;

  with h.Bounds do
  begin
    Left    := Bounds.Left;
    Top     := Bounds.Top;
    Right   := Bounds.Right;
    Bottom  := bounds.Bottom;
  end;

  Stream.Write(h, SizeOf(h));
  SaveToStream(Stream);
end;

function TEscherMetafile.GetESize: LongWord;
begin
  Result := SizeOf(TEscherMetafileHeader) + Size;
end;

//
// TEscherBlip
//

constructor TEscherBlip.Create(Pict: TEscherPicture);
begin
  Self.Pict := Pict;
  EscherGetBlipHash(Hash, Pict);
end;

destructor TEscherBlip.Destroy;
begin
  Pict.Free;
  inherited;
end;

procedure TEscherBlip.Flush(Stream: TEscherStream);
var
  h: TEscherBlipHeader;
  r: TEscherRec;
begin

  { Blip structure is as follows:

      - BSE record
          - BSE header (includes the BLIP signature)
          - Escher record
              - BLIP signature (matches the signature in the BSE header)
              - BLIP header (metafile, bitmap)
              - BLIP data (bitmap pixels, compressed jpeg, etc.) }

  FillChar(h, SizeOf(h), 0);

  h.Win32     := Pict.Kind;
  h.MacOS     := Pict.Kind;
  h.Size      := 8 + SizeOf(Hash) + Pict.GetESize;
  h.RefCount  := RefCount;
  h.Offset    := 0;
  h.Usage     := EscherBuDefault;

  Move(Hash, h.id, 16);

  with Stream.Add do
  begin
    Version   := 2;
    Instance  := Pict.Kind;
    Kind      := EscherRkBse;

    Write(h, SizeOf(h));
    r := Add;

    with r do
    begin
      Version   := 0;
      Instance  := EscherGetBlipSign(Pict.Kind);
      Kind      := EscherRkBlip + Pict.Kind;

      Write(Hash, SizeOf(Hash));
    end;

    Pict.Flush(r);
  end;
end;

//
// TEscherProp
//

constructor TEscherProp.Create(Id: Word);
begin
  Self.Id := Id;
  Blip    := False;
  Complex := False;
  Value   := 0;
end;

//
// TEscherPropList
//

constructor TEscherPropList.Create;
begin
  FS := TList.Create;
end;

destructor TEscherPropList.Destroy;
begin
  DestroyList(FS);
end;

function TEscherPropList.Add(Id: Word): TEscherProp;
var
  i: LongInt;
begin
  Result := TEscherProp.Create(Id);

  if FS.Count = 0 then
  begin
    FS.Add(Result);
    Exit;
  end;

  for i := 0 to FS.Count - 1 do
    if Id < TEscherProp(FS[i]).Id then
      Break;

  FS.Insert(i, Result);
end;

procedure TEscherPropList.Flush(Stream: TEscherStream);
var
  i: LongInt;
  h: TEscherPropHeader;
  p: TEscherProp;
begin
  if FS.Count = 0 then
    Exit;

  with Stream.Add(3, FS.Count, EscherRkOpts) do
    for i := 0 to FS.Count - 1 do
    begin
      p := TEscherProp(FS[i]);

      h.Value := p.Value;
      h.Id    := $3fff and p.Id;

      if p.Blip then
        h.Id := h.Id or $4000;

      if p.Complex then
        h.Id := h.Id or $8000;

      Write(h, SizeOf(h));
    end;
end;

//
// TEscherShapePos
//

procedure TEscherShapePos.Flush(Stream: TEscherStream);
var
  r: TEscherPlacement;
begin
  FillChar(r, SizeOf(r), 0);

  with r do
  begin
    LCol  := Left;
    RCol  := Right;
    TRow  := Top;
    BRow  := Bottom;

    LOff  := LeftOffset;
    ROff  := RightOffset;
    TOff  := TopOffset;
    BOff  := BottomOffset;
  end;

  Stream.Add(0, 0, EscherRkCAnchor).Write(r, SizeOf(r));
end;

//
// TEscherShape
//

constructor TEscherShape.Create;
begin
  Flags := EscherSfShape;
  Pos := TEscherShapePos.Create;
end;

destructor TEscherShape.Destroy;
begin
  Pos.Free;
  inherited;
end;

procedure TEscherShape.Flush(Stream: TEscherStream);
var
  pl:   TEscherPropList;
  spc:  TEscherStream;
begin
  spc := Stream.AddCont(0, EscherRkSpCont);

  with spc do
  begin
    with Add(2, EscherStPictureFrame, EscherRkSp) do
    begin
      WriteVal(Id, 4);
      WriteVal(Flags, 4);
    end;

    pl := TEscherPropList.Create;

    with pl.Add(260) do
    begin
      Blip  := True;
      Value := Image + 1;
    end;

    pl.Flush(spc);
    pl.Free;
    Pos.Flush(spc);
  end;
end;

//
// TEscherGroup
//

constructor TEscherGroup.Create;
begin
  Shapes := TList.Create;
  SId := 1;
end;

destructor TEscherGroup.Destroy;
begin
  DestroyList(Shapes);
end;

function TEscherGroup.GetShape(Index: LongInt): TEscherShape;
begin
  if (Index >= 0) and (Index < Shapes.Count) then
    Result := TEscherShape(Shapes[Index])
  else
    Result := nil;
end;

function TEscherGroup.Count: LongWord;
begin
  Result := Shapes.Count;
end;

function TEscherGroup.Add: TEscherShape;
begin
  Result := TEscherShape.Create;
  Result.Id := GetSId;
  Shapes.Add(Result);
end;

function TEscherGroup.GetSId: LongWord;
begin
  Result := EscherGroupLimit * Id + SId;
  Inc(SId);
end;

function TEscherGroup.GetRId: LongWord;
begin
  Result := EscherGroupLimit * Id;
end;

function TEscherGroup.GetMaxSId: LongWord;
begin
  Result := EscherGroupLimit * Id + SId;
end;

procedure TEscherGroup.Flush(Stream: TEscherStream);
var
  i:  LongInt;
  sc: TEscherStream;

begin
  with Stream.AddCont(0, EscherRkDgCont) do
  begin
    with Add(0, Id, EscherRkDg) do
    begin
      WriteVal(Count + 1, 4);
      WriteVal(Items[Count - 1].Id, 4);
    end;

    sc := AddCont(0, EscherRkSpgrCont);
    with sc do
    begin
      with AddCont(0, EscherRkSpCont) do
      begin
        with Add(1, 0, EscherRkSpgr) do
        begin
          { todo: actually, Spgr contains
            "bounds". The meaning of these "bounds"
            is unknown. MS Excel writes zeros here }

          WriteVal(0, 4); // Left
          WriteVal(0, 4); // Top
          WriteVal(0, 4); // Right
          WriteVal(0, 4); // Bottom
        end;

        with Add(2, EscherStNone, EscherRkSp) do
        begin
          WriteVal(GetRId, 4);
          WriteVal(EscherSfGroup or EscherSfRoot, 4);
        end;
      end;

      for i := 0 to Count - 1 do
        Items[i].Flush(sc);
    end;
  end;
end;

//
// TEscherStorage
//

constructor TEscherStorage.Create;
begin
  Groups  := TList.Create;
  Images  := TList.Create;
  Props   := TEscherPropList.Create;

  with Props do
  begin
    Add(191).Value := $00080008;
    Add(385).Value := $08000041;
    Add(448).Value := $08000040;
  end;
end;

destructor TEscherStorage.Destroy;
begin
  Props.Destroy;
  DestroyList(Images);
  DestroyList(Groups);
end;

function TEscherStorage.AddGroup: TEscherGroup;
begin
  Result := TEscherGroup.Create;
  Groups.Add(Result);
  Result.Id := Groups.Count;
end;

function TEscherStorage.GetGroup(Index: LongInt): TEscherGroup;
begin
  if (Index >= 0) and (Index < Groups.Count) then
    Result := TEscherGroup(Groups[Index])
  else
    Result := nil;
end;

function TEscherStorage.GetImage(Index: LongInt): TEscherBlip;
begin
  if (Index >= 0) and (Index < Images.Count) then
    Result := TEscherBlip(Images[Index])
  else
    Result := nil;
end;

function TEscherStorage.AddImage(Pict: TEscherPicture): TEscherBlip;
label
  done;
var
  i, j: LongInt;
  b: TEscherBlip;
  e: Boolean;
  h1, h2: TEscherBlipHash;
begin
  b := TEscherBlip.Create(Pict);

  for i := 0 to Images.Count - 1 do
  begin
    h1 := Image[i].Hash;
    h2 := b.Hash;

    { This algorithm just compares two hashes
      and when the hashes are identical, it assumes
      that the objects are identical. Normally,
      the function should compare the two objects,
      but it's slow }

    e := True;

    for j := 1 to 16 do
      e := e and (h1[j] = h2[j]);

    if e then
    begin
      b.Free;
      Result := Image[i];
      Exit;
    end;
  end;

  b.Index := Images.Count;
  Images.Add(b);
  Result := b;
end;

function TEscherStorage.Empty;
begin
  Result := Groups.Count = 0;
end;

procedure TEscherStorage.Flush(Stream: TEscherStream);
var
  i:      Longint;
  bs:     TEscherStream;
  cont:   TEscherStream;
  maxId:  LongWord;
  sn:     LongWord;

begin
  maxId := EscherGroupLimit * (1 + Groups.Count) + 1;

  sn := 0;
  for i := 0 to Groups.Count - 1 do
      Inc(sn, Group[i].Count + 1);

  cont := Stream.AddCont(0, EscherRkDggc);
  with cont do
  begin
    with Add(0, 0, EscherRkDgg) do
    begin
      WriteVal(maxId, 4);
      WriteVal(Groups.Count + 1, 4);
      WriteVal(sn, 4);
      WriteVal(Groups.Count, 4);

      for i := 0 to Groups.Count - 1 do
        with Group[i] do
        begin
          WriteVal(Id, 4);
          WriteVal(Count + 1, 4);
        end;
    end;

    bs := AddCont(Images.Count, EscherRkBstore);
    for i := 0 to Images.Count - 1 do
      Image[i].Flush(bs);

    Props.Flush(cont);

    with Add(0, 4, EscherRkSMC) do
    begin
      WriteVal($0800000d, 4);
      WriteVal($0800000c, 4);
      WriteVal($08000017, 4);
      WriteVal($100000f7, 4);
    end;
  end;
end;

end.
