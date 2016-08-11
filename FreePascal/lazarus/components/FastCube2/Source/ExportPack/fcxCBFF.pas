
{******************************************}
{                                          }
{             FastReport v4.0              }
{             CBFF Writing API             }
{                                          }
{         Copyright (c) 1998-2010          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

{ This module provides API for writing CBFF
  documents. The format is documented in
  MSDN [MS-CFB] section. }

//VCL uses section
{$IFNDEF FMX}
unit fcxCBFF;

interface

{$include fcx.inc}

uses
  Classes, fcxStorage, SysUtils;
{$ELSE}
interface

{$include fcx.inc}

uses
  System.Classes, System.SysUtils,
  FMX.fcxStorage;
{$ENDIF}
type

  //
  // SAT writer.
  // Writes data streams with corresponding SAT table.
  //

  TCbffStream = class
  public

    Stream: TBlockStream;
    SAT:    TBlockStream;

    constructor Create; overload;
    constructor Create(SecShift: LongWord); overload;
    destructor Destroy; override;

    //
    // Writes a data block, writes a sequence of secIds and
    // returns the first secId.
    //

    function WriteStream(Data: Pointer; Size: LongWord): LongInt;

  end;

  //
  // CBFF directory.
  // Actually, a directory is a named data stream.
  //

  TCbffDirKind = (
    dkEmpty,
    dkStorage,
    dkStream,
    dkLock,
    dkProp,
    dkRoot);

  TCbffNodeColor = (
    ncRed,
    ncBlack);

  TCbffDir = class
  public

    Name:     WideString;       // Up to 31 characters
    Stream:   TBlockStream;     // Data stream
    Childs:   TObjList;         // List of TCbffDir
    Right:    TCbffDir;         // Right sibling
    Left:     TCbffDir;         // Left sibling
    Parent:   TCbffDir;         // Parent of this directory
    IsBlack:  Boolean;          // Node color in red-black tree
    Kind:     TCbffDirKind;

    SecID:    LongInt;          // SecID of the stream. For internal use.
    LeftID:   LongInt;          // DirID if the left sibling. For internal use.
    RightID:  LongInt;          // DirID if the right sibling. For internal use.
    ChildID:  LongInt;          // DirID if a child. For internal use.
    SelfID:   LongInt;          // DirID of itself. For internal use.

    constructor Create;
    destructor Destroy; override;
    procedure Flush(Stream: TStream);

    { Adds a new named child and returns that child. }

    function Add(const Name: WideString): TCbffDir;

  end;

  //
  // CBFF document.
  // Represents a CBFF document.
  //

  TCbffDocument = class
  public

    Root:           TCbffDir;     // Root directory entry
    SecShift:       LongInt;      // Sector size is 2**SecShift
    ShortSecShift:  LongInt;      // Short sector size is 2**ShortSecShift
    MinStreamSize:  LongWord;     // Minimal size of a standard stream

    constructor Create;
    destructor Destroy; override;

    //
    // CBFF document serializing
    //

    procedure Flush(Stream: TStream);

  end;

  //
  // CBFF file header
  // 512 bytes
  //

  TCbffHeader = packed record
    Sign:     array[1..8] of Byte;  // Signature: D0 CF 11 E0 A1 B1 1A E1
    Id:       array[1..8] of Word;
    Rev:      Word;                 // Revision: 003E
    Ver:      Word;                 // Version: 0003
    Order:    Word;                 // Byte order
    Sec:      Word;                 // Sector size is 2**Sec bytes
    SSec:     Word;                 // Short sector size is 2**SSec bytes
    NA1:      array[1..5] of Word;
    nSAT:     LongWord;             // Count of sectors used for SAT
    Dir:      LongInt;              // Directory stream
    NA2:      LongWord;
    MinSize:  LongWord;             // Minimal size of a standard stream
    SSAT:     LongInt;              // SSAT stream
    nSSAT:    LongWord;             // Count of sectors used for SSAT
    MSAT:     LongInt;              // MSAT stream
    nMSAT:    LongWord;             // Count of sectors used for MSAT
    sMSAT:    array[1..109] of LongInt; // First 109 MSAT SecID values
  end;

  PCbffHeader = ^TCbffHeader;

  //
  // Directory Entry
  // 128 bytes
  //

  TCbffDirEntry = packed record
    Name:     array[1..32] of WideChar;// Unicode zero-ended name
    NameSize: Word;                 // Count of bytes occupied by Name
    Kind:     Byte;                 // Directory type, see TCbffDirKind
    Color:    Byte;                 // Node color, see TCbffNodeColor
    Left:     LongInt;              // Left sibling node
    Right:    LongInt;              // Right sibling node
    Child:    LongInt;              // One of child nodes
    Id:       array[1..8] of Word;
    Flags:    LongWord;
    Creation: array[1..2] of Cardinal; // Creation time
    LastMod:  array[1..2] of Cardinal; // Last modification time
    Stream:   LongInt;              // First SecID of the data stream
    Size:     LongWord;             // Size of the stream
    NA1:      LongWord;
  end;

  PCbffDirEntry = ^TCbffDirEntry;

const

  //
  // Special SecID values.
  //

  siFree  = -1;                     // SecID is not used
  siEOC   = -2;                     // This SecID terminates a chain of SecIDs
  siSAT   = -3;                     // Sector is used by SAT
  siMSAT  = -4;                     // Sector is used by MSAT

implementation

{ TCbffDir }

constructor TCbffDir.Create;
begin
  LeftID  := -1;
  RightID := -1;
  ChildID := -1;
  SelfID  := -1;
  SecID   := LongInt(siEOC);
  Name    := '';
  Stream  := TBlockStream.Create;
  Childs  := TObjList.Create;
  IsBlack := True;
end;

destructor TCbffDir.Destroy;
begin
  Childs.Free;
  Stream.Free;
end;

procedure TCbffDir.Flush(Stream: TStream);
var
  d: TCbffDirEntry;
  i, n: LongInt;
begin
  FillChar(d, SizeOf(d), 0);

  n := Length(Name);
  if n > 31 then
    n := 31;

  for i := 1 to n do
    d.Name[i] := Name[i];

  d.Name[n + 1] := #0;

  d.NameSize := (n + 1)*2;
  d.Kind := Byte(Kind);
  d.Color := Byte(IsBlack);
  d.Left  := LeftID;
  d.Right := RightID;
  d.Child := ChildID;
  d.Stream:= SecID;

  if Parent = nil then
    d.Kind := Byte(dkRoot)
  else if Childs.Count = 0 then
  begin
    if Self.Stream.Size = 0 then
      d.Kind := Byte(dkEmpty)
    else
      d.Kind := Byte(dkStream);
  end
  else
    d.Kind := Byte(dkStorage);

  d.Size := Self.Stream.Size;
  Stream.Write(d, SizeOf(d));
end;

function TCbffDir.Add(const Name: WideString): TCbffDir;
var
  d: TCbffDir;
begin
  d := TCbffDir.Create;
  Result := d;

  d.Name := Name;
  d.Parent := Self;

  { Note, that a single reference is
    added: from the last node to the new node.
    The back reference (from the new node to
    the last node) is not added. Such behaviour
    simplifies serialization to a CBFF document. }

  if Childs.Count > 0 then
    with TCbffDir(Childs.Last) do
      Right := d;

  Childs.Add(d);
end;

{ TCbffSATWriter }

constructor TCbffStream.Create;
begin
  Stream  := TBlockStream.Create;
  SAT     := TBlockStream.Create;
end;

constructor TCbffStream.Create(SecShift: LongWord);
begin
  Stream  := TBlockStream.Create(SecShift);
  SAT     := TBlockStream.Create;
end;

destructor TCbffStream.Destroy;
begin
  Stream.Free;
  SAT.Free;
end;

function TCbffStream.WriteStream(Data: Pointer; Size: LongWord): LongInt;
var
  s1, s2: LongInt;
begin
  s1 := Stream.CurrentBlock;
  Stream.Write(Data^, Size);
  Stream.EndBlock($ff);
  s2 := Stream.CurrentBlock;

  Result := SAT.Size shr 2;
  while s1 + 1 < s2 do
  begin
    Inc(s1);
    SAT.Write(s1, 4);
  end;

  SAT.Imm(siEOC, 4);
end;

{ TCbffDocument }

constructor TCbffDocument.Create;
begin
  Root := TCbffDir.Create;
  with Root do
  begin
    Name    := 'Root Entry';
    IsBlack := True;
  end;

  SecShift      := 9;           // Sector size is 2**9 = 512 bytes
  ShortSecShift := 6;           // Short sector size is 2**6 = 64 bytes
  MinStreamSize := 4096;        // Minimal size of a standard stream is 4KB
end;

destructor TCbffDocument.Destroy;
begin
  Root.Free;
end;

procedure TCbffDocument.Flush(Stream: TStream);

  function GetDir(Base: Pointer; DirID: LongWord): PCbffDirEntry;
  begin
    Result := PCbffDirEntry(LongWord(Base) + SizeOf(TCbffDirEntry) * DirID);
  end;

  { Pushes a specified node and all its subnodes
    to a plain list. }

  procedure Flatten(d: TCbffDir; list: TList);
  var
    i: LongInt;
  begin
    d.SelfID := list.Add(d);
    for i := 0 to d.Childs.Count - 1 do
      Flatten(TCbffDir(d.Childs[i]), list);
  end;

  { Returns a count of sectors that a stream
    would occupy. The size of a sector is
    equal to 2**SecShift bytes. }

  function GetSecCount(StreamSize, SecShift: LongInt): LongInt;
  begin
    Result := StreamSize shr SecShift;
    if (StreamSize and (1 shl SecShift - 1)) <> 0 then
      Inc(Result);
  end;

  { Returns a count of sectors that a MSAT
    table would occupy if it had n SecIDs. }

  function GetMsatSecCount(n: LongInt): LongInt;
  var
    m: LongInt;
  begin
    m := 1 shl (SecShift - 2) - 1;
    Result := n div m;
    if n mod m <> 0 then
      Inc(Result);
  end;

  { Returns count of bytes that a SAT table
    would occupy if had SatData bytes containing
    SecIDs for data streams and SatSelf bytes
    containing SecIDs for the SAT itself. }

  function GetSatSize(SatData, SatSelf: LongInt): LongInt;
  var
    Sat: LongInt;
  begin
    Sat := GetSecCount(SatData + SatSelf, SecShift);

    if Sat > 109 then
      Inc(Sat, GetMsatSecCount(Sat - 109));

    { must be Sat * 4 >= SatSelf }

    if Sat * 4 = SatSelf then
      Result := SatData + SatSelf
    else
      Result := GetSatSize(SatData, Sat * 4);
  end;

  { Appends a SAT table with SecIDs for the
    SAT itself and for the MSAT. The routine
    assumes that SAT is written first, MSAT
    is written second. }

  procedure AdjustSat(Sat: TBlockStream);
  var
    SatSize, SatCount, MsatCount, i: LongInt;
  begin
    SatSize := GetSatSize(LongInt(Sat.Size), 0);
    if SatSize = Sat.Size then Exit;

    SatCount := GetSecCount(SatSize, SecShift);

    if SatCount > 109 then
      MsatCount := GetMsatSecCount(SatCount - 109)
    else
      MsatCount := 0;

    for i := 1 to SatCount do
      Sat.Imm(siSAT, 4);

    for i := 1 to MsatCount do
      Sat.Imm(siMSAT, 4);
  end;

const
  Signature: array[1..8] of Byte = ($d0, $cf, $11, $e0, $a1, $b1, $1a, $e1);

var
  ms:       TCbffStream; // Main stream (normal sectors)
  ss:       TCbffStream; // Container for short streams (short sectors)
  ds:       TBlockStream;   // Directory entries
  d:        TCbffDir;
  dirList:  TList;          // List of TCbffDir that contains all streams.
  i, n, s:  LongInt;
  x:        PLongInt;
  h:        TCbffHeader;    // The CBFF document header (512 bytes)
  dh:       PCbffDirEntry;
  ts:       TCbffStream; // Target stream (either ms or ss)
  ssat:     LongInt;        // SecID of the first SSAT sector
  nssat:    LongInt;        // Count of sectors occupied by SSAT
  nsecid:   LongInt;        // Count of secID values that can be written in a sector

begin

  { The contents of the generated CBFF document
    are written in the following order:

      - CBFF header     - 512 bytes
      - Normal streams  - streams of normal sized sectors
      - SSC             - normal stream that contains short streams
      - Descriptors     - 128-byte headers for all streams
      - SSAT            - sectors table for short streams
      - SAT             - sectors table for all streams
      - MSAT            - tail of sectors table for SAT }

  ms := TCbffStream.Create(SecShift);
  ss := TCbffStream.Create(ShortSecShift);
  ds := TBlockStream.Create(SecShift);

  FillChar(h, SizeOf(h), 0);
  Move(Signature, h.Sign, 8);

  h.Rev     := $3e;
  h.Ver     := 3;
  h.Order   := $fffe;
  h.Sec     := SecShift;
  h.SSec    := ShortSecShift;
  h.MinSize := MinStreamSize;

  { Each stream has a 128-byte descriptor
    with information about this stream.
    The first step is to put all descriptors
    to a list. CBFF requires this list to
    occupy an integer number of sectors, i.e.
    the last last sector may be padded with
    a few stub descriptors. }

  dirList := TList.Create;
  Flatten(Root, dirList);

  for i := 0 to dirList.Count - 1 do
  with TCbffDir(dirList[i]) do
  begin
    if Right <> nil then
      Right.LeftID := SelfID;

    if Left <> nil then
      Left.RightID := SelfID;

    if Parent <> nil then
      Parent.ChildID := SelfID;
  end;

  for i := 0 to dirList.Count - 1 do
  begin
    d := TCbffDir(dirList[i]);

    { At this point, SecID (first sector of the stream)
      is unknown. It will be updated later. }

    d.Flush(ds);
  end;

  i := ds.Size and (1 shl SecShift - 1);
  if i > 0 then
    i := 1 shl SecShift - i;

  i := i shr 7;
  d := TCbffDir.Create;

  while i > 0 do
  begin
    Dec(i);
    d.Name := 'Stub' + IntToStr(i);
    d.Flush(ds);
  end;

  d.Free;

  { Write data streams.

    The 0-th data stream is the root stream.
    It points to the container of short streams,
    which size is unknown at the moment.
    Each stream can be written to one of two
    containers:

      - Main stream (ms) - consists of normal sectors
      - Short stream (ss) - consists of short sectors }

  for i := 1 to dirList.Count - 1 do
  begin
    d := TCbffDir(dirList[i]);
    if d.Stream = nil then
      Continue;

    if Cardinal(d.Stream.Size) < MinStreamSize then
      ts := ss
    else
      ts := ms;

    d.SecID := ts.WriteStream(d.Stream.Memory, d.Stream.Size);
  end;

  { Write SSC (the container of short streams).
    The root stream points to this stream. }

  if ss.Stream.Size > 0 then
    Root.SecID := ms.WriteStream(ss.Stream.Memory, ss.Stream.Size);

  { Adjust directory entries.

    The directory entries have already been written, but
    a few fields of them are known only know. These fields
    must be updated. }

  dh := GetDir(ds.Memory, 0);
  dh^.Stream  := Root.SecID;
  dh^.Size    := ss.Stream.Size;

  for i := 1 to dirList.Count - 1 do
  begin
    d := TCbffDir(dirList[i]);
    dh := GetDir(ds.Memory, i);

    if d.Stream <> nil then
    begin
      dh^.Stream  := d.SecID;
      dh^.Size    := d.Stream.Size;
    end
    else
    begin
      dh^.Stream  := 0;
      dh^.Size    := 0;
    end;
  end;

  { Write the list of descriptors of streams. }

  h.Dir := ms.WriteStream(ds.Memory, ds.Size);

  { Write SSAT: sector allocation table for the container
    of short streams. }

  if ss.SAT.Size > 0 then
  begin
    h.SSAT := ms.WriteStream(ss.SAT.Memory, ss.SAT.Size);
    h.nSSAT := ss.SAT.Size shr SecShift;
    if (ss.SAT.Size and (1 shl SecShift - 1)) <> 0 then
      Inc(h.nSSAT);
  end
  else
  begin
    h.SSAT := LongInt(siEOC);
    h.nSSAT := 0;
  end;

  { Currently, SAT contains SecIDs for data
    streams only. SAT itself and MSAT are not
    data streams, but SAT and MSAT will be
    written to the CBFF document and will occupy
    a few sectors. SAT must contain SecIDs for
    all sectors in the document. }

  AdjustSat(ms.SAT);

  { Write SAT: sector allocation table for all streams.
    The SAT occupies sectors: ssat..ssat + nssat - 1. }

  nssat := ms.SAT.Size shr SecShift;
  if (ms.SAT.Size and (1 shl SecShift - 1)) <> 0 then
    Inc(nssat);

  ssat := ms.Stream.CurrentBlock;
  ms.Stream.Write(ms.SAT.Memory^, ms.SAT.Size);
  ms.Stream.EndBlock($ff);
  h.nSAT := nssat;

  { Write MSAT: sector allocation table for the SAT.

    The first part of MSAT is placed in the CBFF header.
    If MSAT doesn't fit to the header, it's tail is
    written in separate sectors. }

  for i := 1 to 109 do
    h.sMSAT[i] := LongInt(siFree);

  i := nssat;
  if i > 109 then i := 109;
  while i > 0 do
  begin
    h.sMSAT[i] := ssat + i - 1;
    Dec(i);
  end;

  h.MSAT := LongInt(siEOC);

  if nssat > 109 then
  begin
    nsecid := 1 shl (SecShift - 2);
    i := nssat - 109;
    h.nMSAT := i div nsecid;

    if i mod nsecid <> 0 then
      Inc(h.nMSAT);

    h.MSAT := ms.Stream.Size shr SecShift;

    i := ssat + 109;
    n := nsecid;
    x := nil;

    while i < ssat + nssat do
    begin
      if n = 1 then
        x := PLongInt(ms.Stream.Imm(0, 4));

      if (n = nsecid) and (x <> nil) then
        x^ := ms.Stream.CurrentBlock;

      ms.Stream.Write(i, 4);
      Dec(n);
      Inc(i);

      if n = 0 then
        n := nsecid;
    end;

    i := LongInt(siFree);
    s := ms.Stream.CurrentBlock;

    with ms.Stream do
      while s = CurrentBlock do
        Write(i, 4);
  end;

  { Write the CBFF header and the contents of the CBFF
    document. }

  with Stream do
  begin
    Write(h, SizeOf(h));
    Write(ms.Stream.Memory^, ms.Stream.Size);
  end;

  { Release memory. }

  ms.Free;
  ds.Free;
  ss.Free;
  dirList.Free;
end;

end.
