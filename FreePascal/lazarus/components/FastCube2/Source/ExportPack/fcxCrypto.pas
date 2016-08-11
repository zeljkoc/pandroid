
{******************************************}
{                                          }
{             FastReport v4.0              }
{               Cryptography               }
{                                          }
{         Copyright (c) 1998-2011          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxCrypto;

{ Disable overflow checking.
  Cryptography operates in finite fields
  and integer oveflow is meaningless for it. }

{$Q-}

interface

uses
  SysUtils,
  Classes;

{$ELSE}
{$Q-}

interface

uses
  System.SysUtils, System.Classes;
{$ENDIF}

type

  { RC4 cipher }

  TCryptoRC4 = class
  private
    FS: array[0..255] of Byte;
    FI, FJ: Byte;
  public

    { Initializes the RC4 cipher with a specified key }

    procedure Init(const Key; Len: LongInt);

    { Encrypts a data block "in place".
      The RC4 cipher generates a crypto stream (based on the password),
      that is combined with the original data with XOR operation.
      If Data is nil, the crypto stream is generated but is written
      nowhere. }

    procedure Encrypt(Data: Pointer; Size: LongInt);
  end;

  { AES cipher. FIPS 197. }

  TCryptoAESMatrix = array[0..15] of Byte; // matrix 4x4
  TCryptoAESSBox = array[0..255] of Byte;
  TCryptoAESDiffuseRow = array[0..3] of Byte;

  TCryptoAES = class
  private
    FNr: Integer;
    FW: array of TCryptoAESMatrix;

    class procedure InitSBox;
    class function Mul8(a, b: Integer): Integer;
  protected
    class procedure MatrixToData(out Data; const a: TCryptoAESMatrix);
    class procedure DataToMatrix(out a: TCryptoAESMatrix; const Data);

    procedure Sum(var a: TCryptoAESMatrix; const b, c: TCryptoAESMatrix);
    procedure Mov(var a: TCryptoAESMatrix; const b: TCryptoAESMatrix);
    procedure Zero(var a: TCryptoAESMatrix);

    procedure Apply(var a: TCryptoAESMatrix; const Box: TCryptoAESSBox);
    procedure Rotate(var a: TCryptoAESMatrix; Dir: Integer);
    procedure Diffuse(var a: TCryptoAESMatrix; const DiffuseRow: TCryptoAESDiffuseRow);
    procedure ExpandKey(const Key: array of Byte);

    procedure Encrypt(var a: TCryptoAESMatrix); overload; virtual;
    procedure Decrypt(var a: TCryptoAESMatrix); overload; virtual;
  public

    { Key length can be 16, 24 or 32 bytes }

    constructor Create(const Key: array of Byte);

    { Encrypts/decrypts a 16-byte block }

    procedure Encrypt(var Output; const Input); overload; virtual;
    procedure Decrypt(var Output; const Input); overload; virtual;
  end;

  ECryptoAESException = class(Exception);
  ECryptoAESInvalidKeyLength = class(ECryptoAESException);
  ECryptoAESInvalidBlockSize = class(ECryptoAESException);

  { AES CBC mode }

  TCryptoAES_CBC = class(TCryptoAES)
  private
    FC: TCryptoAESMatrix;
  protected
    procedure Encrypt(var a: TCryptoAESMatrix); override;
    procedure Decrypt(var a: TCryptoAESMatrix); override;
  public

    { IV has 16 bytes }

    constructor Create(const Key: array of Byte; const IV);
  end;

  { lag-r CMWC random number generator with base 2**32 }

  TCryptoCMWC = class
  private
    FMultiplier: LongWord;
    FSeed: array of LongWord;
    FCarry: LongWord;
    FNext: LongInt;
  public
    constructor Create(a: Cardinal = 3636507990; r: Cardinal = 1359);
    function Next: LongWord;
  end;

  { Hash-function interface.
    Typical use of a hash function is following:

      h := TCryptoHash.Create('SHA1');
      h.Push(@Message, Length(Message));
      h.GetDigest(@Digest);
      h.Free;

    If several digests are computed with one TCryptoHash
    instance (in order to save resources), then the following
    way can be used:

      h := TCryptoHash.Create('SHA1');

      h.Reset; // optional
      h.Push(@Message1, Length(Message1));
      h.GetDigest(@Digest1);

      h.Reset; // mandatory
      h.Push(@Message2, Length(Message2));
      h.GetDigest(@Digest2);

      h.Reset; // mandatory
      h.Push(@Message2, Length(Message2));
      h.GetDigest(@Digest2);

      h.Free;

    Message can be sent by parts:

      h := TCryptoHash.Create('SHA1');
      h.Push(@MessagePart1, Length(MessagePart1));
      h.Push(@MessagePart2, Length(MessagePart2));
      h.Push(@MessagePart3, Length(MessagePart3));
      h.GetDigest(@Digest);
      h.Free; }

  TCryptoHash = class
  private
    FChunk: array of Byte;
    FLength: Integer;
  protected
    constructor Create(ChunkSize: Integer); overload;

    procedure Process(const Chunk: array of Byte); virtual; abstract;
    procedure Finish(LengthSize: Integer; Straight: Boolean); virtual;
  public

    { Creates an instance of a hash function.
      Acceptable names are:

        MD5
        SHA1
        Whirlpool
        Jenkins

      If the name is not recognised, nil is returned. }

    class function Create(const Name: string): TCryptoHash; overload;

    { Calculates a hash of a text. Note that different hash functions
      produce digests of different lengths. }

    class procedure Hash(const Name: string; var Digest; DigestSize: Integer;
      const Data; Size: Integer); overload;
    class procedure Hash(const Name: string; var Digest; DigestSize: Integer;
      const s: AnsiString); overload;

    { Resets the state of the hash function.
      This method must be called if more than one digest
      is computed with one TCryptoHash instance. }

    procedure Reset; virtual;

    { Sends a piece of message to the hash function }

    procedure Push(b: Byte); overload;
    procedure Push(const Data; Size: Integer); overload;
    procedure Push(Stream: TStream); overload;

    { Returns message digest. This function should not be called several times in a row. }

    procedure GetDigest(out Digest; Size: Integer); virtual; abstract;

    { Returns size of the digest in bytes }

    function DigestSize: Integer; virtual; abstract;

    { Block hashes compute read a message by chunks of fixed size.
      It's most efficient to send a message to a hash function by chunks
      of this size. }

    function ChunkSize: Integer;
  end;

  ECryptoHash = class(Exception);

  ECryptoHashUnknown = class(ECryptoHash)
  public
    constructor Create(Name: string);
  end;

  { MD5 hash. RFC 1321. Digest length: 16 bytes. }

  TCryptoMD5 = class(TCryptoHash)
  private
    FState: array[0..3] of Integer;

    class procedure InitSinTable;
    procedure InitState;
  protected
    procedure Process(const Chunk: array of Byte); override;
  public
    constructor Create;

    procedure GetDigest(out Digest; Size: Integer); override;
    procedure Reset; override;
    function DigestSize: Integer; override;
  end;

  { SHA1 hash. RFC 3174. Digest length: 20 bytes. }

  TCryptoSHA1 = class(TCryptoHash)
  private
    FState: array[0..4] of Integer;

    procedure InitState;
  protected
    procedure Process(const Chunk: array of Byte); override;
  public
    constructor Create;

    procedure GetDigest(out Digest; Size: Integer); override;
    procedure Reset; override;
    function DigestSize: Integer; override;
  end;

  { Whirlpool hash. ISO/IEC 10118-3. Digest length: 64 bytes. }

  TCryptoWhirlpoolMatrix = array[0..7, 0..7] of Byte;

  TCryptoWhirlpool = class(TCryptoHash)
  private
    FState: TCryptoWhirlpoolMatrix;

    procedure ApplySBox(var a: TCryptoWhirlpoolMatrix);
    procedure Rotate(var a: TCryptoWhirlpoolMatrix);
    procedure Diffuse(var a: TCryptoWhirlpoolMatrix);
    procedure Transform(var a: TCryptoWhirlpoolMatrix; const b: TCryptoWhirlpoolMatrix);

    procedure InitState;

    procedure Sum(var a: TCryptoWhirlpoolMatrix; const b, c: TCryptoWhirlpoolMatrix);
    procedure Encrypt(var w: TCryptoWhirlpoolMatrix; const a, b: TCryptoWhirlpoolMatrix);
  protected
    procedure Process(const Chunk: array of Byte); override;
  private
    class procedure InitSBox;
    class procedure InitMul8;

    class function Mul4(a, b: Integer): Integer;
    class function Mul8(a, b: Integer): Integer;
    class function Mul8NoCache(a, b: Integer): Integer;
  public
    constructor Create;

    procedure GetDigest(out Digest; Size: Integer); override;
    procedure Reset; override;
    function DigestSize: Integer; override;
  end;

  { This is a modification of the Jenkins hash.
    It's noncryptographic, but fast. }

  TCryptoJenkins = class(TCryptoHash)
  private
    FState: Cardinal;
  protected
    procedure Finish(LengthSize: Integer; Straight: Boolean); override;
    procedure Process(const Chunk: array of Byte); override;
  public
    constructor Create;

    procedure GetDigest(out Digest; Size: Integer); override;
    procedure Reset; override;
    function DigestSize: Integer; override;
  end;

implementation

{$IFNDEF FMX}
uses
  Math;
{$ELSE}
uses
  System.Math;
{$ENDIF}

{ Delphi4 doesn't allow to put these variables as
  static class fields of corresponding classes. }

var
  AESSBox, AESIBox: TCryptoAESSBox;
  MD5SinTable: array[1..64] of Integer;
  WhirlpoolSBox: array[0..255] of Byte;
  WhirlpoolMul8: array[1..9, 0..255] of Byte;

{ Returns N bytes from Base + Offset }

function GetNBytesAt(const Base; Offset, N: Integer): Integer;
begin
  Assert(SizeOf(Result) >= N);
  Result := 0;
  Move(Pointer(Integer(@Base) + Offset)^, Result, N);
end;

procedure SetNBytesAt(var Base; Offset, N, Value: Integer);
begin
  Assert(SizeOf(Value) >= N);
  Move(Value, Pointer(Integer(@Base) + Offset)^, N);
end;

function Min(a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b
end;

{ Rotates a 32-bit integer by n bits left }

function RotLeft(a, n: Integer): Integer;
var
  p, q: Integer;
begin
  p := Integer(Cardinal(a) shr (32 - n));
  q := a and ((Cardinal(1) shl (32 - n)) - 1);

  Result := (q shl n) xor p;
end;

{ Swap bytes of a 32-bit integer }

function ByteSwap(x: Integer): Integer;
var
  b: array[0..3] of Byte;
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    b[i] := x and $ff;
    x := x shr 8;
  end;

  Result := 0;

  for i := 0 to 3 do
    Result := (Result shl 8) xor b[i];
end;

{ TCryptoCMWC }

constructor TCryptoCMWC.Create(a, r: Cardinal);
var
  t: Extended;
  i: LongInt;
  h: array[0..15] of Byte;
begin
  FMultiplier := a;
  SetLength(FSeed, r);

  { Initialise the first r seed values with some data }

  t := Time;
  TCryptoHash.Hash('MD5', h, Length(h), t, SizeOf(t));

  for i := 0 to Length(FSeed) - 1 do
  begin
    FSeed[i] := 0;
    Move(h[(i mod 4)*4], FSeed[i], 4);
  end;
end;

function TCryptoCMWC.Next: LongWord;
const
  Mask: Cardinal = $ffffffff;
var
  q: Int64;
begin
  q := Int64(FMultiplier)*FSeed[FNext] + FCarry;

  FSeed[FNext] := q and Mask;
  FCarry := (q shr 32) and Mask;
  Result := FSeed[FNext];

  Inc(FNext);

  if FNext = Length(FSeed) then
    FNext := 0;
end;

{ TCryptoRC4 }

procedure TCryptoRC4.Init(const Key; Len: LongInt);
var
  i, j, k: Byte;
  b: Byte;
begin
  for i := 0 to 255 do
    FS[i] := i;

  j := 0;
  k := 0;

  for i := 0 to 255 do
  begin
    j := (Integer(j) + GetNBytesAt(Key, k, 1) + FS[i]) and $ff;
    k := k + 1;

    if k = len then 
      k := 0;

    b := FS[i];
    FS[i] := FS[j];
    FS[j] := b;
  end;

  FI := 0;
  FJ := 0;
end;

procedure TCryptoRC4.Encrypt(Data: Pointer; Size: LongInt);
var
  i: Integer;
  s, h: Byte;
begin
  for i := 0 to Size - 1 do
  begin
    Inc(FI);
    Inc(FJ, FS[FI]);

    s := FS[FJ];
    FS[FJ] := FS[FI];
    FS[FI] := s;

    if Data <> nil then
    begin
      h := FS[(FS[FI] + FS[FJ]) and $ff];
      SetNBytesAt(Data^, i, 1, GetNBytesAt(Data^, i, 1) xor h);
    end;
  end;
end;

{ TCryptoHash }

function TCryptoHash.ChunkSize: Integer;
begin
  Result := Length(FChunk);
end;

class function TCryptoHash.Create(const Name: string): TCryptoHash;
begin
  if Name = 'MD5' then
    Result := TCryptoMD5.Create

  else if Name = 'SHA1' then
    Result := TCryptoSHA1.Create

  else if Name = 'Whirlpool' then
    Result := TCryptoWhirlpool.Create

  else if Name = 'Jenkins' then
    Result := TCryptoJenkins.Create

  else
    Result := nil
end;

class procedure TCryptoHash.Hash(const Name: string; var Digest; DigestSize: Integer;
  const Data; Size: Integer);
var
  h: TCryptoHash;
begin
  h := Create(Name);

  if h = nil then
    raise ECryptoHashUnknown.Create(Name);

  try
    h.Push(Data, Size);
    h.GetDigest(Digest, DigestSize);
  finally
    h.Free;
  end;
end;

constructor TCryptoHash.Create(ChunkSize: Integer);
begin
  SetLength(FChunk, ChunkSize);
end;

class procedure TCryptoHash.Hash(const Name: string; var Digest; DigestSize: Integer;
  const s: AnsiString);
begin
  if s = '' then
    Hash(Name, Digest, DigestSize, nil^, 0)
  else
    Hash(Name, Digest, DigestSize, s[1], Length(s))
end;

procedure TCryptoHash.Push(b: Byte);
begin
  FChunk[FLength mod Length(FChunk)] := b;
  Inc(FLength);

  if FLength mod Length(FChunk) = 0 then
    Process(FChunk);
end;

procedure TCryptoHash.Push(const Data; Size: Integer);
var
  Used, n: Integer;
  Offset: Integer;
begin
  { The following code is equal to:

    for n := 0 to Size - 1 do
      Push(PByte(Integer(Data) + n)^) }

  Used := FLength mod ChunkSize;
  Inc(FLength, Size);
  Offset := 0;

  while Size > 0 do
  begin
    n := Min(Size, ChunkSize - Used);
    Move(Pointer(Integer(@Data) + Offset)^, FChunk[Used], n);

    Inc(Offset, n);
    Inc(Used, n);
    Dec(Size, n);

    if Used = ChunkSize then
    begin
      Process(FChunk);
      Used := 0;
    end;
  end;
end;

procedure TCryptoHash.Reset;
begin
  FLength := 0;
end;

procedure TCryptoHash.Finish(LengthSize: Integer; Straight: Boolean);
var
  i, n, cn: Integer;
begin
  cn := Length(FChunk);
  n := FLength*8;
  Push($80);

  while FLength mod cn <> cn - LengthSize do
    Push(0);

  if Straight then
    for i := cn - LengthSize to cn - 1 do
    begin
      FChunk[i] := n and $ff;
      n := n shr 8;
    end
  else
    for i := cn - 1 downto cn - LengthSize do
    begin
      FChunk[i] := n and $ff;
      n := n shr 8;
    end;

  Process(FChunk);
end;

procedure TCryptoHash.Push(Stream: TStream);
var
  Buffer: array of Byte;
  n: Integer;
begin
  Stream.Position := 0;
  SetLength(Buffer, ChunkSize);

  n := Stream.Read(Buffer[0], Length(Buffer));

  while n > 0 do
  begin
    Push(Buffer[0], n);
    n := Stream.Read(Buffer[0], Length(Buffer));
  end;
end;

{ TCryptoMD5 }

constructor TCryptoMD5.Create;
begin
  inherited Create(64);
  InitState;
end;

procedure TCryptoMD5.InitState;
begin
  FState[0] := ByteSwap(Integer($01234567));
  FState[1] := ByteSwap(Integer($89abcdef));
  FState[2] := ByteSwap(Integer($fedcba98));
  FState[3] := ByteSwap(Integer($76543210));
end;

class procedure TCryptoMD5.InitSinTable;

  function AbsSin(x: Extended): Extended;
  begin
    Result := Sin(x);

    if Result < 0 then
      Result := -Result;
  end;

var
  i: Integer;
begin
  for i := 1 to 64 do
    MD5SinTable[i] := Floor($100000000*AbsSin(i))
end;

procedure TCryptoMD5.Process(const Chunk: array of Byte);

  function X(i: Integer): Integer;
  begin
    Result := GetNBytesAt(Chunk[4*i], 0, 4)
  end;

  function F(i, x, y, z: Integer): Integer;
  begin
    case i of
      00..15: Result := x and y or not x and z;
      16..31: Result := x and z or y and not z;
      32..47: Result := x xor y xor z;
      48..63: Result := y xor (x or not z);

      else Result := 0;
    end;
  end;

const
  G: array[0..63, 0..1] of Integer =
  (
    { Round 1 }

    ( 0,  7),
    ( 1, 12),
    ( 2, 17),
    ( 3, 22),
    ( 4,  7),
    ( 5, 12),
    ( 6, 17),
    ( 7, 22),
    ( 8,  7),
    ( 9, 12),
    (10, 17),
    (11, 22),
    (12,  7),
    (13, 12),
    (14, 17),
    (15, 22),

    { Round 2 }


    ( 1,  5),
    ( 6,  9),
    (11, 14),
    ( 0, 20),
    ( 5,  5),
    (10,  9),
    (15, 14),
    ( 4, 20),
    ( 9,  5),
    (14,  9),
    ( 3, 14),
    ( 8, 20),
    (13,  5),
    ( 2,  9),
    ( 7, 14),
    (12, 20),

    { Round 3 }

    ( 5,  4),
    ( 8, 11),
    (11, 16),
    (14, 23),
    ( 1,  4),
    ( 4, 11),
    ( 7, 16),
    (10, 23),
    (13,  4),
    ( 0, 11),
    ( 3, 16),
    ( 6, 23),
    ( 9,  4),
    (12, 11),
    (15, 16),
    ( 2, 23),

    { Round 4 }

    ( 0,  6),
    ( 7, 10),
    (14, 15),
    ( 5, 21),
    (12,  6),
    ( 3, 10),
    (10, 15),
    ( 1, 21),
    ( 8,  6),
    (15, 10),
    ( 6, 15),
    (13, 21),
    ( 4,  6),
    (11, 10),
    ( 2, 15),
    ( 9, 21)
  );
var
  i, j, k: Integer;
  W, R: array[0..3] of Integer;
begin
  for j := 0 to 3 do
    W[j] := FState[j];

  for i := 0 to 63 do
  begin
    for j := 0 to 3 do
      R[j] := W[(j - i mod 16) and 3];

    k := R[0] + X(G[i, 0]) + MD5SinTable[i + 1] + F(i, R[1], R[2], R[3]);
    W[(0 - i mod 16) and 3] := R[1] + RotLeft(k, G[i, 1]);
  end;

  for j := 0 to 3 do
    Inc(FState[j], W[j]);
end;

procedure TCryptoMD5.Reset;
begin
  inherited;
  InitState;
end;

function TCryptoMD5.DigestSize: Integer;
begin
  Result := 16;
end;

procedure TCryptoMD5.GetDigest(out Digest; Size: Integer);
begin
  Assert(Size = DigestSize);
  Finish(8, True);
  Move(FState, Digest, Size);
end;

{ TCryptoSHA1 }

constructor TCryptoSHA1.Create;
begin
  inherited Create(64);
  InitState;
end;

function TCryptoSHA1.DigestSize: Integer;
begin
  Result := 20;
end;

procedure TCryptoSHA1.GetDigest(out Digest; Size: Integer);
var
  i: Integer;
begin
  Assert(Size = DigestSize);
  Finish(8, False);

  for i := 0 to 4 do
    SetNBytesAt(Digest, i*4, 4, ByteSwap(FState[i]));
end;

procedure TCryptoSHA1.InitState;
begin
  FState[0] := ByteSwap(Integer($01234567));
  FState[1] := ByteSwap(Integer($89abcdef));
  FState[2] := ByteSwap(Integer($fedcba98));
  FState[3] := ByteSwap(Integer($76543210));
  FState[4] := ByteSwap(Integer($f0e1d2c3));
end;

procedure TCryptoSHA1.Process(const Chunk: array of Byte);

  function K(i: Integer): Integer;
  begin
    case i of
      00..19: Result := Integer($5a827999);
      20..39: Result := Integer($6ed9eba1);
      40..59: Result := Integer($8f1bbcdc);
      60..79: Result := Integer($ca62c1d6);

      else Result := 0
    end
  end;

  function F(t, b, c, d: Integer): Integer;
  begin
    case t of
      00..19: Result := b and c or not b and d;
      20..39: Result := b xor c xor d;
      40..59: Result := b and c or b and d or c and d;
      60..79: Result := b xor c xor d;

      else Result := 0
    end
  end;

var
  H: array[0..4] of Integer;

  procedure Shuffle(t: Integer);
  begin
    H[4] := H[3];
    H[3] := H[2];
    H[2] := H[1];
    H[1] := H[0];
    H[0] := t;

    H[2] := RotLeft(H[2], 30);
  end;

var
  i, j: Integer;
  W: array[0..79] of Integer;
begin
  for i := 0 to 15 do
    W[i] := ByteSwap(GetNBytesAt(Chunk[i*4], 0, 4));

  for i := 16 to 79 do
    W[i] := RotLeft(W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16], 1);

  for j := 0 to 4 do
    H[j] := FState[j];

  for i := 0 to 79 do
    Shuffle(RotLeft(H[0], 5) + F(i, H[1], H[2], H[3]) + H[4] + W[i] + K(i));

  for j := 0 to 4 do
    Inc(FState[j], H[j]);
end;

procedure TCryptoSHA1.Reset;
begin
  inherited;
  InitState;
end;

{ TCryptoWhirlpool }

class procedure TCryptoWhirlpool.InitSBox;
const
  R: array[0..15] of Byte = (7, 12, 11, 13, 14, 4, 9, 15, 6, 3, 8, 10, 2, 5, 1, 0);
var
  s, u, v, a: Integer;
  E, I: array[0..15] of Byte;
begin
  E[0] := 1;
  E[15] := 0;

  for s := 1 to 14 do
    E[s] := Mul4($b, E[s - 1]);

  for s := 0 to 15 do
    I[E[s]] := s;

  for s := 0 to 255 do
  begin
    u := s shr 4;
    v := s and $f;

    a := R[E[u] xor I[v]];
    u := E[E[u] xor a];
    v := I[I[v] xor a];

    WhirlpoolSBox[s] := (u shl 4) xor v;
  end;
end;

class function TCryptoWhirlpool.Mul4(a, b: Integer): Integer;
begin
  Result := 0;

  while b <> 0 do
  begin
    if b and 1 = 1 then
      Result := Result xor a;

    b := b shr 1;
    a := a shl 1;

    if a and $10 <> 0 then
      a := a xor $13;
  end;
end;

class function TCryptoWhirlpool.Mul8(a, b: Integer): Integer;
begin
  { If an exception happens here, then Whirlpool is incorrectly initialised.
    Before using Whirlpool, its Mul8 cache must be filled in for all values
    in the diffuse row. }

  Result := WhirlpoolMul8[a, b]
end;

class procedure TCryptoWhirlpool.InitMul8;
var
  a, b: Integer;
begin
  for a := 1 to 9 do
  for b := 0 to 255 do
    WhirlpoolMul8[a, b] := Mul8NoCache(a, b);
end;

class function TCryptoWhirlpool.Mul8NoCache(a, b: Integer): Integer;
begin
  Result := 0;

  while b <> 0 do
  begin
    if b and 1 = 1 then
      Result := Result xor a;

    b := b shr 1;
    a := a shl 1;

    if a and $100 <> 0 then
      a := a xor $11d;
  end;
end;

procedure TCryptoWhirlpool.ApplySBox(var a: TCryptoWhirlpoolMatrix);
var
  i, j: Integer;
begin
  for i := 0 to 7 do
  for j := 0 to 7 do
    a[i, j] := WhirlpoolSBox[a[i, j]]
end;

procedure TCryptoWhirlpool.Rotate(var a: TCryptoWhirlpoolMatrix);
var
  i, j: Integer;
  Col: array[0..7] of Byte;
begin
  for j := 0 to 7 do
  begin
    for i := 0 to 7 do
      Col[i] := a[(i - j) and 7, j];

    for i := 0 to 7 do
      a[i, j] := Col[i];
  end;
end;

procedure TCryptoWhirlpool.Sum(var a: TCryptoWhirlpoolMatrix; const b, c: TCryptoWhirlpoolMatrix);
var
  i, j: Integer;
begin
  for i := 0 to 7 do
  for j := 0 to 7 do
    a[i, j] := b[i, j] xor c[i, j];
end;

procedure TCryptoWhirlpool.Diffuse(var a: TCryptoWhirlpoolMatrix);
const
  DiffuseRow: array[0..7] of Byte = (1, 1, 4, 1, 8, 5, 2, 9);
var
  i, j, k: Integer;
  Row: array[0..7] of Byte;
begin
  for i := 0 to 7 do
  begin
    for j := 0 to 7 do
      Row[j] := 0;

    for j := 0 to 7 do
    for k := 0 to 7 do
      Row[j] := Row[j] xor Mul8(DiffuseRow[(j - k) and 7], a[i, k]);

    for j := 0 to 7 do
      a[i, j] := Row[j];
  end;
end;

function TCryptoWhirlpool.DigestSize: Integer;
begin
  Result := 64;
end;

procedure TCryptoWhirlpool.Encrypt(var w: TCryptoWhirlpoolMatrix; const a, b: TCryptoWhirlpoolMatrix);
var
  c, k: TCryptoWhirlpoolMatrix;
  i, r: Integer;
begin
  Sum(w, a, b);
  Sum(c, c, c);
  Sum(k, a, c);

  for r := 1 to 10 do
  begin
    for i := 0 to 7 do
      c[0, i] := WhirlpoolSBox[8*(r - 1) + i];

    Transform(k, c);
    Transform(w, k);
  end;
end;

procedure TCryptoWhirlpool.Transform(var a: TCryptoWhirlpoolMatrix; const b: TCryptoWhirlpoolMatrix);
begin
  ApplySBox(a);
  Rotate(a);
  Diffuse(a);
  Sum(a, a, b);
end;

procedure TCryptoWhirlpool.InitState;
begin
  Sum(FState, FState, FState);
end;

procedure TCryptoWhirlpool.GetDigest(out Digest; Size: Integer);
var
  i: Integer;
begin
  Assert(Size = DigestSize);
  Finish(32, False);

  for i := 0 to 7 do
    Move(FState[i, 0], Pointer(Integer(@Digest) + i*8)^, 8);
end;

procedure TCryptoWhirlpool.Reset;
begin
  inherited;
  InitState;
end;

procedure TCryptoWhirlpool.Process(const Chunk: array of Byte);
var
  i, j: Integer;
  b, w: TCryptoWhirlpoolMatrix;
begin
  for i := 0 to 7 do
  for j := 0 to 7 do
    b[i, j] := Chunk[i*8 + j];

  Encrypt(w, FState, b);

  Sum(FState, FState, w);
  Sum(FState, FState, b);
end;

constructor TCryptoWhirlpool.Create;
begin
  inherited Create(64);
  InitState;
end;

{ TCryptoAES }

class procedure TCryptoAES.InitSBox;

  function XorBits(b: Byte): Byte;
  var
    i: Integer;
  begin
    Result := 0;

    for i := 0 to 7 do
      Result := Result xor ((b shr i) and 1);
  end;

var
  i, b: Integer;
  m, r, q: Byte;
begin
  for b := 0 to 255 do
  begin
    m := $f8;
    r := 0;

    { Find q•b = 1 }

    if b = 0 then
      q := 0
    else
      for q := 1 to 255 do
        if Mul8(b, q) = 1 then
          Break;

    for i := 0 to 7 do
    begin
      r := (r shl 1) xor XorBits(q and m);
      m := (m shr 1) xor Byte(m shl 7);
    end;

    AESSBox[b] := r xor $63;
    AESIBox[AESSBox[b]] := b;
  end;
end;

class function TCryptoAES.Mul8(a, b: Integer): Integer;
begin
  Result := 0;

  while b <> 0 do
  begin
    if b and 1 <> 0 then
      Result := Result xor a;

    b := b shr 1;
    a := a shl 1;

    if a and $100 <> 0 then
      a := a xor $11b;
  end;
end;

procedure TCryptoAES.Apply(var a: TCryptoAESMatrix; const Box: TCryptoAESSBox);
var
  i: Integer;
begin
  for i := 0 to 15 do
    a[i] := Box[a[i]]
end;

procedure TCryptoAES.Diffuse(var a: TCryptoAESMatrix; const DiffuseRow: TCryptoAESDiffuseRow);
var
  i, j, k: Integer;
  Col: array[0..3] of Byte;
begin
  for j := 0 to 3 do
  begin
    for i := 0 to 3 do
      Col[i] := 0;

    for i := 0 to 3 do
    for k := 0 to 3 do
      Col[i] := Col[i] xor Mul8(DiffuseRow[(k - i) and 3], a[k*4 + j]);

    for i := 0 to 3 do
      a[i*4 + j] := Col[i];
  end;
end;

procedure TCryptoAES.ExpandKey(const Key: array of Byte);
var
  i, j, Nk: Integer;
  c: Byte; // 2^(i/Nk - 1) in GF(2^8)
  a: array of array[0..3] of Byte;
  t: array[0..3] of Byte;
begin
  Nk := Length(Key) div 4;
  SetLength(a, 4*(FNr + 1));

  for i := 0 to Nk - 1 do
  for j := 0 to 3 do
    a[i][j] := Key[i*4 + j];

  c := 1;

  for i := Nk to 4*(FNr + 1) - 1 do
  begin
    for j := 0 to 3 do
      t[j] := a[i - 1][j];

    if i mod Nk = 0 then
    begin
      for j := 0 to 3 do
        t[j] := AESSBox[a[i - 1][(j + 1) and 3]];

      t[0] := t[0] xor c;
      c := Mul8(c, 2);
    end;

    if (i mod Nk = 4) and (Nk > 6) then
      for j := 0 to 3 do
        t[j] := AESSBox[a[i - 1][j]];

    for j := 0 to 3 do
      a[i][j] := t[j] xor a[i - Nk][j];
  end;

  SetLength(FW, FNr + 1);

  for i := 0 to 4*(FNr + 1) - 1 do
  for j := 0 to 3 do
    FW[i div 4][j*4 + i mod 4] := a[i][j]
end;

procedure TCryptoAES.Rotate(var a: TCryptoAESMatrix; Dir: Integer);
var
  i, j: Integer;
  r: array[0..3] of Byte;
begin
  for i := 0 to 3 do
  begin
    for j := 0 to 3 do
      r[j] := a[i*4 + (j - Dir*i) mod 4];

    for j := 0 to 3 do
      a[i*4 + j] := r[j];
  end;
end;

procedure TCryptoAES.Sum(var a: TCryptoAESMatrix; const b, c: TCryptoAESMatrix);
var
  i: Integer;
begin
  for i := 0 to 15 do
    a[i] := b[i] xor c[i]
end;

procedure TCryptoAES.Mov(var a: TCryptoAESMatrix; const b: TCryptoAESMatrix);
var
  i: Integer;
begin
  for i := 0 to 15 do
    a[i] := b[i]
end;

procedure TCryptoAES.Zero(var a: TCryptoAESMatrix);
var
  i: Integer;
begin
  for i := 0 to 15 do
    a[i] := 0
end;

procedure TCryptoAES.Encrypt(var a: TCryptoAESMatrix);
const
  R: TCryptoAESDiffuseRow = (2, 3, 1, 1);
var
  i: Integer;
begin
  Sum(a, a, FW[0]);

  for i := 1 to FNr - 1 do
  begin
    Apply(a, AESSBox);
    Rotate(a, -1);
    Diffuse(a, R);
    Sum(a, a, FW[i]);
  end;

  Apply(a, AESSBox);
  Rotate(a, -1);
  Sum(a, a, FW[FNr]);
end;

procedure TCryptoAES.Decrypt(var a: TCryptoAESMatrix);
const
  R: TCryptoAESDiffuseRow = ($e, $b, $d, 9);
var
  i: Integer;
begin
  Sum(a, a, FW[FNr]);

  for i := FNr - 1 downto 1 do
  begin
    Rotate(a, +1);
    Apply(a, AESIBox);
    Sum(a, a, FW[i]);
    Diffuse(a, R);
  end;

  Rotate(a, +1);
  Apply(a, AESIBox);
  Sum(a, a, FW[0]);
end;

class procedure TCryptoAES.MatrixToData(out Data; const a: TCryptoAESMatrix);
var
  i, j: Integer;
begin
  for i := 0 to 3 do
  for j := 0 to 3 do
    SetNBytesAt(Data, j*4 + i, 1, a[i*4 + j])
end;

class procedure TCryptoAES.DataToMatrix(out a: TCryptoAESMatrix; const Data);
var
  i, j: Integer;
begin
  for i := 0 to 3 do
  for j := 0 to 3 do
    a[i*4 + j] := GetNBytesAt(Data, j*4 + i, 1)
end;

procedure TCryptoAES.Decrypt(var Output; const Input);
var
  a: TCryptoAESMatrix;
begin
  DataToMatrix(a, Input);
  Decrypt(a);
  MatrixToData(Output, a);
end;

procedure TCryptoAES.Encrypt(var Output; const Input);
var
  a: TCryptoAESMatrix;
begin
  DataToMatrix(a, Input);
  Encrypt(a);
  MatrixToData(Output, a);
end;

constructor TCryptoAES.Create(const Key: array of Byte);
begin
  case Length(Key) of
    4*4: FNr := 10;
    4*6: FNr := 12;
    4*8: FNr := 14;
    else raise ECryptoAESInvalidKeyLength.CreateFmt('AES-%d is undefined',
      [Length(Key)*8]);
  end;

  ExpandKey(Key);
end;

{ TCryptoAES_CBC }

constructor TCryptoAES_CBC.Create(const Key: array of Byte; const IV);
begin
  inherited Create(Key);
  DataToMatrix(FC, IV);
end;

procedure TCryptoAES_CBC.Encrypt(var a: TCryptoAESMatrix);
begin
  Sum(a, a, FC);
  inherited Encrypt(a);
  Mov(FC, a);
end;

procedure TCryptoAES_CBC.Decrypt(var a: TCryptoAESMatrix);
var
  s: TCryptoAESMatrix;
begin
  Mov(s, FC);
  Mov(FC, a);
  inherited Decrypt(a);
  Sum(a, a, s);
end;

{ ECryptoHashUnknown }

constructor ECryptoHashUnknown.Create(Name: string);
begin
  CreateFmt('Hash function %s is unknown', [Name]);
end;

{ TCryptoJenkins }

constructor TCryptoJenkins.Create;
begin
  inherited Create(64)
end;

function TCryptoJenkins.DigestSize: Integer;
begin
  Result := 4
end;

procedure TCryptoJenkins.Finish(LengthSize: Integer; Straight: Boolean);
var
  h: Cardinal;
begin
  inherited;

  h := FState;

  h := h + (h shl 3);
  h := h xor (h shr 11);
  h := h + (h shl 15);

  FState := h;
end;

procedure TCryptoJenkins.GetDigest(out Digest; Size: Integer);
begin
  Assert(Size = DigestSize);
  Finish(ChunkSize div 2, True);
  Move(FState, Digest, Size);
end;

procedure TCryptoJenkins.Process(const Chunk: array of Byte);
var
  h: Cardinal;
  i: Integer;
begin
  h := FState;

  for i := 0 to ChunkSize - 1 do
  begin
    h := h + Chunk[i];
    h := h + (h shl 10);
    h := h xor (h shr 6);
  end;

  FState := h;
end;

procedure TCryptoJenkins.Reset;
begin
  inherited;
  FState := 0;
end;

initialization

TCryptoMD5.InitSinTable;
TCryptoWhirlpool.InitSBox;
TCryptoWhirlpool.InitMul8;
TCryptoAES.InitSBox;

end.
