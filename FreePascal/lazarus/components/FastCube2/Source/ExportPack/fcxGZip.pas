
{******************************************}
{                                          }
{             FastReport v4.0              }
{         GZIP compress/decompress         }
{                                          }
{          Copyright (c) 2004-2008         }
{          by Alexander Fediachov,         }
{             Fast Reports, Inc.           }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}

unit fcxGZip;

interface

{$I fcx.inc}

uses
  {$ifndef fpc}
  ZLib,
  {$else}
  zbase, zinflate, zstream,
  {$endif}
  SysUtils, Classes,
  fcxUtils;
{$ELSE}
interface

{$I fcx.inc}

uses
  // RTL
  System.SysUtils, System.Classes, System.ZLib,
  // FC FMX
  FMX.fcxUtils;
{$ENDIF}

type
  TfcxCompressionLevel = (gzNone, gzFastest, gzDefault, gzMax);

procedure fcxCompressStream(Source, Dest: TStream;
  Compression: TfcxCompressionLevel = gzDefault; {$IFDEF Delphi12}FileNameW{$ELSE}FileName{$ENDIF}: String = '');
function fcxDecompressStream(Source, Dest: TStream): AnsiString;
procedure fcxDeflateStream(Source, Dest: TStream;
  Compression: TfcxCompressionLevel = gzDefault);
procedure fcxInflateStream(Source, Dest: TStream);


implementation

{$ifdef fpc}
procedure DecompressBuf(const InBuf: Pointer; InBytes: Integer;
  OutEstimate: Integer; out OutBuf: Pointer; out OutBytes: Integer);
var
  Stream: Z_Stream;
  P: Pointer;
  ReadChunk: Integer;
  err: smallint;
begin
  ReadChunk := InBytes * 3;
  if OutEstimate = 0 then
    OutBytes := ReadChunk
  else
    OutBytes := OutEstimate;
  GetMem(OutBuf, OutBytes);
  try
    err := inflateInit(Stream);
    if err <> Z_OK then
      raise Ecompressionerror.create(zerror(err));
    Stream.next_in := InBuf;
    Stream.avail_in := InBytes;
    Stream.next_out := OutBuf;
    Stream.avail_out := OutBytes;
    try
      while true do
      begin
        err := inflate(Stream, Z_FINISH);
        if err = Z_STREAM_END then
          break;
        if err <> Z_OK then
          raise Ecompressionerror.create(zerror(err));
        P := OutBuf;
        Inc(OutBytes, ReadChunk);
        ReallocMem(OutBuf, OutBytes);
        Stream.next_out := Stream.next_out + PtrInt(OutBuf) - PtrInt(P);
        Stream.avail_out := ReadChunk;
      end;
    finally
      err := inflateEnd(Stream);
      if err <> Z_OK then
        raise Ecompressionerror.create(zerror(err));
    end;
    ReallocMem(OutBuf, Stream.total_out);
    OutBytes := Stream.total_out;
  except
    FreeMem(OutBuf);
    raise
  end;
end;
{$endif}

procedure fcxCompressStream(Source, Dest: TStream;
  Compression: TfcxCompressionLevel = gzDefault; {$IFDEF Delphi12}FileNameW{$ELSE}FileName{$ENDIF}: String = '');
var
  header: array [0..3] of Byte;
  Compressor: TCompressionStream;
  Size: Cardinal;
  CRC: Cardinal;
  {$IFDEF Delphi12}
  FileName: AnsiString;
  {$ENDIF}
begin
  CRC := fcxStreamCRC32(Source);
  Size := Source.Size;
  {$IFDEF Delphi12}
  FileName := AnsiString(FileNameW);
  {$ENDIF}
  if FileName = '' then
    FileName := '1';
  FileName := FileName + #0;

  // put gzip header
  header[0] := $1f; // ID1 (IDentification 1)
  header[1] := $8b; // ID2 (IDentification 2)
  header[2] := $8;  // CM (Compression Method) CM = 8 denotes the "deflate"
  header[3] := $8;  // FLG (FLaGs) bit 3   FNAME
  Dest.Write(header, 4);

  // reserve 4 bytes in MTIME field
  Dest.Write(header, 4);

  header[0] := 0; // XFL (eXtra FLags) XFL = 2 - compressor used maximum compression
  header[1] := 0; // OS (Operating System) 0 - FAT filesystem (MS-DOS, OS/2, NT/Win32)
  Dest.Write(header, 2);

  // original file name, zero-terminated
  Dest.Write(FileName[1], Length(FileName));

  // seek back to skip 2 bytes zlib header
  Dest.Seek(-2, soFromCurrent);

  // put compressed data
  Compressor := TCompressionStream.Create(TCompressionLevel(Compression), Dest);
  try
    Compressor.CopyFrom(Source, 0);
  finally
    Compressor.Free;
  end;

  // get adler32 checksum
  Dest.Seek(-4, soFromEnd);
  Dest.Read(header, 4);
  // write it to the header (to MTIME field)
  Dest.Position := 4;
  Dest.Write(header, 4);

  // restore original file name (it was corrupted by zlib header)
  Dest.Seek(2, soFromCurrent);
  Dest.Write(FileName[1], Length(FileName));

  // put crc32 and length
  Dest.Seek(-4, soFromEnd);
  Dest.Write(CRC, 4);
  Dest.Write(Size, 4);
end;

function fcxDecompressStream(Source, Dest: TStream): AnsiString;
var
  s: AnsiString;
  header: array [0..3] of byte;
  adler32: Integer;
  FTempStream: TMemoryStream;
  UnknownPtr: Pointer;
  NewSize: Integer;
begin
  s := '';

  // read gzip header
  Source.Read(header, 4);
  if (header[0] = $1f) and (header[1] = $8b) and (header[2] = $8) then
  begin
    Source.Read(adler32, 4);
    Source.Read(header, 2);
    if (header[3] and $8) <> 0 then
    begin
      Source.Read(header, 1);
      while header[0] <> 0 do
      begin
        s := s + AnsiChar(Char(header[0]));
        Source.Read(header, 1);
      end;
    end;
  end;

  FTempStream := TMemoryStream.Create;
  try
    // put zlib header
    s := #$78#$DA;
    FTempStream.Write(s[1], 2);
    // put compressed data, skip gzip's crc32 and filelength
    FTempStream.CopyFrom(Source, Source.Size - Source.Position - 8);
    // put adler32
    FTempStream.Write(adler32, 4);

    // uncompress data and save it to the Dest
    {$IFDEF DELPHI12}
    ZDeCompress(FTempStream.Memory, FTempStream.Size, UnknownPtr, NewSize);
    {$ELSE}
    DecompressBuf(FTempStream.Memory, FTempStream.Size, 0, UnknownPtr, NewSize);
    {$ENDIF}

    Dest.Write(UnknownPtr^, NewSize);
    FreeMem(UnknownPtr, NewSize);
  finally
    FTempStream.Free;
  end;
  Result := s;
end;

procedure fcxDeflateStream(Source, Dest: TStream;
  Compression: TfcxCompressionLevel = gzDefault);
var
  Compressor: TCompressionStream;
begin
  Compressor := TCompressionStream.Create(TCompressionLevel(Compression), Dest);
  try
    Compressor.CopyFrom(Source, 0);
  finally
    Compressor.Free;
  end;
end;

procedure fcxInflateStream(Source, Dest: TStream);
var
  FTempStream: TMemoryStream;
  UnknownPtr: Pointer;
  NewSize: Integer;
begin
  FTempStream := TMemoryStream.Create;
  try
    FTempStream.CopyFrom(Source, 0);
    // uncompress data and save it to the Dest
    {$IFDEF DELPHI12}
    ZDeCompress(FTempStream.Memory, FTempStream.Size, UnknownPtr, NewSize);
    {$ELSE}
    DecompressBuf(FTempStream.Memory, FTempStream.Size, 0, UnknownPtr, NewSize);
    {$ENDIF}
    Dest.Write(UnknownPtr^, NewSize);
    FreeMem(UnknownPtr, NewSize);
  finally
    FTempStream.Free;
  end;
end;

end.
