{*******************************************************}
{                                                       }
{          FastCube 2 utility functions Unit            }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxUtils;

{$include fcx.inc}

interface

uses
{$ifndef fpc}
  Windows, ShellApi,
{$else}
  LCLIntf, FileUtil, LazFileUtils,
{$endif}
  SysUtils, Classes;
//FMX uses
{$ELSE}

{$include fcx.inc}

interface

uses
  System.SysUtils, System.Classes, System.IOUtils;
{$ENDIF}

type
  TFileTime = record
    dwLowDateTime: Cardinal;
    dwHighDateTime: Cardinal;
  end;
{$IFDEF FMX}
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  procedure DateTimeToSystemTime(const DateTime: TDateTime; var SystemTime: TSystemTime);
{$ENDIF}

{$ifndef fpc}
function GetTempFileName(ATempDir, APrefix: String): String;
{$endif}
function GetTemporaryFolder: String;
function GetTempFile: String;
function GetAppPath: String;
procedure DeleteFolder(const DirName: String);
function fcxStreamCRC32(Stream: TStream): Cardinal;
function OpenDocument(DocumentPath: String): Boolean;
function SystemTimeToFileTime(SystemTime: TSystemTime; out FileTime: TFileTime): WordBool;

{$ifndef fpc}
const
  {$ifndef DELPHI_6UP}
  DirectorySeparator = '\';
  {$else}
  DirectorySeparator = PathDelim;
  {$endif}
  AllFilesMask = '*';
{$endif}

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  fcxTypes;
//FMX uses
{$ELSE}
uses
  FMX.fcxTypes;
{$ENDIF}

const
  CRCTable: array [0..255] of Cardinal = (
 0000000000, 1996959894, 3993919788, 2567524794,
 0124634137, 1886057615, 3915621685, 2657392035,
 0249268274, 2044508324, 3772115230, 2547177864,
 0162941995, 2125561021, 3887607047, 2428444049,
 0498536548, 1789927666, 4089016648, 2227061214,
 0450548861, 1843258603, 4107580753, 2211677639,
 0325883990, 1684777152, 4251122042, 2321926636,
 0335633487, 1661365465, 4195302755, 2366115317,
 0997073096, 1281953886, 3579855332, 2724688242,
 1006888145, 1258607687, 3524101629, 2768942443,
 0901097722, 1119000684, 3686517206, 2898065728,
 0853044451, 1172266101, 3705015759, 2882616665,
 0651767980, 1373503546, 3369554304, 3218104598,
 0565507253, 1454621731, 3485111705, 3099436303,
 0671266974, 1594198024, 3322730930, 2970347812,
 0795835527, 1483230225, 3244367275, 3060149565,
 1994146192, 0031158534, 2563907772, 4023717930,
 1907459465, 0112637215, 2680153253, 3904427059,
 2013776290, 0251722036, 2517215374, 3775830040,
 2137656763, 0141376813, 2439277719, 3865271297,
 1802195444, 0476864866, 2238001368, 4066508878,
 1812370925, 0453092731, 2181625025, 4111451223,
 1706088902, 0314042704, 2344532202, 4240017532,
 1658658271, 0366619977, 2362670323, 4224994405,
 1303535960, 0984961486, 2747007092, 3569037538,
 1256170817, 1037604311, 2765210733, 3554079995,
 1131014506, 0879679996, 2909243462, 3663771856,
 1141124467, 0855842277, 2852801631, 3708648649,
 1342533948, 0654459306, 3188396048, 3373015174,
 1466479909, 0544179635, 3110523913, 3462522015,
 1591671054, 0702138776, 2966460450, 3352799412,
 1504918807, 0783551873, 3082640443, 3233442989,
 3988292384, 2596254646, 0062317068, 1957810842,
 3939845945, 2647816111, 0081470997, 1943803523,
 3814918930, 2489596804, 0225274430, 2053790376,
 3826175755, 2466906013, 0167816743, 2097651377,
 4027552580, 2265490386, 0503444072, 1762050814,
 4150417245, 2154129355, 0426522225, 1852507879,
 4275313526, 2312317920, 0282753626, 1742555852,
 4189708143, 2394877945, 0397917763, 1622183637,
 3604390888, 2714866558, 0953729732, 1340076626,
 3518719985, 2797360999, 1068828381, 1219638859,
 3624741850, 2936675148, 0906185462, 1090812512,
 3747672003, 2825379669, 0829329135, 1181335161,
 3412177804, 3160834842, 0628085408, 1382605366,
 3423369109, 3138078467, 0570562233, 1426400815,
 3317316542, 2998733608, 0733239954, 1555261956,
 3268935591, 3050360625, 0752459403, 1541320221,
 2607071920, 3965973030, 1969922972, 0040735498,
 2617837225, 3943577151, 1913087877, 0083908371,
 2512341634, 3803740692, 2075208622, 0213261112,
 2463272603, 3855990285, 2094854071, 0198958881,
 2262029012, 4057260610, 1759359992, 0534414190,
 2176718541, 4139329115, 1873836001, 0414664567,
 2282248934, 4279200368, 1711684554, 0285281116,
 2405801727, 4167216745, 1634467795, 0376229701,
 2685067896, 3608007406, 1308918612, 0956543938,
 2808555105, 3495958263, 1231636301, 1047427035,
 2932959818, 3654703836, 1088359270, 0936918000,
 2847714899, 3736837829, 1202900863, 0817233897,
 3183342108, 3401237130, 1404277552, 0615818150,
 3134207493, 3453421203, 1423857449, 0601450431,
 3009837614, 3294710456, 1567103746, 0711928724,
 3020668471, 3272380065, 1510334235, 0755167117);

function fcxStreamCRC32(Stream: TStream): Cardinal;
var
  OldPos: Integer;
  b: Byte;
  c: Cardinal;
begin
  OldPos := Stream.Position;
  Stream.Position := 0;
  c := $ffffffff;
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(b,1);
    c := CrcTable[(c xor Cardinal(b)) and $ff] xor (c shr 8);
  end;
  Stream.Position := OldPos;
  Result := c xor $ffffffff;
end;

{$IFNDEF FMX}

function GetAppFileName: String;
// There is no cross platoform method at moment
// Lets hope that user want the same directory as .exe directory but not .dll
{$IFNDEF FPC}
var
  fName: String;
  nsize: cardinal;
{$ENDIF}
begin
{$IFNDEF FPC}
  nsize := MAX_PATH;
  SetLength(fName, nsize);
  SetLength(fName, GetModuleFileName(hinstance, PChar(fName), nsize));
  Result := fName;
{$ELSE}
  Result := ParamStr(0);
{$ENDIF}
end;

function GetAppPath: String;
begin
  Result := ExtractFilePath(GetAppFileName);
end;

{$ifndef fpc}
function GetTempFileName(ATempDir, APrefix: String): String;
var
{$IFDEF Delphi_12UP}
  Path: WideString;
  FileName: WideString;
{$ELSE}
  Path: String[64];
  FileName: String[255];
{$ENDIF}
begin
{$IFDEF Delphi_12UP}
  SetLength(FileName, 255);
  Path := ATempDir;
  if (Path = '') or not DirectoryExists(String(Path)) then
  begin
    SetLength(Path, 255);
    SetLength(Path, GetTempPath(255, @Path[1]));
  end
  else
{$ELSE}
  Path := ATempDir;
  if (Path = '') or not DirectoryExists(Path) then
    Path[0] := Chr(GetTempPath(64, @Path[1])) else
{$ENDIF}
    Path := Path + #0;
  if (Path <> '') and (Path[Length(Path)] <> '\') then
    Path := Path + '\';
  Windows.GetTempFileName(@Path[1], PChar(APrefix), 0, @FileName[1]);
{$IFDEF Delphi_12UP}
  Result := StrPas(PWideChar(@FileName[1]));
{$ELSE}
  Result := StrPas(@FileName[1]);
{$ENDIF}
end;
{$endif}

function GetTemporaryFolder: String;
{$ifndef fpc}
var
  Path: String;
{$endif}
begin
{$ifdef fpc}
  Result := GetTempDir;
  if (Length(Result) > 0) and (Result[Length(Result)] = DirectorySeparator) then
    Delete(Result, Length(Result), 1);
{$else}
  Setlength(Path, MAX_PATH);
  SetLength(Path, GetTempPath(MAX_PATH, @Path[1]));
{$IFDEF Delphi_12UP}
  Result := StrPas(PWideChar(@Path[1]));
{$ELSE}
  Result := StrPas(@Path[1]);
{$ENDIF}
{$endif}
end;

{$WARNINGS OFF}
{$ifndef fpc}
procedure DeleteFolder(const DirName: String);
var
  SearchRec: TSearchRec;
  i: Integer;
begin
  if DirectoryExists(DirName) then
  begin
    i := FindFirst(DirName + '\*.*', faAnyFile, SearchRec);
    while i = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) > 0 then
          DeleteFolder(DirName + '\' + SearchRec.Name)
        else if (SearchRec.Attr and faVolumeID) = 0 then
        try
          DeleteFile(PChar(DirName + '\' + SearchRec.Name));
        except
        end;
      end;
      i := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
    try
      RemoveDirectory(PChar(DirName));
    except
    end;
  end;
end;
{$else}
procedure DeleteFolder(const DirName: String);
var
  SearchRec: TSearchRec;
  i: Integer;
begin
  if DirectoryExistsUTF8(DirName) then
  begin
    i := FindFirstUTF8(DirName + DirectorySeparator + '*.*', faAnyFile, SearchRec);
    while i = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) > 0 then
          DeleteFolder(DirName + DirectorySeparator + SearchRec.Name)
        else if (SearchRec.Attr and faVolumeID) = 0 then
        try
          DeleteFileUTF8(PChar(DirName + DirectorySeparator + SearchRec.Name));
        except
        end;
      end;
      i := FindNextUTF8(SearchRec);
    end;
    FindCloseUTF8(SearchRec);
    try
      RemoveDirUTF8(DirName);
    except
    end;
  end;
end;
{$endif}
{$WARNINGS ON}

// Contains a 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 (UTC).
function SystemTimeToFileTime(SystemTime: TSystemTime; out FileTime: TFileTime): WordBool;
var
  FileTimeQWD: UInt64;
  Stamp, Stamp2: TTimeStamp;
  DD: Integer;
begin
  Stamp := DateTimeToTimeStamp(SystemTimeToDateTime(SystemTime));
  Stamp2 := DateTimeToTimeStamp(EncodeDate(1601, 1, 1));
  DD := Stamp.Date - Stamp2.Date;
  Result := DD >= 0;
  if Result then
  begin
    FileTimeQWD := (UInt64(DD) * MSecsPerDay + Stamp.Time) * 10000;
    FileTime.dwLowDateTime := FileTimeQWD and $FFFFFFFF;
    FileTime.dwHighDateTime := (FileTimeQWD shr 32) and $FFFFFFFF;
  end;
end;

function OpenDocument(DocumentPath: String): Boolean;
begin
  {$ifdef fpc}
  Result := LCLIntf.OpenDocument(DocumentPath);
  {$else}
  Result := ShellExecute(GetDesktopWindow, 'open', PChar(DocumentPath), nil, nil, SW_SHOW) > 32;
  {$endif}
end;

// FMX
{$ELSE}
function GetAppFileName: String;
begin
  Result := ParamStr(0);
end;

function GetAppPath: String;
begin
  Result := ExtractFilePath(GetAppFileName);
end;

function GetTemporaryFolder: String;
begin
  Result := TPath.GetTempPath;
end;

function GetTempFileName(ATempDir, APrefix: String): String;
var
  Path: String;
  FileName: String;
begin
  SetLength(FileName, 255);
  Path := ATempDir;
  if (Path = '') or not DirectoryExists(Path) then
    Path := GetTemporaryFolder;
  if (Path <> '') and (Path[Length(Path)] <> '\') then
    Path := Path + '\';

  FileName := APrefix + Copy(TPath.GetRandomFileName, 2, 255);

  Result := Path + FileName;
end;

procedure DeleteFolder(const DirName: String);
var
  SearchRec: TSearchRec;
  i: Integer;
begin
  if DirectoryExists(DirName) then
  begin
    i := FindFirst(DirName + '\*.*', faAnyFile, SearchRec);
    while i = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) > 0 then
          DeleteFolder(DirName + '\' + SearchRec.Name)
        else if (SearchRec.Attr and faVolumeID) = 0 then
        try
          DeleteFile(PChar(DirName + '\' + SearchRec.Name));
        except
        end;
      end;
      i := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
    try
      RemoveDir(DirName);
    except
    end;
  end;
end;

function OpenDocument(DocumentPath: String): Boolean;
// todo
begin
  Result := False;
end;

procedure DateTimeToSystemTime(const DateTime: TDateTime; var SystemTime: TSystemTime);
begin
  DecodeDateFully(DateTime, SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay, SystemTime.wDayOfWeek);
  Dec(SystemTime.wDayOfWeek);
  DecodeTime(DateTime, SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);
end;

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  Result := EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay);
  if Result >= 0 then
    Result := Result + EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliSeconds)
  else
    Result := Result - EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliSeconds);
end;

// Contains a 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 (UTC).
function SystemTimeToFileTime(SystemTime: TSystemTime; out FileTime: TFileTime): WordBool;
var
  FileTimeQWD: UInt64;
  Stamp, Stamp2: TTimeStamp;
  DD: Integer;
begin
  Stamp := DateTimeToTimeStamp(SystemTimeToDateTime(SystemTime));
  Stamp2 := DateTimeToTimeStamp(EncodeDate(1601, 1, 1));
  DD := Stamp.Date - Stamp2.Date;
  Result := DD >= 0;
  if Result then
  begin
    FileTimeQWD := (UInt64(DD) * MSecsPerDay + Stamp.Time) * 10000;
    FileTime.dwLowDateTime := FileTimeQWD and $FFFFFFFF;
    FileTime.dwHighDateTime := (FileTimeQWD shr 32) and $FFFFFFFF;
  end;
end;
{$ENDIF}

function GetTempFile: String;
begin
  Result := GetTempFileName(GetTemporaryFolder, 'fc');
{$ifdef fpc}
  // FPC does not create an empty file for us
  with TFileStream.Create(Result, fmCreate) do Free;
{$endif}
end;

end.
