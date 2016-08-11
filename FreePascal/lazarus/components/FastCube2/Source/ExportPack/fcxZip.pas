
{******************************************}
{                                          }
{             FastReport v4.0              }
{         ZIP archiver support unit        }
{                                          }
{         Copyright (c) 2006-2008          }
{          by Alexander Fediachov,         }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}

unit fcxZip;

{$I fcx.inc}

interface

uses
  SysUtils, Classes, 
  {$IFDEF Delphi_12UP}AnsiStrings,{$ENDIF}
  fcxGZip, fcxUtils;
{$ELSE}
interface

{$I fcx.inc}

uses
  // RTL
  System.SysUtils, System.Classes, System.AnsiStrings,
  // FC FMX
  FMX.fcxUtils, FMX.fcxGZip;
{$ENDIF}

type
  TfcxZipLocalFileHeader = class;
  TfcxZipCentralDirectory = class;
  TfcxZipFileHeader = class;

  TfcxZipArchive = class(TObject)
  private
  {$IFDEF Delphi_12UP}
    FRootFolder: AnsiString;
  {$ELSE}
    FRootFolder: String;
  {$ENDIF}
    FErrors: TStringList;
    FFileList: TStringList;
  {$IFDEF Delphi_12UP}
    FComment: AnsiString;
  {$ELSE}
    FComment: AnsiString;
  {$ENDIF}
    FProgress: TNotifyEvent;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  {$IFDEF Delphi_12UP}
    procedure AddFile(const FileName: AnsiString);
    procedure AddDir(const DirName: AnsiString);
    procedure SaveToFile(const Filename: AnsiString);
  {$ELSE}
    procedure AddFile(const FileName: String);
    procedure AddDir(const DirName: String);
    procedure SaveToFile(const Filename: String);
  {$ENDIF}
    procedure SaveToStream(const Stream: TStream);
    property Errors: TStringList read FErrors;
  {$IFDEF Delphi_12UP}
    property Comment: AnsiString read FComment write FComment;
    property RootFolder: AnsiString read FRootFolder write FRootFolder;
  {$ELSE}
    property Comment: String read FComment write FComment;
    property RootFolder: String read FRootFolder write FRootFolder;
  {$ENDIF}
    property FileCount: Integer read GetCount;
    property OnProgress: TNotifyEvent read FProgress write FProgress;
  end;

  TfcxZipLocalFileHeader = class(TObject)
  private
    FLocalFileHeaderSignature: Longword;
    FVersion: WORD;
    FGeneralPurpose: WORD;
    FCompressionMethod: WORD;
    FCrc32: Longword;
    FLastModFileDate: WORD;
    FLastModFileTime: WORD;
    FCompressedSize: Longword;
    FUnCompressedSize: Longword;
  {$IFDEF Delphi_12UP}
    FExtraField: AnsiString;
    FFileName: AnsiString;
  {$ELSE}
    FExtraField: String;
    FFileName: String;
  {$ENDIF}
    FFileNameLength: WORD;
    FExtraFieldLength: WORD;
  {$IFDEF Delphi_12UP}
    procedure SetExtraField(const Value: AnsiString);
    procedure SetFileName(const Value: AnsiString);
  {$ELSE}
    procedure SetExtraField(const Value: String);
    procedure SetFileName(const Value: String);
  {$ENDIF}
  public
    constructor Create;
    procedure SaveToStream(const Stream: TStream);
    property LocalFileHeaderSignature: Longword read FLocalFileHeaderSignature;
    property Version: WORD read FVersion write FVersion;
    property GeneralPurpose: WORD read FGeneralPurpose write FGeneralPurpose;
    property CompressionMethod: WORD read FCompressionMethod write FCompressionMethod;
    property LastModFileTime: WORD read FLastModFileTime write FLastModFileTime;
    property LastModFileDate: WORD read FLastModFileDate write FLastModFileDate;
    property Crc32: Longword read FCrc32 write FCrc32;
    property CompressedSize: Longword read FCompressedSize write FCompressedSize;
    property UnCompressedSize: Longword read FUnCompressedSize write FUnCompressedSize;
    property FileNameLength: WORD read FFileNameLength write FFileNameLength;
    property ExtraFieldLength: WORD read FExtraFieldLength write FExtraFieldLength;
  {$IFDEF Delphi_12UP}
    property FileName: AnsiString read FFileName write SetFileName;
    property ExtraField: AnsiString read FExtraField write SetExtraField;
  {$ELSE}
    property FileName: String read FFileName write SetFileName;
    property ExtraField: String read FExtraField write SetExtraField;
  {$ENDIF}
  end;

  TfcxZipCentralDirectory = class(TObject)
  private
    FEndOfChentralDirSignature: Longword;
    FNumberOfTheDisk: WORD;
    FTotalOfEntriesCentralDirOnDisk: WORD;
    FNumberOfTheDiskStartCentralDir: WORD;
    FTotalOfEntriesCentralDir: WORD;
    FSizeOfCentralDir: Longword;
    FOffsetStartingDiskDir: Longword;
  {$IFDEF Delphi_12UP}
    FComment: AnsiString;
  {$ELSE}
    FComment: String;
  {$ENDIF}
    FCommentLength: WORD;
  {$IFDEF Delphi_12UP}
    procedure SetComment(const Value: AnsiString);
  {$ELSE}
    procedure SetComment(const Value: String);
  {$ENDIF}
  public
    constructor Create;
    procedure SaveToStream(const Stream: TStream);
    property EndOfChentralDirSignature: Longword read FEndOfChentralDirSignature;
    property NumberOfTheDisk: WORD read FNumberOfTheDisk write FNumberOfTheDisk;
    property NumberOfTheDiskStartCentralDir: WORD
      read FNumberOfTheDiskStartCentralDir write FNumberOfTheDiskStartCentralDir;
    property TotalOfEntriesCentralDirOnDisk: WORD
      read FTotalOfEntriesCentralDirOnDisk write FTotalOfEntriesCentralDirOnDisk;
    property TotalOfEntriesCentralDir: WORD
      read FTotalOfEntriesCentralDir write FTotalOfEntriesCentralDir;
    property SizeOfCentralDir: Longword read FSizeOfCentralDir write FSizeOfCentralDir;
    property OffsetStartingDiskDir: Longword read FOffsetStartingDiskDir write FOffsetStartingDiskDir;
    property CommentLength: WORD read FCommentLength write FCommentLength;
  {$IFDEF Delphi_12UP}
    property Comment: AnsiString read FComment write SetComment;
  {$ELSE}
    property Comment: String read FComment write SetComment;
  {$ENDIF}
  end;

  TfcxZipFileHeader = class(TObject)
  private
    FCentralFileHeaderSignature: Longword;
    FRelativeOffsetLocalHeader: Longword;
    FUnCompressedSize: Longword;
    FCompressedSize: Longword;
    FCrc32: Longword;
    FExternalFileAttribute: Longword;
  {$IFDEF Delphi_12UP}
    FExtraField: AnsiString;
    FFileComment: AnsiString;
    FFileName: AnsiString;
  {$ELSE}
    FExtraField: String;
    FFileComment: String;
    FFileName: String;
  {$ENDIF}
    FCompressionMethod: WORD;
    FDiskNumberStart: WORD;
    FLastModFileDate: WORD;
    FLastModFileTime: WORD;
    FVersionMadeBy: WORD;
    FGeneralPurpose: WORD;
    FFileNameLength: WORD;
    FInternalFileAttribute: WORD;
    FExtraFieldLength: WORD;
    FVersionNeeded: WORD;
    FFileCommentLength: WORD;
  {$IFDEF Delphi_12UP}
    procedure SetExtraField(const Value: AnsiString);
    procedure SetFileComment(const Value: AnsiString);
    procedure SetFileName(const Value: AnsiString);
  {$ELSE}
    procedure SetExtraField(const Value: String);
    procedure SetFileComment(const Value: String);
    procedure SetFileName(const Value: String);
  {$ENDIF}
  public
    constructor Create;
    procedure SaveToStream(const Stream: TStream);
    property CentralFileHeaderSignature: Longword read FCentralFileHeaderSignature;
    property VersionMadeBy: WORD read FVersionMadeBy;
    property VersionNeeded: WORD read FVersionNeeded;
    property GeneralPurpose: WORD read FGeneralPurpose write FGeneralPurpose;
    property CompressionMethod: WORD read FCompressionMethod write FCompressionMethod;
    property LastModFileTime: WORD read FLastModFileTime write FLastModFileTime;
    property LastModFileDate: WORD read FLastModFileDate write FLastModFileDate;
    property Crc32: Longword read FCrc32 write FCrc32;
    property CompressedSize: Longword read FCompressedSize write FCompressedSize;
    property UnCompressedSize: Longword read FUnCompressedSize write FUnCompressedSize;
    property FileNameLength: WORD read FFileNameLength write FFileNameLength;
    property ExtraFieldLength: WORD read FExtraFieldLength write FExtraFieldLength;
    property FileCommentLength: WORD read FFileCommentLength write FFileCommentLength;
    property DiskNumberStart: WORD read FDiskNumberStart write FDiskNumberStart;
    property InternalFileAttribute: WORD read FInternalFileAttribute write FInternalFileAttribute;
    property ExternalFileAttribute: Longword read FExternalFileAttribute write FExternalFileAttribute;
    property RelativeOffsetLocalHeader: Longword read FRelativeOffsetLocalHeader write FRelativeOffsetLocalHeader;
  {$IFDEF Delphi_12UP}
    property FileName: AnsiString read FFileName write SetFileName;
    property ExtraField: AnsiString read FExtraField write SetExtraField;
    property FileComment: AnsiString read FFileComment write SetFileComment;
  {$ELSE}
    property FileName: String read FFileName write SetFileName;
    property ExtraField: String read FExtraField write SetExtraField;
    property FileComment: String read FFileComment write SetFileComment;
  {$ENDIF}
  end;

  TfcxZipLocalFile = class(TObject)
  private
    FLocalFileHeader: TfcxZipLocalFileHeader;
    FFileData: TMemoryStream;
    FOffset: Longword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(const Stream: TStream);
    property LocalFileHeader: TfcxZipLocalFileHeader read FLocalFileHeader;
    property FileData: TMemoryStream read FFileData write FFileData;
    property Offset: Longword read FOffset write FOffset;
  end;

implementation

const
  ZIP_VERSIONMADEBY = 20;
  ZIP_NONE = 0;
  ZIP_DEFLATED = 8;
  ZIP_MINSIZE = 128;

{ TfcxZipLocalFile }

constructor TfcxZipLocalFile.Create;
begin
  FLocalFileHeader := TfcxZipLocalFileHeader.Create;
  FOffset := 0;
end;

destructor TfcxZipLocalFile.Destroy;
begin
  FLocalFileHeader.Free;
  inherited;
end;

procedure TfcxZipLocalFile.SaveToStream(const Stream: TStream);
begin
  FLocalFileHeader.SaveToStream(Stream);
  FFileData.Position := 0;
  FFileData.SaveToStream(Stream);
end;

{ TfcxZipLocalFileHeader }

constructor TfcxZipLocalFileHeader.Create;
begin
  inherited;
  FLocalFileHeaderSignature := $04034b50;
  FVersion := ZIP_VERSIONMADEBY;
  FGeneralPurpose := 0;
  FCompressionMethod := ZIP_NONE;
  FCrc32 := 0;
  FLastModFileDate := 0;
  FLastModFileTime := 0;
  FCompressedSize := 0;
  FUnCompressedSize := 0;
  FExtraField := '';
  FFileName := '';
  FFileNameLength := 0;
  FExtraFieldLength := 0;
end;

procedure TfcxZipLocalFileHeader.SaveToStream(const Stream: TStream);
begin
  Stream.Write(FLocalFileHeaderSignature, 4);
  Stream.Write(FVersion, 2);
  Stream.Write(FGeneralPurpose, 2);
  Stream.Write(FCompressionMethod, 2);
  Stream.Write(FLastModFileTime, 2);
  Stream.Write(FLastModFileDate, 2);
  Stream.Write(FCrc32, 4);
  Stream.Write(FCompressedSize, 4);
  Stream.Write(FUnCompressedSize, 4);
  Stream.Write(FFileNameLength, 2);
  Stream.Write(FExtraFieldLength, 2);
  if FFileNameLength > 0 then
    Stream.Write(FFileName[1], FFileNameLength);
  if FExtraFieldLength > 0 then
    Stream.Write(FExtraField[1], FExtraFieldLength);
end;

procedure TfcxZipLocalFileHeader.SetExtraField(const Value: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
begin
  FExtraField := Value;
  FExtraFieldLength := Length(Value);
end;

procedure TfcxZipLocalFileHeader.SetFileName(const Value: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
begin
  FFileName :=  StringReplace(Value,
    {$IFDEF Delphi_12UP}AnsiString('\'), AnsiString('/'){$ELSE}'\', '/'{$ENDIF}, [rfReplaceAll]);
  FFileNameLength := Length(Value);
end;

{ TfcxZipCentralDirectory }

constructor TfcxZipCentralDirectory.Create;
begin
  inherited;
  FEndOfChentralDirSignature := $06054b50;
  FNumberOfTheDisk := 0;
  FNumberOfTheDiskStartCentralDir := 0;
  FTotalOfEntriesCentralDirOnDisk := 0;
  FTotalOfEntriesCentralDir := 0;
  FSizeOfCentralDir := 0;
  FOffsetStartingDiskDir := 0;
  FCommentLength := 0;
  FComment := '';
end;

procedure TfcxZipCentralDirectory.SaveToStream(const Stream: TStream);
begin
  Stream.Write(FEndOfChentralDirSignature, 4);
  Stream.Write(FNumberOfTheDisk, 2);
  Stream.Write(FNumberOfTheDiskStartCentralDir, 2);
  Stream.Write(FTotalOfEntriesCentralDirOnDisk, 2);
  Stream.Write(FTotalOfEntriesCentralDir, 2);
  Stream.Write(FSizeOfCentralDir, 4);
  Stream.Write(FOffsetStartingDiskDir, 4);
  Stream.Write(FCommentLength, 2);
  if FCommentLength > 0 then
    Stream.Write(FComment[1], FCommentLength);
end;

procedure TfcxZipCentralDirectory.SetComment(const Value: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
begin
  FComment := Value;
  FCommentLength := Length(Value);
end;

{ TfcxZipFileHeader }

constructor TfcxZipFileHeader.Create;
begin
  FCentralFileHeaderSignature := $02014b50;
  FRelativeOffsetLocalHeader := 0;
  FUnCompressedSize := 0;
  FCompressedSize := 0;
  FCrc32 := 0;
  FExternalFileAttribute := 0;
  FExtraField := '';
  FFileComment := '';
  FFileName := '';
  FCompressionMethod := 0;
  FDiskNumberStart := 0;
  FLastModFileDate := 0;
  FLastModFileTime := 0;
  FVersionMadeBy := ZIP_VERSIONMADEBY;
  FGeneralPurpose := 0;
  FFileNameLength := 0;
  FInternalFileAttribute := 0;
  FExtraFieldLength := 0;
  FVersionNeeded := ZIP_VERSIONMADEBY;
  FFileCommentLength := 0;
end;

procedure TfcxZipFileHeader.SaveToStream(const Stream: TStream);
begin
  Stream.Write(FCentralFileHeaderSignature, 4);
  Stream.Write(FVersionMadeBy, 2);
  Stream.Write(FVersionNeeded, 2);
  Stream.Write(FGeneralPurpose, 2);
  Stream.Write(FCompressionMethod, 2);
  Stream.Write(FLastModFileTime, 2);
  Stream.Write(FLastModFileDate, 2);
  Stream.Write(FCrc32, 4);
  Stream.Write(FCompressedSize, 4);
  Stream.Write(FUnCompressedSize, 4);
  Stream.Write(FFileNameLength, 2);
  Stream.Write(FExtraFieldLength, 2);
  Stream.Write(FFileCommentLength, 2);
  Stream.Write(FDiskNumberStart, 2);
  Stream.Write(FInternalFileAttribute, 2);
  Stream.Write(FExternalFileAttribute, 4);
  Stream.Write(FRelativeOffsetLocalHeader, 4);
  Stream.Write(FFilename[1], FFileNameLength);
  Stream.Write(FExtraField[1], FExtraFieldLength);
  Stream.Write(FFileComment[1], FFileCommentLength);
end;

procedure TfcxZipFileHeader.SetExtraField(const Value: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
begin
  FExtraField := Value;
  FExtraFieldLength := Length(Value);
end;

procedure TfcxZipFileHeader.SetFileComment(const Value: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
begin
  FFileComment := Value;
  FFileNameLength := Length(Value);
end;

procedure TfcxZipFileHeader.SetFileName(const Value: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
begin
  FFileName := StringReplace(Value,
    {$IFDEF Delphi_12UP}AnsiString('\'), AnsiString('/'){$ELSE}'\', '/'{$ENDIF}, [rfReplaceAll]);
  FFileNameLength := Length(Value);
end;

{ TfcxZipArchive }

procedure TfcxZipArchive.AddDir(const DirName: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
var
  SRec: TSearchRec;
  i: Integer;
{$IFDEF Delphi_12UP}
  s: AnsiString;
{$ELSE}
  s: String;
{$ENDIF}
begin
{$IFDEF Delphi_12UP}
  if DirectoryExists(String(DirName)) then
{$ELSE}
  if DirectoryExists(DirName) then
{$ENDIF}
  begin
    s := DirName;
    if s[Length(s)] <> DirectorySeparator then
      s := s + DirectorySeparator;
{$IFDEF Delphi_12UP}
    i := FindFirst(String(s) + AllFilesMask, faDirectory + faArchive, SRec);
{$ELSE}
    i := FindFirst(s + AllFilesMask, faDirectory + faArchive, SRec);
{$ENDIF}
    try
      while i = 0 do
      begin
        if (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          if (SRec.Attr and faDirectory) = faDirectory then
{$IFDEF Delphi_12UP}
            AddDir(s + AnsiString(SRec.Name))
          else
            AddFile(s + AnsiString(SRec.Name));
{$ELSE}
            AddDir(s + SRec.Name)
          else
            AddFile(s + SRec.Name);
{$ENDIF}
        end;
        i := FindNext(SRec);
      end;
    finally
      FindClose(SRec);
    end;
  end;
end;

{$IFDEF Delphi_12UP}
procedure TfcxZipArchive.AddFile(const FileName: AnsiString);
begin
  if FileExists(String(FileName)) then
  begin
    FFileList.Add(String(FileName));
    if FRootFolder = '' then
      FRootFolder := ExtractFilePath(FileName);
  end
  else
    FErrors.Add('File ' + String(FileName) + ' not found!');
end;
{$ELSE}
procedure TfcxZipArchive.AddFile(const FileName: String);
begin
  if FileExists(FileName) then
  begin
    FFileList.Add(FileName);
    if FRootFolder = '' then
      FRootFolder := ExtractFilePath(FileName);
  end
  else
    FErrors.Add('File ' + FileName + ' not found!');
end;
{$ENDIF}
procedure TfcxZipArchive.Clear;
begin
  FErrors.Clear;
  FFileList.Clear;
  FRootFolder := '';
  FComment := '';
end;

constructor TfcxZipArchive.Create;
begin
  FProgress := nil;
  FErrors := TStringList.Create;
  FFileList := TStringList.Create;
  Clear;
end;

destructor TfcxZipArchive.Destroy;
begin
  FErrors.Free;
  FFileList.Free;
  inherited;
end;

function TfcxZipArchive.GetCount: Integer;
begin
  Result := FFileList.Count;
end;

procedure TfcxZipArchive.SaveToFile(const FileName: {$IFDEF Delphi_12UP}AnsiString{$ELSE}String{$ENDIF});
var
  f: TFileStream;
begin
{$IFDEF Delphi_12UP}
  f := TFileStream.Create(String(FileName), fmCreate);
{$ELSE}
  f := TFileStream.Create(FileName, fmCreate);
{$ENDIF}
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

procedure TfcxZipArchive.SaveToStream(const Stream: TStream);
var
  i: Integer;
  ZipFile: TfcxZipLocalFile;
  ZipFileHeader: TfcxZipFileHeader;
  ZipDir: TfcxZipCentralDirectory;
  FileStream: TFileStream;
  TempStream: TMemoryStream;
{$IFDEF Delphi_12UP}
  FileName: AnsiString;
{$ELSE}
  FileName: String;
{$ENDIF}
  CentralStartPos, CentralEndPos: Longword;
  LFT: LongInt;
begin
  for i := 0 to FFileList.Count - 1 do
  begin
    ZipFile := TfcxZipLocalFile.Create;
    ZipFile.FileData := TMemoryStream.Create;
    try
{$IFDEF Delphi_12UP}
      FileName := StringReplace(AnsiString(FFileList[i]), FRootFolder, AnsiString(''), []);
{$ELSE}
      FileName := StringReplace(FFileList[i], FRootFolder, '', []);
{$ENDIF}
      ZipFile.LocalFileHeader.FileName := FileName;
      FileStream := TFileStream.Create(FFileList[i], fmOpenRead + fmShareDenyWrite);
      try
        if FileStream.Size > ZIP_MINSIZE then
        begin
          FileStream.Position := 0;
          TempStream := TMemoryStream.Create;
          try
            fcxDeflateStream(FileStream, TempStream);
            TempStream.Position := 2;
            ZipFile.FileData.CopyFrom(TempStream, TempStream.Size - 6);
          finally
            TempStream.Free;
          end;
          ZipFile.LocalFileHeader.CompressionMethod := ZIP_DEFLATED;
        end
        else
        begin
          ZipFile.FileData.CopyFrom(FileStream, 0);
          ZipFile.LocalFileHeader.CompressionMethod := ZIP_NONE;
        end;
        ZipFile.LocalFileHeader.CompressedSize := ZipFile.FileData.Size;
        ZipFile.LocalFileHeader.UnCompressedSize := FileStream.Size;
        TempStream := TMemoryStream.Create;
        try
          TempStream.CopyFrom(FileStream, 0);
          ZipFile.LocalFileHeader.Crc32 := fcxStreamCRC32(TempStream);
        finally
          TempStream.Free;
        end;
        ZipFile.Offset := Stream.Position;
        LFT := FileGetDate(FileStream.Handle);

        {$IFNDEF STATIC_EXPORTING_RESULTS}
        ZipFile.LocalFileHeader.LastModFileDate := LongRec(LFT).Hi;
        ZipFile.LocalFileHeader.LastModFileTime := LongRec(LFT).Lo;
        {$ENDIF}
      finally
        FileStream.Free;
      end;
      ZipFile.SaveToStream(Stream);
      if Assigned(FProgress) then
        FProgress(Self);
    finally
      ZipFile.FileData.Free;
      ZipFile.FileData := nil;
    end;
    FFileList.Objects[i] := ZipFile;
  end;
  CentralStartPos := Stream.Position;
  for i := 0 to FFileList.Count - 1 do
  begin
    ZipFile := TfcxZipLocalFile(FFileList.Objects[i]);
    ZipFileHeader := TfcxZipFileHeader.Create;
    try
      ZipFileHeader.CompressionMethod := ZipFile.LocalFileHeader.CompressionMethod;
      ZipFileHeader.LastModFileTime := ZipFile.LocalFileHeader.LastModFileTime;
      ZipFileHeader.LastModFileDate := ZipFile.LocalFileHeader.LastModFileDate;
      ZipFileHeader.GeneralPurpose := ZipFile.LocalFileHeader.GeneralPurpose;
      ZipFileHeader.Crc32 := ZipFile.LocalFileHeader.Crc32;
      ZipFileHeader.CompressedSize := ZipFile.LocalFileHeader.CompressedSize;
      ZipFileHeader.UnCompressedSize := ZipFile.LocalFileHeader.UnCompressedSize;
      ZipFileHeader.RelativeOffsetLocalHeader := ZipFile.Offset;
      ZipFileHeader.FileName := ZipFile.LocalFileHeader.FileName;
      ZipFileHeader.SaveToStream(Stream);
    finally
      ZipFileHeader.Free;
    end;
    ZipFile.Free;
  end;
  CentralEndPos := Stream.Position;
  ZipDir := TfcxZipCentralDirectory.Create;
  try
    ZipDir.TotalOfEntriesCentralDirOnDisk := FFileList.Count;
    ZipDir.TotalOfEntriesCentralDir := FFileList.Count;
    ZipDir.SizeOfCentralDir := CentralEndPos - CentralStartPos;
    ZipDir.OffsetStartingDiskDir := CentralStartPos;
    ZipDir.SaveToStream(Stream);
  finally
    ZipDir.Free;
  end;
end;

end.
