
{******************************************}
{                                          }
{                FastCube 2                }
{            DBF export filter             }
{                                          }
{         Copyright (c) 1998-2009          }
{           by Anton Khayrudinov           }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxExportDBF;

{$INCLUDE fcx.inc}

interface

uses
  SysUtils, Classes, {$IFDEF Delphi6}Variants, {$ENDIF} 
  Controls, StdCtrls, Forms,
  fcxTypes, fcxCustomExport, fcxRes, fcxCube,
  fcxUtils, fcxExportDialog;
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  // RTL
  System.Classes, System.SysUtils, System.Variants, System.UITypes,
  // FMX
  FMX.StdCtrls,
  // FC FMX
  FMX.fcxTypes, FMX.fcxCustomExport, FMX.fcxRes, FMX.fcxCube,
  FMX.fcxUtils, FMX.fcxExportDialog;
{$ENDIF FMX}

type
  TfcxDBFExportDialog = class(TfcxExportDialog)
  private
    OpenCB: TCheckBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TfcxDBFExport = class(TfcxCustomExportFilter)
  private
    FOpenAfterExport: Boolean;
    Exp:          TStream;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
    function ShowModal: TModalResult; override;
    procedure PerformExport(Stream: TStream);
    function Start: Boolean; override;
    procedure Run; override;
    procedure Finish; override;
  published
    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport default False;
    property OverwritePrompt;
  end;

implementation

type

  //
  // DBF header
  // 32 bytes
  //

  TfrxDBFHeader = packed record

    Version:      Byte;

    Year:         Byte;       // date of the last update
    Month:        Byte;
    Day:          Byte;

    RecCount:     LongWord;   // records count
    HdrSize:      Word;       // header size
    RecSize:      Word;       // size of any record
    R1:           Word;
    Transaction:  Byte;
    Encoded:      Byte;
    Environment:  array [1..12] of Byte;
    Indexed:      Byte;
    Language:     Byte;       // language driver number or codepage number
    R2:           Word;

  end;

  //
  // DBF field header
  // 32 bytes
  //

  TfrxDBFFieldHeader = packed record

    Name:         array [1..10] of Byte;
    Zero:         Byte;       // null symbol ending the name
    FieldType:    Byte;

    //
    // Field data address.
    // This works in three modes:
    //
    //  - 4 bytes are written: the address is a pointer to data in virtual memory
    //  - 2 high bytes are zeros: the address is an offset from the record beginning
    //  - all bytes are zeros: the address is ignored
    //

    Address:      LongWord;

    Length:       Byte;       // field length
    Digits:       Byte;       // count of decimal digits
    R1:           Word;
    WSId:         Byte;       // workset identifier
    MultiUser:    Word;       // multi user mode
    SetField:     Byte;
    R2:           array [1..7] of Byte;
    MDX:          Byte;       // this flag means that the field is included into .mdx index

  end;

{ TfcxDBFExport }

constructor TfcxDBFExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FilterDesc := fcxGet(9111);
  DefaultExt := fcxGet(9103);
end;

class function TfcxDBFExport.GetDescription: String;
begin
  Result := fcxGet(9102);
end;

function TfcxDBFExport.ShowModal: TModalResult;
begin
  if not Assigned(Stream) then
  begin
    with TfcxDBFExportDialog.Create(nil) do
    begin
      PrepareSaveDialog(SaveDialog);

      OpenCB.Checked := FOpenAfterExport;

      Result := ShowModal;

      if Result = mrOk then
      begin
        FOpenAfterExport := OpenCB.Checked;

        if SaveDialog.Execute then
          FileName := SaveDialog.FileName
        else
          Result := mrCancel;
      end;
      Free;
    end;
  end else
    Result := mrOk;
end;

function TfcxDBFExport.Start: Boolean;
begin
  if (FileName <> '') or Assigned(Stream) then
  begin
    if (ExtractFilePath(FileName) = '') and (DefaultPath <> '') then
      FileName := DefaultPath + '\' + FileName;
    try
      if Assigned(Stream) then
        Exp := Stream
      else
        Exp := TFileStream.Create(FileName, fmCreate);
       Result := True;
    except
      Result := False;
    end;
  end
  else
    Result := False;
end;

procedure TfcxDBFExport.Finish;
begin
  if not Assigned(Stream) then
    Exp.Free;

  if FOpenAfterExport and not Assigned(Stream) then
    OpenDocument(FileName);
end;

procedure TfcxDBFExport.PerformExport(Stream: TStream);
const
  MaxFieldLen = 254;
var
  Columns: TfcxCubeDataColumns;
  buffer: array [1..MaxFieldLen] of Byte;

  procedure WriteVal(Data: LongInt; Len: LongInt);
  begin
    Stream.Write(Data, Len);
  end;

  procedure WriteRef(const Data; Len: LongInt);
  begin
    Stream.Write(Data, Len);
  end;

  function GetFieldName(i: LongInt): AnsiString;
  begin
    Result := AnsiUpperCase(Columns[i].Field.CubeFieldName);
  end;

  procedure ByteCopy(Dest: Pointer; const Src: AnsiString; MaxLen: Integer);
  var
    n: Integer;
  begin
    n := Length(Src);

    if n > MaxLen then
      n := MaxLen;

    Move(Src[1], Dest^, n);
  end;

  function GetDataTypeSize(DataType: TfcxDataType): Integer;
  begin
    case DataType of
      fcdt_Integer: Result := 11;
      fcdt_Byte: Result := 3;
      fcdt_Word: Result := 5;
      fcdt_SmallInteger: Result := 6;
      fcdt_LargeInteger: Result := 20;
      fcdt_Date: Result := 8;
      fcdt_Double: Result := 18;
      fcdt_Boolean: Result := 1;
    else
      Result := MaxFieldLen;
    end;
  end;

  function GetDataTypeDigits(DataType: TfcxDataType): Integer;
  begin
    case DataType of
      fcdt_Double: Result := 8;
    else
      Result := 0;
    end;
  end;

  function GetFieldType(i: Integer): Byte;
  begin
    case Columns[i].Field.DataType of
      fcdt_Date: Result := Ord('D');
      fcdt_Integer,
      fcdt_Byte,
      fcdt_Word,
      fcdt_SmallInteger,
      fcdt_LargeInteger,
      fcdt_Double: Result := Ord('N');
      fcdt_Boolean: Result := Ord('L');
    else
      Result := Ord('C');
    end;
  end;

  procedure WriteInt(V: Variant; Size: Integer);
  var
    S: String;
    Len: Integer;
  begin
    if TVarData(V).VType <= 1 then
      WriteRef(buffer, Size)
    else
    begin
      S := VarToStr(V);
      Len := Length(S);
      Move(S[1], buffer[Size - Len + 1], Len);
      WriteRef(buffer, Size);
    end
  end;

  procedure WriteValue(RowId, ColumnId: Integer);
  var
    TmpBuffer: array[0..24] of AnsiChar;
    s: String;
    v: Variant;
    d: Extended;
    dt: TDateTime;
    Field: TfcxCommonField;
    Len: Integer;
    P: PAnsiChar;
    Sz: Integer;
  begin
    Field := Columns[ColumnId].Field;
    Sz := GetDataTypeSize(Field.DataType);
    case Field.DataType of
      fcdt_Integer,
      fcdt_Byte,
      fcdt_Word,
      fcdt_SmallInteger,
      fcdt_LargeInteger:
        WriteInt(Cube.SourceHolder.UniqueValueAsVariant[RowId, Field], Sz);
      fcdt_Date:
      begin
        v := Cube.SourceHolder.UniqueValueAsVariant[RowId, Field];
        if TVarData(v).VType > 1 then
        begin
          dt := VarToDateTime(v);
          DateTimeToString(s, 'yyyymmdd', dt);
          ByteCopy(@buffer[1], AnsiString(s), Sz);
        end;
        WriteRef(buffer, Sz);
      end;
      fcdt_Double:
      begin
        v := Cube.SourceHolder.UniqueValueAsVariant[RowId, Field];
        if TVarData(v).VType > 1 then
        begin
          d := v;
          Len := FloatToText(PAnsiChar(@TmpBuffer[0]), d, {$ifndef FPC}fvExtended,{$endif} ffFixed, Sz, GetDataTypeDigits(Field.DataType));
          if Len > Sz then
            Len := Sz;
          TmpBuffer[Len] := #0;
          if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
          begin
            P := StrScan(PAnsiChar(@TmpBuffer[0]), AnsiChar({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator));
            if P <> nil then
              P^ := '.';
          end;
          Move(TmpBuffer[0], buffer[Sz - Len + 1], Len);
        end;
        WriteRef(buffer, Sz);
      end;
      fcdt_Boolean:
      begin
        v := Cube.SourceHolder.UniqueValueAsVariant[RowId, Field];
        if TVarData(v).VType > 1 then
          if Boolean(v) then
            buffer[1] := Ord('T')
          else
            buffer[1] := Ord('F');
        WriteRef(buffer, Sz);
      end;
      else
      begin
        s := Cube.SourceHolder.UniqueValueCaption[RowId, Field];
        if s <> '' then
          ByteCopy(@buffer[1], s, Sz);

        WriteRef(buffer, Sz);
      end;
    end;
  end;

var
  r, c, i, RowID, Address: Integer;
  h: TfrxDBFHeader;
  fh: TfrxDBFFieldHeader;
  y, m, d: Word;
  name: AnsiString;
begin
  Columns := DoGetCubeCols;
  //
  // DBF header
  //

  DecodeDate(CreationTime, y, m, d);
  FillChar(h, Sizeof(h), 0);

  Address := 1;
  for i := 0 to Columns.VisibleCount - 1 do
    inc(Address, GetDataTypeSize(Columns[i].Field.DataType));

  h.Version   := 3;
  h.Year      := y - 2000;
  h.Month     := m;
  h.Day       := d;
  h.RecCount  := DoGetCubeRowCount;
  h.HdrSize   := 32 + 32 * Columns.VisibleCount + 1;
  h.RecSize   := Address;

  WriteRef(h, SizeOf(h));

  //
  // DBF fields descriptions.
  //

  Address := 1;
  for i := 0 to Columns.VisibleCount - 1 do
  begin
    FillChar(fh, SizeOf(fh), 0);
    name := GetFieldName(i);

    if name <> '' then
      ByteCopy(@fh.Name[1], name, 10);

    fh.FieldType    := GetFieldType(i);
    fh.Length       := GetDataTypeSize(Columns[i].Field.DataType);
    fh.Digits       := GetDataTypeDigits(Columns[i].Field.DataType);
    fh.SetField     := 1;
    fh.Address      := Address;

    WriteRef(fh, SizeOf(fh));
    inc(Address, fh.Length);
  end;

  //
  // DBF header ending symbol
  //

  WriteVal(13, 1);

  //
  // DBF records.
  //

  for r := 0 to h.RecCount - 1 do
  begin
    WriteVal(32, 1);
    for c := 0 to Columns.VisibleCount - 1 do
    begin
      FillChar(buffer[1], MaxFieldLen, $20);
      RowID := DoGetCubeRowIndex(r);
      WriteValue(RowId, c);
    end;
    DoProgress(r);    
  end;

  //
  // DBF records ending symbol.
  //

  WriteVal(26, 1);
end;

procedure TfcxDBFExport.Run;
begin
  PerformExport(Exp);
end;

{ TfcxDBFExportDialog }

constructor TfcxDBFExportDialog.Create(AOwner: TComponent);
var
  Page: TComponent;
begin
  inherited;

  Caption := fcxGet(9101);
  Page := AddPage(fcxResources.Get('sExport'));
  OpenCB := AddCheckBox(Page, fcxGet(8706));
end;

end.
