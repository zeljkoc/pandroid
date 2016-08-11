{*******************************************************}
{                                                       }
{             FastCube 2 CSV-export unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxExportCSV;
{$INCLUDE fcx.inc}
interface

{$IFDEF DELPHI_12UP}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

uses
  {$IFNDEF fpc}
    Windows,
  {$ELSE}
    LConvEncoding,
  {$ENDIF}
  Types, Classes, SysUtils, Dialogs, Graphics, Controls, Forms, StdCtrls, ComCtrls,
  {$IFDEF DELPHI_6UP}
  Variants,
  {$ENDIF}
  fcxRes, fcxUtils,fcxTypes, fcxCube, fcxSlice, fcxCustomExport,
  fcxCustomGrid, fcxSliceGrid, fcxExportDialog;
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  // RTL
  System.Classes, System.SysUtils, System.Variants, System.UITypes,
  // FMX
  FMX.Types, FMX.StdCtrls, FMX.ListBox,
  // FC FMX
  FMX.fcxTypes, FMX.fcxSlice, FMX.fcxXML, FMX.fcxCustomExport, FMX.fcxStyles,
  FMX.fcxRes, FMX.fcxCube, FMX.fcxUtils, FMX.fcxExportDialog, FMX.Edit;
{$ENDIF FMX}

type
  TfcxCSVExportDialog = class(TfcxExportDialog)
  private
    OpenCB: TCheckBox;
    OEMCB: TCheckBox;
    edSeparator: TEdit;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TfcxCSVExport = class(TfcxCustomExportFilter)
  private
    FSeparator: String;
    ATempArray: array of AnsiString;
    FOpenAfterExport: Boolean;
    FStream: TStream;
    FOEM: Boolean;
    FUTF8: Boolean;
    FNoSysSymbols: Boolean;
    FForcedQuotes: Boolean;

    FOldDecimalSeparator: Char;

    procedure WriteLN(S: AnsiString);
    function XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
  public
    class function GetDescription: String; override;
    constructor Create(AOwner: TComponent); override;
    procedure PerformExportSlice;
    procedure PerformExportCube;

    function ShowModal: TModalResult; override;
    function Start: Boolean; override;
    procedure Run; override;
    procedure Finish; override;
    function PrepareString(const Str: AnsiString): AnsiString;

    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport default False;
  published
    property OEMCodepage: Boolean read FOEM write FOEM;
    property UTF8: Boolean read FUTF8 write FUTF8;
    property Separator: String read FSeparator write FSeparator;
    { CSV export can ignore exporting of special system characters
      from original strings in the report. The special characters are
      all characters with ASCII codes $00..$1F.  }
    property NoSysSymbols: Boolean read FNoSysSymbols write FNoSysSymbols; {default True}

    { Specification of CSV format allows to leave some fields to be unquoted.
      The ForcedQuotes option allows to enclose all fields in double quotes. }
    property ForcedQuotes: Boolean read FForcedQuotes write FForcedQuotes; {default False}
  end;

implementation

{ TfcxCSVExport }

procedure TfcxCSVExport.WriteLN(S: AnsiString);
begin
  S := S + #$D#$A;
  FStream.Write(S[1], Length(S));
end;

function TfcxCSVExport.XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  AStr: AnsiString;
  i, j: integer;
begin
  AStr := ARec.Text;
  for j := 1 to ARec.TreeRect.SizeLevel do
    for i := ARec.TreeRect.Cell to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
      ATempArray[ARec.TreeRect.Level + j] := ATempArray[ARec.TreeRect.Level + j] + PrepareString(AStr);
  Result := False;
end;

function TfcxCSVExport.YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  AStr: AnsiString;
  i, j: integer;
begin
  AStr := ARec.Text;
  for i := ARec.TreeRect.Cell to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
    for j := 1 to ARec.TreeRect.SizeLevel do
      ATempArray[i] := PrepareString(AStr) + ATempArray[i];
  Result := False;
end;

constructor TfcxCSVExport.Create(AOwner: TComponent);
begin
  inherited;
  FOEM := False;
  FUTF8 := False;
{$IFDEF DELPHI16}
  FSeparator := FormatSettings.ListSeparator;//';';
{$ELSE}
  FSeparator := ListSeparator;//';';
{$ENDIF}
  FNoSysSymbols := True;
  FForcedQuotes := False;
  FilterDesc := fcxGet(8851);
  DefaultExt := fcxGet(8852);
end;

procedure TfcxCSVExport.PerformExportCube;
var
  i, j, Row: Integer;
  S, CellStr: String;
  Value: Variant;
  Columns: TfcxCubeDataColumns;
begin
  Columns := DoGetCubeCols;
  S := '';
  for i := 0 to Columns.VisibleCount - 1 do
    S := S + PrepareString(Columns[i].Field.CubeFieldDisplayLabel);
  if (Length(s)>0) and (s[Length(s)] = Separator) then
    s := copy(s, 1, Length(s)-1);
  WriteLn(S);

  for i := 0 to DoGetCubeRowCount - 1 do
  begin
    S := '';
    for j := 0 to Columns.VisibleCount - 1 do
    begin
      Row := DoGetCubeRowIndex(i);
      Value := Cube.SourceHolder.UniqueValueAsVariant[Row, Columns[j].Field];
      if VarIsNumeric(Value) then
        CellStr := FloatToStr(Value)
      else
        CellStr := Cube.SourceHolder.UniqueValueCaption[Row, Columns[j].Field];
      S := S + PrepareString(CellStr);
    end;
    if (Length(s)>0) and (s[Length(s)] = Separator) then
      s := copy(s, 1, Length(s)-1);
    WriteLn(S);
    DoProgress(i);
  end;
end;

procedure TfcxCSVExport.PerformExportSlice;
var
  ATopRows, i, j: Integer;
  ATempString: AnsiString;
  S: String;
  Cell: TfcxMeasureCell;
begin
  if (Slice.XAxisContainer.VisibleLevelCount = 0) and (Slice.YAxisContainer.VisibleLevelCount = 0) then
    Exit;
  ATopRows := Slice.XAxisContainer.VisibleLevelCount + 1;
  SetLength(ATempArray, ATopRows);
  for i := 0 to ATopRows - 1 do
    ATempArray[i] := '';

  // Draw header

  // row title
  if Slice.YAxisContainer.RealLevelCount <> 0 then
  begin
    if ATopRows > 1 then
      for i := 0 to ATopRows - 2 do
        for j := 0 to Slice.YAxisContainer.VisibleLevelCount - 1 do
          ATempArray[i] := ATempArray[i] + PrepareString('');
    for j := 0 to Slice.YAxisContainer.RealLevelCount - 1 do
    begin
      if (Slice.YAxisContainer.LevelInfo[j].IsVisible) then
      begin
        if Slice.YAxisContainer.LevelInfo[j].IsMeasure then
          ATempArray[ATopRows-1] := ATempArray[ATopRows-1] + PrepareString(Slice.MeasuresContainer.Caption)
        else
          ATempArray[ATopRows-1] := ATempArray[ATopRows-1] + PrepareString(Slice.YAxisContainer.LevelInfo[j].RegionField.Caption);
      end;
    end;
  end;

  // column title
  for i := 0 to Slice.XAxisContainer.RealLevelCount - 1 do
    if (Slice.XAxisContainer.LevelInfo[i].IsVisible) then
    begin
      if Slice.XAxisContainer.LevelInfo[i].IsMeasure then
        ATempArray[0] := ATempArray[0] + PrepareString(Slice.MeasuresContainer.Caption)
      else
        ATempArray[0] := ATempArray[0] + PrepareString(Slice.XAxisContainer.LevelInfo[i].RegionField.Caption);
    end;

   // column header
  Slice.XAxisContainer.TraverseAxis(0, Slice.XAxisContainer.VisibleLevelCount - 1, 0, XAxisDrawProc);

  for i := 0 to ATopRows - 1 do
  begin
    if (Length(ATempArray[i])>0) and (ATempArray[i][Length(ATempArray[i])] = Separator) then
      ATempArray[i] := copy(ATempArray[i], 1, Length(ATempArray[i])-1);
    WriteLn(ATempArray[i]);
  end;

  SetLength(ATempArray, Slice.YAxisContainer.VisibleNodeCount);
  for i := 0 to Slice.YAxisContainer.VisibleNodeCount - 1 do
    ATempArray[i] := '';

  // row header
  if Slice.YAxisContainer.VisibleLevelCount <> 0 then
  begin
    Slice.YAxisContainer.TraverseAxis(0, Slice.YAxisContainer.VisibleLevelCount - 1, 0, YAxisDrawProc);
    for i := 0 to Slice.RowCount - 1 do
    begin
      ATempString := ATempArray[i];
      if (Slice.XAxisContainer.VisibleLevelCount <> 0) then
        for j := 0 to Slice.ColCount - 1 do
        begin
          Slice.GetMeasureCell(j, i, Cell);
          if (Cell.MeasureIndex >= 0) and ((Slice.MeasuresContainer.Measures[Cell.MeasureIndex].DisplayAs in da_Percents) or
            (Slice.MeasuresContainer.Measures[Cell.MeasureIndex].DisplayAs in da_Ranks))then
            S := Cell.StrValue
          else
          if VarIsNumeric(Cell.Value) then
            S := FloatToStr(Cell.Value)
          else
            S := Cell.StrValue;
          if Cell.IsTotal or Cell.IsGrandTotal then
            ATempString := ATempString + PrepareString(S)
          else
            ATempString := ATempString + PrepareString(S);
        end;
      if (Length(ATempString)>0) and (ATempString[Length(ATempString)] = Separator) then
        ATempString := copy(ATempString, 1, Length(ATempString)-1);
      WriteLN(ATempString);
      DoProgress(i);
    end;
  end;
end;

function TfcxCSVExport.PrepareString(const Str: AnsiString): AnsiString;
  function StrToOem(const AnsiStr: AnsiString): AnsiString;
  begin
{$IFNDEF FMX}
{$IFNDEF FPC}
    SetLength(Result, Length(AnsiStr));
    if Length(Result) > 0 then
      CharToOemBuffA(PAnsiChar(AnsiStr), PAnsiChar(Result), Length(Result));
{$ELSE}
    Result := Str;
{$ENDIF}
{$ELSE}
    Result := Str;
{$ENDIF}
  end;

  { Returns count of a character within a byte string }
  function CharCount(const s: AnsiString; c: AnsiChar): Integer;
  var
    i: Integer;
  begin
    Result := 0;

    for i := 1 to Length(s) do
      if s[i] = c then
        Inc(Result);
  end;

  { Copies a source byte string to a destination byte string.
    A specified character is copied twice. }

  procedure DoubleCharacter(r: PAnsiChar; const s: AnsiString; c: AnsiChar);
  var
    si, ri: Integer;

    procedure PushChar(c: AnsiChar);
    begin
      PAnsiChar(PtrInt(r) + ri)^ := c;
      Inc(ri);
    end;

  begin
    ri := 0;

    for si := 1 to Length(s) do
    begin
      PushChar(s[si]);

      if s[si] = c then
        PushChar(s[si]);
    end;
  end;

  { This routine finds special characters in a byte string.
    If characters were found, the result is true, otherwise
    the result is false.

    Special characters are:

    a) all characters with codes 00..31
    b) the double quote character "
    c) a specified character in the 2-nd argument }

  function FindSpecChar(const s: AnsiString; c: AnsiChar): Boolean;
  var
    i: Integer;
  begin
    for i := 1 to Length(s) do
      if (s[i] = c) or (Ord(s[i]) < 32) or (s[i] = '"') then
      begin
        Result := True;
        Exit;
      end;

    Result := False;
  end;

  { Removes system symbols and returns a count of characters in
    the cleaned up string. System symbols are characters with codes 0..31. }

  function CleanSysSymbols(var s: AnsiString): Integer;
  var
    i, j: Integer;
  begin
    j := 1;

    for i := 1 to Length(s) do
      if Ord(s[i]) > 31 then
      begin
        s[j] := s[i];
        Inc(j);
      end;

    Result := j - 1;
  end;

var
  s:    AnsiString; // buffer
  len:  LongWord;   // Length(Str)
  qn:   LongWord;   // Count of the quote characters in Str
  sep:  AnsiChar;
begin
  { FSeparator must never be empty,
    but if it's empty, the export must not
    crash }
  if FSeparator = '' then
{$IFDEF DELPHI16}
    sep := AnsiChar(FormatSettings.ListSeparator)
{$ELSE}
    sep := AnsiChar(ListSeparator)
{$ENDIF}
  else
    sep := AnsiChar(FSeparator[1]);
  if Str = '' then
  begin
    Result := sep;
    Exit;
  end;

  { Convert to the OEM codepage if it needs }

  if FOEM then
    s := StrToOem(Str)
  else
    s := Str;

  len := Length(s);

  { Suppress ending trash }

  if (len >= 2) and (s[len] = #10) and (s[len - 1] = #13) then
  begin
    Dec(len, 2);
    SetLength(s, len);
  end;

  { Remove system symbols if it needs. }

  if (len > 0) and NoSysSymbols then
  begin
    len := CleanSysSymbols(s);
    SetLength(s, len);
  end;

  if len = 0 then
  begin
    Result := sep;
    Exit;
  end;

  { If the text has no special characters,
    it can be written directly to the output CSV
    document }

  if not FindSpecChar(s, sep) then
  begin
    if not ForcedQuotes then
      Result := s + sep
    else
    begin
      SetLength(Result, Length(s) + 3);
      Result[1] := '"';
      Result[2 + Length(s)] := '"';
      Result[3 + Length(s)] := sep;
      Move(s[1], Result[2], Length(s));
    end;

    Exit;
  end;

  { If the text contains special characters,
    escape all double quotes with additional double quotes and
    enclose the text in double quotes }

  qn := CharCount(s, '"');
  SetLength(Result, len + qn + 3);

  Result[1]             := '"';
  Result[2 + qn + len]  := '"';
  Result[3 + qn + len]  := sep;

  DoubleCharacter(@Result[2], s, '"');
end;

class function TfcxCSVExport.GetDescription: String;
begin
  Result := fcxResources.Get('CSVexport');
end;

procedure TfcxCSVExport.Finish;
begin
  {$IFDEF Delphi_16UP}
  FormatSettings.DecimalSeparator := FOldDecimalSeparator;
  {$ELSE}
  DecimalSeparator := FOldDecimalSeparator;
  {$ENDIF}

  if not Assigned(Stream) then
    FStream.Free;

  if FOpenAfterExport and not Assigned(Stream) then
    OpenDocument(FileName);
end;

function TfcxCSVExport.ShowModal: TModalResult;
begin
  if not Assigned(Stream) then
  begin
    with TfcxCSVExportDialog.Create(nil) do
    begin
      PrepareSaveDialog(SaveDialog);

      OpenCB.Checked := FOpenAfterExport;
      OEMCB.Checked := FOEM;
      edSeparator.Text := FSeparator;

      Result := ShowModal;
      if Result = mrOk then
      begin
        OpenAfterExport := OpenCB.Checked;
        OEMCodepage := OEMCB.Checked;
        Separator := edSeparator.Text;
        if SaveDialog.Execute then
          FileName := SaveDialog.FileName
        else
          Result := mrCancel;
      end;
      Free;
    end;
  end
  else
    Result := mrOk;
end;

function TfcxCSVExport.Start: Boolean;
begin
  if (FileName <> '') or Assigned(Stream) then
  begin
    if (ExtractFilePath(FileName) = '') and (DefaultPath <> '') then
      FileName := DefaultPath + '\' + FileName;
    Result := True;

    {$IFDEF FMX}
      FOldDecimalSeparator := FormatSettings.DecimalSeparator;
      FormatSettings.DecimalSeparator := ',';
    {$ELSE}
      {$IFDEF Delphi_16UP}
        FOldDecimalSeparator := FormatSettings.DecimalSeparator;
        FormatSettings.DecimalSeparator := GetLocaleChar(GetThreadLocale, LOCALE_SDECIMAL, '.');
      {$ELSE}
        FOldDecimalSeparator := DecimalSeparator;
        {$IFNDEF fpc}
          DecimalSeparator := GetLocaleChar(GetThreadLocale, LOCALE_SDECIMAL, '.');
        {$ELSE}
          DecimalSeparator := ',';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    if Assigned(Stream) then
      FStream := Stream
    else
      FStream := TFileStream.Create(FileName, fmCreate);
  end
  else
    Result := False;
end;

procedure TfcxCSVExport.Run;
begin
  if Assigned(Slice) then
    PerformExportSlice
  else
  if Assigned(Cube) then
    PerformExportCube;
end;

{ TfcxCSVExportDialog }

constructor TfcxCSVExportDialog.Create(AOwner: TComponent);
var
  Page: TComponent;
begin
  inherited;

  Caption := fcxResources.Get('CSVExport');
  Page := AddPage(fcxResources.Get('sExport'));
  OEMCB := AddCheckBox(Page, fcxResources.Get('8304'));
  edSeparator := AddEdit(Page);
  AddLabelFor(edSeparator, fcxResources.Get('8853'));
  OpenCB := AddCheckBox(Page, fcxGet(8706));
end;

end.
