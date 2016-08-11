{*******************************************************}
{                                                       }
{            FastCube 2 HTML-export unit                }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxExportHTML;
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
  fcxRes, fcxUtils,fcxTypes, fcxCube, fcxSlice, fcxCustomExport, fcxStyles,
  fcxGraphicUtils, fcxCustomGrid, fcxSliceGrid, fcxExportDialog;
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  // RTL
  System.Classes, System.SysUtils, System.Variants, System.UITypes,
  // FMX
  FMX.Types, FMX.StdCtrls, FMX.ListBox,
  {$IFDEF Delphi_19UP}
  FMX.Graphics,
  {$ENDIF}
  // FC FMX
  FMX.fcxTypes, FMX.fcxSlice, FMX.fcxXML, FMX.fcxCustomExport, FMX.fcxStyles,
  FMX.fcxRes, FMX.fcxGraphicUtils, FMX.fcxCube, FMX.fcxUtils, FMX.fcxExportDialog;
{$ENDIF FMX}

type
  TfcxHTMLFormat = (
    hfHTML, // html style
    hfExcel // excel style
  );

  TfcxHTMLExportDialog = class(TfcxExportDialog)
  private
    OpenCB: TCheckBox;
    cbHTMLFormat: TComboBox;
    cbRepeatValues: TCheckBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TfcxHTMLExport = class(TfcxCustomExportFilter)
  private
    FExportFormat: TfcxHTMLFormat;
    ATempArray: array of AnsiString;
    FRepeatValues: Boolean;
    FOpenAfterExport: Boolean;
    FStream: TStream;
    FOldDecimalSeparator: Char;
    function AnsiToCP(S: AnsiString): AnsiString;
    function GetHTMLCell(RowSpan, ColSpan: Integer; Value: String;
      Style: Integer; Alignment: TAlignment): AnsiString;
    function GetHTMLDataCell(Value: String; Style: Integer; Alignment: TAlignment): AnsiString;
    function GetTR(AString: AnsiString): AnsiString;

    procedure WriteLN(S: AnsiString);
    procedure WriteHeader;
    procedure WriteFooter;
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

    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport default False;
  published
    property HTMLFormat: TfcxHtmlFormat read FExportFormat write FExportFormat;
    property RepeatValues: Boolean read FRepeatValues write FRepeatValues;
  end;

implementation

const
  AlignStr: array[TAlignment] of String = (
    'left',
    'right',
    'center'
  );

{ TfcxHTMLExport }

function TfcxHTMLExport.GetHTMLCell(RowSpan, ColSpan: Integer; Value: String; Style: Integer; Alignment: TAlignment): AnsiString;
const
  CellStr = '  <TD%s align=%s>';
  RowSpanStr = ' ROWSPAN=%d';
  ColSpanStr = ' COLSPAN=%d';
  ClassStr  =  ' class="gs%d"';

  function GetTDStr : String;
  var
    RowSS, ColSS, ClassSS : String;
  begin
    RowSS := '';
    ColSS := '';
    ClassSS := '';
    if RowSpan > 1 then
      RowSS := Format(RowSpanStr, [RowSpan]);
    if ColSpan > 1 then
      ColSS := Format(ColSpanStr, [ColSpan]);
    if Style > 0 then
      ClassSS := Format(ClassStr, [Style]);
    Result := Format(CellStr, [ColSS + RowSS + ClassSS, AlignStr[Alignment]]);
  end;

begin
  if Trim(Value) = EmptyStr then
    Value := '&nbsp;';
  Result := GetTDStr + AnsiToCP(Value) + '</TD>';
end;

function TfcxHTMLExport.GetHTMLDataCell(Value: String; Style: Integer; Alignment: TAlignment): AnsiString;
const
  CellStr = '  <TD%s align=%s>';
  ClassStr =  ' class="gs%d"';

  function GetTDStr: String;
  var
    ClassSS: String;
  begin
    ClassSS := '';
    if Style > 0 then
      ClassSS := Format(ClassStr, [Style]);
    ClassSS := ClassSS + ' NOWRAP';
    Result := Format(CellStr, [ClassSS, AlignStr[Alignment]]);
  end;

begin
  if Trim(Value) = EmptyStr then
    Value := '&nbsp;';
  Result := GetTDStr + AnsiToCP(Value) + '</TD>';
end;

function TfcxHTMLExport.GetTR(AString: AnsiString): AnsiString;
begin
  Result := '<TR>'#$D#$A;
  if Trim(AString) <> '' then
    Result := Result + AString + #$D#$A
  else
    Result := Result + '  <TD>&nbsp;</TD>'#$D#$A;
  Result := Result + '</TR>'#$D#$A;
end;


procedure TfcxHTMLExport.WriteFooter;
begin
  WriteLn('</TABLE>');
  WriteLn('</BODY>');
  WriteLn('</HTML>');
end;

procedure TfcxHTMLExport.WriteHeader;

  procedure WriteFont(AFont: TFont);
  var
    sb, si, su: String;
  begin
    su := '';
    sb := '';
    si := '';
    if {$IFDEF FMX}TFontStyle.{$ENDIF}fsBold in AFont.Style then
      sb := ' font-weight: bold;'
    else
      sb := '';
    if {$IFDEF FMX}TFontStyle.{$ENDIF}fsItalic in AFont.Style then
      si := ' font-style: italic;'
    else
      si := ' font-style: normal;';

    if {$IFDEF FMX}TFontStyle.{$ENDIF}fsUnderline in AFont.Style then
      su := ' text-decoration: underline';
    if {$IFDEF FMX}TFontStyle.{$ENDIF}fsStrikeout in AFont.Style then
    begin
      if su = '' then
        su := ' text-decoration: line-through'
      else
        su := su + ' | line-through';
    end;
    if su <> '' then
      su := su + ';';
    WriteLn(' font-family: ' + AFont.Name + ';'#13#10 +
      ' font-size: ' + IntToStr(Round(AFont.Size * 96 / 72)) + 'px;'#13#10 +
      sb + si + su);
  end;

  procedure WriteStyle(StyleID: Integer; Style: TfcxCustomThemeStyle);
  begin
    WriteLn('.gs' + IntToStr(StyleID) + ' {');
    WriteLn('  background: ' + HTMLRGBColor(Style.FillColor) + ';');
    WriteLn('  color: ' + HTMLRGBColor(Style.TextColor) + ';');
    WriteLn('  vertical-align: top;');
    {$IFDEF FPC}
    if not Style.Font.IsDefault then
    {$ENDIF}
      WriteFont(Style.Font);
    if (HTMLFormat = hfExcel) and not (StyleID in [gsDataArea, gsDataCells, gsDataCellsTotals, gsDataCellsSelected]) then
      WriteLn('  mso-number-format:"\@";'); // text format
    WriteLn('}');
  end;

var
  i: integer;
begin
  if HTMLFormat = hfExcel then
  begin
    WriteLn(
      '<html xmlns:o="urn:schemas-microsoft-com:office:office"'#$D#$A+
      'xmlns:x="urn:schemas-microsoft-com:office:excel"'#$D#$A+
      'xmlns="http://www.w3.org/TR/REC-html40">'#$D#$A);
  end;
  WriteLn('<HTML>');
  WriteLn('<HEAD>');
  WriteLn('<TITLE>'+AnsiToCP(GetTitle(rtReportHeader))+'</TITLE>');

  WriteLn('<META content="text/html; charset=utf-8" http-equiv=Content-Type>');
  if HTMLFormat = hfExcel then
  begin
    WriteLn(
      '<!--[if gte mso 9]><xml>' + #$D#$A +
      ' <x:ExcelWorkbook>' + #$D#$A +
      ' <x:ExcelWorksheets>' + #$D#$A +
      ' <x:ExcelWorksheet>' + #$D#$A +
      ' <x:Name>'+AnsiToCP(GetTitle(rtWorksheet))+'</x:Name>' + #$D#$A +
      ' <x:WorksheetOptions>' + #$D#$A +
      ' <x:Selected/>' + #$D#$A +
      ' </x:WorksheetOptions>' + #$D#$A +
      ' </x:ExcelWorksheet>' + #$D#$A +
      ' </x:ExcelWorksheets>' + #$D#$A +
      ' </x:ExcelWorkbook>' + #$D#$A +
      '</xml><![endif]-->' + #$D#$A#$D#$A);
  end;

  WriteLn( '<STYLE type="text/css">');
  for i := Styles.FirstStyleIndex to Styles.LastStyleIndex do
    WriteStyle(i, Styles[i]);
  WriteLn('</STYLE>');
  WriteLn('</HEAD>');
  WriteLn('<BODY class="olap0">');
  WriteLn('<TABLE border=1 CELLPADDING=2 CELLSPACING=0><CAPTION>'+AnsiToCP(GetTitle(rtDataHeader))+'</CAPTION>');
end;

procedure TfcxHTMLExport.WriteLN(S: AnsiString);
begin
  S := S + #$D#$A;
  FStream.Write(S[1], Length(S));
end;

function TfcxHTMLExport.XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  AStr: AnsiString;
  i, j: integer;
begin
  if RepeatValues then
  begin
    AStr := GetHTMLCell(
      1,
      1,
      ARec.Text,
      gsHeaderCells,
      ARec.Alignment
    );
    for j := 1 to ARec.TreeRect.SizeLevel do
      for i := ARec.TreeRect.Cell to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
        ATempArray[ARec.TreeRect.Level + j] := ATempArray[ARec.TreeRect.Level + j] + #13#10 + AStr;
  end
  else
  begin
    AStr := GetHTMLCell(
      ARec.TreeRect.SizeLevel,
      ARec.TreeRect.SizeCell,
      ARec.Text,
      gsHeaderCells,
      ARec.Alignment
    );
    ATempArray[ARec.TreeRect.Level + 1] := ATempArray[ARec.TreeRect.Level + 1] + #13#10 + AStr;
  end;
  Result := False;
end;

function TfcxHTMLExport.YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  AStr: AnsiString;
  i, j: integer;
begin
  if RepeatValues then
  begin
    AStr := GetHTMLCell(
      1,
      1,
      ARec.Text,
      gsHeaderCells,
      ARec.Alignment
    );
    for i := ARec.TreeRect.Cell to ARec.TreeRect.Cell + ARec.TreeRect.SizeCell - 1 do
      for j := 1 to ARec.TreeRect.SizeLevel do
        ATempArray[i] := AStr + #13#10 + ATempArray[i]
  end
  else
  begin
    AStr := GetHTMLCell(
      ARec.TreeRect.SizeCell,
      ARec.TreeRect.SizeLevel,
      ARec.Text,
      gsHeaderCells,
      ARec.Alignment
    );
    ATempArray[ARec.TreeRect.Cell] := AStr + #13#10 + ATempArray[ARec.TreeRect.Cell];
  end;
  Result := False;
end;

constructor TfcxHTMLExport.Create(AOwner: TComponent);
begin
  inherited;
  FilterDesc := fcxGet(8210);
  DefaultExt := fcxGet(8211);
  FRepeatValues := False;
end;

procedure TfcxHTMLExport.PerformExportCube;

  function GetCellAlignment(Field: TfcxCommonField): TAlignment;
  begin
    case Field.DataType of
      fcdt_String, fcdt_WideString: Result := taLeftJustify;
    else
      Result := taRightJustify;
    end;
  end;

var
  i, j, Row: Integer;
  S, CellStr: String;
  Value: Variant;
  Columns: TfcxCubeDataColumns;
begin
  Columns := DoGetCubeCols;
  S := '';
  for i := 0 to Columns.VisibleCount - 1 do
    S := S + #$D#$A + GetHTMLCell(1, 1, Columns[i].Field.CubeFieldDisplayLabel, gsHeaderCells, taCenter);
  WriteLn(GetTR(S));

  for i := 0 to DoGetCubeRowCount - 1 do
  begin
    S := '';
    for j := 0 to Columns.VisibleCount - 1 do
    begin
      Row := DoGetCubeRowIndex(i);
      Value := Cube.SourceHolder.UniqueValueAsVariant[Row, Columns[j].Field];
      if (HTMLFormat = hfExcel) and VarIsNumeric(Value) then
        CellStr := FloatToStr(Value)
      else
        CellStr := Cube.SourceHolder.UniqueValueCaption[Row, Columns[j].Field];
      S := S + #$D#$A + GetHTMLDataCell(CellStr, gsDataCells, GetCellAlignment(Columns[j].Field));
    end;
    WriteLn(GetTR(S));
    DoProgress(i);
  end;
end;

procedure TfcxHTMLExport.PerformExportSlice;
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
      ATempArray[0] := GetHTMLCell(ATopRows-1, Slice.YAxisContainer.VisibleLevelCount, '', gsHeaderArea, taCenter);
    for j := 0 to Slice.YAxisContainer.RealLevelCount - 1 do
    begin
      if (Slice.YAxisContainer.LevelInfo[j].IsVisible) then
      begin
        ATempArray[ATopRows-1] := ATempArray[ATopRows-1] + #13#10;
        if Slice.YAxisContainer.LevelInfo[j].IsMeasure then
          ATempArray[ATopRows-1] := ATempArray[ATopRows-1] + GetHTMLCell(1, 1, Slice.MeasuresContainer.Caption, gsMeasure, taCenter)
        else
          ATempArray[ATopRows-1] := ATempArray[ATopRows-1] + GetHTMLCell(1, 1, Slice.YAxisContainer.LevelInfo[j].RegionField.Caption, gsActiveDimension, taCenter);
      end;
    end;
  end;

  // column title
  for i := 0 to Slice.XAxisContainer.RealLevelCount - 1 do
    if (Slice.XAxisContainer.LevelInfo[i].IsVisible) then
    begin
      ATempArray[0] := ATempArray[0] + #13#10;
      if Slice.XAxisContainer.LevelInfo[i].IsMeasure then
        ATempArray[0] := ATempArray[0] + GetHTMLCell(1, 1, Slice.MeasuresContainer.Caption, gsMeasure, taCenter)
      else
        ATempArray[0] := ATempArray[0] + GetHTMLCell(1, 1, Slice.XAxisContainer.LevelInfo[i].RegionField.Caption, gsActiveDimension, taCenter);
    end;

   // column header
  Slice.XAxisContainer.TraverseAxis(0, Slice.XAxisContainer.VisibleLevelCount - 1, 0, XAxisDrawProc);

  for i := 0 to ATopRows - 1 do
    WriteLn(GetTR(ATempArray[i]));

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
          if (HTMLFormat = hfExcel) and VarIsNumeric(Cell.Value) then
            S := FloatToStr(Cell.Value)
          else
            S := Cell.StrValue;

          ATempString := ATempString + #13#10;
          if Cell.IsTotal or Cell.IsGrandTotal then
            ATempString := ATempString + GetHTMLDataCell(S, gsDataCellsTotals, Cell.Alignment)
          else
            ATempString := ATempString + GetHTMLDataCell(S, gsDataCells, Cell.Alignment);
        end;
      WriteLN(GetTR(ATempString));
      DoProgress(i);
    end;
  end;
end;

function TfcxHTMLExport.AnsiToCP(S: AnsiString): AnsiString;
begin
  Result := StrPas(PAnsiChar(AnsiToUtf8(S)))
end;

class function TfcxHTMLExport.GetDescription: String;
begin
  Result := fcxResources.Get('HTMLexport');
end;

procedure TfcxHTMLExport.Finish;
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

function TfcxHTMLExport.ShowModal: TModalResult;
begin
  if not Assigned(Stream) then
  begin
    with TfcxHTMLExportDialog.Create(nil) do
    begin
      PrepareSaveDialog(SaveDialog);

      OpenCB.Checked := FOpenAfterExport;
      cbHTMLFormat.ItemIndex := Ord(HTMLFormat);
      cbRepeatValues.Checked := RepeatValues;
      cbRepeatValues.Enabled := Assigned(Slice);

      Result := ShowModal;
      if Result = mrOk then
      begin
        OpenAfterExport := OpenCB.Checked;
        HTMLFormat := TfcxHTMLFormat(cbHTMLFormat.ItemIndex);
        RepeatValues := cbRepeatValues.Checked;
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

function TfcxHTMLExport.Start: Boolean;
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

procedure TfcxHTMLExport.Run;
begin
  WriteHeader;
  if Assigned(Slice) then
    PerformExportSlice
  else
  if Assigned(Cube) then
    PerformExportCube;
  WriteFooter;
end;

{ TfcxHTMLExportDialog }

constructor TfcxHTMLExportDialog.Create(AOwner: TComponent);
var
  Page: TComponent;
begin
  inherited;

  Caption := fcxResources.Get('HTMLExport');
  Page := AddPage(fcxResources.Get('sExport'));
  cbHTMLFormat := AddComboBox(Page, ['HTML table', 'Excel Worksheet HTML']);
  AddLabelFor(cbHTMLFormat, fcxResources.Get('sHTMLFormat') + ':', False);
  OpenCB := AddCheckBox(Page, fcxGet(8706));
  cbRepeatValues := AddCheckBox(Page, fcxResources.Get('sRepeatAxesValues'));
end;

end.
