
{******************************************}
{                                          }
{                FastCube 2                }
{        Open Document Format export       }
{                                          }
{         Copyright (c) 1998-2008          }
{          by Alexander Fediachov,         }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxExportODF;
{$I fcx.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  {$IFDEF DELPHI_6UP}
  Variants,
  {$ENDIF}
  fcxXML, fcxZip, fcxCustomExport, fcxFormats, fcxCube, fcxSlice,
  fcxUtils, fcxGraphicUtils, fcxRes,
  fcxCustomGrid, fcxSliceGrid, fcxStyles, fcxExportDialog;
{$ELSE FMX}
{$INCLUDE fcx.inc}
interface

uses
  // RTL
  System.SysUtils, System.Classes, System.UITypes, System.Variants,
  // FMX
  FMX.StdCtrls, FMX.Dialogs,
  // FC FMX
  FMX.fcxRes, FMX.fcxUtils, FMX.fcxXML, FMX.fcxFormats, FMX.fcxZip,
  FMX.fcxCube, FMX.fcxSlice, FMX.fcxCustomExport, FMX.fcxStyles,
  FMX.fcxGraphicUtils, FMX.fcxExportDialog;
{$ENDIF}

type
  TfcxODFExportDialog = class(TfcxExportDialog)
  private
    OpenCB: TCheckBox;
    cbRepeatValues: TCheckBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TfcxODFStyleRec = record
    ID: Integer;
    Alignment: TAlignment;
    Format: TfcxFormat;
  end;

  TfcxODFStyleArray = array of TfcxODFStyleRec;
  TfcxODFStyleCache = class(TObject)
  private
    FStyles: array of TfcxODFStyleArray;
    FStyleNode: TfcxXMLItem;
    FLastStyleIndex: Integer;
    FExportType: String;
    procedure SetStyleNode(const Value: TfcxXMLItem);  // this node contains ODF styles
  public
    procedure AddNumberStyle(Item: TfcxXMLItem; const StyleName, Fmt: string);
    function CreateStyle(Style: TfcxCustomThemeStyle; Alignment: TAlignment; Format: TfcxFormat): Integer;
    function GetStyleID(AStyle: TfcxCustomThemeStyle; AAlignment: TAlignment; AFormat: TfcxFormat): Integer;
    property StyleNode: TfcxXMLItem read FStyleNode write SetStyleNode;
  end;

  TfcxODFExport = class(TfcxCustomExportFilter)
  private
    FExportStyles: Boolean;
    FOpenAfterExport: Boolean;
    FCreator: String;
    FEmptyLines: Boolean;
    FTempFolder: String;
    FExportType: String;
    FLanguage: string;
    FCreationTime: TDateTime;
    FRowStyleNames: TStrings;
    FExportTitle: String;
    FStyleCache: TfcxODFStyleCache;
    FPageNode: TfcxXMLItem;
    FRows: array of TfcxXMLItem;
    FRepeatValues: Boolean;

    function OdfPrepareString(const Str: WideString): WideString;
    procedure OdfMakeHeader(const Item: TfcxXMLItem);
    procedure OdfCreateMeta(const FileName: String; const Creator: String);
    procedure OdfCreateManifest(const FileName: String; const MValue: String);
    procedure OdfCreateMime(const FileName: String; const MValue: String);
    procedure ExportBody(BodyNode: TfcxXMLItem);
    procedure CreateStyles;
    function CreateRowStyle(Row: Integer): string;

    // Exports a row from the matrix to an ODF node.

    procedure ExportRows;

    // Creates an ODF cell with text or a picture.

    // Creates an adjacent empty cell.
    // Such cells are created when an object covers several
    // cells: one cell contains the object's data, the other
    // are adjacent empty cells.

    procedure CreateAdjacentCell(Node: TfcxXMLItem; Columns: Integer = 1);

    procedure CreateDataCell(Node: TfcxXMLItem; Value: Variant; StyleIndex: Integer;
      DisplayFormat: TfcxFormat; RowsSpanned, ColsSpanned: Integer);
    function GetRow(RowIndex: Integer): TfcxXMLItem;
    procedure SetPageNode(const Value: TfcxXMLItem);
  protected
    procedure ExportPage(Stream: TStream);
    procedure ExportCubeRows;
    procedure ExportSliceRows;
    function XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    property PageNode: TfcxXMLItem read FPageNode write SetPageNode;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDescription: String; override;
    class function ProcessSpaces(const s: WideString): WideString;
    class function ProcessTabs(const s: WideString): WideString;
    class function ProcessControlSymbols(const s: WideString): WideString;

    function ShowModal: TModalResult; override;
    function Start: Boolean; override;
    procedure Run; override;
    procedure Finish; override;
    property ExportType: String read FExportType write FExportType;
    property ExportTitle: String read FExportTitle write FExportTitle;
  published
    property ExportStyles: Boolean read FExportStyles write FExportStyles default True;
    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport default False;
    property Creator: String read FCreator write FCreator;
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property EmptyLines: Boolean read FEmptyLines write FEmptyLines default True;
    property Language: string read FLanguage write FLanguage; {default 'en'}
    property RepeatValues: Boolean read FRepeatValues write FRepeatValues default False;
    property OverwritePrompt;
  end;

  TfcxODSExport = class(TfcxODFExport)
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
  published
    property ExportStyles;
    property OpenAfterExport;
    property ShowProgress;
    property Creator;
    property EmptyLines;
    property OverwritePrompt;
  end;

  TfcxODTExport = class(TfcxODFExport)
  public
    constructor Create(AOwner: TComponent); override;
    class function GetDescription: String; override;
  published
    property ExportStyles;
    property OpenAfterExport;
    property ShowProgress;
    property Creator;
    property EmptyLines;
    property OverwritePrompt;
  end;

implementation

const
  odfDivider = 38.5;
  odfPageDiv = 37.8;
  odfMargDiv = 10;
  odfHeaderSize = 21;
  odfRep = 'urn:oasis:names:tc:opendocument:xmlns:';

var
  odfXMLHeader: array[0..odfHeaderSize - 1] of array [0..1] of String = (
  ('xmlns:office', odfRep + 'office:1.0'),
  ('xmlns:style', odfRep + 'style:1.0'),
  ('xmlns:text', odfRep + 'text:1.0'),
  ('xmlns:table', odfRep + 'table:1.0'),
  ('xmlns:draw', odfRep + 'drawing:1.0'),
  ('xmlns:fo', odfRep + 'xsl-fo-compatible:1.0'),
  ('xmlns:xlink', 'http://www.w3.org/1999/xlink'),
  ('xmlns:dc', 'http://purl.org/dc/elements/1.1/'),
  ('xmlns:meta', odfRep + 'meta:1.0'),
  ('xmlns:number', odfRep + 'datastyle:1.0'),
  ('xmlns:svg', odfRep + 'svg-compatible:1.0'),
  ('xmlns:chart', odfRep + 'chart:1.0'),
  ('xmlns:dr3d', odfRep + 'dr3d:1.0'),
  ('xmlns:math', 'http://www.w3.org/1998/Math/MathML'),
  ('xmlns:form', odfRep + 'form:1.0'),
  ('xmlns:script', odfRep + 'script:1.0'),
  ('xmlns:dom', 'http://www.w3.org/2001/xml-events'),
  ('xmlns:xforms', 'http://www.w3.org/2002/xforms'),
  ('xmlns:xsd', 'http://www.w3.org/2001/XMLSchema'),
  ('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance'),
  ('xmlns:msoxl', 'http://schemas.microsoft.com/office/excel/formula'));

type
  TSpanStyle = class
  public
    Color:    TColor;
    Style:    TFontStyles;
    Name:     WideString;
  end;

  TAnsiCharSet = set of AnsiChar;

function FilterStr(const Src: string; const Chars: TAnsiCharSet): string;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to Length(Src) do
    if AnsiChar(Src[i]) in Chars then
      Result := Result + Src[i];
end;

{ TfcxODFExport }

constructor TfcxODFExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExportStyles := True;
  FCreator := 'FastReport';
  FEmptyLines := True;
  FLanguage := 'en';
  FRowStyleNames := TStringList.Create;
  TStringList(FRowStyleNames).Sorted := True;
  FStyleCache := TfcxODFStyleCache.Create;
end;

class function TfcxODFExport.GetDescription: String;
begin
  Result := '';
end;

procedure TfcxODFExport.OdfCreateMeta(const FileName: String; const Creator: String);
var
  XML: TfcxXMLDocument;
begin
  XML := TfcxXMLDocument.Create;
  try
    XML.AutoIndent := False;
    XML.Root.Name := 'office:document-meta';
    XML.Root.Prop['office:version'] := '1.1';
    XML.Root.Prop['xmlns:xlink'] := 'http://www.w3.org/1999/xlink';
    XML.Root.Prop['xmlns:dc'] := 'http://purl.org/dc/elements/1.1/';
    XML.Root.Prop['xmlns:meta'] := 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
    XML.Root.Prop['xmlns:office'] := 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
    with XML.Root.Add do
    begin
      Name := 'office:meta';
      with Add do
      begin
        Name := 'meta:generator';
        Value := 'fast-report.com/Fast cube/build:' + FCX_VERSION;
      end;
      with Add do
      begin
        Name := 'meta:initial-creator';
        Value := string(Creator);
      end;
      with Add do
      begin
        Name := 'meta:creation-date';
        Value := FormatDateTime('YYYY-MM-DD', CreationTime) + 'T' +
          FormatDateTime('HH:MM:SS', CreationTime);
      end;
    end;
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

procedure TfcxODFExport.OdfCreateMime(const FileName: String; const MValue: String);
var
  f: TFileStream;
  s: AnsiString;
begin
  f := TFileStream.Create(FileName, fmCreate);
  try
    s := AnsiString('application/vnd.oasis.opendocument.' + MValue);
    f.Write(s[1], Length(s));
  finally
    f.Free;
  end;
end;

procedure TfcxODFExport.OdfCreateManifest(const FileName: String; const MValue: String);
var
  XML: TfcxXMLDocument;
begin
  XML := TfcxXMLDocument.Create;
  try
    XML.AutoIndent := False;
    XML.Root.Name := 'manifest:manifest';
    XML.Root.Prop['xmlns:manifest'] := 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
    with XML.Root.Add do
    begin
      Name := 'manifest:file-entry';
      Prop['manifest:media-type'] := 'application/vnd.oasis.opendocument.' + MValue;
      Prop['manifest:full-path'] := '/';
    end;
    with XML.Root.Add do
    begin
      Name := 'manifest:file-entry';
      Prop['manifest:media-type'] := 'text/xml';
      Prop['manifest:full-path'] := 'content.xml';
    end;
    with XML.Root.Add do
    begin
      Name := 'manifest:file-entry';
      Prop['manifest:media-type'] := 'text/xml';
      Prop['manifest:full-path'] := 'styles.xml';
    end;
    with XML.Root.Add do
    begin
      Name := 'manifest:file-entry';
      Prop['manifest:media-type'] := 'text/xml';
      Prop['manifest:full-path'] := 'meta.xml';
    end;
    with XML.Root.Add do
    begin
      Name := 'manifest:file-entry';
      Prop['manifest:media-type'] := 'text/plain';
      Prop['manifest:full-path'] := 'mimetype';
    end;
    with XML.Root.Add do
    begin
      Name := 'manifest:file-entry';
      Prop['manifest:media-type'] := 'text/xml';
      Prop['manifest:full-path'] := 'META-INF/manifest.xml';
    end;
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

function TfcxODFExport.OdfPrepareString(const Str: WideString): WideString;
var
  i: Integer;
  s: WideString;
begin
  Result := '';
  s := Str;
  if Copy(s, Length(s) - 1, 4) = #13#10 then
    Delete(s, Length(s) - 1, 4);
  for i := 1 to Length(s) do
  begin
    if s[i] = '&' then
      Result := Result + '&amp;'
    else
    if s[i] = '"' then
      Result := Result + '&quot;'
    else if s[i] = '<' then
      Result := Result + '&lt;'
    else if s[i] = '>' then
      Result := Result + '&gt;'
    else if (s[i] <> #10) then
      Result := Result + s[i]
  end;
end;

class function TfcxODFExport.ProcessSpaces(const s: WideString): WideString;

  function Ch(i: Integer): WideChar;
  begin
    if (i > 0) and (i <= Length(s)) then
      Result := s[i]
    else
      Result := #0;
  end;

  function Ts(n: Integer): WideString;
  begin
    if n = 1 then
      Result := '<text:s/>'
    else
      Result := '<text:s text:c="' + IntToStr(n) + '"/>';
  end;

const
  Space: WideChar = ' ';
var
  f, i: Integer;
begin
  if s = '' then
  begin
    Result := '';
    Exit;
  end;

  f := 1;
  Result := '';

  for i := 1 to Length(s) do
    if (s[f] = Space) <> (s[i] = Space) then
      if s[f] = Space then
      begin
        Result := Result + Ts(i - f);
        f := i;
      end
      else if (i > 0) and (s[i - 1] = Space) or (i = Length(s)) then
      begin
        Result := Result + Copy(s, f, i - f);
        f := i;
      end;

  i := Length(s) + 1;

  if s[f] = Space then
    Result := Result + Ts(i - f)
  else
    Result := Result + Copy(s, f, i - f);
end;

class function TfcxODFExport.ProcessTabs(const s: WideString): WideString;
begin
  Result := StringReplace(s, #9, '<text:tab/>', [rfReplaceAll]);
end;

class function TfcxODFExport.ProcessControlSymbols(const s: WideString): WideString;
var
  i, j: Integer;
begin
  SetLength(Result, Length(s));
  i := 0;

  for j := 1 to Length(s) do
    if Ord(s[j]) > 31 then
    begin
      i := i + 1;
      Result[i] := s[j];
    end;

  SetLength(Result, i);
end;

procedure TfcxODFExport.OdfMakeHeader(const Item: TfcxXMLItem);
var
  i: Integer;
begin
  for i := 0 to odfHeaderSize - 1 do
    Item.Prop[odfXMLHeader[i][0]] := odfXMLHeader[i][1];
end;

procedure TfcxODFExport.CreateDataCell(Node: TfcxXMLItem; Value: Variant; StyleIndex: Integer; DisplayFormat: TfcxFormat; RowsSpanned, ColsSpanned: Integer);
var
  k: Integer;
  sl: TStringList;
  s, s2: WideString;
  LineNode: TfcxXMLItem;
begin
  s := VarToStr(Value);
  s := OdfPrepareString(s);
  s := ProcessSpaces(s);
  s := ProcessTabs(s);
  s := ProcessControlSymbols(s);
  with Node do
  begin
    Name := 'table:table-cell';
    Prop['table:style-name'] := 'ce' + IntToStr(StyleIndex);

    if (RowsSpanned > 1) or (ColsSpanned > 1) then
    begin
      Prop['table:number-columns-spanned'] := IntToStr(ColsSpanned);
      Prop['table:number-rows-spanned'] := IntToStr(RowsSpanned);
    end;

    if not Assigned(DisplayFormat) or (DisplayFormat.TypeFormat.Kind <> fkNumeric) then
    begin
      Prop['office:value-type'] := 'string';

      sl := TStringList.Create;
      sl.Text := s;

      for k := 0 to sl.Count - 1 do
      begin
        LineNode := Add;
        LineNode.Name := 'text:p';

        if ExportType = 'text' then
          LineNode.Prop['text:style-name'] := 'p' + IntToStr(StyleIndex);

        LineNode.Value := sl[k];
      end;

      sl.Free;
    end
    else
    begin
      s2 := '';

      for k := 1 to Length(s) do
        if AnsiChar(s[k]) in ['0'..'9', '-'] then
          s2 := s2 + s[k]
        else
          if (Char(s[k]) = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then
            s2 := s2 + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;

      Prop['office:value-type'] := 'float';
      Prop['office:value'] := string(
          StringReplace(s2, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', [rfReplaceAll]));

      with Add do
      begin
        Name := 'text:p';
        Value := s;

        if ExportType = 'text' then
          Prop['text:style-name'] := 'p' + IntToStr(StyleIndex);
      end;
    end;
  end;
end;

function TfcxODFExport.CreateRowStyle(Row: Integer): string;
begin
  Result := 'ro1';

  if FRowStyleNames.IndexOf(Result) >= 0 then
    Exit;

  FRowStyleNames.Add(Result);

  with FStyleCache.StyleNode.Add do
  begin
    Name := 'style:style';

    Prop['style:name'] := Result;
    Prop['style:family'] := 'table-row';

    with Add do
    begin
      Name := 'style:table-row-properties';

      Prop['fo:break-before'] := 'auto';
      Prop['style:use-optimal-row-height'] := 'false';
      Prop['style:row-height'] := '15pt';
    end;
  end;
end;

procedure TfcxODFExport.CreateStyles;
var
  XML: TfcxXMLDocument;
begin
  XML := TfcxXMLDocument.Create;
  try
    XML.AutoIndent := False;
    XML.Root.Name := 'office:document-styles';
    OdfMakeHeader(XML.Root);

    with XML.Root.Add do
    begin
      Name := 'office:styles';

      with Add do
      begin
        Name := 'style:default-style';
        Prop['style:family'] := 'paragraph';
      end;

      with Add do
      begin
        Name := 'style:paragraph-properties';

        Prop['style:text-autospace'] := 'ideograph-alpha';
        Prop['style:punctuation-wrap'] := 'hanging';
        Prop['style:line-break'] := 'strict';
        Prop['style:writing-mode'] := 'page';
      end;

      with Add do
      begin
        Name := 'style:text-properties';

        Prop['fo:color'] := '#000000';
        Prop['style:font-name'] := 'Times New Roman';
        Prop['fo:font-size'] := '12pt';
        Prop['fo:language'] := LowerCase(Language);
        Prop['fo:country'] := UpperCase(Language);
        Prop['style:font-name-asian'] := 'Arial Unicode MS';
        Prop['style:font-size-asian'] := '12pt';
        Prop['style:language-asian'] := LowerCase(Language);
        Prop['style:country-asian'] := UpperCase(Language);
        Prop['style:font-name-complex'] := 'Tahoma1';
        Prop['style:font-size-complex'] := '12pt';
        Prop['style:language-complex'] := LowerCase(Language);
        Prop['style:country-complex'] := UpperCase(Language);
      end;
    end;

    with XML.Root.Add do
    begin
      Name := 'office:automatic-styles';

      with Add do
      begin
        Name := 'style:page-layout';
        Prop['style:name'] := 'pm1';
        with Add do
        begin
          Name := 'style:page-layout-properties';
          Prop['style:print'] := 'objects charts drawings';
          Prop['style:table-centering'] := 'none';
          Prop['fo:margin-top'] := '0.3in';
          Prop['fo:margin-bottom'] := '0.3in';
          Prop['fo:margin-left'] := '0.7in';
          Prop['fo:margin-right'] := '0.7in';
        end;
      end;
    end;

    with XML.Root.Add do
    begin
      Name := 'office:master-styles';
      with Add do
      begin
        Name := 'style:master-page';
        Prop['style:name'] := 'PageDef';
        Prop['style:page-layout-name'] := 'pm1';
        with Add do
          Name := 'style:header';
        with Add do
        begin
          Name := 'style:header-left';
          BoolProp['style:display'] := False;
        end;
        with Add do
          Name := 'style:footer';
        with Add do
        begin
          Name := 'style:footer-left';
          BoolProp['style:display'] := False;
        end;
      end;
    end;
    XML.SaveToFile(FTempFolder + 'styles.xml');
  finally
    XML.Free;
  end;
end;

function TfcxODFExport.ShowModal: TModalResult;
begin
  if not Assigned(Stream) then
  begin
    with TfcxODFExportDialog.Create(nil) do
    begin
      Caption := ExportTitle;
      PrepareSaveDialog(SaveDialog);

      OpenCB.Checked := OpenAfterExport;
      cbRepeatValues.Checked := RepeatValues;

      Result := ShowModal;

      if Result = mrOk then
      begin
        OpenAfterExport := OpenCB.Checked;
        RepeatValues := cbRepeatValues.Checked;

        CreationTime := Now;
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

function TfcxODFExport.Start: Boolean;
begin
  if (FileName <> '') or Assigned(Stream) then
  begin
    if (ExtractFilePath(FileName) = '') and (DefaultPath <> '') then
      FileName := DefaultPath + '\' + FileName;
    Result := True
  end
  else
    Result := False;
end;

procedure TfcxODFExport.ExportCubeRows;
var
  i, j, Row, StyleIndex: Integer;
  Value: Variant;
  Columns: TfcxCubeDataColumns;
  Node: TfcxXMLItem;
begin
  Columns := DoGetCubeCols;
  Node := PageNode.Add;
  Node.Name := 'table:table-column';
  Node.Prop['table:style-name'] := 'co1';
  Node.IntProp['table:number-columns-repeated'] := Columns.VisibleCount;

  Node := PageNode.Add;
  Node.Name := 'table:table-row';
  Node.Prop['table:style-name'] := CreateRowStyle(0);
  StyleIndex := FStyleCache.GetStyleID(Styles[gsHeaderCells], taCenter, nil);
  for i := 0 to Columns.VisibleCount - 1 do
    CreateDataCell(Node.Add, Columns[i].Field.CubeFieldDisplayLabel, StyleIndex, nil, 1, 1);

  for i := 0 to DoGetCubeRowCount - 1 do
  begin
    Node := PageNode.Add;
    Node.Name := 'table:table-row';
    Node.Prop['table:style-name'] := CreateRowStyle(i + 1);
    for j := 0 to Columns.VisibleCount - 1 do
    begin
      Row := DoGetCubeRowIndex(i);
      Value := Cube.SourceHolder.UniqueValueAsVariant[Row, Columns[j].Field];
      if VarIsNumeric(Value) then
        StyleIndex := FStyleCache.GetStyleID(Styles[gsDataCells], taRightJustify, Columns[j].Field.DisplayFormat)
      else
        StyleIndex := FStyleCache.GetStyleID(Styles[gsDataCells], taLeftJustify, Columns[j].Field.DisplayFormat);
      CreateDataCell(Node.Add, Value, StyleIndex, Columns[j].Field.DisplayFormat, 1, 1);
    end;
    DoProgress(i);    
  end;
end;

procedure TfcxODFExport.ExportSliceRows;
var
  Col, Row, MaxCol, MaxRow: Integer;
  Style1, Style2: Integer;
  MeasureCell: TfcxMeasureCell;
  Node: TfcxXMLItem;
  varvalue: Variant;
  Format: TfcxFormat;
begin
  // write column info
  Node := PageNode.Add;
  Node.Name := 'table:table-column';
  Node.Prop['table:style-name'] := 'co1';
  Node.IntProp['table:number-columns-repeated'] := Slice.YAxisContainer.VisibleLevelCount + Slice.ColCount;

  // add header
  Style1 := FStyleCache.GetStyleID(Styles[gsHeaderArea], taCenter, nil);
  Style2 := FStyleCache.GetStyleID(Styles[gsActiveDimension], taLeftJustify, nil);

  Node := GetRow(0);
  // row title
  if Slice.XAxisContainer.RealLevelCount = 0 then
  begin
    MaxCol := Slice.YAxisContainer.VisibleLevelCount - 1;
    MaxRow := Slice.XAxisContainer.VisibleLevelCount;
    CreateDataCell(Node.Add, Null, Style1, nil, MaxRow + 1, MaxCol + 1);
    if MaxCol > 0 then
      CreateAdjacentCell(Node.Add, MaxCol);
    for Row := 1 to MaxRow do
      CreateAdjacentCell(GetRow(Row).Add, MaxCol + 1);
  end else
  begin
    MaxCol := Slice.YAxisContainer.VisibleLevelCount - 1;
    MaxRow := Slice.XAxisContainer.VisibleLevelCount - 1;
    CreateDataCell(Node.Add, Null, Style1, nil, MaxRow + 1, MaxCol + 1);
    if MaxCol > 0 then
      CreateAdjacentCell(Node.Add, MaxCol);
    for Row := 1 to MaxRow do
      CreateAdjacentCell(GetRow(Row).Add, MaxCol + 1);
    for Col := 0 to Slice.YAxisContainer.RealLevelCount - 1 do
    begin
      if (Slice.YAxisContainer.LevelInfo[Col].IsVisible) then
      begin
        Node := GetRow(MaxRow + 1).Add;
        if Slice.YAxisContainer.LevelInfo[Col].IsMeasure then
          CreateDataCell(Node, Slice.MeasuresContainer.Caption, FStyleCache.GetStyleID(Styles[gsMeasure], taLeftJustify, nil), nil, 1, 1)
        else
          CreateDataCell(Node, Slice.YAxisContainer.LevelInfo[Col].RegionField.Caption, Style2, nil, 1, 1);
      end;
    end;
  end;

  // column title
  Node := GetRow(0);
  for Col := 0 to Slice.XAxisContainer.RealLevelCount - 1 do
    if (Slice.XAxisContainer.LevelInfo[Col].IsVisible) then
    begin
      if Slice.XAxisContainer.LevelInfo[Col].IsMeasure then
        CreateDataCell(Node.Add, Slice.MeasuresContainer.Caption, FStyleCache.GetStyleID(Styles[gsMeasure], taLeftJustify, nil), nil, 1, 1)
      else
        CreateDataCell(Node.Add, Slice.XAxisContainer.LevelInfo[Col].RegionField.Caption, Style2, nil, 1, 1);
    end;

  // column header
  Slice.XAxisContainer.TraverseAxis(0, Slice.XAxisContainer.VisibleLevelCount - 1, 0, XAxisDrawProc);

  // row header
  Slice.YAxisContainer.TraverseAxis(0, Slice.YAxisContainer.VisibleLevelCount - 1, 0, YAxisDrawProc);

  // data
  for Row := 0 to Slice.RowCount - 1 do
  begin
    Node := GetRow(Row + Slice.XAxisContainer.VisibleLevelCount + 1);
    for Col := 0 to Slice.ColCount - 1 do
    begin
      Slice.GetMeasureCell(Col, Row, Measurecell);
      if (MeasureCell.MeasureIndex >= 0) and ((Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayAs in da_Percents) or
        (Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayAs in da_Ranks))then
      begin
        varvalue := MeasureCell.StrValue;
        Format := nil;
      end
      else
      begin
        varvalue := MeasureCell.Value;
        if (MeasureCell.MeasureIndex >= 0) then
          Format := Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayFormat
        else  
          Format := nil;
      end;
      if MeasureCell.IsTotal or MeasureCell.IsGrandTotal then
        Style1 := FStyleCache.GetStyleID(Styles[gsDataCellsTotals], MeasureCell.Alignment, Format)
      else
        Style1 := FStyleCache.GetStyleID(Styles[gsDataCells], MeasureCell.Alignment, Format);
      CreateDataCell(Node.Add, varvalue, Style1, Format, 1, 1);
    end;
    DoProgress(Row);    
  end;
end;

procedure TfcxODFExport.ExportRows;
var
  S: String;
begin
  PageNode.Name := 'table:table';
  S := ProcessControlSymbols(OdfPrepareString(GetTitle(rtWorksheet)));
  PageNode.Prop['table:name'] := Copy(StringReplace(S, ' ', '_', [rfReplaceAll]), 1, 31);
  PageNode.Prop['table:style-name'] := 'ta1';

  if Assigned(Slice) then
    ExportSliceRows
  else
    ExportCubeRows;
end;

procedure TfcxODFExport.ExportBody(BodyNode: TfcxXMLItem);
begin
  BodyNode.Name := 'office:body';

  with BodyNode.Add do
  begin
    if ExportType = 'text' then
    begin
      Name := 'office:text';
      Prop['text:use-soft-page-breaks'] := 'true';
    end
    else
      Name := 'office:spreadsheet';

    PageNode := Add;
    ExportRows;
  end;
end;

procedure TfcxODFExport.Finish;
begin
  if OpenAfterExport and not Assigned(Stream) then
    OpenDocument(FileName);
end;

destructor TfcxODFExport.Destroy;
begin
  FRowStyleNames.Free;
  FStyleCache.Free;
  inherited;
end;

procedure TfcxODFExport.ExportPage(Stream: TStream);
var
  XML: TfcxXMLDocument;
  s: WideString;
  FList: TStringList;
  i: Integer;
  Style: TfcxCustomThemeStyle;
  ZipFile: TfcxZipArchive;
begin
  FTempFolder := GetTempFile;
  DeleteFile(FTempFolder);
  FTempFolder := FTempFolder + DirectorySeparator;
  MkDir(FTempFolder);
  CreateStyles;

  XML := TfcxXMLDocument.Create;
  try
    XML.AutoIndent := False;
    XML.Root.Name := 'office:document-content';
    OdfMakeHeader(XML.Root);
    with XML.Root.Add do
      Name := 'office:scripts';
    // font styles
    FList := TStringList.Create;
    try
      FList.Sorted := True;
      for i := Styles.FirstStyleIndex to Styles.LastStyleIndex - 1 do
      begin
        Style := Styles[i];
        if (FList.IndexOf(Style.Font.Name) = -1) then
          FList.Add(Style.Font.Name);
      end;
      with XML.Root.Add do
      begin
        Name := 'office:font-face-decls';
        for i := 0 to FList.Count - 1 do
        begin
          with Add do
          begin
            Name := 'style:font-face';
            Prop['style:name'] := FList[i];
            Prop['svg:font-family'] := '&quot;' + FList[i] + '&quot;';
            Prop['style:font-pitch'] := 'variable';
          end;
        end;
      end;
    finally
      FList.Free;
    end;

    FStyleCache.StyleNode := XML.Root.Add;
    FStyleCache.FExportType := FExportType;

    with FStyleCache.StyleNode do
    begin
      Name := 'office:automatic-styles';

      with Add do
      begin
        Name := 'style:style';
        Prop['style:name'] := 'co1';
        Prop['style:family'] := 'table-column';
        with Add do
        begin
          Name := 'style:table-column-properties';
          Prop['fo:break-before'] := 'auto';
          Prop['style:column-width'] := '2.6cm';
        end;
      end;

      FRowStyleNames.Clear;

      // table style
      with Add do
      begin
        Name := 'style:style';
        Prop['style:name'] := 'ta1';
        Prop['style:family'] := 'table';
        Prop['style:master-page-name'] := 'PageDef';
        with Add do
        begin
          Name := 'style:table-properties';
          Prop['table:display'] := 'true';
          Prop['style:writing-mode'] := 'lr-tb';  /// RTL - LTR?
        end;
      end;
    end;

    ExportBody(XML.Root.Add);
    XML.SaveToFile(FTempFolder + 'content.xml');
  finally
    XML.Free;
  end;
  MkDir(FTempFolder + 'META-INF');
  s := FExportType;
  OdfCreateManifest(FTempFolder + 'META-INF' + DirectorySeparator + 'manifest.xml', s);
  OdfCreateMime(FTempFolder + 'mimetype', s);
  OdfCreateMeta(FTempFolder + 'meta.xml', Creator);
  ZipFile := TfcxZipArchive.Create;
  try
{$IFDEF Delphi_12UP}
    ZipFile.RootFolder := AnsiString(FTempFolder);
    ZipFile.AddDir(AnsiString(FTempFolder));
{$ELSE}
    ZipFile.RootFolder := FTempFolder;
    ZipFile.AddDir(FTempFolder);
{$ENDIF}
    ZipFile.SaveToStream(Stream);
  finally
    ZipFile.Free;
  end;
  DeleteFolder(FTempFolder);
end;

function TfcxODFExport.GetRow(RowIndex: Integer): TfcxXMLItem;
var
  Row, LastIndex: Integer;
begin
  LastIndex := High(FRows);
  if LastIndex < RowIndex then
  begin
    SetLength(FRows, RowIndex + 1);
    for Row := LastIndex + 1 to RowIndex do
    begin
      FRows[Row] := FPageNode.Add;
      FRows[Row].Name := 'table:table-row';
      FRows[Row].Prop['table:style-name'] := CreateRowStyle(Row);
    end;
  end;
  Result := FRows[RowIndex];
end;

procedure TfcxODFExport.SetPageNode(const Value: TfcxXMLItem);
begin
  FPageNode := Value;
  Finalize(FRows);
end;

procedure TfcxODFExport.CreateAdjacentCell(Node: TfcxXMLItem; Columns: Integer);
begin
  Node.Name := 'table:covered-table-cell';

  if Columns > 1 then
    Node.Prop['table:number-columns-repeated'] := IntToStr(Columns);
end;

function TfcxODFExport.XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  Row: integer;
  StyleIndex: Integer;
  RowNode: TfcxXMLItem;
begin
  StyleIndex := FStyleCache.GetStyleID(Styles[gsHeaderCells], ARec.Alignment, nil);
  if RepeatValues then
  begin
    for Row := 1 to ARec.TreeRect.SizeLevel do
    begin
      RowNode := GetRow(ARec.TreeRect.Level + Row);
      CreateDataCell(RowNode.Add, ARec.Text, StyleIndex, nil, 1, ARec.TreeRect.SizeCell);
      if ARec.TreeRect.SizeCell > 1 then
        CreateAdjacentCell(RowNode.Add, ARec.TreeRect.SizeCell - 1);
    end;
  end
  else
  begin
    RowNode := GetRow(ARec.TreeRect.Level + 1);
    CreateDataCell(RowNode.Add, ARec.Text, StyleIndex, nil, ARec.TreeRect.SizeLevel, ARec.TreeRect.SizeCell);
    if ARec.TreeRect.SizeCell > 1 then
      CreateAdjacentCell(RowNode.Add, ARec.TreeRect.SizeCell - 1);
    for Row := 2 to ARec.TreeRect.SizeLevel do
      CreateAdjacentCell(GetRow(ARec.TreeRect.Level + Row).Add, ARec.TreeRect.SizeCell);
  end;
  Result := False;
end;

function TfcxODFExport.YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;

  function NewNodeAt(RowIndex, ColIndex: Integer): TfcxXMLItem;
  begin
    Result := GetRow(RowIndex).Add;
    Result.Parent.InsertItem(ColIndex, Result);
  end;

var
  Row, RowAdd: integer;
  StyleIndex: Integer;
begin
  RowAdd := Slice.XAxisContainer.VisibleLevelCount + 1;
  StyleIndex := FStyleCache.GetStyleID(Styles[gsHeaderCells], ARec.Alignment, nil);
  if RepeatValues then
  begin
    for Row := 0 to ARec.TreeRect.SizeCell - 1 do
    begin
      CreateDataCell(NewNodeAt(RowAdd + ARec.TreeRect.Cell + Row, 0), ARec.Text, StyleIndex, nil, 1, ARec.TreeRect.SizeLevel);
      if ARec.TreeRect.SizeLevel > 1 then
        CreateAdjacentCell(NewNodeAt(RowAdd + ARec.TreeRect.Cell + Row, 1), ARec.TreeRect.SizeLevel - 1);
    end;
  end
  else
  begin
    CreateDataCell(NewNodeAt(RowAdd + ARec.TreeRect.Cell, 0), ARec.Text, StyleIndex, nil, ARec.TreeRect.SizeCell, ARec.TreeRect.SizeLevel);
    if ARec.TreeRect.SizeLevel > 1 then
      CreateAdjacentCell(NewNodeAt(RowAdd + ARec.TreeRect.Cell, 1), ARec.TreeRect.SizeLevel - 1);
    for Row := 1 to ARec.TreeRect.SizeCell - 1 do
      CreateAdjacentCell(NewNodeAt(ARec.TreeRect.Cell + Row + RowAdd, 0), ARec.TreeRect.SizeLevel);
  end;
  Result := False;
end;

procedure TfcxODFExport.Run;
var
  Exp: TStream;
begin  
  Exp := Stream;
  try
    if not Assigned(Stream) then
      Exp := TFileStream.Create(FileName, fmCreate);
    ExportPage(Exp);
  finally
    if not Assigned(Stream) then
      Exp.Free;
  end;
end;

{ TfcxODSExport }

constructor TfcxODSExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ExportType := 'spreadsheet';
  FilterDesc := fcxResources.Get('ODSExportFilter');
  DefaultExt := fcxGet(8960);
  ExportTitle := fcxResources.Get('ODSExport');
end;

class function TfcxODSExport.GetDescription: String;
begin
  Result := fcxResources.Get('ODSExport');
end;

{ TfcxODTExport }

constructor TfcxODTExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ExportType := 'text';
  FilterDesc := fcxResources.Get('ODTExportFilter');
  DefaultExt := fcxGet(8961);
  ExportTitle := fcxResources.Get('ODTExport');
end;

class function TfcxODTExport.GetDescription: String;
begin
  Result := fcxResources.Get('ODTExport');
end;

{ TfcxODFStyleCache }

procedure TfcxODFStyleCache.AddNumberStyle(Item: TfcxXMLItem; const StyleName, Fmt: string);
var
  DecSep: Integer;
  DecPlaces: string;
begin
  DecSep := Pos('.', Fmt);

  if DecSep = 0 then
    DecPlaces := '0'
  else
    DecPlaces := FilterStr(Copy(Fmt, DecSep, Length(Fmt)), ['0'..'9']);

  if DecPlaces = '' then
    DecPlaces := '0';

  with Item do
  begin
    Name := 'number:number-style';

    Prop['style:name'] := StyleName;

    with Add do
    begin
      Name := 'number:number';

      Prop['number:decimal-places'] := DecPlaces;
      Prop['number:min-integer-digits'] := '1';
    end;
  end;
end;

function TfcxODFStyleCache.CreateStyle(Style: TfcxCustomThemeStyle; Alignment: TAlignment; Format: TfcxFormat): Integer;
begin
  inc(FLastStyleIndex);
  Result := FLastStyleIndex;
  if Assigned(Format) and (Format.TypeFormat.Kind = fkNumeric) then
    AddNumberStyle(StyleNode.Add, 'N' + IntToStr(Result), Format.TypeFormat.FormatStr);

  with StyleNode.Add do
  begin
    Name := 'style:style';
    Prop['style:name'] := 'ce' + IntToStr(Result);
    Prop['style:family'] := 'table-cell';
    Prop['style:parent-style-name'] := 'Default';
    if Assigned(Format) and (Format.TypeFormat.Kind = fkNumeric) then
      Prop['style:data-style-name'] := 'N' + IntToStr(Result);

    if FExportType <> 'text' then
    begin
      with Add do
      begin
        Name := 'style:text-properties';
        Prop['style:font-name'] := Style.Font.Name;
        Prop['fo:font-size'] := IntToStr(Round(Style.Font.Size)) + 'pt';

        if {$IFDEF FMX}TFontStyle.{$ENDIF}fsUnderline in Style.Font.Style then
        begin
          Prop['style:text-underline-style'] := 'solid';
          Prop['style:text-underline-width'] := 'auto';
          Prop['style:text-underline-color'] := 'font-color';
        end;

        if {$IFDEF FMX}TFontStyle.{$ENDIF}fsItalic in Style.Font.Style then
          Prop['fo:font-style'] := 'italic';

        if {$IFDEF FMX}TFontStyle.{$ENDIF}fsBold in Style.Font.Style then
          Prop['fo:font-weight'] := 'bold';

        Prop['fo:color'] := HTMLRGBColor(Style.TextColor);
      end;

      with Add do
      begin
        Name := 'style:paragraph-properties';
        Prop['fo:margin-left'] := '0cm';
        Prop['fo:margin-right'] := Prop['fo:margin-left'];
        case Alignment of
          taLeftJustify: Prop['fo:text-align'] := 'start';
          taCenter: Prop['fo:text-align'] := 'center';
          taRightJustify: Prop['fo:text-align'] := 'end';
        end;
      end;
    end;

    with Add do
    begin
      Name := 'style:table-cell-properties';

      if Style.FillColor = {$IFDEF FMX}TAlphaColors.Null{$ELSE}clNone{$ENDIF} then
        Prop['fo:background-color'] := 'transparent'
      else
        Prop['fo:background-color'] := HTMLRGBColor(Style.FillColor);

      Prop['style:repeat-content'] := 'false';
      Prop['style:vertical-align'] := 'middle';
      Prop['fo:border'] := 'thin solid ' + HTMLRGBColor({$IFDEF FMX}TAlphaColors.Black{$ELSE}clBlack{$ENDIF});
      Prop['fo:wrap-option'] := 'wrap';
    end;
  end;
  if FExportType = 'text' then
  begin
    with StyleNode.Add do
    begin
      Name := 'style:style';
      Prop['style:name'] := 'p' + IntToStr(Result);
      Prop['style:family'] := 'paragraph';
      Prop['style:parent-style-name'] := 'Default';
      with Add do
      begin
        Name := 'style:text-properties';
        Prop['style:font-name'] := Style.Font.Name;
        Prop['fo:font-size'] := IntToStr(Round(Style.Font.Size)) + 'pt';
        if {$IFDEF FMX}TFontStyle.{$ENDIF}fsUnderline in Style.Font.Style then
        begin
          Prop['style:text-underline-style'] := 'solid';
          Prop['style:text-underline-width'] := 'auto';
          Prop['style:text-underline-color'] := 'font-color';
        end;
        if {$IFDEF FMX}TFontStyle.{$ENDIF}fsItalic in Style.Font.Style then
          Prop['fo:font-style'] := 'italic';
        if {$IFDEF FMX}TFontStyle.{$ENDIF}fsBold in Style.Font.Style then
          Prop['fo:font-weight'] := 'bold';
        Prop['fo:color'] := HTMLRGBColor(Style.TextColor);
      end;
      with Add do
      begin
        Name := 'style:paragraph-properties';
        Prop['fo:text-align'] := 'start';
        case Alignment of
          taLeftJustify: Prop['fo:text-align'] := 'start';
          taCenter: Prop['fo:text-align'] := 'center';
          taRightJustify: Prop['fo:text-align'] := 'end';
        end;
      end;
    end;
  end;
end;

function TfcxODFStyleCache.GetStyleID(AStyle: TfcxCustomThemeStyle;
  AAlignment: TAlignment; AFormat: TfcxFormat): Integer;
var
  I: Integer;
begin
  if Length(FStyles) <= AStyle.Index then
    SetLength(FStyles, AStyle.Index + 1)
  else
  begin
    for I := 0 to High(FStyles[AStyle.Index]) do
      if (FStyles[AStyle.Index][I].Alignment = AAlignment) and
         ((FStyles[AStyle.Index][I].Format = AFormat) or
          (Assigned(FStyles[AStyle.Index][I].Format) and
          (FStyles[AStyle.Index][I].Format.Equal(AFormat)))) then
      begin
        Result := FStyles[AStyle.Index][I].ID;
        Exit;
      end;
  end;
  I := Length(FStyles[AStyle.Index]);
  SetLength(FStyles[AStyle.Index], I + 1);
  Result := CreateStyle(AStyle, AAlignment, AFormat);
  FStyles[AStyle.Index][I].ID := Result;
  FStyles[AStyle.Index][I].Alignment := AAlignment;
  FStyles[AStyle.Index][I].Format := AFormat;
end;

procedure TfcxODFStyleCache.SetStyleNode(const Value: TfcxXMLItem);
begin
  FStyleNode := Value;
  Finalize(FStyles);
  FLastStyleIndex := 0;
end;

{ TfcxODFExportDialog }

constructor TfcxODFExportDialog.Create(AOwner: TComponent);
var
  Page: TComponent;
begin
  inherited;

  Page := AddPage(fcxResources.Get('sExport'));
  OpenCB := AddCheckBox(Page, fcxGet(8706));
  cbRepeatValues := AddCheckBox(Page, fcxResources.Get('sRepeatAxesValues'));
end;

end.
