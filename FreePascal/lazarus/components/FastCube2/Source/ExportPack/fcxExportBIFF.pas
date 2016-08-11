
{******************************************}
{                                          }
{             FastCube v2.0                }
{                XLS export                }
{                                          }
{         Copyright (c) 1998-2014          }
{           by Anton Khayrudinov,          }
{        Oleg Pryalkov, Paul Ishenin       }
{             Fast Reports Inc.            }
{                                          }
{******************************************}

{ This module contains a component that allows to
  export prepaired reports to Microsoft Excel 2003 files.
  These files consist of the following parts:

    - CBFF    - Base format. See frxCBFF.pas
    - BIFF    - Actual contents of sheets. See frxBIFF.pas
    - Escher  - Drawings and images. See frxEscher.pas
    - OLEPS   - Additional information. See frxOLEPS.pas
    - RC4     - Document encrypting. See frxCrypto.pas

  There's a bug or a feature in MS Excel that you should
  know if you want to make good xls files for printing. When
  exporting a report, there can be an object (picture, memo, etc.)
  close to a border of a report page, i.e. the distance between
  this object and a border of the page is zero or very small.
  When creating the appropriate xls file, this object will become
  a text cell or a picture that is aligned to a side of the print page.
  We assume that this object fits to the print page.
  MS Excel assumes that neighbouring cells of this object must be
  printed too, but they don't fit to the print page. There's no
  complete solution of this problem. Partial solutions are:

    - Do not place any objects close to a side of the page.
    - If you have to place object close to a side of the page,
      decrease the page margins. }

//VCL uses section
{$IFNDEF FMX}
unit fcxExportBIFF;

{$include fcx.inc}

interface

uses
  {$ifdef fpc}
  LCLType,
  {$else}
  Windows,
  {$endif}
  SysUtils,
  Classes,
  {$IFDEF DELPHI_6UP}
  Variants,
  {$ENDIF}
  Graphics,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  Controls,

  fcxRes,
  fcxUtils,
  fcxFormats,
  fcxCustomExport,
  fcxStyles,
  fcxCustomGrid,
  fcxCube,
  fcxSlice,
  fcxSliceGrid,

  fcxExportDialog,
  fcxBIFF,
  fcxCBFF,
  fcxOLEPS,
  fcxEscher;
{$ELSE}
{$include fcx.inc}

interface

uses
  // RTL
  System.SysUtils, System.Classes, System.UITypes, System.Variants,
  System.IOUtils, System.Math,
  // FMX
  FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.Edit, FMX.Memo, FMX.Dialogs,
  {$IFDEF Delphi_19UP}
  FMX.Graphics,
  {$ENDIF}
  // FC FMX
  FMX.fcxRes, FMX.fcxFormats, FMX.fcxCustomExport, FMX.fcxStyles,
  FMX.fcxCube, FMX.fcxSlice, FMX.fcxUtils, FMX.fcxExportDialog,
  FMX.fcxGraphicUtils, FMX.fcxBIFF, FMX.fcxCBFF, FMX.fcxOLEPS, FMX.fcxEscher;
{$ENDIF}

type

  TfcxBIFFExportDialog = class(TfcxExportDialog)
  private
    edTitle: TEdit;
    edAuthor: TEdit;
    edKeywords: TEdit;
    edRevision: TEdit;
    edAppName: TEdit;
    edSubject: TEdit;
    edCategory: TEdit;
    edCompany: TEdit;
    edManager: TEdit;
    edComment: TMemo;
    edPass1: TEdit;
    edPass2: TEdit;
    OpenCB: TCheckBox;
    cbAutoCreateFile: TCheckBox;
    cbGridLines: TCheckBox;
    cbRepeatValues: TCheckBox;
    cbUseDefaultPalette: TCheckBox;
    cbMergeCells: TCheckBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  EInvalidFCFormat = class(Exception);  // invalid FC number format

  TfcxBiffStyleRec = record
    ID: Integer;
    Alignment: TAlignment;
    Format: TfcxFormat;
    IsPercent: Boolean;
  end;

  TfcxBiffStyleArray = array of TfcxBiffStyleRec;
  TfcxBiffStyleCache = class(TObject)
  private
    FStyles: array of TfcxBiffStyleArray;
    FZCW: LongInt; // Width in points of '0' char of the 0-th font
    FTSW: LongInt; // Width of three blank characters
    FWorkbook: TBiffWorkbook;
    procedure SetWorkbook(const Value: TBiffWorkbook);
  public
    function CreateStyle(S: TfcxCustomThemeStyle; Alignment: TAlignment; AFormat: TfcxFormat; IsPercent: Boolean): LongInt;
    function GetStyleID(AStyle: TfcxCustomThemeStyle; AAlignment: TAlignment; AFormat: TfcxFormat; IsPercent: Boolean): Integer;
    property WorkBook: TBiffWorkbook read FWorkbook write SetWorkbook;
  end;

  { Export filter }

  TfcxBIFFExport = class(TfcxCustomExportFilter)
  private
    FOpenAfterExport: Boolean;

    { The following fields control appearance
      of the generated xls file }

    FGridLines:       Boolean;
    FMergeCells:      Boolean;

    { The following fields are used internally
      during generating the xls file }

    FWB:              TBiffWorkbook;

    FCurrentSheet: TBiffSheet;
    FStartCol: Integer;
    FStartRow: Integer;
    FStyleCache: TfcxBiffStyleCache;

    { The following fields specify additional
      information that is saved in the generated
      xls file }

    FAuthor:          AnsiString;
    FComment:         AnsiString;
    FKeywords:        AnsiString;
    FRevision:        AnsiString;
    FAppName:         AnsiString;
    FSubject:         AnsiString;
    FCategory:        AnsiString;
    FCompany:         AnsiString;
    FTitle:           AnsiString;
    FAccess:          TOlepsAccess;
    FManager:         AnsiString;

    { Document encryption }

    FPassword:        WideString;
    FRepeatValues: Boolean;
    FUseDefPalette: Boolean;
    procedure SetPassword(const s: WideString);

    { Workbook CBFF stream contents }

    procedure SaveWorkbook(s: TStream);

    { <05>SummaryInformation OLEPS stream contents }

    procedure SaveSI(s: TStream);

    { <05>DocumentSummaryInformation OLEPS stream contents }

    procedure SaveDSI(s: TStream);
    procedure CreateCellRange(ACol, ARow, AColCount, ARowCount, SST,
      XF: Integer);
  protected
    function CreateCubeSheet(AOwner: TBiffWorkBook): TBiffSheet;
    function CreateSliceSheet(AOwner: TBiffWorkBook): TBiffSheet;
    function XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
    function YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetDescription: string; override;
    function ShowModal: TModalResult; override;
    function Start: Boolean; override;
    procedure Run; override;
    procedure Finish; override;
  published
    property RepeatValues: Boolean read FRepeatValues write FRepeatValues default False;
    property MergeCells: Boolean read FMergeCells write FMergeCells default True;
    property UseDefPalette: Boolean read FUseDefPalette write FUseDefPalette default False;
    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport default False;
    property OverwritePrompt;

    { Normally, MS Excel draws lines around each cell. This feature
      can be disabled by this property }

    property GridLines: Boolean read FGridLines write FGridLines default True;

    { The list of properties below specify some attributes of
      the generated xls file. All of these properties may be empty.
      If a property is empty, it will not be written to the
      output file.

      Note, that all of these properties are not Unicode. That's why
      MS Excel 2003 does not support Unicode strings in document
      properties. }

    property Author: AnsiString read FAuthor write FAuthor;
    property Comment: AnsiString read FComment write FComment;
    property Keywords: AnsiString read FKeywords write FKeywords;
    property Revision: AnsiString read FRevision write FRevision;
    property AppName: AnsiString read FAppName write FAppName;
    property Subject: AnsiString read FSubject write FSubject;
    property Category: AnsiString read FCategory write FCategory;
    property Company: AnsiString read FCompany write FCompany;
    property Title: AnsiString read FTitle write FTitle;
    property Manager: AnsiString read FManager write FManager;

    { If a non empty password is set, the generated document
      will be encrypted with the RC4 cipher. The maximum
      password length is 255 unicode characters. }

    property Password: WideString read FPassword write SetPassword;
  end;

implementation

{ TfcxBIFFExportDialog }

constructor TfcxBIFFExportDialog.Create(AOwner: TComponent);
var
  Page: TComponent;
begin
  inherited;

  Page := AddPage(fcxGet(9173));
  cbGridLines := AddCheckBox(Page, fcxGet(9155));
  cbGridLines.Checked := True;
  cbRepeatValues := AddCheckBox(Page, fcxResources.Get('sRepeatAxesValues'));
  cbUseDefaultPalette := AddCheckBox(Page, fcxResources.Get('sUseDefaultPalette'));
  cbAutoCreateFile := AddCheckBox(Page, fcxGet(9152));
  cbMergeCells := AddCheckBox(Page, fcxGet(8003));
  OpenCB := AddCheckBox(Page, fcxGet(8706));

  Page := AddPage(fcxGet(9174));
  edTitle := AddEdit(Page);
  AddLabelFor(edTitle, fcxGet(9171));
  edAuthor := AddEdit(Page);
  AddLabelFor(edAuthor, fcxGet(9162));
  edKeywords := AddEdit(Page);
  AddLabelFor(edKeywords, fcxGet(9164));
  edRevision := AddEdit(Page);
  AddLabelFor(edRevision, fcxGet(9165));
  edAppName := AddEdit(Page);
  AddLabelFor(edAppName, fcxGet(9167));
  edSubject := AddEdit(Page);
  AddLabelFor(edSubject, fcxGet(9168));
  edCategory := AddEdit(Page);
  AddLabelFor(edCategory, fcxGet(9169));
  edCompany := AddEdit(Page);
  AddLabelFor(edCompany, fcxGet(9170));
  edManager := AddEdit(Page);
  AddLabelFor(edManager, fcxGet(9172));
  edComment := AddMemo(Page);
  edComment.Height := edManager.Height;
  AddLabelFor(edComment, fcxGet(9163));
  Page := AddPage(fcxGet(9175));
  edPass1 := AddEdit(Page);
  AddLabelFor(edPass1, fcxGet(9176));
  edPass2 := AddEdit(Page);
  AddLabelFor(edPass2, fcxGet(9178));
  {$ifdef FMX}
  with AddLabel(Page, fcxGet(9177)) do
    Height := Height * 4;
  {$else}
  AddLabel(Page, fcxGet(9177));
  {$endif}
end;

{ TfcxBIFFExport }

constructor TfcxBIFFExport.Create(AOwner: TComponent);
begin
  inherited;

  FRepeatValues := False;
  FUseDefPalette := False;
  FGridLines := True;
  FMergeCells := True;
  FAccess := OlepsAfAll;
  FStyleCache := TfcxBiffStyleCache.Create;

  FilterDesc := fcxGet(8009);
  DefaultExt := fcxGet(8010);
end;

procedure TfcxBIFFExport.SetPassword(const s: WideString);
begin
  if Length(s) > 255 then
    raise Exception.CreateFmt(fcxGet(9180), [Length(s), 255]);

  FPassword := s;
end;

class function TfcxBIFFExport.GetDescription: string;
begin
  Result := fcxGet(9151);
end;

function TfcxBIFFExport.ShowModal: TModalResult;
var
  Columns: TfcxCubeDataColumns;

{$IFNDEF FMX}
  procedure st(c: TControl; const s: AnsiString);
  begin
    c.SetTextBuf(PChar(string(s)));
  end;

  procedure rd(var s: AnsiString; e: TCustomEdit);
  begin
    s := AnsiString(e.Text);
  end;
{$ELSE}
  procedure st(c: TEdit; const s: AnsiString); overload;
  begin
    c.Text := s;
  end;

  procedure rd(var s: AnsiString; e: TEdit); overload;
  begin
    s := e.Text;
  end;

  procedure st(c: TMemo; const s: AnsiString); overload;
  begin
    c.Text := s;
  end;

  procedure rd(var s: AnsiString; e: TMemo); overload;
  begin
    s := e.Text;
  end;
{$ENDIF}

begin
  Result := mrOk; // Default

  if Assigned(Stream) then
    Exit;
// Check Col and Row count
  if Assigned(Slice) then
  begin
    if (Slice.ColCount + Slice.YAxisContainer.VisibleLevelCount) > BiffMaxCol then
    begin
      MessageDlg(Format(fcxResources.Get('sBiffAllowColCount'), [BiffMaxCol])
        , {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtError, [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk], 0);
      Exit;
    end
    else if (Slice.RowCount + Slice.XAxisContainer.VisibleLevelCount + 1) > BiffMaxRow then
    begin
      MessageDlg(Format(fcxResources.Get('sBiffAllowRowCount'), [BiffMaxRow])
        , {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtError, [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk], 0);
      Exit;
    end
  end
  else
  if Assigned(Cube) then
  begin
    Columns := DoGetCubeCols;
    if (Columns.VisibleCount + 1) > BiffMaxCol then
    begin
      MessageDlg(Format(fcxResources.Get('sBiffAllowColCount'), [BiffMaxCol])
        , {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtError, [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk], 0);
      Exit;
    end
    else if (DoGetCubeRowCount + 2) > BiffMaxRow then
    begin
      MessageDlg(Format(fcxResources.Get('sBiffAllowRowCount'), [BiffMaxRow])
        , {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtError, [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk], 0);
      Exit;
    end
  end
  else
    Exit;

  with TfcxBIFFExportDialog.Create(nil) do
  begin
    PrepareSaveDialog(SaveDialog);

    cbRepeatValues.Checked := RepeatValues;
    cbMergeCells.Checked := MergeCells;
    cbUseDefaultPalette.Checked := UseDefPalette;
    OpenCB.Checked := FOpenAfterExport;

    cbGridLines.Checked       := FGridLines;

    st(edTitle,     FTitle);
    st(edAuthor,    FAuthor);
    st(edKeywords,  FKeywords);
    st(edRevision,  FRevision);
    st(edAppName,   FAppName);
    st(edSubject,   FSubject);
    st(edCategory,  FCategory);
    st(edCompany,   FCompany);
    st(edManager,   FManager);
    st(edComment,   FComment);
    st(edPass1,     '');
    st(edPass2,     '');

    Result := ShowModal;

    if Result = mrOk then
    begin
      if edPass1.Text <> edPass2.Text then
        MessageDlg(fcxGet(9179), {$IFDEF FMX}TMsgDlgType.{$ENDIF}mtError, [{$IFDEF FMX}TMsgDlgBtn.{$ENDIF}mbOk], 0)
      else
      begin
        FOpenAfterExport  := OpenCB.Checked;
        RepeatValues := cbRepeatValues.Checked;
        MergeCells := cbMergeCells.Checked;
        UseDefPalette := cbUseDefaultPalette.Checked;
        FGridLines        := cbGridLines.Checked;

        rd(FTitle,    edTitle);
        rd(FAuthor,   edAuthor);
        rd(FKeywords, edKeywords);
        rd(FRevision, edRevision);
        rd(FAppName,  edAppName);
        rd(FSubject,  edSubject);
        rd(FCategory, edCategory);
        rd(FCompany,  edCompany);
        rd(FManager,  edManager);
        rd(FComment,  edComment);

        FPassword := edPass1.Text;

        if cbAutoCreateFile.Checked then
          FileName := GetTempFile + DefaultExt
        else if SaveDialog.Execute then
          FileName := SaveDialog.FileName
        else
          Result := mrCancel;
      end;
    end;

    Free;
  end;
end;

function TfcxBIFFExport.Start: Boolean;
begin
  Result := False; // Default

  if (FileName = '') and not Assigned(Stream) then
    Exit;

  if (ExtractFilePath(FileName) = '') and (DefaultPath <> '') then
    FileName := DefaultPath + '\' + FileName;

  FWB := TBiffWorkbook.Create;
  FStyleCache.WorkBook := FWB;
  if FUseDefPalette then
    FWB.AddDefPalette(True);
  Result := True;
end;

procedure TfcxBIFFExport.Finish;
var
  cd: TCbffDocument;
  fs: TFileStream;
begin
  { The generated xls file is saved
    as a compound document }
  if not FUseDefPalette then
    FWB.AddDefPalette(False);
  cd := TCbffDocument.Create;
  try

    { Save the workbook to the required CBFF stream }

    SaveWorkbook(cd.Root.Add('Workbook').Stream);

    { Save the information about the document
      to the required CBFF streams.

      All unicode strings will be saved as ansi-strings,
      because MS Excel 2003 never uses unicode characters in
      the document information. Although OpenOffice allows
      to save unicode to the information, it does it
      in another way as the documentation [MS-OLEPS]
      specifies and MS Excel cannot read such information. }

    SaveSI(cd.Root.Add(#5'SummaryInformation').Stream);
    SaveDSI(cd.Root.Add(#5'DocumentSummaryInformation').Stream);

    { Serialize the CBFF document to a plain stream }

    if Assigned(Stream) then
      cd.Flush(Stream)
    else
    begin
      fs := TFileStream.Create(FileName, fmCreate);

      try
        cd.Flush(fs);
      finally
        fs.Free;
      end;
    end;

  finally
    cd.Free;
    FWB.Free;
  end;

  if OpenAfterExport and not Assigned(Stream) then
    OpenDocument(FileName);
end;

procedure TfcxBIFFExport.SaveWorkbook(s: TStream);
var
  bs: TBiffStream;
begin
  if FPassword <> '' then
    FWB.SetPassword(FPassword);

  bs := TBiffStream.Create(True);

  try
    FWB.Flush(bs);
    bs.SaveToStream(s);
  finally
    bs.Free;
  end;
end;

procedure TfcxBIFFExport.SaveSI(s: TStream);
var
  SysTime: TSystemTime;
  Time: TFileTime;
begin
  DateTimeToSystemTime(CreationTime, SysTime);
  SystemTimeToFileTime(SysTime, Time);

  with TOlepsStream.Create do
  try
    with Add(OlepsFmtIdSi) do
    begin
      AddAnsi(OlepsSiTitle,      FTitle);
      AddAnsi(OlepsSiSubject,    FSubject);
      AddAnsi(OlepsSiAuthor,     FAuthor);
      AddAnsi(OlepsSiKeywords,   FKeywords);
      AddAnsi(OlepsSiLastAuthor, FAuthor);
      AddAnsi(OlepsSiRevision,   FRevision);
      AddAnsi(OlepsSiAppName,    FAppName);
      AddAnsi(OlepsSiComment,    FComment);

      AddTime(OlepsSiCreation, Time);
      AddTime(OlepsSiLastSave, Time);

      Add(OlepsSiAccess, OlepsPtInt).Write(FAccess, 4);
    end;
  finally
    Flush(s);
    Free;
  end;
end;

procedure TfcxBIFFExport.SaveDSI(s: TStream);
begin
  with TOlepsStream.Create do
  try
    with Add(OlepsFmtIdDsi) do
    begin
      AddAnsi(OlepsDsiCategory, FCategory);
      AddAnsi(OlepsDsiCompany,  FCompany);
      AddAnsi(OlepsDsiManager,  FManager);
    end;
  finally
    Flush(s);
    Free;
  end;
end;

function TfcxBIFFExport.CreateCubeSheet(AOwner: TBiffWorkBook): TBiffSheet;
var
  Cell: TBiffCell;
  Col, Row, XF, RowID: Integer;
  Value: Variant;
  Columns: TfcxCubeDataColumns;
begin
  Result := TBiffSheet.Create(AOwner);
  Result.Name := GetTitle(rtWorksheet);

  XF := FStyleCache.GetStyleID(Styles[gsHeaderCells], taCenter, nil, False);
  Columns := DoGetCubeCols;
  for Col := 0 to Columns.VisibleCount - 1 do
  begin
    Cell := TBiffTextCell.Create(AOwner.AddString(Columns[Col].Field.CubeFieldDisplayLabel));
    Cell.Col := Col;
    Cell.Row := 0;
    Cell.XF := XF;
    Result.AddCell(Cell);
  end;

  for Row := 0 to DoGetCubeRowCount - 1 do
  begin
    RowID := DoGetCubeRowIndex(Row);
    for Col := 0 to Columns.VisibleCount - 1 do
    begin
      Value := Cube.SourceHolder.UniqueValueAsVariant[RowID, Columns[Col].Field];
      if VarIsNumeric(Value) then
      begin
        Cell := TBiffNumberCell.Create(Value);
        Cell.XF := FStyleCache.GetStyleID(Styles[gsDataCells], taRightJustify, Columns[Col].Field.DisplayFormat, False);
      end
      else
      begin
        Cell := TBiffTextCell.Create(AOwner.AddString(Cube.SourceHolder.UniqueValueCaption[RowID, Columns[Col].Field]));
        Cell.XF := FStyleCache.GetStyleID(Styles[gsDataCells], taLeftJustify, Columns[Col].Field.DisplayFormat, False);
      end;
      Cell.Row := Row + 1;
      Cell.Col := Col;
      Result.AddCell(Cell);
    end;
    DoProgress(Row);
  end;
end;

function TfcxBIFFExport.CreateSliceSheet(AOwner: TBiffWorkBook): TBiffSheet;
var
  Col, Row, MaxCol, MaxRow: Integer;
  Cell: TBiffCell;
  Style1, Style2: Integer;
  MeasureCell: TfcxMeasureCell;
  Format: TfcxFormat;
  IsPercent: Boolean;
begin
  Result := TBiffSheet.Create(AOwner);
  Result.Name := GetTitle(rtWorksheet);
  if (Slice.XAxisContainer.VisibleLevelCount = 0) and (Slice.YAxisContainer.VisibleLevelCount = 0) then
    Exit;

  // add header
  Style1 := FStyleCache.GetStyleID(Styles[gsHeaderArea], taCenter, nil, False);
  Style2 := FStyleCache.GetStyleID(Styles[gsActiveDimension], taLeftJustify, nil, False);

  // row title
  if Slice.YAxisContainer.RealLevelCount <> 0 then
  begin
    MaxCol := Slice.YAxisContainer.VisibleLevelCount - 1;
    MaxRow := Slice.XAxisContainer.VisibleLevelCount - 1;
    for Col := 0 to MaxCol do
      for Row := 0 to MaxRow do
      begin
        Cell := TBiffCell.Create;
        Cell.Col := Col;
        Cell.Row := Row;
        Cell.XF := Style1;
        Result.AddCell(Cell);
      end;
    if (MaxCol >= 0) and (MaxRow >= 0) and MergeCells then
      Result.MergeCells(Rect(0, 0, MaxCol, MaxRow));
    MaxCol := 0;
    for Col := 0 to Slice.YAxisContainer.RealLevelCount - 1 do
    begin
      if (Slice.YAxisContainer.LevelInfo[Col].IsVisible) then
      begin
        if Slice.YAxisContainer.LevelInfo[Col].IsMeasure then
        begin
          Cell := TBiffTextCell.Create(AOwner.AddString(Slice.MeasuresContainer.Caption));
          Cell.XF := FStyleCache.GetStyleID(Styles[gsMeasure], taLeftJustify, nil, False)
        end
        else
        begin
          Cell := TBiffTextCell.Create(AOwner.AddString(Slice.YAxisContainer.LevelInfo[Col].RegionField.Caption));
          Cell.XF := Style2;
        end;
        Cell.Row := MaxRow + 1;
        Cell.Col := MaxCol;
        Result.AddCell(Cell);
        inc(MaxCol);
      end;
    end;
  end;

  FStartCol := MaxCol;

  // column title
  for Col := 0 to Slice.XAxisContainer.RealLevelCount - 1 do
    if (Slice.XAxisContainer.LevelInfo[Col].IsVisible) then
    begin
      if Slice.XAxisContainer.LevelInfo[Col].IsMeasure then
      begin
        Cell := TBiffTextCell.Create(AOwner.AddString(Slice.MeasuresContainer.Caption));
        Cell.XF := FStyleCache.GetStyleID(Styles[gsMeasure], taLeftJustify, nil, False)
      end
      else
      begin
        Cell := TBiffTextCell.Create(AOwner.AddString(Slice.XAxisContainer.LevelInfo[Col].RegionField.Caption));
        Cell.XF := Style2;
      end;
      Cell.Row := 0;
      Cell.Col := MaxCol;
      Result.AddCell(Cell);
      inc(MaxCol);
    end;

  // column header
  FCurrentSheet := Result;
  FStartRow := 1;
  Slice.XAxisContainer.TraverseAxis(0, Slice.XAxisContainer.VisibleLevelCount - 1, 0, XAxisDrawProc);

  // row header
  if Slice.YAxisContainer.VisibleLevelCount <> 0 then
  begin
    FStartCol := 0;
    FStartRow := Slice.XAxisContainer.VisibleLevelCount + 1;
    Slice.YAxisContainer.TraverseAxis(0, Slice.YAxisContainer.VisibleLevelCount - 1, 0, YAxisDrawProc);
    // data
    FStartCol := Slice.YAxisContainer.VisibleLevelCount;
    for Row := 0 to Slice.RowCount - 1 do
    begin
      if (Slice.XAxisContainer.VisibleLevelCount <> 0) then
        for Col := 0 to Slice.ColCount - 1 do
        begin
          Slice.GetMeasureCell(Col, Row, Measurecell);
          IsPercent := (MeasureCell.MeasureIndex >= 0) and (Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayAs in da_Percents);
          if IsPercent and VarIsNumeric(MeasureCell.Value) then
            Cell := TBiffNumberCell.Create(MeasureCell.Value)
          else
          if (MeasureCell.MeasureIndex >= 0) and (Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayAs in da_Ranks) then
            Cell := TBiffNumberCell.Create(StrToInt(MeasureCell.StrValue))
          else
          if VarIsNumeric(MeasureCell.Value) then
            Cell := TBiffNumberCell.Create(MeasureCell.Value)
          else
            Cell := TBiffTextCell.Create(AOwner.AddString(MeasureCell.StrValue));

          if (MeasureCell.MeasureIndex >= 0) and not ((Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayAs in da_Ranks) or IsPercent) then
            Format := Slice.MeasuresContainer.Measures[MeasureCell.MeasureIndex].DisplayFormat
          else
            Format := nil;
          if MeasureCell.IsTotal or MeasureCell.IsGrandTotal then
            Cell.XF := FStyleCache.GetStyleID(Styles[gsDataCellsTotals], MeasureCell.Alignment, Format, IsPercent)
          else
            Cell.XF := FStyleCache.GetStyleID(Styles[gsDataCells], MeasureCell.Alignment, Format, IsPercent);

          Cell.Col := Col + FStartCol;
          Cell.Row := Row + FStartRow;
          Result.AddCell(Cell);
        end;
      DoProgress(Row);
    end;
  end;
end;

procedure TfcxBIFFExport.CreateCellRange(ACol, ARow, AColCount, ARowCount, SST, XF: Integer);
var
  Col, Row: Integer;
  Cell: TBiffCell;
begin
  Cell := TBiffTextCell.Create(SST);
  Cell.XF := XF;
  Cell.Col := ACol;
  Cell.Row := ARow;
  FCurrentSheet.AddCell(Cell);

  if (AColCount > 1) or (ARowCount > 1) then
  begin
    for Row := 0 to ARowCount - 1 do
      for Col := 0 to AColCount - 1 do
      begin
        if (Row = 0) and (Col = 0) then
          continue;
        Cell := TBiffCell.Create;
        Cell.XF := XF;
        Cell.Col := ACol + Col;
        Cell.Row := ARow + Row;
        FCurrentSheet.AddCell(Cell);
      end;
    if MergeCells then    
      FCurrentSheet.MergeCells(Rect(ACol, ARow, ACol + AColCount - 1, ARow + ARowCount - 1));
  end;
end;

function TfcxBIFFExport.XAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  Row, Col: integer;
  SST: Integer;
  XF: Integer;
begin
  SST := FWB.AddString(ARec.Text);
  XF := FStyleCache.GetStyleID(Styles[gsHeaderCells], ARec.Alignment, nil, False);
  if RepeatValues then
  begin
    for Row := 0 to ARec.TreeRect.SizeLevel - 1 do
      for Col := 0 to ARec.TreeRect.SizeCell - 1 do
        CreateCellRange(FStartCol + ARec.TreeRect.Cell + Col, FStartRow + ARec.TreeRect.Level + Row, 1, 1, SST, XF);
  end
  else
    CreateCellRange(FStartCol + ARec.TreeRect.Cell, FStartRow + ARec.TreeRect.Level, ARec.TreeRect.SizeCell, ARec.TreeRect.SizeLevel, SST, XF);
  Result := False;
end;

function TfcxBIFFExport.YAxisDrawProc(Sender: TfcxAxisContainer; const ARec: TfcxSliceDrawHeader): Boolean;
var
  Row, Col, Col2: integer;
  SST: Integer;
  XF: Integer;
begin
  SST := FWB.AddString(ARec.Text);
  XF := FStyleCache.GetStyleID(Styles[gsHeaderCells], ARec.Alignment, nil, False);
  if Sender.AxisType = at_Tree then
    Col := 0
  else
    Col := FStartCol + ARec.TreeRect.Level;
  if RepeatValues then
  begin
    for Row := 0 to ARec.TreeRect.SizeCell - 1 do
      for Col2 := 0 to ARec.TreeRect.SizeLevel - 1 do
        CreateCellRange(Col + Col2, FStartRow + ARec.TreeRect.Cell + Row, 1, 1, SST, XF);
  end
  else
    CreateCellRange(Col, FStartRow + ARec.TreeRect.Cell, ARec.TreeRect.SizeLevel, ARec.TreeRect.SizeCell, SST, XF);
  Result := False;
end;

destructor TfcxBIFFExport.Destroy;
begin
  FStyleCache.Free;
  inherited;
end;

procedure TfcxBIFFExport.Run;
var
  Sheet: TBiffSheet;
begin
  { Combine all Excel sheets created by a few
    threads into a single workbook }

  if not Assigned(Slice) then
    Sheet := CreateCubeSheet(FWB)
  else
    Sheet := CreateSliceSheet(FWB);
  if not GridLines then
    Sheet.View.Options := Sheet.View.Options and not BiffWoGridLines;
  FWB.AddSheet(Sheet);
end;

{ TfcxBiffStyleCache }

function TfcxBiffStyleCache.CreateStyle(S: TfcxCustomThemeStyle; Alignment: TAlignment; AFormat: TfcxFormat; IsPercent: Boolean): LongInt;
  function CreateFont(f: TFont; FontColor: TColor): LongInt;
  var
    font: TBiffFont;

    procedure AddOption(opt: TBiffFontOptions);
    begin
      font.Data.Options := font.Data.Options or Word(opt);
    end;

    function GetWeight: Word;
    begin
      Result := Word(fwNormal);
      if {$IFDEF FMX}TFontStyle.{$ENDIF}fsBold in f.Style then
        Result := Word(fwBold);
    end;

    function GetUnderline: TBiffFontUnderline;
    begin
      Result := fuNone;
      if {$IFDEF FMX}TFontStyle.{$ENDIF}fsUnderline in f.Style then
        Result := fuSingle;
    end;

    function GetFamily: TBiffFontFamily;
    begin
      Result := ffNone;
      {$IFNDEF FMX}
      if fpFixed = f.Pitch then
        Result := ffModern;
      {$ENDIF}
    end;

  begin
    Result := 0; // Default font
    if f = nil then Exit;

    font := TBiffFont.Create;
    with font.Data do
    begin
      {$IFNDEF FMX}
      Height := -MulDiv(f.Height, 1440, f.PixelsPerInch);
      {$ELSE}
      Height := Ceil(f.Size * 1440 / 96);
      {$ENDIF}

      if {$IFDEF FMX}TFontStyle.{$ENDIF}fsItalic in f.Style then
        AddOption(foItalic);

      if {$IFDEF FMX}TFontStyle.{$ENDIF}fsStrikeOut in f.Style then
        AddOption(foStruckOut);

      Color     := WorkBook.AddColor(LongWord(FontColor));
      Weight    := GetWeight;
      Underline := GetUnderline;
      Family    := GetFamily;
    {$IFNDEF FMX}
      Charset   := f.Charset;
    {$ENDIF}
    end;

    font.Name := f.Name;
    Result := WorkBook.AddFont(font);
  end;

  function DupChar(c: Char; n: Integer): string;
  var
    i: Integer;
  begin
    SetLength(Result, n);

    for i := 1 to n do
      Result[i] := c;
  end;

  //
  // Adds a number format for the current cell and
  // returns an index to it.
  //
  function GetFormat: LongInt;
  var
    Fmt, DecSep, ThSep, DecFmt: string;
    p, DecPlaces: Integer;
  begin
    DecSep := '.';
    ThSep := ',';

    Fmt := AFormat.TypeFormat.FormatStr;

    if Fmt = '' then
    case AFormat.TypeFormat.Kind of
      fkText:     Result := BiffFmtGeneral;
      fkNumeric:  Result := BiffFmtFixedPoint;
      fkDateTime: Result := BiffFmtDateTime;
      fkBoolean:  Result := BiffFmtGeneral;

      else Result := BiffFmtGeneral
    end
    else if Fmt[1] <> '%' then
      Result := FWorkBook.AddFormat(Fmt)
    else
    try
      p := Pos('.', Fmt);

      if p = 0 then
        DecPlaces := 0
      else
        DecPlaces := StrToInt(Copy(Fmt, p + 1, Length(Fmt) - p - 1));

      DecFmt := '0' + DecSep + DupChar('0', DecPlaces);

      case Fmt[Length(Fmt)] of
        'n': Result := FWorkBook.AddFormat('#' + ThSep + '##' + DecFmt);
        'm'://Guillaume
        begin
          Fmt:='#' + ThSep + '##' + DecFmt;
{$IFDEF USE_FORMATSETTINGS}
          case FormatSettings.CurrencyFormat of
            0:Fmt:=FormatSettings.CurrencyString+Fmt; //$1
            1:Fmt:=Fmt+FormatSettings.CurrencyString; //1$
            2:Fmt:=FormatSettings.CurrencyString+' '+Fmt; //$ 1
            3:Fmt:=Fmt+' '+FormatSettings.CurrencyString; //1 $
          end;
{$ELSE}
          case CurrencyFormat of
            0:Fmt:=CurrencyString+Fmt; //$1
            1:Fmt:=Fmt+CurrencyString; //1$
            2:Fmt:=CurrencyString+' '+Fmt; //$ 1
            3:Fmt:=Fmt+' '+CurrencyString; //1 $
          end;
{$ENDIF}

          Result := FWorkBook.AddFormat(Fmt);
        end;
        'f': Result := FWorkBook.AddFormat(DecFmt);
        'd': Result := FWorkBook.AddFormat('#' + DecSep + DupChar('#', DecPlaces));
        'g': Result := BiffFmtGeneral;
        else raise EInvalidFCFormat.Create('');
      end;
    except

      //
      // If the format is not "FR-like" then
      // it's assumed to be "Excel-like" and
      // is added to the list of formats.
      //

      Result := FWorkBook.AddFormat(Fmt)
    end;
  end;

  procedure XFBorder(var b: TBiffLine);

    procedure SWR(ls: TBiffLineStyle; const min, max: Single);
    var
      w: Single;
    begin
      w := 1;
      if ((min < 0) or (w >= min)) and ((max < 0) or (w <= max)) then
        b.Style := ls;
    end;

  begin
    b.Style := lsNone;
    b.Color := FWorkBook.AddColor(1);
    SWR(lsThin, -1, 1.5);
    SWR(lsMedium, 1.5, 2.5);
    SWR(lsThick, 2.5, -1);
  end;

  procedure SetBg(x: TBiffXF; p: TBiffPatternStyle);
  begin
    with x.Data do
      if p = psSolid then
      begin
        Patt        := p;
        PattBgColor := $41;
        PattColor   := WorkBook.AddColor(LongWord({$IFDEF FMX}AlphaColorToColor(s.FillColor){$ELSE}s.FillColor{$ENDIF}));
      end
      else
      begin
        Patt        := p;
        PattBgColor := WorkBook.AddColor(LongWord({$IFDEF FMX}AlphaColorToColor(s.FillColor){$ELSE}s.FillColor{$ENDIF}));
        PattColor   := WorkBook.AddColor($000000);
      end;
  end;

var
  XF: TBiffXF;
begin
  Result := 15; // Default cell XF
  if s = nil then Exit;

  XF := TBiffXF.Create;
  with XF.Data do
  begin
    Parent := 0; // Must be zero for cell XFs

    case Alignment of
      taLeftJustify: HAlign := xfhaLeft;
      taRightJustify: HAlign := xfhaRight;
      taCenter: HAlign := xfhaCentered;
    end;


    XFBorder(L);
    XFBorder(T);
    XFBorder(R);
    XFBorder(B);

    SetBg(XF, psSolid);

    if Assigned(AFormat) then
      Format := GetFormat
    else
    if IsPercent then
      Format := BiffFmtPercentFixed
    else
      Format := 0;
    Font := CreateFont(S.Font, {$IFDEF FMX}AlphaColorToColor(S.TextColor){$ELSE}S.TextColor{$ENDIF});

    UsedAttrs := BiffXfuaAll;
  end;

  with WorkBook do
    Result := AddXF(XF);
end;

function TfcxBiffStyleCache.GetStyleID(AStyle: TfcxCustomThemeStyle;
  AAlignment: TAlignment; AFormat: TfcxFormat; IsPercent: Boolean): Integer;
var
  I: Integer;
begin
  if Length(FStyles) <= AStyle.Index then
    SetLength(FStyles, AStyle.Index + 1)
  else
  begin
    for I := 0 to High(FStyles[AStyle.Index]) do
      if (FStyles[AStyle.Index][I].Alignment = AAlignment) and
         (FStyles[AStyle.Index][I].IsPercent = IsPercent) and
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
  Result := CreateStyle(AStyle, AAlignment, AFormat, IsPercent);
  FStyles[AStyle.Index][I].ID := Result;
  FStyles[AStyle.Index][I].Alignment := AAlignment;
  FStyles[AStyle.Index][I].IsPercent := IsPercent;
  FStyles[AStyle.Index][I].Format := AFormat;
end;

procedure TfcxBiffStyleCache.SetWorkbook(const Value: TBiffWorkbook);
begin
  FWorkbook := Value;
  if Assigned(FWorkbook) then
  begin
    FZCW := FWorkbook.Font[0].StrWidth('0');
    FTSW := FWorkbook.Font[0].StrWidth('   ');
  end;
  Finalize(FStyles);
end;

end.
