{*******************************************************}
{                                                       }
{                FastCube 2 chart view                  }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxChart;

interface

{$INCLUDE fcx.inc}
{$INCLUDE fcxTee.inc}

uses
  Types, Classes, SysUtils, Graphics, Controls, ComCtrls, Menus, Forms, ImgList,
  fcxTypes, fcxXml, fcxSlice, fcxAlerts, fcxCustomToolbar, fcxComponent,
{$IFDEF FPC}
  TAGraph, TACustomSeries, TAChartUtils, TASeries, TALegend, TATools
{$ELSE}
  {$IFDEF DELPHI_16UP}
    VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series, VCLTee.TeCanvas
  {$ELSE}
    TeeProcs, TeEngine, Chart, Series, TeCanvas
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI_6UP}
  , Variants
{$ENDIF}
;

type
  TfcxChartLogType = (
    ltSimple,
    ltHalfLog,
    ltFullLog
  );

  TfcxMarksShowStyle = (
    ssNone,
    ssAlways
  );

  TfcxAxisDataType = (
    adtString,
    adtNumeric,
    adtDateTime
  );

  TfcxSeriesType = (
    stTBarSeries,
    stTLineSeries,
    stTPointSeries,
    stTAreaSeries,
    stTPieSeries,
    stTHorizBarSeries
  );

{$IFDEF FPC}
  TBaseChartClass = TChart;
  TChartSeriesClass = class of TChartSeries;
{$ELSE}
  TBaseChartClass = TCustomChart;
{$ENDIF}

  TfcxChartAction = class(TBasicAction);
  TfcxCustomChart = class;

  TfcxChartGetSeriesClassEvent = procedure(Sender: TfcxCustomChart;
    ASeriesIndex: integer; ASeriesTitle: string; var ASeriesClass: TChartSeriesClass) of object;
  TfcxChartSeriesCreatedEvent = procedure(Sender: TfcxCustomChart; AChartSeries: TChartSeries) of object;
  TfcxChartFilledEvent = procedure(Sender: TfcxCustomChart) of object;

  TfcxCustomChart = class(TBaseChartClass, IfcxChart)
  private
    FListners: TList;
    FSlice: TfcxSlice;
    FUpdating: Boolean;
    FSeriesAxis: TfcxAxisRegion;
    FCategoriesAxis: TfcxAxisRegion;
    FTypeChartData: TfcxTypeChartData;

    FSeriesType: TChartSeriesClass;
    FAllSeriesMarksStyle: TSeriesMarksStyle;
    FSeriesFieldCount: Integer;
    FCategoriesFieldCount: Integer;
    FMeasureFieldIndex: Integer;
    FChangeChart: TNotifyEvent;
    FLogType: TfcxChartLogType;
    FMarksShowStyle: TfcxMarksShowStyle;
    FLegendPopup: TPopupMenu;
    FActive: Boolean;
    FFrozen: Boolean;
    FGetSeriesClass: TfcxChartGetSeriesClassEvent;
    FSeriesCreated: TfcxChartSeriesCreatedEvent;
    FChartFilled: TfcxChartFilledEvent;
    FOnSaveTemplate: TNotifyEvent;
    FOnLoadTemplate: TNotifyEvent;
    FDefaultTemplatePath: string;
{$IFNDEF FPC}
    FStackType: TMultiBar;
{$ENDIF}
    FfcSeriesType: TfcxSeriesType;
    FSkipNullPoints: boolean;
    FBaseAxisDataType: TfcxAxisDataType;
    FRealBaseAxisDataType: TfcxAxisDataType;

    procedure SetSlice(const Value: TfcxSlice);
    procedure SliceChanged(AChangeAlert: TfcxChangeAlert);
    procedure FullUpdate;
    procedure DoChartData(Sender: TfcxSlice; ATypeChartData: TfcxTypeChartData; AData1, AData2: array of String; AValues: array of variant; ASerIndex: integer = -1);

    procedure CreateDataStructure;
    function MakeNewSeries(iFact: Integer; ATitle: string): TChartSeries;
    procedure SetSeriesType(const Value: TChartSeriesClass);
    procedure SetAllSeriesMarksStyle(const Value: TSeriesMarksStyle);
    procedure SetCategoriesAxis(const Value: TfcxAxisRegion);
    procedure SetCategoriesFieldCount(const Value: Integer);
    procedure SetMeasureFieldIndex(const Value: Integer);
    procedure SetSeriesAxis(const Value: TfcxAxisRegion);
    procedure SetSeriesFieldCount(const Value: Integer);
    procedure SetTypeChartData(const Value: TfcxTypeChartData);
    procedure SetLogType(const Value: TfcxChartLogType);
    procedure SetMarksShowStyle(const Value: TfcxMarksShowStyle);
    procedure SetActive(const Value: Boolean);
    procedure SetFrozen(const Value: Boolean);
{$IFNDEF FPC}
    procedure SetStackType(const Value: TMultiBar);
{$ENDIF}
    procedure SetfcxSeriesType(const Value: TfcxSeriesType);
    procedure SetSkipNullPoints(const Value: boolean);
    procedure SetBaseAxisDataType(const Value: TfcxAxisDataType);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Listners_RemoveNotification;
    procedure ChartChange;
    procedure DeleteSeries;
    procedure CreatePopups;
    procedure ButtonClick(Sender: TObject); virtual;
    procedure MenuPopup(Sender: TObject); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SaveToInternalXML(XMLRootItem: TfcxXMLItem);
    procedure LoadFromInternalXML(XMLRootItem: TfcxXMLItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure Updated; override;
    procedure AddListner(Obj : TObject);
    procedure RemoveListner(Obj : TObject);
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SaveToStream(Stream: TStream);
    function LoadFromStream(Stream: TStream): Boolean;
    procedure SaveToFile(AFileName: String);
    function LoadFromFile(AFileName: String): Boolean;
    procedure SaveTemplate(DefaultDirectory: string = '');
    procedure LoadTemplate(DefaultDirectory: string = '');
    procedure SaveToXML(AXMLDoc: TfcxXMLDocument);
    procedure SaveToXMLItem(AItem: TfcxXMLItem);
    function LoadFromXML(AXMLDoc: TfcxXMLDocument): Boolean;
    function LoadFromXMLItem(AItem: TfcxXMLItem): Boolean;

    property TypeChartData: TfcxTypeChartData read FTypeChartData write SetTypeChartData default tcd_ByAxisMeasures;
    property CategoriesAxis: TfcxAxisRegion read FCategoriesAxis write SetCategoriesAxis default ar_RowAxis;
    property SeriesAxis: TfcxAxisRegion read FSeriesAxis write SetSeriesAxis default ar_ColAxis;
    property CategoriesFieldCount: Integer read FCategoriesFieldCount write SetCategoriesFieldCount default 1;
    property SeriesFieldCount: Integer read FSeriesFieldCount write SetSeriesFieldCount default 1;
    property MeasureFieldIndex: Integer read FMeasureFieldIndex write SetMeasureFieldIndex default 0;

    property Frozen: Boolean read FFrozen write SetFrozen;
    property Slice: TfcxSlice read FSlice write SetSlice;
    property Active: Boolean read FActive write SetActive default False;
    property SeriesType: TChartSeriesClass read FSeriesType write SetSeriesType;
    property fcSeriesType: TfcxSeriesType read FfcSeriesType write SetfcxSeriesType default stTBarSeries;
{$IFNDEF FPC}
    property StackType: TMultiBar read FStackType write SetStackType default mbSide;
{$ENDIF}
    property LogType: TfcxChartLogType read FLogType write SetLogType default ltSimple;
    property MarksShowStyle: TfcxMarksShowStyle read FMarksShowStyle write SetMarksShowStyle default ssNone;
    property AllSeriesMarksStyle: TSeriesMarksStyle read FAllSeriesMarksStyle write SetAllSeriesMarksStyle default smsLabelValue;
    property DefaultTemplatePath: string read FDefaultTemplatePath write FDefaultTemplatePath;
    property SkipNullPoints: boolean read FSkipNullPoints write SetSkipNullPoints default False;
    property BaseAxisDataType: TfcxAxisDataType read FBaseAxisDataType write SetBaseAxisDataType default adtString;

    property OnChangeChart: TNotifyEvent read FChangeChart write FChangeChart;
    property OnGetSeriesClass: TfcxChartGetSeriesClassEvent read FGetSeriesClass write FGetSeriesClass;
    property OnSeriesCreated: TfcxChartSeriesCreatedEvent read FSeriesCreated write FSeriesCreated;
    property OnChartFilled: TfcxChartFilledEvent read FChartFilled write FChartFilled;
    property OnSaveTemplate: TNotifyEvent read FOnSaveTemplate write FOnSaveTemplate;
    property OnLoadTemplate: TNotifyEvent read FOnLoadTemplate write FOnLoadTemplate;
  published
    property Version: String read GetVersion write SetVersion;
  end;

  TfcxChartToolbarItem = class(TfcxToolbarItem)
  protected
    class function GetEnabled(Sender: TfcxCustomToolbar): Boolean; override;
  end;

  TfcxChartToolBar = class(TfcxCustomToolBar)
  private
    FChart: TfcxCustomChart;
    procedure SetChart(const Value: TfcxCustomChart); virtual;
  protected
    function GetItems: TfcxToolbarItems; override;
    function GetImageList: TCustomImageList; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
  published
    property Chart: TfcxCustomChart read FChart write SetChart;
  end;

  TfcxChart = class(TfcxCustomChart)
  public
  published
  {$IFNDEF FPC}
    property AllowPanning;
    property AnimatedZoom;
    property AnimatedZoomSteps;
    property BackImage;
    property BackImageInside;
    property BackImageMode;
    property BottomWall;
    property Gradient;
    property LeftWall;
    property MarginBottom;
    property MarginLeft;
    property MarginRight;
    property MarginTop;
    { TCustomChart Events }
    property OnAllowScroll;
    property OnClickAxis;
    property OnClickLegend;
    property OnClickSeries;
    property OnClickBackground;
    property OnGetLegendPos;
    property OnGetLegendRect;
    property OnScroll;
    property OnUndoZoom;
    property OnZoom;
  {$ENDIF}
    property AllowZoom;
    property Foot;
    property Title;
    { TCustomAxisPanel properties }
  {$IFNDEF FPC}
    property Chart3DPercent;
    property ClipPoints;
    property MaxPointsPerPage;
    property Monochrome;
    property Page;
    property RightAxis;
    property ScaleLastPage;
    property SeriesList;
    property TopAxis;
    property View3D;
    property View3DWalls;
    { TCustomAxisPanel events }
    property OnAfterDraw;
    property OnGetAxisLabel;
    property OnGetLegendText;
    property OnGetNextAxisLabel;
    property OnPageChange;
  {$ENDIF}
    property AxisVisible;
    property BackColor;
    property BottomAxis;
    property Frame;
    property LeftAxis;
    property Legend;
    { TPanel properties }
    property Align;
    property Anchors;
    property AutoSize;
  {$IFNDEF FPC}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
  {$ENDIF}
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    { TPanel events }
  {$IFNDEF FPC}
    property OnCanResize;
  {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // Chart
    property Slice;
    property Active;
    property OnChangeChart;
    property TypeChartData;
    property CategoriesAxis;
    property SeriesAxis;
    property CategoriesFieldCount;
    property SeriesFieldCount;
    property MeasureFieldIndex;
//    property SeriesType;
    property fcSeriesType;
  {$IFNDEF FPC}
    property StackType;
  {$ENDIF}
    property LogType;
    property MarksShowStyle;
    property SkipNullPoints;
    property BaseAxisDataType;
    property AllSeriesMarksStyle;
    property DefaultTemplatePath;
    property OnGetSeriesClass;
    property OnChartFilled;
    property OnSeriesCreated;
    property OnSaveTemplate;
    property OnLoadTemplate;
  end;

  {$IFDEF FPC}
  TPointSeries = class(TLineSeries)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THorizBarSeries = class(TBarSeries)
  public
    function AddX(
      AX: Double; ALabel: String = ''; AColor: TColor = clTAColor): Integer;
    constructor Create(AOwner: TComponent); override;
  end;

  {$ENDIF}
  // standard buttons

  TfcxSaveTemplateToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxOpenTemplateToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxChartStyleToolbarItem = class(TfcxChartToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
    class procedure MenuPopup(Sender: TObject);
  protected
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

{$IFNDEF FPC}
  TfcxStackTypeToolbarItem = class(TfcxChartToolbarItem)
  private
    class procedure MenuItemClick(Sender: TObject);
    class procedure MenuPopup(Sender: TObject);
  protected
    class function GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu; override;
    class function GetEnabled(Sender: TfcxCustomToolbar): Boolean; override;
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;
{$ENDIF}

  TfcxFrozenToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
    class function GetChecked(Sender: TfcxCustomToolbar): Boolean; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxDataPropertyToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxMarksToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
    class function GetChecked(Sender: TfcxCustomToolbar): Boolean; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxLegendToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
    class function GetChecked(Sender: TfcxCustomToolbar): Boolean; override;
    class function GetOptions: TfcxToolbarItemOptions; override;
  end;

  TfcxDataManagerToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

  TfcxCopyToolbarItem = class(TfcxChartToolbarItem)
  protected
    class function GetHint: String; override;
    class function GetImageIndex: Integer; override;
    class procedure DoClick(Sender: TfcxToolButton); override;
  end;

function fcxGetSeriesTypeBySeriesClass(ASeriesType: TChartSeriesClass): TfcxSeriesType;
procedure fcxRegisterChartToolBarItem(AItem: TfcxToolbarItemClass);
procedure fcxUnRegisterChartToolBarItem(AItem: TfcxToolbarItemClass);

const
  ChartSeries: array[0..5] of TChartSeriesClass =
  (
    TBarSeries,
    TLineSeries,
    TPointSeries,
    TAreaSeries,
    TPieSeries,
    THorizBarSeries
  );

implementation

uses
  fcxRes,
  fcxGraphicRes,
  fcxChartDataManager,
  fcxChartEditor,
  fcxStreamUtils,
  Dialogs,
  typinfo,
  Math;

{$IFDEF FPC}
const
  clTeeColor = clTAColor;

constructor TPointSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShowLines := False;
  ShowPoints := True;
end;

function THorizBarSeries.AddX(AX: Double; ALabel: String; AColor: TColor): Integer;
begin
  Result := Add(AX, ALabel, AColor);
end;

constructor THorizBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AxisIndexX := 0;
  AxisIndexY := 1;
end;
{$ENDIF}

const
  MarksStyleArr: array[0..7] of TSeriesMarksStyle =
  (
    smsLegend,
    smsValue,
    smsPercent,
    smsLabel,
    smsLabelPercent,
    smsLabelValue,
    smsPercentTotal,
    smsLabelPercentTotal
  );

type
  THackComponent = class(TComponent);

function fcxGetSeriesTypeBySeriesClass(ASeriesType: TChartSeriesClass): TfcxSeriesType;
var
  i: integer;
begin
  Result := stTBarSeries;
  for i := 0 to High(ChartSeries) do
    if ASeriesType = ChartSeries[i] then
    begin
      Result := TfcxSeriesType(i);
      Break;
    end
end;

{ TfcxCustomChart }

procedure TfcxCustomChart.AddListner(Obj: TObject);
begin
  if FListners.IndexOf(Obj) = -1 then
    FListners.Add(Obj);
end;

procedure TfcxCustomChart.ButtonClick(Sender: TObject);
var
  Menu: TPopupMenu;
begin
  if (Sender is TMenuItem) then
  begin
    Menu := TPopupMenu(TMenuItem(Sender).Owner);
    if Menu = FLegendPopup then
      Legend.Alignment := TLegendAlignment((Sender as TMenuItem).MenuIndex);
  end;
end;

procedure TfcxCustomChart.ChartChange;
var
  i: integer;
  Action: TfcxChartAction;
begin
  if FListners <> nil then
  begin
    Action := TfcxChartAction.Create(Self);
    for i := 0 to FListners.Count - 1 do
      TComponent(FListners[i]).ExecuteAction(Action);
    Action.Free;
    if Assigned(FChangeChart) then
      OnChangeChart(Self);
  end;
end;

constructor TfcxCustomChart.Create(AOwner: TComponent);
begin
  inherited;
  FUpdating := False;
  {$IFNDEF FPC}
  FStackType := mbSide;
  {$ENDIF}
  FActive := False;
  FListners   := TList.Create;
  FSeriesType := TBarSeries;
  FfcSeriesType := stTBarSeries;
  FLogType := ltSimple;
  FAllSeriesMarksStyle := smsLabelValue;
  FMarksShowStyle := ssNone;
  FSkipNullPoints := False;

  CreatePopups;

  Title.Text.Text := '';

  FTypeChartData := tcd_ByAxisMeasures;
  FCategoriesAxis := ar_RowAxis;
  FSeriesAxis := ar_ColAxis;
  FCategoriesFieldCount := 1;
  FSeriesFieldCount := 1;
  FMeasureFieldIndex := 0;
  FBaseAxisDataType := adtString;
end;

procedure TfcxCustomChart.CreateDataStructure;
var
  AChartParam: TfcxChartParam;
begin
  If csLoading in ComponentState then
    exit;
  if Frozen then
    exit;
  DeleteSeries;
{$ifdef TeeChartStd}
  Title.Text.Text := '';
{$else}
  {$ifdef TeeChartStd7}
  Title.Text.Text := '';
  {$else}
  {$ifdef TeeChartStd8}
  Title.Text.Text := '';
  {$else}
  {$ifdef TeeChartStd9}
  Title.Text.Text := '';
  {$else}
  //zc SubTitle.Text.Text := '';
  {$endif}
  {$endif}
  {$endif}
{$endif}
  if Assigned(Slice) and {Slice.Active and} Active then
  begin
    AChartParam.TypeChartData := TypeChartData;
    AChartParam.CategoriesAxis := CategoriesAxis;
    AChartParam.SeriesAxis := SeriesAxis;
    AChartParam.CategoriesFieldCount := CategoriesFieldCount;
    AChartParam.SeriesFieldCount := SeriesFieldCount;
    AChartParam.MeasureFieldIndex := MeasureFieldIndex;
    AxisVisible := (SeriesType <> TPieSeries);
    {$IFNDEF FPC}
    View3DWalls := (SeriesType <> TPieSeries);
    {$ENDIF}
    FRealBaseAxisDataType := FBaseAxisDataType;
    Slice.GetChartData(AChartParam, DoChartData);
    if SeriesCount > 0 then
    begin
      if TypeChartData = tcd_ByAxisAxis then
{$ifdef TeeChartStd}
        Title.Text.Text := FSlice.MeasuresContainer.Measures[AChartParam.MeasureFieldIndex].Caption;
{$else}
  {$ifdef TeeChartStd7}
        Title.Text.Text := FSlice.MeasuresContainer.Measures[AChartParam.MeasureFieldIndex].Caption;
  {$else}
  {$ifdef TeeChartStd8}
        Title.Text.Text := FSlice.MeasuresContainer.Measures[AChartParam.MeasureFieldIndex].Caption;
  {$else}
  {$ifdef TeeChartStd9}
        Title.Text.Text := FSlice.MeasuresContainer.Measures[AChartParam.MeasureFieldIndex].Caption;
  {$else}
       //zc SubTitle.Text.Text := FSlice.MeasuresContainer.Measures[AChartParam.MeasureFieldIndex].Caption;
  {$endif}
  {$endif}
  {$endif}
{$endif}
    end
    else
{$ifdef TeeChartStd}
      Title.Text.Text := fcxResources.Get('sChartPropsAreNotCorrect');
{$else}
  {$ifdef TeeChartStd7}
      Title.Text.Text := fcxResources.Get('sChartPropsAreNotCorrect');
  {$else}
  {$ifdef TeeChartStd8}
      Title.Text.Text := fcxResources.Get('sChartPropsAreNotCorrect');
  {$else}
  {$ifdef TeeChartStd9}
      Title.Text.Text := fcxResources.Get('sChartPropsAreNotCorrect');
  {$else}
      //zc SubTitle.Text.Text := fcxResources.Get('sChartPropsAreNotCorrect');
  {$endif}
  {$endif}
  {$endif}
{$endif}
    if Assigned(OnChartFilled) then
      OnChartFilled(Self);
  end;
end;

procedure TfcxCustomChart.CreatePopups;
const
  ChartLegendCaptions: array[0..3] of String = (
    'sChartLegendLeft',
    'sChartLegendRight',
    'sChartLegendTop',
    'sChartLegendBottom'
  );
  
var
  Item: TMenuItem;
  i: integer;
begin
  FLegendPopup := TPopupMenu.Create(nil);
  FLegendPopup.OnPopup := MenuPopup;
  for i := 0 to High(ChartLegendCaptions) do
  begin
    Item := TMenuItem.Create(FLegendPopup);
    with Item do
    begin
      Caption := fcxResources.Get(ChartLegendCaptions[i]);
      OnClick := ButtonClick;
      GroupIndex := 0;
    end;
    FLegendPopup.Items.Add(Item);
  end;
end;

procedure TfcxCustomChart.DeleteSeries;
var
  i: integer;
  aSeries: TChartSeries;
begin
  for i := SeriesCount - 1 downto 0 do
  begin
    aSeries := TChartSeries(series[i]);
    {$IFDEF FPC}
    inherited DeleteSeries(aSeries);
    {$ELSE}
    RemoveSeries(aSeries);
    {$ENDIF}
    aSeries.Free;
  end;
end;

destructor TfcxCustomChart.Destroy;
begin
  Listners_RemoveNotification;
  FreeAndNil(FListners);
  DeleteSeries;
  Slice := nil;
  FLegendPopup.Free;
  inherited Destroy;
end;

function TfcxCustomChart.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action is TfcxAction then
  begin
    if Action.Owner = Slice then
      SliceChanged(TfcxAction(Action).ChangeAlert)
    else
    if Assigned(Slice) and (Action.Owner = Slice.Cube) then
      SliceChanged(TfcxAction(Action).ChangeAlert);
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

procedure TfcxCustomChart.Listners_RemoveNotification;
var
  i: integer;
  Obj: TObject;
begin
  for i := 0 to FListners.Count - 1 do
  begin
    Obj := FListners[i];
    if Obj is TComponent then
    begin
      THackComponent(Obj).Notification(Self, opRemove);
    end;
  end;
end;

function TfcxCustomChart.LoadFromStream(Stream: TStream): Boolean;
const
  XMLSignature: array[0..1] of AnsiChar = '<?';
var
  SeriesTypeName: string;
  Vers: Word;
  b: Boolean;
  XMLDoc: TfcxXMLDocument;
begin
  Result := False;
  if Stream.Read(Vers, SizeOf(Word)) < SizeOf(Word) then
    Exit;
  if Vers = Word(XMLSignature) then
  begin
    // this is XML and should be readed throug LoadFromXML
    XMLDoc := TfcxXMLDocument.Create;
    Stream.Position := Stream.Position - 2;
    XMLDoc.LoadFromStream(Stream);
    LoadFromXML(XMLDoc);
    XMLDoc.Free;
    Result := True;
    Exit;
  end;

  SeriesTypeName := ReadString(Stream);
  FSeriesType := TChartSeriesClass(FindClass(SeriesTypeName));
  FfcSeriesType := fcxGetSeriesTypeBySeriesClass(FSeriesType);
  if Vers < 4 then
    Stream.Read(FMarksShowStyle, SizeOf(TfcxMarksShowStyle));
  if Vers >= 4 then
  begin
    Stream.Read(FTypeChartData, SizeOf(TfcxTypeChartData));
    Stream.Read(FCategoriesAxis, SizeOf(TfcxAxisRegion));
    Stream.Read(FSeriesAxis, SizeOf(TfcxAxisRegion));
    FCategoriesFieldCount := ReadInteger(Stream);
    FSeriesFieldCount := ReadInteger(Stream);
    FMeasureFieldIndex := ReadInteger(Stream);
  end;
  Stream.Read(FLogType, SizeOf(TfcxChartLogType));
  Stream.Read(FMarksShowStyle, SizeOf(TfcxMarksShowStyle));
  Stream.Read(FAllSeriesMarksStyle, SizeOf(TSeriesMarksStyle));
  if Vers < 4 then
    ReadInteger(Stream);

  FullUpdate;

  // version 2
  if Vers >= 2 then
  begin
    Stream.Read(b, SizeOf(Boolean));
    {$IFNDEF FPC}
    View3D := b;
    {$ENDIF}
    if b then
    begin
      {$IFNDEF FPC}Chart3DPercent := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.Elevation := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.Orthogonal := {$ENDIF}ReadBoolean(Stream);
      {$IFNDEF FPC}View3DOptions.HorizOffset := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.VertOffset := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.Perspective := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.Rotation := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.Tilt := {$ENDIF}ReadInteger(Stream);
      {$IFNDEF FPC}View3DOptions.Zoom := {$ENDIF}ReadInteger(Stream);
    end;
  end;
  if Vers >= 3 then
  begin
    Legend.Visible := ReadBoolean(Stream);
  end;
  ChartChange;
  Result := True;
end;

procedure TfcxCustomChart.LoadTemplate(DefaultDirectory: string);
begin
  if Assigned(FOnLoadTemplate) then
    OnLoadTemplate(Self)
  else
  with TOpenDialog.Create(Self) do
  begin
    DefaultExt := '.mdt';
    Filter := fcxResources.Get('sChartTemplateFilter');
    if DefaultTemplatePath = '' then
      if DefaultDirectory <> '' then
        InitialDir := DefaultDirectory
      else
        InitialDir := ExtractFileDir(Application.ExeName)
    else
      InitialDir := DefaultTemplatePath;
    if Execute then
    begin
      LoadFromFile(FileName);
    end;
    Free;
  end;
end;

procedure TfcxCustomChart.SaveTemplate(DefaultDirectory: string);
begin
  if Assigned(FOnSaveTemplate) then
    OnSaveTemplate(Self)
  else
  with TSaveDialog.Create(Self) do
  begin
    Options := Options + [ofOverwritePrompt];
    DefaultExt := '.mdt';
    FileName := 'Chart.mdt';
    Filter := fcxResources.Get('sChartTemplateFilter');
    if DefaultTemplatePath = '' then
      if DefaultDirectory <> '' then
        InitialDir := DefaultDirectory
      else
        InitialDir := ExtractFileDir(Application.ExeName)
    else
      InitialDir := DefaultTemplatePath;
    if Execute then
    begin
      SaveToFile(FileName);
    end;
    Free;
  end;
end;

procedure TfcxCustomChart.SaveToStream(Stream: TStream);
var
  Doc: TfcxXMLDocument;
begin
  Doc := TfcxXMLDocument.Create;
  try
    SaveToXML(Doc);
    Doc.SaveToStream(Stream);
  finally
    Doc.Free;
  end;
end;

function TfcxCustomChart.MakeNewSeries(iFact: Integer; ATitle: string): TChartSeries;
var
  ASeriesClass: TChartSeriesClass;
{$IFDEF FPC}
  PropInfo: PPropInfo;

  function GetSeriesColor(AIndex: Integer): TColor;
  const
    Colors: array[0..19] of TColor = (
      clRed, clGreen, clYellow, clBlue, clWhite,
      clGray, clFuchsia, clTeal, clNavy, clMaroon,
      clLime, clOlive, clPurple, clSilver, clAqua,
      clBlack, clMoneyGreen, clSkyBlue, clCream,
      clMedGray
    );
  begin
    if (AIndex >= Low(Colors)) and (AIndex <= High(Colors)) then
      Result := Colors[AIndex]
    else
      Result := Random(255*255*255);
  end;

{$ENDIF}
begin
  ASeriesClass := SeriesType;
  if Assigned(FGetSeriesClass) then
    OnGetSeriesClass(Self, iFact, ATitle, ASeriesClass);
  {$IFDEF FPC}
  Result := ASeriesClass.Create(Parent);
  PropInfo := GetPropInfo(Result, 'SeriesColor');
  if Assigned(PropInfo) then
    SetPropValue(Result, 'SeriesColor', GetSeriesColor(Series.Count));
  AddSeries(Result);
  {$ELSE}
  Result := CreateNewSeries(Parent, Self, ASeriesClass, nil);

  if Result is THorizBarSeries then
  begin
    Result.YValues.DateTime := FBaseAxisDataType = adtDateTime;
    Result.XValues.DateTime := False;
  end
  else
  begin
    Result.XValues.DateTime := FBaseAxisDataType = adtDateTime;
    Result.YValues.DateTime := False;
  end;

  Result.Identifier := IntToStr(iFact);
  Result.Style := [tssHideDataSource, tssDenyClone];
  {$ENDIF}
  Result.Marks.Visible := MarksShowStyle = ssAlways;
  Result.Marks.Style := AllSeriesMarksStyle;
  Result.Title := ATitle;
  if (Result is TLineSeries) and FSkipNullPoints then
    TLineSeries(Result).Pointer.Visible := True;
  {$IFNDEF FPC}
  if Result is TCustomBarSeries then
    TCustomBarSeries(Result).MultiBar := StackType;
  {$ENDIF}
  if Assigned(FSeriesCreated) then
    OnSeriesCreated(Self, Result);
end;

procedure TfcxCustomChart.MenuPopup(Sender: TObject);
var
  i: integer;
  Menu: TPopupMenu absolute Sender;
begin
  if Menu = FLegendPopup then
  begin
    for i := 0 to FLegendPopup.Items.Count - 1 do
      Menu.Items[i].Checked := Legend.Alignment = TLegendAlignment(i);
  end;
end;

procedure TfcxCustomChart.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{$IFNDEF FPC}
var
  Handled: Boolean;
  tmpPart: TChartClickedPart;
  p: TPoint;
{$ENDIF}
begin
{$IFNDEF FPC}
  Handled := False;
  if not CancelMouse then
  begin
    p := Point(x, y);
    CalcClickedPart(p, tmpPart);
    case tmpPart.Part of
      cpLegend:
        begin
          if Button = mbRight then
          begin
            p := ClientToScreen(p);
            FLegendPopup.Popup(p.X, p.Y);
            Handled := True;
          end;
        end;
    end;
  end;
  if not Handled then
{$ENDIF}
    inherited;
end;

procedure TfcxCustomChart.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FSlice <> nil) and
    (AComponent = FSlice) then
  begin
    FSlice := nil;
    if not (csDestroying in ComponentState) then
      FullUpdate;
  end
end;

procedure TfcxCustomChart.RemoveListner(Obj: TObject);
var
  i: integer;
begin
  i := FListners.IndexOf(Obj);
  if i >= 0 then
    FListners.Delete(i);
end;

procedure TfcxCustomChart.DoChartData(Sender: TfcxSlice; ATypeChartData: TfcxTypeChartData;
  AData1, AData2: array of String; AValues: array of variant;
  ASerIndex: integer);
var
  i: integer;
  aSeries: TChartSeries;
  AStr, ATitle: string;
  AValueReal: Double;
  AValueIsNull: Boolean;
begin
  case ATypeChartData of
    tcd_ByAxisAxis:
      begin
        if ASerIndex = -1 then
        begin
    // new series
          ATitle := AData2[0];
          for i := 1 to High(AData2) do
            ATitle := ATitle + ', ' + AData2[i];
          aSeries := MakeNewSeries(SeriesCount, ATitle);
        {$IFNDEF FPC}
          aSeries.style := aSeries.style + [tssIsPersistent];
        {$ENDIF}
        end
        else
        begin
    // add values to ASerIndex series
          if (Length(AData1) = 1) and (FRealBaseAxisDataType <> adtString) then
          begin
            if FRealBaseAxisDataType = adtNumeric then
              AValueIsNull := not TryStrToFloat(AData1[0], AValueReal)
            else
              AValueIsNull := not TryStrToDateTime(AData1[0], TDateTime(AValueReal));
            if AValueIsNull then
              FRealBaseAxisDataType := adtString;
          end;
          if (Length(AData1) = 1) and (FRealBaseAxisDataType <> adtString) then
          begin
            if (TVarData(AValues[0]).VType = varOleStr) or (TVarData(AValues[0]).VType = varString)
               {$IFDEF Delphi_12UP}or (TVarData(AValues[0]).VType = varUString){$ENDIF} or
               ((TVarData(AValues[0]).VType in [varEmpty, varNull]) and not SkipNullPoints) then
              AValues[0] := 0;
            if FRealBaseAxisDataType = adtNumeric then
              AValueReal := StrToFloat(AData1[0])
            else
              AValueReal := StrToDateTime(AData1[0]);
            if (TVarData(AValues[0]).VType in [varEmpty, varNull]) then
            begin
              if Series[ASerIndex] is THorizBarSeries then
                THorizBarSeries(Series[ASerIndex]).AddXY(0, AValueReal, AData1[0], clTeeColor)
              else
                TChartSeries(Series[ASerIndex]).AddXY(AValueReal, 0, AData1[0], clTeeColor)
            end
            else
            begin
              if Series[ASerIndex] is THorizBarSeries then
                THorizBarSeries(Series[ASerIndex]).AddXY(AValues[0], AValueReal, AStr, clTeeColor)
              else
                TChartSeries(Series[ASerIndex]).AddXY(AValueReal, AValues[0], AStr, clTeeColor)
            end;
          end
          else
          begin
            AStr := AData1[0];
            for i := 1 to High(AData1) do
            begin
              AStr := AStr + ', ' + AData1[i];
            end;
            if (TVarData(AValues[0]).VType = varOleStr) or (TVarData(AValues[0]).VType = varString)
               {$IFDEF Delphi_12UP}or (TVarData(AValues[0]).VType = varUString){$ENDIF} or
               ((TVarData(AValues[0]).VType in [varEmpty, varNull]) and not SkipNullPoints) then
              AValues[0] := 0;
            if (TVarData(AValues[0]).VType in [varEmpty, varNull]) then
              TChartSeries(Series[ASerIndex]).AddNull(AStr)
            else
            begin
              if Series[ASerIndex] is THorizBarSeries then
                THorizBarSeries(Series[ASerIndex]).AddX(AValues[0], AStr, clTeeColor)
              else
                TChartSeries(Series[ASerIndex]).AddY(AValues[0], AStr, clTeeColor);
            end;
          end;
        end;
      end;
    tcd_ByAxisMeasures:
      begin
    // add values to all series
        if SeriesCount = 0 then
        begin
          for i := 0 to High(AData2) do
          begin
            aSeries := MakeNewSeries(i, AData2[i]);
            {$IFNDEF FPC}
            aSeries.style := aSeries.style + [tssIsPersistent];
            {$ENDIF}
          end;
        end;
        if (Length(AData1) = 1) and (FRealBaseAxisDataType <> adtString) then
        begin
          if FRealBaseAxisDataType = adtNumeric then
            AValueIsNull := not TryStrToFloat(AData1[0], AValueReal)
          else
            AValueIsNull := not TryStrToDateTime(AData1[0], TDateTime(AValueReal));
          if AValueIsNull then
            FRealBaseAxisDataType := adtString;
        end;
        if (Length(AData1) = 1) and (FRealBaseAxisDataType <> adtString) then
        begin
          if FRealBaseAxisDataType = adtNumeric then
            AValueReal := StrToFloat(AData1[0])
          else
            AValueReal := StrToDateTime(AData1[0]);
          for i := 0 to High(AValues) do
          begin
            if (TVarData(AValues[i]).VType = varOleStr) or (TVarData(AValues[i]).VType = varString)
               {$IFDEF Delphi_12UP}or (TVarData(AValues[i]).VType = varUString){$ENDIF} or
               ((TVarData(AValues[i]).VType in [varEmpty, varNull]) and not SkipNullPoints) then
              AValues[i] := 0;
            if (TVarData(AValues[i]).VType in [varEmpty, varNull]) then
            begin
              if Series[ASerIndex] is THorizBarSeries then
                THorizBarSeries(Series[i]).AddXY(0, AValueReal, AData1[0], clTeeColor)
              else
                TChartSeries(Series[i]).AddXY(AValueReal, 0, AData1[0], clTeeColor)
            end
            else
            begin
              if Series[i] is THorizBarSeries then
                THorizBarSeries(Series[i]).AddXY(AValues[i], AValueReal, AData1[0], clTeeColor)
              else
                TChartSeries(Series[i]).AddXY(AValueReal, AValues[i], AData1[0], clTeeColor)
            end;
          end;
        end
        else
        begin
          AStr := AData1[0];
          for i := 1 to High(AData1) do
          begin
            AStr := AStr + ', ' + AData1[i];
          end;
          for i := 0 to High(AValues) do
          begin
            if (TVarData(AValues[i]).VType = varOleStr) or (TVarData(AValues[i]).VType = varString)
               {$IFDEF Delphi_12UP}or (TVarData(AValues[i]).VType = varUString){$ENDIF} or
               ((TVarData(AValues[i]).VType in [varEmpty, varNull]) and not SkipNullPoints) then
              AValues[i] := 0;
            if (TVarData(AValues[i]).VType in [varEmpty, varNull]) then
              TChartSeries(Series[i]).AddNull(AStr)
            else
            begin
              if Series[i] is THorizBarSeries then
                THorizBarSeries(Series[i]).AddX(AValues[i], AStr, clTeeColor)
              else
                TChartSeries(Series[i]).AddY(AValues[i], AStr, clTeeColor);
            end;
          end;
        end;
      end;
    tcd_ByMeasuresAxis:
      begin
    // add series with values
        ATitle := AData2[0];
        for i := 1 to High(AData2) do
          ATitle := ATitle + ', ' + AData2[i];
        aSeries := MakeNewSeries(SeriesCount, ATitle);
        {$IFNDEF FPC}
        aSeries.style := aSeries.style + [tssIsPersistent];
        {$ENDIF}
        for i := 0 to High(AData1) do
        begin
          if (FRealBaseAxisDataType <> adtString) then
          begin
            if FRealBaseAxisDataType = adtNumeric then
              AValueIsNull := not TryStrToFloat(AData1[i], AValueReal)
            else
              AValueIsNull := not TryStrToDateTime(AData1[i], TDateTime(AValueReal));
            if AValueIsNull then
              FRealBaseAxisDataType := adtString;
          end;

          if (FRealBaseAxisDataType <> adtString) then
          begin
            if (TVarData(AValues[i]).VType = varOleStr) or (TVarData(AValues[i]).VType = varString)
               {$IFDEF Delphi_12UP}or (TVarData(AValues[i]).VType = varUString){$ENDIF} or
               ((TVarData(AValues[i]).VType in [varEmpty, varNull]) and not SkipNullPoints) then
              AValues[i] := 0;
            if FRealBaseAxisDataType = adtNumeric then
              AValueReal := StrToFloat(AData1[i])
            else
              AValueReal := StrToDateTime(AData1[i]);
            if (TVarData(AValues[i]).VType in [varEmpty, varNull]) then
            begin
              if aSeries is THorizBarSeries then
                THorizBarSeries(aSeries).AddXY(0, AValueReal, AData1[i], clTeeColor)
              else
                TChartSeries(aSeries).AddXY(AValueReal, 0, AData1[i], clTeeColor)
            end
            else
            begin
              if aSeries is THorizBarSeries then
                THorizBarSeries(aSeries).AddXY(AValues[i], AValueReal, AData1[i], clTeeColor)
              else
                TChartSeries(aSeries).AddXY(AValueReal, AValues[i], AData1[i], clTeeColor)
            end;
          end
          else
          begin
            if (TVarData(AValues[i]).VType = varOleStr) or (TVarData(AValues[i]).VType = varString)
               {$IFDEF Delphi_12UP}or (TVarData(AValues[i]).VType = varUString){$ENDIF} or
               ((TVarData(AValues[i]).VType in [varEmpty, varNull]) and not SkipNullPoints) then
              AValues[i] := 0;
            if (TVarData(AValues[i]).VType in [varEmpty, varNull]) then
              TChartSeries(Series[i]).AddNull(AData1[i])
            else
            begin
              if aSeries is THorizBarSeries then
                THorizBarSeries(aSeries).AddX(AValues[i], AData1[i], clTeeColor)
              else
                TChartSeries(aSeries).AddY(AValues[i], AData1[i], clTeeColor);
            end;
          end;
        end;
      end;
  end;
end;

procedure TfcxCustomChart.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FullUpdate;
  end;
end;

procedure TfcxCustomChart.SetAllSeriesMarksStyle(
  const Value: TSeriesMarksStyle);
var
  i: integer;
begin
  if FAllSeriesMarksStyle <> Value then
  begin
    FAllSeriesMarksStyle := Value;
    for i := 0 to SeriesCount - 1 do
      TChartSeries(Series[i]).Marks.Style := Value;
  end;
end;

procedure TfcxCustomChart.SetFrozen(const Value: Boolean);
begin
  if FFrozen <> Value then
  begin
    FFrozen := Value;
    if FFrozen = False then
      FullUpdate
    else
      ChartChange;
  end
end;

procedure TfcxCustomChart.SetLogType(const Value: TfcxChartLogType);
begin
  if FLogType <> Value then
  begin
    FLogType := Value;
{$IFNDEF FPC}
    case Value of
      ltSimple:
        begin
          LeftAxis.Logarithmic := false;
          BottomAxis.Logarithmic := false;
        end;
      ltHalfLog:
        begin
          LeftAxis.Logarithmic := true;
          BottomAxis.Logarithmic := false;
        end;
      ltFullLog:
        begin
          LeftAxis.Logarithmic := true;
          BottomAxis.Logarithmic := true;
        end;
    end;
{$ENDIF}
  end;
end;

procedure TfcxCustomChart.SetCategoriesAxis(const Value: TfcxAxisRegion);
begin
  FCategoriesAxis := Value;
  Updated;
end;

procedure TfcxCustomChart.SetCategoriesFieldCount(const Value: Integer);
begin
  FCategoriesFieldCount := Value;
  Updated;
end;

procedure TfcxCustomChart.SetMarksShowStyle(const Value: TfcxMarksShowStyle);
var
  V: Boolean;
  i: integer;
begin
  if FMarksShowStyle <> Value then
  begin
    FMarksShowStyle := Value;
    V := (Value = ssAlways);
    for i := 0 to SeriesCount - 1 do
    begin
      TChartSeries(Series[i]).Marks.Visible := V;
    end;
  end;
end;

procedure TfcxCustomChart.SetMeasureFieldIndex(const Value: Integer);
begin
  FMeasureFieldIndex := Value;
  Updated;
end;

procedure TfcxCustomChart.SetSeriesAxis(const Value: TfcxAxisRegion);
begin
  FSeriesAxis := Value;
  Updated;
end;

procedure TfcxCustomChart.SetSeriesFieldCount(const Value: Integer);
begin
  FSeriesFieldCount := Value;
  Updated;
end;

procedure TfcxCustomChart.SetSeriesType(const Value: TChartSeriesClass);
begin
  if FSeriesType <> Value then
  begin
    FSeriesType := Value;
    FfcSeriesType := fcxGetSeriesTypeBySeriesClass(FSeriesType);
    {$IFNDEF FPC}
    View3DOptions.Orthogonal := True;
    {$ENDIF}
    FullUpdate;
  end;
end;

procedure TfcxCustomChart.SetSlice(const Value: TfcxSlice);
begin
  if FSlice <> Value then
  begin
    if FSlice <> nil then
    begin
      FSlice.ListnersManager.RemoveListner(Self);
      FSlice.RemoveFreeNotification(Self);
      if Assigned(FSlice.Cube) then
        FSlice.Cube.ListnersManager.RemoveListner(Self);
    end;
    FSlice := Value;
    FullUpdate;
    if FSlice <> nil then
    begin
      FSlice.FreeNotification(Self);
      FSlice.ListnersManager.AddListner(Self);
      if Assigned(FSlice.Cube) then
        FSlice.Cube.ListnersManager.AddListner(Self);
    end;
  end;
end;

procedure TfcxCustomChart.SetTypeChartData(const Value: TfcxTypeChartData);
begin
  FTypeChartData := Value;
  Updated;
end;

procedure TfcxCustomChart.SliceChanged(AChangeAlert: TfcxChangeAlert);
begin
  if not Assigned(Slice) then
    Exit;
  FullUpdate;
end;

procedure TfcxCustomChart.Updated;
begin
  inherited Updated;
  FullUpdate;
end;

function TfcxCustomChart.LoadFromXML(AXMLDoc: TfcxXMLDocument): Boolean;
begin
  Result := LoadFromXMLItem(AXMLDoc.Root);
end;

function TfcxCustomChart.LoadFromXMLItem(AItem: TfcxXMLItem): Boolean;
begin
  LoadFromInternalXML(AItem);
  ChartChange;
  Result := True;
end;

procedure TfcxCustomChart.LoadFromInternalXML(XMLRootItem: TfcxXMLItem);
var
  i: integer;
  XMLItem: TfcxXMLItem;

  procedure LoadChartProperties(Item: TfcxXMLItem);
  var
    SeriesTypeName: string;
  begin
    SeriesTypeName := Item.Prop['SeriesType'];
    FSeriesType    := TChartSeriesClass(FindClass(SeriesTypeName));
    FfcSeriesType  := fcxGetSeriesTypeBySeriesClass(FSeriesType);
    {$IFNDEF FPC}
    if XMLItem.PropExists('StackType') then
      if GetEnumValue(TypeInfo(TMultiBar), Item.Prop['StackType']) >= 0 then
        FStackType  := TMultiBar(GetEnumValue(TypeInfo(TMultiBar), Item.Prop['StackType']));
    {$ENDIF}
    FTypeChartData  := TfcxTypeChartData(GetEnumValue(TypeInfo(TfcxTypeChartData), Item.Prop['TypeChartData']));
    FCategoriesAxis := TfcxAxisRegion(GetEnumValue(TypeInfo(TfcxAxisRegion), Item.Prop['CategoriesAxis']));
    FSeriesAxis     := TfcxAxisRegion(GetEnumValue(TypeInfo(TfcxAxisRegion), Item.Prop['SeriesAxis']));

    FCategoriesFieldCount := Item.IntProp['CategoriesFieldCount'];
    FSeriesFieldCount := Item.IntProp['SeriesFieldCount'];
    FMeasureFieldIndex := Item.IntProp['MeasureFieldIndex'];

    FLogType             := TfcxChartLogType(GetEnumValue(TypeInfo(TfcxChartLogType), Item.Prop['LogType']));
    FMarksShowStyle      := TfcxMarksShowStyle(GetEnumValue(TypeInfo(TfcxMarksShowStyle), Item.Prop['MarksShowStyle']));

    if XMLItem.PropExists('SkipNullPoints') then
      FSkipNullPoints    := Item.BoolProp['SkipNullPoints']
    else
      FSkipNullPoints    := False;

    FAllSeriesMarksStyle := TSeriesMarksStyle(GetEnumValue(TypeInfo(TSeriesMarksStyle), Item.Prop['AllSeriesMarksStyle']));

    FullUpdate;

    Legend.Visible := Item.BoolProp['Legend_Visible'];
    {$IFNDEF FPC}
    View3D := Item.BoolProp['View3D'];
    {$ENDIF}
  end;

  procedure LoadView3DProperties(Item: TfcxXMLItem);
  begin
    {$IFNDEF FPC}
    Chart3DPercent := Item.IntProp['Chart3DPercent'];
    View3DOptions.Elevation   := Item.IntProp['Elevation'];
    View3DOptions.Orthogonal  := Item.BoolProp['Orthogonal'];
    View3DOptions.HorizOffset := Item.IntProp['HorizOffset'];
    View3DOptions.VertOffset  := Item.IntProp['VertOffset'];
    View3DOptions.Perspective := Item.IntProp['Perspective'];
    View3DOptions.Rotation    := Item.IntProp['Rotation'];
    View3DOptions.Tilt        := Item.IntProp['Tilt'];
    View3DOptions.Zoom        := Item.IntProp['Zoom'];
    {$ENDIF}
  end;
begin
  for i := 0 to XMLRootItem.Count - 1 do
  begin
    XMLItem := XMLRootItem[i];
    if XMLItem.Name = 'chart_properties' then
      LoadChartProperties(XMLItem) else
    if XMLItem.Name = 'View3DProperties' then
      LoadView3DProperties(XMLItem);
  end;
end;

procedure TfcxCustomChart.SaveToXML(AXMLDoc: TfcxXMLDocument);
begin
  SaveToXMLItem(AXMLDoc.Root);
end;

procedure TfcxCustomChart.SaveToXMLItem(AItem: TfcxXMLItem);
begin
  AItem.Name := 'chart';
  SaveToInternalXML(AItem);
end;

procedure TfcxCustomChart.SaveToInternalXML(XMLRootItem: TfcxXMLItem);
var
  XMLItem: TfcxXMLItem;
begin
  XMLRootItem.Prop['timestamp'] := DateTimeToStr(Now);
  XMLItem := XMLRootItem.Add;
  XMLItem.Name := 'chart_properties';
  XMLItem.Prop['SeriesType'] := SeriesType.ClassName;
  {$IFNDEF FPC}
  XMLItem.Prop['StackType'] := GetEnumName(TypeInfo(TMultiBar), Ord(StackType));
  {$ENDIF}
  XMLItem.Prop['TypeChartData'] := GetEnumName(TypeInfo(TfcxTypeChartData), Ord(TypeChartData));
  XMLItem.Prop['CategoriesAxis'] := GetEnumName(TypeInfo(TfcxAxisRegion), Ord(CategoriesAxis));
  XMLItem.Prop['SeriesAxis'] := GetEnumName(TypeInfo(TfcxAxisRegion), Ord(SeriesAxis));
  XMLItem.IntProp['CategoriesFieldCount'] := CategoriesFieldCount;
  XMLItem.IntProp['SeriesFieldCount'] := SeriesFieldCount;
  XMLItem.IntProp['MeasureFieldIndex'] := MeasureFieldIndex;

  XMLItem.Prop['LogType'] := GetEnumName(TypeInfo(TfcxChartLogType), Ord(LogType));
  XMLItem.Prop['MarksShowStyle'] := GetEnumName(TypeInfo(TfcxMarksShowStyle), Ord(MarksShowStyle));
  XMLItem.BoolProp['SkipNullPoints'] := FSkipNullPoints;
  XMLItem.Prop['AllSeriesMarksStyle'] := GetEnumName(TypeInfo(TSeriesMarksStyle), Ord(AllSeriesMarksStyle));

  XMLItem.BoolProp['Legend_Visible'] := Legend.Visible;
{$IFNDEF FPC}
  XMLItem.BoolProp['View3D'] := View3D;
  if View3D then
  begin
    XMLItem := XMLRootItem.Add;
    XMLItem.Name := 'View3DProperties';
    XMLItem.IntProp['Chart3DPercent'] := Chart3DPercent;
    XMLItem.IntProp['Elevation']      := View3DOptions.Elevation;
    XMLItem.BoolProp['Orthogonal']    := View3DOptions.Orthogonal;
    XMLItem.IntProp['HorizOffset']    := View3DOptions.HorizOffset;
    XMLItem.IntProp['VertOffset']     := View3DOptions.VertOffset;
    XMLItem.IntProp['Perspective']    := View3DOptions.Perspective;
    XMLItem.IntProp['Rotation']       := View3DOptions.Rotation;
    XMLItem.IntProp['Tilt']           := View3DOptions.Tilt;
    XMLItem.IntProp['Zoom']           := View3DOptions.Zoom;
  end;
{$ENDIF}
end;

{$IFNDEF FPC}
procedure TfcxCustomChart.SetStackType(const Value: TMultiBar);
begin
  if FStackType <> Value then
  begin
    FStackType := Value;
    View3DOptions.Orthogonal := True;
    FullUpdate;
  end;
end;
{$ENDIF}

procedure TfcxCustomChart.SeTfcxSeriesType(const Value: TfcxSeriesType);
begin
  if FfcSeriesType <> Value then
  begin
    FfcSeriesType := Value;
    FSeriesType := ChartSeries[ord(FfcSeriesType)];
{$IFNDEF FPC}
    View3DOptions.Orthogonal := True;
{$ENDIF}
    FullUpdate;
  end;
end;

procedure TfcxCustomChart.SetSkipNullPoints(const Value: boolean);
var
  i: integer;
begin
  if FSkipNullPoints <> Value then
  begin
    FSkipNullPoints := Value;
    for i := 0 to SeriesCount - 1 do
    begin
      if Series[i] is TLineSeries then
        TLineSeries(Series[i]).Pointer.Visible := FSkipNullPoints;
    end;
    FullUpdate;
  end;
end;

var
  ChartToolbarItems: TfcxToolbarItems = nil;

procedure fcxRegisterChartToolBarItem(AItem: TfcxToolbarItemClass);
begin
  if ChartToolbarItems.IndexOf(AItem) = -1 then
    ChartToolbarItems.Add(AItem);
end;

procedure fcxUnRegisterChartToolBarItem(AItem: TfcxToolbarItemClass);
var
  AIndex: Integer;
begin
  AIndex := ChartToolbarItems.IndexOf(AItem);
  if AIndex <> -1 then
    ChartToolbarItems.Delete(AIndex);
end;

procedure TfcxCustomChart.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TfcxCustomChart.EndUpdate;
begin
  FUpdating := False;
  FullUpdate;
end;

procedure TfcxCustomChart.SetBaseAxisDataType(const Value: TfcxAxisDataType);
begin
  if FBaseAxisDataType <> Value then
  begin
    FBaseAxisDataType := Value;
    FullUpdate;
  end;
end;

function TfcxCustomChart.LoadFromFile(AFileName: String): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TfcxCustomChart.SaveToFile(AFileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TfcxCustomChart.GetVersion: String;
begin
  Result := FCX_VERSION;
end;

procedure TfcxCustomChart.SetVersion(const Value: String);
begin
//
end;

{ TfcxChartToolBar }

destructor TfcxChartToolBar.Destroy;
begin
  Chart := nil;
  inherited;
end;

function TfcxChartToolBar.ExecuteAction(Action: TBasicAction): Boolean;
var
  ChartAction: TfcxChartAction absolute Action;
begin
  if Action is TfcxChartAction then
  begin
    UpdateState;
    Result := True;
  end
  else
    Result := inherited ExecuteAction(Action);
end;

function TfcxChartToolBar.GetImageList: TCustomImageList;
begin
  Result := fcxGraphicResources.ChartImages;
end;

function TfcxChartToolBar.GetItems: TfcxToolbarItems;
begin
  Result := ChartToolbarItems;
end;

procedure TfcxChartToolBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FChart) then
  begin
    FChart := nil;
    UpdateState;
  end;
end;

procedure TfcxChartToolBar.SetChart(const Value: TfcxCustomChart);
begin
  if FChart <> Value then
  begin
    if Assigned(Value) then
      Value.AddListner(Self)
    else
      FChart.RemoveListner(Self);
    FChart := Value;
    UpdateState;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

{
если количество полей в оси меньше, чем требует пользователь
если в режиме ось-ось оси одинаковы
если в режиме с показателями индекс поля "показателя" в оси меньше или равен количеству требуемых полей из этой оси
если в режиме ось-ось индекс показателя больше или равен числу видимых показателей
}

procedure TfcxCustomChart.FullUpdate;
begin
  if not FUpdating then
  begin
    CreateDataStructure;
    ChartChange;
  end;
end;

{ TfcxChartToolbarItem }

class function TfcxChartToolbarItem.GetEnabled(Sender: TfcxCustomToolbar): Boolean;
var
  ToolBar: TfcxChartToolbar absolute Sender;
begin
 Result := Assigned(ToolBar.Chart) and
   Assigned(ToolBar.Chart.Slice) and
   Assigned(ToolBar.Chart.Slice.Cube);
end;

{ TfcxSaveTemplateToolbarItem }

class procedure TfcxSaveTemplateToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxChartToolBar(Sender.Owner).Chart.SaveTemplate('');
end;

class function TfcxSaveTemplateToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartSaveTemplate');
end;

class function TfcxSaveTemplateToolbarItem.GetImageIndex: Integer;
begin
  Result := 0;
end;

{ TfcxOpenTemplateToolbarItem }

class procedure TfcxOpenTemplateToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxChartToolBar(Sender.Owner).Chart.LoadTemplate('');
end;

class function TfcxOpenTemplateToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartOpenTemplate');
end;

class function TfcxOpenTemplateToolbarItem.GetImageIndex: Integer;
begin
  Result := 1;
end;

{ TfcxChartStyleToolbarItem }

class function TfcxChartStyleToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
const
  ChartSeriesCaptions: array[0..5] of String = ('sChartStyleBar', 'sChartStyleLine', 'sChartStylePoint', 'sChartStyleArea', 'sChartStylePie', 'sChartStyleHorizBar');
  ChartSeriesImages  : array[0..5] of Integer = (7, 8, 9, 10, 11, 13);
var
  i: integer;
  Item: TMenuItem;
begin
  Result := inherited GetDropDownMenu(Owner);
  Result.Images := fcxGraphicResources.ChartImages;
  for i := Low(ChartSeriesCaptions) to High(ChartSeriesCaptions) do
  begin
    Item := TMenuItem.Create(Result);
    with Item do
    begin
      Caption := fcxResources.Get(ChartSeriesCaptions[i]);
      ImageIndex := ChartSeriesImages[i];
      OnClick := MenuItemClick;
      RadioItem := True;
    end;
    Result.Items.Add(Item);
  end;
  Result.OnPopup := MenuPopup;
end;

class function TfcxChartStyleToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartStyle');
end;

class function TfcxChartStyleToolbarItem.GetImageIndex: Integer;
begin
  Result := 7;
end;

class function TfcxChartStyleToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioStartGroup, ioHasDropDown];
end;

class procedure TfcxChartStyleToolbarItem.MenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  TfcxChartToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar).Chart.SeriesType := ChartSeries[MenuItem.MenuIndex];
  TfcxPopupMenu(MenuItem.Owner).ToolButton.ImageIndex := MenuItem.ImageIndex;
end;

class procedure TfcxChartStyleToolbarItem.MenuPopup(Sender: TObject);
var
  PopupMenu: TfcxPopupMenu absolute Sender;
  I: integer;
begin
  for I := 0 to High(ChartSeries) do
    PopupMenu.Items[I].Checked := ChartSeries[I] = TfcxChartToolbar(PopupMenu.ToolButton.Toolbar).Chart.SeriesType;
end;

{ TfcxFrozenToolbarItem }

class procedure TfcxFrozenToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxChartToolBar(Sender.Owner).Chart.Frozen := not TfcxChartToolBar(Sender.Owner).Chart.Frozen;
end;

class function TfcxFrozenToolbarItem.GetChecked(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := TfcxChartToolBar(Sender).Chart.Frozen;
end;

class function TfcxFrozenToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartFrozen');
end;

class function TfcxFrozenToolbarItem.GetImageIndex: Integer;
begin
  Result := 5;
end;

class function TfcxFrozenToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioStartGroup, ioCheck];
end;

{ TfcxDataPropertyToolbarItem }

class procedure TfcxDataPropertyToolbarItem.DoClick(Sender: TfcxToolButton);
var
  ChartEditor: TfcxChartEditor;
begin
  ChartEditor := TfcxChartEditor.Create(Application);
  try
    ChartEditor.Chart := TfcxChartToolBar(Sender.Owner).Chart;
    ChartEditor.ShowModal;
  finally
    ChartEditor.Free;
  end;
end;

class function TfcxDataPropertyToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartDataProperty');
end;

class function TfcxDataPropertyToolbarItem.GetImageIndex: Integer;
begin
  Result := 6;
end;

{ TfcxMarksToolbarItem }

class procedure TfcxMarksToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  if TfcxChartToolBar(Sender.Owner).Chart.MarksShowStyle = ssNone then
    TfcxChartToolBar(Sender.Owner).Chart.MarksShowStyle := ssAlways
  else
    TfcxChartToolBar(Sender.Owner).Chart.MarksShowStyle := ssNone;
end;

class function TfcxMarksToolbarItem.GetChecked(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := TfcxChartToolBar(Sender).Chart.MarksShowStyle = ssAlways;
end;

class function TfcxMarksToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartMarks');
end;

class function TfcxMarksToolbarItem.GetImageIndex: Integer;
begin
  Result := 3;
end;

class function TfcxMarksToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioCheck];
end;

{ TfcxLegendToolbarItem }

class procedure TfcxLegendToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxChartToolBar(Sender.Owner).Chart.Legend.Visible := not TfcxChartToolBar(Sender.Owner).Chart.Legend.Visible;
end;

class function TfcxLegendToolbarItem.GetChecked(Sender: TfcxCustomToolbar): Boolean;
begin
  Result := TfcxChartToolBar(Sender).Chart.Legend.Visible;
end;

class function TfcxLegendToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartLegend');
end;

class function TfcxLegendToolbarItem.GetImageIndex: Integer;
begin
  Result := 2;
end;

class function TfcxLegendToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioCheck];
end;

{ TfcxDataManagerToolbarItem }

class procedure TfcxDataManagerToolbarItem.DoClick(Sender: TfcxToolButton);
var
  DataManager: TfcxChartDataManager;
begin
  DataManager := TfcxChartDataManager.Create(Application);
  try
    DataManager.Slice := TfcxChartToolBar(Sender.Owner).Chart.Slice;
    DataManager.Execute;
  finally
    DataManager.Free;
  end;
end;

class function TfcxDataManagerToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartDataManager');
end;

class function TfcxDataManagerToolbarItem.GetImageIndex: Integer;
begin
  Result := 4;
end;

{ TfcxCopyToolbarItem }

class procedure TfcxCopyToolbarItem.DoClick(Sender: TfcxToolButton);
begin
  TfcxChartToolBar(Sender.Owner).Chart.CopyToClipboardBitmap;
end;

class function TfcxCopyToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sCopy');
end;

class function TfcxCopyToolbarItem.GetImageIndex: Integer;
begin
  Result := 12;
end;

{ TfcxStackTypeToolbarItem }

{$IFNDEF FPC}
class function TfcxStackTypeToolbarItem.GetDropDownMenu(Owner: TfcxToolButton): TfcxPopupMenu;
const
  ChartStackCaptions: array[0..5] of String = ('sChartStackNone', 'sChartStackSide', 'sChartStackStacked', 'sChartStackStacked100', 'sChartStackSideAll', 'sChartStackSelfStack');
var
  i: integer;
  Item: TMenuItem;
begin
  Result := inherited GetDropDownMenu(Owner);
  Result.Images := fcxGraphicResources.ChartImages;
  for i := Low(ChartStackCaptions) to High(ChartStackCaptions) do
  begin
    Item := TMenuItem.Create(Result);
    with Item do
    begin
      Caption := fcxResources.Get(ChartStackCaptions[i]);
      ImageIndex := -1;
      OnClick := MenuItemClick;
      RadioItem := True;
    end;
    Result.Items.Add(Item);
  end;
  Result.OnPopup := MenuPopup;
end;

class function TfcxStackTypeToolbarItem.GetEnabled(Sender: TfcxCustomToolbar): Boolean;
var
  ToolBar: TfcxChartToolbar absolute Sender;
begin
  Result := inherited GetEnabled(Sender);
  if Result then
    Result := ToolBar.Chart.SeriesType.InheritsFrom(TCustomBarSeries);
end;

class function TfcxStackTypeToolbarItem.GetHint: String;
begin
  Result := fcxResources.Get('sChartStackType');
end;

class function TfcxStackTypeToolbarItem.GetImageIndex: Integer;
begin
  Result := 14;
end;

class function TfcxStackTypeToolbarItem.GetOptions: TfcxToolbarItemOptions;
begin
  Result := [ioHasDropDown];
end;

class procedure TfcxStackTypeToolbarItem.MenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem absolute Sender;
begin
  TfcxChartToolbar(TfcxPopupMenu(MenuItem.Owner).ToolButton.Toolbar).Chart.StackType := TMultiBar(MenuItem.MenuIndex);
end;

class procedure TfcxStackTypeToolbarItem.MenuPopup(Sender: TObject);
var
  PopupMenu: TfcxPopupMenu absolute Sender;
  I: integer;
begin
  for I := 0 to PopupMenu.Items.Count - 1 do
    PopupMenu.Items[I].Checked := Ord(TfcxChartToolbar(PopupMenu.ToolButton.Toolbar).Chart.StackType) = I;
end;
{$ENDIF}

initialization
  ChartToolbarItems := TfcxToolbarItems.Create;
  fcxRegisterChartToolBarItem(TfcxSaveTemplateToolbarItem);
  fcxRegisterChartToolBarItem(TfcxOpenTemplateToolbarItem);
  fcxRegisterChartToolBarItem(TfcxChartStyleToolbarItem);
  {$IFNDEF FPC}
  fcxRegisterChartToolBarItem(TfcxStackTypeToolbarItem);
  {$ENDIF}
  fcxRegisterChartToolBarItem(TfcxFrozenToolbarItem);
  fcxRegisterChartToolBarItem(TfcxDataPropertyToolbarItem);
  fcxRegisterChartToolBarItem(TfcxMarksToolbarItem);
  fcxRegisterChartToolBarItem(TfcxLegendToolbarItem);
  fcxRegisterChartToolBarItem(TfcxDataManagerToolbarItem);
  fcxRegisterChartToolBarItem(TfcxCopyToolbarItem);
{$IFDEF DELPHI_12UP}
  RegisterTeeStandardSeries;
{$ELSE}
{$IFNDEF TeeChartStd}
{$IFNDEF TeeChartStd7}
{$IFNDEF TeeChartStd8}
{$IFNDEF TeeChartStd9}
{$IFNDEF TeeChart4}
  //zc RegisterTeeStandardSeries;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

finalization
  ChartToolbarItems.Free;
end.

