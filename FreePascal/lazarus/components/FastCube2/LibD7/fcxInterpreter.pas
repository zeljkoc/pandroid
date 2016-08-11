{*******************************************************}
{                                                       }
{      FastCube 2 interpreter interface implementation  }
{                                                       }
{                Copyright (c) 2007-2014                }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxInterpreter;

interface
{$I fcx.inc}

uses
  SysUtils, Classes, fcxSlice, fcxTypes, fcxComponent
{$IFDEF INCLUDE_FAST_SCRIPT}
  , fs_iinterpreter, fs_ipascal
{$ENDIF}
{$IFDEF Delphi_6UP}
  , Variants
{$ENDIF};
//FMX uses
{$ELSE FMX}

interface
{$I fcx.inc}

uses
  System.SysUtils, System.Classes, System.Variants,
  FMX.fcxSlice, FMX.fcxTypes, FMX.fcxComponent
{$IFDEF INCLUDE_FAST_SCRIPT}
  , FMX.fs_iinterpreter, FMX.fs_ipascal
{$ENDIF}
;
{$ENDIF FMX}

type

  TCommonScriptItems = class(TPersistent)
  protected
    FItems: TStringList;
    function GetCount: Integer; virtual;
    function IndexOf(AString: String): Integer; virtual;
  public
    FTotalOnX, FTotalOnY: Boolean;
    FBaseLevel, FSecondLevel: TfcxSmallCount;
    FXLevel, FYLevel: TfcxSmallCount;
    FIndexInBaseLevel, FIndexInSecondLevel: Integer;
    FIndexInXLevel, FIndexInYLevel: Integer;
    FBaseAdditionalTotalIndex: TfcxSmallCount;
    FSecondAdditionalTotalIndex: TfcxSmallCount;
//    FBaseIndex, FSecondIndex: integer;
    FRow : integer;
    FCol : integer;
    FCurrentMeasureIndex: Integer;
    constructor Create;  overload; virtual;
    destructor Destroy; override;
    procedure ClearItems; virtual;
  published
    property Count: Integer read GetCount;
  end;

  TfcxCustomObject = class(TPersistent)
  private
    function GetValueIsNil: Boolean;
  protected
    FValue: Pointer;
  published
    property ValueIsNil: Boolean read GetValueIsNil;
  end;

function CreateInterpreter(AOwner: TComponent): IfcInterpreter;
function CreateDimensions(ASlice: TfcxSlice): TPersistent;
function CreateMeasures(ASlice: TfcxSlice): TPersistent;
function CreateCustomObject(ASlice: TfcxSlice): TPersistent;
{$IFDEF INCLUDE_FAST_SCRIPT}
procedure fcxAddRTTI(AScript: TfsScript);
{$ENDIF}

implementation


//VCL uses section
{$IFNDEF FMX}
uses
  fcxCodeUtils;
//FMX uses
{$ELSE FMX}
uses
  FMX.fcxCodeUtils;
{$ENDIF FMX}

type

  // script helper classes
  TScriptItems = class;

  TScriptItem = class(TPersistent)
  protected
    FOwner: TScriptItems;
    FSlice: TfcxSlice;
  published
    constructor Create(ASlice: TfcxSlice; AScriptItems: TScriptItems); virtual;
  end;

  TRegionFieldItem = class(TScriptItem)
  protected
    FField: TfcxCommonFieldOfRegion;
    function GetFieldName: String; virtual;
    function GetCaption: String; virtual;
    function GetCurrentCaption: String; virtual;
    function GetCurrentValue: Variant; virtual;
    function GetRegion: TfcxRegionOfField;
    function GetIndexInRegion: Integer;
  published
    constructor Create(ASlice: TfcxSlice; AScriptItems: TScriptItems; AField: TfcxCommonFieldOfRegion); reintroduce; virtual;

    property FieldName: String read GetFieldName;
    property Caption: String read GetCaption;
    property CurrentValue: Variant read GetCurrentValue;
    property Value: Variant read GetCurrentValue;
    property CurrentCaption: String read GetCurrentCaption;
  end;

  // class Dimension
  TDimension = class(TRegionFieldItem)
  private
    FLevelInfo: TfcxAxisLevelInfo;
    FSubGroup: TDimension;
    function GetSubGroup: TDimension;
  protected
    function GetCurrentCaption: String; override;
    function GetCurrentValue: Variant; override;
  public
  published
    constructor Create(ASlice: TfcxSlice; AScriptItems: TScriptItems; ALevelInfo: TfcxAxisLevelInfo); reintroduce; virtual;
    destructor Destroy; override;
    property SubGroup: TDimension read GetSubGroup;
  end;
  TfcxVarArr = array of Variant;
  // class Measure
  TMeasure = class(TRegionFieldItem)
  private
    function GetColOffsetValueWithDimValue(ADimValue: Variant): Variant;
    function GetRowOffsetValueWithDimValue(ADimValue: Variant): Variant;
    function GetTotalValueForDims(ADimNames: String): Variant;
    function GetColRowOffsetWLevelValue(ColOffset, RowOffset,
      ColLevelOffset, RowLevelOffset: integer): Variant;
    function GetValueWithColDimValues(ADimValues: TfcxVarArr): Variant;
    function GetValueWithRowDimValues(ADimValues: TfcxVarArr): Variant;
    function GetColOffsetTotalValueForDims(Offset: integer;
      ADimNames: String): Variant;
    function GetRowOffsetTotalValueForDims(Offset: integer;
      ADimNames: String): Variant;
    function GetColOffsetValue(Offset: integer): Variant;
    function GetRowOffsetValue(Offset: integer): Variant;
    function GetColRowOffsetValue(ColOffset, RowOffset: integer): Variant;
  protected
    function GetCurrentCaption: String; override;
    function GetCurrentValue: Variant; override;
  public
    property ColOffsetValue[Offset: integer]: Variant read GetColOffsetValue;
    property RowOffsetValue[Offset: integer]: Variant read GetRowOffsetValue;
    property ColRowOffsetValue[ColOffset, RowOffset: integer]: Variant read GetColRowOffsetValue;
    property ColRowOffsetValueWLevel[ColOffset, RowOffset, ColLevelOffset, RowLevelOffset: integer]: Variant read GetColRowOffsetWLevelValue;
    property ColOffsetValueWithDimValue[ADimValue: Variant]: Variant read GetColOffsetValueWithDimValue;
    property RowOffsetValueWithDimValue[ADimValue: Variant]: Variant read GetRowOffsetValueWithDimValue;
    property TotalValueForDims[ADimNames: String]: Variant read GetTotalValueForDims;
    property ColOffsetTotalValueForDims[Offset: integer; ADimNames: String]: Variant read GetColOffsetTotalValueForDims;
    property RowOffsetTotalValueForDims[Offset: integer; ADimNames: String]: Variant read GetRowOffsetTotalValueForDims;
  published
  end;

  TScriptItems = class(TCommonScriptItems)
  protected
    FSlice: TfcxSlice;
  public
    constructor Create(ASlice: TfcxSlice); overload; virtual;
  published
    property Row : integer read FRow;
    property Col : integer read FCol;
  end;

  TMeasures = class(TScriptItems)
  private
    function GetDetailValue(ARecordIndex: Integer; AFieldName: String): Variant;
    function GetRecordCount: integer;
    function GetCurrentMeasureName: String;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): TMeasure;
    function GetItemByCaption(AIndex: String): TMeasure;
    function GetItemByName(AIndex: String): TMeasure;
  public
    property Items[AIndex: Integer]: TMeasure read GetItem;
    property ItemByCaption[AIndex: String]: TMeasure read GetItemByCaption;
    property ItemByName[AIndex: String]: TMeasure read GetItemByName;
    property DetailValue[ARecordIndex: Integer; AFieldName: String]: Variant read GetDetailValue;
    property RecordCount: integer read GetRecordCount;
  published
    property XLevel: TfcxSmallCount read FXLevel;
    property YLevel: TfcxSmallCount read FYLevel;
    property CurrentMeasureIndex: Integer read FCurrentMeasureIndex;
    property CurrentMeasureName: String read GetCurrentMeasureName;
  end;

  TDimensions = class(TScriptItems)
  private
    function GetXAxisItem(AIndex: Integer): TDimension;
    function GetXAxisLevelsCount: TfcxSmallCount;
    function GetYAxisItem(AIndex: Integer): TDimension;
    function GetYAxisLevelsCount: TfcxSmallCount;
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): TDimension;
    function GetItemByCaption(AIndex: String): TDimension;
    function GetItemByName(AIndex: String): TDimension;
  public
    property Items[AIndex: Integer]: TDimension read GetItem;
    property ItemByCaption[AIndex: String]: TDimension read GetItemByCaption;
    property ItemByName[AIndex: String]: TDimension read GetItemByName;
    property XAxisItems[AIndex: Integer]: TDimension read GetXAxisItem;
    property YAxisItems[AIndex: Integer]: TDimension read GetYAxisItem;
  published
    property IsTotalByCol: boolean read FTotalOnX;
    property IsTotalByRow: boolean read FTotalOnY;
    property XLevel: TfcxSmallCount read FXLevel;
    property YLevel: TfcxSmallCount read FYLevel;
    property XAxisLevelsCount: TfcxSmallCount read GetXAxisLevelsCount;
    property YAxisLevelsCount: TfcxSmallCount read GetYAxisLevelsCount;
  end;

  // if there is no FastScript then FastCube can use this stub
  TfcxInterpreterStub = class(TInterfacedObject, IfcInterpreter)
  private
    FScript: TStringList;
    FScriptLanguage: String;
    FCompiled: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AddVariable(const Name, Typ: String; const Value: Variant);
    function CallFunction(const Func: Pointer; const Params: Variant): Variant;
    function Compile: Boolean;
    function GetErrorMsg: String;
    function GetCompiled: Boolean;
    function GetFunctionPointer(AFunctionName: String): Pointer;
    function GetObject: TObject;
    function GetScript: TStrings;
    procedure SetScript(const AScript: TStrings);
    function GetScriptLanguage: String;
    procedure SetScriptLanguage(const Value: String);
    procedure Clear;
    procedure EnumFunctions(const List: TStrings);
  end;

{$IFDEF INCLUDE_FAST_SCRIPT}
  // if FastScript is present then this implementation is used by FastCube
  // class registrator
  TfcxFunctionsFastCube2 = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetIndexProp(Instance: TObject; ClassType: TClass;
      const MethodName: String; var Params: Variant): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
    procedure SetProp(Instance: TObject; ClassType: TClass;
      const PropName: String; Value: Variant);
  public
    procedure AddClasses(AScript: TfsScript);
    constructor Create(AScript: TfsScript); override;
  end;

  procedure fcxAddRTTI(AScript: TfsScript);
  var
    rtti: TfsRTTIModule;
  begin
    AScript.AddRTTI;
    AScript.AddedBy := TObject(1); // do not clear
    rtti := TfsRTTIModule(TfcxFunctionsFastCube2.NewInstance);
    rtti.Create(AScript);
    TfcxFunctionsFastCube2(rtti).AddClasses(AScript);
    AScript.Add('', rtti);
    AScript.AddedBy := nil;
  end;

type
  TfcxFSInterpreter = class(TInterfacedObject, IfcInterpreter)
  private
    FOwner: TComponent;
    FCommonInterpreter: TfsScript;
    FInterpreter: TfsScript;
    FCompiled: Boolean;
    FCommonScriptChangedTick: Cardinal;
    FScript: TfcxScriptStringList;
    function GetScriptChanged: boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure AddVariable(const Name, Typ: String; const Value: Variant);
    function CallFunction(const Func: Pointer; const Params: Variant): Variant;
    function Compile: Boolean;
    function GetErrorMsg: String;
    function GetCompiled: Boolean;
    function GetFunctionPointer(AFunctionName: String): Pointer;
    function GetObject: TObject;
    function GetScript: TStrings;
    procedure SetScript(const AScript: TStrings);
    function GetScriptLanguage: String;
    procedure SetScriptLanguage(const Value: String);
    procedure Clear;
    procedure EnumFunctions(const List: TStrings);
    property ScriptChanged: boolean read GetScriptChanged;
  end;

{ TfcxFunctionsFastCube2 }

procedure TfcxFunctionsFastCube2.AddClasses(AScript: TfsScript);
begin
  with AScript do
  begin
    with AddClass(TDimension, 'TPersistent') do
    begin
    end;
    AddEnum('TfcxRegionOfField', 'rf_Page, rf_CapXAx, rf_CapYAx, rf_CapFacts, rf_None');
    with AddClass(TDimensions, 'TPersistent') do
    begin
      AddDefaultProperty('ItemByName', 'String', 'TDimension', GetIndexProp, True);
      AddIndexProperty('ItemByCaption', 'String', 'TDimension', GetIndexProp, True);
      AddIndexProperty('Items', 'Integer', 'TDimension', GetIndexProp, True);
      AddIndexProperty('XAxisItems', 'Integer', 'TDimension', GetIndexProp, True);
      AddIndexProperty('YAxisItems', 'Integer', 'TDimension', GetIndexProp, True);
    end;

    with AddClass(TMeasure, 'TPersistent') do
    begin
      AddIndexProperty('ColOffsetValue', 'Integer', 'Variant', GetIndexProp, True);
      AddIndexProperty('RowOffsetValue', 'Integer', 'Variant', GetIndexProp, True);
      AddIndexProperty('ColOffsetValueWithDimValue', 'Variant', 'Variant', GetIndexProp, True);
      AddIndexProperty('RowOffsetValueWithDimValue', 'Variant', 'Variant', GetIndexProp, True);
      AddIndexProperty('TotalValueForDims', 'String', 'Variant', GetIndexProp, True);
      AddIndexProperty('ColOffsetTotalValueForDims', 'Integer, String', 'Variant', GetIndexProp, True);
      AddIndexProperty('RowOffsetTotalValueForDims', 'Integer, String', 'Variant', GetIndexProp, True);
      AddIndexProperty('ColRowOffsetValue', 'Integer, Integer', 'Variant', GetIndexProp, True);
      AddIndexProperty('ColRowOffsetValueWLevel', 'Integer, Integer, Integer, Integer', 'Variant', GetIndexProp, True);
      AddMethod('function GetValueWithRowDimValues(ADimValues: Variant): Variant', CallMethod);
      AddMethod('function GetValueWithColDimValues(ADimValues: Variant): Variant', CallMethod);
    end;

    with AddClass(TMeasures, 'TPersistent') do
    begin
      AddDefaultProperty('ItemByName', 'String', 'TMeasure', GetIndexProp, True);
      AddIndexProperty('ItemByCaption', 'String', 'TMeasure', GetIndexProp, True);
      AddIndexProperty('Items', 'Integer', 'TMeasure', GetIndexProp, True);
      AddIndexProperty('DetailValue', 'Integer, String', 'Variant', GetIndexProp, True);
      AddProperty('RecordCount', 'Integer', GetProp, nil);
// for compatibility
      AddMethod('procedure PrepareDetailInfo', CallMethod);
    end;

    with AddClass(TfcxSliceField, 'TPersistent') do
    begin
      AddProperty('FilterCount', 'Integer', GetProp, nil);
      AddProperty('IsFiltered', 'Boolean', GetProp, nil);
    end;

    with AddClass(TfcxSliceFields, 'TPersistent') do
    begin
      AddDefaultProperty('ItemByName', 'String', 'TfcxSliceField', GetIndexProp, True);
      AddIndexProperty('ItemByCaption', 'String', 'TfcxSliceField', GetIndexProp, True);
      AddIndexProperty('Items', 'Integer', 'TfcxSliceField', GetIndexProp, True);
    end;

    with AddClass(TfcxCustomObject, 'TPersistent') do
    begin
      AddProperty('Value', 'Pointer', GetProp, SetProp);
    end;
  end;
end;

function TfcxFunctionsFastCube2.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;
  if ClassType = TMeasures then
  begin
    if MethodName = 'PREPAREDETAILINFO' then
// for compatibility
      ;
  end
  else
  if ClassType = TMeasure then
  begin
    if MethodName = 'GETVALUEWITHROWDIMVALUES' then
      Result := TMeasure(Instance).GetValueWithRowDimValues(Caller.Params[0])
    else
    if MethodName = 'GETVALUEWITHCOLDIMVALUES' then
      Result := TMeasure(Instance).GetValueWithColDimValues(Caller.Params[0])
      ;
  end;
end;

constructor TfcxFunctionsFastCube2.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
end;

{ for future
function TfcxFunctionsFastCube2.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  // no need in mean time - for future use only
  Result := 0;
  if ClassType = TDimension then
  begin
  end
  else if ClassType = TMeasure then
  begin
  end;
end;
}

function TfcxFunctionsFastCube2.GetIndexProp(Instance: TObject; ClassType: TClass;
  const MethodName: String; var Params: Variant): Variant;
begin
  Result := 0;

  if ClassType = TDimensions then
  begin
    if MethodName = 'ITEMBYNAME.GET' then
      Result := PtrUInt(TDimensions(Instance).ItemByName[Params[0]]) else
    if MethodName = 'ITEMBYCAPTION.GET' then
      Result := PtrUInt(TDimensions(Instance).ItemByCaption[Params[0]]) else
    if MethodName = 'ITEMS.GET' then
      Result := PtrUInt(TDimensions(Instance).Items[Params[0]]) else
    if MethodName = 'XAXISITEMS.GET' then
      Result := PtrUInt(TDimensions(Instance).XAxisItems[Params[0]]) else
    if MethodName = 'YAXISITEMS.GET' then
      Result := PtrUInt(TDimensions(Instance).YAxisItems[Params[0]])
  end else
  if ClassType = TMeasures then
  begin
    if MethodName = 'ITEMBYNAME.GET' then
      Result := PtrUInt(TMeasures(Instance).ItemByName[Params[0]]) else
    if MethodName = 'ITEMBYCAPTION.GET' then
      Result := PtrUInt(TMeasures(Instance).ItemByCaption[Params[0]]) else
    if MethodName = 'ITEMS.GET' then
      Result := PtrUInt(TMeasures(Instance).Items[Params[0]]) else
    if MethodName = 'DETAILVALUE.GET' then
      Result := TMeasures(Instance).DetailValue[Params[0], Params[1]]
  end
  else
  if ClassType = TMeasure then
  begin
    if MethodName = 'COLOFFSETVALUE.GET' then
      Result := TMeasure(Instance).ColOffsetValue[Params[0]] else
    if MethodName = 'ROWOFFSETVALUE.GET' then
      Result := TMeasure(Instance).RowOffsetValue[Params[0]] else
    if MethodName = 'COLOFFSETVALUEWITHDIMVALUE.GET' then
      Result := TMeasure(Instance).ColOffsetValueWithDimValue[Params[0]] else
    if MethodName = 'ROWOFFSETVALUEWITHDIMVALUE.GET' then
      Result := TMeasure(Instance).RowOffsetValueWithDimValue[Params[0]] else
    if MethodName = 'TOTALVALUEFORDIMS.GET' then
      Result := TMeasure(Instance).TotalValueForDims[Params[0]] else
    if MethodName = 'COLOFFSETTOTALVALUEFORDIMS.GET' then
      Result := TMeasure(Instance).ColOffsetTotalValueForDims[Params[0], Params[1]] else
    if MethodName = 'ROWOFFSETTOTALVALUEFORDIMS.GET' then
      Result := TMeasure(Instance).RowOffsetTotalValueForDims[Params[0], Params[1]] else
    if MethodName = 'COLROWOFFSETVALUE.GET' then
      Result := TMeasure(Instance).ColRowOffsetValue[Params[0], Params[1]] else
    if MethodName = 'COLROWOFFSETVALUEWLEVEL.GET' then
      Result := TMeasure(Instance).ColRowOffsetValueWLevel[Params[0], Params[1], Params[2], Params[3]]
  end
  else
  if ClassType = TfcxSliceFields then
  begin
    if MethodName = 'ITEMBYNAME.GET' then
      Result := PtrUInt(TfcxSliceFields(Instance).ItemByName[Params[0]]) else
    if MethodName = 'ITEMBYCAPTION.GET' then
      Result := PtrUInt(TfcxSliceFields(Instance).ItemByCaption[Params[0]]) else
    if MethodName = 'ITEMS.GET' then
      Result := PtrUInt(TfcxSliceFields(Instance).Items[Params[0]])
  end
end;

function TfcxFunctionsFastCube2.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TMeasures then
  begin
    if PropName = 'RECORDCOUNT' then
      Result := TMeasures(Instance).RecordCount
  end
  else
  if ClassType = TfcxCustomObject then
  begin
    if PropName = 'VALUE' then
      Result := PtrUInt(TfcxCustomObject(Instance).FValue);
  end
  else
  if ClassType = TfcxSliceField then
  begin
    if PropName = 'FILTERCOUNT' then
      Result := TfcxSliceField(Instance).UVFilteredValuesCount
    else if PropName = 'ISFILTERED' then
      Result := TfcxSliceField(Instance).UVFilteredValuesCount <> 0
  end
end;

procedure TfcxFunctionsFastCube2.SetProp(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
  if ClassType = TfcxCustomObject then
  begin
    if PropName = 'VALUE' then
      TfcxCustomObject(Instance).FValue := Pointer(PtrUInt(Value));
  end
end;

{ TfcxFSInterpreter }

constructor TfcxFSInterpreter.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FCompiled := False;
  FCommonInterpreter := TfsScript.Create(nil);
//  FCommonInterpreter.Parent := fsGlobalUnit;
  fcxAddRTTI(FCommonInterpreter);
  FScript := TfcxScriptStringList.Create;
  FInterpreter := TfsScript.Create(nil);
  FInterpreter.SyntaxType := 'PascalScript';
  fcxAddRTTI(FInterpreter);
  FCommonScriptChangedTick := 0;
  fcxEmptyCode(FScript, GetScriptLanguage);
end;

procedure TfcxFSInterpreter.AddVariable(const Name, Typ: String; const Value: Variant);
begin
  FInterpreter.AddVariable(Name, Typ, Value);
end;

function TfcxFSInterpreter.CallFunction(const Func: Pointer; const Params: Variant): Variant;
begin
  if Assigned(Func) then
  begin
    FInterpreter.CallFunction2(Func, Params);
    Result := TfsProcVariable(Func).Params[0].Value;
  end
  else
    Result := UnAssigned;
end;

function TfcxFSInterpreter.Compile: Boolean;
begin
  Result := False;
  if FOwner is TfcxComponent then
  begin
    if FCompiled and not FScript.IsChanged and (FCommonScriptChangedTick = TfcxComponent(FOwner).GetCommonScriptChangedTick) then
// не надо повторно компилировать
      Result := True
    else
    begin
      FInterpreter.Clear;
      FInterpreter.Lines.Assign(FScript);
// подтащить общий скрипт
      if TfcxComponent(FOwner).CommonScript <> nil then
      begin
        if TfcxComponent(FOwner).CommonScriptLanguage = '' then
          FCommonInterpreter.SyntaxType := 'PascalScript'
        else
          FCommonInterpreter.SyntaxType := TfcxComponent(FOwner).CommonScriptLanguage;
        FCommonInterpreter.Lines.Assign(TfcxComponent(FOwner).CommonScript);
        FCommonScriptChangedTick := TfcxComponent(FOwner).GetCommonScriptChangedTick;
        FInterpreter.Parent := FCommonInterpreter;
        Result := FCommonInterpreter.Compile;
      end
      else
      begin
        FInterpreter.Parent := fsGlobalUnit;
        FCommonScriptChangedTick := 0;
        Result := True;
      end;
// вызов события для подготовительных операций
      if Result then
      begin
        TfcxComponent(FOwner).InitInterpreter;
        Result := FInterpreter.Compile;
      end;
      if Result then
      begin
        FCompiled := True;
        FScript.IsChanged := False;
      end
      else
      begin
// нужно записать информацию об ошибке?
      end
    end;
  end
end;

function TfcxFSInterpreter.GetErrorMsg: String;
begin
  Result := FInterpreter.ErrorMsg;
end;

function TfcxFSInterpreter.GetFunctionPointer(AFunctionName: String): Pointer;
begin
  Result := TfsProcVariable(FInterpreter.FindLocal(AFunctionName));
end;

function TfcxFSInterpreter.GetObject: TObject;
begin
  Result := FInterpreter;
end;

function TfcxFSInterpreter.GetScript: TStrings;
begin
  Result := FScript;
end;

procedure TfcxFSInterpreter.SetScript(const AScript: TStrings);
begin
  FScript.Assign(AScript);
end;

procedure TfcxFSInterpreter.Clear;
begin
  FScript.Clear;
  FCompiled := False;
end;

destructor TfcxFSInterpreter.Destroy;
begin
  FInterpreter.Free;
  FCommonInterpreter.Free;
  FScript.Free;
  inherited;
end;

function TfcxFSInterpreter.GetCompiled: Boolean;
begin
  Result := FCompiled;
end;

function TfcxFSInterpreter.GetScriptChanged: boolean;
begin
  Result := FScript.IsChanged;
end;

procedure TfcxFSInterpreter.EnumFunctions(const List: TStrings);
var
  i: Integer;
  ProcVar: TfsProcVariable;
begin
  if Compile then
  begin
    for i := 0 to FInterpreter.Count - 1 do
      if FInterpreter.Items[i] is TfsProcVariable then
      begin
        ProcVar := TfsProcVariable(FInterpreter.Items[i]);
        if (ProcVar.Count = 0) and ProcVar.IsFunc and (ProcVar.Typ = fvtVariant) then
          List.Add(ProcVar.Name);
      end;
  end;
end;

function TfcxFSInterpreter.GetScriptLanguage: String;
begin
  Result := FInterpreter.SyntaxType;
end;

procedure TfcxFSInterpreter.SetScriptLanguage(const Value: String);
begin
  if Value = '' then
    FInterpreter.SyntaxType := 'PascalScript'
  else
    FInterpreter.SyntaxType := Value;
end;

{$ENDIF}

function CreateInterpreter(AOwner: TComponent): IfcInterpreter;
begin
{$IFDEF INCLUDE_FAST_SCRIPT}
  Result := TfcxFSInterpreter.Create(AOwner);
{$ELSE}
  Result := TfcxInterpreterStub.Create(AOwner);
{$ENDIF}
end;

{ TfcxInterpreterStub }

procedure TfcxInterpreterStub.AddVariable(const Name, Typ: String;
  const Value: Variant);
begin
end;

function TfcxInterpreterStub.CallFunction(const Func: Pointer; const Params: Variant): Variant;
begin
  Result := Unassigned;
end;

procedure TfcxInterpreterStub.Clear;
begin
  FScript.Clear;
end;

function TfcxInterpreterStub.Compile: Boolean;
begin
  Result := False;
end;

constructor TfcxInterpreterStub.Create(AOwner: TComponent);
begin
  FCompiled := True;
  FScriptLanguage := 'PascalScript';
  FScript := TStringList.Create;
end;

destructor TfcxInterpreterStub.Destroy;
begin
  FScript.Free;
  inherited;
end;

procedure TfcxInterpreterStub.EnumFunctions(const List: TStrings);
begin
end;

function TfcxInterpreterStub.GetCompiled: Boolean;
begin
  Result := FCompiled;
end;

function TfcxInterpreterStub.GetErrorMsg: String;
begin
  Result := '';
end;

function TfcxInterpreterStub.GetFunctionPointer(AFunctionName: String): Pointer;
begin
  Result := nil;
end;

function TfcxInterpreterStub.GetObject: TObject;
begin
  Result := nil;
end;

function TfcxInterpreterStub.GetScript: TStrings;
begin
  Result := FScript;
end;

function TfcxInterpreterStub.GetScriptLanguage: String;
begin
  Result := FScriptLanguage;
end;

procedure TfcxInterpreterStub.SetScript(const AScript: TStrings);
begin
  FScript.Assign(AScript);
end;

procedure TfcxInterpreterStub.SetScriptLanguage(const Value: String);
begin
  FScriptLanguage := Value;
end;

{ TScriptItem }

constructor TScriptItem.Create(ASlice: TfcxSlice; AScriptItems: TScriptItems);
begin
  FSlice := ASlice;
  FOwner := AScriptItems;
end;

{ TRegionFieldItem }

constructor TRegionFieldItem.Create(ASlice: TfcxSlice; AScriptItems: TScriptItems; AField: TfcxCommonFieldOfRegion);
begin
  inherited Create(ASlice, AScriptItems);
  FField := AField;
end;

function TRegionFieldItem.GetFieldName: String;
begin
  if (FField <> nil) then
    Result := FField.Name;
end;

function TRegionFieldItem.GetCaption: String;
begin
  if (FField <> nil) then
    Result := FField.Caption;
end;

function TRegionFieldItem.GetCurrentCaption: String;
begin
  Result := '';
end;

function TRegionFieldItem.GetCurrentValue: Variant;
begin
  Result := UnAssigned;
end;

function TRegionFieldItem.GetIndexInRegion: Integer;
begin
  Result := FField.Index;
end;

function TRegionFieldItem.GetRegion: TfcxRegionOfField;
begin
  Result := FField.Owner.Container.Region;
end;

{ TMeasure }

function TMeasure.GetColOffsetTotalValueForDims(Offset: integer;
  ADimNames: String): Variant;
begin
  if FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureTotalValueForDimsBSOffset(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex, ADimNames,
      FOwner.FBaseLevel, FOwner.FSecondLevel, Offset, 0, True)
  else
    Result := FSlice.GetMeasureTotalValueForDimsBSOffset(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex, ADimNames,
      FOwner.FBaseLevel, FOwner.FSecondLevel, 0, Offset, True)
end;

function TMeasure.GetColOffsetValue(Offset: integer): Variant;
begin
  if FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSWOffsetThrough(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      Offset, 0)
  else
    Result := FSlice.GetMeasureValueBSWOffsetThrough(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      0, Offset)
end;

function TMeasure.GetColOffsetValueWithDimValue(ADimValue: Variant): Variant;
begin
  if FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSWOffsetOnValue(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      FOwner.FBaseLevel, FOwner.FSecondLevel, ADimValue, 0)
  else
    Result := FSlice.GetMeasureValueBSWOffsetOnValue(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      FOwner.FBaseLevel, FOwner.FSecondLevel, 0, ADimValue)
end;

function TMeasure.GetColRowOffsetValue(ColOffset,
  RowOffset: integer): Variant;
begin
  if FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSWOffsetThrough(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      ColOffset, RowOffset)
  else
    Result := FSlice.GetMeasureValueBSWOffsetThrough(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      RowOffset, ColOffset)
end;

function TMeasure.GetColRowOffsetWLevelValue(ColOffset, RowOffset,
  ColLevelOffset, RowLevelOffset: integer): Variant;
begin
  if FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSWOffset(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      ColLevelOffset, RowLevelOffset, ColOffset, RowOffset)
  else
    Result := FSlice.GetMeasureValueBSWOffset(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      RowLevelOffset, ColLevelOffset, RowOffset, ColOffset)
end;

function TMeasure.GetCurrentCaption: String;
begin
  Result := FSlice.GetMeasureValueCaptionBS(FOwner.FBaseLevel, FOwner.FSecondLevel,
    FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
    FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex)
end;

function TMeasure.GetCurrentValue: Variant;
begin
  Result := FSlice.GetMeasureValueBS(FOwner.FBaseLevel, FOwner.FSecondLevel,
    FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
    FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex)
end;

function TMeasure.GetRowOffsetTotalValueForDims(Offset: integer;
  ADimNames: String): Variant;
begin
  if not FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureTotalValueForDimsBSOffset(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex, ADimNames,
      FOwner.FBaseLevel, FOwner.FSecondLevel, Offset, 0, True)
  else
    Result := FSlice.GetMeasureTotalValueForDimsBSOffset(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex, ADimNames,
      FOwner.FBaseLevel, FOwner.FSecondLevel, 0, Offset, True)
end;

function TMeasure.GetRowOffsetValue(Offset: integer): Variant;
begin
  if not FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSWOffsetThrough(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      Offset, 0)
  else
    Result := FSlice.GetMeasureValueBSWOffsetThrough(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      0, Offset)
end;

function TMeasure.GetRowOffsetValueWithDimValue(ADimValue: Variant): Variant;
begin
  if not FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSWOffsetOnValue(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      FOwner.FBaseLevel, FOwner.FSecondLevel, ADimValue, 0)
  else
    Result := FSlice.GetMeasureValueBSWOffsetOnValue(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      FOwner.FBaseLevel, FOwner.FSecondLevel, 0, ADimValue)
end;

function TMeasure.GetTotalValueForDims(ADimNames: String): Variant;
begin
  Result := FSlice.GetMeasureTotalValueForDimsBS(FOwner.FBaseLevel, FOwner.FSecondLevel,
    FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
    FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex, ADimNames)
end;

function TMeasure.GetValueWithColDimValues(
  ADimValues: TfcxVarArr): Variant;
begin
  if FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSOnWayValues(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      True, False, ADimValues, [])
  else
    Result := FSlice.GetMeasureValueBSOnWayValues(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      False, True, [], ADimValues)
end;

function TMeasure.GetValueWithRowDimValues(
  ADimValues: TfcxVarArr): Variant;
begin
  if not FSlice.MeasuresContainer.BaseAxisIsX then
    Result := FSlice.GetMeasureValueBSOnWayValues(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      True, False, ADimValues, [])
  else
    Result := FSlice.GetMeasureValueBSOnWayValues(FOwner.FBaseLevel, FOwner.FSecondLevel,
      FOwner.FIndexInBaseLevel, FOwner.FIndexInSecondLevel, FField.Index,
      FOwner.FBaseAdditionalTotalIndex, FOwner.FSecondAdditionalTotalIndex,
      False, True, [], ADimValues)
end;

{ TDimension }

constructor TDimension.Create(ASlice: TfcxSlice;
  AScriptItems: TScriptItems; ALevelInfo: TfcxAxisLevelInfo);
begin
  inherited Create(ASlice, AScriptItems, ALevelInfo.RegionField);
  FLevelInfo := ALevelInfo;
  FSubGroup := Nil;
end;

destructor TDimension.Destroy;
begin
  FSubGroup.Free;
  inherited;
end;

function TDimension.GetCurrentCaption: String;
begin
  Result := '';
  case GetRegion of
    rf_CapXAx: Result := FSlice.GetXAxisDimCaptionLI(FOwner.FXLevel, FOwner.FIndexInXLevel, FLevelInfo.Level);
    rf_CapYAx: Result := FSlice.GetYAxisDimCaptionLI(FOwner.FYLevel, FOwner.FIndexInYLevel, FLevelInfo.Level);
  end;
end;

function TDimension.GetCurrentValue: Variant;
begin
  Result := Null;
  case GetRegion of
    rf_CapXAx: Result := FSlice.GetXAxisDimValueLI(FOwner.FXLevel, FOwner.FIndexInXLevel, FLevelInfo.Level);
    rf_CapYAx: Result := FSlice.GetYAxisDimValueLI(FOwner.FYLevel, FOwner.FIndexInYLevel, FLevelInfo.Level);
  end;
end;

function TDimension.GetSubGroup: TDimension;
begin
  if FLevelInfo.LevelType = fcATLT_HasGroup then
  begin
    if FSubGroup = nil then
    begin
      case GetRegion of
        rf_CapXAx:
            if (FSlice.XAxisContainer.LevelInfoWOMeasures[FLevelInfo.Level+1].RegionField = FField) then
              FSubGroup := TDimension.Create(FSlice, FOwner, FSlice.XAxisContainer.LevelInfoWOMeasures[FLevelInfo.Level+1]);
        rf_CapYAx:
            if (FSlice.YAxisContainer.LevelInfoWOMeasures[FLevelInfo.Level+1].RegionField = FField) then
              FSubGroup := TDimension.Create(FSlice, FOwner, FSlice.YAxisContainer.LevelInfoWOMeasures[FLevelInfo.Level+1])
      end;
    end;
    Result := FSubGroup
  end
  else
    Result := nil;
end;

{ TScriptItems }

constructor TScriptItems.Create(ASlice: TfcxSlice);
begin
  inherited Create;
  FSlice := ASlice;
end;

{ TMeasures }

function TMeasures.GetCount: Integer;
begin
  Result := FSlice.MeasuresContainer.Count;
end;

function TMeasures.GetCurrentMeasureName: String;
begin
  Result := FSlice.MeasuresContainer.Measures[FCurrentMeasureIndex].Name
end;

function TMeasures.GetDetailValue(ARecordIndex: Integer; AFieldName: String): Variant;
var
  AFieldIndex: integer;
begin
  if ARecordIndex < FSlice.MeasuresContainer.DetailRecordsCount[FBaseLevel, FSecondLevel, FIndexInBaseLevel, FIndexInSecondLevel] then
  begin
      AFieldIndex := FSlice.SliceFields.Order[FSlice.SliceFields.IndexOfField(AFieldName)];
      if AFieldIndex <> -1 then
        Result := FSlice.SliceFields[AFieldIndex].GetUVValueFromRec(FSlice.MeasuresContainer.DetailRecords[FBaseLevel, FSecondLevel, FIndexInBaseLevel, FIndexInSecondLevel, ARecordIndex])
      else
        Result := Unassigned;
  end
  else
    Result := Unassigned;
end;

function TMeasures.GetItem(AIndex: Integer): TMeasure;
begin
  if AIndex < Count then
    Result := GetItemByName(FSlice.MeasuresContainer.Measures[AIndex].Name)
  else
    Result := nil;
end;

function TMeasures.GetItemByCaption(AIndex: String): TMeasure;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSlice.MeasuresContainer.Count - 1 do
    if AnsiCompareText(FSlice.MeasuresContainer.Measures[i].Caption, AIndex) = 0 then
    begin
      Result := GetItemByName(FSlice.MeasuresContainer.Measures[i].Name);
      break;
    end;
end;

function TMeasures.GetItemByName(AIndex: String): TMeasure;
var
  i, idx: Integer;
  Measure: TMeasure;
begin
  idx := IndexOf(AIndex);
  if idx = - 1 then
  begin
    for i := 0 to FSlice.MeasuresContainer.Count - 1 do
      if AnsiCompareText(FSlice.MeasuresContainer.Measures[i].Name, AIndex) = 0 then
      begin
        Measure := TMeasure.Create(FSlice, Self, FSlice.MeasuresContainer.Measures[i]);
        idx := FItems.AddObject(FSlice.MeasuresContainer.Measures[i].Name, Measure);
        break;
      end;
  end;
  if idx = - 1 then
    Result := nil
  else
    Result := TMeasure(FItems.Objects[idx]);
end;

function TMeasures.GetRecordCount: integer;
begin
  Result := FSlice.MeasuresContainer.DetailRecordsCount[FBaseLevel, FSecondLevel, FIndexInBaseLevel, FIndexInSecondLevel];
end;

{ TDimensions }

function TDimensions.GetCount: Integer;
begin
  Result :=
    FSlice.FieldsOfRegion[rf_Page].Count +
    FSlice.YAxisContainer.LevelCount +
    FSlice.XAxisContainer.LevelCount;
end;

function TDimensions.GetItem(AIndex: Integer): TDimension;

  function CheckHere(AFields: TfcxCommonFieldsOfRegion; var Index: Integer): String;
  begin
    AIndex := AIndex - AFields.Count;
    if AIndex < 0 then
      Result := AFields.Items[AIndex + AFields.Count].Name
    else
      Result := '';
  end;
  function CheckHere2(AAxis: TfcxAxisContainer; var Index: Integer): String;
  begin
    AIndex := AIndex - AAxis.LevelCount;
    if AIndex < 0 then
      Result := AAxis.LevelInfoWOMeasures[AIndex + AAxis.LevelCount].RegionField.Name
    else
      Result := '';
  end;

var
  AName: String;
begin
  Result := nil;
  if AIndex < Count then
  begin
    AName := CheckHere(FSlice.FieldsOfRegion[rf_Page], AIndex);
    if AName = '' then
      AName := CheckHere2(FSlice.YAxisContainer, AIndex);
    if AName = '' then
      AName := CheckHere2(FSlice.XAxisContainer, AIndex);
    if AName <> '' then
      Result := GetItemByName(AName);
  end;
end;

function TDimensions.GetItemByCaption(AIndex: String): TDimension;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSlice.FieldsOfRegion[rf_Page].Count - 1 do
    if AnsiCompareText(FSlice.FieldsOfRegion[rf_Page].Items[i].Caption, AIndex) = 0 then
    begin
      Result := GetItemByName(FSlice.FieldsOfRegion[rf_Page].Items[i].Name);
      Exit;
    end;
  with FSlice.YAxisContainer do
    for i := 0 to LevelCount - 1 do
      if AnsiCompareText(LevelInfoWOMeasures[i].RegionField.Caption, AIndex) = 0 then
      begin
        Result := GetItemByName(LevelInfoWOMeasures[i].RegionField.Name);
        Exit;
      end;
  with FSlice.XAxisContainer do
    for i := 0 to LevelCount - 1 do
      if AnsiCompareText(LevelInfoWOMeasures[i].RegionField.Caption, AIndex) = 0 then
      begin
        Result := GetItemByName(LevelInfoWOMeasures[i].RegionField.Name);
        Exit;
      end;
end;

function TDimensions.GetItemByName(AIndex: String): TDimension;
var
  i, idx: Integer;
  Dimension: TDimension;
  ALevelInfo: TfcxAxisLevelInfo;
begin
  ALevelInfo.IsMeasure := False;
  ALevelInfo.IsVisible := True;
  ALevelInfo.LevelType := fcATLT_Simple;
  ALevelInfo.RegionField := nil;
  idx := IndexOf(AIndex);
  if idx = - 1 then
  begin
    for i := 0 to FSlice.FieldsOfRegion[rf_Page].Count - 1 do
      if AnsiCompareText(FSlice.FieldsOfRegion[rf_Page].Items[i].Name, AIndex) = 0 then
      begin
        ALevelInfo.RegionField := TfcxAxisField(FSlice.FieldsOfRegion[rf_Page].Items[i]);
        Dimension := TDimension.Create(FSlice, Self, ALevelInfo);
        idx := FItems.AddObject(AIndex, Dimension);
        Break;
      end;
  end;
  with FSlice.YAxisContainer do
    if idx = - 1 then
    begin
      for i := 0 to LevelCount - 1 do
        if AnsiCompareText(LevelInfoWOMeasures[i].RegionField.Name, AIndex) = 0 then
        begin
          Dimension := TDimension.Create(FSlice, Self, LevelInfoWOMeasures[i]);
          idx := FItems.AddObject(AIndex, Dimension);
          Break;
        end;
    end;
  with FSlice.XAxisContainer do
    if idx = - 1 then
    begin
      for i := 0 to LevelCount - 1 do
        if AnsiCompareText(LevelInfoWOMeasures[i].RegionField.Name, AIndex) = 0 then
        begin
          Dimension := TDimension.Create(FSlice, Self, LevelInfoWOMeasures[i]);
          idx := FItems.AddObject(AIndex, Dimension);
          Break;
        end;
    end;
  if idx = - 1 then
    Result := nil
  else
    Result := TDimension(FItems.Objects[idx]);
end;

function CreateDimensions(ASlice: TfcxSlice): TPersistent;
begin
  Result := TDimensions.Create(ASlice);
end;

function CreateMeasures(ASlice: TfcxSlice): TPersistent;
begin
  Result := TMeasures.Create(ASlice);
end;

function CreateCustomObject(ASlice: TfcxSlice): TPersistent;
begin
  Result := TfcxCustomObject.Create;
end;

function TDimensions.GetXAxisItem(AIndex: Integer): TDimension;
begin
  Result := GetItemByName(FSlice.XAxisContainer.LevelInfoWOMeasures[AIndex].RegionField.Name);
  if (Result <> nil) and (Result.FLevelInfo.Level <> AIndex) then
    Result := Result.SubGroup;
end;

function TDimensions.GetXAxisLevelsCount: TfcxSmallCount;
begin
  Result := FSlice.XAxisContainer.LevelCount;
end;

function TDimensions.GetYAxisItem(AIndex: Integer): TDimension;
begin
  Result := GetItemByName(FSlice.YAxisContainer.LevelInfoWOMeasures[AIndex].RegionField.Name);
  if (Result <> nil) and (Result.FLevelInfo.Level <> AIndex) then
    Result := Result.SubGroup;
end;

function TDimensions.GetYAxisLevelsCount: TfcxSmallCount;
begin
  Result := FSlice.YAxisContainer.LevelCount;
end;

{ TCommonScriptItems }

procedure TCommonScriptItems.ClearItems;
var
  i: integer;
begin
  for i := FItems.Count - 1 downto 0 do
  begin
    FItems.Objects[i].Free;
    FItems.Delete(i);
  end;
end;

constructor TCommonScriptItems.Create;
begin
  FItems := TStringList.Create;
end;

destructor TCommonScriptItems.Destroy;
begin
  ClearItems;
  FItems.Free;
  inherited;
end;

function TCommonScriptItems.GetCount: Integer;
begin
  Result := 0;
end;

function TCommonScriptItems.IndexOf(AString: String): Integer;
begin
  for Result := 0 to FItems.Count - 1 do
    if AnsiCompareText(FItems[Result], AString) = 0 then Exit;
  Result := -1;
end;

{ TfcxCustomObject }

function TfcxCustomObject.GetValueIsNil: Boolean;
begin
  Result := not Assigned(FValue);
end;

initialization
{$IFDEF INCLUDE_FAST_SCRIPT}
  fsRTTIModules.Add(TfcxFunctionsFastCube2);
{$ENDIF}
  fcxAddCodeRes;

finalization
{$IFDEF INCLUDE_FAST_SCRIPT}
  if fsRTTIModules <> nil then
    fsRTTIModules.Remove(TfcxFunctionsFastCube2);
{$ENDIF}

end.
