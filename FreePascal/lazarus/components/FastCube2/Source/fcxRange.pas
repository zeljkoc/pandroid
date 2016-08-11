{*******************************************************}
{                                                       }
{            FastCube 2 Range configuration unit        }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}
//VCL uses section
{$IFNDEF FMX}
unit fcxRange;

interface

{$include fcx.inc}

uses
  SysUtils, Classes, fcxXML
{$IFDEF DELPHI_6UP}
  , Variants
{$ENDIF};
//FMX uses
{$ELSE FMX}
interface

{$include fcx.inc}

uses
  System.SysUtils, System.Classes, System.Variants, 
  FMX.fcxXML;
{$ENDIF FMX}

type
  TfcxRangeValueCondition = (
    rvcBetween,
    rvcBeyond,
    rvcEqual,
    rvcNotEqual,
    rvcGreater,
    rvcLess,
    rvcGreaterOrEqual,
    rvcLessOrEqual
  );

  TfcxRangeTextCondition = (
    rtcContains,
    rtcNotContains,
    rtcStartsWith,
    rtcEndsWith
  );

  TfcxRangeCompareObject = (
    rcoValue,
    rcoText,
    rcoDate,
    rcoNull,
    rcoNotNull
  );

  TfcxRangeBinaryOperator = (rboAnd, rboOr);

  TfcxRange = class(TCollectionItem)
  private
    FHighRange: Double;
    FLowRange: Double;
    FValueCondition: TfcxRangeValueCondition;
    FCompareObject: TfcxRangeCompareObject;
    FTextCondition: TfcxRangeTextCondition;
    FText: String;
    FOnChange: TNotifyEvent;
    FUpdateCount: Integer;
    FOperator: TfcxRangeBinaryOperator; // this is needed for ranges only
    procedure SetHighRange(const Value: Double);
    procedure SetLowRange(const Value: Double);
    procedure SetValueCondition(const Value: TfcxRangeValueCondition);
    procedure SetCompareObject(const Value: TfcxRangeCompareObject);
    procedure SetTextCondition(const Value: TfcxRangeTextCondition);
    procedure SetText(const Value: String);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChange;
  public
    constructor Create(Collection: TCollection); override;
    function AsString: String;
    function Match(AValue: Variant): Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure LoadFromXML(AItem: TfcxXMLItem);
    procedure SaveToXML(AItem: TfcxXMLItem);
     
    property CompareObject: TfcxRangeCompareObject read FCompareObject write SetCompareObject;
    property LowRange: Double read FLowRange write SetLowRange;
    property HighRange: Double read FHighRange write SetHighRange;
    property ValueCondition: TfcxRangeValueCondition read FValueCondition write SetValueCondition;
    property TextCondition: TfcxRangeTextCondition read FTextCondition write SetTextCondition;
    property Text: String read FText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TfcxRanges = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    FDefaultCompareObject: TfcxRangeCompareObject;
    function GetItem(AIndex: Integer): TfcxRange;
    function GetOperator(AIndex: Integer): TfcxRangeBinaryOperator;
    procedure SetOperator(AIndex: Integer;
      const Value: TfcxRangeBinaryOperator);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TfcxRange; reintroduce;
    procedure Assign(Source: TPersistent); override;

    function AsString: String;
    function Match(AValue: Variant): Boolean;

    property DefaultCompareObject: TfcxRangeCompareObject read FDefaultCompareObject write FDefaultCompareObject default rcoValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Items[AIndex: Integer]: TfcxRange read GetItem; default;
    property Operator[AIndex: Integer]: TfcxRangeBinaryOperator read GetOperator write SetOperator;
  end;

implementation

//VCL uses section
{$IFNDEF FMX}
uses
  typinfo,
  StrUtils,
  Math,
  fcxRes;
//FMX uses
{$ELSE FMX}
uses
  System.TypInfo, System.StrUtils, System.Math,
  FMX.fcxRes;
{$ENDIF FMX}

{ TfcxRange }

procedure TfcxRange.AssignTo(Dest: TPersistent);
begin
  if Dest is TfcxRange then
  begin
    TfcxRange(Dest).FCompareObject := FCompareObject;
    TfcxRange(Dest).FValueCondition := FValueCondition;
    TfcxRange(Dest).FTextCondition := FTextCondition;
    TfcxRange(Dest).FLowRange := FLowRange;
    TfcxRange(Dest).FHighRange := FHighRange;
    TfcxRange(Dest).FText := FText;
  end
  else
    inherited;
end;

function TfcxRange.AsString: String;
const
  ValueConditionToStr: array[TfcxRangeValueCondition] of String = (
  { rvcBetween        } 'sCellValueBetween',
  { rvcBeyond         } 'sCellValueBeyond',
  { rvcEqual          } 'sCellValueEqual',
  { rvcNotEqual       } 'sCellValueNotEqual',
  { rvcGreater        } 'sCellValueGreater',
  { rvcLess           } 'sCellValueLess',
  { rvcGreaterOrEqual } 'sCellValueGreaterOrEqual',
  { rvcLessOrEqual    } 'sCellValueLessOrEqual'
  );

  TextConditionToStr: array[TfcxRangeTextCondition] of String = (
 { rtcContains    } 'sCellTextContains',
 { rtcNotContains } 'sCellTextNotContains',
 { rtcStartsWith  } 'sCellTextStartsWith',
 { rtcEndsWith    } 'sCellTextEndsWith'
  );
begin
  case CompareObject of
    rcoValue:
      Result := Format(fcxResources.Get(ValueConditionToStr[ValueCondition]), [LowRange, HighRange]);
    rcoText:
      Result := Format(fcxResources.Get(TextConditionToStr[TextCondition]), [Text]);
    rcoNull:
      Result := fcxResources.Get('sCellValueIsNull');
    rcoNotNull:
      Result := fcxResources.Get('sCellValueIsNotNull');
  end;
end;

procedure TfcxRange.BeginUpdate;
begin
  inc(FUpdateCount);
end;

constructor TfcxRange.Create(Collection: TCollection);
begin
  if Collection is TfcxRanges then
    FCompareObject := TfcxRanges(Collection).DefaultCompareObject
  else
    FCompareObject := rcoValue;
  FUpdateCount := 0;
  FValueCondition := rvcBetween;
  FLowRange := NegInfinity;
  FHighRange := Infinity;
  FOperator := rboAnd;
  inherited Create(Collection);
end;

procedure TfcxRange.DoChange;
begin
  if FUpdateCount <> 0 then
    Exit;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TfcxRange.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChange;
end;

procedure TfcxRange.LoadFromXML(AItem: TfcxXMLItem);
begin
  CompareObject := TfcxRangeCompareObject(GetEnumValue(TypeInfo(TfcxRangeCompareObject), AItem.Prop['CompareObject']));
  LowRange := AItem.FloatProp['LowRange'];
  HighRange := AItem.FloatProp['HighRange'];
  ValueCondition := TfcxRangeValueCondition(GetEnumValue(TypeInfo(TfcxRangeValueCondition), AItem.Prop['ValueCondition']));
  TextCondition := TfcxRangeTextCondition(GetEnumValue(TypeInfo(TfcxRangeTextCondition), AItem.Prop['TextCondition']));
  Text := AItem.Prop['Text'];
end;

function TfcxRange.Match(AValue: Variant): Boolean;
var
  D: Double;
  S: String;
begin
  Result := False;
  case CompareObject of
    rcoValue:
      begin
        with TVarData(AValue) do
        begin
          if (VType = varOleStr) or (VType = varString) {$IFDEF Delphi_12UP} or (VType = varUString){$ENDIF} then
            Exit;
          if (VType <= 1) then
            D := 0
          else
          try
            D := AValue;
          except
            D := 0;
          end;
        end;
        case ValueCondition of
          rvcBetween: Result := (D >= LowRange) and (D <= HighRange);
          rvcBeyond: Result := (D < LowRange) or (D > HighRange);
          rvcEqual: Result := D = LowRange;
          rvcNotEqual: Result := D <> LowRange;
          rvcGreater: Result := D > LowRange;
          rvcLess: Result := D < LowRange;
          rvcGreaterOrEqual: Result := D >= LowRange;
          rvcLessOrEqual: Result := D <= LowRange;
        end;
      end;
    rcoText:
      begin
        S := VarToStr(AValue);
        case TextCondition of
          rtcContains: Result := AnsiContainsText(S, Text);
          rtcNotContains: Result := not AnsiContainsText(S, Text);
          rtcStartsWith: Result := AnsiStartsText(S, Text);
          rtcEndsWith: Result := AnsiEndsText(S, Text);
        end;
      end;
    rcoDate:
      begin
        if TVarData(AValue).VType <> varDate then
          Exit;
        D := TDateTime(AValue);
        case ValueCondition of
          rvcBetween: Result := (D >= LowRange) and (D <= HighRange);
          rvcBeyond: Result := (D < LowRange) or (D > HighRange);
          rvcEqual: Result := D = LowRange;
          rvcNotEqual: Result := D <> LowRange;
          rvcGreater: Result := D > LowRange;
          rvcLess: Result := D < LowRange;
          rvcGreaterOrEqual: Result := D >= LowRange;
          rvcLessOrEqual: Result := D <= LowRange;
        end;
      end;
    rcoNull:
      begin
        Result := TVarData(AValue).VType <= 1;
      end;
    rcoNotNull:
      begin
        Result := TVarData(AValue).VType > 1;
      end;
  end;
end;

procedure TfcxRange.SaveToXML(AItem: TfcxXMLItem);
begin
  AItem.Prop['CompareObject'] := GetEnumName(TypeInfo(TfcxRangeCompareObject), Ord(CompareObject));
  AItem.FloatProp['LowRange'] := LowRange;
  AItem.FloatProp['HighRange'] := HighRange;
  AItem.Prop['ValueCondition'] := GetEnumName(TypeInfo(TfcxRangeValueCondition), Ord(ValueCondition));
  AItem.Prop['TextCondition'] := GetEnumName(TypeInfo(TfcxRangeTextCondition), Ord(TextCondition));
  AItem.Prop['Text'] := Text;
end;

procedure TfcxRange.SetCompareObject(const Value: TfcxRangeCompareObject);
begin
  if FCompareObject <> Value then
  begin
    FCompareObject := Value;
    DoChange;
  end;
end;

procedure TfcxRange.SetHighRange(const Value: Double);
begin
  if FHighRange <> Value then
  begin
    FHighRange := Value;
    DoChange;
  end;
end;

procedure TfcxRange.SetLowRange(const Value: Double);
begin
  if FLowRange <> Value then
  begin
    FLowRange := Value;
    DoChange;
  end;
end;

procedure TfcxRange.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    DoChange;
  end;
end;

procedure TfcxRange.SetTextCondition(const Value: TfcxRangeTextCondition);
begin
  if FTextCondition <> Value then
  begin
    FTextCondition := Value;
    DoChange;
  end;
end;

procedure TfcxRange.SetValueCondition(const Value: TfcxRangeValueCondition);
begin
  if FValueCondition <> Value then
  begin
    FValueCondition := Value;
    DoChange;
  end;
end;

{ TfcxRanges }

function TfcxRanges.Add: TfcxRange;
begin
  Result := TfcxRange(inherited Add);
end;

procedure TfcxRanges.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TCollection then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TCollection(Source).Count - 1 do
        with Add do
          Assign(TCollection(Source).Items[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

function TfcxRanges.AsString: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + Items[I].AsString;
end;

constructor TfcxRanges.Create;
begin
  inherited Create(TfcxRange);
  FDefaultCompareObject := rcoValue;
end;

destructor TfcxRanges.Destroy;
begin
{$IFDEF FPC}
  if Count > 0 then
  begin
    BeginUpdate;
    Clear;
  end;
{$ENDIF}
  inherited Destroy;
end;

function TfcxRanges.GetItem(AIndex: Integer): TfcxRange;
begin
  Result := TfcxRange(inherited GetItem(AIndex));
end;

function TfcxRanges.GetOperator(AIndex: Integer): TfcxRangeBinaryOperator;
begin
  Result := Items[AIndex].FOperator;
end;

function TfcxRanges.Match(AValue: Variant): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Count = 0 then
    Exit;
  Result := Items[0].Match(AValue);
  for I := 1 to Count - 1 do
  begin
    case Operator[I-1] of
      rboAnd: Result := Result and Items[I].Match(AValue);
      rboOr: Result := Result or Items[I].Match(AValue);
    end;
  end;
end;

procedure TfcxRanges.SetOperator(AIndex: Integer; const Value: TfcxRangeBinaryOperator);
begin
  if Operator[AIndex] <> Value then
  begin
    Items[AIndex].FOperator := Value;
    Changed;
  end;
end;

procedure TfcxRanges.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

end.
