{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}

unit DB;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

uses
  androidr15;

type
  TDataType = (ftNull, ftInteger, ftFloat, ftString, ftBlob);
  TEditCharCase = (eccNormal, eccLowerCase, eccUpperCase);

  { TValue }

  TValue = class
    FValue: JLString;
  private
    function GetAsString: JLString;
    function GetFloat: jfloat;
    function GetHex: JLString;
    function GetInt: jint;
    procedure SetAsString(Value: JLString);
    procedure SetFloat(Value: jfloat);
    procedure SetHex(Value: JLString);
    procedure SetInt(Value: jint);
  public
   constructor Create; overload; virtual;
   property AsString: JLString read GetAsString write SetAsString;
   property AsInteger: jint read GetInt write SetInt;
   property AsFloat: jfloat read GetFloat write SetFloat;
   property AsHex: JLString read GetHex write SetHex;
  end;

  { TField }

   TField = class
   private
      FFieldNo: jint;
      FDataType: TDataType;
      FReadOnly: jboolean;
      FVisible: jboolean;
      FOldValue: TValue;
      FName: JLString;
      FDisplayName: JLString;
      FValue: TValue;
      FChange: jboolean;
      FCharCase: TEditCharCase;
   public
      constructor create; overload; virtual;
   public
      property FieldNo: jint read FFieldNo write FFieldNo;
      property DataType: TDataType read FDataType write FDataType;
      property ReadOnly: jboolean read FReadOnly write FReadOnly;
      property Visible: jboolean read FVisible write FVisible;
      property Value: TValue read FValue write FValue;
      property OldValue: TValue read FOldValue write FOldValue;
      property Name: JLString read FName write FName;
      property DisplayName: JLString read FDisplayName write FDisplayName;
      property Change: jboolean read FChange write FChange;
      property CharCase: TEditCharCase read FCharCase write FCharCase;
  end;

  { TFieldDefs }

  TFieldDefs = class(JUArrayList)
  private
    function GetChange(Index: jint): jboolean;
   function GetDataType(Index: jint): TDataType;
   function GetDisplayName(Index: jint): JLString;
   function GetName(Index: jint): JLString;
   function GetOldValue(Index: jint): TValue;
   function GetReadOnly(Index: jint): jboolean;
   function GetValue(Index: jint): TValue;
   function GetVisible(Index: jint): jboolean;
   function SetCharCase(Index: jint): TEditCharCase;
   procedure SetCharCase(Index: jint; Value: TEditCharCase);
   procedure SetDataType(Index: jint; Value: TDataType);
   procedure SetDisplayName(Index: jint; Value: JLString);
   procedure SetName(Index: jint; Value: JLString);
   procedure SetOldValue(Index: jint; Value: TValue);
   procedure SetReadOnly(Index: jint; Value: jboolean);
   procedure SetValue(Index: jint; Value: TValue);
   procedure SetVisible(Index: jint; Value: jboolean);
  public
    constructor create; overload; virtual;
    procedure AddField(aName: JLString; aDataType: TDataType);
  public
    property FieldCount: jint read size;
    property DataType[Index: jint]: TDataType read GetDataType write SetDataType;
    property ReadOnly[Index: jint]: jboolean read GetReadOnly write SetReadOnly;
    property Visible[Index: jint]: jboolean read GetVisible write SetVisible;
    property Value[Index: jint]: TValue read GetValue write SetValue;
    property OldValue[Index: jint]: TValue read GetOldValue write SetOldValue;
    property Name[Index: jint]: JLString read GetName write SetName;
    property DisplayName[Index: jint]: JLString read GetDisplayName write SetDisplayName;
    property Change[Index: jint]: jboolean read GetChange;
    property CharCase[Index: jint]: TEditCharCase read SetCharCase write SetCharCase;
  end;


implementation

{ TValue }

function TValue.GetAsString: JLString;
begin
  Result := FValue;
end;

function TValue.GetFloat: jfloat;
begin
  Result := JLFloat.parseFloat(FValue);
end;

function TValue.GetHex: JLString;
begin
  Result :=  upcase(JLInteger.toHexString(JLInteger.parseInt(FValue)));
end;

function TValue.GetInt: jint;
begin
  Result := JLInteger.parseInt(FValue);
end;

procedure TValue.SetAsString(Value: JLString);
begin
  FValue := Value;
end;

procedure TValue.SetFloat(Value: jfloat);
begin
  FValue := JLFloat.toString(Value);
end;

procedure TValue.SetHex(Value: JLString);
begin
  FValue :=  JLInteger.toString(JLInteger.parseInt(Value, 16));
end;

procedure TValue.SetInt(Value: jint);
begin
  FValue := JLInteger.toString(Value);
end;

constructor TValue.Create;
begin
  inherited Create;
end;

{ TField }

constructor TField.create;
begin
  inherited Create;
  FValue := TValue.Create;
  FOldValue := TValue.Create;
end;


{ TFieldDefs }

function TFieldDefs.GetChange(Index: jint): jboolean;
begin
  Result := TField(get(Index)).Change;
end;

function TFieldDefs.GetDataType(Index: jint): TDataType;
begin
  Result := TField(get(Index)).DataType;
end;

function TFieldDefs.GetDisplayName(Index: jint): JLString;
begin
  Result := TField(get(Index)).DisplayName;
end;

function TFieldDefs.GetName(Index: jint): JLString;
begin
  Result := TField(get(Index)).Name;
end;

function TFieldDefs.GetOldValue(Index: jint): TValue;
begin
  Result := TField(get(Index)).OldValue;
end;

function TFieldDefs.GetReadOnly(Index: jint): jboolean;
begin
  Result := TField(get(Index)).ReadOnly;
end;

function TFieldDefs.GetValue(Index: jint): TValue;
begin
   Result := TField(get(Index)).Value;
end;

function TFieldDefs.GetVisible(Index: jint): jboolean;
begin
    Result := TField(get(Index)).Visible;
end;

function TFieldDefs.SetCharCase(Index: jint): TEditCharCase;
begin
   Result := TField(get(Index)).CharCase;
end;

procedure TFieldDefs.SetCharCase(Index: jint; Value: TEditCharCase);
begin
  TField(get(Index)).CharCase := Value;
end;

procedure TFieldDefs.SetDataType(Index: jint; Value: TDataType);
begin
   TField(get(Index)).DataType := Value;
end;

procedure TFieldDefs.SetDisplayName(Index: jint; Value: JLString);
begin
  TField(get(Index)).DisplayName := Value;
end;

procedure TFieldDefs.SetName(Index: jint; Value: JLString);
begin
   TField(get(Index)).Name := Value;
end;

procedure TFieldDefs.SetOldValue(Index: jint; Value: TValue);
begin
  TField(get(Index)).OldValue := Value;
end;

procedure TFieldDefs.SetReadOnly(Index: jint; Value: jboolean);
begin
   TField(get(Index)).ReadOnly := Value;
end;

procedure TFieldDefs.SetValue(Index: jint; Value: TValue);
begin
  TField(get(Index)).Value := Value;
end;

procedure TFieldDefs.SetVisible(Index: jint; Value: jboolean);
begin
   TField(get(Index)).Visible := Value;
end;

constructor TFieldDefs.create;
begin
  inherited Create;
end;

procedure TFieldDefs.AddField(aName: JLString; aDataType: TDataType);
begin
  add(TField.create);
  TField(get(size - 1)).Name := aName;
  TField(get(size - 1)).DisplayName := aName;
  TField(get(size - 1)).DataType := aDataType;
  TField(get(size - 1)).ReadOnly := False;
  TField(get(size - 1)).CharCase := eccNormal;
  TField(get(size - 1)).Visible := true;
end;


end.

