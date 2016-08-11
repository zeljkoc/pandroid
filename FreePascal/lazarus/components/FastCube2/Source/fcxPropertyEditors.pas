{*******************************************************}
{                                                       }
{            FastCube 2 fcPropertyEditors unit          }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxPropertyEditors;
{$INCLUDE fcx.inc}

interface
uses
  TypInfo, Classes, db, DBReg,
{$IFDEF DELPHI_6UP}
  DesignEditors,
  DesignIntf,
  RTLConsts,
{$ELSE}
  Consts,
  dsgnintf,
{$ENDIF}
  SysUtils, Dialogs,
  colnedit, forms, fcxTypes,  
  fcxDataSource, fcxComponent, 
  fcxControl, fcxDefaultSettings;
{$ELSE}
{$INCLUDE fcx.inc}
interface
uses
  // RTL
  System.TypInfo, System.Classes, System.RTLConsts,
  // DB
  Data.DB,
  // IDE
  DesignEditors, DesignIntf, DbReg,
  // FC FMX
  FMX.fcxTypes, FMX.fcxDataSource, FMX.fcxComponent, FMX.fcxControl, FMX.fcxDefaultSettings
  ;
{$ENDIF}

type
  TfcxReferenceDataFieldProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TfcxAttributeProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TfcxUseProperty = class(TClassProperty)
  private
  protected
    function GetUseValueAt(AIndex: integer): boolean; virtual; abstract;
    procedure SetUseValueAt(AIndex: integer; AUse: boolean); virtual; abstract;
  public
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TfcxAttributeTypeProperty = class(TClassProperty)
  private
  protected
  public
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TfcxSplitTypesSetElementProperty = class(TSetElementProperty)
  public
    function GetName: string; override;
  end;

  TfcxSetProperty = class(TSetProperty)
  protected
    function GetStartIndex: integer; virtual;
  public
{$IFDEF DELPHI_6UP}
    procedure GetProperties(Proc: TGetPropProc); override;
{$ELSE}
    procedure GetProperties(Proc: TGetPropEditProc); override;
{$ENDIF}
  end;

  TfcxSplitTypesSetProperty = class(TfcxSetProperty)
  protected
    function GetStartIndex: integer; override;
  public
  end;

  TfcxEnumProperty = class(TEnumProperty)
  protected
    function GetStartIndex: integer; virtual;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TfcxDataTypeProperty = class(TfcxEnumProperty)
  protected
    function GetStartIndex: integer; override;
  public
  end;

  TfcxDataSourceEditor = class(TComponentEditor)
  private
    procedure AddFields;
    procedure DeleteFields;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TfcxComponentEditor = class(TComponentEditor)
  private
    procedure SetDefaults;
  public
//    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

procedure logtofile(AMess: string);
var AStList: TStringList;
begin
  AStList := TStringList.Create;
  try
    AStList.LoadFromFile('C:\out\log.txt');
    AStList.Add(AMess);
    AStList.SaveToFile('C:\out\log.txt');
  finally
    AStList.Free;
  end;
end;

{ TfcxReferenceDataFieldProperty }

procedure TfcxReferenceDataFieldProperty.GetValueList(List: TStrings);
var
  ADataSet: TfcxDataSet;
begin
  ADataSet := (GetComponent(0) as TfcxReferenceDataField).DataSet;
  if (ADataSet <> nil) then
  begin
    ADataSet.GetFieldNames(List);
  end
end;

{ TfcxUseProperty }

function TfcxUseProperty.AllEqual: Boolean;
var
  I: Integer;
  AUse: boolean;
begin
  Result := False;
  if PropCount > 1 then
  begin
    AUse := GetUseValueAt(0);
    for I := 1 to PropCount - 1 do
      if GetUseValueAt(I) <> AUse then Exit;
  end;
  Result := True;
end;

function TfcxUseProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paMultiSelect, paSortList, paRevertable{$IFDEF DELPHI_6UP}, paVolatileSubProperties{$ENDIF}];
end;

function TfcxUseProperty.GetValue: string;
begin
  if GetUseValueAt(0) then
    Result := 'Use'
  else
    Result := 'Not use';
end;

procedure TfcxUseProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('Use');
  Proc('Not use');
end;

procedure TfcxUseProperty.SetValue(const Value: string);
var
  AUse: boolean;
  I: Integer;
begin
  AUse := (Value = 'Use');
  for I := 0 to PropCount - 1 do
    SetUseValueAt(I, AUse);
  Modified;
end;

{ TfcxSplitTypesSetProperty }

function TfcxSplitTypesSetProperty.GetStartIndex: integer;
begin
  Result := 1;
end;

{ TfcxSplitTypesSetElementProperty }

function TfcxSplitTypesSetElementProperty.GetName: string;
begin
  Result := inherited GetName;
//  Result := 'проба' + inherited GetName;
end;

{ TfcxSetProperty }

{$IFDEF DELPHI_6UP}
    procedure TfcxSetProperty.GetProperties(Proc: TGetPropProc);
{$ELSE}
    procedure TfcxSetProperty.GetProperties(Proc: TGetPropEditProc);
{$ENDIF}
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType^)^ do
    for I := MinValue + GetStartIndex to MaxValue do
      Proc(TfcxSplitTypesSetElementProperty.Create(Self, I));
end;

function TfcxSetProperty.GetStartIndex: integer;
begin
  Result := 0;
end;

{ TfcxDataTypeProperty }

function TfcxDataTypeProperty.GetStartIndex: integer;
begin
  Result := 2;
end;

{ TfcxEnumProperty }

function TfcxEnumProperty.GetStartIndex: integer;
begin
  Result := 0;
end;

procedure TfcxEnumProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType;
  with GetTypeData(EnumType)^ do
  begin
    for I := MinValue + GetStartIndex to MaxValue do Proc(GetEnumName(EnumType, I));
  end;
end;

procedure TfcxEnumProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, Value);
  with GetTypeData(GetPropType)^ do
    if (I < (MinValue + GetStartIndex)) or (I > MaxValue) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  SetOrdValue(I);
end;

{ TfcxDataSourceEditor }

procedure TfcxDataSourceEditor.AddFields;
begin
  (Component as TfcxDataSource).AddFields;
  Designer.Modified;
end;

procedure TfcxDataSourceEditor.DeleteFields;
begin
  (Component as TfcxDataSource).DeleteFields;
  Designer.Modified;
end;

procedure TfcxDataSourceEditor.Edit;
begin
//  inherited;
end;

procedure TfcxDataSourceEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: AddFields;
    1: DeleteFields;
  else
  end;
end;

function TfcxDataSourceEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add All Fields';
    1: Result := 'Delete All Fields';
  else
    Result := '';
  end;
end;

function TfcxDataSourceEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TfcxComponentEditor }

procedure TfcxComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: SetDefaults;
  else
  end;
end;

function TfcxComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Reset default';
  else
    Result := '';
  end;
end;

function TfcxComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TfcxComponentEditor.SetDefaults;
begin
  if Component is TfcxComponent then
    (Component as TfcxComponent).SetDefaults
  else
  if Component is TfcxCustomControl then
    (Component as TfcxCustomControl).SetDefaults
  else
  if Component is TfcxDefaultSettings then
    (Component as TfcxDefaultSettings).SetDefaults
end;

{ TfcxAttributeProperty }

procedure TfcxAttributeProperty.GetValueList(List: TStrings);
var
  ASplitProperty: TfcxSplitProperty;
  i: integer;
begin
  ASplitProperty := (GetComponent(0) as TfcxSourceFieldProperties).SplitProperty;
  List.BeginUpdate;
  try
    List.Clear;
    for i := 0 to ASplitProperty.Attributes.Count - 1 do
      List.Add(ASplitProperty.Attributes[i].DataField.CubeFieldName);
  finally
    List.EndUpdate;
  end;
end;

{ TfcxAttributeTypeProperty }

function TfcxAttributeTypeProperty.AllEqual: Boolean;
var
  I: Integer;
  ASourceFieldType: TfcxAttributeType;
begin
  Result := False;
  if PropCount > 1 then
  begin
    ASourceFieldType := TfcxSourceField(GetComponent(0)).SourceFieldType;
    for I := 1 to PropCount - 1 do
      if TfcxSourceField(GetComponent(I)).SourceFieldType <> ASourceFieldType then
      begin
        Exit;
      end
  end;
  Result := True;
end;

function TfcxAttributeTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paValueList, paMultiSelect, paSortList, paRevertable{$IFDEF DELPHI_6UP}, paVolatileSubProperties{$ENDIF}];
end;

function TfcxAttributeTypeProperty.GetValue: string;
begin
  case TfcxSourceField(GetComponent(0)).SourceFieldType of
    fcxsft_Reference:
      Result := 'fcxsft_Reference';
    fcxsft_Custom:
      Result := 'fcxsft_Custom';
    fcxsft_Date:
      Result := 'fcxsft_Date';
    fcxsft_Time:
      Result := 'fcxsft_Time';
  end;
end;

procedure TfcxAttributeTypeProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('fcxsft_Reference');
  Proc('fcxsft_Custom');
  Proc('fcxsft_Date');
  Proc('fcxsft_Time');
end;

procedure TfcxAttributeTypeProperty.SetValue(const Value: string);
var
  ASourceFieldType: TfcxAttributeType;
  I: Integer;
begin
  if Value = 'fcxsft_Reference' then
    ASourceFieldType := fcxsft_Reference
  else
  if Value = 'fcxsft_Custom' then
    ASourceFieldType := fcxsft_Custom
  else
  if Value = 'fcxsft_Date' then
    ASourceFieldType := fcxsft_Date
  else
    ASourceFieldType := fcxsft_Time;
  for I := 0 to PropCount - 1 do
    TfcxSourceField(GetComponent(I)).SourceFieldType := ASourceFieldType;
  Modified;
end;

end.
