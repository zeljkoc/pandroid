{*******************************************************}
{                                                       }
{              FastCube 2 XML-export unit               }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxExportXML;
{$INCLUDE fcx.inc}

interface

uses
  Classes, SysUtils, Controls, Dialogs, Graphics, Forms, StdCtrls,
  {$IFDEF DELPHI_6UP}
  Variants,
  {$ENDIF}
  {$IFDEF SQL_TYPES_EXTRA0}
  FMTBcd,
  {$ENDIF}
  {$IFDEF SQL_TYPES_EXTRA1}
  SqlTimSt,
  {$ENDIF}
  fcxRes, fcxTypes, fcxSlice, fcxXML, fcxCustomExport, fcxExportDialog,
  fcxCube, fcxUtils;
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  // RTL
  System.Classes, System.SysUtils, System.Variants, System.UITypes,
  {$IFDEF SQL_TYPES_EXTRA0}
  Data.FMTBcd,
  {$ENDIF}
  {$IFDEF SQL_TYPES_EXTRA1}
  Data.SqlTimSt,
  {$ENDIF}
  // FMX
  FMX.StdCtrls, FMX.ListBox,
  // FC FMX
  FMX.fcxTypes, FMX.fcxSlice, FMX.fcxXML, FMX.fcxCustomExport,
  FMX.fcxRes, FMX.fcxCube, FMX.fcxUtils, FMX.fcxExportDialog;
{$ENDIF FMX}

type
  TfcxXMLFormat = (
    xfRowset,
    xfDataset,
    xfDataPacket,
    xfXMLA
  );

  TfcxXMLExportDialog = class(TfcxExportDialog)
  private
    OpenCB: TCheckBox;
    cbXMLFormat: TComboBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TfcxXMLExport = class(TfcxCustomExportFilter)
  private
    FXMLFormat: TfcxXMLFormat;
    FXML: TfcxXMLDocument;
    FOpenAfterExport: Boolean;
  protected
    procedure BuildRowset(Doc: TfcxXMLDocument);
    procedure BuildDataset(Doc: TfcxXMLDocument);
    procedure BuildDataPacket(Doc: TfcxXMLDocument);
    procedure BuildXMLA(Doc: TfcxXMLDocument);
  public
    class function GetDescription: String; override;
    constructor Create(AOwner: TComponent); override;
    function ShowModal: TModalResult; override;
    function Start: Boolean; override;
    procedure Run; override;
    procedure Finish; override;
    property XMLFormat: TfcxXMLFormat read FXMLFormat write FXMLFormat default xfXMLA;
    property OpenAfterExport: Boolean read FOpenAfterExport write FOpenAfterExport default False;
  end;

implementation

const
  // XML Tag DataSet
  XMLTagDataPacket = 'DATAPACKET';
  XMLTagMetaData = 'METADATA';
  XMLTagFields = 'FIELDS';
  XMLTagField = 'FIELD';
  XMLTagIndicies = 'INDICIES';
  XMLTagIndex = 'INDEX';
  XMLTagRowData = 'ROWDATA';
  XMLTagRow = 'ROW';

const
// from SQL/A example
  xsdSchema =
'<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" targetNamespace="urn:schemas-microsoft-com:xmlanalysis:mddataset" elementFormDefault="qualified" xmlns:sql="urn:schemasmicrosoft-com:xml-sql">'#$D#$A+
'<xsd:complexType name="MemberType">'#$D#$A+
'<xsd:attribute name="Hierarchy" type="xsd:string"></xsd:attribute>'#$D#$A+
'<xsd:sequence>'#$D#$A+
'<xsd:element name="UName" type="xsd:string"></xsd:element>'#$D#$A+
'<xsd:element name="Caption" type="xsd:string"></xsd:element>'#$D#$A+
'<xsd:element name="LName" type="xsd:string"></xsd:element>'#$D#$A+
'<xsd:element name="LNum" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="DisplayInfo" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded" minOccurs="0">'#$D#$A+
'<xsd:any processContents="lax" maxOccurs="unbounded"></xsd:any>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="PropType">'#$D#$A+
'<xsd:attribute name="name" type="xsd:string"></xsd:attribute>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="TupleType">'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="Member" type="MemberType"></xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="MembersType">'#$D#$A+
'<xsd:attribute name="Hierarchy" type="xsd:string"></xsd:attribute>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="Member" type="MemberType"></xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="TuplesType">'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="Tuple" type="TupleType"></xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="CrossProductType">'#$D#$A+
'<xsd:choice minOccurs="0" maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="Members" type="MembersType"></xsd:element>'#$D#$A+
'<xsd:element name="Tuples" type="TuplesType"></xsd:element>'#$D#$A+
'</xsd:choice>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="OlapInfo">'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="AxesInfo">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="AxisInfo">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:attribute name="name" type="xsd:string"></xsd:attribute>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="HierarchyInfo">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:attribute name="name" type="xsd:string"></xsd:attribute>'#$D#$A+
'<xsd:sequence>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="UName" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="Caption" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="LName" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="LNum" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="DisplayInfo" type="PropType"></xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded" minOccurs="0">'#$D#$A+
'<xsd:any processContents="lax" maxOccurs="unbounded"></xsd:any>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'<xsd:element name="CellInfo">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:sequence>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:choice>'#$D#$A+
'<xsd:element name="Value" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="FmtValue" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="BackColor" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="ForeColor" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="FontName" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="FontSize" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="FontFlags" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="FormatString" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="NonEmptyBehavior" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="SolveOrder" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="Updateable" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="Visible" type="PropType"></xsd:element>'#$D#$A+
'<xsd:element name="Expression" type="PropType"></xsd:element>'#$D#$A+
'</xsd:choice>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded" minOccurs="0">'#$D#$A+
'<xsd:any processContents="lax" maxOccurs="unbounded"></xsd:any>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="Axes">'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="Axis">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:attribute name="name" type="xsd:string"></xsd:attribute>'#$D#$A+
'<xsd:choice minOccurs="0" maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="CrossProduct" type="CrossProductType"></xsd:element>'#$D#$A+
'<xsd:element name="Tuples" type="TuplesType"></xsd:element>'#$D#$A+
'<xsd:element name="Members" type="MembersType"></xsd:element>'#$D#$A+
'</xsd:choice>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:complexType name="CellData">'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="Cell">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:attribute name="CellOrdinal" type="xsd:unsignedInt"></xsd:attribute>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:choice>'#$D#$A+
'<xsd:element name="Value"></xsd:element>'#$D#$A+
'<xsd:element name="FmtValue" type="xsd:string"></xsd:element>'#$D#$A+
'<xsd:element name="BackColor" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="ForeColor" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="FontName" type="xsd:string"></xsd:element>'#$D#$A+
'<xsd:element name="FontSize" type="xsd:unsignedShort"></xsd:element>'#$D#$A+
'<xsd:element name="FontFlags" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="FormatString" type="xsd:string"></xsd:element>'#$D#$A+
'<xsd:element name="NonEmptyBehavior" type="xsd:unsignedShort"></xsd:element>'#$D#$A+
'<xsd:element name="SolveOrder" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="Updateable" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="Visible" type="xsd:unsignedInt"></xsd:element>'#$D#$A+
'<xsd:element name="Expression" type="xsd:string"></xsd:element>'#$D#$A+
'</xsd:choice>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'<xsd:element name="root">'#$D#$A+
'<xsd:complexType>'#$D#$A+
'<xsd:sequence maxOccurs="unbounded">'#$D#$A+
'<xsd:element name="OlapInfo" type="OlapInfo"></xsd:element>'#$D#$A+
'<xsd:element name="Axes" type="Axes"></xsd:element>'#$D#$A+
'<xsd:element name="CellData" type="CellData"></xsd:element>'#$D#$A+
'</xsd:sequence>'#$D#$A+
'</xsd:complexType>'#$D#$A+
'</xsd:element>'#$D#$A+
'</xsd:schema>';

  xsdAnyType = 'anyType';
  xsdString = 'string';
  xsdBoolean = 'boolean';
  xsdDecimal = 'decimal';
  xsdFloat = 'float';
  xsdDouble = 'double';
  xsdDateTime = 'dateTime';
  xsdLong = 'long';
  xsdInt = 'int';
  xsdShort = 'short';
  xsdByte = 'byte';
  xsdUnsignedLong = 'unsignedLong';
  xsdUnsignedInt = 'unsignedInt';
  xsdUnsignedShort = 'unsignedShort';
  xsdUnsignedByte = 'unsignedByte';

const
  VarTypeToXSDMap: array[0..{$IFDEF DELPHI_6UP}varInt64{$ELSE}varByte{$ENDIF}] of String =
  (
 { varEmpty    } xsdAnyType,
 { varNull     } xsdAnyType,
 { varSmallint } xsdShort,
 { varInteger  } xsdInt,
 { varSingle   } xsdFloat,
 { varDouble   } xsdDouble,
 { varCurrency } xsdDecimal,
 { varDate     } xsdDateTime,
 { varOleStr   } xsdString,
 { varDispatch } xsdAnyType,
 { varError    } xsdAnyType,
 { varBoolean  } xsdBoolean,
 { varVariant  } xsdAnyType,
 { varUnknown  } xsdAnyType,
 { varDecimal  } xsdDecimal,
 { undefined   } xsdAnyType,
 { varShortInt } xsdByte,
 { varByte     } xsdUnsignedByte
{$IFDEF DELPHI_6UP}
,
 { varWord     } xsdUnsignedShort,
 { varLongWord } xsdUnsignedInt,
 { varInt64    } xsdLong
{$ENDIF}
  );

function DataTypeToVarType(DataType: TfcxDataType): Integer;
begin
  case DataType of
    fcdt_Integer: Result := varInteger;
    fcdt_Byte: Result := varByte;
    fcdt_Word: Result := varWord;
    fcdt_DateTime: Result := varDate;
    fcdt_Date: Result := varDate;
    fcdt_Time: Result := varDate;
    fcdt_SmallInteger: Result := varSmallint;
    fcdt_LargeInteger: Result := varInt64;
    fcdt_Double: Result := varDouble;
    fcdt_BCD: Result := VarFMTBcd;
    fcdt_String: Result := varString;
    fcdt_WideString: Result := varOleStr;
    fcdt_Boolean: Result := varBoolean;
    fcdt_Currency: Result := varCurrency;
  else
    Result := varError;
  end;
end;

function VartypeToXSD(VType: Integer): String;
begin
  if VType in [0..{$IFDEF DELPHI_6UP}varInt64{$ELSE}varByte{$ENDIF}] then
    Result := VarTypeToXSDMap[VType]
  else
  if VType = varString then
    Result := xsdString
  {$IFDEF SQL_TYPES_EXTRA1}
  else
  if VType = VarSQLTimeStamp then
    Result := xsdDateTime
  else
  if VType = VarFmtBcd then
    Result := xsdDecimal
  {$ENDIF}
  else
    Result := xsdAnyType;
end;

function CheckFieldName(s: String): String;
begin
  if s[1] = '#' then
  begin
    Delete(s, 1, 1);
    s := 'sysfield_' + s;
  end;
  Result := trim(s);
end;

function VarToValidXML(V: Variant): String;
var
  OldDecimalSeparator: Char;
  AResult: String;
begin
  case TVarData(V).VType of
    varDate:
      begin
        DateTimeToString(AResult, 'yyyy-mm-dd"T"hh:nn:ss.zzz', V);
        Result := String(AResult) + '0000';
      end;
    varSingle,
    varDouble:
      begin
        OldDecimalSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
        {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
        Result := FloatToStr(V);
        {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
      end;
  else
    Result := VarToStr(V);
  end;
end;

{ TfcxXMLExport }

// Build flat rowset
procedure TfcxXMLExport.BuildDataset(Doc: TfcxXMLDocument);
var
  Columns: TfcxCubeDataColumns;
  
  procedure BuildDSDescription(el: TfcxXMLItem);
  var
    item, list: TfcxXMLItem;
    i: integer;
  begin
    item := el.Add;
    item.Name := 'xs:complexType';

    item := item.Add;
    item.Name := 'xs:choice';
    item.Prop['minOccurs'] := '0';
    item.Prop['maxOccurs'] := 'unbounded';

    item := item.Add;
    item.Name := 'xs:element';
    item.Prop['name'] := 'FlatDS';

    item := item.Add;
    item.Name := 'xs:complexType';

    list := item.Add;
    list.Name := 'xs:sequence';

    for i := 0 to Columns.VisibleCount - 1 do
    begin
      item := list.Add;
      item.Name := 'xs:element';
      item.Prop['name'] := CheckFieldName(Columns[i].Field.CubeFieldName);
      item.Prop['type'] := 'xs:' + VartypeToXSD(DataTypeToVarType(Columns[i].Field.DataType));
      item.Prop['minOccurs'] := '0';
    end;
  end;

var
  Schema, el, Row: TfcxXMLItem;
  V: Variant;
  i, j: integer;
begin
  Columns := DoGetCubeCols;
  Doc.Root.Name := 'Cube';

  Schema := Doc.Root.Add;
  Schema.Name := 'xs:schema';
  Schema.Prop['id'] := 'Cube';
  Schema.Prop['xmlns:xs'] := 'http://www.w3.org/2001/XMLSchema';
  Schema.Prop['xmlns:msdata'] := 'urn:schemas-microsoft-com:xml-msdata';

  el := Schema.Add;
  el.Name := 'xs:element';
  el.Prop['name'] := 'Cube';
  el.Prop['msdata:IsDataSet'] := 'true';
  //el.Prop['msdata:UseCurrentLocale'] := 'true';

  BuildDSDescription(el);

  for i := 0 to DoGetCubeRowCount - 1 do
  begin
    Row := Doc.Root.Add;
    Row.Name := 'FlatDS';
    for j := 0 to Columns.VisibleCount - 1 do
    begin
      V := Cube.SourceHolder.UniqueValueAsVariant[DoGetCubeRowIndex(i), Columns[j].Field];
      if TVarData(V).VType > 1 then
      begin
        el := Row.Add;
        el.Name := CheckFieldName(Columns[j].Field.CubeFieldName);
        el.Value := VarToValidXML(V);
      end;
    end;
    DoProgress(i);
  end;
end;

procedure TfcxXMLExport.BuildRowset(Doc: TfcxXMLDocument);
var
  Columns: TfcxCubeDataColumns;
  
  procedure BuildSchema(Schema: TfcxXMLItem);
  var
    Container, Attr, dt: TfcxXMLItem;
    i: Integer;
  begin
    Container := Schema.Add;
    Container.Name := 's:ElementType';
    Container.Prop['name'] := 'row';
    Container.Prop['content'] := 'eltOnly';
    Container.Prop['rs:updatable'] := 'true';

    for i := 0 to Columns.VisibleCount - 1 do
    begin
      Attr := Container.Add;
      Attr.Name := 's:AttributeType';
      Attr.Prop['name'] := CheckFieldName(Columns[i].Field.CubeFieldName);
      Attr.IntProp['rs:number'] := i + 1;
      Attr.Prop['rs:nullable'] := 'true';
      Attr.Prop['rs:maydefer'] := 'true';
      Attr.Prop['rs:writeunknown'] := 'true';
      Attr.Prop['rs:basetable'] := 'cube';
      Attr.Prop['rs:basecolumn'] := Attr.Prop['name'];
      dt := Attr.Add;
      dt.Name := 's:datatype';
      dt.Prop['dt:type'] := 'string';
      dt.IntProp['dt:maxLength'] := 1024;
    end;
    Container := Schema.Add;
    Container.Name := 's:extends';
    Container.Prop['type'] := 'rs:rowbase';
  end;

  procedure BuildData(Data: TfcxXMLItem);
  var
    i, j: Integer;
    Row: TfcxXMLItem;
    V: Variant;
  begin
    for i := 0 to DoGetCubeRowCount - 1 do
    begin
      Row := Data.Add;
      Row.Name := 'z:row';
      for j := 0 to Columns.VisibleCount - 1 do
      begin
        V := Cube.SourceHolder.UniqueValueAsVariant[DoGetCubeRowIndex(i), Columns[j].Field];
        if TVarData(V).VType > 1 then
          Row.Prop[CheckFieldName(Columns[j].Field.CubeFieldName)] := VarToStr(V);
      end;
      DoProgress(i);      
    end;
  end;

var
  Schema, Data: TfcxXMLItem;
begin
  Columns := DoGetCubeCols;
  Doc.Root.Name := 'xml';
  Doc.Root.Prop['xmlns:s'] := 'uuid:BDC6E3F0-6DA3-11d1-A2A3-00AA00C14882';
	Doc.Root.Prop['xmlns:dt'] := 'uuid:C2F41010-65B3-11d1-A29F-00AA00C14882';
	Doc.Root.Prop['xmlns:rs'] := 'urn:schemas-microsoft-com:rowset';
	Doc.Root.Prop['xmlns:z'] := '#RowsetSchema';

  Schema := Doc.Root.Add;
  Schema.Name := 's:Schema';
  Schema.Prop['id'] := 'RowsetSchema';

  BuildSchema(Schema);

  Data := Doc.Root.Add;
  Data.Name := 'rs:data';

  BuildData(Data);
end;

procedure TfcxXMLExport.BuildDataPacket(Doc: TfcxXMLDocument);
var
  Columns: TfcxCubeDataColumns;
  
  procedure BuidMetaData(MetaData: TfcxXMLItem);
  var
    i: integer;
    FieldsXML, FieldXML: TfcxXMLItem;
  begin
    FieldsXML := MetaData.Add;
    FieldsXML.Name := XMLTagFields;

    for i := 0 to Columns.VisibleCount - 1 do
    begin
      FieldXML := FieldsXML.Add;
      FieldXML.Name := XMLTagField;
      FieldXML.Prop['attrname'] := CheckFieldName(Columns[i].Field.CubeFieldName);

      case Columns[i].Field.DataType of
        fcdt_String,
        fcdt_WideString:
          begin
            FieldXML.Prop['fieldtype'] := 'string';
            FieldXML.IntProp['width'] := 1024; // ?
          end;
        fcdt_Word, fcdt_SmallInteger,
        fcdt_Integer, fcdt_LargeInteger: FieldXML.Prop['fieldtype'] := 'i4';
        fcdt_Boolean: FieldXML.Prop['fieldtype'] := 'boolean';
        fcdt_BCD, fcdt_Double:
          begin
            FieldXML.Prop['fieldtype'] := 'r8';
            FieldXML.IntProp['width'] := 38;
            FieldXML.IntProp['prec'] := 18; //Columns[i].Field.Precision;
          end;
        fcdt_Currency:
          begin
            FieldXML.Prop['fieldtype'] := 'r8';
            FieldXML.Prop['SUBTYPE'] := 'Money';
          end;
        fcdt_Date: FieldXML.Prop['fieldtype'] := 'date';
        fcdt_Time: FieldXML.Prop['fieldtype'] := 'time';
        fcdt_DateTime: FieldXML.Prop['fieldtype'] := 'datetime';
      end;
    end;
  end;

  procedure BuildRowData(RowData: TfcxXMLItem);
  var
    i, j: integer;
    Row: TfcxXMLItem;
    V: Variant;
  begin
    for i := 0 to DoGetCubeRowCount - 1 do
    begin
      Row := RowData.Add;
      Row.Name := XMLTagRow;
      for j := 0 to Columns.VisibleCount - 1 do
      begin
        V := Cube.SourceHolder.UniqueValueAsVariant[DoGetCubeRowIndex(i), Columns[j].Field];
        if TVarData(V).VType > 1 then
          Row.Prop[CheckFieldName(Columns[j].Field.CubeFieldName)] := VarToStr(V);
      end;
    end;
  end;

var
  DSXML, RowData, MetaData: TfcxXMLItem;
begin
  Columns := DoGetCubeCols;
  DSXML := Doc.Root;

  DSXML.Name := XMLTagDataPacket;
  DSXML.Prop['Version'] := '2';

  MetaData := DSXML.Add;
  MetaData.Name := XMLTagMetaData;

  BuidMetaData(MetaData);

  RowData := DSXML.Add;
  RowData.Name := XMLTagRowData;

  BuildRowData(RowData); // Записываю данные
end;

// Build XML for analysis document
procedure TfcxXMLExport.BuildXMLA(Doc: TfcxXMLDocument);

  procedure BuildXSD(XSD: TfcxXMLItem);
  var
    Reader: TfcxXMLReader;
    S: TStream;
  begin
    S := TStringStream.Create(xsdSchema);
    Reader := TfcxXMLReader.Create(S);
    Reader.ReadRootItem(XSD);
    Reader.Free;
    S.Free;
  end;

  procedure BuildHierarchyInfo(HierarchyInfo: TfcxXMLItem; const LevelInfo: TfcxAxisLevelInfo);
  var
    Name: String;
    Item: TfcxXMLItem;
  begin
    if LevelInfo.IsMeasure then
      Name := Slice.MeasuresContainer.Caption
    else
      Name := LevelInfo.RegionField.Name;
    HierarchyInfo.Prop['name'] := Name;

    // UName MEMBER_UNIQUE_NAME property from OLE DB axis rowset
    Item := HierarchyInfo.Add;
    Item.Name := 'UName';
    Item.Prop['Name'] := Format('[%s].[MEMBER_UNIQUE_NAME]', [Name]);

    // Caption MEMBER_CAPTION property from OLE DB axis rowset
    Item := HierarchyInfo.Add;
    Item.Name := 'Caption';
    Item.Prop['Name'] := Format('[%s].[MEMBER_CAPTION]', [Name]);

    // LName LEVEL_UNIQUE_NAME property from OLE DB axis rowset
    Item := HierarchyInfo.Add;
    Item.Name := 'LName';
    Item.Prop['Name'] := Format('[%s].[LEVEL_UNIQUE_NAME]', [Name]);

    // LNum LEVEL_NUMBER property from OLE DB axis rowset
    Item := HierarchyInfo.Add;
    Item.Name := 'LNum';
    Item.Prop['Name'] := Format('[%s].[LEVEL_NUMBER]', [Name]);

    Item := HierarchyInfo.Add;
    Item.Name := 'DisplayInfo';
    Item.Prop['Name'] := Format('[%s].[DISPLAY_INFO]', [Name]);
  end;

  procedure BuildAxisInfo(AxesInfo: TfcxXMLItem);
  var
    XAxis, YAxis, HierarchyInfo: TfcxXMLItem;
    i: integer;
  begin
    // 2 Axis. Order: X, Y
    XAxis := AxesInfo.Add;
    XAxis.Name := 'AxisInfo';
    XAxis.Prop['name'] := 'Axis0';

    for i := 0 to Slice.XAxisContainer.VisibleLevelCount - 1 do
    begin
      // write hierarchies
      HierarchyInfo := XAxis.Add;
      HierarchyInfo.Name := 'HierarchyInfo';
      BuildHierarchyInfo(HierarchyInfo, Slice.XAxisContainer.VisibleLevelInfo[i]);
    end;

    YAxis := AxesInfo.Add;
    YAxis.Name := 'AxisInfo';
    YAxis.Prop['name'] := 'Axis1';

    for i := 0 to Slice.YAxisContainer.VisibleLevelCount - 1 do
    begin
      // write hierarchies
      HierarchyInfo := YAxis.Add;
      HierarchyInfo.Name := 'HierarchyInfo';
      BuildHierarchyInfo(HierarchyInfo, Slice.YAxisContainer.VisibleLevelInfo[i]);
    end;
  end;

  procedure BuildCellInfo(CellInfo: TfcxXMLItem);
  var
    Item: TfcxXMLItem;
  begin
    // every cell will have:
    // VALUE, FmtValue, FormatString, BackColor, ForeColor, something else?
    Item := CellInfo.Add;
    Item.Name := 'Value';
    Item.Prop['name'] := 'VALUE';

    Item := CellInfo.Add;
    Item.Name := 'FmtValue';
    Item.Prop['name'] := 'FORMATTED_VALUE';

    Item := CellInfo.Add;
    Item.Name := 'FormatString';
    Item.Prop['name'] := 'FORMAT_STRING';

    Item := CellInfo.Add;
    Item.Name := 'BackColor';
    Item.Prop['name'] := 'BACK_COLOR';

    Item := CellInfo.Add;
    Item.Name := 'ForeColor';
    Item.Prop['name'] := 'FORE_COLOR';
  end;

  procedure BuildOlapInfo(OlapInfo: TfcxXMLItem);
  var
    CubeInfo, AxesInfo, CellInfo: TfcxXMLItem;
  begin
    CubeInfo := OlapInfo.Add;
    CubeInfo.Name := 'CubeInfo';
    with CubeInfo.Add do
    begin
      Name := 'Cube';
      with Add do
      begin
        Name := 'CubeName';
        Value := Slice.Cube.Caption;
      end;
    end;

    AxesInfo := OlapInfo.Add;
    AxesInfo.Name := 'AxesInfo';
    BuildAxisInfo(AxesInfo);

    CellInfo := OlapInfo.Add;
    CellInfo.Name := 'CellInfo';
    BuildCellInfo(CellInfo);
  end;

  procedure BuildMember(Tuple: TfcxXMLItem; AxisContainer: TfcxAxisContainer; LevelIndex, VisibleIndex: Integer);
  var
    LevelInfo: TfcxAxisLevelInfo;
    Member, Item: TfcxXMLItem;
    Name: String;
  begin
    LevelInfo := AxisContainer.VisibleLevelInfo[LevelIndex];
    if LevelInfo.IsMeasure then
      Name := AxisContainer.Slice.MeasuresContainer.Caption
    else
      Name := LevelInfo.RegionField.Name;

    Member := Tuple.Add;
    Member.Name := 'Member';
    Member.Prop['Hierarchy'] := Name;

    Item := Member.Add;
    Item.Name := 'UName';
    Item.Value := Format('[%s].[%s]', [Name, VarToValidXML(AxisContainer.DimValue[LevelIndex, VisibleIndex])]);

    Item := Member.Add;
    Item.Name := 'Caption';
    Item.Value := AxisContainer.DimCaption[LevelIndex, VisibleIndex];

    Item := Member.Add;
    Item.Name := 'LName';
    Item.Value := Format('[%s]', [Name]);

    Item := Member.Add;
    Item.Name := 'LNum';
    Item.Value := IntToStr(0); // todo: groups
  end;

  procedure BuildTuple(Tuples: TfcxXMLItem; AxisContainer: TfcxAxisContainer; VisibleIndex: Integer);
  var
    Tuple: TfcxXMLItem;
    i: integer;
  begin
    Tuple := Tuples.Add;
    Tuple.Name := 'Tuple';
    for i := 0 to AxisContainer.VisibleLevelCount - 1 do
      BuildMember(Tuple, AxisContainer, i, VisibleIndex);
  end;

  procedure BuildAxes(Axes: TfcxXMLItem);
  var
    XAxis, YAxis, Tuples: TfcxXMLItem;
    i: integer;
  begin
    // 2 Axis. Order: X, Y
    XAxis := Axes.Add;
    XAxis.Name := 'Axis';
    XAxis.Prop['name'] := 'Axis0';

    Tuples := XAxis.Add;
    Tuples.Name := 'Tuples';


    for i := 0 to Slice.XAxisContainer.VisibleNodeCount - 1 do
      BuildTuple(Tuples, Slice.XAxisContainer, i);

    YAxis := Axes.Add;
    YAxis.Name := 'Axis';
    YAxis.Prop['name'] := 'Axis1';

    Tuples := YAxis.Add;
    Tuples.Name := 'Tuples';

    for i := 0 to Slice.YAxisContainer.VisibleNodeCount - 1 do
      BuildTuple(Tuples, Slice.YAxisContainer, i);
  end;

  function ColorToStr(AColor: Integer): String;
  begin
    Result := '#' + IntToHex(AColor, 6);
  end;

  procedure BuildCellData(CellData: TfcxXMLItem);
  var
    Row, Col, ordinal: Integer;
    Cell, Item: TfcxXMLItem;
    MeasureCell: TfcxMeasureCell;
    Value: Variant;
  begin
    ordinal := 0;
    for Row := 0 to Slice.RowCount - 1 do
    begin
      for Col := 0 to Slice.ColCount - 1 do
      begin
        Slice.GetMeasureCell(Col, Row, MeasureCell);
        Value := MeasureCell.Value;

        if TVarData(Value).VType > 1 then
        begin
          Cell := CellData.Add;
          Cell.Name := 'Cell';
          Cell.IntProp['CellOrdinal'] := ordinal;

          Item := Cell.Add;
          Item.Name := 'Value';
          Item.Prop['xsi:type'] := 'xsd:' + VartypeToXSD(TVarData(Value).VType);
          Item.Value := VarToValidXML(Value);

          Item := Cell.Add;
          Item.Name := 'FmtValue';
          Item.Value := MeasureCell.StrValue;

{ todo
          Item := Cell.Add;
          Item.Name := 'FormatString';
          Item.Value := Slice.FieldsOfRegion(rf_CapFacts)[MeasureIndex].DisplayFormat.FormatStr;
}
        end;
        inc(ordinal);
      end;
      DoProgress(Row);
    end;
  end;
  
  procedure BuildMDDataset(Root: TfcxXMLItem);
  var
    XSD, OlapInfo, Axes, CellData: TfcxXMLItem;
  begin
    XSD := Root.Add;
    XSD.Name := 'xsd:schema';
    XSD.Prop['xmlns:xsd'] := 'http://www.w3.org/2001/XMLSchema';
    BuildXSD(XSD);

    OlapInfo := Root.Add;
    OlapInfo.Name := 'OlapInfo';
    BuildOlapInfo(OlapInfo);

    Axes := Root.Add;
    Axes.Name := 'Axes';
    BuildAxes(Axes);

    CellData := Root.Add;
    CellData.Name := 'CellData';
    CellData.Prop['xmlns:xsi'] := 'http://www.w3.org/2001/XMLSchema-instance';
    BuildCellData(CellData);
  end;

var
  Body, Response, Return, Root: TfcxXMLItem;
begin
  Doc.Root.Name := 'SOAP-ENV:Envelope';
  Doc.Root.Prop['xmlns:SOAP-ENV'] := 'http://schemas.xmlsoap.org/soap/envelope/';
  Doc.Root.Prop['SOAP-ENV:encodingStyle'] := 'http://schemas.xmlsoap.org/soap/encoding/';

  Body := Doc.Root.Add;
  Body.Name := 'SOAP-ENV:Body';

  Response := Body.Add;
  Response.Name := 'ExecuteResponse';
  Response.Prop['xmlns'] := 'urn:schemas-microsoft-com:xml-analysis';

  Return := Response.Add;
  Return.Name := 'return';

  Root := Return.Add;
  Root.Name := 'root';
  Root.Prop['xmlns'] := 'urn:schemas-microsoft-com:xml-analysis:mddataset';

  BuildMDDataset(Root);
end;

constructor TfcxXMLExport.Create(AOwner: TComponent);
begin
  inherited;
  FilterDesc := fcxResources.Get('sXMLFilter');
  DefaultExt := fcxResources.Get('sXMLExt');
  FXMLFormat := xfXMLA;
  FOpenAfterExport := False; 
end;

class function TfcxXMLExport.GetDescription: String;
begin
  Result := fcxResources.Get('sXMLexport');
end;

function TfcxXMLExport.ShowModal: TModalResult;
const
  XMLFormatStr: array[TfcxXMLFormat] of String = (
    'Rowset',
    'Dataset',
    'DataPacket',
    'XML for Analysis'
  );
var
  X: TfcxXMLFormat;  
begin
  if not Assigned(Stream) then
  begin
    with TfcxXMLExportDialog.Create(nil) do
    begin
      PrepareSaveDialog(SaveDialog);
      OpenCB.Checked := FOpenAfterExport;

      for X := Low(TfcxXMLFormat) to Pred(High(TfcxXMLFormat)) do
        cbXMLFormat.Items.Add(XMLFormatStr[X]);
      if Assigned(Slice) then
        cbXMLFormat.Items.Add(XMLFormatStr[High(TfcxXMLFormat)])
      else
      if XMLFormat = High(TfcxXMLFormat) then
        XMLFormat := Low(TfcxXMLFormat);
      cbXMLFormat.ItemIndex := Ord(XMLFormat);

      Result := ShowModal;
      if Result = mrOk then
      begin
        FOpenAfterExport := OpenCB.Checked;
        XMLFormat := TfcxXMLFormat(cbXMLFormat.ItemIndex);
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

procedure TfcxXMLExport.Finish;
begin
  if Assigned(Stream) then
    FXML.SaveToStream(Stream)
  else
    FXML.SaveToFile(FileName);
  
  FXML.Free;

  if FOpenAfterExport and not Assigned(Stream) then
    OpenDocument(FileName);
end;

function TfcxXMLExport.Start: Boolean;
begin
  if (FileName <> '') or Assigned(Stream) then
  begin
    if (ExtractFilePath(FileName) = '') and (DefaultPath <> '') then
      FileName := DefaultPath + '\' + FileName;
    FXML := TfcxXMLDocument.Create;
    Result := True;
  end
  else
    Result := False;
end;

procedure TfcxXMLExport.Run;
begin
  case XMLFormat of
    xfRowset: BuildRowset(FXML);
    xfDataset: BuildDataset(FXML);
    xfDataPacket: BuildDataPacket(FXML);
    xfXMLA: BuildXMLA(FXML);
  end;
end;

{ TfcxXMLExportDialog }

constructor TfcxXMLExportDialog.Create(AOwner: TComponent);
var
  Page: TComponent;
begin
  inherited;

  Caption := fcxResources.Get('HTMLExport');
  Page := AddPage(fcxResources.Get('sExport'));
  cbXMLFormat := AddComboBox(Page, []);
  AddLabelFor(cbXMLFormat, fcxResources.Get('sXMLFormat') + ':', False);
  OpenCB := AddCheckBox(Page, fcxGet(8706));
end;

end.
