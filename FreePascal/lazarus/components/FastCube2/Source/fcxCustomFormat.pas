{*******************************************************}
{                                                       }
{            FastCube 2 Custom Format unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxCustomFormat;
{$INCLUDE fcx.inc}

interface
uses
  Classes, fcxList, fcxTypes;
//FMX uses
{$ELSE FMX}
{$INCLUDE fcx.inc}

interface
uses
  System.Classes, FMX.fcxList, FMX.fcxTypes;
{$ENDIF FMX}

Type
  TfcxFormatData = function(AValue: Variant; ADecSeparator: String): TfcxString;

  TfcxCustomFormat = class(TObject)
  private
    FName: string;
    FCaption: string;
    FFormatData: TfcxFormatData;
    FDecSeparator: String;
    procedure SetCaption(const Value: string);
    procedure SetDecSeparator(const Value: string);
  protected
  public
    constructor Create(AName: string; ACaption: string; AFormatData: TfcxFormatData; ADecSeparator: String);
    function FormatData(AValue: Variant; ADecSeparator: String): TfcxString;
    property Caption: string read FCaption write SetCaption;
    property DecSeparator: string read FDecSeparator write SetDecSeparator;
    property Name: string read FName;
  end;

  TfcxCustomFormats = class(TObject)
  private
    FList: TfcxList;
    function GetCount: integer;
    function GetFormat(AIndex: Integer): TfcxCustomFormat;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function FindFormat(AName: string): integer;
    function AddFormat(AName: string; ACaption: string; AFormatData: TfcxFormatData; ADecSeparator: String): integer;
    property Count: integer read GetCount;
    property Format[AIndex: Integer]: TfcxCustomFormat read GetFormat; default;
  end;
  function fcCustomFormats: TfcxCustomFormats;

implementation
//VCL uses section
{$IFNDEF FMX}
uses
  Math, SysUtils;
//FMX uses
{$ELSE FMX}
uses
  System.Math, System.SysUtils;
{$ENDIF FMX}

var
  FCustomFormats: TfcxCustomFormats = nil;

function fcCustomFormats: TfcxCustomFormats;
begin
  if FCustomFormats = nil then
    FCustomFormats := TfcxCustomFormats.Create;
  Result := FCustomFormats;
end;

{ TfcxCustomFormats }

function TfcxCustomFormats.AddFormat(AName: string; ACaption: string;
  AFormatData: TfcxFormatData; ADecSeparator: String): integer;
begin
  result := FindFormat(AName);
  if result = -1 then
    result := FList.Add(TfcxCustomFormat.Create(AName, ACaption, AFormatData, ADecSeparator))
  else
  begin
    TfcxCustomFormat(FList[result]).FFormatData := AFormatData;
    TfcxCustomFormat(FList[result]).FCaption := ACaption;
    TfcxCustomFormat(FList[result]).FDecSeparator := ADecSeparator;
  end
end;

constructor TfcxCustomFormats.Create;
begin
  FList := TfcxList.Create;
end;

destructor TfcxCustomFormats.Destroy;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    TfcxCustomFormat(FList[i]).Free;
    FList[i] := nil;
  end;
  FList.Free;
  inherited;
end;

function TfcxCustomFormats.FindFormat(AName: string): integer;
begin
  for Result := 0 to Count - 1 do
    if Format[Result].FName = AName then
      Exit;
  Result := -1;
end;

function TfcxCustomFormats.GetCount: integer;
begin
  Result := FList.Count;
end;

function TfcxCustomFormats.GetFormat(AIndex: Integer): TfcxCustomFormat;
begin
  Result := TfcxCustomFormat(FList[AIndex])
end;

{ TfcxCustomFormat }

constructor TfcxCustomFormat.Create(AName: string; ACaption: string;
  AFormatData: TfcxFormatData; ADecSeparator: String);
begin
  FName := AName;
  FCaption := ACaption;
  FFormatData := AFormatData;
  FDecSeparator := ADecSeparator;
end;

function TfcxCustomFormat.FormatData(AValue: Variant; ADecSeparator: String): TfcxString;
begin
  Result := FFormatData(AValue, ADecSeparator);
end;

procedure TfcxCustomFormat.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TfcxCustomFormat.SetDecSeparator(const Value: string);
begin
  FDecSeparator := Value;
end;

initialization

finalization
  if FCustomFormats <> nil then
    FCustomFormats.Free;
  FCustomFormats := nil;
end.


