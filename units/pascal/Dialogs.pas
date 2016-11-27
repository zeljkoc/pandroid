unit Dialogs;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.dialogs}

interface

uses
  androidr15;

type
  TOnDateSetEvent = procedure (para1: AWDatePicker; para2: jint; para3: jint; para4: jint) of object;
  TOnTimeSetEvent = procedure (para1: AWTimePicker; para2: jint; para3: jint) of object;
  { TDatePickerDialog }

  TDatePickerDialog = class( AADatePickerDialog.InnerOnDateSetListener)
    FDatePickerDialog: AADatePickerDialog;
    FOnDateSet: TOnDateSetEvent;
    procedure onDateSet(para1: AWDatePicker; para2: jint; para3: jint; para4: jint); overload;
  public
     constructor Create(para1: ACContext; Year, Month, Day: jint); virtual;
     procedure show; //overload; override;
  public
    property  OnDateSetListener: TOnDateSetEvent read FOnDateSet write FOnDateSet;
  end;

  { TTimePickerDialog }

  TTimePickerDialog = Class(AATimePickerDialog.InnerOnTimeSetListener)
    FTimePickerDialog: AATimePickerDialog;
    FOnTimeSet: TOnTimeSetEvent;
    procedure onTimeSet(para1: AWTimePicker; para2: jint; para3: jint); overload;
  public
    constructor Create(para1: ACContext; Hour, Minute: jint); virtual;
    procedure Show;
  public
    property onTimeSetListener: TOnTimeSetEvent read FOnTimeSet write FOnTimeSet;
  end;


implementation

{ TTimePickerDialog }

procedure TTimePickerDialog.onTimeSet(para1: AWTimePicker; para2: jint; para3: jint);
begin
    if Assigned(FOnTimeSet) then FOnTimeSet(para1, para2, para3);
end;

constructor TTimePickerDialog.Create(para1: ACContext; Hour, Minute: jint);
begin
  inherited Create;
  FTimePickerDialog := AATimePickerDialog.create(para1, self, Hour, Minute, ATFDateFormat.is24HourFormat(para1));
end;

procedure TTimePickerDialog.Show;
begin
  FTimePickerDialog.show;
end;


{ TDatePickerDialog }

procedure TDatePickerDialog.onDateSet(para1: AWDatePicker; para2: jint; para3: jint; para4: jint);
begin
  if Assigned(FOnDateSet) then FOnDateSet(para1, para2, para3, para4);
end;

constructor TDatePickerDialog.create(para1: ACContext; Year, Month, Day: jint);
var
  c: JUCalendar;
begin
  inherited Create;
  FDatePickerDialog:= AADatePickerDialog.create(para1, self, Year, Month, Day);
end;

procedure TDatePickerDialog.show;
begin
  FDatePickerDialog.show;
end;

end.

