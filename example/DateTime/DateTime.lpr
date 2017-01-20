{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit DateTime;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.datetime}

interface

uses androidr15, Rjava, AActivity, AZCDialogs;

 

type

  { MainActivity }

  MainActivity = class(Activity, AADatePickerDialog.InnerOnDateSetListener,
                                 AATimePickerDialog.InnerOnTimeSetListener)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;

    procedure onDateSet(para1: AWDatePicker; para2: jint; para3: jint; para4: jint);  overload;
    procedure onTimeSet(para1: AWTimePicker; para2: jint; para3: jint); overload;
  public
    bDatePicker, bTimePicker: AWButton;
    tDate, tTime: AWEditText;
    mYear, mMonth, mDay, mHour, mMinute: jint;

    DateDialog: AADatePickerDialog;
    TimeDialog: AATimePickerDialog;
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout; 
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

    bDatePicker:= AWButton.create(Self);
    bDatePicker.setText(JLString('SELECT DATE'));
    bDatePicker.setId(1);
    bDatePicker.setOnClickListener(Self);
  layout.addView(bDatePicker);

    bTimePicker:= AWButton.create(Self);
    bTimePicker.setText(JLString('SELECT TIME'));
    bTimePicker.setId(2);
    bTimePicker.setOnClickListener(Self);
  layout.addView(bTimePicker);

    tDate:= AWEditText.create(Self);
  layout.addView(tDate);

    tTime:= AWEditText.create(Self);
  layout.addView(tTime);

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);
var
  c: JUCalendar;
begin
  case aView.getId of
    1: begin
      c := JUCalendar.getInstance;
      mYear  := c.get(JUCalendar.YEAR);
      mMonth := c.get(JUCalendar.MONTH);
      mDay   := c.get(JUCalendar.DAY_OF_MONTH);
      DateDialog:= AADatePickerDialog.Create(Self, Self, mYear, mMonth, mDay );
      DateDialog.Show;
    end;

    2: begin
      c := JUCalendar.getInstance;
      mHour := c.get(JUCalendar.HOUR_OF_DAY);
      mMinute := c.get(JUCalendar.MINUTE);
      TimeDialog:= AATimePickerDialog.Create(Self, Self, mHour, mMinute, true);
      TimeDialog.Show;
    end;
  end;
end;

procedure MainActivity.onDateSet(para1: AWDatePicker; para2: jint; para3: jint; para4: jint);
begin
  tDate.setText(JLString('Date: ').concat(JLInteger.toString(para2)).concat(' : ').concat(JLInteger.toString(para3))
      .concat(' : ').concat(JLInteger.toString(para4)));
end;

procedure MainActivity.onTimeSet(para1: AWTimePicker; para2: jint; para3: jint);
begin
 tTime.setText(JLString('Time: ').concat(JLInteger.toString(para2)).concat(' : ').concat(JLInteger.toString(para3)));
end;

end.
