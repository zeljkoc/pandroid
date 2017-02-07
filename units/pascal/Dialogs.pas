{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
unit Dialogs;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

uses
  androidr15,  DialogsView;

type
  TOnClickEventDialog   = procedure (para1: ACDialogInterface; para2: jint) of object;

  TTypeButton = (btNeutral, btNegative, btPositive);

  { TDialog }

  TDialog = class(AAAlertDialog, ACDialogInterface.InnerOnClickListener)
  private
   fID: integer;
   FOnClick: TOnClickEventDialog;
  public
    procedure onClick(para1: ACDialogInterface; para2: jint); overload; virtual;
  public
   constructor create(para1: ACContext); overload; virtual;
   procedure AddButton(aTypeButton: TTypeButton; aName: JLCharSequence);
  public
   property ID: integer read fID write fID;
   property OnClickListener: TOnClickEventDialog read FOnClick write FOnClick;
  end;

  { TTimePickerDialog }

  TTimePickerDialog = class(TDialog)
  private
    FTimePicker: AWTimePicker;
  public
    constructor create(para1: ACContext); override;
  public
    property TimePicker: AWTimePicker read FTimePicker;
  end;

  { TDatePickerDialog }

  TDatePickerDialog = class(TDialog)
  private
    FDatePicker: AWDatePicker;
  public
    constructor create(para1: ACContext); override;
  public
    property DatePicker: AWDatePicker read FDatePicker;
  end;

  { TUserNamePasswordDialog }

  TUserNamePasswordDialog = class(TDialog)
  private
    FUserPasswordView: TUserPasswordView;
  public
    constructor create(para1: ACContext); overload; override;
  public
    property UserNamePassword: TUserPasswordView read FUserPasswordView;
  end;

  { TEditFile }

  { TEditFileDialog }

  TEditFileDialog = class(TDialog)
  private
    FFileName: JLString;
    FEditText: AWEditText;
    FHorizontalScropllView : AWHorizontalScrollView;
  strict protected
    procedure LoadFile;
    procedure WriteFile;
  public
    constructor create(para1: ACContext; aFileName: JLString); overload;
    procedure show; overload; override;
    procedure onClick(para1: ACDialogInterface; para2: jint); overload; override;
  end;


implementation

uses Utils;

{ TEditFile }

procedure TEditFileDialog.LoadFile;
var
  reader: JIBufferedReader;
  line: JLString;
  Data: JLString;
begin
  line := string(''); Data := string('');

  if checkExistsFile(FFileName) then begin
       reader := JIBufferedReader.create((JIFileReader.create(FFileName)));
       while not (line = nil) do begin
         try
           line := reader.readLine;
           if line <> nil then
             Data := JLString(Data).concat(line).concat(string(#10));
         finally
         end;
       end;
   end;
   FEditText.setText(Data);
end;

procedure TEditFileDialog.WriteFile;
begin
    with JIFileWriter.create(fFileName) do begin
      append(FEditText.getText.toString);
      close;
    end;
end;

constructor TEditFileDialog.create(para1: ACContext; aFileName: JLString);
begin
  fFileName := aFileName;
  inherited create(para1);
  setTitle(JLString('EDIT INI FILE '));

  fHorizontalScropllView := AWHorizontalScrollView.create(para1);

      fEditText:= AWEditText.create(para1);
     fHorizontalScropllView.addView(FEditText);

  setView(fHorizontalScropllView);

  setButton(JLString('Save'), Self);
  setButton2(JLString('Cancel'), Self);
 inherited OnClickListener :=  @onClick;
end;

procedure TEditFileDialog.show;
begin
  LoadFile;
  inherited show;
end;

procedure TEditFileDialog.onClick(para1: ACDialogInterface; para2: jint);
begin
  case para2 of
    -1: WriteFile;
  end;
end;


{ TDialog }

procedure TDialog.onClick(para1: ACDialogInterface; para2: jint);
begin
  if Assigned(FOnClick) then FOnClick(para1, para2);
end;

constructor TDialog.create(para1: ACContext);
begin
  inherited Create(para1);
 // setTitle(AAActivity(para1).getPackageManager.getApplicationLabel(AAActivity(para1).getApplicationInfo));
 // setIcon(AAActivity(para1).getPackageManager.getApplicationIcon(AAActivity(para1).getApplicationInfo));
end;

procedure TDialog.AddButton(aTypeButton: TTypeButton; aName: JLCharSequence);
begin
   setButton(ord(aTypeButton) - (Ord(High(TTypeButton)) + 1), aName, Self);
end;


{ TTimePickerDialog }

constructor TTimePickerDialog.create(para1: ACContext);
begin
  inherited Create(para1);
  FTimePicker:= AWTimePicker.create(getContext);
  Self.setView(FTimePicker);
end;


{ TDatePickerDialog }

constructor TDatePickerDialog.create(para1: ACContext);

begin
  inherited Create(para1);
  FDatePicker := AWDatePicker.create(getContext);
  Self.setView(FDatePicker);
end;


{ TUserNamePasswordDialog }

constructor TUserNamePasswordDialog.create(para1: ACContext);
begin
  inherited Create(para1);
  FUserPasswordView := TUserPasswordView.create(getContext);
  FUserPasswordView.UserNameCaption := 'User name: ';
  FUserPasswordView.PasswordCaption := 'Password:  ';
  Self.setView(FUserPasswordView);
end;








end.

