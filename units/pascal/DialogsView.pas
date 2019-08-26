{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit DialogsView;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

//uses androidr15;
{$include ../AndroidVersion.inc}
;

type

  { TUserPasswordView }

  TUserPasswordView = class(AWLinearLayout)
  private
    FUserNameCaption: AWTextView;
    FPasswordCaption: AWTextView;
    FUserNameEdit: AWEditText;
    FPasswordEdit: AWEditText;
    function GetPassword: JLString;
    function GetUserName: JLString;
    procedure SetPassword(Value: JLString);
    procedure SetPasswordCaption(Value: JLString);
    procedure SetUserName(Value: JLString);
    procedure SetUserNameCaption(Value: JLString);
  public
    constructor create(para1: ACContext); overload;
  public
    property UserNameCaption: JLString write SetUserNameCaption;
    property PasswordCaption: JLString write SetPasswordCaption;
    property UserName: JLString read GetUserName write SetUserName;
    property Password: JLString read GetPassword write SetPassword;
  end;

implementation

{ TUserPasswordView }


procedure TUserPasswordView.SetPasswordCaption(Value: JLString);
begin
  FPasswordCaption.setText(Value);
end;

function TUserPasswordView.GetPassword: JLString;
begin
  Result := FPasswordEdit.getText.toString;
end;

function TUserPasswordView.GetUserName: JLString;
begin
  Result := FUserNameEdit.getText.toString;
end;

procedure TUserPasswordView.SetPassword(Value: JLString);
begin
  FPasswordEdit.setText(Value);
  FPasswordEdit.selectAll();
end;

procedure TUserPasswordView.SetUserName(Value: JLString);
begin
   FUserNameEdit.setText(Value);
end;

procedure TUserPasswordView.SetUserNameCaption(Value: JLString);
begin
  FUserNameCaption.setText(Value);
end;


constructor TUserPasswordView.create(para1: ACContext);
begin
  inherited Create(para1);
  setOrientation(AWLinearLayout.VERTICAL);

  FUserNameCaption:= AWTextView.create(getContext);
  addView(FUserNameCaption);

  FUserNameEdit:= AWEditText.create(getContext);
  addView(FUserNameEdit);

  FPasswordCaption:= AWTextView.create(getContext);
  addView(FPasswordCaption);

  FPasswordEdit:= AWEditText.create(getContext);
  FPasswordEdit.requestFocus;
  FPasswordEdit.setInputType(ATInputType.TYPE_TEXT_VARIATION_PASSWORD);
  FPasswordEdit.setTransformationMethod(ATMPasswordTransformationMethod.create);
  addView(FPasswordEdit);
end;

end.

