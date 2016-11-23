unit AZCUserPasswordDialogs;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.userpassworddialog}

interface

uses androidr15;

type

  { AZCUserPasswordDialog }

  AZCUserPasswordDialog = class(AAAlertDialog)
    procedure onClick(para1: ACDialogInterface; para2: jint); overload;
  private
    FUserNameEdit: AWEditText;
    FPasswordEdit: AWEditText;
    function GetPassword: JLString;
    function GetUserName: JLString;
    procedure SetPassword(Value: JLString);
    procedure SetUserName(Value: JLString);
  public
    constructor create(para1: ACContext); overload;
  public
    property UserName: JLString read GetUserName write SetUserName;
    property Password: JLString read GetPassword write SetPassword;
  end;



implementation

{ AZCUserPasswordDialog }


function AZCUserPasswordDialog.GetUserName: JLString;
begin
  Result := FUserNameEdit.getText.toString;
end;

procedure AZCUserPasswordDialog.onClick(para1: ACDialogInterface; para2: jint);
begin

end;

function AZCUserPasswordDialog.GetPassword: JLString;
begin
  Result := FPasswordEdit.getText.toString;
end;

procedure AZCUserPasswordDialog.SetPassword(Value: JLString);
begin
  FPasswordEdit.setText(Value);
  FPasswordEdit.selectAll();
end;

procedure AZCUserPasswordDialog.SetUserName(Value: JLString);
begin
  FUserNameEdit.setText(Value);
end;


constructor AZCUserPasswordDialog.create(para1: ACContext);
var
  UserNameCaption : AWTextView;
  PasswordCaption : AWTextView;

  layout : AWLinearLayout;
begin
  inherited Create(para1);
  setTitle(AAActivity(para1).getTitle.toString);
  setIcon(AAActivity(para1).getPackageManager.getApplicationIcon(AAActivity(para1).getApplicationInfo));

    layout:= AWLinearLayout.Create(Self.getContext);
    layout.setOrientation(AWLinearLayout.VERTICAL);

      UserNameCaption:= AWTextView.create(self.getContext);
      UserNameCaption.setText(JLString('User name: '));
    layout.addView(UserNameCaption);

  	  FUserNameEdit:= AWEditText.create(Self.getContext);
    layout.addView(FUserNameEdit);


    PasswordCaption:= AWTextView.create(self.getContext);
    PasswordCaption.setText(JLString('Password: '));
  layout.addView(PasswordCaption);

      FPasswordEdit:= AWEditText.create(Self.getContext);
      FPasswordEdit.requestFocus;
      FPasswordEdit.setInputType(ATInputType.TYPE_TEXT_VARIATION_PASSWORD);
      FPasswordEdit.setTransformationMethod(ATMPasswordTransformationMethod.create);
    layout.addView(FPasswordEdit);

  setView(layout);

  setButton(JLString('Ok'), ACDialogInterface.InnerOnClickListener(para1));   //-1
  setButton2(JLString('Cancel'), ACDialogInterface.InnerOnClickListener(para1));      //-2

   getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_STATE_VISIBLE);

end;

end.

