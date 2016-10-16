{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit PasswordDialog;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.passdialog}

interface

uses androidr15, Rjava, AActivity, AZCUserPasswordDialogs;

 

type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
    //
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
  private
	  button: AWButton;
	  result: AWEditText;
    PasswordDialog: AZCUserPasswordDialog;
  end;

implementation



procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
  layout : AWLinearLayout; 
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

    button:= AWButton.create(Self);
    button.setText(JLString('Dialog'));
    button.setId(1);
    button.setOnClickListener(Self);
  layout.addView(button);

	  result:= AWEditText.Create(Self);
  layout.addView(result);

  setContentView(layout);


  PasswordDialog:= AZCUserPasswordDialog.create(Self);

end;

procedure MainActivity.onClick(aView: AVView);
begin
  case aView.getId of
    1: begin
          PasswordDialog.UserName := 'SYSDBA';
          PasswordDialog.show;
       end;
  end;
end;

procedure MainActivity.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin
  case p1 of
   -2: result.setText(JLString('User name: ').concat(PasswordDialog.UserName.toString).concat(JLString(' Password: ')).concat(PasswordDialog.Password.toString))
     else result.setText(JLString('Cancel'));
   end;
end;


end.
