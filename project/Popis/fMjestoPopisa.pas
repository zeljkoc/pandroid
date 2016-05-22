unit fMjestoPopisa;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.popis}

interface

uses androidr15, Rjava, AActivity, AZCListView, ADBDataBase, AZCScrollButons;

type

  { MjestoPopisaActivity }

  MjestoPopisaActivity = class(Activity)
     procedure onCreate(savedInstanceState: AOBundle); override;
     procedure onClick(aView: AVView); override;

  public
    db :  ASQLDatabase;
    EditListView: AEditListView;
    SCButton: AZCScroolButton;
    procedure CreateFields;
  end;

implementation

uses Data, dataset, AZCDialogs;

{ MjestoPopisaActivity }

procedure MjestoPopisaActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;

 bt: AWButton;

begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

 {   SCButton:= AZCScroolButton.create(Self);

      bt:= AWButton.create(Self);
      bt.setText(JLString(string('+')));
      bt.setId(1);
      bt.setTypeface(nil, AGTypeface.BOLD);
      bt.setOnClickListener(Self);
     SCButton.Buttons.add(bt);
     SCButton.RefreshView;

  layout.addView(SCButton); }

   db := initDataBase(self, db);

     EditListView := AEditListView.create(Self, db, JLString('select  MjestoPopisaID, Napomena as MjestoPopisa from MjestoPopisa order by MjestoPopisaID desc'), true );
     EditListView.TableName := 'MjestoPopisa';
  layout.addView(EditListView);

  CreateFields;

  EditListView.RefreshView;

  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);
  setContentView(layout);
end;

procedure MjestoPopisaActivity.onClick(aView: AVView);
var
  bt: AWButton;
  i: integer;
  text: JLstring;
begin

  //inherited onClick(aView);
 { if (aView is AWButton) then
    showMessage(Self, JLString('Klick Button:').concat(JLInteger.toString(AView.getId)))
  else showMessage(Self, JLString('Klick Image:').concat(JLInteger.toString(AView.getId)));}

 { if (SCButton.Buttons.get(0) is AWEditText) then
    ShowMessage(Self, AWEditText(SCButton.Buttons.get(0)).getText.toString);  }

 { SCButton.Buttons.clear;
  for i:=0 to 10 do begin
      bt:= AWButton.create(Self);
      bt.setText(JLString('Button ').concat(JLInteger.toString(i)));
      bt.setId(i);
      bt.setOnClickListener(Self);
      SCButton.Buttons.add(bt);
    end; }
   // SCButton.RefreshView;
end;

procedure MjestoPopisaActivity.CreateFields;
var
  tField: ZCField;
begin
   EditListView.EditFields := CursorToFields(EditListView.ListAdapter.Cursor);

   //MjestoPopisaID
   ZCField(EditListView.EditFields.get(0)).fDataType := ZCField.InnerFDataType.ftIinteger;
   ZCField(EditListView.EditFields.get(0)).fReadOnly := true;

   //Napomena
   ZCField(EditListView.EditFields.get(1)).fName := 'Napomena';
   ZCField(EditListView.EditFields.get(1)).fDisplayName := 'MjestoPopisa';
   ZCField(EditListView.EditFields.get(1)).fCharCase := ZCField.InnerEditCharCase.fUpperCase;


end;

end.

