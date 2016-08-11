{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoSQLite/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoSQLite;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demosqlite}

interface

uses androidr15, Rjava, AActivity,  AZCListView, ADBDataBase, AZCScrollButons;


type

  { MainActivity }

  MainActivity = class(Activity)
  public 
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

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;

 bt: AWButton;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

   db := initDataBase(self, db);

     EditListView := AEditListView.create(Self, db, JLString('select  MjestoPopisaID, MjestoPopisa from MjestoPopisa order by MjestoPopisaID desc'), true );
     EditListView.TableName := 'MjestoPopisa';
  layout.addView(EditListView);

  CreateFields;

  EditListView.RefreshView;

  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);  
begin
 case aView.getId of
      10: Finish;
 end;
         
end;

procedure MainActivity.CreateFields;
var
  tField: ZCField;
begin
 EditListView.EditFields := CursorToFields(EditListView.ListAdapter.Cursor);

 //MjestoPopisaID
 ZCField(EditListView.EditFields.get(0)).fDataType := ZCField.InnerFDataType.ftIinteger;
 ZCField(EditListView.EditFields.get(0)).fReadOnly := true;

 //Napomena
 ZCField(EditListView.EditFields.get(1)).fName := 'MjestoPopisa';
 ZCField(EditListView.EditFields.get(1)).fDisplayName := 'MjestoPopisa';
 ZCField(EditListView.EditFields.get(1)).fCharCase := ZCField.InnerEditCharCase.fUpperCase;
end;


end.
