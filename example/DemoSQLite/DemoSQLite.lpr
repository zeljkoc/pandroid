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

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity,  DB, DataBase, Dialogs, AZCToolBar, StdCtrls;


type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
        //popup menu
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public
    dBase: TDataBase;
    aField : TFieldDef;
    lEditListView: TDBGridViewLayout;
    ToolBar : TZCToolBar;
    ibAdd : TImageButton;

    procedure InsertRecord;
  end;

implementation

uses Data;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;

begin
  inherited onCreate(savedInstanceState);
  setTitle(JLString(' Exit'));

  layout:= AWLinearLayout.create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  getActionBar.setDisplayHomeAsUpEnabled(true);
  getActionBar.setIcon(R.drawable.exit);

       ToolBar:= TZCToolBar.Create(Self);
       with ToolBar do begin
         setVerticalSpacingGridView(2);
         setHorizontalSpacingGridView(2);
         setNumColumnsGridView(AWGridView.AUTO_FIT);
         setColumnWidthGridView(220);
       end;

       ibAdd := TImageButton.create(Self);                // novi
       ibAdd.setImageResource(R.drawable.addrecord);
       ibAdd.Id := 1;
       ibAdd.onClickListener := @onClick;
     ToolBar.add(ibAdd);

  layout.addView(ToolBar);


  dBase := initDataBase(self, dBase);

  lEditListView :=  TDBGridViewLayout.create(Self, dBase);
   with lEditListView  do begin
     GridView.setNumColumns(1);
     Adapter.CursorDataSet.TableName := 'MjestoPopisa';
     Adapter.CursorDataSet.SQLSelect := JLString('select  MjestoPopisaID, MjestoPopisa from MjestoPopisa order by MjestoPopisaID desc');

     //MjestoPopisaID
     Adapter.CursorDataSet.FieldAll.ReadOnly[0] := True;
     Adapter.CursorDataSet.FieldAll.Visible[0] := False;

     //MjestoPopisa
     Adapter.CursorDataSet.FieldAll.DataType[1] := ftString;
     Adapter.CursorDataSet.FieldAll.CharCase[1]:= eccUpperCase;

     Adapter.CursorDataSet.Refresh;
      //click row and edit
     Adapter.CursorDataSet.SQLUpdate := JLString('update MjestoPopisa set MjestoPopisa = :MjestoPopisa where MjestoPopisaID = :MjestoPopisaID ');
     //long click row and Delete
     Adapter.CursorDataSet.SQLDelete := JLString('delete from MjestoPopisa where MjestoPopisaID = :MjestoPopisaID ');
     ReadOnlyEdit:= false;
     ReadOnlyDelete:= false;
     Adapter.CursorDataSet.SQLInsert := JLString('insert into MjestoPopisa (MjestoPopisa ) values ( :MjestoPopisa  ) ');

    //  Adapter.CreateView := @ViewMjestoPopisa;
    //  OnAfterEventDialog := @onAfterDialog;
      Adapter.HTMLTemplate := '<h3> <b>  <u> <#MjestoPopisaID> <#MjestoPopisa>  </u> </b> </h3> ';

   end;
  layout.addView(lEditListView);

  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);  
begin
 case aView.getId of
      1: InsertRecord;
 end;
end;

function MainActivity.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
 Result := true;
  case item.getItemID of
    AR.Innerid.home: Finish; //onBackPressed;
    else
      Result := false;
 end;
end;

procedure MainActivity.InsertRecord;
begin
 aField := lEditListView.Adapter.CursorDataSet.FieldAll; // MjestoPopisaID, MjestoPopisa
 //MjestoPopisaID
 aField.Value[0].AsString := string('1');

// MjestoPopisa
 aField.Value[1].AsString := JLString('Test');

  lEditListView.InsertDialog(aField);             // Dialog
 // or
 // lEditListView.Adapter.CursorDataSet.Insert(aField);  //not Dialogs

 lEditListView.Refresh;
end;


end.
