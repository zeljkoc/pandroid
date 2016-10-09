{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 28-10-15 10:54:43
***********************************************************}
unit fPopis;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.popis}

interface


uses androidr15, Rjava, AActivity, Data, ADBDataBase, AZCDialogs, AZCAdapters, AZCListView, AZCScrollButons;

var
  IDMjestoPopisa: integer;

type

  { PopisActivity }

  PopisActivity = class(Activity)
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
    procedure onItemClick(tAdapter: AWAdapterView; tView: AVView; p1: LongInt; p2: Int64);override;
    function onItemLongClick(tAdapter:AWAdapterView; tView: AVView; p1: LongInt; p2:Int64):Boolean;override;
    procedure afterTextChanged(editable: ATEditable); override;
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    procedure onDismiss(aInt: ACDialogInterface); override;
  public
    db :  ASQLDatabase;
    et: AWEditText;
    bMjestoPopisa: AWButton;
    SCButton: AZCScroolButton;

    cur: ADCursor;

    NewSif: boolean;
    NewIDMaterijal: String;
    dialog: AEditDialog;

    adapter : AZCDBListAdapter;
   // ListView : AWListView;
    EditListView: AEditListView;


  public
     procedure DialogMaterijal(IDMaterijal: String);
     procedure InsertPopis(IDMaterijal: String);
  end;

implementation

uses dataset, {IniFile,} Popis;

{ PopisActivity }

procedure PopisActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;
 bt: AWButton;
  c: ADCursor;

begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);

    db := initDataBase(self, db);

   c := db.rawQuery(JLString('select Napomena from MjestoPopisa where MjestoPopisaID = ').concat(JLInteger.toString(IDMjestoPopisa)) ,nil );
   c.moveToPosition(0);


    bMjestoPopisa:= AWButton.Create(Self);
    if (c.getCount <> 0) then
      bMjestoPopisa.setText(c.getString(0));
    bMjestoPopisa.setID(1);
    bMjestoPopisa.setBackGroundColor(AGColor.RED);
    bMjestoPopisa.setOnClickListener(Self);
  layout.addView(bMjestoPopisa);

   //----------------------------------

    SCButton:= AZCScroolButton.create(Self);

      et:= AWEditText.create(self);
      et.addTextChangedListener(self);
      et.setMinWidth(300);
     // et.setInputType(ATInputType.TYPE_NUMBER_FLAG_DECIMAL );
      et.setVisibility(AVView.VISIBLE);  //AVView.GONE
      et.requestFocus;
    SCButton.Buttons.add(et);

      bt := AWButton.create(Self);
      bt.setId(2);
      bt.setOnClickListener(Self);
      bt.setText(JLstring('...'));
    SCButton.Buttons.add(bt);


    SCButton.RefreshView;

  layout.addView(SCButton);



     EditListView := AEditListView.create(Self, db, JLString('select pl.PopisnaListaID, m.IDMaterijal, m.Materijal, pl.Kolicina  from PopisnaLista pl,  sMaterijal m ').concat(
                                'where (pl.IDMaterijal = m.MaterijalID) and (pl.IDMjestoPopisa = ').concat(JLInteger.toString(IDMjestoPopisa).toString.trim).concat
                                (') order by PopisnaListaID desc ')
                                );

     EditListView.TableName := 'PopisnaLista';
     ZCField(EditListView.ListAdapter.Fields.get(0)).fVisible:= False;   //polje koje se nevidi u listi

     EditListView.EditFields := CursorToFields(EditListView.ListAdapter.Cursor);
     //pl.PopisnaListaID
     ZCField(EditListView.EditFields.get(0)).fReadOnly:= true; //primary key nije vidljiv obavezno i readonly
     ZCField(EditListView.EditFields.get(0)).fVisible:= false;
     ZCField(EditListView.EditFields.get(0)).fDataType := ZCField.InnerFDataType.ftIinteger;
     //m.IDMaterijal
     ZCField(EditListView.EditFields.get(1)).fReadOnly:= true;
     //m.Materijal
      ZCField(EditListView.EditFields.get(2)).fReadOnly:= true;
     //pl.Kolicina
     ZCField(EditListView.EditFields.get(3)).fDataType := ZCField.InnerFDataType.ftIinteger;

  layout.addView(EditListView);

  getWindow.setSoftInputMode(AVWindowManager.InnerLayoutParams.SOFT_INPUT_ADJUST_NOTHING);
  setContentView(layout);

end;

procedure PopisActivity.onClick(aView: AVView);
var
  cur1: ADCursor;
  dMaterijal: AZCLookUpSQLDialog;
begin
     case aView.getId of
       1: begin
            dMaterijal := nil;
            dMaterijal:= AZCLookUpSQLDialog.create(self, db, JLString('select  MjestoPopisaID, Napomena as MjestoPopisa from MjestoPopisa '), 'Napomena', 'MjestoPopisa');
            dMaterijal.ID:= 2;
            dMaterijal.KeyFieldID := 0;    //return string
            dMaterijal.Caption := 'MjestoPopisaID';
            dMaterijal.LookupKeyFields:= string('');
            dMaterijal.setOnDismissListener(Self);
            dMaterijal.Show;

          end;
       2: begin
            dMaterijal := nil;
            dMaterijal:= AZCLookUpSQLDialog.create(self, db, JLString('select  IDMaterijal, Materijal from sMaterijal '), 'Materijal', 'sMaterijal');
            dMaterijal.ID:= 1;
            dMaterijal.KeyFieldID := 0;    //return string
            dMaterijal.Caption := 'Materijal';
            dMaterijal.LookupKeyFields:= string('');
            dMaterijal.setOnDismissListener(Self);
            dMaterijal.Show;
          end;
     end;
end;

procedure PopisActivity.onItemClick(tAdapter: AWAdapterView; tView: AVView; p1: LongInt; p2: Int64);
begin

end;

function PopisActivity.onItemLongClick(tAdapter: AWAdapterView; tView: AVView;  p1: LongInt; p2: Int64): Boolean;
begin

end;

procedure PopisActivity.afterTextChanged(editable: ATEditable);
var
  lastCharacter: char;
  s: string;
begin
  if editable.length > 1 then begin
    lastCharacter := editable.charAt(editable.length - 1);

    if lastCharacter = #10 then begin
      s:= editable.subSequence(0, editable.length - 1).toString.trim;
      if (FindMaterijal(s, db) = 0) then begin
         NewSif:= true;
         NewIDMaterijal := s;
         DialogMaterijal(s);
      end;

       InsertPopis(s);
      // ShowMessage(Self, JLString(s));
       editable.clear;
    end;
  end;
end;

procedure PopisActivity.onClick(dInterface: ACDialogInterface; p1: LongInt);
var i: integer;
begin
 // inherited onClick(dInterface, p1);
  case p1 of
   -1 : case dialog.ID of
          1:  begin
               if not InsertFields(Self, db, 'sMaterijal', dialog.Fields) then ShowMessage(Self, JLString('Greska!!'));   //insert novi materjal
               InsertPopis(NewIDMaterijal);
          end;
        end;
  end;
  EditListView.RefreshView;
end;

procedure PopisActivity.onDismiss(aInt: ACDialogInterface);
{var
  ini: ZCTIniFile;}
begin
  //inherited onDismiss(aInt);
  case AZCLookUpSQLDialog(aInt).ID of
     1: et.setText(JLstring(AZCLookUpSQLDialog(aInt).LookupKeyFields).concat(string(#10))); //Materijal selekt
     2: begin
          try
           IDMjestoPopisa:= JLInteger.parseInt(AZCLookUpSQLDialog(aInt).LookupKeyFields);
           bMjestoPopisa.setText(JLString(AZCLookUpSQLDialog(aInt).LookupResultField));
         //  ini:= ZCTIniFile.create(IniFileName);
           ini.setInt(JLString('PDAsetings'), JLString('IDMjestoPopisa'), IDMjestoPopisa);

           EditListView.SQLSelect := JLString('select pl.PopisnaListaID, m.IDMaterijal, m.Materijal, pl.Kolicina  from PopisnaLista pl,  sMaterijal m ').concat(
                                'where (pl.IDMaterijal = m.MaterijalID) and (pl.IDMjestoPopisa = ').concat(JLInteger.toString(IDMjestoPopisa).toString.trim).concat
                                (') order by PopisnaListaID desc ');
           EditListView.RefreshView;
          except
          end;
        end; // Izbor radnog mjesta
  end;
end;

procedure PopisActivity.DialogMaterijal(IDMaterijal: String);
var
  tField: ZCField;
begin
  if db.isopen then begin
     dialog := AEditDialog.create(Self);
     dialog.ID:=1;
     dialog.setTitle(JLString('Popravki podatke: ').concat(IDMaterijal));
    // dialog.Fields :=  CreateFields(Self, db, JLString('select IDMaterijal, Materijal from sMaterijal '));

    dialog.Fields := JUArrayList.Create;

      tField := ZCField.create;
      tField.fName := 'IDMaterijal';
      tField.fOldValue := IDMaterijal;
      tField.fValue := IDMaterijal;
      tfield.fDataType := ZCField.InnerFDataType.ftString;
      tField.fReadOnly:= True;
    dialog.Fields.add(tField);

      tField := ZCField.create;
      tField.fName := 'Materijal';
      tField.fOldValue := string('');
      tField.fValue := string('');
      tfield.fDataType := ZCField.InnerFDataType.ftString;
      tField.fCharCase := ZCField.InnerEditCharCase.fUpperCase;
    dialog.Fields.add(tField);

     //------------------------------------

     dialog.setButton(JLString('Yes'), Self);
     dialog.setButton2(JLString('No'), Self);
     dialog.show;
  end;
end;

procedure PopisActivity.InsertPopis(IDMaterijal: String);
var
  values: ACContentValues;
begin
  //odrediti broj fields
   values := ACContentValues.Create(1);
   values.put('IDMaterijal', JLLong.Create(FindMaterijal(IDMaterijal, db)).toString);
   IDMaterijal := '1'; //JLFloat.tostring(TezinaKod(IDMaterijal));
   values.put('Kolicina', IDMaterijal); //TezinaKod(IDMaterijal)); //IDMaterijal);
   values.put('IDMjestoPopisa', JLInteger.toString(IDMjestoPopisa));


  db.beginTransaction;
  try
    db.insert('PopisnaLista', Nil, values);
    db.setTransactionSuccessful;
    db.endTransaction;
  except
  end;


  EditListView.RefreshView;

end;


end.


