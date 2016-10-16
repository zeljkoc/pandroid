{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit AZCListView;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15, Rjava, AZCAdapters, ADBDataBase, AZCDialogs, AZCScrollButons;

type

  { AEditListView }

  AEditListView = class(AWLinearLayout,
                        AWAdapterView.InnerOnItemLongClickListener,
                        AWAdapterView.InnerOnItemClickListener,
                        ACDialogInterface.InnerOnClickListener,
                        AVView.InnerOnClickListener)
  private
    fContext : ACContext;
    fListView: AWListView;
    fAdapter:  AZCDBListAdapter;
    fEditFields: JUArrayList;
    fDataBase: ASQLDatabase;
    fSQLSelect: JLString;
    fItemNo: jint;
    fDeleteItem : JLstring;
    fTableName: JLString;
    fActiveDialog: boolean;
    fZCScroolButton: AZCScroolButton;
    const
      DIALOG_YES_NO = 1;
      DIALOG_EDIT = 2;
      DIALOG_INSERT = 3;
  private
   procedure SetAdapter(AValue: AZCDBListAdapter);
   procedure SetEditFields(AValue: JUArrayList);
  strict protected
   procedure InsertDialog;
  public
   function onItemLongClick(para1: AWAdapterView; para2: AVView; para3: jint; para4: jlong): jboolean; overload;
   procedure onItemClick(para1: AWAdapterView; para2: AVView; para3: jint; para4: jlong); overload;
   procedure onClick(para1: ACDialogInterface; para2: jint); overload;
   //AVView.InnerOnClickListener
   procedure onclick(aView: AVView); overload;
  public
   constructor create(para1: ACContext; aDataBase: ASQLDatabase; aSQLSelect: JLString; aNavigationButton: boolean = false); overload;
   procedure RefreshView;
  public
   property SQLSelect: JLstring read fSQLSelect write fSQLSelect;
   property EditFields: JUArrayList read fEditFields write SetEditFields;
   property TableName : JLString read fTableName write fTableName;
   property ListAdapter: AZCDBListAdapter read fAdapter write SetAdapter;
  end;


implementation

uses dataset;

{ AEditListView }

procedure AEditListView.SetAdapter(AValue: AZCDBListAdapter);
begin
  if fAdapter=AValue then Exit;
  fAdapter:=AValue;
  RefreshView; // fAdapter.Cursor := fDataBase.rawQuery(fSQLSelect, nil);
end;

procedure AEditListView.SetEditFields(AValue: JUArrayList);
begin
  if fEditFields=AValue then Exit;
  fEditFields:=AValue;

end;

procedure AEditListView.InsertDialog;
var
  iDialog: AEditDialog;
  i: integer;
begin
  if not fActiveDialog then begin

      iDialog := AEditDialog.create(fContext);
      iDialog.Fields := fEditFields;

      for i:=0 to fAdapter.Cursor.getColumnCount() - 1 do begin
         ZCfield(iDialog.Fields.get(i)).fOldValue := string('');
         ZCfield(iDialog.Fields.get(i)).fValue := string('');
      end;

      iDialog.ID:= DIALOG_INSERT;
      iDialog.setTitle(JLString('INSERT  '));
      iDialog.setButton(JLString('Save'), Self);
      iDialog.setButton2(JLString('Cancel'), Self);

      iDialog.show;
      fActiveDialog := true;
  end;

end;

function AEditListView.onItemLongClick(para1: AWAdapterView; para2: AVView; para3: jint; para4: jlong): jboolean;   //DIALOG DELETE
var
  dialogyn: AZCDialog;
  i: integer;
  tv: AWTextView;
begin
  if not fActiveDialog then begin
    fItemNo:= para3;
    fAdapter.Cursor.moveToPosition(fItemNo);

    fDeleteItem  := string('');
    for i:=0 to fAdapter.Cursor.getColumnCount -1 do begin
      if ZCField(fAdapter.Fields.get(i)).fVisible then
        fDeleteItem  := JLString(fDeleteItem ).concat(fAdapter.Cursor.getColumnName(i))
                              .concat(' : ')
                              .concat(fAdapter.Cursor.getString(i))
                              .concat(string(#10));
    end;

      tv := AWTextView.create(fContext);
      tv.setText(JLString(fDeleteItem));

    dialogyn:= AZCDialog.create(fContext);
    dialogyn.ID:= DIALOG_YES_NO;
    dialogyn.setTitle(JLString(' BRISANJE: '));
    dialogyn.setView(tv);
    dialogyn.setMessage(JLString(' Jeste li sigurni? '));

    dialogyn.setButton(JLString('YES'), Self);     // -1
    dialogyn.setButton2(JLString('No'), Self);    // -2
    dialogyn.show;
    fActiveDialog := true;
  end;

end;

procedure AEditListView.onItemClick(para1: AWAdapterView; para2: AVView; para3: jint; para4: jlong);     //DIALOG EDIT
var
  eDialog: AEditDialog;
  i: integer;
begin
  if not fActiveDialog then begin
      fItemNo:= para4;
      fAdapter.Cursor.moveToPosition(fItemNo);

      eDialog := AEditDialog.create(fContext);
      eDialog.Fields := fEditFields;
      for i:=0 to fAdapter.Cursor.getColumnCount() - 1 do begin
         ZCfield(eDialog.Fields.get(i)).fOldValue := fAdapter.Cursor.getString(i).toString;
         ZCfield(eDialog.Fields.get(i)).fValue := fAdapter.Cursor.getString(i).toString;
      end;

      eDialog.ID:= DIALOG_EDIT;
      eDialog.setTitle(JLString('EDIT: '));
      eDialog.setButton(JLString('Save'), Self);
      eDialog.setButton2(JLString('Cancel'), Self);

      eDialog.show;
      fActiveDialog := true;
  end;
end;

procedure AEditListView.onClick(para1: ACDialogInterface; para2: jint);
begin
  if (para1 is AZCDialog) then  begin  //DIALOG_YES_NO
    case para2 of
        -1: begin
             fAdapter.Cursor.moveToPosition(fItemNo);
             fDataBase.delete(fTableName, JLString(fAdapter.Cursor.getColumnName(0)).concat(' = ').concat(fAdapter.Cursor.getString(0)), nil);
              ShowMessage(fContext, JLSTring('OBRISANO: ').concat(string(#10)).concat(fDeleteItem ));
           end;
    end;
   AZCDialog(para1) := nil;
  end;

  if (para1 is AEditDialog) then begin  //DIALOG_EDIT
   case para2 of
        -1: begin
           case AEditDialog(para1).ID of
            DIALOG_EDIT:  begin
                  fAdapter.Cursor.moveToPosition(fItemNo);
                   DBEditFields(fContext, fDataBase, fTableName, AEditDialog(para1).Fields);
               end;
            DIALOG_INSERT : begin
                DBInsertFields(fContext, fDataBase, fTableName, AEditDialog(para1).Fields);
              end;
           end;
        end;
   end;
    AEditDialog(para1) := nil;
  end;

  fActiveDialog := false;
  RefreshView;
end;

procedure AEditListView.onclick(aView: AVView);
begin
  case aView.getID of
       0: begin   // |< - first
           fListView.smoothScrollToPosition(0);
          end;
       3: begin   // >| - Last
           fListView.smoothScrollToPosition(fListView.getCount - 1);
          end;
       4: begin   // + - Insert
            InsertDialog;
          end;
  end;
end;

procedure AEditListView.RefreshView;
begin
  fAdapter.Cursor := fDataBase.rawQuery(fSQLSelect, nil);
  fListView.invalidateViews;
  fListView.scrollBy(0, 0);
end;


constructor AEditListView.create(para1: ACContext; aDataBase: ASQLDatabase; aSQLSelect: JLString; aNavigationButton: boolean = false);
var
  bt: AWImageButton;
begin
  fContext := para1;
  inherited create(fContext);
  setOrientation(AWLinearLayout.VERTICAL);

  fDataBase := aDataBase;
  fSQLSelect := aSQLSelect;

      if aNavigationButton then begin
        fZCScroolButton:= AZCScroolButton.create(fContext);

          bt:= AWImageButton.create(fContext);
          bt.setId(0);
          bt.setImageResource(R.drawable.ic_firstrecord);
          bt.setOnClickListener(self);
        fZCScroolButton.Buttons.add(bt);

          bt:= AWImageButton.create(fContext);
          bt.setId(3);
          bt.setImageResource(R.drawable.ic_lastrecor);
          bt.setOnClickListener(Self);
        fZCScroolButton.Buttons.add(bt);

          bt:= AWImageButton.create(fContext);
          bt.setId(4);
          bt.setImageResource(R.drawable.ic_add);
          bt.setOnClickListener(Self);
        fZCScroolButton.Buttons.add(bt);


        fZCScroolButton.RefreshView;
      addView(fZCScroolButton);
     end;


    fListView:= AWListView.create(fContext);
    fListView.setOnItemClickListener(self);
    fListView.setOnItemLongClickListener(self);
  addView(fListView);

  fAdapter := AZCDBListAdapter.create(fContext);
  fAdapter.Cursor := fDataBase.rawQuery(fSQLSelect, nil);
  fListView.setAdapter(fAdapter);

  fActiveDialog := false;

end;





end.

