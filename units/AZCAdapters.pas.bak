{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 28-8-15
***********************************************************}
unit AZCAdapters;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses
  Androidr15, AZCFields, dataset;

type

    { AZCDBAdapter }

  { AZCDBListAdapter }

  AZCDBListAdapter = class(AWArrayAdapter)
  private
    fContext: ACContext;
    fCursor: ADCursor;
    fFields: JUArrayList;
  private
    procedure SetCurosr(AValue: ADCursor);
    procedure SetFields(AValue: JUArrayList);
  public
    Constructor create(aContext: ACContext); overload;
    function getView(pos: jint; aView: AVView; aViewGroup: AVViewGroup): AVView; override;
  public
    property Cursor: ADCursor read fCursor write SetCurosr;
    property Fields : JUArrayList read fFields write SetFields;
  end;


implementation


{ AZCDBListAdapter }

procedure AZCDBListAdapter.SetCurosr(AValue: ADCursor);
var
  i: integer;
 tField: ZCField;
begin
    fCursor := AValue;
    clear;
    fCursor.moveToFirst;

    if fFields.size = 0 then
      for i:=0 to fCursor.getColumnCount - 1 do begin
        tField := ZCField.create;
        fFields.add(tField);
      end;

    for i:=0 to fCursor.getCount - 1 do
      add(JLInteger.toString(i));
end;

procedure AZCDBListAdapter.SetFields(AValue: JUArrayList);
var
  i: integer;
begin
  if fFields=AValue then Exit;
  fFields:=AValue;
end;

constructor AZCDBListAdapter.create(aContext: ACContext);
var
  i: integer;
begin
  fContext := aContext;
  inherited create(fContext, AR.Innerlayout.simple_list_item_1);
  fFields:= JUArrayList.create;
end;

function AZCDBListAdapter.getView(pos: jint; aView: AVView; aViewGroup: AVViewGroup): AVView;
var
 lObject: AWTableLayout;
 TableRow: AWTableRow;
 textView: AWTextView;
 i: integer;
begin
   fCursor.moveToPosition(pos);

   lObject:= AWTableLayout.Create(fContext);
   lObject.setOrientation(AWLinearLayout.VERTICAL);

    for i:= 0 to fCursor.getColumnCount - 1 do begin
        if ZCField(fFields.get(i)).fVisible then begin
          TableRow:= AWTableRow.Create(fContext);

           textView:= AWTextView.Create(fContext);
           if  (JLString(ZCField(fFields.get(i)).fDisplayName).length <> 0) then
            textView.setText(JLString(ZCField(fFields.get(i)).fDisplayName).concat(': ').toUpperCase)
           else
            textView.setText(JLString(fCursor.getColumnName(i)).concat(': ').toUpperCase);
           textView.setTypeface(nil, AGTypeface.BOLD);
          TableRow.addView(textView);

           textView:= AWTextView.Create(fContext);
           textView.setText(fCursor.getString(i));
          TableRow.addView(textView);

          lObject.addView(TableRow);
        end;
   end;

  Result := lObject;
end;


end.

