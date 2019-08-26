{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit fPregledi;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

//uses androidr15,
{$include ../AndroidVersion.inc}
, AActivity, DB, Dialogs;

type

   { TFormReview }

   TFormPregledi = class(Activity)
      procedure onCreate(savedInstanceState: AOBundle); override;
   //   procedure onClick(aView: AVView); override;
      //
      function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
      //
      procedure ItemClickListener (para1: AWAdapterView; para2: AVView; para3: jint; para4: jlong);
    public
      intent: ACIntent;
      ReviewMeni: TDBGridViewLayout;
      DBGridView: TDBGridViewLayout;
      Dialog: TDialog;
    end;

   function PregledMeniView(aContext: ACContext; aField: TFieldDef): AVView;
   function PregledListView(aContext: ACContext; aField: TFieldDef): AVView;

implementation
uses StdCtrls, Data,  Rjava;

function PregledMeniView(aContext: ACContext; aField: TFieldDef): AVView;
var
    layout: AWLinearLayout;
begin
  layout:= AWLinearLayout.Create(aContext);
  layout.setGravity(AVGravity.CENTER);

    layout.addView(AWImageView.create(aContext));
    with AWImageView(layout.getChildAt(layout.getChildCount - 1)) do begin
      setImageResource(R.drawable.notes);
    end;

    layout.addView(TTextView.create(aContext));
    with TTextView(layout.getChildAt(layout.getChildCount - 1)) do begin
       Text :=   JLString('  ').concat(aField.Value[1].AsString);
       setTextAppearance(aContext, AR.Innerstyle.TextAppearance_Medium);
    end;

  Result := layout;
end;

function PregledListView(aContext: ACContext; aField: TFieldDef): AVView;
var
   layout: AWLinearLayout;
   i: jint;
begin
     layout:= AWLinearLayout.Create(aContext);
     layout.setOrientation(AWLinearLayout.VERTICAL);

     for i:=0 to aField.FieldCount - 1 do begin
        layout.addView(TTextView.create(aContext));
        with TTextView(layout.getChildAt(layout.getChildCount - 1)) do begin
            Text := JLString(aField.DisplayName[i].trim).concat(': ').concat(aField.Value[i].AsString.trim);
            setGravity(AVGravity.LEFT);
            if i = 0 then begin
              setTypeface(nil, AGTypeface.BOLD);
           end;
        end;
     end;

     //linija ispod
     layout.addView(TTextView.create(aContext));
     TTextView(layout.getChildAt(layout.getChildCount - 1)).setBackgroundColor(AGColor.RED);
     TTextView(layout.getChildAt(layout.getChildCount - 1)).Height := 1;

     Result := layout;
end;

{ TFormPregled }

procedure TFormPregledi.onCreate(savedInstanceState: AOBundle);
var
 layout : AWLinearLayout;
begin
  inherited onCreate(savedInstanceState);
    setTitle(JLString('  Exit'));
    layout:= AWLinearLayout.create(Self);
    layout.setOrientation(AWLinearLayout.VERTICAL);

    getActionBar.setDisplayHomeAsUpEnabled(true);
    getActionBar.setIcon(R.drawable.exit);

     ReviewMeni:= TDBGridViewLayout.create(self, dBase);
     with ReviewMeni do begin                                  //0           1          2
        Adapter.CursorDataSet.SQLSelect := JLString('select PDAPregledID, PregledMeni, SQLSelect from PDAPregled ');
        Adapter.CreateView :=  @PregledMeniView;
        Adapter.CursorDataSet.TableName := 'PDAPregled';
        GridView.setNumColumns(1);  // ini file
        ReadOnlyDelete := true;
        ReadOnlyEdit := true;
        onItemClickListener:= @ItemClickListener;
     end;
     layout.addView(ReviewMeni);

     DBGridView := TDBGridViewLayout.create(Self, dBase);
     DBGridView.Adapter.CreateView := @PregledListView;
     DBGridView.ReadOnlyEdit := True;
     DBGridView.ReadOnlyDelete := True;

     Dialog:= TDialog.create(Self);
     Dialog.AddButton(btPositive, JLString('<<'));

     Dialog.setView(DBGridView);

   setContentView(layout);
end;

function TFormPregledi.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
   Result := true;
    case item.getItemID of
      AR.Innerid.home: onBackPressed;  // exit na prethodni meni
    else
           Result := false;
   end;
end;

procedure TFormPregledi.ItemClickListener(para1: AWAdapterView; para2: AVView;
  para3: jint; para4: jlong);
begin
    ReviewMeni.Adapter.CursorDataSet.Index := para3;
    DBGridView.Adapter.CursorDataSet.SQLSelect := ReviewMeni.Adapter.CursorDataSet.Field.Value[2].AsString;
    Dialog.setTitle(ReviewMeni.Adapter.CursorDataSet.Field.Value[1].AsString);
    Dialog.show;
end;

end.

