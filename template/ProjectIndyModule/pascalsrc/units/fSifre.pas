{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)                    *}
{******************************************}
unit fSifre;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface
//uses androidr15
{$include /usr/local/pandroid/units/AndroidVersion.inc}
, AActivity, DB, Dialogs;

type

  { TFormSifre }

  TFormSifre = class(Activity)
  public
     procedure onCreate(savedInstanceState:AOBundle); override;
     procedure onClick(aView: AVView); override;
     //
     function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public
    DBGridView: TDBGridViewLayout;
    Dialog: TDialog;
  end;



implementation

uses StdCtrls, Data, AZCForms, Rjava;

{ TFormSifre }

procedure TFormSifre.onCreate(savedInstanceState: AOBundle);
var
 layout: AZCForm;
begin
  inherited onCreate(savedInstanceState);
  setTitle(JLString('Exit'));
  layout := AZCForm.create(Self, 0, 'Copyright(c) Zeljko Cvijanovic');

   getActionBar.setDisplayHomeAsUpEnabled(true);
   getActionBar.setIcon(R.drawable.exit);

     layout.addButton(Self, 1, 'Partneri');

     DBGridView := TDBGridViewLayout.create(Self, dBase);
     DBGridView.Adapter.CreateView := @SifrarniciListView;
     DBGridView.ReadOnlyEdit := True;
     DBGridView.ReadOnlyDelete := True;

     Dialog:= TDialog.create(Self);
     Dialog.AddButton(btPositive, JLString('<<'));

     Dialog.setView(DBGridView);

  setContentView(layout);
end;

procedure TFormSifre.onClick(aView: AVView);
begin
  case aView.Id of
      1: begin
           DBGridView.Adapter.CursorDataSet.SQLSelect := JLString('select PartnerID, Partner from sPartner ');
           Dialog.setTitle(JLString('Partneri'));
         end;


      10: begin Finish; Exit; end;
    end;
    Dialog.show;
end;

function TFormSifre.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
  Result := true;
   case item.getItemID of
     AR.Innerid.home: onBackPressed;  // exit na prethodni meni
   else
          Result := false;
  end;
end;

end.

