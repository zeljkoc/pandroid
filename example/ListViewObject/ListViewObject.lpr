{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/ListViewObject/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit ListViewObject;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.listviewobject}

interface

uses androidr15, Rjava, AActivity;

type

    { AZCArrayAdapter }

    AZCArrayAdapter = class(AWArrayAdapter)
      fContext: ACContext;
    public
      constructor create(para1: ACContext; para2: jint; para3: JUList); overload;
      function getView(para1: jint; aView: AVView; aViewGroup: AVViewGroup): AVView;  override;
    end;

  { MainActivity }

  MainActivity = class(Activity)
    procedure onCreate(savedInstanceState: AOBundle); override;
  public
    lv: AWListView;
    list : JUList;
    adapter :  AZCArrayAdapter;
  end;

implementation


{ AZCArrayAdapter }

constructor AZCArrayAdapter.create(para1: ACContext; para2: jint; para3: JUList);
begin
  fContext := para1;
  inherited create(fContext,  para2, para3);
end;

function AZCArrayAdapter.getView(para1: jint; aView: AVView;
  aViewGroup: AVViewGroup): AVView;
var
 lObject: AWLinearLayout;
 bt: AWButton;
 tv: AWTextView;
begin
   lObject:= AWLinearLayout.Create(fContext);
   lObject.setOrientation(AWLinearLayout.VERTICAL);

     bt := AWButton.create(fContext);
     bt.setText(JLString('Ok'));
     bt.setId(1);
   lObject.addView(bt);

     tv:= AWTextView.create(fContext);
     tv.setText(JLString('PrviTekst'));
   lObject.addView(tv);

  Result := lObject;
end;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;
 bt: AWButton;
 tv: AWTextView;
 i : integer;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(Self);
  layout.setOrientation(AWLinearLayout.VERTICAL);


    list := JUArrayList.create;

    for i:= 0 to 10 do begin
      //list.add(i);

      list.add(i, JLString('Miki - ').concat(JLInteger.toString(i)));
    end;

    adapter :=  AZCArrayAdapter.create(self, AR.Innerlayout.simple_list_item_1, list );

    lv := AWListView.create(Self);
    lv.setAdapter(adapter);
    lv.setOnItemClickListener(Self);
    lv.setOnItemLongClickListener(Self);
  layout.addView(lv);
  setContentView(layout);
end;


end.


