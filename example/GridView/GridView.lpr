{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/GridView/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit GridView;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.gridview}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity;


type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onItemClick(aAdapter: AWAdapterView; aView: AVView; position: LongInt; id: Int64); override;
  public
    items: JUList;
  public

  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;
 params: AWLinearLayout.InnerLayoutParams;
 GridView: AWGridView;
 Adapter: AWArrayAdapter;

 i: integer;
begin
  inherited onCreate(savedInstanceState);
  layout:= AWLinearLayout.Create(self);
  params:= AWLinearLayout.InnerLayoutParams.Create(
              AWLinearLayout.InnerLayoutParams.FILL_PARENT ,
              AWLinearLayout.InnerLayoutParams.WRAP_CONTENT);
  layout.setOrientation(AWLinearLayout.VERTICAL);

   items:= JUArrayList.Create;
   for i:=0 to 40 do
    items.add(JLString('ID-'+JLInteger.toString(i)));


  GridView:= AWGridView.Create(Self);
  Adapter := AWArrayAdapter.create(Self, AR.innerLayout.simple_list_item_1, items);

  GridView.setVerticalSpacing(10);
  GridView.setHorizontalSpacing(10);
  GridView.setNumColumns(AWGridView.AUTO_FIT);
  //GridView.setStretchMode(AR.Innerattr.COLUMNWIDTH);
  GridView.setColumnWidth(90);
  GridView.setOnItemClickListener(Self);
  GridView.setAdapter(Adapter);

  layout.addView(GridView);
  setContentView(layout);
end;

procedure MainActivity.onItemClick(aAdapter: AWAdapterView; aView: AVView;  position: LongInt; id: Int64);
begin
  AWToast.makeText(self,JLString('Select: '+ items.get(id).toString) ,AWToast.LENGTH_SHORT).show();
end;


end.
