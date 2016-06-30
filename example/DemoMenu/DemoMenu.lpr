{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoMenu/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoMenu;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demomenu}

interface

uses androidr15, Rjava, AActivity;


type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public

  end;

implementation

uses AZCForms, AZCDialogs;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
layout : AZCForm; 
begin
  inherited onCreate(savedInstanceState);

  layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic');   

  
  layout.addButton(Self, 10, 'Exit');

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);  
begin
 case aView.getId of
      10: Finish;
 end;
         
end;


//-------------------------------------
function MainActivity.onCreateOptionsMenu(menu: AVMenu): JBoolean;
var
 MenuItem : AVMenuItem;
 SubMenu : AVSubMenu; 
begin
  inherited onCreateOptionsMenu(menu);

  SubMenu := menu.addSubMenu(0, 0, 0, JLString(string('Menu')));
  SubMenu.add(0, 1, 0, JLString('Item 1')).setIcon(R.drawable.ic_next);
  SubMenu.add(0, 2, 0, JLString('Item 2')).setIcon(R.drawable.ic_next);
  SubMenu.add(0, 3, 0, JLString('Item 3')).setIcon(R.drawable.ic_next);


  MenuItem := SubMenu.getItem;
  MenuItem.setIcon(r.drawable.ic_menu); 
  MenuItem.setShowAsAction(AVMenuItem.SHOW_AS_ACTION_ALWAYS);   

  Result := true;
end;

function MainActivity.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
   Result := true;
   case item.getItemID of
      1: ShowMessage(Self, JLString('Menu Item 1'));
      2: ShowMessage(Self, JLString('Menu Item 2'));
      3: ShowMessage(Self, JLString('Menu Item 3'));

      else Result := false;
   end;
end;



end.
