{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 

unit Fragment;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.fragment01}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity, StdCtrls ;

 

type

  { TTabFragment }

  TTabFragment = class(AAFragment)
  public
    function onCreateView(para1: AVLayoutInflater; para2: AVViewGroup; para3: AOBundle): AVView; overload; override;
  end;

  { TTabFragment2 }

  TTabFragment2 = class(AAFragment)
  public
    function onCreateView(para1: AVLayoutInflater; para2: AVViewGroup; para3: AOBundle): AVView; overload; override;
  end;


  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    //AAActionBar.InnerTabListener
    procedure onTabSelected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction); override;
  public
     FragmentTransaction: AAFragmentTransaction;
     Fragment1: TTabFragment;
     Fragment2: TTabFragment2;

     actionbar : AAActionBar;
  end;

implementation

uses Dialogs;


{ TTabFragment2 }

function TTabFragment2.onCreateView(para1: AVLayoutInflater; para2: AVViewGroup; para3: AOBundle): AVView;
var
  layout2 : AWLinearLayout;
   bt: TButton;
   i: integer;
begin
  layout2:= AWLinearLayout.Create(para1.getContext);
  layout2.setOrientation(AWLinearLayout.VERTICAL);

  for i:=0 to 5 do begin
   bt:= TButton.create(para1.getContext);
   bt.Text := JLString('Button 2');
   layout2.addView(bt);
  end;

  Result := layout2;
end;

{ TTabFragment }

function TTabFragment.onCreateView(para1: AVLayoutInflater; para2: AVViewGroup; para3: AOBundle): AVView;
var
  layout : AWLinearLayout;
   tv: TTextView;
   i: integer;
begin
  layout:= AWLinearLayout.Create(para1.getContext);
  layout.setOrientation(AWLinearLayout.VERTICAL);

  for i:= 0 to 5 do begin
    tv:= TTextView.create(para1.getContext);
    tv.Text := JLString('fragment');
   layout.addView(tv);
  end;

  Result := layout;
end;


{ --------------- MainActivity }

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
begin
  inherited onCreate(savedInstanceState);

  getActionBar.setNavigationMode(AAActionBar.NAVIGATION_MODE_TABS);
  getActionBar.setDisplayShowTitleEnabled(true);
//  getActionBar.setDisplayHomeAsUpEnabled(true);

  getActionBar.addTab(getActionBar.newTab.setText(JLString('jedan')).setTabListener(Self));
  getActionBar.addTab(getActionBar.newTab.setText(JLString('dva')).setTabListener(Self));
end;


procedure MainActivity.onTabSelected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction);
begin
  case string(para1.getText.toString)  of
   'jedan': begin
       FragmentTransaction := getFragmentManager.beginTransaction;
       Fragment1:= TTabFragment.create;
       FragmentTransaction.replace(AR.Innerid.content, Fragment1);
       FragmentTransaction.setTransition(AAFragmentTransaction.TRANSIT_FRAGMENT_FADE);
       FragmentTransaction.commit;
      end;
   'dva': begin
       FragmentTransaction := getFragmentManager.beginTransaction;
       Fragment2:= TTabFragment2.create;
       FragmentTransaction.replace(AR.Innerid.content, Fragment2);
       FragmentTransaction.setTransition(AAFragmentTransaction.TRANSIT_FRAGMENT_CLOSE);
       FragmentTransaction.commit;
      end;
  end;
end;


end.
