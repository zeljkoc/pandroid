{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit AActivity;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

//uses androidr15;
{$include AndroidVersion.inc}
;


Type

  { Activity }

  Activity = class(AAActivity,
                   AVView.InnerOnAttachStateChangeListener,
                   AVView.InnerOnSystemUiVisibilityChangeListener,
                   AVView.InnerOnClickListener,
                   AVView.InnerOnFocusChangeListener,
                   AVView.InnerOnDragListener,
                   AVView.InnerOnLongClickListener,
                   AVView.InnerOnGenericMotionListener,
                   AVView.InnerOnHoverListener,
                   AVView.InnerOnTouchListener,
                   AVView.InnerOnKeyListener,

                   ATTextWatcher,

                   AWAdapterView.InnerOnItemClickListener,
                   AWAdapterView.InnerOnItemLongClickListener,

                   AWPopupMenu.InnerOnMenuItemClickListener,

                   JLRunnable,

                   ACDialogInterface.InnerOnClickListener,
                   AOHandler.InnerCallback,
                   ACDialogInterface.InnerOnMultiChoiceClickListener,

                   APPreferenceManager.InnerOnActivityResultListener,

                   ABBluetoothProfile.InnerServiceListener,

                   ACDialogInterface.InnerOnDismissListener,
                   ACDialogInterface.InnerOnCancelListener,

                   ALLocationListener,

                   ASTTextToSpeech.InnerOnInitListener,

                   AAActionBar.InnerTabListener)
    public
      //AVView.InnerOnAttachStateChangeListener
      procedure onViewAttachedToWindow(aView: AVView); virtual;
      procedure onViewDetachedFromWindow(aView: AVView);virtual;
      //AVView.InnerOnSystemUiVisibilityChangeListener
      procedure onSystemUiVisibilityChange(p1: LongInt);virtual;
      //AVView.InnerOnClickListener          setOnClickListener(Self);
      procedure onClick(aView: AVView); virtual;
      //AVView.InnerOnFocusChangeListener   setOnFocusChangeListener(Self);
      procedure onFocusChange(aView: AVView; aBoolean: Boolean);virtual;
      //AVView.InnerOnDragListener       setOnDragListener(Self);
      function onDrag(aview: AVView; aDragEvent: AVDragEvent):Boolean;virtual;
      //AVView.InnerOnLongClickListener
      function onLongClick(aView: AVView):Boolean;virtual;
      //AVView.InnerOnGenericMotionListener
      function onGenericMotion(aView: AVView; aMotionEvet: AVMotionEvent):Boolean;virtual;
      //AVView.InnerOnHoverListener
      function onHover(aView: AVView; aMotionEvetn: AVMotionEvent):Boolean;virtual;
      //AVView.innerOnTouchListener
      function onTouch(aView: AVView; aEvent: AVMotionEvent):Boolean;virtual;
      //AVView.InnerOnKeyListener            setOnKeyListener(Self);
      function onKey(aView: AVView; aKeyCode: LongInt; aKeyEvent: AVKeyEvent):Boolean; virtual;
      //ATTextWatcher                        addTextChangedListener(self);
      procedure beforeTextChanged(charSequence: JLCharSequence; start: LongInt; lengthBefore: LongInt; lengthAfter: LongInt); virtual;
      procedure onTextChanged(charSequence: JLCharSequence; start: LongInt; before: LongInt; count: LongInt); virtual;
      procedure afterTextChanged(editable: ATEditable); virtual;
      //AWAdapterView.InnerOnItemClickListener      setOnItemClickListener(Self);
      procedure onItemClick(aAdapter: AWAdapterView; aView: AVView; position: LongInt; id: Int64); virtual;
      //AWAdapterView.InnerOnItemLongClickListener    setOnItemLongClickListener(Self);
      function onItemLongClick(tAdapter:AWAdapterView; tView: AVView; position: LongInt; id:Int64):Boolean; virtual;
      //AWPopupMenu.InnerOnMenuItemClickListener
      function onMenuItemClick(aMenuItem: AVMenuItem):Boolean; virtual;
      //JLRunnable
      procedure Run; virtual;
      //ACDialogInterface.InnerOnClickListener
      procedure onClick(dInterface: ACDialogInterface; p1: LongInt); virtual;
      //AOHandler.InnerCallback,
      function handleMessage(eMessage: AOMessage): Boolean; virtual;
      //ACDialogInterface.InnerOnMultiChoiceClickListener
      procedure onClick(dInterface: ACDialogInterface; p1: LongInt; p2: Boolean); virtual;
      //APPreferenceManager.InnerOnActivityResultListener
      function onActivityResult(requestCode: LongInt; resultCode: LongInt;data: ACIntent):Boolean; virtual;
      //ABBluetoothProfile.InnerServiceListener
      procedure onServiceConnected(profile: LongInt; proxy: ABBluetoothProfile); virtual;
      procedure onServiceDisconnected(profile: LongInt); virtual;
      //ACDialogInterface.InnerOnDismissListener
      procedure onDismiss(aInt: ACDialogInterface); virtual;
      //ACDialogInterface.InnerOnCancelListener
      procedure onCancel(para1: ACDialogInterface); virtual;
      // ALLocationListener
      procedure onLocationChanged(para1: ALLocation); overload; virtual;
      procedure onStatusChanged(para1: JLString; para2: jint; para3: AOBundle); overload; virtual;
      procedure onProviderDisabled(para1: JLString); overload; virtual;
      procedure onProviderEnabled(para1: JLString); overload; virtual;
      //ASTTextToSpeech.InnerOnInitListener
      procedure onInit(status: jint); overload;virtual;
      //AAActionBar.InnerTabListener
      procedure onTabSelected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction); overload; virtual;
      procedure onTabReselected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction); overload; virtual;
      procedure onTabUnselected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction); overload; virtual;
   end;

implementation


//AVView.InnerOnAttachStateChangeListener
procedure Activity.onViewAttachedToWindow(aView: AVView);
begin
end;

procedure Activity.onViewDetachedFromWindow(aView: AVView);
begin
end;

//AVView.InnerOnSystemUiVisibilityChangeListener
procedure Activity.onSystemUiVisibilityChange(p1: LongInt);
begin
end;

//AVView.InnerOnClickListener
procedure Activity.onClick(aView: AVView);
begin
end;

//AVView.InnerOnFocusChangeListener
procedure Activity.onFocusChange(aView: AVView; aBoolean: Boolean);
begin
end;

//AVView.InnerOnDragListener
function Activity.onDrag(aview: AVView; aDragEvent: AVDragEvent):Boolean;
begin
end;

//AVView.InnerOnLongClickListener
function Activity.onLongClick(aView: AVView):Boolean;
begin
end;

//AVView.InnerOnGenericMotionListener
function Activity.onGenericMotion(aView: AVView; aMotionEvet: AVMotionEvent):Boolean;
begin
end;

//AVView.InnerOnHoverListener
function Activity.onHover(aView: AVView; aMotionEvetn: AVMotionEvent):Boolean;
begin
end;

//AVView.innerOnTouchListener
function Activity.onTouch(aView: AVView; aEvent: AVMotionEvent):Boolean;
begin
end;

//AVView.InnerOnKeyListener
function Activity.onKey(aView: AVView; aKeyCode: LongInt; aKeyEvent: AVKeyEvent):Boolean;
begin
end;

//ATTextWatcher
procedure Activity.beforeTextChanged(charSequence: JLCharSequence;
  start: LongInt; lengthBefore: LongInt; lengthAfter: LongInt);
begin

end;

procedure Activity.onTextChanged(charSequence: JLCharSequence; start: LongInt;
  before: LongInt; count: LongInt);
begin

end;

procedure Activity.afterTextChanged(editable: ATEditable);
begin

end;

//AWAdapterView.InnerOnItemClickListener
procedure Activity.onItemClick(aAdapter: AWAdapterView; aView: AVView;
  position: LongInt; id: Int64);
begin

end;

//AWAdapterView.InnerOnItemLongClickListener
function Activity.onItemLongClick(tAdapter: AWAdapterView; tView: AVView;
  position: LongInt; id: Int64): Boolean;
begin

end;

function Activity.onMenuItemClick(aMenuItem: AVMenuItem): Boolean;
begin

end;

//JLRunnable
procedure Activity.Run;
begin

end;

//ACDialogInterface.InnerOnClickListener
procedure Activity.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin

end;

//AOHandler.InnerCallback,
function Activity.handleMessage(eMessage: AOMessage): Boolean;
begin

end;

//ACDialogInterface.InnerOnMultiChoiceClickListener
procedure Activity.onClick(dInterface: ACDialogInterface; p1: LongInt;
  p2: Boolean);
begin

end;

//APPreferenceManager.InnerOnActivityResultListener
function Activity.onActivityResult(requestCode: LongInt; resultCode: LongInt;
  data: ACIntent): Boolean;
begin

end;

//ABBluetoothProfile.InnerServiceListener
procedure Activity.onServiceConnected(profile: LongInt;
  proxy: ABBluetoothProfile);
begin

end;

procedure Activity.onServiceDisconnected(profile: LongInt);
begin

end;

procedure Activity.onDismiss(aInt: ACDialogInterface);
begin

end;

procedure Activity.onCancel(para1: ACDialogInterface);
begin

end;

// ALLocationListener
procedure Activity.onLocationChanged(para1: ALLocation);
begin

end;

procedure Activity.onStatusChanged(para1: JLString; para2: jint; para3: AOBundle
  );
begin

end;

procedure Activity.onProviderDisabled(para1: JLString);
begin

end;

procedure Activity.onProviderEnabled(para1: JLString);
begin

end;

procedure Activity.onInit(status: jint);
begin

end;

procedure Activity.onTabSelected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction);
begin

end;

procedure Activity.onTabReselected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction);
begin

end;

procedure Activity.onTabUnselected(para1: AAActionBar.InnerTab; para2: AAFragmentTransaction);
begin

end;


end.

