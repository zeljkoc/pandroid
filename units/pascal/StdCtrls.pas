unit StdCtrls;

{$mode objfpc}{$H+}

interface

uses
  androidr15;

type
  { TOnclickNotifyEvent }
  TOnClickEvent             = procedure (Value: AVView) of object;
  TOnCreateContextMenuEvent = procedure(aContexMenu: AVContextMenu; aView: AVView; AInfo: AVContextMenu.InnerContextMenuInfo) of object;
  TOnDragEvent              = function (aView: AVView; aDragEvent: AVDragEvent): jboolean of object;
  TonFocusChangeEvent       = procedure (aView: AVView; aBoolean: jboolean) of object;
  TonGenericMotionEvent     = function (aView: AVView; aMotionEvent: AVMotionEvent): jboolean of object;
  TonHoverEvent             = function (para1: AVView; para2: AVMotionEvent): jboolean of object;
  TonKeyEvent               = function (para1: AVView; para2: jint; para3: AVKeyEvent): jboolean of object;
  TonLongClickEvent         = function (para1: AVView): jboolean of object;
  TonSystemUiVisibilityChangeEvent = procedure (para1: jint) of object;
  TonTouchEvent             = function (para1: AVView; para2: AVMotionEvent): jboolean of object;


  TTextView = class(AWTextView)
  public

  end;

  TEditText = class(AWEditText)
  public

  end;

  { TButton }

  TButton = class(AWButton,  AVView.InnerOnClickListener, AVView.InnerOnLongClickListener)
  private
    FOnclick            : TOnclickEvent;
    FonLongClick        : TonLongClickEvent;
  public
    procedure onClick(para1: AVView); overload;
    function onLongClick(para1: AVView): jboolean; overload;
  public
    constructor create(para1: ACContext); overload;
  public
    property onClickListener: TOnclickEvent read FOnClick write FOnClick;
    property OnLongClickListener: TonLongClickEvent read FonLongClick write FonLongClick;
  end;



implementation

{ TButton }

procedure TButton.onClick(para1: AVView);
begin
  if Assigned(FOnclick) then  FOnclick(para1);
end;

function TButton.onLongClick(para1: AVView): jboolean;
begin
  if Assigned(FonLongClick) then Result := FonLongClick(para1)
  else Result:= onLongClick(para1);
end;

constructor TButton.create(para1: ACContext);
begin
  inherited create(para1);
  self.setOnClickListener(Self);
  self.setOnLongClickListener(Self);
end;

end.

