{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 28-8-15
***********************************************************}
unit AZCForms;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}


interface

//uses
//  androidr15, StdCtrls;

{$include AndroidVersion.inc}
, StdCtrls;

type

  { AZCForm }
  AZCForm = class(AWRelativeLayout)
  private
    FHTMLTemplate: String;
    m_panel : AWLinearLayout;
  public
    constructor create(aContext: ACContext; aResource: jint; aFooterText: JLString); overload;
    procedure addButton(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aButtonText: String); overload; virtual;
    procedure addButtonImage(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aButtonText: String; aImageResuource: Jint); overload; virtual;
    procedure addLine(aColor: jint = AGColor.RED); overload; virtual;
    procedure addText(aText: JLstring); overload; virtual;
    procedure setView(aView: AVView); overload; virtual;

    property HTMLTemplate: String read FHTMLTemplate write FHTMLTemplate;
  end;

  { AZCHorizontalForm }

  AZCHorizontalForm = class(AWLinearLayout)
  private
    fLayout: AWLinearLayout;
    fRowLayout : AWLinearLayout;
    fWidth: integer;
  public
    constructor create(aContext: ACContext; aWidth: integer = 55); overload;
    procedure InsertTableRow;
    procedure addTableRow;
    procedure addCurrenView(aView: AVView);
    procedure addTextView(aTextViewID: Integer; aTextView: String; aMinWidth: integer = 0); overload; virtual;
    procedure addButton(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aButtonText: String); overload; virtual;
    procedure addImageButton(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aResource: jint); overload; virtual;
    procedure addLookupEdit(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aViewText, aEditText: String); overload; virtual;
    procedure addLine(aColor: jint; aWidthLine: integer); overload; virtual;
  end;

implementation

{ AZCHorizontalForm }

uses AZCToolBar;

constructor AZCHorizontalForm.create(aContext: ACContext;  aWidth: integer = 55);
var
  fHorizontalScrollView : AWHorizontalScrollView;
  fScrollView: AWScrollView;
begin
  inherited Create(aContext);
  fWidth:= aWidth;
    fScrollView := AWScrollView.create(getContext);
      fHorizontalScrollView := AWHorizontalScrollView.create(getContext);
         fLayout:= AWLinearLayout.create(getContext());
         fLayout.setOrientation(AWLinearLayout.VERTICAL);


        fHorizontalScrollView.addView(fLayout);
    fScrollView.addView(fHorizontalScrollView);
  addView(fScrollView);
end;

procedure AZCHorizontalForm.InsertTableRow;
begin
  fRowLayout := AWLinearLayout.create(getContext);
  fRowLayout.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(
     AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth))); // fAWLinearLayout.InnerLayoutParams.WRAP_CONTENT )));;
end;

procedure AZCHorizontalForm.addTableRow;
begin
  fLayout.addView(fRowLayout);
end;

procedure AZCHorizontalForm.addCurrenView(aView: AVView);
begin
  fRowLayout.addView(aView, AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
end;


procedure AZCHorizontalForm.addTextView(aTextViewID: Integer; aTextView: String; aMinWidth: integer = 0);
var
  tv: AWTextView;
begin
     tv:= AWTextView.create(getContext);
     tv.setId(aTextViewID);
     tv.setText(JLString(aTextView));
     if aMinWidth <> 0 then tv.setMinWidth(aMinWidth);
     tv.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
   fRowLayout.addView(tv);
end;

procedure AZCHorizontalForm.addButton(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aButtonText: String);
var
  b: AWButton;
begin
   b:= AWButton.create(getContext);
   b.setID(aButtonID);
   b.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
   b.setText(JLString(aButtonText));
   b.setOnClickListener(aOnClickListener);
 fRowLayout.addView(b);
end;

procedure AZCHorizontalForm.addImageButton(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aResource: jint);
var
  b: AWImageButton;
begin
     b:= AWImageButton.create(getContext);
     b.setID(aButtonID);
     b.setImageResource(aResource);
     b.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
     b.setOnClickListener(aOnClickListener);
  fRowLayout.addView(b);
end;

procedure AZCHorizontalForm.addLookupEdit(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aViewText, aEditText: String);
var
  b: AWButton;
 tv: AWTextView;
begin
     tv := AWTextView.create(getContext);
     tv.setId(aButtonID);
     tv.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
     tv.setText(JLString(aViewText).concat(': '));
  //   tv.settex
   fRowLayout.addView(tv);

    tv := AWTextView.create(getContext);
    tv.setId(aButtonID);
    tv.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
    tv.setText(JLString(aEditText).concat('  '));
   fRowLayout.addView(tv);

     b:= AWButton.create(getContext);
     b.setID(aButtonID);
     b.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.WRAP_CONTENT, fWidth )));
     b.setText(JLString('...'));
     b.setOnClickListener(aOnClickListener);
   fRowLayout.addView(b);
end;

procedure AZCHorizontalForm.addLine(aColor: jint; aWidthLine: integer);
var
  line: AVView;
begin
     line:= AVView.create(getContext);
     line.setBackgroundColor(aColor);
   fLayout.addView(line, AVViewGroup_LayoutParams.create(AVViewGroup_LayoutParams.FILL_PARENT, aWidthLine));
end;



{ AZCForm }

constructor AZCForm.create(aContext: ACContext; aResource: jint; aFooterText: JLString);
var
  ibMenu: AWRelativeLayout;
  topParams: AWRelativeLayout.InnerLayoutParams;

  idTopLayout,
  ibMenuPadding: integer;

  image: AWImageView;
  lpcTV: AWRelativeLayout.InnerLayoutParams;

  ibMenuBot: AWRelativeLayout;
  botParams: AWRelativeLayout.InnerLayoutParams;

  cTVBot: AWTextView;
  lpcTVBot: AWRelativeLayout.InnerLayoutParams;

  midLayout: AWLinearLayout;
  midParams: AWRelativeLayout.InnerLayoutParams;

  vscroll : AWScrollView;

begin
  inherited Create(aContext);
  setLayoutParams(AVViewGroup_LayoutParams(AWRelativeLayout.InnerLayoutParams.create(AWRelativeLayout.InnerLayoutParams.FILL_PARENT, AWRelativeLayout.InnerLayoutParams.FILL_PARENT)));
  setGravity(AVGravity.FILL);

     // +++++++++++++ TOP COMPONENT: the header
     idTopLayout:=1;
     ibMenu := AWRelativeLayout.create(getContext);
     ibMenu.setId(idTopLayout);
     //ibMenu.setBackgroundDrawable(getResources().getDrawable(aResourceLine));
     ibMenuPadding := 6;
     ibMenu.setPadding(ibMenuPadding, ibMenuPadding, ibMenuPadding, ibMenuPadding);

     topParams:= AWRelativeLayout.InnerLayoutParams.create(AWRelativeLayout.InnerLayoutParams.FILL_PARENT, AWRelativeLayout.InnerLayoutParams.WRAP_CONTENT);
     topParams.addRule(AWRelativeLayout.ALIGN_PARENT_TOP);
  addView(ibMenu, AVViewGroup_LayoutParams(topParams));

        // textview in ibMenu : card holder
        image:= AWImageView.Create(getContext);
        image.setImageResource(aResource);
        lpcTV:= AWRelativeLayout.InnerLayoutParams.create(AWRelativeLayout.InnerLayoutParams.WRAP_CONTENT, AWRelativeLayout.InnerLayoutParams.WRAP_CONTENT);
        lpcTV.addRule(AWRelativeLayout.CENTER_IN_PARENT);
      ibMenu.addView(image, AVViewGroup_LayoutParams(lpcTV));

      // +++++++++++++ BOTTOM COMPONENT: the footer
      ibMenuBot:= AWRelativeLayout.create(getContext);
      ibMenuBot.setId(2);
      //ibMenuBot.setBackgroundDrawable(getResources().getDrawable(aResourceLine));
      ibMenuBot.setPadding(ibMenuPadding,ibMenuPadding,ibMenuPadding,ibMenuPadding);
      botParams:= AWRelativeLayout.InnerLayoutParams.create(AWRelativeLayout.InnerLayoutParams.FILL_PARENT, AWRelativeLayout.InnerLayoutParams.WRAP_CONTENT);
      botParams.addRule(AWRelativeLayout.ALIGN_PARENT_BOTTOM);
  addView(ibMenuBot, AVViewGroup_LayoutParams(botParams));

       // textview in ibMenu : card holder
      cTVBot:= AWTextView.create(getContext);
      cTVBot.setText(aFooterText);
      lpcTVBot:= AWRelativeLayout.InnerLayoutParams.create(AWRelativeLayout.InnerLayoutParams.WRAP_CONTENT, AWRelativeLayout.InnerLayoutParams.WRAP_CONTENT);
      lpcTVBot.addRule(AWRelativeLayout.CENTER_IN_PARENT);
      ibMenuBot.addView(cTVBot, AVViewGroup_LayoutParams(lpcTVBot));


      // +++++++++++++ MIDDLE COMPONENT: all our GUI content
       midLayout:= AWLinearLayout.create(getContext);
       midLayout.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.FILL_PARENT, AWLinearLayout.InnerLayoutParams.FILL_PARENT)));
       midLayout.setOrientation(AWLinearLayout.VERTICAL);

       midParams:= AWRelativeLayout.InnerLayoutParams.create(AWRelativeLayout.InnerLayoutParams.FILL_PARENT, AWRelativeLayout.InnerLayoutParams.FILL_PARENT);
       midParams.addRule(AWRelativeLayout.ABOVE, ibMenuBot.getId());
       midParams.addRule(AWRelativeLayout.BELOW, ibMenu.getId());
  addView(midLayout, AVViewGroup_LayoutParams(midParams));

       //scroll - so our content will be scrollable between the header and the footer
       vscroll := AWScrollView.create(getContext);
       vscroll.setFillViewport(false);
       midLayout.addView(vscroll);

       //panel in scroll: add all controls/ objects to this layout
       m_panel := AWLinearLayout.create(getContext);
       m_panel.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.FILL_PARENT, AWLinearLayout.InnerLayoutParams.WRAP_CONTENT )));
       m_panel.setOrientation(AWLinearLayout.VERTICAL);
       vscroll.addView(m_panel);
  FHTMLTemplate := '';
end;

procedure AZCForm.addButton(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aButtonText: String);
var
  b: AWButton;
begin
     b:= AWButton.create(getContext);
     b.setID(aButtonID);
     b.setBackgroundColor(0);
     b.setLayoutParams(AVViewGroup_LayoutParams(AWLinearLayout.InnerLayoutParams.create(AWLinearLayout.InnerLayoutParams.FILL_PARENT, AWLinearLayout.InnerLayoutParams.WRAP_CONTENT )));
     b.setText(JLString(aButtonText));
     b.setOnClickListener(aOnClickListener);
     m_panel.addView(b);
end;

procedure AZCForm.addButtonImage(aOnClickListener: AVView.InnerOnClickListener; aButtonID: Integer; aButtonText: String; aImageResuource: Jint);
var
   llayout: AWLinearLayout;
   Button: AWButton;
   ImageView: AWImageView;
   tHTMLTemplate: JLString;
begin
   llayout:= AWLinearLayout.Create(getContext);
   llayout.setOrientation(AWLinearLayout.HORIZONTAL);
   llayout.setGravity(AVGravity.CENTER);
   llayout.setBackgroundColor(0);

       ImageView := AWImageView.create(getContext);
       ImageView.setImageResource(aImageResuource);
   llayout.addView(ImageView);

       Button := AWButton.create(getContext);
       Button.setOnClickListener(aOnClickListener);
       Button.setId(aButtonID);

       if FHTMLTemplate <> '' then begin
         tHTMLTemplate := FHTMLTemplate;
         tHTMLTemplate := tHTMLTemplate.replaceAll('<#Button>', aButtonText);
         Button.setText(ATHtml.fromHtml(tHTMLTemplate));
       end else
       Button.setText(JLString(aButtonText));
       Button.setBackgroundColor(0);
   llayout.addView(Button);

     m_panel.addView(llayout);
end;

procedure AZCForm.addLine(aColor: jint = AGColor.RED);
var
   v: AVView;
begin
   v:= AVView.create(getContext);
   v.setLayoutParams(AVViewGroup_LayoutParams((AWLinearLayout.InnerLayoutParams.create(AVViewGroup_LayoutParams.MATCH_PARENT, 5))));
   v.setBackgroundColor(aColor);

   m_panel.addView(v);
end;

procedure AZCForm.addText(aText: JLstring);
var
   tv: AWTextView;
begin
   tv:= AWTextView.create(getContext);
  // tv.setLayoutParams(AVViewGroup_LayoutParams((AWLinearLayout.InnerLayoutParams.create(AVViewGroup_LayoutParams.MATCH_PARENT, 5))));
  // tv.setBackgroundColor(aColor);
   tv.Text := aText;
   m_panel.addView(tv);
end;

procedure AZCForm.setView(aView: AVView);
begin
  m_panel.addView(aView);
end;


end.

