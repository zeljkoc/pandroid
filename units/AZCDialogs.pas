{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit AZCDialogs;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15, Rjava, AZCAdapters, ADBDataBase, AUtils, dataset;

type

  { AZCDialog }

  AZCDialog = class(AAAlertDialog)
  private
   fID: integer;
  public
   constructor create(para1: ACContext); overload; virtual;
  public
   property ID: integer read fID write fID;
  end;

  { AZCFormDialog }

  AZCFormDialog = class(AZCDialog)
  public
   constructor create(para1: ACContext); overload; override;
  public
   property ID;
  end;

   { AEditDialog }

   AEditDialog = class(AAAlertDialog,
                      // AVView.InnerOnClickListener,
                       AVView.InnerOnFocusChangeListener,
                       ATTextWatcher)
   private
    fID: Integer;
    fEditPanel: AWScrollView;
    fContext: ACContext;
    fBeforeValue: JLString;
    fCurrentField : integer;
    fFields: JUArrayList;
    fDataType: ZCField.InnerFDataType;
    function GetField(index: integer): ZCField;
    procedure SetField(index: integer; AValue: ZCField);
    procedure SetFields(AValue: JUArrayList);
   protected
    procedure CreateEditPanel;
  public
    constructor create(para1: ACContext); overload;
    procedure show; overload; override;
    procedure setView(para1: AVView); overload; override;
    // AVView.InnerOnFocusChangeListener
    procedure onFocusChange(para1: AVView; para2: jboolean); overload;
    //text change
    procedure afterTextChanged(editable: ATEditable); overload;
    procedure beforeTextChanged(charSequence: JLCharSequence; i: LongInt; i1: LongInt; i2: LongInt); overload;
    procedure onTextChanged(charSequence: JLCharSequence; i: LongInt; i1: LongInt; i2: LongInt); overload;
  public
    property ID: Integer read fID write fID;
    property Field[index: integer]: ZCField read GetField write SetField;
    property Fields : JUArrayList read fFields write SetFields;
    property CurrentField: integer read fCurrentField write fCurrentField;
   end;

  { TLookUpSQLDialog }

  AZCLookUpSQLDialog = class(AADialog,
                         AWAdapterView.InnerOnItemClickListener,
                         AVView.InnerOnClickListener)
  private
    fID: Integer;
    FContext: ACContext;
    FCursor: ADCursor;
    FAdapter : AZCDBListAdapter;
    FListView : AWListView;
    FKeyFieldID: integer;
    FLookupKeyFields: String;
    FLookupResultField: String;
    FSQLDatabase: ASQLDatabase;
    FSQL: JLString;
    FTable: string;
    FWhere: JLString;

    FHorizontalScropllView : AWHorizontalScrollView;
    FLinearLayout: AWLinearLayout;

    FCaption: String;
    procedure UpdateData;
    procedure setCaption(aCaption: String);
  public
    //AWAdapterView.InnerOnItemClickListener
    procedure onItemClick(tAdapter: AWAdapterView; tView: AVView; p1: LongInt; p2: Int64);
    //AVView.InnerOnClickListener
    procedure onClick(aView: AVView);
  public
    Text: string;
    ButtonText: string;
    TextView: AWTextView;
  public
    constructor create(aContext: ACContext; aSQLDatabase: ASQLDatabase; aSQL: JLString; aLookupResultField: string; aTable: String; aWhere : JLstring = string(''); aButonWidth: integer = 70); overload;
  public
    property ID: Integer read fID write fID;
    property KeyFieldID: integer read FKeyFieldID write FKeyFieldID;
    property LookupKeyFields: String read FLookupKeyFields write FLookupKeyFields;
    property LookupResultField: String read FLookupResultField;
    property Caption: String read FCaption write SetCaption;
  end;

procedure ShowMessage(aContext:ACContext; aMessage: JLString);
function isInputTest(aValue: JLString; aFieldType: ZCField.InnerFDataType): boolean;
function SetInputType(aZCField: ZCField) : jint;

implementation

procedure ShowMessage(aContext:ACContext; aMessage: JLString);
var
  Toast : AWToast;
  layout: AWLinearLayout;
  Message: AWTextView;
begin
   layout:= AWLinearLayout.Create(aContext);
   layout.setOrientation(AWLinearLayout.VERTICAL);
      Message:= AWTextView.Create(aContext);
      Message.setText(aMessage);
    //  Message.setBackgroundColor(AGColor.YELLOW);
    //  Message.setTextColor(AGColor.BLACK);
      Message.setBackgroundResource(R.drawable.toast_drawable);
   layout.addView(Message);

   Toast := AWToast.create(aContext);
   Toast.setView(layout);
  // Toast.setGravity(AVGravity.CENTER_HORIZONTAL, 0, 0);   //AVGravity.CENTER_HORIZONTAL | AVGravity.CENTER_VERTICAL
   Toast.setGravity(AVGravity.CENTER_HORIZONTAL, 0, 0);
   Toast.setDuration(AWToast.LENGTH_SHORT);
   Toast.show;
end;

function isInputTest(aValue: JLString; aFieldType: ZCField.InnerFDataType): boolean;
begin
   try
      if aFieldType =  ZCField.InnerFDataType.ftIinteger then  JLInteger.parseInt(aValue.toString) //Integer
       else if aFieldType = ZCField.InnerFDataType.ftFloat then JLFloat.parseFloat(aValue.toString);  //Float

      Result := true;
   except
      Result := false;
   end;
end;

function SetInputType(aZCField: ZCField): jint;
begin
  if  aZCField.fDataType = ZCField.InnerFDataType.ftIinteger then  Result := ATInputType.TYPE_CLASS_PHONE
   else  if aZCField.fDataType = ZCField.InnerFDataType.ftFloat  then  Result := ATInputType.TYPE_CLASS_PHONE
   else  if aZCField.fDataType = ZCField.InnerFDataType.ftDateTime then  Result := ATInputType.TYPE_DATETIME_VARIATION_DATE
   else if (aZCField.fDataType = ZCField.InnerFDataType.ftString) and   {velika slova}
           (aZCField.fCharCase = ZCField.InnerEditCharCase.fUpperCase) then Result := ATInputType.TYPE_TEXT_FLAG_CAP_CHARACTERS


   else  Result := ATInputType.TYPE_TEXT_VARIATION_EMAIL_ADDRESS;


end;

{ AZCFormDialog }

constructor AZCFormDialog.create(para1: ACContext);
begin
  inherited create(para1);
end;

{ AZCDialog }

constructor AZCDialog.create(para1: ACContext);
begin
  inherited create(para1);
  setIcon(R.drawable.ic_launcher);
end;


{ AEditDialog }

constructor AEditDialog.create(para1: ACContext);
begin
  fContext := para1;
  inherited Create(fContext);
  setIcon(R.drawable.ic_launcher);
end;

procedure AEditDialog.show;
begin
  CreateEditPanel;
  setView(fEditPanel);
  inherited show;
end;

procedure AEditDialog.setView(para1: AVView);
begin
  inherited setView(para1);
end;

procedure AEditDialog.onFocusChange(para1: AVView; para2: jboolean);
begin

  if para2 then begin
     fCurrentField := (para1 as AWEditText).getId;
     fDataType :=  ZCfield(fFields.get(fCurrentField)).fDataType;
  end;
end;

procedure AEditDialog.afterTextChanged(editable: ATEditable);
begin
  if editable.length <> 0 then begin
    try
      if not isInputTest(editable.toString, fDataType) then editable.replace(0, editable.length(), fBeforeValue);
      if not ZCfield(fFields.get(fCurrentField)).fReadOnly then begin
        ZCfield(fFields.get(fCurrentField)).fValue := editable.toString;
        if (ZCfield(fFields.get(fCurrentField)).fValue <> ZCfield(fFields.get(fCurrentField)).fOldValue) then  ZCfield(fFields.get(fCurrentField)).fChange:= True;
      end;
    except
       editable.replace(0, editable.length(), JLString(''));
    end;
  end;
end;

procedure AEditDialog.beforeTextChanged(charSequence: JLCharSequence; i: LongInt; i1: LongInt; i2: LongInt);
begin
    fBeforeValue := charSequence.toString;
end;

procedure AEditDialog.onTextChanged(charSequence: JLCharSequence; i: LongInt; i1: LongInt; i2: LongInt);
begin

end;

procedure AEditDialog.CreateEditPanel;
var
   layout: AWLinearLayout;
   tv :AWTextView;
   et :AWEditText;
   i: integer;
begin                      ;
  FEditPanel := AWScrollView.create(FContext);
  layout:= AWLinearLayout.create(FContext);
  layout.setOrientation(AWLinearLayout.VERTICAL);
 // layout.setVerticalScrollBarEnabled(true);

  for i:=0 to fFields.size -1 do begin
    if (ZCfield(fFields.get(i)).fVisible)  then begin        // visible
               tv := AWTextView.create(fContext);
               tv.setId(i + ((fFields.size -1) * 2));
               tv.setTypeface(nil, AGTypeface.BOLD);
               tv.setGravity(AVGravity.LEFT);
              // tv.setTextColor(fContext.getResources().getColor(AAAlertDialog.InnerBuilder. )); //AlertDialog color
               if JLString(ZCfield(fFields.get(i)).fDisplayName).length > 0 then
                 tv.setText(ZCfield(fFields.get(i)).fDisplayName.toString.toUpperCase)
               else tv.setText(ZCfield(fFields.get(i)).fName.toString.toUpperCase);

            layout.addView(tv);

             if (not ZCfield(fFields.get(i)).fReadOnly) then begin  //not ReadOnly
                 et:= AWEditText.Create(fContext);
                 et.setInputType(SetInputType(ZCfield(fFields.get(i))));

                 et.setSelectAllOnFocus(true);
                 et.setOnFocusChangeListener(Self);
                 et.addTextChangedListener(self);
                 et.setText(ZCfield(fFields.get(i)).fValue.toString);
               //  et.setError(JLString('Edit'));
                // et.setHint(ZCField(fFields.get(i)).fName.toString);
                 et.setID(i);
               layout.addView(et);
            end else begin    // REadOnly polje
                 tv := AWTextView.create(fContext);
                 tv.setId(i + (fFields.size -1));
                 //tv.setTypeface(nil, AGTypeface.BOLD_ITALIC);
                 tv.setGravity(AVGravity.LEFT);
                 tv.setText(ZCfield(fFields.get(i)).fValue.toString);
              layout.addView(tv);
            end;
      end;
  end;

  for  i:=0 to fFields.size -1 do
     ZCfield(fFields.get(i)).fChange:= false;

fEditPanel.addView(layout);
end;

function AEditDialog.GetField(index: integer): ZCField;
begin
  Result := ZCfield(fFields.get(index));
end;

procedure AEditDialog.SetField(index: integer; AValue: ZCField);
begin
  fFields.&set(index, aValue);
end;

procedure AEditDialog.SetFields(AValue: JUArrayList);
begin
  if fFields=AValue then Exit;
  fFields:=AValue;
end;


{ TLookUpSQLDialog }

procedure AZCLookUpSQLDialog.UpdateData;
var
 Button: AWButton;
 i: integer;
 SQL : JLString;
begin
   if Assigned(FLinearLayout) then begin
     FLinearLayout.removeAllViews;
     FHorizontalScropllView.removeAllViews;

     if JLString(ButtonText) = JLString('ALL') then Text := '' else
     if ((JLString(ButtonText) = JLString(string('<'))) and (Length(Text) > 0)) then Text :=  JLString.CopyValueOf(JLString(Text).toCharArray, 0, JLString(Text).length -1) // Copy(Text, 1, Length(text)-1)
     else  if (JLString(ButtonText) <> JLString(string('<'))) then Text := JLString(Text).concat(ButtonText);
   end else Text:='';

  //=====================================
   TextView.setText(JLString(Text));
  //=====================================

  try
     SQL := JLString('select distinct SUBSTR(UPPER(').
             concat(FLookupResultField).concat(') , ').concat(JLInteger.toString(Length(Text)+1)).concat(', 1 ) from ').concat(FTable).
             concat(' where ').concat(FWhere.toString).concat('(UPPER(').concat(FLookupResultField).concat(') LIKE "');

     if JLString(Text).length > 0 then
        SQL := SQL.concat(JLString.CopyValueOf(JLString(Text).toCharArray, 0, JLString(Text).length-0));

     SQL := SQL.concat(string('%") ')).concat(' order by ').concat(' SUBSTR(UPPER(').
             concat(FLookupResultField).concat(') , ').concat(JLInteger.toString(Length(Text)+1)).concat(', 1 ) ') ;

     if FSQLDatabase.isopen then
        FCursor := FSQLDatabase.rawQuery(SQL, nil);

     // TextView.setText(JLString(SQL).concat(' : ').concat(Text));
  except
    Exit;
  end;

  try
    if not Assigned(FLinearLayout) then
     FLinearLayout:= AWLinearLayout.Create(FContext);

      Button:= AWButton.Create(FContext);
      Button.setID(0);
      Button.setOnClickListener(Self);
      Button.setText(JLString('ALL'));
      Button.setTypeface(nil, AGTypeface.BOLD);
     // Button.setTextColor(AGColor.YELLOW);
    FLinearLayout.addView(Button);

      Button:= AWButton.Create(FContext);
      Button.setID(1);
      Button.setOnClickListener(Self);
      Button.setText(JLString(string('<')));
      Button.setTypeface(nil, AGTypeface.BOLD);
    //  Button.setTextColor(AGColor.YELLOW);
    FLinearLayout.addView(Button);

      for i:=0 to FCursor.getCount -1 do begin
         if FCursor.moveToPosition(i) then begin
            Button:= AWButton.Create(FContext);
            Button.setID(i+2);                     //dva button pre
            Button.setOnClickListener(Self);
            Button.setText(FCursor.getString(0).toString);
            FLinearLayout.addView(Button);
         end;
     end;
    FHorizontalScropllView.addView(FLinearLayout);

  except
    //ShowMessage(FContext, JLString('Greska'));
    Exit;
  end;

  //----------------------------------------------
  try
     SQL := FSQL.concat(' where').concat(FWhere.toString).concat(' (UPPER(').concat(FLookupResultField).concat(') LIKE "').concat(JLString(Text)).concat('%") ').
               concat(' order by ').concat(FLookupResultField).concat(' ASC LIMIT 50 ');    //--- show max 50 row select data

     if FSQLDatabase.isopen then
         FCursor := FSQLDatabase.rawQuery(SQL, nil);
     FAdapter.Cursor := FCursor;

    // setCaption(FCaption);
   except
   end;

end;

procedure AZCLookUpSQLDialog.setCaption(aCaption: String);
{var
   Broj: string;  }
begin
    //Broj := JLInteger.toString(FCursor.getCount);
    self.setTitle(JLString(aCaption)); //.concat(': <').concat(Broj).concat(String('>')));
    FCaption := aCaption;
end;



procedure AZCLookUpSQLDialog.onItemClick(tAdapter: AWAdapterView; tView: AVView;
  p1: LongInt; p2: Int64);
begin
  if FCursor.moveToPosition(p2) then begin
    FLookupKeyFields := FCursor.getString(FKeyFieldID).toString.trim;

   try
    if FLookupResultField <> '' then   //Problem ako polje ima razlicit naziv nego u bazi
       FLookupResultField := FCursor.getString(FCursor.getColumnIndex(FLookupResultField)).toString.trim;
    except
      try
       FLookupResultField := FCursor.getString(1).toString.trim;
      except
      end;
    end;
  end;
 Dismiss;
end;

procedure AZCLookUpSQLDialog.onClick(aView: AVView);
begin
  if AWButton(aView).getText.toString.trim = '"' then
    ButtonText := ''
  else
    ButtonText :=  AWButton(aView).getText.toString.trim;

  UpdateData;
end;

constructor AZCLookUpSQLDialog.create(aContext: ACContext;
  aSQLDatabase: ASQLDatabase; aSQL: JLString; aLookupResultField: string;
  aTable: String; aWhere : JLstring = string(''); aButonWidth: integer = 70);
var
 layout: AWTableLayout;
 Params:  AWLinearLayout.InnerLayoutParams;
begin
  inherited Create(aContext);
  //setIcon(R.drawable.ic_launcher);
  requestWindowFeature(AVWindow.FEATURE_LEFT_ICON);

  FContext:= aContext;
  FSQLDatabase := aSQLDatabase;
  FSQL := aSQL;
  FTable := aTable;
  FLookupResultField := aLookupResultField;

  Layout:= AWTableLayout.Create(FContext);
  setContentView(Layout);
  //---------------------------------------
    FHorizontalScropllView := AWHorizontalScrollView.Create(FContext);
    Params:=  AWLinearLayout.InnerLayoutParams.Create(
              AWLinearLayout.InnerLayoutParams.FILL_PARENT ,  aButonWidth);  //sirina button-a
  Layout.addView(FHorizontalScropllView, AVViewGroup_LayoutParams(params));
  //-------------------------------------------
    TextView:= AWTextView.Create(FContext);
   // TextView.setTextColor(AGColor.YELLOW);
  Layout.addView(TextView );
   //-------------------------------------------
    FListView := AWListView.Create(FContext);
    FListView.setOnItemClickListener(self);
  Layout.addView(FListView);
  //------------------------------------------

  ButtonText := ''; fWhere := aWhere;

  FAdapter:= AZCDBListAdapter.create(FContext);
  UpdateData;
  FListView.setAdapter(FAdapter);
  setFeatureDrawableResource(AVWindow.FEATURE_LEFT_ICON, R.drawable.ic_launcher);
end;


end.

