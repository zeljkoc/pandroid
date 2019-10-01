{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoXML/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoXML;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demoxml}

interface

uses androidr15, Rjava, AActivity;


type

  { MainActivity }

  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override; 
  public
    tv: AWTextView;
  public
    procedure ParseXML;
    function getValue(tag: JLString; element: OWDElement): JLString;
  end;

implementation


procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
 layout: AWLinearLayout;
 bt: AWButton;
begin
   inherited onCreate(savedInstanceState);
   layout:= AWLinearLayout.Create(Self);
   layout.setOrientation(AWLinearLayout.VERTICAL);
   setContentView(layout);

     bt:= AWButton.Create(Self);
     bt.setID(1);
     bt.setText(JLString('Parser'));
     bt.setOnClickListener(Self);
   layout.addView(bt);

     tv:= AWTextView.Create(Self);
    // tv.setText(JLString(''));
   layout.addView(tv);
end;

procedure MainActivity.onClick(aView: AVView);  
begin
  case aView.getId of
    1: //Parser
      begin
        ParseXML;
      end;
  end;
         
end;

procedure MainActivity.ParseXML;
var
 tekst : JLString;

 istream: JIInputStream;
 dbFactory: JXPDocumentBuilderFactory;
 dBuilder: JXPDocumentBuilder;
 doc: OWDDocument;

 element, element2: OWDElement;
 nList: OWDNodeList;
 node: OWDNode;
 i: integer;
begin
 tekst := '';

   istream:= getAssets.open('fajl.xml');
   dbFactory:= JXPDocumentBuilderFactory.newInstance;
   dBuilder:= dbFactory.newDocumentBuilder;
   doc:= dBuilder.parse(istream);

   element:=doc.getDocumentElement;
   element.normalize;

   nList:= doc.getElementsByTagName('employee');

   for i:=0 to  nList.getLength -1 do begin
      node:= nList.item(i);
      if (node.getNodeType = OWDNode.ELEMENT_NODE) then begin
        element2:= OWDElement(node);
        tekst := tekst.concat(#10).concat('Name : ').concat(getValue('name', element2)).concat(#10);
        tekst := tekst.concat('Salary : ').concat(getValue('salary', element2)).concat(#10);
        tekst := tekst.concat('-----------------------');
      end;
   end;

   tv.setText(tekst);
end;

function MainActivity.getValue(tag: JLString; element: OWDElement): JLString;
var
  nodeList: OWDNodeList;
  node: OWDNode;
begin
 nodeList:= element.getElementsByTagName(tag).item(0).getChildNodes;
 node:= OWDNode(nodeList.item(0));
  Result :=  node.getNodeValue;
end;



end.
