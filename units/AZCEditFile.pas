{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 30-8-15 19:28:32
***********************************************************}
unit AZCEditFile;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15, Rjava, AZCDialogs;

type

  { AEditFile }

  AEditFile = class(AZCDialog,
                       ACDialogInterface.InnerOnClickListener)
  private
   fContext : ACContext;
   fFileName: JLString;
   fEditText: AWEditText;
   fHorizontalScropllView : AWHorizontalScrollView;
  strict protected
   procedure LoadFile;
   procedure WriteFile;
  public
   constructor create(para1: ACContext; aFileName: JLString); overload; //override;
   procedure show; overload; override;
   //ACDialogInterface.InnerOnClickListener
   procedure onClick(para1: ACDialogInterface; para2: jint); overload;
   // procedure onCreate(para1: AOBundle); overload; override;
  end;

implementation

uses ADBDataBase;

{ AEditFile }

procedure AEditFile.onClick(para1: ACDialogInterface; para2: jint);
begin
  case para2 of
    -1: WriteFile;
  end;
end;


procedure AEditFile.LoadFile;
var
  reader: JIBufferedReader;
  line: JLString;
  Data: JLString;
begin
  line := string(''); Data := string('');

  if checkExistsFile(fFileName) then begin
       reader := JIBufferedReader.create((JIFileReader.create(fFileName)));
       while not (line = nil) do begin
         try
           line := reader.readLine;
           if line <> nil then
             Data := JLString(Data).concat(line).concat(string(#10));
         finally
         end;
       end;
   end;
   fEditText.setText(Data);
end;

procedure AEditFile.WriteFile;
var
  fw: JIFileWriter;
begin
    fw:= JIFileWriter.create(fFileName);
    fw.append(fEditText.getText.toString);
    fw.close;
end;

constructor AEditFile.create(para1: ACContext; aFileName: JLString);
begin
  fContext := para1;
  fFileName := aFileName;
  inherited create(fContext);
 // setIcon(R.drawable.ic_launcher);
  setTitle(JLString('EDIT INI FILE '));

     fHorizontalScropllView := AWHorizontalScrollView.create(fContext);

      fEditText:= AWEditText.create(fContext);
     fHorizontalScropllView.addView(fEditText);

  setView(fHorizontalScropllView);

  setButton(JLString('Save'), Self);
  setButton2(JLString('Cancel'), Self);
end;

procedure AEditFile.show;
begin
  LoadFile;
  inherited show;
end;

end.

