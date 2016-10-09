unit Data;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.popis}

interface

uses androidr15, ADBDataBase;

function initDataBase(aContext: ACContext; aDatabase: ASQLDatabase): ASQLDatabase;


function FindMaterijal(IDMaterijal: string;  aDataBase: ASQLDatabase): JLong;

function generateCsvFile(sFileName: JLString; aDatabase: ASQLDatabase): boolean;
procedure ExportPopis(aContext: ACContext; aFileName: JLString; aDatabase: ASQLDatabase);

procedure FillMaterijal(aContext: ACContext; aFileName: String;  aDataBase: ASQLDatabase);

implementation

uses  Popis, AZCDialogs;

const
  DataBaseName  = 'popis001.ldb';



function initDataBase(aContext: ACContext; aDatabase: ASQLDatabase): ASQLDatabase;
begin
  aDatabase :=  ASQLDatabase.Create(aContext, DataBaseName, nil, 1);

   aDatabase.SQL.add(JLString('create table MjestoPopisa (' +
           'MjestoPopisaID integer primary key autoincrement, ' +
           'Napomena text not null ) '));

   aDatabase.SQL.add(JLString('create table sMaterijal (' +
           'MaterijalID integer primary key autoincrement, '+
           'IDMaterijal text not null unique,'+
           'Materijal text not null  '+
           ') '));

  aDatabase.SQL.add(JLString('create table PopisnaLista ( '+
 	'PopisnaListaID integer not null primary key autoincrement, '+
 	'IDMjestoPopisa	integer not null, '+
        'IDMaterijal integer not null, '+
 	'Kolicina numeric not null, ' +
        'foreign key(IDMjestoPopisa) references MjestoPopisa(MjestoPopisaID) on delete cascade, '+
        'foreign key(IDMaterijal) references sMaterijal(MaterijalID) on delete cascade '+
        ') '));

  //---------------------------------------------------------

  Result := aDatabase;
end;




function FindMaterijal(IDMaterijal: string; aDataBase: ASQLDatabase): JLong;
var
 c: ADCursor;
begin
 Result := 0;
 try
   c := aDataBase.rawQuery(JLString('select MaterijalID from sMaterijal where IDMaterijal = ').concat(string('"')).concat(IDMaterijal).concat(string('"')) ,nil );
   c.moveToPosition(0);
   if (c.getCount <> 0) then Result := c.getLong(0);
 except
 end;
end;


function generateCsvFile(sFileName: JLString; aDatabase: ASQLDatabase): boolean;
var
 fw: JIFileWriter;
  c: ADCursor;
  line : JLstring;
begin
  result := false;

  c := aDatabase.rawQuery('select pl.IDMjestoPopisa, m.IDMaterijal, m.Materijal, pl.Kolicina from PopisnaLista pl, sMaterijal m '+
                     'where (pl.IDMaterijal = m.MaterijalID) ' +
                     ' order by pl.IDMaterijal desc' , nil);

  fw:= JIFileWriter.create(JLString(sFileName));
  try
   c.moveToFirst;
   while not c.isAfterLast do begin
     line := c.getString(0).concat(';').
         concat(c.getString(1)).concat(';').
         concat(c.getString(2)).concat(';').
         concat(c.getString(3)).concat(#10);
     fw.append(line);
     c.moveToNext;
   end;

   fw.close;
   result := true;
  except

  end;
end;

procedure ExportPopis(aContext: ACContext; aFileName: JLString; aDatabase: ASQLDatabase);
var
 intent : ACIntent;
  data: JIFile;
  eto: array of JLString;
begin

  eto := emailTo.Split(',');
  data := JIFile.create(aFileName);

  intent := ACIntent.create(ACIntent.ACTION_SEND);
  //intent.putExtra(ACIntent.EXTRA_STREAM, ANUri.fromFile(data));
  intent.putExtra(ACIntent.EXTRA_STREAM, ANUri.Parse(JLString('file:').concat(aFileName)));
  Intent.addFlags(ACIntent.FLAG_GRANT_READ_URI_PERMISSION);
  intent.putExtra(ACIntent.EXTRA_EMAIL  ,  eto);
  intent.putExtra(ACIntent.EXTRA_SUBJECT, JLString('Popis'));
  intent.putExtra(ACIntent.EXTRA_TEXT   , JLString('Pozdrav: ').concat(aFileName));

  Intent.setType(JLString('txt/csv'));
  aContext.startActivity(ACIntent.createChooser(Intent, JLString('Nacin slanja popisa..')));

end;

procedure FillMaterijal(aContext: ACContext; aFileName: String;
  aDataBase: ASQLDatabase);
Var
  reader: JIBufferedReader;
  line: JLString;
  RowData: Array of JLString;
  values: ACContentValues;

   c: ADCursor;
   str: string;
   l : array of jbyte;
begin
   reader := JIBufferedReader.create((JIFileReader.create(aFileName)));
  try
    aDataBase.beginTransactionNonExclusive;
    Line := '  ';
       while not (line = nil) do begin
         try
           line := reader.readLine;
           RowData:= line.split(';');
          if 2 = 2 then begin
           values := ACContentValues.Create(1);
           values.put('IDMaterijal', RowData[0]);
           values.put('Materijal', RowData[1].toString);
           aDataBase.insert('sMaterijal', Nil, values);
          end;
        except
         end;
       end;
       aDataBase.setTransactionSuccessful;
     reader.Close;
   finally

      aDataBase.endTransaction;
   end;

end;


end.


