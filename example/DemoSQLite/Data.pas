unit Data;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demosqlite}

interface

uses
  androidr15, ADBDataBase;

function initDataBase(aContext: ACContext; aDatabase: ASQLDatabase): ASQLDatabase;

implementation

const
  DataBaseName  = 'proba01.ldb';

function initDataBase(aContext: ACContext; aDatabase: ASQLDatabase): ASQLDatabase;
begin
  aDatabase :=  ASQLDatabase.Create(aContext, DataBaseName, nil, 1);

  aDatabase.SQL.add(JLString('create table MjestoPopisa (' +
             'MjestoPopisaID integer primary key autoincrement, ' +
             'MjestoPopisa text not null ) '));

    //---------------------------------------------------------

    Result := aDatabase;
end;


end.

