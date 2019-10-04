unit Data;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demosqlite}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, DB, DataBase;

function initDataBase(aContext: ACContext; aDatabase: TDataBase): TDataBase;

implementation

const
  DataBaseName  = 'proba02.ldb';

function initDataBase(aContext: ACContext; aDatabase: TDataBase): TDataBase;
begin
  aDatabase :=  TDatabase.Create(aContext, DataBaseName, nil, 1);

  aDatabase.SQL.add(JLString('create table MjestoPopisa (' +
             'MjestoPopisaID integer not null primary key autoincrement, ' +
             'MjestoPopisa text not null ) '));

    //---------------------------------------------------------

    Result := aDatabase;
end;


end.

