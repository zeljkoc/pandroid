unit uReplicate;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TModulReplReceive = record
    BrojTabela: integer;

    SQLSelect: Array of String;
    SQLInsert: Array of String;
  end;

  TModulReplReceiveRedniBroj = record
    BrojTabela: integer;

    SQLSelect: Array of String;
    SQLSelectLite: Array of String;
  end;


  TModulReplSend = record
    BrojTabela : integer;
    TableNameSource: Array of String;

    SQLInsertSource: Array of String;
    SQLInsertTarget: Array of String;

    SQLUpdateSource: Array of String;
    SQLUpdateTarget: Array of String;

    SQLDeleteSource: Array of String;
    SQLDeleteTarget: Array of String;
  end;


var
  MReplReceive : TModulReplReceive;
  MReplReceiveRedniBroj : TModulReplReceiveRedniBroj;
  MReplSend : TModulReplSend;

Procedure InitReplParams(aIDPda: String; aIDPartner: String);

implementation


procedure InitReplParams(aIDPda: String; aIDPartner: String);
begin
  //RECEIVE      {Sifrarnici}
  with MReplReceive do begin
      BrojTabela := 1;                     //Broj tabela koje osvjezavas
      SetLength(SQLSelect, BrojTabela);
      SetLength(SQLInsert, BrojTabela);

      //BARKOD
      SQLSelect[0] := 'select "BarKodID" as BarKodID from "BarKod"   ';
      SQLInsert[0] := 'INSERT or REPLACE INTO sBarCode (BarCodeID ) VALUES (:BarKodID)';
  end;


  //RECEIVE RedniBroj  {Redni broj sa servera }
(*  with MReplReceiveRedniBroj do begin
      BrojTabela := 1;                     //Broj tabela koje osvjezavas
      SetLength(SQLSelect, BrojTabela);
      SetLength(SQLSelectLite, BrojTabela);

      //PopisnaLista RedniBroj
      SQLSelect[0] := 'SELECT GEN_ID("New_RedniBroj_PopisnaLista", :RedniBroj ) as "RemoteRedniBroj" FROM RDB$DATABASE  ';
      SQLSelectLite[0] := 'SELECT RedniBrojID, RemoteRedniBroj FROM sRedniBroj where (RemoteRedniBroj is null) ';


  end;  *)

  //SEND  {slanje podataka replikovano}
(*  with MReplSend do begin
      BrojTabela := 2;                    //Broj tabela koje se prenose

      //Source
      SetLength(TableNameSource, BrojTabela);  //Naziv tabele u REPL_LOG
      SetLength(SQLInsertSource, BrojTabela);
      SetLength(SQLUpdateSource, BrojTabela);
      SetLength(SQLDeleteSource, BrojTabela);

      //Target
      SetLength(SQLInsertTarget, BrojTabela);
      SetLength(SQLUpdateTarget, BrojTabela);
      SetLength(SQLDeleteTarget, BrojTabela);
  //----------------------------------------------------

      // BARKOD Insert
      TableNameSource[0] := 'sBarCode';
      SQLInsertSource[0] := 'select BarCodeID, BarCodeNaziv, Komada  from sBarCode where (BarCodeID = :Kljuc )' ;
      SQLInsertTarget[0] := 'insert into "BarKod" ("BarKodID", "NazivProizvoda", "KolicinaPakovanja") values ( :BarCodeID, '':BarCodeNaziv'', :Komada ) ' ; //String definisati ' '

      // PopisnaLista Insert
      TableNameSource[1] := 'PopisnaLista';
      SQLInsertSource[1] := 'select pl.PopisnaListaID, rb.RemoteRedniBroj, pl.IDBarCode, round(pl.Kolicina, 2) as Kolicina from PopisnaLista pl, sRedniBroj rb where (rb.RedniBrojID = pl.IDRedniBroj) and (PopisnaListaID = :Kljuc )' ;
      SQLInsertTarget[1] := 'insert into "PopisnaLista" ("IDPartner", "RedniBroj", "IDPopisnaLista", "IDBarCode", "Kolicina", "Datum", "IDPda") values (:IDPartner, :RemoteRedniBroj, :PopisnaListaID, :IDBarCode, '':Kolicina'', current_timestamp, :IDPda) ' ;
        //Update
        SQLUpdateSource[1] := 'select pl.PopisnaListaID, rb.RemoteRedniBroj, pl.IDBarCode, round(pl.Kolicina, 2) as Kolicina  from PopisnaLista pl, sRedniBroj rb where (rb.RedniBrojID = pl.IDRedniBroj) and (PopisnaListaID = :Kljuc )' ;
        SQLUpdateTarget[1] := 'update "PopisnaLista" set  "Kolicina" = '':Kolicina'', "Datum" = current_timestamp where ("IDPartner" = :IDPartner) and ("RedniBroj" = :RemoteRedniBroj) and ("IDPopisnaLista" = :PopisnaListaID)  ' ;
        //Delete
        SQLDeleteSource[1] := 'select RemoteRedniBroj from sRedniBroj where (RedniBrojID = :Kljuc2) ' ;
        SQLDeleteTarget[1] := 'delete from "PopisnaLista" where ("IDPartner" = :IDPartner) and ("RedniBroj" = :RemoteRedniBroj ) and ("IDPopisnaLista" = :Kljuc )  ' ;
  end;     *)

end;



end.

