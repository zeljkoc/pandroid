{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
{%BuildWorkingDir /usr/local/pandroid/library/Firebird}
{%BuildCommand sh compile.sh}
{%BuildScan MAKE-}

library project1;

{$mode objfpc}{$H+}

uses jni, zcFirebird;

exports
  //UIBDataBase
  Java_zeljus_com_firebird_UIBDataBase_Init,
  Java_zeljus_com_firebird_UIBDataBase_setUserNamePassword,
  Java_zeljus_com_firebird_UIBDataBase_Connect,
  Java_zeljus_com_firebird_UIBDataBase_Disconnect,
  Java_zeljus_com_firebird_UIBDataBase_isConnected,

  //UIBDataSet
  Java_zeljus_com_firebird_UIBDataSet_Init,
  Java_zeljus_com_firebird_UIBDataSet_Open,
  Java_zeljus_com_firebird_UIBDataSet_Close,
  Java_zeljus_com_firebird_UIBDataSet_isActive,
  Java_zeljus_com_firebird_UIBDataSet_ExecSQL,

  Java_zeljus_com_firebird_UIBDataSet_Edit,
  Java_zeljus_com_firebird_UIBDataSet_Post,
  Java_zeljus_com_firebird_UIBDataSet_Next,
  Java_zeljus_com_firebird_UIBDataSet_Prior,
  Java_zeljus_com_firebird_UIBDataSet_EOF,
  Java_zeljus_com_firebird_UIBDataSet_BOF,
  Java_zeljus_com_firebird_UIBDataSet_First,
  Java_zeljus_com_firebird_UIBDataSet_Last,
  Java_zeljus_com_firebird_UIBDataSet_getRecNo,
  Java_zeljus_com_firebird_UIBDataSet_setRecNo,

  Java_zeljus_com_firebird_UIBDataSet_FieldCount,
  Java_zeljus_com_firebird_UIBDataSet_RecordCount,
  Java_zeljus_com_firebird_UIBDataSet_getAsString,
  Java_zeljus_com_firebird_UIBDataSet_getAsInteger,
  Java_zeljus_com_firebird_UIBDataSet_setAsString

;

begin
end.

