library project1;

{$mode objfpc}{$H+}

uses jni, pl_synapse, insertRecord;

exports
  IndyConnect        name 'Java_zeljus_com_jni_jnireplicate_IndyConnect',
  IndyConnected      name 'Java_zeljus_com_jni_jnireplicate_IndyConnected',
  IndyDisconnect     name 'Java_zeljus_com_jni_jnireplicate_IndyDisconnect',
  SetDataBaseParams  name 'Java_zeljus_com_jni_jnireplicate_SetDataBaseParams',
  SendSql            name 'Java_zeljus_com_jni_jnireplicate_SendSql',
  GetSql             name 'Java_zeljus_com_jni_jnireplicate_GetSql',
  SelectFBTable      name 'Java_zeljus_com_jni_jnireplicate_SelectFBTable',
  InsertFBTable      name 'Java_zeljus_com_jni_jnireplicate_InsertFBTable',

  HTTPSOAPPost       name 'Java_zeljus_com_jni_jnireplicate_HTTPSOAPPost'
;

begin
end.

