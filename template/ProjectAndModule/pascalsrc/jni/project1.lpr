{***********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************}
library project1;

{%BuildCommand sh compile.sh}
{%BuildScan MAKE-}

{$mode delphi}


uses PandoidModule;

exports
  Java_zeljus_com_PandroidModule_CreateObject,
  Java_zeljus_com_PandroidModule_SetPropValue,
  Java_zeljus_com_PandroidModule_GetPropValue,
  Java_zeljus_com_PandroidModule_SetObjectProp,
  Java_zeljus_com_PandroidModule_Free;

begin
end.

