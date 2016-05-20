{ Imports for Java packages/classes: zeljus.com.firebird. }
unit zcfbclient;
{$mode delphi}

interface

type
  ZCFUIBDataBase = class;
  Arr1ZCFUIBDataBase = array of ZCFUIBDataBase;
  Arr2ZCFUIBDataBase = array of Arr1ZCFUIBDataBase;
  Arr3ZCFUIBDataBase = array of Arr2ZCFUIBDataBase;

  ZCFUIBDataSet = class;
  Arr1ZCFUIBDataSet = array of ZCFUIBDataSet;
  Arr2ZCFUIBDataSet = array of Arr1ZCFUIBDataSet;
  Arr3ZCFUIBDataSet = array of Arr2ZCFUIBDataSet;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLBoolean = class external 'java.lang' name 'Boolean';
  Arr1JLBoolean = array of JLBoolean;
  Arr2JLBoolean = array of Arr1JLBoolean;
  Arr3JLBoolean = array of Arr2JLBoolean;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;


{$include zcfbclient.inc}

implementation

end.
