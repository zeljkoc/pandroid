{ Imports for Java packages/classes: zeljus.com. }
unit dataset;
{$mode delphi}

interface

type
  ZCField = class;
  Arr1ZCField = array of ZCField;
  Arr2ZCField = array of Arr1ZCField;
  Arr3ZCField = array of Arr2ZCField;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  JLEnum = class external 'java.lang' name 'Enum';
  Arr1JLEnum = array of JLEnum;
  Arr2JLEnum = array of Arr1JLEnum;
  Arr3JLEnum = array of Arr2JLEnum;


{$include dataset.inc}

implementation

end.
