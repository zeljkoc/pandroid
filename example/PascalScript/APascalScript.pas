{ Imports for Java packages/classes: zeljus. }
unit APascalScript;
{$mode delphi}

interface

type
  ZCPPascalScript = class;
  Arr1ZCPPascalScript = array of ZCPPascalScript;
  Arr2ZCPPascalScript = array of Arr1ZCPPascalScript;
  Arr3ZCPPascalScript = array of Arr2ZCPPascalScript;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;


{$include APascalScript.inc}

implementation

end.
