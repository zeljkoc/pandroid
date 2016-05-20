{ Imports for Java packages/classes: zeljus. }
unit fbsqlite;
{$mode delphi}

interface

type
  ZCJjnireplicate = class;
  Arr1ZCJjnireplicate = array of ZCJjnireplicate;
  Arr2ZCJjnireplicate = array of Arr1ZCJjnireplicate;
  Arr3ZCJjnireplicate = array of Arr2ZCJjnireplicate;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;


{$include fbsqlite.inc}

implementation

end.
