{ Imports for Java packages/classes: cilico. }
unit CilicoRFID;
{$mode delphi}

interface

type
  CTI2CTools = class;
  Arr1CTI2CTools = array of CTI2CTools;
  Arr2CTI2CTools = array of Arr1CTI2CTools;
  Arr3CTI2CTools = array of Arr2CTI2CTools;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;


{$include CilicoRFID.inc}

implementation

end.
