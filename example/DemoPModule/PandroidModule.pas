{ Imports for Java packages/classes: zeljus. }
unit PandroidModule;
{$mode delphi}

interface

type
  ZCPandroidModule = class;
  Arr1ZCPandroidModule = array of ZCPandroidModule;
  Arr2ZCPandroidModule = array of Arr1ZCPandroidModule;
  Arr3ZCPandroidModule = array of Arr2ZCPandroidModule;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;


{$include PandroidModule.inc}

implementation

end.
