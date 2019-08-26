{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
{ Imports for Java packages/classes: zeljus. }
unit IniFile;
{$mode delphi}
{$namespace zeljus.com.units}

interface
//uses androidr15
{$include AndroidVersion.inc}
;

type
  ZCTIniFile = class;
  Arr1ZCTIniFile = array of ZCTIniFile;
  Arr2ZCTIniFile = array of Arr1ZCTIniFile;
  Arr3ZCTIniFile = array of Arr2ZCTIniFile;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;


{$include IniFile.inc}

implementation

end.
