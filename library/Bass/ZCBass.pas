{ Imports for Java packages/classes: com.un4seen.bass. }
unit ZCBass;
{$mode delphi}

interface

type
  CUBBASS = class;
  Arr1CUBBASS = array of CUBBASS;
  Arr2CUBBASS = array of Arr1CUBBASS;
  Arr3CUBBASS = array of Arr2CUBBASS;

  JLInteger = class external 'java.lang' name 'Integer';
  Arr1JLInteger = array of JLInteger;
  Arr2JLInteger = array of Arr1JLInteger;
  Arr3JLInteger = array of Arr2JLInteger;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JNByteBuffer = class external 'java.nio' name 'ByteBuffer';
  Arr1JNByteBuffer = array of JNByteBuffer;
  Arr2JNByteBuffer = array of Arr1JNByteBuffer;
  Arr3JNByteBuffer = array of Arr2JNByteBuffer;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  JLFloat = class external 'java.lang' name 'Float';
  Arr1JLFloat = array of JLFloat;
  Arr2JLFloat = array of Arr1JLFloat;
  Arr3JLFloat = array of Arr2JLFloat;


{$include ZCBass.inc}

implementation

end.
