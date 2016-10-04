{ Imports for Java packages/classes: com. }
unit usbapi10;
{$mode delphi}

interface

type
  CAFUUsbManager = class;
  Arr1CAFUUsbManager = array of CAFUUsbManager;
  Arr2CAFUUsbManager = array of Arr1CAFUUsbManager;
  Arr3CAFUUsbManager = array of Arr2CAFUUsbManager;

  CAFUUsbAccessory = class;
  Arr1CAFUUsbAccessory = array of CAFUUsbAccessory;
  Arr2CAFUUsbAccessory = array of Arr1CAFUUsbAccessory;
  Arr3CAFUUsbAccessory = array of Arr2CAFUUsbAccessory;

  CAIUPredicate = interface;
  Arr1CAIUPredicate = array of CAIUPredicate;
  Arr2CAIUPredicate = array of Arr1CAIUPredicate;
  Arr3CAIUPredicate = array of Arr2CAIUPredicate;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  ACIntent = class external 'android.content' name 'Intent';
  Arr1ACIntent = array of ACIntent;
  Arr2ACIntent = array of Arr1ACIntent;
  Arr3ACIntent = array of Arr2ACIntent;

  AOParcelFileDescriptor = class external 'android.os' name 'ParcelFileDescriptor';
  Arr1AOParcelFileDescriptor = array of AOParcelFileDescriptor;
  Arr2AOParcelFileDescriptor = array of Arr1AOParcelFileDescriptor;
  Arr3AOParcelFileDescriptor = array of Arr2AOParcelFileDescriptor;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  ACContext = class external 'android.content' name 'Context';
  Arr1ACContext = array of ACContext;
  Arr2ACContext = array of Arr1ACContext;
  Arr3ACContext = array of Arr2ACContext;

  AAPendingIntent = class external 'android.app' name 'PendingIntent';
  Arr1AAPendingIntent = array of AAPendingIntent;
  Arr2AAPendingIntent = array of Arr1AAPendingIntent;
  Arr3AAPendingIntent = array of Arr2AAPendingIntent;


{$include usbapi10.inc}

implementation

end.
