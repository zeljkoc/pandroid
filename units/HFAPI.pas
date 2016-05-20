{ Imports for Java packages/classes: com. }
unit HFAPI;
{$mode delphi}

interface
uses androidr15;

type
  CSRARfidPacket = class;
  Arr1CSRARfidPacket = array of CSRARfidPacket;
  Arr2CSRARfidPacket = array of Arr1CSRARfidPacket;
  Arr3CSRARfidPacket = array of Arr2CSRARfidPacket;

  CSRAUMediaUtil = class;
  Arr1CSRAUMediaUtil = array of CSRAUMediaUtil;
  Arr2CSRAUMediaUtil = array of Arr1CSRAUMediaUtil;
  Arr3CSRAUMediaUtil = array of Arr2CSRAUMediaUtil;

  CSRAEHFKeySelect = class;
  Arr1CSRAEHFKeySelect = array of CSRAEHFKeySelect;
  Arr2CSRAEHFKeySelect = array of Arr1CSRAEHFKeySelect;
  Arr3CSRAEHFKeySelect = array of Arr2CSRAEHFKeySelect;

  CSRAEHFCardManufacturer = class;
  Arr1CSRAEHFCardManufacturer = array of CSRAEHFCardManufacturer;
  Arr2CSRAEHFCardManufacturer = array of Arr1CSRAEHFCardManufacturer;
  Arr3CSRAEHFCardManufacturer = array of Arr2CSRAEHFCardManufacturer;

  CSRASEPC = class;
  Arr1CSRASEPC = array of CSRASEPC;
  Arr2CSRASEPC = array of Arr1CSRASEPC;
  Arr3CSRASEPC = array of Arr2CSRASEPC;

  CSRARfidAPI = class;
  Arr1CSRARfidAPI = array of CSRARfidAPI;
  Arr2CSRARfidAPI = array of Arr1CSRARfidAPI;
  Arr3CSRARfidAPI = array of Arr2CSRARfidAPI;

  CSRAURfidApplication = class;
  Arr1CSRAURfidApplication = array of CSRAURfidApplication;
  Arr2CSRAURfidApplication = array of Arr1CSRAURfidApplication;
  Arr3CSRAURfidApplication = array of Arr2CSRAURfidApplication;

  CSRAEProtocolMode = class;
  Arr1CSRAEProtocolMode = array of CSRAEProtocolMode;
  Arr2CSRAEProtocolMode = array of Arr1CSRAEProtocolMode;
  Arr3CSRAEProtocolMode = array of Arr2CSRAEProtocolMode;

  CSRAULogUtil = class;
  Arr1CSRAULogUtil = array of CSRAULogUtil;
  Arr2CSRAULogUtil = array of Arr1CSRAULogUtil;
  Arr3CSRAULogUtil = array of Arr2CSRAULogUtil;

  CSRAEHFCardType = class;
  Arr1CSRAEHFCardType = array of CSRAEHFCardType;
  Arr2CSRAEHFCardType = array of Arr1CSRAEHFCardType;
  Arr3CSRAEHFCardType = array of Arr2CSRAEHFCardType;

  CSRAUBaseUtil = class;
  Arr1CSRAUBaseUtil = array of CSRAUBaseUtil;
  Arr2CSRAUBaseUtil = array of Arr1CSRAUBaseUtil;
  Arr3CSRAUBaseUtil = array of Arr2CSRAUBaseUtil;

  CSRARfid_M102 = class;
  Arr1CSRARfid_M102 = array of CSRARfid_M102;
  Arr2CSRARfid_M102 = array of Arr1CSRARfid_M102;
  Arr3CSRARfid_M102 = array of Arr2CSRARfid_M102;

  CSRASTagAntennaPower = class;
  Arr1CSRASTagAntennaPower = array of CSRASTagAntennaPower;
  Arr2CSRASTagAntennaPower = array of Arr1CSRASTagAntennaPower;
  Arr3CSRASTagAntennaPower = array of Arr2CSRASTagAntennaPower;

  CSRAPowerNocify = class;
  Arr1CSRAPowerNocify = array of CSRAPowerNocify;
  Arr2CSRAPowerNocify = array of Arr1CSRAPowerNocify;
  Arr3CSRAPowerNocify = array of Arr2CSRAPowerNocify;

  CSRAIRfid_M102 = interface;
  Arr1CSRAIRfid_M102 = array of CSRAIRfid_M102;
  Arr2CSRAIRfid_M102 = array of Arr1CSRAIRfid_M102;
  Arr3CSRAIRfid_M102 = array of Arr2CSRAIRfid_M102;

  CSRAIRfidAPI = interface;
  Arr1CSRAIRfidAPI = array of CSRAIRfidAPI;
  Arr2CSRAIRfidAPI = array of Arr1CSRAIRfidAPI;
  Arr3CSRAIRfidAPI = array of Arr2CSRAIRfidAPI;

  CAIUPredicate = interface;
  Arr1CAIUPredicate = array of CAIUPredicate;
  Arr2CAIUPredicate = array of Arr1CAIUPredicate;
  Arr3CAIUPredicate = array of Arr2CAIUPredicate;

  JLException = class external 'java.lang' name 'Exception';
  Arr1JLException = array of JLException;
  Arr2JLException = array of Arr1JLException;
  Arr3JLException = array of Arr2JLException;

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

  ACContext = class external 'android.content' name 'Context';
  Arr1ACContext = array of ACContext;
  Arr2ACContext = array of Arr1ACContext;
  Arr3ACContext = array of Arr2ACContext;

  AAApplication = class external 'android.app' name 'Application';
  Arr1AAApplication = array of AAApplication;
  Arr2AAApplication = array of Arr1AAApplication;
  Arr3AAApplication = array of Arr2AAApplication;

  JUList = interface external 'java.util' name 'List';
  Arr1JUList = array of JUList;
  Arr2JUList = array of Arr1JUList;
  Arr3JUList = array of Arr2JUList;


{$include HFAPI.inc}

implementation

end.
