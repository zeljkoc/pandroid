  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

  CSRAIRfidAPI = interface external 'com.seuic.rfid.api' name 'IRfidAPI' 
    function rfidOpen(): jboolean; overload;
    function rfidClose(): jboolean; overload;
    function rfidGetHardwareVersion(): JLString; overload;
    function rfidGetFirmwareVersion(): JLString; overload;
    function rfidGetVersion(): JLString; overload;
    function rfidSetMode(para1: CSRAEProtocolMode): jboolean; overload;
    function rfidGetMode(): CSRAEProtocolMode; overload;
    function rfidSetPower(para1: jint; para2: jint): jboolean; overload;
    function rfidGetPower(para1: jint; para2: jint): jboolean; overload;
    procedure rfidSetInventoryInterval(para1: jshort); overload;
    function rfidGetInventoryInterval(): jint; overload;
    function rfidInventoryOnce(para1: CSRASEPC; para2: jint): jboolean; overload;
    procedure rfidInventoryStart(); overload;
    procedure rfidInventoryStop(); overload;
    function rfidGetTagIDCount(): jint; overload;
    function rfidGetTagIDs(): JUList; overload;
    function rfidReadTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload;
    function rfidReadTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload;
    function rfidWriteTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload;
    function rfidWriteTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload;
    function rfidLockTag(): jboolean; overload;
    function rfidKillTag(): jboolean; overload;
    function rfidDownloadFirmware(): jboolean; overload;
    function rfidIOControl(): jboolean; overload;
    function rfid14443ASelect(para1: CSRASEPC): jboolean; overload;
    function rfid14443AValidate(para1: jint; para2: CSRAEHFKeySelect; para3: Arr1jbyte): jboolean; overload;
    function rfid14443AValidate(para1: jint; para2: CSRAEHFKeySelect; var para3: array of jbyte): jboolean; overload;
    function rfid14443AReadTagData(para1: jint; para2: Arr1jbyte): jboolean; overload;
    function rfid14443AReadTagData(para1: jint; var para2: array of jbyte): jboolean; overload;
    function rfid14443AWriteTagData(para1: jint; para2: Arr1jbyte): jboolean; overload;
    function rfid14443AWriteTagData(para1: jint; var para2: array of jbyte): jboolean; overload;
    function sendCommand(): jboolean; overload;
    function rfidPSAMCMD(): jboolean; overload;
    function rfidPSAMReset(): jboolean; overload;
    function rfidPSAMBaud(): jboolean; overload;
    function rfidInitValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidInitValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidReadValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: Arr1jint): jboolean; overload;
    function rfidReadValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; var para5: array of jint): jboolean; overload;
    function rfidAddValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidAddValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidSubtractValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidSubtractValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidBackupValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function rfidBackupValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
  end;

  CSRAIRfid_M102 = interface external 'com.seuic.rfid.api' name 'IRfid_M102' 
    function GetHardwareVersion(): JLString; overload;
    function GetFirmwareVersion(): JLString; overload;
    function SetMode(para1: CSRAEProtocolMode): jboolean; overload;
    function GetMode(): CSRAEProtocolMode; overload;
    function SetPower(para1: jint; para2: jint; para3: jint; para4: jint): jboolean; overload;
    function GetPower(para1: jint; para2: CSRASTagAntennaPower; para3: jint): jboolean; overload;
    function SetAntenna(para1: jint): jboolean; overload;
    function GetAntenna(para1: jint): jboolean; overload;
    procedure SetInventoryInterval(para1: jint); overload;
    function GetInventoryInterval(): jint; overload;
    procedure SetCardManufacturer(para1: CSRAEHFCardManufacturer); overload;
    function GetCardManufacturer(): CSRAEHFCardManufacturer; overload;
    function InventoryOnce(para1: CSRASEPC; para2: jint): jboolean; overload;
    procedure InventoryStart(); overload;
    procedure InventoryStop(); overload;
    function GetTagIDCount(): jint; overload;
    function GetTagIDs(): JUList; overload;
    function ReadTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload;
    function ReadTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload;
    function WriteTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload;
    function WriteTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload;
    function LockTag(para1: jbyte; para2: Arr1jbyte; para3: jint): jboolean; overload;
    function LockTag(para1: jbyte; var para2: array of jbyte; para3: jint): jboolean; overload;
    function KillTag(para1: jbyte; para2: jbyte): jboolean; overload;
    function InventoryType_14443A(para1: CSRASEPC; para2: jint): jboolean; overload;
    function InventoryCard_14443A(para1: CSRASEPC; para2: jint): jboolean; overload;
    function Select_14443A(para1: CSRASEPC): jboolean; overload;
    function Select_14443A_Ultralight(para1: CSRASEPC): jboolean; overload;
    function Validate_14443A(para1: jint; para2: CSRAEHFKeySelect; para3: Arr1jbyte): jboolean; overload;
    function Validate_14443A(para1: jint; para2: CSRAEHFKeySelect; var para3: array of jbyte): jboolean; overload;
    function ReadTagData_14443A(para1: jint; para2: Arr1jbyte): jboolean; overload;
    function ReadTagData_14443A(para1: jint; var para2: array of jbyte): jboolean; overload;
    function WriteTagData_14443A(para1: jint; para2: Arr1jbyte): jboolean; overload;
    function WriteTagData_14443A(para1: jint; var para2: array of jbyte): jboolean; overload;
    function WriteTagData_14443A_Ultralight(para1: jint; para2: Arr1jbyte): jboolean; overload;
    function WriteTagData_14443A_Ultralight(para1: jint; var para2: array of jbyte): jboolean; overload;
    function Inventory_15693(para1: CSRASEPC; para2: jint): jboolean; overload;
    function ReadTagData_15693(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload;
    function ReadTagData_15693(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload;
    function WriteTagData_15693(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload;
    function WriteTagData_15693(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload;
    function SendCmdAndRecvResponse(para1: jbyte; para2: jint; para3: jbyte; para4: jint; para5: jint; para6: jint): jboolean; overload;
    function SendAPDU(para1: Arr1jbyte; para2: jint; para3: Arr1jbyte; para4: jint): jboolean; overload;
    function SendAPDU(var para1: array of jbyte; para2: jint; var para3: array of jbyte; para4: jint): jboolean; overload;
    function PSAM_Reset(para1: Arr1jbyte; para2: jint): jboolean; overload;
    function PSAM_Reset(var para1: array of jbyte; para2: jint): jboolean; overload;
    function PSAM_Baud(para1: jint): jboolean; overload;
    function InitValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function InitValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
    function ReadValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: Arr1jint): jboolean; overload;
    function ReadValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; var para5: array of jint): jboolean; overload;
    function AddValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function AddValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
    function SubtractValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function SubtractValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
    function BackupValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload;
    function BackupValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload;
  end;

  CSRAPowerNocify = class external 'com.seuic.rfid.api' name 'PowerNocify' (JLObject)
  public
    constructor create(para1: jint); overload;
    procedure start(); overload; virtual;
    procedure stop(); overload; virtual;
  end;

  CSRARfidPacket = class external 'com.seuic.rfid.api' name 'RfidPacket' (JLObject)
  public
    constructor create(); overload;
  end;

  CSRASEPC = class external 'com.seuic.rfid.api.structs' name 'EPC' (JLObject)
  public
    var
      fid: Arr1jbyte; external name 'id';
      flen: jint; external name 'len';
      ftype: CSRAEHFCardType; external name 'type';
      fcount: jint; external name 'count';
  public
    constructor create(); overload;
    function getId(): JLString; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
  end;

  CSRASTagAntennaPower = class external 'com.seuic.rfid.api.structs' name 'TagAntennaPower' (JLObject)
  public
    constructor create(); overload;
  end;

  CSRAUBaseUtil = class external 'com.seuic.rfid.api.util' name 'BaseUtil' (JLObject)
  public
    constructor create(); overload;
    class procedure memcpy(para1: Arr1jbyte; para2: Arr1jbyte; para3: jint); static; overload;
    class procedure memcpy(var para1: array of jbyte; var para2: array of jbyte; para3: jint); static; overload;
    class procedure memcpy(para1: Arr1jbyte; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint); static; overload;
    class procedure memcpy(var para1: array of jbyte; para2: jint; var para3: array of jbyte; para4: jint; para5: jint); static; overload;
    class function memcmp(para1: Arr1jbyte; para2: Arr1jbyte; para3: jint): jboolean; static; overload;
    class function memcmp(var para1: array of jbyte; var para2: array of jbyte; para3: jint): jboolean; static; overload;
    class function getHexString(para1: Arr1jbyte; para2: jint): JLString; static; overload;
    class function getHexString(var para1: array of jbyte; para2: jint): JLString; static; overload;
    class function getHexString(para1: Arr1jbyte; para2: jint; para3: JLString): JLString; static; overload;
    class function getHexString(var para1: array of jbyte; para2: jint; para3: JLString): JLString; static; overload;
    class function getHexByteArray(para1: JLString): Arr1jbyte; static; overload;
  end;

  CSRAULogUtil = class external 'com.seuic.rfid.api.util' name 'LogUtil' (JLObject)
  public
    constructor create(); overload;
    class procedure printExceptionStackTrace(para1: JLString; para2: JLException); static; overload;
  end;

  CSRAUMediaUtil = class external 'com.seuic.rfid.api.util' name 'MediaUtil' (JLObject)
  public
    constructor create(); overload;
    class procedure playSound(para1: ACContext); static; overload;
  end;

  CSRAEHFCardManufacturer = class sealed external 'com.seuic.rfid.api.enums' name 'HFCardManufacturer' (JLEnum)
  public
    final class var
      fCard_NXP: CSRAEHFCardManufacturer; external name 'Card_NXP';
      fCard_TI: CSRAEHFCardManufacturer; external name 'Card_TI';
  public
    class function values(): Arr1CSRAEHFCardManufacturer; static; overload;
    class function valueOf(para1: JLString): CSRAEHFCardManufacturer; static; overload;
  end;

  CSRAEHFCardType = class sealed external 'com.seuic.rfid.api.enums' name 'HFCardType' (JLEnum)
  public
    final class var
      fCard_M1_S50: CSRAEHFCardType; external name 'Card_M1_S50';
      fCard_M1_S70: CSRAEHFCardType; external name 'Card_M1_S70';
      fCard_UltraLight: CSRAEHFCardType; external name 'Card_UltraLight';
      fCard_DesFire: CSRAEHFCardType; external name 'Card_DesFire';
      fCard_MifarePro: CSRAEHFCardType; external name 'Card_MifarePro';
      fCard_I_CODE_SL2: CSRAEHFCardType; external name 'Card_I_CODE_SL2';
      fCard_Unknown: CSRAEHFCardType; external name 'Card_Unknown';
  public
    class function values(): Arr1CSRAEHFCardType; static; overload;
    class function valueOf(para1: JLString): CSRAEHFCardType; static; overload;
  end;

  CSRAEHFKeySelect = class abstract external 'com.seuic.rfid.api.enums' name 'HFKeySelect' (JLEnum)
  public
    final class var
      fKeySelectA: CSRAEHFKeySelect; external name 'KeySelectA';
      fKeySelectB: CSRAEHFKeySelect; external name 'KeySelectB';
  public
    function getValue(): jint; overload; virtual; abstract;
    class function values(): Arr1CSRAEHFKeySelect; static; overload;
    class function valueOf(para1: JLString): CSRAEHFKeySelect; static; overload;
  end;

  CSRAEProtocolMode = class abstract external 'com.seuic.rfid.api.enums' name 'ProtocolMode' (JLEnum)
  public
    final class var
      fPROTOCOL_ISO6C: CSRAEProtocolMode; external name 'PROTOCOL_ISO6C';
      fPROTOCOL_ISO6B: CSRAEProtocolMode; external name 'PROTOCOL_ISO6B';
      fPROTOCOL_ISO14443A: CSRAEProtocolMode; external name 'PROTOCOL_ISO14443A';
      fPROTOCOL_ISO14443B: CSRAEProtocolMode; external name 'PROTOCOL_ISO14443B';
      fPROTOCOL_ISO15693: CSRAEProtocolMode; external name 'PROTOCOL_ISO15693';
  public
    function getValue(): jint; overload; virtual; abstract;
    class function values(): Arr1CSRAEProtocolMode; static; overload;
    class function valueOf(para1: JLString): CSRAEProtocolMode; static; overload;
  end;

  CSRAURfidApplication = class external 'com.seuic.rfid.api.util' name 'RfidApplication' (AAApplication)
  public
    constructor create(); overload;
    procedure onCreate(); overload; virtual;
    class function getAppContext(): ACContext; static; overload;
  end;

  CSRARfidAPI = class external 'com.seuic.rfid.api' name 'RfidAPI' (JLObject, CSRAIRfidAPI)
  public
    constructor create(para1: ACContext); overload;
    function isContinued(): jboolean; overload; virtual;
    function rfidOpen(): jboolean; overload; virtual;
    function rfidClose(): jboolean; overload; virtual;
    function rfidGetHardwareVersion(): JLString; overload; virtual;
    function rfidGetFirmwareVersion(): JLString; overload; virtual;
    function rfidGetVersion(): JLString; overload; virtual;
    function rfidSetMode(para1: CSRAEProtocolMode): jboolean; overload; virtual;
    function rfidGetMode(): CSRAEProtocolMode; overload; virtual;
    function rfidSetPower(para1: jint; para2: jint): jboolean; overload; virtual;
    function rfidGetPower(para1: jint; para2: jint): jboolean; overload; virtual;
    procedure rfidSetInventoryInterval(para1: jshort); overload; virtual;
    function rfidGetInventoryInterval(): jint; overload; virtual;
    function rfidInventoryOnce(para1: CSRASEPC; para2: jint): jboolean; overload; virtual;
    procedure rfidInventoryStart(); overload; virtual;
    procedure rfidInventoryStop(); overload; virtual;
    function rfidGetTagIDCount(): jint; overload; virtual;
    function rfidGetTagIDs(): JUList; overload; virtual;
    function rfidReadTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload; virtual;
    function rfidReadTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload; virtual;
    function rfidWriteTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload; virtual;
    function rfidWriteTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload; virtual;
    function rfidLockTag(): jboolean; overload; virtual;
    function rfidKillTag(): jboolean; overload; virtual;
    function rfidDownloadFirmware(): jboolean; overload; virtual;
    function rfidIOControl(): jboolean; overload; virtual;
    function rfid14443ASelect(para1: CSRASEPC): jboolean; overload; virtual;
    function rfid14443AValidate(para1: jint; para2: CSRAEHFKeySelect; para3: Arr1jbyte): jboolean; overload; virtual;
    function rfid14443AValidate(para1: jint; para2: CSRAEHFKeySelect; var para3: array of jbyte): jboolean; overload; virtual;
    function rfid14443AReadTagData(para1: jint; para2: Arr1jbyte): jboolean; overload; virtual;
    function rfid14443AReadTagData(para1: jint; var para2: array of jbyte): jboolean; overload; virtual;
    function rfid14443AWriteTagData(para1: jint; para2: Arr1jbyte): jboolean; overload; virtual;
    function rfid14443AWriteTagData(para1: jint; var para2: array of jbyte): jboolean; overload; virtual;
    function sendCommand(): jboolean; overload; virtual;
    function rfidPSAMCMD(): jboolean; overload; virtual;
    function rfidPSAMReset(): jboolean; overload; virtual;
    function rfidPSAMBaud(): jboolean; overload; virtual;
    function rfidInitValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidInitValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidReadValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: Arr1jint): jboolean; overload; virtual;
    function rfidReadValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; var para5: array of jint): jboolean; overload; virtual;
    function rfidAddValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidAddValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidSubtractValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidSubtractValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidBackupValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function rfidBackupValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
  end;

  CSRARfid_M102 = class external 'com.seuic.rfid.api' name 'Rfid_M102' (JLObject, CSRAIRfid_M102)
  public
    function JNISendCmdAndRecvResponse(para1: Arr1jbyte; para2: jint; para3: Arr1jbyte; para4: jint; para5: Arr1jint; para6: jint): jint; overload; virtual;
    function JNISendCmdAndRecvResponse(var para1: array of jbyte; para2: jint; var para3: array of jbyte; para4: jint; var para5: array of jint; para6: jint): jint; overload; virtual;
    function JNISendCmdAndRecvResponseByChar(para1: Arr1jbyte; para2: jint; para3: Arr1jchar; para4: jint; para5: Arr1jint; para6: jint): jint; overload; virtual;
    function JNISendCmdAndRecvResponseByChar(var para1: array of jbyte; para2: jint; var para3: array of jchar; para4: jint; var para5: array of jint; para6: jint): jint; overload; virtual;
    constructor create(para1: ACContext); overload;
    function sendCmdAndRecvResponse(para1: Arr1jbyte; para2: jint; para3: Arr1jbyte; para4: jint; para5: Arr1jint; para6: jint): jboolean; overload; virtual;
    function sendCmdAndRecvResponse(var para1: array of jbyte; para2: jint; var para3: array of jbyte; para4: jint; var para5: array of jint; para6: jint): jboolean; overload; virtual;
    function isContinued(): jboolean; overload; virtual;
    function open(): jboolean; overload; virtual;
    function close(): jboolean; overload; virtual;
    function isOpen(): jboolean; overload; virtual;
    function GetHardwareVersion(): JLString; overload; virtual;
    function GetFirmwareVersion(): JLString; overload; virtual;
    function SetMode(para1: CSRAEProtocolMode): jboolean; overload; virtual;
    function GetMode(): CSRAEProtocolMode; overload; virtual;
    function SetPower(para1: jint; para2: jint; para3: jint; para4: jint): jboolean; overload; virtual;
    function GetPower(para1: jint; para2: CSRASTagAntennaPower; para3: jint): jboolean; overload; virtual;
    function SetAntenna(para1: jint): jboolean; overload; virtual;
    function GetAntenna(para1: jint): jboolean; overload; virtual;
    procedure SetInventoryInterval(para1: jint); overload; virtual;
    function GetInventoryInterval(): jint; overload; virtual;
    procedure SetCardManufacturer(para1: CSRAEHFCardManufacturer); overload; virtual;
    function GetCardManufacturer(): CSRAEHFCardManufacturer; overload; virtual;
    function InventoryOnce(para1: CSRASEPC; para2: jint): jboolean; overload; virtual;
    procedure InventoryStart(); overload; virtual;
    procedure InventoryStop(); overload; virtual;
    function GetTagIDCount(): jint; overload; virtual;
    function GetTagIDs(): JUList; overload; virtual;
    function ReadTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload; virtual;
    function ReadTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload; virtual;
    function WriteTagData(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload; virtual;
    function WriteTagData(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload; virtual;
    function LockTag(para1: jbyte; para2: Arr1jbyte; para3: jint): jboolean; overload; virtual;
    function LockTag(para1: jbyte; var para2: array of jbyte; para3: jint): jboolean; overload; virtual;
    function KillTag(para1: jbyte; para2: jbyte): jboolean; overload; virtual;
    function InventoryType_14443A(para1: CSRASEPC; para2: jint): jboolean; overload; virtual;
    function InventoryCard_14443A(para1: CSRASEPC; para2: jint): jboolean; overload; virtual;
    function Select_14443A(para1: CSRASEPC): jboolean; overload; virtual;
    function Select_14443A_Ultralight(para1: CSRASEPC): jboolean; overload; virtual;
    function Validate_14443A(para1: jint; para2: CSRAEHFKeySelect; para3: Arr1jbyte): jboolean; overload; virtual;
    function Validate_14443A(para1: jint; para2: CSRAEHFKeySelect; var para3: array of jbyte): jboolean; overload; virtual;
    function ReadTagData_14443A(para1: jint; para2: Arr1jbyte): jboolean; overload; virtual;
    function ReadTagData_14443A(para1: jint; var para2: array of jbyte): jboolean; overload; virtual;
    function WriteTagData_14443A(para1: jint; para2: Arr1jbyte): jboolean; overload; virtual;
    function WriteTagData_14443A(para1: jint; var para2: array of jbyte): jboolean; overload; virtual;
    function WriteTagData_14443A_Ultralight(para1: jint; para2: Arr1jbyte): jboolean; overload; virtual;
    function WriteTagData_14443A_Ultralight(para1: jint; var para2: array of jbyte): jboolean; overload; virtual;
    function Inventory_15693(para1: CSRASEPC; para2: jint): jboolean; overload; virtual;
    function ReadTagData_15693(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload; virtual;
    function ReadTagData_15693(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload; virtual;
    function WriteTagData_15693(para1: CSRASEPC; para2: Arr1jbyte; para3: jint; para4: jint; para5: jint; para6: Arr1jbyte): jboolean; overload; virtual;
    function WriteTagData_15693(para1: CSRASEPC; var para2: array of jbyte; para3: jint; para4: jint; para5: jint; var para6: array of jbyte): jboolean; overload; virtual;
    function SendCmdAndRecvResponse_(para1: jbyte; para2: jint; para3: jbyte; para4: jint; para5: jint; para6: jint): jboolean; external name 'SendCmdAndRecvResponse'; overload; virtual;
    function SendAPDU(para1: Arr1jbyte; para2: jint; para3: Arr1jbyte; para4: jint): jboolean; overload; virtual;
    function SendAPDU(var para1: array of jbyte; para2: jint; var para3: array of jbyte; para4: jint): jboolean; overload; virtual;
    function PSAM_Reset(para1: Arr1jbyte; para2: jint): jboolean; overload; virtual;
    function PSAM_Reset(var para1: array of jbyte; para2: jint): jboolean; overload; virtual;
    function PSAM_Baud(para1: jint): jboolean; overload; virtual;
    function InitValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function InitValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function ReadValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: Arr1jint): jboolean; overload; virtual;
    function ReadValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; var para5: array of jint): jboolean; overload; virtual;
    function AddValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function AddValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function SubtractValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function SubtractValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function BackupValue(para1: CSRASEPC; para2: jint; para3: Arr1jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
    function BackupValue(para1: CSRASEPC; para2: jint; var para3: array of jbyte; para4: jint; para5: jint): jboolean; overload; virtual;
  end;

