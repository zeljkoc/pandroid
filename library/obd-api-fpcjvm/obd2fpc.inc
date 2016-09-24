  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

  CGPOCObdCommand = class abstract external 'com.github.pires.obd.commands' name 'ObdCommand' (JLObject)
  strict protected
    var
      fbuffer: JUArrayList; external name 'buffer';
      fcmd: JLString; external name 'cmd';
      fuseImperialUnits: jboolean; external name 'useImperialUnits';
      frawData: JLString; external name 'rawData';
      fresponseDelayInMs: JLLong; external name 'responseDelayInMs';
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCObdCommand); overload;
    procedure run(para1: JIInputStream; para2: JIOutputStream); overload; virtual;  // throws java.io.IOException, java.lang.InterruptedException
  strict protected
    procedure sendCommand(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException, java.lang.InterruptedException
    procedure resendCommand(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException, java.lang.InterruptedException
    procedure readResult(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
    procedure performCalculations(); overload; virtual; abstract;
    procedure fillBuffer(); overload; virtual;
    procedure readRawData(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
  public
    function getResult(): JLString; overload; virtual;
    function getFormattedResult(): JLString; overload; virtual; abstract;
    function getCalculatedResult(): JLString; overload; virtual; abstract;
  strict protected
    function getBuffer(): JUArrayList; overload; virtual;
  public
    function useImperialUnits(): jboolean; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    procedure useImperialUnits(para1: jboolean); overload; virtual;
    function getName(): JLString; overload; virtual; abstract;
    function getResponseTimeDelay(): JLLong; overload; virtual;
    procedure setResponseTimeDelay(para1: JLLong); overload; virtual;
    function getStart(): jlong; overload; virtual;
    procedure setStart(para1: jlong); overload; virtual;
    function getEnd(): jlong; overload; virtual;
    procedure setEnd(para1: jlong); overload; virtual;
    function getCommandPID(): JLString; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  CGPOCObdMultiCommand = class external 'com.github.pires.obd.commands' name 'ObdMultiCommand' (JLObject)
  public
    constructor create(); overload;
    procedure add(para1: CGPOCObdCommand); overload; virtual;
    procedure remove(para1: CGPOCObdCommand); overload; virtual;
    procedure sendCommands(para1: JIInputStream; para2: JIOutputStream); overload; virtual;  // throws java.io.IOException, java.lang.InterruptedException
    function getFormattedResult(): JLString; overload; virtual;
  end;

  CGPOCSystemOfUnits = interface external 'com.github.pires.obd.commands' name 'SystemOfUnits' 
    function getImperialUnit(): jfloat; overload;
  end;

  CGPOUCommandAvailabilityHelper = class abstract external 'com.github.pires.obd.utils' name 'CommandAvailabilityHelper' (JLObject)
  public
    constructor create(); overload;
    class function digestAvailabilityString(para1: JLString): Arr1jint; static; overload;  // throws java.lang.IllegalArgumentException
    class function isAvailable(para1: JLString; para2: JLString; para3: jboolean): jboolean; static; overload;
    class function isAvailable(para1: JLString; para2: JLString): jboolean; static; overload;  // throws java.lang.IllegalArgumentException
    class function isAvailable(para1: JLString; para2: Arr1jint; para3: jboolean): jboolean; static; overload;
    class function isAvailable(para1: JLString; var para2: array of jint; para3: jboolean): jboolean; static; overload;
    class function isAvailable(para1: JLString; para2: Arr1jint): jboolean; static; overload;  // throws java.lang.IllegalArgumentException
    class function isAvailable(para1: JLString; var para2: array of jint): jboolean; static; overload;  // throws java.lang.IllegalArgumentException
  end;

  CGPOEAvailableCommandNames = class sealed external 'com.github.pires.obd.enums' name 'AvailableCommandNames' (JLEnum)
  public
    final class var
      fAIR_INTAKE_TEMP: CGPOEAvailableCommandNames; external name 'AIR_INTAKE_TEMP';
      fAMBIENT_AIR_TEMP: CGPOEAvailableCommandNames; external name 'AMBIENT_AIR_TEMP';
      fENGINE_COOLANT_TEMP: CGPOEAvailableCommandNames; external name 'ENGINE_COOLANT_TEMP';
      fBAROMETRIC_PRESSURE: CGPOEAvailableCommandNames; external name 'BAROMETRIC_PRESSURE';
      fFUEL_PRESSURE: CGPOEAvailableCommandNames; external name 'FUEL_PRESSURE';
      fINTAKE_MANIFOLD_PRESSURE: CGPOEAvailableCommandNames; external name 'INTAKE_MANIFOLD_PRESSURE';
      fENGINE_LOAD: CGPOEAvailableCommandNames; external name 'ENGINE_LOAD';
      fENGINE_RUNTIME: CGPOEAvailableCommandNames; external name 'ENGINE_RUNTIME';
      fENGINE_RPM: CGPOEAvailableCommandNames; external name 'ENGINE_RPM';
      fSPEED: CGPOEAvailableCommandNames; external name 'SPEED';
      fMAF: CGPOEAvailableCommandNames; external name 'MAF';
      fTHROTTLE_POS: CGPOEAvailableCommandNames; external name 'THROTTLE_POS';
      fTROUBLE_CODES: CGPOEAvailableCommandNames; external name 'TROUBLE_CODES';
      fPENDING_TROUBLE_CODES: CGPOEAvailableCommandNames; external name 'PENDING_TROUBLE_CODES';
      fPERMANENT_TROUBLE_CODES: CGPOEAvailableCommandNames; external name 'PERMANENT_TROUBLE_CODES';
      fFUEL_LEVEL: CGPOEAvailableCommandNames; external name 'FUEL_LEVEL';
      fFUEL_TYPE: CGPOEAvailableCommandNames; external name 'FUEL_TYPE';
      fFUEL_CONSUMPTION_RATE: CGPOEAvailableCommandNames; external name 'FUEL_CONSUMPTION_RATE';
      fTIMING_ADVANCE: CGPOEAvailableCommandNames; external name 'TIMING_ADVANCE';
      fDTC_NUMBER: CGPOEAvailableCommandNames; external name 'DTC_NUMBER';
      fEQUIV_RATIO: CGPOEAvailableCommandNames; external name 'EQUIV_RATIO';
      fDISTANCE_TRAVELED_AFTER_CODES_CLEARED: CGPOEAvailableCommandNames; external name 'DISTANCE_TRAVELED_AFTER_CODES_CLEARED';
      fCONTROL_MODULE_VOLTAGE: CGPOEAvailableCommandNames; external name 'CONTROL_MODULE_VOLTAGE';
      fENGINE_FUEL_RATE: CGPOEAvailableCommandNames; external name 'ENGINE_FUEL_RATE';
      fFUEL_RAIL_PRESSURE: CGPOEAvailableCommandNames; external name 'FUEL_RAIL_PRESSURE';
      fVIN: CGPOEAvailableCommandNames; external name 'VIN';
      fDISTANCE_TRAVELED_MIL_ON: CGPOEAvailableCommandNames; external name 'DISTANCE_TRAVELED_MIL_ON';
      fTIME_TRAVELED_MIL_ON: CGPOEAvailableCommandNames; external name 'TIME_TRAVELED_MIL_ON';
      fTIME_SINCE_TC_CLEARED: CGPOEAvailableCommandNames; external name 'TIME_SINCE_TC_CLEARED';
      fREL_THROTTLE_POS: CGPOEAvailableCommandNames; external name 'REL_THROTTLE_POS';
      fPIDS_01_20: CGPOEAvailableCommandNames; external name 'PIDS_01_20';
      fPIDS_21_40: CGPOEAvailableCommandNames; external name 'PIDS_21_40';
      fPIDS_41_60: CGPOEAvailableCommandNames; external name 'PIDS_41_60';
      fABS_LOAD: CGPOEAvailableCommandNames; external name 'ABS_LOAD';
      fENGINE_OIL_TEMP: CGPOEAvailableCommandNames; external name 'ENGINE_OIL_TEMP';
      fAIR_FUEL_RATIO: CGPOEAvailableCommandNames; external name 'AIR_FUEL_RATIO';
      fWIDEBAND_AIR_FUEL_RATIO: CGPOEAvailableCommandNames; external name 'WIDEBAND_AIR_FUEL_RATIO';
      fDESCRIBE_PROTOCOL: CGPOEAvailableCommandNames; external name 'DESCRIBE_PROTOCOL';
      fDESCRIBE_PROTOCOL_NUMBER: CGPOEAvailableCommandNames; external name 'DESCRIBE_PROTOCOL_NUMBER';
      fIGNITION_MONITOR: CGPOEAvailableCommandNames; external name 'IGNITION_MONITOR';
  public
    class function values(): Arr1CGPOEAvailableCommandNames; static; overload;
    class function valueOf(para1: JLString): CGPOEAvailableCommandNames; static; overload;
    function getValue(): JLString; overload; virtual; final;
  end;

  CGPOEFuelTrim = class sealed external 'com.github.pires.obd.enums' name 'FuelTrim' (JLEnum)
  public
    final class var
      fSHORT_TERM_BANK_1: CGPOEFuelTrim; external name 'SHORT_TERM_BANK_1';
      fLONG_TERM_BANK_1: CGPOEFuelTrim; external name 'LONG_TERM_BANK_1';
      fSHORT_TERM_BANK_2: CGPOEFuelTrim; external name 'SHORT_TERM_BANK_2';
      fLONG_TERM_BANK_2: CGPOEFuelTrim; external name 'LONG_TERM_BANK_2';
  public
    class function values(): Arr1CGPOEFuelTrim; static; overload;
    class function valueOf(para1: JLString): CGPOEFuelTrim; static; overload;
    class function fromValue(para1: jint): CGPOEFuelTrim; static; overload;
    function getValue(): jint; overload; virtual;
    function getBank(): JLString; overload; virtual;
    function buildObdCommand(): JLString; overload; virtual; final;
  end;

  CGPOEFuelType = class sealed external 'com.github.pires.obd.enums' name 'FuelType' (JLEnum)
  public
    final class var
      fGASOLINE: CGPOEFuelType; external name 'GASOLINE';
      fMETHANOL: CGPOEFuelType; external name 'METHANOL';
      fETHANOL: CGPOEFuelType; external name 'ETHANOL';
      fDIESEL: CGPOEFuelType; external name 'DIESEL';
      fLPG: CGPOEFuelType; external name 'LPG';
      fCNG: CGPOEFuelType; external name 'CNG';
      fPROPANE: CGPOEFuelType; external name 'PROPANE';
      fELECTRIC: CGPOEFuelType; external name 'ELECTRIC';
      fBIFUEL_GASOLINE: CGPOEFuelType; external name 'BIFUEL_GASOLINE';
      fBIFUEL_METHANOL: CGPOEFuelType; external name 'BIFUEL_METHANOL';
      fBIFUEL_ETHANOL: CGPOEFuelType; external name 'BIFUEL_ETHANOL';
      fBIFUEL_LPG: CGPOEFuelType; external name 'BIFUEL_LPG';
      fBIFUEL_CNG: CGPOEFuelType; external name 'BIFUEL_CNG';
      fBIFUEL_PROPANE: CGPOEFuelType; external name 'BIFUEL_PROPANE';
      fBIFUEL_ELECTRIC: CGPOEFuelType; external name 'BIFUEL_ELECTRIC';
      fBIFUEL_GASOLINE_ELECTRIC: CGPOEFuelType; external name 'BIFUEL_GASOLINE_ELECTRIC';
      fHYBRID_GASOLINE: CGPOEFuelType; external name 'HYBRID_GASOLINE';
      fHYBRID_ETHANOL: CGPOEFuelType; external name 'HYBRID_ETHANOL';
      fHYBRID_DIESEL: CGPOEFuelType; external name 'HYBRID_DIESEL';
      fHYBRID_ELECTRIC: CGPOEFuelType; external name 'HYBRID_ELECTRIC';
      fHYBRID_MIXED: CGPOEFuelType; external name 'HYBRID_MIXED';
      fHYBRID_REGENERATIVE: CGPOEFuelType; external name 'HYBRID_REGENERATIVE';
  public
    class function values(): Arr1CGPOEFuelType; static; overload;
    class function valueOf(para1: JLString): CGPOEFuelType; static; overload;
    class function fromValue(para1: jint): CGPOEFuelType; static; overload;
    function getValue(): jint; overload; virtual;
    function getDescription(): JLString; overload; virtual;
  end;

  CGPOEObdProtocols = class sealed external 'com.github.pires.obd.enums' name 'ObdProtocols' (JLEnum)
  public
    final class var
      fAUTO: CGPOEObdProtocols; external name 'AUTO';
      fSAE_J1850_PWM: CGPOEObdProtocols; external name 'SAE_J1850_PWM';
      fSAE_J1850_VPW: CGPOEObdProtocols; external name 'SAE_J1850_VPW';
      fISO_9141_2: CGPOEObdProtocols; external name 'ISO_9141_2';
      fISO_14230_4_KWP: CGPOEObdProtocols; external name 'ISO_14230_4_KWP';
      fISO_14230_4_KWP_FAST: CGPOEObdProtocols; external name 'ISO_14230_4_KWP_FAST';
      fISO_15765_4_CAN: CGPOEObdProtocols; external name 'ISO_15765_4_CAN';
      fISO_15765_4_CAN_B: CGPOEObdProtocols; external name 'ISO_15765_4_CAN_B';
      fISO_15765_4_CAN_C: CGPOEObdProtocols; external name 'ISO_15765_4_CAN_C';
      fISO_15765_4_CAN_D: CGPOEObdProtocols; external name 'ISO_15765_4_CAN_D';
      fSAE_J1939_CAN: CGPOEObdProtocols; external name 'SAE_J1939_CAN';
      fUSER1_CAN: CGPOEObdProtocols; external name 'USER1_CAN';
      fUSER2_CAN: CGPOEObdProtocols; external name 'USER2_CAN';
  public
    class function values(): Arr1CGPOEObdProtocols; static; overload;
    class function valueOf(para1: JLString): CGPOEObdProtocols; static; overload;
    function getValue(): jchar; overload; virtual;
  end;

  CGPOENonNumericResponseException = class external 'com.github.pires.obd.exceptions' name 'NonNumericResponseException' (JLRuntimeException)
  public
    constructor create(para1: JLString); overload;
  end;

  CGPOEResponseException = class external 'com.github.pires.obd.exceptions' name 'ResponseException' (JLRuntimeException)
  strict protected
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: jboolean); overload;
  public
    function isError(para1: JLString): jboolean; overload; virtual;
    procedure setCommand(para1: JLString); overload; virtual;
    function getMessage(): JLString; overload; virtual;
  end;

  CGPOCPercentageObdCommand = class abstract external 'com.github.pires.obd.commands' name 'PercentageObdCommand' (CGPOCObdCommand)
  strict protected
    var
      fpercentage: jfloat; external name 'percentage';
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCPercentageObdCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getPercentage(): jfloat; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  end;

  CGPOCPersistentCommand = class abstract external 'com.github.pires.obd.commands' name 'PersistentCommand' (CGPOCObdCommand)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCObdCommand); overload;
    class procedure reset(); static; overload;
    class function knows(para1: JLClass): jboolean; static; overload;
  strict protected
    procedure readResult(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
  public
    procedure run(para1: JIInputStream; para2: JIOutputStream); overload; virtual;  // throws java.io.IOException, java.lang.InterruptedException
  end;

  CGPOCCDtcNumberCommand = class external 'com.github.pires.obd.commands.control' name 'DtcNumberCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCDtcNumberCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getTotalAvailableCodes(): jint; overload; virtual;
    function getMilOn(): jboolean; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCIgnitionMonitorCommand = class external 'com.github.pires.obd.commands.control' name 'IgnitionMonitorCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCIgnitionMonitorCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  strict protected
    procedure fillBuffer(); overload; virtual;
  public
    function isIgnitionOn(): jboolean; overload; virtual;
  end;

  CGPOCCModuleVoltageCommand = class external 'com.github.pires.obd.commands.control' name 'ModuleVoltageCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCModuleVoltageCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getVoltage(): jdouble; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCPendingTroubleCodesCommand = class external 'com.github.pires.obd.commands.control' name 'PendingTroubleCodesCommand' (CGPOCObdCommand)
  strict protected
    final class var
      fdtcLetters: Arr1jchar; external name 'dtcLetters';
      fhexArray: Arr1jchar; external name 'hexArray';
  strict protected
    var
      fcodes: JLStringBuilder; external name 'codes';
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCPendingTroubleCodesCommand); overload;
  strict protected
    procedure fillBuffer(); overload; virtual;
    procedure performCalculations(); overload; virtual;
  public
    function formatResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  strict protected
    procedure readRawData(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCPermanentTroubleCodesCommand = class external 'com.github.pires.obd.commands.control' name 'PermanentTroubleCodesCommand' (CGPOCObdCommand)
  strict protected
    final class var
      fdtcLetters: Arr1jchar; external name 'dtcLetters';
      fhexArray: Arr1jchar; external name 'hexArray';
  strict protected
    var
      fcodes: JLStringBuilder; external name 'codes';
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCPermanentTroubleCodesCommand); overload;
  strict protected
    procedure fillBuffer(); overload; virtual;
    procedure performCalculations(); overload; virtual;
  public
    function formatResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  strict protected
    procedure readRawData(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCTroubleCodesCommand = class external 'com.github.pires.obd.commands.control' name 'TroubleCodesCommand' (CGPOCObdCommand)
  strict protected
    final class var
      fdtcLetters: Arr1jchar; external name 'dtcLetters';
      fhexArray: Arr1jchar; external name 'hexArray';
  strict protected
    var
      fcodes: JLStringBuilder; external name 'codes';
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCTroubleCodesCommand); overload;
  strict protected
    procedure fillBuffer(); overload; virtual;
    procedure performCalculations(); overload; virtual;
  public
    function formatResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  strict protected
    procedure readRawData(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCEMassAirFlowCommand = class external 'com.github.pires.obd.commands.engine' name 'MassAirFlowCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCEMassAirFlowCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getMAF(): jdouble; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCERPMCommand = class external 'com.github.pires.obd.commands.engine' name 'RPMCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCERPMCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getRPM(): jint; overload; virtual;
  end;

  CGPOCERuntimeCommand = class external 'com.github.pires.obd.commands.engine' name 'RuntimeCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCERuntimeCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCFAirFuelRatioCommand = class external 'com.github.pires.obd.commands.fuel' name 'AirFuelRatioCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getAirFuelRatio(): jdouble; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCFConsumptionRateCommand = class external 'com.github.pires.obd.commands.fuel' name 'ConsumptionRateCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCFConsumptionRateCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getLitersPerHour(): jfloat; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCFFindFuelTypeCommand = class external 'com.github.pires.obd.commands.fuel' name 'FindFuelTypeCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCFFindFuelTypeCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCFWidebandAirFuelRatioCommand = class external 'com.github.pires.obd.commands.fuel' name 'WidebandAirFuelRatioCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getWidebandAirFuelRatio(): jdouble; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPDescribeProtocolNumberCommand = class external 'com.github.pires.obd.commands.protocol' name 'DescribeProtocolNumberCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getObdProtocol(): CGPOEObdProtocols; overload; virtual;
  end;

  CGPOCPObdProtocolCommand = class abstract external 'com.github.pires.obd.commands.protocol' name 'ObdProtocolCommand' (CGPOCObdCommand)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCPObdProtocolCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
    procedure fillBuffer(); overload; virtual;
  public
    function getCalculatedResult(): JLString; overload; virtual;
  end;

  CGPOCPResetTroubleCodesCommand = class external 'com.github.pires.obd.commands.protocol' name 'ResetTroubleCodesCommand' (CGPOCObdCommand)
  public
    constructor create(); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCSpeedCommand = class external 'com.github.pires.obd.commands' name 'SpeedCommand' (CGPOCObdCommand, CGPOCSystemOfUnits)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCSpeedCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getMetricSpeed(): jint; overload; virtual;
    function getImperialSpeed(): jfloat; overload; virtual;
    function getImperialUnit(): jfloat; overload; virtual;
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCDistanceMILOnCommand = class external 'com.github.pires.obd.commands.control' name 'DistanceMILOnCommand' (CGPOCObdCommand, CGPOCSystemOfUnits)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCDistanceMILOnCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getImperialUnit(): jfloat; overload; virtual;
    function getKm(): jint; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCDistanceSinceCCCommand = class external 'com.github.pires.obd.commands.control' name 'DistanceSinceCCCommand' (CGPOCObdCommand, CGPOCSystemOfUnits)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCDistanceSinceCCCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getImperialUnit(): jfloat; overload; virtual;
    function getKm(): jint; overload; virtual;
    procedure setKm(para1: jint); overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPPressureCommand = class abstract external 'com.github.pires.obd.commands.pressure' name 'PressureCommand' (CGPOCObdCommand, CGPOCSystemOfUnits)
  strict protected
    var
      ftempValue: jint; external name 'tempValue';
      fpressure: jint; external name 'pressure';
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCPPressureCommand); overload;
  strict protected
    function preparePressureValue(): jint; overload; virtual;
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getMetricUnit(): jint; overload; virtual;
    function getImperialUnit(): jfloat; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
  end;

  CGPOCTTemperatureCommand = class abstract external 'com.github.pires.obd.commands.temperature' name 'TemperatureCommand' (CGPOCObdCommand, CGPOCSystemOfUnits)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCTTemperatureCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
    function getResultUnit(): JLString; overload; virtual;
    function getTemperature(): jfloat; overload; virtual;
    function getImperialUnit(): jfloat; overload; virtual;
    function getKelvin(): jfloat; overload; virtual;
    function getName(): JLString; overload; virtual; abstract;
  end;

  CGPOEBusInitException = class external 'com.github.pires.obd.exceptions' name 'BusInitException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOEMisunderstoodCommandException = class external 'com.github.pires.obd.exceptions' name 'MisunderstoodCommandException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOENoDataException = class external 'com.github.pires.obd.exceptions' name 'NoDataException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOEStoppedException = class external 'com.github.pires.obd.exceptions' name 'StoppedException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOEUnableToConnectException = class external 'com.github.pires.obd.exceptions' name 'UnableToConnectException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOEUnknownErrorException = class external 'com.github.pires.obd.exceptions' name 'UnknownErrorException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOEUnsupportedCommandException = class external 'com.github.pires.obd.exceptions' name 'UnsupportedCommandException' (CGPOEResponseException)
  public
    constructor create(); overload;
  end;

  CGPOCCEquivalentRatioCommand = class external 'com.github.pires.obd.commands.control' name 'EquivalentRatioCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCEquivalentRatioCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getRatio(): jdouble; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCTimingAdvanceCommand = class external 'com.github.pires.obd.commands.control' name 'TimingAdvanceCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCTimingAdvanceCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCEAbsoluteLoadCommand = class external 'com.github.pires.obd.commands.engine' name 'AbsoluteLoadCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCEAbsoluteLoadCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getRatio(): jdouble; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCELoadCommand = class external 'com.github.pires.obd.commands.engine' name 'LoadCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCELoadCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCEThrottlePositionCommand = class external 'com.github.pires.obd.commands.engine' name 'ThrottlePositionCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCEThrottlePositionCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCFFuelLevelCommand = class external 'com.github.pires.obd.commands.fuel' name 'FuelLevelCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getName(): JLString; overload; virtual;
    function getFuelLevel(): jfloat; overload; virtual;
  end;

  CGPOCFFuelTrimCommand = class external 'com.github.pires.obd.commands.fuel' name 'FuelTrimCommand' (CGPOCPercentageObdCommand)
  public
    constructor create(para1: CGPOEFuelTrim); overload;
    constructor create(); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getValue(): jfloat; overload; virtual; final;
    function getBank(): JLString; overload; virtual; final;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCCVinCommand = class external 'com.github.pires.obd.commands.control' name 'VinCommand' (CGPOCPersistentCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCCVinCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  strict protected
    procedure fillBuffer(); overload; virtual;
  public
    function convertHexToString(para1: JLString): JLString; overload; virtual;
  end;

  CGPOCPAvailablePidsCommand = class abstract external 'com.github.pires.obd.commands.protocol' name 'AvailablePidsCommand' (CGPOCPersistentCommand)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: CGPOCPAvailablePidsCommand); overload;
  strict protected
    procedure performCalculations(); overload; virtual;
  public
    function getFormattedResult(): JLString; overload; virtual;
    function getCalculatedResult(): JLString; overload; virtual;
  end;

  CGPOCPAdaptiveTimingCommand = class external 'com.github.pires.obd.commands.protocol' name 'AdaptiveTimingCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(para1: jint); overload;
    constructor create(para1: CGPOCPAdaptiveTimingCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPDescribeProtocolCommand = class external 'com.github.pires.obd.commands.protocol' name 'DescribeProtocolCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPEchoOffCommand = class external 'com.github.pires.obd.commands.protocol' name 'EchoOffCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPEchoOffCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPHeadersOffCommand = class external 'com.github.pires.obd.commands.protocol' name 'HeadersOffCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPHeadersOffCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPLineFeedOffCommand = class external 'com.github.pires.obd.commands.protocol' name 'LineFeedOffCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPLineFeedOffCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPObdRawCommand = class external 'com.github.pires.obd.commands.protocol' name 'ObdRawCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(para1: JLString); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPObdResetCommand = class external 'com.github.pires.obd.commands.protocol' name 'ObdResetCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPObdResetCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPObdWarmstartCommand = class external 'com.github.pires.obd.commands.protocol' name 'ObdWarmstartCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPObdWarmstartCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPSelectProtocolCommand = class external 'com.github.pires.obd.commands.protocol' name 'SelectProtocolCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(para1: CGPOEObdProtocols); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPSpacesOffCommand = class external 'com.github.pires.obd.commands.protocol' name 'SpacesOffCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPSpacesOffCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPTimeoutCommand = class external 'com.github.pires.obd.commands.protocol' name 'TimeoutCommand' (CGPOCPObdProtocolCommand)
  public
    constructor create(para1: jint); overload;
    constructor create(para1: CGPOCPTimeoutCommand); overload;
    function getFormattedResult(): JLString; overload; virtual;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPBarometricPressureCommand = class external 'com.github.pires.obd.commands.pressure' name 'BarometricPressureCommand' (CGPOCPPressureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPPressureCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPFuelPressureCommand = class external 'com.github.pires.obd.commands.pressure' name 'FuelPressureCommand' (CGPOCPPressureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPFuelPressureCommand); overload;
  strict protected
    function preparePressureValue(): jint; overload; virtual; final;
  public
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPFuelRailPressureCommand = class external 'com.github.pires.obd.commands.pressure' name 'FuelRailPressureCommand' (CGPOCPPressureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPFuelRailPressureCommand); overload;
  strict protected
    function preparePressureValue(): jint; overload; virtual; final;
  public
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPIntakeManifoldPressureCommand = class external 'com.github.pires.obd.commands.pressure' name 'IntakeManifoldPressureCommand' (CGPOCPPressureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPIntakeManifoldPressureCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCEOilTempCommand = class external 'com.github.pires.obd.commands.engine' name 'OilTempCommand' (CGPOCTTemperatureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCEOilTempCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCTAirIntakeTemperatureCommand = class external 'com.github.pires.obd.commands.temperature' name 'AirIntakeTemperatureCommand' (CGPOCTTemperatureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCTAirIntakeTemperatureCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCTAmbientAirTemperatureCommand = class external 'com.github.pires.obd.commands.temperature' name 'AmbientAirTemperatureCommand' (CGPOCTTemperatureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCTTemperatureCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCTEngineCoolantTemperatureCommand = class external 'com.github.pires.obd.commands.temperature' name 'EngineCoolantTemperatureCommand' (CGPOCTTemperatureCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCTTemperatureCommand); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPAvailablePidsCommand_01_20 = class external 'com.github.pires.obd.commands.protocol' name 'AvailablePidsCommand_01_20' (CGPOCPAvailablePidsCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPAvailablePidsCommand_01_20); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPAvailablePidsCommand_21_40 = class external 'com.github.pires.obd.commands.protocol' name 'AvailablePidsCommand_21_40' (CGPOCPAvailablePidsCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPAvailablePidsCommand_21_40); overload;
    function getName(): JLString; overload; virtual;
  end;

  CGPOCPAvailablePidsCommand_41_60 = class external 'com.github.pires.obd.commands.protocol' name 'AvailablePidsCommand_41_60' (CGPOCPAvailablePidsCommand)
  public
    constructor create(); overload;
    constructor create(para1: CGPOCPAvailablePidsCommand_41_60); overload;
    function getName(): JLString; overload; virtual;
  end;

