  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

  CWDIUHFDao = interface external 'com.wpx.dao' name 'IUHFDao' 
    function connect(para1: jint; para2: jint; para3: jint; para4: jint): jint; overload;
    function disconnect(para1: jint; para2: jint): jint; overload;
    function get_Version(): jint; overload;
    function find_Tag(): jint; overload;
    function read_Tag_Block(): jint; overload;
    function write_Tag_Block(): jint; overload;
    function lock_Tag(): jint; overload;
    function change_EPC(): jint; overload;
    function setPA_Output(para1: jint; para2: jint): jint; overload;
    function setC1G2_Tag_Speed(para1: jint; para2: jint): jint; overload;
    function fw_Upgrage(para1: jint): jint; overload;
    function set_Channel_Type(para1: jint; para2: jint; para3: jint): jint; overload;
    function Write_Pwd(): jint; overload;
    function Reset_Pwd(): jint; overload;
    function Lock_Tag_Memory(): jint; overload;
    function MS_Channel_Calibration(para1: jint; para2: jint): jint; overload;
  end;

  CWSIUHFService = interface external 'com.wpx.service' name 'IUHFService' 
    function connected(para1: jint): CWBResultInfo; overload;
    procedure disConnected(); overload;
    function getVersion(): JLString; overload;
    function getEPCOne(): JLString; overload;
    function getEPCList(para1: jint): CWBResultInfo; overload;
    function readUser(para1: jint; para2: JLString; para3: jint): CWBResultInfo; overload;
    function writeUser(para1: jint; para2: JLString; para3: JLString): CWBResultInfo; overload;
    function readBank1(para1: jint; para2: JLString; para3: jint): CWBResultInfo; overload;
    function writeBank1(para1: jint; para2: JLString; para3: JLString): CWBResultInfo; overload;
    function readReserve(para1: jint; para2: JLString): CWBResultInfo; overload;
    function writeReserve(para1: jint; para2: JLString; para3: JLString): CWBResultInfo; overload;
    function resetPassword(para1: JLString; para2: JLString): CWBResultInfo; overload;
    function writePassword(para1: JLString; para2: JLString): CWBResultInfo; overload;
    function readTID(para1: JLString): CWBResultInfo; overload;
    function changeEPC(para1: JLString; para2: JLString): CWBResultInfo; overload;
    function lockTag(para1: JLString; para2: JLString): CWBResultInfo; overload;
    function set_Channel_Type(para1: jint; para2: jint): CWBResultInfo; overload;
    function Channel_Calibration(para1: jint): CWBResultInfo; overload;
    function fw_Upgrage(): CWBResultInfo; overload;
    function setC1G2_Tag_Speed(): jint; overload;
    function setPA_Output(para1: jint): jint; overload;
    procedure setBLF(para1: jint); overload;
    procedure setTagType(para1: jint); overload;
    procedure setMemoryType(para1: jint); overload;
    procedure setQValue(para1: jint); overload;
    function getQValue(): jint; overload;
    function setModulation(para1: jint): Arr1JLString; overload;
    procedure saveSelectedEPC(para1: JLString); overload;
    function getSelectedEPC(): JLString; overload;
    function getString(para1: JLString): JLString; overload;
    function CodeToChinese(para1: JLString): JLString; overload;
    procedure setDeviceType(para1: jint); overload;
  end;

  CWUGlobalUtil = class external 'com.wpx.util' name 'GlobalUtil' (JLObject)
  public
    class var
      fdev_type: jint; external name 'dev_type';
  public
    const
      DELAY_CLICK = 500;
  public
    class var
      fop: CCUmanager; external name 'op';
  public
    const
      MAX_EPC_LEN = 62;
  public
    class var
      faccessEPC: Arr1jbyte; external name 'accessEPC';
      faccessEPC_LEN: jint; external name 'accessEPC_LEN';
      fisFoundEPC: jboolean; external name 'isFoundEPC';
      fisSelectEPC: jboolean; external name 'isSelectEPC';
      fgintAntHandle: jint; external name 'gintAntHandle';
      fgQvalue: jint; external name 'gQvalue';
      fgBLF: jint; external name 'gBLF';
      fgModulation: jint; external name 'gModulation';
      fgFrequencyIndex: jint; external name 'gFrequencyIndex';
      fgChannelType: jint; external name 'gChannelType';
      fisConnected: jboolean; external name 'isConnected';
      fconnectParamersV1: Arr1jint; external name 'connectParamersV1';
      fconnectParamersV2: Arr1jint; external name 'connectParamersV2';
      faryFoundEPC: Arr2jbyte; external name 'aryFoundEPC';
      faryFoundEPC_LEN: Arr1jint; external name 'aryFoundEPC_LEN';
      fgScanCnt: jint; external name 'gScanCnt';
      fgEPClist: Arr1JLString; external name 'gEPClist';
      fstrFoundEPCs: Arr1JLString; external name 'strFoundEPCs';
  public
    constructor create(); overload;
    class function toHex(para1: jbyte): JLString; static; overload;
    class procedure delay(); static; overload;
    class procedure delay(para1: jint); static; overload;
    class function accessEPC2HexString(): JLString; static; overload;
    class procedure hexEPC2accessBytes(para1: JLString); static; overload;
  end;

  CCUmanager = class external 'com.cilico.UHFlib' name 'manager' (JLObject)
  public
    type
      InnerBaudRateID = class;
      Arr1InnerBaudRateID = array of InnerBaudRateID;
      Arr2InnerBaudRateID = array of Arr1InnerBaudRateID;
      Arr3InnerBaudRateID = array of Arr2InnerBaudRateID;
      InnerC1G2_BLF_et_ = class;
      Arr1InnerC1G2_BLF_et_ = array of InnerC1G2_BLF_et_;
      Arr2InnerC1G2_BLF_et_ = array of Arr1InnerC1G2_BLF_et_;
      Arr3InnerC1G2_BLF_et_ = array of Arr2InnerC1G2_BLF_et_;
      InnerC1G2_Inventory_Mode_et_ = class;
      Arr1InnerC1G2_Inventory_Mode_et_ = array of InnerC1G2_Inventory_Mode_et_;
      Arr2InnerC1G2_Inventory_Mode_et_ = array of Arr1InnerC1G2_Inventory_Mode_et_;
      Arr3InnerC1G2_Inventory_Mode_et_ = array of Arr2InnerC1G2_Inventory_Mode_et_;
      InnerC1G2_Mem_Type_et_ = class;
      Arr1InnerC1G2_Mem_Type_et_ = array of InnerC1G2_Mem_Type_et_;
      Arr2InnerC1G2_Mem_Type_et_ = array of Arr1InnerC1G2_Mem_Type_et_;
      Arr3InnerC1G2_Mem_Type_et_ = array of Arr2InnerC1G2_Mem_Type_et_;
      InnerC1G2_Mod_Type_et_ = class;
      Arr1InnerC1G2_Mod_Type_et_ = array of InnerC1G2_Mod_Type_et_;
      Arr2InnerC1G2_Mod_Type_et_ = array of Arr1InnerC1G2_Mod_Type_et_;
      Arr3InnerC1G2_Mod_Type_et_ = array of Arr2InnerC1G2_Mod_Type_et_;
      InnerChannel_Type_e_ = class;
      Arr1InnerChannel_Type_e_ = array of InnerChannel_Type_e_;
      Arr2InnerChannel_Type_e_ = array of Arr1InnerChannel_Type_e_;
      Arr3InnerChannel_Type_e_ = array of Arr2InnerChannel_Type_e_;
      InnerHF693_Quiet_Mode_et_ = class;
      Arr1InnerHF693_Quiet_Mode_et_ = array of InnerHF693_Quiet_Mode_et_;
      Arr2InnerHF693_Quiet_Mode_et_ = array of Arr1InnerHF693_Quiet_Mode_et_;
      Arr3InnerHF693_Quiet_Mode_et_ = array of Arr2InnerHF693_Quiet_Mode_et_;
      InnerTag_Type_et_ = class;
      Arr1InnerTag_Type_et_ = array of InnerTag_Type_et_;
      Arr2InnerTag_Type_et_ = array of Arr1InnerTag_Type_et_;
      Arr3InnerTag_Type_et_ = array of Arr2InnerTag_Type_et_;
      Inner_ISO15693 = class;
      Arr1Inner_ISO15693 = array of Inner_ISO15693;
      Arr2Inner_ISO15693 = array of Arr1Inner_ISO15693;
      Arr3Inner_ISO15693 = array of Arr2Inner_ISO15693;
      InnerBaudRateID = class sealed external 'com.cilico.UHFlib' name 'BaudRateID' (JLEnum)
      public
        final class var
          fB9600: CCUmanager.InnerBaudRateID; external name 'B9600';
          fB19200: CCUmanager.InnerBaudRateID; external name 'B19200';
          fB38400: CCUmanager.InnerBaudRateID; external name 'B38400';
          fB57600: CCUmanager.InnerBaudRateID; external name 'B57600';
          fB115200: CCUmanager.InnerBaudRateID; external name 'B115200';
          fB230400: CCUmanager.InnerBaudRateID; external name 'B230400';
      public
        class function values(): CCUmanager.Arr1InnerBaudRateID; static; overload;
        class function valueOf(para1: JLString): CCUmanager.InnerBaudRateID; static; overload;
      end;

      InnerC1G2_BLF_et_ = class external 'com.cilico.UHFlib' name 'C1G2_BLF_et_' (JLObject)
      public
        const
          C1G2_BLF_640kbps = 0;
          C1G2_BLF_320kbps = 1;
          C1G2_BLF_200kbps = 2;
          C1G2_BLF_160kbps = 3;
          C1G2_BLF_80kbps = 4;
          C1G2_BLF_Max = 5;
      public
        constructor create(); overload;
      end;

      InnerC1G2_Inventory_Mode_et_ = class external 'com.cilico.UHFlib' name 'C1G2_Inventory_Mode_et_' (JLObject)
      public
        const
          C1G2_Inv_Normal = 0;
          C1G2_Inv_Turbo = 1;
          C1G2_Inv_Max = 2;
      public
        constructor create(); overload;
      end;

      InnerC1G2_Mem_Type_et_ = class external 'com.cilico.UHFlib' name 'C1G2_Mem_Type_et_' (JLObject)
      public
        const
          C1G2_MEM_TYPE_RESERVED = 0;
          C1G2_MEM_TYPE_EPC = 1;
          C1G2_MEM_TYPE_TID = 2;
          C1G2_MEM_TYPE_USER = 3;
          C1G2_MEM_TYPE_MAX = 4;
      public
        constructor create(); overload;
      end;

      InnerC1G2_Mod_Type_et_ = class external 'com.cilico.UHFlib' name 'C1G2_Mod_Type_et_' (JLObject)
      public
        const
          C1G2_MOD_FM0 = 0;
          C1G2_MOD_MILLER2 = 1;
          C1G2_MOD_MILLER4 = 2;
          C1G2_MOD_MILLER8 = 3;
          C1G2_MOD_MAX = 4;
      public
        constructor create(); overload;
      end;

      InnerChannel_Type_e_ = class external 'com.cilico.UHFlib' name 'Channel_Type_e_' (JLObject)
      public
        const
          CH_TYPE_HOPPING = 0;
          CH_TYPE_FIXED = 1;
      public
        constructor create(); overload;
      end;

      InnerHF693_Quiet_Mode_et_ = class external 'com.cilico.UHFlib' name 'HF693_Quiet_Mode_et_' (JLObject)
      public
        const
          HF693_Not_Quiet = 0;
          HF693_Quiet = 1;
          HF693_Max = 2;
      public
        constructor create(); overload;
      end;

      InnerTag_Type_et_ = class external 'com.cilico.UHFlib' name 'Tag_Type_et_' (JLObject)
      public
        const
          TAG_TYPE_ALL = 0;
          TAG_TYPE_ISO15693 = 1;
          TAG_TYPE_C1G2 = 2;
          TAG_TYPE_ISO15693_CUSTOM = 3;
          TAG_TYPE_C1G2_CUSTOM = 4;
          TAG_TYPE_ISO14443A_UltraLight = 5;
          TAG_TYPE_ISO14443A_Topaz = 6;
          TAG_TYPE_MAX = 7;
      public
        constructor create(); overload;
      end;

      Inner_ISO15693 = class external 'com.cilico.UHFlib' name '_ISO15693' 
      end;

  public
    var
      ftagType: Arr1JLString; external name 'tagType';
      ftableHopping: Arr1JLString; external name 'tableHopping';
      ftableFreq: Arr1JLString; external name 'tableFreq';
      ftableBLFFM0: Arr1JLString; external name 'tableBLFFM0';
      ftableBLFMiller2: Arr1JLString; external name 'tableBLFMiller2';
      ftableBLFMiller4: Arr1JLString; external name 'tableBLFMiller4';
      ftableBLFMiller8: Arr1JLString; external name 'tableBLFMiller8';
      ftableModulation: Arr1JLString; external name 'tableModulation';
      ftablePowerLevel: Arr1JLString; external name 'tablePowerLevel';
      fHandle: jint; external name 'Handle';
      fMajor: jint; external name 'Major';
      fMinor: jint; external name 'Minor';
      fMonth: jbyte; external name 'Month';
      fDay: jbyte; external name 'Day';
      fYear: jint; external name 'Year';
      fHour: jbyte; external name 'Hour';
      fMinute: jbyte; external name 'Minute';
      fSecond: jbyte; external name 'Second';
      fSerial_Number: Arr1jbyte; external name 'Serial_Number';
      fTag_Type: jint; external name 'Tag_Type';
      fMem_Bank: jint; external name 'Mem_Bank';
      fBLF: jint; external name 'BLF';
      fModulation: jint; external name 'Modulation';
      fSearch_Mode: jint; external name 'Search_Mode';
      fPointer: jint; external name 'Pointer';
      fAnt_Mask: jint; external name 'Ant_Mask';
      fAccessPasswd: Arr1jbyte; external name 'AccessPasswd';
      fRead_Bank_Type: jint; external name 'Read_Bank_Type';
      fWrite_Bank_Type: jint; external name 'Write_Bank_Type';
      fQ: jint; external name 'Q';
      fPilotTone: jint; external name 'PilotTone';
      fInventoryTimes: jint; external name 'InventoryTimes';
      fISO15693: CCUmanager.Inner_ISO15693; external name 'ISO15693';
      fHF693_Quiet_Mode: jint; external name 'HF693_Quiet_Mode';
      fMask_Len: jint; external name 'Mask_Len';
      fMask: Arr1jbyte; external name 'Mask';
      fResp_Buf: Arr1jbyte; external name 'Resp_Buf';
      fResp_Len: jint; external name 'Resp_Len';
      fTag_Count: jint; external name 'Tag_Count';
      fTag_Model: jchar; external name 'Tag_Model';
      fCus_Category: jchar; external name 'Cus_Category';
      fPWD: Arr1jchar; external name 'PWD';
      fStart_Block: jint; external name 'Start_Block';
      fRead_Len: jint; external name 'Read_Len';
      fWrite_Buf: Arr1jbyte; external name 'Write_Buf';
      fWrite_Len: jint; external name 'Write_Len';
      fNew_EPC: Arr1jbyte; external name 'New_EPC';
      fNew_EPC_LEN: jint; external name 'New_EPC_LEN';
      fKill_mask: jbyte; external name 'Kill_mask';
      fAccess_mask: jbyte; external name 'Access_mask';
      fEPC_mask: jbyte; external name 'EPC_mask';
      fTID_mask: jbyte; external name 'TID_mask';
      fUser_mask: jbyte; external name 'User_mask';
      fKill_action: jbyte; external name 'Kill_action';
      fAccess_action: jbyte; external name 'Access_action';
      fEPC_action: jbyte; external name 'EPC_action';
      fTID_action: jbyte; external name 'TID_action';
      fUser_action: jbyte; external name 'User_action';
  public
    constructor create(); overload;
    function MS_Connect(para1: jint; para2: jint; para3: jint; para4: jint): jint; overload; virtual;
    function MS_Disconnect(para1: jint; para2: jint): jint; overload; virtual;
    function MS_Get_Version(): jint; overload; virtual;
    function MS_Find_Tag(): jint; overload; virtual;
    function MS_Read_Tag_Block(): jint; overload; virtual;
    function MS_Write_Tag_Block(): jint; overload; virtual;
    function MS_Set_Channel_Type(para1: jint; para2: jint; para3: jint): jint; overload; virtual;
    function MS_Set_C1G2_Tag_Speed(para1: jint; para2: jint): jint; overload; virtual;
    function MS_Set_PA_Output(para1: jint; para2: jint): jint; overload; virtual;
    function MS_FW_Upgrage(para1: jint): jint; overload; virtual;
    function MS_Change_EPC(): jint; overload; virtual;
    function MS_Lock_Tag(): jint; overload; virtual;
    function Write_Pwd(): jint; overload; virtual;
    function Reset_Pwd(): jint; overload; virtual;
    function Secured_Write(): jint; overload; virtual;
    function Secured_Read(): jint; overload; virtual;
    function Lock_Tag_Memory(): jint; overload; virtual;
    function MS_Erase_Tag_Block(): jint; overload; virtual;
    function MS_Channel_Calibration(para1: jint; para2: jint): jint; overload; virtual;
  end;

  CWBResultInfo = class external 'com.wpx.bean' name 'ResultInfo' (JLObject, JISerializable)
  public
    constructor create(); overload;
    function getValues(): JUList; overload; virtual;
    procedure setValues(para1: JUList); overload; virtual;
    function getResult(): jint; overload; virtual;
    procedure setResult(para1: jint); overload; virtual;
    function getErrInfo(): JLString; overload; virtual;
    procedure setErrInfo(para1: JLString); overload; virtual;
    function getResultValue(): JLString; overload; virtual;
    procedure setResultValue(para1: JLString); overload; virtual;
  end;

  CWDIUHFDaoImpl = class external 'com.wpx.dao.impl' name 'UHFDaoImpl' (JLObject, CWDIUHFDao)
  public
    constructor create(); overload;
    function connect(para1: jint; para2: jint; para3: jint; para4: jint): jint; overload; virtual;
    function disconnect(para1: jint; para2: jint): jint; overload; virtual;
    function get_Version(): jint; overload; virtual;
    function fw_Upgrage(para1: jint): jint; overload; virtual;
    function read_Tag_Block(): jint; overload; virtual;
    function write_Tag_Block(): jint; overload; virtual;
    function setC1G2_Tag_Speed(para1: jint; para2: jint): jint; overload; virtual;
    function setPA_Output(para1: jint; para2: jint): jint; overload; virtual;
    function lock_Tag(): jint; overload; virtual;
    function change_EPC(): jint; overload; virtual;
    function find_Tag(): jint; overload; virtual;
    function set_Channel_Type(para1: jint; para2: jint; para3: jint): jint; overload; virtual;
    function Write_Pwd(): jint; overload; virtual;
    function Reset_Pwd(): jint; overload; virtual;
    function Lock_Tag_Memory(): jint; overload; virtual;
    function MS_Channel_Calibration(para1: jint; para2: jint): jint; overload; virtual;
  end;

  CWSIUHFServiceImpl = class external 'com.wpx.service.impl' name 'UHFServiceImpl' (JLObject, CWSIUHFService)
  public
    constructor create(); overload;
    function connected(para1: jint): CWBResultInfo; overload; virtual;
    function getVersion(): JLString; overload; virtual;
    procedure disConnected(); overload; virtual;
    function getEPCOne(): JLString; overload; virtual;
    function getEPCList(para1: jint): CWBResultInfo; overload; virtual;
    function readBank1(para1: jint; para2: JLString; para3: jint): CWBResultInfo; overload; virtual;
    function readUser(para1: jint; para2: JLString; para3: jint): CWBResultInfo; overload; virtual;
    function writeUser(para1: jint; para2: JLString; para3: JLString): CWBResultInfo; overload; virtual;
    function writeBank1(para1: jint; para2: JLString; para3: JLString): CWBResultInfo; overload; virtual;
    procedure setTagType(para1: jint); overload; virtual;
    procedure setMemoryType(para1: jint); overload; virtual;
    function setC1G2_Tag_Speed(): jint; overload; virtual;
    function setPA_Output(para1: jint): jint; overload; virtual;
    function lockTag(para1: JLString; para2: JLString): CWBResultInfo; overload; virtual;
    function changeEPC(para1: JLString; para2: JLString): CWBResultInfo; overload; virtual;
    function resetPassword(para1: JLString; para2: JLString): CWBResultInfo; overload; virtual;
    function writePassword(para1: JLString; para2: JLString): CWBResultInfo; overload; virtual;
    function readReserve(para1: jint; para2: JLString): CWBResultInfo; overload; virtual;
    function writeReserve(para1: jint; para2: JLString; para3: JLString): CWBResultInfo; overload; virtual;
    function readTID(para1: JLString): CWBResultInfo; overload; virtual;
    function fw_Upgrage(): CWBResultInfo; overload; virtual;
    procedure setQValue(para1: jint); overload; virtual;
    function getQValue(): jint; overload; virtual;
    function set_Channel_Type(para1: jint; para2: jint): CWBResultInfo; overload; virtual;
    function Channel_Calibration(para1: jint): CWBResultInfo; overload; virtual;
    procedure setBLF(para1: jint); overload; virtual;
    function setModulation(para1: jint): Arr1JLString; overload; virtual;
    procedure saveSelectedEPC(para1: JLString); overload; virtual;
    function getSelectedEPC(): JLString; overload; virtual;
    function bytes2HexString(para1: jbyte): JLString; overload; virtual;
    function bytes2HexString(para1: Arr1jbyte): JLString; overload; virtual;
    function bytes2HexString(var para1: array of jbyte): JLString; overload; virtual;
    function getString(para1: JLString): JLString; overload; virtual;
    function hex2Bytes(para1: JLString): Arr1jbyte; overload; virtual;
    function CodeToChinese(para1: JLString): JLString; overload; virtual;
    procedure setDeviceType(para1: jint); overload; virtual;
  end;

