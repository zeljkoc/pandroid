  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

  CMADBarCodeReader = class external 'com.motorolasolutions.adc.decoder' name 'BarCodeReader' (JLObject)
  public
    type
      InnerEventHandler = class;
      Arr1InnerEventHandler = array of InnerEventHandler;
      Arr2InnerEventHandler = array of Arr1InnerEventHandler;
      Arr3InnerEventHandler = array of Arr2InnerEventHandler;
      InnerEventHandler = class external 'com.motorolasolutions.adc.decoder' name 'EventHandler' 
      end;

  public
    type
      InnerAutoFocusCallback = interface;
      Arr1InnerAutoFocusCallback = array of InnerAutoFocusCallback;
      Arr2InnerAutoFocusCallback = array of Arr1InnerAutoFocusCallback;
      Arr3InnerAutoFocusCallback = array of Arr2InnerAutoFocusCallback;
      InnerDecodeCallback = interface;
      Arr1InnerDecodeCallback = array of InnerDecodeCallback;
      Arr2InnerDecodeCallback = array of Arr1InnerDecodeCallback;
      Arr3InnerDecodeCallback = array of Arr2InnerDecodeCallback;
      InnerErrorCallback = interface;
      Arr1InnerErrorCallback = array of InnerErrorCallback;
      Arr2InnerErrorCallback = array of Arr1InnerErrorCallback;
      Arr3InnerErrorCallback = array of Arr2InnerErrorCallback;
      InnerOnZoomChangeListener = interface;
      Arr1InnerOnZoomChangeListener = array of InnerOnZoomChangeListener;
      Arr2InnerOnZoomChangeListener = array of Arr1InnerOnZoomChangeListener;
      Arr3InnerOnZoomChangeListener = array of Arr2InnerOnZoomChangeListener;
      InnerParamNum = class;
      Arr1InnerParamNum = array of InnerParamNum;
      Arr2InnerParamNum = array of Arr1InnerParamNum;
      Arr3InnerParamNum = array of Arr2InnerParamNum;
      InnerParamVal = class;
      Arr1InnerParamVal = array of InnerParamVal;
      Arr2InnerParamVal = array of Arr1InnerParamVal;
      Arr3InnerParamVal = array of Arr2InnerParamVal;
      InnerPictureCallback = interface;
      Arr1InnerPictureCallback = array of InnerPictureCallback;
      Arr2InnerPictureCallback = array of Arr1InnerPictureCallback;
      Arr3InnerPictureCallback = array of Arr2InnerPictureCallback;
      InnerPreviewCallback = interface;
      Arr1InnerPreviewCallback = array of InnerPreviewCallback;
      Arr2InnerPreviewCallback = array of Arr1InnerPreviewCallback;
      Arr3InnerPreviewCallback = array of Arr2InnerPreviewCallback;
      InnerPropertyNum = class;
      Arr1InnerPropertyNum = array of InnerPropertyNum;
      Arr2InnerPropertyNum = array of Arr1InnerPropertyNum;
      Arr3InnerPropertyNum = array of Arr2InnerPropertyNum;
      InnerReaderInfo = class;
      Arr1InnerReaderInfo = array of InnerReaderInfo;
      Arr2InnerReaderInfo = array of Arr1InnerReaderInfo;
      Arr3InnerReaderInfo = array of Arr2InnerReaderInfo;
      InnerSize = class;
      Arr1InnerSize = array of InnerSize;
      Arr2InnerSize = array of Arr1InnerSize;
      Arr3InnerSize = array of Arr2InnerSize;
      InnerVideoCallback = interface;
      Arr1InnerVideoCallback = array of InnerVideoCallback;
      Arr2InnerVideoCallback = array of Arr1InnerVideoCallback;
      Arr3InnerVideoCallback = array of Arr2InnerVideoCallback;
      InnerParameters = class;
      Arr1InnerParameters = array of InnerParameters;
      Arr2InnerParameters = array of Arr1InnerParameters;
      Arr3InnerParameters = array of Arr2InnerParameters;
      InnerAutoFocusCallback = interface external 'com.motorolasolutions.adc.decoder' name 'AutoFocusCallback' 
        procedure onAutoFocus(para1: jboolean; para2: CMADBarCodeReader); overload;
      end;

      InnerDecodeCallback = interface external 'com.motorolasolutions.adc.decoder' name 'DecodeCallback' 
        procedure onDecodeComplete(para1: jint; para2: jint; para3: Arr1jbyte; para4: CMADBarCodeReader); overload;
        procedure onDecodeComplete(para1: jint; para2: jint; var para3: array of jbyte; para4: CMADBarCodeReader); overload;
        procedure onEvent(para1: jint; para2: jint; para3: Arr1jbyte; para4: CMADBarCodeReader); overload;
        procedure onEvent(para1: jint; para2: jint; var para3: array of jbyte; para4: CMADBarCodeReader); overload;
      end;

      InnerErrorCallback = interface external 'com.motorolasolutions.adc.decoder' name 'ErrorCallback' 
        procedure onError(para1: jint; para2: CMADBarCodeReader); overload;
      end;

      InnerOnZoomChangeListener = interface external 'com.motorolasolutions.adc.decoder' name 'OnZoomChangeListener' 
        procedure onZoomChange(para1: jint; para2: jboolean; para3: CMADBarCodeReader); overload;
      end;

      InnerParamNum = class external 'com.motorolasolutions.adc.decoder' name 'ParamNum' (JLObject)
      public
        const
          CODE39 = 0;
          UPCA = 1;
          UPCE = 2;
          EAN13 = 3;
          EAN8 = 4;
          D25 = 5;
          I25 = 6;
          CODABAR = 7;
          CODE128 = 8;
          CODE93 = 9;
          CODE11 = 10;
          MSI = 11;
          UPCE1 = 12;
          TRIOPTIC = 13;
          EAN128 = 14;
          PDF = 15;
          SUPPS = 16;
          C39_FULL_ASCII = 17;
          C39_LEN1 = 18;
          C39_LEN2 = 19;
          D25_LEN1 = 20;
          D25_LEN2 = 21;
          I25_LEN1 = 22;
          I25_LEN2 = 23;
          CBR_LEN1 = 24;
          CBR_LEN2 = 25;
          C93_LEN1 = 26;
          C93_LEN2 = 27;
          C11_LEN1 = 28;
          C11_LEN2 = 29;
          MSI_LEN1 = 30;
          MSI_LEN2 = 31;
          UPCA_PREAM = 34;
          UPCE_PREAM = 35;
          UPCE1_PREAM = 36;
          UPCE_TO_A = 37;
          UPCE1_TO_A = 38;
          EAN8_TO_13 = 39;
          UPCA_CHK = 40;
          UPCE_CHK = 41;
          UPCE1_CHK = 42;
          XMIT_C39_CHK = 43;
          XMIT_I25_CHK = 44;
          XMIT_CODE_ID = 45;
          XMIT_MSI_CHK = 46;
          XMIT_C11_CHK = 47;
          C39_CHK_EN = 48;
          I25_CHK_TYPE = 49;
          MSI_CHK_1_2 = 50;
          MSI_CHK_SCHEME = 51;
          C11_CHK_TYPE = 52;
          CLSI = 54;
          NOTIS = 55;
          UPC_SEC_LEV = 77;
          LIN_SEC_LEV = 78;
          SUPP_REDUN = 80;
          I25_TO_EAN13 = 82;
          BOOKLAND = 83;
          ISBT_128 = 84;
          COUPON = 85;
          CODE32 = 86;
          POST_US1 = 89;
          POST_US2 = 90;
          POST_UK = 91;
          SIGNATURE = 93;
          XMIT_NO_READ = 94;
          POST_US_PARITY = 95;
          POST_UK_PARITY = 96;
          EMUL_EAN128 = 123;
          LASER_ON_PRIM = 136;
          LASER_OFF_PRIM = 137;
          PRIM_TRIG_MODE = 138;
          C128_LEN1 = 209;
          C128_LEN2 = 210;
          ISBT_MAX_TRY = 223;
          UPDF = 227;
          C32_PREFIX = 231;
          POSTAL_JAP = 290;
          POSTAL_AUS = 291;
          DATAMATRIX = 292;
          QRCODE = 293;
          MAXICODE = 294;
          IMG_ILLUM = 298;
          IMG_AIM_SNAPSHOT = 300;
          IMG_CROP = 301;
          IMG_SUBSAMPLE = 302;
          IMG_BPP = 303;
          IMG_FILE_FORMAT = 304;
          IMG_JPEG_QUAL = 305;
          IMG_AIM_MODE = 306;
          IMG_SIG_FMT = 313;
          IMG_SIG_BPP = 314;
          IMG_CROP_TOP = 315;
          IMG_CROP_LEFT = 316;
          IMG_CROP_BOT = 317;
          IMG_CROP_RIGHT = 318;
          IMG_SNAPTIMEOUT = 323;
          IMG_VIDEOVF = 324;
          POSTAL_DUTCH = 326;
          RSS_14 = 338;
          RSS_LIM = 339;
          RSS_EXP = 340;
          CCC_ENABLE = 341;
          CCAB_ENABLE = 342;
          UPC_COMPOSITE = 344;
          IMG_IMAGE_ILLUM = 361;
          SIGCAP_WIDTH = 366;
          SIGCAP_HEIGHT = 367;
          TCIF = 371;
          MARGIN_RATIO = 381;
          DEMOTE_RSS = 397;
          PICKLIST_MODE = 402;
          C25 = 408;
          IMAGE_SIG_JPEG_QUALITY = 421;
          EMUL_UCCEAN128 = 427;
          MIRROR_IMAGE = 537;
          IMG_ENHANCEMENT = 564;
          UQR_EN = 573;
          AZTEC = 574;
          BOOKLAND_FORMAT = 576;
          ISBT_CONCAT_MODE = 577;
          CHECK_ISBT_TABLE = 578;
          SUPP_USER_1 = 579;
          SUPP_USER_2 = 580;
          K35 = 581;
          ONE_D_INVERSE = 586;
          QR_INVERSE = 587;
          DATAMATRIX_INVERSE = 588;
          AZTEC_INVERSE = 589;
          AIMMODEHANDSFREE = 590;
          POST_US3 = 592;
          POST_US4 = 611;
          ISSN_EAN_EN = 617;
          MATRIX_25_EN = 618;
          MATRIX_25_LEN1 = 619;
          MATRIX_25_LEN2 = 620;
          MATRIX_25_REDUN = 621;
          MATRIX_25_CHK_EN = 622;
          MATRIX_25_XMIT_CHK = 623;
          AIMID_SUPP_FORMAT = 672;
          POST_AUS_FMT = 718;
          DATABAR_LIM_SEC_LEV = 728;
          COUPON_REPORT = 730;
          IMG_MOTIONILLUM = 762;
      public
        constructor create(); overload;
      end;

      InnerParamVal = class external 'com.motorolasolutions.adc.decoder' name 'ParamVal' (JLObject)
      public
        const
          SUPP_NONE = 0;
          SUPP_ONLY = 1;
          SUPP_AUTOD = 2;
          SUPP_SMART = 3;
          SUPP_378_379 = 4;
          SUPP_978_979 = 5;
          SUPP_414_419_434_439 = 6;
          SUPP_977 = 7;
          SUPP_491 = 8;
          SUPP_PROG_1 = 9;
          SUPP_PROG_1_AND_2 = 10;
          SUPP_SMART_PLUS_1 = 11;
          SUPP_SMART_PLUS_1_2 = 12;
          LEVEL = 0;
          HANDSFREE = 7;
          AUTO_AIM = 9;
          IMG_BPP_1 = 0;
          IMG_BPP_4 = 1;
          IMG_BPP_8 = 2;
          IMG_FORMAT_JPEG = 1;
          IMG_FORMAT_BMP = 3;
          IMG_FORMAT_TIFF = 4;
          IMG_SUBSAMPLE_FACTOR_1 = 0;
          IMG_SUBSAMPLE_FACTOR_2 = 1;
          IMG_SUBSAMPLE_FACTOR_3 = 2;
          IMG_SUBSAMPLE_FACTOR_4 = 3;
          AIM_OFF = 0;
          AIM_ON = 1;
          AIM_ON_ALWAYS = 2;
          UPC_NEVER = 0;
          UPC_ALWAYS = 1;
          UPC_AUTOD = 2;
          PICKLIST_NEVER = 0;
          PICKLIST_OUT_OF_SCANSTAND = 1;
          PICKLIST_ALWAYS = 1;
          MIRROR_NEVER = 0;
          MIRROR_ALWAYS = 1;
          MIRROR_AUTO = 2;
          IMG_ENHANCE_OFF = 0;
          IMG_ENHANCE_LOW = 1;
          IMG_ENHANCE_MED = 2;
          IMG_ENHANCE_HIGH = 3;
          IMG_ENHANCE_CUSTOM = 4;
          ISBT_CONCAT_NONE = 0;
          ISBT_CONCAT_ONLY = 1;
          ISBT_CONCAT_AUTOD = 2;
          REGULAR_ONLY = 0;
          INVERSE_ONLY = 1;
          INVERSE_AUTOD = 2;
          PDF_SECURITY_STRICT = 0;
          PDF_CWLEN_ZERO_OK = 1;
      public
        constructor create(); overload;
      end;

      InnerPictureCallback = interface external 'com.motorolasolutions.adc.decoder' name 'PictureCallback' 
        procedure onPictureTaken(para1: jint; para2: jint; para3: jint; para4: Arr1jbyte; para5: CMADBarCodeReader); overload;
        procedure onPictureTaken(para1: jint; para2: jint; para3: jint; var para4: array of jbyte; para5: CMADBarCodeReader); overload;
      end;

      InnerPreviewCallback = interface external 'com.motorolasolutions.adc.decoder' name 'PreviewCallback' 
        procedure onPreviewFrame(para1: Arr1jbyte; para2: CMADBarCodeReader); overload;
        procedure onPreviewFrame(var para1: array of jbyte; para2: CMADBarCodeReader); overload;
      end;

      InnerPropertyNum = class external 'com.motorolasolutions.adc.decoder' name 'PropertyNum' (JLObject)
      public
        const
          MODEL_NUMBER = 1;
          SERIAL_NUM = 2;
          MAX_FRAME_BUFFER_SIZE = 3;
          HORIZONTAL_RES = 4;
          VERTICAL_RES = 5;
          IMGKIT_VER = 6;
          ENGINE_VER = 7;
      public
        constructor create(); overload;
      end;

      InnerReaderInfo = class external 'com.motorolasolutions.adc.decoder' name 'ReaderInfo' (JLObject)
      public
        const
          BCRDR_FACING_BACK = 0;
          BCRDR_FACING_FRONT = 1;
      public
        var
          ffacing: jint; external name 'facing';
          forientation: jint; external name 'orientation';
      public
        constructor create(); overload;
      end;

      InnerSize = class external 'com.motorolasolutions.adc.decoder' name 'Size' 
      end;

      InnerVideoCallback = interface external 'com.motorolasolutions.adc.decoder' name 'VideoCallback' 
        procedure onVideoFrame(para1: jint; para2: jint; para3: jint; para4: Arr1jbyte; para5: CMADBarCodeReader); overload;
        procedure onVideoFrame(para1: jint; para2: jint; para3: jint; var para4: array of jbyte; para5: CMADBarCodeReader); overload;
      end;

      InnerParameters = class external 'com.motorolasolutions.adc.decoder' name 'Parameters' 
      end;

  public
    const
      BCR_SUCCESS = 0;
      BCR_ERROR = -1;
      DECODE_STATUS_TIMEOUT = 0;
      DECODE_STATUS_CANCELED = -1;
      DECODE_STATUS_ERROR = -2;
      BCRDR_EVENT_SCAN_MODE_CHANGED = 5;
      BCRDR_EVENT_MOTION_DETECTED = 6;
      BCRDR_EVENT_SCANNER_RESET = 7;
      BCRDR_ERROR_UNKNOWN = 1;
      BCRDR_ERROR_SERVER_DIED = 100;
  public
    class function getNumberOfReaders(): jint; static; overload;
    class procedure getReaderInfo(para1: jint; para2: CMADBarCodeReader.InnerReaderInfo); static; overload;
    procedure lock(); overload; virtual; final;
    procedure unlock(); overload; virtual; final;
    procedure reconnect(); overload; virtual; final;  // throws java.io.IOException
    function getNumProperty(para1: jint): jint; overload; virtual; final;
    function getStrProperty(para1: jint): JLString; overload; virtual; final;
    function getNumParameter(para1: jint): jint; overload; virtual; final;
    function getStrParameter(para1: jint): JLString; overload; virtual; final;
    function setParameter(para1: jint; para2: jint): jint; overload; virtual; final;
    function setParameter(para1: jint; para2: JLString): jint; overload; virtual; final;
    procedure setDefaultParameters(); overload; virtual; final;
    procedure addCallbackBuffer(para1: Arr1jbyte); overload; virtual; final;
    procedure addCallbackBuffer(var para1: array of jbyte); overload; virtual; final;
    procedure startVideoCapture(para1: CMADBarCodeReader.InnerVideoCallback); overload; virtual; final;
    procedure startViewFinder(); overload; virtual; final;
    procedure startPreview(); overload; virtual; final;
    procedure stopPreview(); overload; virtual; final;
    procedure startDecode(); overload; virtual; final;
    function startHandsFreeDecode(para1: jint): jint; overload; virtual; final;
    procedure stopDecode(); overload; virtual; final;
    function previewEnabled(): jboolean; overload; virtual; final;
    procedure startSmoothZoom(para1: jint); overload; virtual; final;
    procedure stopSmoothZoom(); overload; virtual; final;
    procedure setDisplayOrientation(para1: jint); overload; virtual; final;
    class function open(para1: jint): CMADBarCodeReader; static; overload;
    class function open(): CMADBarCodeReader; static; overload;
  strict protected
    procedure finalize(); overload; virtual;
  public
    procedure release(); overload; virtual; final;
    procedure setPreviewDisplay(para1: AVSurfaceHolder); overload; virtual; final;  // throws java.io.IOException
    procedure autoFocus(para1: CMADBarCodeReader.InnerAutoFocusCallback); overload; virtual; final;
    procedure cancelAutoFocus(); overload; virtual; final;
    procedure setAutoFocusDelay(para1: jint; para2: jint); overload; virtual; final;
    procedure setDecodeCallback(para1: CMADBarCodeReader.InnerDecodeCallback); overload; virtual; final;
    procedure takePicture(para1: CMADBarCodeReader.InnerPictureCallback); overload; virtual; final;
    procedure setOneShotPreviewCallback(para1: CMADBarCodeReader.InnerPreviewCallback); overload; virtual; final;
    procedure setPreviewCallbackWithBuffer(para1: CMADBarCodeReader.InnerPreviewCallback); overload; virtual; final;
    function getDecodeFinished(): jboolean; overload; virtual;
    procedure setDecodeFinished(para1: jboolean); overload; virtual;
    procedure setZoomChangeListener(para1: CMADBarCodeReader.InnerOnZoomChangeListener); overload; virtual; final;
    procedure setErrorCallback(para1: CMADBarCodeReader.InnerErrorCallback); overload; virtual; final;
    procedure setParameters(para1: CMADBarCodeReader.InnerParameters); overload; virtual;
    function getParameters(): CMADBarCodeReader.InnerParameters; overload; virtual;
  end;

  CSSDecodeCallback = interface external 'com.seuic.scanner' name 'DecodeCallback' 
    procedure onDecodeComplete(para1: JLString); overload;
  end;

  CSSFileUtil = class external 'com.seuic.scanner' name 'FileUtil' (JLObject)
  public
    final class var
      fAPP_TEMP_DIRECTORY: JLString; external name 'APP_TEMP_DIRECTORY';
  public
    constructor create(); overload;
    class function fileExists(para1: JLString): jboolean; static; overload;
    class function createDirectory(para1: JLString): jboolean; static; overload;
    class procedure createFile(para1: JLString); static; overload;
    class procedure clear(para1: JLString); static; overload;
  end;

  CSSIScanner = interface external 'com.seuic.scanner' name 'IScanner' 
    type
      InnerParamCode = class;
      Arr1InnerParamCode = array of InnerParamCode;
      Arr2InnerParamCode = array of Arr1InnerParamCode;
      Arr3InnerParamCode = array of Arr2InnerParamCode;
      InnerParamCode = class external 'com.seuic.scanner' name 'ParamCode' (JLObject)
      public
        const
          LASTER_ONTIME = 1;
          SCAN_ANGLE = 2;
          TIMEOUT_SAMESYMBOL = 3;
          SECURITY_LEVEL = 4;
          FACTORY_DEFAULT = 5;
          BIdIRECTIONAL = 6;
          UPCA = 257;
          UPCE = 258;
          UPCE1 = 259;
          EAN8 = 260;
          EAN13 = 261;
          BOOKLAND = 262;
          UPCACHK = 263;
          UPCECHK = 264;
          UPCE1CHK = 265;
          UPCAPREM = 266;
          UPCEPREM = 267;
          UPCE1PREM = 268;
          UPCETOA = 269;
          UPCE1TOA = 270;
          EAN8TO13 = 271;
          COUPON = 272;
          CODE128 = 289;
          ISBT128 = 290;
          GS1128 = 291;
          CODE39 = 305;
          TRIC39 = 306;
          C39FULL = 307;
          C39TOC32 = 308;
          C32PRE = 309;
          LENC391 = 310;
          LENC392 = 311;
          C39CHK = 312;
          C39VER = 313;
          CODE93 = 321;
          LENC931 = 322;
          LENC932 = 323;
          CODE11 = 337;
          LENC111 = 338;
          LENC112 = 339;
          C11CHKVER = 340;
          C11CHK = 341;
          I25 = 353;
          LENI251 = 354;
          LENI252 = 355;
          D25 = 369;
          LEND251 = 370;
          LEND252 = 371;
          C25 = 385;
          LENC251 = 386;
          LENC252 = 387;
          CODABAR = 401;
          LENCODABAR1 = 402;
          LENCODABAR2 = 403;
          MSI = 513;
          LENMSI1 = 514;
          LENMSI2 = 515;
          RSS14 = 529;
          QRCODE = 545;
          QRINVERSE = 546;
          MICROQR = 547;
          DM = 561;
          DMINVERSE = 562;
          DMI = 563;
      public
        constructor create(); overload;
      end;

    const
      SCAN_KEYCODE = 142;
    function open(): jboolean; overload;
    procedure close(); overload;
    procedure startScan(); overload;
    procedure stopScan(); overload;
    procedure enable(); overload;
    procedure disable(); overload;
    function setParams(para1: jint; para2: jint): jboolean; overload;
    function getParams(para1: jint): jint; overload;
    procedure setDecodeCallBack(para1: CSSDecodeCallback); overload;
  end;

  CSSParam1D = class external 'com.seuic.scanner' name 'Param1D' (JLObject)
  public
    var
      fid: jint; external name 'id';
      fvalue: jint; external name 'value';
  public
    constructor create(); overload;
  end;

  CSSScannerFactory = class external 'com.seuic.scanner' name 'ScannerFactory' (JLObject)
  public
    constructor create(); overload;
    class function getScanner(para1: ACContext): CSSScanner; static; overload;
  end;

  CSSXmlPullParserUtil = class external 'com.seuic.scanner' name 'XmlPullParserUtil' (JLObject)
  public
    constructor create(); overload;
    procedure PullParserFromFile(para1: JLString); overload; virtual;
  end;

  CSSscankey = class external 'com.seuic.scanner' name 'scankey' (JLObject)
  public
    constructor create(); overload;
    function openScanKey(): jint; overload; virtual;
    function WaitScanKey(): jint; overload; virtual;
    procedure CloseScanKey(); overload; virtual;
  end;

  CSSScanner = class abstract external 'com.seuic.scanner' name 'Scanner' (JLObject, CSSIScanner)
  public
    var
      fplaySound: jboolean; external name 'playSound';
      fviberate: jboolean; external name 'viberate';
  public
    function getAllParams(): JUMap; overload; virtual; abstract;
    procedure setPlaySound(para1: jboolean); overload; virtual;
    procedure setViberate(para1: jboolean); overload; virtual;
    function getPlaySound(): jboolean; overload; virtual;
    function getViberate(): jboolean; overload; virtual;
  end;

  CSSScanner1D = class external 'com.seuic.scanner' name 'Scanner1D' (CSSScanner)
  public
    type
      InnerBarcodeInfo = class;
      Arr1InnerBarcodeInfo = array of InnerBarcodeInfo;
      Arr2InnerBarcodeInfo = array of Arr1InnerBarcodeInfo;
      Arr3InnerBarcodeInfo = array of Arr2InnerBarcodeInfo;
      InnerScanner1DParams = class;
      Arr1InnerScanner1DParams = array of InnerScanner1DParams;
      Arr2InnerScanner1DParams = array of Arr1InnerScanner1DParams;
      Arr3InnerScanner1DParams = array of Arr2InnerScanner1DParams;
      InnerDecodeHandler = class;
      Arr1InnerDecodeHandler = array of InnerDecodeHandler;
      Arr2InnerDecodeHandler = array of Arr1InnerDecodeHandler;
      Arr3InnerDecodeHandler = array of Arr2InnerDecodeHandler;
      InnerBarcodeInfo = class external 'com.seuic.scanner' name 'BarcodeInfo' 
      end;

      InnerScanner1DParams = class external 'com.seuic.scanner' name 'Scanner1DParams' 
      end;

      InnerDecodeHandler = class external 'com.seuic.scanner' name 'DecodeHandler' 
      end;

  public
    function JNIOpen(): jboolean; overload; virtual; final;
    procedure JNIClose(); overload; virtual; final;
    function JNIGetBarcode(para1: CSSScanner1D.InnerBarcodeInfo; para2: jint): jboolean; overload; virtual; final;
    function JNIStopScan(): jboolean; overload; virtual; final;
    function JNISetParam(para1: CSSParam1D): jboolean; overload; virtual; final;
    function JNIGetParam(para1: CSSParam1D): jboolean; overload; virtual; final;
    procedure JNIEnableScan(); overload; virtual; final;
    procedure JNIDisableScan(); overload; virtual; final;
    function getAllParams(): JUMap; overload; virtual;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    procedure startScan(); overload; virtual;
    procedure stopScan(); overload; virtual;
    procedure enable(); overload; virtual;
    procedure disable(); overload; virtual;
    function setParams(para1: jint; para2: jint): jboolean; overload; virtual;
    function getParams(para1: jint): jint; overload; virtual;
    procedure setDecodeCallBack(para1: CSSDecodeCallback); overload; virtual;
  end;

  CSSScanner2D = class external 'com.seuic.scanner' name 'Scanner2D' (CSSScanner, CMADBarCodeReader.InnerDecodeCallback)
  public
    constructor create(para1: ACContext); overload;
    function getAllParams(): JUMap; overload; virtual;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    procedure stopScan(); overload; virtual;
    procedure enable(); overload; virtual;
    procedure disable(); overload; virtual;
    function setParams(para1: jint; para2: jint): jboolean; overload; virtual;
    function getParams(para1: jint): jint; overload; virtual;
    procedure onDecodeComplete(para1: jint; para2: jint; para3: Arr1jbyte; para4: CMADBarCodeReader); overload; virtual;
    procedure onDecodeComplete(para1: jint; para2: jint; var para3: array of jbyte; para4: CMADBarCodeReader); overload; virtual;
    procedure onEvent(para1: jint; para2: jint; para3: Arr1jbyte; para4: CMADBarCodeReader); overload; virtual;
    procedure onEvent(para1: jint; para2: jint; var para3: array of jbyte; para4: CMADBarCodeReader); overload; virtual;
    procedure setDecodeCallBack(para1: CSSDecodeCallback); overload; virtual;
    procedure startScan(); overload; virtual;
  end;

