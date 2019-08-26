  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

  CFDCH34xIds = class external 'com.felhr.deviceids' name 'CH34xIds' (JLObject)
  public
    type
      InnerConcreteDevice = class;
      Arr1InnerConcreteDevice = array of InnerConcreteDevice;
      Arr2InnerConcreteDevice = array of Arr1InnerConcreteDevice;
      Arr3InnerConcreteDevice = array of Arr2InnerConcreteDevice;
      InnerConcreteDevice = class external 'com.felhr.deviceids' name 'ConcreteDevice' 
      end;

  public
    class function isDeviceSupported(para1: jint; para2: jint): jboolean; static; overload;
  end;

  CFDCP210xIds = class external 'com.felhr.deviceids' name 'CP210xIds' (JLObject)
  public
    type
      InnerConcreteDevice = class;
      Arr1InnerConcreteDevice = array of InnerConcreteDevice;
      Arr2InnerConcreteDevice = array of Arr1InnerConcreteDevice;
      Arr3InnerConcreteDevice = array of Arr2InnerConcreteDevice;
      InnerConcreteDevice = class external 'com.felhr.deviceids' name 'ConcreteDevice' 
      end;

  public
    constructor create(); overload;
    class function isDeviceSupported(para1: jint; para2: jint): jboolean; static; overload;
  end;

  CFDCP2130Ids = class external 'com.felhr.deviceids' name 'CP2130Ids' (JLObject)
  public
    type
      InnerConcreteDevice = class;
      Arr1InnerConcreteDevice = array of InnerConcreteDevice;
      Arr2InnerConcreteDevice = array of Arr1InnerConcreteDevice;
      Arr3InnerConcreteDevice = array of Arr2InnerConcreteDevice;
      InnerConcreteDevice = class external 'com.felhr.deviceids' name 'ConcreteDevice' 
      end;

  public
    constructor create(); overload;
    class function isDeviceSupported(para1: jint; para2: jint): jboolean; static; overload;
  end;

  CFDFTDISioIds = class external 'com.felhr.deviceids' name 'FTDISioIds' (JLObject)
  public
    type
      InnerConcreteDevice = class;
      Arr1InnerConcreteDevice = array of InnerConcreteDevice;
      Arr2InnerConcreteDevice = array of Arr1InnerConcreteDevice;
      Arr3InnerConcreteDevice = array of Arr2InnerConcreteDevice;
      InnerConcreteDevice = class external 'com.felhr.deviceids' name 'ConcreteDevice' 
      end;

  public
    class function isDeviceSupported(para1: jint; para2: jint): jboolean; static; overload;
  end;

  CFDPL2303Ids = class external 'com.felhr.deviceids' name 'PL2303Ids' (JLObject)
  public
    type
      InnerConcreteDevice = class;
      Arr1InnerConcreteDevice = array of InnerConcreteDevice;
      Arr2InnerConcreteDevice = array of Arr1InnerConcreteDevice;
      Arr3InnerConcreteDevice = array of Arr2InnerConcreteDevice;
      InnerConcreteDevice = class external 'com.felhr.deviceids' name 'ConcreteDevice' 
      end;

  public
    class function isDeviceSupported(para1: jint; para2: jint): jboolean; static; overload;
  end;

  CFDXdcVcpIds = class external 'com.felhr.deviceids' name 'XdcVcpIds' (JLObject)
  public
    type
      InnerConcreteDevice = class;
      Arr1InnerConcreteDevice = array of InnerConcreteDevice;
      Arr2InnerConcreteDevice = array of Arr1InnerConcreteDevice;
      Arr3InnerConcreteDevice = array of Arr2InnerConcreteDevice;
      InnerConcreteDevice = class external 'com.felhr.deviceids' name 'ConcreteDevice' 
      end;

  public
    constructor create(); overload;
    class function isDeviceSupported(para1: jint; para2: jint): jboolean; static; overload;
  end;

  CFUBuildConfig = class sealed external 'com.felhr.usbserial' name 'BuildConfig' (JLObject)
  public
    const
      DEBUG = 0;
      APPLICATION_ID = 'com.felhr.usbserial';
      BUILD_TYPE = 'release';
      FLAVOR = '';
      VERSION_CODE = -1;
      VERSION_NAME = '';
  public
    constructor create(); overload;
  end;

  CFUSerialBuffer = class external 'com.felhr.usbserial' name 'SerialBuffer' (JLObject)
  public
    type
      InnerSynchronizedBuffer = class;
      Arr1InnerSynchronizedBuffer = array of InnerSynchronizedBuffer;
      Arr2InnerSynchronizedBuffer = array of Arr1InnerSynchronizedBuffer;
      Arr3InnerSynchronizedBuffer = array of Arr2InnerSynchronizedBuffer;
      InnerSynchronizedBuffer = class external 'com.felhr.usbserial' name 'SynchronizedBuffer' 
      end;

  public
    const
      DEFAULT_READ_BUFFER_SIZE = 16384;
      DEFAULT_WRITE_BUFFER_SIZE = 16384;
  public
    constructor create(para1: jboolean); overload;
    procedure debug(para1: jboolean); overload; virtual;
    procedure putReadBuffer(para1: JNByteBuffer); overload; virtual;
    function getReadBuffer(): JNByteBuffer; overload; virtual;
    function getDataReceived(): Arr1jbyte; overload; virtual;
    procedure clearReadBuffer(); overload; virtual;
    function getWriteBuffer(): Arr1jbyte; overload; virtual;
    procedure putWriteBuffer(para1: Arr1jbyte); overload; virtual;
    procedure putWriteBuffer(var para1: array of jbyte); overload; virtual;
    procedure resetWriteBuffer(); overload; virtual;
    function getBufferCompatible(): Arr1jbyte; overload; virtual;
    function getDataReceivedCompatible(para1: jint): Arr1jbyte; overload; virtual;
  end;

  CFUUsbSerialDebugger = class external 'com.felhr.usbserial' name 'UsbSerialDebugger' (JLObject)
  public
    const
      ENCODING = 'UTF-8';
  public
    class procedure printLogGet(para1: Arr1jbyte; para2: jboolean); static; overload;
    class procedure printLogGet(var para1: array of jbyte; para2: jboolean); static; overload;
    class procedure printLogPut(para1: Arr1jbyte; para2: jboolean); static; overload;
    class procedure printLogPut(var para1: array of jbyte; para2: jboolean); static; overload;
    class procedure printReadLogGet(para1: Arr1jbyte; para2: jboolean); static; overload;
    class procedure printReadLogGet(var para1: array of jbyte; para2: jboolean); static; overload;
    class procedure printReadLogPut(para1: Arr1jbyte; para2: jboolean); static; overload;
    class procedure printReadLogPut(var para1: array of jbyte; para2: jboolean); static; overload;
  end;

  CFUUsbSerialInterface = interface external 'com.felhr.usbserial' name 'UsbSerialInterface' 
    type
      InnerUsbReadCallback = interface;
      Arr1InnerUsbReadCallback = array of InnerUsbReadCallback;
      Arr2InnerUsbReadCallback = array of Arr1InnerUsbReadCallback;
      Arr3InnerUsbReadCallback = array of Arr2InnerUsbReadCallback;
      InnerUsbParityCallback = interface;
      Arr1InnerUsbParityCallback = array of InnerUsbParityCallback;
      Arr2InnerUsbParityCallback = array of Arr1InnerUsbParityCallback;
      Arr3InnerUsbParityCallback = array of Arr2InnerUsbParityCallback;
      InnerUsbOverrunCallback = interface;
      Arr1InnerUsbOverrunCallback = array of InnerUsbOverrunCallback;
      Arr2InnerUsbOverrunCallback = array of Arr1InnerUsbOverrunCallback;
      Arr3InnerUsbOverrunCallback = array of Arr2InnerUsbOverrunCallback;
      InnerUsbFrameCallback = interface;
      Arr1InnerUsbFrameCallback = array of InnerUsbFrameCallback;
      Arr2InnerUsbFrameCallback = array of Arr1InnerUsbFrameCallback;
      Arr3InnerUsbFrameCallback = array of Arr2InnerUsbFrameCallback;
      InnerUsbBreakCallback = interface;
      Arr1InnerUsbBreakCallback = array of InnerUsbBreakCallback;
      Arr2InnerUsbBreakCallback = array of Arr1InnerUsbBreakCallback;
      Arr3InnerUsbBreakCallback = array of Arr2InnerUsbBreakCallback;
      InnerUsbDSRCallback = interface;
      Arr1InnerUsbDSRCallback = array of InnerUsbDSRCallback;
      Arr2InnerUsbDSRCallback = array of Arr1InnerUsbDSRCallback;
      Arr3InnerUsbDSRCallback = array of Arr2InnerUsbDSRCallback;
      InnerUsbCTSCallback = interface;
      Arr1InnerUsbCTSCallback = array of InnerUsbCTSCallback;
      Arr2InnerUsbCTSCallback = array of Arr1InnerUsbCTSCallback;
      Arr3InnerUsbCTSCallback = array of Arr2InnerUsbCTSCallback;
      InnerUsbReadCallback = interface external 'com.felhr.usbserial' name 'UsbReadCallback' 
       //zc procedure onReceivedData(para1: Arr1jbyte); overload;
        procedure onReceivedData(var para1: array of jbyte); overload;
      end;

      InnerUsbParityCallback = interface external 'com.felhr.usbserial' name 'UsbParityCallback' 
        procedure onParityError(); overload;
      end;

      InnerUsbOverrunCallback = interface external 'com.felhr.usbserial' name 'UsbOverrunCallback' 
        procedure onOverrunError(); overload;
      end;

      InnerUsbFrameCallback = interface external 'com.felhr.usbserial' name 'UsbFrameCallback' 
        procedure onFramingError(); overload;
      end;

      InnerUsbBreakCallback = interface external 'com.felhr.usbserial' name 'UsbBreakCallback' 
        procedure onBreakInterrupt(); overload;
      end;

      InnerUsbDSRCallback = interface external 'com.felhr.usbserial' name 'UsbDSRCallback' 
        procedure onDSRChanged(para1: jboolean); overload;
      end;

      InnerUsbCTSCallback = interface external 'com.felhr.usbserial' name 'UsbCTSCallback' 
        procedure onCTSChanged(para1: jboolean); overload;
      end;

    const
      DATA_BITS_5 = 5;
      DATA_BITS_6 = 6;
      DATA_BITS_7 = 7;
      DATA_BITS_8 = 8;
      STOP_BITS_1 = 1;
      STOP_BITS_15 = 3;
      STOP_BITS_2 = 2;
      PARITY_NONE = 0;
      PARITY_ODD = 1;
      PARITY_EVEN = 2;
      PARITY_MARK = 3;
      PARITY_SPACE = 4;
      FLOW_CONTROL_OFF = 0;
      FLOW_CONTROL_RTS_CTS = 1;
      FLOW_CONTROL_DSR_DTR = 2;
      FLOW_CONTROL_XON_XOFF = 3;
    function open(): jboolean; overload;
    procedure write(para1: Arr1jbyte); overload;
    procedure write(var para1: array of jbyte); overload;
    function read(para1: CFUUsbSerialInterface.InnerUsbReadCallback): jint; overload;
    procedure close(); overload;
    function syncOpen(): jboolean; overload;
    function syncWrite(para1: Arr1jbyte; para2: jint): jint; overload;
    function syncWrite(var para1: array of jbyte; para2: jint): jint; overload;
    function syncRead(para1: Arr1jbyte; para2: jint): jint; overload;
    function syncRead(var para1: array of jbyte; para2: jint): jint; overload;
    procedure syncClose(); overload;
    procedure setBaudRate(para1: jint); overload;
    procedure setDataBits(para1: jint); overload;
    procedure setStopBits(para1: jint); overload;
    procedure setParity(para1: jint); overload;
    procedure setFlowControl(para1: jint); overload;
    procedure setRTS(para1: jboolean); overload;
    procedure setDTR(para1: jboolean); overload;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload;
  end;

  CFUUsbSpiInterface = interface external 'com.felhr.usbserial' name 'UsbSpiInterface' 
    type
      InnerUsbMISOCallback = interface;
      Arr1InnerUsbMISOCallback = array of InnerUsbMISOCallback;
      Arr2InnerUsbMISOCallback = array of Arr1InnerUsbMISOCallback;
      Arr3InnerUsbMISOCallback = array of Arr2InnerUsbMISOCallback;
      InnerUsbMISOCallback = interface external 'com.felhr.usbserial' name 'UsbMISOCallback' 
        function onReceivedData(para1: Arr1jbyte): jint; overload;
        function onReceivedData(var para1: array of jbyte): jint; overload;
      end;

    const
      DIVIDER_2 = 2;
      DIVIDER_4 = 4;
      DIVIDER_8 = 8;
      DIVIDER_16 = 16;
      DIVIDER_32 = 32;
      DIVIDER_64 = 64;
      DIVIDER_128 = 128;
    function connectSPI(): jboolean; overload;
    procedure writeMOSI(para1: Arr1jbyte); overload;
    procedure writeMOSI(var para1: array of jbyte); overload;
    procedure readMISO(para1: jint); overload;
    procedure writeRead(para1: Arr1jbyte; para2: jint); overload;
    procedure writeRead(var para1: array of jbyte; para2: jint); overload;
    procedure setClock(para1: jint); overload;
    procedure selectSlave(para1: jint); overload;
    procedure setMISOCallback(para1: CFUUsbSpiInterface.InnerUsbMISOCallback); overload;
    procedure closeSPI(); overload;
    function getClockDivider(): jint; overload;
    function getSelectedSlave(): jint; overload;
  end;

  CFUHexData = class external 'com.felhr.utils' name 'HexData' (JLObject)
  public
    class function hexToString(para1: Arr1jbyte): JLString; static; overload;
    class function hexToString(var para1: array of jbyte): JLString; static; overload;
    class function stringTobytes(para1: JLString): Arr1jbyte; static; overload;
    class function hex4digits(para1: JLString): JLString; static; overload;
  end;

  CFUSerialOutputStream = class external 'com.felhr.usbserial' name 'SerialOutputStream' (JIOutputStream)
  strict protected
    final var
      fdevice: CFUUsbSerialInterface; external name 'device';
  public
    constructor create(para1: CFUUsbSerialInterface); overload;
    procedure write(para1: jint); overload; virtual;
    procedure write(para1: Arr1jbyte); overload; virtual;
    procedure write(var para1: array of jbyte); overload; virtual;
  end;

  CFUSerialInputStream = class external 'com.felhr.usbserial' name 'SerialInputStream' (JIInputStream, CFUUsbSerialInterface.InnerUsbReadCallback)
  strict protected
    final var
      fdevice: CFUUsbSerialInterface; external name 'device';
  strict protected
    var
      fdata: JUCArrayBlockingQueue; external name 'data';
      fis_open: jboolean; external name 'is_open';
  public
    constructor create(para1: CFUUsbSerialInterface); overload;
    function read(): jint; overload; virtual;
    procedure close(); overload; virtual;
    procedure onReceivedData(para1: Arr1jbyte); overload; virtual;
    procedure onReceivedData(var para1: array of jbyte); overload; virtual;
  end;

  CFUUsbSerialDevice = class abstract external 'com.felhr.usbserial' name 'UsbSerialDevice' (JLObject, CFUUsbSerialInterface)
  public
    type
      InnerReadThread = class;
      Arr1InnerReadThread = array of InnerReadThread;
      Arr2InnerReadThread = array of Arr1InnerReadThread;
      Arr3InnerReadThread = array of Arr2InnerReadThread;
      InnerWriteThread = class;
      Arr1InnerWriteThread = array of InnerWriteThread;
      Arr2InnerWriteThread = array of Arr1InnerWriteThread;
      Arr3InnerWriteThread = array of Arr2InnerWriteThread;
      InnerWorkerThread = class;
      Arr1InnerWorkerThread = array of InnerWorkerThread;
      Arr2InnerWorkerThread = array of Arr1InnerWorkerThread;
      Arr3InnerWorkerThread = array of Arr2InnerWorkerThread;
      InnerReadThread = class external 'com.felhr.usbserial' name 'ReadThread' 
      end;

      InnerWriteThread = class external 'com.felhr.usbserial' name 'WriteThread' 
      end;

      InnerWorkerThread = class external 'com.felhr.usbserial' name 'WorkerThread' 
      end;

  strict protected
    final var
      fdevice: AHUUsbDevice; external name 'device';
      fconnection: AHUUsbDeviceConnection; external name 'connection';
  strict protected
    const
      USB_TIMEOUT = 5000;
  strict protected
    var
      fserialBuffer: CFUSerialBuffer; external name 'serialBuffer';
      fworkerThread: CFUUsbSerialDevice.InnerWorkerThread; external name 'workerThread';
      fwriteThread: CFUUsbSerialDevice.InnerWriteThread; external name 'writeThread';
      freadThread: CFUUsbSerialDevice.InnerReadThread; external name 'readThread';
      fasyncMode: jboolean; external name 'asyncMode';
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    class function createUsbSerialDevice(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection): CFUUsbSerialDevice; static; overload;
    class function createUsbSerialDevice(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint): CFUUsbSerialDevice; static; overload;
    class function isSupported(para1: AHUUsbDevice): jboolean; static; overload;
    function open(): jboolean; overload; virtual; abstract;
    procedure write(para1: Arr1jbyte); overload; virtual;
    procedure write(var para1: array of jbyte); overload; virtual;
    function read(para1: CFUUsbSerialInterface.InnerUsbReadCallback): jint; overload; virtual;
    procedure close(); overload; virtual; abstract;
    function syncOpen(): jboolean; overload; virtual; abstract;
    procedure syncClose(); overload; virtual; abstract;
    function syncWrite(para1: Arr1jbyte; para2: jint): jint; overload; virtual;
    function syncWrite(var para1: array of jbyte; para2: jint): jint; overload; virtual;
    function syncRead(para1: Arr1jbyte; para2: jint): jint; overload; virtual;
    function syncRead(var para1: array of jbyte; para2: jint): jint; overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual; abstract;
    procedure setDataBits(para1: jint); overload; virtual; abstract;
    procedure setStopBits(para1: jint); overload; virtual; abstract;
    procedure setParity(para1: jint); overload; virtual; abstract;
    procedure setFlowControl(para1: jint); overload; virtual; abstract;
    procedure debug(para1: jboolean); overload; virtual;
    class function isCdcDevice(para1: AHUUsbDevice): jboolean; static; overload;
  strict protected
    procedure setSyncParams(para1: AHUUsbEndpoint; para2: AHUUsbEndpoint); overload; virtual;
    procedure setThreadsParams(para1: AHUUsbRequest; para2: AHUUsbEndpoint); overload; virtual;
    procedure killWorkingThread(); overload; virtual;
    procedure restartWorkingThread(); overload; virtual;
    procedure killWriteThread(); overload; virtual;
    procedure restartWriteThread(); overload; virtual;
  end;

  CFUUsbSpiDevice = class abstract external 'com.felhr.usbserial' name 'UsbSpiDevice' (JLObject, CFUUsbSpiInterface)
  public
    type
      InnerWriteThread = class;
      Arr1InnerWriteThread = array of InnerWriteThread;
      Arr2InnerWriteThread = array of Arr1InnerWriteThread;
      Arr3InnerWriteThread = array of Arr2InnerWriteThread;
      InnerReadThread = class;
      Arr1InnerReadThread = array of InnerReadThread;
      Arr2InnerReadThread = array of Arr1InnerReadThread;
      Arr3InnerReadThread = array of Arr2InnerReadThread;
      InnerWriteThread = class external 'com.felhr.usbserial' name 'WriteThread' 
      end;

      InnerReadThread = class external 'com.felhr.usbserial' name 'ReadThread' 
      end;

  strict protected
    const
      USB_TIMEOUT = 5000;
  strict protected
    final var
      fdevice: AHUUsbDevice; external name 'device';
      fconnection: AHUUsbDeviceConnection; external name 'connection';
  strict protected
    var
      fserialBuffer: CFUSerialBuffer; external name 'serialBuffer';
      fwriteThread: CFUUsbSpiDevice.InnerWriteThread; external name 'writeThread';
      freadThread: CFUUsbSpiDevice.InnerReadThread; external name 'readThread';
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    class function createUsbSerialDevice(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection): CFUUsbSpiDevice; static; overload;
    class function createUsbSerialDevice(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint): CFUUsbSpiDevice; static; overload;
    function connectSPI(): jboolean; overload; virtual; abstract;
    procedure writeMOSI(para1: Arr1jbyte); overload; virtual; abstract;
    procedure writeMOSI(var para1: array of jbyte); overload; virtual; abstract;
    procedure readMISO(para1: jint); overload; virtual; abstract;
    procedure writeRead(para1: Arr1jbyte; para2: jint); overload; virtual; abstract;
    procedure writeRead(var para1: array of jbyte; para2: jint); overload; virtual; abstract;
    procedure setClock(para1: jint); overload; virtual; abstract;
    procedure selectSlave(para1: jint); overload; virtual; abstract;
    procedure setMISOCallback(para1: CFUUsbSpiInterface.InnerUsbMISOCallback); overload; virtual;
    function getClockDivider(): jint; overload; virtual; abstract;
    function getSelectedSlave(): jint; overload; virtual; abstract;
    procedure closeSPI(); overload; virtual; abstract;
  strict protected
    procedure setThreadsParams(para1: AHUUsbEndpoint; para2: AHUUsbEndpoint); overload; virtual;
    procedure killWorkingThread(); overload; virtual;
    procedure restartWorkingThread(); overload; virtual;
    procedure killWriteThread(); overload; virtual;
    procedure restartWriteThread(); overload; virtual;
  end;

  CFUBLED112SerialDevice = class external 'com.felhr.usbserial' name 'BLED112SerialDevice' (CFUUsbSerialDevice)
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
  end;

  CFUCDCSerialDevice = class external 'com.felhr.usbserial' name 'CDCSerialDevice' (CFUUsbSerialDevice)
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
  end;

  CFUCH34xSerialDevice = class external 'com.felhr.usbserial' name 'CH34xSerialDevice' (CFUUsbSerialDevice)
  public
    type
      InnerFlowControlThread = class;
      Arr1InnerFlowControlThread = array of InnerFlowControlThread;
      Arr2InnerFlowControlThread = array of Arr1InnerFlowControlThread;
      Arr3InnerFlowControlThread = array of Arr2InnerFlowControlThread;
      InnerFlowControlThread = class external 'com.felhr.usbserial' name 'FlowControlThread' 
      end;

  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
  end;

  CFUCP2102SerialDevice = class external 'com.felhr.usbserial' name 'CP2102SerialDevice' (CFUUsbSerialDevice)
  public
    type
      InnerFlowControlThread = class;
      Arr1InnerFlowControlThread = array of InnerFlowControlThread;
      Arr2InnerFlowControlThread = array of Arr1InnerFlowControlThread;
      Arr3InnerFlowControlThread = array of Arr2InnerFlowControlThread;
      InnerFlowControlThread = class external 'com.felhr.usbserial' name 'FlowControlThread' 
      end;

  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
  end;

  CFUFTDISerialDevice = class external 'com.felhr.usbserial' name 'FTDISerialDevice' (CFUUsbSerialDevice)
  public
    type
      InnerFTDIUtilities = class;
      Arr1InnerFTDIUtilities = array of InnerFTDIUtilities;
      Arr2InnerFTDIUtilities = array of Arr1InnerFTDIUtilities;
      Arr3InnerFTDIUtilities = array of Arr2InnerFTDIUtilities;
      InnerFTDIUtilities = class external 'com.felhr.usbserial' name 'FTDIUtilities' 
      end;

  public
    const
      FTDI_BAUDRATE_300 = 10000;
      FTDI_BAUDRATE_600 = 5000;
      FTDI_BAUDRATE_1200 = 2500;
      FTDI_BAUDRATE_2400 = 1250;
      FTDI_BAUDRATE_4800 = 625;
      FTDI_BAUDRATE_9600 = 16696;
      FTDI_BAUDRATE_19200 = 32924;
      FTDI_BAUDRATE_38400 = 49230;
      FTDI_BAUDRATE_57600 = 52;
      FTDI_BAUDRATE_115200 = 26;
      FTDI_BAUDRATE_230400 = 13;
      FTDI_BAUDRATE_460800 = 16390;
      FTDI_BAUDRATE_921600 = 32771;
  public
    var
      fftdiUtilities: CFUFTDISerialDevice.InnerFTDIUtilities; external name 'ftdiUtilities';
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
    function syncRead(para1: Arr1jbyte; para2: jint): jint; overload; virtual;
    function syncRead(var para1: array of jbyte; para2: jint): jint; overload; virtual;
  end;

  CFUPL2303SerialDevice = class external 'com.felhr.usbserial' name 'PL2303SerialDevice' (CFUUsbSerialDevice)
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
  end;

  CFUXdcVcpSerialDevice = class external 'com.felhr.usbserial' name 'XdcVcpSerialDevice' (CFUUsbSerialDevice)
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function open(): jboolean; overload; virtual;
    procedure close(); overload; virtual;
    function syncOpen(): jboolean; overload; virtual;
    procedure syncClose(); overload; virtual;
    procedure setBaudRate(para1: jint); overload; virtual;
    procedure setDataBits(para1: jint); overload; virtual;
    procedure setStopBits(para1: jint); overload; virtual;
    procedure setParity(para1: jint); overload; virtual;
    procedure setFlowControl(para1: jint); overload; virtual;
    procedure setRTS(para1: jboolean); overload; virtual;
    procedure setDTR(para1: jboolean); overload; virtual;
    procedure getCTS(para1: CFUUsbSerialInterface.InnerUsbCTSCallback); overload; virtual;
    procedure getDSR(para1: CFUUsbSerialInterface.InnerUsbDSRCallback); overload; virtual;
    procedure getBreak(para1: CFUUsbSerialInterface.InnerUsbBreakCallback); overload; virtual;
    procedure getFrame(para1: CFUUsbSerialInterface.InnerUsbFrameCallback); overload; virtual;
    procedure getOverrun(para1: CFUUsbSerialInterface.InnerUsbOverrunCallback); overload; virtual;
    procedure getParity(para1: CFUUsbSerialInterface.InnerUsbParityCallback); overload; virtual;
  end;

  CFUCP2130SpiDevice = class external 'com.felhr.usbserial' name 'CP2130SpiDevice' (CFUUsbSpiDevice)
  public
    const
      CLOCK_12MHz = 0;
      CLOCK_6MHz = 1;
      CLOCK_3MHz = 2;
      CLOCK_1_5MHz = 3;
      CLOCK_750KHz = 4;
      CLOCK_375KHz = 5;
      CLOCK_187_5KHz = 6;
      CLOCK_93_75KHz = 7;
  public
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection); overload;
    constructor create(para1: AHUUsbDevice; para2: AHUUsbDeviceConnection; para3: jint); overload;
    function connectSPI(): jboolean; overload; virtual;
    function getSelectedSlave(): jint; overload; virtual;
    procedure writeMOSI(para1: Arr1jbyte); overload; virtual;
    procedure writeMOSI(var para1: array of jbyte); overload; virtual;
    procedure setClock(para1: jint); overload; virtual;
    procedure readMISO(para1: jint); overload; virtual;
    procedure writeRead(para1: Arr1jbyte; para2: jint); overload; virtual;
    procedure writeRead(var para1: array of jbyte; para2: jint); overload; virtual;
    procedure selectSlave(para1: jint); overload; virtual;
    function getClockDivider(): jint; overload; virtual;
    procedure closeSPI(); overload; virtual;
  end;

