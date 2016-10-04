  CAFUUsbAccessory = class external 'com.android.future.usb' name 'UsbAccessory' (JLObject)
  public
    function getManufacturer(): JLString; overload; virtual;
    function getModel(): JLString; overload; virtual;
    function getDescription(): JLString; overload; virtual;
    function getVersion(): JLString; overload; virtual;
    function getUri(): JLString; overload; virtual;
    function getSerial(): JLString; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  CAFUUsbManager = class external 'com.android.future.usb' name 'UsbManager' (JLObject)
  public
    const
      ACTION_USB_ACCESSORY_ATTACHED = 'android.hardware.usb.action.USB_ACCESSORY_ATTACHED';
      ACTION_USB_ACCESSORY_DETACHED = 'android.hardware.usb.action.USB_ACCESSORY_DETACHED';
      EXTRA_PERMISSION_GRANTED = 'permission';
  public
    class function getInstance(para1: ACContext): CAFUUsbManager; static; overload;
    class function getAccessory(para1: ACIntent): CAFUUsbAccessory; static; overload;
    function getAccessoryList(): Arr1CAFUUsbAccessory; overload; virtual;
    function openAccessory(para1: CAFUUsbAccessory): AOParcelFileDescriptor; overload; virtual;
    function hasPermission(para1: CAFUUsbAccessory): jboolean; overload; virtual;
    procedure requestPermission(para1: CAFUUsbAccessory; para2: AAPendingIntent); overload; virtual;
  end;

  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

