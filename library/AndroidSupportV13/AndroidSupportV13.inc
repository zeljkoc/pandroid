  ASVAFragmentCompat = class external 'android.support.v13.app' name 'FragmentCompat' (JLObject)
  public
    type
      InnerICSMR1FragmentCompatImpl = class;
      Arr1InnerICSMR1FragmentCompatImpl = array of InnerICSMR1FragmentCompatImpl;
      Arr2InnerICSMR1FragmentCompatImpl = array of Arr1InnerICSMR1FragmentCompatImpl;
      Arr3InnerICSMR1FragmentCompatImpl = array of Arr2InnerICSMR1FragmentCompatImpl;
      InnerICSFragmentCompatImpl = class;
      Arr1InnerICSFragmentCompatImpl = array of InnerICSFragmentCompatImpl;
      Arr2InnerICSFragmentCompatImpl = array of Arr1InnerICSFragmentCompatImpl;
      Arr3InnerICSFragmentCompatImpl = array of Arr2InnerICSFragmentCompatImpl;
      InnerBaseFragmentCompatImpl = class;
      Arr1InnerBaseFragmentCompatImpl = array of InnerBaseFragmentCompatImpl;
      Arr2InnerBaseFragmentCompatImpl = array of Arr1InnerBaseFragmentCompatImpl;
      Arr3InnerBaseFragmentCompatImpl = array of Arr2InnerBaseFragmentCompatImpl;
      InnerFragmentCompatImpl = interface;
      Arr1InnerFragmentCompatImpl = array of InnerFragmentCompatImpl;
      Arr2InnerFragmentCompatImpl = array of Arr1InnerFragmentCompatImpl;
      Arr3InnerFragmentCompatImpl = array of Arr2InnerFragmentCompatImpl;
      InnerICSMR1FragmentCompatImpl = class external 'android.support.v13.app' name 'ICSMR1FragmentCompatImpl' 
      end;

      InnerICSFragmentCompatImpl = class external 'android.support.v13.app' name 'ICSFragmentCompatImpl' 
      end;

      InnerBaseFragmentCompatImpl = class external 'android.support.v13.app' name 'BaseFragmentCompatImpl' 
      end;

      InnerFragmentCompatImpl = interface external 'android.support.v13.app' name 'FragmentCompatImpl' 
      end;

  public
    constructor create(); overload;
    class procedure setMenuVisibility(para1: AAFragment; para2: jboolean); static; overload;
    class procedure setUserVisibleHint(para1: AAFragment; para2: jboolean); static; overload;
  end;

  ASVAFragmentCompatICS = class external 'android.support.v13.app' name 'FragmentCompatICS' (JLObject)
  public
    class procedure setMenuVisibility(para1: AAFragment; para2: jboolean); static; overload;
  end;

  ASVAFragmentCompatICSMR1 = class external 'android.support.v13.app' name 'FragmentCompatICSMR1' (JLObject)
  public
    class procedure setUserVisibleHint(para1: AAFragment; para2: jboolean); static; overload;
  end;

  ASVAAccessibilityServiceInfoCompat = class external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoCompat' (JLObject)
  public
    type
      InnerAccessibilityServiceInfoJellyBeanMr2 = class;
      Arr1InnerAccessibilityServiceInfoJellyBeanMr2 = array of InnerAccessibilityServiceInfoJellyBeanMr2;
      Arr2InnerAccessibilityServiceInfoJellyBeanMr2 = array of Arr1InnerAccessibilityServiceInfoJellyBeanMr2;
      Arr3InnerAccessibilityServiceInfoJellyBeanMr2 = array of Arr2InnerAccessibilityServiceInfoJellyBeanMr2;
      InnerAccessibilityServiceInfoIcsImpl = class;
      Arr1InnerAccessibilityServiceInfoIcsImpl = array of InnerAccessibilityServiceInfoIcsImpl;
      Arr2InnerAccessibilityServiceInfoIcsImpl = array of Arr1InnerAccessibilityServiceInfoIcsImpl;
      Arr3InnerAccessibilityServiceInfoIcsImpl = array of Arr2InnerAccessibilityServiceInfoIcsImpl;
      InnerAccessibilityServiceInfoStubImpl = class;
      Arr1InnerAccessibilityServiceInfoStubImpl = array of InnerAccessibilityServiceInfoStubImpl;
      Arr2InnerAccessibilityServiceInfoStubImpl = array of Arr1InnerAccessibilityServiceInfoStubImpl;
      Arr3InnerAccessibilityServiceInfoStubImpl = array of Arr2InnerAccessibilityServiceInfoStubImpl;
      InnerAccessibilityServiceInfoVersionImpl = interface;
      Arr1InnerAccessibilityServiceInfoVersionImpl = array of InnerAccessibilityServiceInfoVersionImpl;
      Arr2InnerAccessibilityServiceInfoVersionImpl = array of Arr1InnerAccessibilityServiceInfoVersionImpl;
      Arr3InnerAccessibilityServiceInfoVersionImpl = array of Arr2InnerAccessibilityServiceInfoVersionImpl;
      InnerAccessibilityServiceInfoJellyBeanMr2 = class external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoJellyBeanMr2' 
      end;

      InnerAccessibilityServiceInfoIcsImpl = class external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoIcsImpl' 
      end;

      InnerAccessibilityServiceInfoStubImpl = class external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoStubImpl' 
      end;

      InnerAccessibilityServiceInfoVersionImpl = interface external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoVersionImpl' 
      end;

  public
    const
      CAPABILITY_CAN_RETRIEVE_WINDOW_CONTENT = 1;
      CAPABILITY_CAN_REQUEST_TOUCH_EXPLORATION = 2;
      CAPABILITY_CAN_REQUEST_ENHANCED_WEB_ACCESSIBILITY = 4;
      CAPABILITY_CAN_FILTER_KEY_EVENTS = 8;
      FEEDBACK_BRAILLE = 32;
      FEEDBACK_ALL_MASK = -1;
      DEFAULT = 1;
      FLAG_INCLUDE_NOT_IMPORTANT_VIEWS = 2;
      FLAG_REQUEST_TOUCH_EXPLORATION_MODE = 4;
      FLAG_REQUEST_ENHANCED_WEB_ACCESSIBILITY = 8;
      FLAG_REPORT_VIEW_IDS = 16;
      FLAG_REQUEST_FILTER_KEY_EVENTS = 32;
  public
    class function getId(para1: AAAccessibilityServiceInfo): JLString; static; overload;
    class function getResolveInfo(para1: AAAccessibilityServiceInfo): ACPResolveInfo; static; overload;
    class function getSettingsActivityName(para1: AAAccessibilityServiceInfo): JLString; static; overload;
    class function getCanRetrieveWindowContent(para1: AAAccessibilityServiceInfo): jboolean; static; overload;
    class function getDescription(para1: AAAccessibilityServiceInfo): JLString; static; overload;
    class function feedbackTypeToString(para1: jint): JLString; static; overload;
    class function flagToString(para1: jint): JLString; static; overload;
    class function getCapabilities(para1: AAAccessibilityServiceInfo): jint; static; overload;
    class function capabilityToString(para1: jint): JLString; static; overload;
  end;

  ASVAAccessibilityServiceInfoCompatIcs = class external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoCompatIcs' (JLObject)
  public
    class function getCanRetrieveWindowContent(para1: AAAccessibilityServiceInfo): jboolean; static; overload;
    class function getDescription(para1: AAAccessibilityServiceInfo): JLString; static; overload;
    class function getId(para1: AAAccessibilityServiceInfo): JLString; static; overload;
    class function getResolveInfo(para1: AAAccessibilityServiceInfo): ACPResolveInfo; static; overload;
    class function getSettingsActivityName(para1: AAAccessibilityServiceInfo): JLString; static; overload;
  end;

  ASVAAccessibilityServiceInfoCompatJellyBeanMr2 = class external 'android.support.v4.accessibilityservice' name 'AccessibilityServiceInfoCompatJellyBeanMr2' (JLObject)
  public
    class function getCapabilities(para1: AAAccessibilityServiceInfo): jint; static; overload;
  end;

  ASVAActionBarDrawerToggleHoneycomb = class external 'android.support.v4.app' name 'ActionBarDrawerToggleHoneycomb' (JLObject)
  public
    type
      InnerSetIndicatorInfo = class;
      Arr1InnerSetIndicatorInfo = array of InnerSetIndicatorInfo;
      Arr2InnerSetIndicatorInfo = array of Arr1InnerSetIndicatorInfo;
      Arr3InnerSetIndicatorInfo = array of Arr2InnerSetIndicatorInfo;
      InnerSetIndicatorInfo = class external 'android.support.v4.app' name 'SetIndicatorInfo' 
      end;

  public
    class function setActionBarUpIndicator(para1: JLObject; para2: AAActivity; para3: AGDDrawable; para4: jint): JLObject; static; overload;
    class function setActionBarDescription(para1: JLObject; para2: AAActivity; para3: jint): JLObject; static; overload;
    class function getThemeUpIndicator(para1: AAActivity): AGDDrawable; static; overload;
  end;

  ASVAActivityCompatHoneycomb = class external 'android.support.v4.app' name 'ActivityCompatHoneycomb' (JLObject)
  end;

  ASVAActivityCompatJB = class external 'android.support.v4.app' name 'ActivityCompatJB' (JLObject)
  public
    class procedure startActivity(para1: ACContext; para2: ACIntent; para3: AOBundle); static; overload;
    class procedure startActivityForResult(para1: AAActivity; para2: ACIntent; para3: jint; para4: AOBundle); static; overload;
    class procedure finishAffinity(para1: AAActivity); static; overload;
  end;

  ASVAActivityOptionsCompat = class external 'android.support.v4.app' name 'ActivityOptionsCompat' (JLObject)
  public
    type
      InnerActivityOptionsImplJB = class;
      Arr1InnerActivityOptionsImplJB = array of InnerActivityOptionsImplJB;
      Arr2InnerActivityOptionsImplJB = array of Arr1InnerActivityOptionsImplJB;
      Arr3InnerActivityOptionsImplJB = array of Arr2InnerActivityOptionsImplJB;
      InnerActivityOptionsImplJB = class external 'android.support.v4.app' name 'ActivityOptionsImplJB' 
      end;

  public
    class function makeCustomAnimation(para1: ACContext; para2: jint; para3: jint): ASVAActivityOptionsCompat; static; overload;
    class function makeScaleUpAnimation(para1: AVView; para2: jint; para3: jint; para4: jint; para5: jint): ASVAActivityOptionsCompat; static; overload;
    class function makeThumbnailScaleUpAnimation(para1: AVView; para2: AGBitmap; para3: jint; para4: jint): ASVAActivityOptionsCompat; static; overload;
  strict protected
    constructor create(); overload;
  public
    function toBundle(): AOBundle; overload; virtual;
    procedure update(para1: ASVAActivityOptionsCompat); overload; virtual;
  end;

  ASVAActivityOptionsCompatJB = class external 'android.support.v4.app' name 'ActivityOptionsCompatJB' (JLObject)
  public
    class function makeCustomAnimation(para1: ACContext; para2: jint; para3: jint): ASVAActivityOptionsCompatJB; static; overload;
    class function makeScaleUpAnimation(para1: AVView; para2: jint; para3: jint; para4: jint; para5: jint): ASVAActivityOptionsCompatJB; static; overload;
    class function makeThumbnailScaleUpAnimation(para1: AVView; para2: AGBitmap; para3: jint; para4: jint): ASVAActivityOptionsCompatJB; static; overload;
    function toBundle(): AOBundle; overload; virtual;
    procedure update(para1: ASVAActivityOptionsCompatJB); overload; virtual;
  end;

  ASVAFragmentContainer = interface external 'android.support.v4.app' name 'FragmentContainer' 
    function findViewById(para1: jint): AVView; overload;
  end;

  ASVAFragmentTransaction = class abstract external 'android.support.v4.app' name 'FragmentTransaction' (JLObject)
  public
    const
      TRANSIT_ENTER_MASK = 4096;
      TRANSIT_EXIT_MASK = 8192;
      TRANSIT_UNSET = -1;
      TRANSIT_NONE = 0;
      TRANSIT_FRAGMENT_OPEN = 4097;
      TRANSIT_FRAGMENT_CLOSE = 8194;
      TRANSIT_FRAGMENT_FADE = 4099;
  public
    constructor create(); overload;
    function add(para1: ASVAFragment; para2: JLString): ASVAFragmentTransaction; overload; virtual; abstract;
    function add(para1: jint; para2: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function add(para1: jint; para2: ASVAFragment; para3: JLString): ASVAFragmentTransaction; overload; virtual; abstract;
    function replace(para1: jint; para2: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function replace(para1: jint; para2: ASVAFragment; para3: JLString): ASVAFragmentTransaction; overload; virtual; abstract;
    function remove(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function hide(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function show(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function detach(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function attach(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual; abstract;
    function isEmpty(): jboolean; overload; virtual; abstract;
    function setCustomAnimations(para1: jint; para2: jint): ASVAFragmentTransaction; overload; virtual; abstract;
    function setCustomAnimations(para1: jint; para2: jint; para3: jint; para4: jint): ASVAFragmentTransaction; overload; virtual; abstract;
    function setTransition(para1: jint): ASVAFragmentTransaction; overload; virtual; abstract;
    function setTransitionStyle(para1: jint): ASVAFragmentTransaction; overload; virtual; abstract;
    function addToBackStack(para1: JLString): ASVAFragmentTransaction; overload; virtual; abstract;
    function isAddToBackStackAllowed(): jboolean; overload; virtual; abstract;
    function disallowAddToBackStack(): ASVAFragmentTransaction; overload; virtual; abstract;
    function setBreadCrumbTitle(para1: jint): ASVAFragmentTransaction; overload; virtual; abstract;
    function setBreadCrumbTitle(para1: JLCharSequence): ASVAFragmentTransaction; overload; virtual; abstract;
    function setBreadCrumbShortTitle(para1: jint): ASVAFragmentTransaction; overload; virtual; abstract;
    function setBreadCrumbShortTitle(para1: JLCharSequence): ASVAFragmentTransaction; overload; virtual; abstract;
    function commit(): jint; overload; virtual; abstract;
    function commitAllowingStateLoss(): jint; overload; virtual; abstract;
  end;

  ASVALoaderManager = class abstract external 'android.support.v4.app' name 'LoaderManager' (JLObject)
  public
    type
      InnerLoaderCallbacks = interface;
      Arr1InnerLoaderCallbacks = array of InnerLoaderCallbacks;
      Arr2InnerLoaderCallbacks = array of Arr1InnerLoaderCallbacks;
      Arr3InnerLoaderCallbacks = array of Arr2InnerLoaderCallbacks;
      InnerLoaderCallbacks = interface external 'android.support.v4.app' name 'LoaderCallbacks' 
        function onCreateLoader(para1: jint; para2: AOBundle): ASVCLoader; overload;
        procedure onLoadFinished(para1: ASVCLoader; para2: JLObject); overload;
        procedure onLoaderReset(para1: ASVCLoader); overload;
      end;

  public
    constructor create(); overload;
    function initLoader(para1: jint; para2: AOBundle; para3: ASVALoaderManager.InnerLoaderCallbacks): ASVCLoader; overload; virtual; abstract;
    function restartLoader(para1: jint; para2: AOBundle; para3: ASVALoaderManager.InnerLoaderCallbacks): ASVCLoader; overload; virtual; abstract;
    procedure destroyLoader(para1: jint); overload; virtual; abstract;
    function getLoader(para1: jint): ASVCLoader; overload; virtual; abstract;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual; abstract;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual; abstract;
    class procedure enableDebugLogging(para1: jboolean); static; overload;
    function hasRunningLoaders(): jboolean; overload; virtual;
  end;

  ASVANavUtils = class external 'android.support.v4.app' name 'NavUtils' (JLObject)
  public
    type
      InnerNavUtilsImplJB = class;
      Arr1InnerNavUtilsImplJB = array of InnerNavUtilsImplJB;
      Arr2InnerNavUtilsImplJB = array of Arr1InnerNavUtilsImplJB;
      Arr3InnerNavUtilsImplJB = array of Arr2InnerNavUtilsImplJB;
      InnerNavUtilsImplBase = class;
      Arr1InnerNavUtilsImplBase = array of InnerNavUtilsImplBase;
      Arr2InnerNavUtilsImplBase = array of Arr1InnerNavUtilsImplBase;
      Arr3InnerNavUtilsImplBase = array of Arr2InnerNavUtilsImplBase;
      InnerNavUtilsImpl = interface;
      Arr1InnerNavUtilsImpl = array of InnerNavUtilsImpl;
      Arr2InnerNavUtilsImpl = array of Arr1InnerNavUtilsImpl;
      Arr3InnerNavUtilsImpl = array of Arr2InnerNavUtilsImpl;
      InnerNavUtilsImplJB = class external 'android.support.v4.app' name 'NavUtilsImplJB' 
      end;

      InnerNavUtilsImplBase = class external 'android.support.v4.app' name 'NavUtilsImplBase' 
      end;

      InnerNavUtilsImpl = interface external 'android.support.v4.app' name 'NavUtilsImpl' 
      end;

  public
    const
      PARENT_ACTIVITY = 'android.support.PARENT_ACTIVITY';
  public
    class function shouldUpRecreateTask(para1: AAActivity; para2: ACIntent): jboolean; static; overload;
    class procedure navigateUpFromSameTask(para1: AAActivity); static; overload;
    class procedure navigateUpTo(para1: AAActivity; para2: ACIntent); static; overload;
    class function getParentActivityIntent(para1: AAActivity): ACIntent; static; overload;
    class function getParentActivityIntent(para1: ACContext; para2: JLClass): ACIntent; static; overload;  // throws android.content.pm.PackageManager$NameNotFoundException
    class function getParentActivityIntent(para1: ACContext; para2: ACComponentName): ACIntent; static; overload;  // throws android.content.pm.PackageManager$NameNotFoundException
    class function getParentActivityName(para1: AAActivity): JLString; static; overload;
    class function getParentActivityName(para1: ACContext; para2: ACComponentName): JLString; static; overload;  // throws android.content.pm.PackageManager$NameNotFoundException
  end;

  ASVANavUtilsJB = class external 'android.support.v4.app' name 'NavUtilsJB' (JLObject)
  public
    class function getParentActivityIntent(para1: AAActivity): ACIntent; static; overload;
    class function shouldUpRecreateTask(para1: AAActivity; para2: ACIntent): jboolean; static; overload;
    class procedure navigateUpTo(para1: AAActivity; para2: ACIntent); static; overload;
    class function getParentActivityName(para1: ACPActivityInfo): JLString; static; overload;
  end;

  ASVANotificationCompat = class external 'android.support.v4.app' name 'NotificationCompat' (JLObject)
  public
    type
      InnerNotificationCompatImplJellybean = class;
      Arr1InnerNotificationCompatImplJellybean = array of InnerNotificationCompatImplJellybean;
      Arr2InnerNotificationCompatImplJellybean = array of Arr1InnerNotificationCompatImplJellybean;
      Arr3InnerNotificationCompatImplJellybean = array of Arr2InnerNotificationCompatImplJellybean;
      InnerNotificationCompatImplIceCreamSandwich = class;
      Arr1InnerNotificationCompatImplIceCreamSandwich = array of InnerNotificationCompatImplIceCreamSandwich;
      Arr2InnerNotificationCompatImplIceCreamSandwich = array of Arr1InnerNotificationCompatImplIceCreamSandwich;
      Arr3InnerNotificationCompatImplIceCreamSandwich = array of Arr2InnerNotificationCompatImplIceCreamSandwich;
      InnerNotificationCompatImplHoneycomb = class;
      Arr1InnerNotificationCompatImplHoneycomb = array of InnerNotificationCompatImplHoneycomb;
      Arr2InnerNotificationCompatImplHoneycomb = array of Arr1InnerNotificationCompatImplHoneycomb;
      Arr3InnerNotificationCompatImplHoneycomb = array of Arr2InnerNotificationCompatImplHoneycomb;
      InnerNotificationCompatImplBase = class;
      Arr1InnerNotificationCompatImplBase = array of InnerNotificationCompatImplBase;
      Arr2InnerNotificationCompatImplBase = array of Arr1InnerNotificationCompatImplBase;
      Arr3InnerNotificationCompatImplBase = array of Arr2InnerNotificationCompatImplBase;
      InnerNotificationCompatImpl = interface;
      Arr1InnerNotificationCompatImpl = array of InnerNotificationCompatImpl;
      Arr2InnerNotificationCompatImpl = array of Arr1InnerNotificationCompatImpl;
      Arr3InnerNotificationCompatImpl = array of Arr2InnerNotificationCompatImpl;
      InnerNotificationCompatImplJellybean = class external 'android.support.v4.app' name 'NotificationCompatImplJellybean' 
      end;

      InnerNotificationCompatImplIceCreamSandwich = class external 'android.support.v4.app' name 'NotificationCompatImplIceCreamSandwich' 
      end;

      InnerNotificationCompatImplHoneycomb = class external 'android.support.v4.app' name 'NotificationCompatImplHoneycomb' 
      end;

      InnerNotificationCompatImplBase = class external 'android.support.v4.app' name 'NotificationCompatImplBase' 
      end;

      InnerNotificationCompatImpl = interface external 'android.support.v4.app' name 'NotificationCompatImpl' 
      end;

  public
    type
      InnerAction = class;
      Arr1InnerAction = array of InnerAction;
      Arr2InnerAction = array of Arr1InnerAction;
      Arr3InnerAction = array of Arr2InnerAction;
      InnerStyle = class;
      Arr1InnerStyle = array of InnerStyle;
      Arr2InnerStyle = array of Arr1InnerStyle;
      Arr3InnerStyle = array of Arr2InnerStyle;
      InnerBuilder = class;
      Arr1InnerBuilder = array of InnerBuilder;
      Arr2InnerBuilder = array of Arr1InnerBuilder;
      Arr3InnerBuilder = array of Arr2InnerBuilder;
      InnerInboxStyle = class;
      Arr1InnerInboxStyle = array of InnerInboxStyle;
      Arr2InnerInboxStyle = array of Arr1InnerInboxStyle;
      Arr3InnerInboxStyle = array of Arr2InnerInboxStyle;
      InnerBigTextStyle = class;
      Arr1InnerBigTextStyle = array of InnerBigTextStyle;
      Arr2InnerBigTextStyle = array of Arr1InnerBigTextStyle;
      Arr3InnerBigTextStyle = array of Arr2InnerBigTextStyle;
      InnerBigPictureStyle = class;
      Arr1InnerBigPictureStyle = array of InnerBigPictureStyle;
      Arr2InnerBigPictureStyle = array of Arr1InnerBigPictureStyle;
      Arr3InnerBigPictureStyle = array of Arr2InnerBigPictureStyle;
      InnerAction = class external 'android.support.v4.app' name 'Action' (JLObject)
      public
        var
          ficon: jint; external name 'icon';
          ftitle: JLCharSequence; external name 'title';
          factionIntent: AAPendingIntent; external name 'actionIntent';
      public
        constructor create(para1: jint; para2: JLCharSequence; para3: AAPendingIntent); overload;
      end;

      InnerStyle = class abstract external 'android.support.v4.app' name 'Style' (JLObject)
      public
        constructor create(); overload;
        procedure setBuilder(para1: ASVANotificationCompat.InnerBuilder); overload; virtual;
        function build(): AANotification; overload; virtual;
      end;

      InnerBuilder = class external 'android.support.v4.app' name 'Builder' (JLObject)
      public
        constructor create(para1: ACContext); overload;
        function setWhen(para1: jlong): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setUsesChronometer(para1: jboolean): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setSmallIcon(para1: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setSmallIcon(para1: jint; para2: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setContentTitle(para1: JLCharSequence): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setContentText(para1: JLCharSequence): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setSubText(para1: JLCharSequence): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setNumber(para1: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setContentInfo(para1: JLCharSequence): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setProgress(para1: jint; para2: jint; para3: jboolean): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setContent(para1: AWRemoteViews): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setContentIntent(para1: AAPendingIntent): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setDeleteIntent(para1: AAPendingIntent): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setFullScreenIntent(para1: AAPendingIntent; para2: jboolean): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setTicker(para1: JLCharSequence): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setTicker(para1: JLCharSequence; para2: AWRemoteViews): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setLargeIcon(para1: AGBitmap): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setSound(para1: ANUri): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setSound(para1: ANUri; para2: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setVibrate(para1: Arr1jlong): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setVibrate(var para1: array of jlong): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setLights(para1: jint; para2: jint; para3: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setOngoing(para1: jboolean): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setOnlyAlertOnce(para1: jboolean): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setAutoCancel(para1: jboolean): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setDefaults(para1: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setPriority(para1: jint): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function addAction(para1: jint; para2: JLCharSequence; para3: AAPendingIntent): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function setStyle(para1: ASVANotificationCompat.InnerStyle): ASVANotificationCompat.InnerBuilder; overload; virtual;
        function getNotification(): AANotification; overload; virtual;
        function build(): AANotification; overload; virtual;
      end;

      InnerInboxStyle = class external 'android.support.v4.app' name 'InboxStyle' (InnerStyle)
      public
        constructor create(); overload;
        constructor create(para1: ASVANotificationCompat.InnerBuilder); overload;
        function setBigContentTitle(para1: JLCharSequence): ASVANotificationCompat.InnerInboxStyle; overload; virtual;
        function setSummaryText(para1: JLCharSequence): ASVANotificationCompat.InnerInboxStyle; overload; virtual;
        function addLine(para1: JLCharSequence): ASVANotificationCompat.InnerInboxStyle; overload; virtual;
      end;

      InnerBigTextStyle = class external 'android.support.v4.app' name 'BigTextStyle' (InnerStyle)
      public
        constructor create(); overload;
        constructor create(para1: ASVANotificationCompat.InnerBuilder); overload;
        function setBigContentTitle(para1: JLCharSequence): ASVANotificationCompat.InnerBigTextStyle; overload; virtual;
        function setSummaryText(para1: JLCharSequence): ASVANotificationCompat.InnerBigTextStyle; overload; virtual;
        function bigText(para1: JLCharSequence): ASVANotificationCompat.InnerBigTextStyle; overload; virtual;
      end;

      InnerBigPictureStyle = class external 'android.support.v4.app' name 'BigPictureStyle' (InnerStyle)
      public
        constructor create(); overload;
        constructor create(para1: ASVANotificationCompat.InnerBuilder); overload;
        function setBigContentTitle(para1: JLCharSequence): ASVANotificationCompat.InnerBigPictureStyle; overload; virtual;
        function setSummaryText(para1: JLCharSequence): ASVANotificationCompat.InnerBigPictureStyle; overload; virtual;
        function bigPicture(para1: AGBitmap): ASVANotificationCompat.InnerBigPictureStyle; overload; virtual;
        function bigLargeIcon(para1: AGBitmap): ASVANotificationCompat.InnerBigPictureStyle; overload; virtual;
      end;

  public
    const
      FLAG_HIGH_PRIORITY = 128 deprecated;
      PRIORITY_DEFAULT = 0;
      PRIORITY_LOW = -1;
      PRIORITY_MIN = -2;
      PRIORITY_HIGH = 1;
      PRIORITY_MAX = 2;
  public
    constructor create(); overload;
  end;

  ASVANotificationCompatHoneycomb = class external 'android.support.v4.app' name 'NotificationCompatHoneycomb' (JLObject)
  end;

  ASVANotificationCompatIceCreamSandwich = class external 'android.support.v4.app' name 'NotificationCompatIceCreamSandwich' (JLObject)
  end;

  ASVANotificationCompatJellybean = class external 'android.support.v4.app' name 'NotificationCompatJellybean' (JLObject)
  public
    constructor create(para1: ACContext; para2: AANotification; para3: JLCharSequence; para4: JLCharSequence; para5: JLCharSequence; para6: AWRemoteViews; para7: jint; para8: AAPendingIntent; para9: AAPendingIntent; para10: AGBitmap; para11: jint; para12: jint; para13: jboolean; para14: jboolean; para15: jint; para16: JLCharSequence); overload;
    procedure addAction(para1: jint; para2: JLCharSequence; para3: AAPendingIntent); overload; virtual;
    procedure addBigTextStyle(para1: JLCharSequence; para2: jboolean; para3: JLCharSequence; para4: JLCharSequence); overload; virtual;
    procedure addBigPictureStyle(para1: JLCharSequence; para2: jboolean; para3: JLCharSequence; para4: AGBitmap; para5: AGBitmap; para6: jboolean); overload; virtual;
    procedure addInboxStyle(para1: JLCharSequence; para2: jboolean; para3: JLCharSequence; para4: JUArrayList); overload; virtual;
    function build(): AANotification; overload; virtual;
  end;

  ASVAServiceCompat = class external 'android.support.v4.app' name 'ServiceCompat' (JLObject)
  public
    const
      START_STICKY = 1;
  end;

  ASVAShareCompat = class external 'android.support.v4.app' name 'ShareCompat' (JLObject)
  public
    type
      InnerShareCompatImplJB = class;
      Arr1InnerShareCompatImplJB = array of InnerShareCompatImplJB;
      Arr2InnerShareCompatImplJB = array of Arr1InnerShareCompatImplJB;
      Arr3InnerShareCompatImplJB = array of Arr2InnerShareCompatImplJB;
      InnerShareCompatImplICS = class;
      Arr1InnerShareCompatImplICS = array of InnerShareCompatImplICS;
      Arr2InnerShareCompatImplICS = array of Arr1InnerShareCompatImplICS;
      Arr3InnerShareCompatImplICS = array of Arr2InnerShareCompatImplICS;
      InnerShareCompatImplBase = class;
      Arr1InnerShareCompatImplBase = array of InnerShareCompatImplBase;
      Arr2InnerShareCompatImplBase = array of Arr1InnerShareCompatImplBase;
      Arr3InnerShareCompatImplBase = array of Arr2InnerShareCompatImplBase;
      InnerShareCompatImpl = interface;
      Arr1InnerShareCompatImpl = array of InnerShareCompatImpl;
      Arr2InnerShareCompatImpl = array of Arr1InnerShareCompatImpl;
      Arr3InnerShareCompatImpl = array of Arr2InnerShareCompatImpl;
      InnerShareCompatImplJB = class external 'android.support.v4.app' name 'ShareCompatImplJB' 
      end;

      InnerShareCompatImplICS = class external 'android.support.v4.app' name 'ShareCompatImplICS' 
      end;

      InnerShareCompatImplBase = class external 'android.support.v4.app' name 'ShareCompatImplBase' 
      end;

      InnerShareCompatImpl = interface external 'android.support.v4.app' name 'ShareCompatImpl' 
      end;

  public
    type
      InnerIntentReader = class;
      Arr1InnerIntentReader = array of InnerIntentReader;
      Arr2InnerIntentReader = array of Arr1InnerIntentReader;
      Arr3InnerIntentReader = array of Arr2InnerIntentReader;
      InnerIntentBuilder = class;
      Arr1InnerIntentBuilder = array of InnerIntentBuilder;
      Arr2InnerIntentBuilder = array of Arr1InnerIntentBuilder;
      Arr3InnerIntentBuilder = array of Arr2InnerIntentBuilder;
      InnerIntentReader = class external 'android.support.v4.app' name 'IntentReader' (JLObject)
      public
        class function from(para1: AAActivity): ASVAShareCompat.InnerIntentReader; static; overload;
        function isShareIntent(): jboolean; overload; virtual;
        function isSingleShare(): jboolean; overload; virtual;
        function isMultipleShare(): jboolean; overload; virtual;
        function getType(): JLString; overload; virtual;
        function getText(): JLCharSequence; overload; virtual;
        function getHtmlText(): JLString; overload; virtual;
        function getStream(): ANUri; overload; virtual;
        function getStream(para1: jint): ANUri; overload; virtual;
        function getStreamCount(): jint; overload; virtual;
        function getEmailTo(): Arr1JLString; overload; virtual;
        function getEmailCc(): Arr1JLString; overload; virtual;
        function getEmailBcc(): Arr1JLString; overload; virtual;
        function getSubject(): JLString; overload; virtual;
        function getCallingPackage(): JLString; overload; virtual;
        function getCallingActivity(): ACComponentName; overload; virtual;
        function getCallingActivityIcon(): AGDDrawable; overload; virtual;
        function getCallingApplicationIcon(): AGDDrawable; overload; virtual;
        function getCallingApplicationLabel(): JLCharSequence; overload; virtual;
      end;

      InnerIntentBuilder = class external 'android.support.v4.app' name 'IntentBuilder' (JLObject)
      public
        class function from(para1: AAActivity): ASVAShareCompat.InnerIntentBuilder; static; overload;
        function getIntent(): ACIntent; overload; virtual;
        function createChooserIntent(): ACIntent; overload; virtual;
        procedure startChooser(); overload; virtual;
        function setChooserTitle(para1: JLCharSequence): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setChooserTitle(para1: jint): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setType(para1: JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setText(para1: JLCharSequence): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setHtmlText(para1: JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setStream(para1: ANUri): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addStream(para1: ANUri): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setEmailTo(para1: Arr1JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setEmailTo(var para1: array of JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailTo(para1: JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailTo(para1: Arr1JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailTo(var para1: array of JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setEmailCc(para1: Arr1JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setEmailCc(var para1: array of JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailCc(para1: JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailCc(para1: Arr1JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailCc(var para1: array of JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setEmailBcc(para1: Arr1JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setEmailBcc(var para1: array of JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailBcc(para1: JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailBcc(para1: Arr1JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function addEmailBcc(var para1: array of JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
        function setSubject(para1: JLString): ASVAShareCompat.InnerIntentBuilder; overload; virtual;
      end;

  public
    const
      EXTRA_CALLING_PACKAGE = 'android.support.v4.app.EXTRA_CALLING_PACKAGE';
      EXTRA_CALLING_ACTIVITY = 'android.support.v4.app.EXTRA_CALLING_ACTIVITY';
  public
    constructor create(); overload;
    class function getCallingPackage(para1: AAActivity): JLString; static; overload;
    class function getCallingActivity(para1: AAActivity): ACComponentName; static; overload;
    class procedure configureMenuItem(para1: AVMenuItem; para2: ASVAShareCompat.InnerIntentBuilder); static; overload;
    class procedure configureMenuItem(para1: AVMenu; para2: jint; para3: ASVAShareCompat.InnerIntentBuilder); static; overload;
  end;

  ASVAShareCompatICS = class external 'android.support.v4.app' name 'ShareCompatICS' (JLObject)
  public
    class procedure configureMenuItem(para1: AVMenuItem; para2: AAActivity; para3: ACIntent); static; overload;
  end;

  ASVAShareCompatJB = class external 'android.support.v4.app' name 'ShareCompatJB' (JLObject)
  public
    class function escapeHtml(para1: JLCharSequence): JLString; static; overload;
  end;

  ASVATaskStackBuilderHoneycomb = class external 'android.support.v4.app' name 'TaskStackBuilderHoneycomb' (JLObject)
  public
    class function getActivitiesPendingIntent(para1: ACContext; para2: jint; para3: Arr1ACIntent; para4: jint): AAPendingIntent; static; overload;
    class function getActivitiesPendingIntent(para1: ACContext; para2: jint; var para3: array of ACIntent; para4: jint): AAPendingIntent; static; overload;
  end;

  ASVATaskStackBuilderJellybean = class external 'android.support.v4.app' name 'TaskStackBuilderJellybean' (JLObject)
  public
    class function getActivitiesPendingIntent(para1: ACContext; para2: jint; para3: Arr1ACIntent; para4: jint; para5: AOBundle): AAPendingIntent; static; overload;
    class function getActivitiesPendingIntent(para1: ACContext; para2: jint; var para3: array of ACIntent; para4: jint; para5: AOBundle): AAPendingIntent; static; overload;
  end;

  ASVCContextCompat = class external 'android.support.v4.content' name 'ContextCompat' (JLObject)
  public
    constructor create(); overload;
    class function startActivities(para1: ACContext; para2: Arr1ACIntent): jboolean; static; overload;
    class function startActivities(para1: ACContext; var para2: array of ACIntent): jboolean; static; overload;
    class function startActivities(para1: ACContext; para2: Arr1ACIntent; para3: AOBundle): jboolean; static; overload;
    class function startActivities(para1: ACContext; var para2: array of ACIntent; para3: AOBundle): jboolean; static; overload;
  end;

  ASVCContextCompatHoneycomb = class external 'android.support.v4.content' name 'ContextCompatHoneycomb' (JLObject)
  end;

  ASVCContextCompatJellybean = class external 'android.support.v4.content' name 'ContextCompatJellybean' (JLObject)
  public
    class procedure startActivities(para1: ACContext; para2: Arr1ACIntent; para3: AOBundle); static; overload;
    class procedure startActivities(para1: ACContext; var para2: array of ACIntent; para3: AOBundle); static; overload;
  end;

  ASVCIntentCompat = class external 'android.support.v4.content' name 'IntentCompat' (JLObject)
  public
    type
      InnerIntentCompatImplIcsMr1 = class;
      Arr1InnerIntentCompatImplIcsMr1 = array of InnerIntentCompatImplIcsMr1;
      Arr2InnerIntentCompatImplIcsMr1 = array of Arr1InnerIntentCompatImplIcsMr1;
      Arr3InnerIntentCompatImplIcsMr1 = array of Arr2InnerIntentCompatImplIcsMr1;
      InnerIntentCompatImplHC = class;
      Arr1InnerIntentCompatImplHC = array of InnerIntentCompatImplHC;
      Arr2InnerIntentCompatImplHC = array of Arr1InnerIntentCompatImplHC;
      Arr3InnerIntentCompatImplHC = array of Arr2InnerIntentCompatImplHC;
      InnerIntentCompatImplBase = class;
      Arr1InnerIntentCompatImplBase = array of InnerIntentCompatImplBase;
      Arr2InnerIntentCompatImplBase = array of Arr1InnerIntentCompatImplBase;
      Arr3InnerIntentCompatImplBase = array of Arr2InnerIntentCompatImplBase;
      InnerIntentCompatImpl = interface;
      Arr1InnerIntentCompatImpl = array of InnerIntentCompatImpl;
      Arr2InnerIntentCompatImpl = array of Arr1InnerIntentCompatImpl;
      Arr3InnerIntentCompatImpl = array of Arr2InnerIntentCompatImpl;
      InnerIntentCompatImplIcsMr1 = class external 'android.support.v4.content' name 'IntentCompatImplIcsMr1' 
      end;

      InnerIntentCompatImplHC = class external 'android.support.v4.content' name 'IntentCompatImplHC' 
      end;

      InnerIntentCompatImplBase = class external 'android.support.v4.content' name 'IntentCompatImplBase' 
      end;

      InnerIntentCompatImpl = interface external 'android.support.v4.content' name 'IntentCompatImpl' 
      end;

  public
    const
      ACTION_EXTERNAL_APPLICATIONS_AVAILABLE = 'android.intent.action.EXTERNAL_APPLICATIONS_AVAILABLE';
      ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE = 'android.intent.action.EXTERNAL_APPLICATIONS_UNAVAILABLE';
      EXTRA_CHANGED_PACKAGE_LIST = 'android.intent.extra.changed_package_list';
      EXTRA_CHANGED_UID_LIST = 'android.intent.extra.changed_uid_list';
      EXTRA_HTML_TEXT = 'android.intent.extra.HTML_TEXT';
      FLAG_ACTIVITY_TASK_ON_HOME = 16384;
      FLAG_ACTIVITY_CLEAR_TASK = 32768;
  public
    class function makeMainActivity(para1: ACComponentName): ACIntent; static; overload;
    class function makeMainSelectorActivity(para1: JLString; para2: JLString): ACIntent; static; overload;
    class function makeRestartActivityTask(para1: ACComponentName): ACIntent; static; overload;
  end;

  ASVCIntentCompatHoneycomb = class external 'android.support.v4.content' name 'IntentCompatHoneycomb' (JLObject)
  public
    class function makeMainActivity(para1: ACComponentName): ACIntent; static; overload;
    class function makeRestartActivityTask(para1: ACComponentName): ACIntent; static; overload;
  end;

  ASVCIntentCompatIcsMr1 = class external 'android.support.v4.content' name 'IntentCompatIcsMr1' (JLObject)
  public
    class function makeMainSelectorActivity(para1: JLString; para2: JLString): ACIntent; static; overload;
  end;

  ASVCLoader = class external 'android.support.v4.content' name 'Loader' (JLObject)
  public
    type
      InnerOnLoadCompleteListener = interface;
      Arr1InnerOnLoadCompleteListener = array of InnerOnLoadCompleteListener;
      Arr2InnerOnLoadCompleteListener = array of Arr1InnerOnLoadCompleteListener;
      Arr3InnerOnLoadCompleteListener = array of Arr2InnerOnLoadCompleteListener;
      InnerForceLoadContentObserver = class;
      Arr1InnerForceLoadContentObserver = array of InnerForceLoadContentObserver;
      Arr2InnerForceLoadContentObserver = array of Arr1InnerForceLoadContentObserver;
      Arr3InnerForceLoadContentObserver = array of Arr2InnerForceLoadContentObserver;
      InnerOnLoadCompleteListener = interface external 'android.support.v4.content' name 'OnLoadCompleteListener' 
        procedure onLoadComplete(para1: ASVCLoader; para2: JLObject); overload;
      end;

      InnerForceLoadContentObserver = class sealed external 'android.support.v4.content' name 'ForceLoadContentObserver' 
      end;

  public
    constructor create(para1: ACContext); overload;
    procedure deliverResult(para1: JLObject); overload; virtual;
    function getContext(): ACContext; overload; virtual;
    function getId(): jint; overload; virtual;
    procedure registerListener(para1: jint; para2: ASVCLoader.InnerOnLoadCompleteListener); overload; virtual;
    procedure unregisterListener(para1: ASVCLoader.InnerOnLoadCompleteListener); overload; virtual;
    function isStarted(): jboolean; overload; virtual;
    function isAbandoned(): jboolean; overload; virtual;
    function isReset(): jboolean; overload; virtual;
    procedure startLoading(); overload; virtual; final;
  strict protected
    procedure onStartLoading(); overload; virtual;
  public
    procedure forceLoad(); overload; virtual;
  strict protected
    procedure onForceLoad(); overload; virtual;
  public
    procedure stopLoading(); overload; virtual;
  strict protected
    procedure onStopLoading(); overload; virtual;
  public
    procedure abandon(); overload; virtual;
  strict protected
    procedure onAbandon(); overload; virtual;
  public
    procedure reset(); overload; virtual;
  strict protected
    procedure onReset(); overload; virtual;
  public
    function takeContentChanged(): jboolean; overload; virtual;
    procedure commitContentChanged(); overload; virtual;
    procedure rollbackContentChanged(); overload; virtual;
    procedure onContentChanged(); overload; virtual;
    function dataToString(para1: JLObject): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
  end;

  ASVCLocalBroadcastManager = class external 'android.support.v4.content' name 'LocalBroadcastManager' (JLObject)
  public
    type
      InnerBroadcastRecord = class;
      Arr1InnerBroadcastRecord = array of InnerBroadcastRecord;
      Arr2InnerBroadcastRecord = array of Arr1InnerBroadcastRecord;
      Arr3InnerBroadcastRecord = array of Arr2InnerBroadcastRecord;
      InnerReceiverRecord = class;
      Arr1InnerReceiverRecord = array of InnerReceiverRecord;
      Arr2InnerReceiverRecord = array of Arr1InnerReceiverRecord;
      Arr3InnerReceiverRecord = array of Arr2InnerReceiverRecord;
      InnerBroadcastRecord = class external 'android.support.v4.content' name 'BroadcastRecord' 
      end;

      InnerReceiverRecord = class external 'android.support.v4.content' name 'ReceiverRecord' 
      end;

  public
    class function getInstance(para1: ACContext): ASVCLocalBroadcastManager; static; overload;
    procedure registerReceiver(para1: ACBroadcastReceiver; para2: ACIntentFilter); overload; virtual;
    procedure unregisterReceiver(para1: ACBroadcastReceiver); overload; virtual;
    function sendBroadcast(para1: ACIntent): jboolean; overload; virtual;
    procedure sendBroadcastSync(para1: ACIntent); overload; virtual;
  end;

  ASVCPActivityInfoCompat = class external 'android.support.v4.content.pm' name 'ActivityInfoCompat' (JLObject)
  public
    const
      CONFIG_UI_MODE = 512;
  end;

  ASVDDatabaseUtilsCompat = class external 'android.support.v4.database' name 'DatabaseUtilsCompat' (JLObject)
  public
    class function concatenateWhere(para1: JLString; para2: JLString): JLString; static; overload;
    class function appendSelectionArgs(para1: Arr1JLString; para2: Arr1JLString): Arr1JLString; static; overload;
    class function appendSelectionArgs(var para1: array of JLString; var para2: array of JLString): Arr1JLString; static; overload;
  end;

  ASVGDDrawableCompat = class external 'android.support.v4.graphics.drawable' name 'DrawableCompat' (JLObject)
  public
    type
      InnerDrawableImpl = interface;
      Arr1InnerDrawableImpl = array of InnerDrawableImpl;
      Arr2InnerDrawableImpl = array of Arr1InnerDrawableImpl;
      Arr3InnerDrawableImpl = array of Arr2InnerDrawableImpl;
      InnerHoneycombDrawableImpl = class;
      Arr1InnerHoneycombDrawableImpl = array of InnerHoneycombDrawableImpl;
      Arr2InnerHoneycombDrawableImpl = array of Arr1InnerHoneycombDrawableImpl;
      Arr3InnerHoneycombDrawableImpl = array of Arr2InnerHoneycombDrawableImpl;
      InnerBaseDrawableImpl = class;
      Arr1InnerBaseDrawableImpl = array of InnerBaseDrawableImpl;
      Arr2InnerBaseDrawableImpl = array of Arr1InnerBaseDrawableImpl;
      Arr3InnerBaseDrawableImpl = array of Arr2InnerBaseDrawableImpl;
      InnerDrawableImpl = interface external 'android.support.v4.graphics.drawable' name 'DrawableImpl' 
      end;

      InnerHoneycombDrawableImpl = class external 'android.support.v4.graphics.drawable' name 'HoneycombDrawableImpl' 
      end;

      InnerBaseDrawableImpl = class external 'android.support.v4.graphics.drawable' name 'BaseDrawableImpl' 
      end;

  public
    constructor create(); overload;
    class procedure jumpToCurrentState(para1: AGDDrawable); static; overload;
  end;

  ASVGDDrawableCompatHoneycomb = class external 'android.support.v4.graphics.drawable' name 'DrawableCompatHoneycomb' (JLObject)
  public
    class procedure jumpToCurrentState(para1: AGDDrawable); static; overload;
  end;

  ASVHDDisplayManagerCompat = class abstract external 'android.support.v4.hardware.display' name 'DisplayManagerCompat' (JLObject)
  public
    type
      InnerJellybeanMr1Impl = class;
      Arr1InnerJellybeanMr1Impl = array of InnerJellybeanMr1Impl;
      Arr2InnerJellybeanMr1Impl = array of Arr1InnerJellybeanMr1Impl;
      Arr3InnerJellybeanMr1Impl = array of Arr2InnerJellybeanMr1Impl;
      InnerLegacyImpl = class;
      Arr1InnerLegacyImpl = array of InnerLegacyImpl;
      Arr2InnerLegacyImpl = array of Arr1InnerLegacyImpl;
      Arr3InnerLegacyImpl = array of Arr2InnerLegacyImpl;
      InnerJellybeanMr1Impl = class external 'android.support.v4.hardware.display' name 'JellybeanMr1Impl' 
      end;

      InnerLegacyImpl = class external 'android.support.v4.hardware.display' name 'LegacyImpl' 
      end;

  public
    const
      DISPLAY_CATEGORY_PRESENTATION = 'android.hardware.display.category.PRESENTATION';
  public
    class function getInstance(para1: ACContext): ASVHDDisplayManagerCompat; static; overload;
    function getDisplay(para1: jint): AVDisplay; overload; virtual; abstract;
    function getDisplays(): Arr1AVDisplay; overload; virtual; abstract;
    function getDisplays(para1: JLString): Arr1AVDisplay; overload; virtual; abstract;
  end;

  ASVHDDisplayManagerJellybeanMr1 = class sealed external 'android.support.v4.hardware.display' name 'DisplayManagerJellybeanMr1' (JLObject)
  public
    class function getDisplayManager(para1: ACContext): JLObject; static; overload;
    class function getDisplay(para1: JLObject; para2: jint): AVDisplay; static; overload;
    class function getDisplays(para1: JLObject): Arr1AVDisplay; static; overload;
    class function getDisplays(para1: JLObject; para2: JLString): Arr1AVDisplay; static; overload;
  end;

  ASVMTransportController = class abstract external 'android.support.v4.media' name 'TransportController' (JLObject)
  public
    constructor create(); overload;
    procedure registerStateListener(para1: ASVMTransportStateListener); overload; virtual; abstract;
    procedure unregisterStateListener(para1: ASVMTransportStateListener); overload; virtual; abstract;
    procedure startPlaying(); overload; virtual; abstract;
    procedure pausePlaying(); overload; virtual; abstract;
    procedure stopPlaying(); overload; virtual; abstract;
    function getDuration(): jlong; overload; virtual; abstract;
    function getCurrentPosition(): jlong; overload; virtual; abstract;
    procedure seekTo(para1: jlong); overload; virtual; abstract;
    function isPlaying(): jboolean; overload; virtual; abstract;
    function getBufferPercentage(): jint; overload; virtual; abstract;
    function getTransportControlFlags(): jint; overload; virtual; abstract;
  end;

  ASVMTransportMediatorCallback = interface external 'android.support.v4.media' name 'TransportMediatorCallback' 
    procedure handleKey(para1: AVKeyEvent); overload;
    procedure handleAudioFocusChange(para1: jint); overload;
    function getPlaybackPosition(): jlong; overload;
    procedure playbackPositionUpdate(para1: jlong); overload;
  end;

  ASVMTransportPerformer = class abstract external 'android.support.v4.media' name 'TransportPerformer' (JLObject)
  public
    constructor create(); overload;
    procedure onStart(); overload; virtual; abstract;
    procedure onPause(); overload; virtual; abstract;
    procedure onStop(); overload; virtual; abstract;
    function onGetDuration(): jlong; overload; virtual; abstract;
    function onGetCurrentPosition(): jlong; overload; virtual; abstract;
    procedure onSeekTo(para1: jlong); overload; virtual; abstract;
    function onIsPlaying(): jboolean; overload; virtual; abstract;
    function onGetBufferPercentage(): jint; overload; virtual;
    function onGetTransportControlFlags(): jint; overload; virtual;
    function onMediaButtonDown(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    function onMediaButtonUp(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    procedure onAudioFocusChange(para1: jint); overload; virtual;
  end;

  ASVMTransportStateListener = class external 'android.support.v4.media' name 'TransportStateListener' (JLObject)
  public
    constructor create(); overload;
    procedure onPlayingChanged(para1: ASVMTransportController); overload; virtual;
    procedure onTransportControlsChanged(para1: ASVMTransportController); overload; virtual;
  end;

  ASVNConnectivityManagerCompat = class external 'android.support.v4.net' name 'ConnectivityManagerCompat' (JLObject)
  public
    type
      InnerConnectivityManagerCompatImpl = interface;
      Arr1InnerConnectivityManagerCompatImpl = array of InnerConnectivityManagerCompatImpl;
      Arr2InnerConnectivityManagerCompatImpl = array of Arr1InnerConnectivityManagerCompatImpl;
      Arr3InnerConnectivityManagerCompatImpl = array of Arr2InnerConnectivityManagerCompatImpl;
      InnerJellyBeanConnectivityManagerCompatImpl = class;
      Arr1InnerJellyBeanConnectivityManagerCompatImpl = array of InnerJellyBeanConnectivityManagerCompatImpl;
      Arr2InnerJellyBeanConnectivityManagerCompatImpl = array of Arr1InnerJellyBeanConnectivityManagerCompatImpl;
      Arr3InnerJellyBeanConnectivityManagerCompatImpl = array of Arr2InnerJellyBeanConnectivityManagerCompatImpl;
      InnerHoneycombMR2ConnectivityManagerCompatImpl = class;
      Arr1InnerHoneycombMR2ConnectivityManagerCompatImpl = array of InnerHoneycombMR2ConnectivityManagerCompatImpl;
      Arr2InnerHoneycombMR2ConnectivityManagerCompatImpl = array of Arr1InnerHoneycombMR2ConnectivityManagerCompatImpl;
      Arr3InnerHoneycombMR2ConnectivityManagerCompatImpl = array of Arr2InnerHoneycombMR2ConnectivityManagerCompatImpl;
      InnerGingerbreadConnectivityManagerCompatImpl = class;
      Arr1InnerGingerbreadConnectivityManagerCompatImpl = array of InnerGingerbreadConnectivityManagerCompatImpl;
      Arr2InnerGingerbreadConnectivityManagerCompatImpl = array of Arr1InnerGingerbreadConnectivityManagerCompatImpl;
      Arr3InnerGingerbreadConnectivityManagerCompatImpl = array of Arr2InnerGingerbreadConnectivityManagerCompatImpl;
      InnerBaseConnectivityManagerCompatImpl = class;
      Arr1InnerBaseConnectivityManagerCompatImpl = array of InnerBaseConnectivityManagerCompatImpl;
      Arr2InnerBaseConnectivityManagerCompatImpl = array of Arr1InnerBaseConnectivityManagerCompatImpl;
      Arr3InnerBaseConnectivityManagerCompatImpl = array of Arr2InnerBaseConnectivityManagerCompatImpl;
      InnerConnectivityManagerCompatImpl = interface external 'android.support.v4.net' name 'ConnectivityManagerCompatImpl' 
      end;

      InnerJellyBeanConnectivityManagerCompatImpl = class external 'android.support.v4.net' name 'JellyBeanConnectivityManagerCompatImpl' 
      end;

      InnerHoneycombMR2ConnectivityManagerCompatImpl = class external 'android.support.v4.net' name 'HoneycombMR2ConnectivityManagerCompatImpl' 
      end;

      InnerGingerbreadConnectivityManagerCompatImpl = class external 'android.support.v4.net' name 'GingerbreadConnectivityManagerCompatImpl' 
      end;

      InnerBaseConnectivityManagerCompatImpl = class external 'android.support.v4.net' name 'BaseConnectivityManagerCompatImpl' 
      end;

  public
    constructor create(); overload;
    class function isActiveNetworkMetered(para1: ANConnectivityManager): jboolean; static; overload;
    class function getNetworkInfoFromBroadcast(para1: ANConnectivityManager; para2: ACIntent): ANNetworkInfo; static; overload;
  end;

  ASVNConnectivityManagerCompatGingerbread = class external 'android.support.v4.net' name 'ConnectivityManagerCompatGingerbread' (JLObject)
  public
    class function isActiveNetworkMetered(para1: ANConnectivityManager): jboolean; static; overload;
  end;

  ASVNConnectivityManagerCompatHoneycombMR2 = class external 'android.support.v4.net' name 'ConnectivityManagerCompatHoneycombMR2' (JLObject)
  public
    class function isActiveNetworkMetered(para1: ANConnectivityManager): jboolean; static; overload;
  end;

  ASVNConnectivityManagerCompatJellyBean = class external 'android.support.v4.net' name 'ConnectivityManagerCompatJellyBean' (JLObject)
  public
    class function isActiveNetworkMetered(para1: ANConnectivityManager): jboolean; static; overload;
  end;

  ASVNTrafficStatsCompat = class external 'android.support.v4.net' name 'TrafficStatsCompat' (JLObject)
  public
    type
      InnerTrafficStatsCompatImpl = interface;
      Arr1InnerTrafficStatsCompatImpl = array of InnerTrafficStatsCompatImpl;
      Arr2InnerTrafficStatsCompatImpl = array of Arr1InnerTrafficStatsCompatImpl;
      Arr3InnerTrafficStatsCompatImpl = array of Arr2InnerTrafficStatsCompatImpl;
      InnerIcsTrafficStatsCompatImpl = class;
      Arr1InnerIcsTrafficStatsCompatImpl = array of InnerIcsTrafficStatsCompatImpl;
      Arr2InnerIcsTrafficStatsCompatImpl = array of Arr1InnerIcsTrafficStatsCompatImpl;
      Arr3InnerIcsTrafficStatsCompatImpl = array of Arr2InnerIcsTrafficStatsCompatImpl;
      InnerBaseTrafficStatsCompatImpl = class;
      Arr1InnerBaseTrafficStatsCompatImpl = array of InnerBaseTrafficStatsCompatImpl;
      Arr2InnerBaseTrafficStatsCompatImpl = array of Arr1InnerBaseTrafficStatsCompatImpl;
      Arr3InnerBaseTrafficStatsCompatImpl = array of Arr2InnerBaseTrafficStatsCompatImpl;
      InnerTrafficStatsCompatImpl = interface external 'android.support.v4.net' name 'TrafficStatsCompatImpl' 
      end;

      InnerIcsTrafficStatsCompatImpl = class external 'android.support.v4.net' name 'IcsTrafficStatsCompatImpl' 
      end;

      InnerBaseTrafficStatsCompatImpl = class external 'android.support.v4.net' name 'BaseTrafficStatsCompatImpl' 
      public
        type
          InnerSocketTags = class;
          Arr1InnerSocketTags = array of InnerSocketTags;
          Arr2InnerSocketTags = array of Arr1InnerSocketTags;
          Arr3InnerSocketTags = array of Arr2InnerSocketTags;
          InnerSocketTags = class external 'android.support.v4.net' name 'SocketTags' 
          end;

      end;

  public
    constructor create(); overload;
    class procedure clearThreadStatsTag(); static; overload;
    class function getThreadStatsTag(): jint; static; overload;
    class procedure incrementOperationCount(para1: jint); static; overload;
    class procedure incrementOperationCount(para1: jint; para2: jint); static; overload;
    class procedure setThreadStatsTag(para1: jint); static; overload;
    class procedure tagSocket(para1: JNSocket); static; overload;  // throws java.net.SocketException
    class procedure untagSocket(para1: JNSocket); static; overload;  // throws java.net.SocketException
  end;

  ASVNTrafficStatsCompatIcs = class external 'android.support.v4.net' name 'TrafficStatsCompatIcs' (JLObject)
  public
    class procedure clearThreadStatsTag(); static; overload;
    class function getThreadStatsTag(): jint; static; overload;
    class procedure incrementOperationCount(para1: jint); static; overload;
    class procedure incrementOperationCount(para1: jint; para2: jint); static; overload;
    class procedure setThreadStatsTag(para1: jint); static; overload;
    class procedure tagSocket(para1: JNSocket); static; overload;  // throws java.net.SocketException
    class procedure untagSocket(para1: JNSocket); static; overload;  // throws java.net.SocketException
  end;

  ASVOParcelableCompatCreatorCallbacks = interface external 'android.support.v4.os' name 'ParcelableCompatCreatorCallbacks' 
    function createFromParcel(para1: AOParcel; para2: JLClassLoader): JLObject; overload;
    function newArray(para1: jint): Arr1JLObject; overload;
  end;

  ASVOParcelableCompatCreatorHoneycombMR2Stub = class external 'android.support.v4.os' name 'ParcelableCompatCreatorHoneycombMR2Stub' (JLObject)
  end;

  ASVTBidiFormatter = class sealed external 'android.support.v4.text' name 'BidiFormatter' (JLObject)
  public
    type
      InnerDirectionalityEstimator = class;
      Arr1InnerDirectionalityEstimator = array of InnerDirectionalityEstimator;
      Arr2InnerDirectionalityEstimator = array of Arr1InnerDirectionalityEstimator;
      Arr3InnerDirectionalityEstimator = array of Arr2InnerDirectionalityEstimator;
      InnerDirectionalityEstimator = class external 'android.support.v4.text' name 'DirectionalityEstimator' 
      end;

  public
    type
      InnerBuilder = class;
      Arr1InnerBuilder = array of InnerBuilder;
      Arr2InnerBuilder = array of Arr1InnerBuilder;
      Arr3InnerBuilder = array of Arr2InnerBuilder;
      InnerBuilder = class sealed external 'android.support.v4.text' name 'Builder' (JLObject)
      public
        constructor create(); overload;
        constructor create(para1: jboolean); overload;
        constructor create(para1: JULocale); overload;
        function stereoReset(para1: jboolean): ASVTBidiFormatter.InnerBuilder; overload; virtual;
        function setTextDirectionHeuristic(para1: ASVTTextDirectionHeuristicCompat): ASVTBidiFormatter.InnerBuilder; overload; virtual;
        function build(): ASVTBidiFormatter; overload; virtual;
      end;

  public
    class function getInstance(): ASVTBidiFormatter; static; overload;
    class function getInstance(para1: jboolean): ASVTBidiFormatter; static; overload;
    class function getInstance(para1: JULocale): ASVTBidiFormatter; static; overload;
    function isRtlContext(): jboolean; overload; virtual;
    function getStereoReset(): jboolean; overload; virtual;
    function isRtl(para1: JLString): jboolean; overload; virtual;
    function unicodeWrap(para1: JLString; para2: ASVTTextDirectionHeuristicCompat; para3: jboolean): JLString; overload; virtual;
    function unicodeWrap(para1: JLString; para2: ASVTTextDirectionHeuristicCompat): JLString; overload; virtual;
    function unicodeWrap(para1: JLString; para2: jboolean): JLString; overload; virtual;
    function unicodeWrap(para1: JLString): JLString; overload; virtual;
  end;

  ASVTICUCompat = class external 'android.support.v4.text' name 'ICUCompat' (JLObject)
  public
    type
      InnerICUCompatImplIcs = class;
      Arr1InnerICUCompatImplIcs = array of InnerICUCompatImplIcs;
      Arr2InnerICUCompatImplIcs = array of Arr1InnerICUCompatImplIcs;
      Arr3InnerICUCompatImplIcs = array of Arr2InnerICUCompatImplIcs;
      InnerICUCompatImplBase = class;
      Arr1InnerICUCompatImplBase = array of InnerICUCompatImplBase;
      Arr2InnerICUCompatImplBase = array of Arr1InnerICUCompatImplBase;
      Arr3InnerICUCompatImplBase = array of Arr2InnerICUCompatImplBase;
      InnerICUCompatImpl = interface;
      Arr1InnerICUCompatImpl = array of InnerICUCompatImpl;
      Arr2InnerICUCompatImpl = array of Arr1InnerICUCompatImpl;
      Arr3InnerICUCompatImpl = array of Arr2InnerICUCompatImpl;
      InnerICUCompatImplIcs = class external 'android.support.v4.text' name 'ICUCompatImplIcs' 
      end;

      InnerICUCompatImplBase = class external 'android.support.v4.text' name 'ICUCompatImplBase' 
      end;

      InnerICUCompatImpl = interface external 'android.support.v4.text' name 'ICUCompatImpl' 
      end;

  public
    constructor create(); overload;
    class function getScript(para1: JLString): JLString; static; overload;
    class function addLikelySubtags(para1: JLString): JLString; static; overload;
  end;

  ASVTICUCompatIcs = class external 'android.support.v4.text' name 'ICUCompatIcs' (JLObject)
  public
    class function getScript(para1: JLString): JLString; static; overload;
    class function addLikelySubtags(para1: JLString): JLString; static; overload;
  end;

  ASVTTextDirectionHeuristicCompat = interface external 'android.support.v4.text' name 'TextDirectionHeuristicCompat' 
    function isRtl(para1: Arr1jchar; para2: jint; para3: jint): jboolean; overload;
    function isRtl(var para1: array of jchar; para2: jint; para3: jint): jboolean; overload;
    function isRtl(para1: JLCharSequence; para2: jint; para3: jint): jboolean; overload;
  end;

  ASVTTextDirectionHeuristicsCompat = class external 'android.support.v4.text' name 'TextDirectionHeuristicsCompat' (JLObject)
  public
    type
      InnerTextDirectionHeuristicLocale = class;
      Arr1InnerTextDirectionHeuristicLocale = array of InnerTextDirectionHeuristicLocale;
      Arr2InnerTextDirectionHeuristicLocale = array of Arr1InnerTextDirectionHeuristicLocale;
      Arr3InnerTextDirectionHeuristicLocale = array of Arr2InnerTextDirectionHeuristicLocale;
      InnerTextDirectionHeuristicInternal = class;
      Arr1InnerTextDirectionHeuristicInternal = array of InnerTextDirectionHeuristicInternal;
      Arr2InnerTextDirectionHeuristicInternal = array of Arr1InnerTextDirectionHeuristicInternal;
      Arr3InnerTextDirectionHeuristicInternal = array of Arr2InnerTextDirectionHeuristicInternal;
      InnerAnyStrong = class;
      Arr1InnerAnyStrong = array of InnerAnyStrong;
      Arr2InnerAnyStrong = array of Arr1InnerAnyStrong;
      Arr3InnerAnyStrong = array of Arr2InnerAnyStrong;
      InnerFirstStrong = class;
      Arr1InnerFirstStrong = array of InnerFirstStrong;
      Arr2InnerFirstStrong = array of Arr1InnerFirstStrong;
      Arr3InnerFirstStrong = array of Arr2InnerFirstStrong;
      InnerTextDirectionAlgorithm = interface;
      Arr1InnerTextDirectionAlgorithm = array of InnerTextDirectionAlgorithm;
      Arr2InnerTextDirectionAlgorithm = array of Arr1InnerTextDirectionAlgorithm;
      Arr3InnerTextDirectionAlgorithm = array of Arr2InnerTextDirectionAlgorithm;
      InnerTextDirectionHeuristicImpl = class;
      Arr1InnerTextDirectionHeuristicImpl = array of InnerTextDirectionHeuristicImpl;
      Arr2InnerTextDirectionHeuristicImpl = array of Arr1InnerTextDirectionHeuristicImpl;
      Arr3InnerTextDirectionHeuristicImpl = array of Arr2InnerTextDirectionHeuristicImpl;
      InnerTextDirectionHeuristicLocale = class external 'android.support.v4.text' name 'TextDirectionHeuristicLocale' 
      end;

      InnerTextDirectionHeuristicInternal = class external 'android.support.v4.text' name 'TextDirectionHeuristicInternal' 
      end;

      InnerAnyStrong = class external 'android.support.v4.text' name 'AnyStrong' 
      end;

      InnerFirstStrong = class external 'android.support.v4.text' name 'FirstStrong' 
      end;

      InnerTextDirectionAlgorithm = interface external 'android.support.v4.text' name 'TextDirectionAlgorithm' 
      end;

      InnerTextDirectionHeuristicImpl = class abstract external 'android.support.v4.text' name 'TextDirectionHeuristicImpl' 
      end;

  public
    final class var
      fLTR: ASVTTextDirectionHeuristicCompat; external name 'LTR';
      fRTL: ASVTTextDirectionHeuristicCompat; external name 'RTL';
      fFIRSTSTRONG_LTR: ASVTTextDirectionHeuristicCompat; external name 'FIRSTSTRONG_LTR';
      fFIRSTSTRONG_RTL: ASVTTextDirectionHeuristicCompat; external name 'FIRSTSTRONG_RTL';
      fANYRTL_LTR: ASVTTextDirectionHeuristicCompat; external name 'ANYRTL_LTR';
      fLOCALE: ASVTTextDirectionHeuristicCompat; external name 'LOCALE';
  public
    constructor create(); overload;
  end;

  ASVTTextUtilsCompat = class external 'android.support.v4.text' name 'TextUtilsCompat' (JLObject)
  public
    final class var
      fROOT: JULocale; external name 'ROOT';
  public
    constructor create(); overload;
    class function htmlEncode(para1: JLString): JLString; static; overload;
    class function getLayoutDirectionFromLocale(para1: JULocale): jint; static; overload;
  end;

  ASVUAtomicFile = class external 'android.support.v4.util' name 'AtomicFile' (JLObject)
  public
    constructor create(para1: JIFile); overload;
    function getBaseFile(): JIFile; overload; virtual;
    procedure delete(); overload; virtual;
    function startWrite(): JIFileOutputStream; overload; virtual;  // throws java.io.IOException
    procedure finishWrite(para1: JIFileOutputStream); overload; virtual;
    procedure failWrite(para1: JIFileOutputStream); overload; virtual;
    function openRead(): JIFileInputStream; overload; virtual;  // throws java.io.FileNotFoundException
    function readFully(): Arr1jbyte; overload; virtual;  // throws java.io.IOException
  end;

  ASVUDebugUtils = class external 'android.support.v4.util' name 'DebugUtils' (JLObject)
  public
    constructor create(); overload;
    class procedure buildShortClassTag(para1: JLObject; para2: JLStringBuilder); static; overload;
  end;

  ASVULruCache = class external 'android.support.v4.util' name 'LruCache' (JLObject)
  public
    constructor create(para1: jint); overload;
    function get(para1: JLObject): JLObject; overload; virtual; final;
    function put(para1: JLObject; para2: JLObject): JLObject; overload; virtual; final;
    procedure trimToSize(para1: jint); overload; virtual;
    function remove(para1: JLObject): JLObject; overload; virtual; final;
  strict protected
    procedure entryRemoved(para1: jboolean; para2: JLObject; para3: JLObject; para4: JLObject); overload; virtual;
    function create_(para1: JLObject): JLObject; external name 'create'; overload; virtual;
    function sizeOf(para1: JLObject; para2: JLObject): jint; overload; virtual;
  public
    procedure evictAll(); overload; virtual; final;
    function size(): jint; overload; virtual; final;
    function maxSize(): jint; overload; virtual; final;
    function hitCount(): jint; overload; virtual; final;
    function missCount(): jint; overload; virtual; final;
    function createCount(): jint; overload; virtual; final;
    function putCount(): jint; overload; virtual; final;
    function evictionCount(): jint; overload; virtual; final;
    function snapshot(): JUMap; overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
  end;

  ASVUSparseArrayCompat = class external 'android.support.v4.util' name 'SparseArrayCompat' (JLObject)
  public
    constructor create(); overload;
    constructor create(para1: jint); overload;
    function get(para1: jint): JLObject; overload; virtual;
    function get(para1: jint; para2: JLObject): JLObject; overload; virtual;
    procedure delete(para1: jint); overload; virtual;
    procedure remove(para1: jint); overload; virtual;
    procedure removeAt(para1: jint); overload; virtual;
    procedure removeAtRange(para1: jint; para2: jint); overload; virtual;
    procedure put(para1: jint; para2: JLObject); overload; virtual;
    function size(): jint; overload; virtual;
    function keyAt(para1: jint): jint; overload; virtual;
    function valueAt(para1: jint): JLObject; overload; virtual;
    procedure setValueAt(para1: jint; para2: JLObject); overload; virtual;
    function indexOfKey(para1: jint): jint; overload; virtual;
    function indexOfValue(para1: JLObject): jint; overload; virtual;
    procedure clear(); overload; virtual;
    procedure append(para1: jint; para2: JLObject); overload; virtual;
  end;

  ASVUTimeUtils = class external 'android.support.v4.util' name 'TimeUtils' (JLObject)
  public
    const
      HUNDRED_DAY_FIELD_LEN = 19;
  public
    constructor create(); overload;
    class procedure formatDuration(para1: jlong; para2: JLStringBuilder); static; overload;
    class procedure formatDuration(para1: jlong; para2: JIPrintWriter; para3: jint); static; overload;
    class procedure formatDuration(para1: jlong; para2: JIPrintWriter); static; overload;
    class procedure formatDuration(para1: jlong; para2: jlong; para3: JIPrintWriter); static; overload;
  end;

  ASVVAccessibilityDelegateCompat = class external 'android.support.v4.view' name 'AccessibilityDelegateCompat' (JLObject)
  public
    type
      InnerAccessibilityDelegateJellyBeanImpl = class;
      Arr1InnerAccessibilityDelegateJellyBeanImpl = array of InnerAccessibilityDelegateJellyBeanImpl;
      Arr2InnerAccessibilityDelegateJellyBeanImpl = array of Arr1InnerAccessibilityDelegateJellyBeanImpl;
      Arr3InnerAccessibilityDelegateJellyBeanImpl = array of Arr2InnerAccessibilityDelegateJellyBeanImpl;
      InnerAccessibilityDelegateIcsImpl = class;
      Arr1InnerAccessibilityDelegateIcsImpl = array of InnerAccessibilityDelegateIcsImpl;
      Arr2InnerAccessibilityDelegateIcsImpl = array of Arr1InnerAccessibilityDelegateIcsImpl;
      Arr3InnerAccessibilityDelegateIcsImpl = array of Arr2InnerAccessibilityDelegateIcsImpl;
      InnerAccessibilityDelegateStubImpl = class;
      Arr1InnerAccessibilityDelegateStubImpl = array of InnerAccessibilityDelegateStubImpl;
      Arr2InnerAccessibilityDelegateStubImpl = array of Arr1InnerAccessibilityDelegateStubImpl;
      Arr3InnerAccessibilityDelegateStubImpl = array of Arr2InnerAccessibilityDelegateStubImpl;
      InnerAccessibilityDelegateImpl = interface;
      Arr1InnerAccessibilityDelegateImpl = array of InnerAccessibilityDelegateImpl;
      Arr2InnerAccessibilityDelegateImpl = array of Arr1InnerAccessibilityDelegateImpl;
      Arr3InnerAccessibilityDelegateImpl = array of Arr2InnerAccessibilityDelegateImpl;
      InnerAccessibilityDelegateJellyBeanImpl = class external 'android.support.v4.view' name 'AccessibilityDelegateJellyBeanImpl' 
      end;

      InnerAccessibilityDelegateIcsImpl = class external 'android.support.v4.view' name 'AccessibilityDelegateIcsImpl' 
      end;

      InnerAccessibilityDelegateStubImpl = class external 'android.support.v4.view' name 'AccessibilityDelegateStubImpl' 
      end;

      InnerAccessibilityDelegateImpl = interface external 'android.support.v4.view' name 'AccessibilityDelegateImpl' 
      end;

  public
    constructor create(); overload;
    procedure sendAccessibilityEvent(para1: AVView; para2: jint); overload; virtual;
    procedure sendAccessibilityEventUnchecked(para1: AVView; para2: AVAAccessibilityEvent); overload; virtual;
    function dispatchPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent): jboolean; overload; virtual;
    procedure onPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); overload; virtual;
    procedure onInitializeAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); overload; virtual;
    procedure onInitializeAccessibilityNodeInfo(para1: AVView; para2: ASVVAAccessibilityNodeInfoCompat); overload; virtual;
    function onRequestSendAccessibilityEvent(para1: AVViewGroup; para2: AVView; para3: AVAAccessibilityEvent): jboolean; overload; virtual;
    function getAccessibilityNodeProvider(para1: AVView): ASVVAAccessibilityNodeProviderCompat; overload; virtual;
    function performAccessibilityAction(para1: AVView; para2: jint; para3: AOBundle): jboolean; overload; virtual;
  end;

  ASVVAccessibilityDelegateCompatIcs = class external 'android.support.v4.view' name 'AccessibilityDelegateCompatIcs' (JLObject)
  public
    type
      InnerAccessibilityDelegateBridge = interface;
      Arr1InnerAccessibilityDelegateBridge = array of InnerAccessibilityDelegateBridge;
      Arr2InnerAccessibilityDelegateBridge = array of Arr1InnerAccessibilityDelegateBridge;
      Arr3InnerAccessibilityDelegateBridge = array of Arr2InnerAccessibilityDelegateBridge;
      InnerAccessibilityDelegateBridge = interface external 'android.support.v4.view' name 'AccessibilityDelegateBridge' 
        function dispatchPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent): jboolean; overload;
        procedure onInitializeAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); overload;
        procedure onInitializeAccessibilityNodeInfo(para1: AVView; para2: JLObject); overload;
        procedure onPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); overload;
        function onRequestSendAccessibilityEvent(para1: AVViewGroup; para2: AVView; para3: AVAAccessibilityEvent): jboolean; overload;
        procedure sendAccessibilityEvent(para1: AVView; para2: jint); overload;
        procedure sendAccessibilityEventUnchecked(para1: AVView; para2: AVAAccessibilityEvent); overload;
      end;

  public
    class function newAccessibilityDelegateDefaultImpl(): JLObject; static; overload;
    class function newAccessibilityDelegateBridge(para1: ASVVAccessibilityDelegateCompatIcs.InnerAccessibilityDelegateBridge): JLObject; static; overload;
    class function dispatchPopulateAccessibilityEvent(para1: JLObject; para2: AVView; para3: AVAAccessibilityEvent): jboolean; static; overload;
    class procedure onInitializeAccessibilityEvent(para1: JLObject; para2: AVView; para3: AVAAccessibilityEvent); static; overload;
    class procedure onInitializeAccessibilityNodeInfo(para1: JLObject; para2: AVView; para3: JLObject); static; overload;
    class procedure onPopulateAccessibilityEvent(para1: JLObject; para2: AVView; para3: AVAAccessibilityEvent); static; overload;
    class function onRequestSendAccessibilityEvent(para1: JLObject; para2: AVViewGroup; para3: AVView; para4: AVAAccessibilityEvent): jboolean; static; overload;
    class procedure sendAccessibilityEvent(para1: JLObject; para2: AVView; para3: jint); static; overload;
    class procedure sendAccessibilityEventUnchecked(para1: JLObject; para2: AVView; para3: AVAAccessibilityEvent); static; overload;
  end;

  ASVVAccessibilityDelegateCompatJellyBean = class external 'android.support.v4.view' name 'AccessibilityDelegateCompatJellyBean' (JLObject)
  public
    type
      InnerAccessibilityDelegateBridgeJellyBean = interface;
      Arr1InnerAccessibilityDelegateBridgeJellyBean = array of InnerAccessibilityDelegateBridgeJellyBean;
      Arr2InnerAccessibilityDelegateBridgeJellyBean = array of Arr1InnerAccessibilityDelegateBridgeJellyBean;
      Arr3InnerAccessibilityDelegateBridgeJellyBean = array of Arr2InnerAccessibilityDelegateBridgeJellyBean;
      InnerAccessibilityDelegateBridgeJellyBean = interface external 'android.support.v4.view' name 'AccessibilityDelegateBridgeJellyBean' 
        function dispatchPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent): jboolean; overload;
        procedure onInitializeAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); overload;
        procedure onInitializeAccessibilityNodeInfo(para1: AVView; para2: JLObject); overload;
        procedure onPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); overload;
        function onRequestSendAccessibilityEvent(para1: AVViewGroup; para2: AVView; para3: AVAAccessibilityEvent): jboolean; overload;
        procedure sendAccessibilityEvent(para1: AVView; para2: jint); overload;
        procedure sendAccessibilityEventUnchecked(para1: AVView; para2: AVAAccessibilityEvent); overload;
        function getAccessibilityNodeProvider(para1: AVView): JLObject; overload;
        function performAccessibilityAction(para1: AVView; para2: jint; para3: AOBundle): jboolean; overload;
      end;

  public
    class function newAccessibilityDelegateBridge(para1: ASVVAccessibilityDelegateCompatJellyBean.InnerAccessibilityDelegateBridgeJellyBean): JLObject; static; overload;
    class function getAccessibilityNodeProvider(para1: JLObject; para2: AVView): JLObject; static; overload;
    class function performAccessibilityAction(para1: JLObject; para2: AVView; para3: jint; para4: AOBundle): jboolean; static; overload;
  end;

  ASVVActionProvider = class abstract external 'android.support.v4.view' name 'ActionProvider' (JLObject)
  public
    type
      InnerVisibilityListener = interface;
      Arr1InnerVisibilityListener = array of InnerVisibilityListener;
      Arr2InnerVisibilityListener = array of Arr1InnerVisibilityListener;
      Arr3InnerVisibilityListener = array of Arr2InnerVisibilityListener;
      InnerSubUiVisibilityListener = interface;
      Arr1InnerSubUiVisibilityListener = array of InnerSubUiVisibilityListener;
      Arr2InnerSubUiVisibilityListener = array of Arr1InnerSubUiVisibilityListener;
      Arr3InnerSubUiVisibilityListener = array of Arr2InnerSubUiVisibilityListener;
      InnerVisibilityListener = interface external 'android.support.v4.view' name 'VisibilityListener' 
        procedure onActionProviderVisibilityChanged(para1: jboolean); overload;
      end;

      InnerSubUiVisibilityListener = interface external 'android.support.v4.view' name 'SubUiVisibilityListener' 
        procedure onSubUiVisibilityChanged(para1: jboolean); overload;
      end;

  public
    constructor create(para1: ACContext); overload;
    function getContext(): ACContext; overload; virtual;
    function onCreateActionView(): AVView; overload; virtual; abstract;
    function onCreateActionView(para1: AVMenuItem): AVView; overload; virtual;
    function overridesItemVisibility(): jboolean; overload; virtual;
    function isVisible(): jboolean; overload; virtual;
    procedure refreshVisibility(); overload; virtual;
    function onPerformDefaultAction(): jboolean; overload; virtual;
    function hasSubMenu(): jboolean; overload; virtual;
    procedure onPrepareSubMenu(para1: AVSubMenu); overload; virtual;
    procedure subUiVisibilityChanged(para1: jboolean); overload; virtual;
    procedure setSubUiVisibilityListener(para1: ASVVActionProvider.InnerSubUiVisibilityListener); overload; virtual;
    procedure setVisibilityListener(para1: ASVVActionProvider.InnerVisibilityListener); overload; virtual;
  end;

  ASVVGravityCompat = class external 'android.support.v4.view' name 'GravityCompat' (JLObject)
  public
    type
      InnerGravityCompatImpl = interface;
      Arr1InnerGravityCompatImpl = array of InnerGravityCompatImpl;
      Arr2InnerGravityCompatImpl = array of Arr1InnerGravityCompatImpl;
      Arr3InnerGravityCompatImpl = array of Arr2InnerGravityCompatImpl;
      InnerGravityCompatImplJellybeanMr1 = class;
      Arr1InnerGravityCompatImplJellybeanMr1 = array of InnerGravityCompatImplJellybeanMr1;
      Arr2InnerGravityCompatImplJellybeanMr1 = array of Arr1InnerGravityCompatImplJellybeanMr1;
      Arr3InnerGravityCompatImplJellybeanMr1 = array of Arr2InnerGravityCompatImplJellybeanMr1;
      InnerGravityCompatImplBase = class;
      Arr1InnerGravityCompatImplBase = array of InnerGravityCompatImplBase;
      Arr2InnerGravityCompatImplBase = array of Arr1InnerGravityCompatImplBase;
      Arr3InnerGravityCompatImplBase = array of Arr2InnerGravityCompatImplBase;
      InnerGravityCompatImpl = interface external 'android.support.v4.view' name 'GravityCompatImpl' 
      end;

      InnerGravityCompatImplJellybeanMr1 = class external 'android.support.v4.view' name 'GravityCompatImplJellybeanMr1' 
      end;

      InnerGravityCompatImplBase = class external 'android.support.v4.view' name 'GravityCompatImplBase' 
      end;

  public
    const
      RELATIVE_LAYOUT_DIRECTION = 8388608;
      START = 8388611;
      &END = 8388613;
      RELATIVE_HORIZONTAL_GRAVITY_MASK = 8388615;
  public
    constructor create(); overload;
    class procedure apply(para1: jint; para2: jint; para3: jint; para4: AGRect; para5: AGRect; para6: jint); static; overload;
    class procedure apply(para1: jint; para2: jint; para3: jint; para4: AGRect; para5: jint; para6: jint; para7: AGRect; para8: jint); static; overload;
    class procedure applyDisplay(para1: jint; para2: AGRect; para3: AGRect; para4: jint); static; overload;
    class function getAbsoluteGravity(para1: jint; para2: jint): jint; static; overload;
  end;

  ASVVGravityCompatJellybeanMr1 = class external 'android.support.v4.view' name 'GravityCompatJellybeanMr1' (JLObject)
  public
    class function getAbsoluteGravity(para1: jint; para2: jint): jint; static; overload;
    class procedure apply(para1: jint; para2: jint; para3: jint; para4: AGRect; para5: AGRect; para6: jint); static; overload;
    class procedure apply(para1: jint; para2: jint; para3: jint; para4: AGRect; para5: jint; para6: jint; para7: AGRect; para8: jint); static; overload;
    class procedure applyDisplay(para1: jint; para2: AGRect; para3: AGRect; para4: jint); static; overload;
  end;

  ASVVKeyEventCompatHoneycomb = class external 'android.support.v4.view' name 'KeyEventCompatHoneycomb' (JLObject)
  public
    class function normalizeMetaState(para1: jint): jint; static; overload;
    class function metaStateHasModifiers(para1: jint; para2: jint): jboolean; static; overload;
    class function metaStateHasNoModifiers(para1: jint): jboolean; static; overload;
  end;

  ASVVMenuCompat = class external 'android.support.v4.view' name 'MenuCompat' (JLObject)
  public
    constructor create(); overload;
    class procedure setShowAsAction(para1: AVMenuItem; para2: jint); static; overload;
  end;

  ASVVMenuItemCompat = class external 'android.support.v4.view' name 'MenuItemCompat' (JLObject)
  public
    type
      InnerIcsMenuVersionImpl = class;
      Arr1InnerIcsMenuVersionImpl = array of InnerIcsMenuVersionImpl;
      Arr2InnerIcsMenuVersionImpl = array of Arr1InnerIcsMenuVersionImpl;
      Arr3InnerIcsMenuVersionImpl = array of Arr2InnerIcsMenuVersionImpl;
      InnerHoneycombMenuVersionImpl = class;
      Arr1InnerHoneycombMenuVersionImpl = array of InnerHoneycombMenuVersionImpl;
      Arr2InnerHoneycombMenuVersionImpl = array of Arr1InnerHoneycombMenuVersionImpl;
      Arr3InnerHoneycombMenuVersionImpl = array of Arr2InnerHoneycombMenuVersionImpl;
      InnerBaseMenuVersionImpl = class;
      Arr1InnerBaseMenuVersionImpl = array of InnerBaseMenuVersionImpl;
      Arr2InnerBaseMenuVersionImpl = array of Arr1InnerBaseMenuVersionImpl;
      Arr3InnerBaseMenuVersionImpl = array of Arr2InnerBaseMenuVersionImpl;
      InnerMenuVersionImpl = interface;
      Arr1InnerMenuVersionImpl = array of InnerMenuVersionImpl;
      Arr2InnerMenuVersionImpl = array of Arr1InnerMenuVersionImpl;
      Arr3InnerMenuVersionImpl = array of Arr2InnerMenuVersionImpl;
      InnerIcsMenuVersionImpl = class external 'android.support.v4.view' name 'IcsMenuVersionImpl' 
      end;

      InnerHoneycombMenuVersionImpl = class external 'android.support.v4.view' name 'HoneycombMenuVersionImpl' 
      end;

      InnerBaseMenuVersionImpl = class external 'android.support.v4.view' name 'BaseMenuVersionImpl' 
      end;

      InnerMenuVersionImpl = interface external 'android.support.v4.view' name 'MenuVersionImpl' 
      end;

  public
    type
      InnerOnActionExpandListener = interface;
      Arr1InnerOnActionExpandListener = array of InnerOnActionExpandListener;
      Arr2InnerOnActionExpandListener = array of Arr1InnerOnActionExpandListener;
      Arr3InnerOnActionExpandListener = array of Arr2InnerOnActionExpandListener;
      InnerOnActionExpandListener = interface external 'android.support.v4.view' name 'OnActionExpandListener' 
        function onMenuItemActionExpand(para1: AVMenuItem): jboolean; overload;
        function onMenuItemActionCollapse(para1: AVMenuItem): jboolean; overload;
      end;

  public
    const
      SHOW_AS_ACTION_NEVER = 0;
      SHOW_AS_ACTION_IF_ROOM = 1;
      SHOW_AS_ACTION_ALWAYS = 2;
      SHOW_AS_ACTION_WITH_TEXT = 4;
      SHOW_AS_ACTION_COLLAPSE_ACTION_VIEW = 8;
  public
    constructor create(); overload;
    class procedure setShowAsAction(para1: AVMenuItem; para2: jint); static; overload;
    class function setActionView(para1: AVMenuItem; para2: AVView): AVMenuItem; static; overload;
    class function setActionView(para1: AVMenuItem; para2: jint): AVMenuItem; static; overload;
    class function getActionView(para1: AVMenuItem): AVView; static; overload;
    class function setActionProvider(para1: AVMenuItem; para2: ASVVActionProvider): AVMenuItem; static; overload;
    class function getActionProvider(para1: AVMenuItem): ASVVActionProvider; static; overload;
    class function expandActionView(para1: AVMenuItem): jboolean; static; overload;
    class function collapseActionView(para1: AVMenuItem): jboolean; static; overload;
    class function isActionViewExpanded(para1: AVMenuItem): jboolean; static; overload;
    class function setOnActionExpandListener(para1: AVMenuItem; para2: ASVVMenuItemCompat.InnerOnActionExpandListener): AVMenuItem; static; overload;
  end;

  ASVVMenuItemCompatHoneycomb = class external 'android.support.v4.view' name 'MenuItemCompatHoneycomb' (JLObject)
  public
    class procedure setShowAsAction(para1: AVMenuItem; para2: jint); static; overload;
    class function setActionView(para1: AVMenuItem; para2: AVView): AVMenuItem; static; overload;
    class function setActionView(para1: AVMenuItem; para2: jint): AVMenuItem; static; overload;
    class function getActionView(para1: AVMenuItem): AVView; static; overload;
  end;

  ASVVMenuItemCompatIcs = class external 'android.support.v4.view' name 'MenuItemCompatIcs' (JLObject)
  public
    type
      InnerSupportActionExpandProxy = interface;
      Arr1InnerSupportActionExpandProxy = array of InnerSupportActionExpandProxy;
      Arr2InnerSupportActionExpandProxy = array of Arr1InnerSupportActionExpandProxy;
      Arr3InnerSupportActionExpandProxy = array of Arr2InnerSupportActionExpandProxy;
      InnerOnActionExpandListenerWrapper = class;
      Arr1InnerOnActionExpandListenerWrapper = array of InnerOnActionExpandListenerWrapper;
      Arr2InnerOnActionExpandListenerWrapper = array of Arr1InnerOnActionExpandListenerWrapper;
      Arr3InnerOnActionExpandListenerWrapper = array of Arr2InnerOnActionExpandListenerWrapper;
      InnerSupportActionExpandProxy = interface external 'android.support.v4.view' name 'SupportActionExpandProxy' 
      end;

      InnerOnActionExpandListenerWrapper = class external 'android.support.v4.view' name 'OnActionExpandListenerWrapper' 
      end;

  public
    class function expandActionView(para1: AVMenuItem): jboolean; static; overload;
    class function collapseActionView(para1: AVMenuItem): jboolean; static; overload;
    class function isActionViewExpanded(para1: AVMenuItem): jboolean; static; overload;
    class function setOnActionExpandListener(para1: AVMenuItem; para2: ASVVMenuItemCompatIcs.InnerSupportActionExpandProxy): AVMenuItem; static; overload;
  end;

  ASVVMotionEventCompat = class external 'android.support.v4.view' name 'MotionEventCompat' (JLObject)
  public
    type
      InnerMotionEventVersionImpl = interface;
      Arr1InnerMotionEventVersionImpl = array of InnerMotionEventVersionImpl;
      Arr2InnerMotionEventVersionImpl = array of Arr1InnerMotionEventVersionImpl;
      Arr3InnerMotionEventVersionImpl = array of Arr2InnerMotionEventVersionImpl;
      InnerEclairMotionEventVersionImpl = class;
      Arr1InnerEclairMotionEventVersionImpl = array of InnerEclairMotionEventVersionImpl;
      Arr2InnerEclairMotionEventVersionImpl = array of Arr1InnerEclairMotionEventVersionImpl;
      Arr3InnerEclairMotionEventVersionImpl = array of Arr2InnerEclairMotionEventVersionImpl;
      InnerBaseMotionEventVersionImpl = class;
      Arr1InnerBaseMotionEventVersionImpl = array of InnerBaseMotionEventVersionImpl;
      Arr2InnerBaseMotionEventVersionImpl = array of Arr1InnerBaseMotionEventVersionImpl;
      Arr3InnerBaseMotionEventVersionImpl = array of Arr2InnerBaseMotionEventVersionImpl;
      InnerMotionEventVersionImpl = interface external 'android.support.v4.view' name 'MotionEventVersionImpl' 
      end;

      InnerEclairMotionEventVersionImpl = class external 'android.support.v4.view' name 'EclairMotionEventVersionImpl' 
      end;

      InnerBaseMotionEventVersionImpl = class external 'android.support.v4.view' name 'BaseMotionEventVersionImpl' 
      end;

  public
    const
      ACTION_MASK = 255;
      ACTION_POINTER_DOWN = 5;
      ACTION_POINTER_UP = 6;
      ACTION_HOVER_MOVE = 7;
      ACTION_SCROLL = 8;
      ACTION_POINTER_INDEX_MASK = 65280;
      ACTION_POINTER_INDEX_SHIFT = 8;
      ACTION_HOVER_ENTER = 9;
      ACTION_HOVER_EXIT = 10;
  public
    constructor create(); overload;
    class function getActionMasked(para1: AVMotionEvent): jint; static; overload;
    class function getActionIndex(para1: AVMotionEvent): jint; static; overload;
    class function findPointerIndex(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getPointerId(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getX(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
    class function getY(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
    class function getPointerCount(para1: AVMotionEvent): jint; static; overload;
  end;

  ASVVMotionEventCompatEclair = class external 'android.support.v4.view' name 'MotionEventCompatEclair' (JLObject)
  public
    class function findPointerIndex(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getPointerId(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getX(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
    class function getY(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
    class function getPointerCount(para1: AVMotionEvent): jint; static; overload;
  end;

  ASVVPagerAdapter = class abstract external 'android.support.v4.view' name 'PagerAdapter' (JLObject)
  public
    const
      POSITION_UNCHANGED = -1;
      POSITION_NONE = -2;
  public
    constructor create(); overload;
    function getCount(): jint; overload; virtual; abstract;
    procedure startUpdate(para1: AVViewGroup); overload; virtual;
    function instantiateItem(para1: AVViewGroup; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure setPrimaryItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVViewGroup); overload; virtual;
    procedure startUpdate(para1: AVView); overload; virtual;
    function instantiateItem(para1: AVView; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVView; para2: jint; para3: JLObject); overload; virtual;
    procedure setPrimaryItem(para1: AVView; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVView); overload; virtual;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual; abstract;
    function saveState(): AOParcelable; overload; virtual;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual;
    function getItemPosition(para1: JLObject): jint; overload; virtual;
    procedure notifyDataSetChanged(); overload; virtual;
    procedure registerDataSetObserver(para1: ADDataSetObserver); overload; virtual;
    procedure unregisterDataSetObserver(para1: ADDataSetObserver); overload; virtual;
    function getPageTitle(para1: jint): JLCharSequence; overload; virtual;
    function getPageWidth(para1: jint): jfloat; overload; virtual;
  end;

  ASVVPagerTitleStripIcs = class external 'android.support.v4.view' name 'PagerTitleStripIcs' (JLObject)
  public
    type
      InnerSingleLineAllCapsTransform = class;
      Arr1InnerSingleLineAllCapsTransform = array of InnerSingleLineAllCapsTransform;
      Arr2InnerSingleLineAllCapsTransform = array of Arr1InnerSingleLineAllCapsTransform;
      Arr3InnerSingleLineAllCapsTransform = array of Arr2InnerSingleLineAllCapsTransform;
      InnerSingleLineAllCapsTransform = class external 'android.support.v4.view' name 'SingleLineAllCapsTransform' 
      end;

  public
    class procedure setSingleLineAllCaps(para1: AWTextView); static; overload;
  end;

  ASVVVelocityTrackerCompat = class external 'android.support.v4.view' name 'VelocityTrackerCompat' (JLObject)
  public
    type
      InnerVelocityTrackerVersionImpl = interface;
      Arr1InnerVelocityTrackerVersionImpl = array of InnerVelocityTrackerVersionImpl;
      Arr2InnerVelocityTrackerVersionImpl = array of Arr1InnerVelocityTrackerVersionImpl;
      Arr3InnerVelocityTrackerVersionImpl = array of Arr2InnerVelocityTrackerVersionImpl;
      InnerHoneycombVelocityTrackerVersionImpl = class;
      Arr1InnerHoneycombVelocityTrackerVersionImpl = array of InnerHoneycombVelocityTrackerVersionImpl;
      Arr2InnerHoneycombVelocityTrackerVersionImpl = array of Arr1InnerHoneycombVelocityTrackerVersionImpl;
      Arr3InnerHoneycombVelocityTrackerVersionImpl = array of Arr2InnerHoneycombVelocityTrackerVersionImpl;
      InnerBaseVelocityTrackerVersionImpl = class;
      Arr1InnerBaseVelocityTrackerVersionImpl = array of InnerBaseVelocityTrackerVersionImpl;
      Arr2InnerBaseVelocityTrackerVersionImpl = array of Arr1InnerBaseVelocityTrackerVersionImpl;
      Arr3InnerBaseVelocityTrackerVersionImpl = array of Arr2InnerBaseVelocityTrackerVersionImpl;
      InnerVelocityTrackerVersionImpl = interface external 'android.support.v4.view' name 'VelocityTrackerVersionImpl' 
      end;

      InnerHoneycombVelocityTrackerVersionImpl = class external 'android.support.v4.view' name 'HoneycombVelocityTrackerVersionImpl' 
      end;

      InnerBaseVelocityTrackerVersionImpl = class external 'android.support.v4.view' name 'BaseVelocityTrackerVersionImpl' 
      end;

  public
    constructor create(); overload;
    class function getXVelocity(para1: AVVelocityTracker; para2: jint): jfloat; static; overload;
    class function getYVelocity(para1: AVVelocityTracker; para2: jint): jfloat; static; overload;
  end;

  ASVVVelocityTrackerCompatHoneycomb = class external 'android.support.v4.view' name 'VelocityTrackerCompatHoneycomb' (JLObject)
  public
    class function getXVelocity(para1: AVVelocityTracker; para2: jint): jfloat; static; overload;
    class function getYVelocity(para1: AVVelocityTracker; para2: jint): jfloat; static; overload;
  end;

  ASVVViewCompat = class external 'android.support.v4.view' name 'ViewCompat' (JLObject)
  public
    type
      InnerJbMr1ViewCompatImpl = class;
      Arr1InnerJbMr1ViewCompatImpl = array of InnerJbMr1ViewCompatImpl;
      Arr2InnerJbMr1ViewCompatImpl = array of Arr1InnerJbMr1ViewCompatImpl;
      Arr3InnerJbMr1ViewCompatImpl = array of Arr2InnerJbMr1ViewCompatImpl;
      InnerJBViewCompatImpl = class;
      Arr1InnerJBViewCompatImpl = array of InnerJBViewCompatImpl;
      Arr2InnerJBViewCompatImpl = array of Arr1InnerJBViewCompatImpl;
      Arr3InnerJBViewCompatImpl = array of Arr2InnerJBViewCompatImpl;
      InnerICSViewCompatImpl = class;
      Arr1InnerICSViewCompatImpl = array of InnerICSViewCompatImpl;
      Arr2InnerICSViewCompatImpl = array of Arr1InnerICSViewCompatImpl;
      Arr3InnerICSViewCompatImpl = array of Arr2InnerICSViewCompatImpl;
      InnerHCViewCompatImpl = class;
      Arr1InnerHCViewCompatImpl = array of InnerHCViewCompatImpl;
      Arr2InnerHCViewCompatImpl = array of Arr1InnerHCViewCompatImpl;
      Arr3InnerHCViewCompatImpl = array of Arr2InnerHCViewCompatImpl;
      InnerGBViewCompatImpl = class;
      Arr1InnerGBViewCompatImpl = array of InnerGBViewCompatImpl;
      Arr2InnerGBViewCompatImpl = array of Arr1InnerGBViewCompatImpl;
      Arr3InnerGBViewCompatImpl = array of Arr2InnerGBViewCompatImpl;
      InnerEclairMr1ViewCompatImpl = class;
      Arr1InnerEclairMr1ViewCompatImpl = array of InnerEclairMr1ViewCompatImpl;
      Arr2InnerEclairMr1ViewCompatImpl = array of Arr1InnerEclairMr1ViewCompatImpl;
      Arr3InnerEclairMr1ViewCompatImpl = array of Arr2InnerEclairMr1ViewCompatImpl;
      InnerBaseViewCompatImpl = class;
      Arr1InnerBaseViewCompatImpl = array of InnerBaseViewCompatImpl;
      Arr2InnerBaseViewCompatImpl = array of Arr1InnerBaseViewCompatImpl;
      Arr3InnerBaseViewCompatImpl = array of Arr2InnerBaseViewCompatImpl;
      InnerViewCompatImpl = interface;
      Arr1InnerViewCompatImpl = array of InnerViewCompatImpl;
      Arr2InnerViewCompatImpl = array of Arr1InnerViewCompatImpl;
      Arr3InnerViewCompatImpl = array of Arr2InnerViewCompatImpl;
      InnerJbMr1ViewCompatImpl = class external 'android.support.v4.view' name 'JbMr1ViewCompatImpl' 
      end;

      InnerJBViewCompatImpl = class external 'android.support.v4.view' name 'JBViewCompatImpl' 
      end;

      InnerICSViewCompatImpl = class external 'android.support.v4.view' name 'ICSViewCompatImpl' 
      end;

      InnerHCViewCompatImpl = class external 'android.support.v4.view' name 'HCViewCompatImpl' 
      end;

      InnerGBViewCompatImpl = class external 'android.support.v4.view' name 'GBViewCompatImpl' 
      end;

      InnerEclairMr1ViewCompatImpl = class external 'android.support.v4.view' name 'EclairMr1ViewCompatImpl' 
      end;

      InnerBaseViewCompatImpl = class external 'android.support.v4.view' name 'BaseViewCompatImpl' 
      end;

      InnerViewCompatImpl = interface external 'android.support.v4.view' name 'ViewCompatImpl' 
      end;

  public
    const
      OVER_SCROLL_ALWAYS = 0;
      OVER_SCROLL_IF_CONTENT_SCROLLS = 1;
      OVER_SCROLL_NEVER = 2;
      IMPORTANT_FOR_ACCESSIBILITY_AUTO = 0;
      IMPORTANT_FOR_ACCESSIBILITY_YES = 1;
      IMPORTANT_FOR_ACCESSIBILITY_NO = 2;
      LAYER_TYPE_NONE = 0;
      LAYER_TYPE_SOFTWARE = 1;
      LAYER_TYPE_HARDWARE = 2;
      LAYOUT_DIRECTION_LTR = 0;
      LAYOUT_DIRECTION_RTL = 1;
      LAYOUT_DIRECTION_INHERIT = 2;
      LAYOUT_DIRECTION_LOCALE = 3;
  public
    constructor create(); overload;
    class function canScrollHorizontally(para1: AVView; para2: jint): jboolean; static; overload;
    class function canScrollVertically(para1: AVView; para2: jint): jboolean; static; overload;
    class function getOverScrollMode(para1: AVView): jint; static; overload;
    class procedure setOverScrollMode(para1: AVView; para2: jint); static; overload;
    class procedure onPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); static; overload;
    class procedure onInitializeAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); static; overload;
    class procedure onInitializeAccessibilityNodeInfo(para1: AVView; para2: ASVVAAccessibilityNodeInfoCompat); static; overload;
    class procedure setAccessibilityDelegate(para1: AVView; para2: ASVVAccessibilityDelegateCompat); static; overload;
    class function hasTransientState(para1: AVView): jboolean; static; overload;
    class procedure setHasTransientState(para1: AVView; para2: jboolean); static; overload;
    class procedure postInvalidateOnAnimation(para1: AVView); static; overload;
    class procedure postInvalidateOnAnimation(para1: AVView; para2: jint; para3: jint; para4: jint; para5: jint); static; overload;
    class procedure postOnAnimation(para1: AVView; para2: JLRunnable); static; overload;
    class procedure postOnAnimationDelayed(para1: AVView; para2: JLRunnable; para3: jlong); static; overload;
    class function getImportantForAccessibility(para1: AVView): jint; static; overload;
    class procedure setImportantForAccessibility(para1: AVView; para2: jint); static; overload;
    class function performAccessibilityAction(para1: AVView; para2: jint; para3: AOBundle): jboolean; static; overload;
    class function getAccessibilityNodeProvider(para1: AVView): ASVVAAccessibilityNodeProviderCompat; static; overload;
    class function getAlpha(para1: AVView): jfloat; static; overload;
    class procedure setLayerType(para1: AVView; para2: jint; para3: AGPaint); static; overload;
    class function getLayerType(para1: AVView): jint; static; overload;
    class function getLabelFor(para1: AVView): jint; static; overload;
    class procedure setLabelFor(para1: AVView; para2: jint); static; overload;
    class procedure setLayerPaint(para1: AVView; para2: AGPaint); static; overload;
    class function getLayoutDirection(para1: AVView): jint; static; overload;
    class procedure setLayoutDirection(para1: AVView; para2: jint); static; overload;
    class function getParentForAccessibility(para1: AVView): AVViewParent; static; overload;
    class function isOpaque(para1: AVView): jboolean; static; overload;
  end;

  ASVVViewCompatEclairMr1 = class external 'android.support.v4.view' name 'ViewCompatEclairMr1' (JLObject)
  public
    class function isOpaque(para1: AVView): jboolean; static; overload;
  end;

  ASVVViewCompatGingerbread = class external 'android.support.v4.view' name 'ViewCompatGingerbread' (JLObject)
  public
    class function getOverScrollMode(para1: AVView): jint; static; overload;
    class procedure setOverScrollMode(para1: AVView; para2: jint); static; overload;
  end;

  ASVVViewCompatHC = class external 'android.support.v4.view' name 'ViewCompatHC' (JLObject)
  public
    class function getAlpha(para1: AVView): jfloat; static; overload;
    class procedure setLayerType(para1: AVView; para2: jint; para3: AGPaint); static; overload;
    class function getLayerType(para1: AVView): jint; static; overload;
  end;

  ASVVViewCompatICS = class external 'android.support.v4.view' name 'ViewCompatICS' (JLObject)
  public
    class function canScrollHorizontally(para1: AVView; para2: jint): jboolean; static; overload;
    class function canScrollVertically(para1: AVView; para2: jint): jboolean; static; overload;
    class procedure setAccessibilityDelegate(para1: AVView; para2: JLObject); static; overload;
    class procedure onPopulateAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); static; overload;
    class procedure onInitializeAccessibilityEvent(para1: AVView; para2: AVAAccessibilityEvent); static; overload;
    class procedure onInitializeAccessibilityNodeInfo(para1: AVView; para2: JLObject); static; overload;
  end;

  ASVVViewCompatJB = class external 'android.support.v4.view' name 'ViewCompatJB' (JLObject)
  public
    class function hasTransientState(para1: AVView): jboolean; static; overload;
    class procedure setHasTransientState(para1: AVView; para2: jboolean); static; overload;
    class procedure postInvalidateOnAnimation(para1: AVView); static; overload;
    class procedure postInvalidateOnAnimation(para1: AVView; para2: jint; para3: jint; para4: jint; para5: jint); static; overload;
    class procedure postOnAnimation(para1: AVView; para2: JLRunnable); static; overload;
    class procedure postOnAnimationDelayed(para1: AVView; para2: JLRunnable; para3: jlong); static; overload;
    class function getImportantForAccessibility(para1: AVView): jint; static; overload;
    class procedure setImportantForAccessibility(para1: AVView; para2: jint); static; overload;
    class function performAccessibilityAction(para1: AVView; para2: jint; para3: AOBundle): jboolean; static; overload;
    class function getAccessibilityNodeProvider(para1: AVView): JLObject; static; overload;
    class function getParentForAccessibility(para1: AVView): AVViewParent; static; overload;
  end;

  ASVVViewCompatJellybeanMr1 = class external 'android.support.v4.view' name 'ViewCompatJellybeanMr1' (JLObject)
  public
    class function getLabelFor(para1: AVView): jint; static; overload;
    class procedure setLabelFor(para1: AVView; para2: jint); static; overload;
    class procedure setLayerPaint(para1: AVView; para2: AGPaint); static; overload;
    class function getLayoutDirection(para1: AVView): jint; static; overload;
    class procedure setLayoutDirection(para1: AVView; para2: jint); static; overload;
  end;

  ASVVViewConfigurationCompat = class external 'android.support.v4.view' name 'ViewConfigurationCompat' (JLObject)
  public
    type
      InnerFroyoViewConfigurationVersionImpl = class;
      Arr1InnerFroyoViewConfigurationVersionImpl = array of InnerFroyoViewConfigurationVersionImpl;
      Arr2InnerFroyoViewConfigurationVersionImpl = array of Arr1InnerFroyoViewConfigurationVersionImpl;
      Arr3InnerFroyoViewConfigurationVersionImpl = array of Arr2InnerFroyoViewConfigurationVersionImpl;
      InnerBaseViewConfigurationVersionImpl = class;
      Arr1InnerBaseViewConfigurationVersionImpl = array of InnerBaseViewConfigurationVersionImpl;
      Arr2InnerBaseViewConfigurationVersionImpl = array of Arr1InnerBaseViewConfigurationVersionImpl;
      Arr3InnerBaseViewConfigurationVersionImpl = array of Arr2InnerBaseViewConfigurationVersionImpl;
      InnerViewConfigurationVersionImpl = interface;
      Arr1InnerViewConfigurationVersionImpl = array of InnerViewConfigurationVersionImpl;
      Arr2InnerViewConfigurationVersionImpl = array of Arr1InnerViewConfigurationVersionImpl;
      Arr3InnerViewConfigurationVersionImpl = array of Arr2InnerViewConfigurationVersionImpl;
      InnerFroyoViewConfigurationVersionImpl = class external 'android.support.v4.view' name 'FroyoViewConfigurationVersionImpl' 
      end;

      InnerBaseViewConfigurationVersionImpl = class external 'android.support.v4.view' name 'BaseViewConfigurationVersionImpl' 
      end;

      InnerViewConfigurationVersionImpl = interface external 'android.support.v4.view' name 'ViewConfigurationVersionImpl' 
      end;

  public
    constructor create(); overload;
    class function getScaledPagingTouchSlop(para1: AVViewConfiguration): jint; static; overload;
  end;

  ASVVViewConfigurationCompatFroyo = class external 'android.support.v4.view' name 'ViewConfigurationCompatFroyo' (JLObject)
  public
    class function getScaledPagingTouchSlop(para1: AVViewConfiguration): jint; static; overload;
  end;

  ASVVViewGroupCompat = class external 'android.support.v4.view' name 'ViewGroupCompat' (JLObject)
  public
    type
      InnerViewGroupCompatIcsImpl = class;
      Arr1InnerViewGroupCompatIcsImpl = array of InnerViewGroupCompatIcsImpl;
      Arr2InnerViewGroupCompatIcsImpl = array of Arr1InnerViewGroupCompatIcsImpl;
      Arr3InnerViewGroupCompatIcsImpl = array of Arr2InnerViewGroupCompatIcsImpl;
      InnerViewGroupCompatHCImpl = class;
      Arr1InnerViewGroupCompatHCImpl = array of InnerViewGroupCompatHCImpl;
      Arr2InnerViewGroupCompatHCImpl = array of Arr1InnerViewGroupCompatHCImpl;
      Arr3InnerViewGroupCompatHCImpl = array of Arr2InnerViewGroupCompatHCImpl;
      InnerViewGroupCompatStubImpl = class;
      Arr1InnerViewGroupCompatStubImpl = array of InnerViewGroupCompatStubImpl;
      Arr2InnerViewGroupCompatStubImpl = array of Arr1InnerViewGroupCompatStubImpl;
      Arr3InnerViewGroupCompatStubImpl = array of Arr2InnerViewGroupCompatStubImpl;
      InnerViewGroupCompatImpl = interface;
      Arr1InnerViewGroupCompatImpl = array of InnerViewGroupCompatImpl;
      Arr2InnerViewGroupCompatImpl = array of Arr1InnerViewGroupCompatImpl;
      Arr3InnerViewGroupCompatImpl = array of Arr2InnerViewGroupCompatImpl;
      InnerViewGroupCompatIcsImpl = class external 'android.support.v4.view' name 'ViewGroupCompatIcsImpl' 
      end;

      InnerViewGroupCompatHCImpl = class external 'android.support.v4.view' name 'ViewGroupCompatHCImpl' 
      end;

      InnerViewGroupCompatStubImpl = class external 'android.support.v4.view' name 'ViewGroupCompatStubImpl' 
      end;

      InnerViewGroupCompatImpl = interface external 'android.support.v4.view' name 'ViewGroupCompatImpl' 
      end;

  public
    class function onRequestSendAccessibilityEvent(para1: AVViewGroup; para2: AVView; para3: AVAAccessibilityEvent): jboolean; static; overload;
    class procedure setMotionEventSplittingEnabled(para1: AVViewGroup; para2: jboolean); static; overload;
  end;

  ASVVViewGroupCompatHC = class external 'android.support.v4.view' name 'ViewGroupCompatHC' (JLObject)
  public
    class procedure setMotionEventSplittingEnabled(para1: AVViewGroup; para2: jboolean); static; overload;
  end;

  ASVVViewGroupCompatIcs = class external 'android.support.v4.view' name 'ViewGroupCompatIcs' (JLObject)
  public
    class function onRequestSendAccessibilityEvent(para1: AVViewGroup; para2: AVView; para3: AVAAccessibilityEvent): jboolean; static; overload;
  end;

  ASVVViewParentCompat = class external 'android.support.v4.view' name 'ViewParentCompat' (JLObject)
  public
    type
      InnerViewParentCompatICSImpl = class;
      Arr1InnerViewParentCompatICSImpl = array of InnerViewParentCompatICSImpl;
      Arr2InnerViewParentCompatICSImpl = array of Arr1InnerViewParentCompatICSImpl;
      Arr3InnerViewParentCompatICSImpl = array of Arr2InnerViewParentCompatICSImpl;
      InnerViewParentCompatStubImpl = class;
      Arr1InnerViewParentCompatStubImpl = array of InnerViewParentCompatStubImpl;
      Arr2InnerViewParentCompatStubImpl = array of Arr1InnerViewParentCompatStubImpl;
      Arr3InnerViewParentCompatStubImpl = array of Arr2InnerViewParentCompatStubImpl;
      InnerViewParentCompatImpl = interface;
      Arr1InnerViewParentCompatImpl = array of InnerViewParentCompatImpl;
      Arr2InnerViewParentCompatImpl = array of Arr1InnerViewParentCompatImpl;
      Arr3InnerViewParentCompatImpl = array of Arr2InnerViewParentCompatImpl;
      InnerViewParentCompatICSImpl = class external 'android.support.v4.view' name 'ViewParentCompatICSImpl' 
      end;

      InnerViewParentCompatStubImpl = class external 'android.support.v4.view' name 'ViewParentCompatStubImpl' 
      end;

      InnerViewParentCompatImpl = interface external 'android.support.v4.view' name 'ViewParentCompatImpl' 
      end;

  public
    class function requestSendAccessibilityEvent(para1: AVViewParent; para2: AVView; para3: AVAAccessibilityEvent): jboolean; static; overload;
  end;

  ASVVViewParentCompatICS = class external 'android.support.v4.view' name 'ViewParentCompatICS' (JLObject)
  public
    constructor create(); overload;
    class function requestSendAccessibilityEvent(para1: AVViewParent; para2: AVView; para3: AVAAccessibilityEvent): jboolean; static; overload;
  end;

  ASVVWindowCompat = class external 'android.support.v4.view' name 'WindowCompat' (JLObject)
  public
    const
      FEATURE_ACTION_BAR = 8;
      FEATURE_ACTION_BAR_OVERLAY = 9;
      FEATURE_ACTION_MODE_OVERLAY = 10;
  public
    constructor create(); overload;
  end;

  ASVVAAccessibilityEventCompat = class external 'android.support.v4.view.accessibility' name 'AccessibilityEventCompat' (JLObject)
  public
    type
      InnerAccessibilityEventIcsImpl = class;
      Arr1InnerAccessibilityEventIcsImpl = array of InnerAccessibilityEventIcsImpl;
      Arr2InnerAccessibilityEventIcsImpl = array of Arr1InnerAccessibilityEventIcsImpl;
      Arr3InnerAccessibilityEventIcsImpl = array of Arr2InnerAccessibilityEventIcsImpl;
      InnerAccessibilityEventStubImpl = class;
      Arr1InnerAccessibilityEventStubImpl = array of InnerAccessibilityEventStubImpl;
      Arr2InnerAccessibilityEventStubImpl = array of Arr1InnerAccessibilityEventStubImpl;
      Arr3InnerAccessibilityEventStubImpl = array of Arr2InnerAccessibilityEventStubImpl;
      InnerAccessibilityEventVersionImpl = interface;
      Arr1InnerAccessibilityEventVersionImpl = array of InnerAccessibilityEventVersionImpl;
      Arr2InnerAccessibilityEventVersionImpl = array of Arr1InnerAccessibilityEventVersionImpl;
      Arr3InnerAccessibilityEventVersionImpl = array of Arr2InnerAccessibilityEventVersionImpl;
      InnerAccessibilityEventIcsImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityEventIcsImpl' 
      end;

      InnerAccessibilityEventStubImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityEventStubImpl' 
      end;

      InnerAccessibilityEventVersionImpl = interface external 'android.support.v4.view.accessibility' name 'AccessibilityEventVersionImpl' 
      end;

  public
    const
      TYPE_VIEW_HOVER_ENTER = 128;
      TYPE_VIEW_HOVER_EXIT = 256;
      TYPE_TOUCH_EXPLORATION_GESTURE_START = 512;
      TYPE_TOUCH_EXPLORATION_GESTURE_END = 1024;
      TYPE_WINDOW_CONTENT_CHANGED = 2048;
      TYPE_VIEW_SCROLLED = 4096;
      TYPE_VIEW_TEXT_SELECTION_CHANGED = 8192;
      TYPE_ANNOUNCEMENT = 16384;
      TYPE_VIEW_ACCESSIBILITY_FOCUSED = 32768;
      TYPE_VIEW_ACCESSIBILITY_FOCUS_CLEARED = 65536;
      TYPE_VIEW_TEXT_TRAVERSED_AT_MOVEMENT_GRANULARITY = 131072;
      TYPE_GESTURE_DETECTION_START = 262144;
      TYPE_GESTURE_DETECTION_END = 524288;
      TYPE_TOUCH_INTERACTION_START = 1048576;
      TYPE_TOUCH_INTERACTION_END = 2097152;
      TYPES_ALL_MASK = -1;
  public
    class function getRecordCount(para1: AVAAccessibilityEvent): jint; static; overload;
    class procedure appendRecord(para1: AVAAccessibilityEvent; para2: ASVVAAccessibilityRecordCompat); static; overload;
    class function getRecord(para1: AVAAccessibilityEvent; para2: jint): ASVVAAccessibilityRecordCompat; static; overload;
    class function asRecord(para1: AVAAccessibilityEvent): ASVVAAccessibilityRecordCompat; static; overload;
  end;

  ASVVAAccessibilityEventCompatIcs = class external 'android.support.v4.view.accessibility' name 'AccessibilityEventCompatIcs' (JLObject)
  public
    class function getRecordCount(para1: AVAAccessibilityEvent): jint; static; overload;
    class procedure appendRecord(para1: AVAAccessibilityEvent; para2: JLObject); static; overload;
    class function getRecord(para1: AVAAccessibilityEvent; para2: jint): JLObject; static; overload;
  end;

  ASVVAAccessibilityManagerCompat = class external 'android.support.v4.view.accessibility' name 'AccessibilityManagerCompat' (JLObject)
  public
    type
      InnerAccessibilityManagerIcsImpl = class;
      Arr1InnerAccessibilityManagerIcsImpl = array of InnerAccessibilityManagerIcsImpl;
      Arr2InnerAccessibilityManagerIcsImpl = array of Arr1InnerAccessibilityManagerIcsImpl;
      Arr3InnerAccessibilityManagerIcsImpl = array of Arr2InnerAccessibilityManagerIcsImpl;
      InnerAccessibilityManagerStubImpl = class;
      Arr1InnerAccessibilityManagerStubImpl = array of InnerAccessibilityManagerStubImpl;
      Arr2InnerAccessibilityManagerStubImpl = array of Arr1InnerAccessibilityManagerStubImpl;
      Arr3InnerAccessibilityManagerStubImpl = array of Arr2InnerAccessibilityManagerStubImpl;
      InnerAccessibilityManagerVersionImpl = interface;
      Arr1InnerAccessibilityManagerVersionImpl = array of InnerAccessibilityManagerVersionImpl;
      Arr2InnerAccessibilityManagerVersionImpl = array of Arr1InnerAccessibilityManagerVersionImpl;
      Arr3InnerAccessibilityManagerVersionImpl = array of Arr2InnerAccessibilityManagerVersionImpl;
      InnerAccessibilityManagerIcsImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityManagerIcsImpl' 
      end;

      InnerAccessibilityManagerStubImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityManagerStubImpl' 
      end;

      InnerAccessibilityManagerVersionImpl = interface external 'android.support.v4.view.accessibility' name 'AccessibilityManagerVersionImpl' 
      end;

  public
    type
      InnerAccessibilityStateChangeListenerCompat = class;
      Arr1InnerAccessibilityStateChangeListenerCompat = array of InnerAccessibilityStateChangeListenerCompat;
      Arr2InnerAccessibilityStateChangeListenerCompat = array of Arr1InnerAccessibilityStateChangeListenerCompat;
      Arr3InnerAccessibilityStateChangeListenerCompat = array of Arr2InnerAccessibilityStateChangeListenerCompat;
      InnerAccessibilityStateChangeListenerCompat = class abstract external 'android.support.v4.view.accessibility' name 'AccessibilityStateChangeListenerCompat' (JLObject)
      public
        constructor create(); overload;
        procedure onAccessibilityStateChanged(para1: jboolean); overload; virtual; abstract;
      end;

  public
    constructor create(); overload;
    class function addAccessibilityStateChangeListener(para1: AVAAccessibilityManager; para2: ASVVAAccessibilityManagerCompat.InnerAccessibilityStateChangeListenerCompat): jboolean; static; overload;
    class function removeAccessibilityStateChangeListener(para1: AVAAccessibilityManager; para2: ASVVAAccessibilityManagerCompat.InnerAccessibilityStateChangeListenerCompat): jboolean; static; overload;
    class function getInstalledAccessibilityServiceList(para1: AVAAccessibilityManager): JUList; static; overload;
    class function getEnabledAccessibilityServiceList(para1: AVAAccessibilityManager; para2: jint): JUList; static; overload;
    class function isTouchExplorationEnabled(para1: AVAAccessibilityManager): jboolean; static; overload;
  end;

  ASVVAAccessibilityManagerCompatIcs = class external 'android.support.v4.view.accessibility' name 'AccessibilityManagerCompatIcs' (JLObject)
  public
    type
      InnerAccessibilityStateChangeListenerBridge = interface;
      Arr1InnerAccessibilityStateChangeListenerBridge = array of InnerAccessibilityStateChangeListenerBridge;
      Arr2InnerAccessibilityStateChangeListenerBridge = array of Arr1InnerAccessibilityStateChangeListenerBridge;
      Arr3InnerAccessibilityStateChangeListenerBridge = array of Arr2InnerAccessibilityStateChangeListenerBridge;
      InnerAccessibilityStateChangeListenerBridge = interface external 'android.support.v4.view.accessibility' name 'AccessibilityStateChangeListenerBridge' 
      end;

  public
    class function newAccessibilityStateChangeListener(para1: ASVVAAccessibilityManagerCompatIcs.InnerAccessibilityStateChangeListenerBridge): JLObject; static; overload;
    class function addAccessibilityStateChangeListener(para1: AVAAccessibilityManager; para2: JLObject): jboolean; static; overload;
    class function removeAccessibilityStateChangeListener(para1: AVAAccessibilityManager; para2: JLObject): jboolean; static; overload;
    class function getEnabledAccessibilityServiceList(para1: AVAAccessibilityManager; para2: jint): JUList; static; overload;
    class function getInstalledAccessibilityServiceList(para1: AVAAccessibilityManager): JUList; static; overload;
    class function isTouchExplorationEnabled(para1: AVAAccessibilityManager): jboolean; static; overload;
  end;

  ASVVAAccessibilityNodeInfoCompat = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoCompat' (JLObject)
  public
    type
      InnerAccessibilityNodeInfoJellybeanMr2Impl = class;
      Arr1InnerAccessibilityNodeInfoJellybeanMr2Impl = array of InnerAccessibilityNodeInfoJellybeanMr2Impl;
      Arr2InnerAccessibilityNodeInfoJellybeanMr2Impl = array of Arr1InnerAccessibilityNodeInfoJellybeanMr2Impl;
      Arr3InnerAccessibilityNodeInfoJellybeanMr2Impl = array of Arr2InnerAccessibilityNodeInfoJellybeanMr2Impl;
      InnerAccessibilityNodeInfoJellybeanImpl = class;
      Arr1InnerAccessibilityNodeInfoJellybeanImpl = array of InnerAccessibilityNodeInfoJellybeanImpl;
      Arr2InnerAccessibilityNodeInfoJellybeanImpl = array of Arr1InnerAccessibilityNodeInfoJellybeanImpl;
      Arr3InnerAccessibilityNodeInfoJellybeanImpl = array of Arr2InnerAccessibilityNodeInfoJellybeanImpl;
      InnerAccessibilityNodeInfoIcsImpl = class;
      Arr1InnerAccessibilityNodeInfoIcsImpl = array of InnerAccessibilityNodeInfoIcsImpl;
      Arr2InnerAccessibilityNodeInfoIcsImpl = array of Arr1InnerAccessibilityNodeInfoIcsImpl;
      Arr3InnerAccessibilityNodeInfoIcsImpl = array of Arr2InnerAccessibilityNodeInfoIcsImpl;
      InnerAccessibilityNodeInfoStubImpl = class;
      Arr1InnerAccessibilityNodeInfoStubImpl = array of InnerAccessibilityNodeInfoStubImpl;
      Arr2InnerAccessibilityNodeInfoStubImpl = array of Arr1InnerAccessibilityNodeInfoStubImpl;
      Arr3InnerAccessibilityNodeInfoStubImpl = array of Arr2InnerAccessibilityNodeInfoStubImpl;
      InnerAccessibilityNodeInfoImpl = interface;
      Arr1InnerAccessibilityNodeInfoImpl = array of InnerAccessibilityNodeInfoImpl;
      Arr2InnerAccessibilityNodeInfoImpl = array of Arr1InnerAccessibilityNodeInfoImpl;
      Arr3InnerAccessibilityNodeInfoImpl = array of Arr2InnerAccessibilityNodeInfoImpl;
      InnerAccessibilityNodeInfoJellybeanMr2Impl = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoJellybeanMr2Impl' 
      end;

      InnerAccessibilityNodeInfoJellybeanImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoJellybeanImpl' 
      end;

      InnerAccessibilityNodeInfoIcsImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoIcsImpl' 
      end;

      InnerAccessibilityNodeInfoStubImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoStubImpl' 
      end;

      InnerAccessibilityNodeInfoImpl = interface external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoImpl' 
      end;

  public
    const
      ACTION_FOCUS = 1;
      ACTION_CLEAR_FOCUS = 2;
      ACTION_SELECT = 4;
      ACTION_CLEAR_SELECTION = 8;
      ACTION_CLICK = 16;
      ACTION_LONG_CLICK = 32;
      ACTION_ACCESSIBILITY_FOCUS = 64;
      ACTION_CLEAR_ACCESSIBILITY_FOCUS = 128;
      ACTION_NEXT_AT_MOVEMENT_GRANULARITY = 256;
      ACTION_PREVIOUS_AT_MOVEMENT_GRANULARITY = 512;
      ACTION_NEXT_HTML_ELEMENT = 1024;
      ACTION_PREVIOUS_HTML_ELEMENT = 2048;
      ACTION_SCROLL_FORWARD = 4096;
      ACTION_SCROLL_BACKWARD = 8192;
      ACTION_COPY = 16384;
      ACTION_PASTE = 32768;
      ACTION_CUT = 65536;
      ACTION_SET_SELECTION = 131072;
      ACTION_ARGUMENT_MOVEMENT_GRANULARITY_INT = 'ACTION_ARGUMENT_MOVEMENT_GRANULARITY_INT';
      ACTION_ARGUMENT_HTML_ELEMENT_STRING = 'ACTION_ARGUMENT_HTML_ELEMENT_STRING';
      ACTION_ARGUMENT_EXTEND_SELECTION_BOOLEAN = 'ACTION_ARGUMENT_EXTEND_SELECTION_BOOLEAN';
      ACTION_ARGUMENT_SELECTION_START_INT = 'ACTION_ARGUMENT_SELECTION_START_INT';
      ACTION_ARGUMENT_SELECTION_END_INT = 'ACTION_ARGUMENT_SELECTION_END_INT';
      FOCUS_INPUT = 1;
      FOCUS_ACCESSIBILITY = 2;
      MOVEMENT_GRANULARITY_CHARACTER = 1;
      MOVEMENT_GRANULARITY_WORD = 2;
      MOVEMENT_GRANULARITY_LINE = 4;
      MOVEMENT_GRANULARITY_PARAGRAPH = 8;
      MOVEMENT_GRANULARITY_PAGE = 16;
  public
    constructor create(para1: JLObject); overload;
    function getInfo(): JLObject; overload; virtual;
    class function obtain(para1: AVView): ASVVAAccessibilityNodeInfoCompat; static; overload;
    class function obtain(para1: AVView; para2: jint): ASVVAAccessibilityNodeInfoCompat; static; overload;
    class function obtain(): ASVVAAccessibilityNodeInfoCompat; static; overload;
    class function obtain(para1: ASVVAAccessibilityNodeInfoCompat): ASVVAAccessibilityNodeInfoCompat; static; overload;
    procedure setSource(para1: AVView); overload; virtual;
    procedure setSource(para1: AVView; para2: jint); overload; virtual;
    function findFocus(para1: jint): ASVVAAccessibilityNodeInfoCompat; overload; virtual;
    function focusSearch(para1: jint): ASVVAAccessibilityNodeInfoCompat; overload; virtual;
    function getWindowId(): jint; overload; virtual;
    function getChildCount(): jint; overload; virtual;
    function getChild(para1: jint): ASVVAAccessibilityNodeInfoCompat; overload; virtual;
    procedure addChild(para1: AVView); overload; virtual;
    procedure addChild(para1: AVView; para2: jint); overload; virtual;
    function getActions(): jint; overload; virtual;
    procedure addAction(para1: jint); overload; virtual;
    function performAction(para1: jint): jboolean; overload; virtual;
    function performAction(para1: jint; para2: AOBundle): jboolean; overload; virtual;
    procedure setMovementGranularities(para1: jint); overload; virtual;
    function getMovementGranularities(): jint; overload; virtual;
    function findAccessibilityNodeInfosByText(para1: JLString): JUList; overload; virtual;
    function getParent(): ASVVAAccessibilityNodeInfoCompat; overload; virtual;
    procedure setParent(para1: AVView); overload; virtual;
    procedure setParent(para1: AVView; para2: jint); overload; virtual;
    procedure getBoundsInParent(para1: AGRect); overload; virtual;
    procedure setBoundsInParent(para1: AGRect); overload; virtual;
    procedure getBoundsInScreen(para1: AGRect); overload; virtual;
    procedure setBoundsInScreen(para1: AGRect); overload; virtual;
    function isCheckable(): jboolean; overload; virtual;
    procedure setCheckable(para1: jboolean); overload; virtual;
    function isChecked(): jboolean; overload; virtual;
    procedure setChecked(para1: jboolean); overload; virtual;
    function isFocusable(): jboolean; overload; virtual;
    procedure setFocusable(para1: jboolean); overload; virtual;
    function isFocused(): jboolean; overload; virtual;
    procedure setFocused(para1: jboolean); overload; virtual;
    function isVisibleToUser(): jboolean; overload; virtual;
    procedure setVisibleToUser(para1: jboolean); overload; virtual;
    function isAccessibilityFocused(): jboolean; overload; virtual;
    procedure setAccessibilityFocused(para1: jboolean); overload; virtual;
    function isSelected(): jboolean; overload; virtual;
    procedure setSelected(para1: jboolean); overload; virtual;
    function isClickable(): jboolean; overload; virtual;
    procedure setClickable(para1: jboolean); overload; virtual;
    function isLongClickable(): jboolean; overload; virtual;
    procedure setLongClickable(para1: jboolean); overload; virtual;
    function isEnabled(): jboolean; overload; virtual;
    procedure setEnabled(para1: jboolean); overload; virtual;
    function isPassword(): jboolean; overload; virtual;
    procedure setPassword(para1: jboolean); overload; virtual;
    function isScrollable(): jboolean; overload; virtual;
    procedure setScrollable(para1: jboolean); overload; virtual;
    function getPackageName(): JLCharSequence; overload; virtual;
    procedure setPackageName(para1: JLCharSequence); overload; virtual;
    function getClassName(): JLCharSequence; overload; virtual;
    procedure setClassName(para1: JLCharSequence); overload; virtual;
    function getText(): JLCharSequence; overload; virtual;
    procedure setText(para1: JLCharSequence); overload; virtual;
    function getContentDescription(): JLCharSequence; overload; virtual;
    procedure setContentDescription(para1: JLCharSequence); overload; virtual;
    procedure recycle(); overload; virtual;
    procedure setViewIdResourceName(para1: JLString); overload; virtual;
    function getViewIdResourceName(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  ASVVAAccessibilityNodeInfoCompatIcs = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoCompatIcs' (JLObject)
  public
    class function obtain(): JLObject; static; overload;
    class function obtain(para1: AVView): JLObject; static; overload;
    class function obtain(para1: JLObject): JLObject; static; overload;
    class procedure addAction(para1: JLObject; para2: jint); static; overload;
    class procedure addChild(para1: JLObject; para2: AVView); static; overload;
    class function findAccessibilityNodeInfosByText(para1: JLObject; para2: JLString): JUList; static; overload;
    class function getActions(para1: JLObject): jint; static; overload;
    class procedure getBoundsInParent(para1: JLObject; para2: AGRect); static; overload;
    class procedure getBoundsInScreen(para1: JLObject; para2: AGRect); static; overload;
    class function getChild(para1: JLObject; para2: jint): JLObject; static; overload;
    class function getChildCount(para1: JLObject): jint; static; overload;
    class function getClassName(para1: JLObject): JLCharSequence; static; overload;
    class function getContentDescription(para1: JLObject): JLCharSequence; static; overload;
    class function getPackageName(para1: JLObject): JLCharSequence; static; overload;
    class function getParent(para1: JLObject): JLObject; static; overload;
    class function getText(para1: JLObject): JLCharSequence; static; overload;
    class function getWindowId(para1: JLObject): jint; static; overload;
    class function isCheckable(para1: JLObject): jboolean; static; overload;
    class function isChecked(para1: JLObject): jboolean; static; overload;
    class function isClickable(para1: JLObject): jboolean; static; overload;
    class function isEnabled(para1: JLObject): jboolean; static; overload;
    class function isFocusable(para1: JLObject): jboolean; static; overload;
    class function isFocused(para1: JLObject): jboolean; static; overload;
    class function isLongClickable(para1: JLObject): jboolean; static; overload;
    class function isPassword(para1: JLObject): jboolean; static; overload;
    class function isScrollable(para1: JLObject): jboolean; static; overload;
    class function isSelected(para1: JLObject): jboolean; static; overload;
    class function performAction(para1: JLObject; para2: jint): jboolean; static; overload;
    class procedure setBoundsInParent(para1: JLObject; para2: AGRect); static; overload;
    class procedure setBoundsInScreen(para1: JLObject; para2: AGRect); static; overload;
    class procedure setCheckable(para1: JLObject; para2: jboolean); static; overload;
    class procedure setChecked(para1: JLObject; para2: jboolean); static; overload;
    class procedure setClassName(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure setClickable(para1: JLObject; para2: jboolean); static; overload;
    class procedure setContentDescription(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure setEnabled(para1: JLObject; para2: jboolean); static; overload;
    class procedure setFocusable(para1: JLObject; para2: jboolean); static; overload;
    class procedure setFocused(para1: JLObject; para2: jboolean); static; overload;
    class procedure setLongClickable(para1: JLObject; para2: jboolean); static; overload;
    class procedure setPackageName(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure setParent(para1: JLObject; para2: AVView); static; overload;
    class procedure setPassword(para1: JLObject; para2: jboolean); static; overload;
    class procedure setScrollable(para1: JLObject; para2: jboolean); static; overload;
    class procedure setSelected(para1: JLObject; para2: jboolean); static; overload;
    class procedure setSource(para1: JLObject; para2: AVView); static; overload;
    class procedure setText(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure recycle(para1: JLObject); static; overload;
  end;

  ASVVAAccessibilityNodeInfoCompatJellyBean = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoCompatJellyBean' (JLObject)
  public
    class procedure addChild(para1: JLObject; para2: AVView; para3: jint); static; overload;
    class procedure setSource(para1: JLObject; para2: AVView; para3: jint); static; overload;
    class function isVisibleToUser(para1: JLObject): jboolean; static; overload;
    class procedure setVisibleToUser(para1: JLObject; para2: jboolean); static; overload;
    class function performAction(para1: JLObject; para2: jint; para3: AOBundle): jboolean; static; overload;
    class procedure setMovementGranularities(para1: JLObject; para2: jint); static; overload;
    class function getMovementGranularities(para1: JLObject): jint; static; overload;
    class function obtain(para1: AVView; para2: jint): JLObject; static; overload;
    class function findFocus(para1: JLObject; para2: jint): JLObject; static; overload;
    class function focusSearch(para1: JLObject; para2: jint): JLObject; static; overload;
    class procedure setParent(para1: JLObject; para2: AVView; para3: jint); static; overload;
    class function isAccessibilityFocused(para1: JLObject): jboolean; static; overload;
    class procedure setAccesibilityFocused(para1: JLObject; para2: jboolean); static; overload;
  end;

  ASVVAAccessibilityNodeInfoCompatJellybeanMr2 = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoCompatJellybeanMr2' (JLObject)
  public
    class procedure setViewIdResourceName(para1: JLObject; para2: JLString); static; overload;
    class function getViewIdResourceName(para1: JLObject): JLString; static; overload;
  end;

  ASVVAAccessibilityNodeProviderCompat = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeProviderCompat' (JLObject)
  public
    type
      InnerAccessibilityNodeProviderJellyBeanImpl = class;
      Arr1InnerAccessibilityNodeProviderJellyBeanImpl = array of InnerAccessibilityNodeProviderJellyBeanImpl;
      Arr2InnerAccessibilityNodeProviderJellyBeanImpl = array of Arr1InnerAccessibilityNodeProviderJellyBeanImpl;
      Arr3InnerAccessibilityNodeProviderJellyBeanImpl = array of Arr2InnerAccessibilityNodeProviderJellyBeanImpl;
      InnerAccessibilityNodeProviderStubImpl = class;
      Arr1InnerAccessibilityNodeProviderStubImpl = array of InnerAccessibilityNodeProviderStubImpl;
      Arr2InnerAccessibilityNodeProviderStubImpl = array of Arr1InnerAccessibilityNodeProviderStubImpl;
      Arr3InnerAccessibilityNodeProviderStubImpl = array of Arr2InnerAccessibilityNodeProviderStubImpl;
      InnerAccessibilityNodeProviderImpl = interface;
      Arr1InnerAccessibilityNodeProviderImpl = array of InnerAccessibilityNodeProviderImpl;
      Arr2InnerAccessibilityNodeProviderImpl = array of Arr1InnerAccessibilityNodeProviderImpl;
      Arr3InnerAccessibilityNodeProviderImpl = array of Arr2InnerAccessibilityNodeProviderImpl;
      InnerAccessibilityNodeProviderJellyBeanImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeProviderJellyBeanImpl' 
      end;

      InnerAccessibilityNodeProviderStubImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeProviderStubImpl' 
      end;

      InnerAccessibilityNodeProviderImpl = interface external 'android.support.v4.view.accessibility' name 'AccessibilityNodeProviderImpl' 
      end;

  public
    constructor create(); overload;
    constructor create(para1: JLObject); overload;
    function getProvider(): JLObject; overload; virtual;
    function createAccessibilityNodeInfo(para1: jint): ASVVAAccessibilityNodeInfoCompat; overload; virtual;
    function performAction(para1: jint; para2: jint; para3: AOBundle): jboolean; overload; virtual;
    function findAccessibilityNodeInfosByText(para1: JLString; para2: jint): JUList; overload; virtual;
  end;

  ASVVAAccessibilityNodeProviderCompatJellyBean = class external 'android.support.v4.view.accessibility' name 'AccessibilityNodeProviderCompatJellyBean' (JLObject)
  public
    type
      InnerAccessibilityNodeInfoBridge = interface;
      Arr1InnerAccessibilityNodeInfoBridge = array of InnerAccessibilityNodeInfoBridge;
      Arr2InnerAccessibilityNodeInfoBridge = array of Arr1InnerAccessibilityNodeInfoBridge;
      Arr3InnerAccessibilityNodeInfoBridge = array of Arr2InnerAccessibilityNodeInfoBridge;
      InnerAccessibilityNodeInfoBridge = interface external 'android.support.v4.view.accessibility' name 'AccessibilityNodeInfoBridge' 
      end;

  public
    class function newAccessibilityNodeProviderBridge(para1: ASVVAAccessibilityNodeProviderCompatJellyBean.InnerAccessibilityNodeInfoBridge): JLObject; static; overload;
  end;

  ASVVAAccessibilityRecordCompat = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordCompat' (JLObject)
  public
    type
      InnerAccessibilityRecordJellyBeanImpl = class;
      Arr1InnerAccessibilityRecordJellyBeanImpl = array of InnerAccessibilityRecordJellyBeanImpl;
      Arr2InnerAccessibilityRecordJellyBeanImpl = array of Arr1InnerAccessibilityRecordJellyBeanImpl;
      Arr3InnerAccessibilityRecordJellyBeanImpl = array of Arr2InnerAccessibilityRecordJellyBeanImpl;
      InnerAccessibilityRecordIcsMr1Impl = class;
      Arr1InnerAccessibilityRecordIcsMr1Impl = array of InnerAccessibilityRecordIcsMr1Impl;
      Arr2InnerAccessibilityRecordIcsMr1Impl = array of Arr1InnerAccessibilityRecordIcsMr1Impl;
      Arr3InnerAccessibilityRecordIcsMr1Impl = array of Arr2InnerAccessibilityRecordIcsMr1Impl;
      InnerAccessibilityRecordIcsImpl = class;
      Arr1InnerAccessibilityRecordIcsImpl = array of InnerAccessibilityRecordIcsImpl;
      Arr2InnerAccessibilityRecordIcsImpl = array of Arr1InnerAccessibilityRecordIcsImpl;
      Arr3InnerAccessibilityRecordIcsImpl = array of Arr2InnerAccessibilityRecordIcsImpl;
      InnerAccessibilityRecordStubImpl = class;
      Arr1InnerAccessibilityRecordStubImpl = array of InnerAccessibilityRecordStubImpl;
      Arr2InnerAccessibilityRecordStubImpl = array of Arr1InnerAccessibilityRecordStubImpl;
      Arr3InnerAccessibilityRecordStubImpl = array of Arr2InnerAccessibilityRecordStubImpl;
      InnerAccessibilityRecordImpl = interface;
      Arr1InnerAccessibilityRecordImpl = array of InnerAccessibilityRecordImpl;
      Arr2InnerAccessibilityRecordImpl = array of Arr1InnerAccessibilityRecordImpl;
      Arr3InnerAccessibilityRecordImpl = array of Arr2InnerAccessibilityRecordImpl;
      InnerAccessibilityRecordJellyBeanImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordJellyBeanImpl' 
      end;

      InnerAccessibilityRecordIcsMr1Impl = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordIcsMr1Impl' 
      end;

      InnerAccessibilityRecordIcsImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordIcsImpl' 
      end;

      InnerAccessibilityRecordStubImpl = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordStubImpl' 
      end;

      InnerAccessibilityRecordImpl = interface external 'android.support.v4.view.accessibility' name 'AccessibilityRecordImpl' 
      end;

  public
    constructor create(para1: JLObject); overload;
    function getImpl(): JLObject; overload; virtual;
    class function obtain(para1: ASVVAAccessibilityRecordCompat): ASVVAAccessibilityRecordCompat; static; overload;
    class function obtain(): ASVVAAccessibilityRecordCompat; static; overload;
    procedure setSource(para1: AVView); overload; virtual;
    procedure setSource(para1: AVView; para2: jint); overload; virtual;
    function getSource(): ASVVAAccessibilityNodeInfoCompat; overload; virtual;
    function getWindowId(): jint; overload; virtual;
    function isChecked(): jboolean; overload; virtual;
    procedure setChecked(para1: jboolean); overload; virtual;
    function isEnabled(): jboolean; overload; virtual;
    procedure setEnabled(para1: jboolean); overload; virtual;
    function isPassword(): jboolean; overload; virtual;
    procedure setPassword(para1: jboolean); overload; virtual;
    function isFullScreen(): jboolean; overload; virtual;
    procedure setFullScreen(para1: jboolean); overload; virtual;
    function isScrollable(): jboolean; overload; virtual;
    procedure setScrollable(para1: jboolean); overload; virtual;
    function getItemCount(): jint; overload; virtual;
    procedure setItemCount(para1: jint); overload; virtual;
    function getCurrentItemIndex(): jint; overload; virtual;
    procedure setCurrentItemIndex(para1: jint); overload; virtual;
    function getFromIndex(): jint; overload; virtual;
    procedure setFromIndex(para1: jint); overload; virtual;
    function getToIndex(): jint; overload; virtual;
    procedure setToIndex(para1: jint); overload; virtual;
    function getScrollX(): jint; overload; virtual;
    procedure setScrollX(para1: jint); overload; virtual;
    function getScrollY(): jint; overload; virtual;
    procedure setScrollY(para1: jint); overload; virtual;
    function getMaxScrollX(): jint; overload; virtual;
    procedure setMaxScrollX(para1: jint); overload; virtual;
    function getMaxScrollY(): jint; overload; virtual;
    procedure setMaxScrollY(para1: jint); overload; virtual;
    function getAddedCount(): jint; overload; virtual;
    procedure setAddedCount(para1: jint); overload; virtual;
    function getRemovedCount(): jint; overload; virtual;
    procedure setRemovedCount(para1: jint); overload; virtual;
    function getClassName(): JLCharSequence; overload; virtual;
    procedure setClassName(para1: JLCharSequence); overload; virtual;
    function getText(): JUList; overload; virtual;
    function getBeforeText(): JLCharSequence; overload; virtual;
    procedure setBeforeText(para1: JLCharSequence); overload; virtual;
    function getContentDescription(): JLCharSequence; overload; virtual;
    procedure setContentDescription(para1: JLCharSequence); overload; virtual;
    function getParcelableData(): AOParcelable; overload; virtual;
    procedure setParcelableData(para1: AOParcelable); overload; virtual;
    procedure recycle(); overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
  end;

  ASVVAAccessibilityRecordCompatIcs = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordCompatIcs' (JLObject)
  public
    class function obtain(): JLObject; static; overload;
    class function obtain(para1: JLObject): JLObject; static; overload;
    class function getAddedCount(para1: JLObject): jint; static; overload;
    class function getBeforeText(para1: JLObject): JLCharSequence; static; overload;
    class function getClassName(para1: JLObject): JLCharSequence; static; overload;
    class function getContentDescription(para1: JLObject): JLCharSequence; static; overload;
    class function getCurrentItemIndex(para1: JLObject): jint; static; overload;
    class function getFromIndex(para1: JLObject): jint; static; overload;
    class function getItemCount(para1: JLObject): jint; static; overload;
    class function getParcelableData(para1: JLObject): AOParcelable; static; overload;
    class function getRemovedCount(para1: JLObject): jint; static; overload;
    class function getScrollX(para1: JLObject): jint; static; overload;
    class function getScrollY(para1: JLObject): jint; static; overload;
    class function getSource(para1: JLObject): JLObject; static; overload;
    class function getText(para1: JLObject): JUList; static; overload;
    class function getToIndex(para1: JLObject): jint; static; overload;
    class function getWindowId(para1: JLObject): jint; static; overload;
    class function isChecked(para1: JLObject): jboolean; static; overload;
    class function isEnabled(para1: JLObject): jboolean; static; overload;
    class function isFullScreen(para1: JLObject): jboolean; static; overload;
    class function isPassword(para1: JLObject): jboolean; static; overload;
    class function isScrollable(para1: JLObject): jboolean; static; overload;
    class procedure recycle(para1: JLObject); static; overload;
    class procedure setAddedCount(para1: JLObject; para2: jint); static; overload;
    class procedure setBeforeText(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure setChecked(para1: JLObject; para2: jboolean); static; overload;
    class procedure setClassName(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure setContentDescription(para1: JLObject; para2: JLCharSequence); static; overload;
    class procedure setCurrentItemIndex(para1: JLObject; para2: jint); static; overload;
    class procedure setEnabled(para1: JLObject; para2: jboolean); static; overload;
    class procedure setFromIndex(para1: JLObject; para2: jint); static; overload;
    class procedure setFullScreen(para1: JLObject; para2: jboolean); static; overload;
    class procedure setItemCount(para1: JLObject; para2: jint); static; overload;
    class procedure setParcelableData(para1: JLObject; para2: AOParcelable); static; overload;
    class procedure setPassword(para1: JLObject; para2: jboolean); static; overload;
    class procedure setRemovedCount(para1: JLObject; para2: jint); static; overload;
    class procedure setScrollX(para1: JLObject; para2: jint); static; overload;
    class procedure setScrollY(para1: JLObject; para2: jint); static; overload;
    class procedure setScrollable(para1: JLObject; para2: jboolean); static; overload;
    class procedure setSource(para1: JLObject; para2: AVView); static; overload;
    class procedure setToIndex(para1: JLObject; para2: jint); static; overload;
  end;

  ASVVAAccessibilityRecordCompatIcsMr1 = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordCompatIcsMr1' (JLObject)
  public
    class function getMaxScrollX(para1: JLObject): jint; static; overload;
    class function getMaxScrollY(para1: JLObject): jint; static; overload;
    class procedure setMaxScrollX(para1: JLObject; para2: jint); static; overload;
    class procedure setMaxScrollY(para1: JLObject; para2: jint); static; overload;
  end;

  ASVVAAccessibilityRecordCompatJellyBean = class external 'android.support.v4.view.accessibility' name 'AccessibilityRecordCompatJellyBean' (JLObject)
  public
    class procedure setSource(para1: JLObject; para2: AVView; para3: jint); static; overload;
  end;

  ASVWEdgeEffectCompat = class external 'android.support.v4.widget' name 'EdgeEffectCompat' (JLObject)
  public
    type
      InnerEdgeEffectIcsImpl = class;
      Arr1InnerEdgeEffectIcsImpl = array of InnerEdgeEffectIcsImpl;
      Arr2InnerEdgeEffectIcsImpl = array of Arr1InnerEdgeEffectIcsImpl;
      Arr3InnerEdgeEffectIcsImpl = array of Arr2InnerEdgeEffectIcsImpl;
      InnerBaseEdgeEffectImpl = class;
      Arr1InnerBaseEdgeEffectImpl = array of InnerBaseEdgeEffectImpl;
      Arr2InnerBaseEdgeEffectImpl = array of Arr1InnerBaseEdgeEffectImpl;
      Arr3InnerBaseEdgeEffectImpl = array of Arr2InnerBaseEdgeEffectImpl;
      InnerEdgeEffectImpl = interface;
      Arr1InnerEdgeEffectImpl = array of InnerEdgeEffectImpl;
      Arr2InnerEdgeEffectImpl = array of Arr1InnerEdgeEffectImpl;
      Arr3InnerEdgeEffectImpl = array of Arr2InnerEdgeEffectImpl;
      InnerEdgeEffectIcsImpl = class external 'android.support.v4.widget' name 'EdgeEffectIcsImpl' 
      end;

      InnerBaseEdgeEffectImpl = class external 'android.support.v4.widget' name 'BaseEdgeEffectImpl' 
      end;

      InnerEdgeEffectImpl = interface external 'android.support.v4.widget' name 'EdgeEffectImpl' 
      end;

  public
    constructor create(para1: ACContext); overload;
    procedure setSize(para1: jint; para2: jint); overload; virtual;
    function isFinished(): jboolean; overload; virtual;
    procedure finish(); overload; virtual;
    function onPull(para1: jfloat): jboolean; overload; virtual;
    function onRelease(): jboolean; overload; virtual;
    function onAbsorb(para1: jint): jboolean; overload; virtual;
    function draw(para1: AGCanvas): jboolean; overload; virtual;
  end;

  ASVWEdgeEffectCompatIcs = class external 'android.support.v4.widget' name 'EdgeEffectCompatIcs' (JLObject)
  public
    class function newEdgeEffect(para1: ACContext): JLObject; static; overload;
    class procedure setSize(para1: JLObject; para2: jint; para3: jint); static; overload;
    class function isFinished(para1: JLObject): jboolean; static; overload;
    class procedure finish(para1: JLObject); static; overload;
    class function onPull(para1: JLObject; para2: jfloat): jboolean; static; overload;
    class function onRelease(para1: JLObject): jboolean; static; overload;
    class function onAbsorb(para1: JLObject; para2: jint): jboolean; static; overload;
    class function draw(para1: JLObject; para2: AGCanvas): jboolean; static; overload;
  end;

  ASVWScrollerCompat = class external 'android.support.v4.widget' name 'ScrollerCompat' (JLObject)
  public
    type
      InnerScrollerCompatImplIcs = class;
      Arr1InnerScrollerCompatImplIcs = array of InnerScrollerCompatImplIcs;
      Arr2InnerScrollerCompatImplIcs = array of Arr1InnerScrollerCompatImplIcs;
      Arr3InnerScrollerCompatImplIcs = array of Arr2InnerScrollerCompatImplIcs;
      InnerScrollerCompatImplGingerbread = class;
      Arr1InnerScrollerCompatImplGingerbread = array of InnerScrollerCompatImplGingerbread;
      Arr2InnerScrollerCompatImplGingerbread = array of Arr1InnerScrollerCompatImplGingerbread;
      Arr3InnerScrollerCompatImplGingerbread = array of Arr2InnerScrollerCompatImplGingerbread;
      InnerScrollerCompatImplBase = class;
      Arr1InnerScrollerCompatImplBase = array of InnerScrollerCompatImplBase;
      Arr2InnerScrollerCompatImplBase = array of Arr1InnerScrollerCompatImplBase;
      Arr3InnerScrollerCompatImplBase = array of Arr2InnerScrollerCompatImplBase;
      InnerScrollerCompatImpl = interface;
      Arr1InnerScrollerCompatImpl = array of InnerScrollerCompatImpl;
      Arr2InnerScrollerCompatImpl = array of Arr1InnerScrollerCompatImpl;
      Arr3InnerScrollerCompatImpl = array of Arr2InnerScrollerCompatImpl;
      InnerScrollerCompatImplIcs = class external 'android.support.v4.widget' name 'ScrollerCompatImplIcs' 
      end;

      InnerScrollerCompatImplGingerbread = class external 'android.support.v4.widget' name 'ScrollerCompatImplGingerbread' 
      end;

      InnerScrollerCompatImplBase = class external 'android.support.v4.widget' name 'ScrollerCompatImplBase' 
      end;

      InnerScrollerCompatImpl = interface external 'android.support.v4.widget' name 'ScrollerCompatImpl' 
      end;

  public
    class function create_(para1: ACContext): ASVWScrollerCompat; static; external name 'create'; overload;
    class function create_(para1: ACContext; para2: AVAInterpolator): ASVWScrollerCompat; static; external name 'create'; overload;
    function isFinished(): jboolean; overload; virtual;
    function getCurrX(): jint; overload; virtual;
    function getCurrY(): jint; overload; virtual;
    function getFinalX(): jint; overload; virtual;
    function getFinalY(): jint; overload; virtual;
    function getCurrVelocity(): jfloat; overload; virtual;
    function computeScrollOffset(): jboolean; overload; virtual;
    procedure startScroll(para1: jint; para2: jint; para3: jint; para4: jint); overload; virtual;
    procedure startScroll(para1: jint; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
    procedure fling(para1: jint; para2: jint; para3: jint; para4: jint; para5: jint; para6: jint; para7: jint; para8: jint); overload; virtual;
    procedure fling(para1: jint; para2: jint; para3: jint; para4: jint; para5: jint; para6: jint; para7: jint; para8: jint; para9: jint; para10: jint); overload; virtual;
    procedure abortAnimation(); overload; virtual;
    procedure notifyHorizontalEdgeReached(para1: jint; para2: jint; para3: jint); overload; virtual;
    procedure notifyVerticalEdgeReached(para1: jint; para2: jint; para3: jint); overload; virtual;
    function isOverScrolled(): jboolean; overload; virtual;
  end;

  ASVWScrollerCompatGingerbread = class external 'android.support.v4.widget' name 'ScrollerCompatGingerbread' (JLObject)
  public
    class function createScroller(para1: ACContext; para2: AVAInterpolator): JLObject; static; overload;
    class function isFinished(para1: JLObject): jboolean; static; overload;
    class function getCurrX(para1: JLObject): jint; static; overload;
    class function getCurrY(para1: JLObject): jint; static; overload;
    class function computeScrollOffset(para1: JLObject): jboolean; static; overload;
    class procedure startScroll(para1: JLObject; para2: jint; para3: jint; para4: jint; para5: jint); static; overload;
    class procedure startScroll(para1: JLObject; para2: jint; para3: jint; para4: jint; para5: jint; para6: jint); static; overload;
    class procedure fling(para1: JLObject; para2: jint; para3: jint; para4: jint; para5: jint; para6: jint; para7: jint; para8: jint; para9: jint); static; overload;
    class procedure fling(para1: JLObject; para2: jint; para3: jint; para4: jint; para5: jint; para6: jint; para7: jint; para8: jint; para9: jint; para10: jint; para11: jint); static; overload;
    class procedure abortAnimation(para1: JLObject); static; overload;
    class procedure notifyHorizontalEdgeReached(para1: JLObject; para2: jint; para3: jint; para4: jint); static; overload;
    class procedure notifyVerticalEdgeReached(para1: JLObject; para2: jint; para3: jint; para4: jint); static; overload;
    class function isOverScrolled(para1: JLObject): jboolean; static; overload;
    class function getFinalX(para1: JLObject): jint; static; overload;
    class function getFinalY(para1: JLObject): jint; static; overload;
  end;

  ASVWScrollerCompatIcs = class external 'android.support.v4.widget' name 'ScrollerCompatIcs' (JLObject)
  public
    class function getCurrVelocity(para1: JLObject): jfloat; static; overload;
  end;

  ASVWSearchViewCompat = class external 'android.support.v4.widget' name 'SearchViewCompat' (JLObject)
  public
    type
      InnerSearchViewCompatIcsImpl = class;
      Arr1InnerSearchViewCompatIcsImpl = array of InnerSearchViewCompatIcsImpl;
      Arr2InnerSearchViewCompatIcsImpl = array of Arr1InnerSearchViewCompatIcsImpl;
      Arr3InnerSearchViewCompatIcsImpl = array of Arr2InnerSearchViewCompatIcsImpl;
      InnerSearchViewCompatHoneycombImpl = class;
      Arr1InnerSearchViewCompatHoneycombImpl = array of InnerSearchViewCompatHoneycombImpl;
      Arr2InnerSearchViewCompatHoneycombImpl = array of Arr1InnerSearchViewCompatHoneycombImpl;
      Arr3InnerSearchViewCompatHoneycombImpl = array of Arr2InnerSearchViewCompatHoneycombImpl;
      InnerSearchViewCompatStubImpl = class;
      Arr1InnerSearchViewCompatStubImpl = array of InnerSearchViewCompatStubImpl;
      Arr2InnerSearchViewCompatStubImpl = array of Arr1InnerSearchViewCompatStubImpl;
      Arr3InnerSearchViewCompatStubImpl = array of Arr2InnerSearchViewCompatStubImpl;
      InnerSearchViewCompatImpl = interface;
      Arr1InnerSearchViewCompatImpl = array of InnerSearchViewCompatImpl;
      Arr2InnerSearchViewCompatImpl = array of Arr1InnerSearchViewCompatImpl;
      Arr3InnerSearchViewCompatImpl = array of Arr2InnerSearchViewCompatImpl;
      InnerSearchViewCompatIcsImpl = class external 'android.support.v4.widget' name 'SearchViewCompatIcsImpl' 
      end;

      InnerSearchViewCompatHoneycombImpl = class external 'android.support.v4.widget' name 'SearchViewCompatHoneycombImpl' 
      end;

      InnerSearchViewCompatStubImpl = class external 'android.support.v4.widget' name 'SearchViewCompatStubImpl' 
      end;

      InnerSearchViewCompatImpl = interface external 'android.support.v4.widget' name 'SearchViewCompatImpl' 
      end;

  public
    type
      InnerOnCloseListenerCompat = class;
      Arr1InnerOnCloseListenerCompat = array of InnerOnCloseListenerCompat;
      Arr2InnerOnCloseListenerCompat = array of Arr1InnerOnCloseListenerCompat;
      Arr3InnerOnCloseListenerCompat = array of Arr2InnerOnCloseListenerCompat;
      InnerOnQueryTextListenerCompat = class;
      Arr1InnerOnQueryTextListenerCompat = array of InnerOnQueryTextListenerCompat;
      Arr2InnerOnQueryTextListenerCompat = array of Arr1InnerOnQueryTextListenerCompat;
      Arr3InnerOnQueryTextListenerCompat = array of Arr2InnerOnQueryTextListenerCompat;
      InnerOnCloseListenerCompat = class abstract external 'android.support.v4.widget' name 'OnCloseListenerCompat' (JLObject)
      public
        constructor create(); overload;
        function onClose(): jboolean; overload; virtual;
      end;

      InnerOnQueryTextListenerCompat = class abstract external 'android.support.v4.widget' name 'OnQueryTextListenerCompat' (JLObject)
      public
        constructor create(); overload;
        function onQueryTextSubmit(para1: JLString): jboolean; overload; virtual;
        function onQueryTextChange(para1: JLString): jboolean; overload; virtual;
      end;

  public
    class function newSearchView(para1: ACContext): AVView; static; overload;
    class procedure setSearchableInfo(para1: AVView; para2: ACComponentName); static; overload;
    class procedure setImeOptions(para1: AVView; para2: jint); static; overload;
    class procedure setInputType(para1: AVView; para2: jint); static; overload;
    class procedure setOnQueryTextListener(para1: AVView; para2: ASVWSearchViewCompat.InnerOnQueryTextListenerCompat); static; overload;
    class procedure setOnCloseListener(para1: AVView; para2: ASVWSearchViewCompat.InnerOnCloseListenerCompat); static; overload;
    class function getQuery(para1: AVView): JLCharSequence; static; overload;
    class procedure setQuery(para1: AVView; para2: JLCharSequence; para3: jboolean); static; overload;
    class procedure setQueryHint(para1: AVView; para2: JLCharSequence); static; overload;
    class procedure setIconified(para1: AVView; para2: jboolean); static; overload;
    class function isIconified(para1: AVView): jboolean; static; overload;
    class procedure setSubmitButtonEnabled(para1: AVView; para2: jboolean); static; overload;
    class function isSubmitButtonEnabled(para1: AVView): jboolean; static; overload;
    class procedure setQueryRefinementEnabled(para1: AVView; para2: jboolean); static; overload;
    class function isQueryRefinementEnabled(para1: AVView): jboolean; static; overload;
    class procedure setMaxWidth(para1: AVView; para2: jint); static; overload;
  end;

  ASVWSearchViewCompatHoneycomb = class external 'android.support.v4.widget' name 'SearchViewCompatHoneycomb' (JLObject)
  public
    type
      InnerOnCloseListenerCompatBridge = interface;
      Arr1InnerOnCloseListenerCompatBridge = array of InnerOnCloseListenerCompatBridge;
      Arr2InnerOnCloseListenerCompatBridge = array of Arr1InnerOnCloseListenerCompatBridge;
      Arr3InnerOnCloseListenerCompatBridge = array of Arr2InnerOnCloseListenerCompatBridge;
      InnerOnQueryTextListenerCompatBridge = interface;
      Arr1InnerOnQueryTextListenerCompatBridge = array of InnerOnQueryTextListenerCompatBridge;
      Arr2InnerOnQueryTextListenerCompatBridge = array of Arr1InnerOnQueryTextListenerCompatBridge;
      Arr3InnerOnQueryTextListenerCompatBridge = array of Arr2InnerOnQueryTextListenerCompatBridge;
      InnerOnCloseListenerCompatBridge = interface external 'android.support.v4.widget' name 'OnCloseListenerCompatBridge' 
      end;

      InnerOnQueryTextListenerCompatBridge = interface external 'android.support.v4.widget' name 'OnQueryTextListenerCompatBridge' 
      end;

  public
    class function newSearchView(para1: ACContext): AVView; static; overload;
    class procedure setSearchableInfo(para1: AVView; para2: ACComponentName); static; overload;
    class function newOnQueryTextListener(para1: ASVWSearchViewCompatHoneycomb.InnerOnQueryTextListenerCompatBridge): JLObject; static; overload;
    class procedure setOnQueryTextListener(para1: JLObject; para2: JLObject); static; overload;
    class function newOnCloseListener(para1: ASVWSearchViewCompatHoneycomb.InnerOnCloseListenerCompatBridge): JLObject; static; overload;
    class procedure setOnCloseListener(para1: JLObject; para2: JLObject); static; overload;
    class function getQuery(para1: AVView): JLCharSequence; static; overload;
    class procedure setQuery(para1: AVView; para2: JLCharSequence; para3: jboolean); static; overload;
    class procedure setQueryHint(para1: AVView; para2: JLCharSequence); static; overload;
    class procedure setIconified(para1: AVView; para2: jboolean); static; overload;
    class function isIconified(para1: AVView): jboolean; static; overload;
    class procedure setSubmitButtonEnabled(para1: AVView; para2: jboolean); static; overload;
    class function isSubmitButtonEnabled(para1: AVView): jboolean; static; overload;
    class procedure setQueryRefinementEnabled(para1: AVView; para2: jboolean); static; overload;
    class function isQueryRefinementEnabled(para1: AVView): jboolean; static; overload;
    class procedure setMaxWidth(para1: AVView; para2: jint); static; overload;
  end;

  ASVWViewDragHelper = class external 'android.support.v4.widget' name 'ViewDragHelper' (JLObject)
  public
    type
      InnerCallback = class;
      Arr1InnerCallback = array of InnerCallback;
      Arr2InnerCallback = array of Arr1InnerCallback;
      Arr3InnerCallback = array of Arr2InnerCallback;
      InnerCallback = class abstract external 'android.support.v4.widget' name 'Callback' (JLObject)
      public
        constructor create(); overload;
        procedure onViewDragStateChanged(para1: jint); overload; virtual;
        procedure onViewPositionChanged(para1: AVView; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
        procedure onViewCaptured(para1: AVView; para2: jint); overload; virtual;
        procedure onViewReleased(para1: AVView; para2: jfloat; para3: jfloat); overload; virtual;
        procedure onEdgeTouched(para1: jint; para2: jint); overload; virtual;
        function onEdgeLock(para1: jint): jboolean; overload; virtual;
        procedure onEdgeDragStarted(para1: jint; para2: jint); overload; virtual;
        function getOrderedChildIndex(para1: jint): jint; overload; virtual;
        function getViewHorizontalDragRange(para1: AVView): jint; overload; virtual;
        function getViewVerticalDragRange(para1: AVView): jint; overload; virtual;
        function tryCaptureView(para1: AVView; para2: jint): jboolean; overload; virtual; abstract;
        function clampViewPositionHorizontal(para1: AVView; para2: jint; para3: jint): jint; overload; virtual;
        function clampViewPositionVertical(para1: AVView; para2: jint; para3: jint): jint; overload; virtual;
      end;

  public
    const
      INVALID_POINTER = -1;
      STATE_IDLE = 0;
      STATE_DRAGGING = 1;
      STATE_SETTLING = 2;
      EDGE_LEFT = 1;
      EDGE_RIGHT = 2;
      EDGE_TOP = 4;
      EDGE_BOTTOM = 8;
      EDGE_ALL = 15;
      DIRECTION_HORIZONTAL = 1;
      DIRECTION_VERTICAL = 2;
      DIRECTION_ALL = 3;
  public
    class function create_(para1: AVViewGroup; para2: ASVWViewDragHelper.InnerCallback): ASVWViewDragHelper; static; external name 'create'; overload;
    class function create_(para1: AVViewGroup; para2: jfloat; para3: ASVWViewDragHelper.InnerCallback): ASVWViewDragHelper; static; external name 'create'; overload;
    procedure setMinVelocity(para1: jfloat); overload; virtual;
    function getMinVelocity(): jfloat; overload; virtual;
    function getViewDragState(): jint; overload; virtual;
    procedure setEdgeTrackingEnabled(para1: jint); overload; virtual;
    function getEdgeSize(): jint; overload; virtual;
    procedure captureChildView(para1: AVView; para2: jint); overload; virtual;
    function getCapturedView(): AVView; overload; virtual;
    function getActivePointerId(): jint; overload; virtual;
    function getTouchSlop(): jint; overload; virtual;
    procedure cancel(); overload; virtual;
    procedure abort(); overload; virtual;
    function smoothSlideViewTo(para1: AVView; para2: jint; para3: jint): jboolean; overload; virtual;
    function settleCapturedViewAt(para1: jint; para2: jint): jboolean; overload; virtual;
    procedure flingCapturedView(para1: jint; para2: jint; para3: jint; para4: jint); overload; virtual;
    function continueSettling(para1: jboolean): jboolean; overload; virtual;
    function isPointerDown(para1: jint): jboolean; overload; virtual;
  strict protected
    function canScroll(para1: AVView; para2: jboolean; para3: jint; para4: jint; para5: jint; para6: jint): jboolean; overload; virtual;
  public
    function shouldInterceptTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    procedure processTouchEvent(para1: AVMotionEvent); overload; virtual;
    function checkTouchSlop(para1: jint): jboolean; overload; virtual;
    function checkTouchSlop(para1: jint; para2: jint): jboolean; overload; virtual;
    function isEdgeTouched(para1: jint): jboolean; overload; virtual;
    function isEdgeTouched(para1: jint; para2: jint): jboolean; overload; virtual;
    function isCapturedViewUnder(para1: jint; para2: jint): jboolean; overload; virtual;
    function isViewUnder(para1: AVView; para2: jint; para3: jint): jboolean; overload; virtual;
    function findTopChildUnder(para1: jint; para2: jint): AVView; overload; virtual;
  end;

  ASV13AFragmentTabHost = class external 'android.support.v13.app' name 'FragmentTabHost' (AWTabHost, AWTabHost.InnerOnTabChangeListener)
  public
    type
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerTabInfo = class;
      Arr1InnerTabInfo = array of InnerTabInfo;
      Arr2InnerTabInfo = array of Arr1InnerTabInfo;
      Arr3InnerTabInfo = array of Arr2InnerTabInfo;
      InnerDummyTabFactory = class;
      Arr1InnerDummyTabFactory = array of InnerDummyTabFactory;
      Arr2InnerDummyTabFactory = array of Arr1InnerDummyTabFactory;
      Arr3InnerDummyTabFactory = array of Arr2InnerDummyTabFactory;
      InnerSavedState = class external 'android.support.v13.app' name 'SavedState' 
      end;

      InnerTabInfo = class sealed external 'android.support.v13.app' name 'TabInfo' 
      end;

      InnerDummyTabFactory = class external 'android.support.v13.app' name 'DummyTabFactory' 
      end;

  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    procedure setup(); overload; virtual;
    procedure setup(para1: ACContext; para2: AAFragmentManager); overload; virtual;
    procedure setup(para1: ACContext; para2: AAFragmentManager; para3: jint); overload; virtual;
    procedure setOnTabChangedListener(para1: AWTabHost.InnerOnTabChangeListener); overload; virtual;
    procedure addTab(para1: AWTabHost.InnerTabSpec; para2: JLClass; para3: AOBundle); overload; virtual;
  strict protected
    procedure onAttachedToWindow(); overload; virtual;
    procedure onDetachedFromWindow(); overload; virtual;
    function onSaveInstanceState(): AOParcelable; overload; virtual;
    procedure onRestoreInstanceState(para1: AOParcelable); overload; virtual;
  public
    procedure onTabChanged(para1: JLString); overload; virtual;
  end;

  ASV4AFragmentTabHost = class external 'android.support.v4.app' name 'FragmentTabHost' (AWTabHost, AWTabHost.InnerOnTabChangeListener)
  public
    type
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerTabInfo = class;
      Arr1InnerTabInfo = array of InnerTabInfo;
      Arr2InnerTabInfo = array of Arr1InnerTabInfo;
      Arr3InnerTabInfo = array of Arr2InnerTabInfo;
      InnerDummyTabFactory = class;
      Arr1InnerDummyTabFactory = array of InnerDummyTabFactory;
      Arr2InnerDummyTabFactory = array of Arr1InnerDummyTabFactory;
      Arr3InnerDummyTabFactory = array of Arr2InnerDummyTabFactory;
      InnerSavedState = class external 'android.support.v4.app' name 'SavedState' 
      end;

      InnerTabInfo = class sealed external 'android.support.v4.app' name 'TabInfo' 
      end;

      InnerDummyTabFactory = class external 'android.support.v4.app' name 'DummyTabFactory' 
      end;

  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    procedure setup(); overload; virtual;
    procedure setup(para1: ACContext; para2: ASVAFragmentManager); overload; virtual;
    procedure setup(para1: ACContext; para2: ASVAFragmentManager; para3: jint); overload; virtual;
    procedure setOnTabChangedListener(para1: AWTabHost.InnerOnTabChangeListener); overload; virtual;
    procedure addTab(para1: AWTabHost.InnerTabSpec; para2: JLClass; para3: AOBundle); overload; virtual;
  strict protected
    procedure onAttachedToWindow(); overload; virtual;
    procedure onDetachedFromWindow(); overload; virtual;
    function onSaveInstanceState(): AOParcelable; overload; virtual;
    procedure onRestoreInstanceState(para1: AOParcelable); overload; virtual;
  public
    procedure onTabChanged(para1: JLString); overload; virtual;
  end;

  ASVABackStackState = class sealed external 'android.support.v4.app' name 'BackStackState' (JLObject, AOParcelable)
  public
    final class var
      fCREATOR: AOParcelable.InnerCreator; external name 'CREATOR';
  public
    constructor create(para1: ASVAFragmentManagerImpl; para2: ASVABackStackRecord); overload;
    constructor create(para1: AOParcel); overload;
    function instantiate(para1: ASVAFragmentManagerImpl): ASVABackStackRecord; overload; virtual;
    function describeContents(): jint; overload; virtual;
    procedure writeToParcel(para1: AOParcel; para2: jint); overload; virtual;
  end;

  ASVAFragmentManagerState = class sealed external 'android.support.v4.app' name 'FragmentManagerState' (JLObject, AOParcelable)
  public
    final class var
      fCREATOR: AOParcelable.InnerCreator; external name 'CREATOR';
  public
    constructor create(); overload;
    constructor create(para1: AOParcel); overload;
    function describeContents(): jint; overload; virtual;
    procedure writeToParcel(para1: AOParcel; para2: jint); overload; virtual;
  end;

  ASVAFragmentState = class sealed external 'android.support.v4.app' name 'FragmentState' (JLObject, AOParcelable)
  public
    final class var
      fCREATOR: AOParcelable.InnerCreator; external name 'CREATOR';
  public
    constructor create(para1: ASVAFragment); overload;
    constructor create(para1: AOParcel); overload;
    function instantiate(para1: ASVAFragmentActivity; para2: ASVAFragment): ASVAFragment; overload; virtual;
    function describeContents(): jint; overload; virtual;
    procedure writeToParcel(para1: AOParcel; para2: jint); overload; virtual;
  end;

  ASVOParcelableCompat = class external 'android.support.v4.os' name 'ParcelableCompat' (JLObject)
  public
    type
      InnerCompatCreator = class;
      Arr1InnerCompatCreator = array of InnerCompatCreator;
      Arr2InnerCompatCreator = array of Arr1InnerCompatCreator;
      Arr3InnerCompatCreator = array of Arr2InnerCompatCreator;
      InnerCompatCreator = class external 'android.support.v4.os' name 'CompatCreator' 
      end;

  public
    constructor create(); overload;
    class function newCreator(para1: ASVOParcelableCompatCreatorCallbacks): AOParcelable.InnerCreator; static; overload;
  end;

  ASVOParcelableCompatCreatorHoneycombMR2 = class external 'android.support.v4.os' name 'ParcelableCompatCreatorHoneycombMR2' (JLObject, AOParcelable.InnerClassLoaderCreator)
  public
    constructor create(para1: ASVOParcelableCompatCreatorCallbacks); overload;
    function createFromParcel(para1: AOParcel): JLObject; overload; virtual;
    function createFromParcel(para1: AOParcel; para2: JLClassLoader): JLObject; overload; virtual;
    function newArray(para1: jint): Arr1JLObject; overload; virtual;
  end;

  ASVAFragment = class external 'android.support.v4.app' name 'Fragment' (JLObject, ACComponentCallbacks, AVView.InnerOnCreateContextMenuListener)
  public
    type
      InnerInstantiationException = class;
      Arr1InnerInstantiationException = array of InnerInstantiationException;
      Arr2InnerInstantiationException = array of Arr1InnerInstantiationException;
      Arr3InnerInstantiationException = array of Arr2InnerInstantiationException;
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerInstantiationException = class external 'android.support.v4.app' name 'InstantiationException' (JLRuntimeException)
      public
        constructor create(para1: JLString; para2: JLException); overload;
      end;

      InnerSavedState = class external 'android.support.v4.app' name 'SavedState' (JLObject, AOParcelable)
      public
        final class var
          fCREATOR: AOParcelable.InnerCreator; external name 'CREATOR';
      public
        function describeContents(): jint; overload; virtual;
        procedure writeToParcel(para1: AOParcel; para2: jint); overload; virtual;
      end;

  public
    constructor create(); overload;
    class function instantiate(para1: ACContext; para2: JLString): ASVAFragment; static; overload;
    class function instantiate(para1: ACContext; para2: JLString; para3: AOBundle): ASVAFragment; static; overload;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function hashCode(): jint; overload; virtual; final;
    function toString(): JLString; overload; virtual;
    function getId(): jint; overload; virtual; final;
    function getTag(): JLString; overload; virtual; final;
    procedure setArguments(para1: AOBundle); overload; virtual;
    function getArguments(): AOBundle; overload; virtual; final;
    procedure setInitialSavedState(para1: ASVAFragment.InnerSavedState); overload; virtual;
    procedure setTargetFragment(para1: ASVAFragment; para2: jint); overload; virtual;
    function getTargetFragment(): ASVAFragment; overload; virtual; final;
    function getTargetRequestCode(): jint; overload; virtual; final;
    function getActivity(): ASVAFragmentActivity; overload; virtual; final;
    function getResources(): ACRResources; overload; virtual; final;
    function getText(para1: jint): JLCharSequence; overload; virtual; final;
    function getString(para1: jint): JLString; overload; virtual; final;
    function getString(para1: jint; para2: Arr1JLObject): JLString; overload; virtual; final;
    function getString(para1: jint; const para2: array of JLObject): JLString; overload; virtual; final;
    function getFragmentManager(): ASVAFragmentManager; overload; virtual; final;
    function getChildFragmentManager(): ASVAFragmentManager; overload; virtual; final;
    function getParentFragment(): ASVAFragment; overload; virtual; final;
    function isAdded(): jboolean; overload; virtual; final;
    function isDetached(): jboolean; overload; virtual; final;
    function isRemoving(): jboolean; overload; virtual; final;
    function isInLayout(): jboolean; overload; virtual; final;
    function isResumed(): jboolean; overload; virtual; final;
    function isVisible(): jboolean; overload; virtual; final;
    function isHidden(): jboolean; overload; virtual; final;
    function hasOptionsMenu(): jboolean; overload; virtual; final;
    function isMenuVisible(): jboolean; overload; virtual; final;
    procedure onHiddenChanged(para1: jboolean); overload; virtual;
    procedure setRetainInstance(para1: jboolean); overload; virtual;
    function getRetainInstance(): jboolean; overload; virtual; final;
    procedure setHasOptionsMenu(para1: jboolean); overload; virtual;
    procedure setMenuVisibility(para1: jboolean); overload; virtual;
    procedure setUserVisibleHint(para1: jboolean); overload; virtual;
    function getUserVisibleHint(): jboolean; overload; virtual;
    function getLoaderManager(): ASVALoaderManager; overload; virtual;
    procedure startActivity(para1: ACIntent); overload; virtual;
    procedure startActivityForResult(para1: ACIntent; para2: jint); overload; virtual;
    procedure onActivityResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
    function getLayoutInflater(para1: AOBundle): AVLayoutInflater; overload; virtual;
    procedure onInflate(para1: AAActivity; para2: AUAttributeSet; para3: AOBundle); overload; virtual;
    procedure onAttach(para1: AAActivity); overload; virtual;
    function onCreateAnimation(para1: jint; para2: jboolean; para3: jint): AVAAnimation; overload; virtual;
    procedure onCreate(para1: AOBundle); overload; virtual;
    function onCreateView(para1: AVLayoutInflater; para2: AVViewGroup; para3: AOBundle): AVView; overload; virtual;
    procedure onViewCreated(para1: AVView; para2: AOBundle); overload; virtual;
    function getView(): AVView; overload; virtual;
    procedure onActivityCreated(para1: AOBundle); overload; virtual;
    procedure onViewStateRestored(para1: AOBundle); overload; virtual;
    procedure onStart(); overload; virtual;
    procedure onResume(); overload; virtual;
    procedure onSaveInstanceState(para1: AOBundle); overload; virtual;
    procedure onConfigurationChanged(para1: ACRConfiguration); overload; virtual;
    procedure onPause(); overload; virtual;
    procedure onStop(); overload; virtual;
    procedure onLowMemory(); overload; virtual;
    procedure onDestroyView(); overload; virtual;
    procedure onDestroy(); overload; virtual;
    procedure onDetach(); overload; virtual;
    procedure onCreateOptionsMenu(para1: AVMenu; para2: AVMenuInflater); overload; virtual;
    procedure onPrepareOptionsMenu(para1: AVMenu); overload; virtual;
    procedure onDestroyOptionsMenu(); overload; virtual;
    function onOptionsItemSelected(para1: AVMenuItem): jboolean; overload; virtual;
    procedure onOptionsMenuClosed(para1: AVMenu); overload; virtual;
    procedure onCreateContextMenu(para1: AVContextMenu; para2: AVView; para3: AVContextMenu.InnerContextMenuInfo); overload; virtual;
    procedure registerForContextMenu(para1: AVView); overload; virtual;
    procedure unregisterForContextMenu(para1: AVView); overload; virtual;
    function onContextItemSelected(para1: AVMenuItem): jboolean; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
  end;

  ASVAFragmentActivity = class external 'android.support.v4.app' name 'FragmentActivity' (AAActivity)
  public
    type
      InnerFragmentTag = class;
      Arr1InnerFragmentTag = array of InnerFragmentTag;
      Arr2InnerFragmentTag = array of Arr1InnerFragmentTag;
      Arr3InnerFragmentTag = array of Arr2InnerFragmentTag;
      InnerNonConfigurationInstances = class;
      Arr1InnerNonConfigurationInstances = array of InnerNonConfigurationInstances;
      Arr2InnerNonConfigurationInstances = array of Arr1InnerNonConfigurationInstances;
      Arr3InnerNonConfigurationInstances = array of Arr2InnerNonConfigurationInstances;
      InnerFragmentTag = class external 'android.support.v4.app' name 'FragmentTag' 
      end;

      InnerNonConfigurationInstances = class sealed external 'android.support.v4.app' name 'NonConfigurationInstances' 
      end;

  public
    constructor create(); overload;
  strict protected
    procedure onActivityResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
  public
    procedure onBackPressed(); overload; virtual;
    procedure onConfigurationChanged(para1: ACRConfiguration); overload; virtual;
  strict protected
    procedure onCreate(para1: AOBundle); overload; virtual;
  public
    function onCreatePanelMenu(para1: jint; para2: AVMenu): jboolean; overload; virtual;
    function onCreateView(para1: JLString; para2: ACContext; para3: AUAttributeSet): AVView; overload; virtual;
  strict protected
    procedure onDestroy(); overload; virtual;
  public
    function onKeyDown(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    procedure onLowMemory(); overload; virtual;
    function onMenuItemSelected(para1: jint; para2: AVMenuItem): jboolean; overload; virtual;
    procedure onPanelClosed(para1: jint; para2: AVMenu); overload; virtual;
  strict protected
    procedure onPause(); overload; virtual;
    procedure onNewIntent(para1: ACIntent); overload; virtual;
    procedure onResume(); overload; virtual;
    procedure onPostResume(); overload; virtual;
    procedure onResumeFragments(); overload; virtual;
  public
    function onPreparePanel(para1: jint; para2: AVView; para3: AVMenu): jboolean; overload; virtual;
    function onRetainNonConfigurationInstance(): JLObject; overload; virtual; final;
  strict protected
    procedure onSaveInstanceState(para1: AOBundle); overload; virtual;
    procedure onStart(); overload; virtual;
    procedure onStop(); overload; virtual;
  public
    function onRetainCustomNonConfigurationInstance(): JLObject; overload; virtual;
    function getLastCustomNonConfigurationInstance(): JLObject; overload; virtual;
    procedure supportInvalidateOptionsMenu(); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
    procedure onAttachFragment(para1: ASVAFragment); overload; virtual;
    function getSupportFragmentManager(): ASVAFragmentManager; overload; virtual;
    procedure startActivityForResult(para1: ACIntent; para2: jint); overload; virtual;
    procedure startActivityFromFragment(para1: ASVAFragment; para2: ACIntent; para3: jint); overload; virtual;
    function getSupportLoaderManager(): ASVALoaderManager; overload; virtual;
  end;

  ASVANoSaveStateFrameLayout = class external 'android.support.v4.app' name 'NoSaveStateFrameLayout' (AWFrameLayout)
  public
    constructor create(para1: ACContext); overload;
  strict protected
    procedure dispatchSaveInstanceState(para1: AUSparseArray); overload; virtual;
    procedure dispatchRestoreInstanceState(para1: AUSparseArray); overload; virtual;
  end;

  ASVASuperNotCalledException = class sealed external 'android.support.v4.app' name 'SuperNotCalledException' (AUAndroidRuntimeException)
  public
    constructor create(para1: JLString); overload;
  end;

  ASVATaskStackBuilder = class external 'android.support.v4.app' name 'TaskStackBuilder' (JLObject, JLIterable)
  public
    type
      InnerTaskStackBuilderImpl = interface;
      Arr1InnerTaskStackBuilderImpl = array of InnerTaskStackBuilderImpl;
      Arr2InnerTaskStackBuilderImpl = array of Arr1InnerTaskStackBuilderImpl;
      Arr3InnerTaskStackBuilderImpl = array of Arr2InnerTaskStackBuilderImpl;
      InnerTaskStackBuilderImplJellybean = class;
      Arr1InnerTaskStackBuilderImplJellybean = array of InnerTaskStackBuilderImplJellybean;
      Arr2InnerTaskStackBuilderImplJellybean = array of Arr1InnerTaskStackBuilderImplJellybean;
      Arr3InnerTaskStackBuilderImplJellybean = array of Arr2InnerTaskStackBuilderImplJellybean;
      InnerTaskStackBuilderImplHoneycomb = class;
      Arr1InnerTaskStackBuilderImplHoneycomb = array of InnerTaskStackBuilderImplHoneycomb;
      Arr2InnerTaskStackBuilderImplHoneycomb = array of Arr1InnerTaskStackBuilderImplHoneycomb;
      Arr3InnerTaskStackBuilderImplHoneycomb = array of Arr2InnerTaskStackBuilderImplHoneycomb;
      InnerTaskStackBuilderImplBase = class;
      Arr1InnerTaskStackBuilderImplBase = array of InnerTaskStackBuilderImplBase;
      Arr2InnerTaskStackBuilderImplBase = array of Arr1InnerTaskStackBuilderImplBase;
      Arr3InnerTaskStackBuilderImplBase = array of Arr2InnerTaskStackBuilderImplBase;
      InnerTaskStackBuilderImpl = interface external 'android.support.v4.app' name 'TaskStackBuilderImpl' 
      end;

      InnerTaskStackBuilderImplJellybean = class external 'android.support.v4.app' name 'TaskStackBuilderImplJellybean' 
      end;

      InnerTaskStackBuilderImplHoneycomb = class external 'android.support.v4.app' name 'TaskStackBuilderImplHoneycomb' 
      end;

      InnerTaskStackBuilderImplBase = class external 'android.support.v4.app' name 'TaskStackBuilderImplBase' 
      end;

  public
    type
      InnerSupportParentable = interface;
      Arr1InnerSupportParentable = array of InnerSupportParentable;
      Arr2InnerSupportParentable = array of Arr1InnerSupportParentable;
      Arr3InnerSupportParentable = array of Arr2InnerSupportParentable;
      InnerSupportParentable = interface external 'android.support.v4.app' name 'SupportParentable' 
        function getSupportParentActivityIntent(): ACIntent; overload;
      end;

  public
    class function create_(para1: ACContext): ASVATaskStackBuilder; static; external name 'create'; overload;
    class function from(para1: ACContext): ASVATaskStackBuilder; static; overload;
    function addNextIntent(para1: ACIntent): ASVATaskStackBuilder; overload; virtual;
    function addNextIntentWithParentStack(para1: ACIntent): ASVATaskStackBuilder; overload; virtual;
    function addParentStack(para1: AAActivity): ASVATaskStackBuilder; overload; virtual;
    function addParentStack(para1: JLClass): ASVATaskStackBuilder; overload; virtual;
    function addParentStack(para1: ACComponentName): ASVATaskStackBuilder; overload; virtual;
    function getIntentCount(): jint; overload; virtual;
    function getIntent(para1: jint): ACIntent; overload; virtual;
    function editIntentAt(para1: jint): ACIntent; overload; virtual;
    function iterator(): JUIterator; overload; virtual;
    procedure startActivities(); overload; virtual;
    procedure startActivities(para1: AOBundle); overload; virtual;
    function getPendingIntent(para1: jint; para2: jint): AAPendingIntent; overload; virtual;
    function getPendingIntent(para1: jint; para2: jint; para3: AOBundle): AAPendingIntent; overload; virtual;
    function getIntents(): Arr1ACIntent; overload; virtual;
  end;

  ASVCFileProvider = class external 'android.support.v4.content' name 'FileProvider' (ACContentProvider)
  public
    type
      InnerPathStrategy = interface;
      Arr1InnerPathStrategy = array of InnerPathStrategy;
      Arr2InnerPathStrategy = array of Arr1InnerPathStrategy;
      Arr3InnerPathStrategy = array of Arr2InnerPathStrategy;
      InnerSimplePathStrategy = class;
      Arr1InnerSimplePathStrategy = array of InnerSimplePathStrategy;
      Arr2InnerSimplePathStrategy = array of Arr1InnerSimplePathStrategy;
      Arr3InnerSimplePathStrategy = array of Arr2InnerSimplePathStrategy;
      InnerPathStrategy = interface external 'android.support.v4.content' name 'PathStrategy' 
      end;

      InnerSimplePathStrategy = class external 'android.support.v4.content' name 'SimplePathStrategy' 
      end;

  public
    constructor create(); overload;
    function onCreate(): jboolean; overload; virtual;
    procedure attachInfo(para1: ACContext; para2: ACPProviderInfo); overload; virtual;
    class function getUriForFile(para1: ACContext; para2: JLString; para3: JIFile): ANUri; static; overload;
    function query(para1: ANUri; para2: Arr1JLString; para3: JLString; para4: Arr1JLString; para5: JLString): ADCursor; overload; virtual;
    function query(para1: ANUri; var para2: array of JLString; para3: JLString; var para4: array of JLString; para5: JLString): ADCursor; overload; virtual;
    function getType(para1: ANUri): JLString; overload; virtual;
    function insert(para1: ANUri; para2: ACContentValues): ANUri; overload; virtual;
    function update(para1: ANUri; para2: ACContentValues; para3: JLString; para4: Arr1JLString): jint; overload; virtual;
    function update(para1: ANUri; para2: ACContentValues; para3: JLString; var para4: array of JLString): jint; overload; virtual;
    function delete(para1: ANUri; para2: JLString; para3: Arr1JLString): jint; overload; virtual;
    function delete(para1: ANUri; para2: JLString; var para3: array of JLString): jint; overload; virtual;
    function openFile(para1: ANUri; para2: JLString): AOParcelFileDescriptor; overload; virtual;  // throws java.io.FileNotFoundException
  end;

  ASVCModernAsyncTask = class abstract external 'android.support.v4.content' name 'ModernAsyncTask' (JLObject)
  public
    type
      InnerAsyncTaskResult = class;
      Arr1InnerAsyncTaskResult = array of InnerAsyncTaskResult;
      Arr2InnerAsyncTaskResult = array of Arr1InnerAsyncTaskResult;
      Arr3InnerAsyncTaskResult = array of Arr2InnerAsyncTaskResult;
      InnerWorkerRunnable = class;
      Arr1InnerWorkerRunnable = array of InnerWorkerRunnable;
      Arr2InnerWorkerRunnable = array of Arr1InnerWorkerRunnable;
      Arr3InnerWorkerRunnable = array of Arr2InnerWorkerRunnable;
      InnerInternalHandler = class;
      Arr1InnerInternalHandler = array of InnerInternalHandler;
      Arr2InnerInternalHandler = array of Arr1InnerInternalHandler;
      Arr3InnerInternalHandler = array of Arr2InnerInternalHandler;
      InnerAsyncTaskResult = class external 'android.support.v4.content' name 'AsyncTaskResult' 
      end;

      InnerWorkerRunnable = class abstract external 'android.support.v4.content' name 'WorkerRunnable' 
      end;

      InnerInternalHandler = class external 'android.support.v4.content' name 'InternalHandler' 
      end;

  public
    type
      InnerStatus = class;
      Arr1InnerStatus = array of InnerStatus;
      Arr2InnerStatus = array of Arr1InnerStatus;
      Arr3InnerStatus = array of Arr2InnerStatus;
      InnerStatus = class sealed external 'android.support.v4.content' name 'Status' (JLEnum)
      public
        final class var
          fPENDING: ASVCModernAsyncTask.InnerStatus; external name 'PENDING';
          fRUNNING: ASVCModernAsyncTask.InnerStatus; external name 'RUNNING';
          fFINISHED: ASVCModernAsyncTask.InnerStatus; external name 'FINISHED';
      public
        class function values(): ASVCModernAsyncTask.Arr1InnerStatus; static; overload;
        class function valueOf(para1: JLString): ASVCModernAsyncTask.InnerStatus; static; overload;
      end;

  public
    final class var
      fTHREAD_POOL_EXECUTOR: JUCExecutor; external name 'THREAD_POOL_EXECUTOR';
  public
    class procedure init(); static; overload;
    class procedure setDefaultExecutor(para1: JUCExecutor); static; overload;
    constructor create(); overload;
    function getStatus(): ASVCModernAsyncTask.InnerStatus; overload; virtual; final;
  strict protected
    function doInBackground(para1: Arr1JLObject): JLObject; overload; virtual; abstract;
    function doInBackground(const para1: array of JLObject): JLObject; overload; virtual; abstract;
    procedure onPreExecute(); overload; virtual;
    procedure onPostExecute(para1: JLObject); overload; virtual;
    procedure onProgressUpdate(para1: Arr1JLObject); overload; virtual;
    procedure onProgressUpdate(const para1: array of JLObject); overload; virtual;
    procedure onCancelled(para1: JLObject); overload; virtual;
    procedure onCancelled(); overload; virtual;
  public
    function isCancelled(): jboolean; overload; virtual; final;
    function cancel(para1: jboolean): jboolean; overload; virtual; final;
    function get(): JLObject; overload; virtual; final;  // throws java.lang.InterruptedException, java.util.concurrent.ExecutionException
    function get(para1: jlong; para2: JUCTimeUnit): JLObject; overload; virtual; final;  // throws java.lang.InterruptedException, java.util.concurrent.ExecutionException, java.util.concurrent.TimeoutException
    function execute(para1: Arr1JLObject): ASVCModernAsyncTask; overload; virtual; final;
    function execute(const para1: array of JLObject): ASVCModernAsyncTask; overload; virtual; final;
    function executeOnExecutor(para1: JUCExecutor; para2: Arr1JLObject): ASVCModernAsyncTask; overload; virtual; final;
    function executeOnExecutor(para1: JUCExecutor; const para2: array of JLObject): ASVCModernAsyncTask; overload; virtual; final;
    class procedure execute(para1: JLRunnable); static; overload;
  strict protected
    procedure publishProgress(para1: Arr1JLObject); overload; virtual; final;
    procedure publishProgress(const para1: array of JLObject); overload; virtual; final;
  end;

  ASVCWakefulBroadcastReceiver = class abstract external 'android.support.v4.content' name 'WakefulBroadcastReceiver' (ACBroadcastReceiver)
  public
    constructor create(); overload;
    class function startWakefulService(para1: ACContext; para2: ACIntent): ACComponentName; static; overload;
    class function completeWakefulIntent(para1: ACIntent): jboolean; static; overload;
  end;

  ASVIVSupportMenu = interface external 'android.support.v4.internal.view' name 'SupportMenu' (AVMenu)
    const
      USER_MASK = 65535;
      USER_SHIFT = 0;
      CATEGORY_MASK = -65536;
      CATEGORY_SHIFT = 16;
  end;

  ASVMTransportMediatorJellybeanMR2 = class external 'android.support.v4.media' name 'TransportMediatorJellybeanMR2' (JLObject {, AMRemoteControlClient.InnerOnGetPlaybackPositionListener, AMRemoteControlClient.InnerOnPlaybackPositionUpdateListener})
  public
    constructor create(para1: ACContext; para2: AMAudioManager; para3: AVView; para4: ASVMTransportMediatorCallback); overload;
    function getRemoteControlClient(): JLObject; overload; virtual;
    procedure destroy(); overload; virtual;
    procedure startPlaying(); overload; virtual;
    function onGetPlaybackPosition(): jlong; overload; virtual;
    procedure onPlaybackPositionUpdate(para1: jlong); overload; virtual;
    procedure refreshState(para1: jboolean; para2: jlong; para3: jint); overload; virtual;
    procedure pausePlaying(); overload; virtual;
    procedure stopPlaying(); overload; virtual;
  end;

  ASVULogWriter = class external 'android.support.v4.util' name 'LogWriter' (JIWriter)
  public
    constructor create(para1: JLString); overload;
    procedure close(); overload; virtual;
    procedure flush(); overload; virtual;
    procedure write(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;
    procedure write(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;
  end;

  ASVULongSparseArray = class external 'android.support.v4.util' name 'LongSparseArray' (JLObject, JLCloneable)
  public
    constructor create(); overload;
    constructor create(para1: jint); overload;
    function clone(): ASVULongSparseArray; overload; virtual;
    function get(para1: jlong): JLObject; overload; virtual;
    function get(para1: jlong; para2: JLObject): JLObject; overload; virtual;
    procedure delete(para1: jlong); overload; virtual;
    procedure remove(para1: jlong); overload; virtual;
    procedure removeAt(para1: jint); overload; virtual;
    procedure put(para1: jlong; para2: JLObject); overload; virtual;
    function size(): jint; overload; virtual;
    function keyAt(para1: jint): jlong; overload; virtual;
    function valueAt(para1: jint): JLObject; overload; virtual;
    procedure setValueAt(para1: jint; para2: JLObject); overload; virtual;
    function indexOfKey(para1: jlong): jint; overload; virtual;
    function indexOfValue(para1: JLObject): jint; overload; virtual;
    procedure clear(); overload; virtual;
    procedure append(para1: jlong; para2: JLObject); overload; virtual;
    class function idealByteArraySize(para1: jint): jint; static; overload;
    class function idealLongArraySize(para1: jint): jint; static; overload;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  ASVVGestureDetectorCompat = class external 'android.support.v4.view' name 'GestureDetectorCompat' (JLObject)
  public
    type
      InnerGestureDetectorCompatImpl = interface;
      Arr1InnerGestureDetectorCompatImpl = array of InnerGestureDetectorCompatImpl;
      Arr2InnerGestureDetectorCompatImpl = array of Arr1InnerGestureDetectorCompatImpl;
      Arr3InnerGestureDetectorCompatImpl = array of Arr2InnerGestureDetectorCompatImpl;
      InnerGestureDetectorCompatImplJellybeanMr2 = class;
      Arr1InnerGestureDetectorCompatImplJellybeanMr2 = array of InnerGestureDetectorCompatImplJellybeanMr2;
      Arr2InnerGestureDetectorCompatImplJellybeanMr2 = array of Arr1InnerGestureDetectorCompatImplJellybeanMr2;
      Arr3InnerGestureDetectorCompatImplJellybeanMr2 = array of Arr2InnerGestureDetectorCompatImplJellybeanMr2;
      InnerGestureDetectorCompatImplBase = class;
      Arr1InnerGestureDetectorCompatImplBase = array of InnerGestureDetectorCompatImplBase;
      Arr2InnerGestureDetectorCompatImplBase = array of Arr1InnerGestureDetectorCompatImplBase;
      Arr3InnerGestureDetectorCompatImplBase = array of Arr2InnerGestureDetectorCompatImplBase;
      InnerGestureDetectorCompatImpl = interface external 'android.support.v4.view' name 'GestureDetectorCompatImpl' 
      end;

      InnerGestureDetectorCompatImplJellybeanMr2 = class external 'android.support.v4.view' name 'GestureDetectorCompatImplJellybeanMr2' 
      end;

      InnerGestureDetectorCompatImplBase = class external 'android.support.v4.view' name 'GestureDetectorCompatImplBase' 
      public
        type
          InnerGestureHandler = class;
          Arr1InnerGestureHandler = array of InnerGestureHandler;
          Arr2InnerGestureHandler = array of Arr1InnerGestureHandler;
          Arr3InnerGestureHandler = array of Arr2InnerGestureHandler;
          InnerGestureHandler = class external 'android.support.v4.view' name 'GestureHandler' 
          end;

      end;

  public
    constructor create(para1: ACContext; para2: AVGestureDetector.InnerOnGestureListener); overload;
    constructor create(para1: ACContext; para2: AVGestureDetector.InnerOnGestureListener; para3: AOHandler); overload;
    function isLongpressEnabled(): jboolean; overload; virtual;
    function onTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    procedure setIsLongpressEnabled(para1: jboolean); overload; virtual;
    procedure setOnDoubleTapListener(para1: AVGestureDetector.InnerOnDoubleTapListener); overload; virtual;
  end;

  ASVVKeyEventCompat = class external 'android.support.v4.view' name 'KeyEventCompat' (JLObject)
  public
    type
      InnerHoneycombKeyEventVersionImpl = class;
      Arr1InnerHoneycombKeyEventVersionImpl = array of InnerHoneycombKeyEventVersionImpl;
      Arr2InnerHoneycombKeyEventVersionImpl = array of Arr1InnerHoneycombKeyEventVersionImpl;
      Arr3InnerHoneycombKeyEventVersionImpl = array of Arr2InnerHoneycombKeyEventVersionImpl;
      InnerEclairKeyEventVersionImpl = class;
      Arr1InnerEclairKeyEventVersionImpl = array of InnerEclairKeyEventVersionImpl;
      Arr2InnerEclairKeyEventVersionImpl = array of Arr1InnerEclairKeyEventVersionImpl;
      Arr3InnerEclairKeyEventVersionImpl = array of Arr2InnerEclairKeyEventVersionImpl;
      InnerBaseKeyEventVersionImpl = class;
      Arr1InnerBaseKeyEventVersionImpl = array of InnerBaseKeyEventVersionImpl;
      Arr2InnerBaseKeyEventVersionImpl = array of Arr1InnerBaseKeyEventVersionImpl;
      Arr3InnerBaseKeyEventVersionImpl = array of Arr2InnerBaseKeyEventVersionImpl;
      InnerKeyEventVersionImpl = interface;
      Arr1InnerKeyEventVersionImpl = array of InnerKeyEventVersionImpl;
      Arr2InnerKeyEventVersionImpl = array of Arr1InnerKeyEventVersionImpl;
      Arr3InnerKeyEventVersionImpl = array of Arr2InnerKeyEventVersionImpl;
      InnerHoneycombKeyEventVersionImpl = class external 'android.support.v4.view' name 'HoneycombKeyEventVersionImpl' 
      end;

      InnerEclairKeyEventVersionImpl = class external 'android.support.v4.view' name 'EclairKeyEventVersionImpl' 
      end;

      InnerBaseKeyEventVersionImpl = class external 'android.support.v4.view' name 'BaseKeyEventVersionImpl' 
      end;

      InnerKeyEventVersionImpl = interface external 'android.support.v4.view' name 'KeyEventVersionImpl' 
      end;

  public
    constructor create(); overload;
    class function normalizeMetaState(para1: jint): jint; static; overload;
    class function metaStateHasModifiers(para1: jint; para2: jint): jboolean; static; overload;
    class function metaStateHasNoModifiers(para1: jint): jboolean; static; overload;
    class function hasModifiers(para1: AVKeyEvent; para2: jint): jboolean; static; overload;
    class function hasNoModifiers(para1: AVKeyEvent): jboolean; static; overload;
    class procedure startTracking(para1: AVKeyEvent); static; overload;
    class function isTracking(para1: AVKeyEvent): jboolean; static; overload;
    class function getKeyDispatcherState(para1: AVView): JLObject; static; overload;
    class function dispatch(para1: AVKeyEvent; para2: AVKeyEvent.InnerCallback; para3: JLObject; para4: JLObject): jboolean; static; overload;
  end;

  ASVVKeyEventCompatEclair = class external 'android.support.v4.view' name 'KeyEventCompatEclair' (JLObject)
  public
    class function getKeyDispatcherState(para1: AVView): JLObject; static; overload;
    class function dispatch(para1: AVKeyEvent; para2: AVKeyEvent.InnerCallback; para3: JLObject; para4: JLObject): jboolean; static; overload;
    class procedure startTracking(para1: AVKeyEvent); static; overload;
    class function isTracking(para1: AVKeyEvent): jboolean; static; overload;
  end;

  ASVVMarginLayoutParamsCompat = class external 'android.support.v4.view' name 'MarginLayoutParamsCompat' (JLObject)
  public
    type
      InnerMarginLayoutParamsCompatImplJbMr1 = class;
      Arr1InnerMarginLayoutParamsCompatImplJbMr1 = array of InnerMarginLayoutParamsCompatImplJbMr1;
      Arr2InnerMarginLayoutParamsCompatImplJbMr1 = array of Arr1InnerMarginLayoutParamsCompatImplJbMr1;
      Arr3InnerMarginLayoutParamsCompatImplJbMr1 = array of Arr2InnerMarginLayoutParamsCompatImplJbMr1;
      InnerMarginLayoutParamsCompatImplBase = class;
      Arr1InnerMarginLayoutParamsCompatImplBase = array of InnerMarginLayoutParamsCompatImplBase;
      Arr2InnerMarginLayoutParamsCompatImplBase = array of Arr1InnerMarginLayoutParamsCompatImplBase;
      Arr3InnerMarginLayoutParamsCompatImplBase = array of Arr2InnerMarginLayoutParamsCompatImplBase;
      InnerMarginLayoutParamsCompatImpl = interface;
      Arr1InnerMarginLayoutParamsCompatImpl = array of InnerMarginLayoutParamsCompatImpl;
      Arr2InnerMarginLayoutParamsCompatImpl = array of Arr1InnerMarginLayoutParamsCompatImpl;
      Arr3InnerMarginLayoutParamsCompatImpl = array of Arr2InnerMarginLayoutParamsCompatImpl;
      InnerMarginLayoutParamsCompatImplJbMr1 = class external 'android.support.v4.view' name 'MarginLayoutParamsCompatImplJbMr1' 
      end;

      InnerMarginLayoutParamsCompatImplBase = class external 'android.support.v4.view' name 'MarginLayoutParamsCompatImplBase' 
      end;

      InnerMarginLayoutParamsCompatImpl = interface external 'android.support.v4.view' name 'MarginLayoutParamsCompatImpl' 
      end;

  public
    constructor create(); overload;
    class function getMarginStart(para1: AVViewGroup.InnerMarginLayoutParams): jint; static; overload;
    class function getMarginEnd(para1: AVViewGroup.InnerMarginLayoutParams): jint; static; overload;
    class procedure setMarginStart(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
    class procedure setMarginEnd(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
    class function isMarginRelative(para1: AVViewGroup.InnerMarginLayoutParams): jboolean; static; overload;
    class function getLayoutDirection(para1: AVViewGroup.InnerMarginLayoutParams): jint; static; overload;
    class procedure setLayoutDirection(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
    class procedure resolveLayoutDirection(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
  end;

  ASVVMarginLayoutParamsCompatJellybeanMr1 = class external 'android.support.v4.view' name 'MarginLayoutParamsCompatJellybeanMr1' (JLObject)
  public
    class function getMarginStart(para1: AVViewGroup.InnerMarginLayoutParams): jint; static; overload;
    class function getMarginEnd(para1: AVViewGroup.InnerMarginLayoutParams): jint; static; overload;
    class procedure setMarginStart(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
    class procedure setMarginEnd(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
    class function isMarginRelative(para1: AVViewGroup.InnerMarginLayoutParams): jboolean; static; overload;
    class function getLayoutDirection(para1: AVViewGroup.InnerMarginLayoutParams): jint; static; overload;
    class procedure setLayoutDirection(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
    class procedure resolveLayoutDirection(para1: AVViewGroup.InnerMarginLayoutParams; para2: jint); static; overload;
  end;

  ASVVViewPager = class external 'android.support.v4.view' name 'ViewPager' (AVViewGroup)
  public
    type
      InnerDecor = interface;
      Arr1InnerDecor = array of InnerDecor;
      Arr2InnerDecor = array of Arr1InnerDecor;
      Arr3InnerDecor = array of Arr2InnerDecor;
      InnerOnAdapterChangeListener = interface;
      Arr1InnerOnAdapterChangeListener = array of InnerOnAdapterChangeListener;
      Arr2InnerOnAdapterChangeListener = array of Arr1InnerOnAdapterChangeListener;
      Arr3InnerOnAdapterChangeListener = array of Arr2InnerOnAdapterChangeListener;
      InnerItemInfo = class;
      Arr1InnerItemInfo = array of InnerItemInfo;
      Arr2InnerItemInfo = array of Arr1InnerItemInfo;
      Arr3InnerItemInfo = array of Arr2InnerItemInfo;
      InnerViewPositionComparator = class;
      Arr1InnerViewPositionComparator = array of InnerViewPositionComparator;
      Arr2InnerViewPositionComparator = array of Arr1InnerViewPositionComparator;
      Arr3InnerViewPositionComparator = array of Arr2InnerViewPositionComparator;
      InnerPagerObserver = class;
      Arr1InnerPagerObserver = array of InnerPagerObserver;
      Arr2InnerPagerObserver = array of Arr1InnerPagerObserver;
      Arr3InnerPagerObserver = array of Arr2InnerPagerObserver;
      InnerMyAccessibilityDelegate = class;
      Arr1InnerMyAccessibilityDelegate = array of InnerMyAccessibilityDelegate;
      Arr2InnerMyAccessibilityDelegate = array of Arr1InnerMyAccessibilityDelegate;
      Arr3InnerMyAccessibilityDelegate = array of Arr2InnerMyAccessibilityDelegate;
      InnerDecor = interface external 'android.support.v4.view' name 'Decor' 
      end;

      InnerOnAdapterChangeListener = interface external 'android.support.v4.view' name 'OnAdapterChangeListener' 
      end;

      InnerItemInfo = class external 'android.support.v4.view' name 'ItemInfo' 
      end;

      InnerViewPositionComparator = class external 'android.support.v4.view' name 'ViewPositionComparator' 
      end;

      InnerPagerObserver = class external 'android.support.v4.view' name 'PagerObserver' 
      end;

      InnerMyAccessibilityDelegate = class external 'android.support.v4.view' name 'MyAccessibilityDelegate' 
      end;

  public
    type
      InnerPageTransformer = interface;
      Arr1InnerPageTransformer = array of InnerPageTransformer;
      Arr2InnerPageTransformer = array of Arr1InnerPageTransformer;
      Arr3InnerPageTransformer = array of Arr2InnerPageTransformer;
      InnerOnPageChangeListener = interface;
      Arr1InnerOnPageChangeListener = array of InnerOnPageChangeListener;
      Arr2InnerOnPageChangeListener = array of Arr1InnerOnPageChangeListener;
      Arr3InnerOnPageChangeListener = array of Arr2InnerOnPageChangeListener;
      InnerLayoutParams = class;
      Arr1InnerLayoutParams = array of InnerLayoutParams;
      Arr2InnerLayoutParams = array of Arr1InnerLayoutParams;
      Arr3InnerLayoutParams = array of Arr2InnerLayoutParams;
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerSimpleOnPageChangeListener = class;
      Arr1InnerSimpleOnPageChangeListener = array of InnerSimpleOnPageChangeListener;
      Arr2InnerSimpleOnPageChangeListener = array of Arr1InnerSimpleOnPageChangeListener;
      Arr3InnerSimpleOnPageChangeListener = array of Arr2InnerSimpleOnPageChangeListener;
      InnerPageTransformer = interface external 'android.support.v4.view' name 'PageTransformer' 
        procedure transformPage(para1: AVView; para2: jfloat); overload;
      end;

      InnerOnPageChangeListener = interface external 'android.support.v4.view' name 'OnPageChangeListener' 
        procedure onPageScrolled(para1: jint; para2: jfloat; para3: jint); overload;
        procedure onPageSelected(para1: jint); overload;
        procedure onPageScrollStateChanged(para1: jint); overload;
      end;

      InnerLayoutParams = class external 'android.support.v4.view' name 'LayoutParams' (AVViewGroup.InnerLayoutParams)
      public
        var
          fisDecor: jboolean; external name 'isDecor';
          fgravity: jint; external name 'gravity';
      public
        constructor create(); overload;
        constructor create(para1: ACContext; para2: AUAttributeSet); overload;
      end;

      InnerSavedState = class external 'android.support.v4.view' name 'SavedState' (AVView.InnerBaseSavedState)
      public
        final class var
          fCREATOR: AOParcelable.InnerCreator; external name 'CREATOR';
      public
        constructor create(para1: AOParcelable); overload;
        procedure writeToParcel(para1: AOParcel; para2: jint); overload; virtual;
        function toString(): JLString; overload; virtual;
      end;

      InnerSimpleOnPageChangeListener = class external 'android.support.v4.view' name 'SimpleOnPageChangeListener' (JLObject, ASVVViewPager.InnerOnPageChangeListener)
      public
        constructor create(); overload;
        procedure onPageScrolled(para1: jint; para2: jfloat; para3: jint); overload; virtual;
        procedure onPageSelected(para1: jint); overload; virtual;
        procedure onPageScrollStateChanged(para1: jint); overload; virtual;
      end;

  public
    const
      SCROLL_STATE_IDLE = 0;
      SCROLL_STATE_DRAGGING = 1;
      SCROLL_STATE_SETTLING = 2;
  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
  strict protected
    procedure onDetachedFromWindow(); overload; virtual;
  public
    procedure setAdapter(para1: ASVVPagerAdapter); overload; virtual;
    function getAdapter(): ASVVPagerAdapter; overload; virtual;
    procedure setCurrentItem(para1: jint); overload; virtual;
    procedure setCurrentItem(para1: jint; para2: jboolean); overload; virtual;
    function getCurrentItem(): jint; overload; virtual;
    procedure setOnPageChangeListener(para1: ASVVViewPager.InnerOnPageChangeListener); overload; virtual;
    procedure setPageTransformer(para1: jboolean; para2: ASVVViewPager.InnerPageTransformer); overload; virtual;
  strict protected
    function getChildDrawingOrder(para1: jint; para2: jint): jint; overload; virtual;
  public
    function getOffscreenPageLimit(): jint; overload; virtual;
    procedure setOffscreenPageLimit(para1: jint); overload; virtual;
    procedure setPageMargin(para1: jint); overload; virtual;
    function getPageMargin(): jint; overload; virtual;
    procedure setPageMarginDrawable(para1: AGDDrawable); overload; virtual;
    procedure setPageMarginDrawable(para1: jint); overload; virtual;
  strict protected
    function verifyDrawable(para1: AGDDrawable): jboolean; overload; virtual;
    procedure drawableStateChanged(); overload; virtual;
  public
    function onSaveInstanceState(): AOParcelable; overload; virtual;
    procedure onRestoreInstanceState(para1: AOParcelable); overload; virtual;
    procedure addView(para1: AVView; para2: jint; para3: AVViewGroup.InnerLayoutParams); overload; virtual;
    procedure removeView(para1: AVView); overload; virtual;
  strict protected
    procedure onAttachedToWindow(); overload; virtual;
    procedure onMeasure(para1: jint; para2: jint); overload; virtual;
    procedure onSizeChanged(para1: jint; para2: jint; para3: jint; para4: jint); overload; virtual;
    procedure onLayout(para1: jboolean; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
  public
    procedure computeScroll(); overload; virtual;
  strict protected
    procedure onPageScrolled(para1: jint; para2: jfloat; para3: jint); overload; virtual;
  public
    function onInterceptTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    function onTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    procedure draw(para1: AGCanvas); overload; virtual;
  strict protected
    procedure onDraw(para1: AGCanvas); overload; virtual;
  public
    function beginFakeDrag(): jboolean; overload; virtual;
    procedure endFakeDrag(); overload; virtual;
    procedure fakeDragBy(para1: jfloat); overload; virtual;
    function isFakeDragging(): jboolean; overload; virtual;
    function canScrollHorizontally(para1: jint): jboolean; overload; virtual;
  strict protected
    function canScroll(para1: AVView; para2: jboolean; para3: jint; para4: jint; para5: jint): jboolean; overload; virtual;
  public
    function dispatchKeyEvent(para1: AVKeyEvent): jboolean; overload; virtual;
    function executeKeyEvent(para1: AVKeyEvent): jboolean; overload; virtual;
    function arrowScroll(para1: jint): jboolean; overload; virtual;
    procedure addFocusables(para1: JUArrayList; para2: jint; para3: jint); overload; virtual;
    procedure addTouchables(para1: JUArrayList); overload; virtual;
  strict protected
    function onRequestFocusInDescendants(para1: jint; para2: AGRect): jboolean; overload; virtual;
  public
    function dispatchPopulateAccessibilityEvent(para1: AVAAccessibilityEvent): jboolean; overload; virtual;
  strict protected
    function generateDefaultLayoutParams(): AVViewGroup.InnerLayoutParams; overload; virtual;
    function generateLayoutParams(para1: AVViewGroup.InnerLayoutParams): AVViewGroup.InnerLayoutParams; overload; virtual;
    function checkLayoutParams(para1: AVViewGroup.InnerLayoutParams): jboolean; overload; virtual;
  public
    function generateLayoutParams(para1: AUAttributeSet): AVViewGroup.InnerLayoutParams; overload; virtual;
  end;

  ASVWDrawerLayout = class external 'android.support.v4.widget' name 'DrawerLayout' (AVViewGroup)
  public
    type
      InnerAccessibilityDelegate = class;
      Arr1InnerAccessibilityDelegate = array of InnerAccessibilityDelegate;
      Arr2InnerAccessibilityDelegate = array of Arr1InnerAccessibilityDelegate;
      Arr3InnerAccessibilityDelegate = array of Arr2InnerAccessibilityDelegate;
      InnerViewDragCallback = class;
      Arr1InnerViewDragCallback = array of InnerViewDragCallback;
      Arr2InnerViewDragCallback = array of Arr1InnerViewDragCallback;
      Arr3InnerViewDragCallback = array of Arr2InnerViewDragCallback;
      InnerAccessibilityDelegate = class external 'android.support.v4.widget' name 'AccessibilityDelegate' 
      end;

      InnerViewDragCallback = class external 'android.support.v4.widget' name 'ViewDragCallback' 
      end;

  public
    type
      InnerLayoutParams = class;
      Arr1InnerLayoutParams = array of InnerLayoutParams;
      Arr2InnerLayoutParams = array of Arr1InnerLayoutParams;
      Arr3InnerLayoutParams = array of Arr2InnerLayoutParams;
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerDrawerListener = interface;
      Arr1InnerDrawerListener = array of InnerDrawerListener;
      Arr2InnerDrawerListener = array of Arr1InnerDrawerListener;
      Arr3InnerDrawerListener = array of Arr2InnerDrawerListener;
      InnerSimpleDrawerListener = class;
      Arr1InnerSimpleDrawerListener = array of InnerSimpleDrawerListener;
      Arr2InnerSimpleDrawerListener = array of Arr1InnerSimpleDrawerListener;
      Arr3InnerSimpleDrawerListener = array of Arr2InnerSimpleDrawerListener;
      InnerLayoutParams = class external 'android.support.v4.widget' name 'LayoutParams' (AVViewGroup.InnerMarginLayoutParams)
      public
        var
          fgravity: jint; external name 'gravity';
      public
        constructor create(para1: ACContext; para2: AUAttributeSet); overload;
        constructor create(para1: jint; para2: jint); overload;
        constructor create(para1: jint; para2: jint; para3: jint); overload;
        constructor create(para1: ASVWDrawerLayout.InnerLayoutParams); overload;
        constructor create(para1: AVViewGroup.InnerLayoutParams); overload;
        constructor create(para1: AVViewGroup.InnerMarginLayoutParams); overload;
      end;

      InnerSavedState = class external 'android.support.v4.widget' name 'SavedState' (AVView.InnerBaseSavedState)
      public
        final class var
          fCREATOR: AOParcelable.InnerCreator; external name 'CREATOR';
      public
        constructor create(para1: AOParcel); overload;
        constructor create(para1: AOParcelable); overload;
        procedure writeToParcel(para1: AOParcel; para2: jint); overload; virtual;
      end;

      InnerDrawerListener = interface external 'android.support.v4.widget' name 'DrawerListener' 
        procedure onDrawerSlide(para1: AVView; para2: jfloat); overload;
        procedure onDrawerOpened(para1: AVView); overload;
        procedure onDrawerClosed(para1: AVView); overload;
        procedure onDrawerStateChanged(para1: jint); overload;
      end;

      InnerSimpleDrawerListener = class abstract external 'android.support.v4.widget' name 'SimpleDrawerListener' (JLObject, ASVWDrawerLayout.InnerDrawerListener)
      public
        constructor create(); overload;
        procedure onDrawerSlide(para1: AVView; para2: jfloat); overload; virtual;
        procedure onDrawerOpened(para1: AVView); overload; virtual;
        procedure onDrawerClosed(para1: AVView); overload; virtual;
        procedure onDrawerStateChanged(para1: jint); overload; virtual;
      end;

  public
    const
      STATE_IDLE = 0;
      STATE_DRAGGING = 1;
      STATE_SETTLING = 2;
      LOCK_MODE_UNLOCKED = 0;
      LOCK_MODE_LOCKED_CLOSED = 1;
      LOCK_MODE_LOCKED_OPEN = 2;
  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet; para3: jint); overload;
    procedure setDrawerShadow(para1: AGDDrawable; para2: jint); overload; virtual;
    procedure setDrawerShadow(para1: jint; para2: jint); overload; virtual;
    procedure setScrimColor(para1: jint); overload; virtual;
    procedure setDrawerListener(para1: ASVWDrawerLayout.InnerDrawerListener); overload; virtual;
    procedure setDrawerLockMode(para1: jint); overload; virtual;
    procedure setDrawerLockMode(para1: jint; para2: jint); overload; virtual;
    procedure setDrawerLockMode(para1: jint; para2: AVView); overload; virtual;
    function getDrawerLockMode(para1: jint): jint; overload; virtual;
    function getDrawerLockMode(para1: AVView): jint; overload; virtual;
  strict protected
    procedure onDetachedFromWindow(); overload; virtual;
    procedure onAttachedToWindow(); overload; virtual;
    procedure onMeasure(para1: jint; para2: jint); overload; virtual;
    procedure onLayout(para1: jboolean; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
  public
    procedure requestLayout(); overload; virtual;
    procedure computeScroll(); overload; virtual;
  strict protected
    function drawChild(para1: AGCanvas; para2: AVView; para3: jlong): jboolean; overload; virtual;
  public
    function onInterceptTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    function onTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    procedure requestDisallowInterceptTouchEvent(para1: jboolean); overload; virtual;
    procedure closeDrawers(); overload; virtual;
    procedure openDrawer(para1: AVView); overload; virtual;
    procedure openDrawer(para1: jint); overload; virtual;
    procedure closeDrawer(para1: AVView); overload; virtual;
    procedure closeDrawer(para1: jint); overload; virtual;
    function isDrawerOpen(para1: AVView): jboolean; overload; virtual;
    function isDrawerOpen(para1: jint): jboolean; overload; virtual;
    function isDrawerVisible(para1: AVView): jboolean; overload; virtual;
    function isDrawerVisible(para1: jint): jboolean; overload; virtual;
  strict protected
    function generateDefaultLayoutParams(): AVViewGroup.InnerLayoutParams; overload; virtual;
    function generateLayoutParams(para1: AVViewGroup.InnerLayoutParams): AVViewGroup.InnerLayoutParams; overload; virtual;
    function checkLayoutParams(para1: AVViewGroup.InnerLayoutParams): jboolean; overload; virtual;
  public
    function generateLayoutParams(para1: AUAttributeSet): AVViewGroup.InnerLayoutParams; overload; virtual;
    function onKeyDown(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    function onKeyUp(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
  strict protected
    procedure onRestoreInstanceState(para1: AOParcelable); overload; virtual;
    function onSaveInstanceState(): AOParcelable; overload; virtual;
  end;

  ASVWSlidingPaneLayout = class external 'android.support.v4.widget' name 'SlidingPaneLayout' (AVViewGroup)
  public
    type
      InnerSlidingPanelLayoutImpl = interface;
      Arr1InnerSlidingPanelLayoutImpl = array of InnerSlidingPanelLayoutImpl;
      Arr2InnerSlidingPanelLayoutImpl = array of Arr1InnerSlidingPanelLayoutImpl;
      Arr3InnerSlidingPanelLayoutImpl = array of Arr2InnerSlidingPanelLayoutImpl;
      InnerDisableLayerRunnable = class;
      Arr1InnerDisableLayerRunnable = array of InnerDisableLayerRunnable;
      Arr2InnerDisableLayerRunnable = array of Arr1InnerDisableLayerRunnable;
      Arr3InnerDisableLayerRunnable = array of Arr2InnerDisableLayerRunnable;
      InnerAccessibilityDelegate = class;
      Arr1InnerAccessibilityDelegate = array of InnerAccessibilityDelegate;
      Arr2InnerAccessibilityDelegate = array of Arr1InnerAccessibilityDelegate;
      Arr3InnerAccessibilityDelegate = array of Arr2InnerAccessibilityDelegate;
      InnerSlidingPanelLayoutImplJBMR1 = class;
      Arr1InnerSlidingPanelLayoutImplJBMR1 = array of InnerSlidingPanelLayoutImplJBMR1;
      Arr2InnerSlidingPanelLayoutImplJBMR1 = array of Arr1InnerSlidingPanelLayoutImplJBMR1;
      Arr3InnerSlidingPanelLayoutImplJBMR1 = array of Arr2InnerSlidingPanelLayoutImplJBMR1;
      InnerSlidingPanelLayoutImplJB = class;
      Arr1InnerSlidingPanelLayoutImplJB = array of InnerSlidingPanelLayoutImplJB;
      Arr2InnerSlidingPanelLayoutImplJB = array of Arr1InnerSlidingPanelLayoutImplJB;
      Arr3InnerSlidingPanelLayoutImplJB = array of Arr2InnerSlidingPanelLayoutImplJB;
      InnerSlidingPanelLayoutImplBase = class;
      Arr1InnerSlidingPanelLayoutImplBase = array of InnerSlidingPanelLayoutImplBase;
      Arr2InnerSlidingPanelLayoutImplBase = array of Arr1InnerSlidingPanelLayoutImplBase;
      Arr3InnerSlidingPanelLayoutImplBase = array of Arr2InnerSlidingPanelLayoutImplBase;
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerDragHelperCallback = class;
      Arr1InnerDragHelperCallback = array of InnerDragHelperCallback;
      Arr2InnerDragHelperCallback = array of Arr1InnerDragHelperCallback;
      Arr3InnerDragHelperCallback = array of Arr2InnerDragHelperCallback;
      InnerSlidingPanelLayoutImpl = interface external 'android.support.v4.widget' name 'SlidingPanelLayoutImpl' 
      end;

      InnerDisableLayerRunnable = class external 'android.support.v4.widget' name 'DisableLayerRunnable' 
      end;

      InnerAccessibilityDelegate = class external 'android.support.v4.widget' name 'AccessibilityDelegate' 
      end;

      InnerSlidingPanelLayoutImplJBMR1 = class external 'android.support.v4.widget' name 'SlidingPanelLayoutImplJBMR1' 
      end;

      InnerSlidingPanelLayoutImplJB = class external 'android.support.v4.widget' name 'SlidingPanelLayoutImplJB' 
      end;

      InnerSlidingPanelLayoutImplBase = class external 'android.support.v4.widget' name 'SlidingPanelLayoutImplBase' 
      end;

      InnerSavedState = class external 'android.support.v4.widget' name 'SavedState' 
      end;

      InnerDragHelperCallback = class external 'android.support.v4.widget' name 'DragHelperCallback' 
      end;

  public
    type
      InnerPanelSlideListener = interface;
      Arr1InnerPanelSlideListener = array of InnerPanelSlideListener;
      Arr2InnerPanelSlideListener = array of Arr1InnerPanelSlideListener;
      Arr3InnerPanelSlideListener = array of Arr2InnerPanelSlideListener;
      InnerLayoutParams = class;
      Arr1InnerLayoutParams = array of InnerLayoutParams;
      Arr2InnerLayoutParams = array of Arr1InnerLayoutParams;
      Arr3InnerLayoutParams = array of Arr2InnerLayoutParams;
      InnerSimplePanelSlideListener = class;
      Arr1InnerSimplePanelSlideListener = array of InnerSimplePanelSlideListener;
      Arr2InnerSimplePanelSlideListener = array of Arr1InnerSimplePanelSlideListener;
      Arr3InnerSimplePanelSlideListener = array of Arr2InnerSimplePanelSlideListener;
      InnerPanelSlideListener = interface external 'android.support.v4.widget' name 'PanelSlideListener' 
        procedure onPanelSlide(para1: AVView; para2: jfloat); overload;
        procedure onPanelOpened(para1: AVView); overload;
        procedure onPanelClosed(para1: AVView); overload;
      end;

      InnerLayoutParams = class external 'android.support.v4.widget' name 'LayoutParams' (AVViewGroup.InnerMarginLayoutParams)
      public
        var
          fweight: jfloat; external name 'weight';
      public
        constructor create(); overload;
        constructor create(para1: jint; para2: jint); overload;
        constructor create(para1: AVViewGroup.InnerLayoutParams); overload;
        constructor create(para1: AVViewGroup.InnerMarginLayoutParams); overload;
        constructor create(para1: ASVWSlidingPaneLayout.InnerLayoutParams); overload;
        constructor create(para1: ACContext; para2: AUAttributeSet); overload;
      end;

      InnerSimplePanelSlideListener = class external 'android.support.v4.widget' name 'SimplePanelSlideListener' (JLObject, ASVWSlidingPaneLayout.InnerPanelSlideListener)
      public
        constructor create(); overload;
        procedure onPanelSlide(para1: AVView; para2: jfloat); overload; virtual;
        procedure onPanelOpened(para1: AVView); overload; virtual;
        procedure onPanelClosed(para1: AVView); overload; virtual;
      end;

  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet; para3: jint); overload;
    procedure setParallaxDistance(para1: jint); overload; virtual;
    function getParallaxDistance(): jint; overload; virtual;
    procedure setSliderFadeColor(para1: jint); overload; virtual;
    function getSliderFadeColor(): jint; overload; virtual;
    procedure setCoveredFadeColor(para1: jint); overload; virtual;
    function getCoveredFadeColor(): jint; overload; virtual;
    procedure setPanelSlideListener(para1: ASVWSlidingPaneLayout.InnerPanelSlideListener); overload; virtual;
  strict protected
    procedure onAttachedToWindow(); overload; virtual;
    procedure onDetachedFromWindow(); overload; virtual;
    procedure onMeasure(para1: jint; para2: jint); overload; virtual;
    procedure onLayout(para1: jboolean; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
    procedure onSizeChanged(para1: jint; para2: jint; para3: jint; para4: jint); overload; virtual;
  public
    procedure requestChildFocus(para1: AVView; para2: AVView); overload; virtual;
    function onInterceptTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    function onTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    procedure smoothSlideOpen(); overload; virtual;
    function openPane(): jboolean; overload; virtual;
    procedure smoothSlideClosed(); overload; virtual;
    function closePane(): jboolean; overload; virtual;
    function isOpen(): jboolean; overload; virtual;
    function canSlide(): jboolean; overload; virtual;
    function isSlideable(): jboolean; overload; virtual;
  strict protected
    function drawChild(para1: AGCanvas; para2: AVView; para3: jlong): jboolean; overload; virtual;
  public
    procedure computeScroll(); overload; virtual;
    procedure setShadowDrawable(para1: AGDDrawable); overload; virtual;
    procedure setShadowResource(para1: jint); overload; virtual;
    procedure draw(para1: AGCanvas); overload; virtual;
  strict protected
    function canScroll(para1: AVView; para2: jboolean; para3: jint; para4: jint; para5: jint): jboolean; overload; virtual;
    function generateDefaultLayoutParams(): AVViewGroup.InnerLayoutParams; overload; virtual;
    function generateLayoutParams(para1: AVViewGroup.InnerLayoutParams): AVViewGroup.InnerLayoutParams; overload; virtual;
    function checkLayoutParams(para1: AVViewGroup.InnerLayoutParams): jboolean; overload; virtual;
  public
    function generateLayoutParams(para1: AUAttributeSet): AVViewGroup.InnerLayoutParams; overload; virtual;
  strict protected
    function onSaveInstanceState(): AOParcelable; overload; virtual;
    procedure onRestoreInstanceState(para1: AOParcelable); overload; virtual;
  end;

  ASVWCursorFilter = class external 'android.support.v4.widget' name 'CursorFilter' (AWFilter)
  public
    type
      InnerCursorFilterClient = interface;
      Arr1InnerCursorFilterClient = array of InnerCursorFilterClient;
      Arr2InnerCursorFilterClient = array of Arr1InnerCursorFilterClient;
      Arr3InnerCursorFilterClient = array of Arr2InnerCursorFilterClient;
      InnerCursorFilterClient = interface external 'android.support.v4.widget' name 'CursorFilterClient' 
      end;

  public
    function convertResultToString(para1: JLObject): JLCharSequence; overload; virtual;
  strict protected
    function performFiltering(para1: JLCharSequence): AWFilter.InnerFilterResults; overload; virtual;
    procedure publishResults(para1: JLCharSequence; para2: AWFilter.InnerFilterResults); overload; virtual;
  end;

  ASVWSearchViewCompatIcs = class external 'android.support.v4.widget' name 'SearchViewCompatIcs' (JLObject)
  public
    type
      InnerMySearchView = class;
      Arr1InnerMySearchView = array of InnerMySearchView;
      Arr2InnerMySearchView = array of Arr1InnerMySearchView;
      Arr3InnerMySearchView = array of Arr2InnerMySearchView;
      InnerMySearchView = class external 'android.support.v4.widget' name 'MySearchView' (AWSearchView)
      public
        constructor create(para1: ACContext); overload;
        procedure onActionViewCollapsed(); overload; virtual;
      end;

  public
    class function newSearchView(para1: ACContext): AVView; static; overload;
    class procedure setImeOptions(para1: AVView; para2: jint); static; overload;
    class procedure setInputType(para1: AVView; para2: jint); static; overload;
  end;

  ASVALoaderManagerImpl = class external 'android.support.v4.app' name 'LoaderManagerImpl' (ASVALoaderManager)
  public
    type
      InnerLoaderInfo = class;
      Arr1InnerLoaderInfo = array of InnerLoaderInfo;
      Arr2InnerLoaderInfo = array of Arr1InnerLoaderInfo;
      Arr3InnerLoaderInfo = array of Arr2InnerLoaderInfo;
      InnerLoaderInfo = class sealed external 'android.support.v4.app' name 'LoaderInfo' 
      end;

  public
    function initLoader(para1: jint; para2: AOBundle; para3: ASVALoaderManager.InnerLoaderCallbacks): ASVCLoader; overload; virtual;
    function restartLoader(para1: jint; para2: AOBundle; para3: ASVALoaderManager.InnerLoaderCallbacks): ASVCLoader; overload; virtual;
    procedure destroyLoader(para1: jint); overload; virtual;
    function getLoader(para1: jint): ASVCLoader; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
    function hasRunningLoaders(): jboolean; overload; virtual;
  end;

  ASVAActivityCompat = class external 'android.support.v4.app' name 'ActivityCompat' (ASVCContextCompat)
  public
    constructor create(); overload;
    class function invalidateOptionsMenu(para1: AAActivity): jboolean; static; overload;
    class procedure startActivity(para1: AAActivity; para2: ACIntent; para3: AOBundle); static; overload;
    class procedure startActivityForResult(para1: AAActivity; para2: ACIntent; para3: jint; para4: AOBundle); static; overload;
    class procedure finishAffinity(para1: AAActivity); static; overload;
  end;

  ASVCAsyncTaskLoader = class abstract external 'android.support.v4.content' name 'AsyncTaskLoader' (ASVCLoader)
  public
    type
      InnerLoadTask = class;
      Arr1InnerLoadTask = array of InnerLoadTask;
      Arr2InnerLoadTask = array of Arr1InnerLoadTask;
      Arr3InnerLoadTask = array of Arr2InnerLoadTask;
      InnerLoadTask = class sealed external 'android.support.v4.content' name 'LoadTask' 
      end;

  public
    constructor create(para1: ACContext); overload;
    procedure setUpdateThrottle(para1: jlong); overload; virtual;
  strict protected
    procedure onForceLoad(); overload; virtual;
  public
    function cancelLoad(): jboolean; overload; virtual;
    procedure onCanceled(para1: JLObject); overload; virtual;
    function loadInBackground(): JLObject; overload; virtual; abstract;
  strict protected
    function onLoadInBackground(): JLObject; overload; virtual;
  public
    procedure waitForLoader(); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
  end;

  ASVMTransportMediator = class external 'android.support.v4.media' name 'TransportMediator' (ASVMTransportController)
  public
    const
      KEYCODE_MEDIA_PLAY = 126;
      KEYCODE_MEDIA_PAUSE = 127;
      KEYCODE_MEDIA_RECORD = 130;
      FLAG_KEY_MEDIA_PREVIOUS = 1;
      FLAG_KEY_MEDIA_REWIND = 2;
      FLAG_KEY_MEDIA_PLAY = 4;
      FLAG_KEY_MEDIA_PLAY_PAUSE = 8;
      FLAG_KEY_MEDIA_PAUSE = 16;
      FLAG_KEY_MEDIA_STOP = 32;
      FLAG_KEY_MEDIA_FAST_FORWARD = 64;
      FLAG_KEY_MEDIA_NEXT = 128;
  public
    constructor create(para1: AAActivity; para2: ASVMTransportPerformer); overload;
    constructor create(para1: AVView; para2: ASVMTransportPerformer); overload;
    function getRemoteControlClient(): JLObject; overload; virtual;
    function dispatchKeyEvent(para1: AVKeyEvent): jboolean; overload; virtual;
    procedure registerStateListener(para1: ASVMTransportStateListener); overload; virtual;
    procedure unregisterStateListener(para1: ASVMTransportStateListener); overload; virtual;
    procedure refreshState(); overload; virtual;
    procedure startPlaying(); overload; virtual;
    procedure pausePlaying(); overload; virtual;
    procedure stopPlaying(); overload; virtual;
    function getDuration(): jlong; overload; virtual;
    function getCurrentPosition(): jlong; overload; virtual;
    procedure seekTo(para1: jlong); overload; virtual;
    function isPlaying(): jboolean; overload; virtual;
    function getBufferPercentage(): jint; overload; virtual;
    function getTransportControlFlags(): jint; overload; virtual;
    procedure destroy(); overload; virtual;
  end;

  ASVWExploreByTouchHelper = class abstract external 'android.support.v4.widget' name 'ExploreByTouchHelper' (ASVVAccessibilityDelegateCompat)
  public
    type
      InnerExploreByTouchNodeProvider = class;
      Arr1InnerExploreByTouchNodeProvider = array of InnerExploreByTouchNodeProvider;
      Arr2InnerExploreByTouchNodeProvider = array of Arr1InnerExploreByTouchNodeProvider;
      Arr3InnerExploreByTouchNodeProvider = array of Arr2InnerExploreByTouchNodeProvider;
      InnerExploreByTouchNodeProvider = class external 'android.support.v4.widget' name 'ExploreByTouchNodeProvider' 
      end;

  public
    const
      INVALID_ID = -2147483648;
  public
    constructor create(para1: AVView); overload;
    function getAccessibilityNodeProvider(para1: AVView): ASVVAAccessibilityNodeProviderCompat; overload; virtual;
    function dispatchHoverEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    function sendEventForVirtualView(para1: jint; para2: jint): jboolean; overload; virtual;
    procedure invalidateRoot(); overload; virtual;
    procedure invalidateVirtualView(para1: jint); overload; virtual;
    function getFocusedVirtualView(): jint; overload; virtual;
  strict protected
    function getVirtualViewAt(para1: jfloat; para2: jfloat): jint; overload; virtual; abstract;
    procedure getVisibleVirtualViews(para1: JUList); overload; virtual; abstract;
    procedure onPopulateEventForVirtualView(para1: jint; para2: AVAAccessibilityEvent); overload; virtual; abstract;
    procedure onPopulateNodeForVirtualView(para1: jint; para2: ASVVAAccessibilityNodeInfoCompat); overload; virtual; abstract;
    function onPerformActionForVirtualView(para1: jint; para2: jint; para3: AOBundle): jboolean; overload; virtual; abstract;
  end;

  ASVIVSupportMenuItem = interface external 'android.support.v4.internal.view' name 'SupportMenuItem' (AVMenuItem)
    const
      SHOW_AS_ACTION_NEVER = 0;
      SHOW_AS_ACTION_IF_ROOM = 1;
      SHOW_AS_ACTION_ALWAYS = 2;
      SHOW_AS_ACTION_WITH_TEXT = 4;
      SHOW_AS_ACTION_COLLAPSE_ACTION_VIEW = 8;
    procedure setShowAsAction(para1: jint); overload;
    function setShowAsActionFlags(para1: jint): AVMenuItem; overload;
    function setActionView(para1: AVView): AVMenuItem; overload;
    function setActionView(para1: jint): AVMenuItem; overload;
    function getActionView(): AVView; overload;
    function setSupportActionProvider(para1: ASVVActionProvider): ASVIVSupportMenuItem; overload;
    function getSupportActionProvider(): ASVVActionProvider; overload;
    function expandActionView(): jboolean; overload;
    function collapseActionView(): jboolean; overload;
    function isActionViewExpanded(): jboolean; overload;
    function setSupportOnActionExpandListener(para1: ASVVMenuItemCompat.InnerOnActionExpandListener): ASVIVSupportMenuItem; overload;
  end;

  ASV13AFragmentPagerAdapter = class abstract external 'android.support.v13.app' name 'FragmentPagerAdapter' (ASVVPagerAdapter)
  public
    constructor create(para1: AAFragmentManager); overload;
    function getItem(para1: jint): AAFragment; overload; virtual; abstract;
    procedure startUpdate(para1: AVViewGroup); overload; virtual;
    function instantiateItem(para1: AVViewGroup; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure setPrimaryItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVViewGroup); overload; virtual;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual;
    function saveState(): AOParcelable; overload; virtual;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual;
    function getItemId(para1: jint): jlong; overload; virtual;
  end;

  ASV13AFragmentStatePagerAdapter = class abstract external 'android.support.v13.app' name 'FragmentStatePagerAdapter' (ASVVPagerAdapter)
  public
    constructor create(para1: AAFragmentManager); overload;
    function getItem(para1: jint): AAFragment; overload; virtual; abstract;
    procedure startUpdate(para1: AVViewGroup); overload; virtual;
    function instantiateItem(para1: AVViewGroup; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure setPrimaryItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVViewGroup); overload; virtual;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual;
    function saveState(): AOParcelable; overload; virtual;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual;
  end;

  ASV4AFragmentPagerAdapter = class abstract external 'android.support.v4.app' name 'FragmentPagerAdapter' (ASVVPagerAdapter)
  public
    constructor create(para1: ASVAFragmentManager); overload;
    function getItem(para1: jint): ASVAFragment; overload; virtual; abstract;
    procedure startUpdate(para1: AVViewGroup); overload; virtual;
    function instantiateItem(para1: AVViewGroup; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure setPrimaryItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVViewGroup); overload; virtual;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual;
    function saveState(): AOParcelable; overload; virtual;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual;
    function getItemId(para1: jint): jlong; overload; virtual;
  end;

  ASV4AFragmentStatePagerAdapter = class abstract external 'android.support.v4.app' name 'FragmentStatePagerAdapter' (ASVVPagerAdapter)
  public
    constructor create(para1: ASVAFragmentManager); overload;
    function getItem(para1: jint): ASVAFragment; overload; virtual; abstract;
    procedure startUpdate(para1: AVViewGroup); overload; virtual;
    function instantiateItem(para1: AVViewGroup; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure setPrimaryItem(para1: AVViewGroup; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVViewGroup); overload; virtual;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual;
    function saveState(): AOParcelable; overload; virtual;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual;
  end;

  ASVADialogFragment = class external 'android.support.v4.app' name 'DialogFragment' (ASVAFragment, ACDialogInterface.InnerOnCancelListener, ACDialogInterface.InnerOnDismissListener)
  public
    const
      STYLE_NORMAL = 0;
      STYLE_NO_TITLE = 1;
      STYLE_NO_FRAME = 2;
      STYLE_NO_INPUT = 3;
  public
    constructor create(); overload;
    procedure setStyle(para1: jint; para2: jint); overload; virtual;
    procedure show(para1: ASVAFragmentManager; para2: JLString); overload; virtual;
    function show(para1: ASVAFragmentTransaction; para2: JLString): jint; overload; virtual;
    procedure dismiss(); overload; virtual;
    procedure dismissAllowingStateLoss(); overload; virtual;
    function getDialog(): AADialog; overload; virtual;
    function getTheme(): jint; overload; virtual;
    procedure setCancelable(para1: jboolean); overload; virtual;
    function isCancelable(): jboolean; overload; virtual;
    procedure setShowsDialog(para1: jboolean); overload; virtual;
    function getShowsDialog(): jboolean; overload; virtual;
    procedure onAttach(para1: AAActivity); overload; virtual;
    procedure onDetach(); overload; virtual;
    procedure onCreate(para1: AOBundle); overload; virtual;
    function getLayoutInflater(para1: AOBundle): AVLayoutInflater; overload; virtual;
    function onCreateDialog(para1: AOBundle): AADialog; overload; virtual;
    procedure onCancel(para1: ACDialogInterface); overload; virtual;
    procedure onDismiss(para1: ACDialogInterface); overload; virtual;
    procedure onActivityCreated(para1: AOBundle); overload; virtual;
    procedure onStart(); overload; virtual;
    procedure onSaveInstanceState(para1: AOBundle); overload; virtual;
    procedure onStop(); overload; virtual;
    procedure onDestroyView(); overload; virtual;
  end;

  ASVAFragmentManager = class abstract external 'android.support.v4.app' name 'FragmentManager' (JLObject)
  public
    type
      InnerOnBackStackChangedListener = interface;
      Arr1InnerOnBackStackChangedListener = array of InnerOnBackStackChangedListener;
      Arr2InnerOnBackStackChangedListener = array of Arr1InnerOnBackStackChangedListener;
      Arr3InnerOnBackStackChangedListener = array of Arr2InnerOnBackStackChangedListener;
      InnerBackStackEntry = interface;
      Arr1InnerBackStackEntry = array of InnerBackStackEntry;
      Arr2InnerBackStackEntry = array of Arr1InnerBackStackEntry;
      Arr3InnerBackStackEntry = array of Arr2InnerBackStackEntry;
      InnerOnBackStackChangedListener = interface external 'android.support.v4.app' name 'OnBackStackChangedListener' 
        procedure onBackStackChanged(); overload;
      end;

      InnerBackStackEntry = interface external 'android.support.v4.app' name 'BackStackEntry' 
        function getId(): jint; overload;
        function getName(): JLString; overload;
        function getBreadCrumbTitleRes(): jint; overload;
        function getBreadCrumbShortTitleRes(): jint; overload;
        function getBreadCrumbTitle(): JLCharSequence; overload;
        function getBreadCrumbShortTitle(): JLCharSequence; overload;
      end;

  public
    const
      POP_BACK_STACK_INCLUSIVE = 1;
  public
    constructor create(); overload;
    function beginTransaction(): ASVAFragmentTransaction; overload; virtual; abstract;
    function openTransaction(): ASVAFragmentTransaction; overload; virtual;
    function executePendingTransactions(): jboolean; overload; virtual; abstract;
    function findFragmentById(para1: jint): ASVAFragment; overload; virtual; abstract;
    function findFragmentByTag(para1: JLString): ASVAFragment; overload; virtual; abstract;
    procedure popBackStack(); overload; virtual; abstract;
    function popBackStackImmediate(): jboolean; overload; virtual; abstract;
    procedure popBackStack(para1: JLString; para2: jint); overload; virtual; abstract;
    function popBackStackImmediate(para1: JLString; para2: jint): jboolean; overload; virtual; abstract;
    procedure popBackStack(para1: jint; para2: jint); overload; virtual; abstract;
    function popBackStackImmediate(para1: jint; para2: jint): jboolean; overload; virtual; abstract;
    function getBackStackEntryCount(): jint; overload; virtual; abstract;
    function getBackStackEntryAt(para1: jint): ASVAFragmentManager.InnerBackStackEntry; overload; virtual; abstract;
    procedure addOnBackStackChangedListener(para1: ASVAFragmentManager.InnerOnBackStackChangedListener); overload; virtual; abstract;
    procedure removeOnBackStackChangedListener(para1: ASVAFragmentManager.InnerOnBackStackChangedListener); overload; virtual; abstract;
    procedure putFragment(para1: AOBundle; para2: JLString; para3: ASVAFragment); overload; virtual; abstract;
    function getFragment(para1: AOBundle; para2: JLString): ASVAFragment; overload; virtual; abstract;
    function getFragments(): JUList; overload; virtual; abstract;
    function saveFragmentInstanceState(para1: ASVAFragment): ASVAFragment.InnerSavedState; overload; virtual; abstract;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual; abstract;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual; abstract;
    class procedure enableDebugLogging(para1: jboolean); static; overload;
  end;

  ASVAListFragment = class external 'android.support.v4.app' name 'ListFragment' (ASVAFragment)
  public
    constructor create(); overload;
    function onCreateView(para1: AVLayoutInflater; para2: AVViewGroup; para3: AOBundle): AVView; overload; virtual;
    procedure onViewCreated(para1: AVView; para2: AOBundle); overload; virtual;
    procedure onDestroyView(); overload; virtual;
    procedure onListItemClick(para1: AWListView; para2: AVView; para3: jint; para4: jlong); overload; virtual;
    procedure setListAdapter(para1: AWListAdapter); overload; virtual;
    procedure setSelection(para1: jint); overload; virtual;
    function getSelectedItemPosition(): jint; overload; virtual;
    function getSelectedItemId(): jlong; overload; virtual;
    function getListView(): AWListView; overload; virtual;
    procedure setEmptyText(para1: JLCharSequence); overload; virtual;
    procedure setListShown(para1: jboolean); overload; virtual;
    procedure setListShownNoAnimation(para1: jboolean); overload; virtual;
    function getListAdapter(): AWListAdapter; overload; virtual;
  end;

  ASVIVSupportSubMenu = interface external 'android.support.v4.internal.view' name 'SupportSubMenu' (ASVIVSupportMenu, AVSubMenu)
  end;

  ASVVPagerTitleStrip = class external 'android.support.v4.view' name 'PagerTitleStrip' (AVViewGroup, ASVVViewPager.InnerDecor)
  public
    type
      InnerPageListener = class;
      Arr1InnerPageListener = array of InnerPageListener;
      Arr2InnerPageListener = array of Arr1InnerPageListener;
      Arr3InnerPageListener = array of Arr2InnerPageListener;
      InnerPagerTitleStripImpl = interface;
      Arr1InnerPagerTitleStripImpl = array of InnerPagerTitleStripImpl;
      Arr2InnerPagerTitleStripImpl = array of Arr1InnerPagerTitleStripImpl;
      Arr3InnerPagerTitleStripImpl = array of Arr2InnerPagerTitleStripImpl;
      InnerPagerTitleStripImplIcs = class;
      Arr1InnerPagerTitleStripImplIcs = array of InnerPagerTitleStripImplIcs;
      Arr2InnerPagerTitleStripImplIcs = array of Arr1InnerPagerTitleStripImplIcs;
      Arr3InnerPagerTitleStripImplIcs = array of Arr2InnerPagerTitleStripImplIcs;
      InnerPagerTitleStripImplBase = class;
      Arr1InnerPagerTitleStripImplBase = array of InnerPagerTitleStripImplBase;
      Arr2InnerPagerTitleStripImplBase = array of Arr1InnerPagerTitleStripImplBase;
      Arr3InnerPagerTitleStripImplBase = array of Arr2InnerPagerTitleStripImplBase;
      InnerPageListener = class external 'android.support.v4.view' name 'PageListener' 
      end;

      InnerPagerTitleStripImpl = interface external 'android.support.v4.view' name 'PagerTitleStripImpl' 
      end;

      InnerPagerTitleStripImplIcs = class external 'android.support.v4.view' name 'PagerTitleStripImplIcs' 
      end;

      InnerPagerTitleStripImplBase = class external 'android.support.v4.view' name 'PagerTitleStripImplBase' 
      end;

  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    procedure setTextSpacing(para1: jint); overload; virtual;
    function getTextSpacing(): jint; overload; virtual;
    procedure setNonPrimaryAlpha(para1: jfloat); overload; virtual;
    procedure setTextColor(para1: jint); overload; virtual;
    procedure setTextSize(para1: jint; para2: jfloat); overload; virtual;
    procedure setGravity(para1: jint); overload; virtual;
  strict protected
    procedure onAttachedToWindow(); overload; virtual;
    procedure onDetachedFromWindow(); overload; virtual;
  public
    procedure requestLayout(); overload; virtual;
  strict protected
    procedure onMeasure(para1: jint; para2: jint); overload; virtual;
    procedure onLayout(para1: jboolean; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
  end;

  ASVAActionBarDrawerToggle = class external 'android.support.v4.app' name 'ActionBarDrawerToggle' (JLObject, ASVWDrawerLayout.InnerDrawerListener)
  public
    type
      InnerSlideDrawable = class;
      Arr1InnerSlideDrawable = array of InnerSlideDrawable;
      Arr2InnerSlideDrawable = array of Arr1InnerSlideDrawable;
      Arr3InnerSlideDrawable = array of Arr2InnerSlideDrawable;
      InnerActionBarDrawerToggleImpl = interface;
      Arr1InnerActionBarDrawerToggleImpl = array of InnerActionBarDrawerToggleImpl;
      Arr2InnerActionBarDrawerToggleImpl = array of Arr1InnerActionBarDrawerToggleImpl;
      Arr3InnerActionBarDrawerToggleImpl = array of Arr2InnerActionBarDrawerToggleImpl;
      InnerActionBarDrawerToggleImplHC = class;
      Arr1InnerActionBarDrawerToggleImplHC = array of InnerActionBarDrawerToggleImplHC;
      Arr2InnerActionBarDrawerToggleImplHC = array of Arr1InnerActionBarDrawerToggleImplHC;
      Arr3InnerActionBarDrawerToggleImplHC = array of Arr2InnerActionBarDrawerToggleImplHC;
      InnerActionBarDrawerToggleImplBase = class;
      Arr1InnerActionBarDrawerToggleImplBase = array of InnerActionBarDrawerToggleImplBase;
      Arr2InnerActionBarDrawerToggleImplBase = array of Arr1InnerActionBarDrawerToggleImplBase;
      Arr3InnerActionBarDrawerToggleImplBase = array of Arr2InnerActionBarDrawerToggleImplBase;
      InnerSlideDrawable = class external 'android.support.v4.app' name 'SlideDrawable' 
      end;

      InnerActionBarDrawerToggleImpl = interface external 'android.support.v4.app' name 'ActionBarDrawerToggleImpl' 
      end;

      InnerActionBarDrawerToggleImplHC = class external 'android.support.v4.app' name 'ActionBarDrawerToggleImplHC' 
      end;

      InnerActionBarDrawerToggleImplBase = class external 'android.support.v4.app' name 'ActionBarDrawerToggleImplBase' 
      end;

  public
    type
      InnerDelegate = interface;
      Arr1InnerDelegate = array of InnerDelegate;
      Arr2InnerDelegate = array of Arr1InnerDelegate;
      Arr3InnerDelegate = array of Arr2InnerDelegate;
      InnerDelegateProvider = interface;
      Arr1InnerDelegateProvider = array of InnerDelegateProvider;
      Arr2InnerDelegateProvider = array of Arr1InnerDelegateProvider;
      Arr3InnerDelegateProvider = array of Arr2InnerDelegateProvider;
      InnerDelegate = interface external 'android.support.v4.app' name 'Delegate' 
        function getThemeUpIndicator(): AGDDrawable; overload;
        procedure setActionBarUpIndicator(para1: AGDDrawable; para2: jint); overload;
        procedure setActionBarDescription(para1: jint); overload;
      end;

      InnerDelegateProvider = interface external 'android.support.v4.app' name 'DelegateProvider' 
        function getDrawerToggleDelegate(): ASVAActionBarDrawerToggle.InnerDelegate; overload;
      end;

  public
    constructor create(para1: AAActivity; para2: ASVWDrawerLayout; para3: jint; para4: jint; para5: jint); overload;
    procedure syncState(); overload; virtual;
    procedure setDrawerIndicatorEnabled(para1: jboolean); overload; virtual;
    function isDrawerIndicatorEnabled(): jboolean; overload; virtual;
    procedure onConfigurationChanged(para1: ACRConfiguration); overload; virtual;
    function onOptionsItemSelected(para1: AVMenuItem): jboolean; overload; virtual;
    procedure onDrawerSlide(para1: AVView; para2: jfloat); overload; virtual;
    procedure onDrawerOpened(para1: AVView); overload; virtual;
    procedure onDrawerClosed(para1: AVView); overload; virtual;
    procedure onDrawerStateChanged(para1: jint); overload; virtual;
  end;

  ASVWCursorAdapter = class abstract external 'android.support.v4.widget' name 'CursorAdapter' (AWBaseAdapter, AWFilterable, ASVWCursorFilter.InnerCursorFilterClient)
  public
    type
      InnerMyDataSetObserver = class;
      Arr1InnerMyDataSetObserver = array of InnerMyDataSetObserver;
      Arr2InnerMyDataSetObserver = array of Arr1InnerMyDataSetObserver;
      Arr3InnerMyDataSetObserver = array of Arr2InnerMyDataSetObserver;
      InnerChangeObserver = class;
      Arr1InnerChangeObserver = array of InnerChangeObserver;
      Arr2InnerChangeObserver = array of Arr1InnerChangeObserver;
      Arr3InnerChangeObserver = array of Arr2InnerChangeObserver;
      InnerMyDataSetObserver = class external 'android.support.v4.widget' name 'MyDataSetObserver' 
      end;

      InnerChangeObserver = class external 'android.support.v4.widget' name 'ChangeObserver' 
      end;

  strict protected
    var
      fmDataValid: jboolean; external name 'mDataValid';
      fmAutoRequery: jboolean; external name 'mAutoRequery';
      fmCursor: ADCursor; external name 'mCursor';
      fmContext: ACContext; external name 'mContext';
      fmRowIDColumn: jint; external name 'mRowIDColumn';
      fmChangeObserver: ASVWCursorAdapter.InnerChangeObserver; external name 'mChangeObserver';
      fmDataSetObserver: ADDataSetObserver; external name 'mDataSetObserver';
      fmCursorFilter: ASVWCursorFilter; external name 'mCursorFilter';
      fmFilterQueryProvider: AWFilterQueryProvider; external name 'mFilterQueryProvider';
  public
    const
      FLAG_AUTO_REQUERY = 1 deprecated;
      FLAG_REGISTER_CONTENT_OBSERVER = 2;
  public
    constructor create(para1: ACContext; para2: ADCursor); overload;
    constructor create(para1: ACContext; para2: ADCursor; para3: jboolean); overload;
    constructor create(para1: ACContext; para2: ADCursor; para3: jint); overload;
  strict protected
    procedure init(para1: ACContext; para2: ADCursor; para3: jboolean); overload; virtual;
  public
    function getCursor(): ADCursor; overload; virtual;
    function getCount(): jint; overload; virtual;
    function getItem(para1: jint): JLObject; overload; virtual;
    function getItemId(para1: jint): jlong; overload; virtual;
    function hasStableIds(): jboolean; overload; virtual;
    function getView(para1: jint; para2: AVView; para3: AVViewGroup): AVView; overload; virtual;
    function getDropDownView(para1: jint; para2: AVView; para3: AVViewGroup): AVView; overload; virtual;
    function newView(para1: ACContext; para2: ADCursor; para3: AVViewGroup): AVView; overload; virtual; abstract;
    function newDropDownView(para1: ACContext; para2: ADCursor; para3: AVViewGroup): AVView; overload; virtual;
    procedure bindView(para1: AVView; para2: ACContext; para3: ADCursor); overload; virtual; abstract;
    procedure changeCursor(para1: ADCursor); overload; virtual;
    function swapCursor(para1: ADCursor): ADCursor; overload; virtual;
    function convertToString(para1: ADCursor): JLCharSequence; overload; virtual;
    function runQueryOnBackgroundThread(para1: JLCharSequence): ADCursor; overload; virtual;
    function getFilter(): AWFilter; overload; virtual;
    function getFilterQueryProvider(): AWFilterQueryProvider; overload; virtual;
    procedure setFilterQueryProvider(para1: AWFilterQueryProvider); overload; virtual;
  strict protected
    procedure onContentChanged(); overload; virtual;
  end;

  ASVCCursorLoader = class external 'android.support.v4.content' name 'CursorLoader' (ASVCAsyncTaskLoader)
  public
    function loadInBackground(): ADCursor; overload; virtual;
    procedure deliverResult(para1: ADCursor); overload; virtual;
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: ANUri; para3: Arr1JLString; para4: JLString; para5: Arr1JLString; para6: JLString); overload;
    constructor create(para1: ACContext; para2: ANUri; const para3: array of JLString; para4: JLString; const para5: array of JLString; para6: JLString); overload;
  strict protected
    procedure onStartLoading(); overload; virtual;
    procedure onStopLoading(); overload; virtual;
  public
    procedure onCanceled(para1: ADCursor); overload; virtual;
  strict protected
    procedure onReset(); overload; virtual;
  public
    function getUri(): ANUri; overload; virtual;
    procedure setUri(para1: ANUri); overload; virtual;
    function getProjection(): Arr1JLString; overload; virtual;
    procedure setProjection(para1: Arr1JLString); overload; virtual;
    procedure setProjection(var para1: array of JLString); overload; virtual;
    function getSelection(): JLString; overload; virtual;
    procedure setSelection(para1: JLString); overload; virtual;
    function getSelectionArgs(): Arr1JLString; overload; virtual;
    procedure setSelectionArgs(para1: Arr1JLString); overload; virtual;
    procedure setSelectionArgs(var para1: array of JLString); overload; virtual;
    function getSortOrder(): JLString; overload; virtual;
    procedure setSortOrder(para1: JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
    function loadInBackground(): JLObject; overload; virtual;
    procedure onCanceled(para1: JLObject); overload; virtual;
    procedure deliverResult(para1: JLObject); overload; virtual;
  end;

  ASVABackStackRecord = class sealed external 'android.support.v4.app' name 'BackStackRecord' (ASVAFragmentTransaction, ASVAFragmentManager.InnerBackStackEntry, JLRunnable)
  public
    type
      InnerOp = class;
      Arr1InnerOp = array of InnerOp;
      Arr2InnerOp = array of Arr1InnerOp;
      Arr3InnerOp = array of Arr2InnerOp;
      InnerOp = class sealed external 'android.support.v4.app' name 'Op' 
      end;

  public
    function toString(): JLString; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIPrintWriter; para3: jboolean); overload; virtual;
    constructor create(para1: ASVAFragmentManagerImpl); overload;
    function getId(): jint; overload; virtual;
    function getBreadCrumbTitleRes(): jint; overload; virtual;
    function getBreadCrumbShortTitleRes(): jint; overload; virtual;
    function getBreadCrumbTitle(): JLCharSequence; overload; virtual;
    function getBreadCrumbShortTitle(): JLCharSequence; overload; virtual;
    function add(para1: ASVAFragment; para2: JLString): ASVAFragmentTransaction; overload; virtual;
    function add(para1: jint; para2: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function add(para1: jint; para2: ASVAFragment; para3: JLString): ASVAFragmentTransaction; overload; virtual;
    function replace(para1: jint; para2: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function replace(para1: jint; para2: ASVAFragment; para3: JLString): ASVAFragmentTransaction; overload; virtual;
    function remove(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function hide(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function show(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function detach(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function attach(para1: ASVAFragment): ASVAFragmentTransaction; overload; virtual;
    function setCustomAnimations(para1: jint; para2: jint): ASVAFragmentTransaction; overload; virtual;
    function setCustomAnimations(para1: jint; para2: jint; para3: jint; para4: jint): ASVAFragmentTransaction; overload; virtual;
    function setTransition(para1: jint): ASVAFragmentTransaction; overload; virtual;
    function setTransitionStyle(para1: jint): ASVAFragmentTransaction; overload; virtual;
    function addToBackStack(para1: JLString): ASVAFragmentTransaction; overload; virtual;
    function isAddToBackStackAllowed(): jboolean; overload; virtual;
    function disallowAddToBackStack(): ASVAFragmentTransaction; overload; virtual;
    function setBreadCrumbTitle(para1: jint): ASVAFragmentTransaction; overload; virtual;
    function setBreadCrumbTitle(para1: JLCharSequence): ASVAFragmentTransaction; overload; virtual;
    function setBreadCrumbShortTitle(para1: jint): ASVAFragmentTransaction; overload; virtual;
    function setBreadCrumbShortTitle(para1: JLCharSequence): ASVAFragmentTransaction; overload; virtual;
    function commit(): jint; overload; virtual;
    function commitAllowingStateLoss(): jint; overload; virtual;
    procedure run(); overload; virtual;
    procedure popFromBackStack(para1: jboolean); overload; virtual;
    function getName(): JLString; overload; virtual;
    function getTransition(): jint; overload; virtual;
    function getTransitionStyle(): jint; overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
  end;

  ASVAFragmentManagerImpl = class sealed external 'android.support.v4.app' name 'FragmentManagerImpl' (ASVAFragmentManager)
  public
    const
      ANIM_STYLE_OPEN_ENTER = 1;
      ANIM_STYLE_OPEN_EXIT = 2;
      ANIM_STYLE_CLOSE_ENTER = 3;
      ANIM_STYLE_CLOSE_EXIT = 4;
      ANIM_STYLE_FADE_ENTER = 5;
      ANIM_STYLE_FADE_EXIT = 6;
  public
    function beginTransaction(): ASVAFragmentTransaction; overload; virtual;
    function executePendingTransactions(): jboolean; overload; virtual;
    procedure popBackStack(); overload; virtual;
    function popBackStackImmediate(): jboolean; overload; virtual;
    procedure popBackStack(para1: JLString; para2: jint); overload; virtual;
    function popBackStackImmediate(para1: JLString; para2: jint): jboolean; overload; virtual;
    procedure popBackStack(para1: jint; para2: jint); overload; virtual;
    function popBackStackImmediate(para1: jint; para2: jint): jboolean; overload; virtual;
    function getBackStackEntryCount(): jint; overload; virtual;
    function getBackStackEntryAt(para1: jint): ASVAFragmentManager.InnerBackStackEntry; overload; virtual;
    procedure addOnBackStackChangedListener(para1: ASVAFragmentManager.InnerOnBackStackChangedListener); overload; virtual;
    procedure removeOnBackStackChangedListener(para1: ASVAFragmentManager.InnerOnBackStackChangedListener); overload; virtual;
    procedure putFragment(para1: AOBundle; para2: JLString; para3: ASVAFragment); overload; virtual;
    function getFragment(para1: AOBundle; para2: JLString): ASVAFragment; overload; virtual;
    function getFragments(): JUList; overload; virtual;
    function saveFragmentInstanceState(para1: ASVAFragment): ASVAFragment.InnerSavedState; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
    procedure performPendingDeferredStart(para1: ASVAFragment); overload; virtual;
    procedure addFragment(para1: ASVAFragment; para2: jboolean); overload; virtual;
    procedure removeFragment(para1: ASVAFragment; para2: jint; para3: jint); overload; virtual;
    procedure hideFragment(para1: ASVAFragment; para2: jint; para3: jint); overload; virtual;
    procedure showFragment(para1: ASVAFragment; para2: jint; para3: jint); overload; virtual;
    procedure detachFragment(para1: ASVAFragment; para2: jint; para3: jint); overload; virtual;
    procedure attachFragment(para1: ASVAFragment; para2: jint; para3: jint); overload; virtual;
    function findFragmentById(para1: jint): ASVAFragment; overload; virtual;
    function findFragmentByTag(para1: JLString): ASVAFragment; overload; virtual;
    function findFragmentByWho(para1: JLString): ASVAFragment; overload; virtual;
    procedure enqueueAction(para1: JLRunnable; para2: jboolean); overload; virtual;
    function allocBackStackIndex(para1: ASVABackStackRecord): jint; overload; virtual;
    procedure setBackStackIndex(para1: jint; para2: ASVABackStackRecord); overload; virtual;
    procedure freeBackStackIndex(para1: jint); overload; virtual;
    function execPendingActions(): jboolean; overload; virtual;
    procedure attachActivity(para1: ASVAFragmentActivity; para2: ASVAFragmentContainer; para3: ASVAFragment); overload; virtual;
    procedure noteStateNotSaved(); overload; virtual;
    procedure dispatchCreate(); overload; virtual;
    procedure dispatchActivityCreated(); overload; virtual;
    procedure dispatchStart(); overload; virtual;
    procedure dispatchResume(); overload; virtual;
    procedure dispatchPause(); overload; virtual;
    procedure dispatchStop(); overload; virtual;
    procedure dispatchReallyStop(); overload; virtual;
    procedure dispatchDestroyView(); overload; virtual;
    procedure dispatchDestroy(); overload; virtual;
    procedure dispatchConfigurationChanged(para1: ACRConfiguration); overload; virtual;
    procedure dispatchLowMemory(); overload; virtual;
    function dispatchCreateOptionsMenu(para1: AVMenu; para2: AVMenuInflater): jboolean; overload; virtual;
    function dispatchPrepareOptionsMenu(para1: AVMenu): jboolean; overload; virtual;
    function dispatchOptionsItemSelected(para1: AVMenuItem): jboolean; overload; virtual;
    function dispatchContextItemSelected(para1: AVMenuItem): jboolean; overload; virtual;
    procedure dispatchOptionsMenuClosed(para1: AVMenu); overload; virtual;
    class function reverseTransit(para1: jint): jint; static; overload;
    class function transitToStyleIndex(para1: jint; para2: jboolean): jint; static; overload;
  end;

  ASVVPagerTabStrip = class external 'android.support.v4.view' name 'PagerTabStrip' (ASVVPagerTitleStrip)
  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    procedure setTabIndicatorColor(para1: jint); overload; virtual;
    procedure setTabIndicatorColorResource(para1: jint); overload; virtual;
    function getTabIndicatorColor(): jint; overload; virtual;
    procedure setPadding(para1: jint; para2: jint; para3: jint; para4: jint); overload; virtual;
    procedure setTextSpacing(para1: jint); overload; virtual;
    procedure setBackgroundDrawable(para1: AGDDrawable); overload; virtual;
    procedure setBackgroundColor(para1: jint); overload; virtual;
    procedure setBackgroundResource(para1: jint); overload; virtual;
    procedure setDrawFullUnderline(para1: jboolean); overload; virtual;
    function getDrawFullUnderline(): jboolean; overload; virtual;
    function onTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
  strict protected
    procedure onDraw(para1: AGCanvas); overload; virtual;
  end;

  ASVWResourceCursorAdapter = class abstract external 'android.support.v4.widget' name 'ResourceCursorAdapter' (ASVWCursorAdapter)
  public
    constructor create(para1: ACContext; para2: jint; para3: ADCursor); overload;
    constructor create(para1: ACContext; para2: jint; para3: ADCursor; para4: jboolean); overload;
    constructor create(para1: ACContext; para2: jint; para3: ADCursor; para4: jint); overload;
    function newView(para1: ACContext; para2: ADCursor; para3: AVViewGroup): AVView; overload; virtual;
    function newDropDownView(para1: ACContext; para2: ADCursor; para3: AVViewGroup): AVView; overload; virtual;
    procedure setViewResource(para1: jint); overload; virtual;
    procedure setDropDownViewResource(para1: jint); overload; virtual;
  end;

  ASVWSimpleCursorAdapter = class external 'android.support.v4.widget' name 'SimpleCursorAdapter' (ASVWResourceCursorAdapter)
  public
    type
      InnerCursorToStringConverter = interface;
      Arr1InnerCursorToStringConverter = array of InnerCursorToStringConverter;
      Arr2InnerCursorToStringConverter = array of Arr1InnerCursorToStringConverter;
      Arr3InnerCursorToStringConverter = array of Arr2InnerCursorToStringConverter;
      InnerViewBinder = interface;
      Arr1InnerViewBinder = array of InnerViewBinder;
      Arr2InnerViewBinder = array of Arr1InnerViewBinder;
      Arr3InnerViewBinder = array of Arr2InnerViewBinder;
      InnerCursorToStringConverter = interface external 'android.support.v4.widget' name 'CursorToStringConverter' 
        function convertToString(para1: ADCursor): JLCharSequence; overload;
      end;

      InnerViewBinder = interface external 'android.support.v4.widget' name 'ViewBinder' 
        function setViewValue(para1: AVView; para2: ADCursor; para3: jint): jboolean; overload;
      end;

  strict protected
    var
      fmFrom: Arr1jint; external name 'mFrom';
      fmTo: Arr1jint; external name 'mTo';
  public
    constructor create(para1: ACContext; para2: jint; para3: ADCursor; para4: Arr1JLString; para5: Arr1jint); overload;
    constructor create(para1: ACContext; para2: jint; para3: ADCursor; const para4: array of JLString; const para5: array of jint); overload;
    constructor create(para1: ACContext; para2: jint; para3: ADCursor; para4: Arr1JLString; para5: Arr1jint; para6: jint); overload;
    constructor create(para1: ACContext; para2: jint; para3: ADCursor; const para4: array of JLString; const para5: array of jint; para6: jint); overload;
    procedure bindView(para1: AVView; para2: ACContext; para3: ADCursor); overload; virtual;
    function getViewBinder(): ASVWSimpleCursorAdapter.InnerViewBinder; overload; virtual;
    procedure setViewBinder(para1: ASVWSimpleCursorAdapter.InnerViewBinder); overload; virtual;
    procedure setViewImage(para1: AWImageView; para2: JLString); overload; virtual;
    procedure setViewText(para1: AWTextView; para2: JLString); overload; virtual;
    function getStringConversionColumn(): jint; overload; virtual;
    procedure setStringConversionColumn(para1: jint); overload; virtual;
    function getCursorToStringConverter(): ASVWSimpleCursorAdapter.InnerCursorToStringConverter; overload; virtual;
    procedure setCursorToStringConverter(para1: ASVWSimpleCursorAdapter.InnerCursorToStringConverter); overload; virtual;
    function convertToString(para1: ADCursor): JLCharSequence; overload; virtual;
    function swapCursor(para1: ADCursor): ADCursor; overload; virtual;
    procedure changeCursorAndColumns(para1: ADCursor; para2: Arr1JLString; para3: Arr1jint); overload; virtual;
    procedure changeCursorAndColumns(para1: ADCursor; var para2: array of JLString; var para3: array of jint); overload; virtual;
  end;

