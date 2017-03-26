  ASVAActivityCompatHoneycomb = class external 'android.support.v4.app' name 'ActivityCompatHoneycomb' (JLObject)
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

  ASVAHCSparseArray = class external 'android.support.v4.app' name 'HCSparseArray' (JLObject)
  public
    constructor create(); overload;
    constructor create(para1: jint); overload;
    function get(para1: jint): JLObject; overload; virtual;
    function get(para1: jint; para2: JLObject): JLObject; overload; virtual;
    procedure delete(para1: jint); overload; virtual;
    procedure remove(para1: jint); overload; virtual;
    procedure removeAt(para1: jint); overload; virtual;
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
    procedure onContentChanged(); overload; virtual;
    function dataToString(para1: JLObject): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
  end;

  ASVOParcelableCompatCreatorCallbacks = interface external 'android.support.v4.os' name 'ParcelableCompatCreatorCallbacks' 
    function createFromParcel(para1: AOParcel; para2: JLClassLoader): JLObject; overload;
    function newArray(para1: jint): Arr1JLObject; overload;
  end;

  ASVOParcelableCompatCreatorHoneycombMR2Stub = class external 'android.support.v4.os' name 'ParcelableCompatCreatorHoneycombMR2Stub' (JLObject)
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

  ASVVMenuCompat = class external 'android.support.v4.view' name 'MenuCompat' (JLObject)
  public
    type
      InnerMenuVersionImpl = interface;
      Arr1InnerMenuVersionImpl = array of InnerMenuVersionImpl;
      Arr2InnerMenuVersionImpl = array of Arr1InnerMenuVersionImpl;
      Arr3InnerMenuVersionImpl = array of Arr2InnerMenuVersionImpl;
      InnerHoneycombMenuVersionImpl = class;
      Arr1InnerHoneycombMenuVersionImpl = array of InnerHoneycombMenuVersionImpl;
      Arr2InnerHoneycombMenuVersionImpl = array of Arr1InnerHoneycombMenuVersionImpl;
      Arr3InnerHoneycombMenuVersionImpl = array of Arr2InnerHoneycombMenuVersionImpl;
      InnerBaseMenuVersionImpl = class;
      Arr1InnerBaseMenuVersionImpl = array of InnerBaseMenuVersionImpl;
      Arr2InnerBaseMenuVersionImpl = array of Arr1InnerBaseMenuVersionImpl;
      Arr3InnerBaseMenuVersionImpl = array of Arr2InnerBaseMenuVersionImpl;
      InnerMenuVersionImpl = interface external 'android.support.v4.view' name 'MenuVersionImpl' 
      end;

      InnerHoneycombMenuVersionImpl = class external 'android.support.v4.view' name 'HoneycombMenuVersionImpl' 
      end;

      InnerBaseMenuVersionImpl = class external 'android.support.v4.view' name 'BaseMenuVersionImpl' 
      end;

  public
    constructor create(); overload;
    class function setShowAsAction(para1: AVMenuItem; para2: jint): jboolean; static; overload;
  end;

  ASVVMenuCompatHoneycomb = class external 'android.support.v4.view' name 'MenuCompatHoneycomb' (JLObject)
  public
    class procedure setShowAsAction(para1: AVMenuItem; para2: jint); static; overload;
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
  public
    constructor create(); overload;
    class function getActionMasked(para1: AVMotionEvent): jint; static; overload;
    class function getActionIndex(para1: AVMotionEvent): jint; static; overload;
    class function findPointerIndex(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getPointerId(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getX(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
    class function getY(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
  end;

  ASVVMotionEventCompatEclair = class external 'android.support.v4.view' name 'MotionEventCompatEclair' (JLObject)
  public
    class function findPointerIndex(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getPointerId(para1: AVMotionEvent; para2: jint): jint; static; overload;
    class function getX(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
    class function getY(para1: AVMotionEvent; para2: jint): jfloat; static; overload;
  end;

  ASVVPagerAdapter = class abstract external 'android.support.v4.view' name 'PagerAdapter' (JLObject)
  public
    type
      InnerDataSetObserver = interface;
      Arr1InnerDataSetObserver = array of InnerDataSetObserver;
      Arr2InnerDataSetObserver = array of Arr1InnerDataSetObserver;
      Arr3InnerDataSetObserver = array of Arr2InnerDataSetObserver;
      InnerDataSetObserver = interface external 'android.support.v4.view' name 'DataSetObserver' 
      end;

  public
    const
      POSITION_UNCHANGED = -1;
      POSITION_NONE = -2;
  public
    constructor create(); overload;
    function getCount(): jint; overload; virtual; abstract;
    procedure startUpdate(para1: AVView); overload; virtual; abstract;
    function instantiateItem(para1: AVView; para2: jint): JLObject; overload; virtual; abstract;
    procedure destroyItem(para1: AVView; para2: jint; para3: JLObject); overload; virtual; abstract;
    procedure finishUpdate(para1: AVView); overload; virtual; abstract;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual; abstract;
    function saveState(): AOParcelable; overload; virtual; abstract;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual; abstract;
    function getItemPosition(para1: JLObject): jint; overload; virtual;
    procedure notifyDataSetChanged(); overload; virtual;
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
    function instantiate(para1: ASVAFragmentActivity): ASVAFragment; overload; virtual;
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
    function isAdded(): jboolean; overload; virtual; final;
    function isDetached(): jboolean; overload; virtual; final;
    function isRemoving(): jboolean; overload; virtual; final;
    function isInLayout(): jboolean; overload; virtual; final;
    function isResumed(): jboolean; overload; virtual; final;
    function isVisible(): jboolean; overload; virtual; final;
    function isHidden(): jboolean; overload; virtual; final;
    procedure onHiddenChanged(para1: jboolean); overload; virtual;
    procedure setRetainInstance(para1: jboolean); overload; virtual;
    function getRetainInstance(): jboolean; overload; virtual; final;
    procedure setHasOptionsMenu(para1: jboolean); overload; virtual;
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
    procedure onResume(); overload; virtual;
    procedure onPostResume(); overload; virtual;
  public
    function onPreparePanel(para1: jint; para2: AVView; para3: AVMenu): jboolean; overload; virtual;
    function onRetainNonConfigurationInstance(): JLObject; overload; virtual; final;
  strict protected
    procedure onSaveInstanceState(para1: AOBundle); overload; virtual;
    procedure onStart(); overload; virtual;
    procedure onStop(); overload; virtual;
  public
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

  ASVULogWriter = class external 'android.support.v4.util' name 'LogWriter' (JIWriter)
  public
    constructor create(para1: JLString); overload;
    procedure close(); overload; virtual;
    procedure flush(); overload; virtual;
    procedure write(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;
    procedure write(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;
  end;

  ASVVViewPager = class external 'android.support.v4.view' name 'ViewPager' (AVViewGroup)
  public
    type
      InnerItemInfo = class;
      Arr1InnerItemInfo = array of InnerItemInfo;
      Arr2InnerItemInfo = array of Arr1InnerItemInfo;
      Arr3InnerItemInfo = array of Arr2InnerItemInfo;
      InnerDataSetObserver = class;
      Arr1InnerDataSetObserver = array of InnerDataSetObserver;
      Arr2InnerDataSetObserver = array of Arr1InnerDataSetObserver;
      Arr3InnerDataSetObserver = array of Arr2InnerDataSetObserver;
      InnerItemInfo = class external 'android.support.v4.view' name 'ItemInfo' 
      end;

      InnerDataSetObserver = class external 'android.support.v4.view' name 'DataSetObserver' 
      end;

  public
    type
      InnerOnPageChangeListener = interface;
      Arr1InnerOnPageChangeListener = array of InnerOnPageChangeListener;
      Arr2InnerOnPageChangeListener = array of Arr1InnerOnPageChangeListener;
      Arr3InnerOnPageChangeListener = array of Arr2InnerOnPageChangeListener;
      InnerSavedState = class;
      Arr1InnerSavedState = array of InnerSavedState;
      Arr2InnerSavedState = array of Arr1InnerSavedState;
      Arr3InnerSavedState = array of Arr2InnerSavedState;
      InnerSimpleOnPageChangeListener = class;
      Arr1InnerSimpleOnPageChangeListener = array of InnerSimpleOnPageChangeListener;
      Arr2InnerSimpleOnPageChangeListener = array of Arr1InnerSimpleOnPageChangeListener;
      Arr3InnerSimpleOnPageChangeListener = array of Arr2InnerSimpleOnPageChangeListener;
      InnerOnPageChangeListener = interface external 'android.support.v4.view' name 'OnPageChangeListener' 
        procedure onPageScrolled(para1: jint; para2: jfloat; para3: jint); overload;
        procedure onPageSelected(para1: jint); overload;
        procedure onPageScrollStateChanged(para1: jint); overload;
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
    procedure setAdapter(para1: ASVVPagerAdapter); overload; virtual;
    function getAdapter(): ASVVPagerAdapter; overload; virtual;
    procedure setCurrentItem(para1: jint); overload; virtual;
    procedure setOnPageChangeListener(para1: ASVVViewPager.InnerOnPageChangeListener); overload; virtual;
    function onSaveInstanceState(): AOParcelable; overload; virtual;
    procedure onRestoreInstanceState(para1: AOParcelable); overload; virtual;
    procedure addView(para1: AVView; para2: jint; para3: AVViewGroup.InnerLayoutParams); overload; virtual;
  strict protected
    procedure onAttachedToWindow(); overload; virtual;
    procedure onMeasure(para1: jint; para2: jint); overload; virtual;
    procedure onSizeChanged(para1: jint; para2: jint; para3: jint; para4: jint); overload; virtual;
    procedure onLayout(para1: jboolean; para2: jint; para3: jint; para4: jint; para5: jint); overload; virtual;
  public
    procedure computeScroll(); overload; virtual;
    function onInterceptTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
    function onTouchEvent(para1: AVMotionEvent): jboolean; overload; virtual;
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

  ASVAFragmentPagerAdapter = class abstract external 'android.support.v4.app' name 'FragmentPagerAdapter' (ASVVPagerAdapter)
  public
    constructor create(para1: ASVAFragmentManager); overload;
    function getItem(para1: jint): ASVAFragment; overload; virtual; abstract;
    procedure startUpdate(para1: AVView); overload; virtual;
    function instantiateItem(para1: AVView; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVView; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVView); overload; virtual;
    function isViewFromObject(para1: AVView; para2: JLObject): jboolean; overload; virtual;
    function saveState(): AOParcelable; overload; virtual;
    procedure restoreState(para1: AOParcelable; para2: JLClassLoader); overload; virtual;
  end;

  ASVAFragmentStatePagerAdapter = class abstract external 'android.support.v4.app' name 'FragmentStatePagerAdapter' (ASVVPagerAdapter)
  public
    constructor create(para1: ASVAFragmentManager); overload;
    function getItem(para1: jint): ASVAFragment; overload; virtual; abstract;
    procedure startUpdate(para1: AVView); overload; virtual;
    function instantiateItem(para1: AVView; para2: jint): JLObject; overload; virtual;
    procedure destroyItem(para1: AVView; para2: jint; para3: JLObject); overload; virtual;
    procedure finishUpdate(para1: AVView); overload; virtual;
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
    function getDialog(): AADialog; overload; virtual;
    function getTheme(): jint; overload; virtual;
    procedure setCancelable(para1: jboolean); overload; virtual;
    function isCancelable(): jboolean; overload; virtual;
    procedure setShowsDialog(para1: jboolean); overload; virtual;
    function getShowsDialog(): jboolean; overload; virtual;
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
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
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
    function saveFragmentInstanceState(para1: ASVAFragment): ASVAFragment.InnerSavedState; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; para4: Arr1JLString); overload; virtual;
    procedure dump(para1: JLString; para2: JIFileDescriptor; para3: JIPrintWriter; var para4: array of JLString); overload; virtual;
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
    procedure attachActivity(para1: ASVAFragmentActivity); overload; virtual;
    procedure noteStateNotSaved(); overload; virtual;
    procedure dispatchCreate(); overload; virtual;
    procedure dispatchActivityCreated(); overload; virtual;
    procedure dispatchStart(); overload; virtual;
    procedure dispatchResume(); overload; virtual;
    procedure dispatchPause(); overload; virtual;
    procedure dispatchStop(); overload; virtual;
    procedure dispatchReallyStop(para1: jboolean); overload; virtual;
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

