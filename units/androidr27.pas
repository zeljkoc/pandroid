{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
{ Imports for Java packages/classes: android., java., javax., junit., org. }
unit androidr27;
{$mode delphi}

interface

type
  AManifest = class;
  Arr1AManifest = array of AManifest;
  Arr2AManifest = array of Arr1AManifest;
  Arr3AManifest = array of Arr2AManifest;

  AR = class;
  Arr1AR = array of AR;
  Arr2AR = array of Arr1AR;
  Arr3AR = array of Arr2AR;

  AAAbstractAccountAuthenticator = class;
  Arr1AAAbstractAccountAuthenticator = array of AAAbstractAccountAuthenticator;
  Arr2AAAbstractAccountAuthenticator = array of Arr1AAAbstractAccountAuthenticator;
  Arr3AAAbstractAccountAuthenticator = array of Arr2AAAbstractAccountAuthenticator;

  AAAccountManager = class;
  Arr1AAAccountManager = array of AAAccountManager;
  Arr2AAAccountManager = array of Arr1AAAccountManager;
  Arr3AAAccountManager = array of Arr2AAAccountManager;

  AAAccountManagerCallback = interface;
  Arr1AAAccountManagerCallback = array of AAAccountManagerCallback;
  Arr2AAAccountManagerCallback = array of Arr1AAAccountManagerCallback;
  Arr3AAAccountManagerCallback = array of Arr2AAAccountManagerCallback;

  AAAccountManagerFuture = interface;
  Arr1AAAccountManagerFuture = array of AAAccountManagerFuture;
  Arr2AAAccountManagerFuture = array of Arr1AAAccountManagerFuture;
  Arr3AAAccountManagerFuture = array of Arr2AAAccountManagerFuture;

  AAOnAccountsUpdateListener = interface;
  Arr1AAOnAccountsUpdateListener = array of AAOnAccountsUpdateListener;
  Arr2AAOnAccountsUpdateListener = array of Arr1AAOnAccountsUpdateListener;
  Arr3AAOnAccountsUpdateListener = array of Arr2AAOnAccountsUpdateListener;

  AAAnimatorInflater = class;
  Arr1AAAnimatorInflater = array of AAAnimatorInflater;
  Arr2AAAnimatorInflater = array of Arr1AAAnimatorInflater;
  Arr3AAAnimatorInflater = array of Arr2AAAnimatorInflater;

  AALayoutTransition = class;
  Arr1AALayoutTransition = array of AALayoutTransition;
  Arr2AALayoutTransition = array of Arr1AALayoutTransition;
  Arr3AALayoutTransition = array of Arr2AALayoutTransition;

  AATimeInterpolator = interface;
  Arr1AATimeInterpolator = array of AATimeInterpolator;
  Arr2AATimeInterpolator = array of Arr1AATimeInterpolator;
  Arr3AATimeInterpolator = array of Arr2AATimeInterpolator;

  AATypeEvaluator = interface;
  Arr1AATypeEvaluator = array of AATypeEvaluator;
  Arr2AATypeEvaluator = array of Arr1AATypeEvaluator;
  Arr3AATypeEvaluator = array of Arr2AATypeEvaluator;

  AOParcelable = interface;
  Arr1AOParcelable = array of AOParcelable;
  Arr2AOParcelable = array of Arr1AOParcelable;
  Arr3AOParcelable = array of Arr2AOParcelable;

  AAAlarmManager = class;
  Arr1AAAlarmManager = array of AAAlarmManager;
  Arr2AAAlarmManager = array of Arr1AAAlarmManager;
  Arr3AAAlarmManager = array of Arr2AAAlarmManager;

  AADownloadManager = class;
  Arr1AADownloadManager = array of AADownloadManager;
  Arr2AADownloadManager = array of Arr1AADownloadManager;
  Arr3AADownloadManager = array of Arr2AADownloadManager;

  AAFragmentTransaction = class;
  Arr1AAFragmentTransaction = array of AAFragmentTransaction;
  Arr2AAFragmentTransaction = array of Arr1AAFragmentTransaction;
  Arr3AAFragmentTransaction = array of Arr2AAFragmentTransaction;

  AAInstrumentation = class;
  Arr1AAInstrumentation = array of AAInstrumentation;
  Arr2AAInstrumentation = array of Arr1AAInstrumentation;
  Arr3AAInstrumentation = array of Arr2AAInstrumentation;

  AAKeyguardManager = class;
  Arr1AAKeyguardManager = array of AAKeyguardManager;
  Arr2AAKeyguardManager = array of Arr1AAKeyguardManager;
  Arr3AAKeyguardManager = array of Arr2AAKeyguardManager;

  AALoaderManager = class;
  Arr1AALoaderManager = array of AALoaderManager;
  Arr2AALoaderManager = array of Arr1AALoaderManager;
  Arr3AALoaderManager = array of Arr2AALoaderManager;

  AALocalActivityManager = class;
  Arr1AALocalActivityManager = array of AALocalActivityManager;
  Arr2AALocalActivityManager = array of Arr1AALocalActivityManager;
  Arr3AALocalActivityManager = array of Arr2AALocalActivityManager;

  AANotificationManager = class;
  Arr1AANotificationManager = array of AANotificationManager;
  Arr2AANotificationManager = array of Arr1AANotificationManager;
  Arr3AANotificationManager = array of Arr2AANotificationManager;

  AAUiModeManager = class;
  Arr1AAUiModeManager = array of AAUiModeManager;
  Arr2AAUiModeManager = array of Arr1AAUiModeManager;
  Arr3AAUiModeManager = array of Arr2AAUiModeManager;

  AAWallpaperManager = class;
  Arr1AAWallpaperManager = array of AAWallpaperManager;
  Arr2AAWallpaperManager = array of Arr1AAWallpaperManager;
  Arr3AAWallpaperManager = array of Arr2AAWallpaperManager;

  AAADevicePolicyManager = class;
  Arr1AAADevicePolicyManager = array of AAADevicePolicyManager;
  Arr2AAADevicePolicyManager = array of Arr1AAADevicePolicyManager;
  Arr3AAADevicePolicyManager = array of Arr2AAADevicePolicyManager;

  AABBackupDataInput = class;
  Arr1AABBackupDataInput = array of AABBackupDataInput;
  Arr2AABBackupDataInput = array of Arr1AABBackupDataInput;
  Arr3AABBackupDataInput = array of Arr2AABBackupDataInput;

  AABBackupDataOutput = class;
  Arr1AABBackupDataOutput = array of AABBackupDataOutput;
  Arr2AABBackupDataOutput = array of Arr1AABBackupDataOutput;
  Arr3AABBackupDataOutput = array of Arr2AABBackupDataOutput;

  AABBackupHelper = interface;
  Arr1AABBackupHelper = array of AABBackupHelper;
  Arr2AABBackupHelper = array of Arr1AABBackupHelper;
  Arr3AABBackupHelper = array of Arr2AABBackupHelper;

  AABBackupManager = class;
  Arr1AABBackupManager = array of AABBackupManager;
  Arr2AABBackupManager = array of Arr1AABBackupManager;
  Arr3AABBackupManager = array of Arr2AABBackupManager;

  AABFileBackupHelperBase = class;
  Arr1AABFileBackupHelperBase = array of AABFileBackupHelperBase;
  Arr2AABFileBackupHelperBase = array of Arr1AABFileBackupHelperBase;
  Arr3AABFileBackupHelperBase = array of Arr2AABFileBackupHelperBase;

  AABFullBackupDataOutput = class;
  Arr1AABFullBackupDataOutput = array of AABFullBackupDataOutput;
  Arr2AABFullBackupDataOutput = array of Arr1AABFullBackupDataOutput;
  Arr3AABFullBackupDataOutput = array of Arr2AABFullBackupDataOutput;

  AABRestoreObserver = class;
  Arr1AABRestoreObserver = array of AABRestoreObserver;
  Arr2AABRestoreObserver = array of Arr1AABRestoreObserver;
  Arr3AABRestoreObserver = array of Arr2AABRestoreObserver;

  AAAppWidgetHost = class;
  Arr1AAAppWidgetHost = array of AAAppWidgetHost;
  Arr2AAAppWidgetHost = array of Arr1AAAppWidgetHost;
  Arr3AAAppWidgetHost = array of Arr2AAAppWidgetHost;

  AAAppWidgetManager = class;
  Arr1AAAppWidgetManager = array of AAAppWidgetManager;
  Arr2AAAppWidgetManager = array of Arr1AAAppWidgetManager;
  Arr3AAAppWidgetManager = array of Arr2AAAppWidgetManager;

  ABBluetoothAssignedNumbers = class;
  Arr1ABBluetoothAssignedNumbers = array of ABBluetoothAssignedNumbers;
  Arr2ABBluetoothAssignedNumbers = array of Arr1ABBluetoothAssignedNumbers;
  Arr3ABBluetoothAssignedNumbers = array of Arr2ABBluetoothAssignedNumbers;

  ABBluetoothHealthCallback = class;
  Arr1ABBluetoothHealthCallback = array of ABBluetoothHealthCallback;
  Arr2ABBluetoothHealthCallback = array of Arr1ABBluetoothHealthCallback;
  Arr3ABBluetoothHealthCallback = array of Arr2ABBluetoothHealthCallback;

  ABBluetoothProfile = interface;
  Arr1ABBluetoothProfile = array of ABBluetoothProfile;
  Arr2ABBluetoothProfile = array of Arr1ABBluetoothProfile;
  Arr3ABBluetoothProfile = array of Arr2ABBluetoothProfile;

  ACAbstractThreadedSyncAdapter = class;
  Arr1ACAbstractThreadedSyncAdapter = array of ACAbstractThreadedSyncAdapter;
  Arr2ACAbstractThreadedSyncAdapter = array of Arr1ACAbstractThreadedSyncAdapter;
  Arr3ACAbstractThreadedSyncAdapter = array of Arr2ACAbstractThreadedSyncAdapter;

  ACBroadcastReceiver = class;
  Arr1ACBroadcastReceiver = array of ACBroadcastReceiver;
  Arr2ACBroadcastReceiver = array of Arr1ACBroadcastReceiver;
  Arr3ACBroadcastReceiver = array of Arr2ACBroadcastReceiver;

  ACComponentCallbacks = interface;
  Arr1ACComponentCallbacks = array of ACComponentCallbacks;
  Arr2ACComponentCallbacks = array of Arr1ACComponentCallbacks;
  Arr3ACComponentCallbacks = array of Arr2ACComponentCallbacks;

  JLAutoCloseable = interface;
  Arr1JLAutoCloseable = array of JLAutoCloseable;
  Arr2JLAutoCloseable = array of Arr1JLAutoCloseable;
  Arr3JLAutoCloseable = array of Arr2JLAutoCloseable;

  ACContentProviderClient = class;
  Arr1ACContentProviderClient = array of ACContentProviderClient;
  Arr2ACContentProviderClient = array of Arr1ACContentProviderClient;
  Arr3ACContentProviderClient = array of Arr2ACContentProviderClient;

  ACContentResolver = class;
  Arr1ACContentResolver = array of ACContentResolver;
  Arr2ACContentResolver = array of Arr1ACContentResolver;
  Arr3ACContentResolver = array of Arr2ACContentResolver;

  ACDialogInterface = interface;
  Arr1ACDialogInterface = array of ACDialogInterface;
  Arr2ACDialogInterface = array of Arr1ACDialogInterface;
  Arr3ACDialogInterface = array of Arr2ACDialogInterface;

  ACEntity = class;
  Arr1ACEntity = array of ACEntity;
  Arr2ACEntity = array of Arr1ACEntity;
  Arr3ACEntity = array of Arr2ACEntity;

  ACLoader = class;
  Arr1ACLoader = array of ACLoader;
  Arr2ACLoader = array of Arr1ACLoader;
  Arr3ACLoader = array of Arr2ACLoader;

  ACServiceConnection = interface;
  Arr1ACServiceConnection = array of ACServiceConnection;
  Arr2ACServiceConnection = array of Arr1ACServiceConnection;
  Arr3ACServiceConnection = array of Arr2ACServiceConnection;

  ACSharedPreferences = interface;
  Arr1ACSharedPreferences = array of ACSharedPreferences;
  Arr2ACSharedPreferences = array of Arr1ACSharedPreferences;
  Arr3ACSharedPreferences = array of Arr2ACSharedPreferences;

  ACSyncContext = class;
  Arr1ACSyncContext = array of ACSyncContext;
  Arr2ACSyncContext = array of Arr1ACSyncContext;
  Arr3ACSyncContext = array of Arr2ACSyncContext;

  ACSyncStatusObserver = interface;
  Arr1ACSyncStatusObserver = array of ACSyncStatusObserver;
  Arr2ACSyncStatusObserver = array of Arr1ACSyncStatusObserver;
  Arr3ACSyncStatusObserver = array of Arr2ACSyncStatusObserver;

  ACUriMatcher = class;
  Arr1ACUriMatcher = array of ACUriMatcher;
  Arr2ACUriMatcher = array of Arr1ACUriMatcher;
  Arr3ACUriMatcher = array of Arr2ACUriMatcher;

  ACRAssetManager = class;
  Arr1ACRAssetManager = array of ACRAssetManager;
  Arr2ACRAssetManager = array of Arr1ACRAssetManager;
  Arr3ACRAssetManager = array of Arr2ACRAssetManager;

  ACRObbScanner = class;
  Arr1ACRObbScanner = array of ACRObbScanner;
  Arr2ACRObbScanner = array of Arr1ACRObbScanner;
  Arr3ACRObbScanner = array of Arr2ACRObbScanner;

  ACRTypedArray = class;
  Arr1ACRTypedArray = array of ACRTypedArray;
  Arr2ACRTypedArray = array of Arr1ACRTypedArray;
  Arr3ACRTypedArray = array of Arr2ACRTypedArray;

  ADCharArrayBuffer = class;
  Arr1ADCharArrayBuffer = array of ADCharArrayBuffer;
  Arr2ADCharArrayBuffer = array of Arr1ADCharArrayBuffer;
  Arr3ADCharArrayBuffer = array of Arr2ADCharArrayBuffer;

  ADContentObserver = class;
  Arr1ADContentObserver = array of ADContentObserver;
  Arr2ADContentObserver = array of Arr1ADContentObserver;
  Arr3ADContentObserver = array of Arr2ADContentObserver;

  JICloseable = interface;
  Arr1JICloseable = array of JICloseable;
  Arr2JICloseable = array of Arr1JICloseable;
  Arr3JICloseable = array of Arr2JICloseable;

  ADCursor = interface;
  Arr1ADCursor = array of ADCursor;
  Arr2ADCursor = array of Arr1ADCursor;
  Arr3ADCursor = array of Arr2ADCursor;

  ADDataSetObserver = class;
  Arr1ADDataSetObserver = array of ADDataSetObserver;
  Arr2ADDataSetObserver = array of Arr1ADDataSetObserver;
  Arr3ADDataSetObserver = array of Arr2ADDataSetObserver;

  ADDatabaseErrorHandler = interface;
  Arr1ADDatabaseErrorHandler = array of ADDatabaseErrorHandler;
  Arr2ADDatabaseErrorHandler = array of Arr1ADDatabaseErrorHandler;
  Arr3ADDatabaseErrorHandler = array of Arr2ADDatabaseErrorHandler;

  ADDatabaseUtils = class;
  Arr1ADDatabaseUtils = array of ADDatabaseUtils;
  Arr2ADDatabaseUtils = array of Arr1ADDatabaseUtils;
  Arr3ADDatabaseUtils = array of Arr2ADDatabaseUtils;

  ADObservable = class;
  Arr1ADObservable = array of ADObservable;
  Arr2ADObservable = array of Arr1ADObservable;
  Arr3ADObservable = array of Arr2ADObservable;

  ADSSQLiteClosable = class;
  Arr1ADSSQLiteClosable = array of ADSSQLiteClosable;
  Arr2ADSSQLiteClosable = array of Arr1ADSSQLiteClosable;
  Arr3ADSSQLiteClosable = array of Arr2ADSSQLiteClosable;

  ADSSQLiteTransactionListener = interface;
  Arr1ADSSQLiteTransactionListener = array of ADSSQLiteTransactionListener;
  Arr2ADSSQLiteTransactionListener = array of Arr1ADSSQLiteTransactionListener;
  Arr3ADSSQLiteTransactionListener = array of Arr2ADSSQLiteTransactionListener;

  ADDrmConvertedStatus = class;
  Arr1ADDrmConvertedStatus = array of ADDrmConvertedStatus;
  Arr2ADDrmConvertedStatus = array of Arr1ADDrmConvertedStatus;
  Arr3ADDrmConvertedStatus = array of Arr2ADDrmConvertedStatus;

  ADDrmEvent = class;
  Arr1ADDrmEvent = array of ADDrmEvent;
  Arr2ADDrmEvent = array of Arr1ADDrmEvent;
  Arr3ADDrmEvent = array of Arr2ADDrmEvent;

  ADDrmInfo = class;
  Arr1ADDrmInfo = array of ADDrmInfo;
  Arr2ADDrmInfo = array of Arr1ADDrmInfo;
  Arr3ADDrmInfo = array of Arr2ADDrmInfo;

  ADDrmInfoRequest = class;
  Arr1ADDrmInfoRequest = array of ADDrmInfoRequest;
  Arr2ADDrmInfoRequest = array of Arr1ADDrmInfoRequest;
  Arr3ADDrmInfoRequest = array of Arr2ADDrmInfoRequest;

  ADDrmInfoStatus = class;
  Arr1ADDrmInfoStatus = array of ADDrmInfoStatus;
  Arr2ADDrmInfoStatus = array of Arr1ADDrmInfoStatus;
  Arr3ADDrmInfoStatus = array of Arr2ADDrmInfoStatus;

  ADDrmManagerClient = class;
  Arr1ADDrmManagerClient = array of ADDrmManagerClient;
  Arr2ADDrmManagerClient = array of Arr1ADDrmManagerClient;
  Arr3ADDrmManagerClient = array of Arr2ADDrmManagerClient;

  ADDrmRights = class;
  Arr1ADDrmRights = array of ADDrmRights;
  Arr2ADDrmRights = array of Arr1ADDrmRights;
  Arr3ADDrmRights = array of Arr2ADDrmRights;

  ADDrmStore = class;
  Arr1ADDrmStore = array of ADDrmStore;
  Arr2ADDrmStore = array of Arr1ADDrmStore;
  Arr3ADDrmStore = array of Arr2ADDrmStore;

  ADDrmSupportInfo = class;
  Arr1ADDrmSupportInfo = array of ADDrmSupportInfo;
  Arr2ADDrmSupportInfo = array of Arr1ADDrmSupportInfo;
  Arr3ADDrmSupportInfo = array of Arr2ADDrmSupportInfo;

  ADDrmUtils = class;
  Arr1ADDrmUtils = array of ADDrmUtils;
  Arr2ADDrmUtils = array of Arr1ADDrmUtils;
  Arr3ADDrmUtils = array of Arr2ADDrmUtils;

  ADProcessedData = class;
  Arr1ADProcessedData = array of ADProcessedData;
  Arr2ADProcessedData = array of Arr1ADProcessedData;
  Arr3ADProcessedData = array of Arr2ADProcessedData;

  AGGestureLibraries = class;
  Arr1AGGestureLibraries = array of AGGestureLibraries;
  Arr2AGGestureLibraries = array of Arr1AGGestureLibraries;
  Arr3AGGestureLibraries = array of Arr2AGGestureLibraries;

  AGGestureLibrary = class;
  Arr1AGGestureLibrary = array of AGGestureLibrary;
  Arr2AGGestureLibrary = array of Arr1AGGestureLibrary;
  Arr3AGGestureLibrary = array of Arr2AGGestureLibrary;

  AGGesturePoint = class;
  Arr1AGGesturePoint = array of AGGesturePoint;
  Arr2AGGesturePoint = array of Arr1AGGesturePoint;
  Arr3AGGesturePoint = array of Arr2AGGesturePoint;

  AGGestureStore = class;
  Arr1AGGestureStore = array of AGGestureStore;
  Arr2AGGestureStore = array of Arr1AGGestureStore;
  Arr3AGGestureStore = array of Arr2AGGestureStore;

  AGGestureStroke = class;
  Arr1AGGestureStroke = array of AGGestureStroke;
  Arr2AGGestureStroke = array of Arr1AGGestureStroke;
  Arr3AGGestureStroke = array of Arr2AGGestureStroke;

  AGGestureUtils = class;
  Arr1AGGestureUtils = array of AGGestureUtils;
  Arr2AGGestureUtils = array of Arr1AGGestureUtils;
  Arr3AGGestureUtils = array of Arr2AGGestureUtils;

  AGOrientedBoundingBox = class;
  Arr1AGOrientedBoundingBox = array of AGOrientedBoundingBox;
  Arr2AGOrientedBoundingBox = array of Arr1AGOrientedBoundingBox;
  Arr3AGOrientedBoundingBox = array of Arr2AGOrientedBoundingBox;

  AGPrediction = class;
  Arr1AGPrediction = array of AGPrediction;
  Arr2AGPrediction = array of Arr1AGPrediction;
  Arr3AGPrediction = array of Arr2AGPrediction;

  AGCamera = class;
  Arr1AGCamera = array of AGCamera;
  Arr2AGCamera = array of Arr1AGCamera;
  Arr3AGCamera = array of Arr2AGCamera;

  AGColor = class;
  Arr1AGColor = array of AGColor;
  Arr2AGColor = array of Arr1AGColor;
  Arr3AGColor = array of Arr2AGColor;

  AGColorFilter = class;
  Arr1AGColorFilter = array of AGColorFilter;
  Arr2AGColorFilter = array of Arr1AGColorFilter;
  Arr3AGColorFilter = array of Arr2AGColorFilter;

  AGColorMatrix = class;
  Arr1AGColorMatrix = array of AGColorMatrix;
  Arr2AGColorMatrix = array of Arr1AGColorMatrix;
  Arr3AGColorMatrix = array of Arr2AGColorMatrix;

  AGDrawFilter = class;
  Arr1AGDrawFilter = array of AGDrawFilter;
  Arr2AGDrawFilter = array of Arr1AGDrawFilter;
  Arr3AGDrawFilter = array of Arr2AGDrawFilter;

  AGImageFormat = class;
  Arr1AGImageFormat = array of AGImageFormat;
  Arr2AGImageFormat = array of Arr1AGImageFormat;
  Arr3AGImageFormat = array of Arr2AGImageFormat;

  AGMaskFilter = class;
  Arr1AGMaskFilter = array of AGMaskFilter;
  Arr2AGMaskFilter = array of Arr1AGMaskFilter;
  Arr3AGMaskFilter = array of Arr2AGMaskFilter;

  AGMovie = class;
  Arr1AGMovie = array of AGMovie;
  Arr2AGMovie = array of Arr1AGMovie;
  Arr3AGMovie = array of Arr2AGMovie;

  AGNinePatch = class;
  Arr1AGNinePatch = array of AGNinePatch;
  Arr2AGNinePatch = array of Arr1AGNinePatch;
  Arr3AGNinePatch = array of Arr2AGNinePatch;

  AGPathEffect = class;
  Arr1AGPathEffect = array of AGPathEffect;
  Arr2AGPathEffect = array of Arr1AGPathEffect;
  Arr3AGPathEffect = array of Arr2AGPathEffect;

  AGPathMeasure = class;
  Arr1AGPathMeasure = array of AGPathMeasure;
  Arr2AGPathMeasure = array of Arr1AGPathMeasure;
  Arr3AGPathMeasure = array of Arr2AGPathMeasure;

  AGPicture = class;
  Arr1AGPicture = array of AGPicture;
  Arr2AGPicture = array of Arr1AGPicture;
  Arr3AGPicture = array of Arr2AGPicture;

  AGPixelFormat = class;
  Arr1AGPixelFormat = array of AGPixelFormat;
  Arr2AGPixelFormat = array of Arr1AGPixelFormat;
  Arr3AGPixelFormat = array of Arr2AGPixelFormat;

  AGRasterizer = class;
  Arr1AGRasterizer = array of AGRasterizer;
  Arr2AGRasterizer = array of Arr1AGRasterizer;
  Arr3AGRasterizer = array of Arr2AGRasterizer;

  AGRegionIterator = class;
  Arr1AGRegionIterator = array of AGRegionIterator;
  Arr2AGRegionIterator = array of Arr1AGRegionIterator;
  Arr3AGRegionIterator = array of Arr2AGRegionIterator;

  AGTypeface = class;
  Arr1AGTypeface = array of AGTypeface;
  Arr2AGTypeface = array of Arr1AGTypeface;
  Arr3AGTypeface = array of Arr2AGTypeface;

  AGXfermode = class;
  Arr1AGXfermode = array of AGXfermode;
  Arr2AGXfermode = array of Arr1AGXfermode;
  Arr3AGXfermode = array of Arr2AGXfermode;

  AGYuvImage = class;
  Arr1AGYuvImage = array of AGYuvImage;
  Arr2AGYuvImage = array of Arr1AGYuvImage;
  Arr3AGYuvImage = array of Arr2AGYuvImage;

  AGDAnimatable = interface;
  Arr1AGDAnimatable = array of AGDAnimatable;
  Arr2AGDAnimatable = array of Arr1AGDAnimatable;
  Arr3AGDAnimatable = array of Arr2AGDAnimatable;

  AHCamera = class;
  Arr1AHCamera = array of AHCamera;
  Arr2AHCamera = array of Arr1AHCamera;
  Arr3AHCamera = array of Arr2AHCamera;

  AHGeomagneticField = class;
  Arr1AHGeomagneticField = array of AHGeomagneticField;
  Arr2AHGeomagneticField = array of Arr1AHGeomagneticField;
  Arr3AHGeomagneticField = array of Arr2AHGeomagneticField;

  AHSensor = class;
  Arr1AHSensor = array of AHSensor;
  Arr2AHSensor = array of Arr1AHSensor;
  Arr3AHSensor = array of Arr2AHSensor;

  AHSensorEvent = class;
  Arr1AHSensorEvent = array of AHSensorEvent;
  Arr2AHSensorEvent = array of Arr1AHSensorEvent;
  Arr3AHSensorEvent = array of Arr2AHSensorEvent;

  AHSensorEventListener = interface;
  Arr1AHSensorEventListener = array of AHSensorEventListener;
  Arr2AHSensorEventListener = array of Arr1AHSensorEventListener;
  Arr3AHSensorEventListener = array of Arr2AHSensorEventListener;

  AHSensorListener = interface;
  Arr1AHSensorListener = array of AHSensorListener;
  Arr2AHSensorListener = array of Arr1AHSensorListener;
  Arr3AHSensorListener = array of Arr2AHSensorListener;

  AHSensorManager = class;
  Arr1AHSensorManager = array of AHSensorManager;
  Arr2AHSensorManager = array of Arr1AHSensorManager;
  Arr3AHSensorManager = array of Arr2AHSensorManager;

  AHUUsbConstants = class;
  Arr1AHUUsbConstants = array of AHUUsbConstants;
  Arr2AHUUsbConstants = array of Arr1AHUUsbConstants;
  Arr3AHUUsbConstants = array of Arr2AHUUsbConstants;

  AHUUsbDeviceConnection = class;
  Arr1AHUUsbDeviceConnection = array of AHUUsbDeviceConnection;
  Arr2AHUUsbDeviceConnection = array of Arr1AHUUsbDeviceConnection;
  Arr3AHUUsbDeviceConnection = array of Arr2AHUUsbDeviceConnection;

  AHUUsbManager = class;
  Arr1AHUUsbManager = array of AHUUsbManager;
  Arr2AHUUsbManager = array of Arr1AHUUsbManager;
  Arr3AHUUsbManager = array of Arr2AHUUsbManager;

  AHUUsbRequest = class;
  Arr1AHUUsbRequest = array of AHUUsbRequest;
  Arr2AHUUsbRequest = array of Arr1AHUUsbRequest;
  Arr3AHUUsbRequest = array of Arr2AHUUsbRequest;

  AIKeyboard = class;
  Arr1AIKeyboard = array of AIKeyboard;
  Arr2AIKeyboard = array of Arr1AIKeyboard;
  Arr3AIKeyboard = array of Arr2AIKeyboard;

  ALGeocoder = class;
  Arr1ALGeocoder = array of ALGeocoder;
  Arr2ALGeocoder = array of Arr1ALGeocoder;
  Arr3ALGeocoder = array of Arr2ALGeocoder;

  ALGpsSatellite = class;
  Arr1ALGpsSatellite = array of ALGpsSatellite;
  Arr2ALGpsSatellite = array of Arr1ALGpsSatellite;
  Arr3ALGpsSatellite = array of Arr2ALGpsSatellite;

  ALGpsStatus = class;
  Arr1ALGpsStatus = array of ALGpsStatus;
  Arr2ALGpsStatus = array of Arr1ALGpsStatus;
  Arr3ALGpsStatus = array of Arr2ALGpsStatus;

  ALLocationListener = interface;
  Arr1ALLocationListener = array of ALLocationListener;
  Arr2ALLocationListener = array of Arr1ALLocationListener;
  Arr3ALLocationListener = array of Arr2ALLocationListener;

  ALLocationProvider = class;
  Arr1ALLocationProvider = array of ALLocationProvider;
  Arr2ALLocationProvider = array of Arr1ALLocationProvider;
  Arr3ALLocationProvider = array of Arr2ALLocationProvider;

  AMAsyncPlayer = class;
  Arr1AMAsyncPlayer = array of AMAsyncPlayer;
  Arr2AMAsyncPlayer = array of Arr1AMAsyncPlayer;
  Arr3AMAsyncPlayer = array of Arr2AMAsyncPlayer;

  AMAudioFormat = class;
  Arr1AMAudioFormat = array of AMAudioFormat;
  Arr2AMAudioFormat = array of Arr1AMAudioFormat;
  Arr3AMAudioFormat = array of Arr2AMAudioFormat;

  AMAudioManager = class;
  Arr1AMAudioManager = array of AMAudioManager;
  Arr2AMAudioManager = array of Arr1AMAudioManager;
  Arr3AMAudioManager = array of Arr2AMAudioManager;

  AMAudioRouting = interface;
  Arr1AMAudioRouting = array of AMAudioRouting;
  Arr2AMAudioRouting = array of Arr1AMAudioRouting;
  Arr3AMAudioRouting = array of Arr2AMAudioRouting;

  AMAudioRecord = class;
  Arr1AMAudioRecord = array of AMAudioRecord;
  Arr2AMAudioRecord = array of Arr1AMAudioRecord;
  Arr3AMAudioRecord = array of Arr2AMAudioRecord;

  AMAudioTrack = class;
  Arr1AMAudioTrack = array of AMAudioTrack;
  Arr2AMAudioTrack = array of Arr1AMAudioTrack;
  Arr3AMAudioTrack = array of Arr2AMAudioTrack;

  AMCamcorderProfile = class;
  Arr1AMCamcorderProfile = array of AMCamcorderProfile;
  Arr2AMCamcorderProfile = array of Arr1AMCamcorderProfile;
  Arr3AMCamcorderProfile = array of Arr2AMCamcorderProfile;

  AMCameraProfile = class;
  Arr1AMCameraProfile = array of AMCameraProfile;
  Arr2AMCameraProfile = array of Arr1AMCameraProfile;
  Arr3AMCameraProfile = array of Arr2AMCameraProfile;

  AMExifInterface = class;
  Arr1AMExifInterface = array of AMExifInterface;
  Arr2AMExifInterface = array of Arr1AMExifInterface;
  Arr3AMExifInterface = array of Arr2AMExifInterface;

  AMFaceDetector = class;
  Arr1AMFaceDetector = array of AMFaceDetector;
  Arr2AMFaceDetector = array of Arr1AMFaceDetector;
  Arr3AMFaceDetector = array of Arr2AMFaceDetector;

  AMJetPlayer = class;
  Arr1AMJetPlayer = array of AMJetPlayer;
  Arr2AMJetPlayer = array of Arr1AMJetPlayer;
  Arr3AMJetPlayer = array of Arr2AMJetPlayer;

  AMMediaMetadataRetriever = class;
  Arr1AMMediaMetadataRetriever = array of AMMediaMetadataRetriever;
  Arr2AMMediaMetadataRetriever = array of Arr1AMMediaMetadataRetriever;
  Arr3AMMediaMetadataRetriever = array of Arr2AMMediaMetadataRetriever;

  AMMediaPlayer = class;
  Arr1AMMediaPlayer = array of AMMediaPlayer;
  Arr2AMMediaPlayer = array of Arr1AMMediaPlayer;
  Arr3AMMediaPlayer = array of Arr2AMMediaPlayer;

  AMMediaRecorder = class;
  Arr1AMMediaRecorder = array of AMMediaRecorder;
  Arr2AMMediaRecorder = array of Arr1AMMediaRecorder;
  Arr3AMMediaRecorder = array of Arr2AMMediaRecorder;

  AMRemoteControlClient = class;
  Arr1AMRemoteControlClient = array of AMRemoteControlClient;
  Arr2AMRemoteControlClient = array of Arr1AMRemoteControlClient;
  Arr3AMRemoteControlClient = array of Arr2AMRemoteControlClient;

  AMRingtone = class;
  Arr1AMRingtone = array of AMRingtone;
  Arr2AMRingtone = array of Arr1AMRingtone;
  Arr3AMRingtone = array of Arr2AMRingtone;

  AMRingtoneManager = class;
  Arr1AMRingtoneManager = array of AMRingtoneManager;
  Arr2AMRingtoneManager = array of Arr1AMRingtoneManager;
  Arr3AMRingtoneManager = array of Arr2AMRingtoneManager;

  AMSoundPool = class;
  Arr1AMSoundPool = array of AMSoundPool;
  Arr2AMSoundPool = array of Arr1AMSoundPool;
  Arr3AMSoundPool = array of Arr2AMSoundPool;

  AMThumbnailUtils = class;
  Arr1AMThumbnailUtils = array of AMThumbnailUtils;
  Arr2AMThumbnailUtils = array of Arr1AMThumbnailUtils;
  Arr3AMThumbnailUtils = array of Arr2AMThumbnailUtils;

  AMToneGenerator = class;
  Arr1AMToneGenerator = array of AMToneGenerator;
  Arr2AMToneGenerator = array of Arr1AMToneGenerator;
  Arr3AMToneGenerator = array of Arr2AMToneGenerator;

  AMAAudioEffect = class;
  Arr1AMAAudioEffect = array of AMAAudioEffect;
  Arr2AMAAudioEffect = array of Arr1AMAAudioEffect;
  Arr3AMAAudioEffect = array of Arr2AMAAudioEffect;

  AMAVisualizer = class;
  Arr1AMAVisualizer = array of AMAVisualizer;
  Arr2AMAVisualizer = array of Arr1AMAVisualizer;
  Arr3AMAVisualizer = array of Arr2AMAVisualizer;

  AMEEffect = class;
  Arr1AMEEffect = array of AMEEffect;
  Arr2AMEEffect = array of Arr1AMEEffect;
  Arr3AMEEffect = array of Arr2AMEEffect;

  AMEEffectContext = class;
  Arr1AMEEffectContext = array of AMEEffectContext;
  Arr2AMEEffectContext = array of Arr1AMEEffectContext;
  Arr3AMEEffectContext = array of Arr2AMEEffectContext;

  AMEEffectFactory = class;
  Arr1AMEEffectFactory = array of AMEEffectFactory;
  Arr2AMEEffectFactory = array of Arr1AMEEffectFactory;
  Arr3AMEEffectFactory = array of Arr2AMEEffectFactory;

  AMEEffectUpdateListener = interface;
  Arr1AMEEffectUpdateListener = array of AMEEffectUpdateListener;
  Arr2AMEEffectUpdateListener = array of Arr1AMEEffectUpdateListener;
  Arr3AMEEffectUpdateListener = array of Arr2AMEEffectUpdateListener;

  AMMtpConstants = class;
  Arr1AMMtpConstants = array of AMMtpConstants;
  Arr2AMMtpConstants = array of Arr1AMMtpConstants;
  Arr3AMMtpConstants = array of Arr2AMMtpConstants;

  AMMtpDevice = class;
  Arr1AMMtpDevice = array of AMMtpDevice;
  Arr2AMMtpDevice = array of Arr1AMMtpDevice;
  Arr3AMMtpDevice = array of Arr2AMMtpDevice;

  AMMtpDeviceInfo = class;
  Arr1AMMtpDeviceInfo = array of AMMtpDeviceInfo;
  Arr2AMMtpDeviceInfo = array of Arr1AMMtpDeviceInfo;
  Arr3AMMtpDeviceInfo = array of Arr2AMMtpDeviceInfo;

  AMMtpObjectInfo = class;
  Arr1AMMtpObjectInfo = array of AMMtpObjectInfo;
  Arr2AMMtpObjectInfo = array of Arr1AMMtpObjectInfo;
  Arr3AMMtpObjectInfo = array of Arr2AMMtpObjectInfo;

  AMMtpStorageInfo = class;
  Arr1AMMtpStorageInfo = array of AMMtpStorageInfo;
  Arr2AMMtpStorageInfo = array of Arr1AMMtpStorageInfo;
  Arr3AMMtpStorageInfo = array of Arr2AMMtpStorageInfo;

  ANConnectivityManager = class;
  Arr1ANConnectivityManager = array of ANConnectivityManager;
  Arr2ANConnectivityManager = array of Arr1ANConnectivityManager;
  Arr3ANConnectivityManager = array of Arr2ANConnectivityManager;

  ANCredentials = class;
  Arr1ANCredentials = array of ANCredentials;
  Arr2ANCredentials = array of Arr1ANCredentials;
  Arr3ANCredentials = array of Arr2ANCredentials;

  ANLocalServerSocket = class;
  Arr1ANLocalServerSocket = array of ANLocalServerSocket;
  Arr2ANLocalServerSocket = array of Arr1ANLocalServerSocket;
  Arr3ANLocalServerSocket = array of Arr2ANLocalServerSocket;

  ANLocalSocket = class;
  Arr1ANLocalSocket = array of ANLocalSocket;
  Arr2ANLocalSocket = array of Arr1ANLocalSocket;
  Arr3ANLocalSocket = array of Arr2ANLocalSocket;

  ANMailTo = class;
  Arr1ANMailTo = array of ANMailTo;
  Arr2ANMailTo = array of Arr1ANMailTo;
  Arr3ANMailTo = array of Arr2ANMailTo;

  ANProxy = class;
  Arr1ANProxy = array of ANProxy;
  Arr2ANProxy = array of Arr1ANProxy;
  Arr3ANProxy = array of Arr2ANProxy;

  ANSSLSessionCache = class;
  Arr1ANSSLSessionCache = array of ANSSLSessionCache;
  Arr2ANSSLSessionCache = array of Arr1ANSSLSessionCache;
  Arr3ANSSLSessionCache = array of Arr2ANSSLSessionCache;

  ANTrafficStats = class;
  Arr1ANTrafficStats = array of ANTrafficStats;
  Arr2ANTrafficStats = array of Arr1ANTrafficStats;
  Arr3ANTrafficStats = array of Arr2ANTrafficStats;

  ANUrlQuerySanitizer = class;
  Arr1ANUrlQuerySanitizer = array of ANUrlQuerySanitizer;
  Arr2ANUrlQuerySanitizer = array of Arr1ANUrlQuerySanitizer;
  Arr3ANUrlQuerySanitizer = array of Arr2ANUrlQuerySanitizer;

  ANHSslCertificate = class;
  Arr1ANHSslCertificate = array of ANHSslCertificate;
  Arr2ANHSslCertificate = array of Arr1ANHSslCertificate;
  Arr3ANHSslCertificate = array of Arr2ANHSslCertificate;

  ANHSslError = class;
  Arr1ANHSslError = array of ANHSslError;
  Arr2ANHSslError = array of Arr1ANHSslError;
  Arr3ANHSslError = array of Arr2ANHSslError;

  ANRAudioCodec = class;
  Arr1ANRAudioCodec = array of ANRAudioCodec;
  Arr2ANRAudioCodec = array of Arr1ANRAudioCodec;
  Arr3ANRAudioCodec = array of Arr2ANRAudioCodec;

  ANRAudioGroup = class;
  Arr1ANRAudioGroup = array of ANRAudioGroup;
  Arr2ANRAudioGroup = array of Arr1ANRAudioGroup;
  Arr3ANRAudioGroup = array of Arr2ANRAudioGroup;

  ANRRtpStream = class;
  Arr1ANRRtpStream = array of ANRRtpStream;
  Arr2ANRRtpStream = array of Arr1ANRRtpStream;
  Arr3ANRRtpStream = array of Arr2ANRRtpStream;

  ANSSipAudioCall = class;
  Arr1ANSSipAudioCall = array of ANSSipAudioCall;
  Arr2ANSSipAudioCall = array of Arr1ANSSipAudioCall;
  Arr3ANSSipAudioCall = array of Arr2ANSSipAudioCall;

  ANSSipErrorCode = class;
  Arr1ANSSipErrorCode = array of ANSSipErrorCode;
  Arr2ANSSipErrorCode = array of Arr1ANSSipErrorCode;
  Arr3ANSSipErrorCode = array of Arr2ANSSipErrorCode;

  ANSSipRegistrationListener = interface;
  Arr1ANSSipRegistrationListener = array of ANSSipRegistrationListener;
  Arr2ANSSipRegistrationListener = array of Arr1ANSSipRegistrationListener;
  Arr3ANSSipRegistrationListener = array of Arr2ANSSipRegistrationListener;

  ANSSipSession = class;
  Arr1ANSSipSession = array of ANSSipSession;
  Arr2ANSSipSession = array of Arr1ANSSipSession;
  Arr3ANSSipSession = array of Arr2ANSSipSession;

  ANWWifiManager = class;
  Arr1ANWWifiManager = array of ANWWifiManager;
  Arr2ANWWifiManager = array of Arr1ANWWifiManager;
  Arr3ANWWifiManager = array of Arr2ANWWifiManager;

  ANWPWifiP2pManager = class;
  Arr1ANWPWifiP2pManager = array of ANWPWifiP2pManager;
  Arr2ANWPWifiP2pManager = array of Arr1ANWPWifiP2pManager;
  Arr3ANWPWifiP2pManager = array of Arr2ANWPWifiP2pManager;

  ANNfcAdapter = class;
  Arr1ANNfcAdapter = array of ANNfcAdapter;
  Arr2ANNfcAdapter = array of Arr1ANNfcAdapter;
  Arr3ANNfcAdapter = array of Arr2ANNfcAdapter;

  ANNfcEvent = class;
  Arr1ANNfcEvent = array of ANNfcEvent;
  Arr2ANNfcEvent = array of Arr1ANNfcEvent;
  Arr3ANNfcEvent = array of Arr2ANNfcEvent;

  ANNfcManager = class;
  Arr1ANNfcManager = array of ANNfcManager;
  Arr2ANNfcManager = array of Arr1ANNfcManager;
  Arr3ANNfcManager = array of Arr2ANNfcManager;

  AOETC1 = class;
  Arr1AOETC1 = array of AOETC1;
  Arr2AOETC1 = array of Arr1AOETC1;
  Arr3AOETC1 = array of Arr2AOETC1;

  AOETC1Util = class;
  Arr1AOETC1Util = array of AOETC1Util;
  Arr2AOETC1Util = array of Arr1AOETC1Util;
  Arr3AOETC1Util = array of Arr2AOETC1Util;

  AOGLDebugHelper = class;
  Arr1AOGLDebugHelper = array of AOGLDebugHelper;
  Arr2AOGLDebugHelper = array of Arr1AOGLDebugHelper;
  Arr3AOGLDebugHelper = array of Arr2AOGLDebugHelper;

  AOGLES10 = class;
  Arr1AOGLES10 = array of AOGLES10;
  Arr2AOGLES10 = array of Arr1AOGLES10;
  Arr3AOGLES10 = array of Arr2AOGLES10;

  AOGLES10Ext = class;
  Arr1AOGLES10Ext = array of AOGLES10Ext;
  Arr2AOGLES10Ext = array of Arr1AOGLES10Ext;
  Arr3AOGLES10Ext = array of Arr2AOGLES10Ext;

  AOGLES11Ext = class;
  Arr1AOGLES11Ext = array of AOGLES11Ext;
  Arr2AOGLES11Ext = array of Arr1AOGLES11Ext;
  Arr3AOGLES11Ext = array of Arr2AOGLES11Ext;

  AOGLES20 = class;
  Arr1AOGLES20 = array of AOGLES20;
  Arr2AOGLES20 = array of Arr1AOGLES20;
  Arr3AOGLES20 = array of Arr2AOGLES20;

  AOGLU = class;
  Arr1AOGLU = array of AOGLU;
  Arr2AOGLU = array of Arr1AOGLU;
  Arr3AOGLU = array of Arr2AOGLU;

  AOGLUtils = class;
  Arr1AOGLUtils = array of AOGLUtils;
  Arr2AOGLUtils = array of Arr1AOGLUtils;
  Arr3AOGLUtils = array of Arr2AOGLUtils;

  AOMatrix = class;
  Arr1AOMatrix = array of AOMatrix;
  Arr2AOMatrix = array of Arr1AOMatrix;
  Arr3AOMatrix = array of Arr2AOMatrix;

  AOVisibility = class;
  Arr1AOVisibility = array of AOVisibility;
  Arr2AOVisibility = array of Arr1AOVisibility;
  Arr3AOVisibility = array of Arr2AOVisibility;

  AOBatteryManager = class;
  Arr1AOBatteryManager = array of AOBatteryManager;
  Arr2AOBatteryManager = array of Arr1AOBatteryManager;
  Arr3AOBatteryManager = array of Arr2AOBatteryManager;

  AOBuild = class;
  Arr1AOBuild = array of AOBuild;
  Arr2AOBuild = array of Arr1AOBuild;
  Arr3AOBuild = array of Arr2AOBuild;

  AOConditionVariable = class;
  Arr1AOConditionVariable = array of AOConditionVariable;
  Arr2AOConditionVariable = array of Arr1AOConditionVariable;
  Arr3AOConditionVariable = array of Arr2AOConditionVariable;

  AOCountDownTimer = class;
  Arr1AOCountDownTimer = array of AOCountDownTimer;
  Arr2AOCountDownTimer = array of Arr1AOCountDownTimer;
  Arr3AOCountDownTimer = array of Arr2AOCountDownTimer;

  AOEnvironment = class;
  Arr1AOEnvironment = array of AOEnvironment;
  Arr2AOEnvironment = array of Arr1AOEnvironment;
  Arr3AOEnvironment = array of Arr2AOEnvironment;

  AOFileObserver = class;
  Arr1AOFileObserver = array of AOFileObserver;
  Arr2AOFileObserver = array of Arr1AOFileObserver;
  Arr3AOFileObserver = array of Arr2AOFileObserver;

  AOHandler = class;
  Arr1AOHandler = array of AOHandler;
  Arr2AOHandler = array of Arr1AOHandler;
  Arr3AOHandler = array of Arr2AOHandler;

  AOIBinder = interface;
  Arr1AOIBinder = array of AOIBinder;
  Arr2AOIBinder = array of Arr1AOIBinder;
  Arr3AOIBinder = array of Arr2AOIBinder;

  AOIInterface = interface;
  Arr1AOIInterface = array of AOIInterface;
  Arr2AOIInterface = array of Arr1AOIInterface;
  Arr3AOIInterface = array of Arr2AOIInterface;

  AOLooper = class;
  Arr1AOLooper = array of AOLooper;
  Arr2AOLooper = array of Arr1AOLooper;
  Arr3AOLooper = array of Arr2AOLooper;

  AOMemoryFile = class;
  Arr1AOMemoryFile = array of AOMemoryFile;
  Arr2AOMemoryFile = array of Arr1AOMemoryFile;
  Arr3AOMemoryFile = array of Arr2AOMemoryFile;

  AOMessageQueue = class;
  Arr1AOMessageQueue = array of AOMessageQueue;
  Arr2AOMessageQueue = array of Arr1AOMessageQueue;
  Arr3AOMessageQueue = array of Arr2AOMessageQueue;

  AOPowerManager = class;
  Arr1AOPowerManager = array of AOPowerManager;
  Arr2AOPowerManager = array of Arr1AOPowerManager;
  Arr3AOPowerManager = array of Arr2AOPowerManager;

  AOProcess = class;
  Arr1AOProcess = array of AOProcess;
  Arr2AOProcess = array of Arr1AOProcess;
  Arr3AOProcess = array of Arr2AOProcess;

  AORecoverySystem = class;
  Arr1AORecoverySystem = array of AORecoverySystem;
  Arr2AORecoverySystem = array of Arr1AORecoverySystem;
  Arr3AORecoverySystem = array of Arr2AORecoverySystem;

  AORemoteCallbackList = class;
  Arr1AORemoteCallbackList = array of AORemoteCallbackList;
  Arr2AORemoteCallbackList = array of Arr1AORemoteCallbackList;
  Arr3AORemoteCallbackList = array of Arr2AORemoteCallbackList;

  AOStatFs = class;
  Arr1AOStatFs = array of AOStatFs;
  Arr2AOStatFs = array of Arr1AOStatFs;
  Arr3AOStatFs = array of Arr2AOStatFs;

  AOStrictMode = class;
  Arr1AOStrictMode = array of AOStrictMode;
  Arr2AOStrictMode = array of Arr1AOStrictMode;
  Arr3AOStrictMode = array of Arr2AOStrictMode;

  AOSystemClock = class;
  Arr1AOSystemClock = array of AOSystemClock;
  Arr2AOSystemClock = array of Arr1AOSystemClock;
  Arr3AOSystemClock = array of Arr2AOSystemClock;

  AOTokenWatcher = class;
  Arr1AOTokenWatcher = array of AOTokenWatcher;
  Arr2AOTokenWatcher = array of Arr1AOTokenWatcher;
  Arr3AOTokenWatcher = array of Arr2AOTokenWatcher;

  AOVibrator = class;
  Arr1AOVibrator = array of AOVibrator;
  Arr2AOVibrator = array of Arr1AOVibrator;
  Arr3AOVibrator = array of Arr2AOVibrator;

  AOSOnObbStateChangeListener = class;
  Arr1AOSOnObbStateChangeListener = array of AOSOnObbStateChangeListener;
  Arr2AOSOnObbStateChangeListener = array of Arr1AOSOnObbStateChangeListener;
  Arr3AOSOnObbStateChangeListener = array of Arr2AOSOnObbStateChangeListener;

  AOSStorageManager = class;
  Arr1AOSStorageManager = array of AOSStorageManager;
  Arr2AOSStorageManager = array of Arr1AOSStorageManager;
  Arr3AOSStorageManager = array of Arr2AOSStorageManager;

  APPreferenceManager = class;
  Arr1APPreferenceManager = array of APPreferenceManager;
  Arr2APPreferenceManager = array of Arr1APPreferenceManager;
  Arr3APPreferenceManager = array of Arr2APPreferenceManager;

  APAlarmClock = class;
  Arr1APAlarmClock = array of APAlarmClock;
  Arr2APAlarmClock = array of Arr1APAlarmClock;
  Arr3APAlarmClock = array of Arr2APAlarmClock;

  APBaseColumns = interface;
  Arr1APBaseColumns = array of APBaseColumns;
  Arr2APBaseColumns = array of Arr1APBaseColumns;
  Arr3APBaseColumns = array of Arr2APBaseColumns;

  APOpenableColumns = interface;
  Arr1APOpenableColumns = array of APOpenableColumns;
  Arr2APOpenableColumns = array of Arr1APOpenableColumns;
  Arr3APOpenableColumns = array of Arr2APOpenableColumns;

  APSearchRecentSuggestions = class;
  Arr1APSearchRecentSuggestions = array of APSearchRecentSuggestions;
  Arr2APSearchRecentSuggestions = array of Arr1APSearchRecentSuggestions;
  Arr3APSearchRecentSuggestions = array of Arr2APSearchRecentSuggestions;

  ARBaseObj = class;
  Arr1ARBaseObj = array of ARBaseObj;
  Arr2ARBaseObj = array of Arr1ARBaseObj;
  Arr3ARBaseObj = array of Arr2ARBaseObj;

  ARByte2 = class;
  Arr1ARByte2 = array of ARByte2;
  Arr2ARByte2 = array of Arr1ARByte2;
  Arr3ARByte2 = array of Arr2ARByte2;

  ARByte3 = class;
  Arr1ARByte3 = array of ARByte3;
  Arr2ARByte3 = array of Arr1ARByte3;
  Arr3ARByte3 = array of Arr2ARByte3;

  ARByte4 = class;
  Arr1ARByte4 = array of ARByte4;
  Arr2ARByte4 = array of Arr1ARByte4;
  Arr3ARByte4 = array of Arr2ARByte4;

  ARDouble2 = class;
  Arr1ARDouble2 = array of ARDouble2;
  Arr2ARDouble2 = array of Arr1ARDouble2;
  Arr3ARDouble2 = array of Arr2ARDouble2;

  ARDouble3 = class;
  Arr1ARDouble3 = array of ARDouble3;
  Arr2ARDouble3 = array of Arr1ARDouble3;
  Arr3ARDouble3 = array of Arr2ARDouble3;

  ARDouble4 = class;
  Arr1ARDouble4 = array of ARDouble4;
  Arr2ARDouble4 = array of Arr1ARDouble4;
  Arr3ARDouble4 = array of Arr2ARDouble4;

  ARFieldPacker = class;
  Arr1ARFieldPacker = array of ARFieldPacker;
  Arr2ARFieldPacker = array of Arr1ARFieldPacker;
  Arr3ARFieldPacker = array of Arr2ARFieldPacker;

  ARFloat2 = class;
  Arr1ARFloat2 = array of ARFloat2;
  Arr2ARFloat2 = array of Arr1ARFloat2;
  Arr3ARFloat2 = array of Arr2ARFloat2;

  ARFloat3 = class;
  Arr1ARFloat3 = array of ARFloat3;
  Arr2ARFloat3 = array of Arr1ARFloat3;
  Arr3ARFloat3 = array of Arr2ARFloat3;

  ARFloat4 = class;
  Arr1ARFloat4 = array of ARFloat4;
  Arr2ARFloat4 = array of Arr1ARFloat4;
  Arr3ARFloat4 = array of Arr2ARFloat4;

  ARInt2 = class;
  Arr1ARInt2 = array of ARInt2;
  Arr2ARInt2 = array of Arr1ARInt2;
  Arr3ARInt2 = array of Arr2ARInt2;

  ARInt3 = class;
  Arr1ARInt3 = array of ARInt3;
  Arr2ARInt3 = array of Arr1ARInt3;
  Arr3ARInt3 = array of Arr2ARInt3;

  ARInt4 = class;
  Arr1ARInt4 = array of ARInt4;
  Arr2ARInt4 = array of Arr1ARInt4;
  Arr3ARInt4 = array of Arr2ARInt4;

  ARLong2 = class;
  Arr1ARLong2 = array of ARLong2;
  Arr2ARLong2 = array of Arr1ARLong2;
  Arr3ARLong2 = array of Arr2ARLong2;

  ARLong3 = class;
  Arr1ARLong3 = array of ARLong3;
  Arr2ARLong3 = array of Arr1ARLong3;
  Arr3ARLong3 = array of Arr2ARLong3;

  ARLong4 = class;
  Arr1ARLong4 = array of ARLong4;
  Arr2ARLong4 = array of Arr1ARLong4;
  Arr3ARLong4 = array of Arr2ARLong4;

  ARMatrix2f = class;
  Arr1ARMatrix2f = array of ARMatrix2f;
  Arr2ARMatrix2f = array of Arr1ARMatrix2f;
  Arr3ARMatrix2f = array of Arr2ARMatrix2f;

  ARMatrix3f = class;
  Arr1ARMatrix3f = array of ARMatrix3f;
  Arr2ARMatrix3f = array of Arr1ARMatrix3f;
  Arr3ARMatrix3f = array of Arr2ARMatrix3f;

  ARMatrix4f = class;
  Arr1ARMatrix4f = array of ARMatrix4f;
  Arr2ARMatrix4f = array of Arr1ARMatrix4f;
  Arr3ARMatrix4f = array of Arr2ARMatrix4f;

  ARShort2 = class;
  Arr1ARShort2 = array of ARShort2;
  Arr2ARShort2 = array of Arr1ARShort2;
  Arr3ARShort2 = array of Arr2ARShort2;

  ARShort3 = class;
  Arr1ARShort3 = array of ARShort3;
  Arr2ARShort3 = array of Arr1ARShort3;
  Arr3ARShort3 = array of Arr2ARShort3;

  ARShort4 = class;
  Arr1ARShort4 = array of ARShort4;
  Arr2ARShort4 = array of Arr1ARShort4;
  Arr3ARShort4 = array of Arr2ARShort4;

  ASElement = class;
  Arr1ASElement = array of ASElement;
  Arr2ASElement = array of Arr1ASElement;
  Arr3ASElement = array of Arr2ASElement;

  ASEndElementListener = interface;
  Arr1ASEndElementListener = array of ASEndElementListener;
  Arr2ASEndElementListener = array of Arr1ASEndElementListener;
  Arr3ASEndElementListener = array of Arr2ASEndElementListener;

  ASEndTextElementListener = interface;
  Arr1ASEndTextElementListener = array of ASEndTextElementListener;
  Arr2ASEndTextElementListener = array of Arr1ASEndTextElementListener;
  Arr3ASEndTextElementListener = array of Arr2ASEndTextElementListener;

  ASStartElementListener = interface;
  Arr1ASStartElementListener = array of ASStartElementListener;
  Arr2ASStartElementListener = array of Arr1ASStartElementListener;
  Arr3ASStartElementListener = array of Arr2ASStartElementListener;

  ASKeyChain = class;
  Arr1ASKeyChain = array of ASKeyChain;
  Arr2ASKeyChain = array of Arr1ASKeyChain;
  Arr3ASKeyChain = array of Arr2ASKeyChain;

  ASKeyChainAliasCallback = interface;
  Arr1ASKeyChainAliasCallback = array of ASKeyChainAliasCallback;
  Arr2ASKeyChainAliasCallback = array of Arr1ASKeyChainAliasCallback;
  Arr3ASKeyChainAliasCallback = array of Arr2ASKeyChainAliasCallback;

  ASRecognitionListener = interface;
  Arr1ASRecognitionListener = array of ASRecognitionListener;
  Arr2ASRecognitionListener = array of Arr1ASRecognitionListener;
  Arr3ASRecognitionListener = array of Arr2ASRecognitionListener;

  ASRecognizerIntent = class;
  Arr1ASRecognizerIntent = array of ASRecognizerIntent;
  Arr2ASRecognizerIntent = array of Arr1ASRecognizerIntent;
  Arr3ASRecognizerIntent = array of Arr2ASRecognizerIntent;

  ASRecognizerResultsIntent = class;
  Arr1ASRecognizerResultsIntent = array of ASRecognizerResultsIntent;
  Arr2ASRecognizerResultsIntent = array of Arr1ASRecognizerResultsIntent;
  Arr3ASRecognizerResultsIntent = array of Arr2ASRecognizerResultsIntent;

  ASSpeechRecognizer = class;
  Arr1ASSpeechRecognizer = array of ASSpeechRecognizer;
  Arr2ASSpeechRecognizer = array of Arr1ASSpeechRecognizer;
  Arr3ASSpeechRecognizer = array of Arr2ASSpeechRecognizer;

  ASTSynthesisCallback = interface;
  Arr1ASTSynthesisCallback = array of ASTSynthesisCallback;
  Arr2ASTSynthesisCallback = array of Arr1ASTSynthesisCallback;
  Arr3ASTSynthesisCallback = array of Arr2ASTSynthesisCallback;

  ASTSynthesisRequest = class;
  Arr1ASTSynthesisRequest = array of ASTSynthesisRequest;
  Arr2ASTSynthesisRequest = array of Arr1ASTSynthesisRequest;
  Arr3ASTSynthesisRequest = array of Arr2ASTSynthesisRequest;

  ASTTextToSpeech = class;
  Arr1ASTTextToSpeech = array of ASTTextToSpeech;
  Arr2ASTTextToSpeech = array of Arr1ASTTextToSpeech;
  Arr3ASTTextToSpeech = array of Arr2ASTTextToSpeech;

  ASTUtteranceProgressListener = class;
  Arr1ASTUtteranceProgressListener = array of ASTUtteranceProgressListener;
  Arr2ASTUtteranceProgressListener = array of Arr1ASTUtteranceProgressListener;
  Arr3ASTUtteranceProgressListener = array of Arr2ASTUtteranceProgressListener;

  ATCellLocation = class;
  Arr1ATCellLocation = array of ATCellLocation;
  Arr2ATCellLocation = array of Arr1ATCellLocation;
  Arr3ATCellLocation = array of Arr2ATCellLocation;

  ATPhoneNumberUtils = class;
  Arr1ATPhoneNumberUtils = array of ATPhoneNumberUtils;
  Arr2ATPhoneNumberUtils = array of Arr1ATPhoneNumberUtils;
  Arr3ATPhoneNumberUtils = array of Arr2ATPhoneNumberUtils;

  ATPhoneStateListener = class;
  Arr1ATPhoneStateListener = array of ATPhoneStateListener;
  Arr2ATPhoneStateListener = array of Arr1ATPhoneStateListener;
  Arr3ATPhoneStateListener = array of Arr2ATPhoneStateListener;

  ATSmsManager = class;
  Arr1ATSmsManager = array of ATSmsManager;
  Arr2ATSmsManager = array of Arr1ATSmsManager;
  Arr3ATSmsManager = array of Arr2ATSmsManager;

  ATTelephonyManager = class;
  Arr1ATTelephonyManager = array of ATTelephonyManager;
  Arr2ATTelephonyManager = array of Arr1ATTelephonyManager;
  Arr3ATTelephonyManager = array of Arr2ATTelephonyManager;

  ATGSmsManager = class;
  Arr1ATGSmsManager = array of ATGSmsManager;
  Arr2ATGSmsManager = array of Arr1ATGSmsManager;
  Arr3ATGSmsManager = array of Arr2ATGSmsManager;

  ATMoreAsserts = class;
  Arr1ATMoreAsserts = array of ATMoreAsserts;
  Arr2ATMoreAsserts = array of Arr1ATMoreAsserts;
  Arr3ATMoreAsserts = array of Arr2ATMoreAsserts;

  ATPerformanceTestCase = interface;
  Arr1ATPerformanceTestCase = array of ATPerformanceTestCase;
  Arr2ATPerformanceTestCase = array of Arr1ATPerformanceTestCase;
  Arr3ATPerformanceTestCase = array of Arr2ATPerformanceTestCase;

  ATTestSuiteProvider = interface;
  Arr1ATTestSuiteProvider = array of ATTestSuiteProvider;
  Arr2ATTestSuiteProvider = array of Arr1ATTestSuiteProvider;
  Arr3ATTestSuiteProvider = array of Arr2ATTestSuiteProvider;

  ATTouchUtils = class;
  Arr1ATTouchUtils = array of ATTouchUtils;
  Arr2ATTouchUtils = array of Arr1ATTouchUtils;
  Arr3ATTouchUtils = array of Arr2ATTouchUtils;

  ATViewAsserts = class;
  Arr1ATViewAsserts = array of ATViewAsserts;
  Arr2ATViewAsserts = array of Arr1ATViewAsserts;
  Arr3ATViewAsserts = array of Arr2ATViewAsserts;

  ATSTestMethod = class;
  Arr1ATSTestMethod = array of ATSTestMethod;
  Arr2ATSTestMethod = array of Arr1ATSTestMethod;
  Arr3ATSTestMethod = array of Arr2ATSTestMethod;

  ATAndroidCharacter = class;
  Arr1ATAndroidCharacter = array of ATAndroidCharacter;
  Arr2ATAndroidCharacter = array of Arr1ATAndroidCharacter;
  Arr3ATAndroidCharacter = array of Arr2ATAndroidCharacter;

  ATAutoText = class;
  Arr1ATAutoText = array of ATAutoText;
  Arr2ATAutoText = array of Arr1ATAutoText;
  Arr3ATAutoText = array of Arr2ATAutoText;

  ATClipboardManager = class;
  Arr1ATClipboardManager = array of ATClipboardManager;
  Arr2ATClipboardManager = array of Arr1ATClipboardManager;
  Arr3ATClipboardManager = array of Arr2ATClipboardManager;

  ATHtml = class;
  Arr1ATHtml = array of ATHtml;
  Arr2ATHtml = array of Arr1ATHtml;
  Arr3ATHtml = array of Arr2ATHtml;

  ATInputFilter = interface;
  Arr1ATInputFilter = array of ATInputFilter;
  Arr2ATInputFilter = array of Arr1ATInputFilter;
  Arr3ATInputFilter = array of Arr2ATInputFilter;

  ATInputType = interface;
  Arr1ATInputType = array of ATInputType;
  Arr2ATInputType = array of Arr1ATInputType;
  Arr3ATInputType = array of Arr2ATInputType;

  ATNoCopySpan = interface;
  Arr1ATNoCopySpan = array of ATNoCopySpan;
  Arr2ATNoCopySpan = array of Arr1ATNoCopySpan;
  Arr3ATNoCopySpan = array of Arr2ATNoCopySpan;

  ATSelection = class;
  Arr1ATSelection = array of ATSelection;
  Arr2ATSelection = array of Arr1ATSelection;
  Arr3ATSelection = array of Arr2ATSelection;

  ATSpannableStringInternal = class;
  Arr1ATSpannableStringInternal = array of ATSpannableStringInternal;
  Arr2ATSpannableStringInternal = array of Arr1ATSpannableStringInternal;
  Arr3ATSpannableStringInternal = array of Arr2ATSpannableStringInternal;

  ATFDateFormat = class;
  Arr1ATFDateFormat = array of ATFDateFormat;
  Arr2ATFDateFormat = array of Arr1ATFDateFormat;
  Arr3ATFDateFormat = array of Arr2ATFDateFormat;

  ATFDateUtils = class;
  Arr1ATFDateUtils = array of ATFDateUtils;
  Arr2ATFDateUtils = array of Arr1ATFDateUtils;
  Arr3ATFDateUtils = array of Arr2ATFDateUtils;

  ATFFormatter = class;
  Arr1ATFFormatter = array of ATFFormatter;
  Arr2ATFFormatter = array of Arr1ATFFormatter;
  Arr3ATFFormatter = array of Arr2ATFFormatter;

  ATFTime = class;
  Arr1ATFTime = array of ATFTime;
  Arr2ATFTime = array of Arr1ATFTime;
  Arr3ATFTime = array of Arr2ATFTime;

  ATMKeyListener = interface;
  Arr1ATMKeyListener = array of ATMKeyListener;
  Arr2ATMKeyListener = array of Arr1ATMKeyListener;
  Arr3ATMKeyListener = array of Arr2ATMKeyListener;

  ATMMetaKeyKeyListener = class;
  Arr1ATMMetaKeyKeyListener = array of ATMMetaKeyKeyListener;
  Arr2ATMMetaKeyKeyListener = array of Arr1ATMMetaKeyKeyListener;
  Arr3ATMMetaKeyKeyListener = array of Arr2ATMMetaKeyKeyListener;

  ATMMovementMethod = interface;
  Arr1ATMMovementMethod = array of ATMMovementMethod;
  Arr2ATMMovementMethod = array of Arr1ATMMovementMethod;
  Arr3ATMMovementMethod = array of Arr2ATMMovementMethod;

  ATMTouch = class;
  Arr1ATMTouch = array of ATMTouch;
  Arr2ATMTouch = array of Arr1ATMTouch;
  Arr3ATMTouch = array of Arr2ATMTouch;

  ATMTransformationMethod = interface;
  Arr1ATMTransformationMethod = array of ATMTransformationMethod;
  Arr2ATMTransformationMethod = array of Arr1ATMTransformationMethod;
  Arr3ATMTransformationMethod = array of Arr2ATMTransformationMethod;

  ATSCharacterStyle = class;
  Arr1ATSCharacterStyle = array of ATSCharacterStyle;
  Arr2ATSCharacterStyle = array of Arr1ATSCharacterStyle;
  Arr3ATSCharacterStyle = array of Arr2ATSCharacterStyle;

  ATSParagraphStyle = interface;
  Arr1ATSParagraphStyle = array of ATSParagraphStyle;
  Arr2ATSParagraphStyle = array of Arr1ATSParagraphStyle;
  Arr3ATSParagraphStyle = array of Arr2ATSParagraphStyle;

  ATSUpdateAppearance = interface;
  Arr1ATSUpdateAppearance = array of ATSUpdateAppearance;
  Arr2ATSUpdateAppearance = array of Arr1ATSUpdateAppearance;
  Arr3ATSUpdateAppearance = array of Arr2ATSUpdateAppearance;

  ATULinkify = class;
  Arr1ATULinkify = array of ATULinkify;
  Arr2ATULinkify = array of Arr1ATULinkify;
  Arr3ATULinkify = array of Arr2ATULinkify;

  ATURfc822Token = class;
  Arr1ATURfc822Token = array of ATURfc822Token;
  Arr2ATURfc822Token = array of Arr1ATURfc822Token;
  Arr3ATURfc822Token = array of Arr2ATURfc822Token;

  AUAttributeSet = interface;
  Arr1AUAttributeSet = array of AUAttributeSet;
  Arr2AUAttributeSet = array of Arr1AUAttributeSet;
  Arr3AUAttributeSet = array of Arr2AUAttributeSet;

  AUBase64 = class;
  Arr1AUBase64 = array of AUBase64;
  Arr2AUBase64 = array of Arr1AUBase64;
  Arr3AUBase64 = array of Arr2AUBase64;

  AUConfig = class;
  Arr1AUConfig = array of AUConfig;
  Arr2AUConfig = array of Arr1AUConfig;
  Arr3AUConfig = array of Arr2AUConfig;

  AUDebugUtils = class;
  Arr1AUDebugUtils = array of AUDebugUtils;
  Arr2AUDebugUtils = array of Arr1AUDebugUtils;
  Arr3AUDebugUtils = array of Arr2AUDebugUtils;

  AUDisplayMetrics = class;
  Arr1AUDisplayMetrics = array of AUDisplayMetrics;
  Arr2AUDisplayMetrics = array of Arr1AUDisplayMetrics;
  Arr3AUDisplayMetrics = array of Arr2AUDisplayMetrics;

  AUEventLog = class;
  Arr1AUEventLog = array of AUEventLog;
  Arr2AUEventLog = array of Arr1AUEventLog;
  Arr3AUEventLog = array of Arr2AUEventLog;

  AUEventLogTags = class;
  Arr1AUEventLogTags = array of AUEventLogTags;
  Arr2AUEventLogTags = array of Arr1AUEventLogTags;
  Arr3AUEventLogTags = array of Arr2AUEventLogTags;

  AUFloatMath = class;
  Arr1AUFloatMath = array of AUFloatMath;
  Arr2AUFloatMath = array of Arr1AUFloatMath;
  Arr3AUFloatMath = array of Arr2AUFloatMath;

  AULog = class;
  Arr1AULog = array of AULog;
  Arr2AULog = array of Arr1AULog;
  Arr3AULog = array of Arr2AULog;

  AULruCache = class;
  Arr1AULruCache = array of AULruCache;
  Arr2AULruCache = array of Arr1AULruCache;
  Arr3AULruCache = array of Arr2AULruCache;

  AUMonthDisplayHelper = class;
  Arr1AUMonthDisplayHelper = array of AUMonthDisplayHelper;
  Arr2AUMonthDisplayHelper = array of Arr1AUMonthDisplayHelper;
  Arr3AUMonthDisplayHelper = array of Arr2AUMonthDisplayHelper;

  AUPair = class;
  Arr1AUPair = array of AUPair;
  Arr2AUPair = array of Arr1AUPair;
  Arr3AUPair = array of Arr2AUPair;

  AUPatterns = class;
  Arr1AUPatterns = array of AUPatterns;
  Arr2AUPatterns = array of Arr1AUPatterns;
  Arr3AUPatterns = array of Arr2AUPatterns;

  AUPrinter = interface;
  Arr1AUPrinter = array of AUPrinter;
  Arr2AUPrinter = array of Arr1AUPrinter;
  Arr3AUPrinter = array of Arr2AUPrinter;

  AUProperty = class;
  Arr1AUProperty = array of AUProperty;
  Arr2AUProperty = array of Arr1AUProperty;
  Arr3AUProperty = array of Arr2AUProperty;

  AUStateSet = class;
  Arr1AUStateSet = array of AUStateSet;
  Arr2AUStateSet = array of Arr1AUStateSet;
  Arr3AUStateSet = array of Arr2AUStateSet;

  AUTimeUtils = class;
  Arr1AUTimeUtils = array of AUTimeUtils;
  Arr2AUTimeUtils = array of Arr1AUTimeUtils;
  Arr3AUTimeUtils = array of Arr2AUTimeUtils;

  AUTimingLogger = class;
  Arr1AUTimingLogger = array of AUTimingLogger;
  Arr2AUTimingLogger = array of Arr1AUTimingLogger;
  Arr3AUTimingLogger = array of Arr2AUTimingLogger;

  AUTypedValue = class;
  Arr1AUTypedValue = array of AUTypedValue;
  Arr2AUTypedValue = array of Arr1AUTypedValue;
  Arr3AUTypedValue = array of Arr2AUTypedValue;

  AVActionMode = class;
  Arr1AVActionMode = array of AVActionMode;
  Arr2AVActionMode = array of Arr1AVActionMode;
  Arr3AVActionMode = array of Arr2AVActionMode;

  AVActionProvider = class;
  Arr1AVActionProvider = array of AVActionProvider;
  Arr2AVActionProvider = array of Arr1AVActionProvider;
  Arr3AVActionProvider = array of Arr2AVActionProvider;

  AVCollapsibleActionView = interface;
  Arr1AVCollapsibleActionView = array of AVCollapsibleActionView;
  Arr2AVCollapsibleActionView = array of Arr1AVCollapsibleActionView;
  Arr3AVCollapsibleActionView = array of Arr2AVCollapsibleActionView;

  AVDisplay = class;
  Arr1AVDisplay = array of AVDisplay;
  Arr2AVDisplay = array of Arr1AVDisplay;
  Arr3AVDisplay = array of Arr2AVDisplay;

  AVFocusFinder = class;
  Arr1AVFocusFinder = array of AVFocusFinder;
  Arr2AVFocusFinder = array of Arr1AVFocusFinder;
  Arr3AVFocusFinder = array of Arr2AVFocusFinder;

  AVGestureDetector = class;
  Arr1AVGestureDetector = array of AVGestureDetector;
  Arr2AVGestureDetector = array of Arr1AVGestureDetector;
  Arr3AVGestureDetector = array of Arr2AVGestureDetector;

  AVGravity = class;
  Arr1AVGravity = array of AVGravity;
  Arr2AVGravity = array of Arr1AVGravity;
  Arr3AVGravity = array of Arr2AVGravity;

  AVHapticFeedbackConstants = class;
  Arr1AVHapticFeedbackConstants = array of AVHapticFeedbackConstants;
  Arr2AVHapticFeedbackConstants = array of Arr1AVHapticFeedbackConstants;
  Arr3AVHapticFeedbackConstants = array of Arr2AVHapticFeedbackConstants;

  AVInputQueue = class;
  Arr1AVInputQueue = array of AVInputQueue;
  Arr2AVInputQueue = array of Arr1AVInputQueue;
  Arr3AVInputQueue = array of Arr2AVInputQueue;

  AVLayoutInflater = class;
  Arr1AVLayoutInflater = array of AVLayoutInflater;
  Arr2AVLayoutInflater = array of Arr1AVLayoutInflater;
  Arr3AVLayoutInflater = array of Arr2AVLayoutInflater;

  AVMenu = interface;
  Arr1AVMenu = array of AVMenu;
  Arr2AVMenu = array of Arr1AVMenu;
  Arr3AVMenu = array of Arr2AVMenu;

  AVMenuInflater = class;
  Arr1AVMenuInflater = array of AVMenuInflater;
  Arr2AVMenuInflater = array of Arr1AVMenuInflater;
  Arr3AVMenuInflater = array of Arr2AVMenuInflater;

  AVOrientationEventListener = class;
  Arr1AVOrientationEventListener = array of AVOrientationEventListener;
  Arr2AVOrientationEventListener = array of Arr1AVOrientationEventListener;
  Arr3AVOrientationEventListener = array of Arr2AVOrientationEventListener;

  AVScaleGestureDetector = class;
  Arr1AVScaleGestureDetector = array of AVScaleGestureDetector;
  Arr2AVScaleGestureDetector = array of Arr1AVScaleGestureDetector;
  Arr3AVScaleGestureDetector = array of Arr2AVScaleGestureDetector;

  AVSoundEffectConstants = class;
  Arr1AVSoundEffectConstants = array of AVSoundEffectConstants;
  Arr2AVSoundEffectConstants = array of Arr1AVSoundEffectConstants;
  Arr3AVSoundEffectConstants = array of Arr2AVSoundEffectConstants;

  AVTouchDelegate = class;
  Arr1AVTouchDelegate = array of AVTouchDelegate;
  Arr2AVTouchDelegate = array of Arr1AVTouchDelegate;
  Arr3AVTouchDelegate = array of Arr2AVTouchDelegate;

  AVVelocityTracker = class;
  Arr1AVVelocityTracker = array of AVVelocityTracker;
  Arr2AVVelocityTracker = array of Arr1AVVelocityTracker;
  Arr3AVVelocityTracker = array of Arr2AVVelocityTracker;

  AVViewConfiguration = class;
  Arr1AVViewConfiguration = array of AVViewConfiguration;
  Arr2AVViewConfiguration = array of Arr1AVViewConfiguration;
  Arr3AVViewConfiguration = array of Arr2AVViewConfiguration;

  AVViewManager = interface;
  Arr1AVViewManager = array of AVViewManager;
  Arr2AVViewManager = array of Arr1AVViewManager;
  Arr3AVViewManager = array of Arr2AVViewManager;

  AVViewTreeObserver = class;
  Arr1AVViewTreeObserver = array of AVViewTreeObserver;
  Arr2AVViewTreeObserver = array of Arr1AVViewTreeObserver;
  Arr3AVViewTreeObserver = array of Arr2AVViewTreeObserver;

  AVAAccessibilityEventSource = interface;
  Arr1AVAAccessibilityEventSource = array of AVAAccessibilityEventSource;
  Arr2AVAAccessibilityEventSource = array of Arr1AVAAccessibilityEventSource;
  Arr3AVAAccessibilityEventSource = array of Arr2AVAAccessibilityEventSource;

  AVAAccessibilityManager = class;
  Arr1AVAAccessibilityManager = array of AVAAccessibilityManager;
  Arr2AVAAccessibilityManager = array of Arr1AVAAccessibilityManager;
  Arr3AVAAccessibilityManager = array of Arr2AVAAccessibilityManager;

  AVAAccessibilityRecord = class;
  Arr1AVAAccessibilityRecord = array of AVAAccessibilityRecord;
  Arr2AVAAccessibilityRecord = array of Arr1AVAAccessibilityRecord;
  Arr3AVAAccessibilityRecord = array of Arr2AVAAccessibilityRecord;

  AVAAnimationUtils = class;
  Arr1AVAAnimationUtils = array of AVAAnimationUtils;
  Arr2AVAAnimationUtils = array of Arr1AVAAnimationUtils;
  Arr3AVAAnimationUtils = array of Arr2AVAAnimationUtils;

  AVALayoutAnimationController = class;
  Arr1AVALayoutAnimationController = array of AVALayoutAnimationController;
  Arr2AVALayoutAnimationController = array of Arr1AVALayoutAnimationController;
  Arr3AVALayoutAnimationController = array of Arr2AVALayoutAnimationController;

  AVATransformation = class;
  Arr1AVATransformation = array of AVATransformation;
  Arr2AVATransformation = array of Arr1AVATransformation;
  Arr3AVATransformation = array of Arr2AVATransformation;

  AVIInputConnection = interface;
  Arr1AVIInputConnection = array of AVIInputConnection;
  Arr2AVIInputConnection = array of Arr1AVIInputConnection;
  Arr3AVIInputConnection = array of Arr2AVIInputConnection;

  AVIInputMethod = interface;
  Arr1AVIInputMethod = array of AVIInputMethod;
  Arr2AVIInputMethod = array of Arr1AVIInputMethod;
  Arr3AVIInputMethod = array of Arr2AVIInputMethod;

  AVIInputMethodManager = class;
  Arr1AVIInputMethodManager = array of AVIInputMethodManager;
  Arr2AVIInputMethodManager = array of Arr1AVIInputMethodManager;
  Arr3AVIInputMethodManager = array of Arr2AVIInputMethodManager;

  AVIInputMethodSession = interface;
  Arr1AVIInputMethodSession = array of AVIInputMethodSession;
  Arr2AVIInputMethodSession = array of Arr1AVIInputMethodSession;
  Arr3AVIInputMethodSession = array of Arr2AVIInputMethodSession;

  AVTSpellCheckerSession = class;
  Arr1AVTSpellCheckerSession = array of AVTSpellCheckerSession;
  Arr2AVTSpellCheckerSession = array of Arr1AVTSpellCheckerSession;
  Arr3AVTSpellCheckerSession = array of Arr2AVTSpellCheckerSession;

  AWCookieManager = class;
  Arr1AWCookieManager = array of AWCookieManager;
  Arr2AWCookieManager = array of Arr1AWCookieManager;
  Arr3AWCookieManager = array of Arr2AWCookieManager;

  AWDateSorter = class;
  Arr1AWDateSorter = array of AWDateSorter;
  Arr2AWDateSorter = array of Arr1AWDateSorter;
  Arr3AWDateSorter = array of Arr2AWDateSorter;

  AWDownloadListener = interface;
  Arr1AWDownloadListener = array of AWDownloadListener;
  Arr2AWDownloadListener = array of Arr1AWDownloadListener;
  Arr3AWDownloadListener = array of Arr2AWDownloadListener;

  AWGeolocationPermissions = class;
  Arr1AWGeolocationPermissions = array of AWGeolocationPermissions;
  Arr2AWGeolocationPermissions = array of Arr1AWGeolocationPermissions;
  Arr3AWGeolocationPermissions = array of Arr2AWGeolocationPermissions;

  AWJsResult = class;
  Arr1AWJsResult = array of AWJsResult;
  Arr2AWJsResult = array of Arr1AWJsResult;
  Arr3AWJsResult = array of Arr2AWJsResult;

  AWMimeTypeMap = class;
  Arr1AWMimeTypeMap = array of AWMimeTypeMap;
  Arr2AWMimeTypeMap = array of Arr1AWMimeTypeMap;
  Arr3AWMimeTypeMap = array of Arr2AWMimeTypeMap;

  AWPluginStub = interface;
  Arr1AWPluginStub = array of AWPluginStub;
  Arr2AWPluginStub = array of Arr1AWPluginStub;
  Arr3AWPluginStub = array of Arr2AWPluginStub;

  AWURLUtil = class;
  Arr1AWURLUtil = array of AWURLUtil;
  Arr2AWURLUtil = array of Arr1AWURLUtil;
  Arr3AWURLUtil = array of Arr2AWURLUtil;

  AWValueCallback = interface;
  Arr1AWValueCallback = array of AWValueCallback;
  Arr2AWValueCallback = array of Arr1AWValueCallback;
  Arr3AWValueCallback = array of Arr2AWValueCallback;

  AWWebIconDatabase = class;
  Arr1AWWebIconDatabase = array of AWWebIconDatabase;
  Arr2AWWebIconDatabase = array of Arr1AWWebIconDatabase;
  Arr3AWWebIconDatabase = array of Arr2AWWebIconDatabase;

  AWWebResourceResponse = class;
  Arr1AWWebResourceResponse = array of AWWebResourceResponse;
  Arr2AWWebResourceResponse = array of Arr1AWWebResourceResponse;
  Arr3AWWebResourceResponse = array of Arr2AWWebResourceResponse;

  AWWebStorage = class;
  Arr1AWWebStorage = array of AWWebStorage;
  Arr2AWWebStorage = array of Arr1AWWebStorage;
  Arr3AWWebStorage = array of Arr2AWWebStorage;

  AWWebViewClient = class;
  Arr1AWWebViewClient = array of AWWebViewClient;
  Arr2AWWebViewClient = array of Arr1AWWebViewClient;
  Arr3AWWebViewClient = array of Arr2AWWebViewClient;

  AWWebViewDatabase = class;
  Arr1AWWebViewDatabase = array of AWWebViewDatabase;
  Arr2AWWebViewDatabase = array of Arr1AWWebViewDatabase;
  Arr3AWWebViewDatabase = array of Arr2AWWebViewDatabase;

  AWAdapter = interface;
  Arr1AWAdapter = array of AWAdapter;
  Arr2AWAdapter = array of Arr1AWAdapter;
  Arr3AWAdapter = array of Arr2AWAdapter;

  AWCheckable = interface;
  Arr1AWCheckable = array of AWCheckable;
  Arr2AWCheckable = array of Arr1AWCheckable;
  Arr3AWCheckable = array of Arr2AWCheckable;

  AWEdgeEffect = class;
  Arr1AWEdgeEffect = array of AWEdgeEffect;
  Arr2AWEdgeEffect = array of Arr1AWEdgeEffect;
  Arr3AWEdgeEffect = array of Arr2AWEdgeEffect;

  AWExpandableListAdapter = interface;
  Arr1AWExpandableListAdapter = array of AWExpandableListAdapter;
  Arr2AWExpandableListAdapter = array of Arr1AWExpandableListAdapter;
  Arr3AWExpandableListAdapter = array of Arr2AWExpandableListAdapter;

  AWFilter = class;
  Arr1AWFilter = array of AWFilter;
  Arr2AWFilter = array of Arr1AWFilter;
  Arr3AWFilter = array of Arr2AWFilter;

  AWFilterQueryProvider = interface;
  Arr1AWFilterQueryProvider = array of AWFilterQueryProvider;
  Arr2AWFilterQueryProvider = array of Arr1AWFilterQueryProvider;
  Arr3AWFilterQueryProvider = array of Arr2AWFilterQueryProvider;

  AWFilterable = interface;
  Arr1AWFilterable = array of AWFilterable;
  Arr2AWFilterable = array of Arr1AWFilterable;
  Arr3AWFilterable = array of Arr2AWFilterable;

  AWHeterogeneousExpandableList = interface;
  Arr1AWHeterogeneousExpandableList = array of AWHeterogeneousExpandableList;
  Arr2AWHeterogeneousExpandableList = array of Arr1AWHeterogeneousExpandableList;
  Arr3AWHeterogeneousExpandableList = array of Arr2AWHeterogeneousExpandableList;

  AWOverScroller = class;
  Arr1AWOverScroller = array of AWOverScroller;
  Arr2AWOverScroller = array of Arr1AWOverScroller;
  Arr3AWOverScroller = array of Arr2AWOverScroller;

  AWScroller = class;
  Arr1AWScroller = array of AWScroller;
  Arr2AWScroller = array of Arr1AWScroller;
  Arr3AWScroller = array of Arr2AWScroller;

  AWSectionIndexer = interface;
  Arr1AWSectionIndexer = array of AWSectionIndexer;
  Arr2AWSectionIndexer = array of Arr1AWSectionIndexer;
  Arr3AWSectionIndexer = array of Arr2AWSectionIndexer;

  AWToast = class;
  Arr1AWToast = array of AWToast;
  Arr2AWToast = array of Arr1AWToast;
  Arr3AWToast = array of Arr2AWToast;

  JIDataInput = interface;
  Arr1JIDataInput = array of JIDataInput;
  Arr2JIDataInput = array of Arr1JIDataInput;
  Arr3JIDataInput = array of Arr2JIDataInput;

  JIDataOutput = interface;
  Arr1JIDataOutput = array of JIDataOutput;
  Arr2JIDataOutput = array of Arr1JIDataOutput;
  Arr3JIDataOutput = array of Arr2JIDataOutput;

  JIFileDescriptor = class;
  Arr1JIFileDescriptor = array of JIFileDescriptor;
  Arr2JIFileDescriptor = array of Arr1JIFileDescriptor;
  Arr3JIFileDescriptor = array of Arr2JIFileDescriptor;

  JIFileFilter = interface;
  Arr1JIFileFilter = array of JIFileFilter;
  Arr2JIFileFilter = array of Arr1JIFileFilter;
  Arr3JIFileFilter = array of Arr2JIFileFilter;

  JIFilenameFilter = interface;
  Arr1JIFilenameFilter = array of JIFilenameFilter;
  Arr2JIFilenameFilter = array of Arr1JIFilenameFilter;
  Arr3JIFilenameFilter = array of Arr2JIFilenameFilter;

  JIFlushable = interface;
  Arr1JIFlushable = array of JIFlushable;
  Arr2JIFlushable = array of Arr1JIFlushable;
  Arr3JIFlushable = array of Arr2JIFlushable;

  JIObjectInputValidation = interface;
  Arr1JIObjectInputValidation = array of JIObjectInputValidation;
  Arr2JIObjectInputValidation = array of Arr1JIObjectInputValidation;
  Arr3JIObjectInputValidation = array of Arr2JIObjectInputValidation;

  JIObjectStreamConstants = interface;
  Arr1JIObjectStreamConstants = array of JIObjectStreamConstants;
  Arr2JIObjectStreamConstants = array of Arr1JIObjectStreamConstants;
  Arr3JIObjectStreamConstants = array of Arr2JIObjectStreamConstants;

  JIStreamTokenizer = class;
  Arr1JIStreamTokenizer = array of JIStreamTokenizer;
  Arr2JIStreamTokenizer = array of Arr1JIStreamTokenizer;
  Arr3JIStreamTokenizer = array of Arr2JIStreamTokenizer;

  JLClassLoader = class;
  Arr1JLClassLoader = array of JLClassLoader;
  Arr2JLClassLoader = array of Arr1JLClassLoader;
  Arr3JLClassLoader = array of Arr2JLClassLoader;

  JLCompiler = class;
  Arr1JLCompiler = array of JLCompiler;
  Arr2JLCompiler = array of Arr1JLCompiler;
  Arr3JLCompiler = array of Arr2JLCompiler;

  JLProcess = class;
  Arr1JLProcess = array of JLProcess;
  Arr2JLProcess = array of Arr1JLProcess;
  Arr3JLProcess = array of Arr2JLProcess;

  JLProcessBuilder = class;
  Arr1JLProcessBuilder = array of JLProcessBuilder;
  Arr2JLProcessBuilder = array of Arr1JLProcessBuilder;
  Arr3JLProcessBuilder = array of Arr2JLProcessBuilder;

  JLRunnable = interface;
  Arr1JLRunnable = array of JLRunnable;
  Arr2JLRunnable = array of Arr1JLRunnable;
  Arr3JLRunnable = array of Arr2JLRunnable;

  JLSecurityManager = class;
  Arr1JLSecurityManager = array of JLSecurityManager;
  Arr2JLSecurityManager = array of Arr1JLSecurityManager;
  Arr3JLSecurityManager = array of Arr2JLSecurityManager;

  JLStrictMath = class;
  Arr1JLStrictMath = array of JLStrictMath;
  Arr2JLStrictMath = array of Arr1JLStrictMath;
  Arr3JLStrictMath = array of Arr2JLStrictMath;

  JLVoid = class;
  Arr1JLVoid = array of JLVoid;
  Arr2JLVoid = array of Arr1JLVoid;
  Arr3JLVoid = array of Arr2JLVoid;

  JLAAnnotation = interface;
  Arr1JLAAnnotation = array of JLAAnnotation;
  Arr2JLAAnnotation = array of Arr1JLAAnnotation;
  Arr3JLAAnnotation = array of Arr2JLAAnnotation;

  JLRReference = class;
  Arr1JLRReference = array of JLRReference;
  Arr2JLRReference = array of Arr1JLRReference;
  Arr3JLRReference = array of Arr2JLRReference;

  JLRReferenceQueue = class;
  Arr1JLRReferenceQueue = array of JLRReferenceQueue;
  Arr2JLRReferenceQueue = array of Arr1JLRReferenceQueue;
  Arr3JLRReferenceQueue = array of Arr2JLRReferenceQueue;

  JLRInvocationHandler = interface;
  Arr1JLRInvocationHandler = array of JLRInvocationHandler;
  Arr2JLRInvocationHandler = array of Arr1JLRInvocationHandler;
  Arr3JLRInvocationHandler = array of Arr2JLRInvocationHandler;

  JLRModifier = class;
  Arr1JLRModifier = array of JLRModifier;
  Arr2JLRModifier = array of Arr1JLRModifier;
  Arr3JLRModifier = array of Arr2JLRModifier;

  JNCacheRequest = class;
  Arr1JNCacheRequest = array of JNCacheRequest;
  Arr2JNCacheRequest = array of Arr1JNCacheRequest;
  Arr3JNCacheRequest = array of Arr2JNCacheRequest;

  JNCacheResponse = class;
  Arr1JNCacheResponse = array of JNCacheResponse;
  Arr2JNCacheResponse = array of Arr1JNCacheResponse;
  Arr3JNCacheResponse = array of Arr2JNCacheResponse;

  JNContentHandler = class;
  Arr1JNContentHandler = array of JNContentHandler;
  Arr2JNContentHandler = array of Arr1JNContentHandler;
  Arr3JNContentHandler = array of Arr2JNContentHandler;

  JNContentHandlerFactory = interface;
  Arr1JNContentHandlerFactory = array of JNContentHandlerFactory;
  Arr2JNContentHandlerFactory = array of Arr1JNContentHandlerFactory;
  Arr3JNContentHandlerFactory = array of Arr2JNContentHandlerFactory;

  JNCookieHandler = class;
  Arr1JNCookieHandler = array of JNCookieHandler;
  Arr2JNCookieHandler = array of Arr1JNCookieHandler;
  Arr3JNCookieHandler = array of Arr2JNCookieHandler;

  JNCookiePolicy = interface;
  Arr1JNCookiePolicy = array of JNCookiePolicy;
  Arr2JNCookiePolicy = array of Arr1JNCookiePolicy;
  Arr3JNCookiePolicy = array of Arr2JNCookiePolicy;

  JNCookieStore = interface;
  Arr1JNCookieStore = array of JNCookieStore;
  Arr2JNCookieStore = array of Arr1JNCookieStore;
  Arr3JNCookieStore = array of Arr2JNCookieStore;

  JNDatagramPacket = class;
  Arr1JNDatagramPacket = array of JNDatagramPacket;
  Arr2JNDatagramPacket = array of Arr1JNDatagramPacket;
  Arr3JNDatagramPacket = array of Arr2JNDatagramPacket;

  JNDatagramSocket = class;
  Arr1JNDatagramSocket = array of JNDatagramSocket;
  Arr2JNDatagramSocket = array of Arr1JNDatagramSocket;
  Arr3JNDatagramSocket = array of Arr2JNDatagramSocket;

  JNDatagramSocketImplFactory = interface;
  Arr1JNDatagramSocketImplFactory = array of JNDatagramSocketImplFactory;
  Arr2JNDatagramSocketImplFactory = array of Arr1JNDatagramSocketImplFactory;
  Arr3JNDatagramSocketImplFactory = array of Arr2JNDatagramSocketImplFactory;

  JNFileNameMap = interface;
  Arr1JNFileNameMap = array of JNFileNameMap;
  Arr2JNFileNameMap = array of Arr1JNFileNameMap;
  Arr3JNFileNameMap = array of Arr2JNFileNameMap;

  JNIDN = class;
  Arr1JNIDN = array of JNIDN;
  Arr2JNIDN = array of Arr1JNIDN;
  Arr3JNIDN = array of Arr2JNIDN;

  JNInterfaceAddress = class;
  Arr1JNInterfaceAddress = array of JNInterfaceAddress;
  Arr2JNInterfaceAddress = array of Arr1JNInterfaceAddress;
  Arr3JNInterfaceAddress = array of Arr2JNInterfaceAddress;

  JNNetworkInterface = class;
  Arr1JNNetworkInterface = array of JNNetworkInterface;
  Arr2JNNetworkInterface = array of Arr1JNNetworkInterface;
  Arr3JNNetworkInterface = array of Arr2JNNetworkInterface;

  JNPasswordAuthentication = class;
  Arr1JNPasswordAuthentication = array of JNPasswordAuthentication;
  Arr2JNPasswordAuthentication = array of Arr1JNPasswordAuthentication;
  Arr3JNPasswordAuthentication = array of Arr2JNPasswordAuthentication;

  JNProxySelector = class;
  Arr1JNProxySelector = array of JNProxySelector;
  Arr2JNProxySelector = array of Arr1JNProxySelector;
  Arr3JNProxySelector = array of Arr2JNProxySelector;

  JNResponseCache = class;
  Arr1JNResponseCache = array of JNResponseCache;
  Arr2JNResponseCache = array of Arr1JNResponseCache;
  Arr3JNResponseCache = array of Arr2JNResponseCache;

  JNServerSocket = class;
  Arr1JNServerSocket = array of JNServerSocket;
  Arr2JNServerSocket = array of Arr1JNServerSocket;
  Arr3JNServerSocket = array of Arr2JNServerSocket;

  JNSocket = class;
  Arr1JNSocket = array of JNSocket;
  Arr2JNSocket = array of Arr1JNSocket;
  Arr3JNSocket = array of Arr2JNSocket;

  JNSocketImplFactory = interface;
  Arr1JNSocketImplFactory = array of JNSocketImplFactory;
  Arr2JNSocketImplFactory = array of Arr1JNSocketImplFactory;
  Arr3JNSocketImplFactory = array of Arr2JNSocketImplFactory;

  JNSocketOptions = interface;
  Arr1JNSocketOptions = array of JNSocketOptions;
  Arr2JNSocketOptions = array of Arr1JNSocketOptions;
  Arr3JNSocketOptions = array of Arr2JNSocketOptions;

  JNURLConnection = class;
  Arr1JNURLConnection = array of JNURLConnection;
  Arr2JNURLConnection = array of Arr1JNURLConnection;
  Arr3JNURLConnection = array of Arr2JNURLConnection;

  JNURLDecoder = class;
  Arr1JNURLDecoder = array of JNURLDecoder;
  Arr2JNURLDecoder = array of Arr1JNURLDecoder;
  Arr3JNURLDecoder = array of Arr2JNURLDecoder;

  JNURLEncoder = class;
  Arr1JNURLEncoder = array of JNURLEncoder;
  Arr2JNURLEncoder = array of Arr1JNURLEncoder;
  Arr3JNURLEncoder = array of Arr2JNURLEncoder;

  JNURLStreamHandler = class;
  Arr1JNURLStreamHandler = array of JNURLStreamHandler;
  Arr2JNURLStreamHandler = array of Arr1JNURLStreamHandler;
  Arr3JNURLStreamHandler = array of Arr2JNURLStreamHandler;

  JNURLStreamHandlerFactory = interface;
  Arr1JNURLStreamHandlerFactory = array of JNURLStreamHandlerFactory;
  Arr2JNURLStreamHandlerFactory = array of Arr1JNURLStreamHandlerFactory;
  Arr3JNURLStreamHandlerFactory = array of Arr2JNURLStreamHandlerFactory;

  JNByteOrder = class;
  Arr1JNByteOrder = array of JNByteOrder;
  Arr2JNByteOrder = array of Arr1JNByteOrder;
  Arr3JNByteOrder = array of Arr2JNByteOrder;

  JNCChannels = class;
  Arr1JNCChannels = array of JNCChannels;
  Arr2JNCChannels = array of Arr1JNCChannels;
  Arr3JNCChannels = array of Arr2JNCChannels;

  JNCFileLock = class;
  Arr1JNCFileLock = array of JNCFileLock;
  Arr2JNCFileLock = array of Arr1JNCFileLock;
  Arr3JNCFileLock = array of Arr2JNCFileLock;

  JNCSelectionKey = class;
  Arr1JNCSelectionKey = array of JNCSelectionKey;
  Arr2JNCSelectionKey = array of Arr1JNCSelectionKey;
  Arr3JNCSelectionKey = array of Arr2JNCSelectionKey;

  JNCSelector = class;
  Arr1JNCSelector = array of JNCSelector;
  Arr2JNCSelector = array of Arr1JNCSelector;
  Arr3JNCSelector = array of Arr2JNCSelector;

  JNCSSelectorProvider = class;
  Arr1JNCSSelectorProvider = array of JNCSSelectorProvider;
  Arr2JNCSSelectorProvider = array of Arr1JNCSSelectorProvider;
  Arr3JNCSSelectorProvider = array of Arr2JNCSSelectorProvider;

  JSAccessControlContext = class;
  Arr1JSAccessControlContext = array of JSAccessControlContext;
  Arr2JSAccessControlContext = array of Arr1JSAccessControlContext;
  Arr3JSAccessControlContext = array of Arr2JSAccessControlContext;

  JSAccessController = class;
  Arr1JSAccessController = array of JSAccessController;
  Arr2JSAccessController = array of Arr1JSAccessController;
  Arr3JSAccessController = array of Arr2JSAccessController;

  JSAlgorithmParameterGenerator = class;
  Arr1JSAlgorithmParameterGenerator = array of JSAlgorithmParameterGenerator;
  Arr2JSAlgorithmParameterGenerator = array of Arr1JSAlgorithmParameterGenerator;
  Arr3JSAlgorithmParameterGenerator = array of Arr2JSAlgorithmParameterGenerator;

  JSAlgorithmParameterGeneratorSpi = class;
  Arr1JSAlgorithmParameterGeneratorSpi = array of JSAlgorithmParameterGeneratorSpi;
  Arr2JSAlgorithmParameterGeneratorSpi = array of Arr1JSAlgorithmParameterGeneratorSpi;
  Arr3JSAlgorithmParameterGeneratorSpi = array of Arr2JSAlgorithmParameterGeneratorSpi;

  JSAlgorithmParameters = class;
  Arr1JSAlgorithmParameters = array of JSAlgorithmParameters;
  Arr2JSAlgorithmParameters = array of Arr1JSAlgorithmParameters;
  Arr3JSAlgorithmParameters = array of Arr2JSAlgorithmParameters;

  JSAlgorithmParametersSpi = class;
  Arr1JSAlgorithmParametersSpi = array of JSAlgorithmParametersSpi;
  Arr2JSAlgorithmParametersSpi = array of Arr1JSAlgorithmParametersSpi;
  Arr3JSAlgorithmParametersSpi = array of Arr2JSAlgorithmParametersSpi;

  JSCertificate = interface;
  Arr1JSCertificate = array of JSCertificate;
  Arr2JSCertificate = array of Arr1JSCertificate;
  Arr3JSCertificate = array of Arr2JSCertificate;

  JSDomainCombiner = interface;
  Arr1JSDomainCombiner = array of JSDomainCombiner;
  Arr2JSDomainCombiner = array of Arr1JSDomainCombiner;
  Arr3JSDomainCombiner = array of Arr2JSDomainCombiner;

  JSGuard = interface;
  Arr1JSGuard = array of JSGuard;
  Arr2JSGuard = array of Arr1JSGuard;
  Arr3JSGuard = array of Arr2JSGuard;

  JSKeyFactory = class;
  Arr1JSKeyFactory = array of JSKeyFactory;
  Arr2JSKeyFactory = array of Arr1JSKeyFactory;
  Arr3JSKeyFactory = array of Arr2JSKeyFactory;

  JSKeyFactorySpi = class;
  Arr1JSKeyFactorySpi = array of JSKeyFactorySpi;
  Arr2JSKeyFactorySpi = array of Arr1JSKeyFactorySpi;
  Arr3JSKeyFactorySpi = array of Arr2JSKeyFactorySpi;

  JSKeyPairGeneratorSpi = class;
  Arr1JSKeyPairGeneratorSpi = array of JSKeyPairGeneratorSpi;
  Arr2JSKeyPairGeneratorSpi = array of Arr1JSKeyPairGeneratorSpi;
  Arr3JSKeyPairGeneratorSpi = array of Arr2JSKeyPairGeneratorSpi;

  JSMessageDigestSpi = class;
  Arr1JSMessageDigestSpi = array of JSMessageDigestSpi;
  Arr2JSMessageDigestSpi = array of Arr1JSMessageDigestSpi;
  Arr3JSMessageDigestSpi = array of Arr2JSMessageDigestSpi;

  JSPolicy = class;
  Arr1JSPolicy = array of JSPolicy;
  Arr2JSPolicy = array of Arr1JSPolicy;
  Arr3JSPolicy = array of Arr2JSPolicy;

  JSPolicySpi = class;
  Arr1JSPolicySpi = array of JSPolicySpi;
  Arr2JSPolicySpi = array of Arr1JSPolicySpi;
  Arr3JSPolicySpi = array of Arr2JSPolicySpi;

  JSPrincipal = interface;
  Arr1JSPrincipal = array of JSPrincipal;
  Arr2JSPrincipal = array of Arr1JSPrincipal;
  Arr3JSPrincipal = array of Arr2JSPrincipal;

  JSPrivilegedAction = interface;
  Arr1JSPrivilegedAction = array of JSPrivilegedAction;
  Arr2JSPrivilegedAction = array of Arr1JSPrivilegedAction;
  Arr3JSPrivilegedAction = array of Arr2JSPrivilegedAction;

  JSPrivilegedExceptionAction = interface;
  Arr1JSPrivilegedExceptionAction = array of JSPrivilegedExceptionAction;
  Arr2JSPrivilegedExceptionAction = array of Arr1JSPrivilegedExceptionAction;
  Arr3JSPrivilegedExceptionAction = array of Arr2JSPrivilegedExceptionAction;

  JSProtectionDomain = class;
  Arr1JSProtectionDomain = array of JSProtectionDomain;
  Arr2JSProtectionDomain = array of Arr1JSProtectionDomain;
  Arr3JSProtectionDomain = array of Arr2JSProtectionDomain;

  JSSecurity = class;
  Arr1JSSecurity = array of JSSecurity;
  Arr2JSSecurity = array of Arr1JSSecurity;
  Arr3JSSecurity = array of Arr2JSSecurity;

  JSSignatureSpi = class;
  Arr1JSSignatureSpi = array of JSSignatureSpi;
  Arr2JSSignatureSpi = array of Arr1JSSignatureSpi;
  Arr3JSSignatureSpi = array of Arr2JSSignatureSpi;

  JSAOwner = interface;
  Arr1JSAOwner = array of JSAOwner;
  Arr2JSAOwner = array of Arr1JSAOwner;
  Arr3JSAOwner = array of Arr2JSAOwner;

  JSAPermission = interface;
  Arr1JSAPermission = array of JSAPermission;
  Arr2JSAPermission = array of Arr1JSAPermission;
  Arr3JSAPermission = array of Arr2JSAPermission;

  JSCCRL = class;
  Arr1JSCCRL = array of JSCCRL;
  Arr2JSCCRL = array of Arr1JSCCRL;
  Arr3JSCCRL = array of Arr2JSCCRL;

  JSCCertPathBuilder = class;
  Arr1JSCCertPathBuilder = array of JSCCertPathBuilder;
  Arr2JSCCertPathBuilder = array of Arr1JSCCertPathBuilder;
  Arr3JSCCertPathBuilder = array of Arr2JSCCertPathBuilder;

  JSCCertPathBuilderSpi = class;
  Arr1JSCCertPathBuilderSpi = array of JSCCertPathBuilderSpi;
  Arr2JSCCertPathBuilderSpi = array of Arr1JSCCertPathBuilderSpi;
  Arr3JSCCertPathBuilderSpi = array of Arr2JSCCertPathBuilderSpi;

  JSCCertPathValidator = class;
  Arr1JSCCertPathValidator = array of JSCCertPathValidator;
  Arr2JSCCertPathValidator = array of Arr1JSCCertPathValidator;
  Arr3JSCCertPathValidator = array of Arr2JSCCertPathValidator;

  JSCCertPathValidatorSpi = class;
  Arr1JSCCertPathValidatorSpi = array of JSCCertPathValidatorSpi;
  Arr2JSCCertPathValidatorSpi = array of Arr1JSCCertPathValidatorSpi;
  Arr3JSCCertPathValidatorSpi = array of Arr2JSCCertPathValidatorSpi;

  JSCCertStore = class;
  Arr1JSCCertStore = array of JSCCertStore;
  Arr2JSCCertStore = array of Arr1JSCCertStore;
  Arr3JSCCertStore = array of Arr2JSCCertStore;

  JSCCertStoreSpi = class;
  Arr1JSCCertStoreSpi = array of JSCCertStoreSpi;
  Arr2JSCCertStoreSpi = array of Arr1JSCCertStoreSpi;
  Arr3JSCCertStoreSpi = array of Arr2JSCCertStoreSpi;

  JSCCertificateFactory = class;
  Arr1JSCCertificateFactory = array of JSCCertificateFactory;
  Arr2JSCCertificateFactory = array of Arr1JSCCertificateFactory;
  Arr3JSCCertificateFactory = array of Arr2JSCCertificateFactory;

  JSCCertificateFactorySpi = class;
  Arr1JSCCertificateFactorySpi = array of JSCCertificateFactorySpi;
  Arr2JSCCertificateFactorySpi = array of Arr1JSCCertificateFactorySpi;
  Arr3JSCCertificateFactorySpi = array of Arr2JSCCertificateFactorySpi;

  JSCPolicyNode = interface;
  Arr1JSCPolicyNode = array of JSCPolicyNode;
  Arr2JSCPolicyNode = array of Arr1JSCPolicyNode;
  Arr3JSCPolicyNode = array of Arr2JSCPolicyNode;

  JSCPolicyQualifierInfo = class;
  Arr1JSCPolicyQualifierInfo = array of JSCPolicyQualifierInfo;
  Arr2JSCPolicyQualifierInfo = array of Arr1JSCPolicyQualifierInfo;
  Arr3JSCPolicyQualifierInfo = array of Arr2JSCPolicyQualifierInfo;

  JSCTrustAnchor = class;
  Arr1JSCTrustAnchor = array of JSCTrustAnchor;
  Arr2JSCTrustAnchor = array of Arr1JSCTrustAnchor;
  Arr3JSCTrustAnchor = array of Arr2JSCTrustAnchor;

  JSCX509Extension = interface;
  Arr1JSCX509Extension = array of JSCX509Extension;
  Arr2JSCX509Extension = array of Arr1JSCX509Extension;
  Arr3JSCX509Extension = array of Arr2JSCX509Extension;

  JSIDSAKey = interface;
  Arr1JSIDSAKey = array of JSIDSAKey;
  Arr2JSIDSAKey = array of Arr1JSIDSAKey;
  Arr3JSIDSAKey = array of Arr2JSIDSAKey;

  JSIDSAKeyPairGenerator = interface;
  Arr1JSIDSAKeyPairGenerator = array of JSIDSAKeyPairGenerator;
  Arr2JSIDSAKeyPairGenerator = array of Arr1JSIDSAKeyPairGenerator;
  Arr3JSIDSAKeyPairGenerator = array of Arr2JSIDSAKeyPairGenerator;

  JSIDSAParams = interface;
  Arr1JSIDSAParams = array of JSIDSAParams;
  Arr2JSIDSAParams = array of Arr1JSIDSAParams;
  Arr3JSIDSAParams = array of Arr2JSIDSAParams;

  JSIECKey = interface;
  Arr1JSIECKey = array of JSIECKey;
  Arr2JSIECKey = array of Arr1JSIECKey;
  Arr3JSIECKey = array of Arr2JSIECKey;

  JSIRSAKey = interface;
  Arr1JSIRSAKey = array of JSIRSAKey;
  Arr2JSIRSAKey = array of Arr1JSIRSAKey;
  Arr3JSIRSAKey = array of Arr2JSIRSAKey;

  JSSAlgorithmParameterSpec = interface;
  Arr1JSSAlgorithmParameterSpec = array of JSSAlgorithmParameterSpec;
  Arr2JSSAlgorithmParameterSpec = array of Arr1JSSAlgorithmParameterSpec;
  Arr3JSSAlgorithmParameterSpec = array of Arr2JSSAlgorithmParameterSpec;

  JSSECField = interface;
  Arr1JSSECField = array of JSSECField;
  Arr2JSSECField = array of Arr1JSSECField;
  Arr3JSSECField = array of Arr2JSSECField;

  JSSECPoint = class;
  Arr1JSSECPoint = array of JSSECPoint;
  Arr2JSSECPoint = array of Arr1JSSECPoint;
  Arr3JSSECPoint = array of Arr2JSSECPoint;

  JSSEllipticCurve = class;
  Arr1JSSEllipticCurve = array of JSSEllipticCurve;
  Arr2JSSEllipticCurve = array of Arr1JSSEllipticCurve;
  Arr3JSSEllipticCurve = array of Arr2JSSEllipticCurve;

  JSSKeySpec = interface;
  Arr1JSSKeySpec = array of JSSKeySpec;
  Arr2JSSKeySpec = array of Arr1JSSKeySpec;
  Arr3JSSKeySpec = array of Arr2JSSKeySpec;

  JSSRSAOtherPrimeInfo = class;
  Arr1JSSRSAOtherPrimeInfo = array of JSSRSAOtherPrimeInfo;
  Arr2JSSRSAOtherPrimeInfo = array of Arr1JSSRSAOtherPrimeInfo;
  Arr3JSSRSAOtherPrimeInfo = array of Arr2JSSRSAOtherPrimeInfo;

  JSArray = interface;
  Arr1JSArray = array of JSArray;
  Arr2JSArray = array of Arr1JSArray;
  Arr3JSArray = array of Arr2JSArray;

  JSBlob = interface;
  Arr1JSBlob = array of JSBlob;
  Arr2JSBlob = array of Arr1JSBlob;
  Arr3JSBlob = array of Arr2JSBlob;

  JSClob = interface;
  Arr1JSClob = array of JSClob;
  Arr2JSClob = array of Arr1JSClob;
  Arr3JSClob = array of Arr2JSClob;

  JSDriver = interface;
  Arr1JSDriver = array of JSDriver;
  Arr2JSDriver = array of Arr1JSDriver;
  Arr3JSDriver = array of Arr2JSDriver;

  JSDriverManager = class;
  Arr1JSDriverManager = array of JSDriverManager;
  Arr2JSDriverManager = array of Arr1JSDriverManager;
  Arr3JSDriverManager = array of Arr2JSDriverManager;

  JSDriverPropertyInfo = class;
  Arr1JSDriverPropertyInfo = array of JSDriverPropertyInfo;
  Arr2JSDriverPropertyInfo = array of Arr1JSDriverPropertyInfo;
  Arr3JSDriverPropertyInfo = array of Arr2JSDriverPropertyInfo;

  JSRef = interface;
  Arr1JSRef = array of JSRef;
  Arr2JSRef = array of Arr1JSRef;
  Arr3JSRef = array of Arr2JSRef;

  JSRowId = interface;
  Arr1JSRowId = array of JSRowId;
  Arr2JSRowId = array of Arr1JSRowId;
  Arr3JSRowId = array of Arr2JSRowId;

  JSSQLData = interface;
  Arr1JSSQLData = array of JSSQLData;
  Arr2JSSQLData = array of Arr1JSSQLData;
  Arr3JSSQLData = array of Arr2JSSQLData;

  JSSQLInput = interface;
  Arr1JSSQLInput = array of JSSQLInput;
  Arr2JSSQLInput = array of Arr1JSSQLInput;
  Arr3JSSQLInput = array of Arr2JSSQLInput;

  JSSQLOutput = interface;
  Arr1JSSQLOutput = array of JSSQLOutput;
  Arr2JSSQLOutput = array of Arr1JSSQLOutput;
  Arr3JSSQLOutput = array of Arr2JSSQLOutput;

  JSSQLXML = interface;
  Arr1JSSQLXML = array of JSSQLXML;
  Arr2JSSQLXML = array of Arr1JSSQLXML;
  Arr3JSSQLXML = array of Arr2JSSQLXML;

  JSSavepoint = interface;
  Arr1JSSavepoint = array of JSSavepoint;
  Arr2JSSavepoint = array of Arr1JSSavepoint;
  Arr3JSSavepoint = array of Arr2JSSavepoint;

  JSStruct = interface;
  Arr1JSStruct = array of JSStruct;
  Arr2JSStruct = array of Arr1JSStruct;
  Arr3JSStruct = array of Arr2JSStruct;

  JSTypes = class;
  Arr1JSTypes = array of JSTypes;
  Arr2JSTypes = array of Arr1JSTypes;
  Arr3JSTypes = array of Arr2JSTypes;

  JSWrapper = interface;
  Arr1JSWrapper = array of JSWrapper;
  Arr2JSWrapper = array of Arr1JSWrapper;
  Arr3JSWrapper = array of Arr2JSWrapper;

  JTAnnotation = class;
  Arr1JTAnnotation = array of JTAnnotation;
  Arr2JTAnnotation = array of Arr1JTAnnotation;
  Arr3JTAnnotation = array of Arr2JTAnnotation;

  JTBidi = class;
  Arr1JTBidi = array of JTBidi;
  Arr2JTBidi = array of Arr1JTBidi;
  Arr3JTBidi = array of Arr2JTBidi;

  JTCollationElementIterator = class;
  Arr1JTCollationElementIterator = array of JTCollationElementIterator;
  Arr2JTCollationElementIterator = array of Arr1JTCollationElementIterator;
  Arr3JTCollationElementIterator = array of Arr2JTCollationElementIterator;

  JTParsePosition = class;
  Arr1JTParsePosition = array of JTParsePosition;
  Arr2JTParsePosition = array of Arr1JTParsePosition;
  Arr3JTParsePosition = array of Arr2JTParsePosition;

  JUCollections = class;
  Arr1JUCollections = array of JUCollections;
  Arr2JUCollections = array of Arr1JUCollections;
  Arr3JUCollections = array of Arr2JUCollections;

  JUDictionary = class;
  Arr1JUDictionary = array of JUDictionary;
  Arr2JUDictionary = array of Arr1JUDictionary;
  Arr3JUDictionary = array of Arr2JUDictionary;

  JUEnumeration = interface;
  Arr1JUEnumeration = array of JUEnumeration;
  Arr2JUEnumeration = array of Arr1JUEnumeration;
  Arr3JUEnumeration = array of Arr2JUEnumeration;

  JUEventListener = interface;
  Arr1JUEventListener = array of JUEventListener;
  Arr2JUEventListener = array of Arr1JUEventListener;
  Arr3JUEventListener = array of Arr2JUEventListener;

  JUFormattable = interface;
  Arr1JUFormattable = array of JUFormattable;
  Arr2JUFormattable = array of Arr1JUFormattable;
  Arr3JUFormattable = array of Arr2JUFormattable;

  JUFormattableFlags = class;
  Arr1JUFormattableFlags = array of JUFormattableFlags;
  Arr2JUFormattableFlags = array of Arr1JUFormattableFlags;
  Arr3JUFormattableFlags = array of Arr2JUFormattableFlags;

  JUObservable = class;
  Arr1JUObservable = array of JUObservable;
  Arr2JUObservable = array of Arr1JUObservable;
  Arr3JUObservable = array of Arr2JUObservable;

  JUObserver = interface;
  Arr1JUObserver = array of JUObserver;
  Arr2JUObserver = array of Arr1JUObserver;
  Arr3JUObserver = array of Arr2JUObserver;

  JURandomAccess = interface;
  Arr1JURandomAccess = array of JURandomAccess;
  Arr2JURandomAccess = array of Arr1JURandomAccess;
  Arr3JURandomAccess = array of Arr2JURandomAccess;

  JUResourceBundle = class;
  Arr1JUResourceBundle = array of JUResourceBundle;
  Arr2JUResourceBundle = array of Arr1JUResourceBundle;
  Arr3JUResourceBundle = array of Arr2JUResourceBundle;

  JUTimer = class;
  Arr1JUTimer = array of JUTimer;
  Arr2JUTimer = array of Arr1JUTimer;
  Arr3JUTimer = array of Arr2JUTimer;

  JUCCallable = interface;
  Arr1JUCCallable = array of JUCCallable;
  Arr2JUCCallable = array of Arr1JUCCallable;
  Arr3JUCCallable = array of Arr2JUCCallable;

  JUCCompletionService = interface;
  Arr1JUCCompletionService = array of JUCCompletionService;
  Arr2JUCCompletionService = array of Arr1JUCCompletionService;
  Arr3JUCCompletionService = array of Arr2JUCCompletionService;

  JUCCountDownLatch = class;
  Arr1JUCCountDownLatch = array of JUCCountDownLatch;
  Arr2JUCCountDownLatch = array of Arr1JUCCountDownLatch;
  Arr3JUCCountDownLatch = array of Arr2JUCCountDownLatch;

  JUCCyclicBarrier = class;
  Arr1JUCCyclicBarrier = array of JUCCyclicBarrier;
  Arr2JUCCyclicBarrier = array of Arr1JUCCyclicBarrier;
  Arr3JUCCyclicBarrier = array of Arr2JUCCyclicBarrier;

  JUCExchanger = class;
  Arr1JUCExchanger = array of JUCExchanger;
  Arr2JUCExchanger = array of Arr1JUCExchanger;
  Arr3JUCExchanger = array of Arr2JUCExchanger;

  JUCExecutor = interface;
  Arr1JUCExecutor = array of JUCExecutor;
  Arr2JUCExecutor = array of Arr1JUCExecutor;
  Arr3JUCExecutor = array of Arr2JUCExecutor;

  JUCExecutors = class;
  Arr1JUCExecutors = array of JUCExecutors;
  Arr2JUCExecutors = array of Arr1JUCExecutors;
  Arr3JUCExecutors = array of Arr2JUCExecutors;

  JUCFuture = interface;
  Arr1JUCFuture = array of JUCFuture;
  Arr2JUCFuture = array of Arr1JUCFuture;
  Arr3JUCFuture = array of Arr2JUCFuture;

  JUCRejectedExecutionHandler = interface;
  Arr1JUCRejectedExecutionHandler = array of JUCRejectedExecutionHandler;
  Arr2JUCRejectedExecutionHandler = array of Arr1JUCRejectedExecutionHandler;
  Arr3JUCRejectedExecutionHandler = array of Arr2JUCRejectedExecutionHandler;

  JUCThreadFactory = interface;
  Arr1JUCThreadFactory = array of JUCThreadFactory;
  Arr2JUCThreadFactory = array of Arr1JUCThreadFactory;
  Arr3JUCThreadFactory = array of Arr2JUCThreadFactory;

  JUCAAtomicIntegerFieldUpdater = class;
  Arr1JUCAAtomicIntegerFieldUpdater = array of JUCAAtomicIntegerFieldUpdater;
  Arr2JUCAAtomicIntegerFieldUpdater = array of Arr1JUCAAtomicIntegerFieldUpdater;
  Arr3JUCAAtomicIntegerFieldUpdater = array of Arr2JUCAAtomicIntegerFieldUpdater;

  JUCAAtomicLongFieldUpdater = class;
  Arr1JUCAAtomicLongFieldUpdater = array of JUCAAtomicLongFieldUpdater;
  Arr2JUCAAtomicLongFieldUpdater = array of Arr1JUCAAtomicLongFieldUpdater;
  Arr3JUCAAtomicLongFieldUpdater = array of Arr2JUCAAtomicLongFieldUpdater;

  JUCAAtomicMarkableReference = class;
  Arr1JUCAAtomicMarkableReference = array of JUCAAtomicMarkableReference;
  Arr2JUCAAtomicMarkableReference = array of Arr1JUCAAtomicMarkableReference;
  Arr3JUCAAtomicMarkableReference = array of Arr2JUCAAtomicMarkableReference;

  JUCAAtomicReferenceFieldUpdater = class;
  Arr1JUCAAtomicReferenceFieldUpdater = array of JUCAAtomicReferenceFieldUpdater;
  Arr2JUCAAtomicReferenceFieldUpdater = array of Arr1JUCAAtomicReferenceFieldUpdater;
  Arr3JUCAAtomicReferenceFieldUpdater = array of Arr2JUCAAtomicReferenceFieldUpdater;

  JUCAAtomicStampedReference = class;
  Arr1JUCAAtomicStampedReference = array of JUCAAtomicStampedReference;
  Arr2JUCAAtomicStampedReference = array of Arr1JUCAAtomicStampedReference;
  Arr3JUCAAtomicStampedReference = array of Arr2JUCAAtomicStampedReference;

  JUCLCondition = interface;
  Arr1JUCLCondition = array of JUCLCondition;
  Arr2JUCLCondition = array of Arr1JUCLCondition;
  Arr3JUCLCondition = array of Arr2JUCLCondition;

  JUCLLock = interface;
  Arr1JUCLLock = array of JUCLLock;
  Arr2JUCLLock = array of Arr1JUCLLock;
  Arr3JUCLLock = array of Arr2JUCLLock;

  JUCLLockSupport = class;
  Arr1JUCLLockSupport = array of JUCLLockSupport;
  Arr2JUCLLockSupport = array of Arr1JUCLLockSupport;
  Arr3JUCLLockSupport = array of Arr2JUCLLockSupport;

  JUCLReadWriteLock = interface;
  Arr1JUCLReadWriteLock = array of JUCLReadWriteLock;
  Arr2JUCLReadWriteLock = array of Arr1JUCLReadWriteLock;
  Arr3JUCLReadWriteLock = array of Arr2JUCLReadWriteLock;

  JUJPack200 = class;
  Arr1JUJPack200 = array of JUJPack200;
  Arr2JUJPack200 = array of Arr1JUJPack200;
  Arr3JUJPack200 = array of Arr2JUJPack200;

  JULErrorManager = class;
  Arr1JULErrorManager = array of JULErrorManager;
  Arr2JULErrorManager = array of Arr1JULErrorManager;
  Arr3JULErrorManager = array of Arr2JULErrorManager;

  JULFilter = interface;
  Arr1JULFilter = array of JULFilter;
  Arr2JULFilter = array of Arr1JULFilter;
  Arr3JULFilter = array of Arr2JULFilter;

  JULFormatter = class;
  Arr1JULFormatter = array of JULFormatter;
  Arr2JULFormatter = array of Arr1JULFormatter;
  Arr3JULFormatter = array of Arr2JULFormatter;

  JULHandler = class;
  Arr1JULHandler = array of JULHandler;
  Arr2JULHandler = array of Arr1JULHandler;
  Arr3JULHandler = array of Arr2JULHandler;

  JULLogManager = class;
  Arr1JULLogManager = array of JULLogManager;
  Arr2JULLogManager = array of Arr1JULLogManager;
  Arr3JULLogManager = array of Arr2JULLogManager;

  JULLogger = class;
  Arr1JULLogger = array of JULLogger;
  Arr2JULLogger = array of Arr1JULLogger;
  Arr3JULLogger = array of Arr2JULLogger;

  JULLoggingMXBean = interface;
  Arr1JULLoggingMXBean = array of JULLoggingMXBean;
  Arr2JULLoggingMXBean = array of Arr1JULLoggingMXBean;
  Arr3JULLoggingMXBean = array of Arr2JULLoggingMXBean;

  JUPPreferences = class;
  Arr1JUPPreferences = array of JUPPreferences;
  Arr2JUPPreferences = array of Arr1JUPPreferences;
  Arr3JUPPreferences = array of Arr2JUPPreferences;

  JUPPreferencesFactory = interface;
  Arr1JUPPreferencesFactory = array of JUPPreferencesFactory;
  Arr2JUPPreferencesFactory = array of Arr1JUPPreferencesFactory;
  Arr3JUPPreferencesFactory = array of Arr2JUPPreferencesFactory;

  JURMatchResult = interface;
  Arr1JURMatchResult = array of JURMatchResult;
  Arr2JURMatchResult = array of Arr1JURMatchResult;
  Arr3JURMatchResult = array of Arr2JURMatchResult;

  JUZChecksum = interface;
  Arr1JUZChecksum = array of JUZChecksum;
  Arr2JUZChecksum = array of Arr1JUZChecksum;
  Arr3JUZChecksum = array of Arr2JUZChecksum;

  JUZDeflater = class;
  Arr1JUZDeflater = array of JUZDeflater;
  Arr2JUZDeflater = array of Arr1JUZDeflater;
  Arr3JUZDeflater = array of Arr2JUZDeflater;

  JUZInflater = class;
  Arr1JUZInflater = array of JUZInflater;
  Arr2JUZInflater = array of Arr1JUZInflater;
  Arr3JUZInflater = array of Arr2JUZInflater;

  JUZZipFile = class;
  Arr1JUZZipFile = array of JUZZipFile;
  Arr2JUZZipFile = array of Arr1JUZZipFile;
  Arr3JUZZipFile = array of Arr2JUZZipFile;

  JCCipher = class;
  Arr1JCCipher = array of JCCipher;
  Arr2JCCipher = array of Arr1JCCipher;
  Arr3JCCipher = array of Arr2JCCipher;

  JCCipherSpi = class;
  Arr1JCCipherSpi = array of JCCipherSpi;
  Arr2JCCipherSpi = array of Arr1JCCipherSpi;
  Arr3JCCipherSpi = array of Arr2JCCipherSpi;

  JCEncryptedPrivateKeyInfo = class;
  Arr1JCEncryptedPrivateKeyInfo = array of JCEncryptedPrivateKeyInfo;
  Arr2JCEncryptedPrivateKeyInfo = array of Arr1JCEncryptedPrivateKeyInfo;
  Arr3JCEncryptedPrivateKeyInfo = array of Arr2JCEncryptedPrivateKeyInfo;

  JCExemptionMechanism = class;
  Arr1JCExemptionMechanism = array of JCExemptionMechanism;
  Arr2JCExemptionMechanism = array of Arr1JCExemptionMechanism;
  Arr3JCExemptionMechanism = array of Arr2JCExemptionMechanism;

  JCExemptionMechanismSpi = class;
  Arr1JCExemptionMechanismSpi = array of JCExemptionMechanismSpi;
  Arr2JCExemptionMechanismSpi = array of Arr1JCExemptionMechanismSpi;
  Arr3JCExemptionMechanismSpi = array of Arr2JCExemptionMechanismSpi;

  JCKeyAgreement = class;
  Arr1JCKeyAgreement = array of JCKeyAgreement;
  Arr2JCKeyAgreement = array of Arr1JCKeyAgreement;
  Arr3JCKeyAgreement = array of Arr2JCKeyAgreement;

  JCKeyAgreementSpi = class;
  Arr1JCKeyAgreementSpi = array of JCKeyAgreementSpi;
  Arr2JCKeyAgreementSpi = array of Arr1JCKeyAgreementSpi;
  Arr3JCKeyAgreementSpi = array of Arr2JCKeyAgreementSpi;

  JCKeyGenerator = class;
  Arr1JCKeyGenerator = array of JCKeyGenerator;
  Arr2JCKeyGenerator = array of Arr1JCKeyGenerator;
  Arr3JCKeyGenerator = array of Arr2JCKeyGenerator;

  JCKeyGeneratorSpi = class;
  Arr1JCKeyGeneratorSpi = array of JCKeyGeneratorSpi;
  Arr2JCKeyGeneratorSpi = array of Arr1JCKeyGeneratorSpi;
  Arr3JCKeyGeneratorSpi = array of Arr2JCKeyGeneratorSpi;

  JCMacSpi = class;
  Arr1JCMacSpi = array of JCMacSpi;
  Arr2JCMacSpi = array of Arr1JCMacSpi;
  Arr3JCMacSpi = array of Arr2JCMacSpi;

  JCSecretKeyFactory = class;
  Arr1JCSecretKeyFactory = array of JCSecretKeyFactory;
  Arr2JCSecretKeyFactory = array of Arr1JCSecretKeyFactory;
  Arr3JCSecretKeyFactory = array of Arr2JCSecretKeyFactory;

  JCSecretKeyFactorySpi = class;
  Arr1JCSecretKeyFactorySpi = array of JCSecretKeyFactorySpi;
  Arr2JCSecretKeyFactorySpi = array of Arr1JCSecretKeyFactorySpi;
  Arr3JCSecretKeyFactorySpi = array of Arr2JCSecretKeyFactorySpi;

  JCIDHKey = interface;
  Arr1JCIDHKey = array of JCIDHKey;
  Arr2JCIDHKey = array of Arr1JCIDHKey;
  Arr3JCIDHKey = array of Arr2JCIDHKey;

  JCSPSource = class;
  Arr1JCSPSource = array of JCSPSource;
  Arr2JCSPSource = array of Arr1JCSPSource;
  Arr3JCSPSource = array of Arr2JCSPSource;

  JMKEEGL = interface;
  Arr1JMKEEGL = array of JMKEEGL;
  Arr2JMKEEGL = array of Arr1JMKEEGL;
  Arr3JMKEEGL = array of Arr2JMKEEGL;

  JMKEEGLConfig = class;
  Arr1JMKEEGLConfig = array of JMKEEGLConfig;
  Arr2JMKEEGLConfig = array of Arr1JMKEEGLConfig;
  Arr3JMKEEGLConfig = array of Arr2JMKEEGLConfig;

  JMKEEGLContext = class;
  Arr1JMKEEGLContext = array of JMKEEGLContext;
  Arr2JMKEEGLContext = array of Arr1JMKEEGLContext;
  Arr3JMKEEGLContext = array of Arr2JMKEEGLContext;

  JMKEEGLDisplay = class;
  Arr1JMKEEGLDisplay = array of JMKEEGLDisplay;
  Arr2JMKEEGLDisplay = array of Arr1JMKEEGLDisplay;
  Arr3JMKEEGLDisplay = array of Arr2JMKEEGLDisplay;

  JMKEEGLSurface = class;
  Arr1JMKEEGLSurface = array of JMKEEGLSurface;
  Arr2JMKEEGLSurface = array of Arr1JMKEEGLSurface;
  Arr3JMKEEGLSurface = array of Arr2JMKEEGLSurface;

  JMKOGL = interface;
  Arr1JMKOGL = array of JMKOGL;
  Arr2JMKOGL = array of Arr1JMKOGL;
  Arr3JMKOGL = array of Arr2JMKOGL;

  JNServerSocketFactory = class;
  Arr1JNServerSocketFactory = array of JNServerSocketFactory;
  Arr2JNServerSocketFactory = array of Arr1JNServerSocketFactory;
  Arr3JNServerSocketFactory = array of Arr2JNServerSocketFactory;

  JNSocketFactory = class;
  Arr1JNSocketFactory = array of JNSocketFactory;
  Arr2JNSocketFactory = array of Arr1JNSocketFactory;
  Arr3JNSocketFactory = array of Arr2JNSocketFactory;

  JNSHostnameVerifier = interface;
  Arr1JNSHostnameVerifier = array of JNSHostnameVerifier;
  Arr2JNSHostnameVerifier = array of Arr1JNSHostnameVerifier;
  Arr3JNSHostnameVerifier = array of Arr2JNSHostnameVerifier;

  JNSKeyManager = interface;
  Arr1JNSKeyManager = array of JNSKeyManager;
  Arr2JNSKeyManager = array of Arr1JNSKeyManager;
  Arr3JNSKeyManager = array of Arr2JNSKeyManager;

  JNSKeyManagerFactory = class;
  Arr1JNSKeyManagerFactory = array of JNSKeyManagerFactory;
  Arr2JNSKeyManagerFactory = array of Arr1JNSKeyManagerFactory;
  Arr3JNSKeyManagerFactory = array of Arr2JNSKeyManagerFactory;

  JNSKeyManagerFactorySpi = class;
  Arr1JNSKeyManagerFactorySpi = array of JNSKeyManagerFactorySpi;
  Arr2JNSKeyManagerFactorySpi = array of Arr1JNSKeyManagerFactorySpi;
  Arr3JNSKeyManagerFactorySpi = array of Arr2JNSKeyManagerFactorySpi;

  JNSManagerFactoryParameters = interface;
  Arr1JNSManagerFactoryParameters = array of JNSManagerFactoryParameters;
  Arr2JNSManagerFactoryParameters = array of Arr1JNSManagerFactoryParameters;
  Arr3JNSManagerFactoryParameters = array of Arr2JNSManagerFactoryParameters;

  JNSSSLContext = class;
  Arr1JNSSSLContext = array of JNSSSLContext;
  Arr2JNSSSLContext = array of Arr1JNSSSLContext;
  Arr3JNSSSLContext = array of Arr2JNSSSLContext;

  JNSSSLContextSpi = class;
  Arr1JNSSSLContextSpi = array of JNSSSLContextSpi;
  Arr2JNSSSLContextSpi = array of Arr1JNSSSLContextSpi;
  Arr3JNSSSLContextSpi = array of Arr2JNSSSLContextSpi;

  JNSSSLParameters = class;
  Arr1JNSSSLParameters = array of JNSSSLParameters;
  Arr2JNSSSLParameters = array of Arr1JNSSSLParameters;
  Arr3JNSSSLParameters = array of Arr2JNSSSLParameters;

  JNSSSLSession = interface;
  Arr1JNSSSLSession = array of JNSSSLSession;
  Arr2JNSSSLSession = array of Arr1JNSSSLSession;
  Arr3JNSSSLSession = array of Arr2JNSSSLSession;

  JNSSSLSessionContext = interface;
  Arr1JNSSSLSessionContext = array of JNSSSLSessionContext;
  Arr2JNSSSLSessionContext = array of Arr1JNSSSLSessionContext;
  Arr3JNSSSLSessionContext = array of Arr2JNSSSLSessionContext;

  JNSTrustManager = interface;
  Arr1JNSTrustManager = array of JNSTrustManager;
  Arr2JNSTrustManager = array of Arr1JNSTrustManager;
  Arr3JNSTrustManager = array of Arr2JNSTrustManager;

  JNSTrustManagerFactory = class;
  Arr1JNSTrustManagerFactory = array of JNSTrustManagerFactory;
  Arr2JNSTrustManagerFactory = array of Arr1JNSTrustManagerFactory;
  Arr3JNSTrustManagerFactory = array of Arr2JNSTrustManagerFactory;

  JNSTrustManagerFactorySpi = class;
  Arr1JNSTrustManagerFactorySpi = array of JNSTrustManagerFactorySpi;
  Arr2JNSTrustManagerFactorySpi = array of Arr1JNSTrustManagerFactorySpi;
  Arr3JNSTrustManagerFactorySpi = array of Arr2JNSTrustManagerFactorySpi;

  JSADestroyable = interface;
  Arr1JSADestroyable = array of JSADestroyable;
  Arr2JSADestroyable = array of Arr1JSADestroyable;
  Arr3JSADestroyable = array of Arr2JSADestroyable;

  JSACCallback = interface;
  Arr1JSACCallback = array of JSACCallback;
  Arr2JSACCallback = array of Arr1JSACCallback;
  Arr3JSACCallback = array of Arr2JSACCallback;

  JSACCallbackHandler = interface;
  Arr1JSACCallbackHandler = array of JSACCallbackHandler;
  Arr2JSACCallbackHandler = array of Arr1JSACCallbackHandler;
  Arr3JSACCallbackHandler = array of Arr2JSACCallbackHandler;

  JxSCCertificate = class;
  Arr1JxSCCertificate = array of JxSCCertificate;
  Arr2JxSCCertificate = array of Arr1JxSCCertificate;
  Arr3JxSCCertificate = array of Arr2JxSCCertificate;

  JSCommonDataSource = interface;
  Arr1JSCommonDataSource = array of JSCommonDataSource;
  Arr2JSCommonDataSource = array of Arr1JSCommonDataSource;
  Arr3JSCommonDataSource = array of Arr2JSCommonDataSource;

  JSPooledConnection = interface;
  Arr1JSPooledConnection = array of JSPooledConnection;
  Arr2JSPooledConnection = array of Arr1JSPooledConnection;
  Arr3JSPooledConnection = array of Arr2JSPooledConnection;

  JSRowSetInternal = interface;
  Arr1JSRowSetInternal = array of JSRowSetInternal;
  Arr2JSRowSetInternal = array of Arr1JSRowSetInternal;
  Arr3JSRowSetInternal = array of Arr2JSRowSetInternal;

  JSRowSetReader = interface;
  Arr1JSRowSetReader = array of JSRowSetReader;
  Arr2JSRowSetReader = array of Arr1JSRowSetReader;
  Arr3JSRowSetReader = array of Arr2JSRowSetReader;

  JSRowSetWriter = interface;
  Arr1JSRowSetWriter = array of JSRowSetWriter;
  Arr2JSRowSetWriter = array of Arr1JSRowSetWriter;
  Arr3JSRowSetWriter = array of Arr2JSRowSetWriter;

  JXXMLConstants = class;
  Arr1JXXMLConstants = array of JXXMLConstants;
  Arr2JXXMLConstants = array of Arr1JXXMLConstants;
  Arr3JXXMLConstants = array of Arr2JXXMLConstants;

  JXDDatatypeConstants = class;
  Arr1JXDDatatypeConstants = array of JXDDatatypeConstants;
  Arr2JXDDatatypeConstants = array of Arr1JXDDatatypeConstants;
  Arr3JXDDatatypeConstants = array of Arr2JXDDatatypeConstants;

  JXDDatatypeFactory = class;
  Arr1JXDDatatypeFactory = array of JXDDatatypeFactory;
  Arr2JXDDatatypeFactory = array of Arr1JXDDatatypeFactory;
  Arr3JXDDatatypeFactory = array of Arr2JXDDatatypeFactory;

  JXNNamespaceContext = interface;
  Arr1JXNNamespaceContext = array of JXNNamespaceContext;
  Arr2JXNNamespaceContext = array of Arr1JXNNamespaceContext;
  Arr3JXNNamespaceContext = array of Arr2JXNNamespaceContext;

  JXPDocumentBuilder = class;
  Arr1JXPDocumentBuilder = array of JXPDocumentBuilder;
  Arr2JXPDocumentBuilder = array of Arr1JXPDocumentBuilder;
  Arr3JXPDocumentBuilder = array of Arr2JXPDocumentBuilder;

  JXPDocumentBuilderFactory = class;
  Arr1JXPDocumentBuilderFactory = array of JXPDocumentBuilderFactory;
  Arr2JXPDocumentBuilderFactory = array of Arr1JXPDocumentBuilderFactory;
  Arr3JXPDocumentBuilderFactory = array of Arr2JXPDocumentBuilderFactory;

  JXPSAXParser = class;
  Arr1JXPSAXParser = array of JXPSAXParser;
  Arr2JXPSAXParser = array of Arr1JXPSAXParser;
  Arr3JXPSAXParser = array of Arr2JXPSAXParser;

  JXPSAXParserFactory = class;
  Arr1JXPSAXParserFactory = array of JXPSAXParserFactory;
  Arr2JXPSAXParserFactory = array of Arr1JXPSAXParserFactory;
  Arr3JXPSAXParserFactory = array of Arr2JXPSAXParserFactory;

  JXTErrorListener = interface;
  Arr1JXTErrorListener = array of JXTErrorListener;
  Arr2JXTErrorListener = array of Arr1JXTErrorListener;
  Arr3JXTErrorListener = array of Arr2JXTErrorListener;

  JXTOutputKeys = class;
  Arr1JXTOutputKeys = array of JXTOutputKeys;
  Arr2JXTOutputKeys = array of Arr1JXTOutputKeys;
  Arr3JXTOutputKeys = array of Arr2JXTOutputKeys;

  JXTResult = interface;
  Arr1JXTResult = array of JXTResult;
  Arr2JXTResult = array of Arr1JXTResult;
  Arr3JXTResult = array of Arr2JXTResult;

  JXTSource = interface;
  Arr1JXTSource = array of JXTSource;
  Arr2JXTSource = array of Arr1JXTSource;
  Arr3JXTSource = array of Arr2JXTSource;

  JXTSourceLocator = interface;
  Arr1JXTSourceLocator = array of JXTSourceLocator;
  Arr2JXTSourceLocator = array of Arr1JXTSourceLocator;
  Arr3JXTSourceLocator = array of Arr2JXTSourceLocator;

  JXTTemplates = interface;
  Arr1JXTTemplates = array of JXTTemplates;
  Arr2JXTTemplates = array of Arr1JXTTemplates;
  Arr3JXTTemplates = array of Arr2JXTTemplates;

  JXTTransformer = class;
  Arr1JXTTransformer = array of JXTTransformer;
  Arr2JXTTransformer = array of Arr1JXTTransformer;
  Arr3JXTTransformer = array of Arr2JXTTransformer;

  JXTTransformerFactory = class;
  Arr1JXTTransformerFactory = array of JXTTransformerFactory;
  Arr2JXTTransformerFactory = array of Arr1JXTTransformerFactory;
  Arr3JXTTransformerFactory = array of Arr2JXTTransformerFactory;

  JXTURIResolver = interface;
  Arr1JXTURIResolver = array of JXTURIResolver;
  Arr2JXTURIResolver = array of Arr1JXTURIResolver;
  Arr3JXTURIResolver = array of Arr2JXTURIResolver;

  JXVSchema = class;
  Arr1JXVSchema = array of JXVSchema;
  Arr2JXVSchema = array of Arr1JXVSchema;
  Arr3JXVSchema = array of Arr2JXVSchema;

  JXVSchemaFactory = class;
  Arr1JXVSchemaFactory = array of JXVSchemaFactory;
  Arr2JXVSchemaFactory = array of Arr1JXVSchemaFactory;
  Arr3JXVSchemaFactory = array of Arr2JXVSchemaFactory;

  JXVSchemaFactoryLoader = class;
  Arr1JXVSchemaFactoryLoader = array of JXVSchemaFactoryLoader;
  Arr2JXVSchemaFactoryLoader = array of Arr1JXVSchemaFactoryLoader;
  Arr3JXVSchemaFactoryLoader = array of Arr2JXVSchemaFactoryLoader;

  JXVTypeInfoProvider = class;
  Arr1JXVTypeInfoProvider = array of JXVTypeInfoProvider;
  Arr2JXVTypeInfoProvider = array of Arr1JXVTypeInfoProvider;
  Arr3JXVTypeInfoProvider = array of Arr2JXVTypeInfoProvider;

  JXVValidator = class;
  Arr1JXVValidator = array of JXVValidator;
  Arr2JXVValidator = array of Arr1JXVValidator;
  Arr3JXVValidator = array of Arr2JXVValidator;

  JXXXPath = interface;
  Arr1JXXXPath = array of JXXXPath;
  Arr2JXXXPath = array of Arr1JXXXPath;
  Arr3JXXXPath = array of Arr2JXXXPath;

  JXXXPathConstants = class;
  Arr1JXXXPathConstants = array of JXXXPathConstants;
  Arr2JXXXPathConstants = array of Arr1JXXXPathConstants;
  Arr3JXXXPathConstants = array of Arr2JXXXPathConstants;

  JXXXPathExpression = interface;
  Arr1JXXXPathExpression = array of JXXXPathExpression;
  Arr2JXXXPathExpression = array of Arr1JXXXPathExpression;
  Arr3JXXXPathExpression = array of Arr2JXXXPathExpression;

  JXXXPathFactory = class;
  Arr1JXXXPathFactory = array of JXXXPathFactory;
  Arr2JXXXPathFactory = array of Arr1JXXXPathFactory;
  Arr3JXXXPathFactory = array of Arr2JXXXPathFactory;

  JXXXPathFunction = interface;
  Arr1JXXXPathFunction = array of JXXXPathFunction;
  Arr2JXXXPathFunction = array of Arr1JXXXPathFunction;
  Arr3JXXXPathFunction = array of Arr2JXXXPathFunction;

  JXXXPathFunctionResolver = interface;
  Arr1JXXXPathFunctionResolver = array of JXXXPathFunctionResolver;
  Arr2JXXXPathFunctionResolver = array of Arr1JXXXPathFunctionResolver;
  Arr3JXXXPathFunctionResolver = array of Arr2JXXXPathFunctionResolver;

  JXXXPathVariableResolver = interface;
  Arr1JXXXPathVariableResolver = array of JXXXPathVariableResolver;
  Arr2JXXXPathVariableResolver = array of Arr1JXXXPathVariableResolver;
  Arr3JXXXPathVariableResolver = array of Arr2JXXXPathVariableResolver;

  JFAssert = class;
  Arr1JFAssert = array of JFAssert;
  Arr2JFAssert = array of Arr1JFAssert;
  Arr3JFAssert = array of Arr2JFAssert;

  JFProtectable = interface;
  Arr1JFProtectable = array of JFProtectable;
  Arr2JFProtectable = array of Arr1JFProtectable;
  Arr3JFProtectable = array of Arr2JFProtectable;

  JFTest = interface;
  Arr1JFTest = array of JFTest;
  Arr2JFTest = array of Arr1JFTest;
  Arr3JFTest = array of Arr2JFTest;

  JFTestFailure = class;
  Arr1JFTestFailure = array of JFTestFailure;
  Arr2JFTestFailure = array of Arr1JFTestFailure;
  Arr3JFTestFailure = array of Arr2JFTestFailure;

  JFTestListener = interface;
  Arr1JFTestListener = array of JFTestListener;
  Arr2JFTestListener = array of Arr1JFTestListener;
  Arr3JFTestListener = array of Arr2JFTestListener;

  JFTestResult = class;
  Arr1JFTestResult = array of JFTestResult;
  Arr2JFTestResult = array of Arr1JFTestResult;
  Arr3JFTestResult = array of Arr2JFTestResult;

  JRTestSuiteLoader = interface;
  Arr1JRTestSuiteLoader = array of JRTestSuiteLoader;
  Arr2JRTestSuiteLoader = array of Arr1JRTestSuiteLoader;
  Arr3JRTestSuiteLoader = array of Arr2JRTestSuiteLoader;

  JRVersion = class;
  Arr1JRVersion = array of JRVersion;
  Arr2JRVersion = array of Arr1JRVersion;
  Arr3JRVersion = array of Arr2JRVersion;

  OAHCSHostNameResolver = interface;
  Arr1OAHCSHostNameResolver = array of OAHCSHostNameResolver;
  Arr2OAHCSHostNameResolver = array of Arr1OAHCSHostNameResolver;
  Arr3OAHCSHostNameResolver = array of Arr2OAHCSHostNameResolver;

  OAHCSSocketFactory = interface;
  Arr1OAHCSSocketFactory = array of OAHCSSocketFactory;
  Arr2OAHCSSocketFactory = array of Arr1OAHCSSocketFactory;
  Arr3OAHCSSocketFactory = array of Arr2OAHCSSocketFactory;

  OAHPCoreConnectionPNames = interface;
  Arr1OAHPCoreConnectionPNames = array of OAHPCoreConnectionPNames;
  Arr2OAHPCoreConnectionPNames = array of Arr1OAHPCoreConnectionPNames;
  Arr3OAHPCoreConnectionPNames = array of Arr2OAHPCoreConnectionPNames;

  OAHPHttpParams = interface;
  Arr1OAHPHttpParams = array of OAHPHttpParams;
  Arr2OAHPHttpParams = array of Arr1OAHPHttpParams;
  Arr3OAHPHttpParams = array of Arr2OAHPHttpParams;

  OJJSONArray = class;
  Arr1OJJSONArray = array of OJJSONArray;
  Arr2OJJSONArray = array of Arr1OJJSONArray;
  Arr3OJJSONArray = array of Arr2OJJSONArray;

  OJJSONObject = class;
  Arr1OJJSONObject = array of OJJSONObject;
  Arr2OJJSONObject = array of Arr1OJJSONObject;
  Arr3OJJSONObject = array of Arr2OJJSONObject;

  OJJSONStringer = class;
  Arr1OJJSONStringer = array of OJJSONStringer;
  Arr2OJJSONStringer = array of Arr1OJJSONStringer;
  Arr3OJJSONStringer = array of Arr2OJJSONStringer;

  OJJSONTokener = class;
  Arr1OJJSONTokener = array of OJJSONTokener;
  Arr2OJJSONTokener = array of Arr1OJJSONTokener;
  Arr3OJJSONTokener = array of Arr2OJJSONTokener;

  OWDDOMConfiguration = interface;
  Arr1OWDDOMConfiguration = array of OWDDOMConfiguration;
  Arr2OWDDOMConfiguration = array of Arr1OWDDOMConfiguration;
  Arr3OWDDOMConfiguration = array of Arr2OWDDOMConfiguration;

  OWDDOMError = interface;
  Arr1OWDDOMError = array of OWDDOMError;
  Arr2OWDDOMError = array of Arr1OWDDOMError;
  Arr3OWDDOMError = array of Arr2OWDDOMError;

  OWDDOMErrorHandler = interface;
  Arr1OWDDOMErrorHandler = array of OWDDOMErrorHandler;
  Arr2OWDDOMErrorHandler = array of Arr1OWDDOMErrorHandler;
  Arr3OWDDOMErrorHandler = array of Arr2OWDDOMErrorHandler;

  OWDDOMImplementation = interface;
  Arr1OWDDOMImplementation = array of OWDDOMImplementation;
  Arr2OWDDOMImplementation = array of Arr1OWDDOMImplementation;
  Arr3OWDDOMImplementation = array of Arr2OWDDOMImplementation;

  OWDDOMImplementationList = interface;
  Arr1OWDDOMImplementationList = array of OWDDOMImplementationList;
  Arr2OWDDOMImplementationList = array of Arr1OWDDOMImplementationList;
  Arr3OWDDOMImplementationList = array of Arr2OWDDOMImplementationList;

  OWDDOMImplementationSource = interface;
  Arr1OWDDOMImplementationSource = array of OWDDOMImplementationSource;
  Arr2OWDDOMImplementationSource = array of Arr1OWDDOMImplementationSource;
  Arr3OWDDOMImplementationSource = array of Arr2OWDDOMImplementationSource;

  OWDDOMLocator = interface;
  Arr1OWDDOMLocator = array of OWDDOMLocator;
  Arr2OWDDOMLocator = array of Arr1OWDDOMLocator;
  Arr3OWDDOMLocator = array of Arr2OWDDOMLocator;

  OWDDOMStringList = interface;
  Arr1OWDDOMStringList = array of OWDDOMStringList;
  Arr2OWDDOMStringList = array of Arr1OWDDOMStringList;
  Arr3OWDDOMStringList = array of Arr2OWDDOMStringList;

  OWDNameList = interface;
  Arr1OWDNameList = array of OWDNameList;
  Arr2OWDNameList = array of Arr1OWDNameList;
  Arr3OWDNameList = array of Arr2OWDNameList;

  OWDNamedNodeMap = interface;
  Arr1OWDNamedNodeMap = array of OWDNamedNodeMap;
  Arr2OWDNamedNodeMap = array of Arr1OWDNamedNodeMap;
  Arr3OWDNamedNodeMap = array of Arr2OWDNamedNodeMap;

  OWDNode = interface;
  Arr1OWDNode = array of OWDNode;
  Arr2OWDNode = array of Arr1OWDNode;
  Arr3OWDNode = array of Arr2OWDNode;

  OWDNodeList = interface;
  Arr1OWDNodeList = array of OWDNodeList;
  Arr2OWDNodeList = array of Arr1OWDNodeList;
  Arr3OWDNodeList = array of Arr2OWDNodeList;

  OWDTypeInfo = interface;
  Arr1OWDTypeInfo = array of OWDTypeInfo;
  Arr2OWDTypeInfo = array of Arr1OWDTypeInfo;
  Arr3OWDTypeInfo = array of Arr2OWDTypeInfo;

  OWDUserDataHandler = interface;
  Arr1OWDUserDataHandler = array of OWDUserDataHandler;
  Arr2OWDUserDataHandler = array of Arr1OWDUserDataHandler;
  Arr3OWDUserDataHandler = array of Arr2OWDUserDataHandler;

  OWDLDOMImplementationLS = interface;
  Arr1OWDLDOMImplementationLS = array of OWDLDOMImplementationLS;
  Arr2OWDLDOMImplementationLS = array of Arr1OWDLDOMImplementationLS;
  Arr3OWDLDOMImplementationLS = array of Arr2OWDLDOMImplementationLS;

  OWDLLSInput = interface;
  Arr1OWDLLSInput = array of OWDLLSInput;
  Arr2OWDLLSInput = array of Arr1OWDLLSInput;
  Arr3OWDLLSInput = array of Arr2OWDLLSInput;

  OWDLLSOutput = interface;
  Arr1OWDLLSOutput = array of OWDLLSOutput;
  Arr2OWDLLSOutput = array of Arr1OWDLLSOutput;
  Arr3OWDLLSOutput = array of Arr2OWDLLSOutput;

  OWDLLSParser = interface;
  Arr1OWDLLSParser = array of OWDLLSParser;
  Arr2OWDLLSParser = array of Arr1OWDLLSParser;
  Arr3OWDLLSParser = array of Arr2OWDLLSParser;

  OWDLLSParserFilter = interface;
  Arr1OWDLLSParserFilter = array of OWDLLSParserFilter;
  Arr2OWDLLSParserFilter = array of Arr1OWDLLSParserFilter;
  Arr3OWDLLSParserFilter = array of Arr2OWDLLSParserFilter;

  OWDLLSResourceResolver = interface;
  Arr1OWDLLSResourceResolver = array of OWDLLSResourceResolver;
  Arr2OWDLLSResourceResolver = array of Arr1OWDLLSResourceResolver;
  Arr3OWDLLSResourceResolver = array of Arr2OWDLLSResourceResolver;

  OWDLLSSerializer = interface;
  Arr1OWDLLSSerializer = array of OWDLLSSerializer;
  Arr2OWDLLSSerializer = array of Arr1OWDLLSSerializer;
  Arr3OWDLLSSerializer = array of Arr2OWDLLSSerializer;

  OXSAttributeList = interface;
  Arr1OXSAttributeList = array of OXSAttributeList;
  Arr2OXSAttributeList = array of Arr1OXSAttributeList;
  Arr3OXSAttributeList = array of Arr2OXSAttributeList;

  OXSAttributes = interface;
  Arr1OXSAttributes = array of OXSAttributes;
  Arr2OXSAttributes = array of Arr1OXSAttributes;
  Arr3OXSAttributes = array of Arr2OXSAttributes;

  OXSContentHandler = interface;
  Arr1OXSContentHandler = array of OXSContentHandler;
  Arr2OXSContentHandler = array of Arr1OXSContentHandler;
  Arr3OXSContentHandler = array of Arr2OXSContentHandler;

  OXSDTDHandler = interface;
  Arr1OXSDTDHandler = array of OXSDTDHandler;
  Arr2OXSDTDHandler = array of Arr1OXSDTDHandler;
  Arr3OXSDTDHandler = array of Arr2OXSDTDHandler;

  OXSDocumentHandler = interface;
  Arr1OXSDocumentHandler = array of OXSDocumentHandler;
  Arr2OXSDocumentHandler = array of Arr1OXSDocumentHandler;
  Arr3OXSDocumentHandler = array of Arr2OXSDocumentHandler;

  OXSEntityResolver = interface;
  Arr1OXSEntityResolver = array of OXSEntityResolver;
  Arr2OXSEntityResolver = array of Arr1OXSEntityResolver;
  Arr3OXSEntityResolver = array of Arr2OXSEntityResolver;

  OXSErrorHandler = interface;
  Arr1OXSErrorHandler = array of OXSErrorHandler;
  Arr2OXSErrorHandler = array of Arr1OXSErrorHandler;
  Arr3OXSErrorHandler = array of Arr2OXSErrorHandler;

  OXSInputSource = class;
  Arr1OXSInputSource = array of OXSInputSource;
  Arr2OXSInputSource = array of Arr1OXSInputSource;
  Arr3OXSInputSource = array of Arr2OXSInputSource;

  OXSLocator = interface;
  Arr1OXSLocator = array of OXSLocator;
  Arr2OXSLocator = array of Arr1OXSLocator;
  Arr3OXSLocator = array of Arr2OXSLocator;

  OXSParser = interface;
  Arr1OXSParser = array of OXSParser;
  Arr2OXSParser = array of Arr1OXSParser;
  Arr3OXSParser = array of Arr2OXSParser;

  OXSXMLReader = interface;
  Arr1OXSXMLReader = array of OXSXMLReader;
  Arr2OXSXMLReader = array of Arr1OXSXMLReader;
  Arr3OXSXMLReader = array of Arr2OXSXMLReader;

  OXSEDeclHandler = interface;
  Arr1OXSEDeclHandler = array of OXSEDeclHandler;
  Arr2OXSEDeclHandler = array of Arr1OXSEDeclHandler;
  Arr3OXSEDeclHandler = array of Arr2OXSEDeclHandler;

  OXSELexicalHandler = interface;
  Arr1OXSELexicalHandler = array of OXSELexicalHandler;
  Arr2OXSELexicalHandler = array of Arr1OXSELexicalHandler;
  Arr3OXSELexicalHandler = array of Arr2OXSELexicalHandler;

  OXSHNamespaceSupport = class;
  Arr1OXSHNamespaceSupport = array of OXSHNamespaceSupport;
  Arr2OXSHNamespaceSupport = array of Arr1OXSHNamespaceSupport;
  Arr3OXSHNamespaceSupport = array of Arr2OXSHNamespaceSupport;

  OXSHParserFactory = class;
  Arr1OXSHParserFactory = array of OXSHParserFactory;
  Arr2OXSHParserFactory = array of Arr1OXSHParserFactory;
  Arr3OXSHParserFactory = array of Arr2OXSHParserFactory;

  OXSHXMLReaderFactory = class;
  Arr1OXSHXMLReaderFactory = array of OXSHXMLReaderFactory;
  Arr2OXSHXMLReaderFactory = array of Arr1OXSHXMLReaderFactory;
  Arr3OXSHXMLReaderFactory = array of Arr2OXSHXMLReaderFactory;

  OXVXmlPullParser = interface;
  Arr1OXVXmlPullParser = array of OXVXmlPullParser;
  Arr2OXVXmlPullParser = array of Arr1OXVXmlPullParser;
  Arr3OXVXmlPullParser = array of Arr2OXVXmlPullParser;

  OXVXmlPullParserFactory = class;
  Arr1OXVXmlPullParserFactory = array of OXVXmlPullParserFactory;
  Arr2OXVXmlPullParserFactory = array of Arr1OXVXmlPullParserFactory;
  Arr3OXVXmlPullParserFactory = array of Arr2OXVXmlPullParserFactory;

  OXVXmlSerializer = interface;
  Arr1OXVXmlSerializer = array of OXVXmlSerializer;
  Arr2OXVXmlSerializer = array of Arr1OXVXmlSerializer;
  Arr3OXVXmlSerializer = array of Arr2OXVXmlSerializer;

  AAAccountsException = class;
  Arr1AAAccountsException = array of AAAccountsException;
  Arr2AAAccountsException = array of Arr1AAAccountsException;
  Arr3AAAccountsException = array of Arr2AAAccountsException;

  ACOperationApplicationException = class;
  Arr1ACOperationApplicationException = array of ACOperationApplicationException;
  Arr2ACOperationApplicationException = array of Arr1ACOperationApplicationException;
  Arr3ACOperationApplicationException = array of Arr2ACOperationApplicationException;

  AGSurfaceTexture = class;
  Arr1AGSurfaceTexture = array of AGSurfaceTexture;
  Arr2AGSurfaceTexture = array of Arr1AGSurfaceTexture;
  Arr3AGSurfaceTexture = array of Arr2AGSurfaceTexture;

  ANSSipException = class;
  Arr1ANSSipException = array of ANSSipException;
  Arr2ANSSipException = array of Arr1ANSSipException;
  Arr3ANSSipException = array of Arr2ANSSipException;

  ANFormatException = class;
  Arr1ANFormatException = array of ANFormatException;
  Arr2ANFormatException = array of Arr1ANFormatException;
  Arr3ANFormatException = array of Arr2ANFormatException;

  ASKeyChainException = class;
  Arr1ASKeyChainException = array of ASKeyChainException;
  Arr2ASKeyChainException = array of Arr1ASKeyChainException;
  Arr3ASKeyChainException = array of Arr2ASKeyChainException;

  AUAndroidException = class;
  Arr1AUAndroidException = array of AUAndroidException;
  Arr2AUAndroidException = array of Arr1AUAndroidException;
  Arr3AUAndroidException = array of Arr2AUAndroidException;

  JLReflectiveOperationException = class;
  Arr1JLReflectiveOperationException = array of JLReflectiveOperationException;
  Arr2JLReflectiveOperationException = array of Arr1JLReflectiveOperationException;
  Arr3JLReflectiveOperationException = array of Arr2JLReflectiveOperationException;

  JLClassNotFoundException = class;
  Arr1JLClassNotFoundException = array of JLClassNotFoundException;
  Arr2JLClassNotFoundException = array of Arr1JLClassNotFoundException;
  Arr3JLClassNotFoundException = array of Arr2JLClassNotFoundException;

  JLCloneNotSupportedException = class;
  Arr1JLCloneNotSupportedException = array of JLCloneNotSupportedException;
  Arr2JLCloneNotSupportedException = array of Arr1JLCloneNotSupportedException;
  Arr3JLCloneNotSupportedException = array of Arr2JLCloneNotSupportedException;

  JLIllegalAccessException = class;
  Arr1JLIllegalAccessException = array of JLIllegalAccessException;
  Arr2JLIllegalAccessException = array of Arr1JLIllegalAccessException;
  Arr3JLIllegalAccessException = array of Arr2JLIllegalAccessException;

  JLInstantiationException = class;
  Arr1JLInstantiationException = array of JLInstantiationException;
  Arr2JLInstantiationException = array of Arr1JLInstantiationException;
  Arr3JLInstantiationException = array of Arr2JLInstantiationException;

  JLInterruptedException = class;
  Arr1JLInterruptedException = array of JLInterruptedException;
  Arr2JLInterruptedException = array of Arr1JLInterruptedException;
  Arr3JLInterruptedException = array of Arr2JLInterruptedException;

  JLNoSuchFieldException = class;
  Arr1JLNoSuchFieldException = array of JLNoSuchFieldException;
  Arr2JLNoSuchFieldException = array of Arr1JLNoSuchFieldException;
  Arr3JLNoSuchFieldException = array of Arr2JLNoSuchFieldException;

  JLNoSuchMethodException = class;
  Arr1JLNoSuchMethodException = array of JLNoSuchMethodException;
  Arr2JLNoSuchMethodException = array of Arr1JLNoSuchMethodException;
  Arr3JLNoSuchMethodException = array of Arr2JLNoSuchMethodException;

  JNURISyntaxException = class;
  Arr1JNURISyntaxException = array of JNURISyntaxException;
  Arr2JNURISyntaxException = array of Arr1JNURISyntaxException;
  Arr3JNURISyntaxException = array of Arr2JNURISyntaxException;

  JSGeneralSecurityException = class;
  Arr1JSGeneralSecurityException = array of JSGeneralSecurityException;
  Arr2JSGeneralSecurityException = array of Arr1JSGeneralSecurityException;
  Arr3JSGeneralSecurityException = array of Arr2JSGeneralSecurityException;

  JSPrivilegedActionException = class;
  Arr1JSPrivilegedActionException = array of JSPrivilegedActionException;
  Arr2JSPrivilegedActionException = array of Arr1JSPrivilegedActionException;
  Arr3JSPrivilegedActionException = array of Arr2JSPrivilegedActionException;

  JSAAclNotFoundException = class;
  Arr1JSAAclNotFoundException = array of JSAAclNotFoundException;
  Arr2JSAAclNotFoundException = array of Arr1JSAAclNotFoundException;
  Arr3JSAAclNotFoundException = array of Arr2JSAAclNotFoundException;

  JSALastOwnerException = class;
  Arr1JSALastOwnerException = array of JSALastOwnerException;
  Arr2JSALastOwnerException = array of Arr1JSALastOwnerException;
  Arr3JSALastOwnerException = array of Arr2JSALastOwnerException;

  JSANotOwnerException = class;
  Arr1JSANotOwnerException = array of JSANotOwnerException;
  Arr2JSANotOwnerException = array of Arr1JSANotOwnerException;
  Arr3JSANotOwnerException = array of Arr2JSANotOwnerException;

  JTParseException = class;
  Arr1JTParseException = array of JTParseException;
  Arr2JTParseException = array of Arr1JTParseException;
  Arr3JTParseException = array of Arr2JTParseException;

  JUTooManyListenersException = class;
  Arr1JUTooManyListenersException = array of JUTooManyListenersException;
  Arr2JUTooManyListenersException = array of Arr1JUTooManyListenersException;
  Arr3JUTooManyListenersException = array of Arr2JUTooManyListenersException;

  JUCBrokenBarrierException = class;
  Arr1JUCBrokenBarrierException = array of JUCBrokenBarrierException;
  Arr2JUCBrokenBarrierException = array of Arr1JUCBrokenBarrierException;
  Arr3JUCBrokenBarrierException = array of Arr2JUCBrokenBarrierException;

  JUCExecutionException = class;
  Arr1JUCExecutionException = array of JUCExecutionException;
  Arr2JUCExecutionException = array of Arr1JUCExecutionException;
  Arr3JUCExecutionException = array of Arr2JUCExecutionException;

  JUCTimeoutException = class;
  Arr1JUCTimeoutException = array of JUCTimeoutException;
  Arr2JUCTimeoutException = array of Arr1JUCTimeoutException;
  Arr3JUCTimeoutException = array of Arr2JUCTimeoutException;

  JUPBackingStoreException = class;
  Arr1JUPBackingStoreException = array of JUPBackingStoreException;
  Arr2JUPBackingStoreException = array of Arr1JUPBackingStoreException;
  Arr3JUPBackingStoreException = array of Arr2JUPBackingStoreException;

  JUPInvalidPreferencesFormatException = class;
  Arr1JUPInvalidPreferencesFormatException = array of JUPInvalidPreferencesFormatException;
  Arr2JUPInvalidPreferencesFormatException = array of Arr1JUPInvalidPreferencesFormatException;
  Arr3JUPInvalidPreferencesFormatException = array of Arr2JUPInvalidPreferencesFormatException;

  JUZDataFormatException = class;
  Arr1JUZDataFormatException = array of JUZDataFormatException;
  Arr2JUZDataFormatException = array of Arr1JUZDataFormatException;
  Arr3JUZDataFormatException = array of Arr2JUZDataFormatException;

  JSADestroyFailedException = class;
  Arr1JSADestroyFailedException = array of JSADestroyFailedException;
  Arr2JSADestroyFailedException = array of Arr1JSADestroyFailedException;
  Arr3JSADestroyFailedException = array of Arr2JSADestroyFailedException;

  JSACUnsupportedCallbackException = class;
  Arr1JSACUnsupportedCallbackException = array of JSACUnsupportedCallbackException;
  Arr2JSACUnsupportedCallbackException = array of Arr1JSACUnsupportedCallbackException;
  Arr3JSACUnsupportedCallbackException = array of Arr2JSACUnsupportedCallbackException;

  JxSCCertificateException = class;
  Arr1JxSCCertificateException = array of JxSCCertificateException;
  Arr2JxSCCertificateException = array of Arr1JxSCCertificateException;
  Arr3JxSCCertificateException = array of Arr2JxSCCertificateException;

  JXDDatatypeConfigurationException = class;
  Arr1JXDDatatypeConfigurationException = array of JXDDatatypeConfigurationException;
  Arr2JXDDatatypeConfigurationException = array of Arr1JXDDatatypeConfigurationException;
  Arr3JXDDatatypeConfigurationException = array of Arr2JXDDatatypeConfigurationException;

  JXPParserConfigurationException = class;
  Arr1JXPParserConfigurationException = array of JXPParserConfigurationException;
  Arr2JXPParserConfigurationException = array of Arr1JXPParserConfigurationException;
  Arr3JXPParserConfigurationException = array of Arr2JXPParserConfigurationException;

  JXTTransformerException = class;
  Arr1JXTTransformerException = array of JXTTransformerException;
  Arr2JXTTransformerException = array of Arr1JXTTransformerException;
  Arr3JXTTransformerException = array of Arr2JXTTransformerException;

  JXXXPathException = class;
  Arr1JXXXPathException = array of JXXXPathException;
  Arr2JXXXPathException = array of Arr1JXXXPathException;
  Arr3JXXXPathException = array of Arr2JXXXPathException;

  OJJSONException = class;
  Arr1OJJSONException = array of OJJSONException;
  Arr2OJJSONException = array of Arr1OJJSONException;
  Arr3OJJSONException = array of Arr2OJJSONException;

  OXSSAXException = class;
  Arr1OXSSAXException = array of OXSSAXException;
  Arr2OXSSAXException = array of Arr1OXSSAXException;
  Arr3OXSSAXException = array of Arr2OXSSAXException;

  OXVXmlPullParserException = class;
  Arr1OXVXmlPullParserException = array of OXVXmlPullParserException;
  Arr2OXVXmlPullParserException = array of Arr1OXVXmlPullParserException;
  Arr3OXVXmlPullParserException = array of Arr2OXVXmlPullParserException;

  AAAnimator = class;
  Arr1AAAnimator = array of AAAnimator;
  Arr2AAAnimator = array of Arr1AAAnimator;
  Arr3AAAnimator = array of Arr2AAAnimator;

  AAKeyframe = class;
  Arr1AAKeyframe = array of AAKeyframe;
  Arr2AAKeyframe = array of Arr1AAKeyframe;
  Arr3AAKeyframe = array of Arr2AAKeyframe;

  AAPropertyValuesHolder = class;
  Arr1AAPropertyValuesHolder = array of AAPropertyValuesHolder;
  Arr2AAPropertyValuesHolder = array of Arr1AAPropertyValuesHolder;
  Arr3AAPropertyValuesHolder = array of Arr2AAPropertyValuesHolder;

  AGDSShape = class;
  Arr1AGDSShape = array of AGDSShape;
  Arr2AGDSShape = array of Arr1AGDSShape;
  Arr3AGDSShape = array of Arr2AGDSShape;

  AUSparseArray = class;
  Arr1AUSparseArray = array of AUSparseArray;
  Arr2AUSparseArray = array of Arr1AUSparseArray;
  Arr3AUSparseArray = array of Arr2AUSparseArray;

  AUSparseBooleanArray = class;
  Arr1AUSparseBooleanArray = array of AUSparseBooleanArray;
  Arr2AUSparseBooleanArray = array of Arr1AUSparseBooleanArray;
  Arr3AUSparseBooleanArray = array of Arr2AUSparseBooleanArray;

  AUSparseIntArray = class;
  Arr1AUSparseIntArray = array of AUSparseIntArray;
  Arr2AUSparseIntArray = array of Arr1AUSparseIntArray;
  Arr3AUSparseIntArray = array of Arr2AUSparseIntArray;

  AVAAnimation = class;
  Arr1AVAAnimation = array of AVAAnimation;
  Arr2AVAAnimation = array of Arr1AVAAnimation;
  Arr3AVAAnimation = array of Arr2AVAAnimation;

  AWWebHistoryItem = class;
  Arr1AWWebHistoryItem = array of AWWebHistoryItem;
  Arr2AWWebHistoryItem = array of Arr1AWWebHistoryItem;
  Arr3AWWebHistoryItem = array of Arr2AWWebHistoryItem;

  JNHttpCookie = class;
  Arr1JNHttpCookie = array of JNHttpCookie;
  Arr2JNHttpCookie = array of Arr1JNHttpCookie;
  Arr3JNHttpCookie = array of Arr2JNHttpCookie;

  JSAAclEntry = interface;
  Arr1JSAAclEntry = array of JSAAclEntry;
  Arr2JSAAclEntry = array of Arr1JSAAclEntry;
  Arr3JSAAclEntry = array of Arr2JSAAclEntry;

  JSCCRLSelector = interface;
  Arr1JSCCRLSelector = array of JSCCRLSelector;
  Arr2JSCCRLSelector = array of Arr1JSCCRLSelector;
  Arr3JSCCRLSelector = array of Arr2JSCCRLSelector;

  JSCCertPathBuilderResult = interface;
  Arr1JSCCertPathBuilderResult = array of JSCCertPathBuilderResult;
  Arr2JSCCertPathBuilderResult = array of Arr1JSCCertPathBuilderResult;
  Arr3JSCCertPathBuilderResult = array of Arr2JSCCertPathBuilderResult;

  JSCCertPathParameters = interface;
  Arr1JSCCertPathParameters = array of JSCCertPathParameters;
  Arr2JSCCertPathParameters = array of Arr1JSCCertPathParameters;
  Arr3JSCCertPathParameters = array of Arr2JSCCertPathParameters;

  JSCCertPathValidatorResult = interface;
  Arr1JSCCertPathValidatorResult = array of JSCCertPathValidatorResult;
  Arr2JSCCertPathValidatorResult = array of Arr1JSCCertPathValidatorResult;
  Arr3JSCCertPathValidatorResult = array of Arr2JSCCertPathValidatorResult;

  JSCCertSelector = interface;
  Arr1JSCCertSelector = array of JSCCertSelector;
  Arr2JSCCertSelector = array of Arr1JSCCertSelector;
  Arr3JSCCertSelector = array of Arr2JSCCertSelector;

  JSCCertStoreParameters = interface;
  Arr1JSCCertStoreParameters = array of JSCCertStoreParameters;
  Arr2JSCCertStoreParameters = array of Arr1JSCCertStoreParameters;
  Arr3JSCCertStoreParameters = array of Arr2JSCCertStoreParameters;

  JSCCertPathChecker = interface;
  Arr1JSCCertPathChecker = array of JSCCertPathChecker;
  Arr2JSCCertPathChecker = array of Arr1JSCCertPathChecker;
  Arr3JSCCertPathChecker = array of Arr2JSCCertPathChecker;

  JSCPKIXCertPathChecker = class;
  Arr1JSCPKIXCertPathChecker = array of JSCPKIXCertPathChecker;
  Arr2JSCPKIXCertPathChecker = array of Arr1JSCPKIXCertPathChecker;
  Arr3JSCPKIXCertPathChecker = array of Arr2JSCPKIXCertPathChecker;

  JTBreakIterator = class;
  Arr1JTBreakIterator = array of JTBreakIterator;
  Arr2JTBreakIterator = array of Arr1JTBreakIterator;
  Arr3JTBreakIterator = array of Arr2JTBreakIterator;

  JTCharacterIterator = interface;
  Arr1JTCharacterIterator = array of JTCharacterIterator;
  Arr2JTCharacterIterator = array of Arr1JTCharacterIterator;
  Arr3JTCharacterIterator = array of Arr2JTCharacterIterator;

  JUJManifest = class;
  Arr1JUJManifest = array of JUJManifest;
  Arr2JUJManifest = array of Arr1JUJManifest;
  Arr3JUJManifest = array of Arr2JUJManifest;

  JUZZipEntry = class;
  Arr1JUZZipEntry = array of JUZZipEntry;
  Arr2JUZZipEntry = array of Arr1JUZZipEntry;
  Arr3JUZZipEntry = array of Arr2JUZZipEntry;

  JCMac = class;
  Arr1JCMac = array of JCMac;
  Arr2JCMac = array of Arr1JCMac;
  Arr3JCMac = array of Arr2JCMac;

  JXDXMLGregorianCalendar = class;
  Arr1JXDXMLGregorianCalendar = array of JXDXMLGregorianCalendar;
  Arr2JXDXMLGregorianCalendar = array of Arr1JXDXMLGregorianCalendar;
  Arr3JXDXMLGregorianCalendar = array of Arr2JXDXMLGregorianCalendar;

  ACActivityNotFoundException = class;
  Arr1ACActivityNotFoundException = array of ACActivityNotFoundException;
  Arr2ACActivityNotFoundException = array of Arr1ACActivityNotFoundException;
  Arr3ACActivityNotFoundException = array of Arr2ACActivityNotFoundException;

  ACRResources = class;
  Arr1ACRResources = array of ACRResources;
  Arr2ACRResources = array of Arr1ACRResources;
  Arr3ACRResources = array of Arr2ACRResources;

  ADSQLException = class;
  Arr1ADSQLException = array of ADSQLException;
  Arr2ADSQLException = array of Arr1ADSQLException;
  Arr3ADSQLException = array of Arr2ADSQLException;

  ADStaleDataException = class;
  Arr1ADStaleDataException = array of ADStaleDataException;
  Arr2ADStaleDataException = array of Arr1ADStaleDataException;
  Arr3ADStaleDataException = array of Arr2ADStaleDataException;

  ANParseException = class;
  Arr1ANParseException = array of ANParseException;
  Arr2ANParseException = array of Arr1ANParseException;
  Arr3ANParseException = array of Arr2ANParseException;

  AOGLException = class;
  Arr1AOGLException = array of AOGLException;
  Arr2AOGLException = array of Arr1AOGLException;
  Arr3AOGLException = array of Arr2AOGLException;

  AONetworkOnMainThreadException = class;
  Arr1AONetworkOnMainThreadException = array of AONetworkOnMainThreadException;
  Arr2AONetworkOnMainThreadException = array of Arr1AONetworkOnMainThreadException;
  Arr3AONetworkOnMainThreadException = array of Arr2AONetworkOnMainThreadException;

  AOParcelFormatException = class;
  Arr1AOParcelFormatException = array of AOParcelFormatException;
  Arr2AOParcelFormatException = array of Arr1AOParcelFormatException;
  Arr3AOParcelFormatException = array of Arr2AOParcelFormatException;

  ARRSRuntimeException = class;
  Arr1ARRSRuntimeException = array of ARRSRuntimeException;
  Arr2ARRSRuntimeException = array of Arr1ARRSRuntimeException;
  Arr3ARRSRuntimeException = array of Arr2ARRSRuntimeException;

  AUAndroidRuntimeException = class;
  Arr1AUAndroidRuntimeException = array of AUAndroidRuntimeException;
  Arr2AUAndroidRuntimeException = array of Arr1AUAndroidRuntimeException;
  Arr3AUAndroidRuntimeException = array of Arr2AUAndroidRuntimeException;

  AUNoSuchPropertyException = class;
  Arr1AUNoSuchPropertyException = array of AUNoSuchPropertyException;
  Arr2AUNoSuchPropertyException = array of Arr1AUNoSuchPropertyException;
  Arr3AUNoSuchPropertyException = array of Arr2AUNoSuchPropertyException;

  AUTimeFormatException = class;
  Arr1AUTimeFormatException = array of AUTimeFormatException;
  Arr2AUTimeFormatException = array of Arr1AUTimeFormatException;
  Arr3AUTimeFormatException = array of Arr2AUTimeFormatException;

  AVInflateException = class;
  Arr1AVInflateException = array of AVInflateException;
  Arr2AVInflateException = array of Arr1AVInflateException;
  Arr3AVInflateException = array of Arr2AVInflateException;

  AVSurfaceHolder = interface;
  Arr1AVSurfaceHolder = array of AVSurfaceHolder;
  Arr2AVSurfaceHolder = array of Arr1AVSurfaceHolder;
  Arr3AVSurfaceHolder = array of Arr2AVSurfaceHolder;

  JLArithmeticException = class;
  Arr1JLArithmeticException = array of JLArithmeticException;
  Arr2JLArithmeticException = array of Arr1JLArithmeticException;
  Arr3JLArithmeticException = array of Arr2JLArithmeticException;

  JLArrayStoreException = class;
  Arr1JLArrayStoreException = array of JLArrayStoreException;
  Arr2JLArrayStoreException = array of Arr1JLArrayStoreException;
  Arr3JLArrayStoreException = array of Arr2JLArrayStoreException;

  JLClassCastException = class;
  Arr1JLClassCastException = array of JLClassCastException;
  Arr2JLClassCastException = array of Arr1JLClassCastException;
  Arr3JLClassCastException = array of Arr2JLClassCastException;

  JLEnumConstantNotPresentException = class;
  Arr1JLEnumConstantNotPresentException = array of JLEnumConstantNotPresentException;
  Arr2JLEnumConstantNotPresentException = array of Arr1JLEnumConstantNotPresentException;
  Arr3JLEnumConstantNotPresentException = array of Arr2JLEnumConstantNotPresentException;

  JLIllegalMonitorStateException = class;
  Arr1JLIllegalMonitorStateException = array of JLIllegalMonitorStateException;
  Arr2JLIllegalMonitorStateException = array of Arr1JLIllegalMonitorStateException;
  Arr3JLIllegalMonitorStateException = array of Arr2JLIllegalMonitorStateException;

  JLNegativeArraySizeException = class;
  Arr1JLNegativeArraySizeException = array of JLNegativeArraySizeException;
  Arr2JLNegativeArraySizeException = array of Arr1JLNegativeArraySizeException;
  Arr3JLNegativeArraySizeException = array of Arr2JLNegativeArraySizeException;

  JLNullPointerException = class;
  Arr1JLNullPointerException = array of JLNullPointerException;
  Arr2JLNullPointerException = array of Arr1JLNullPointerException;
  Arr3JLNullPointerException = array of Arr2JLNullPointerException;

  JLSecurityException = class;
  Arr1JLSecurityException = array of JLSecurityException;
  Arr2JLSecurityException = array of Arr1JLSecurityException;
  Arr3JLSecurityException = array of Arr2JLSecurityException;

  JLTypeNotPresentException = class;
  Arr1JLTypeNotPresentException = array of JLTypeNotPresentException;
  Arr2JLTypeNotPresentException = array of Arr1JLTypeNotPresentException;
  Arr3JLTypeNotPresentException = array of Arr2JLTypeNotPresentException;

  JLAAnnotationTypeMismatchException = class;
  Arr1JLAAnnotationTypeMismatchException = array of JLAAnnotationTypeMismatchException;
  Arr2JLAAnnotationTypeMismatchException = array of Arr1JLAAnnotationTypeMismatchException;
  Arr3JLAAnnotationTypeMismatchException = array of Arr2JLAAnnotationTypeMismatchException;

  JLAIncompleteAnnotationException = class;
  Arr1JLAIncompleteAnnotationException = array of JLAIncompleteAnnotationException;
  Arr2JLAIncompleteAnnotationException = array of Arr1JLAIncompleteAnnotationException;
  Arr3JLAIncompleteAnnotationException = array of Arr2JLAIncompleteAnnotationException;

  JLRMalformedParameterizedTypeException = class;
  Arr1JLRMalformedParameterizedTypeException = array of JLRMalformedParameterizedTypeException;
  Arr2JLRMalformedParameterizedTypeException = array of Arr1JLRMalformedParameterizedTypeException;
  Arr3JLRMalformedParameterizedTypeException = array of Arr2JLRMalformedParameterizedTypeException;

  JLRUndeclaredThrowableException = class;
  Arr1JLRUndeclaredThrowableException = array of JLRUndeclaredThrowableException;
  Arr2JLRUndeclaredThrowableException = array of Arr1JLRUndeclaredThrowableException;
  Arr3JLRUndeclaredThrowableException = array of Arr2JLRUndeclaredThrowableException;

  JNBufferOverflowException = class;
  Arr1JNBufferOverflowException = array of JNBufferOverflowException;
  Arr2JNBufferOverflowException = array of Arr1JNBufferOverflowException;
  Arr3JNBufferOverflowException = array of Arr2JNBufferOverflowException;

  JNBufferUnderflowException = class;
  Arr1JNBufferUnderflowException = array of JNBufferUnderflowException;
  Arr2JNBufferUnderflowException = array of Arr1JNBufferUnderflowException;
  Arr3JNBufferUnderflowException = array of Arr2JNBufferUnderflowException;

  JSProviderException = class;
  Arr1JSProviderException = array of JSProviderException;
  Arr2JSProviderException = array of Arr1JSProviderException;
  Arr3JSProviderException = array of Arr2JSProviderException;

  JUConcurrentModificationException = class;
  Arr1JUConcurrentModificationException = array of JUConcurrentModificationException;
  Arr2JUConcurrentModificationException = array of Arr1JUConcurrentModificationException;
  Arr3JUConcurrentModificationException = array of Arr2JUConcurrentModificationException;

  JUEmptyStackException = class;
  Arr1JUEmptyStackException = array of JUEmptyStackException;
  Arr2JUEmptyStackException = array of Arr1JUEmptyStackException;
  Arr3JUEmptyStackException = array of Arr2JUEmptyStackException;

  JUMissingResourceException = class;
  Arr1JUMissingResourceException = array of JUMissingResourceException;
  Arr2JUMissingResourceException = array of Arr1JUMissingResourceException;
  Arr3JUMissingResourceException = array of Arr2JUMissingResourceException;

  JUNoSuchElementException = class;
  Arr1JUNoSuchElementException = array of JUNoSuchElementException;
  Arr2JUNoSuchElementException = array of Arr1JUNoSuchElementException;
  Arr3JUNoSuchElementException = array of Arr2JUNoSuchElementException;

  JUCRejectedExecutionException = class;
  Arr1JUCRejectedExecutionException = array of JUCRejectedExecutionException;
  Arr2JUCRejectedExecutionException = array of Arr1JUCRejectedExecutionException;
  Arr3JUCRejectedExecutionException = array of Arr2JUCRejectedExecutionException;

  OWDDOMException = class;
  Arr1OWDDOMException = array of OWDDOMException;
  Arr2OWDDOMException = array of Arr1OWDDOMException;
  Arr3OWDDOMException = array of Arr2OWDDOMException;

  OWDLLSException = class;
  Arr1OWDLLSException = array of OWDLLSException;
  Arr2OWDLLSException = array of Arr1OWDLLSException;
  Arr3OWDLLSException = array of Arr2OWDLLSException;

  JIObjectStreamField = class;
  Arr1JIObjectStreamField = array of JIObjectStreamField;
  Arr2JIObjectStreamField = array of Arr1JIObjectStreamField;
  Arr3JIObjectStreamField = array of Arr2JIObjectStreamField;

  JTCollationKey = class;
  Arr1JTCollationKey = array of JTCollationKey;
  Arr2JTCollationKey = array of Arr1JTCollationKey;
  Arr3JTCollationKey = array of Arr2JTCollationKey;

  JUCDelayed = interface;
  Arr1JUCDelayed = array of JUCDelayed;
  Arr2JUCDelayed = array of Arr1JUCDelayed;
  Arr3JUCDelayed = array of Arr2JUCDelayed;

  ACEntityIterator = interface;
  Arr1ACEntityIterator = array of ACEntityIterator;
  Arr2ACEntityIterator = array of Arr1ACEntityIterator;
  Arr3ACEntityIterator = array of Arr2ACEntityIterator;

  JUListIterator = interface;
  Arr1JUListIterator = array of JUListIterator;
  Arr2JUListIterator = array of Arr1JUListIterator;
  Arr3JUListIterator = array of Arr2JUListIterator;

  JUScanner = class;
  Arr1JUScanner = array of JUScanner;
  Arr2JUScanner = array of Arr1JUScanner;
  Arr3JUScanner = array of Arr2JUScanner;

  ACPPackageItemInfo = class;
  Arr1ACPPackageItemInfo = array of ACPPackageItemInfo;
  Arr2ACPPackageItemInfo = array of Arr1ACPPackageItemInfo;
  Arr3ACPPackageItemInfo = array of Arr2ACPPackageItemInfo;

  ADCursorIndexOutOfBoundsException = class;
  Arr1ADCursorIndexOutOfBoundsException = array of ADCursorIndexOutOfBoundsException;
  Arr2ADCursorIndexOutOfBoundsException = array of Arr1ADCursorIndexOutOfBoundsException;
  Arr3ADCursorIndexOutOfBoundsException = array of Arr2ADCursorIndexOutOfBoundsException;

  JLArrayIndexOutOfBoundsException = class;
  Arr1JLArrayIndexOutOfBoundsException = array of JLArrayIndexOutOfBoundsException;
  Arr2JLArrayIndexOutOfBoundsException = array of Arr1JLArrayIndexOutOfBoundsException;
  Arr3JLArrayIndexOutOfBoundsException = array of Arr2JLArrayIndexOutOfBoundsException;

  JLStringIndexOutOfBoundsException = class;
  Arr1JLStringIndexOutOfBoundsException = array of JLStringIndexOutOfBoundsException;
  Arr2JLStringIndexOutOfBoundsException = array of Arr1JLStringIndexOutOfBoundsException;
  Arr3JLStringIndexOutOfBoundsException = array of Arr2JLStringIndexOutOfBoundsException;

  JUServiceLoader = class;
  Arr1JUServiceLoader = array of JUServiceLoader;
  Arr2JUServiceLoader = array of Arr1JUServiceLoader;
  Arr3JUServiceLoader = array of Arr2JUServiceLoader;

  ADCursorJoiner = class;
  Arr1ADCursorJoiner = array of ADCursorJoiner;
  Arr2ADCursorJoiner = array of Arr1ADCursorJoiner;
  Arr3ADCursorJoiner = array of Arr2ADCursorJoiner;

  AGInterpolator = class;
  Arr1AGInterpolator = array of AGInterpolator;
  Arr2AGInterpolator = array of Arr1AGInterpolator;
  Arr3AGInterpolator = array of Arr2AGInterpolator;

  AGMatrix = class;
  Arr1AGMatrix = array of AGMatrix;
  Arr2AGMatrix = array of Arr1AGMatrix;
  Arr3AGMatrix = array of Arr2AGMatrix;

  AGPaint = class;
  Arr1AGPaint = array of AGPaint;
  Arr2AGPaint = array of Arr1AGPaint;
  Arr3AGPaint = array of Arr2AGPaint;

  AGPath = class;
  Arr1AGPath = array of AGPath;
  Arr2AGPath = array of Arr1AGPath;
  Arr3AGPath = array of Arr2AGPath;

  AGPorterDuff = class;
  Arr1AGPorterDuff = array of AGPorterDuff;
  Arr2AGPorterDuff = array of Arr1AGPorterDuff;
  Arr3AGPorterDuff = array of Arr2AGPorterDuff;

  AGShader = class;
  Arr1AGShader = array of AGShader;
  Arr2AGShader = array of Arr1AGShader;
  Arr3AGShader = array of Arr2AGShader;

  ANLocalSocketAddress = class;
  Arr1ANLocalSocketAddress = array of ANLocalSocketAddress;
  Arr2ANLocalSocketAddress = array of Arr1ANLocalSocketAddress;
  Arr3ANLocalSocketAddress = array of Arr2ANLocalSocketAddress;

  AOAsyncTask = class;
  Arr1AOAsyncTask = array of AOAsyncTask;
  Arr2AOAsyncTask = array of Arr1AOAsyncTask;
  Arr3AOAsyncTask = array of Arr2AOAsyncTask;

  ATSmsMessage = class;
  Arr1ATSmsMessage = array of ATSmsMessage;
  Arr2ATSmsMessage = array of Arr1ATSmsMessage;
  Arr3ATSmsMessage = array of Arr2ATSmsMessage;

  ATGSmsMessage = class;
  Arr1ATGSmsMessage = array of ATGSmsMessage;
  Arr2ATGSmsMessage = array of Arr1ATGSmsMessage;
  Arr3ATGSmsMessage = array of Arr2ATGSmsMessage;

  ATLayout = class;
  Arr1ATLayout = array of ATLayout;
  Arr2ATLayout = array of Arr1ATLayout;
  Arr3ATLayout = array of Arr2ATLayout;

  AUJsonToken = class;
  Arr1AUJsonToken = array of AUJsonToken;
  Arr2AUJsonToken = array of Arr1AUJsonToken;
  Arr3AUJsonToken = array of Arr2AUJsonToken;

  AUXml = class;
  Arr1AUXml = array of AUXml;
  Arr2AUXml = array of Arr1AUXml;
  Arr3AUXml = array of Arr2AUXml;

  AWConsoleMessage = class;
  Arr1AWConsoleMessage = array of AWConsoleMessage;
  Arr2AWConsoleMessage = array of Arr1AWConsoleMessage;
  Arr3AWConsoleMessage = array of Arr2AWConsoleMessage;

  AWWebSettings = class;
  Arr1AWWebSettings = array of AWWebSettings;
  Arr2AWWebSettings = array of Arr1AWWebSettings;
  Arr3AWWebSettings = array of Arr2AWWebSettings;

  JLAElementType = class;
  Arr1JLAElementType = array of JLAElementType;
  Arr2JLAElementType = array of Arr1JLAElementType;
  Arr3JLAElementType = array of Arr2JLAElementType;

  JLARetentionPolicy = class;
  Arr1JLARetentionPolicy = array of JLARetentionPolicy;
  Arr2JLARetentionPolicy = array of Arr1JLARetentionPolicy;
  Arr3JLARetentionPolicy = array of Arr2JLARetentionPolicy;

  JMRoundingMode = class;
  Arr1JMRoundingMode = array of JMRoundingMode;
  Arr2JMRoundingMode = array of Arr1JMRoundingMode;
  Arr3JMRoundingMode = array of Arr2JMRoundingMode;

  JNAuthenticator = class;
  Arr1JNAuthenticator = array of JNAuthenticator;
  Arr2JNAuthenticator = array of Arr1JNAuthenticator;
  Arr3JNAuthenticator = array of Arr2JNAuthenticator;

  JNProxy = class;
  Arr1JNProxy = array of JNProxy;
  Arr2JNProxy = array of Arr1JNProxy;
  Arr3JNProxy = array of Arr2JNProxy;

  JSClientInfoStatus = class;
  Arr1JSClientInfoStatus = array of JSClientInfoStatus;
  Arr2JSClientInfoStatus = array of Arr1JSClientInfoStatus;
  Arr3JSClientInfoStatus = array of Arr2JSClientInfoStatus;

  JSRowIdLifetime = class;
  Arr1JSRowIdLifetime = array of JSRowIdLifetime;
  Arr2JSRowIdLifetime = array of Arr1JSRowIdLifetime;
  Arr3JSRowIdLifetime = array of Arr2JSRowIdLifetime;

  JTNormalizer = class;
  Arr1JTNormalizer = array of JTNormalizer;
  Arr2JTNormalizer = array of Arr1JTNormalizer;
  Arr3JTNormalizer = array of Arr2JTNormalizer;

  JUCTimeUnit = class;
  Arr1JUCTimeUnit = array of JUCTimeUnit;
  Arr2JUCTimeUnit = array of Arr1JUCTimeUnit;
  Arr3JUCTimeUnit = array of Arr2JUCTimeUnit;

  JNSSSLEngineResult = class;
  Arr1JNSSSLEngineResult = array of JNSSSLEngineResult;
  Arr2JNSSSLEngineResult = array of Arr1JNSSSLEngineResult;
  Arr3JNSSSLEngineResult = array of Arr2JNSSSLEngineResult;

  AWWebBackForwardList = class;
  Arr1AWWebBackForwardList = array of AWWebBackForwardList;
  Arr2AWWebBackForwardList = array of Arr1AWWebBackForwardList;
  Arr3AWWebBackForwardList = array of Arr2AWWebBackForwardList;

  JBPropertyChangeSupport = class;
  Arr1JBPropertyChangeSupport = array of JBPropertyChangeSupport;
  Arr2JBPropertyChangeSupport = array of Arr1JBPropertyChangeSupport;
  Arr3JBPropertyChangeSupport = array of Arr2JBPropertyChangeSupport;

  JIExternalizable = interface;
  Arr1JIExternalizable = array of JIExternalizable;
  Arr2JIExternalizable = array of Arr1JIExternalizable;
  Arr3JIExternalizable = array of Arr2JIExternalizable;

  JIFile = class;
  Arr1JIFile = array of JIFile;
  Arr2JIFile = array of Arr1JIFile;
  Arr3JIFile = array of Arr2JIFile;

  JIObjectStreamClass = class;
  Arr1JIObjectStreamClass = array of JIObjectStreamClass;
  Arr2JIObjectStreamClass = array of Arr1JIObjectStreamClass;
  Arr3JIObjectStreamClass = array of Arr2JIObjectStreamClass;

  JLStackTraceElement = class;
  Arr1JLStackTraceElement = array of JLStackTraceElement;
  Arr2JLStackTraceElement = array of Arr1JLStackTraceElement;
  Arr3JLStackTraceElement = array of Arr2JLStackTraceElement;

  JLRProxy = class;
  Arr1JLRProxy = array of JLRProxy;
  Arr2JLRProxy = array of Arr1JLRProxy;
  Arr3JLRProxy = array of Arr2JLRProxy;

  JMMathContext = class;
  Arr1JMMathContext = array of JMMathContext;
  Arr2JMMathContext = array of Arr1JMMathContext;
  Arr3JMMathContext = array of Arr2JMMathContext;

  JNInetAddress = class;
  Arr1JNInetAddress = array of JNInetAddress;
  Arr2JNInetAddress = array of Arr1JNInetAddress;
  Arr3JNInetAddress = array of Arr2JNInetAddress;

  JNSocketAddress = class;
  Arr1JNSocketAddress = array of JNSocketAddress;
  Arr2JNSocketAddress = array of Arr1JNSocketAddress;
  Arr3JNSocketAddress = array of Arr2JNSocketAddress;

  JNURI = class;
  Arr1JNURI = array of JNURI;
  Arr2JNURI = array of Arr1JNURI;
  Arr3JNURI = array of Arr2JNURI;

  JNURL = class;
  Arr1JNURL = array of JNURL;
  Arr2JNURL = array of Arr1JNURL;
  Arr3JNURL = array of Arr2JNURL;

  JSCodeSigner = class;
  Arr1JSCodeSigner = array of JSCodeSigner;
  Arr2JSCodeSigner = array of Arr1JSCodeSigner;
  Arr3JSCodeSigner = array of Arr2JSCodeSigner;

  JSCodeSource = class;
  Arr1JSCodeSource = array of JSCodeSource;
  Arr2JSCodeSource = array of Arr1JSCodeSource;
  Arr3JSCodeSource = array of Arr2JSCodeSource;

  JSGuardedObject = class;
  Arr1JSGuardedObject = array of JSGuardedObject;
  Arr2JSGuardedObject = array of Arr1JSGuardedObject;
  Arr3JSGuardedObject = array of Arr2JSGuardedObject;

  JSKey = interface;
  Arr1JSKey = array of JSKey;
  Arr2JSKey = array of Arr1JSKey;
  Arr3JSKey = array of Arr2JSKey;

  JSKeyPair = class;
  Arr1JSKeyPair = array of JSKeyPair;
  Arr2JSKeyPair = array of Arr1JSKeyPair;
  Arr3JSKeyPair = array of Arr2JSKeyPair;

  JSKeyRep = class;
  Arr1JSKeyRep = array of JSKeyRep;
  Arr2JSKeyRep = array of Arr1JSKeyRep;
  Arr3JSKeyRep = array of Arr2JSKeyRep;

  JSPermissionCollection = class;
  Arr1JSPermissionCollection = array of JSPermissionCollection;
  Arr2JSPermissionCollection = array of Arr1JSPermissionCollection;
  Arr3JSPermissionCollection = array of Arr2JSPermissionCollection;

  JSSecureRandomSpi = class;
  Arr1JSSecureRandomSpi = array of JSSecureRandomSpi;
  Arr2JSSecureRandomSpi = array of Arr1JSSecureRandomSpi;
  Arr3JSSecureRandomSpi = array of Arr2JSSecureRandomSpi;

  JSSignedObject = class;
  Arr1JSSignedObject = array of JSSignedObject;
  Arr2JSSignedObject = array of Arr1JSSignedObject;
  Arr3JSSignedObject = array of Arr2JSSignedObject;

  JSecurityTimestamp = class;
  Arr1JSecurityTimestamp = array of JSecurityTimestamp;
  Arr2JSecurityTimestamp = array of Arr1JSecurityTimestamp;
  Arr3JSecurityTimestamp = array of Arr2JSecurityTimestamp;

  JSCCertPath = class;
  Arr1JSCCertPath = array of JSCCertPath;
  Arr2JSCCertPath = array of Arr1JSCCertPath;
  Arr3JSCCertPath = array of Arr2JSCCertPath;

  JSCCertificate = class;
  Arr1JSCCertificate = array of JSCCertificate;
  Arr2JSCCertificate = array of Arr1JSCCertificate;
  Arr3JSCCertificate = array of Arr2JSCCertificate;

  JSSQLException = class;
  Arr1JSSQLException = array of JSSQLException;
  Arr2JSSQLException = array of Arr1JSSQLException;
  Arr3JSSQLException = array of Arr2JSSQLException;

  JTDateFormatSymbols = class;
  Arr1JTDateFormatSymbols = array of JTDateFormatSymbols;
  Arr2JTDateFormatSymbols = array of Arr1JTDateFormatSymbols;
  Arr3JTDateFormatSymbols = array of Arr2JTDateFormatSymbols;

  JTDecimalFormatSymbols = class;
  Arr1JTDecimalFormatSymbols = array of JTDecimalFormatSymbols;
  Arr2JTDecimalFormatSymbols = array of Arr1JTDecimalFormatSymbols;
  Arr3JTDecimalFormatSymbols = array of Arr2JTDecimalFormatSymbols;

  JUCurrency = class;
  Arr1JUCurrency = array of JUCurrency;
  Arr2JUCurrency = array of Arr1JUCurrency;
  Arr3JUCurrency = array of Arr2JUCurrency;

  JUDate = class;
  Arr1JUDate = array of JUDate;
  Arr2JUDate = array of Arr1JUDate;
  Arr3JUDate = array of Arr2JUDate;

  JUEventObject = class;
  Arr1JUEventObject = array of JUEventObject;
  Arr2JUEventObject = array of Arr1JUEventObject;
  Arr3JUEventObject = array of Arr2JUEventObject;

  JULocale = class;
  Arr1JULocale = array of JULocale;
  Arr2JULocale = array of Arr1JULocale;
  Arr3JULocale = array of Arr2JULocale;

  JURandom = class;
  Arr1JURandom = array of JURandom;
  Arr2JURandom = array of Arr1JURandom;
  Arr3JURandom = array of Arr2JURandom;

  JUTimeZone = class;
  Arr1JUTimeZone = array of JUTimeZone;
  Arr2JUTimeZone = array of Arr1JUTimeZone;
  Arr3JUTimeZone = array of Arr2JUTimeZone;

  JUUUID = class;
  Arr1JUUUID = array of JUUUID;
  Arr2JUUUID = array of Arr1JUUUID;
  Arr3JUUUID = array of Arr2JUUUID;

  JUCSemaphore = class;
  Arr1JUCSemaphore = array of JUCSemaphore;
  Arr2JUCSemaphore = array of Arr1JUCSemaphore;
  Arr3JUCSemaphore = array of Arr2JUCSemaphore;

  JUCAAtomicBoolean = class;
  Arr1JUCAAtomicBoolean = array of JUCAAtomicBoolean;
  Arr2JUCAAtomicBoolean = array of Arr1JUCAAtomicBoolean;
  Arr3JUCAAtomicBoolean = array of Arr2JUCAAtomicBoolean;

  JUCAAtomicIntegerArray = class;
  Arr1JUCAAtomicIntegerArray = array of JUCAAtomicIntegerArray;
  Arr2JUCAAtomicIntegerArray = array of Arr1JUCAAtomicIntegerArray;
  Arr3JUCAAtomicIntegerArray = array of Arr2JUCAAtomicIntegerArray;

  JUCAAtomicLongArray = class;
  Arr1JUCAAtomicLongArray = array of JUCAAtomicLongArray;
  Arr2JUCAAtomicLongArray = array of Arr1JUCAAtomicLongArray;
  Arr3JUCAAtomicLongArray = array of Arr2JUCAAtomicLongArray;

  JUCAAtomicReference = class;
  Arr1JUCAAtomicReference = array of JUCAAtomicReference;
  Arr2JUCAAtomicReference = array of Arr1JUCAAtomicReference;
  Arr3JUCAAtomicReference = array of Arr2JUCAAtomicReference;

  JUCAAtomicReferenceArray = class;
  Arr1JUCAAtomicReferenceArray = array of JUCAAtomicReferenceArray;
  Arr2JUCAAtomicReferenceArray = array of Arr1JUCAAtomicReferenceArray;
  Arr3JUCAAtomicReferenceArray = array of Arr2JUCAAtomicReferenceArray;

  JUCLAbstractOwnableSynchronizer = class;
  Arr1JUCLAbstractOwnableSynchronizer = array of JUCLAbstractOwnableSynchronizer;
  Arr2JUCLAbstractOwnableSynchronizer = array of Arr1JUCLAbstractOwnableSynchronizer;
  Arr3JUCLAbstractOwnableSynchronizer = array of Arr2JUCLAbstractOwnableSynchronizer;

  JULLevel = class;
  Arr1JULLevel = array of JULLevel;
  Arr2JULLevel = array of Arr1JULLevel;
  Arr3JULLevel = array of Arr2JULLevel;

  JULLogRecord = class;
  Arr1JULLogRecord = array of JULLogRecord;
  Arr2JULLogRecord = array of Arr1JULLogRecord;
  Arr3JULLogRecord = array of Arr2JULLogRecord;

  JURPattern = class;
  Arr1JURPattern = array of JURPattern;
  Arr2JURPattern = array of Arr1JURPattern;
  Arr3JURPattern = array of Arr2JURPattern;

  JCSealedObject = class;
  Arr1JCSealedObject = array of JCSealedObject;
  Arr2JCSealedObject = array of Arr1JCSealedObject;
  Arr3JCSealedObject = array of Arr2JCSealedObject;

  JSASubject = class;
  Arr1JSASubject = array of JSASubject;
  Arr2JSASubject = array of Arr1JSASubject;
  Arr3JSASubject = array of Arr2JSASubject;

  JXNQName = class;
  Arr1JXNQName = array of JXNQName;
  Arr2JXNQName = array of Arr1JXNQName;
  Arr3JXNQName = array of Arr2JXNQName;

  ANTagLostException = class;
  Arr1ANTagLostException = array of ANTagLostException;
  Arr2ANTagLostException = array of Arr1ANTagLostException;
  Arr3ANTagLostException = array of Arr2ANTagLostException;

  AUBase64DataException = class;
  Arr1AUBase64DataException = array of AUBase64DataException;
  Arr2AUBase64DataException = array of Arr1AUBase64DataException;
  Arr3AUBase64DataException = array of Arr2AUBase64DataException;

  AUMalformedJsonException = class;
  Arr1AUMalformedJsonException = array of AUMalformedJsonException;
  Arr2AUMalformedJsonException = array of Arr1AUMalformedJsonException;
  Arr3AUMalformedJsonException = array of Arr2AUMalformedJsonException;

  JICharConversionException = class;
  Arr1JICharConversionException = array of JICharConversionException;
  Arr2JICharConversionException = array of Arr1JICharConversionException;
  Arr3JICharConversionException = array of Arr2JICharConversionException;

  JIEOFException = class;
  Arr1JIEOFException = array of JIEOFException;
  Arr2JIEOFException = array of Arr1JIEOFException;
  Arr3JIEOFException = array of Arr2JIEOFException;

  JIFileNotFoundException = class;
  Arr1JIFileNotFoundException = array of JIFileNotFoundException;
  Arr2JIFileNotFoundException = array of Arr1JIFileNotFoundException;
  Arr3JIFileNotFoundException = array of Arr2JIFileNotFoundException;

  JIInterruptedIOException = class;
  Arr1JIInterruptedIOException = array of JIInterruptedIOException;
  Arr2JIInterruptedIOException = array of Arr1JIInterruptedIOException;
  Arr3JIInterruptedIOException = array of Arr2JIInterruptedIOException;

  JIObjectStreamException = class;
  Arr1JIObjectStreamException = array of JIObjectStreamException;
  Arr2JIObjectStreamException = array of Arr1JIObjectStreamException;
  Arr3JIObjectStreamException = array of Arr2JIObjectStreamException;

  JISyncFailedException = class;
  Arr1JISyncFailedException = array of JISyncFailedException;
  Arr2JISyncFailedException = array of Arr1JISyncFailedException;
  Arr3JISyncFailedException = array of Arr2JISyncFailedException;

  JIUTFDataFormatException = class;
  Arr1JIUTFDataFormatException = array of JIUTFDataFormatException;
  Arr2JIUTFDataFormatException = array of Arr1JIUTFDataFormatException;
  Arr3JIUTFDataFormatException = array of Arr2JIUTFDataFormatException;

  JIUnsupportedEncodingException = class;
  Arr1JIUnsupportedEncodingException = array of JIUnsupportedEncodingException;
  Arr2JIUnsupportedEncodingException = array of Arr1JIUnsupportedEncodingException;
  Arr3JIUnsupportedEncodingException = array of Arr2JIUnsupportedEncodingException;

  JNHttpRetryException = class;
  Arr1JNHttpRetryException = array of JNHttpRetryException;
  Arr2JNHttpRetryException = array of Arr1JNHttpRetryException;
  Arr3JNHttpRetryException = array of Arr2JNHttpRetryException;

  JNMalformedURLException = class;
  Arr1JNMalformedURLException = array of JNMalformedURLException;
  Arr2JNMalformedURLException = array of Arr1JNMalformedURLException;
  Arr3JNMalformedURLException = array of Arr2JNMalformedURLException;

  JNProtocolException = class;
  Arr1JNProtocolException = array of JNProtocolException;
  Arr2JNProtocolException = array of Arr1JNProtocolException;
  Arr3JNProtocolException = array of Arr2JNProtocolException;

  JNSocketException = class;
  Arr1JNSocketException = array of JNSocketException;
  Arr2JNSocketException = array of Arr1JNSocketException;
  Arr3JNSocketException = array of Arr2JNSocketException;

  JNUnknownHostException = class;
  Arr1JNUnknownHostException = array of JNUnknownHostException;
  Arr2JNUnknownHostException = array of Arr1JNUnknownHostException;
  Arr3JNUnknownHostException = array of Arr2JNUnknownHostException;

  JNUnknownServiceException = class;
  Arr1JNUnknownServiceException = array of JNUnknownServiceException;
  Arr2JNUnknownServiceException = array of Arr1JNUnknownServiceException;
  Arr3JNUnknownServiceException = array of Arr2JNUnknownServiceException;

  JNCClosedChannelException = class;
  Arr1JNCClosedChannelException = array of JNCClosedChannelException;
  Arr2JNCClosedChannelException = array of Arr1JNCClosedChannelException;
  Arr3JNCClosedChannelException = array of Arr2JNCClosedChannelException;

  JNCFileLockInterruptionException = class;
  Arr1JNCFileLockInterruptionException = array of JNCFileLockInterruptionException;
  Arr2JNCFileLockInterruptionException = array of Arr1JNCFileLockInterruptionException;
  Arr3JNCFileLockInterruptionException = array of Arr2JNCFileLockInterruptionException;

  JUInvalidPropertiesFormatException = class;
  Arr1JUInvalidPropertiesFormatException = array of JUInvalidPropertiesFormatException;
  Arr2JUInvalidPropertiesFormatException = array of Arr1JUInvalidPropertiesFormatException;
  Arr3JUInvalidPropertiesFormatException = array of Arr2JUInvalidPropertiesFormatException;

  JUZZipException = class;
  Arr1JUZZipException = array of JUZZipException;
  Arr2JUZZipException = array of Arr1JUZZipException;
  Arr3JUZZipException = array of Arr2JUZZipException;

  JNSSSLException = class;
  Arr1JNSSSLException = array of JNSSSLException;
  Arr2JNSSSLException = array of Arr1JNSSSLException;
  Arr3JNSSSLException = array of Arr2JNSSSLException;

  ATAssertionFailedError = class;
  Arr1ATAssertionFailedError = array of ATAssertionFailedError;
  Arr2ATAssertionFailedError = array of Arr1ATAssertionFailedError;
  Arr3ATAssertionFailedError = array of Arr2ATAssertionFailedError;

  JIIOError = class;
  Arr1JIIOError = array of JIIOError;
  Arr2JIIOError = array of Arr1JIIOError;
  Arr3JIIOError = array of Arr2JIIOError;

  JLAssertionError = class;
  Arr1JLAssertionError = array of JLAssertionError;
  Arr2JLAssertionError = array of Arr1JLAssertionError;
  Arr3JLAssertionError = array of Arr2JLAssertionError;

  JLThreadDeath = class;
  Arr1JLThreadDeath = array of JLThreadDeath;
  Arr2JLThreadDeath = array of Arr1JLThreadDeath;
  Arr3JLThreadDeath = array of Arr2JLThreadDeath;

  JLVirtualMachineError = class;
  Arr1JLVirtualMachineError = array of JLVirtualMachineError;
  Arr2JLVirtualMachineError = array of Arr1JLVirtualMachineError;
  Arr3JLVirtualMachineError = array of Arr2JLVirtualMachineError;

  JLAAnnotationFormatError = class;
  Arr1JLAAnnotationFormatError = array of JLAAnnotationFormatError;
  Arr2JLAAnnotationFormatError = array of Arr1JLAAnnotationFormatError;
  Arr3JLAAnnotationFormatError = array of Arr2JLAAnnotationFormatError;

  JUServiceConfigurationError = class;
  Arr1JUServiceConfigurationError = array of JUServiceConfigurationError;
  Arr2JUServiceConfigurationError = array of Arr1JUServiceConfigurationError;
  Arr3JUServiceConfigurationError = array of Arr2JUServiceConfigurationError;

  JXPFactoryConfigurationError = class;
  Arr1JXPFactoryConfigurationError = array of JXPFactoryConfigurationError;
  Arr2JXPFactoryConfigurationError = array of Arr1JXPFactoryConfigurationError;
  Arr3JXPFactoryConfigurationError = array of Arr2JXPFactoryConfigurationError;

  JXTTransformerFactoryConfigurationError = class;
  Arr1JXTTransformerFactoryConfigurationError = array of JXTTransformerFactoryConfigurationError;
  Arr2JXTTransformerFactoryConfigurationError = array of Arr1JXTTransformerFactoryConfigurationError;
  Arr3JXTTransformerFactoryConfigurationError = array of Arr2JXTTransformerFactoryConfigurationError;

  JFAssertionFailedError = class;
  Arr1JFAssertionFailedError = array of JFAssertionFailedError;
  Arr2JFAssertionFailedError = array of Arr1JFAssertionFailedError;
  Arr3JFAssertionFailedError = array of Arr2JFAssertionFailedError;

  ATGetChars = interface;
  Arr1ATGetChars = array of ATGetChars;
  Arr2ATGetChars = array of Arr1ATGetChars;
  Arr3ATGetChars = array of Arr2ATGetChars;

  ATSpanned = interface;
  Arr1ATSpanned = array of ATSpanned;
  Arr2ATSpanned = array of Arr1ATSpanned;
  Arr3ATSpanned = array of Arr2ATSpanned;

  JLClassCircularityError = class;
  Arr1JLClassCircularityError = array of JLClassCircularityError;
  Arr2JLClassCircularityError = array of Arr1JLClassCircularityError;
  Arr3JLClassCircularityError = array of Arr2JLClassCircularityError;

  JLClassFormatError = class;
  Arr1JLClassFormatError = array of JLClassFormatError;
  Arr2JLClassFormatError = array of Arr1JLClassFormatError;
  Arr3JLClassFormatError = array of Arr2JLClassFormatError;

  JLExceptionInInitializerError = class;
  Arr1JLExceptionInInitializerError = array of JLExceptionInInitializerError;
  Arr2JLExceptionInInitializerError = array of Arr1JLExceptionInInitializerError;
  Arr3JLExceptionInInitializerError = array of Arr2JLExceptionInInitializerError;

  JLIncompatibleClassChangeError = class;
  Arr1JLIncompatibleClassChangeError = array of JLIncompatibleClassChangeError;
  Arr2JLIncompatibleClassChangeError = array of Arr1JLIncompatibleClassChangeError;
  Arr3JLIncompatibleClassChangeError = array of Arr2JLIncompatibleClassChangeError;

  JLNoClassDefFoundError = class;
  Arr1JLNoClassDefFoundError = array of JLNoClassDefFoundError;
  Arr2JLNoClassDefFoundError = array of Arr1JLNoClassDefFoundError;
  Arr3JLNoClassDefFoundError = array of Arr2JLNoClassDefFoundError;

  JLUnsatisfiedLinkError = class;
  Arr1JLUnsatisfiedLinkError = array of JLUnsatisfiedLinkError;
  Arr2JLUnsatisfiedLinkError = array of Arr1JLUnsatisfiedLinkError;
  Arr3JLUnsatisfiedLinkError = array of Arr2JLUnsatisfiedLinkError;

  JLVerifyError = class;
  Arr1JLVerifyError = array of JLVerifyError;
  Arr2JLVerifyError = array of Arr1JLVerifyError;
  Arr3JLVerifyError = array of Arr2JLVerifyError;

  JLIllegalThreadStateException = class;
  Arr1JLIllegalThreadStateException = array of JLIllegalThreadStateException;
  Arr2JLIllegalThreadStateException = array of Arr1JLIllegalThreadStateException;
  Arr3JLIllegalThreadStateException = array of Arr2JLIllegalThreadStateException;

  JLNumberFormatException = class;
  Arr1JLNumberFormatException = array of JLNumberFormatException;
  Arr2JLNumberFormatException = array of Arr1JLNumberFormatException;
  Arr3JLNumberFormatException = array of Arr2JLNumberFormatException;

  JNCIllegalSelectorException = class;
  Arr1JNCIllegalSelectorException = array of JNCIllegalSelectorException;
  Arr2JNCIllegalSelectorException = array of Arr1JNCIllegalSelectorException;
  Arr3JNCIllegalSelectorException = array of Arr2JNCIllegalSelectorException;

  JNCUnresolvedAddressException = class;
  Arr1JNCUnresolvedAddressException = array of JNCUnresolvedAddressException;
  Arr2JNCUnresolvedAddressException = array of Arr1JNCUnresolvedAddressException;
  Arr3JNCUnresolvedAddressException = array of Arr2JNCUnresolvedAddressException;

  JNCUnsupportedAddressTypeException = class;
  Arr1JNCUnsupportedAddressTypeException = array of JNCUnsupportedAddressTypeException;
  Arr2JNCUnsupportedAddressTypeException = array of Arr1JNCUnsupportedAddressTypeException;
  Arr3JNCUnsupportedAddressTypeException = array of Arr2JNCUnsupportedAddressTypeException;

  JSInvalidParameterException = class;
  Arr1JSInvalidParameterException = array of JSInvalidParameterException;
  Arr2JSInvalidParameterException = array of Arr1JSInvalidParameterException;
  Arr3JSInvalidParameterException = array of Arr2JSInvalidParameterException;

  JUIllegalFormatException = class;
  Arr1JUIllegalFormatException = array of JUIllegalFormatException;
  Arr2JUIllegalFormatException = array of Arr1JUIllegalFormatException;
  Arr3JUIllegalFormatException = array of Arr2JUIllegalFormatException;

  JURPatternSyntaxException = class;
  Arr1JURPatternSyntaxException = array of JURPatternSyntaxException;
  Arr2JURPatternSyntaxException = array of Arr1JURPatternSyntaxException;
  Arr3JURPatternSyntaxException = array of Arr2JURPatternSyntaxException;

  JLInheritableThreadLocal = class;
  Arr1JLInheritableThreadLocal = array of JLInheritableThreadLocal;
  Arr2JLInheritableThreadLocal = array of Arr1JLInheritableThreadLocal;
  Arr3JLInheritableThreadLocal = array of Arr2JLInheritableThreadLocal;

  JLPackage = class;
  Arr1JLPackage = array of JLPackage;
  Arr2JLPackage = array of Arr1JLPackage;
  Arr3JLPackage = array of Arr2JLPackage;

  JLRConstructor = class;
  Arr1JLRConstructor = array of JLRConstructor;
  Arr2JLRConstructor = array of Arr1JLRConstructor;
  Arr3JLRConstructor = array of Arr2JLRConstructor;

  JLRGenericArrayType = interface;
  Arr1JLRGenericArrayType = array of JLRGenericArrayType;
  Arr2JLRGenericArrayType = array of Arr1JLRGenericArrayType;
  Arr3JLRGenericArrayType = array of Arr2JLRGenericArrayType;

  JLRParameterizedType = interface;
  Arr1JLRParameterizedType = array of JLRParameterizedType;
  Arr2JLRParameterizedType = array of Arr1JLRParameterizedType;
  Arr3JLRParameterizedType = array of Arr2JLRParameterizedType;

  JLRTypeVariable = interface;
  Arr1JLRTypeVariable = array of JLRTypeVariable;
  Arr2JLRTypeVariable = array of Arr1JLRTypeVariable;
  Arr3JLRTypeVariable = array of Arr2JLRTypeVariable;

  JLRWildcardType = interface;
  Arr1JLRWildcardType = array of JLRWildcardType;
  Arr2JLRWildcardType = array of Arr1JLRWildcardType;
  Arr3JLRWildcardType = array of Arr2JLRWildcardType;

  JMBigDecimal = class;
  Arr1JMBigDecimal = array of JMBigDecimal;
  Arr2JMBigDecimal = array of Arr1JMBigDecimal;
  Arr3JMBigDecimal = array of Arr2JMBigDecimal;

  JUCAAtomicInteger = class;
  Arr1JUCAAtomicInteger = array of JUCAAtomicInteger;
  Arr2JUCAAtomicInteger = array of Arr1JUCAAtomicInteger;
  Arr3JUCAAtomicInteger = array of Arr2JUCAAtomicInteger;

  JUCAAtomicLong = class;
  Arr1JUCAAtomicLong = array of JUCAAtomicLong;
  Arr2JUCAAtomicLong = array of Arr1JUCAAtomicLong;
  Arr3JUCAAtomicLong = array of Arr2JUCAAtomicLong;

  JNDoubleBuffer = class;
  Arr1JNDoubleBuffer = array of JNDoubleBuffer;
  Arr2JNDoubleBuffer = array of Arr1JNDoubleBuffer;
  Arr3JNDoubleBuffer = array of Arr2JNDoubleBuffer;

  JNFloatBuffer = class;
  Arr1JNFloatBuffer = array of JNFloatBuffer;
  Arr2JNFloatBuffer = array of Arr1JNFloatBuffer;
  Arr3JNFloatBuffer = array of Arr2JNFloatBuffer;

  JNIntBuffer = class;
  Arr1JNIntBuffer = array of JNIntBuffer;
  Arr2JNIntBuffer = array of Arr1JNIntBuffer;
  Arr3JNIntBuffer = array of Arr2JNIntBuffer;

  JNLongBuffer = class;
  Arr1JNLongBuffer = array of JNLongBuffer;
  Arr2JNLongBuffer = array of Arr1JNLongBuffer;
  Arr3JNLongBuffer = array of Arr2JNLongBuffer;

  JNShortBuffer = class;
  Arr1JNShortBuffer = array of JNShortBuffer;
  Arr2JNShortBuffer = array of Arr1JNShortBuffer;
  Arr3JNShortBuffer = array of Arr2JNShortBuffer;

  JNInvalidMarkException = class;
  Arr1JNInvalidMarkException = array of JNInvalidMarkException;
  Arr2JNInvalidMarkException = array of Arr1JNInvalidMarkException;
  Arr3JNInvalidMarkException = array of Arr2JNInvalidMarkException;

  JNCAlreadyConnectedException = class;
  Arr1JNCAlreadyConnectedException = array of JNCAlreadyConnectedException;
  Arr2JNCAlreadyConnectedException = array of Arr1JNCAlreadyConnectedException;
  Arr3JNCAlreadyConnectedException = array of Arr2JNCAlreadyConnectedException;

  JNCCancelledKeyException = class;
  Arr1JNCCancelledKeyException = array of JNCCancelledKeyException;
  Arr2JNCCancelledKeyException = array of Arr1JNCCancelledKeyException;
  Arr3JNCCancelledKeyException = array of Arr2JNCCancelledKeyException;

  JNCClosedSelectorException = class;
  Arr1JNCClosedSelectorException = array of JNCClosedSelectorException;
  Arr2JNCClosedSelectorException = array of Arr1JNCClosedSelectorException;
  Arr3JNCClosedSelectorException = array of Arr2JNCClosedSelectorException;

  JNCConnectionPendingException = class;
  Arr1JNCConnectionPendingException = array of JNCConnectionPendingException;
  Arr2JNCConnectionPendingException = array of Arr1JNCConnectionPendingException;
  Arr3JNCConnectionPendingException = array of Arr2JNCConnectionPendingException;

  JNCIllegalBlockingModeException = class;
  Arr1JNCIllegalBlockingModeException = array of JNCIllegalBlockingModeException;
  Arr2JNCIllegalBlockingModeException = array of Arr1JNCIllegalBlockingModeException;
  Arr3JNCIllegalBlockingModeException = array of Arr2JNCIllegalBlockingModeException;

  JNCNoConnectionPendingException = class;
  Arr1JNCNoConnectionPendingException = array of JNCNoConnectionPendingException;
  Arr2JNCNoConnectionPendingException = array of Arr1JNCNoConnectionPendingException;
  Arr3JNCNoConnectionPendingException = array of Arr2JNCNoConnectionPendingException;

  JNCNonReadableChannelException = class;
  Arr1JNCNonReadableChannelException = array of JNCNonReadableChannelException;
  Arr2JNCNonReadableChannelException = array of Arr1JNCNonReadableChannelException;
  Arr3JNCNonReadableChannelException = array of Arr2JNCNonReadableChannelException;

  JNCNonWritableChannelException = class;
  Arr1JNCNonWritableChannelException = array of JNCNonWritableChannelException;
  Arr2JNCNonWritableChannelException = array of Arr1JNCNonWritableChannelException;
  Arr3JNCNonWritableChannelException = array of Arr2JNCNonWritableChannelException;

  JNCNotYetBoundException = class;
  Arr1JNCNotYetBoundException = array of JNCNotYetBoundException;
  Arr2JNCNotYetBoundException = array of Arr1JNCNotYetBoundException;
  Arr3JNCNotYetBoundException = array of Arr2JNCNotYetBoundException;

  JNCNotYetConnectedException = class;
  Arr1JNCNotYetConnectedException = array of JNCNotYetConnectedException;
  Arr2JNCNotYetConnectedException = array of Arr1JNCNotYetConnectedException;
  Arr3JNCNotYetConnectedException = array of Arr2JNCNotYetConnectedException;

  JNCOverlappingFileLockException = class;
  Arr1JNCOverlappingFileLockException = array of JNCOverlappingFileLockException;
  Arr2JNCOverlappingFileLockException = array of Arr1JNCOverlappingFileLockException;
  Arr3JNCOverlappingFileLockException = array of Arr2JNCOverlappingFileLockException;

  JUFormatterClosedException = class;
  Arr1JUFormatterClosedException = array of JUFormatterClosedException;
  Arr2JUFormatterClosedException = array of Arr1JUFormatterClosedException;
  Arr3JUFormatterClosedException = array of Arr2JUFormatterClosedException;

  JUCCancellationException = class;
  Arr1JUCCancellationException = array of JUCCancellationException;
  Arr2JUCCancellationException = array of Arr1JUCCancellationException;
  Arr3JUCCancellationException = array of Arr2JUCCancellationException;

  JNMappedByteBuffer = class;
  Arr1JNMappedByteBuffer = array of JNMappedByteBuffer;
  Arr2JNMappedByteBuffer = array of Arr1JNMappedByteBuffer;
  Arr3JNMappedByteBuffer = array of Arr2JNMappedByteBuffer;

  JNReadOnlyBufferException = class;
  Arr1JNReadOnlyBufferException = array of JNReadOnlyBufferException;
  Arr2JNReadOnlyBufferException = array of Arr1JNReadOnlyBufferException;
  Arr3JNReadOnlyBufferException = array of Arr2JNReadOnlyBufferException;

  JTRuleBasedCollator = class;
  Arr1JTRuleBasedCollator = array of JTRuleBasedCollator;
  Arr2JTRuleBasedCollator = array of Arr1JTRuleBasedCollator;
  Arr3JTRuleBasedCollator = array of Arr2JTRuleBasedCollator;

  JUSortedMap = interface;
  Arr1JUSortedMap = array of JUSortedMap;
  Arr2JUSortedMap = array of Arr1JUSortedMap;
  Arr3JUSortedMap = array of Arr2JUSortedMap;

  JUCConcurrentMap = interface;
  Arr1JUCConcurrentMap = array of JUCConcurrentMap;
  Arr2JUCConcurrentMap = array of Arr1JUCConcurrentMap;
  Arr3JUCConcurrentMap = array of Arr2JUCConcurrentMap;

  JUJAttributes = class;
  Arr1JUJAttributes = array of JUJAttributes;
  Arr2JUJAttributes = array of Arr1JUJAttributes;
  Arr3JUJAttributes = array of Arr2JUJAttributes;

  JUEnumMap = class;
  Arr1JUEnumMap = array of JUEnumMap;
  Arr2JUEnumMap = array of Arr1JUEnumMap;
  Arr3JUEnumMap = array of Arr2JUEnumMap;

  JUIdentityHashMap = class;
  Arr1JUIdentityHashMap = array of JUIdentityHashMap;
  Arr2JUIdentityHashMap = array of Arr1JUIdentityHashMap;
  Arr3JUIdentityHashMap = array of Arr2JUIdentityHashMap;

  JUWeakHashMap = class;
  Arr1JUWeakHashMap = array of JUWeakHashMap;
  Arr2JUWeakHashMap = array of Arr1JUWeakHashMap;
  Arr3JUWeakHashMap = array of Arr2JUWeakHashMap;

  JUGregorianCalendar = class;
  Arr1JUGregorianCalendar = array of JUGregorianCalendar;
  Arr2JUGregorianCalendar = array of Arr1JUGregorianCalendar;
  Arr3JUGregorianCalendar = array of Arr2JUGregorianCalendar;

  JUCCopyOnWriteArraySet = class;
  Arr1JUCCopyOnWriteArraySet = array of JUCCopyOnWriteArraySet;
  Arr2JUCCopyOnWriteArraySet = array of Arr1JUCCopyOnWriteArraySet;
  Arr3JUCCopyOnWriteArraySet = array of Arr2JUCCopyOnWriteArraySet;

  JUHashSet = class;
  Arr1JUHashSet = array of JUHashSet;
  Arr2JUHashSet = array of Arr1JUHashSet;
  Arr3JUHashSet = array of Arr2JUHashSet;

  JUSortedSet = interface;
  Arr1JUSortedSet = array of JUSortedSet;
  Arr2JUSortedSet = array of Arr1JUSortedSet;
  Arr3JUSortedSet = array of Arr2JUSortedSet;

  JULinkedHashMap = class;
  Arr1JULinkedHashMap = array of JULinkedHashMap;
  Arr2JULinkedHashMap = array of Arr1JULinkedHashMap;
  Arr3JULinkedHashMap = array of Arr2JULinkedHashMap;

  JUList = interface;
  Arr1JUList = array of JUList;
  Arr2JUList = array of Arr1JUList;
  Arr3JUList = array of Arr2JUList;

  JUQueue = interface;
  Arr1JUQueue = array of JUQueue;
  Arr2JUQueue = array of Arr1JUQueue;
  Arr3JUQueue = array of Arr2JUQueue;

  AVAInterpolator = interface;
  Arr1AVAInterpolator = array of AVAInterpolator;
  Arr2AVAInterpolator = array of Arr1AVAInterpolator;
  Arr3AVAInterpolator = array of Arr2AVAInterpolator;

  AAArgbEvaluator = class;
  Arr1AAArgbEvaluator = array of AAArgbEvaluator;
  Arr2AAArgbEvaluator = array of Arr1AAArgbEvaluator;
  Arr3AAArgbEvaluator = array of Arr2AAArgbEvaluator;

  AAFloatEvaluator = class;
  Arr1AAFloatEvaluator = array of AAFloatEvaluator;
  Arr2AAFloatEvaluator = array of Arr1AAFloatEvaluator;
  Arr3AAFloatEvaluator = array of Arr2AAFloatEvaluator;

  AAIntEvaluator = class;
  Arr1AAIntEvaluator = array of AAIntEvaluator;
  Arr2AAIntEvaluator = array of Arr1AAIntEvaluator;
  Arr3AAIntEvaluator = array of Arr2AAIntEvaluator;

  AABFileBackupHelper = class;
  Arr1AABFileBackupHelper = array of AABFileBackupHelper;
  Arr2AABFileBackupHelper = array of Arr1AABFileBackupHelper;
  Arr3AABFileBackupHelper = array of Arr2AABFileBackupHelper;

  AABSharedPreferencesBackupHelper = class;
  Arr1AABSharedPreferencesBackupHelper = array of AABSharedPreferencesBackupHelper;
  Arr2AABSharedPreferencesBackupHelper = array of Arr1AABSharedPreferencesBackupHelper;
  Arr3AABSharedPreferencesBackupHelper = array of Arr2AABSharedPreferencesBackupHelper;

  ABBluetoothA2dp = class;
  Arr1ABBluetoothA2dp = array of ABBluetoothA2dp;
  Arr2ABBluetoothA2dp = array of Arr1ABBluetoothA2dp;
  Arr3ABBluetoothA2dp = array of Arr2ABBluetoothA2dp;

  ABBluetoothAdapter = class;
  Arr1ABBluetoothAdapter = array of ABBluetoothAdapter;
  Arr2ABBluetoothAdapter = array of Arr1ABBluetoothAdapter;
  Arr3ABBluetoothAdapter = array of Arr2ABBluetoothAdapter;

  ABBluetoothHeadset = class;
  Arr1ABBluetoothHeadset = array of ABBluetoothHeadset;
  Arr2ABBluetoothHeadset = array of Arr1ABBluetoothHeadset;
  Arr3ABBluetoothHeadset = array of Arr2ABBluetoothHeadset;

  ABBluetoothHealth = class;
  Arr1ABBluetoothHealth = array of ABBluetoothHealth;
  Arr2ABBluetoothHealth = array of Arr1ABBluetoothHealth;
  Arr3ABBluetoothHealth = array of Arr2ABBluetoothHealth;

  AAADeviceAdminReceiver = class;
  Arr1AAADeviceAdminReceiver = array of AAADeviceAdminReceiver;
  Arr2AAADeviceAdminReceiver = array of Arr1AAADeviceAdminReceiver;
  Arr3AAADeviceAdminReceiver = array of Arr2AAADeviceAdminReceiver;

  AAAppWidgetProvider = class;
  Arr1AAAppWidgetProvider = array of AAAppWidgetProvider;
  Arr2AAAppWidgetProvider = array of Arr1AAAppWidgetProvider;
  Arr3AAAppWidgetProvider = array of Arr2AAAppWidgetProvider;

  ACComponentCallbacks2 = interface;
  Arr1ACComponentCallbacks2 = array of ACComponentCallbacks2;
  Arr2ACComponentCallbacks2 = array of Arr1ACComponentCallbacks2;
  Arr3ACComponentCallbacks2 = array of Arr2ACComponentCallbacks2;

  ATMMockContentResolver = class;
  Arr1ATMMockContentResolver = array of ATMMockContentResolver;
  Arr2ATMMockContentResolver = array of Arr1ATMMockContentResolver;
  Arr3ATMMockContentResolver = array of Arr2ATMMockContentResolver;

  AASearchManager = class;
  Arr1AASearchManager = array of AASearchManager;
  Arr2AASearchManager = array of Arr1AASearchManager;
  Arr3AASearchManager = array of Arr2AASearchManager;

  ATMMockDialogInterface = class;
  Arr1ATMMockDialogInterface = array of ATMMockDialogInterface;
  Arr2ATMMockDialogInterface = array of Arr1ATMMockDialogInterface;
  Arr3ATMMockDialogInterface = array of Arr2ATMMockDialogInterface;

  ACAsyncTaskLoader = class;
  Arr1ACAsyncTaskLoader = array of ACAsyncTaskLoader;
  Arr2ACAsyncTaskLoader = array of Arr1ACAsyncTaskLoader;
  Arr3ACAsyncTaskLoader = array of Arr2ACAsyncTaskLoader;

  AMMediaScannerConnection = class;
  Arr1AMMediaScannerConnection = array of AMMediaScannerConnection;
  Arr2AMMediaScannerConnection = array of Arr1AMMediaScannerConnection;
  Arr3AMMediaScannerConnection = array of Arr2AMMediaScannerConnection;

  ADCrossProcessCursor = interface;
  Arr1ADCrossProcessCursor = array of ADCrossProcessCursor;
  Arr2ADCrossProcessCursor = array of Arr1ADCrossProcessCursor;
  Arr3ADCrossProcessCursor = array of Arr2ADCrossProcessCursor;

  ADCursorWrapper = class;
  Arr1ADCursorWrapper = array of ADCursorWrapper;
  Arr2ADCursorWrapper = array of Arr1ADCursorWrapper;
  Arr3ADCursorWrapper = array of Arr2ADCursorWrapper;

  ATMMockCursor = class;
  Arr1ATMMockCursor = array of ATMMockCursor;
  Arr2ATMMockCursor = array of Arr1ATMMockCursor;
  Arr3ATMMockCursor = array of Arr2ATMMockCursor;

  ADDefaultDatabaseErrorHandler = class;
  Arr1ADDefaultDatabaseErrorHandler = array of ADDefaultDatabaseErrorHandler;
  Arr2ADDefaultDatabaseErrorHandler = array of Arr1ADDefaultDatabaseErrorHandler;
  Arr3ADDefaultDatabaseErrorHandler = array of Arr2ADDefaultDatabaseErrorHandler;

  ADContentObservable = class;
  Arr1ADContentObservable = array of ADContentObservable;
  Arr2ADContentObservable = array of Arr1ADContentObservable;
  Arr3ADContentObservable = array of Arr2ADContentObservable;

  ADDataSetObservable = class;
  Arr1ADDataSetObservable = array of ADDataSetObservable;
  Arr2ADDataSetObservable = array of Arr1ADDataSetObservable;
  Arr3ADDataSetObservable = array of Arr2ADDataSetObservable;

  ADSSQLiteDatabase = class;
  Arr1ADSSQLiteDatabase = array of ADSSQLiteDatabase;
  Arr2ADSSQLiteDatabase = array of Arr1ADSSQLiteDatabase;
  Arr3ADSSQLiteDatabase = array of Arr2ADSSQLiteDatabase;

  ADSSQLiteProgram = class;
  Arr1ADSSQLiteProgram = array of ADSSQLiteProgram;
  Arr2ADSSQLiteProgram = array of Arr1ADSSQLiteProgram;
  Arr3ADSSQLiteProgram = array of Arr2ADSSQLiteProgram;

  ADDrmErrorEvent = class;
  Arr1ADDrmErrorEvent = array of ADDrmErrorEvent;
  Arr2ADDrmErrorEvent = array of Arr1ADDrmErrorEvent;
  Arr3ADDrmErrorEvent = array of Arr2ADDrmErrorEvent;

  ADDrmInfoEvent = class;
  Arr1ADDrmInfoEvent = array of ADDrmInfoEvent;
  Arr2ADDrmInfoEvent = array of Arr1ADDrmInfoEvent;
  Arr3ADDrmInfoEvent = array of Arr2ADDrmInfoEvent;

  AGColorMatrixColorFilter = class;
  Arr1AGColorMatrixColorFilter = array of AGColorMatrixColorFilter;
  Arr2AGColorMatrixColorFilter = array of Arr1AGColorMatrixColorFilter;
  Arr3AGColorMatrixColorFilter = array of Arr2AGColorMatrixColorFilter;

  AGLightingColorFilter = class;
  Arr1AGLightingColorFilter = array of AGLightingColorFilter;
  Arr2AGLightingColorFilter = array of Arr1AGLightingColorFilter;
  Arr3AGLightingColorFilter = array of Arr2AGLightingColorFilter;

  AGPaintFlagsDrawFilter = class;
  Arr1AGPaintFlagsDrawFilter = array of AGPaintFlagsDrawFilter;
  Arr2AGPaintFlagsDrawFilter = array of Arr1AGPaintFlagsDrawFilter;
  Arr3AGPaintFlagsDrawFilter = array of Arr2AGPaintFlagsDrawFilter;

  AGBlurMaskFilter = class;
  Arr1AGBlurMaskFilter = array of AGBlurMaskFilter;
  Arr2AGBlurMaskFilter = array of Arr1AGBlurMaskFilter;
  Arr3AGBlurMaskFilter = array of Arr2AGBlurMaskFilter;

  AGEmbossMaskFilter = class;
  Arr1AGEmbossMaskFilter = array of AGEmbossMaskFilter;
  Arr2AGEmbossMaskFilter = array of Arr1AGEmbossMaskFilter;
  Arr3AGEmbossMaskFilter = array of Arr2AGEmbossMaskFilter;

  AGComposePathEffect = class;
  Arr1AGComposePathEffect = array of AGComposePathEffect;
  Arr2AGComposePathEffect = array of Arr1AGComposePathEffect;
  Arr3AGComposePathEffect = array of Arr2AGComposePathEffect;

  AGCornerPathEffect = class;
  Arr1AGCornerPathEffect = array of AGCornerPathEffect;
  Arr2AGCornerPathEffect = array of Arr1AGCornerPathEffect;
  Arr3AGCornerPathEffect = array of Arr2AGCornerPathEffect;

  AGDashPathEffect = class;
  Arr1AGDashPathEffect = array of AGDashPathEffect;
  Arr2AGDashPathEffect = array of Arr1AGDashPathEffect;
  Arr3AGDashPathEffect = array of Arr2AGDashPathEffect;

  AGDiscretePathEffect = class;
  Arr1AGDiscretePathEffect = array of AGDiscretePathEffect;
  Arr2AGDiscretePathEffect = array of Arr1AGDiscretePathEffect;
  Arr3AGDiscretePathEffect = array of Arr2AGDiscretePathEffect;

  AGPathDashPathEffect = class;
  Arr1AGPathDashPathEffect = array of AGPathDashPathEffect;
  Arr2AGPathDashPathEffect = array of Arr1AGPathDashPathEffect;
  Arr3AGPathDashPathEffect = array of Arr2AGPathDashPathEffect;

  AGSumPathEffect = class;
  Arr1AGSumPathEffect = array of AGSumPathEffect;
  Arr2AGSumPathEffect = array of Arr1AGSumPathEffect;
  Arr3AGSumPathEffect = array of Arr2AGSumPathEffect;

  AGLayerRasterizer = class;
  Arr1AGLayerRasterizer = array of AGLayerRasterizer;
  Arr2AGLayerRasterizer = array of Arr1AGLayerRasterizer;
  Arr3AGLayerRasterizer = array of Arr2AGLayerRasterizer;

  AVOrientationListener = class;
  Arr1AVOrientationListener = array of AVOrientationListener;
  Arr2AVOrientationListener = array of Arr1AVOrientationListener;
  Arr3AVOrientationListener = array of Arr2AVOrientationListener;

  AMABassBoost = class;
  Arr1AMABassBoost = array of AMABassBoost;
  Arr2AMABassBoost = array of Arr1AMABassBoost;
  Arr3AMABassBoost = array of Arr2AMABassBoost;

  AMAEnvironmentalReverb = class;
  Arr1AMAEnvironmentalReverb = array of AMAEnvironmentalReverb;
  Arr2AMAEnvironmentalReverb = array of Arr1AMAEnvironmentalReverb;
  Arr3AMAEnvironmentalReverb = array of Arr2AMAEnvironmentalReverb;

  AMAEqualizer = class;
  Arr1AMAEqualizer = array of AMAEqualizer;
  Arr2AMAEqualizer = array of Arr1AMAEqualizer;
  Arr3AMAEqualizer = array of Arr2AMAEqualizer;

  AMAPresetReverb = class;
  Arr1AMAPresetReverb = array of AMAPresetReverb;
  Arr2AMAPresetReverb = array of Arr1AMAPresetReverb;
  Arr3AMAPresetReverb = array of Arr2AMAPresetReverb;

  AMAVirtualizer = class;
  Arr1AMAVirtualizer = array of AMAVirtualizer;
  Arr2AMAVirtualizer = array of Arr1AMAVirtualizer;
  Arr3AMAVirtualizer = array of Arr2AMAVirtualizer;

  ANRAudioStream = class;
  Arr1ANRAudioStream = array of ANRAudioStream;
  Arr2ANRAudioStream = array of Arr1ANRAudioStream;
  Arr3ANRAudioStream = array of Arr2ANRAudioStream;

  ANSSipManager = class;
  Arr1ANSSipManager = array of ANSSipManager;
  Arr2ANSSipManager = array of Arr1ANSSipManager;
  Arr3ANSSipManager = array of Arr2ANSSipManager;

  AOGLES11 = class;
  Arr1AOGLES11 = array of AOGLES11;
  Arr2AOGLES11 = array of Arr1AOGLES11;
  Arr3AOGLES11 = array of Arr2AOGLES11;

  ACAsyncQueryHandler = class;
  Arr1ACAsyncQueryHandler = array of ACAsyncQueryHandler;
  Arr2ACAsyncQueryHandler = array of Arr1ACAsyncQueryHandler;
  Arr3ACAsyncQueryHandler = array of Arr2ACAsyncQueryHandler;

  AWHttpAuthHandler = class;
  Arr1AWHttpAuthHandler = array of AWHttpAuthHandler;
  Arr2AWHttpAuthHandler = array of Arr1AWHttpAuthHandler;
  Arr3AWHttpAuthHandler = array of Arr2AWHttpAuthHandler;

  AWSslErrorHandler = class;
  Arr1AWSslErrorHandler = array of AWSslErrorHandler;
  Arr2AWSslErrorHandler = array of Arr1AWSslErrorHandler;
  Arr3AWSslErrorHandler = array of Arr2AWSslErrorHandler;

  AOBinder = class;
  Arr1AOBinder = array of AOBinder;
  Arr2AOBinder = array of Arr1AOBinder;
  Arr3AOBinder = array of Arr2AOBinder;

  AAAccessibilityServiceInfo = class;
  Arr1AAAccessibilityServiceInfo = array of AAAccessibilityServiceInfo;
  Arr2AAAccessibilityServiceInfo = array of Arr1AAAccessibilityServiceInfo;
  Arr3AAAccessibilityServiceInfo = array of Arr2AAAccessibilityServiceInfo;

  AAAccount = class;
  Arr1AAAccount = array of AAAccount;
  Arr2AAAccount = array of Arr1AAAccount;
  Arr3AAAccount = array of Arr2AAAccount;

  AAAccountAuthenticatorResponse = class;
  Arr1AAAccountAuthenticatorResponse = array of AAAccountAuthenticatorResponse;
  Arr2AAAccountAuthenticatorResponse = array of Arr1AAAccountAuthenticatorResponse;
  Arr3AAAccountAuthenticatorResponse = array of Arr2AAAccountAuthenticatorResponse;

  AAAuthenticatorDescription = class;
  Arr1AAAuthenticatorDescription = array of AAAuthenticatorDescription;
  Arr2AAAuthenticatorDescription = array of Arr1AAAuthenticatorDescription;
  Arr3AAAuthenticatorDescription = array of Arr2AAAuthenticatorDescription;

  AAApplicationErrorReport = class;
  Arr1AAApplicationErrorReport = array of AAApplicationErrorReport;
  Arr2AAApplicationErrorReport = array of Arr1AAApplicationErrorReport;
  Arr3AAApplicationErrorReport = array of Arr2AAApplicationErrorReport;

  AASearchableInfo = class;
  Arr1AASearchableInfo = array of AASearchableInfo;
  Arr2AASearchableInfo = array of Arr1AASearchableInfo;
  Arr3AASearchableInfo = array of Arr2AASearchableInfo;

  AAWallpaperInfo = class;
  Arr1AAWallpaperInfo = array of AAWallpaperInfo;
  Arr2AAWallpaperInfo = array of Arr1AAWallpaperInfo;
  Arr3AAWallpaperInfo = array of Arr2AAWallpaperInfo;

  AAADeviceAdminInfo = class;
  Arr1AAADeviceAdminInfo = array of AAADeviceAdminInfo;
  Arr2AAADeviceAdminInfo = array of Arr1AAADeviceAdminInfo;
  Arr3AAADeviceAdminInfo = array of Arr2AAADeviceAdminInfo;

  AAAppWidgetProviderInfo = class;
  Arr1AAAppWidgetProviderInfo = array of AAAppWidgetProviderInfo;
  Arr2AAAppWidgetProviderInfo = array of Arr1AAAppWidgetProviderInfo;
  Arr3AAAppWidgetProviderInfo = array of Arr2AAAppWidgetProviderInfo;

  ABBluetoothClass = class;
  Arr1ABBluetoothClass = array of ABBluetoothClass;
  Arr2ABBluetoothClass = array of Arr1ABBluetoothClass;
  Arr3ABBluetoothClass = array of Arr2ABBluetoothClass;

  ABBluetoothDevice = class;
  Arr1ABBluetoothDevice = array of ABBluetoothDevice;
  Arr2ABBluetoothDevice = array of Arr1ABBluetoothDevice;
  Arr3ABBluetoothDevice = array of Arr2ABBluetoothDevice;

  ABBluetoothHealthAppConfiguration = class;
  Arr1ABBluetoothHealthAppConfiguration = array of ABBluetoothHealthAppConfiguration;
  Arr2ABBluetoothHealthAppConfiguration = array of Arr1ABBluetoothHealthAppConfiguration;
  Arr3ABBluetoothHealthAppConfiguration = array of Arr2ABBluetoothHealthAppConfiguration;

  ACClipData = class;
  Arr1ACClipData = array of ACClipData;
  Arr2ACClipData = array of Arr1ACClipData;
  Arr3ACClipData = array of Arr2ACClipData;

  ACClipDescription = class;
  Arr1ACClipDescription = array of ACClipDescription;
  Arr2ACClipDescription = array of Arr1ACClipDescription;
  Arr3ACClipDescription = array of Arr2ACClipDescription;

  ACComponentName = class;
  Arr1ACComponentName = array of ACComponentName;
  Arr2ACComponentName = array of Arr1ACComponentName;
  Arr3ACComponentName = array of Arr2ACComponentName;

  ACContentProviderOperation = class;
  Arr1ACContentProviderOperation = array of ACContentProviderOperation;
  Arr2ACContentProviderOperation = array of Arr1ACContentProviderOperation;
  Arr3ACContentProviderOperation = array of Arr2ACContentProviderOperation;

  ACContentProviderResult = class;
  Arr1ACContentProviderResult = array of ACContentProviderResult;
  Arr2ACContentProviderResult = array of Arr1ACContentProviderResult;
  Arr3ACContentProviderResult = array of Arr2ACContentProviderResult;

  ACContentValues = class;
  Arr1ACContentValues = array of ACContentValues;
  Arr2ACContentValues = array of Arr1ACContentValues;
  Arr3ACContentValues = array of Arr2ACContentValues;

  ACIntent = class;
  Arr1ACIntent = array of ACIntent;
  Arr2ACIntent = array of Arr1ACIntent;
  Arr3ACIntent = array of Arr2ACIntent;

  ACPeriodicSync = class;
  Arr1ACPeriodicSync = array of ACPeriodicSync;
  Arr2ACPeriodicSync = array of Arr1ACPeriodicSync;
  Arr3ACPeriodicSync = array of Arr2ACPeriodicSync;

  ACSyncAdapterType = class;
  Arr1ACSyncAdapterType = array of ACSyncAdapterType;
  Arr2ACSyncAdapterType = array of Arr1ACSyncAdapterType;
  Arr3ACSyncAdapterType = array of Arr2ACSyncAdapterType;

  ACSyncInfo = class;
  Arr1ACSyncInfo = array of ACSyncInfo;
  Arr2ACSyncInfo = array of Arr1ACSyncInfo;
  Arr3ACSyncInfo = array of Arr2ACSyncInfo;

  ACSyncResult = class;
  Arr1ACSyncResult = array of ACSyncResult;
  Arr2ACSyncResult = array of Arr1ACSyncResult;
  Arr3ACSyncResult = array of Arr2ACSyncResult;

  ACSyncStats = class;
  Arr1ACSyncStats = array of ACSyncStats;
  Arr2ACSyncStats = array of Arr1ACSyncStats;
  Arr3ACSyncStats = array of Arr2ACSyncStats;

  ACPConfigurationInfo = class;
  Arr1ACPConfigurationInfo = array of ACPConfigurationInfo;
  Arr2ACPConfigurationInfo = array of Arr1ACPConfigurationInfo;
  Arr3ACPConfigurationInfo = array of Arr2ACPConfigurationInfo;

  ACPFeatureInfo = class;
  Arr1ACPFeatureInfo = array of ACPFeatureInfo;
  Arr2ACPFeatureInfo = array of Arr1ACPFeatureInfo;
  Arr3ACPFeatureInfo = array of Arr2ACPFeatureInfo;

  ACPPackageInfo = class;
  Arr1ACPPackageInfo = array of ACPPackageInfo;
  Arr2ACPPackageInfo = array of Arr1ACPPackageInfo;
  Arr3ACPPackageInfo = array of Arr2ACPPackageInfo;

  ACPPackageStats = class;
  Arr1ACPPackageStats = array of ACPPackageStats;
  Arr2ACPPackageStats = array of Arr1ACPPackageStats;
  Arr3ACPPackageStats = array of Arr2ACPPackageStats;

  ACPResolveInfo = class;
  Arr1ACPResolveInfo = array of ACPResolveInfo;
  Arr2ACPResolveInfo = array of Arr1ACPResolveInfo;
  Arr3ACPResolveInfo = array of Arr2ACPResolveInfo;

  ACPSignature = class;
  Arr1ACPSignature = array of ACPSignature;
  Arr2ACPSignature = array of Arr1ACPSignature;
  Arr3ACPSignature = array of Arr2ACPSignature;

  ACRColorStateList = class;
  Arr1ACRColorStateList = array of ACRColorStateList;
  Arr2ACRColorStateList = array of Arr1ACRColorStateList;
  Arr3ACRColorStateList = array of Arr2ACRColorStateList;

  ACRConfiguration = class;
  Arr1ACRConfiguration = array of ACRConfiguration;
  Arr2ACRConfiguration = array of Arr1ACRConfiguration;
  Arr3ACRConfiguration = array of Arr2ACRConfiguration;

  ACRObbInfo = class;
  Arr1ACRObbInfo = array of ACRObbInfo;
  Arr2ACRObbInfo = array of Arr1ACRObbInfo;
  Arr3ACRObbInfo = array of Arr2ACRObbInfo;

  ADCursorWindow = class;
  Arr1ADCursorWindow = array of ADCursorWindow;
  Arr2ADCursorWindow = array of Arr1ADCursorWindow;
  Arr3ADCursorWindow = array of Arr2ADCursorWindow;

  AGGesture = class;
  Arr1AGGesture = array of AGGesture;
  Arr2AGGesture = array of Arr1AGGesture;
  Arr3AGGesture = array of Arr2AGGesture;

  AGBitmap = class;
  Arr1AGBitmap = array of AGBitmap;
  Arr2AGBitmap = array of Arr1AGBitmap;
  Arr3AGBitmap = array of Arr2AGBitmap;

  AGPoint = class;
  Arr1AGPoint = array of AGPoint;
  Arr2AGPoint = array of Arr1AGPoint;
  Arr3AGPoint = array of Arr2AGPoint;

  AGPointF = class;
  Arr1AGPointF = array of AGPointF;
  Arr2AGPointF = array of Arr1AGPointF;
  Arr3AGPointF = array of Arr2AGPointF;

  AGRect = class;
  Arr1AGRect = array of AGRect;
  Arr2AGRect = array of Arr1AGRect;
  Arr3AGRect = array of Arr2AGRect;

  AGRectF = class;
  Arr1AGRectF = array of AGRectF;
  Arr2AGRectF = array of Arr1AGRectF;
  Arr3AGRectF = array of Arr2AGRectF;

  AGRegion = class;
  Arr1AGRegion = array of AGRegion;
  Arr2AGRegion = array of Arr1AGRegion;
  Arr3AGRegion = array of Arr2AGRegion;

  AHUUsbAccessory = class;
  Arr1AHUUsbAccessory = array of AHUUsbAccessory;
  Arr2AHUUsbAccessory = array of Arr1AHUUsbAccessory;
  Arr3AHUUsbAccessory = array of Arr2AHUUsbAccessory;

  AHUUsbDevice = class;
  Arr1AHUUsbDevice = array of AHUUsbDevice;
  Arr2AHUUsbDevice = array of Arr1AHUUsbDevice;
  Arr3AHUUsbDevice = array of Arr2AHUUsbDevice;

  AHUUsbEndpoint = class;
  Arr1AHUUsbEndpoint = array of AHUUsbEndpoint;
  Arr2AHUUsbEndpoint = array of Arr1AHUUsbEndpoint;
  Arr3AHUUsbEndpoint = array of Arr2AHUUsbEndpoint;

  AHUUsbInterface = class;
  Arr1AHUUsbInterface = array of AHUUsbInterface;
  Arr2AHUUsbInterface = array of Arr1AHUUsbInterface;
  Arr3AHUUsbInterface = array of Arr2AHUUsbInterface;

  ALAddress = class;
  Arr1ALAddress = array of ALAddress;
  Arr2ALAddress = array of Arr1ALAddress;
  Arr3ALAddress = array of Arr2ALAddress;

  ALCriteria = class;
  Arr1ALCriteria = array of ALCriteria;
  Arr2ALCriteria = array of Arr1ALCriteria;
  Arr3ALCriteria = array of Arr2ALCriteria;

  ALLocation = class;
  Arr1ALLocation = array of ALLocation;
  Arr2ALLocation = array of Arr1ALLocation;
  Arr3ALLocation = array of Arr2ALLocation;

  ANDhcpInfo = class;
  Arr1ANDhcpInfo = array of ANDhcpInfo;
  Arr2ANDhcpInfo = array of Arr1ANDhcpInfo;
  Arr3ANDhcpInfo = array of Arr2ANDhcpInfo;

  ANNetworkInfo = class;
  Arr1ANNetworkInfo = array of ANNetworkInfo;
  Arr2ANNetworkInfo = array of Arr1ANNetworkInfo;
  Arr3ANNetworkInfo = array of Arr2ANNetworkInfo;

  ANUri = class;
  Arr1ANUri = array of ANUri;
  Arr2ANUri = array of Arr1ANUri;
  Arr3ANUri = array of Arr2ANUri;

  ANSSipProfile = class;
  Arr1ANSSipProfile = array of ANSSipProfile;
  Arr2ANSSipProfile = array of Arr1ANSSipProfile;
  Arr3ANSSipProfile = array of Arr2ANSSipProfile;

  ANWScanResult = class;
  Arr1ANWScanResult = array of ANWScanResult;
  Arr2ANWScanResult = array of Arr1ANWScanResult;
  Arr3ANWScanResult = array of Arr2ANWScanResult;

  ANWSupplicantState = class;
  Arr1ANWSupplicantState = array of ANWSupplicantState;
  Arr2ANWSupplicantState = array of Arr1ANWSupplicantState;
  Arr3ANWSupplicantState = array of Arr2ANWSupplicantState;

  ANWWifiConfiguration = class;
  Arr1ANWWifiConfiguration = array of ANWWifiConfiguration;
  Arr2ANWWifiConfiguration = array of Arr1ANWWifiConfiguration;
  Arr3ANWWifiConfiguration = array of Arr2ANWWifiConfiguration;

  ANWWpsInfo = class;
  Arr1ANWWpsInfo = array of ANWWpsInfo;
  Arr2ANWWpsInfo = array of Arr1ANWWpsInfo;
  Arr3ANWWpsInfo = array of Arr2ANWWpsInfo;

  ANWPWifiP2pConfig = class;
  Arr1ANWPWifiP2pConfig = array of ANWPWifiP2pConfig;
  Arr2ANWPWifiP2pConfig = array of Arr1ANWPWifiP2pConfig;
  Arr3ANWPWifiP2pConfig = array of Arr2ANWPWifiP2pConfig;

  ANWPWifiP2pDevice = class;
  Arr1ANWPWifiP2pDevice = array of ANWPWifiP2pDevice;
  Arr2ANWPWifiP2pDevice = array of Arr1ANWPWifiP2pDevice;
  Arr3ANWPWifiP2pDevice = array of Arr2ANWPWifiP2pDevice;

  ANWPWifiP2pDeviceList = class;
  Arr1ANWPWifiP2pDeviceList = array of ANWPWifiP2pDeviceList;
  Arr2ANWPWifiP2pDeviceList = array of Arr1ANWPWifiP2pDeviceList;
  Arr3ANWPWifiP2pDeviceList = array of Arr2ANWPWifiP2pDeviceList;

  ANWPWifiP2pGroup = class;
  Arr1ANWPWifiP2pGroup = array of ANWPWifiP2pGroup;
  Arr2ANWPWifiP2pGroup = array of Arr1ANWPWifiP2pGroup;
  Arr3ANWPWifiP2pGroup = array of Arr2ANWPWifiP2pGroup;

  ANWPWifiP2pInfo = class;
  Arr1ANWPWifiP2pInfo = array of ANWPWifiP2pInfo;
  Arr2ANWPWifiP2pInfo = array of Arr1ANWPWifiP2pInfo;
  Arr3ANWPWifiP2pInfo = array of Arr2ANWPWifiP2pInfo;

  ANNdefMessage = class;
  Arr1ANNdefMessage = array of ANNdefMessage;
  Arr2ANNdefMessage = array of Arr1ANNdefMessage;
  Arr3ANNdefMessage = array of Arr2ANNdefMessage;

  ANNdefRecord = class;
  Arr1ANNdefRecord = array of ANNdefRecord;
  Arr2ANNdefRecord = array of Arr1ANNdefRecord;
  Arr3ANNdefRecord = array of Arr2ANNdefRecord;

  ANTag = class;
  Arr1ANTag = array of ANTag;
  Arr2ANTag = array of Arr1ANTag;
  Arr3ANTag = array of Arr2ANTag;

  AOBaseBundle = class;
  Arr1AOBaseBundle = array of AOBaseBundle;
  Arr2AOBaseBundle = array of Arr1AOBaseBundle;
  Arr3AOBaseBundle = array of Arr2AOBaseBundle;

  AOBundle = class;
  Arr1AOBundle = array of AOBundle;
  Arr2AOBundle = array of Arr1AOBundle;
  Arr3AOBundle = array of Arr2AOBundle;

  AODebug = class;
  Arr1AODebug = array of AODebug;
  Arr2AODebug = array of Arr1AODebug;
  Arr3AODebug = array of Arr2AODebug;

  AOMessage = class;
  Arr1AOMessage = array of AOMessage;
  Arr2AOMessage = array of Arr1AOMessage;
  Arr3AOMessage = array of Arr2AOMessage;

  AOMessenger = class;
  Arr1AOMessenger = array of AOMessenger;
  Arr2AOMessenger = array of Arr1AOMessenger;
  Arr3AOMessenger = array of Arr2AOMessenger;

  AOParcel = class;
  Arr1AOParcel = array of AOParcel;
  Arr2AOParcel = array of Arr1AOParcel;
  Arr3AOParcel = array of Arr2AOParcel;

  AOParcelUuid = class;
  Arr1AOParcelUuid = array of AOParcelUuid;
  Arr2AOParcelUuid = array of Arr1AOParcelUuid;
  Arr3AOParcelUuid = array of Arr2AOParcelUuid;

  AOPatternMatcher = class;
  Arr1AOPatternMatcher = array of AOPatternMatcher;
  Arr2AOPatternMatcher = array of Arr1AOPatternMatcher;
  Arr3AOPatternMatcher = array of Arr2AOPatternMatcher;

  AOResultReceiver = class;
  Arr1AOResultReceiver = array of AOResultReceiver;
  Arr2AOResultReceiver = array of Arr1AOResultReceiver;
  Arr3AOResultReceiver = array of Arr2AOResultReceiver;

  AOWorkSource = class;
  Arr1AOWorkSource = array of AOWorkSource;
  Arr2AOWorkSource = array of Arr1AOWorkSource;
  Arr3AOWorkSource = array of Arr2AOWorkSource;

  ATNeighboringCellInfo = class;
  Arr1ATNeighboringCellInfo = array of ATNeighboringCellInfo;
  Arr2ATNeighboringCellInfo = array of Arr1ATNeighboringCellInfo;
  Arr3ATNeighboringCellInfo = array of Arr2ATNeighboringCellInfo;

  ATServiceState = class;
  Arr1ATServiceState = array of ATServiceState;
  Arr2ATServiceState = array of Arr1ATServiceState;
  Arr3ATServiceState = array of Arr2ATServiceState;

  ATSignalStrength = class;
  Arr1ATSignalStrength = array of ATSignalStrength;
  Arr2ATSignalStrength = array of Arr1ATSignalStrength;
  Arr3ATSignalStrength = array of Arr2ATSignalStrength;

  ATParcelableSpan = interface;
  Arr1ATParcelableSpan = array of ATParcelableSpan;
  Arr2ATParcelableSpan = array of Arr1ATParcelableSpan;
  Arr3ATParcelableSpan = array of Arr2ATParcelableSpan;

  ATTextUtils = class;
  Arr1ATTextUtils = array of ATTextUtils;
  Arr2ATTextUtils = array of Arr1ATTextUtils;
  Arr3ATTextUtils = array of Arr2ATTextUtils;

  AVAbsSavedState = class;
  Arr1AVAbsSavedState = array of AVAbsSavedState;
  Arr2AVAbsSavedState = array of Arr1AVAbsSavedState;
  Arr3AVAbsSavedState = array of Arr2AVAbsSavedState;

  AVDragEvent = class;
  Arr1AVDragEvent = array of AVDragEvent;
  Arr2AVDragEvent = array of Arr1AVDragEvent;
  Arr3AVDragEvent = array of Arr2AVDragEvent;

  AVInputDevice = class;
  Arr1AVInputDevice = array of AVInputDevice;
  Arr2AVInputDevice = array of Arr1AVInputDevice;
  Arr3AVInputDevice = array of Arr2AVInputDevice;

  AVInputEvent = class;
  Arr1AVInputEvent = array of AVInputEvent;
  Arr2AVInputEvent = array of Arr1AVInputEvent;
  Arr3AVInputEvent = array of Arr2AVInputEvent;

  AVSurface = class;
  Arr1AVSurface = array of AVSurface;
  Arr2AVSurface = array of Arr1AVSurface;
  Arr3AVSurface = array of Arr2AVSurface;

  AVAAccessibilityNodeInfo = class;
  Arr1AVAAccessibilityNodeInfo = array of AVAAccessibilityNodeInfo;
  Arr2AVAAccessibilityNodeInfo = array of Arr1AVAAccessibilityNodeInfo;
  Arr3AVAAccessibilityNodeInfo = array of Arr2AVAAccessibilityNodeInfo;

  AVICompletionInfo = class;
  Arr1AVICompletionInfo = array of AVICompletionInfo;
  Arr2AVICompletionInfo = array of Arr1AVICompletionInfo;
  Arr3AVICompletionInfo = array of Arr2AVICompletionInfo;

  AVICorrectionInfo = class;
  Arr1AVICorrectionInfo = array of AVICorrectionInfo;
  Arr2AVICorrectionInfo = array of Arr1AVICorrectionInfo;
  Arr3AVICorrectionInfo = array of Arr2AVICorrectionInfo;

  AVIExtractedText = class;
  Arr1AVIExtractedText = array of AVIExtractedText;
  Arr2AVIExtractedText = array of Arr1AVIExtractedText;
  Arr3AVIExtractedText = array of Arr2AVIExtractedText;

  AVIExtractedTextRequest = class;
  Arr1AVIExtractedTextRequest = array of AVIExtractedTextRequest;
  Arr2AVIExtractedTextRequest = array of Arr1AVIExtractedTextRequest;
  Arr3AVIExtractedTextRequest = array of Arr2AVIExtractedTextRequest;

  AVIInputBinding = class;
  Arr1AVIInputBinding = array of AVIInputBinding;
  Arr2AVIInputBinding = array of Arr1AVIInputBinding;
  Arr3AVIInputBinding = array of Arr2AVIInputBinding;

  AVIInputMethodInfo = class;
  Arr1AVIInputMethodInfo = array of AVIInputMethodInfo;
  Arr2AVIInputMethodInfo = array of Arr1AVIInputMethodInfo;
  Arr3AVIInputMethodInfo = array of Arr2AVIInputMethodInfo;

  AVIInputMethodSubtype = class;
  Arr1AVIInputMethodSubtype = array of AVIInputMethodSubtype;
  Arr2AVIInputMethodSubtype = array of Arr1AVIInputMethodSubtype;
  Arr3AVIInputMethodSubtype = array of Arr2AVIInputMethodSubtype;

  AVTSpellCheckerInfo = class;
  Arr1AVTSpellCheckerInfo = array of AVTSpellCheckerInfo;
  Arr2AVTSpellCheckerInfo = array of Arr1AVTSpellCheckerInfo;
  Arr3AVTSpellCheckerInfo = array of Arr2AVTSpellCheckerInfo;

  AVTSpellCheckerSubtype = class;
  Arr1AVTSpellCheckerSubtype = array of AVTSpellCheckerSubtype;
  Arr2AVTSpellCheckerSubtype = array of Arr1AVTSpellCheckerSubtype;
  Arr3AVTSpellCheckerSubtype = array of Arr2AVTSpellCheckerSubtype;

  AVTSuggestionsInfo = class;
  Arr1AVTSuggestionsInfo = array of AVTSuggestionsInfo;
  Arr2AVTSuggestionsInfo = array of Arr1AVTSuggestionsInfo;
  Arr3AVTSuggestionsInfo = array of Arr2AVTSuggestionsInfo;

  AVTTextInfo = class;
  Arr1AVTTextInfo = array of AVTTextInfo;
  Arr2AVTTextInfo = array of Arr1AVTTextInfo;
  Arr3AVTTextInfo = array of Arr2AVTTextInfo;

  APCallLog = class;
  Arr1APCallLog = array of APCallLog;
  Arr2APCallLog = array of Arr1APCallLog;
  Arr3APCallLog = array of Arr2APCallLog;

  APLiveFolders = class;
  Arr1APLiveFolders = array of APLiveFolders;
  Arr2APLiveFolders = array of Arr1APLiveFolders;
  Arr3APLiveFolders = array of Arr2APLiveFolders;

  APSyncStateContract = class;
  Arr1APSyncStateContract = array of APSyncStateContract;
  Arr2APSyncStateContract = array of Arr1APSyncStateContract;
  Arr3APSyncStateContract = array of Arr2APSyncStateContract;

  APUserDictionary = class;
  Arr1APUserDictionary = array of APUserDictionary;
  Arr2APUserDictionary = array of Arr1APUserDictionary;
  Arr3APUserDictionary = array of Arr2APUserDictionary;

  APVoicemailContract = class;
  Arr1APVoicemailContract = array of APVoicemailContract;
  Arr2APVoicemailContract = array of Arr1APVoicemailContract;
  Arr3APVoicemailContract = array of Arr2APVoicemailContract;

  ARAllocation = class;
  Arr1ARAllocation = array of ARAllocation;
  Arr2ARAllocation = array of Arr1ARAllocation;
  Arr3ARAllocation = array of Arr2ARAllocation;

  ARElement = class;
  Arr1ARElement = array of ARElement;
  Arr2ARElement = array of Arr1ARElement;
  Arr3ARElement = array of Arr2ARElement;

  ARSampler = class;
  Arr1ARSampler = array of ARSampler;
  Arr2ARSampler = array of Arr1ARSampler;
  Arr3ARSampler = array of Arr2ARSampler;

  ARScript = class;
  Arr1ARScript = array of ARScript;
  Arr2ARScript = array of Arr1ARScript;
  Arr3ARScript = array of Arr2ARScript;

  ARType = class;
  Arr1ARType = array of ARType;
  Arr2ARType = array of Arr1ARType;
  Arr3ARType = array of Arr2ARType;

  ASRootElement = class;
  Arr1ASRootElement = array of ASRootElement;
  Arr2ASRootElement = array of Arr1ASRootElement;
  Arr3ASRootElement = array of Arr2ASRootElement;

  ASElementListener = interface;
  Arr1ASElementListener = array of ASElementListener;
  Arr2ASElementListener = array of Arr1ASElementListener;
  Arr3ASElementListener = array of Arr2ASElementListener;

  ASTextElementListener = interface;
  Arr1ASTextElementListener = array of ASTextElementListener;
  Arr2ASTextElementListener = array of Arr1ASTextElementListener;
  Arr3ASTextElementListener = array of Arr2ASTextElementListener;

  ATCCdmaCellLocation = class;
  Arr1ATCCdmaCellLocation = array of ATCCdmaCellLocation;
  Arr2ATCCdmaCellLocation = array of Arr1ATCCdmaCellLocation;
  Arr3ATCCdmaCellLocation = array of Arr2ATCCdmaCellLocation;

  ATGGsmCellLocation = class;
  Arr1ATGGsmCellLocation = array of ATGGsmCellLocation;
  Arr2ATGGsmCellLocation = array of Arr1ATGGsmCellLocation;
  Arr3ATGGsmCellLocation = array of Arr2ATGGsmCellLocation;

  ATInstrumentationTestRunner = class;
  Arr1ATInstrumentationTestRunner = array of ATInstrumentationTestRunner;
  Arr2ATInstrumentationTestRunner = array of Arr1ATInstrumentationTestRunner;
  Arr3ATInstrumentationTestRunner = array of Arr2ATInstrumentationTestRunner;

  ACClipboardManager = class;
  Arr1ACClipboardManager = array of ACClipboardManager;
  Arr2ACClipboardManager = array of Arr1ACClipboardManager;
  Arr3ACClipboardManager = array of Arr2ACClipboardManager;

  ATLoginFilter = class;
  Arr1ATLoginFilter = array of ATLoginFilter;
  Arr2ATLoginFilter = array of Arr1ATLoginFilter;
  Arr3ATLoginFilter = array of Arr2ATLoginFilter;

  AVIEditorInfo = class;
  Arr1AVIEditorInfo = array of AVIEditorInfo;
  Arr2AVIEditorInfo = array of Arr1AVIEditorInfo;
  Arr3AVIEditorInfo = array of Arr2AVIEditorInfo;

  ATSpanWatcher = interface;
  Arr1ATSpanWatcher = array of ATSpanWatcher;
  Arr2ATSpanWatcher = array of Arr1ATSpanWatcher;
  Arr3ATSpanWatcher = array of Arr2ATSpanWatcher;

  ATTextWatcher = interface;
  Arr1ATTextWatcher = array of ATTextWatcher;
  Arr2ATTextWatcher = array of Arr1ATTextWatcher;
  Arr3ATTextWatcher = array of Arr2ATTextWatcher;

  ATMBaseKeyListener = class;
  Arr1ATMBaseKeyListener = array of ATMBaseKeyListener;
  Arr2ATMBaseKeyListener = array of Arr1ATMBaseKeyListener;
  Arr3ATMBaseKeyListener = array of Arr2ATMBaseKeyListener;

  ATMBaseMovementMethod = class;
  Arr1ATMBaseMovementMethod = array of ATMBaseMovementMethod;
  Arr2ATMBaseMovementMethod = array of Arr1ATMBaseMovementMethod;
  Arr3ATMBaseMovementMethod = array of Arr2ATMBaseMovementMethod;

  ATMReplacementTransformationMethod = class;
  Arr1ATMReplacementTransformationMethod = array of ATMReplacementTransformationMethod;
  Arr2ATMReplacementTransformationMethod = array of Arr1ATMReplacementTransformationMethod;
  Arr3ATMReplacementTransformationMethod = array of Arr2ATMReplacementTransformationMethod;

  ATSLineBackgroundSpan = interface;
  Arr1ATSLineBackgroundSpan = array of ATSLineBackgroundSpan;
  Arr2ATSLineBackgroundSpan = array of Arr1ATSLineBackgroundSpan;
  Arr3ATSLineBackgroundSpan = array of Arr2ATSLineBackgroundSpan;

  ATSTabStopSpan = interface;
  Arr1ATSTabStopSpan = array of ATSTabStopSpan;
  Arr2ATSTabStopSpan = array of Arr1ATSTabStopSpan;
  Arr3ATSTabStopSpan = array of Arr2ATSTabStopSpan;

  ATSWrapTogetherSpan = interface;
  Arr1ATSWrapTogetherSpan = array of ATSWrapTogetherSpan;
  Arr2ATSWrapTogetherSpan = array of Arr1ATSWrapTogetherSpan;
  Arr3ATSWrapTogetherSpan = array of Arr2ATSWrapTogetherSpan;

  ATSClickableSpan = class;
  Arr1ATSClickableSpan = array of ATSClickableSpan;
  Arr2ATSClickableSpan = array of Arr1ATSClickableSpan;
  Arr3ATSClickableSpan = array of Arr2ATSClickableSpan;

  ATSMaskFilterSpan = class;
  Arr1ATSMaskFilterSpan = array of ATSMaskFilterSpan;
  Arr2ATSMaskFilterSpan = array of Arr1ATSMaskFilterSpan;
  Arr3ATSMaskFilterSpan = array of Arr2ATSMaskFilterSpan;

  ATSRasterizerSpan = class;
  Arr1ATSRasterizerSpan = array of ATSRasterizerSpan;
  Arr2ATSRasterizerSpan = array of Arr1ATSRasterizerSpan;
  Arr3ATSRasterizerSpan = array of Arr2ATSRasterizerSpan;

  ATSUpdateLayout = interface;
  Arr1ATSUpdateLayout = array of ATSUpdateLayout;
  Arr2ATSUpdateLayout = array of Arr1ATSUpdateLayout;
  Arr3ATSUpdateLayout = array of Arr2ATSUpdateLayout;

  AULogPrinter = class;
  Arr1AULogPrinter = array of AULogPrinter;
  Arr2AULogPrinter = array of Arr1AULogPrinter;
  Arr3AULogPrinter = array of Arr2AULogPrinter;

  AUPrintStreamPrinter = class;
  Arr1AUPrintStreamPrinter = array of AUPrintStreamPrinter;
  Arr2AUPrintStreamPrinter = array of Arr1AUPrintStreamPrinter;
  Arr3AUPrintStreamPrinter = array of Arr2AUPrintStreamPrinter;

  AUPrintWriterPrinter = class;
  Arr1AUPrintWriterPrinter = array of AUPrintWriterPrinter;
  Arr2AUPrintWriterPrinter = array of Arr1AUPrintWriterPrinter;
  Arr3AUPrintWriterPrinter = array of Arr2AUPrintWriterPrinter;

  AUStringBuilderPrinter = class;
  Arr1AUStringBuilderPrinter = array of AUStringBuilderPrinter;
  Arr2AUStringBuilderPrinter = array of Arr1AUStringBuilderPrinter;
  Arr3AUStringBuilderPrinter = array of Arr2AUStringBuilderPrinter;

  AVViewParent = interface;
  Arr1AVViewParent = array of AVViewParent;
  Arr2AVViewParent = array of Arr1AVViewParent;
  Arr3AVViewParent = array of Arr2AVViewParent;

  AWShareActionProvider = class;
  Arr1AWShareActionProvider = array of AWShareActionProvider;
  Arr2AWShareActionProvider = array of Arr1AWShareActionProvider;
  Arr3AWShareActionProvider = array of Arr2AWShareActionProvider;

  AVContextMenu = interface;
  Arr1AVContextMenu = array of AVContextMenu;
  Arr2AVContextMenu = array of Arr1AVContextMenu;
  Arr3AVContextMenu = array of Arr2AVContextMenu;

  AVSubMenu = interface;
  Arr1AVSubMenu = array of AVSubMenu;
  Arr2AVSubMenu = array of Arr1AVSubMenu;
  Arr3AVSubMenu = array of Arr2AVSubMenu;

  AVViewGroup_LayoutParams = class;
  Arr1AVViewGroup_LayoutParams = array of AVViewGroup_LayoutParams;
  Arr2AVViewGroup_LayoutParams = array of Arr1AVViewGroup_LayoutParams;
  Arr3AVViewGroup_LayoutParams = array of Arr2AVViewGroup_LayoutParams;

  AVWindowManager = interface;
  Arr1AVWindowManager = array of AVWindowManager;
  Arr2AVWindowManager = array of Arr1AVWindowManager;
  Arr3AVWindowManager = array of Arr2AVWindowManager;

  AVAAccessibilityEvent = class;
  Arr1AVAAccessibilityEvent = array of AVAAccessibilityEvent;
  Arr2AVAAccessibilityEvent = array of Arr1AVAAccessibilityEvent;
  Arr3AVAAccessibilityEvent = array of Arr2AVAAccessibilityEvent;

  AVAGridLayoutAnimationController = class;
  Arr1AVAGridLayoutAnimationController = array of AVAGridLayoutAnimationController;
  Arr2AVAGridLayoutAnimationController = array of Arr1AVAGridLayoutAnimationController;
  Arr3AVAGridLayoutAnimationController = array of Arr2AVAGridLayoutAnimationController;

  AVIBaseInputConnection = class;
  Arr1AVIBaseInputConnection = array of AVIBaseInputConnection;
  Arr2AVIBaseInputConnection = array of Arr1AVIBaseInputConnection;
  Arr3AVIBaseInputConnection = array of Arr2AVIBaseInputConnection;

  AVIInputConnectionWrapper = class;
  Arr1AVIInputConnectionWrapper = array of AVIInputConnectionWrapper;
  Arr2AVIInputConnectionWrapper = array of Arr1AVIInputConnectionWrapper;
  Arr3AVIInputConnectionWrapper = array of Arr2AVIInputConnectionWrapper;

  AVTTextServicesManager = class;
  Arr1AVTTextServicesManager = array of AVTTextServicesManager;
  Arr2AVTTextServicesManager = array of Arr1AVTTextServicesManager;
  Arr3AVTTextServicesManager = array of Arr2AVTTextServicesManager;

  AWJsPromptResult = class;
  Arr1AWJsPromptResult = array of AWJsPromptResult;
  Arr2AWJsPromptResult = array of Arr1AWJsPromptResult;
  Arr3AWJsPromptResult = array of Arr2AWJsPromptResult;

  APBrowser = class;
  Arr1APBrowser = array of APBrowser;
  Arr2APBrowser = array of Arr1APBrowser;
  Arr3APBrowser = array of Arr2APBrowser;

  AWWebChromeClient = class;
  Arr1AWWebChromeClient = array of AWWebChromeClient;
  Arr2AWWebChromeClient = array of Arr1AWWebChromeClient;
  Arr3AWWebChromeClient = array of Arr2AWWebChromeClient;

  AWListAdapter = interface;
  Arr1AWListAdapter = array of AWListAdapter;
  Arr2AWListAdapter = array of Arr1AWListAdapter;
  Arr3AWListAdapter = array of Arr2AWListAdapter;

  AWSpinnerAdapter = interface;
  Arr1AWSpinnerAdapter = array of AWSpinnerAdapter;
  Arr2AWSpinnerAdapter = array of Arr1AWSpinnerAdapter;
  Arr3AWSpinnerAdapter = array of Arr2AWSpinnerAdapter;

  AWBaseExpandableListAdapter = class;
  Arr1AWBaseExpandableListAdapter = array of AWBaseExpandableListAdapter;
  Arr2AWBaseExpandableListAdapter = array of Arr1AWBaseExpandableListAdapter;
  Arr3AWBaseExpandableListAdapter = array of Arr2AWBaseExpandableListAdapter;

  AWAlphabetIndexer = class;
  Arr1AWAlphabetIndexer = array of AWAlphabetIndexer;
  Arr2AWAlphabetIndexer = array of Arr1AWAlphabetIndexer;
  Arr3AWAlphabetIndexer = array of Arr2AWAlphabetIndexer;

  ABBluetoothServerSocket = class;
  Arr1ABBluetoothServerSocket = array of ABBluetoothServerSocket;
  Arr2ABBluetoothServerSocket = array of Arr1ABBluetoothServerSocket;
  Arr3ABBluetoothServerSocket = array of Arr2ABBluetoothServerSocket;

  ABBluetoothSocket = class;
  Arr1ABBluetoothSocket = array of ABBluetoothSocket;
  Arr2ABBluetoothSocket = array of Arr1ABBluetoothSocket;
  Arr3ABBluetoothSocket = array of Arr2ABBluetoothSocket;

  ANTTagTechnology = interface;
  Arr1ANTTagTechnology = array of ANTTagTechnology;
  Arr2ANTTagTechnology = array of Arr1ANTTagTechnology;
  Arr3ANTTagTechnology = array of Arr2ANTTagTechnology;

  AODropBoxManager = class;
  Arr1AODropBoxManager = array of AODropBoxManager;
  Arr2AODropBoxManager = array of Arr1AODropBoxManager;
  Arr3AODropBoxManager = array of Arr2AODropBoxManager;

  AUJsonReader = class;
  Arr1AUJsonReader = array of AUJsonReader;
  Arr2AUJsonReader = array of Arr1AUJsonReader;
  Arr3AUJsonReader = array of Arr2AUJsonReader;

  AUJsonWriter = class;
  Arr1AUJsonWriter = array of AUJsonWriter;
  Arr2AUJsonWriter = array of Arr1AUJsonWriter;
  Arr3AUJsonWriter = array of Arr2AUJsonWriter;

  JIInputStream = class;
  Arr1JIInputStream = array of JIInputStream;
  Arr2JIInputStream = array of Arr1JIInputStream;
  Arr3JIInputStream = array of Arr2JIInputStream;

  JIReader = class;
  Arr1JIReader = array of JIReader;
  Arr2JIReader = array of Arr1JIReader;
  Arr3JIReader = array of Arr2JIReader;

  JNCChannel = interface;
  Arr1JNCChannel = array of JNCChannel;
  Arr2JNCChannel = array of Arr1JNCChannel;
  Arr3JNCChannel = array of Arr2JNCChannel;

  JIObjectInput = interface;
  Arr1JIObjectInput = array of JIObjectInput;
  Arr2JIObjectInput = array of Arr1JIObjectInput;
  Arr3JIObjectInput = array of Arr2JIObjectInput;

  JIObjectOutput = interface;
  Arr1JIObjectOutput = array of JIObjectOutput;
  Arr2JIObjectOutput = array of Arr1JIObjectOutput;
  Arr3JIObjectOutput = array of Arr2JIObjectOutput;

  JIRandomAccessFile = class;
  Arr1JIRandomAccessFile = array of JIRandomAccessFile;
  Arr2JIRandomAccessFile = array of Arr1JIRandomAccessFile;
  Arr3JIRandomAccessFile = array of Arr2JIRandomAccessFile;

  JIConsole = class;
  Arr1JIConsole = array of JIConsole;
  Arr2JIConsole = array of Arr1JIConsole;
  Arr3JIConsole = array of Arr2JIConsole;

  JIOutputStream = class;
  Arr1JIOutputStream = array of JIOutputStream;
  Arr2JIOutputStream = array of Arr1JIOutputStream;
  Arr3JIOutputStream = array of Arr2JIOutputStream;

  JIWriter = class;
  Arr1JIWriter = array of JIWriter;
  Arr2JIWriter = array of Arr1JIWriter;
  Arr3JIWriter = array of Arr2JIWriter;

  JUFormatter = class;
  Arr1JUFormatter = array of JUFormatter;
  Arr2JUFormatter = array of Arr1JUFormatter;
  Arr3JUFormatter = array of Arr2JUFormatter;

  JSSecureClassLoader = class;
  Arr1JSSecureClassLoader = array of JSSecureClassLoader;
  Arr2JSSecureClassLoader = array of Arr1JSSecureClassLoader;
  Arr3JSSecureClassLoader = array of Arr2JSSecureClassLoader;

  ARRenderScript = class;
  Arr1ARRenderScript = array of ARRenderScript;
  Arr2ARRenderScript = array of Arr1ARRenderScript;
  Arr3ARRenderScript = array of Arr2ARRenderScript;

  AWWebSyncManager = class;
  Arr1AWWebSyncManager = array of AWWebSyncManager;
  Arr2AWWebSyncManager = array of Arr1AWWebSyncManager;
  Arr3AWWebSyncManager = array of Arr2AWWebSyncManager;

  JLThread = class;
  Arr1JLThread = array of JLThread;
  Arr2JLThread = array of Arr1JLThread;
  Arr3JLThread = array of Arr2JLThread;

  JUTimerTask = class;
  Arr1JUTimerTask = array of JUTimerTask;
  Arr2JUTimerTask = array of Arr1JUTimerTask;
  Arr3JUTimerTask = array of Arr2JUTimerTask;

  ATFlakyTest = interface;
  Arr1ATFlakyTest = array of ATFlakyTest;
  Arr2ATFlakyTest = array of Arr1ATFlakyTest;
  Arr3ATFlakyTest = array of Arr2ATFlakyTest;

  ATUiThreadTest = interface;
  Arr1ATUiThreadTest = array of ATUiThreadTest;
  Arr2ATUiThreadTest = array of Arr1ATUiThreadTest;
  Arr3ATUiThreadTest = array of Arr2ATUiThreadTest;

  ATSALargeTest = interface;
  Arr1ATSALargeTest = array of ATSALargeTest;
  Arr2ATSALargeTest = array of Arr1ATSALargeTest;
  Arr3ATSALargeTest = array of Arr2ATSALargeTest;

  ATSAMediumTest = interface;
  Arr1ATSAMediumTest = array of ATSAMediumTest;
  Arr2ATSAMediumTest = array of Arr1ATSAMediumTest;
  Arr3ATSAMediumTest = array of Arr2ATSAMediumTest;

  ATSASmallTest = interface;
  Arr1ATSASmallTest = array of ATSASmallTest;
  Arr2ATSASmallTest = array of Arr1ATSASmallTest;
  Arr3ATSASmallTest = array of Arr2ATSASmallTest;

  ATSASmoke = interface;
  Arr1ATSASmoke = array of ATSASmoke;
  Arr2ATSASmoke = array of Arr1ATSASmoke;
  Arr3ATSASmoke = array of Arr2ATSASmoke;

  ATSASuppress = interface;
  Arr1ATSASuppress = array of ATSASuppress;
  Arr2ATSASuppress = array of Arr1ATSASuppress;
  Arr3ATSASuppress = array of Arr2ATSASuppress;

  AVViewDebug = class;
  Arr1AVViewDebug = array of AVViewDebug;
  Arr2AVViewDebug = array of Arr1AVViewDebug;
  Arr3AVViewDebug = array of Arr2AVViewDebug;

  AWRemoteViews = class;
  Arr1AWRemoteViews = array of AWRemoteViews;
  Arr2AWRemoteViews = array of Arr1AWRemoteViews;
  Arr3AWRemoteViews = array of Arr2AWRemoteViews;

  JLDeprecated = interface;
  Arr1JLDeprecated = array of JLDeprecated;
  Arr2JLDeprecated = array of Arr1JLDeprecated;
  Arr3JLDeprecated = array of Arr2JLDeprecated;

  JLOverride = interface;
  Arr1JLOverride = array of JLOverride;
  Arr2JLOverride = array of Arr1JLOverride;
  Arr3JLOverride = array of Arr2JLOverride;

  JLSuppressWarnings = interface;
  Arr1JLSuppressWarnings = array of JLSuppressWarnings;
  Arr2JLSuppressWarnings = array of Arr1JLSuppressWarnings;
  Arr3JLSuppressWarnings = array of Arr2JLSuppressWarnings;

  JLADocumented = interface;
  Arr1JLADocumented = array of JLADocumented;
  Arr2JLADocumented = array of Arr1JLADocumented;
  Arr3JLADocumented = array of Arr2JLADocumented;

  JLAInherited = interface;
  Arr1JLAInherited = array of JLAInherited;
  Arr2JLAInherited = array of Arr1JLAInherited;
  Arr3JLAInherited = array of Arr2JLAInherited;

  JLARetention = interface;
  Arr1JLARetention = array of JLARetention;
  Arr2JLARetention = array of Arr1JLARetention;
  Arr3JLARetention = array of Arr2JLARetention;

  JLATarget = interface;
  Arr1JLATarget = array of JLATarget;
  Arr2JLATarget = array of Arr1JLATarget;
  Arr3JLATarget = array of Arr2JLATarget;

  JLRPhantomReference = class;
  Arr1JLRPhantomReference = array of JLRPhantomReference;
  Arr2JLRPhantomReference = array of Arr1JLRPhantomReference;
  Arr3JLRPhantomReference = array of Arr2JLRPhantomReference;

  JLRSoftReference = class;
  Arr1JLRSoftReference = array of JLRSoftReference;
  Arr2JLRSoftReference = array of Arr1JLRSoftReference;
  Arr3JLRSoftReference = array of Arr2JLRSoftReference;

  JLRWeakReference = class;
  Arr1JLRWeakReference = array of JLRWeakReference;
  Arr2JLRWeakReference = array of Arr1JLRWeakReference;
  Arr3JLRWeakReference = array of Arr2JLRWeakReference;

  JNSecureCacheResponse = class;
  Arr1JNSecureCacheResponse = array of JNSecureCacheResponse;
  Arr2JNSecureCacheResponse = array of Arr1JNSecureCacheResponse;
  Arr3JNSecureCacheResponse = array of Arr2JNSecureCacheResponse;

  JNCookieManager = class;
  Arr1JNCookieManager = array of JNCookieManager;
  Arr2JNCookieManager = array of Arr1JNCookieManager;
  Arr3JNCookieManager = array of Arr2JNCookieManager;

  JNMulticastSocket = class;
  Arr1JNMulticastSocket = array of JNMulticastSocket;
  Arr2JNMulticastSocket = array of Arr1JNMulticastSocket;
  Arr3JNMulticastSocket = array of Arr2JNMulticastSocket;

  ANHHttpResponseCache = class;
  Arr1ANHHttpResponseCache = array of ANHHttpResponseCache;
  Arr2ANHHttpResponseCache = array of Arr1ANHHttpResponseCache;
  Arr3ANHHttpResponseCache = array of Arr2ANHHttpResponseCache;

  JNSSSLServerSocket = class;
  Arr1JNSSSLServerSocket = array of JNSSSLServerSocket;
  Arr2JNSSSLServerSocket = array of Arr1JNSSSLServerSocket;
  Arr3JNSSSLServerSocket = array of Arr2JNSSSLServerSocket;

  JNSSSLSocket = class;
  Arr1JNSSSLSocket = array of JNSSSLSocket;
  Arr2JNSSSLSocket = array of Arr1JNSSSLSocket;
  Arr3JNSSSLSocket = array of Arr2JNSSSLSocket;

  JNDatagramSocketImpl = class;
  Arr1JNDatagramSocketImpl = array of JNDatagramSocketImpl;
  Arr2JNDatagramSocketImpl = array of Arr1JNDatagramSocketImpl;
  Arr3JNDatagramSocketImpl = array of Arr2JNDatagramSocketImpl;

  JNSocketImpl = class;
  Arr1JNSocketImpl = array of JNSocketImpl;
  Arr2JNSocketImpl = array of Arr1JNSocketImpl;
  Arr3JNSocketImpl = array of Arr2JNSocketImpl;

  JNHttpURLConnection = class;
  Arr1JNHttpURLConnection = array of JNHttpURLConnection;
  Arr2JNHttpURLConnection = array of Arr1JNHttpURLConnection;
  Arr3JNHttpURLConnection = array of Arr2JNHttpURLConnection;

  JNJarURLConnection = class;
  Arr1JNJarURLConnection = array of JNJarURLConnection;
  Arr2JNJarURLConnection = array of Arr1JNJarURLConnection;
  Arr3JNJarURLConnection = array of Arr2JNJarURLConnection;

  JNCSAbstractSelectionKey = class;
  Arr1JNCSAbstractSelectionKey = array of JNCSAbstractSelectionKey;
  Arr2JNCSAbstractSelectionKey = array of Arr1JNCSAbstractSelectionKey;
  Arr3JNCSAbstractSelectionKey = array of Arr2JNCSAbstractSelectionKey;

  JNCSAbstractSelector = class;
  Arr1JNCSAbstractSelector = array of JNCSAbstractSelector;
  Arr2JNCSAbstractSelector = array of Arr1JNCSAbstractSelector;
  Arr3JNCSAbstractSelector = array of Arr2JNCSAbstractSelector;

  JSASubjectDomainCombiner = class;
  Arr1JSASubjectDomainCombiner = array of JSASubjectDomainCombiner;
  Arr2JSASubjectDomainCombiner = array of Arr1JSASubjectDomainCombiner;
  Arr3JSASubjectDomainCombiner = array of Arr2JSASubjectDomainCombiner;

  JSPermission = class;
  Arr1JSPermission = array of JSPermission;
  Arr2JSPermission = array of Arr1JSPermission;
  Arr3JSPermission = array of Arr2JSPermission;

  JSKeyPairGenerator = class;
  Arr1JSKeyPairGenerator = array of JSKeyPairGenerator;
  Arr2JSKeyPairGenerator = array of Arr1JSKeyPairGenerator;
  Arr3JSKeyPairGenerator = array of Arr2JSKeyPairGenerator;

  JSMessageDigest = class;
  Arr1JSMessageDigest = array of JSMessageDigest;
  Arr2JSMessageDigest = array of Arr1JSMessageDigest;
  Arr3JSMessageDigest = array of Arr2JSMessageDigest;

  JSIdentity = class;
  Arr1JSIdentity = array of JSIdentity;
  Arr2JSIdentity = array of Arr1JSIdentity;
  Arr3JSIdentity = array of Arr2JSIdentity;

  JSAGroup = interface;
  Arr1JSAGroup = array of JSAGroup;
  Arr2JSAGroup = array of Arr1JSAGroup;
  Arr3JSAGroup = array of Arr2JSAGroup;

  JSAXX500Principal = class;
  Arr1JSAXX500Principal = array of JSAXX500Principal;
  Arr2JSAXX500Principal = array of Arr1JSAXX500Principal;
  Arr3JSAXX500Principal = array of Arr2JSAXX500Principal;

  JSSignature = class;
  Arr1JSSignature = array of JSSignature;
  Arr2JSSignature = array of Arr1JSSignature;
  Arr3JSSignature = array of Arr2JSSignature;

  JSAAcl = interface;
  Arr1JSAAcl = array of JSAAcl;
  Arr2JSAAcl = array of Arr1JSAAcl;
  Arr3JSAAcl = array of Arr2JSAAcl;

  JSCX509CRL = class;
  Arr1JSCX509CRL = array of JSCX509CRL;
  Arr2JSCX509CRL = array of Arr1JSCX509CRL;
  Arr3JSCX509CRL = array of Arr2JSCX509CRL;

  JSCX509CRLEntry = class;
  Arr1JSCX509CRLEntry = array of JSCX509CRLEntry;
  Arr2JSCX509CRLEntry = array of Arr1JSCX509CRLEntry;
  Arr3JSCX509CRLEntry = array of Arr2JSCX509CRLEntry;

  JSSDSAParameterSpec = class;
  Arr1JSSDSAParameterSpec = array of JSSDSAParameterSpec;
  Arr2JSSDSAParameterSpec = array of Arr1JSSDSAParameterSpec;
  Arr3JSSDSAParameterSpec = array of Arr2JSSDSAParameterSpec;

  JSSECGenParameterSpec = class;
  Arr1JSSECGenParameterSpec = array of JSSECGenParameterSpec;
  Arr2JSSECGenParameterSpec = array of Arr1JSSECGenParameterSpec;
  Arr3JSSECGenParameterSpec = array of Arr2JSSECGenParameterSpec;

  JSSECParameterSpec = class;
  Arr1JSSECParameterSpec = array of JSSECParameterSpec;
  Arr2JSSECParameterSpec = array of Arr1JSSECParameterSpec;
  Arr3JSSECParameterSpec = array of Arr2JSSECParameterSpec;

  JSSMGF1ParameterSpec = class;
  Arr1JSSMGF1ParameterSpec = array of JSSMGF1ParameterSpec;
  Arr2JSSMGF1ParameterSpec = array of Arr1JSSMGF1ParameterSpec;
  Arr3JSSMGF1ParameterSpec = array of Arr2JSSMGF1ParameterSpec;

  JSSPSSParameterSpec = class;
  Arr1JSSPSSParameterSpec = array of JSSPSSParameterSpec;
  Arr2JSSPSSParameterSpec = array of Arr1JSSPSSParameterSpec;
  Arr3JSSPSSParameterSpec = array of Arr2JSSPSSParameterSpec;

  JSSRSAKeyGenParameterSpec = class;
  Arr1JSSRSAKeyGenParameterSpec = array of JSSRSAKeyGenParameterSpec;
  Arr2JSSRSAKeyGenParameterSpec = array of Arr1JSSRSAKeyGenParameterSpec;
  Arr3JSSRSAKeyGenParameterSpec = array of Arr2JSSRSAKeyGenParameterSpec;

  JCSDHGenParameterSpec = class;
  Arr1JCSDHGenParameterSpec = array of JCSDHGenParameterSpec;
  Arr2JCSDHGenParameterSpec = array of Arr1JCSDHGenParameterSpec;
  Arr3JCSDHGenParameterSpec = array of Arr2JCSDHGenParameterSpec;

  JCSDHParameterSpec = class;
  Arr1JCSDHParameterSpec = array of JCSDHParameterSpec;
  Arr2JCSDHParameterSpec = array of Arr1JCSDHParameterSpec;
  Arr3JCSDHParameterSpec = array of Arr2JCSDHParameterSpec;

  JCSIvParameterSpec = class;
  Arr1JCSIvParameterSpec = array of JCSIvParameterSpec;
  Arr2JCSIvParameterSpec = array of Arr1JCSIvParameterSpec;
  Arr3JCSIvParameterSpec = array of Arr2JCSIvParameterSpec;

  JCSOAEPParameterSpec = class;
  Arr1JCSOAEPParameterSpec = array of JCSOAEPParameterSpec;
  Arr2JCSOAEPParameterSpec = array of Arr1JCSOAEPParameterSpec;
  Arr3JCSOAEPParameterSpec = array of Arr2JCSOAEPParameterSpec;

  JCSPBEParameterSpec = class;
  Arr1JCSPBEParameterSpec = array of JCSPBEParameterSpec;
  Arr2JCSPBEParameterSpec = array of Arr1JCSPBEParameterSpec;
  Arr3JCSPBEParameterSpec = array of Arr2JCSPBEParameterSpec;

  JCSRC2ParameterSpec = class;
  Arr1JCSRC2ParameterSpec = array of JCSRC2ParameterSpec;
  Arr2JCSRC2ParameterSpec = array of Arr1JCSRC2ParameterSpec;
  Arr3JCSRC2ParameterSpec = array of Arr2JCSRC2ParameterSpec;

  JCSRC5ParameterSpec = class;
  Arr1JCSRC5ParameterSpec = array of JCSRC5ParameterSpec;
  Arr2JCSRC5ParameterSpec = array of Arr1JCSRC5ParameterSpec;
  Arr3JCSRC5ParameterSpec = array of Arr2JCSRC5ParameterSpec;

  JSSECFieldF2m = class;
  Arr1JSSECFieldF2m = array of JSSECFieldF2m;
  Arr2JSSECFieldF2m = array of Arr1JSSECFieldF2m;
  Arr3JSSECFieldF2m = array of Arr2JSSECFieldF2m;

  JSSECFieldFp = class;
  Arr1JSSECFieldFp = array of JSSECFieldFp;
  Arr2JSSECFieldFp = array of Arr1JSSECFieldFp;
  Arr3JSSECFieldFp = array of Arr2JSSECFieldFp;

  JSSDSAPrivateKeySpec = class;
  Arr1JSSDSAPrivateKeySpec = array of JSSDSAPrivateKeySpec;
  Arr2JSSDSAPrivateKeySpec = array of Arr1JSSDSAPrivateKeySpec;
  Arr3JSSDSAPrivateKeySpec = array of Arr2JSSDSAPrivateKeySpec;

  JSSDSAPublicKeySpec = class;
  Arr1JSSDSAPublicKeySpec = array of JSSDSAPublicKeySpec;
  Arr2JSSDSAPublicKeySpec = array of Arr1JSSDSAPublicKeySpec;
  Arr3JSSDSAPublicKeySpec = array of Arr2JSSDSAPublicKeySpec;

  JSSECPrivateKeySpec = class;
  Arr1JSSECPrivateKeySpec = array of JSSECPrivateKeySpec;
  Arr2JSSECPrivateKeySpec = array of Arr1JSSECPrivateKeySpec;
  Arr3JSSECPrivateKeySpec = array of Arr2JSSECPrivateKeySpec;

  JSSECPublicKeySpec = class;
  Arr1JSSECPublicKeySpec = array of JSSECPublicKeySpec;
  Arr2JSSECPublicKeySpec = array of Arr1JSSECPublicKeySpec;
  Arr3JSSECPublicKeySpec = array of Arr2JSSECPublicKeySpec;

  JSSEncodedKeySpec = class;
  Arr1JSSEncodedKeySpec = array of JSSEncodedKeySpec;
  Arr2JSSEncodedKeySpec = array of Arr1JSSEncodedKeySpec;
  Arr3JSSEncodedKeySpec = array of Arr2JSSEncodedKeySpec;

  JSSRSAPrivateKeySpec = class;
  Arr1JSSRSAPrivateKeySpec = array of JSSRSAPrivateKeySpec;
  Arr2JSSRSAPrivateKeySpec = array of Arr1JSSRSAPrivateKeySpec;
  Arr3JSSRSAPrivateKeySpec = array of Arr2JSSRSAPrivateKeySpec;

  JSSRSAPublicKeySpec = class;
  Arr1JSSRSAPublicKeySpec = array of JSSRSAPublicKeySpec;
  Arr2JSSRSAPublicKeySpec = array of Arr1JSSRSAPublicKeySpec;
  Arr3JSSRSAPublicKeySpec = array of Arr2JSSRSAPublicKeySpec;

  JCSDESKeySpec = class;
  Arr1JCSDESKeySpec = array of JCSDESKeySpec;
  Arr2JCSDESKeySpec = array of Arr1JCSDESKeySpec;
  Arr3JCSDESKeySpec = array of Arr2JCSDESKeySpec;

  JCSDESedeKeySpec = class;
  Arr1JCSDESedeKeySpec = array of JCSDESedeKeySpec;
  Arr2JCSDESedeKeySpec = array of Arr1JCSDESedeKeySpec;
  Arr3JCSDESedeKeySpec = array of Arr2JCSDESedeKeySpec;

  JCSDHPrivateKeySpec = class;
  Arr1JCSDHPrivateKeySpec = array of JCSDHPrivateKeySpec;
  Arr2JCSDHPrivateKeySpec = array of Arr1JCSDHPrivateKeySpec;
  Arr3JCSDHPrivateKeySpec = array of Arr2JCSDHPrivateKeySpec;

  JCSDHPublicKeySpec = class;
  Arr1JCSDHPublicKeySpec = array of JCSDHPublicKeySpec;
  Arr2JCSDHPublicKeySpec = array of Arr1JCSDHPublicKeySpec;
  Arr3JCSDHPublicKeySpec = array of Arr2JCSDHPublicKeySpec;

  JCSPBEKeySpec = class;
  Arr1JCSPBEKeySpec = array of JCSPBEKeySpec;
  Arr2JCSPBEKeySpec = array of Arr1JCSPBEKeySpec;
  Arr3JCSPBEKeySpec = array of Arr2JCSPBEKeySpec;

  JSNClob = interface;
  Arr1JSNClob = array of JSNClob;
  Arr2JSNClob = array of Arr1JSNClob;
  Arr3JSNClob = array of Arr2JSNClob;

  JSConnection = interface;
  Arr1JSConnection = array of JSConnection;
  Arr2JSConnection = array of Arr1JSConnection;
  Arr3JSConnection = array of Arr2JSConnection;

  JSDatabaseMetaData = interface;
  Arr1JSDatabaseMetaData = array of JSDatabaseMetaData;
  Arr2JSDatabaseMetaData = array of Arr1JSDatabaseMetaData;
  Arr3JSDatabaseMetaData = array of Arr2JSDatabaseMetaData;

  JSParameterMetaData = interface;
  Arr1JSParameterMetaData = array of JSParameterMetaData;
  Arr2JSParameterMetaData = array of Arr1JSParameterMetaData;
  Arr3JSParameterMetaData = array of Arr2JSParameterMetaData;

  JSResultSet = interface;
  Arr1JSResultSet = array of JSResultSet;
  Arr2JSResultSet = array of Arr1JSResultSet;
  Arr3JSResultSet = array of Arr2JSResultSet;

  JSResultSetMetaData = interface;
  Arr1JSResultSetMetaData = array of JSResultSetMetaData;
  Arr2JSResultSetMetaData = array of Arr1JSResultSetMetaData;
  Arr3JSResultSetMetaData = array of Arr2JSResultSetMetaData;

  JSStatement = interface;
  Arr1JSStatement = array of JSStatement;
  Arr2JSStatement = array of Arr1JSStatement;
  Arr3JSStatement = array of Arr2JSStatement;

  JUHashtable = class;
  Arr1JUHashtable = array of JUHashtable;
  Arr2JUHashtable = array of Arr1JUHashtable;
  Arr3JUHashtable = array of Arr2JUHashtable;

  JUStringTokenizer = class;
  Arr1JUStringTokenizer = array of JUStringTokenizer;
  Arr2JUStringTokenizer = array of Arr1JUStringTokenizer;
  Arr3JUStringTokenizer = array of Arr2JUStringTokenizer;

  JBPropertyChangeListener = interface;
  Arr1JBPropertyChangeListener = array of JBPropertyChangeListener;
  Arr2JBPropertyChangeListener = array of Arr1JBPropertyChangeListener;
  Arr3JBPropertyChangeListener = array of Arr2JBPropertyChangeListener;

  JUEventListenerProxy = class;
  Arr1JUEventListenerProxy = array of JUEventListenerProxy;
  Arr2JUEventListenerProxy = array of Arr1JUEventListenerProxy;
  Arr3JUEventListenerProxy = array of Arr2JUEventListenerProxy;

  JUPNodeChangeListener = interface;
  Arr1JUPNodeChangeListener = array of JUPNodeChangeListener;
  Arr2JUPNodeChangeListener = array of Arr1JUPNodeChangeListener;
  Arr3JUPNodeChangeListener = array of Arr2JUPNodeChangeListener;

  JUPPreferenceChangeListener = interface;
  Arr1JUPPreferenceChangeListener = array of JUPPreferenceChangeListener;
  Arr2JUPPreferenceChangeListener = array of Arr1JUPPreferenceChangeListener;
  Arr3JUPPreferenceChangeListener = array of Arr2JUPPreferenceChangeListener;

  JNSHandshakeCompletedListener = interface;
  Arr1JNSHandshakeCompletedListener = array of JNSHandshakeCompletedListener;
  Arr2JNSHandshakeCompletedListener = array of Arr1JNSHandshakeCompletedListener;
  Arr3JNSHandshakeCompletedListener = array of Arr2JNSHandshakeCompletedListener;

  JNSSSLSessionBindingListener = interface;
  Arr1JNSSSLSessionBindingListener = array of JNSSSLSessionBindingListener;
  Arr2JNSSSLSessionBindingListener = array of Arr1JNSSSLSessionBindingListener;
  Arr3JNSSSLSessionBindingListener = array of Arr2JNSSSLSessionBindingListener;

  JSConnectionEventListener = interface;
  Arr1JSConnectionEventListener = array of JSConnectionEventListener;
  Arr2JSConnectionEventListener = array of Arr1JSConnectionEventListener;
  Arr3JSConnectionEventListener = array of Arr2JSConnectionEventListener;

  JSRowSetListener = interface;
  Arr1JSRowSetListener = array of JSRowSetListener;
  Arr2JSRowSetListener = array of Arr1JSRowSetListener;
  Arr3JSRowSetListener = array of Arr2JSRowSetListener;

  JSStatementEventListener = interface;
  Arr1JSStatementEventListener = array of JSStatementEventListener;
  Arr2JSStatementEventListener = array of Arr1JSStatementEventListener;
  Arr3JSStatementEventListener = array of Arr2JSStatementEventListener;

  ACContentQueryMap = class;
  Arr1ACContentQueryMap = array of ACContentQueryMap;
  Arr2ACContentQueryMap = array of Arr1ACContentQueryMap;
  Arr3ACContentQueryMap = array of Arr2ACContentQueryMap;

  JUListResourceBundle = class;
  Arr1JUListResourceBundle = array of JUListResourceBundle;
  Arr2JUListResourceBundle = array of Arr1JUListResourceBundle;
  Arr3JUListResourceBundle = array of Arr2JUListResourceBundle;

  JUPropertyResourceBundle = class;
  Arr1JUPropertyResourceBundle = array of JUPropertyResourceBundle;
  Arr2JUPropertyResourceBundle = array of Arr1JUPropertyResourceBundle;
  Arr3JUPropertyResourceBundle = array of Arr2JUPropertyResourceBundle;

  JUCExecutorCompletionService = class;
  Arr1JUCExecutorCompletionService = array of JUCExecutorCompletionService;
  Arr2JUCExecutorCompletionService = array of Arr1JUCExecutorCompletionService;
  Arr3JUCExecutorCompletionService = array of Arr2JUCExecutorCompletionService;

  JUCExecutorService = interface;
  Arr1JUCExecutorService = array of JUCExecutorService;
  Arr2JUCExecutorService = array of Arr1JUCExecutorService;
  Arr3JUCExecutorService = array of Arr2JUCExecutorService;

  JUCRunnableFuture = interface;
  Arr1JUCRunnableFuture = array of JUCRunnableFuture;
  Arr2JUCRunnableFuture = array of Arr1JUCRunnableFuture;
  Arr3JUCRunnableFuture = array of Arr2JUCRunnableFuture;

  JUCLReentrantLock = class;
  Arr1JUCLReentrantLock = array of JUCLReentrantLock;
  Arr2JUCLReentrantLock = array of Arr1JUCLReentrantLock;
  Arr3JUCLReentrantLock = array of Arr2JUCLReentrantLock;

  JUCLReentrantReadWriteLock = class;
  Arr1JUCLReentrantReadWriteLock = array of JUCLReentrantReadWriteLock;
  Arr2JUCLReentrantReadWriteLock = array of Arr1JUCLReentrantReadWriteLock;
  Arr3JUCLReentrantReadWriteLock = array of Arr2JUCLReentrantReadWriteLock;

  JULSimpleFormatter = class;
  Arr1JULSimpleFormatter = array of JULSimpleFormatter;
  Arr2JULSimpleFormatter = array of Arr1JULSimpleFormatter;
  Arr3JULSimpleFormatter = array of Arr2JULSimpleFormatter;

  JULXMLFormatter = class;
  Arr1JULXMLFormatter = array of JULXMLFormatter;
  Arr2JULXMLFormatter = array of Arr1JULXMLFormatter;
  Arr3JULXMLFormatter = array of Arr2JULXMLFormatter;

  JULMemoryHandler = class;
  Arr1JULMemoryHandler = array of JULMemoryHandler;
  Arr2JULMemoryHandler = array of Arr1JULMemoryHandler;
  Arr3JULMemoryHandler = array of Arr2JULMemoryHandler;

  JULStreamHandler = class;
  Arr1JULStreamHandler = array of JULStreamHandler;
  Arr2JULStreamHandler = array of Arr1JULStreamHandler;
  Arr3JULStreamHandler = array of Arr2JULStreamHandler;

  JUPAbstractPreferences = class;
  Arr1JUPAbstractPreferences = array of JUPAbstractPreferences;
  Arr2JUPAbstractPreferences = array of Arr1JUPAbstractPreferences;
  Arr3JUPAbstractPreferences = array of Arr2JUPAbstractPreferences;

  JURMatcher = class;
  Arr1JURMatcher = array of JURMatcher;
  Arr2JURMatcher = array of Arr1JURMatcher;
  Arr3JURMatcher = array of Arr2JURMatcher;

  JUZAdler32 = class;
  Arr1JUZAdler32 = array of JUZAdler32;
  Arr2JUZAdler32 = array of Arr1JUZAdler32;
  Arr3JUZAdler32 = array of Arr2JUZAdler32;

  JUZCRC32 = class;
  Arr1JUZCRC32 = array of JUZCRC32;
  Arr2JUZCRC32 = array of Arr1JUZCRC32;
  Arr3JUZCRC32 = array of Arr2JUZCRC32;

  JUJJarFile = class;
  Arr1JUJJarFile = array of JUJJarFile;
  Arr2JUJJarFile = array of Arr1JUJJarFile;
  Arr3JUJJarFile = array of Arr2JUJJarFile;

  JCNullCipher = class;
  Arr1JCNullCipher = array of JCNullCipher;
  Arr2JCNullCipher = array of Arr1JCNullCipher;
  Arr3JCNullCipher = array of Arr2JCNullCipher;

  JMKEEGL10 = interface;
  Arr1JMKEEGL10 = array of JMKEEGL10;
  Arr2JMKEEGL10 = array of Arr1JMKEEGL10;
  Arr3JMKEEGL10 = array of Arr2JMKEEGL10;

  JMKOGL10 = interface;
  Arr1JMKOGL10 = array of JMKOGL10;
  Arr2JMKOGL10 = array of Arr1JMKOGL10;
  Arr3JMKOGL10 = array of Arr2JMKOGL10;

  JMKOGL10Ext = interface;
  Arr1JMKOGL10Ext = array of JMKOGL10Ext;
  Arr2JMKOGL10Ext = array of Arr1JMKOGL10Ext;
  Arr3JMKOGL10Ext = array of Arr2JMKOGL10Ext;

  JMKOGL11Ext = interface;
  Arr1JMKOGL11Ext = array of JMKOGL11Ext;
  Arr2JMKOGL11Ext = array of Arr1JMKOGL11Ext;
  Arr3JMKOGL11Ext = array of Arr2JMKOGL11Ext;

  JMKOGL11ExtensionPack = interface;
  Arr1JMKOGL11ExtensionPack = array of JMKOGL11ExtensionPack;
  Arr2JMKOGL11ExtensionPack = array of Arr1JMKOGL11ExtensionPack;
  Arr3JMKOGL11ExtensionPack = array of Arr2JMKOGL11ExtensionPack;

  JNSSSLServerSocketFactory = class;
  Arr1JNSSSLServerSocketFactory = array of JNSSSLServerSocketFactory;
  Arr2JNSSSLServerSocketFactory = array of Arr1JNSSSLServerSocketFactory;
  Arr3JNSSSLServerSocketFactory = array of Arr2JNSSSLServerSocketFactory;

  JNSSSLSocketFactory = class;
  Arr1JNSSSLSocketFactory = array of JNSSSLSocketFactory;
  Arr2JNSSSLSocketFactory = array of Arr1JNSSSLSocketFactory;
  Arr3JNSSSLSocketFactory = array of Arr2JNSSSLSocketFactory;

  OAHCSX509HostnameVerifier = interface;
  Arr1OAHCSX509HostnameVerifier = array of OAHCSX509HostnameVerifier;
  Arr2OAHCSX509HostnameVerifier = array of Arr1OAHCSX509HostnameVerifier;
  Arr3OAHCSX509HostnameVerifier = array of Arr2OAHCSX509HostnameVerifier;

  JNSX509KeyManager = interface;
  Arr1JNSX509KeyManager = array of JNSX509KeyManager;
  Arr2JNSX509KeyManager = array of Arr1JNSX509KeyManager;
  Arr3JNSX509KeyManager = array of Arr2JNSX509KeyManager;

  JNSCertPathTrustManagerParameters = class;
  Arr1JNSCertPathTrustManagerParameters = array of JNSCertPathTrustManagerParameters;
  Arr2JNSCertPathTrustManagerParameters = array of Arr1JNSCertPathTrustManagerParameters;
  Arr3JNSCertPathTrustManagerParameters = array of Arr2JNSCertPathTrustManagerParameters;

  JNSX509TrustManager = interface;
  Arr1JNSX509TrustManager = array of JNSX509TrustManager;
  Arr2JNSX509TrustManager = array of Arr1JNSX509TrustManager;
  Arr3JNSX509TrustManager = array of Arr2JNSX509TrustManager;

  JSKeyStore = class;
  Arr1JSKeyStore = array of JSKeyStore;
  Arr2JSKeyStore = array of Arr1JSKeyStore;
  Arr3JSKeyStore = array of Arr2JSKeyStore;

  JSACPasswordCallback = class;
  Arr1JSACPasswordCallback = array of JSACPasswordCallback;
  Arr2JSACPasswordCallback = array of Arr1JSACPasswordCallback;
  Arr3JSACPasswordCallback = array of Arr2JSACPasswordCallback;

  JxSCX509Certificate = class;
  Arr1JxSCX509Certificate = array of JxSCX509Certificate;
  Arr2JxSCX509Certificate = array of Arr1JxSCX509Certificate;
  Arr3JxSCX509Certificate = array of Arr2JxSCX509Certificate;

  JSConnectionPoolDataSource = interface;
  Arr1JSConnectionPoolDataSource = array of JSConnectionPoolDataSource;
  Arr2JSConnectionPoolDataSource = array of Arr1JSConnectionPoolDataSource;
  Arr3JSConnectionPoolDataSource = array of Arr2JSConnectionPoolDataSource;

  JSDataSource = interface;
  Arr1JSDataSource = array of JSDataSource;
  Arr2JSDataSource = array of Arr1JSDataSource;
  Arr3JSDataSource = array of Arr2JSDataSource;

  JXDDuration = class;
  Arr1JXDDuration = array of JXDDuration;
  Arr2JXDDuration = array of Arr1JXDDuration;
  Arr3JXDDuration = array of Arr2JXDDuration;

  JXTDDOMResult = class;
  Arr1JXTDDOMResult = array of JXTDDOMResult;
  Arr2JXTDDOMResult = array of Arr1JXTDDOMResult;
  Arr3JXTDDOMResult = array of Arr2JXTDDOMResult;

  JXTSSAXResult = class;
  Arr1JXTSSAXResult = array of JXTSSAXResult;
  Arr2JXTSSAXResult = array of Arr1JXTSSAXResult;
  Arr3JXTSSAXResult = array of Arr2JXTSSAXResult;

  JXTSStreamResult = class;
  Arr1JXTSStreamResult = array of JXTSStreamResult;
  Arr2JXTSStreamResult = array of Arr1JXTSStreamResult;
  Arr3JXTSStreamResult = array of Arr2JXTSStreamResult;

  JXTDDOMSource = class;
  Arr1JXTDDOMSource = array of JXTDDOMSource;
  Arr2JXTDDOMSource = array of Arr1JXTDDOMSource;
  Arr3JXTDDOMSource = array of Arr2JXTDDOMSource;

  JXTSSAXSource = class;
  Arr1JXTSSAXSource = array of JXTSSAXSource;
  Arr2JXTSSAXSource = array of Arr1JXTSSAXSource;
  Arr3JXTSSAXSource = array of Arr2JXTSSAXSource;

  JXTSStreamSource = class;
  Arr1JXTSStreamSource = array of JXTSStreamSource;
  Arr2JXTSStreamSource = array of Arr1JXTSStreamSource;
  Arr3JXTSStreamSource = array of Arr2JXTSStreamSource;

  JXTDDOMLocator = interface;
  Arr1JXTDDOMLocator = array of JXTDDOMLocator;
  Arr2JXTDDOMLocator = array of Arr1JXTDDOMLocator;
  Arr3JXTDDOMLocator = array of Arr2JXTDDOMLocator;

  JXTSSAXTransformerFactory = class;
  Arr1JXTSSAXTransformerFactory = array of JXTSSAXTransformerFactory;
  Arr2JXTSSAXTransformerFactory = array of Arr1JXTSSAXTransformerFactory;
  Arr3JXTSSAXTransformerFactory = array of Arr2JXTSSAXTransformerFactory;

  JFTestCase = class;
  Arr1JFTestCase = array of JFTestCase;
  Arr2JFTestCase = array of Arr1JFTestCase;
  Arr3JFTestCase = array of Arr2JFTestCase;

  JFTestSuite = class;
  Arr1JFTestSuite = array of JFTestSuite;
  Arr2JFTestSuite = array of Arr1JFTestSuite;
  Arr3JFTestSuite = array of Arr2JFTestSuite;

  JRBaseTestRunner = class;
  Arr1JRBaseTestRunner = array of JRBaseTestRunner;
  Arr2JRBaseTestRunner = array of Arr1JRBaseTestRunner;
  Arr3JRBaseTestRunner = array of Arr2JRBaseTestRunner;

  OAHCSLayeredSocketFactory = interface;
  Arr1OAHCSLayeredSocketFactory = array of OAHCSLayeredSocketFactory;
  Arr2OAHCSLayeredSocketFactory = array of Arr1OAHCSLayeredSocketFactory;
  Arr3OAHCSLayeredSocketFactory = array of Arr2OAHCSLayeredSocketFactory;

  OAHPHttpConnectionParams = class;
  Arr1OAHPHttpConnectionParams = array of OAHPHttpConnectionParams;
  Arr2OAHPHttpConnectionParams = array of Arr1OAHPHttpConnectionParams;
  Arr3OAHPHttpConnectionParams = array of Arr2OAHPHttpConnectionParams;

  OWDAttr = interface;
  Arr1OWDAttr = array of OWDAttr;
  Arr2OWDAttr = array of Arr1OWDAttr;
  Arr3OWDAttr = array of Arr2OWDAttr;

  OWDCharacterData = interface;
  Arr1OWDCharacterData = array of OWDCharacterData;
  Arr2OWDCharacterData = array of Arr1OWDCharacterData;
  Arr3OWDCharacterData = array of Arr2OWDCharacterData;

  OWDDocument = interface;
  Arr1OWDDocument = array of OWDDocument;
  Arr2OWDDocument = array of Arr1OWDDocument;
  Arr3OWDDocument = array of Arr2OWDDocument;

  OWDDocumentFragment = interface;
  Arr1OWDDocumentFragment = array of OWDDocumentFragment;
  Arr2OWDDocumentFragment = array of Arr1OWDDocumentFragment;
  Arr3OWDDocumentFragment = array of Arr2OWDDocumentFragment;

  OWDDocumentType = interface;
  Arr1OWDDocumentType = array of OWDDocumentType;
  Arr2OWDDocumentType = array of Arr1OWDDocumentType;
  Arr3OWDDocumentType = array of Arr2OWDDocumentType;

  OWDElement = interface;
  Arr1OWDElement = array of OWDElement;
  Arr2OWDElement = array of Arr1OWDElement;
  Arr3OWDElement = array of Arr2OWDElement;

  OWDEntity = interface;
  Arr1OWDEntity = array of OWDEntity;
  Arr2OWDEntity = array of Arr1OWDEntity;
  Arr3OWDEntity = array of Arr2OWDEntity;

  OWDEntityReference = interface;
  Arr1OWDEntityReference = array of OWDEntityReference;
  Arr2OWDEntityReference = array of Arr1OWDEntityReference;
  Arr3OWDEntityReference = array of Arr2OWDEntityReference;

  OWDNotation = interface;
  Arr1OWDNotation = array of OWDNotation;
  Arr2OWDNotation = array of Arr1OWDNotation;
  Arr3OWDNotation = array of Arr2OWDNotation;

  OWDProcessingInstruction = interface;
  Arr1OWDProcessingInstruction = array of OWDProcessingInstruction;
  Arr2OWDProcessingInstruction = array of Arr1OWDProcessingInstruction;
  Arr3OWDProcessingInstruction = array of Arr2OWDProcessingInstruction;

  OXSHAttributeListImpl = class;
  Arr1OXSHAttributeListImpl = array of OXSHAttributeListImpl;
  Arr2OXSHAttributeListImpl = array of Arr1OXSHAttributeListImpl;
  Arr3OXSHAttributeListImpl = array of Arr2OXSHAttributeListImpl;

  OXSEAttributes2 = interface;
  Arr1OXSEAttributes2 = array of OXSEAttributes2;
  Arr2OXSEAttributes2 = array of Arr1OXSEAttributes2;
  Arr3OXSEAttributes2 = array of Arr2OXSEAttributes2;

  OXSHAttributesImpl = class;
  Arr1OXSHAttributesImpl = array of OXSHAttributesImpl;
  Arr2OXSHAttributesImpl = array of Arr1OXSHAttributesImpl;
  Arr3OXSHAttributesImpl = array of Arr2OXSHAttributesImpl;

  JXTSTemplatesHandler = interface;
  Arr1JXTSTemplatesHandler = array of JXTSTemplatesHandler;
  Arr2JXTSTemplatesHandler = array of Arr1JXTSTemplatesHandler;
  Arr3JXTSTemplatesHandler = array of Arr2JXTSTemplatesHandler;

  JXVValidatorHandler = class;
  Arr1JXVValidatorHandler = array of JXVValidatorHandler;
  Arr2JXVValidatorHandler = array of Arr1JXVValidatorHandler;
  Arr3JXVValidatorHandler = array of Arr2JXVValidatorHandler;

  OXSEEntityResolver2 = interface;
  Arr1OXSEEntityResolver2 = array of OXSEEntityResolver2;
  Arr2OXSEEntityResolver2 = array of Arr1OXSEEntityResolver2;
  Arr3OXSEEntityResolver2 = array of Arr2OXSEEntityResolver2;

  OXSHandlerBase = class;
  Arr1OXSHandlerBase = array of OXSHandlerBase;
  Arr2OXSHandlerBase = array of Arr1OXSHandlerBase;
  Arr3OXSHandlerBase = array of Arr2OXSHandlerBase;

  OXSHDefaultHandler = class;
  Arr1OXSHDefaultHandler = array of OXSHDefaultHandler;
  Arr2OXSHDefaultHandler = array of Arr1OXSHDefaultHandler;
  Arr3OXSHDefaultHandler = array of Arr2OXSHDefaultHandler;

  OXSELocator2 = interface;
  Arr1OXSELocator2 = array of OXSELocator2;
  Arr2OXSELocator2 = array of Arr1OXSELocator2;
  Arr3OXSELocator2 = array of Arr2OXSELocator2;

  OXSHLocatorImpl = class;
  Arr1OXSHLocatorImpl = array of OXSHLocatorImpl;
  Arr2OXSHLocatorImpl = array of Arr1OXSHLocatorImpl;
  Arr3OXSHLocatorImpl = array of Arr2OXSHLocatorImpl;

  OXSHXMLReaderAdapter = class;
  Arr1OXSHXMLReaderAdapter = array of OXSHXMLReaderAdapter;
  Arr2OXSHXMLReaderAdapter = array of Arr1OXSHXMLReaderAdapter;
  Arr3OXSHXMLReaderAdapter = array of Arr2OXSHXMLReaderAdapter;

  OXSXMLFilter = interface;
  Arr1OXSXMLFilter = array of OXSXMLFilter;
  Arr2OXSXMLFilter = array of Arr1OXSXMLFilter;
  Arr3OXSXMLFilter = array of Arr2OXSXMLFilter;

  OXSHParserAdapter = class;
  Arr1OXSHParserAdapter = array of OXSHParserAdapter;
  Arr2OXSHParserAdapter = array of Arr1OXSHParserAdapter;
  Arr3OXSHParserAdapter = array of Arr2OXSHParserAdapter;

  OXVSDriver = class;
  Arr1OXVSDriver = array of OXVSDriver;
  Arr2OXVSDriver = array of Arr1OXVSDriver;
  Arr3OXVSDriver = array of Arr2OXVSDriver;

  JXTSTransformerHandler = interface;
  Arr1JXTSTransformerHandler = array of JXTSTransformerHandler;
  Arr2JXTSTransformerHandler = array of Arr1JXTSTransformerHandler;
  Arr3JXTSTransformerHandler = array of Arr2JXTSTransformerHandler;

  ACRXmlResourceParser = interface;
  Arr1ACRXmlResourceParser = array of ACRXmlResourceParser;
  Arr2ACRXmlResourceParser = array of Arr1ACRXmlResourceParser;
  Arr3ACRXmlResourceParser = array of Arr2ACRXmlResourceParser;

  AAAuthenticatorException = class;
  Arr1AAAuthenticatorException = array of AAAuthenticatorException;
  Arr2AAAuthenticatorException = array of Arr1AAAuthenticatorException;
  Arr3AAAuthenticatorException = array of Arr2AAAuthenticatorException;

  AANetworkErrorException = class;
  Arr1AANetworkErrorException = array of AANetworkErrorException;
  Arr2AANetworkErrorException = array of Arr1AANetworkErrorException;
  Arr3AANetworkErrorException = array of Arr2AANetworkErrorException;

  AAOperationCanceledException = class;
  Arr1AAOperationCanceledException = array of AAOperationCanceledException;
  Arr2AAOperationCanceledException = array of Arr1AAOperationCanceledException;
  Arr3AAOperationCanceledException = array of Arr2AAOperationCanceledException;

  AAPendingIntent = class;
  Arr1AAPendingIntent = array of AAPendingIntent;
  Arr2AAPendingIntent = array of Arr1AAPendingIntent;
  Arr3AAPendingIntent = array of Arr2AAPendingIntent;

  ACIntentFilter = class;
  Arr1ACIntentFilter = array of ACIntentFilter;
  Arr2ACIntentFilter = array of Arr1ACIntentFilter;
  Arr3ACIntentFilter = array of Arr2ACIntentFilter;

  ACIntentSender = class;
  Arr1ACIntentSender = array of ACIntentSender;
  Arr2ACIntentSender = array of Arr1ACIntentSender;
  Arr3ACIntentSender = array of Arr2ACIntentSender;

  ACPPackageManager = class;
  Arr1ACPPackageManager = array of ACPPackageManager;
  Arr2ACPPackageManager = array of Arr1ACPPackageManager;
  Arr3ACPPackageManager = array of Arr2ACPPackageManager;

  AORemoteException = class;
  Arr1AORemoteException = array of AORemoteException;
  Arr2AORemoteException = array of Arr1AORemoteException;
  Arr3AORemoteException = array of Arr2AORemoteException;

  APSettings = class;
  Arr1APSettings = array of APSettings;
  Arr2APSettings = array of Arr1APSettings;
  Arr3APSettings = array of Arr2APSettings;

  JSDigestException = class;
  Arr1JSDigestException = array of JSDigestException;
  Arr2JSDigestException = array of Arr1JSDigestException;
  Arr3JSDigestException = array of Arr2JSDigestException;

  JSInvalidAlgorithmParameterException = class;
  Arr1JSInvalidAlgorithmParameterException = array of JSInvalidAlgorithmParameterException;
  Arr2JSInvalidAlgorithmParameterException = array of Arr1JSInvalidAlgorithmParameterException;
  Arr3JSInvalidAlgorithmParameterException = array of Arr2JSInvalidAlgorithmParameterException;

  JSKeyException = class;
  Arr1JSKeyException = array of JSKeyException;
  Arr2JSKeyException = array of Arr1JSKeyException;
  Arr3JSKeyException = array of Arr2JSKeyException;

  JSKeyStoreException = class;
  Arr1JSKeyStoreException = array of JSKeyStoreException;
  Arr2JSKeyStoreException = array of Arr1JSKeyStoreException;
  Arr3JSKeyStoreException = array of Arr2JSKeyStoreException;

  JSNoSuchAlgorithmException = class;
  Arr1JSNoSuchAlgorithmException = array of JSNoSuchAlgorithmException;
  Arr2JSNoSuchAlgorithmException = array of Arr1JSNoSuchAlgorithmException;
  Arr3JSNoSuchAlgorithmException = array of Arr2JSNoSuchAlgorithmException;

  JSNoSuchProviderException = class;
  Arr1JSNoSuchProviderException = array of JSNoSuchProviderException;
  Arr2JSNoSuchProviderException = array of Arr1JSNoSuchProviderException;
  Arr3JSNoSuchProviderException = array of Arr2JSNoSuchProviderException;

  JSSignatureException = class;
  Arr1JSSignatureException = array of JSSignatureException;
  Arr2JSSignatureException = array of Arr1JSSignatureException;
  Arr3JSSignatureException = array of Arr2JSSignatureException;

  JSUnrecoverableEntryException = class;
  Arr1JSUnrecoverableEntryException = array of JSUnrecoverableEntryException;
  Arr2JSUnrecoverableEntryException = array of Arr1JSUnrecoverableEntryException;
  Arr3JSUnrecoverableEntryException = array of Arr2JSUnrecoverableEntryException;

  JSCCRLException = class;
  Arr1JSCCRLException = array of JSCCRLException;
  Arr2JSCCRLException = array of Arr1JSCCRLException;
  Arr3JSCCRLException = array of Arr2JSCCRLException;

  JSCCertPathBuilderException = class;
  Arr1JSCCertPathBuilderException = array of JSCCertPathBuilderException;
  Arr2JSCCertPathBuilderException = array of Arr1JSCCertPathBuilderException;
  Arr3JSCCertPathBuilderException = array of Arr2JSCCertPathBuilderException;

  JSCCertPathValidatorException = class;
  Arr1JSCCertPathValidatorException = array of JSCCertPathValidatorException;
  Arr2JSCCertPathValidatorException = array of Arr1JSCCertPathValidatorException;
  Arr3JSCCertPathValidatorException = array of Arr2JSCCertPathValidatorException;

  JSCCertStoreException = class;
  Arr1JSCCertStoreException = array of JSCCertStoreException;
  Arr2JSCCertStoreException = array of Arr1JSCCertStoreException;
  Arr3JSCCertStoreException = array of Arr2JSCCertStoreException;

  JSCCertificateException = class;
  Arr1JSCCertificateException = array of JSCCertificateException;
  Arr2JSCCertificateException = array of Arr1JSCCertificateException;
  Arr3JSCCertificateException = array of Arr2JSCCertificateException;

  JSSInvalidKeySpecException = class;
  Arr1JSSInvalidKeySpecException = array of JSSInvalidKeySpecException;
  Arr2JSSInvalidKeySpecException = array of Arr1JSSInvalidKeySpecException;
  Arr3JSSInvalidKeySpecException = array of Arr2JSSInvalidKeySpecException;

  JSSInvalidParameterSpecException = class;
  Arr1JSSInvalidParameterSpecException = array of JSSInvalidParameterSpecException;
  Arr2JSSInvalidParameterSpecException = array of Arr1JSSInvalidParameterSpecException;
  Arr3JSSInvalidParameterSpecException = array of Arr2JSSInvalidParameterSpecException;

  JCBadPaddingException = class;
  Arr1JCBadPaddingException = array of JCBadPaddingException;
  Arr2JCBadPaddingException = array of Arr1JCBadPaddingException;
  Arr3JCBadPaddingException = array of Arr2JCBadPaddingException;

  JCExemptionMechanismException = class;
  Arr1JCExemptionMechanismException = array of JCExemptionMechanismException;
  Arr2JCExemptionMechanismException = array of Arr1JCExemptionMechanismException;
  Arr3JCExemptionMechanismException = array of Arr2JCExemptionMechanismException;

  JCIllegalBlockSizeException = class;
  Arr1JCIllegalBlockSizeException = array of JCIllegalBlockSizeException;
  Arr2JCIllegalBlockSizeException = array of Arr1JCIllegalBlockSizeException;
  Arr3JCIllegalBlockSizeException = array of Arr2JCIllegalBlockSizeException;

  JCNoSuchPaddingException = class;
  Arr1JCNoSuchPaddingException = array of JCNoSuchPaddingException;
  Arr2JCNoSuchPaddingException = array of Arr1JCNoSuchPaddingException;
  Arr3JCNoSuchPaddingException = array of Arr2JCNoSuchPaddingException;

  JCShortBufferException = class;
  Arr1JCShortBufferException = array of JCShortBufferException;
  Arr2JCShortBufferException = array of Arr1JCShortBufferException;
  Arr3JCShortBufferException = array of Arr2JCShortBufferException;

  JSALLoginException = class;
  Arr1JSALLoginException = array of JSALLoginException;
  Arr2JSALLoginException = array of Arr1JSALLoginException;
  Arr3JSALLoginException = array of Arr2JSALLoginException;

  JxSCCertificateEncodingException = class;
  Arr1JxSCCertificateEncodingException = array of JxSCCertificateEncodingException;
  Arr2JxSCCertificateEncodingException = array of Arr1JxSCCertificateEncodingException;
  Arr3JxSCCertificateEncodingException = array of Arr2JxSCCertificateEncodingException;

  JxSCCertificateExpiredException = class;
  Arr1JxSCCertificateExpiredException = array of JxSCCertificateExpiredException;
  Arr2JxSCCertificateExpiredException = array of Arr1JxSCCertificateExpiredException;
  Arr3JxSCCertificateExpiredException = array of Arr2JxSCCertificateExpiredException;

  JxSCCertificateNotYetValidException = class;
  Arr1JxSCCertificateNotYetValidException = array of JxSCCertificateNotYetValidException;
  Arr2JxSCCertificateNotYetValidException = array of Arr1JxSCCertificateNotYetValidException;
  Arr3JxSCCertificateNotYetValidException = array of Arr2JxSCCertificateNotYetValidException;

  JxSCCertificateParsingException = class;
  Arr1JxSCCertificateParsingException = array of JxSCCertificateParsingException;
  Arr2JxSCCertificateParsingException = array of Arr1JxSCCertificateParsingException;
  Arr3JxSCCertificateParsingException = array of Arr2JxSCCertificateParsingException;

  JXTTransformerConfigurationException = class;
  Arr1JXTTransformerConfigurationException = array of JXTTransformerConfigurationException;
  Arr2JXTTransformerConfigurationException = array of Arr1JXTTransformerConfigurationException;
  Arr3JXTTransformerConfigurationException = array of Arr2JXTTransformerConfigurationException;

  JXXXPathExpressionException = class;
  Arr1JXXXPathExpressionException = array of JXXXPathExpressionException;
  Arr2JXXXPathExpressionException = array of Arr1JXXXPathExpressionException;
  Arr3JXXXPathExpressionException = array of Arr2JXXXPathExpressionException;

  JXXXPathFactoryConfigurationException = class;
  Arr1JXXXPathFactoryConfigurationException = array of JXXXPathFactoryConfigurationException;
  Arr2JXXXPathFactoryConfigurationException = array of Arr1JXXXPathFactoryConfigurationException;
  Arr3JXXXPathFactoryConfigurationException = array of Arr2JXXXPathFactoryConfigurationException;

  OXSSAXNotRecognizedException = class;
  Arr1OXSSAXNotRecognizedException = array of OXSSAXNotRecognizedException;
  Arr2OXSSAXNotRecognizedException = array of Arr1OXSSAXNotRecognizedException;
  Arr3OXSSAXNotRecognizedException = array of Arr2OXSSAXNotRecognizedException;

  OXSSAXNotSupportedException = class;
  Arr1OXSSAXNotSupportedException = array of OXSSAXNotSupportedException;
  Arr2OXSSAXNotSupportedException = array of Arr1OXSSAXNotSupportedException;
  Arr3OXSSAXNotSupportedException = array of Arr2OXSSAXNotSupportedException;

  OXSSAXParseException = class;
  Arr1OXSSAXParseException = array of OXSSAXParseException;
  Arr2OXSSAXParseException = array of Arr1OXSSAXParseException;
  Arr3OXSSAXParseException = array of Arr2OXSSAXParseException;

  AAAnimatorListenerAdapter = class;
  Arr1AAAnimatorListenerAdapter = array of AAAnimatorListenerAdapter;
  Arr2AAAnimatorListenerAdapter = array of Arr1AAAnimatorListenerAdapter;
  Arr3AAAnimatorListenerAdapter = array of Arr2AAAnimatorListenerAdapter;

  AAAnimatorSet = class;
  Arr1AAAnimatorSet = array of AAAnimatorSet;
  Arr2AAAnimatorSet = array of Arr1AAAnimatorSet;
  Arr3AAAnimatorSet = array of Arr2AAAnimatorSet;

  AAValueAnimator = class;
  Arr1AAValueAnimator = array of AAValueAnimator;
  Arr2AAValueAnimator = array of Arr1AAValueAnimator;
  Arr3AAValueAnimator = array of Arr2AAValueAnimator;

  AVViewPropertyAnimator = class;
  Arr1AVViewPropertyAnimator = array of AVViewPropertyAnimator;
  Arr2AVViewPropertyAnimator = array of Arr1AVViewPropertyAnimator;
  Arr3AVViewPropertyAnimator = array of Arr2AVViewPropertyAnimator;

  AGDSPathShape = class;
  Arr1AGDSPathShape = array of AGDSPathShape;
  Arr2AGDSPathShape = array of Arr1AGDSPathShape;
  Arr3AGDSPathShape = array of Arr2AGDSPathShape;

  AGDSRectShape = class;
  Arr1AGDSRectShape = array of AGDSRectShape;
  Arr2AGDSRectShape = array of Arr1AGDSRectShape;
  Arr3AGDSRectShape = array of Arr2AGDSRectShape;

  AVAAlphaAnimation = class;
  Arr1AVAAlphaAnimation = array of AVAAlphaAnimation;
  Arr2AVAAlphaAnimation = array of Arr1AVAAlphaAnimation;
  Arr3AVAAlphaAnimation = array of Arr2AVAAlphaAnimation;

  AVAAnimationSet = class;
  Arr1AVAAnimationSet = array of AVAAnimationSet;
  Arr2AVAAnimationSet = array of Arr1AVAAnimationSet;
  Arr3AVAAnimationSet = array of Arr2AVAAnimationSet;

  AVARotateAnimation = class;
  Arr1AVARotateAnimation = array of AVARotateAnimation;
  Arr2AVARotateAnimation = array of Arr1AVARotateAnimation;
  Arr3AVARotateAnimation = array of Arr2AVARotateAnimation;

  AVAScaleAnimation = class;
  Arr1AVAScaleAnimation = array of AVAScaleAnimation;
  Arr2AVAScaleAnimation = array of Arr1AVAScaleAnimation;
  Arr3AVAScaleAnimation = array of Arr2AVAScaleAnimation;

  AVATranslateAnimation = class;
  Arr1AVATranslateAnimation = array of AVATranslateAnimation;
  Arr2AVATranslateAnimation = array of Arr1AVATranslateAnimation;
  Arr3AVATranslateAnimation = array of Arr2AVATranslateAnimation;

  JSCX509CRLSelector = class;
  Arr1JSCX509CRLSelector = array of JSCX509CRLSelector;
  Arr2JSCX509CRLSelector = array of Arr1JSCX509CRLSelector;
  Arr3JSCX509CRLSelector = array of Arr2JSCX509CRLSelector;

  JSCPKIXParameters = class;
  Arr1JSCPKIXParameters = array of JSCPKIXParameters;
  Arr2JSCPKIXParameters = array of Arr1JSCPKIXParameters;
  Arr3JSCPKIXParameters = array of Arr2JSCPKIXParameters;

  JSCPKIXCertPathValidatorResult = class;
  Arr1JSCPKIXCertPathValidatorResult = array of JSCPKIXCertPathValidatorResult;
  Arr2JSCPKIXCertPathValidatorResult = array of Arr1JSCPKIXCertPathValidatorResult;
  Arr3JSCPKIXCertPathValidatorResult = array of Arr2JSCPKIXCertPathValidatorResult;

  JSCX509CertSelector = class;
  Arr1JSCX509CertSelector = array of JSCX509CertSelector;
  Arr2JSCX509CertSelector = array of Arr1JSCX509CertSelector;
  Arr3JSCX509CertSelector = array of Arr2JSCX509CertSelector;

  JSCCollectionCertStoreParameters = class;
  Arr1JSCCollectionCertStoreParameters = array of JSCCollectionCertStoreParameters;
  Arr2JSCCollectionCertStoreParameters = array of Arr1JSCCollectionCertStoreParameters;
  Arr3JSCCollectionCertStoreParameters = array of Arr2JSCCollectionCertStoreParameters;

  JSCLDAPCertStoreParameters = class;
  Arr1JSCLDAPCertStoreParameters = array of JSCLDAPCertStoreParameters;
  Arr2JSCLDAPCertStoreParameters = array of Arr1JSCLDAPCertStoreParameters;
  Arr3JSCLDAPCertStoreParameters = array of Arr2JSCLDAPCertStoreParameters;

  JTAttributedCharacterIterator = interface;
  Arr1JTAttributedCharacterIterator = array of JTAttributedCharacterIterator;
  Arr2JTAttributedCharacterIterator = array of Arr1JTAttributedCharacterIterator;
  Arr3JTAttributedCharacterIterator = array of Arr2JTAttributedCharacterIterator;

  JTStringCharacterIterator = class;
  Arr1JTStringCharacterIterator = array of JTStringCharacterIterator;
  Arr2JTStringCharacterIterator = array of Arr1JTStringCharacterIterator;
  Arr3JTStringCharacterIterator = array of Arr2JTStringCharacterIterator;

  JUJJarEntry = class;
  Arr1JUJJarEntry = array of JUJJarEntry;
  Arr2JUJJarEntry = array of Arr1JUJJarEntry;
  Arr3JUJJarEntry = array of Arr2JUJJarEntry;

  ATMMockResources = class;
  Arr1ATMMockResources = array of ATMMockResources;
  Arr2ATMMockResources = array of Arr1ATMMockResources;
  Arr3ATMMockResources = array of Arr2ATMMockResources;

  ADSSQLiteException = class;
  Arr1ADSSQLiteException = array of ADSSQLiteException;
  Arr2ADSSQLiteException = array of Arr1ADSSQLiteException;
  Arr3ADSSQLiteException = array of Arr2ADSSQLiteException;

  ARRSDriverException = class;
  Arr1ARRSDriverException = array of ARRSDriverException;
  Arr2ARRSDriverException = array of Arr1ARRSDriverException;
  Arr3ARRSDriverException = array of Arr2ARRSDriverException;

  ARRSIllegalArgumentException = class;
  Arr1ARRSIllegalArgumentException = array of ARRSIllegalArgumentException;
  Arr2ARRSIllegalArgumentException = array of Arr1ARRSIllegalArgumentException;
  Arr3ARRSIllegalArgumentException = array of Arr2ARRSIllegalArgumentException;

  ARRSInvalidStateException = class;
  Arr1ARRSInvalidStateException = array of ARRSInvalidStateException;
  Arr2ARRSInvalidStateException = array of Arr1ARRSInvalidStateException;
  Arr3ARRSInvalidStateException = array of Arr2ARRSInvalidStateException;

  ACReceiverCallNotAllowedException = class;
  Arr1ACReceiverCallNotAllowedException = array of ACReceiverCallNotAllowedException;
  Arr2ACReceiverCallNotAllowedException = array of Arr1ACReceiverCallNotAllowedException;
  Arr3ACReceiverCallNotAllowedException = array of Arr2ACReceiverCallNotAllowedException;

  AOBadParcelableException = class;
  Arr1AOBadParcelableException = array of AOBadParcelableException;
  Arr2AOBadParcelableException = array of Arr1AOBadParcelableException;
  Arr3AOBadParcelableException = array of Arr2AOBadParcelableException;

  AVKeyCharacterMap = class;
  Arr1AVKeyCharacterMap = array of AVKeyCharacterMap;
  Arr2AVKeyCharacterMap = array of Arr1AVKeyCharacterMap;
  Arr3AVKeyCharacterMap = array of Arr2AVKeyCharacterMap;

  JSAccessControlException = class;
  Arr1JSAccessControlException = array of JSAccessControlException;
  Arr2JSAccessControlException = array of Arr1JSAccessControlException;
  Arr3JSAccessControlException = array of Arr2JSAccessControlException;

  JUInputMismatchException = class;
  Arr1JUInputMismatchException = array of JUInputMismatchException;
  Arr2JUInputMismatchException = array of Arr1JUInputMismatchException;
  Arr3JUInputMismatchException = array of Arr2JUInputMismatchException;

  JUCScheduledFuture = interface;
  Arr1JUCScheduledFuture = array of JUCScheduledFuture;
  Arr2JUCScheduledFuture = array of Arr1JUCScheduledFuture;
  Arr3JUCScheduledFuture = array of Arr2JUCScheduledFuture;

  ACPApplicationInfo = class;
  Arr1ACPApplicationInfo = array of ACPApplicationInfo;
  Arr2ACPApplicationInfo = array of Arr1ACPApplicationInfo;
  Arr3ACPApplicationInfo = array of Arr2ACPApplicationInfo;

  ACPComponentInfo = class;
  Arr1ACPComponentInfo = array of ACPComponentInfo;
  Arr2ACPComponentInfo = array of Arr1ACPComponentInfo;
  Arr3ACPComponentInfo = array of Arr2ACPComponentInfo;

  ACPInstrumentationInfo = class;
  Arr1ACPInstrumentationInfo = array of ACPInstrumentationInfo;
  Arr2ACPInstrumentationInfo = array of Arr1ACPInstrumentationInfo;
  Arr3ACPInstrumentationInfo = array of Arr2ACPInstrumentationInfo;

  ACPPermissionGroupInfo = class;
  Arr1ACPPermissionGroupInfo = array of ACPPermissionGroupInfo;
  Arr2ACPPermissionGroupInfo = array of Arr1ACPPermissionGroupInfo;
  Arr3ACPPermissionGroupInfo = array of Arr2ACPPermissionGroupInfo;

  ACPPermissionInfo = class;
  Arr1ACPPermissionInfo = array of ACPPermissionInfo;
  Arr2ACPPermissionInfo = array of Arr1ACPPermissionInfo;
  Arr3ACPPermissionInfo = array of Arr2ACPPermissionInfo;

  ATTextPaint = class;
  Arr1ATTextPaint = array of ATTextPaint;
  Arr2ATTextPaint = array of Arr1ATTextPaint;
  Arr3ATTextPaint = array of Arr2ATTextPaint;

  AGPorterDuffColorFilter = class;
  Arr1AGPorterDuffColorFilter = array of AGPorterDuffColorFilter;
  Arr2AGPorterDuffColorFilter = array of Arr1AGPorterDuffColorFilter;
  Arr3AGPorterDuffColorFilter = array of Arr2AGPorterDuffColorFilter;

  AGPorterDuffXfermode = class;
  Arr1AGPorterDuffXfermode = array of AGPorterDuffXfermode;
  Arr2AGPorterDuffXfermode = array of Arr1AGPorterDuffXfermode;
  Arr3AGPorterDuffXfermode = array of Arr2AGPorterDuffXfermode;

  AGBitmapShader = class;
  Arr1AGBitmapShader = array of AGBitmapShader;
  Arr2AGBitmapShader = array of Arr1AGBitmapShader;
  Arr3AGBitmapShader = array of Arr2AGBitmapShader;

  AGComposeShader = class;
  Arr1AGComposeShader = array of AGComposeShader;
  Arr2AGComposeShader = array of Arr1AGComposeShader;
  Arr3AGComposeShader = array of Arr2AGComposeShader;

  AGLinearGradient = class;
  Arr1AGLinearGradient = array of AGLinearGradient;
  Arr2AGLinearGradient = array of Arr1AGLinearGradient;
  Arr3AGLinearGradient = array of Arr2AGLinearGradient;

  AGRadialGradient = class;
  Arr1AGRadialGradient = array of AGRadialGradient;
  Arr2AGRadialGradient = array of Arr1AGRadialGradient;
  Arr3AGRadialGradient = array of Arr2AGRadialGradient;

  AGSweepGradient = class;
  Arr1AGSweepGradient = array of AGSweepGradient;
  Arr2AGSweepGradient = array of Arr1AGSweepGradient;
  Arr3AGSweepGradient = array of Arr2AGSweepGradient;

  JNSSSLEngine = class;
  Arr1JNSSSLEngine = array of JNSSSLEngine;
  Arr2JNSSSLEngine = array of Arr1JNSSSLEngine;
  Arr3JNSSSLEngine = array of Arr2JNSSSLEngine;

  JNInet4Address = class;
  Arr1JNInet4Address = array of JNInet4Address;
  Arr2JNInet4Address = array of Arr1JNInet4Address;
  Arr3JNInet4Address = array of Arr2JNInet4Address;

  JNInet6Address = class;
  Arr1JNInet6Address = array of JNInet6Address;
  Arr2JNInet6Address = array of Arr1JNInet6Address;
  Arr3JNInet6Address = array of Arr2JNInet6Address;

  JNInetSocketAddress = class;
  Arr1JNInetSocketAddress = array of JNInetSocketAddress;
  Arr2JNInetSocketAddress = array of Arr1JNInetSocketAddress;
  Arr3JNInetSocketAddress = array of Arr2JNInetSocketAddress;

  JSPrivateKey = interface;
  Arr1JSPrivateKey = array of JSPrivateKey;
  Arr2JSPrivateKey = array of Arr1JSPrivateKey;
  Arr3JSPrivateKey = array of Arr2JSPrivateKey;

  JSPublicKey = interface;
  Arr1JSPublicKey = array of JSPublicKey;
  Arr2JSPublicKey = array of Arr1JSPublicKey;
  Arr3JSPublicKey = array of Arr2JSPublicKey;

  JCSecretKey = interface;
  Arr1JCSecretKey = array of JCSecretKey;
  Arr2JCSecretKey = array of Arr1JCSecretKey;
  Arr3JCSecretKey = array of Arr2JCSecretKey;

  JSPermissions = class;
  Arr1JSPermissions = array of JSPermissions;
  Arr2JSPermissions = array of Arr1JSPermissions;
  Arr3JSPermissions = array of Arr2JSPermissions;

  JSCX509Certificate = class;
  Arr1JSCX509Certificate = array of JSCX509Certificate;
  Arr2JSCX509Certificate = array of Arr1JSCX509Certificate;
  Arr3JSCX509Certificate = array of Arr2JSCX509Certificate;

  JSBatchUpdateException = class;
  Arr1JSBatchUpdateException = array of JSBatchUpdateException;
  Arr2JSBatchUpdateException = array of Arr1JSBatchUpdateException;
  Arr3JSBatchUpdateException = array of Arr2JSBatchUpdateException;

  JSSQLClientInfoException = class;
  Arr1JSSQLClientInfoException = array of JSSQLClientInfoException;
  Arr2JSSQLClientInfoException = array of Arr1JSSQLClientInfoException;
  Arr3JSSQLClientInfoException = array of Arr2JSSQLClientInfoException;

  JSSQLNonTransientException = class;
  Arr1JSSQLNonTransientException = array of JSSQLNonTransientException;
  Arr2JSSQLNonTransientException = array of Arr1JSSQLNonTransientException;
  Arr3JSSQLNonTransientException = array of Arr2JSSQLNonTransientException;

  JSSQLRecoverableException = class;
  Arr1JSSQLRecoverableException = array of JSSQLRecoverableException;
  Arr2JSSQLRecoverableException = array of Arr1JSSQLRecoverableException;
  Arr3JSSQLRecoverableException = array of Arr2JSSQLRecoverableException;

  JSSQLTransientException = class;
  Arr1JSSQLTransientException = array of JSSQLTransientException;
  Arr2JSSQLTransientException = array of Arr1JSSQLTransientException;
  Arr3JSSQLTransientException = array of Arr2JSSQLTransientException;

  JSSQLWarning = class;
  Arr1JSSQLWarning = array of JSSQLWarning;
  Arr2JSSQLWarning = array of Arr1JSSQLWarning;
  Arr3JSSQLWarning = array of Arr2JSSQLWarning;

  JSDate = class;
  Arr1JSDate = array of JSDate;
  Arr2JSDate = array of Arr1JSDate;
  Arr3JSDate = array of Arr2JSDate;

  JSTime = class;
  Arr1JSTime = array of JSTime;
  Arr2JSTime = array of Arr1JSTime;
  Arr3JSTime = array of Arr2JSTime;

  JSqlTimestamp = class;
  Arr1JSqlTimestamp = array of JSqlTimestamp;
  Arr2JSqlTimestamp = array of Arr1JSqlTimestamp;
  Arr3JSqlTimestamp = array of Arr2JSqlTimestamp;

  JBPropertyChangeEvent = class;
  Arr1JBPropertyChangeEvent = array of JBPropertyChangeEvent;
  Arr2JBPropertyChangeEvent = array of Arr1JBPropertyChangeEvent;
  Arr3JBPropertyChangeEvent = array of Arr2JBPropertyChangeEvent;

  JUPNodeChangeEvent = class;
  Arr1JUPNodeChangeEvent = array of JUPNodeChangeEvent;
  Arr2JUPNodeChangeEvent = array of Arr1JUPNodeChangeEvent;
  Arr3JUPNodeChangeEvent = array of Arr2JUPNodeChangeEvent;

  JUPPreferenceChangeEvent = class;
  Arr1JUPPreferenceChangeEvent = array of JUPPreferenceChangeEvent;
  Arr2JUPPreferenceChangeEvent = array of Arr1JUPPreferenceChangeEvent;
  Arr3JUPPreferenceChangeEvent = array of Arr2JUPPreferenceChangeEvent;

  JNSHandshakeCompletedEvent = class;
  Arr1JNSHandshakeCompletedEvent = array of JNSHandshakeCompletedEvent;
  Arr2JNSHandshakeCompletedEvent = array of Arr1JNSHandshakeCompletedEvent;
  Arr3JNSHandshakeCompletedEvent = array of Arr2JNSHandshakeCompletedEvent;

  JNSSSLSessionBindingEvent = class;
  Arr1JNSSSLSessionBindingEvent = array of JNSSSLSessionBindingEvent;
  Arr2JNSSSLSessionBindingEvent = array of Arr1JNSSSLSessionBindingEvent;
  Arr3JNSSSLSessionBindingEvent = array of Arr2JNSSSLSessionBindingEvent;

  JSConnectionEvent = class;
  Arr1JSConnectionEvent = array of JSConnectionEvent;
  Arr2JSConnectionEvent = array of Arr1JSConnectionEvent;
  Arr3JSConnectionEvent = array of Arr2JSConnectionEvent;

  JSRowSetEvent = class;
  Arr1JSRowSetEvent = array of JSRowSetEvent;
  Arr2JSRowSetEvent = array of Arr1JSRowSetEvent;
  Arr3JSRowSetEvent = array of Arr2JSRowSetEvent;

  JSStatementEvent = class;
  Arr1JSStatementEvent = array of JSStatementEvent;
  Arr2JSStatementEvent = array of Arr1JSStatementEvent;
  Arr3JSStatementEvent = array of Arr2JSStatementEvent;

  JSSecureRandom = class;
  Arr1JSSecureRandom = array of JSSecureRandom;
  Arr2JSSecureRandom = array of Arr1JSSecureRandom;
  Arr3JSSecureRandom = array of Arr2JSSecureRandom;

  JUSimpleTimeZone = class;
  Arr1JUSimpleTimeZone = array of JUSimpleTimeZone;
  Arr2JUSimpleTimeZone = array of Arr1JUSimpleTimeZone;
  Arr3JUSimpleTimeZone = array of Arr2JUSimpleTimeZone;

  JUCLAbstractQueuedLongSynchronizer = class;
  Arr1JUCLAbstractQueuedLongSynchronizer = array of JUCLAbstractQueuedLongSynchronizer;
  Arr2JUCLAbstractQueuedLongSynchronizer = array of Arr1JUCLAbstractQueuedLongSynchronizer;
  Arr3JUCLAbstractQueuedLongSynchronizer = array of Arr2JUCLAbstractQueuedLongSynchronizer;

  JUCLAbstractQueuedSynchronizer = class;
  Arr1JUCLAbstractQueuedSynchronizer = array of JUCLAbstractQueuedSynchronizer;
  Arr2JUCLAbstractQueuedSynchronizer = array of Arr1JUCLAbstractQueuedSynchronizer;
  Arr3JUCLAbstractQueuedSynchronizer = array of Arr2JUCLAbstractQueuedSynchronizer;

  JNSocketTimeoutException = class;
  Arr1JNSocketTimeoutException = array of JNSocketTimeoutException;
  Arr2JNSocketTimeoutException = array of Arr1JNSocketTimeoutException;
  Arr3JNSocketTimeoutException = array of Arr2JNSocketTimeoutException;

  OAHCConnectTimeoutException = class;
  Arr1OAHCConnectTimeoutException = array of OAHCConnectTimeoutException;
  Arr2OAHCConnectTimeoutException = array of Arr1OAHCConnectTimeoutException;
  Arr3OAHCConnectTimeoutException = array of Arr2OAHCConnectTimeoutException;

  JIInvalidClassException = class;
  Arr1JIInvalidClassException = array of JIInvalidClassException;
  Arr2JIInvalidClassException = array of Arr1JIInvalidClassException;
  Arr3JIInvalidClassException = array of Arr2JIInvalidClassException;

  JIInvalidObjectException = class;
  Arr1JIInvalidObjectException = array of JIInvalidObjectException;
  Arr2JIInvalidObjectException = array of Arr1JIInvalidObjectException;
  Arr3JIInvalidObjectException = array of Arr2JIInvalidObjectException;

  JINotActiveException = class;
  Arr1JINotActiveException = array of JINotActiveException;
  Arr2JINotActiveException = array of Arr1JINotActiveException;
  Arr3JINotActiveException = array of Arr2JINotActiveException;

  JINotSerializableException = class;
  Arr1JINotSerializableException = array of JINotSerializableException;
  Arr2JINotSerializableException = array of Arr1JINotSerializableException;
  Arr3JINotSerializableException = array of Arr2JINotSerializableException;

  JIOptionalDataException = class;
  Arr1JIOptionalDataException = array of JIOptionalDataException;
  Arr2JIOptionalDataException = array of Arr1JIOptionalDataException;
  Arr3JIOptionalDataException = array of Arr2JIOptionalDataException;

  JIStreamCorruptedException = class;
  Arr1JIStreamCorruptedException = array of JIStreamCorruptedException;
  Arr2JIStreamCorruptedException = array of Arr1JIStreamCorruptedException;
  Arr3JIStreamCorruptedException = array of Arr2JIStreamCorruptedException;

  JIWriteAbortedException = class;
  Arr1JIWriteAbortedException = array of JIWriteAbortedException;
  Arr2JIWriteAbortedException = array of Arr1JIWriteAbortedException;
  Arr3JIWriteAbortedException = array of Arr2JIWriteAbortedException;

  JNBindException = class;
  Arr1JNBindException = array of JNBindException;
  Arr2JNBindException = array of Arr1JNBindException;
  Arr3JNBindException = array of Arr2JNBindException;

  JNConnectException = class;
  Arr1JNConnectException = array of JNConnectException;
  Arr2JNConnectException = array of Arr1JNConnectException;
  Arr3JNConnectException = array of Arr2JNConnectException;

  JNNoRouteToHostException = class;
  Arr1JNNoRouteToHostException = array of JNNoRouteToHostException;
  Arr2JNNoRouteToHostException = array of Arr1JNNoRouteToHostException;
  Arr3JNNoRouteToHostException = array of Arr2JNNoRouteToHostException;

  JNPortUnreachableException = class;
  Arr1JNPortUnreachableException = array of JNPortUnreachableException;
  Arr2JNPortUnreachableException = array of Arr1JNPortUnreachableException;
  Arr3JNPortUnreachableException = array of Arr2JNPortUnreachableException;

  JNCAsynchronousCloseException = class;
  Arr1JNCAsynchronousCloseException = array of JNCAsynchronousCloseException;
  Arr2JNCAsynchronousCloseException = array of Arr1JNCAsynchronousCloseException;
  Arr3JNCAsynchronousCloseException = array of Arr2JNCAsynchronousCloseException;

  JUJJarException = class;
  Arr1JUJJarException = array of JUJJarException;
  Arr2JUJJarException = array of Arr1JUJJarException;
  Arr3JUJJarException = array of Arr2JUJJarException;

  JNSSSLHandshakeException = class;
  Arr1JNSSSLHandshakeException = array of JNSSSLHandshakeException;
  Arr2JNSSSLHandshakeException = array of Arr1JNSSSLHandshakeException;
  Arr3JNSSSLHandshakeException = array of Arr2JNSSSLHandshakeException;

  JNSSSLKeyException = class;
  Arr1JNSSSLKeyException = array of JNSSSLKeyException;
  Arr2JNSSSLKeyException = array of Arr1JNSSSLKeyException;
  Arr3JNSSSLKeyException = array of Arr2JNSSSLKeyException;

  JNSSSLPeerUnverifiedException = class;
  Arr1JNSSSLPeerUnverifiedException = array of JNSSSLPeerUnverifiedException;
  Arr2JNSSSLPeerUnverifiedException = array of Arr1JNSSSLPeerUnverifiedException;
  Arr3JNSSSLPeerUnverifiedException = array of Arr2JNSSSLPeerUnverifiedException;

  JNSSSLProtocolException = class;
  Arr1JNSSSLProtocolException = array of JNSSSLProtocolException;
  Arr2JNSSSLProtocolException = array of Arr1JNSSSLProtocolException;
  Arr3JNSSSLProtocolException = array of Arr2JNSSSLProtocolException;

  ATComparisonFailure = class;
  Arr1ATComparisonFailure = array of ATComparisonFailure;
  Arr2ATComparisonFailure = array of Arr1ATComparisonFailure;
  Arr3ATComparisonFailure = array of Arr2ATComparisonFailure;

  JLInternalError = class;
  Arr1JLInternalError = array of JLInternalError;
  Arr2JLInternalError = array of Arr1JLInternalError;
  Arr3JLInternalError = array of Arr2JLInternalError;

  JLOutOfMemoryError = class;
  Arr1JLOutOfMemoryError = array of JLOutOfMemoryError;
  Arr2JLOutOfMemoryError = array of Arr1JLOutOfMemoryError;
  Arr3JLOutOfMemoryError = array of Arr2JLOutOfMemoryError;

  JLStackOverflowError = class;
  Arr1JLStackOverflowError = array of JLStackOverflowError;
  Arr2JLStackOverflowError = array of Arr1JLStackOverflowError;
  Arr3JLStackOverflowError = array of Arr2JLStackOverflowError;

  JLUnknownError = class;
  Arr1JLUnknownError = array of JLUnknownError;
  Arr2JLUnknownError = array of Arr1JLUnknownError;
  Arr3JLUnknownError = array of Arr2JLUnknownError;

  JFComparisonFailure = class;
  Arr1JFComparisonFailure = array of JFComparisonFailure;
  Arr2JFComparisonFailure = array of Arr1JFComparisonFailure;
  Arr3JFComparisonFailure = array of Arr2JFComparisonFailure;

  ATAlteredCharSequence = class;
  Arr1ATAlteredCharSequence = array of ATAlteredCharSequence;
  Arr2ATAlteredCharSequence = array of Arr1ATAlteredCharSequence;
  Arr3ATAlteredCharSequence = array of Arr2ATAlteredCharSequence;

  ATSpannable = interface;
  Arr1ATSpannable = array of ATSpannable;
  Arr2ATSpannable = array of Arr1ATSpannable;
  Arr3ATSpannable = array of Arr2ATSpannable;

  ATSpannedString = class;
  Arr1ATSpannedString = array of ATSpannedString;
  Arr2ATSpannedString = array of Arr1ATSpannedString;
  Arr3ATSpannedString = array of Arr2ATSpannedString;

  JLUnsupportedClassVersionError = class;
  Arr1JLUnsupportedClassVersionError = array of JLUnsupportedClassVersionError;
  Arr2JLUnsupportedClassVersionError = array of Arr1JLUnsupportedClassVersionError;
  Arr3JLUnsupportedClassVersionError = array of Arr2JLUnsupportedClassVersionError;

  JLRGenericSignatureFormatError = class;
  Arr1JLRGenericSignatureFormatError = array of JLRGenericSignatureFormatError;
  Arr2JLRGenericSignatureFormatError = array of Arr1JLRGenericSignatureFormatError;
  Arr3JLRGenericSignatureFormatError = array of Arr2JLRGenericSignatureFormatError;

  JLAbstractMethodError = class;
  Arr1JLAbstractMethodError = array of JLAbstractMethodError;
  Arr2JLAbstractMethodError = array of Arr1JLAbstractMethodError;
  Arr3JLAbstractMethodError = array of Arr2JLAbstractMethodError;

  JLIllegalAccessError = class;
  Arr1JLIllegalAccessError = array of JLIllegalAccessError;
  Arr2JLIllegalAccessError = array of Arr1JLIllegalAccessError;
  Arr3JLIllegalAccessError = array of Arr2JLIllegalAccessError;

  JLInstantiationError = class;
  Arr1JLInstantiationError = array of JLInstantiationError;
  Arr2JLInstantiationError = array of Arr1JLInstantiationError;
  Arr3JLInstantiationError = array of Arr2JLInstantiationError;

  JLNoSuchFieldError = class;
  Arr1JLNoSuchFieldError = array of JLNoSuchFieldError;
  Arr2JLNoSuchFieldError = array of Arr1JLNoSuchFieldError;
  Arr3JLNoSuchFieldError = array of Arr2JLNoSuchFieldError;

  JLNoSuchMethodError = class;
  Arr1JLNoSuchMethodError = array of JLNoSuchMethodError;
  Arr2JLNoSuchMethodError = array of Arr1JLNoSuchMethodError;
  Arr3JLNoSuchMethodError = array of Arr2JLNoSuchMethodError;

  JUDuplicateFormatFlagsException = class;
  Arr1JUDuplicateFormatFlagsException = array of JUDuplicateFormatFlagsException;
  Arr2JUDuplicateFormatFlagsException = array of Arr1JUDuplicateFormatFlagsException;
  Arr3JUDuplicateFormatFlagsException = array of Arr2JUDuplicateFormatFlagsException;

  JUFormatFlagsConversionMismatchException = class;
  Arr1JUFormatFlagsConversionMismatchException = array of JUFormatFlagsConversionMismatchException;
  Arr2JUFormatFlagsConversionMismatchException = array of Arr1JUFormatFlagsConversionMismatchException;
  Arr3JUFormatFlagsConversionMismatchException = array of Arr2JUFormatFlagsConversionMismatchException;

  JUIllegalFormatCodePointException = class;
  Arr1JUIllegalFormatCodePointException = array of JUIllegalFormatCodePointException;
  Arr2JUIllegalFormatCodePointException = array of Arr1JUIllegalFormatCodePointException;
  Arr3JUIllegalFormatCodePointException = array of Arr2JUIllegalFormatCodePointException;

  JUIllegalFormatConversionException = class;
  Arr1JUIllegalFormatConversionException = array of JUIllegalFormatConversionException;
  Arr2JUIllegalFormatConversionException = array of Arr1JUIllegalFormatConversionException;
  Arr3JUIllegalFormatConversionException = array of Arr2JUIllegalFormatConversionException;

  JUIllegalFormatFlagsException = class;
  Arr1JUIllegalFormatFlagsException = array of JUIllegalFormatFlagsException;
  Arr2JUIllegalFormatFlagsException = array of Arr1JUIllegalFormatFlagsException;
  Arr3JUIllegalFormatFlagsException = array of Arr2JUIllegalFormatFlagsException;

  JUIllegalFormatPrecisionException = class;
  Arr1JUIllegalFormatPrecisionException = array of JUIllegalFormatPrecisionException;
  Arr2JUIllegalFormatPrecisionException = array of Arr1JUIllegalFormatPrecisionException;
  Arr3JUIllegalFormatPrecisionException = array of Arr2JUIllegalFormatPrecisionException;

  JUIllegalFormatWidthException = class;
  Arr1JUIllegalFormatWidthException = array of JUIllegalFormatWidthException;
  Arr2JUIllegalFormatWidthException = array of Arr1JUIllegalFormatWidthException;
  Arr3JUIllegalFormatWidthException = array of Arr2JUIllegalFormatWidthException;

  JUMissingFormatArgumentException = class;
  Arr1JUMissingFormatArgumentException = array of JUMissingFormatArgumentException;
  Arr2JUMissingFormatArgumentException = array of Arr1JUMissingFormatArgumentException;
  Arr3JUMissingFormatArgumentException = array of Arr2JUMissingFormatArgumentException;

  JUMissingFormatWidthException = class;
  Arr1JUMissingFormatWidthException = array of JUMissingFormatWidthException;
  Arr2JUMissingFormatWidthException = array of Arr1JUMissingFormatWidthException;
  Arr3JUMissingFormatWidthException = array of Arr2JUMissingFormatWidthException;

  JUUnknownFormatConversionException = class;
  Arr1JUUnknownFormatConversionException = array of JUUnknownFormatConversionException;
  Arr2JUUnknownFormatConversionException = array of Arr1JUUnknownFormatConversionException;
  Arr3JUUnknownFormatConversionException = array of Arr2JUUnknownFormatConversionException;

  JUUnknownFormatFlagsException = class;
  Arr1JUUnknownFormatFlagsException = array of JUUnknownFormatFlagsException;
  Arr2JUUnknownFormatFlagsException = array of Arr1JUUnknownFormatFlagsException;
  Arr3JUUnknownFormatFlagsException = array of Arr2JUUnknownFormatFlagsException;

  JUNavigableMap = interface;
  Arr1JUNavigableMap = array of JUNavigableMap;
  Arr2JUNavigableMap = array of Arr1JUNavigableMap;
  Arr3JUNavigableMap = array of Arr2JUNavigableMap;

  JUCConcurrentHashMap = class;
  Arr1JUCConcurrentHashMap = array of JUCConcurrentHashMap;
  Arr2JUCConcurrentHashMap = array of Arr1JUCConcurrentHashMap;
  Arr3JUCConcurrentHashMap = array of Arr2JUCConcurrentHashMap;

  JULinkedHashSet = class;
  Arr1JULinkedHashSet = array of JULinkedHashSet;
  Arr2JULinkedHashSet = array of Arr1JULinkedHashSet;
  Arr3JULinkedHashSet = array of Arr2JULinkedHashSet;

  JUNavigableSet = interface;
  Arr1JUNavigableSet = array of JUNavigableSet;
  Arr2JUNavigableSet = array of Arr1JUNavigableSet;
  Arr3JUNavigableSet = array of Arr2JUNavigableSet;

  JUAbstractList = class;
  Arr1JUAbstractList = array of JUAbstractList;
  Arr2JUAbstractList = array of Arr1JUAbstractList;
  Arr3JUAbstractList = array of Arr2JUAbstractList;

  JUCCopyOnWriteArrayList = class;
  Arr1JUCCopyOnWriteArrayList = array of JUCCopyOnWriteArrayList;
  Arr2JUCCopyOnWriteArrayList = array of Arr1JUCCopyOnWriteArrayList;
  Arr3JUCCopyOnWriteArrayList = array of Arr2JUCCopyOnWriteArrayList;

  JUAbstractQueue = class;
  Arr1JUAbstractQueue = array of JUAbstractQueue;
  Arr2JUAbstractQueue = array of Arr1JUAbstractQueue;
  Arr3JUAbstractQueue = array of Arr2JUAbstractQueue;

  JUDeque = interface;
  Arr1JUDeque = array of JUDeque;
  Arr2JUDeque = array of Arr1JUDeque;
  Arr3JUDeque = array of Arr2JUDeque;

  JUCBlockingQueue = interface;
  Arr1JUCBlockingQueue = array of JUCBlockingQueue;
  Arr2JUCBlockingQueue = array of Arr1JUCBlockingQueue;
  Arr3JUCBlockingQueue = array of Arr2JUCBlockingQueue;

  AVABaseInterpolator = class;
  Arr1AVABaseInterpolator = array of AVABaseInterpolator;
  Arr2AVABaseInterpolator = array of Arr1AVABaseInterpolator;
  Arr3AVABaseInterpolator = array of Arr2AVABaseInterpolator;

  AVAAccelerateDecelerateInterpolator = class;
  Arr1AVAAccelerateDecelerateInterpolator = array of AVAAccelerateDecelerateInterpolator;
  Arr2AVAAccelerateDecelerateInterpolator = array of Arr1AVAAccelerateDecelerateInterpolator;
  Arr3AVAAccelerateDecelerateInterpolator = array of Arr2AVAAccelerateDecelerateInterpolator;

  AVAAccelerateInterpolator = class;
  Arr1AVAAccelerateInterpolator = array of AVAAccelerateInterpolator;
  Arr2AVAAccelerateInterpolator = array of Arr1AVAAccelerateInterpolator;
  Arr3AVAAccelerateInterpolator = array of Arr2AVAAccelerateInterpolator;

  AVAAnticipateInterpolator = class;
  Arr1AVAAnticipateInterpolator = array of AVAAnticipateInterpolator;
  Arr2AVAAnticipateInterpolator = array of Arr1AVAAnticipateInterpolator;
  Arr3AVAAnticipateInterpolator = array of Arr2AVAAnticipateInterpolator;

  AVAAnticipateOvershootInterpolator = class;
  Arr1AVAAnticipateOvershootInterpolator = array of AVAAnticipateOvershootInterpolator;
  Arr2AVAAnticipateOvershootInterpolator = array of Arr1AVAAnticipateOvershootInterpolator;
  Arr3AVAAnticipateOvershootInterpolator = array of Arr2AVAAnticipateOvershootInterpolator;

  AVABounceInterpolator = class;
  Arr1AVABounceInterpolator = array of AVABounceInterpolator;
  Arr2AVABounceInterpolator = array of Arr1AVABounceInterpolator;
  Arr3AVABounceInterpolator = array of Arr2AVABounceInterpolator;

  AVACycleInterpolator = class;
  Arr1AVACycleInterpolator = array of AVACycleInterpolator;
  Arr2AVACycleInterpolator = array of Arr1AVACycleInterpolator;
  Arr3AVACycleInterpolator = array of Arr2AVACycleInterpolator;

  AVADecelerateInterpolator = class;
  Arr1AVADecelerateInterpolator = array of AVADecelerateInterpolator;
  Arr2AVADecelerateInterpolator = array of Arr1AVADecelerateInterpolator;
  Arr3AVADecelerateInterpolator = array of Arr2AVADecelerateInterpolator;

  AVALinearInterpolator = class;
  Arr1AVALinearInterpolator = array of AVALinearInterpolator;
  Arr2AVALinearInterpolator = array of Arr1AVALinearInterpolator;
  Arr3AVALinearInterpolator = array of Arr2AVALinearInterpolator;

  AVAOvershootInterpolator = class;
  Arr1AVAOvershootInterpolator = array of AVAOvershootInterpolator;
  Arr2AVAOvershootInterpolator = array of Arr1AVAOvershootInterpolator;
  Arr3AVAOvershootInterpolator = array of Arr2AVAOvershootInterpolator;

  ACContentProvider = class;
  Arr1ACContentProvider = array of ACContentProvider;
  Arr2ACContentProvider = array of Arr1ACContentProvider;
  Arr3ACContentProvider = array of Arr2ACContentProvider;

  ACCursorLoader = class;
  Arr1ACCursorLoader = array of ACCursorLoader;
  Arr2ACCursorLoader = array of Arr1ACCursorLoader;
  Arr3ACCursorLoader = array of Arr2ACCursorLoader;

  ADAbstractCursor = class;
  Arr1ADAbstractCursor = array of ADAbstractCursor;
  Arr2ADAbstractCursor = array of Arr1ADAbstractCursor;
  Arr3ADAbstractCursor = array of Arr2ADAbstractCursor;

  ADCrossProcessCursorWrapper = class;
  Arr1ADCrossProcessCursorWrapper = array of ADCrossProcessCursorWrapper;
  Arr2ADCrossProcessCursorWrapper = array of Arr1ADCrossProcessCursorWrapper;
  Arr3ADCrossProcessCursorWrapper = array of Arr2ADCrossProcessCursorWrapper;

  ACContext = class;
  Arr1ACContext = array of ACContext;
  Arr2ACContext = array of Arr1ACContext;
  Arr3ACContext = array of Arr2ACContext;

  ADSSQLiteCursorDriver = interface;
  Arr1ADSSQLiteCursorDriver = array of ADSSQLiteCursorDriver;
  Arr2ADSSQLiteCursorDriver = array of Arr1ADSSQLiteCursorDriver;
  Arr3ADSSQLiteCursorDriver = array of Arr2ADSSQLiteCursorDriver;

  ADSSQLiteOpenHelper = class;
  Arr1ADSSQLiteOpenHelper = array of ADSSQLiteOpenHelper;
  Arr2ADSSQLiteOpenHelper = array of Arr1ADSSQLiteOpenHelper;
  Arr3ADSSQLiteOpenHelper = array of Arr2ADSSQLiteOpenHelper;

  ADSSQLiteQueryBuilder = class;
  Arr1ADSSQLiteQueryBuilder = array of ADSSQLiteQueryBuilder;
  Arr2ADSSQLiteQueryBuilder = array of Arr1ADSSQLiteQueryBuilder;
  Arr3ADSSQLiteQueryBuilder = array of Arr2ADSSQLiteQueryBuilder;

  ADSSQLiteQuery = class;
  Arr1ADSSQLiteQuery = array of ADSSQLiteQuery;
  Arr2ADSSQLiteQuery = array of Arr1ADSSQLiteQuery;
  Arr3ADSSQLiteQuery = array of Arr2ADSSQLiteQuery;

  ADSSQLiteStatement = class;
  Arr1ADSSQLiteStatement = array of ADSSQLiteStatement;
  Arr2ADSSQLiteStatement = array of Arr1ADSSQLiteStatement;
  Arr3ADSSQLiteStatement = array of Arr2ADSSQLiteStatement;

  ACPLabeledIntent = class;
  Arr1ACPLabeledIntent = array of ACPLabeledIntent;
  Arr2ACPLabeledIntent = array of Arr1ACPLabeledIntent;
  Arr3ACPLabeledIntent = array of Arr2ACPLabeledIntent;

  AGBitmapFactory = class;
  Arr1AGBitmapFactory = array of AGBitmapFactory;
  Arr2AGBitmapFactory = array of Arr1AGBitmapFactory;
  Arr3AGBitmapFactory = array of Arr2AGBitmapFactory;

  AGCanvas = class;
  Arr1AGCanvas = array of AGCanvas;
  Arr2AGCanvas = array of Arr1AGCanvas;
  Arr3AGCanvas = array of Arr2AGCanvas;

  ANWWifiInfo = class;
  Arr1ANWWifiInfo = array of ANWWifiInfo;
  Arr2ANWWifiInfo = array of Arr1ANWWifiInfo;
  Arr3ANWWifiInfo = array of Arr2ANWWifiInfo;

  ACContentUris = class;
  Arr1ACContentUris = array of ACContentUris;
  Arr2ACContentUris = array of Arr1ACContentUris;
  Arr3ACContentUris = array of Arr2ACContentUris;

  AAActivityManager = class;
  Arr1AAActivityManager = array of AAActivityManager;
  Arr2AAActivityManager = array of Arr1AAActivityManager;
  Arr3AAActivityManager = array of Arr2AAActivityManager;

  ACPPathPermission = class;
  Arr1ACPPathPermission = array of ACPPathPermission;
  Arr2ACPPathPermission = array of Arr1ACPPathPermission;
  Arr3ACPPathPermission = array of Arr2ACPPathPermission;

  ATAnnotation = class;
  Arr1ATAnnotation = array of ATAnnotation;
  Arr2ATAnnotation = array of Arr1ATAnnotation;
  Arr3ATAnnotation = array of Arr2ATAnnotation;

  ATSAlignmentSpan = interface;
  Arr1ATSAlignmentSpan = array of ATSAlignmentSpan;
  Arr2ATSAlignmentSpan = array of Arr1ATSAlignmentSpan;
  Arr3ATSAlignmentSpan = array of Arr2ATSAlignmentSpan;

  ATSBackgroundColorSpan = class;
  Arr1ATSBackgroundColorSpan = array of ATSBackgroundColorSpan;
  Arr2ATSBackgroundColorSpan = array of Arr1ATSBackgroundColorSpan;
  Arr3ATSBackgroundColorSpan = array of Arr2ATSBackgroundColorSpan;

  ATSEasyEditSpan = class;
  Arr1ATSEasyEditSpan = array of ATSEasyEditSpan;
  Arr2ATSEasyEditSpan = array of Arr1ATSEasyEditSpan;
  Arr3ATSEasyEditSpan = array of Arr2ATSEasyEditSpan;

  ATSForegroundColorSpan = class;
  Arr1ATSForegroundColorSpan = array of ATSForegroundColorSpan;
  Arr2ATSForegroundColorSpan = array of Arr1ATSForegroundColorSpan;
  Arr3ATSForegroundColorSpan = array of Arr2ATSForegroundColorSpan;

  ATSStrikethroughSpan = class;
  Arr1ATSStrikethroughSpan = array of ATSStrikethroughSpan;
  Arr2ATSStrikethroughSpan = array of Arr1ATSStrikethroughSpan;
  Arr3ATSStrikethroughSpan = array of Arr2ATSStrikethroughSpan;

  ATSSuggestionSpan = class;
  Arr1ATSSuggestionSpan = array of ATSSuggestionSpan;
  Arr2ATSSuggestionSpan = array of Arr1ATSSuggestionSpan;
  Arr3ATSSuggestionSpan = array of Arr2ATSSuggestionSpan;

  ATSUnderlineSpan = class;
  Arr1ATSUnderlineSpan = array of ATSUnderlineSpan;
  Arr2ATSUnderlineSpan = array of Arr1ATSUnderlineSpan;
  Arr3ATSUnderlineSpan = array of Arr2ATSUnderlineSpan;

  ATBoringLayout = class;
  Arr1ATBoringLayout = array of ATBoringLayout;
  Arr2ATBoringLayout = array of Arr1ATBoringLayout;
  Arr3ATBoringLayout = array of Arr2ATBoringLayout;

  ATDynamicLayout = class;
  Arr1ATDynamicLayout = array of ATDynamicLayout;
  Arr2ATDynamicLayout = array of Arr1ATDynamicLayout;
  Arr3ATDynamicLayout = array of Arr2ATDynamicLayout;

  ATStaticLayout = class;
  Arr1ATStaticLayout = array of ATStaticLayout;
  Arr2ATStaticLayout = array of Arr1ATStaticLayout;
  Arr3ATStaticLayout = array of Arr2ATStaticLayout;

  APPreference = class;
  Arr1APPreference = array of APPreference;
  Arr2APPreference = array of Arr1APPreference;
  Arr3APPreference = array of Arr2APPreference;

  AVMotionEvent = class;
  Arr1AVMotionEvent = array of AVMotionEvent;
  Arr2AVMotionEvent = array of Arr1AVMotionEvent;
  Arr3AVMotionEvent = array of Arr2AVMotionEvent;

  APCalendarContract = class;
  Arr1APCalendarContract = array of APCalendarContract;
  Arr2APCalendarContract = array of Arr1APCalendarContract;
  Arr3APCalendarContract = array of Arr2APCalendarContract;

  APContactsContract = class;
  Arr1APContactsContract = array of APContactsContract;
  Arr2APContactsContract = array of Arr1APContactsContract;
  Arr3APContactsContract = array of Arr2APContactsContract;

  ARScriptC = class;
  Arr1ARScriptC = array of ARScriptC;
  Arr2ARScriptC = array of Arr1ARScriptC;
  Arr3ARScriptC = array of Arr2ARScriptC;

  ARAllocationAdapter = class;
  Arr1ARAllocationAdapter = array of ARAllocationAdapter;
  Arr2ARAllocationAdapter = array of Arr1ARAllocationAdapter;
  Arr3ARAllocationAdapter = array of Arr2ARAllocationAdapter;

  ATPhoneNumberFormattingTextWatcher = class;
  Arr1ATPhoneNumberFormattingTextWatcher = array of ATPhoneNumberFormattingTextWatcher;
  Arr2ATPhoneNumberFormattingTextWatcher = array of Arr1ATPhoneNumberFormattingTextWatcher;
  Arr3ATPhoneNumberFormattingTextWatcher = array of Arr2ATPhoneNumberFormattingTextWatcher;

  ATMPasswordTransformationMethod = class;
  Arr1ATMPasswordTransformationMethod = array of ATMPasswordTransformationMethod;
  Arr2ATMPasswordTransformationMethod = array of Arr1ATMPasswordTransformationMethod;
  Arr3ATMPasswordTransformationMethod = array of Arr2ATMPasswordTransformationMethod;

  ATMNumberKeyListener = class;
  Arr1ATMNumberKeyListener = array of ATMNumberKeyListener;
  Arr2ATMNumberKeyListener = array of Arr1ATMNumberKeyListener;
  Arr3ATMNumberKeyListener = array of Arr2ATMNumberKeyListener;

  ATMTextKeyListener = class;
  Arr1ATMTextKeyListener = array of ATMTextKeyListener;
  Arr2ATMTextKeyListener = array of Arr1ATMTextKeyListener;
  Arr3ATMTextKeyListener = array of Arr2ATMTextKeyListener;

  ATMArrowKeyMovementMethod = class;
  Arr1ATMArrowKeyMovementMethod = array of ATMArrowKeyMovementMethod;
  Arr2ATMArrowKeyMovementMethod = array of Arr1ATMArrowKeyMovementMethod;
  Arr3ATMArrowKeyMovementMethod = array of Arr2ATMArrowKeyMovementMethod;

  ATMScrollingMovementMethod = class;
  Arr1ATMScrollingMovementMethod = array of ATMScrollingMovementMethod;
  Arr2ATMScrollingMovementMethod = array of Arr1ATMScrollingMovementMethod;
  Arr3ATMScrollingMovementMethod = array of Arr2ATMScrollingMovementMethod;

  ATMHideReturnsTransformationMethod = class;
  Arr1ATMHideReturnsTransformationMethod = array of ATMHideReturnsTransformationMethod;
  Arr2ATMHideReturnsTransformationMethod = array of Arr1ATMHideReturnsTransformationMethod;
  Arr3ATMHideReturnsTransformationMethod = array of Arr2ATMHideReturnsTransformationMethod;

  ATMSingleLineTransformationMethod = class;
  Arr1ATMSingleLineTransformationMethod = array of ATMSingleLineTransformationMethod;
  Arr2ATMSingleLineTransformationMethod = array of Arr1ATMSingleLineTransformationMethod;
  Arr3ATMSingleLineTransformationMethod = array of Arr2ATMSingleLineTransformationMethod;

  ATSLeadingMarginSpan = interface;
  Arr1ATSLeadingMarginSpan = array of ATSLeadingMarginSpan;
  Arr2ATSLeadingMarginSpan = array of Arr1ATSLeadingMarginSpan;
  Arr3ATSLeadingMarginSpan = array of Arr2ATSLeadingMarginSpan;

  ATSLineHeightSpan = interface;
  Arr1ATSLineHeightSpan = array of ATSLineHeightSpan;
  Arr2ATSLineHeightSpan = array of Arr1ATSLineHeightSpan;
  Arr3ATSLineHeightSpan = array of Arr2ATSLineHeightSpan;

  ATSURLSpan = class;
  Arr1ATSURLSpan = array of ATSURLSpan;
  Arr2ATSURLSpan = array of Arr1ATSURLSpan;
  Arr3ATSURLSpan = array of Arr2ATSURLSpan;

  ATSMetricAffectingSpan = class;
  Arr1ATSMetricAffectingSpan = array of ATSMetricAffectingSpan;
  Arr2ATSMetricAffectingSpan = array of Arr1ATSMetricAffectingSpan;
  Arr3ATSMetricAffectingSpan = array of Arr2ATSMetricAffectingSpan;

  AVMenuItem = interface;
  Arr1AVMenuItem = array of AVMenuItem;
  Arr2AVMenuItem = array of Arr1AVMenuItem;
  Arr3AVMenuItem = array of Arr2AVMenuItem;

  AVWindow = class;
  Arr1AVWindow = array of AVWindow;
  Arr2AVWindow = array of Arr1AVWindow;
  Arr3AVWindow = array of Arr2AVWindow;

  AWWrapperListAdapter = interface;
  Arr1AWWrapperListAdapter = array of AWWrapperListAdapter;
  Arr2AWWrapperListAdapter = array of Arr1AWWrapperListAdapter;
  Arr3AWWrapperListAdapter = array of Arr2AWWrapperListAdapter;

  AWBaseAdapter = class;
  Arr1AWBaseAdapter = array of AWBaseAdapter;
  Arr2AWBaseAdapter = array of Arr1AWBaseAdapter;
  Arr3AWBaseAdapter = array of Arr2AWBaseAdapter;

  AWCursorTreeAdapter = class;
  Arr1AWCursorTreeAdapter = array of AWCursorTreeAdapter;
  Arr2AWCursorTreeAdapter = array of Arr1AWCursorTreeAdapter;
  Arr3AWCursorTreeAdapter = array of Arr2AWCursorTreeAdapter;

  AWSimpleExpandableListAdapter = class;
  Arr1AWSimpleExpandableListAdapter = array of AWSimpleExpandableListAdapter;
  Arr2AWSimpleExpandableListAdapter = array of Arr1AWSimpleExpandableListAdapter;
  Arr3AWSimpleExpandableListAdapter = array of Arr2AWSimpleExpandableListAdapter;

  ANTBasicTagTechnology = class;
  Arr1ANTBasicTagTechnology = array of ANTBasicTagTechnology;
  Arr2ANTBasicTagTechnology = array of Arr1ANTBasicTagTechnology;
  Arr3ANTBasicTagTechnology = array of Arr2ANTBasicTagTechnology;

  AABBackupDataInputStream = class;
  Arr1AABBackupDataInputStream = array of AABBackupDataInputStream;
  Arr2AABBackupDataInputStream = array of Arr1AABBackupDataInputStream;
  Arr3AABBackupDataInputStream = array of Arr2AABBackupDataInputStream;

  JIByteArrayInputStream = class;
  Arr1JIByteArrayInputStream = array of JIByteArrayInputStream;
  Arr2JIByteArrayInputStream = array of Arr1JIByteArrayInputStream;
  Arr3JIByteArrayInputStream = array of Arr2JIByteArrayInputStream;

  JIFileInputStream = class;
  Arr1JIFileInputStream = array of JIFileInputStream;
  Arr2JIFileInputStream = array of Arr1JIFileInputStream;
  Arr3JIFileInputStream = array of Arr2JIFileInputStream;

  JIFilterInputStream = class;
  Arr1JIFilterInputStream = array of JIFilterInputStream;
  Arr2JIFilterInputStream = array of Arr1JIFilterInputStream;
  Arr3JIFilterInputStream = array of Arr2JIFilterInputStream;

  JIPipedInputStream = class;
  Arr1JIPipedInputStream = array of JIPipedInputStream;
  Arr2JIPipedInputStream = array of Arr1JIPipedInputStream;
  Arr3JIPipedInputStream = array of Arr2JIPipedInputStream;

  JISequenceInputStream = class;
  Arr1JISequenceInputStream = array of JISequenceInputStream;
  Arr2JISequenceInputStream = array of Arr1JISequenceInputStream;
  Arr3JISequenceInputStream = array of Arr2JISequenceInputStream;

  JIStringBufferInputStream = class;
  Arr1JIStringBufferInputStream = array of JIStringBufferInputStream;
  Arr2JIStringBufferInputStream = array of Arr1JIStringBufferInputStream;
  Arr3JIStringBufferInputStream = array of Arr2JIStringBufferInputStream;

  JIBufferedReader = class;
  Arr1JIBufferedReader = array of JIBufferedReader;
  Arr2JIBufferedReader = array of Arr1JIBufferedReader;
  Arr3JIBufferedReader = array of Arr2JIBufferedReader;

  JICharArrayReader = class;
  Arr1JICharArrayReader = array of JICharArrayReader;
  Arr2JICharArrayReader = array of Arr1JICharArrayReader;
  Arr3JICharArrayReader = array of Arr2JICharArrayReader;

  JIFilterReader = class;
  Arr1JIFilterReader = array of JIFilterReader;
  Arr2JIFilterReader = array of Arr1JIFilterReader;
  Arr3JIFilterReader = array of Arr2JIFilterReader;

  JIInputStreamReader = class;
  Arr1JIInputStreamReader = array of JIInputStreamReader;
  Arr2JIInputStreamReader = array of Arr1JIInputStreamReader;
  Arr3JIInputStreamReader = array of Arr2JIInputStreamReader;

  JIPipedReader = class;
  Arr1JIPipedReader = array of JIPipedReader;
  Arr2JIPipedReader = array of Arr1JIPipedReader;
  Arr3JIPipedReader = array of Arr2JIPipedReader;

  JIStringReader = class;
  Arr1JIStringReader = array of JIStringReader;
  Arr2JIStringReader = array of Arr1JIStringReader;
  Arr3JIStringReader = array of Arr2JIStringReader;

  JNCInterruptibleChannel = interface;
  Arr1JNCInterruptibleChannel = array of JNCInterruptibleChannel;
  Arr2JNCInterruptibleChannel = array of Arr1JNCInterruptibleChannel;
  Arr3JNCInterruptibleChannel = array of Arr2JNCInterruptibleChannel;

  JNCReadableByteChannel = interface;
  Arr1JNCReadableByteChannel = array of JNCReadableByteChannel;
  Arr2JNCReadableByteChannel = array of Arr1JNCReadableByteChannel;
  Arr3JNCReadableByteChannel = array of Arr2JNCReadableByteChannel;

  JNCWritableByteChannel = interface;
  Arr1JNCWritableByteChannel = array of JNCWritableByteChannel;
  Arr2JNCWritableByteChannel = array of Arr1JNCWritableByteChannel;
  Arr3JNCWritableByteChannel = array of Arr2JNCWritableByteChannel;

  JIObjectInputStream = class;
  Arr1JIObjectInputStream = array of JIObjectInputStream;
  Arr2JIObjectInputStream = array of Arr1JIObjectInputStream;
  Arr3JIObjectInputStream = array of Arr2JIObjectInputStream;

  JIByteArrayOutputStream = class;
  Arr1JIByteArrayOutputStream = array of JIByteArrayOutputStream;
  Arr2JIByteArrayOutputStream = array of Arr1JIByteArrayOutputStream;
  Arr3JIByteArrayOutputStream = array of Arr2JIByteArrayOutputStream;

  JIFileOutputStream = class;
  Arr1JIFileOutputStream = array of JIFileOutputStream;
  Arr2JIFileOutputStream = array of Arr1JIFileOutputStream;
  Arr3JIFileOutputStream = array of Arr2JIFileOutputStream;

  JIFilterOutputStream = class;
  Arr1JIFilterOutputStream = array of JIFilterOutputStream;
  Arr2JIFilterOutputStream = array of Arr1JIFilterOutputStream;
  Arr3JIFilterOutputStream = array of Arr2JIFilterOutputStream;

  JIObjectOutputStream = class;
  Arr1JIObjectOutputStream = array of JIObjectOutputStream;
  Arr2JIObjectOutputStream = array of Arr1JIObjectOutputStream;
  Arr3JIObjectOutputStream = array of Arr2JIObjectOutputStream;

  JIPipedOutputStream = class;
  Arr1JIPipedOutputStream = array of JIPipedOutputStream;
  Arr2JIPipedOutputStream = array of Arr1JIPipedOutputStream;
  Arr3JIPipedOutputStream = array of Arr2JIPipedOutputStream;

  JIBufferedWriter = class;
  Arr1JIBufferedWriter = array of JIBufferedWriter;
  Arr2JIBufferedWriter = array of Arr1JIBufferedWriter;
  Arr3JIBufferedWriter = array of Arr2JIBufferedWriter;

  JICharArrayWriter = class;
  Arr1JICharArrayWriter = array of JICharArrayWriter;
  Arr2JICharArrayWriter = array of Arr1JICharArrayWriter;
  Arr3JICharArrayWriter = array of Arr2JICharArrayWriter;

  JIFilterWriter = class;
  Arr1JIFilterWriter = array of JIFilterWriter;
  Arr2JIFilterWriter = array of Arr1JIFilterWriter;
  Arr3JIFilterWriter = array of Arr2JIFilterWriter;

  JIOutputStreamWriter = class;
  Arr1JIOutputStreamWriter = array of JIOutputStreamWriter;
  Arr2JIOutputStreamWriter = array of Arr1JIOutputStreamWriter;
  Arr3JIOutputStreamWriter = array of Arr2JIOutputStreamWriter;

  JIPipedWriter = class;
  Arr1JIPipedWriter = array of JIPipedWriter;
  Arr2JIPipedWriter = array of Arr1JIPipedWriter;
  Arr3JIPipedWriter = array of Arr2JIPipedWriter;

  JIPrintWriter = class;
  Arr1JIPrintWriter = array of JIPrintWriter;
  Arr2JIPrintWriter = array of Arr1JIPrintWriter;
  Arr3JIPrintWriter = array of Arr2JIPrintWriter;

  JIStringWriter = class;
  Arr1JIStringWriter = array of JIStringWriter;
  Arr2JIStringWriter = array of Arr1JIStringWriter;
  Arr3JIStringWriter = array of Arr2JIStringWriter;

  JNURLClassLoader = class;
  Arr1JNURLClassLoader = array of JNURLClassLoader;
  Arr2JNURLClassLoader = array of Arr1JNURLClassLoader;
  Arr3JNURLClassLoader = array of Arr2JNURLClassLoader;

  AWCookieSyncManager = class;
  Arr1AWCookieSyncManager = array of AWCookieSyncManager;
  Arr2AWCookieSyncManager = array of Arr1AWCookieSyncManager;
  Arr3AWCookieSyncManager = array of Arr2AWCookieSyncManager;

  AOHandlerThread = class;
  Arr1AOHandlerThread = array of AOHandlerThread;
  Arr2AOHandlerThread = array of Arr1AOHandlerThread;
  Arr3AOHandlerThread = array of Arr2AOHandlerThread;

  JLThreadGroup = class;
  Arr1JLThreadGroup = array of JLThreadGroup;
  Arr2JLThreadGroup = array of Arr1JLThreadGroup;
  Arr3JLThreadGroup = array of Arr2JLThreadGroup;

  JNSHttpsURLConnection = class;
  Arr1JNSHttpsURLConnection = array of JNSHttpsURLConnection;
  Arr2JNSHttpsURLConnection = array of Arr1JNSHttpsURLConnection;
  Arr3JNSHttpsURLConnection = array of Arr2JNSHttpsURLConnection;

  JIFilePermission = class;
  Arr1JIFilePermission = array of JIFilePermission;
  Arr2JIFilePermission = array of Arr1JIFilePermission;
  Arr3JIFilePermission = array of Arr2JIFilePermission;

  JNSocketPermission = class;
  Arr1JNSocketPermission = array of JNSocketPermission;
  Arr2JNSocketPermission = array of Arr1JNSocketPermission;
  Arr3JNSocketPermission = array of Arr2JNSocketPermission;

  JSAllPermission = class;
  Arr1JSAllPermission = array of JSAllPermission;
  Arr2JSAllPermission = array of Arr1JSAllPermission;
  Arr3JSAllPermission = array of Arr2JSAllPermission;

  JSBasicPermission = class;
  Arr1JSBasicPermission = array of JSBasicPermission;
  Arr2JSBasicPermission = array of Arr1JSBasicPermission;
  Arr3JSBasicPermission = array of Arr2JSBasicPermission;

  JSUnresolvedPermission = class;
  Arr1JSUnresolvedPermission = array of JSUnresolvedPermission;
  Arr2JSUnresolvedPermission = array of Arr1JSUnresolvedPermission;
  Arr3JSUnresolvedPermission = array of Arr2JSUnresolvedPermission;

  JSAPrivateCredentialPermission = class;
  Arr1JSAPrivateCredentialPermission = array of JSAPrivateCredentialPermission;
  Arr2JSAPrivateCredentialPermission = array of Arr1JSAPrivateCredentialPermission;
  Arr3JSAPrivateCredentialPermission = array of Arr2JSAPrivateCredentialPermission;

  JSIdentityScope = class;
  Arr1JSIdentityScope = array of JSIdentityScope;
  Arr2JSIdentityScope = array of Arr1JSIdentityScope;
  Arr3JSIdentityScope = array of Arr2JSIdentityScope;

  JSSigner = class;
  Arr1JSSigner = array of JSSigner;
  Arr2JSSigner = array of Arr1JSSigner;
  Arr3JSSigner = array of Arr2JSSigner;

  JSSPKCS8EncodedKeySpec = class;
  Arr1JSSPKCS8EncodedKeySpec = array of JSSPKCS8EncodedKeySpec;
  Arr2JSSPKCS8EncodedKeySpec = array of Arr1JSSPKCS8EncodedKeySpec;
  Arr3JSSPKCS8EncodedKeySpec = array of Arr2JSSPKCS8EncodedKeySpec;

  JSSX509EncodedKeySpec = class;
  Arr1JSSX509EncodedKeySpec = array of JSSX509EncodedKeySpec;
  Arr2JSSX509EncodedKeySpec = array of Arr1JSSX509EncodedKeySpec;
  Arr3JSSX509EncodedKeySpec = array of Arr2JSSX509EncodedKeySpec;

  JSSRSAMultiPrimePrivateCrtKeySpec = class;
  Arr1JSSRSAMultiPrimePrivateCrtKeySpec = array of JSSRSAMultiPrimePrivateCrtKeySpec;
  Arr2JSSRSAMultiPrimePrivateCrtKeySpec = array of Arr1JSSRSAMultiPrimePrivateCrtKeySpec;
  Arr3JSSRSAMultiPrimePrivateCrtKeySpec = array of Arr2JSSRSAMultiPrimePrivateCrtKeySpec;

  JSSRSAPrivateCrtKeySpec = class;
  Arr1JSSRSAPrivateCrtKeySpec = array of JSSRSAPrivateCrtKeySpec;
  Arr2JSSRSAPrivateCrtKeySpec = array of Arr1JSSRSAPrivateCrtKeySpec;
  Arr3JSSRSAPrivateCrtKeySpec = array of Arr2JSSRSAPrivateCrtKeySpec;

  JSRowSet = interface;
  Arr1JSRowSet = array of JSRowSet;
  Arr2JSRowSet = array of Arr1JSRowSet;
  Arr3JSRowSet = array of Arr2JSRowSet;

  JSRowSetMetaData = interface;
  Arr1JSRowSetMetaData = array of JSRowSetMetaData;
  Arr2JSRowSetMetaData = array of Arr1JSRowSetMetaData;
  Arr3JSRowSetMetaData = array of Arr2JSRowSetMetaData;

  JSPreparedStatement = interface;
  Arr1JSPreparedStatement = array of JSPreparedStatement;
  Arr2JSPreparedStatement = array of Arr1JSPreparedStatement;
  Arr3JSPreparedStatement = array of Arr2JSPreparedStatement;

  JUProperties = class;
  Arr1JUProperties = array of JUProperties;
  Arr2JUProperties = array of Arr1JUProperties;
  Arr3JUProperties = array of Arr2JUProperties;

  JBPropertyChangeListenerProxy = class;
  Arr1JBPropertyChangeListenerProxy = array of JBPropertyChangeListenerProxy;
  Arr2JBPropertyChangeListenerProxy = array of Arr1JBPropertyChangeListenerProxy;
  Arr3JBPropertyChangeListenerProxy = array of Arr2JBPropertyChangeListenerProxy;

  JUCAbstractExecutorService = class;
  Arr1JUCAbstractExecutorService = array of JUCAbstractExecutorService;
  Arr2JUCAbstractExecutorService = array of Arr1JUCAbstractExecutorService;
  Arr3JUCAbstractExecutorService = array of Arr2JUCAbstractExecutorService;

  JUCScheduledExecutorService = interface;
  Arr1JUCScheduledExecutorService = array of JUCScheduledExecutorService;
  Arr2JUCScheduledExecutorService = array of Arr1JUCScheduledExecutorService;
  Arr3JUCScheduledExecutorService = array of Arr2JUCScheduledExecutorService;

  JUCFutureTask = class;
  Arr1JUCFutureTask = array of JUCFutureTask;
  Arr2JUCFutureTask = array of Arr1JUCFutureTask;
  Arr3JUCFutureTask = array of Arr2JUCFutureTask;

  JULConsoleHandler = class;
  Arr1JULConsoleHandler = array of JULConsoleHandler;
  Arr2JULConsoleHandler = array of Arr1JULConsoleHandler;
  Arr3JULConsoleHandler = array of Arr2JULConsoleHandler;

  JULFileHandler = class;
  Arr1JULFileHandler = array of JULFileHandler;
  Arr2JULFileHandler = array of Arr1JULFileHandler;
  Arr3JULFileHandler = array of Arr2JULFileHandler;

  JULSocketHandler = class;
  Arr1JULSocketHandler = array of JULSocketHandler;
  Arr2JULSocketHandler = array of Arr1JULSocketHandler;
  Arr3JULSocketHandler = array of Arr2JULSocketHandler;

  JMKEEGL11 = interface;
  Arr1JMKEEGL11 = array of JMKEEGL11;
  Arr2JMKEEGL11 = array of Arr1JMKEEGL11;
  Arr3JMKEEGL11 = array of Arr2JMKEEGL11;

  JMKOGL11 = interface;
  Arr1JMKOGL11 = array of JMKOGL11;
  Arr2JMKOGL11 = array of Arr1JMKOGL11;
  Arr3JMKOGL11 = array of Arr2JMKOGL11;

  ANSSLCertificateSocketFactory = class;
  Arr1ANSSLCertificateSocketFactory = array of ANSSLCertificateSocketFactory;
  Arr2ANSSLCertificateSocketFactory = array of Arr1ANSSLCertificateSocketFactory;
  Arr3ANSSLCertificateSocketFactory = array of Arr2ANSSLCertificateSocketFactory;

  OAHCSAbstractVerifier = class;
  Arr1OAHCSAbstractVerifier = array of OAHCSAbstractVerifier;
  Arr2OAHCSAbstractVerifier = array of Arr1OAHCSAbstractVerifier;
  Arr3OAHCSAbstractVerifier = array of Arr2OAHCSAbstractVerifier;

  JNSX509ExtendedKeyManager = class;
  Arr1JNSX509ExtendedKeyManager = array of JNSX509ExtendedKeyManager;
  Arr2JNSX509ExtendedKeyManager = array of Arr1JNSX509ExtendedKeyManager;
  Arr3JNSX509ExtendedKeyManager = array of Arr2JNSX509ExtendedKeyManager;

  JSKeyStoreSpi = class;
  Arr1JSKeyStoreSpi = array of JSKeyStoreSpi;
  Arr2JSKeyStoreSpi = array of Arr1JSKeyStoreSpi;
  Arr3JSKeyStoreSpi = array of Arr2JSKeyStoreSpi;

  JNSKeyStoreBuilderParameters = class;
  Arr1JNSKeyStoreBuilderParameters = array of JNSKeyStoreBuilderParameters;
  Arr2JNSKeyStoreBuilderParameters = array of Arr1JNSKeyStoreBuilderParameters;
  Arr3JNSKeyStoreBuilderParameters = array of Arr2JNSKeyStoreBuilderParameters;

  ATAndroidTestCase = class;
  Arr1ATAndroidTestCase = array of ATAndroidTestCase;
  Arr2ATAndroidTestCase = array of Arr1ATAndroidTestCase;
  Arr3ATAndroidTestCase = array of Arr2ATAndroidTestCase;

  ATInstrumentationTestCase = class;
  Arr1ATInstrumentationTestCase = array of ATInstrumentationTestCase;
  Arr2ATInstrumentationTestCase = array of Arr1ATInstrumentationTestCase;
  Arr3ATInstrumentationTestCase = array of Arr2ATInstrumentationTestCase;

  ATSTestSuiteBuilder = class;
  Arr1ATSTestSuiteBuilder = array of ATSTestSuiteBuilder;
  Arr2ATSTestSuiteBuilder = array of Arr1ATSTestSuiteBuilder;
  Arr3ATSTestSuiteBuilder = array of Arr2ATSTestSuiteBuilder;

  ATInstrumentationTestSuite = class;
  Arr1ATInstrumentationTestSuite = array of ATInstrumentationTestSuite;
  Arr2ATInstrumentationTestSuite = array of Arr1ATInstrumentationTestSuite;
  Arr3ATInstrumentationTestSuite = array of Arr2ATInstrumentationTestSuite;

  ATAndroidTestRunner = class;
  Arr1ATAndroidTestRunner = array of ATAndroidTestRunner;
  Arr2ATAndroidTestRunner = array of Arr1ATAndroidTestRunner;
  Arr3ATAndroidTestRunner = array of Arr2ATAndroidTestRunner;

  OAHCSSSLSocketFactory = class;
  Arr1OAHCSSSLSocketFactory = array of OAHCSSSLSocketFactory;
  Arr2OAHCSSSLSocketFactory = array of Arr1OAHCSSSLSocketFactory;
  Arr3OAHCSSSLSocketFactory = array of Arr2OAHCSSSLSocketFactory;

  OWDComment = interface;
  Arr1OWDComment = array of OWDComment;
  Arr2OWDComment = array of Arr1OWDComment;
  Arr3OWDComment = array of Arr2OWDComment;

  OWDText = interface;
  Arr1OWDText = array of OWDText;
  Arr2OWDText = array of Arr1OWDText;
  Arr3OWDText = array of Arr2OWDText;

  OXSEAttributes2Impl = class;
  Arr1OXSEAttributes2Impl = array of OXSEAttributes2Impl;
  Arr2OXSEAttributes2Impl = array of Arr1OXSEAttributes2Impl;
  Arr3OXSEAttributes2Impl = array of Arr2OXSEAttributes2Impl;

  OXSEDefaultHandler2 = class;
  Arr1OXSEDefaultHandler2 = array of OXSEDefaultHandler2;
  Arr2OXSEDefaultHandler2 = array of Arr1OXSEDefaultHandler2;
  Arr3OXSEDefaultHandler2 = array of Arr2OXSEDefaultHandler2;

  OXSELocator2Impl = class;
  Arr1OXSELocator2Impl = array of OXSELocator2Impl;
  Arr2OXSELocator2Impl = array of Arr1OXSELocator2Impl;
  Arr3OXSELocator2Impl = array of Arr2OXSELocator2Impl;

  OXSHXMLFilterImpl = class;
  Arr1OXSHXMLFilterImpl = array of OXSHXMLFilterImpl;
  Arr2OXSHXMLFilterImpl = array of Arr1OXSHXMLFilterImpl;
  Arr3OXSHXMLFilterImpl = array of Arr2OXSHXMLFilterImpl;

  ATMMockPackageManager = class;
  Arr1ATMMockPackageManager = array of ATMMockPackageManager;
  Arr2ATMMockPackageManager = array of Arr1ATMMockPackageManager;
  Arr3ATMMockPackageManager = array of Arr2ATMMockPackageManager;

  AODeadObjectException = class;
  Arr1AODeadObjectException = array of AODeadObjectException;
  Arr2AODeadObjectException = array of Arr1AODeadObjectException;
  Arr3AODeadObjectException = array of Arr2AODeadObjectException;

  AOTransactionTooLargeException = class;
  Arr1AOTransactionTooLargeException = array of AOTransactionTooLargeException;
  Arr2AOTransactionTooLargeException = array of Arr1AOTransactionTooLargeException;
  Arr3AOTransactionTooLargeException = array of Arr2AOTransactionTooLargeException;

  JSInvalidKeyException = class;
  Arr1JSInvalidKeyException = array of JSInvalidKeyException;
  Arr2JSInvalidKeyException = array of Arr1JSInvalidKeyException;
  Arr3JSInvalidKeyException = array of Arr2JSInvalidKeyException;

  JSKeyManagementException = class;
  Arr1JSKeyManagementException = array of JSKeyManagementException;
  Arr2JSKeyManagementException = array of Arr1JSKeyManagementException;
  Arr3JSKeyManagementException = array of Arr2JSKeyManagementException;

  JSUnrecoverableKeyException = class;
  Arr1JSUnrecoverableKeyException = array of JSUnrecoverableKeyException;
  Arr2JSUnrecoverableKeyException = array of Arr1JSUnrecoverableKeyException;
  Arr3JSUnrecoverableKeyException = array of Arr2JSUnrecoverableKeyException;

  JSCCertificateEncodingException = class;
  Arr1JSCCertificateEncodingException = array of JSCCertificateEncodingException;
  Arr2JSCCertificateEncodingException = array of Arr1JSCCertificateEncodingException;
  Arr3JSCCertificateEncodingException = array of Arr2JSCCertificateEncodingException;

  JSCCertificateExpiredException = class;
  Arr1JSCCertificateExpiredException = array of JSCCertificateExpiredException;
  Arr2JSCCertificateExpiredException = array of Arr1JSCCertificateExpiredException;
  Arr3JSCCertificateExpiredException = array of Arr2JSCCertificateExpiredException;

  JSCCertificateNotYetValidException = class;
  Arr1JSCCertificateNotYetValidException = array of JSCCertificateNotYetValidException;
  Arr2JSCCertificateNotYetValidException = array of Arr1JSCCertificateNotYetValidException;
  Arr3JSCCertificateNotYetValidException = array of Arr2JSCCertificateNotYetValidException;

  JSCCertificateParsingException = class;
  Arr1JSCCertificateParsingException = array of JSCCertificateParsingException;
  Arr2JSCCertificateParsingException = array of Arr1JSCCertificateParsingException;
  Arr3JSCCertificateParsingException = array of Arr2JSCCertificateParsingException;

  JXXXPathFunctionException = class;
  Arr1JXXXPathFunctionException = array of JXXXPathFunctionException;
  Arr2JXXXPathFunctionException = array of Arr1JXXXPathFunctionException;
  Arr3JXXXPathFunctionException = array of Arr2JXXXPathFunctionException;

  AAObjectAnimator = class;
  Arr1AAObjectAnimator = array of AAObjectAnimator;
  Arr2AAObjectAnimator = array of Arr1AAObjectAnimator;
  Arr3AAObjectAnimator = array of Arr2AAObjectAnimator;

  AGDSArcShape = class;
  Arr1AGDSArcShape = array of AGDSArcShape;
  Arr2AGDSArcShape = array of Arr1AGDSArcShape;
  Arr3AGDSArcShape = array of Arr2AGDSArcShape;

  AGDSOvalShape = class;
  Arr1AGDSOvalShape = array of AGDSOvalShape;
  Arr2AGDSOvalShape = array of Arr1AGDSOvalShape;
  Arr3AGDSOvalShape = array of Arr2AGDSOvalShape;

  AGDSRoundRectShape = class;
  Arr1AGDSRoundRectShape = array of AGDSRoundRectShape;
  Arr2AGDSRoundRectShape = array of Arr1AGDSRoundRectShape;
  Arr3AGDSRoundRectShape = array of Arr2AGDSRoundRectShape;

  JSCPKIXBuilderParameters = class;
  Arr1JSCPKIXBuilderParameters = array of JSCPKIXBuilderParameters;
  Arr2JSCPKIXBuilderParameters = array of Arr1JSCPKIXBuilderParameters;
  Arr3JSCPKIXBuilderParameters = array of Arr2JSCPKIXBuilderParameters;

  JSCPKIXCertPathBuilderResult = class;
  Arr1JSCPKIXCertPathBuilderResult = array of JSCPKIXCertPathBuilderResult;
  Arr2JSCPKIXCertPathBuilderResult = array of Arr1JSCPKIXCertPathBuilderResult;
  Arr3JSCPKIXCertPathBuilderResult = array of Arr2JSCPKIXCertPathBuilderResult;

  JTAttributedString = class;
  Arr1JTAttributedString = array of JTAttributedString;
  Arr2JTAttributedString = array of Arr1JTAttributedString;
  Arr3JTAttributedString = array of Arr2JTAttributedString;

  JTFormat = class;
  Arr1JTFormat = array of JTFormat;
  Arr2JTFormat = array of Arr1JTFormat;
  Arr3JTFormat = array of Arr2JTFormat;

  ADSSQLiteAbortException = class;
  Arr1ADSSQLiteAbortException = array of ADSSQLiteAbortException;
  Arr2ADSSQLiteAbortException = array of Arr1ADSSQLiteAbortException;
  Arr3ADSSQLiteAbortException = array of Arr2ADSSQLiteAbortException;

  ADSSQLiteAccessPermException = class;
  Arr1ADSSQLiteAccessPermException = array of ADSSQLiteAccessPermException;
  Arr2ADSSQLiteAccessPermException = array of Arr1ADSSQLiteAccessPermException;
  Arr3ADSSQLiteAccessPermException = array of Arr2ADSSQLiteAccessPermException;

  ADSSQLiteBindOrColumnIndexOutOfRangeException = class;
  Arr1ADSSQLiteBindOrColumnIndexOutOfRangeException = array of ADSSQLiteBindOrColumnIndexOutOfRangeException;
  Arr2ADSSQLiteBindOrColumnIndexOutOfRangeException = array of Arr1ADSSQLiteBindOrColumnIndexOutOfRangeException;
  Arr3ADSSQLiteBindOrColumnIndexOutOfRangeException = array of Arr2ADSSQLiteBindOrColumnIndexOutOfRangeException;

  ADSSQLiteBlobTooBigException = class;
  Arr1ADSSQLiteBlobTooBigException = array of ADSSQLiteBlobTooBigException;
  Arr2ADSSQLiteBlobTooBigException = array of Arr1ADSSQLiteBlobTooBigException;
  Arr3ADSSQLiteBlobTooBigException = array of Arr2ADSSQLiteBlobTooBigException;

  ADSSQLiteCantOpenDatabaseException = class;
  Arr1ADSSQLiteCantOpenDatabaseException = array of ADSSQLiteCantOpenDatabaseException;
  Arr2ADSSQLiteCantOpenDatabaseException = array of Arr1ADSSQLiteCantOpenDatabaseException;
  Arr3ADSSQLiteCantOpenDatabaseException = array of Arr2ADSSQLiteCantOpenDatabaseException;

  ADSSQLiteConstraintException = class;
  Arr1ADSSQLiteConstraintException = array of ADSSQLiteConstraintException;
  Arr2ADSSQLiteConstraintException = array of Arr1ADSSQLiteConstraintException;
  Arr3ADSSQLiteConstraintException = array of Arr2ADSSQLiteConstraintException;

  ADSSQLiteDatabaseCorruptException = class;
  Arr1ADSSQLiteDatabaseCorruptException = array of ADSSQLiteDatabaseCorruptException;
  Arr2ADSSQLiteDatabaseCorruptException = array of Arr1ADSSQLiteDatabaseCorruptException;
  Arr3ADSSQLiteDatabaseCorruptException = array of Arr2ADSSQLiteDatabaseCorruptException;

  ADSSQLiteDatabaseLockedException = class;
  Arr1ADSSQLiteDatabaseLockedException = array of ADSSQLiteDatabaseLockedException;
  Arr2ADSSQLiteDatabaseLockedException = array of Arr1ADSSQLiteDatabaseLockedException;
  Arr3ADSSQLiteDatabaseLockedException = array of Arr2ADSSQLiteDatabaseLockedException;

  ADSSQLiteDatatypeMismatchException = class;
  Arr1ADSSQLiteDatatypeMismatchException = array of ADSSQLiteDatatypeMismatchException;
  Arr2ADSSQLiteDatatypeMismatchException = array of Arr1ADSSQLiteDatatypeMismatchException;
  Arr3ADSSQLiteDatatypeMismatchException = array of Arr2ADSSQLiteDatatypeMismatchException;

  ADSSQLiteDiskIOException = class;
  Arr1ADSSQLiteDiskIOException = array of ADSSQLiteDiskIOException;
  Arr2ADSSQLiteDiskIOException = array of Arr1ADSSQLiteDiskIOException;
  Arr3ADSSQLiteDiskIOException = array of Arr2ADSSQLiteDiskIOException;

  ADSSQLiteDoneException = class;
  Arr1ADSSQLiteDoneException = array of ADSSQLiteDoneException;
  Arr2ADSSQLiteDoneException = array of Arr1ADSSQLiteDoneException;
  Arr3ADSSQLiteDoneException = array of Arr2ADSSQLiteDoneException;

  ADSSQLiteFullException = class;
  Arr1ADSSQLiteFullException = array of ADSSQLiteFullException;
  Arr2ADSSQLiteFullException = array of Arr1ADSSQLiteFullException;
  Arr3ADSSQLiteFullException = array of Arr2ADSSQLiteFullException;

  ADSSQLiteMisuseException = class;
  Arr1ADSSQLiteMisuseException = array of ADSSQLiteMisuseException;
  Arr2ADSSQLiteMisuseException = array of Arr1ADSSQLiteMisuseException;
  Arr3ADSSQLiteMisuseException = array of Arr2ADSSQLiteMisuseException;

  ADSSQLiteOutOfMemoryException = class;
  Arr1ADSSQLiteOutOfMemoryException = array of ADSSQLiteOutOfMemoryException;
  Arr2ADSSQLiteOutOfMemoryException = array of Arr1ADSSQLiteOutOfMemoryException;
  Arr3ADSSQLiteOutOfMemoryException = array of Arr2ADSSQLiteOutOfMemoryException;

  ADSSQLiteReadOnlyDatabaseException = class;
  Arr1ADSSQLiteReadOnlyDatabaseException = array of ADSSQLiteReadOnlyDatabaseException;
  Arr2ADSSQLiteReadOnlyDatabaseException = array of Arr1ADSSQLiteReadOnlyDatabaseException;
  Arr3ADSSQLiteReadOnlyDatabaseException = array of Arr2ADSSQLiteReadOnlyDatabaseException;

  ADSSQLiteTableLockedException = class;
  Arr1ADSSQLiteTableLockedException = array of ADSSQLiteTableLockedException;
  Arr2ADSSQLiteTableLockedException = array of Arr1ADSSQLiteTableLockedException;
  Arr3ADSSQLiteTableLockedException = array of Arr2ADSSQLiteTableLockedException;

  AVKeyEvent = class;
  Arr1AVKeyEvent = array of AVKeyEvent;
  Arr2AVKeyEvent = array of Arr1AVKeyEvent;
  Arr3AVKeyEvent = array of Arr2AVKeyEvent;

  JUCRunnableScheduledFuture = interface;
  Arr1JUCRunnableScheduledFuture = array of JUCRunnableScheduledFuture;
  Arr2JUCRunnableScheduledFuture = array of Arr1JUCRunnableScheduledFuture;
  Arr3JUCRunnableScheduledFuture = array of Arr2JUCRunnableScheduledFuture;

  ACPActivityInfo = class;
  Arr1ACPActivityInfo = array of ACPActivityInfo;
  Arr2ACPActivityInfo = array of Arr1ACPActivityInfo;
  Arr3ACPActivityInfo = array of Arr2ACPActivityInfo;

  ACPProviderInfo = class;
  Arr1ACPProviderInfo = array of ACPProviderInfo;
  Arr2ACPProviderInfo = array of Arr1ACPProviderInfo;
  Arr3ACPProviderInfo = array of Arr2ACPProviderInfo;

  ACPServiceInfo = class;
  Arr1ACPServiceInfo = array of ACPServiceInfo;
  Arr2ACPServiceInfo = array of Arr1ACPServiceInfo;
  Arr3ACPServiceInfo = array of Arr2ACPServiceInfo;

  JSIDSAPrivateKey = interface;
  Arr1JSIDSAPrivateKey = array of JSIDSAPrivateKey;
  Arr2JSIDSAPrivateKey = array of Arr1JSIDSAPrivateKey;
  Arr3JSIDSAPrivateKey = array of Arr2JSIDSAPrivateKey;

  JSIECPrivateKey = interface;
  Arr1JSIECPrivateKey = array of JSIECPrivateKey;
  Arr2JSIECPrivateKey = array of Arr1JSIECPrivateKey;
  Arr3JSIECPrivateKey = array of Arr2JSIECPrivateKey;

  JSIRSAPrivateKey = interface;
  Arr1JSIRSAPrivateKey = array of JSIRSAPrivateKey;
  Arr2JSIRSAPrivateKey = array of Arr1JSIRSAPrivateKey;
  Arr3JSIRSAPrivateKey = array of Arr2JSIRSAPrivateKey;

  JCIDHPrivateKey = interface;
  Arr1JCIDHPrivateKey = array of JCIDHPrivateKey;
  Arr2JCIDHPrivateKey = array of Arr1JCIDHPrivateKey;
  Arr3JCIDHPrivateKey = array of Arr2JCIDHPrivateKey;

  JSIDSAPublicKey = interface;
  Arr1JSIDSAPublicKey = array of JSIDSAPublicKey;
  Arr2JSIDSAPublicKey = array of Arr1JSIDSAPublicKey;
  Arr3JSIDSAPublicKey = array of Arr2JSIDSAPublicKey;

  JSIECPublicKey = interface;
  Arr1JSIECPublicKey = array of JSIECPublicKey;
  Arr2JSIECPublicKey = array of Arr1JSIECPublicKey;
  Arr3JSIECPublicKey = array of Arr2JSIECPublicKey;

  JSIRSAPublicKey = interface;
  Arr1JSIRSAPublicKey = array of JSIRSAPublicKey;
  Arr2JSIRSAPublicKey = array of Arr1JSIRSAPublicKey;
  Arr3JSIRSAPublicKey = array of Arr2JSIRSAPublicKey;

  JCIDHPublicKey = interface;
  Arr1JCIDHPublicKey = array of JCIDHPublicKey;
  Arr2JCIDHPublicKey = array of Arr1JCIDHPublicKey;
  Arr3JCIDHPublicKey = array of Arr2JCIDHPublicKey;

  JCIPBEKey = interface;
  Arr1JCIPBEKey = array of JCIPBEKey;
  Arr2JCIPBEKey = array of Arr1JCIPBEKey;
  Arr3JCIPBEKey = array of Arr2JCIPBEKey;

  JCSSecretKeySpec = class;
  Arr1JCSSecretKeySpec = array of JCSSecretKeySpec;
  Arr2JCSSecretKeySpec = array of Arr1JCSSecretKeySpec;
  Arr3JCSSecretKeySpec = array of Arr2JCSSecretKeySpec;

  JSSQLDataException = class;
  Arr1JSSQLDataException = array of JSSQLDataException;
  Arr2JSSQLDataException = array of Arr1JSSQLDataException;
  Arr3JSSQLDataException = array of Arr2JSSQLDataException;

  JSSQLFeatureNotSupportedException = class;
  Arr1JSSQLFeatureNotSupportedException = array of JSSQLFeatureNotSupportedException;
  Arr2JSSQLFeatureNotSupportedException = array of Arr1JSSQLFeatureNotSupportedException;
  Arr3JSSQLFeatureNotSupportedException = array of Arr2JSSQLFeatureNotSupportedException;

  JSSQLIntegrityConstraintViolationException = class;
  Arr1JSSQLIntegrityConstraintViolationException = array of JSSQLIntegrityConstraintViolationException;
  Arr2JSSQLIntegrityConstraintViolationException = array of Arr1JSSQLIntegrityConstraintViolationException;
  Arr3JSSQLIntegrityConstraintViolationException = array of Arr2JSSQLIntegrityConstraintViolationException;

  JSSQLInvalidAuthorizationSpecException = class;
  Arr1JSSQLInvalidAuthorizationSpecException = array of JSSQLInvalidAuthorizationSpecException;
  Arr2JSSQLInvalidAuthorizationSpecException = array of Arr1JSSQLInvalidAuthorizationSpecException;
  Arr3JSSQLInvalidAuthorizationSpecException = array of Arr2JSSQLInvalidAuthorizationSpecException;

  JSSQLNonTransientConnectionException = class;
  Arr1JSSQLNonTransientConnectionException = array of JSSQLNonTransientConnectionException;
  Arr2JSSQLNonTransientConnectionException = array of Arr1JSSQLNonTransientConnectionException;
  Arr3JSSQLNonTransientConnectionException = array of Arr2JSSQLNonTransientConnectionException;

  JSSQLSyntaxErrorException = class;
  Arr1JSSQLSyntaxErrorException = array of JSSQLSyntaxErrorException;
  Arr2JSSQLSyntaxErrorException = array of Arr1JSSQLSyntaxErrorException;
  Arr3JSSQLSyntaxErrorException = array of Arr2JSSQLSyntaxErrorException;

  JSSQLTimeoutException = class;
  Arr1JSSQLTimeoutException = array of JSSQLTimeoutException;
  Arr2JSSQLTimeoutException = array of Arr1JSSQLTimeoutException;
  Arr3JSSQLTimeoutException = array of Arr2JSSQLTimeoutException;

  JSSQLTransactionRollbackException = class;
  Arr1JSSQLTransactionRollbackException = array of JSSQLTransactionRollbackException;
  Arr2JSSQLTransactionRollbackException = array of Arr1JSSQLTransactionRollbackException;
  Arr3JSSQLTransactionRollbackException = array of Arr2JSSQLTransactionRollbackException;

  JSSQLTransientConnectionException = class;
  Arr1JSSQLTransientConnectionException = array of JSSQLTransientConnectionException;
  Arr2JSSQLTransientConnectionException = array of Arr1JSSQLTransientConnectionException;
  Arr3JSSQLTransientConnectionException = array of Arr2JSSQLTransientConnectionException;

  JSDataTruncation = class;
  Arr1JSDataTruncation = array of JSDataTruncation;
  Arr2JSDataTruncation = array of Arr1JSDataTruncation;
  Arr3JSDataTruncation = array of Arr2JSDataTruncation;

  JBIndexedPropertyChangeEvent = class;
  Arr1JBIndexedPropertyChangeEvent = array of JBIndexedPropertyChangeEvent;
  Arr2JBIndexedPropertyChangeEvent = array of Arr1JBIndexedPropertyChangeEvent;
  Arr3JBIndexedPropertyChangeEvent = array of Arr2JBIndexedPropertyChangeEvent;

  JNCClosedByInterruptException = class;
  Arr1JNCClosedByInterruptException = array of JNCClosedByInterruptException;
  Arr2JNCClosedByInterruptException = array of Arr1JNCClosedByInterruptException;
  Arr3JNCClosedByInterruptException = array of Arr2JNCClosedByInterruptException;

  JUZZipError = class;
  Arr1JUZZipError = array of JUZZipError;
  Arr2JUZZipError = array of Arr1JUZZipError;
  Arr3JUZZipError = array of Arr2JUZZipError;

  ATEditable = interface;
  Arr1ATEditable = array of ATEditable;
  Arr2ATEditable = array of Arr1ATEditable;
  Arr3ATEditable = array of Arr2ATEditable;

  ATSpannableString = class;
  Arr1ATSpannableString = array of ATSpannableString;
  Arr2ATSpannableString = array of Arr1ATSpannableString;
  Arr3ATSpannableString = array of Arr2ATSpannableString;

  JUTreeMap = class;
  Arr1JUTreeMap = array of JUTreeMap;
  Arr2JUTreeMap = array of Arr1JUTreeMap;
  Arr3JUTreeMap = array of Arr2JUTreeMap;

  JUCConcurrentNavigableMap = interface;
  Arr1JUCConcurrentNavigableMap = array of JUCConcurrentNavigableMap;
  Arr2JUCConcurrentNavigableMap = array of Arr1JUCConcurrentNavigableMap;
  Arr3JUCConcurrentNavigableMap = array of Arr2JUCConcurrentNavigableMap;

  JUTreeSet = class;
  Arr1JUTreeSet = array of JUTreeSet;
  Arr2JUTreeSet = array of Arr1JUTreeSet;
  Arr3JUTreeSet = array of Arr2JUTreeSet;

  JUCConcurrentSkipListSet = class;
  Arr1JUCConcurrentSkipListSet = array of JUCConcurrentSkipListSet;
  Arr2JUCConcurrentSkipListSet = array of Arr1JUCConcurrentSkipListSet;
  Arr3JUCConcurrentSkipListSet = array of Arr2JUCConcurrentSkipListSet;

  JUAbstractSequentialList = class;
  Arr1JUAbstractSequentialList = array of JUAbstractSequentialList;
  Arr2JUAbstractSequentialList = array of Arr1JUAbstractSequentialList;
  Arr3JUAbstractSequentialList = array of Arr2JUAbstractSequentialList;

  JUArrayList = class;
  Arr1JUArrayList = array of JUArrayList;
  Arr2JUArrayList = array of Arr1JUArrayList;
  Arr3JUArrayList = array of Arr2JUArrayList;

  JUVector = class;
  Arr1JUVector = array of JUVector;
  Arr2JUVector = array of Arr1JUVector;
  Arr3JUVector = array of Arr2JUVector;

  JUPriorityQueue = class;
  Arr1JUPriorityQueue = array of JUPriorityQueue;
  Arr2JUPriorityQueue = array of Arr1JUPriorityQueue;
  Arr3JUPriorityQueue = array of Arr2JUPriorityQueue;

  JUCConcurrentLinkedQueue = class;
  Arr1JUCConcurrentLinkedQueue = array of JUCConcurrentLinkedQueue;
  Arr2JUCConcurrentLinkedQueue = array of Arr1JUCConcurrentLinkedQueue;
  Arr3JUCConcurrentLinkedQueue = array of Arr2JUCConcurrentLinkedQueue;

  JUArrayDeque = class;
  Arr1JUArrayDeque = array of JUArrayDeque;
  Arr2JUArrayDeque = array of Arr1JUArrayDeque;
  Arr3JUArrayDeque = array of Arr2JUArrayDeque;

  JUCArrayBlockingQueue = class;
  Arr1JUCArrayBlockingQueue = array of JUCArrayBlockingQueue;
  Arr2JUCArrayBlockingQueue = array of Arr1JUCArrayBlockingQueue;
  Arr3JUCArrayBlockingQueue = array of Arr2JUCArrayBlockingQueue;

  JUCBlockingDeque = interface;
  Arr1JUCBlockingDeque = array of JUCBlockingDeque;
  Arr2JUCBlockingDeque = array of Arr1JUCBlockingDeque;
  Arr3JUCBlockingDeque = array of Arr2JUCBlockingDeque;

  JUCDelayQueue = class;
  Arr1JUCDelayQueue = array of JUCDelayQueue;
  Arr2JUCDelayQueue = array of Arr1JUCDelayQueue;
  Arr3JUCDelayQueue = array of Arr2JUCDelayQueue;

  JUCLinkedBlockingQueue = class;
  Arr1JUCLinkedBlockingQueue = array of JUCLinkedBlockingQueue;
  Arr2JUCLinkedBlockingQueue = array of Arr1JUCLinkedBlockingQueue;
  Arr3JUCLinkedBlockingQueue = array of Arr2JUCLinkedBlockingQueue;

  JUCPriorityBlockingQueue = class;
  Arr1JUCPriorityBlockingQueue = array of JUCPriorityBlockingQueue;
  Arr2JUCPriorityBlockingQueue = array of Arr1JUCPriorityBlockingQueue;
  Arr3JUCPriorityBlockingQueue = array of Arr2JUCPriorityBlockingQueue;

  JUCSynchronousQueue = class;
  Arr1JUCSynchronousQueue = array of JUCSynchronousQueue;
  Arr2JUCSynchronousQueue = array of Arr1JUCSynchronousQueue;
  Arr3JUCSynchronousQueue = array of Arr2JUCSynchronousQueue;

  ACSearchRecentSuggestionsProvider = class;
  Arr1ACSearchRecentSuggestionsProvider = array of ACSearchRecentSuggestionsProvider;
  Arr2ACSearchRecentSuggestionsProvider = array of Arr1ACSearchRecentSuggestionsProvider;
  Arr3ACSearchRecentSuggestionsProvider = array of Arr2ACSearchRecentSuggestionsProvider;

  ATMMockContentProvider = class;
  Arr1ATMMockContentProvider = array of ATMMockContentProvider;
  Arr2ATMMockContentProvider = array of Arr1ATMMockContentProvider;
  Arr3ATMMockContentProvider = array of Arr2ATMMockContentProvider;

  ADAbstractWindowedCursor = class;
  Arr1ADAbstractWindowedCursor = array of ADAbstractWindowedCursor;
  Arr2ADAbstractWindowedCursor = array of Arr1ADAbstractWindowedCursor;
  Arr3ADAbstractWindowedCursor = array of Arr2ADAbstractWindowedCursor;

  ADMatrixCursor = class;
  Arr1ADMatrixCursor = array of ADMatrixCursor;
  Arr2ADMatrixCursor = array of Arr1ADMatrixCursor;
  Arr3ADMatrixCursor = array of Arr2ADMatrixCursor;

  ADMergeCursor = class;
  Arr1ADMergeCursor = array of ADMergeCursor;
  Arr2ADMergeCursor = array of Arr1ADMergeCursor;
  Arr3ADMergeCursor = array of Arr2ADMergeCursor;

  ACContextWrapper = class;
  Arr1ACContextWrapper = array of ACContextWrapper;
  Arr2ACContextWrapper = array of Arr1ACContextWrapper;
  Arr3ACContextWrapper = array of Arr2ACContextWrapper;

  ATMMockContext = class;
  Arr1ATMMockContext = array of ATMMockContext;
  Arr2ATMMockContext = array of Arr1ATMMockContext;
  Arr3ATMMockContext = array of Arr2ATMMockContext;

  AGBitmapRegionDecoder = class;
  Arr1AGBitmapRegionDecoder = array of AGBitmapRegionDecoder;
  Arr2AGBitmapRegionDecoder = array of Arr1AGBitmapRegionDecoder;
  Arr3AGBitmapRegionDecoder = array of Arr2AGBitmapRegionDecoder;

  AGDDrawable = class;
  Arr1AGDDrawable = array of AGDDrawable;
  Arr2AGDDrawable = array of Arr1AGDDrawable;
  Arr3AGDDrawable = array of Arr2AGDDrawable;

  APContacts = class;
  Arr1APContacts = array of APContacts;
  Arr2APContacts = array of Arr1APContacts;
  Arr3APContacts = array of Arr2APContacts;

  APMediaStore = class;
  Arr1APMediaStore = array of APMediaStore;
  Arr2APMediaStore = array of Arr1APMediaStore;
  Arr3APMediaStore = array of Arr2APMediaStore;

  APPreferenceGroup = class;
  Arr1APPreferenceGroup = array of APPreferenceGroup;
  Arr2APPreferenceGroup = array of Arr1APPreferenceGroup;
  Arr3APPreferenceGroup = array of Arr2APPreferenceGroup;

  APRingtonePreference = class;
  Arr1APRingtonePreference = array of APRingtonePreference;
  Arr2APRingtonePreference = array of Arr1APRingtonePreference;
  Arr3APRingtonePreference = array of Arr2APRingtonePreference;

  APTwoStatePreference = class;
  Arr1APTwoStatePreference = array of APTwoStatePreference;
  Arr2APTwoStatePreference = array of Arr1APTwoStatePreference;
  Arr3APTwoStatePreference = array of Arr2APTwoStatePreference;

  ATMDateKeyListener = class;
  Arr1ATMDateKeyListener = array of ATMDateKeyListener;
  Arr2ATMDateKeyListener = array of Arr1ATMDateKeyListener;
  Arr3ATMDateKeyListener = array of Arr2ATMDateKeyListener;

  ATMDateTimeKeyListener = class;
  Arr1ATMDateTimeKeyListener = array of ATMDateTimeKeyListener;
  Arr2ATMDateTimeKeyListener = array of Arr1ATMDateTimeKeyListener;
  Arr3ATMDateTimeKeyListener = array of Arr2ATMDateTimeKeyListener;

  ATMDialerKeyListener = class;
  Arr1ATMDialerKeyListener = array of ATMDialerKeyListener;
  Arr2ATMDialerKeyListener = array of Arr1ATMDialerKeyListener;
  Arr3ATMDialerKeyListener = array of Arr2ATMDialerKeyListener;

  ATMDigitsKeyListener = class;
  Arr1ATMDigitsKeyListener = array of ATMDigitsKeyListener;
  Arr2ATMDigitsKeyListener = array of Arr1ATMDigitsKeyListener;
  Arr3ATMDigitsKeyListener = array of Arr2ATMDigitsKeyListener;

  ATMTimeKeyListener = class;
  Arr1ATMTimeKeyListener = array of ATMTimeKeyListener;
  Arr2ATMTimeKeyListener = array of Arr1ATMTimeKeyListener;
  Arr3ATMTimeKeyListener = array of Arr2ATMTimeKeyListener;

  ATMMultiTapKeyListener = class;
  Arr1ATMMultiTapKeyListener = array of ATMMultiTapKeyListener;
  Arr2ATMMultiTapKeyListener = array of Arr1ATMMultiTapKeyListener;
  Arr3ATMMultiTapKeyListener = array of Arr2ATMMultiTapKeyListener;

  ATMQwertyKeyListener = class;
  Arr1ATMQwertyKeyListener = array of ATMQwertyKeyListener;
  Arr2ATMQwertyKeyListener = array of Arr1ATMQwertyKeyListener;
  Arr3ATMQwertyKeyListener = array of Arr2ATMQwertyKeyListener;

  ATMLinkMovementMethod = class;
  Arr1ATMLinkMovementMethod = array of ATMLinkMovementMethod;
  Arr2ATMLinkMovementMethod = array of Arr1ATMLinkMovementMethod;
  Arr3ATMLinkMovementMethod = array of Arr2ATMLinkMovementMethod;

  ATSBulletSpan = class;
  Arr1ATSBulletSpan = array of ATSBulletSpan;
  Arr2ATSBulletSpan = array of Arr1ATSBulletSpan;
  Arr3ATSBulletSpan = array of Arr2ATSBulletSpan;

  ATSQuoteSpan = class;
  Arr1ATSQuoteSpan = array of ATSQuoteSpan;
  Arr2ATSQuoteSpan = array of Arr1ATSQuoteSpan;
  Arr3ATSQuoteSpan = array of Arr2ATSQuoteSpan;

  ATSDrawableMarginSpan = class;
  Arr1ATSDrawableMarginSpan = array of ATSDrawableMarginSpan;
  Arr2ATSDrawableMarginSpan = array of Arr1ATSDrawableMarginSpan;
  Arr3ATSDrawableMarginSpan = array of Arr2ATSDrawableMarginSpan;

  ATSIconMarginSpan = class;
  Arr1ATSIconMarginSpan = array of ATSIconMarginSpan;
  Arr2ATSIconMarginSpan = array of Arr1ATSIconMarginSpan;
  Arr3ATSIconMarginSpan = array of Arr2ATSIconMarginSpan;

  ATSAbsoluteSizeSpan = class;
  Arr1ATSAbsoluteSizeSpan = array of ATSAbsoluteSizeSpan;
  Arr2ATSAbsoluteSizeSpan = array of Arr1ATSAbsoluteSizeSpan;
  Arr3ATSAbsoluteSizeSpan = array of Arr2ATSAbsoluteSizeSpan;

  ATSRelativeSizeSpan = class;
  Arr1ATSRelativeSizeSpan = array of ATSRelativeSizeSpan;
  Arr2ATSRelativeSizeSpan = array of Arr1ATSRelativeSizeSpan;
  Arr3ATSRelativeSizeSpan = array of Arr2ATSRelativeSizeSpan;

  ATSReplacementSpan = class;
  Arr1ATSReplacementSpan = array of ATSReplacementSpan;
  Arr2ATSReplacementSpan = array of Arr1ATSReplacementSpan;
  Arr3ATSReplacementSpan = array of Arr2ATSReplacementSpan;

  ATSScaleXSpan = class;
  Arr1ATSScaleXSpan = array of ATSScaleXSpan;
  Arr2ATSScaleXSpan = array of Arr1ATSScaleXSpan;
  Arr3ATSScaleXSpan = array of Arr2ATSScaleXSpan;

  ATSStyleSpan = class;
  Arr1ATSStyleSpan = array of ATSStyleSpan;
  Arr2ATSStyleSpan = array of Arr1ATSStyleSpan;
  Arr3ATSStyleSpan = array of Arr2ATSStyleSpan;

  ATSSubscriptSpan = class;
  Arr1ATSSubscriptSpan = array of ATSSubscriptSpan;
  Arr2ATSSubscriptSpan = array of Arr1ATSSubscriptSpan;
  Arr3ATSSubscriptSpan = array of Arr2ATSSubscriptSpan;

  ATSSuperscriptSpan = class;
  Arr1ATSSuperscriptSpan = array of ATSSuperscriptSpan;
  Arr2ATSSuperscriptSpan = array of Arr1ATSSuperscriptSpan;
  Arr3ATSSuperscriptSpan = array of Arr2ATSSuperscriptSpan;

  ATSTextAppearanceSpan = class;
  Arr1ATSTextAppearanceSpan = array of ATSTextAppearanceSpan;
  Arr2ATSTextAppearanceSpan = array of Arr1ATSTextAppearanceSpan;
  Arr3ATSTextAppearanceSpan = array of Arr2ATSTextAppearanceSpan;

  ATSTypefaceSpan = class;
  Arr1ATSTypefaceSpan = array of ATSTypefaceSpan;
  Arr2ATSTypefaceSpan = array of Arr1ATSTypefaceSpan;
  Arr3ATSTypefaceSpan = array of Arr2ATSTypefaceSpan;

  AWHeaderViewListAdapter = class;
  Arr1AWHeaderViewListAdapter = array of AWHeaderViewListAdapter;
  Arr2AWHeaderViewListAdapter = array of Arr1AWHeaderViewListAdapter;
  Arr3AWHeaderViewListAdapter = array of Arr2AWHeaderViewListAdapter;

  AWThemedSpinnerAdapter = interface;
  Arr1AWThemedSpinnerAdapter = array of AWThemedSpinnerAdapter;
  Arr2AWThemedSpinnerAdapter = array of Arr1AWThemedSpinnerAdapter;
  Arr3AWThemedSpinnerAdapter = array of Arr2AWThemedSpinnerAdapter;

  AWArrayAdapter = class;
  Arr1AWArrayAdapter = array of AWArrayAdapter;
  Arr2AWArrayAdapter = array of Arr1AWArrayAdapter;
  Arr3AWArrayAdapter = array of Arr2AWArrayAdapter;

  AWCursorAdapter = class;
  Arr1AWCursorAdapter = array of AWCursorAdapter;
  Arr2AWCursorAdapter = array of Arr1AWCursorAdapter;
  Arr3AWCursorAdapter = array of Arr2AWCursorAdapter;

  AWSimpleAdapter = class;
  Arr1AWSimpleAdapter = array of AWSimpleAdapter;
  Arr2AWSimpleAdapter = array of Arr1AWSimpleAdapter;
  Arr3AWSimpleAdapter = array of Arr2AWSimpleAdapter;

  AWResourceCursorTreeAdapter = class;
  Arr1AWResourceCursorTreeAdapter = array of AWResourceCursorTreeAdapter;
  Arr2AWResourceCursorTreeAdapter = array of Arr1AWResourceCursorTreeAdapter;
  Arr3AWResourceCursorTreeAdapter = array of Arr2AWResourceCursorTreeAdapter;

  ANTIsoDep = class;
  Arr1ANTIsoDep = array of ANTIsoDep;
  Arr2ANTIsoDep = array of Arr1ANTIsoDep;
  Arr3ANTIsoDep = array of Arr2ANTIsoDep;

  ANTMifareClassic = class;
  Arr1ANTMifareClassic = array of ANTMifareClassic;
  Arr2ANTMifareClassic = array of Arr1ANTMifareClassic;
  Arr3ANTMifareClassic = array of Arr2ANTMifareClassic;

  ANTMifareUltralight = class;
  Arr1ANTMifareUltralight = array of ANTMifareUltralight;
  Arr2ANTMifareUltralight = array of Arr1ANTMifareUltralight;
  Arr3ANTMifareUltralight = array of Arr2ANTMifareUltralight;

  ANTNdef = class;
  Arr1ANTNdef = array of ANTNdef;
  Arr2ANTNdef = array of Arr1ANTNdef;
  Arr3ANTNdef = array of Arr2ANTNdef;

  ANTNdefFormatable = class;
  Arr1ANTNdefFormatable = array of ANTNdefFormatable;
  Arr2ANTNdefFormatable = array of Arr1ANTNdefFormatable;
  Arr3ANTNdefFormatable = array of Arr2ANTNdefFormatable;

  ANTNfcA = class;
  Arr1ANTNfcA = array of ANTNfcA;
  Arr2ANTNfcA = array of Arr1ANTNfcA;
  Arr3ANTNfcA = array of Arr2ANTNfcA;

  ANTNfcB = class;
  Arr1ANTNfcB = array of ANTNfcB;
  Arr2ANTNfcB = array of Arr1ANTNfcB;
  Arr3ANTNfcB = array of Arr2ANTNfcB;

  ANTNfcF = class;
  Arr1ANTNfcF = array of ANTNfcF;
  Arr2ANTNfcF = array of Arr1ANTNfcF;
  Arr3ANTNfcF = array of Arr2ANTNfcF;

  ANTNfcV = class;
  Arr1ANTNfcV = array of ANTNfcV;
  Arr2ANTNfcV = array of Arr1ANTNfcV;
  Arr3ANTNfcV = array of Arr2ANTNfcV;

  AUBase64InputStream = class;
  Arr1AUBase64InputStream = array of AUBase64InputStream;
  Arr2AUBase64InputStream = array of Arr1AUBase64InputStream;
  Arr3AUBase64InputStream = array of Arr2AUBase64InputStream;

  JIBufferedInputStream = class;
  Arr1JIBufferedInputStream = array of JIBufferedInputStream;
  Arr2JIBufferedInputStream = array of Arr1JIBufferedInputStream;
  Arr3JIBufferedInputStream = array of Arr2JIBufferedInputStream;

  JIDataInputStream = class;
  Arr1JIDataInputStream = array of JIDataInputStream;
  Arr2JIDataInputStream = array of Arr1JIDataInputStream;
  Arr3JIDataInputStream = array of Arr2JIDataInputStream;

  JILineNumberInputStream = class;
  Arr1JILineNumberInputStream = array of JILineNumberInputStream;
  Arr2JILineNumberInputStream = array of Arr1JILineNumberInputStream;
  Arr3JILineNumberInputStream = array of Arr2JILineNumberInputStream;

  JIPushbackInputStream = class;
  Arr1JIPushbackInputStream = array of JIPushbackInputStream;
  Arr2JIPushbackInputStream = array of Arr1JIPushbackInputStream;
  Arr3JIPushbackInputStream = array of Arr2JIPushbackInputStream;

  JSDigestInputStream = class;
  Arr1JSDigestInputStream = array of JSDigestInputStream;
  Arr2JSDigestInputStream = array of Arr1JSDigestInputStream;
  Arr3JSDigestInputStream = array of Arr2JSDigestInputStream;

  JUZCheckedInputStream = class;
  Arr1JUZCheckedInputStream = array of JUZCheckedInputStream;
  Arr2JUZCheckedInputStream = array of Arr1JUZCheckedInputStream;
  Arr3JUZCheckedInputStream = array of Arr2JUZCheckedInputStream;

  JUZDeflaterInputStream = class;
  Arr1JUZDeflaterInputStream = array of JUZDeflaterInputStream;
  Arr2JUZDeflaterInputStream = array of Arr1JUZDeflaterInputStream;
  Arr3JUZDeflaterInputStream = array of Arr2JUZDeflaterInputStream;

  JUZInflaterInputStream = class;
  Arr1JUZInflaterInputStream = array of JUZInflaterInputStream;
  Arr2JUZInflaterInputStream = array of Arr1JUZInflaterInputStream;
  Arr3JUZInflaterInputStream = array of Arr2JUZInflaterInputStream;

  JCCipherInputStream = class;
  Arr1JCCipherInputStream = array of JCCipherInputStream;
  Arr2JCCipherInputStream = array of Arr1JCCipherInputStream;
  Arr3JCCipherInputStream = array of Arr2JCCipherInputStream;

  JILineNumberReader = class;
  Arr1JILineNumberReader = array of JILineNumberReader;
  Arr2JILineNumberReader = array of Arr1JILineNumberReader;
  Arr3JILineNumberReader = array of Arr2JILineNumberReader;

  JIPushbackReader = class;
  Arr1JIPushbackReader = array of JIPushbackReader;
  Arr2JIPushbackReader = array of Arr1JIPushbackReader;
  Arr3JIPushbackReader = array of Arr2JIPushbackReader;

  JIFileReader = class;
  Arr1JIFileReader = array of JIFileReader;
  Arr2JIFileReader = array of Arr1JIFileReader;
  Arr3JIFileReader = array of Arr2JIFileReader;

  JNCSAbstractInterruptibleChannel = class;
  Arr1JNCSAbstractInterruptibleChannel = array of JNCSAbstractInterruptibleChannel;
  Arr2JNCSAbstractInterruptibleChannel = array of Arr1JNCSAbstractInterruptibleChannel;
  Arr3JNCSAbstractInterruptibleChannel = array of Arr2JNCSAbstractInterruptibleChannel;

  JNCScatteringByteChannel = interface;
  Arr1JNCScatteringByteChannel = array of JNCScatteringByteChannel;
  Arr2JNCScatteringByteChannel = array of Arr1JNCScatteringByteChannel;
  Arr3JNCScatteringByteChannel = array of Arr2JNCScatteringByteChannel;

  JNCByteChannel = interface;
  Arr1JNCByteChannel = array of JNCByteChannel;
  Arr2JNCByteChannel = array of Arr1JNCByteChannel;
  Arr3JNCByteChannel = array of Arr2JNCByteChannel;

  JNCGatheringByteChannel = interface;
  Arr1JNCGatheringByteChannel = array of JNCGatheringByteChannel;
  Arr2JNCGatheringByteChannel = array of Arr1JNCGatheringByteChannel;
  Arr3JNCGatheringByteChannel = array of Arr2JNCGatheringByteChannel;

  AOParcelFileDescriptor = class;
  Arr1AOParcelFileDescriptor = array of AOParcelFileDescriptor;
  Arr2AOParcelFileDescriptor = array of Arr1AOParcelFileDescriptor;
  Arr3AOParcelFileDescriptor = array of Arr2AOParcelFileDescriptor;

  AUBase64OutputStream = class;
  Arr1AUBase64OutputStream = array of AUBase64OutputStream;
  Arr2AUBase64OutputStream = array of Arr1AUBase64OutputStream;
  Arr3AUBase64OutputStream = array of Arr2AUBase64OutputStream;

  JIBufferedOutputStream = class;
  Arr1JIBufferedOutputStream = array of JIBufferedOutputStream;
  Arr2JIBufferedOutputStream = array of Arr1JIBufferedOutputStream;
  Arr3JIBufferedOutputStream = array of Arr2JIBufferedOutputStream;

  JIDataOutputStream = class;
  Arr1JIDataOutputStream = array of JIDataOutputStream;
  Arr2JIDataOutputStream = array of Arr1JIDataOutputStream;
  Arr3JIDataOutputStream = array of Arr2JIDataOutputStream;

  JIPrintStream = class;
  Arr1JIPrintStream = array of JIPrintStream;
  Arr2JIPrintStream = array of Arr1JIPrintStream;
  Arr3JIPrintStream = array of Arr2JIPrintStream;

  JSDigestOutputStream = class;
  Arr1JSDigestOutputStream = array of JSDigestOutputStream;
  Arr2JSDigestOutputStream = array of Arr1JSDigestOutputStream;
  Arr3JSDigestOutputStream = array of Arr2JSDigestOutputStream;

  JUZCheckedOutputStream = class;
  Arr1JUZCheckedOutputStream = array of JUZCheckedOutputStream;
  Arr2JUZCheckedOutputStream = array of Arr1JUZCheckedOutputStream;
  Arr3JUZCheckedOutputStream = array of Arr2JUZCheckedOutputStream;

  JUZDeflaterOutputStream = class;
  Arr1JUZDeflaterOutputStream = array of JUZDeflaterOutputStream;
  Arr2JUZDeflaterOutputStream = array of Arr1JUZDeflaterOutputStream;
  Arr3JUZDeflaterOutputStream = array of Arr2JUZDeflaterOutputStream;

  JUZInflaterOutputStream = class;
  Arr1JUZInflaterOutputStream = array of JUZInflaterOutputStream;
  Arr2JUZInflaterOutputStream = array of Arr1JUZInflaterOutputStream;
  Arr3JUZInflaterOutputStream = array of Arr2JUZInflaterOutputStream;

  JCCipherOutputStream = class;
  Arr1JCCipherOutputStream = array of JCCipherOutputStream;
  Arr2JCCipherOutputStream = array of Arr1JCCipherOutputStream;
  Arr3JCCipherOutputStream = array of Arr2JCCipherOutputStream;

  JIFileWriter = class;
  Arr1JIFileWriter = array of JIFileWriter;
  Arr2JIFileWriter = array of Arr1JIFileWriter;
  Arr3JIFileWriter = array of Arr2JIFileWriter;

  JISerializablePermission = class;
  Arr1JISerializablePermission = array of JISerializablePermission;
  Arr2JISerializablePermission = array of Arr1JISerializablePermission;
  Arr3JISerializablePermission = array of Arr2JISerializablePermission;

  JLRuntimePermission = class;
  Arr1JLRuntimePermission = array of JLRuntimePermission;
  Arr2JLRuntimePermission = array of Arr1JLRuntimePermission;
  Arr3JLRuntimePermission = array of Arr2JLRuntimePermission;

  JLRReflectPermission = class;
  Arr1JLRReflectPermission = array of JLRReflectPermission;
  Arr2JLRReflectPermission = array of Arr1JLRReflectPermission;
  Arr3JLRReflectPermission = array of Arr2JLRReflectPermission;

  JNNetPermission = class;
  Arr1JNNetPermission = array of JNNetPermission;
  Arr2JNNetPermission = array of Arr1JNNetPermission;
  Arr3JNNetPermission = array of Arr2JNNetPermission;

  JSSecurityPermission = class;
  Arr1JSSecurityPermission = array of JSSecurityPermission;
  Arr2JSSecurityPermission = array of Arr1JSSecurityPermission;
  Arr3JSSecurityPermission = array of Arr2JSSecurityPermission;

  JSSQLPermission = class;
  Arr1JSSQLPermission = array of JSSQLPermission;
  Arr2JSSQLPermission = array of Arr1JSSQLPermission;
  Arr3JSSQLPermission = array of Arr2JSSQLPermission;

  JUPropertyPermission = class;
  Arr1JUPropertyPermission = array of JUPropertyPermission;
  Arr2JUPropertyPermission = array of Arr1JUPropertyPermission;
  Arr3JUPropertyPermission = array of Arr2JUPropertyPermission;

  JULLoggingPermission = class;
  Arr1JULLoggingPermission = array of JULLoggingPermission;
  Arr2JULLoggingPermission = array of Arr1JULLoggingPermission;
  Arr3JULLoggingPermission = array of Arr2JULLoggingPermission;

  JNSSSLPermission = class;
  Arr1JNSSSLPermission = array of JNSSSLPermission;
  Arr2JNSSSLPermission = array of Arr1JNSSSLPermission;
  Arr3JNSSSLPermission = array of Arr2JNSSSLPermission;

  JSAAuthPermission = class;
  Arr1JSAAuthPermission = array of JSAAuthPermission;
  Arr2JSAAuthPermission = array of Arr1JSAAuthPermission;
  Arr3JSAAuthPermission = array of Arr2JSAAuthPermission;

  JSCallableStatement = interface;
  Arr1JSCallableStatement = array of JSCallableStatement;
  Arr2JSCallableStatement = array of Arr1JSCallableStatement;
  Arr3JSCallableStatement = array of Arr2JSCallableStatement;

  JSProvider = class;
  Arr1JSProvider = array of JSProvider;
  Arr2JSProvider = array of Arr1JSProvider;
  Arr3JSProvider = array of Arr2JSProvider;

  JUCThreadPoolExecutor = class;
  Arr1JUCThreadPoolExecutor = array of JUCThreadPoolExecutor;
  Arr2JUCThreadPoolExecutor = array of Arr1JUCThreadPoolExecutor;
  Arr3JUCThreadPoolExecutor = array of Arr2JUCThreadPoolExecutor;

  OAHCSAllowAllHostnameVerifier = class;
  Arr1OAHCSAllowAllHostnameVerifier = array of OAHCSAllowAllHostnameVerifier;
  Arr2OAHCSAllowAllHostnameVerifier = array of Arr1OAHCSAllowAllHostnameVerifier;
  Arr3OAHCSAllowAllHostnameVerifier = array of Arr2OAHCSAllowAllHostnameVerifier;

  OAHCSBrowserCompatHostnameVerifier = class;
  Arr1OAHCSBrowserCompatHostnameVerifier = array of OAHCSBrowserCompatHostnameVerifier;
  Arr2OAHCSBrowserCompatHostnameVerifier = array of Arr1OAHCSBrowserCompatHostnameVerifier;
  Arr3OAHCSBrowserCompatHostnameVerifier = array of Arr2OAHCSBrowserCompatHostnameVerifier;

  OAHCSStrictHostnameVerifier = class;
  Arr1OAHCSStrictHostnameVerifier = array of OAHCSStrictHostnameVerifier;
  Arr2OAHCSStrictHostnameVerifier = array of Arr1OAHCSStrictHostnameVerifier;
  Arr3OAHCSStrictHostnameVerifier = array of Arr2OAHCSStrictHostnameVerifier;

  ATApplicationTestCase = class;
  Arr1ATApplicationTestCase = array of ATApplicationTestCase;
  Arr2ATApplicationTestCase = array of Arr1ATApplicationTestCase;
  Arr3ATApplicationTestCase = array of Arr2ATApplicationTestCase;

  ATLoaderTestCase = class;
  Arr1ATLoaderTestCase = array of ATLoaderTestCase;
  Arr2ATLoaderTestCase = array of Arr1ATLoaderTestCase;
  Arr3ATLoaderTestCase = array of Arr2ATLoaderTestCase;

  ATProviderTestCase2 = class;
  Arr1ATProviderTestCase2 = array of ATProviderTestCase2;
  Arr2ATProviderTestCase2 = array of Arr1ATProviderTestCase2;
  Arr3ATProviderTestCase2 = array of Arr2ATProviderTestCase2;

  ATServiceTestCase = class;
  Arr1ATServiceTestCase = array of ATServiceTestCase;
  Arr2ATServiceTestCase = array of Arr1ATServiceTestCase;
  Arr3ATServiceTestCase = array of Arr2ATServiceTestCase;

  ATActivityTestCase = class;
  Arr1ATActivityTestCase = array of ATActivityTestCase;
  Arr2ATActivityTestCase = array of Arr1ATActivityTestCase;
  Arr3ATActivityTestCase = array of Arr2ATActivityTestCase;

  ATProviderTestCase = class;
  Arr1ATProviderTestCase = array of ATProviderTestCase;
  Arr2ATProviderTestCase = array of Arr1ATProviderTestCase;
  Arr3ATProviderTestCase = array of Arr2ATProviderTestCase;

  ATSingleLaunchActivityTestCase = class;
  Arr1ATSingleLaunchActivityTestCase = array of ATSingleLaunchActivityTestCase;
  Arr2ATSingleLaunchActivityTestCase = array of Arr1ATSingleLaunchActivityTestCase;
  Arr3ATSingleLaunchActivityTestCase = array of Arr2ATSingleLaunchActivityTestCase;

  ATSyncBaseInstrumentation = class;
  Arr1ATSyncBaseInstrumentation = array of ATSyncBaseInstrumentation;
  Arr2ATSyncBaseInstrumentation = array of Arr1ATSyncBaseInstrumentation;
  Arr3ATSyncBaseInstrumentation = array of Arr2ATSyncBaseInstrumentation;

  OWDCDATASection = interface;
  Arr1OWDCDATASection = array of OWDCDATASection;
  Arr2OWDCDATASection = array of Arr1OWDCDATASection;
  Arr3OWDCDATASection = array of Arr2OWDCDATASection;

  JTDateFormat = class;
  Arr1JTDateFormat = array of JTDateFormat;
  Arr2JTDateFormat = array of Arr1JTDateFormat;
  Arr3JTDateFormat = array of Arr2JTDateFormat;

  JTFieldPosition = class;
  Arr1JTFieldPosition = array of JTFieldPosition;
  Arr2JTFieldPosition = array of Arr1JTFieldPosition;
  Arr3JTFieldPosition = array of Arr2JTFieldPosition;

  JTMessageFormat = class;
  Arr1JTMessageFormat = array of JTMessageFormat;
  Arr2JTMessageFormat = array of Arr1JTMessageFormat;
  Arr3JTMessageFormat = array of Arr2JTMessageFormat;

  JTNumberFormat = class;
  Arr1JTNumberFormat = array of JTNumberFormat;
  Arr2JTNumberFormat = array of Arr1JTNumberFormat;
  Arr3JTNumberFormat = array of Arr2JTNumberFormat;

  JSIRSAMultiPrimePrivateCrtKey = interface;
  Arr1JSIRSAMultiPrimePrivateCrtKey = array of JSIRSAMultiPrimePrivateCrtKey;
  Arr2JSIRSAMultiPrimePrivateCrtKey = array of Arr1JSIRSAMultiPrimePrivateCrtKey;
  Arr3JSIRSAMultiPrimePrivateCrtKey = array of Arr2JSIRSAMultiPrimePrivateCrtKey;

  JSIRSAPrivateCrtKey = interface;
  Arr1JSIRSAPrivateCrtKey = array of JSIRSAPrivateCrtKey;
  Arr2JSIRSAPrivateCrtKey = array of Arr1JSIRSAPrivateCrtKey;
  Arr3JSIRSAPrivateCrtKey = array of Arr2JSIRSAPrivateCrtKey;

  ATSpannableStringBuilder = class;
  Arr1ATSpannableStringBuilder = array of ATSpannableStringBuilder;
  Arr2ATSpannableStringBuilder = array of Arr1ATSpannableStringBuilder;
  Arr3ATSpannableStringBuilder = array of Arr2ATSpannableStringBuilder;

  JUCConcurrentSkipListMap = class;
  Arr1JUCConcurrentSkipListMap = array of JUCConcurrentSkipListMap;
  Arr2JUCConcurrentSkipListMap = array of Arr1JUCConcurrentSkipListMap;
  Arr3JUCConcurrentSkipListMap = array of Arr2JUCConcurrentSkipListMap;

  JULinkedList = class;
  Arr1JULinkedList = array of JULinkedList;
  Arr2JULinkedList = array of Arr1JULinkedList;
  Arr3JULinkedList = array of Arr2JULinkedList;

  JUStack = class;
  Arr1JUStack = array of JUStack;
  Arr2JUStack = array of Arr1JUStack;
  Arr3JUStack = array of Arr2JUStack;

  JUCLinkedBlockingDeque = class;
  Arr1JUCLinkedBlockingDeque = array of JUCLinkedBlockingDeque;
  Arr2JUCLinkedBlockingDeque = array of Arr1JUCLinkedBlockingDeque;
  Arr3JUCLinkedBlockingDeque = array of Arr2JUCLinkedBlockingDeque;

  ADSSQLiteCursor = class;
  Arr1ADSSQLiteCursor = array of ADSSQLiteCursor;
  Arr2ADSSQLiteCursor = array of Arr1ADSSQLiteCursor;
  Arr3ADSSQLiteCursor = array of Arr2ADSSQLiteCursor;

  AAApplication = class;
  Arr1AAApplication = array of AAApplication;
  Arr2AAApplication = array of Arr1AAApplication;
  Arr3AAApplication = array of Arr2AAApplication;

  AAService = class;
  Arr1AAService = array of AAService;
  Arr2AAService = array of Arr1AAService;
  Arr3AAService = array of Arr2AAService;

  AABBackupAgent = class;
  Arr1AABBackupAgent = array of AABBackupAgent;
  Arr2AABBackupAgent = array of Arr1AABBackupAgent;
  Arr3AABBackupAgent = array of Arr2AABBackupAgent;

  ACMutableContextWrapper = class;
  Arr1ACMutableContextWrapper = array of ACMutableContextWrapper;
  Arr2ACMutableContextWrapper = array of Arr1ACMutableContextWrapper;
  Arr3ACMutableContextWrapper = array of Arr2ACMutableContextWrapper;

  ATIsolatedContext = class;
  Arr1ATIsolatedContext = array of ATIsolatedContext;
  Arr2ATIsolatedContext = array of Arr1ATIsolatedContext;
  Arr3ATIsolatedContext = array of Arr2ATIsolatedContext;

  ATRenamingDelegatingContext = class;
  Arr1ATRenamingDelegatingContext = array of ATRenamingDelegatingContext;
  Arr2ATRenamingDelegatingContext = array of Arr1ATRenamingDelegatingContext;
  Arr3ATRenamingDelegatingContext = array of Arr2ATRenamingDelegatingContext;

  AVContextThemeWrapper = class;
  Arr1AVContextThemeWrapper = array of AVContextThemeWrapper;
  Arr2AVContextThemeWrapper = array of Arr1AVContextThemeWrapper;
  Arr3AVContextThemeWrapper = array of Arr2AVContextThemeWrapper;

  AGDBitmapDrawable = class;
  Arr1AGDBitmapDrawable = array of AGDBitmapDrawable;
  Arr2AGDBitmapDrawable = array of Arr1AGDBitmapDrawable;
  Arr3AGDBitmapDrawable = array of Arr2AGDBitmapDrawable;

  AGDDrawableWrapper = class;
  Arr1AGDDrawableWrapper = array of AGDDrawableWrapper;
  Arr2AGDDrawableWrapper = array of Arr1AGDDrawableWrapper;
  Arr3AGDDrawableWrapper = array of Arr2AGDDrawableWrapper;

  AGDClipDrawable = class;
  Arr1AGDClipDrawable = array of AGDClipDrawable;
  Arr2AGDClipDrawable = array of Arr1AGDClipDrawable;
  Arr3AGDClipDrawable = array of Arr2AGDClipDrawable;

  AGDColorDrawable = class;
  Arr1AGDColorDrawable = array of AGDColorDrawable;
  Arr2AGDColorDrawable = array of Arr1AGDColorDrawable;
  Arr3AGDColorDrawable = array of Arr2AGDColorDrawable;

  AGDDrawableContainer = class;
  Arr1AGDDrawableContainer = array of AGDDrawableContainer;
  Arr2AGDDrawableContainer = array of Arr1AGDDrawableContainer;
  Arr3AGDDrawableContainer = array of Arr2AGDDrawableContainer;

  AGDGradientDrawable = class;
  Arr1AGDGradientDrawable = array of AGDGradientDrawable;
  Arr2AGDGradientDrawable = array of Arr1AGDGradientDrawable;
  Arr3AGDGradientDrawable = array of Arr2AGDGradientDrawable;

  AGDInsetDrawable = class;
  Arr1AGDInsetDrawable = array of AGDInsetDrawable;
  Arr2AGDInsetDrawable = array of Arr1AGDInsetDrawable;
  Arr3AGDInsetDrawable = array of Arr2AGDInsetDrawable;

  AGDLayerDrawable = class;
  Arr1AGDLayerDrawable = array of AGDLayerDrawable;
  Arr2AGDLayerDrawable = array of Arr1AGDLayerDrawable;
  Arr3AGDLayerDrawable = array of Arr2AGDLayerDrawable;

  AGDNinePatchDrawable = class;
  Arr1AGDNinePatchDrawable = array of AGDNinePatchDrawable;
  Arr2AGDNinePatchDrawable = array of Arr1AGDNinePatchDrawable;
  Arr3AGDNinePatchDrawable = array of Arr2AGDNinePatchDrawable;

  AGDPictureDrawable = class;
  Arr1AGDPictureDrawable = array of AGDPictureDrawable;
  Arr2AGDPictureDrawable = array of Arr1AGDPictureDrawable;
  Arr3AGDPictureDrawable = array of Arr2AGDPictureDrawable;

  AGDRotateDrawable = class;
  Arr1AGDRotateDrawable = array of AGDRotateDrawable;
  Arr2AGDRotateDrawable = array of Arr1AGDRotateDrawable;
  Arr3AGDRotateDrawable = array of Arr2AGDRotateDrawable;

  AGDScaleDrawable = class;
  Arr1AGDScaleDrawable = array of AGDScaleDrawable;
  Arr2AGDScaleDrawable = array of Arr1AGDScaleDrawable;
  Arr3AGDScaleDrawable = array of Arr2AGDScaleDrawable;

  AGDShapeDrawable = class;
  Arr1AGDShapeDrawable = array of AGDShapeDrawable;
  Arr2AGDShapeDrawable = array of Arr1AGDShapeDrawable;
  Arr3AGDShapeDrawable = array of Arr2AGDShapeDrawable;

  AVView = class;
  Arr1AVView = array of AVView;
  Arr2AVView = array of Arr1AVView;
  Arr3AVView = array of Arr2AVView;

  AWPopupMenu = class;
  Arr1AWPopupMenu = array of AWPopupMenu;
  Arr2AWPopupMenu = array of Arr1AWPopupMenu;
  Arr3AWPopupMenu = array of Arr2AWPopupMenu;

  APPreferenceCategory = class;
  Arr1APPreferenceCategory = array of APPreferenceCategory;
  Arr2APPreferenceCategory = array of Arr1APPreferenceCategory;
  Arr3APPreferenceCategory = array of Arr2APPreferenceCategory;

  APCheckBoxPreference = class;
  Arr1APCheckBoxPreference = array of APCheckBoxPreference;
  Arr2APCheckBoxPreference = array of Arr1APCheckBoxPreference;
  Arr3APCheckBoxPreference = array of Arr2APCheckBoxPreference;

  APSwitchPreference = class;
  Arr1APSwitchPreference = array of APSwitchPreference;
  Arr2APSwitchPreference = array of Arr1APSwitchPreference;
  Arr3APSwitchPreference = array of Arr2APSwitchPreference;

  ATSDynamicDrawableSpan = class;
  Arr1ATSDynamicDrawableSpan = array of ATSDynamicDrawableSpan;
  Arr2ATSDynamicDrawableSpan = array of Arr1ATSDynamicDrawableSpan;
  Arr3ATSDynamicDrawableSpan = array of Arr2ATSDynamicDrawableSpan;

  AWResourceCursorAdapter = class;
  Arr1AWResourceCursorAdapter = array of AWResourceCursorAdapter;
  Arr2AWResourceCursorAdapter = array of Arr1AWResourceCursorAdapter;
  Arr3AWResourceCursorAdapter = array of Arr2AWResourceCursorAdapter;

  AWSimpleCursorTreeAdapter = class;
  Arr1AWSimpleCursorTreeAdapter = array of AWSimpleCursorTreeAdapter;
  Arr2AWSimpleCursorTreeAdapter = array of Arr1AWSimpleCursorTreeAdapter;
  Arr3AWSimpleCursorTreeAdapter = array of Arr2AWSimpleCursorTreeAdapter;

  JUZGZIPInputStream = class;
  Arr1JUZGZIPInputStream = array of JUZGZIPInputStream;
  Arr2JUZGZIPInputStream = array of Arr1JUZGZIPInputStream;
  Arr3JUZGZIPInputStream = array of Arr2JUZGZIPInputStream;

  JUZZipInputStream = class;
  Arr1JUZZipInputStream = array of JUZZipInputStream;
  Arr2JUZZipInputStream = array of Arr1JUZZipInputStream;
  Arr3JUZZipInputStream = array of Arr2JUZZipInputStream;

  JNCSelectableChannel = class;
  Arr1JNCSelectableChannel = array of JNCSelectableChannel;
  Arr2JNCSelectableChannel = array of Arr1JNCSelectableChannel;
  Arr3JNCSelectableChannel = array of Arr2JNCSelectableChannel;

  JNCSeekableByteChannel = interface;
  Arr1JNCSeekableByteChannel = array of JNCSeekableByteChannel;
  Arr2JNCSeekableByteChannel = array of Arr1JNCSeekableByteChannel;
  Arr3JNCSeekableByteChannel = array of Arr2JNCSeekableByteChannel;

  JNCFileChannel = class;
  Arr1JNCFileChannel = array of JNCFileChannel;
  Arr2JNCFileChannel = array of Arr1JNCFileChannel;
  Arr3JNCFileChannel = array of Arr2JNCFileChannel;

  ACRAssetFileDescriptor = class;
  Arr1ACRAssetFileDescriptor = array of ACRAssetFileDescriptor;
  Arr2ACRAssetFileDescriptor = array of Arr1ACRAssetFileDescriptor;
  Arr3ACRAssetFileDescriptor = array of Arr2ACRAssetFileDescriptor;

  JUZGZIPOutputStream = class;
  Arr1JUZGZIPOutputStream = array of JUZGZIPOutputStream;
  Arr2JUZGZIPOutputStream = array of Arr1JUZGZIPOutputStream;
  Arr3JUZGZIPOutputStream = array of Arr2JUZGZIPOutputStream;

  JUZZipOutputStream = class;
  Arr1JUZZipOutputStream = array of JUZZipOutputStream;
  Arr2JUZZipOutputStream = array of Arr1JUZZipOutputStream;
  Arr3JUZZipOutputStream = array of Arr2JUZZipOutputStream;

  JSAuthProvider = class;
  Arr1JSAuthProvider = array of JSAuthProvider;
  Arr2JSAuthProvider = array of Arr1JSAuthProvider;
  Arr3JSAuthProvider = array of Arr2JSAuthProvider;

  JUCScheduledThreadPoolExecutor = class;
  Arr1JUCScheduledThreadPoolExecutor = array of JUCScheduledThreadPoolExecutor;
  Arr2JUCScheduledThreadPoolExecutor = array of Arr1JUCScheduledThreadPoolExecutor;
  Arr3JUCScheduledThreadPoolExecutor = array of Arr2JUCScheduledThreadPoolExecutor;

  ATActivityInstrumentationTestCase = class;
  Arr1ATActivityInstrumentationTestCase = array of ATActivityInstrumentationTestCase;
  Arr2ATActivityInstrumentationTestCase = array of Arr1ATActivityInstrumentationTestCase;
  Arr3ATActivityInstrumentationTestCase = array of Arr2ATActivityInstrumentationTestCase;

  ATActivityInstrumentationTestCase2 = class;
  Arr1ATActivityInstrumentationTestCase2 = array of ATActivityInstrumentationTestCase2;
  Arr2ATActivityInstrumentationTestCase2 = array of Arr1ATActivityInstrumentationTestCase2;
  Arr3ATActivityInstrumentationTestCase2 = array of Arr2ATActivityInstrumentationTestCase2;

  ATActivityUnitTestCase = class;
  Arr1ATActivityUnitTestCase = array of ATActivityUnitTestCase;
  Arr2ATActivityUnitTestCase = array of Arr1ATActivityUnitTestCase;
  Arr3ATActivityUnitTestCase = array of Arr2ATActivityUnitTestCase;

  JTSimpleDateFormat = class;
  Arr1JTSimpleDateFormat = array of JTSimpleDateFormat;
  Arr2JTSimpleDateFormat = array of Arr1JTSimpleDateFormat;
  Arr3JTSimpleDateFormat = array of Arr2JTSimpleDateFormat;

  JTChoiceFormat = class;
  Arr1JTChoiceFormat = array of JTChoiceFormat;
  Arr2JTChoiceFormat = array of Arr1JTChoiceFormat;
  Arr3JTChoiceFormat = array of Arr2JTChoiceFormat;

  JTDecimalFormat = class;
  Arr1JTDecimalFormat = array of JTDecimalFormat;
  Arr2JTDecimalFormat = array of Arr1JTDecimalFormat;
  Arr3JTDecimalFormat = array of Arr2JTDecimalFormat;

  ATMMockApplication = class;
  Arr1ATMMockApplication = array of ATMMockApplication;
  Arr2ATMMockApplication = array of Arr1ATMMockApplication;
  Arr3ATMMockApplication = array of Arr2ATMMockApplication;

  AAAccessibilityService = class;
  Arr1AAAccessibilityService = array of AAAccessibilityService;
  Arr2AAAccessibilityService = array of Arr1AAAccessibilityService;
  Arr3AAAccessibilityService = array of Arr2AAAccessibilityService;

  AAIntentService = class;
  Arr1AAIntentService = array of AAIntentService;
  Arr2AAIntentService = array of Arr1AAIntentService;
  Arr3AAIntentService = array of Arr2AAIntentService;

  AIAbstractInputMethodService = class;
  Arr1AIAbstractInputMethodService = array of AIAbstractInputMethodService;
  Arr2AIAbstractInputMethodService = array of Arr1AIAbstractInputMethodService;
  Arr3AIAbstractInputMethodService = array of Arr2AIAbstractInputMethodService;

  ANVpnService = class;
  Arr1ANVpnService = array of ANVpnService;
  Arr2ANVpnService = array of Arr1ANVpnService;
  Arr3ANVpnService = array of Arr2ANVpnService;

  ASTSpellCheckerService = class;
  Arr1ASTSpellCheckerService = array of ASTSpellCheckerService;
  Arr2ASTSpellCheckerService = array of Arr1ASTSpellCheckerService;
  Arr3ASTSpellCheckerService = array of Arr2ASTSpellCheckerService;

  ASWWallpaperService = class;
  Arr1ASWWallpaperService = array of ASWWallpaperService;
  Arr2ASWWallpaperService = array of Arr1ASWWallpaperService;
  Arr3ASWWallpaperService = array of Arr2ASWWallpaperService;

  ASRecognitionService = class;
  Arr1ASRecognitionService = array of ASRecognitionService;
  Arr2ASRecognitionService = array of Arr1ASRecognitionService;
  Arr3ASRecognitionService = array of Arr2ASRecognitionService;

  ASTTextToSpeechService = class;
  Arr1ASTTextToSpeechService = array of ASTTextToSpeechService;
  Arr2ASTTextToSpeechService = array of Arr1ASTTextToSpeechService;
  Arr3ASTTextToSpeechService = array of Arr2ASTTextToSpeechService;

  AWRemoteViewsService = class;
  Arr1AWRemoteViewsService = array of AWRemoteViewsService;
  Arr2AWRemoteViewsService = array of Arr1AWRemoteViewsService;
  Arr3AWRemoteViewsService = array of Arr2AWRemoteViewsService;

  AABBackupAgentHelper = class;
  Arr1AABBackupAgentHelper = array of AABBackupAgentHelper;
  Arr2AABBackupAgentHelper = array of Arr1AABBackupAgentHelper;
  Arr3AABBackupAgentHelper = array of Arr2AABBackupAgentHelper;

  AGDAnimationDrawable = class;
  Arr1AGDAnimationDrawable = array of AGDAnimationDrawable;
  Arr2AGDAnimationDrawable = array of Arr1AGDAnimationDrawable;
  Arr3AGDAnimationDrawable = array of Arr2AGDAnimationDrawable;

  AGDLevelListDrawable = class;
  Arr1AGDLevelListDrawable = array of AGDLevelListDrawable;
  Arr2AGDLevelListDrawable = array of Arr1AGDLevelListDrawable;
  Arr3AGDLevelListDrawable = array of Arr2AGDLevelListDrawable;

  AGDStateListDrawable = class;
  Arr1AGDStateListDrawable = array of AGDStateListDrawable;
  Arr2AGDStateListDrawable = array of Arr1AGDStateListDrawable;
  Arr3AGDStateListDrawable = array of Arr2AGDStateListDrawable;

  AGDTransitionDrawable = class;
  Arr1AGDTransitionDrawable = array of AGDTransitionDrawable;
  Arr2AGDTransitionDrawable = array of Arr1AGDTransitionDrawable;
  Arr3AGDTransitionDrawable = array of Arr2AGDTransitionDrawable;

  AGDPaintDrawable = class;
  Arr1AGDPaintDrawable = array of AGDPaintDrawable;
  Arr2AGDPaintDrawable = array of Arr1AGDPaintDrawable;
  Arr3AGDPaintDrawable = array of Arr2AGDPaintDrawable;

  AAActivity = class;
  Arr1AAActivity = array of AAActivity;
  Arr2AAActivity = array of Arr1AAActivity;
  Arr3AAActivity = array of Arr2AAActivity;

  AADialog = class;
  Arr1AADialog = array of AADialog;
  Arr2AADialog = array of Arr1AADialog;
  Arr3AADialog = array of Arr2AADialog;

  AAFragment = class;
  Arr1AAFragment = array of AAFragment;
  Arr2AAFragment = array of Arr1AAFragment;
  Arr3AAFragment = array of Arr2AAFragment;

  AIKeyboardView = class;
  Arr1AIKeyboardView = array of AIKeyboardView;
  Arr2AIKeyboardView = array of Arr1AIKeyboardView;
  Arr3AIKeyboardView = array of Arr2AIKeyboardView;

  AVSurfaceView = class;
  Arr1AVSurfaceView = array of AVSurfaceView;
  Arr2AVSurfaceView = array of Arr1AVSurfaceView;
  Arr3AVSurfaceView = array of Arr2AVSurfaceView;

  AVTextureView = class;
  Arr1AVTextureView = array of AVTextureView;
  Arr2AVTextureView = array of Arr1AVTextureView;
  Arr3AVTextureView = array of Arr2AVTextureView;

  AVViewGroup = class;
  Arr1AVViewGroup = array of AVViewGroup;
  Arr2AVViewGroup = array of Arr1AVViewGroup;
  Arr3AVViewGroup = array of Arr2AVViewGroup;

  AVViewStub = class;
  Arr1AVViewStub = array of AVViewStub;
  Arr2AVViewStub = array of Arr1AVViewStub;
  Arr3AVViewStub = array of Arr2AVViewStub;

  AWAnalogClock = class;
  Arr1AWAnalogClock = array of AWAnalogClock;
  Arr2AWAnalogClock = array of Arr1AWAnalogClock;
  Arr3AWAnalogClock = array of Arr2AWAnalogClock;

  AWImageView = class;
  Arr1AWImageView = array of AWImageView;
  Arr2AWImageView = array of Arr1AWImageView;
  Arr3AWImageView = array of Arr2AWImageView;

  AWPopupWindow = class;
  Arr1AWPopupWindow = array of AWPopupWindow;
  Arr2AWPopupWindow = array of Arr1AWPopupWindow;
  Arr3AWPopupWindow = array of Arr2AWPopupWindow;

  AWProgressBar = class;
  Arr1AWProgressBar = array of AWProgressBar;
  Arr2AWProgressBar = array of Arr1AWProgressBar;
  Arr3AWProgressBar = array of Arr2AWProgressBar;

  AWSpace = class;
  Arr1AWSpace = array of AWSpace;
  Arr2AWSpace = array of Arr1AWSpace;
  Arr3AWSpace = array of Arr2AWSpace;

  AWTextView = class;
  Arr1AWTextView = array of AWTextView;
  Arr2AWTextView = array of Arr1AWTextView;
  Arr3AWTextView = array of Arr2AWTextView;

  AWZoomButtonsController = class;
  Arr1AWZoomButtonsController = array of AWZoomButtonsController;
  Arr2AWZoomButtonsController = array of Arr1AWZoomButtonsController;
  Arr3AWZoomButtonsController = array of Arr2AWZoomButtonsController;

  ATSImageSpan = class;
  Arr1ATSImageSpan = array of ATSImageSpan;
  Arr2ATSImageSpan = array of Arr1ATSImageSpan;
  Arr3ATSImageSpan = array of Arr2ATSImageSpan;

  AWSimpleCursorAdapter = class;
  Arr1AWSimpleCursorAdapter = array of AWSimpleCursorAdapter;
  Arr2AWSimpleCursorAdapter = array of Arr1AWSimpleCursorAdapter;
  Arr3AWSimpleCursorAdapter = array of Arr2AWSimpleCursorAdapter;

  JUJJarInputStream = class;
  Arr1JUJJarInputStream = array of JUJJarInputStream;
  Arr2JUJJarInputStream = array of Arr1JUJJarInputStream;
  Arr3JUJJarInputStream = array of Arr2JUJJarInputStream;

  JNCSAbstractSelectableChannel = class;
  Arr1JNCSAbstractSelectableChannel = array of JNCSAbstractSelectableChannel;
  Arr2JNCSAbstractSelectableChannel = array of Arr1JNCSAbstractSelectableChannel;
  Arr3JNCSAbstractSelectableChannel = array of Arr2JNCSAbstractSelectableChannel;

  JUJJarOutputStream = class;
  Arr1JUJJarOutputStream = array of JUJJarOutputStream;
  Arr2JUJJarOutputStream = array of Arr1JUJJarOutputStream;
  Arr3JUJJarOutputStream = array of Arr2JUJJarOutputStream;

  AIInputMethodService = class;
  Arr1AIInputMethodService = array of AIInputMethodService;
  Arr2AIInputMethodService = array of Arr1AIInputMethodService;
  Arr3AIInputMethodService = array of Arr2AIInputMethodService;

  AAAccountAuthenticatorActivity = class;
  Arr1AAAccountAuthenticatorActivity = array of AAAccountAuthenticatorActivity;
  Arr2AAAccountAuthenticatorActivity = array of Arr1AAAccountAuthenticatorActivity;
  Arr3AAAccountAuthenticatorActivity = array of Arr2AAAccountAuthenticatorActivity;

  AAActivityGroup = class;
  Arr1AAActivityGroup = array of AAActivityGroup;
  Arr2AAActivityGroup = array of Arr1AAActivityGroup;
  Arr3AAActivityGroup = array of Arr2AAActivityGroup;

  AAAliasActivity = class;
  Arr1AAAliasActivity = array of AAAliasActivity;
  Arr2AAAliasActivity = array of Arr1AAAliasActivity;
  Arr3AAAliasActivity = array of Arr2AAAliasActivity;

  AAListActivity = class;
  Arr1AAListActivity = array of AAListActivity;
  Arr2AAListActivity = array of Arr1AAListActivity;
  Arr3AAListActivity = array of Arr2AAListActivity;

  AANativeActivity = class;
  Arr1AANativeActivity = array of AANativeActivity;
  Arr2AANativeActivity = array of Arr1AANativeActivity;
  Arr3AANativeActivity = array of Arr2AANativeActivity;

  AADialogFragment = class;
  Arr1AADialogFragment = array of AADialogFragment;
  Arr2AADialogFragment = array of Arr1AADialogFragment;
  Arr3AADialogFragment = array of Arr2AADialogFragment;

  AAFragmentManager = class;
  Arr1AAFragmentManager = array of AAFragmentManager;
  Arr2AAFragmentManager = array of Arr1AAFragmentManager;
  Arr3AAFragmentManager = array of Arr2AAFragmentManager;

  AAListFragment = class;
  Arr1AAListFragment = array of AAListFragment;
  Arr2AAListFragment = array of Arr1AAListFragment;
  Arr3AAListFragment = array of Arr2AAListFragment;

  APPreferenceFragment = class;
  Arr1APPreferenceFragment = array of APPreferenceFragment;
  Arr2APPreferenceFragment = array of Arr1APPreferenceFragment;
  Arr3APPreferenceFragment = array of Arr2APPreferenceFragment;

  AWWebViewFragment = class;
  Arr1AWWebViewFragment = array of AWWebViewFragment;
  Arr2AWWebViewFragment = array of Arr1AWWebViewFragment;
  Arr3AWWebViewFragment = array of Arr2AWWebViewFragment;

  AOGLSurfaceView = class;
  Arr1AOGLSurfaceView = array of AOGLSurfaceView;
  Arr2AOGLSurfaceView = array of Arr1AOGLSurfaceView;
  Arr3AOGLSurfaceView = array of Arr2AOGLSurfaceView;

  AAActionBar = class;
  Arr1AAActionBar = array of AAActionBar;
  Arr2AAActionBar = array of Arr1AAActionBar;
  Arr3AAActionBar = array of Arr2AAActionBar;

  AWAbsoluteLayout = class;
  Arr1AWAbsoluteLayout = array of AWAbsoluteLayout;
  Arr2AWAbsoluteLayout = array of Arr1AWAbsoluteLayout;
  Arr3AWAbsoluteLayout = array of Arr2AWAbsoluteLayout;

  AWAdapterView = class;
  Arr1AWAdapterView = array of AWAdapterView;
  Arr2AWAdapterView = array of Arr1AWAdapterView;
  Arr3AWAdapterView = array of Arr2AWAdapterView;

  AWFrameLayout = class;
  Arr1AWFrameLayout = array of AWFrameLayout;
  Arr2AWFrameLayout = array of Arr1AWFrameLayout;
  Arr3AWFrameLayout = array of Arr2AWFrameLayout;

  AWGridLayout = class;
  Arr1AWGridLayout = array of AWGridLayout;
  Arr2AWGridLayout = array of Arr1AWGridLayout;
  Arr3AWGridLayout = array of Arr2AWGridLayout;

  AWLinearLayout = class;
  Arr1AWLinearLayout = array of AWLinearLayout;
  Arr2AWLinearLayout = array of Arr1AWLinearLayout;
  Arr3AWLinearLayout = array of Arr2AWLinearLayout;

  AWRelativeLayout = class;
  Arr1AWRelativeLayout = array of AWRelativeLayout;
  Arr2AWRelativeLayout = array of Arr1AWRelativeLayout;
  Arr3AWRelativeLayout = array of Arr2AWRelativeLayout;

  AWSlidingDrawer = class;
  Arr1AWSlidingDrawer = array of AWSlidingDrawer;
  Arr2AWSlidingDrawer = array of Arr1AWSlidingDrawer;
  Arr3AWSlidingDrawer = array of Arr2AWSlidingDrawer;

  AWImageButton = class;
  Arr1AWImageButton = array of AWImageButton;
  Arr2AWImageButton = array of Arr1AWImageButton;
  Arr3AWImageButton = array of Arr2AWImageButton;

  AWQuickContactBadge = class;
  Arr1AWQuickContactBadge = array of AWQuickContactBadge;
  Arr2AWQuickContactBadge = array of Arr1AWQuickContactBadge;
  Arr3AWQuickContactBadge = array of Arr2AWQuickContactBadge;

  AWAbsSeekBar = class;
  Arr1AWAbsSeekBar = array of AWAbsSeekBar;
  Arr2AWAbsSeekBar = array of Arr1AWAbsSeekBar;
  Arr3AWAbsSeekBar = array of Arr2AWAbsSeekBar;

  AWButton = class;
  Arr1AWButton = array of AWButton;
  Arr2AWButton = array of Arr1AWButton;
  Arr3AWButton = array of Arr2AWButton;

  AWCheckedTextView = class;
  Arr1AWCheckedTextView = array of AWCheckedTextView;
  Arr2AWCheckedTextView = array of Arr1AWCheckedTextView;
  Arr3AWCheckedTextView = array of Arr2AWCheckedTextView;

  AWChronometer = class;
  Arr1AWChronometer = array of AWChronometer;
  Arr2AWChronometer = array of Arr1AWChronometer;
  Arr3AWChronometer = array of Arr2AWChronometer;

  AWDigitalClock = class;
  Arr1AWDigitalClock = array of AWDigitalClock;
  Arr2AWDigitalClock = array of Arr1AWDigitalClock;
  Arr3AWDigitalClock = array of Arr2AWDigitalClock;

  AWEditText = class;
  Arr1AWEditText = array of AWEditText;
  Arr2AWEditText = array of Arr1AWEditText;
  Arr3AWEditText = array of Arr2AWEditText;

  JNCNetworkChannel = interface;
  Arr1JNCNetworkChannel = array of JNCNetworkChannel;
  Arr2JNCNetworkChannel = array of Arr1JNCNetworkChannel;
  Arr3JNCNetworkChannel = array of Arr2JNCNetworkChannel;

  JNCDatagramChannel = class;
  Arr1JNCDatagramChannel = array of JNCDatagramChannel;
  Arr2JNCDatagramChannel = array of Arr1JNCDatagramChannel;
  Arr3JNCDatagramChannel = array of Arr2JNCDatagramChannel;

  JNCPipe = class;
  Arr1JNCPipe = array of JNCPipe;
  Arr2JNCPipe = array of Arr1JNCPipe;
  Arr3JNCPipe = array of Arr2JNCPipe;

  JNCServerSocketChannel = class;
  Arr1JNCServerSocketChannel = array of JNCServerSocketChannel;
  Arr2JNCServerSocketChannel = array of Arr1JNCServerSocketChannel;
  Arr3JNCServerSocketChannel = array of Arr2JNCServerSocketChannel;

  JNCSocketChannel = class;
  Arr1JNCSocketChannel = array of JNCSocketChannel;
  Arr2JNCSocketChannel = array of Arr1JNCSocketChannel;
  Arr3JNCSocketChannel = array of Arr2JNCSocketChannel;

  AATabActivity = class;
  Arr1AATabActivity = array of AATabActivity;
  Arr2AATabActivity = array of Arr1AATabActivity;
  Arr3AATabActivity = array of Arr2AATabActivity;

  AALauncherActivity = class;
  Arr1AALauncherActivity = array of AALauncherActivity;
  Arr2AALauncherActivity = array of Arr1AALauncherActivity;
  Arr3AALauncherActivity = array of Arr2AALauncherActivity;

  AAFragmentBreadCrumbs = class;
  Arr1AAFragmentBreadCrumbs = array of AAFragmentBreadCrumbs;
  Arr2AAFragmentBreadCrumbs = array of Arr1AAFragmentBreadCrumbs;
  Arr3AAFragmentBreadCrumbs = array of Arr2AAFragmentBreadCrumbs;

  APPreferenceActivity = class;
  Arr1APPreferenceActivity = array of APPreferenceActivity;
  Arr2APPreferenceActivity = array of Arr1APPreferenceActivity;
  Arr3APPreferenceActivity = array of Arr2APPreferenceActivity;

  AWWebView = class;
  Arr1AWWebView = array of AWWebView;
  Arr2AWWebView = array of Arr1AWWebView;
  Arr3AWWebView = array of Arr2AWWebView;

  AAAlertDialog = class;
  Arr1AAAlertDialog = array of AAAlertDialog;
  Arr2AAAlertDialog = array of Arr1AAAlertDialog;
  Arr3AAAlertDialog = array of Arr2AAAlertDialog;

  APPreferenceScreen = class;
  Arr1APPreferenceScreen = array of APPreferenceScreen;
  Arr2APPreferenceScreen = array of Arr1APPreferenceScreen;
  Arr3APPreferenceScreen = array of Arr2APPreferenceScreen;

  ATMCharacterPickerDialog = class;
  Arr1ATMCharacterPickerDialog = array of ATMCharacterPickerDialog;
  Arr2ATMCharacterPickerDialog = array of Arr1ATMCharacterPickerDialog;
  Arr3ATMCharacterPickerDialog = array of Arr2ATMCharacterPickerDialog;

  AWAbsListView = class;
  Arr1AWAbsListView = array of AWAbsListView;
  Arr2AWAbsListView = array of Arr1AWAbsListView;
  Arr3AWAbsListView = array of Arr2AWAbsListView;

  AWAbsSpinner = class;
  Arr1AWAbsSpinner = array of AWAbsSpinner;
  Arr2AWAbsSpinner = array of Arr1AWAbsSpinner;
  Arr3AWAbsSpinner = array of Arr2AWAbsSpinner;

  AWAdvanceable = interface;
  Arr1AWAdvanceable = array of AWAdvanceable;
  Arr2AWAdvanceable = array of Arr1AWAdvanceable;
  Arr3AWAdvanceable = array of Arr2AWAdvanceable;

  AWAdapterViewAnimator = class;
  Arr1AWAdapterViewAnimator = array of AWAdapterViewAnimator;
  Arr2AWAdapterViewAnimator = array of Arr1AWAdapterViewAnimator;
  Arr3AWAdapterViewAnimator = array of Arr2AWAdapterViewAnimator;

  AWListPopupWindow = class;
  Arr1AWListPopupWindow = array of AWListPopupWindow;
  Arr2AWListPopupWindow = array of Arr1AWListPopupWindow;
  Arr3AWListPopupWindow = array of Arr2AWListPopupWindow;

  AAAppWidgetHostView = class;
  Arr1AAAppWidgetHostView = array of AAAppWidgetHostView;
  Arr2AAAppWidgetHostView = array of Arr1AAAppWidgetHostView;
  Arr3AAAppWidgetHostView = array of Arr2AAAppWidgetHostView;

  AGGestureOverlayView = class;
  Arr1AGGestureOverlayView = array of AGGestureOverlayView;
  Arr2AGGestureOverlayView = array of Arr1AGGestureOverlayView;
  Arr3AGGestureOverlayView = array of Arr2AGGestureOverlayView;

  AWCalendarView = class;
  Arr1AWCalendarView = array of AWCalendarView;
  Arr2AWCalendarView = array of Arr1AWCalendarView;
  Arr3AWCalendarView = array of Arr2AWCalendarView;

  AWDatePicker = class;
  Arr1AWDatePicker = array of AWDatePicker;
  Arr2AWDatePicker = array of Arr1AWDatePicker;
  Arr3AWDatePicker = array of Arr2AWDatePicker;

  AWHorizontalScrollView = class;
  Arr1AWHorizontalScrollView = array of AWHorizontalScrollView;
  Arr2AWHorizontalScrollView = array of Arr1AWHorizontalScrollView;
  Arr3AWHorizontalScrollView = array of Arr2AWHorizontalScrollView;

  AWMediaController = class;
  Arr1AWMediaController = array of AWMediaController;
  Arr2AWMediaController = array of Arr1AWMediaController;
  Arr3AWMediaController = array of Arr2AWMediaController;

  AWScrollView = class;
  Arr1AWScrollView = array of AWScrollView;
  Arr2AWScrollView = array of Arr1AWScrollView;
  Arr3AWScrollView = array of Arr2AWScrollView;

  AWTabHost = class;
  Arr1AWTabHost = array of AWTabHost;
  Arr2AWTabHost = array of Arr1AWTabHost;
  Arr3AWTabHost = array of Arr2AWTabHost;

  AWTimePicker = class;
  Arr1AWTimePicker = array of AWTimePicker;
  Arr2AWTimePicker = array of Arr1AWTimePicker;
  Arr3AWTimePicker = array of Arr2AWTimePicker;

  AWViewAnimator = class;
  Arr1AWViewAnimator = array of AWViewAnimator;
  Arr2AWViewAnimator = array of Arr1AWViewAnimator;
  Arr3AWViewAnimator = array of Arr2AWViewAnimator;

  AWNumberPicker = class;
  Arr1AWNumberPicker = array of AWNumberPicker;
  Arr2AWNumberPicker = array of Arr1AWNumberPicker;
  Arr3AWNumberPicker = array of Arr2AWNumberPicker;

  AWRadioGroup = class;
  Arr1AWRadioGroup = array of AWRadioGroup;
  Arr2AWRadioGroup = array of Arr1AWRadioGroup;
  Arr3AWRadioGroup = array of Arr2AWRadioGroup;

  AWSearchView = class;
  Arr1AWSearchView = array of AWSearchView;
  Arr2AWSearchView = array of Arr1AWSearchView;
  Arr3AWSearchView = array of Arr2AWSearchView;

  AWTabWidget = class;
  Arr1AWTabWidget = array of AWTabWidget;
  Arr2AWTabWidget = array of Arr1AWTabWidget;
  Arr3AWTabWidget = array of Arr2AWTabWidget;

  AWTableLayout = class;
  Arr1AWTableLayout = array of AWTableLayout;
  Arr2AWTableLayout = array of Arr1AWTableLayout;
  Arr3AWTableLayout = array of Arr2AWTableLayout;

  AWTableRow = class;
  Arr1AWTableRow = array of AWTableRow;
  Arr2AWTableRow = array of Arr1AWTableRow;
  Arr3AWTableRow = array of Arr2AWTableRow;

  AWZoomControls = class;
  Arr1AWZoomControls = array of AWZoomControls;
  Arr2AWZoomControls = array of Arr1AWZoomControls;
  Arr3AWZoomControls = array of Arr2AWZoomControls;

  AWDialerFilter = class;
  Arr1AWDialerFilter = array of AWDialerFilter;
  Arr2AWDialerFilter = array of Arr1AWDialerFilter;
  Arr3AWDialerFilter = array of Arr2AWDialerFilter;

  AWTwoLineListItem = class;
  Arr1AWTwoLineListItem = array of AWTwoLineListItem;
  Arr2AWTwoLineListItem = array of Arr1AWTwoLineListItem;
  Arr3AWTwoLineListItem = array of Arr2AWTwoLineListItem;

  AWZoomButton = class;
  Arr1AWZoomButton = array of AWZoomButton;
  Arr2AWZoomButton = array of Arr1AWZoomButton;
  Arr3AWZoomButton = array of Arr2AWZoomButton;

  AWRatingBar = class;
  Arr1AWRatingBar = array of AWRatingBar;
  Arr2AWRatingBar = array of Arr1AWRatingBar;
  Arr3AWRatingBar = array of Arr2AWRatingBar;

  AWSeekBar = class;
  Arr1AWSeekBar = array of AWSeekBar;
  Arr2AWSeekBar = array of Arr1AWSeekBar;
  Arr3AWSeekBar = array of Arr2AWSeekBar;

  AWCompoundButton = class;
  Arr1AWCompoundButton = array of AWCompoundButton;
  Arr2AWCompoundButton = array of Arr1AWCompoundButton;
  Arr3AWCompoundButton = array of Arr2AWCompoundButton;

  AIExtractEditText = class;
  Arr1AIExtractEditText = array of AIExtractEditText;
  Arr2AIExtractEditText = array of Arr1AIExtractEditText;
  Arr3AIExtractEditText = array of Arr2AIExtractEditText;

  AWAutoCompleteTextView = class;
  Arr1AWAutoCompleteTextView = array of AWAutoCompleteTextView;
  Arr2AWAutoCompleteTextView = array of Arr1AWAutoCompleteTextView;
  Arr3AWAutoCompleteTextView = array of Arr2AWAutoCompleteTextView;

  AAProgressDialog = class;
  Arr1AAProgressDialog = array of AAProgressDialog;
  Arr2AAProgressDialog = array of Arr1AAProgressDialog;
  Arr3AAProgressDialog = array of Arr2AAProgressDialog;

  APDialogPreference = class;
  Arr1APDialogPreference = array of APDialogPreference;
  Arr2APDialogPreference = array of Arr1APDialogPreference;
  Arr3APDialogPreference = array of Arr2APDialogPreference;

  AWGridView = class;
  Arr1AWGridView = array of AWGridView;
  Arr2AWGridView = array of Arr1AWGridView;
  Arr3AWGridView = array of Arr2AWGridView;

  AWListView = class;
  Arr1AWListView = array of AWListView;
  Arr2AWListView = array of Arr1AWListView;
  Arr3AWListView = array of Arr2AWListView;

  AWGallery = class;
  Arr1AWGallery = array of AWGallery;
  Arr2AWGallery = array of Arr1AWGallery;
  Arr3AWGallery = array of Arr2AWGallery;

  AWSpinner = class;
  Arr1AWSpinner = array of AWSpinner;
  Arr2AWSpinner = array of Arr1AWSpinner;
  Arr3AWSpinner = array of Arr2AWSpinner;

  AWAdapterViewFlipper = class;
  Arr1AWAdapterViewFlipper = array of AWAdapterViewFlipper;
  Arr2AWAdapterViewFlipper = array of Arr1AWAdapterViewFlipper;
  Arr3AWAdapterViewFlipper = array of Arr2AWAdapterViewFlipper;

  AWStackView = class;
  Arr1AWStackView = array of AWStackView;
  Arr2AWStackView = array of Arr1AWStackView;
  Arr3AWStackView = array of Arr2AWStackView;

  AADatePickerDialog = class;
  Arr1AADatePickerDialog = array of AADatePickerDialog;
  Arr2AADatePickerDialog = array of Arr1AADatePickerDialog;
  Arr3AADatePickerDialog = array of Arr2AADatePickerDialog;

  AWVideoView = class;
  Arr1AWVideoView = array of AWVideoView;
  Arr2AWVideoView = array of Arr1AWVideoView;
  Arr3AWVideoView = array of Arr2AWVideoView;

  AATimePickerDialog = class;
  Arr1AATimePickerDialog = array of AATimePickerDialog;
  Arr2AATimePickerDialog = array of Arr1AATimePickerDialog;
  Arr3AATimePickerDialog = array of Arr2AATimePickerDialog;

  AWViewFlipper = class;
  Arr1AWViewFlipper = array of AWViewFlipper;
  Arr2AWViewFlipper = array of Arr1AWViewFlipper;
  Arr3AWViewFlipper = array of Arr2AWViewFlipper;

  AWViewSwitcher = class;
  Arr1AWViewSwitcher = array of AWViewSwitcher;
  Arr2AWViewSwitcher = array of Arr1AWViewSwitcher;
  Arr3AWViewSwitcher = array of Arr2AWViewSwitcher;

  AWCheckBox = class;
  Arr1AWCheckBox = array of AWCheckBox;
  Arr2AWCheckBox = array of Arr1AWCheckBox;
  Arr3AWCheckBox = array of Arr2AWCheckBox;

  AWRadioButton = class;
  Arr1AWRadioButton = array of AWRadioButton;
  Arr2AWRadioButton = array of Arr1AWRadioButton;
  Arr3AWRadioButton = array of Arr2AWRadioButton;

  AWSwitch = class;
  Arr1AWSwitch = array of AWSwitch;
  Arr2AWSwitch = array of Arr1AWSwitch;
  Arr3AWSwitch = array of Arr2AWSwitch;

  AWToggleButton = class;
  Arr1AWToggleButton = array of AWToggleButton;
  Arr2AWToggleButton = array of Arr1AWToggleButton;
  Arr3AWToggleButton = array of Arr2AWToggleButton;

  AWMultiAutoCompleteTextView = class;
  Arr1AWMultiAutoCompleteTextView = array of AWMultiAutoCompleteTextView;
  Arr2AWMultiAutoCompleteTextView = array of Arr1AWMultiAutoCompleteTextView;
  Arr3AWMultiAutoCompleteTextView = array of Arr2AWMultiAutoCompleteTextView;

  APEditTextPreference = class;
  Arr1APEditTextPreference = array of APEditTextPreference;
  Arr2APEditTextPreference = array of Arr1APEditTextPreference;
  Arr3APEditTextPreference = array of Arr2APEditTextPreference;

  APListPreference = class;
  Arr1APListPreference = array of APListPreference;
  Arr2APListPreference = array of Arr1APListPreference;
  Arr3APListPreference = array of Arr2APListPreference;

  APMultiSelectListPreference = class;
  Arr1APMultiSelectListPreference = array of APMultiSelectListPreference;
  Arr2APMultiSelectListPreference = array of Arr1APMultiSelectListPreference;
  Arr3APMultiSelectListPreference = array of Arr2APMultiSelectListPreference;

  AWExpandableListView = class;
  Arr1AWExpandableListView = array of AWExpandableListView;
  Arr2AWExpandableListView = array of Arr1AWExpandableListView;
  Arr3AWExpandableListView = array of Arr2AWExpandableListView;

  AWImageSwitcher = class;
  Arr1AWImageSwitcher = array of AWImageSwitcher;
  Arr2AWImageSwitcher = array of Arr1AWImageSwitcher;
  Arr3AWImageSwitcher = array of Arr2AWImageSwitcher;

  AWTextSwitcher = class;
  Arr1AWTextSwitcher = array of AWTextSwitcher;
  Arr2AWTextSwitcher = array of Arr1AWTextSwitcher;
  Arr3AWTextSwitcher = array of Arr2AWTextSwitcher;

  ATURfc822Tokenizer = class;
  Arr1ATURfc822Tokenizer = array of ATURfc822Tokenizer;
  Arr2ATURfc822Tokenizer = array of Arr1ATURfc822Tokenizer;
  Arr3ATURfc822Tokenizer = array of Arr2ATURfc822Tokenizer;

  AAExpandableListActivity = class;
  Arr1AAExpandableListActivity = array of AAExpandableListActivity;
  Arr2AAExpandableListActivity = array of Arr1AAExpandableListActivity;
  Arr3AAExpandableListActivity = array of Arr2AAExpandableListActivity;

  AATypeConverter = class;
  Arr1AATypeConverter = array of AATypeConverter;
  Arr2AATypeConverter = array of Arr1AATypeConverter;
  Arr3AATypeConverter = array of Arr2AATypeConverter;

  AAActivityOptions = class;
  Arr1AAActivityOptions = array of AAActivityOptions;
  Arr2AAActivityOptions = array of Arr1AAActivityOptions;
  Arr3AAActivityOptions = array of Arr2AAActivityOptions;

  AAAppOpsManager = class;
  Arr1AAAppOpsManager = array of AAAppOpsManager;
  Arr2AAAppOpsManager = array of Arr1AAAppOpsManager;
  Arr3AAAppOpsManager = array of Arr2AAAppOpsManager;

  AAFragmentContainer = class;
  Arr1AAFragmentContainer = array of AAFragmentContainer;
  Arr2AAFragmentContainer = array of Arr1AAFragmentContainer;
  Arr3AAFragmentContainer = array of Arr2AAFragmentContainer;

  AAFragmentController = class;
  Arr1AAFragmentController = array of AAFragmentController;
  Arr2AAFragmentController = array of Arr1AAFragmentController;
  Arr3AAFragmentController = array of Arr2AAFragmentController;

  AAFragmentManagerNonConfig = class;
  Arr1AAFragmentManagerNonConfig = array of AAFragmentManagerNonConfig;
  Arr2AAFragmentManagerNonConfig = array of Arr1AAFragmentManagerNonConfig;
  Arr3AAFragmentManagerNonConfig = array of Arr2AAFragmentManagerNonConfig;

  AASharedElementCallback = class;
  Arr1AASharedElementCallback = array of AASharedElementCallback;
  Arr2AASharedElementCallback = array of Arr1AASharedElementCallback;
  Arr3AASharedElementCallback = array of Arr2AASharedElementCallback;

  AATaskStackBuilder = class;
  Arr1AATaskStackBuilder = array of AATaskStackBuilder;
  Arr2AATaskStackBuilder = array of Arr1AATaskStackBuilder;
  Arr3AATaskStackBuilder = array of Arr2AATaskStackBuilder;

  AAUiAutomation = class;
  Arr1AAUiAutomation = array of AAUiAutomation;
  Arr2AAUiAutomation = array of Arr1AAUiAutomation;
  Arr3AAUiAutomation = array of Arr2AAUiAutomation;

  AAJJobScheduler = class;
  Arr1AAJJobScheduler = array of AAJJobScheduler;
  Arr2AAJJobScheduler = array of Arr1AAJJobScheduler;
  Arr3AAJJobScheduler = array of Arr2AAJJobScheduler;

  AAUUsageStatsManager = class;
  Arr1AAUUsageStatsManager = array of AAUUsageStatsManager;
  Arr2AAUUsageStatsManager = array of Arr1AAUUsageStatsManager;
  Arr3AAUUsageStatsManager = array of Arr2AAUUsageStatsManager;

  ABBluetoothGattCallback = class;
  Arr1ABBluetoothGattCallback = array of ABBluetoothGattCallback;
  Arr2ABBluetoothGattCallback = array of Arr1ABBluetoothGattCallback;
  Arr3ABBluetoothGattCallback = array of Arr2ABBluetoothGattCallback;

  ABBluetoothGattServerCallback = class;
  Arr1ABBluetoothGattServerCallback = array of ABBluetoothGattServerCallback;
  Arr2ABBluetoothGattServerCallback = array of Arr1ABBluetoothGattServerCallback;
  Arr3ABBluetoothGattServerCallback = array of Arr2ABBluetoothGattServerCallback;

  ABBluetoothManager = class;
  Arr1ABBluetoothManager = array of ABBluetoothManager;
  Arr2ABBluetoothManager = array of Arr1ABBluetoothManager;
  Arr3ABBluetoothManager = array of Arr2ABBluetoothManager;

  ABLAdvertiseCallback = class;
  Arr1ABLAdvertiseCallback = array of ABLAdvertiseCallback;
  Arr2ABLAdvertiseCallback = array of Arr1ABLAdvertiseCallback;
  Arr3ABLAdvertiseCallback = array of Arr2ABLAdvertiseCallback;

  ABLBluetoothLeAdvertiser = class;
  Arr1ABLBluetoothLeAdvertiser = array of ABLBluetoothLeAdvertiser;
  Arr2ABLBluetoothLeAdvertiser = array of Arr1ABLBluetoothLeAdvertiser;
  Arr3ABLBluetoothLeAdvertiser = array of Arr2ABLBluetoothLeAdvertiser;

  ABLBluetoothLeScanner = class;
  Arr1ABLBluetoothLeScanner = array of ABLBluetoothLeScanner;
  Arr2ABLBluetoothLeScanner = array of Arr1ABLBluetoothLeScanner;
  Arr3ABLBluetoothLeScanner = array of Arr2ABLBluetoothLeScanner;

  ABLScanCallback = class;
  Arr1ABLScanCallback = array of ABLScanCallback;
  Arr2ABLScanCallback = array of Arr1ABLScanCallback;
  Arr3ABLScanCallback = array of Arr2ABLScanCallback;

  ABLScanRecord = class;
  Arr1ABLScanRecord = array of ABLScanRecord;
  Arr2ABLScanRecord = array of Arr1ABLScanRecord;
  Arr3ABLScanRecord = array of Arr2ABLScanRecord;

  ACRestrictionsManager = class;
  Arr1ACRestrictionsManager = array of ACRestrictionsManager;
  Arr2ACRestrictionsManager = array of Arr1ACRestrictionsManager;
  Arr3ACRestrictionsManager = array of Arr2ACRestrictionsManager;

  ACPLauncherActivityInfo = class;
  Arr1ACPLauncherActivityInfo = array of ACPLauncherActivityInfo;
  Arr2ACPLauncherActivityInfo = array of Arr1ACPLauncherActivityInfo;
  Arr3ACPLauncherActivityInfo = array of Arr2ACPLauncherActivityInfo;

  ACPLauncherApps = class;
  Arr1ACPLauncherApps = array of ACPLauncherApps;
  Arr2ACPLauncherApps = array of Arr1ACPLauncherApps;
  Arr3ACPLauncherApps = array of Arr2ACPLauncherApps;

  ACPShortcutManager = class;
  Arr1ACPShortcutManager = array of ACPShortcutManager;
  Arr2ACPShortcutManager = array of Arr1ACPShortcutManager;
  Arr3ACPShortcutManager = array of Arr2ACPShortcutManager;

  AGOutline = class;
  Arr1AGOutline = array of AGOutline;
  Arr2AGOutline = array of Arr1AGOutline;
  Arr3AGOutline = array of Arr2AGOutline;

  AGPPdfDocument = class;
  Arr1AGPPdfDocument = array of AGPPdfDocument;
  Arr2AGPPdfDocument = array of Arr1AGPPdfDocument;
  Arr3AGPPdfDocument = array of Arr2AGPPdfDocument;

  AHConsumerIrManager = class;
  Arr1AHConsumerIrManager = array of AHConsumerIrManager;
  Arr2AHConsumerIrManager = array of Arr1AHConsumerIrManager;
  Arr3AHConsumerIrManager = array of Arr2AHConsumerIrManager;

  AHSensorAdditionalInfo = class;
  Arr1AHSensorAdditionalInfo = array of AHSensorAdditionalInfo;
  Arr2AHSensorAdditionalInfo = array of Arr1AHSensorAdditionalInfo;
  Arr3AHSensorAdditionalInfo = array of Arr2AHSensorAdditionalInfo;

  AHTriggerEvent = class;
  Arr1AHTriggerEvent = array of AHTriggerEvent;
  Arr2AHTriggerEvent = array of Arr1AHTriggerEvent;
  Arr3AHTriggerEvent = array of Arr2AHTriggerEvent;

  AHTriggerEventListener = class;
  Arr1AHTriggerEventListener = array of AHTriggerEventListener;
  Arr2AHTriggerEventListener = array of Arr1AHTriggerEventListener;
  Arr3AHTriggerEventListener = array of Arr2AHTriggerEventListener;

  AHCCameraMetadata = class;
  Arr1AHCCameraMetadata = array of AHCCameraMetadata;
  Arr2AHCCameraMetadata = array of Arr1AHCCameraMetadata;
  Arr3AHCCameraMetadata = array of Arr2AHCCameraMetadata;

  AHCCaptureFailure = class;
  Arr1AHCCaptureFailure = array of AHCCaptureFailure;
  Arr2AHCCaptureFailure = array of Arr1AHCCaptureFailure;
  Arr3AHCCaptureFailure = array of Arr2AHCCaptureFailure;

  AHCPBlackLevelPattern = class;
  Arr1AHCPBlackLevelPattern = array of AHCPBlackLevelPattern;
  Arr2AHCPBlackLevelPattern = array of Arr1AHCPBlackLevelPattern;
  Arr3AHCPBlackLevelPattern = array of Arr2AHCPBlackLevelPattern;

  AHCPColorSpaceTransform = class;
  Arr1AHCPColorSpaceTransform = array of AHCPColorSpaceTransform;
  Arr2AHCPColorSpaceTransform = array of Arr1AHCPColorSpaceTransform;
  Arr3AHCPColorSpaceTransform = array of Arr2AHCPColorSpaceTransform;

  AHCPFace = class;
  Arr1AHCPFace = array of AHCPFace;
  Arr2AHCPFace = array of Arr1AHCPFace;
  Arr3AHCPFace = array of Arr2AHCPFace;

  AHCPInputConfiguration = class;
  Arr1AHCPInputConfiguration = array of AHCPInputConfiguration;
  Arr2AHCPInputConfiguration = array of Arr1AHCPInputConfiguration;
  Arr3AHCPInputConfiguration = array of Arr2AHCPInputConfiguration;

  AHCPLensShadingMap = class;
  Arr1AHCPLensShadingMap = array of AHCPLensShadingMap;
  Arr2AHCPLensShadingMap = array of Arr1AHCPLensShadingMap;
  Arr3AHCPLensShadingMap = array of Arr2AHCPLensShadingMap;

  AHCPMeteringRectangle = class;
  Arr1AHCPMeteringRectangle = array of AHCPMeteringRectangle;
  Arr2AHCPMeteringRectangle = array of Arr1AHCPMeteringRectangle;
  Arr3AHCPMeteringRectangle = array of Arr2AHCPMeteringRectangle;

  AHCPRggbChannelVector = class;
  Arr1AHCPRggbChannelVector = array of AHCPRggbChannelVector;
  Arr2AHCPRggbChannelVector = array of Arr1AHCPRggbChannelVector;
  Arr3AHCPRggbChannelVector = array of Arr2AHCPRggbChannelVector;

  AHCPStreamConfigurationMap = class;
  Arr1AHCPStreamConfigurationMap = array of AHCPStreamConfigurationMap;
  Arr2AHCPStreamConfigurationMap = array of Arr1AHCPStreamConfigurationMap;
  Arr3AHCPStreamConfigurationMap = array of Arr2AHCPStreamConfigurationMap;

  AHCPTonemapCurve = class;
  Arr1AHCPTonemapCurve = array of AHCPTonemapCurve;
  Arr2AHCPTonemapCurve = array of Arr1AHCPTonemapCurve;
  Arr3AHCPTonemapCurve = array of Arr2AHCPTonemapCurve;

  AHDVirtualDisplay = class;
  Arr1AHDVirtualDisplay = array of AHDVirtualDisplay;
  Arr2AHDVirtualDisplay = array of Arr1AHDVirtualDisplay;
  Arr3AHDVirtualDisplay = array of Arr2AHDVirtualDisplay;

  AHFFingerprintManager = class;
  Arr1AHFFingerprintManager = array of AHFFingerprintManager;
  Arr2AHFFingerprintManager = array of Arr1AHFFingerprintManager;
  Arr3AHFFingerprintManager = array of Arr2AHFFingerprintManager;

  AHIInputManager = class;
  Arr1AHIInputManager = array of AHIInputManager;
  Arr2AHIInputManager = array of Arr1AHIInputManager;
  Arr3AHIInputManager = array of Arr2AHIInputManager;

  AILUCharacterEnums = class;
  Arr1AILUCharacterEnums = array of AILUCharacterEnums;
  Arr2AILUCharacterEnums = array of Arr1AILUCharacterEnums;
  Arr3AILUCharacterEnums = array of Arr2AILUCharacterEnums;

  AILUProperty = interface;
  Arr1AILUProperty = array of AILUProperty;
  Arr2AILUProperty = array of Arr1AILUProperty;
  Arr3AILUProperty = array of Arr2AILUProperty;

  AITCollationElementIterator = class;
  Arr1AITCollationElementIterator = array of AITCollationElementIterator;
  Arr2AITCollationElementIterator = array of Arr1AITCollationElementIterator;
  Arr3AITCollationElementIterator = array of Arr2AITCollationElementIterator;

  AITNumberingSystem = class;
  Arr1AITNumberingSystem = array of AITNumberingSystem;
  Arr2AITNumberingSystem = array of Arr1AITNumberingSystem;
  Arr3AITNumberingSystem = array of Arr2AITNumberingSystem;

  AITReplaceable = interface;
  Arr1AITReplaceable = array of AITReplaceable;
  Arr2AITReplaceable = array of Arr1AITReplaceable;
  Arr3AITReplaceable = array of Arr2AITReplaceable;

  AITSymbolTable = interface;
  Arr1AITSymbolTable = array of AITSymbolTable;
  Arr2AITSymbolTable = array of Arr1AITSymbolTable;
  Arr3AITSymbolTable = array of Arr2AITSymbolTable;

  AITUnicodeMatcher = interface;
  Arr1AITUnicodeMatcher = array of AITUnicodeMatcher;
  Arr2AITUnicodeMatcher = array of Arr1AITUnicodeMatcher;
  Arr3AITUnicodeMatcher = array of Arr2AITUnicodeMatcher;

  AITUnicodeSetIterator = class;
  Arr1AITUnicodeSetIterator = array of AITUnicodeSetIterator;
  Arr2AITUnicodeSetIterator = array of Arr1AITUnicodeSetIterator;
  Arr3AITUnicodeSetIterator = array of Arr2AITUnicodeSetIterator;

  AIUMeasure = class;
  Arr1AIUMeasure = array of AIUMeasure;
  Arr2AIUMeasure = array of Arr1AIUMeasure;
  Arr3AIUMeasure = array of Arr2AIUMeasure;

  AIUOutput = class;
  Arr1AIUOutput = array of AIUOutput;
  Arr2AIUOutput = array of Arr1AIUOutput;
  Arr3AIUOutput = array of Arr2AIUOutput;

  AIURangeValueIterator = interface;
  Arr1AIURangeValueIterator = array of AIURangeValueIterator;
  Arr2AIURangeValueIterator = array of Arr1AIURangeValueIterator;
  Arr3AIURangeValueIterator = array of Arr2AIURangeValueIterator;

  AIUValueIterator = interface;
  Arr1AIUValueIterator = array of AIUValueIterator;
  Arr2AIUValueIterator = array of Arr1AIUValueIterator;
  Arr3AIUValueIterator = array of Arr2AIUValueIterator;

  ALGnssStatus = class;
  Arr1ALGnssStatus = array of ALGnssStatus;
  Arr2ALGnssStatus = array of Arr1ALGnssStatus;
  Arr3ALGnssStatus = array of Arr2ALGnssStatus;

  ALOnNmeaMessageListener = interface;
  Arr1ALOnNmeaMessageListener = array of ALOnNmeaMessageListener;
  Arr2ALOnNmeaMessageListener = array of Arr1ALOnNmeaMessageListener;
  Arr3ALOnNmeaMessageListener = array of Arr2ALOnNmeaMessageListener;

  AMAudioDeviceCallback = class;
  Arr1AMAudioDeviceCallback = array of AMAudioDeviceCallback;
  Arr2AMAudioDeviceCallback = array of Arr1AMAudioDeviceCallback;
  Arr3AMAudioDeviceCallback = array of Arr2AMAudioDeviceCallback;

  AMAudioDeviceInfo = class;
  Arr1AMAudioDeviceInfo = array of AMAudioDeviceInfo;
  Arr2AMAudioDeviceInfo = array of Arr1AMAudioDeviceInfo;
  Arr3AMAudioDeviceInfo = array of Arr2AMAudioDeviceInfo;

  AMAudioTimestamp = class;
  Arr1AMAudioTimestamp = array of AMAudioTimestamp;
  Arr2AMAudioTimestamp = array of Arr1AMAudioTimestamp;
  Arr3AMAudioTimestamp = array of Arr2AMAudioTimestamp;

  AMDrmInitData = class;
  Arr1AMDrmInitData = array of AMDrmInitData;
  Arr2AMDrmInitData = array of Arr1AMDrmInitData;
  Arr3AMDrmInitData = array of Arr2AMDrmInitData;

  AMMediaActionSound = class;
  Arr1AMMediaActionSound = array of AMMediaActionSound;
  Arr2AMMediaActionSound = array of Arr1AMMediaActionSound;
  Arr3AMMediaActionSound = array of Arr2AMMediaActionSound;

  AMMediaCodecInfo = class;
  Arr1AMMediaCodecInfo = array of AMMediaCodecInfo;
  Arr2AMMediaCodecInfo = array of Arr1AMMediaCodecInfo;
  Arr3AMMediaCodecInfo = array of Arr2AMMediaCodecInfo;

  AMMediaCodecList = class;
  Arr1AMMediaCodecList = array of AMMediaCodecList;
  Arr2AMMediaCodecList = array of Arr1AMMediaCodecList;
  Arr3AMMediaCodecList = array of Arr2AMMediaCodecList;

  AMMediaCrypto = class;
  Arr1AMMediaCrypto = array of AMMediaCrypto;
  Arr2AMMediaCrypto = array of Arr1AMMediaCrypto;
  Arr3AMMediaCrypto = array of Arr2AMMediaCrypto;

  AMMediaFormat = class;
  Arr1AMMediaFormat = array of AMMediaFormat;
  Arr2AMMediaFormat = array of Arr1AMMediaFormat;
  Arr3AMMediaFormat = array of Arr2AMMediaFormat;

  AMMediaMetadataEditor = class;
  Arr1AMMediaMetadataEditor = array of AMMediaMetadataEditor;
  Arr2AMMediaMetadataEditor = array of Arr1AMMediaMetadataEditor;
  Arr3AMMediaMetadataEditor = array of Arr2AMMediaMetadataEditor;

  AMMediaRouter = class;
  Arr1AMMediaRouter = array of AMMediaRouter;
  Arr2AMMediaRouter = array of Arr1AMMediaRouter;
  Arr3AMMediaRouter = array of Arr2AMMediaRouter;

  AMMediaSync = class;
  Arr1AMMediaSync = array of AMMediaSync;
  Arr2AMMediaSync = array of Arr1AMMediaSync;
  Arr3AMMediaSync = array of Arr2AMMediaSync;

  AMMediaSyncEvent = class;
  Arr1AMMediaSyncEvent = array of AMMediaSyncEvent;
  Arr2AMMediaSyncEvent = array of Arr1AMMediaSyncEvent;
  Arr3AMMediaSyncEvent = array of Arr2AMMediaSyncEvent;

  AMMediaTimestamp = class;
  Arr1AMMediaTimestamp = array of AMMediaTimestamp;
  Arr2AMMediaTimestamp = array of Arr1AMMediaTimestamp;
  Arr3AMMediaTimestamp = array of Arr2AMMediaTimestamp;

  AMRemoteController = class;
  Arr1AMRemoteController = array of AMRemoteController;
  Arr2AMRemoteController = array of Arr1AMRemoteController;
  Arr3AMRemoteController = array of Arr2AMRemoteController;

  AMSyncParams = class;
  Arr1AMSyncParams = array of AMSyncParams;
  Arr2AMSyncParams = array of Arr1AMSyncParams;
  Arr3AMSyncParams = array of Arr2AMSyncParams;

  AMTimedMetaData = class;
  Arr1AMTimedMetaData = array of AMTimedMetaData;
  Arr2AMTimedMetaData = array of Arr1AMTimedMetaData;
  Arr3AMTimedMetaData = array of Arr2AMTimedMetaData;

  AMTimedText = class;
  Arr1AMTimedText = array of AMTimedText;
  Arr2AMTimedText = array of Arr1AMTimedText;
  Arr3AMTimedText = array of Arr2AMTimedText;

  AMVolumeProvider = class;
  Arr1AMVolumeProvider = array of AMVolumeProvider;
  Arr2AMVolumeProvider = array of Arr1AMVolumeProvider;
  Arr3AMVolumeProvider = array of Arr2AMVolumeProvider;

  AMMMidiManager = class;
  Arr1AMMMidiManager = array of AMMMidiManager;
  Arr2AMMMidiManager = array of Arr1AMMMidiManager;
  Arr3AMMMidiManager = array of Arr2AMMMidiManager;

  AMMMidiReceiver = class;
  Arr1AMMMidiReceiver = array of AMMMidiReceiver;
  Arr2AMMMidiReceiver = array of Arr1AMMMidiReceiver;
  Arr3AMMMidiReceiver = array of Arr2AMMMidiReceiver;

  AMMMidiSender = class;
  Arr1AMMMidiSender = array of AMMMidiSender;
  Arr2AMMMidiSender = array of Arr1AMMMidiSender;
  Arr3AMMMidiSender = array of Arr2AMMMidiSender;

  AMPMediaProjectionManager = class;
  Arr1AMPMediaProjectionManager = array of AMPMediaProjectionManager;
  Arr2AMPMediaProjectionManager = array of Arr1AMPMediaProjectionManager;
  Arr3AMPMediaProjectionManager = array of Arr2AMPMediaProjectionManager;

  AMSMediaSessionManager = class;
  Arr1AMSMediaSessionManager = array of AMSMediaSessionManager;
  Arr2AMSMediaSessionManager = array of Arr1AMSMediaSessionManager;
  Arr3AMSMediaSessionManager = array of Arr2AMSMediaSessionManager;

  AMTTvContentRating = class;
  Arr1AMTTvContentRating = array of AMTTvContentRating;
  Arr2AMTTvContentRating = array of Arr1AMTTvContentRating;
  Arr3AMTTvContentRating = array of Arr2AMTTvContentRating;

  AMTTvInputManager = class;
  Arr1AMTTvInputManager = array of AMTTvInputManager;
  Arr2AMTTvInputManager = array of Arr1AMTTvInputManager;
  Arr3AMTTvInputManager = array of Arr2AMTTvInputManager;

  AMTTvRecordingClient = class;
  Arr1AMTTvRecordingClient = array of AMTTvRecordingClient;
  Arr2AMTTvRecordingClient = array of Arr1AMTTvRecordingClient;
  Arr3AMTTvRecordingClient = array of Arr2AMTTvRecordingClient;

  AMMtpEvent = class;
  Arr1AMMtpEvent = array of AMMtpEvent;
  Arr2AMMtpEvent = array of Arr1AMMtpEvent;
  Arr3AMMtpEvent = array of Arr2AMMtpEvent;

  ANPskKeyManager = class;
  Arr1ANPskKeyManager = array of ANPskKeyManager;
  Arr2ANPskKeyManager = array of Arr1ANPskKeyManager;
  Arr3ANPskKeyManager = array of Arr2ANPskKeyManager;

  ANHX509TrustManagerExtensions = class;
  Arr1ANHX509TrustManagerExtensions = array of ANHX509TrustManagerExtensions;
  Arr2ANHX509TrustManagerExtensions = array of Arr1ANHX509TrustManagerExtensions;
  Arr3ANHX509TrustManagerExtensions = array of Arr2ANHX509TrustManagerExtensions;

  ANNNsdManager = class;
  Arr1ANNNsdManager = array of ANNNsdManager;
  Arr2ANNNsdManager = array of Arr1ANNNsdManager;
  Arr3ANNNsdManager = array of Arr2ANNNsdManager;

  ANCCardEmulation = class;
  Arr1ANCCardEmulation = array of ANCCardEmulation;
  Arr2ANCCardEmulation = array of Arr1ANCCardEmulation;
  Arr3ANCCardEmulation = array of Arr2ANCCardEmulation;

  ANCNfcFCardEmulation = class;
  Arr1ANCNfcFCardEmulation = array of ANCNfcFCardEmulation;
  Arr2ANCNfcFCardEmulation = array of Arr1ANCNfcFCardEmulation;
  Arr3ANCNfcFCardEmulation = array of Arr2ANCNfcFCardEmulation;

  AOEGL14 = class;
  Arr1AOEGL14 = array of AOEGL14;
  Arr2AOEGL14 = array of Arr1AOEGL14;
  Arr3AOEGL14 = array of Arr2AOEGL14;

  AOEGLExt = class;
  Arr1AOEGLExt = array of AOEGLExt;
  Arr2AOEGLExt = array of Arr1AOEGLExt;
  Arr3AOEGLExt = array of Arr2AOEGLExt;

  AOEGLObjectHandle = class;
  Arr1AOEGLObjectHandle = array of AOEGLObjectHandle;
  Arr2AOEGLObjectHandle = array of Arr1AOEGLObjectHandle;
  Arr3AOEGLObjectHandle = array of Arr2AOEGLObjectHandle;

  AOGLES31Ext = class;
  Arr1AOGLES31Ext = array of AOGLES31Ext;
  Arr2AOGLES31Ext = array of Arr1AOGLES31Ext;
  Arr3AOGLES31Ext = array of Arr2AOGLES31Ext;

  AOCancellationSignal = class;
  Arr1AOCancellationSignal = array of AOCancellationSignal;
  Arr2AOCancellationSignal = array of Arr1AOCancellationSignal;
  Arr3AOCancellationSignal = array of Arr2AOCancellationSignal;

  AOHardwarePropertiesManager = class;
  Arr1AOHardwarePropertiesManager = array of AOHardwarePropertiesManager;
  Arr2AOHardwarePropertiesManager = array of Arr1AOHardwarePropertiesManager;
  Arr3AOHardwarePropertiesManager = array of Arr2AOHardwarePropertiesManager;

  AOTrace = class;
  Arr1AOTrace = array of AOTrace;
  Arr2AOTrace = array of Arr1AOTrace;
  Arr3AOTrace = array of Arr2AOTrace;

  AOUserManager = class;
  Arr1AOUserManager = array of AOUserManager;
  Arr2AOUserManager = array of Arr1AOUserManager;
  Arr3AOUserManager = array of Arr2AOUserManager;

  AOHHealthStats = class;
  Arr1AOHHealthStats = array of AOHHealthStats;
  Arr2AOHHealthStats = array of Arr1AOHHealthStats;
  Arr3AOHHealthStats = array of Arr2AOHHealthStats;

  AOHPackageHealthStats = class;
  Arr1AOHPackageHealthStats = array of AOHPackageHealthStats;
  Arr2AOHPackageHealthStats = array of Arr1AOHPackageHealthStats;
  Arr3AOHPackageHealthStats = array of Arr2AOHPackageHealthStats;

  AOHPidHealthStats = class;
  Arr1AOHPidHealthStats = array of AOHPidHealthStats;
  Arr2AOHPidHealthStats = array of Arr1AOHPidHealthStats;
  Arr3AOHPidHealthStats = array of Arr2AOHPidHealthStats;

  AOHProcessHealthStats = class;
  Arr1AOHProcessHealthStats = array of AOHProcessHealthStats;
  Arr2AOHProcessHealthStats = array of Arr1AOHProcessHealthStats;
  Arr3AOHProcessHealthStats = array of Arr2AOHProcessHealthStats;

  AOHServiceHealthStats = class;
  Arr1AOHServiceHealthStats = array of AOHServiceHealthStats;
  Arr2AOHServiceHealthStats = array of Arr1AOHServiceHealthStats;
  Arr3AOHServiceHealthStats = array of Arr2AOHServiceHealthStats;

  AOHSystemHealthManager = class;
  Arr1AOHSystemHealthManager = array of AOHSystemHealthManager;
  Arr2AOHSystemHealthManager = array of Arr1AOHSystemHealthManager;
  Arr3AOHSystemHealthManager = array of Arr2AOHSystemHealthManager;

  AOHUidHealthStats = class;
  Arr1AOHUidHealthStats = array of AOHUidHealthStats;
  Arr2AOHUidHealthStats = array of Arr1AOHUidHealthStats;
  Arr3AOHUidHealthStats = array of Arr2AOHUidHealthStats;

  APPrintDocumentAdapter = class;
  Arr1APPrintDocumentAdapter = array of APPrintDocumentAdapter;
  Arr2APPrintDocumentAdapter = array of Arr1APPrintDocumentAdapter;
  Arr3APPrintDocumentAdapter = array of Arr2APPrintDocumentAdapter;

  APPrintJob = class;
  Arr1APPrintJob = array of APPrintJob;
  Arr2APPrintJob = array of Arr1APPrintJob;
  Arr3APPrintJob = array of Arr2APPrintJob;

  APPrintManager = class;
  Arr1APPrintManager = array of APPrintManager;
  Arr2APPrintManager = array of Arr1APPrintManager;
  Arr3APPrintManager = array of Arr2APPrintManager;

  APCustomPrinterIconCallback = class;
  Arr1APCustomPrinterIconCallback = array of APCustomPrinterIconCallback;
  Arr2APCustomPrinterIconCallback = array of Arr1APCustomPrinterIconCallback;
  Arr3APCustomPrinterIconCallback = array of Arr2APCustomPrinterIconCallback;

  APPrintDocument = class;
  Arr1APPrintDocument = array of APPrintDocument;
  Arr2APPrintDocument = array of Arr1APPrintDocument;
  Arr3APPrintDocument = array of Arr2APPrintDocument;

  APservicePrintJob = class;
  Arr1APservicePrintJob = array of APservicePrintJob;
  Arr2APservicePrintJob = array of Arr1APservicePrintJob;
  Arr3APservicePrintJob = array of Arr2APservicePrintJob;

  APPrinterDiscoverySession = class;
  Arr1APPrinterDiscoverySession = array of APPrinterDiscoverySession;
  Arr2APPrinterDiscoverySession = array of Arr1APPrinterDiscoverySession;
  Arr3APPrinterDiscoverySession = array of Arr2APPrinterDiscoverySession;

  APBlockedNumberContract = class;
  Arr1APBlockedNumberContract = array of APBlockedNumberContract;
  Arr2APBlockedNumberContract = array of Arr1APBlockedNumberContract;
  Arr3APBlockedNumberContract = array of Arr2APBlockedNumberContract;

  APDocumentsContract = class;
  Arr1APDocumentsContract = array of APDocumentsContract;
  Arr2APDocumentsContract = array of Arr1APDocumentsContract;
  Arr3APDocumentsContract = array of Arr2APDocumentsContract;

  ASNetworkSecurityPolicy = class;
  Arr1ASNetworkSecurityPolicy = array of ASNetworkSecurityPolicy;
  Arr2ASNetworkSecurityPolicy = array of Arr1ASNetworkSecurityPolicy;
  Arr3ASNetworkSecurityPolicy = array of Arr2ASNetworkSecurityPolicy;

  ASKKeyProperties = class;
  Arr1ASKKeyProperties = array of ASKKeyProperties;
  Arr2ASKKeyProperties = array of Arr1ASKKeyProperties;
  Arr3ASKKeyProperties = array of Arr2ASKKeyProperties;

  ASVAlwaysOnHotwordDetector = class;
  Arr1ASVAlwaysOnHotwordDetector = array of ASVAlwaysOnHotwordDetector;
  Arr2ASVAlwaysOnHotwordDetector = array of Arr1ASVAlwaysOnHotwordDetector;
  Arr3ASVAlwaysOnHotwordDetector = array of Arr2ASVAlwaysOnHotwordDetector;

  ASOs = class;
  Arr1ASOs = array of ASOs;
  Arr2ASOs = array of Arr1ASOs;
  Arr3ASOs = array of Arr2ASOs;

  ASOsConstants = class;
  Arr1ASOsConstants = array of ASOsConstants;
  Arr2ASOsConstants = array of Arr1ASOsConstants;
  Arr3ASOsConstants = array of Arr2ASOsConstants;

  ASStructPollfd = class;
  Arr1ASStructPollfd = array of ASStructPollfd;
  Arr2ASStructPollfd = array of Arr1ASStructPollfd;
  Arr3ASStructPollfd = array of Arr2ASStructPollfd;

  ASStructStat = class;
  Arr1ASStructStat = array of ASStructStat;
  Arr2ASStructStat = array of Arr1ASStructStat;
  Arr3ASStructStat = array of Arr2ASStructStat;

  ASStructStatVfs = class;
  Arr1ASStructStatVfs = array of ASStructStatVfs;
  Arr2ASStructStatVfs = array of Arr1ASStructStatVfs;
  Arr3ASStructStatVfs = array of Arr2ASStructStatVfs;

  ASStructUtsname = class;
  Arr1ASStructUtsname = array of ASStructUtsname;
  Arr2ASStructUtsname = array of Arr1ASStructUtsname;
  Arr3ASStructUtsname = array of Arr2ASStructUtsname;

  ATConferenceable = class;
  Arr1ATConferenceable = array of ATConferenceable;
  Arr2ATConferenceable = array of Arr1ATConferenceable;
  Arr3ATConferenceable = array of Arr2ATConferenceable;

  ATRemoteConference = class;
  Arr1ATRemoteConference = array of ATRemoteConference;
  Arr2ATRemoteConference = array of Arr1ATRemoteConference;
  Arr3ATRemoteConference = array of Arr2ATRemoteConference;

  ATTelecomManager = class;
  Arr1ATTelecomManager = array of ATTelecomManager;
  Arr2ATTelecomManager = array of Arr1ATTelecomManager;
  Arr3ATTelecomManager = array of Arr2ATTelecomManager;

  ATCarrierConfigManager = class;
  Arr1ATCarrierConfigManager = array of ATCarrierConfigManager;
  Arr2ATCarrierConfigManager = array of Arr1ATCarrierConfigManager;
  Arr3ATCarrierConfigManager = array of Arr2ATCarrierConfigManager;

  ATCellSignalStrength = class;
  Arr1ATCellSignalStrength = array of ATCellSignalStrength;
  Arr2ATCellSignalStrength = array of Arr1ATCellSignalStrength;
  Arr3ATCellSignalStrength = array of Arr2ATCellSignalStrength;

  ATSubscriptionManager = class;
  Arr1ATSubscriptionManager = array of ATSubscriptionManager;
  Arr2ATSubscriptionManager = array of Arr1ATSubscriptionManager;
  Arr3ATSubscriptionManager = array of Arr2ATSubscriptionManager;

  ATBidiFormatter = class;
  Arr1ATBidiFormatter = array of ATBidiFormatter;
  Arr2ATBidiFormatter = array of Arr1ATBidiFormatter;
  Arr3ATBidiFormatter = array of Arr2ATBidiFormatter;

  ATTextDirectionHeuristic = interface;
  Arr1ATTextDirectionHeuristic = array of ATTextDirectionHeuristic;
  Arr2ATTextDirectionHeuristic = array of Arr1ATTextDirectionHeuristic;
  Arr3ATTextDirectionHeuristic = array of Arr2ATTextDirectionHeuristic;

  ATTextDirectionHeuristics = class;
  Arr1ATTextDirectionHeuristics = array of ATTextDirectionHeuristics;
  Arr2ATTextDirectionHeuristics = array of Arr1ATTextDirectionHeuristics;
  Arr3ATTextDirectionHeuristics = array of Arr2ATTextDirectionHeuristics;

  ATPathMotion = class;
  Arr1ATPathMotion = array of ATPathMotion;
  Arr2ATPathMotion = array of Arr1ATPathMotion;
  Arr3ATPathMotion = array of Arr2ATPathMotion;

  ATScene = class;
  Arr1ATScene = array of ATScene;
  Arr2ATScene = array of Arr1ATScene;
  Arr3ATScene = array of Arr2ATScene;

  ATTransitionInflater = class;
  Arr1ATTransitionInflater = array of ATTransitionInflater;
  Arr2ATTransitionInflater = array of Arr1ATTransitionInflater;
  Arr3ATTransitionInflater = array of Arr2ATTransitionInflater;

  ATTransitionManager = class;
  Arr1ATTransitionManager = array of ATTransitionManager;
  Arr2ATTransitionManager = array of Arr1ATTransitionManager;
  Arr3ATTransitionManager = array of Arr2ATTransitionManager;

  ATTransitionPropagation = class;
  Arr1ATTransitionPropagation = array of ATTransitionPropagation;
  Arr2ATTransitionPropagation = array of Arr1ATTransitionPropagation;
  Arr3ATTransitionPropagation = array of Arr2ATTransitionPropagation;

  ATTransitionValues = class;
  Arr1ATTransitionValues = array of ATTransitionValues;
  Arr2ATTransitionValues = array of Arr1ATTransitionValues;
  Arr3ATTransitionValues = array of Arr2ATTransitionValues;

  AUAtomicFile = class;
  Arr1AUAtomicFile = array of AUAtomicFile;
  Arr2AUAtomicFile = array of Arr1AUAtomicFile;
  Arr3AUAtomicFile = array of Arr2AUAtomicFile;

  AULayoutDirection = class;
  Arr1AULayoutDirection = array of AULayoutDirection;
  Arr2AULayoutDirection = array of Arr1AULayoutDirection;
  Arr3AULayoutDirection = array of Arr2AULayoutDirection;

  AUMutableBoolean = class;
  Arr1AUMutableBoolean = array of AUMutableBoolean;
  Arr2AUMutableBoolean = array of Arr1AUMutableBoolean;
  Arr3AUMutableBoolean = array of Arr2AUMutableBoolean;

  AUMutableByte = class;
  Arr1AUMutableByte = array of AUMutableByte;
  Arr2AUMutableByte = array of Arr1AUMutableByte;
  Arr3AUMutableByte = array of Arr2AUMutableByte;

  AUMutableChar = class;
  Arr1AUMutableChar = array of AUMutableChar;
  Arr2AUMutableChar = array of Arr1AUMutableChar;
  Arr3AUMutableChar = array of Arr2AUMutableChar;

  AUMutableDouble = class;
  Arr1AUMutableDouble = array of AUMutableDouble;
  Arr2AUMutableDouble = array of Arr1AUMutableDouble;
  Arr3AUMutableDouble = array of Arr2AUMutableDouble;

  AUMutableFloat = class;
  Arr1AUMutableFloat = array of AUMutableFloat;
  Arr2AUMutableFloat = array of Arr1AUMutableFloat;
  Arr3AUMutableFloat = array of Arr2AUMutableFloat;

  AUMutableInt = class;
  Arr1AUMutableInt = array of AUMutableInt;
  Arr2AUMutableInt = array of Arr1AUMutableInt;
  Arr3AUMutableInt = array of Arr2AUMutableInt;

  AUMutableLong = class;
  Arr1AUMutableLong = array of AUMutableLong;
  Arr2AUMutableLong = array of Arr1AUMutableLong;
  Arr3AUMutableLong = array of Arr2AUMutableLong;

  AUMutableShort = class;
  Arr1AUMutableShort = array of AUMutableShort;
  Arr2AUMutableShort = array of Arr1AUMutableShort;
  Arr3AUMutableShort = array of Arr2AUMutableShort;

  AURange = class;
  Arr1AURange = array of AURange;
  Arr2AURange = array of Arr1AURange;
  Arr3AURange = array of Arr2AURange;

  AUSize = class;
  Arr1AUSize = array of AUSize;
  Arr2AUSize = array of Arr1AUSize;
  Arr3AUSize = array of Arr2AUSize;

  AUSizeF = class;
  Arr1AUSizeF = array of AUSizeF;
  Arr2AUSizeF = array of Arr1AUSizeF;
  Arr3AUSizeF = array of Arr2AUSizeF;

  AVChoreographer = class;
  Arr1AVChoreographer = array of AVChoreographer;
  Arr2AVChoreographer = array of Arr1AVChoreographer;
  Arr3AVChoreographer = array of Arr2AVChoreographer;

  AVFrameMetrics = class;
  Arr1AVFrameMetrics = array of AVFrameMetrics;
  Arr2AVFrameMetrics = array of Arr1AVFrameMetrics;
  Arr3AVFrameMetrics = array of Arr2AVFrameMetrics;

  AVFrameStats = class;
  Arr1AVFrameStats = array of AVFrameStats;
  Arr2AVFrameStats = array of Arr1AVFrameStats;
  Arr3AVFrameStats = array of Arr2AVFrameStats;

  AVPixelCopy = class;
  Arr1AVPixelCopy = array of AVPixelCopy;
  Arr2AVPixelCopy = array of Arr1AVPixelCopy;
  Arr3AVPixelCopy = array of Arr2AVPixelCopy;

  AVSearchEvent = class;
  Arr1AVSearchEvent = array of AVSearchEvent;
  Arr2AVSearchEvent = array of Arr1AVSearchEvent;
  Arr3AVSearchEvent = array of Arr2AVSearchEvent;

  AVViewAnimationUtils = class;
  Arr1AVViewAnimationUtils = array of AVViewAnimationUtils;
  Arr2AVViewAnimationUtils = array of Arr1AVViewAnimationUtils;
  Arr3AVViewAnimationUtils = array of Arr2AVViewAnimationUtils;

  AVViewOutlineProvider = class;
  Arr1AVViewOutlineProvider = array of AVViewOutlineProvider;
  Arr2AVViewOutlineProvider = array of Arr1AVViewOutlineProvider;
  Arr3AVViewOutlineProvider = array of Arr2AVViewOutlineProvider;

  AVViewOverlay = class;
  Arr1AVViewOverlay = array of AVViewOverlay;
  Arr2AVViewOverlay = array of Arr1AVViewOverlay;
  Arr3AVViewOverlay = array of Arr2AVViewOverlay;

  AVViewStructure = class;
  Arr1AVViewStructure = array of AVViewStructure;
  Arr2AVViewStructure = array of Arr1AVViewStructure;
  Arr3AVViewStructure = array of Arr2AVViewStructure;

  AVWindowInsets = class;
  Arr1AVWindowInsets = array of AVWindowInsets;
  Arr2AVWindowInsets = array of Arr1AVWindowInsets;
  Arr3AVWindowInsets = array of Arr2AVWindowInsets;

  AVAAccessibilityNodeProvider = class;
  Arr1AVAAccessibilityNodeProvider = array of AVAAccessibilityNodeProvider;
  Arr2AVAAccessibilityNodeProvider = array of Arr1AVAAccessibilityNodeProvider;
  Arr3AVAAccessibilityNodeProvider = array of Arr2AVAAccessibilityNodeProvider;

  AVACaptioningManager = class;
  Arr1AVACaptioningManager = array of AVACaptioningManager;
  Arr2AVACaptioningManager = array of Arr1AVACaptioningManager;
  Arr3AVACaptioningManager = array of Arr2AVACaptioningManager;

  AWClientCertRequest = class;
  Arr1AWClientCertRequest = array of AWClientCertRequest;
  Arr2AWClientCertRequest = array of Arr1AWClientCertRequest;
  Arr3AWClientCertRequest = array of Arr2AWClientCertRequest;

  AWPermissionRequest = class;
  Arr1AWPermissionRequest = array of AWPermissionRequest;
  Arr2AWPermissionRequest = array of Arr1AWPermissionRequest;
  Arr3AWPermissionRequest = array of Arr2AWPermissionRequest;

  AWServiceWorkerClient = class;
  Arr1AWServiceWorkerClient = array of AWServiceWorkerClient;
  Arr2AWServiceWorkerClient = array of Arr1AWServiceWorkerClient;
  Arr3AWServiceWorkerClient = array of Arr2AWServiceWorkerClient;

  AWServiceWorkerController = class;
  Arr1AWServiceWorkerController = array of AWServiceWorkerController;
  Arr2AWServiceWorkerController = array of Arr1AWServiceWorkerController;
  Arr3AWServiceWorkerController = array of Arr2AWServiceWorkerController;

  AWServiceWorkerWebSettings = class;
  Arr1AWServiceWorkerWebSettings = array of AWServiceWorkerWebSettings;
  Arr2AWServiceWorkerWebSettings = array of Arr1AWServiceWorkerWebSettings;
  Arr3AWServiceWorkerWebSettings = array of Arr2AWServiceWorkerWebSettings;

  AWWebMessage = class;
  Arr1AWWebMessage = array of AWWebMessage;
  Arr2AWWebMessage = array of Arr1AWWebMessage;
  Arr3AWWebMessage = array of Arr2AWWebMessage;

  AWWebMessagePort = class;
  Arr1AWWebMessagePort = array of AWWebMessagePort;
  Arr2AWWebMessagePort = array of Arr1AWWebMessagePort;
  Arr3AWWebMessagePort = array of Arr2AWWebMessagePort;

  AWWebResourceError = class;
  Arr1AWWebResourceError = array of AWWebResourceError;
  Arr2AWWebResourceError = array of Arr1AWWebResourceError;
  Arr3AWWebResourceError = array of Arr2AWWebResourceError;

  AWWebResourceRequest = interface;
  Arr1AWWebResourceRequest = array of AWWebResourceRequest;
  Arr2AWWebResourceRequest = array of Arr1AWWebResourceRequest;
  Arr3AWWebResourceRequest = array of Arr2AWWebResourceRequest;

  JNProtocolFamily = interface;
  Arr1JNProtocolFamily = array of JNProtocolFamily;
  Arr2JNProtocolFamily = array of Arr1JNProtocolFamily;
  Arr3JNProtocolFamily = array of Arr2JNProtocolFamily;

  JNSocketOption = interface;
  Arr1JNSocketOption = array of JNSocketOption;
  Arr2JNSocketOption = array of Arr1JNSocketOption;
  Arr3JNSocketOption = array of Arr2JNSocketOption;

  JNStandardSocketOptions = class;
  Arr1JNStandardSocketOptions = array of JNStandardSocketOptions;
  Arr2JNStandardSocketOptions = array of Arr1JNStandardSocketOptions;
  Arr3JNStandardSocketOptions = array of Arr2JNStandardSocketOptions;

  JSAlgorithmConstraints = interface;
  Arr1JSAlgorithmConstraints = array of JSAlgorithmConstraints;
  Arr2JSAlgorithmConstraints = array of Arr1JSAlgorithmConstraints;
  Arr3JSAlgorithmConstraints = array of Arr2JSAlgorithmConstraints;

  JSCExtension = interface;
  Arr1JSCExtension = array of JSCExtension;
  Arr2JSCExtension = array of Arr1JSCExtension;
  Arr3JSCExtension = array of Arr2JSCExtension;

  JUObjects = class;
  Arr1JUObjects = array of JUObjects;
  Arr2JUObjects = array of Arr1JUObjects;
  Arr3JUObjects = array of Arr2JUObjects;

  JUOptional = class;
  Arr1JUOptional = array of JUOptional;
  Arr2JUOptional = array of Arr1JUOptional;
  Arr3JUOptional = array of Arr2JUOptional;

  JUOptionalDouble = class;
  Arr1JUOptionalDouble = array of JUOptionalDouble;
  Arr2JUOptionalDouble = array of Arr1JUOptionalDouble;
  Arr3JUOptionalDouble = array of Arr2JUOptionalDouble;

  JUOptionalInt = class;
  Arr1JUOptionalInt = array of JUOptionalInt;
  Arr2JUOptionalInt = array of Arr1JUOptionalInt;
  Arr3JUOptionalInt = array of Arr2JUOptionalInt;

  JUOptionalLong = class;
  Arr1JUOptionalLong = array of JUOptionalLong;
  Arr2JUOptionalLong = array of Arr1JUOptionalLong;
  Arr3JUOptionalLong = array of Arr2JUOptionalLong;

  JUSpliterator = interface;
  Arr1JUSpliterator = array of JUSpliterator;
  Arr2JUSpliterator = array of Arr1JUSpliterator;
  Arr3JUSpliterator = array of Arr2JUSpliterator;

  JUSplittableRandom = class;
  Arr1JUSplittableRandom = array of JUSplittableRandom;
  Arr2JUSplittableRandom = array of Arr1JUSplittableRandom;
  Arr3JUSplittableRandom = array of Arr2JUSplittableRandom;

  JUStringJoiner = class;
  Arr1JUStringJoiner = array of JUStringJoiner;
  Arr2JUStringJoiner = array of Arr1JUStringJoiner;
  Arr3JUStringJoiner = array of Arr2JUStringJoiner;

  JUCCompletionStage = interface;
  Arr1JUCCompletionStage = array of JUCCompletionStage;
  Arr2JUCCompletionStage = array of Arr1JUCCompletionStage;
  Arr3JUCCompletionStage = array of Arr2JUCCompletionStage;

  JUCPhaser = class;
  Arr1JUCPhaser = array of JUCPhaser;
  Arr2JUCPhaser = array of Arr1JUCPhaser;
  Arr3JUCPhaser = array of Arr2JUCPhaser;

  JUFBiConsumer = interface;
  Arr1JUFBiConsumer = array of JUFBiConsumer;
  Arr2JUFBiConsumer = array of Arr1JUFBiConsumer;
  Arr3JUFBiConsumer = array of Arr2JUFBiConsumer;

  JUFBiFunction = interface;
  Arr1JUFBiFunction = array of JUFBiFunction;
  Arr2JUFBiFunction = array of Arr1JUFBiFunction;
  Arr3JUFBiFunction = array of Arr2JUFBiFunction;

  JUFBiPredicate = interface;
  Arr1JUFBiPredicate = array of JUFBiPredicate;
  Arr2JUFBiPredicate = array of Arr1JUFBiPredicate;
  Arr3JUFBiPredicate = array of Arr2JUFBiPredicate;

  JUFBooleanSupplier = interface;
  Arr1JUFBooleanSupplier = array of JUFBooleanSupplier;
  Arr2JUFBooleanSupplier = array of Arr1JUFBooleanSupplier;
  Arr3JUFBooleanSupplier = array of Arr2JUFBooleanSupplier;

  JUFConsumer = interface;
  Arr1JUFConsumer = array of JUFConsumer;
  Arr2JUFConsumer = array of Arr1JUFConsumer;
  Arr3JUFConsumer = array of Arr2JUFConsumer;

  JUFDoubleBinaryOperator = interface;
  Arr1JUFDoubleBinaryOperator = array of JUFDoubleBinaryOperator;
  Arr2JUFDoubleBinaryOperator = array of Arr1JUFDoubleBinaryOperator;
  Arr3JUFDoubleBinaryOperator = array of Arr2JUFDoubleBinaryOperator;

  JUFDoubleConsumer = interface;
  Arr1JUFDoubleConsumer = array of JUFDoubleConsumer;
  Arr2JUFDoubleConsumer = array of Arr1JUFDoubleConsumer;
  Arr3JUFDoubleConsumer = array of Arr2JUFDoubleConsumer;

  JUFDoubleFunction = interface;
  Arr1JUFDoubleFunction = array of JUFDoubleFunction;
  Arr2JUFDoubleFunction = array of Arr1JUFDoubleFunction;
  Arr3JUFDoubleFunction = array of Arr2JUFDoubleFunction;

  JUFDoublePredicate = interface;
  Arr1JUFDoublePredicate = array of JUFDoublePredicate;
  Arr2JUFDoublePredicate = array of Arr1JUFDoublePredicate;
  Arr3JUFDoublePredicate = array of Arr2JUFDoublePredicate;

  JUFDoubleSupplier = interface;
  Arr1JUFDoubleSupplier = array of JUFDoubleSupplier;
  Arr2JUFDoubleSupplier = array of Arr1JUFDoubleSupplier;
  Arr3JUFDoubleSupplier = array of Arr2JUFDoubleSupplier;

  JUFDoubleToIntFunction = interface;
  Arr1JUFDoubleToIntFunction = array of JUFDoubleToIntFunction;
  Arr2JUFDoubleToIntFunction = array of Arr1JUFDoubleToIntFunction;
  Arr3JUFDoubleToIntFunction = array of Arr2JUFDoubleToIntFunction;

  JUFDoubleToLongFunction = interface;
  Arr1JUFDoubleToLongFunction = array of JUFDoubleToLongFunction;
  Arr2JUFDoubleToLongFunction = array of Arr1JUFDoubleToLongFunction;
  Arr3JUFDoubleToLongFunction = array of Arr2JUFDoubleToLongFunction;

  JUFDoubleUnaryOperator = interface;
  Arr1JUFDoubleUnaryOperator = array of JUFDoubleUnaryOperator;
  Arr2JUFDoubleUnaryOperator = array of Arr1JUFDoubleUnaryOperator;
  Arr3JUFDoubleUnaryOperator = array of Arr2JUFDoubleUnaryOperator;

  JUFFunction = interface;
  Arr1JUFFunction = array of JUFFunction;
  Arr2JUFFunction = array of Arr1JUFFunction;
  Arr3JUFFunction = array of Arr2JUFFunction;

  JUFIntBinaryOperator = interface;
  Arr1JUFIntBinaryOperator = array of JUFIntBinaryOperator;
  Arr2JUFIntBinaryOperator = array of Arr1JUFIntBinaryOperator;
  Arr3JUFIntBinaryOperator = array of Arr2JUFIntBinaryOperator;

  JUFIntConsumer = interface;
  Arr1JUFIntConsumer = array of JUFIntConsumer;
  Arr2JUFIntConsumer = array of Arr1JUFIntConsumer;
  Arr3JUFIntConsumer = array of Arr2JUFIntConsumer;

  JUFIntFunction = interface;
  Arr1JUFIntFunction = array of JUFIntFunction;
  Arr2JUFIntFunction = array of Arr1JUFIntFunction;
  Arr3JUFIntFunction = array of Arr2JUFIntFunction;

  JUFIntPredicate = interface;
  Arr1JUFIntPredicate = array of JUFIntPredicate;
  Arr2JUFIntPredicate = array of Arr1JUFIntPredicate;
  Arr3JUFIntPredicate = array of Arr2JUFIntPredicate;

  JUFIntSupplier = interface;
  Arr1JUFIntSupplier = array of JUFIntSupplier;
  Arr2JUFIntSupplier = array of Arr1JUFIntSupplier;
  Arr3JUFIntSupplier = array of Arr2JUFIntSupplier;

  JUFIntToDoubleFunction = interface;
  Arr1JUFIntToDoubleFunction = array of JUFIntToDoubleFunction;
  Arr2JUFIntToDoubleFunction = array of Arr1JUFIntToDoubleFunction;
  Arr3JUFIntToDoubleFunction = array of Arr2JUFIntToDoubleFunction;

  JUFIntToLongFunction = interface;
  Arr1JUFIntToLongFunction = array of JUFIntToLongFunction;
  Arr2JUFIntToLongFunction = array of Arr1JUFIntToLongFunction;
  Arr3JUFIntToLongFunction = array of Arr2JUFIntToLongFunction;

  JUFIntUnaryOperator = interface;
  Arr1JUFIntUnaryOperator = array of JUFIntUnaryOperator;
  Arr2JUFIntUnaryOperator = array of Arr1JUFIntUnaryOperator;
  Arr3JUFIntUnaryOperator = array of Arr2JUFIntUnaryOperator;

  JUFLongBinaryOperator = interface;
  Arr1JUFLongBinaryOperator = array of JUFLongBinaryOperator;
  Arr2JUFLongBinaryOperator = array of Arr1JUFLongBinaryOperator;
  Arr3JUFLongBinaryOperator = array of Arr2JUFLongBinaryOperator;

  JUFLongConsumer = interface;
  Arr1JUFLongConsumer = array of JUFLongConsumer;
  Arr2JUFLongConsumer = array of Arr1JUFLongConsumer;
  Arr3JUFLongConsumer = array of Arr2JUFLongConsumer;

  JUFLongFunction = interface;
  Arr1JUFLongFunction = array of JUFLongFunction;
  Arr2JUFLongFunction = array of Arr1JUFLongFunction;
  Arr3JUFLongFunction = array of Arr2JUFLongFunction;

  JUFLongPredicate = interface;
  Arr1JUFLongPredicate = array of JUFLongPredicate;
  Arr2JUFLongPredicate = array of Arr1JUFLongPredicate;
  Arr3JUFLongPredicate = array of Arr2JUFLongPredicate;

  JUFLongSupplier = interface;
  Arr1JUFLongSupplier = array of JUFLongSupplier;
  Arr2JUFLongSupplier = array of Arr1JUFLongSupplier;
  Arr3JUFLongSupplier = array of Arr2JUFLongSupplier;

  JUFLongToDoubleFunction = interface;
  Arr1JUFLongToDoubleFunction = array of JUFLongToDoubleFunction;
  Arr2JUFLongToDoubleFunction = array of Arr1JUFLongToDoubleFunction;
  Arr3JUFLongToDoubleFunction = array of Arr2JUFLongToDoubleFunction;

  JUFLongToIntFunction = interface;
  Arr1JUFLongToIntFunction = array of JUFLongToIntFunction;
  Arr2JUFLongToIntFunction = array of Arr1JUFLongToIntFunction;
  Arr3JUFLongToIntFunction = array of Arr2JUFLongToIntFunction;

  JUFLongUnaryOperator = interface;
  Arr1JUFLongUnaryOperator = array of JUFLongUnaryOperator;
  Arr2JUFLongUnaryOperator = array of Arr1JUFLongUnaryOperator;
  Arr3JUFLongUnaryOperator = array of Arr2JUFLongUnaryOperator;

  JUFObjDoubleConsumer = interface;
  Arr1JUFObjDoubleConsumer = array of JUFObjDoubleConsumer;
  Arr2JUFObjDoubleConsumer = array of Arr1JUFObjDoubleConsumer;
  Arr3JUFObjDoubleConsumer = array of Arr2JUFObjDoubleConsumer;

  JUFObjIntConsumer = interface;
  Arr1JUFObjIntConsumer = array of JUFObjIntConsumer;
  Arr2JUFObjIntConsumer = array of Arr1JUFObjIntConsumer;
  Arr3JUFObjIntConsumer = array of Arr2JUFObjIntConsumer;

  JUFObjLongConsumer = interface;
  Arr1JUFObjLongConsumer = array of JUFObjLongConsumer;
  Arr2JUFObjLongConsumer = array of Arr1JUFObjLongConsumer;
  Arr3JUFObjLongConsumer = array of Arr2JUFObjLongConsumer;

  JUFPredicate = interface;
  Arr1JUFPredicate = array of JUFPredicate;
  Arr2JUFPredicate = array of Arr1JUFPredicate;
  Arr3JUFPredicate = array of Arr2JUFPredicate;

  JUFSupplier = interface;
  Arr1JUFSupplier = array of JUFSupplier;
  Arr2JUFSupplier = array of Arr1JUFSupplier;
  Arr3JUFSupplier = array of Arr2JUFSupplier;

  JUFToDoubleBiFunction = interface;
  Arr1JUFToDoubleBiFunction = array of JUFToDoubleBiFunction;
  Arr2JUFToDoubleBiFunction = array of Arr1JUFToDoubleBiFunction;
  Arr3JUFToDoubleBiFunction = array of Arr2JUFToDoubleBiFunction;

  JUFToDoubleFunction = interface;
  Arr1JUFToDoubleFunction = array of JUFToDoubleFunction;
  Arr2JUFToDoubleFunction = array of Arr1JUFToDoubleFunction;
  Arr3JUFToDoubleFunction = array of Arr2JUFToDoubleFunction;

  JUFToIntBiFunction = interface;
  Arr1JUFToIntBiFunction = array of JUFToIntBiFunction;
  Arr2JUFToIntBiFunction = array of Arr1JUFToIntBiFunction;
  Arr3JUFToIntBiFunction = array of Arr2JUFToIntBiFunction;

  JUFToIntFunction = interface;
  Arr1JUFToIntFunction = array of JUFToIntFunction;
  Arr2JUFToIntFunction = array of Arr1JUFToIntFunction;
  Arr3JUFToIntFunction = array of Arr2JUFToIntFunction;

  JUFToLongBiFunction = interface;
  Arr1JUFToLongBiFunction = array of JUFToLongBiFunction;
  Arr2JUFToLongBiFunction = array of Arr1JUFToLongBiFunction;
  Arr3JUFToLongBiFunction = array of Arr2JUFToLongBiFunction;

  JUFToLongFunction = interface;
  Arr1JUFToLongFunction = array of JUFToLongFunction;
  Arr2JUFToLongFunction = array of Arr1JUFToLongFunction;
  Arr3JUFToLongFunction = array of Arr2JUFToLongFunction;

  JUSCollectors = class;
  Arr1JUSCollectors = array of JUSCollectors;
  Arr2JUSCollectors = array of Arr1JUSCollectors;
  Arr3JUSCollectors = array of Arr2JUSCollectors;

  JNSSNIMatcher = class;
  Arr1JNSSNIMatcher = array of JNSSNIMatcher;
  Arr2JNSSNIMatcher = array of Arr1JNSSNIMatcher;
  Arr3JNSSNIMatcher = array of Arr2JNSSNIMatcher;

  JNSSNIServerName = class;
  Arr1JNSSNIServerName = array of JNSSNIServerName;
  Arr2JNSSNIServerName = array of Arr1JNSSNIServerName;
  Arr3JNSSNIServerName = array of Arr2JNSSNIServerName;

  JNSStandardConstants = class;
  Arr1JNSStandardConstants = array of JNSStandardConstants;
  Arr2JNSStandardConstants = array of Arr1JNSStandardConstants;
  Arr3JNSStandardConstants = array of Arr2JNSStandardConstants;

  AMMediaCryptoException = class;
  Arr1AMMediaCryptoException = array of AMMediaCryptoException;
  Arr2AMMediaCryptoException = array of Arr1AMMediaCryptoException;
  Arr3AMMediaCryptoException = array of Arr2AMMediaCryptoException;

  AMMediaDrmException = class;
  Arr1AMMediaDrmException = array of AMMediaDrmException;
  Arr2AMMediaDrmException = array of Arr1AMMediaDrmException;
  Arr3AMMediaDrmException = array of Arr2AMMediaDrmException;

  ASErrnoException = class;
  Arr1ASErrnoException = array of ASErrnoException;
  Arr2ASErrnoException = array of Arr1ASErrnoException;
  Arr3ASErrnoException = array of Arr2ASErrnoException;

  AAStateListAnimator = class;
  Arr1AAStateListAnimator = array of AAStateListAnimator;
  Arr2AAStateListAnimator = array of Arr1AAStateListAnimator;
  Arr3AAStateListAnimator = array of Arr2AAStateListAnimator;

  AITBreakIterator = class;
  Arr1AITBreakIterator = array of AITBreakIterator;
  Arr2AITBreakIterator = array of Arr1AITBreakIterator;
  Arr3AITBreakIterator = array of Arr2AITBreakIterator;

  AITNormalizer = class;
  Arr1AITNormalizer = array of AITNormalizer;
  Arr2AITNormalizer = array of Arr1AITNormalizer;
  Arr3AITNormalizer = array of Arr2AITNormalizer;

  AITUCharacterIterator = class;
  Arr1AITUCharacterIterator = array of AITUCharacterIterator;
  Arr2AITUCharacterIterator = array of Arr1AITUCharacterIterator;
  Arr3AITUCharacterIterator = array of Arr2AITUCharacterIterator;

  AIUFreezable = interface;
  Arr1AIUFreezable = array of AIUFreezable;
  Arr2AIUFreezable = array of Arr1AIUFreezable;
  Arr3AIUFreezable = array of Arr2AIUFreezable;

  ATTransition = class;
  Arr1ATTransition = array of ATTransition;
  Arr2ATTransition = array of Arr1ATTransition;
  Arr3ATTransition = array of Arr2ATTransition;

  AULongSparseArray = class;
  Arr1AULongSparseArray = array of AULongSparseArray;
  Arr2AULongSparseArray = array of Arr1AULongSparseArray;
  Arr3AULongSparseArray = array of Arr2AULongSparseArray;

  AUSparseLongArray = class;
  Arr1AUSparseLongArray = array of AUSparseLongArray;
  Arr2AUSparseLongArray = array of Arr1AUSparseLongArray;
  Arr3AUSparseLongArray = array of Arr2AUSparseLongArray;

  AIUICUUncheckedIOException = class;
  Arr1AIUICUUncheckedIOException = array of AIUICUUncheckedIOException;
  Arr2AIUICUUncheckedIOException = array of Arr1AIUICUUncheckedIOException;
  Arr3AIUICUUncheckedIOException = array of Arr2AIUICUUncheckedIOException;

  AOFileUriExposedException = class;
  Arr1AOFileUriExposedException = array of AOFileUriExposedException;
  Arr2AOFileUriExposedException = array of Arr1AOFileUriExposedException;
  Arr3AOFileUriExposedException = array of Arr2AOFileUriExposedException;

  AOOperationCanceledException = class;
  Arr1AOOperationCanceledException = array of AOOperationCanceledException;
  Arr2AOOperationCanceledException = array of Arr1AOOperationCanceledException;
  Arr3AOOperationCanceledException = array of Arr2AOOperationCanceledException;

  JIUncheckedIOException = class;
  Arr1JIUncheckedIOException = array of JIUncheckedIOException;
  Arr2JIUncheckedIOException = array of Arr1JIUncheckedIOException;
  Arr3JIUncheckedIOException = array of Arr2JIUncheckedIOException;

  JUIllformedLocaleException = class;
  Arr1JUIllformedLocaleException = array of JUIllformedLocaleException;
  Arr2JUIllformedLocaleException = array of Arr1JUIllformedLocaleException;
  Arr3JUIllformedLocaleException = array of Arr2JUIllformedLocaleException;

  JUCCompletionException = class;
  Arr1JUCCompletionException = array of JUCCompletionException;
  Arr2JUCCompletionException = array of Arr1JUCCompletionException;
  Arr3JUCCompletionException = array of Arr2JUCCompletionException;

  AITCollationKey = class;
  Arr1AITCollationKey = array of AITCollationKey;
  Arr2AITCollationKey = array of Arr1AITCollationKey;
  Arr3AITCollationKey = array of Arr2AITCollationKey;

  AIUVersionInfo = class;
  Arr1AIUVersionInfo = array of AIUVersionInfo;
  Arr2AIUVersionInfo = array of Arr1AIUVersionInfo;
  Arr3AIUVersionInfo = array of Arr2AIUVersionInfo;

  JUPrimitiveIterator = interface;
  Arr1JUPrimitiveIterator = array of JUPrimitiveIterator;
  Arr2JUPrimitiveIterator = array of Arr1JUPrimitiveIterator;
  Arr3JUPrimitiveIterator = array of Arr2JUPrimitiveIterator;

  AILUScript = class;
  Arr1AILUScript = array of AILUScript;
  Arr2AILUScript = array of Arr1AILUScript;
  Arr3AILUScript = array of Arr2AILUScript;

  AITAlphabeticIndex = class;
  Arr1AITAlphabeticIndex = array of AITAlphabeticIndex;
  Arr2AITAlphabeticIndex = array of Arr1AITAlphabeticIndex;
  Arr3AITAlphabeticIndex = array of Arr2AITAlphabeticIndex;

  AITDisplayContext = class;
  Arr1AITDisplayContext = array of AITDisplayContext;
  Arr2AITDisplayContext = array of Arr1AITDisplayContext;
  Arr3AITDisplayContext = array of Arr2AITDisplayContext;

  AITIDNA = class;
  Arr1AITIDNA = array of AITIDNA;
  Arr2AITIDNA = array of Arr1AITIDNA;
  Arr3AITIDNA = array of Arr2AITIDNA;

  AITRelativeDateTimeFormatter = class;
  Arr1AITRelativeDateTimeFormatter = array of AITRelativeDateTimeFormatter;
  Arr2AITRelativeDateTimeFormatter = array of Arr1AITRelativeDateTimeFormatter;
  Arr3AITRelativeDateTimeFormatter = array of Arr2AITRelativeDateTimeFormatter;

  AITSearchIterator = class;
  Arr1AITSearchIterator = array of AITSearchIterator;
  Arr2AITSearchIterator = array of Arr1AITSearchIterator;
  Arr3AITSearchIterator = array of Arr2AITSearchIterator;

  JSCryptoPrimitive = class;
  Arr1JSCryptoPrimitive = array of JSCryptoPrimitive;
  Arr2JSCryptoPrimitive = array of Arr1JSCryptoPrimitive;
  Arr3JSCryptoPrimitive = array of Arr2JSCryptoPrimitive;

  JSCCRLReason = class;
  Arr1JSCCRLReason = array of JSCCRLReason;
  Arr2JSCCRLReason = array of Arr1JSCCRLReason;
  Arr3JSCCRLReason = array of Arr2JSCCRLReason;

  JUSCollector = interface;
  Arr1JUSCollector = array of JUSCollector;
  Arr2JUSCollector = array of Arr1JUSCollector;
  Arr3JUSCollector = array of Arr2JUSCollector;

  AURational = class;
  Arr1AURational = array of AURational;
  Arr2AURational = array of Arr1AURational;
  Arr3AURational = array of Arr2AURational;

  JUCAStriped64 = class;
  Arr1JUCAStriped64 = array of JUCAStriped64;
  Arr2JUCAStriped64 = array of Arr1JUCAStriped64;
  Arr3JUCAStriped64 = array of Arr2JUCAStriped64;

  AIMBigDecimal = class;
  Arr1AIMBigDecimal = array of AIMBigDecimal;
  Arr2AIMBigDecimal = array of Arr1AIMBigDecimal;
  Arr3AIMBigDecimal = array of Arr2AIMBigDecimal;

  AIMMathContext = class;
  Arr1AIMMathContext = array of AIMMathContext;
  Arr2AIMMathContext = array of Arr1AIMMathContext;
  Arr3AIMMathContext = array of Arr2AIMMathContext;

  AITCurrencyPluralInfo = class;
  Arr1AITCurrencyPluralInfo = array of AITCurrencyPluralInfo;
  Arr2AITCurrencyPluralInfo = array of Arr1AITCurrencyPluralInfo;
  Arr3AITCurrencyPluralInfo = array of Arr2AITCurrencyPluralInfo;

  AITDateFormatSymbols = class;
  Arr1AITDateFormatSymbols = array of AITDateFormatSymbols;
  Arr2AITDateFormatSymbols = array of Arr1AITDateFormatSymbols;
  Arr3AITDateFormatSymbols = array of Arr2AITDateFormatSymbols;

  AITDecimalFormatSymbols = class;
  Arr1AITDecimalFormatSymbols = array of AITDecimalFormatSymbols;
  Arr2AITDecimalFormatSymbols = array of Arr1AITDecimalFormatSymbols;
  Arr3AITDecimalFormatSymbols = array of Arr2AITDecimalFormatSymbols;

  AITPluralRules = class;
  Arr1AITPluralRules = array of AITPluralRules;
  Arr2AITPluralRules = array of Arr1AITPluralRules;
  Arr3AITPluralRules = array of Arr2AITPluralRules;

  AITTimeZoneNames = class;
  Arr1AITTimeZoneNames = array of AITTimeZoneNames;
  Arr2AITTimeZoneNames = array of Arr1AITTimeZoneNames;
  Arr3AITTimeZoneNames = array of Arr2AITTimeZoneNames;

  AIUCalendar = class;
  Arr1AIUCalendar = array of AIUCalendar;
  Arr2AIUCalendar = array of Arr1AIUCalendar;
  Arr3AIUCalendar = array of Arr2AIUCalendar;

  AIUDateInterval = class;
  Arr1AIUDateInterval = array of AIUDateInterval;
  Arr2AIUDateInterval = array of Arr1AIUDateInterval;
  Arr3AIUDateInterval = array of Arr2AIUDateInterval;

  AIUMeasureUnit = class;
  Arr1AIUMeasureUnit = array of AIUMeasureUnit;
  Arr2AIUMeasureUnit = array of Arr1AIUMeasureUnit;
  Arr3AIUMeasureUnit = array of Arr2AIUMeasureUnit;

  AIUULocale = class;
  Arr1AIUULocale = array of AIUULocale;
  Arr2AIUULocale = array of Arr1AIUULocale;
  Arr3AIUULocale = array of Arr2AIUULocale;

  JUCLStampedLock = class;
  Arr1JUCLStampedLock = array of JUCLStampedLock;
  Arr2JUCLStampedLock = array of Arr1JUCLStampedLock;
  Arr3JUCLStampedLock = array of Arr2JUCLStampedLock;

  AMMediaCodec = class;
  Arr1AMMediaCodec = array of AMMediaCodec;
  Arr2AMMediaCodec = array of Arr1AMMediaCodec;
  Arr3AMMediaCodec = array of Arr2AMMediaCodec;

  AMMediaDrm = class;
  Arr1AMMediaDrm = array of AMMediaDrm;
  Arr2AMMediaDrm = array of Arr1AMMediaDrm;
  Arr3AMMediaDrm = array of Arr2AMMediaDrm;

  AMMediaDrmResetException = class;
  Arr1AMMediaDrmResetException = array of AMMediaDrmResetException;
  Arr2AMMediaDrmResetException = array of Arr1AMMediaDrmResetException;
  Arr3AMMediaDrmResetException = array of Arr2AMMediaDrmResetException;

  JNCAlreadyBoundException = class;
  Arr1JNCAlreadyBoundException = array of JNCAlreadyBoundException;
  Arr2JNCAlreadyBoundException = array of Arr1JNCAlreadyBoundException;
  Arr3JNCAlreadyBoundException = array of Arr2JNCAlreadyBoundException;

  AUArrayMap = class;
  Arr1AUArrayMap = array of AUArrayMap;
  Arr2AUArrayMap = array of Arr1AUArrayMap;
  Arr3AUArrayMap = array of Arr2AUArrayMap;

  AUArraySet = class;
  Arr1AUArraySet = array of AUArraySet;
  Arr2AUArraySet = array of Arr1AUArraySet;
  Arr3AUArraySet = array of Arr2AUArraySet;

  AABidirectionalTypeConverter = class;
  Arr1AABidirectionalTypeConverter = array of AABidirectionalTypeConverter;
  Arr2AABidirectionalTypeConverter = array of Arr1AABidirectionalTypeConverter;
  Arr3AABidirectionalTypeConverter = array of Arr2AABidirectionalTypeConverter;

  AAFloatArrayEvaluator = class;
  Arr1AAFloatArrayEvaluator = array of AAFloatArrayEvaluator;
  Arr2AAFloatArrayEvaluator = array of Arr1AAFloatArrayEvaluator;
  Arr3AAFloatArrayEvaluator = array of Arr2AAFloatArrayEvaluator;

  AAIntArrayEvaluator = class;
  Arr1AAIntArrayEvaluator = array of AAIntArrayEvaluator;
  Arr2AAIntArrayEvaluator = array of Arr1AAIntArrayEvaluator;
  Arr3AAIntArrayEvaluator = array of Arr2AAIntArrayEvaluator;

  AAPointFEvaluator = class;
  Arr1AAPointFEvaluator = array of AAPointFEvaluator;
  Arr2AAPointFEvaluator = array of Arr1AAPointFEvaluator;
  Arr3AAPointFEvaluator = array of Arr2AAPointFEvaluator;

  AARectEvaluator = class;
  Arr1AARectEvaluator = array of AARectEvaluator;
  Arr2AARectEvaluator = array of Arr1AARectEvaluator;
  Arr3AARectEvaluator = array of Arr2AARectEvaluator;

  AAFragmentHostCallback = class;
  Arr1AAFragmentHostCallback = array of AAFragmentHostCallback;
  Arr2AAFragmentHostCallback = array of Arr1AAFragmentHostCallback;
  Arr3AAFragmentHostCallback = array of Arr2AAFragmentHostCallback;

  ABBluetoothGatt = class;
  Arr1ABBluetoothGatt = array of ABBluetoothGatt;
  Arr2ABBluetoothGatt = array of Arr1ABBluetoothGatt;
  Arr3ABBluetoothGatt = array of Arr2ABBluetoothGatt;

  ABBluetoothGattServer = class;
  Arr1ABBluetoothGattServer = array of ABBluetoothGattServer;
  Arr2ABBluetoothGattServer = array of Arr1ABBluetoothGattServer;
  Arr3ABBluetoothGattServer = array of Arr2ABBluetoothGattServer;

  ASRRestrictionsReceiver = class;
  Arr1ASRRestrictionsReceiver = array of ASRRestrictionsReceiver;
  Arr2ASRRestrictionsReceiver = array of Arr1ASRRestrictionsReceiver;
  Arr3ASRRestrictionsReceiver = array of Arr2ASRRestrictionsReceiver;

  AGDAnimatable2 = interface;
  Arr1AGDAnimatable2 = array of AGDAnimatable2;
  Arr2AGDAnimatable2 = array of Arr1AGDAnimatable2;
  Arr3AGDAnimatable2 = array of Arr2AGDAnimatable2;

  APPPrintedPdfDocument = class;
  Arr1APPPrintedPdfDocument = array of APPPrintedPdfDocument;
  Arr2APPPrintedPdfDocument = array of Arr1APPPrintedPdfDocument;
  Arr3APPPrintedPdfDocument = array of Arr2APPPrintedPdfDocument;

  AHSensorEventListener2 = interface;
  Arr1AHSensorEventListener2 = array of AHSensorEventListener2;
  Arr2AHSensorEventListener2 = array of Arr1AHSensorEventListener2;
  Arr3AHSensorEventListener2 = array of Arr2AHSensorEventListener2;

  AHCCameraCharacteristics = class;
  Arr1AHCCameraCharacteristics = array of AHCCameraCharacteristics;
  Arr2AHCCameraCharacteristics = array of Arr1AHCCameraCharacteristics;
  Arr3AHCCameraCharacteristics = array of Arr2AHCCameraCharacteristics;

  AHCCaptureResult = class;
  Arr1AHCCaptureResult = array of AHCCaptureResult;
  Arr2AHCCaptureResult = array of Arr1AHCCaptureResult;
  Arr3AHCCaptureResult = array of Arr2AHCCaptureResult;

  AHDDisplayManager = class;
  Arr1AHDDisplayManager = array of AHDDisplayManager;
  Arr2AHDDisplayManager = array of Arr1AHDDisplayManager;
  Arr3AHDDisplayManager = array of Arr2AHDDisplayManager;

  AMPMediaProjection = class;
  Arr1AMPMediaProjection = array of AMPMediaProjection;
  Arr2AMPMediaProjection = array of Arr1AMPMediaProjection;
  Arr3AMPMediaProjection = array of Arr2AMPMediaProjection;

  AILUCharacter = class;
  Arr1AILUCharacter = array of AILUCharacter;
  Arr2AILUCharacter = array of Arr1AILUCharacter;
  Arr3AILUCharacter = array of Arr2AILUCharacter;

  AILUCharacterCategory = class;
  Arr1AILUCharacterCategory = array of AILUCharacterCategory;
  Arr2AILUCharacterCategory = array of Arr1AILUCharacterCategory;
  Arr3AILUCharacterCategory = array of Arr2AILUCharacterCategory;

  AILUCharacterDirection = class;
  Arr1AILUCharacterDirection = array of AILUCharacterDirection;
  Arr2AILUCharacterDirection = array of Arr1AILUCharacterDirection;
  Arr3AILUCharacterDirection = array of Arr2AILUCharacterDirection;

  AITUnicodeFilter = class;
  Arr1AITUnicodeFilter = array of AITUnicodeFilter;
  Arr2AITUnicodeFilter = array of Arr1AITUnicodeFilter;
  Arr3AITUnicodeFilter = array of Arr2AITUnicodeFilter;

  AIUCurrencyAmount = class;
  Arr1AIUCurrencyAmount = array of AIUCurrencyAmount;
  Arr2AIUCurrencyAmount = array of Arr1AIUCurrencyAmount;
  Arr3AIUCurrencyAmount = array of Arr2AIUCurrencyAmount;

  AMAAcousticEchoCanceler = class;
  Arr1AMAAcousticEchoCanceler = array of AMAAcousticEchoCanceler;
  Arr2AMAAcousticEchoCanceler = array of Arr1AMAAcousticEchoCanceler;
  Arr3AMAAcousticEchoCanceler = array of Arr2AMAAcousticEchoCanceler;

  AMAAutomaticGainControl = class;
  Arr1AMAAutomaticGainControl = array of AMAAutomaticGainControl;
  Arr2AMAAutomaticGainControl = array of Arr1AMAAutomaticGainControl;
  Arr3AMAAutomaticGainControl = array of Arr2AMAAutomaticGainControl;

  AMALoudnessEnhancer = class;
  Arr1AMALoudnessEnhancer = array of AMALoudnessEnhancer;
  Arr2AMALoudnessEnhancer = array of Arr1AMALoudnessEnhancer;
  Arr3AMALoudnessEnhancer = array of Arr2AMALoudnessEnhancer;

  AMANoiseSuppressor = class;
  Arr1AMANoiseSuppressor = array of AMANoiseSuppressor;
  Arr2AMANoiseSuppressor = array of Arr1AMANoiseSuppressor;
  Arr3AMANoiseSuppressor = array of Arr2AMANoiseSuppressor;

  AOEGLConfig = class;
  Arr1AOEGLConfig = array of AOEGLConfig;
  Arr2AOEGLConfig = array of Arr1AOEGLConfig;
  Arr3AOEGLConfig = array of Arr2AOEGLConfig;

  AOEGLContext = class;
  Arr1AOEGLContext = array of AOEGLContext;
  Arr2AOEGLContext = array of Arr1AOEGLContext;
  Arr3AOEGLContext = array of Arr2AOEGLContext;

  AOEGLDisplay = class;
  Arr1AOEGLDisplay = array of AOEGLDisplay;
  Arr2AOEGLDisplay = array of Arr1AOEGLDisplay;
  Arr3AOEGLDisplay = array of Arr2AOEGLDisplay;

  AOEGLSurface = class;
  Arr1AOEGLSurface = array of AOEGLSurface;
  Arr2AOEGLSurface = array of Arr1AOEGLSurface;
  Arr3AOEGLSurface = array of Arr2AOEGLSurface;

  AOGLES30 = class;
  Arr1AOGLES30 = array of AOGLES30;
  Arr2AOGLES30 = array of Arr1AOGLES30;
  Arr3AOGLES30 = array of Arr2AOGLES30;

  AAAutomaticZenRule = class;
  Arr1AAAutomaticZenRule = array of AAAutomaticZenRule;
  Arr2AAAutomaticZenRule = array of Arr1AAAutomaticZenRule;
  Arr3AAAutomaticZenRule = array of Arr2AAAutomaticZenRule;

  AARemoteInput = class;
  Arr1AARemoteInput = array of AARemoteInput;
  Arr2AARemoteInput = array of Arr1AARemoteInput;
  Arr3AARemoteInput = array of Arr2AARemoteInput;

  AAVoiceInteractor = class;
  Arr1AAVoiceInteractor = array of AAVoiceInteractor;
  Arr2AAVoiceInteractor = array of Arr1AAVoiceInteractor;
  Arr3AAVoiceInteractor = array of Arr2AAVoiceInteractor;

  AAASecurityLog = class;
  Arr1AAASecurityLog = array of AAASecurityLog;
  Arr2AAASecurityLog = array of Arr1AAASecurityLog;
  Arr3AAASecurityLog = array of Arr2AAASecurityLog;

  AAASystemUpdatePolicy = class;
  Arr1AAASystemUpdatePolicy = array of AAASystemUpdatePolicy;
  Arr2AAASystemUpdatePolicy = array of Arr1AAASystemUpdatePolicy;
  Arr3AAASystemUpdatePolicy = array of Arr2AAASystemUpdatePolicy;

  AAAAssistContent = class;
  Arr1AAAAssistContent = array of AAAAssistContent;
  Arr2AAAAssistContent = array of Arr1AAAAssistContent;
  Arr3AAAAssistContent = array of Arr2AAAAssistContent;

  AAAAssistStructure = class;
  Arr1AAAAssistStructure = array of AAAAssistStructure;
  Arr2AAAAssistStructure = array of Arr1AAAAssistStructure;
  Arr3AAAAssistStructure = array of Arr2AAAAssistStructure;

  AAJJobInfo = class;
  Arr1AAJJobInfo = array of AAJJobInfo;
  Arr2AAJJobInfo = array of Arr1AAJJobInfo;
  Arr3AAJJobInfo = array of Arr2AAJJobInfo;

  AAJJobParameters = class;
  Arr1AAJJobParameters = array of AAJJobParameters;
  Arr2AAJJobParameters = array of Arr1AAJJobParameters;
  Arr3AAJJobParameters = array of Arr2AAJJobParameters;

  AAUConfigurationStats = class;
  Arr1AAUConfigurationStats = array of AAUConfigurationStats;
  Arr2AAUConfigurationStats = array of Arr1AAUConfigurationStats;
  Arr3AAUConfigurationStats = array of Arr2AAUConfigurationStats;

  AAUUsageEvents = class;
  Arr1AAUUsageEvents = array of AAUUsageEvents;
  Arr2AAUUsageEvents = array of Arr1AAUUsageEvents;
  Arr3AAUUsageEvents = array of Arr2AAUUsageEvents;

  AAUUsageStats = class;
  Arr1AAUUsageStats = array of AAUUsageStats;
  Arr2AAUUsageStats = array of Arr1AAUUsageStats;
  Arr3AAUUsageStats = array of Arr2AAUUsageStats;

  ABBluetoothGattCharacteristic = class;
  Arr1ABBluetoothGattCharacteristic = array of ABBluetoothGattCharacteristic;
  Arr2ABBluetoothGattCharacteristic = array of Arr1ABBluetoothGattCharacteristic;
  Arr3ABBluetoothGattCharacteristic = array of Arr2ABBluetoothGattCharacteristic;

  ABBluetoothGattDescriptor = class;
  Arr1ABBluetoothGattDescriptor = array of ABBluetoothGattDescriptor;
  Arr2ABBluetoothGattDescriptor = array of Arr1ABBluetoothGattDescriptor;
  Arr3ABBluetoothGattDescriptor = array of Arr2ABBluetoothGattDescriptor;

  ABBluetoothGattService = class;
  Arr1ABBluetoothGattService = array of ABBluetoothGattService;
  Arr2ABBluetoothGattService = array of Arr1ABBluetoothGattService;
  Arr3ABBluetoothGattService = array of Arr2ABBluetoothGattService;

  ABLAdvertiseData = class;
  Arr1ABLAdvertiseData = array of ABLAdvertiseData;
  Arr2ABLAdvertiseData = array of Arr1ABLAdvertiseData;
  Arr3ABLAdvertiseData = array of Arr2ABLAdvertiseData;

  ABLAdvertiseSettings = class;
  Arr1ABLAdvertiseSettings = array of ABLAdvertiseSettings;
  Arr2ABLAdvertiseSettings = array of Arr1ABLAdvertiseSettings;
  Arr3ABLAdvertiseSettings = array of Arr2ABLAdvertiseSettings;

  ABLScanFilter = class;
  Arr1ABLScanFilter = array of ABLScanFilter;
  Arr2ABLScanFilter = array of Arr1ABLScanFilter;
  Arr3ABLScanFilter = array of Arr2ABLScanFilter;

  ABLScanResult = class;
  Arr1ABLScanResult = array of ABLScanResult;
  Arr2ABLScanResult = array of Arr1ABLScanResult;
  Arr3ABLScanResult = array of Arr2ABLScanResult;

  ABLScanSettings = class;
  Arr1ABLScanSettings = array of ABLScanSettings;
  Arr2ABLScanSettings = array of Arr1ABLScanSettings;
  Arr3ABLScanSettings = array of Arr2ABLScanSettings;

  ACRestrictionEntry = class;
  Arr1ACRestrictionEntry = array of ACRestrictionEntry;
  Arr2ACRestrictionEntry = array of Arr1ACRestrictionEntry;
  Arr3ACRestrictionEntry = array of Arr2ACRestrictionEntry;

  ACSyncRequest = class;
  Arr1ACSyncRequest = array of ACSyncRequest;
  Arr2ACSyncRequest = array of Arr1ACSyncRequest;
  Arr3ACSyncRequest = array of Arr2ACSyncRequest;

  ACUriPermission = class;
  Arr1ACUriPermission = array of ACUriPermission;
  Arr2ACUriPermission = array of Arr1ACUriPermission;
  Arr3ACUriPermission = array of Arr2ACUriPermission;

  ACPFeatureGroupInfo = class;
  Arr1ACPFeatureGroupInfo = array of ACPFeatureGroupInfo;
  Arr2ACPFeatureGroupInfo = array of Arr1ACPFeatureGroupInfo;
  Arr3ACPFeatureGroupInfo = array of Arr2ACPFeatureGroupInfo;

  ACPShortcutInfo = class;
  Arr1ACPShortcutInfo = array of ACPShortcutInfo;
  Arr2ACPShortcutInfo = array of Arr1ACPShortcutInfo;
  Arr3ACPShortcutInfo = array of Arr2ACPShortcutInfo;

  AHCCaptureRequest = class;
  Arr1AHCCaptureRequest = array of AHCCaptureRequest;
  Arr2AHCCaptureRequest = array of Arr1AHCCaptureRequest;
  Arr3AHCCaptureRequest = array of Arr2AHCCaptureRequest;

  AHCPOutputConfiguration = class;
  Arr1AHCPOutputConfiguration = array of AHCPOutputConfiguration;
  Arr2AHCPOutputConfiguration = array of Arr1AHCPOutputConfiguration;
  Arr3AHCPOutputConfiguration = array of Arr2AHCPOutputConfiguration;

  AHUUsbConfiguration = class;
  Arr1AHUUsbConfiguration = array of AHUUsbConfiguration;
  Arr2AHUUsbConfiguration = array of Arr1AHUUsbConfiguration;
  Arr3AHUUsbConfiguration = array of Arr2AHUUsbConfiguration;

  ALGnssClock = class;
  Arr1ALGnssClock = array of ALGnssClock;
  Arr2ALGnssClock = array of Arr1ALGnssClock;
  Arr3ALGnssClock = array of Arr2ALGnssClock;

  ALGnssMeasurement = class;
  Arr1ALGnssMeasurement = array of ALGnssMeasurement;
  Arr2ALGnssMeasurement = array of Arr1ALGnssMeasurement;
  Arr3ALGnssMeasurement = array of Arr2ALGnssMeasurement;

  ALGnssMeasurementsEvent = class;
  Arr1ALGnssMeasurementsEvent = array of ALGnssMeasurementsEvent;
  Arr2ALGnssMeasurementsEvent = array of Arr1ALGnssMeasurementsEvent;
  Arr3ALGnssMeasurementsEvent = array of Arr2ALGnssMeasurementsEvent;

  ALGnssNavigationMessage = class;
  Arr1ALGnssNavigationMessage = array of ALGnssNavigationMessage;
  Arr2ALGnssNavigationMessage = array of Arr1ALGnssNavigationMessage;
  Arr3ALGnssNavigationMessage = array of Arr2ALGnssNavigationMessage;

  ALLocationManager = class;
  Arr1ALLocationManager = array of ALLocationManager;
  Arr2ALLocationManager = array of Arr1ALLocationManager;
  Arr3ALLocationManager = array of Arr2ALLocationManager;

  AMAudioAttributes = class;
  Arr1AMAudioAttributes = array of AMAudioAttributes;
  Arr2AMAudioAttributes = array of Arr1AMAudioAttributes;
  Arr3AMAudioAttributes = array of Arr2AMAudioAttributes;

  AMAudioRecordingConfiguration = class;
  Arr1AMAudioRecordingConfiguration = array of AMAudioRecordingConfiguration;
  Arr2AMAudioRecordingConfiguration = array of Arr1AMAudioRecordingConfiguration;
  Arr3AMAudioRecordingConfiguration = array of Arr2AMAudioRecordingConfiguration;

  AMMediaDescription = class;
  Arr1AMMediaDescription = array of AMMediaDescription;
  Arr2AMMediaDescription = array of Arr1AMMediaDescription;
  Arr3AMMediaDescription = array of Arr2AMMediaDescription;

  AMMediaMetadata = class;
  Arr1AMMediaMetadata = array of AMMediaMetadata;
  Arr2AMMediaMetadata = array of Arr1AMMediaMetadata;
  Arr3AMMediaMetadata = array of Arr2AMMediaMetadata;

  AMPlaybackParams = class;
  Arr1AMPlaybackParams = array of AMPlaybackParams;
  Arr2AMPlaybackParams = array of Arr1AMPlaybackParams;
  Arr3AMPlaybackParams = array of Arr2AMPlaybackParams;

  AMRating = class;
  Arr1AMRating = array of AMRating;
  Arr2AMRating = array of Arr1AMRating;
  Arr3AMRating = array of Arr2AMRating;

  AMMMidiDeviceInfo = class;
  Arr1AMMMidiDeviceInfo = array of AMMMidiDeviceInfo;
  Arr2AMMMidiDeviceInfo = array of Arr1AMMMidiDeviceInfo;
  Arr3AMMMidiDeviceInfo = array of Arr2AMMMidiDeviceInfo;

  AMMMidiDeviceStatus = class;
  Arr1AMMMidiDeviceStatus = array of AMMMidiDeviceStatus;
  Arr2AMMMidiDeviceStatus = array of Arr1AMMMidiDeviceStatus;
  Arr3AMMMidiDeviceStatus = array of Arr2AMMMidiDeviceStatus;

  AMSMediaSession = class;
  Arr1AMSMediaSession = array of AMSMediaSession;
  Arr2AMSMediaSession = array of Arr1AMSMediaSession;
  Arr3AMSMediaSession = array of Arr2AMSMediaSession;

  AANotification = class;
  Arr1AANotification = array of AANotification;
  Arr2AANotification = array of Arr1AANotification;
  Arr3AANotification = array of Arr2AANotification;

  AMSPlaybackState = class;
  Arr1AMSPlaybackState = array of AMSPlaybackState;
  Arr2AMSPlaybackState = array of Arr1AMSPlaybackState;
  Arr3AMSPlaybackState = array of Arr2AMSPlaybackState;

  AMTTvInputInfo = class;
  Arr1AMTTvInputInfo = array of AMTTvInputInfo;
  Arr2AMTTvInputInfo = array of Arr1AMTTvInputInfo;
  Arr3AMTTvInputInfo = array of Arr2AMTTvInputInfo;

  AMTTvTrackInfo = class;
  Arr1AMTTvTrackInfo = array of AMTTvTrackInfo;
  Arr2AMTTvTrackInfo = array of Arr1AMTTvTrackInfo;
  Arr3AMTTvTrackInfo = array of Arr2AMTTvTrackInfo;

  ANCaptivePortal = class;
  Arr1ANCaptivePortal = array of ANCaptivePortal;
  Arr2ANCaptivePortal = array of Arr1ANCaptivePortal;
  Arr3ANCaptivePortal = array of Arr2ANCaptivePortal;

  ANIpPrefix = class;
  Arr1ANIpPrefix = array of ANIpPrefix;
  Arr2ANIpPrefix = array of Arr1ANIpPrefix;
  Arr3ANIpPrefix = array of Arr2ANIpPrefix;

  ANLinkAddress = class;
  Arr1ANLinkAddress = array of ANLinkAddress;
  Arr2ANLinkAddress = array of Arr1ANLinkAddress;
  Arr3ANLinkAddress = array of Arr2ANLinkAddress;

  ANLinkProperties = class;
  Arr1ANLinkProperties = array of ANLinkProperties;
  Arr2ANLinkProperties = array of Arr1ANLinkProperties;
  Arr3ANLinkProperties = array of Arr2ANLinkProperties;

  ANNetwork = class;
  Arr1ANNetwork = array of ANNetwork;
  Arr2ANNetwork = array of Arr1ANNetwork;
  Arr3ANNetwork = array of Arr2ANNetwork;

  ANNetworkCapabilities = class;
  Arr1ANNetworkCapabilities = array of ANNetworkCapabilities;
  Arr2ANNetworkCapabilities = array of Arr1ANNetworkCapabilities;
  Arr3ANNetworkCapabilities = array of Arr2ANNetworkCapabilities;

  ANNetworkRequest = class;
  Arr1ANNetworkRequest = array of ANNetworkRequest;
  Arr2ANNetworkRequest = array of Arr1ANNetworkRequest;
  Arr3ANNetworkRequest = array of Arr2ANNetworkRequest;

  ANProxyInfo = class;
  Arr1ANProxyInfo = array of ANProxyInfo;
  Arr2ANProxyInfo = array of Arr1ANProxyInfo;
  Arr3ANProxyInfo = array of Arr2ANProxyInfo;

  ANRouteInfo = class;
  Arr1ANRouteInfo = array of ANRouteInfo;
  Arr2ANRouteInfo = array of Arr1ANRouteInfo;
  Arr3ANRouteInfo = array of Arr2ANRouteInfo;

  ANNNsdServiceInfo = class;
  Arr1ANNNsdServiceInfo = array of ANNNsdServiceInfo;
  Arr2ANNNsdServiceInfo = array of Arr1ANNNsdServiceInfo;
  Arr3ANNNsdServiceInfo = array of Arr2ANNNsdServiceInfo;

  ANWWifiEnterpriseConfig = class;
  Arr1ANWWifiEnterpriseConfig = array of ANWWifiEnterpriseConfig;
  Arr2ANWWifiEnterpriseConfig = array of Arr1ANWWifiEnterpriseConfig;
  Arr3ANWWifiEnterpriseConfig = array of Arr2ANWWifiEnterpriseConfig;

  ANWPNWifiP2pServiceInfo = class;
  Arr1ANWPNWifiP2pServiceInfo = array of ANWPNWifiP2pServiceInfo;
  Arr2ANWPNWifiP2pServiceInfo = array of Arr1ANWPNWifiP2pServiceInfo;
  Arr3ANWPNWifiP2pServiceInfo = array of Arr2ANWPNWifiP2pServiceInfo;

  ANWPNWifiP2pServiceRequest = class;
  Arr1ANWPNWifiP2pServiceRequest = array of ANWPNWifiP2pServiceRequest;
  Arr2ANWPNWifiP2pServiceRequest = array of Arr1ANWPNWifiP2pServiceRequest;
  Arr3ANWPNWifiP2pServiceRequest = array of Arr2ANWPNWifiP2pServiceRequest;

  AOCpuUsageInfo = class;
  Arr1AOCpuUsageInfo = array of AOCpuUsageInfo;
  Arr2AOCpuUsageInfo = array of Arr1AOCpuUsageInfo;
  Arr3AOCpuUsageInfo = array of Arr2AOCpuUsageInfo;

  AOLocaleList = class;
  Arr1AOLocaleList = array of AOLocaleList;
  Arr2AOLocaleList = array of Arr1AOLocaleList;
  Arr3AOLocaleList = array of Arr2AOLocaleList;

  AOPersistableBundle = class;
  Arr1AOPersistableBundle = array of AOPersistableBundle;
  Arr2AOPersistableBundle = array of Arr1AOPersistableBundle;
  Arr3AOPersistableBundle = array of Arr2AOPersistableBundle;

  AOUserHandle = class;
  Arr1AOUserHandle = array of AOUserHandle;
  Arr2AOUserHandle = array of Arr1AOUserHandle;
  Arr3AOUserHandle = array of Arr2AOUserHandle;

  AOHTimerStat = class;
  Arr1AOHTimerStat = array of AOHTimerStat;
  Arr2AOHTimerStat = array of Arr1AOHTimerStat;
  Arr3AOHTimerStat = array of Arr2AOHTimerStat;

  AOSStorageVolume = class;
  Arr1AOSStorageVolume = array of AOSStorageVolume;
  Arr2AOSStorageVolume = array of Arr1AOSStorageVolume;
  Arr3AOSStorageVolume = array of Arr2AOSStorageVolume;

  APPageRange = class;
  Arr1APPageRange = array of APPageRange;
  Arr2APPageRange = array of Arr1APPageRange;
  Arr3APPageRange = array of Arr2APPageRange;

  APPrintAttributes = class;
  Arr1APPrintAttributes = array of APPrintAttributes;
  Arr2APPrintAttributes = array of Arr1APPrintAttributes;
  Arr3APPrintAttributes = array of Arr2APPrintAttributes;

  APPrintDocumentInfo = class;
  Arr1APPrintDocumentInfo = array of APPrintDocumentInfo;
  Arr2APPrintDocumentInfo = array of Arr1APPrintDocumentInfo;
  Arr3APPrintDocumentInfo = array of Arr2APPrintDocumentInfo;

  APPrintJobId = class;
  Arr1APPrintJobId = array of APPrintJobId;
  Arr2APPrintJobId = array of Arr1APPrintJobId;
  Arr3APPrintJobId = array of Arr2APPrintJobId;

  APPrintJobInfo = class;
  Arr1APPrintJobInfo = array of APPrintJobInfo;
  Arr2APPrintJobInfo = array of Arr1APPrintJobInfo;
  Arr3APPrintJobInfo = array of Arr2APPrintJobInfo;

  APPrinterId = class;
  Arr1APPrinterId = array of APPrinterId;
  Arr2APPrinterId = array of Arr1APPrinterId;
  Arr3APPrinterId = array of Arr2APPrinterId;

  APPrinterInfo = class;
  Arr1APPrinterInfo = array of APPrinterInfo;
  Arr2APPrinterInfo = array of Arr1APPrinterInfo;
  Arr3APPrinterInfo = array of Arr2APPrinterInfo;

  ASCCarrierIdentifier = class;
  Arr1ASCCarrierIdentifier = array of ASCCarrierIdentifier;
  Arr2ASCCarrierIdentifier = array of Arr1ASCCarrierIdentifier;
  Arr3ASCCarrierIdentifier = array of Arr2ASCCarrierIdentifier;

  ASCMessagePdu = class;
  Arr1ASCMessagePdu = array of ASCMessagePdu;
  Arr2ASCMessagePdu = array of Arr1ASCMessagePdu;
  Arr3ASCMessagePdu = array of Arr2ASCMessagePdu;

  ASCChooserTarget = class;
  Arr1ASCChooserTarget = array of ASCChooserTarget;
  Arr2ASCChooserTarget = array of Arr1ASCChooserTarget;
  Arr3ASCChooserTarget = array of Arr2ASCChooserTarget;

  ASNStatusBarNotification = class;
  Arr1ASNStatusBarNotification = array of ASNStatusBarNotification;
  Arr2ASNStatusBarNotification = array of Arr1ASNStatusBarNotification;
  Arr3ASNStatusBarNotification = array of Arr2ASNStatusBarNotification;

  ASQTile = class;
  Arr1ASQTile = array of ASQTile;
  Arr2ASQTile = array of Arr1ASQTile;
  Arr3ASQTile = array of Arr2ASQTile;

  ASTVoice = class;
  Arr1ASTVoice = array of ASTVoice;
  Arr2ASTVoice = array of Arr1ASTVoice;
  Arr3ASTVoice = array of Arr2ASTVoice;

  ATCallAudioState = class;
  Arr1ATCallAudioState = array of ATCallAudioState;
  Arr2ATCallAudioState = array of Arr1ATCallAudioState;
  Arr3ATCallAudioState = array of Arr2ATCallAudioState;

  ATConnectionRequest = class;
  Arr1ATConnectionRequest = array of ATConnectionRequest;
  Arr2ATConnectionRequest = array of Arr1ATConnectionRequest;
  Arr3ATConnectionRequest = array of Arr2ATConnectionRequest;

  ATDisconnectCause = class;
  Arr1ATDisconnectCause = array of ATDisconnectCause;
  Arr2ATDisconnectCause = array of Arr1ATDisconnectCause;
  Arr3ATDisconnectCause = array of Arr2ATDisconnectCause;

  ATGatewayInfo = class;
  Arr1ATGatewayInfo = array of ATGatewayInfo;
  Arr2ATGatewayInfo = array of Arr1ATGatewayInfo;
  Arr3ATGatewayInfo = array of Arr2ATGatewayInfo;

  ATPhoneAccount = class;
  Arr1ATPhoneAccount = array of ATPhoneAccount;
  Arr2ATPhoneAccount = array of Arr1ATPhoneAccount;
  Arr3ATPhoneAccount = array of Arr2ATPhoneAccount;

  ATPhoneAccountHandle = class;
  Arr1ATPhoneAccountHandle = array of ATPhoneAccountHandle;
  Arr2ATPhoneAccountHandle = array of Arr1ATPhoneAccountHandle;
  Arr3ATPhoneAccountHandle = array of Arr2ATPhoneAccountHandle;

  ATStatusHints = class;
  Arr1ATStatusHints = array of ATStatusHints;
  Arr2ATStatusHints = array of Arr1ATStatusHints;
  Arr3ATStatusHints = array of Arr2ATStatusHints;

  ATVideoProfile = class;
  Arr1ATVideoProfile = array of ATVideoProfile;
  Arr2ATVideoProfile = array of Arr1ATVideoProfile;
  Arr3ATVideoProfile = array of Arr2ATVideoProfile;

  ATCellIdentityCdma = class;
  Arr1ATCellIdentityCdma = array of ATCellIdentityCdma;
  Arr2ATCellIdentityCdma = array of Arr1ATCellIdentityCdma;
  Arr3ATCellIdentityCdma = array of Arr2ATCellIdentityCdma;

  ATCellIdentityGsm = class;
  Arr1ATCellIdentityGsm = array of ATCellIdentityGsm;
  Arr2ATCellIdentityGsm = array of Arr1ATCellIdentityGsm;
  Arr3ATCellIdentityGsm = array of Arr2ATCellIdentityGsm;

  ATCellIdentityLte = class;
  Arr1ATCellIdentityLte = array of ATCellIdentityLte;
  Arr2ATCellIdentityLte = array of Arr1ATCellIdentityLte;
  Arr3ATCellIdentityLte = array of Arr2ATCellIdentityLte;

  ATCellIdentityWcdma = class;
  Arr1ATCellIdentityWcdma = array of ATCellIdentityWcdma;
  Arr2ATCellIdentityWcdma = array of Arr1ATCellIdentityWcdma;
  Arr3ATCellIdentityWcdma = array of Arr2ATCellIdentityWcdma;

  ATCellInfo = class;
  Arr1ATCellInfo = array of ATCellInfo;
  Arr2ATCellInfo = array of Arr1ATCellInfo;
  Arr3ATCellInfo = array of Arr2ATCellInfo;

  ATIccOpenLogicalChannelResponse = class;
  Arr1ATIccOpenLogicalChannelResponse = array of ATIccOpenLogicalChannelResponse;
  Arr2ATIccOpenLogicalChannelResponse = array of Arr1ATIccOpenLogicalChannelResponse;
  Arr3ATIccOpenLogicalChannelResponse = array of Arr2ATIccOpenLogicalChannelResponse;

  ATSubscriptionInfo = class;
  Arr1ATSubscriptionInfo = array of ATSubscriptionInfo;
  Arr2ATSubscriptionInfo = array of Arr1ATSubscriptionInfo;
  Arr3ATSubscriptionInfo = array of Arr2ATSubscriptionInfo;

  AVDragAndDropPermissions = class;
  Arr1AVDragAndDropPermissions = array of AVDragAndDropPermissions;
  Arr2AVDragAndDropPermissions = array of Arr1AVDragAndDropPermissions;
  Arr3AVDragAndDropPermissions = array of Arr2AVDragAndDropPermissions;

  AVKeyboardShortcutGroup = class;
  Arr1AVKeyboardShortcutGroup = array of AVKeyboardShortcutGroup;
  Arr2AVKeyboardShortcutGroup = array of Arr1AVKeyboardShortcutGroup;
  Arr3AVKeyboardShortcutGroup = array of Arr2AVKeyboardShortcutGroup;

  AVKeyboardShortcutInfo = class;
  Arr1AVKeyboardShortcutInfo = array of AVKeyboardShortcutInfo;
  Arr2AVKeyboardShortcutInfo = array of Arr1AVKeyboardShortcutInfo;
  Arr3AVKeyboardShortcutInfo = array of Arr2AVKeyboardShortcutInfo;

  AVPointerIcon = class;
  Arr1AVPointerIcon = array of AVPointerIcon;
  Arr2AVPointerIcon = array of Arr1AVPointerIcon;
  Arr3AVPointerIcon = array of Arr2AVPointerIcon;

  AVWindowId = class;
  Arr1AVWindowId = array of AVWindowId;
  Arr2AVWindowId = array of Arr1AVWindowId;
  Arr3AVWindowId = array of Arr2AVWindowId;

  AVAAccessibilityWindowInfo = class;
  Arr1AVAAccessibilityWindowInfo = array of AVAAccessibilityWindowInfo;
  Arr2AVAAccessibilityWindowInfo = array of Arr1AVAAccessibilityWindowInfo;
  Arr3AVAAccessibilityWindowInfo = array of Arr2AVAAccessibilityWindowInfo;

  AVICursorAnchorInfo = class;
  Arr1AVICursorAnchorInfo = array of AVICursorAnchorInfo;
  Arr2AVICursorAnchorInfo = array of Arr1AVICursorAnchorInfo;
  Arr3AVICursorAnchorInfo = array of Arr2AVICursorAnchorInfo;

  AVIInputContentInfo = class;
  Arr1AVIInputContentInfo = array of AVIInputContentInfo;
  Arr2AVIInputContentInfo = array of Arr1AVIInputContentInfo;
  Arr3AVIInputContentInfo = array of Arr2AVIInputContentInfo;

  AVTSentenceSuggestionsInfo = class;
  Arr1AVTSentenceSuggestionsInfo = array of AVTSentenceSuggestionsInfo;
  Arr2AVTSentenceSuggestionsInfo = array of Arr1AVTSentenceSuggestionsInfo;
  Arr3AVTSentenceSuggestionsInfo = array of Arr2AVTSentenceSuggestionsInfo;

  AMTTvContract = class;
  Arr1AMTTvContract = array of AMTTvContract;
  Arr2AMTTvContract = array of Arr1AMTTvContract;
  Arr3AMTTvContract = array of Arr2AMTTvContract;

  APTelephony = class;
  Arr1APTelephony = array of APTelephony;
  Arr2APTelephony = array of Arr1APTelephony;
  Arr3APTelephony = array of Arr2APTelephony;

  ATCellSignalStrengthCdma = class;
  Arr1ATCellSignalStrengthCdma = array of ATCellSignalStrengthCdma;
  Arr2ATCellSignalStrengthCdma = array of Arr1ATCellSignalStrengthCdma;
  Arr3ATCellSignalStrengthCdma = array of Arr2ATCellSignalStrengthCdma;

  ATCellSignalStrengthGsm = class;
  Arr1ATCellSignalStrengthGsm = array of ATCellSignalStrengthGsm;
  Arr2ATCellSignalStrengthGsm = array of Arr1ATCellSignalStrengthGsm;
  Arr3ATCellSignalStrengthGsm = array of Arr2ATCellSignalStrengthGsm;

  ATCellSignalStrengthLte = class;
  Arr1ATCellSignalStrengthLte = array of ATCellSignalStrengthLte;
  Arr2ATCellSignalStrengthLte = array of Arr1ATCellSignalStrengthLte;
  Arr3ATCellSignalStrengthLte = array of Arr2ATCellSignalStrengthLte;

  ATCellSignalStrengthWcdma = class;
  Arr1ATCellSignalStrengthWcdma = array of ATCellSignalStrengthWcdma;
  Arr2ATCellSignalStrengthWcdma = array of Arr1ATCellSignalStrengthWcdma;
  Arr3ATCellSignalStrengthWcdma = array of Arr2ATCellSignalStrengthWcdma;

  ATArcMotion = class;
  Arr1ATArcMotion = array of ATArcMotion;
  Arr2ATArcMotion = array of Arr1ATArcMotion;
  Arr3ATArcMotion = array of Arr2ATArcMotion;

  ATPatternPathMotion = class;
  Arr1ATPatternPathMotion = array of ATPatternPathMotion;
  Arr2ATPatternPathMotion = array of Arr1ATPatternPathMotion;
  Arr3ATPatternPathMotion = array of Arr2ATPatternPathMotion;

  ATVisibilityPropagation = class;
  Arr1ATVisibilityPropagation = array of ATVisibilityPropagation;
  Arr2ATVisibilityPropagation = array of Arr1ATVisibilityPropagation;
  Arr3ATVisibilityPropagation = array of Arr2ATVisibilityPropagation;

  AUFloatProperty = class;
  Arr1AUFloatProperty = array of AUFloatProperty;
  Arr2AUFloatProperty = array of Arr1AUFloatProperty;
  Arr3AUFloatProperty = array of Arr2AUFloatProperty;

  AUIntProperty = class;
  Arr1AUIntProperty = array of AUIntProperty;
  Arr2AUIntProperty = array of Arr1AUIntProperty;
  Arr3AUIntProperty = array of Arr2AUIntProperty;

  AVWindowAnimationFrameStats = class;
  Arr1AVWindowAnimationFrameStats = array of AVWindowAnimationFrameStats;
  Arr2AVWindowAnimationFrameStats = array of Arr1AVWindowAnimationFrameStats;
  Arr3AVWindowAnimationFrameStats = array of Arr2AVWindowAnimationFrameStats;

  AVWindowContentFrameStats = class;
  Arr1AVWindowContentFrameStats = array of AVWindowContentFrameStats;
  Arr2AVWindowContentFrameStats = array of Arr1AVWindowContentFrameStats;
  Arr3AVWindowContentFrameStats = array of Arr2AVWindowContentFrameStats;

  AVViewGroupOverlay = class;
  Arr1AVViewGroupOverlay = array of AVViewGroupOverlay;
  Arr2AVViewGroupOverlay = array of Arr1AVViewGroupOverlay;
  Arr3AVViewGroupOverlay = array of Arr2AVViewGroupOverlay;

  AAUNetworkStats = class;
  Arr1AAUNetworkStats = array of AAUNetworkStats;
  Arr2AAUNetworkStats = array of Arr1AAUNetworkStats;
  Arr3AAUNetworkStats = array of Arr2AAUNetworkStats;

  AGPPdfRenderer = class;
  Arr1AGPPdfRenderer = array of AGPPdfRenderer;
  Arr2AGPPdfRenderer = array of Arr1AGPPdfRenderer;
  Arr3AGPPdfRenderer = array of Arr2AGPPdfRenderer;

  AHCCameraCaptureSession = class;
  Arr1AHCCameraCaptureSession = array of AHCCameraCaptureSession;
  Arr2AHCCameraCaptureSession = array of Arr1AHCCameraCaptureSession;
  Arr3AHCCameraCaptureSession = array of Arr2AHCCameraCaptureSession;

  AHCDngCreator = class;
  Arr1AHCDngCreator = array of AHCDngCreator;
  Arr2AHCDngCreator = array of Arr1AHCDngCreator;
  Arr3AHCDngCreator = array of Arr2AHCDngCreator;

  AMImage = class;
  Arr1AMImage = array of AMImage;
  Arr2AMImage = array of Arr1AMImage;
  Arr3AMImage = array of Arr2AMImage;

  AMImageReader = class;
  Arr1AMImageReader = array of AMImageReader;
  Arr2AMImageReader = array of Arr1AMImageReader;
  Arr3AMImageReader = array of Arr2AMImageReader;

  AMImageWriter = class;
  Arr1AMImageWriter = array of AMImageWriter;
  Arr2AMImageWriter = array of Arr1AMImageWriter;
  Arr3AMImageWriter = array of Arr2AMImageWriter;

  JUSBaseStream = interface;
  Arr1JUSBaseStream = array of JUSBaseStream;
  Arr2JUSBaseStream = array of Arr1JUSBaseStream;
  Arr3JUSBaseStream = array of Arr2JUSBaseStream;

  AASuppressLint = interface;
  Arr1AASuppressLint = array of AASuppressLint;
  Arr2AASuppressLint = array of Arr1AASuppressLint;
  Arr3AASuppressLint = array of Arr2AASuppressLint;

  AATargetApi = interface;
  Arr1AATargetApi = array of AATargetApi;
  Arr2AATargetApi = array of Arr1AATargetApi;
  Arr3AATargetApi = array of Arr2AATargetApi;

  AWJavascriptInterface = interface;
  Arr1AWJavascriptInterface = array of AWJavascriptInterface;
  Arr2AWJavascriptInterface = array of Arr1AWJavascriptInterface;
  Arr3AWJavascriptInterface = array of Arr2AWJavascriptInterface;

  JLFunctionalInterface = interface;
  Arr1JLFunctionalInterface = array of JLFunctionalInterface;
  Arr2JLFunctionalInterface = array of Arr1JLFunctionalInterface;
  Arr3JLFunctionalInterface = array of Arr2JLFunctionalInterface;

  JLSafeVarargs = interface;
  Arr1JLSafeVarargs = array of JLSafeVarargs;
  Arr2JLSafeVarargs = array of Arr1JLSafeVarargs;
  Arr3JLSafeVarargs = array of Arr2JLSafeVarargs;

  JLARepeatable = interface;
  Arr1JLARepeatable = array of JLARepeatable;
  Arr2JLARepeatable = array of Arr1JLARepeatable;
  Arr3JLARepeatable = array of Arr2JLARepeatable;

  JNStandardProtocolFamily = class;
  Arr1JNStandardProtocolFamily = array of JNStandardProtocolFamily;
  Arr2JNStandardProtocolFamily = array of Arr1JNStandardProtocolFamily;
  Arr3JNStandardProtocolFamily = array of Arr2JNStandardProtocolFamily;

  ASKeyPairGeneratorSpec = class;
  Arr1ASKeyPairGeneratorSpec = array of ASKeyPairGeneratorSpec;
  Arr2ASKeyPairGeneratorSpec = array of Arr1ASKeyPairGeneratorSpec;
  Arr3ASKeyPairGeneratorSpec = array of Arr2ASKeyPairGeneratorSpec;

  ASKKeyGenParameterSpec = class;
  Arr1ASKKeyGenParameterSpec = array of ASKKeyGenParameterSpec;
  Arr2ASKKeyGenParameterSpec = array of Arr1ASKKeyGenParameterSpec;
  Arr3ASKKeyGenParameterSpec = array of Arr2ASKKeyGenParameterSpec;

  JCSGCMParameterSpec = class;
  Arr1JCSGCMParameterSpec = array of JCSGCMParameterSpec;
  Arr2JCSGCMParameterSpec = array of Arr1JCSGCMParameterSpec;
  Arr3JCSGCMParameterSpec = array of Arr2JCSGCMParameterSpec;

  ASKKeyInfo = class;
  Arr1ASKKeyInfo = array of ASKKeyInfo;
  Arr2ASKKeyInfo = array of Arr1ASKKeyInfo;
  Arr3ASKKeyInfo = array of Arr2ASKKeyInfo;

  JUSStreamSupport = class;
  Arr1JUSStreamSupport = array of JUSStreamSupport;
  Arr2JUSStreamSupport = array of Arr1JUSStreamSupport;
  Arr3JUSStreamSupport = array of Arr2JUSStreamSupport;

  JUCCompletableFuture = class;
  Arr1JUCCompletableFuture = array of JUCCompletableFuture;
  Arr2JUCCompletableFuture = array of Arr1JUCCompletableFuture;
  Arr3JUCCompletableFuture = array of Arr2JUCCompletableFuture;

  JUCForkJoinTask = class;
  Arr1JUCForkJoinTask = array of JUCForkJoinTask;
  Arr2JUCForkJoinTask = array of Arr1JUCForkJoinTask;
  Arr3JUCForkJoinTask = array of Arr2JUCForkJoinTask;

  JUFBinaryOperator = interface;
  Arr1JUFBinaryOperator = array of JUFBinaryOperator;
  Arr2JUFBinaryOperator = array of Arr1JUFBinaryOperator;
  Arr3JUFBinaryOperator = array of Arr2JUFBinaryOperator;

  JUDoubleSummaryStatistics = class;
  Arr1JUDoubleSummaryStatistics = array of JUDoubleSummaryStatistics;
  Arr2JUDoubleSummaryStatistics = array of Arr1JUDoubleSummaryStatistics;
  Arr3JUDoubleSummaryStatistics = array of Arr2JUDoubleSummaryStatistics;

  JUFUnaryOperator = interface;
  Arr1JUFUnaryOperator = array of JUFUnaryOperator;
  Arr2JUFUnaryOperator = array of Arr1JUFUnaryOperator;
  Arr3JUFUnaryOperator = array of Arr2JUFUnaryOperator;

  JUIntSummaryStatistics = class;
  Arr1JUIntSummaryStatistics = array of JUIntSummaryStatistics;
  Arr2JUIntSummaryStatistics = array of Arr1JUIntSummaryStatistics;
  Arr3JUIntSummaryStatistics = array of Arr2JUIntSummaryStatistics;

  JULongSummaryStatistics = class;
  Arr1JULongSummaryStatistics = array of JULongSummaryStatistics;
  Arr2JULongSummaryStatistics = array of Arr1JULongSummaryStatistics;
  Arr3JULongSummaryStatistics = array of Arr2JULongSummaryStatistics;

  JNSSNIHostName = class;
  Arr1JNSSNIHostName = array of JNSSNIHostName;
  Arr2JNSSNIHostName = array of Arr1JNSSNIHostName;
  Arr3JNSSNIHostName = array of Arr2JNSSNIHostName;

  JNSExtendedSSLSession = class;
  Arr1JNSExtendedSSLSession = array of JNSExtendedSSLSession;
  Arr2JNSExtendedSSLSession = array of Arr1JNSExtendedSSLSession;
  Arr3JNSExtendedSSLSession = array of Arr2JNSExtendedSSLSession;

  AMDeniedByServerException = class;
  Arr1AMDeniedByServerException = array of AMDeniedByServerException;
  Arr2AMDeniedByServerException = array of Arr1AMDeniedByServerException;
  Arr3AMDeniedByServerException = array of Arr2AMDeniedByServerException;

  AMNotProvisionedException = class;
  Arr1AMNotProvisionedException = array of AMNotProvisionedException;
  Arr2AMNotProvisionedException = array of Arr1AMNotProvisionedException;
  Arr3AMNotProvisionedException = array of Arr2AMNotProvisionedException;

  AMResourceBusyException = class;
  Arr1AMResourceBusyException = array of AMResourceBusyException;
  Arr2AMResourceBusyException = array of Arr1AMResourceBusyException;
  Arr3AMResourceBusyException = array of Arr2AMResourceBusyException;

  AMUnsupportedSchemeException = class;
  Arr1AMUnsupportedSchemeException = array of AMUnsupportedSchemeException;
  Arr2AMUnsupportedSchemeException = array of Arr1AMUnsupportedSchemeException;
  Arr3AMUnsupportedSchemeException = array of Arr2AMUnsupportedSchemeException;

  AHCCameraAccessException = class;
  Arr1AHCCameraAccessException = array of AHCCameraAccessException;
  Arr2AHCCameraAccessException = array of Arr1AHCCameraAccessException;
  Arr3AHCCameraAccessException = array of Arr2AHCCameraAccessException;

  AITStringPrepParseException = class;
  Arr1AITStringPrepParseException = array of AITStringPrepParseException;
  Arr2AITStringPrepParseException = array of Arr1AITStringPrepParseException;
  Arr3AITStringPrepParseException = array of Arr2AITStringPrepParseException;

  AITNormalizer2 = class;
  Arr1AITNormalizer2 = array of AITNormalizer2;
  Arr2AITNormalizer2 = array of Arr1AITNormalizer2;
  Arr3AITNormalizer2 = array of Arr2AITNormalizer2;

  AITCollator = class;
  Arr1AITCollator = array of AITCollator;
  Arr2AITCollator = array of Arr1AITCollator;
  Arr3AITCollator = array of Arr2AITCollator;

  AITDateIntervalInfo = class;
  Arr1AITDateIntervalInfo = array of AITDateIntervalInfo;
  Arr2AITDateIntervalInfo = array of Arr1AITDateIntervalInfo;
  Arr3AITDateIntervalInfo = array of Arr2AITDateIntervalInfo;

  AITDateTimePatternGenerator = class;
  Arr1AITDateTimePatternGenerator = array of AITDateTimePatternGenerator;
  Arr2AITDateTimePatternGenerator = array of Arr1AITDateTimePatternGenerator;
  Arr3AITDateTimePatternGenerator = array of Arr2AITDateTimePatternGenerator;

  AITMessagePattern = class;
  Arr1AITMessagePattern = array of AITMessagePattern;
  Arr2AITMessagePattern = array of Arr1AITMessagePattern;
  Arr3AITMessagePattern = array of Arr2AITMessagePattern;

  AIUTimeZone = class;
  Arr1AIUTimeZone = array of AIUTimeZone;
  Arr2AIUTimeZone = array of Arr1AIUTimeZone;
  Arr3AIUTimeZone = array of Arr2AIUTimeZone;

  ATChangeBounds = class;
  Arr1ATChangeBounds = array of ATChangeBounds;
  Arr2ATChangeBounds = array of Arr1ATChangeBounds;
  Arr3ATChangeBounds = array of Arr2ATChangeBounds;

  ATChangeClipBounds = class;
  Arr1ATChangeClipBounds = array of ATChangeClipBounds;
  Arr2ATChangeClipBounds = array of Arr1ATChangeClipBounds;
  Arr3ATChangeClipBounds = array of Arr2ATChangeClipBounds;

  ATChangeImageTransform = class;
  Arr1ATChangeImageTransform = array of ATChangeImageTransform;
  Arr2ATChangeImageTransform = array of Arr1ATChangeImageTransform;
  Arr3ATChangeImageTransform = array of Arr2ATChangeImageTransform;

  ATChangeScroll = class;
  Arr1ATChangeScroll = array of ATChangeScroll;
  Arr2ATChangeScroll = array of Arr1ATChangeScroll;
  Arr3ATChangeScroll = array of Arr2ATChangeScroll;

  ATChangeTransform = class;
  Arr1ATChangeTransform = array of ATChangeTransform;
  Arr2ATChangeTransform = array of Arr1ATChangeTransform;
  Arr3ATChangeTransform = array of Arr2ATChangeTransform;

  ATTransitionSet = class;
  Arr1ATTransitionSet = array of ATTransitionSet;
  Arr2ATTransitionSet = array of Arr1ATTransitionSet;
  Arr3ATTransitionSet = array of Arr2ATTransitionSet;

  ATVisibility = class;
  Arr1ATVisibility = array of ATVisibility;
  Arr2ATVisibility = array of Arr1ATVisibility;
  Arr3ATVisibility = array of Arr2ATVisibility;

  JUSpliterators = class;
  Arr1JUSpliterators = array of JUSpliterators;
  Arr2JUSpliterators = array of Arr1JUSpliterators;
  Arr3JUSpliterators = array of Arr2JUSpliterators;

  AGDIcon = class;
  Arr1AGDIcon = array of AGDIcon;
  Arr2AGDIcon = array of Arr1AGDIcon;
  Arr3AGDIcon = array of Arr2AGDIcon;

  AITLocaleDisplayNames = class;
  Arr1AITLocaleDisplayNames = array of AITLocaleDisplayNames;
  Arr2AITLocaleDisplayNames = array of Arr1AITLocaleDisplayNames;
  Arr3AITLocaleDisplayNames = array of Arr2AITLocaleDisplayNames;

  AITStringSearch = class;
  Arr1AITStringSearch = array of AITStringSearch;
  Arr2AITStringSearch = array of Arr1AITStringSearch;
  Arr3AITStringSearch = array of Arr2AITStringSearch;

  JUCADoubleAccumulator = class;
  Arr1JUCADoubleAccumulator = array of JUCADoubleAccumulator;
  Arr2JUCADoubleAccumulator = array of Arr1JUCADoubleAccumulator;
  Arr3JUCADoubleAccumulator = array of Arr2JUCADoubleAccumulator;

  JUCADoubleAdder = class;
  Arr1JUCADoubleAdder = array of JUCADoubleAdder;
  Arr2JUCADoubleAdder = array of Arr1JUCADoubleAdder;
  Arr3JUCADoubleAdder = array of Arr2JUCADoubleAdder;

  JUCALongAccumulator = class;
  Arr1JUCALongAccumulator = array of JUCALongAccumulator;
  Arr2JUCALongAccumulator = array of Arr1JUCALongAccumulator;
  Arr3JUCALongAccumulator = array of Arr2JUCALongAccumulator;

  JUCALongAdder = class;
  Arr1JUCALongAdder = array of JUCALongAdder;
  Arr2JUCALongAdder = array of Arr1JUCALongAdder;
  Arr3JUCALongAdder = array of Arr2JUCALongAdder;

  AIUCECalendar = class;
  Arr1AIUCECalendar = array of AIUCECalendar;
  Arr2AIUCECalendar = array of Arr1AIUCECalendar;
  Arr3AIUCECalendar = array of Arr2AIUCECalendar;

  AIUChineseCalendar = class;
  Arr1AIUChineseCalendar = array of AIUChineseCalendar;
  Arr2AIUChineseCalendar = array of Arr1AIUChineseCalendar;
  Arr3AIUChineseCalendar = array of Arr2AIUChineseCalendar;

  AIUGregorianCalendar = class;
  Arr1AIUGregorianCalendar = array of AIUGregorianCalendar;
  Arr2AIUGregorianCalendar = array of Arr1AIUGregorianCalendar;
  Arr3AIUGregorianCalendar = array of Arr2AIUGregorianCalendar;

  AIUHebrewCalendar = class;
  Arr1AIUHebrewCalendar = array of AIUHebrewCalendar;
  Arr2AIUHebrewCalendar = array of Arr1AIUHebrewCalendar;
  Arr3AIUHebrewCalendar = array of Arr2AIUHebrewCalendar;

  AIUIndianCalendar = class;
  Arr1AIUIndianCalendar = array of AIUIndianCalendar;
  Arr2AIUIndianCalendar = array of Arr1AIUIndianCalendar;
  Arr3AIUIndianCalendar = array of Arr2AIUIndianCalendar;

  AIUIslamicCalendar = class;
  Arr1AIUIslamicCalendar = array of AIUIslamicCalendar;
  Arr2AIUIslamicCalendar = array of Arr1AIUIslamicCalendar;
  Arr3AIUIslamicCalendar = array of Arr2AIUIslamicCalendar;

  AIUCurrency = class;
  Arr1AIUCurrency = array of AIUCurrency;
  Arr2AIUCurrency = array of Arr1AIUCurrency;
  Arr3AIUCurrency = array of Arr2AIUCurrency;

  AIUTimeUnit = class;
  Arr1AIUTimeUnit = array of AIUTimeUnit;
  Arr2AIUTimeUnit = array of Arr1AIUTimeUnit;
  Arr3AIUTimeUnit = array of Arr2AIUTimeUnit;

  JUCThreadLocalRandom = class;
  Arr1JUCThreadLocalRandom = array of JUCThreadLocalRandom;
  Arr2JUCThreadLocalRandom = array of Arr1JUCThreadLocalRandom;
  Arr3JUCThreadLocalRandom = array of Arr2JUCThreadLocalRandom;

  AMMediaExtractor = class;
  Arr1AMMediaExtractor = array of AMMediaExtractor;
  Arr2AMMediaExtractor = array of Arr1AMMediaExtractor;
  Arr3AMMediaExtractor = array of Arr2AMMediaExtractor;

  AMMediaMuxer = class;
  Arr1AMMediaMuxer = array of AMMediaMuxer;
  Arr2AMMediaMuxer = array of Arr1AMMediaMuxer;
  Arr3AMMediaMuxer = array of Arr2AMMediaMuxer;

  AHSensorEventCallback = class;
  Arr1AHSensorEventCallback = array of AHSensorEventCallback;
  Arr2AHSensorEventCallback = array of Arr1AHSensorEventCallback;
  Arr3AHSensorEventCallback = array of Arr2AHSensorEventCallback;

  AHCTotalCaptureResult = class;
  Arr1AHCTotalCaptureResult = array of AHCTotalCaptureResult;
  Arr2AHCTotalCaptureResult = array of Arr1AHCTotalCaptureResult;
  Arr3AHCTotalCaptureResult = array of Arr2AHCTotalCaptureResult;

  AITUnicodeSet = class;
  Arr1AITUnicodeSet = array of AITUnicodeSet;
  Arr2AITUnicodeSet = array of Arr1AITUnicodeSet;
  Arr3AITUnicodeSet = array of Arr2AITUnicodeSet;

  AOGLES31 = class;
  Arr1AOGLES31 = array of AOGLES31;
  Arr2AOGLES31 = array of Arr1AOGLES31;
  Arr3AOGLES31 = array of Arr2AOGLES31;

  AMBMediaBrowser = class;
  Arr1AMBMediaBrowser = array of AMBMediaBrowser;
  Arr2AMBMediaBrowser = array of Arr1AMBMediaBrowser;
  Arr3AMBMediaBrowser = array of Arr2AMBMediaBrowser;

  AMSMediaController = class;
  Arr1AMSMediaController = array of AMSMediaController;
  Arr2AMSMediaController = array of Arr1AMSMediaController;
  Arr3AMSMediaController = array of Arr2AMSMediaController;

  ASNCondition = class;
  Arr1ASNCondition = array of ASNCondition;
  Arr2ASNCondition = array of Arr1ASNCondition;
  Arr3ASNCondition = array of Arr2ASNCondition;

  ANWPNWifiP2pDnsSdServiceInfo = class;
  Arr1ANWPNWifiP2pDnsSdServiceInfo = array of ANWPNWifiP2pDnsSdServiceInfo;
  Arr2ANWPNWifiP2pDnsSdServiceInfo = array of Arr1ANWPNWifiP2pDnsSdServiceInfo;
  Arr3ANWPNWifiP2pDnsSdServiceInfo = array of Arr2ANWPNWifiP2pDnsSdServiceInfo;

  ANWPNWifiP2pUpnpServiceInfo = class;
  Arr1ANWPNWifiP2pUpnpServiceInfo = array of ANWPNWifiP2pUpnpServiceInfo;
  Arr2ANWPNWifiP2pUpnpServiceInfo = array of Arr1ANWPNWifiP2pUpnpServiceInfo;
  Arr3ANWPNWifiP2pUpnpServiceInfo = array of Arr2ANWPNWifiP2pUpnpServiceInfo;

  ANWPNWifiP2pDnsSdServiceRequest = class;
  Arr1ANWPNWifiP2pDnsSdServiceRequest = array of ANWPNWifiP2pDnsSdServiceRequest;
  Arr2ANWPNWifiP2pDnsSdServiceRequest = array of Arr1ANWPNWifiP2pDnsSdServiceRequest;
  Arr3ANWPNWifiP2pDnsSdServiceRequest = array of Arr2ANWPNWifiP2pDnsSdServiceRequest;

  ANWPNWifiP2pUpnpServiceRequest = class;
  Arr1ANWPNWifiP2pUpnpServiceRequest = array of ANWPNWifiP2pUpnpServiceRequest;
  Arr2ANWPNWifiP2pUpnpServiceRequest = array of Arr1ANWPNWifiP2pUpnpServiceRequest;
  Arr3ANWPNWifiP2pUpnpServiceRequest = array of Arr2ANWPNWifiP2pUpnpServiceRequest;

  APPrinterCapabilitiesInfo = class;
  Arr1APPrinterCapabilitiesInfo = array of APPrinterCapabilitiesInfo;
  Arr2APPrinterCapabilitiesInfo = array of Arr1APPrinterCapabilitiesInfo;
  Arr3APPrinterCapabilitiesInfo = array of Arr2APPrinterCapabilitiesInfo;

  ATConnection = class;
  Arr1ATConnection = array of ATConnection;
  Arr2ATConnection = array of Arr1ATConnection;
  Arr3ATConnection = array of Arr2ATConnection;

  ATRemoteConnection = class;
  Arr1ATRemoteConnection = array of ATRemoteConnection;
  Arr2ATRemoteConnection = array of Arr1ATRemoteConnection;
  Arr3ATRemoteConnection = array of Arr2ATRemoteConnection;

  ATCellInfoCdma = class;
  Arr1ATCellInfoCdma = array of ATCellInfoCdma;
  Arr2ATCellInfoCdma = array of Arr1ATCellInfoCdma;
  Arr3ATCellInfoCdma = array of Arr2ATCellInfoCdma;

  ATCellInfoGsm = class;
  Arr1ATCellInfoGsm = array of ATCellInfoGsm;
  Arr2ATCellInfoGsm = array of Arr1ATCellInfoGsm;
  Arr3ATCellInfoGsm = array of Arr2ATCellInfoGsm;

  ATCellInfoLte = class;
  Arr1ATCellInfoLte = array of ATCellInfoLte;
  Arr2ATCellInfoLte = array of Arr1ATCellInfoLte;
  Arr3ATCellInfoLte = array of Arr2ATCellInfoLte;

  ATCellInfoWcdma = class;
  Arr1ATCellInfoWcdma = array of ATCellInfoWcdma;
  Arr2ATCellInfoWcdma = array of Arr1ATCellInfoWcdma;
  Arr3ATCellInfoWcdma = array of Arr2ATCellInfoWcdma;

  ATSTtsSpan = class;
  Arr1ATSTtsSpan = array of ATSTtsSpan;
  Arr2ATSTtsSpan = array of Arr1ATSTtsSpan;
  Arr3ATSTtsSpan = array of Arr2ATSTtsSpan;

  ARScriptGroup = class;
  Arr1ARScriptGroup = array of ARScriptGroup;
  Arr2ARScriptGroup = array of Arr1ARScriptGroup;
  Arr3ARScriptGroup = array of Arr2ARScriptGroup;

  ARScriptIntrinsic = class;
  Arr1ARScriptIntrinsic = array of ARScriptIntrinsic;
  Arr2ARScriptIntrinsic = array of Arr1ARScriptIntrinsic;
  Arr3ARScriptIntrinsic = array of Arr2ARScriptIntrinsic;

  ATCircularPropagation = class;
  Arr1ATCircularPropagation = array of ATCircularPropagation;
  Arr2ATCircularPropagation = array of Arr1ATCircularPropagation;
  Arr3ATCircularPropagation = array of Arr2ATCircularPropagation;

  ATSidePropagation = class;
  Arr1ATSidePropagation = array of ATSidePropagation;
  Arr2ATSidePropagation = array of Arr1ATSidePropagation;
  Arr3ATSidePropagation = array of Arr2ATSidePropagation;

  AAUNetworkStatsManager = class;
  Arr1AAUNetworkStatsManager = array of AAUNetworkStatsManager;
  Arr2AAUNetworkStatsManager = array of Arr1AAUNetworkStatsManager;
  Arr3AAUNetworkStatsManager = array of Arr2AAUNetworkStatsManager;

  AHCCameraConstrainedHighSpeedCaptureSession = class;
  Arr1AHCCameraConstrainedHighSpeedCaptureSession = array of AHCCameraConstrainedHighSpeedCaptureSession;
  Arr2AHCCameraConstrainedHighSpeedCaptureSession = array of Arr1AHCCameraConstrainedHighSpeedCaptureSession;
  Arr3AHCCameraConstrainedHighSpeedCaptureSession = array of Arr2AHCCameraConstrainedHighSpeedCaptureSession;

  AHCCameraDevice = class;
  Arr1AHCCameraDevice = array of AHCCameraDevice;
  Arr2AHCCameraDevice = array of Arr1AHCCameraDevice;
  Arr3AHCCameraDevice = array of Arr2AHCCameraDevice;

  ACPPackageInstaller = class;
  Arr1ACPPackageInstaller = array of ACPPackageInstaller;
  Arr2ACPPackageInstaller = array of Arr1ACPPackageInstaller;
  Arr3ACPPackageInstaller = array of Arr2ACPPackageInstaller;

  AMMediaDataSource = class;
  Arr1AMMediaDataSource = array of AMMediaDataSource;
  Arr2AMMediaDataSource = array of Arr1AMMediaDataSource;
  Arr3AMMediaDataSource = array of Arr2AMMediaDataSource;

  AMMMidiDevice = class;
  Arr1AMMMidiDevice = array of AMMMidiDevice;
  Arr2AMMMidiDevice = array of Arr1AMMMidiDevice;
  Arr3AMMMidiDevice = array of Arr2AMMMidiDevice;

  AMMMidiInputPort = class;
  Arr1AMMMidiInputPort = array of AMMMidiInputPort;
  Arr2AMMMidiInputPort = array of Arr1AMMMidiInputPort;
  Arr3AMMMidiInputPort = array of Arr2AMMMidiInputPort;

  AMMMidiOutputPort = class;
  Arr1AMMMidiOutputPort = array of AMMMidiOutputPort;
  Arr2AMMMidiOutputPort = array of Arr1AMMMidiOutputPort;
  Arr3AMMMidiOutputPort = array of Arr2AMMMidiOutputPort;

  JUSDoubleStream = interface;
  Arr1JUSDoubleStream = array of JUSDoubleStream;
  Arr2JUSDoubleStream = array of Arr1JUSDoubleStream;
  Arr3JUSDoubleStream = array of Arr2JUSDoubleStream;

  JUSIntStream = interface;
  Arr1JUSIntStream = array of JUSIntStream;
  Arr2JUSIntStream = array of Arr1JUSIntStream;
  Arr3JUSIntStream = array of Arr2JUSIntStream;

  JUSLongStream = interface;
  Arr1JUSLongStream = array of JUSLongStream;
  Arr2JUSLongStream = array of Arr1JUSLongStream;
  Arr3JUSLongStream = array of Arr2JUSLongStream;

  JUSStream = interface;
  Arr1JUSStream = array of JUSStream;
  Arr2JUSStream = array of Arr1JUSStream;
  Arr3JUSStream = array of Arr2JUSStream;

  JUCForkJoinWorkerThread = class;
  Arr1JUCForkJoinWorkerThread = array of JUCForkJoinWorkerThread;
  Arr2JUCForkJoinWorkerThread = array of Arr1JUCForkJoinWorkerThread;
  Arr3JUCForkJoinWorkerThread = array of Arr2JUCForkJoinWorkerThread;

  JSCPKIXRevocationChecker = class;
  Arr1JSCPKIXRevocationChecker = array of JSCPKIXRevocationChecker;
  Arr2JSCPKIXRevocationChecker = array of Arr1JSCPKIXRevocationChecker;
  Arr3JSCPKIXRevocationChecker = array of Arr2JSCPKIXRevocationChecker;

  JUCCountedCompleter = class;
  Arr1JUCCountedCompleter = array of JUCCountedCompleter;
  Arr2JUCCountedCompleter = array of Arr1JUCCountedCompleter;
  Arr3JUCCountedCompleter = array of Arr2JUCCountedCompleter;

  JUCRecursiveAction = class;
  Arr1JUCRecursiveAction = array of JUCRecursiveAction;
  Arr2JUCRecursiveAction = array of Arr1JUCRecursiveAction;
  Arr3JUCRecursiveAction = array of Arr2JUCRecursiveAction;

  JUCRecursiveTask = class;
  Arr1JUCRecursiveTask = array of JUCRecursiveTask;
  Arr2JUCRecursiveTask = array of Arr1JUCRecursiveTask;
  Arr3JUCRecursiveTask = array of Arr2JUCRecursiveTask;

  JNSX509ExtendedTrustManager = class;
  Arr1JNSX509ExtendedTrustManager = array of JNSX509ExtendedTrustManager;
  Arr2JNSX509ExtendedTrustManager = array of Arr1JNSX509ExtendedTrustManager;
  Arr3JNSX509ExtendedTrustManager = array of Arr2JNSX509ExtendedTrustManager;

  ASKeyStoreParameter = class;
  Arr1ASKeyStoreParameter = array of ASKeyStoreParameter;
  Arr2ASKeyStoreParameter = array of Arr1ASKeyStoreParameter;
  Arr3ASKeyStoreParameter = array of Arr2ASKeyStoreParameter;

  ASKKeyProtection = class;
  Arr1ASKKeyProtection = array of ASKKeyProtection;
  Arr2ASKKeyProtection = array of Arr1ASKKeyProtection;
  Arr3ASKKeyProtection = array of Arr2ASKKeyProtection;

  JSCPKIXReason = class;
  Arr1JSCPKIXReason = array of JSCPKIXReason;
  Arr2JSCPKIXReason = array of Arr1JSCPKIXReason;
  Arr3JSCPKIXReason = array of Arr2JSCPKIXReason;

  JSCCertificateRevokedException = class;
  Arr1JSCCertificateRevokedException = array of JSCCertificateRevokedException;
  Arr2JSCCertificateRevokedException = array of Arr1JSCCertificateRevokedException;
  Arr3JSCCertificateRevokedException = array of Arr2JSCCertificateRevokedException;

  JCAEADBadTagException = class;
  Arr1JCAEADBadTagException = array of JCAEADBadTagException;
  Arr2JCAEADBadTagException = array of Arr1JCAEADBadTagException;
  Arr3JCAEADBadTagException = array of Arr2JCAEADBadTagException;

  AATimeAnimator = class;
  Arr1AATimeAnimator = array of AATimeAnimator;
  Arr2AATimeAnimator = array of Arr1AATimeAnimator;
  Arr3AATimeAnimator = array of Arr2AATimeAnimator;

  AITRuleBasedCollator = class;
  Arr1AITRuleBasedCollator = array of AITRuleBasedCollator;
  Arr2AITRuleBasedCollator = array of Arr1AITRuleBasedCollator;
  Arr3AITRuleBasedCollator = array of Arr2AITRuleBasedCollator;

  ATAutoTransition = class;
  Arr1ATAutoTransition = array of ATAutoTransition;
  Arr2ATAutoTransition = array of Arr1ATAutoTransition;
  Arr3ATAutoTransition = array of Arr2ATAutoTransition;

  ATExplode = class;
  Arr1ATExplode = array of ATExplode;
  Arr2ATExplode = array of Arr1ATExplode;
  Arr3ATExplode = array of Arr2ATExplode;

  ATFade = class;
  Arr1ATFade = array of ATFade;
  Arr2ATFade = array of Arr1ATFade;
  Arr3ATFade = array of Arr2ATFade;

  ATSlide = class;
  Arr1ATSlide = array of ATSlide;
  Arr2ATSlide = array of Arr1ATSlide;
  Arr3ATSlide = array of Arr2ATSlide;

  AIUCopticCalendar = class;
  Arr1AIUCopticCalendar = array of AIUCopticCalendar;
  Arr2AIUCopticCalendar = array of Arr1AIUCopticCalendar;
  Arr3AIUCopticCalendar = array of Arr2AIUCopticCalendar;

  AIUBuddhistCalendar = class;
  Arr1AIUBuddhistCalendar = array of AIUBuddhistCalendar;
  Arr2AIUBuddhistCalendar = array of Arr1AIUBuddhistCalendar;
  Arr3AIUBuddhistCalendar = array of Arr2AIUBuddhistCalendar;

  AIUJapaneseCalendar = class;
  Arr1AIUJapaneseCalendar = array of AIUJapaneseCalendar;
  Arr2AIUJapaneseCalendar = array of Arr1AIUJapaneseCalendar;
  Arr3AIUJapaneseCalendar = array of Arr2AIUJapaneseCalendar;

  AIUTaiwanCalendar = class;
  Arr1AIUTaiwanCalendar = array of AIUTaiwanCalendar;
  Arr2AIUTaiwanCalendar = array of Arr1AIUTaiwanCalendar;
  Arr3AIUTaiwanCalendar = array of Arr2AIUTaiwanCalendar;

  JUCConcurrentLinkedDeque = class;
  Arr1JUCConcurrentLinkedDeque = array of JUCConcurrentLinkedDeque;
  Arr2JUCConcurrentLinkedDeque = array of Arr1JUCConcurrentLinkedDeque;
  Arr3JUCConcurrentLinkedDeque = array of Arr2JUCConcurrentLinkedDeque;

  JUCTransferQueue = interface;
  Arr1JUCTransferQueue = array of JUCTransferQueue;
  Arr2JUCTransferQueue = array of Arr1JUCTransferQueue;
  Arr3JUCTransferQueue = array of Arr2JUCTransferQueue;

  AVAPathInterpolator = class;
  Arr1AVAPathInterpolator = array of AVAPathInterpolator;
  Arr2AVAPathInterpolator = array of Arr1AVAPathInterpolator;
  Arr3AVAPathInterpolator = array of Arr2AVAPathInterpolator;

  APDocumentsProvider = class;
  Arr1APDocumentsProvider = array of APDocumentsProvider;
  Arr2APDocumentsProvider = array of Arr1APDocumentsProvider;
  Arr3APDocumentsProvider = array of Arr2APDocumentsProvider;

  AITUnicodeSetSpanner = class;
  Arr1AITUnicodeSetSpanner = array of AITUnicodeSetSpanner;
  Arr2AITUnicodeSetSpanner = array of Arr1AITUnicodeSetSpanner;
  Arr3AITUnicodeSetSpanner = array of Arr2AITUnicodeSetSpanner;

  AOGLES32 = class;
  Arr1AOGLES32 = array of AOGLES32;
  Arr2AOGLES32 = array of Arr1AOGLES32;
  Arr3AOGLES32 = array of Arr2AOGLES32;

  ATConference = class;
  Arr1ATConference = array of ATConference;
  Arr2ATConference = array of Arr1ATConference;
  Arr3ATConference = array of Arr2ATConference;

  ARScriptIntrinsic3DLUT = class;
  Arr1ARScriptIntrinsic3DLUT = array of ARScriptIntrinsic3DLUT;
  Arr2ARScriptIntrinsic3DLUT = array of Arr1ARScriptIntrinsic3DLUT;
  Arr3ARScriptIntrinsic3DLUT = array of Arr2ARScriptIntrinsic3DLUT;

  ARScriptIntrinsicBLAS = class;
  Arr1ARScriptIntrinsicBLAS = array of ARScriptIntrinsicBLAS;
  Arr2ARScriptIntrinsicBLAS = array of Arr1ARScriptIntrinsicBLAS;
  Arr3ARScriptIntrinsicBLAS = array of Arr2ARScriptIntrinsicBLAS;

  ARScriptIntrinsicBlend = class;
  Arr1ARScriptIntrinsicBlend = array of ARScriptIntrinsicBlend;
  Arr2ARScriptIntrinsicBlend = array of Arr1ARScriptIntrinsicBlend;
  Arr3ARScriptIntrinsicBlend = array of Arr2ARScriptIntrinsicBlend;

  ARScriptIntrinsicBlur = class;
  Arr1ARScriptIntrinsicBlur = array of ARScriptIntrinsicBlur;
  Arr2ARScriptIntrinsicBlur = array of Arr1ARScriptIntrinsicBlur;
  Arr3ARScriptIntrinsicBlur = array of Arr2ARScriptIntrinsicBlur;

  ARScriptIntrinsicColorMatrix = class;
  Arr1ARScriptIntrinsicColorMatrix = array of ARScriptIntrinsicColorMatrix;
  Arr2ARScriptIntrinsicColorMatrix = array of Arr1ARScriptIntrinsicColorMatrix;
  Arr3ARScriptIntrinsicColorMatrix = array of Arr2ARScriptIntrinsicColorMatrix;

  ARScriptIntrinsicConvolve3x3 = class;
  Arr1ARScriptIntrinsicConvolve3x3 = array of ARScriptIntrinsicConvolve3x3;
  Arr2ARScriptIntrinsicConvolve3x3 = array of Arr1ARScriptIntrinsicConvolve3x3;
  Arr3ARScriptIntrinsicConvolve3x3 = array of Arr2ARScriptIntrinsicConvolve3x3;

  ARScriptIntrinsicConvolve5x5 = class;
  Arr1ARScriptIntrinsicConvolve5x5 = array of ARScriptIntrinsicConvolve5x5;
  Arr2ARScriptIntrinsicConvolve5x5 = array of Arr1ARScriptIntrinsicConvolve5x5;
  Arr3ARScriptIntrinsicConvolve5x5 = array of Arr2ARScriptIntrinsicConvolve5x5;

  ARScriptIntrinsicHistogram = class;
  Arr1ARScriptIntrinsicHistogram = array of ARScriptIntrinsicHistogram;
  Arr2ARScriptIntrinsicHistogram = array of Arr1ARScriptIntrinsicHistogram;
  Arr3ARScriptIntrinsicHistogram = array of Arr2ARScriptIntrinsicHistogram;

  ARScriptIntrinsicLUT = class;
  Arr1ARScriptIntrinsicLUT = array of ARScriptIntrinsicLUT;
  Arr2ARScriptIntrinsicLUT = array of Arr1ARScriptIntrinsicLUT;
  Arr3ARScriptIntrinsicLUT = array of Arr2ARScriptIntrinsicLUT;

  ARScriptIntrinsicResize = class;
  Arr1ARScriptIntrinsicResize = array of ARScriptIntrinsicResize;
  Arr2ARScriptIntrinsicResize = array of Arr1ARScriptIntrinsicResize;
  Arr3ARScriptIntrinsicResize = array of Arr2ARScriptIntrinsicResize;

  ARScriptIntrinsicYuvToRGB = class;
  Arr1ARScriptIntrinsicYuvToRGB = array of ARScriptIntrinsicYuvToRGB;
  Arr2ARScriptIntrinsicYuvToRGB = array of Arr1ARScriptIntrinsicYuvToRGB;
  Arr3ARScriptIntrinsicYuvToRGB = array of Arr2ARScriptIntrinsicYuvToRGB;

  ATSLocaleSpan = class;
  Arr1ATSLocaleSpan = array of ATSLocaleSpan;
  Arr2ATSLocaleSpan = array of Arr1ATSLocaleSpan;
  Arr3ATSLocaleSpan = array of Arr2ATSLocaleSpan;

  AHCCameraManager = class;
  Arr1AHCCameraManager = array of AHCCameraManager;
  Arr2AHCCameraManager = array of Arr1AHCCameraManager;
  Arr3AHCCameraManager = array of Arr2AHCCameraManager;

  JUCForkJoinPool = class;
  Arr1JUCForkJoinPool = array of JUCForkJoinPool;
  Arr2JUCForkJoinPool = array of Arr1JUCForkJoinPool;
  Arr3JUCForkJoinPool = array of Arr2JUCForkJoinPool;

  AODeadSystemException = class;
  Arr1AODeadSystemException = array of AODeadSystemException;
  Arr2AODeadSystemException = array of Arr1AODeadSystemException;
  Arr3AODeadSystemException = array of Arr2AODeadSystemException;

  ASKKeyExpiredException = class;
  Arr1ASKKeyExpiredException = array of ASKKeyExpiredException;
  Arr2ASKKeyExpiredException = array of Arr1ASKKeyExpiredException;
  Arr3ASKKeyExpiredException = array of Arr2ASKKeyExpiredException;

  ASKKeyNotYetValidException = class;
  Arr1ASKKeyNotYetValidException = array of ASKKeyNotYetValidException;
  Arr2ASKKeyNotYetValidException = array of Arr1ASKKeyNotYetValidException;
  Arr3ASKKeyNotYetValidException = array of Arr2ASKKeyNotYetValidException;

  ASKKeyPermanentlyInvalidatedException = class;
  Arr1ASKKeyPermanentlyInvalidatedException = array of ASKKeyPermanentlyInvalidatedException;
  Arr2ASKKeyPermanentlyInvalidatedException = array of Arr1ASKKeyPermanentlyInvalidatedException;
  Arr3ASKKeyPermanentlyInvalidatedException = array of Arr2ASKKeyPermanentlyInvalidatedException;

  ASKUserNotAuthenticatedException = class;
  Arr1ASKUserNotAuthenticatedException = array of ASKUserNotAuthenticatedException;
  Arr2ASKUserNotAuthenticatedException = array of Arr1ASKUserNotAuthenticatedException;
  Arr3ASKUserNotAuthenticatedException = array of Arr2ASKUserNotAuthenticatedException;

  AITSelectFormat = class;
  Arr1AITSelectFormat = array of AITSelectFormat;
  Arr2AITSelectFormat = array of Arr1AITSelectFormat;
  Arr3AITSelectFormat = array of Arr2AITSelectFormat;

  AITUFormat = class;
  Arr1AITUFormat = array of AITUFormat;
  Arr2AITUFormat = array of Arr1AITUFormat;
  Arr3AITUFormat = array of Arr2AITUFormat;

  ASVVoiceInteractionSession = class;
  Arr1ASVVoiceInteractionSession = array of ASVVoiceInteractionSession;
  Arr2ASVVoiceInteractionSession = array of Arr1ASVVoiceInteractionSession;
  Arr3ASVVoiceInteractionSession = array of Arr2ASVVoiceInteractionSession;

  JUCLinkedTransferQueue = class;
  Arr1JUCLinkedTransferQueue = array of JUCLinkedTransferQueue;
  Arr2JUCLinkedTransferQueue = array of Arr1JUCLinkedTransferQueue;
  Arr3JUCLinkedTransferQueue = array of Arr2JUCLinkedTransferQueue;

  AGDAnimatedVectorDrawable = class;
  Arr1AGDAnimatedVectorDrawable = array of AGDAnimatedVectorDrawable;
  Arr2AGDAnimatedVectorDrawable = array of Arr1AGDAnimatedVectorDrawable;
  Arr3AGDAnimatedVectorDrawable = array of Arr2AGDAnimatedVectorDrawable;

  AGDVectorDrawable = class;
  Arr1AGDVectorDrawable = array of AGDVectorDrawable;
  Arr2AGDVectorDrawable = array of Arr1AGDVectorDrawable;
  Arr3AGDVectorDrawable = array of Arr2AGDVectorDrawable;

  ANTNfcBarcode = class;
  Arr1ANTNfcBarcode = array of ANTNfcBarcode;
  Arr2ANTNfcBarcode = array of Arr1ANTNfcBarcode;
  Arr3ANTNfcBarcode = array of Arr2ANTNfcBarcode;

  AITDateFormat = class;
  Arr1AITDateFormat = array of AITDateFormat;
  Arr2AITDateFormat = array of Arr1AITDateFormat;
  Arr3AITDateFormat = array of Arr2AITDateFormat;

  AITDateIntervalFormat = class;
  Arr1AITDateIntervalFormat = array of AITDateIntervalFormat;
  Arr2AITDateIntervalFormat = array of Arr1AITDateIntervalFormat;
  Arr3AITDateIntervalFormat = array of Arr2AITDateIntervalFormat;

  AITMeasureFormat = class;
  Arr1AITMeasureFormat = array of AITMeasureFormat;
  Arr2AITMeasureFormat = array of Arr1AITMeasureFormat;
  Arr3AITMeasureFormat = array of Arr2AITMeasureFormat;

  AITMessageFormat = class;
  Arr1AITMessageFormat = array of AITMessageFormat;
  Arr2AITMessageFormat = array of Arr1AITMessageFormat;
  Arr3AITMessageFormat = array of Arr2AITMessageFormat;

  AITNumberFormat = class;
  Arr1AITNumberFormat = array of AITNumberFormat;
  Arr2AITNumberFormat = array of Arr1AITNumberFormat;
  Arr3AITNumberFormat = array of Arr2AITNumberFormat;

  AITPluralFormat = class;
  Arr1AITPluralFormat = array of AITPluralFormat;
  Arr2AITPluralFormat = array of Arr1AITPluralFormat;
  Arr3AITPluralFormat = array of Arr2AITPluralFormat;

  AITTimeZoneFormat = class;
  Arr1AITTimeZoneFormat = array of AITTimeZoneFormat;
  Arr2AITTimeZoneFormat = array of Arr1AITTimeZoneFormat;
  Arr3AITTimeZoneFormat = array of Arr2AITTimeZoneFormat;

  AGDRippleDrawable = class;
  Arr1AGDRippleDrawable = array of AGDRippleDrawable;
  Arr2AGDRippleDrawable = array of Arr1AGDRippleDrawable;
  Arr3AGDRippleDrawable = array of Arr2AGDRippleDrawable;

  AITSimpleDateFormat = class;
  Arr1AITSimpleDateFormat = array of AITSimpleDateFormat;
  Arr2AITSimpleDateFormat = array of Arr1AITSimpleDateFormat;
  Arr3AITSimpleDateFormat = array of Arr2AITSimpleDateFormat;

  AITDecimalFormat = class;
  Arr1AITDecimalFormat = array of AITDecimalFormat;
  Arr2AITDecimalFormat = array of Arr1AITDecimalFormat;
  Arr3AITDecimalFormat = array of Arr2AITDecimalFormat;

  AGDAnimatedStateListDrawable = class;
  Arr1AGDAnimatedStateListDrawable = array of AGDAnimatedStateListDrawable;
  Arr2AGDAnimatedStateListDrawable = array of Arr1AGDAnimatedStateListDrawable;
  Arr3AGDAnimatedStateListDrawable = array of Arr2AGDAnimatedStateListDrawable;

  AITCompactDecimalFormat = class;
  Arr1AITCompactDecimalFormat = array of AITCompactDecimalFormat;
  Arr2AITCompactDecimalFormat = array of Arr1AITCompactDecimalFormat;
  Arr3AITCompactDecimalFormat = array of Arr2AITCompactDecimalFormat;

  AAJJobService = class;
  Arr1AAJJobService = array of AAJJobService;
  Arr2AAJJobService = array of Arr1AAJJobService;
  Arr3AAJJobService = array of Arr2AAJJobService;

  ALSettingInjectorService = class;
  Arr1ALSettingInjectorService = array of ALSettingInjectorService;
  Arr2ALSettingInjectorService = array of Arr1ALSettingInjectorService;
  Arr3ALSettingInjectorService = array of Arr2ALSettingInjectorService;

  AMMMidiDeviceService = class;
  Arr1AMMMidiDeviceService = array of AMMMidiDeviceService;
  Arr2AMMMidiDeviceService = array of Arr1AMMMidiDeviceService;
  Arr3AMMMidiDeviceService = array of Arr2AMMMidiDeviceService;

  AMTTvInputService = class;
  Arr1AMTTvInputService = array of AMTTvInputService;
  Arr2AMTTvInputService = array of Arr1AMTTvInputService;
  Arr3AMTTvInputService = array of Arr2AMTTvInputService;

  ANCHostApduService = class;
  Arr1ANCHostApduService = array of ANCHostApduService;
  Arr2ANCHostApduService = array of Arr1ANCHostApduService;
  Arr3ANCHostApduService = array of Arr2ANCHostApduService;

  ANCHostNfcFService = class;
  Arr1ANCHostNfcFService = array of ANCHostNfcFService;
  Arr2ANCHostNfcFService = array of Arr1ANCHostNfcFService;
  Arr3ANCHostNfcFService = array of Arr2ANCHostNfcFService;

  ANCOffHostApduService = class;
  Arr1ANCOffHostApduService = array of ANCOffHostApduService;
  Arr2ANCOffHostApduService = array of Arr1ANCOffHostApduService;
  Arr3ANCOffHostApduService = array of Arr2ANCOffHostApduService;

  APPrintService = class;
  Arr1APPrintService = array of APPrintService;
  Arr2APPrintService = array of Arr1APPrintService;
  Arr3APPrintService = array of Arr2APPrintService;

  ASCCarrierMessagingService = class;
  Arr1ASCCarrierMessagingService = array of ASCCarrierMessagingService;
  Arr2ASCCarrierMessagingService = array of Arr1ASCCarrierMessagingService;
  Arr3ASCCarrierMessagingService = array of Arr2ASCCarrierMessagingService;

  ASCCarrierService = class;
  Arr1ASCCarrierService = array of ASCCarrierService;
  Arr2ASCCarrierService = array of Arr1ASCCarrierService;
  Arr3ASCCarrierService = array of Arr2ASCCarrierService;

  ASCChooserTargetService = class;
  Arr1ASCChooserTargetService = array of ASCChooserTargetService;
  Arr2ASCChooserTargetService = array of Arr1ASCChooserTargetService;
  Arr3ASCChooserTargetService = array of Arr2ASCChooserTargetService;

  ASMCameraPrewarmService = class;
  Arr1ASMCameraPrewarmService = array of ASMCameraPrewarmService;
  Arr2ASMCameraPrewarmService = array of Arr1ASMCameraPrewarmService;
  Arr3ASMCameraPrewarmService = array of Arr2ASMCameraPrewarmService;

  ASMMediaBrowserService = class;
  Arr1ASMMediaBrowserService = array of ASMMediaBrowserService;
  Arr2ASMMediaBrowserService = array of Arr1ASMMediaBrowserService;
  Arr3ASMMediaBrowserService = array of Arr2ASMMediaBrowserService;

  ASNConditionProviderService = class;
  Arr1ASNConditionProviderService = array of ASNConditionProviderService;
  Arr2ASNConditionProviderService = array of Arr1ASNConditionProviderService;
  Arr3ASNConditionProviderService = array of Arr2ASNConditionProviderService;

  ASNNotificationListenerService = class;
  Arr1ASNNotificationListenerService = array of ASNNotificationListenerService;
  Arr2ASNNotificationListenerService = array of Arr1ASNNotificationListenerService;
  Arr3ASNNotificationListenerService = array of Arr2ASNNotificationListenerService;

  ASQTileService = class;
  Arr1ASQTileService = array of ASQTileService;
  Arr2ASQTileService = array of Arr1ASQTileService;
  Arr3ASQTileService = array of Arr2ASQTileService;

  ASVVoiceInteractionService = class;
  Arr1ASVVoiceInteractionService = array of ASVVoiceInteractionService;
  Arr2ASVVoiceInteractionService = array of Arr1ASVVoiceInteractionService;
  Arr3ASVVoiceInteractionService = array of Arr2ASVVoiceInteractionService;

  ASVVoiceInteractionSessionService = class;
  Arr1ASVVoiceInteractionSessionService = array of ASVVoiceInteractionSessionService;
  Arr2ASVVoiceInteractionSessionService = array of Arr1ASVVoiceInteractionSessionService;
  Arr3ASVVoiceInteractionSessionService = array of Arr2ASVVoiceInteractionSessionService;

  ASVVrListenerService = class;
  Arr1ASVVrListenerService = array of ASVVrListenerService;
  Arr2ASVVrListenerService = array of Arr1ASVVrListenerService;
  Arr3ASVVrListenerService = array of Arr2ASVVrListenerService;

  ATConnectionService = class;
  Arr1ATConnectionService = array of ATConnectionService;
  Arr2ATConnectionService = array of Arr1ATConnectionService;
  Arr3ATConnectionService = array of Arr2ATConnectionService;

  ATInCallService = class;
  Arr1ATInCallService = array of ATInCallService;
  Arr2ATInCallService = array of Arr1ATInCallService;
  Arr3ATInCallService = array of Arr2ATInCallService;

  ATCall = class;
  Arr1ATCall = array of ATCall;
  Arr2ATCall = array of Arr1ATCall;
  Arr3ATCall = array of Arr2ATCall;

  ATCallScreeningService = class;
  Arr1ATCallScreeningService = array of ATCallScreeningService;
  Arr2ATCallScreeningService = array of Arr1ATCallScreeningService;
  Arr3ATCallScreeningService = array of Arr2ATCallScreeningService;

  AMTTvView = class;
  Arr1AMTTvView = array of AMTTvView;
  Arr2AMTTvView = array of Arr1AMTTvView;
  Arr3AMTTvView = array of Arr2AMTTvView;

  ASDDreamService = class;
  Arr1ASDDreamService = array of ASDDreamService;
  Arr2ASDDreamService = array of Arr1ASDDreamService;
  Arr3ASDDreamService = array of Arr2ASDDreamService;

  AAMediaRouteButton = class;
  Arr1AAMediaRouteButton = array of AAMediaRouteButton;
  Arr2AAMediaRouteButton = array of Arr1AAMediaRouteButton;
  Arr3AAMediaRouteButton = array of Arr2AAMediaRouteButton;

  AAPresentation = class;
  Arr1AAPresentation = array of AAPresentation;
  Arr2AAPresentation = array of Arr1AAPresentation;
  Arr3AAPresentation = array of Arr2AAPresentation;

  AAMediaRouteActionProvider = class;
  Arr1AAMediaRouteActionProvider = array of AAMediaRouteActionProvider;
  Arr2AAMediaRouteActionProvider = array of Arr1AAMediaRouteActionProvider;
  Arr3AAMediaRouteActionProvider = array of Arr2AAMediaRouteActionProvider;

  AWActionMenuView = class;
  Arr1AWActionMenuView = array of AWActionMenuView;
  Arr2AWActionMenuView = array of Arr1AWActionMenuView;
  Arr3AWActionMenuView = array of Arr2AWActionMenuView;

  AWTextClock = class;
  Arr1AWTextClock = array of AWTextClock;
  Arr2AWTextClock = array of Arr1AWTextClock;
  Arr3AWTextClock = array of Arr2AWTextClock;

  AWToolbar = class;
  Arr1AWToolbar = array of AWToolbar;
  Arr2AWToolbar = array of Arr1AWToolbar;
  Arr3AWToolbar = array of Arr2AWToolbar;

  AAAccessibilityButtonController = class;
  Arr1AAAccessibilityButtonController = array of AAAccessibilityButtonController;
  Arr2AAAccessibilityButtonController = array of Arr1AAAccessibilityButtonController;
  Arr3AAAccessibilityButtonController = array of Arr2AAAccessibilityButtonController;

  AAFingerprintGestureController = class;
  Arr1AAFingerprintGestureController = array of AAFingerprintGestureController;
  Arr2AAFingerprintGestureController = array of Arr1AAFingerprintGestureController;
  Arr3AAFingerprintGestureController = array of Arr2AAFingerprintGestureController;

  AAGestureDescription = class;
  Arr1AAGestureDescription = array of AAGestureDescription;
  Arr2AAGestureDescription = array of Arr1AAGestureDescription;
  Arr3AAGestureDescription = array of Arr2AAGestureDescription;

  AAJJobServiceEngine = class;
  Arr1AAJJobServiceEngine = array of AAJJobServiceEngine;
  Arr2AAJJobServiceEngine = array of Arr1AAJJobServiceEngine;
  Arr3AAJJobServiceEngine = array of Arr2AAJJobServiceEngine;

  AAUStorageStatsManager = class;
  Arr1AAUStorageStatsManager = array of AAUStorageStatsManager;
  Arr2AAUStorageStatsManager = array of Arr1AAUStorageStatsManager;
  Arr3AAUStorageStatsManager = array of Arr2AAUStorageStatsManager;

  ABLAdvertisingSet = class;
  Arr1ABLAdvertisingSet = array of ABLAdvertisingSet;
  Arr2ABLAdvertisingSet = array of Arr1ABLAdvertisingSet;
  Arr3ABLAdvertisingSet = array of Arr2ABLAdvertisingSet;

  ABLAdvertisingSetCallback = class;
  Arr1ABLAdvertisingSetCallback = array of ABLAdvertisingSetCallback;
  Arr2ABLAdvertisingSetCallback = array of Arr1ABLAdvertisingSetCallback;
  Arr3ABLAdvertisingSetCallback = array of Arr2ABLAdvertisingSetCallback;

  ACCompanionDeviceManager = class;
  Arr1ACCompanionDeviceManager = array of ACCompanionDeviceManager;
  Arr2ACCompanionDeviceManager = array of Arr1ACCompanionDeviceManager;
  Arr3ACCompanionDeviceManager = array of Arr2ACCompanionDeviceManager;

  ACQuickViewConstants = class;
  Arr1ACQuickViewConstants = array of ACQuickViewConstants;
  Arr2ACQuickViewConstants = array of Arr1ACQuickViewConstants;
  Arr3ACQuickViewConstants = array of Arr2ACQuickViewConstants;

  AGFFontVariationAxis = class;
  Arr1AGFFontVariationAxis = array of AGFFontVariationAxis;
  Arr2AGFFontVariationAxis = array of Arr1AGFFontVariationAxis;
  Arr3AGFFontVariationAxis = array of Arr2AGFFontVariationAxis;

  AITListFormatter = class;
  Arr1AITListFormatter = array of AITListFormatter;
  Arr2AITListFormatter = array of Arr1AITListFormatter;
  Arr3AITListFormatter = array of Arr2AITListFormatter;

  AITScientificNumberFormatter = class;
  Arr1AITScientificNumberFormatter = array of AITScientificNumberFormatter;
  Arr2AITScientificNumberFormatter = array of Arr1AITScientificNumberFormatter;
  Arr3AITScientificNumberFormatter = array of Arr2AITScientificNumberFormatter;

  AIUUniversalTimeScale = class;
  Arr1AIUUniversalTimeScale = array of AIUUniversalTimeScale;
  Arr2AIUUniversalTimeScale = array of Arr1AIUUniversalTimeScale;
  Arr3AIUUniversalTimeScale = array of Arr2AIUUniversalTimeScale;

  ANNetworkSpecifier = class;
  Arr1ANNetworkSpecifier = array of ANNetworkSpecifier;
  Arr2ANNetworkSpecifier = array of Arr1ANNetworkSpecifier;
  Arr3ANNetworkSpecifier = array of Arr2ANNetworkSpecifier;

  ANWAAttachCallback = class;
  Arr1ANWAAttachCallback = array of ANWAAttachCallback;
  Arr2ANWAAttachCallback = array of Arr1ANWAAttachCallback;
  Arr3ANWAAttachCallback = array of Arr2ANWAAttachCallback;

  ANWADiscoverySessionCallback = class;
  Arr1ANWADiscoverySessionCallback = array of ANWADiscoverySessionCallback;
  Arr2ANWADiscoverySessionCallback = array of Arr1ANWADiscoverySessionCallback;
  Arr3ANWADiscoverySessionCallback = array of Arr2ANWADiscoverySessionCallback;

  ANWAIdentityChangedListener = class;
  Arr1ANWAIdentityChangedListener = array of ANWAIdentityChangedListener;
  Arr2ANWAIdentityChangedListener = array of Arr1ANWAIdentityChangedListener;
  Arr3ANWAIdentityChangedListener = array of Arr2ANWAIdentityChangedListener;

  ANWAPeerHandle = class;
  Arr1ANWAPeerHandle = array of ANWAPeerHandle;
  Arr2ANWAPeerHandle = array of Arr1ANWAPeerHandle;
  Arr3ANWAPeerHandle = array of Arr2ANWAPeerHandle;

  ANWAWifiAwareManager = class;
  Arr1ANWAWifiAwareManager = array of ANWAWifiAwareManager;
  Arr2ANWAWifiAwareManager = array of Arr1ANWAWifiAwareManager;
  Arr3ANWAWifiAwareManager = array of Arr2ANWAWifiAwareManager;

  ANWHConfigParser = class;
  Arr1ANWHConfigParser = array of ANWHConfigParser;
  Arr2ANWHConfigParser = array of Arr1ANWHConfigParser;
  Arr3ANWHConfigParser = array of Arr2ANWHConfigParser;

  ANWHOPpsMoParser = class;
  Arr1ANWHOPpsMoParser = array of ANWHOPpsMoParser;
  Arr2ANWHOPpsMoParser = array of Arr1ANWHOPpsMoParser;
  Arr3ANWHOPpsMoParser = array of Arr2ANWHOPpsMoParser;

  AOProxyFileDescriptorCallback = class;
  Arr1AOProxyFileDescriptorCallback = array of AOProxyFileDescriptorCallback;
  Arr2AOProxyFileDescriptorCallback = array of Arr1AOProxyFileDescriptorCallback;
  Arr3AOProxyFileDescriptorCallback = array of Arr2AOProxyFileDescriptorCallback;

  AOTestLooperManager = class;
  Arr1AOTestLooperManager = array of AOTestLooperManager;
  Arr2AOTestLooperManager = array of Arr1AOTestLooperManager;
  Arr3AOTestLooperManager = array of Arr2AOTestLooperManager;

  APPreferenceDataStore = interface;
  Arr1APPreferenceDataStore = array of APPreferenceDataStore;
  Arr2APPreferenceDataStore = array of Arr1APPreferenceDataStore;
  Arr3APPreferenceDataStore = array of Arr2APPreferenceDataStore;

  APFontRequest = class;
  Arr1APFontRequest = array of APFontRequest;
  Arr2APFontRequest = array of Arr1APFontRequest;
  Arr3APFontRequest = array of Arr2APFontRequest;

  ASAFillCallback = class;
  Arr1ASAFillCallback = array of ASAFillCallback;
  Arr2ASAFillCallback = array of Arr1ASAFillCallback;
  Arr3ASAFillCallback = array of Arr2ASAFillCallback;

  ASASaveCallback = class;
  Arr1ASASaveCallback = array of ASASaveCallback;
  Arr2ASASaveCallback = array of Arr1ASASaveCallback;
  Arr3ASASaveCallback = array of Arr2ASASaveCallback;

  ASATransformation = interface;
  Arr1ASATransformation = array of ASATransformation;
  Arr2ASATransformation = array of Arr1ASATransformation;
  Arr3ASATransformation = array of Arr2ASATransformation;

  ASAValidator = interface;
  Arr1ASAValidator = array of ASAValidator;
  Arr2ASAValidator = array of Arr1ASAValidator;
  Arr3ASAValidator = array of Arr2ASAValidator;

  ASAValidators = class;
  Arr1ASAValidators = array of ASAValidators;
  Arr2ASAValidators = array of Arr1ASAValidators;
  Arr3ASAValidators = array of Arr2ASAValidators;

  AVAAccessibilityRequestPreparer = class;
  Arr1AVAAccessibilityRequestPreparer = array of AVAAccessibilityRequestPreparer;
  Arr2AVAAccessibilityRequestPreparer = array of Arr1AVAAccessibilityRequestPreparer;
  Arr3AVAAccessibilityRequestPreparer = array of Arr2AVAAccessibilityRequestPreparer;

  AVAAutofillManager = class;
  Arr1AVAAutofillManager = array of AVAAutofillManager;
  Arr2AVAAutofillManager = array of Arr1AVAAutofillManager;
  Arr3AVAAutofillManager = array of Arr2AVAAutofillManager;

  AVTTextClassificationManager = class;
  Arr1AVTTextClassificationManager = array of AVTTextClassificationManager;
  Arr2AVTTextClassificationManager = array of Arr1AVTTextClassificationManager;
  Arr3AVTTextClassificationManager = array of Arr2AVTTextClassificationManager;

  AVTTextClassifier = interface;
  Arr1AVTTextClassifier = array of AVTTextClassifier;
  Arr2AVTTextClassifier = array of Arr1AVTTextClassifier;
  Arr3AVTTextClassifier = array of Arr2AVTTextClassifier;

  AVTTextSelection = class;
  Arr1AVTTextSelection = array of AVTTextSelection;
  Arr2AVTTextSelection = array of Arr1AVTTextSelection;
  Arr3AVTTextSelection = array of Arr2AVTTextSelection;

  AWRenderProcessGoneDetail = class;
  Arr1AWRenderProcessGoneDetail = array of AWRenderProcessGoneDetail;
  Arr2AWRenderProcessGoneDetail = array of Arr1AWRenderProcessGoneDetail;
  Arr3AWRenderProcessGoneDetail = array of Arr2AWRenderProcessGoneDetail;

  AWSafeBrowsingResponse = class;
  Arr1AWSafeBrowsingResponse = array of AWSafeBrowsingResponse;
  Arr2AWSafeBrowsingResponse = array of Arr1AWSafeBrowsingResponse;
  Arr3AWSafeBrowsingResponse = array of Arr2AWSafeBrowsingResponse;

  JLICallSite = class;
  Arr1JLICallSite = array of JLICallSite;
  Arr2JLICallSite = array of Arr1JLICallSite;
  Arr3JLICallSite = array of Arr2JLICallSite;

  JLIMethodHandle = class;
  Arr1JLIMethodHandle = array of JLIMethodHandle;
  Arr2JLIMethodHandle = array of Arr1JLIMethodHandle;
  Arr3JLIMethodHandle = array of Arr2JLIMethodHandle;

  JLIMethodHandles = class;
  Arr1JLIMethodHandles = array of JLIMethodHandles;
  Arr2JLIMethodHandles = array of Arr1JLIMethodHandles;
  Arr3JLIMethodHandles = array of Arr2JLIMethodHandles;

  JNCAsynchronousChannelGroup = class;
  Arr1JNCAsynchronousChannelGroup = array of JNCAsynchronousChannelGroup;
  Arr2JNCAsynchronousChannelGroup = array of Arr1JNCAsynchronousChannelGroup;
  Arr3JNCAsynchronousChannelGroup = array of Arr2JNCAsynchronousChannelGroup;

  JNCCompletionHandler = interface;
  Arr1JNCCompletionHandler = array of JNCCompletionHandler;
  Arr2JNCCompletionHandler = array of Arr1JNCCompletionHandler;
  Arr3JNCCompletionHandler = array of Arr2JNCCompletionHandler;

  JNCMembershipKey = class;
  Arr1JNCMembershipKey = array of JNCMembershipKey;
  Arr2JNCMembershipKey = array of Arr1JNCMembershipKey;
  Arr3JNCMembershipKey = array of Arr2JNCMembershipKey;

  JNCSAsynchronousChannelProvider = class;
  Arr1JNCSAsynchronousChannelProvider = array of JNCSAsynchronousChannelProvider;
  Arr2JNCSAsynchronousChannelProvider = array of Arr1JNCSAsynchronousChannelProvider;
  Arr3JNCSAsynchronousChannelProvider = array of Arr2JNCSAsynchronousChannelProvider;

  JNFCopyOption = interface;
  Arr1JNFCopyOption = array of JNFCopyOption;
  Arr2JNFCopyOption = array of Arr1JNFCopyOption;
  Arr3JNFCopyOption = array of Arr2JNFCopyOption;

  JNFFileStore = class;
  Arr1JNFFileStore = array of JNFFileStore;
  Arr2JNFFileStore = array of Arr1JNFFileStore;
  Arr3JNFFileStore = array of Arr2JNFFileStore;

  JNFFileSystems = class;
  Arr1JNFFileSystems = array of JNFFileSystems;
  Arr2JNFFileSystems = array of Arr1JNFFileSystems;
  Arr3JNFFileSystems = array of Arr2JNFFileSystems;

  JNFFileVisitor = interface;
  Arr1JNFFileVisitor = array of JNFFileVisitor;
  Arr2JNFFileVisitor = array of Arr1JNFFileVisitor;
  Arr3JNFFileVisitor = array of Arr2JNFFileVisitor;

  JNFOpenOption = interface;
  Arr1JNFOpenOption = array of JNFOpenOption;
  Arr2JNFOpenOption = array of Arr1JNFOpenOption;
  Arr3JNFOpenOption = array of Arr2JNFOpenOption;

  JNFPathMatcher = interface;
  Arr1JNFPathMatcher = array of JNFPathMatcher;
  Arr2JNFPathMatcher = array of Arr1JNFPathMatcher;
  Arr3JNFPathMatcher = array of Arr2JNFPathMatcher;

  JNFPaths = class;
  Arr1JNFPaths = array of JNFPaths;
  Arr2JNFPaths = array of Arr1JNFPaths;
  Arr3JNFPaths = array of Arr2JNFPaths;

  JNFWatchEvent = interface;
  Arr1JNFWatchEvent = array of JNFWatchEvent;
  Arr2JNFWatchEvent = array of Arr1JNFWatchEvent;
  Arr3JNFWatchEvent = array of Arr2JNFWatchEvent;

  JNFWatchKey = interface;
  Arr1JNFWatchKey = array of JNFWatchKey;
  Arr2JNFWatchKey = array of Arr1JNFWatchKey;
  Arr3JNFWatchKey = array of Arr2JNFWatchKey;

  JNFAAclEntry = class;
  Arr1JNFAAclEntry = array of JNFAAclEntry;
  Arr2JNFAAclEntry = array of Arr1JNFAAclEntry;
  Arr3JNFAAclEntry = array of Arr2JNFAAclEntry;

  JNFAAttributeView = interface;
  Arr1JNFAAttributeView = array of JNFAAttributeView;
  Arr2JNFAAttributeView = array of Arr1JNFAAttributeView;
  Arr3JNFAAttributeView = array of Arr2JNFAAttributeView;

  JNFABasicFileAttributes = interface;
  Arr1JNFABasicFileAttributes = array of JNFABasicFileAttributes;
  Arr2JNFABasicFileAttributes = array of Arr1JNFABasicFileAttributes;
  Arr3JNFABasicFileAttributes = array of Arr2JNFABasicFileAttributes;

  JNFAFileAttribute = interface;
  Arr1JNFAFileAttribute = array of JNFAFileAttribute;
  Arr2JNFAFileAttribute = array of Arr1JNFAFileAttribute;
  Arr3JNFAFileAttribute = array of Arr2JNFAFileAttribute;

  JNFAPosixFilePermissions = class;
  Arr1JNFAPosixFilePermissions = array of JNFAPosixFilePermissions;
  Arr2JNFAPosixFilePermissions = array of Arr1JNFAPosixFilePermissions;
  Arr3JNFAPosixFilePermissions = array of Arr2JNFAPosixFilePermissions;

  JNFAUserPrincipalLookupService = class;
  Arr1JNFAUserPrincipalLookupService = array of JNFAUserPrincipalLookupService;
  Arr2JNFAUserPrincipalLookupService = array of Arr1JNFAUserPrincipalLookupService;
  Arr3JNFAUserPrincipalLookupService = array of Arr2JNFAUserPrincipalLookupService;

  JNFSFileTypeDetector = class;
  Arr1JNFSFileTypeDetector = array of JNFSFileTypeDetector;
  Arr2JNFSFileTypeDetector = array of Arr1JNFSFileTypeDetector;
  Arr3JNFSFileTypeDetector = array of Arr2JNFSFileTypeDetector;

  JTClock = class;
  Arr1JTClock = array of JTClock;
  Arr2JTClock = array of Arr1JTClock;
  Arr3JTClock = array of Arr2JTClock;

  JTFDateTimeFormatter = class;
  Arr1JTFDateTimeFormatter = array of JTFDateTimeFormatter;
  Arr2JTFDateTimeFormatter = array of Arr1JTFDateTimeFormatter;
  Arr3JTFDateTimeFormatter = array of Arr2JTFDateTimeFormatter;

  JTFDateTimeFormatterBuilder = class;
  Arr1JTFDateTimeFormatterBuilder = array of JTFDateTimeFormatterBuilder;
  Arr2JTFDateTimeFormatterBuilder = array of Arr1JTFDateTimeFormatterBuilder;
  Arr3JTFDateTimeFormatterBuilder = array of Arr2JTFDateTimeFormatterBuilder;

  JTFDecimalStyle = class;
  Arr1JTFDecimalStyle = array of JTFDecimalStyle;
  Arr2JTFDecimalStyle = array of Arr1JTFDecimalStyle;
  Arr3JTFDecimalStyle = array of Arr2JTFDecimalStyle;

  JTTIsoFields = class;
  Arr1JTTIsoFields = array of JTTIsoFields;
  Arr2JTTIsoFields = array of Arr1JTTIsoFields;
  Arr3JTTIsoFields = array of Arr2JTTIsoFields;

  JTTJulianFields = class;
  Arr1JTTJulianFields = array of JTTJulianFields;
  Arr2JTTJulianFields = array of Arr1JTTJulianFields;
  Arr3JTTJulianFields = array of Arr2JTTJulianFields;

  JTTTemporalAccessor = interface;
  Arr1JTTTemporalAccessor = array of JTTTemporalAccessor;
  Arr2JTTTemporalAccessor = array of Arr1JTTTemporalAccessor;
  Arr3JTTTemporalAccessor = array of Arr2JTTTemporalAccessor;

  JTTTemporalAdjuster = interface;
  Arr1JTTTemporalAdjuster = array of JTTTemporalAdjuster;
  Arr2JTTTemporalAdjuster = array of Arr1JTTTemporalAdjuster;
  Arr3JTTTemporalAdjuster = array of Arr2JTTTemporalAdjuster;

  JTTTemporalAdjusters = class;
  Arr1JTTTemporalAdjusters = array of JTTTemporalAdjusters;
  Arr2JTTTemporalAdjusters = array of Arr1JTTTemporalAdjusters;
  Arr3JTTTemporalAdjusters = array of Arr2JTTTemporalAdjusters;

  JTTTemporalAmount = interface;
  Arr1JTTTemporalAmount = array of JTTTemporalAmount;
  Arr2JTTTemporalAmount = array of Arr1JTTTemporalAmount;
  Arr3JTTTemporalAmount = array of Arr2JTTTemporalAmount;

  JTTTemporalField = interface;
  Arr1JTTTemporalField = array of JTTTemporalField;
  Arr2JTTTemporalField = array of Arr1JTTTemporalField;
  Arr3JTTTemporalField = array of Arr2JTTTemporalField;

  JTTTemporalQueries = class;
  Arr1JTTTemporalQueries = array of JTTTemporalQueries;
  Arr2JTTTemporalQueries = array of Arr1JTTTemporalQueries;
  Arr3JTTTemporalQueries = array of Arr2JTTTemporalQueries;

  JTTTemporalQuery = interface;
  Arr1JTTTemporalQuery = array of JTTTemporalQuery;
  Arr2JTTTemporalQuery = array of Arr1JTTTemporalQuery;
  Arr3JTTTemporalQuery = array of Arr2JTTTemporalQuery;

  JTTTemporalUnit = interface;
  Arr1JTTTemporalUnit = array of JTTTemporalUnit;
  Arr2JTTTemporalUnit = array of Arr1JTTTemporalUnit;
  Arr3JTTTemporalUnit = array of Arr2JTTTemporalUnit;

  JUBase64 = class;
  Arr1JUBase64 = array of JUBase64;
  Arr2JUBase64 = array of Arr1JUBase64;
  Arr3JUBase64 = array of Arr2JUBase64;

  AMMediaCasException = class;
  Arr1AMMediaCasException = array of AMMediaCasException;
  Arr2AMMediaCasException = array of Arr1AMMediaCasException;
  Arr3AMMediaCasException = array of Arr2AMMediaCasException;

  JLILambdaConversionException = class;
  Arr1JLILambdaConversionException = array of JLILambdaConversionException;
  Arr2JLILambdaConversionException = array of Arr1JLILambdaConversionException;
  Arr3JLILambdaConversionException = array of Arr2JLILambdaConversionException;

  JLIWrongMethodTypeException = class;
  Arr1JLIWrongMethodTypeException = array of JLIWrongMethodTypeException;
  Arr2JLIWrongMethodTypeException = array of Arr1JLIWrongMethodTypeException;
  Arr3JLIWrongMethodTypeException = array of Arr2JLIWrongMethodTypeException;

  JNFFileSystemAlreadyExistsException = class;
  Arr1JNFFileSystemAlreadyExistsException = array of JNFFileSystemAlreadyExistsException;
  Arr2JNFFileSystemAlreadyExistsException = array of Arr1JNFFileSystemAlreadyExistsException;
  Arr3JNFFileSystemAlreadyExistsException = array of Arr2JNFFileSystemAlreadyExistsException;

  JNFFileSystemNotFoundException = class;
  Arr1JNFFileSystemNotFoundException = array of JNFFileSystemNotFoundException;
  Arr2JNFFileSystemNotFoundException = array of Arr1JNFFileSystemNotFoundException;
  Arr3JNFFileSystemNotFoundException = array of Arr2JNFFileSystemNotFoundException;

  JNFProviderNotFoundException = class;
  Arr1JNFProviderNotFoundException = array of JNFProviderNotFoundException;
  Arr2JNFProviderNotFoundException = array of Arr1JNFProviderNotFoundException;
  Arr3JNFProviderNotFoundException = array of Arr2JNFProviderNotFoundException;

  JTDateTimeException = class;
  Arr1JTDateTimeException = array of JTDateTimeException;
  Arr2JTDateTimeException = array of Arr1JTDateTimeException;
  Arr3JTDateTimeException = array of Arr2JTDateTimeException;

  ASStructTimespec = class;
  Arr1ASStructTimespec = array of ASStructTimespec;
  Arr2ASStructTimespec = array of Arr1ASStructTimespec;
  Arr3ASStructTimespec = array of Arr2ASStructTimespec;

  JNFAFileTime = class;
  Arr1JNFAFileTime = array of JNFAFileTime;
  Arr2JNFAFileTime = array of Arr1JNFAFileTime;
  Arr3JNFAFileTime = array of Arr2JNFAFileTime;

  JTCChronology = interface;
  Arr1JTCChronology = array of JTCChronology;
  Arr2JTCChronology = array of Arr1JTCChronology;
  Arr3JTCChronology = array of Arr2JTCChronology;

  AGColorSpace = class;
  Arr1AGColorSpace = array of AGColorSpace;
  Arr2AGColorSpace = array of Arr1AGColorSpace;
  Arr3AGColorSpace = array of Arr2AGColorSpace;

  JNFAccessMode = class;
  Arr1JNFAccessMode = array of JNFAccessMode;
  Arr2JNFAccessMode = array of Arr1JNFAccessMode;
  Arr3JNFAccessMode = array of Arr2JNFAccessMode;

  JNFFileVisitOption = class;
  Arr1JNFFileVisitOption = array of JNFFileVisitOption;
  Arr2JNFFileVisitOption = array of Arr1JNFFileVisitOption;
  Arr3JNFFileVisitOption = array of Arr2JNFFileVisitOption;

  JNFFileVisitResult = class;
  Arr1JNFFileVisitResult = array of JNFFileVisitResult;
  Arr2JNFFileVisitResult = array of Arr1JNFFileVisitResult;
  Arr3JNFFileVisitResult = array of Arr2JNFFileVisitResult;

  JNFAAclEntryFlag = class;
  Arr1JNFAAclEntryFlag = array of JNFAAclEntryFlag;
  Arr2JNFAAclEntryFlag = array of Arr1JNFAAclEntryFlag;
  Arr3JNFAAclEntryFlag = array of Arr2JNFAAclEntryFlag;

  JNFAAclEntryPermission = class;
  Arr1JNFAAclEntryPermission = array of JNFAAclEntryPermission;
  Arr2JNFAAclEntryPermission = array of Arr1JNFAAclEntryPermission;
  Arr3JNFAAclEntryPermission = array of Arr2JNFAAclEntryPermission;

  JNFAAclEntryType = class;
  Arr1JNFAAclEntryType = array of JNFAAclEntryType;
  Arr2JNFAAclEntryType = array of Arr1JNFAAclEntryType;
  Arr3JNFAAclEntryType = array of Arr2JNFAAclEntryType;

  JNFAPosixFilePermission = class;
  Arr1JNFAPosixFilePermission = array of JNFAPosixFilePermission;
  Arr2JNFAPosixFilePermission = array of Arr1JNFAPosixFilePermission;
  Arr3JNFAPosixFilePermission = array of Arr2JNFAPosixFilePermission;

  JTFFormatStyle = class;
  Arr1JTFFormatStyle = array of JTFFormatStyle;
  Arr2JTFFormatStyle = array of Arr1JTFFormatStyle;
  Arr3JTFFormatStyle = array of Arr2JTFFormatStyle;

  JTFResolverStyle = class;
  Arr1JTFResolverStyle = array of JTFResolverStyle;
  Arr2JTFResolverStyle = array of Arr1JTFResolverStyle;
  Arr3JTFResolverStyle = array of Arr2JTFResolverStyle;

  JTFSignStyle = class;
  Arr1JTFSignStyle = array of JTFSignStyle;
  Arr2JTFSignStyle = array of Arr1JTFSignStyle;
  Arr3JTFSignStyle = array of Arr2JTFSignStyle;

  JTFTextStyle = class;
  Arr1JTFTextStyle = array of JTFTextStyle;
  Arr2JTFTextStyle = array of Arr1JTFTextStyle;
  Arr3JTFTextStyle = array of Arr2JTFTextStyle;

  AUHalf = class;
  Arr1AUHalf = array of AUHalf;
  Arr2AUHalf = array of Arr1AUHalf;
  Arr3AUHalf = array of Arr2AUHalf;

  JLIMethodType = class;
  Arr1JLIMethodType = array of JLIMethodType;
  Arr2JLIMethodType = array of Arr1JLIMethodType;
  Arr3JLIMethodType = array of Arr2JLIMethodType;

  JTZoneId = class;
  Arr1JTZoneId = array of JTZoneId;
  Arr2JTZoneId = array of Arr1JTZoneId;
  Arr3JTZoneId = array of Arr2JTZoneId;

  JTTValueRange = class;
  Arr1JTTValueRange = array of JTTValueRange;
  Arr2JTTValueRange = array of Arr1JTTValueRange;
  Arr3JTTValueRange = array of Arr2JTTValueRange;

  JTTWeekFields = class;
  Arr1JTTWeekFields = array of JTTWeekFields;
  Arr2JTTWeekFields = array of Arr1JTTWeekFields;
  Arr3JTTWeekFields = array of Arr2JTTWeekFields;

  JTZZoneOffsetTransition = class;
  Arr1JTZZoneOffsetTransition = array of JTZZoneOffsetTransition;
  Arr2JTZZoneOffsetTransition = array of Arr1JTZZoneOffsetTransition;
  Arr3JTZZoneOffsetTransition = array of Arr2JTZZoneOffsetTransition;

  JTZZoneOffsetTransitionRule = class;
  Arr1JTZZoneOffsetTransitionRule = array of JTZZoneOffsetTransitionRule;
  Arr2JTZZoneOffsetTransitionRule = array of Arr1JTZZoneOffsetTransitionRule;
  Arr3JTZZoneOffsetTransitionRule = array of Arr2JTZZoneOffsetTransitionRule;

  JTZZoneRules = class;
  Arr1JTZZoneRules = array of JTZZoneRules;
  Arr2JTZZoneRules = array of Arr1JTZZoneRules;
  Arr3JTZZoneRules = array of Arr2JTZZoneRules;

  AMMediaCasStateException = class;
  Arr1AMMediaCasStateException = array of AMMediaCasStateException;
  Arr2AMMediaCasStateException = array of Arr1AMMediaCasStateException;
  Arr3AMMediaCasStateException = array of Arr2AMMediaCasStateException;

  JNCAcceptPendingException = class;
  Arr1JNCAcceptPendingException = array of JNCAcceptPendingException;
  Arr2JNCAcceptPendingException = array of Arr1JNCAcceptPendingException;
  Arr3JNCAcceptPendingException = array of Arr2JNCAcceptPendingException;

  JNCReadPendingException = class;
  Arr1JNCReadPendingException = array of JNCReadPendingException;
  Arr2JNCReadPendingException = array of Arr1JNCReadPendingException;
  Arr3JNCReadPendingException = array of Arr2JNCReadPendingException;

  JNCShutdownChannelGroupException = class;
  Arr1JNCShutdownChannelGroupException = array of JNCShutdownChannelGroupException;
  Arr2JNCShutdownChannelGroupException = array of Arr1JNCShutdownChannelGroupException;
  Arr3JNCShutdownChannelGroupException = array of Arr2JNCShutdownChannelGroupException;

  JNCWritePendingException = class;
  Arr1JNCWritePendingException = array of JNCWritePendingException;
  Arr2JNCWritePendingException = array of Arr1JNCWritePendingException;
  Arr3JNCWritePendingException = array of Arr2JNCWritePendingException;

  JNFClosedDirectoryStreamException = class;
  Arr1JNFClosedDirectoryStreamException = array of JNFClosedDirectoryStreamException;
  Arr2JNFClosedDirectoryStreamException = array of Arr1JNFClosedDirectoryStreamException;
  Arr3JNFClosedDirectoryStreamException = array of Arr2JNFClosedDirectoryStreamException;

  JNFClosedFileSystemException = class;
  Arr1JNFClosedFileSystemException = array of JNFClosedFileSystemException;
  Arr2JNFClosedFileSystemException = array of Arr1JNFClosedFileSystemException;
  Arr3JNFClosedFileSystemException = array of Arr2JNFClosedFileSystemException;

  JNFClosedWatchServiceException = class;
  Arr1JNFClosedWatchServiceException = array of JNFClosedWatchServiceException;
  Arr2JNFClosedWatchServiceException = array of Arr1JNFClosedWatchServiceException;
  Arr3JNFClosedWatchServiceException = array of Arr2JNFClosedWatchServiceException;

  JNCInterruptedByTimeoutException = class;
  Arr1JNCInterruptedByTimeoutException = array of JNCInterruptedByTimeoutException;
  Arr2JNCInterruptedByTimeoutException = array of Arr1JNCInterruptedByTimeoutException;
  Arr3JNCInterruptedByTimeoutException = array of Arr2JNCInterruptedByTimeoutException;

  JNFFileSystemException = class;
  Arr1JNFFileSystemException = array of JNFFileSystemException;
  Arr2JNFFileSystemException = array of Arr1JNFFileSystemException;
  Arr3JNFFileSystemException = array of Arr2JNFFileSystemException;

  JNFAUserPrincipalNotFoundException = class;
  Arr1JNFAUserPrincipalNotFoundException = array of JNFAUserPrincipalNotFoundException;
  Arr2JNFAUserPrincipalNotFoundException = array of Arr1JNFAUserPrincipalNotFoundException;
  Arr3JNFAUserPrincipalNotFoundException = array of Arr2JNFAUserPrincipalNotFoundException;

  JLBootstrapMethodError = class;
  Arr1JLBootstrapMethodError = array of JLBootstrapMethodError;
  Arr2JLBootstrapMethodError = array of Arr1JLBootstrapMethodError;
  Arr3JLBootstrapMethodError = array of Arr2JLBootstrapMethodError;

  JNCIllegalChannelGroupException = class;
  Arr1JNCIllegalChannelGroupException = array of JNCIllegalChannelGroupException;
  Arr2JNCIllegalChannelGroupException = array of Arr1JNCIllegalChannelGroupException;
  Arr3JNCIllegalChannelGroupException = array of Arr2JNCIllegalChannelGroupException;

  JNFInvalidPathException = class;
  Arr1JNFInvalidPathException = array of JNFInvalidPathException;
  Arr2JNFInvalidPathException = array of Arr1JNFInvalidPathException;
  Arr3JNFInvalidPathException = array of Arr2JNFInvalidPathException;

  JNFProviderMismatchException = class;
  Arr1JNFProviderMismatchException = array of JNFProviderMismatchException;
  Arr2JNFProviderMismatchException = array of Arr1JNFProviderMismatchException;
  Arr3JNFProviderMismatchException = array of Arr2JNFProviderMismatchException;

  JLRParameter = class;
  Arr1JLRParameter = array of JLRParameter;
  Arr2JLRParameter = array of Arr1JLRParameter;
  Arr3JLRParameter = array of Arr2JLRParameter;

  JLRExecutable = class;
  Arr1JLRExecutable = array of JLRExecutable;
  Arr2JLRExecutable = array of Arr1JLRExecutable;
  Arr3JLRExecutable = array of Arr2JLRExecutable;

  JNFReadOnlyFileSystemException = class;
  Arr1JNFReadOnlyFileSystemException = array of JNFReadOnlyFileSystemException;
  Arr2JNFReadOnlyFileSystemException = array of Arr1JNFReadOnlyFileSystemException;
  Arr3JNFReadOnlyFileSystemException = array of Arr2JNFReadOnlyFileSystemException;

  AMAudioFocusRequest = class;
  Arr1AMAudioFocusRequest = array of AMAudioFocusRequest;
  Arr2AMAudioFocusRequest = array of Arr1AMAudioFocusRequest;
  Arr3AMAudioFocusRequest = array of Arr2AMAudioFocusRequest;

  AANotificationChannel = class;
  Arr1AANotificationChannel = array of AANotificationChannel;
  Arr2AANotificationChannel = array of Arr1AANotificationChannel;
  Arr3AANotificationChannel = array of Arr2AANotificationChannel;

  AANotificationChannelGroup = class;
  Arr1AANotificationChannelGroup = array of AANotificationChannelGroup;
  Arr2AANotificationChannelGroup = array of Arr1AANotificationChannelGroup;
  Arr3AANotificationChannelGroup = array of Arr2AANotificationChannelGroup;

  AAPictureInPictureParams = class;
  Arr1AAPictureInPictureParams = array of AAPictureInPictureParams;
  Arr2AAPictureInPictureParams = array of Arr1AAPictureInPictureParams;
  Arr3AAPictureInPictureParams = array of Arr2AAPictureInPictureParams;

  AARemoteAction = class;
  Arr1AARemoteAction = array of AARemoteAction;
  Arr2AARemoteAction = array of Arr1AARemoteAction;
  Arr3AARemoteAction = array of Arr2AARemoteAction;

  AAWallpaperColors = class;
  Arr1AAWallpaperColors = array of AAWallpaperColors;
  Arr2AAWallpaperColors = array of Arr1AAWallpaperColors;
  Arr3AAWallpaperColors = array of Arr2AAWallpaperColors;

  AAANetworkEvent = class;
  Arr1AAANetworkEvent = array of AAANetworkEvent;
  Arr2AAANetworkEvent = array of Arr1AAANetworkEvent;
  Arr3AAANetworkEvent = array of Arr2AAANetworkEvent;

  AAASystemUpdateInfo = class;
  Arr1AAASystemUpdateInfo = array of AAASystemUpdateInfo;
  Arr2AAASystemUpdateInfo = array of Arr1AAASystemUpdateInfo;
  Arr3AAASystemUpdateInfo = array of Arr2AAASystemUpdateInfo;

  AAJJobWorkItem = class;
  Arr1AAJJobWorkItem = array of AAJJobWorkItem;
  Arr2AAJJobWorkItem = array of Arr1AAJJobWorkItem;
  Arr3AAJJobWorkItem = array of Arr2AAJJobWorkItem;

  AAUExternalStorageStats = class;
  Arr1AAUExternalStorageStats = array of AAUExternalStorageStats;
  Arr2AAUExternalStorageStats = array of Arr1AAUExternalStorageStats;
  Arr3AAUExternalStorageStats = array of Arr2AAUExternalStorageStats;

  AAUStorageStats = class;
  Arr1AAUStorageStats = array of AAUStorageStats;
  Arr2AAUStorageStats = array of Arr1AAUStorageStats;
  Arr3AAUStorageStats = array of Arr2AAUStorageStats;

  ABLAdvertisingSetParameters = class;
  Arr1ABLAdvertisingSetParameters = array of ABLAdvertisingSetParameters;
  Arr2ABLAdvertisingSetParameters = array of Arr1ABLAdvertisingSetParameters;
  Arr3ABLAdvertisingSetParameters = array of Arr2ABLAdvertisingSetParameters;

  ABLPeriodicAdvertisingParameters = class;
  Arr1ABLPeriodicAdvertisingParameters = array of ABLPeriodicAdvertisingParameters;
  Arr2ABLPeriodicAdvertisingParameters = array of Arr1ABLPeriodicAdvertisingParameters;
  Arr3ABLPeriodicAdvertisingParameters = array of Arr2ABLPeriodicAdvertisingParameters;

  ACAssociationRequest = class;
  Arr1ACAssociationRequest = array of ACAssociationRequest;
  Arr2ACAssociationRequest = array of Arr1ACAssociationRequest;
  Arr3ACAssociationRequest = array of Arr2ACAssociationRequest;

  ACDeviceFilter = interface;
  Arr1ACDeviceFilter = array of ACDeviceFilter;
  Arr2ACDeviceFilter = array of Arr1ACDeviceFilter;
  Arr3ACDeviceFilter = array of Arr2ACDeviceFilter;

  ACPChangedPackages = class;
  Arr1ACPChangedPackages = array of ACPChangedPackages;
  Arr2ACPChangedPackages = array of Arr1ACPChangedPackages;
  Arr3ACPChangedPackages = array of Arr2ACPChangedPackages;

  ACPSharedLibraryInfo = class;
  Arr1ACPSharedLibraryInfo = array of ACPSharedLibraryInfo;
  Arr2ACPSharedLibraryInfo = array of Arr1ACPSharedLibraryInfo;
  Arr3ACPSharedLibraryInfo = array of Arr2ACPSharedLibraryInfo;

  ACPVersionedPackage = class;
  Arr1ACPVersionedPackage = array of ACPVersionedPackage;
  Arr2ACPVersionedPackage = array of Arr1ACPVersionedPackage;
  Arr3ACPVersionedPackage = array of Arr2ACPVersionedPackage;

  AMAudioPlaybackConfiguration = class;
  Arr1AMAudioPlaybackConfiguration = array of AMAudioPlaybackConfiguration;
  Arr2AMAudioPlaybackConfiguration = array of Arr1AMAudioPlaybackConfiguration;
  Arr3AMAudioPlaybackConfiguration = array of Arr2AMAudioPlaybackConfiguration;

  ANWACharacteristics = class;
  Arr1ANWACharacteristics = array of ANWACharacteristics;
  Arr2ANWACharacteristics = array of Arr1ANWACharacteristics;
  Arr3ANWACharacteristics = array of Arr2ANWACharacteristics;

  ANWAPublishConfig = class;
  Arr1ANWAPublishConfig = array of ANWAPublishConfig;
  Arr2ANWAPublishConfig = array of Arr1ANWAPublishConfig;
  Arr3ANWAPublishConfig = array of Arr2ANWAPublishConfig;

  ANWASubscribeConfig = class;
  Arr1ANWASubscribeConfig = array of ANWASubscribeConfig;
  Arr2ANWASubscribeConfig = array of Arr1ANWASubscribeConfig;
  Arr3ANWASubscribeConfig = array of Arr2ANWASubscribeConfig;

  ANWHPasspointConfiguration = class;
  Arr1ANWHPasspointConfiguration = array of ANWHPasspointConfiguration;
  Arr2ANWHPasspointConfiguration = array of Arr1ANWHPasspointConfiguration;
  Arr3ANWHPasspointConfiguration = array of Arr2ANWHPasspointConfiguration;

  ANWHPCredential = class;
  Arr1ANWHPCredential = array of ANWHPCredential;
  Arr2ANWHPCredential = array of Arr1ANWHPCredential;
  Arr3ANWHPCredential = array of Arr2ANWHPCredential;

  ANWHPHomeSp = class;
  Arr1ANWHPHomeSp = array of ANWHPHomeSp;
  Arr2ANWHPHomeSp = array of Arr1ANWHPHomeSp;
  Arr3ANWHPHomeSp = array of Arr2ANWHPHomeSp;

  AOVibrationEffect = class;
  Arr1AOVibrationEffect = array of AOVibrationEffect;
  Arr2AOVibrationEffect = array of Arr1AOVibrationEffect;
  Arr3AOVibrationEffect = array of Arr2AOVibrationEffect;

  ASACustomDescription = class;
  Arr1ASACustomDescription = array of ASACustomDescription;
  Arr2ASACustomDescription = array of Arr1ASACustomDescription;
  Arr3ASACustomDescription = array of Arr2ASACustomDescription;

  ASADataset = class;
  Arr1ASADataset = array of ASADataset;
  Arr2ASADataset = array of Arr1ASADataset;
  Arr3ASADataset = array of Arr2ASADataset;

  ASAFillContext = class;
  Arr1ASAFillContext = array of ASAFillContext;
  Arr2ASAFillContext = array of Arr1ASAFillContext;
  Arr3ASAFillContext = array of Arr2ASAFillContext;

  ASAFillEventHistory = class;
  Arr1ASAFillEventHistory = array of ASAFillEventHistory;
  Arr2ASAFillEventHistory = array of Arr1ASAFillEventHistory;
  Arr3ASAFillEventHistory = array of Arr2ASAFillEventHistory;

  ASAFillRequest = class;
  Arr1ASAFillRequest = array of ASAFillRequest;
  Arr2ASAFillRequest = array of Arr1ASAFillRequest;
  Arr3ASAFillRequest = array of Arr2ASAFillRequest;

  ASAFillResponse = class;
  Arr1ASAFillResponse = array of ASAFillResponse;
  Arr2ASAFillResponse = array of Arr1ASAFillResponse;
  Arr3ASAFillResponse = array of Arr2ASAFillResponse;

  ASASaveInfo = class;
  Arr1ASASaveInfo = array of ASASaveInfo;
  Arr2ASASaveInfo = array of Arr1ASASaveInfo;
  Arr3ASASaveInfo = array of Arr2ASASaveInfo;

  ASASaveRequest = class;
  Arr1ASASaveRequest = array of ASASaveRequest;
  Arr2ASASaveRequest = array of Arr1ASASaveRequest;
  Arr3ASASaveRequest = array of Arr2ASASaveRequest;

  ATVisualVoicemailSms = class;
  Arr1ATVisualVoicemailSms = array of ATVisualVoicemailSms;
  Arr2ATVisualVoicemailSms = array of Arr1ATVisualVoicemailSms;
  Arr3ATVisualVoicemailSms = array of Arr2ATVisualVoicemailSms;

  ATVisualVoicemailSmsFilterSettings = class;
  Arr1ATVisualVoicemailSmsFilterSettings = array of ATVisualVoicemailSmsFilterSettings;
  Arr2ATVisualVoicemailSmsFilterSettings = array of Arr1ATVisualVoicemailSmsFilterSettings;
  Arr3ATVisualVoicemailSmsFilterSettings = array of Arr2ATVisualVoicemailSmsFilterSettings;

  AVAAutofillId = class;
  Arr1AVAAutofillId = array of AVAAutofillId;
  Arr2AVAAutofillId = array of Arr1AVAAutofillId;
  Arr3AVAAutofillId = array of Arr2AVAAutofillId;

  AVAAutofillValue = class;
  Arr1AVAAutofillValue = array of AVAAutofillValue;
  Arr2AVAAutofillValue = array of Arr1AVAAutofillValue;
  Arr3AVAAutofillValue = array of Arr2AVAAutofillValue;

  APFontsContract = class;
  Arr1APFontsContract = array of APFontsContract;
  Arr2APFontsContract = array of Arr1APFontsContract;
  Arr3APFontsContract = array of Arr2APFontsContract;

  ASACharSequenceTransformation = class;
  Arr1ASACharSequenceTransformation = array of ASACharSequenceTransformation;
  Arr2ASACharSequenceTransformation = array of Arr1ASACharSequenceTransformation;
  Arr3ASACharSequenceTransformation = array of Arr2ASACharSequenceTransformation;

  ASAImageTransformation = class;
  Arr1ASAImageTransformation = array of ASAImageTransformation;
  Arr2ASAImageTransformation = array of Arr1ASAImageTransformation;
  Arr3ASAImageTransformation = array of Arr2ASAImageTransformation;

  ASALuhnChecksumValidator = class;
  Arr1ASALuhnChecksumValidator = array of ASALuhnChecksumValidator;
  Arr2ASALuhnChecksumValidator = array of Arr1ASALuhnChecksumValidator;
  Arr3ASALuhnChecksumValidator = array of Arr2ASALuhnChecksumValidator;

  ASARegexValidator = class;
  Arr1ASARegexValidator = array of ASARegexValidator;
  Arr2ASARegexValidator = array of Arr1ASARegexValidator;
  Arr3ASARegexValidator = array of Arr2ASARegexValidator;

  AHHardwareBuffer = class;
  Arr1AHHardwareBuffer = array of AHHardwareBuffer;
  Arr2AHHardwareBuffer = array of Arr1AHHardwareBuffer;
  Arr3AHHardwareBuffer = array of Arr2AHHardwareBuffer;

  AMMediaCas = class;
  Arr1AMMediaCas = array of AMMediaCas;
  Arr2AMMediaCas = array of Arr1AMMediaCas;
  Arr3AMMediaCas = array of Arr2AMMediaCas;

  AMVolumeShaper = class;
  Arr1AMVolumeShaper = array of AMVolumeShaper;
  Arr2AMVolumeShaper = array of Arr1AMVolumeShaper;
  Arr3AMVolumeShaper = array of Arr2AMVolumeShaper;

  ANWADiscoverySession = class;
  Arr1ANWADiscoverySession = array of ANWADiscoverySession;
  Arr2ANWADiscoverySession = array of Arr1ANWADiscoverySession;
  Arr3ANWADiscoverySession = array of Arr2ANWADiscoverySession;

  ANWAWifiAwareSession = class;
  Arr1ANWAWifiAwareSession = array of ANWAWifiAwareSession;
  Arr2ANWAWifiAwareSession = array of Arr1ANWAWifiAwareSession;
  Arr3ANWAWifiAwareSession = array of Arr2ANWAWifiAwareSession;

  JLANative = interface;
  Arr1JLANative = array of JLANative;
  Arr2JLANative = array of Arr1JLANative;
  Arr3JLANative = array of Arr2JLANative;

  JLIConstantCallSite = class;
  Arr1JLIConstantCallSite = array of JLIConstantCallSite;
  Arr2JLIConstantCallSite = array of Arr1JLIConstantCallSite;
  Arr3JLIConstantCallSite = array of Arr2JLIConstantCallSite;

  JLIMutableCallSite = class;
  Arr1JLIMutableCallSite = array of JLIMutableCallSite;
  Arr2JLIMutableCallSite = array of Arr1JLIMutableCallSite;
  Arr3JLIMutableCallSite = array of Arr2JLIMutableCallSite;

  JLIVolatileCallSite = class;
  Arr1JLIVolatileCallSite = array of JLIVolatileCallSite;
  Arr2JLIVolatileCallSite = array of Arr1JLIVolatileCallSite;
  Arr3JLIVolatileCallSite = array of Arr2JLIVolatileCallSite;

  JLIMethodHandleInfo = interface;
  Arr1JLIMethodHandleInfo = array of JLIMethodHandleInfo;
  Arr2JLIMethodHandleInfo = array of Arr1JLIMethodHandleInfo;
  Arr3JLIMethodHandleInfo = array of Arr2JLIMethodHandleInfo;

  JNFStandardCopyOption = class;
  Arr1JNFStandardCopyOption = array of JNFStandardCopyOption;
  Arr2JNFStandardCopyOption = array of Arr1JNFStandardCopyOption;
  Arr3JNFStandardCopyOption = array of Arr2JNFStandardCopyOption;

  JNFSimpleFileVisitor = class;
  Arr1JNFSimpleFileVisitor = array of JNFSimpleFileVisitor;
  Arr2JNFSimpleFileVisitor = array of Arr1JNFSimpleFileVisitor;
  Arr3JNFSimpleFileVisitor = array of Arr2JNFSimpleFileVisitor;

  JNFLinkOption = class;
  Arr1JNFLinkOption = array of JNFLinkOption;
  Arr2JNFLinkOption = array of Arr1JNFLinkOption;
  Arr3JNFLinkOption = array of Arr2JNFLinkOption;

  JNFStandardOpenOption = class;
  Arr1JNFStandardOpenOption = array of JNFStandardOpenOption;
  Arr2JNFStandardOpenOption = array of Arr1JNFStandardOpenOption;
  Arr3JNFStandardOpenOption = array of Arr2JNFStandardOpenOption;

  JNFStandardWatchEventKinds = class;
  Arr1JNFStandardWatchEventKinds = array of JNFStandardWatchEventKinds;
  Arr2JNFStandardWatchEventKinds = array of Arr1JNFStandardWatchEventKinds;
  Arr3JNFStandardWatchEventKinds = array of Arr2JNFStandardWatchEventKinds;

  JNFWatchable = interface;
  Arr1JNFWatchable = array of JNFWatchable;
  Arr2JNFWatchable = array of Arr1JNFWatchable;
  Arr3JNFWatchable = array of Arr2JNFWatchable;

  JNFAFileAttributeView = interface;
  Arr1JNFAFileAttributeView = array of JNFAFileAttributeView;
  Arr2JNFAFileAttributeView = array of Arr1JNFAFileAttributeView;
  Arr3JNFAFileAttributeView = array of Arr2JNFAFileAttributeView;

  JNFAFileStoreAttributeView = interface;
  Arr1JNFAFileStoreAttributeView = array of JNFAFileStoreAttributeView;
  Arr2JNFAFileStoreAttributeView = array of Arr1JNFAFileStoreAttributeView;
  Arr3JNFAFileStoreAttributeView = array of Arr2JNFAFileStoreAttributeView;

  JNFADosFileAttributes = interface;
  Arr1JNFADosFileAttributes = array of JNFADosFileAttributes;
  Arr2JNFADosFileAttributes = array of Arr1JNFADosFileAttributes;
  Arr3JNFADosFileAttributes = array of Arr2JNFADosFileAttributes;

  JNFAPosixFileAttributes = interface;
  Arr1JNFAPosixFileAttributes = array of JNFAPosixFileAttributes;
  Arr2JNFAPosixFileAttributes = array of Arr1JNFAPosixFileAttributes;
  Arr3JNFAPosixFileAttributes = array of Arr2JNFAPosixFileAttributes;

  JNFAUserPrincipal = interface;
  Arr1JNFAUserPrincipal = array of JNFAUserPrincipal;
  Arr2JNFAUserPrincipal = array of Arr1JNFAUserPrincipal;
  Arr3JNFAUserPrincipal = array of Arr2JNFAUserPrincipal;

  JTTTemporal = interface;
  Arr1JTTTemporal = array of JTTTemporal;
  Arr2JTTTemporal = array of Arr1JTTTemporal;
  Arr3JTTTemporal = array of Arr2JTTTemporal;

  JTDayOfWeek = class;
  Arr1JTDayOfWeek = array of JTDayOfWeek;
  Arr2JTDayOfWeek = array of Arr1JTDayOfWeek;
  Arr3JTDayOfWeek = array of Arr2JTDayOfWeek;

  JTMonth = class;
  Arr1JTMonth = array of JTMonth;
  Arr2JTMonth = array of Arr1JTMonth;
  Arr3JTMonth = array of Arr2JTMonth;

  JTMonthDay = class;
  Arr1JTMonthDay = array of JTMonthDay;
  Arr2JTMonthDay = array of Arr1JTMonthDay;
  Arr3JTMonthDay = array of Arr2JTMonthDay;

  JTCEra = interface;
  Arr1JTCEra = array of JTCEra;
  Arr2JTCEra = array of Arr1JTCEra;
  Arr3JTCEra = array of Arr2JTCEra;

  JTDuration = class;
  Arr1JTDuration = array of JTDuration;
  Arr2JTDuration = array of Arr1JTDuration;
  Arr3JTDuration = array of Arr2JTDuration;

  JTCChronoPeriod = interface;
  Arr1JTCChronoPeriod = array of JTCChronoPeriod;
  Arr2JTCChronoPeriod = array of Arr1JTCChronoPeriod;
  Arr3JTCChronoPeriod = array of Arr2JTCChronoPeriod;

  JTTChronoField = class;
  Arr1JTTChronoField = array of JTTChronoField;
  Arr2JTTChronoField = array of Arr1JTTChronoField;
  Arr3JTTChronoField = array of Arr2JTTChronoField;

  JTTChronoUnit = class;
  Arr1JTTChronoUnit = array of JTTChronoUnit;
  Arr2JTTChronoUnit = array of Arr1JTTChronoUnit;
  Arr3JTTChronoUnit = array of Arr2JTTChronoUnit;

  ATTransitionListenerAdapter = class;
  Arr1ATTransitionListenerAdapter = array of ATTransitionListenerAdapter;
  Arr2ATTransitionListenerAdapter = array of Arr1ATTransitionListenerAdapter;
  Arr3ATTransitionListenerAdapter = array of Arr2ATTransitionListenerAdapter;

  AAAuthenticationRequiredException = class;
  Arr1AAAuthenticationRequiredException = array of AAAuthenticationRequiredException;
  Arr2AAAuthenticationRequiredException = array of Arr1AAAuthenticationRequiredException;
  Arr3AAAuthenticationRequiredException = array of Arr2AAAuthenticationRequiredException;

  JTFDateTimeParseException = class;
  Arr1JTFDateTimeParseException = array of JTFDateTimeParseException;
  Arr2JTFDateTimeParseException = array of Arr1JTFDateTimeParseException;
  Arr3JTFDateTimeParseException = array of Arr2JTFDateTimeParseException;

  JTTUnsupportedTemporalTypeException = class;
  Arr1JTTUnsupportedTemporalTypeException = array of JTTUnsupportedTemporalTypeException;
  Arr2JTTUnsupportedTemporalTypeException = array of Arr1JTTUnsupportedTemporalTypeException;
  Arr3JTTUnsupportedTemporalTypeException = array of Arr2JTTUnsupportedTemporalTypeException;

  JTZZoneRulesException = class;
  Arr1JTZZoneRulesException = array of JTZZoneRulesException;
  Arr2JTZZoneRulesException = array of Arr1JTZZoneRulesException;
  Arr3JTZZoneRulesException = array of Arr2JTZZoneRulesException;

  JNFDirectoryIteratorException = class;
  Arr1JNFDirectoryIteratorException = array of JNFDirectoryIteratorException;
  Arr2JNFDirectoryIteratorException = array of Arr1JNFDirectoryIteratorException;
  Arr3JNFDirectoryIteratorException = array of Arr2JNFDirectoryIteratorException;

  JTCAbstractChronology = class;
  Arr1JTCAbstractChronology = array of JTCAbstractChronology;
  Arr2JTCAbstractChronology = array of Arr1JTCAbstractChronology;
  Arr3JTCAbstractChronology = array of Arr2JTCAbstractChronology;

  JTZoneOffset = class;
  Arr1JTZoneOffset = array of JTZoneOffset;
  Arr2JTZoneOffset = array of Arr1JTZoneOffset;
  Arr3JTZoneOffset = array of Arr2JTZoneOffset;

  JNFAccessDeniedException = class;
  Arr1JNFAccessDeniedException = array of JNFAccessDeniedException;
  Arr2JNFAccessDeniedException = array of Arr1JNFAccessDeniedException;
  Arr3JNFAccessDeniedException = array of Arr2JNFAccessDeniedException;

  JNFAtomicMoveNotSupportedException = class;
  Arr1JNFAtomicMoveNotSupportedException = array of JNFAtomicMoveNotSupportedException;
  Arr2JNFAtomicMoveNotSupportedException = array of Arr1JNFAtomicMoveNotSupportedException;
  Arr3JNFAtomicMoveNotSupportedException = array of Arr2JNFAtomicMoveNotSupportedException;

  JNFDirectoryNotEmptyException = class;
  Arr1JNFDirectoryNotEmptyException = array of JNFDirectoryNotEmptyException;
  Arr2JNFDirectoryNotEmptyException = array of Arr1JNFDirectoryNotEmptyException;
  Arr3JNFDirectoryNotEmptyException = array of Arr2JNFDirectoryNotEmptyException;

  JNFFileAlreadyExistsException = class;
  Arr1JNFFileAlreadyExistsException = array of JNFFileAlreadyExistsException;
  Arr2JNFFileAlreadyExistsException = array of Arr1JNFFileAlreadyExistsException;
  Arr3JNFFileAlreadyExistsException = array of Arr2JNFFileAlreadyExistsException;

  JNFFileSystemLoopException = class;
  Arr1JNFFileSystemLoopException = array of JNFFileSystemLoopException;
  Arr2JNFFileSystemLoopException = array of Arr1JNFFileSystemLoopException;
  Arr3JNFFileSystemLoopException = array of Arr2JNFFileSystemLoopException;

  JNFNoSuchFileException = class;
  Arr1JNFNoSuchFileException = array of JNFNoSuchFileException;
  Arr2JNFNoSuchFileException = array of Arr1JNFNoSuchFileException;
  Arr3JNFNoSuchFileException = array of Arr2JNFNoSuchFileException;

  JNFNotDirectoryException = class;
  Arr1JNFNotDirectoryException = array of JNFNotDirectoryException;
  Arr2JNFNotDirectoryException = array of Arr1JNFNotDirectoryException;
  Arr3JNFNotDirectoryException = array of Arr2JNFNotDirectoryException;

  JNFNotLinkException = class;
  Arr1JNFNotLinkException = array of JNFNotLinkException;
  Arr2JNFNotLinkException = array of Arr1JNFNotLinkException;
  Arr3JNFNotLinkException = array of Arr2JNFNotLinkException;

  AAAConnectEvent = class;
  Arr1AAAConnectEvent = array of AAAConnectEvent;
  Arr2AAAConnectEvent = array of Arr1AAAConnectEvent;
  Arr3AAAConnectEvent = array of Arr2AAAConnectEvent;

  AAADnsEvent = class;
  Arr1AAADnsEvent = array of AAADnsEvent;
  Arr2AAADnsEvent = array of Arr1AAADnsEvent;
  Arr3AAADnsEvent = array of Arr2AAADnsEvent;

  ACBluetoothDeviceFilter = class;
  Arr1ACBluetoothDeviceFilter = array of ACBluetoothDeviceFilter;
  Arr2ACBluetoothDeviceFilter = array of Arr1ACBluetoothDeviceFilter;
  Arr3ACBluetoothDeviceFilter = array of Arr2ACBluetoothDeviceFilter;

  ACBluetoothLeDeviceFilter = class;
  Arr1ACBluetoothLeDeviceFilter = array of ACBluetoothLeDeviceFilter;
  Arr2ACBluetoothLeDeviceFilter = array of Arr1ACBluetoothLeDeviceFilter;
  Arr3ACBluetoothLeDeviceFilter = array of Arr2ACBluetoothLeDeviceFilter;

  ACWifiDeviceFilter = class;
  Arr1ACWifiDeviceFilter = array of ACWifiDeviceFilter;
  Arr2ACWifiDeviceFilter = array of Arr1ACWifiDeviceFilter;
  Arr3ACWifiDeviceFilter = array of Arr2ACWifiDeviceFilter;

  AMMediaDescrambler = class;
  Arr1AMMediaDescrambler = array of AMMediaDescrambler;
  Arr2AMMediaDescrambler = array of Arr1AMMediaDescrambler;
  Arr3AMMediaDescrambler = array of Arr2AMMediaDescrambler;

  AMVolumeAutomation = interface;
  Arr1AMVolumeAutomation = array of AMVolumeAutomation;
  Arr2AMVolumeAutomation = array of Arr1AMVolumeAutomation;
  Arr3AMVolumeAutomation = array of Arr2AMVolumeAutomation;

  ANWAPublishDiscoverySession = class;
  Arr1ANWAPublishDiscoverySession = array of ANWAPublishDiscoverySession;
  Arr2ANWAPublishDiscoverySession = array of Arr1ANWAPublishDiscoverySession;
  Arr3ANWAPublishDiscoverySession = array of Arr2ANWAPublishDiscoverySession;

  ANWASubscribeDiscoverySession = class;
  Arr1ANWASubscribeDiscoverySession = array of ANWASubscribeDiscoverySession;
  Arr2ANWASubscribeDiscoverySession = array of Arr1ANWASubscribeDiscoverySession;
  Arr3ANWASubscribeDiscoverySession = array of Arr2ANWASubscribeDiscoverySession;

  AOSharedMemory = class;
  Arr1AOSharedMemory = array of AOSharedMemory;
  Arr2AOSharedMemory = array of Arr1AOSharedMemory;
  Arr3AOSharedMemory = array of Arr2AOSharedMemory;

  JNFDirectoryStream = interface;
  Arr1JNFDirectoryStream = array of JNFDirectoryStream;
  Arr2JNFDirectoryStream = array of Arr1JNFDirectoryStream;
  Arr3JNFDirectoryStream = array of Arr2JNFDirectoryStream;

  JNFFileSystem = class;
  Arr1JNFFileSystem = array of JNFFileSystem;
  Arr2JNFFileSystem = array of Arr1JNFFileSystem;
  Arr3JNFFileSystem = array of Arr2JNFFileSystem;

  JNFWatchService = interface;
  Arr1JNFWatchService = array of JNFWatchService;
  Arr2JNFWatchService = array of Arr1JNFWatchService;
  Arr3JNFWatchService = array of Arr2JNFWatchService;

  JNFPath = interface;
  Arr1JNFPath = array of JNFPath;
  Arr2JNFPath = array of Arr1JNFPath;
  Arr3JNFPath = array of Arr2JNFPath;

  JNFABasicFileAttributeView = interface;
  Arr1JNFABasicFileAttributeView = array of JNFABasicFileAttributeView;
  Arr2JNFABasicFileAttributeView = array of Arr1JNFABasicFileAttributeView;
  Arr3JNFABasicFileAttributeView = array of Arr2JNFABasicFileAttributeView;

  JNFAFileOwnerAttributeView = interface;
  Arr1JNFAFileOwnerAttributeView = array of JNFAFileOwnerAttributeView;
  Arr2JNFAFileOwnerAttributeView = array of Arr1JNFAFileOwnerAttributeView;
  Arr3JNFAFileOwnerAttributeView = array of Arr2JNFAFileOwnerAttributeView;

  JNFAUserDefinedFileAttributeView = interface;
  Arr1JNFAUserDefinedFileAttributeView = array of JNFAUserDefinedFileAttributeView;
  Arr2JNFAUserDefinedFileAttributeView = array of Arr1JNFAUserDefinedFileAttributeView;
  Arr3JNFAUserDefinedFileAttributeView = array of Arr2JNFAUserDefinedFileAttributeView;

  JNFAGroupPrincipal = interface;
  Arr1JNFAGroupPrincipal = array of JNFAGroupPrincipal;
  Arr2JNFAGroupPrincipal = array of Arr1JNFAGroupPrincipal;
  Arr3JNFAGroupPrincipal = array of Arr2JNFAGroupPrincipal;

  JTInstant = class;
  Arr1JTInstant = array of JTInstant;
  Arr2JTInstant = array of Arr1JTInstant;
  Arr3JTInstant = array of Arr2JTInstant;

  JTLocalTime = class;
  Arr1JTLocalTime = array of JTLocalTime;
  Arr2JTLocalTime = array of Arr1JTLocalTime;
  Arr3JTLocalTime = array of Arr2JTLocalTime;

  JTOffsetDateTime = class;
  Arr1JTOffsetDateTime = array of JTOffsetDateTime;
  Arr2JTOffsetDateTime = array of Arr1JTOffsetDateTime;
  Arr3JTOffsetDateTime = array of Arr2JTOffsetDateTime;

  JTOffsetTime = class;
  Arr1JTOffsetTime = array of JTOffsetTime;
  Arr2JTOffsetTime = array of Arr1JTOffsetTime;
  Arr3JTOffsetTime = array of Arr2JTOffsetTime;

  JTYear = class;
  Arr1JTYear = array of JTYear;
  Arr2JTYear = array of Arr1JTYear;
  Arr3JTYear = array of Arr2JTYear;

  JTYearMonth = class;
  Arr1JTYearMonth = array of JTYearMonth;
  Arr2JTYearMonth = array of Arr1JTYearMonth;
  Arr3JTYearMonth = array of Arr2JTYearMonth;

  JTCChronoLocalDate = interface;
  Arr1JTCChronoLocalDate = array of JTCChronoLocalDate;
  Arr2JTCChronoLocalDate = array of Arr1JTCChronoLocalDate;
  Arr3JTCChronoLocalDate = array of Arr2JTCChronoLocalDate;

  JTCChronoLocalDateTime = interface;
  Arr1JTCChronoLocalDateTime = array of JTCChronoLocalDateTime;
  Arr2JTCChronoLocalDateTime = array of Arr1JTCChronoLocalDateTime;
  Arr3JTCChronoLocalDateTime = array of Arr2JTCChronoLocalDateTime;

  JTCChronoZonedDateTime = interface;
  Arr1JTCChronoZonedDateTime = array of JTCChronoZonedDateTime;
  Arr2JTCChronoZonedDateTime = array of Arr1JTCChronoZonedDateTime;
  Arr3JTCChronoZonedDateTime = array of Arr2JTCChronoZonedDateTime;

  JTCHijrahEra = class;
  Arr1JTCHijrahEra = array of JTCHijrahEra;
  Arr2JTCHijrahEra = array of Arr1JTCHijrahEra;
  Arr3JTCHijrahEra = array of Arr2JTCHijrahEra;

  JTCIsoEra = class;
  Arr1JTCIsoEra = array of JTCIsoEra;
  Arr2JTCIsoEra = array of Arr1JTCIsoEra;
  Arr3JTCIsoEra = array of Arr2JTCIsoEra;

  JTCJapaneseEra = class;
  Arr1JTCJapaneseEra = array of JTCJapaneseEra;
  Arr2JTCJapaneseEra = array of Arr1JTCJapaneseEra;
  Arr3JTCJapaneseEra = array of Arr2JTCJapaneseEra;

  JTCMinguoEra = class;
  Arr1JTCMinguoEra = array of JTCMinguoEra;
  Arr2JTCMinguoEra = array of Arr1JTCMinguoEra;
  Arr3JTCMinguoEra = array of Arr2JTCMinguoEra;

  JTCThaiBuddhistEra = class;
  Arr1JTCThaiBuddhistEra = array of JTCThaiBuddhistEra;
  Arr2JTCThaiBuddhistEra = array of Arr1JTCThaiBuddhistEra;
  Arr3JTCThaiBuddhistEra = array of Arr2JTCThaiBuddhistEra;

  JTPeriod = class;
  Arr1JTPeriod = array of JTPeriod;
  Arr2JTPeriod = array of Arr1JTPeriod;
  Arr3JTPeriod = array of Arr2JTPeriod;

  JSDomainLoadStoreParameter = class;
  Arr1JSDomainLoadStoreParameter = array of JSDomainLoadStoreParameter;
  Arr2JSDomainLoadStoreParameter = array of Arr1JSDomainLoadStoreParameter;
  Arr3JSDomainLoadStoreParameter = array of Arr2JSDomainLoadStoreParameter;

  JSPKCS12Attribute = class;
  Arr1JSPKCS12Attribute = array of JSPKCS12Attribute;
  Arr2JSPKCS12Attribute = array of Arr1JSPKCS12Attribute;
  Arr3JSPKCS12Attribute = array of Arr2JSPKCS12Attribute;

  JTCHijrahChronology = class;
  Arr1JTCHijrahChronology = array of JTCHijrahChronology;
  Arr2JTCHijrahChronology = array of Arr1JTCHijrahChronology;
  Arr3JTCHijrahChronology = array of Arr2JTCHijrahChronology;

  JTCIsoChronology = class;
  Arr1JTCIsoChronology = array of JTCIsoChronology;
  Arr2JTCIsoChronology = array of Arr1JTCIsoChronology;
  Arr3JTCIsoChronology = array of Arr2JTCIsoChronology;

  JTCJapaneseChronology = class;
  Arr1JTCJapaneseChronology = array of JTCJapaneseChronology;
  Arr2JTCJapaneseChronology = array of Arr1JTCJapaneseChronology;
  Arr3JTCJapaneseChronology = array of Arr2JTCJapaneseChronology;

  JTCMinguoChronology = class;
  Arr1JTCMinguoChronology = array of JTCMinguoChronology;
  Arr2JTCMinguoChronology = array of Arr1JTCMinguoChronology;
  Arr3JTCMinguoChronology = array of Arr2JTCMinguoChronology;

  JTCThaiBuddhistChronology = class;
  Arr1JTCThaiBuddhistChronology = array of JTCThaiBuddhistChronology;
  Arr2JTCThaiBuddhistChronology = array of Arr1JTCThaiBuddhistChronology;
  Arr3JTCThaiBuddhistChronology = array of Arr2JTCThaiBuddhistChronology;

  AIUEthiopicCalendar = class;
  Arr1AIUEthiopicCalendar = array of AIUEthiopicCalendar;
  Arr2AIUEthiopicCalendar = array of Arr1AIUEthiopicCalendar;
  Arr3AIUEthiopicCalendar = array of Arr2AIUEthiopicCalendar;

  AHSensorDirectChannel = class;
  Arr1AHSensorDirectChannel = array of AHSensorDirectChannel;
  Arr2AHSensorDirectChannel = array of Arr1AHSensorDirectChannel;
  Arr3AHSensorDirectChannel = array of Arr2AHSensorDirectChannel;

  JNCAsynchronousChannel = interface;
  Arr1JNCAsynchronousChannel = array of JNCAsynchronousChannel;
  Arr2JNCAsynchronousChannel = array of Arr1JNCAsynchronousChannel;
  Arr3JNCAsynchronousChannel = array of Arr2JNCAsynchronousChannel;

  JNFFiles = class;
  Arr1JNFFiles = array of JNFFiles;
  Arr2JNFFiles = array of Arr1JNFFiles;
  Arr3JNFFiles = array of Arr2JNFFiles;

  JNFSecureDirectoryStream = interface;
  Arr1JNFSecureDirectoryStream = array of JNFSecureDirectoryStream;
  Arr2JNFSecureDirectoryStream = array of Arr1JNFSecureDirectoryStream;
  Arr3JNFSecureDirectoryStream = array of Arr2JNFSecureDirectoryStream;

  JNFSFileSystemProvider = class;
  Arr1JNFSFileSystemProvider = array of JNFSFileSystemProvider;
  Arr2JNFSFileSystemProvider = array of Arr1JNFSFileSystemProvider;
  Arr3JNFSFileSystemProvider = array of Arr2JNFSFileSystemProvider;

  JNFADosFileAttributeView = interface;
  Arr1JNFADosFileAttributeView = array of JNFADosFileAttributeView;
  Arr2JNFADosFileAttributeView = array of Arr1JNFADosFileAttributeView;
  Arr3JNFADosFileAttributeView = array of Arr2JNFADosFileAttributeView;

  JNFAAclFileAttributeView = interface;
  Arr1JNFAAclFileAttributeView = array of JNFAAclFileAttributeView;
  Arr2JNFAAclFileAttributeView = array of Arr1JNFAAclFileAttributeView;
  Arr3JNFAAclFileAttributeView = array of Arr2JNFAAclFileAttributeView;

  JNFAPosixFileAttributeView = interface;
  Arr1JNFAPosixFileAttributeView = array of JNFAPosixFileAttributeView;
  Arr2JNFAPosixFileAttributeView = array of Arr1JNFAPosixFileAttributeView;
  Arr3JNFAPosixFileAttributeView = array of Arr2JNFAPosixFileAttributeView;

  JNFLinkPermission = class;
  Arr1JNFLinkPermission = array of JNFLinkPermission;
  Arr2JNFLinkPermission = array of Arr1JNFLinkPermission;
  Arr3JNFLinkPermission = array of Arr2JNFLinkPermission;

  JTLocalDate = class;
  Arr1JTLocalDate = array of JTLocalDate;
  Arr2JTLocalDate = array of Arr1JTLocalDate;
  Arr3JTLocalDate = array of Arr2JTLocalDate;

  JTCChronoLocalDateImpl = class;
  Arr1JTCChronoLocalDateImpl = array of JTCChronoLocalDateImpl;
  Arr2JTCChronoLocalDateImpl = array of Arr1JTCChronoLocalDateImpl;
  Arr3JTCChronoLocalDateImpl = array of Arr2JTCChronoLocalDateImpl;

  JTLocalDateTime = class;
  Arr1JTLocalDateTime = array of JTLocalDateTime;
  Arr2JTLocalDateTime = array of Arr1JTLocalDateTime;
  Arr3JTLocalDateTime = array of Arr2JTLocalDateTime;

  JTZonedDateTime = class;
  Arr1JTZonedDateTime = array of JTZonedDateTime;
  Arr2JTZonedDateTime = array of Arr1JTZonedDateTime;
  Arr3JTZonedDateTime = array of Arr2JTZonedDateTime;

  AGDAdaptiveIconDrawable = class;
  Arr1AGDAdaptiveIconDrawable = array of AGDAdaptiveIconDrawable;
  Arr2AGDAdaptiveIconDrawable = array of Arr1AGDAdaptiveIconDrawable;
  Arr3AGDAdaptiveIconDrawable = array of Arr2AGDAdaptiveIconDrawable;

  JNCAsynchronousByteChannel = interface;
  Arr1JNCAsynchronousByteChannel = array of JNCAsynchronousByteChannel;
  Arr2JNCAsynchronousByteChannel = array of Arr1JNCAsynchronousByteChannel;
  Arr3JNCAsynchronousByteChannel = array of Arr2JNCAsynchronousByteChannel;

  JNCAsynchronousFileChannel = class;
  Arr1JNCAsynchronousFileChannel = array of JNCAsynchronousFileChannel;
  Arr2JNCAsynchronousFileChannel = array of Arr1JNCAsynchronousFileChannel;
  Arr3JNCAsynchronousFileChannel = array of Arr2JNCAsynchronousFileChannel;

  JNCAsynchronousServerSocketChannel = class;
  Arr1JNCAsynchronousServerSocketChannel = array of JNCAsynchronousServerSocketChannel;
  Arr2JNCAsynchronousServerSocketChannel = array of Arr1JNCAsynchronousServerSocketChannel;
  Arr3JNCAsynchronousServerSocketChannel = array of Arr2JNCAsynchronousServerSocketChannel;

  JNCMulticastChannel = interface;
  Arr1JNCMulticastChannel = array of JNCMulticastChannel;
  Arr2JNCMulticastChannel = array of Arr1JNCMulticastChannel;
  Arr3JNCMulticastChannel = array of Arr2JNCMulticastChannel;

  JTCHijrahDate = class;
  Arr1JTCHijrahDate = array of JTCHijrahDate;
  Arr2JTCHijrahDate = array of Arr1JTCHijrahDate;
  Arr3JTCHijrahDate = array of Arr2JTCHijrahDate;

  JTCJapaneseDate = class;
  Arr1JTCJapaneseDate = array of JTCJapaneseDate;
  Arr2JTCJapaneseDate = array of Arr1JTCJapaneseDate;
  Arr3JTCJapaneseDate = array of Arr2JTCJapaneseDate;

  JTCMinguoDate = class;
  Arr1JTCMinguoDate = array of JTCMinguoDate;
  Arr2JTCMinguoDate = array of Arr1JTCMinguoDate;
  Arr3JTCMinguoDate = array of Arr2JTCMinguoDate;

  JTCThaiBuddhistDate = class;
  Arr1JTCThaiBuddhistDate = array of JTCThaiBuddhistDate;
  Arr2JTCThaiBuddhistDate = array of Arr1JTCThaiBuddhistDate;
  Arr3JTCThaiBuddhistDate = array of Arr2JTCThaiBuddhistDate;

  JNCAsynchronousSocketChannel = class;
  Arr1JNCAsynchronousSocketChannel = array of JNCAsynchronousSocketChannel;
  Arr2JNCAsynchronousSocketChannel = array of Arr1JNCAsynchronousSocketChannel;
  Arr3JNCAsynchronousSocketChannel = array of Arr2JNCAsynchronousSocketChannel;

  AAADeviceAdminService = class;
  Arr1AAADeviceAdminService = array of AAADeviceAdminService;
  Arr2AAADeviceAdminService = array of Arr1AAADeviceAdminService;
  Arr3AAADeviceAdminService = array of Arr2AAADeviceAdminService;

  ASAAutofillService = class;
  Arr1ASAAutofillService = array of ASAAutofillService;
  Arr2ASAAutofillService = array of Arr1ASAAutofillService;
  Arr3ASAAutofillService = array of Arr2ASAAutofillService;

  ATVisualVoicemailService = class;
  Arr1ATVisualVoicemailService = array of ATVisualVoicemailService;
  Arr2ATVisualVoicemailService = array of Arr1ATVisualVoicemailService;
  Arr3ATVisualVoicemailService = array of Arr2ATVisualVoicemailService;

  AVTTextClassification = class;
  Arr1AVTTextClassification = array of AVTTextClassification;
  Arr2AVTTextClassification = array of Arr1AVTTextClassification;
  Arr3AVTTextClassification = array of Arr2AVTTextClassification;

  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate';
  Arr1CAIUPredicate = array of CAIUPredicate;
  Arr2CAIUPredicate = array of Arr1CAIUPredicate;
  Arr3CAIUPredicate = array of Arr2CAIUPredicate;

{$include androidr27.inc}

implementation

end.
