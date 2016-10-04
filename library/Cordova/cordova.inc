  OACLLog = interface external 'org.apache.commons.logging' name 'Log' 
    function isDebugEnabled(): jboolean; overload;
    function isErrorEnabled(): jboolean; overload;
    function isFatalEnabled(): jboolean; overload;
    function isInfoEnabled(): jboolean; overload;
    function isTraceEnabled(): jboolean; overload;
    function isWarnEnabled(): jboolean; overload;
    procedure trace(para1: JLObject); overload;
    procedure trace(para1: JLObject; para2: JLThrowable); overload;
    procedure debug(para1: JLObject); overload;
    procedure debug(para1: JLObject; para2: JLThrowable); overload;
    procedure info(para1: JLObject); overload;
    procedure info(para1: JLObject; para2: JLThrowable); overload;
    procedure warn(para1: JLObject); overload;
    procedure warn(para1: JLObject; para2: JLThrowable); overload;
    procedure error(para1: JLObject); overload;
    procedure error(para1: JLObject; para2: JLThrowable); overload;
    procedure fatal(para1: JLObject); overload;
    procedure fatal(para1: JLObject; para2: JLThrowable); overload;
  end;

  OACAuthenticationToken = class external 'org.apache.cordova' name 'AuthenticationToken' (JLObject)
  public
    constructor create(); overload;
    function getUserName(): JLString; overload; virtual;
    procedure setUserName(para1: JLString); overload; virtual;
    function getPassword(): JLString; overload; virtual;
    procedure setPassword(para1: JLString); overload; virtual;
  end;

  OACBuildConfig = class sealed external 'org.apache.cordova' name 'BuildConfig' (JLObject)
  public
    const
      DEBUG = 0;
  public
    constructor create(); overload;
  end;

  OACConfig = class external 'org.apache.cordova' name 'Config' (JLObject)
  public
    const
      TAG = 'Config';
  public
    class procedure init(para1: AAActivity); static; overload;
    class procedure init(); static; overload;
    class procedure addWhiteListEntry(para1: JLString; para2: jboolean); static; overload;
    class function isUrlWhiteListed(para1: JLString): jboolean; static; overload;
    class function getStartUrl(): JLString; static; overload;
  end;

  OACContactAccessor = class abstract external 'org.apache.cordova' name 'ContactAccessor' (JLObject)
  public
    type
      InnerWhereOptions = class;
      Arr1InnerWhereOptions = array of InnerWhereOptions;
      Arr2InnerWhereOptions = array of Arr1InnerWhereOptions;
      Arr3InnerWhereOptions = array of Arr2InnerWhereOptions;
      InnerWhereOptions = class external 'org.apache.cordova' name 'WhereOptions' 
      end;

  strict protected
    const
      LOG_TAG = 'ContactsAccessor';
  strict protected
    var
      fmApp: OACACordovaInterface; external name 'mApp';
      fmView: AWWebView; external name 'mView';
  public
    constructor create(); overload;
  strict protected
    function isRequired(para1: JLString; para2: JUHashMap): jboolean; overload; virtual;
    function buildPopulationSet(para1: OJJSONArray): JUHashMap; overload; virtual;
    function getJsonString(para1: OJJSONObject; para2: JLString): JLString; overload; virtual;
  public
    function save(para1: OJJSONObject): JLString; overload; virtual; abstract;
    function search(para1: OJJSONArray; para2: OJJSONObject): OJJSONArray; overload; virtual; abstract;
    function getContactById(para1: JLString): OJJSONObject; overload; virtual; abstract;  // throws org.json.JSONException
    function remove(para1: JLString): jboolean; overload; virtual; abstract;
  end;

  OACCordovaArgs = class external 'org.apache.cordova' name 'CordovaArgs' (JLObject)
  public
    constructor create(para1: OJJSONArray); overload;
    function get(para1: jint): JLObject; overload; virtual;  // throws org.json.JSONException
    function getBoolean(para1: jint): jboolean; overload; virtual;  // throws org.json.JSONException
    function getDouble(para1: jint): jdouble; overload; virtual;  // throws org.json.JSONException
    function getInt(para1: jint): jint; overload; virtual;  // throws org.json.JSONException
    function getJSONArray(para1: jint): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function getJSONObject(para1: jint): JLObject; overload; virtual;  // throws org.json.JSONException
    function getLong(para1: jint): jlong; overload; virtual;  // throws org.json.JSONException
    function getString(para1: jint): JLString; overload; virtual;  // throws org.json.JSONException
    function opt(para1: jint): JLObject; overload; virtual;
    function optBoolean(para1: jint): jboolean; overload; virtual;
    function optDouble(para1: jint): jdouble; overload; virtual;
    function optInt(para1: jint): jint; overload; virtual;
    function optJSONArray(para1: jint): OJJSONArray; overload; virtual;
    function optJSONObject(para1: jint): JLObject; overload; virtual;
    function optLong(para1: jint): jlong; overload; virtual;
    function optString(para1: jint): JLString; overload; virtual;
    function isNull(para1: jint): jboolean; overload; virtual;
    function getArrayBuffer(para1: jint): Arr1jbyte; overload; virtual;  // throws org.json.JSONException
  end;

  OACDirectoryManager = class external 'org.apache.cordova' name 'DirectoryManager' (JLObject)
  public
    constructor create(); overload;
  strict protected
    class function testFileExists(para1: JLString): jboolean; static; overload;
    class function getFreeDiskSpace(para1: jboolean): jlong; static; overload;
    class function testSaveLocationExists(): jboolean; static; overload;
    class function getTempDirectoryPath(para1: ACContext): JLString; static; overload;
  end;

  OACExifHelper = class external 'org.apache.cordova' name 'ExifHelper' (JLObject)
  public
    constructor create(); overload;
    procedure createInFile(para1: JLString); overload; virtual;  // throws java.io.IOException
    procedure createOutFile(para1: JLString); overload; virtual;  // throws java.io.IOException
    procedure readExifData(); overload; virtual;
    procedure writeExifData(); overload; virtual;  // throws java.io.IOException
    function getOrientation(): jint; overload; virtual;
    procedure resetOrientation(); overload; virtual;
  end;

  OACExposedJsApi = class external 'org.apache.cordova' name 'ExposedJsApi' (JLObject)
  public
    constructor create(para1: OACAPluginManager; para2: OACNativeToJsMessageQueue); overload;
    function exec(para1: JLString; para2: JLString; para3: JLString; para4: JLString): JLString; overload; virtual;  // throws org.json.JSONException
    procedure setNativeToJsBridgeMode(para1: jint); overload; virtual;
    function retrieveJsMessages(): JLString; overload; virtual;
  end;

  OACFileProgressResult = class external 'org.apache.cordova' name 'FileProgressResult' (JLObject)
  public
    constructor create(); overload;
    function getLengthComputable(): jboolean; overload; virtual;
    procedure setLengthComputable(para1: jboolean); overload; virtual;
    function getLoaded(): jlong; overload; virtual;
    procedure setLoaded(para1: jlong); overload; virtual;
    function getTotal(): jlong; overload; virtual;
    procedure setTotal(para1: jlong); overload; virtual;
    function toJSONObject(): OJJSONObject; overload; virtual;  // throws org.json.JSONException
  end;

  OACFileUploadResult = class external 'org.apache.cordova' name 'FileUploadResult' (JLObject)
  public
    constructor create(); overload;
    function getBytesSent(): jlong; overload; virtual;
    procedure setBytesSent(para1: jlong); overload; virtual;
    function getResponseCode(): jint; overload; virtual;
    procedure setResponseCode(para1: jint); overload; virtual;
    function getResponse(): JLString; overload; virtual;
    procedure setResponse(para1: JLString); overload; virtual;
    function getObjectId(): JLString; overload; virtual;
    procedure setObjectId(para1: JLString); overload; virtual;
    function toJSONObject(): OJJSONObject; overload; virtual;  // throws org.json.JSONException
  end;

  OACHttpHandler = class external 'org.apache.cordova' name 'HttpHandler' (JLObject)
  public
    constructor create(); overload;
  strict protected
    function get(para1: JLString; para2: JLString): JLBoolean; overload; virtual;
  end;

  OACNativeToJsMessageQueue = class external 'org.apache.cordova' name 'NativeToJsMessageQueue' (JLObject)
  public
    type
      InnerJsMessage = class;
      Arr1InnerJsMessage = array of InnerJsMessage;
      Arr2InnerJsMessage = array of Arr1InnerJsMessage;
      Arr3InnerJsMessage = array of Arr2InnerJsMessage;
      InnerBridgeMode = interface;
      Arr1InnerBridgeMode = array of InnerBridgeMode;
      Arr2InnerBridgeMode = array of Arr1InnerBridgeMode;
      Arr3InnerBridgeMode = array of Arr2InnerBridgeMode;
      InnerPrivateApiBridgeMode = class;
      Arr1InnerPrivateApiBridgeMode = array of InnerPrivateApiBridgeMode;
      Arr2InnerPrivateApiBridgeMode = array of Arr1InnerPrivateApiBridgeMode;
      Arr3InnerPrivateApiBridgeMode = array of Arr2InnerPrivateApiBridgeMode;
      InnerOnlineEventsBridgeMode = class;
      Arr1InnerOnlineEventsBridgeMode = array of InnerOnlineEventsBridgeMode;
      Arr2InnerOnlineEventsBridgeMode = array of Arr1InnerOnlineEventsBridgeMode;
      Arr3InnerOnlineEventsBridgeMode = array of Arr2InnerOnlineEventsBridgeMode;
      InnerLoadUrlBridgeMode = class;
      Arr1InnerLoadUrlBridgeMode = array of InnerLoadUrlBridgeMode;
      Arr2InnerLoadUrlBridgeMode = array of Arr1InnerLoadUrlBridgeMode;
      Arr3InnerLoadUrlBridgeMode = array of Arr2InnerLoadUrlBridgeMode;
      InnerJsMessage = class external 'org.apache.cordova' name 'JsMessage' 
      end;

      InnerBridgeMode = interface external 'org.apache.cordova' name 'BridgeMode' 
      end;

      InnerPrivateApiBridgeMode = class external 'org.apache.cordova' name 'PrivateApiBridgeMode' 
      end;

      InnerOnlineEventsBridgeMode = class external 'org.apache.cordova' name 'OnlineEventsBridgeMode' 
      end;

      InnerLoadUrlBridgeMode = class external 'org.apache.cordova' name 'LoadUrlBridgeMode' 
      end;

  public
    constructor create(para1: OACCordovaWebView; para2: OACACordovaInterface); overload;
    procedure setBridgeMode(para1: jint); overload; virtual;
    procedure reset(); overload; virtual;
    function popAndEncode(): JLString; overload; virtual;
    procedure addJavaScript(para1: JLString); overload; virtual;
    procedure addPluginResult(para1: OACAPluginResult; para2: JLString); overload; virtual;
    procedure setPaused(para1: jboolean); overload; virtual;
    function getPaused(): jboolean; overload; virtual;
  end;

  OACACallbackContext = class external 'org.apache.cordova.api' name 'CallbackContext' (JLObject)
  public
    constructor create(para1: JLString; para2: OACCordovaWebView); overload;
    function isFinished(): jboolean; overload; virtual;
    function isChangingThreads(): jboolean; overload; virtual;
    function getCallbackId(): JLString; overload; virtual;
    procedure sendPluginResult(para1: OACAPluginResult); overload; virtual;
    procedure success(para1: OJJSONObject); overload; virtual;
    procedure success(para1: JLString); overload; virtual;
    procedure success(para1: OJJSONArray); overload; virtual;
    procedure success(para1: Arr1jbyte); overload; virtual;
    procedure success(var para1: array of jbyte); overload; virtual;
    procedure success(); overload; virtual;
    procedure error(para1: OJJSONObject); overload; virtual;
    procedure error(para1: JLString); overload; virtual;
    procedure error(para1: jint); overload; virtual;
  end;

  OACACordovaInterface = interface external 'org.apache.cordova.api' name 'CordovaInterface' 
    procedure startActivityForResult(para1: OACACordovaPlugin; para2: ACIntent; para3: jint); overload;
    procedure setActivityResultCallback(para1: OACACordovaPlugin); overload;
    function getActivity(): AAActivity; overload;
    function onMessage(para1: JLString; para2: JLObject): JLObject; overload;
    function getThreadPool(): JUCExecutorService; overload;
  end;

  OACACordovaPlugin = class external 'org.apache.cordova.api' name 'CordovaPlugin' (JLObject)
  public
    var
      fid: JLString; external name 'id';
      fwebView: OACCordovaWebView; external name 'webView';
      fcordova: OACACordovaInterface; external name 'cordova';
  public
    constructor create(); overload;
    procedure initialize(para1: OACACordovaInterface; para2: OACCordovaWebView); overload; virtual;
    function execute(para1: JLString; para2: JLString; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    function execute(para1: JLString; para2: OACCordovaArgs; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onPause(para1: jboolean); overload; virtual;
    procedure onResume(para1: jboolean); overload; virtual;
    procedure onNewIntent(para1: ACIntent); overload; virtual;
    procedure onDestroy(); overload; virtual;
    function onMessage(para1: JLString; para2: JLObject): JLObject; overload; virtual;
    procedure onActivityResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
    function onOverrideUrlLoading(para1: JLString): jboolean; overload; virtual;
    procedure onReset(); overload; virtual;
  end;

  OACALOG = class external 'org.apache.cordova.api' name 'LOG' (JLObject)
  public
    const
      VERBOSE = 2;
      DEBUG = 3;
      INFO = 4;
      WARN = 5;
      ERROR = 6;
  public
    class var
      fLOGLEVEL: jint; external name 'LOGLEVEL';
  public
    constructor create(); overload;
    class procedure setLogLevel(para1: jint); static; overload;
    class procedure setLogLevel(para1: JLString); static; overload;
    class function isLoggable(para1: jint): jboolean; static; overload;
    class procedure v(para1: JLString; para2: JLString); static; overload;
    class procedure d(para1: JLString; para2: JLString); static; overload;
    class procedure i(para1: JLString; para2: JLString); static; overload;
    class procedure w(para1: JLString; para2: JLString); static; overload;
    class procedure e(para1: JLString; para2: JLString); static; overload;
    class procedure v(para1: JLString; para2: JLString; para3: JLThrowable); static; overload;
    class procedure d(para1: JLString; para2: JLString; para3: JLThrowable); static; overload;
    class procedure i(para1: JLString; para2: JLString; para3: JLThrowable); static; overload;
    class procedure w(para1: JLString; para2: JLString; para3: JLThrowable); static; overload;
    class procedure e(para1: JLString; para2: JLString; para3: JLThrowable); static; overload;
    class procedure v(para1: JLString; para2: JLString; para3: Arr1JLObject); static; overload;
    class procedure v(para1: JLString; para2: JLString; const para3: array of JLObject); static; overload;
    class procedure d(para1: JLString; para2: JLString; para3: Arr1JLObject); static; overload;
    class procedure d(para1: JLString; para2: JLString; const para3: array of JLObject); static; overload;
    class procedure i(para1: JLString; para2: JLString; para3: Arr1JLObject); static; overload;
    class procedure i(para1: JLString; para2: JLString; const para3: array of JLObject); static; overload;
    class procedure w(para1: JLString; para2: JLString; para3: Arr1JLObject); static; overload;
    class procedure w(para1: JLString; para2: JLString; const para3: array of JLObject); static; overload;
    class procedure e(para1: JLString; para2: JLString; para3: Arr1JLObject); static; overload;
    class procedure e(para1: JLString; para2: JLString; const para3: array of JLObject); static; overload;
  end;

  OACAPluginEntry = class external 'org.apache.cordova.api' name 'PluginEntry' (JLObject)
  public
    var
      fservice: JLString; external name 'service';
      fpluginClass: JLString; external name 'pluginClass';
      fplugin: OACACordovaPlugin; external name 'plugin';
      fonload: jboolean; external name 'onload';
  public
    constructor create(para1: JLString; para2: JLString; para3: jboolean); overload;
    function createPlugin(para1: OACCordovaWebView; para2: OACACordovaInterface): OACACordovaPlugin; overload; virtual;
  end;

  OACAPluginManager = class external 'org.apache.cordova.api' name 'PluginManager' (JLObject)
  strict protected
    var
      furlMap: JUHashMap; external name 'urlMap';
  public
    constructor create(para1: OACCordovaWebView; para2: OACACordovaInterface); overload;
    procedure init(); overload; virtual;
    procedure loadPlugins(); overload; virtual;
    procedure clearPluginObjects(); overload; virtual;
    procedure startupPlugins(); overload; virtual;
    function exec(para1: JLString; para2: JLString; para3: JLString; para4: JLString): jboolean; overload; virtual;
    function exec(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: jboolean): jboolean; overload; virtual;
    function getPlugin(para1: JLString): OACACordovaPlugin; overload; virtual;
    procedure addService(para1: JLString; para2: JLString); overload; virtual;
    procedure addService(para1: OACAPluginEntry); overload; virtual;
    procedure onPause(para1: jboolean); overload; virtual;
    procedure onResume(para1: jboolean); overload; virtual;
    procedure onDestroy(); overload; virtual;
    function postMessage(para1: JLString; para2: JLObject): JLObject; overload; virtual;
    procedure onNewIntent(para1: ACIntent); overload; virtual;
    function onOverrideUrlLoading(para1: JLString): jboolean; overload; virtual;
    procedure onReset(); overload; virtual;
  end;

  OAHConnectionReuseStrategy = interface external 'org.apache.http' name 'ConnectionReuseStrategy' 
    function keepAlive(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload;
  end;

  OAHHeader = interface external 'org.apache.http' name 'Header' 
    function getName(): JLString; overload;
    function getValue(): JLString; overload;
    function getElements(): Arr1OAHHeaderElement; overload;  // throws org.apache.http.ParseException
  end;

  OAHHeaderElement = interface external 'org.apache.http' name 'HeaderElement' 
    function getName(): JLString; overload;
    function getValue(): JLString; overload;
    function getParameters(): Arr1OAHNameValuePair; overload;
    function getParameterByName(para1: JLString): OAHNameValuePair; overload;
    function getParameterCount(): jint; overload;
    function getParameter(para1: jint): OAHNameValuePair; overload;
  end;

  OAHHttpConnection = interface external 'org.apache.http' name 'HttpConnection' 
    procedure close(); overload;  // throws java.io.IOException
    function isOpen(): jboolean; overload;
    function isStale(): jboolean; overload;
    procedure setSocketTimeout(para1: jint); overload;
    function getSocketTimeout(): jint; overload;
    procedure shutdown(); overload;  // throws java.io.IOException
    function getMetrics(): OAHHttpConnectionMetrics; overload;
  end;

  OAHHttpConnectionMetrics = interface external 'org.apache.http' name 'HttpConnectionMetrics' 
    function getRequestCount(): jlong; overload;
    function getResponseCount(): jlong; overload;
    function getSentBytesCount(): jlong; overload;
    function getReceivedBytesCount(): jlong; overload;
    function getMetric(para1: JLString): JLObject; overload;
    procedure reset(); overload;
  end;

  OAHHttpEntity = interface external 'org.apache.http' name 'HttpEntity' 
    function isRepeatable(): jboolean; overload;
    function isChunked(): jboolean; overload;
    function getContentLength(): jlong; overload;
    function getContentType(): OAHHeader; overload;
    function getContentEncoding(): OAHHeader; overload;
    function getContent(): JIInputStream; overload;  // throws java.io.IOException, java.lang.IllegalStateException
    procedure writeTo(para1: JIOutputStream); overload;  // throws java.io.IOException
    function isStreaming(): jboolean; overload;
    procedure consumeContent(); overload;  // throws java.io.IOException
  end;

  OAHHttpMessage = interface external 'org.apache.http' name 'HttpMessage' 
    function getProtocolVersion(): OAHProtocolVersion; overload;
    function containsHeader(para1: JLString): jboolean; overload;
    function getHeaders(para1: JLString): Arr1OAHHeader; overload;
    function getFirstHeader(para1: JLString): OAHHeader; overload;
    function getLastHeader(para1: JLString): OAHHeader; overload;
    function getAllHeaders(): Arr1OAHHeader; overload;
    procedure addHeader(para1: OAHHeader); overload;
    procedure addHeader(para1: JLString; para2: JLString); overload;
    procedure setHeader(para1: OAHHeader); overload;
    procedure setHeader(para1: JLString; para2: JLString); overload;
    procedure setHeaders(para1: Arr1OAHHeader); overload;
    procedure setHeaders(var para1: array of OAHHeader); overload;
    procedure removeHeader(para1: OAHHeader); overload;
    procedure removeHeaders(para1: JLString); overload;
    function headerIterator(): OAHHeaderIterator; overload;
    function headerIterator(para1: JLString): OAHHeaderIterator; overload;
    function getParams(): OAHPHttpParams; overload;
    procedure setParams(para1: OAHPHttpParams); overload;
  end;

  OAHHttpRequestFactory = interface external 'org.apache.http' name 'HttpRequestFactory' 
    function newHttpRequest(para1: OAHRequestLine): OAHHttpRequest; overload;  // throws org.apache.http.MethodNotSupportedException
    function newHttpRequest(para1: JLString; para2: JLString): OAHHttpRequest; overload;  // throws org.apache.http.MethodNotSupportedException
  end;

  OAHHttpRequestInterceptor = interface external 'org.apache.http' name 'HttpRequestInterceptor' 
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHHttpResponseFactory = interface external 'org.apache.http' name 'HttpResponseFactory' 
    function newHttpResponse(para1: OAHProtocolVersion; para2: jint; para3: OAHPHttpContext): OAHHttpResponse; overload;
    function newHttpResponse(para1: OAHStatusLine; para2: OAHPHttpContext): OAHHttpResponse; overload;
  end;

  OAHHttpResponseInterceptor = interface external 'org.apache.http' name 'HttpResponseInterceptor' 
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHHttpStatus = interface external 'org.apache.http' name 'HttpStatus' 
    const
      SC_CONTINUE = 100;
      SC_SWITCHING_PROTOCOLS = 101;
      SC_PROCESSING = 102;
      SC_OK = 200;
      SC_CREATED = 201;
      SC_ACCEPTED = 202;
      SC_NON_AUTHORITATIVE_INFORMATION = 203;
      SC_NO_CONTENT = 204;
      SC_RESET_CONTENT = 205;
      SC_PARTIAL_CONTENT = 206;
      SC_MULTI_STATUS = 207;
      SC_MULTIPLE_CHOICES = 300;
      SC_MOVED_PERMANENTLY = 301;
      SC_MOVED_TEMPORARILY = 302;
      SC_SEE_OTHER = 303;
      SC_NOT_MODIFIED = 304;
      SC_USE_PROXY = 305;
      SC_TEMPORARY_REDIRECT = 307;
      SC_BAD_REQUEST = 400;
      SC_UNAUTHORIZED = 401;
      SC_PAYMENT_REQUIRED = 402;
      SC_FORBIDDEN = 403;
      SC_NOT_FOUND = 404;
      SC_METHOD_NOT_ALLOWED = 405;
      SC_NOT_ACCEPTABLE = 406;
      SC_PROXY_AUTHENTICATION_REQUIRED = 407;
      SC_REQUEST_TIMEOUT = 408;
      SC_CONFLICT = 409;
      SC_GONE = 410;
      SC_LENGTH_REQUIRED = 411;
      SC_PRECONDITION_FAILED = 412;
      SC_REQUEST_TOO_LONG = 413;
      SC_REQUEST_URI_TOO_LONG = 414;
      SC_UNSUPPORTED_MEDIA_TYPE = 415;
      SC_REQUESTED_RANGE_NOT_SATISFIABLE = 416;
      SC_EXPECTATION_FAILED = 417;
      SC_INSUFFICIENT_SPACE_ON_RESOURCE = 419;
      SC_METHOD_FAILURE = 420;
      SC_UNPROCESSABLE_ENTITY = 422;
      SC_LOCKED = 423;
      SC_FAILED_DEPENDENCY = 424;
      SC_INTERNAL_SERVER_ERROR = 500;
      SC_NOT_IMPLEMENTED = 501;
      SC_BAD_GATEWAY = 502;
      SC_SERVICE_UNAVAILABLE = 503;
      SC_GATEWAY_TIMEOUT = 504;
      SC_HTTP_VERSION_NOT_SUPPORTED = 505;
      SC_INSUFFICIENT_STORAGE = 507;
  end;

  OAHNameValuePair = interface external 'org.apache.http' name 'NameValuePair' 
    function getName(): JLString; overload;
    function getValue(): JLString; overload;
  end;

  OAHReasonPhraseCatalog = interface external 'org.apache.http' name 'ReasonPhraseCatalog' 
    function getReason(para1: jint; para2: JULocale): JLString; overload;
  end;

  OAHRequestLine = interface external 'org.apache.http' name 'RequestLine' 
    function getMethod(): JLString; overload;
    function getProtocolVersion(): OAHProtocolVersion; overload;
    function getUri(): JLString; overload;
  end;

  OAHStatusLine = interface external 'org.apache.http' name 'StatusLine' 
    function getProtocolVersion(): OAHProtocolVersion; overload;
    function getStatusCode(): jint; overload;
    function getReasonPhrase(): JLString; overload;
  end;

  OAHAAUTH = class sealed external 'org.apache.http.auth' name 'AUTH' (JLObject)
  public
    const
      WWW_AUTH = 'WWW-Authenticate';
      WWW_AUTH_RESP = 'Authorization';
      PROXY_AUTH = 'Proxy-Authenticate';
      PROXY_AUTH_RESP = 'Proxy-Authorization';
  end;

  OAHAAuthScheme = interface external 'org.apache.http.auth' name 'AuthScheme' 
    procedure processChallenge(para1: OAHHeader); overload;  // throws org.apache.http.auth.MalformedChallengeException
    function getSchemeName(): JLString; overload;
    function getParameter(para1: JLString): JLString; overload;
    function getRealm(): JLString; overload;
    function isConnectionBased(): jboolean; overload;
    function isComplete(): jboolean; overload;
    function authenticate(para1: OAHACredentials; para2: OAHHttpRequest): OAHHeader; overload;  // throws org.apache.http.auth.AuthenticationException
  end;

  OAHAAuthSchemeFactory = interface external 'org.apache.http.auth' name 'AuthSchemeFactory' 
    function newInstance(para1: OAHPHttpParams): OAHAAuthScheme; overload;
  end;

  OAHAAuthSchemeRegistry = class sealed external 'org.apache.http.auth' name 'AuthSchemeRegistry' (JLObject)
  public
    constructor create(); overload;
    procedure register(para1: JLString; para2: OAHAAuthSchemeFactory); overload; virtual;
    procedure unregister(para1: JLString); overload; virtual;
    function getAuthScheme(para1: JLString; para2: OAHPHttpParams): OAHAAuthScheme; overload; virtual;  // throws java.lang.IllegalStateException
    function getSchemeNames(): JUList; overload; virtual;
    procedure setItems(para1: JUMap); overload; virtual;
  end;

  OAHAAuthScope = class external 'org.apache.http.auth' name 'AuthScope' (JLObject)
  public
    final class var
      fANY_HOST: JLString; external name 'ANY_HOST';
  public
    const
      ANY_PORT = -1;
  public
    final class var
      fANY_REALM: JLString; external name 'ANY_REALM';
      fANY_SCHEME: JLString; external name 'ANY_SCHEME';
      fANY: OAHAAuthScope; external name 'ANY';
  public
    constructor create(para1: JLString; para2: jint; para3: JLString; para4: JLString); overload;
    constructor create(para1: JLString; para2: jint; para3: JLString); overload;
    constructor create(para1: JLString; para2: jint); overload;
    constructor create(para1: OAHAAuthScope); overload;
    function getHost(): JLString; overload; virtual;
    function getPort(): jint; overload; virtual;
    function getRealm(): JLString; overload; virtual;
    function getScheme(): JLString; overload; virtual;
    function match(para1: OAHAAuthScope): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  OAHAAuthState = class external 'org.apache.http.auth' name 'AuthState' (JLObject)
  public
    constructor create(); overload;
    procedure invalidate(); overload; virtual;
    function isValid(): jboolean; overload; virtual;
    procedure setAuthScheme(para1: OAHAAuthScheme); overload; virtual;
    function getAuthScheme(): OAHAAuthScheme; overload; virtual;
    function getCredentials(): OAHACredentials; overload; virtual;
    procedure setCredentials(para1: OAHACredentials); overload; virtual;
    function getAuthScope(): OAHAAuthScope; overload; virtual;
    procedure setAuthScope(para1: OAHAAuthScope); overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHACredentials = interface external 'org.apache.http.auth' name 'Credentials' 
    function getUserPrincipal(): JSPrincipal; overload;
    function getPassword(): JLString; overload;
  end;

  OAHAPAuthPNames = interface external 'org.apache.http.auth.params' name 'AuthPNames' 
    const
      CREDENTIAL_CHARSET = 'http.auth.credential-charset';
  end;

  OAHAPAuthParams = class sealed external 'org.apache.http.auth.params' name 'AuthParams' (JLObject)
  public
    class function getCredentialCharset(para1: OAHPHttpParams): JLString; static; overload;
    class procedure setCredentialCharset(para1: OAHPHttpParams; para2: JLString); static; overload;
  end;

  OAHCAuthenticationHandler = interface external 'org.apache.http.client' name 'AuthenticationHandler' 
    function isAuthenticationRequested(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload;
    function getChallenges(para1: OAHHttpResponse; para2: OAHPHttpContext): JUMap; overload;  // throws org.apache.http.auth.MalformedChallengeException
    function selectScheme(para1: JUMap; para2: OAHHttpResponse; para3: OAHPHttpContext): OAHAAuthScheme; overload;  // throws org.apache.http.auth.AuthenticationException
  end;

  OAHCCookieStore = interface external 'org.apache.http.client' name 'CookieStore' 
    procedure addCookie(para1: OAHCCookie); overload;
    function getCookies(): JUList; overload;
    function clearExpired(para1: JUDate): jboolean; overload;
    procedure clear(); overload;
  end;

  OAHCCredentialsProvider = interface external 'org.apache.http.client' name 'CredentialsProvider' 
    procedure setCredentials(para1: OAHAAuthScope; para2: OAHACredentials); overload;
    function getCredentials(para1: OAHAAuthScope): OAHACredentials; overload;
    procedure clear(); overload;
  end;

  OAHCHttpClient = interface external 'org.apache.http.client' name 'HttpClient' 
    function getParams(): OAHPHttpParams; overload;
    function getConnectionManager(): OAHCClientConnectionManager; overload;
    function execute(para1: OAHCMHttpUriRequest): OAHHttpResponse; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHCMHttpUriRequest; para2: OAHPHttpContext): OAHHttpResponse; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest): OAHHttpResponse; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHHttpResponse; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHCMHttpUriRequest; para2: OAHCResponseHandler): JLObject; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHCMHttpUriRequest; para2: OAHCResponseHandler; para3: OAHPHttpContext): JLObject; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHCResponseHandler): JLObject; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHCResponseHandler; para4: OAHPHttpContext): JLObject; overload;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
  end;

  OAHCHttpRequestRetryHandler = interface external 'org.apache.http.client' name 'HttpRequestRetryHandler' 
    function retryRequest(para1: JIIOException; para2: jint; para3: OAHPHttpContext): jboolean; overload;
  end;

  OAHCRedirectHandler = interface external 'org.apache.http.client' name 'RedirectHandler' 
    function isRedirectRequested(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload;
    function getLocationURI(para1: OAHHttpResponse; para2: OAHPHttpContext): JNURI; overload;  // throws org.apache.http.ProtocolException
  end;

  OAHCRequestDirector = interface external 'org.apache.http.client' name 'RequestDirector' 
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHHttpResponse; overload;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHCResponseHandler = interface external 'org.apache.http.client' name 'ResponseHandler' 
    function handleResponse(para1: OAHHttpResponse): JLObject; overload;  // throws org.apache.http.client.ClientProtocolException, java.io.IOException
  end;

  OAHCUserTokenHandler = interface external 'org.apache.http.client' name 'UserTokenHandler' 
    function getUserToken(para1: OAHPHttpContext): JLObject; overload;
  end;

  OAHCMAbortableHttpRequest = interface external 'org.apache.http.client.methods' name 'AbortableHttpRequest' 
    procedure setConnectionRequest(para1: OAHCClientConnectionRequest); overload;  // throws java.io.IOException
    procedure setReleaseTrigger(para1: OAHCConnectionReleaseTrigger); overload;  // throws java.io.IOException
    procedure abort(); overload;
  end;

  OAHCPAuthPolicy = class sealed external 'org.apache.http.client.params' name 'AuthPolicy' (JLObject)
  public
    const
      NTLM = 'NTLM';
      DIGEST = 'Digest';
      BASIC = 'Basic';
  end;

  OAHCPClientPNames = interface external 'org.apache.http.client.params' name 'ClientPNames' 
    const
      CONNECTION_MANAGER_FACTORY_CLASS_NAME = 'http.connection-manager.factory-class-name';
      CONNECTION_MANAGER_FACTORY = 'http.connection-manager.factory-object';
      HANDLE_REDIRECTS = 'http.protocol.handle-redirects';
      REJECT_RELATIVE_REDIRECT = 'http.protocol.reject-relative-redirect';
      MAX_REDIRECTS = 'http.protocol.max-redirects';
      ALLOW_CIRCULAR_REDIRECTS = 'http.protocol.allow-circular-redirects';
      HANDLE_AUTHENTICATION = 'http.protocol.handle-authentication';
      COOKIE_POLICY = 'http.protocol.cookie-policy';
      VIRTUAL_HOST = 'http.virtual-host';
      DEFAULT_HEADERS = 'http.default-headers';
      DEFAULT_HOST = 'http.default-host';
  end;

  OAHCPCookiePolicy = class sealed external 'org.apache.http.client.params' name 'CookiePolicy' (JLObject)
  public
    const
      BROWSER_COMPATIBILITY = 'compatibility';
      NETSCAPE = 'netscape';
      RFC_2109 = 'rfc2109';
      RFC_2965 = 'rfc2965';
      BEST_MATCH = 'best-match';
  end;

  OAHCPHttpClientParams = class external 'org.apache.http.client.params' name 'HttpClientParams' (JLObject)
  public
    class function isRedirecting(para1: OAHPHttpParams): jboolean; static; overload;
    class procedure setRedirecting(para1: OAHPHttpParams; para2: jboolean); static; overload;
    class function isAuthenticating(para1: OAHPHttpParams): jboolean; static; overload;
    class procedure setAuthenticating(para1: OAHPHttpParams; para2: jboolean); static; overload;
    class function getCookiePolicy(para1: OAHPHttpParams): JLString; static; overload;
    class procedure setCookiePolicy(para1: OAHPHttpParams; para2: JLString); static; overload;
  end;

  OAHCPClientContext = interface external 'org.apache.http.client.protocol' name 'ClientContext' 
    const
      COOKIESPEC_REGISTRY = 'http.cookiespec-registry';
      AUTHSCHEME_REGISTRY = 'http.authscheme-registry';
      COOKIE_STORE = 'http.cookie-store';
      COOKIE_SPEC = 'http.cookie-spec';
      COOKIE_ORIGIN = 'http.cookie-origin';
      CREDS_PROVIDER = 'http.auth.credentials-provider';
      TARGET_AUTH_STATE = 'http.auth.target-scope';
      PROXY_AUTH_STATE = 'http.auth.proxy-scope';
      AUTH_SCHEME_PREF = 'http.auth.scheme-pref';
      USER_TOKEN = 'http.user-token';
  end;

  OAHCUCloneUtils = class external 'org.apache.http.client.utils' name 'CloneUtils' (JLObject)
  public
    class function clone(para1: JLObject): JLObject; static; overload;  // throws java.lang.CloneNotSupportedException
  end;

  OAHCUURIUtils = class external 'org.apache.http.client.utils' name 'URIUtils' (JLObject)
  public
    class function createURI(para1: JLString; para2: JLString; para3: jint; para4: JLString; para5: JLString; para6: JLString): JNURI; static; overload;  // throws java.net.URISyntaxException
    class function rewriteURI(para1: JNURI; para2: OAHHttpHost; para3: jboolean): JNURI; static; overload;  // throws java.net.URISyntaxException
    class function rewriteURI(para1: JNURI; para2: OAHHttpHost): JNURI; static; overload;  // throws java.net.URISyntaxException
    class function resolve(para1: JNURI; para2: JLString): JNURI; static; overload;
    class function resolve(para1: JNURI; para2: JNURI): JNURI; static; overload;
  end;

  OAHCUURLEncodedUtils = class external 'org.apache.http.client.utils' name 'URLEncodedUtils' (JLObject)
  public
    const
      CONTENT_TYPE = 'application/x-www-form-urlencoded';
  public
    constructor create(); overload;
    class function parse(para1: JNURI; para2: JLString): JUList; static; overload;
    class function parse(para1: OAHHttpEntity): JUList; static; overload;  // throws java.io.IOException
    class function isEncoded(para1: OAHHttpEntity): jboolean; static; overload;
    class procedure parse(para1: JUList; para2: JUScanner; para3: JLString); static; overload;
    class function format(para1: JUList; para2: JLString): JLString; static; overload;
  end;

  OAHCClientConnectionManager = interface external 'org.apache.http.conn' name 'ClientConnectionManager' 
    function getSchemeRegistry(): OAHCSSchemeRegistry; overload;
    function requestConnection(para1: OAHCRHttpRoute; para2: JLObject): OAHCClientConnectionRequest; overload;
    procedure releaseConnection(para1: OAHCManagedClientConnection; para2: jlong; para3: JUCTimeUnit); overload;
    procedure closeIdleConnections(para1: jlong; para2: JUCTimeUnit); overload;
    procedure closeExpiredConnections(); overload;
    procedure shutdown(); overload;
  end;

  OAHCClientConnectionManagerFactory = interface external 'org.apache.http.conn' name 'ClientConnectionManagerFactory' 
    function newInstance(para1: OAHPHttpParams; para2: OAHCSSchemeRegistry): OAHCClientConnectionManager; overload;
  end;

  OAHCClientConnectionOperator = interface external 'org.apache.http.conn' name 'ClientConnectionOperator' 
    function createConnection(): OAHCOperatedClientConnection; overload;
    procedure openConnection(para1: OAHCOperatedClientConnection; para2: OAHHttpHost; para3: JNInetAddress; para4: OAHPHttpContext; para5: OAHPHttpParams); overload;  // throws java.io.IOException
    procedure updateSecureConnection(para1: OAHCOperatedClientConnection; para2: OAHHttpHost; para3: OAHPHttpContext; para4: OAHPHttpParams); overload;  // throws java.io.IOException
  end;

  OAHCClientConnectionRequest = interface external 'org.apache.http.conn' name 'ClientConnectionRequest' 
    function getConnection(para1: jlong; para2: JUCTimeUnit): OAHCManagedClientConnection; overload;  // throws java.lang.InterruptedException, org.apache.http.conn.ConnectionPoolTimeoutException
    procedure abortRequest(); overload;
  end;

  OAHCConnectionKeepAliveStrategy = interface external 'org.apache.http.conn' name 'ConnectionKeepAliveStrategy' 
    function getKeepAliveDuration(para1: OAHHttpResponse; para2: OAHPHttpContext): jlong; overload;
  end;

  OAHCConnectionReleaseTrigger = interface external 'org.apache.http.conn' name 'ConnectionReleaseTrigger' 
    procedure releaseConnection(); overload;  // throws java.io.IOException
    procedure abortConnection(); overload;  // throws java.io.IOException
  end;

  OAHCEofSensorWatcher = interface external 'org.apache.http.conn' name 'EofSensorWatcher' 
    function eofDetected(para1: JIInputStream): jboolean; overload;  // throws java.io.IOException
    function streamClosed(para1: JIInputStream): jboolean; overload;  // throws java.io.IOException
    function streamAbort(para1: JIInputStream): jboolean; overload;  // throws java.io.IOException
  end;

  OAHCPConnConnectionPNames = interface external 'org.apache.http.conn.params' name 'ConnConnectionPNames' 
    const
      MAX_STATUS_LINE_GARBAGE = 'http.connection.max-status-line-garbage';
  end;

  OAHCPConnManagerPNames = interface external 'org.apache.http.conn.params' name 'ConnManagerPNames' 
    const
      TIMEOUT = 'http.conn-manager.timeout';
      MAX_CONNECTIONS_PER_ROUTE = 'http.conn-manager.max-per-route';
      MAX_TOTAL_CONNECTIONS = 'http.conn-manager.max-total';
  end;

  OAHCPConnPerRoute = interface external 'org.apache.http.conn.params' name 'ConnPerRoute' 
    function getMaxForRoute(para1: OAHCRHttpRoute): jint; overload;
  end;

  OAHCPConnRoutePNames = interface external 'org.apache.http.conn.params' name 'ConnRoutePNames' 
    const
      DEFAULT_PROXY = 'http.route.default-proxy';
      LOCAL_ADDRESS = 'http.route.local-address';
      FORCED_ROUTE = 'http.route.forced-route';
  end;

  OAHCRHttpRouteDirector = interface external 'org.apache.http.conn.routing' name 'HttpRouteDirector' 
    const
      UNREACHABLE = -1;
      COMPLETE = 0;
      CONNECT_TARGET = 1;
      CONNECT_PROXY = 2;
      TUNNEL_TARGET = 3;
      TUNNEL_PROXY = 4;
      LAYER_PROTOCOL = 5;
    function nextStep(para1: OAHCRRouteInfo; para2: OAHCRRouteInfo): jint; overload;
  end;

  OAHCRHttpRoutePlanner = interface external 'org.apache.http.conn.routing' name 'HttpRoutePlanner' 
    function determineRoute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHCRHttpRoute; overload;  // throws org.apache.http.HttpException
  end;

  OAHCSHostNameResolver = interface external 'org.apache.http.conn.scheme' name 'HostNameResolver' 
    function resolve(para1: JLString): JNInetAddress; overload;  // throws java.io.IOException
  end;

  OAHCSScheme = class sealed external 'org.apache.http.conn.scheme' name 'Scheme' (JLObject)
  public
    constructor create(para1: JLString; para2: OAHCSSocketFactory; para3: jint); overload;
    function getDefaultPort(): jint; overload; virtual; final;
    function getSocketFactory(): OAHCSSocketFactory; overload; virtual; final;
    function getName(): JLString; overload; virtual; final;
    function isLayered(): jboolean; overload; virtual; final;
    function resolvePort(para1: jint): jint; overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function hashCode(): jint; overload; virtual;
  end;

  OAHCSSchemeRegistry = class sealed external 'org.apache.http.conn.scheme' name 'SchemeRegistry' (JLObject)
  public
    constructor create(); overload;
    function getScheme(para1: JLString): OAHCSScheme; overload; virtual; final;
    function getScheme(para1: OAHHttpHost): OAHCSScheme; overload; virtual; final;
    function get(para1: JLString): OAHCSScheme; overload; virtual; final;
    function register(para1: OAHCSScheme): OAHCSScheme; overload; virtual; final;
    function unregister(para1: JLString): OAHCSScheme; overload; virtual; final;
    function getSchemeNames(): JUList; overload; virtual; final;
    procedure setItems(para1: JUMap); overload; virtual;
  end;

  OAHCSSocketFactory = interface external 'org.apache.http.conn.scheme' name 'SocketFactory' 
    function createSocket(): JNSocket; overload;  // throws java.io.IOException
    function connectSocket(para1: JNSocket; para2: JLString; para3: jint; para4: JNInetAddress; para5: jint; para6: OAHPHttpParams): JNSocket; overload;  // throws java.io.IOException, java.net.UnknownHostException, org.apache.http.conn.ConnectTimeoutException
    function isSecure(para1: JNSocket): jboolean; overload;  // throws java.lang.IllegalArgumentException
  end;

  OAHCUInetAddressUtils = class external 'org.apache.http.conn.util' name 'InetAddressUtils' (JLObject)
  public
    class function isIPv4Address(para1: JLString): jboolean; static; overload;
    class function isIPv6StdAddress(para1: JLString): jboolean; static; overload;
    class function isIPv6HexCompressedAddress(para1: JLString): jboolean; static; overload;
    class function isIPv6Address(para1: JLString): jboolean; static; overload;
  end;

  OAHCCookie = interface external 'org.apache.http.cookie' name 'Cookie' 
    function getName(): JLString; overload;
    function getValue(): JLString; overload;
    function getComment(): JLString; overload;
    function getCommentURL(): JLString; overload;
    function getExpiryDate(): JUDate; overload;
    function isPersistent(): jboolean; overload;
    function getDomain(): JLString; overload;
    function getPath(): JLString; overload;
    function getPorts(): Arr1jint; overload;
    function isSecure(): jboolean; overload;
    function getVersion(): jint; overload;
    function isExpired(para1: JUDate): jboolean; overload;
  end;

  OAHCCookieAttributeHandler = interface external 'org.apache.http.cookie' name 'CookieAttributeHandler' 
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload;
  end;

  OAHCCookieOrigin = class sealed external 'org.apache.http.cookie' name 'CookieOrigin' (JLObject)
  public
    constructor create(para1: JLString; para2: jint; para3: JLString; para4: jboolean); overload;
    function getHost(): JLString; overload; virtual;
    function getPath(): JLString; overload; virtual;
    function getPort(): jint; overload; virtual;
    function isSecure(): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHCCookieSpec = interface external 'org.apache.http.cookie' name 'CookieSpec' 
    function getVersion(): jint; overload;
    function parse(para1: OAHHeader; para2: OAHCCookieOrigin): JUList; overload;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload;
    function formatCookies(para1: JUList): JUList; overload;
    function getVersionHeader(): OAHHeader; overload;
  end;

  OAHCCookieSpecFactory = interface external 'org.apache.http.cookie' name 'CookieSpecFactory' 
    function newInstance(para1: OAHPHttpParams): OAHCCookieSpec; overload;
  end;

  OAHCCookieSpecRegistry = class sealed external 'org.apache.http.cookie' name 'CookieSpecRegistry' (JLObject)
  public
    constructor create(); overload;
    procedure register(para1: JLString; para2: OAHCCookieSpecFactory); overload; virtual;
    procedure unregister(para1: JLString); overload; virtual;
    function getCookieSpec(para1: JLString; para2: OAHPHttpParams): OAHCCookieSpec; overload; virtual;  // throws java.lang.IllegalStateException
    function getCookieSpec(para1: JLString): OAHCCookieSpec; overload; virtual;  // throws java.lang.IllegalStateException
    function getSpecNames(): JUList; overload; virtual;
    procedure setItems(para1: JUMap); overload; virtual;
  end;

  OAHCSM = interface external 'org.apache.http.cookie' name 'SM' 
    const
      COOKIE = 'Cookie';
      COOKIE2 = 'Cookie2';
      SET_COOKIE = 'Set-Cookie';
      SET_COOKIE2 = 'Set-Cookie2';
  end;

  OAHCPCookieSpecPNames = interface external 'org.apache.http.cookie.params' name 'CookieSpecPNames' 
    const
      DATE_PATTERNS = 'http.protocol.cookie-datepatterns';
      SINGLE_COOKIE_HEADER = 'http.protocol.single-cookie-header';
  end;

  OAHEContentLengthStrategy = interface external 'org.apache.http.entity' name 'ContentLengthStrategy' 
    const
      IDENTITY = -1;
      CHUNKED = -2;
    function determineLength(para1: OAHHttpMessage): jlong; overload;  // throws org.apache.http.HttpException
  end;

  OAHEContentProducer = interface external 'org.apache.http.entity' name 'ContentProducer' 
    procedure writeTo(para1: JIOutputStream); overload;  // throws java.io.IOException
  end;

  OAHIANTLMEngine = interface external 'org.apache.http.impl.auth' name 'NTLMEngine' 
    function generateType1Msg(para1: JLString; para2: JLString): JLString; overload;  // throws org.apache.http.impl.auth.NTLMEngineException
    function generateType3Msg(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString): JLString; overload;  // throws org.apache.http.impl.auth.NTLMEngineException
  end;

  OAHICRedirectLocations = class external 'org.apache.http.impl.client' name 'RedirectLocations' (JLObject)
  public
    constructor create(); overload;
    function contains(para1: JNURI): jboolean; overload; virtual;
    procedure add(para1: JNURI); overload; virtual;
    function remove(para1: JNURI): jboolean; overload; virtual;
  end;

  OAHICRoutedRequest = class external 'org.apache.http.impl.client' name 'RoutedRequest' (JLObject)
  strict protected
    final var
      frequest: OAHICRequestWrapper; external name 'request';
      froute: OAHCRHttpRoute; external name 'route';
  public
    constructor create(para1: OAHICRequestWrapper; para2: OAHCRHttpRoute); overload;
    function getRequest(): OAHICRequestWrapper; overload; virtual; final;
    function getRoute(): OAHCRHttpRoute; overload; virtual; final;
  end;

  OAHICAbstractPoolEntry = class abstract external 'org.apache.http.impl.conn' name 'AbstractPoolEntry' (JLObject)
  strict protected
    final var
      fconnOperator: OAHCClientConnectionOperator; external name 'connOperator';
      fconnection: OAHCOperatedClientConnection; external name 'connection';
  strict protected
    var
      froute: OAHCRHttpRoute; external name 'route';
      fstate: JLObject; external name 'state';
      ftracker: OAHCRRouteTracker; external name 'tracker';
  strict protected
    constructor create(para1: OAHCClientConnectionOperator; para2: OAHCRHttpRoute); overload;
  public
    function getState(): JLObject; overload; virtual;
    procedure setState(para1: JLObject); overload; virtual;
    procedure open(para1: OAHCRHttpRoute; para2: OAHPHttpContext; para3: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure tunnelTarget(para1: jboolean; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure tunnelProxy(para1: OAHHttpHost; para2: jboolean; para3: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure layerProtocol(para1: OAHPHttpContext; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
  strict protected
    procedure shutdownEntry(); overload; virtual;
  end;

  OAHICIdleConnectionHandler = class external 'org.apache.http.impl.conn' name 'IdleConnectionHandler' (JLObject)
  public
    constructor create(); overload;
    procedure add(para1: OAHHttpConnection; para2: jlong; para3: JUCTimeUnit); overload; virtual;
    function remove(para1: OAHHttpConnection): jboolean; overload; virtual;
    procedure removeAll(); overload; virtual;
    procedure closeIdleConnections(para1: jlong); overload; virtual;
    procedure closeExpiredConnections(); overload; virtual;
  end;

  OAHICWire = class external 'org.apache.http.impl.conn' name 'Wire' (JLObject)
  public
    constructor create(para1: OACLLog); overload;
    function enabled(): jboolean; overload; virtual;
    procedure output(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
    procedure input(para1: JIInputStream); overload; virtual;  // throws java.io.IOException
    procedure output(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure output(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure input(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure input(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure output(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure output(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure input(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure input(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure output(para1: jint); overload; virtual;  // throws java.io.IOException
    procedure input(para1: jint); overload; virtual;  // throws java.io.IOException
    procedure output(para1: JLString); overload; virtual;  // throws java.io.IOException
    procedure input(para1: JLString); overload; virtual;  // throws java.io.IOException
  end;

  OAHICTPoolEntryRequest = interface external 'org.apache.http.impl.conn.tsccm' name 'PoolEntryRequest' 
    function getPoolEntry(para1: jlong; para2: JUCTimeUnit): OAHICTBasicPoolEntry; overload;  // throws java.lang.InterruptedException, org.apache.http.conn.ConnectionPoolTimeoutException
    procedure abortRequest(); overload;
  end;

  OAHICTRefQueueHandler = interface external 'org.apache.http.impl.conn.tsccm' name 'RefQueueHandler' 
    procedure handleReference(para1: JLRReference); overload;
  end;

  OAHICTRouteSpecificPool = class external 'org.apache.http.impl.conn.tsccm' name 'RouteSpecificPool' (JLObject)
  strict protected
    final var
      froute: OAHCRHttpRoute; external name 'route';
      fmaxEntries: jint; external name 'maxEntries';
      ffreeEntries: JULinkedList; external name 'freeEntries';
      fwaitingThreads: JUQueue; external name 'waitingThreads';
  strict protected
    var
      fnumEntries: jint; external name 'numEntries';
  public
    constructor create(para1: OAHCRHttpRoute; para2: jint); overload;
    function getRoute(): OAHCRHttpRoute; overload; virtual; final;
    function getMaxEntries(): jint; overload; virtual; final;
    function isUnused(): jboolean; overload; virtual;
    function getCapacity(): jint; overload; virtual;
    function getEntryCount(): jint; overload; virtual; final;
    function allocEntry(para1: JLObject): OAHICTBasicPoolEntry; overload; virtual;
    procedure freeEntry(para1: OAHICTBasicPoolEntry); overload; virtual;
    procedure createdEntry(para1: OAHICTBasicPoolEntry); overload; virtual;
    function deleteEntry(para1: OAHICTBasicPoolEntry): jboolean; overload; virtual;
    procedure dropEntry(); overload; virtual;
    procedure queueThread(para1: OAHICTWaitingThread); overload; virtual;
    function hasThread(): jboolean; overload; virtual;
    function nextThread(): OAHICTWaitingThread; overload; virtual;
    procedure removeThread(para1: OAHICTWaitingThread); overload; virtual;
  end;

  OAHICTWaitingThread = class external 'org.apache.http.impl.conn.tsccm' name 'WaitingThread' (JLObject)
  public
    constructor create(para1: JUCLCondition; para2: OAHICTRouteSpecificPool); overload;
    function getCondition(): JUCLCondition; overload; virtual; final;
    function getPool(): OAHICTRouteSpecificPool; overload; virtual; final;
    function getThread(): JLThread; overload; virtual; final;
    function await(para1: JUDate): jboolean; overload; virtual;  // throws java.lang.InterruptedException
    procedure wakeup(); overload; virtual;
    procedure interrupt(); overload; virtual;
  end;

  OAHICTWaitingThreadAborter = class external 'org.apache.http.impl.conn.tsccm' name 'WaitingThreadAborter' (JLObject)
  public
    constructor create(); overload;
    procedure abort(); overload; virtual;
    procedure setWaitingThread(para1: OAHICTWaitingThread); overload; virtual;
  end;

  OAHICDateUtils = class sealed external 'org.apache.http.impl.cookie' name 'DateUtils' (JLObject)
  public
    const
      PATTERN_RFC1123 = 'EEE, dd MMM yyyy HH:mm:ss zzz';
      PATTERN_RFC1036 = 'EEEE, dd-MMM-yy HH:mm:ss zzz';
      PATTERN_ASCTIME = 'EEE MMM d HH:mm:ss yyyy';
  public
    final class var
      fGMT: JUTimeZone; external name 'GMT';
  public
    class function parseDate(para1: JLString): JUDate; static; overload;  // throws org.apache.http.impl.cookie.DateParseException
    class function parseDate(para1: JLString; para2: Arr1JLString): JUDate; static; overload;  // throws org.apache.http.impl.cookie.DateParseException
    class function parseDate(para1: JLString; var para2: array of JLString): JUDate; static; overload;  // throws org.apache.http.impl.cookie.DateParseException
    class function parseDate(para1: JLString; para2: Arr1JLString; para3: JUDate): JUDate; static; overload;  // throws org.apache.http.impl.cookie.DateParseException
    class function parseDate(para1: JLString; var para2: array of JLString; para3: JUDate): JUDate; static; overload;  // throws org.apache.http.impl.cookie.DateParseException
    class function formatDate(para1: JUDate): JLString; static; overload;
    class function formatDate(para1: JUDate; para2: JLString): JLString; static; overload;
  end;

  OAHICNetscapeDraftHeaderParser = class external 'org.apache.http.impl.cookie' name 'NetscapeDraftHeaderParser' (JLObject)
  public
    final class var
      fDEFAULT: OAHICNetscapeDraftHeaderParser; external name 'DEFAULT';
  public
    constructor create(); overload;
    function parseHeader(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHHeaderElement; overload; virtual;  // throws org.apache.http.ParseException
  end;

  OAHIEEntityDeserializer = class external 'org.apache.http.impl.entity' name 'EntityDeserializer' (JLObject)
  public
    constructor create(para1: OAHEContentLengthStrategy); overload;
  strict protected
    function doDeserialize(para1: OAHISessionInputBuffer; para2: OAHHttpMessage): OAHEBasicHttpEntity; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  public
    function deserialize(para1: OAHISessionInputBuffer; para2: OAHHttpMessage): OAHHttpEntity; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHIEEntitySerializer = class external 'org.apache.http.impl.entity' name 'EntitySerializer' (JLObject)
  public
    constructor create(para1: OAHEContentLengthStrategy); overload;
  strict protected
    function doSerialize(para1: OAHISessionOutputBuffer; para2: OAHHttpMessage): JIOutputStream; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  public
    procedure serialize(para1: OAHISessionOutputBuffer; para2: OAHHttpMessage; para3: OAHHttpEntity); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHIHttpMessageParser = interface external 'org.apache.http.io' name 'HttpMessageParser' 
    function parse(): OAHHttpMessage; overload;  // throws java.io.IOException, org.apache.http.HttpException
  end;

  OAHIHttpMessageWriter = interface external 'org.apache.http.io' name 'HttpMessageWriter' 
    procedure write(para1: OAHHttpMessage); overload;  // throws java.io.IOException, org.apache.http.HttpException
  end;

  OAHIHttpTransportMetrics = interface external 'org.apache.http.io' name 'HttpTransportMetrics' 
    function getBytesTransferred(): jlong; overload;
    procedure reset(); overload;
  end;

  OAHISessionInputBuffer = interface external 'org.apache.http.io' name 'SessionInputBuffer' 
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload;  // throws java.io.IOException
    function read(para1: Arr1jbyte): jint; overload;  // throws java.io.IOException
    function read(var para1: array of jbyte): jint; overload;  // throws java.io.IOException
    function read(): jint; overload;  // throws java.io.IOException
    function readLine(para1: OAHUCharArrayBuffer): jint; overload;  // throws java.io.IOException
    function readLine(): JLString; overload;  // throws java.io.IOException
    function isDataAvailable(para1: jint): jboolean; overload;  // throws java.io.IOException
    function getMetrics(): OAHIHttpTransportMetrics; overload;
  end;

  OAHISessionOutputBuffer = interface external 'org.apache.http.io' name 'SessionOutputBuffer' 
    procedure write(para1: Arr1jbyte; para2: jint; para3: jint); overload;  // throws java.io.IOException
    procedure write(var para1: array of jbyte; para2: jint; para3: jint); overload;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte); overload;  // throws java.io.IOException
    procedure write(var para1: array of jbyte); overload;  // throws java.io.IOException
    procedure write(para1: jint); overload;  // throws java.io.IOException
    procedure writeLine(para1: JLString); overload;  // throws java.io.IOException
    procedure writeLine(para1: OAHUCharArrayBuffer); overload;  // throws java.io.IOException
    procedure flush(); overload;  // throws java.io.IOException
    function getMetrics(): OAHIHttpTransportMetrics; overload;
  end;

  OAHMHeaderValueFormatter = interface external 'org.apache.http.message' name 'HeaderValueFormatter' 
    function formatElements(para1: OAHUCharArrayBuffer; para2: Arr1OAHHeaderElement; para3: jboolean): OAHUCharArrayBuffer; overload;
    function formatElements(para1: OAHUCharArrayBuffer; var para2: array of OAHHeaderElement; para3: jboolean): OAHUCharArrayBuffer; overload;
    function formatHeaderElement(para1: OAHUCharArrayBuffer; para2: OAHHeaderElement; para3: jboolean): OAHUCharArrayBuffer; overload;
    function formatParameters(para1: OAHUCharArrayBuffer; para2: Arr1OAHNameValuePair; para3: jboolean): OAHUCharArrayBuffer; overload;
    function formatParameters(para1: OAHUCharArrayBuffer; var para2: array of OAHNameValuePair; para3: jboolean): OAHUCharArrayBuffer; overload;
    function formatNameValuePair(para1: OAHUCharArrayBuffer; para2: OAHNameValuePair; para3: jboolean): OAHUCharArrayBuffer; overload;
  end;

  OAHMHeaderValueParser = interface external 'org.apache.http.message' name 'HeaderValueParser' 
    function parseElements(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): Arr1OAHHeaderElement; overload;  // throws org.apache.http.ParseException
    function parseHeaderElement(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHHeaderElement; overload;  // throws org.apache.http.ParseException
    function parseParameters(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): Arr1OAHNameValuePair; overload;  // throws org.apache.http.ParseException
    function parseNameValuePair(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHNameValuePair; overload;  // throws org.apache.http.ParseException
  end;

  OAHMLineFormatter = interface external 'org.apache.http.message' name 'LineFormatter' 
    function appendProtocolVersion(para1: OAHUCharArrayBuffer; para2: OAHProtocolVersion): OAHUCharArrayBuffer; overload;
    function formatRequestLine(para1: OAHUCharArrayBuffer; para2: OAHRequestLine): OAHUCharArrayBuffer; overload;
    function formatStatusLine(para1: OAHUCharArrayBuffer; para2: OAHStatusLine): OAHUCharArrayBuffer; overload;
    function formatHeader(para1: OAHUCharArrayBuffer; para2: OAHHeader): OAHUCharArrayBuffer; overload;
  end;

  OAHMLineParser = interface external 'org.apache.http.message' name 'LineParser' 
    function parseProtocolVersion(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHProtocolVersion; overload;  // throws org.apache.http.ParseException
    function hasProtocolVersion(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): jboolean; overload;
    function parseRequestLine(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHRequestLine; overload;  // throws org.apache.http.ParseException
    function parseStatusLine(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHStatusLine; overload;  // throws org.apache.http.ParseException
    function parseHeader(para1: OAHUCharArrayBuffer): OAHHeader; overload;  // throws org.apache.http.ParseException
  end;

  OAHMParserCursor = class external 'org.apache.http.message' name 'ParserCursor' (JLObject)
  public
    constructor create(para1: jint; para2: jint); overload;
    function getLowerBound(): jint; overload; virtual;
    function getUpperBound(): jint; overload; virtual;
    function getPos(): jint; overload; virtual;
    procedure updatePos(para1: jint); overload; virtual;
    function atEnd(): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHPCoreConnectionPNames = interface external 'org.apache.http.params' name 'CoreConnectionPNames' 
    const
      SO_TIMEOUT = 'http.socket.timeout';
      TCP_NODELAY = 'http.tcp.nodelay';
      SOCKET_BUFFER_SIZE = 'http.socket.buffer-size';
      SO_LINGER = 'http.socket.linger';
      CONNECTION_TIMEOUT = 'http.connection.timeout';
      STALE_CONNECTION_CHECK = 'http.connection.stalecheck';
      MAX_LINE_LENGTH = 'http.connection.max-line-length';
      MAX_HEADER_COUNT = 'http.connection.max-header-count';
  end;

  OAHPCoreProtocolPNames = interface external 'org.apache.http.params' name 'CoreProtocolPNames' 
    const
      PROTOCOL_VERSION = 'http.protocol.version';
      HTTP_ELEMENT_CHARSET = 'http.protocol.element-charset';
      HTTP_CONTENT_CHARSET = 'http.protocol.content-charset';
      USER_AGENT = 'http.useragent';
      ORIGIN_SERVER = 'http.origin-server';
      STRICT_TRANSFER_ENCODING = 'http.protocol.strict-transfer-encoding';
      USE_EXPECT_CONTINUE = 'http.protocol.expect-continue';
      WAIT_FOR_CONTINUE = 'http.protocol.wait-for-continue';
  end;

  OAHPHttpAbstractParamBean = class abstract external 'org.apache.http.params' name 'HttpAbstractParamBean' (JLObject)
  strict protected
    final var
      fparams: OAHPHttpParams; external name 'params';
  public
    constructor create(para1: OAHPHttpParams); overload;
  end;

  OAHPHttpParams = interface external 'org.apache.http.params' name 'HttpParams' 
    function getParameter(para1: JLString): JLObject; overload;
    function setParameter(para1: JLString; para2: JLObject): OAHPHttpParams; overload;
    function copy(): OAHPHttpParams; overload;
    function removeParameter(para1: JLString): jboolean; overload;
    function getLongParameter(para1: JLString; para2: jlong): jlong; overload;
    function setLongParameter(para1: JLString; para2: jlong): OAHPHttpParams; overload;
    function getIntParameter(para1: JLString; para2: jint): jint; overload;
    function setIntParameter(para1: JLString; para2: jint): OAHPHttpParams; overload;
    function getDoubleParameter(para1: JLString; para2: jdouble): jdouble; overload;
    function setDoubleParameter(para1: JLString; para2: jdouble): OAHPHttpParams; overload;
    function getBooleanParameter(para1: JLString; para2: jboolean): jboolean; overload;
    function setBooleanParameter(para1: JLString; para2: jboolean): OAHPHttpParams; overload;
    function isParameterTrue(para1: JLString): jboolean; overload;
    function isParameterFalse(para1: JLString): jboolean; overload;
  end;

  OAHPExecutionContext = interface external 'org.apache.http.protocol' name 'ExecutionContext' 
    const
      HTTP_CONNECTION = 'http.connection';
      HTTP_REQUEST = 'http.request';
      HTTP_RESPONSE = 'http.response';
      HTTP_TARGET_HOST = 'http.target_host';
      HTTP_PROXY_HOST = 'http.proxy_host';
      HTTP_REQ_SENT = 'http.request_sent';
  end;

  OAHPHTTP = class sealed external 'org.apache.http.protocol' name 'HTTP' (JLObject)
  public
    const
      CR = 13;
      LF = 10;
      SP = 32;
      HT = 9;
      TRANSFER_ENCODING = 'Transfer-Encoding';
      CONTENT_LEN = 'Content-Length';
      CONTENT_TYPE = 'Content-Type';
      CONTENT_ENCODING = 'Content-Encoding';
      EXPECT_DIRECTIVE = 'Expect';
      CONN_DIRECTIVE = 'Connection';
      TARGET_HOST = 'Host';
      USER_AGENT = 'User-Agent';
      DATE_HEADER = 'Date';
      SERVER_HEADER = 'Server';
      EXPECT_CONTINUE = '100-continue';
      CONN_CLOSE = 'Close';
      CONN_KEEP_ALIVE = 'Keep-Alive';
      CHUNK_CODING = 'chunked';
      IDENTITY_CODING = 'identity';
      UTF_8 = 'UTF-8';
      UTF_16 = 'UTF-16';
      US_ASCII = 'US-ASCII';
      ASCII = 'ASCII';
      ISO_8859_1 = 'ISO-8859-1';
      DEFAULT_CONTENT_CHARSET = 'ISO-8859-1';
      DEFAULT_PROTOCOL_CHARSET = 'US-ASCII';
      OCTET_STREAM_TYPE = 'application/octet-stream';
      PLAIN_TEXT_TYPE = 'text/plain';
      CHARSET_PARAM = '; charset=';
      DEFAULT_CONTENT_TYPE = 'application/octet-stream';
  public
    class function isWhitespace(para1: jchar): jboolean; static; overload;
  end;

  OAHPHttpContext = interface external 'org.apache.http.protocol' name 'HttpContext' 
    const
      RESERVED_PREFIX = 'http.';
    function getAttribute(para1: JLString): JLObject; overload;
    procedure setAttribute(para1: JLString; para2: JLObject); overload;
    function removeAttribute(para1: JLString): JLObject; overload;
  end;

  OAHPHttpDateGenerator = class external 'org.apache.http.protocol' name 'HttpDateGenerator' (JLObject)
  public
    const
      PATTERN_RFC1123 = 'EEE, dd MMM yyyy HH:mm:ss zzz';
  public
    final class var
      fGMT: JUTimeZone; external name 'GMT';
  public
    constructor create(); overload;
    function getCurrentDate(): JLString; overload; virtual;
  end;

  OAHPHttpExpectationVerifier = interface external 'org.apache.http.protocol' name 'HttpExpectationVerifier' 
    procedure verify(para1: OAHHttpRequest; para2: OAHHttpResponse; para3: OAHPHttpContext); overload;  // throws org.apache.http.HttpException
  end;

  OAHPHttpRequestExecutor = class external 'org.apache.http.protocol' name 'HttpRequestExecutor' (JLObject)
  public
    constructor create(); overload;
  strict protected
    function canResponseHaveBody(para1: OAHHttpRequest; para2: OAHHttpResponse): jboolean; overload; virtual;
  public
    function execute(para1: OAHHttpRequest; para2: OAHHttpClientConnection; para3: OAHPHttpContext): OAHHttpResponse; overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
    procedure preProcess(para1: OAHHttpRequest; para2: OAHPHttpProcessor; para3: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  strict protected
    function doSendRequest(para1: OAHHttpRequest; para2: OAHHttpClientConnection; para3: OAHPHttpContext): OAHHttpResponse; overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
    function doReceiveResponse(para1: OAHHttpRequest; para2: OAHHttpClientConnection; para3: OAHPHttpContext): OAHHttpResponse; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  public
    procedure postProcess(para1: OAHHttpResponse; para2: OAHPHttpProcessor; para3: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPHttpRequestHandler = interface external 'org.apache.http.protocol' name 'HttpRequestHandler' 
    procedure handle(para1: OAHHttpRequest; para2: OAHHttpResponse; para3: OAHPHttpContext); overload;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPHttpRequestHandlerResolver = interface external 'org.apache.http.protocol' name 'HttpRequestHandlerResolver' 
    function lookup(para1: JLString): OAHPHttpRequestHandler; overload;
  end;

  OAHPHttpRequestInterceptorList = interface external 'org.apache.http.protocol' name 'HttpRequestInterceptorList' 
    procedure addRequestInterceptor(para1: OAHHttpRequestInterceptor); overload;
    procedure addRequestInterceptor(para1: OAHHttpRequestInterceptor; para2: jint); overload;
    function getRequestInterceptorCount(): jint; overload;
    function getRequestInterceptor(para1: jint): OAHHttpRequestInterceptor; overload;
    procedure clearRequestInterceptors(); overload;
    procedure removeRequestInterceptorByClass(para1: JLClass); overload;
    procedure setInterceptors(para1: JUList); overload;
  end;

  OAHPHttpResponseInterceptorList = interface external 'org.apache.http.protocol' name 'HttpResponseInterceptorList' 
    procedure addResponseInterceptor(para1: OAHHttpResponseInterceptor); overload;
    procedure addResponseInterceptor(para1: OAHHttpResponseInterceptor; para2: jint); overload;
    function getResponseInterceptorCount(): jint; overload;
    function getResponseInterceptor(para1: jint): OAHHttpResponseInterceptor; overload;
    procedure clearResponseInterceptors(); overload;
    procedure removeResponseInterceptorByClass(para1: JLClass); overload;
    procedure setInterceptors(para1: JUList); overload;
  end;

  OAHPHttpService = class external 'org.apache.http.protocol' name 'HttpService' (JLObject)
  public
    constructor create(para1: OAHPHttpProcessor; para2: OAHConnectionReuseStrategy; para3: OAHHttpResponseFactory); overload;
    procedure setHttpProcessor(para1: OAHPHttpProcessor); overload; virtual;
    procedure setConnReuseStrategy(para1: OAHConnectionReuseStrategy); overload; virtual;
    procedure setResponseFactory(para1: OAHHttpResponseFactory); overload; virtual;
    procedure setHandlerResolver(para1: OAHPHttpRequestHandlerResolver); overload; virtual;
    procedure setExpectationVerifier(para1: OAHPHttpExpectationVerifier); overload; virtual;
    function getParams(): OAHPHttpParams; overload; virtual;
    procedure setParams(para1: OAHPHttpParams); overload; virtual;
    procedure handleRequest(para1: OAHHttpServerConnection; para2: OAHPHttpContext); overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
  strict protected
    procedure handleException(para1: OAHHttpException; para2: OAHHttpResponse); overload; virtual;
    procedure doService(para1: OAHHttpRequest; para2: OAHHttpResponse; para3: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPUriPatternMatcher = class external 'org.apache.http.protocol' name 'UriPatternMatcher' (JLObject)
  public
    constructor create(); overload;
    procedure register(para1: JLString; para2: JLObject); overload; virtual;
    procedure unregister(para1: JLString); overload; virtual;
    procedure setHandlers(para1: JUMap); overload; virtual;
    function lookup(para1: JLString): JLObject; overload; virtual;
  strict protected
    function matchUriRequestPattern(para1: JLString; para2: JLString): jboolean; overload; virtual;
  end;

  OAHUByteArrayBuffer = class sealed external 'org.apache.http.util' name 'ByteArrayBuffer' (JLObject)
  public
    constructor create(para1: jint); overload;
    procedure append(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;
    procedure append(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;
    procedure append(para1: jint); overload; virtual;
    procedure append(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;
    procedure append(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;
    procedure append(para1: OAHUCharArrayBuffer; para2: jint; para3: jint); overload; virtual;
    procedure clear(); overload; virtual;
    function toByteArray(): Arr1jbyte; overload; virtual;
    function byteAt(para1: jint): jint; overload; virtual;
    function capacity(): jint; overload; virtual;
    function length(): jint; overload; virtual;
    function buffer(): Arr1jbyte; overload; virtual;
    procedure setLength(para1: jint); overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function isFull(): jboolean; overload; virtual;
  end;

  OAHUCharArrayBuffer = class sealed external 'org.apache.http.util' name 'CharArrayBuffer' (JLObject)
  public
    constructor create(para1: jint); overload;
    procedure append(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;
    procedure append(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;
    procedure append(para1: JLString); overload; virtual;
    procedure append(para1: OAHUCharArrayBuffer; para2: jint; para3: jint); overload; virtual;
    procedure append(para1: OAHUCharArrayBuffer); overload; virtual;
    procedure append(para1: jchar); overload; virtual;
    procedure append(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;
    procedure append(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;
    procedure append(para1: OAHUByteArrayBuffer; para2: jint; para3: jint); overload; virtual;
    procedure append(para1: JLObject); overload; virtual;
    procedure clear(); overload; virtual;
    function toCharArray(): Arr1jchar; overload; virtual;
    function charAt(para1: jint): jchar; overload; virtual;
    function buffer(): Arr1jchar; overload; virtual;
    function capacity(): jint; overload; virtual;
    function length(): jint; overload; virtual;
    procedure ensureCapacity(para1: jint); overload; virtual;
    procedure setLength(para1: jint); overload; virtual;
    function isEmpty(): jboolean; overload; virtual;
    function isFull(): jboolean; overload; virtual;
    function indexOf(para1: jint; para2: jint; para3: jint): jint; overload; virtual;
    function indexOf(para1: jint): jint; overload; virtual;
    function substring(para1: jint; para2: jint): JLString; overload; virtual;
    function substringTrimmed(para1: jint; para2: jint): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHUEncodingUtils = class sealed external 'org.apache.http.util' name 'EncodingUtils' (JLObject)
  public
    class function getString(para1: Arr1jbyte; para2: jint; para3: jint; para4: JLString): JLString; static; overload;
    class function getString(var para1: array of jbyte; para2: jint; para3: jint; para4: JLString): JLString; static; overload;
    class function getString(para1: Arr1jbyte; para2: JLString): JLString; static; overload;
    class function getString(var para1: array of jbyte; para2: JLString): JLString; static; overload;
    class function getBytes(para1: JLString; para2: JLString): Arr1jbyte; static; overload;
    class function getAsciiBytes(para1: JLString): Arr1jbyte; static; overload;
    class function getAsciiString(para1: Arr1jbyte; para2: jint; para3: jint): JLString; static; overload;
    class function getAsciiString(var para1: array of jbyte; para2: jint; para3: jint): JLString; static; overload;
    class function getAsciiString(para1: Arr1jbyte): JLString; static; overload;
    class function getAsciiString(var para1: array of jbyte): JLString; static; overload;
  end;

  OAHUEntityUtils = class sealed external 'org.apache.http.util' name 'EntityUtils' (JLObject)
  public
    class function toByteArray(para1: OAHHttpEntity): Arr1jbyte; static; overload;  // throws java.io.IOException
    class function getContentCharSet(para1: OAHHttpEntity): JLString; static; overload;  // throws org.apache.http.ParseException
    class function toString(para1: OAHHttpEntity; para2: JLString): JLString; static; overload;  // throws java.io.IOException, org.apache.http.ParseException
    class function toString(para1: OAHHttpEntity): JLString; static; overload;  // throws java.io.IOException, org.apache.http.ParseException
  end;

  OAHUExceptionUtils = class sealed external 'org.apache.http.util' name 'ExceptionUtils' (JLObject)
  public
    class procedure initCause(para1: JLThrowable; para2: JLThrowable); static; overload;
  end;

  OAHULangUtils = class sealed external 'org.apache.http.util' name 'LangUtils' (JLObject)
  public
    const
      HASH_SEED = 17;
      HASH_OFFSET = 37;
  public
    class function hashCode(para1: jint; para2: jint): jint; static; overload;
    class function hashCode(para1: jint; para2: jboolean): jint; static; overload;
    class function hashCode(para1: jint; para2: JLObject): jint; static; overload;
    class function equals(para1: JLObject; para2: JLObject): jboolean; static; overload;
    class function equals(para1: Arr1JLObject; para2: Arr1JLObject): jboolean; static; overload;
    class function equals(var para1: array of JLObject; var para2: array of JLObject): jboolean; static; overload;
  end;

  OAHUVersionInfo = class external 'org.apache.http.util' name 'VersionInfo' (JLObject)
  public
    const
      UNAVAILABLE = 'UNAVAILABLE';
      VERSION_PROPERTY_FILE = 'version.properties';
      PROPERTY_MODULE = 'info.module';
      PROPERTY_RELEASE = 'info.release';
      PROPERTY_TIMESTAMP = 'info.timestamp';
  strict protected
    constructor create(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString); overload;
  public
    function getPackage(): JLString; overload; virtual; final;
    function getModule(): JLString; overload; virtual; final;
    function getRelease(): JLString; overload; virtual; final;
    function getTimestamp(): JLString; overload; virtual; final;
    function getClassloader(): JLString; overload; virtual; final;
    function toString(): JLString; overload; virtual;
    class function loadVersionInfo(para1: Arr1JLString; para2: JLClassLoader): Arr1OAHUVersionInfo; static; overload; final;
    class function loadVersionInfo(var para1: array of JLString; para2: JLClassLoader): Arr1OAHUVersionInfo; static; overload; final;
    class function loadVersionInfo(para1: JLString; para2: JLClassLoader): OAHUVersionInfo; static; overload; final;
  strict protected
    class function fromMap(para1: JLString; para2: JUMap; para3: JLClassLoader): OAHUVersionInfo; static; overload; final;
  end;

  OJJSONArray = class external 'org.json' name 'JSONArray' (JLObject)
  public
    constructor create(); overload;
    constructor create(para1: JUCollection); overload;
    constructor create(para1: OJJSONTokener); overload;  // throws org.json.JSONException
    constructor create(para1: JLString); overload;  // throws org.json.JSONException
    function length(): jint; overload; virtual;
    function put(para1: jboolean): OJJSONArray; overload; virtual;
    function put(para1: jdouble): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function put(para1: jint): OJJSONArray; overload; virtual;
    function put(para1: jlong): OJJSONArray; overload; virtual;
    function put(para1: JLObject): OJJSONArray; overload; virtual;
    function put(para1: jint; para2: jboolean): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function put(para1: jint; para2: jdouble): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function put(para1: jint; para2: jint): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function put(para1: jint; para2: jlong): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function put(para1: jint; para2: JLObject): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function isNull(para1: jint): jboolean; overload; virtual;
    function get(para1: jint): JLObject; overload; virtual;  // throws org.json.JSONException
    function opt(para1: jint): JLObject; overload; virtual;
    function getBoolean(para1: jint): jboolean; overload; virtual;  // throws org.json.JSONException
    function optBoolean(para1: jint): jboolean; overload; virtual;
    function optBoolean(para1: jint; para2: jboolean): jboolean; overload; virtual;
    function getDouble(para1: jint): jdouble; overload; virtual;  // throws org.json.JSONException
    function optDouble(para1: jint): jdouble; overload; virtual;
    function optDouble(para1: jint; para2: jdouble): jdouble; overload; virtual;
    function getInt(para1: jint): jint; overload; virtual;  // throws org.json.JSONException
    function optInt(para1: jint): jint; overload; virtual;
    function optInt(para1: jint; para2: jint): jint; overload; virtual;
    function getLong(para1: jint): jlong; overload; virtual;  // throws org.json.JSONException
    function optLong(para1: jint): jlong; overload; virtual;
    function optLong(para1: jint; para2: jlong): jlong; overload; virtual;
    function getString(para1: jint): JLString; overload; virtual;  // throws org.json.JSONException
    function optString(para1: jint): JLString; overload; virtual;
    function optString(para1: jint; para2: JLString): JLString; overload; virtual;
    function getJSONArray(para1: jint): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function optJSONArray(para1: jint): OJJSONArray; overload; virtual;
    function getJSONObject(para1: jint): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function optJSONObject(para1: jint): OJJSONObject; overload; virtual;
    function toJSONObject(para1: OJJSONArray): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function join(para1: JLString): JLString; overload; virtual;  // throws org.json.JSONException
    function toString(): JLString; overload; virtual;
    function toString(para1: jint): JLString; overload; virtual;  // throws org.json.JSONException
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  OJJSONObject = class external 'org.json' name 'JSONObject' (JLObject)
  public
    final class var
      fNULL: JLObject; external name 'NULL';
  public
    constructor create(); overload;
    constructor create(para1: JUMap); overload;
    constructor create(para1: OJJSONTokener); overload;  // throws org.json.JSONException
    constructor create(para1: JLString); overload;  // throws org.json.JSONException
    constructor create(para1: OJJSONObject; para2: Arr1JLString); overload;  // throws org.json.JSONException
    constructor create(para1: OJJSONObject; const para2: array of JLString); overload;  // throws org.json.JSONException
    function length(): jint; overload; virtual;
    function put(para1: JLString; para2: jboolean): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function put(para1: JLString; para2: jdouble): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function put(para1: JLString; para2: jint): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function put(para1: JLString; para2: jlong): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function put(para1: JLString; para2: JLObject): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function putOpt(para1: JLString; para2: JLObject): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function accumulate(para1: JLString; para2: JLObject): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function remove(para1: JLString): JLObject; overload; virtual;
    function isNull(para1: JLString): jboolean; overload; virtual;
    function has(para1: JLString): jboolean; overload; virtual;
    function get(para1: JLString): JLObject; overload; virtual;  // throws org.json.JSONException
    function opt(para1: JLString): JLObject; overload; virtual;
    function getBoolean(para1: JLString): jboolean; overload; virtual;  // throws org.json.JSONException
    function optBoolean(para1: JLString): jboolean; overload; virtual;
    function optBoolean(para1: JLString; para2: jboolean): jboolean; overload; virtual;
    function getDouble(para1: JLString): jdouble; overload; virtual;  // throws org.json.JSONException
    function optDouble(para1: JLString): jdouble; overload; virtual;
    function optDouble(para1: JLString; para2: jdouble): jdouble; overload; virtual;
    function getInt(para1: JLString): jint; overload; virtual;  // throws org.json.JSONException
    function optInt(para1: JLString): jint; overload; virtual;
    function optInt(para1: JLString; para2: jint): jint; overload; virtual;
    function getLong(para1: JLString): jlong; overload; virtual;  // throws org.json.JSONException
    function optLong(para1: JLString): jlong; overload; virtual;
    function optLong(para1: JLString; para2: jlong): jlong; overload; virtual;
    function getString(para1: JLString): JLString; overload; virtual;  // throws org.json.JSONException
    function optString(para1: JLString): JLString; overload; virtual;
    function optString(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getJSONArray(para1: JLString): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function optJSONArray(para1: JLString): OJJSONArray; overload; virtual;
    function getJSONObject(para1: JLString): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function optJSONObject(para1: JLString): OJJSONObject; overload; virtual;
    function toJSONArray(para1: OJJSONArray): OJJSONArray; overload; virtual;  // throws org.json.JSONException
    function keys(): JUIterator; overload; virtual;
    function names(): OJJSONArray; overload; virtual;
    function toString(): JLString; overload; virtual;
    function toString(para1: jint): JLString; overload; virtual;  // throws org.json.JSONException
    class function numberToString(para1: JLNumber): JLString; static; overload;  // throws org.json.JSONException
    class function quote(para1: JLString): JLString; static; overload;
  end;

  OJJSONStringer = class external 'org.json' name 'JSONStringer' (JLObject)
  public
    constructor create(); overload;
    function &array(): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function endArray(): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function &object(): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function endObject(): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function value(para1: JLObject): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function value(para1: jboolean): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function value(para1: jdouble): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function value(para1: jlong): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function key(para1: JLString): OJJSONStringer; overload; virtual;  // throws org.json.JSONException
    function toString(): JLString; overload; virtual;
  end;

  OJJSONTokener = class external 'org.json' name 'JSONTokener' (JLObject)
  public
    constructor create(para1: JLString); overload;
    function nextValue(): JLObject; overload; virtual;  // throws org.json.JSONException
    function nextString(para1: jchar): JLString; overload; virtual;  // throws org.json.JSONException
    function syntaxError(para1: JLString): OJJSONException; overload; virtual;
    function toString(): JLString; overload; virtual;
    function more(): jboolean; overload; virtual;
    function next(): jchar; overload; virtual;
    function next(para1: jchar): jchar; overload; virtual;  // throws org.json.JSONException
    function nextClean(): jchar; overload; virtual;  // throws org.json.JSONException
    function next(para1: jint): JLString; overload; virtual;  // throws org.json.JSONException
    function nextTo(para1: JLString): JLString; overload; virtual;
    function nextTo(para1: jchar): JLString; overload; virtual;
    procedure skipPast(para1: JLString); overload; virtual;
    function skipTo(para1: jchar): jchar; overload; virtual;
    procedure back(); overload; virtual;
    class function dehexchar(para1: jchar): jint; static; overload;
  end;

  OWDDOMConfiguration = interface external 'org.w3c.dom' name 'DOMConfiguration' 
    procedure setParameter(para1: JLString; para2: JLObject); overload;  // throws org.w3c.dom.DOMException
    function getParameter(para1: JLString): JLObject; overload;  // throws org.w3c.dom.DOMException
    function canSetParameter(para1: JLString; para2: JLObject): jboolean; overload;
    function getParameterNames(): OWDDOMStringList; overload;
  end;

  OWDDOMError = interface external 'org.w3c.dom' name 'DOMError' 
    const
      SEVERITY_WARNING = 1;
      SEVERITY_ERROR = 2;
      SEVERITY_FATAL_ERROR = 3;
    function getSeverity(): jshort; overload;
    function getMessage(): JLString; overload;
    function getType(): JLString; overload;
    function getRelatedException(): JLObject; overload;
    function getRelatedData(): JLObject; overload;
    function getLocation(): OWDDOMLocator; overload;
  end;

  OWDDOMErrorHandler = interface external 'org.w3c.dom' name 'DOMErrorHandler' 
    function handleError(para1: OWDDOMError): jboolean; overload;
  end;

  OWDDOMImplementation = interface external 'org.w3c.dom' name 'DOMImplementation' 
    function hasFeature(para1: JLString; para2: JLString): jboolean; overload;
    function createDocumentType(para1: JLString; para2: JLString; para3: JLString): OWDDocumentType; overload;  // throws org.w3c.dom.DOMException
    function createDocument(para1: JLString; para2: JLString; para3: OWDDocumentType): OWDDocument; overload;  // throws org.w3c.dom.DOMException
    function getFeature(para1: JLString; para2: JLString): JLObject; overload;
  end;

  OWDDOMImplementationList = interface external 'org.w3c.dom' name 'DOMImplementationList' 
    function item(para1: jint): OWDDOMImplementation; overload;
    function getLength(): jint; overload;
  end;

  OWDDOMImplementationSource = interface external 'org.w3c.dom' name 'DOMImplementationSource' 
    function getDOMImplementation(para1: JLString): OWDDOMImplementation; overload;
    function getDOMImplementationList(para1: JLString): OWDDOMImplementationList; overload;
  end;

  OWDDOMLocator = interface external 'org.w3c.dom' name 'DOMLocator' 
    function getLineNumber(): jint; overload;
    function getColumnNumber(): jint; overload;
    function getByteOffset(): jint; overload;
    function getUtf16Offset(): jint; overload;
    function getRelatedNode(): OWDNode; overload;
    function getUri(): JLString; overload;
  end;

  OWDDOMStringList = interface external 'org.w3c.dom' name 'DOMStringList' 
    function item(para1: jint): JLString; overload;
    function getLength(): jint; overload;
    function contains(para1: JLString): jboolean; overload;
  end;

  OWDNameList = interface external 'org.w3c.dom' name 'NameList' 
    function getName(para1: jint): JLString; overload;
    function getNamespaceURI(para1: jint): JLString; overload;
    function getLength(): jint; overload;
    function contains(para1: JLString): jboolean; overload;
    function containsNS(para1: JLString; para2: JLString): jboolean; overload;
  end;

  OWDNamedNodeMap = interface external 'org.w3c.dom' name 'NamedNodeMap' 
    function getNamedItem(para1: JLString): OWDNode; overload;
    function setNamedItem(para1: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function removeNamedItem(para1: JLString): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function item(para1: jint): OWDNode; overload;
    function getLength(): jint; overload;
    function getNamedItemNS(para1: JLString; para2: JLString): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function setNamedItemNS(para1: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function removeNamedItemNS(para1: JLString; para2: JLString): OWDNode; overload;  // throws org.w3c.dom.DOMException
  end;

  OWDNode = interface external 'org.w3c.dom' name 'Node' 
    const
      ELEMENT_NODE = 1;
      ATTRIBUTE_NODE = 2;
      TEXT_NODE = 3;
      CDATA_SECTION_NODE = 4;
      ENTITY_REFERENCE_NODE = 5;
      ENTITY_NODE = 6;
      PROCESSING_INSTRUCTION_NODE = 7;
      COMMENT_NODE = 8;
      DOCUMENT_NODE = 9;
      DOCUMENT_TYPE_NODE = 10;
      DOCUMENT_FRAGMENT_NODE = 11;
      NOTATION_NODE = 12;
      DOCUMENT_POSITION_DISCONNECTED = 1;
      DOCUMENT_POSITION_PRECEDING = 2;
      DOCUMENT_POSITION_FOLLOWING = 4;
      DOCUMENT_POSITION_CONTAINS = 8;
      DOCUMENT_POSITION_CONTAINED_BY = 16;
      DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 32;
    function getNodeName(): JLString; overload;
    function getNodeValue(): JLString; overload;  // throws org.w3c.dom.DOMException
    procedure setNodeValue(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function getNodeType(): jshort; overload;
    function getParentNode(): OWDNode; overload;
    function getChildNodes(): OWDNodeList; overload;
    function getFirstChild(): OWDNode; overload;
    function getLastChild(): OWDNode; overload;
    function getPreviousSibling(): OWDNode; overload;
    function getNextSibling(): OWDNode; overload;
    function getAttributes(): OWDNamedNodeMap; overload;
    function getOwnerDocument(): OWDDocument; overload;
    function insertBefore(para1: OWDNode; para2: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function replaceChild(para1: OWDNode; para2: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function removeChild(para1: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function appendChild(para1: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function hasChildNodes(): jboolean; overload;
    function cloneNode(para1: jboolean): OWDNode; overload;
    procedure normalize(); overload;
    function isSupported(para1: JLString; para2: JLString): jboolean; overload;
    function getNamespaceURI(): JLString; overload;
    function getPrefix(): JLString; overload;
    procedure setPrefix(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function getLocalName(): JLString; overload;
    function hasAttributes(): jboolean; overload;
    function getBaseURI(): JLString; overload;
    function compareDocumentPosition(para1: OWDNode): jshort; overload;  // throws org.w3c.dom.DOMException
    function getTextContent(): JLString; overload;  // throws org.w3c.dom.DOMException
    procedure setTextContent(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function isSameNode(para1: OWDNode): jboolean; overload;
    function lookupPrefix(para1: JLString): JLString; overload;
    function isDefaultNamespace(para1: JLString): jboolean; overload;
    function lookupNamespaceURI(para1: JLString): JLString; overload;
    function isEqualNode(para1: OWDNode): jboolean; overload;
    function getFeature(para1: JLString; para2: JLString): JLObject; overload;
    function setUserData(para1: JLString; para2: JLObject; para3: OWDUserDataHandler): JLObject; overload;
    function getUserData(para1: JLString): JLObject; overload;
  end;

  OWDNodeList = interface external 'org.w3c.dom' name 'NodeList' 
    function item(para1: jint): OWDNode; overload;
    function getLength(): jint; overload;
  end;

  OWDTypeInfo = interface external 'org.w3c.dom' name 'TypeInfo' 
    const
      DERIVATION_RESTRICTION = 1;
      DERIVATION_EXTENSION = 2;
      DERIVATION_UNION = 4;
      DERIVATION_LIST = 8;
    function getTypeName(): JLString; overload;
    function getTypeNamespace(): JLString; overload;
    function isDerivedFrom(para1: JLString; para2: JLString; para3: jint): jboolean; overload;
  end;

  OWDUserDataHandler = interface external 'org.w3c.dom' name 'UserDataHandler' 
    const
      NODE_CLONED = 1;
      NODE_IMPORTED = 2;
      NODE_DELETED = 3;
      NODE_RENAMED = 4;
      NODE_ADOPTED = 5;
    procedure handle(para1: jshort; para2: JLString; para3: JLObject; para4: OWDNode; para5: OWDNode); overload;
  end;

  OWDLDOMImplementationLS = interface external 'org.w3c.dom.ls' name 'DOMImplementationLS' 
    const
      MODE_SYNCHRONOUS = 1;
      MODE_ASYNCHRONOUS = 2;
    function createLSParser(para1: jshort; para2: JLString): OWDLLSParser; overload;  // throws org.w3c.dom.DOMException
    function createLSSerializer(): OWDLLSSerializer; overload;
    function createLSInput(): OWDLLSInput; overload;
    function createLSOutput(): OWDLLSOutput; overload;
  end;

  OWDLLSInput = interface external 'org.w3c.dom.ls' name 'LSInput' 
    function getCharacterStream(): JIReader; overload;
    procedure setCharacterStream(para1: JIReader); overload;
    function getByteStream(): JIInputStream; overload;
    procedure setByteStream(para1: JIInputStream); overload;
    function getStringData(): JLString; overload;
    procedure setStringData(para1: JLString); overload;
    function getSystemId(): JLString; overload;
    procedure setSystemId(para1: JLString); overload;
    function getPublicId(): JLString; overload;
    procedure setPublicId(para1: JLString); overload;
    function getBaseURI(): JLString; overload;
    procedure setBaseURI(para1: JLString); overload;
    function getEncoding(): JLString; overload;
    procedure setEncoding(para1: JLString); overload;
    function getCertifiedText(): jboolean; overload;
    procedure setCertifiedText(para1: jboolean); overload;
  end;

  OWDLLSOutput = interface external 'org.w3c.dom.ls' name 'LSOutput' 
    function getCharacterStream(): JIWriter; overload;
    procedure setCharacterStream(para1: JIWriter); overload;
    function getByteStream(): JIOutputStream; overload;
    procedure setByteStream(para1: JIOutputStream); overload;
    function getSystemId(): JLString; overload;
    procedure setSystemId(para1: JLString); overload;
    function getEncoding(): JLString; overload;
    procedure setEncoding(para1: JLString); overload;
  end;

  OWDLLSParser = interface external 'org.w3c.dom.ls' name 'LSParser' 
    const
      ACTION_APPEND_AS_CHILDREN = 1;
      ACTION_REPLACE_CHILDREN = 2;
      ACTION_INSERT_BEFORE = 3;
      ACTION_INSERT_AFTER = 4;
      ACTION_REPLACE = 5;
    function getDomConfig(): OWDDOMConfiguration; overload;
    function getFilter(): OWDLLSParserFilter; overload;
    procedure setFilter(para1: OWDLLSParserFilter); overload;
    function getAsync(): jboolean; overload;
    function getBusy(): jboolean; overload;
    function parse(para1: OWDLLSInput): OWDDocument; overload;  // throws org.w3c.dom.DOMException, org.w3c.dom.ls.LSException
    function parseURI(para1: JLString): OWDDocument; overload;  // throws org.w3c.dom.DOMException, org.w3c.dom.ls.LSException
    function parseWithContext(para1: OWDLLSInput; para2: OWDNode; para3: jshort): OWDNode; overload;  // throws org.w3c.dom.DOMException, org.w3c.dom.ls.LSException
    procedure abort(); overload;
  end;

  OWDLLSParserFilter = interface external 'org.w3c.dom.ls' name 'LSParserFilter' 
    const
      FILTER_ACCEPT = 1;
      FILTER_REJECT = 2;
      FILTER_SKIP = 3;
      FILTER_INTERRUPT = 4;
    function startElement(para1: OWDElement): jshort; overload;
    function acceptNode(para1: OWDNode): jshort; overload;
    function getWhatToShow(): jint; overload;
  end;

  OWDLLSResourceResolver = interface external 'org.w3c.dom.ls' name 'LSResourceResolver' 
    function resolveResource(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString): OWDLLSInput; overload;
  end;

  OWDLLSSerializer = interface external 'org.w3c.dom.ls' name 'LSSerializer' 
    function getDomConfig(): OWDDOMConfiguration; overload;
    function getNewLine(): JLString; overload;
    procedure setNewLine(para1: JLString); overload;
    function write(para1: OWDNode; para2: OWDLLSOutput): jboolean; overload;  // throws org.w3c.dom.ls.LSException
    function writeToURI(para1: OWDNode; para2: JLString): jboolean; overload;  // throws org.w3c.dom.ls.LSException
    function writeToString(para1: OWDNode): JLString; overload;  // throws org.w3c.dom.DOMException, org.w3c.dom.ls.LSException
  end;

  OXSAttributeList = interface external 'org.xml.sax' name 'AttributeList' 
    function getLength(): jint; overload;
    function getName(para1: jint): JLString; overload;
    function getType(para1: jint): JLString; overload;
    function getValue(para1: jint): JLString; overload;
    function getType(para1: JLString): JLString; overload;
    function getValue(para1: JLString): JLString; overload;
  end;

  OXSAttributes = interface external 'org.xml.sax' name 'Attributes' 
    function getLength(): jint; overload;
    function getURI(para1: jint): JLString; overload;
    function getLocalName(para1: jint): JLString; overload;
    function getQName(para1: jint): JLString; overload;
    function getType(para1: jint): JLString; overload;
    function getValue(para1: jint): JLString; overload;
    function getIndex(para1: JLString; para2: JLString): jint; overload;
    function getIndex(para1: JLString): jint; overload;
    function getType(para1: JLString; para2: JLString): JLString; overload;
    function getType(para1: JLString): JLString; overload;
    function getValue(para1: JLString; para2: JLString): JLString; overload;
    function getValue(para1: JLString): JLString; overload;
  end;

  OXSContentHandler = interface external 'org.xml.sax' name 'ContentHandler' 
    procedure setDocumentLocator(para1: OXSLocator); overload;
    procedure startDocument(); overload;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload;  // throws org.xml.sax.SAXException
    procedure startPrefixMapping(para1: JLString; para2: JLString); overload;  // throws org.xml.sax.SAXException
    procedure endPrefixMapping(para1: JLString); overload;  // throws org.xml.sax.SAXException
    procedure startElement(para1: JLString; para2: JLString; para3: JLString; para4: OXSAttributes); overload;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString; para2: JLString; para3: JLString); overload;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload;  // throws org.xml.sax.SAXException
    procedure skippedEntity(para1: JLString); overload;  // throws org.xml.sax.SAXException
  end;

  OXSDTDHandler = interface external 'org.xml.sax' name 'DTDHandler' 
    procedure notationDecl(para1: JLString; para2: JLString; para3: JLString); overload;  // throws org.xml.sax.SAXException
    procedure unparsedEntityDecl(para1: JLString; para2: JLString; para3: JLString; para4: JLString); overload;  // throws org.xml.sax.SAXException
  end;

  OXSDocumentHandler = interface external 'org.xml.sax' name 'DocumentHandler' 
    procedure setDocumentLocator(para1: OXSLocator); overload;
    procedure startDocument(); overload;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload;  // throws org.xml.sax.SAXException
    procedure startElement(para1: JLString; para2: OXSAttributeList); overload;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString); overload;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload;  // throws org.xml.sax.SAXException
  end;

  OXSEntityResolver = interface external 'org.xml.sax' name 'EntityResolver' 
    function resolveEntity(para1: JLString; para2: JLString): OXSInputSource; overload;  // throws org.xml.sax.SAXException, java.io.IOException
  end;

  OXSErrorHandler = interface external 'org.xml.sax' name 'ErrorHandler' 
    procedure warning(para1: OXSSAXParseException); overload;  // throws org.xml.sax.SAXException
    procedure error(para1: OXSSAXParseException); overload;  // throws org.xml.sax.SAXException
    procedure fatalError(para1: OXSSAXParseException); overload;  // throws org.xml.sax.SAXException
  end;

  OXSInputSource = class external 'org.xml.sax' name 'InputSource' (JLObject)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JIInputStream); overload;
    constructor create(para1: JIReader); overload;
    procedure setPublicId(para1: JLString); overload; virtual;
    function getPublicId(): JLString; overload; virtual;
    procedure setSystemId(para1: JLString); overload; virtual;
    function getSystemId(): JLString; overload; virtual;
    procedure setByteStream(para1: JIInputStream); overload; virtual;
    function getByteStream(): JIInputStream; overload; virtual;
    procedure setEncoding(para1: JLString); overload; virtual;
    function getEncoding(): JLString; overload; virtual;
    procedure setCharacterStream(para1: JIReader); overload; virtual;
    function getCharacterStream(): JIReader; overload; virtual;
  end;

  OXSLocator = interface external 'org.xml.sax' name 'Locator' 
    function getPublicId(): JLString; overload;
    function getSystemId(): JLString; overload;
    function getLineNumber(): jint; overload;
    function getColumnNumber(): jint; overload;
  end;

  OXSParser = interface external 'org.xml.sax' name 'Parser' 
    procedure setLocale(para1: JULocale); overload;  // throws org.xml.sax.SAXException
    procedure setEntityResolver(para1: OXSEntityResolver); overload;
    procedure setDTDHandler(para1: OXSDTDHandler); overload;
    procedure setDocumentHandler(para1: OXSDocumentHandler); overload;
    procedure setErrorHandler(para1: OXSErrorHandler); overload;
    procedure parse(para1: OXSInputSource); overload;  // throws org.xml.sax.SAXException, java.io.IOException
    procedure parse(para1: JLString); overload;  // throws org.xml.sax.SAXException, java.io.IOException
  end;

  OXSXMLReader = interface external 'org.xml.sax' name 'XMLReader' 
    function getFeature(para1: JLString): jboolean; overload;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setFeature(para1: JLString; para2: jboolean); overload;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    function getProperty(para1: JLString): JLObject; overload;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setProperty(para1: JLString; para2: JLObject); overload;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setEntityResolver(para1: OXSEntityResolver); overload;
    function getEntityResolver(): OXSEntityResolver; overload;
    procedure setDTDHandler(para1: OXSDTDHandler); overload;
    function getDTDHandler(): OXSDTDHandler; overload;
    procedure setContentHandler(para1: OXSContentHandler); overload;
    function getContentHandler(): OXSContentHandler; overload;
    procedure setErrorHandler(para1: OXSErrorHandler); overload;
    function getErrorHandler(): OXSErrorHandler; overload;
    procedure parse(para1: OXSInputSource); overload;  // throws java.io.IOException, org.xml.sax.SAXException
    procedure parse(para1: JLString); overload;  // throws java.io.IOException, org.xml.sax.SAXException
  end;

  OXSEDeclHandler = interface external 'org.xml.sax.ext' name 'DeclHandler' 
    procedure elementDecl(para1: JLString; para2: JLString); overload;  // throws org.xml.sax.SAXException
    procedure attributeDecl(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString); overload;  // throws org.xml.sax.SAXException
    procedure internalEntityDecl(para1: JLString; para2: JLString); overload;  // throws org.xml.sax.SAXException
    procedure externalEntityDecl(para1: JLString; para2: JLString; para3: JLString); overload;  // throws org.xml.sax.SAXException
  end;

  OXSELexicalHandler = interface external 'org.xml.sax.ext' name 'LexicalHandler' 
    procedure startDTD(para1: JLString; para2: JLString; para3: JLString); overload;  // throws org.xml.sax.SAXException
    procedure endDTD(); overload;  // throws org.xml.sax.SAXException
    procedure startEntity(para1: JLString); overload;  // throws org.xml.sax.SAXException
    procedure endEntity(para1: JLString); overload;  // throws org.xml.sax.SAXException
    procedure startCDATA(); overload;  // throws org.xml.sax.SAXException
    procedure endCDATA(); overload;  // throws org.xml.sax.SAXException
    procedure comment(para1: Arr1jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
    procedure comment(var para1: array of jchar; para2: jint; para3: jint); overload;  // throws org.xml.sax.SAXException
  end;

  OXSHNamespaceSupport = class external 'org.xml.sax.helpers' name 'NamespaceSupport' (JLObject)
  public
    const
      XMLNS = 'http://www.w3.org/XML/1998/namespace';
      NSDECL = 'http://www.w3.org/xmlns/2000/';
  public
    constructor create(); overload;
    procedure reset(); overload; virtual;
    procedure pushContext(); overload; virtual;
    procedure popContext(); overload; virtual;
    function declarePrefix(para1: JLString; para2: JLString): jboolean; overload; virtual;
    function processName(para1: JLString; para2: Arr1JLString; para3: jboolean): Arr1JLString; overload; virtual;
    function processName(para1: JLString; var para2: array of JLString; para3: jboolean): Arr1JLString; overload; virtual;
    function getURI(para1: JLString): JLString; overload; virtual;
    function getPrefixes(): JUEnumeration; overload; virtual;
    function getPrefix(para1: JLString): JLString; overload; virtual;
    function getPrefixes(para1: JLString): JUEnumeration; overload; virtual;
    function getDeclaredPrefixes(): JUEnumeration; overload; virtual;
    procedure setNamespaceDeclUris(para1: jboolean); overload; virtual;
    function isNamespaceDeclUris(): jboolean; overload; virtual;
  end;

  OXSHParserFactory = class external 'org.xml.sax.helpers' name 'ParserFactory' (JLObject)
  public
    class function makeParser(): OXSParser; static; overload;  // throws java.lang.ClassNotFoundException, java.lang.IllegalAccessException, java.lang.InstantiationException, java.lang.NullPointerException, java.lang.ClassCastException
    class function makeParser(para1: JLString): OXSParser; static; overload;  // throws java.lang.ClassNotFoundException, java.lang.IllegalAccessException, java.lang.InstantiationException, java.lang.ClassCastException
  end;

  OXSHXMLReaderFactory = class sealed external 'org.xml.sax.helpers' name 'XMLReaderFactory' (JLObject)
  public
    class function createXMLReader(): OXSXMLReader; static; overload;  // throws org.xml.sax.SAXException
    class function createXMLReader(para1: JLString): OXSXMLReader; static; overload;  // throws org.xml.sax.SAXException
  end;

  OXVXmlPullParser = interface external 'org.xmlpull.v1' name 'XmlPullParser' 
    const
      NO_NAMESPACE = '';
      START_DOCUMENT = 0;
      END_DOCUMENT = 1;
      START_TAG = 2;
      END_TAG = 3;
      TEXT = 4;
      CDSECT = 5;
      ENTITY_REF = 6;
      IGNORABLE_WHITESPACE = 7;
      PROCESSING_INSTRUCTION = 8;
      COMMENT = 9;
      DOCDECL = 10;
    final class var
      fTYPES: Arr1JLString; external name 'TYPES';
    const
      FEATURE_PROCESS_NAMESPACES = 'http://xmlpull.org/v1/doc/features.html#process-namespaces';
      FEATURE_REPORT_NAMESPACE_ATTRIBUTES = 'http://xmlpull.org/v1/doc/features.html#report-namespace-prefixes';
      FEATURE_PROCESS_DOCDECL = 'http://xmlpull.org/v1/doc/features.html#process-docdecl';
      FEATURE_VALIDATION = 'http://xmlpull.org/v1/doc/features.html#validation';
    procedure setFeature(para1: JLString; para2: jboolean); overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getFeature(para1: JLString): jboolean; overload;
    procedure setProperty(para1: JLString; para2: JLObject); overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getProperty(para1: JLString): JLObject; overload;
    procedure setInput(para1: JIReader); overload;  // throws org.xmlpull.v1.XmlPullParserException
    procedure setInput(para1: JIInputStream; para2: JLString); overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getInputEncoding(): JLString; overload;
    procedure defineEntityReplacementText(para1: JLString; para2: JLString); overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getNamespaceCount(para1: jint): jint; overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getNamespacePrefix(para1: jint): JLString; overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getNamespaceUri(para1: jint): JLString; overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getNamespace(para1: JLString): JLString; overload;
    function getDepth(): jint; overload;
    function getPositionDescription(): JLString; overload;
    function getLineNumber(): jint; overload;
    function getColumnNumber(): jint; overload;
    function isWhitespace(): jboolean; overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getText(): JLString; overload;
    function getTextCharacters(para1: Arr1jint): Arr1jchar; overload;
    function getTextCharacters(var para1: array of jint): Arr1jchar; overload;
    function getNamespace(): JLString; overload;
    function getName(): JLString; overload;
    function getPrefix(): JLString; overload;
    function isEmptyElementTag(): jboolean; overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getAttributeCount(): jint; overload;
    function getAttributeNamespace(para1: jint): JLString; overload;
    function getAttributeName(para1: jint): JLString; overload;
    function getAttributePrefix(para1: jint): JLString; overload;
    function getAttributeType(para1: jint): JLString; overload;
    function isAttributeDefault(para1: jint): jboolean; overload;
    function getAttributeValue(para1: jint): JLString; overload;
    function getAttributeValue(para1: JLString; para2: JLString): JLString; overload;
    function getEventType(): jint; overload;  // throws org.xmlpull.v1.XmlPullParserException
    function next(): jint; overload;  // throws org.xmlpull.v1.XmlPullParserException, java.io.IOException
    function nextToken(): jint; overload;  // throws org.xmlpull.v1.XmlPullParserException, java.io.IOException
    procedure require(para1: jint; para2: JLString; para3: JLString); overload;  // throws org.xmlpull.v1.XmlPullParserException, java.io.IOException
    function nextText(): JLString; overload;  // throws org.xmlpull.v1.XmlPullParserException, java.io.IOException
    function nextTag(): jint; overload;  // throws org.xmlpull.v1.XmlPullParserException, java.io.IOException
  end;

  OXVXmlPullParserFactory = class external 'org.xmlpull.v1' name 'XmlPullParserFactory' (JLObject)
  public
    const
      PROPERTY_NAME = 'org.xmlpull.v1.XmlPullParserFactory';
  strict protected
    var
      fparserClasses: JUArrayList; external name 'parserClasses';
      fclassNamesLocation: JLString; external name 'classNamesLocation';
      fserializerClasses: JUArrayList; external name 'serializerClasses';
      ffeatures: JUHashMap; external name 'features';
  strict protected
    constructor create(); overload;
  public
    procedure setFeature(para1: JLString; para2: jboolean); overload; virtual;  // throws org.xmlpull.v1.XmlPullParserException
    function getFeature(para1: JLString): jboolean; overload; virtual;
    procedure setNamespaceAware(para1: jboolean); overload; virtual;
    function isNamespaceAware(): jboolean; overload; virtual;
    procedure setValidating(para1: jboolean); overload; virtual;
    function isValidating(): jboolean; overload; virtual;
    function newPullParser(): OXVXmlPullParser; overload; virtual;  // throws org.xmlpull.v1.XmlPullParserException
    function newSerializer(): OXVXmlSerializer; overload; virtual;  // throws org.xmlpull.v1.XmlPullParserException
    class function newInstance(): OXVXmlPullParserFactory; static; overload;  // throws org.xmlpull.v1.XmlPullParserException
    class function newInstance(para1: JLString; para2: JLClass): OXVXmlPullParserFactory; static; overload;  // throws org.xmlpull.v1.XmlPullParserException
  end;

  OXVXmlSerializer = interface external 'org.xmlpull.v1' name 'XmlSerializer' 
    procedure setFeature(para1: JLString; para2: jboolean); overload;  // throws java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function getFeature(para1: JLString): jboolean; overload;
    procedure setProperty(para1: JLString; para2: JLObject); overload;  // throws java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function getProperty(para1: JLString): JLObject; overload;
    procedure setOutput(para1: JIOutputStream; para2: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure setOutput(para1: JIWriter); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure startDocument(para1: JLString; para2: JLBoolean); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure endDocument(); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure setPrefix(para1: JLString; para2: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function getPrefix(para1: JLString; para2: jboolean): JLString; overload;  // throws java.lang.IllegalArgumentException
    function getDepth(): jint; overload;
    function getNamespace(): JLString; overload;
    function getName(): JLString; overload;
    function startTag(para1: JLString; para2: JLString): OXVXmlSerializer; overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function attribute(para1: JLString; para2: JLString; para3: JLString): OXVXmlSerializer; overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function endTag(para1: JLString; para2: JLString): OXVXmlSerializer; overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function text(para1: JLString): OXVXmlSerializer; overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function text(para1: Arr1jchar; para2: jint; para3: jint): OXVXmlSerializer; overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    function text(var para1: array of jchar; para2: jint; para3: jint): OXVXmlSerializer; overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure cdsect(para1: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure entityRef(para1: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure processingInstruction(para1: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure comment(para1: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure docdecl(para1: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure ignorableWhitespace(para1: JLString); overload;  // throws java.io.IOException, java.lang.IllegalArgumentException, java.lang.IllegalStateException
    procedure flush(); overload;  // throws java.io.IOException
  end;

  OACAPluginResult = class external 'org.apache.cordova.api' name 'PluginResult' (JLObject)
  public
    type
      InnerStatus = class;
      Arr1InnerStatus = array of InnerStatus;
      Arr2InnerStatus = array of Arr1InnerStatus;
      Arr3InnerStatus = array of Arr2InnerStatus;
      InnerStatus = class sealed external 'org.apache.cordova.api' name 'Status' (JLEnum)
      public
        final class var
          fNO_RESULT: OACAPluginResult.InnerStatus; external name 'NO_RESULT';
          fOK: OACAPluginResult.InnerStatus; external name 'OK';
          fCLASS_NOT_FOUND_EXCEPTION: OACAPluginResult.InnerStatus; external name 'CLASS_NOT_FOUND_EXCEPTION';
          fILLEGAL_ACCESS_EXCEPTION: OACAPluginResult.InnerStatus; external name 'ILLEGAL_ACCESS_EXCEPTION';
          fINSTANTIATION_EXCEPTION: OACAPluginResult.InnerStatus; external name 'INSTANTIATION_EXCEPTION';
          fMALFORMED_URL_EXCEPTION: OACAPluginResult.InnerStatus; external name 'MALFORMED_URL_EXCEPTION';
          fIO_EXCEPTION: OACAPluginResult.InnerStatus; external name 'IO_EXCEPTION';
          fINVALID_ACTION: OACAPluginResult.InnerStatus; external name 'INVALID_ACTION';
          fJSON_EXCEPTION: OACAPluginResult.InnerStatus; external name 'JSON_EXCEPTION';
          fERROR: OACAPluginResult.InnerStatus; external name 'ERROR';
      public
        class function values(): OACAPluginResult.Arr1InnerStatus; static; overload;
        class function valueOf(para1: JLString): OACAPluginResult.InnerStatus; static; overload;
      end;

  public
    const
      MESSAGE_TYPE_STRING = 1;
      MESSAGE_TYPE_JSON = 2;
      MESSAGE_TYPE_NUMBER = 3;
      MESSAGE_TYPE_BOOLEAN = 4;
      MESSAGE_TYPE_NULL = 5;
      MESSAGE_TYPE_ARRAYBUFFER = 6;
  public
    class var
      fStatusMessages: Arr1JLString; external name 'StatusMessages';
  public
    constructor create(para1: OACAPluginResult.InnerStatus); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: JLString); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: OJJSONArray); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: OJJSONObject); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: jint); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: jfloat); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: jboolean); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; para2: Arr1jbyte); overload;
    constructor create(para1: OACAPluginResult.InnerStatus; const para2: array of jbyte); overload;
    procedure setKeepCallback(para1: jboolean); overload; virtual;
    function getStatus(): jint; overload; virtual;
    function getMessageType(): jint; overload; virtual;
    function getMessage(): JLString; overload; virtual;
    function getStrMessage(): JLString; overload; virtual;
    function getKeepCallback(): jboolean; overload; virtual;
    function getJSONString(): JLString; overload; virtual;
    function toCallbackString(para1: JLString): JLString; overload; virtual;
    function toSuccessCallbackString(para1: JLString): JLString; overload; virtual;
    function toErrorCallbackString(para1: JLString): JLString; overload; virtual;
  end;

  OAHCRRouteInfo = interface external 'org.apache.http.conn.routing' name 'RouteInfo' 
    type
      InnerLayerType = class;
      Arr1InnerLayerType = array of InnerLayerType;
      Arr2InnerLayerType = array of Arr1InnerLayerType;
      Arr3InnerLayerType = array of Arr2InnerLayerType;
      InnerTunnelType = class;
      Arr1InnerTunnelType = array of InnerTunnelType;
      Arr2InnerTunnelType = array of Arr1InnerTunnelType;
      Arr3InnerTunnelType = array of Arr2InnerTunnelType;
      InnerLayerType = class sealed external 'org.apache.http.conn.routing' name 'LayerType' (JLEnum)
      public
        final class var
          fLAYERED: OAHCRRouteInfo.InnerLayerType; external name 'LAYERED';
          fPLAIN: OAHCRRouteInfo.InnerLayerType; external name 'PLAIN';
      public
        class function values(): OAHCRRouteInfo.Arr1InnerLayerType; static; overload;
        class function valueOf(para1: JLString): OAHCRRouteInfo.InnerLayerType; static; overload;
      end;

      InnerTunnelType = class sealed external 'org.apache.http.conn.routing' name 'TunnelType' (JLEnum)
      public
        final class var
          fPLAIN: OAHCRRouteInfo.InnerTunnelType; external name 'PLAIN';
          fTUNNELLED: OAHCRRouteInfo.InnerTunnelType; external name 'TUNNELLED';
      public
        class function values(): OAHCRRouteInfo.Arr1InnerTunnelType; static; overload;
        class function valueOf(para1: JLString): OAHCRRouteInfo.InnerTunnelType; static; overload;
      end;

    function getTargetHost(): OAHHttpHost; overload;
    function getLocalAddress(): JNInetAddress; overload;
    function getHopCount(): jint; overload;
    function getHopTarget(para1: jint): OAHHttpHost; overload;
    function getProxyHost(): OAHHttpHost; overload;
    function getTunnelType(): OAHCRRouteInfo.InnerTunnelType; overload;
    function isTunnelled(): jboolean; overload;
    function getLayerType(): OAHCRRouteInfo.InnerLayerType; overload;
    function isLayered(): jboolean; overload;
    function isSecure(): jboolean; overload;
  end;

  OACAudioPlayer = class external 'org.apache.cordova' name 'AudioPlayer' (JLObject, AMMediaPlayer.InnerOnCompletionListener, AMMediaPlayer.InnerOnPreparedListener, AMMediaPlayer.InnerOnErrorListener)
  public
    type
      InnerSTATE = class;
      Arr1InnerSTATE = array of InnerSTATE;
      Arr2InnerSTATE = array of Arr1InnerSTATE;
      Arr3InnerSTATE = array of Arr2InnerSTATE;
      InnerMODE = class;
      Arr1InnerMODE = array of InnerMODE;
      Arr2InnerMODE = array of Arr1InnerMODE;
      Arr3InnerMODE = array of Arr2InnerMODE;
      InnerSTATE = class sealed external 'org.apache.cordova' name 'STATE' (JLEnum)
      public
        final class var
          fMEDIA_NONE: OACAudioPlayer.InnerSTATE; external name 'MEDIA_NONE';
          fMEDIA_STARTING: OACAudioPlayer.InnerSTATE; external name 'MEDIA_STARTING';
          fMEDIA_RUNNING: OACAudioPlayer.InnerSTATE; external name 'MEDIA_RUNNING';
          fMEDIA_PAUSED: OACAudioPlayer.InnerSTATE; external name 'MEDIA_PAUSED';
          fMEDIA_STOPPED: OACAudioPlayer.InnerSTATE; external name 'MEDIA_STOPPED';
          fMEDIA_LOADING: OACAudioPlayer.InnerSTATE; external name 'MEDIA_LOADING';
      public
        class function values(): OACAudioPlayer.Arr1InnerSTATE; static; overload;
        class function valueOf(para1: JLString): OACAudioPlayer.InnerSTATE; static; overload;
      end;

      InnerMODE = class sealed external 'org.apache.cordova' name 'MODE' (JLEnum)
      public
        final class var
          fNONE: OACAudioPlayer.InnerMODE; external name 'NONE';
          fPLAY: OACAudioPlayer.InnerMODE; external name 'PLAY';
          fRECORD: OACAudioPlayer.InnerMODE; external name 'RECORD';
      public
        class function values(): OACAudioPlayer.Arr1InnerMODE; static; overload;
        class function valueOf(para1: JLString): OACAudioPlayer.InnerMODE; static; overload;
      end;

  public
    constructor create(para1: OACAudioHandler; para2: JLString; para3: JLString); overload;
    procedure destroy(); overload; virtual;
    procedure startRecording(para1: JLString); overload; virtual;
    procedure moveFile(para1: JLString); overload; virtual;
    procedure stopRecording(); overload; virtual;
    procedure startPlaying(para1: JLString); overload; virtual;
    procedure seekToPlaying(para1: jint); overload; virtual;
    procedure pausePlaying(); overload; virtual;
    procedure stopPlaying(); overload; virtual;
    procedure onCompletion(para1: AMMediaPlayer); overload; virtual;
    function getCurrentPosition(): jlong; overload; virtual;
    function isStreaming(para1: JLString): jboolean; overload; virtual;
    function getDuration(para1: JLString): jfloat; overload; virtual;
    procedure onPrepared(para1: AMMediaPlayer); overload; virtual;
    function onError(para1: AMMediaPlayer; para2: jint; para3: jint): jboolean; overload; virtual;
    function getState(): jint; overload; virtual;
    procedure setVolume(para1: jfloat); overload; virtual;
  end;

  OACCordovaChromeClient = class external 'org.apache.cordova' name 'CordovaChromeClient' (AWWebChromeClient)
  public
    const
      FILECHOOSER_RESULTCODE = 5173;
  public
    var
      fmUploadMessage: AWValueCallback; external name 'mUploadMessage';
  public
    constructor create(para1: OACACordovaInterface); overload;
    constructor create(para1: OACACordovaInterface; para2: OACCordovaWebView); overload;
    procedure setWebView(para1: OACCordovaWebView); overload; virtual;
    function onJsAlert(para1: AWWebView; para2: JLString; para3: JLString; para4: AWJsResult): jboolean; overload; virtual;
    function onJsConfirm(para1: AWWebView; para2: JLString; para3: JLString; para4: AWJsResult): jboolean; overload; virtual;
    function onJsPrompt(para1: AWWebView; para2: JLString; para3: JLString; para4: JLString; para5: AWJsPromptResult): jboolean; overload; virtual;
    procedure onExceededDatabaseQuota(para1: JLString; para2: JLString; para3: jlong; para4: jlong; para5: jlong; para6: AWWebStorage.InnerQuotaUpdater); overload; virtual;
    procedure onConsoleMessage(para1: JLString; para2: jint; para3: JLString); overload; virtual;
    function onConsoleMessage(para1: AWConsoleMessage): jboolean; overload; virtual;
    procedure onGeolocationPermissionsShowPrompt(para1: JLString; para2: AWGeolocationPermissions.InnerCallback); overload; virtual;
    procedure onShowCustomView(para1: AVView; para2: AWWebChromeClient.InnerCustomViewCallback); overload; virtual;
    procedure onHideCustomView(); overload; virtual;
    function getVideoLoadingProgressView(): AVView; overload; virtual;
    procedure openFileChooser(para1: AWValueCallback); overload; virtual;
    procedure openFileChooser(para1: AWValueCallback; para2: JLString); overload; virtual;
    procedure openFileChooser(para1: AWValueCallback; para2: JLString; para3: JLString); overload; virtual;
    function getValueCallback(): AWValueCallback; overload; virtual;
  end;

  OACCordovaLocationListener = class external 'org.apache.cordova' name 'CordovaLocationListener' (JLObject, ALLocationListener)
  public
    class var
      fPERMISSION_DENIED: jint; external name 'PERMISSION_DENIED';
      fPOSITION_UNAVAILABLE: jint; external name 'POSITION_UNAVAILABLE';
      fTIMEOUT: jint; external name 'TIMEOUT';
  strict protected
    var
      flocationManager: ALLocationManager; external name 'locationManager';
      frunning: jboolean; external name 'running';
  public
    var
      fwatches: JUHashMap; external name 'watches';
  public
    constructor create(para1: ALLocationManager; para2: OACGeoBroker; para3: JLString); overload;
  strict protected
    procedure fail(para1: jint; para2: JLString); overload; virtual;
  public
    procedure onProviderDisabled(para1: JLString); overload; virtual;
    procedure onProviderEnabled(para1: JLString); overload; virtual;
    procedure onStatusChanged(para1: JLString; para2: jint; para3: AOBundle); overload; virtual;
    procedure onLocationChanged(para1: ALLocation); overload; virtual;
    function size(): jint; overload; virtual;
    procedure addWatch(para1: JLString; para2: OACACallbackContext); overload; virtual;
    procedure addCallback(para1: OACACallbackContext); overload; virtual;
    procedure clearWatch(para1: JLString); overload; virtual;
    procedure destroy(); overload; virtual;
  strict protected
    procedure start(); overload; virtual;
  end;

  OACCordovaWebView = class external 'org.apache.cordova' name 'CordovaWebView' (AWWebView)
  public
    type
      InnerLevel16Apis = class;
      Arr1InnerLevel16Apis = array of InnerLevel16Apis;
      Arr2InnerLevel16Apis = array of Arr1InnerLevel16Apis;
      Arr3InnerLevel16Apis = array of Arr2InnerLevel16Apis;
      InnerActivityResult = class;
      Arr1InnerActivityResult = array of InnerActivityResult;
      Arr2InnerActivityResult = array of Arr1InnerActivityResult;
      Arr3InnerActivityResult = array of Arr2InnerActivityResult;
      InnerLevel16Apis = class external 'org.apache.cordova' name 'Level16Apis' 
      end;

      InnerActivityResult = class external 'org.apache.cordova' name 'ActivityResult' 
      end;

  public
    const
      TAG = 'CordovaWebView';
  public
    var
      fpluginManager: OACAPluginManager; external name 'pluginManager';
  public
    constructor create(para1: ACContext); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet; para3: jint); overload;
    constructor create(para1: ACContext; para2: AUAttributeSet; para3: jint; para4: jboolean); overload;
    procedure setWebViewClient(para1: OACCordovaWebViewClient); overload; virtual;
    procedure setWebChromeClient(para1: OACCordovaChromeClient); overload; virtual;
    function getWebChromeClient(): OACCordovaChromeClient; overload; virtual;
    procedure loadUrl(para1: JLString); overload; virtual;
    procedure loadUrl(para1: JLString; para2: jint); overload; virtual;
    procedure loadUrlIntoView(para1: JLString); overload; virtual;
    procedure loadUrlIntoView(para1: JLString; para2: jint); overload; virtual;
    procedure sendJavascript(para1: JLString); overload; virtual;
    procedure sendPluginResult(para1: OACAPluginResult; para2: JLString); overload; virtual;
    procedure postMessage(para1: JLString; para2: JLObject); overload; virtual;
    function peekAtUrlStack(): JLString; overload; virtual;
    procedure pushUrl(para1: JLString); overload; virtual;
    function backHistory(): jboolean; overload; virtual;
    function canGoBack(): jboolean; overload; virtual;
    procedure showWebPage(para1: JLString; para2: jboolean; para3: jboolean; para4: JUHashMap); overload; virtual;
    function getProperty(para1: JLString; para2: JLString): JLString; overload; virtual;
    function onKeyDown(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    function onKeyUp(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    procedure bindButton(para1: jboolean); overload; virtual;
    procedure bindButton(para1: JLString; para2: jboolean); overload; virtual;
    procedure bindButton(para1: jint; para2: jboolean; para3: jboolean); overload; virtual;
    function isBackButtonBound(): jboolean; overload; virtual;
    procedure handlePause(para1: jboolean); overload; virtual;
    procedure handleResume(para1: jboolean; para2: jboolean); overload; virtual;
    procedure handleDestroy(); overload; virtual;
    procedure onNewIntent(para1: ACIntent); overload; virtual;
    function isPaused(): jboolean; overload; virtual;
    function hadKeyEvent(): jboolean; overload; virtual;
    procedure printBackForwardList(); overload; virtual;
    function startOfHistory(): jboolean; overload; virtual;
    procedure showCustomView(para1: AVView; para2: AWWebChromeClient.InnerCustomViewCallback); overload; virtual;
    procedure hideCustomView(); overload; virtual;
    function isCustomViewShowing(): jboolean; overload; virtual;
    function restoreState(para1: AOBundle): AWWebBackForwardList; overload; virtual;
    procedure storeResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
  end;

  OACCordovaWebViewClient = class external 'org.apache.cordova' name 'CordovaWebViewClient' (AWWebViewClient)
  public
    constructor create(para1: OACACordovaInterface); overload;
    constructor create(para1: OACACordovaInterface; para2: OACCordovaWebView); overload;
    procedure setWebView(para1: OACCordovaWebView); overload; virtual;
    function shouldOverrideUrlLoading(para1: AWWebView; para2: JLString): jboolean; overload; virtual;
    procedure onReceivedHttpAuthRequest(para1: AWWebView; para2: AWHttpAuthHandler; para3: JLString; para4: JLString); overload; virtual;
    procedure onPageStarted(para1: AWWebView; para2: JLString; para3: AGBitmap); overload; virtual;
    procedure onPageFinished(para1: AWWebView; para2: JLString); overload; virtual;
    procedure onReceivedError(para1: AWWebView; para2: jint; para3: JLString; para4: JLString); overload; virtual;
    procedure onReceivedSslError(para1: AWWebView; para2: AWSslErrorHandler; para3: ANHSslError); overload; virtual;
    procedure doUpdateVisitedHistory(para1: AWWebView; para2: JLString; para3: jboolean); overload; virtual;
    procedure setAuthenticationToken(para1: OACAuthenticationToken; para2: JLString; para3: JLString); overload; virtual;
    function removeAuthenticationToken(para1: JLString; para2: JLString): OACAuthenticationToken; overload; virtual;
    function getAuthenticationToken(para1: JLString; para2: JLString): OACAuthenticationToken; overload; virtual;
    procedure clearAuthenticationTokens(); overload; virtual;
  end;

  OACGlobalizationError = class external 'org.apache.cordova' name 'GlobalizationError' (JLException)
  public
    const
      UNKNOWN_ERROR = 'UNKNOWN_ERROR';
      FORMATTING_ERROR = 'FORMATTING_ERROR';
      PARSING_ERROR = 'PARSING_ERROR';
      PATTERN_ERROR = 'PATTERN_ERROR';
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    function getErrorString(): JLString; overload; virtual;
    function getErrorCode(): jint; overload; virtual;
    function toJson(): OJJSONObject; overload; virtual;
  end;

  OACFEncodingException = class external 'org.apache.cordova.file' name 'EncodingException' (JLException)
  public
    constructor create(para1: JLString); overload;
  end;

  OACFFileExistsException = class external 'org.apache.cordova.file' name 'FileExistsException' (JLException)
  public
    constructor create(para1: JLString); overload;
  end;

  OACFInvalidModificationException = class external 'org.apache.cordova.file' name 'InvalidModificationException' (JLException)
  public
    constructor create(para1: JLString); overload;
  end;

  OACFNoModificationAllowedException = class external 'org.apache.cordova.file' name 'NoModificationAllowedException' (JLException)
  public
    constructor create(para1: JLString); overload;
  end;

  OACFTypeMismatchException = class external 'org.apache.cordova.file' name 'TypeMismatchException' (JLException)
  public
    constructor create(para1: JLString); overload;
  end;

  OAHHttpException = class external 'org.apache.http' name 'HttpException' (JLException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHICDateParseException = class external 'org.apache.http.impl.cookie' name 'DateParseException' (JLException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OJJSONException = class external 'org.json' name 'JSONException' (JLException)
  public
    constructor create(para1: JLString); overload;
  end;

  OXSSAXException = class external 'org.xml.sax' name 'SAXException' (JLException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLException); overload;
    constructor create(para1: JLString; para2: JLException); overload;
    function getMessage(): JLString; overload; virtual;
    function getException(): JLException; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OXVXmlPullParserException = class external 'org.xmlpull.v1' name 'XmlPullParserException' (JLException)
  strict protected
    var
      fdetail: JLThrowable; external name 'detail';
      frow: jint; external name 'row';
      fcolumn: jint; external name 'column';
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: OXVXmlPullParser; para3: JLThrowable); overload;
    function getDetail(): JLThrowable; overload; virtual;
    function getLineNumber(): jint; overload; virtual;
    function getColumnNumber(): jint; overload; virtual;
    procedure printStackTrace(); overload; virtual;
  end;

  OACLinearLayoutSoftKeyboardDetect = class external 'org.apache.cordova' name 'LinearLayoutSoftKeyboardDetect' (AWLinearLayout)
  public
    constructor create(para1: ACContext; para2: jint; para3: jint); overload;
  strict protected
    procedure onMeasure(para1: jint; para2: jint); overload; virtual;
  end;

  OAHConnectionClosedException = class external 'org.apache.http' name 'ConnectionClosedException' (JIIOException)
  public
    constructor create(para1: JLString); overload;
  end;

  OAHMalformedChunkCodingException = class external 'org.apache.http' name 'MalformedChunkCodingException' (JIIOException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OAHNoHttpResponseException = class external 'org.apache.http' name 'NoHttpResponseException' (JIIOException)
  public
    constructor create(para1: JLString); overload;
  end;

  OAHCClientProtocolException = class external 'org.apache.http.client' name 'ClientProtocolException' (JIIOException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLThrowable); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHHeaderElementIterator = interface external 'org.apache.http' name 'HeaderElementIterator' (JUIterator)
    function hasNext(): jboolean; overload;
    function nextElement(): OAHHeaderElement; overload;
  end;

  OAHHeaderIterator = interface external 'org.apache.http' name 'HeaderIterator' (JUIterator)
    function hasNext(): jboolean; overload;
    function nextHeader(): OAHHeader; overload;
  end;

  OAHTokenIterator = interface external 'org.apache.http' name 'TokenIterator' (JUIterator)
    function hasNext(): jboolean; overload;
    function nextToken(): JLString; overload;
  end;

  OAHHttpHost = class sealed external 'org.apache.http' name 'HttpHost' (JLObject, JLCloneable)
  public
    const
      DEFAULT_SCHEME_NAME = 'http';
  strict protected
    final var
      fhostname: JLString; external name 'hostname';
      flcHostname: JLString; external name 'lcHostname';
      fport: jint; external name 'port';
      fschemeName: JLString; external name 'schemeName';
  public
    constructor create(para1: JLString; para2: jint; para3: JLString); overload;
    constructor create(para1: JLString; para2: jint); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: OAHHttpHost); overload;
    function getHostName(): JLString; overload; virtual;
    function getPort(): jint; overload; virtual;
    function getSchemeName(): JLString; overload; virtual;
    function toURI(): JLString; overload; virtual;
    function toHostString(): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHMHeaderGroup = class external 'org.apache.http.message' name 'HeaderGroup' (JLObject, JLCloneable)
  public
    constructor create(); overload;
    procedure clear(); overload; virtual;
    procedure addHeader(para1: OAHHeader); overload; virtual;
    procedure removeHeader(para1: OAHHeader); overload; virtual;
    procedure updateHeader(para1: OAHHeader); overload; virtual;
    procedure setHeaders(para1: Arr1OAHHeader); overload; virtual;
    procedure setHeaders(var para1: array of OAHHeader); overload; virtual;
    function getCondensedHeader(para1: JLString): OAHHeader; overload; virtual;
    function getHeaders(para1: JLString): Arr1OAHHeader; overload; virtual;
    function getFirstHeader(para1: JLString): OAHHeader; overload; virtual;
    function getLastHeader(para1: JLString): OAHHeader; overload; virtual;
    function getAllHeaders(): Arr1OAHHeader; overload; virtual;
    function containsHeader(para1: JLString): jboolean; overload; virtual;
    function iterator(): OAHHeaderIterator; overload; virtual;
    function iterator(para1: JLString): OAHHeaderIterator; overload; virtual;
    function copy(): OAHMHeaderGroup; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHProtocolVersion = class external 'org.apache.http' name 'ProtocolVersion' (JLObject, JISerializable, JLCloneable)
  strict protected
    final var
      fprotocol: JLString; external name 'protocol';
      fmajor: jint; external name 'major';
      fminor: jint; external name 'minor';
  public
    constructor create(para1: JLString; para2: jint; para3: jint); overload;
    function getProtocol(): JLString; overload; virtual; final;
    function getMajor(): jint; overload; virtual; final;
    function getMinor(): jint; overload; virtual; final;
    function forVersion(para1: jint; para2: jint): OAHProtocolVersion; overload; virtual;
    function hashCode(): jint; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function isComparable(para1: OAHProtocolVersion): jboolean; overload; virtual;
    function compareToVersion(para1: OAHProtocolVersion): jint; overload; virtual;
    function greaterEquals(para1: OAHProtocolVersion): jboolean; overload; virtual; final;
    function lessEquals(para1: OAHProtocolVersion): jboolean; overload; virtual; final;
    function toString(): JLString; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHParseException = class external 'org.apache.http' name 'ParseException' (JLRuntimeException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OAHIAUnsupportedDigestAlgorithmException = class external 'org.apache.http.impl.auth' name 'UnsupportedDigestAlgorithmException' (JLRuntimeException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OWDDOMException = class external 'org.w3c.dom' name 'DOMException' (JLRuntimeException)
  public
    var
      fcode: jshort; external name 'code';
  public
    const
      INDEX_SIZE_ERR = 1;
      DOMSTRING_SIZE_ERR = 2;
      HIERARCHY_REQUEST_ERR = 3;
      WRONG_DOCUMENT_ERR = 4;
      INVALID_CHARACTER_ERR = 5;
      NO_DATA_ALLOWED_ERR = 6;
      NO_MODIFICATION_ALLOWED_ERR = 7;
      NOT_FOUND_ERR = 8;
      NOT_SUPPORTED_ERR = 9;
      INUSE_ATTRIBUTE_ERR = 10;
      INVALID_STATE_ERR = 11;
      SYNTAX_ERR = 12;
      INVALID_MODIFICATION_ERR = 13;
      NAMESPACE_ERR = 14;
      INVALID_ACCESS_ERR = 15;
      VALIDATION_ERR = 16;
      TYPE_MISMATCH_ERR = 17;
  public
    constructor create(para1: jshort; para2: JLString); overload;
  end;

  OWDLLSException = class external 'org.w3c.dom.ls' name 'LSException' (JLRuntimeException)
  public
    var
      fcode: jshort; external name 'code';
  public
    const
      PARSE_ERR = 81;
      SERIALIZE_ERR = 82;
  public
    constructor create(para1: jshort; para2: JLString); overload;
  end;

  OAHABasicUserPrincipal = class sealed external 'org.apache.http.auth' name 'BasicUserPrincipal' (JLObject, JSPrincipal)
  public
    constructor create(para1: JLString); overload;
    function getName(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHANTUserPrincipal = class external 'org.apache.http.auth' name 'NTUserPrincipal' (JLObject, JSPrincipal)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    function getName(): JLString; overload; virtual;
    function getDomain(): JLString; overload; virtual;
    function getUsername(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHCConnectTimeoutException = class external 'org.apache.http.conn' name 'ConnectTimeoutException' (JIInterruptedIOException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OAHIIChunkedInputStream = class external 'org.apache.http.impl.io' name 'ChunkedInputStream' (JIInputStream)
  public
    constructor create(para1: OAHISessionInputBuffer); overload;
    function read(): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte): jint; overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
    function getFooters(): Arr1OAHHeader; overload; virtual;
  end;

  OAHIIContentLengthInputStream = class external 'org.apache.http.impl.io' name 'ContentLengthInputStream' (JIInputStream)
  public
    constructor create(para1: OAHISessionInputBuffer; para2: jlong); overload;
    procedure close(); overload; virtual;  // throws java.io.IOException
    function read(): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte): jint; overload; virtual;  // throws java.io.IOException
    function skip(para1: jlong): jlong; overload; virtual;  // throws java.io.IOException
  end;

  OAHIIIdentityInputStream = class external 'org.apache.http.impl.io' name 'IdentityInputStream' (JIInputStream)
  public
    constructor create(para1: OAHISessionInputBuffer); overload;
    function available(): jint; overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
    function read(): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
  end;

  OAHCHttpHostConnectException = class external 'org.apache.http.conn' name 'HttpHostConnectException' (JNConnectException)
  public
    constructor create(para1: OAHHttpHost; para2: JNConnectException); overload;
    function getHost(): OAHHttpHost; overload; virtual;
  end;

  OAHCSX509HostnameVerifier = interface external 'org.apache.http.conn.ssl' name 'X509HostnameVerifier' (JNSHostnameVerifier)
    function verify(para1: JLString; para2: JNSSSLSession): jboolean; overload;
    procedure verify(para1: JLString; para2: JNSSSLSocket); overload;  // throws java.io.IOException
    procedure verify(para1: JLString; para2: JSCX509Certificate); overload;  // throws javax.net.ssl.SSLException
    procedure verify(para1: JLString; para2: Arr1JLString; para3: Arr1JLString); overload;  // throws javax.net.ssl.SSLException
    procedure verify(para1: JLString; var para2: array of JLString; var para3: array of JLString); overload;  // throws javax.net.ssl.SSLException
  end;

  OAHCCookieIdentityComparator = class external 'org.apache.http.cookie' name 'CookieIdentityComparator' (JLObject, JISerializable, JUComparator)
  public
    constructor create(); overload;
    function compare(para1: OAHCCookie; para2: OAHCCookie): jint; overload; virtual;
    function compare(para1: JLObject; para2: JLObject): jint; overload; virtual;
  end;

  OAHCCookiePathComparator = class external 'org.apache.http.cookie' name 'CookiePathComparator' (JLObject, JISerializable, JUComparator)
  public
    constructor create(); overload;
    function compare(para1: OAHCCookie; para2: OAHCCookie): jint; overload; virtual;
    function compare(para1: JLObject; para2: JLObject): jint; overload; virtual;
  end;

  OAHICTBasicPoolEntryRef = class external 'org.apache.http.impl.conn.tsccm' name 'BasicPoolEntryRef' (JLRWeakReference)
  public
    constructor create(para1: OAHICTBasicPoolEntry; para2: JLRReferenceQueue); overload;
    function getRoute(): OAHCRHttpRoute; overload; virtual; final;
  end;

  OAHICTRefQueueWorker = class external 'org.apache.http.impl.conn.tsccm' name 'RefQueueWorker' (JLObject, JLRunnable)
  strict protected
    final var
      frefQueue: JLRReferenceQueue; external name 'refQueue';
      frefHandler: OAHICTRefQueueHandler; external name 'refHandler';
  strict protected
    var
      fworkerThread: JLThread; external name 'workerThread';
  public
    constructor create(para1: JLRReferenceQueue; para2: OAHICTRefQueueHandler); overload;
    procedure run(); overload; virtual;
    procedure shutdown(); overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHIIChunkedOutputStream = class external 'org.apache.http.impl.io' name 'ChunkedOutputStream' (JIOutputStream)
  public
    constructor create(para1: OAHISessionOutputBuffer; para2: jint); overload;  // throws java.io.IOException
    constructor create(para1: OAHISessionOutputBuffer); overload;  // throws java.io.IOException
  strict protected
    procedure flushCache(); overload; virtual;  // throws java.io.IOException
    procedure flushCacheWithAppend(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure flushCacheWithAppend(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure writeClosingChunk(); overload; virtual;  // throws java.io.IOException
  public
    procedure finish(); overload; virtual;  // throws java.io.IOException
    procedure write(para1: jint); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  end;

  OAHIIContentLengthOutputStream = class external 'org.apache.http.impl.io' name 'ContentLengthOutputStream' (JIOutputStream)
  public
    constructor create(para1: OAHISessionOutputBuffer; para2: jlong); overload;
    procedure close(); overload; virtual;  // throws java.io.IOException
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(para1: jint); overload; virtual;  // throws java.io.IOException
  end;

  OAHIIIdentityOutputStream = class external 'org.apache.http.impl.io' name 'IdentityOutputStream' (JIOutputStream)
  public
    constructor create(para1: OAHISessionOutputBuffer); overload;
    procedure close(); overload; virtual;  // throws java.io.IOException
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(para1: jint); overload; virtual;  // throws java.io.IOException
  end;

  OACContactAccessorSdk5 = class external 'org.apache.cordova' name 'ContactAccessorSdk5' (OACContactAccessor)
  public
    constructor create(para1: AWWebView; para2: OACACordovaInterface); overload;
    function search(para1: OJJSONArray; para2: OJJSONObject): OJJSONArray; overload; virtual;
    function getContactById(para1: JLString): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function save(para1: OJJSONObject): JLString; overload; virtual;
    function remove(para1: JLString): jboolean; overload; virtual;
  end;

  OACDroidGap = class external 'org.apache.cordova' name 'DroidGap' (AAActivity, OACACordovaInterface)
  public
    class var
      fTAG: JLString; external name 'TAG';
  strict protected
    var
      fappView: OACCordovaWebView; external name 'appView';
      fwebViewClient: OACCordovaWebViewClient; external name 'webViewClient';
      froot: AWLinearLayout; external name 'root';
      fcancelLoadUrl: jboolean; external name 'cancelLoadUrl';
      fspinnerDialog: AAProgressDialog; external name 'spinnerDialog';
      factivityResultCallback: OACACordovaPlugin; external name 'activityResultCallback';
      factivityResultKeepRunning: jboolean; external name 'activityResultKeepRunning';
      fsplashscreen: jint; external name 'splashscreen';
      fsplashscreenTime: jint; external name 'splashscreenTime';
      floadUrlTimeoutValue: jint; external name 'loadUrlTimeoutValue';
      fkeepRunning: jboolean; external name 'keepRunning';
      fsplashDialog: AADialog; external name 'splashDialog';
  public
    constructor create(); overload;
    procedure setAuthenticationToken(para1: OACAuthenticationToken; para2: JLString; para3: JLString); overload; virtual;
    function removeAuthenticationToken(para1: JLString; para2: JLString): OACAuthenticationToken; overload; virtual;
    function getAuthenticationToken(para1: JLString; para2: JLString): OACAuthenticationToken; overload; virtual;
    procedure clearAuthenticationTokens(); overload; virtual;
    procedure onCreate(para1: AOBundle); overload; virtual;
    function getActivity(): AAActivity; overload; virtual;
    procedure init(); overload; virtual;
    procedure init(para1: OACCordovaWebView; para2: OACCordovaWebViewClient; para3: OACCordovaChromeClient); overload; virtual;
    procedure loadUrl(para1: JLString); overload; virtual;
    procedure loadUrl(para1: JLString; para2: jint); overload; virtual;
    procedure cancelLoadUrl(); overload; virtual;
    procedure clearCache(); overload; virtual;
    procedure clearHistory(); overload; virtual;
    function backHistory(): jboolean; overload; virtual;
    procedure onConfigurationChanged(para1: ACRConfiguration); overload; virtual;
    function getBooleanProperty(para1: JLString; para2: jboolean): jboolean; overload; virtual;
    function getIntegerProperty(para1: JLString; para2: jint): jint; overload; virtual;
    function getStringProperty(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getDoubleProperty(para1: JLString; para2: jdouble): jdouble; overload; virtual;
    procedure setBooleanProperty(para1: JLString; para2: jboolean); overload; virtual;
    procedure setIntegerProperty(para1: JLString; para2: jint); overload; virtual;
    procedure setStringProperty(para1: JLString; para2: JLString); overload; virtual;
    procedure setDoubleProperty(para1: JLString; para2: jdouble); overload; virtual;
  strict protected
    procedure onPause(); overload; virtual;
    procedure onNewIntent(para1: ACIntent); overload; virtual;
    procedure onResume(); overload; virtual;
  public
    procedure onDestroy(); overload; virtual;
    procedure postMessage(para1: JLString; para2: JLObject); overload; virtual;
    procedure addService(para1: JLString; para2: JLString); overload; virtual;
    procedure sendJavascript(para1: JLString); overload; virtual;
    procedure spinnerStart(para1: JLString; para2: JLString); overload; virtual;
    procedure spinnerStop(); overload; virtual;
    procedure endActivity(); overload; virtual;
    procedure startActivityForResult(para1: OACACordovaPlugin; para2: ACIntent; para3: jint); overload; virtual;
  strict protected
    procedure onActivityResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
  public
    procedure setActivityResultCallback(para1: OACACordovaPlugin); overload; virtual;
    procedure onReceivedError(para1: jint; para2: JLString; para3: JLString); overload; virtual;
    procedure displayError(para1: JLString; para2: JLString; para3: JLString; para4: jboolean); overload; virtual;
    function isUrlWhiteListed(para1: JLString): jboolean; overload; virtual;
    function onCreateOptionsMenu(para1: AVMenu): jboolean; overload; virtual;
    function onPrepareOptionsMenu(para1: AVMenu): jboolean; overload; virtual;
    function onOptionsItemSelected(para1: AVMenuItem): jboolean; overload; virtual;
    function getContext(): ACContext; overload; virtual;
    procedure showWebPage(para1: JLString; para2: jboolean; para3: jboolean; para4: JUHashMap); overload; virtual;
    procedure removeSplashScreen(); overload; virtual;
  strict protected
    procedure showSplashScreen(para1: jint); overload; virtual;
  public
    function onKeyUp(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    function onKeyDown(para1: jint; para2: AVKeyEvent): jboolean; overload; virtual;
    function onMessage(para1: JLString; para2: JLObject): JLObject; overload; virtual;
    function getThreadPool(): JUCExecutorService; overload; virtual;
  strict protected
    procedure onSaveInstanceState(para1: AOBundle); overload; virtual;
  end;

  OACALegacyContext = class external 'org.apache.cordova.api' name 'LegacyContext' (JLObject, OACACordovaInterface)
  public
    constructor create(para1: OACACordovaInterface); overload;
    procedure cancelLoadUrl(); overload; virtual;
    function getActivity(): AAActivity; overload; virtual;
    function getContext(): ACContext; overload; virtual;
    function onMessage(para1: JLString; para2: JLObject): JLObject; overload; virtual;
    procedure setActivityResultCallback(para1: OACACordovaPlugin); overload; virtual;
    procedure startActivityForResult(para1: OACACordovaPlugin; para2: ACIntent; para3: jint); overload; virtual;
    procedure startActivity(para1: ACIntent); overload; virtual;
    function getSystemService(para1: JLString): JLObject; overload; virtual;
    function getAssets(): ACRAssetManager; overload; virtual;
    procedure runOnUiThread(para1: JLRunnable); overload; virtual;
    function getApplicationContext(): ACContext; overload; virtual;
    function getPackageManager(): ACPPackageManager; overload; virtual;
    function getSharedPreferences(para1: JLString; para2: jint): ACSharedPreferences; overload; virtual;
    procedure unregisterReceiver(para1: ACBroadcastReceiver); overload; virtual;
    function getResources(): ACRResources; overload; virtual;
    function startService(para1: ACIntent): ACComponentName; overload; virtual;
    function bindService(para1: ACIntent; para2: ACServiceConnection; para3: jint): jboolean; overload; virtual;
    procedure unbindService(para1: ACServiceConnection); overload; virtual;
    function getThreadPool(): JUCExecutorService; overload; virtual;
  end;

  OACAccelListener = class external 'org.apache.cordova' name 'AccelListener' (OACACordovaPlugin, AHSensorEventListener)
  public
    class var
      fSTOPPED: jint; external name 'STOPPED';
      fSTARTING: jint; external name 'STARTING';
      fRUNNING: jint; external name 'RUNNING';
      fERROR_FAILED_TO_START: jint; external name 'ERROR_FAILED_TO_START';
  public
    constructor create(); overload;
    procedure initialize(para1: OACACordovaInterface; para2: OACCordovaWebView); overload; virtual;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;
    procedure onDestroy(); overload; virtual;
    procedure onAccuracyChanged(para1: AHSensor; para2: jint); overload; virtual;
    procedure onSensorChanged(para1: AHSensorEvent); overload; virtual;
    procedure onReset(); overload; virtual;
  end;

  OACApp = class external 'org.apache.cordova' name 'App' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure clearCache(); overload; virtual;
    procedure loadUrl(para1: JLString; para2: OJJSONObject); overload; virtual;  // throws org.json.JSONException
    procedure clearHistory(); overload; virtual;
    procedure backHistory(); overload; virtual;
    procedure overrideBackbutton(para1: jboolean); overload; virtual;
    procedure overrideButton(para1: JLString; para2: jboolean); overload; virtual;
    function isBackbuttonOverridden(): jboolean; overload; virtual;
    procedure exitApp(); overload; virtual;
  end;

  OACAudioHandler = class external 'org.apache.cordova' name 'AudioHandler' (OACACordovaPlugin)
  public
    class var
      fTAG: JLString; external name 'TAG';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onDestroy(); overload; virtual;
    procedure onReset(); overload; virtual;
    function onMessage(para1: JLString; para2: JLObject): JLObject; overload; virtual;
    procedure startRecordingAudio(para1: JLString; para2: JLString); overload; virtual;
    procedure stopRecordingAudio(para1: JLString); overload; virtual;
    procedure startPlayingAudio(para1: JLString; para2: JLString); overload; virtual;
    procedure seekToAudio(para1: JLString; para2: jint); overload; virtual;
    procedure pausePlayingAudio(para1: JLString); overload; virtual;
    procedure stopPlayingAudio(para1: JLString); overload; virtual;
    function getCurrentPositionAudio(para1: JLString): jfloat; overload; virtual;
    function getDurationAudio(para1: JLString; para2: JLString): jfloat; overload; virtual;
    procedure setAudioOutputDevice(para1: jint); overload; virtual;
    function getAudioOutputDevice(): jint; overload; virtual;
    procedure setVolume(para1: JLString; para2: jfloat); overload; virtual;
  end;

  OACBatteryListener = class external 'org.apache.cordova' name 'BatteryListener' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;
    procedure onDestroy(); overload; virtual;
    procedure onReset(); overload; virtual;
  end;

  OACCameraLauncher = class external 'org.apache.cordova' name 'CameraLauncher' (OACACordovaPlugin, AMMediaScannerConnection.InnerMediaScannerConnectionClient)
  public
    var
      fcallbackContext: OACACallbackContext; external name 'callbackContext';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure takePicture(para1: jint; para2: jint); overload; virtual;
    procedure getImage(para1: jint; para2: jint); overload; virtual;
    procedure onActivityResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
    function calculateAspectRatio(para1: jint; para2: jint): Arr1jint; overload; virtual;
    class function calculateSampleSize(para1: jint; para2: jint; para3: jint; para4: jint): jint; static; overload;
    procedure processPicture(para1: AGBitmap); overload; virtual;
    procedure failPicture(para1: JLString); overload; virtual;
    procedure onMediaScannerConnected(); overload; virtual;
    procedure onScanCompleted(para1: JLString; para2: ANUri); overload; virtual;
  end;

  OACCapture = class external 'org.apache.cordova' name 'Capture' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onActivityResult(para1: jint; para2: jint; para3: ACIntent); overload; virtual;
    procedure fail(para1: OJJSONObject); overload; virtual;
  end;

  OACCompassListener = class external 'org.apache.cordova' name 'CompassListener' (OACACordovaPlugin, AHSensorEventListener)
  public
    class var
      fSTOPPED: jint; external name 'STOPPED';
      fSTARTING: jint; external name 'STARTING';
      fRUNNING: jint; external name 'RUNNING';
      fERROR_FAILED_TO_START: jint; external name 'ERROR_FAILED_TO_START';
  public
    var
      fTIMEOUT: jlong; external name 'TIMEOUT';
  public
    constructor create(); overload;
    procedure initialize(para1: OACACordovaInterface; para2: OACCordovaWebView); overload; virtual;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onDestroy(); overload; virtual;
    procedure onReset(); overload; virtual;
    function start(): jint; overload; virtual;
    procedure stop(); overload; virtual;
    procedure onAccuracyChanged(para1: AHSensor; para2: jint); overload; virtual;
    procedure onSensorChanged(para1: AHSensorEvent); overload; virtual;
    function getStatus(): jint; overload; virtual;
    function getHeading(): jfloat; overload; virtual;
    procedure setTimeout(para1: jlong); overload; virtual;
    function getTimeout(): jlong; overload; virtual;
  end;

  OACContactManager = class external 'org.apache.cordova' name 'ContactManager' (OACACordovaPlugin)
  public
    const
      UNKNOWN_ERROR = 0;
      INVALID_ARGUMENT_ERROR = 1;
      TIMEOUT_ERROR = 2;
      PENDING_OPERATION_ERROR = 3;
      IO_ERROR = 4;
      NOT_SUPPORTED_ERROR = 5;
      PERMISSION_DENIED_ERROR = 20;
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
  end;

  OACDevice = class external 'org.apache.cordova' name 'Device' (OACACordovaPlugin)
  public
    const
      TAG = 'Device';
  public
    class var
      fcordovaVersion: JLString; external name 'cordovaVersion';
      fplatform: JLString; external name 'platform';
      fuuid: JLString; external name 'uuid';
  public
    constructor create(); overload;
    procedure initialize(para1: OACACordovaInterface; para2: OACCordovaWebView); overload; virtual;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onDestroy(); overload; virtual;
    function getPlatform(): JLString; overload; virtual;
    function getUuid(): JLString; overload; virtual;
    function getCordovaVersion(): JLString; overload; virtual;
    function getModel(): JLString; overload; virtual;
    function getProductName(): JLString; overload; virtual;
    function getOSVersion(): JLString; overload; virtual;
    function getSDKVersion(): JLString; overload; virtual;
    function getTimeZoneID(): JLString; overload; virtual;
  end;

  OACEcho = class external 'org.apache.cordova' name 'Echo' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OACCordovaArgs; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
  end;

  OACFileTransfer = class external 'org.apache.cordova' name 'FileTransfer' (OACACordovaPlugin)
  public
    type
      InnerDoneHandlerInputStream = class;
      Arr1InnerDoneHandlerInputStream = array of InnerDoneHandlerInputStream;
      Arr2InnerDoneHandlerInputStream = array of Arr1InnerDoneHandlerInputStream;
      Arr3InnerDoneHandlerInputStream = array of Arr2InnerDoneHandlerInputStream;
      InnerRequestContext = class;
      Arr1InnerRequestContext = array of InnerRequestContext;
      Arr2InnerRequestContext = array of Arr1InnerRequestContext;
      Arr3InnerRequestContext = array of Arr2InnerRequestContext;
      InnerDoneHandlerInputStream = class sealed external 'org.apache.cordova' name 'DoneHandlerInputStream' 
      end;

      InnerRequestContext = class sealed external 'org.apache.cordova' name 'RequestContext' 
      end;

  public
    class var
      fFILE_NOT_FOUND_ERR: jint; external name 'FILE_NOT_FOUND_ERR';
      fINVALID_URL_ERR: jint; external name 'INVALID_URL_ERR';
      fCONNECTION_ERR: jint; external name 'CONNECTION_ERR';
      fABORTED_ERR: jint; external name 'ABORTED_ERR';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
  end;

  OACFileUtils = class external 'org.apache.cordova' name 'FileUtils' (OACACordovaPlugin)
  public
    class var
      fNOT_FOUND_ERR: jint; external name 'NOT_FOUND_ERR';
      fSECURITY_ERR: jint; external name 'SECURITY_ERR';
      fABORT_ERR: jint; external name 'ABORT_ERR';
      fNOT_READABLE_ERR: jint; external name 'NOT_READABLE_ERR';
      fENCODING_ERR: jint; external name 'ENCODING_ERR';
      fNO_MODIFICATION_ALLOWED_ERR: jint; external name 'NO_MODIFICATION_ALLOWED_ERR';
      fINVALID_STATE_ERR: jint; external name 'INVALID_STATE_ERR';
      fSYNTAX_ERR: jint; external name 'SYNTAX_ERR';
      fINVALID_MODIFICATION_ERR: jint; external name 'INVALID_MODIFICATION_ERR';
      fQUOTA_EXCEEDED_ERR: jint; external name 'QUOTA_EXCEEDED_ERR';
      fTYPE_MISMATCH_ERR: jint; external name 'TYPE_MISMATCH_ERR';
      fPATH_EXISTS_ERR: jint; external name 'PATH_EXISTS_ERR';
      fTEMPORARY: jint; external name 'TEMPORARY';
      fPERSISTENT: jint; external name 'PERSISTENT';
      fRESOURCE: jint; external name 'RESOURCE';
      fAPPLICATION: jint; external name 'APPLICATION';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    class function stripFileProtocol(para1: JLString): JLString; static; overload;
    function getEntry(para1: JIFile): OJJSONObject; overload; virtual;  // throws org.json.JSONException
    function isSynch(para1: JLString): jboolean; overload; virtual;
    function readAsText(para1: JLString; para2: JLString; para3: jint; para4: jint): JLString; overload; virtual;  // throws java.io.FileNotFoundException, java.io.IOException
    function readAsDataURL(para1: JLString; para2: jint; para3: jint): JLString; overload; virtual;  // throws java.io.FileNotFoundException, java.io.IOException
    class function getMimeType(para1: JLString): JLString; static; overload;
    function write(para1: JLString; para2: JLString; para3: jint): jlong; overload; virtual;  // throws java.io.FileNotFoundException, java.io.IOException
  strict protected
    class function getRealPathFromURI(para1: ANUri; para2: OACACordovaInterface): JLString; static; overload;
  end;

  OACGeoBroker = class external 'org.apache.cordova' name 'GeoBroker' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onDestroy(); overload; virtual;
    procedure onReset(); overload; virtual;
    function returnLocationJSON(para1: ALLocation): OJJSONObject; overload; virtual;
    procedure win(para1: ALLocation; para2: OACACallbackContext); overload; virtual;
    procedure fail(para1: jint; para2: JLString; para3: OACACallbackContext); overload; virtual;
    function isGlobalListener(para1: OACCordovaLocationListener): jboolean; overload; virtual;
  end;

  OACGlobalization = class external 'org.apache.cordova' name 'Globalization' (OACACordovaPlugin)
  public
    const
      GETLOCALENAME = 'getLocaleName';
      DATETOSTRING = 'dateToString';
      STRINGTODATE = 'stringToDate';
      GETDATEPATTERN = 'getDatePattern';
      GETDATENAMES = 'getDateNames';
      ISDAYLIGHTSAVINGSTIME = 'isDayLightSavingsTime';
      GETFIRSTDAYOFWEEK = 'getFirstDayOfWeek';
      NUMBERTOSTRING = 'numberToString';
      STRINGTONUMBER = 'stringToNumber';
      GETNUMBERPATTERN = 'getNumberPattern';
      GETCURRENCYPATTERN = 'getCurrencyPattern';
      GETPREFERREDLANGUAGE = 'getPreferredLanguage';
      OPTIONS = 'options';
      FORMATLENGTH = 'formatLength';
      MEDIUM = 'medium';
      LONG = 'long';
      FULL = 'full';
      SELECTOR = 'selector';
      DATE = 'date';
      TIME = 'time';
      DATESTRING = 'dateString';
      &TYPE = 'type';
      ITEM = 'item';
      NARROW = 'narrow';
      WIDE = 'wide';
      MONTHS = 'months';
      DAYS = 'days';
      NUMBER = 'number';
      NUMBERSTRING = 'numberString';
      PERCENT = 'percent';
      CURRENCY = 'currency';
      CURRENCYCODE = 'currencyCode';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;
  end;

  OACInAppBrowser = class external 'org.apache.cordova' name 'InAppBrowser' (OACACordovaPlugin)
  public
    type
      InnerInAppBrowserClient = class;
      Arr1InnerInAppBrowserClient = array of InnerInAppBrowserClient;
      Arr2InnerInAppBrowserClient = array of Arr1InnerInAppBrowserClient;
      Arr3InnerInAppBrowserClient = array of Arr2InnerInAppBrowserClient;
      InnerInAppBrowserClient = class external 'org.apache.cordova' name 'InAppBrowserClient' 
      end;

  strict protected
    const
      LOG_TAG = 'InAppBrowser';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    function openExternal(para1: JLString): JLString; overload; virtual;
    function showWebPage(para1: JLString; para2: JUHashMap): JLString; overload; virtual;
  end;

  OACNetworkManager = class external 'org.apache.cordova' name 'NetworkManager' (OACACordovaPlugin)
  public
    class var
      fNOT_REACHABLE: jint; external name 'NOT_REACHABLE';
      fREACHABLE_VIA_CARRIER_DATA_NETWORK: jint; external name 'REACHABLE_VIA_CARRIER_DATA_NETWORK';
      fREACHABLE_VIA_WIFI_NETWORK: jint; external name 'REACHABLE_VIA_WIFI_NETWORK';
  public
    const
      WIFI = 'wifi';
      WIMAX = 'wimax';
      MOBILE = 'mobile';
      GSM = 'gsm';
      GPRS = 'gprs';
      EDGE = 'edge';
      CDMA = 'cdma';
      UMTS = 'umts';
      HSPA = 'hspa';
      HSUPA = 'hsupa';
      HSDPA = 'hsdpa';
      ONEXRTT = '1xrtt';
      EHRPD = 'ehrpd';
      LTE = 'lte';
      UMB = 'umb';
      HSPA_PLUS = 'hspa+';
      TYPE_UNKNOWN = 'unknown';
      TYPE_ETHERNET = 'ethernet';
      TYPE_WIFI = 'wifi';
      TYPE_2G = '2g';
      TYPE_3G = '3g';
      TYPE_4G = '4g';
      TYPE_NONE = 'none';
  public
    constructor create(); overload;
    procedure initialize(para1: OACACordovaInterface; para2: OACCordovaWebView); overload; virtual;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;
    procedure onDestroy(); overload; virtual;
  end;

  OACNotification = class external 'org.apache.cordova' name 'Notification' (OACACordovaPlugin)
  public
    var
      fconfirmResult: jint; external name 'confirmResult';
      fspinnerDialog: AAProgressDialog; external name 'spinnerDialog';
      fprogressDialog: AAProgressDialog; external name 'progressDialog';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure beep(para1: jlong); overload; virtual;
    procedure vibrate(para1: jlong); overload; virtual;
    procedure alert(para1: JLString; para2: JLString; para3: JLString; para4: OACACallbackContext); overload; virtual;
    procedure confirm(para1: JLString; para2: JLString; para3: JLString; para4: OACACallbackContext); overload; virtual;
    procedure activityStart(para1: JLString; para2: JLString); overload; virtual;
    procedure activityStop(); overload; virtual;
    procedure progressStart(para1: JLString; para2: JLString); overload; virtual;
    procedure progressValue(para1: jint); overload; virtual;
    procedure progressStop(); overload; virtual;
  end;

  OACSplashScreen = class external 'org.apache.cordova' name 'SplashScreen' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;
  end;

  OACStorage = class external 'org.apache.cordova' name 'Storage' (OACACordovaPlugin)
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure onDestroy(); overload; virtual;
    procedure onReset(); overload; virtual;
    procedure openDatabase(para1: JLString; para2: JLString; para3: JLString; para4: jlong); overload; virtual;
    procedure executeSql(para1: JLString; para2: Arr1JLString; para3: JLString); overload; virtual;
    procedure executeSql(para1: JLString; var para2: array of JLString; para3: JLString); overload; virtual;
    procedure processResults(para1: ADCursor; para2: JLString); overload; virtual;
  end;

  OACAPlugin = class abstract external 'org.apache.cordova.api' name 'Plugin' (OACACordovaPlugin)
  public
    var
      fctx: OACALegacyContext; external name 'ctx';
  public
    constructor create(); overload;
    function execute(para1: JLString; para2: OJJSONArray; para3: JLString): OACAPluginResult; overload; virtual; abstract;
    function isSynch(para1: JLString): jboolean; overload; virtual;
    procedure initialize(para1: OACACordovaInterface; para2: OACCordovaWebView); overload; virtual;
    procedure setContext(para1: OACACordovaInterface); overload; virtual;
    procedure setView(para1: OACCordovaWebView); overload; virtual;
    function execute(para1: JLString; para2: OJJSONArray; para3: OACACallbackContext): jboolean; overload; virtual;  // throws org.json.JSONException
    procedure sendJavascript(para1: JLString); overload; virtual;
    procedure sendPluginResult(para1: OACAPluginResult; para2: JLString); overload; virtual;
    procedure success(para1: OACAPluginResult; para2: JLString); overload; virtual;
    procedure success(para1: OJJSONObject; para2: JLString); overload; virtual;
    procedure success(para1: JLString; para2: JLString); overload; virtual;
    procedure error(para1: OACAPluginResult; para2: JLString); overload; virtual;
    procedure error(para1: OJJSONObject; para2: JLString); overload; virtual;
    procedure error(para1: JLString; para2: JLString); overload; virtual;
  end;

  OAHIDefaultConnectionReuseStrategy = class external 'org.apache.http.impl' name 'DefaultConnectionReuseStrategy' (JLObject, OAHConnectionReuseStrategy)
  public
    constructor create(); overload;
    function keepAlive(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload; virtual;
  strict protected
    function createTokenIterator(para1: OAHHeaderIterator): OAHTokenIterator; overload; virtual;
  end;

  OAHINoConnectionReuseStrategy = class external 'org.apache.http.impl' name 'NoConnectionReuseStrategy' (JLObject, OAHConnectionReuseStrategy)
  public
    constructor create(); overload;
    function keepAlive(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload; virtual;
  end;

  OAHFormattedHeader = interface external 'org.apache.http' name 'FormattedHeader' (OAHHeader)
    function getBuffer(): OAHUCharArrayBuffer; overload;
    function getValuePos(): jint; overload;
  end;

  OAHMBasicHeader = class external 'org.apache.http.message' name 'BasicHeader' (JLObject, OAHHeader, JLCloneable)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    function getName(): JLString; overload; virtual;
    function getValue(): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function getElements(): Arr1OAHHeaderElement; overload; virtual;  // throws org.apache.http.ParseException
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHMBasicHeaderElement = class external 'org.apache.http.message' name 'BasicHeaderElement' (JLObject, OAHHeaderElement, JLCloneable)
  public
    constructor create(para1: JLString; para2: JLString; para3: Arr1OAHNameValuePair); overload;
    constructor create(para1: JLString; para2: JLString; const para3: array of OAHNameValuePair); overload;
    constructor create(para1: JLString; para2: JLString); overload;
    function getName(): JLString; overload; virtual;
    function getValue(): JLString; overload; virtual;
    function getParameters(): Arr1OAHNameValuePair; overload; virtual;
    function getParameterCount(): jint; overload; virtual;
    function getParameter(para1: jint): OAHNameValuePair; overload; virtual;
    function getParameterByName(para1: JLString): OAHNameValuePair; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function toString(): JLString; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHHttpClientConnection = interface external 'org.apache.http' name 'HttpClientConnection' (OAHHttpConnection)
    function isResponseAvailable(para1: jint): jboolean; overload;  // throws java.io.IOException
    procedure sendRequestHeader(para1: OAHHttpRequest); overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendRequestEntity(para1: OAHHttpEntityEnclosingRequest); overload;  // throws org.apache.http.HttpException, java.io.IOException
    function receiveResponseHeader(): OAHHttpResponse; overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure receiveResponseEntity(para1: OAHHttpResponse); overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure flush(); overload;  // throws java.io.IOException
  end;

  OAHHttpInetConnection = interface external 'org.apache.http' name 'HttpInetConnection' (OAHHttpConnection)
    function getLocalAddress(): JNInetAddress; overload;
    function getLocalPort(): jint; overload;
    function getRemoteAddress(): JNInetAddress; overload;
    function getRemotePort(): jint; overload;
  end;

  OAHHttpServerConnection = interface external 'org.apache.http' name 'HttpServerConnection' (OAHHttpConnection)
    function receiveRequestHeader(): OAHHttpRequest; overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure receiveRequestEntity(para1: OAHHttpEntityEnclosingRequest); overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendResponseHeader(para1: OAHHttpResponse); overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendResponseEntity(para1: OAHHttpResponse); overload;  // throws org.apache.http.HttpException, java.io.IOException
    procedure flush(); overload;  // throws java.io.IOException
  end;

  OAHIHttpConnectionMetricsImpl = class external 'org.apache.http.impl' name 'HttpConnectionMetricsImpl' (JLObject, OAHHttpConnectionMetrics)
  public
    const
      REQUEST_COUNT = 'http.request-count';
      RESPONSE_COUNT = 'http.response-count';
      SENT_BYTES_COUNT = 'http.sent-bytes-count';
      RECEIVED_BYTES_COUNT = 'http.received-bytes-count';
  public
    constructor create(para1: OAHIHttpTransportMetrics; para2: OAHIHttpTransportMetrics); overload;
    function getReceivedBytesCount(): jlong; overload; virtual;
    function getSentBytesCount(): jlong; overload; virtual;
    function getRequestCount(): jlong; overload; virtual;
    procedure incrementRequestCount(); overload; virtual;
    function getResponseCount(): jlong; overload; virtual;
    procedure incrementResponseCount(); overload; virtual;
    function getMetric(para1: JLString): JLObject; overload; virtual;
    procedure setMetric(para1: JLString; para2: JLObject); overload; virtual;
    procedure reset(); overload; virtual;
  end;

  OAHEAbstractHttpEntity = class abstract external 'org.apache.http.entity' name 'AbstractHttpEntity' (JLObject, OAHHttpEntity)
  strict protected
    var
      fcontentType: OAHHeader; external name 'contentType';
      fcontentEncoding: OAHHeader; external name 'contentEncoding';
      fchunked: jboolean; external name 'chunked';
  strict protected
    constructor create(); overload;
  public
    function getContentType(): OAHHeader; overload; virtual;
    function getContentEncoding(): OAHHeader; overload; virtual;
    function isChunked(): jboolean; overload; virtual;
    procedure setContentType(para1: OAHHeader); overload; virtual;
    procedure setContentType(para1: JLString); overload; virtual;
    procedure setContentEncoding(para1: OAHHeader); overload; virtual;
    procedure setContentEncoding(para1: JLString); overload; virtual;
    procedure setChunked(para1: jboolean); overload; virtual;
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException, java.lang.UnsupportedOperationException
  end;

  OAHEHttpEntityWrapper = class external 'org.apache.http.entity' name 'HttpEntityWrapper' (JLObject, OAHHttpEntity)
  strict protected
    var
      fwrappedEntity: OAHHttpEntity; external name 'wrappedEntity';
  public
    constructor create(para1: OAHHttpEntity); overload;
    function isRepeatable(): jboolean; overload; virtual;
    function isChunked(): jboolean; overload; virtual;
    function getContentLength(): jlong; overload; virtual;
    function getContentType(): OAHHeader; overload; virtual;
    function getContentEncoding(): OAHHeader; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException
  end;

  OAHHttpRequest = interface external 'org.apache.http' name 'HttpRequest' (OAHHttpMessage)
    function getRequestLine(): OAHRequestLine; overload;
  end;

  OAHHttpResponse = interface external 'org.apache.http' name 'HttpResponse' (OAHHttpMessage)
    function getStatusLine(): OAHStatusLine; overload;
    procedure setStatusLine(para1: OAHStatusLine); overload;
    procedure setStatusLine(para1: OAHProtocolVersion; para2: jint); overload;
    procedure setStatusLine(para1: OAHProtocolVersion; para2: jint; para3: JLString); overload;
    procedure setStatusCode(para1: jint); overload;  // throws java.lang.IllegalStateException
    procedure setReasonPhrase(para1: JLString); overload;  // throws java.lang.IllegalStateException
    function getEntity(): OAHHttpEntity; overload;
    procedure setEntity(para1: OAHHttpEntity); overload;
    function getLocale(): JULocale; overload;
    procedure setLocale(para1: JULocale); overload;
  end;

  OAHMAbstractHttpMessage = class abstract external 'org.apache.http.message' name 'AbstractHttpMessage' (JLObject, OAHHttpMessage)
  strict protected
    var
      fheadergroup: OAHMHeaderGroup; external name 'headergroup';
      fparams: OAHPHttpParams; external name 'params';
  strict protected
    constructor create(para1: OAHPHttpParams); overload;
    constructor create(); overload;
  public
    function containsHeader(para1: JLString): jboolean; overload; virtual;
    function getHeaders(para1: JLString): Arr1OAHHeader; overload; virtual;
    function getFirstHeader(para1: JLString): OAHHeader; overload; virtual;
    function getLastHeader(para1: JLString): OAHHeader; overload; virtual;
    function getAllHeaders(): Arr1OAHHeader; overload; virtual;
    procedure addHeader(para1: OAHHeader); overload; virtual;
    procedure addHeader(para1: JLString; para2: JLString); overload; virtual;
    procedure setHeader(para1: OAHHeader); overload; virtual;
    procedure setHeader(para1: JLString; para2: JLString); overload; virtual;
    procedure setHeaders(para1: Arr1OAHHeader); overload; virtual;
    procedure setHeaders(var para1: array of OAHHeader); overload; virtual;
    procedure removeHeader(para1: OAHHeader); overload; virtual;
    procedure removeHeaders(para1: JLString); overload; virtual;
    function headerIterator(): OAHHeaderIterator; overload; virtual;
    function headerIterator(para1: JLString): OAHHeaderIterator; overload; virtual;
    function getParams(): OAHPHttpParams; overload; virtual;
    procedure setParams(para1: OAHPHttpParams); overload; virtual;
  end;

  OAHIDefaultHttpRequestFactory = class external 'org.apache.http.impl' name 'DefaultHttpRequestFactory' (JLObject, OAHHttpRequestFactory)
  public
    constructor create(); overload;
    function newHttpRequest(para1: OAHRequestLine): OAHHttpRequest; overload; virtual;  // throws org.apache.http.MethodNotSupportedException
    function newHttpRequest(para1: JLString; para2: JLString): OAHHttpRequest; overload; virtual;  // throws org.apache.http.MethodNotSupportedException
  end;

  OAHCPRequestAddCookies = class external 'org.apache.http.client.protocol' name 'RequestAddCookies' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHCPRequestDefaultHeaders = class external 'org.apache.http.client.protocol' name 'RequestDefaultHeaders' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHCPRequestProxyAuthentication = class external 'org.apache.http.client.protocol' name 'RequestProxyAuthentication' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHCPRequestTargetAuthentication = class external 'org.apache.http.client.protocol' name 'RequestTargetAuthentication' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPRequestConnControl = class external 'org.apache.http.protocol' name 'RequestConnControl' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPRequestContent = class external 'org.apache.http.protocol' name 'RequestContent' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPRequestDate = class external 'org.apache.http.protocol' name 'RequestDate' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPRequestExpectContinue = class external 'org.apache.http.protocol' name 'RequestExpectContinue' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPRequestTargetHost = class external 'org.apache.http.protocol' name 'RequestTargetHost' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPRequestUserAgent = class external 'org.apache.http.protocol' name 'RequestUserAgent' (JLObject, OAHHttpRequestInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHIDefaultHttpResponseFactory = class external 'org.apache.http.impl' name 'DefaultHttpResponseFactory' (JLObject, OAHHttpResponseFactory)
  strict protected
    final var
      freasonCatalog: OAHReasonPhraseCatalog; external name 'reasonCatalog';
  public
    constructor create(para1: OAHReasonPhraseCatalog); overload;
    constructor create(); overload;
    function newHttpResponse(para1: OAHProtocolVersion; para2: jint; para3: OAHPHttpContext): OAHHttpResponse; overload; virtual;
    function newHttpResponse(para1: OAHStatusLine; para2: OAHPHttpContext): OAHHttpResponse; overload; virtual;
  strict protected
    function determineLocale(para1: OAHPHttpContext): JULocale; overload; virtual;
  end;

  OAHCPResponseProcessCookies = class external 'org.apache.http.client.protocol' name 'ResponseProcessCookies' (JLObject, OAHHttpResponseInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPHttpProcessor = interface external 'org.apache.http.protocol' name 'HttpProcessor' (OAHHttpRequestInterceptor, OAHHttpResponseInterceptor)
  end;

  OAHPResponseConnControl = class external 'org.apache.http.protocol' name 'ResponseConnControl' (JLObject, OAHHttpResponseInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPResponseContent = class external 'org.apache.http.protocol' name 'ResponseContent' (JLObject, OAHHttpResponseInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPResponseDate = class external 'org.apache.http.protocol' name 'ResponseDate' (JLObject, OAHHttpResponseInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHPResponseServer = class external 'org.apache.http.protocol' name 'ResponseServer' (JLObject, OAHHttpResponseInterceptor)
  public
    constructor create(); overload;
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHMBasicNameValuePair = class external 'org.apache.http.message' name 'BasicNameValuePair' (JLObject, OAHNameValuePair, JLCloneable)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    function getName(): JLString; overload; virtual;
    function getValue(): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHIEnglishReasonPhraseCatalog = class external 'org.apache.http.impl' name 'EnglishReasonPhraseCatalog' (JLObject, OAHReasonPhraseCatalog)
  public
    final class var
      fINSTANCE: OAHIEnglishReasonPhraseCatalog; external name 'INSTANCE';
  strict protected
    constructor create(); overload;
  public
    function getReason(para1: jint; para2: JULocale): JLString; overload; virtual;
  end;

  OAHMBasicRequestLine = class external 'org.apache.http.message' name 'BasicRequestLine' (JLObject, OAHRequestLine, JLCloneable)
  public
    constructor create(para1: JLString; para2: JLString; para3: OAHProtocolVersion); overload;
    function getMethod(): JLString; overload; virtual;
    function getProtocolVersion(): OAHProtocolVersion; overload; virtual;
    function getUri(): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHMBasicStatusLine = class external 'org.apache.http.message' name 'BasicStatusLine' (JLObject, OAHStatusLine, JLCloneable)
  public
    constructor create(para1: OAHProtocolVersion; para2: jint; para3: JLString); overload;
    function getStatusCode(): jint; overload; virtual;
    function getProtocolVersion(): OAHProtocolVersion; overload; virtual;
    function getReasonPhrase(): JLString; overload; virtual;
    function toString(): JLString; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHIAAuthSchemeBase = class abstract external 'org.apache.http.impl.auth' name 'AuthSchemeBase' (JLObject, OAHAAuthScheme)
  public
    constructor create(); overload;
    procedure processChallenge(para1: OAHHeader); overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
  strict protected
    procedure parseChallenge(para1: OAHUCharArrayBuffer; para2: jint; para3: jint); overload; virtual; abstract;  // throws org.apache.http.auth.MalformedChallengeException
  public
    function isProxy(): jboolean; overload; virtual;
  end;

  OAHIABasicSchemeFactory = class external 'org.apache.http.impl.auth' name 'BasicSchemeFactory' (JLObject, OAHAAuthSchemeFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHAAuthScheme; overload; virtual;
  end;

  OAHIADigestSchemeFactory = class external 'org.apache.http.impl.auth' name 'DigestSchemeFactory' (JLObject, OAHAAuthSchemeFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHAAuthScheme; overload; virtual;
  end;

  OAHANTCredentials = class external 'org.apache.http.auth' name 'NTCredentials' (JLObject, OAHACredentials)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLString; para3: JLString; para4: JLString); overload;
    function getUserPrincipal(): JSPrincipal; overload; virtual;
    function getUserName(): JLString; overload; virtual;
    function getPassword(): JLString; overload; virtual;
    function getDomain(): JLString; overload; virtual;
    function getWorkstation(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHAUsernamePasswordCredentials = class external 'org.apache.http.auth' name 'UsernamePasswordCredentials' (JLObject, OAHACredentials)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLString); overload;
    function getUserPrincipal(): JSPrincipal; overload; virtual;
    function getUserName(): JLString; overload; virtual;
    function getPassword(): JLString; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  OAHICAbstractAuthenticationHandler = class abstract external 'org.apache.http.impl.client' name 'AbstractAuthenticationHandler' (JLObject, OAHCAuthenticationHandler)
  public
    constructor create(); overload;
  strict protected
    function parseChallenges(para1: Arr1OAHHeader): JUMap; overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
    function parseChallenges(var para1: array of OAHHeader): JUMap; overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
    function getAuthPreferences(): JUList; overload; virtual;
  public
    function selectScheme(para1: JUMap; para2: OAHHttpResponse; para3: OAHPHttpContext): OAHAAuthScheme; overload; virtual;  // throws org.apache.http.auth.AuthenticationException
  end;

  OAHICBasicCookieStore = class external 'org.apache.http.impl.client' name 'BasicCookieStore' (JLObject, OAHCCookieStore)
  public
    constructor create(); overload;
    procedure addCookie(para1: OAHCCookie); overload; virtual;
    procedure addCookies(para1: Arr1OAHCCookie); overload; virtual;
    procedure addCookies(var para1: array of OAHCCookie); overload; virtual;
    function getCookies(): JUList; overload; virtual;
    function clearExpired(para1: JUDate): jboolean; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure clear(); overload; virtual;
  end;

  OAHICBasicCredentialsProvider = class external 'org.apache.http.impl.client' name 'BasicCredentialsProvider' (JLObject, OAHCCredentialsProvider)
  public
    constructor create(); overload;
    procedure setCredentials(para1: OAHAAuthScope; para2: OAHACredentials); overload; virtual;
    function getCredentials(para1: OAHAAuthScope): OAHACredentials; overload; virtual;
    function toString(): JLString; overload; virtual;
    procedure clear(); overload; virtual;
  end;

  OAHICAbstractHttpClient = class abstract external 'org.apache.http.impl.client' name 'AbstractHttpClient' (JLObject, OAHCHttpClient)
  strict protected
    constructor create(para1: OAHCClientConnectionManager; para2: OAHPHttpParams); overload;
    function createHttpParams(): OAHPHttpParams; overload; virtual; abstract;
    function createHttpContext(): OAHPHttpContext; overload; virtual; abstract;
    function createRequestExecutor(): OAHPHttpRequestExecutor; overload; virtual; abstract;
    function createClientConnectionManager(): OAHCClientConnectionManager; overload; virtual; abstract;
    function createAuthSchemeRegistry(): OAHAAuthSchemeRegistry; overload; virtual; abstract;
    function createCookieSpecRegistry(): OAHCCookieSpecRegistry; overload; virtual; abstract;
    function createConnectionReuseStrategy(): OAHConnectionReuseStrategy; overload; virtual; abstract;
    function createConnectionKeepAliveStrategy(): OAHCConnectionKeepAliveStrategy; overload; virtual; abstract;
    function createHttpProcessor(): OAHPBasicHttpProcessor; overload; virtual; abstract;
    function createHttpRequestRetryHandler(): OAHCHttpRequestRetryHandler; overload; virtual; abstract;
    function createRedirectHandler(): OAHCRedirectHandler; overload; virtual; abstract;
    function createTargetAuthenticationHandler(): OAHCAuthenticationHandler; overload; virtual; abstract;
    function createProxyAuthenticationHandler(): OAHCAuthenticationHandler; overload; virtual; abstract;
    function createCookieStore(): OAHCCookieStore; overload; virtual; abstract;
    function createCredentialsProvider(): OAHCCredentialsProvider; overload; virtual; abstract;
    function createHttpRoutePlanner(): OAHCRHttpRoutePlanner; overload; virtual; abstract;
    function createUserTokenHandler(): OAHCUserTokenHandler; overload; virtual; abstract;
  public
    function getParams(): OAHPHttpParams; overload; virtual; final;
    procedure setParams(para1: OAHPHttpParams); overload; virtual;
    function getConnectionManager(): OAHCClientConnectionManager; overload; virtual; final;
    function getRequestExecutor(): OAHPHttpRequestExecutor; overload; virtual; final;
    function getAuthSchemes(): OAHAAuthSchemeRegistry; overload; virtual; final;
    procedure setAuthSchemes(para1: OAHAAuthSchemeRegistry); overload; virtual;
    function getCookieSpecs(): OAHCCookieSpecRegistry; overload; virtual; final;
    procedure setCookieSpecs(para1: OAHCCookieSpecRegistry); overload; virtual;
    function getConnectionReuseStrategy(): OAHConnectionReuseStrategy; overload; virtual; final;
    procedure setReuseStrategy(para1: OAHConnectionReuseStrategy); overload; virtual;
    function getConnectionKeepAliveStrategy(): OAHCConnectionKeepAliveStrategy; overload; virtual; final;
    procedure setKeepAliveStrategy(para1: OAHCConnectionKeepAliveStrategy); overload; virtual;
    function getHttpRequestRetryHandler(): OAHCHttpRequestRetryHandler; overload; virtual; final;
    procedure setHttpRequestRetryHandler(para1: OAHCHttpRequestRetryHandler); overload; virtual;
    function getRedirectHandler(): OAHCRedirectHandler; overload; virtual; final;
    procedure setRedirectHandler(para1: OAHCRedirectHandler); overload; virtual;
    function getTargetAuthenticationHandler(): OAHCAuthenticationHandler; overload; virtual; final;
    procedure setTargetAuthenticationHandler(para1: OAHCAuthenticationHandler); overload; virtual;
    function getProxyAuthenticationHandler(): OAHCAuthenticationHandler; overload; virtual; final;
    procedure setProxyAuthenticationHandler(para1: OAHCAuthenticationHandler); overload; virtual;
    function getCookieStore(): OAHCCookieStore; overload; virtual; final;
    procedure setCookieStore(para1: OAHCCookieStore); overload; virtual;
    function getCredentialsProvider(): OAHCCredentialsProvider; overload; virtual; final;
    procedure setCredentialsProvider(para1: OAHCCredentialsProvider); overload; virtual;
    function getRoutePlanner(): OAHCRHttpRoutePlanner; overload; virtual; final;
    procedure setRoutePlanner(para1: OAHCRHttpRoutePlanner); overload; virtual;
    function getUserTokenHandler(): OAHCUserTokenHandler; overload; virtual; final;
    procedure setUserTokenHandler(para1: OAHCUserTokenHandler); overload; virtual;
  strict protected
    function getHttpProcessor(): OAHPBasicHttpProcessor; overload; virtual; final;
  public
    procedure addResponseInterceptor(para1: OAHHttpResponseInterceptor); overload; virtual;
    procedure addResponseInterceptor(para1: OAHHttpResponseInterceptor; para2: jint); overload; virtual;
    function getResponseInterceptor(para1: jint): OAHHttpResponseInterceptor; overload; virtual;
    function getResponseInterceptorCount(): jint; overload; virtual;
    procedure clearResponseInterceptors(); overload; virtual;
    procedure removeResponseInterceptorByClass(para1: JLClass); overload; virtual;
    procedure addRequestInterceptor(para1: OAHHttpRequestInterceptor); overload; virtual;
    procedure addRequestInterceptor(para1: OAHHttpRequestInterceptor; para2: jint); overload; virtual;
    function getRequestInterceptor(para1: jint): OAHHttpRequestInterceptor; overload; virtual;
    function getRequestInterceptorCount(): jint; overload; virtual;
    procedure clearRequestInterceptors(); overload; virtual;
    procedure removeRequestInterceptorByClass(para1: JLClass); overload; virtual;
    function execute(para1: OAHCMHttpUriRequest): OAHHttpResponse; overload; virtual; final;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHCMHttpUriRequest; para2: OAHPHttpContext): OAHHttpResponse; overload; virtual; final;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest): OAHHttpResponse; overload; virtual; final;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHHttpResponse; overload; virtual; final;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
  strict protected
    function createClientRequestDirector(para1: OAHPHttpRequestExecutor; para2: OAHCClientConnectionManager; para3: OAHConnectionReuseStrategy; para4: OAHCConnectionKeepAliveStrategy; para5: OAHCRHttpRoutePlanner; para6: OAHPHttpProcessor; para7: OAHCHttpRequestRetryHandler; para8: OAHCRedirectHandler; para9: OAHCAuthenticationHandler; para10: OAHCAuthenticationHandler; para11: OAHCUserTokenHandler; para12: OAHPHttpParams): OAHCRequestDirector; overload; virtual;
    function determineParams(para1: OAHHttpRequest): OAHPHttpParams; overload; virtual;
  public
    function execute(para1: OAHCMHttpUriRequest; para2: OAHCResponseHandler): JLObject; overload; virtual;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHCMHttpUriRequest; para2: OAHCResponseHandler; para3: OAHPHttpContext): JLObject; overload; virtual;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHCResponseHandler): JLObject; overload; virtual;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHCResponseHandler; para4: OAHPHttpContext): JLObject; overload; virtual;  // throws java.io.IOException, org.apache.http.client.ClientProtocolException
  end;

  OAHICDefaultHttpRequestRetryHandler = class external 'org.apache.http.impl.client' name 'DefaultHttpRequestRetryHandler' (JLObject, OAHCHttpRequestRetryHandler)
  public
    constructor create(para1: jint; para2: jboolean); overload;
    constructor create(); overload;
    function retryRequest(para1: JIIOException; para2: jint; para3: OAHPHttpContext): jboolean; overload; virtual;
    function isRequestSentRetryEnabled(): jboolean; overload; virtual;
    function getRetryCount(): jint; overload; virtual;
  end;

  OAHICDefaultRedirectHandler = class external 'org.apache.http.impl.client' name 'DefaultRedirectHandler' (JLObject, OAHCRedirectHandler)
  public
    constructor create(); overload;
    function isRedirectRequested(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload; virtual;
    function getLocationURI(para1: OAHHttpResponse; para2: OAHPHttpContext): JNURI; overload; virtual;  // throws org.apache.http.ProtocolException
  end;

  OAHICDefaultRequestDirector = class external 'org.apache.http.impl.client' name 'DefaultRequestDirector' (JLObject, OAHCRequestDirector)
  strict protected
    final var
      fconnManager: OAHCClientConnectionManager; external name 'connManager';
      froutePlanner: OAHCRHttpRoutePlanner; external name 'routePlanner';
      freuseStrategy: OAHConnectionReuseStrategy; external name 'reuseStrategy';
      fkeepAliveStrategy: OAHCConnectionKeepAliveStrategy; external name 'keepAliveStrategy';
      frequestExec: OAHPHttpRequestExecutor; external name 'requestExec';
      fhttpProcessor: OAHPHttpProcessor; external name 'httpProcessor';
      fretryHandler: OAHCHttpRequestRetryHandler; external name 'retryHandler';
      fredirectHandler: OAHCRedirectHandler; external name 'redirectHandler';
      fparams: OAHPHttpParams; external name 'params';
  strict protected
    var
      fmanagedConn: OAHCManagedClientConnection; external name 'managedConn';
  public
    constructor create(para1: OAHPHttpRequestExecutor; para2: OAHCClientConnectionManager; para3: OAHConnectionReuseStrategy; para4: OAHCConnectionKeepAliveStrategy; para5: OAHCRHttpRoutePlanner; para6: OAHPHttpProcessor; para7: OAHCHttpRequestRetryHandler; para8: OAHCRedirectHandler; para9: OAHCAuthenticationHandler; para10: OAHCAuthenticationHandler; para11: OAHCUserTokenHandler; para12: OAHPHttpParams); overload;
  strict protected
    procedure rewriteRequestURI(para1: OAHICRequestWrapper; para2: OAHCRHttpRoute); overload; virtual;  // throws org.apache.http.ProtocolException
  public
    function execute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHHttpResponse; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  strict protected
    procedure releaseConnection(); overload; virtual;
    function determineRoute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHCRHttpRoute; overload; virtual;  // throws org.apache.http.HttpException
    procedure establishRoute(para1: OAHCRHttpRoute; para2: OAHPHttpContext); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function createTunnelToTarget(para1: OAHCRHttpRoute; para2: OAHPHttpContext): jboolean; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function createTunnelToProxy(para1: OAHCRHttpRoute; para2: jint; para3: OAHPHttpContext): jboolean; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function createConnectRequest(para1: OAHCRHttpRoute; para2: OAHPHttpContext): OAHHttpRequest; overload; virtual;
    function handleResponse(para1: OAHICRoutedRequest; para2: OAHHttpResponse; para3: OAHPHttpContext): OAHICRoutedRequest; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHICBasicResponseHandler = class external 'org.apache.http.impl.client' name 'BasicResponseHandler' (JLObject, OAHCResponseHandler)
  public
    constructor create(); overload;
    function handleResponse(para1: OAHHttpResponse): JLString; overload; virtual;  // throws org.apache.http.client.HttpResponseException, java.io.IOException
    function handleResponse(para1: OAHHttpResponse): JLObject; overload; virtual;  // throws org.apache.http.client.ClientProtocolException, java.io.IOException
  end;

  OAHICDefaultUserTokenHandler = class external 'org.apache.http.impl.client' name 'DefaultUserTokenHandler' (JLObject, OAHCUserTokenHandler)
  public
    constructor create(); overload;
    function getUserToken(para1: OAHPHttpContext): JLObject; overload; virtual;
  end;

  OAHCPClientContextConfigurer = class external 'org.apache.http.client.protocol' name 'ClientContextConfigurer' (JLObject, OAHCPClientContext)
  public
    constructor create(para1: OAHPHttpContext); overload;
    procedure setCookieSpecRegistry(para1: OAHCCookieSpecRegistry); overload; virtual;
    procedure setAuthSchemeRegistry(para1: OAHAAuthSchemeRegistry); overload; virtual;
    procedure setCookieStore(para1: OAHCCookieStore); overload; virtual;
    procedure setCredentialsProvider(para1: OAHCCredentialsProvider); overload; virtual;
    procedure setAuthSchemePref(para1: JUList); overload; virtual;
  end;

  OAHICSingleClientConnManager = class external 'org.apache.http.impl.conn' name 'SingleClientConnManager' (JLObject, OAHCClientConnectionManager)
  public
    type
      InnerConnAdapter = class;
      Arr1InnerConnAdapter = array of InnerConnAdapter;
      Arr2InnerConnAdapter = array of Arr1InnerConnAdapter;
      Arr3InnerConnAdapter = array of Arr2InnerConnAdapter;
      InnerPoolEntry = class;
      Arr1InnerPoolEntry = array of InnerPoolEntry;
      Arr2InnerPoolEntry = array of Arr1InnerPoolEntry;
      Arr3InnerPoolEntry = array of Arr2InnerPoolEntry;
      InnerConnAdapter = class external 'org.apache.http.impl.conn' name 'ConnAdapter' 
      end;

      InnerPoolEntry = class external 'org.apache.http.impl.conn' name 'PoolEntry' 
      end;

  public
    const
      MISUSE_MESSAGE = 'Invalid use of SingleClientConnManager: connection still allocated.'#$000a'Make sure to release the connection before allocating another one.';
  strict protected
    var
      fschemeRegistry: OAHCSSchemeRegistry; external name 'schemeRegistry';
      fconnOperator: OAHCClientConnectionOperator; external name 'connOperator';
      funiquePoolEntry: OAHICSingleClientConnManager.InnerPoolEntry; external name 'uniquePoolEntry';
      fmanagedConn: OAHICSingleClientConnManager.InnerConnAdapter; external name 'managedConn';
      flastReleaseTime: jlong; external name 'lastReleaseTime';
      fconnectionExpiresTime: jlong; external name 'connectionExpiresTime';
      falwaysShutDown: jboolean; external name 'alwaysShutDown';
      fisShutDown: jboolean; external name 'isShutDown';
  public
    constructor create(para1: OAHPHttpParams; para2: OAHCSSchemeRegistry); overload;
  strict protected
    procedure finalize(); overload; virtual;  // throws java.lang.Throwable
  public
    function getSchemeRegistry(): OAHCSSchemeRegistry; overload; virtual;
  strict protected
    function createConnectionOperator(para1: OAHCSSchemeRegistry): OAHCClientConnectionOperator; overload; virtual;
    procedure assertStillUp(); overload; virtual; final;  // throws java.lang.IllegalStateException
  public
    function requestConnection(para1: OAHCRHttpRoute; para2: JLObject): OAHCClientConnectionRequest; overload; virtual; final;
    function getConnection(para1: OAHCRHttpRoute; para2: JLObject): OAHCManagedClientConnection; overload; virtual;
    procedure releaseConnection(para1: OAHCManagedClientConnection; para2: jlong; para3: JUCTimeUnit); overload; virtual;
    procedure closeExpiredConnections(); overload; virtual;
    procedure closeIdleConnections(para1: jlong; para2: JUCTimeUnit); overload; virtual;
    procedure shutdown(); overload; virtual;
  strict protected
    procedure revokeConnection(); overload; virtual;
  end;

  OAHICTThreadSafeClientConnManager = class external 'org.apache.http.impl.conn.tsccm' name 'ThreadSafeClientConnManager' (JLObject, OAHCClientConnectionManager)
  strict protected
    var
      fschemeRegistry: OAHCSSchemeRegistry; external name 'schemeRegistry';
  strict protected
    final var
      fconnectionPool: OAHICTAbstractConnPool; external name 'connectionPool';
  strict protected
    var
      fconnOperator: OAHCClientConnectionOperator; external name 'connOperator';
  public
    constructor create(para1: OAHPHttpParams; para2: OAHCSSchemeRegistry); overload;
  strict protected
    procedure finalize(); overload; virtual;  // throws java.lang.Throwable
    function createConnectionPool(para1: OAHPHttpParams): OAHICTAbstractConnPool; overload; virtual;
    function createConnectionOperator(para1: OAHCSSchemeRegistry): OAHCClientConnectionOperator; overload; virtual;
  public
    function getSchemeRegistry(): OAHCSSchemeRegistry; overload; virtual;
    function requestConnection(para1: OAHCRHttpRoute; para2: JLObject): OAHCClientConnectionRequest; overload; virtual;
    procedure releaseConnection(para1: OAHCManagedClientConnection; para2: jlong; para3: JUCTimeUnit); overload; virtual;
    procedure shutdown(); overload; virtual;
    function getConnectionsInPool(para1: OAHCRHttpRoute): jint; overload; virtual;
    function getConnectionsInPool(): jint; overload; virtual;
    procedure closeIdleConnections(para1: jlong; para2: JUCTimeUnit); overload; virtual;
    procedure closeExpiredConnections(); overload; virtual;
  end;

  OAHICDefaultClientConnectionOperator = class external 'org.apache.http.impl.conn' name 'DefaultClientConnectionOperator' (JLObject, OAHCClientConnectionOperator)
  strict protected
    var
      fschemeRegistry: OAHCSSchemeRegistry; external name 'schemeRegistry';
  public
    constructor create(para1: OAHCSSchemeRegistry); overload;
    function createConnection(): OAHCOperatedClientConnection; overload; virtual;
    procedure openConnection(para1: OAHCOperatedClientConnection; para2: OAHHttpHost; para3: JNInetAddress; para4: OAHPHttpContext; para5: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure updateSecureConnection(para1: OAHCOperatedClientConnection; para2: OAHHttpHost; para3: OAHPHttpContext; para4: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
  strict protected
    procedure prepareSocket(para1: JNSocket; para2: OAHPHttpContext; para3: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
  end;

  OAHICDefaultConnectionKeepAliveStrategy = class external 'org.apache.http.impl.client' name 'DefaultConnectionKeepAliveStrategy' (JLObject, OAHCConnectionKeepAliveStrategy)
  public
    constructor create(); overload;
    function getKeepAliveDuration(para1: OAHHttpResponse; para2: OAHPHttpContext): jlong; overload; virtual;
  end;

  OAHCEofSensorInputStream = class external 'org.apache.http.conn' name 'EofSensorInputStream' (JIInputStream, OAHCConnectionReleaseTrigger)
  strict protected
    var
      fwrappedStream: JIInputStream; external name 'wrappedStream';
  public
    constructor create(para1: JIInputStream; para2: OAHCEofSensorWatcher); overload;
  strict protected
    function isReadAllowed(): jboolean; overload; virtual;  // throws java.io.IOException
  public
    function read(): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte): jint; overload; virtual;  // throws java.io.IOException
    function available(): jint; overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  strict protected
    procedure checkEOF(para1: jint); overload; virtual;  // throws java.io.IOException
    procedure checkClose(); overload; virtual;  // throws java.io.IOException
    procedure checkAbort(); overload; virtual;  // throws java.io.IOException
  public
    procedure releaseConnection(); overload; virtual;  // throws java.io.IOException
    procedure abortConnection(); overload; virtual;  // throws java.io.IOException
  end;

  OAHCBasicEofSensorWatcher = class external 'org.apache.http.conn' name 'BasicEofSensorWatcher' (JLObject, OAHCEofSensorWatcher)
  strict protected
    var
      fmanagedConn: OAHCManagedClientConnection; external name 'managedConn';
      fattemptReuse: jboolean; external name 'attemptReuse';
  public
    constructor create(para1: OAHCManagedClientConnection; para2: jboolean); overload;
    function eofDetected(para1: JIInputStream): jboolean; overload; virtual;  // throws java.io.IOException
    function streamClosed(para1: JIInputStream): jboolean; overload; virtual;  // throws java.io.IOException
    function streamAbort(para1: JIInputStream): jboolean; overload; virtual;  // throws java.io.IOException
  end;

  OAHCPConnManagerParams = class sealed external 'org.apache.http.conn.params' name 'ConnManagerParams' (JLObject, OAHCPConnManagerPNames)
  public
    const
      DEFAULT_MAX_TOTAL_CONNECTIONS = 20;
  public
    constructor create(); overload;
    class function getTimeout(para1: OAHPHttpParams): jlong; static; overload;
    class procedure setTimeout(para1: OAHPHttpParams; para2: jlong); static; overload;
    class procedure setMaxConnectionsPerRoute(para1: OAHPHttpParams; para2: OAHCPConnPerRoute); static; overload;
    class function getMaxConnectionsPerRoute(para1: OAHPHttpParams): OAHCPConnPerRoute; static; overload;
    class procedure setMaxTotalConnections(para1: OAHPHttpParams; para2: jint); static; overload;
    class function getMaxTotalConnections(para1: OAHPHttpParams): jint; static; overload;
  end;

  OAHCPConnPerRouteBean = class sealed external 'org.apache.http.conn.params' name 'ConnPerRouteBean' (JLObject, OAHCPConnPerRoute)
  public
    const
      DEFAULT_MAX_CONNECTIONS_PER_ROUTE = 2;
  public
    constructor create(para1: jint); overload;
    constructor create(); overload;
    function getDefaultMax(): jint; overload; virtual;
    procedure setDefaultMaxPerRoute(para1: jint); overload; virtual;
    procedure setMaxForRoute(para1: OAHCRHttpRoute; para2: jint); overload; virtual;
    function getMaxForRoute(para1: OAHCRHttpRoute): jint; overload; virtual;
    procedure setMaxForRoutes(para1: JUMap); overload; virtual;
  end;

  OAHCPConnRouteParams = class external 'org.apache.http.conn.params' name 'ConnRouteParams' (JLObject, OAHCPConnRoutePNames)
  public
    final class var
      fNO_HOST: OAHHttpHost; external name 'NO_HOST';
      fNO_ROUTE: OAHCRHttpRoute; external name 'NO_ROUTE';
  public
    class function getDefaultProxy(para1: OAHPHttpParams): OAHHttpHost; static; overload;
    class procedure setDefaultProxy(para1: OAHPHttpParams; para2: OAHHttpHost); static; overload;
    class function getForcedRoute(para1: OAHPHttpParams): OAHCRHttpRoute; static; overload;
    class procedure setForcedRoute(para1: OAHPHttpParams; para2: OAHCRHttpRoute); static; overload;
    class function getLocalAddress(para1: OAHPHttpParams): JNInetAddress; static; overload;
    class procedure setLocalAddress(para1: OAHPHttpParams; para2: JNInetAddress); static; overload;
  end;

  OAHCRBasicRouteDirector = class external 'org.apache.http.conn.routing' name 'BasicRouteDirector' (JLObject, OAHCRHttpRouteDirector)
  public
    constructor create(); overload;
    function nextStep(para1: OAHCRRouteInfo; para2: OAHCRRouteInfo): jint; overload; virtual;
  strict protected
    function firstStep(para1: OAHCRRouteInfo): jint; overload; virtual;
    function directStep(para1: OAHCRRouteInfo; para2: OAHCRRouteInfo): jint; overload; virtual;
    function proxiedStep(para1: OAHCRRouteInfo; para2: OAHCRRouteInfo): jint; overload; virtual;
  end;

  OAHICDefaultHttpRoutePlanner = class external 'org.apache.http.impl.conn' name 'DefaultHttpRoutePlanner' (JLObject, OAHCRHttpRoutePlanner)
  strict protected
    var
      fschemeRegistry: OAHCSSchemeRegistry; external name 'schemeRegistry';
  public
    constructor create(para1: OAHCSSchemeRegistry); overload;
    function determineRoute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHCRHttpRoute; overload; virtual;  // throws org.apache.http.HttpException
  end;

  OAHICProxySelectorRoutePlanner = class external 'org.apache.http.impl.conn' name 'ProxySelectorRoutePlanner' (JLObject, OAHCRHttpRoutePlanner)
  strict protected
    var
      fschemeRegistry: OAHCSSchemeRegistry; external name 'schemeRegistry';
      fproxySelector: JNProxySelector; external name 'proxySelector';
  public
    constructor create(para1: OAHCSSchemeRegistry; para2: JNProxySelector); overload;
    function getProxySelector(): JNProxySelector; overload; virtual;
    procedure setProxySelector(para1: JNProxySelector); overload; virtual;
    function determineRoute(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHCRHttpRoute; overload; virtual;  // throws org.apache.http.HttpException
  strict protected
    function determineProxy(para1: OAHHttpHost; para2: OAHHttpRequest; para3: OAHPHttpContext): OAHHttpHost; overload; virtual;  // throws org.apache.http.HttpException
    function getHost(para1: JNInetSocketAddress): JLString; overload; virtual;
    function chooseProxy(para1: JUList; para2: OAHHttpHost; para3: OAHHttpRequest; para4: OAHPHttpContext): JNProxy; overload; virtual;
  end;

  OAHCMultihomePlainSocketFactory = class sealed external 'org.apache.http.conn' name 'MultihomePlainSocketFactory' (JLObject, OAHCSSocketFactory)
  public
    class function getSocketFactory(): OAHCMultihomePlainSocketFactory; static; overload;
    function createSocket(): JNSocket; overload; virtual;
    function connectSocket(para1: JNSocket; para2: JLString; para3: jint; para4: JNInetAddress; para5: jint; para6: OAHPHttpParams): JNSocket; overload; virtual;  // throws java.io.IOException
    function isSecure(para1: JNSocket): jboolean; overload; virtual; final;  // throws java.lang.IllegalArgumentException
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  OAHCSLayeredSocketFactory = interface external 'org.apache.http.conn.scheme' name 'LayeredSocketFactory' (OAHCSSocketFactory)
    function createSocket(para1: JNSocket; para2: JLString; para3: jint; para4: jboolean): JNSocket; overload;  // throws java.io.IOException, java.net.UnknownHostException
  end;

  OAHCSPlainSocketFactory = class sealed external 'org.apache.http.conn.scheme' name 'PlainSocketFactory' (JLObject, OAHCSSocketFactory)
  public
    constructor create(para1: OAHCSHostNameResolver); overload;
    constructor create(); overload;
    class function getSocketFactory(): OAHCSPlainSocketFactory; static; overload;
    function createSocket(): JNSocket; overload; virtual;
    function connectSocket(para1: JNSocket; para2: JLString; para3: jint; para4: JNInetAddress; para5: jint; para6: OAHPHttpParams): JNSocket; overload; virtual;  // throws java.io.IOException
    function isSecure(para1: JNSocket): jboolean; overload; virtual; final;  // throws java.lang.IllegalArgumentException
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  OAHCClientCookie = interface external 'org.apache.http.cookie' name 'ClientCookie' (OAHCCookie)
    const
      VERSION_ATTR = 'version';
      PATH_ATTR = 'path';
      DOMAIN_ATTR = 'domain';
      MAX_AGE_ATTR = 'max-age';
      SECURE_ATTR = 'secure';
      COMMENT_ATTR = 'comment';
      EXPIRES_ATTR = 'expires';
      PORT_ATTR = 'port';
      COMMENTURL_ATTR = 'commenturl';
      DISCARD_ATTR = 'discard';
    function getAttribute(para1: JLString): JLString; overload;
    function containsAttribute(para1: JLString): jboolean; overload;
  end;

  OAHCSetCookie = interface external 'org.apache.http.cookie' name 'SetCookie' (OAHCCookie)
    procedure setValue(para1: JLString); overload;
    procedure setComment(para1: JLString); overload;
    procedure setExpiryDate(para1: JUDate); overload;
    procedure setDomain(para1: JLString); overload;
    procedure setPath(para1: JLString); overload;
    procedure setSecure(para1: jboolean); overload;
    procedure setVersion(para1: jint); overload;
  end;

  OAHICAbstractCookieAttributeHandler = class abstract external 'org.apache.http.impl.cookie' name 'AbstractCookieAttributeHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICBasicDomainHandler = class external 'org.apache.http.impl.cookie' name 'BasicDomainHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICBasicPathHandler = class external 'org.apache.http.impl.cookie' name 'BasicPathHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2109DomainHandler = class external 'org.apache.http.impl.cookie' name 'RFC2109DomainHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2965CommentUrlAttributeHandler = class external 'org.apache.http.impl.cookie' name 'RFC2965CommentUrlAttributeHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2965DiscardAttributeHandler = class external 'org.apache.http.impl.cookie' name 'RFC2965DiscardAttributeHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2965DomainAttributeHandler = class external 'org.apache.http.impl.cookie' name 'RFC2965DomainAttributeHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function domainMatch(para1: JLString; para2: JLString): jboolean; overload; virtual;
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2965PortAttributeHandler = class external 'org.apache.http.impl.cookie' name 'RFC2965PortAttributeHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2965VersionAttributeHandler = class external 'org.apache.http.impl.cookie' name 'RFC2965VersionAttributeHandler' (JLObject, OAHCCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICAbstractCookieSpec = class abstract external 'org.apache.http.impl.cookie' name 'AbstractCookieSpec' (JLObject, OAHCCookieSpec)
  public
    constructor create(); overload;
    procedure registerAttribHandler(para1: JLString; para2: OAHCCookieAttributeHandler); overload; virtual;
  strict protected
    function findAttribHandler(para1: JLString): OAHCCookieAttributeHandler; overload; virtual;
    function getAttribHandler(para1: JLString): OAHCCookieAttributeHandler; overload; virtual;
    function getAttribHandlers(): JUCollection; overload; virtual;
  end;

  OAHICBestMatchSpec = class external 'org.apache.http.impl.cookie' name 'BestMatchSpec' (JLObject, OAHCCookieSpec)
  public
    constructor create(para1: Arr1JLString; para2: jboolean); overload;
    constructor create(const para1: array of JLString; para2: jboolean); overload;
    constructor create(); overload;
    function parse(para1: OAHHeader; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
    function formatCookies(para1: JUList): JUList; overload; virtual;
    function getVersion(): jint; overload; virtual;
    function getVersionHeader(): OAHHeader; overload; virtual;
  end;

  OAHICBestMatchSpecFactory = class external 'org.apache.http.impl.cookie' name 'BestMatchSpecFactory' (JLObject, OAHCCookieSpecFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHCCookieSpec; overload; virtual;
  end;

  OAHICBrowserCompatSpecFactory = class external 'org.apache.http.impl.cookie' name 'BrowserCompatSpecFactory' (JLObject, OAHCCookieSpecFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHCCookieSpec; overload; virtual;
  end;

  OAHICNetscapeDraftSpecFactory = class external 'org.apache.http.impl.cookie' name 'NetscapeDraftSpecFactory' (JLObject, OAHCCookieSpecFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHCCookieSpec; overload; virtual;
  end;

  OAHICRFC2109SpecFactory = class external 'org.apache.http.impl.cookie' name 'RFC2109SpecFactory' (JLObject, OAHCCookieSpecFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHCCookieSpec; overload; virtual;
  end;

  OAHICRFC2965SpecFactory = class external 'org.apache.http.impl.cookie' name 'RFC2965SpecFactory' (JLObject, OAHCCookieSpecFactory)
  public
    constructor create(); overload;
    function newInstance(para1: OAHPHttpParams): OAHCCookieSpec; overload; virtual;
  end;

  OAHIELaxContentLengthStrategy = class external 'org.apache.http.impl.entity' name 'LaxContentLengthStrategy' (JLObject, OAHEContentLengthStrategy)
  public
    constructor create(); overload;
    function determineLength(para1: OAHHttpMessage): jlong; overload; virtual;  // throws org.apache.http.HttpException
  end;

  OAHIEStrictContentLengthStrategy = class external 'org.apache.http.impl.entity' name 'StrictContentLengthStrategy' (JLObject, OAHEContentLengthStrategy)
  public
    constructor create(); overload;
    function determineLength(para1: OAHHttpMessage): jlong; overload; virtual;  // throws org.apache.http.HttpException
  end;

  OAHICTBasicPoolEntry = class external 'org.apache.http.impl.conn.tsccm' name 'BasicPoolEntry' (OAHICAbstractPoolEntry)
  public
    constructor create(para1: OAHCClientConnectionOperator; para2: OAHCRHttpRoute; para3: JLRReferenceQueue); overload;
  strict protected
    function getConnection(): OAHCOperatedClientConnection; overload; virtual; final;
    function getPlannedRoute(): OAHCRHttpRoute; overload; virtual; final;
    function getWeakRef(): OAHICTBasicPoolEntryRef; overload; virtual; final;
  end;

  OAHICTAbstractConnPool = class abstract external 'org.apache.http.impl.conn.tsccm' name 'AbstractConnPool' (JLObject, OAHICTRefQueueHandler)
  strict protected
    final var
      fpoolLock: JUCLLock; external name 'poolLock';
  strict protected
    var
      fissuedConnections: JUSet; external name 'issuedConnections';
      fidleConnHandler: OAHICIdleConnectionHandler; external name 'idleConnHandler';
      fnumConnections: jint; external name 'numConnections';
      frefQueue: JLRReferenceQueue; external name 'refQueue';
      fisShutDown: jboolean; external name 'isShutDown';
  strict protected
    constructor create(); overload;
  public
    procedure enableConnectionGC(); overload; virtual;  // throws java.lang.IllegalStateException
    function getEntry(para1: OAHCRHttpRoute; para2: JLObject; para3: jlong; para4: JUCTimeUnit): OAHICTBasicPoolEntry; overload; virtual; final;  // throws org.apache.http.conn.ConnectionPoolTimeoutException, java.lang.InterruptedException
    function requestPoolEntry(para1: OAHCRHttpRoute; para2: JLObject): OAHICTPoolEntryRequest; overload; virtual; abstract;
    procedure freeEntry(para1: OAHICTBasicPoolEntry; para2: jboolean; para3: jlong; para4: JUCTimeUnit); overload; virtual; abstract;
    procedure handleReference(para1: JLRReference); overload; virtual;
  strict protected
    procedure handleLostEntry(para1: OAHCRHttpRoute); overload; virtual; abstract;
  public
    procedure closeIdleConnections(para1: jlong; para2: JUCTimeUnit); overload; virtual;
    procedure closeExpiredConnections(); overload; virtual;
    procedure deleteClosedConnections(); overload; virtual; abstract;
    procedure shutdown(); overload; virtual;
  strict protected
    procedure closeConnection(para1: OAHCOperatedClientConnection); overload; virtual;
  end;

  OAHIIAbstractMessageParser = class abstract external 'org.apache.http.impl.io' name 'AbstractMessageParser' (JLObject, OAHIHttpMessageParser)
  strict protected
    final var
      flineParser: OAHMLineParser; external name 'lineParser';
  public
    constructor create(para1: OAHISessionInputBuffer; para2: OAHMLineParser; para3: OAHPHttpParams); overload;
    class function parseHeaders(para1: OAHISessionInputBuffer; para2: jint; para3: jint; para4: OAHMLineParser): Arr1OAHHeader; static; overload;  // throws org.apache.http.HttpException, java.io.IOException
  strict protected
    function parseHead(para1: OAHISessionInputBuffer): OAHHttpMessage; overload; virtual; abstract;  // throws java.io.IOException, org.apache.http.HttpException, org.apache.http.ParseException
  public
    function parse(): OAHHttpMessage; overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
  end;

  OAHIIAbstractMessageWriter = class abstract external 'org.apache.http.impl.io' name 'AbstractMessageWriter' (JLObject, OAHIHttpMessageWriter)
  strict protected
    final var
      fsessionBuffer: OAHISessionOutputBuffer; external name 'sessionBuffer';
      flineBuf: OAHUCharArrayBuffer; external name 'lineBuf';
      flineFormatter: OAHMLineFormatter; external name 'lineFormatter';
  public
    constructor create(para1: OAHISessionOutputBuffer; para2: OAHMLineFormatter; para3: OAHPHttpParams); overload;
  strict protected
    procedure writeHeadLine(para1: OAHHttpMessage); overload; virtual; abstract;  // throws java.io.IOException
  public
    procedure write(para1: OAHHttpMessage); overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
  end;

  OAHIIHttpTransportMetricsImpl = class external 'org.apache.http.impl.io' name 'HttpTransportMetricsImpl' (JLObject, OAHIHttpTransportMetrics)
  public
    constructor create(); overload;
    function getBytesTransferred(): jlong; overload; virtual;
    procedure setBytesTransferred(para1: jlong); overload; virtual;
    procedure incrementBytesTransferred(para1: jlong); overload; virtual;
    procedure reset(); overload; virtual;
  end;

  OAHICLoggingSessionInputBuffer = class external 'org.apache.http.impl.conn' name 'LoggingSessionInputBuffer' (JLObject, OAHISessionInputBuffer)
  public
    constructor create(para1: OAHISessionInputBuffer; para2: OAHICWire); overload;
    function isDataAvailable(para1: jint): jboolean; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte): jint; overload; virtual;  // throws java.io.IOException
    function readLine(): JLString; overload; virtual;  // throws java.io.IOException
    function readLine(para1: OAHUCharArrayBuffer): jint; overload; virtual;  // throws java.io.IOException
    function getMetrics(): OAHIHttpTransportMetrics; overload; virtual;
  end;

  OAHIIAbstractSessionInputBuffer = class abstract external 'org.apache.http.impl.io' name 'AbstractSessionInputBuffer' (JLObject, OAHISessionInputBuffer)
  public
    constructor create(); overload;
  strict protected
    procedure init(para1: JIInputStream; para2: jint; para3: OAHPHttpParams); overload; virtual;
    function fillBuffer(): jint; overload; virtual;  // throws java.io.IOException
    function hasBufferedData(): jboolean; overload; virtual;
  public
    function read(): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte; para2: jint; para3: jint): jint; overload; virtual;  // throws java.io.IOException
    function read(para1: Arr1jbyte): jint; overload; virtual;  // throws java.io.IOException
    function read(var para1: array of jbyte): jint; overload; virtual;  // throws java.io.IOException
    function readLine(para1: OAHUCharArrayBuffer): jint; overload; virtual;  // throws java.io.IOException
    function readLine(): JLString; overload; virtual;  // throws java.io.IOException
    function getMetrics(): OAHIHttpTransportMetrics; overload; virtual;
  end;

  OAHICLoggingSessionOutputBuffer = class external 'org.apache.http.impl.conn' name 'LoggingSessionOutputBuffer' (JLObject, OAHISessionOutputBuffer)
  public
    constructor create(para1: OAHISessionOutputBuffer; para2: OAHICWire); overload;
    procedure write(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(para1: jint); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure writeLine(para1: OAHUCharArrayBuffer); overload; virtual;  // throws java.io.IOException
    procedure writeLine(para1: JLString); overload; virtual;  // throws java.io.IOException
    function getMetrics(): OAHIHttpTransportMetrics; overload; virtual;
  end;

  OAHIIAbstractSessionOutputBuffer = class abstract external 'org.apache.http.impl.io' name 'AbstractSessionOutputBuffer' (JLObject, OAHISessionOutputBuffer)
  public
    constructor create(); overload;
  strict protected
    procedure init(para1: JIOutputStream; para2: jint; para3: OAHPHttpParams); overload; virtual;
    procedure flushBuffer(); overload; virtual;  // throws java.io.IOException
  public
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte; para2: jint; para3: jint); overload; virtual;  // throws java.io.IOException
    procedure write(para1: Arr1jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(var para1: array of jbyte); overload; virtual;  // throws java.io.IOException
    procedure write(para1: jint); overload; virtual;  // throws java.io.IOException
    procedure writeLine(para1: JLString); overload; virtual;  // throws java.io.IOException
    procedure writeLine(para1: OAHUCharArrayBuffer); overload; virtual;  // throws java.io.IOException
    function getMetrics(): OAHIHttpTransportMetrics; overload; virtual;
  end;

  OAHMBasicHeaderValueFormatter = class external 'org.apache.http.message' name 'BasicHeaderValueFormatter' (JLObject, OAHMHeaderValueFormatter)
  public
    final class var
      fDEFAULT: OAHMBasicHeaderValueFormatter; external name 'DEFAULT';
  public
    const
      SEPARATORS = ' ;,:@()<>\"/[]?={}'#$0009'';
      UNSAFE_CHARS = '"\';
  public
    constructor create(); overload;
    class function formatElements(para1: Arr1OAHHeaderElement; para2: jboolean; para3: OAHMHeaderValueFormatter): JLString; static; overload; final;
    class function formatElements(var para1: array of OAHHeaderElement; para2: jboolean; para3: OAHMHeaderValueFormatter): JLString; static; overload; final;
    function formatElements(para1: OAHUCharArrayBuffer; para2: Arr1OAHHeaderElement; para3: jboolean): OAHUCharArrayBuffer; overload; virtual;
    function formatElements(para1: OAHUCharArrayBuffer; var para2: array of OAHHeaderElement; para3: jboolean): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    function estimateElementsLen(para1: Arr1OAHHeaderElement): jint; overload; virtual;
    function estimateElementsLen(var para1: array of OAHHeaderElement): jint; overload; virtual;
  public
    class function formatHeaderElement(para1: OAHHeaderElement; para2: jboolean; para3: OAHMHeaderValueFormatter): JLString; static; overload; final;
    function formatHeaderElement(para1: OAHUCharArrayBuffer; para2: OAHHeaderElement; para3: jboolean): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    function estimateHeaderElementLen(para1: OAHHeaderElement): jint; overload; virtual;
  public
    class function formatParameters(para1: Arr1OAHNameValuePair; para2: jboolean; para3: OAHMHeaderValueFormatter): JLString; static; overload; final;
    class function formatParameters(var para1: array of OAHNameValuePair; para2: jboolean; para3: OAHMHeaderValueFormatter): JLString; static; overload; final;
    function formatParameters(para1: OAHUCharArrayBuffer; para2: Arr1OAHNameValuePair; para3: jboolean): OAHUCharArrayBuffer; overload; virtual;
    function formatParameters(para1: OAHUCharArrayBuffer; var para2: array of OAHNameValuePair; para3: jboolean): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    function estimateParametersLen(para1: Arr1OAHNameValuePair): jint; overload; virtual;
    function estimateParametersLen(var para1: array of OAHNameValuePair): jint; overload; virtual;
  public
    class function formatNameValuePair(para1: OAHNameValuePair; para2: jboolean; para3: OAHMHeaderValueFormatter): JLString; static; overload; final;
    function formatNameValuePair(para1: OAHUCharArrayBuffer; para2: OAHNameValuePair; para3: jboolean): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    function estimateNameValuePairLen(para1: OAHNameValuePair): jint; overload; virtual;
    procedure doFormatValue(para1: OAHUCharArrayBuffer; para2: JLString; para3: jboolean); overload; virtual;
    function isSeparator(para1: jchar): jboolean; overload; virtual;
    function isUnsafe(para1: jchar): jboolean; overload; virtual;
  end;

  OAHMBasicHeaderValueParser = class external 'org.apache.http.message' name 'BasicHeaderValueParser' (JLObject, OAHMHeaderValueParser)
  public
    final class var
      fDEFAULT: OAHMBasicHeaderValueParser; external name 'DEFAULT';
  public
    constructor create(); overload;
    class function parseElements(para1: JLString; para2: OAHMHeaderValueParser): Arr1OAHHeaderElement; static; overload; final;  // throws org.apache.http.ParseException
    function parseElements(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): Arr1OAHHeaderElement; overload; virtual;
    class function parseHeaderElement(para1: JLString; para2: OAHMHeaderValueParser): OAHHeaderElement; static; overload; final;  // throws org.apache.http.ParseException
    function parseHeaderElement(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHHeaderElement; overload; virtual;
  strict protected
    function createHeaderElement(para1: JLString; para2: JLString; para3: Arr1OAHNameValuePair): OAHHeaderElement; overload; virtual;
    function createHeaderElement(para1: JLString; para2: JLString; var para3: array of OAHNameValuePair): OAHHeaderElement; overload; virtual;
  public
    class function parseParameters(para1: JLString; para2: OAHMHeaderValueParser): Arr1OAHNameValuePair; static; overload; final;  // throws org.apache.http.ParseException
    function parseParameters(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): Arr1OAHNameValuePair; overload; virtual;
    class function parseNameValuePair(para1: JLString; para2: OAHMHeaderValueParser): OAHNameValuePair; static; overload; final;  // throws org.apache.http.ParseException
    function parseNameValuePair(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHNameValuePair; overload; virtual;
    function parseNameValuePair(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor; para3: Arr1jchar): OAHNameValuePair; overload; virtual;
    function parseNameValuePair(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor; var para3: array of jchar): OAHNameValuePair; overload; virtual;
  strict protected
    function createNameValuePair(para1: JLString; para2: JLString): OAHNameValuePair; overload; virtual;
  end;

  OAHMBasicLineFormatter = class external 'org.apache.http.message' name 'BasicLineFormatter' (JLObject, OAHMLineFormatter)
  public
    final class var
      fDEFAULT: OAHMBasicLineFormatter; external name 'DEFAULT';
  public
    constructor create(); overload;
  strict protected
    function initBuffer(para1: OAHUCharArrayBuffer): OAHUCharArrayBuffer; overload; virtual;
  public
    class function formatProtocolVersion(para1: OAHProtocolVersion; para2: OAHMLineFormatter): JLString; static; overload; final;
    function appendProtocolVersion(para1: OAHUCharArrayBuffer; para2: OAHProtocolVersion): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    function estimateProtocolVersionLen(para1: OAHProtocolVersion): jint; overload; virtual;
  public
    class function formatRequestLine(para1: OAHRequestLine; para2: OAHMLineFormatter): JLString; static; overload; final;
    function formatRequestLine(para1: OAHUCharArrayBuffer; para2: OAHRequestLine): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    procedure doFormatRequestLine(para1: OAHUCharArrayBuffer; para2: OAHRequestLine); overload; virtual;
  public
    class function formatStatusLine(para1: OAHStatusLine; para2: OAHMLineFormatter): JLString; static; overload; final;
    function formatStatusLine(para1: OAHUCharArrayBuffer; para2: OAHStatusLine): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    procedure doFormatStatusLine(para1: OAHUCharArrayBuffer; para2: OAHStatusLine); overload; virtual;
  public
    class function formatHeader(para1: OAHHeader; para2: OAHMLineFormatter): JLString; static; overload; final;
    function formatHeader(para1: OAHUCharArrayBuffer; para2: OAHHeader): OAHUCharArrayBuffer; overload; virtual;
  strict protected
    procedure doFormatHeader(para1: OAHUCharArrayBuffer; para2: OAHHeader); overload; virtual;
  end;

  OAHMBasicLineParser = class external 'org.apache.http.message' name 'BasicLineParser' (JLObject, OAHMLineParser)
  public
    final class var
      fDEFAULT: OAHMBasicLineParser; external name 'DEFAULT';
  strict protected
    final var
      fprotocol: OAHProtocolVersion; external name 'protocol';
  public
    constructor create(para1: OAHProtocolVersion); overload;
    constructor create(); overload;
    class function parseProtocolVersion(para1: JLString; para2: OAHMLineParser): OAHProtocolVersion; static; overload; final;  // throws org.apache.http.ParseException
    function parseProtocolVersion(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHProtocolVersion; overload; virtual;  // throws org.apache.http.ParseException
  strict protected
    function createProtocolVersion(para1: jint; para2: jint): OAHProtocolVersion; overload; virtual;
  public
    function hasProtocolVersion(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): jboolean; overload; virtual;
    class function parseRequestLine(para1: JLString; para2: OAHMLineParser): OAHRequestLine; static; overload; final;  // throws org.apache.http.ParseException
    function parseRequestLine(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHRequestLine; overload; virtual;  // throws org.apache.http.ParseException
  strict protected
    function createRequestLine(para1: JLString; para2: JLString; para3: OAHProtocolVersion): OAHRequestLine; overload; virtual;
  public
    class function parseStatusLine(para1: JLString; para2: OAHMLineParser): OAHStatusLine; static; overload; final;  // throws org.apache.http.ParseException
    function parseStatusLine(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor): OAHStatusLine; overload; virtual;  // throws org.apache.http.ParseException
  strict protected
    function createStatusLine(para1: OAHProtocolVersion; para2: jint; para3: JLString): OAHStatusLine; overload; virtual;
  public
    class function parseHeader(para1: JLString; para2: OAHMLineParser): OAHHeader; static; overload; final;  // throws org.apache.http.ParseException
    function parseHeader(para1: OAHUCharArrayBuffer): OAHHeader; overload; virtual;  // throws org.apache.http.ParseException
  strict protected
    procedure skipWhitespace(para1: OAHUCharArrayBuffer; para2: OAHMParserCursor); overload; virtual;
  end;

  OAHPHttpConnectionParams = class sealed external 'org.apache.http.params' name 'HttpConnectionParams' (JLObject, OAHPCoreConnectionPNames)
  public
    class function getSoTimeout(para1: OAHPHttpParams): jint; static; overload;
    class procedure setSoTimeout(para1: OAHPHttpParams; para2: jint); static; overload;
    class function getTcpNoDelay(para1: OAHPHttpParams): jboolean; static; overload;
    class procedure setTcpNoDelay(para1: OAHPHttpParams; para2: jboolean); static; overload;
    class function getSocketBufferSize(para1: OAHPHttpParams): jint; static; overload;
    class procedure setSocketBufferSize(para1: OAHPHttpParams; para2: jint); static; overload;
    class function getLinger(para1: OAHPHttpParams): jint; static; overload;
    class procedure setLinger(para1: OAHPHttpParams; para2: jint); static; overload;
    class function getConnectionTimeout(para1: OAHPHttpParams): jint; static; overload;
    class procedure setConnectionTimeout(para1: OAHPHttpParams; para2: jint); static; overload;
    class function isStaleCheckingEnabled(para1: OAHPHttpParams): jboolean; static; overload;
    class procedure setStaleCheckingEnabled(para1: OAHPHttpParams; para2: jboolean); static; overload;
  end;

  OAHCPAllClientPNames = interface external 'org.apache.http.client.params' name 'AllClientPNames' (OAHPCoreConnectionPNames, OAHPCoreProtocolPNames, OAHCPClientPNames, OAHAPAuthPNames, OAHCPCookieSpecPNames, OAHCPConnConnectionPNames, OAHCPConnManagerPNames, OAHCPConnRoutePNames)
  end;

  OAHPHttpProtocolParams = class sealed external 'org.apache.http.params' name 'HttpProtocolParams' (JLObject, OAHPCoreProtocolPNames)
  public
    class function getHttpElementCharset(para1: OAHPHttpParams): JLString; static; overload;
    class procedure setHttpElementCharset(para1: OAHPHttpParams; para2: JLString); static; overload;
    class function getContentCharset(para1: OAHPHttpParams): JLString; static; overload;
    class procedure setContentCharset(para1: OAHPHttpParams; para2: JLString); static; overload;
    class function getVersion(para1: OAHPHttpParams): OAHProtocolVersion; static; overload;
    class procedure setVersion(para1: OAHPHttpParams; para2: OAHProtocolVersion); static; overload;
    class function getUserAgent(para1: OAHPHttpParams): JLString; static; overload;
    class procedure setUserAgent(para1: OAHPHttpParams; para2: JLString); static; overload;
    class function useExpectContinue(para1: OAHPHttpParams): jboolean; static; overload;
    class procedure setUseExpectContinue(para1: OAHPHttpParams; para2: jboolean); static; overload;
  end;

  OAHAPAuthParamBean = class external 'org.apache.http.auth.params' name 'AuthParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setCredentialCharset(para1: JLString); overload; virtual;
  end;

  OAHCPClientParamBean = class external 'org.apache.http.client.params' name 'ClientParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setConnectionManagerFactoryClassName(para1: JLString); overload; virtual;
    procedure setConnectionManagerFactory(para1: OAHCClientConnectionManagerFactory); overload; virtual;
    procedure setHandleRedirects(para1: jboolean); overload; virtual;
    procedure setRejectRelativeRedirect(para1: jboolean); overload; virtual;
    procedure setMaxRedirects(para1: jint); overload; virtual;
    procedure setAllowCircularRedirects(para1: jboolean); overload; virtual;
    procedure setHandleAuthentication(para1: jboolean); overload; virtual;
    procedure setCookiePolicy(para1: JLString); overload; virtual;
    procedure setVirtualHost(para1: OAHHttpHost); overload; virtual;
    procedure setDefaultHeaders(para1: JUCollection); overload; virtual;
    procedure setDefaultHost(para1: OAHHttpHost); overload; virtual;
  end;

  OAHCPConnConnectionParamBean = class external 'org.apache.http.conn.params' name 'ConnConnectionParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setMaxStatusLineGarbage(para1: jint); overload; virtual;
  end;

  OAHCPConnManagerParamBean = class external 'org.apache.http.conn.params' name 'ConnManagerParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setTimeout(para1: jlong); overload; virtual;
    procedure setMaxTotalConnections(para1: jint); overload; virtual;
    procedure setConnectionsPerRoute(para1: OAHCPConnPerRouteBean); overload; virtual;
  end;

  OAHCPConnRouteParamBean = class external 'org.apache.http.conn.params' name 'ConnRouteParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setDefaultProxy(para1: OAHHttpHost); overload; virtual;
    procedure setLocalAddress(para1: JNInetAddress); overload; virtual;
    procedure setForcedRoute(para1: OAHCRHttpRoute); overload; virtual;
  end;

  OAHCPCookieSpecParamBean = class external 'org.apache.http.cookie.params' name 'CookieSpecParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setDatePatterns(para1: JUCollection); overload; virtual;
    procedure setSingleHeader(para1: jboolean); overload; virtual;
  end;

  OAHPHttpConnectionParamBean = class external 'org.apache.http.params' name 'HttpConnectionParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setSoTimeout(para1: jint); overload; virtual;
    procedure setTcpNoDelay(para1: jboolean); overload; virtual;
    procedure setSocketBufferSize(para1: jint); overload; virtual;
    procedure setLinger(para1: jint); overload; virtual;
    procedure setConnectionTimeout(para1: jint); overload; virtual;
    procedure setStaleCheckingEnabled(para1: jboolean); overload; virtual;
  end;

  OAHPHttpProtocolParamBean = class external 'org.apache.http.params' name 'HttpProtocolParamBean' (OAHPHttpAbstractParamBean)
  public
    constructor create(para1: OAHPHttpParams); overload;
    procedure setHttpElementCharset(para1: JLString); overload; virtual;
    procedure setContentCharset(para1: JLString); overload; virtual;
    procedure setVersion(para1: OAHHttpVersion); overload; virtual;
    procedure setUserAgent(para1: JLString); overload; virtual;
    procedure setUseExpectContinue(para1: jboolean); overload; virtual;
  end;

  OAHPAbstractHttpParams = class abstract external 'org.apache.http.params' name 'AbstractHttpParams' (JLObject, OAHPHttpParams)
  strict protected
    constructor create(); overload;
  public
    function getLongParameter(para1: JLString; para2: jlong): jlong; overload; virtual;
    function setLongParameter(para1: JLString; para2: jlong): OAHPHttpParams; overload; virtual;
    function getIntParameter(para1: JLString; para2: jint): jint; overload; virtual;
    function setIntParameter(para1: JLString; para2: jint): OAHPHttpParams; overload; virtual;
    function getDoubleParameter(para1: JLString; para2: jdouble): jdouble; overload; virtual;
    function setDoubleParameter(para1: JLString; para2: jdouble): OAHPHttpParams; overload; virtual;
    function getBooleanParameter(para1: JLString; para2: jboolean): jboolean; overload; virtual;
    function setBooleanParameter(para1: JLString; para2: jboolean): OAHPHttpParams; overload; virtual;
    function isParameterTrue(para1: JLString): jboolean; overload; virtual;
    function isParameterFalse(para1: JLString): jboolean; overload; virtual;
  end;

  OAHPBasicHttpContext = class external 'org.apache.http.protocol' name 'BasicHttpContext' (JLObject, OAHPHttpContext)
  public
    constructor create(); overload;
    constructor create(para1: OAHPHttpContext); overload;
    function getAttribute(para1: JLString): JLObject; overload; virtual;
    procedure setAttribute(para1: JLString; para2: JLObject); overload; virtual;
    function removeAttribute(para1: JLString): JLObject; overload; virtual;
  end;

  OAHPDefaultedHttpContext = class sealed external 'org.apache.http.protocol' name 'DefaultedHttpContext' (JLObject, OAHPHttpContext)
  public
    constructor create(para1: OAHPHttpContext; para2: OAHPHttpContext); overload;
    function getAttribute(para1: JLString): JLObject; overload; virtual;
    function removeAttribute(para1: JLString): JLObject; overload; virtual;
    procedure setAttribute(para1: JLString; para2: JLObject); overload; virtual;
    function getDefaults(): OAHPHttpContext; overload; virtual;
  end;

  OAHPHttpRequestHandlerRegistry = class external 'org.apache.http.protocol' name 'HttpRequestHandlerRegistry' (JLObject, OAHPHttpRequestHandlerResolver)
  public
    constructor create(); overload;
    procedure register(para1: JLString; para2: OAHPHttpRequestHandler); overload; virtual;
    procedure unregister(para1: JLString); overload; virtual;
    procedure setHandlers(para1: JUMap); overload; virtual;
    function lookup(para1: JLString): OAHPHttpRequestHandler; overload; virtual;
  strict protected
    function matchUriRequestPattern(para1: JLString; para2: JLString): jboolean; overload; virtual;
  end;

  OWDAttr = interface external 'org.w3c.dom' name 'Attr' (OWDNode)
    function getName(): JLString; overload;
    function getSpecified(): jboolean; overload;
    function getValue(): JLString; overload;
    procedure setValue(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function getOwnerElement(): OWDElement; overload;
    function getSchemaTypeInfo(): OWDTypeInfo; overload;
    function isId(): jboolean; overload;
  end;

  OWDCharacterData = interface external 'org.w3c.dom' name 'CharacterData' (OWDNode)
    function getData(): JLString; overload;  // throws org.w3c.dom.DOMException
    procedure setData(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function getLength(): jint; overload;
    function substringData(para1: jint; para2: jint): JLString; overload;  // throws org.w3c.dom.DOMException
    procedure appendData(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    procedure insertData(para1: jint; para2: JLString); overload;  // throws org.w3c.dom.DOMException
    procedure deleteData(para1: jint; para2: jint); overload;  // throws org.w3c.dom.DOMException
    procedure replaceData(para1: jint; para2: jint; para3: JLString); overload;  // throws org.w3c.dom.DOMException
  end;

  OWDDocument = interface external 'org.w3c.dom' name 'Document' (OWDNode)
    function getDoctype(): OWDDocumentType; overload;
    function getImplementation(): OWDDOMImplementation; overload;
    function getDocumentElement(): OWDElement; overload;
    function createElement(para1: JLString): OWDElement; overload;  // throws org.w3c.dom.DOMException
    function createDocumentFragment(): OWDDocumentFragment; overload;
    function createTextNode(para1: JLString): OWDText; overload;
    function createComment(para1: JLString): OWDComment; overload;
    function createCDATASection(para1: JLString): OWDCDATASection; overload;  // throws org.w3c.dom.DOMException
    function createProcessingInstruction(para1: JLString; para2: JLString): OWDProcessingInstruction; overload;  // throws org.w3c.dom.DOMException
    function createAttribute(para1: JLString): OWDAttr; overload;  // throws org.w3c.dom.DOMException
    function createEntityReference(para1: JLString): OWDEntityReference; overload;  // throws org.w3c.dom.DOMException
    function getElementsByTagName(para1: JLString): OWDNodeList; overload;
    function importNode(para1: OWDNode; para2: jboolean): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function createElementNS(para1: JLString; para2: JLString): OWDElement; overload;  // throws org.w3c.dom.DOMException
    function createAttributeNS(para1: JLString; para2: JLString): OWDAttr; overload;  // throws org.w3c.dom.DOMException
    function getElementsByTagNameNS(para1: JLString; para2: JLString): OWDNodeList; overload;
    function getElementById(para1: JLString): OWDElement; overload;
    function getInputEncoding(): JLString; overload;
    function getXmlEncoding(): JLString; overload;
    function getXmlStandalone(): jboolean; overload;
    procedure setXmlStandalone(para1: jboolean); overload;  // throws org.w3c.dom.DOMException
    function getXmlVersion(): JLString; overload;
    procedure setXmlVersion(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function getStrictErrorChecking(): jboolean; overload;
    procedure setStrictErrorChecking(para1: jboolean); overload;
    function getDocumentURI(): JLString; overload;
    procedure setDocumentURI(para1: JLString); overload;
    function adoptNode(para1: OWDNode): OWDNode; overload;  // throws org.w3c.dom.DOMException
    function getDomConfig(): OWDDOMConfiguration; overload;
    procedure normalizeDocument(); overload;
    function renameNode(para1: OWDNode; para2: JLString; para3: JLString): OWDNode; overload;  // throws org.w3c.dom.DOMException
  end;

  OWDDocumentFragment = interface external 'org.w3c.dom' name 'DocumentFragment' (OWDNode)
  end;

  OWDDocumentType = interface external 'org.w3c.dom' name 'DocumentType' (OWDNode)
    function getName(): JLString; overload;
    function getEntities(): OWDNamedNodeMap; overload;
    function getNotations(): OWDNamedNodeMap; overload;
    function getPublicId(): JLString; overload;
    function getSystemId(): JLString; overload;
    function getInternalSubset(): JLString; overload;
  end;

  OWDElement = interface external 'org.w3c.dom' name 'Element' (OWDNode)
    function getTagName(): JLString; overload;
    function getAttribute(para1: JLString): JLString; overload;
    procedure setAttribute(para1: JLString; para2: JLString); overload;  // throws org.w3c.dom.DOMException
    procedure removeAttribute(para1: JLString); overload;  // throws org.w3c.dom.DOMException
    function getAttributeNode(para1: JLString): OWDAttr; overload;
    function setAttributeNode(para1: OWDAttr): OWDAttr; overload;  // throws org.w3c.dom.DOMException
    function removeAttributeNode(para1: OWDAttr): OWDAttr; overload;  // throws org.w3c.dom.DOMException
    function getElementsByTagName(para1: JLString): OWDNodeList; overload;
    function getAttributeNS(para1: JLString; para2: JLString): JLString; overload;  // throws org.w3c.dom.DOMException
    procedure setAttributeNS(para1: JLString; para2: JLString; para3: JLString); overload;  // throws org.w3c.dom.DOMException
    procedure removeAttributeNS(para1: JLString; para2: JLString); overload;  // throws org.w3c.dom.DOMException
    function getAttributeNodeNS(para1: JLString; para2: JLString): OWDAttr; overload;  // throws org.w3c.dom.DOMException
    function setAttributeNodeNS(para1: OWDAttr): OWDAttr; overload;  // throws org.w3c.dom.DOMException
    function getElementsByTagNameNS(para1: JLString; para2: JLString): OWDNodeList; overload;  // throws org.w3c.dom.DOMException
    function hasAttribute(para1: JLString): jboolean; overload;
    function hasAttributeNS(para1: JLString; para2: JLString): jboolean; overload;  // throws org.w3c.dom.DOMException
    function getSchemaTypeInfo(): OWDTypeInfo; overload;
    procedure setIdAttribute(para1: JLString; para2: jboolean); overload;  // throws org.w3c.dom.DOMException
    procedure setIdAttributeNS(para1: JLString; para2: JLString; para3: jboolean); overload;  // throws org.w3c.dom.DOMException
    procedure setIdAttributeNode(para1: OWDAttr; para2: jboolean); overload;  // throws org.w3c.dom.DOMException
  end;

  OWDEntity = interface external 'org.w3c.dom' name 'Entity' (OWDNode)
    function getPublicId(): JLString; overload;
    function getSystemId(): JLString; overload;
    function getNotationName(): JLString; overload;
    function getInputEncoding(): JLString; overload;
    function getXmlEncoding(): JLString; overload;
    function getXmlVersion(): JLString; overload;
  end;

  OWDEntityReference = interface external 'org.w3c.dom' name 'EntityReference' (OWDNode)
  end;

  OWDNotation = interface external 'org.w3c.dom' name 'Notation' (OWDNode)
    function getPublicId(): JLString; overload;
    function getSystemId(): JLString; overload;
  end;

  OWDProcessingInstruction = interface external 'org.w3c.dom' name 'ProcessingInstruction' (OWDNode)
    function getTarget(): JLString; overload;
    function getData(): JLString; overload;
    procedure setData(para1: JLString); overload;  // throws org.w3c.dom.DOMException
  end;

  OXSHAttributeListImpl = class external 'org.xml.sax.helpers' name 'AttributeListImpl' (JLObject, OXSAttributeList)
  public
    constructor create(); overload;
    constructor create(para1: OXSAttributeList); overload;
    procedure setAttributeList(para1: OXSAttributeList); overload; virtual;
    procedure addAttribute(para1: JLString; para2: JLString; para3: JLString); overload; virtual;
    procedure removeAttribute(para1: JLString); overload; virtual;
    procedure clear(); overload; virtual;
    function getLength(): jint; overload; virtual;
    function getName(para1: jint): JLString; overload; virtual;
    function getType(para1: jint): JLString; overload; virtual;
    function getValue(para1: jint): JLString; overload; virtual;
    function getType(para1: JLString): JLString; overload; virtual;
    function getValue(para1: JLString): JLString; overload; virtual;
  end;

  OXSEAttributes2 = interface external 'org.xml.sax.ext' name 'Attributes2' (OXSAttributes)
    function isDeclared(para1: jint): jboolean; overload;
    function isDeclared(para1: JLString): jboolean; overload;
    function isDeclared(para1: JLString; para2: JLString): jboolean; overload;
    function isSpecified(para1: jint): jboolean; overload;
    function isSpecified(para1: JLString; para2: JLString): jboolean; overload;
    function isSpecified(para1: JLString): jboolean; overload;
  end;

  OXSHAttributesImpl = class external 'org.xml.sax.helpers' name 'AttributesImpl' (JLObject, OXSAttributes)
  public
    constructor create(); overload;
    constructor create(para1: OXSAttributes); overload;
    function getLength(): jint; overload; virtual;
    function getURI(para1: jint): JLString; overload; virtual;
    function getLocalName(para1: jint): JLString; overload; virtual;
    function getQName(para1: jint): JLString; overload; virtual;
    function getType(para1: jint): JLString; overload; virtual;
    function getValue(para1: jint): JLString; overload; virtual;
    function getIndex(para1: JLString; para2: JLString): jint; overload; virtual;
    function getIndex(para1: JLString): jint; overload; virtual;
    function getType(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getType(para1: JLString): JLString; overload; virtual;
    function getValue(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getValue(para1: JLString): JLString; overload; virtual;
    procedure clear(); overload; virtual;
    procedure setAttributes(para1: OXSAttributes); overload; virtual;
    procedure addAttribute(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString); overload; virtual;
    procedure setAttribute(para1: jint; para2: JLString; para3: JLString; para4: JLString; para5: JLString; para6: JLString); overload; virtual;
    procedure removeAttribute(para1: jint); overload; virtual;
    procedure setURI(para1: jint; para2: JLString); overload; virtual;
    procedure setLocalName(para1: jint; para2: JLString); overload; virtual;
    procedure setQName(para1: jint; para2: JLString); overload; virtual;
    procedure setType(para1: jint; para2: JLString); overload; virtual;
    procedure setValue(para1: jint; para2: JLString); overload; virtual;
  end;

  OXSEEntityResolver2 = interface external 'org.xml.sax.ext' name 'EntityResolver2' (OXSEntityResolver)
    function getExternalSubset(para1: JLString; para2: JLString): OXSInputSource; overload;  // throws org.xml.sax.SAXException, java.io.IOException
    function resolveEntity(para1: JLString; para2: JLString; para3: JLString; para4: JLString): OXSInputSource; overload;  // throws org.xml.sax.SAXException, java.io.IOException
  end;

  OXSHandlerBase = class external 'org.xml.sax' name 'HandlerBase' (JLObject, OXSEntityResolver, OXSDTDHandler, OXSDocumentHandler, OXSErrorHandler)
  public
    constructor create(); overload;
    function resolveEntity(para1: JLString; para2: JLString): OXSInputSource; overload; virtual;  // throws org.xml.sax.SAXException
    procedure notationDecl(para1: JLString; para2: JLString; para3: JLString); overload; virtual;
    procedure unparsedEntityDecl(para1: JLString; para2: JLString; para3: JLString; para4: JLString); overload; virtual;
    procedure setDocumentLocator(para1: OXSLocator); overload; virtual;
    procedure startDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startElement(para1: JLString; para2: OXSAttributeList); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure warning(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
    procedure error(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
    procedure fatalError(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
  end;

  OXSHDefaultHandler = class external 'org.xml.sax.helpers' name 'DefaultHandler' (JLObject, OXSEntityResolver, OXSDTDHandler, OXSContentHandler, OXSErrorHandler)
  public
    constructor create(); overload;
    function resolveEntity(para1: JLString; para2: JLString): OXSInputSource; overload; virtual;  // throws java.io.IOException, org.xml.sax.SAXException
    procedure notationDecl(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure unparsedEntityDecl(para1: JLString; para2: JLString; para3: JLString; para4: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure setDocumentLocator(para1: OXSLocator); overload; virtual;
    procedure startDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startPrefixMapping(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endPrefixMapping(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startElement(para1: JLString; para2: JLString; para3: JLString; para4: OXSAttributes); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure skippedEntity(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure warning(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
    procedure error(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
    procedure fatalError(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
  end;

  OXSELocator2 = interface external 'org.xml.sax.ext' name 'Locator2' (OXSLocator)
    function getXMLVersion(): JLString; overload;
    function getEncoding(): JLString; overload;
  end;

  OXSHLocatorImpl = class external 'org.xml.sax.helpers' name 'LocatorImpl' (JLObject, OXSLocator)
  public
    constructor create(); overload;
    constructor create(para1: OXSLocator); overload;
    function getPublicId(): JLString; overload; virtual;
    function getSystemId(): JLString; overload; virtual;
    function getLineNumber(): jint; overload; virtual;
    function getColumnNumber(): jint; overload; virtual;
    procedure setPublicId(para1: JLString); overload; virtual;
    procedure setSystemId(para1: JLString); overload; virtual;
    procedure setLineNumber(para1: jint); overload; virtual;
    procedure setColumnNumber(para1: jint); overload; virtual;
  end;

  OXSHXMLReaderAdapter = class external 'org.xml.sax.helpers' name 'XMLReaderAdapter' (JLObject, OXSParser, OXSContentHandler)
  public
    constructor create(); overload;  // throws org.xml.sax.SAXException
    constructor create(para1: OXSXMLReader); overload;
    procedure setLocale(para1: JULocale); overload; virtual;  // throws org.xml.sax.SAXException
    procedure setEntityResolver(para1: OXSEntityResolver); overload; virtual;
    procedure setDTDHandler(para1: OXSDTDHandler); overload; virtual;
    procedure setDocumentHandler(para1: OXSDocumentHandler); overload; virtual;
    procedure setErrorHandler(para1: OXSErrorHandler); overload; virtual;
    procedure parse(para1: JLString); overload; virtual;  // throws java.io.IOException, org.xml.sax.SAXException
    procedure parse(para1: OXSInputSource); overload; virtual;  // throws java.io.IOException, org.xml.sax.SAXException
    procedure setDocumentLocator(para1: OXSLocator); overload; virtual;
    procedure startDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startPrefixMapping(para1: JLString; para2: JLString); overload; virtual;
    procedure endPrefixMapping(para1: JLString); overload; virtual;
    procedure startElement(para1: JLString; para2: JLString; para3: JLString; para4: OXSAttributes); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure skippedEntity(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
  end;

  OXSXMLFilter = interface external 'org.xml.sax' name 'XMLFilter' (OXSXMLReader)
    procedure setParent(para1: OXSXMLReader); overload;
    function getParent(): OXSXMLReader; overload;
  end;

  OXSHParserAdapter = class external 'org.xml.sax.helpers' name 'ParserAdapter' (JLObject, OXSXMLReader, OXSDocumentHandler)
  public
    constructor create(); overload;  // throws org.xml.sax.SAXException
    constructor create(para1: OXSParser); overload;
    procedure setFeature(para1: JLString; para2: jboolean); overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    function getFeature(para1: JLString): jboolean; overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setProperty(para1: JLString; para2: JLObject); overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    function getProperty(para1: JLString): JLObject; overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setEntityResolver(para1: OXSEntityResolver); overload; virtual;
    function getEntityResolver(): OXSEntityResolver; overload; virtual;
    procedure setDTDHandler(para1: OXSDTDHandler); overload; virtual;
    function getDTDHandler(): OXSDTDHandler; overload; virtual;
    procedure setContentHandler(para1: OXSContentHandler); overload; virtual;
    function getContentHandler(): OXSContentHandler; overload; virtual;
    procedure setErrorHandler(para1: OXSErrorHandler); overload; virtual;
    function getErrorHandler(): OXSErrorHandler; overload; virtual;
    procedure parse(para1: JLString); overload; virtual;  // throws java.io.IOException, org.xml.sax.SAXException
    procedure parse(para1: OXSInputSource); overload; virtual;  // throws java.io.IOException, org.xml.sax.SAXException
    procedure setDocumentLocator(para1: OXSLocator); overload; virtual;
    procedure startDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startElement(para1: JLString; para2: OXSAttributeList); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
  end;

  OXVSDriver = class external 'org.xmlpull.v1.sax2' name 'Driver' (JLObject, OXSLocator, OXSXMLReader, OXSAttributes)
  strict protected
    const
      DECLARATION_HANDLER_PROPERTY = 'http://xml.org/sax/properties/declaration-handler';
      LEXICAL_HANDLER_PROPERTY = 'http://xml.org/sax/properties/lexical-handler';
      NAMESPACES_FEATURE = 'http://xml.org/sax/features/namespaces';
      NAMESPACE_PREFIXES_FEATURE = 'http://xml.org/sax/features/namespace-prefixes';
      VALIDATION_FEATURE = 'http://xml.org/sax/features/validation';
      APACHE_SCHEMA_VALIDATION_FEATURE = 'http://apache.org/xml/features/validation/schema';
      APACHE_DYNAMIC_VALIDATION_FEATURE = 'http://apache.org/xml/features/validation/dynamic';
  strict protected
    var
      fcontentHandler: OXSContentHandler; external name 'contentHandler';
      ferrorHandler: OXSErrorHandler; external name 'errorHandler';
      fsystemId: JLString; external name 'systemId';
      fpp: OXVXmlPullParser; external name 'pp';
  public
    constructor create(); overload;  // throws org.xmlpull.v1.XmlPullParserException
    constructor create(para1: OXVXmlPullParser); overload;  // throws org.xmlpull.v1.XmlPullParserException
    function getLength(): jint; overload; virtual;
    function getURI(para1: jint): JLString; overload; virtual;
    function getLocalName(para1: jint): JLString; overload; virtual;
    function getQName(para1: jint): JLString; overload; virtual;
    function getType(para1: jint): JLString; overload; virtual;
    function getValue(para1: jint): JLString; overload; virtual;
    function getIndex(para1: JLString; para2: JLString): jint; overload; virtual;
    function getIndex(para1: JLString): jint; overload; virtual;
    function getType(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getType(para1: JLString): JLString; overload; virtual;
    function getValue(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getValue(para1: JLString): JLString; overload; virtual;
    function getPublicId(): JLString; overload; virtual;
    function getSystemId(): JLString; overload; virtual;
    function getLineNumber(): jint; overload; virtual;
    function getColumnNumber(): jint; overload; virtual;
    function getFeature(para1: JLString): jboolean; overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setFeature(para1: JLString; para2: jboolean); overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    function getProperty(para1: JLString): JLObject; overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setProperty(para1: JLString; para2: JLObject); overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setEntityResolver(para1: OXSEntityResolver); overload; virtual;
    function getEntityResolver(): OXSEntityResolver; overload; virtual;
    procedure setDTDHandler(para1: OXSDTDHandler); overload; virtual;
    function getDTDHandler(): OXSDTDHandler; overload; virtual;
    procedure setContentHandler(para1: OXSContentHandler); overload; virtual;
    function getContentHandler(): OXSContentHandler; overload; virtual;
    procedure setErrorHandler(para1: OXSErrorHandler); overload; virtual;
    function getErrorHandler(): OXSErrorHandler; overload; virtual;
    procedure parse(para1: OXSInputSource); overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    procedure parse(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    procedure parseSubTree(para1: OXVXmlPullParser); overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
  strict protected
    procedure startElement(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
  end;

  OAHCRHttpRoute = class sealed external 'org.apache.http.conn.routing' name 'HttpRoute' (JLObject, OAHCRRouteInfo, JLCloneable)
  public
    constructor create(para1: OAHHttpHost; para2: JNInetAddress; para3: Arr1OAHHttpHost; para4: jboolean; para5: OAHCRRouteInfo.InnerTunnelType; para6: OAHCRRouteInfo.InnerLayerType); overload;
    constructor create(para1: OAHHttpHost; para2: JNInetAddress; const para3: array of OAHHttpHost; para4: jboolean; para5: OAHCRRouteInfo.InnerTunnelType; para6: OAHCRRouteInfo.InnerLayerType); overload;
    constructor create(para1: OAHHttpHost; para2: JNInetAddress; para3: OAHHttpHost; para4: jboolean; para5: OAHCRRouteInfo.InnerTunnelType; para6: OAHCRRouteInfo.InnerLayerType); overload;
    constructor create(para1: OAHHttpHost; para2: JNInetAddress; para3: jboolean); overload;
    constructor create(para1: OAHHttpHost); overload;
    constructor create(para1: OAHHttpHost; para2: JNInetAddress; para3: OAHHttpHost; para4: jboolean); overload;
    function getTargetHost(): OAHHttpHost; overload; virtual; final;
    function getLocalAddress(): JNInetAddress; overload; virtual; final;
    function getHopCount(): jint; overload; virtual; final;
    function getHopTarget(para1: jint): OAHHttpHost; overload; virtual; final;
    function getProxyHost(): OAHHttpHost; overload; virtual; final;
    function getTunnelType(): OAHCRRouteInfo.InnerTunnelType; overload; virtual; final;
    function isTunnelled(): jboolean; overload; virtual; final;
    function getLayerType(): OAHCRRouteInfo.InnerLayerType; overload; virtual; final;
    function isLayered(): jboolean; overload; virtual; final;
    function isSecure(): jboolean; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function hashCode(): jint; overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHCRRouteTracker = class sealed external 'org.apache.http.conn.routing' name 'RouteTracker' (JLObject, OAHCRRouteInfo, JLCloneable)
  public
    constructor create(para1: OAHHttpHost; para2: JNInetAddress); overload;
    constructor create(para1: OAHCRHttpRoute); overload;
    procedure connectTarget(para1: jboolean); overload; virtual; final;
    procedure connectProxy(para1: OAHHttpHost; para2: jboolean); overload; virtual; final;
    procedure tunnelTarget(para1: jboolean); overload; virtual; final;
    procedure tunnelProxy(para1: OAHHttpHost; para2: jboolean); overload; virtual; final;
    procedure layerProtocol(para1: jboolean); overload; virtual; final;
    function getTargetHost(): OAHHttpHost; overload; virtual; final;
    function getLocalAddress(): JNInetAddress; overload; virtual; final;
    function getHopCount(): jint; overload; virtual; final;
    function getHopTarget(para1: jint): OAHHttpHost; overload; virtual; final;
    function getProxyHost(): OAHHttpHost; overload; virtual; final;
    function isConnected(): jboolean; overload; virtual; final;
    function getTunnelType(): OAHCRRouteInfo.InnerTunnelType; overload; virtual; final;
    function isTunnelled(): jboolean; overload; virtual; final;
    function getLayerType(): OAHCRRouteInfo.InnerLayerType; overload; virtual; final;
    function isLayered(): jboolean; overload; virtual; final;
    function isSecure(): jboolean; overload; virtual; final;
    function toRoute(): OAHCRHttpRoute; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function hashCode(): jint; overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OACGPSListener = class external 'org.apache.cordova' name 'GPSListener' (OACCordovaLocationListener)
  public
    constructor create(para1: ALLocationManager; para2: OACGeoBroker); overload;
  strict protected
    procedure start(); overload; virtual;
  end;

  OACNetworkListener = class external 'org.apache.cordova' name 'NetworkListener' (OACCordovaLocationListener)
  public
    constructor create(para1: ALLocationManager; para2: OACGeoBroker); overload;
  end;

  OACIceCreamCordovaWebViewClient = class external 'org.apache.cordova' name 'IceCreamCordovaWebViewClient' (OACCordovaWebViewClient)
  public
    constructor create(para1: OACACordovaInterface); overload;
    constructor create(para1: OACACordovaInterface; para2: OACCordovaWebView); overload;
    function shouldInterceptRequest(para1: AWWebView; para2: JLString): AWWebResourceResponse; overload; virtual;
  end;

  OAHMethodNotSupportedException = class external 'org.apache.http' name 'MethodNotSupportedException' (OAHHttpException)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHProtocolException = class external 'org.apache.http' name 'ProtocolException' (OAHHttpException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHICTunnelRefusedException = class external 'org.apache.http.impl.client' name 'TunnelRefusedException' (OAHHttpException)
  public
    constructor create(para1: JLString; para2: OAHHttpResponse); overload;
    function getResponse(): OAHHttpResponse; overload; virtual;
  end;

  OXSSAXNotRecognizedException = class external 'org.xml.sax' name 'SAXNotRecognizedException' (OXSSAXException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OXSSAXNotSupportedException = class external 'org.xml.sax' name 'SAXNotSupportedException' (OXSSAXException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OXSSAXParseException = class external 'org.xml.sax' name 'SAXParseException' (OXSSAXException)
  public
    constructor create(para1: JLString; para2: OXSLocator); overload;
    constructor create(para1: JLString; para2: OXSLocator; para3: JLException); overload;
    constructor create(para1: JLString; para2: JLString; para3: JLString; para4: jint; para5: jint); overload;
    constructor create(para1: JLString; para2: JLString; para3: JLString; para4: jint; para5: jint; para6: JLException); overload;
    function getPublicId(): JLString; overload; virtual;
    function getSystemId(): JLString; overload; virtual;
    function getLineNumber(): jint; overload; virtual;
    function getColumnNumber(): jint; overload; virtual;
  end;

  OAHCHttpResponseException = class external 'org.apache.http.client' name 'HttpResponseException' (OAHCClientProtocolException)
  public
    constructor create(para1: jint; para2: JLString); overload;
    function getStatusCode(): jint; overload; virtual;
  end;

  OAHMBasicHeaderElementIterator = class external 'org.apache.http.message' name 'BasicHeaderElementIterator' (JLObject, OAHHeaderElementIterator)
  public
    constructor create(para1: OAHHeaderIterator; para2: OAHMHeaderValueParser); overload;
    constructor create(para1: OAHHeaderIterator); overload;
    function hasNext(): jboolean; overload; virtual;
    function nextElement(): OAHHeaderElement; overload; virtual;  // throws java.util.NoSuchElementException
    function next(): JLObject; overload; virtual; final;  // throws java.util.NoSuchElementException
    procedure remove(); overload; virtual;  // throws java.lang.UnsupportedOperationException
  end;

  OAHMBasicHeaderIterator = class external 'org.apache.http.message' name 'BasicHeaderIterator' (JLObject, OAHHeaderIterator)
  strict protected
    final var
      fallHeaders: Arr1OAHHeader; external name 'allHeaders';
  strict protected
    var
      fcurrentIndex: jint; external name 'currentIndex';
      fheaderName: JLString; external name 'headerName';
  public
    constructor create(para1: Arr1OAHHeader; para2: JLString); overload;
    constructor create(const para1: array of OAHHeader; para2: JLString); overload;
  strict protected
    function findNext(para1: jint): jint; overload; virtual;
    function filterHeader(para1: jint): jboolean; overload; virtual;
  public
    function hasNext(): jboolean; overload; virtual;
    function nextHeader(): OAHHeader; overload; virtual;  // throws java.util.NoSuchElementException
    function next(): JLObject; overload; virtual; final;  // throws java.util.NoSuchElementException
    procedure remove(); overload; virtual;  // throws java.lang.UnsupportedOperationException
  end;

  OAHMBasicListHeaderIterator = class external 'org.apache.http.message' name 'BasicListHeaderIterator' (JLObject, OAHHeaderIterator)
  strict protected
    final var
      fallHeaders: JUList; external name 'allHeaders';
  strict protected
    var
      fcurrentIndex: jint; external name 'currentIndex';
      flastIndex: jint; external name 'lastIndex';
      fheaderName: JLString; external name 'headerName';
  public
    constructor create(para1: JUList; para2: JLString); overload;
  strict protected
    function findNext(para1: jint): jint; overload; virtual;
    function filterHeader(para1: jint): jboolean; overload; virtual;
  public
    function hasNext(): jboolean; overload; virtual;
    function nextHeader(): OAHHeader; overload; virtual;  // throws java.util.NoSuchElementException
    function next(): JLObject; overload; virtual; final;  // throws java.util.NoSuchElementException
    procedure remove(); overload; virtual;  // throws java.lang.UnsupportedOperationException
  end;

  OAHMBasicTokenIterator = class external 'org.apache.http.message' name 'BasicTokenIterator' (JLObject, OAHTokenIterator)
  public
    const
      HTTP_SEPARATORS = ' ,;=()<>@:\"/[]?{}'#$0009'';
  strict protected
    final var
      fheaderIt: OAHHeaderIterator; external name 'headerIt';
  strict protected
    var
      fcurrentHeader: JLString; external name 'currentHeader';
      fcurrentToken: JLString; external name 'currentToken';
      fsearchPos: jint; external name 'searchPos';
  public
    constructor create(para1: OAHHeaderIterator); overload;
    function hasNext(): jboolean; overload; virtual;
    function nextToken(): JLString; overload; virtual;  // throws java.util.NoSuchElementException, org.apache.http.ParseException
    function next(): JLObject; overload; virtual; final;  // throws java.util.NoSuchElementException, org.apache.http.ParseException
    procedure remove(); overload; virtual; final;  // throws java.lang.UnsupportedOperationException
  strict protected
    function findNext(para1: jint): jint; overload; virtual;  // throws org.apache.http.ParseException
    function createToken(para1: JLString; para2: jint; para3: jint): JLString; overload; virtual;
    function findTokenStart(para1: jint): jint; overload; virtual;
    function findTokenSeparator(para1: jint): jint; overload; virtual;
    function findTokenEnd(para1: jint): jint; overload; virtual;
    function isTokenSeparator(para1: jchar): jboolean; overload; virtual;
    function isWhitespace(para1: jchar): jboolean; overload; virtual;
    function isTokenChar(para1: jchar): jboolean; overload; virtual;
    function isHttpSeparator(para1: jchar): jboolean; overload; virtual;
  end;

  OAHHttpVersion = class sealed external 'org.apache.http' name 'HttpVersion' (OAHProtocolVersion, JISerializable)
  public
    const
      HTTP = 'HTTP';
  public
    final class var
      fHTTP_0_9: OAHHttpVersion; external name 'HTTP_0_9';
      fHTTP_1_0: OAHHttpVersion; external name 'HTTP_1_0';
      fHTTP_1_1: OAHHttpVersion; external name 'HTTP_1_1';
  public
    constructor create(para1: jint; para2: jint); overload;
    function forVersion(para1: jint; para2: jint): OAHProtocolVersion; overload; virtual;
  end;

  OAHCConnectionPoolTimeoutException = class external 'org.apache.http.conn' name 'ConnectionPoolTimeoutException' (OAHCConnectTimeoutException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OAHCSAbstractVerifier = class abstract external 'org.apache.http.conn.ssl' name 'AbstractVerifier' (JLObject, OAHCSX509HostnameVerifier)
  public
    constructor create(); overload;
    procedure verify(para1: JLString; para2: JNSSSLSocket); overload; virtual; final;  // throws java.io.IOException
    function verify(para1: JLString; para2: JNSSSLSession): jboolean; overload; virtual; final;
    procedure verify(para1: JLString; para2: JSCX509Certificate); overload; virtual; final;  // throws javax.net.ssl.SSLException
    procedure verify(para1: JLString; para2: Arr1JLString; para3: Arr1JLString; para4: jboolean); overload; virtual; final;  // throws javax.net.ssl.SSLException
    procedure verify(para1: JLString; var para2: array of JLString; var para3: array of JLString; para4: jboolean); overload; virtual; final;  // throws javax.net.ssl.SSLException
    class function acceptableCountryWildcard(para1: JLString): jboolean; static; overload;
    class function getCNs(para1: JSCX509Certificate): Arr1JLString; static; overload;
    class function getDNSSubjectAlts(para1: JSCX509Certificate): Arr1JLString; static; overload;
    class function countDots(para1: JLString): jint; static; overload;
  end;

  OACStandAlone = class external 'org.apache.cordova' name 'StandAlone' (OACDroidGap)
  public
    constructor create(); overload;
    procedure onCreate(para1: AOBundle); overload; virtual;
  end;

  OAHMBufferedHeader = class external 'org.apache.http.message' name 'BufferedHeader' (JLObject, OAHFormattedHeader, JLCloneable)
  public
    constructor create(para1: OAHUCharArrayBuffer); overload;  // throws org.apache.http.ParseException
    function getName(): JLString; overload; virtual;
    function getValue(): JLString; overload; virtual;
    function getElements(): Arr1OAHHeaderElement; overload; virtual;  // throws org.apache.http.ParseException
    function getValuePos(): jint; overload; virtual;
    function getBuffer(): OAHUCharArrayBuffer; overload; virtual;
    function toString(): JLString; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHIAbstractHttpClientConnection = class abstract external 'org.apache.http.impl' name 'AbstractHttpClientConnection' (JLObject, OAHHttpClientConnection)
  public
    constructor create(); overload;
  strict protected
    procedure assertOpen(); overload; virtual; abstract;  // throws java.lang.IllegalStateException
    function createEntityDeserializer(): OAHIEEntityDeserializer; overload; virtual;
    function createEntitySerializer(): OAHIEEntitySerializer; overload; virtual;
    function createHttpResponseFactory(): OAHHttpResponseFactory; overload; virtual;
    function createResponseParser(para1: OAHISessionInputBuffer; para2: OAHHttpResponseFactory; para3: OAHPHttpParams): OAHIHttpMessageParser; overload; virtual;
    function createRequestWriter(para1: OAHISessionOutputBuffer; para2: OAHPHttpParams): OAHIHttpMessageWriter; overload; virtual;
    procedure init(para1: OAHISessionInputBuffer; para2: OAHISessionOutputBuffer; para3: OAHPHttpParams); overload; virtual;
  public
    function isResponseAvailable(para1: jint): jboolean; overload; virtual;  // throws java.io.IOException
    procedure sendRequestHeader(para1: OAHHttpRequest); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendRequestEntity(para1: OAHHttpEntityEnclosingRequest); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  strict protected
    procedure doFlush(); overload; virtual;  // throws java.io.IOException
  public
    procedure flush(); overload; virtual;  // throws java.io.IOException
    function receiveResponseHeader(): OAHHttpResponse; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure receiveResponseEntity(para1: OAHHttpResponse); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function isStale(): jboolean; overload; virtual;
    function getMetrics(): OAHHttpConnectionMetrics; overload; virtual;
  end;

  OAHCManagedClientConnection = interface external 'org.apache.http.conn' name 'ManagedClientConnection' (OAHHttpClientConnection, OAHHttpInetConnection, OAHCConnectionReleaseTrigger)
    function isSecure(): jboolean; overload;
    function getRoute(): OAHCRHttpRoute; overload;
    function getSSLSession(): JNSSSLSession; overload;
    procedure open(para1: OAHCRHttpRoute; para2: OAHPHttpContext; para3: OAHPHttpParams); overload;  // throws java.io.IOException
    procedure tunnelTarget(para1: jboolean; para2: OAHPHttpParams); overload;  // throws java.io.IOException
    procedure tunnelProxy(para1: OAHHttpHost; para2: jboolean; para3: OAHPHttpParams); overload;  // throws java.io.IOException
    procedure layerProtocol(para1: OAHPHttpContext; para2: OAHPHttpParams); overload;  // throws java.io.IOException
    procedure markReusable(); overload;
    procedure unmarkReusable(); overload;
    function isMarkedReusable(): jboolean; overload;
    procedure setState(para1: JLObject); overload;
    function getState(): JLObject; overload;
    procedure setIdleDuration(para1: jlong; para2: JUCTimeUnit); overload;
  end;

  OAHCOperatedClientConnection = interface external 'org.apache.http.conn' name 'OperatedClientConnection' (OAHHttpClientConnection, OAHHttpInetConnection)
    function getTargetHost(): OAHHttpHost; overload;
    function isSecure(): jboolean; overload;
    function getSocket(): JNSocket; overload;
    procedure opening(para1: JNSocket; para2: OAHHttpHost); overload;  // throws java.io.IOException
    procedure openCompleted(para1: jboolean; para2: OAHPHttpParams); overload;  // throws java.io.IOException
    procedure update(para1: JNSocket; para2: OAHHttpHost; para3: jboolean; para4: OAHPHttpParams); overload;  // throws java.io.IOException
  end;

  OAHIAbstractHttpServerConnection = class abstract external 'org.apache.http.impl' name 'AbstractHttpServerConnection' (JLObject, OAHHttpServerConnection)
  public
    constructor create(); overload;
  strict protected
    procedure assertOpen(); overload; virtual; abstract;  // throws java.lang.IllegalStateException
    function createEntityDeserializer(): OAHIEEntityDeserializer; overload; virtual;
    function createEntitySerializer(): OAHIEEntitySerializer; overload; virtual;
    function createHttpRequestFactory(): OAHHttpRequestFactory; overload; virtual;
    function createRequestParser(para1: OAHISessionInputBuffer; para2: OAHHttpRequestFactory; para3: OAHPHttpParams): OAHIHttpMessageParser; overload; virtual;
    function createResponseWriter(para1: OAHISessionOutputBuffer; para2: OAHPHttpParams): OAHIHttpMessageWriter; overload; virtual;
    procedure init(para1: OAHISessionInputBuffer; para2: OAHISessionOutputBuffer; para3: OAHPHttpParams); overload; virtual;
  public
    function receiveRequestHeader(): OAHHttpRequest; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure receiveRequestEntity(para1: OAHHttpEntityEnclosingRequest); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  strict protected
    procedure doFlush(); overload; virtual;  // throws java.io.IOException
  public
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure sendResponseHeader(para1: OAHHttpResponse); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendResponseEntity(para1: OAHHttpResponse); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function isStale(): jboolean; overload; virtual;
    function getMetrics(): OAHHttpConnectionMetrics; overload; virtual;
  end;

  OAHEBasicHttpEntity = class external 'org.apache.http.entity' name 'BasicHttpEntity' (OAHEAbstractHttpEntity)
  public
    constructor create(); overload;
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.lang.IllegalStateException
    function isRepeatable(): jboolean; overload; virtual;
    procedure setContentLength(para1: jlong); overload; virtual;
    procedure setContent(para1: JIInputStream); overload; virtual;
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException
  end;

  OAHEByteArrayEntity = class external 'org.apache.http.entity' name 'ByteArrayEntity' (OAHEAbstractHttpEntity, JLCloneable)
  strict protected
    final var
      fcontent: Arr1jbyte; external name 'content';
  public
    constructor create(para1: Arr1jbyte); overload;
    constructor create(const para1: array of jbyte); overload;
    function isRepeatable(): jboolean; overload; virtual;
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHEEntityTemplate = class external 'org.apache.http.entity' name 'EntityTemplate' (OAHEAbstractHttpEntity)
  public
    constructor create(para1: OAHEContentProducer); overload;
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;
    function isRepeatable(): jboolean; overload; virtual;
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException
  end;

  OAHEFileEntity = class external 'org.apache.http.entity' name 'FileEntity' (OAHEAbstractHttpEntity, JLCloneable)
  strict protected
    final var
      ffile: JIFile; external name 'file';
  public
    constructor create(para1: JIFile; para2: JLString); overload;
    function isRepeatable(): jboolean; overload; virtual;
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHEInputStreamEntity = class external 'org.apache.http.entity' name 'InputStreamEntity' (OAHEAbstractHttpEntity)
  public
    constructor create(para1: JIInputStream; para2: jlong); overload;
    function isRepeatable(): jboolean; overload; virtual;
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException
  end;

  OAHESerializableEntity = class external 'org.apache.http.entity' name 'SerializableEntity' (OAHEAbstractHttpEntity)
  public
    constructor create(para1: JISerializable; para2: jboolean); overload;  // throws java.io.IOException
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException, java.lang.IllegalStateException
    function getContentLength(): jlong; overload; virtual;
    function isRepeatable(): jboolean; overload; virtual;
    function isStreaming(): jboolean; overload; virtual;
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
  end;

  OAHEStringEntity = class external 'org.apache.http.entity' name 'StringEntity' (OAHEAbstractHttpEntity, JLCloneable)
  strict protected
    final var
      fcontent: Arr1jbyte; external name 'content';
  public
    constructor create(para1: JLString; para2: JLString); overload;  // throws java.io.UnsupportedEncodingException
    constructor create(para1: JLString); overload;  // throws java.io.UnsupportedEncodingException
    function isRepeatable(): jboolean; overload; virtual;
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHCBasicManagedEntity = class external 'org.apache.http.conn' name 'BasicManagedEntity' (OAHEHttpEntityWrapper, OAHCConnectionReleaseTrigger, OAHCEofSensorWatcher)
  strict protected
    var
      fmanagedConn: OAHCManagedClientConnection; external name 'managedConn';
  strict protected
    final var
      fattemptReuse: jboolean; external name 'attemptReuse';
  public
    constructor create(para1: OAHHttpEntity; para2: OAHCManagedClientConnection; para3: jboolean); overload;
    function isRepeatable(): jboolean; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    procedure releaseConnection(); overload; virtual;  // throws java.io.IOException
    procedure abortConnection(); overload; virtual;  // throws java.io.IOException
    function eofDetected(para1: JIInputStream): jboolean; overload; virtual;  // throws java.io.IOException
    function streamClosed(para1: JIInputStream): jboolean; overload; virtual;  // throws java.io.IOException
    function streamAbort(para1: JIInputStream): jboolean; overload; virtual;  // throws java.io.IOException
  strict protected
    procedure releaseManagedConnection(); overload; virtual;  // throws java.io.IOException
  end;

  OAHEBufferedHttpEntity = class external 'org.apache.http.entity' name 'BufferedHttpEntity' (OAHEHttpEntityWrapper)
  public
    constructor create(para1: OAHHttpEntity); overload;  // throws java.io.IOException
    function getContentLength(): jlong; overload; virtual;
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException
    function isChunked(): jboolean; overload; virtual;
    function isRepeatable(): jboolean; overload; virtual;
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function isStreaming(): jboolean; overload; virtual;
  end;

  OAHHttpEntityEnclosingRequest = interface external 'org.apache.http' name 'HttpEntityEnclosingRequest' (OAHHttpRequest)
    function expectContinue(): jboolean; overload;
    procedure setEntity(para1: OAHHttpEntity); overload;
    function getEntity(): OAHHttpEntity; overload;
  end;

  OAHCMHttpUriRequest = interface external 'org.apache.http.client.methods' name 'HttpUriRequest' (OAHHttpRequest)
    function getMethod(): JLString; overload;
    function getURI(): JNURI; overload;
    procedure abort(); overload;  // throws java.lang.UnsupportedOperationException
    function isAborted(): jboolean; overload;
  end;

  OAHMBasicHttpRequest = class external 'org.apache.http.message' name 'BasicHttpRequest' (OAHMAbstractHttpMessage, OAHHttpRequest)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    constructor create(para1: JLString; para2: JLString; para3: OAHProtocolVersion); overload;
    constructor create(para1: OAHRequestLine); overload;
    function getProtocolVersion(): OAHProtocolVersion; overload; virtual;
    function getRequestLine(): OAHRequestLine; overload; virtual;
  end;

  OAHMBasicHttpResponse = class external 'org.apache.http.message' name 'BasicHttpResponse' (OAHMAbstractHttpMessage, OAHHttpResponse)
  public
    constructor create(para1: OAHStatusLine; para2: OAHReasonPhraseCatalog; para3: JULocale); overload;
    constructor create(para1: OAHStatusLine); overload;
    constructor create(para1: OAHProtocolVersion; para2: jint; para3: JLString); overload;
    function getProtocolVersion(): OAHProtocolVersion; overload; virtual;
    function getStatusLine(): OAHStatusLine; overload; virtual;
    function getEntity(): OAHHttpEntity; overload; virtual;
    function getLocale(): JULocale; overload; virtual;
    procedure setStatusLine(para1: OAHStatusLine); overload; virtual;
    procedure setStatusLine(para1: OAHProtocolVersion; para2: jint); overload; virtual;
    procedure setStatusLine(para1: OAHProtocolVersion; para2: jint; para3: JLString); overload; virtual;
    procedure setStatusCode(para1: jint); overload; virtual;
    procedure setReasonPhrase(para1: JLString); overload; virtual;
    procedure setEntity(para1: OAHHttpEntity); overload; virtual;
    procedure setLocale(para1: JULocale); overload; virtual;
  strict protected
    function getReason(para1: jint): JLString; overload; virtual;
  end;

  OAHPBasicHttpProcessor = class sealed external 'org.apache.http.protocol' name 'BasicHttpProcessor' (JLObject, OAHPHttpProcessor, OAHPHttpRequestInterceptorList, OAHPHttpResponseInterceptorList, JLCloneable)
  strict protected
    var
      frequestInterceptors: JUList; external name 'requestInterceptors';
      fresponseInterceptors: JUList; external name 'responseInterceptors';
  public
    constructor create(); overload;
    procedure addRequestInterceptor(para1: OAHHttpRequestInterceptor); overload; virtual;
    procedure addRequestInterceptor(para1: OAHHttpRequestInterceptor; para2: jint); overload; virtual;
    procedure addResponseInterceptor(para1: OAHHttpResponseInterceptor; para2: jint); overload; virtual;
    procedure removeRequestInterceptorByClass(para1: JLClass); overload; virtual;
    procedure removeResponseInterceptorByClass(para1: JLClass); overload; virtual;
    procedure addInterceptor(para1: OAHHttpRequestInterceptor); overload; virtual; final;
    procedure addInterceptor(para1: OAHHttpRequestInterceptor; para2: jint); overload; virtual; final;
    function getRequestInterceptorCount(): jint; overload; virtual;
    function getRequestInterceptor(para1: jint): OAHHttpRequestInterceptor; overload; virtual;
    procedure clearRequestInterceptors(); overload; virtual;
    procedure addResponseInterceptor(para1: OAHHttpResponseInterceptor); overload; virtual;
    procedure addInterceptor(para1: OAHHttpResponseInterceptor); overload; virtual; final;
    procedure addInterceptor(para1: OAHHttpResponseInterceptor; para2: jint); overload; virtual; final;
    function getResponseInterceptorCount(): jint; overload; virtual;
    function getResponseInterceptor(para1: jint): OAHHttpResponseInterceptor; overload; virtual;
    procedure clearResponseInterceptors(); overload; virtual;
    procedure setInterceptors(para1: JUList); overload; virtual;
    procedure clearInterceptors(); overload; virtual;
    procedure process(para1: OAHHttpRequest; para2: OAHPHttpContext); overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
    procedure process(para1: OAHHttpResponse; para2: OAHPHttpContext); overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
  strict protected
    procedure copyInterceptors(para1: OAHPBasicHttpProcessor); overload; virtual;
  public
    function copy(): OAHPBasicHttpProcessor; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHIANTLMScheme = class external 'org.apache.http.impl.auth' name 'NTLMScheme' (OAHIAAuthSchemeBase)
  public
    constructor create(para1: OAHIANTLMEngine); overload;
    function getSchemeName(): JLString; overload; virtual;
    function getParameter(para1: JLString): JLString; overload; virtual;
    function getRealm(): JLString; overload; virtual;
    function isConnectionBased(): jboolean; overload; virtual;
  strict protected
    procedure parseChallenge(para1: OAHUCharArrayBuffer; para2: jint; para3: jint); overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
  public
    function authenticate(para1: OAHACredentials; para2: OAHHttpRequest): OAHHeader; overload; virtual;  // throws org.apache.http.auth.AuthenticationException
    function isComplete(): jboolean; overload; virtual;
  end;

  OAHIARFC2617Scheme = class abstract external 'org.apache.http.impl.auth' name 'RFC2617Scheme' (OAHIAAuthSchemeBase)
  public
    constructor create(); overload;
  strict protected
    procedure parseChallenge(para1: OAHUCharArrayBuffer; para2: jint; para3: jint); overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
    function getParameters(): JUMap; overload; virtual;
  public
    function getParameter(para1: JLString): JLString; overload; virtual;
    function getRealm(): JLString; overload; virtual;
  end;

  OAHICDefaultProxyAuthenticationHandler = class external 'org.apache.http.impl.client' name 'DefaultProxyAuthenticationHandler' (OAHICAbstractAuthenticationHandler)
  public
    constructor create(); overload;
    function isAuthenticationRequested(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload; virtual;
    function getChallenges(para1: OAHHttpResponse; para2: OAHPHttpContext): JUMap; overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
  end;

  OAHICDefaultTargetAuthenticationHandler = class external 'org.apache.http.impl.client' name 'DefaultTargetAuthenticationHandler' (OAHICAbstractAuthenticationHandler)
  public
    constructor create(); overload;
    function isAuthenticationRequested(para1: OAHHttpResponse; para2: OAHPHttpContext): jboolean; overload; virtual;
    function getChallenges(para1: OAHHttpResponse; para2: OAHPHttpContext): JUMap; overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
  end;

  OAHICDefaultHttpClient = class external 'org.apache.http.impl.client' name 'DefaultHttpClient' (OAHICAbstractHttpClient)
  public
    constructor create(para1: OAHCClientConnectionManager; para2: OAHPHttpParams); overload;
    constructor create(para1: OAHPHttpParams); overload;
    constructor create(); overload;
  strict protected
    function createHttpParams(): OAHPHttpParams; overload; virtual;
    function createRequestExecutor(): OAHPHttpRequestExecutor; overload; virtual;
    function createClientConnectionManager(): OAHCClientConnectionManager; overload; virtual;
    function createHttpContext(): OAHPHttpContext; overload; virtual;
    function createConnectionReuseStrategy(): OAHConnectionReuseStrategy; overload; virtual;
    function createConnectionKeepAliveStrategy(): OAHCConnectionKeepAliveStrategy; overload; virtual;
    function createAuthSchemeRegistry(): OAHAAuthSchemeRegistry; overload; virtual;
    function createCookieSpecRegistry(): OAHCCookieSpecRegistry; overload; virtual;
    function createHttpProcessor(): OAHPBasicHttpProcessor; overload; virtual;
    function createHttpRequestRetryHandler(): OAHCHttpRequestRetryHandler; overload; virtual;
    function createRedirectHandler(): OAHCRedirectHandler; overload; virtual;
    function createTargetAuthenticationHandler(): OAHCAuthenticationHandler; overload; virtual;
    function createProxyAuthenticationHandler(): OAHCAuthenticationHandler; overload; virtual;
    function createCookieStore(): OAHCCookieStore; overload; virtual;
    function createCredentialsProvider(): OAHCCredentialsProvider; overload; virtual;
    function createHttpRoutePlanner(): OAHCRHttpRoutePlanner; overload; virtual;
    function createUserTokenHandler(): OAHCUserTokenHandler; overload; virtual;
  end;

  OAHCSSSLSocketFactory = class external 'org.apache.http.conn.ssl' name 'SSLSocketFactory' (JLObject, OAHCSLayeredSocketFactory)
  public
    const
      TLS = 'TLS';
      SSL = 'SSL';
      SSLV2 = 'SSLv2';
  public
    final class var
      fALLOW_ALL_HOSTNAME_VERIFIER: OAHCSX509HostnameVerifier; external name 'ALLOW_ALL_HOSTNAME_VERIFIER';
      fBROWSER_COMPATIBLE_HOSTNAME_VERIFIER: OAHCSX509HostnameVerifier; external name 'BROWSER_COMPATIBLE_HOSTNAME_VERIFIER';
      fSTRICT_HOSTNAME_VERIFIER: OAHCSX509HostnameVerifier; external name 'STRICT_HOSTNAME_VERIFIER';
  public
    constructor create(para1: JLString; para2: JSKeyStore; para3: JLString; para4: JSKeyStore; para5: JSSecureRandom; para6: OAHCSHostNameResolver); overload;  // throws java.security.NoSuchAlgorithmException, java.security.KeyManagementException, java.security.KeyStoreException, java.security.UnrecoverableKeyException
    constructor create(para1: JSKeyStore; para2: JLString; para3: JSKeyStore); overload;  // throws java.security.NoSuchAlgorithmException, java.security.KeyManagementException, java.security.KeyStoreException, java.security.UnrecoverableKeyException
    constructor create(para1: JSKeyStore; para2: JLString); overload;  // throws java.security.NoSuchAlgorithmException, java.security.KeyManagementException, java.security.KeyStoreException, java.security.UnrecoverableKeyException
    constructor create(para1: JSKeyStore); overload;  // throws java.security.NoSuchAlgorithmException, java.security.KeyManagementException, java.security.KeyStoreException, java.security.UnrecoverableKeyException
    class function getSocketFactory(): OAHCSSSLSocketFactory; static; overload;
    function createSocket(): JNSocket; overload; virtual;  // throws java.io.IOException
    function connectSocket(para1: JNSocket; para2: JLString; para3: jint; para4: JNInetAddress; para5: jint; para6: OAHPHttpParams): JNSocket; overload; virtual;  // throws java.io.IOException
    function isSecure(para1: JNSocket): jboolean; overload; virtual;  // throws java.lang.IllegalArgumentException
    function createSocket(para1: JNSocket; para2: JLString; para3: jint; para4: jboolean): JNSocket; overload; virtual;  // throws java.io.IOException, java.net.UnknownHostException
    procedure setHostnameVerifier(para1: OAHCSX509HostnameVerifier); overload; virtual;
    function getHostnameVerifier(): OAHCSX509HostnameVerifier; overload; virtual;
  end;

  OAHCSetCookie2 = interface external 'org.apache.http.cookie' name 'SetCookie2' (OAHCSetCookie)
    procedure setCommentURL(para1: JLString); overload;
    procedure setPorts(para1: Arr1jint); overload;
    procedure setPorts(var para1: array of jint); overload;
    procedure setDiscard(para1: jboolean); overload;
  end;

  OAHICBasicClientCookie = class external 'org.apache.http.impl.cookie' name 'BasicClientCookie' (JLObject, OAHCSetCookie, OAHCClientCookie, JLCloneable)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    function getName(): JLString; overload; virtual;
    function getValue(): JLString; overload; virtual;
    procedure setValue(para1: JLString); overload; virtual;
    function getComment(): JLString; overload; virtual;
    procedure setComment(para1: JLString); overload; virtual;
    function getCommentURL(): JLString; overload; virtual;
    function getExpiryDate(): JUDate; overload; virtual;
    procedure setExpiryDate(para1: JUDate); overload; virtual;
    function isPersistent(): jboolean; overload; virtual;
    function getDomain(): JLString; overload; virtual;
    procedure setDomain(para1: JLString); overload; virtual;
    function getPath(): JLString; overload; virtual;
    procedure setPath(para1: JLString); overload; virtual;
    function isSecure(): jboolean; overload; virtual;
    procedure setSecure(para1: jboolean); overload; virtual;
    function getPorts(): Arr1jint; overload; virtual;
    function getVersion(): jint; overload; virtual;
    procedure setVersion(para1: jint); overload; virtual;
    function isExpired(para1: JUDate): jboolean; overload; virtual;
    procedure setAttribute(para1: JLString; para2: JLString); overload; virtual;
    function getAttribute(para1: JLString): JLString; overload; virtual;
    function containsAttribute(para1: JLString): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
    function toString(): JLString; overload; virtual;
  end;

  OAHICBasicCommentHandler = class external 'org.apache.http.impl.cookie' name 'BasicCommentHandler' (OAHICAbstractCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
  end;

  OAHICBasicExpiresHandler = class external 'org.apache.http.impl.cookie' name 'BasicExpiresHandler' (OAHICAbstractCookieAttributeHandler)
  public
    constructor create(para1: Arr1JLString); overload;
    constructor create(const para1: array of JLString); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
  end;

  OAHICBasicMaxAgeHandler = class external 'org.apache.http.impl.cookie' name 'BasicMaxAgeHandler' (OAHICAbstractCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
  end;

  OAHICBasicSecureHandler = class external 'org.apache.http.impl.cookie' name 'BasicSecureHandler' (OAHICAbstractCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICRFC2109VersionHandler = class external 'org.apache.http.impl.cookie' name 'RFC2109VersionHandler' (OAHICAbstractCookieAttributeHandler)
  public
    constructor create(); overload;
    procedure parse(para1: OAHCSetCookie; para2: JLString); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
  end;

  OAHICNetscapeDomainHandler = class external 'org.apache.http.impl.cookie' name 'NetscapeDomainHandler' (OAHICBasicDomainHandler)
  public
    constructor create(); overload;
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICCookieSpecBase = class abstract external 'org.apache.http.impl.cookie' name 'CookieSpecBase' (OAHICAbstractCookieSpec)
  public
    constructor create(); overload;
  strict protected
    class function getDefaultPath(para1: OAHCCookieOrigin): JLString; static; overload;
    class function getDefaultDomain(para1: OAHCCookieOrigin): JLString; static; overload;
    function parse(para1: Arr1OAHHeaderElement; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function parse(var para1: array of OAHHeaderElement; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
  public
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  end;

  OAHICTConnPoolByRoute = class external 'org.apache.http.impl.conn.tsccm' name 'ConnPoolByRoute' (OAHICTAbstractConnPool)
  strict protected
    final var
      foperator: OAHCClientConnectionOperator; external name 'operator';
  strict protected
    var
      ffreeConnections: JUQueue; external name 'freeConnections';
      fwaitingThreads: JUQueue; external name 'waitingThreads';
  strict protected
    final var
      frouteToPool: JUMap; external name 'routeToPool';
      fmaxTotalConnections: jint; external name 'maxTotalConnections';
  public
    constructor create(para1: OAHCClientConnectionOperator; para2: OAHPHttpParams); overload;
  strict protected
    function createFreeConnQueue(): JUQueue; overload; virtual;
    function createWaitingThreadQueue(): JUQueue; overload; virtual;
    function createRouteToPoolMap(): JUMap; overload; virtual;
    function newRouteSpecificPool(para1: OAHCRHttpRoute): OAHICTRouteSpecificPool; overload; virtual;
    function newWaitingThread(para1: JUCLCondition; para2: OAHICTRouteSpecificPool): OAHICTWaitingThread; overload; virtual;
    function getRoutePool(para1: OAHCRHttpRoute; para2: jboolean): OAHICTRouteSpecificPool; overload; virtual;
  public
    function getConnectionsInPool(para1: OAHCRHttpRoute): jint; overload; virtual;
    function requestPoolEntry(para1: OAHCRHttpRoute; para2: JLObject): OAHICTPoolEntryRequest; overload; virtual;
  strict protected
    function getEntryBlocking(para1: OAHCRHttpRoute; para2: JLObject; para3: jlong; para4: JUCTimeUnit; para5: OAHICTWaitingThreadAborter): OAHICTBasicPoolEntry; overload; virtual;  // throws org.apache.http.conn.ConnectionPoolTimeoutException, java.lang.InterruptedException
  public
    procedure freeEntry(para1: OAHICTBasicPoolEntry; para2: jboolean; para3: jlong; para4: JUCTimeUnit); overload; virtual;
  strict protected
    function getFreeEntry(para1: OAHICTRouteSpecificPool; para2: JLObject): OAHICTBasicPoolEntry; overload; virtual;
    function createEntry(para1: OAHICTRouteSpecificPool; para2: OAHCClientConnectionOperator): OAHICTBasicPoolEntry; overload; virtual;
    procedure deleteEntry(para1: OAHICTBasicPoolEntry); overload; virtual;
    procedure deleteLeastUsedEntry(); overload; virtual;
    procedure handleLostEntry(para1: OAHCRHttpRoute); overload; virtual;
    procedure notifyWaitingThread(para1: OAHICTRouteSpecificPool); overload; virtual;
  public
    procedure deleteClosedConnections(); overload; virtual;
    procedure shutdown(); overload; virtual;
  end;

  OAHICDefaultResponseParser = class external 'org.apache.http.impl.conn' name 'DefaultResponseParser' (OAHIIAbstractMessageParser)
  public
    constructor create(para1: OAHISessionInputBuffer; para2: OAHMLineParser; para3: OAHHttpResponseFactory; para4: OAHPHttpParams); overload;
  strict protected
    function parseHead(para1: OAHISessionInputBuffer): OAHHttpMessage; overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException
  end;

  OAHIIHttpRequestParser = class external 'org.apache.http.impl.io' name 'HttpRequestParser' (OAHIIAbstractMessageParser)
  public
    constructor create(para1: OAHISessionInputBuffer; para2: OAHMLineParser; para3: OAHHttpRequestFactory; para4: OAHPHttpParams); overload;
  strict protected
    function parseHead(para1: OAHISessionInputBuffer): OAHHttpMessage; overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException, org.apache.http.ParseException
  end;

  OAHIIHttpResponseParser = class external 'org.apache.http.impl.io' name 'HttpResponseParser' (OAHIIAbstractMessageParser)
  public
    constructor create(para1: OAHISessionInputBuffer; para2: OAHMLineParser; para3: OAHHttpResponseFactory; para4: OAHPHttpParams); overload;
  strict protected
    function parseHead(para1: OAHISessionInputBuffer): OAHHttpMessage; overload; virtual;  // throws java.io.IOException, org.apache.http.HttpException, org.apache.http.ParseException
  end;

  OAHIIHttpRequestWriter = class external 'org.apache.http.impl.io' name 'HttpRequestWriter' (OAHIIAbstractMessageWriter)
  public
    constructor create(para1: OAHISessionOutputBuffer; para2: OAHMLineFormatter; para3: OAHPHttpParams); overload;
  strict protected
    procedure writeHeadLine(para1: OAHHttpMessage); overload; virtual;  // throws java.io.IOException
  end;

  OAHIIHttpResponseWriter = class external 'org.apache.http.impl.io' name 'HttpResponseWriter' (OAHIIAbstractMessageWriter)
  public
    constructor create(para1: OAHISessionOutputBuffer; para2: OAHMLineFormatter; para3: OAHPHttpParams); overload;
  strict protected
    procedure writeHeadLine(para1: OAHHttpMessage); overload; virtual;  // throws java.io.IOException
  end;

  OAHIISocketInputBuffer = class external 'org.apache.http.impl.io' name 'SocketInputBuffer' (OAHIIAbstractSessionInputBuffer)
  public
    constructor create(para1: JNSocket; para2: jint; para3: OAHPHttpParams); overload;  // throws java.io.IOException
    function isDataAvailable(para1: jint): jboolean; overload; virtual;  // throws java.io.IOException
  end;

  OAHIISocketOutputBuffer = class external 'org.apache.http.impl.io' name 'SocketOutputBuffer' (OAHIIAbstractSessionOutputBuffer)
  public
    constructor create(para1: JNSocket; para2: jint; para3: OAHPHttpParams); overload;  // throws java.io.IOException
  end;

  OAHICClientParamsStack = class external 'org.apache.http.impl.client' name 'ClientParamsStack' (OAHPAbstractHttpParams)
  strict protected
    final var
      fapplicationParams: OAHPHttpParams; external name 'applicationParams';
      fclientParams: OAHPHttpParams; external name 'clientParams';
      frequestParams: OAHPHttpParams; external name 'requestParams';
      foverrideParams: OAHPHttpParams; external name 'overrideParams';
  public
    constructor create(para1: OAHPHttpParams; para2: OAHPHttpParams; para3: OAHPHttpParams; para4: OAHPHttpParams); overload;
    constructor create(para1: OAHICClientParamsStack); overload;
    constructor create(para1: OAHICClientParamsStack; para2: OAHPHttpParams; para3: OAHPHttpParams; para4: OAHPHttpParams; para5: OAHPHttpParams); overload;
    function getApplicationParams(): OAHPHttpParams; overload; virtual; final;
    function getClientParams(): OAHPHttpParams; overload; virtual; final;
    function getRequestParams(): OAHPHttpParams; overload; virtual; final;
    function getOverrideParams(): OAHPHttpParams; overload; virtual; final;
    function getParameter(para1: JLString): JLObject; overload; virtual;
    function setParameter(para1: JLString; para2: JLObject): OAHPHttpParams; overload; virtual;  // throws java.lang.UnsupportedOperationException
    function removeParameter(para1: JLString): jboolean; overload; virtual;
    function copy(): OAHPHttpParams; overload; virtual;
  end;

  OAHPBasicHttpParams = class sealed external 'org.apache.http.params' name 'BasicHttpParams' (OAHPAbstractHttpParams, JISerializable, JLCloneable)
  public
    constructor create(); overload;
    function getParameter(para1: JLString): JLObject; overload; virtual;
    function setParameter(para1: JLString; para2: JLObject): OAHPHttpParams; overload; virtual;
    function removeParameter(para1: JLString): jboolean; overload; virtual;
    procedure setParameters(para1: Arr1JLString; para2: JLObject); overload; virtual;
    procedure setParameters(var para1: array of JLString; para2: JLObject); overload; virtual;
    function isParameterSet(para1: JLString): jboolean; overload; virtual;
    function isParameterSetLocally(para1: JLString): jboolean; overload; virtual;
    procedure clear(); overload; virtual;
    function copy(): OAHPHttpParams; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  strict protected
    procedure copyParams(para1: OAHPHttpParams); overload; virtual;
  end;

  OAHPDefaultedHttpParams = class sealed external 'org.apache.http.params' name 'DefaultedHttpParams' (OAHPAbstractHttpParams)
  public
    constructor create(para1: OAHPHttpParams; para2: OAHPHttpParams); overload;
    function copy(): OAHPHttpParams; overload; virtual;
    function getParameter(para1: JLString): JLObject; overload; virtual;
    function removeParameter(para1: JLString): jboolean; overload; virtual;
    function setParameter(para1: JLString; para2: JLObject): OAHPHttpParams; overload; virtual;
    function getDefaults(): OAHPHttpParams; overload; virtual;
  end;

  OAHPSyncBasicHttpContext = class external 'org.apache.http.protocol' name 'SyncBasicHttpContext' (OAHPBasicHttpContext)
  public
    constructor create(para1: OAHPHttpContext); overload;
    function getAttribute(para1: JLString): JLObject; overload; virtual;
    procedure setAttribute(para1: JLString; para2: JLObject); overload; virtual;
    function removeAttribute(para1: JLString): JLObject; overload; virtual;
  end;

  OWDComment = interface external 'org.w3c.dom' name 'Comment' (OWDCharacterData)
  end;

  OWDText = interface external 'org.w3c.dom' name 'Text' (OWDCharacterData)
    function splitText(para1: jint): OWDText; overload;  // throws org.w3c.dom.DOMException
    function isElementContentWhitespace(): jboolean; overload;
    function getWholeText(): JLString; overload;
    function replaceWholeText(para1: JLString): OWDText; overload;  // throws org.w3c.dom.DOMException
  end;

  OXSEAttributes2Impl = class external 'org.xml.sax.ext' name 'Attributes2Impl' (OXSHAttributesImpl, OXSEAttributes2)
  public
    constructor create(); overload;
    constructor create(para1: OXSAttributes); overload;
    function isDeclared(para1: jint): jboolean; overload; virtual;
    function isDeclared(para1: JLString; para2: JLString): jboolean; overload; virtual;
    function isDeclared(para1: JLString): jboolean; overload; virtual;
    function isSpecified(para1: jint): jboolean; overload; virtual;
    function isSpecified(para1: JLString; para2: JLString): jboolean; overload; virtual;
    function isSpecified(para1: JLString): jboolean; overload; virtual;
    procedure setAttributes(para1: OXSAttributes); overload; virtual;
    procedure addAttribute(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString); overload; virtual;
    procedure removeAttribute(para1: jint); overload; virtual;
    procedure setDeclared(para1: jint; para2: jboolean); overload; virtual;
    procedure setSpecified(para1: jint; para2: jboolean); overload; virtual;
  end;

  OXSEDefaultHandler2 = class external 'org.xml.sax.ext' name 'DefaultHandler2' (OXSHDefaultHandler, OXSELexicalHandler, OXSEDeclHandler, OXSEEntityResolver2)
  public
    constructor create(); overload;
    procedure startCDATA(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endCDATA(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startDTD(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endDTD(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startEntity(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endEntity(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure comment(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure comment(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure attributeDecl(para1: JLString; para2: JLString; para3: JLString; para4: JLString; para5: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure elementDecl(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure externalEntityDecl(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure internalEntityDecl(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    function getExternalSubset(para1: JLString; para2: JLString): OXSInputSource; overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    function resolveEntity(para1: JLString; para2: JLString; para3: JLString; para4: JLString): OXSInputSource; overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    function resolveEntity(para1: JLString; para2: JLString): OXSInputSource; overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
  end;

  OXSELocator2Impl = class external 'org.xml.sax.ext' name 'Locator2Impl' (OXSHLocatorImpl, OXSELocator2)
  public
    constructor create(); overload;
    constructor create(para1: OXSLocator); overload;
    function getXMLVersion(): JLString; overload; virtual;
    function getEncoding(): JLString; overload; virtual;
    procedure setXMLVersion(para1: JLString); overload; virtual;
    procedure setEncoding(para1: JLString); overload; virtual;
  end;

  OXSHXMLFilterImpl = class external 'org.xml.sax.helpers' name 'XMLFilterImpl' (JLObject, OXSXMLFilter, OXSEntityResolver, OXSDTDHandler, OXSContentHandler, OXSErrorHandler)
  public
    constructor create(); overload;
    constructor create(para1: OXSXMLReader); overload;
    procedure setParent(para1: OXSXMLReader); overload; virtual;
    function getParent(): OXSXMLReader; overload; virtual;
    procedure setFeature(para1: JLString; para2: jboolean); overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    function getFeature(para1: JLString): jboolean; overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setProperty(para1: JLString; para2: JLObject); overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    function getProperty(para1: JLString): JLObject; overload; virtual;  // throws org.xml.sax.SAXNotRecognizedException, org.xml.sax.SAXNotSupportedException
    procedure setEntityResolver(para1: OXSEntityResolver); overload; virtual;
    function getEntityResolver(): OXSEntityResolver; overload; virtual;
    procedure setDTDHandler(para1: OXSDTDHandler); overload; virtual;
    function getDTDHandler(): OXSDTDHandler; overload; virtual;
    procedure setContentHandler(para1: OXSContentHandler); overload; virtual;
    function getContentHandler(): OXSContentHandler; overload; virtual;
    procedure setErrorHandler(para1: OXSErrorHandler); overload; virtual;
    function getErrorHandler(): OXSErrorHandler; overload; virtual;
    procedure parse(para1: OXSInputSource); overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    procedure parse(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    function resolveEntity(para1: JLString; para2: JLString): OXSInputSource; overload; virtual;  // throws org.xml.sax.SAXException, java.io.IOException
    procedure notationDecl(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure unparsedEntityDecl(para1: JLString; para2: JLString; para3: JLString; para4: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure setDocumentLocator(para1: OXSLocator); overload; virtual;
    procedure startDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endDocument(); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startPrefixMapping(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endPrefixMapping(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure startElement(para1: JLString; para2: JLString; para3: JLString; para4: OXSAttributes); overload; virtual;  // throws org.xml.sax.SAXException
    procedure endElement(para1: JLString; para2: JLString; para3: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure characters(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(para1: Arr1jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure ignorableWhitespace(var para1: array of jchar; para2: jint; para3: jint); overload; virtual;  // throws org.xml.sax.SAXException
    procedure processingInstruction(para1: JLString; para2: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure skippedEntity(para1: JLString); overload; virtual;  // throws org.xml.sax.SAXException
    procedure warning(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
    procedure error(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
    procedure fatalError(para1: OXSSAXParseException); overload; virtual;  // throws org.xml.sax.SAXException
  end;

  OAHUnsupportedHttpVersionException = class external 'org.apache.http' name 'UnsupportedHttpVersionException' (OAHProtocolException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OAHAAuthenticationException = class external 'org.apache.http.auth' name 'AuthenticationException' (OAHProtocolException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHAMalformedChallengeException = class external 'org.apache.http.auth' name 'MalformedChallengeException' (OAHProtocolException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHCNonRepeatableRequestException = class external 'org.apache.http.client' name 'NonRepeatableRequestException' (OAHProtocolException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
  end;

  OAHCRedirectException = class external 'org.apache.http.client' name 'RedirectException' (OAHProtocolException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHCMalformedCookieException = class external 'org.apache.http.cookie' name 'MalformedCookieException' (OAHProtocolException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHCSAllowAllHostnameVerifier = class external 'org.apache.http.conn.ssl' name 'AllowAllHostnameVerifier' (OAHCSAbstractVerifier)
  public
    constructor create(); overload;
    procedure verify(para1: JLString; para2: Arr1JLString; para3: Arr1JLString); overload; virtual; final;
    procedure verify(para1: JLString; var para2: array of JLString; var para3: array of JLString); overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
  end;

  OAHCSBrowserCompatHostnameVerifier = class external 'org.apache.http.conn.ssl' name 'BrowserCompatHostnameVerifier' (OAHCSAbstractVerifier)
  public
    constructor create(); overload;
    procedure verify(para1: JLString; para2: Arr1JLString; para3: Arr1JLString); overload; virtual; final;  // throws javax.net.ssl.SSLException
    procedure verify(para1: JLString; var para2: array of JLString; var para3: array of JLString); overload; virtual; final;  // throws javax.net.ssl.SSLException
    function toString(): JLString; overload; virtual; final;
  end;

  OAHCSStrictHostnameVerifier = class external 'org.apache.http.conn.ssl' name 'StrictHostnameVerifier' (OAHCSAbstractVerifier)
  public
    constructor create(); overload;
    procedure verify(para1: JLString; para2: Arr1JLString; para3: Arr1JLString); overload; virtual; final;  // throws javax.net.ssl.SSLException
    procedure verify(para1: JLString; var para2: array of JLString; var para3: array of JLString); overload; virtual; final;  // throws javax.net.ssl.SSLException
    function toString(): JLString; overload; virtual; final;
  end;

  OAHISocketHttpClientConnection = class external 'org.apache.http.impl' name 'SocketHttpClientConnection' (OAHIAbstractHttpClientConnection, OAHHttpInetConnection)
  public
    constructor create(); overload;
  strict protected
    procedure assertNotOpen(); overload; virtual;
    procedure assertOpen(); overload; virtual;
    function createSessionInputBuffer(para1: JNSocket; para2: jint; para3: OAHPHttpParams): OAHISessionInputBuffer; overload; virtual;  // throws java.io.IOException
    function createSessionOutputBuffer(para1: JNSocket; para2: jint; para3: OAHPHttpParams): OAHISessionOutputBuffer; overload; virtual;  // throws java.io.IOException
    procedure bind(para1: JNSocket; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
  public
    function isOpen(): jboolean; overload; virtual;
  strict protected
    function getSocket(): JNSocket; overload; virtual;
  public
    function getLocalAddress(): JNInetAddress; overload; virtual;
    function getLocalPort(): jint; overload; virtual;
    function getRemoteAddress(): JNInetAddress; overload; virtual;
    function getRemotePort(): jint; overload; virtual;
    procedure setSocketTimeout(para1: jint); overload; virtual;
    function getSocketTimeout(): jint; overload; virtual;
    procedure shutdown(); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  end;

  OAHICAbstractClientConnAdapter = class abstract external 'org.apache.http.impl.conn' name 'AbstractClientConnAdapter' (JLObject, OAHCManagedClientConnection)
  strict protected
    constructor create(para1: OAHCClientConnectionManager; para2: OAHCOperatedClientConnection); overload;
    procedure detach(); overload; virtual;
    function getWrappedConnection(): OAHCOperatedClientConnection; overload; virtual;
    function getManager(): OAHCClientConnectionManager; overload; virtual;
    procedure assertNotAborted(); overload; virtual; final;  // throws java.io.InterruptedIOException
    procedure assertValid(para1: OAHCOperatedClientConnection); overload; virtual; final;
  public
    function isOpen(): jboolean; overload; virtual;
    function isStale(): jboolean; overload; virtual;
    procedure setSocketTimeout(para1: jint); overload; virtual;
    function getSocketTimeout(): jint; overload; virtual;
    function getMetrics(): OAHHttpConnectionMetrics; overload; virtual;
    procedure flush(); overload; virtual;  // throws java.io.IOException
    function isResponseAvailable(para1: jint): jboolean; overload; virtual;  // throws java.io.IOException
    procedure receiveResponseEntity(para1: OAHHttpResponse); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function receiveResponseHeader(): OAHHttpResponse; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendRequestEntity(para1: OAHHttpEntityEnclosingRequest); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendRequestHeader(para1: OAHHttpRequest); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    function getLocalAddress(): JNInetAddress; overload; virtual;
    function getLocalPort(): jint; overload; virtual;
    function getRemoteAddress(): JNInetAddress; overload; virtual;
    function getRemotePort(): jint; overload; virtual;
    function isSecure(): jboolean; overload; virtual;
    function getSSLSession(): JNSSSLSession; overload; virtual;
    procedure markReusable(); overload; virtual;
    procedure unmarkReusable(); overload; virtual;
    function isMarkedReusable(): jboolean; overload; virtual;
    procedure setIdleDuration(para1: jlong; para2: JUCTimeUnit); overload; virtual;
    procedure releaseConnection(); overload; virtual;
    procedure abortConnection(); overload; virtual;
  end;

  OAHISocketHttpServerConnection = class external 'org.apache.http.impl' name 'SocketHttpServerConnection' (OAHIAbstractHttpServerConnection, OAHHttpInetConnection)
  public
    constructor create(); overload;
  strict protected
    procedure assertNotOpen(); overload; virtual;
    procedure assertOpen(); overload; virtual;
    function createHttpDataReceiver(para1: JNSocket; para2: jint; para3: OAHPHttpParams): OAHISessionInputBuffer; overload; virtual;  // throws java.io.IOException
    function createHttpDataTransmitter(para1: JNSocket; para2: jint; para3: OAHPHttpParams): OAHISessionOutputBuffer; overload; virtual;  // throws java.io.IOException
    procedure bind(para1: JNSocket; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    function getSocket(): JNSocket; overload; virtual;
  public
    function isOpen(): jboolean; overload; virtual;
    function getLocalAddress(): JNInetAddress; overload; virtual;
    function getLocalPort(): jint; overload; virtual;
    function getRemoteAddress(): JNInetAddress; overload; virtual;
    function getRemotePort(): jint; overload; virtual;
    procedure setSocketTimeout(para1: jint); overload; virtual;
    function getSocketTimeout(): jint; overload; virtual;
    procedure shutdown(); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  end;

  OAHCEUrlEncodedFormEntity = class external 'org.apache.http.client.entity' name 'UrlEncodedFormEntity' (OAHEStringEntity)
  public
    constructor create(para1: JUList; para2: JLString); overload;  // throws java.io.UnsupportedEncodingException
    constructor create(para1: JUList); overload;  // throws java.io.UnsupportedEncodingException
  end;

  OAHCMHttpRequestBase = class abstract external 'org.apache.http.client.methods' name 'HttpRequestBase' (OAHMAbstractHttpMessage, OAHCMHttpUriRequest, OAHCMAbortableHttpRequest, JLCloneable)
  public
    constructor create(); overload;
    function getMethod(): JLString; overload; virtual; abstract;
    function getProtocolVersion(): OAHProtocolVersion; overload; virtual;
    function getURI(): JNURI; overload; virtual;
    function getRequestLine(): OAHRequestLine; overload; virtual;
    procedure setURI(para1: JNURI); overload; virtual;
    procedure setConnectionRequest(para1: OAHCClientConnectionRequest); overload; virtual;  // throws java.io.IOException
    procedure setReleaseTrigger(para1: OAHCConnectionReleaseTrigger); overload; virtual;  // throws java.io.IOException
    procedure abort(); overload; virtual;
    function isAborted(): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHICRequestWrapper = class external 'org.apache.http.impl.client' name 'RequestWrapper' (OAHMAbstractHttpMessage, OAHCMHttpUriRequest)
  public
    constructor create(para1: OAHHttpRequest); overload;  // throws org.apache.http.ProtocolException
    procedure resetHeaders(); overload; virtual;
    function getMethod(): JLString; overload; virtual;
    procedure setMethod(para1: JLString); overload; virtual;
    function getProtocolVersion(): OAHProtocolVersion; overload; virtual;
    procedure setProtocolVersion(para1: OAHProtocolVersion); overload; virtual;
    function getURI(): JNURI; overload; virtual;
    procedure setURI(para1: JNURI); overload; virtual;
    function getRequestLine(): OAHRequestLine; overload; virtual;
    procedure abort(); overload; virtual;  // throws java.lang.UnsupportedOperationException
    function isAborted(): jboolean; overload; virtual;
    function getOriginal(): OAHHttpRequest; overload; virtual;
    function isRepeatable(): jboolean; overload; virtual;
    function getExecCount(): jint; overload; virtual;
    procedure incrementExecCount(); overload; virtual;
  end;

  OAHMBasicHttpEntityEnclosingRequest = class external 'org.apache.http.message' name 'BasicHttpEntityEnclosingRequest' (OAHMBasicHttpRequest, OAHHttpEntityEnclosingRequest)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    constructor create(para1: JLString; para2: JLString; para3: OAHProtocolVersion); overload;
    constructor create(para1: OAHRequestLine); overload;
    function getEntity(): OAHHttpEntity; overload; virtual;
    procedure setEntity(para1: OAHHttpEntity); overload; virtual;
    function expectContinue(): jboolean; overload; virtual;
  end;

  OAHIABasicScheme = class external 'org.apache.http.impl.auth' name 'BasicScheme' (OAHIARFC2617Scheme)
  public
    constructor create(); overload;
    function getSchemeName(): JLString; overload; virtual;
    procedure processChallenge(para1: OAHHeader); overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
    function isComplete(): jboolean; overload; virtual;
    function isConnectionBased(): jboolean; overload; virtual;
    function authenticate(para1: OAHACredentials; para2: OAHHttpRequest): OAHHeader; overload; virtual;  // throws org.apache.http.auth.AuthenticationException
    class function authenticate(para1: OAHACredentials; para2: JLString; para3: jboolean): OAHHeader; static; overload;
  end;

  OAHIADigestScheme = class external 'org.apache.http.impl.auth' name 'DigestScheme' (OAHIARFC2617Scheme)
  public
    constructor create(); overload;
    procedure processChallenge(para1: OAHHeader); overload; virtual;  // throws org.apache.http.auth.MalformedChallengeException
    function isComplete(): jboolean; overload; virtual;
    function getSchemeName(): JLString; overload; virtual;
    function isConnectionBased(): jboolean; overload; virtual;
    procedure overrideParamter(para1: JLString; para2: JLString); overload; virtual;
    function authenticate(para1: OAHACredentials; para2: OAHHttpRequest): OAHHeader; overload; virtual;  // throws org.apache.http.auth.AuthenticationException
    class function createCnonce(): JLString; static; overload;
  end;

  OAHICBasicClientCookie2 = class external 'org.apache.http.impl.cookie' name 'BasicClientCookie2' (OAHICBasicClientCookie, OAHCSetCookie2)
  public
    constructor create(para1: JLString; para2: JLString); overload;
    function getPorts(): Arr1jint; overload; virtual;
    procedure setPorts(para1: Arr1jint); overload; virtual;
    procedure setPorts(var para1: array of jint); overload; virtual;
    function getCommentURL(): JLString; overload; virtual;
    procedure setCommentURL(para1: JLString); overload; virtual;
    procedure setDiscard(para1: jboolean); overload; virtual;
    function isPersistent(): jboolean; overload; virtual;
    function isExpired(para1: JUDate): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHICBrowserCompatSpec = class external 'org.apache.http.impl.cookie' name 'BrowserCompatSpec' (OAHICCookieSpecBase)
  strict protected
    final class var
      fDATE_PATTERNS: Arr1JLString; external name 'DATE_PATTERNS';
  public
    constructor create(para1: Arr1JLString); overload;
    constructor create(const para1: array of JLString); overload;
    constructor create(); overload;
    function parse(para1: OAHHeader; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function formatCookies(para1: JUList): JUList; overload; virtual;
    function getVersion(): jint; overload; virtual;
    function getVersionHeader(): OAHHeader; overload; virtual;
  end;

  OAHICNetscapeDraftSpec = class external 'org.apache.http.impl.cookie' name 'NetscapeDraftSpec' (OAHICCookieSpecBase)
  strict protected
    const
      EXPIRES_PATTERN = 'EEE, dd-MMM-yyyy HH:mm:ss z';
  public
    constructor create(para1: Arr1JLString); overload;
    constructor create(const para1: array of JLString); overload;
    constructor create(); overload;
    function parse(para1: OAHHeader; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function formatCookies(para1: JUList): JUList; overload; virtual;
    function getVersion(): jint; overload; virtual;
    function getVersionHeader(): OAHHeader; overload; virtual;
  end;

  OAHICRFC2109Spec = class external 'org.apache.http.impl.cookie' name 'RFC2109Spec' (OAHICCookieSpecBase)
  public
    constructor create(para1: Arr1JLString; para2: jboolean); overload;
    constructor create(const para1: array of JLString; para2: jboolean); overload;
    constructor create(); overload;
    function parse(para1: OAHHeader; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function formatCookies(para1: JUList): JUList; overload; virtual;
  strict protected
    procedure formatParamAsVer(para1: OAHUCharArrayBuffer; para2: JLString; para3: JLString; para4: jint); overload; virtual;
    procedure formatCookieAsVer(para1: OAHUCharArrayBuffer; para2: OAHCCookie; para3: jint); overload; virtual;
  public
    function getVersion(): jint; overload; virtual;
    function getVersionHeader(): OAHHeader; overload; virtual;
  end;

  OWDCDATASection = interface external 'org.w3c.dom' name 'CDATASection' (OWDText)
  end;

  OAHAInvalidCredentialsException = class external 'org.apache.http.auth' name 'InvalidCredentialsException' (OAHAAuthenticationException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHIANTLMEngineException = class external 'org.apache.http.impl.auth' name 'NTLMEngineException' (OAHAAuthenticationException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHCCircularRedirectException = class external 'org.apache.http.client' name 'CircularRedirectException' (OAHCRedirectException)
  public
    constructor create(); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
  end;

  OAHIDefaultHttpClientConnection = class external 'org.apache.http.impl' name 'DefaultHttpClientConnection' (OAHISocketHttpClientConnection)
  public
    constructor create(); overload;
    procedure bind(para1: JNSocket; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    function toString(): JLString; overload; virtual;
  end;

  OAHICDefaultClientConnection = class external 'org.apache.http.impl.conn' name 'DefaultClientConnection' (OAHISocketHttpClientConnection, OAHCOperatedClientConnection)
  public
    constructor create(); overload;
    function getTargetHost(): OAHHttpHost; overload; virtual; final;
    function isSecure(): jboolean; overload; virtual; final;
    function getSocket(): JNSocket; overload; virtual; final;
    procedure opening(para1: JNSocket; para2: OAHHttpHost); overload; virtual;  // throws java.io.IOException
    procedure openCompleted(para1: jboolean; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure shutdown(); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  strict protected
    function createSessionInputBuffer(para1: JNSocket; para2: jint; para3: OAHPHttpParams): OAHISessionInputBuffer; overload; virtual;  // throws java.io.IOException
    function createSessionOutputBuffer(para1: JNSocket; para2: jint; para3: OAHPHttpParams): OAHISessionOutputBuffer; overload; virtual;  // throws java.io.IOException
    function createResponseParser(para1: OAHISessionInputBuffer; para2: OAHHttpResponseFactory; para3: OAHPHttpParams): OAHIHttpMessageParser; overload; virtual;
  public
    procedure update(para1: JNSocket; para2: OAHHttpHost; para3: jboolean; para4: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    function receiveResponseHeader(): OAHHttpResponse; overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
    procedure sendRequestHeader(para1: OAHHttpRequest); overload; virtual;  // throws org.apache.http.HttpException, java.io.IOException
  end;

  OAHICAbstractPooledConnAdapter = class abstract external 'org.apache.http.impl.conn' name 'AbstractPooledConnAdapter' (OAHICAbstractClientConnAdapter)
  strict protected
    var
      fpoolEntry: OAHICAbstractPoolEntry; external name 'poolEntry';
  strict protected
    constructor create(para1: OAHCClientConnectionManager; para2: OAHICAbstractPoolEntry); overload;
    procedure assertAttached(); overload; virtual; final;
    procedure detach(); overload; virtual;
  public
    function getRoute(): OAHCRHttpRoute; overload; virtual;
    procedure open(para1: OAHCRHttpRoute; para2: OAHPHttpContext; para3: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure tunnelTarget(para1: jboolean; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure tunnelProxy(para1: OAHHttpHost; para2: jboolean; para3: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure layerProtocol(para1: OAHPHttpContext; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
    procedure shutdown(); overload; virtual;  // throws java.io.IOException
    function getState(): JLObject; overload; virtual;
    procedure setState(para1: JLObject); overload; virtual;
  end;

  OAHIDefaultHttpServerConnection = class external 'org.apache.http.impl' name 'DefaultHttpServerConnection' (OAHISocketHttpServerConnection)
  public
    constructor create(); overload;
    procedure bind(para1: JNSocket; para2: OAHPHttpParams); overload; virtual;  // throws java.io.IOException
    function toString(): JLString; overload; virtual;
  end;

  OAHCMHttpDelete = class external 'org.apache.http.client.methods' name 'HttpDelete' (OAHCMHttpRequestBase)
  public
    const
      METHOD_NAME = 'DELETE';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
  end;

  OAHCMHttpEntityEnclosingRequestBase = class abstract external 'org.apache.http.client.methods' name 'HttpEntityEnclosingRequestBase' (OAHCMHttpRequestBase, OAHHttpEntityEnclosingRequest)
  public
    constructor create(); overload;
    function getEntity(): OAHHttpEntity; overload; virtual;
    procedure setEntity(para1: OAHHttpEntity); overload; virtual;
    function expectContinue(): jboolean; overload; virtual;
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  OAHCMHttpGet = class external 'org.apache.http.client.methods' name 'HttpGet' (OAHCMHttpRequestBase)
  public
    const
      METHOD_NAME = 'GET';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
  end;

  OAHCMHttpHead = class external 'org.apache.http.client.methods' name 'HttpHead' (OAHCMHttpRequestBase)
  public
    const
      METHOD_NAME = 'HEAD';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
  end;

  OAHCMHttpOptions = class external 'org.apache.http.client.methods' name 'HttpOptions' (OAHCMHttpRequestBase)
  public
    const
      METHOD_NAME = 'OPTIONS';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
    function getAllowedMethods(para1: OAHHttpResponse): JUSet; overload; virtual;
  end;

  OAHCMHttpTrace = class external 'org.apache.http.client.methods' name 'HttpTrace' (OAHCMHttpRequestBase)
  public
    const
      METHOD_NAME = 'TRACE';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
  end;

  OAHICEntityEnclosingRequestWrapper = class external 'org.apache.http.impl.client' name 'EntityEnclosingRequestWrapper' (OAHICRequestWrapper, OAHHttpEntityEnclosingRequest)
  public
    constructor create(para1: OAHHttpEntityEnclosingRequest); overload;  // throws org.apache.http.ProtocolException
    function getEntity(): OAHHttpEntity; overload; virtual;
    procedure setEntity(para1: OAHHttpEntity); overload; virtual;
    function expectContinue(): jboolean; overload; virtual;
    function isRepeatable(): jboolean; overload; virtual;
  end;

  OAHICRFC2965Spec = class external 'org.apache.http.impl.cookie' name 'RFC2965Spec' (OAHICRFC2109Spec)
  public
    constructor create(); overload;
    constructor create(para1: Arr1JLString; para2: jboolean); overload;
    constructor create(const para1: array of JLString; para2: jboolean); overload;
    function parse(para1: OAHHeader; para2: OAHCCookieOrigin): JUList; overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    procedure validate(para1: OAHCCookie; para2: OAHCCookieOrigin); overload; virtual;  // throws org.apache.http.cookie.MalformedCookieException
    function match(para1: OAHCCookie; para2: OAHCCookieOrigin): jboolean; overload; virtual;
  strict protected
    procedure formatCookieAsVer(para1: OAHUCharArrayBuffer; para2: OAHCCookie; para3: jint); overload; virtual;
  public
    function getVersion(): jint; overload; virtual;
    function getVersionHeader(): OAHHeader; overload; virtual;
  end;

  OAHICTBasicPooledConnAdapter = class external 'org.apache.http.impl.conn.tsccm' name 'BasicPooledConnAdapter' (OAHICAbstractPooledConnAdapter)
  strict protected
    constructor create(para1: OAHICTThreadSafeClientConnManager; para2: OAHICAbstractPoolEntry); overload;
    function getManager(): OAHCClientConnectionManager; overload; virtual;
    function getPoolEntry(): OAHICAbstractPoolEntry; overload; virtual;
    procedure detach(); overload; virtual;
  end;

  OAHCMHttpPost = class external 'org.apache.http.client.methods' name 'HttpPost' (OAHCMHttpEntityEnclosingRequestBase)
  public
    const
      METHOD_NAME = 'POST';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
  end;

  OAHCMHttpPut = class external 'org.apache.http.client.methods' name 'HttpPut' (OAHCMHttpEntityEnclosingRequestBase)
  public
    const
      METHOD_NAME = 'PUT';
  public
    constructor create(); overload;
    constructor create(para1: JNURI); overload;
    constructor create(para1: JLString); overload;
    function getMethod(): JLString; overload; virtual;
  end;

