  CAIUPredicate = interface external 'com.android.internal.util' name 'Predicate' 
    function apply(para1: JLObject): jboolean; overload;
  end;

  CLAHAsyncHttpClient = class external 'com.loopj.android.http' name 'AsyncHttpClient' (JLObject)
  public
    type
      InnerInflatingEntity = class;
      Arr1InnerInflatingEntity = array of InnerInflatingEntity;
      Arr2InnerInflatingEntity = array of Arr1InnerInflatingEntity;
      Arr3InnerInflatingEntity = array of Arr2InnerInflatingEntity;
      InnerInflatingEntity = class external 'com.loopj.android.http' name 'InflatingEntity' 
      end;

  public
    constructor create(); overload;
    function getHttpClient(): OAHCHttpClient; overload; virtual;
    function getHttpContext(): OAHPHttpContext; overload; virtual;
    procedure setCookieStore(para1: OAHCCookieStore); overload; virtual;
    procedure setThreadPool(para1: JUCThreadPoolExecutor); overload; virtual;
    procedure setUserAgent(para1: JLString); overload; virtual;
    procedure setTimeout(para1: jint); overload; virtual;
    procedure setSSLSocketFactory(para1: OAHCSSSLSocketFactory); overload; virtual;
    procedure addHeader(para1: JLString; para2: JLString); overload; virtual;
    procedure setBasicAuth(para1: JLString; para2: JLString); overload; virtual;
    procedure setBasicAuth(para1: JLString; para2: JLString; para3: OAHAAuthScope); overload; virtual;
    procedure cancelRequests(para1: ACContext; para2: jboolean); overload; virtual;
    procedure get(para1: JLString; para2: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure get(para1: JLString; para2: CLAHRequestParams; para3: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure get(para1: ACContext; para2: JLString; para3: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure get(para1: ACContext; para2: JLString; para3: CLAHRequestParams; para4: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure get(para1: ACContext; para2: JLString; para3: Arr1OAHHeader; para4: CLAHRequestParams; para5: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure get(para1: ACContext; para2: JLString; var para3: array of OAHHeader; para4: CLAHRequestParams; para5: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: JLString; para2: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: JLString; para2: CLAHRequestParams; para3: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: ACContext; para2: JLString; para3: CLAHRequestParams; para4: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: ACContext; para2: JLString; para3: OAHHttpEntity; para4: JLString; para5: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: ACContext; para2: JLString; para3: Arr1OAHHeader; para4: CLAHRequestParams; para5: JLString; para6: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: ACContext; para2: JLString; var para3: array of OAHHeader; para4: CLAHRequestParams; para5: JLString; para6: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: ACContext; para2: JLString; para3: Arr1OAHHeader; para4: OAHHttpEntity; para5: JLString; para6: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure post(para1: ACContext; para2: JLString; var para3: array of OAHHeader; para4: OAHHttpEntity; para5: JLString; para6: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure put(para1: JLString; para2: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure put(para1: JLString; para2: CLAHRequestParams; para3: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure put(para1: ACContext; para2: JLString; para3: CLAHRequestParams; para4: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure put(para1: ACContext; para2: JLString; para3: OAHHttpEntity; para4: JLString; para5: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure put(para1: ACContext; para2: JLString; para3: Arr1OAHHeader; para4: OAHHttpEntity; para5: JLString; para6: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure put(para1: ACContext; para2: JLString; var para3: array of OAHHeader; para4: OAHHttpEntity; para5: JLString; para6: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure delete(para1: JLString; para2: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure delete(para1: ACContext; para2: JLString; para3: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure delete(para1: ACContext; para2: JLString; para3: Arr1OAHHeader; para4: CLAHAsyncHttpResponseHandler); overload; virtual;
    procedure delete(para1: ACContext; para2: JLString; var para3: array of OAHHeader; para4: CLAHAsyncHttpResponseHandler); overload; virtual;
  strict protected
    procedure sendRequest(para1: OAHICDefaultHttpClient; para2: OAHPHttpContext; para3: OAHCMHttpUriRequest; para4: JLString; para5: CLAHAsyncHttpResponseHandler; para6: ACContext); overload; virtual;
  public
    class function getUrlWithQueryString(para1: JLString; para2: CLAHRequestParams): JLString; static; overload;
  end;

  CLAHAsyncHttpResponseHandler = class external 'com.loopj.android.http' name 'AsyncHttpResponseHandler' (JLObject)
  strict protected
    const
      SUCCESS_MESSAGE = 0;
      FAILURE_MESSAGE = 1;
      START_MESSAGE = 2;
      FINISH_MESSAGE = 3;
  public
    constructor create(); overload;
    procedure onStart(); overload; virtual;
    procedure onFinish(); overload; virtual;
    procedure onSuccess(para1: JLString); overload; virtual;
    procedure onSuccess(para1: jint; para2: JLString); overload; virtual;
    procedure onFailure(para1: JLThrowable); overload; virtual;
    procedure onFailure(para1: JLThrowable; para2: JLString); overload; virtual;
  strict protected
    procedure sendSuccessMessage(para1: jint; para2: JLString); overload; virtual;
    procedure sendFailureMessage(para1: JLThrowable; para2: JLString); overload; virtual;
    procedure sendFailureMessage(para1: JLThrowable; para2: Arr1jbyte); overload; virtual;
    procedure sendFailureMessage(para1: JLThrowable; var para2: array of jbyte); overload; virtual;
    procedure sendStartMessage(); overload; virtual;
    procedure sendFinishMessage(); overload; virtual;
    procedure handleSuccessMessage(para1: jint; para2: JLString); overload; virtual;
    procedure handleFailureMessage(para1: JLThrowable; para2: JLString); overload; virtual;
    procedure handleMessage(para1: AOMessage); overload; virtual;
    procedure sendMessage(para1: AOMessage); overload; virtual;
    function obtainMessage(para1: jint; para2: JLObject): AOMessage; overload; virtual;
  end;

  CLAHRequestParams = class external 'com.loopj.android.http' name 'RequestParams' (JLObject)
  public
    type
      InnerFileWrapper = class;
      Arr1InnerFileWrapper = array of InnerFileWrapper;
      Arr2InnerFileWrapper = array of Arr1InnerFileWrapper;
      Arr3InnerFileWrapper = array of Arr2InnerFileWrapper;
      InnerFileWrapper = class external 'com.loopj.android.http' name 'FileWrapper' 
      end;

  strict protected
    var
      furlParams: JUCConcurrentHashMap; external name 'urlParams';
      ffileParams: JUCConcurrentHashMap; external name 'fileParams';
      furlParamsWithArray: JUCConcurrentHashMap; external name 'urlParamsWithArray';
  public
    constructor create(); overload;
    constructor create(para1: JUMap); overload;
    constructor create(para1: JLString; para2: JLString); overload;
    constructor create(para1: Arr1JLObject); overload;
    constructor create(const para1: array of JLObject); overload;
    procedure put(para1: JLString; para2: JLString); overload; virtual;
    procedure put(para1: JLString; para2: JIFile); overload; virtual;  // throws java.io.FileNotFoundException
    procedure put(para1: JLString; para2: JUArrayList); overload; virtual;
    procedure put(para1: JLString; para2: JIInputStream); overload; virtual;
    procedure put(para1: JLString; para2: JIInputStream; para3: JLString); overload; virtual;
    procedure put(para1: JLString; para2: JIInputStream; para3: JLString; para4: JLString); overload; virtual;
    procedure remove(para1: JLString); overload; virtual;
    function toString(): JLString; overload; virtual;
    function getEntity(): OAHHttpEntity; overload; virtual;
  strict protected
    function getParamsList(): JUList; overload; virtual;
    function getParamString(): JLString; overload; virtual;
  end;

  CLAHAsyncHttpRequest = class external 'com.loopj.android.http' name 'AsyncHttpRequest' (JLObject, JLRunnable)
  public
    constructor create(para1: OAHICAbstractHttpClient; para2: OAHPHttpContext; para3: OAHCMHttpUriRequest; para4: CLAHAsyncHttpResponseHandler); overload;
    procedure run(); overload; virtual;
  end;

  CLAHPersistentCookieStore = class external 'com.loopj.android.http' name 'PersistentCookieStore' (JLObject, OAHCCookieStore)
  public
    constructor create(para1: ACContext); overload;
    procedure addCookie(para1: OAHCCookie); overload; virtual;
    procedure clear(); overload; virtual;
    function clearExpired(para1: JUDate): jboolean; overload; virtual;
    function getCookies(): JUList; overload; virtual;
  strict protected
    function encodeCookie(para1: CLAHSerializableCookie): JLString; overload; virtual;
    function decodeCookie(para1: JLString): OAHCCookie; overload; virtual;
    function byteArrayToHexString(para1: Arr1jbyte): JLString; overload; virtual;
    function byteArrayToHexString(var para1: array of jbyte): JLString; overload; virtual;
    function hexStringToByteArray(para1: JLString): Arr1jbyte; overload; virtual;
  end;

  CLAHRetryHandler = class external 'com.loopj.android.http' name 'RetryHandler' (JLObject, OAHCHttpRequestRetryHandler)
  public
    constructor create(para1: jint); overload;
    function retryRequest(para1: JIIOException; para2: jint; para3: OAHPHttpContext): jboolean; overload; virtual;
  strict protected
    function isInList(para1: JUHashSet; para2: JLThrowable): jboolean; overload; virtual;
  end;

  CLAHSerializableCookie = class external 'com.loopj.android.http' name 'SerializableCookie' (JLObject, JISerializable)
  public
    constructor create(para1: OAHCCookie); overload;
    function getCookie(): OAHCCookie; overload; virtual;
  end;

  CLAHSimpleMultipartEntity = class external 'com.loopj.android.http' name 'SimpleMultipartEntity' (JLObject, OAHHttpEntity)
  public
    constructor create(); overload;
    procedure writeFirstBoundaryIfNeeds(); overload; virtual;
    procedure writeLastBoundaryIfNeeds(); overload; virtual;
    procedure addPart(para1: JLString; para2: JLString); overload; virtual;
    procedure addPart(para1: JLString; para2: JLString; para3: JIInputStream; para4: jboolean); overload; virtual;
    procedure addPart(para1: JLString; para2: JLString; para3: JIInputStream; para4: JLString; para5: jboolean); overload; virtual;
    procedure addPart(para1: JLString; para2: JIFile; para3: jboolean); overload; virtual;
    function getContentLength(): jlong; overload; virtual;
    function getContentType(): OAHHeader; overload; virtual;
    function isChunked(): jboolean; overload; virtual;
    function isRepeatable(): jboolean; overload; virtual;
    function isStreaming(): jboolean; overload; virtual;
    procedure writeTo(para1: JIOutputStream); overload; virtual;  // throws java.io.IOException
    function getContentEncoding(): OAHHeader; overload; virtual;
    procedure consumeContent(); overload; virtual;  // throws java.io.IOException, java.lang.UnsupportedOperationException
    function getContent(): JIInputStream; overload; virtual;  // throws java.io.IOException, java.lang.UnsupportedOperationException
  end;

  CLAHSyncHttpClient = class abstract external 'com.loopj.android.http' name 'SyncHttpClient' (CLAHAsyncHttpClient)
  public
    constructor create(); overload;
    function getResponseCode(): jint; overload; virtual;
  strict protected
    procedure sendRequest(para1: OAHICDefaultHttpClient; para2: OAHPHttpContext; para3: OAHCMHttpUriRequest; para4: JLString; para5: CLAHAsyncHttpResponseHandler; para6: ACContext); overload; virtual;
  public
    function onRequestFailed(para1: JLThrowable; para2: JLString): JLString; overload; virtual; abstract;
    procedure delete(para1: JLString; para2: CLAHRequestParams; para3: CLAHAsyncHttpResponseHandler); overload; virtual;
    function get(para1: JLString; para2: CLAHRequestParams): JLString; overload; virtual;
    function get(para1: JLString): JLString; overload; virtual;
    function put(para1: JLString; para2: CLAHRequestParams): JLString; overload; virtual;
    function put(para1: JLString): JLString; overload; virtual;
    function post(para1: JLString; para2: CLAHRequestParams): JLString; overload; virtual;
    function post(para1: JLString): JLString; overload; virtual;
    function delete(para1: JLString; para2: CLAHRequestParams): JLString; overload; virtual;
    function delete(para1: JLString): JLString; overload; virtual;
  end;

  CLAHBinaryHttpResponseHandler = class external 'com.loopj.android.http' name 'BinaryHttpResponseHandler' (CLAHAsyncHttpResponseHandler)
  public
    constructor create(); overload;
    constructor create(para1: Arr1JLString); overload;
    constructor create(const para1: array of JLString); overload;
    procedure onSuccess(para1: Arr1jbyte); overload; virtual;
    procedure onSuccess(var para1: array of jbyte); overload; virtual;
    procedure onSuccess(para1: jint; para2: Arr1jbyte); overload; virtual;
    procedure onSuccess(para1: jint; var para2: array of jbyte); overload; virtual;
    procedure onFailure(para1: JLThrowable; para2: Arr1jbyte); overload; virtual;
    procedure onFailure(para1: JLThrowable; var para2: array of jbyte); overload; virtual;
  strict protected
    procedure sendSuccessMessage(para1: jint; para2: Arr1jbyte); overload; virtual;
    procedure sendSuccessMessage(para1: jint; var para2: array of jbyte); overload; virtual;
    procedure sendFailureMessage(para1: JLThrowable; para2: Arr1jbyte); overload; virtual;
    procedure sendFailureMessage(para1: JLThrowable; var para2: array of jbyte); overload; virtual;
    procedure handleSuccessMessage(para1: jint; para2: Arr1jbyte); overload; virtual;
    procedure handleSuccessMessage(para1: jint; var para2: array of jbyte); overload; virtual;
    procedure handleFailureMessage(para1: JLThrowable; para2: Arr1jbyte); overload; virtual;
    procedure handleFailureMessage(para1: JLThrowable; var para2: array of jbyte); overload; virtual;
    procedure handleMessage(para1: AOMessage); overload; virtual;
  end;

  CLAHJsonHttpResponseHandler = class external 'com.loopj.android.http' name 'JsonHttpResponseHandler' (CLAHAsyncHttpResponseHandler)
  strict protected
    const
      SUCCESS_JSON_MESSAGE = 100;
  public
    constructor create(); overload;
    procedure onSuccess(para1: OJJSONObject); overload; virtual;
    procedure onSuccess(para1: OJJSONArray); overload; virtual;
    procedure onSuccess(para1: jint; para2: OJJSONObject); overload; virtual;
    procedure onSuccess(para1: jint; para2: OJJSONArray); overload; virtual;
    procedure onFailure(para1: JLThrowable; para2: OJJSONObject); overload; virtual;
    procedure onFailure(para1: JLThrowable; para2: OJJSONArray); overload; virtual;
  strict protected
    procedure sendSuccessMessage(para1: jint; para2: JLString); overload; virtual;
    procedure handleMessage(para1: AOMessage); overload; virtual;
    procedure handleSuccessJsonMessage(para1: jint; para2: JLObject); overload; virtual;
    function parseResponse(para1: JLString): JLObject; overload; virtual;  // throws org.json.JSONException
    procedure handleFailureMessage(para1: JLThrowable; para2: JLString); overload; virtual;
  end;

