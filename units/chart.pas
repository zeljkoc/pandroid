{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
{ Imports for Java packages/classes: org. }
unit chart;
{$mode delphi}
{$namespace zeljus.com.units} 

interface
//uses androidr15
{$include AndroidVersion.inc};

type
  OAHICAbstractPooledConnAdapter = class;
  Arr1OAHICAbstractPooledConnAdapter = array of OAHICAbstractPooledConnAdapter;
  Arr2OAHICAbstractPooledConnAdapter = array of Arr1OAHICAbstractPooledConnAdapter;
  Arr3OAHICAbstractPooledConnAdapter = array of Arr2OAHICAbstractPooledConnAdapter;

  OAHIDefaultHttpResponseFactory = class;
  Arr1OAHIDefaultHttpResponseFactory = array of OAHIDefaultHttpResponseFactory;
  Arr2OAHIDefaultHttpResponseFactory = array of Arr1OAHIDefaultHttpResponseFactory;
  Arr3OAHIDefaultHttpResponseFactory = array of Arr2OAHIDefaultHttpResponseFactory;

  OAHICDefaultRequestDirector = class;
  Arr1OAHICDefaultRequestDirector = array of OAHICDefaultRequestDirector;
  Arr2OAHICDefaultRequestDirector = array of Arr1OAHICDefaultRequestDirector;
  Arr3OAHICDefaultRequestDirector = array of Arr2OAHICDefaultRequestDirector;

  OAHIANTLMScheme = class;
  Arr1OAHIANTLMScheme = array of OAHIANTLMScheme;
  Arr2OAHIANTLMScheme = array of Arr1OAHIANTLMScheme;
  Arr3OAHIANTLMScheme = array of Arr2OAHIANTLMScheme;

  OAHAUsernamePasswordCredentials = class;
  Arr1OAHAUsernamePasswordCredentials = array of OAHAUsernamePasswordCredentials;
  Arr2OAHAUsernamePasswordCredentials = array of Arr1OAHAUsernamePasswordCredentials;
  Arr3OAHAUsernamePasswordCredentials = array of Arr2OAHAUsernamePasswordCredentials;

  OAHCEUrlEncodedFormEntity = class;
  Arr1OAHCEUrlEncodedFormEntity = array of OAHCEUrlEncodedFormEntity;
  Arr2OAHCEUrlEncodedFormEntity = array of Arr1OAHCEUrlEncodedFormEntity;
  Arr3OAHCEUrlEncodedFormEntity = array of Arr2OAHCEUrlEncodedFormEntity;

  OAHPAbstractHttpParams = class;
  Arr1OAHPAbstractHttpParams = array of OAHPAbstractHttpParams;
  Arr2OAHPAbstractHttpParams = array of Arr1OAHPAbstractHttpParams;
  Arr3OAHPAbstractHttpParams = array of Arr2OAHPAbstractHttpParams;

  OAMTimeSeries = class;
  Arr1OAMTimeSeries = array of OAMTimeSeries;
  Arr2OAMTimeSeries = array of Arr1OAMTimeSeries;
  Arr3OAMTimeSeries = array of Arr2OAMTimeSeries;

  OAHHttpHost = class;
  Arr1OAHHttpHost = array of OAHHttpHost;
  Arr2OAHHttpHost = array of Arr1OAHHttpHost;
  Arr3OAHHttpHost = array of Arr2OAHHttpHost;

  OACBubbleChart = class;
  Arr1OACBubbleChart = array of OACBubbleChart;
  Arr2OACBubbleChart = array of Arr1OACBubbleChart;
  Arr3OACBubbleChart = array of Arr2OACBubbleChart;

  OAHCSSSLSocketFactory = class;
  Arr1OAHCSSSLSocketFactory = array of OAHCSSSLSocketFactory;
  Arr2OAHCSSSLSocketFactory = array of Arr1OAHCSSSLSocketFactory;
  Arr3OAHCSSSLSocketFactory = array of Arr2OAHCSSSLSocketFactory;

  OAHCRHttpRoute = class;
  Arr1OAHCRHttpRoute = array of OAHCRHttpRoute;
  Arr2OAHCRHttpRoute = array of Arr1OAHCRHttpRoute;
  Arr3OAHCRHttpRoute = array of Arr2OAHCRHttpRoute;

  OAHUCharArrayBuffer = class;
  Arr1OAHUCharArrayBuffer = array of OAHUCharArrayBuffer;
  Arr2OAHUCharArrayBuffer = array of Arr1OAHUCharArrayBuffer;
  Arr3OAHUCharArrayBuffer = array of Arr2OAHUCharArrayBuffer;

  OAHIIHttpRequestParser = class;
  Arr1OAHIIHttpRequestParser = array of OAHIIHttpRequestParser;
  Arr2OAHIIHttpRequestParser = array of Arr1OAHIIHttpRequestParser;
  Arr3OAHIIHttpRequestParser = array of Arr2OAHIIHttpRequestParser;

  OATouchHandlerOld = class;
  Arr1OATouchHandlerOld = array of OATouchHandlerOld;
  Arr2OATouchHandlerOld = array of Arr1OATouchHandlerOld;
  Arr3OATouchHandlerOld = array of Arr2OATouchHandlerOld;

  OAHMethodNotSupportedException = class;
  Arr1OAHMethodNotSupportedException = array of OAHMethodNotSupportedException;
  Arr2OAHMethodNotSupportedException = array of Arr1OAHMethodNotSupportedException;
  Arr3OAHMethodNotSupportedException = array of Arr2OAHMethodNotSupportedException;

  OAHConnectionClosedException = class;
  Arr1OAHConnectionClosedException = array of OAHConnectionClosedException;
  Arr2OAHConnectionClosedException = array of Arr1OAHConnectionClosedException;
  Arr3OAHConnectionClosedException = array of Arr2OAHConnectionClosedException;

  OAHICBestMatchSpecFactory = class;
  Arr1OAHICBestMatchSpecFactory = array of OAHICBestMatchSpecFactory;
  Arr2OAHICBestMatchSpecFactory = array of Arr1OAHICBestMatchSpecFactory;
  Arr3OAHICBestMatchSpecFactory = array of Arr2OAHICBestMatchSpecFactory;

  OAHICCookieSpecBase = class;
  Arr1OAHICCookieSpecBase = array of OAHICCookieSpecBase;
  Arr2OAHICCookieSpecBase = array of Arr1OAHICCookieSpecBase;
  Arr3OAHICCookieSpecBase = array of Arr2OAHICCookieSpecBase;

  OAHPRequestContent = class;
  Arr1OAHPRequestContent = array of OAHPRequestContent;
  Arr2OAHPRequestContent = array of Arr1OAHPRequestContent;
  Arr3OAHPRequestContent = array of Arr2OAHPRequestContent;

  OXSSAXException = class;
  Arr1OXSSAXException = array of OXSSAXException;
  Arr2OXSSAXException = array of Arr1OXSSAXException;
  Arr3OXSSAXException = array of Arr2OXSSAXException;

  OAHICDefaultHttpRequestRetryHandler = class;
  Arr1OAHICDefaultHttpRequestRetryHandler = array of OAHICDefaultHttpRequestRetryHandler;
  Arr2OAHICDefaultHttpRequestRetryHandler = array of Arr1OAHICDefaultHttpRequestRetryHandler;
  Arr3OAHICDefaultHttpRequestRetryHandler = array of Arr2OAHICDefaultHttpRequestRetryHandler;

  OXSHAttributesImpl = class;
  Arr1OXSHAttributesImpl = array of OXSHAttributesImpl;
  Arr2OXSHAttributesImpl = array of Arr1OXSHAttributesImpl;
  Arr3OXSHAttributesImpl = array of Arr2OXSHAttributesImpl;

  OACDialChart = class;
  Arr1OACDialChart = array of OACDialChart;
  Arr2OACDialChart = array of Arr1OACDialChart;
  Arr3OACDialChart = array of Arr2OACDialChart;

  OAHCMHttpTrace = class;
  Arr1OAHCMHttpTrace = array of OAHCMHttpTrace;
  Arr2OAHCMHttpTrace = array of Arr1OAHCMHttpTrace;
  Arr3OAHCMHttpTrace = array of Arr2OAHCMHttpTrace;

  OAHMBasicHttpEntityEnclosingRequest = class;
  Arr1OAHMBasicHttpEntityEnclosingRequest = array of OAHMBasicHttpEntityEnclosingRequest;
  Arr2OAHMBasicHttpEntityEnclosingRequest = array of Arr1OAHMBasicHttpEntityEnclosingRequest;
  Arr3OAHMBasicHttpEntityEnclosingRequest = array of Arr2OAHMBasicHttpEntityEnclosingRequest;

  OAHICBasicSecureHandler = class;
  Arr1OAHICBasicSecureHandler = array of OAHICBasicSecureHandler;
  Arr2OAHICBasicSecureHandler = array of Arr1OAHICBasicSecureHandler;
  Arr3OAHICBasicSecureHandler = array of Arr2OAHICBasicSecureHandler;

  OAHAAuthenticationException = class;
  Arr1OAHAAuthenticationException = array of OAHAAuthenticationException;
  Arr2OAHAAuthenticationException = array of Arr1OAHAAuthenticationException;
  Arr3OAHAAuthenticationException = array of Arr2OAHAAuthenticationException;

  OAHPRequestConnControl = class;
  Arr1OAHPRequestConnControl = array of OAHPRequestConnControl;
  Arr2OAHPRequestConnControl = array of Arr1OAHPRequestConnControl;
  Arr3OAHPRequestConnControl = array of Arr2OAHPRequestConnControl;

  OAHICAbstractClientConnAdapter = class;
  Arr1OAHICAbstractClientConnAdapter = array of OAHICAbstractClientConnAdapter;
  Arr2OAHICAbstractClientConnAdapter = array of Arr1OAHICAbstractClientConnAdapter;
  Arr3OAHICAbstractClientConnAdapter = array of Arr2OAHICAbstractClientConnAdapter;

  OAHUnsupportedHttpVersionException = class;
  Arr1OAHUnsupportedHttpVersionException = array of OAHUnsupportedHttpVersionException;
  Arr2OAHUnsupportedHttpVersionException = array of Arr1OAHUnsupportedHttpVersionException;
  Arr3OAHUnsupportedHttpVersionException = array of Arr2OAHUnsupportedHttpVersionException;

  OAHIAUnsupportedDigestAlgorithmException = class;
  Arr1OAHIAUnsupportedDigestAlgorithmException = array of OAHIAUnsupportedDigestAlgorithmException;
  Arr2OAHIAUnsupportedDigestAlgorithmException = array of Arr1OAHIAUnsupportedDigestAlgorithmException;
  Arr3OAHIAUnsupportedDigestAlgorithmException = array of Arr2OAHIAUnsupportedDigestAlgorithmException;

  OAHMBasicHttpResponse = class;
  Arr1OAHMBasicHttpResponse = array of OAHMBasicHttpResponse;
  Arr2OAHMBasicHttpResponse = array of Arr1OAHMBasicHttpResponse;
  Arr3OAHMBasicHttpResponse = array of Arr2OAHMBasicHttpResponse;

  OAHCPResponseProcessCookies = class;
  Arr1OAHCPResponseProcessCookies = array of OAHCPResponseProcessCookies;
  Arr2OAHCPResponseProcessCookies = array of Arr1OAHCPResponseProcessCookies;
  Arr3OAHCPResponseProcessCookies = array of Arr2OAHCPResponseProcessCookies;

  OAHIIHttpTransportMetricsImpl = class;
  Arr1OAHIIHttpTransportMetricsImpl = array of OAHIIHttpTransportMetricsImpl;
  Arr2OAHIIHttpTransportMetricsImpl = array of Arr1OAHIIHttpTransportMetricsImpl;
  Arr3OAHIIHttpTransportMetricsImpl = array of Arr2OAHIIHttpTransportMetricsImpl;

  OAHICEntityEnclosingRequestWrapper = class;
  Arr1OAHICEntityEnclosingRequestWrapper = array of OAHICEntityEnclosingRequestWrapper;
  Arr2OAHICEntityEnclosingRequestWrapper = array of Arr1OAHICEntityEnclosingRequestWrapper;
  Arr3OAHICEntityEnclosingRequestWrapper = array of Arr2OAHICEntityEnclosingRequestWrapper;

  OAHEFileEntity = class;
  Arr1OAHEFileEntity = array of OAHEFileEntity;
  Arr2OAHEFileEntity = array of Arr1OAHEFileEntity;
  Arr3OAHEFileEntity = array of Arr2OAHEFileEntity;

  OAHCPConnManagerParamBean = class;
  Arr1OAHCPConnManagerParamBean = array of OAHCPConnManagerParamBean;
  Arr2OAHCPConnManagerParamBean = array of Arr1OAHCPConnManagerParamBean;
  Arr3OAHCPConnManagerParamBean = array of Arr2OAHCPConnManagerParamBean;

  OAHICDefaultClientConnection = class;
  Arr1OAHICDefaultClientConnection = array of OAHICDefaultClientConnection;
  Arr2OAHICDefaultClientConnection = array of Arr1OAHICDefaultClientConnection;
  Arr3OAHICDefaultClientConnection = array of Arr2OAHICDefaultClientConnection;

  OAHPResponseDate = class;
  Arr1OAHPResponseDate = array of OAHPResponseDate;
  Arr2OAHPResponseDate = array of Arr1OAHPResponseDate;
  Arr3OAHPResponseDate = array of Arr2OAHPResponseDate;

  OAHMBasicHttpRequest = class;
  Arr1OAHMBasicHttpRequest = array of OAHMBasicHttpRequest;
  Arr2OAHMBasicHttpRequest = array of Arr1OAHMBasicHttpRequest;
  Arr3OAHMBasicHttpRequest = array of Arr2OAHMBasicHttpRequest;

  OAHICRFC2965DiscardAttributeHandler = class;
  Arr1OAHICRFC2965DiscardAttributeHandler = array of OAHICRFC2965DiscardAttributeHandler;
  Arr2OAHICRFC2965DiscardAttributeHandler = array of Arr1OAHICRFC2965DiscardAttributeHandler;
  Arr3OAHICRFC2965DiscardAttributeHandler = array of Arr2OAHICRFC2965DiscardAttributeHandler;

  OACXYChart = class;
  Arr1OACXYChart = array of OACXYChart;
  Arr2OACXYChart = array of Arr1OACXYChart;
  Arr3OACXYChart = array of Arr2OACXYChart;

  OAHIEStrictContentLengthStrategy = class;
  Arr1OAHIEStrictContentLengthStrategy = array of OAHIEStrictContentLengthStrategy;
  Arr2OAHIEStrictContentLengthStrategy = array of Arr1OAHIEStrictContentLengthStrategy;
  Arr3OAHIEStrictContentLengthStrategy = array of Arr2OAHIEStrictContentLengthStrategy;

  OACLineChart = class;
  Arr1OACLineChart = array of OACLineChart;
  Arr2OACLineChart = array of Arr1OACLineChart;
  Arr3OACLineChart = array of Arr2OACLineChart;

  OAHMBufferedHeader = class;
  Arr1OAHMBufferedHeader = array of OAHMBufferedHeader;
  Arr2OAHMBufferedHeader = array of Arr1OAHMBufferedHeader;
  Arr3OAHMBufferedHeader = array of Arr2OAHMBufferedHeader;

  OAHCMalformedCookieException = class;
  Arr1OAHCMalformedCookieException = array of OAHCMalformedCookieException;
  Arr2OAHCMalformedCookieException = array of Arr1OAHCMalformedCookieException;
  Arr3OAHCMalformedCookieException = array of Arr2OAHCMalformedCookieException;

  OAHICIdleConnectionHandler = class;
  Arr1OAHICIdleConnectionHandler = array of OAHICIdleConnectionHandler;
  Arr2OAHICIdleConnectionHandler = array of Arr1OAHICIdleConnectionHandler;
  Arr3OAHICIdleConnectionHandler = array of Arr2OAHICIdleConnectionHandler;

  OAHUByteArrayBuffer = class;
  Arr1OAHUByteArrayBuffer = array of OAHUByteArrayBuffer;
  Arr2OAHUByteArrayBuffer = array of Arr1OAHUByteArrayBuffer;
  Arr3OAHUByteArrayBuffer = array of Arr2OAHUByteArrayBuffer;

  OARXYSeriesRenderer = class;
  Arr1OARXYSeriesRenderer = array of OARXYSeriesRenderer;
  Arr2OARXYSeriesRenderer = array of Arr1OARXYSeriesRenderer;
  Arr3OARXYSeriesRenderer = array of Arr2OARXYSeriesRenderer;

  OAHCMHttpHead = class;
  Arr1OAHCMHttpHead = array of OAHCMHttpHead;
  Arr2OAHCMHttpHead = array of Arr1OAHCMHttpHead;
  Arr3OAHCMHttpHead = array of Arr2OAHCMHttpHead;

  OAHCCircularRedirectException = class;
  Arr1OAHCCircularRedirectException = array of OAHCCircularRedirectException;
  Arr2OAHCCircularRedirectException = array of Arr1OAHCCircularRedirectException;
  Arr3OAHCCircularRedirectException = array of Arr2OAHCCircularRedirectException;

  OXSELocator2Impl = class;
  Arr1OXSELocator2Impl = array of OXSELocator2Impl;
  Arr2OXSELocator2Impl = array of Arr1OXSELocator2Impl;
  Arr3OXSELocator2Impl = array of Arr2OXSELocator2Impl;

  OAHICBrowserCompatSpecFactory = class;
  Arr1OAHICBrowserCompatSpecFactory = array of OAHICBrowserCompatSpecFactory;
  Arr2OAHICBrowserCompatSpecFactory = array of Arr1OAHICBrowserCompatSpecFactory;
  Arr3OAHICBrowserCompatSpecFactory = array of Arr2OAHICBrowserCompatSpecFactory;

  OAHProtocolException = class;
  Arr1OAHProtocolException = array of OAHProtocolException;
  Arr2OAHProtocolException = array of Arr1OAHProtocolException;
  Arr3OAHProtocolException = array of Arr2OAHProtocolException;

  OXSEDefaultHandler2 = class;
  Arr1OXSEDefaultHandler2 = array of OXSEDefaultHandler2;
  Arr2OXSEDefaultHandler2 = array of Arr1OXSEDefaultHandler2;
  Arr3OXSEDefaultHandler2 = array of Arr2OXSEDefaultHandler2;

  OAHIIChunkedOutputStream = class;
  Arr1OAHIIChunkedOutputStream = array of OAHIIChunkedOutputStream;
  Arr2OAHIIChunkedOutputStream = array of Arr1OAHIIChunkedOutputStream;
  Arr3OAHIIChunkedOutputStream = array of Arr2OAHIIChunkedOutputStream;

  OAHIEnglishReasonPhraseCatalog = class;
  Arr1OAHIEnglishReasonPhraseCatalog = array of OAHIEnglishReasonPhraseCatalog;
  Arr2OAHIEnglishReasonPhraseCatalog = array of Arr1OAHIEnglishReasonPhraseCatalog;
  Arr3OAHIEnglishReasonPhraseCatalog = array of Arr2OAHIEnglishReasonPhraseCatalog;

  OAHICRFC2965DomainAttributeHandler = class;
  Arr1OAHICRFC2965DomainAttributeHandler = array of OAHICRFC2965DomainAttributeHandler;
  Arr2OAHICRFC2965DomainAttributeHandler = array of Arr1OAHICRFC2965DomainAttributeHandler;
  Arr3OAHICRFC2965DomainAttributeHandler = array of Arr2OAHICRFC2965DomainAttributeHandler;

  OAHParseException = class;
  Arr1OAHParseException = array of OAHParseException;
  Arr2OAHParseException = array of Arr1OAHParseException;
  Arr3OAHParseException = array of Arr2OAHParseException;

  OAHICRFC2965SpecFactory = class;
  Arr1OAHICRFC2965SpecFactory = array of OAHICRFC2965SpecFactory;
  Arr2OAHICRFC2965SpecFactory = array of Arr1OAHICRFC2965SpecFactory;
  Arr3OAHICRFC2965SpecFactory = array of Arr2OAHICRFC2965SpecFactory;

  OAHCPHttpClientParams = class;
  Arr1OAHCPHttpClientParams = array of OAHCPHttpClientParams;
  Arr2OAHCPHttpClientParams = array of Arr1OAHCPHttpClientParams;
  Arr3OAHCPHttpClientParams = array of Arr2OAHCPHttpClientParams;

  OAHICTRefQueueWorker = class;
  Arr1OAHICTRefQueueWorker = array of OAHICTRefQueueWorker;
  Arr2OAHICTRefQueueWorker = array of Arr1OAHICTRefQueueWorker;
  Arr3OAHICTRefQueueWorker = array of Arr2OAHICTRefQueueWorker;

  OAHMBasicRequestLine = class;
  Arr1OAHMBasicRequestLine = array of OAHMBasicRequestLine;
  Arr2OAHMBasicRequestLine = array of Arr1OAHMBasicRequestLine;
  Arr3OAHMBasicRequestLine = array of Arr2OAHMBasicRequestLine;

  OAHICDefaultUserTokenHandler = class;
  Arr1OAHICDefaultUserTokenHandler = array of OAHICDefaultUserTokenHandler;
  Arr2OAHICDefaultUserTokenHandler = array of Arr1OAHICDefaultUserTokenHandler;
  Arr3OAHICDefaultUserTokenHandler = array of Arr2OAHICDefaultUserTokenHandler;

  OAHICTBasicPoolEntry = class;
  Arr1OAHICTBasicPoolEntry = array of OAHICTBasicPoolEntry;
  Arr2OAHICTBasicPoolEntry = array of Arr1OAHICTBasicPoolEntry;
  Arr3OAHICTBasicPoolEntry = array of Arr2OAHICTBasicPoolEntry;

  OAMXYMultipleSeriesDataset = class;
  Arr1OAMXYMultipleSeriesDataset = array of OAMXYMultipleSeriesDataset;
  Arr2OAMXYMultipleSeriesDataset = array of Arr1OAMXYMultipleSeriesDataset;
  Arr3OAMXYMultipleSeriesDataset = array of Arr2OAMXYMultipleSeriesDataset;

  OARBasicStroke = class;
  Arr1OARBasicStroke = array of OARBasicStroke;
  Arr2OARBasicStroke = array of Arr1OARBasicStroke;
  Arr3OARBasicStroke = array of Arr2OARBasicStroke;

  OXSHParserAdapter = class;
  Arr1OXSHParserAdapter = array of OXSHParserAdapter;
  Arr2OXSHParserAdapter = array of Arr1OXSHParserAdapter;
  Arr3OXSHParserAdapter = array of Arr2OXSHParserAdapter;

  OATAbstractTool = class;
  Arr1OATAbstractTool = array of OATAbstractTool;
  Arr2OATAbstractTool = array of Arr1OATAbstractTool;
  Arr3OATAbstractTool = array of Arr2OATAbstractTool;

  OAHPResponseConnControl = class;
  Arr1OAHPResponseConnControl = array of OAHPResponseConnControl;
  Arr2OAHPResponseConnControl = array of Arr1OAHPResponseConnControl;
  Arr3OAHPResponseConnControl = array of Arr2OAHPResponseConnControl;

  OAHIAbstractHttpServerConnection = class;
  Arr1OAHIAbstractHttpServerConnection = array of OAHIAbstractHttpServerConnection;
  Arr2OAHIAbstractHttpServerConnection = array of Arr1OAHIAbstractHttpServerConnection;
  Arr3OAHIAbstractHttpServerConnection = array of Arr2OAHIAbstractHttpServerConnection;

  OAHCUCloneUtils = class;
  Arr1OAHCUCloneUtils = array of OAHCUCloneUtils;
  Arr2OAHCUCloneUtils = array of Arr1OAHCUCloneUtils;
  Arr3OAHCUCloneUtils = array of Arr2OAHCUCloneUtils;

  OAHCPClientContextConfigurer = class;
  Arr1OAHCPClientContextConfigurer = array of OAHCPClientContextConfigurer;
  Arr2OAHCPClientContextConfigurer = array of Arr1OAHCPClientContextConfigurer;
  Arr3OAHCPClientContextConfigurer = array of Arr2OAHCPClientContextConfigurer;

  OXSHNamespaceSupport = class;
  Arr1OXSHNamespaceSupport = array of OXSHNamespaceSupport;
  Arr2OXSHNamespaceSupport = array of Arr1OXSHNamespaceSupport;
  Arr3OXSHNamespaceSupport = array of Arr2OXSHNamespaceSupport;

  OAHICDefaultClientConnectionOperator = class;
  Arr1OAHICDefaultClientConnectionOperator = array of OAHICDefaultClientConnectionOperator;
  Arr2OAHICDefaultClientConnectionOperator = array of Arr1OAHICDefaultClientConnectionOperator;
  Arr3OAHICDefaultClientConnectionOperator = array of Arr2OAHICDefaultClientConnectionOperator;

  OAHHttpException = class;
  Arr1OAHHttpException = array of OAHHttpException;
  Arr2OAHHttpException = array of Arr1OAHHttpException;
  Arr3OAHHttpException = array of Arr2OAHHttpException;

  OAHICDefaultProxyAuthenticationHandler = class;
  Arr1OAHICDefaultProxyAuthenticationHandler = array of OAHICDefaultProxyAuthenticationHandler;
  Arr2OAHICDefaultProxyAuthenticationHandler = array of Arr1OAHICDefaultProxyAuthenticationHandler;
  Arr3OAHICDefaultProxyAuthenticationHandler = array of Arr2OAHICDefaultProxyAuthenticationHandler;

  OAHCMHttpRequestBase = class;
  Arr1OAHCMHttpRequestBase = array of OAHCMHttpRequestBase;
  Arr2OAHCMHttpRequestBase = array of Arr1OAHCMHttpRequestBase;
  Arr3OAHCMHttpRequestBase = array of Arr2OAHCMHttpRequestBase;

  OAHIHttpConnectionMetricsImpl = class;
  Arr1OAHIHttpConnectionMetricsImpl = array of OAHIHttpConnectionMetricsImpl;
  Arr2OAHIHttpConnectionMetricsImpl = array of Arr1OAHIHttpConnectionMetricsImpl;
  Arr3OAHIHttpConnectionMetricsImpl = array of Arr2OAHIHttpConnectionMetricsImpl;

  OAHIISocketOutputBuffer = class;
  Arr1OAHIISocketOutputBuffer = array of OAHIISocketOutputBuffer;
  Arr2OAHIISocketOutputBuffer = array of Arr1OAHIISocketOutputBuffer;
  Arr3OAHIISocketOutputBuffer = array of Arr2OAHIISocketOutputBuffer;

  OXSHandlerBase = class;
  Arr1OXSHandlerBase = array of OXSHandlerBase;
  Arr2OXSHandlerBase = array of Arr1OXSHandlerBase;
  Arr3OXSHandlerBase = array of Arr2OXSHandlerBase;

  OAHAAuthScope = class;
  Arr1OAHAAuthScope = array of OAHAAuthScope;
  Arr2OAHAAuthScope = array of Arr1OAHAAuthScope;
  Arr3OAHAAuthScope = array of Arr2OAHAAuthScope;

  OAHEBufferedHttpEntity = class;
  Arr1OAHEBufferedHttpEntity = array of OAHEBufferedHttpEntity;
  Arr2OAHEBufferedHttpEntity = array of Arr1OAHEBufferedHttpEntity;
  Arr3OAHEBufferedHttpEntity = array of Arr2OAHEBufferedHttpEntity;

  OAHICBasicClientCookie2 = class;
  Arr1OAHICBasicClientCookie2 = array of OAHICBasicClientCookie2;
  Arr2OAHICBasicClientCookie2 = array of Arr1OAHICBasicClientCookie2;
  Arr3OAHICBasicClientCookie2 = array of Arr2OAHICBasicClientCookie2;

  OAHPRequestDate = class;
  Arr1OAHPRequestDate = array of OAHPRequestDate;
  Arr2OAHPRequestDate = array of Arr1OAHPRequestDate;
  Arr3OAHPRequestDate = array of Arr2OAHPRequestDate;

  OAHCCookieSpecRegistry = class;
  Arr1OAHCCookieSpecRegistry = array of OAHCCookieSpecRegistry;
  Arr2OAHCCookieSpecRegistry = array of Arr1OAHCCookieSpecRegistry;
  Arr3OAHCCookieSpecRegistry = array of Arr2OAHCCookieSpecRegistry;

  OAHPRequestUserAgent = class;
  Arr1OAHPRequestUserAgent = array of OAHPRequestUserAgent;
  Arr2OAHPRequestUserAgent = array of Arr1OAHPRequestUserAgent;
  Arr3OAHPRequestUserAgent = array of Arr2OAHPRequestUserAgent;

  OXSSAXNotSupportedException = class;
  Arr1OXSSAXNotSupportedException = array of OXSSAXNotSupportedException;
  Arr2OXSSAXNotSupportedException = array of Arr1OXSSAXNotSupportedException;
  Arr3OXSSAXNotSupportedException = array of Arr2OXSSAXNotSupportedException;

  OAHICDefaultTargetAuthenticationHandler = class;
  Arr1OAHICDefaultTargetAuthenticationHandler = array of OAHICDefaultTargetAuthenticationHandler;
  Arr2OAHICDefaultTargetAuthenticationHandler = array of Arr1OAHICDefaultTargetAuthenticationHandler;
  Arr3OAHICDefaultTargetAuthenticationHandler = array of Arr2OAHICDefaultTargetAuthenticationHandler;

  OAHPHttpAbstractParamBean = class;
  Arr1OAHPHttpAbstractParamBean = array of OAHPHttpAbstractParamBean;
  Arr2OAHPHttpAbstractParamBean = array of Arr1OAHPHttpAbstractParamBean;
  Arr3OAHPHttpAbstractParamBean = array of Arr2OAHPHttpAbstractParamBean;

  OAHCPRequestTargetAuthentication = class;
  Arr1OAHCPRequestTargetAuthentication = array of OAHCPRequestTargetAuthentication;
  Arr2OAHCPRequestTargetAuthentication = array of Arr1OAHCPRequestTargetAuthentication;
  Arr3OAHCPRequestTargetAuthentication = array of Arr2OAHCPRequestTargetAuthentication;

  OAHCBasicManagedEntity = class;
  Arr1OAHCBasicManagedEntity = array of OAHCBasicManagedEntity;
  Arr2OAHCBasicManagedEntity = array of Arr1OAHCBasicManagedEntity;
  Arr3OAHCBasicManagedEntity = array of Arr2OAHCBasicManagedEntity;

  OAHIAbstractHttpClientConnection = class;
  Arr1OAHIAbstractHttpClientConnection = array of OAHIAbstractHttpClientConnection;
  Arr2OAHIAbstractHttpClientConnection = array of Arr1OAHIAbstractHttpClientConnection;
  Arr3OAHIAbstractHttpClientConnection = array of Arr2OAHIAbstractHttpClientConnection;

  OAHANTUserPrincipal = class;
  Arr1OAHANTUserPrincipal = array of OAHANTUserPrincipal;
  Arr2OAHANTUserPrincipal = array of Arr1OAHANTUserPrincipal;
  Arr3OAHANTUserPrincipal = array of Arr2OAHANTUserPrincipal;

  OAHICWire = class;
  Arr1OAHICWire = array of OAHICWire;
  Arr2OAHICWire = array of Arr1OAHICWire;
  Arr3OAHICWire = array of Arr2OAHICWire;

  OAHICBrowserCompatSpec = class;
  Arr1OAHICBrowserCompatSpec = array of OAHICBrowserCompatSpec;
  Arr2OAHICBrowserCompatSpec = array of Arr1OAHICBrowserCompatSpec;
  Arr3OAHICBrowserCompatSpec = array of Arr2OAHICBrowserCompatSpec;

  OAHCHttpResponseException = class;
  Arr1OAHCHttpResponseException = array of OAHCHttpResponseException;
  Arr2OAHCHttpResponseException = array of Arr1OAHCHttpResponseException;
  Arr3OAHCHttpResponseException = array of Arr2OAHCHttpResponseException;

  OAHEBasicHttpEntity = class;
  Arr1OAHEBasicHttpEntity = array of OAHEBasicHttpEntity;
  Arr2OAHEBasicHttpEntity = array of Arr1OAHEBasicHttpEntity;
  Arr3OAHEBasicHttpEntity = array of Arr2OAHEBasicHttpEntity;

  OAHCUInetAddressUtils = class;
  Arr1OAHCUInetAddressUtils = array of OAHCUInetAddressUtils;
  Arr2OAHCUInetAddressUtils = array of Arr1OAHCUInetAddressUtils;
  Arr3OAHCUInetAddressUtils = array of Arr2OAHCUInetAddressUtils;

  OAHIAAuthSchemeBase = class;
  Arr1OAHIAAuthSchemeBase = array of OAHIAAuthSchemeBase;
  Arr2OAHIAAuthSchemeBase = array of Arr1OAHIAAuthSchemeBase;
  Arr3OAHIAAuthSchemeBase = array of Arr2OAHIAAuthSchemeBase;

  OAHIIIdentityOutputStream = class;
  Arr1OAHIIIdentityOutputStream = array of OAHIIIdentityOutputStream;
  Arr2OAHIIIdentityOutputStream = array of Arr1OAHIIIdentityOutputStream;
  Arr3OAHIIIdentityOutputStream = array of Arr2OAHIIIdentityOutputStream;

  OAHICTWaitingThreadAborter = class;
  Arr1OAHICTWaitingThreadAborter = array of OAHICTWaitingThreadAborter;
  Arr2OAHICTWaitingThreadAborter = array of Arr1OAHICTWaitingThreadAborter;
  Arr3OAHICTWaitingThreadAborter = array of Arr2OAHICTWaitingThreadAborter;

  OAHCConnectTimeoutException = class;
  Arr1OAHCConnectTimeoutException = array of OAHCConnectTimeoutException;
  Arr2OAHCConnectTimeoutException = array of Arr1OAHCConnectTimeoutException;
  Arr3OAHCConnectTimeoutException = array of Arr2OAHCConnectTimeoutException;

  OAHICRequestWrapper = class;
  Arr1OAHICRequestWrapper = array of OAHICRequestWrapper;
  Arr2OAHICRequestWrapper = array of Arr1OAHICRequestWrapper;
  Arr3OAHICRequestWrapper = array of Arr2OAHICRequestWrapper;

  OAHMBasicListHeaderIterator = class;
  Arr1OAHMBasicListHeaderIterator = array of OAHMBasicListHeaderIterator;
  Arr2OAHMBasicListHeaderIterator = array of Arr1OAHMBasicListHeaderIterator;
  Arr3OAHMBasicListHeaderIterator = array of Arr2OAHMBasicListHeaderIterator;

  OAMRangeCategorySeries = class;
  Arr1OAMRangeCategorySeries = array of OAMRangeCategorySeries;
  Arr2OAMRangeCategorySeries = array of Arr1OAMRangeCategorySeries;
  Arr3OAMRangeCategorySeries = array of Arr2OAMRangeCategorySeries;

  OAHCPAuthPolicy = class;
  Arr1OAHCPAuthPolicy = array of OAHCPAuthPolicy;
  Arr2OAHCPAuthPolicy = array of Arr1OAHCPAuthPolicy;
  Arr3OAHCPAuthPolicy = array of Arr2OAHCPAuthPolicy;

  OXSHXMLFilterImpl = class;
  Arr1OXSHXMLFilterImpl = array of OXSHXMLFilterImpl;
  Arr2OXSHXMLFilterImpl = array of Arr1OXSHXMLFilterImpl;
  Arr3OXSHXMLFilterImpl = array of Arr2OXSHXMLFilterImpl;

  OXSHXMLReaderAdapter = class;
  Arr1OXSHXMLReaderAdapter = array of OXSHXMLReaderAdapter;
  Arr2OXSHXMLReaderAdapter = array of Arr1OXSHXMLReaderAdapter;
  Arr3OXSHXMLReaderAdapter = array of Arr2OXSHXMLReaderAdapter;

  OAHAAUTH = class;
  Arr1OAHAAUTH = array of OAHAAUTH;
  Arr2OAHAAUTH = array of Arr1OAHAAUTH;
  Arr3OAHAAUTH = array of Arr2OAHAAUTH;

  OAHICProxySelectorRoutePlanner = class;
  Arr1OAHICProxySelectorRoutePlanner = array of OAHICProxySelectorRoutePlanner;
  Arr2OAHICProxySelectorRoutePlanner = array of Arr1OAHICProxySelectorRoutePlanner;
  Arr3OAHICProxySelectorRoutePlanner = array of Arr2OAHICProxySelectorRoutePlanner;

  OAHPHttpConnectionParamBean = class;
  Arr1OAHPHttpConnectionParamBean = array of OAHPHttpConnectionParamBean;
  Arr2OAHPHttpConnectionParamBean = array of Arr1OAHPHttpConnectionParamBean;
  Arr3OAHPHttpConnectionParamBean = array of Arr2OAHPHttpConnectionParamBean;

  OXSHAttributeListImpl = class;
  Arr1OXSHAttributeListImpl = array of OXSHAttributeListImpl;
  Arr2OXSHAttributeListImpl = array of Arr1OXSHAttributeListImpl;
  Arr3OXSHAttributeListImpl = array of Arr2OXSHAttributeListImpl;

  OAHEAbstractHttpEntity = class;
  Arr1OAHEAbstractHttpEntity = array of OAHEAbstractHttpEntity;
  Arr2OAHEAbstractHttpEntity = array of Arr1OAHEAbstractHttpEntity;
  Arr3OAHEAbstractHttpEntity = array of Arr2OAHEAbstractHttpEntity;

  OAHICLoggingSessionOutputBuffer = class;
  Arr1OAHICLoggingSessionOutputBuffer = array of OAHICLoggingSessionOutputBuffer;
  Arr2OAHICLoggingSessionOutputBuffer = array of Arr1OAHICLoggingSessionOutputBuffer;
  Arr3OAHICLoggingSessionOutputBuffer = array of Arr2OAHICLoggingSessionOutputBuffer;

  OXSHDefaultHandler = class;
  Arr1OXSHDefaultHandler = array of OXSHDefaultHandler;
  Arr2OXSHDefaultHandler = array of Arr1OXSHDefaultHandler;
  Arr3OXSHDefaultHandler = array of Arr2OXSHDefaultHandler;

  OXVXmlPullParserException = class;
  Arr1OXVXmlPullParserException = array of OXVXmlPullParserException;
  Arr2OXVXmlPullParserException = array of Arr1OXVXmlPullParserException;
  Arr3OXVXmlPullParserException = array of Arr2OXVXmlPullParserException;

  OAHICRFC2965VersionAttributeHandler = class;
  Arr1OAHICRFC2965VersionAttributeHandler = array of OAHICRFC2965VersionAttributeHandler;
  Arr2OAHICRFC2965VersionAttributeHandler = array of Arr1OAHICRFC2965VersionAttributeHandler;
  Arr3OAHICRFC2965VersionAttributeHandler = array of Arr2OAHICRFC2965VersionAttributeHandler;

  OAHPHttpProtocolParams = class;
  Arr1OAHPHttpProtocolParams = array of OAHPHttpProtocolParams;
  Arr2OAHPHttpProtocolParams = array of Arr1OAHPHttpProtocolParams;
  Arr3OAHPHttpProtocolParams = array of Arr2OAHPHttpProtocolParams;

  OACRoundChart = class;
  Arr1OACRoundChart = array of OACRoundChart;
  Arr2OACRoundChart = array of Arr1OACRoundChart;
  Arr3OACRoundChart = array of Arr2OACRoundChart;

  OAHICBasicCredentialsProvider = class;
  Arr1OAHICBasicCredentialsProvider = array of OAHICBasicCredentialsProvider;
  Arr2OAHICBasicCredentialsProvider = array of Arr1OAHICBasicCredentialsProvider;
  Arr3OAHICBasicCredentialsProvider = array of Arr2OAHICBasicCredentialsProvider;

  OJJSONTokener = class;
  Arr1OJJSONTokener = array of OJJSONTokener;
  Arr2OJJSONTokener = array of Arr1OJJSONTokener;
  Arr3OJJSONTokener = array of Arr2OJJSONTokener;

  OAHIIAbstractSessionOutputBuffer = class;
  Arr1OAHIIAbstractSessionOutputBuffer = array of OAHIIAbstractSessionOutputBuffer;
  Arr2OAHIIAbstractSessionOutputBuffer = array of Arr1OAHIIAbstractSessionOutputBuffer;
  Arr3OAHIIAbstractSessionOutputBuffer = array of Arr2OAHIIAbstractSessionOutputBuffer;

  OAHINoConnectionReuseStrategy = class;
  Arr1OAHINoConnectionReuseStrategy = array of OAHINoConnectionReuseStrategy;
  Arr2OAHINoConnectionReuseStrategy = array of Arr1OAHINoConnectionReuseStrategy;
  Arr3OAHINoConnectionReuseStrategy = array of Arr2OAHINoConnectionReuseStrategy;

  OXVSDriver = class;
  Arr1OXVSDriver = array of OXVSDriver;
  Arr2OXVSDriver = array of Arr1OXVSDriver;
  Arr3OXVSDriver = array of Arr2OXVSDriver;

  OAHICBasicCommentHandler = class;
  Arr1OAHICBasicCommentHandler = array of OAHICBasicCommentHandler;
  Arr2OAHICBasicCommentHandler = array of Arr1OAHICBasicCommentHandler;
  Arr3OAHICBasicCommentHandler = array of Arr2OAHICBasicCommentHandler;

  OAHMBasicHeaderValueParser = class;
  Arr1OAHMBasicHeaderValueParser = array of OAHMBasicHeaderValueParser;
  Arr2OAHMBasicHeaderValueParser = array of Arr1OAHMBasicHeaderValueParser;
  Arr3OAHMBasicHeaderValueParser = array of Arr2OAHMBasicHeaderValueParser;

  OAHICTBasicPoolEntryRef = class;
  Arr1OAHICTBasicPoolEntryRef = array of OAHICTBasicPoolEntryRef;
  Arr2OAHICTBasicPoolEntryRef = array of Arr1OAHICTBasicPoolEntryRef;
  Arr3OAHICTBasicPoolEntryRef = array of Arr2OAHICTBasicPoolEntryRef;

  OAMCategorySeries = class;
  Arr1OAMCategorySeries = array of OAMCategorySeries;
  Arr2OAMCategorySeries = array of Arr1OAMCategorySeries;
  Arr3OAMCategorySeries = array of Arr2OAMCategorySeries;

  OAHCPRequestAddCookies = class;
  Arr1OAHCPRequestAddCookies = array of OAHCPRequestAddCookies;
  Arr2OAHCPRequestAddCookies = array of Arr1OAHCPRequestAddCookies;
  Arr3OAHCPRequestAddCookies = array of Arr2OAHCPRequestAddCookies;

  OAHCPConnRouteParamBean = class;
  Arr1OAHCPConnRouteParamBean = array of OAHCPConnRouteParamBean;
  Arr2OAHCPConnRouteParamBean = array of Arr1OAHCPConnRouteParamBean;
  Arr3OAHCPConnRouteParamBean = array of Arr2OAHCPConnRouteParamBean;

  OAHCRedirectException = class;
  Arr1OAHCRedirectException = array of OAHCRedirectException;
  Arr2OAHCRedirectException = array of Arr1OAHCRedirectException;
  Arr3OAHCRedirectException = array of Arr2OAHCRedirectException;

  OAHICDefaultRedirectHandler = class;
  Arr1OAHICDefaultRedirectHandler = array of OAHICDefaultRedirectHandler;
  Arr2OAHICDefaultRedirectHandler = array of Arr1OAHICDefaultRedirectHandler;
  Arr3OAHICDefaultRedirectHandler = array of Arr2OAHICDefaultRedirectHandler;

  OXSEAttributes2Impl = class;
  Arr1OXSEAttributes2Impl = array of OXSEAttributes2Impl;
  Arr2OXSEAttributes2Impl = array of Arr1OXSEAttributes2Impl;
  Arr3OXSEAttributes2Impl = array of Arr2OXSEAttributes2Impl;

  OAUXYEntry = class;
  Arr1OAUXYEntry = array of OAUXYEntry;
  Arr2OAUXYEntry = array of Arr1OAUXYEntry;
  Arr3OAUXYEntry = array of Arr2OAUXYEntry;

  OAHICRFC2109DomainHandler = class;
  Arr1OAHICRFC2109DomainHandler = array of OAHICRFC2109DomainHandler;
  Arr2OAHICRFC2109DomainHandler = array of Arr1OAHICRFC2109DomainHandler;
  Arr3OAHICRFC2109DomainHandler = array of Arr2OAHICRFC2109DomainHandler;

  OAHCPConnManagerParams = class;
  Arr1OAHCPConnManagerParams = array of OAHCPConnManagerParams;
  Arr2OAHCPConnManagerParams = array of Arr1OAHCPConnManagerParams;
  Arr3OAHCPConnManagerParams = array of Arr2OAHCPConnManagerParams;

  OATPan = class;
  Arr1OATPan = array of OATPan;
  Arr2OATPan = array of Arr1OATPan;
  Arr3OATPan = array of Arr2OATPan;

  OAHPUriPatternMatcher = class;
  Arr1OAHPUriPatternMatcher = array of OAHPUriPatternMatcher;
  Arr2OAHPUriPatternMatcher = array of Arr1OAHPUriPatternMatcher;
  Arr3OAHPUriPatternMatcher = array of Arr2OAHPUriPatternMatcher;

  OAHIARFC2617Scheme = class;
  Arr1OAHIARFC2617Scheme = array of OAHIARFC2617Scheme;
  Arr2OAHIARFC2617Scheme = array of Arr1OAHIARFC2617Scheme;
  Arr3OAHIARFC2617Scheme = array of Arr2OAHIARFC2617Scheme;

  OAHEEntityTemplate = class;
  Arr1OAHEEntityTemplate = array of OAHEEntityTemplate;
  Arr2OAHEEntityTemplate = array of Arr1OAHEEntityTemplate;
  Arr3OAHEEntityTemplate = array of Arr2OAHEEntityTemplate;

  OACRangeStackedBarChart = class;
  Arr1OACRangeStackedBarChart = array of OACRangeStackedBarChart;
  Arr2OACRangeStackedBarChart = array of Arr1OACRangeStackedBarChart;
  Arr3OACRangeStackedBarChart = array of Arr2OACRangeStackedBarChart;

  OAHMHeaderGroup = class;
  Arr1OAHMHeaderGroup = array of OAHMHeaderGroup;
  Arr2OAHMHeaderGroup = array of Arr1OAHMHeaderGroup;
  Arr3OAHMHeaderGroup = array of Arr2OAHMHeaderGroup;

  OAHICTunnelRefusedException = class;
  Arr1OAHICTunnelRefusedException = array of OAHICTunnelRefusedException;
  Arr2OAHICTunnelRefusedException = array of Arr1OAHICTunnelRefusedException;
  Arr3OAHICTunnelRefusedException = array of Arr2OAHICTunnelRefusedException;

  OAHMBasicLineParser = class;
  Arr1OAHMBasicLineParser = array of OAHMBasicLineParser;
  Arr2OAHMBasicLineParser = array of Arr1OAHMBasicLineParser;
  Arr3OAHMBasicLineParser = array of Arr2OAHMBasicLineParser;

  OAHICTThreadSafeClientConnManager = class;
  Arr1OAHICTThreadSafeClientConnManager = array of OAHICTThreadSafeClientConnManager;
  Arr2OAHICTThreadSafeClientConnManager = array of Arr1OAHICTThreadSafeClientConnManager;
  Arr3OAHICTThreadSafeClientConnManager = array of Arr2OAHICTThreadSafeClientConnManager;

  OAHIDefaultHttpClientConnection = class;
  Arr1OAHIDefaultHttpClientConnection = array of OAHIDefaultHttpClientConnection;
  Arr2OAHIDefaultHttpClientConnection = array of Arr1OAHIDefaultHttpClientConnection;
  Arr3OAHIDefaultHttpClientConnection = array of Arr2OAHIDefaultHttpClientConnection;

  OAHCMHttpPut = class;
  Arr1OAHCMHttpPut = array of OAHCMHttpPut;
  Arr2OAHCMHttpPut = array of Arr1OAHCMHttpPut;
  Arr3OAHCMHttpPut = array of Arr2OAHCMHttpPut;

  OJJSONArray = class;
  Arr1OJJSONArray = array of OJJSONArray;
  Arr2OJJSONArray = array of Arr1OJJSONArray;
  Arr3OJJSONArray = array of Arr2OJJSONArray;

  OAHCCookiePathComparator = class;
  Arr1OAHCCookiePathComparator = array of OAHCCookiePathComparator;
  Arr2OAHCCookiePathComparator = array of Arr1OAHCCookiePathComparator;
  Arr3OAHCCookiePathComparator = array of Arr2OAHCCookiePathComparator;

  OAHMBasicHeaderElement = class;
  Arr1OAHMBasicHeaderElement = array of OAHMBasicHeaderElement;
  Arr2OAHMBasicHeaderElement = array of Arr1OAHMBasicHeaderElement;
  Arr3OAHMBasicHeaderElement = array of Arr2OAHMBasicHeaderElement;

  OACTimeChart = class;
  Arr1OACTimeChart = array of OACTimeChart;
  Arr2OACTimeChart = array of Arr1OACTimeChart;
  Arr3OACTimeChart = array of Arr2OACTimeChart;

  OXSHParserFactory = class;
  Arr1OXSHParserFactory = array of OXSHParserFactory;
  Arr2OXSHParserFactory = array of Arr1OXSHParserFactory;
  Arr3OXSHParserFactory = array of Arr2OXSHParserFactory;

  OAHICSingleClientConnManager = class;
  Arr1OAHICSingleClientConnManager = array of OAHICSingleClientConnManager;
  Arr2OAHICSingleClientConnManager = array of Arr1OAHICSingleClientConnManager;
  Arr3OAHICSingleClientConnManager = array of Arr2OAHICSingleClientConnManager;

  OAHCRRouteTracker = class;
  Arr1OAHCRRouteTracker = array of OAHCRRouteTracker;
  Arr2OAHCRRouteTracker = array of Arr1OAHCRRouteTracker;
  Arr3OAHCRRouteTracker = array of Arr2OAHCRRouteTracker;

  OXSHXMLReaderFactory = class;
  Arr1OXSHXMLReaderFactory = array of OXSHXMLReaderFactory;
  Arr2OXSHXMLReaderFactory = array of Arr1OXSHXMLReaderFactory;
  Arr3OXSHXMLReaderFactory = array of Arr2OXSHXMLReaderFactory;

  OAHIABasicSchemeFactory = class;
  Arr1OAHIABasicSchemeFactory = array of OAHIABasicSchemeFactory;
  Arr2OAHIABasicSchemeFactory = array of Arr1OAHIABasicSchemeFactory;
  Arr3OAHIABasicSchemeFactory = array of Arr2OAHIABasicSchemeFactory;

  OAHCPCookiePolicy = class;
  Arr1OAHCPCookiePolicy = array of OAHCPCookiePolicy;
  Arr2OAHCPCookiePolicy = array of Arr1OAHCPCookiePolicy;
  Arr3OAHCPCookiePolicy = array of Arr2OAHCPCookiePolicy;

  OAHIIAbstractSessionInputBuffer = class;
  Arr1OAHIIAbstractSessionInputBuffer = array of OAHIIAbstractSessionInputBuffer;
  Arr2OAHIIAbstractSessionInputBuffer = array of Arr1OAHIIAbstractSessionInputBuffer;
  Arr3OAHIIAbstractSessionInputBuffer = array of Arr2OAHIIAbstractSessionInputBuffer;

  OAHPBasicHttpProcessor = class;
  Arr1OAHPBasicHttpProcessor = array of OAHPBasicHttpProcessor;
  Arr2OAHPBasicHttpProcessor = array of Arr1OAHPBasicHttpProcessor;
  Arr3OAHPBasicHttpProcessor = array of Arr2OAHPBasicHttpProcessor;

  OAHCSBrowserCompatHostnameVerifier = class;
  Arr1OAHCSBrowserCompatHostnameVerifier = array of OAHCSBrowserCompatHostnameVerifier;
  Arr2OAHCSBrowserCompatHostnameVerifier = array of Arr1OAHCSBrowserCompatHostnameVerifier;
  Arr3OAHCSBrowserCompatHostnameVerifier = array of Arr2OAHCSBrowserCompatHostnameVerifier;

  OAHIIContentLengthOutputStream = class;
  Arr1OAHIIContentLengthOutputStream = array of OAHIIContentLengthOutputStream;
  Arr2OAHIIContentLengthOutputStream = array of Arr1OAHIIContentLengthOutputStream;
  Arr3OAHIIContentLengthOutputStream = array of Arr2OAHIIContentLengthOutputStream;

  OAHABasicUserPrincipal = class;
  Arr1OAHABasicUserPrincipal = array of OAHABasicUserPrincipal;
  Arr2OAHABasicUserPrincipal = array of Arr1OAHABasicUserPrincipal;
  Arr3OAHABasicUserPrincipal = array of Arr2OAHABasicUserPrincipal;

  OAHICAbstractCookieAttributeHandler = class;
  Arr1OAHICAbstractCookieAttributeHandler = array of OAHICAbstractCookieAttributeHandler;
  Arr2OAHICAbstractCookieAttributeHandler = array of Arr1OAHICAbstractCookieAttributeHandler;
  Arr3OAHICAbstractCookieAttributeHandler = array of Arr2OAHICAbstractCookieAttributeHandler;

  OAGraphicalActivity = class;
  Arr1OAGraphicalActivity = array of OAGraphicalActivity;
  Arr2OAGraphicalActivity = array of Arr1OAGraphicalActivity;
  Arr3OAGraphicalActivity = array of Arr2OAGraphicalActivity;

  OAHICRedirectLocations = class;
  Arr1OAHICRedirectLocations = array of OAHICRedirectLocations;
  Arr2OAHICRedirectLocations = array of Arr1OAHICRedirectLocations;
  Arr3OAHICRedirectLocations = array of Arr2OAHICRedirectLocations;

  OAHESerializableEntity = class;
  Arr1OAHESerializableEntity = array of OAHESerializableEntity;
  Arr2OAHESerializableEntity = array of Arr1OAHESerializableEntity;
  Arr3OAHESerializableEntity = array of Arr2OAHESerializableEntity;

  OATFitZoom = class;
  Arr1OATFitZoom = array of OATFitZoom;
  Arr2OATFitZoom = array of Arr1OATFitZoom;
  Arr3OATFitZoom = array of Arr2OATFitZoom;

  OAHCPConnRouteParams = class;
  Arr1OAHCPConnRouteParams = array of OAHCPConnRouteParams;
  Arr2OAHCPConnRouteParams = array of Arr1OAHCPConnRouteParams;
  Arr3OAHCPConnRouteParams = array of Arr2OAHCPConnRouteParams;

  OAHCMHttpPost = class;
  Arr1OAHCMHttpPost = array of OAHCMHttpPost;
  Arr2OAHCMHttpPost = array of Arr1OAHCMHttpPost;
  Arr3OAHCMHttpPost = array of Arr2OAHCMHttpPost;

  OAHIEEntityDeserializer = class;
  Arr1OAHIEEntityDeserializer = array of OAHIEEntityDeserializer;
  Arr2OAHIEEntityDeserializer = array of Arr1OAHIEEntityDeserializer;
  Arr3OAHIEEntityDeserializer = array of Arr2OAHIEEntityDeserializer;

  OAHUEncodingUtils = class;
  Arr1OAHUEncodingUtils = array of OAHUEncodingUtils;
  Arr2OAHUEncodingUtils = array of Arr1OAHUEncodingUtils;
  Arr3OAHUEncodingUtils = array of Arr2OAHUEncodingUtils;

  OAHCHttpHostConnectException = class;
  Arr1OAHCHttpHostConnectException = array of OAHCHttpHostConnectException;
  Arr2OAHCHttpHostConnectException = array of Arr1OAHCHttpHostConnectException;
  Arr3OAHCHttpHostConnectException = array of Arr2OAHCHttpHostConnectException;

  OAMXYValueSeries = class;
  Arr1OAMXYValueSeries = array of OAMXYValueSeries;
  Arr2OAMXYValueSeries = array of Arr1OAMXYValueSeries;
  Arr3OAMXYValueSeries = array of Arr2OAMXYValueSeries;

  OAHCRBasicRouteDirector = class;
  Arr1OAHCRBasicRouteDirector = array of OAHCRBasicRouteDirector;
  Arr2OAHCRBasicRouteDirector = array of Arr1OAHCRBasicRouteDirector;
  Arr3OAHCRBasicRouteDirector = array of Arr2OAHCRBasicRouteDirector;

  OAHCPRequestDefaultHeaders = class;
  Arr1OAHCPRequestDefaultHeaders = array of OAHCPRequestDefaultHeaders;
  Arr2OAHCPRequestDefaultHeaders = array of Arr1OAHCPRequestDefaultHeaders;
  Arr3OAHCPRequestDefaultHeaders = array of Arr2OAHCPRequestDefaultHeaders;

  OAHEByteArrayEntity = class;
  Arr1OAHEByteArrayEntity = array of OAHEByteArrayEntity;
  Arr2OAHEByteArrayEntity = array of Arr1OAHEByteArrayEntity;
  Arr3OAHEByteArrayEntity = array of Arr2OAHEByteArrayEntity;

  OAHCPRequestProxyAuthentication = class;
  Arr1OAHCPRequestProxyAuthentication = array of OAHCPRequestProxyAuthentication;
  Arr2OAHCPRequestProxyAuthentication = array of Arr1OAHCPRequestProxyAuthentication;
  Arr3OAHCPRequestProxyAuthentication = array of Arr2OAHCPRequestProxyAuthentication;

  OAHIADigestSchemeFactory = class;
  Arr1OAHIADigestSchemeFactory = array of OAHIADigestSchemeFactory;
  Arr2OAHIADigestSchemeFactory = array of Arr1OAHIADigestSchemeFactory;
  Arr3OAHIADigestSchemeFactory = array of Arr2OAHIADigestSchemeFactory;

  OAHICNetscapeDraftSpec = class;
  Arr1OAHICNetscapeDraftSpec = array of OAHICNetscapeDraftSpec;
  Arr2OAHICNetscapeDraftSpec = array of Arr1OAHICNetscapeDraftSpec;
  Arr3OAHICNetscapeDraftSpec = array of Arr2OAHICNetscapeDraftSpec;

  OAHICRFC2965CommentUrlAttributeHandler = class;
  Arr1OAHICRFC2965CommentUrlAttributeHandler = array of OAHICRFC2965CommentUrlAttributeHandler;
  Arr2OAHICRFC2965CommentUrlAttributeHandler = array of Arr1OAHICRFC2965CommentUrlAttributeHandler;
  Arr3OAHICRFC2965CommentUrlAttributeHandler = array of Arr2OAHICRFC2965CommentUrlAttributeHandler;

  OAHIABasicScheme = class;
  Arr1OAHIABasicScheme = array of OAHIABasicScheme;
  Arr2OAHIABasicScheme = array of Arr1OAHIABasicScheme;
  Arr3OAHIABasicScheme = array of Arr2OAHIABasicScheme;

  OAHICTWaitingThread = class;
  Arr1OAHICTWaitingThread = array of OAHICTWaitingThread;
  Arr2OAHICTWaitingThread = array of Arr1OAHICTWaitingThread;
  Arr3OAHICTWaitingThread = array of Arr2OAHICTWaitingThread;

  OAHPHttpProtocolParamBean = class;
  Arr1OAHPHttpProtocolParamBean = array of OAHPHttpProtocolParamBean;
  Arr2OAHPHttpProtocolParamBean = array of Arr1OAHPHttpProtocolParamBean;
  Arr3OAHPHttpProtocolParamBean = array of Arr2OAHPHttpProtocolParamBean;

  OAHProtocolVersion = class;
  Arr1OAHProtocolVersion = array of OAHProtocolVersion;
  Arr2OAHProtocolVersion = array of Arr1OAHProtocolVersion;
  Arr3OAHProtocolVersion = array of Arr2OAHProtocolVersion;

  OAHCSStrictHostnameVerifier = class;
  Arr1OAHCSStrictHostnameVerifier = array of OAHCSStrictHostnameVerifier;
  Arr2OAHCSStrictHostnameVerifier = array of Arr1OAHCSStrictHostnameVerifier;
  Arr3OAHCSStrictHostnameVerifier = array of Arr2OAHCSStrictHostnameVerifier;

  OAMXYSeries = class;
  Arr1OAMXYSeries = array of OAMXYSeries;
  Arr2OAMXYSeries = array of Arr1OAMXYSeries;
  Arr3OAMXYSeries = array of Arr2OAMXYSeries;

  OACClickableArea = class;
  Arr1OACClickableArea = array of OACClickableArea;
  Arr2OACClickableArea = array of Arr1OACClickableArea;
  Arr3OACClickableArea = array of Arr2OACClickableArea;

  OAHPDefaultedHttpParams = class;
  Arr1OAHPDefaultedHttpParams = array of OAHPDefaultedHttpParams;
  Arr2OAHPDefaultedHttpParams = array of Arr1OAHPDefaultedHttpParams;
  Arr3OAHPDefaultedHttpParams = array of Arr2OAHPDefaultedHttpParams;

  OAHMBasicStatusLine = class;
  Arr1OAHMBasicStatusLine = array of OAHMBasicStatusLine;
  Arr2OAHMBasicStatusLine = array of Arr1OAHMBasicStatusLine;
  Arr3OAHMBasicStatusLine = array of Arr2OAHMBasicStatusLine;

  OAHICBestMatchSpec = class;
  Arr1OAHICBestMatchSpec = array of OAHICBestMatchSpec;
  Arr2OAHICBestMatchSpec = array of Arr1OAHICBestMatchSpec;
  Arr3OAHICBestMatchSpec = array of Arr2OAHICBestMatchSpec;

  OAHIELaxContentLengthStrategy = class;
  Arr1OAHIELaxContentLengthStrategy = array of OAHIELaxContentLengthStrategy;
  Arr2OAHIELaxContentLengthStrategy = array of Arr1OAHIELaxContentLengthStrategy;
  Arr3OAHIELaxContentLengthStrategy = array of Arr2OAHIELaxContentLengthStrategy;

  OACCubicLineChart = class;
  Arr1OACCubicLineChart = array of OACCubicLineChart;
  Arr2OACCubicLineChart = array of Arr1OACCubicLineChart;
  Arr3OACCubicLineChart = array of Arr2OACCubicLineChart;

  OACBarChart = class;
  Arr1OACBarChart = array of OACBarChart;
  Arr2OACBarChart = array of Arr1OACBarChart;
  Arr3OACBarChart = array of Arr2OACBarChart;

  OAHICBasicClientCookie = class;
  Arr1OAHICBasicClientCookie = array of OAHICBasicClientCookie;
  Arr2OAHICBasicClientCookie = array of Arr1OAHICBasicClientCookie;
  Arr3OAHICBasicClientCookie = array of Arr2OAHICBasicClientCookie;

  OAHISocketHttpClientConnection = class;
  Arr1OAHISocketHttpClientConnection = array of OAHISocketHttpClientConnection;
  Arr2OAHISocketHttpClientConnection = array of Arr1OAHISocketHttpClientConnection;
  Arr3OAHISocketHttpClientConnection = array of Arr2OAHISocketHttpClientConnection;

  OAHAAuthSchemeRegistry = class;
  Arr1OAHAAuthSchemeRegistry = array of OAHAAuthSchemeRegistry;
  Arr2OAHAAuthSchemeRegistry = array of Arr1OAHAAuthSchemeRegistry;
  Arr3OAHAAuthSchemeRegistry = array of Arr2OAHAAuthSchemeRegistry;

  OAHICBasicMaxAgeHandler = class;
  Arr1OAHICBasicMaxAgeHandler = array of OAHICBasicMaxAgeHandler;
  Arr2OAHICBasicMaxAgeHandler = array of Arr1OAHICBasicMaxAgeHandler;
  Arr3OAHICBasicMaxAgeHandler = array of Arr2OAHICBasicMaxAgeHandler;

  OAHAPAuthParamBean = class;
  Arr1OAHAPAuthParamBean = array of OAHAPAuthParamBean;
  Arr2OAHAPAuthParamBean = array of Arr1OAHAPAuthParamBean;
  Arr3OAHAPAuthParamBean = array of Arr2OAHAPAuthParamBean;

  OAHCSSchemeRegistry = class;
  Arr1OAHCSSchemeRegistry = array of OAHCSSchemeRegistry;
  Arr2OAHCSSchemeRegistry = array of Arr1OAHCSSchemeRegistry;
  Arr3OAHCSSchemeRegistry = array of Arr2OAHCSSchemeRegistry;

  OAHIIIdentityInputStream = class;
  Arr1OAHIIIdentityInputStream = array of OAHIIIdentityInputStream;
  Arr2OAHIIIdentityInputStream = array of Arr1OAHIIIdentityInputStream;
  Arr3OAHIIIdentityInputStream = array of Arr2OAHIIIdentityInputStream;

  OAHPRequestTargetHost = class;
  Arr1OAHPRequestTargetHost = array of OAHPRequestTargetHost;
  Arr2OAHPRequestTargetHost = array of Arr1OAHPRequestTargetHost;
  Arr3OAHPRequestTargetHost = array of Arr2OAHPRequestTargetHost;

  OACDoughnutChart = class;
  Arr1OACDoughnutChart = array of OACDoughnutChart;
  Arr2OACDoughnutChart = array of Arr1OACDoughnutChart;
  Arr3OACDoughnutChart = array of Arr2OACDoughnutChart;

  OAHIISocketInputBuffer = class;
  Arr1OAHIISocketInputBuffer = array of OAHIISocketInputBuffer;
  Arr2OAHIISocketInputBuffer = array of Arr1OAHIISocketInputBuffer;
  Arr3OAHIISocketInputBuffer = array of Arr2OAHIISocketInputBuffer;

  OAHICBasicDomainHandler = class;
  Arr1OAHICBasicDomainHandler = array of OAHICBasicDomainHandler;
  Arr2OAHICBasicDomainHandler = array of Arr1OAHICBasicDomainHandler;
  Arr3OAHICBasicDomainHandler = array of Arr2OAHICBasicDomainHandler;

  OAHICBasicResponseHandler = class;
  Arr1OAHICBasicResponseHandler = array of OAHICBasicResponseHandler;
  Arr2OAHICBasicResponseHandler = array of Arr1OAHICBasicResponseHandler;
  Arr3OAHICBasicResponseHandler = array of Arr2OAHICBasicResponseHandler;

  OAHIANTLMEngineException = class;
  Arr1OAHIANTLMEngineException = array of OAHIANTLMEngineException;
  Arr2OAHIANTLMEngineException = array of Arr1OAHIANTLMEngineException;
  Arr3OAHIANTLMEngineException = array of Arr2OAHIANTLMEngineException;

  OJJSONObject = class;
  Arr1OJJSONObject = array of OJJSONObject;
  Arr2OJJSONObject = array of Arr1OJJSONObject;
  Arr3OJJSONObject = array of Arr2OJJSONObject;

  OAHCUURIUtils = class;
  Arr1OAHCUURIUtils = array of OAHCUURIUtils;
  Arr2OAHCUURIUtils = array of Arr1OAHCUURIUtils;
  Arr3OAHCUURIUtils = array of Arr2OAHCUURIUtils;

  OAHEStringEntity = class;
  Arr1OAHEStringEntity = array of OAHEStringEntity;
  Arr2OAHEStringEntity = array of Arr1OAHEStringEntity;
  Arr3OAHEStringEntity = array of Arr2OAHEStringEntity;

  OAHMBasicTokenIterator = class;
  Arr1OAHMBasicTokenIterator = array of OAHMBasicTokenIterator;
  Arr2OAHMBasicTokenIterator = array of Arr1OAHMBasicTokenIterator;
  Arr3OAHMBasicTokenIterator = array of Arr2OAHMBasicTokenIterator;

  OAHULangUtils = class;
  Arr1OAHULangUtils = array of OAHULangUtils;
  Arr2OAHULangUtils = array of Arr1OAHULangUtils;
  Arr3OAHULangUtils = array of Arr2OAHULangUtils;

  OJJSONStringer = class;
  Arr1OJJSONStringer = array of OJJSONStringer;
  Arr2OJJSONStringer = array of Arr1OJJSONStringer;
  Arr3OJJSONStringer = array of Arr2OJJSONStringer;

  OAHIDefaultConnectionReuseStrategy = class;
  Arr1OAHIDefaultConnectionReuseStrategy = array of OAHIDefaultConnectionReuseStrategy;
  Arr2OAHIDefaultConnectionReuseStrategy = array of Arr1OAHIDefaultConnectionReuseStrategy;
  Arr3OAHIDefaultConnectionReuseStrategy = array of Arr2OAHIDefaultConnectionReuseStrategy;

  OARXYMultipleSeriesRenderer = class;
  Arr1OARXYMultipleSeriesRenderer = array of OARXYMultipleSeriesRenderer;
  Arr2OARXYMultipleSeriesRenderer = array of Arr1OARXYMultipleSeriesRenderer;
  Arr3OARXYMultipleSeriesRenderer = array of Arr2OARXYMultipleSeriesRenderer;

  OAHCCookieIdentityComparator = class;
  Arr1OAHCCookieIdentityComparator = array of OAHCCookieIdentityComparator;
  Arr2OAHCCookieIdentityComparator = array of Arr1OAHCCookieIdentityComparator;
  Arr3OAHCCookieIdentityComparator = array of Arr2OAHCCookieIdentityComparator;

  OAHCNonRepeatableRequestException = class;
  Arr1OAHCNonRepeatableRequestException = array of OAHCNonRepeatableRequestException;
  Arr2OAHCNonRepeatableRequestException = array of Arr1OAHCNonRepeatableRequestException;
  Arr3OAHCNonRepeatableRequestException = array of Arr2OAHCNonRepeatableRequestException;

  OAHICAbstractPoolEntry = class;
  Arr1OAHICAbstractPoolEntry = array of OAHICAbstractPoolEntry;
  Arr2OAHICAbstractPoolEntry = array of Arr1OAHICAbstractPoolEntry;
  Arr3OAHICAbstractPoolEntry = array of Arr2OAHICAbstractPoolEntry;

  OAHCSPlainSocketFactory = class;
  Arr1OAHCSPlainSocketFactory = array of OAHCSPlainSocketFactory;
  Arr2OAHCSPlainSocketFactory = array of Arr1OAHCSPlainSocketFactory;
  Arr3OAHCSPlainSocketFactory = array of Arr2OAHCSPlainSocketFactory;

  OAHMBasicLineFormatter = class;
  Arr1OAHMBasicLineFormatter = array of OAHMBasicLineFormatter;
  Arr2OAHMBasicLineFormatter = array of Arr1OAHMBasicLineFormatter;
  Arr3OAHMBasicLineFormatter = array of Arr2OAHMBasicLineFormatter;

  OAHICLoggingSessionInputBuffer = class;
  Arr1OAHICLoggingSessionInputBuffer = array of OAHICLoggingSessionInputBuffer;
  Arr2OAHICLoggingSessionInputBuffer = array of Arr1OAHICLoggingSessionInputBuffer;
  Arr3OAHICLoggingSessionInputBuffer = array of Arr2OAHICLoggingSessionInputBuffer;

  OAHUEntityUtils = class;
  Arr1OAHUEntityUtils = array of OAHUEntityUtils;
  Arr2OAHUEntityUtils = array of Arr1OAHUEntityUtils;
  Arr3OAHUEntityUtils = array of Arr2OAHUEntityUtils;

  OAHIADigestScheme = class;
  Arr1OAHIADigestScheme = array of OAHIADigestScheme;
  Arr2OAHIADigestScheme = array of Arr1OAHIADigestScheme;
  Arr3OAHIADigestScheme = array of Arr2OAHIADigestScheme;

  OAHMalformedChunkCodingException = class;
  Arr1OAHMalformedChunkCodingException = array of OAHMalformedChunkCodingException;
  Arr2OAHMalformedChunkCodingException = array of Arr1OAHMalformedChunkCodingException;
  Arr3OAHMalformedChunkCodingException = array of Arr2OAHMalformedChunkCodingException;

  OACScatterChart = class;
  Arr1OACScatterChart = array of OACScatterChart;
  Arr2OACScatterChart = array of Arr1OACScatterChart;
  Arr3OACScatterChart = array of Arr2OACScatterChart;

  OAHPHTTP = class;
  Arr1OAHPHTTP = array of OAHPHTTP;
  Arr2OAHPHTTP = array of Arr1OAHPHTTP;
  Arr3OAHPHTTP = array of Arr2OAHPHTTP;

  OAHCMHttpDelete = class;
  Arr1OAHCMHttpDelete = array of OAHCMHttpDelete;
  Arr2OAHCMHttpDelete = array of Arr1OAHCMHttpDelete;
  Arr3OAHCMHttpDelete = array of Arr2OAHCMHttpDelete;

  OAHICDefaultResponseParser = class;
  Arr1OAHICDefaultResponseParser = array of OAHICDefaultResponseParser;
  Arr2OAHICDefaultResponseParser = array of Arr1OAHICDefaultResponseParser;
  Arr3OAHICDefaultResponseParser = array of Arr2OAHICDefaultResponseParser;

  OAHPRequestExpectContinue = class;
  Arr1OAHPRequestExpectContinue = array of OAHPRequestExpectContinue;
  Arr2OAHPRequestExpectContinue = array of Arr1OAHPRequestExpectContinue;
  Arr3OAHPRequestExpectContinue = array of Arr2OAHPRequestExpectContinue;

  OAHICDefaultConnectionKeepAliveStrategy = class;
  Arr1OAHICDefaultConnectionKeepAliveStrategy = array of OAHICDefaultConnectionKeepAliveStrategy;
  Arr2OAHICDefaultConnectionKeepAliveStrategy = array of Arr1OAHICDefaultConnectionKeepAliveStrategy;
  Arr3OAHICDefaultConnectionKeepAliveStrategy = array of Arr2OAHICDefaultConnectionKeepAliveStrategy;

  OAHPResponseContent = class;
  Arr1OAHPResponseContent = array of OAHPResponseContent;
  Arr2OAHPResponseContent = array of Arr1OAHPResponseContent;
  Arr3OAHPResponseContent = array of Arr2OAHPResponseContent;

  OAHIDefaultHttpServerConnection = class;
  Arr1OAHIDefaultHttpServerConnection = array of OAHIDefaultHttpServerConnection;
  Arr2OAHIDefaultHttpServerConnection = array of Arr1OAHIDefaultHttpServerConnection;
  Arr3OAHIDefaultHttpServerConnection = array of Arr2OAHIDefaultHttpServerConnection;

  OAHICRFC2965PortAttributeHandler = class;
  Arr1OAHICRFC2965PortAttributeHandler = array of OAHICRFC2965PortAttributeHandler;
  Arr2OAHICRFC2965PortAttributeHandler = array of Arr1OAHICRFC2965PortAttributeHandler;
  Arr3OAHICRFC2965PortAttributeHandler = array of Arr2OAHICRFC2965PortAttributeHandler;

  OACCombinedXYChart = class;
  Arr1OACCombinedXYChart = array of OACCombinedXYChart;
  Arr2OACCombinedXYChart = array of Arr1OACCombinedXYChart;
  Arr3OACCombinedXYChart = array of Arr2OACCombinedXYChart;

  OAMMultipleCategorySeries = class;
  Arr1OAMMultipleCategorySeries = array of OAMMultipleCategorySeries;
  Arr2OAMMultipleCategorySeries = array of Arr1OAMMultipleCategorySeries;
  Arr3OAMMultipleCategorySeries = array of Arr2OAMMultipleCategorySeries;

  OATouchHandler = class;
  Arr1OATouchHandler = array of OATouchHandler;
  Arr2OATouchHandler = array of Arr1OATouchHandler;
  Arr3OATouchHandler = array of Arr2OATouchHandler;

  OWDLLSException = class;
  Arr1OWDLLSException = array of OWDLLSException;
  Arr2OWDLLSException = array of Arr1OWDLLSException;
  Arr3OWDLLSException = array of Arr2OWDLLSException;

  OAHCClientProtocolException = class;
  Arr1OAHCClientProtocolException = array of OAHCClientProtocolException;
  Arr2OAHCClientProtocolException = array of Arr1OAHCClientProtocolException;
  Arr3OAHCClientProtocolException = array of Arr2OAHCClientProtocolException;

  OAHPHttpRequestExecutor = class;
  Arr1OAHPHttpRequestExecutor = array of OAHPHttpRequestExecutor;
  Arr2OAHPHttpRequestExecutor = array of Arr1OAHPHttpRequestExecutor;
  Arr3OAHPHttpRequestExecutor = array of Arr2OAHPHttpRequestExecutor;

  OAHICRFC2965Spec = class;
  Arr1OAHICRFC2965Spec = array of OAHICRFC2965Spec;
  Arr2OAHICRFC2965Spec = array of Arr1OAHICRFC2965Spec;
  Arr3OAHICRFC2965Spec = array of Arr2OAHICRFC2965Spec;

  OAHEInputStreamEntity = class;
  Arr1OAHEInputStreamEntity = array of OAHEInputStreamEntity;
  Arr2OAHEInputStreamEntity = array of Arr1OAHEInputStreamEntity;
  Arr3OAHEInputStreamEntity = array of Arr2OAHEInputStreamEntity;

  OXSInputSource = class;
  Arr1OXSInputSource = array of OXSInputSource;
  Arr2OXSInputSource = array of Arr1OXSInputSource;
  Arr3OXSInputSource = array of Arr2OXSInputSource;

  OAHICBasicCookieStore = class;
  Arr1OAHICBasicCookieStore = array of OAHICBasicCookieStore;
  Arr2OAHICBasicCookieStore = array of Arr1OAHICBasicCookieStore;
  Arr3OAHICBasicCookieStore = array of Arr2OAHICBasicCookieStore;

  OAHCSScheme = class;
  Arr1OAHCSScheme = array of OAHCSScheme;
  Arr2OAHCSScheme = array of Arr1OAHCSScheme;
  Arr3OAHCSScheme = array of Arr2OAHCSScheme;

  OAHCUURLEncodedUtils = class;
  Arr1OAHCUURLEncodedUtils = array of OAHCUURLEncodedUtils;
  Arr2OAHCUURLEncodedUtils = array of Arr1OAHCUURLEncodedUtils;
  Arr3OAHCUURLEncodedUtils = array of Arr2OAHCUURLEncodedUtils;

  OAHICDateParseException = class;
  Arr1OAHICDateParseException = array of OAHICDateParseException;
  Arr2OAHICDateParseException = array of Arr1OAHICDateParseException;
  Arr3OAHICDateParseException = array of Arr2OAHICDateParseException;

  OAHICTRouteSpecificPool = class;
  Arr1OAHICTRouteSpecificPool = array of OAHICTRouteSpecificPool;
  Arr2OAHICTRouteSpecificPool = array of Arr1OAHICTRouteSpecificPool;
  Arr3OAHICTRouteSpecificPool = array of Arr2OAHICTRouteSpecificPool;

  OAHPHttpRequestHandlerRegistry = class;
  Arr1OAHPHttpRequestHandlerRegistry = array of OAHPHttpRequestHandlerRegistry;
  Arr2OAHPHttpRequestHandlerRegistry = array of Arr1OAHPHttpRequestHandlerRegistry;
  Arr3OAHPHttpRequestHandlerRegistry = array of Arr2OAHPHttpRequestHandlerRegistry;

  OAHCSAllowAllHostnameVerifier = class;
  Arr1OAHCSAllowAllHostnameVerifier = array of OAHCSAllowAllHostnameVerifier;
  Arr2OAHCSAllowAllHostnameVerifier = array of Arr1OAHCSAllowAllHostnameVerifier;
  Arr3OAHCSAllowAllHostnameVerifier = array of Arr2OAHCSAllowAllHostnameVerifier;

  OAHAAuthState = class;
  Arr1OAHAAuthState = array of OAHAAuthState;
  Arr2OAHAAuthState = array of Arr1OAHAAuthState;
  Arr3OAHAAuthState = array of Arr2OAHAAuthState;

  OAHCPConnPerRouteBean = class;
  Arr1OAHCPConnPerRouteBean = array of OAHCPConnPerRouteBean;
  Arr2OAHCPConnPerRouteBean = array of Arr1OAHCPConnPerRouteBean;
  Arr3OAHCPConnPerRouteBean = array of Arr2OAHCPConnPerRouteBean;

  OXVXmlPullParserFactory = class;
  Arr1OXVXmlPullParserFactory = array of OXVXmlPullParserFactory;
  Arr2OXVXmlPullParserFactory = array of Arr1OXVXmlPullParserFactory;
  Arr3OXVXmlPullParserFactory = array of Arr2OXVXmlPullParserFactory;

  OAHPDefaultedHttpContext = class;
  Arr1OAHPDefaultedHttpContext = array of OAHPDefaultedHttpContext;
  Arr2OAHPDefaultedHttpContext = array of Arr1OAHPDefaultedHttpContext;
  Arr3OAHPDefaultedHttpContext = array of Arr2OAHPDefaultedHttpContext;

  OAHPHttpService = class;
  Arr1OAHPHttpService = array of OAHPHttpService;
  Arr2OAHPHttpService = array of Arr1OAHPHttpService;
  Arr3OAHPHttpService = array of Arr2OAHPHttpService;

  OAHIIHttpResponseParser = class;
  Arr1OAHIIHttpResponseParser = array of OAHIIHttpResponseParser;
  Arr2OAHIIHttpResponseParser = array of Arr1OAHIIHttpResponseParser;
  Arr3OAHIIHttpResponseParser = array of Arr2OAHIIHttpResponseParser;

  OAHCMHttpOptions = class;
  Arr1OAHCMHttpOptions = array of OAHCMHttpOptions;
  Arr2OAHCMHttpOptions = array of Arr1OAHCMHttpOptions;
  Arr3OAHCMHttpOptions = array of Arr2OAHCMHttpOptions;

  OXSSAXParseException = class;
  Arr1OXSSAXParseException = array of OXSSAXParseException;
  Arr2OXSSAXParseException = array of Arr1OXSSAXParseException;
  Arr3OXSSAXParseException = array of Arr2OXSSAXParseException;

  OAHCMHttpGet = class;
  Arr1OAHCMHttpGet = array of OAHCMHttpGet;
  Arr2OAHCMHttpGet = array of Arr1OAHCMHttpGet;
  Arr3OAHCMHttpGet = array of Arr2OAHCMHttpGet;

  OAHIIAbstractMessageParser = class;
  Arr1OAHIIAbstractMessageParser = array of OAHIIAbstractMessageParser;
  Arr2OAHIIAbstractMessageParser = array of Arr1OAHIIAbstractMessageParser;
  Arr3OAHIIAbstractMessageParser = array of Arr2OAHIIAbstractMessageParser;

  OAMPoint = class;
  Arr1OAMPoint = array of OAMPoint;
  Arr2OAMPoint = array of Arr1OAMPoint;
  Arr3OAMPoint = array of Arr2OAMPoint;

  OAChartFactory = class;
  Arr1OAChartFactory = array of OAChartFactory;
  Arr2OAChartFactory = array of Arr1OAChartFactory;
  Arr3OAChartFactory = array of Arr2OAChartFactory;

  OAHCSAbstractVerifier = class;
  Arr1OAHCSAbstractVerifier = array of OAHCSAbstractVerifier;
  Arr2OAHCSAbstractVerifier = array of Arr1OAHCSAbstractVerifier;
  Arr3OAHCSAbstractVerifier = array of Arr2OAHCSAbstractVerifier;

  OAHCConnectionPoolTimeoutException = class;
  Arr1OAHCConnectionPoolTimeoutException = array of OAHCConnectionPoolTimeoutException;
  Arr2OAHCConnectionPoolTimeoutException = array of Arr1OAHCConnectionPoolTimeoutException;
  Arr3OAHCConnectionPoolTimeoutException = array of Arr2OAHCConnectionPoolTimeoutException;

  OAHPHttpConnectionParams = class;
  Arr1OAHPHttpConnectionParams = array of OAHPHttpConnectionParams;
  Arr2OAHPHttpConnectionParams = array of Arr1OAHPHttpConnectionParams;
  Arr3OAHPHttpConnectionParams = array of Arr2OAHPHttpConnectionParams;

  OACPointStyle = class;
  Arr1OACPointStyle = array of OACPointStyle;
  Arr2OACPointStyle = array of Arr1OACPointStyle;
  Arr3OACPointStyle = array of Arr2OACPointStyle;

  OAHCEofSensorInputStream = class;
  Arr1OAHCEofSensorInputStream = array of OAHCEofSensorInputStream;
  Arr2OAHCEofSensorInputStream = array of Arr1OAHCEofSensorInputStream;
  Arr3OAHCEofSensorInputStream = array of Arr2OAHCEofSensorInputStream;

  OAUMathHelper = class;
  Arr1OAUMathHelper = array of OAUMathHelper;
  Arr2OAUMathHelper = array of Arr1OAUMathHelper;
  Arr3OAUMathHelper = array of Arr2OAUMathHelper;

  OAHCPClientParamBean = class;
  Arr1OAHCPClientParamBean = array of OAHCPClientParamBean;
  Arr2OAHCPClientParamBean = array of Arr1OAHCPClientParamBean;
  Arr3OAHCPClientParamBean = array of Arr2OAHCPClientParamBean;

  OAHUExceptionUtils = class;
  Arr1OAHUExceptionUtils = array of OAHUExceptionUtils;
  Arr2OAHUExceptionUtils = array of Arr1OAHUExceptionUtils;
  Arr3OAHUExceptionUtils = array of Arr2OAHUExceptionUtils;

  OAHICTAbstractConnPool = class;
  Arr1OAHICTAbstractConnPool = array of OAHICTAbstractConnPool;
  Arr2OAHICTAbstractConnPool = array of Arr1OAHICTAbstractConnPool;
  Arr3OAHICTAbstractConnPool = array of Arr2OAHICTAbstractConnPool;

  OAHUVersionInfo = class;
  Arr1OAHUVersionInfo = array of OAHUVersionInfo;
  Arr2OAHUVersionInfo = array of Arr1OAHUVersionInfo;
  Arr3OAHUVersionInfo = array of Arr2OAHUVersionInfo;

  OATZoom = class;
  Arr1OATZoom = array of OATZoom;
  Arr2OATZoom = array of Arr1OATZoom;
  Arr3OATZoom = array of Arr2OATZoom;

  OAHPSyncBasicHttpContext = class;
  Arr1OAHPSyncBasicHttpContext = array of OAHPSyncBasicHttpContext;
  Arr2OAHPSyncBasicHttpContext = array of Arr1OAHPSyncBasicHttpContext;
  Arr3OAHPSyncBasicHttpContext = array of Arr2OAHPSyncBasicHttpContext;

  OAHCPCookieSpecParamBean = class;
  Arr1OAHCPCookieSpecParamBean = array of OAHCPCookieSpecParamBean;
  Arr2OAHCPCookieSpecParamBean = array of Arr1OAHCPCookieSpecParamBean;
  Arr3OAHCPCookieSpecParamBean = array of Arr2OAHCPCookieSpecParamBean;

  OAGraphicalView = class;
  Arr1OAGraphicalView = array of OAGraphicalView;
  Arr2OAGraphicalView = array of Arr1OAGraphicalView;
  Arr3OAGraphicalView = array of Arr2OAGraphicalView;

  OACPieChart = class;
  Arr1OACPieChart = array of OACPieChart;
  Arr2OACPieChart = array of Arr1OACPieChart;
  Arr3OACPieChart = array of Arr2OACPieChart;

  OAHICDefaultHttpClient = class;
  Arr1OAHICDefaultHttpClient = array of OAHICDefaultHttpClient;
  Arr2OAHICDefaultHttpClient = array of Arr1OAHICDefaultHttpClient;
  Arr3OAHICDefaultHttpClient = array of Arr2OAHICDefaultHttpClient;

  OAHICRFC2109SpecFactory = class;
  Arr1OAHICRFC2109SpecFactory = array of OAHICRFC2109SpecFactory;
  Arr2OAHICRFC2109SpecFactory = array of Arr1OAHICRFC2109SpecFactory;
  Arr3OAHICRFC2109SpecFactory = array of Arr2OAHICRFC2109SpecFactory;

  OAHIIHttpResponseWriter = class;
  Arr1OAHIIHttpResponseWriter = array of OAHIIHttpResponseWriter;
  Arr2OAHIIHttpResponseWriter = array of Arr1OAHIIHttpResponseWriter;
  Arr3OAHIIHttpResponseWriter = array of Arr2OAHIIHttpResponseWriter;

  OAHIIHttpRequestWriter = class;
  Arr1OAHIIHttpRequestWriter = array of OAHIIHttpRequestWriter;
  Arr2OAHIIHttpRequestWriter = array of Arr1OAHIIHttpRequestWriter;
  Arr3OAHIIHttpRequestWriter = array of Arr2OAHIIHttpRequestWriter;

  OAHAPAuthParams = class;
  Arr1OAHAPAuthParams = array of OAHAPAuthParams;
  Arr2OAHAPAuthParams = array of Arr1OAHAPAuthParams;
  Arr3OAHAPAuthParams = array of Arr2OAHAPAuthParams;

  OAHANTCredentials = class;
  Arr1OAHANTCredentials = array of OAHANTCredentials;
  Arr2OAHANTCredentials = array of Arr1OAHANTCredentials;
  Arr3OAHANTCredentials = array of Arr2OAHANTCredentials;

  OAHHttpVersion = class;
  Arr1OAHHttpVersion = array of OAHHttpVersion;
  Arr2OAHHttpVersion = array of Arr1OAHHttpVersion;
  Arr3OAHHttpVersion = array of Arr2OAHHttpVersion;

  OAMSeriesSelection = class;
  Arr1OAMSeriesSelection = array of OAMSeriesSelection;
  Arr2OAMSeriesSelection = array of Arr1OAMSeriesSelection;
  Arr3OAMSeriesSelection = array of Arr2OAMSeriesSelection;

  OAHICTConnPoolByRoute = class;
  Arr1OAHICTConnPoolByRoute = array of OAHICTConnPoolByRoute;
  Arr2OAHICTConnPoolByRoute = array of Arr1OAHICTConnPoolByRoute;
  Arr3OAHICTConnPoolByRoute = array of Arr2OAHICTConnPoolByRoute;

  OWDDOMException = class;
  Arr1OWDDOMException = array of OWDDOMException;
  Arr2OWDDOMException = array of Arr1OWDDOMException;
  Arr3OWDDOMException = array of Arr2OWDDOMException;

  OAHCMultihomePlainSocketFactory = class;
  Arr1OAHCMultihomePlainSocketFactory = array of OAHCMultihomePlainSocketFactory;
  Arr2OAHCMultihomePlainSocketFactory = array of Arr1OAHCMultihomePlainSocketFactory;
  Arr3OAHCMultihomePlainSocketFactory = array of Arr2OAHCMultihomePlainSocketFactory;

  OAHIEEntitySerializer = class;
  Arr1OAHIEEntitySerializer = array of OAHIEEntitySerializer;
  Arr2OAHIEEntitySerializer = array of Arr1OAHIEEntitySerializer;
  Arr3OAHIEEntitySerializer = array of Arr2OAHIEEntitySerializer;

  OAHAMalformedChallengeException = class;
  Arr1OAHAMalformedChallengeException = array of OAHAMalformedChallengeException;
  Arr2OAHAMalformedChallengeException = array of Arr1OAHAMalformedChallengeException;
  Arr3OAHAMalformedChallengeException = array of Arr2OAHAMalformedChallengeException;

  OAHPBasicHttpParams = class;
  Arr1OAHPBasicHttpParams = array of OAHPBasicHttpParams;
  Arr2OAHPBasicHttpParams = array of Arr1OAHPBasicHttpParams;
  Arr3OAHPBasicHttpParams = array of Arr2OAHPBasicHttpParams;

  OACPieSegment = class;
  Arr1OACPieSegment = array of OACPieSegment;
  Arr2OACPieSegment = array of Arr1OACPieSegment;
  Arr3OACPieSegment = array of Arr2OACPieSegment;

  OAHEHttpEntityWrapper = class;
  Arr1OAHEHttpEntityWrapper = array of OAHEHttpEntityWrapper;
  Arr2OAHEHttpEntityWrapper = array of Arr1OAHEHttpEntityWrapper;
  Arr3OAHEHttpEntityWrapper = array of Arr2OAHEHttpEntityWrapper;

  OAHCMHttpEntityEnclosingRequestBase = class;
  Arr1OAHCMHttpEntityEnclosingRequestBase = array of OAHCMHttpEntityEnclosingRequestBase;
  Arr2OAHCMHttpEntityEnclosingRequestBase = array of Arr1OAHCMHttpEntityEnclosingRequestBase;
  Arr3OAHCMHttpEntityEnclosingRequestBase = array of Arr2OAHCMHttpEntityEnclosingRequestBase;

  OAHICRoutedRequest = class;
  Arr1OAHICRoutedRequest = array of OAHICRoutedRequest;
  Arr2OAHICRoutedRequest = array of Arr1OAHICRoutedRequest;
  Arr3OAHICRoutedRequest = array of Arr2OAHICRoutedRequest;

  OAHISocketHttpServerConnection = class;
  Arr1OAHISocketHttpServerConnection = array of OAHISocketHttpServerConnection;
  Arr2OAHISocketHttpServerConnection = array of Arr1OAHISocketHttpServerConnection;
  Arr3OAHISocketHttpServerConnection = array of Arr2OAHISocketHttpServerConnection;

  OAHCBasicEofSensorWatcher = class;
  Arr1OAHCBasicEofSensorWatcher = array of OAHCBasicEofSensorWatcher;
  Arr2OAHCBasicEofSensorWatcher = array of Arr1OAHCBasicEofSensorWatcher;
  Arr3OAHCBasicEofSensorWatcher = array of Arr2OAHCBasicEofSensorWatcher;

  OAHMAbstractHttpMessage = class;
  Arr1OAHMAbstractHttpMessage = array of OAHMAbstractHttpMessage;
  Arr2OAHMAbstractHttpMessage = array of Arr1OAHMAbstractHttpMessage;
  Arr3OAHMAbstractHttpMessage = array of Arr2OAHMAbstractHttpMessage;

  OAHICClientParamsStack = class;
  Arr1OAHICClientParamsStack = array of OAHICClientParamsStack;
  Arr2OAHICClientParamsStack = array of Arr1OAHICClientParamsStack;
  Arr3OAHICClientParamsStack = array of Arr2OAHICClientParamsStack;

  OARDialRenderer = class;
  Arr1OARDialRenderer = array of OARDialRenderer;
  Arr2OARDialRenderer = array of Arr1OARDialRenderer;
  Arr3OARDialRenderer = array of Arr2OARDialRenderer;

  OARSimpleSeriesRenderer = class;
  Arr1OARSimpleSeriesRenderer = array of OARSimpleSeriesRenderer;
  Arr2OARSimpleSeriesRenderer = array of Arr1OARSimpleSeriesRenderer;
  Arr3OARSimpleSeriesRenderer = array of Arr2OARSimpleSeriesRenderer;

  OAHCPConnConnectionParamBean = class;
  Arr1OAHCPConnConnectionParamBean = array of OAHCPConnConnectionParamBean;
  Arr2OAHCPConnConnectionParamBean = array of Arr1OAHCPConnConnectionParamBean;
  Arr3OAHCPConnConnectionParamBean = array of Arr2OAHCPConnConnectionParamBean;

  OAHICBasicExpiresHandler = class;
  Arr1OAHICBasicExpiresHandler = array of OAHICBasicExpiresHandler;
  Arr2OAHICBasicExpiresHandler = array of Arr1OAHICBasicExpiresHandler;
  Arr3OAHICBasicExpiresHandler = array of Arr2OAHICBasicExpiresHandler;

  OAHCCookieOrigin = class;
  Arr1OAHCCookieOrigin = array of OAHCCookieOrigin;
  Arr2OAHCCookieOrigin = array of Arr1OAHCCookieOrigin;
  Arr3OAHCCookieOrigin = array of Arr2OAHCCookieOrigin;

  OAHNoHttpResponseException = class;
  Arr1OAHNoHttpResponseException = array of OAHNoHttpResponseException;
  Arr2OAHNoHttpResponseException = array of Arr1OAHNoHttpResponseException;
  Arr3OAHNoHttpResponseException = array of Arr2OAHNoHttpResponseException;

  OAHICDefaultHttpRoutePlanner = class;
  Arr1OAHICDefaultHttpRoutePlanner = array of OAHICDefaultHttpRoutePlanner;
  Arr2OAHICDefaultHttpRoutePlanner = array of Arr1OAHICDefaultHttpRoutePlanner;
  Arr3OAHICDefaultHttpRoutePlanner = array of Arr2OAHICDefaultHttpRoutePlanner;

  OARDefaultRenderer = class;
  Arr1OARDefaultRenderer = array of OARDefaultRenderer;
  Arr2OARDefaultRenderer = array of Arr1OARDefaultRenderer;
  Arr3OARDefaultRenderer = array of Arr2OARDefaultRenderer;

  OAHICTBasicPooledConnAdapter = class;
  Arr1OAHICTBasicPooledConnAdapter = array of OAHICTBasicPooledConnAdapter;
  Arr2OAHICTBasicPooledConnAdapter = array of Arr1OAHICTBasicPooledConnAdapter;
  Arr3OAHICTBasicPooledConnAdapter = array of Arr2OAHICTBasicPooledConnAdapter;

  OAHIIAbstractMessageWriter = class;
  Arr1OAHIIAbstractMessageWriter = array of OAHIIAbstractMessageWriter;
  Arr2OAHIIAbstractMessageWriter = array of Arr1OAHIIAbstractMessageWriter;
  Arr3OAHIIAbstractMessageWriter = array of Arr2OAHIIAbstractMessageWriter;

  OAHICBasicPathHandler = class;
  Arr1OAHICBasicPathHandler = array of OAHICBasicPathHandler;
  Arr2OAHICBasicPathHandler = array of Arr1OAHICBasicPathHandler;
  Arr3OAHICBasicPathHandler = array of Arr2OAHICBasicPathHandler;

  OAHIIContentLengthInputStream = class;
  Arr1OAHIIContentLengthInputStream = array of OAHIIContentLengthInputStream;
  Arr2OAHIIContentLengthInputStream = array of Arr1OAHIIContentLengthInputStream;
  Arr3OAHIIContentLengthInputStream = array of Arr2OAHIIContentLengthInputStream;

  OAHMParserCursor = class;
  Arr1OAHMParserCursor = array of OAHMParserCursor;
  Arr2OAHMParserCursor = array of Arr1OAHMParserCursor;
  Arr3OAHMParserCursor = array of Arr2OAHMParserCursor;

  OAHICAbstractHttpClient = class;
  Arr1OAHICAbstractHttpClient = array of OAHICAbstractHttpClient;
  Arr2OAHICAbstractHttpClient = array of Arr1OAHICAbstractHttpClient;
  Arr3OAHICAbstractHttpClient = array of Arr2OAHICAbstractHttpClient;

  OAUIndexXYMap = class;
  Arr1OAUIndexXYMap = array of OAUIndexXYMap;
  Arr2OAUIndexXYMap = array of Arr1OAUIndexXYMap;
  Arr3OAUIndexXYMap = array of Arr2OAUIndexXYMap;

  OAHMBasicHeaderElementIterator = class;
  Arr1OAHMBasicHeaderElementIterator = array of OAHMBasicHeaderElementIterator;
  Arr2OAHMBasicHeaderElementIterator = array of Arr1OAHMBasicHeaderElementIterator;
  Arr3OAHMBasicHeaderElementIterator = array of Arr2OAHMBasicHeaderElementIterator;

  OAHICDateUtils = class;
  Arr1OAHICDateUtils = array of OAHICDateUtils;
  Arr2OAHICDateUtils = array of Arr1OAHICDateUtils;
  Arr3OAHICDateUtils = array of Arr2OAHICDateUtils;

  OATZoomEvent = class;
  Arr1OATZoomEvent = array of OATZoomEvent;
  Arr2OATZoomEvent = array of Arr1OATZoomEvent;
  Arr3OATZoomEvent = array of Arr2OATZoomEvent;

  OAHIIChunkedInputStream = class;
  Arr1OAHIIChunkedInputStream = array of OAHIIChunkedInputStream;
  Arr2OAHIIChunkedInputStream = array of Arr1OAHIIChunkedInputStream;
  Arr3OAHIIChunkedInputStream = array of Arr2OAHIIChunkedInputStream;

  OAHMBasicHeader = class;
  Arr1OAHMBasicHeader = array of OAHMBasicHeader;
  Arr2OAHMBasicHeader = array of Arr1OAHMBasicHeader;
  Arr3OAHMBasicHeader = array of Arr2OAHMBasicHeader;

  OAHICNetscapeDomainHandler = class;
  Arr1OAHICNetscapeDomainHandler = array of OAHICNetscapeDomainHandler;
  Arr2OAHICNetscapeDomainHandler = array of Arr1OAHICNetscapeDomainHandler;
  Arr3OAHICNetscapeDomainHandler = array of Arr2OAHICNetscapeDomainHandler;

  OAHMBasicNameValuePair = class;
  Arr1OAHMBasicNameValuePair = array of OAHMBasicNameValuePair;
  Arr2OAHMBasicNameValuePair = array of Arr1OAHMBasicNameValuePair;
  Arr3OAHMBasicNameValuePair = array of Arr2OAHMBasicNameValuePair;

  OAHICRFC2109VersionHandler = class;
  Arr1OAHICRFC2109VersionHandler = array of OAHICRFC2109VersionHandler;
  Arr2OAHICRFC2109VersionHandler = array of Arr1OAHICRFC2109VersionHandler;
  Arr3OAHICRFC2109VersionHandler = array of Arr2OAHICRFC2109VersionHandler;

  OAHICNetscapeDraftHeaderParser = class;
  Arr1OAHICNetscapeDraftHeaderParser = array of OAHICNetscapeDraftHeaderParser;
  Arr2OAHICNetscapeDraftHeaderParser = array of Arr1OAHICNetscapeDraftHeaderParser;
  Arr3OAHICNetscapeDraftHeaderParser = array of Arr2OAHICNetscapeDraftHeaderParser;

  OAHPBasicHttpContext = class;
  Arr1OAHPBasicHttpContext = array of OAHPBasicHttpContext;
  Arr2OAHPBasicHttpContext = array of Arr1OAHPBasicHttpContext;
  Arr3OAHPBasicHttpContext = array of Arr2OAHPBasicHttpContext;

  OAHICRFC2109Spec = class;
  Arr1OAHICRFC2109Spec = array of OAHICRFC2109Spec;
  Arr2OAHICRFC2109Spec = array of Arr1OAHICRFC2109Spec;
  Arr3OAHICRFC2109Spec = array of Arr2OAHICRFC2109Spec;

  OACAbstractChart = class;
  Arr1OACAbstractChart = array of OACAbstractChart;
  Arr2OACAbstractChart = array of Arr1OACAbstractChart;
  Arr3OACAbstractChart = array of Arr2OACAbstractChart;

  OXSSAXNotRecognizedException = class;
  Arr1OXSSAXNotRecognizedException = array of OXSSAXNotRecognizedException;
  Arr2OXSSAXNotRecognizedException = array of Arr1OXSSAXNotRecognizedException;
  Arr3OXSSAXNotRecognizedException = array of Arr2OXSSAXNotRecognizedException;

  OAHICAbstractCookieSpec = class;
  Arr1OAHICAbstractCookieSpec = array of OAHICAbstractCookieSpec;
  Arr2OAHICAbstractCookieSpec = array of Arr1OAHICAbstractCookieSpec;
  Arr3OAHICAbstractCookieSpec = array of Arr2OAHICAbstractCookieSpec;

  OAHAInvalidCredentialsException = class;
  Arr1OAHAInvalidCredentialsException = array of OAHAInvalidCredentialsException;
  Arr2OAHAInvalidCredentialsException = array of Arr1OAHAInvalidCredentialsException;
  Arr3OAHAInvalidCredentialsException = array of Arr2OAHAInvalidCredentialsException;

  OAHMBasicHeaderIterator = class;
  Arr1OAHMBasicHeaderIterator = array of OAHMBasicHeaderIterator;
  Arr2OAHMBasicHeaderIterator = array of Arr1OAHMBasicHeaderIterator;
  Arr3OAHMBasicHeaderIterator = array of Arr2OAHMBasicHeaderIterator;

  OAHICAbstractAuthenticationHandler = class;
  Arr1OAHICAbstractAuthenticationHandler = array of OAHICAbstractAuthenticationHandler;
  Arr2OAHICAbstractAuthenticationHandler = array of Arr1OAHICAbstractAuthenticationHandler;
  Arr3OAHICAbstractAuthenticationHandler = array of Arr2OAHICAbstractAuthenticationHandler;

  OAHPResponseServer = class;
  Arr1OAHPResponseServer = array of OAHPResponseServer;
  Arr2OAHPResponseServer = array of Arr1OAHPResponseServer;
  Arr3OAHPResponseServer = array of Arr2OAHPResponseServer;

  OXSHLocatorImpl = class;
  Arr1OXSHLocatorImpl = array of OXSHLocatorImpl;
  Arr2OXSHLocatorImpl = array of Arr1OXSHLocatorImpl;
  Arr3OXSHLocatorImpl = array of Arr2OXSHLocatorImpl;

  OAHPHttpDateGenerator = class;
  Arr1OAHPHttpDateGenerator = array of OAHPHttpDateGenerator;
  Arr2OAHPHttpDateGenerator = array of Arr1OAHPHttpDateGenerator;
  Arr3OAHPHttpDateGenerator = array of Arr2OAHPHttpDateGenerator;

  OACPieMapper = class;
  Arr1OACPieMapper = array of OACPieMapper;
  Arr2OACPieMapper = array of Arr1OACPieMapper;
  Arr3OACPieMapper = array of Arr2OACPieMapper;

  OAHMBasicHeaderValueFormatter = class;
  Arr1OAHMBasicHeaderValueFormatter = array of OAHMBasicHeaderValueFormatter;
  Arr2OAHMBasicHeaderValueFormatter = array of Arr1OAHMBasicHeaderValueFormatter;
  Arr3OAHMBasicHeaderValueFormatter = array of Arr2OAHMBasicHeaderValueFormatter;

  OJJSONException = class;
  Arr1OJJSONException = array of OJJSONException;
  Arr2OJJSONException = array of Arr1OJJSONException;
  Arr3OJJSONException = array of Arr2OJJSONException;

  OAHICNetscapeDraftSpecFactory = class;
  Arr1OAHICNetscapeDraftSpecFactory = array of OAHICNetscapeDraftSpecFactory;
  Arr2OAHICNetscapeDraftSpecFactory = array of Arr1OAHICNetscapeDraftSpecFactory;
  Arr3OAHICNetscapeDraftSpecFactory = array of Arr2OAHICNetscapeDraftSpecFactory;

  OACRangeBarChart = class;
  Arr1OACRangeBarChart = array of OACRangeBarChart;
  Arr2OACRangeBarChart = array of Arr1OACRangeBarChart;
  Arr3OACRangeBarChart = array of Arr2OACRangeBarChart;

  OAHIDefaultHttpRequestFactory = class;
  Arr1OAHIDefaultHttpRequestFactory = array of OAHIDefaultHttpRequestFactory;
  Arr2OAHIDefaultHttpRequestFactory = array of Arr1OAHIDefaultHttpRequestFactory;
  Arr3OAHIDefaultHttpRequestFactory = array of Arr2OAHIDefaultHttpRequestFactory;

  OAHCPConnConnectionPNames = interface;
  Arr1OAHCPConnConnectionPNames = array of OAHCPConnConnectionPNames;
  Arr2OAHCPConnConnectionPNames = array of Arr1OAHCPConnConnectionPNames;
  Arr3OAHCPConnConnectionPNames = array of Arr2OAHCPConnConnectionPNames;

  OAHHeader = interface;
  Arr1OAHHeader = array of OAHHeader;
  Arr2OAHHeader = array of Arr1OAHHeader;
  Arr3OAHHeader = array of Arr2OAHHeader;

  OAHPHttpRequestHandler = interface;
  Arr1OAHPHttpRequestHandler = array of OAHPHttpRequestHandler;
  Arr2OAHPHttpRequestHandler = array of Arr1OAHPHttpRequestHandler;
  Arr3OAHPHttpRequestHandler = array of Arr2OAHPHttpRequestHandler;

  OAHCPCookieSpecPNames = interface;
  Arr1OAHCPCookieSpecPNames = array of OAHCPCookieSpecPNames;
  Arr2OAHCPCookieSpecPNames = array of Arr1OAHCPCookieSpecPNames;
  Arr3OAHCPCookieSpecPNames = array of Arr2OAHCPCookieSpecPNames;

  OATPanListener = interface;
  Arr1OATPanListener = array of OATPanListener;
  Arr2OATPanListener = array of Arr1OATPanListener;
  Arr3OATPanListener = array of Arr2OATPanListener;

  OXSAttributeList = interface;
  Arr1OXSAttributeList = array of OXSAttributeList;
  Arr2OXSAttributeList = array of Arr1OXSAttributeList;
  Arr3OXSAttributeList = array of Arr2OXSAttributeList;

  OXSContentHandler = interface;
  Arr1OXSContentHandler = array of OXSContentHandler;
  Arr2OXSContentHandler = array of Arr1OXSContentHandler;
  Arr3OXSContentHandler = array of Arr2OXSContentHandler;

  OWDLLSInput = interface;
  Arr1OWDLLSInput = array of OWDLLSInput;
  Arr2OWDLLSInput = array of Arr1OWDLLSInput;
  Arr3OWDLLSInput = array of Arr2OWDLLSInput;

  OXVXmlPullParser = interface;
  Arr1OXVXmlPullParser = array of OXVXmlPullParser;
  Arr2OXVXmlPullParser = array of Arr1OXVXmlPullParser;
  Arr3OXVXmlPullParser = array of Arr2OXVXmlPullParser;

  OAHCPClientContext = interface;
  Arr1OAHCPClientContext = array of OAHCPClientContext;
  Arr2OAHCPClientContext = array of Arr1OAHCPClientContext;
  Arr3OAHCPClientContext = array of Arr2OAHCPClientContext;

  OWDLLSParserFilter = interface;
  Arr1OWDLLSParserFilter = array of OWDLLSParserFilter;
  Arr2OWDLLSParserFilter = array of Arr1OWDLLSParserFilter;
  Arr3OWDLLSParserFilter = array of Arr2OWDLLSParserFilter;

  OWDDOMImplementation = interface;
  Arr1OWDDOMImplementation = array of OWDDOMImplementation;
  Arr2OWDDOMImplementation = array of Arr1OWDDOMImplementation;
  Arr3OWDDOMImplementation = array of Arr2OWDDOMImplementation;

  OAHHttpEntity = interface;
  Arr1OAHHttpEntity = array of OAHHttpEntity;
  Arr2OAHHttpEntity = array of Arr1OAHHttpEntity;
  Arr3OAHHttpEntity = array of Arr2OAHHttpEntity;

  OAHISessionOutputBuffer = interface;
  Arr1OAHISessionOutputBuffer = array of OAHISessionOutputBuffer;
  Arr2OAHISessionOutputBuffer = array of Arr1OAHISessionOutputBuffer;
  Arr3OAHISessionOutputBuffer = array of Arr2OAHISessionOutputBuffer;

  OAHIHttpTransportMetrics = interface;
  Arr1OAHIHttpTransportMetrics = array of OAHIHttpTransportMetrics;
  Arr2OAHIHttpTransportMetrics = array of Arr1OAHIHttpTransportMetrics;
  Arr3OAHIHttpTransportMetrics = array of Arr2OAHIHttpTransportMetrics;

  OWDDOMLocator = interface;
  Arr1OWDDOMLocator = array of OWDDOMLocator;
  Arr2OWDDOMLocator = array of Arr1OWDDOMLocator;
  Arr3OWDDOMLocator = array of Arr2OWDDOMLocator;

  OAHCRRouteInfo = interface;
  Arr1OAHCRRouteInfo = array of OAHCRRouteInfo;
  Arr2OAHCRRouteInfo = array of Arr1OAHCRRouteInfo;
  Arr3OAHCRRouteInfo = array of Arr2OAHCRRouteInfo;

  OAHCPConnManagerPNames = interface;
  Arr1OAHCPConnManagerPNames = array of OAHCPConnManagerPNames;
  Arr2OAHCPConnManagerPNames = array of Arr1OAHCPConnManagerPNames;
  Arr3OAHCPConnManagerPNames = array of Arr2OAHCPConnManagerPNames;

  OAHCClientCookie = interface;
  Arr1OAHCClientCookie = array of OAHCClientCookie;
  Arr2OAHCClientCookie = array of Arr1OAHCClientCookie;
  Arr3OAHCClientCookie = array of Arr2OAHCClientCookie;

  OWDUserDataHandler = interface;
  Arr1OWDUserDataHandler = array of OWDUserDataHandler;
  Arr2OWDUserDataHandler = array of Arr1OWDUserDataHandler;
  Arr3OWDUserDataHandler = array of Arr2OWDUserDataHandler;

  OWDNamedNodeMap = interface;
  Arr1OWDNamedNodeMap = array of OWDNamedNodeMap;
  Arr2OWDNamedNodeMap = array of Arr1OWDNamedNodeMap;
  Arr3OWDNamedNodeMap = array of Arr2OWDNamedNodeMap;

  OAHMHeaderValueParser = interface;
  Arr1OAHMHeaderValueParser = array of OAHMHeaderValueParser;
  Arr2OAHMHeaderValueParser = array of Arr1OAHMHeaderValueParser;
  Arr3OAHMHeaderValueParser = array of Arr2OAHMHeaderValueParser;

  OWDCharacterData = interface;
  Arr1OWDCharacterData = array of OWDCharacterData;
  Arr2OWDCharacterData = array of Arr1OWDCharacterData;
  Arr3OWDCharacterData = array of Arr2OWDCharacterData;

  OAHEContentProducer = interface;
  Arr1OAHEContentProducer = array of OAHEContentProducer;
  Arr2OAHEContentProducer = array of Arr1OAHEContentProducer;
  Arr3OAHEContentProducer = array of Arr2OAHEContentProducer;

  OAHCConnectionKeepAliveStrategy = interface;
  Arr1OAHCConnectionKeepAliveStrategy = array of OAHCConnectionKeepAliveStrategy;
  Arr2OAHCConnectionKeepAliveStrategy = array of Arr1OAHCConnectionKeepAliveStrategy;
  Arr3OAHCConnectionKeepAliveStrategy = array of Arr2OAHCConnectionKeepAliveStrategy;

  OAHCSSocketFactory = interface;
  Arr1OAHCSSocketFactory = array of OAHCSSocketFactory;
  Arr2OAHCSSocketFactory = array of Arr1OAHCSSocketFactory;
  Arr3OAHCSSocketFactory = array of Arr2OAHCSSocketFactory;

  OAHPHttpRequestHandlerResolver = interface;
  Arr1OAHPHttpRequestHandlerResolver = array of OAHPHttpRequestHandlerResolver;
  Arr2OAHPHttpRequestHandlerResolver = array of Arr1OAHPHttpRequestHandlerResolver;
  Arr3OAHPHttpRequestHandlerResolver = array of Arr2OAHPHttpRequestHandlerResolver;

  OWDNameList = interface;
  Arr1OWDNameList = array of OWDNameList;
  Arr2OWDNameList = array of Arr1OWDNameList;
  Arr3OWDNameList = array of Arr2OWDNameList;

  OAHAAuthSchemeFactory = interface;
  Arr1OAHAAuthSchemeFactory = array of OAHAAuthSchemeFactory;
  Arr2OAHAAuthSchemeFactory = array of Arr1OAHAAuthSchemeFactory;
  Arr3OAHAAuthSchemeFactory = array of Arr2OAHAAuthSchemeFactory;

  OAHCCookieSpecFactory = interface;
  Arr1OAHCCookieSpecFactory = array of OAHCCookieSpecFactory;
  Arr2OAHCCookieSpecFactory = array of Arr1OAHCCookieSpecFactory;
  Arr3OAHCCookieSpecFactory = array of Arr2OAHCCookieSpecFactory;

  OAHCCookie = interface;
  Arr1OAHCCookie = array of OAHCCookie;
  Arr2OAHCCookie = array of Arr1OAHCCookie;
  Arr3OAHCCookie = array of Arr2OAHCCookie;

  OXSAttributes = interface;
  Arr1OXSAttributes = array of OXSAttributes;
  Arr2OXSAttributes = array of Arr1OXSAttributes;
  Arr3OXSAttributes = array of Arr2OXSAttributes;

  OWDElement = interface;
  Arr1OWDElement = array of OWDElement;
  Arr2OWDElement = array of Arr1OWDElement;
  Arr3OWDElement = array of Arr2OWDElement;

  OAHCSM = interface;
  Arr1OAHCSM = array of OAHCSM;
  Arr2OAHCSM = array of Arr1OAHCSM;
  Arr3OAHCSM = array of Arr2OAHCSM;

  OAHReasonPhraseCatalog = interface;
  Arr1OAHReasonPhraseCatalog = array of OAHReasonPhraseCatalog;
  Arr2OAHReasonPhraseCatalog = array of Arr1OAHReasonPhraseCatalog;
  Arr3OAHReasonPhraseCatalog = array of Arr2OAHReasonPhraseCatalog;

  OWDComment = interface;
  Arr1OWDComment = array of OWDComment;
  Arr2OWDComment = array of Arr1OWDComment;
  Arr3OWDComment = array of Arr2OWDComment;

  OXSErrorHandler = interface;
  Arr1OXSErrorHandler = array of OXSErrorHandler;
  Arr2OXSErrorHandler = array of Arr1OXSErrorHandler;
  Arr3OXSErrorHandler = array of Arr2OXSErrorHandler;

  OAHCSX509HostnameVerifier = interface;
  Arr1OAHCSX509HostnameVerifier = array of OAHCSX509HostnameVerifier;
  Arr2OAHCSX509HostnameVerifier = array of Arr1OAHCSX509HostnameVerifier;
  Arr3OAHCSX509HostnameVerifier = array of Arr2OAHCSX509HostnameVerifier;

  OAHCRHttpRoutePlanner = interface;
  Arr1OAHCRHttpRoutePlanner = array of OAHCRHttpRoutePlanner;
  Arr2OAHCRHttpRoutePlanner = array of Arr1OAHCRHttpRoutePlanner;
  Arr3OAHCRHttpRoutePlanner = array of Arr2OAHCRHttpRoutePlanner;

  OXSEDeclHandler = interface;
  Arr1OXSEDeclHandler = array of OXSEDeclHandler;
  Arr2OXSEDeclHandler = array of Arr1OXSEDeclHandler;
  Arr3OXSEDeclHandler = array of Arr2OXSEDeclHandler;

  OAHCPConnRoutePNames = interface;
  Arr1OAHCPConnRoutePNames = array of OAHCPConnRoutePNames;
  Arr2OAHCPConnRoutePNames = array of Arr1OAHCPConnRoutePNames;
  Arr3OAHCPConnRoutePNames = array of Arr2OAHCPConnRoutePNames;

  OWDNode = interface;
  Arr1OWDNode = array of OWDNode;
  Arr2OWDNode = array of Arr1OWDNode;
  Arr3OWDNode = array of Arr2OWDNode;

  OAHACredentials = interface;
  Arr1OAHACredentials = array of OAHACredentials;
  Arr2OAHACredentials = array of Arr1OAHACredentials;
  Arr3OAHACredentials = array of Arr2OAHACredentials;

  OWDEntityReference = interface;
  Arr1OWDEntityReference = array of OWDEntityReference;
  Arr2OWDEntityReference = array of Arr1OWDEntityReference;
  Arr3OWDEntityReference = array of Arr2OWDEntityReference;

  OAHIANTLMEngine = interface;
  Arr1OAHIANTLMEngine = array of OAHIANTLMEngine;
  Arr2OAHIANTLMEngine = array of Arr1OAHIANTLMEngine;
  Arr3OAHIANTLMEngine = array of Arr2OAHIANTLMEngine;

  OAHHttpEntityEnclosingRequest = interface;
  Arr1OAHHttpEntityEnclosingRequest = array of OAHHttpEntityEnclosingRequest;
  Arr2OAHHttpEntityEnclosingRequest = array of Arr1OAHHttpEntityEnclosingRequest;
  Arr3OAHHttpEntityEnclosingRequest = array of Arr2OAHHttpEntityEnclosingRequest;

  OAHCRHttpRouteDirector = interface;
  Arr1OAHCRHttpRouteDirector = array of OAHCRHttpRouteDirector;
  Arr2OAHCRHttpRouteDirector = array of Arr1OAHCRHttpRouteDirector;
  Arr3OAHCRHttpRouteDirector = array of Arr2OAHCRHttpRouteDirector;

  OAHCHttpClient = interface;
  Arr1OAHCHttpClient = array of OAHCHttpClient;
  Arr2OAHCHttpClient = array of Arr1OAHCHttpClient;
  Arr3OAHCHttpClient = array of Arr2OAHCHttpClient;

  OAHCMAbortableHttpRequest = interface;
  Arr1OAHCMAbortableHttpRequest = array of OAHCMAbortableHttpRequest;
  Arr2OAHCMAbortableHttpRequest = array of Arr1OAHCMAbortableHttpRequest;
  Arr3OAHCMAbortableHttpRequest = array of Arr2OAHCMAbortableHttpRequest;

  OWDTypeInfo = interface;
  Arr1OWDTypeInfo = array of OWDTypeInfo;
  Arr2OWDTypeInfo = array of Arr1OWDTypeInfo;
  Arr3OWDTypeInfo = array of Arr2OWDTypeInfo;

  OAHHttpRequestInterceptor = interface;
  Arr1OAHHttpRequestInterceptor = array of OAHHttpRequestInterceptor;
  Arr2OAHHttpRequestInterceptor = array of Arr1OAHHttpRequestInterceptor;
  Arr3OAHHttpRequestInterceptor = array of Arr2OAHHttpRequestInterceptor;

  OWDCDATASection = interface;
  Arr1OWDCDATASection = array of OWDCDATASection;
  Arr2OWDCDATASection = array of Arr1OWDCDATASection;
  Arr3OWDCDATASection = array of Arr2OWDCDATASection;

  OAHPExecutionContext = interface;
  Arr1OAHPExecutionContext = array of OAHPExecutionContext;
  Arr2OAHPExecutionContext = array of Arr1OAHPExecutionContext;
  Arr3OAHPExecutionContext = array of Arr2OAHPExecutionContext;

  OWDDocumentFragment = interface;
  Arr1OWDDocumentFragment = array of OWDDocumentFragment;
  Arr2OWDDocumentFragment = array of Arr1OWDDocumentFragment;
  Arr3OWDDocumentFragment = array of Arr2OWDDocumentFragment;

  OAHCClientConnectionManagerFactory = interface;
  Arr1OAHCClientConnectionManagerFactory = array of OAHCClientConnectionManagerFactory;
  Arr2OAHCClientConnectionManagerFactory = array of Arr1OAHCClientConnectionManagerFactory;
  Arr3OAHCClientConnectionManagerFactory = array of Arr2OAHCClientConnectionManagerFactory;

  OAHHeaderElementIterator = interface;
  Arr1OAHHeaderElementIterator = array of OAHHeaderElementIterator;
  Arr2OAHHeaderElementIterator = array of Arr1OAHHeaderElementIterator;
  Arr3OAHHeaderElementIterator = array of Arr2OAHHeaderElementIterator;

  OAHCClientConnectionManager = interface;
  Arr1OAHCClientConnectionManager = array of OAHCClientConnectionManager;
  Arr2OAHCClientConnectionManager = array of Arr1OAHCClientConnectionManager;
  Arr3OAHCClientConnectionManager = array of Arr2OAHCClientConnectionManager;

  OAHHttpConnectionMetrics = interface;
  Arr1OAHHttpConnectionMetrics = array of OAHHttpConnectionMetrics;
  Arr2OAHHttpConnectionMetrics = array of Arr1OAHHttpConnectionMetrics;
  Arr3OAHHttpConnectionMetrics = array of Arr2OAHHttpConnectionMetrics;

  OAHIHttpMessageParser = interface;
  Arr1OAHIHttpMessageParser = array of OAHIHttpMessageParser;
  Arr2OAHIHttpMessageParser = array of Arr1OAHIHttpMessageParser;
  Arr3OAHIHttpMessageParser = array of Arr2OAHIHttpMessageParser;

  OAHHeaderElement = interface;
  Arr1OAHHeaderElement = array of OAHHeaderElement;
  Arr2OAHHeaderElement = array of Arr1OAHHeaderElement;
  Arr3OAHHeaderElement = array of Arr2OAHHeaderElement;

  OWDLLSParser = interface;
  Arr1OWDLLSParser = array of OWDLLSParser;
  Arr2OWDLLSParser = array of Arr1OWDLLSParser;
  Arr3OWDLLSParser = array of Arr2OWDLLSParser;

  OAHCRedirectHandler = interface;
  Arr1OAHCRedirectHandler = array of OAHCRedirectHandler;
  Arr2OAHCRedirectHandler = array of Arr1OAHCRedirectHandler;
  Arr3OAHCRedirectHandler = array of Arr2OAHCRedirectHandler;

  OAHCSLayeredSocketFactory = interface;
  Arr1OAHCSLayeredSocketFactory = array of OAHCSLayeredSocketFactory;
  Arr2OAHCSLayeredSocketFactory = array of Arr1OAHCSLayeredSocketFactory;
  Arr3OAHCSLayeredSocketFactory = array of Arr2OAHCSLayeredSocketFactory;

  OAHTokenIterator = interface;
  Arr1OAHTokenIterator = array of OAHTokenIterator;
  Arr2OAHTokenIterator = array of Arr1OAHTokenIterator;
  Arr3OAHTokenIterator = array of Arr2OAHTokenIterator;

  OAHHttpServerConnection = interface;
  Arr1OAHHttpServerConnection = array of OAHHttpServerConnection;
  Arr2OAHHttpServerConnection = array of Arr1OAHHttpServerConnection;
  Arr3OAHHttpServerConnection = array of Arr2OAHHttpServerConnection;

  OAHAPAuthPNames = interface;
  Arr1OAHAPAuthPNames = array of OAHAPAuthPNames;
  Arr2OAHAPAuthPNames = array of Arr1OAHAPAuthPNames;
  Arr3OAHAPAuthPNames = array of Arr2OAHAPAuthPNames;

  OAHCPAllClientPNames = interface;
  Arr1OAHCPAllClientPNames = array of OAHCPAllClientPNames;
  Arr2OAHCPAllClientPNames = array of Arr1OAHCPAllClientPNames;
  Arr3OAHCPAllClientPNames = array of Arr2OAHCPAllClientPNames;

  OAHCRequestDirector = interface;
  Arr1OAHCRequestDirector = array of OAHCRequestDirector;
  Arr2OAHCRequestDirector = array of Arr1OAHCRequestDirector;
  Arr3OAHCRequestDirector = array of Arr2OAHCRequestDirector;

  OAHMHeaderValueFormatter = interface;
  Arr1OAHMHeaderValueFormatter = array of OAHMHeaderValueFormatter;
  Arr2OAHMHeaderValueFormatter = array of Arr1OAHMHeaderValueFormatter;
  Arr3OAHMHeaderValueFormatter = array of Arr2OAHMHeaderValueFormatter;

  OWDNodeList = interface;
  Arr1OWDNodeList = array of OWDNodeList;
  Arr2OWDNodeList = array of Arr1OWDNodeList;
  Arr3OWDNodeList = array of Arr2OWDNodeList;

  OAHHttpClientConnection = interface;
  Arr1OAHHttpClientConnection = array of OAHHttpClientConnection;
  Arr2OAHHttpClientConnection = array of Arr1OAHHttpClientConnection;
  Arr3OAHHttpClientConnection = array of Arr2OAHHttpClientConnection;

  OAHCUserTokenHandler = interface;
  Arr1OAHCUserTokenHandler = array of OAHCUserTokenHandler;
  Arr2OAHCUserTokenHandler = array of Arr1OAHCUserTokenHandler;
  Arr3OAHCUserTokenHandler = array of Arr2OAHCUserTokenHandler;

  OAHIHttpMessageWriter = interface;
  Arr1OAHIHttpMessageWriter = array of OAHIHttpMessageWriter;
  Arr2OAHIHttpMessageWriter = array of Arr1OAHIHttpMessageWriter;
  Arr3OAHIHttpMessageWriter = array of Arr2OAHIHttpMessageWriter;

  OXSXMLFilter = interface;
  Arr1OXSXMLFilter = array of OXSXMLFilter;
  Arr2OXSXMLFilter = array of Arr1OXSXMLFilter;
  Arr3OXSXMLFilter = array of Arr2OXSXMLFilter;

  OAHCHttpRequestRetryHandler = interface;
  Arr1OAHCHttpRequestRetryHandler = array of OAHCHttpRequestRetryHandler;
  Arr2OAHCHttpRequestRetryHandler = array of Arr1OAHCHttpRequestRetryHandler;
  Arr3OAHCHttpRequestRetryHandler = array of Arr2OAHCHttpRequestRetryHandler;

  OAHPHttpExpectationVerifier = interface;
  Arr1OAHPHttpExpectationVerifier = array of OAHPHttpExpectationVerifier;
  Arr2OAHPHttpExpectationVerifier = array of Arr1OAHPHttpExpectationVerifier;
  Arr3OAHPHttpExpectationVerifier = array of Arr2OAHPHttpExpectationVerifier;

  OAHAAuthScheme = interface;
  Arr1OAHAAuthScheme = array of OAHAAuthScheme;
  Arr2OAHAAuthScheme = array of Arr1OAHAAuthScheme;
  Arr3OAHAAuthScheme = array of Arr2OAHAAuthScheme;

  OAHConnectionReuseStrategy = interface;
  Arr1OAHConnectionReuseStrategy = array of OAHConnectionReuseStrategy;
  Arr2OAHConnectionReuseStrategy = array of Arr1OAHConnectionReuseStrategy;
  Arr3OAHConnectionReuseStrategy = array of Arr2OAHConnectionReuseStrategy;

  OAHCCookieStore = interface;
  Arr1OAHCCookieStore = array of OAHCCookieStore;
  Arr2OAHCCookieStore = array of Arr1OAHCCookieStore;
  Arr3OAHCCookieStore = array of Arr2OAHCCookieStore;

  OAHPCoreProtocolPNames = interface;
  Arr1OAHPCoreProtocolPNames = array of OAHPCoreProtocolPNames;
  Arr2OAHPCoreProtocolPNames = array of Arr1OAHPCoreProtocolPNames;
  Arr3OAHPCoreProtocolPNames = array of Arr2OAHPCoreProtocolPNames;

  OWDDOMStringList = interface;
  Arr1OWDDOMStringList = array of OWDDOMStringList;
  Arr2OWDDOMStringList = array of Arr1OWDDOMStringList;
  Arr3OWDDOMStringList = array of Arr2OWDDOMStringList;

  OAHRequestLine = interface;
  Arr1OAHRequestLine = array of OAHRequestLine;
  Arr2OAHRequestLine = array of Arr1OAHRequestLine;
  Arr3OAHRequestLine = array of Arr2OAHRequestLine;

  OWDText = interface;
  Arr1OWDText = array of OWDText;
  Arr2OWDText = array of Arr1OWDText;
  Arr3OWDText = array of Arr2OWDText;

  OWDEntity = interface;
  Arr1OWDEntity = array of OWDEntity;
  Arr2OWDEntity = array of Arr1OWDEntity;
  Arr3OWDEntity = array of Arr2OWDEntity;

  OAHPHttpParams = interface;
  Arr1OAHPHttpParams = array of OAHPHttpParams;
  Arr2OAHPHttpParams = array of Arr1OAHPHttpParams;
  Arr3OAHPHttpParams = array of Arr2OAHPHttpParams;

  OXSDocumentHandler = interface;
  Arr1OXSDocumentHandler = array of OXSDocumentHandler;
  Arr2OXSDocumentHandler = array of Arr1OXSDocumentHandler;
  Arr3OXSDocumentHandler = array of Arr2OXSDocumentHandler;

  OAHHttpStatus = interface;
  Arr1OAHHttpStatus = array of OAHHttpStatus;
  Arr2OAHHttpStatus = array of Arr1OAHHttpStatus;
  Arr3OAHHttpStatus = array of Arr2OAHHttpStatus;

  OAHHttpRequest = interface;
  Arr1OAHHttpRequest = array of OAHHttpRequest;
  Arr2OAHHttpRequest = array of Arr1OAHHttpRequest;
  Arr3OAHHttpRequest = array of Arr2OAHHttpRequest;

  OAHCClientConnectionOperator = interface;
  Arr1OAHCClientConnectionOperator = array of OAHCClientConnectionOperator;
  Arr2OAHCClientConnectionOperator = array of Arr1OAHCClientConnectionOperator;
  Arr3OAHCClientConnectionOperator = array of Arr2OAHCClientConnectionOperator;

  OAHICTRefQueueHandler = interface;
  Arr1OAHICTRefQueueHandler = array of OAHICTRefQueueHandler;
  Arr2OAHICTRefQueueHandler = array of Arr1OAHICTRefQueueHandler;
  Arr3OAHICTRefQueueHandler = array of Arr2OAHICTRefQueueHandler;

  OAHEContentLengthStrategy = interface;
  Arr1OAHEContentLengthStrategy = array of OAHEContentLengthStrategy;
  Arr2OAHEContentLengthStrategy = array of Arr1OAHEContentLengthStrategy;
  Arr3OAHEContentLengthStrategy = array of Arr2OAHEContentLengthStrategy;

  OAHMLineParser = interface;
  Arr1OAHMLineParser = array of OAHMLineParser;
  Arr2OAHMLineParser = array of Arr1OAHMLineParser;
  Arr3OAHMLineParser = array of Arr2OAHMLineParser;

  OAHHttpRequestFactory = interface;
  Arr1OAHHttpRequestFactory = array of OAHHttpRequestFactory;
  Arr2OAHHttpRequestFactory = array of Arr1OAHHttpRequestFactory;
  Arr3OAHHttpRequestFactory = array of Arr2OAHHttpRequestFactory;

  OAHCAuthenticationHandler = interface;
  Arr1OAHCAuthenticationHandler = array of OAHCAuthenticationHandler;
  Arr2OAHCAuthenticationHandler = array of Arr1OAHCAuthenticationHandler;
  Arr3OAHCAuthenticationHandler = array of Arr2OAHCAuthenticationHandler;

  OAHCOperatedClientConnection = interface;
  Arr1OAHCOperatedClientConnection = array of OAHCOperatedClientConnection;
  Arr2OAHCOperatedClientConnection = array of Arr1OAHCOperatedClientConnection;
  Arr3OAHCOperatedClientConnection = array of Arr2OAHCOperatedClientConnection;

  OAHCConnectionReleaseTrigger = interface;
  Arr1OAHCConnectionReleaseTrigger = array of OAHCConnectionReleaseTrigger;
  Arr2OAHCConnectionReleaseTrigger = array of Arr1OAHCConnectionReleaseTrigger;
  Arr3OAHCConnectionReleaseTrigger = array of Arr2OAHCConnectionReleaseTrigger;

  OAHFormattedHeader = interface;
  Arr1OAHFormattedHeader = array of OAHFormattedHeader;
  Arr2OAHFormattedHeader = array of Arr1OAHFormattedHeader;
  Arr3OAHFormattedHeader = array of Arr2OAHFormattedHeader;

  OAHHeaderIterator = interface;
  Arr1OAHHeaderIterator = array of OAHHeaderIterator;
  Arr2OAHHeaderIterator = array of Arr1OAHHeaderIterator;
  Arr3OAHHeaderIterator = array of Arr2OAHHeaderIterator;

  OWDDOMImplementationSource = interface;
  Arr1OWDDOMImplementationSource = array of OWDDOMImplementationSource;
  Arr2OWDDOMImplementationSource = array of Arr1OWDDOMImplementationSource;
  Arr3OWDDOMImplementationSource = array of Arr2OWDDOMImplementationSource;

  OXSLocator = interface;
  Arr1OXSLocator = array of OXSLocator;
  Arr2OXSLocator = array of Arr1OXSLocator;
  Arr3OXSLocator = array of Arr2OXSLocator;

  OWDLLSOutput = interface;
  Arr1OWDLLSOutput = array of OWDLLSOutput;
  Arr2OWDLLSOutput = array of Arr1OWDLLSOutput;
  Arr3OWDLLSOutput = array of Arr2OWDLLSOutput;

  OXSEAttributes2 = interface;
  Arr1OXSEAttributes2 = array of OXSEAttributes2;
  Arr2OXSEAttributes2 = array of Arr1OXSEAttributes2;
  Arr3OXSEAttributes2 = array of Arr2OXSEAttributes2;

  OAHCCookieSpec = interface;
  Arr1OAHCCookieSpec = array of OAHCCookieSpec;
  Arr2OAHCCookieSpec = array of Arr1OAHCCookieSpec;
  Arr3OAHCCookieSpec = array of Arr2OAHCCookieSpec;

  OXSEntityResolver = interface;
  Arr1OXSEntityResolver = array of OXSEntityResolver;
  Arr2OXSEntityResolver = array of Arr1OXSEntityResolver;
  Arr3OXSEntityResolver = array of Arr2OXSEntityResolver;

  OAHCPConnPerRoute = interface;
  Arr1OAHCPConnPerRoute = array of OAHCPConnPerRoute;
  Arr2OAHCPConnPerRoute = array of Arr1OAHCPConnPerRoute;
  Arr3OAHCPConnPerRoute = array of Arr2OAHCPConnPerRoute;

  OAHHttpConnection = interface;
  Arr1OAHHttpConnection = array of OAHHttpConnection;
  Arr2OAHHttpConnection = array of Arr1OAHHttpConnection;
  Arr3OAHHttpConnection = array of Arr2OAHHttpConnection;

  OWDLDOMImplementationLS = interface;
  Arr1OWDLDOMImplementationLS = array of OWDLDOMImplementationLS;
  Arr2OWDLDOMImplementationLS = array of Arr1OWDLDOMImplementationLS;
  Arr3OWDLDOMImplementationLS = array of Arr2OWDLDOMImplementationLS;

  OAHPHttpContext = interface;
  Arr1OAHPHttpContext = array of OAHPHttpContext;
  Arr2OAHPHttpContext = array of Arr1OAHPHttpContext;
  Arr3OAHPHttpContext = array of Arr2OAHPHttpContext;

  OAHCSetCookie2 = interface;
  Arr1OAHCSetCookie2 = array of OAHCSetCookie2;
  Arr2OAHCSetCookie2 = array of Arr1OAHCSetCookie2;
  Arr3OAHCSetCookie2 = array of Arr2OAHCSetCookie2;

  OAHMLineFormatter = interface;
  Arr1OAHMLineFormatter = array of OAHMLineFormatter;
  Arr2OAHMLineFormatter = array of Arr1OAHMLineFormatter;
  Arr3OAHMLineFormatter = array of Arr2OAHMLineFormatter;

  OAHICTPoolEntryRequest = interface;
  Arr1OAHICTPoolEntryRequest = array of OAHICTPoolEntryRequest;
  Arr2OAHICTPoolEntryRequest = array of Arr1OAHICTPoolEntryRequest;
  Arr3OAHICTPoolEntryRequest = array of Arr2OAHICTPoolEntryRequest;

  OXSDTDHandler = interface;
  Arr1OXSDTDHandler = array of OXSDTDHandler;
  Arr2OXSDTDHandler = array of Arr1OXSDTDHandler;
  Arr3OXSDTDHandler = array of Arr2OXSDTDHandler;

  OXSEEntityResolver2 = interface;
  Arr1OXSEEntityResolver2 = array of OXSEEntityResolver2;
  Arr2OXSEEntityResolver2 = array of Arr1OXSEEntityResolver2;
  Arr3OXSEEntityResolver2 = array of Arr2OXSEEntityResolver2;

  OXVXmlSerializer = interface;
  Arr1OXVXmlSerializer = array of OXVXmlSerializer;
  Arr2OXVXmlSerializer = array of Arr1OXVXmlSerializer;
  Arr3OXVXmlSerializer = array of Arr2OXVXmlSerializer;

  OAHCCookieAttributeHandler = interface;
  Arr1OAHCCookieAttributeHandler = array of OAHCCookieAttributeHandler;
  Arr2OAHCCookieAttributeHandler = array of Arr1OAHCCookieAttributeHandler;
  Arr3OAHCCookieAttributeHandler = array of Arr2OAHCCookieAttributeHandler;

  OWDLLSResourceResolver = interface;
  Arr1OWDLLSResourceResolver = array of OWDLLSResourceResolver;
  Arr2OWDLLSResourceResolver = array of Arr1OWDLLSResourceResolver;
  Arr3OWDLLSResourceResolver = array of Arr2OWDLLSResourceResolver;

  OAHPHttpProcessor = interface;
  Arr1OAHPHttpProcessor = array of OAHPHttpProcessor;
  Arr2OAHPHttpProcessor = array of Arr1OAHPHttpProcessor;
  Arr3OAHPHttpProcessor = array of Arr2OAHPHttpProcessor;

  OWDDOMImplementationList = interface;
  Arr1OWDDOMImplementationList = array of OWDDOMImplementationList;
  Arr2OWDDOMImplementationList = array of Arr1OWDDOMImplementationList;
  Arr3OWDDOMImplementationList = array of Arr2OWDDOMImplementationList;

  OXSELocator2 = interface;
  Arr1OXSELocator2 = array of OXSELocator2;
  Arr2OXSELocator2 = array of Arr1OXSELocator2;
  Arr3OXSELocator2 = array of Arr2OXSELocator2;

  OXSParser = interface;
  Arr1OXSParser = array of OXSParser;
  Arr2OXSParser = array of Arr1OXSParser;
  Arr3OXSParser = array of Arr2OXSParser;

  OAHStatusLine = interface;
  Arr1OAHStatusLine = array of OAHStatusLine;
  Arr2OAHStatusLine = array of Arr1OAHStatusLine;
  Arr3OAHStatusLine = array of Arr2OAHStatusLine;

  OWDDocument = interface;
  Arr1OWDDocument = array of OWDDocument;
  Arr2OWDDocument = array of Arr1OWDDocument;
  Arr3OWDDocument = array of Arr2OWDDocument;

  OAHPHttpRequestInterceptorList = interface;
  Arr1OAHPHttpRequestInterceptorList = array of OAHPHttpRequestInterceptorList;
  Arr2OAHPHttpRequestInterceptorList = array of Arr1OAHPHttpRequestInterceptorList;
  Arr3OAHPHttpRequestInterceptorList = array of Arr2OAHPHttpRequestInterceptorList;

  OWDLLSSerializer = interface;
  Arr1OWDLLSSerializer = array of OWDLLSSerializer;
  Arr2OWDLLSSerializer = array of Arr1OWDLLSSerializer;
  Arr3OWDLLSSerializer = array of Arr2OWDLLSSerializer;

  OWDDocumentType = interface;
  Arr1OWDDocumentType = array of OWDDocumentType;
  Arr2OWDDocumentType = array of Arr1OWDDocumentType;
  Arr3OWDDocumentType = array of Arr2OWDDocumentType;

  OWDDOMError = interface;
  Arr1OWDDOMError = array of OWDDOMError;
  Arr2OWDDOMError = array of Arr1OWDDOMError;
  Arr3OWDDOMError = array of Arr2OWDDOMError;

  OAHCCredentialsProvider = interface;
  Arr1OAHCCredentialsProvider = array of OAHCCredentialsProvider;
  Arr2OAHCCredentialsProvider = array of Arr1OAHCCredentialsProvider;
  Arr3OAHCCredentialsProvider = array of Arr2OAHCCredentialsProvider;

  OXSELexicalHandler = interface;
  Arr1OXSELexicalHandler = array of OXSELexicalHandler;
  Arr2OXSELexicalHandler = array of Arr1OXSELexicalHandler;
  Arr3OXSELexicalHandler = array of Arr2OXSELexicalHandler;

  OWDDOMErrorHandler = interface;
  Arr1OWDDOMErrorHandler = array of OWDDOMErrorHandler;
  Arr2OWDDOMErrorHandler = array of Arr1OWDDOMErrorHandler;
  Arr3OWDDOMErrorHandler = array of Arr2OWDDOMErrorHandler;

  OAHISessionInputBuffer = interface;
  Arr1OAHISessionInputBuffer = array of OAHISessionInputBuffer;
  Arr2OAHISessionInputBuffer = array of Arr1OAHISessionInputBuffer;
  Arr3OAHISessionInputBuffer = array of Arr2OAHISessionInputBuffer;

  OAHHttpInetConnection = interface;
  Arr1OAHHttpInetConnection = array of OAHHttpInetConnection;
  Arr2OAHHttpInetConnection = array of Arr1OAHHttpInetConnection;
  Arr3OAHHttpInetConnection = array of Arr2OAHHttpInetConnection;

  OWDDOMConfiguration = interface;
  Arr1OWDDOMConfiguration = array of OWDDOMConfiguration;
  Arr2OWDDOMConfiguration = array of Arr1OWDDOMConfiguration;
  Arr3OWDDOMConfiguration = array of Arr2OWDDOMConfiguration;

  OAHCPClientPNames = interface;
  Arr1OAHCPClientPNames = array of OAHCPClientPNames;
  Arr2OAHCPClientPNames = array of Arr1OAHCPClientPNames;
  Arr3OAHCPClientPNames = array of Arr2OAHCPClientPNames;

  OATZoomListener = interface;
  Arr1OATZoomListener = array of OATZoomListener;
  Arr2OATZoomListener = array of Arr1OATZoomListener;
  Arr3OATZoomListener = array of Arr2OATZoomListener;

  OAHCSetCookie = interface;
  Arr1OAHCSetCookie = array of OAHCSetCookie;
  Arr2OAHCSetCookie = array of Arr1OAHCSetCookie;
  Arr3OAHCSetCookie = array of Arr2OAHCSetCookie;

  OWDProcessingInstruction = interface;
  Arr1OWDProcessingInstruction = array of OWDProcessingInstruction;
  Arr2OWDProcessingInstruction = array of Arr1OWDProcessingInstruction;
  Arr3OWDProcessingInstruction = array of Arr2OWDProcessingInstruction;

  OAHCEofSensorWatcher = interface;
  Arr1OAHCEofSensorWatcher = array of OAHCEofSensorWatcher;
  Arr2OAHCEofSensorWatcher = array of Arr1OAHCEofSensorWatcher;
  Arr3OAHCEofSensorWatcher = array of Arr2OAHCEofSensorWatcher;

  OAHHttpMessage = interface;
  Arr1OAHHttpMessage = array of OAHHttpMessage;
  Arr2OAHHttpMessage = array of Arr1OAHHttpMessage;
  Arr3OAHHttpMessage = array of Arr2OAHHttpMessage;

  OAHPCoreConnectionPNames = interface;
  Arr1OAHPCoreConnectionPNames = array of OAHPCoreConnectionPNames;
  Arr2OAHPCoreConnectionPNames = array of Arr1OAHPCoreConnectionPNames;
  Arr3OAHPCoreConnectionPNames = array of Arr2OAHPCoreConnectionPNames;

  OWDAttr = interface;
  Arr1OWDAttr = array of OWDAttr;
  Arr2OWDAttr = array of Arr1OWDAttr;
  Arr3OWDAttr = array of Arr2OWDAttr;

  OAHHttpResponseInterceptor = interface;
  Arr1OAHHttpResponseInterceptor = array of OAHHttpResponseInterceptor;
  Arr2OAHHttpResponseInterceptor = array of Arr1OAHHttpResponseInterceptor;
  Arr3OAHHttpResponseInterceptor = array of Arr2OAHHttpResponseInterceptor;

  OAITouchHandler = interface;
  Arr1OAITouchHandler = array of OAITouchHandler;
  Arr2OAITouchHandler = array of Arr1OAITouchHandler;
  Arr3OAITouchHandler = array of Arr2OAITouchHandler;

  OXSXMLReader = interface;
  Arr1OXSXMLReader = array of OXSXMLReader;
  Arr2OXSXMLReader = array of Arr1OXSXMLReader;
  Arr3OXSXMLReader = array of Arr2OXSXMLReader;

  OAHCResponseHandler = interface;
  Arr1OAHCResponseHandler = array of OAHCResponseHandler;
  Arr2OAHCResponseHandler = array of Arr1OAHCResponseHandler;
  Arr3OAHCResponseHandler = array of Arr2OAHCResponseHandler;

  OAHPHttpResponseInterceptorList = interface;
  Arr1OAHPHttpResponseInterceptorList = array of OAHPHttpResponseInterceptorList;
  Arr2OAHPHttpResponseInterceptorList = array of Arr1OAHPHttpResponseInterceptorList;
  Arr3OAHPHttpResponseInterceptorList = array of Arr2OAHPHttpResponseInterceptorList;

  OACLLog = interface;
  Arr1OACLLog = array of OACLLog;
  Arr2OACLLog = array of Arr1OACLLog;
  Arr3OACLLog = array of Arr2OACLLog;

  OWDNotation = interface;
  Arr1OWDNotation = array of OWDNotation;
  Arr2OWDNotation = array of Arr1OWDNotation;
  Arr3OWDNotation = array of Arr2OWDNotation;

  OAHCSHostNameResolver = interface;
  Arr1OAHCSHostNameResolver = array of OAHCSHostNameResolver;
  Arr2OAHCSHostNameResolver = array of Arr1OAHCSHostNameResolver;
  Arr3OAHCSHostNameResolver = array of Arr2OAHCSHostNameResolver;

  OAHCClientConnectionRequest = interface;
  Arr1OAHCClientConnectionRequest = array of OAHCClientConnectionRequest;
  Arr2OAHCClientConnectionRequest = array of Arr1OAHCClientConnectionRequest;
  Arr3OAHCClientConnectionRequest = array of Arr2OAHCClientConnectionRequest;

  OAHCMHttpUriRequest = interface;
  Arr1OAHCMHttpUriRequest = array of OAHCMHttpUriRequest;
  Arr2OAHCMHttpUriRequest = array of Arr1OAHCMHttpUriRequest;
  Arr3OAHCMHttpUriRequest = array of Arr2OAHCMHttpUriRequest;

  OAHHttpResponseFactory = interface;
  Arr1OAHHttpResponseFactory = array of OAHHttpResponseFactory;
  Arr2OAHHttpResponseFactory = array of Arr1OAHHttpResponseFactory;
  Arr3OAHHttpResponseFactory = array of Arr2OAHHttpResponseFactory;

  OAHNameValuePair = interface;
  Arr1OAHNameValuePair = array of OAHNameValuePair;
  Arr2OAHNameValuePair = array of Arr1OAHNameValuePair;
  Arr3OAHNameValuePair = array of Arr2OAHNameValuePair;

  OAHHttpResponse = interface;
  Arr1OAHHttpResponse = array of OAHHttpResponse;
  Arr2OAHHttpResponse = array of Arr1OAHHttpResponse;
  Arr3OAHHttpResponse = array of Arr2OAHHttpResponse;

  OAHCManagedClientConnection = interface;
  Arr1OAHCManagedClientConnection = array of OAHCManagedClientConnection;
  Arr2OAHCManagedClientConnection = array of Arr1OAHCManagedClientConnection;
  Arr3OAHCManagedClientConnection = array of Arr2OAHCManagedClientConnection;

  JUCTimeUnit = class external 'java.util.concurrent' name 'TimeUnit';
  Arr1JUCTimeUnit = array of JUCTimeUnit;
  Arr2JUCTimeUnit = array of Arr1JUCTimeUnit;
  Arr3JUCTimeUnit = array of Arr2JUCTimeUnit;

  JSCX509Certificate = class external 'java.security.cert' name 'X509Certificate';
  Arr1JSCX509Certificate = array of JSCX509Certificate;
  Arr2JSCX509Certificate = array of Arr1JSCX509Certificate;
  Arr3JSCX509Certificate = array of Arr2JSCX509Certificate;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JLThread = class external 'java.lang' name 'Thread';
  Arr1JLThread = array of JLThread;
  Arr2JLThread = array of Arr1JLThread;
  Arr3JLThread = array of Arr2JLThread;

  ACIntent = class external 'android.content' name 'Intent';
  Arr1ACIntent = array of ACIntent;
  Arr2ACIntent = array of Arr1ACIntent;
  Arr3ACIntent = array of Arr2ACIntent;

  JIInputStream = class external 'java.io' name 'InputStream';
  Arr1JIInputStream = array of JIInputStream;
  Arr2JIInputStream = array of Arr1JIInputStream;
  Arr3JIInputStream = array of Arr2JIInputStream;

  JIWriter = class external 'java.io' name 'Writer';
  Arr1JIWriter = array of JIWriter;
  Arr2JIWriter = array of Arr1JIWriter;
  Arr3JIWriter = array of Arr2JIWriter;

  JSKeyStore = class external 'java.security' name 'KeyStore';
  Arr1JSKeyStore = array of JSKeyStore;
  Arr2JSKeyStore = array of Arr1JSKeyStore;
  Arr3JSKeyStore = array of Arr2JSKeyStore;

  JNSocket = class external 'java.net' name 'Socket';
  Arr1JNSocket = array of JNSocket;
  Arr2JNSocket = array of Arr1JNSocket;
  Arr3JNSocket = array of Arr2JNSocket;

  JNInetSocketAddress = class external 'java.net' name 'InetSocketAddress';
  Arr1JNInetSocketAddress = array of JNInetSocketAddress;
  Arr2JNInetSocketAddress = array of Arr1JNInetSocketAddress;
  Arr3JNInetSocketAddress = array of Arr2JNInetSocketAddress;

  AGRect = class external 'android.graphics' name 'Rect';
  Arr1AGRect = array of AGRect;
  Arr2AGRect = array of Arr1AGRect;
  Arr3AGRect = array of Arr2AGRect;

  AOBundle = class external 'android.os' name 'Bundle';
  Arr1AOBundle = array of AOBundle;
  Arr2AOBundle = array of Arr1AOBundle;
  Arr3AOBundle = array of Arr2AOBundle;

  JIReader = class external 'java.io' name 'Reader';
  Arr1JIReader = array of JIReader;
  Arr2JIReader = array of Arr1JIReader;
  Arr3JIReader = array of Arr2JIReader;

  AVMotionEvent = class external 'android.view' name 'MotionEvent';
  Arr1AVMotionEvent = array of AVMotionEvent;
  Arr2AVMotionEvent = array of Arr1AVMotionEvent;
  Arr3AVMotionEvent = array of Arr2AVMotionEvent;

  JLRReference = class external 'java.lang.ref' name 'Reference';
  Arr1JLRReference = array of JLRReference;
  Arr2JLRReference = array of Arr1JLRReference;
  Arr3JLRReference = array of Arr2JLRReference;

  JNConnectException = class external 'java.net' name 'ConnectException';
  Arr1JNConnectException = array of JNConnectException;
  Arr2JNConnectException = array of Arr1JNConnectException;
  Arr3JNConnectException = array of Arr2JNConnectException;

  JIFile = class external 'java.io' name 'File';
  Arr1JIFile = array of JIFile;
  Arr2JIFile = array of Arr1JIFile;
  Arr3JIFile = array of Arr2JIFile;

  JULinkedList = class external 'java.util' name 'LinkedList';
  Arr1JULinkedList = array of JULinkedList;
  Arr2JULinkedList = array of Arr1JULinkedList;
  Arr3JULinkedList = array of Arr2JULinkedList;

  AVView = class external 'android.view' name 'View';
  Arr1AVView = array of AVView;
  Arr2AVView = array of Arr1AVView;
  Arr3AVView = array of Arr2AVView;

  JUTreeMap = class external 'java.util' name 'TreeMap';
  Arr1JUTreeMap = array of JUTreeMap;
  Arr2JUTreeMap = array of Arr1JUTreeMap;
  Arr3JUTreeMap = array of Arr2JUTreeMap;

  JNInetAddress = class external 'java.net' name 'InetAddress';
  Arr1JNInetAddress = array of JNInetAddress;
  Arr2JNInetAddress = array of Arr1JNInetAddress;
  Arr3JNInetAddress = array of Arr2JNInetAddress;

  JNProxySelector = class external 'java.net' name 'ProxySelector';
  Arr1JNProxySelector = array of JNProxySelector;
  Arr2JNProxySelector = array of Arr1JNProxySelector;
  Arr3JNProxySelector = array of Arr2JNProxySelector;

  AGCanvas = class external 'android.graphics' name 'Canvas';
  Arr1AGCanvas = array of AGCanvas;
  Arr2AGCanvas = array of Arr1AGCanvas;
  Arr3AGCanvas = array of Arr2AGCanvas;

  JNProxy = class external 'java.net' name 'Proxy';
  Arr1JNProxy = array of JNProxy;
  Arr2JNProxy = array of Arr1JNProxy;
  Arr3JNProxy = array of Arr2JNProxy;

  JUHashMap = class external 'java.util' name 'HashMap';
  Arr1JUHashMap = array of JUHashMap;
  Arr2JUHashMap = array of Arr1JUHashMap;
  Arr3JUHashMap = array of Arr2JUHashMap;

  JIInterruptedIOException = class external 'java.io' name 'InterruptedIOException';
  Arr1JIInterruptedIOException = array of JIInterruptedIOException;
  Arr2JIInterruptedIOException = array of Arr1JIInterruptedIOException;
  Arr3JIInterruptedIOException = array of Arr2JIInterruptedIOException;

  JLDouble = class external 'java.lang' name 'Double';
  Arr1JLDouble = array of JLDouble;
  Arr2JLDouble = array of Arr1JLDouble;
  Arr3JLDouble = array of Arr2JLDouble;

  JLRWeakReference = class external 'java.lang.ref' name 'WeakReference';
  Arr1JLRWeakReference = array of JLRWeakReference;
  Arr2JLRWeakReference = array of Arr1JLRWeakReference;
  Arr3JLRWeakReference = array of Arr2JLRWeakReference;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  JLRuntimeException = class external 'java.lang' name 'RuntimeException';
  Arr1JLRuntimeException = array of JLRuntimeException;
  Arr2JLRuntimeException = array of Arr1JLRuntimeException;
  Arr3JLRuntimeException = array of Arr2JLRuntimeException;

  AGPaint = class external 'android.graphics' name 'Paint';
  Arr1AGPaint = array of AGPaint;
  Arr2AGPaint = array of Arr1AGPaint;
  Arr3AGPaint = array of Arr2AGPaint;

  JUTimeZone = class external 'java.util' name 'TimeZone';
  Arr1JUTimeZone = array of JUTimeZone;
  Arr2JUTimeZone = array of Arr1JUTimeZone;
  Arr3JUTimeZone = array of Arr2JUTimeZone;

  JUDate = class external 'java.util' name 'Date';
  Arr1JUDate = array of JUDate;
  Arr2JUDate = array of Arr1JUDate;
  Arr3JUDate = array of Arr2JUDate;

  JLClassLoader = class external 'java.lang' name 'ClassLoader';
  Arr1JLClassLoader = array of JLClassLoader;
  Arr2JLClassLoader = array of Arr1JLClassLoader;
  Arr3JLClassLoader = array of Arr2JLClassLoader;

  JUArrayList = class external 'java.util' name 'ArrayList';
  Arr1JUArrayList = array of JUArrayList;
  Arr2JUArrayList = array of Arr1JUArrayList;
  Arr3JUArrayList = array of Arr2JUArrayList;

  JLEnum = class external 'java.lang' name 'Enum';
  Arr1JLEnum = array of JLEnum;
  Arr2JLEnum = array of Arr1JLEnum;
  Arr3JLEnum = array of Arr2JLEnum;

  ACContext = class external 'android.content' name 'Context';
  Arr1ACContext = array of ACContext;
  Arr2ACContext = array of Arr1ACContext;
  Arr3ACContext = array of Arr2ACContext;

  JLNumber = class external 'java.lang' name 'Number';
  Arr1JLNumber = array of JLNumber;
  Arr2JLNumber = array of Arr1JLNumber;
  Arr3JLNumber = array of Arr2JLNumber;

  JIIOException = class external 'java.io' name 'IOException';
  Arr1JIIOException = array of JIIOException;
  Arr2JIIOException = array of Arr1JIIOException;
  Arr3JIIOException = array of Arr2JIIOException;

  AGRectF = class external 'android.graphics' name 'RectF';
  Arr1AGRectF = array of AGRectF;
  Arr2AGRectF = array of Arr1AGRectF;
  Arr3AGRectF = array of Arr2AGRectF;

  JSSecureRandom = class external 'java.security' name 'SecureRandom';
  Arr1JSSecureRandom = array of JSSecureRandom;
  Arr2JSSecureRandom = array of Arr1JSSecureRandom;
  Arr3JSSecureRandom = array of Arr2JSSecureRandom;

  AGBitmap = class external 'android.graphics' name 'Bitmap';
  Arr1AGBitmap = array of AGBitmap;
  Arr2AGBitmap = array of Arr1AGBitmap;
  Arr3AGBitmap = array of Arr2AGBitmap;

  JLBoolean = class external 'java.lang' name 'Boolean';
  Arr1JLBoolean = array of JLBoolean;
  Arr2JLBoolean = array of Arr1JLBoolean;
  Arr3JLBoolean = array of Arr2JLBoolean;

  JLRReferenceQueue = class external 'java.lang.ref' name 'ReferenceQueue';
  Arr1JLRReferenceQueue = array of JLRReferenceQueue;
  Arr2JLRReferenceQueue = array of Arr1JLRReferenceQueue;
  Arr3JLRReferenceQueue = array of Arr2JLRReferenceQueue;

  JIOutputStream = class external 'java.io' name 'OutputStream';
  Arr1JIOutputStream = array of JIOutputStream;
  Arr2JIOutputStream = array of Arr1JIOutputStream;
  Arr3JIOutputStream = array of Arr2JIOutputStream;

  AAActivity = class external 'android.app' name 'Activity';
  Arr1AAActivity = array of AAActivity;
  Arr2AAActivity = array of Arr1AAActivity;
  Arr3AAActivity = array of Arr2AAActivity;

  JLThrowable = class external 'java.lang' name 'Throwable';
  Arr1JLThrowable = array of JLThrowable;
  Arr2JLThrowable = array of Arr1JLThrowable;
  Arr3JLThrowable = array of Arr2JLThrowable;

  JNSSSLSocket = class external 'javax.net.ssl' name 'SSLSocket';
  Arr1JNSSSLSocket = array of JNSSSLSocket;
  Arr2JNSSSLSocket = array of Arr1JNSSSLSocket;
  Arr3JNSSSLSocket = array of Arr2JNSSSLSocket;

  JUScanner = class external 'java.util' name 'Scanner';
  Arr1JUScanner = array of JUScanner;
  Arr2JUScanner = array of Arr1JUScanner;
  Arr3JUScanner = array of Arr2JUScanner;

  JNURI = class external 'java.net' name 'URI';
  Arr1JNURI = array of JNURI;
  Arr2JNURI = array of Arr1JNURI;
  Arr3JNURI = array of Arr2JNURI;

  JLException = class external 'java.lang' name 'Exception';
  Arr1JLException = array of JLException;
  Arr2JLException = array of Arr1JLException;
  Arr3JLException = array of Arr2JLException;

  JLClass = class external 'java.lang' name 'Class';
  Arr1JLClass = array of JLClass;
  Arr2JLClass = array of Arr1JLClass;
  Arr3JLClass = array of Arr2JLClass;

  JULocale = class external 'java.util' name 'Locale';
  Arr1JULocale = array of JULocale;
  Arr2JULocale = array of Arr1JULocale;
  Arr3JULocale = array of Arr2JULocale;

  JUCLCondition = interface external 'java.util.concurrent.locks' name 'Condition';
  Arr1JUCLCondition = array of JUCLCondition;
  Arr2JUCLCondition = array of Arr1JUCLCondition;
  Arr3JUCLCondition = array of Arr2JUCLCondition;

  JUList = interface external 'java.util' name 'List';
  Arr1JUList = array of JUList;
  Arr2JUList = array of Arr1JUList;
  Arr3JUList = array of Arr2JUList;

  JSPrincipal = interface external 'java.security' name 'Principal';
  Arr1JSPrincipal = array of JSPrincipal;
  Arr2JSPrincipal = array of Arr1JSPrincipal;
  Arr3JSPrincipal = array of Arr2JSPrincipal;

  JUEnumeration = interface external 'java.util' name 'Enumeration';
  Arr1JUEnumeration = array of JUEnumeration;
  Arr2JUEnumeration = array of Arr1JUEnumeration;
  Arr3JUEnumeration = array of Arr2JUEnumeration;

  JLRunnable = interface external 'java.lang' name 'Runnable';
  Arr1JLRunnable = array of JLRunnable;
  Arr2JLRunnable = array of Arr1JLRunnable;
  Arr3JLRunnable = array of Arr2JLRunnable;

  JUIterator = interface external 'java.util' name 'Iterator';
  Arr1JUIterator = array of JUIterator;
  Arr2JUIterator = array of Arr1JUIterator;
  Arr3JUIterator = array of Arr2JUIterator;

  JLCloneable = interface external 'java.lang' name 'Cloneable';
  Arr1JLCloneable = array of JLCloneable;
  Arr2JLCloneable = array of Arr1JLCloneable;
  Arr3JLCloneable = array of Arr2JLCloneable;

  JUCollection = interface external 'java.util' name 'Collection';
  Arr1JUCollection = array of JUCollection;
  Arr2JUCollection = array of Arr1JUCollection;
  Arr3JUCollection = array of Arr2JUCollection;

  JUMap = interface external 'java.util' name 'Map';
  Arr1JUMap = array of JUMap;
  Arr2JUMap = array of Arr1JUMap;
  Arr3JUMap = array of Arr2JUMap;

  JUSet = interface external 'java.util' name 'Set';
  Arr1JUSet = array of JUSet;
  Arr2JUSet = array of Arr1JUSet;
  Arr3JUSet = array of Arr2JUSet;

  JUSortedMap = interface external 'java.util' name 'SortedMap';
  Arr1JUSortedMap = array of JUSortedMap;
  Arr2JUSortedMap = array of Arr1JUSortedMap;
  Arr3JUSortedMap = array of Arr2JUSortedMap;

  JUQueue = interface external 'java.util' name 'Queue';
  Arr1JUQueue = array of JUQueue;
  Arr2JUQueue = array of Arr1JUQueue;
  Arr3JUQueue = array of Arr2JUQueue;

  JUCLLock = interface external 'java.util.concurrent.locks' name 'Lock';
  Arr1JUCLLock = array of JUCLLock;
  Arr2JUCLLock = array of Arr1JUCLLock;
  Arr3JUCLLock = array of Arr2JUCLLock;

  JNSSSLSession = interface external 'javax.net.ssl' name 'SSLSession';
  Arr1JNSSSLSession = array of JNSSSLSession;
  Arr2JNSSSLSession = array of Arr1JNSSSLSession;
  Arr3JNSSSLSession = array of Arr2JNSSSLSession;

  JNSHostnameVerifier = interface external 'javax.net.ssl' name 'HostnameVerifier';
  Arr1JNSHostnameVerifier = array of JNSHostnameVerifier;
  Arr2JNSHostnameVerifier = array of Arr1JNSHostnameVerifier;
  Arr3JNSHostnameVerifier = array of Arr2JNSHostnameVerifier;

  JUComparator = interface external 'java.util' name 'Comparator';
  Arr1JUComparator = array of JUComparator;
  Arr2JUComparator = array of Arr1JUComparator;
  Arr3JUComparator = array of Arr2JUComparator;

  JISerializable = interface external 'java.io' name 'Serializable';
  Arr1JISerializable = array of JISerializable;
  Arr2JISerializable = array of Arr1JISerializable;
  Arr3JISerializable = array of Arr2JISerializable;


{$include chart.inc}

implementation

end.
