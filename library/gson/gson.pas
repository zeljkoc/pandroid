{ Imports for Java packages/classes: com.google.gson. }
unit gson;
{$mode delphi}

interface

type
  CGGJsonArray = class;
  Arr1CGGJsonArray = array of CGGJsonArray;
  Arr2CGGJsonArray = array of Arr1CGGJsonArray;
  Arr3CGGJsonArray = array of Arr2CGGJsonArray;

  CGGIPrimitives = class;
  Arr1CGGIPrimitives = array of CGGIPrimitives;
  Arr2CGGIPrimitives = array of Arr1CGGIPrimitives;
  Arr3CGGIPrimitives = array of Arr2CGGIPrimitives;

  CGGIBCollectionTypeAdapterFactory = class;
  Arr1CGGIBCollectionTypeAdapterFactory = array of CGGIBCollectionTypeAdapterFactory;
  Arr2CGGIBCollectionTypeAdapterFactory = array of Arr1CGGIBCollectionTypeAdapterFactory;
  Arr3CGGIBCollectionTypeAdapterFactory = array of Arr2CGGIBCollectionTypeAdapterFactory;

  CGGJsonParser = class;
  Arr1CGGJsonParser = array of CGGJsonParser;
  Arr2CGGJsonParser = array of Arr1CGGJsonParser;
  Arr3CGGJsonParser = array of Arr2CGGJsonParser;

  CGGIBJsonTreeReader = class;
  Arr1CGGIBJsonTreeReader = array of CGGIBJsonTreeReader;
  Arr2CGGIBJsonTreeReader = array of Arr1CGGIBJsonTreeReader;
  Arr3CGGIBJsonTreeReader = array of Arr2CGGIBJsonTreeReader;

  CGGIBTimeTypeAdapter = class;
  Arr1CGGIBTimeTypeAdapter = array of CGGIBTimeTypeAdapter;
  Arr2CGGIBTimeTypeAdapter = array of Arr1CGGIBTimeTypeAdapter;
  Arr3CGGIBTimeTypeAdapter = array of Arr2CGGIBTimeTypeAdapter;

  CGGSJsonReader = class;
  Arr1CGGSJsonReader = array of CGGSJsonReader;
  Arr2CGGSJsonReader = array of Arr1CGGSJsonReader;
  Arr3CGGSJsonReader = array of Arr2CGGSJsonReader;

  CGGTypeAdapter = class;
  Arr1CGGTypeAdapter = array of CGGTypeAdapter;
  Arr2CGGTypeAdapter = array of Arr1CGGTypeAdapter;
  Arr3CGGTypeAdapter = array of Arr2CGGTypeAdapter;

  CGGJsonIOException = class;
  Arr1CGGJsonIOException = array of CGGJsonIOException;
  Arr2CGGJsonIOException = array of Arr1CGGJsonIOException;
  Arr3CGGJsonIOException = array of Arr2CGGJsonIOException;

  CGGILinkedTreeMap = class;
  Arr1CGGILinkedTreeMap = array of CGGILinkedTreeMap;
  Arr2CGGILinkedTreeMap = array of Arr1CGGILinkedTreeMap;
  Arr3CGGILinkedTreeMap = array of Arr2CGGILinkedTreeMap;

  CGGIBTypeAdapters = class;
  Arr1CGGIBTypeAdapters = array of CGGIBTypeAdapters;
  Arr2CGGIBTypeAdapters = array of Arr1CGGIBTypeAdapters;
  Arr3CGGIBTypeAdapters = array of Arr2CGGIBTypeAdapters;

  CGGDefaultDateTypeAdapter = class;
  Arr1CGGDefaultDateTypeAdapter = array of CGGDefaultDateTypeAdapter;
  Arr2CGGDefaultDateTypeAdapter = array of Arr1CGGDefaultDateTypeAdapter;
  Arr3CGGDefaultDateTypeAdapter = array of Arr2CGGDefaultDateTypeAdapter;

  CGGIBReflectiveTypeAdapterFactory = class;
  Arr1CGGIBReflectiveTypeAdapterFactory = array of CGGIBReflectiveTypeAdapterFactory;
  Arr2CGGIBReflectiveTypeAdapterFactory = array of Arr1CGGIBReflectiveTypeAdapterFactory;
  Arr3CGGIBReflectiveTypeAdapterFactory = array of Arr2CGGIBReflectiveTypeAdapterFactory;

  CGGGson = class;
  Arr1CGGGson = array of CGGGson;
  Arr2CGGGson = array of Arr1CGGGson;
  Arr3CGGGson = array of Arr2CGGGson;

  CGGSJsonScope = class;
  Arr1CGGSJsonScope = array of CGGSJsonScope;
  Arr2CGGSJsonScope = array of Arr1CGGSJsonScope;
  Arr3CGGSJsonScope = array of Arr2CGGSJsonScope;

  CGGIExcluder = class;
  Arr1CGGIExcluder = array of CGGIExcluder;
  Arr2CGGIExcluder = array of Arr1CGGIExcluder;
  Arr3CGGIExcluder = array of Arr2CGGIExcluder;

  CGGIBJsonAdapterAnnotationTypeAdapterFactory = class;
  Arr1CGGIBJsonAdapterAnnotationTypeAdapterFactory = array of CGGIBJsonAdapterAnnotationTypeAdapterFactory;
  Arr2CGGIBJsonAdapterAnnotationTypeAdapterFactory = array of Arr1CGGIBJsonAdapterAnnotationTypeAdapterFactory;
  Arr3CGGIBJsonAdapterAnnotationTypeAdapterFactory = array of Arr2CGGIBJsonAdapterAnnotationTypeAdapterFactory;

  CGGIBArrayTypeAdapter = class;
  Arr1CGGIBArrayTypeAdapter = array of CGGIBArrayTypeAdapter;
  Arr2CGGIBArrayTypeAdapter = array of Arr1CGGIBArrayTypeAdapter;
  Arr3CGGIBArrayTypeAdapter = array of Arr2CGGIBArrayTypeAdapter;

  CGGIStreams = class;
  Arr1CGGIStreams = array of CGGIStreams;
  Arr2CGGIStreams = array of Arr1CGGIStreams;
  Arr3CGGIStreams = array of Arr2CGGIStreams;

  CGGJsonSyntaxException = class;
  Arr1CGGJsonSyntaxException = array of CGGJsonSyntaxException;
  Arr2CGGJsonSyntaxException = array of Arr1CGGJsonSyntaxException;
  Arr3CGGJsonSyntaxException = array of Arr2CGGJsonSyntaxException;

  CGGIBDateTypeAdapter = class;
  Arr1CGGIBDateTypeAdapter = array of CGGIBDateTypeAdapter;
  Arr2CGGIBDateTypeAdapter = array of Arr1CGGIBDateTypeAdapter;
  Arr3CGGIBDateTypeAdapter = array of Arr2CGGIBDateTypeAdapter;

  CGGIJsonReaderInternalAccess = class;
  Arr1CGGIJsonReaderInternalAccess = array of CGGIJsonReaderInternalAccess;
  Arr2CGGIJsonReaderInternalAccess = array of Arr1CGGIJsonReaderInternalAccess;
  Arr3CGGIJsonReaderInternalAccess = array of Arr2CGGIJsonReaderInternalAccess;

  CGGIUnsafeAllocator = class;
  Arr1CGGIUnsafeAllocator = array of CGGIUnsafeAllocator;
  Arr2CGGIUnsafeAllocator = array of Arr1CGGIUnsafeAllocator;
  Arr3CGGIUnsafeAllocator = array of Arr2CGGIUnsafeAllocator;

  CGGILazilyParsedNumber = class;
  Arr1CGGILazilyParsedNumber = array of CGGILazilyParsedNumber;
  Arr2CGGILazilyParsedNumber = array of Arr1CGGILazilyParsedNumber;
  Arr3CGGILazilyParsedNumber = array of Arr2CGGILazilyParsedNumber;

  CGGJsonStreamParser = class;
  Arr1CGGJsonStreamParser = array of CGGJsonStreamParser;
  Arr2CGGJsonStreamParser = array of Arr1CGGJsonStreamParser;
  Arr3CGGJsonStreamParser = array of Arr2CGGJsonStreamParser;

  CGGSJsonToken = class;
  Arr1CGGSJsonToken = array of CGGSJsonToken;
  Arr2CGGSJsonToken = array of Arr1CGGSJsonToken;
  Arr3CGGSJsonToken = array of Arr2CGGSJsonToken;

  CGGIBObjectTypeAdapter = class;
  Arr1CGGIBObjectTypeAdapter = array of CGGIBObjectTypeAdapter;
  Arr2CGGIBObjectTypeAdapter = array of Arr1CGGIBObjectTypeAdapter;
  Arr3CGGIBObjectTypeAdapter = array of Arr2CGGIBObjectTypeAdapter;

  CGGLongSerializationPolicy = class;
  Arr1CGGLongSerializationPolicy = array of CGGLongSerializationPolicy;
  Arr2CGGLongSerializationPolicy = array of Arr1CGGLongSerializationPolicy;
  Arr3CGGLongSerializationPolicy = array of Arr2CGGLongSerializationPolicy;

  CGGJsonNull = class;
  Arr1CGGJsonNull = array of CGGJsonNull;
  Arr2CGGJsonNull = array of Arr1CGGJsonNull;
  Arr3CGGJsonNull = array of Arr2CGGJsonNull;

  CGGJsonObject = class;
  Arr1CGGJsonObject = array of CGGJsonObject;
  Arr2CGGJsonObject = array of Arr1CGGJsonObject;
  Arr3CGGJsonObject = array of Arr2CGGJsonObject;

  CGGSJsonWriter = class;
  Arr1CGGSJsonWriter = array of CGGSJsonWriter;
  Arr2CGGSJsonWriter = array of Arr1CGGSJsonWriter;
  Arr3CGGSJsonWriter = array of Arr2CGGSJsonWriter;

  CGGJsonParseException = class;
  Arr1CGGJsonParseException = array of CGGJsonParseException;
  Arr2CGGJsonParseException = array of Arr1CGGJsonParseException;
  Arr3CGGJsonParseException = array of Arr2CGGJsonParseException;

  CGGRTypeToken = class;
  Arr1CGGRTypeToken = array of CGGRTypeToken;
  Arr2CGGRTypeToken = array of Arr1CGGRTypeToken;
  Arr3CGGRTypeToken = array of Arr2CGGRTypeToken;

  CGGSMalformedJsonException = class;
  Arr1CGGSMalformedJsonException = array of CGGSMalformedJsonException;
  Arr2CGGSMalformedJsonException = array of Arr1CGGSMalformedJsonException;
  Arr3CGGSMalformedJsonException = array of Arr2CGGSMalformedJsonException;

  CGGFieldNamingPolicy = class;
  Arr1CGGFieldNamingPolicy = array of CGGFieldNamingPolicy;
  Arr2CGGFieldNamingPolicy = array of Arr1CGGFieldNamingPolicy;
  Arr3CGGFieldNamingPolicy = array of Arr2CGGFieldNamingPolicy;

  CGGIBJsonTreeWriter = class;
  Arr1CGGIBJsonTreeWriter = array of CGGIBJsonTreeWriter;
  Arr2CGGIBJsonTreeWriter = array of Arr1CGGIBJsonTreeWriter;
  Arr3CGGIBJsonTreeWriter = array of Arr2CGGIBJsonTreeWriter;

  CGGGsonBuilder = class;
  Arr1CGGGsonBuilder = array of CGGGsonBuilder;
  Arr2CGGGsonBuilder = array of Arr1CGGGsonBuilder;
  Arr3CGGGsonBuilder = array of Arr2CGGGsonBuilder;

  CGGIConstructorConstructor = class;
  Arr1CGGIConstructorConstructor = array of CGGIConstructorConstructor;
  Arr2CGGIConstructorConstructor = array of Arr1CGGIConstructorConstructor;
  Arr3CGGIConstructorConstructor = array of Arr2CGGIConstructorConstructor;

  CGGIBMapTypeAdapterFactory = class;
  Arr1CGGIBMapTypeAdapterFactory = array of CGGIBMapTypeAdapterFactory;
  Arr2CGGIBMapTypeAdapterFactory = array of Arr1CGGIBMapTypeAdapterFactory;
  Arr3CGGIBMapTypeAdapterFactory = array of Arr2CGGIBMapTypeAdapterFactory;

  CGGFieldAttributes = class;
  Arr1CGGFieldAttributes = array of CGGFieldAttributes;
  Arr2CGGFieldAttributes = array of Arr1CGGFieldAttributes;
  Arr3CGGFieldAttributes = array of Arr2CGGFieldAttributes;

  CGGJsonPrimitive = class;
  Arr1CGGJsonPrimitive = array of CGGJsonPrimitive;
  Arr2CGGJsonPrimitive = array of Arr1CGGJsonPrimitive;
  Arr3CGGJsonPrimitive = array of Arr2CGGJsonPrimitive;

  CGGTreeTypeAdapter = class;
  Arr1CGGTreeTypeAdapter = array of CGGTreeTypeAdapter;
  Arr2CGGTreeTypeAdapter = array of Arr1CGGTreeTypeAdapter;
  Arr3CGGTreeTypeAdapter = array of Arr2CGGTreeTypeAdapter;

  CGGIBSqlDateTypeAdapter = class;
  Arr1CGGIBSqlDateTypeAdapter = array of CGGIBSqlDateTypeAdapter;
  Arr2CGGIBSqlDateTypeAdapter = array of Arr1CGGIBSqlDateTypeAdapter;
  Arr3CGGIBSqlDateTypeAdapter = array of Arr2CGGIBSqlDateTypeAdapter;

  CGGILinkedHashTreeMap = class;
  Arr1CGGILinkedHashTreeMap = array of CGGILinkedHashTreeMap;
  Arr2CGGILinkedHashTreeMap = array of Arr1CGGILinkedHashTreeMap;
  Arr3CGGILinkedHashTreeMap = array of Arr2CGGILinkedHashTreeMap;

  CGGIBTypeAdapterRuntimeTypeWrapper = class;
  Arr1CGGIBTypeAdapterRuntimeTypeWrapper = array of CGGIBTypeAdapterRuntimeTypeWrapper;
  Arr2CGGIBTypeAdapterRuntimeTypeWrapper = array of Arr1CGGIBTypeAdapterRuntimeTypeWrapper;
  Arr3CGGIBTypeAdapterRuntimeTypeWrapper = array of Arr2CGGIBTypeAdapterRuntimeTypeWrapper;

  CGGJsonElement = class;
  Arr1CGGJsonElement = array of CGGJsonElement;
  Arr2CGGJsonElement = array of Arr1CGGJsonElement;
  Arr3CGGJsonElement = array of Arr2CGGJsonElement;

  CGGASerializedName = interface;
  Arr1CGGASerializedName = array of CGGASerializedName;
  Arr2CGGASerializedName = array of Arr1CGGASerializedName;
  Arr3CGGASerializedName = array of Arr2CGGASerializedName;

  CGGJsonSerializer = interface;
  Arr1CGGJsonSerializer = array of CGGJsonSerializer;
  Arr2CGGJsonSerializer = array of Arr1CGGJsonSerializer;
  Arr3CGGJsonSerializer = array of Arr2CGGJsonSerializer;

  CGGASince = interface;
  Arr1CGGASince = array of CGGASince;
  Arr2CGGASince = array of Arr1CGGASince;
  Arr3CGGASince = array of Arr2CGGASince;

  CGGAUntil = interface;
  Arr1CGGAUntil = array of CGGAUntil;
  Arr2CGGAUntil = array of Arr1CGGAUntil;
  Arr3CGGAUntil = array of Arr2CGGAUntil;

  CGGAJsonAdapter = interface;
  Arr1CGGAJsonAdapter = array of CGGAJsonAdapter;
  Arr2CGGAJsonAdapter = array of Arr1CGGAJsonAdapter;
  Arr3CGGAJsonAdapter = array of Arr2CGGAJsonAdapter;

  CGGTypeAdapterFactory = interface;
  Arr1CGGTypeAdapterFactory = array of CGGTypeAdapterFactory;
  Arr2CGGTypeAdapterFactory = array of Arr1CGGTypeAdapterFactory;
  Arr3CGGTypeAdapterFactory = array of Arr2CGGTypeAdapterFactory;

  CGGJsonDeserializationContext = interface;
  Arr1CGGJsonDeserializationContext = array of CGGJsonDeserializationContext;
  Arr2CGGJsonDeserializationContext = array of Arr1CGGJsonDeserializationContext;
  Arr3CGGJsonDeserializationContext = array of Arr2CGGJsonDeserializationContext;

  CGGFieldNamingStrategy = interface;
  Arr1CGGFieldNamingStrategy = array of CGGFieldNamingStrategy;
  Arr2CGGFieldNamingStrategy = array of Arr1CGGFieldNamingStrategy;
  Arr3CGGFieldNamingStrategy = array of Arr2CGGFieldNamingStrategy;

  CGGJsonDeserializer = interface;
  Arr1CGGJsonDeserializer = array of CGGJsonDeserializer;
  Arr2CGGJsonDeserializer = array of Arr1CGGJsonDeserializer;
  Arr3CGGJsonDeserializer = array of Arr2CGGJsonDeserializer;

  CGGInstanceCreator = interface;
  Arr1CGGInstanceCreator = array of CGGInstanceCreator;
  Arr2CGGInstanceCreator = array of Arr1CGGInstanceCreator;
  Arr3CGGInstanceCreator = array of Arr2CGGInstanceCreator;

  CGGAExpose = interface;
  Arr1CGGAExpose = array of CGGAExpose;
  Arr2CGGAExpose = array of Arr1CGGAExpose;
  Arr3CGGAExpose = array of Arr2CGGAExpose;

  CGGIObjectConstructor = interface;
  Arr1CGGIObjectConstructor = array of CGGIObjectConstructor;
  Arr2CGGIObjectConstructor = array of Arr1CGGIObjectConstructor;
  Arr3CGGIObjectConstructor = array of Arr2CGGIObjectConstructor;

  CGGExclusionStrategy = interface;
  Arr1CGGExclusionStrategy = array of CGGExclusionStrategy;
  Arr2CGGExclusionStrategy = array of Arr1CGGExclusionStrategy;
  Arr3CGGExclusionStrategy = array of Arr2CGGExclusionStrategy;

  CGGJsonSerializationContext = interface;
  Arr1CGGJsonSerializationContext = array of CGGJsonSerializationContext;
  Arr2CGGJsonSerializationContext = array of Arr1CGGJsonSerializationContext;
  Arr3CGGJsonSerializationContext = array of Arr2CGGJsonSerializationContext;

  JSTime = class external 'java.sql' name 'Time';
  Arr1JSTime = array of JSTime;
  Arr2JSTime = array of Arr1JSTime;
  Arr3JSTime = array of Arr2JSTime;

  JUDate = class external 'java.util' name 'Date';
  Arr1JUDate = array of JUDate;
  Arr2JUDate = array of Arr1JUDate;
  Arr3JUDate = array of Arr2JUDate;

  JLThrowable = class external 'java.lang' name 'Throwable';
  Arr1JLThrowable = array of JLThrowable;
  Arr2JLThrowable = array of Arr1JLThrowable;
  Arr3JLThrowable = array of Arr2JLThrowable;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JSDate = class external 'java.sql' name 'Date';
  Arr1JSDate = array of JSDate;
  Arr2JSDate = array of Arr1JSDate;
  Arr3JSDate = array of Arr2JSDate;

  JLEnum = class external 'java.lang' name 'Enum';
  Arr1JLEnum = array of JLEnum;
  Arr2JLEnum = array of Arr1JLEnum;
  Arr3JLEnum = array of Arr2JLEnum;

  JUAbstractMap = class external 'java.util' name 'AbstractMap';
  Arr1JUAbstractMap = array of JUAbstractMap;
  Arr2JUAbstractMap = array of Arr1JUAbstractMap;
  Arr3JUAbstractMap = array of Arr2JUAbstractMap;

  JLRField = class external 'java.lang.reflect' name 'Field';
  Arr1JLRField = array of JLRField;
  Arr2JLRField = array of Arr1JLRField;
  Arr3JLRField = array of Arr2JLRField;

  JLNumber = class external 'java.lang' name 'Number';
  Arr1JLNumber = array of JLNumber;
  Arr2JLNumber = array of Arr1JLNumber;
  Arr3JLNumber = array of Arr2JLNumber;

  JIIOException = class external 'java.io' name 'IOException';
  Arr1JIIOException = array of JIIOException;
  Arr2JIIOException = array of Arr1JIIOException;
  Arr3JIIOException = array of Arr2JIIOException;

  JMBigDecimal = class external 'java.math' name 'BigDecimal';
  Arr1JMBigDecimal = array of JMBigDecimal;
  Arr2JMBigDecimal = array of Arr1JMBigDecimal;
  Arr3JMBigDecimal = array of Arr2JMBigDecimal;

  JLCharacter = class external 'java.lang' name 'Character';
  Arr1JLCharacter = array of JLCharacter;
  Arr2JLCharacter = array of Arr1JLCharacter;
  Arr3JLCharacter = array of Arr2JLCharacter;

  JIWriter = class external 'java.io' name 'Writer';
  Arr1JIWriter = array of JIWriter;
  Arr2JIWriter = array of Arr1JIWriter;
  Arr3JIWriter = array of Arr2JIWriter;

  JMBigInteger = class external 'java.math' name 'BigInteger';
  Arr1JMBigInteger = array of JMBigInteger;
  Arr2JMBigInteger = array of Arr1JMBigInteger;
  Arr3JMBigInteger = array of Arr2JMBigInteger;

  JLBoolean = class external 'java.lang' name 'Boolean';
  Arr1JLBoolean = array of JLBoolean;
  Arr2JLBoolean = array of Arr1JLBoolean;
  Arr3JLBoolean = array of Arr2JLBoolean;

  JLClass = class external 'java.lang' name 'Class';
  Arr1JLClass = array of JLClass;
  Arr2JLClass = array of Arr1JLClass;
  Arr3JLClass = array of Arr2JLClass;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  JLLong = class external 'java.lang' name 'Long';
  Arr1JLLong = array of JLLong;
  Arr2JLLong = array of Arr1JLLong;
  Arr3JLLong = array of Arr2JLLong;

  JIReader = class external 'java.io' name 'Reader';
  Arr1JIReader = array of JIReader;
  Arr2JIReader = array of Arr1JIReader;
  Arr3JIReader = array of Arr2JIReader;

  JLRuntimeException = class external 'java.lang' name 'RuntimeException';
  Arr1JLRuntimeException = array of JLRuntimeException;
  Arr2JLRuntimeException = array of Arr1JLRuntimeException;
  Arr3JLRuntimeException = array of Arr2JLRuntimeException;

  JICloseable = interface external 'java.io' name 'Closeable';
  Arr1JICloseable = array of JICloseable;
  Arr2JICloseable = array of Arr1JICloseable;
  Arr3JICloseable = array of Arr2JICloseable;

  JIFlushable = interface external 'java.io' name 'Flushable';
  Arr1JIFlushable = array of JIFlushable;
  Arr2JIFlushable = array of Arr1JIFlushable;
  Arr3JIFlushable = array of Arr2JIFlushable;

  JLIterable = interface external 'java.lang' name 'Iterable';
  Arr1JLIterable = array of JLIterable;
  Arr2JLIterable = array of Arr1JLIterable;
  Arr3JLIterable = array of Arr2JLIterable;

  JLAAnnotation = interface external 'java.lang.annotation' name 'Annotation';
  Arr1JLAAnnotation = array of JLAAnnotation;
  Arr2JLAAnnotation = array of Arr1JLAAnnotation;
  Arr3JLAAnnotation = array of Arr2JLAAnnotation;

  JLCloneable = interface external 'java.lang' name 'Cloneable';
  Arr1JLCloneable = array of JLCloneable;
  Arr2JLCloneable = array of Arr1JLCloneable;
  Arr3JLCloneable = array of Arr2JLCloneable;

  JUIterator = interface external 'java.util' name 'Iterator';
  Arr1JUIterator = array of JUIterator;
  Arr2JUIterator = array of Arr1JUIterator;
  Arr3JUIterator = array of Arr2JUIterator;

  JUComparator = interface external 'java.util' name 'Comparator';
  Arr1JUComparator = array of JUComparator;
  Arr2JUComparator = array of Arr1JUComparator;
  Arr3JUComparator = array of Arr2JUComparator;

  JLAppendable = interface external 'java.lang' name 'Appendable';
  Arr1JLAppendable = array of JLAppendable;
  Arr2JLAppendable = array of Arr1JLAppendable;
  Arr3JLAppendable = array of Arr2JLAppendable;

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

  JLRType = interface external 'java.lang.reflect' name 'Type';
  Arr1JLRType = array of JLRType;
  Arr2JLRType = array of Arr1JLRType;
  Arr3JLRType = array of Arr2JLRType;

  JISerializable = interface external 'java.io' name 'Serializable';
  Arr1JISerializable = array of JISerializable;
  Arr2JISerializable = array of Arr1JISerializable;
  Arr3JISerializable = array of Arr2JISerializable;


{$include gson.inc}

implementation

end.
