  CGGExclusionStrategy = interface external 'com.google.gson' name 'ExclusionStrategy' 
    function shouldSkipField(para1: CGGFieldAttributes): jboolean; overload;
    function shouldSkipClass(para1: JLClass): jboolean; overload;
  end;

  CGGFieldAttributes = class sealed external 'com.google.gson' name 'FieldAttributes' (JLObject)
  public
    constructor create(para1: JLRField); overload;
    function getDeclaringClass(): JLClass; overload; virtual;
    function getName(): JLString; overload; virtual;
    function getDeclaredType(): JLRType; overload; virtual;
    function getDeclaredClass(): JLClass; overload; virtual;
    function getAnnotation(para1: JLClass): JLAAnnotation; overload; virtual;
    function getAnnotations(): JUCollection; overload; virtual;
    function hasModifier(para1: jint): jboolean; overload; virtual;
  end;

  CGGFieldNamingStrategy = interface external 'com.google.gson' name 'FieldNamingStrategy' 
    function translateName(para1: JLRField): JLString; overload;
  end;

  CGGGson = class sealed external 'com.google.gson' name 'Gson' (JLObject)
  public
    type
      InnerFutureTypeAdapter = class;
      Arr1InnerFutureTypeAdapter = array of InnerFutureTypeAdapter;
      Arr2InnerFutureTypeAdapter = array of Arr1InnerFutureTypeAdapter;
      Arr3InnerFutureTypeAdapter = array of Arr2InnerFutureTypeAdapter;
      InnerFutureTypeAdapter = class external 'com.google.gson' name 'FutureTypeAdapter' 
      end;

  public
    constructor create(); overload;
    function getAdapter(para1: CGGRTypeToken): CGGTypeAdapter; overload; virtual;
    function getDelegateAdapter(para1: CGGTypeAdapterFactory; para2: CGGRTypeToken): CGGTypeAdapter; overload; virtual;
    function getAdapter(para1: JLClass): CGGTypeAdapter; overload; virtual;
    function toJsonTree(para1: JLObject): CGGJsonElement; overload; virtual;
    function toJsonTree(para1: JLObject; para2: JLRType): CGGJsonElement; overload; virtual;
    function toJson(para1: JLObject): JLString; overload; virtual;
    function toJson(para1: JLObject; para2: JLRType): JLString; overload; virtual;
    procedure toJson(para1: JLObject; para2: JLAppendable); overload; virtual;  // throws com.google.gson.JsonIOException
    procedure toJson(para1: JLObject; para2: JLRType; para3: JLAppendable); overload; virtual;  // throws com.google.gson.JsonIOException
    procedure toJson(para1: JLObject; para2: JLRType; para3: CGGSJsonWriter); overload; virtual;  // throws com.google.gson.JsonIOException
    function toJson(para1: CGGJsonElement): JLString; overload; virtual;
    procedure toJson(para1: CGGJsonElement; para2: JLAppendable); overload; virtual;  // throws com.google.gson.JsonIOException
    procedure toJson(para1: CGGJsonElement; para2: CGGSJsonWriter); overload; virtual;  // throws com.google.gson.JsonIOException
    function fromJson(para1: JLString; para2: JLClass): JLObject; overload; virtual;  // throws com.google.gson.JsonSyntaxException
    function fromJson(para1: JLString; para2: JLRType): JLObject; overload; virtual;  // throws com.google.gson.JsonSyntaxException
    function fromJson(para1: JIReader; para2: JLClass): JLObject; overload; virtual;  // throws com.google.gson.JsonSyntaxException, com.google.gson.JsonIOException
    function fromJson(para1: JIReader; para2: JLRType): JLObject; overload; virtual;  // throws com.google.gson.JsonIOException, com.google.gson.JsonSyntaxException
    function fromJson(para1: CGGSJsonReader; para2: JLRType): JLObject; overload; virtual;  // throws com.google.gson.JsonIOException, com.google.gson.JsonSyntaxException
    function fromJson(para1: CGGJsonElement; para2: JLClass): JLObject; overload; virtual;  // throws com.google.gson.JsonSyntaxException
    function fromJson(para1: CGGJsonElement; para2: JLRType): JLObject; overload; virtual;  // throws com.google.gson.JsonSyntaxException
    function toString(): JLString; overload; virtual;
  end;

  CGGGsonBuilder = class sealed external 'com.google.gson' name 'GsonBuilder' (JLObject)
  public
    constructor create(); overload;
    function setVersion(para1: jdouble): CGGGsonBuilder; overload; virtual;
    function excludeFieldsWithModifiers(para1: Arr1jint): CGGGsonBuilder; overload; virtual;
    function excludeFieldsWithModifiers(const para1: array of jint): CGGGsonBuilder; overload; virtual;
    function generateNonExecutableJson(): CGGGsonBuilder; overload; virtual;
    function excludeFieldsWithoutExposeAnnotation(): CGGGsonBuilder; overload; virtual;
    function serializeNulls(): CGGGsonBuilder; overload; virtual;
    function enableComplexMapKeySerialization(): CGGGsonBuilder; overload; virtual;
    function disableInnerClassSerialization(): CGGGsonBuilder; overload; virtual;
    function setLongSerializationPolicy(para1: CGGLongSerializationPolicy): CGGGsonBuilder; overload; virtual;
    function setFieldNamingPolicy(para1: CGGFieldNamingPolicy): CGGGsonBuilder; overload; virtual;
    function setFieldNamingStrategy(para1: CGGFieldNamingStrategy): CGGGsonBuilder; overload; virtual;
    function setExclusionStrategies(para1: Arr1CGGExclusionStrategy): CGGGsonBuilder; overload; virtual;
    function setExclusionStrategies(const para1: array of CGGExclusionStrategy): CGGGsonBuilder; overload; virtual;
    function addSerializationExclusionStrategy(para1: CGGExclusionStrategy): CGGGsonBuilder; overload; virtual;
    function addDeserializationExclusionStrategy(para1: CGGExclusionStrategy): CGGGsonBuilder; overload; virtual;
    function setPrettyPrinting(): CGGGsonBuilder; overload; virtual;
    function disableHtmlEscaping(): CGGGsonBuilder; overload; virtual;
    function setDateFormat(para1: JLString): CGGGsonBuilder; overload; virtual;
    function setDateFormat(para1: jint): CGGGsonBuilder; overload; virtual;
    function setDateFormat(para1: jint; para2: jint): CGGGsonBuilder; overload; virtual;
    function registerTypeAdapter(para1: JLRType; para2: JLObject): CGGGsonBuilder; overload; virtual;
    function registerTypeAdapterFactory(para1: CGGTypeAdapterFactory): CGGGsonBuilder; overload; virtual;
    function registerTypeHierarchyAdapter(para1: JLClass; para2: JLObject): CGGGsonBuilder; overload; virtual;
    function serializeSpecialFloatingPointValues(): CGGGsonBuilder; overload; virtual;
    function create_(): CGGGson; external name 'create'; overload; virtual;
  end;

  CGGInstanceCreator = interface external 'com.google.gson' name 'InstanceCreator' 
    function createInstance(para1: JLRType): JLObject; overload;
  end;

  CGGJsonDeserializationContext = interface external 'com.google.gson' name 'JsonDeserializationContext' 
    function deserialize(para1: CGGJsonElement; para2: JLRType): JLObject; overload;  // throws com.google.gson.JsonParseException
  end;

  CGGJsonDeserializer = interface external 'com.google.gson' name 'JsonDeserializer' 
    function deserialize(para1: CGGJsonElement; para2: JLRType; para3: CGGJsonDeserializationContext): JLObject; overload;  // throws com.google.gson.JsonParseException
  end;

  CGGJsonElement = class abstract external 'com.google.gson' name 'JsonElement' (JLObject)
  public
    constructor create(); overload;
    function isJsonArray(): jboolean; overload; virtual;
    function isJsonObject(): jboolean; overload; virtual;
    function isJsonPrimitive(): jboolean; overload; virtual;
    function isJsonNull(): jboolean; overload; virtual;
    function getAsJsonObject(): CGGJsonObject; overload; virtual;
    function getAsJsonArray(): CGGJsonArray; overload; virtual;
    function getAsJsonPrimitive(): CGGJsonPrimitive; overload; virtual;
    function getAsJsonNull(): CGGJsonNull; overload; virtual;
    function getAsBoolean(): jboolean; overload; virtual;
    function getAsNumber(): JLNumber; overload; virtual;
    function getAsString(): JLString; overload; virtual;
    function getAsDouble(): jdouble; overload; virtual;
    function getAsFloat(): jfloat; overload; virtual;
    function getAsLong(): jlong; overload; virtual;
    function getAsInt(): jint; overload; virtual;
    function getAsByte(): jbyte; overload; virtual;
    function getAsCharacter(): jchar; overload; virtual;
    function getAsBigDecimal(): JMBigDecimal; overload; virtual;
    function getAsBigInteger(): JMBigInteger; overload; virtual;
    function getAsShort(): jshort; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  CGGJsonParser = class sealed external 'com.google.gson' name 'JsonParser' (JLObject)
  public
    constructor create(); overload;
    function parse(para1: JLString): CGGJsonElement; overload; virtual;  // throws com.google.gson.JsonSyntaxException
    function parse(para1: JIReader): CGGJsonElement; overload; virtual;  // throws com.google.gson.JsonIOException, com.google.gson.JsonSyntaxException
    function parse(para1: CGGSJsonReader): CGGJsonElement; overload; virtual;  // throws com.google.gson.JsonIOException, com.google.gson.JsonSyntaxException
  end;

  CGGJsonSerializationContext = interface external 'com.google.gson' name 'JsonSerializationContext' 
    function serialize(para1: JLObject): CGGJsonElement; overload;
    function serialize(para1: JLObject; para2: JLRType): CGGJsonElement; overload;
  end;

  CGGJsonSerializer = interface external 'com.google.gson' name 'JsonSerializer' 
    function serialize(para1: JLObject; para2: JLRType; para3: CGGJsonSerializationContext): CGGJsonElement; overload;
  end;

  CGGTypeAdapter = class abstract external 'com.google.gson' name 'TypeAdapter' (JLObject)
  public
    constructor create(); overload;
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual; abstract;  // throws java.io.IOException
    procedure toJson(para1: JIWriter; para2: JLObject); overload; virtual; final;  // throws java.io.IOException
    function nullSafe(): CGGTypeAdapter; overload; virtual; final;
    function toJson(para1: JLObject): JLString; overload; virtual; final;  // throws java.io.IOException
    function toJsonTree(para1: JLObject): CGGJsonElement; overload; virtual; final;
    function read(para1: CGGSJsonReader): JLObject; overload; virtual; abstract;  // throws java.io.IOException
    function fromJson(para1: JIReader): JLObject; overload; virtual; final;  // throws java.io.IOException
    function fromJson(para1: JLString): JLObject; overload; virtual; final;  // throws java.io.IOException
    function fromJsonTree(para1: CGGJsonElement): JLObject; overload; virtual; final;
  end;

  CGGTypeAdapterFactory = interface external 'com.google.gson' name 'TypeAdapterFactory' 
    function create_(para1: CGGGson; para2: CGGRTypeToken): CGGTypeAdapter; external name 'create'; overload;
  end;

  CGGIConstructorConstructor = class sealed external 'com.google.gson.internal' name 'ConstructorConstructor' (JLObject)
  public
    constructor create(para1: JUMap); overload;
    function get(para1: CGGRTypeToken): CGGIObjectConstructor; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  CGGIJsonReaderInternalAccess = class abstract external 'com.google.gson.internal' name 'JsonReaderInternalAccess' (JLObject)
  public
    class var
      fINSTANCE: CGGIJsonReaderInternalAccess; external name 'INSTANCE';
  public
    constructor create(); overload;
    procedure promoteNameToValue(para1: CGGSJsonReader); overload; virtual; abstract;  // throws java.io.IOException
  end;

  CGGIObjectConstructor = interface external 'com.google.gson.internal' name 'ObjectConstructor' 
    function construct(): JLObject; overload;
  end;

  CGGIPrimitives = class sealed external 'com.google.gson.internal' name 'Primitives' (JLObject)
  public
    class function isPrimitive(para1: JLRType): jboolean; static; overload;
    class function isWrapperType(para1: JLRType): jboolean; static; overload;
    class function wrap(para1: JLClass): JLClass; static; overload;
    class function unwrap(para1: JLClass): JLClass; static; overload;
  end;

  CGGIStreams = class sealed external 'com.google.gson.internal' name 'Streams' (JLObject)
  public
    type
      InnerAppendableWriter = class;
      Arr1InnerAppendableWriter = array of InnerAppendableWriter;
      Arr2InnerAppendableWriter = array of Arr1InnerAppendableWriter;
      Arr3InnerAppendableWriter = array of Arr2InnerAppendableWriter;
      InnerAppendableWriter = class sealed external 'com.google.gson.internal' name 'AppendableWriter' 
      public
        type
          InnerCurrentWrite = class;
          Arr1InnerCurrentWrite = array of InnerCurrentWrite;
          Arr2InnerCurrentWrite = array of Arr1InnerCurrentWrite;
          Arr3InnerCurrentWrite = array of Arr2InnerCurrentWrite;
          InnerCurrentWrite = class external 'com.google.gson.internal' name 'CurrentWrite' 
          end;

      end;

  public
    constructor create(); overload;
    class function parse(para1: CGGSJsonReader): CGGJsonElement; static; overload;  // throws com.google.gson.JsonParseException
    class procedure write(para1: CGGJsonElement; para2: CGGSJsonWriter); static; overload;  // throws java.io.IOException
    class function writerForAppendable(para1: JLAppendable): JIWriter; static; overload;
  end;

  CGGIUnsafeAllocator = class abstract external 'com.google.gson.internal' name 'UnsafeAllocator' (JLObject)
  public
    constructor create(); overload;
    function newInstance(para1: JLClass): JLObject; overload; virtual; abstract;  // throws java.lang.Exception
    class function create_(): CGGIUnsafeAllocator; static; external name 'create'; overload;
  end;

  CGGIBTypeAdapters = class sealed external 'com.google.gson.internal.bind' name 'TypeAdapters' (JLObject)
  public
    type
      InnerEnumTypeAdapter = class;
      Arr1InnerEnumTypeAdapter = array of InnerEnumTypeAdapter;
      Arr2InnerEnumTypeAdapter = array of Arr1InnerEnumTypeAdapter;
      Arr3InnerEnumTypeAdapter = array of Arr2InnerEnumTypeAdapter;
      InnerEnumTypeAdapter = class sealed external 'com.google.gson.internal.bind' name 'EnumTypeAdapter' 
      end;

  public
    final class var
      fCLASS: CGGTypeAdapter; external name 'CLASS';
      fCLASS_FACTORY: CGGTypeAdapterFactory; external name 'CLASS_FACTORY';
      fBIT_SET: CGGTypeAdapter; external name 'BIT_SET';
      fBIT_SET_FACTORY: CGGTypeAdapterFactory; external name 'BIT_SET_FACTORY';
      fBOOLEAN: CGGTypeAdapter; external name 'BOOLEAN';
      fBOOLEAN_AS_STRING: CGGTypeAdapter; external name 'BOOLEAN_AS_STRING';
      fBOOLEAN_FACTORY: CGGTypeAdapterFactory; external name 'BOOLEAN_FACTORY';
      fBYTE: CGGTypeAdapter; external name 'BYTE';
      fBYTE_FACTORY: CGGTypeAdapterFactory; external name 'BYTE_FACTORY';
      fSHORT: CGGTypeAdapter; external name 'SHORT';
      fSHORT_FACTORY: CGGTypeAdapterFactory; external name 'SHORT_FACTORY';
      fINTEGER: CGGTypeAdapter; external name 'INTEGER';
      fINTEGER_FACTORY: CGGTypeAdapterFactory; external name 'INTEGER_FACTORY';
      fLONG: CGGTypeAdapter; external name 'LONG';
      fFLOAT: CGGTypeAdapter; external name 'FLOAT';
      fDOUBLE: CGGTypeAdapter; external name 'DOUBLE';
      fNUMBER: CGGTypeAdapter; external name 'NUMBER';
      fNUMBER_FACTORY: CGGTypeAdapterFactory; external name 'NUMBER_FACTORY';
      fCHARACTER: CGGTypeAdapter; external name 'CHARACTER';
      fCHARACTER_FACTORY: CGGTypeAdapterFactory; external name 'CHARACTER_FACTORY';
      fSTRING: CGGTypeAdapter; external name 'STRING';
      fBIG_DECIMAL: CGGTypeAdapter; external name 'BIG_DECIMAL';
      fBIG_INTEGER: CGGTypeAdapter; external name 'BIG_INTEGER';
      fSTRING_FACTORY: CGGTypeAdapterFactory; external name 'STRING_FACTORY';
      fSTRING_BUILDER: CGGTypeAdapter; external name 'STRING_BUILDER';
      fSTRING_BUILDER_FACTORY: CGGTypeAdapterFactory; external name 'STRING_BUILDER_FACTORY';
      fSTRING_BUFFER: CGGTypeAdapter; external name 'STRING_BUFFER';
      fSTRING_BUFFER_FACTORY: CGGTypeAdapterFactory; external name 'STRING_BUFFER_FACTORY';
      fURL: CGGTypeAdapter; external name 'URL';
      fURL_FACTORY: CGGTypeAdapterFactory; external name 'URL_FACTORY';
      fURI: CGGTypeAdapter; external name 'URI';
      fURI_FACTORY: CGGTypeAdapterFactory; external name 'URI_FACTORY';
      fINET_ADDRESS: CGGTypeAdapter; external name 'INET_ADDRESS';
      fINET_ADDRESS_FACTORY: CGGTypeAdapterFactory; external name 'INET_ADDRESS_FACTORY';
      fUUID: CGGTypeAdapter; external name 'UUID';
      fUUID_FACTORY: CGGTypeAdapterFactory; external name 'UUID_FACTORY';
      fTIMESTAMP_FACTORY: CGGTypeAdapterFactory; external name 'TIMESTAMP_FACTORY';
      fCALENDAR: CGGTypeAdapter; external name 'CALENDAR';
      fCALENDAR_FACTORY: CGGTypeAdapterFactory; external name 'CALENDAR_FACTORY';
      fLOCALE: CGGTypeAdapter; external name 'LOCALE';
      fLOCALE_FACTORY: CGGTypeAdapterFactory; external name 'LOCALE_FACTORY';
      fJSON_ELEMENT: CGGTypeAdapter; external name 'JSON_ELEMENT';
      fJSON_ELEMENT_FACTORY: CGGTypeAdapterFactory; external name 'JSON_ELEMENT_FACTORY';
      fENUM_FACTORY: CGGTypeAdapterFactory; external name 'ENUM_FACTORY';
  public
    class function newFactory(para1: CGGRTypeToken; para2: CGGTypeAdapter): CGGTypeAdapterFactory; static; overload;
    class function newFactory(para1: JLClass; para2: CGGTypeAdapter): CGGTypeAdapterFactory; static; overload;
    class function newFactory(para1: JLClass; para2: JLClass; para3: CGGTypeAdapter): CGGTypeAdapterFactory; static; overload;
    class function newFactoryForMultipleTypes(para1: JLClass; para2: JLClass; para3: CGGTypeAdapter): CGGTypeAdapterFactory; static; overload;
    class function newTypeHierarchyFactory(para1: JLClass; para2: CGGTypeAdapter): CGGTypeAdapterFactory; static; overload;
  end;

  CGGRTypeToken = class external 'com.google.gson.reflect' name 'TypeToken' (JLObject)
  strict protected
    constructor create(); overload;
  public
    function getRawType(): JLClass; overload; virtual; final;
    function getType(): JLRType; overload; virtual; final;
    function isAssignableFrom(para1: JLClass): jboolean; overload; virtual;
    function isAssignableFrom(para1: JLRType): jboolean; overload; virtual;
    function isAssignableFrom(para1: CGGRTypeToken): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual; final;
    function equals(para1: JLObject): jboolean; overload; virtual; final;
    function toString(): JLString; overload; virtual; final;
    class function get(para1: JLRType): CGGRTypeToken; static; overload;
    class function get(para1: JLClass): CGGRTypeToken; static; overload;
  end;

  CGGSJsonScope = class sealed external 'com.google.gson.stream' name 'JsonScope' (JLObject)
  end;

  CGGLongSerializationPolicy = class abstract external 'com.google.gson' name 'LongSerializationPolicy' (JLEnum)
  public
    final class var
      fDEFAULT: CGGLongSerializationPolicy; external name 'DEFAULT';
      fSTRING: CGGLongSerializationPolicy; external name 'STRING';
  public
    class function values(): Arr1CGGLongSerializationPolicy; static; overload;
    class function valueOf(para1: JLString): CGGLongSerializationPolicy; static; overload;
    function serialize(para1: JLLong): CGGJsonElement; overload; virtual; abstract;
  end;

  CGGSJsonToken = class sealed external 'com.google.gson.stream' name 'JsonToken' (JLEnum)
  public
    final class var
      fBEGIN_ARRAY: CGGSJsonToken; external name 'BEGIN_ARRAY';
      fEND_ARRAY: CGGSJsonToken; external name 'END_ARRAY';
      fBEGIN_OBJECT: CGGSJsonToken; external name 'BEGIN_OBJECT';
      fEND_OBJECT: CGGSJsonToken; external name 'END_OBJECT';
      fNAME: CGGSJsonToken; external name 'NAME';
      fSTRING: CGGSJsonToken; external name 'STRING';
      fNUMBER: CGGSJsonToken; external name 'NUMBER';
      fBOOLEAN: CGGSJsonToken; external name 'BOOLEAN';
      fNULL: CGGSJsonToken; external name 'NULL';
      fEND_DOCUMENT: CGGSJsonToken; external name 'END_DOCUMENT';
  public
    class function values(): Arr1CGGSJsonToken; static; overload;
    class function valueOf(para1: JLString): CGGSJsonToken; static; overload;
  end;

  CGGJsonParseException = class external 'com.google.gson' name 'JsonParseException' (JLRuntimeException)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  CGGJsonStreamParser = class sealed external 'com.google.gson' name 'JsonStreamParser' (JLObject, JUIterator)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JIReader); overload;
    function next(): CGGJsonElement; overload; virtual;  // throws com.google.gson.JsonParseException
    function hasNext(): jboolean; overload; virtual;
    procedure remove(); overload; virtual;
    function next(): JLObject; overload; virtual;
  end;

  CGGAExpose = interface external 'com.google.gson.annotations' name 'Expose' (JLAAnnotation)
    function serialize(): jboolean; overload;
    function deserialize(): jboolean; overload;
  end;

  CGGAJsonAdapter = interface external 'com.google.gson.annotations' name 'JsonAdapter' (JLAAnnotation)
    function value(): JLClass; overload;
  end;

  CGGASerializedName = interface external 'com.google.gson.annotations' name 'SerializedName' (JLAAnnotation)
    function value(): JLString; overload;
  end;

  CGGASince = interface external 'com.google.gson.annotations' name 'Since' (JLAAnnotation)
    function value(): jdouble; overload;
  end;

  CGGAUntil = interface external 'com.google.gson.annotations' name 'Until' (JLAAnnotation)
    function value(): jdouble; overload;
  end;

  CGGILazilyParsedNumber = class sealed external 'com.google.gson.internal' name 'LazilyParsedNumber' (JLNumber)
  public
    constructor create(para1: JLString); overload;
    function intValue(): jint; overload; virtual;
    function longValue(): jlong; overload; virtual;
    function floatValue(): jfloat; overload; virtual;
    function doubleValue(): jdouble; overload; virtual;
    function toString(): JLString; overload; virtual;
  end;

  CGGILinkedHashTreeMap = class sealed external 'com.google.gson.internal' name 'LinkedHashTreeMap' (JUAbstractMap, JISerializable)
  public
    type
      InnerKeySet = class;
      Arr1InnerKeySet = array of InnerKeySet;
      Arr2InnerKeySet = array of Arr1InnerKeySet;
      Arr3InnerKeySet = array of Arr2InnerKeySet;
      InnerEntrySet = class;
      Arr1InnerEntrySet = array of InnerEntrySet;
      Arr2InnerEntrySet = array of Arr1InnerEntrySet;
      Arr3InnerEntrySet = array of Arr2InnerEntrySet;
      InnerAvlBuilder = class;
      Arr1InnerAvlBuilder = array of InnerAvlBuilder;
      Arr2InnerAvlBuilder = array of Arr1InnerAvlBuilder;
      Arr3InnerAvlBuilder = array of Arr2InnerAvlBuilder;
      InnerLinkedTreeMapIterator = class;
      Arr1InnerLinkedTreeMapIterator = array of InnerLinkedTreeMapIterator;
      Arr2InnerLinkedTreeMapIterator = array of Arr1InnerLinkedTreeMapIterator;
      Arr3InnerLinkedTreeMapIterator = array of Arr2InnerLinkedTreeMapIterator;
      InnerAvlIterator = class;
      Arr1InnerAvlIterator = array of InnerAvlIterator;
      Arr2InnerAvlIterator = array of Arr1InnerAvlIterator;
      Arr3InnerAvlIterator = array of Arr2InnerAvlIterator;
      InnerNode = class;
      Arr1InnerNode = array of InnerNode;
      Arr2InnerNode = array of Arr1InnerNode;
      Arr3InnerNode = array of Arr2InnerNode;
      InnerKeySet = class sealed external 'com.google.gson.internal' name 'KeySet' 
      end;

      InnerEntrySet = class sealed external 'com.google.gson.internal' name 'EntrySet' 
      end;

      InnerAvlBuilder = class sealed external 'com.google.gson.internal' name 'AvlBuilder' 
      end;

      InnerLinkedTreeMapIterator = class abstract external 'com.google.gson.internal' name 'LinkedTreeMapIterator' 
      end;

      InnerAvlIterator = class external 'com.google.gson.internal' name 'AvlIterator' 
      end;

      InnerNode = class sealed external 'com.google.gson.internal' name 'Node' 
      end;

  public
    constructor create(); overload;
    constructor create(para1: JUComparator); overload;
    function size(): jint; overload; virtual;
    function get(para1: JLObject): JLObject; overload; virtual;
    function containsKey(para1: JLObject): jboolean; overload; virtual;
    function put(para1: JLObject; para2: JLObject): JLObject; overload; virtual;
    procedure clear(); overload; virtual;
    function remove(para1: JLObject): JLObject; overload; virtual;
    function entrySet(): JUSet; overload; virtual;
    function keySet(): JUSet; overload; virtual;
  end;

  CGGILinkedTreeMap = class sealed external 'com.google.gson.internal' name 'LinkedTreeMap' (JUAbstractMap, JISerializable)
  public
    type
      InnerKeySet = class;
      Arr1InnerKeySet = array of InnerKeySet;
      Arr2InnerKeySet = array of Arr1InnerKeySet;
      Arr3InnerKeySet = array of Arr2InnerKeySet;
      InnerEntrySet = class;
      Arr1InnerEntrySet = array of InnerEntrySet;
      Arr2InnerEntrySet = array of Arr1InnerEntrySet;
      Arr3InnerEntrySet = array of Arr2InnerEntrySet;
      InnerLinkedTreeMapIterator = class;
      Arr1InnerLinkedTreeMapIterator = array of InnerLinkedTreeMapIterator;
      Arr2InnerLinkedTreeMapIterator = array of Arr1InnerLinkedTreeMapIterator;
      Arr3InnerLinkedTreeMapIterator = array of Arr2InnerLinkedTreeMapIterator;
      InnerNode = class;
      Arr1InnerNode = array of InnerNode;
      Arr2InnerNode = array of Arr1InnerNode;
      Arr3InnerNode = array of Arr2InnerNode;
      InnerKeySet = class sealed external 'com.google.gson.internal' name 'KeySet' 
      end;

      InnerEntrySet = class external 'com.google.gson.internal' name 'EntrySet' 
      end;

      InnerLinkedTreeMapIterator = class abstract external 'com.google.gson.internal' name 'LinkedTreeMapIterator' 
      end;

      InnerNode = class sealed external 'com.google.gson.internal' name 'Node' 
      end;

  public
    constructor create(); overload;
    constructor create(para1: JUComparator); overload;
    function size(): jint; overload; virtual;
    function get(para1: JLObject): JLObject; overload; virtual;
    function containsKey(para1: JLObject): jboolean; overload; virtual;
    function put(para1: JLObject; para2: JLObject): JLObject; overload; virtual;
    procedure clear(); overload; virtual;
    function remove(para1: JLObject): JLObject; overload; virtual;
    function entrySet(): JUSet; overload; virtual;
    function keySet(): JUSet; overload; virtual;
  end;

  CGGSJsonReader = class external 'com.google.gson.stream' name 'JsonReader' (JLObject, JICloseable)
  public
    constructor create(para1: JIReader); overload;
    procedure setLenient(para1: jboolean); overload; virtual; final;
    function isLenient(): jboolean; overload; virtual; final;
    procedure beginArray(); overload; virtual;  // throws java.io.IOException
    procedure endArray(); overload; virtual;  // throws java.io.IOException
    procedure beginObject(); overload; virtual;  // throws java.io.IOException
    procedure endObject(); overload; virtual;  // throws java.io.IOException
    function hasNext(): jboolean; overload; virtual;  // throws java.io.IOException
    function peek(): CGGSJsonToken; overload; virtual;  // throws java.io.IOException
    function nextName(): JLString; overload; virtual;  // throws java.io.IOException
    function nextString(): JLString; overload; virtual;  // throws java.io.IOException
    function nextBoolean(): jboolean; overload; virtual;  // throws java.io.IOException
    procedure nextNull(); overload; virtual;  // throws java.io.IOException
    function nextDouble(): jdouble; overload; virtual;  // throws java.io.IOException
    function nextLong(): jlong; overload; virtual;  // throws java.io.IOException
    function nextInt(): jint; overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
    procedure skipValue(); overload; virtual;  // throws java.io.IOException
    function toString(): JLString; overload; virtual;
    function getPath(): JLString; overload; virtual;
  end;

  CGGSJsonWriter = class external 'com.google.gson.stream' name 'JsonWriter' (JLObject, JICloseable, JIFlushable)
  public
    constructor create(para1: JIWriter); overload;
    procedure setIndent(para1: JLString); overload; virtual; final;
    procedure setLenient(para1: jboolean); overload; virtual; final;
    function isLenient(): jboolean; overload; virtual;
    procedure setHtmlSafe(para1: jboolean); overload; virtual; final;
    function isHtmlSafe(): jboolean; overload; virtual; final;
    procedure setSerializeNulls(para1: jboolean); overload; virtual; final;
    function getSerializeNulls(): jboolean; overload; virtual; final;
    function beginArray(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function endArray(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function beginObject(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function endObject(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function name(para1: JLString): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: JLString): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function nullValue(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: jboolean): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: jdouble): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: jlong): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: JLNumber): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  end;

  CGGSMalformedJsonException = class sealed external 'com.google.gson.stream' name 'MalformedJsonException' (JIIOException)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  CGGFieldNamingPolicy = class abstract external 'com.google.gson' name 'FieldNamingPolicy' (JLEnum, CGGFieldNamingStrategy)
  public
    final class var
      fIDENTITY: CGGFieldNamingPolicy; external name 'IDENTITY';
      fUPPER_CAMEL_CASE: CGGFieldNamingPolicy; external name 'UPPER_CAMEL_CASE';
      fUPPER_CAMEL_CASE_WITH_SPACES: CGGFieldNamingPolicy; external name 'UPPER_CAMEL_CASE_WITH_SPACES';
      fLOWER_CASE_WITH_UNDERSCORES: CGGFieldNamingPolicy; external name 'LOWER_CASE_WITH_UNDERSCORES';
      fLOWER_CASE_WITH_DASHES: CGGFieldNamingPolicy; external name 'LOWER_CASE_WITH_DASHES';
  public
    class function values(): Arr1CGGFieldNamingPolicy; static; overload;
    class function valueOf(para1: JLString): CGGFieldNamingPolicy; static; overload;
  end;

  CGGJsonArray = class sealed external 'com.google.gson' name 'JsonArray' (CGGJsonElement, JLIterable)
  public
    constructor create(); overload;
    procedure add(para1: CGGJsonElement); overload; virtual;
    procedure addAll(para1: CGGJsonArray); overload; virtual;
    function &set(para1: jint; para2: CGGJsonElement): CGGJsonElement; overload; virtual;
    function remove(para1: CGGJsonElement): jboolean; overload; virtual;
    function remove(para1: jint): CGGJsonElement; overload; virtual;
    function contains(para1: CGGJsonElement): jboolean; overload; virtual;
    function size(): jint; overload; virtual;
    function iterator(): JUIterator; overload; virtual;
    function get(para1: jint): CGGJsonElement; overload; virtual;
    function getAsNumber(): JLNumber; overload; virtual;
    function getAsString(): JLString; overload; virtual;
    function getAsDouble(): jdouble; overload; virtual;
    function getAsBigDecimal(): JMBigDecimal; overload; virtual;
    function getAsBigInteger(): JMBigInteger; overload; virtual;
    function getAsFloat(): jfloat; overload; virtual;
    function getAsLong(): jlong; overload; virtual;
    function getAsInt(): jint; overload; virtual;
    function getAsByte(): jbyte; overload; virtual;
    function getAsCharacter(): jchar; overload; virtual;
    function getAsShort(): jshort; overload; virtual;
    function getAsBoolean(): jboolean; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  CGGJsonNull = class sealed external 'com.google.gson' name 'JsonNull' (CGGJsonElement)
  public
    final class var
      fINSTANCE: CGGJsonNull; external name 'INSTANCE';
  public
    constructor create(); overload;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
  end;

  CGGJsonObject = class sealed external 'com.google.gson' name 'JsonObject' (CGGJsonElement)
  public
    constructor create(); overload;
    procedure add(para1: JLString; para2: CGGJsonElement); overload; virtual;
    function remove(para1: JLString): CGGJsonElement; overload; virtual;
    procedure addProperty(para1: JLString; para2: JLString); overload; virtual;
    procedure addProperty(para1: JLString; para2: JLNumber); overload; virtual;
    procedure addProperty(para1: JLString; para2: JLBoolean); overload; virtual;
    procedure addProperty(para1: JLString; para2: JLCharacter); overload; virtual;
    function entrySet(): JUSet; overload; virtual;
    function has(para1: JLString): jboolean; overload; virtual;
    function get(para1: JLString): CGGJsonElement; overload; virtual;
    function getAsJsonPrimitive(para1: JLString): CGGJsonPrimitive; overload; virtual;
    function getAsJsonArray(para1: JLString): CGGJsonArray; overload; virtual;
    function getAsJsonObject(para1: JLString): CGGJsonObject; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
    function hashCode(): jint; overload; virtual;
  end;

  CGGJsonPrimitive = class sealed external 'com.google.gson' name 'JsonPrimitive' (CGGJsonElement)
  public
    constructor create(para1: JLBoolean); overload;
    constructor create(para1: JLNumber); overload;
    constructor create(para1: JLString); overload;
    constructor create(para1: JLCharacter); overload;
    function isBoolean(): jboolean; overload; virtual;
    function getAsBoolean(): jboolean; overload; virtual;
    function isNumber(): jboolean; overload; virtual;
    function getAsNumber(): JLNumber; overload; virtual;
    function isString(): jboolean; overload; virtual;
    function getAsString(): JLString; overload; virtual;
    function getAsDouble(): jdouble; overload; virtual;
    function getAsBigDecimal(): JMBigDecimal; overload; virtual;
    function getAsBigInteger(): JMBigInteger; overload; virtual;
    function getAsFloat(): jfloat; overload; virtual;
    function getAsLong(): jlong; overload; virtual;
    function getAsShort(): jshort; overload; virtual;
    function getAsInt(): jint; overload; virtual;
    function getAsByte(): jbyte; overload; virtual;
    function getAsCharacter(): jchar; overload; virtual;
    function hashCode(): jint; overload; virtual;
    function equals(para1: JLObject): jboolean; overload; virtual;
  end;

  CGGDefaultDateTypeAdapter = class sealed external 'com.google.gson' name 'DefaultDateTypeAdapter' (JLObject, CGGJsonSerializer, CGGJsonDeserializer)
  public
    constructor create(para1: jint; para2: jint); overload;
    function serialize(para1: JUDate; para2: JLRType; para3: CGGJsonSerializationContext): CGGJsonElement; overload; virtual;
    function deserialize(para1: CGGJsonElement; para2: JLRType; para3: CGGJsonDeserializationContext): JUDate; overload; virtual;  // throws com.google.gson.JsonParseException
    function toString(): JLString; overload; virtual;
    function serialize(para1: JLObject; para2: JLRType; para3: CGGJsonSerializationContext): CGGJsonElement; overload; virtual;
    function deserialize(para1: CGGJsonElement; para2: JLRType; para3: CGGJsonDeserializationContext): JLObject; overload; virtual;  // throws com.google.gson.JsonParseException
  end;

  CGGTreeTypeAdapter = class sealed external 'com.google.gson' name 'TreeTypeAdapter' (CGGTypeAdapter)
  public
    type
      InnerSingleTypeFactory = class;
      Arr1InnerSingleTypeFactory = array of InnerSingleTypeFactory;
      Arr2InnerSingleTypeFactory = array of Arr1InnerSingleTypeFactory;
      Arr3InnerSingleTypeFactory = array of Arr2InnerSingleTypeFactory;
      InnerSingleTypeFactory = class external 'com.google.gson' name 'SingleTypeFactory' 
      end;

  public
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
    class function newFactory(para1: CGGRTypeToken; para2: JLObject): CGGTypeAdapterFactory; static; overload;
    class function newFactoryWithMatchRawType(para1: CGGRTypeToken; para2: JLObject): CGGTypeAdapterFactory; static; overload;
    class function newTypeHierarchyFactory(para1: JLClass; para2: JLObject): CGGTypeAdapterFactory; static; overload;
  end;

  CGGIBArrayTypeAdapter = class sealed external 'com.google.gson.internal.bind' name 'ArrayTypeAdapter' (CGGTypeAdapter)
  public
    final class var
      fFACTORY: CGGTypeAdapterFactory; external name 'FACTORY';
  public
    constructor create(para1: CGGGson; para2: CGGTypeAdapter; para3: JLClass); overload;
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
  end;

  CGGIBDateTypeAdapter = class sealed external 'com.google.gson.internal.bind' name 'DateTypeAdapter' (CGGTypeAdapter)
  public
    final class var
      fFACTORY: CGGTypeAdapterFactory; external name 'FACTORY';
  public
    constructor create(); overload;
    function read(para1: CGGSJsonReader): JUDate; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JUDate); overload; virtual;  // throws java.io.IOException
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
  end;

  CGGIBObjectTypeAdapter = class sealed external 'com.google.gson.internal.bind' name 'ObjectTypeAdapter' (CGGTypeAdapter)
  public
    final class var
      fFACTORY: CGGTypeAdapterFactory; external name 'FACTORY';
  public
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
  end;

  CGGIBSqlDateTypeAdapter = class sealed external 'com.google.gson.internal.bind' name 'SqlDateTypeAdapter' (CGGTypeAdapter)
  public
    final class var
      fFACTORY: CGGTypeAdapterFactory; external name 'FACTORY';
  public
    constructor create(); overload;
    function read(para1: CGGSJsonReader): JSDate; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JSDate); overload; virtual;  // throws java.io.IOException
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
  end;

  CGGIBTimeTypeAdapter = class sealed external 'com.google.gson.internal.bind' name 'TimeTypeAdapter' (CGGTypeAdapter)
  public
    final class var
      fFACTORY: CGGTypeAdapterFactory; external name 'FACTORY';
  public
    constructor create(); overload;
    function read(para1: CGGSJsonReader): JSTime; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JSTime); overload; virtual;  // throws java.io.IOException
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
  end;

  CGGIBTypeAdapterRuntimeTypeWrapper = class sealed external 'com.google.gson.internal.bind' name 'TypeAdapterRuntimeTypeWrapper' (CGGTypeAdapter)
  public
    function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
    procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
  end;

  CGGIExcluder = class sealed external 'com.google.gson.internal' name 'Excluder' (JLObject, CGGTypeAdapterFactory, JLCloneable)
  public
    final class var
      fDEFAULT: CGGIExcluder; external name 'DEFAULT';
  public
    constructor create(); overload;
  strict protected
    function clone(): CGGIExcluder; overload; virtual;
  public
    function withVersion(para1: jdouble): CGGIExcluder; overload; virtual;
    function withModifiers(para1: Arr1jint): CGGIExcluder; overload; virtual;
    function withModifiers(const para1: array of jint): CGGIExcluder; overload; virtual;
    function disableInnerClassSerialization(): CGGIExcluder; overload; virtual;
    function excludeFieldsWithoutExposeAnnotation(): CGGIExcluder; overload; virtual;
    function withExclusionStrategy(para1: CGGExclusionStrategy; para2: jboolean; para3: jboolean): CGGIExcluder; overload; virtual;
    function create_(para1: CGGGson; para2: CGGRTypeToken): CGGTypeAdapter; external name 'create'; overload; virtual;
    function excludeField(para1: JLRField; para2: jboolean): jboolean; overload; virtual;
    function excludeClass(para1: JLClass; para2: jboolean): jboolean; overload; virtual;
  strict protected
    function clone(): JLObject; overload; virtual;  // throws java.lang.CloneNotSupportedException
  end;

  CGGIBCollectionTypeAdapterFactory = class sealed external 'com.google.gson.internal.bind' name 'CollectionTypeAdapterFactory' (JLObject, CGGTypeAdapterFactory)
  public
    type
      InnerAdapter = class;
      Arr1InnerAdapter = array of InnerAdapter;
      Arr2InnerAdapter = array of Arr1InnerAdapter;
      Arr3InnerAdapter = array of Arr2InnerAdapter;
      InnerAdapter = class sealed external 'com.google.gson.internal.bind' name 'Adapter' 
      end;

  public
    constructor create(para1: CGGIConstructorConstructor); overload;
    function create_(para1: CGGGson; para2: CGGRTypeToken): CGGTypeAdapter; external name 'create'; overload; virtual;
  end;

  CGGIBJsonAdapterAnnotationTypeAdapterFactory = class sealed external 'com.google.gson.internal.bind' name 'JsonAdapterAnnotationTypeAdapterFactory' (JLObject, CGGTypeAdapterFactory)
  public
    constructor create(para1: CGGIConstructorConstructor); overload;
    function create_(para1: CGGGson; para2: CGGRTypeToken): CGGTypeAdapter; external name 'create'; overload; virtual;
  end;

  CGGIBMapTypeAdapterFactory = class sealed external 'com.google.gson.internal.bind' name 'MapTypeAdapterFactory' (JLObject, CGGTypeAdapterFactory)
  public
    type
      InnerAdapter = class;
      Arr1InnerAdapter = array of InnerAdapter;
      Arr2InnerAdapter = array of Arr1InnerAdapter;
      Arr3InnerAdapter = array of Arr2InnerAdapter;
      InnerAdapter = class sealed external 'com.google.gson.internal.bind' name 'Adapter' 
      end;

  public
    constructor create(para1: CGGIConstructorConstructor; para2: jboolean); overload;
    function create_(para1: CGGGson; para2: CGGRTypeToken): CGGTypeAdapter; external name 'create'; overload; virtual;
  end;

  CGGIBReflectiveTypeAdapterFactory = class sealed external 'com.google.gson.internal.bind' name 'ReflectiveTypeAdapterFactory' (JLObject, CGGTypeAdapterFactory)
  public
    type
      InnerBoundField = class;
      Arr1InnerBoundField = array of InnerBoundField;
      Arr2InnerBoundField = array of Arr1InnerBoundField;
      Arr3InnerBoundField = array of Arr2InnerBoundField;
      InnerBoundField = class abstract external 'com.google.gson.internal.bind' name 'BoundField' 
      end;

  public
    type
      InnerAdapter = class;
      Arr1InnerAdapter = array of InnerAdapter;
      Arr2InnerAdapter = array of Arr1InnerAdapter;
      Arr3InnerAdapter = array of Arr2InnerAdapter;
      InnerAdapter = class sealed external 'com.google.gson.internal.bind' name 'Adapter' (CGGTypeAdapter)
      public
        function read(para1: CGGSJsonReader): JLObject; overload; virtual;  // throws java.io.IOException
        procedure write(para1: CGGSJsonWriter; para2: JLObject); overload; virtual;  // throws java.io.IOException
      end;

  public
    constructor create(para1: CGGIConstructorConstructor; para2: CGGFieldNamingStrategy; para3: CGGIExcluder); overload;
    function excludeField(para1: JLRField; para2: jboolean): jboolean; overload; virtual;
    function create_(para1: CGGGson; para2: CGGRTypeToken): CGGTypeAdapter; external name 'create'; overload; virtual;
  end;

  CGGJsonIOException = class sealed external 'com.google.gson' name 'JsonIOException' (CGGJsonParseException)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  CGGJsonSyntaxException = class sealed external 'com.google.gson' name 'JsonSyntaxException' (CGGJsonParseException)
  public
    constructor create(para1: JLString); overload;
    constructor create(para1: JLString; para2: JLThrowable); overload;
    constructor create(para1: JLThrowable); overload;
  end;

  CGGIBJsonTreeReader = class sealed external 'com.google.gson.internal.bind' name 'JsonTreeReader' (CGGSJsonReader)
  public
    constructor create(para1: CGGJsonElement); overload;
    procedure beginArray(); overload; virtual;  // throws java.io.IOException
    procedure endArray(); overload; virtual;  // throws java.io.IOException
    procedure beginObject(); overload; virtual;  // throws java.io.IOException
    procedure endObject(); overload; virtual;  // throws java.io.IOException
    function hasNext(): jboolean; overload; virtual;  // throws java.io.IOException
    function peek(): CGGSJsonToken; overload; virtual;  // throws java.io.IOException
    function nextName(): JLString; overload; virtual;  // throws java.io.IOException
    function nextString(): JLString; overload; virtual;  // throws java.io.IOException
    function nextBoolean(): jboolean; overload; virtual;  // throws java.io.IOException
    procedure nextNull(); overload; virtual;  // throws java.io.IOException
    function nextDouble(): jdouble; overload; virtual;  // throws java.io.IOException
    function nextLong(): jlong; overload; virtual;  // throws java.io.IOException
    function nextInt(): jint; overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
    procedure skipValue(); overload; virtual;  // throws java.io.IOException
    function toString(): JLString; overload; virtual;
    procedure promoteNameToValue(); overload; virtual;  // throws java.io.IOException
  end;

  CGGIBJsonTreeWriter = class sealed external 'com.google.gson.internal.bind' name 'JsonTreeWriter' (CGGSJsonWriter)
  public
    constructor create(); overload;
    function get(): CGGJsonElement; overload; virtual;
    function beginArray(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function endArray(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function beginObject(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function endObject(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function name(para1: JLString): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: JLString): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function nullValue(): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: jboolean): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: jdouble): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: jlong): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    function value(para1: JLNumber): CGGSJsonWriter; overload; virtual;  // throws java.io.IOException
    procedure flush(); overload; virtual;  // throws java.io.IOException
    procedure close(); overload; virtual;  // throws java.io.IOException
  end;

