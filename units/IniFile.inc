  ZCTIniFile = class external 'zeljus.com' name 'TIniFile' (JLObject)
  public
    constructor create(para1: JLString); overload;
    function clear(): jboolean; overload; virtual;
    function getBoolean(para1: JLString; para2: JLString; para3: jboolean): jboolean; overload; virtual;
    function getDouble(para1: JLString; para2: JLString; para3: jdouble): jdouble; overload; virtual;
    function getInt(para1: JLString; para2: JLString; para3: jint): jint; overload; virtual;
    function getSection(para1: JLString; para2: JLString): JLString; overload; virtual;
    function getSectionCount(): jint; overload; virtual;
    function getSections(): Arr1JLString; overload; virtual;
    function getString(para1: JLString; para2: JLString; para3: JLString): JLString; overload; virtual;
    function isInSection(para1: JLString; para2: JLString): jboolean; overload; virtual;
    function isSection(para1: JLString): jboolean; overload; virtual;
    function saveFile(): jboolean; overload; virtual;
    function setBoolean(para1: JLString; para2: JLString; para3: jboolean): jboolean; overload; virtual;
    function setDouble(para1: JLString; para2: JLString; para3: jdouble): jboolean; overload; virtual;
    function setInt(para1: JLString; para2: JLString; para3: jint): jboolean; overload; virtual;
    function setString(para1: JLString; para2: JLString; para3: JLString): jboolean; overload; virtual;
  strict protected
    procedure finalize(); overload; virtual;
  end;

