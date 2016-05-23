  ZCFUIBDataBase = class sealed external 'zeljus.com.firebird' name 'UIBDataBase' (JLObject)
  public
    constructor create(); overload;
    class procedure Init(para1: JLString; para2: JLString; para3: JLString); static; overload;
    class procedure setUserNamePassword(para1: JLString; para2: JLString); static; overload;
    class function Connect(): JLString; static; overload;
    class procedure Disconnect(); static; overload;
    class function isConnected(): JLBoolean; static; overload;
  end;

  ZCFUIBDataSet = class sealed external 'zeljus.com.firebird' name 'UIBDataSet' (JLObject)
  public
    constructor create(); overload;
    class procedure Init(para1: JLString); static; overload;
    class function Open(): JLString; static; overload;
    class procedure Close(); static; overload;
    class function isActive(): JLBoolean; static; overload;
    class function ExecSQL(): JLString; static; overload;
    class procedure Edit(); static; overload;
    class procedure Post(); static; overload;
    class procedure Next(); static; overload;
    class procedure Prior(); static; overload;
    class function EOF(): JLBoolean; static; overload;
    class function BOF(): JLBoolean; static; overload;
    class procedure First(); static; overload;
    class procedure Last(); static; overload;
    class function getRecNo(): jlong; static; overload;
    class procedure setRecNo(para1: jlong); static; overload;
    class function FieldCount(): jlong; static; overload;
    class function FieldName(para1: jlong): JLString; static; overload;
    class function RecordCount(): jlong; static; overload;
    class function getAsString(para1: jlong): JLString; static; overload;
    class procedure setAsString(para1: jlong; para2: JLString); static; overload;
    class procedure setAsInteger(para1: jlong; para2: jlong); static; overload;
  end;

