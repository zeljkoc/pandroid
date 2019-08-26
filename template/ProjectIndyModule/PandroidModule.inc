  ZCPandroidModule = class sealed external 'zeljus.com' name 'PandroidModule' (JLObject)
  public
    constructor create(); overload;
    class function CreateObject(para1: JLString): jlong; static; overload;
    class procedure SetPropValue(para1: jlong; para2: JLString; para3: JLString); static; overload;
    class function GetPropValue(para1: jlong; para2: JLString): JLString; static; overload;
    class procedure SetObjectProp(para1: jlong; para2: JLString; para3: jlong); static; overload;
    class procedure Free(para1: jlong); static; overload;
  end;

