  ZCJjnireplicate = class sealed external 'zeljus.com.jni' name 'jnireplicate' (JLObject)
  public
    constructor create(); overload;
    class function IndyConnect(para1: JLString; para2: JLString): JLString; static; overload;
    class function IndyConnected(): jboolean; static; overload;
    class function IndyDisconnect(): JLString; static; overload;
    class function SetDataBaseParams(para1: JLString; para2: JLString; para3: JLString; para4: JLString): jint; static; overload;
    class function SendSql(para1: JLString): JLString; static; overload;
    class function GetSql(para1: JLString): JLString; static; overload;
    class function InsertFBTable(para1: JLString; para2: JLString; para3: JLString): JLString; static; overload;
    class function SelectFBTable(para1: JLString; para2: JLString; para3: JLString): JLString; static; overload;
    class function HTTPSOAPPost(para1: JLString; para2: JLString; para3: JLString): JLString; static; overload;
  end;

