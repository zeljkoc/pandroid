  ZCPPascalScript = class sealed external 'zeljus.com.Pascal' name 'PascalScript' (JLObject)
  public
    constructor create(); overload;
    class procedure Init(); static; overload;
    class procedure Free(); static; overload;
    class procedure Execute(para1: JLString); static; overload;
    class function Result(): JLString; static; overload;
    class function Message(): JLString; static; overload;
  end;

