  ZCPPascalScript = class sealed external 'zeljus.com.Pascal' name 'PascalScript' (JLObject)
  public
    constructor create(); overload;
    class procedure Init(); static; overload;
    class procedure Free(); static; overload;
    class function Execute(para1: JLString): JLString; static; overload;
    class function Message(): JLString; static; overload;
  end;

