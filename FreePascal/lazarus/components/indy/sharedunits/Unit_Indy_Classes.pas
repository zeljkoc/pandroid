unit Unit_Indy_Classes;

interface

uses IdTCPServer, IdThreadSafe, IdCmdTCPServer, StdCtrls, IDGlobal,
  IDSocketHandle, Graphics,
  SysUtils;

type

  TINDYCMD = record
    CMD_CLASS: Integer;
    CMD_VALUE: String[200];
    CMD_TIMESTAMP: TDateTime;
  end;

  TMyRecord = record
    Details: string[255];
    FileName: string[255];
    FileDate: TDateTime;
    FileSize: Integer;
    Recordsize: Integer;
  end;


   TMyRECORDThreadSafeRecord = class(TIdThreadSafe)
          value   : TMyRecord;
  end;


  TINDYCMDThreadSafeRecord = class(TIdThreadSafe)
          value   : TINDYCMD;
  end;


    TBITMAPSafeRecord = class(TIdThreadSafe)
          value   : TBitmap;
  end;



implementation

end.
