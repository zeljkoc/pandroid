{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 25-11-15 18:04:47
***********************************************************}
unit AZCUHF;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15, CilicoUHF;

type

  { ACilicoUHF }

  ACilicoUHF = class(JLObject)
  public
    type
      InnerDeviceType = class
      const
        CM390 = 0; //default
        CM709 = 1;
        CM719 = 2;
        CM719_V1 = 3;
      end;

      InnerTagType = class
      const
          TAG_TYPE_ALL = 0;
          TAG_TYPE_ISO15693 = 1;
          TAG_TYPE_C1G2 = 2;
          TAG_TYPE_ISO15693_CUSTOM = 3;
          TAG_TYPE_C1G2_CUSTOM = 4;
          TAG_TYPE_ISO14443A_UltraLight = 5;
          TAG_TYPE_ISO14443A_Topaz = 6;
          TAG_TYPE_MAX = 7;
      end;

      InnerBLF = class
      const
          C1G2_BLF_640kbps = 0;
          C1G2_BLF_320kbps = 1;
          C1G2_BLF_200kbps = 2;
          C1G2_BLF_160kbps = 3;
          C1G2_BLF_80kbps = 4;
          C1G2_BLF_Max = 5;
      end;

      InnerMemoryType = class
      const
          C1G2_MEM_TYPE_RESERVED = 0;
          C1G2_MEM_TYPE_EPC = 1;
          C1G2_MEM_TYPE_TID = 2;
          C1G2_MEM_TYPE_USER = 3;
          C1G2_MEM_TYPE_MAX = 4;
      end;

      InnerModulation = class
      const
        C1G2_MOD_FM0 = 0;
        C1G2_MOD_MILLER2 = 1;
        C1G2_MOD_MILLER4 = 2;
        C1G2_MOD_MILLER8 = 3;
        C1G2_MOD_MAX = 4;
      end;

    private
     fBaudrateID: CCUmanager.InnerBaudRateID;
     fIUHFServiceImpl: CWSIUHFServiceImpl;
     fResultInfo:  CWBResultInfo;

     fDeviceType : jint;
     fQValue: jint;
     fTagType: jint;
     fBLF: jint;
     fMemoryType : jint;
     fModulation: jint;

     procedure SetBaudRateID(AValue: CCUmanager.InnerBaudRateID);
     procedure SetDeviceType(aValue: Jint);
     procedure SetQValue(aValue: jint);
     procedure SetTagType(aValue: jint); overload; virtual;
     procedure SetBLF(aValue: jint);
     procedure SetMemoryType(aValue: jint);
     procedure SetModulation(aValue: jint);
   public
     constructor create;  virtual;
     function Connected(versionParam: jint): JLString; virtual;
     procedure disConnected; virtual;

     function ReadTID(Password: String): JLString; virtual;
     function ReadEPCone: JLString; virtual;
     function ChangeEPC(new_epc: JLString; Password: JLString): JLString; virtual;

     function writePassword(oldPAssword: JLString; NewPassword: JLString): JLString; virtual;
     function ResetPassword(oldPAssword: JLString; NewPassword: JLString): JLString; virtual;

     function WriteUser(block_Id: jint; password: JLString; block_Data: JLString): JLString; virtual;
     function ReadUser(block_Id: jint; password: JLString; read_len: jint): JLString; virtual;

     function writeReserve(block_Id: jint; password: JLString; block_Data: JLString): JLString; virtual;
     function readReserve(block_Id: jint; password: JLString): JLString; virtual;

     function lockTag(password: JLString; lock_cmd: JLString): JLString; virtual;
   public
     property DeviceType: jint read fDeviceType write SetDeviceType default InnerDeviceType.CM390;
     property QValue: jint read fQValue  write SetQValue default 0;
     property TagType: jint read fTagType write SetTagType default InnerTagType.TAG_TYPE_C1G2;
     property BLF: jint read fBLF write SetBLF default InnerBLF.C1G2_BLF_160kbps;
     property MemoryType: jint read fMemoryType write SetMemoryType default InnerMemoryType.C1G2_MEM_TYPE_USER;
     property Modulation: jint read fModulation write SetModulation default InnerModulation.C1G2_MOD_MILLER4;
     property BaudRateID: CCUmanager.InnerBaudRateID read fBaudrateID write SetBaudRateID;
  end;

implementation

{ ACilicoUHF }

procedure ACilicoUHF.SetDeviceType(aValue: jint);
begin
   fDeviceType:= aValue;
   fIUHFServiceImpl.SetDeviceType(fDeviceType);
end;

procedure ACilicoUHF.SetBaudRateID(AValue: CCUmanager.InnerBaudRateID);
begin
  if fBaudrateID=AValue then Exit;
  fBaudrateID:=AValue;
end;

procedure ACilicoUHF.SetQValue(aValue: jint);
begin
  fQValue:= aValue;
  fIUHFServiceImpl.setQValue(fQValue);
  {0, 4}
end;

procedure ACilicoUHF.SetTagType(aValue: jint);
var
  i: jint;
begin
  fTagType := aValue;
  fIUHFServiceImpl.setTagType(fTagType);
end;

procedure ACilicoUHF.SetBLF(aValue: jint);
begin
  fBLF := aValue;
  fIUHFServiceImpl.setBLF(fBLF);
end;

procedure ACilicoUHF.SetMemoryType(aValue: jint);
begin
  fMemoryType := aValue;
  fIUHFServiceImpl.setMemoryType(fMemoryType);
end;

procedure ACilicoUHF.SetModulation(aValue: jint);
begin
   fModulation := aValue;
   fIUHFServiceImpl.setModulation(fModulation);
end;

constructor ACilicoUHF.create;
begin
   try
    JLSystem.loadLibrary('cilicoUHFlib');
  except
    exit;
  end;

  inherited;
  fIUHFServiceImpl:= CWSIUHFServiceImpl.Create;
  fResultInfo:=  CWBResultInfo.Create;

  //init
  fIUHFServiceImpl.SetDeviceType(InnerDeviceType.CM390);
  fIUHFServiceImpl.setQValue(0);
  fIUHFServiceImpl.setTagType(InnerTagType.TAG_TYPE_C1G2);
  fIUHFServiceImpl.setBLF(InnerBLF.C1G2_BLF_160kbps);
  fIUHFServiceImpl.setMemoryType(InnerMemoryType.C1G2_MEM_TYPE_USER);
  fIUHFServiceImpl.setModulation(InnerModulation.C1G2_MOD_MILLER4);

  fIUHFServiceImpl.Channel_Calibration(0);
  fIUHFServiceImpl.setC1G2_Tag_Speed;
end;

function ACilicoUHF.Connected(versionParam: jint): JLString;
begin
  fResultInfo := fIUHFServiceImpl.connected(versionParam);
  if fResultInfo.getErrInfo.toString = '' then
    result := fResultInfo.getResultValue
  else
    result := fResultInfo.getErrInfo;
end;

procedure ACilicoUHF.disConnected;
begin
   fIUHFServiceImpl.disConnected;
end;

function ACilicoUHF.ReadTID(Password: String): JLString;
begin
  fResultInfo := fIUHFServiceImpl.readTID(JLSTring(Password));
  if fResultInfo.getErrInfo.toString = '' then
    result := fResultInfo.getResultValue
  else
    result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.ReadEPCone: JLString;
begin
  Result := fIUHFServiceImpl.getEPCOne;
end;

function ACilicoUHF.ChangeEPC(new_epc: JLString; Password: JLString): JLString;
begin
  fResultInfo := fIUHFServiceImpl.changeEPC(new_epc, Password);
  if fResultInfo.getErrInfo.toString = '' then
    result := fResultInfo.getResultValue
  else
    result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.writePassword(oldPAssword: JLString; NewPassword: JLString): JLString;
begin
  fResultInfo := fIUHFServiceImpl.writePassword(OldPassword,  NewPassword);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
     result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.ResetPassword(oldPAssword: JLString; NewPassword: JLString): JLString;
begin
  fResultInfo := fIUHFServiceImpl.ResetPassword(OldPassword,  NewPassword);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
     result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.WriteUser(block_Id: jint; password: JLString; block_Data: JLString): JLString;
begin
  fResultInfo := fIUHFServiceImpl.WriteUser(block_Id, password, block_Data);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
     result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.ReadUser(block_Id: jint; password: JLString; read_len: jint): JLString;
begin
  fResultInfo := fIUHFServiceImpl.ReadUser(block_Id, password, read_len);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
    result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.writeReserve(block_Id: jint; password: JLString; block_Data: JLString): JLString;
begin
  fResultInfo := fIUHFServiceImpl.writeReserve(block_Id, password, block_Data);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
     result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.readReserve(block_Id: jint; password: JLString): JLString;
begin
   fResultInfo := fIUHFServiceImpl.readReserve(block_Id, password);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
     result := fResultInfo.getErrInfo;
end;

function ACilicoUHF.lockTag(password: JLString; lock_cmd: JLString): JLString;
begin
  fResultInfo := fIUHFServiceImpl.lockTag(password, lock_cmd);
  if fResultInfo.getErrInfo.toString = '' then
     result := fResultInfo.getResultValue
  else
     result := fResultInfo.getErrInfo;
end;

end.

