{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IndyPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  IndyReg, IdTCPServer, IdTCPClient, IdCompressorZLib, IdContext, IdUDPServer, IdUDPClient, IdIntercept, IdIOHandler, 
  IdCmdTCPServer, IdCmdTCPClient, IdMessage, IdGlobal, IdGlobalCore, IdGlobalProtocols, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IndyReg', @IndyReg.Register);
end;

initialization
  RegisterPackage('IndyPackage', @Register);
end.
