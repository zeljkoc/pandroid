unit IndyReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, LResources;

procedure Register;


implementation
uses IdTCPServer, IdUDPServer, IdCmdTCPServer,

     IdTCPClient, IdUDPClient, IdCmdTCPClient,

     IdCompressorZLib, {IdIntercept, IdIOHandler, IdScheduler,} IdMessage;


procedure Register;
begin
    RegisterComponents('IndyServer',
      [TIdTCPServer, TIdUDPServer, TIdCmdTCPServer]);

    RegisterComponents('IndyClient',
      [TIdTCPClient, TIdUDPClient, TIdCmdTCPClient]);

    RegisterComponents('Indy',
      [TIdCompressorZLib, {TIdServerIntercept, TIdIOHandler, TIdScheduler,} TIdMessage]);
end;

initialization
{$I IdRegisterCore.lrs}
{$I IdRegister.lrs}
{$I IdDsnSASLListEditorFormVCL.lrs}
{$I IdAboutVCL.lrs}

end.

