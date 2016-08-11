{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fcxExport;

interface

uses
  fcxBIFF, fcxCBFF, fcxCrypto, fcxEscher, fcxExportBIFF, fcxExportHTML, 
  fcxExportODF, fcxExportXML, fcxGZip, fcxOLEPS, fcxStorage, fcxZip, 
  fcxExportDBF, fcxeReg, fcxExportCSV, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fcxeReg', @fcxeReg.Register);
end;

initialization
  RegisterPackage('fcxExport', @Register);
end.
