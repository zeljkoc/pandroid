{ This file was automatically created by Typhon IDE.
  Do not edit!
  This source is only used to compile and install the package.
 }

unit zc_ibx;

interface

uses
  IBDBReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('IBDBReg', @IBDBReg.Register);
end;

initialization
  RegisterPackage('zc_ibx', @Register);
end.
