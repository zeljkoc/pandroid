{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fcxCharting;

interface

uses
  fcxChart, fcxChartDataManager, fcxChartEditor, fcxcReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('fcxcReg', @fcxcReg.Register);
end;

initialization
  RegisterPackage('fcxCharting', @Register);
end.
