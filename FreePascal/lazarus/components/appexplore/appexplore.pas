{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit appexplore;

interface

uses
  AppExploreRegister, AppExploreFrm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AppExploreRegister', @AppExploreRegister.Register);
end;

initialization
  RegisterPackage('appexplore', @Register);
end.
