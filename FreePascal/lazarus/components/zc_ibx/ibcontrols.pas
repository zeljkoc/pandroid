{ This file was automatically created by Typhon IDE.
  Do not edit!
  This source is only used to compile and install the package.
 }

unit ibcontrols;

interface

uses
  DBControlGrid, IBDynamicGrid, IBLookupComboEditBox, IBTreeView, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ibcontrols', @Register);
end.
