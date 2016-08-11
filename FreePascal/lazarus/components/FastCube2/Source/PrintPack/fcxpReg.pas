{******************************************}
{                                          }
{             FastReport v4.0              }
{    FastCube 2 components registration    }
{                                          }
{            Copyright (c) 2001-2014       }
{       by Oleg Pryalkov, Paul Ishenin,    }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpReg;

interface

{$I frx.inc}
{$I fcx.inc}

procedure Register;

implementation

{$R fcxpReg.res}

uses
  Classes, fcxpComponents, fcxpSliceGridReport;

procedure Register;
begin
  RegisterComponents('FastReport 4.0', [TfcxpComponents]);
  RegisterComponents('FastCube 2', [TfcxpSliceGridReport]);
  RegisterComponents('FastReport 4.0', [TfcxpSliceGridProvider]);
end;

end.
