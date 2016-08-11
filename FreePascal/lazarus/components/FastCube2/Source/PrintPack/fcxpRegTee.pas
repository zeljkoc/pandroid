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

unit fcxpRegTee;

interface

{$I frx.inc}
{$I fcx.inc}
{$I fcxTee.inc}

procedure Register;

implementation

{$R fcxReg.res}

uses
  Classes, fcxpChartComponents, fcxpChart;

procedure Register;
begin
  RegisterComponents('FastReport 4.0', [TfcxpChartComponents]);
//  RegisterComponents('FastReport 4.0', [TfcxpChartObject]);
  RegisterComponents('FastReport 4.0', [TfcxpChartProvider]);
end;

end.
