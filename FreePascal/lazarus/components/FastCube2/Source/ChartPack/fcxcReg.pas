{*******************************************************}
{                                                       }
{         FastCube 2 Chart Registration unit            }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxcReg;
{$INCLUDE fcx.inc}
{$INCLUDE fcxTee.inc}

interface
uses
  {$IFDEF DELPHI_16UP}
    Controls,
  {$ENDIF}
  Classes;

procedure Register;

implementation
{$R *.res}

uses
{$ifdef fpc}
  LazarusPackageIntf,
{$endif}
  fcxChart;

{$IFDEF FPC}
procedure RegisterUnitfcxcReg;
{$ELSE}
procedure Register;
{$ENDIF}
begin
{$IFDEF DELPHI16}
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TBaseChartClass, TControl);
  GroupDescendentsWith(TfcxCustomChart, TControl);
  GroupDescendentsWith(TfcxChart, TControl);
  GroupDescendentsWith(TfcxChartToolBar, TControl);
{$ENDIF}
  RegisterComponents('FastCube 2', [TfcxChart, TfcxChartToolBar]);
end;

{$ifdef FPC}
procedure Register;
begin
  RegisterUnit('fcxcReg', @RegisterUnitfcxcReg);
end;
{$endif}

end.
