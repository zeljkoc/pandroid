{************************************************************************}
{                                                                        }
{       Borland Delphi Visual Component Library                          }
{       InterBase Express core components                                }
{                                                                        }
{       Copyright (c) 1998-2000 Inprise Corporation                      }
{                                                                        }
{    InterBase Express is based in part on the product                   }
{    Free IB Components, written by Gregory H. Deatz for                 }
{    Hoagland, Longo, Moran, Dunst & Doukas Company.                     }
{    Free IB Components is used under license.                           }
{                                                                        }
{    The contents of this file are subject to the InterBase              }
{    Public License Version 1.0 (the "License"); you may not             }
{    use this file except in compliance with the License. You            }
{    may obtain a copy of the License at http://www.Inprise.com/IPL.html }
{    Software distributed under the License is distributed on            }
{    an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{    express or implied. See the License for the specific language       }
{    governing rights and limitations under the License.                 }
{    The Original Code was created by InterBase Software Corporation     }
{       and its successors.                                              }
{    Portions created by Inprise Corporation are Copyright (C) Inprise   }
{       Corporation. All Rights Reserved.                                }
{    Contributor(s): Jeff Overcash                                       }
{                                                                        }
{    IBX For Lazarus (Firebird Express)                                  }
{    Contributor: Tony Whyman, MWA Software http://www.mwasoftware.co.uk }
{    Portions created by MWA Software are copyright McCallum Whyman      }
{    Associates Ltd 2011                                                 }
{************************************************************************}

unit IBDBReg;

{$MODE Delphi}

(*
 * Compiler defines
 *)
{$A+}                           (* Aligned records: On *)
{$B-}                           (* Short circuit boolean expressions: Off *)
{$G+}                           (* Imported data: On *)
{$H+}                           (* Huge Strings: On *)
{$J-}                           (* Modification of Typed Constants: Off *)
{$M+}                           (* Generate run-time type information: On *)
{$O+}                           (* Optimization: On *)
{$Q-}                           (* Overflow checks: Off *)
{$R-}                           (* Range checks: Off *)
{$T+}                           (* Typed address: On *)
{$U+}                           (* Pentim-safe FDIVs: On *)
{$W-}                           (* Always generate stack frames: Off *)
{$X+}                           (* Extended syntax: On *)
{$Z1}                           (* Minimum Enumeration Size: 1 Byte *)

interface

uses SysUtils, Classes, Graphics, Dialogs, Controls, Forms, TypInfo,
      IBTable, IBDatabase,  LazarusPackageIntf,  IBUpdateSQL;




procedure Register;

implementation

uses IB, IBQuery, IBStoredProc, IBCustomDataSet,
     IBIntf, IBSQL, IBSQLMonitor, IBDatabaseInfo, IBEvents,
     IBServices,
     IBBatchMove, IBExtract,LResources,
     LCLVersion, IBDynamicGrid, IBLookupComboEditBox,
     IBTreeView, DBControlGrid, ibxscript;



procedure Register;
begin
{  if not TryIBLoad then
  begin
    MessageDlg('IBX is unable to locate the Firebird Library - have you remembered to install it?',mtError,[mbOK],0);
    Exit;
  end;}

  RegisterNoIcon([TIBStringField, TIBBCDField]);
  {$if lcl_fullversion < 01010000}
  {see http://bugs.freepascal.org/view.php?id=19035 }
  RegisterNoIcon([TIntegerField]);
  {$endif}
  RegisterComponents(IBPalette1, [ TIBQuery, TIBDataSet,
   TIBDatabase, TIBTransaction, TIBUpdateSQL, TIBEvents,
     TIBSQL, TIBDatabaseInfo, TIBSQLMonitor,
       TIBStoredProc,TIBBatchMove,  TIBTable,TIBExtract, TIBXScript]);
 // if IBServiceAPIPresent  then
    RegisterComponents(IBPalette2, [TIBConfigService, TIBBackupService,
      TIBRestoreService, TIBValidationService, TIBStatisticalService,
      TIBLogService, TIBSecurityService, TIBServerProperties]);

  RegisterComponents(IBPalette3,[TIBLookupComboEditBox,TIBDynamicGrid,TIBTreeView,TDBControlGrid]);
end;



initialization
  {$I IBDBReg.lrs}
end.
