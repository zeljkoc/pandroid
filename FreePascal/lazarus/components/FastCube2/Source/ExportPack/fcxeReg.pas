{*******************************************************}
{                                                       }
{           FastCube 2 Exports Registration unit        }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxeReg;

{$include fcx.inc}

interface

uses
  Classes
  {$IFNDEF FPC}
    {$IFDEF DELPHI_16UP}
      ,VCL.Controls
    {$ENDIF}
    {$IFNDEF Delphi6}
      ,DsgnIntf
    {$ELSE}
      ,DesignIntf, DesignEditors
    {$ENDIF}
  {$ELSE}
    ,PropEdits, LazarusPackageIntf
  {$ENDIF};

procedure Register;

implementation

{$R *.res}

uses
{$IFDEF DELPHI16}
  fcxCustomExport,
{$ENDIF}
  fcxExportXML,
  fcxExportODF,
  fcxExportBIFF,
  fcxExportHTML,
  fcxExportDBF,
  fcxExportCSV;

{-----------------------------------------------------------------------}

{$IFDEF FPC}
procedure RegisterUnitfcxeReg;
{$ELSE}
procedure Register;
{$ENDIF}
begin
{$IFDEF DELPHI16}
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TfcxCustomExportFilter, TControl);
  GroupDescendentsWith(TfcxHTMLExport, TControl);
  GroupDescendentsWith(TfcxBIFFExport, TControl);
  GroupDescendentsWith(TfcxXMLExport, TControl);
  GroupDescendentsWith(TfcxODSExport, TControl);
  GroupDescendentsWith(TfcxDBFExport, TControl);
  GroupDescendentsWith(TfcxCSVExport, TControl);
{$ENDIF}
  RegisterComponents('FastCube 2 exports',
    [
     TfcxXMLExport,
     TfcxODSExport,
     TfcxBIFFExport,
     TfcxHTMLExport,
     TfcxDBFExport,
     TfcxCSVExport]);
end;

{$ifdef FPC}
procedure Register;
begin
  RegisterUnit('fcxeReg', @RegisterUnitfcxeReg);
end;
{$endif}

end.
