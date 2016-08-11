{  $Id: registersdf.pas 43098 2013-10-05 04:32:53Z paul $  }
{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt
  
  This unit registers the TSDFDataset/TFixedFormatDataset components of the FCL.
}
unit RegisterSDF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, sdfdata, LazarusPackageIntf;

procedure Register;

implementation

{$R registersdf.res}

procedure RegisterUnitSDF;
begin
  RegisterComponents('Data Access',[TSDFDataset,TFixedFormatDataSet]);
end;

procedure Register;
begin
  RegisterUnit('sdfdata',@RegisterUnitSDF);
end;

end.
