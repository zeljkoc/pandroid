unit Utils;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

uses
  androidr15;

function checkExistsFile(aFileName: JLString): jboolean;

implementation

function checkExistsFile(aFileName: JLString): jboolean;
begin
  Result := JIFile.Create(aFileName).exists;
end;

end.

