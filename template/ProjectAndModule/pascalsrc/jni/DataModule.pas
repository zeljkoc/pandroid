unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ TDataM }

type
  TDataM = class(TDataModule)
  private

  private
    { private declarations }
    procedure SetParameters(Value: String);
  public
    { public declarations }
  published
    property Parameters: String write SetParameters;  
  end;

{var
  DataM: TDataM;  }

implementation

{$R *.lfm}

{ TDataM }

procedure TDataM.SetParameters(Value: String);
var
  parametri: TStringList;
  i: integer;
begin
  parametri:= TStringList.Create;
  try
    parametri.StrictDelimiter := true;
    parametri.Delimiter := ';';    
    parametri.DelimitedText := Value; // Value := 'user=SYSDBA;password=masterkey'

    for i:=0 to parametri.Count -1 do begin
      case parametri.Names[i] of
           '': parametri.ValueFromIndex[i];
      end;
    end;

  finally
    parametri.Free;
  end;

end; 

end.

