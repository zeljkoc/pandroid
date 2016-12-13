unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uib;

{ TDataM }

type
  TDataM = class(TDataModule)
    UIBDataBase: TUIBDataBase;
    UIBTransaction: TUIBTransaction;
  private
    FTest: string;
  private
    function GetDatabaseName: TFilename;
    procedure SetDatabaseName(Value: TFilename);
    { private declarations }
  public
    { public declarations }
  published
    property Test: String read FTest write FTest;
    property DatabaseName: TFilename read GetDatabaseName write SetDatabaseName;
  end;

{var
  DataM: TDataM;  }

implementation

{$R *.lfm}



{ TDataM }

function TDataM.GetDatabaseName: TFilename;
begin
  Result := UIBDataBase.DatabaseName;
end;

procedure TDataM.SetDatabaseName(Value: TFilename);
begin
  UIBDataBase.DatabaseName := Value;
end;

end.

