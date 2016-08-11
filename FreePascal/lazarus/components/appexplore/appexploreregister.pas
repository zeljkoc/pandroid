unit AppExploreRegister;

{$mode delphi}

interface

uses
  MenuIntf, AppExploreFrm;


procedure Register;

implementation

procedure AppExplorerItemOnClick(Sender: TObject);
begin
  ShowAppExplorer;
end;

procedure Register;
begin
  RegisterIDEMenuCommand(itmInfoHelps, 'AppExplorerItem', 'Application Explorer',
    nil, AppExplorerItemOnClick, nil, 'menu_information');
end;

end.
