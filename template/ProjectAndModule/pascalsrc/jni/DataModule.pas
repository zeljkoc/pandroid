unit DataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ TDataM }

type
  TDataM = class(TDataModule)
  private
    FTest: string;
  private
    { private declarations }
  public
    { public declarations }
  published
    property Test: String read FTest write FTest;
  end;

{var
  DataM: TDataM;  }

implementation

{$R *.lfm}



end.

