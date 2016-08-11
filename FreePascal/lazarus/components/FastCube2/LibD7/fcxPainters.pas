{*******************************************************}
{                                                       }
{            FastCube 2 base painters unit              }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

unit fcxPainters;
interface
{$INCLUDE fcx.inc}
uses
  Graphics;
  
type
  TfcxPaintStyle = // painting style
  (
    psDefault,  // Use application default
    psStandard, // Use standard style (win95)
    psFlat,     // Use flat style
    psXP        // Use XP style is possible
  );

  TfcxThemeState = // state of theme drawing part
  (
    tsNormal,   // normal state
    tsHot,      // hot
    tsPressed,  // pressed
    tsDisabled  // disabled
  );

  TfcxThemeDirection =
  (
    tdLeft,
    tdRight,
    tdUp,
    tdDown
  );

  TfcxCustomPainter = class
  protected
    function LoadBitmapRes(ResName: String): TBitmap;
  public
    class function GetPaintStyle: TfcxPaintStyle; virtual;
    constructor Create; virtual;
  end;

implementation

{ TfcxCustomPainter }

constructor TfcxCustomPainter.Create;
begin
  // nothing yet
end;

class function TfcxCustomPainter.GetPaintStyle: TfcxPaintStyle;
begin
  Result := psDefault;
end;

function TfcxCustomPainter.LoadBitmapRes(ResName: String): TBitmap;
begin
  Result := TBitmap.Create;
{$IFNDEF FPC}
  Result.LoadFromResourceName(HInstance, ResName);
{$ELSE}
  Result.LoadFromLazarusResource(ResName);
{$ENDIF}
end;

end.
