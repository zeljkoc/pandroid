{*******************************************************}
{                                                       }
{          FastCube 2 components error unit             }
{                                                       }
{               Copyright (c) 2001-2014                 }
{           by Oleg Pryalkov, Paul Ishenin              }
{                  Fast Reports Inc.                    }
{*******************************************************}

//VCL uses section
{$IFNDEF FMX}
unit fcxError;
{$INCLUDE fcx.inc}

interface
uses SysUtils;
//FMX uses
{$ELSE}
{$INCLUDE fcx.inc}

interface
uses System.SysUtils;
{$ENDIF}

Type
  TfcxError = (
    exfcError,
    exfcCustomCaptionNotSupported,
    exfcGroupNotSupported,
    exfcAddInOtherGroup,
//    exfcSplitNotSupported,
    exfcPathSplitNotSupported,
    exfcSplitPath,
    exfcDateSplitNotSupported,
    exfcTimeSplitNotSupported,
    exfcCustomSplitNotSupported,
    exfcReferenceSplitNotSupported,
    exfcOnlyForLoading,
    exfcNotSupportedDuringLoading,
    exfcTypeCanNotSplit,
    exfcCaptionNotSupported,
    exfcOrderNotSupported,
    exfcTransactionAlreadyStarted
    );

  EfcError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(AErrorCode: Integer; Msg: string);
    property ErrorCode: integer read FErrorCode;
  end;

const
  fcErrorMessages: array[TfcxError] of string = (
    'FastCube error: %s',
    'Custom Caption is not supported',
    'Group is not supported',
    'You can not add a value in the OTHER group',
//    'Splitting is not supported',
    'This is not a splitting path',
    'Path can not be splited',
    'Date splitting is not supported',
    'Time splitting is not supported',
    'Custom spliting is not supported',
    'Reference spliting is not supported',
    'Function is only for data loading',
    'Function is not supported during data loading',
    'Type %s can not be splitted as %s',
    'Caption is not supported',
    'Order is not supported',
    'Transaction is already started'
    );

procedure RaisefcError(ErrMess: TfcxError; const Args: array of const);

implementation

procedure RaisefcError(ErrMess: TfcxError; const Args: array of const);
begin
  raise EfcError.Create(Ord(ErrMess), Format(fcErrorMessages[ErrMess], Args));
end;

{ EfcError }

constructor EfcError.Create(AErrorCode: Integer; Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := AErrorCode;
end;

end.
