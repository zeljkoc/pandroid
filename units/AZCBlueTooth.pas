{**********************************************************
*    Copyright (c) Zeljko Cvijanovic Teslic RS/BiH
*    www.zeljus.com
*    Created by: 25-11-15 18:04:47
***********************************************************}
unit AZCBlueTooth;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units}

interface

uses androidr15;

Type

    { AZCBlueToothPrint }

     AZCBlueToothPrint = class
     public
       fAdapter: ABBluetoothAdapter;
       fSocket: ABBluetoothSocket;
       fDevice: ABBluetoothDevice;
       fOutputStream: JIOutputStream;
       fInputStream:  JIInputStream;

       fContext: ACContext;
      const
       UidString: string = '00001101-0000-1000-8000-00805F9B34FB';
     public
       constructor Create(aContext: ACContext);
       function isConnected: boolean;
       procedure OpenDevice;
       function ConnectToDevice(address: String): boolean;
       procedure CloseDevice;
       function Write(msg: JLString): boolean;
       procedure CloseBT;
    end;

    { AZCBlueToothVaga }

    AZCBlueToothVaga = class(JLThread)
    public
      type
         InnerVagaType = class
         const
          NONE      = 0;
          VILICAR   = 1;
          TALNA     = 2;
          VISECA    = 3;
         end;
    private
      const
        UidString: string = '00001101-0000-1000-8000-00805F9B34FB';
    private
     fAdapter: ABBluetoothAdapter;
     fSocket: ABBluetoothSocket;
     fDevice: ABBluetoothDevice;
     fOutputStream: JIOutputStream;
     fInputStream:  JIInputStream;
     fContext: ACContext;

     fTezina : JLString;
     fVagaType: jint;
     fOstatak: JLstring;
     fDelimeter : string;

     function GetTezina: JLstring;
     procedure SetVagaType(AValue: jint);
     procedure ParseVagaType;
    public
     constructor Create(aContext: ACContext); virtual; overload;
     destructor Destroy; virtual; overload;
     procedure OpenDevice;
     procedure run(); override; overload;
     function isConnected: boolean;
     function ConnectToDevice(address: String): boolean;
   public
     property Tezina: JLstring read GetTezina; // fTezina;
     property VagaType: jint read fVagaType write SetVagaType default InnerVagaType.VISECA;
    end;


implementation

{ AZCBlueToothPrint }

constructor AZCBlueToothPrint.Create(aContext: ACContext);
begin
  inherited Create;
  fContext := aContext;
  fAdapter := ABBluetoothAdapter.getDefaultAdapter;
end;

function AZCBlueToothPrint.isConnected: boolean;
begin
   if fSocket.isConnected then Result := True else Result := false;
end;

procedure AZCBlueToothPrint.OpenDevice;
begin
  fContext.startActivity(ACIntent.create(ABBluetoothAdapter.ACTION_REQUEST_ENABLE));
end;

function AZCBlueToothPrint.ConnectToDevice(address: String): boolean;
begin
  Result := False;
  try
    if (ABBluetoothAdapter.checkBluetoothAddress(address)) then begin
      fDevice := fAdapter.getRemoteDevice(address);

      fSocket := fDevice.createRfcommSocketToServiceRecord(
        JUUUID.fromString(JLString(UidString)));
      fSocket.connect;
      fOutputStream := fSocket.getOutputStream;
      fInputStream := fSocket.getInputStream;
   end;
    Result := True;
  except
  end;
end;

procedure AZCBlueToothPrint.CloseDevice;
begin
  fAdapter.disable;
end;

function AZCBlueToothPrint.Write(msg: JLString): boolean;
begin
  Result := False;
  try
    fOutputStream.write(msg.getBytes);
    Result := True;
  except
  end;
end;

procedure AZCBlueToothPrint.CloseBT;
begin
  try
    fOutputStream.close;
    fInputStream.close;
    fSocket.close;
  except
  end;
end;

{ AZCBlueToothVaga }

constructor AZCBlueToothVaga.Create(aContext: ACContext);
begin
  inherited Create;
  fContext := aContext;
  fAdapter := ABBluetoothAdapter.getDefaultAdapter;
  fVagaType:=InnerVagaType.VISECA;
  fTezina := '';
  fOstatak := '';
  fDelimeter := '';
end;

destructor AZCBlueToothVaga.Destroy;
begin
  fSocket.Close;
end;

function AZCBlueToothVaga.GetTezina: JLstring;
begin
  ParseVagaType;
  Result := fTezina;
  fTezina := '';
end;

procedure AZCBlueToothVaga.SetVagaType(AValue: jint);
begin
   case aValue of
           InnerVagaType.NONE   : fDelimeter := '1';  //(kada nema vage samo radi probe)
           InnerVagaType.TALNA  : fDelimeter := '=';  //(Testirano na talnoj vazi u Sloveniji}
           InnerVagaType.VISECA : fDelimeter := '@';  //(Testirano na vazi do 300 kg)
           InnerVagaType.VILICAR: fDelimeter := 'Net'; //(Rucni Viljuskar vaga )
  end;

  if fVagaType=AValue then Exit;
  fVagaType:=AValue;
end;

procedure AZCBlueToothVaga.ParseVagaType;
var
  i, j: integer;
  msg : array of jbyte;
begin
  case fVagaType of
          InnerVagaType.VILICAR : begin    //tip vilicar
             fTezina := '';
             msg := fOstatak.getBytes;
             for i:=4 to 12 do //.   0  1    2  3    4   5   6   7   8   9
                  if Msg[i] in  [46, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57] then
                    fTezina := fTezina.concat(JLString.create(Msg[i]));
           //  fTezina := fTezina.concat(' | ').concat(fOstatak);
          end;

          InnerVagaType.VISECA : begin   //tip Viseca vaga
            msg := fOstatak.getBytes;
            j := fOstatak.length - 1;
            repeat
              fTezina := '';
              for i:=0 to 8 do //.   0  1    2  3    4   5   6   7   8   9
                  if Msg[i] in  [46, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57] then begin
                    fTezina := fTezina.concat(JLString.create(Msg[i]));
                    j := j - 1;
                  end;
            until j < 8;

          end;

          InnerVagaType.TALNA : begin   //tip Viseca vaga
            msg := fOstatak.getBytes;
            j := fOstatak.length - 1;
            repeat
              fTezina := '';
              for i:=8 Downto 1 do //.   0  1    2  3    4   5   6   7   8   9
                  if Msg[i] in  [46, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57] then begin
                    fTezina := fTezina.concat(JLString.create(Msg[i]));
                    j := j - 1;
                  end;
            until j < 8;
          end;

    end;
end;


procedure AZCBlueToothVaga.OpenDevice;
begin
  fContext.startActivity(ACIntent.create(ABBluetoothAdapter.ACTION_REQUEST_ENABLE));
end;

procedure AZCBlueToothVaga.run;
var
  i : integer;
  tMsg: array[0..1024] of jbyte;
  fReadCount: integer;
begin
 // inherited Run;
  while 1 <> 0 do begin
    fAdapter.cancelDiscovery;
   // fSocket.connect;
    sleep(100);
    try
       if fDelimeter <> '' then begin
          fReadCount :=  fInputStream.Read(tMsg);  //citanje bafera
           case fVagaType of
               InnerVagaType.NONE : fTezina := fTezina.concat(JLString.create(tMsg));
            else
              begin   //8 char
                 for i:=0 to fReadCount - 1 do
                    fOstatak := fOstatak.concat(JLString.create(tMsg[i]));

                 if fOstatak.lastIndexOf(JLString(fDelimeter)) > 0 then
                   fOstatak := JLString.CopyValueOf(fOstatak.toCharArray,
                                                    fOstatak.lastIndexOf(JLString(fDelimeter)) - JLString(fDelimeter).length,
                                                    fOstatak.length - (fOstatak.lastIndexOf(JLString(fDelimeter)) - JLString(fDelimeter).length) );
               end;

           end;
      end;
    except
        fTezina := '111';
    end;

  end;
end;

function AZCBlueToothVaga.isConnected: boolean;
begin
  Result := fSocket.isConnected;
end;

function AZCBlueToothVaga.ConnectToDevice(address: String): boolean;
begin
   Result := False;

   if not fAdapter.isEnabled then  OpenDevice;

   try
      if (ABBluetoothAdapter.checkBluetoothAddress(address)) then begin
        fDevice := fAdapter.getRemoteDevice(address);

        //fSocket := fDevice.createInsecureRfcommSocketToServiceRecord(JUUUID.fromString(JLString(UidString)));
        fSocket := fDevice.createRfcommSocketToServiceRecord(JUUUID.fromString(JLString(UidString)));

        fSocket.connect;

        if fSocket.isConnected then begin
           fOutputStream := fSocket.getOutputStream;
           fInputStream := fSocket.getInputStream;
           Start;
        end;
     end;
     Result := True;
   except
      Result := False;
   end;
end;

end.

