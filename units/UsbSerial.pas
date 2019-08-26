{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
{ Imports for Java packages/classes: com. }
unit UsbSerial;
{$mode delphi}

interface
uses androidr15;

type
  CFUUsbSerialDevice = class;
  Arr1CFUUsbSerialDevice = array of CFUUsbSerialDevice;
  Arr2CFUUsbSerialDevice = array of Arr1CFUUsbSerialDevice;
  Arr3CFUUsbSerialDevice = array of Arr2CFUUsbSerialDevice;

  CFUSerialBuffer = class;
  Arr1CFUSerialBuffer = array of CFUSerialBuffer;
  Arr2CFUSerialBuffer = array of Arr1CFUSerialBuffer;
  Arr3CFUSerialBuffer = array of Arr2CFUSerialBuffer;

  CFUCH34xSerialDevice = class;
  Arr1CFUCH34xSerialDevice = array of CFUCH34xSerialDevice;
  Arr2CFUCH34xSerialDevice = array of Arr1CFUCH34xSerialDevice;
  Arr3CFUCH34xSerialDevice = array of Arr2CFUCH34xSerialDevice;

  CFUSerialOutputStream = class;
  Arr1CFUSerialOutputStream = array of CFUSerialOutputStream;
  Arr2CFUSerialOutputStream = array of Arr1CFUSerialOutputStream;
  Arr3CFUSerialOutputStream = array of Arr2CFUSerialOutputStream;

  CFUCP2102SerialDevice = class;
  Arr1CFUCP2102SerialDevice = array of CFUCP2102SerialDevice;
  Arr2CFUCP2102SerialDevice = array of Arr1CFUCP2102SerialDevice;
  Arr3CFUCP2102SerialDevice = array of Arr2CFUCP2102SerialDevice;

  CFUSerialInputStream = class;
  Arr1CFUSerialInputStream = array of CFUSerialInputStream;
  Arr2CFUSerialInputStream = array of Arr1CFUSerialInputStream;
  Arr3CFUSerialInputStream = array of Arr2CFUSerialInputStream;

  CFUCDCSerialDevice = class;
  Arr1CFUCDCSerialDevice = array of CFUCDCSerialDevice;
  Arr2CFUCDCSerialDevice = array of Arr1CFUCDCSerialDevice;
  Arr3CFUCDCSerialDevice = array of Arr2CFUCDCSerialDevice;

  CFDCP210xIds = class;
  Arr1CFDCP210xIds = array of CFDCP210xIds;
  Arr2CFDCP210xIds = array of Arr1CFDCP210xIds;
  Arr3CFDCP210xIds = array of Arr2CFDCP210xIds;

  CFUUsbSpiDevice = class;
  Arr1CFUUsbSpiDevice = array of CFUUsbSpiDevice;
  Arr2CFUUsbSpiDevice = array of Arr1CFUUsbSpiDevice;
  Arr3CFUUsbSpiDevice = array of Arr2CFUUsbSpiDevice;

  CFUHexData = class;
  Arr1CFUHexData = array of CFUHexData;
  Arr2CFUHexData = array of Arr1CFUHexData;
  Arr3CFUHexData = array of Arr2CFUHexData;

  CFUUsbSerialDebugger = class;
  Arr1CFUUsbSerialDebugger = array of CFUUsbSerialDebugger;
  Arr2CFUUsbSerialDebugger = array of Arr1CFUUsbSerialDebugger;
  Arr3CFUUsbSerialDebugger = array of Arr2CFUUsbSerialDebugger;

  CFUBLED112SerialDevice = class;
  Arr1CFUBLED112SerialDevice = array of CFUBLED112SerialDevice;
  Arr2CFUBLED112SerialDevice = array of Arr1CFUBLED112SerialDevice;
  Arr3CFUBLED112SerialDevice = array of Arr2CFUBLED112SerialDevice;

  CFUPL2303SerialDevice = class;
  Arr1CFUPL2303SerialDevice = array of CFUPL2303SerialDevice;
  Arr2CFUPL2303SerialDevice = array of Arr1CFUPL2303SerialDevice;
  Arr3CFUPL2303SerialDevice = array of Arr2CFUPL2303SerialDevice;

  CFDCH34xIds = class;
  Arr1CFDCH34xIds = array of CFDCH34xIds;
  Arr2CFDCH34xIds = array of Arr1CFDCH34xIds;
  Arr3CFDCH34xIds = array of Arr2CFDCH34xIds;

  CFUBuildConfig = class;
  Arr1CFUBuildConfig = array of CFUBuildConfig;
  Arr2CFUBuildConfig = array of Arr1CFUBuildConfig;
  Arr3CFUBuildConfig = array of Arr2CFUBuildConfig;

  CFUCP2130SpiDevice = class;
  Arr1CFUCP2130SpiDevice = array of CFUCP2130SpiDevice;
  Arr2CFUCP2130SpiDevice = array of Arr1CFUCP2130SpiDevice;
  Arr3CFUCP2130SpiDevice = array of Arr2CFUCP2130SpiDevice;

  CFUXdcVcpSerialDevice = class;
  Arr1CFUXdcVcpSerialDevice = array of CFUXdcVcpSerialDevice;
  Arr2CFUXdcVcpSerialDevice = array of Arr1CFUXdcVcpSerialDevice;
  Arr3CFUXdcVcpSerialDevice = array of Arr2CFUXdcVcpSerialDevice;

  CFUFTDISerialDevice = class;
  Arr1CFUFTDISerialDevice = array of CFUFTDISerialDevice;
  Arr2CFUFTDISerialDevice = array of Arr1CFUFTDISerialDevice;
  Arr3CFUFTDISerialDevice = array of Arr2CFUFTDISerialDevice;

  CFDPL2303Ids = class;
  Arr1CFDPL2303Ids = array of CFDPL2303Ids;
  Arr2CFDPL2303Ids = array of Arr1CFDPL2303Ids;
  Arr3CFDPL2303Ids = array of Arr2CFDPL2303Ids;

  CFDXdcVcpIds = class;
  Arr1CFDXdcVcpIds = array of CFDXdcVcpIds;
  Arr2CFDXdcVcpIds = array of Arr1CFDXdcVcpIds;
  Arr3CFDXdcVcpIds = array of Arr2CFDXdcVcpIds;

  CFDCP2130Ids = class;
  Arr1CFDCP2130Ids = array of CFDCP2130Ids;
  Arr2CFDCP2130Ids = array of Arr1CFDCP2130Ids;
  Arr3CFDCP2130Ids = array of Arr2CFDCP2130Ids;

  CFDFTDISioIds = class;
  Arr1CFDFTDISioIds = array of CFDFTDISioIds;
  Arr2CFDFTDISioIds = array of Arr1CFDFTDISioIds;
  Arr3CFDFTDISioIds = array of Arr2CFDFTDISioIds;

  CFUUsbSerialInterface = interface;
  Arr1CFUUsbSerialInterface = array of CFUUsbSerialInterface;
  Arr2CFUUsbSerialInterface = array of Arr1CFUUsbSerialInterface;
  Arr3CFUUsbSerialInterface = array of Arr2CFUUsbSerialInterface;

  CFUUsbSpiInterface = interface;
  Arr1CFUUsbSpiInterface = array of CFUUsbSpiInterface;
  Arr2CFUUsbSpiInterface = array of Arr1CFUUsbSpiInterface;
  Arr3CFUUsbSpiInterface = array of Arr2CFUUsbSpiInterface;

  CAIUPredicate = interface;
  Arr1CAIUPredicate = array of CAIUPredicate;
  Arr2CAIUPredicate = array of Arr1CAIUPredicate;
  Arr3CAIUPredicate = array of Arr2CAIUPredicate;

  AHUUsbEndpoint = class external 'android.hardware.usb' name 'UsbEndpoint';
  Arr1AHUUsbEndpoint = array of AHUUsbEndpoint;
  Arr2AHUUsbEndpoint = array of Arr1AHUUsbEndpoint;
  Arr3AHUUsbEndpoint = array of Arr2AHUUsbEndpoint;

  AHUUsbDeviceConnection = class external 'android.hardware.usb' name 'UsbDeviceConnection';
  Arr1AHUUsbDeviceConnection = array of AHUUsbDeviceConnection;
  Arr2AHUUsbDeviceConnection = array of Arr1AHUUsbDeviceConnection;
  Arr3AHUUsbDeviceConnection = array of Arr2AHUUsbDeviceConnection;

  JLObject = class external 'java.lang' name 'Object';
  Arr1JLObject = array of JLObject;
  Arr2JLObject = array of Arr1JLObject;
  Arr3JLObject = array of Arr2JLObject;

  JUCArrayBlockingQueue = class external 'java.util.concurrent' name 'ArrayBlockingQueue';
  Arr1JUCArrayBlockingQueue = array of JUCArrayBlockingQueue;
  Arr2JUCArrayBlockingQueue = array of Arr1JUCArrayBlockingQueue;
  Arr3JUCArrayBlockingQueue = array of Arr2JUCArrayBlockingQueue;

  JNByteBuffer = class external 'java.nio' name 'ByteBuffer';
  Arr1JNByteBuffer = array of JNByteBuffer;
  Arr2JNByteBuffer = array of Arr1JNByteBuffer;
  Arr3JNByteBuffer = array of Arr2JNByteBuffer;

  JIInputStream = class external 'java.io' name 'InputStream';
  Arr1JIInputStream = array of JIInputStream;
  Arr2JIInputStream = array of Arr1JIInputStream;
  Arr3JIInputStream = array of Arr2JIInputStream;

  JLString = class external 'java.lang' name 'String';
  Arr1JLString = array of JLString;
  Arr2JLString = array of Arr1JLString;
  Arr3JLString = array of Arr2JLString;

  AHUUsbDevice = class external 'android.hardware.usb' name 'UsbDevice';
  Arr1AHUUsbDevice = array of AHUUsbDevice;
  Arr2AHUUsbDevice = array of Arr1AHUUsbDevice;
  Arr3AHUUsbDevice = array of Arr2AHUUsbDevice;

  AHUUsbRequest = class external 'android.hardware.usb' name 'UsbRequest';
  Arr1AHUUsbRequest = array of AHUUsbRequest;
  Arr2AHUUsbRequest = array of Arr1AHUUsbRequest;
  Arr3AHUUsbRequest = array of Arr2AHUUsbRequest;

  JIOutputStream = class external 'java.io' name 'OutputStream';
  Arr1JIOutputStream = array of JIOutputStream;
  Arr2JIOutputStream = array of Arr1JIOutputStream;
  Arr3JIOutputStream = array of Arr2JIOutputStream;


{$include UsbSerial.inc}

implementation

end.
