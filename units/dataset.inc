  ZCField = class sealed external 'zeljus.com' name 'Field' (JLObject)
  public
    type
      InnerEditCharCase = class;
      Arr1InnerEditCharCase = array of InnerEditCharCase;
      Arr2InnerEditCharCase = array of Arr1InnerEditCharCase;
      Arr3InnerEditCharCase = array of Arr2InnerEditCharCase;
      InnerFDataType = class;
      Arr1InnerFDataType = array of InnerFDataType;
      Arr2InnerFDataType = array of Arr1InnerFDataType;
      Arr3InnerFDataType = array of Arr2InnerFDataType;
      InnerEditCharCase = class sealed external 'zeljus.com' name 'EditCharCase' (JLEnum)
      public
        final class var
          fLowerCase: ZCField.InnerEditCharCase; external name 'LowerCase';
          fNormal: ZCField.InnerEditCharCase; external name 'Normal';
          fUpperCase: ZCField.InnerEditCharCase; external name 'UpperCase';
      public
        class function values(): ZCField.Arr1InnerEditCharCase; static; overload;
        class function valueOf(para1: JLString): ZCField.InnerEditCharCase; static; overload;
      end;

      InnerFDataType = class sealed external 'zeljus.com' name 'FDataType' (JLEnum)
      public
        final class var
          ftIinteger: ZCField.InnerFDataType; external name 'tIinteger';
          ftFloat: ZCField.InnerFDataType; external name 'tFloat';
          ftString: ZCField.InnerFDataType; external name 'tString';
          ftBlob: ZCField.InnerFDataType; external name 'tBlob';
          ftDate: ZCField.InnerFDataType; external name 'tDate';
          ftTime: ZCField.InnerFDataType; external name 'tTime';
          ftDateTime: ZCField.InnerFDataType; external name 'tDateTime';
      public
        class function values(): ZCField.Arr1InnerFDataType; static; overload;
        class function valueOf(para1: JLString): ZCField.InnerFDataType; static; overload;
      end;

  public
    var
      fFieldNo: jint; external name 'FieldNo';
      fDataType: ZCField.InnerFDataType; external name 'DataType';
      fReadOnly: jboolean; external name 'ReadOnly';
      fVisible: jboolean; external name 'Visible';
      fOldValue: JLString; external name 'OldValue';
      fNewValue: JLString; external name 'NewValue';
      fName: JLString; external name 'Name';
      fDisplayName: JLString; external name 'DisplayName';
      fValue: JLString; external name 'Value';
      fChange: jboolean; external name 'Change';
      fSize: jint; external name 'Size';
      fCharCase: ZCField.InnerEditCharCase; external name 'CharCase';
  public
    constructor create(); overload;
  end;

