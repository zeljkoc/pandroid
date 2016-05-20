/**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************/
package zeljus.com.firebird;


final public class UIBDataSet

{
    static {System.loadLibrary("zcfbclient"); }

    public static native void Init(String SQL);
    public static native String Open();
    public static native void Close();
    public static native Boolean isActive();
    public static native String ExecSQL();

    public static native void Edit();
    public static native void Post();
    public static native void Next();
    public static native void Prior();
    public static native Boolean EOF();
    public static native Boolean BOF();
    public static native void First();
    public static native void Last();
    public static native long getRecNo();
    public static native void setRecNo(long RecNo);

    public static native long FieldCount();
    public static native long RecordCount();
    public static native String getAsString(long FieldNo);
    public static native void setAsString(long FieldNo, String Value);
}
