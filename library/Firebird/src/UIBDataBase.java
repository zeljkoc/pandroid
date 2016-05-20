/***********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************/
package zeljus.com.firebird;


final public class UIBDataBase

{
    static {System.loadLibrary("zcfbclient"); }

    public static native void Init(String DataBaseName, String CharSet, String LibraryName);
    public static native void setUserNamePassword(String UserName, String Password);
    public static native String Connect();
    public static native void Disconnect();
    public static native Boolean isConnected();
}
