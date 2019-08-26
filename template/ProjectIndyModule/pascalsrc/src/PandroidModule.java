/***********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************/
package zeljus.com;


final public class PandroidModule {
    static {System.loadLibrary("pandroidmodule"); }

    public static native long CreateObject(String AClassName);
    public static native void SetPropValue(long AID, String AProperty, String AValue);
    public static native String GetPropValue(long AID, String AProperty);
    public static native void SetObjectProp(long AID, String AProperty, long AIDObject);

    public static native void Free(long AID);
}
