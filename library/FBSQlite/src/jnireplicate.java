package zeljus.com.jni;


final public class jnireplicate

{
   
    static {System.loadLibrary("fbsqliterepl"); }

    public static native String IndyConnect(String host, String port);
    public static native boolean IndyConnected();
    public static native String IndyDisconnect();
    public static native int SetDataBaseParams(String aDataBaseName, String aUserName, String aPassword, String aSQLiteDataBaseName);
    public static native String SendSql(String sql);
    public static native String GetSql(String sql);
    public static native String InsertFBTable(String aSQLiteSelectSQL, String aFBInsertSql, String aFBUpdateSql);
    public static native String SelectFBTable(String aFBSelectSql, String aSQLiteInsertSQL, String aSQLiteUpdateSQL);
    
    public static native String HTTPSOAPPost(String aUrl, String aSOAPAction, String aXml);
    
}
