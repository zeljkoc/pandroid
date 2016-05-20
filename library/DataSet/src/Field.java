package zeljus.com;



final public class Field 
{
 public enum FDataType {tIinteger, tFloat, tString, tBlob, tDate, tTime, tDateTime};
 public enum EditCharCase {LowerCase, Normal, UpperCase};

 public int FieldNo ;
 public FDataType DataType;
 public boolean ReadOnly = false;
 public boolean Visible = true;    
 public String OldValue = "";
 public String NewValue = "";
 public String Name;
 public String DisplayName = "";
 public String Value = "";
 public boolean Change = false;
 public int Size = 10;
 public EditCharCase CharCase;
 
}
