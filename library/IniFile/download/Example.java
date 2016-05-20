/*
 * Example.java
 * Author: Frank Merten
 */

public class Example
{
	/**
	 * @param args Not used
	 */
	public static void main(String[] args)
	{
		TIniFile	testInit;
		
		String		szValue;
		
		/*
		 * An example for the usage of TIniFile.java
		 * =========================================");
		 * 
		 * Definition:
		 * [Section]
		 * Key=Value
		 * 
		 * Constructor:
		 */
		
		testInit = new TIniFile("./Project/Settings/Ressources.settings");
		
		/*
		 * The param within the constructor is the path to the file.
		 * The appendix '.ini' is NOT mandatory, use whatever you like.
		 * When the file exists the data will be read, else a new file will be created.
		 * When the needed directories don't exist they will be created too.
		 */
		
		/*
		 * Setting int values:
		 */
		
		testInit.setInt("Window", "Left", 0);
		testInit.setInt("Window", "Top", 0);
		testInit.setInt("Window", "Width", 800);
		testInit.setInt("Window", "Height", 600);
		
		/*
		 * Setting String values:
		 */
		
		testInit.setString("User", "Login", "userX");
		testInit.setString("User", "Name", "Tom");
		testInit.setString("User", "First name", "Mallone");

		/*
		 * Setting double values:
		 */

		testInit.setDouble("Math", "Pi", 3.14159265358979323);
		testInit.setDouble("Math", "e", 2.718281828459045235);

		/*
		 * Setting boolean values:
		 */

		testInit.setBoolean("Universe", "Infinity", true);
		testInit.setBoolean("Window", "Visible", true);
		
		/*
		 * All setting functions return a boolean value to control wether the writing was successful or not.
		 */
		
		if(testInit.setString("Font", "Name", "Helvetica"))
		{
			// ToDo
		}
		else
		{
			// ToDo
		}
		
		/*
		 * Existing values are overwritten:
		 */
		
		testInit.setInt("Result", "Value", 0);
		testInit.setInt("Result", "Value", -1);
		
		/*
		 * Reading a String value:
		 */
		
		szValue = testInit.getString("Font", "Name", "unknown");
		System.out.println("Font Name = " + szValue);

		/*
		 * TIniFile is NOT case sensitive:
		 */
		
		szValue = testInit.getString("font", "name", "unknown");
		System.out.println("font name = " + szValue);

		/*
		 * Reading an int value, the TIniFile can be integrated:
		 */
		
		System.out.println("The width of the window is " + testInit.getInt("Window", "Width", -1) + " pixel.");
		
		/*
		 * When the combination of 'Section' and 'Key' doesn't exist the default value will be returned: 
		 */
		
		System.out.println("User password = " + testInit.getString("User", "Password", "unknown"));
		
		/*
		 * Reading a boolean value:
		 */
		
		if(testInit.getBoolean("Universe", "Infinity", false))
		{
			System.out.println("The universe is endless!");
		}
		else
		{
			System.out.println("The universe is a nutshell!");
		}
		
		/*
		 * There are some other useful functions,
		 * e.g String[] getSections() delivers an array with all sections
		 * and int getSectionCount() delivers the number of section
		 */
		
		String[] sections = testInit.getSections();
		System.out.println("Sections: ");
		for(int i = 0; i < sections.length; i++)
		{
			System.out.println("Section " + i + " = " + sections[i]);
		}
		System.out.println("Total: " + testInit.getSectionCount() + " sections");
	}

}

