/*
 * TIniFile.java
 * @author Frank Merten
 *
 * Copyright (C) 2010/2011 Frank Merten
 *
 *-----------------------------------------------------------------------
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU Library General Public License as published
 *   by the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Library General Public License for more details.
 *
 *   You should have received a copy of the GNU Library General Public
 *   License along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------
 */

package zeljus.com;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintWriter;


/**
 * This class handles ini-files. It works like this:<BR>
 * You have a combination of section, key and value, it looks like this:<BR>
 *<BR>
 * [Section]<BR>
 * Key=Value<BR>
 *<BR>
 * With set- and get-functions you can write values into and read them from a file,<BR>
 * e.g. 'setInt("Window", "Width", 700);' produces:<BR>
 * <BR>
 * [Window]<BR>
 * Width=700<BR>
 *<BR>
 * The function 'int nValue = getInt("Window", "Width", -1)' will give you 700.<BR>
 * If the combination of key and section delivers no value, the default value, -1 in this example, will be returned.<BR>
 *<BR>
 * The parameter within the constructor is the path to your file, e.g. 'mainFrameInit = new TIniFile("/home/User/Project/MainFrame.ini")' on Linux and 'mainFrameInit = new TIniFile("E:\Data\MainFrame.ini")' on Windows.<BR>
 * When the neccessary destination folders don't exist, they will be created in the saveFile() function.<BR>
 *<BR>
 * There are set- and get-functions for boolean, int, double and String and some other usable functions.<BR>
 *<BR>
 * Thats a simple one, or not? Try and error!<BR>
 *<BR>
 *<BR>
 * ChangeLog<BR>
 * =========<BR>
 *<BR>
 * Version 1.1<BR>
 * June 2011<BR>
 * The function public boolean exists() in the constructor was created to check if the file exists or not.<BR>
 * When the file doesn't exist, it is created.<BR>
 *<BR>
 * <BR>
 * Version 1.2<BR>
 * June 2011<BR>
 * For testing all excptions are printed to console.<BR>
 *<BR>
 *<BR>
 * Version 1.3<BR>
 * July 2011<BR>
 * The function saveFile() was extended. Creating a new file is now placed here and not in the constructor.<BR>
 *<BR>
 *<BR>
 * Version 1.4<BR>
 * November 2011<BR>
 * The use of indexOf("=") was not controlled. When the result was -1, an exception was called and the using function was cancelled.<BR>
 *<BR>
 *<BR>
 * License<BR>
 * =======<BR>
 *   This program is free software; you can redistribute it and/or modify<BR>
 *   it under the terms of the GNU Library General Public License as published<BR>
 *   by the Free Software Foundation; either version 2 of the License, or<BR>
 *   (at your option) any later version.<BR>
 *<BR>
 *   This program is distributed in the hope that it will be useful,<BR>
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of<BR>
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the<BR>
 *   GNU Library General Public License for more details.<BR>
 *<BR>
 *   You should have received a copy of the GNU Library General Public<BR>
 *   License along with this program; if not, write to the Free Software<BR>
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.<BR>
 *<BR>
 *<BR>
*/
public class TIniFile
{
	/**
	 * Input stream
	 */
	private BufferedReader		fileInput;
	
	/**
	 * Output stream
	 */
	private PrintWriter			fileOutput;
	
	/**
	 * The File object
	 */
	private File				file;
	
	/**
	 * Path to the ini-file
	 */
	private String				szFileName;
	
	/**
	 * A single line from the stream
	 */
	private String				szInputValue;
	
	/**
	 * The content of the ini-file
	 */
	private String[]			stringArray;
	
	/**
	 * Number of lines of the ini-file.
	 */
	private int					nLineCount;
	
	/**
	 * Constructor<BR>
	 * Opens the file, initializations.<BR>
	 * @param pFileName The path to the file.
	 */
	public TIniFile(String pFileName)
	{
		int				i;
		
		szFileName = pFileName.replace("\\", "/");
		file = new File(szFileName);
		nLineCount = 0;
		if(file.exists())
		{
			try
			{
				fileInput = new BufferedReader(new FileReader(szFileName));
				while((szInputValue = fileInput.readLine()) != null)
					nLineCount++;
				fileInput.close();
				stringArray = new String[nLineCount];
				fileInput = new BufferedReader(new FileReader(szFileName));
				for(i = 0; i < nLineCount; i++)
				{
					stringArray[i] = fileInput.readLine();
				}
				fileInput.close();
			}
			catch(Exception exception)
			{
				//Log.d("Exception@TIniFile.constructor(): ", exception);
				//Log.d("Exception@TIniFile.constructor(): szFileName = ", szFileName);
			}
		}
	}
	
	/**
	 * Deletes all content from the file and saves it.
	 */
	public boolean clear()
	{
		nLineCount = 0;
		return saveFile();
	}
	
	/**
	 * Gets a boolean value.<BR>
	 * If the combination of pSection and pKey delivers no value, the function will return the pDefaultValue.<BR>
	 * Uses getString().<BR>
	 * @param pSection
	 * @param pKey
	 * @param pDefaultValue
	 * @return The boolean value of pKey in pSection.
	 * @see TIniFile#getString
	 */
	public boolean getBoolean(String pSection, String pKey, boolean pDefaultValue)
	{
		if(getString(pSection, pKey, "").equals("true"))
		{
			return true;
		}
		else
		{
			if(getString(pSection, pKey, "").equals("false"))
			{
				return false;
			}
			else
			{			
				return pDefaultValue;
			}
		}
	}
	
	/**
	 * Gets a double value.<BR>
	 * If the combination of pSection and pKey delivers no value, the function will return pDefaultValue.<BR>
	 * Uses getString().<BR>
	 * @param pSection
	 * @param pKey
	 * @param pDefaultValue
	 * @return The double value of pKey in pSection.
	 * @see TIniFile#getString
	 */
	public double getDouble(String pSection, String pKey, double pDefaultValue)
	{
		try
		{
			return Double.parseDouble(getString(pSection, pKey, ""));
		}
		catch(Exception exception)
		{
			//Log.d("Exception@TIniFile.getDouble(" + pSection + ", " + pKey + ", " + String.valueOf(pDefaultValue) + "): ", exception);
			return pDefaultValue;
		}
	}
	
	/**
	 * Gets an int value.<BR>
	 * If the combination of pSection and pKey delivers no value, the function will return pDefaultValue.<BR>
	 * Uses getString().<BR>
	 * @param pSection
	 * @param pKey
	 * @param pDefaultValue
	 * @return The int value of pKey in pSection.
	 * @see TIniFile#getString
	 */
	public int getInt(String pSection, String pKey, int pDefaultValue)
	{
		try
		{
			return Integer.parseInt(getString(pSection, pKey, ""));
		}
		catch(Exception exception)
		{
			//Log.d("Exception@TIniFile.getInt(" + pSection + ", " + pKey + ", " +  String.valueOf(pDefaultValue) + "): " , exception);
			return pDefaultValue;
		}
	}
	
	/**
	 * Returns the name of the first section with the combination of sKey and sValue.<BR>
	 * If the combination of sKey and sValue can not be found, the function will return an empty String.<BR>
	 * @param pKey
	 * @param pValue
	 * @return The Name of the section.
	 */
	public String getSection(String pKey, String pValue)
	{
		int				i;
		
		i = 0;
		try
		{
			if(nLineCount > 0)
			{
				while(i < nLineCount)
				{
					if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
					{
						szInputValue = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
						i++;
					}
					if(stringArray[i].indexOf("=") >= 0)
					{
						if(stringArray[i].substring(0, stringArray[i].indexOf("=")).trim().toUpperCase().equals(pKey.trim().toUpperCase()))
						{
							if(stringArray[i].toUpperCase().substring(stringArray[i].indexOf("=") + 1, stringArray[i].length()).trim().equals(pValue.toUpperCase().trim()))
							{
								return szInputValue;
							}
						}
					}
					i++;
				}
				return "";
			}
			else
			{
				return "";
			}
		}
		catch(Exception exception)
		{
			//Log.d("Exception@TIniFile.getSection(" + pKey + ", " + pValue + "): " , exception);
			return "";
		}
	}
	
	/**
	 * Gets the number of sections.<BR>
	 * @return Returns the number of sections as int.
	 */
	public int getSectionCount()
	{
		int		i,
				nCounter;
		
		nCounter = 0;
		for(i = 0; i < nLineCount; i++)
		{
			if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
			{
				nCounter++;
			}
		}
		return nCounter;
	}
	
	/**
	 * Returns the names of all sections in an array.<BR>
	 * If there is no section, the function will return an empty array.<BR>
	 * @return An array with the names of all sections.
	 */
	public String[] getSections()
	{
		String[]	sectionArray;
		
		int			i,
					j;
		
		if(nLineCount > 0)
		{
			sectionArray = new String[getSectionCount()];
			i = 0;
			j = 0;
			while(i < nLineCount)
			{
				if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
				{
					sectionArray[j] = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
					j++;
				}
				i++;
			}
			return sectionArray;
		}
		else
		{
			sectionArray = new String[0];
			return sectionArray;
		}
	}
	
	/**
	 * Gets a String value.<BR>
	 * If the combination of pSection and pKey delivers no value, the function will return pDefaultValue.<BR>
	 * @param pSection
	 * @param pKey
	 * @param pDefaultValue
	 * @return The String value of pKey in pSection.
	 */
	public String getString(String pSection, String pKey, String pDefaultValue)
	{
		int				i;
		
		if(nLineCount > 0)
		{
			i = 0;
			while(i < nLineCount)
			{
				if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
				{
					szInputValue = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
					if(szInputValue.toUpperCase().equals(pSection.trim().toUpperCase()))
					{
						i++;
						while((i < nLineCount) && (!stringArray[i].trim().startsWith("[")) && (!stringArray[i].trim().endsWith("]")))
						{
							if(stringArray[i].equals(""))
							{
								i++;
							}
							else
							{
								if(stringArray[i].indexOf("=") >= 0)
								{
									if(stringArray[i].substring(0, stringArray[i].indexOf("=")).trim().toUpperCase().equals(pKey.trim().toUpperCase()))
									{
										return stringArray[i].substring(stringArray[i].indexOf("=") + 1, stringArray[i].length()).trim();
									}
								}
								i++;
							}
						}
					}
				}
				i++;
			}
			return pDefaultValue;
		}
		else
		{
			return pDefaultValue;
		}
	}
	
	/**
	 * If the file contains the combination of pSection and pKey, the function will return true, else false.
	 * @param pSection
	 * @param pKey
	 * @return true or false
	 */
	public boolean isInSection(String pSection, String pKey)
	{
		int				i;
		
		if(nLineCount > 0)
		{
			i = 0;
			while(i < nLineCount)
			{
				if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
				{
					szInputValue = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
					if(szInputValue.toUpperCase().equals(pSection.trim().toUpperCase()))
					{
						i++;
						while((i < nLineCount) &&(!stringArray[i].trim().startsWith("[")) &&(!stringArray[i].trim().endsWith("]")))
						{
							if(stringArray[i].equals(""))
							{
								i++;
							}
							else
							{
								if(stringArray[i].indexOf("=") >= 0)
								{
									if(stringArray[i].substring(0, stringArray[i].indexOf("=")).trim().toUpperCase().equals(pKey.trim().toUpperCase()))
									{
										return true;
									}
								}
								i++;
							}
						}
					}
				}
				i++;
			}
			return false;
		}
		else
		{
			return false;
		}
	}
	
	/**
	 * If the file contains this section, the function will return true, else false.
	 * @param pSection 
	 * @return true or false
	 */
	public boolean isSection(String pSection)
	{
		int				i;
		
		if(nLineCount > 0)
		{
			i = 0;
			while(i < nLineCount)
			{
				if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
				{
					szInputValue = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
					if(szInputValue.toUpperCase().equals(pSection.trim().toUpperCase()))
					{
						return true;
					}
				}
				i++;
			}
		}
		return false;
	}
	
	/**
	 * Saves the file.
	 * @return Returns true when saving was successful, else false.
	 */
	private boolean saveFile()
	{
		int			i;
		
		String		szFolderPath;
		
		File		fFolderFile;
		
		// Checking folders
		if(szFileName.lastIndexOf('/') >= 0)
		{
			szFolderPath = szFileName.substring(0, szFileName.lastIndexOf('/'));
			fFolderFile = new File(szFolderPath);
			if(!fFolderFile.exists())
			{
				if(!fFolderFile.mkdirs())
				{
					//Log.d("Can't create folders: " , szFolderPath);
					//Log.d("fFolderFile.mkdirs() = false!", "");
					return false;
				}
				else
				{
					// ???
				}
			}
			else
			{
				// ???
			}
		}
		else
		{
			// ???
		}
		
		// Writing data
		try
		{
			fileOutput = new PrintWriter(new BufferedWriter(new FileWriter(szFileName)));
			for(i = 0; i < nLineCount; i++)
			{
				fileOutput.println(stringArray[i]);
			}
			fileOutput.close();
			return true;
		}
		catch(Exception exception)
		{
			//Log.d("Exception@TIniFile.saveFile(): " , exception);
			//Log.d("Exception@TIniFile.saveFile(): szFilename = " , szFileName);
			return false;
		}
	}
	
	/**
	 * Sets a boolean value and saves it to file.<BR>
	 * Uses setString().<BR>
	 * @param pSection
	 * @param pKey
	 * @param pValue
	 * @return Returns true when saving was successfully, else false.
	 * @see TIniFile#setString
	 */
	public boolean setBoolean(String pSection, String pKey, boolean pValue)
	{
		if(pValue)
		{
			return setString(pSection, pKey, "true");
		}
		else
		{
			return setString(pSection, pKey, "false");
		}
	}
	
	/**
	 * Sets a double value and saves it to file.<BR>
	 * Uses setString().<BR>
	 * @param pSection
	 * @param pKey
	 * @param pValue
	 * @return Returns true when saving was successfully, else false.
	 * @see TIniFile#setString
	 */
	public boolean setDouble(String pSection, String pKey, double pValue)
	{
		return setString(pSection, pKey, String.valueOf(pValue));
	}
	
	/**
	 * Sets an int value and saves it to file.<BR>
	 * Uses setString().<BR>
	 * @param pSection
	 * @param pKey
	 * @param pValue
	 * @return Returns true when saving was successfully, else false.
	 * @see TIniFile#setString
	 */
	public boolean setInt(String pSection, String pKey, int pValue)
	{
		return setString(pSection, pKey, String.valueOf(pValue));
	}
	
	/**
	 * Sets a String value and saves it to file.
	 * @param pSection
	 * @param pKey
	 * @param pValue
	 * @return Returns true when saving was successfully, else false.
	 */
	public boolean setString(String pSection, String pKey, String pValue)
	{
		String[]	tempArray;
		
		int			i,
					j;
		
		if(pSection.trim().equals("") || pKey.trim().equals(""))
		{
			return false;
		}
		else
		{
			if(isSection(pSection))
			{
				if(isInSection(pSection, pKey))
				{
					i = 0;
					while(i < nLineCount)
					{
						if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
						{
							szInputValue = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
							if(szInputValue.toUpperCase().equals(pSection.trim().toUpperCase()))
							{
								i++;
								while((i < nLineCount) &&(!stringArray[i].trim().startsWith("[")) &&(!stringArray[i].trim().endsWith("]")))
								{
									if(stringArray[i].equals(""))
									{
										i++;
									}
									else
									{
										if(stringArray[i].indexOf("=") >= 0)
										{
											if(stringArray[i].substring(0, stringArray[i].indexOf("=")).trim().toUpperCase().equals(pKey.trim().toUpperCase()))
											{
												stringArray[i] = pKey + "=" + pValue;
												if(saveFile())
													return true;
												else
													return false;
											}
										}
										i++;
									}
								}
							}
						}
						i++;
					}
					return false;
				}
				else
				{
					i = 0;
					while(i < nLineCount)
					{
						if(stringArray[i].trim().startsWith("[") && stringArray[i].trim().endsWith("]"))
						{
							szInputValue = stringArray[i].substring(stringArray[i].indexOf("[") + 1, stringArray[i].indexOf("]")).trim();
							if(szInputValue.toUpperCase().equals(pSection.trim().toUpperCase()))
							{
								i++;
								while(i < nLineCount)
								{
									if((stringArray[i].trim().startsWith("[")) &&(stringArray[i].trim().endsWith("]")))
									{
										tempArray = new String[nLineCount + 1];
										for(j = 0; j < nLineCount; j++)
											tempArray[j] = stringArray[j];
										nLineCount += 1;
										stringArray = new String[nLineCount];
										for(j = 0; j < i; j++)
											stringArray[j] = tempArray[j];
										stringArray[i] = pKey + "=" + pValue;
										for(j = i + 1; j < nLineCount; j++)
											stringArray[j] = tempArray[j - 1];
										if(saveFile())
											return true;
										else
											return false;
									}
									i++;
								}
								tempArray = new String[nLineCount + 1];
								for(i = 0; i < nLineCount; i++)
									tempArray[i] = stringArray[i];
								nLineCount += 1;
								tempArray[nLineCount - 1] = pKey + "=" + pValue;
								stringArray = new String[nLineCount];
								for(i = 0; i < nLineCount; i++)
									stringArray[i] = tempArray[i];
								if(saveFile())
									return true;
								else
									return false;
							}
						}
						i++;
					}
					return false;
				}
			}
			else
			{
				tempArray = new String[nLineCount + 2];
				for(i = 0; i < nLineCount; i++)
					tempArray[i] = stringArray[i];
				nLineCount += 2;
				tempArray[nLineCount - 2] = "[" + pSection + "]";
				tempArray[nLineCount - 1] = pKey + "=" + pValue;
				stringArray = new String[nLineCount];
				for(i = 0; i < nLineCount; i++)
					stringArray[i] = tempArray[i];
				if(saveFile())
					return true;
				else
					return false;
			}
		}
	}
	
	/**
	 * Senseless?
	 */
	protected void finalize()
	{
		
	}
}
