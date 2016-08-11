..\frcc.exe fcxrcStrings.xml
..\frcc.exe fcxrcDesgn.xml
..\frcc.exe fcxrcExports.xml
copy fcxrcStrings.pas ..\..\Source
copy fcxrcDesgn.pas ..\..\Source
copy fcxrcExports.pas ..\..\Source
del *.pas
