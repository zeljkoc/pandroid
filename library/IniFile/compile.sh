SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon

rm -rf bin
mkdir -p bin
mkdir -p bin/classes

rm lib.jar
jar -cvf lib.jar lib 

ant main

rm lib.jar
rm -rf bin

#sleep 10
echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/TIniFile.jar -extdirs . -protected zeljus.com. -o IniFile

echo -------------------------- compile

