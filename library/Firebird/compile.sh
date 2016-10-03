SDK=/usr/local/pandroid/sdk
PANDROID=/usr/local/pandroid

rm -rf bin
mkdir -p bin
mkdir -p bin/classes

rm lib.jar
jar -cvf lib.jar lib 

ant main

rm lib.jar
rm -rf bin


echo ============================ pascal
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/zcfbclient.jar -extdirs . -protected zeljus.com.firebird. -o zcfbclient

echo -------------------------- compile
#sleep 50
