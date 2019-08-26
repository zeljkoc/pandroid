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
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $PANDROID/sdk/platforms/android-15/android.jar -classpath dist/ZCBass.jar -extdirs . -protected com.un4seen.bass. -o ZCBass

echo -------------------------- compile

