PANDROID=/usr/local/pandroid

rm -rf bin
mkdir -p bin
mkdir -p bin/classes

rm lib.jar
zip -rv lib.jar lib  

ant main

rm lib.jar
rm -rf bin


echo ============================ pascal
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $PANDROID/sdk/platforms/android-15/android.jar -classpath dist/PandroidModule.jar -extdirs . -protected zeljus. -o PandroidModule


rm ../android/libs/PandroidModule.jar
rm ../PandroidModule.pas
rm ../PandroidModule.inc
echo ====== rm_jar file

cp dist/PandroidModule.jar ../android/libs/PandroidModule.jar
cp PandroidModule.pas ../PandroidModule.pas
cp PandroidModule.inc ../PandroidModule.inc
echo ====== cp_jar file

echo -------------------------- compile

