SDK=/usr/local/pandroid/sdk
FPC=/usr/local/pandroid/FreePascal

rm -rf bin
rm -rf dist

mkdir -p bin
mkdir -p bin/classes

# rm lib.jar
# zip -rv lib.jar lib  

ant main

# rm lib.jar
rm -rf bin


echo ============================ pascal
java -jar $FPC/fpc/bin/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/obd2fpc.jar -extdirs . -protected com. -o obd2fpc

echo -------------------------- compile
#sleep 50
