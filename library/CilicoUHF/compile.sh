SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon

rm -rf dist
mkdir -p dist
zip -rv dist/CilicoUHF.jar lib com


echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/CilicoUHF.jar -extdirs . -protected com. -o CilicoUHF

echo -------------------------- compile

