PANDROID=/usr/local/pandroid
SDK=$PANDROID/sdk



echo ============================ pascal
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/android-support-v4.jar -extdirs . -protected android.support. -o AndroidSupportV4

echo -------------------------- compile

