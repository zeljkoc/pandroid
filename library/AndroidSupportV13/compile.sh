PANDROID=/usr/local/pandroid
SDK=$PANDROID/sdk



echo ============================ pascal
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/android-support-v13.jar -extdirs . -protected android.support. -o AndroidSupportV13

echo -------------------------- compile

