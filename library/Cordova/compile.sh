PANDROID=/usr/local/pandroid
SDK=$PANDROID/sdk



echo ============================ pascal
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/cordova-2.4.0.jar -extdirs . -protected org. -o cordova

echo -------------------------- compile

