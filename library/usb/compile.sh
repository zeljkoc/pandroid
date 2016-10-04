PANDROID=/usr/local/pandroid
SDK=$PANDROID/sdk



echo ============================ pascal
java -jar $PANDROID/FreePascal/fpc/bin/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/android-google-usb-api10.jar -extdirs . -protected com. -o usbapi10

echo -------------------------- compile

