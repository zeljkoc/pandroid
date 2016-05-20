SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon

echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath android-async-http-1.4.3.jar -extdirs . -protected com. -o Http
echo -------------------------- compile

