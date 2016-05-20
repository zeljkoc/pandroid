SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon

echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath achartengine-1.1.0.jar -extdirs . -protected org. -o chart

echo -------------------------- compile

