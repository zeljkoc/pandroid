SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon

echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath gson-2.3.1.jar -extdirs . -protected com.google.gson. -o gson

echo -------------------------- compile

