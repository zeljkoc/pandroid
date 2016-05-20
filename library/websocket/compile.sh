SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon

echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath dist/java-websocket-1.3.0.jar -extdirs . -protected org. -o WebSocket

echo -------------------------- compile

