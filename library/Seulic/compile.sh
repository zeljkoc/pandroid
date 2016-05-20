SDK=/usr/local/pandroid/sdk
CODETYPHON=/usr/local/codetyphon


echo ============================ pascal
java -jar $CODETYPHON/fpc/fpc64/bin/x86_64-linux/javapp.jar -bootclasspath $SDK/platforms/android-15/android.jar -classpath HFAPI.jar -extdirs . -protected -x com.seuic.rfid.api.model. com. -o HFAPI

echo -------------------------- compile

