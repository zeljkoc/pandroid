#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Assembling pandoidmodule
/usr/local/pandroid/FreePascal/fpc/bin/arm-linux-androideabi-as -march=armv5t -mfpu=softvfp -o /usr/local/pandroid/example/DemoPModule/pascalsrc/jni/lib/arm-android/PandoidModule.o  /usr/local/pandroid/example/DemoPModule/pascalsrc/jni/lib/arm-android/PandoidModule.s
if [ $? != 0 ]; then DoExitAsm pandoidmodule; fi
rm /usr/local/pandroid/example/DemoPModule/pascalsrc/jni/lib/arm-android/PandoidModule.s
echo Assembling project1
/usr/local/pandroid/FreePascal/fpc/bin/arm-linux-androideabi-as -march=armv5t -mfpu=softvfp -o /usr/local/pandroid/example/DemoPModule/pascalsrc/jni/lib/arm-android/project1.o  /usr/local/pandroid/example/DemoPModule/pascalsrc/jni/lib/arm-android/project1.s
if [ $? != 0 ]; then DoExitAsm project1; fi
rm /usr/local/pandroid/example/DemoPModule/pascalsrc/jni/lib/arm-android/project1.s
echo Linking /usr/local/pandroid/example/DemoPModule/pascalsrc/lib/armeabi/libpandroidmodule.so
OFS=$IFS
IFS="
"
/usr/local/pandroid/FreePascal/fpc/bin/arm-linux-androideabi-ld.bfd -z max-page-size=0x1000 -z common-page-size=0x1000 -z noexecstack -z now -s --gc-sections -L. -T /usr/local/pandroid/example/DemoPModule/pascalsrc/lib/armeabi/link.res -o /usr/local/pandroid/example/DemoPModule/pascalsrc/lib/armeabi/libpandroidmodule.so -shared -soname libpandroidmodule.so
if [ $? != 0 ]; then DoExitLink /usr/local/pandroid/example/DemoPModule/pascalsrc/lib/armeabi/libpandroidmodule.so; fi
IFS=$OFS
