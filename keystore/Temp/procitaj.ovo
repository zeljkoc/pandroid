keytool

keytool -genkey -dname "cn=Zeljko Cvijanovic, OU=programer, O=ZELJUS, L=Teslic, ST=Teslic, C=RS" -alias zeljus -keyalg RSA -keystore zeljus.keystore -storepass 111111 -keypass 111111 -validity 360 -keysize 2048

keytool -certreq -v -alias zeljus -keypass 111111 -storepass 111111 -keystore zeljus.keystore -file zeljus.csr

keytool -gencert -v -alias zeljus -keypass 111111 -storepass 111111 -keystore zeljus.keystore -infile zeljus.csr -outfile zeljus.crt -ext KeyUsage:critical="digitalSignature,keyEncipherment" -ext EKU="serverAuth" -ext SAN="DNS:zeljus" -rfc

keytool -import -v -alias zeljus -file zeljus.crt -keystore zeljus.keystore -storepass 111111

keytool -importcert -keystore zeljus.keystore -alias zeljus -file zeljus.crt -storepass 111111

keytool -list -v -keystore zeljus.keystore -storepass 111111

keytool -printcert -file zeljus.crt -v



keygen -genseckey {-alias alias} {-keyalg keyalg} {-keysize keysize} [-keypass keypass] {-storetype storetype} {-keystore keystore} [-storepass storepass] {-providerClass provider_class_name {-providerArg provider_arg}} {-v} {-protected} {-Jjavaoption}


http://doc.bccnsoft.com/docs/jdk7-docs/technotes/tools/solaris/jar.html

jar cf myFile.jar *.class