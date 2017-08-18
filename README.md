# ABAPFire
ABAP Firebase Client

## Install certificate ##

From a terminal execute:  

_echo -n | openssl s_client -connect [PROJECT_ID].firebaseio.com:443 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p'_

Save in a file certificate.cer

Import with transaction STRUST 
