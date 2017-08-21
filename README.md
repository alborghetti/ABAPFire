# ABAPFire
ABAP Firebase Client

## Installation ##

Install [abapGit](http://larshp.github.io/abapGit/guide-install.html), then clone the repository in a pacakge (e.g $ABAPFIRE).

## Install certificate ##

### Firebase application certificate ###

From a terminal execute:  

`echo -n | openssl s_client -connect [PROJECT_ID].firebaseio.com:443 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p'`


Save in a file firebase.cer

Import with transaction STRUST 

### Google API certificate ###

For authentication API also Google APIs certificate is required:

`echo -n | openssl s_client -connect www.googleapis.com:443 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p'`
