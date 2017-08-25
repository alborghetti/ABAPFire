# ABAPFire #
ABAP Firebase REST API Helper Library

## Installation ##

### Install it using abapGit ##

Install [abapGit](http://larshp.github.io/abapGit/guide-install.html), then clone the repository in a pacakge (e.g $ABAPFIRE).

### Install Google certificate ###

Install Root CA from [Google Internet Authority G2](https://pki.google.com/) using transaction STRUST in SSL identity ANONYMOUS.

## Usage ##

Program ZABAPFIRE_DEMO provides usage examples.

### Initialize application ###

To initialize the library, just pass to it your firebase configuration:

```
DATA:
  firebase     TYPE REF TO zabapfire_cl_firebase,
  ls_config    TYPE zabapfire_cl_firebase=>ty_firebase_config.
  
  ls_config-apikey = '[your apikey]'.
  ls_config-authdomain = '[your authdomain]'.
  ls_config-databaseurl = '[your databaseurl]'.
  ls_config-messagingsenderid = '[your messagingsenderid]'.
  ls_config-projectid = '[your projectid]'.
  ls_config-storagebucket = '[your storagebucket]'.

  firebase = zabapfire_cl_firebase=>initialize_app( ls_config ).
  ```
  ### Authenticate user ###
  
  The library support only email and password authentication:
  
  ```
TRY.
      firebase->auth->authenticate_with_email(
        EXPORTING
        email = p_email
        password = p_pass ).
    CATCH zcx_abapfire_firebase INTO lcx_firebase.
      WRITE lcx_firebase->get_text( ).

  ENDTRY.
```
### Retrieve firebase data ###
```
  TRY.
      firebase->db->get(
          EXPORTING
          path =  '/users'
          IMPORTING
          child = lt_abap ).
    CATCH zcx_abapfire_firebase INTO lcx_firebase.
      WRITE lcx_firebase->get_text( ).
  ENDTRY.
```
