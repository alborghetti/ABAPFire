![Tested SAP release: ABAP 740 SP9](https://img.shields.io/badge/ABAP-740%20SP9-green.svg?style=flat)

# ABAPFire #
ABAP Firebase REST API Helper Library

## Installation ##

### Clone it using abapGit ##

Install [abapGit](http://larshp.github.io/abapGit/guide-install.html), then clone the repository in a package (e.g. $ABAPFIRE).

### Install Google certificate ###

Install Root CA from [Google Internet Authority G2](https://pki.google.com/) using transaction STRUST in SSL identity ANONYMOUS.

## Usage ##

Program [ZABAPFIRE_DEMO](src/zabapfire_demo.prog.abap) provides usage examples.

### Initialize the Application ###

To initialize the library, just pass to it your firebase configuration:

```abap
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
  
### User Authentication ###
  
The library support only email and password authentication:
  
```abap
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

```abap
TRY.
    ls_parameters-order_by = 'carrid'.
    ls_parameters-equal_to = 'AC'.
    firebase->db->get(
        EXPORTING
        path =  p_path
        parameters = ls_parameters
        IMPORTING
        child = lt_abap ).
  CATCH zcx_abapfire_firebase INTO lcx_firebase.
    MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
      DISPLAY LIKE 'E'.
    EXIT.
ENDTRY.

```

If your target ABAP structure contains a column with name $KEY, firebase generated unique keys are saved in this column:

```abap
 TYPES:
      BEGIN OF ty_abap,
        $key        TYPE string.
          INCLUDE STRUCTURE sflight.
  TYPES:
     END OF ty_abap
```
![retrieve firebase keys](/docs/img/retrieve_keys.JPG)

For data retrieving, following query parameters are supported:
* **shallow**: Limit the depth of the data returned. If the data at the location is a JSON primitive (string, number, or boolean) its value will simply be returned. If the data snapshot at the location is a JSON object, the values for each key will be truncated to true.
* **orderBy**: Set a sequence order that can be used to filter the data in combination with startAt endAt and equalTo parameters.
* **startAt**: Set an arbitrary starting point.
* **endAt**: Set an arbitrary ending point.
* **equalTo**: Filter on specific value.
* **limitToFirst**: Set a maximum number of children for which to receive data.
* **limitToLast**: Set a maximum number of children for which to receive data in reverse order.

Refer to official [firebase documentation](https://firebase.google.com/docs/database/rest/retrieve-data) for more info.

### Save data to firebase ###

#### Set ####

Writes data to firebase Database location.
This will overwrite any data at this location and all child locations.

```abap
SELECT * FROM sflight
INTO CORRESPONDING FIELDS OF TABLE lt_abap.
TRY.
    firebase->db->set(
      EXPORTING
        path =  p_path
        child = lt_abap ).
  CATCH zcx_abapfire_firebase INTO lcx_firebase.
    MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
      DISPLAY LIKE 'E'.
    EXIT.
ENDTRY.
```

#### Update ####

As opposed to the set( ) method, update( ) can be use to selectively update only the referenced properties at the current location (instead of replacing all the child properties at the current location).


```abap
TRY.
    firebase->db->update(
      EXPORTING
        path =  p_path
        child = lt_abap ).
  CATCH zcx_abapfire_firebase INTO lcx_firebase.
    MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
      DISPLAY LIKE 'E'.
    EXIT.
ENDTRY.
```

#### Push ####

Generates a new child location using a unique key and return the generated unique key.

```abap
TRY.
    LOOP AT lt_abap ASSIGNING <ls_abap>.
      <ls_abap>-$key = firebase->db->push(
         EXPORTING
           path =  p_path
           child = <ls_abap> ).
    ENDLOOP.
  CATCH zcx_abapfire_firebase INTO lcx_firebase.
    MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
      DISPLAY LIKE 'E'.
    EXIT.
ENDTRY.
```

#### Remove ####

Removes the data at this firebase Database location.

```abap
TRY.
    firebase->db->remove(
      EXPORTING
        path =  p_path ).
  CATCH zcx_abapfire_firebase INTO lcx_firebase.
    MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
      DISPLAY LIKE 'E'.
    EXIT.
ENDTRY.
```
