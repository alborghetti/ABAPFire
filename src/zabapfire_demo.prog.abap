*&---------------------------------------------------------------------*
*& Report zabapfire_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapfire_demo.

DATA:
  firebase     TYPE REF TO zabapfire_cl_firebase,
  lcx_firebase TYPE REF TO zcx_abapfire_firebase,
  ls_config    TYPE zabapfire_cl_firebase=>ty_firebase_config.

PARAMETERS:
  p_email TYPE string LOWER CASE,
  p_pass  TYPE string LOWER CASE.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'P_PASS'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.

**********************************************************************
* Initialize App
**********************************************************************
  ls_config-apikey = 'AIzaSyCWOrr40bNNP-7yWDGgdDcpfWHRd7ZGG50'.
  ls_config-authdomain = 'abap-test-825b8.firebaseapp.com'.
  ls_config-databaseurl = 'https://abap-test-825b8.firebaseio.com'.
  ls_config-messagingsenderid = 'abap-test-825b8'.
  ls_config-projectid = 'abap-test-825b8.appspot.com'.
  ls_config-storagebucket = '686267099123'.

  firebase = zabapfire_cl_firebase=>initialize_app( ls_config ).

**********************************************************************
* Authenticate
**********************************************************************
  TRY.
      firebase->auth->authenticate_with_email(
        EXPORTING
        email = p_email
        password = p_pass ).
    CATCH zcx_abapfire_firebase INTO lcx_firebase.
      WRITE lcx_firebase->get_text( ).

  ENDTRY.

**********************************************************************
* Get data
**********************************************************************
  TYPES:

    BEGIN OF ty_user,
      name          TYPE string,
      role          TYPE string,
      first_manager TYPE string,
      weigth        TYPE i,
    END OF ty_user,
    BEGIN OF ty_task,
      description TYPE string,
      status      TYPE string,
      due_on      TYPE timestampl,
    END OF ty_task,
    ty_tasks TYPE SORTED TABLE OF ty_task
      WITH UNIQUE DEFAULT KEY,
    BEGIN OF ty_abap.
          INCLUDE TYPE ty_user.
  TYPES:
    tasks TYPE ty_tasks,
    END OF ty_abap.
  DATA:
          lt_abap TYPE TABLE OF ty_abap.
  TRY.
      firebase->db->get(
          EXPORTING
          path =  '/users'
          IMPORTING
          child = lt_abap ).
    CATCH zcx_abapfire_firebase INTO lcx_firebase.
      WRITE lcx_firebase->get_text( ).
    CATCH zcx_abapfire_json.
      WRITE 'JSON ERROR'.


  ENDTRY.

  READ TABLE lt_abap TRANSPORTING NO FIELDS
  INDEX 1.
