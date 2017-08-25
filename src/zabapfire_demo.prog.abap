*&---------------------------------------------------------------------*
*& Report zabapfire_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapfire_demo.
TYPE-POOLS : slis.
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
      MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
        DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.

**********************************************************************
* Get data
**********************************************************************
  TYPES:

    BEGIN OF ty_user,
      name TYPE string,
      role TYPE string,
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
    ls_parameters TYPE  zabapfire_cl_firebase_db=>ty_get_parameters,
    lt_abap       TYPE TABLE OF ty_abap,
    ls_abap       LIKE LINE OF lt_abap,
    ls_user       TYPE ty_user,
    lt_users      TYPE TABLE OF ty_user,
    ls_fc         TYPE slis_fieldcat_alv,
    lt_fc         TYPE slis_t_fieldcat_alv.

*  ls_parameters-order_by = 'role'.
  TRY.
      firebase->db->get(
          EXPORTING
          path =  '/users'
          parameters = ls_parameters
          IMPORTING
          child = lt_abap ).
    CATCH zcx_abapfire_firebase INTO lcx_firebase.
      MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
        DISPLAY LIKE 'E'.
      EXIT.
  ENDTRY.

  LOOP AT lt_abap INTO ls_abap.
    MOVE-CORRESPONDING ls_abap TO ls_user.
    APPEND ls_user TO lt_users.
  ENDLOOP.

  ls_fc-fieldname = 'NAME'.
  ls_fc-seltext_m = 'User name'.
  APPEND ls_fc TO lt_fc.
  ls_fc-fieldname = 'ROLE'.
  ls_fc-seltext_m = 'Role'.
  APPEND ls_fc TO lt_fc.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = lt_fc
    TABLES
      t_outtab      = lt_abap
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
