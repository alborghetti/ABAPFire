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
  p_pass  TYPE string LOWER CASE,
  p_path  TYPE  string LOWER CASE DEFAULT '/flights' OBLIGATORY,
  p_get   TYPE char1 RADIOBUTTON GROUP rb1 DEFAULT 'X',
  p_set   TYPE char1 RADIOBUTTON GROUP rb1,
  p_push  TYPE char1 RADIOBUTTON GROUP rb1,
  p_del   TYPE char1 RADIOBUTTON GROUP rb1.

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
      BEGIN OF ty_abap,
        $key        TYPE string.
          INCLUDE STRUCTURE sflight.
  TYPES:
     END OF ty_abap.
  DATA:
    ls_parameters TYPE  zabapfire_cl_firebase_db=>ty_get_parameters,
    lt_abap       TYPE TABLE OF ty_abap,
    ls_fc         TYPE slis_fieldcat_alv,
    lt_fc         TYPE slis_t_fieldcat_alv.

  FIELD-SYMBOLS:
    <ls_abap>     TYPE ty_abap.

  IF p_get = abap_true.
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
  ELSEIF p_del = abap_true.
    TRY.
        firebase->db->remove(
          EXPORTING
            path =  p_path ).
      CATCH zcx_abapfire_firebase INTO lcx_firebase.
        MESSAGE i000(zabapfire_msg) WITH lcx_firebase->get_text( )
          DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.
  ELSE.
    SELECT * FROM sflight
      INTO CORRESPONDING FIELDS OF TABLE lt_abap.
    IF p_set = abap_true.
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
    ELSE.
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
    ENDIF.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'SFLIGHT'
    CHANGING
      ct_fieldcat      = lt_fc.

  ls_fc-fieldname = '$KEY'.
  ls_fc-seltext_m = 'Firebase key'.
  ls_fc-lowercase = abap_true.
  APPEND ls_fc TO lt_fc.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = lt_fc
    TABLES
      t_outtab      = lt_abap
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
