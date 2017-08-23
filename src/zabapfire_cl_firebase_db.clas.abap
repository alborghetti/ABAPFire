class ZABAPFIRE_CL_FIREBASE_DB definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF ty_firebase_config,
        apikey            TYPE string,
        authdomain        TYPE string,
        databaseurl       TYPE string,
        projectid         TYPE string,
        storagebucket     TYPE string,
        messagingsenderid TYPE string,
      END OF ty_firebase_config .

  class-methods CREATE
    importing
      !APPLICATION type ref to ZABAPFIRE_CL_FIREBASE
    returning
      value(RREF_DB) type ref to ZABAPFIRE_CL_FIREBASE_DB
    raising
      ZCX_ABAPFIRE_FIREBASE .
  methods GET
    importing
      !PATH type STRING
    exporting
      value(CHILD) type ANY
    raising
      ZCX_ABAPFIRE_FIREBASE
      ZCX_ABAPFIRE_JSON .
PROTECTED SECTION.

PRIVATE SECTION.

  CLASS-DATA sref_db TYPE REF TO zabapfire_cl_firebase_db .
  DATA application TYPE REF TO zabapfire_cl_firebase .

  METHODS constructor
    IMPORTING
      !application TYPE REF TO zabapfire_cl_firebase .

ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE_DB IMPLEMENTATION.


METHOD constructor.
  me->application = application.

ENDMETHOD.


  METHOD create.

    IF sref_db IS INITIAL.
      CREATE OBJECT sref_db
        EXPORTING
          application = application.

    ENDIF.

    rref_db = sref_db.

  ENDMETHOD.


  METHOD get.

  DATA:
          lv_uri                 TYPE string,
          lv_idtoken             TYPE string,
          ls_payload             TYPE string,
          lv_response_data       TYPE string,
          lv_http_status         TYPE i,
          lref_json_deserializer TYPE REF TO zabapfire_cl_json_deserializer.

  ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'GET' ).
    application->get_client( )->request->set_content_type('application/json').

*   Get authentication token
    lv_idtoken = application->auth->get_token( ).

    IF NOT lv_idtoken IS INITIAL.
      CONCATENATE path '.json?auth=' lv_idtoken
        INTO lv_uri.
        ELSE.
      CONCATENATE path '.json'
        INTO lv_uri.
    ENDIF.

*   Set URI
    cl_http_utility=>set_request_uri(
        EXPORTING
          request = application->get_client( )->request
          uri     = lv_uri
      ).

*   Send request
    application->get_client( )->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
        ).
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid = zcx_abapfire_firebase=>http_connection_error.
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid = zcx_abapfire_firebase=>http_connection_error.
    ENDIF.

    lv_response_data = application->get_client( )->response->get_cdata( ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_response_data WITH ''.

*   Check response code
    application->get_client( )->response->get_status(
        IMPORTING
            code = lv_http_status  ).

    CASE lv_http_status.
      WHEN 302.
        zcx_abapfire_firebase=>raise( 'HTTP redirect, check URL' ).
      WHEN 401.
        zcx_abapfire_firebase=>raise( 'HTTP 401, unauthorized' ).
      WHEN 403.
        zcx_abapfire_firebase=>raise( 'HTTP 403, forbidden' ).
      WHEN 404.
        zcx_abapfire_firebase=>raise( 'HTTP 404, not found' ).
      WHEN 415.
        zcx_abapfire_firebase=>raise( 'HTTP 415, unsupported media type' ).
      WHEN 400.
        zcx_abapfire_firebase=>raise(
          zabapfire_cl_http_util=>get_http_error_msg(
            EXPORTING
            code = lv_http_status
            payload = lv_response_data )
        ).

    ENDCASE.

*   Deserialize JSON Payload
    CREATE OBJECT lref_json_deserializer.
    CALL METHOD lref_json_deserializer->deserialize
      EXPORTING
        json = lv_response_data
      IMPORTING
        abap = child.

  ENDMETHOD.
ENDCLASS.
