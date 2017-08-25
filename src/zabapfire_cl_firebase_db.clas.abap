class ZABAPFIRE_CL_FIREBASE_DB definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF ty_get_parameters,
        shallow        TYPE abap_bool,
        order_by       TYPE string,
        start_at       TYPE string,
        end_at         TYPE string,
        limit_to_first TYPE string,
        limit_to_last  TYPE string,
        equal_to       TYPE string,
      END OF ty_get_parameters .
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
      value(RREF_DB) type ref to ZABAPFIRE_CL_FIREBASE_DB .
  methods GET
    importing
      !PATH type STRING
      !PARAMETERS type TY_GET_PARAMETERS optional
    exporting
      value(CHILD) type ANY
    raising
      ZCX_ABAPFIRE_FIREBASE .
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
      lref_json_deserializer TYPE REF TO zabapfire_cl_json_deserializer,
      l_len                  TYPE i,
      l_query_delimiter      TYPE c LENGTH 1 VALUE '?',
      lcx_json               TYPE REF TO zcx_abapfire_json.

    ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'GET' ).
    application->get_client( )->request->set_content_type('application/json').

    CONCATENATE path '.json' INTO lv_uri.

*   Set parameters
    IF parameters-shallow = abap_true.
      CONCATENATE lv_uri l_query_delimiter 'shallow=true'
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.
    IF NOT parameters-order_by IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'orderBy="' parameters-order_by '"'
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.
    IF NOT parameters-start_at IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'startAt=' parameters-start_at
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.
    IF NOT parameters-end_at IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'endAt=' parameters-end_at
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.
    IF NOT parameters-limit_to_first IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'limitToFirst=' parameters-limit_to_first
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.
    IF NOT parameters-limit_to_last IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'limitToLast=' parameters-limit_to_last
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.
    IF NOT parameters-equal_to IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'equalTo=' parameters-equal_to
        INTO lv_uri.
      l_query_delimiter = '&'.
    ENDIF.

*   Get authentication token
    lv_idtoken = application->auth->get_token( ).

    IF NOT lv_idtoken IS INITIAL.
      CONCATENATE lv_uri l_query_delimiter 'auth=' lv_idtoken
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
      zcx_abapfire_firebase=>raise( 'HTTP Connection fails' ).
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise( 'HTTP Connection fails' ).
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
    TRY.
        CREATE OBJECT lref_json_deserializer.
        CALL METHOD lref_json_deserializer->deserialize
          EXPORTING
            json = lv_response_data
          IMPORTING
            abap = child.
      CATCH zcx_abapfire_json INTO lcx_json.
        zcx_abapfire_firebase=>raise( lcx_json->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
