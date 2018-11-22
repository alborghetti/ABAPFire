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
  methods REMOVE
    importing
      !PATH type STRING
    raising
      ZCX_ABAPFIRE_FIREBASE .
  methods PUSH
    importing
      !PATH type STRING
      !CHILD type ANY
    returning
      value(KEY) type STRING
    raising
      ZCX_ABAPFIRE_FIREBASE .
  methods SET
    importing
      !PATH type STRING
      !CHILD type ANY
    raising
      ZCX_ABAPFIRE_FIREBASE .
  methods UPDATE
    importing
      !PATH type STRING
      !CHILD type ANY
    raising
      ZCX_ABAPFIRE_FIREBASE .
PROTECTED SECTION.

private section.

  class-data SREF_DB type ref to ZABAPFIRE_CL_FIREBASE_DB .
  data APPLICATION type ref to ZABAPFIRE_CL_FIREBASE .

  methods CONSTRUCTOR
    importing
      !APPLICATION type ref to ZABAPFIRE_CL_FIREBASE .
  methods ADD_PARAMETERS
    importing
      !PARAMETERS type ANY
    changing
      !URI type STRING
      !QUERY_DELIMITER type CHAR1
    raising
      ZCX_ABAPFIRE_FIREBASE .
ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE_DB IMPLEMENTATION.


  METHOD add_parameters.

    DATA:
      l_abap_type       TYPE REF TO cl_abap_typedescr,
      l_abap_stru       TYPE REF TO cl_abap_structdescr,
      lt_components     TYPE cl_abap_structdescr=>component_table,
      l_component       TYPE cl_abap_structdescr=>component,
      l_parameter       TYPE string,
      l_parameter_value TYPE string.
    FIELD-SYMBOLS:
      <comp> TYPE any.

    l_abap_type = cl_abap_typedescr=>describe_by_data( parameters ).
    IF l_abap_type->kind = cl_abap_typedescr=>kind_struct.
      l_abap_stru ?= cl_abap_typedescr=>describe_by_data( parameters ).
      lt_components = l_abap_stru->get_components( ).
      LOOP AT lt_components INTO l_component.
        ASSIGN COMPONENT l_component-name OF STRUCTURE parameters
          TO <comp>.
        IF sy-subrc NE 0.
          zcx_abapfire_firebase=>raise(
           'Wrong URI parameters' ).                        "#EC NOTEXT
        ENDIF.
        CHECK NOT <comp> IS INITIAL.
        l_parameter =
          zabapfire_cl_http_util=>abap_lcc( l_component-name ).
        l_parameter_value =
          zabapfire_cl_http_util=>serialize_value( <comp> ).
        CONCATENATE
          uri
          query_delimiter
          l_parameter
          '='
          l_parameter_value
          INTO uri.
        query_delimiter = '&'.                              "#EC NOTEXT
      ENDLOOP.
    ELSE.
      zcx_abapfire_firebase=>raise(
       'Wrong URI parameters' ).                            "#EC NOTEXT
    ENDIF.

  ENDMETHOD.


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
      lv_response_data       TYPE string,
      lv_http_status         TYPE i,
      lref_json_deserializer TYPE REF TO
                                zabapfire_cl_json_deserializer,
      l_query_delimiter      TYPE c LENGTH 1 VALUE '?',
      lcx_json               TYPE REF TO zcx_abapfire_json.

    ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'GET' ).
    application->get_client( )->request->set_content_type(
                                                  'application/json' ).

    CONCATENATE path '.json' INTO lv_uri.

*   Set parameters
    add_parameters(
      EXPORTING
        parameters = parameters
      CHANGING
        uri = lv_uri
        query_delimiter = l_query_delimiter ).

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
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

    lv_response_data =
      application->get_client( )->response->get_cdata( ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
      IN lv_response_data WITH ''.

*   Check response code
    application->get_client( )->response->get_status(
        IMPORTING
            code = lv_http_status  ).

    CASE lv_http_status.
      WHEN 302.
        zcx_abapfire_firebase=>raise( 'HTTP redirect, check URL' ).
                                                            "#EC NOTEXT
      WHEN 401.
        zcx_abapfire_firebase=>raise( 'HTTP 401, unauthorized' ).
                                                            "#EC NOTEXT
      WHEN 403.
        zcx_abapfire_firebase=>raise( 'HTTP 403, forbidden' ).
                                                            "#EC NOTEXT
      WHEN 404.
        zcx_abapfire_firebase=>raise( 'HTTP 404, not found' ).
                                                            "#EC NOTEXT
      WHEN 415.
        zcx_abapfire_firebase=>raise(
          'HTTP 415, unsupported media type' ).             "#EC NOTEXT

      WHEN 400.
        zcx_abapfire_firebase=>raise(
          zabapfire_cl_http_util=>get_http_error_msg(
            EXPORTING
            code = lv_http_status
            api = 'firebase'
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


  METHOD push.

    DATA:
      BEGIN OF ls_key,
        name TYPE string,
      END OF ls_key,
      lv_uri                 TYPE string,
      lv_idtoken             TYPE string,
      lv_body                TYPE string,
      lv_response_data       TYPE string,
      lv_http_status         TYPE i,
      lref_json_serializer   TYPE REF TO zabapfire_cl_json_serializer,
      lref_json_deserializer TYPE REF TO
                                zabapfire_cl_json_deserializer,
      l_query_delimiter      TYPE c LENGTH 1 VALUE '?',
      lcx_json               TYPE REF TO zcx_abapfire_json.

    ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'POST' ).
    application->get_client( )->request->set_content_type(
                                                  'application/json' ).

    CONCATENATE path '.json' INTO lv_uri.

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

*   Set request body
    TRY.
        CREATE OBJECT lref_json_serializer.
        lv_body = lref_json_serializer->serialize( child ).
      CATCH zcx_abapfire_json INTO lcx_json.
        zcx_abapfire_firebase=>raise( lcx_json->get_text( ) ).
    ENDTRY.

    application->get_client( )->request->set_cdata( lv_body ).

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
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Get response data
    lv_response_data =
      application->get_client( )->response->get_cdata( ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
      IN lv_response_data WITH ''.

*   Check response code
    application->get_client( )->response->get_status(
        IMPORTING
            code = lv_http_status  ).

    CASE lv_http_status.
      WHEN 302.
        zcx_abapfire_firebase=>raise( 'HTTP redirect, check URL' ).
                                                            "#EC NOTEXT
      WHEN 401.
        zcx_abapfire_firebase=>raise( 'HTTP 401, unauthorized' ).
                                                            "#EC NOTEXT
      WHEN 403.
        zcx_abapfire_firebase=>raise( 'HTTP 403, forbidden' ).
                                                            "#EC NOTEXT
      WHEN 404.
        zcx_abapfire_firebase=>raise( 'HTTP 404, not found' ).
                                                            "#EC NOTEXT
      WHEN 415.
        zcx_abapfire_firebase=>raise(
          'HTTP 415, unsupported media type' ).             "#EC NOTEXT

      WHEN 400.
        zcx_abapfire_firebase=>raise(
          zabapfire_cl_http_util=>get_http_error_msg(
            EXPORTING
            code = lv_http_status
            api = 'firebase'
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
            abap = ls_key.
      CATCH zcx_abapfire_json INTO lcx_json.
        zcx_abapfire_firebase=>raise( lcx_json->get_text( ) ).
    ENDTRY.

    key = ls_key-name.

  ENDMETHOD.


  METHOD REMOVE.

    DATA:
      lv_uri                 TYPE string,
      lv_idtoken             TYPE string,
      lv_body                TYPE string,
      lv_response_data       TYPE string,
      lv_http_status         TYPE i,
      lref_json_serializer   TYPE REF TO zabapfire_cl_json_serializer,
      l_query_delimiter      TYPE c LENGTH 1 VALUE '?',
      lcx_json               TYPE REF TO zcx_abapfire_json.

    ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'DELETE' ).

    CONCATENATE path '.json' INTO lv_uri.

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
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Check response code
    application->get_client( )->response->get_status(
        IMPORTING
            code = lv_http_status  ).

    CASE lv_http_status.
      WHEN 302.
        zcx_abapfire_firebase=>raise( 'HTTP redirect, check URL' ).
                                                            "#EC NOTEXT
      WHEN 401.
        zcx_abapfire_firebase=>raise( 'HTTP 401, unauthorized' ).
                                                            "#EC NOTEXT
      WHEN 403.
        zcx_abapfire_firebase=>raise( 'HTTP 403, forbidden' ).
                                                            "#EC NOTEXT
      WHEN 404.
        zcx_abapfire_firebase=>raise( 'HTTP 404, not found' ).
                                                            "#EC NOTEXT
      WHEN 415.
        zcx_abapfire_firebase=>raise(
          'HTTP 415, unsupported media type' ).             "#EC NOTEXT

      WHEN 400.
        lv_response_data =
          application->get_client( )->response->get_cdata( ).
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
          IN lv_response_data WITH ''.
        zcx_abapfire_firebase=>raise(
          zabapfire_cl_http_util=>get_http_error_msg(
            EXPORTING
            code = lv_http_status
            api = 'firebase'
            payload = lv_response_data )
        ).

    ENDCASE.

  ENDMETHOD.


  METHOD set.

    DATA:
      lv_uri                 TYPE string,
      lv_idtoken             TYPE string,
      lv_body                TYPE string,
      lv_response_data       TYPE string,
      lv_http_status         TYPE i,
      lref_json_serializer   TYPE REF TO zabapfire_cl_json_serializer,
      l_query_delimiter      TYPE c LENGTH 1 VALUE '?',
      lcx_json               TYPE REF TO zcx_abapfire_json.

    ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'PUT' ).
    application->get_client( )->request->set_content_type(
                                                  'application/json' ).

    CONCATENATE path '.json' INTO lv_uri.

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

*   Set request body
    TRY.
        CREATE OBJECT lref_json_serializer.
        lv_body = lref_json_serializer->serialize( child ).
      CATCH zcx_abapfire_json INTO lcx_json.
        zcx_abapfire_firebase=>raise( lcx_json->get_text( ) ).
    ENDTRY.

    application->get_client( )->request->set_cdata( lv_body ).

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
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Check response code
    application->get_client( )->response->get_status(
        IMPORTING
            code = lv_http_status  ).

    CASE lv_http_status.
      WHEN 302.
        zcx_abapfire_firebase=>raise( 'HTTP redirect, check URL' ).
                                                            "#EC NOTEXT
      WHEN 401.
        zcx_abapfire_firebase=>raise( 'HTTP 401, unauthorized' ).
                                                            "#EC NOTEXT
      WHEN 403.
        zcx_abapfire_firebase=>raise( 'HTTP 403, forbidden' ).
                                                            "#EC NOTEXT
      WHEN 404.
        zcx_abapfire_firebase=>raise( 'HTTP 404, not found' ).
                                                            "#EC NOTEXT
      WHEN 415.
        zcx_abapfire_firebase=>raise(
          'HTTP 415, unsupported media type' ).             "#EC NOTEXT

      WHEN 400.
        lv_response_data =
          application->get_client( )->response->get_cdata( ).
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
          IN lv_response_data WITH ''.
        zcx_abapfire_firebase=>raise(
          zabapfire_cl_http_util=>get_http_error_msg(
            EXPORTING
            code = lv_http_status
            api = 'firebase'
            payload = lv_response_data )
        ).

    ENDCASE.

  ENDMETHOD.


  METHOD update.

    DATA:
      lv_uri                 TYPE string,
      lv_idtoken             TYPE string,
      lv_body                TYPE string,
      lv_response_data       TYPE string,
      lv_http_status         TYPE i,
      lref_json_serializer   TYPE REF TO zabapfire_cl_json_serializer,
      l_query_delimiter      TYPE c LENGTH 1 VALUE '?',
      lcx_json               TYPE REF TO zcx_abapfire_json.

    ASSERT CONDITION NOT path IS INITIAL.

    application->get_client( )->request->set_method( 'PATCH' ).
    application->get_client( )->request->set_content_type(
                                                  'application/json' ).

    CONCATENATE path '.json' INTO lv_uri.

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

*   Set request body
    TRY.
        CREATE OBJECT lref_json_serializer.
        lv_body = lref_json_serializer->serialize( child ).
      CATCH zcx_abapfire_json INTO lcx_json.
        zcx_abapfire_firebase=>raise( lcx_json->get_text( ) ).
    ENDTRY.

    application->get_client( )->request->set_cdata( lv_body ).

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
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Get response
    application->get_client( )->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
     ).
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise(
        'HTTP Connection fails' ).                          "#EC NOTEXT
    ENDIF.

*   Check response code
    application->get_client( )->response->get_status(
        IMPORTING
            code = lv_http_status  ).

    CASE lv_http_status.
      WHEN 302.
        zcx_abapfire_firebase=>raise( 'HTTP redirect, check URL' ).
                                                            "#EC NOTEXT
      WHEN 401.
        zcx_abapfire_firebase=>raise( 'HTTP 401, unauthorized' ).
                                                            "#EC NOTEXT
      WHEN 403.
        zcx_abapfire_firebase=>raise( 'HTTP 403, forbidden' ).
                                                            "#EC NOTEXT
      WHEN 404.
        zcx_abapfire_firebase=>raise( 'HTTP 404, not found' ).
                                                            "#EC NOTEXT
      WHEN 415.
        zcx_abapfire_firebase=>raise(
          'HTTP 415, unsupported media type' ).             "#EC NOTEXT

      WHEN 400.
        lv_response_data =
          application->get_client( )->response->get_cdata( ).
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
          IN lv_response_data WITH ''.
        zcx_abapfire_firebase=>raise(
          zabapfire_cl_http_util=>get_http_error_msg(
            EXPORTING
            code = lv_http_status
            api = 'firebase'
            payload = lv_response_data )
        ).

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
