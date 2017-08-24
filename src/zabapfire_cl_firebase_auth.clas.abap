class ZABAPFIRE_CL_FIREBASE_AUTH definition
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
      value(AUTH) type ref to ZABAPFIRE_CL_FIREBASE_AUTH
    raising
      ZCX_ABAPFIRE_FIREBASE .
  methods AUTHENTICATE_WITH_EMAIL
    importing
      !EMAIL type STRING
      !PASSWORD type STRING
    raising
      ZCX_ABAPFIRE_FIREBASE
      ZCX_ABAPFIRE_JSON .
  methods GET_TOKEN
    returning
      value(IDTOKEN) type STRING .
  PROTECTED SECTION.
private section.

  class-data SREF_AUTH type ref to ZABAPFIRE_CL_FIREBASE_AUTH .
  data APPLICATION type ref to ZABAPFIRE_CL_FIREBASE .
  data MV_EMAIL type STRING .
  data MV_PASSWORD type STRING .
  data MV_ID_TOKEN type STRING .
  data MV_REFRESH_TOKEN type STRING .

  methods CONSTRUCTOR
    importing
      !APPLICATION type ref to ZABAPFIRE_CL_FIREBASE .
ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE_AUTH IMPLEMENTATION.


  METHOD authenticate_with_email.

    TYPES:
      BEGIN OF lty_payload,
        email               TYPE string,
        password            TYPE string,
        return_secure_token TYPE abap_bool,
      END OF lty_payload,
      BEGIN OF lty_response_payload,
        kind          TYPE string,
        local_id      TYPE string,
        email         TYPE string,
        display_name  TYPE string,
        id_token      TYPE string,
        registered    TYPE boolean,
        refresh_token TYPE string,
        expires_in    TYPE string,
      END OF lty_response_payload.

    DATA: lv_config              TYPE zabapfire_cl_firebase=>ty_firebase_config,
          lv_proxy               TYPE string,
          lv_proxy_port          TYPE string,
          lv_http_client         TYPE REF TO if_http_client,
          lv_body                TYPE string,
          lv_url                 TYPE string,
          ls_payload             TYPE lty_payload,
          lref_json_serializer   TYPE REF TO zabapfire_cl_json_serializer,
          lv_response_data       TYPE string,
          lref_json_deserializer TYPE REF TO zabapfire_cl_json_deserializer,
          ls_response_abap       TYPE lty_response_payload,
          lv_http_status         TYPE i.

    IF email IS INITIAL OR password IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid = zcx_abapfire_firebase=>incorrect_authentication_param.
    ELSE.
      mv_email = email.
      mv_password = password.
    ENDIF.

*   Get ApyKey from application configuration
    lv_config = application->get_config( ).
    CALL METHOD application->get_proxy_conf
      IMPORTING
        proxy      = lv_proxy
        proxy_port = lv_proxy_port.
    IF lv_config-apikey IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid                     = zcx_abapfire_firebase=>firebase_config
          wrong_firebase_config_parm = 'APIKEY'.
    ELSE.
      lv_url = 'https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key='.
      CONCATENATE lv_url lv_config-apikey
        INTO lv_url.
    ENDIF.

*   Create HTTP Client
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
        proxy_host         = lv_proxy
        proxy_service      = lv_proxy_port
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = lv_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

*   Get authorization token - begin
    lv_http_client->request->set_method( 'POST' ).
    lv_http_client->request->set_content_type('application/json').

*   Build request payload
    ls_payload-email = email.
    ls_payload-password = password.
    ls_payload-return_secure_token = abap_true.
    CREATE OBJECT lref_json_serializer.
    lv_body = lref_json_serializer->serialize( ls_payload ).

    lv_http_client->request->set_cdata( lv_body ).

*   Send request
    CALL METHOD lv_http_client->send
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid = zcx_abapfire_firebase=>http_connection_error.
    ENDIF.

*   Get response
    CALL METHOD lv_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid = zcx_abapfire_firebase=>http_connection_error.
    ENDIF.

    lv_response_data = lv_http_client->response->get_cdata( ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_response_data WITH ''.

*   Check response code
    lv_http_client->response->get_status(
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
            abap = ls_response_abap.
      CATCH zcx_abapfire_json.
        zcx_abapfire_firebase=>raise( 'JSON deserialize error ' ).
    ENDTRY.

*   Store token
    mv_id_token = ls_response_abap-id_token.
    mv_refresh_token = ls_response_abap-refresh_token.

    lv_http_client->close( ).

  ENDMETHOD.


  METHOD constructor.

    me->application = application.

  ENDMETHOD.


  METHOD create.

    IF sref_auth IS INITIAL.
      CREATE OBJECT sref_auth
        EXPORTING
          application = application.

    ENDIF.

    auth = sref_auth.

  ENDMETHOD.


  METHOD get_token.

    idtoken = mv_id_token.

  ENDMETHOD.
ENDCLASS.
