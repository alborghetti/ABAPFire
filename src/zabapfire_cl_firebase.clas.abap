class ZABAPFIRE_CL_FIREBASE definition
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

  data DB type ref to ZABAPFIRE_CL_FIREBASE_DB .
  data AUTH type ref to ZABAPFIRE_CL_FIREBASE_AUTH .

  class-methods INITIALIZE_APP
    importing
      !CONFIG type TY_FIREBASE_CONFIG
      !PROXY type STRING optional
      !PROXY_PORT type STRING optional
    returning
      value(RREF_FIREBASE) type ref to ZABAPFIRE_CL_FIREBASE
    raising
      ZCX_ABAPFIRE_FIREBASE .
  PROTECTED SECTION.
private section.

  class-data SV_PROXY type STRING .
  class-data SV_PROXY_PORT type STRING .
  class-data SREF_FIREBASE type ref to ZABAPFIRE_CL_FIREBASE .
  data MS_CONFIG type TY_FIREBASE_CONFIG .
  data MV_HTTP_CLIENT type ref to IF_HTTP_CLIENT .

  methods CONSTRUCTOR
    importing
      !CONFIG type TY_FIREBASE_CONFIG optional
    raising
      ZCX_ABAPFIRE_FIREBASE .
ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE IMPLEMENTATION.


  METHOD constructor.
    DATA lv_url TYPE string.
*   Create API Url
    ASSERT CONDITION NOT config-projectid IS INITIAL.
    ms_config = config.
    CONCATENATE 'https://' ms_config-projectid '.firebaseio.com'
      INTO lv_url.
*   Create HTTP Client
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
        proxy_host         = sv_proxy
        proxy_service      = sv_proxy_port
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = mv_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapfire_firebase
        EXPORTING
          textid = zcx_abapfire_firebase=>http_connection_error.
    ENDIF.

*   Create DB Helper
    db = zabapfire_cl_firebase_db=>create( mv_http_client ).
*   Create authentication Helper
    auth = zabapfire_cl_firebase_auth=>create( mv_http_client ).

  ENDMETHOD.


  METHOD initialize_app.

    IF sref_firebase IS INITIAL.
      sv_proxy = proxy.
      sv_proxy_port = proxy_port.
      CREATE OBJECT sref_firebase
        EXPORTING
          config = config.

    ENDIF.

    rref_firebase = sref_firebase.

  ENDMETHOD.
ENDCLASS.
