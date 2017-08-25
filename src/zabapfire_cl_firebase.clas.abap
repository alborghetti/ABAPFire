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
  methods GET_CONFIG
    returning
      value(RS_CONFIG) type TY_FIREBASE_CONFIG .
  methods GET_CLIENT
    returning
      value(RREF_HTTP_CLIENT) type ref to IF_HTTP_CLIENT .
  methods GET_PROXY_CONF
    exporting
      !PROXY type STRING
      !PROXY_PORT type STRING .
protected section.
private section.

  class-data SREF_FIREBASE type ref to ZABAPFIRE_CL_FIREBASE .
  data MV_PROXY type STRING .
  data MV_PROXY_PORT type STRING .
  data MS_CONFIG type TY_FIREBASE_CONFIG .
  data MREF_HTTP_CLIENT type ref to IF_HTTP_CLIENT .

  methods CONSTRUCTOR
    importing
      !CONFIG type TY_FIREBASE_CONFIG optional
      !PROXY type STRING
      !PROXY_PORT type STRING
    raising
      ZCX_ABAPFIRE_FIREBASE .
ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE IMPLEMENTATION.


  METHOD constructor.
    DATA lv_url TYPE string.
    mv_proxy = proxy.
    mv_proxy_port = proxy_port.
*   Create API Url
    ASSERT CONDITION NOT config-projectid IS INITIAL.
    ms_config = config.
    lv_url = ms_config-databaseurl.
*   Create HTTP Client
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
        proxy_host         = mv_proxy
        proxy_service      = mv_proxy_port
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = mref_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      zcx_abapfire_firebase=>raise( 'HTTP Connection fails' ).
    ENDIF.

*   Create DB Helper
    db = zabapfire_cl_firebase_db=>create( me ).
*   Create authentication Helper
    auth = zabapfire_cl_firebase_auth=>create( me ).

  ENDMETHOD.


  METHOD get_client.

    rref_http_client = mref_http_client.

  ENDMETHOD.


  METHOD get_config.

    rs_config = ms_config.

  ENDMETHOD.


  METHOD get_proxy_conf.

    proxy = mv_proxy.
    proxy_port = mv_proxy_port.

  ENDMETHOD.


  METHOD initialize_app.

    IF sref_firebase IS INITIAL.
      CREATE OBJECT sref_firebase
        EXPORTING
          config     = config
          proxy      = proxy
          proxy_port = proxy_port.

    ENDIF.

    rref_firebase = sref_firebase.

  ENDMETHOD.
ENDCLASS.
