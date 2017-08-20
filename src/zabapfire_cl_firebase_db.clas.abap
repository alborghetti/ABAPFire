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
      !HTTP_CLIENT type ref to IF_HTTP_CLIENT
    returning
      value(RREF_DB) type ref to ZABAPFIRE_CL_FIREBASE_DB
    raising
      ZCX_ABAPFIRE_FIREBASE .
  PROTECTED SECTION.
private section.

  class-data SREF_DB type ref to ZABAPFIRE_CL_FIREBASE_DB .
  data MREF_HTTP_CLIENT type ref to IF_HTTP_CLIENT .

  methods CONSTRUCTOR
    importing
      !HTTP_CLIENT type ref to IF_HTTP_CLIENT .
ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE_DB IMPLEMENTATION.


METHOD constructor.
  mref_http_client = http_client.

ENDMETHOD.


  METHOD create.

    IF sref_db IS INITIAL.
      CREATE OBJECT sref_db
        EXPORTING
          http_client = http_client.

    ENDIF.

    rref_db = sref_db.

  ENDMETHOD.
ENDCLASS.
