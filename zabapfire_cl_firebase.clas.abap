CLASS zabapfire_cl_firebase DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_firebase_config,
        apikey            TYPE string,
        authdomain        TYPE string,
        databaseurl       TYPE string,
        projectid         TYPE string,
        storagebucket     TYPE string,
        messagingsenderid TYPE string,
      END OF ty_firebase_config .

    CLASS-METHODS initialize_app
      IMPORTING
        !config              TYPE ty_firebase_config
      RETURNING
        VALUE(rref_firebase) TYPE REF TO zabapfire_cl_firebase .
  PROTECTED SECTION.
private section.

  data MS_CONFIG type TY_FIREBASE_CONFIG .
  class-data SREF_FIREBASE type ref to ZABAPFIRE_CL_FIREBASE .

  methods INIT_CONFIG
    importing
      !CONFIG type TY_FIREBASE_CONFIG .
ENDCLASS.



CLASS ZABAPFIRE_CL_FIREBASE IMPLEMENTATION.


  METHOD initialize_app.

    IF sref_firebase IS INITIAL.
      CREATE OBJECT sref_firebase.

      sref_firebase->init_config( config ).

    ENDIF.

    rref_firebase = sref_firebase.

  ENDMETHOD.


  METHOD init_config.

    ms_config = config.

  ENDMETHOD.
ENDCLASS.
