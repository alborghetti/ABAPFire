class ZABAPFIRE_CL_HTTP_UTIL definition
  public
  final
  create public .

public section.

  class-methods GET_HTTP_ERROR_MSG
    importing
      !CODE type I
      !PAYLOAD type STRING
      !API type STRING
    returning
      value(RV_MSG) type STRING
    raising
      ZCX_ABAPFIRE_FIREBASE .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZABAPFIRE_CL_HTTP_UTIL IMPLEMENTATION.


  METHOD get_http_error_msg.

    TYPES:
      BEGIN OF ty_error_array,
        domain  TYPE string,
        reason  TYPE string,
        message TYPE string,
      END OF ty_error_array,
      BEGIN OF ty_error_google,
        errors  TYPE TABLE OF ty_error_array
          WITH DEFAULT KEY,
        code    TYPE i,
        message TYPE string,
      END OF ty_error_google,
      BEGIN OF ty_response_google,
        error TYPE ty_error_google,
      END OF ty_response_google,
      BEGIN OF ty_response_firebase,
        error TYPE string,
      END OF ty_response_firebase.

    DATA:
      lref_json_deserializer TYPE REF TO zabapfire_cl_json_deserializer,
      ls_error_google        TYPE ty_response_google,
      ls_error_firebase      TYPE ty_response_firebase,
      lv_code                TYPE n LENGTH 4,
      lcx_exception          TYPE REF TO zcx_abapfire_json.

    FIELD-SYMBOLS:
      <ls_error> TYPE any,
      <message>  TYPE any.

    lv_code = code.

    CASE api.
      WHEN 'google'.
        ASSIGN ls_error_google TO <ls_error>.
      WHEN 'firebase'.
        ASSIGN ls_error_firebase TO <ls_error>.
      WHEN 'others'.
        CONCATENATE 'HTTP ERROR:' lv_code INTO rv_msg SEPARATED BY space.
        RETURN.
    ENDCASE.

    TRY.
        CREATE OBJECT lref_json_deserializer.
        CALL METHOD lref_json_deserializer->deserialize
          EXPORTING
            json = payload
          IMPORTING
            abap = <ls_error>.
      CATCH zcx_abapfire_json INTO lcx_exception.
        zcx_abapfire_firebase=>raise( lcx_exception->get_text( ) ).
    ENDTRY.

    CASE api.
      WHEN 'google'.
        ASSIGN COMPONENT 'message' OF STRUCTURE ls_error_google-error
          TO <message>.
      WHEN 'firebase'.
        ASSIGN ls_error_firebase-error TO <message>.
    ENDCASE.

    IF <message> IS ASSIGNED.
      CONCATENATE 'HTTP ERROR:' lv_code <message>
      INTO rv_msg SEPARATED BY space.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
