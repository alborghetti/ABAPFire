class ZABAPFIRE_CL_HTTP_UTIL definition
  public
  final
  create public .

public section.

  class-methods GET_HTTP_ERROR_MSG
    importing
      !CODE type I
      !PAYLOAD type STRING
    returning
      value(RV_MSG) type STRING
    raising
      ZCX_ABAPFIRE_FIREBASE .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZABAPFIRE_CL_HTTP_UTIL IMPLEMENTATION.


  METHOD get_http_error_msg.

    DATA:
          lcx_exception TYPE REF TO zcx_abapfire_json.

    TYPES:
      BEGIN OF ty_error_array,
        domain  TYPE string,
        reason  TYPE string,
        message TYPE string,
      END OF ty_error_array,
      BEGIN OF ty_error,
        errors  TYPE TABLE OF ty_error_array
          WITH DEFAULT KEY,
        code    TYPE i,
        message TYPE string,
      END OF ty_error,
      BEGIN OF ty_response_payload,
        error TYPE ty_error,
      END OF ty_response_payload.

    DATA:
      lref_json_deserializer TYPE REF TO zabapfire_cl_json_deserializer,
      lt_response_abap       TYPE ty_response_payload,
      lv_code                TYPE n LENGTH 4.

    TRY.
        CREATE OBJECT lref_json_deserializer.
        CALL METHOD lref_json_deserializer->deserialize
          EXPORTING
            json = payload
          IMPORTING
            abap = lt_response_abap.
      CATCH zcx_abapfire_json INTO lcx_exception.
        zcx_abapfire_firebase=>raise( lcx_exception->get_text( ) ).
    ENDTRY.

    rv_msg = lt_response_abap-error-message.
    lv_code = code.
    CONCATENATE 'HTTP ERROR:' lv_code lt_response_abap-error-message
    INTO rv_msg SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
