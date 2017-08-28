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
  class-methods ABAP_LCC
    importing
      !IN type STRING
    returning
      value(OUT) type STRING .
  class-methods LCC_ABAP
    importing
      !IN type STRING
    returning
      value(OUT) type STRING .
  class-methods DESERIALIZE_VALUE
    importing
      !JSON_VALUE type STRING
      !JSON_TYPE type STRING
    exporting
      value(ABAP_VALUE) type ANY .
  class-methods SERIALIZE_VALUE
    importing
      !ABAP_VALUE type ANY
    returning
      value(JSON_VALUE) type STRING .
  PROTECTED SECTION.
private section.

  constants C_BOOL_TYPES type STRING value `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD` ##NO_TEXT.

  class-methods XSTRING_TO_STRING
    importing
      !IN type ANY
    returning
      value(OUT) type STRING .
  class-methods STRING_TO_XSTRING
    importing
      !IN type STRING
    changing
      !OUT type ANY .
ENDCLASS.



CLASS ZABAPFIRE_CL_HTTP_UTIL IMPLEMENTATION.


  METHOD ABAP_LCC.

    DATA: tokens TYPE TABLE OF char128.

    FIELD-SYMBOLS:
                   <token> TYPE any.

    CHECK NOT in IS INITIAL.

    out = in.
    TRANSLATE out TO LOWER CASE.

    SPLIT out AT `_` INTO TABLE tokens.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO out.

  ENDMETHOD.


  METHOD DESERIALIZE_VALUE.
    DATA: l_elem_descr TYPE REF TO cl_abap_elemdescr,
          l_string     TYPE string,
          l_year       TYPE n LENGTH 4,
          l_month      TYPE n LENGTH 2,
          l_day        TYPE n LENGTH 2,
          l_hour       TYPE n LENGTH 2,
          l_minute     TYPE n LENGTH 2,
          l_second     TYPE n LENGTH 2,
          l_decimals   TYPE n LENGTH 7,
          tk           LIKE l_elem_descr->type_kind.

    DEFINE escape_string.
      REPLACE ALL OCCURRENCES OF `\"` IN &1 WITH `"`.
      REPLACE ALL OCCURRENCES OF `\\` IN &1 WITH `\`.
    END-OF-DEFINITION.

    l_elem_descr ?= cl_abap_typedescr=>describe_by_data( abap_value ).
    tk = l_elem_descr->type_kind.

    CASE json_type.
      WHEN 'string'.
        l_string = json_value.
        escape_string l_string.
        CASE tk.
          WHEN cl_abap_typedescr=>typekind_xstring OR
               cl_abap_typedescr=>typekind_hex.
            string_to_xstring( EXPORTING in = l_string CHANGING out = abap_value ).
          WHEN cl_abap_typedescr=>typekind_date.
            FIND FIRST OCCURRENCE OF REGEX '(\d{4})-(\d{2})-(\d{2})' IN json_value
              SUBMATCHES l_year l_month l_day.
            IF sy-subrc EQ 0.
              CONCATENATE l_year l_month l_day INTO abap_value.
            ENDIF.
          WHEN cl_abap_typedescr=>typekind_time.
            FIND FIRST OCCURRENCE OF REGEX '(\d{2}):(\d{2}):(\d{2})' IN json_value
              SUBMATCHES l_hour l_minute l_second.
            IF sy-subrc EQ 0.
              CONCATENATE l_hour l_minute l_second INTO abap_value.
            ENDIF.
          WHEN cl_abap_typedescr=>typekind_packed.
            FIND FIRST OCCURRENCE OF REGEX
            '(\d{4})-?(\d{2})-?(\d{2})T(\d{2}):?(\d{2}):?(\d{2})(?:[\.,](\d{0,7}))?Z?'
                                                            "#EC NOTEXT
              IN json_value
              SUBMATCHES l_year l_month l_day l_hour l_minute l_second l_decimals.
            IF sy-subrc EQ 0.
              CONCATENATE l_year l_month l_day
                          l_hour l_minute l_second '.' l_decimals INTO l_string.
              abap_value = l_string.
            ENDIF.
          WHEN OTHERS.
            abap_value = l_string.
        ENDCASE.
      WHEN 'boolean'.
        CASE json_value.
          WHEN 'true'.
            abap_value = abap_true.
          WHEN 'false' OR 'null'. "TODO Manage 3 values boolean
            abap_value = abap_false.
        ENDCASE.
      WHEN 'number'.
        abap_value = json_value.
    ENDCASE.

  ENDMETHOD.


  METHOD get_http_error_msg.

    TYPES:
      BEGIN OF ty_error_array,
        domain  TYPE string,
        reason  TYPE string,
        message TYPE string,
      END OF ty_error_array,
      BEGIN OF ty_error_google,
        errors  TYPE STANDARD TABLE OF ty_error_array
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
      WHEN OTHERS.
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
      WHEN 'google'.                                        "#EC NOTEXT
        ASSIGN COMPONENT 'message'
          OF STRUCTURE ls_error_google-error
          TO <message>.                                     "#EC NOTEXT
      WHEN 'firebase'.                                      "#EC NOTEXT
        ASSIGN ls_error_firebase-error TO <message>.
    ENDCASE.

    IF <message> IS ASSIGNED.
      CONCATENATE 'HTTP ERROR:' lv_code <message>
      INTO rv_msg SEPARATED BY space.
    ENDIF.

  ENDMETHOD.


  METHOD lcc_abap.

    CHECK NOT in IS INITIAL.

    out = in.
    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])`
      IN out WITH `$1_$2`.                                  "#EC NOTEXT
    TRANSLATE out TO UPPER CASE.

  ENDMETHOD.


  METHOD SERIALIZE_VALUE.
    DATA: l_elem_descr TYPE REF TO cl_abap_elemdescr,
          tk           LIKE l_elem_descr->type_kind,
          an           LIKE l_elem_descr->absolute_name,
          ol           LIKE l_elem_descr->output_length.

    DEFINE escape_string.
      REPLACE ALL OCCURRENCES OF `\` IN &1 WITH `\\`.
      REPLACE ALL OCCURRENCES OF `"` IN &1 WITH `\"`.
    END-OF-DEFINITION.

    l_elem_descr ?= cl_abap_typedescr=>describe_by_data( abap_value ).
    tk = l_elem_descr->type_kind.
    an = l_elem_descr->absolute_name.
    ol = l_elem_descr->output_length.

    CASE tk.
      WHEN cl_abap_typedescr=>typekind_float  OR
           cl_abap_typedescr=>typekind_int    OR
           cl_abap_typedescr=>typekind_int1   OR
           cl_abap_typedescr=>typekind_int2   OR
           cl_abap_typedescr=>typekind_packed OR
           `8`. "typekind_int8 -> '8' only from 7.40.
        IF tk EQ cl_abap_typedescr=>typekind_packed AND
           an CP `\TYPE=TIMESTAMP*`.
          IF abap_value IS INITIAL.
            json_value = `""`.
          ELSE.
            MOVE abap_value TO json_value.
            IF an EQ `\TYPE=TIMESTAMP`.
              CONCATENATE `"` json_value(4)
                          `-` json_value+4(2)
                          `-` json_value+6(2)
                          `T` json_value+8(2)
                          `:` json_value+10(2)
                          `:` json_value+12(2)
                          `.0000000Z"` INTO json_value.
            ELSEIF an EQ `\TYPE=TIMESTAMPL`.
              CONCATENATE `"` json_value(4)
                          `-` json_value+4(2)
                          `-` json_value+6(2)
                          `T` json_value+8(2)
                          `:` json_value+10(2)
                          `:` json_value+12(2)
                          `.` json_value+15(7) `Z"` INTO json_value.
            ENDIF.
          ENDIF.
        ELSEIF abap_value IS INITIAL.
          json_value = `0`.
        ELSE.
          MOVE abap_value TO json_value.
          IF abap_value LT 0.
            IF tk <> cl_abap_typedescr=>typekind_float. "float: sign is already at the beginning
              SHIFT json_value RIGHT CIRCULAR.
            ENDIF.
          ELSE.
            CONDENSE json_value.
          ENDIF.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_num.
        IF abap_value IS INITIAL.
          json_value = `0`.
        ELSE.
          MOVE abap_value TO json_value.
          SHIFT json_value LEFT DELETING LEADING ` 0`.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_string OR
           cl_abap_typedescr=>typekind_csequence OR
           cl_abap_typedescr=>typekind_clike.
        IF abap_value IS INITIAL.
          json_value = `""`.
        ELSE.
          MOVE abap_value TO json_value.
          escape_string json_value.
          CONCATENATE `"` json_value `"` INTO json_value.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_xstring OR
           cl_abap_typedescr=>typekind_hex.
        IF abap_value IS INITIAL.
          json_value = `""`.
        ELSE.
          json_value = xstring_to_string( abap_value ).
          escape_string json_value.
          CONCATENATE `"` json_value `"` INTO json_value.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_char.
        IF ol EQ 1 AND c_bool_types CS an.
          IF abap_value EQ abap_true.
            json_value = `true`.                            "#EC NOTEXT
          ELSE.
            json_value = `false`.                           "#EC NOTEXT
          ENDIF.
        ELSE.
          MOVE abap_value TO json_value.
          escape_string json_value.
          CONCATENATE `"` json_value `"` INTO json_value.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_date.
        MOVE abap_value TO json_value.
        CONCATENATE `"` json_value(4)
                    `-` json_value+4(2)
                    `-` json_value+6(2) `"` INTO json_value.
      WHEN cl_abap_typedescr=>typekind_time.
        MOVE abap_value TO json_value.
        CONCATENATE `"` json_value(2)
                    `:` json_value+2(2)
                    `:` json_value+4(2) `"` INTO json_value.
      WHEN OTHERS.
        IF abap_value IS INITIAL.
          json_value = `null`.                              "#EC NOTEXT
        ELSE.
          MOVE abap_value TO json_value.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD STRING_TO_XSTRING.

    DATA: l_xstring TYPE xstring.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = in
      IMPORTING
        bindata = l_xstring
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc IS INITIAL.
      MOVE l_xstring TO out.
    ELSE.
      MOVE in TO out.
    ENDIF.

  ENDMETHOD.


  METHOD XSTRING_TO_STRING.

    DATA: l_xstring TYPE xstring.

    l_xstring = in.

    CALL FUNCTION 'SSFC_BASE64_ENCODE'
      EXPORTING
        bindata = l_xstring
      IMPORTING
        b64data = out
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc IS NOT INITIAL.
      MOVE in TO out.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
