class ZABAPFIRE_CL_JSON_SERIALIZER definition
  public
  final
  create public .

public section.

*"* public components of class CL_TREX_JSON_SERIALIZER
*"* do not include other source files here!!!
  class-methods CLASS_CONSTRUCTOR .
  methods SERIALIZE
    importing
      !ABAP type ANY
    returning
      value(JSON) type STRING
    raising
      ZCX_ABAPFIRE_JSON .
protected section.
*"* protected components of class ZL_ASXML_TO_JSON
*"* do not include other source files here!!!
private section.

  class-data C_COLON type STRING .
  class-data C_COMMA type STRING .
  constants C_BOOL_TYPES type STRING value `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD` ##NO_TEXT.
  data:
    mt_lines TYPE TABLE OF string .
  data JSON_HIERARCHY type ref to ZABAPFIRE_CL_JSON_NODE .

  methods RECURSE
    importing
      !COMP type DATA
      !NAME type STRING optional
    changing
      !NODE type ref to ZABAPFIRE_CL_JSON_NODE optional
    raising
      ZCX_ABAPFIRE_JSON .
  methods WRITE_JSON
    changing
      !JSON type STRING
    raising
      ZCX_ABAPFIRE_JSON .
  methods RECURSE_JSON
    importing
      !NODE type ref to ZABAPFIRE_CL_JSON_NODE
    raising
      ZCX_ABAPFIRE_JSON .
  methods CONVERT_NAME_TO_JSON
    importing
      !ABAP_NAME type STRING
    returning
      value(JSON_NAME) type STRING .
  methods CONVERT_VALUE_TO_JSON
    importing
      !ABAP_VALUE type ANY
    returning
      value(JSON_VALUE) type STRING .
  methods XSTRING_TO_STRING
    importing
      !IN type ANY
    returning
      value(OUT) type STRING .
ENDCLASS.



CLASS ZABAPFIRE_CL_JSON_SERIALIZER IMPLEMENTATION.


method CLASS_CONSTRUCTOR.

  cl_abap_string_utilities=>c2str_preserving_blanks(
      exporting source = ': '
      importing dest   = c_colon ) .
  cl_abap_string_utilities=>c2str_preserving_blanks(
      exporting source = ', '
      importing dest   = c_comma ) .

endmethod.


  METHOD convert_name_to_json.
    DATA: tokens TYPE TABLE OF char128.

    FIELD-SYMBOLS:
                   <token> TYPE any.

    CHECK NOT abap_name IS INITIAL.

    json_name = abap_name.

    TRANSLATE json_name TO LOWER CASE.
*    TRANSLATE json_name USING `/_:_~_`.

    SPLIT json_name AT `_` INTO TABLE tokens.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO json_name.

  ENDMETHOD.


  METHOD convert_value_to_json.
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


METHOD recurse.

  DATA:
    l_children   TYPE zabapfire_cl_json_node=>ty_nodes,
    l_node       TYPE REF TO zabapfire_cl_json_node,
    l_abap_type  TYPE REF TO cl_abap_typedescr,
    l_abap_stru  TYPE REF TO cl_abap_structdescr,
    l_component  TYPE cl_abap_structdescr=>component,
    l_components TYPE cl_abap_structdescr=>component_table,
    l_name       TYPE string,
    l_data       TYPE REF TO data.
  FIELD-SYMBOLS:
    <tab>  TYPE ANY TABLE,
    <str>  TYPE any,
    <comp> TYPE any.

  l_abap_type = cl_abap_typedescr=>describe_by_data( comp ).

* Create current node
  IF l_abap_type->kind = cl_abap_typedescr=>kind_table.
    CREATE OBJECT l_node
      EXPORTING
        name  = convert_name_to_json( name )
        array = abap_true.
  ELSE.
    CREATE OBJECT l_node
      EXPORTING
        name = convert_name_to_json( name ).
  ENDIF.
* Add node to father if it exists
  IF node IS BOUND.
    node->add_child( l_node ).
  ENDIF.
* Set first node of hierarchy
  IF NOT json_hierarchy IS BOUND.
    json_hierarchy = l_node.
  ENDIF.


* Iterate on ABAP and build JSON Hierarchy
  IF l_abap_type->kind = cl_abap_typedescr=>kind_table.
*   ABAP Table
    ASSIGN comp->* TO <tab>.
    CREATE DATA l_data LIKE LINE OF <tab>.
    ASSIGN l_data TO <str>.
    LOOP AT <tab> INTO <str>.
      recurse(
        EXPORTING
          comp = <str>
        CHANGING
          node = l_node ).
      INSERT <str> INTO TABLE <tab>.
    ENDLOOP.
  ELSEIF l_abap_type->kind = cl_abap_typedescr=>kind_struct.
*   ABAP Structure
    l_abap_stru ?= cl_abap_typedescr=>describe_by_data( comp ).
    l_components = l_abap_stru->get_components( ).
    LOOP AT l_components INTO l_component.
      ASSIGN COMPONENT l_component-name OF STRUCTURE comp TO <comp>.
      recurse(
        EXPORTING
          comp = <comp>
          name = l_component-name
        CHANGING
          node = l_node ).
    ENDLOOP.
  ELSE.
    l_node->set_value( convert_value_to_json( comp ) ).
  ENDIF.

ENDMETHOD.


METHOD recurse_json.

  DATA:
    l_children TYPE zabapfire_cl_json_node=>ty_nodes,
    l_node     TYPE REF TO zabapfire_cl_json_node,
    l_len      TYPE i,
    l_index    TYPE i,
    l_name     TYPE string,
    l_value    TYPE string.

  l_children = node->get_children( ).
  l_name = node->get_name( ).
  l_value = node->get_value( ).

  l_len = lines( l_children ).

  IF NOT l_name IS INITIAL.
    APPEND l_name TO mt_lines.
    APPEND c_colon TO mt_lines.
  ENDIF.

  IF node->is_array( ) = abap_true.
*   Array
    APPEND '[' TO mt_lines.
    LOOP AT l_children INTO l_node.
      ADD 1 TO l_index .
      recurse_json( l_node ).
      IF l_index < l_len .
        APPEND c_comma TO mt_lines.
      ENDIF .
    ENDLOOP.
    APPEND ']' TO mt_lines .
  ELSEIF l_len > 0.
*   Object
    APPEND '{' TO mt_lines.
    LOOP AT l_children INTO l_node.
      ADD 1 TO l_index .
      recurse_json( l_node ).
      IF l_index < l_len .
        APPEND c_comma TO mt_lines.
      ENDIF .
    ENDLOOP.
    APPEND '}' TO mt_lines .
  ELSE.
*   Scalar value
    APPEND l_value TO mt_lines.
  ENDIF.

ENDMETHOD.


METHOD serialize.

  recurse(
    EXPORTING comp = abap ) .

  write_json(
    CHANGING
      json = json ).

ENDMETHOD.


  METHOD write_json.

    recurse_json( json_hierarchy ).

    CONCATENATE LINES OF mt_lines INTO json .

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
