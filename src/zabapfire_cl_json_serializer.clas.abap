class ZABAPFIRE_CL_JSON_SERIALIZER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_entity_attribute,
             name TYPE string,
           END OF ty_entity_attribute .
  types:
    ty_entity_attributes TYPE TABLE OF ty_entity_attribute .

*"* public components of class CL_TREX_JSON_SERIALIZER
*"* do not include other source files here!!!
  class-methods CLASS_CONSTRUCTOR .
  methods CONSTRUCTOR
    importing
      !DATA type DATA
      !ENTITY_ATTRIBUTES type DATA .
  methods SERIALIZE
    raising
      ZCX_ABAPFIRE_FIREBASE .
  methods GET_DATA
    returning
      value(RVAL) type STRING .
protected section.
*"* protected components of class ZL_ASXML_TO_JSON
*"* do not include other source files here!!!
private section.

*"* private components of class CL_TREX_JSON_SERIALIZER
*"* do not include other source files here!!!
  data FRAGMENTS type TREXT_STRING .
  data DATA_REF type ref to DATA .
  class-data C_COLON type STRING .
  class-data C_COMMA type STRING .
  data MT_ENTITY_ATTRIBUTES type TY_ENTITY_ATTRIBUTES .

  methods RECURSE
    importing
      !DATA type DATA
    raising
      ZCX_ABAPFIRE_FIREBASE .
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


METHOD constructor.
  GET REFERENCE OF data INTO me->data_ref .
  mt_entity_attributes = entity_attributes.
ENDMETHOD.


method GET_DATA.
  concatenate lines of me->fragments into rval .
endmethod.


METHOD recurse.
  DATA:
    l_type       TYPE c,
    l_comps      TYPE i,
    l_lines      TYPE i,
    l_index      TYPE i,
    l_value      TYPE string,
    lv_attribute TYPE ty_entity_attribute.
  FIELD-SYMBOLS:
    <itab> TYPE ANY TABLE,
    <comp> TYPE any.

  DESCRIBE FIELD data TYPE l_type COMPONENTS l_comps .

  IF l_type = cl_abap_typedescr=>typekind_table .
*   itab -> array
    APPEND '[' TO me->fragments .
    ASSIGN data TO <itab> .
    l_lines = lines( <itab> ) .
    LOOP AT <itab> ASSIGNING <comp> .
      ADD 1 TO l_index .
      recurse( <comp> ) .
      IF l_index < l_lines .
        APPEND c_comma TO me->fragments .
      ENDIF .
    ENDLOOP .
    APPEND ']' TO fragments .
  ELSE .
    IF l_comps IS INITIAL .
*     field -> scalar
*     todo: format
      l_value = data .
      REPLACE ALL OCCURRENCES OF '\' IN l_value WITH '\\' .
      REPLACE ALL OCCURRENCES OF '''' IN l_value WITH '\''' .
      REPLACE ALL OCCURRENCES OF '"' IN l_value WITH '\"' .
      REPLACE ALL OCCURRENCES OF '&' IN l_value WITH '\&' .
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN l_value WITH '\r\n' .
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN l_value WITH '\n' .
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN l_value WITH '\t' .
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace IN l_value WITH '\b' .
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN l_value WITH '\f' .
      CONCATENATE '"' l_value '"' INTO l_value .
      APPEND l_value TO me->fragments .
    ELSE .
*     structure -> object
      DATA l_typedescr TYPE REF TO cl_abap_structdescr .
      FIELD-SYMBOLS <abapcomp> TYPE abap_compdescr .

      APPEND '{' TO me->fragments .
      l_typedescr ?= cl_abap_typedescr=>describe_by_data( data ) .
      LOOP AT l_typedescr->components ASSIGNING <abapcomp> .
        l_index = sy-tabix .
        "Read attribute id
        READ TABLE mt_entity_attributes INTO lv_attribute
          INDEX l_index.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_abapfire_firebase
            EXPORTING
              textid = zcx_abapfire_firebase=>missing_attribute.
        ENDIF.
        CONCATENATE lv_attribute-name c_colon INTO l_value . "change here
        APPEND l_value TO me->fragments .
        ASSIGN COMPONENT <abapcomp>-name OF STRUCTURE data TO <comp> .
        recurse( <comp> ) .
        IF l_index < l_comps .
          APPEND c_comma TO me->fragments .
        ENDIF .
      ENDLOOP .
      APPEND '}' TO me->fragments .
    ENDIF .
  ENDIF .
ENDMETHOD.


method SERIALIZE.
  field-symbols <data> type data .

  assign me->data_ref->* to <data> .
  recurse( <data> ) .
endmethod.
ENDCLASS.
