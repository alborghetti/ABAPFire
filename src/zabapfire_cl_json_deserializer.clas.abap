class ZABAPFIRE_CL_JSON_DESERIALIZER definition
  public
  final
  create public .

public section.

  methods DESERIALIZE
    importing
      !JSON type STRING
    exporting
      !ABAP type ANY .
  methods DESERIALIZE_REF
    importing
      !JSON type STRING
      !REF type ref to OBJECT .
protected section.
*"* protected components of class CL_TREX_JSON_DESERIALIZER
*"* do not include other source files here!!!
private section.
*"* private components of class CL_TREX_JSON_DESERIALIZER
*"* do not include other source files here!!!

  methods DESERIALIZE_NODE
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
      !NODE type ANY
    raising
      CX_TREX_SERIALIZATION .
  methods DESERIALIZE_OBJECT
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
      !NODE type ANY
    raising
      CX_TREX_SERIALIZATION .
  methods DESERIALIZE_ARRAY
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
      !NODE type ANY
    raising
      CX_TREX_SERIALIZATION .
ENDCLASS.



CLASS ZABAPFIRE_CL_JSON_DESERIALIZER IMPLEMENTATION.


method deserialize.
  DESERIALIZE_NODE(
    exporting
      json = json
    changing
      node = abap ) .
endmethod.


method deserialize_array.
  data:
    l_done type abap_bool ,
    l_rec type ref to data .
  field-symbols:
    <itab> type any table ,
    <rec> type data .

  add 1 to offset . "skip [

  assign node to <itab> .

* create record
  create data l_rec like line of <itab> .
  assign l_rec->* to <rec> .

  while l_done = abap_false .
    clear <rec> .
    deserialize_node(
      exporting
        json = json
      changing
        offset = offset
        node = <rec> ) .

    insert <rec> into table <itab> .

    find regex ',|\]' in section offset offset of json match offset offset .
    if sy-subrc <> 0 .
      raise exception type cx_trex_serialization .
    endif .
    if json+offset(1) = ']' .
      l_done = abap_true .
    endif .
    add 1 to offset .
  endwhile .
endmethod.


METHOD deserialize_node.
  DATA:
    l_len    TYPE i,
    l_string TYPE string,
    l_number TYPE string.

  FIND REGEX '\{|\[|"([^"]*)"|(\d+)|(true|false)' IN SECTION OFFSET offset OF json
    MATCH OFFSET offset MATCH LENGTH l_len
    SUBMATCHES l_string l_number .

  IF sy-subrc <> 0 .
    RAISE EXCEPTION TYPE cx_trex_serialization .
  ENDIF .

  CASE json+offset(1) .
    WHEN '{' .
      deserialize_object(
        EXPORTING
          json = json
        CHANGING
          offset = offset
          node = node ) .

    WHEN '[' .
      deserialize_array(
        EXPORTING
          json = json
        CHANGING
          offset = offset
          node = node ) .

    WHEN '"' .
      node = l_string .
      ADD l_len TO offset .

    WHEN 't' OR 'T' . "True
      node = 'X'.
    WHEN 'f' OR 'F'. "False
      node = ''.
    WHEN OTHERS . "0-9
      node = l_number .
      ADD l_len TO offset .
  ENDCASE .
ENDMETHOD.


METHOD deserialize_object.
  DATA:
    l_node_type TYPE REF TO cl_abap_typedescr,
    l_ref       TYPE REF TO object.

  ADD 1 TO offset . "skip {

  l_node_type = cl_abap_typedescr=>describe_by_data( node ) .

* prepare for dynamic access
  CASE l_node_type->kind .
    WHEN cl_abap_typedescr=>kind_ref .
      l_ref = node .
    WHEN cl_abap_typedescr=>kind_struct .

    WHEN OTHERS .
      RAISE EXCEPTION TYPE cx_trex_serialization .
  ENDCASE .

  DATA:
    l_done TYPE abap_bool,
    l_len  TYPE i,
    l_name TYPE string.

* handle each component
  WHILE l_done = abap_false .
    "find next key ("key":)
    FIND REGEX '("[^"]+")\s*:' IN SECTION OFFSET offset OF json
      MATCH OFFSET offset MATCH LENGTH l_len
      SUBMATCHES l_name .
    IF sy-subrc <> 0 .
      RAISE EXCEPTION TYPE cx_trex_serialization .
    ENDIF .
    ADD l_len TO offset .

    FIELD-SYMBOLS <comp> TYPE any .
*   remove " from l_name
    REPLACE ALL OCCURRENCES OF '"' IN l_name WITH ''.
*   dynamic binding to component
    TRANSLATE l_name TO UPPER CASE .
    CASE l_node_type->kind .
      WHEN cl_abap_typedescr=>kind_ref .
        ASSIGN l_ref->(l_name) TO <comp> .
      WHEN cl_abap_typedescr=>kind_struct .
        ASSIGN COMPONENT l_name OF STRUCTURE node TO <comp> .

      WHEN OTHERS .
        RAISE EXCEPTION TYPE cx_trex_serialization .
    ENDCASE .

    DATA:
      l_comp_type TYPE REF TO cl_abap_typedescr,
      l_ref_type  TYPE REF TO cl_abap_refdescr.

*   check component type
    l_comp_type = cl_abap_typedescr=>describe_by_data( <comp> ) .
    CASE l_comp_type->kind .
*     create instance if it's an oref
      WHEN cl_abap_typedescr=>kind_ref .
        l_ref_type ?= l_comp_type .
        l_comp_type = l_ref_type->get_referenced_type( ) .
        CREATE OBJECT <comp> TYPE (l_comp_type->absolute_name) .
    ENDCASE .

*   deserialize current component
    deserialize_node(
      EXPORTING
        json = json
      CHANGING
        offset = offset
        node = <comp> ) .

    FIND REGEX ',|\}' IN SECTION OFFSET offset OF json MATCH OFFSET offset .
    IF sy-subrc <> 0 .
      RAISE EXCEPTION TYPE cx_trex_serialization .
    ENDIF .

    IF json+offset(1) = '}' .
      l_done = abap_true .
    ENDIF .
    ADD 1 TO offset .
  ENDWHILE .

ENDMETHOD.


method DESERIALIZE_REF.
  data l_ref type ref to object .
  l_ref = ref .
  DESERIALIZE_NODE(
    exporting
      json = json
    changing
      node = l_ref ) .
endmethod.
ENDCLASS.
