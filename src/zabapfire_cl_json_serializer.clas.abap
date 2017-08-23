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
  data JSON_HIERARCHY type ref to ZABAPFIRE_CL_JSON_NODE .

  methods RECURSE
    importing
      !COMP type DATA
    changing
      !NODE type ref to ZABAPFIRE_CL_JSON_NODE
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
    changing
      !JSON type STRING
    raising
      ZCX_ABAPFIRE_JSON .
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


METHOD recurse.

  DATA:
    l_children  TYPE zabapfire_cl_json_node=>ty_nodes,
    l_node      TYPE REF TO zabapfire_cl_json_node,
    l_abap_type TYPE REF TO cl_abap_typedescr,
    l_name      TYPE string,
    l_data      TYPE REF TO data.
  FIELD-SYMBOLS:
    <tab>  TYPE ANY TABLE,
    <str>  TYPE any,
    <comp> TYPE any.


* Iterate on ABAP and build JSON Hierarchy
  l_abap_type = cl_abap_typedescr=>describe_by_data( comp ).
  IF l_abap_type->kind = cl_abap_typedescr=>kind_table.
*   ABAP Table
    ASSIGN comp TO <tab>.
    CREATE DATA l_data LIKE LINE OF <tab>.
    ASSIGN l_data->* TO <str>.
    CREATE OBJECT l_node
      EXPORTING
        name  = l_abap_type->get_relative_name( )
        array = abap_true.
    node->add_child( l_node ).
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
    ASSIGN comp TO <str>.
    CREATE OBJECT l_node
      EXPORTING
        name = l_abap_type->get_relative_name( ).
    node->add_child( l_node ).
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <str> TO <comp>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      recurse(
        EXPORTING
          comp = <str>
        CHANGING
          node = l_node ).
    ENDDO.
  ELSE.
*   ABAP Scalar values
    CREATE OBJECT l_node
      EXPORTING
        name  = l_abap_type->get_relative_name( )
        value = comp.
    node->add_child( l_node ).
  ENDIF.

ENDMETHOD.


METHOD RECURSE_JSON.


ENDMETHOD.


METHOD serialize.

  CREATE OBJECT json_hierarchy.

  recurse(
    EXPORTING comp = abap
    CHANGING node = json_hierarchy  ) .

  write_json(
    CHANGING
      json = json ).

ENDMETHOD.


  METHOD write_json.

    recurse_json(
      EXPORTING
        node = json_hierarchy
      CHANGING
        json = json ).

  ENDMETHOD.
ENDCLASS.
