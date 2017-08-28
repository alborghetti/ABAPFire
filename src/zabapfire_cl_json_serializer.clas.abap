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
    l_node       TYPE REF TO zabapfire_cl_json_node,
    l_abap_type  TYPE REF TO cl_abap_typedescr,
    l_abap_stru  TYPE REF TO cl_abap_structdescr,
    l_component  TYPE abap_compdescr,
    l_rec        TYPE REF TO data,
    l_name       TYPE string.
  FIELD-SYMBOLS:
    <tab>  TYPE ANY TABLE,
    <str>  TYPE any,
    <comp> TYPE any.

  l_abap_type = cl_abap_typedescr=>describe_by_data( comp ).

* Create current node
  CHECK NOT name CA '$#[]/'. "In case ABAP strucutre contains a $KEY

  IF NOT name IS INITIAL.
    l_name = zabapfire_cl_http_util=>abap_lcc( name ).
    CONCATENATE '"' l_name '"' INTO l_name.
  ENDIF.
  IF l_abap_type->kind = cl_abap_typedescr=>kind_table.
    CREATE OBJECT l_node
      EXPORTING
        name  = l_name
        array = abap_true.
  ELSE.
    CREATE OBJECT l_node
      EXPORTING
        name = l_name.
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
    ASSIGN comp TO <tab>.
    CREATE DATA l_rec LIKE LINE OF <tab>.
    ASSIGN l_rec->* TO <str>.
    LOOP AT <tab> INTO <str>.
      recurse(
        EXPORTING
          comp = <str>
        CHANGING
          node = l_node ).
    ENDLOOP.
  ELSEIF l_abap_type->kind = cl_abap_typedescr=>kind_struct.
*   ABAP Structure
    l_abap_stru ?= cl_abap_typedescr=>describe_by_data( comp ).
    LOOP AT l_abap_stru->components INTO l_component.
      l_name = l_component-name.
      ASSIGN COMPONENT l_name OF STRUCTURE comp TO <comp>.
      recurse(
        EXPORTING
          comp = <comp>
          name = l_name
        CHANGING
          node = l_node ).
    ENDLOOP.
  ELSE.
    l_node->set_value(
      zabapfire_cl_http_util=>serialize_value( comp )
    ).
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
ENDCLASS.
