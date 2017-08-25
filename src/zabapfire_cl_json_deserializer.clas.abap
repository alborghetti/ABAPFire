CLASS zabapfire_cl_json_deserializer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS deserialize
    IMPORTING
      !json TYPE string
    EXPORTING
      !abap TYPE any
    RAISING
      zcx_abapfire_json .
PROTECTED SECTION.
*"* protected components of class CL_TREX_JSON_DESERIALIZER
*"* do not include other source files here!!!
private section.

  types TY_NODE type ref to ZABAPFIRE_CL_JSON_NODE .

  data MV_NAME type STRING .
  data:
    mt_nodes_stack TYPE TABLE OF ty_node .
  data JSON_HIERARCHY type ref to ZABAPFIRE_CL_JSON_NODE .

*"* private components of class CL_TREX_JSON_DESERIALIZER
*"* do not include other source files here!!!
  methods DESERIALIZE_NODE
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
    raising
      ZCX_ABAPFIRE_JSON .
  methods DESERIALIZE_OBJECT
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
    raising
      ZCX_ABAPFIRE_JSON .
  methods DESERIALIZE_ARRAY
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
    raising
      ZCX_ABAPFIRE_JSON .
  methods DESERIALIZE_KEY
    importing
      !JSON type STRING
    changing
      !OFFSET type I default 0
    raising
      ZCX_ABAPFIRE_JSON .
  methods GET_CURRENT_NODE
    returning
      value(NODE) type ref to ZABAPFIRE_CL_JSON_NODE
    raising
      ZCX_ABAPFIRE_JSON .
  methods SET_NODE_LEVEL
    importing
      !ADD type ABAP_BOOL optional
      !REMOVE type ABAP_BOOL optional
      !ARRAY type ABAP_BOOL optional
      !VALUE type STRING optional
      !TYPE type STRING optional
    raising
      ZCX_ABAPFIRE_JSON .
  methods RECURSE
    importing
      !NODE type ref to ZABAPFIRE_CL_JSON_NODE
    changing
      !COMP type ANY
    raising
      ZCX_ABAPFIRE_JSON .
  methods CONVERT_NAME_TO_ABAP
    importing
      !JSON_NAME type STRING
    returning
      value(ABAP_NAME) type STRING .
  methods CONVERT_VALUE_TO_ABAP
    importing
      !JSON_VALUE type STRING
      !JSON_TYPE type STRING
    exporting
      value(ABAP_VALUE) type ANY .
  methods STRING_TO_XSTRING
    importing
      !IN type STRING
    changing
      !OUT type ANY .
ENDCLASS.



CLASS ZABAPFIRE_CL_JSON_DESERIALIZER IMPLEMENTATION.


  METHOD convert_name_to_abap.

    CHECK NOT json_name IS INITIAL.

    abap_name = json_name.
    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN abap_name WITH `$1_$2`. "#EC NOTEXT
    TRANSLATE abap_name TO UPPER CASE.

  ENDMETHOD.


  METHOD convert_value_to_abap.
    DATA: l_elem_descr TYPE REF TO cl_abap_elemdescr,
          l_string     TYPE string,
          l_year       TYPE n LENGTH 4,
          l_month      TYPE n LENGTH 2,
          l_day        TYPE n LENGTH 2,
          l_hour       TYPE n LENGTH 2,
          l_minute     TYPE n LENGTH 2,
          l_second     TYPE n LENGTH 2,
          l_decimals   TYPE n LENGTH 7,
          tk           LIKE l_elem_descr->type_kind,
          an           LIKE l_elem_descr->absolute_name,
          ol           LIKE l_elem_descr->output_length.

    DEFINE escape_string.
      REPLACE ALL OCCURRENCES OF `\"` IN &1 WITH `"`.
      REPLACE ALL OCCURRENCES OF `\\` IN &1 WITH `\`.
    END-OF-DEFINITION.

    l_elem_descr ?= cl_abap_typedescr=>describe_by_data( abap_value ).
    tk = l_elem_descr->type_kind.
    an = l_elem_descr->absolute_name.
    ol = l_elem_descr->output_length.

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


METHOD deserialize.

  deserialize_node(
    EXPORTING
      json = json ).

  recurse(
    EXPORTING
      node = json_hierarchy
    CHANGING
      comp = abap ).

ENDMETHOD.


METHOD deserialize_array.
  DATA:
    l_done TYPE abap_bool,
    l_rec  TYPE REF TO data.
  FIELD-SYMBOLS:
    <itab> TYPE ANY TABLE,
    <rec>  TYPE data.

  ADD 1 TO offset . "skip [

* Add object node to nodes stack
  set_node_level(
    EXPORTING
      add   = abap_true
      array = abap_true ).


  WHILE l_done = abap_false .
    deserialize_node(
      EXPORTING
        json = json
      CHANGING
        offset = offset ) .

    FIND REGEX ',|\]' IN SECTION OFFSET offset OF json MATCH OFFSET offset .
    IF sy-subrc <> 0 .
      zcx_abapfire_json=>raise( 'Invalid JSON' ).
    ENDIF .
    IF json+offset(1) = ']' .
      l_done = abap_true .
      set_node_level(
        EXPORTING
          remove = abap_true ).
    ENDIF .
    ADD 1 TO offset .
  ENDWHILE .

ENDMETHOD.


  METHOD deserialize_key.

    DATA:
      l_len     TYPE i,
      l_string  TYPE string,
      l_number  TYPE string,
      l_boolean TYPE string,
      l_value   TYPE string,
      l_type    TYPE string.

    FIND REGEX
      '"((?:\\"|(?!").)+)"|(-{1}\d+|\d+)|(true|false|null)'
      IN SECTION OFFSET offset OF json
      MATCH OFFSET offset MATCH LENGTH l_len
      SUBMATCHES l_string l_number l_boolean.
    IF sy-subrc <> 0 .
      zcx_abapfire_json=>raise( 'Invalid JSON' ).
    ENDIF .

    CASE json+offset(1) .
      WHEN '"' .
        l_type = 'string'.
        l_value = l_string.
        ADD l_len TO offset .
      WHEN 't' OR 'f' OR 'n'.
        l_type = 'boolean'.
        l_value = l_boolean.
      WHEN OTHERS . "0-9 or negative numbers
        l_type = 'number'.
        l_value = l_number.
    ENDCASE .

    set_node_level(
      EXPORTING
        value = l_value
        type  = l_type
        add   = abap_true ).
    CLEAR mv_name.
*   key nodes do not have children
    set_node_level(
      EXPORTING
        remove = abap_true ).

  ENDMETHOD.


METHOD deserialize_node.
  DATA:
    l_len     TYPE i.

  FIND REGEX
    '\{|\[|"((?:\\"|(?!").)+)"|(-{1}\d+|\d+)|(true|false)'
    " (\{)                          --> capturing group 1 {
    " (\[)                          --> capturing group 2 [
    " (["]((?:\\"|(?!").)+)")       --> capturing group 3 strings
    " (-{1}\d+|\d+)                 --> capturing group 4 numbers
    " (true|false)                  --> capturing group 5 booleans

    IN SECTION OFFSET offset OF json
    MATCH OFFSET offset MATCH LENGTH l_len.

  IF sy-subrc <> 0 .
    zcx_abapfire_json=>raise( 'Invalid JSON' ).
  ENDIF .

  CASE json+offset(1) .
    WHEN '{' .
      deserialize_object(
        EXPORTING
          json = json
        CHANGING
          offset = offset ).
    WHEN '[' .
      deserialize_array(
        EXPORTING
          json = json
        CHANGING
          offset = offset ).
    WHEN OTHERS.
      deserialize_key(
        EXPORTING
          json = json
        CHANGING
          offset = offset ).
  ENDCASE .

ENDMETHOD.


METHOD deserialize_object.
  DATA:
    l_done TYPE abap_bool,
    l_len  TYPE i.

  ADD 1 TO offset . "skip {

* Add object node to nodes stack
  set_node_level(
    EXPORTING
      add = abap_true ).

* handle each component
  WHILE l_done = abap_false .
    "find next key ("key":)
    FIND REGEX '"((?:\\"|(?!").)+)"\s*:' IN SECTION OFFSET offset OF json
      MATCH OFFSET offset MATCH LENGTH l_len
      SUBMATCHES mv_name.
    IF sy-subrc <> 0 .
      zcx_abapfire_json=>raise( 'Invalid JSON' ).
    ENDIF .
    ADD l_len TO offset.
*   remove " from l_name
    REPLACE ALL OCCURRENCES OF '"' IN mv_name WITH ''.
*   deserialize current component
    deserialize_node(
      EXPORTING
        json = json
      CHANGING
        offset = offset ).
    FIND REGEX ',|\}' IN SECTION OFFSET offset OF json MATCH OFFSET offset .
    IF sy-subrc <> 0 .
      zcx_abapfire_json=>raise( 'Invalid JSON' ).
    ENDIF .
    IF json+offset(1) = '}' .
      l_done = abap_true .
      set_node_level(
        EXPORTING
          remove = abap_true ).
    ENDIF .
    ADD 1 TO offset .
  ENDWHILE .

ENDMETHOD.


  METHOD get_current_node.
    DATA: l_len TYPE i.

    DESCRIBE TABLE mt_nodes_stack LINES l_len.

    READ TABLE mt_nodes_stack INTO node
      INDEX l_len.
    IF sy-subrc NE 0.
      zcx_abapfire_json=>raise( 'Invalid JSON' ).
    ENDIF.

  ENDMETHOD.


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


*   Iterate on JSON Hierarchy and fill ABAP type
    l_abap_type = cl_abap_typedescr=>describe_by_data( comp ).
    IF l_abap_type->kind = cl_abap_typedescr=>kind_table.
*     JSON Hierarchy children are table rows
      ASSIGN comp TO <tab>.
      CREATE DATA l_data LIKE LINE OF <tab>.
      ASSIGN l_data->* TO <str>.
      l_children = node->get_children( ).
      LOOP AT l_children INTO l_node.
        CLEAR <str>.
        recurse(
          EXPORTING
            node = l_node
          CHANGING
            comp = <str> ).
        INSERT <str> INTO TABLE <tab>.
      ENDLOOP.
    ELSEIF l_abap_type->kind = cl_abap_typedescr=>kind_struct.
*     JSON Hierarchy children are structure fields
      ASSIGN comp TO <str>.
      l_children = node->get_children( ).
      LOOP AT l_children INTO l_node.
        l_name = l_node->get_name( ).
        ASSIGN COMPONENT l_name OF STRUCTURE <str> TO <comp>.
        IF sy-subrc NE 0.
          zcx_abapfire_json=>raise( 'Invalid ABAP Target' ).
        ENDIF.
        recurse(
          EXPORTING
            node = l_node
          CHANGING
            comp = <comp> ).
      ENDLOOP.
    ELSE.
*     JSON Hierarchy children are key values
      TRY.
          convert_value_to_abap(
            EXPORTING
              json_value = node->get_value( )
              json_type = node->get_type( )
            IMPORTING
              abap_value = comp ).
        CATCH cx_sy_conversion_error.
          zcx_abapfire_json=>raise( 'Invalid ABAP Target' ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD set_node_level.

    DATA:
      l_len    TYPE i,
      l_parent TYPE REF TO zabapfire_cl_json_node,
      l_node   TYPE REF TO zabapfire_cl_json_node.

    DESCRIBE TABLE mt_nodes_stack LINES l_len.

    IF remove = abap_true.
*     Remove last node
      IF l_len > 0.
        READ TABLE mt_nodes_stack INTO json_hierarchy
          INDEX l_len.
        DELETE mt_nodes_stack INDEX l_len.
      ENDIF.
    ENDIF.

    IF add = abap_true.
*     Add new node
      IF l_len > 0.
        READ TABLE mt_nodes_stack INTO l_parent
          INDEX l_len.
      ENDIF.
      IF l_parent IS INITIAL.
        CREATE OBJECT l_node
          EXPORTING
            name  = convert_name_to_abap( mv_name )
            array = array
            value = value
            type  = type.
      ELSE.
        CREATE OBJECT l_node
          EXPORTING
            name  = convert_name_to_abap( mv_name )
            array = array
            value = value
            type  = type.
        l_parent->add_child( l_node ).
      ENDIF.
      CLEAR mv_name.
      APPEND l_node TO mt_nodes_stack.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_xstring.

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
ENDCLASS.
