*"* use this source file for your ABAP unit test classes

class lcl_test definition deferred .

class lcl_object definition friends if_trex_serialization .
  private section .
    data:
      string type string ,
      int type i .
endclass .

class lcl_nested_object definition .
  public section .
    data:
      o1 type ref to lcl_object ,
      o2 type ref to lcl_object .
endclass .

class lcl_test definition inheriting from cl_aunit_assert
  for testing risk level harmless duration short .
  public section .
    interfaces if_trex_serialization . "private access for asserts
  private section .
    methods:
       setup ,
       teardown ,
       test_string for testing ,
       test_struct for testing ,
       test_itab for testing ,
       test_object for testing ,
       test_nested_object for testing ,
       test_ci_checkvariant for testing .
    data:
      deserializer type ref to zabapfire_cl_json_deserializer .
endclass .

class lcl_test implementation .
  method setup .
    create object me->deserializer .
  endmethod .

  method teardown .
    clear me->deserializer .
  endmethod .

  method test_string .
    data l_string type string .

    me->deserializer->deserialize(
        exporting json = '"hello"'
        importing abap = l_string ) .

    assert_equals( exp = 'hello' act = l_string ) .
  endmethod .

  method test_struct .
    data:
      begin of l_struct ,
        string type string ,
        int type i ,
      end of l_struct .

    me->deserializer->deserialize(
        exporting json = '{string: "hello", int: 123}'
        importing abap = l_struct ) .

    assert_equals( exp = 'hello' act = l_struct-string ) .
    assert_equals( exp = 123 act = l_struct-int ) .
  endmethod .

  method test_object .
    data l_object type ref to lcl_object .

    create object l_object .

    me->deserializer->deserialize(
        exporting json = '{string: "hello", int: 123}'
        importing abap = l_object ) .

    assert_equals( exp = 'hello' act = l_object->string ) .
    assert_equals( exp = 123 act = l_object->int ) .
  endmethod .

  method test_itab .
    data:
      l_itab type standard table of string ,
      l_exp_itab like l_itab .

    me->deserializer->deserialize(
        exporting json = '["hello", "world"]'
        importing abap = l_itab ) .

    append 'hello' to l_exp_itab .
    append 'world' to l_exp_itab .

    assert_equals( exp = l_exp_itab act = l_itab ) .
  endmethod .

  method test_nested_object .
    data l_object type ref to lcl_nested_object .

    create object l_object .

    me->deserializer->deserialize(
        exporting json = '{o1: {string: "s 1", int: 1}, o2: {string: "s 2", int: 2}}'
        importing abap = l_object ) .

    assert_equals( exp = 's 1' act = l_object->o1->string ) .
    assert_equals( exp = 1 act = l_object->o1->int ) .
    assert_equals( exp = 's 2' act = l_object->o2->string ) .
    assert_equals( exp = 2 act = l_object->o2->int ) .
  endmethod .

  method test_ci_checkvariant .
    constants c_testname type string value 'CL_SAUNIT_LEGACY_CI_CHECK' .

*   create JSON
    data:
      l_aunit_check type ref to cl_trex_saunit_legacy_ci_check ,
      l_attributes type string ,
      l_json type string .

    create object l_aunit_check .
    l_attributes = l_aunit_check->get_attributes( ) .
    l_json = |\{TESTS: [\{TESTNAME: "{ c_testname }", ATTRIBUTES: "{ l_attributes }"\}]\}| .

*   deserialize
    data l_object type ref to cl_trex_ci_checkvariant .

    create object l_object
      exporting
        user = sy-uname
        name = 'UNIT_TEST'.

    me->deserializer->deserialize(
        exporting json = l_json
        importing abap = l_object ) .

*   assert
    data:
      l_exp like l_object->tests ,
      l_test like line of l_object->tests .

    l_test-testname = c_testname .
    l_test-attributes = l_attributes .

    insert l_test into table l_exp .

    assert_equals( exp = l_exp act = l_object->tests ) .
  endmethod .
endclass .
