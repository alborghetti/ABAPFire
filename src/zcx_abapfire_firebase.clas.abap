class ZCX_ABAPFIRE_FIREBASE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    BEGIN OF http_connection_error,
        msgid TYPE symsgid VALUE 'ZABAPFIRE_MSG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF http_connection_error .
  constants:
    BEGIN OF incorrect_authentication_param,
        msgid TYPE symsgid VALUE 'ZABAPFIRE_MSG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF incorrect_authentication_param .
  constants:
    BEGIN OF firebase_config,
        msgid TYPE symsgid VALUE 'ZABAPFIRE_MSG',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'WRONG_FIREBASE_CONFIG_PARM',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF firebase_config .
  constants:
    BEGIN OF missing_attribute,
        msgid TYPE symsgid VALUE 'ZABAPFIRE_MSG',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF missing_attribute .
  constants:
    BEGIN OF conversion_error,
        msgid TYPE symsgid VALUE 'ZABAPFIRE_MSG',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF conversion_error .
  data WRONG_FIREBASE_CONFIG_PARM type STRING .
  data RAISE_ATTR1 type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !WRONG_FIREBASE_CONFIG_PARM type STRING optional
      !RAISE_ATTR1 type STRING optional .
  class-methods RAISE
    importing
      !IV_TEXT type CLIKE
    raising
      ZCX_ABAPFIRE_FIREBASE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_ABAPFIRE_FIREBASE IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->wrong_firebase_config_parm = wrong_firebase_config_parm .
    me->raise_attr1 = raise_attr1 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD raise.
    DATA lv_text LIKE if_t100_message=>t100key.

    DATA: BEGIN OF lv_msg,
            msgid TYPE symsgid VALUE 'ZABAPFIRE_MSG',
            msgno TYPE symsgno VALUE '000',
            attr1 TYPE scx_attrname VALUE 'RAISE_ATTR1',
            attr2 TYPE scx_attrname VALUE '',
            attr3 TYPE scx_attrname VALUE '',
            attr4 TYPE scx_attrname VALUE '',
          END OF lv_msg .

    RAISE EXCEPTION TYPE zcx_abapfire_firebase
      EXPORTING
        textid      = lv_msg
        raise_attr1 = iv_text.
  ENDMETHOD.
ENDCLASS.
