CLASS zcm_abc_booking DEFINITION PUBLIC
                                 INHERITING FROM cx_static_check
                                 FINAL
                                 CREATE PUBLIC .
  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_abap_behv_message .

    CONSTANTS:
      BEGIN OF book_unavailable,
        msgid TYPE symsgid VALUE 'ZABC_MC_BOOKING',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'BOOKNAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF book_unavailable .
    DATA bookname TYPE string READ-ONLY .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous TYPE REF TO cx_root OPTIONAL
        !severity TYPE if_abap_behv_message=>t_severity DEFAULT if_abap_behv_message=>severity-error
        !bookname TYPE zabc_d_book-book_name OPTIONAL .

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCM_ABC_BOOKING IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->if_abap_behv_message~m_severity = severity.
    me->bookname = bookname.

  ENDMETHOD.
ENDCLASS.
