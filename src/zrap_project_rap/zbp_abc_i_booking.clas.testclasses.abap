CLASS ltcl_booking DEFINITION FINAL FOR TESTING
                                        DURATION SHORT
                                        RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: cut                  TYPE REF TO lhc_Booking,
                cds_test_environment TYPE REF TO if_cds_test_environment,
                sql_test_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS:
      class_setup,
      class_teardown.
    METHODS:
      setup RAISING cx_abap_auth_check_exception,
      teardown,
      validate_bookavailability FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_booking IMPLEMENTATION.

  METHOD class_setup.
    cds_test_environment = cl_cds_test_environment=>create_for_multiple_cds( i_for_entities = VALUE #( ( i_for_entity = 'zabc_c_booking' )
*                                                                                                      ( i_for_entity = 'zlab_c_book' )
                                                                                                     ) ).
    cds_test_environment->enable_double_redirection( ).
  ENDMETHOD.

  METHOD setup.
    cds_test_environment->clear_doubles( ).
    CREATE OBJECT cut FOR TESTING.
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD class_teardown.
    cds_test_environment->destroy( ).

  ENDMETHOD.

  METHOD validate_bookavailability.

    DATA: lt_booking TYPE STANDARD TABLE OF zabc_i_booking,
          lt_book    TYPE STANDARD TABLE OF zabc_i_book.

    lt_booking = VALUE #(
      ( BookingUuid    = 'FA163E6E92A51EECA49787B89FF99C2E'  BookingId      = '1'        BookId         ='000001'   PersonId       ='000001' BookingStatus ='1'
        BookingBegDate = '20210601'                          BookingBegTime = '090000'   BookingEndDate ='20210801' BookingEndTime ='090000'  )
      ( BookingUuid    = 'FA163E6E92A51EECA49787B89FF9BC2E'  BookingId      = '2'        BookId         ='000002'   PersonId       ='000002' BookingStatus ='2'
        BookingBegDate = '20210602'                          BookingBegTime = '100000'   BookingEndDate ='20220403' BookingEndTime ='235418'  )
      ( BookingUuid    = 'FA163E6E92A51EECA49787B89FF9DC2E'  BookingId      = '3'        BookId         ='000003'   PersonId       ='000003' BookingStatus ='3'
        BookingBegDate = '20210603'                          BookingBegTime = '110000'   BookingEndDate ='00000000' BookingEndTime ='120000'  )
      ( BookingUuid    = 'FA163E6E92A51EECA49787B89FF9FC2E'  BookingId      = '4'        BookId         ='000004'   PersonId       ='000004' BookingStatus ='4'
        BookingBegDate = '20210604'                          BookingBegTime = '120000'   BookingEndDate ='00000000' BookingEndTime ='120000'  )
                        ).
    lt_book = VALUE #(
      ( BookId ='000001' BookName ='A Song of Ice and Fire'     AuthorId ='000001' PagesNum ='800 ' CopyQty ='2'  )
      ( BookId ='000002' BookName ='IT'                         AuthorId ='000002' PagesNum ='300 ' CopyQty ='-1' ) " Expected Failing
      ( BookId ='000003' BookName ='The Picture of Dorian Gray' AuthorId ='000003' PagesNum ='150 ' CopyQty ='2'  )
      ( BookId ='000004' BookName ='The Master and Margarita'   AuthorId ='000004' PagesNum ='350 ' CopyQty ='2'  )
                      ).
    cds_test_environment->insert_test_data( i_data = lt_booking ).
    cds_test_environment->insert_test_data( i_data = lt_book ).

    DATA: lt_failed   TYPE RESPONSE FOR FAILED LATE   zabc_i_booking,
          lt_reported TYPE RESPONSE FOR REPORTED LATE zabc_i_booking.

    SELECT * FROM zabc_i_booking INTO TABLE @DATA(act_booking). "#EC CI_ALL_FIELDS_NEEDED
    cl_abap_unit_assert=>assert_equals( msg = 'Projects insert error'
                                        act = act_booking   " Actual Value
                                        exp = lt_booking ). " Expected Value

    SELECT * FROM zabc_i_book INTO TABLE @DATA(act_book). "#EC CI_ALL_FIELDS_NEEDED
    cl_abap_unit_assert=>assert_equals( msg = 'Projects insert error'
                                        act = act_book
                                        exp = lt_book ).

    cut->validate_book_availability( EXPORTING it_keys  = CORRESPONDING #( lt_booking )
                                     CHANGING  reported = lt_reported
                                               failed   = lt_failed ).
    cl_abap_unit_assert=>assert_equals( msg = 'failed-booking-uuid'
                                        act = lt_failed-booking[ 1 ]-BookingUuid
                                        exp = 'FA163E6E92A51EECA49787B89FF9BC2E' ).
    cl_abap_unit_assert=>assert_not_initial( msg = 'initial reported '
                                             act = lt_reported ).
  ENDMETHOD.

ENDCLASS.
