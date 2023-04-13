CLASS zcl_abc_generate_data_booking DEFINITION PUBLIC
                                               FINAL
                                               CREATE PUBLIC.
  PUBLIC SECTION.
* --------------------------
    INTERFACES if_oo_adt_classrun .

* ----------------- run class in Console mode
    CONSTANTS:
      ci_nr_gen_booking_entries TYPE i         VALUE 100,
      cc_init_db_flag           TYPE abap_bool VALUE abap_false,  " | true  - clear + generate new DB entries
*                                                                 " | false - generate appending  DB entries
      ci_diff_bt_dates          TYPE i          VALUE 365.        "

    CONSTANTS:
      ci_booking_complete TYPE i VALUE 1,
      ci_booking_active   TYPE i VALUE 2.
    CONSTANTS:
      cc_min TYPE c LENGTH 3 VALUE 'MIN',
      cc_max TYPE c LENGTH 3 VALUE 'MAX'.


    METHODS:
      constructor.

  PROTECTED SECTION.
* --------------------------

  PRIVATE SECTION.
* --------------------------
    TYPES:
      t_date        TYPE d,
      t_time        TYPE t,
      tt_booking_db TYPE STANDARD TABLE OF zabc_d_booking_a
                      WITH KEY primary_key COMPONENTS booking_uuid booking_id.
* -------------
    DATA:
      mi_booking_all_gen_rows TYPE i,  " number generated rows including irrelevant
      mi_booking_rel_gen_rows TYPE i,  " number generated relevant rows
      mn_last_booking_id      TYPE zabc_d_booking_a-booking_id.
    DATA:
      md_beg_date    TYPE t_date,
      md_end_date    TYPE t_date,
      mt_beg_time    TYPE t_time,
      mt_end_time    TYPE t_time,
      mt_booking     TYPE tt_booking_db,
      mt_readers     TYPE STANDARD TABLE OF zabc_d_reader WITH EMPTY KEY,
      mt_readers_txt TYPE STANDARD TABLE OF zabc_d_reader_t WITH EMPTY KEY,
      mt_authors     TYPE STANDARD TABLE OF zabc_d_author WITH EMPTY KEY,
      mt_authors_txt TYPE STANDARD TABLE OF zabc_d_author_t WITH EMPTY KEY,
      mt_books       TYPE STANDARD TABLE OF zabc_d_book WITH EMPTY KEY.

    DATA: _mo_rnd_seed TYPE REF TO cl_abap_random,
          _mi_rnd_seed TYPE i.

* -------------
    METHODS:
      fill_booking
        RETURNING VALUE(ri_entries) TYPE int8.

    METHODS:
      fill_authors RETURNING VALUE(ri_entries) TYPE int8,
      fill_books   RETURNING VALUE(ri_entries) TYPE int8,
      fill_readers RETURNING VALUE(ri_entries) TYPE int8.

    METHODS:
      generate_booking_db
        RETURNING VALUE(rt_booking_data) TYPE tt_booking_db,
      chk_relevance_booking_entry
        IMPORTING is_db_row        TYPE zabc_d_booking_a
        RETURNING VALUE(rc_return) TYPE abap_bool,
      chk_booking_dates_overlap
        IMPORTING id_new_beg_date  TYPE t_date
                  id_new_end_date  TYPE t_date
                  id_db_beg_date   TYPE t_date
                  id_db_end_date   TYPE t_date
        RETURNING VALUE(rc_return) TYPE abap_bool,
      get_db_booking,
      get_db_last_booking_id
        RETURNING VALUE(rn_booking_id) TYPE zabc_d_booking_a-booking_id,
      get_booking_uuid
        RETURNING VALUE(rx_booking_uuid) TYPE zabc_d_booking_a-booking_uuid,
      get_next_booking_id
        RETURNING VALUE(rn_booking_id) TYPE zabc_d_booking_a-booking_id,
      revert_booking_id,
      get_rnd_date_from_range
        IMPORTING id_beg_date    TYPE t_date
                  id_end_date    TYPE t_date
        RETURNING VALUE(rd_date) TYPE t_date,
      get_rnd_time_from_range
        IMPORTING it_beg_time    TYPE t_time OPTIONAL
                  it_end_time    TYPE t_time OPTIONAL
        RETURNING VALUE(rt_time) TYPE t_time,
      get_rnd_book_id
        RETURNING VALUE(rn_rnd_value) TYPE zabc_book_id ,
      get_rnd_person_id
        RETURNING VALUE(rn_rnd_value) TYPE zabc_person_id ,
      get_rnd_bookinig_status
        RETURNING VALUE(rc_rnd_value) TYPE zabc_booking_status,
      chk_book_available_by_qnt
        IMPORTING in_book_id       TYPE zabc_d_booking_a-book_id
                  id_chk_date      TYPE t_date
        RETURNING VALUE(rc_return) TYPE abap_bool.

    METHODS:
      get_book_id_margin
        IMPORTING ic_margin         TYPE clike
        RETURNING VALUE(rn_book_id) TYPE zabc_d_book-book_id,
      get_person_id_margin
        IMPORTING ic_margin           TYPE clike
        RETURNING VALUE(rn_person_id) TYPE zabc_d_reader-person_id.

    METHODS:
      _init_rnd_seed,

      _get_random_date_from_range
        IMPORTING id_beg_date         TYPE t_date
                  id_end_date         TYPE t_date
        RETURNING VALUE(rd_rnd_value) TYPE t_date,

      _get_random_time_from_range
        IMPORTING it_beg_time         TYPE t_time
                  it_end_time         TYPE t_time
        RETURNING VALUE(rt_rnd_value) TYPE t_time,

      _get_random_int_from_range
        IMPORTING ii_low              TYPE i
                  ii_high             TYPE i
        RETURNING VALUE(ri_rnd_value) TYPE i.

ENDCLASS.



CLASS ZCL_ABC_GENERATE_DATA_BOOKING IMPLEMENTATION.


  METHOD constructor.
    mn_last_booking_id = me->get_db_last_booking_id( ).

* -- initialize dates/times for random range
    md_end_date = sy-datum.
    md_beg_date = md_end_date - ci_diff_bt_dates.
    mt_end_time = '235959'.
    mt_beg_time = '000000'.

* -- initialize randomize generators
    me->_init_rnd_seed( ).

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    out->write( |<<< Started: { sy-datum } { sy-uzeit }| ).
    out->write( |Library Data generation: mode - { SWITCH string( cc_init_db_flag WHEN abap_true THEN 'DB Intitialization'
                                                                                                 ELSE 'Appending DB entries' ) }| ).
    DATA(li_authors) = me->fill_authors(  ).
    out->write( | - Authors lines - { li_authors }| ).

    DATA(li_books)   = me->fill_books(  ).
    out->write( | - Books lines - { li_books }| ).

    DATA(li_readers) = me->fill_readers(  ).
    out->write( | - Readers lines - { li_readers }| ).

    DATA(li_booking) = me->fill_booking(  ).
    out->write( | - Booking lines - { li_booking }| ).
    out->write( |  -- Generated rows: { mi_booking_all_gen_rows }| ).
    out->write( |  -- Relevant rows : { mi_booking_rel_gen_rows }| ).
    out->write( mt_booking ).
    out->write( |>>> Complete { sy-datum } { sy-uzeit }| ).
  ENDMETHOD.


  METHOD fill_readers.
    mt_readers  = VALUE #( ( person_id = '1' birth_date = '20010101' phone_number = '+995 (111) 111-11-11' )  " GE
                           ( person_id = '2' birth_date = '20020202' phone_number = '+44 (222) 222-22-22' )   " UK
                           ( person_id = '3' birth_date = '20030303' phone_number = '+44 (333) 333-33-33' )   " UK
                           ( person_id = '4' birth_date = '20040404' phone_number = '+34 (444) 444-44-44' )   " ES
                           ( person_id = '5' birth_date = '20050505' phone_number = '+995 (555) 555-55-55' )  " MX
                           ( person_id = '6' birth_date = '20060606' phone_number = '+52 (666) 666-66-66' )   " DE
                           ( person_id = '7' birth_date = '20070707' phone_number = '+45 (777) 777-77-77' )   " DM
                           ( person_id = '8' birth_date = '20080808' phone_number = '+45 (888) 888-88-88' )   " SE
                           ( person_id = '9' birth_date = '20090909' phone_number = '+45 (999) 999-99-99' )   " NO
                         ).
    mt_readers_txt = VALUE #( ( person_id = '1'  langu = 'E' person_first_name = 'Anonymous'   person_last_name = 'V'      )
                              ( person_id = '2'  langu = 'E' person_first_name = 'John'        person_last_name = 'Doe'    )
                              ( person_id = '3'  langu = 'E' person_first_name = 'John'        person_last_name = 'Smith'  )
                              ( person_id = '4'  langu = 'E' person_first_name = 'Francisco'   person_last_name = 'Garcia' )
                              ( person_id = '5'  langu = 'E' person_first_name = 'José'        person_last_name = 'Perez'  )
                              ( person_id = '6'  langu = 'E' person_first_name = 'Hanz'        person_last_name = 'Muller' )
                              ( person_id = '7'  langu = 'E' person_first_name = 'Axel'        person_last_name = 'Nilsen' )
                              ( person_id = '8'  langu = 'E' person_first_name = 'Saga'        person_last_name = 'Nuren' )
                              ( person_id = '9'  langu = 'E' person_first_name = 'Biorn'       person_last_name = 'Holfstren' )
                            ).

    SORT:
      mt_readers     BY person_id,
      mt_readers_txt BY person_id.

    CASE cc_init_db_flag.
      WHEN abap_true.
        DELETE FROM: zabc_d_reader, zabc_d_reader_t.
        INSERT: zabc_d_reader   FROM TABLE @mt_readers,
                zabc_d_reader_t FROM TABLE @mt_readers_txt.

      WHEN abap_false.
        SELECT * FROM:
          zabc_d_reader   APPENDING TABLE @mt_readers,
          zabc_d_reader_t APPENDING TABLE @mt_readers_txt.
        SORT:
          mt_readers     BY person_id birth_date phone_number,
          mt_readers_txt BY person_id langu person_first_name person_last_name.
        DELETE ADJACENT DUPLICATES FROM:
          mt_readers     COMPARING person_id birth_date phone_number,
          mt_readers_txt COMPARING person_id langu person_first_name person_last_name.
        MODIFY: zabc_d_reader   FROM TABLE @mt_readers,
                zabc_d_reader_t FROM TABLE @mt_readers_txt.
    ENDCASE.

    ri_entries = lines( mt_readers ).
  ENDMETHOD.


  METHOD fill_authors.
    mt_authors  = VALUE #( ( author_id = '1' birth_date = '15640429' country = 'UK' ) " William Shakespeare
                           ( author_id = '2' birth_date = '18280909' country = 'RU' ) " Leo Tolstoy
                           ( author_id = '3' birth_date = '18910515' country = 'RU' ) " Mikhail Bulgakov
                           ( author_id = '4' birth_date = '19470921' country = 'US' ) " Stephen King
                           ( author_id = '5' birth_date = '19311019' country = 'UK' ) " John le Carré
                           ( author_id = '6' birth_date = '19490112' country = 'JP' ) " Haruki Murakami
                         ).
    mt_authors_txt = VALUE #( ( author_id = '1'  langu = 'E' author_first_name = 'William' author_last_name = 'Shakespeare' )
                              ( author_id = '2'  langu = 'E' author_first_name = 'Leo'     author_last_name = 'Tolstoy'     )
                              ( author_id = '3'  langu = 'E' author_first_name = 'Mikhail' author_last_name = 'Bulgakov'    )
                              ( author_id = '4'  langu = 'E' author_first_name = 'Stephen' author_last_name = 'King' )
                              ( author_id = '5'  langu = 'E' author_first_name = 'John'    author_last_name = 'le Carré' )
                              ( author_id = '6'  langu = 'E' author_first_name = 'Haruki'  author_last_name = 'Murakami' )
                            ).
    SORT:
      mt_authors     BY author_id,
      mt_authors_txt BY author_id.

    CASE cc_init_db_flag.
      WHEN abap_true.
        DELETE FROM: zabc_d_author, zabc_d_author_t.
        INSERT:
          zabc_d_author   FROM TABLE @mt_authors,
          zabc_d_author_t FROM TABLE @mt_authors_txt.

      WHEN abap_false.
        SELECT * FROM:
          zabc_d_author   APPENDING TABLE @mt_authors,
          zabc_d_author_t APPENDING TABLE @mt_authors_txt.
        SORT: mt_authors     BY author_id birth_date country,
              mt_authors_txt BY author_id langu author_first_name author_last_name.
        DELETE ADJACENT DUPLICATES FROM:
          mt_authors     COMPARING author_id birth_date country,
          mt_authors_txt COMPARING author_id langu author_first_name author_last_name.
        MODIFY:
          zabc_d_author   FROM TABLE @mt_authors,
          zabc_d_author_t FROM TABLE @mt_authors_txt.
    ENDCASE.

    ri_entries = lines( mt_authors ).
  ENDMETHOD.


  METHOD fill_books.
    mt_books = VALUE #(
                        ( book_id = '01' book_name = 'Hamlet'                   author_id = '1'  pages_num = 101  copy_qty = 2 ) " William Shakespeare
                        ( book_id = '02' book_name = 'King Lear'                author_id = '1'  pages_num = 102  copy_qty = 1 ) " William Shakespeare
                        ( book_id = '03' book_name = 'The Tampest'              author_id = '1'  pages_num = 103  copy_qty = 3 ) " William Shakespeare
                        ( book_id = '04' book_name = 'Macbeth'                  author_id = '1'  pages_num = 104  copy_qty = 1 ) " William Shakespeare
                        ( book_id = '05' book_name = 'War and Peace'            author_id = '2'  pages_num = 205  copy_qty = 2 ) " Leo Tolstoy
                        ( book_id = '06' book_name = 'Anna Karenina'            author_id = '2'  pages_num = 206  copy_qty = 4 ) " Leo Tolstoy
                        ( book_id = '07' book_name = 'The Master and Margarita' author_id = '3'  pages_num = 307  copy_qty = 6 ) " Mikhail Bulgakov
                        ( book_id = '08' book_name = 'The Thing'                author_id = '4'  pages_num = 408  copy_qty = 2 ) " Stephen King
                        ( book_id = '09' book_name = 'Black Tower'              author_id = '4'  pages_num = 409  copy_qty = 2 ) " Stephen King
                        ( book_id = '10' book_name = 'Tinker-Tailor-Soldie-Spy' author_id = '5'  pages_num = 510  copy_qty = 2 ) " John le Carré
                        ( book_id = '11' book_name = 'Smyley People'            author_id = '5'  pages_num = 511  copy_qty = 2 ) " John le Carré
                        ( book_id = '12' book_name = 'A Wild Sheep Chase'       author_id = '6'  pages_num = 612  copy_qty = 1 ) " Haruki Murakami
                        ( book_id = '13' book_name = 'Dance-Dance-Dance'        author_id = '6'  pages_num = 613  copy_qty = 1 ) " Haruki Murakami
                         ).
    SORT mt_books BY book_id.

    CASE cc_init_db_flag.
      WHEN abap_true.
        DELETE FROM: zabc_d_book.
        INSERT: zabc_d_book   FROM TABLE @mt_books.

      WHEN abap_false.
        SELECT * FROM zabc_d_book APPENDING TABLE @mt_books.
        SORT mt_books BY book_id book_name author_id pages_num copy_qty.
        DELETE ADJACENT DUPLICATES FROM mt_books COMPARING book_id book_name author_id pages_num copy_qty.
        MODIFY: zabc_d_book FROM TABLE @mt_books.
    ENDCASE.

    ri_entries = lines( mt_books ).
  ENDMETHOD.


  METHOD fill_booking.
    CASE cc_init_db_flag.
      WHEN abap_true.
        DELETE FROM: zabc_d_booking_d,     " Clear Drafts
                     zabc_d_booking_a.     " Clear persistent
        me->generate_booking_db( ).
        INSERT zabc_d_booking_a FROM TABLE @mt_booking.

      WHEN abap_false.
        me->get_db_booking( ).      " select data from DB
        me->generate_booking_db( ).
        MODIFY zabc_d_booking_a FROM TABLE @mt_booking.
    ENDCASE.
    ri_entries = lines( mt_booking ).

  ENDMETHOD.


  METHOD get_booking_uuid.
*        RETURNING VALUE(rx_booking_uuid) TYPE zabc_d_booking_a-booking_uuid,
*
* -----------------------------------------------
* ---- Generate uuid
    DO 10 TIMES.
      TRY.
          DATA(lv_id) =  cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error .
*            <...>
      ENDTRY.

      CHECK NOT line_exists( mt_booking[ booking_uuid = lv_id ] ).
      rx_booking_uuid = lv_id.
      RETURN.
    ENDDO.

  ENDMETHOD.


  METHOD _get_random_int_from_range.
    DATA(lo_rand)   = cl_abap_random=>create( _mo_rnd_seed->int( ) ).
    DATA(li_random) = lo_rand->intinrange( low  = ii_low
                                           high = ii_high ).
    ri_rnd_value = li_random.
  ENDMETHOD.


  METHOD chk_book_available_by_qnt.
*        IMPORTING in_book_id       TYPE zabc_d_booking_a-book_id
*                  id_chk_date      TYPE t_date
*        RETURNING VALUE(rc_return) TYPE abap_bool.
*
* -----------------------------------------------
    rc_return = abap_false.

    DO 1 TIMES.
      CHECK line_exists( mt_books[ book_id = in_book_id ] ).
      DATA(wa_books) =  mt_books[ book_id = in_book_id ].

* -- check whether Book has reserve by Quantity on certain date
      DATA(li_active_bookings) = REDUCE i( INIT i = 0
                                          FOR wa_booking IN mt_booking
                                            WHERE ( book_id            = in_book_id   AND
                                                    ( booking_beg_date > id_chk_date  AND " for complete Bookings
                                                      booking_end_date < id_chk_date ) OR
                                                    ( booking_beg_date > id_chk_date  AND " for active Bookings
                                                      booking_end_date IS INITIAL ) )
                                          NEXT i = i + 1 ).
      CHECK wa_books-copy_qty > li_active_bookings.
      rc_return = abap_true.
    ENDDO.

  ENDMETHOD.


  METHOD _init_rnd_seed.
    DATA: lp_tsl TYPE timestampl. " UTC Time Stamp in Long Form (YYYYMMDDhhmmssmmmuuun)
    GET TIME STAMP FIELD lp_tsl.
    DATA(lv_timestamp_l) = CONV string( lp_tsl ).

    DATA: li_seed TYPE i.
    _mi_rnd_seed = frac( lp_tsl ) * 10000000.  " only decimal part

* -- Seed generator
    _mo_rnd_seed = cl_abap_random=>create( _mi_rnd_seed ).
  ENDMETHOD.


  METHOD get_book_id_margin.
*        IMPORTING ic_margin TYPE clike
*        RETURNING VALUE(rn_book_id) TYPE zabc_d_book-book_id
*
* -----------------------------------------------
    DO 1 TIMES.
      CHECK lines( mt_books ) > 0.
      CASE ic_margin.
        WHEN cc_min.
          DATA(_wa_book) =  VALUE #( mt_books[ 1 ] ).
        WHEN cc_max.
          _wa_book =  VALUE #( mt_books[ lines( mt_books ) ] ).
        WHEN OTHERS.
          RETURN.
      ENDCASE.

      rn_book_id = _wa_book-book_id.
    ENDDO.
  ENDMETHOD.


  METHOD get_db_booking.
    SELECT * FROM zabc_d_booking_a
       INTO TABLE @mt_booking.

  ENDMETHOD.


  METHOD get_person_id_margin.
*        IMPORTING ic_margin         TYPE clike
*        RETURNING VALUE(rn_person_id) TYPE zabc_d_reader-person_id.
*
* -----------------------------------------------
    DO 1 TIMES.
      CHECK lines( mt_readers ) > 0.
      CASE ic_margin.
        WHEN cc_min.
          DATA(_wa_reader) =  VALUE #( mt_readers[ 1 ] ).
        WHEN cc_max.
          _wa_reader =  VALUE #( mt_readers[ lines( mt_readers ) ] ).
        WHEN OTHERS.
          RETURN.
      ENDCASE.

      rn_person_id = _wa_reader-person_id.
    ENDDO.
  ENDMETHOD.


  METHOD get_rnd_date_from_range.
*        IMPORTING id_beg_date    TYPE t_date
*                  id_end_date    TYPE t_date
*        RETURNING VALUE(rd_date) TYPE t_date
*
* -----------------------------------------------
    TRY.
        rd_date = me->_get_random_date_from_range( id_beg_date = id_beg_date
                                                   id_end_date = id_end_date ).
      CATCH cx_root.
        rd_date = md_end_date.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_rnd_time_from_range.
*        IMPORTING it_beg_time    TYPE t_time OPTIONAL
*                  it_end_time    TYPE t_time OPTIONAL
*        RETURNING VALUE(rt_time) TYPE t_time
*
* -----------------------------------------------
    DATA(li_beg_time) = COND t_time( WHEN it_beg_time IS SUPPLIED THEN it_beg_time
                                                                  ELSE mt_beg_time ).
    DATA(li_end_time) = COND t_time( WHEN it_end_time IS SUPPLIED THEN it_end_time
                                                                  ELSE mt_end_time ).
    TRY.
        rt_time = me->_get_random_time_from_range( it_beg_time = li_beg_time
                                                   it_end_time = li_end_time ).
      CATCH cx_root.
        rt_time = mt_end_time.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD chk_booking_dates_overlap.
*        IMPORTING id_new_beg_date  TYPE t_date
*                  id_new_end_date  TYPE t_date
*                  id_db_beg_date   TYPE t_date
*                  id_db_end_date   TYPE t_date
*        RETURNING VALUE(rc_return) TYPE abap_bool
*
* -----------------------------------------------
    rc_return = COND abap_bool( WHEN ( id_db_beg_date >= id_new_beg_date AND id_db_beg_date <= id_new_end_date ) OR
                                     ( id_db_end_date <= id_new_beg_date AND id_db_end_date <= id_new_end_date )
                                  THEN abap_true        " overlapping
                                  ELSE abap_false ).    " not overlapping
  ENDMETHOD.


  METHOD chk_relevance_booking_entry.
*        IMPORTING is_db_row        TYPE zabc_d_booking_a
*        RETURNING VALUE(rc_return) TYPE abap_bool
*
* -----------------------------------------------
    rc_return = abap_false.

* -- Check #0 - UUID is initial - not relevant
    IF  is_db_row-booking_uuid IS  INITIAL. RETURN. ENDIF.

* ---------------------------
* -- Prepare checking matrix
    TYPES:
      BEGIN OF t_matrix_key,
        aval_qnty      TYPE abap_bool,
        new_entry      TYPE abap_bool,
        overlap_dates  TYPE abap_bool,
        booikng_status TYPE abap_bool,
      END OF t_matrix_key,

      BEGIN OF t_matrix.
        INCLUDE TYPE t_matrix_key.
    TYPES:
        relevant TYPE abap_bool,
      END OF t_matrix,

      tt_matrix TYPE STANDARD TABLE OF t_matrix
                  WITH KEY primary_key COMPONENTS aval_qnty new_entry overlap_dates booikng_status.
*
*    +-----------+-----------+---------------+----------------+----------+
*    | aval_qnty | new_entry | overlap_dates | booikng_status | Is entry |
*    |   Book    |           |               |1-comp./2-active| relevant?|
*    +-----------+-----------+---------------+----------------+----------+
*    |     -     |     -     |       -       |       1        |     X    |
*    |     -     |     -     |       -       |       2        |     -    |
*    |     -     |     -     |       X       |       1        |     -    |
*    |     -     |     -     |       X       |       2        |     -    |
*    |     -     |     X     |       -       |       1        |     X    |
*    |     -     |     X     |       -       |       2        |     -    |
*    |     -     |     X     |       X       |       1        |     -    |
*    |     -     |     X     |       X       |       2        |     -    |
*    |     X     |     -     |       -       |       1        |     X    |
*    |     X     |     -     |       -       |       2        |     X    |
*    |     X     |     -     |       X       |       1        |     -    |
*    |     X     |     -     |       X       |       2        |     -    |
*    |     X     |     X     |       -       |       1        |     X    |
*    |     X     |     X     |       -       |       2        |     X    |
*    |     X     |     X     |       X       |       1        |     X    |
*    |     X     |     X     |       X       |       2        |     X    |
*    +-----------+-----------+---------------+----------------+----------+

    DATA(lt_relevant_matrix) = VALUE tt_matrix(
      ( aval_qnty = abap_false  new_entry = abap_false  overlap_dates = abap_false  booikng_status = ci_booking_complete relevant = abap_true  )
      ( aval_qnty = abap_false  new_entry = abap_false  overlap_dates = abap_false  booikng_status = ci_booking_active   relevant = abap_false )
      ( aval_qnty = abap_false  new_entry = abap_false  overlap_dates = abap_true   booikng_status = ci_booking_complete relevant = abap_false )
      ( aval_qnty = abap_false  new_entry = abap_false  overlap_dates = abap_true   booikng_status = ci_booking_active   relevant = abap_false )
      ( aval_qnty = abap_false  new_entry = abap_true   overlap_dates = abap_false  booikng_status = ci_booking_complete relevant = abap_true  )
      ( aval_qnty = abap_false  new_entry = abap_true   overlap_dates = abap_false  booikng_status = ci_booking_active   relevant = abap_false )
      ( aval_qnty = abap_false  new_entry = abap_true   overlap_dates = abap_true   booikng_status = ci_booking_complete relevant = abap_false )
      ( aval_qnty = abap_false  new_entry = abap_true   overlap_dates = abap_true   booikng_status = ci_booking_active   relevant = abap_false )
      ( aval_qnty = abap_true   new_entry = abap_false  overlap_dates = abap_false  booikng_status = ci_booking_complete relevant = abap_true  )
      ( aval_qnty = abap_true   new_entry = abap_false  overlap_dates = abap_false  booikng_status = ci_booking_active   relevant = abap_true  )
      ( aval_qnty = abap_true   new_entry = abap_false  overlap_dates = abap_true   booikng_status = ci_booking_complete relevant = abap_false )
      ( aval_qnty = abap_true   new_entry = abap_false  overlap_dates = abap_true   booikng_status = ci_booking_active   relevant = abap_false )
      ( aval_qnty = abap_true   new_entry = abap_true   overlap_dates = abap_false  booikng_status = ci_booking_complete relevant = abap_true  )
      ( aval_qnty = abap_true   new_entry = abap_true   overlap_dates = abap_false  booikng_status = ci_booking_active   relevant = abap_true  )
      ( aval_qnty = abap_true   new_entry = abap_true   overlap_dates = abap_true   booikng_status = ci_booking_complete relevant = abap_true  )
      ( aval_qnty = abap_true   new_entry = abap_true   overlap_dates = abap_true   booikng_status = ci_booking_active   relevant = abap_true  )
    ).

* -- generated line available/ not available by quantity
    DATA: ls_compare_key TYPE t_matrix_key.

    IF me->chk_book_available_by_qnt( in_book_id  = is_db_row-book_id
                                      id_chk_date = is_db_row-booking_beg_date ).
      ls_compare_key-aval_qnty = abap_true.
    ENDIF.

* -- generated line already has corresponding entry for book / person / booking status
    IF line_exists( mt_booking[ book_id = is_db_row-book_id  person_id = is_db_row-person_id  booking_status = is_db_row-booking_status ] ).
      DATA(wa) = mt_booking[ book_id = is_db_row-book_id  person_id = is_db_row-person_id  booking_status = is_db_row-booking_status ] .
* ---- booking dates overlapping
      IF chk_booking_dates_overlap( id_new_beg_date = is_db_row-booking_beg_date
                                    id_new_end_date = is_db_row-booking_end_date
                                    id_db_beg_date  = wa-booking_beg_date
                                    id_db_end_date  = wa-booking_end_date ).
        ls_compare_key-overlap_dates = abap_true.
      ENDIF.
    ELSE.
      ls_compare_key-new_entry = abap_true.
    ENDIF.

* -- generated line is active/ completed booking
    ls_compare_key-booikng_status = is_db_row-booking_status.

* -- set Checking relevance
    DO 1 TIMES.
      READ TABLE lt_relevant_matrix ASSIGNING FIELD-SYMBOL(<fs_result>)
        WITH KEY  aval_qnty      = ls_compare_key-aval_qnty
                  new_entry      = ls_compare_key-new_entry
                  overlap_dates  = ls_compare_key-overlap_dates
                  booikng_status = ls_compare_key-booikng_status.
      CHECK sy-subrc = 0.
      rc_return = <fs_result>-relevant.
    ENDDO.
  ENDMETHOD.


  METHOD generate_booking_db.

    DO ci_nr_gen_booking_entries TIMES.
      DATA(lc_booking_sts) = me->get_rnd_bookinig_status( ).
      DATA(ld_beg_date) = me->get_rnd_date_from_range( id_beg_date = md_beg_date
                                                       id_end_date = md_end_date ).
      DATA(ld_end_date) = SWITCH t_date( lc_booking_sts
                                          WHEN ci_booking_complete THEN me->get_rnd_date_from_range( id_beg_date = ld_beg_date
                                                                                                     id_end_date = md_end_date )
                                                                   ELSE '00000000' ).
      DATA(lt_end_time) = SWITCH t_time( lc_booking_sts
                                          WHEN ci_booking_complete THEN get_rnd_time_from_range( )
                                                                   ELSE '000000' ).
* -- fill the entry
      DATA(ls_booking_data) = VALUE zabc_d_booking_a( booking_uuid     = me->get_booking_uuid( )
                                                      booking_id       = me->get_next_booking_id( )
                                                      book_id          = me->get_rnd_book_id( )
                                                      person_id        = me->get_rnd_person_id( )
                                                      booking_status   = lc_booking_sts
                                                      booking_beg_date = ld_beg_date
                                                      booking_beg_time = me->get_rnd_time_from_range( )
                                                      booking_end_date = ld_end_date
                                                      booking_end_time = lt_end_time
                                                      ).
      mi_booking_all_gen_rows = mi_booking_all_gen_rows + 1.


      CASE me->chk_relevance_booking_entry( ls_booking_data ).
        WHEN abap_true.
          APPEND  ls_booking_data TO  mt_booking.
          mi_booking_rel_gen_rows = mi_booking_rel_gen_rows + 1.
        WHEN OTHERS.
          me->revert_booking_id( ).
      ENDCASE.
    ENDDO.

  ENDMETHOD.


  METHOD get_db_last_booking_id.
*        RETURNING VALUE(rn_booking_id) TYPE zabc_d_booking_a-booking_id
*
* -----------------------------------------------
    DO 1 TIMES.
      CHECK cc_init_db_flag = abap_false.  "
      SELECT MAX( booking_id ) FROM zabc_d_booking_a
      INTO (@DATA(ln_max_booking_id)).
      CHECK sy-subrc = 0.

      rn_booking_id = ln_max_booking_id.
    ENDDO.
  ENDMETHOD.


  METHOD get_next_booking_id.
*        RETURNING VALUE(rn_booking_id) TYPE zabc_d_booking_a-booking_id
*
* -----------------------------------------------
    rn_booking_id = mn_last_booking_id = mn_last_booking_id + 1.
  ENDMETHOD.


  METHOD get_rnd_bookinig_status.
*        RETURNING VALUE(rc_rnd_value) TYPE zabc_booking_status
*
* -----------------------------------------------
    TRY.
        rc_rnd_value = CONV #( me->_get_random_int_from_range( ii_low  = ci_booking_complete
                                                               ii_high = ci_booking_active ) ).
      CATCH cx_root.
        rc_rnd_value = 0.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_rnd_book_id.
*        RETURNING VALUE(rn_rnd_value) TYPE zabc_book_id
*
* -----------------------------------------------
    TRY.
        rn_rnd_value = CONV #( me->_get_random_int_from_range( ii_low  = CONV #( me->get_book_id_margin( cc_min ) )
                                                               ii_high = CONV #( me->get_book_id_margin( cc_max ) ) ) ).
      CATCH cx_root.
        rn_rnd_value = 0.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD get_rnd_person_id.
*        RETURNING VALUE(rn_rnd_value) TYPE zabc_book_id
*
* -----------------------------------------------
    TRY.
        rn_rnd_value = CONV #( me->_get_random_int_from_range( ii_low  = CONV #( me->get_person_id_margin( cc_min ) )
                                                               ii_high = CONV #( me->get_person_id_margin( cc_max ) ) ) ).
      CATCH cx_root.
        rn_rnd_value = 0.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD revert_booking_id.
    mn_last_booking_id = COND #( WHEN mn_last_booking_id > 0  THEN mn_last_booking_id - 1
                                                              ELSE 0 ).
  ENDMETHOD.


  METHOD _get_random_date_from_range.
    DATA(li_high)   = id_end_date - id_beg_date.
    DATA(lo_rand)   = cl_abap_random=>create( _mo_rnd_seed->int( ) ).
    DATA(li_random) = lo_rand->intinrange( low  = 0
                                           high = li_high ).

    rd_rnd_value = id_beg_date + li_random.
  ENDMETHOD.


  METHOD _get_random_time_from_range.
    DATA(li_high)   = it_end_time - it_beg_time.
    DATA(lo_rand)   = cl_abap_random=>create( _mo_rnd_seed->int( ) ).
    DATA(li_random) = lo_rand->intinrange( low  = 0
                                           high = li_high ).

    rt_rnd_value = it_beg_time + li_random.
  ENDMETHOD.
ENDCLASS.
