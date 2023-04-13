@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'OVP: Booking.Interface View'
define view entity zabc_i_ovp_booking
  as select from zabc_d_booking_a
  association [1..1] to zabc_i_reader as _Reader on $projection.PersonId = _Reader.PersonId
  association [1..1] to zabc_i_book   as _Book   on $projection.BookId = _Book.BookId
{
  key booking_uuid                                              as BookingUuid,
      booking_id                                                as BookingId,
      book_id                                                   as BookId,
      person_id                                                 as PersonId,
      concat_with_space(_Reader.FirstName, _Reader.LastName, 1) as PersonName,
      booking_beg_date                                          as BookingBegDate,
      booking_beg_time                                          as BookingBegTime,
      booking_end_date                                          as BookingEndDate,
      booking_end_time                                          as BookingEndTime,
      1                                                         as BookQuantity,

      /* Associations */
      _Reader,
      _Book
}
