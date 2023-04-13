@AbapCatalog.viewEnhancementCategory: [#NONE]       // defines how CDS can be extended by View extension (values #GROUP_BY, #NONE, #PROJECTION_LIST, #UNION)
@AccessControl.authorizationCheck: #NOT_REQUIRED    // Access Control is not defined for Entity
@EndUserText.label: 'Data model for booking'        // Short Text is depended of Translation
@Metadata.ignorePropagatedAnnotations: true         // the derived annotations aren’t inherited from the used objects, if False then vice versa
@ObjectModel.usageType:{
    serviceQuality: #X,                             // - the view is built to push down app code to HANA
    sizeCategory: #S,                               // - expended <1000 rows
    dataClass: #MIXED                               // - for Data that changed frequently
}

// =={ Initial value }
//define view entity zabc_i_booking as select from ZABC_D_BOOKING_A { ....

// =={ Extended value
define root view entity ZABC_I_BOOKING              // This CDS - the root entity, represent hierarchy of  ‘Booking’ Business Object
  as select from zabc_d_booking_a
  association [1..1] to zabc_i_reader as _Reader on $projection.PersonId = _Reader.PersonId   // For each Booking we have 1 Reader 
  association [1..1] to zabc_i_book   as _Book   on $projection.BookId = _Book.BookId         // For each Booking we have 1 Book
// ==} Extended value

{
  key booking_uuid     as BookingUuid,
      booking_id       as BookingId,
      book_id          as BookId,
      person_id        as PersonId,
      booking_status   as BookingStatus,
      booking_beg_date as BookingBegDate,
      booking_beg_time as BookingBegTime,
      booking_end_date as BookingEndDate,
      booking_end_time as BookingEndTime,

// =={ Associations }
      _Reader,
      _Book
}
