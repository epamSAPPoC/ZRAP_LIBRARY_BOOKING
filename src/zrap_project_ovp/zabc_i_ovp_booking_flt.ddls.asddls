@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'OVP: Booking'
@Search.searchable: true
define view entity zabc_i_ovp_booking_flt
  as select from zabc_d_booking_a
  association [1..1] to zabc_i_reader as _Reader on $projection.PersonId = _Reader.PersonId
  association [1..1] to zabc_i_book   as _Book   on $projection.BookId = _Book.BookId
{
  key booking_uuid                                              as BookingUuid,
      @Search: { defaultSearchElement: true }
      booking_id                                                as BookingId,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'zabc_i_book', element: 'BookId'} }]
      @ObjectModel.text.element: ['BookName']
      book_id                                                   as BookId,
      _Book.BookName                                            as BookName,
      @UI.selectionField: [{ position: 10 }]
      @Consumption.valueHelpDefinition: [{ entity: { name: 'zabc_i_reader', element: 'PersonId'} }]
      @ObjectModel.text.element: ['PersonName']
      person_id                                                 as PersonId,
      concat_with_space(_Reader.FirstName, _Reader.LastName, 1) as PersonName,
      @UI.selectionField: [{ position: 20 }]
      @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
      booking_beg_date                                          as BookingBegDate,
      @UI.selectionField: [{ position: 25 }]
      @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
      booking_beg_time                                          as BookingBegTime,
      @UI.selectionField: [{ position: 30 }]
      @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
      booking_end_date                                          as BookingEndDate,
      @UI.selectionField: [{ position: 35 }]
      @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
      booking_end_time                                          as BookingEndTime,

      /* Associations */
      _Reader,
      _Book
}
