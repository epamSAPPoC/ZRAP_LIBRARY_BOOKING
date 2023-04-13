@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'OVP: Booking'
@Metadata.allowExtensions: true
define view entity zabc_c_ovp_booking
  as select from zabc_i_ovp_booking
{
  key BookingUuid,
      @Consumption.semanticObject: 'zabc_ui_booking'
      BookingId,
      @Consumption.semanticObject: 'zabc_ui_book'
      @ObjectModel.text.element: ['BookName']
      BookId,
      _Book.BookName as BookName,
      @Consumption.semanticObject: 'zabc_ui_reader'
      @ObjectModel.text.element: ['PersonName']
      PersonId,
      PersonName,
      BookingBegDate,
      BookingBegTime,
      BookingEndDate,
      BookingEndTime,
      @Aggregation.default: #SUM
      BookQuantity,

      /* Associations */
      _Book,
      _Reader
}
