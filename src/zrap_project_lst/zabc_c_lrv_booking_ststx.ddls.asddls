@AbapCatalog.sqlViewName: 'ZABCBOOKSTSTX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS consumption view. Booking Sts.Descr.'
define view ZABC_C_LRV_BOOKING_STSTX
  as select from ZABC_I_LRV_BOOKING
  //association to parent target_data_source_name as _association_name
  //  on $projection.element_name = _association_name.target_element_name {
  //
  //  _association_name // Make association public
  //}

{
  key BookingUuid,
      BookingStatus,
      case BookingStatus
        when '2' then 'Booking is active'     //  Booked books
        else          'Booking is completed'  //  Booking is completed
      end           as BookingStatusDescr

}
