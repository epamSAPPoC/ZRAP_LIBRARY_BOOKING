@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data model for reader'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zabc_i_reader
  as select from zabc_d_reader
  association [0..1] to zabc_d_reader_t as _ReaderTxt on  _ReaderTxt.langu     = $session.system_language
                                                      and _ReaderTxt.person_id = $projection.PersonId
{
  key person_id                                                                         as PersonId,
      _ReaderTxt.person_first_name                                                      as FirstName,
      _ReaderTxt.person_last_name                                                       as LastName,
      birth_date                                                                        as BirthDate,
      phone_number                                                                      as PhoneNumber,
      concat_with_space( _ReaderTxt.person_first_name, _ReaderTxt.person_last_name, 1 ) as FullName,

      /* Associations */
      _ReaderTxt
}
