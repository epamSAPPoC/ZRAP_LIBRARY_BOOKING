@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true                         // allowed extends a CDS entity with CDS annotations (metadata Extension object)
@Search.searchable: true                                // relevant for Fiori Elements and creates a general search bar.

define view entity ZABC_C_LRV_BOOKING
  as select from ZABC_I_LRV_BOOKING
  association [1..1] to ZABC_C_LRV_BOOK          as _BookDetails   on _BookDetails.BookId = $projection.BookId        // For each Booking we have 1 author
  association [1..1] to ZABC_C_LRV_READER        as _ReaderDetails on _ReaderDetails.PersonId = $projection.PersonId  // For each Booking we have 1 reader
  association [1..1] to ZABC_C_LRV_AUTHOR        as _AuthorDetails on _AuthorDetails.AuthorId = $projection.AuthorId  // For each Booking we have 1 author
  association [1..1] to ZABC_C_LRV_BOOKING_STSTX as _BookStat      on $projection.BookingUuid = _BookStat.BookingUuid // For each Booking we have 1 status

{
  key  BookingUuid,

       @Search: { defaultSearchElement: true }
       BookingId,

       @ObjectModel: { foreignKey.association: '_BookDetails' }   // Quick View:
       @ObjectModel.text.element: ['BookName']
       BookId,
       _Book.BookName               as BookName,

       @ObjectModel: { foreignKey.association: '_AuthorDetails' } // Quick View:
       @ObjectModel.text.element: ['AuthorFullName']
       _Book.AuthorId               as AuthorId,
       _Author.AuthorFullName       as AuthorFullName,

       @ObjectModel: { foreignKey.association: '_ReaderDetails' } // Quick View:
       @ObjectModel.text.element: ['ReaderFullName']
       PersonId,
       _Reader.FullName             as ReaderFullName,

       @ObjectModel.text.element: ['BookingStatusDescr']
       BookingStatus,
       _BookStat.BookingStatusDescr as BookingStatusDescr,

       BookingBegDate,

       BookingBegTime,

       BookingEndDate,

       BookingEndTime,

       @Aggregation.default: #SUM
       BookingQuantity,

       @Aggregation.default: #SUM
       @EndUserText.label:     'Duration (days)'
       @EndUserText.quickInfo: 'Booking duration in days'
       BookingDuration,

       /* Associations */
       _Book,
       _BookDetails,
       _ReaderDetails,
       _AuthorDetails
}
