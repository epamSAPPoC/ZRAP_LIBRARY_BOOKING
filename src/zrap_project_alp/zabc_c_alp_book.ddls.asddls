@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ALP.Book.Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@Search.searchable: true
define view entity ZABC_C_ALP_BOOK
  as select from ZABC_I_ALP_Book
{
      @Search: { defaultSearchElement: true,
                 ranking:  #HIGH,
                 fuzzinessThreshold: 0.8
                }
  key BookId,

      @Search: { defaultSearchElement: true,
                 ranking:  #HIGH,
                 fuzzinessThreshold: 0.8
                }
      BookName,

      @Consumption.valueHelpDefinition: [{ entity: { name: 'zabc_i_author',
                                                     element: 'AuthorId' } }]
      @ObjectModel.text.element: ['AuthorName']
      AuthorId,
      AuthorName,

      @Aggregation.default: #AVG
      PagesNum,

      @Aggregation.default: #AVG
      CopyQty,

      ImageUrl,

      /* Associations */
      _Author
}
