@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data model for book'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zabc_i_book
  as select from zabc_d_book
{
  key book_id   as BookId,
      book_name as BookName,
      author_id as AuthorId,
      pages_num as PagesNum,
      copy_qty  as CopyQty
}
