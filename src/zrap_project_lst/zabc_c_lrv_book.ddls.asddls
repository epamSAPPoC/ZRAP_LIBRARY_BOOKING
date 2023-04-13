@EndUserText.label: 'Consumption View'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: false  // true - allowed extends a CDS entity with CDS annotations (metadata Extension object)
@Search.searchable: true          // relevant for Fiori Elements and creates a general search bar.

//  Web resource: https://sapui5.hana.ondemand.com/sdk/test-resources/sap/m/demokit/iconExplorer/webapp/index.html
@UI.headerInfo: { typeImageUrl: 'sap-icon://education' } // Add Header TypeImageUrl icon
define view entity ZABC_C_LRV_BOOK
  as select from zabc_i_book
{
      @UI:{ facet: [{ label:          'Book Details',
                     purpose:         #QUICK_VIEW,
                     type:            #FIELDGROUP_REFERENCE,
                     targetQualifier: 'BookDetails'
                     }]
      }
  key BookId,

      @UI:{ lineItem:   [{ position:   10,              // Detail view
                           importance: #HIGH }]
//            fieldGroup: [{ qualifier:  'BookDetails',   // QuickView
//                           position:   1,
//                           type: #STANDARD,
//                           value: 'BookName' }]
      }
      @Search: { defaultSearchElement: true }
      BookName,
      
      @UI:{ lineItem:   [{ position:   20,              // Detail view
                           importance: #HIGH }],
            fieldGroup: [{ qualifier:  'BookDetails',   // QuickView
                           position:   2,
                           type: #STANDARD,
                           value: 'BookAuthorId' }]
      }
     @Search: { defaultSearchElement: true }
      AuthorId as BookAuthorId,

      @UI:{ fieldGroup: [{ qualifier:  'BookDetails',   // QuickView
                            position:   4,
                            type: #STANDARD,
                            value: 'BookPagesNum' }]
       }
      PagesNum as BookPagesNum,

      @UI:{ fieldGroup: [{ qualifier:  'BookDetails',   // QuickView
                            position:   5,
                            type: #STANDARD,
                            value: 'BookCopyQty' }]
       }
      CopyQty  as BookCopyQty
}
