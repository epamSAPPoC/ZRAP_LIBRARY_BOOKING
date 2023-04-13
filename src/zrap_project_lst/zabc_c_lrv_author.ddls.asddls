@EndUserText.label: 'Consumption View'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: false  // true - allowed extends a CDS entity with CDS annotations (metadata Extension object)
@Search.searchable: true          // relevant for Fiori Elements and creates a general search bar.
//@OData.publish: true  - requred to register in Backend via trx  /IWFND/MAINT_SERVICE and so on...

//  Web resource: https://sapui5.hana.ondemand.com/sdk/test-resources/sap/m/demokit/iconExplorer/webapp/index.html
@UI.headerInfo: { typeImageUrl: 'sap-icon://person-placeholder' } // Add Header TypeImageUrl icon
define view entity ZABC_C_LRV_AUTHOR
  as select from zabc_i_author
{
      @UI:{ facet: [{ label:           'Author Details',
                     purpose:         #QUICK_VIEW,
                     type:            #FIELDGROUP_REFERENCE,
                     targetQualifier: 'AuthorDetails'
                     }]
      }
  key AuthorId,

      @UI:{ lineItem:   [{ position:   30,                // Detail view
                           importance: #HIGH }],
            fieldGroup: [{ qualifier:  'AuthorDetails',   // QuickView
                           position:   2,
                           type: #STANDARD,
                           value: 'BirthDate' }]
      }
      BirthDate,

      @UI:{ lineItem:   [{ position:    40,               // Detail view
                        importance:  #HIGH }],
            fieldGroup: [{ qualifier:   'AuthorDetails',  // QuickView
                           position:    3,
                           type: #STANDARD,
                           value: 'Country' }]
      }
      Country,

      @Search: { defaultSearchElement: true }
      @UI:{ lineItem:   [{ position:   10,                // Detail view
                           importance: #HIGH }]
      }
      AuthorLastName,

      @Search: { defaultSearchElement: true }
      @UI:{ lineItem:   [{ position:   20,                // Detail view
                           importance: #HIGH }]
      }
      AuthorFirstName,

      AuthorFullName,

      /* Associations */
      _AuthorTxt
}
