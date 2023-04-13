@EndUserText.label: 'Consumption View'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: false  // true - allowed extends a CDS entity with CDS annotations (metadata Extension object)
@Search.searchable: true          // relevant for Fiori Elements and creates a general search bar.

//  Web resource: https://sapui5.hana.ondemand.com/sdk/test-resources/sap/m/demokit/iconExplorer/webapp/index.html
@UI.headerInfo: { typeImageUrl: 'sap-icon://learning-assistant' } // Add Header TypeImageUrl icon
define view entity ZABC_C_LRV_READER
  as select from zabc_i_reader
{
      @UI:{ facet: [{ label:           'Reader Details',
                      purpose:         #QUICK_VIEW,
                      type:            #FIELDGROUP_REFERENCE,
                      targetQualifier: 'ReaderDetails'
                     }]
      }
  key PersonId,

      @UI:{ lineItem:   [{ position:   10,                // Detail view
                           importance: #HIGH }]
      }
      @Search: { defaultSearchElement: true }
      FirstName   as ReaderFirstName,

      @UI:{ lineItem:   [{ position:   20,                // Detail view
                           importance: #HIGH }]
      }
      @Search: { defaultSearchElement: true }
      LastName    as ReaderLastName,

      @UI:{ lineItem:   [{ position:   30,                // Detail view
                           importance: #HIGH }],
            fieldGroup: [{ qualifier:  'ReaderDetails',   // QuickView
                           position:   2,
                           type: #STANDARD,
                           value: 'ReaderBirthDate' }]
      }
      @Search: { defaultSearchElement: true }
      BirthDate   as ReaderBirthDate,

      @UI:{ lineItem:   [{ position:   40,                // Detail view
                           importance: #HIGH }],
            fieldGroup: [{ qualifier:  'ReaderDetails',   // QuickView
                           position:   3,
                           type: #STANDARD,
                           value: 'ReaderPhoneNumber' }]
      }
      @Search: { defaultSearchElement: true }
      PhoneNumber as ReaderPhoneNumber,

      @UI:{ lineItem:   [{ position:   20,                // Detail view
                           importance: #HIGH }],
            fieldGroup: [{ qualifier:  'AuthorDetails',   // QuickView
                           position:   1,
                           type: #STANDARD,
                           value: 'ReaderFullName' }]
      }
      FullName    as ReaderFullName,

      /* Associations */
      _ReaderTxt
}
