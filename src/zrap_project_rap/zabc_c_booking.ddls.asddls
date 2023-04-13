@EndUserText.label: 'Booking'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true                         // allowed extends a CDS entity with CDS annotations (metadata Extension object)
@Search.searchable: true                                // relevant for Fiori Elements and creates a general search bar.
/* ==== moved to Metadata Extension
@UI.headerInfo: {                                       // define Entity Header characteristics: title, short description, the name of its entity in singular and plural form, image URLs
                typeName: 'Booking',                   // - title on detail Page
                typeNamePlural: 'Bookings',            // - Id on group maintenance page
                title.value: 'BookingUuid',            //
                description.value: '_Book.BookName'    //
              }
==== moved to Metadata Extension */

define root view entity ZABC_C_BOOKING
  provider contract                                     // defines the expected runtime is considered for your CDS Projection View
  transactional_query                                   // for a transactional RAP application
  as projection on ZABC_I_BOOKING
{
       /* ==== moved to Metadata Extension
              @UI.facet: [                                     //  @UI.facet defines facets for the object page
                            {
                                id:              'GeneralInformation',
                                type:            #COLLECTION,
                                label:           'General Information',
                                position:        10
                             },
                            {
                                id:              'AdminDataCollection',
                                type:            #COLLECTION,
                                label:           'Admin Data',
                                position:        20
                             },
                            {
                                id:              'BasicData',
                                purpose:         #STANDARD ,
                                parentId:        'GeneralInformation',
                                type:            #FIELDGROUP_REFERENCE,
                                label:           'Basic Data',
                                position:        10,
                                targetQualifier: 'BasicData'
                             },
                            {
                                id:              'ReaderData',
                                purpose:         #STANDARD ,
                                parentId:        'GeneralInformation',
                                type:            #FIELDGROUP_REFERENCE,
                                label:           'Reader',
                                position:        20,
                                targetQualifier: 'Reader'
                             }
                             ,
                            {
                                id:              'BookData',
                                purpose:         #STANDARD ,
                                parentId:        'GeneralInformation',
                                type:            #FIELDGROUP_REFERENCE,
                                label:           'Book',
                                position:        30,
                                targetQualifier: 'Book'
                             },
                            {
                                id:              'BookingTime',
                                purpose:         #STANDARD ,
                                parentId:        'GeneralInformation',
                                type:            #FIELDGROUP_REFERENCE,
                                label:           'Booking Time',
                                position:        40,
                                targetQualifier: 'BookingTime'
                             }]


              @UI.identification: [ { type: #FOR_ACTION,                     //@UI.identification: represents collection of spec.data fields with headerInfo identifies an entity to an end user
                                     dataAction: 'returnTheBook',
                                     label: 'Return the Book'
                                   }]
       ==== moved to Metadata Extension */

       //       @UI.hidden: true               //  ==== moved to Metadata Extension
  key  BookingUuid,

       @Search: { defaultSearchElement: true }
       /* ==== moved to Metadata Extension
              @UI.lineItem:       [{ position: 10, importance: #HIGH }]      // @UI.lineItem: provides a collection of data fields that is used in a table or a list
              @UI.fieldGroup:     [{ position: 10,                           // @UI.fieldGroup: provides a collection of data fields with a label for the group.
                                                                             //  UI.fieldGroup provides a single data instance in a form
                                     qualifier: 'BasicData' }]               //  qualifier: define field for fieldGroup.
        ==== moved to Metadata Extension */
       BookingId,

       @Consumption.valueHelpDefinition: [{ entity: { name: 'zabc_i_book',
                                                      element: 'BookId'} }]
       @Consumption.semanticObject: 'zabc_i_book'
       @ObjectModel.text.element: ['BookName']
       /* ==== moved to Metadata Extension
             @UI.selectionField: [{ position: 20 }]                         // @UI.selectionField: the field is used in an initial page as filter bar
             @UI.lineItem:       [{ position: 20, importance: #HIGH }]
             @UI.fieldGroup:     [{ position: 20, qualifier: 'Book' }]
      ==== moved to Metadata Extension */
       BookId,
       _Book.BookName   as BookName,

       @Consumption.valueHelpDefinition: [{ entity: { name: 'zabc_i_reader',  // @Consumption.valueHelpDefinition: connect to entity that has Values for Help.
                                                      element: 'PersonId'} }]
       @Consumption.semanticObject: 'zabc_i_reader'
       @ObjectModel.text.element: ['ReaderFullName']
       @UI.selectionField: [{ position: 30 }]
       @UI.lineItem:       [{ position: 30, importance: #HIGH }]
       @UI.fieldGroup:     [{ position: 30, qualifier: 'Reader' }]
       PersonId,
       _Reader.FullName as ReaderFullName,

       /* ==== moved to Metadata Extension
             @UI.selectionField: [{ position: 40 }]
             @UI.lineItem:       [{ position: 40, importance: #HIGH }]
             @UI.fieldGroup:     [{ position: 40, qualifier: 'Book' }]
       ==== moved to Metadata Extension */
       BookingStatus,

       @Consumption.filter: { selectionType: #INTERVAL ,        // @Consumption.filter: enables filtering elements of the underlying view. A filter should be specified before executing a query on the view
                              multipleSelections:  false }
       /* ==== moved to Metadata Extension
             @UI.selectionField: [{ position: 50 }]
             @UI.lineItem:       [{ position: 50, importance: #HIGH }]
             @UI.fieldGroup:     [{ position: 50, qualifier: 'BookingTime' }]
             @UI.dataPoint: { title: 'Start Date' }
       ==== moved to Metadata Extension */
       BookingBegDate,

       @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
       /* ==== moved to Metadata Extension
             @UI.selectionField: [{ position: 60 }]
             @UI.lineItem:       [{ position: 60, importance: #MEDIUM }]
             @UI.fieldGroup:     [{ position: 60, qualifier: 'BookingTime' }]
        ==== moved to Metadata Extension */
       BookingBegTime,

       @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
       /* ==== moved to Metadata Extension
             @UI.selectionField: [{ position: 70 }]
             @UI.lineItem:       [{ position: 70, importance: #HIGH }]
             @UI.fieldGroup:     [{ position: 70, qualifier: 'BookingTime' }]
             @UI.dataPoint: { title: 'End Date' }
       ==== moved to Metadata Extension */
       BookingEndDate,

       @Consumption.filter: { selectionType: #INTERVAL , multipleSelections:  false }
       /* ==== moved to Metadata Extension
             @UI.lineItem:       [{ position: 80, importance: #MEDIUM }]
             @UI.fieldGroup:     [{ position: 80, qualifier: 'BookingTime' }]
             @UI.selectionField: [{ position: 80 }]
       ==== moved to Metadata Extension */
       BookingEndTime,

       /* Associations */
       _Book,
       _Reader
}
