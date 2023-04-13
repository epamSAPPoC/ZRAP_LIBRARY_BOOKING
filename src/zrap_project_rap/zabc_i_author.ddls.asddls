@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Author'
define root view entity zabc_i_author
  as select from zabc_d_author
  association [0..1] to zabc_i_author_t as _AuthorTxt on  _AuthorTxt.Language = $session.system_language
                                                      and _AuthorTxt.AuthorId = $projection.AuthorId
{
  key author_id                                                                   as AuthorId,
      birth_date                                                                  as BirthDate,
      country                                                                     as Country,
      _AuthorTxt.AuthorLastName                                                   as AuthorLastName,
      _AuthorTxt.AuthorFirstName                                                  as AuthorFirstName,
      concat_with_space(_AuthorTxt.AuthorFirstName, _AuthorTxt.AuthorLastName, 1) as AuthorFullName,

      /* associations */
      _AuthorTxt
}
