(*---
fsharp_multiline_bracket_style = cramped
---*)
type RequestParser<'ctx, 'a> = internal {
  consumedFields: Set<ConsumedFieldName>
  parse: 'ctx -> Request ->  Async<Result<'a, Error list>>
  prohibited: ProhibitedRequestGetter list
}
