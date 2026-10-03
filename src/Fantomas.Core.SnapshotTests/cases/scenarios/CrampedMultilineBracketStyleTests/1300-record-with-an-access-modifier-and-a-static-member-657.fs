(*---
fsharp_multiline_bracket_style = cramped
---*)
type RequestParser<'ctx, 'a> =
    internal
        { consumedFields: Set<ConsumedFieldName>
          parse: 'ctx -> Request -> Async<Result<'a, Error list>>
          prohibited: ProhibitedRequestGetter list }

        static member internal Create
            (
                consumedFields, parse: 'ctx -> Request -> Async<Result<'a, Error list>>
            ) : RequestParser<'ctx, 'a> =
            { consumedFields = consumedFields
              parse = parse
              prohibited = [] }

