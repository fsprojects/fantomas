(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_alternative_long_member_definitions = true
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

