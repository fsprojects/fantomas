(*---
max_line_length = 90
fsharp_space_before_class_constructor = true
---*)
type FieldNotFoundException<'T>(obj:'T, field:string, specLink:string) =
    inherit SwaggerSchemaParseException(
        sprintf "Object MUST contain field `%s` (See %s for more details).\nObject:%A"
            field specLink obj)
