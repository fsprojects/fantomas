type FieldNotFoundException<'T> (obj: 'T, field: string, specLink: string) =
    inherit
        SwaggerSchemaParseException (
            sprintf
                "Object MUST contain field `%s` (See %s for more details).\nObject:%A"
                field
                specLink
                obj
        )
