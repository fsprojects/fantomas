module Fantomas.Core.Tests.BaseConstructorTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``multiple base constructors in record, 2111`` () =
    formatSourceString
        """
type UnhandledWebException =
    inherit Exception

    new(status: WebExceptionStatus, innerException: Exception) =
        { inherit Exception(SPrintF1
                                "Backend not prepared for this WebException with Status[%i]"
                                (int status),
                            innerException) }

    new(info: SerializationInfo, context: StreamingContext) =
        { inherit Exception(info, context) }
"""
        { config with MaxLineLength = 100 }
    |> prepend newline
    |> should
        equal
        """
type UnhandledWebException =
    inherit Exception

    new(status: WebExceptionStatus, innerException: Exception) =
        { inherit
            Exception(
                SPrintF1 "Backend not prepared for this WebException with Status[%i]" (int status),
                innerException
            ) }

    new(info: SerializationInfo, context: StreamingContext) = { inherit Exception(info, context) }
"""

[<Test>]
let ``single multiline base constructor, 2335`` () =
    formatSourceString
        """
type FieldNotFoundException<'T>(obj:'T, field:string, specLink:string) =
    inherit SwaggerSchemaParseException(
        sprintf "Object MUST contain field `%s` (See %s for more details).\nObject:%A"
            field specLink obj)
"""
        { config with
            SpaceBeforeClassConstructor = true
            MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
type FieldNotFoundException<'T> (obj: 'T, field: string, specLink: string) =
    inherit
        SwaggerSchemaParseException (
            sprintf
                "Object MUST contain field `%s` (See %s for more details).\nObject:%A"
                field
                specLink
                obj
        )
"""
