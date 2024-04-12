module Fantomas.Core.Tests.ConstructorTests

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

[<Test>]
let ``multiline secondary constructor, 3037`` () =
    formatSourceString
        """
type IntersectionOptions
    private
    (
        primary: bool,
        ?root: Element,
        ?rootMargin: string,
        ?threshold: ResizeArray<float>,
        ?triggerOnce: bool
    )
    =

    new(?root: Element,
        ?rootMargin: string,
        ?threshold: ResizeArray<float>,
        ?triggerOnce: bool) =

        IntersectionOptions(true)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type IntersectionOptions
    private
    (
        primary: bool,
        ?root: Element,
        ?rootMargin: string,
        ?threshold: ResizeArray<float>,
        ?triggerOnce: bool
    ) =

    new
        (
            ?root: Element,
            ?rootMargin: string,
            ?threshold: ResizeArray<float>,
            ?triggerOnce: bool
        ) =

        IntersectionOptions(true)
"""

[<Test>]
let ``secondary constructor with xml doc`` () =
    formatSourceString
        """
type IntersectionOptions
    (
        primary: bool
    ) =

    /// Good stuff
    new (secondary: int) = IntersectionOptions(secondary = 0)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type IntersectionOptions(primary: bool) =

    /// Good stuff
    new(secondary: int) = IntersectionOptions(secondary = 0)
"""

[<Test>]
let ``setting AlternativeLongMemberDefinitions should be respected in long secondary constructor`` () =
    formatSourceString
        """
type StateMachine(
    // meh
) =
    new(
        // also meh but with an int
        x:int) as secondCtor = StateMachine()
"""
        { config with
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
type StateMachine
    (
    // meh
    )
    =
    new
        (
        // also meh but with an int
        x: int) as secondCtor
        =
        StateMachine()
"""

[<Test>]
let ``explicit constructor with then keyword, 3074`` () =
    formatSourceString
        """
type CreateBuildingViewModel =
    new (items) as vm
        =
        let p = ""
        {
            inherit ResizeArray(seq {
                yield p
                yield! items
            })
        }
        then
            vm.program <- p
"""
        config
    |> prepend newline
    |> should
        equal
        """
type CreateBuildingViewModel =
    new(items) as vm =
        let p = ""

        { inherit
            ResizeArray(
                seq {
                    yield p
                    yield! items
                }
            ) }

        then vm.program <- p
"""
