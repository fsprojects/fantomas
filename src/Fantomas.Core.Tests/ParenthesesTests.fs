module Fantomas.Core.Tests.ParenthesesTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``trivia after opening parenthesis, 2847`` () =
    formatSourceString
        """
let canConvertMemorised =
    Memoized.memoize
        (fun objectType ->
            (   // Include F# discriminated unions
                FSharpType.IsUnion objectType
                // and exclude the standard FSharp lists (which are implemented as discriminated unions)
                && not (objectType.GetTypeInfo().IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<_ list>)
            )
            // include tuples
            || tupleAsHeterogeneousArray && FSharpType.IsTuple objectType
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let canConvertMemorised =
    Memoized.memoize (fun objectType ->
        ( // Include F# discriminated unions
        FSharpType.IsUnion objectType
        // and exclude the standard FSharp lists (which are implemented as discriminated unions)
        && not (
            objectType.GetTypeInfo().IsGenericType
            && objectType.GetGenericTypeDefinition() = typedefof<_ list>
        ))
        // include tuples
        || tupleAsHeterogeneousArray && FSharpType.IsTuple objectType)
"""
