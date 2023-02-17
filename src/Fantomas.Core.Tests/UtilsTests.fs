module Fantomas.Core.Tests.UtilsTests

open System
open NUnit.Framework
open Fantomas.Core
open Fantomas.Core.Tests.TestHelper
open FsCheck

let private mergeAndCompare a b expected = failwith "TODO"
// let result =
//     let getFragments code =
//         String.splitInFragments config.EndOfLine.NewLineString [ code ]
//         |> List.head
//         |> snd
//
//     String.merge (getFragments a) (getFragments b)
//     |> String.concat Environment.NewLine
//     |> String.normalizeNewLine
//
// let normalizedExpected = String.normalizeNewLine expected
// normalizedExpected == result

[<Test>]
let ``merging of source code that starts with a hash`` () =
    let a =
        """#if NOT_DEFINED
    printfn \"meh\"
#else

#endif
"""

    let b =
        """#if NOT_DEFINED

#else
    printfn \"foo\"
#endif
"""

    """#if NOT_DEFINED
    printfn \"meh\"
#else
    printfn \"foo\"
#endif
"""
    |> mergeAndCompare ([], a) ([ "NOT_DEFINED" ], b)

[<Test>]
let ``merging of defines content work when source code starts with a newline`` () =
    let a =
        """
[<Literal>]
let private assemblyConfig() =
    #if TRACE

    #else
    let x = "x"
    #endif
    x
"""

    let b =
        """
[<Literal>]
let private assemblyConfig() =
    #if TRACE
    let x = ""
    #else

    #endif
    x
"""

    """
[<Literal>]
let private assemblyConfig() =
#if TRACE
    let x = ""
#else
    let x = "x"
#endif
    x
"""
    |> mergeAndCompare ([], a) ([ "TRACE" ], b)

[<Test>]
let ``only split on control structure keyword`` () =
    let a =
        """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""

    let b =
        """
#if INTERACTIVE
#else



#endif
    """

    """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""
    |> mergeAndCompare ([], a) ([ "INTERACTIVE" ], b)

[<Test>]
let ``when input is empty`` () =
    let property (p: bool) : bool =
        let before, after = List.partitionWhile (fun _ _ -> p) []
        List.isEmpty before && List.isEmpty after

    Check.QuickThrowOnFailure property

[<Test>]
let ``when predicate always returns false`` () =
    let property (xs: int list) : bool =
        let before, after = List.partitionWhile (fun _ _ -> false) xs

        List.isEmpty before && after = xs

    Check.QuickThrowOnFailure property

[<Test>]
let ``when predicate always returns true`` () =
    let property (xs: int list) : bool =
        let before, after = List.partitionWhile (fun _ _ -> true) xs

        before = xs && List.isEmpty after

    Check.QuickThrowOnFailure property

[<Test>]
let ``when predicate returns true until certain index`` () =
    let property (xs: int list, i: int) : bool =
        let before, after = List.partitionWhile (fun index _ -> i <> index) xs

        let beforeLength = List.length before
        let afterLength = List.length after
        beforeLength = i && afterLength = List.length xs - i && before @ after = xs

    let gen =
        gen {
            let! xs = Arb.generate<int> |> Gen.nonEmptyListOf
            let len = List.length xs
            let! n = Gen.choose (0, len - 1)

            return (xs, n)
        }

    property |> Prop.forAll (Arb.fromGen gen) |> Check.QuickThrowOnFailure
