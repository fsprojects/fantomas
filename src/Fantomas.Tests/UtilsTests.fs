module Fantomas.Tests.UtilsTests

open System
open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open FsCheck

module String =
    let private mergeAndCompare a b expected =
        let result =
            String.merge Environment.NewLine a b
            |> String.normalizeNewLine

        let normalizedExpected = String.normalizeNewLine expected
        normalizedExpected == result

    [<Test>]
    let ``Merging of source code that starts with a hash`` () =
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
        |> mergeAndCompare a b

    [<Test>]
    let ``Merging of defines content work when source code starts with a newline`` () =
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
        |> mergeAndCompare a b

    [<Test>]
    let ``Only split on control structure keyword`` () =
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
        |> mergeAndCompare a b

module List =
    [<Test>]
    let ``When n greater than or equal to list length`` () =
        let property (xs: int list, n: int) : bool =
            let actual = List.splitAround n xs

            match actual with
            | None -> n >= List.length xs
            | Some (before, None) -> before = xs
            | _ -> false

        let gen =
            gen {
                let! xs = Arb.generate<int> |> Gen.listOf
                let len = List.length xs

                let! n =
                    Arb.generate<int>
                    |> Gen.filter (fun n -> n >= len)

                return (xs, n)
            }

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure

    [<Test>]
    let ``When n less than list length`` () =
        let property (xs: int list, n: int) : bool =
            let actual = List.splitAround n xs

            match actual with
            | None -> n < 0
            | Some (_, None) -> List.isEmpty xs
            | Some (before, Some (at, after)) ->
                let xsLength = List.length xs
                let beforeLength = List.length before
                let afterLength = List.length after

                beforeLength = max 0 n
                && afterLength = xsLength - beforeLength - 1
                && before @ (at :: after) = xs

        let gen =
            gen {
                let! xs = Arb.generate<int> |> Gen.listOf
                let len = List.length xs
                let! n = Arb.generate<int> |> Gen.filter (fun n -> n < len)
                return (xs, n)
            }

        property
        |> Prop.forAll (Arb.fromGen gen)
        |> Check.QuickThrowOnFailure


    [<Test>]
    let ``When n equals list length`` () =
        let property (xs: int list) : bool =
            let actual = List.splitAround (List.length xs) xs

            match actual with
            | None -> false
            | Some (before, None) -> before = xs
            | Some (_, Some _) -> false

        Check.QuickThrowOnFailure property
