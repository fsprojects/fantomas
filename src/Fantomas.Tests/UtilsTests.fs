module Fantomas.Tests.UtilsTests

open System
open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open FsCheck

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

[<Test>]
let ``when input is empty`` () =
    let property (p: bool) : bool =
        let before, after = List.partitionWhile (fun _ _ -> p) []
        before = [] && after = []

    Check.QuickThrowOnFailure(property true)
    Check.QuickThrowOnFailure(property false)

[<Test>]
let ``when predicate always returns false`` () =
    let property (xs: int list) : bool =
        let before, after =
            List.partitionWhile (fun _ _ -> false) xs

        before = [] && after = xs

    Check.QuickThrowOnFailure property

[<Test>]
let ``when predicate always returns true`` () =
    let property (xs: int list) : bool =
        let before, after =
            List.partitionWhile (fun _ _ -> true) xs

        before = xs && after = []

    Check.QuickThrowOnFailure property

[<Test>]
let ``when predicate returns true until certain index`` () =
    let property (xs: int list, i: int) : bool =
        let before, after =
            List.partitionWhile (fun index _ -> i <> index) xs

        let beforeLength = List.length before
        let afterLength = List.length after

        beforeLength = i
        && afterLength = List.length xs - i
        && before @ after = xs

    let gen =
        gen {
            let! xs =
                Arb.generate<int>
                |> Gen.listOf
                |> Gen.filter (fun l -> l.Length > 0)

            let len = List.length xs

            let! n =
                Arb.generate<int>
                |> Gen.filter (fun n -> n >= 0 && n < len)

            return (xs, n)
        }

    property
    |> Prop.forAll (Arb.fromGen gen)
    |> Check.QuickThrowOnFailure
