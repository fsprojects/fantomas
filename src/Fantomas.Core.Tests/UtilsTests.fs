module Fantomas.Core.Tests.UtilsTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open FsCheck

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

[<Test>]
let ``String.visualWidth counts grapheme clusters not UTF-16 code units, 2945`` () =
    // ASCII: visual width equals String.length
    String.visualWidth "hello" |> should equal 5
    // Combining characters attach to the preceding base character with no visual advance.
    // U+036E, U+0312, U+036B are all combining marks.
    // "l" + 3 combining marks = 1 grapheme cluster
    let combining = "l\u036e\u0312\u036b"
    String.visualWidth combining |> should equal 1
    // String.length counts UTF-16 code units, not grapheme clusters
    String.length combining |> should equal 4
    // Empty string
    String.visualWidth "" |> should equal 0
