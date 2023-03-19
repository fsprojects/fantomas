module Fantomas.Core.Tests.UtilsTests

open NUnit.Framework
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
