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

// List.moreThanOne

[<Test>]
let ``List.moreThanOne returns false for empty list`` () =
    let result = List.moreThanOne ([] : int list)
    Assert.That(result, Is.False)

[<Test>]
let ``List.moreThanOne returns false for single-element list`` () =
    let property (x: int) : bool = not (List.moreThanOne [ x ])
    Check.QuickThrowOnFailure property

[<Test>]
let ``List.moreThanOne returns true for two or more elements`` () =
    let property (x: int, y: int, rest: int list) : bool = List.moreThanOne (x :: y :: rest)
    Check.QuickThrowOnFailure property

// List.isNotEmpty

[<Test>]
let ``List.isNotEmpty returns false for empty list`` () =
    let result = List.isNotEmpty ([] : int list)
    Assert.That(result, Is.False)

[<Test>]
let ``List.isNotEmpty returns true for non-empty list`` () =
    let property (x: int, rest: int list) : bool = List.isNotEmpty (x :: rest)
    Check.QuickThrowOnFailure property

// List.mapWithLast

[<Test>]
let ``List.mapWithLast empty list returns empty`` () =
    let result = List.mapWithLast id id ([] : int list)
    Assert.That(result, Is.Empty)

[<Test>]
let ``List.mapWithLast single element uses g`` () =
    let property (x: int) : bool =
        List.mapWithLast (fun _ -> 0) (fun n -> n * 2) [ x ] = [ x * 2 ]

    Check.QuickThrowOnFailure property

[<Test>]
let ``List.mapWithLast applies f to init elements and g to last`` () =
    let gen =
        gen {
            let! head = Arb.generate<int>
            let! tail = Gen.listOf Arb.generate<int>
            return head :: tail
        }

    let property (xs: int list) : bool =
        let result = List.mapWithLast (fun n -> n + 1) (fun n -> n * 2) xs
        let n = List.length xs
        let expectedInit = List.map (fun n -> n + 1) (List.take (n - 1) xs)
        let expectedLast = List.last xs * 2
        result = expectedInit @ [ expectedLast ]

    property |> Prop.forAll (Arb.fromGen gen) |> Check.QuickThrowOnFailure

// List.cutOffLast

[<Test>]
let ``List.cutOffLast empty list returns empty`` () =
    let result = List.cutOffLast ([] : int list)
    Assert.That(result, Is.Empty)

[<Test>]
let ``List.cutOffLast single element returns empty`` () =
    let property (x: int) : bool = List.cutOffLast [ x ] = []
    Check.QuickThrowOnFailure property

[<Test>]
let ``List.cutOffLast removes last element`` () =
    let gen =
        gen {
            let! head = Arb.generate<int>
            let! tail = Gen.listOf Arb.generate<int>
            return head :: tail
        }

    let property (xs: int list) : bool =
        let result = List.cutOffLast xs
        result = List.take (List.length xs - 1) xs

    property |> Prop.forAll (Arb.fromGen gen) |> Check.QuickThrowOnFailure

// List.foldWithLast

[<Test>]
let ``List.foldWithLast empty list returns initial state`` () =
    let property (init: int) : bool = List.foldWithLast (+) ( * ) init [] = init
    Check.QuickThrowOnFailure property

[<Test>]
let ``List.foldWithLast single element applies g to initial state and element`` () =
    let property (x: int) : bool = List.foldWithLast (+) ( * ) 1 [ x ] = 1 * x
    Check.QuickThrowOnFailure property

[<Test>]
let ``List.foldWithLast folds with f on all but last, g on last`` () =
    let gen =
        gen {
            let! head = Arb.generate<int>
            let! tail = Gen.listOf Arb.generate<int>
            return head :: tail
        }

    let property (xs: int list) : bool =
        let result = List.foldWithLast (fun acc x -> acc + x) (fun acc x -> acc - x) 0 xs
        let init = List.take (List.length xs - 1) xs
        let last = List.last xs
        let expected = List.fold (fun acc x -> acc + x) 0 init - last
        result = expected

    property |> Prop.forAll (Arb.fromGen gen) |> Check.QuickThrowOnFailure

// List.chooseState

[<Test>]
let ``List.chooseState with always None returns empty`` () =
    let property (xs: int list) : bool =
        List.chooseState (fun s x -> (s + x, None)) 0 xs = []

    Check.QuickThrowOnFailure property

[<Test>]
let ``List.chooseState with always Some preserves all elements`` () =
    let property (xs: int list) : bool =
        List.chooseState (fun () x -> ((), Some x)) () xs = xs

    Check.QuickThrowOnFailure property

[<Test>]
let ``List.chooseState threads state correctly`` () =
    // Running sum: choose element only when running sum is even
    let result =
        List.chooseState
            (fun sum x ->
                let newSum = sum + x
                (newSum, if newSum % 2 = 0 then Some x else None))
            0
            [ 1; 3; 2; 4; 1 ]

    // sum after 1=1 (odd, None), 1+3=4 (even, Some 3), 4+2=6 (even, Some 2), 6+4=10 (even, Some 4), 10+1=11 (odd, None)
    Assert.That(result, Is.EqualTo([ 3; 2; 4 ]))
