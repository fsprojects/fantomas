module Fantomas.Tests.QueueTests

open Fantomas
open NUnit.Framework
open FsUnit
open FsCheck

module Queue =
    let ofLists xss = (Queue.empty, xss) ||> List.fold Queue.append 

[<Test>]
let ``Queue.append``() =
    Check.One (Config.Default, fun xs ys ->
        let q = Queue.ofList xs
        let result q = Queue.append q ys |> Queue.toSeq |> Seq.toList
        let expected x = x @ ys
        result q |> should equivalent (expected xs)
        result (Queue.rev q |> Queue.ofSeq) |> should equivalent (expected (List.rev xs))
        )

[<Test>]
let ``Queue.tryHead``() =
    Check.One (Config.Default, fun xs ->
        let result = Queue.ofList xs |> Queue.tryHead
        let expected = xs |> List.tryHead
        result |> should equal expected
        )

[<Test>]
let ``Queue.length``() =
    Check.One (Config.Default, fun xs ->
        let result = Queue.ofList xs |> Queue.length
        let expected = xs |> List.length
        result |> should equal expected
        )

[<Test>]
let ``Queue.rev``() =
    Check.One (Config.Default, fun xs ->
        let result = Queue.ofList xs |> Queue.rev |> Seq.toList
        let expected = xs |> List.rev
        result |> should equivalent expected
        )

[<Test>]
let ``Queue.toSeq``() =
    Check.One (Config.Default, fun xs ->
        let result = Queue.ofList xs |> Queue.toSeq |> Seq.toList
        let expected = xs
        result |> should equivalent expected
        )

[<Test>]
let ``Queue.skipExists``() =
    Check.One (Config.Default, fun xss n ->
        let f = id
        n <= List.sumBy List.length xss ==>
            lazy
                (let result = Queue.ofLists xss |> Queue.skipExists n f
                 let expected = xss |> List.collect id |> Seq.skip n |> Seq.exists f
                 result |> should equal expected)
        )


[<Test>]
let ``Queue ref transp``() =
    Check.One (Config.Default, fun xs ys zs ->
        let result1 = Queue.ofList xs
        let result2 = Queue.append result1 ys 
        let result3 = Queue.append result1 zs 
        let expected1 = xs
        let expected2 = xs @ ys
        let expected3 = xs @ zs
        result1 |> should equivalent expected1
        result2 |> should equivalent expected2
        result3 |> should equivalent expected3
        )
