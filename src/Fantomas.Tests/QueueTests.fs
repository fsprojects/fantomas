module Fantomas.Tests.QueueTests

open Fantomas
open NUnit.Framework
open FsUnit
open FsCheck

[<Test>]
let ``Queue.append``() =
    Check.One (Config.Default, fun xs ys ->
        let q = Queue.ofList xs
        let result q = Queue.append q ys |> Queue.toSeq |> Seq.toList
        let expected q = (q, ys) ||> List.fold (fun q y -> Queue.conj y q) |> Queue.toSeq |> Seq.toList
        result q |> should equivalent (expected q)
        result (Queue.rev q) |> should equivalent (expected (Queue.rev q))
        )
