module Fantomas.Tests.QueueTests

open Fantomas
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open FsCheck

let private verboseConf =
    { Config.Verbose with
          MaxTest = 500
          EndSize = 20
          Runner = NUnitRunner() }


[<Test>]
let ``Queue.append``() =
    Check.One
        (verboseConf, (
            fun xs ys ->
                let q = Queue.ofList xs
                let result = Queue.append q ys
                let expected = (q, ys) ||> List.fold (fun q y -> Queue.conj y q)
                Queue.toSeq result |> should equivalent (Queue.toSeq expected)    
                ))
