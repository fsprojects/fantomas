module Fantomas.Tests.TestIgnoreFile

open System.Collections.Concurrent
open NUnit.Framework
open FsUnitTyped
open Fantomas.Extras
open System.IO.Abstractions.TestingHelpers

[<Test>]
let ``findIgnoreFile does not crash at the root`` () =
    let fs = MockFileSystem ()
    let root = fs.Directory.GetDirectoryRoot "."

    let readFile (path : string) = path
    
    let dict = ConcurrentDictionary ()
    IgnoreFile.findIgnoreFile fs readFile dict root
    |> shouldEqual None
    
    dict
    |> shouldBeEmpty
