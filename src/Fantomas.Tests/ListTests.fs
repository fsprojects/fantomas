module Fantomas.Tests.ListTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should keep -> notation``() =
    formatSourceString false """let environVars target =
    [for e in Environment.GetEnvironmentVariables target ->
        let e1 = e :?> Collections.DictionaryEntry
        e1.Key, e1.Value]
    """ config
    |> prepend newline
    |> should equal """
let environVars target = 
    [for e in Environment.GetEnvironmentVariables target -> 
         let e1 = e :?> Collections.DictionaryEntry
         e1.Key, e1.Value]
"""