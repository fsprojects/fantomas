module Fantomas.Tests.ListTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

// the current behavior results in a compile error since the -> is replaced to do notation
// but this is not valid in this case
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