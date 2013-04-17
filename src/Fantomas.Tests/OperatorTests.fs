module Fantomas.TestsOperatorTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``should keep triple ~~~ operator``() =
    formatSourceString false """x ~~~FileAttributes.ReadOnly
    """ config
    |> should equal """x ~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``should keep single triple ~~~ operator``() =
    formatSourceString false """~~~FileAttributes.ReadOnly
    """ config
    |> should equal """~~~FileAttributes.ReadOnly
"""