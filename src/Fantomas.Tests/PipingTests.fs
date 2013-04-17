module Fantomas.Tests.PipingTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

// the current behavior results in a compile error since the |> is merged to the last line 
[<Test>]
let ``should keep the pipe after infix operator``() =
    formatSourceString false """
let f x =
    someveryveryveryverylongexpression
    <|> if someveryveryveryverylongexpression then someveryveryveryverylongexpression else someveryveryveryverylongexpression
    <|> if someveryveryveryverylongexpression then someveryveryveryverylongexpression else someveryveryveryverylongexpression
    |> f

    """ config
    |> should equal """let f x = 
    someveryveryveryverylongexpression 
    <|> if someveryveryveryverylongexpression
        then someveryveryveryverylongexpression
        else someveryveryveryverylongexpression 
    <|> if someveryveryveryverylongexpression
        then someveryveryveryverylongexpression
        else someveryveryveryverylongexpression
    |> f
"""

// the current behavior results in a compile error since the |> is merged to the last line 
[<Test>]
let ``should keep the pipe after pattern matching``() =
    formatSourceString false """let m = 
    match x with
    | y -> ErrorMessage msg
    | _ -> LogMessage(msg, true) 
    |> console.Write

    """ config
    |> should equal """let m = 
    match x with
    | y -> ErrorMessage msg
    | _ -> LogMessage(msg, true)
    |> console.Write
"""