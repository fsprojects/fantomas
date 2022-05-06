module Fantomas.Core.Tests.InterpolatedStringTests

open FSharp.Compiler.Text
open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``basic string interpolation`` () =
    formatSourceString
        false
        """
let text = "foo"
let s = $"%s{text} bar"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let text = "foo"
let s = $"%s{text} bar"
"""

[<Test>]
let ``modifiers before interpolation`` () =
    formatSourceString
        false
        """
let x = 1
let pi = 3.1414
let text = "cats"

let s = $"I say {x} is one and %0.2f{pi} is pi and %10s{text} are dogs"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = 1
let pi = 3.1414
let text = "cats"

let s = $"I say {x} is one and %0.2f{pi} is pi and %10s{text} are dogs"
"""

[<Test>]
let ``triple quote string interpolation`` () =
    formatSourceString
        false
        "
let text = \"foo\"
let s = $\"\"\"%s{text} bar\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let text = \"foo\"
let s = $\"\"\"%s{text} bar\"\"\"
"

[<Test>]
let ``interpolation in strict mode`` () =
    formatSourceString
        false
        """
let text = "foo"
let s = $"%s{text} bar"
"""
        { config with StrictMode = true }
    |> prepend newline
    |> should
        equal
        """
let text = "foo"
let s = $"%s{text} bar"
"""

[<Test>]
let ``multiline expression in multiline string`` () =
    formatSourceString
        false
        "
let str =
    $\"\"\"
    {
        let square x = x  * x
        let isOdd x = x % 2 <> 0
        let oddSquares =
            List.filter isOdd >> List.map square
        oddSquares [  1 .. 0 ]
    }\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let str =
    $\"\"\"
    {let square x = x * x
     let isOdd x = x % 2 <> 0
     let oddSquares = List.filter isOdd >> List.map square
     oddSquares [ 1..0 ]}\"\"\"
"

[<Test>]
let ``keep indentation in interpolation`` () =
    formatSourceString
        false
        """
    $"abc {let x = 3
           x + x} def {let x = 4
                       x + x} xyz"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"abc {let x = 3
       x + x} def {let x = 4
                   x + x} xyz"
"""

[<Test>]
let ``backslash in interpolation, issue 1344`` () =
    formatSourceString
        false
        """
$"\"{bar}\" {1} {2}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"\"{bar}\" {1} {2}"
"""

// TODO: consider cheating an getting all string from ISourceText

[<Test>]
let ``meh`` () =
    let source =
        SourceText.ofString
            """
$"\"{bar}\" {1} {2}"
"""

    let getContentAt (range: range) : string =
        let startLine = range.StartLine - 1
        let line = source.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring(range.StartColumn, length)
        else
            // TODO: not sure if this is a safe assumption
            "\n"

    let r = Range.mkRange "meh.fs" (Position.mkPos 2 0) (Position.mkPos 2 5)
    let content = getContentAt r
    ()

[<Test>]
let ``multiline string literal, issue 1451`` () =
    formatSourceString
        false
        "
$\"\"\"one: {1}<
>two: {2}\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
$\"\"\"one: {1}<
>two: {2}\"\"\"
"

[<Test>]
let ``prefix application, 1414`` () =
    formatSourceString
        false
        """
!- $".{s}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
!- $".{s}"
"""

[<Test>]
let ``format in FillExpr, 1549`` () =
    formatSourceString
        false
        """
let percent =0.1548486

Console.WriteLine($"Formatted: {percent:p2}")
"""
        config
    |> prepend newline
    |> should
        equal
        """
let percent = 0.1548486

Console.WriteLine($"Formatted: {percent:p2}")
"""

[<Test>]
let ``extra newlines in interpolated string, 1613`` () =
    formatSourceString
        false
        "
$\"\"\"
{1}
{2}
\"\"\"

$\"\"\"
- {1}
- 2

- {4}
- 5

\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
$\"\"\"
{1}
{2}
\"\"\"

$\"\"\"
- {1}
- 2

- {4}
- 5

\"\"\"
"

[<Test>]
let ``verbatim interpolated strings, 1645 `` () =
    formatSourceString
        false
        "
let main _ =
    let oldId = 1
    let newId = 2
    printfn $@\"Migrate notes of file \"\"{oldId}\"\" to new file \"\"{newId}\"\".\"
    0
"
        config
    |> prepend newline
    |> should
        equal
        "
let main _ =
    let oldId = 1
    let newId = 2
    printfn $@\"Migrate notes of file \"\"{oldId}\"\" to new file \"\"{newId}\"\".\"
    0
"

[<Test>]
let ``multiline expression should not receive any extra indentation, 1511`` () =
    formatSourceString
        false
        """
let storageConnection = $"DefaultEndpointsProtocol=https;AccountName=%s{storageAccount.Name};AccountKey=%s{storageAccountKey.Value}"

let serviceStorageConnection = $"DefaultEndpointsProtocol=https;AccountName=%s{serviceStorageAccount.Name};AccountKey=%s{serviceStorageAccountKey.Value}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let storageConnection =
    $"DefaultEndpointsProtocol=https;AccountName=%s{storageAccount.Name};AccountKey=%s{storageAccountKey.Value}"

let serviceStorageConnection =
    $"DefaultEndpointsProtocol=https;AccountName=%s{serviceStorageAccount.Name};AccountKey=%s{serviceStorageAccountKey.Value}"
"""

[<Test>]
let ``indentation inside try with is correct`` () =
    formatSourceString
        false
        """
$"
                    {
                         try
                             let a = 0
                             let b = y
                             let c = 9
                             foo ()
                         with ex -> interpolationFailed ()
}
"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"
                    {try
                         let a = 0
                         let b = y
                         let c = 9
                         foo ()
                     with
                     | ex -> interpolationFailed ()}
"
"""

[<Test>]
let ``construct url with Fable`` () =
    formatSourceString
        false
        """
   let newUrl =
            $"{window.location.protocol}//{window.location.host}{ window.location.pathname}{newHash}?{``params``.ToString()}"
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let newUrl =
    $"{window.location.protocol}//{window.location.host}{window.location.pathname}{newHash}?{``params``.ToString()}"
"""

[<Test>]
let ``multiline function application inside interpolated expression is printed as multiline`` () =
    formatSourceString
        false
        """
let foo = $"
longLeadingStringPaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaart{bar.ToString(window.location.protocol,window.location.host,window.location.pathname,newHash,``params``)}
"
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let foo =
    $"
longLeadingStringPaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaart{bar.ToString(
                                                                                                                                    window.location.protocol,
                                                                                                                                    window.location.host,
                                                                                                                                    window.location.pathname,
                                                                                                                                    newHash,
                                                                                                                                    ``params``
                                                                                                                                )}
"
"""

[<Test>]
let ``very long triple-quoted strings do not cause the interpolated string active pattern to stack overflow, 1837`` () =
    let loremIpsum =
        String.replicate
            1000
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n\n"

    formatSourceString false $"let value = \"\"\"{loremIpsum}\"\"\"" config
    |> should
        equal
        $"let value =
    \"\"\"{loremIpsum}\"\"\"
"
