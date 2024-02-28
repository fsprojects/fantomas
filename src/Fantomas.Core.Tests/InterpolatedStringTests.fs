module Fantomas.Core.Tests.InterpolatedStringTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``basic string interpolation`` () =
    formatSourceString
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
    formatAST
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
let ``interpolation from AST with multiple fillExprs`` () =
    formatAST
        false
        """
$"%s{text} %i{bar} %f{meh}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"%s{text} %i{bar} %f{meh}"
"""

[<Test>]
let ``multiline expression in multiline string`` () =
    formatSourceString
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

[<Test>]
let ``multiline string literal, issue 1451`` () =
    formatSourceString
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
                     with ex ->
                         interpolationFailed ()}
"
"""

[<Test>]
let ``construct url with Fable`` () =
    formatSourceString
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

    formatSourceString $"let value = \"\"\"%s{loremIpsum}\"\"\"" config
    |> should
        equal
        $"let value =
    \"\"\"%s{loremIpsum}\"\"\"
"

[<Test>]
let ``don't eat braces, 3012`` () =
    formatSourceString
        "
$$$\"\"\"{{{5}}}\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
$$$\"\"\"{{{5}}}\"\"\"
"

[<Test>]
let ``extended interpolated string with several fill expressions`` () =
    formatSourceString
        "
let x = $$$\"\"\"one {{{1}}} two {{{2}}} three {{{3}}}\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let x = $$$\"\"\"one {{{1}}} two {{{2}}} three {{{3}}}\"\"\"
"
