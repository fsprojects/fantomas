module Fantomas.Analyzers.Tests.PrintfAnalyzerTests

open NUnit.Framework
open Fantomas.Analyzers.Tests.TestHelpers
open Fantomas.Analyzers.PrintfAnalyzer

let private toolFile: string = "/repository/src/Fantomas/Report.fs"

let private analyzeToolSource (source: string) : FSharp.Analyzers.SDK.Message list =
    analyzeSourceAt cliAnalyzer toolFile source

[<Test>]
let ``sprintf is reported`` () =
    let source: string =
        """module M

let f (x: int) : string = sprintf "%d items" x"""

    analyzeToolSource source |> assertLines [ 3 ]

[<Test>]
let ``failwithf is reported`` () =
    let source: string =
        """module M

let f (x: string) : int = failwithf "no such setting: %s" x"""

    analyzeToolSource source |> assertLines [ 3 ]

[<Test>]
let ``an interpolated string with a precision is reported`` () =
    let source: string =
        """module M

let f (x: float) : string = $"%.0f{x}ms"
"""

    analyzeToolSource source |> assertLines [ 3 ]

[<Test>]
let ``an interpolated string with a structured hole is reported`` () =
    let source: string =
        """module M

let f (x: int list) : string = $"got %A{x}"
"""

    analyzeToolSource source |> assertLines [ 3 ]

// The compiler lowers these to `String.Concat`, so nothing goes through printf.
[<Test>]
let ``an interpolated string with bare and typed holes is not reported`` () =
    let source: string =
        """module M

let f (name: string) (count: int) : string = $"%s{name} has %d{count} files, {count} in total"
"""

    analyzeToolSource source |> assertLines []

[<Test>]
let ``a hole with a width is reported`` () =
    let source: string =
        """module M

let f (x: int) : string = $"[%5d{x}]"
"""

    analyzeToolSource source |> assertLines [ 3 ]

[<Test>]
let ``a hole with a lowercase boolean specifier is reported`` () =
    let source: string =
        """module M

let f (x: bool) : string = $"flag: %b{x}"
"""

    analyzeToolSource source |> assertLines [ 3 ]

// `%%` is an escaped percent sign, so what follows is text and the hole after it is bare.
[<Test>]
let ``an escaped percent sign before a bare hole is not reported`` () =
    let source: string =
        """module M

let f (x: int) : string = $"100%%d{x}"
"""

    analyzeToolSource source |> assertLines []

[<Test>]
let ``a hole with a dotnet format is not reported`` () =
    let source: string =
        """module M

let f (x: float) : string = $"{x:N2} and {x,8}"
"""

    analyzeToolSource source |> assertLines []

[<Test>]
let ``kprintf is reported`` () =
    let source: string =
        """module M

let f (format: Printf.StringFormat<'T, string>) : 'T = Printf.kprintf id format"""

    analyzeToolSource source |> assertLines [ 3 ]

[<Test>]
let ``sprintf passed as a value is reported`` () =
    let source: string =
        """module M

let f (xs: int list) : string list = List.map (sprintf "%d") xs"""

    analyzeToolSource source |> assertLines [ 3 ]

[<Test>]
let ``printf inside a lambda is reported`` () =
    let source: string =
        """module M

let f (xs: int list) : string list =
    xs |> List.map (fun x -> sprintf "%i" x)"""

    analyzeToolSource source |> assertLines [ 4 ]

[<Test>]
let ``printf in Fantomas.Core is reported`` () =
    let source: string =
        """module M

let f (x: int) : string = sprintf "%d" x"""

    analyzeSourceAt cliAnalyzer "/repository/src/Fantomas.Core/Utils.fs" source
    |> assertLines [ 3 ]

// The tests and Fantomas.Client only run on the JIT, where printf is fine.
[<TestCase("/repository/src/Fantomas.Tests/ReportTests.fs")>]
[<TestCase("/repository/src/Fantomas.Client/LSPFantomasService.fs")>]
[<TestCase("/repository/src/Fantomas.Core.Tests/CodePrinterTests.fs")>]
let ``printf outside the code the tool runs is not reported`` (fileName: string) =
    let source: string =
        """module M

let f (x: int) : string = sprintf "%d" x"""

    analyzeSourceAt cliAnalyzer fileName source |> assertLines []
