module Fantomas.Tests.TokenParserTests


open NUnit.Framework
open FsUnit
open Fantomas
open Fantomas.SourceParser
open Fantomas.TokenParser
open Fantomas.Tests.TestHelper

let private getDefines (v: string) =
    let sourceText = CodeFormatterImpl.getSourceText v
    let baseUntypedTree, _diagnostics = Fantomas.FCS.Parse.parseFile false sourceText []

    let hashDirectives =
        match baseUntypedTree with
        | ImplFile (ParsedImplFileInput (_, _, directives, _))
        | SigFile (ParsedSigFileInput (_, _, directives, _)) -> directives

    getDefineCombination hashDirectives
    |> List.collect id
    |> List.distinct
    |> List.sort

[<Test>]
let ``simple compiler directive should be found`` () =
    let source =
        """
#if DEBUG
setupServer false
#else
setupServer true
#endif
"""

    getDefines source |> List.length |> should equal 1

[<Test>]
let ``simple compiler directive should be DEBUG`` () =
    let source =
        """
#if DEBUG
setupServer false
#else
setupServer true
#endif
"""

    getDefines source
    |> List.head
    |> should equal "DEBUG"

[<Test>]
let ``get defines from complex statements`` () =
    let source =
        """
#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""

    getDefines source
    == [ "BAR"; "BUZZ"; "FOO"; "INTERACTIVE" ]

[<Test>]
let ``tokens from directive inside a directive are being added`` () =
    let source =
        """#if FOO
  #if BAR
  #else
  #endif
#endif
"""

    let defines = getDefines source
    defines == [ "BAR"; "FOO" ]

[<Test>]
let ``define with underscore`` () =
    let source =
        """#if INVARIANT_CULTURE_STRING_COMPARISON

#else

#endif
"""

    getDefines source
    == [ "INVARIANT_CULTURE_STRING_COMPARISON" ]

[<Test>]
let ``open close of string on same line`` () =
    let source =
        "
let a = \"\"
#if FOO
#if BAR
#endif
#endif
"

    getDefines source == [ "BAR"; "FOO" ]

[<Test>]
let ``open close of triple quote string on same line`` () =
    let source =
        "
let a = \"\"\"foo\"\"\"
#if FOO
#endif
"

    getDefines source == [ "FOO" ]

[<Test>]
let ``open, quote, close of triple quote string on same line`` () =
    let source =
        "
let a = \"\"\"fo\"o\"\"\"
#if FOO
#endif
"

    getDefines source == [ "FOO" ]

[<Test>]
let ``defines inside string`` () =
    let source =
        "
let a = \"
#if FOO
#if BAR
#endif
#endif
\"
"

    getDefines source == List<string>.Empty

[<Test>]
let ``defines inside string, escaped quote`` () =
    let source =
        "
let a = \"\\\"
#if FOO
#if BAR
#endif
#endif
\"
"

    getDefines source == List<string>.Empty

[<Test>]
let ``defines inside triple quote string`` () =
    let source =
        "
let a = \"\"\"
#if FOO
#if BAR
#endif
#endif
\"\"\"
"

    getDefines source == List<string>.Empty

[<Test>]
let ``defines inside triple quote string, escaped quote`` () =
    let source =
        "
let a = \"\"\"\\\"
#if FOO
#if BAR
#endif
#endif
\"\"\"
"

    getDefines source == List<string>.Empty

[<Test>]
[<Ignore "This is an acceptable limitation for now. In the future we could check if the directive is part of a string range or comment block">]
let ``defines inside triple quote string, escaped triple quote`` () =
    let source =
        "
let a = \"\"\"\\\"\"\"
#if FOO
#if BAR
#endif
#endif
\"\"\"
"

    getDefines source == List<string>.Empty

[<Test>]
let ``backslashes in strings prior to hash directives should not affect token parsing`` () =
    let source =
        "
let file =
    System.IO.Path.Combine(contentDir,
                           (n |> System.IO.Path.GetFileNameWithoutExtension)
                           + \".md\").Replace(\"\\\\\", \"//\")

#if WATCH
#endif
"

    getDefines source == [ "WATCH" ]

[<Test>]
let ``escaped backslash inside escaped string, 1290`` () =
    let source =
        "
[<Test>]
let ``defines inside string, escaped quote`` () =
    let source = \"
let a = \\\"\\\\\\\"
#if FOO
  #if BAR
  #endif
#endif
\\\"
\"

    getDefines source == []
"

    getDefines source == List<string>.Empty

[<Test>]
let ``opening quote in line comment, 1504`` () =
    let source =
        "
// \"
type A () =

#if DEBUG
  let a() = ()
#endif

  let f (x: int) =
    match x with
    | _ ->
#if DEBUG
      ()
#else
      ()
#endif
    "

    getDefines source == [ "DEBUG" ]

[<Test>]
let ``opening quote in second line comment`` () =
    let source =
        "
// nothing special here
// \"
#if DEBUG
prinfn \"Debug shizzle\"
#endif
    "

    getDefines source == [ "DEBUG" ]

[<Test>]
let ``backslash in verbatim string`` () =
    let source =
        "
let ProgramFilesX86 =
    if detected = null then @\"C:\Program Files (x86)\\\" else detected

let isUnix =
#if NETSTANDARD1_6 || NETSTANDARD2_0
    meh
#else
    foo
#endif
        "

    getDefines source
    == [ "NETSTANDARD1_6"; "NETSTANDARD2_0" ]
