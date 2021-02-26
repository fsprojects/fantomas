module Fantomas.Tests.TokenParserTests

open Fantomas
open NUnit.Framework
open FsUnit
open Fantomas.TokenParser
open Fantomas.TriviaTypes
open Fantomas.Tests.TestHelper

let private isNewline item =
    match item with
    | Newline -> true
    | _ -> false

let private getDefines v =
    let normalizedString = String.normalizeNewLine v
    let _, hashTokens = getDefines normalizedString
    getDefinesWords hashTokens

let private tokenize v = tokenize [] [] v

let private mkRange : MkRange =
    fun (sl, sc) (el, ec) ->
        FSharp.Compiler.Text.Range.mkRange
            "TokenParserTests"
            (FSharp.Compiler.Text.Pos.mkPos sl sc)
            (FSharp.Compiler.Text.Pos.mkPos el ec)

let private getTriviaFromTokens = getTriviaFromTokens mkRange
let private getTriviaNodesFromTokens = getTriviaNodesFromTokens mkRange

[<Test>]
let ``Simple compiler directive should be found`` () =
    let source = """
#if DEBUG
setupServer false
#else
setupServer true
#endif
"""

    getDefines source |> List.length |> should equal 1

[<Test>]
let ``Simple compiler directive should be DEBUG`` () =
    let source = """
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
let ``Get defines from complex statements`` () =
    let source = """
#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""

    getDefines source
    == [ "INTERACTIVE"; "FOO"; "BAR"; "BUZZ" ]

[<Test>]
let ``Tokens from directive inside a directive are being added`` () =
    let source = """#if FOO
  #if BAR
  #else
  #endif
#endif
"""

    let _, hashTokens = TokenParser.getDefines source
    getDefinesWords hashTokens == [ "FOO"; "BAR" ]

    let tokens =
        TokenParser.tokenize [] hashTokens source

    let hashTokens =
        tokens
        |> List.filter (fun { TokenInfo = { TokenName = tn } } -> tn = "HASH_IF")

    let furtherInwards =
        hashTokens
        |> List.filter (fun { TokenInfo = { LeftColumn = lc } } -> lc = 2)

    List.length hashTokens == 5
    List.length furtherInwards == 3


[<Test>]
let ``define with underscore`` () =
    let source = """#if INVARIANT_CULTURE_STRING_COMPARISON

#else

#endif
"""

    getDefines source
    == [ "INVARIANT_CULTURE_STRING_COMPARISON" ]

[<Test>]
let ``tokenize should return correct amount`` () =
    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32

    tokenize source |> List.length |> should equal 7

[<Test>]
let ``tokenize should return correct sequence of tokens`` () =
    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32

    let tokens =
        tokenize source
        |> List.map (fun t -> t.TokenInfo.TokenName)

    tokens.[0] == "LET"
    tokens.[1] == "WHITESPACE"
    tokens.[2] == "IDENT"
    tokens.[3] == "WHITESPACE"
    tokens.[4] == "EQUALS"
    tokens.[5] == "WHITESPACE"
    tokens.[6] == "INT32"

[<Test>]
let ``tokenize should work with multiple lines`` () =
    let source = """let a = 8
let b = 9"""
    let tokens = tokenize source
    let tokensLength = List.length tokens
    tokensLength == 14

    let aTokens =
        List.filter (fun t -> t.LineNumber = 1) tokens

    let aTok = List.item 2 aTokens
    aTok.Content == "a"

    let bTokens =
        List.filter (fun t -> t.LineNumber = 2) tokens

    let bTok = List.item 2 bTokens
    bTok.Content == "b"

[<Test>]
let ``simple line comment should be found in tokens`` () =
    let source = "let a = 7 // some comment"
    let tokens = tokenize source
    let triviaNodes = getTriviaFromTokens tokens

    match List.tryLast triviaNodes with
    | Some ({ Item = Comment (LineCommentAfterSourceCode (lineComment))
              Range = range }) ->
        lineComment == "// some comment"
        range.StartLine == range.EndLine

    | _ -> failwith "expected comment"

[<Test>]
let ``Single line block comment should be found in tokens`` () =
    let source = "let foo (* not fonz *) = bar"
    let tokens = tokenize source
    let triviaNodes = getTriviaFromTokens tokens

    match List.tryLast triviaNodes with
    | Some ({ Item = Comment (BlockComment (blockComment, _, _)) }) -> blockComment == "(* not fonz *)"
    | _ -> failwith "expected block comment"

[<Test>]
let ``multi line block comment should be found in tokens`` () =
    let source = """let bar =
(* multi
   line
   comment *)
    7"""
    let tokens = tokenize source
    let triviaNodes = getTriviaFromTokens tokens

    let expectedComment =
        """(* multi
   line
   comment *)"""
        |> String.normalizeNewLine

    match triviaNodes with
    | [ { Item = Comment (BlockComment (blockComment, _, _))
          Range = range } ] ->
        blockComment == expectedComment
        range.StartLine == 2
        range.EndLine == 4
    | _ -> failwith "expected block comment"

[<Test>]
let ``multiple line comment should be found in tokens`` () =
    let source = """// meh
// foo
let a = 9
"""

    let triviaNodes = tokenize source |> getTriviaFromTokens

    let expectedComment =
        String.normalizeNewLine
            """// meh
// foo"""

    match triviaNodes with
    | ({ Item = Comment (LineCommentOnSingleLine (l1)) }) :: _ -> String.normalizeNewLine l1 == expectedComment
    | _ -> failwith "Expected two line comments"

[<Test>]
let ``newline should be found in tokens`` () =
    let source = """printfn foo

printfn bar"""

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = item; Range = range } ] when (isNewline item) ->
        range.StartLine == 2
        range.EndLine == 2
    | _ -> failwith "expected newline"

[<Test>]
let ``Only empty spaces in line are also consider as Newline`` () =
    let source = """printfn foo

printfn bar""" // difference is the 4 spaces on line 188

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = item; Range = range } ] when (isNewline item) ->
        range.StartLine == 2
        range.EndLine == 2
    | _ -> failwith "expected newline"

[<Test>]
let ``Comment after left brace of record`` () =
    let source = """let a =
    { // foo
    B = 7 }"""

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = Comment (LineCommentAfterSourceCode (comment))
          Range = range } ] ->
        comment == "// foo"
        range.StartLine == 2
    | _ -> failwith "expected line comment after left brace"

[<Test>]
let ``left brace should be found in tokens`` () =
    let source = "type R = { A: int }"

    let triviaNodes =
        tokenize source |> getTriviaNodesFromTokens

    match triviaNodes.[0].Type, triviaNodes.[1].Type, triviaNodes.[2].Type with
    | Token (EQUALS, _), Token (LBRACE, _), Token (RBRACE, _) -> pass ()
    | _ -> fail ()

[<Test>]
let ``leading and trailing whitespaces should be found in tokens`` () =
    let source = """
type T() =
    let x = 123
"""

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = Newline; Range = rAbove } ] -> rAbove.StartLine == 1
    | _ -> fail ()

[<Test>]
let ``if keyword should be found in tokens`` () =
    let source = """if true then ()
elif true then ()"""

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = Keyword ({ Content = "if" }) }; { Item = Keyword ({ Content = "then" }) };
        { Item = Keyword ({ Content = "elif" }) }; { Item = Keyword ({ Content = "then" }) } ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``directives are found in tokens`` () =
    let source = """
#if NOT_DEFINED
#else
let x = 1
#endif
"""

    let defines, hashTokens = TokenParser.getDefines source

    let triviaNodes =
        TokenParser.tokenize defines.[0] hashTokens source
        |> getTriviaFromTokens
        |> List.choose
            (fun tv ->
                match tv.Item with
                | Directive (directive) -> Some directive
                | _ -> None)

    List.length triviaNodes == 3

[<Test>]
let ``member and override`` () =
    let source = """
type MyLogInteface() =
    interface LogInterface with
        member x.Print msg = printfn "%s" msg
        override x.GetLogFile environment = "..."
"""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.choose
            (fun { Item = item } ->
                match item with
                | Keyword ({ Content = kw }) -> Some kw
                | _ -> None)

    match triviaNodes with
    | [ "member"; "override" ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``at before string`` () =
    let source = "@\"foo\""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.filter
            (fun { Item = item } ->
                match item with
                | StringContent ("@\"foo\"") -> true
                | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``newline in string`` () =
    let source = "\"
\""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.filter
            (fun { Item = item } ->
                match item with
                | StringContent ("\"\n\"") -> true
                | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``newline with slashes in string`` () =
    let source = "\"\\r\\n\""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.filter
            (fun { Item = item } ->
                match item with
                | StringContent ("\"\\r\\n\"") -> true
                | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``triple quotes`` () =
    let source = "\"\"\"foo\"\"\""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.filter
            (fun { Item = item } ->
                match item with
                | StringContent ("\"\"\"foo\"\"\"") -> true
                | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``with quotes`` () =
    let quotes = "\\\"\\\""
    let source = "\"" + quotes + "\""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.filter
            (fun { Item = item } ->
                match item with
                | StringContent (sc) when (sc = source) -> true
                | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``infix operator in full words inside an ident`` () =
    let source = """let op_LessThan(a, b) = a < b"""

    let triviaNodes =
        tokenize source
        |> getTriviaFromTokens
        |> List.filter
            (fun { Item = item } ->
                match item with
                | IdentOperatorAsWord "op_LessThan" -> true
                | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``ident between tickets `` () =
    let source = "let ``/ operator combines paths`` = ()"

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = IdentBetweenTicks ("``/ operator combines paths``") } ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``escaped char content`` () =
    let source = "let nulchar = \'\\u0000\'"

    let triviaNodes = tokenize source |> getTriviaFromTokens

    match triviaNodes with
    | [ { Item = CharContent ("\'\\u0000\'") } ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``open close of string on same line`` () =
    let source = "
let a = \"\"
#if FOO
#if BAR
#endif
#endif
"

    getDefines source == [ "FOO"; "BAR" ]

[<Test>]
let ``open close of triple quote string on same line`` () =
    let source = "
let a = \"\"\"foo\"\"\"
#if FOO
#endif
"

    getDefines source == [ "FOO" ]

[<Test>]
let ``open, quote, close of triple quote string on same line`` () =
    let source = "
let a = \"\"\"fo\"o\"\"\"
#if FOO
#endif
"

    getDefines source == [ "FOO" ]

[<Test>]
let ``defines inside string`` () =
    let source = "
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
    let source = "
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
    let source = "
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
    let source = "
let a = \"\"\"\\\"
#if FOO
#if BAR
#endif
#endif
\"\"\"
"

    getDefines source == List<string>.Empty

[<Test>]
let ``defines inside triple quote string, escaped triple quote`` () =
    let source = "
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
    let source = "
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
    let source = "
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
