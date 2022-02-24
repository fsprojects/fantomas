module Fantomas.Tests.TokenParserTests

open System.Xml
open FSharp.Compiler.Text
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
//
//let private getDefines v =
//    let normalizedString = String.normalizeNewLine v
//    let _, hashTokens = getDefines normalizedString
//    getDefinesWords hashTokens
//
//let private tokenize v = tokenize [] [] v
//
//let private mkRange: MkRange =
//    fun (sl, sc) (el, ec) ->
//        FSharp.Compiler.Text.Range.mkRange
//            "TokenParserTests"
//            (FSharp.Compiler.Text.Position.mkPos sl sc)
//            (FSharp.Compiler.Text.Position.mkPos el ec)
//
let private getTriviaFromTokens text =
    CodeFormatterImpl.getSourceText text
    |> fun source -> getTriviaFromTokens source []
    |> fun { Trivia = trivia } -> trivia
//
//[<Test>]
//let ``simple compiler directive should be found`` () =
//    let source =
//        """
//#if DEBUG
//setupServer false
//#else
//setupServer true
//#endif
//"""
//
//    getDefines source |> List.length |> should equal 1
//
//[<Test>]
//let ``simple compiler directive should be DEBUG`` () =
//    let source =
//        """
//#if DEBUG
//setupServer false
//#else
//setupServer true
//#endif
//"""
//
//    getDefines source
//    |> List.head
//    |> should equal "DEBUG"
//
//[<Test>]
//let ``get defines from complex statements`` () =
//    let source =
//        """
//#if INTERACTIVE || (FOO && BAR) || BUZZ
//let x = 1
//#endif
//"""
//
//    getDefines source
//    == [ "INTERACTIVE"; "FOO"; "BAR"; "BUZZ" ]
//
//[<Test>]
//let ``tokens from directive inside a directive are being added`` () =
//    let source =
//        """#if FOO
//  #if BAR
//  #else
//  #endif
//#endif
//"""
//
//    let _, hashTokens = TokenParser.getDefines source
//    getDefinesWords hashTokens == [ "FOO"; "BAR" ]
//
//    let tokens = TokenParser.tokenize [] hashTokens source
//
//    let hashTokens =
//        tokens
//        |> List.filter (fun { TokenInfo = { TokenName = tn } } -> tn = "HASH_IF")
//
//    let furtherInwards =
//        hashTokens
//        |> List.filter (fun { TokenInfo = { LeftColumn = lc } } -> lc = 2)
//
//    List.length hashTokens == 5
//    List.length furtherInwards == 3
//
//
//[<Test>]
//let ``define with underscore`` () =
//    let source =
//        """#if INVARIANT_CULTURE_STRING_COMPARISON
//
//#else
//
//#endif
//"""
//
//    getDefines source
//    == [ "INVARIANT_CULTURE_STRING_COMPARISON" ]
//
//[<Test>]
//let ``tokenize should return correct amount`` () =
//    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32
//
//    tokenize source |> List.length |> should equal 7
//
//[<Test>]
//let ``tokenize should return correct sequence of tokens`` () =
//    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32
//
//    let tokens =
//        tokenize source
//        |> List.map (fun t -> t.TokenInfo.TokenName)
//
//    tokens.[0] == "LET"
//    tokens.[1] == "WHITESPACE"
//    tokens.[2] == "IDENT"
//    tokens.[3] == "WHITESPACE"
//    tokens.[4] == "EQUALS"
//    tokens.[5] == "WHITESPACE"
//    tokens.[6] == "INT32"
//
//[<Test>]
//let ``tokenize should work with multiple lines`` () =
//    let source =
//        """let a = 8
//let b = 9"""
//
//    let tokens = tokenize source
//    let tokensLength = List.length tokens
//    tokensLength == 14
//
//    let aTokens = List.filter (fun t -> t.LineNumber = 1) tokens
//
//    let aTok = List.item 2 aTokens
//    aTok.Content == "a"
//
//    let bTokens = List.filter (fun t -> t.LineNumber = 2) tokens
//
//    let bTok = List.item 2 bTokens
//    bTok.Content == "b"
//
//[<Test>]
//let ``simple line comment should be found in tokens`` () =
//    let source = "let a = 7 // some comment"
//    let tokens = tokenize source
//    let triviaNodes = getTriviaFromTokens tokens
//
//    match List.tryLast triviaNodes with
//    | Some ({ Item = Comment (LineCommentAfterSourceCode lineComment)
//              Range = range }) ->
//        lineComment == "// some comment"
//        range.StartLine == range.EndLine
//
//    | _ -> failwith "expected comment"
//
//[<Test>]
//let ``single line block comment should be found in tokens`` () =
//    let source = "let foo (* not fonz *) = bar"
//    let tokens = tokenize source
//    let triviaNodes = getTriviaFromTokens tokens
//
//    match List.tryLast triviaNodes with
//    | Some { Item = Comment (BlockComment (blockComment, _, _)) } -> blockComment == "(* not fonz *)"
//    | _ -> failwith "expected block comment"
//
//[<Test>]
//let ``multi line block comment should be found in tokens`` () =
//    let source =
//        """let bar =
//(* multi
//   line
//   comment *)
//    7"""
//
//    let tokens = tokenize source
//    let triviaNodes = getTriviaFromTokens tokens
//
//    let expectedComment =
//        """(* multi
//   line
//   comment *)"""
//        |> String.normalizeNewLine
//
//    match triviaNodes with
//    | [ { Item = Comment (BlockComment (blockComment, _, _))
//          Range = range } ] ->
//        blockComment == expectedComment
//        range.StartLine == 2
//        range.EndLine == 4
//    | _ -> failwith "expected block comment"
//
//[<Test>]
//let ``multiple line comment should be found in tokens`` () =
//    let source =
//        """// meh
//// foo
//let a = 9
//"""
//
//    let triviaNodes = tokenize source |> getTriviaFromTokens
//
//    let expectedComment =
//        String.normalizeNewLine
//            """// meh
//// foo"""
//
//    match triviaNodes with
//    | { Item = Comment (LineCommentOnSingleLine l1) } :: _ -> String.normalizeNewLine l1 == expectedComment
//    | _ -> failwith "Expected two line comments"
//
[<Test>]
let ``newline should be found in tokens`` () =
    let source =
        """printfn foo

printfn bar"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = item; Range = range } ] when (isNewline item) ->
        range.StartLine == 2
        range.EndLine == 2
    | _ -> failwith "expected newline"

[<Test>]
let ``trailing newline should not be found in tokens`` () =
    let source =
        """printfn foo

"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [] -> Assert.Pass()
    | _ -> Assert.Fail($"Expected no trivia, got {triviaNodes}")

[<Test>]
let ``two comments on single lines`` () =
    let source =
        """
// one
// two
let a = 0
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment1) }; { Item = Comment (LineCommentOnSingleLine comment2) } ] ->
        comment1 == "// one"
        comment2 == "// two"
    | _ -> Assert.Fail($"Expected trivia, got {triviaNodes}")

[<Test>]
let ``two comments on single lines, with newline in between`` () =
    let source =
        """
// one

// two
let a = 0
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment1) }
        { Item = Newline }
        { Item = Comment (LineCommentOnSingleLine comment2) } ] ->
        comment1 == "// one"
        comment2 == "// two"
    | _ -> Assert.Fail($"Expected trivia, got {triviaNodes}")

[<Test>]
let ``two comments on single lines, with two newline in between`` () =
    let source =
        """
// one


// two
let a = 0
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment1) }
        { Item = Newline }
        { Item = Newline }
        { Item = Comment (LineCommentOnSingleLine comment2) } ] ->
        comment1 == "// one"
        comment2 == "// two"
    | _ -> Assert.Fail($"Expected trivia, got {triviaNodes}")

[<Test>]
let ``comments on single lines, after equals sign`` () =
    let source =
        """
    let innerMultilineFunction () =
        // some comment
        printfn "foo"
    ()
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment) } ] -> comment == "// some comment"
    | _ -> Assert.Fail($"Expected trivia, got {triviaNodes}")

[<Test>]
let ``comments on single line with <`` () =
    let source =
        """
type MaybeBuilder () =
    // M<'T> * ('T -> M<'U>) -> M<'U>
    class end
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment) } ] -> comment == "// M<'T> * ('T -> M<'U>) -> M<'U>"
    | _ -> Assert.Fail($"Expected trivia, got {triviaNodes}")

[<Test>]
let ``block comment at end of line`` () =
    let source =
        """
exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (BlockComment (comment, false, false)) } ] -> comment == "(*filename*)"
    | _ -> Assert.Fail($"Expected BlockComment, got {triviaNodes}")

//
//[<Test>]
//let ``only empty spaces in line are also consider as Newline`` () =
//    let source =
//        """printfn foo
//
//printfn bar""" // difference is the 4 spaces on line 188
//
//    let triviaNodes = tokenize source |> getTriviaFromTokens
//
//    match triviaNodes with
//    | [ { Item = item; Range = range } ] when (isNewline item) ->
//        range.StartLine == 2
//        range.EndLine == 2
//    | _ -> failwith "expected newline"
//
[<Test>]
let ``comment after integer`` () =
    let source = "42 // meh"

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentAfterSourceCode comment)
          Range = range } ] ->
        comment == "// meh"
        range.StartLine == 1
    | _ -> Assert.Fail($"Expected LineCommentAfterSourceCode, got {triviaNodes}")

[<Test>]
let ``comment with multiple spaces after integer`` () =
    let source = "42   //    meh    foo    bar  "

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentAfterSourceCode comment)
          Range = range } ] ->
        comment == "//    meh    foo    bar"
        range.StartLine == 1
    | _ -> Assert.Fail($"Expected LineCommentAfterSourceCode, got {triviaNodes}")

[<Test>]
let ``comment after parenthesis`` () =
    let source =
        """
SomeFunction(arg1,
    arg2,
    arg3) // does something
SomeOtherFunction(arg1, arg2) // does another thing
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentAfterSourceCode comment1)
          Range = range1 }
        { Item = Comment (LineCommentAfterSourceCode comment2)
          Range = range2 } ] ->
        comment1 == "// does something"
        range1.StartLine == 4
        range1.StartColumn == 10
        range1.EndLine == 4
        range1.EndColumn == 27
        comment2 == "// does another thing"
        range2.StartLine == 5
        range2.StartColumn == 30
        range2.EndLine == 5
        range2.EndColumn == 51
    | _ -> Assert.Fail($"Expected LineCommentAfterSourceCode, got {triviaNodes}")

[<Test>]
let ``comment on separate line`` () =
    let source =
        """// some comment
let a = 0
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment) } ] -> "// some comment" == comment
    | _ -> Assert.Fail($"Expected LineCommentOnSingleLine, got {triviaNodes}")

[<Test>]
let ``triple slash comment on separate line not captured`` () =
    let source =
        """/// xml comment
let a = 0
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [] -> Assert.Pass()
    | _ -> Assert.Fail($"Expected no trivia, got {triviaNodes}")

[<Test>]
let ``comment after left brace of record`` () =
    let source =
        """let a =
    { // foo
    B = 7 }"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentAfterSourceCode comment)
          Range = range } ] ->
        comment == "// foo"
        range.StartLine == 2
    | _ -> failwith "expected line comment after left brace"

[<Test>]
let ``comment on single line before attribute`` () =
    let source =
        """
type Commenter =
    { [<JsonProperty("display_name")>]
      // foo
      [<Bar>]
      DisplayName: string }"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment)
          Range = range } ] ->
        comment == "// foo"
        range.StartLine == 4
    | _ -> Assert.Fail($"expected LineCommentOnSingleLine, got {triviaNodes}")

[<Test>]
let ``three comments on single lines`` () =
    let source =
        """
    // You can use the `fun` keyword to write lambda's.
    // Mind the -> instead of C#'s =>
    // TODO: complete the lambda so that the value is returned in uppercase.
    let toUpperCase = fun a -> a"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment1) }
        { Item = Comment (LineCommentOnSingleLine comment2) }
        { Item = Comment (LineCommentOnSingleLine comment3) } ] ->
        comment1
        == "// You can use the `fun` keyword to write lambda's."

        comment2 == "// Mind the -> instead of C#'s =>"

        comment3
        == "// TODO: complete the lambda so that the value is returned in uppercase."
    | _ -> Assert.Fail($"expected LineCommentOnSingleLine, got {triviaNodes}")

[<Test>]
let `` comment followed by newline`` () =
    let source =
        """
    let toUpperCase = fun a -> a
    // ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/lambda-expressions-the-fun-keyword

    let name = "Joey"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment) }; { Item = Newline } ] ->
        comment
        == "// ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/lambda-expressions-the-fun-keyword"
    | _ -> Assert.Fail($"expected LineCommentOnSingleLine, got {triviaNodes}")

[<Test>]
let `` comment after single semicolon is still on single line`` () =
    let source =
        """
type T =
  { id : int
  ; // Delay in ms since it entered the queue
    delay : float }"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine comment) } ] ->
        comment
        == "// Delay in ms since it entered the queue"
    | _ -> Assert.Fail($"expected LineCommentOnSingleLine, got {triviaNodes}")

[<Test>]
let ``quotes in triple quoted string`` () =
    let source = "\"\"\"...\"\"...\"\"\""
    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [] -> Assert.Pass()
    | _ -> Assert.Fail($"expected no trivia, got {triviaNodes}")

(*
type T =
  { id : int
  ; value : RT.Dval
  ; retries : int
  ; canvasID : CanvasID
  ; canvasName : string
  ; module_ : string
  ; name : string
  ; modifier : string
  ; // Delay in ms since it entered the queue
    delay : float }
*)

//
//[<Test>]
//let ``leading and trailing whitespaces should be found in tokens`` () =
//    let source =
//        """
//type T() =
//    let x = 123
//"""
//
//    let triviaNodes = tokenize source |> getTriviaFromTokens
//
//    match triviaNodes with
//    | [ { Item = Newline; Range = rAbove } ] -> rAbove.StartLine == 1
//    | _ -> fail ()
//
//[<Test>]
//let ``directives are found in tokens`` () =
//    let source =
//        """
//#if NOT_DEFINED
//#else
//let x = 1
//#endif
//"""
//
//    let defines, hashTokens = TokenParser.getDefines source
//
//    let triviaNodes =
//        TokenParser.tokenize defines.[0] hashTokens source
//        |> getTriviaFromTokens
//
//    match triviaNodes with
//    | [ { Item = Newline }; { Item = Directive "#if NOT_DEFINED\n#else\n\n#endif" } ] -> pass ()
//    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" triviaNodes)
//
//[<Test>]
//let ``at before string`` () =
//    let source = "@\"foo\""
//
//    let triviaNodes =
//        tokenize source
//        |> getTriviaFromTokens
//        |> List.filter (fun { Item = item } ->
//            match item with
//            | StringContent "@\"foo\"" -> true
//            | _ -> false)
//
//    List.length triviaNodes == 1
//

[<Test>]
let ``simple string should not be trivia`` () =
    let source = "\"a b /\""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [] -> Assert.Pass()
    | _ -> Assert.Fail($"Expected no trivia, got {triviaNodes}")

[<Test>]
let ``detect IdentOperatorAsWord`` () =
    let source = "op_LessThan"

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = IdentOperatorAsWord o } ] -> "op_LessThan" == o
    | _ -> Assert.Fail($"Expected IdentOperatorAsWord, got {triviaNodes}")

//
//[<Test>]
//let ``newline with slashes in string`` () =
//    let source = "\"\\r\\n\""
//
//    let triviaNodes =
//        tokenize source
//        |> getTriviaFromTokens
//        |> List.filter (fun { Item = item } ->
//            match item with
//            | StringContent "\"\\r\\n\"" -> true
//            | _ -> false)
//
//    List.length triviaNodes == 1
//
//[<Test>]
//let ``triple quotes`` () =
//    let source = "\"\"\"foo\"\"\""
//
//    let triviaNodes =
//        tokenize source
//        |> getTriviaFromTokens
//        |> List.filter (fun { Item = item } ->
//            match item with
//            | StringContent "\"\"\"foo\"\"\"" -> true
//            | _ -> false)
//
//    List.length triviaNodes == 1
//
//[<Test>]
//let ``with quotes`` () =
//    let quotes = "\\\"\\\""
//    let source = "\"" + quotes + "\""
//
//    let triviaNodes =
//        tokenize source
//        |> getTriviaFromTokens
//        |> List.filter (fun { Item = item } ->
//            match item with
//            | StringContent sc when (sc = source) -> true
//            | _ -> false)
//
//    List.length triviaNodes == 1
//
//[<Test>]
//let ``infix operator in full words inside an ident`` () =
//    let source = """let op_LessThan(a, b) = a < b"""
//
//    let triviaNodes =
//        tokenize source
//        |> getTriviaFromTokens
//        |> List.filter (fun { Item = item } ->
//            match item with
//            | IdentOperatorAsWord "op_LessThan" -> true
//            | _ -> false)
//
//    List.length triviaNodes == 1
//
//[<Test>]
//let ``ident between tickets `` () =
//    let source = "let ``/ operator combines paths`` = ()"
//
//    let triviaNodes = tokenize source |> getTriviaFromTokens
//
//    match triviaNodes with
//    | [ { Item = IdentBetweenTicks "``/ operator combines paths``" } ] -> pass ()
//    | _ -> fail ()
//
//[<Test>]
//let ``escaped char content`` () =
//    let source = "let nulchar = \'\\u0000\'"
//
//    let triviaNodes = tokenize source |> getTriviaFromTokens
//
//    match triviaNodes with
//    | [ { Item = CharContent "\'\\u0000\'" } ] -> pass ()
//    | _ -> fail ()
//
//[<Test>]
//let ``open close of string on same line`` () =
//    let source =
//        "
//let a = \"\"
//#if FOO
//#if BAR
//#endif
//#endif
//"
//
//    getDefines source == [ "FOO"; "BAR" ]
//
//[<Test>]
//let ``open close of triple quote string on same line`` () =
//    let source =
//        "
//let a = \"\"\"foo\"\"\"
//#if FOO
//#endif
//"
//
//    getDefines source == [ "FOO" ]
//
//[<Test>]
//let ``open, quote, close of triple quote string on same line`` () =
//    let source =
//        "
//let a = \"\"\"fo\"o\"\"\"
//#if FOO
//#endif
//"
//
//    getDefines source == [ "FOO" ]
//
//[<Test>]
//let ``defines inside string`` () =
//    let source =
//        "
//let a = \"
//#if FOO
//#if BAR
//#endif
//#endif
//\"
//"
//
//    getDefines source == List<string>.Empty
//
//[<Test>]
//let ``defines inside string, escaped quote`` () =
//    let source =
//        "
//let a = \"\\\"
//#if FOO
//#if BAR
//#endif
//#endif
//\"
//"
//
//    getDefines source == List<string>.Empty
//
//[<Test>]
//let ``defines inside triple quote string`` () =
//    let source =
//        "
//let a = \"\"\"
//#if FOO
//#if BAR
//#endif
//#endif
//\"\"\"
//"
//
//    getDefines source == List<string>.Empty
//
//[<Test>]
//let ``defines inside triple quote string, escaped quote`` () =
//    let source =
//        "
//let a = \"\"\"\\\"
//#if FOO
//#if BAR
//#endif
//#endif
//\"\"\"
//"
//
//    getDefines source == List<string>.Empty
//
//[<Test>]
//let ``defines inside triple quote string, escaped triple quote`` () =
//    let source =
//        "
//let a = \"\"\"\\\"\"\"
//#if FOO
//#if BAR
//#endif
//#endif
//\"\"\"
//"
//
//    getDefines source == List<string>.Empty
//
//[<Test>]
//let ``backslashes in strings prior to hash directives should not affect token parsing`` () =
//    let source =
//        "
//let file =
//    System.IO.Path.Combine(contentDir,
//                           (n |> System.IO.Path.GetFileNameWithoutExtension)
//                           + \".md\").Replace(\"\\\\\", \"//\")
//
//#if WATCH
//#endif
//"
//
//    getDefines source == [ "WATCH" ]
//
//[<Test>]
//let ``escaped backslash inside escaped string, 1290`` () =
//    let source =
//        "
//[<Test>]
//let ``defines inside string, escaped quote`` () =
//    let source = \"
//let a = \\\"\\\\\\\"
//#if FOO
//  #if BAR
//  #endif
//#endif
//\\\"
//\"
//
//    getDefines source == []
//"
//
//    getDefines source == List<string>.Empty
//
//[<Test>]
//let ``opening quote in line comment, 1504`` () =
//    let source =
//        "
//// \"
//type A () =
//
//#if DEBUG
//  let a() = ()
//#endif
//
//  let f (x: int) =
//    match x with
//    | _ ->
//#if DEBUG
//      ()
//#else
//      ()
//#endif
//    "
//
//    getDefines source == [ "DEBUG" ]
//
//[<Test>]
//let ``opening quote in second line comment`` () =
//    let source =
//        "
//// nothing special here
//// \"
//#if DEBUG
//prinfn \"Debug shizzle\"
//#endif
//    "
//
//    getDefines source == [ "DEBUG" ]
//
//[<Test>]
//let ``backslash in verbatim string`` () =
//    let source =
//        "
//let ProgramFilesX86 =
//    if detected = null then @\"C:\Program Files (x86)\\\" else detected
//
//let isUnix =
//#if NETSTANDARD1_6 || NETSTANDARD2_0
//    meh
//#else
//    foo
//#endif
//        "
//
//    getDefines source
//    == [ "NETSTANDARD1_6"; "NETSTANDARD2_0" ]
