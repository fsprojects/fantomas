module Fantomas.Tests.TokenParserTests

open Fantomas
open NUnit.Framework
open FsUnit
open Fantomas.TokenParser
open Fantomas.TriviaTypes
open Fantomas.Tests.TestHelper

let private getDefines (v: string) =
    let sourceText = CodeFormatterImpl.getSourceText v

    getDefineCombination sourceText
    |> snd
    |> List.collect id
    |> List.distinct
    |> List.sort

let private getTriviaFromTokens text =
    let source = CodeFormatterImpl.getSourceText text
    let tokens, _ = getDefineCombination source

    getTriviaFromTokens source tokens []

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
let ``simple line comment should be found in tokens`` () =
    let triviaNodes = getTriviaFromTokens "let a = 7 // some comment"

    match List.tryLast triviaNodes with
    | Some ({ Item = Comment (LineCommentAfterSourceCode lineComment)
              Range = range }) ->
        lineComment == "// some comment"
        range.StartLine == range.EndLine

    | _ -> failwith "expected comment"

[<Test>]
let ``single line block comment should be found in tokens`` () =
    let triviaNodes = getTriviaFromTokens "let foo (* not fonz *) = bar"

    match List.tryLast triviaNodes with
    | Some { Item = Comment (BlockComment (blockComment, _, _)) } -> blockComment == "(* not fonz *)"
    | _ -> failwith "expected block comment"

[<Test>]
let ``multi line block comment should be found in tokens`` () =
    let source =
        """let bar =
(* multi
   line
   comment *)
    7"""
        |> String.normalizeNewLine

    let triviaNodes = getTriviaFromTokens source

    let expectedComment =
        """(* multi
   line
   comment *)"""

    match triviaNodes with
    | [ { Item = Comment (BlockComment (blockComment, _, _))
          Range = range } ] ->
        (String.normalizeNewLine blockComment)
        == (String.normalizeNewLine expectedComment)

        range.StartLine == 2
        range.EndLine == 4
    | _ -> failwith "expected block comment"

[<Test>]
let ``multiple line comment should be found in tokens`` () =
    let source =
        """// meh
// foo
let a = 9
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Comment (LineCommentOnSingleLine l1) }; { Item = Comment (LineCommentOnSingleLine l2) } ] ->
        "// meh" == l1
        "// foo" == l2
    | _ -> failwith "Expected two line comments"

[<Test>]
let ``newline should be found in tokens`` () =
    let source =
        """printfn foo

printfn bar"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Newline; Range = range } ] ->
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

[<Test>]
let ``only empty spaces in line are also consider as Newline`` () =
    let source =
        """printfn foo

printfn bar""" // difference is the 4 spaces on line 188

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Newline; Range = range } ] ->
        range.StartLine == 2
        range.EndLine == 2
    | _ -> failwith "expected newline"

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

[<Test>]
let ``leading newlines are not consider trivia`` () =
    let source =
        """

type T() =
    let x = 123
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [] -> pass ()
    | _ -> fail ()

[<Test>]
let ``directives are found in tokens`` () =
    let source =
        """
#if NOT_DEFINED
#else
let x = 1
#endif
"""

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = Directive "#if NOT_DEFINED\n#else" }; { Item = Directive "#endif" } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" triviaNodes)

[<Test>]
let ``at before string but not lead to trivia`` () =
    let source = "@\"foo\""

    let triviaNodes = getTriviaFromTokens source
    Assert.True triviaNodes.IsEmpty

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

[<Test>]
let ``infix operator in full words inside an ident`` () =
    let source = """let op_LessThan(a, b) = a < b"""

    let triviaNodes =
        source
        |> getTriviaFromTokens
        |> List.filter (fun { Item = item } ->
            match item with
            | IdentOperatorAsWord "op_LessThan" -> true
            | _ -> false)

    List.length triviaNodes == 1

[<Test>]
let ``ident between tickets `` () =
    let source = "let ``/ operator combines paths`` = ()"

    let triviaNodes = getTriviaFromTokens source

    match triviaNodes with
    | [ { Item = IdentBetweenTicks "``/ operator combines paths``" } ] -> pass ()
    | _ -> fail ()

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
