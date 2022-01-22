module Fantomas.Tests.TriviaTests

open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.TriviaTypes

let private collectTrivia =
    Trivia.collectTrivia (fun (sl, sc) (el, ec) ->
        FSharp.Compiler.Text.Range.mkRange
            "TriviaTests"
            (FSharp.Compiler.Text.Position.mkPos sl sc)
            (FSharp.Compiler.Text.Position.mkPos el ec))

let private toTrivia source =
    let astWithDefines = parse false source |> Array.toList

    astWithDefines
    |> List.map (fun (ast, defines, hashTokens) ->
        let tokens = TokenParser.tokenize defines hashTokens source

        collectTrivia tokens ast)

let private toTriviaWithDefines source =
    let astWithDefines = parse false source |> Array.toList

    astWithDefines
    |> List.map (fun (ast, defines, hashTokens) ->
        let tokens = TokenParser.tokenize defines hashTokens source

        defines, collectTrivia tokens ast)
    |> Map.ofList

[<Test>]
let ``line comment that starts at the beginning of a line added to trivia`` () =
    let source =
        """// meh
let a = 9
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (LineCommentOnSingleLine lineComment) ] } ] -> lineComment == "// meh"
    | _ -> failwith "Expected line comment"

[<Test>]
let ``line comment that is alone on the single, preceded by whitespaces`` () =
    let source =
        """    // foo
let a = 'c'
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (LineCommentOnSingleLine lineComment) ] } ] -> lineComment == "// foo"
    | _ -> failwith "Expected line comment"

[<Test>]
let ``line comment on same line, is after last AST item`` () =
    let source = "let foo = 7 // should be 8"
    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { Type = MainNode SynConst_Int32
          ContentAfter = [ Comment (LineCommentAfterSourceCode lineComment) ] } ] -> lineComment == "// should be 8"
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" triviaNodes)

[<Test>]
let ``newline pick up before let binding`` () =
    let source =
        """let a = 7

let b = 9"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Newline ] } ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``multiple comments should be linked to same trivia node`` () =
    let source =
        """// foo
// bar
let a = 7
"""

    let triviaNodes = toTrivia source |> List.head

    let expectedComment =
        String.normalizeNewLine
            """// foo
// bar"""

    match triviaNodes with
    | [ { ContentBefore = [ Comment (LineCommentOnSingleLine comments) ] } ] ->
        String.normalizeNewLine comments
        == expectedComment
    | _ -> fail ()

[<Test>]
let ``comments inside record`` () =
    let source =
        """let a =
    { // foo
    B = 7 }"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { Type = TriviaNodeType.MainNode SynExpr_Record_OpeningBrace
          ContentAfter = [ Comment (LineCommentAfterSourceCode "// foo") ] } ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``comment after all source code`` () =
    let source =
        """type T() =
    let x = 123
//    override private x.ToString() = ""
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { Type = MainNode mn
          ContentAfter = [ Comment (LineCommentOnSingleLine lineComment) ] } ] ->
        mn == SynModuleDecl_Types

        lineComment
        == "//    override private x.ToString() = \"\""

        pass ()
    | _ -> fail ()

[<Test>]
let ``block comment added to trivia`` () =
    let source = """let a = (* meh *) 9"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (comment, _, _)) ] } ] -> comment == "(* meh *)"
    | _ -> failwith "Expected block comment"

[<Test>]
let ``block comment and newline added to trivia`` () =
    let source =
        """(* meh *)
let a =  b
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (comment, _, true)) ] } ] -> comment == "(* meh *)"
    | _ -> failwith "Expected block comment"

[<Test>]
let ``block comment on newline EOF added to trivia`` () =
    let source =
        """let a =  b
(* meh *)"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentAfter = [ Comment (BlockComment (comment, true, _)) ] } ] -> comment == "(* meh *)"
    | _ -> failwith "Expected block comment"

[<Test>]
let ``block comment on EOF added to trivia`` () =
    let source = """let a =  9 (* meh *)"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentAfter = [ Comment (BlockComment (comment, _, _)) ] } ] -> comment == "(* meh *)"
    | _ -> failwith "Expected block comment"

[<Test>]
let ``nested block comment parsed correctly`` () =
    let source =
        """(* (* meh *) *)
let a =  c + d
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (comment, _, true)) ] } ] -> comment == "(* (* meh *) *)"
    | _ -> failwith "Expected block comment"


[<Test>]
let ``line comment inside block comment parsed correctly`` () =
    let source =
        """(* // meh *)
let a =  9
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (comment, _, true)) ] } ] -> comment == "(* // meh *)"
    | _ -> failwith "Expected block comment"


[<Test>]
let ``multiline block comment added to trivia`` () =
    let source =
        """(* meh
bla *)
let a =  b
"""

    let triviaNodes = toTrivia source |> List.head

    let expectedComment =
        """(* meh
bla *)"""
        |> String.normalizeNewLine

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (comment, _, true)) ] } ] -> comment == expectedComment
    | _ -> failwith "Expected block comment"


[<Test>]
let ``multiple block comments should be linked to same trivia node`` () =
    let source =
        """let x = y / z
(* foo *)
(* bar *)
x
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (combinedComment, _, true)) ] } ] ->
        combinedComment == "(* foo *)\n(* bar *)"
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" triviaNodes)

[<Test>]
let ``block comment inside line comment parsed correctly`` () =
    let source =
        """// (* meh *)
let a =  b + c
"""

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (LineCommentOnSingleLine comment) ] } ] -> comment == "// (* meh *)"
    | _ -> failwith "Expected line comment"

[<Test>]
let ``newlines inside a block comment should not counts as newlines`` () =
    let comment =
        """(*

MEH

*)"""

    let source = sprintf "%s\nprintfn message" comment

    let triviaNodes = toTrivia source |> List.head

    match triviaNodes with
    | [ { ContentBefore = [ Comment (BlockComment (c, _, true)) ] } ] -> c == (String.normalizeNewLine comment)
    | _ -> failwith "Expected block comment"

[<Test>]
let ``directives before and after are linked to let binding`` () =
    let source =
        """#if NOT_DEFINED
#else
doSomething()
#endif
"""

    let triviaNodes = toTriviaWithDefines source

    let withDefine = Map.find [ "NOT_DEFINED" ] triviaNodes
    let withoutDefine = Map.find [] triviaNodes

    match withoutDefine with
    | [ { Type = MainNode SynModuleDecl_DoExpr
          ContentBefore = [ Directive "#if NOT_DEFINED\n#else" ]
          ContentAfter = [ Directive "#endif" ] } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" withoutDefine)

    match withDefine with
    | [ { Type = MainNode LongIdent_
          ContentBefore = [ Directive "#if NOT_DEFINED"; Directive "#else"; Directive "#endif" ]
          ContentAfter = [] } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" withDefine)

[<Test>]
let ``directive without else clause`` () =
    let source =
        """#if NOT_DEFINED
let x = 1
#endif
"""

    let triviaNodes = toTriviaWithDefines source

    let withDefine = Map.find [ "NOT_DEFINED" ] triviaNodes
    let withoutDefine = Map.find [] triviaNodes

    match withoutDefine with
    | [ { Type = MainNode LongIdent_
          ContentAfter = [ Directive "#if NOT_DEFINED\n\n#endif" ]
          ContentBefore = [] } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" withoutDefine)

    match withDefine with
    | [ { Type = MainNode LongIdent_
          ContentBefore = [ Directive "#if NOT_DEFINED" ]
          ContentAfter = [] }
        { Type = MainNode SynModuleDecl_Let
          ContentBefore = []
          ContentAfter = [ Directive "#endif" ] } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" withDefine)

[<Test>]
let ``unreachable directive should be present in trivia`` () =
    let source =
        """namespace Internal.Utilities.Diagnostic
#if EXTENSIBLE_DUMPER
#if DEBUG
type ExtensibleDumper = A | B
#endif
#endif"""

    let triviaNodes = toTriviaWithDefines source

    let trivias = Map.find [ "DEBUG" ] triviaNodes

    match trivias with
    | [ { Type = MainNode LongIdent_
          ContentAfter = [ Directive "#if EXTENSIBLE_DUMPER\n#if DEBUG\n\n#endif\n#endif" ] } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia %A" trivias)

[<Test>]
let ``trailing newlines should not be picked up in trivia`` () =
    let source =
        """printfn someLogMessage


"""

    let trivia = toTrivia source |> List.head

    match trivia with
    | [] -> pass ()
    | _ -> fail ()


[<Test>]
let ``code in non-active defines should be returned in trivia`` () =
    let source =
        """#if SOMETHING
let foo = 42
#endif"""

    let trivia = toTriviaWithDefines source |> Map.find []

    match trivia with
    | [ { Type = MainNode LongIdent_
          ContentAfter = [ Directive "#if SOMETHING\n\n#endif" ] } ] -> pass ()
    | _ -> fail ()

[<Test>]
let ``string constant with blank lines`` () =
    let multilineString =
        "some

content

with empty lines"
        |> String.normalizeNewLine

    let source = sprintf "let x = \"%s\"" multilineString

    let trivia = toTrivia source |> List.head

    match trivia with
    | [ { ContentItself = Some (StringContent sc)
          Type = TriviaNodeType.MainNode SynConst_String } ] -> sc == sprintf "\"%s\"" multilineString
    | _ -> fail ()

[<Test>]
let ``triple quote string constant with blank lines`` () =
    let multilineString =
        "some

content

with empty lines"
        |> String.normalizeNewLine

    let source = sprintf "let x = \"\"\"%s\"\"\"" multilineString

    let trivia = toTrivia source |> List.head

    match trivia with
    | [ { ContentItself = Some (StringContent sc)
          Type = TriviaNodeType.MainNode SynConst_String } ] -> sc == sprintf "\"\"\"%s\"\"\"" multilineString
    | _ -> fail ()

[<Test>]
let ``char content`` () =
    let source = "let nulchar = \'\\u0000\'"
    let trivia = toTrivia source |> List.head

    match trivia with
    | [ { ContentItself = Some (CharContent "\'\\u0000\'")
          Type = TriviaNodeType.MainNode SynConst_Char } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia: %A" trivia)

[<Test>]
let ``leading newlines should not be captured as trivia`` () =
    let source =
        """
let a = b
"""

    let trivia = toTrivia source |> List.head

    match trivia with
    | [] -> pass ()
    | _ -> fail ()

[<Test>]
let ``multiple line comments form a single trivia`` () =
    let source =
        """
/// Represents a long identifier with possible '.' at end.
///
/// Typically dotms.Length = lid.Length-1, but they may be same if (incomplete) code ends in a dot, e.g. "Foo.Bar."
/// The dots mostly matter for parsing, and are typically ignored by the typechecker, but
/// if dotms.Length = lid.Length, then the parser must have reported an error, so the typechecker is allowed
/// more freedom about typechecking these expressions.
/// LongIdent can be empty list - it is used to denote that name of some AST element is absent (i.e. empty type name in inherit)
type LongIdentWithDots =
    | LongIdentWithDots of id: LongIdent * dotms: range list
"""

    let trivia = toTrivia source |> List.head

    let expectedComment =
        String.normalizeNewLine
            """/// Represents a long identifier with possible '.' at end.
///
/// Typically dotms.Length = lid.Length-1, but they may be same if (incomplete) code ends in a dot, e.g. "Foo.Bar."
/// The dots mostly matter for parsing, and are typically ignored by the typechecker, but
/// if dotms.Length = lid.Length, then the parser must have reported an error, so the typechecker is allowed
/// more freedom about typechecking these expressions.
/// LongIdent can be empty list - it is used to denote that name of some AST element is absent (i.e. empty type name in inherit)"""

    match trivia with
    | [ { ContentBefore = [ Comment (LineCommentOnSingleLine comment) ] } ] ->
        String.normalizeNewLine comment == expectedComment
    | _ -> fail ()

[<Test>]
let ``number expression`` () =
    let source = sprintf "let x = 2.0m"

    let trivia = toTrivia source |> List.head

    match trivia with
    | [ { ContentItself = Some (Number n)
          Type = TriviaNodeType.MainNode _ } ] -> n == "2.0m"
    | _ -> fail ()

[<Test>]
let ``collect multiple hash directive as one`` () =
    let source =
        """
let x =
    #if DEBUG
    printfn "DEBUG"
    #endif
    ()
"""

    let trivia = toTriviaWithDefines source |> Map.find []

    printfn "%A" trivia

    match trivia with
    | [ { Type = TriviaNodeType.MainNode SynConst_Unit
          ContentBefore = [ Directive "#if DEBUG\n\n#endif" ] } ] -> pass ()
    | _ -> Assert.Fail(sprintf "Unexpected trivia: %A" trivia)
