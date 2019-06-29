module Fantomas.Tests.TriviaTests

open System
open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.TriviaTypes
    
let private toTrivia source =
    let astWithDefines = parse false source |> Array.toList
    
    astWithDefines
    |> List.map (fun (ast, defines) ->
        let (tokens, lineCount) = TokenParser.tokenize defines source
        Trivia.collectTrivia tokens lineCount ast
    )

[<Test>]
let ``Line comment that starts at the beginning of a line added to trivia`` () =
    let source = """// meh
let a = 9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(LineCommentOnSingleLine(lineComment))];  }] ->
        lineComment == "// meh"
    | _ ->
        failwith "Expected line comment"

[<Test>]
let ``Line comment that is alone on the single, preceded by whitespaces`` () =
    let source = """    // foo
let a = 'c'
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(LineCommentOnSingleLine(lineComment))];  }] ->
        lineComment == "// foo"
    | _ ->
        failwith "Expected line comment"
        
[<Test>]
let ``Line comment on same line, is after last AST item`` () =
    let source = "let foo = 7 // should be 8"
    let triviaNodes =
        toTrivia source
        |> List.head

    match triviaNodes with
    | [{ContentAfter = [Comment(LineCommentAfterSourceCode(lineComment))]}] ->
        lineComment == "// should be 8"
    | _ ->
        fail()

[<Test>]
let ``Newline pick up before let binding`` () =
    let source = """let a = 7

let b = 9"""
    let triviaNodes =
        toTrivia source
        |> List.head

    match triviaNodes with
    | [{ContentBefore = cb}] ->
        List.length cb == 1
    | _ ->
        fail()

[<Test>]
let ``Multiple comments should be linked to same trivia node`` () =
    let source = """// foo
// bar
let a = 7
"""

    let triviaNodes =
        toTrivia source
        |> List.head

    match triviaNodes with
    | [{ContentBefore = [Comment(LineCommentOnSingleLine(fooComment));Comment(LineCommentOnSingleLine(barComment))]}] ->
        fooComment == "// foo"
        barComment == "// bar"
    | _ ->
        fail()


[<Test>]
let ``Comments inside record`` () =
    let source = """let a = 
    { // foo
    B = 7 }"""
    
    let triviaNodes =
        toTrivia source
        |> List.head

    match triviaNodes with
    | [{ Type = TriviaNodeType.Token(t); ContentAfter = [Comment(LineCommentAfterSourceCode("// foo"))] }] ->
        t.Content == "{"
    | _ ->
        fail()
        
[<Test>]
let ``Comment after all source code`` () =
    let source = """type T() =
    let x = 123
//    override private x.ToString() = ""
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ Type = MainNode(mn); ContentAfter = [Comment(LineCommentOnSingleLine(lineComment))] }] ->
        mn == "SynModuleDecl.Types"
        lineComment == (sprintf "%s//    override private x.ToString() = \"\"" Environment.NewLine)
        pass()
    | _ ->
        fail()
        
[<Test>]
let ``Block comment added to trivia`` () =
    let source = """let a = (* meh *) 9"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentAfter = [Comment(BlockComment(comment))]
         Type = Token { Content = "=" } }] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"

[<Test>]
let ``Block comment and newline added to trivia`` () =
    let source = """(* meh *)
let a =  9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment)); Newline] }] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"
        
[<Test>]
let ``Block comment on newline EOF added to trivia`` () =
    let source = """let a =  9
(* meh *)"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentAfter = [Newline; Comment(BlockComment(comment))] }] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"

[<Test>]
let ``Block comment on EOF added to trivia`` () =
    let source = """let a =  9 (* meh *)"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentAfter = [Comment(BlockComment(comment))] }] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"

[<Test>]
let ``Nested block comment parsed correctly`` () =
    let source = """(* (* meh *) *)
let a =  9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment)); Newline] }] ->
        comment == "(* (* meh *) *)"
    | _ ->
        failwith "Expected block comment"


[<Test>]
let ``Line comment inside block comment parsed correctly`` () =
    let source = """(* // meh *)
let a =  9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment)); Newline] }] ->
        comment == "(* // meh *)"
    | _ ->
        failwith "Expected block comment"


[<Test>]
let ``Multiline block comment added to trivia`` () =
    let source = """(* meh
bla *)
let a =  9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment)); Newline] }] ->
        comment == """(* meh
bla *)"""
    | _ ->
        failwith "Expected block comment"


[<Test>]
let ``Multiple block comments should be linked to same trivia node`` () =
    let source = """let x = 1
(* foo *)
(* bar *)
x
"""

    let triviaNodes =
        toTrivia source
        |> List.head

    match triviaNodes with
    | [{ContentBefore = [Comment(BlockComment(fooComment)); Newline; Comment(BlockComment(barComment)); Newline]}] ->
        fooComment == "(* foo *)"
        barComment == "(* bar *)"
    | _ ->
        fail()

[<Test>]
let ``Block comment inside line comment parsed correctly`` () =
    let source = """// (* meh *)
let a =  9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(LineCommentOnSingleLine(comment))] }] ->
        comment == "// (* meh *)"
    | _ ->
        failwith "Expected line comment"


[<Test>]
let ``if keyword before SynExpr.IfThenElse`` () =
    let source = """if true then ()
elif true then ()"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ Type = MainNode("SynExpr.IfThenElse"); ContentBefore = [Keyword("if")] }
       { Type = MainNode("SynExpr.IfThenElse"); ContentBefore = [Keyword("elif")]}] ->
        pass()
    | _ ->
        fail()
        
[<Test>]
let ``directives before and after are linked to let binding`` () =
    let source = """#if NOT_DEFINED
#else
let x = 1
#endif
"""

    let triviaNodes =
        toTrivia source

    let withDefine = List.head triviaNodes
    let withoutDefine = List.last triviaNodes

    match withDefine with
    | [{ Type = MainNode("SynModuleOrNamespace.AnonModule")
         ContentBefore = [Directive("#if NOT_DEFINED"); Directive("#else")]
         ContentAfter = [] }
       { Type = MainNode("SynModuleDecl.Let")
         ContentBefore = []
         ContentAfter = [Directive("\n#endif")]}] ->
        pass()
    | _ ->
        fail()
        
    match withoutDefine with
    | [{ Type = MainNode("SynModuleOrNamespace.AnonModule")
         ContentBefore = [Directive("#if NOT_DEFINED"); Directive("#else"); Directive("#endif")]
         ContentAfter = [] }] ->
        pass()
    | _ ->
        fail() 

[<Test>]
let ``directive without else clause`` () =
    let source = """#if NOT_DEFINED
let x = 1
#endif
"""
    
    let triviaNodes =
        toTrivia source

    let withoutDefine = List.head triviaNodes
    let withDefine = List.last triviaNodes
    
    match withoutDefine with
    | [{ Type = MainNode("SynModuleOrNamespace.AnonModule")
         ContentBefore = [Directive("#if NOT_DEFINED"); Newline; Directive("#endif")]
         ContentAfter = [] }] ->
        pass()
    | _ ->
        fail()

    match withDefine with
    | [{ Type = MainNode("SynModuleOrNamespace.AnonModule")
         ContentBefore = [Directive("#if NOT_DEFINED")]
         ContentAfter = [] }
       { Type = MainNode("SynModuleDecl.Let")
         ContentBefore = []
         ContentAfter = [Directive("\r\n#endif")]}] ->
        pass()
    | _ ->
        fail()
        
