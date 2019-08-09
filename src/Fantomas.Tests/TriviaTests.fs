module Fantomas.Tests.TriviaTests

open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.TriviaTypes
    
let private toTrivia source =
    let astWithDefines = parse false source |> Array.toList
    
    astWithDefines
    |> List.map (fun (ast, defines) ->
        let (tokens, lineCount) = TokenParser.tokenize defines source
        Trivia.collectTrivia config tokens lineCount ast
    )
    
let private toTriviaWithDefines source =
    let astWithDefines = parse false source |> Array.toList
    
    astWithDefines
    |> List.map (fun (ast, defines) ->
        let (tokens, lineCount) = TokenParser.tokenize defines source
        defines, Trivia.collectTrivia config tokens lineCount ast
    )
    |> Map.ofList

[<Test>]
let ``Line comment that starts at the beginning of a line added to trivia`` () =
    let source = """// meh
let a = 9
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(LineCommentOnSingleLine(lineComment))];  }
       { ContentItself = Some(Number("9"))}] ->
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
    | [{Type = MainNode("SynModuleOrNamespace.AnonModule") ;ContentAfter = [Comment(LineCommentAfterSourceCode(lineComment))]}; {Type = MainNode("SynExpr.Const"); ContentItself =Some(Number("7"))}] ->
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
    | [{ContentItself = Some(Number("7"))}
       {ContentBefore = [Newline]}
       {ContentItself = Some(Number("9"))}] ->
        pass()
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
    | [{ContentBefore = [Comment(LineCommentOnSingleLine(fooComment));Comment(LineCommentOnSingleLine(barComment))]}
       {ContentItself = Some(Number("7"))}] ->
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
    | [{ Type = TriviaNodeType.Token(t); ContentAfter = [Comment(LineCommentAfterSourceCode("// foo"))] }
       { ContentItself = Some(Number("7")) }] ->
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
    | [ { Type = MainNode(mn); ContentAfter = [Comment(LineCommentOnSingleLine(lineComment))] }
        { ContentItself = Some(Number("123")) } ] ->
        mn == "SynModuleDecl.Types"
        lineComment == "//    override private x.ToString() = \"\""
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
    | [{ ContentAfter = [Comment(BlockComment(comment,_,_))]
         Type = Token { Content = "=" } }; {ContentItself = Some(Number("9"))}] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"

[<Test>]
let ``Block comment and newline added to trivia`` () =
    let source = """(* meh *)
let a =  b
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment,_,true))] }] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"
        
[<Test>]
let ``Block comment on newline EOF added to trivia`` () =
    let source = """let a =  b
(* meh *)"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentAfter = [Comment(BlockComment(comment,true,_))] }] ->
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
    | [{ ContentAfter = [Comment(BlockComment(comment,_,_))] }] ->
        comment == "(* meh *)"
    | _ ->
        failwith "Expected block comment"

[<Test>]
let ``Nested block comment parsed correctly`` () =
    let source = """(* (* meh *) *)
let a =  c + d
"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment,_,true))] }] ->
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
    | [{ ContentBefore = [Comment(BlockComment(comment,_,true))] }
       { ContentItself = Some(Number("9")) }] ->
        comment == "(* // meh *)"
    | _ ->
        failwith "Expected block comment"


[<Test>]
let ``Multiline block comment added to trivia`` () =
    let source = """(* meh
bla *)
let a =  b
"""

    let triviaNodes =
        toTrivia source
        |> List.head

    let expectedComment =
        """(* meh
bla *)"""
        |> String.normalizeNewLine
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(comment,_,true))] }] ->
        comment == expectedComment
    | _ ->
        failwith "Expected block comment"


[<Test>]
let ``Multiple block comments should be linked to same trivia node`` () =
    let source = """let x = y / z
(* foo *)
(* bar *)
x
"""

    let triviaNodes =
        toTrivia source
        |> List.head

    match triviaNodes with
    | [{ContentBefore = [Comment(BlockComment(fooComment,_,true)); Comment(BlockComment(barComment,_,true))]}] ->
        fooComment == "(* foo *)"
        barComment == "(* bar *)"
    | _ ->
        fail()

[<Test>]
let ``Block comment inside line comment parsed correctly`` () =
    let source = """// (* meh *)
let a =  b + c
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
let ``newlines inside a block comment should not counts as newlines`` () =
    let comment = """(*

MEH

*)"""

    let source = sprintf "%s\nprintfn message" comment

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ ContentBefore = [Comment(BlockComment(c,_,true))] }] ->
        c == (String.normalizeNewLine comment)
    | _ ->
        failwith "Expected block comment"

[<Test>]
let ``if keyword before SynExpr.IfThenElse`` () =
    let source = """if true then ()
elif true then ()"""

    let triviaNodes =
        toTrivia source
        |> List.head
    
    match triviaNodes with
    | [{ Type = MainNode("SynExpr.IfThenElse"); ContentBefore = [Keyword({Content = "if"})] }
       { Type = MainNode("SynExpr.IfThenElse"); ContentBefore = [Keyword({Content = "elif"})]}] ->
        pass()
    | _ ->
        fail()
        
[<Test>]
let ``directives before and after are linked to let binding`` () =
    let source = """#if NOT_DEFINED
#else
doSomething()
#endif
"""

    let triviaNodes =
        toTriviaWithDefines source

    let withDefine = Map.find ["NOT_DEFINED"] triviaNodes
    let withoutDefine = Map.find [] triviaNodes

    match withoutDefine with
    | [{ Type = MainNode("SynModuleOrNamespace.AnonModule")
         ContentBefore = [Directive("#if NOT_DEFINED"); Directive("#else")]
         ContentAfter = [Directive("#endif")] }] ->
        pass()
    | _ ->
        fail()
        
    match withDefine with
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
        toTriviaWithDefines source

    let withDefine = Map.find ["NOT_DEFINED"] triviaNodes
    let withoutDefine = Map.find [] triviaNodes
    
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
         ContentAfter = [Directive("#endif")]}] ->
        pass()
    | _ ->
        fail()
        
[<Test>]
let ``Unreachable directive should be present in trivia`` () =
    let source = """namespace Internal.Utilities.Diagnostic
#if EXTENSIBLE_DUMPER
#if DEBUG
type ExtensibleDumper = A | B
#endif
#endif"""

    let triviaNodes =
        toTriviaWithDefines source

    let trivias = Map.find ["DEBUG"] triviaNodes

    match trivias with
    | [{ Type = MainNode("Ident")
         ContentAfter = [Directive("#if EXTENSIBLE_DUMPER")
                         Directive("#if DEBUG")
                         Newline
                         Directive("#endif")
                         Directive("#endif")] }] ->
        pass()
    | _ ->
        fail()

[<Test>]
let ``trailing newlines should not be picked up in trivia`` () =
    let source = """printfn someLogMessage


"""

    let trivia =
        toTrivia source
        |> List.head

    match trivia with
    | [] -> pass()
    | _ -> fail()
