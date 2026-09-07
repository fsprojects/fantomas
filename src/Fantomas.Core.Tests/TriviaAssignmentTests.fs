module Fantomas.Core.Tests.TriviaAssignmentTests

open NUnit.Framework
open Fantomas.Core
open Fantomas.Core.SyntaxOak

// This test suite illustrates where `Trivia.enrichTree` attaches comments and blank lines.
// Each test parses a small source, walks to a node, and asserts which trivia sits before or after it.
// Debug these when a comment ends up in the wrong place after formatting.

let private parseOak (source: string) : Oak =
    CodeFormatter.ParseOakAsync(false, source)
    |> Async.RunSynchronously
    |> Array.head
    |> fst

let private declarations (oak: Oak) : ModuleDecl list =
    oak.ModulesOrNamespaces.[0].Declarations

let private binding (decl: ModuleDecl) : BindingNode =
    match decl with
    | ModuleDecl.TopLevelBinding node -> node
    | other -> failwith $"Expected a binding, got %A{other}"

let private contentsBefore (node: Node) : TriviaContent list =
    node.ContentBefore |> Seq.map _.Content |> Seq.toList

let private contentsAfter (node: Node) : TriviaContent list =
    node.ContentAfter |> Seq.map _.Content |> Seq.toList

let private assertTrivia (expected: TriviaContent list) (actual: TriviaContent list) : unit =
    Assert.That(actual, Is.EqualTo<TriviaContent list>(expected))

[<Test>]
let ``a comment on its own line goes before the next sibling`` () =
    let oak =
        parseOak
            """
let a = 1
// about b
let b = 2
"""

    let b = declarations oak |> List.item 1 |> binding
    assertTrivia [ CommentOnSingleLine "// about b" ] (contentsBefore b)
    // The comment is not attached to `a`, the sibling it follows.
    let a = declarations oak |> List.head |> binding
    assertTrivia List.empty<TriviaContent> (contentsAfter a)

[<Test>]
let ``a comment after code on the same line goes after the last node on that line`` () =
    let oak =
        parseOak
            """
let a = 1 // trailing
let b = 2
"""

    // The container is the module, and the child that ends on the comment's line is the binding `a`.
    // The trivia is pushed down to the last node of that binding, the literal `1`, so that the
    // printer emits the comment right after the code that was on the line, not after the whole binding.
    let a = declarations oak |> List.head |> binding

    match a.Expr with
    | Expr.Constant(Constant.FromText literal) ->
        assertTrivia [ LineCommentAfterSourceCode "// trailing" ] (contentsAfter literal)
    | other -> Assert.Fail $"Expected a constant, got %A{other}"

    assertTrivia List.empty<TriviaContent> (contentsAfter a)

[<Test>]
let ``a comment before a closing bracket goes after the last element`` () =
    let oak =
        parseOak
            """
let xs = [
    1
    // after the last element
]
"""

    // The next sibling is the closing bracket, and a bracket never owns a comment.
    let xs = declarations oak |> List.head |> binding

    match xs.Expr with
    | Expr.ArrayOrList list ->
        assertTrivia List.empty<TriviaContent> (contentsBefore list.Closing)

        match list.Elements with
        | [ Expr.Constant(Constant.FromText one) ] ->
            assertTrivia [ CommentOnSingleLine "// after the last element" ] (contentsAfter one)
        | other -> Assert.Fail $"Expected one element, got %A{other}"
    | other -> Assert.Fail $"Expected a list, got %A{other}"

[<Test>]
let ``an indented comment stays with the sibling at its column`` () =
    let oak =
        parseOak
            """
let x =
    try foo () with _ -> ()
    // still about the try
let y = 1
"""

    // The next sibling in the module, `y`, starts at column 0 and the comment at column 4.
    // The comment therefore goes after the deepest earlier node that starts at column 4, the try-with.
    let x = declarations oak |> List.head |> binding
    let y = declarations oak |> List.item 1 |> binding
    assertTrivia List.empty<TriviaContent> (contentsBefore y)

    match x.Expr with
    | Expr.TryWithSingleClause tryWith ->
        assertTrivia [ CommentOnSingleLine "// still about the try" ] (contentsAfter tryWith)
    | other -> Assert.Fail $"Expected a try-with, got %A{other}"

[<Test>]
let ``a blank line is a Newline trivia before the node that follows it`` () =
    let oak =
        parseOak
            """
let a = 1

let b = 2
"""

    let b = declarations oak |> List.item 1 |> binding
    assertTrivia [ Newline ] (contentsBefore b)

[<Test>]
let ``blank lines before an indented comment travel with the comment`` () =
    let oak =
        parseOak
            """
let f () =
    let a = 1

    // about b
    let b = 2
    a + b
"""

    // The blank line is at column 0 and the comment at column 4. Kept apart, they would be assigned
    // through different rules and could land on different nodes. Together they are one trivia that
    // records how many blank lines preceded the comment.
    let f = declarations oak |> List.head |> binding

    match f.Expr with
    | Expr.CompExprBody body ->
        match body.Statements with
        | [ _; ComputationExpressionStatement.BindingStatement b; _ ] ->
            assertTrivia [ CommentOnSingleLineWithLeadingNewlines(1, "// about b") ] (contentsBefore b)
        | other -> Assert.Fail $"Expected three statements, got %A{other}"
    | other -> Assert.Fail $"Expected a computation expression body, got %A{other}"

[<Test>]
let ``trivia outside every node is attached to the root`` () =
    let oak = parseOak "let a = 1\n// the end"

    // Nothing in the tree spans the final comment, so it is content after the module node.
    let moduleNode = oak.ModulesOrNamespaces.[0]
    assertTrivia [ CommentOnSingleLine "// the end" ] (contentsAfter moduleNode)
