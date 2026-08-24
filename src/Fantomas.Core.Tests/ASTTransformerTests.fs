module Fantomas.Core.Tests.ASTTransformerTests

open NUnit.Framework
open Fantomas.FCS.Text
open Fantomas.FCS.Xml
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.Core
open Fantomas.Core.SyntaxOak

let private parseOak source =
    CodeFormatter.ParseOakAsync(false, source)
    |> Async.RunSynchronously
    |> Array.head
    |> fst

let private getExprFromBinding (oak: Oak) =
    match oak.ModulesOrNamespaces.[0].Declarations.[0] with
    | ModuleDecl.TopLevelBinding binding -> binding.Expr
    | _ ->
        Assert.Fail "Expected TopLevelBinding"
        raise (System.Exception())

let private assertIdent (text: string) (expr: Expr) =
    match expr with
    | Expr.Ident node -> Assert.That(node.Text, Is.EqualTo text)
    | Expr.OptVar node ->
        match List.tryLast node.Identifier.Content with
        | Some(IdentifierOrDot.Ident identNode) -> Assert.That(identNode.Text, Is.EqualTo text)
        | _ -> Assert.Fail $"Expected OptVar ending with Ident '%s{text}', got %A{expr}"
    | _ -> Assert.Fail $"Expected Ident or OptVar '%s{text}', got %A{expr}"

let private assertCount (expected: int) (list: 'a list) =
    Assert.That(List.length list, Is.EqualTo expected)

let private assertText (expected: string) (node: SingleTextNode) =
    Assert.That(node.Text, Is.EqualTo expected)

let private lastIdentText (expr: Expr) : string =
    match expr with
    | Expr.Ident node -> node.Text
    | Expr.OptVar node ->
        match List.tryLast node.Identifier.Content with
        | Some(IdentifierOrDot.Ident identNode) -> identNode.Text
        | _ -> failwith "no ident"
    | _ -> failwithf "not an ident or optvar: %A" expr

/// Like lastIdentText, but also looks through a type application (e.g. `OfType<Customer>` -> "OfType").
let rec private memberName (expr: Expr) : string =
    match expr with
    | Expr.TypeApp node -> memberName node.Identifier
    | _ -> lastIdentText expr

[<Test>]
let ``avoid stack-overflow in long array/list, 2485`` () =
    let mkStringExpr () =
        SynExpr.Const(
            SynConst.String((System.Guid.NewGuid().ToString("N"), SynStringKind.Regular, Range.range0)),
            Range.range0
        )

    let longArrayExpr: SynExpr =
        let rec mkArray count childExpr =
            if count = 20_000 then
                childExpr
            else
                mkArray
                    (count + 1)
                    (SynExpr.Sequential(
                        DebugPointAtSequential.SuppressNeither,
                        true,
                        mkStringExpr (),
                        childExpr,
                        Range.range0,
                        SynExprSequentialTrivia.Zero
                    ))

        SynExpr.ArrayOrListComputed(true, mkArray 0 (mkStringExpr ()), Range.range0)

    let ast =
        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "filename.fsx",
                true,
                QualifiedNameOfFile(Ident("", Range.range0)),
                [],
                [
                    SynModuleOrNamespace(
                        [],
                        false,
                        SynModuleOrNamespaceKind.AnonModule,
                        [ SynModuleDecl.Expr(longArrayExpr, Range.range0) ],
                        PreXmlDoc.Empty,
                        [],
                        None,
                        Range.range0,
                        {
                            LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None
                        }
                    )
                ],
                (false, false),
                ParsedInputTrivia.Empty,
                Set.empty
            )
        )

    let _rootNode = ASTTransformer.mkOak None ast
    Assert.Pass()

// ============================================================
// Chain transformation tests, worked examples from docs/docs/contributors/Chains.md
// ============================================================

[<Test>]
let ``a.Foo(x).Bar() — regular chain with intermediate call and terminal unit call`` () =
    let oak = parseOak "let x = a.Foo(x).Bar()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // head = Identifier(a)
        assertIdent "a" node.Head

        // segments = [DotApplication(., Foo, Paren(x)), DotMember(., Bar)]
        assertCount 2 node.Segments

        match node.Segments with
        | [ ChainSegment.DotApplication(dot1, fooExpr, ChainCall.Paren _); ChainSegment.DotMember(dot2, barExpr) ] ->
            assertText "." dot1
            Assert.That(lastIdentText fooExpr, Is.EqualTo "Foo")
            assertText "." dot2
            Assert.That(lastIdentText barExpr, Is.EqualTo "Bar")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        // terminal = SpaceAllowed(Unit)
        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail "Expected Chain expression"

[<Test>]
let ``a.b.c — pure property access chain`` () =
    let oak = parseOak "let x = a.b.c"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "a" node.Head
        assertCount 2 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, b); ChainSegment.DotMember(_, c) ] ->
            Assert.That(lastIdentText b, Is.EqualTo "b")
            Assert.That(lastIdentText c, Is.EqualTo "c")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.NoTerminal -> ()
        | _ -> Assert.Fail "Expected NoTerminal"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``_.Substring(0,16).ToLower() — DotLambda becomes NoSpaceAllowed terminal`` () =
    let oak = parseOak "let x = arr |> _.Substring(0,16).ToLower()"
    let expr = getExprFromBinding oak

    // The pipe right side should be a Chain
    let rec findChain (e: Expr) : ExprChain =
        match e with
        | Expr.Chain node -> node
        | Expr.InfixApp app -> findChain app.RightHandSide
        | _ ->
            Assert.Fail "Expected to find Chain in expression"
            raise (System.Exception())

    let chainNode = findChain expr

    // head = Identifier(_)
    assertIdent "_" chainNode.Head

    // segments = [DotApplication(., Substring, Paren(...)), DotMember(., ToLower)]
    assertCount 2 chainNode.Segments

    match chainNode.Segments with
    | [ ChainSegment.DotApplication(_, sub, ChainCall.Paren _); ChainSegment.DotMember(_, lower) ] ->
        Assert.That(lastIdentText sub, Is.EqualTo "Substring")
        Assert.That(lastIdentText lower, Is.EqualTo "ToLower")
    | _ -> Assert.Fail $"Unexpected segments: %A{chainNode.Segments}"

    // terminal = NoSpaceAllowed(Unit)
    match chainNode.Terminal with
    | ChainTerminal.NoSpaceAllowed(ChainCall.Unit _) -> ()
    | _ -> Assert.Fail "Expected NoSpaceAllowed(Unit)"

[<Test>]
let ``path.Replace("x","y") — absorbed from AppLongIdentAndSingleParenArg`` () =
    let oak = parseOak """let x = path.Replace("x","y")"""
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "path" node.Head

        // segments = [DotMember(., Replace)]
        assertCount 1 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, replace) ] -> Assert.That(lastIdentText replace, Is.EqualTo "Replace")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        // terminal = SpaceAllowed(Paren(...))
        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Paren paren) ->
            // paren.Expr should be a tuple of the two string args
            match paren.Expr with
            | Expr.Tuple _ -> ()
            | _ -> Assert.Fail "Expected tuple paren for terminal call"
        | _ -> Assert.Fail "Expected SpaceAllowed(Paren)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``List.map (fun x -> x+1) — absorbed from AppWithLambda with no prefix args`` () =
    let oak = parseOak "let x = List.map (fun x -> x+1)"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "List" node.Head

        // segments = [DotMember(., map)]
        assertCount 1 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, map) ] -> Assert.That(lastIdentText map, Is.EqualTo "map")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        // terminal = SpaceAllowed(Paren(lambda))
        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Paren paren) ->
            match paren.Expr with
            | Expr.Lambda _ -> ()
            | _ -> Assert.Fail "Expected lambda inside paren"
        | _ -> Assert.Fail "Expected SpaceAllowed(Paren)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``arr.[0] — standalone dot-indexed becomes single-segment chain`` () =
    let oak = parseOak "let x = arr.[0]"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "arr" node.Head

        // segments = [DotIndex(., [0])]
        assertCount 1 node.Segments

        match node.Segments with
        | [ ChainSegment.DotIndex(dot, idx) ] ->
            assertText "." dot

            match idx with
            | Expr.Constant _ -> ()
            | _ -> Assert.Fail "Expected constant index expression"
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.NoTerminal -> ()
        | _ -> Assert.Fail "Expected NoTerminal"
    | _ -> Assert.Fail "Expected Chain expression"

[<Test>]
let ``arr.[0].Foo() — dot-indexed then chained`` () =
    let oak = parseOak "let x = arr.[0].Foo()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "arr" node.Head

        // segments = [DotIndex(., [0]), DotMember(., Foo)]
        assertCount 2 node.Segments

        match node.Segments with
        | [ ChainSegment.DotIndex(dot1, _); ChainSegment.DotMember(dot2, foo) ] ->
            assertText "." dot1
            assertText "." dot2
            Assert.That(lastIdentText foo, Is.EqualTo "Foo")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        // terminal = SpaceAllowed(Unit)
        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail "Expected Chain expression"

[<Test>]
let ``getBuilder().Configure(opts).Build() — chain whose head is itself a call chain`` () =
    let oak = parseOak "let x = getBuilder().Configure(opts).Build()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // head = Chain(getBuilder()) — a tight call receiver
        match node.Head with
        | Expr.Chain headChain ->
            assertIdent "getBuilder" headChain.Head
            assertCount 0 headChain.Segments

            match headChain.Terminal with
            | ChainTerminal.NoSpaceAllowed(ChainCall.Unit _) -> ()
            | _ -> Assert.Fail "Expected NoSpaceAllowed(Unit) for head chain"
        | _ -> Assert.Fail $"Expected Chain head, got %A{node.Head}"

        // segments = [DotApplication(., Configure, Paren), DotMember(., Build)]
        assertCount 2 node.Segments

        match node.Segments with
        | [ ChainSegment.DotApplication(_, configure, ChainCall.Paren _); ChainSegment.DotMember(_, build) ] ->
            Assert.That(memberName configure, Is.EqualTo "Configure")
            Assert.That(memberName build, Is.EqualTo "Build")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``build(config).Run() — chain whose head is a call with a paren argument`` () =
    let oak = parseOak "let x = build(config).Run()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // head = Chain(build(config)) — a tight call receiver with a paren arg
        match node.Head with
        | Expr.Chain headChain ->
            assertIdent "build" headChain.Head
            assertCount 0 headChain.Segments

            match headChain.Terminal with
            | ChainTerminal.NoSpaceAllowed(ChainCall.Paren _) -> ()
            | _ -> Assert.Fail "Expected NoSpaceAllowed(Paren) for head chain"
        | _ -> Assert.Fail $"Expected Chain head, got %A{node.Head}"

        assertCount 1 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, run) ] -> Assert.That(memberName run, Is.EqualTo "Run")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``query.OfType<Customer>().Where(p).Cast<IEntity>() — type-application call segments and terminal`` () =
    let oak = parseOak "let x = query.OfType<Customer>().Where(p).Cast<IEntity>()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "query" node.Head

        // segments = [DotApplication(., OfType<_>, Unit), DotApplication(., Where, Paren), DotMember(., Cast<_>)]
        assertCount 3 node.Segments

        match node.Segments with
        | [ ChainSegment.DotApplication(_, ofType, ChainCall.Unit _)
            ChainSegment.DotApplication(_, where, ChainCall.Paren _)
            ChainSegment.DotMember(_, cast) ] ->
            Assert.That(memberName ofType, Is.EqualTo "OfType")
            Assert.That(memberName where, Is.EqualTo "Where")
            Assert.That(memberName cast, Is.EqualTo "Cast")

            // The type-application segments are genuinely TypeApp expressions.
            match ofType with
            | Expr.TypeApp _ -> ()
            | _ -> Assert.Fail "Expected OfType to be a TypeApp"

            match cast with
            | Expr.TypeApp _ -> ()
            | _ -> Assert.Fail "Expected Cast to be a TypeApp"
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``a.b<int>.c.Print() — type-application navigation segment (no call)`` () =
    let oak = parseOak "let x = a.b<int>.c.Print()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "a" node.Head

        // segments = [DotMember(., b<int>), DotMember(., c), DotMember(., Print)]
        assertCount 3 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, b); ChainSegment.DotMember(_, c); ChainSegment.DotMember(_, print) ] ->
            Assert.That(memberName b, Is.EqualTo "b")
            Assert.That(memberName c, Is.EqualTo "c")
            Assert.That(memberName print, Is.EqualTo "Print")

            // `.b<int>` is a navigation segment that carries a type application.
            match b with
            | Expr.TypeApp _ -> ()
            | _ -> Assert.Fail "Expected b to be a TypeApp"
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``x().y[0].Foo() — indexed member becomes a segment whose expr is IndexWithoutDot`` () =
    let oak = parseOak "let x = x().y[0].Foo()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // head = Chain(x())
        match node.Head with
        | Expr.Chain headChain ->
            assertIdent "x" headChain.Head
            assertCount 0 headChain.Segments
        | _ -> Assert.Fail $"Expected Chain head, got %A{node.Head}"

        // segments = [DotMember(., IndexWithoutDot(y, [0])), DotMember(., Foo)]
        assertCount 2 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, yIndexed); ChainSegment.DotMember(_, foo) ] ->
            // `.y[0]` is a DotMember whose expr is an IndexWithoutDot (member with a dotless index suffix).
            match yIndexed with
            | Expr.IndexWithoutDot _ -> ()
            | _ -> Assert.Fail $"Expected IndexWithoutDot expr for .y[0], got %A{yIndexed}"

            Assert.That(memberName foo, Is.EqualTo "Foo")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``node.Children[0].Render() — leading indexed member becomes the head`` () =
    // Contrast with `x().y[0]`: when the indexed member is at the START of the chain,
    // the whole `node.Children[0]` is the opaque head (an IndexWithoutDot), not a segment.
    let oak = parseOak "let x = node.Children[0].Render()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // head = IndexWithoutDot(Chain(node.Children), [0])
        match node.Head with
        | Expr.IndexWithoutDot idx ->
            match idx.Identifier with
            | Expr.Chain inner -> assertIdent "node" inner.Head
            | _ -> Assert.Fail $"Expected Chain inside IndexWithoutDot head, got %A{idx.Identifier}"
        | _ -> Assert.Fail $"Expected IndexWithoutDot head, got %A{node.Head}"

        // segments = [DotMember(., Render)]
        assertCount 1 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, render) ] -> Assert.That(memberName render, Is.EqualTo "Render")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``a.Foo(x).b.c.Bar(y) — multiple navigation segments between two calls`` () =
    let oak = parseOak "let x = a.Foo(x).b.c.Bar(y)"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "a" node.Head

        // segments = [DotApplication(., Foo, Paren), DotMember(., b), DotMember(., c), DotMember(., Bar)]
        assertCount 4 node.Segments

        match node.Segments with
        | [ ChainSegment.DotApplication(_, foo, ChainCall.Paren _)
            ChainSegment.DotMember(_, b)
            ChainSegment.DotMember(_, c)
            ChainSegment.DotMember(_, bar) ] ->
            Assert.That(memberName foo, Is.EqualTo "Foo")
            Assert.That(memberName b, Is.EqualTo "b")
            Assert.That(memberName c, Is.EqualTo "c")
            Assert.That(memberName bar, Is.EqualTo "Bar")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        // The trailing call `Bar(y)` is the terminal.
        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Paren _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Paren)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``a.Foo(x).[2].Bar(y) — dot-index segment between two calls`` () =
    let oak = parseOak "let x = a.Foo(x).[2].Bar(y)"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "a" node.Head

        // segments = [DotApplication(., Foo, Paren), DotIndex(., [2]), DotMember(., Bar)]
        assertCount 3 node.Segments

        match node.Segments with
        | [ ChainSegment.DotApplication(_, foo, ChainCall.Paren _)
            ChainSegment.DotIndex(_, idx)
            ChainSegment.DotMember(_, bar) ] ->
            Assert.That(memberName foo, Is.EqualTo "Foo")
            Assert.That(memberName bar, Is.EqualTo "Bar")

            match idx with
            | Expr.Constant _ -> ()
            | _ -> Assert.Fail "Expected constant index expression"
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Paren _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Paren)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``repo.Where(fun a -> a.B).Select(f).ToList() — intermediate call carrying a lambda`` () =
    let oak = parseOak "let x = repo.Where(fun a -> a.B).Select(f).ToList()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        assertIdent "repo" node.Head

        // segments = [DotApplication(., Where, Paren(lambda)), DotApplication(., Select, Paren), DotMember(., ToList)]
        assertCount 3 node.Segments

        match node.Segments with
        | [ ChainSegment.DotApplication(_, where, ChainCall.Paren wherePack)
            ChainSegment.DotApplication(_, select, ChainCall.Paren _)
            ChainSegment.DotMember(_, toList) ] ->
            Assert.That(memberName where, Is.EqualTo "Where")
            Assert.That(memberName select, Is.EqualTo "Select")
            Assert.That(memberName toList, Is.EqualTo "ToList")

            // The intermediate `.Where(...)` call carries a lambda argument.
            match wherePack.Expr with
            | Expr.Lambda _ -> ()
            | _ -> Assert.Fail $"Expected lambda inside Where paren, got %A{wherePack.Expr}"
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``(a.Foo()).[0].Bar(), a dot-index segment after a parenthesised call head`` () =
    // Note: `a.Foo()[0].Bar()` does not parse (FS0597), so the dot-index spelling is
    // required here. (`a.Foo()[0]` on its own parses fine.)
    let oak = parseOak "let x = (a.Foo()).[0].Bar()"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // head = Paren(Chain(a.Foo())) — the paren wraps the inner chain
        match node.Head with
        | Expr.Paren parenNode ->
            match parenNode.Expr with
            | Expr.Chain innerChain ->
                assertIdent "a" innerChain.Head
                // inner chain: a.Foo() — Foo is a segment, () is the terminal
                match innerChain.Segments with
                | [ ChainSegment.DotMember(_, foo) ] -> Assert.That(lastIdentText foo, Is.EqualTo "Foo")
                | _ -> Assert.Fail "Expected single Foo segment in head chain"

                match innerChain.Terminal with
                | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
                | _ -> Assert.Fail "Expected SpaceAllowed(Unit) for inner chain"
            | _ -> Assert.Fail "Expected chain inside paren"
        | _ -> Assert.Fail "Expected Paren as head"

        // segments = [DotIndex(., [0]), DotMember(., Bar)]
        assertCount 2 node.Segments

        match node.Segments with
        | [ ChainSegment.DotIndex(dot1, idx); ChainSegment.DotMember(dot2, bar) ] ->
            assertText "." dot1
            assertText "." dot2
            Assert.That(lastIdentText bar, Is.EqualTo "Bar")

            match idx with
            | Expr.Constant _ -> ()
            | _ -> Assert.Fail "Expected constant index"
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        // terminal = SpaceAllowed(Unit)
        match node.Terminal with
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> ()
        | _ -> Assert.Fail "Expected SpaceAllowed(Unit)"
    | _ -> Assert.Fail "Expected Chain expression"

// ============================================================
// Negative routing — expressions that must NOT become chains
// ============================================================
//
// A chain is a sequence of DOT-separated steps, ending in at most one call. `xs[i]` and
// `f (args)` have no dot at all, and `List.map f (fun ...)` has a prefix argument the
// chain terminal model cannot hold, so the transformer must route all three to their own
// nodes. If a future FCS change alters how they are grouped, these tests fail rather
// than silently widening what a chain is.

[<Test>]
let ``List.map f (fun x -> x+1) — prefix args mean AppWithLambda, not a chain`` () =
    let oak = parseOak "let x = List.map f (fun y -> y + 1)"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.AppWithLambda node ->
        // The lambda is the trailing argument; `f` sits in front of it as a prefix arg.
        // That prefix arg is exactly what disqualifies this from the chain terminal model.
        assertCount 1 node.Arguments
    | _ -> Assert.Fail $"Expected AppWithLambda, got %A{expr}"

[<Test>]
let ``xs[i] — new-style index has no dot, so it is not a chain`` () =
    let oak = parseOak "let x = xs[i]"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.IndexWithoutDot node -> assertIdent "xs" node.Identifier
    | _ -> Assert.Fail $"Expected IndexWithoutDot, got %A{expr}"

[<Test>]
let ``f (args) — an undotted call is AppSingleParenArg, not a chain`` () =
    let oak = parseOak "let x = f (args)"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.AppSingleParenArg node -> assertIdent "f" node.FunctionExpr
    | _ -> Assert.Fail $"Expected AppSingleParenArg, got %A{expr}"

// ============================================================
// Zero-segment chains — the tight-receiver shape
// ============================================================

[<Test>]
let ``X().Y — the receiver call is a chain with zero segments and a tight terminal`` () =
    let oak = parseOak "let x = X().Y"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        // The outer chain is `<head>.Y`.
        assertCount 1 node.Segments

        match node.Segments with
        | [ ChainSegment.DotMember(_, y) ] -> Assert.That(lastIdentText y, Is.EqualTo "Y")
        | _ -> Assert.Fail $"Unexpected segments: %A{node.Segments}"

        match node.Terminal with
        | ChainTerminal.NoTerminal -> ()
        | _ -> Assert.Fail "Expected NoTerminal on the outer chain"

        // The head is itself a chain: `X()` with NO segments at all. It exists only to
        // pair the callee with its argument under a terminal that forbids the space,
        // because `X ().Y` would parse as `X (().Y)`.
        match node.Head with
        | Expr.Chain headChain ->
            assertIdent "X" headChain.Head
            assertCount 0 headChain.Segments

            match headChain.Terminal with
            | ChainTerminal.NoSpaceAllowed(ChainCall.Unit _) -> ()
            | _ -> Assert.Fail $"Expected NoSpaceAllowed(Unit), got %A{headChain.Terminal}"
        | _ -> Assert.Fail $"Expected the head to be a zero-segment Chain, got %A{node.Head}"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

[<Test>]
let ``X(a).Y — a paren receiver call is also a zero-segment tight chain`` () =
    let oak = parseOak "let x = X(a).Y"
    let expr = getExprFromBinding oak

    match expr with
    | Expr.Chain node ->
        match node.Head with
        | Expr.Chain headChain ->
            assertIdent "X" headChain.Head
            assertCount 0 headChain.Segments

            match headChain.Terminal with
            | ChainTerminal.NoSpaceAllowed(ChainCall.Paren _) -> ()
            | _ -> Assert.Fail $"Expected NoSpaceAllowed(Paren), got %A{headChain.Terminal}"
        | _ -> Assert.Fail $"Expected the head to be a zero-segment Chain, got %A{node.Head}"
    | _ -> Assert.Fail $"Expected Chain expression, got %A{expr}"

// ============================================================
// Dot-lambda whose body ends in an indexed member
// ============================================================

[<Test>]
let ``_.Values[0] — indexed member arrives as one opaque segment with no terminal`` () =
    // `Values[0]` has no dot of its own, so it cannot be split into a member plus an
    // index. It reaches the chain as a single opaque step, and because there is no call
    // there is nothing to negotiate a space for.
    let oak = parseOak "let x = xs |> List.map _.Values[0]"
    let expr = getExprFromBinding oak

    // Walk the whole tree looking for the `_` chain.
    let rec search (node: Node) : ExprChain option =
        let isUnderscoreChain (chain: ExprChain) =
            match chain.Head with
            | Expr.Ident n -> n.Text = "_"
            | _ -> false

        match node with
        | :? ExprChain as chain when isUnderscoreChain chain -> Some chain
        | _ -> node.Children |> Array.tryPick search

    match search (Expr.Node expr) with
    | Some chain ->
        assertCount 1 chain.Segments

        match chain.Segments with
        | [ ChainSegment.DotMember(dot, _) ] -> assertText "." dot
        | _ -> Assert.Fail $"Unexpected segments: %A{chain.Segments}"

        match chain.Terminal with
        | ChainTerminal.NoTerminal -> ()
        | _ -> Assert.Fail $"Expected NoTerminal, got %A{chain.Terminal}"
    | None -> Assert.Fail "Expected to find the `_` dot-lambda chain"
