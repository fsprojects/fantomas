module Fantomas.Core.Tests.CodePrinterHelperFunctionsTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Context
open Fantomas.Core
open Fantomas.Core.SyntaxOak

// This test suite is created to illustrate the various helper functions that are being used in `CodePrinter`.
// We encourage you to debug these when you are new to the code base.
// It might help for some things to "click".

/// Transform the WriterEvents in a Context to a string
let private dump (context: Context) : string = (dump false context).Code

[<Test>]
let ``!- add a single WriterEvent.Write`` () =
    let f (context: Context) : Context =
        // (!-) is a custom operator defined in `Context.fs`
        !- "one new event" context

    let contextBefore: Context = Context.Default
    Assert.That(contextBefore.WriterEvents.ToSeq() |> Seq.isEmpty, Is.True)

    // Calling function `f` with an empty context will add a single event to the context.
    let contextAfter: Context = f contextBefore
    // The events are stored in a mutable doubly-linked list called `EventList`.
    // Checkout `EventList.fs` to learn more about this collection type.
    let events = contextAfter.WriterEvents.ToSeq() |> Seq.toList
    Assert.That(1, Is.EqualTo events.Length)

    match events with
    | [ Write "one new event" ] -> Assert.Pass()
    | events -> Assert.Fail $"Expected one event, got: %A{events}"

[<Test>]
let ``+> will compose two functions`` () =
    let f (context: Context) : Context = !- "f" context
    let g (context: Context) : Context = !- " and g" context

    // (+>) is very similar to `>>` in F#
    // There is an implementation detail but conceptually it is the same.
    let h (context: Context) : Context =
        // This is the equivalent of `g (f context)`
        (f +> g) context

    let contextBefore: Context = Context.Default
    Assert.That(contextBefore.WriterEvents.ToSeq() |> Seq.isEmpty, Is.True)

    let contextAfter: Context = h contextBefore
    // We expect two events to be added to the context.
    // "f - long" and "g - long"
    let events = contextAfter.WriterEvents.ToSeq() |> Seq.toList
    Assert.That(events.Length, Is.EqualTo 2)
    let code = dump contextAfter
    Assert.AreEqual("f and g", code)

[<Test>]
let ``partially application when composing function`` () =
    // We can write the previous example in a more concise way.
    // Because of partial application in F#: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#partial-application-of-arguments
    let f = !-"f" // signature of f remains the same: Context -> Context
    let g = !-" and g" // signature of g remains the same: Context -> Context
    let h = f +> g // signature of h remains the same: Context -> Context

    // Conceptually, you should really read `+>` as and then.
    // Apply the function on the left and then the function on the right.
    // Both function can potentially add events to the Context.

    let contextBefore: Context = Context.Default
    let contextAfter: Context = h contextBefore

    let events = contextAfter.WriterEvents.ToSeq() |> Seq.toList
    Assert.That(2, Is.EqualTo events.Length)
    let code = dump contextAfter
    Assert.AreEqual("f and g", code)

[<Test>]
let ``the Context module has a lot of helper functions`` () =
    // Checkout `Context.fs` to see all the helper functions.
    let starArrowColon = sepStar +> sepArrow +> sepColon

    // The convention `sep` stands for separator.
    let contextBefore: Context = Context.Default

    let contextAfter: Context = starArrowColon contextBefore

    let code = dump contextAfter
    Assert.AreEqual("*  -> :", code)

[<Test>]
let ``some helper function are clever like sepSpace`` () =
    let f = !-"a" +> sepSpace +> sepSpace +> !-"b"

    let contextBefore: Context = Context.Default
    let contextAfter: Context = f contextBefore
    let code = dump contextAfter

    // Wait! Why is there only one space between `a` and `b`?
    // Because `sepSpace` is a helper function that will check if there already is a space as last character in the current line.
    // This is a very useful function to make sure we don't add duplicate spaces.
    // Depending on where we are in the code, we cannot predict what the previous function will have added to the `Context`.
    Assert.AreEqual("a b", code)

[<Test>]
let ``other helper function respect configuration settings`` () =
    let f = !-"a" +> sepColon +> !-"b"
    let defaultConfig: Context = Context.Default
    // The `FormatConfig` is present in the `Context`.
    let configWithSpaceBeforeTrue =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    SpaceBeforeColon = true
                }
        }

    let codeWithDefaultSettings: string = f defaultConfig |> dump

    Assert.AreEqual("a: b", codeWithDefaultSettings)

    let codeWithSpaceBeforeTrue: string = f configWithSpaceBeforeTrue |> dump

    Assert.AreEqual("a : b", codeWithSpaceBeforeTrue)

[<Test>]
let ``traversing collections`` () =
    // It is easy to compose a function when everything is fixed, but what to do when the AST has a list of items?
    let items = [ 2; 3; 4 ]
    // The `col` function will traverse the collection and apply the first function between elements and the last function for each individual element.
    let f (items: int seq) : Context -> Context =
        col (!-" + ") items (fun (item: int) -> !- $"%i{item}")

    // Note that there are some variants of `col` that can be used to process a collection in a different way.
    // coli, colEx, ...

    let ctx = Context.Default
    let code = f items ctx |> dump
    Assert.AreEqual("2 + 3 + 4", code)

[<Test>]
let ``newlines and indentation`` () =
    // We can update the WriterModel to indent the next line using the `WriterEvent.IndentBy` event
    // The commonly used helper function for this is `indent`
    // Indentation only kick in on the next line, this is something to be aware of.
    // `sepNln` is a helper function that will add a newline.

    let f = !-"first line" +> sepNln +> indent +> !-"second line"
    // The dump function will respect the newline from the configuration.
    // For this test we will set it to `EndOfLineStyle.LF`
    let mkCtx () =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let code = f (mkCtx ()) |> dump
    Assert.AreEqual("first line\nsecond line", code)
    // There is no indentation because that would only kick in after the second line.

    let g = !-"first line" +> indent +> sepNln +> !-"second line" +> unindent
    let indentedCode = g (mkCtx ()) |> dump
    Assert.AreEqual("first line\n    second line", indentedCode)

    // Using `indent` typically goes together with and `unindent` call.
    // This is a very common pattern in CodePrinter, so the use of `indentSepNlnUnindent` is encouraged.
    // Forgetting to `unindent` can be a nasty bug in Fantomas.
    let h = !-"first line" +> indentSepNlnUnindent (!-"second line")
    let indentedCtx = h (mkCtx ())
    let indentedCode = dump indentedCtx
    Assert.AreEqual("first line\n    second line", indentedCode)

    let events = indentedCtx.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ Write "first line"; IndentBy 4; WriteLine; Write "second line"; UnIndentBy 4 ] -> Assert.Pass()
    | events -> Assert.Fail $"Expected one event, got: %A{events}"

[<Test>]
let ``trying multiple code paths`` () =
    // Sometimes we want to try and fit everything in a single line.
    // And have a fallback behavior when that is not possible.
    let short = !-"This fits on a single line"
    let long = !-"This fits on" +> sepNln +> !-"two lines"
    // `expressionFitsOnRestOfLine` will try the first expression and if it doesn't fit, it will try the second expression.
    // All the events of the first expression will be remove from the context when it needs to fallback to the second expression.
    let f = expressionFitsOnRestOfLine short long
    // The remainder of the line is calculated by the `max_line_length` and the current column of the WriterModel.
    // We will artificially set the max_line_length to 10, to trigger the fallback behavior.
    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    MaxLineLength = 10
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let code = f ctx |> dump
    Assert.AreEqual("This fits on\ntwo lines", code)

// There are other various helper functions for code path fallback.
// `isShortExpression`, `sepSpaceIfShortExpressionOrAddIndentAndNewline`, `leadingExpressionIsMultiline`, ...

[<Test>]
let ``printing trivia instructions`` () =
    // Source code will be transformed to a Oak tree.
    // In `CodePrinter` in process this Oak and all its child nodes.
    // A `Node` interface can store TriviaNodes in the ContentBefore or ContentAfter collection.
    let _sourceCode =
        """
let a =
    // code comment
    b"""

    // Let's create a dummy Oak
    // In practise, a FCS Syntax tree will be transformed to an Oak
    let zeroRange = Fantomas.FCS.Text.Range.range0
    let stn text = SingleTextNode(text, zeroRange)

    let tree =
        Oak(
            [],
            [
                ModuleOrNamespaceNode(
                    None,
                    [
                        ModuleDecl.TopLevelBinding(
                            BindingNode(
                                None,
                                None,
                                MultipleTextsNode([ stn "let" ], zeroRange),
                                false,
                                None,
                                None,
                                Choice1Of2(IdentListNode([ IdentifierOrDot.Ident(stn "a") ], zeroRange)),
                                None,
                                [],
                                None,
                                stn "=",
                                Expr.Ident(stn "b"),
                                None,
                                zeroRange
                            )
                        )
                    ],
                    zeroRange
                )
            ],
            zeroRange
        )

    let genExpr (expr: Expr) =
        match expr with
        | Expr.Ident identNode -> !-identNode.Text
        | _ -> !-"error"

    let f (genExpr: Expr -> Context -> Context) (tree: Oak) : Context -> Context =
        match tree.ModulesOrNamespaces.[0].Declarations.[0] with
        | ModuleDecl.TopLevelBinding bindingNode ->
            let genLet = !-bindingNode.LeadingKeyword.Content.[0].Text

            let genFunctionName =
                match bindingNode.FunctionName with
                | Choice2Of2 _ -> !-"error"
                | Choice1Of2 functionNameNode ->

                match functionNameNode.Content with
                | [ IdentifierOrDot.Ident node ] -> !-node.Text
                | _ -> !-"error"

            let genEq = !-bindingNode.Equals.Text

            genLet
            +> sepSpace
            +> genFunctionName
            +> sepSpace
            +> genEq
            // Try to add a space and print the expression.
            // If the expression is multiline add indent, newline, print the expression and unindent.
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr bindingNode.Expr)
        | _ -> !-"error"

    let mkCtx () =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let codeWithoutTriviaPrinting = f genExpr tree (mkCtx ()) |> dump
    Assert.AreEqual("let a = b", codeWithoutTriviaPrinting)

    // The problem now is that our tree doesn't contain the code comment.
    // We need to add the comment to the right node in the tree.
    // In practise, this happens in Trivia.fs in a generic way.
    // For our example we will add it by traversing the tree hardcoded.
    match tree.ModulesOrNamespaces.[0].Declarations with
    | [ ModuleDecl.TopLevelBinding bindingNode ] ->
        let exprNode = Expr.Node bindingNode.Expr
        exprNode.AddBefore(TriviaNode(TriviaContent.CommentOnSingleLine "// code comment", zeroRange))
    | _ -> ()

    // We need to write a better function, where the code comment will be restored in the expression.
    // `genNode` is typically the go-to function to write a trivia instruction.
    // As it is not exposed from `CodePrinter`, we need to write our own.
    let genExprWithTrivia (expr: Expr) : Context -> Context =
        match expr with
        | Expr.Ident identNode ->
            // We try and grab the first comment from the generic Node interface.
            let firstComment =
                match Seq.tryHead identNode.ContentBefore with
                | None -> sepNone
                | Some triviaNode ->

                // If found we check the content and try to print the comment text followed by a newline
                match triviaNode.Content with
                | CommentOnSingleLine comment -> !-comment +> sepNln
                | _ -> !-"error"

            firstComment +> !-identNode.Text
        | _ -> !-"error"

    let codeWithTriviaPrinting = f genExprWithTrivia tree (mkCtx ()) |> dump
    Assert.AreEqual("let a =\n    // code comment\n    b", codeWithTriviaPrinting)

[<Test>]
let ``blank lines trivia`` () =
    // Blank lines are also printed as trivia.
    // However, in some situations they can clash with a composition that always adds a new line.

    let _cleanInput =
        """let a = 1
let b = 2
"""

    // Imagine that we always want to print a new line between let bindings.
    let zeroRange = Fantomas.FCS.Text.Range.range0
    let stn text = SingleTextNode(text, zeroRange)

    let mkBinding name body =
        BindingNode(
            None,
            None,
            MultipleTextsNode([ stn "let" ], zeroRange),
            false,
            None,
            None,
            Choice1Of2(IdentListNode([ IdentifierOrDot.Ident(stn name) ], zeroRange)),
            None,
            [],
            None,
            stn "=",
            Expr.Ident(stn body),
            None,
            zeroRange
        )

    let tree =
        Oak(
            [],
            [
                ModuleOrNamespaceNode(
                    None,
                    [
                        ModuleDecl.TopLevelBinding(mkBinding "a" "1")
                        ModuleDecl.TopLevelBinding(mkBinding "b" "2")
                    ],
                    zeroRange
                )
            ],
            zeroRange
        )

    let enterNode (node: Node) =
        col
            sepNln
            node.ContentBefore
            (fun (tn: TriviaNode) ->
                match tn.Content with
                | Newline -> sepNln
                | _ -> sepNone
            )

    let f (tree: Oak) =
        match tree.ModulesOrNamespaces.[0].Declarations with
        | [ ModuleDecl.TopLevelBinding a; ModuleDecl.TopLevelBinding b ] ->

            let genBinding (node: BindingNode) =
                let name =
                    match node.FunctionName with
                    | Choice1Of2 iln ->
                        match iln.Content with
                        | [ IdentifierOrDot.Ident ident ] -> ident
                        | _ -> failwith "expected single ident"
                    | _ -> failwith "expected single ident"

                let body =
                    match node.Expr with
                    | Expr.Ident ident -> ident
                    | _ -> failwith "expected ident expr"

                // print trivia before BindingNode
                enterNode node
                +> !-"let"
                +> sepSpace
                +> !-name.Text
                +> sepEq
                +> sepSpace
                +> !-body.Text

            genBinding a
            // One `sepNln` to move to the next line.
            +> sepNln
            // Another to insert a complete blank line.
            +> sepNln
            +> genBinding b

        | _ -> !-"error"

    let mkCtx () =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let formattedCode = f tree (mkCtx ()) |> dump
    Assert.AreEqual("let a = 1\n\nlet b = 2", formattedCode)

    // This worked fine, but the next time we will format there will be a TriviaNode for the blank line.
    // The newly found newline will be added to the last top level binding node
    match tree.ModulesOrNamespaces.[0].Declarations.[1] with
    | ModuleDecl.TopLevelBinding binding -> binding.AddBefore(TriviaNode(TriviaContent.Newline, zeroRange))
    | _ -> ()

    let formattedCodeWithTrivia = f tree (mkCtx ()) |> dump
    // Notice that we now have two blank lines.
    // One from the trivia, and one from the fixed sepNln inside `f`.
    Assert.AreEqual("let a = 1\n\n\nlet b = 2", formattedCodeWithTrivia)

    // The next time we ran this code (assuming all the trivia instructions are provided properly), we would get three newlines.
    // So, this is becoming a repeating newline bug.

    let g (tree: Oak) =
        match tree.ModulesOrNamespaces.[0].Declarations with
        | [ ModuleDecl.TopLevelBinding a; ModuleDecl.TopLevelBinding b ] ->

            let genBinding (node: BindingNode) =
                let name =
                    match node.FunctionName with
                    | Choice1Of2 iln ->
                        match iln.Content with
                        | [ IdentifierOrDot.Ident ident ] -> ident
                        | _ -> failwith "expected single ident"
                    | _ -> failwith "expected single ident"

                let body =
                    match node.Expr with
                    | Expr.Ident ident -> ident
                    | _ -> failwith "expected ident expr"

                // print trivia before BindingNode
                enterNode node
                +> !-"let"
                +> sepSpace
                +> !-name.Text
                +> sepEq
                +> sepSpace
                +> !-body.Text

            // Normally in CodePrinter you would use `sepNlnUnlessContentBefore` but it is not exposed.
            let sepNlnUnlessBindingHasTrivia (node: Node) =
                if Seq.isEmpty node.ContentBefore then sepNln else sepNone

            genBinding a
            // One `sepNln` to move to the next line.
            +> sepNln
            // Another to insert a complete blank line.
            +> sepNlnUnlessBindingHasTrivia b
            +> genBinding b

        | _ -> !-"error"

    let finalCode = g tree (mkCtx ()) |> dump
    Assert.AreEqual("let a = 1\n\nlet b = 2", finalCode)

[<Test>]
let ``locking the indentation at a fixed column`` () =
    // In some scenarios we need to keep code indented at a fixed column.
    // This is typically to produce valid F# due to the offset rules.
    let f =
        sepOpenT
        +> atCurrentColumn (!-"first line" +> sepNln +> !-"second line")
        +> sepCloseT

    let ctxBefore =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let ctxAfter = f ctxBefore
    let code = dump ctxAfter
    // `atCurrentColumn` will lock the column at position one
    // Notice the space before the "second line", it is there because of the line will start at column 1.
    Assert.AreEqual("(first line\n second line)", code)

    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ WriterEvent.Write "("
        WriterEvent.SetAtColumn 1
        WriterEvent.Write "first line"
        WriterEvent.WriteLine
        WriterEvent.Write "second line"
        WriterEvent.RestoreAtColumn 0
        WriterEvent.RestoreIndent 0
        WriterEvent.Write ")" ] -> Assert.Pass()
    | events -> Assert.Fail $"Expected one event, got: %A{events}"

// There is also a variation of `atCurrentColumn`: `atCurrentColumnIndent`
// This locks the column and also applies indentation from that column.
// `atCurrentColumn` does not have an influence over the indentation.

// In general, you want to avoid using `atCurrentColumn` and `atCurrentColumnIndent` as it breaks the "indentation flow".
// "indentation flow" is a made up term to indicate that every indent is a multitude of the `indent_size`.

// ============================================================================
// WriterEvent handling in dump
// These tests verify that specific event patterns produce the expected output.
// ============================================================================

let private mkLfCtx () =
    { Context.Default with
        Config =
            { Context.Default.Config with
                EndOfLine = EndOfLineStyle.LF
            }
    }

[<Test>]
let ``WriteLineInsideStringConst produces a raw newline without indentation`` () =
    // Even when indented, WriteLineInsideStringConst should not add indentation.
    // This is how multiline strings are preserved verbatim.
    let f =
        !-"let s = \""
        +> indent
        +> writerEvent WriteLineInsideStringConst
        +> !-"second line of string"

    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("let s = \"\nsecond line of string", code)

[<Test>]
let ``WriteLineInsideTrivia produces a raw newline without indentation`` () =
    let f = !-"(* comment" +> writerEvent WriteLineInsideTrivia +> !-"   continued *)"

    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("(* comment\n   continued *)", code)

[<Test>]
let ``WriteBeforeNewline queues text that appears before the next newline`` () =
    // WriteBeforeNewline is used for trailing line comments: "code // comment\n"
    let f =
        !-"code"
        +> writerEvent (WriteBeforeNewline " // trailing")
        +> sepNln
        +> !-"next line"

    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("code // trailing\nnext line", code)

[<Test>]
let ``WriteBeforeNewline without a following newline is flushed by finalizeWriterModel`` () =
    let f = !-"code" +> writerEvent (WriteBeforeNewline " // trailing")
    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("code // trailing", code)

[<Test>]
let ``trailing spaces are trimmed on each line`` () =
    let f = !-"hello   " +> sepNln +> !-"world   "
    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("hello\nworld", code)

[<Test>]
let ``leading blank lines are stripped in non-selection mode`` () =
    let f = sepNln +> sepNln +> !-"content"
    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("content", code)

[<Test>]
let ``leading blank lines are preserved in selection mode`` () =
    let f = sepNln +> sepNln +> !-"content"
    let ctx = f (mkLfCtx ())
    let code = (Context.dump true ctx).Code
    Assert.AreEqual("\n\ncontent", code)

// =============================================================================
// Separator helpers
// =============================================================================

[<Test>]
let ``sepSpace does not duplicate trailing space`` () =
    let f = !-"a " +> sepSpace +> !-"b"
    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("a b", code)

[<Test>]
let ``sepNlnForTrivia emits WriteLineBecauseOfTrivia`` () =
    let f = !-"code" +> sepNlnForTrivia +> !-"trivia line"
    let ctx = f (mkLfCtx ())
    let code = dump ctx
    Assert.AreEqual("code\ntrivia line", code)

    let events = ctx.WriterEvents.ToSeq() |> Seq.toList

    let hasTriviaNln =
        events
        |> List.exists (
            function
            | WriteLineBecauseOfTrivia -> true
            | _ -> false
        )

    Assert.That(hasTriviaNln, Is.True, "Expected WriteLineBecauseOfTrivia event")

[<Test>]
let ``sepNlnUnlessLastEventIsNewline skips newline after existing newline`` () =
    let f = !-"line" +> sepNln +> sepNlnUnlessLastEventIsNewline +> !-"next"
    let code = f (mkLfCtx ()) |> dump
    // Only one newline, the second one is skipped
    Assert.AreEqual("line\nnext", code)

[<Test>]
let ``sepNlnUnlessLastEventIsNewline adds newline when last event is not newline`` () =
    let f = !-"line" +> sepNlnUnlessLastEventIsNewline +> !-"next"
    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("line\nnext", code)

[<Test>]
let ``lastWriteEventIsNewline returns true after newline`` () =
    let ctx = (!-"a" +> sepNln) (mkLfCtx ())
    Assert.That(lastWriteEventIsNewline ctx, Is.True)

[<Test>]
let ``lastWriteEventIsNewline returns false after write`` () =
    let ctx = (!-"a" +> sepNln +> !-"b") (mkLfCtx ())
    Assert.That(lastWriteEventIsNewline ctx, Is.False)

[<Test>]
let ``lastWriteEventIsNewline skips trailing restore events`` () =
    let ctx = (!-"a" +> indentSepNlnUnindent (!-"b")) (mkLfCtx ())
    let events = ctx.WriterEvents.ToSeq() |> Seq.toList

    // Verify the events end with UnIndentBy after the Write
    match events with
    | [ Write "a"; IndentBy 4; WriteLine; Write "b"; UnIndentBy 4 ] ->
        // lastWriteEventIsNewline should skip UnIndentBy and find Write, returning false
        Assert.That(lastWriteEventIsNewline ctx, Is.False)
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

// =============================================================================
// WriteBeforeNewline-aware helpers
// =============================================================================

[<Test>]
let ``sepNlnWhenWriteBeforeNewlineNotEmpty emits newline when content is queued`` () =
    let f =
        !-"code"
        +> writerEvent (WriteBeforeNewline " // comment")
        +> sepNlnWhenWriteBeforeNewlineNotEmpty
        +> !-"next"

    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("code // comment\nnext", code)

[<Test>]
let ``sepNlnWhenWriteBeforeNewlineNotEmpty is no-op when nothing is queued`` () =
    let f = !-"code" +> sepNlnWhenWriteBeforeNewlineNotEmpty +> !-" more"
    let code = f (mkLfCtx ()) |> dump
    Assert.AreEqual("code more", code)

// =============================================================================
// Speculative formatting / line-length checks
// =============================================================================

[<Test>]
let ``futureNlnCheck returns true for multiline expression`` () =
    let multiline = !-"first" +> sepNln +> !-"second"
    let ctx = mkLfCtx ()
    Assert.That(futureNlnCheck multiline ctx, Is.True)

[<Test>]
let ``futureNlnCheck returns false for single-line expression`` () =
    let singleLine = !-"short"
    let ctx = mkLfCtx ()
    Assert.That(futureNlnCheck singleLine ctx, Is.False)

[<Test>]
let ``futureNlnCheck leaves no trace in the event list`` () =
    let ctx = (!-"prefix") (mkLfCtx ())
    let eventsBefore = ctx.WriterEvents.ToSeq() |> Seq.toList
    let _ = futureNlnCheck (!-"probe" +> sepNln +> !-"more") ctx
    let eventsAfter = ctx.WriterEvents.ToSeq() |> Seq.toList
    Assert.AreEqual(eventsBefore, eventsAfter)

[<Test>]
let ``exceedsWidth returns true when expression is wider than allowed`` () =
    let wide = !-"this is a long expression"
    let ctx = mkLfCtx ()
    Assert.That(exceedsWidth 5 wide ctx, Is.True)

[<Test>]
let ``exceedsWidth returns false when expression fits`` () =
    let narrow = !-"ok"
    let ctx = mkLfCtx ()
    Assert.That(exceedsWidth 50 narrow ctx, Is.False)

[<Test>]
let ``exceedsWidth leaves no trace in the event list`` () =
    let ctx = (!-"prefix") (mkLfCtx ())
    let eventsBefore = ctx.WriterEvents.ToSeq() |> Seq.toList
    let _ = exceedsWidth 5 (!-"probe content") ctx
    let eventsAfter = ctx.WriterEvents.ToSeq() |> Seq.toList
    Assert.AreEqual(eventsBefore, eventsAfter)

[<Test>]
let ``expressionFitsOnRestOfLine keeps events when expression fits`` () =
    let short = !-"fits"
    let long = !-"does not" +> sepNln +> !-"fit"
    let ctx = mkLfCtx ()
    let code = expressionFitsOnRestOfLine short long ctx |> dump
    Assert.AreEqual("fits", code)

[<Test>]
let ``isShortExpression uses maxWidth to decide`` () =
    let short = !-"ab"
    let long = !-"fallback"
    // maxWidth of 5: "ab" (length 2) fits within 5
    let code = isShortExpression 5 short long (mkLfCtx ()) |> dump
    Assert.AreEqual("ab", code)

[<Test>]
let ``isShortExpression falls back when expression exceeds maxWidth`` () =
    let short = !-"this is too long for the width"
    let long = !-"fallback"
    // maxWidth of 5: the short expression exceeds it
    let code = isShortExpression 5 short long (mkLfCtx ()) |> dump
    Assert.AreEqual("fallback", code)

[<Test>]
let ``isSmallExpression with NumberOfItems falls back when item count exceeds max`` () =
    let short = !-"short"
    let long = !-"long"
    let size = Size.NumberOfItems(items = 10, maxItems = 3)
    let code = isSmallExpression size short long (mkLfCtx ()) |> dump
    Assert.AreEqual("long", code)

[<Test>]
let ``isSmallExpression with NumberOfItems uses expressionFitsOnRestOfLine when under max`` () =
    let short = !-"short"
    let long = !-"long"
    let size = Size.NumberOfItems(items = 2, maxItems = 3)
    let code = isSmallExpression size short long (mkLfCtx ()) |> dump
    Assert.AreEqual("short", code)

[<Test>]
let ``autoIndentAndNlnIfExpressionExceedsPageWidth indents when expression is too long`` () =
    let expr = !-"a long expression that will not fit"

    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    MaxLineLength = 20
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let code =
        (!-"let x =" +> autoIndentAndNlnIfExpressionExceedsPageWidth expr) ctx |> dump

    Assert.AreEqual("let x =\n    a long expression that will not fit", code)

[<Test>]
let ``autoIndentAndNlnIfExpressionExceedsPageWidth keeps inline when it fits`` () =
    let expr = !-"1"

    let code =
        (!-"let x = " +> autoIndentAndNlnIfExpressionExceedsPageWidth expr) (mkLfCtx ())
        |> dump

    Assert.AreEqual("let x = 1", code)

[<Test>]
let ``sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth adds space when it fits`` () =
    let expr = !-"1"

    let code =
        (!-"let x =" +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth expr) (mkLfCtx ())
        |> dump

    Assert.AreEqual("let x = 1", code)

[<Test>]
let ``sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth indents when too long`` () =
    let expr = !-"a long expression"

    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    MaxLineLength = 15
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let code =
        (!-"let x =" +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth expr) ctx
        |> dump

    Assert.AreEqual("let x =\n    a long expression", code)

// =============================================================================
// Leading expression inspection
// =============================================================================

[<Test>]
let ``leadingExpressionResult reports line count and column`` () =
    let leading = !-"first" +> sepNln +> !-"second"

    let mutable lineBefore = -1
    let mutable colBefore = -1
    let mutable lineAfter = -1
    let mutable colAfter = -1

    let _ctx =
        leadingExpressionResult
            leading
            (fun ((lb, cb), (la, ca)) ctx ->
                lineBefore <- lb
                colBefore <- cb
                lineAfter <- la
                colAfter <- ca
                ctx
            )
            (mkLfCtx ())

    Assert.AreEqual(0, lineBefore)
    Assert.AreEqual(0, colBefore)
    Assert.AreEqual(1, lineAfter)
    Assert.AreEqual(6, colAfter) // "second" = 6 chars

[<Test>]
let ``leadingExpressionIsMultiline detects multiline`` () =
    let leading = !-"first" +> sepNln +> !-"second"
    let mutable result = false

    let _ctx =
        leadingExpressionIsMultiline
            leading
            (fun isMulti ctx ->
                result <- isMulti
                ctx
            )
            (mkLfCtx ())

    Assert.That(result, Is.True)

[<Test>]
let ``leadingExpressionIsMultiline detects single line`` () =
    let leading = !-"single line"
    let mutable result = true

    let _ctx =
        leadingExpressionIsMultiline
            leading
            (fun isMulti ctx ->
                result <- isMulti
                ctx
            )
            (mkLfCtx ())

    Assert.That(result, Is.False)

// =============================================================================
// Multiline item handling
// =============================================================================

[<Test>]
let ``colWithNlnWhenItemIsMultiline with all single-line items`` () =
    let items =
        [
            ColMultilineItem(!-"let a = 1", sepNln)
            ColMultilineItem(!-"let b = 2", sepNln)
            ColMultilineItem(!-"let c = 3", sepNln)
        ]

    let code = colWithNlnWhenItemIsMultiline items (mkLfCtx ()) |> dump
    Assert.AreEqual("let a = 1\nlet b = 2\nlet c = 3", code)

[<Test>]
let ``colWithNlnWhenItemIsMultiline adds blank line around multiline item`` () =
    let items =
        [
            ColMultilineItem(!-"let a = 1", sepNln)
            ColMultilineItem(!-"let b =" +> indentSepNlnUnindent (!-"longBody"), sepNln)
            ColMultilineItem(!-"let c = 3", sepNln)
        ]

    let code = colWithNlnWhenItemIsMultiline items (mkLfCtx ()) |> dump
    // Blank line before and after the multiline item
    Assert.AreEqual("let a = 1\n\nlet b =\n    longBody\n\nlet c = 3", code)

// =============================================================================
// unindentWithTriviaAwareness
// =============================================================================

[<Test>]
let ``unindentWithTriviaAwareness splices before line comment`` () =
    let f =
        indent
        +> !-"someContent"
        +> writerEvent WriteLineBecauseOfTrivia
        +> writerEvent (WriteTrivia "// comment")
        +> writerEvent WriteLineBecauseOfTrivia

    let ctx = f (mkLfCtx ())
    let ctxAfter = unindentWithTriviaAwareness ctx
    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ IndentBy 4
        Write "someContent"
        WriteLineBecauseOfTrivia
        WriteTrivia "// comment"
        UnIndentBy 4
        WriteLineBecauseOfTrivia ] -> Assert.Pass()
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

[<Test>]
let ``unindentWithTriviaAwareness splices before block comment`` () =
    let f =
        indent
        +> !-"someContent"
        +> writerEvent WriteLineBecauseOfTrivia
        +> writerEvent (WriteTrivia "(* block comment *)")
        +> writerEvent WriteLineBecauseOfTrivia

    let ctx = f (mkLfCtx ())
    let ctxAfter = unindentWithTriviaAwareness ctx
    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ IndentBy 4
        Write "someContent"
        WriteLineBecauseOfTrivia
        WriteTrivia "(* block comment *)"
        UnIndentBy 4
        WriteLineBecauseOfTrivia ] -> Assert.Pass()
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

[<Test>]
let ``unindentWithTriviaAwareness falls back to normal unindent without trailing trivia`` () =
    let f = indent +> !-"someContent"
    let ctx = f (mkLfCtx ())
    let ctxAfter = unindentWithTriviaAwareness ctx
    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ IndentBy 4; Write "someContent"; UnIndentBy 4 ] -> Assert.Pass()
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

[<Test>]
let ``unindentWithTriviaAwareness falls back when WriteLineBecauseOfTrivia not preceded by comment`` () =
    let f = indent +> !-"someContent" +> writerEvent WriteLineBecauseOfTrivia
    let ctx = f (mkLfCtx ())
    let ctxAfter = unindentWithTriviaAwareness ctx
    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ IndentBy 4; Write "someContent"; WriteLineBecauseOfTrivia; UnIndentBy 4 ] -> Assert.Pass()
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

[<Test>]
let ``unindentWithTriviaAwareness splices before multiline block comment`` () =
    // Multiline block comment gets split by WriterEvents.normalize into
    // WriteTrivia per line with WriteLineInsideTrivia between them
    let f =
        indent
        +> !-"someContent"
        +> writerEvent WriteLineBecauseOfTrivia
        +> writerEvent (WriteTrivia "(*")
        +> writerEvent WriteLineInsideTrivia
        +> writerEvent (WriteTrivia "        comment")
        +> writerEvent WriteLineInsideTrivia
        +> writerEvent (WriteTrivia "    *)")
        +> writerEvent WriteLineBecauseOfTrivia

    let ctx = f (mkLfCtx ())
    let ctxAfter = unindentWithTriviaAwareness ctx

    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ IndentBy 4
        Write "someContent"
        WriteLineBecauseOfTrivia
        WriteTrivia "(*"
        WriteLineInsideTrivia
        WriteTrivia "        comment"
        WriteLineInsideTrivia
        WriteTrivia "    *)"
        UnIndentBy 4
        WriteLineBecauseOfTrivia ] -> Assert.Pass()
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

[<Test>]
let ``unindentWithTriviaAwareness finds trivia past trailing restore events`` () =
    // Simulates the try-with pattern where restore/unindent events follow the trivia
    let f =
        indent
        +> !-"content"
        +> writerEvent WriteLineBecauseOfTrivia
        +> writerEvent (WriteTrivia "// trailing comment")
        +> writerEvent WriteLineBecauseOfTrivia
        +> writerEvent (RestoreAtColumn 0)
        +> writerEvent (RestoreIndent 0)

    let ctx = f (mkLfCtx ())
    let ctxAfter = unindentWithTriviaAwareness ctx
    let events = ctxAfter.WriterEvents.ToSeq() |> Seq.toList

    match events with
    | [ IndentBy 4
        Write "content"
        WriteLineBecauseOfTrivia
        WriteTrivia "// trailing comment"
        UnIndentBy 4
        WriteLineBecauseOfTrivia
        RestoreAtColumn 0
        RestoreIndent 0 ] -> Assert.Pass()
    | _ -> Assert.Fail $"Unexpected events: %A{events}"

// =============================================================================
// Trivia-before-unindent integration
// =============================================================================

[<Test>]
let ``autoIndentAndNlnIfExpressionExceedsPageWidth splices unindent before trailing trivia`` () =
    // Simulate an expression with trailing trivia (comment + WriteLineBecauseOfTrivia).
    // The unindent should be spliced before the trailing newline so the newline
    // uses the reduced indent level.
    let exprWithTrailingComment =
        !-"someContent"
        +> writerEvent WriteLineBecauseOfTrivia
        +> !-"// trailing comment"
        +> writerEvent WriteLineBecauseOfTrivia

    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    MaxLineLength = 20
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let code =
        (!-"let x ="
         +> autoIndentAndNlnIfExpressionExceedsPageWidth exprWithTrailingComment)
            ctx
        |> dump

    // The comment should be at indent 4, the newline after it at indent 0
    Assert.AreEqual("let x =\n    someContent\n    // trailing comment\n", code)

[<Test>]
let ``autoIndentAndNlnIfExpressionExceedsPageWidth without trailing trivia still unindents normally`` () =
    let expr = !-"a long expression that does not fit"

    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    MaxLineLength = 20
                    EndOfLine = EndOfLineStyle.LF
                }
        }

    let code =
        (!-"let x ="
         +> autoIndentAndNlnIfExpressionExceedsPageWidth expr
         +> sepNln
         +> !-"next")
            ctx
        |> dump

    Assert.AreEqual("let x =\n    a long expression that does not fit\nnext", code)
