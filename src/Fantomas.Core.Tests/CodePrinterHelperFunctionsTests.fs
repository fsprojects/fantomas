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
    Assert.IsTrue(contextBefore.WriterEvents.Length = 0)

    // Calling function `f` with an empty context will add a single event to the context.
    let contextAfter: Context = f contextBefore
    Assert.IsTrue(contextAfter.WriterEvents.Length = 1)
    // The events are stored in a custom collection type called `Queue`.
    // Checkout `Queue.fs` to learn more about this collection type.
    let events = Queue.toSeq contextAfter.WriterEvents |> Seq.toList

    match events with
    | [ Write "one new event" ] -> Assert.Pass()
    | events -> Assert.Fail $"Expected one event, got: ${events}"

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
    Assert.IsTrue(contextBefore.WriterEvents.Length = 0)

    let contextAfter: Context = h contextBefore
    // We expect two events to be added to the context.
    // "f - long" and "g - long"
    Assert.IsTrue(contextAfter.WriterEvents.Length = 2)
    let code = dump contextAfter
    Assert.AreEqual("f and g", code)

[<Test>]
let ``partially application when composing function`` () =
    // We can write the previous example in a more concise way.
    // Because of partial application in F#: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#partial-application-of-arguments
    let f = !- "f" // signature of f remains the same: Context -> Context
    let g = !- " and g" // signature of g remains the same: Context -> Context
    let h = f +> g // signature of h remains the same: Context -> Context

    // Conceptually, you should really read `+>` as and then.
    // Apply the function on the left and then the function on the right.
    // Both function can potentially add events to the Context.

    let contextBefore: Context = Context.Default
    let contextAfter: Context = h contextBefore

    Assert.IsTrue(contextAfter.WriterEvents.Length = 2)
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
    let f = !- "a" +> sepSpace +> sepSpace +> !- "b"

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
    let f = !- "a" +> sepColon +> !- "b"
    let defaultConfig: Context = Context.Default
    // The `FormatConfig` is present in the `Context`.
    let configWithSpaceBeforeTrue =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    SpaceBeforeColon = true } }

    let codeWithDefaultSettings: string = f defaultConfig |> dump

    Assert.AreEqual("a: b", codeWithDefaultSettings)

    let codeWithSpaceBeforeTrue: string = f configWithSpaceBeforeTrue |> dump

    Assert.AreEqual("a : b", codeWithSpaceBeforeTrue)

[<Test>]
let ``traversing collections`` () =
    // It is easy to compose a function when everything is fixed, but what to do when the AST has a list of items?
    let items = [ 2; 3; 4 ]
    // The `col` function will traverse the collection and apply the first function between elements and the last function for each individual element.
    let f (items: seq<int>) : Context -> Context =
        col (!- " + ") items (fun (item: int) -> !- $"{item}")

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

    let f = !- "first line" +> sepNln +> indent +> !- "second line"
    // The dump function will respect the newline from the configuration.
    // For this test we will set it to `EndOfLineStyle.LF`
    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF } }

    let code = f ctx |> dump
    Assert.AreEqual("first line\nsecond line", code)
    // There is no indentation because that would only kick in after the second line.

    let g = !- "first line" +> indent +> sepNln +> !- "second line" +> unindent
    let indentedCode = g ctx |> dump
    Assert.AreEqual("first line\n    second line", indentedCode)

    // Using `indent` typically goes together with and `unindent` call.
    // This is a very common pattern in CodePrinter, so the use of `indentSepNlnUnindent` is encouraged.
    // Forgetting to `unindent` can be a nasty bug in Fantomas.
    let h = !- "first line" +> indentSepNlnUnindent (!- "second line")
    let indentedCtx = h ctx
    let indentedCode = dump indentedCtx
    Assert.AreEqual("first line\n    second line", indentedCode)

    let events = Queue.toSeq indentedCtx.WriterEvents |> Seq.toList

    match events with
    | [ Write "first line"; IndentBy 4; WriteLine; Write "second line"; UnIndentBy 4 ] -> Assert.Pass()
    | events -> Assert.Fail $"Expected one event, got: ${events}"

[<Test>]
let ``trying multiple code paths`` () =
    // Sometimes we want to try and fit everything in a single line.
    // And have a fallback behavior when that is not possible.
    let short = !- "This fits on a single line"
    let long = !- "This fits on" +> sepNln +> !- "two lines"
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
                    EndOfLine = EndOfLineStyle.LF } }

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
    let zeroRange = FSharp.Compiler.Text.Range.Zero
    let stn text = SingleTextNode(text, zeroRange)

    let tree =
        Oak(
            [],
            [ ModuleOrNamespaceNode(
                  None,
                  [ ModuleDecl.TopLevelBinding(
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
                            zeroRange
                        )
                    ) ],
                  zeroRange
              ) ],
            zeroRange
        )

    let genExpr (expr: Expr) =
        match expr with
        | Expr.Ident identNode -> !-identNode.Text
        | _ -> !- "error"

    let f (genExpr: Expr -> Context -> Context) (tree: Oak) : Context -> Context =
        match tree.ModulesOrNamespaces.[0].Declarations.[0] with
        | ModuleDecl.TopLevelBinding bindingNode ->
            let genLet = !-bindingNode.LeadingKeyword.Content.[0].Text

            let genFunctionName =
                match bindingNode.FunctionName with
                | Choice1Of2 functionNameNode ->
                    match functionNameNode.Content with
                    | [ IdentifierOrDot.Ident node ] -> !-node.Text
                    | _ -> !- "error"
                | Choice2Of2 _ -> !- "error"

            let genEq = !-bindingNode.Equals.Text

            genLet
            +> sepSpace
            +> genFunctionName
            +> sepSpace
            +> genEq
            // Try to add a space and print the expression.
            // If the expression is multiline add indent, newline, print the expression and unindent.
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr bindingNode.Expr)
        | _ -> !- "error"

    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF } }

    let codeWithoutTriviaPrinting = f genExpr tree ctx |> dump
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
            let node = identNode :> Node

            // We try and grab the first comment from the generic Node interface.
            let firstComment =
                match Seq.tryHead node.ContentBefore with
                | None -> sepNone
                | Some triviaNode ->
                    // If found we check the content and try to print the comment text followed by a newline
                    match triviaNode.Content with
                    | CommentOnSingleLine comment -> !-comment +> sepNln
                    | _ -> !- "error"

            firstComment +> !-identNode.Text
        | _ -> !- "error"

    let codeWithTriviaPrinting = f genExprWithTrivia tree ctx |> dump
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
    let zeroRange = FSharp.Compiler.Text.Range.Zero
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
            zeroRange
        )

    let tree =
        Oak(
            [],
            [ ModuleOrNamespaceNode(
                  None,
                  [ ModuleDecl.TopLevelBinding(mkBinding "a" "1")
                    ModuleDecl.TopLevelBinding(mkBinding "b" "2") ],
                  zeroRange
              ) ],
            zeroRange
        )

    let enterNode (node: Node) =
        col sepNln node.ContentBefore (fun (tn: TriviaNode) ->
            match tn.Content with
            | Newline -> sepNln
            | _ -> sepNone)

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
                +> !- "let"
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

        | _ -> !- "error"

    let ctx =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF } }

    let formattedCode = f tree ctx |> dump
    Assert.AreEqual("let a = 1\n\nlet b = 2", formattedCode)

    // This worked fine, but the next time we will format there will be a TriviaNode for the blank line.
    // The newly found newline will be added to the last top level binding node
    match tree.ModulesOrNamespaces.[0].Declarations.[1] with
    | ModuleDecl.TopLevelBinding binding -> (binding :> Node).AddBefore(TriviaNode(TriviaContent.Newline, zeroRange))
    | _ -> ()

    let formattedCodeWithTrivia = f tree ctx |> dump
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
                +> !- "let"
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

        | _ -> !- "error"

    let finalCode = g tree ctx |> dump
    Assert.AreEqual("let a = 1\n\nlet b = 2", finalCode)

[<Test>]
let ``locking the indentation at a fixed column`` () =
    // In some scenarios we need to keep code indented at a fixed column.
    // This is typically to produce valid F# due to the offset rules.
    let f =
        sepOpenT
        +> atCurrentColumn (!- "first line" +> sepNln +> !- "second line")
        +> sepCloseT

    let ctxBefore =
        { Context.Default with
            Config =
                { Context.Default.Config with
                    EndOfLine = EndOfLineStyle.LF } }

    let ctxAfter = f ctxBefore
    let code = dump ctxAfter
    // `atCurrentColumn` will lock the column at position one
    // Notice the space before the "second line", it is there because of the line will start at column 1.
    Assert.AreEqual("(first line\n second line)", code)

    let events = (Queue.toSeq >> Seq.toList) ctxAfter.WriterEvents

    match events with
    | [ WriterEvent.Write "("
        WriterEvent.SetAtColumn 1
        WriterEvent.Write "first line"
        WriterEvent.WriteLine
        WriterEvent.Write "second line"
        WriterEvent.RestoreAtColumn 0
        WriterEvent.RestoreIndent 0
        WriterEvent.Write ")" ] -> Assert.Pass()
    | events -> Assert.Fail $"Expected one event, got: ${events}"

// There is also a variation of `atCurrentColumn`: `atCurrentColumnIndent`
// This locks the column and also applies indentation from that column.
// `atCurrentColumn` does not have an influence over the indentation.

// In general, you want to avoid using `atCurrentColumn` and `atCurrentColumnIndent` as it breaks the "indentation flow".
// "indentation flow" is a made up term to indicate that every indent is a multitude of the `indent_size`.
