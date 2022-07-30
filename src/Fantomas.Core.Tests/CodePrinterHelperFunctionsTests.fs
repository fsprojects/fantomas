module Fantomas.Core.Tests.CodePrinterHelperFunctionsTests

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open NUnit.Framework
open FsUnit
open Fantomas.Core.Context
open Fantomas.Core.FormatConfig
open Fantomas.Core
open Fantomas.Core.TriviaTypes

// This test suite is created to illustrated the various helper function that are being used in `CodePrinter`.
// We encourage you to debug these when you are new to the code base.
// It might help for some things to "click"

/// Transform the WriterEvents in a Context to a string
let private dump (context: Context) : string = dump false context

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

    // Wait! Why is the only one space between `a` and `b`?
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
        { Context.Default with Config = { Context.Default.Config with SpaceBeforeColon = true } }

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
        { Context.Default with Config = { Context.Default.Config with EndOfLine = EndOfLineStyle.LF } }

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
    // As established in the documentation, `TriviaInstructions` can be added to the `Context`.
    // In `CodePrinter` these need to printed at the right time.
    // A `TriviaInstruction` always has an `FsAstType` and a `range` value.
    // This should always correspond to the range of the AST node that is being printed.
    let sourceCode =
        """let a =
    // code comment
    b"""

    // Parse the source code to an AST.
    let ast =
        CodeFormatter.ParseAsync(false, sourceCode)
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    // Dummy active pattern, this mimics what we typically do in SourceParser.
    // This active pattern will return the `a` node and the `b` node.
    let (|InterestingTreeNodes|_|) (ast: ParsedInput) =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(modules = [ SynModuleOrNamespace.SynModuleOrNamespace(decls = [ SynModuleDecl.Let(bindings = [ SynBinding (headPat = SynPat.Named(ident = SynIdent (aNode,
                                                                                                                                                                                                                            _))
                                                                                                                                                                                   expr = SynExpr.Ident bNode) ]) ]) ])) ->
            Some(aNode, bNode)
        | _ -> None

    // Another active pattern to extract the code comment.
    // This represents what is happening in `Trivia`.
    // Grabbing all comments from the toplevel file node and to later assign them to the correct AST child node.
    let (|SingleComment|_|) (ast: ParsedInput) =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(trivia = { CodeComments = [ CommentTrivia.LineComment rangeOfLineComment ] })) ->
            Some rangeOfLineComment
        | _ -> None

    let f (ast: ParsedInput) : Context -> Context =
        match ast with
        | InterestingTreeNodes (a, b) -> !- "let " +> !-a.idText +> sepEq +> sepSpace +> !-b.idText
        | _ -> !- "error"

    let codeCommentAsTriviaInstruction: TriviaInstruction list =
        match ast with
        | SingleComment comment & InterestingTreeNodes (_, b) ->
            // In `Trivia` we figured out that the comment belongs to `b`.
            // Now will map this as `TriviaInstruction`.
            let trivia: Trivia =
                { Range = comment
                  Item = TriviaContent.Comment(Comment.CommentOnSingleLine "// code comment") }

            let instruction: TriviaInstruction =
                { Trivia = trivia
                  Type = Ident_
                  Range = b.idRange
                  AddBefore = true }

            [ instruction ]
        | _ -> []

    // Add the trivia instructions to the context.
    // We optimize this a bit according to trivia before/after and the FsAstType.
    let ctx =
        { Context.Default with
            Config = { Context.Default.Config with EndOfLine = EndOfLineStyle.LF }
            TriviaBefore = Map.ofList [ FsAstType.Ident_, codeCommentAsTriviaInstruction ] }

    let codeWithoutTriviaPrinting = f ast ctx |> dump
    Assert.AreEqual("let a = b", codeWithoutTriviaPrinting)

    // We need to write a better function, where the code comment will be restored
    // `genTriviaFor` is typically the go-to function to write a trivia instruction.
    // As it not exposed from `CodePrinter`, we need to write our own.
    let g (ast: ParsedInput) : Context -> Context =
        match ast with
        | InterestingTreeNodes (a, b) ->
            let genB: Context -> Context =
                // This will write any instructions that match 'FsAstType.Ident_'
                // and the range of `b` equals the range of the `TriviaInstruction`.
                enterNodeFor Ident_ b.idRange +> !-b.idText
            // We could also add `+> leaveNodeFor` here, but again we know in this example there is only one instruction before.

            // Since we know in this example that the body of the let binding is multiline,
            // we are going to use `indentSepNlnUnindent`.
            // In practise you cannot make this assumption and we should try both short and long paths.
            // As an example this is fine.
            !- "let " +> !-a.idText +> sepEq +> indentSepNlnUnindent genB
        | _ -> !- "error"

    // Please take a look at `genTriviaFor` to see why we used this function.

    let codeWithTriviaPrinting = g ast ctx |> dump
    Assert.AreEqual("let a =\n    // code comment\n    b", codeWithTriviaPrinting)

[<Test>]
let ``blank lines trivia`` () =
    // Blank lines are also printed as trivia.
    // However, in some situations they can clash with a composition that always adds a new line.

    let cleanInput =
        """let a = 1
let b = 2
"""

    // Imagine that we always want to print a new line between let bindings.

    // Parse the source code to an AST.
    let getAst input =
        CodeFormatter.ParseAsync(false, input)
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    let ast = getAst cleanInput

    let (|TwoBindingsInFile|_|) (ast: ParsedInput) =
        let (|ValueBinding|_|) (node: SynModuleDecl) =
            match node with
            | SynModuleDecl.Let(bindings = [ SynBinding (headPat = SynPat.Named(ident = SynIdent (name, _))
                                                         expr = SynExpr.Const (SynConst.Int32 value, _)) ]) ->
                Some(name.idText, value, node.Range)
            | _ -> None

        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(modules = [ SynModuleOrNamespace.SynModuleOrNamespace(decls = [ ValueBinding a
                                                                                                                                        ValueBinding b ]) ])) ->
            Some(a, b)
        | _ -> None

    let f ast =
        match ast with
        | TwoBindingsInFile (a, b) ->
            let genBinding (name, value, range) =
                // print trivia before SynModuleDecl.Let
                enterNodeFor SynModuleDecl_Let range
                +> !- "let"
                +> sepSpace
                +> !-name
                +> sepEq
                +> sepSpace
                +> !-(string value)

            genBinding a
            // One `sepNln` to move to the next line.
            +> sepNln
            // Another to insert a complete blank line.
            +> sepNln
            +> genBinding b

        | _ -> !- "error"

    let ctx =
        { Context.Default with Config = { Context.Default.Config with EndOfLine = EndOfLineStyle.LF } }

    let formattedCode = f ast ctx |> dump
    Assert.AreEqual("let a = 1\n\nlet b = 2", formattedCode)

    // This worked fine, but the next time we will format there will be a TriviaInstruction for the blank line.
    let newAst = getAst formattedCode

    let triviaInstructions =
        match newAst with
        | TwoBindingsInFile (_, (_, _, rangeOfB)) ->
            // Again, this trivia would be detected in `Trivia`.
            // We simply things for this example.
            let rangeOfNewline = CodeFormatter.MakeRange(rangeOfB.FileName, 2, 0, 2, 0)

            let trivia =
                { Range = rangeOfNewline
                  Item = TriviaContent.Newline }

            let triviaInstruction =
                { Trivia = trivia
                  Type = SynModuleDecl_Let
                  Range = rangeOfB
                  AddBefore = true }

            [ triviaInstruction ]
        | _ -> []

    let ctxWithTrivia =
        { Context.Default with
            Config = { Context.Default.Config with EndOfLine = EndOfLineStyle.LF }
            TriviaBefore = Map.ofList [ SynModuleDecl_Let, triviaInstructions ] }

    let formattedCodeWithTrivia = f newAst ctxWithTrivia |> dump
    // Notice that we now have two blank lines.
    // One from the trivia, and one from the fixed sepNln inside `f`.
    Assert.AreEqual("let a = 1\n\n\nlet b = 2", formattedCodeWithTrivia)

    // The next time we ran this code (assuming all the trivia instructions are provided properly), we would get three newlines.
    // So, this is becoming a repeating newline bug.

    let g ast =
        match ast with
        | TwoBindingsInFile (a, b) ->
            let genBinding (name, value, range) =
                // print trivia before SynModuleDecl.Let
                enterNodeFor SynModuleDecl_Let range
                +> !- "let"
                +> sepSpace
                +> !-name
                +> sepEq
                +> sepSpace
                +> !-(string value)

            // We can resolve this problem by using the helper function `sepNlnConsideringTriviaContentBeforeFor`.
            // This will add a newline unless there is already a new line following as part of the trivia.
            let _, _, rangeOfB = b

            genBinding a
            // A regular `sepNln` to go to the next line.
            +> sepNln
            // Only insert a full blank line if there is no newline following the trivia.
            +> sepNlnConsideringTriviaContentBeforeFor SynModuleDecl_Let rangeOfB
            +> genBinding b

        | _ -> !- "error"

    let finalCode = g ast ctxWithTrivia |> dump
    Assert.AreEqual("let a = 1\n\nlet b = 2", finalCode)

[<Test>]
let ``locking the indentation at a fixed column`` () =
    // In some scenario's we need to keep code indented at a fixed column.
    // This is typically to produce valid F# due to the offset rules.
    let f =
        sepOpenT
        +> atCurrentColumn (!- "first line" +> sepNln +> !- "second line")
        +> sepCloseT

    let ctxBefore =
        { Context.Default with Config = { Context.Default.Config with EndOfLine = EndOfLineStyle.LF } }

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

// There is also a variation on `atCurrentColumn`: `atCurrentColumnIndent`
// This locks the column and also applies indentation from that column.
// `atCurrentColumn` does not have an influence over the indentation.

// In general, you want to avoid using `atCurrentColumn` and `atCurrentColumnIndent` as it breaking the "indentation flow".
// "indentation flow" is a made up term to indicate that every indent is a multitude of the `indent_size`.
