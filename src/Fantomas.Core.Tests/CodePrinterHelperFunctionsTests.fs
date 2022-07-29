module Fantomas.Core.Tests.CodePrinterHelperFunctionsTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Context
open Fantomas.Core.FormatConfig
open Fantomas.Core

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
