---
category: Contributors
categoryindex: 2
index: 6
---
# Fantomas.Core overview (2)

Once the `Context` is constructed, we can traverse the syntax tree to captures all the `WriterEvent`s.

<div class="mermaid text-center">
graph TD
    A[Prepare Context] --> B
    B[Print AST with Context] --> C[Formatted code]
    style B stroke:#338CBB,stroke-width:2px
 </div>

We enter the module of `CodePrinter` and try and reconstruct the code based on the given configuration.

## WriterEvents and WriterModel

In previous versions of Fantomas, `Context` had a [TextWriter](https://docs.microsoft.com/en-us/dotnet/api/system.io.textwriter?view=net-6.0) that was used to write the output.
This is a more advanced version of a `StringBuilder` and we wrote the formatted code directly to the buffer.

The key problem with this approach that we couldn't easily revert code that was written to the buffer.  
For example is the formatted code was crossing the `max_line_length`, we couldn't easily revert the code and try an alternative.

That is why we first capture a collection of `WriterEvent`s and then reconstruct the formatted code.  
If the code is too long, we can drop the last events and try an alternative.

### WriterModel

When we capture new events we also want to capture the current state of the result.  
This happens in the `WriterModel` record. There we store the result of each event as if it were finalized.  
By doing this, we can assert the result of the output. For example if our code is too long or not.

`WriterEvents` and `WriterModel` are very stable in the code base.  
When solving a bug, you typically need to change the collected series of events by using different helper function inside `CodePrinter`.

### CodePrinter

In `CodePrinter` the syntax tree is being traversed with helper of various (partial) active patterns.  
These active patterns are defined in `SourceParser` and typically are used to present the information we are interested in, in a different shape.

`CodePrinter` exposes one function `genParsedInput`.

```fsharp
val genParsedInput:
    astContext: ASTContext -> ast: FSharp.Compiler.Syntax.ParsedInput -> (Context.Context -> Context.Context)
```

This takes an `ASTContext` and the syntax tree.  
It returns a function that takes a `Context` and returns a new `Context`.

We will eventually call this function with our previously constructed `Context`.  
In this function, all events are captured and stored in the `WriterEvents` and `WriterModel`.

While we are traversing the syntax tree, we will compose the `Context -> Context` function based on the content.
`Context.dump` then eventually takes the `Context` and returns a `string` of formatted code.

This may seem a bit complicated, but you typically can see this as an implementation detail and can abstract this when working in `CodePrinter`.

### Creating WriterEvents

There are various helper functions in `CodePrinter` that create `WriterEvent`s.  
In `CodePrinter` we will typically never construct a `WriterEvent` directly.

Instead we can use various helper functions that take the `Context` as parameter and return an updated `Context` with additional events.  
Please take a moment to debug the unit tests in `CodePrinterHelperFunctionsTests.fs`.  
This will give you a better understanding of how we capture events in `CodePrinter`.

<div class="d-flex justify-content-between my-4">
  <a href="./Prepare%20Context.html">Previous</a>
  <a href="./Formatted+Code.html">Next</a>
</div>