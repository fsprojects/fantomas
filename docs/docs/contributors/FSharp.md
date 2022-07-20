---
category: Contributors
categoryindex: 2
index: 2
---
# F#

## New to F#?

If you are truly brand-new to the F# language, you might want to start by reading the [F# documentation of Microsoft](https://dotnet.microsoft.com/en-us/languages/fsharp).  
Some other great resources (in no particular order) are:

- [Essential F#](https://leanpub.com/essential-fsharp)
- [F# for Fun and Profit](https://fsharpforfunandprofit.com/)
- [F# Fundamentals Tutorial | Learn Functional Programming | Step-by-Step Guide](https://www.youtube.com/watch?v=SvOInBxPL30)
- [F# Foundation Slack](https://fsharp.org/guides/slack/)
- [F# on Discord](https://discord.com/invite/R6n7c54)

## Used F# features

F# has a lot of nice language features, although not all of them are used in Fantomas.
We wish to highlight the most important ones that we use before continuing:

- [Partial active pattern](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns#partial-active-patterns), these heavily used in `SourceParser.fs`.
  In short, we use the `Untyped Abstract Syntax Tree` created by the F# parser, we don't use all the information in that tree to restore the source code.

For example [SynExpr.For](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#For), the definition looks like:

```fsharp
type SynExpr =
    ...
    /// F# syntax: 'for i = ... to ... do ...'
    | For of
        forDebugPoint: DebugPointAtFor *
        toDebugPoint: DebugPointAtInOrTo *
        ident: Ident *
        equalsRange: range option *
        identBody: SynExpr *
        direction: bool *
        toBody: SynExpr *
        doBody: SynExpr *
        range: range
```

  However, in Fantomas we have a partial active pattern that we use to easily grab the information we need from the AST.
```fsharp
let (|For|_|) =
    function
    | SynExpr.For (_, _, ident, equalsRange, e1, isUp, e2, e3, _) -> Some(ident, equalsRange, e1, e2, e3, isUp)
    | _ -> None  
```

Notice the underscores, we don't use the `DebugPoint` info and `range`, so we drop that information in the result of the partial active pattern.
We then later use this pattern to work with the AST we need.
Example usage in `CodePrinter.fs`:

```fsharp
| For (ident, equalsRange, e1, e2, e3, isUp) ->
    atCurrentColumn (
        !- "for "
        +> genIdent ident
        +> genEq SynExpr_For_Equals equalsRange
        +> sepSpace
        +> genExpr astContext e1
        +> ifElse isUp (!- " to ") (!- " downto ")
        +> genExpr astContext e2
        +> !- " do"
        +> indent
        +> sepNln
        +> genExpr astContext e3
        +> unindent
    )
```

- [Custom operators](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading#creating-new-operators). In F# there are some special operators like [|>](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-operators.html#(|%3E)) and [>>](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-operators.html#(%3E%3E)).  

Note that these are just function themselves as well. Instead of specifying all the arguments after the function name, (infix) operators let you specify an argument before the operator and after. 

In F#, you are able to create your own operators as well. In Fantomas, the most notable are `!-` and `+>`. We will cover them later, but if you peek in `CodePrinter.fs`, they are heavily used there.

- [Signature files](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/signature-files).

In Fantomas we use signature files to define the module boundaries. Everything that is both defined in the implementation file (the `*.fs` file) and in the signature file (the `*.fsi` file) is considered to be visible to other modules.
If a signature file is present, there is no need to specify `private` in a function you don't want to be visible to other modules. Just don't add `val` entry to the signature file and it will be private automatically.
You can look at a signature file to get a glimpse of what the module really does.

- [Type extensions](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/type-extensions).

In contrast to partial active patterns, where we want to hide some AST information, it can occur that we need to extend the type of an AST node.
We do this by adding a new type member to an existing Syntax tree type.
Example in `AstExtensions.fs`:

```fsharp
type SynMemberFlags with

    member memberFlags.FullRange: range option =
        RangeHelpers.mergeRanges
            [ yield! Option.toList memberFlags.Trivia.AbstractRange
              yield! Option.toList memberFlags.Trivia.DefaultRange
              yield! Option.toList memberFlags.Trivia.MemberRange
              yield! Option.toList memberFlags.Trivia.OverrideRange
              yield! Option.toList memberFlags.Trivia.StaticRange ]
```

The type `SynMemberFlags` does not expose any range information, but we can extend it to do so.  
The `.FullRange` naming convention is used to indicate that we are not satisfied by the original range or it is lacking all together.  
Don't worry just yet about this implementation, so keep in mind that with this feature we can later use `memberFlags.FullRange` on a `SynMemberFlags` instance.

- [Function Values](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#function-values)

This is well-known concept in F# and for completion sake we to mention this. In F#, you can pass a function as an argument to another function.
Fantomas is full of this kind of functions, so be sure to grasp this concept before continuing.

- [Tail recursion](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword#tail-recursion)

There are places in the code base where we use some more advanced recursion techniques. `AstTransformer.fs` is one of them.  
A very good explanation of what happens here can be found in this [blogpost](https://www.gresearch.co.uk/blog/article/advanced-recursion-techniques-in-f/).

- [Event Sourcing](https://medium.com/@dzoukr/event-sourcing-step-by-step-in-f-be808aa0ca18)

We use event sourcing to capture the instructions on how to write the new code. Instead of writing the new code directly to for example a `StringBuilder`, we write it to a list of events.  
That list of events will contain instructions like `Write "let"`, `IndentBy 4`, `WriteLine` etc. So it is useful to have some notion of event sourcing.

Although, it really is an implementation detail in `Context.fs`, think of it as writing a letter with a pen and a paper.
We first rehearse what we want to say, then we write the letter. Not write evey word as we are making up the letter, but write the letter as a whole once we know the content.  
These events are used to achieve this.
