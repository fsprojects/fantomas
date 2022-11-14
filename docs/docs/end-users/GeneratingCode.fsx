(**
---
category: End-users
categoryindex: 1
index: 9
---
*)
(**
# Generating source code

The [Fantomas.Core](https://www.nuget.org/packages/Fantomas.Core) NuGet package can also be used to format code programmatically.   
The public API is available from the static `CodeFormatter` class. It exposes a couple of APIs to format code, one being to format code from a raw syntax tree.  
This API assumes the user already parsed a syntax tree or constructed an artificial one.

## Key motivation

It can be very tempting to generate some F# code by doing some string concatenations.  
In simple scenarios this can work out, but in the long run it doesn't scale well:

- The more code constructs you want to support, the more conditional logic you will need to ensure all edge cases.
- A string is just a string, you cannot guarantee the output will be valid code.
- It is easier to map your domain model to untyped syntax tree nodes and let Fantomas take care of the actual generation of code.

**For mercy's sake don't use string concatenation when generating F# code, use Fantomas instead. It is battle tested and proven technology!**
*)

(**
## Generating source code from scratch

### Example syntax tree

To illustrate the API, lets generate a simple value binding: `let a = 0`.
*)

#r "nuget: Fantomas.Core, 5.*" // Note that this will also load Fantomas.FCS, which contains the syntax tree types.

open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let implementationSyntaxTree =
    ParsedInput.ImplFile(
        ParsedImplFileInput(
            "filename.fsx",
            true,
            QualifiedNameOfFile(Ident("", Range.Zero)),
            [],
            [],
            [ SynModuleOrNamespace(
                  [],
                  false,
                  SynModuleOrNamespaceKind.AnonModule,
                  [ SynModuleDecl.Let(
                        false,
                        [ SynBinding(
                              None,
                              SynBindingKind.Normal,
                              false,
                              false,
                              [],
                              PreXmlDoc.Empty,
                              SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
                              SynPat.Named(SynIdent(Ident("a", Range.Zero), None), false, None, Range.Zero),
                              None,
                              SynExpr.Const(SynConst.Int32(0), Range.Zero),
                              Range.Zero,
                              DebugPointAtBinding.Yes Range.Zero,
                              { EqualsRange = Some Range.Zero
                                LeadingKeyword = SynLeadingKeyword.Let Range.Zero }
                          ) ],
                        Range.Zero
                    ) ],
                  PreXmlDoc.Empty,
                  [],
                  None,
                  Range.Zero,
                  { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
              ) ],
            (false, false),
            { ConditionalDirectives = []
              CodeComments = [] }
        )
    )

open Fantomas.Core

CodeFormatter.FormatASTAsync(implementationSyntaxTree) |> Async.RunSynchronously
(*** include-it ***)

(**
Constructing the entire syntax tree can be a bit overwhelming at first. There is a lot of information to provide and a lot to unpack if you have never seen any of this before.

Let's deconstruct a couple of things:

- Every file has one or more [SynModuleOrNamespace](../../reference/fsharp-compiler-syntax-synmoduleornamespace.html). In this case the module was anonymous and thus invisible.
- Every `SynModuleOrNamespace` has top level [SynModuleDecl](../../https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-synmoduledecl.html).
- [SynModuleDecl.Let](../../https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-synmoduledecl.html#Let) takes one or more [SynBinding](../../reference/fsharp-compiler-syntax-synbinding.html).  

  You would have multiple bindings in case of a recursive function.  
- The `headPat` of binding contains the name and the parameters. 
- The `expr` ([SynExpr](../../reference/fsharp-compiler-syntax-synexpr.html)) represents the F# syntax expression.
- Because there is no actual source code, all ranges will be `Range.Zero`.  

The more you interact with AST, the easier you pick up which node represents what.

### Fantomas.FCS

When looking at the example, we notice that we've opened a couple of `FSharp.Compiler.*` namespaces.   
Don't be fooled by this, `Fantomas.Core` and `Fantomas.FCS` **do not reference [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)**!  
Instead, `Fantomas.FCS` is a custom version of the F# compiler (built from source) that only exposes the F# parser and the syntax tree.

`Fantomas.FCS` exposes the exact same namespaces because it builds from the exact same F# compiler source code.   
The key difference is that `Fantomas.FCS` will most likely contain a more recent version of the F# parser.  
You can read the [CHANGELOG](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md) to see what git commit was used to build `Fantomas.FCS`.

You can use `Fantomas.FCS` in your own projects, but be aware that it is **not binary compatible** with `FSharp.Compiler.Service`.  
Example usage:

*)

#r "nuget: Fantomas.FCS"

open FSharp.Compiler.Text
open Fantomas.FCS

Parse.parseFile false (SourceText.ofString "let a = 1") []
(*** include-it ***)

(**
## Tips and tricks

### Online tool

The syntax tree can have an overwhelming type hierarchy.  
We wholeheartedly recommend to use our **[online tool](https://fsprojects.github.io/fantomas-tools/#/ast)** when working with AST.

![F# AST Viewer](../../images/ast-viewer.png)

This shows you what AST nodes the parser created for a given input text.  
From there on you can use our search bar to find the corresponding documentation:

![Search bar](../../images/searchbar-ast.png)

### Match the AST the parser would produce

Fantomas will very selectively use information from the AST.  
Please make sure you construct the same AST as the parser would.
*)

// You typically make some helper functions along the way
let mkCodeFromExpression (e: SynExpr) : string =
    ParsedInput.ImplFile(
        ParsedImplFileInput(
            "filename.fsx",
            true,
            QualifiedNameOfFile(Ident("", Range.Zero)),
            [],
            [],
            [ SynModuleOrNamespace(
                  [],
                  false,
                  SynModuleOrNamespaceKind.AnonModule,
                  [ SynModuleDecl.Expr(e, Range.Zero) ],
                  PreXmlDoc.Empty,
                  [],
                  None,
                  Range.Zero,
                  { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
              ) ],
            (false, false),
            { ConditionalDirectives = []
              CodeComments = [] }
        )
    )
    |> CodeFormatter.FormatASTAsync
    |> Async.RunSynchronously

let numberExpr = SynExpr.Const(SynConst.Int32(7), Range.Zero)
let wrappedNumber = SynExpr.Paren(numberExpr, Range.Zero, None, Range.Zero)
mkCodeFromExpression wrappedNumber
(*** include-it ***)

(**
Notice that last but one argument `None`, it represents the range of the closing `)`.  
The F# parser would include `Some range` when it parses code, so you need to provide a `Some range` value as well.  
Even though the range is empty. Fantomas is designed to work with AST created by the parser.  
Creating a `SynExpr.Paren` node is not enough to get both parentheses!  
The `CodeFormatter.FormatASTAsync` API is really a side-effect and not a first class citizen.  
It will work when you play ball with the exact shape of the parser.
*)

let betterWrappedNumber =
    SynExpr.Paren(numberExpr, Range.Zero, Some Range.Zero, Range.Zero)

mkCodeFromExpression betterWrappedNumber
(*** include-it ***)

(**
As a rule of thumb: **create what the parser creates, use the online tool!**  
Just because you can create AST nodes, does not mean Fantomas will do the right thing.

### Look at the Fantomas code base

As mentioned, not every AST node is being used in Fantomas. There are numerous things that do not have any influence on the generation of code.  
For example creating [SynExpr.Lambda](../../reference/fsharp-compiler-syntax-synexpr.html#Lambda).

When you want to construct `fun a b -> a + b`, the AST the online tool produces looks like:
```fsharp
Lambda
  (false, false,
    SimplePats
    ([Id (a, None, false, false, false, tmp.fsx (1,4--1,5))],
    tmp.fsx (1,4--1,5)),
    Lambda
    (false, true,
    SimplePats
        ([Id (b, None, false, false, false, tmp.fsx (1,6--1,7))],
        tmp.fsx (1,6--1,7)),
    App
        (NonAtomic, false,
        App
            (NonAtomic, true,
            LongIdent
            (false,
                SynLongIdent
                ([op_Addition], [], [Some (OriginalNotation "+")]),
                None, tmp.fsx (1,13--1,14)), Ident a,
            tmp.fsx (1,11--1,14)), Ident b, tmp.fsx (1,11--1,16)),
    None, tmp.fsx (1,0--1,16),
    { ArrowRange = Some tmp.fsx (1,8--1,10) }),
    Some
    ([Named (SynIdent (a, None), false, None, tmp.fsx (1,4--1,5));
        Named (SynIdent (b, None), false, None, tmp.fsx (1,6--1,7))],
    App
        (NonAtomic, false,
        App
            (NonAtomic, true,
            LongIdent
            (false,
                SynLongIdent
                ([op_Addition], [], [Some (OriginalNotation "+")]),
                None, tmp.fsx (1,13--1,14)), Ident a,
            tmp.fsx (1,11--1,14)), Ident b, tmp.fsx (1,11--1,16))),
    tmp.fsx (1,0--1,16), { ArrowRange = Some tmp.fsx (1,8--1,10) })
```

but the Fantomas `CodePrinter` does not use all this data.
We can easily create a `Lambda` without the nested body structure, as Fantomas will use the `parsedData` information.
*)
// this dummy expr will never be used!
let dummyExpr = SynExpr.Const(SynConst.Unit, Range.Zero)

let lambdaExpr =
    let args =
        [ SynPat.Named(SynIdent(Ident("a", Range.Zero), None), false, None, Range.Zero)
          SynPat.Named(SynIdent(Ident("b", Range.Zero), None), false, None, Range.Zero) ]

    let expr =
        SynExpr.App(
            ExprAtomicFlag.NonAtomic,
            false,
            SynExpr.App(
                ExprAtomicFlag.NonAtomic,
                true,
                SynExpr.LongIdent(
                    false,
                    SynLongIdent(
                        [ Ident("_actually_not_used_", Range.Zero) ],
                        [],
                        [ Some(IdentTrivia.OriginalNotation("+")) ]
                    ),
                    None,
                    Range.Zero

                ),
                SynExpr.Ident(Ident("a", Range.Zero)),
                Range.Zero
            ),
            SynExpr.Ident(Ident("b", Range.Zero)),
            Range.Zero
        )

    SynExpr.Lambda(
        false,
        false,
        SynSimplePats.SimplePats([], Range.Zero), // not used
        dummyExpr, // not used
        Some(args, expr), // The good stuff is in here!
        Range.Zero,
        { ArrowRange = Some Range.Zero }
    )

mkCodeFromExpression lambdaExpr
(*** include-it ***)

(**
Notice how minimal the AST is, versus to what the parser produced. A subset of the data was enough.  
How to know which nodes to include? Take a look at `CodePrinter.fs` and `SourceParser.fs`!

### Create your own set of helper functions

Throughout all these examples, we have duplicated a lot of code. You can typically easily refactor this into some helper functions.  
The Fantomas maintainers are not affiliated with any projects that expose AST construction helpers.  
Relying on these projects, is at your own risk. The constructed AST might not be suitable for what Fantomas expects.

### Updates

Since code generation is considered to be a nice to have functionality, there is no compatibility between any `Fantomas.FCS`.  
We do not apply any semantic versioning to `Fantomas.FCS`. Breaking changes can be expected at any given point.

<fantomas-nav previous="./VSCode.html" next="./FAQ.html"></fantomas-nav>
*)
