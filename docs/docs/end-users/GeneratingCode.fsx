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

#r "../../../src/Fantomas/bin/Release/net6.0/Fantomas.FCS.dll"
#r "../../../src/Fantomas/bin/Release/net6.0/Fantomas.Core.dll" // In production use #r "nuget: Fantomas.Core, 6.0-alpha-*"

open FSharp.Compiler.Text
open Fantomas.Core.ImmutableArray
open Fantomas.Core.SyntaxOak

let implementationSyntaxTree =
    Oak(
        ImmutableArray.empty,
        immarray 1 {
            ModuleOrNamespaceNode(
                None,
                immarray 1 {
                    BindingNode(
                        None,
                        None,
                        MultipleTextsNode(immarray 1 { SingleTextNode("let", Range.Zero) }, Range.Zero),
                        false,
                        None,
                        None,
                        Choice1Of2(
                            IdentListNode(
                                immarray 1 { IdentifierOrDot.Ident(SingleTextNode("a", Range.Zero)) },
                                Range.Zero
                            )
                        ),
                        None,
                        ImmutableArray.empty,
                        None,
                        SingleTextNode("=", Range.Zero),
                        Expr.Constant(Constant.FromText(SingleTextNode("0", Range.Zero))),
                        Range.Zero
                    )
                    |> ModuleDecl.TopLevelBinding
                },
                Range.Zero
            )
        },
        Range.Zero
    )

open Fantomas.Core

CodeFormatter.FormatOakAsync(implementationSyntaxTree)
|> Async.RunSynchronously
|> printfn "%s"
(*** include-output  ***)

(**
Constructing the entire syntax tree can be a bit overwhelming at first. There is a lot of information to provide and a lot to unpack if you have never seen any of this before.

Let's deconstruct a couple of things:

- Every file has one or more [ModuleOrNamespaceNode](../../reference/fantomas-core-syntaxoak-moduleornamespacenode.html). In this case the module was anonymous and thus invisible.
- Every `ModuleOrNamespaceNode` has top level [ModuleDecl](../../reference/fantomas-core-syntaxoak-moduledecl.html).
- [ModuleDecl.TopLevelBinding](../../https://fsprojects.github.io/fantomas/reference/fantomas-core-syntaxoak-moduledecl.html#TopLevelBinding) takes a [BindingNode ](../../reference/fantomas-core-syntaxoak-bindingnode.html).  

- The `functionName ` of binding contains the name or is a pattern. 
- The `expr` ([Expr](../../reference/fantomas-core-syntaxoak-expr.html)) represents the F# syntax expression.
- Because there is no actual source code, all ranges will be `Range.Zero`.  

The more you interact with AST/Oak, the easier you pick up which node represents what.

### Fantomas.FCS

When looking at the example, we notice that we've opened `FSharp.Compiler.Text`.   
Don't be fooled by this, `Fantomas.Core` and `Fantomas.FCS` **do not reference [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)**!  
Instead, `Fantomas.FCS` is a custom version of the F# compiler (built from source) that only exposes the F# parser and the syntax tree.

`Fantomas.FCS` exposes the exact same namespaces because it builds from the exact same F# compiler source code.   
The key difference is that `Fantomas.FCS` will most likely contain a more recent version of the F# parser.  
You can read the [CHANGELOG](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md) to see what git commit was used to build `Fantomas.FCS`.

You can use `Fantomas.FCS` in your own projects, but be aware that it is **not binary compatible** with `FSharp.Compiler.Service`.  
Example usage:

*)

open Fantomas.FCS

Parse.parseFile false (SourceText.ofString "let a = 1") []
(*** include-it ***)

(**
You can format untyped AST created from `Fantomas.FCS` using the `CodeFormatter` API.  
However, we recommend to use the new `Oak` model (as in the example) instead.  
The `Oak` model is easier to reason with as it structures certain concepts differently than the untyped AST.
*)

(**
## Tips and tricks

### Online tool

The syntax tree can have an overwhelming type hierarchy.  
We wholeheartedly recommend to use our **[online tool](https://fsprojects.github.io/fantomas-tools/#/ast)** when working with AST.

![F# AST Viewer](../../images/oak-viewer.png)

This shows you what Oak nodes the parser created for a given input text.  
From there on you can use our search bar to find the corresponding documentation:

![Search bar](../../images/searchbar-ast.png)

### Match the AST the parser would produce

Fantomas will very selectively use information from the AST to construct the Oak.
Please make sure you construct the same Oak as Fantomas would.
*)

// You typically make some helper functions along the way
let text v = SingleTextNode(v, Range.Zero)

let mkCodeFromExpression (e: Expr) =
    Oak(
        ImmutableArray.empty,
        immarray 1 { ModuleOrNamespaceNode(None, immarray 1 { ModuleDecl.DeclExpr e }, Range.Zero) },
        Range.Zero
    )
    |> CodeFormatter.FormatOakAsync
    |> Async.RunSynchronously
    |> printfn "%s"

let numberExpr = Expr.Constant(Constant.FromText(text "7"))

let wrappedNumber =
    Expr.Paren(ExprParenNode(text "(", numberExpr, text ")", Range.Zero))

mkCodeFromExpression wrappedNumber
(*** include-output  ***)

(**
As a rule of thumb: **create what the parser creates, use the online tool!**  
Just because you can create Oak nodes, does not mean Fantomas will do the right thing.

### Look at the Fantomas code base

As mentioned, not every AST node is being used in Fantomas. There are numerous things that do not have any influence on the generation of code.  
For example creating [SynExpr.Lambda](../../reference/fsharp-compiler-syntax-synexpr.html#Lambda).

When you want to construct `fun a b -> a + b`, the AST the online tool produces looks like:
```fsharp
Oak (1,0-1,16)
  ModuleOrNamespaceNode (1,0-1,16)
    ExprLambdaNode (1,0-1,16)
      "fun" (1,0-1,3)
      PatNamedNode (1,4-1,5)
        "a" (1,4-1,5)
      PatNamedNode (1,6-1,7)
        "b" (1,6-1,7)
      "->" (1,8-1,10)
      ExprInfixAppNode (1,11-1,16)
        "a" (1,11-1,12)
        "+" (1,13-1,14)
        "b" (1,15-1,16)
```

*)

let lambdaExpr =
    let body: Expr =
        ExprInfixAppNode(Expr.Ident(text "a"), text "+", Expr.Ident(text "b"), Range.Zero)
        |> Expr.InfixApp

    ExprLambdaNode(
        text "fun",
        immarray 2 {
            Pattern.Named(PatNamedNode(None, text "a", Range.Zero))
            Pattern.Named(PatNamedNode(None, text "b", Range.Zero))
        },
        text "->",
        body,
        Range.Zero
    )
    |> Expr.Lambda

mkCodeFromExpression lambdaExpr
(*** include-output  ***)

(**  
How to know which nodes to include? Take a look at `CodePrinter.fs`!

### Create your own set of helper functions

Throughout all these examples, we have duplicated a lot of code. You can typically easily refactor this into some helper functions.  
The Fantomas maintainers are not affiliated with any projects that expose AST construction helpers.  

### Updates

Since code generation is considered to be a nice to have functionality, there is no compatibility between any `Fantomas.Core` version when it comes to the `SyntaxOak` module.  
We do not apply any semantic versioning to `Fantomas.FCS` or `Fantomas.Core.SyntaxOak`. Breaking changes can be expected at any given point.  
Our recommendation is that you include a set of regression tests  to meet your own expectations when upgrading.  
As none of our versions are compatible it is advised to take a very strict dependency on `Fantomas.Core`. Using constraints like `(>= 6.0.0)` will inevitably lead to unexpected problems. 

<fantomas-nav previous="./VSCode.html" next="./UpgradeGuide.html"></fantomas-nav>
*)
