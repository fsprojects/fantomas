---
category: End-users
categoryindex: 1
index: 11
---

# Upgrade guide

We wish to capture all changes required to upgrade to a new version. Please note that the focus of this document is about how to upgrade.  
New features are not covered in detail here, for those please refer to our [changelog](https://github.com/fsprojects/fantomas/blob/main/CHANGELOG.md).  
If you find something to be missing from this guide, please consider opening a PR to mend the gap instead of opening an issue.

## v5.0

### .editorconfig

* `fsharp_max_elmish_width` was removed.
* `fsharp_single_argument_web_mode` was removed.
* `fsharp_disable_elmish_syntax` was removed.
* `fsharp_semicolon_at_end_of_line` was removed.
* `fsharp_keep_if_then_in_same_line` was removed.
* `fsharp_indent_on_try_with` was removed.
* If you were using Elmish inspired code (or `fsharp_single_argument_web_mode`) use

```
fsharp_multiline_block_brackets_on_same_column = true
fsharp_experimental_stroustrup_style = true
```
* `fsharp_keep_indent_in_branch` was renamed to `fsharp_experimental_keep_indent_in_branch`

### console application

* The dotnet tool is now targeting `net6.0`.
* `--stdin` was removed.
* `--stdout` was removed.
* `--fsi` was removed.
* `--force` now writes a formatted file to disk, regardless of its validity.

### Miscellaneous

* NuGet package `Fantomas` was renamed to `Fantomas.Core`.
* NuGet package `fantomas-tool` was renamed to `fantomas`.
* `Fantomas.Core` uses [Fantomas.FCS](https://www.nuget.org/packages/Fantomas.FCS) instead of [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service)
* NuGet package `Fantomas.Extras` is deprecated.

## v5.1

### .editorconfig

* The space in patterns is no longer controlled by `fsharp_space_before_parameter`,  
`fsharp_space_before_lowercase_invocation` and `fsharp_space_before_uppercase_invocation` are now used.

## v5.2

### .editorconfig

* `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style` are now merged into one setting `fsharp_multiline_bracket_style`.  
The accepted values for `fsharp_multiline_bracket_style` are `cramped`, `aligned` and `experimental_stroustrup`.  <br />
Note that `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style` will continue to work until the next major version.

## v6.0

### .editorconfig

* `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style` are replaced with `fsharp_multiline_bracket_style`
* `experimental_stroustrup` for `fsharp_multiline_bracket_style` is now `stroustrup`
* `fsharp_newline_before_multiline_computation_expression` was extracted from `fsharp_multiline_bracket_style = stroustrup` and now controls how computation expression behave.
* `fsharp_strict_mode` was removed and can no longer be used.

### console application

* `-v` is now short for `--verbosity` instead of `--version`
* The console output was revamped.
* `--recurse` was removed. Please use [.fantomasignore](./IgnoreFiles.html) file if you wish to ignore certain files.

### Miscellaneous

* The public API of CodeFormatter no longer uses `FSharpOption<'T>`, instead overloads are now used.
* `StrictMode` was removed from `FormatConfig`, not passing the source text in the public API will have the same effect.

## v6.1

### Miscellaneous

* The namespace in [Fantomas.FCS](https://www.nuget.org/packages/Fantomas.FCS) changed from `FSharp.Compiler` to `Fantomas.FCS`.

## v7

### console application

* Target framework is now `net8.0`.

### .editorconfig

* `fsharp_max_dot_get_expression_width` was removed.

## v8 alpha

### .editorconfig

* The default setting for `fsharp_multiline_bracket_style` is now `aligned`, to restore the previous behaviour use `fsharp_multiline_bracket_style = cramped`.

### console application

* Target framework is now `net10.0`.

### Formatting

Chains (dotted member access and calls) are laid out by a new set of rules, written up in full in
[Chains](../contributors/Chains.html). They are a proposal for the F# style guide and may still
change before `v8.0.0` is final.

The rules only apply once a chain has to break, so a chain that already fits on one line is left
alone. Reformatting the whole of `Fantomas.Core` with `v8` moves a handful of lines, none of them
in a chain. The changes below are the ones you are most likely to notice.

#### A run of property access wraps instead of overflowing

Navigation that does not fit is spread over balanced lines, chosen so the longest resulting line is
as short as possible. Previously it was left to overflow the margin. At `max_line_length = 80`:

```fsharp
// v7
let navigation =
    builder.Services.Configuration.Providers.Defaults.Primary.Fallback.Value.Inner

// v8
let navigation =
    builder.Services.Configuration.Providers
        .Defaults.Primary.Fallback.Value.Inner
```

#### A comment no longer fans the chain out one step per line

A comment between the steps forces the chain to break, whatever the line length allows. In `v7`
that break was taken by every step. In `v8` only a call claims a line of its own, and plain
property access rides along at the front of the line belonging to the call it introduces:

```fsharp
// v7
let a =
    config
        // note
        .Settings
        .GetValue(key)

// v8
let a =
    config
        // note
        .Settings.GetValue(key)
```

A chain whose steps are all calls is unaffected, because each of those claims a line either way.

#### A match lambda keeps `function` beside the `(`

This applies with **`fsharp_multi_line_lambda_closing_newline = false`**, which is the default.
With the setting set to `true` nothing changes.

In `v7` a call reached through a dot pushed `function` onto its own line, while the very same
call without a receiver kept it beside the `(`. The two disagreed about the same argument.
A chain now follows what the receiverless call already did:

```fsharp
// v7 and v8 agree here: no receiver, `function` stays beside the `(`
let a =
    configureTheThing (function
        | Some v -> handleSome v
        | None -> handleNone ())

// v7: the same argument, reached through a dot, was laid out differently
let b =
    builder
        .Build()
        .Configure(
            function
            | Some v -> handleSome v
            | None -> handleNone ()
        )

// v8: the dot makes no difference any more
let b =
    builder
        .Build()
        .Configure(function
            | Some v -> handleSome v
            | None -> handleNone ())
```

#### A lambda whose opening line does not fit moves to its own line

This applies with **`fsharp_multi_line_lambda_closing_newline = true`**. With the setting left at
its default of `false` nothing changes.

In `v7` the parameters were hung underneath the opening parenthesis, which pushed them far to the
right and could force the pattern itself to break. Now the whole argument moves down one line and
indents normally. This affects calls with and without a receiver alike. At `max_line_length = 80`:

```fsharp
// v7
let dotted ifaces =
    ifaces
    |> List.tryPick (fun
                         (SynInterfaceImpl(
                             interfaceTy = ty; withKeyword = withRange)) ->
        Some(ty, withRange)
    )

// v8
let dotted ifaces =
    ifaces
    |> List.tryPick
        (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) ->
            Some(ty, withRange)
        )
```

### Fantomas.Core API

These only affect you if you consume `Fantomas.Core` as a library. Formatting source text through
`CodeFormatter.FormatDocumentAsync` is unaffected.

#### Exceptions

* `InvariantViolationException` was added. It derives from `FormatException` and is raised when Fantomas reaches a state its own model says is impossible, which always means a bug in Fantomas rather than a problem with your code.
If you catch `FormatException`, you already catch this.
* `DefineParseException` was added, raised when one or more conditional compilation define combinations produce invalid syntax trees.
* `EndOfLineStyle.OfConfigString "cr"` now raises `FormatException` instead of calling `failwith`.

#### CodeFormatter

All additions, nothing was removed:

* `CodeFormatter.FormatASTAsync(ast, config, source)` was added, next to the existing `FormatASTAsync(ast, source)`.
* `CodeFormatter.GetWriterEventsAsync` was added for debugging. It returns the writer events produced while formatting.

#### Oak: chains

`Expr.Chain` no longer holds a flat `ChainLink list`. A chain is now a head expression, a list of
dot-prefixed segments, and a terminal call:

```fsharp
type ExprChain(head: Expr, segments: ChainSegment list, terminal: ChainTerminal, range)

type ChainCall =
    | Paren of ExprParenNode
    | Unit of UnitNode

type ChainSegment =
    | DotMember of dot: SingleTextNode * expr: Expr
    | DotApplication of dot: SingleTextNode * expr: Expr * call: ChainCall
    | DotIndex of dot: SingleTextNode * indexExpr: Expr

type ChainTerminal =
    | SpaceAllowed of ChainCall
    | NoSpaceAllowed of ChainCall
    | NoTerminal
```

Mapping from the old model:

Removed | Replacement
--- | ---
`ChainLink.Identifier` | `ExprChain.Head`, when it is the first link
`ChainLink.Dot` | the `dot` field of the segment that follows it
`ChainLink.Expr` | `ChainSegment.DotMember`
`ChainLink.AppParen` | `ChainSegment.DotApplication` with `ChainCall.Paren`, or `ExprChain.Terminal` when last
`ChainLink.AppUnit` | `ChainSegment.DotApplication` with `ChainCall.Unit`, or `ExprChain.Terminal` when last
`ChainLink.IndexExpr` | `ChainSegment.DotIndex`
`LinkSingleAppParen`, `LinkSingleAppUnit` | `ChainCall`

A dot now always belongs to the step that follows it, so two adjacent dots are unrepresentable and
you no longer have to pair links up yourself. The final call is `Terminal` rather than the last
element of the list, and `ChainTerminal.NoSpaceAllowed` records that no space may precede its
parenthesis. That is a grammar constraint, not a style choice: a space there reparses
`a.Foo (x).Bar()` as `a.Foo ((x).Bar())`.

#### Oak: expressions absorbed into `Expr.Chain`

These `Expr` cases were removed. Each was a chain in all but name, and all four now arrive as
`Expr.Chain`:

* `Expr.DotLambda` (`_.Property`), now a chain whose `Head` is the `_`
* `Expr.DotIndexedGet` (`a.[i]`), now a `ChainSegment.DotIndex`
* `Expr.AppLongIdentAndSingleParenArg` (`a.Foo(x)`), now a chain with a terminal call
* `Expr.NestedIndexWithoutDot`, which was already dead: nothing ever constructed it

`Expr.AppWithLambda` is unchanged, but no longer receives calls that have no prefix arguments;
those are chains now.

A dotted long identifier such as `a.b.c` yields `Expr.Chain` in expression position, where it
previously yielded `Expr.OptVar`. `Expr.OptVar` still exists, and is still produced for long
identifiers without dots and for the optional-argument form `?a.b`. A single identifier is
`Expr.Ident`, as before.

#### Oak: other node changes

* `Expr.DynamicChain` was added for chained `?` operator accesses such as `x?a("")?b(t)`.
* `ComputationExpressionStatement` collapsed from four cases to two. `LetOrUseStatement`,
`LetOrUseBangStatement` and `AndBangStatement` are all `BindingStatement of BindingNode` now,
and `ExprLetOrUseNode`, `ExprLetOrUseBangNode` and `ExprAndBang` were removed.
* `NamePatPair` was renamed to `NamePatPairNode`, and its `ident: SingleTextNode` became
`fieldName: IdentListNode`.
* `PatRecordField` was removed and merged into `NamePatPairNode`. `PatRecordNode.Fields` is now
a `NamePatPairNode list`, and the old `Prefix` and `FieldName` fields are together in
`fieldName`.

<fantomas-nav previous="GeneratingCode.md" next="Recipes.md"></fantomas-nav>