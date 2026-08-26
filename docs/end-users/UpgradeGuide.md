---
category: End-users
categoryindex: 1
index: 12
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
* Warnings and errors are written to standard error instead of standard out. A script that captured standard out to detect failures needs to capture standard error as well. Informational output stays on standard out, including `--version` and the files `--check` reports as needing formatting.
* A run over a single file reports the path it was given rather than only the file name, so `fantomas src/A.fs` prints `src/A.fs was formatted.` where it printed `A.fs was formatted.`. The same applies to the unchanged, ignored and failure messages. A run over several files already reported the path, so a script that handled both cases can now treat them alike.
* A file that cannot be parsed is reported with the position of each diagnostic instead of `Could not parse the file.`, one line per diagnostic in the shape `src/A.fs(3,9): error FS0583: Unmatched '('`, followed by the source around the failure with a caret under it. `--check` reported the same failure as an exception dump with a stack trace and now uses this as well. A script that matched on `Could not parse the file.` needs to match on the new text.
* The `--help` page is written by Fantomas instead of by Argu, and `-h` is accepted alongside `--help`. An argument error prints its complaint on standard error followed by a pointer to `--help`, where it used to print Argu's usage block.
* `--out` now mirrors the structure of the input folder, and creates the folders it needs.
* The message for an input path Fantomas cannot work with is the same whether the run formats or checks. `--check` reported `Input path 'x' is unsupported file type` and `Input path 'x' not found` without a full stop where a format run ended both with one. Both now read `Input path 'x' is an unsupported file type.` and `Input path 'x' not found.` A script matching on the old text needs updating. A run with no input path at all is no longer refused; see the bullet on the current folder below.
* A file whose extension is not lowercase, such as `A.FS`, is now formatted. Up to `v7` it was refused as an unsupported file type when named directly, and passed over when found while walking a folder, so a folder run can now touch files it used to leave alone.
* `fantomas src/ --out src` now formats the folder in place. Up to `v7` a trailing separator made the two paths count as different places, so every file was taken for the previous run's output and skipped: the run printed an empty table and exited 0 without formatting anything. The same applied to `fantomas src --out src/`. A build step that used either spelling was doing nothing and will start doing the work.
* Several input paths are told apart by asking the file system rather than by whether they carry an extension. Up to `v7`, `fantomas my.stuff src` reported `Failed to format file: my.stuff` and exited 1, because a folder whose name contains a dot was taken for a file. It now formats the folder. The converse also held: a path with no extension was taken for a folder, so naming a file that way ended the whole run rather than that one file.
* A single file matched by `.fantomasignore` is reported on standard out, as `- A.fs was ignored by .fantomasignore.` Up to `v7` it printed nothing unless `--verbosity d` was given, even though a folder run whose only file was that same one reported it at normal verbosity. The two now agree.
* Everything a format or check run prints was rewritten. A run over a folder printed a bordered table of headings and counts; it now prints one sentence per file that changed and one line of counts. A script reading this output needs updating, and `--json` is there for a caller that has to act on the result rather than read it. The shapes are set out under "What a run prints now" below.
* `--profile` was removed. Use `fantomas profile <paths>`, which formats one file at a time so the timings can be compared and writes nothing. The flag wrote formatted files to disk as a side effect of measuring them, and the command does not, so it could not be kept working without silently changing what it did. Typing `--profile` reports that it is a command and prints the line to run instead.
* `--check` and `--daemon` can also be spelled `fantomas check` and `fantomas daemon`. Both flags keep working and are not deprecated, so nothing has to change. On a terminal the older spelling prints a one line note saying how it is spelled now; a redirected stream never sees it, so build logs and editor integrations are unaffected.
* A run with no input path formats the folder you are in, where it used to refuse with `No input path provided.` A script that relied on the refusal to catch a missing argument no longer gets one, and will format the working directory instead.
* A malformed command line exits 1 rather than 2. The documented exit codes have only ever been 0, 99 and 1; 2 came from the argument parser and was never one Fantomas chose.
* A token beginning with a dash that is not a flag Fantomas has is reported as an unknown flag rather than as a missing input path. `fantomas --chek src` said `Input path '--chek' not found.` and now says `'--chek' is not a Fantomas flag. Did you mean '--check'?` `--` now ends the flags, so a path beginning with a dash can be named after it.
* Repeating a flag is allowed and the last one wins, where it used to be refused outright with `argument '--check' has been specified more than once`. `--out` is the exception: given twice, the run is refused rather than a destination being chosen for you.
* A file that cannot be parsed is reported as `src/A.fs could not be parsed by Fantomas:` rather than `Fantomas could not parse src/A.fs:`, and a construct that could not be modelled as `src/A.fs could not be formatted by Fantomas:`. A script matching on the old text needs updating.
* `--check` no longer reports a file that will not parse twice, once as an error and once as needing formatting.
* `--force` announces invalid output as a warning on standard error rather than on standard out, and says what happened rather than only that it happened.
* Output that Fantomas will not accept from itself is reported as a bug in Fantomas rather than as a problem with your file, and says that nothing was written and where to report it. `Formatting A.fs leads to invalid F# code` became `src/A.fs could not be formatted by Fantomas:` followed by that explanation, what the parser said about the output, and the lines of the output around it with a caret under the failure. Those are lines of the output, which is written nowhere and is not your file, and the report says so; the diagnostics carry no line and column of their own for the same reason. Up to `v7` you were told only that something was invalid and left to run again with `--force` and find it yourself. `--check` reports it the same way. A script matching on the old text needs updating.
* `--version` prints the commit hash trimmed to the short form the `--help` page has always shown, and the whole of it at `--verbosity d`. `Fantomas.Client` is unaffected: it cuts the version at `+`.
* The `.fantomasignore` that applies to a file is found by walking up from that file rather than from the directory Fantomas was started in, so where you run the command from no longer decides which ignore file applies. See "Which `.fantomasignore` applies" below.
* `--json` no longer names a file that `.fantomasignore` matched, and `ignored` is no longer a status a file can carry. See "What a run says about skipped files" below.
* A run writing to `--out` reports what was written rather than what changed, `32 files written to build, 2 reformatted.`, since under `--out` every input produces an output file whether or not its content changed. A single file named on the command line says where it went, `+ src/A.fs was formatted and written to build/A.fs.`

#### What a run prints now

Each file that changed gets a sentence of its own, opening with a character that says what happened
to it, and the run ends with a line of counts:

```text
$ dotnet fantomas src
+ src/A.fs was formatted.

1 file formatted, 30 unchanged.
```

```text
$ dotnet fantomas check src
! src/A.fs needs formatting.

1 file needs formatting, 30 already formatted. Run dotnet fantomas src to format it.
```

A run over a single named file is answered on its own terms, with no counts added to the one line.
A check that finds nothing prints nothing and exits 0, as it always did.

The characters are `+` formatted, `=` unchanged, `-` skipped, `!` needs formatting and `x` failed.
Where the output goes to a terminal that can draw them, they are `✔`, `=`, `○`, `!` and `✘`
instead; both sets carry the same five states and the words beside them say the same thing either
way, so nothing is lost by the plainer set. Colour is used where the terminal takes it, dropped
where the stream is redirected, and `NO_COLOR` is honoured. Standard out and standard error are
decided separately, so piping one of them does not take the colour off the other.

If you parse this output, move to `--json`, which puts one document on standard out describing
every file the run looked at.

#### Which `.fantomasignore` applies

Up to now the command line resolved one ignore file for the whole run, the nearest at or above the
directory it started in. It is now found by walking up from the file being formatted, which is what
the daemon has always done. Of everything in this release this is the change most likely to be
noticed.

The first thing it changes is that the directory you run from no longer decides which ignore file
applies. Up to `v7` this skipped every file under `src`, on the strength of an ignore file that had
nothing to do with them, and never read the one sitting above `src`:

```text
$ cd tools && dotnet fantomas ../src     # v7: matched ../src/*.fs against tools/.fantomasignore
```

The second is that an ignore file in a subfolder was honoured by an editor and invisible to a
pipeline, so the same file was skipped in one and formatted in the other:

```text
repo/.fantomasignore          # found by both
repo/sub/.fantomasignore      # found by the editor, ignored by the command line
repo/sub/S.fs                 # skipped in the editor, formatted by CI
```

Both now resolve per file, so a nested ignore file that used to have no effect on a command line
run has one. If a subfolder of your repository carries a `.fantomasignore`, check whether it names
files you have been formatting all along.

Note that this is still the nearest ignore file and not the union of every one above it, which is
where Fantomas differs from `.gitignore`. A pattern you wrote at the root of a repository therefore
has no effect on a folder that carries an ignore file of its own, and `fantomas doctor <file>` will
say so: it names the ignore file that governs the file, and names one further up whose pattern
would have skipped it.

A folder that `.fantomasignore` names is no longer opened at all. Up to now every file inside it
was found and then rejected one at a time. Nothing is formatted that was not formatted before, and
a run over a repository that ignores a vendored checkout no longer reads it.

#### What a run says about skipped files

It no longer says how many, and `--json` no longer names them at all: a file an ignore pattern
matched used to be listed in `files` with a status of `ignored`, and is absent now.

The number could not be honest. An ignore pattern that names a file can be counted, because the
file is found and then set aside. One that names a folder cannot, because the folder is never
opened and what is inside it is unknown by design. A count right about the first and blind to the
second reads as though it covered both: Fantomas's own repository ignores three folders holding
ninety six F# files, and the count said nought.

A file you name on the command line is the exception and still gets a line of its own, because a
count is the only other place a path could be accounted for and no count carries this one:

```text
$ dotnet fantomas A.fs Skipped.fs
- Skipped.fs was ignored by .fantomasignore.

1 file unchanged.
```

Everything else that was skipped is named at `--verbosity d`, a file and a folder each in its own
words:

```text
$ dotnet fantomas check --verbosity d .
[... DBG] './.deps' was not opened, .fantomasignore names it
[... DBG] './src/A.fs' was ignored
```

A run that looked at no file at all still says so on standard error and exits 0, so a glob that
matches nothing and an ignore file that grew too wide are both still caught.

#### `--out <folder>` mirrors the input folder

Up to `v7`, every file found under the input folder was written straight into the root of the
output folder, whatever its depth. Nesting collapsed, and two files with the same name in
different subfolders overwrote each other without a warning. From `v8`, the path of each file
relative to the input folder is preserved:

```
# input
src/A.fs
src/nested/A.fs

# v7: dotnet fantomas src --out out
out/A.fs           # whichever of the two was formatted last

# v8: dotnet fantomas src --out out
out/A.fs
out/nested/A.fs
```

This is what [Getting Started](./GettingStarted.html) has always described, so no action is
needed if you followed the documentation. If you relied on the flattening to collect a tree of
files into a single folder, that step now has to be done by whatever calls Fantomas.

An output folder that sits inside the input folder is left out of the scan. Up to `v7`, running
`dotnet fantomas src --out src/formatted` picked the previous run's output back up as input,
which the flattening hid; with the tree preserved it would nest one folder deeper on every run.

#### `--out` creates the folders it writes into

Up to `v7`, `--out <file>` failed with `Failed to format file` and exit code 1 when the folder
of the path given to it did not exist. The root of an `--out <folder>` was always created for
you. From `v8`, Fantomas creates whatever folder it has to write into, which includes the
subfolders the mirroring above needs:

```bash
# v7: fails unless ./output exists
# v8: creates ./output
dotnet fantomas ./input/array.fs --out ./output/array.fs
```

If your build script creates the output folders before calling Fantomas, it can keep doing so.
`mkdir -p` and its equivalents are unaffected by this change.

### Formatting

Chains (dotted member access and calls) are laid out by a new set of rules, written up in full in
[Formatting chain expressions](../contributors/Chains.html). They are a proposal for the F# style guide and may still
change before `v8.0.0` is final.

The layout rules only apply once a chain has to break, so a chain that already fits on one line is
left alone. The spacing rule directly below is the exception: it applies whether or not the chain
breaks, and it reaches a few things that are not chains at all. The changes here are the ones you
are most likely to notice.

#### A call keeps its space only when the whole name is plain

`fsharp_space_before_uppercase_invocation` and `fsharp_space_before_lowercase_invocation` ask for a
space before the parenthesis of a call. They now get a say only when the whole thing being called
is a plain dotted name. A call, an index, a receiver that is not a name, or a type application
anywhere in it, and the parenthesis stays tight whatever the settings say.

On default settings, where `fsharp_space_before_lowercase_invocation` is `true`:

```fsharp
// v7
xs.map(fun a -> a + 1).filter (fun a -> a > 1)
Foo().bar ()
myList.[7].someFunction (arg)
unbox<bool> (value)
jsOptions<Vis.Options> (fun o -> o.autoResize <- Some true)

// v8
xs.map(fun a -> a + 1).filter(fun a -> a > 1)
Foo().bar()
myList.[7].someFunction(arg)
unbox<bool>(value)
jsOptions<Vis.Options>(fun o -> o.autoResize <- Some true)
```

A plain dotted name is untouched, however long it is, so module-qualified functions keep the space
they had:

```fsharp
// v7 and v8 agree
List.map (f)
Fantomas.FCS.Text.Range.unionRanges (r1, r2)
```

Fantomas does not add parentheses, so a generic application written without them never comes into
it and `unbox<int> obj` is left as written.

This is the rule agreed at
[fslang-design#648](https://github.com/fsharp/fslang-design/issues/648), where the reasoning is
laid out in full.

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

#### No longer binary compatible with `v7`

Several discriminated unions are structs now. That changes nothing about how they are constructed,
matched or compared, so no source of yours has to be edited, but an assembly compiled against `v7`
has to be rebuilt against `v8`.

#### Exceptions

* `InvariantViolationException` was added. It derives from `FormatException` and is raised when Fantomas reaches a state its own model says is impossible, which always means a bug in Fantomas rather than a problem with your code.
If you catch `FormatException`, you already catch this.
* `DefineParseException` was added, raised when one or more conditional compilation define combinations produce invalid syntax trees.
* `EndOfLineStyle.OfConfigString "cr"` now raises `FormatException` instead of calling `failwith`.

#### CodeFormatter

* `CodeFormatter.IsValidFSharpCodeAsync` was replaced by `CodeFormatter.ValidateFSharpCodeAsync`, which answers with a `ValidationResult` instead of a `bool`:
  
  ```fsharp
  // v7
  let! isValid = CodeFormatter.IsValidFSharpCodeAsync(isSignature, source)
  
  // v8
  let! validation = CodeFormatter.ValidateFSharpCodeAsync(isSignature, source)
  let isValid = validation.IsValid
  ```
  
  `ValidationResult.Diagnostics` carries what Fantomas refused: every error, and every warning it does not tolerate, positioned in the source it was given. It is empty exactly when `IsValid` is true. The boolean threw that away, so a caller that had to tell somebody why the source was refused had to parse it a second time to find out. This is what lets the command line show you the line of its own output that failed.
  

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