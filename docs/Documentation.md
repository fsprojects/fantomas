# Fantomas: How to use

## Using the command line tool

---

Create a [.NET tool manifest](https://docs.microsoft.com/en-us/dotnet/core/tools/local-tools-how-to-use) to install tools locally
> dotnet new tool-manifest

Install the command line tool with:
> dotnet tool install fantomas

or install the tool globally with
> dotnet tool install -g fantomas

For the overview how to use the tool, you can type the command

	dotnet fantomas --help

```
USAGE: dotnet fantomas [--help] [--recurse] [--force] [--profile] [--fsi <string>] [--stdin] [--stdout] [--out <string>] [--check] [--daemon] [--version] [<string>...]

INPUT:

    <string>...           Input paths: can be multiple folders or files with *.fs,*.fsi,*.fsx,*.ml,*.mli extension.

OPTIONS:

    --recurse, -r         Process the input folder recursively.
    --force               Print the source unchanged if it cannot be parsed correctly.
    --profile             Print performance profiling information.
    --fsi <string>        Read F# source from stdin as F# signatures.
    --stdin               Read F# source from standard input.
    --stdout              Write the formatted source code to standard output.
    --out <string>        Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only.
    --check               Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error
    --daemon              Daemon mode, launches an LSP-like server to can be used by editor tooling.
    --version, -v         Displays the version of Fantomas
    --help                display this list of options.

```

You have to specify an input path and optionally an output path. 
The output path is prompted by `--out` e.g.

	dotnet fantomas ../../../../tests/stackexchange/array.fs --out ../../../../tests/stackexchange_output/array.fs 

Both paths have to be files or folders at the same time. 
If they are folders, the structure of input folder will be reflected in the output one. 
The tool will explore the input folder recursively if you set `--recurse` option.
If you omit the output path, Fantomas will overwrite the input files.

### Check mode

*starting version 3.3*

Verify that a single file or folder was formatted correctly.

> dotnet fantomas --check Source.fs

This will verify if the file `Source.fs` still needs formatting.
If it does, the process will return exit code 99.
In the case that the file does not require any formatting, exit code 0 is returned.
Unexpected errors will return exit code 1.

This scenario is meant to be executed in a continuous integration environment, to enforce that the newly added code was formatted correctly.

### Multiple paths

*starting version 4.5*

Multiple paths can be passed as last argument, these can be both files and folders.  
This cannot be combined with the `--out` and `--stdout` flags.  
When combined with the `--recurse` flag, all passed folders will be processed recursively.

One interesting use-case of passing down multiple paths is that you can easily control the selection and filtering of paths from the current shell.

Consider the following PowerShell scripts:
```powershell
# Create an array with paths
$files =
     Get-ChildItem src/*.fs -Recurse # Find all *.fs files in src,
     | Where-Object { $_.FullName -notlike "*obj*" } # ignore files in the `obj` folder
     | ForEach-Object { $_.FullName } #  and select the full path name.

& "dotnet" "fantomas" $files
```

```powershell
# Filter all added and modified files in git
$files = git status --porcelain | Where-Object { $_ -match "^\s?A?M(.*)\.fs(x|i)?$" } | ForEach-Object { $_.TrimStart("AM").TrimStart(" ", "M") }
& "dotnet" "fantomas" $files
```

Or usage with `find` on unix:

`find my-project/ -type f -name "*.fs" -not -path "*obj*" | xargs dotnet fantomas --check`

### Daemon mode

`--daemon` should not be used directly by end-users. Learn more about this feature in the [Daemon mode documentation](./Daemon%20mode.md)

## Configuration

Fantomas ships with a series of format options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the commandline tool.

A default .editorconfig file would look like
```ini
[*.{fs,fsx}]
indent_size=4
max_line_length=120
end_of_line=crlf
insert_final_newline=true
fsharp_semicolon_at_end_of_line=false
fsharp_space_before_parameter=true
fsharp_space_before_lowercase_invocation=true
fsharp_space_before_uppercase_invocation=false
fsharp_space_before_class_constructor=false
fsharp_space_before_member=false
fsharp_space_before_colon=false
fsharp_space_after_comma=true
fsharp_space_before_semicolon=false
fsharp_space_after_semicolon=true
fsharp_indent_on_try_with=false
fsharp_space_around_delimiter=true
fsharp_max_if_then_else_short_width=40
fsharp_max_infix_operator_expression=50
fsharp_max_record_width=40
fsharp_max_record_number_of_items=1
fsharp_record_multiline_formatter=character_width
fsharp_max_array_or_list_width=40
fsharp_max_array_or_list_number_of_items=1
fsharp_array_or_list_multiline_formatter=character_width
fsharp_max_value_binding_width=80
fsharp_max_function_binding_width=40
fsharp_max_dot_get_expression_width=50
fsharp_multiline_block_brackets_on_same_column=false
fsharp_newline_between_type_definition_and_members=false
fsharp_keep_if_then_in_same_line=false
fsharp_align_function_signature_to_indentation=false
fsharp_alternative_long_member_definitions=false
fsharp_multi_line_lambda_closing_newline=false
fsharp_keep_indent_in_branch=false
fsharp_blank_lines_around_nested_multiline_expressions=true
fsharp_bar_before_discriminated_union_declaration=false
fsharp_keep_max_number_of_blank_lines=100
fsharp_strict_mode=false
```

Please note that you should only add settings to the `.editorconfig` file when you want to deviate from the default settings.
Copying the entire list above is unnecessary.

### indent_size

` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`

```fsharp
let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1
```

`{ defaultConfig with IdentSize = 2 }`

```fsharp
let inline selectRandom (f: _ []) =
  let r = random 1.0

  let rec find =
    function
    | 0 -> fst f.[0]
    | n when r < snd f.[n] -> fst f.[n]
    | n -> find (n - 1)

  find <| f.Length - 1
```

### max_line_length

`max_line_length` has to be an integer greater or equal to 60.
This preference sets the column where we break F# constructs into new lines.
Default = 120.

`defaultConfig`

```fsharp
match myValue with
| Some foo -> someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"
```

`{ defaultConfig with MaxLineLength = 60 }`

```fsharp
match myValue with
| Some foo ->
    someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"
```

### end_of_line

`end_of_line` determines the newline character, `lf` will add `\n` where `crlf` will add `\r\n`.
`cr` is not supported by the F# language spec.
If not set by the user, the default value is determined by `System.Environment.NewLine`.

### insert_final_newline

Adds a final newline character at the end of the file.
Default = true

`defaultConfig`

```fsharp
let a = 42

```

`{ default with InsertFinalNewline = false }`

```fsharp
let a = 42
```

### fsharp_semicolon_at_end_of_line

Add semicolons at the end of lines.
Default = false.

`defaultConfig`

```fsharp
let saturn =
    { X = 8.343366718
      Y = 4.124798564
      Z = -0.4035234171
      VX = -0.002767425107 * daysPerYear
      VY = 0.004998528012 * daysPerYear
      VZ = 2.304172976e-05 * daysPerYear
      Mass = 0.0002858859807 * solarMass }
```

`{ defaultConfig with SemicolonAtEndOfLine = true }`

```fsharp
let saturn =
    { X = 8.343366718;
      Y = 4.124798564;
      Z = -0.4035234171;
      VX = -0.002767425107 * daysPerYear;
      VY = 0.004998528012 * daysPerYear;
      VZ = 2.304172976e-05 * daysPerYear;
      Mass = 0.0002858859807 * solarMass }
```

### fsharp_space_before_parameter

Add a space after the name of a function and before the opening parenthesis of the first parameter.
This setting influences function definitions.
Default = true.

`defaultConfig`

```fsharp
let value (a: int) = x
let DumpTrace () = ()
```

`{ defaultConfig with SpaceBeforeParameter = false }`

```fsharp
let value(a: int) = x
let DumpTrace() = ()
```

### fsharp_space_before_lowercase_invocation

Add a space after the name of a lowercased function and before the opening parenthesis of the first argument.
This setting influences function invocation.
Default = true.

`defaultConfig`

```fsharp
value (a, b)
startTimer ()
```

`{ defaultConfig with SpaceBeforeLowercaseInvocation = false }`

```fsharp
value(a, b)
startTimer()
```

### fsharp_space_before_uppercase_invocation

Add a space after the name of a uppercased function and before the opening parenthesis of the first argument.
This setting influences function invocation.
Default = false.

`defaultConfig`

```fsharp
Value(a, b)
person.ToString()
```

`{ defaultConfig with SpaceBeforeUppercaseInvocation = true }`

```fsharp
Value (a, b)
person.ToString ()
```

### fsharp_space_before_class_constructor

Add a space after a type name and before the class constructor.
Default = false.

`defaultConfig`

```fsharp
type Person() =
    class
    end
```

`{ defaultConfig with SpaceBeforeClassConstructor = true }`

```fsharp
type Person () =
    class
    end
```

### fsharp_space_before_member

Add a space after a member name and before the opening parenthesis of the first parameter.
Default = false.

`defaultConfig`

```fsharp
type Person() =
    member this.Walk(distance: int) = ()
    member this.Sleep() = ignore
    member __.singAlong() = ()
    member __.swim(duration: TimeSpan) = ()
```

`{ defaultConfig with SpaceBeforeMember = true }`

```fsharp
type Person() =
    member this.Walk (distance: int) = ()
    member this.Sleep () = ignore
    member __.singAlong () = ()
    member __.swim (duration: TimeSpan) = ()
```

### fsharp_space_before_colon

Add a space before `:`. Please note that not every `:` is controlled by this setting.
Default = false.

`defaultConfig`

```fsharp
type Point = { x: int; y: int }
let myValue: int = 42 // See https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#right-pad-value-and-function-argument-type-annotations
let update (msg: Msg) (model: Model) : Model = model // See https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#surround-return-type-annotations-with-white-space
```

`{ defaultConfig with SpaceBeforeColon = true }`

```fsharp
type Point = { x : int; y : int }
let myValue : int = 42
let update (msg : Msg) (model : Model) : Model = model
```

### fsharp_space_after_comma

Adds a space after `,` in tuples.
Default = true.

`defaultConfig`

```fsharp
myValue.SomeFunction(foo, bar, somethingElse)
(a, b, c)
```

`{ defaultConfig with SpaceAfterComma = false }`

```fsharp
myValue.SomeFunction(foo,bar,somethingElse)
(a,b,c)
```

### fsharp_space_before_semicolon

Adds a space before `;` in records, arrays, lists, etc.
Default = false.

`defaultConfig`

```fsharp
let a = [ 1; 2; 3 ]
let b = [| foo; bar |]
type C = { X: int; Y: int }
```

`{ defaultConfig with SpaceBeforeSemicolon = true }`

```fsharp
let a = [ 1 ; 2 ; 3 ]
let b = [| foo ; bar |]
type C = { X: int ; Y: int }
```

### fsharp_space_after_semicolon

Adds a space after `;` in records, arrays, lists, etc.
Default = true.

`defaultConfig`

```fsharp
let a = [ 1; 2; 3 ]
let b = [| foo; bar |]
type C = { X: int; Y: int }
```

`{ defaultConfig with SpaceAfterSemicolon = false }`

```fsharp
let a = [ 1;2;3 ]
let b = [| foo;bar |]
type C = { X: int;Y: int }
```

### fsharp_indent_on_try_with

Adds an extra indent to the `with` block of a try/with expression.
Default = false.

`defaultConfig`

```fsharp
try
    if System.DateTime.Now.Second % 3 = 0
    then raise (new System.Exception())
    else raise (new System.ApplicationException())
with
| :? System.ApplicationException -> printfn "A second that was not a multiple of 3"
| _ -> printfn "A second that was a multiple of 3"
```

`{ defaultConfig with IndentOnTryWith = true }`

```fsharp
try
    if System.DateTime.Now.Second % 3 = 0
    then raise (new System.Exception())
    else raise (new System.ApplicationException())
with
    | :? System.ApplicationException -> printfn "A second that was not a multiple of 3"
    | _ -> printfn "A second that was a multiple of 3"
```

### fsharp_space_around_delimiter

Adds a space around delimiters like `[`,`[|`,`{`.
Default = true.

`defaultConfig`

```fsharp
let a = [ 1;2;3 ]
let b = [| 4;5;6 |]
```

`{ defaultConfig with SpaceAroundDelimiter = false }`

```fsharp
let a = [1;2;3]
let b = [|4;5;6|]
```

### fsharp_max_if_then_else_short_width

Fantomas by default follows the if/then/else conventions listed in the [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions).
There is stated:

> Indentation of conditionals depends on the size and complexity of the expressions that make them up. Write them on one line when:
> cond, e1, and e2 are short
> e1 and e2 are not if/then/else expressions themselves.

This setting facilitates this by determining the maximum character width where the if/then/else expression stays in one line.
Default = 40.

`defaultConfig`

```fsharp
if myCheck then truth else bogus
```

`{ defaultConfig with MaxIfThenElseShortWidth = 10 }`

```fsharp
if myCheck then 
    truth 
else 
    bogus
```

### fsharp_max_infix_operator_expression

Control the maximum length for which infix expression can be on one line.
Default = 50.

`defaultConfig`

```fsharp
let WebApp =
    route "/ping" >=> authorized >=> text "pong"
```

`{ defaultConfig with MaxInfixOperatorExpression = 20 }`

```fsharp
let WebApp =
    route "/ping"
    >=> authorized
    >=> text "pong"
```

### fsharp_max_record_width

Control the maximum width for which records should be in one line. Default = 40.
Requires `fsharp_record_multiline_formatter` to be `character_width` to take
effect.

`defaultConfig`

```fsharp
type MyRecord = { X: int; Y: int; Length: int }
let myInstance = { X = 10; Y = 20; Length = 90 }
```

`{ defaultConfig with MaxRecordWidth = 20 }`

```fsharp
type MyRecord =
    { X: int
      Y: int
      Length: int }

let myInstance =
    { X = 10
      Y = 20
      Length = 90 }
```

### fsharp_max_record_number_of_items

Control the maximum number of fields for which records should be in one line.
Default = 1. Requires `fsharp_record_multiline_formatter` to be
`number_of_items` to take effect.

`defaultConfig`

```fsharp
type R = { x: int }

type S = { x: int; y: string }

type T = { x: int; y: string; z: float }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' = { r with x = 3; y = "hello" }

let myRecord''' = { r with x = 3; y = "hello"; z = 0.0 }
```

`{ defaultConfig with MaxRecordSize = 2; RecordMultilineFormatter =
MultilineFormatterType.NumberOfItems }`

```fsharp
type R = { x: int }

type S = { x: int; y: string }

type T =
    { x: int
      y: string
      z: float }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' = { r with x = 3; y = "hello" }

let myRecord''' =
    { r with
          x = 3
          y = "hello"
          z = 0.0 }
```

### fsharp_record_multiline_formatter

Split records expressions/statements into multiple lines based on the given
condition. `character_width` uses character count of the expression, controlled
by `fsharp_max_record_width`. `number_of_items` uses the number of fields in the
record, controlled by `fsharp_max_record_number_of_items`. Default =
`character_width`. Note that in either case, record expressions/statements are
still governed by `max_line_length`.

`defaultConfig`

```fsharp
type R = { x: int }

type S = { x: int; y: string }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' = { r with x = 3; y = "hello" }
```

`{ defaultConfig with RecordMultilineFormatter =
MultilineFormatterType.NumberOfItems }`

```fsharp
type R = { x: int }

type S =
    { x: int
      y: string }

let myRecord = { x = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' =
    { r with
          x = 3
          y = "hello" }
```

### fsharp_max_array_or_list_width

Control the maximum width for which lists and arrays can be in one line. Default
= 40. Requires `fsharp_array_or_list_multiline_formatter` to be
`character_width` to take effect.

`defaultConfig`

```fsharp
let myArray = [| one; two; three |]
```

`{ defaultConfig with MaxArrayOrListWidth = 20 }`

```fsharp
let myArray =
    [| one
       two
       three |]
```

### fsharp_max_array_or_list_number_of_items

Control the maximum number of elements for which lists and arrays can be in
one line. Default = 1. Requires `fsharp_array_or_list_multiline_formatter` to be
`number_of_items` to take effect.

`defaultConfig`

```fsharp
let myList = [ one; two ]

let myArray = [| one; two; three |]
```

`{ defaultConfig with MaxArrayOrListNumberOfItems = 2; ArrayOrListMultilineFormatter =
MultilineFormatterType.NumberOfItems }`

```fsharp
let myList = [ one; two ]

let myArray =
    [| one
       two
       three |]
```

### fsharp_array_or_list_multiline_formatter

Split arrays and lists into multiple lines based on the given condition.
`character_width` uses character count of the expression, controlled by
`fsharp_max_array_or_list_width`. `number_of_items` uses the number of elements
in the array or list, controlled by `fsharp_max_array_or_list_number_of_items`.
Default = `character_width`. Note that in either case, list expressions are
still governed by `max_line_length`.

`defaultConfig`

```fsharp
let myArray = [| one; two; three |]
```

`{ defaultConfig with ArrayOrListMultilineFormatter =
MultilineFormatterType.NumberOfItems }`

```fsharp
let myArray =
    [| one
       two
       three |]
```

### fsharp_max_value_binding_width

Control the maximum expression width for which let and member value/property bindings should be in one line.
The width is that of the pattern for the binding plus the implementating expression but not the keywords (e.g. "let").
Default = 80.

`defaultConfig`

```fsharp
let title = "Great title of project"

type MyType() =
    member this.HelpText = "Some help text"
```

`{ defaultConfig with MaxValueBindingWidth = 10 }`

```fsharp
let title =
    "Great title of project"

type MyType() =
    member this.HelpText =
        "Some help text"
```

### fsharp_max_function_binding_width

Control the maximum width for which function and member bindings should be in one line.
Default = 40.

`defaultConfig`

```fsharp
let printScore score total = printfn "%i / %i" score total

type Triangle() =
    member this.CalculateSurface(width: int, height: int) = width * height / 2
```

`{ defaultConfig with MaxFunctionBindingWidth = 10 }`

```fsharp
let printScore score total =
    printfn "%i / %i" score total

type Triangle() =
    member this.CalculateSurface(width: int, height: int) =
        width * height / 2
```

### fsharp_max_dot_get_expression_width

Control the maximum width for which (nested) [SynExpr.DotGet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#DotGet) expressions should be in one line.
Default = 50.

`defaultConfig`

```fsharp
let job =
    JobBuilder
        .UsingJobData(jobDataMap)
        .Create<WrapperJob>()
        .Build()
```

`{ defaultConfig with MaxDotGetExpressionWidth = 100 }`

```fsharp
let job =
    JobBuilder.UsingJobData(jobDataMap).Create<WrapperJob>().Build()
```

### fsharp_multiline_block_brackets_on_same_column

Alternative way of formatting records, arrays and lists. This will align the braces at the same column level.
Default = false.

`defaultConfig`

```fsharp
let myRecord =
    { Level = 1
      Progress = "foo"
      Bar = "bar"
      Street = "Bakerstreet"
      Number = 42 }

type Range =
    { From: float
      To: float }

let a =
    [| (1, 2, 3)
       (4, 5, 6)
       (7, 8, 9) |]
```

`{ defaultConfig with MultilineBlockBracketsOnSameColumn = true }`

```fsharp
let myRecord =
    {
        Level = 1
        Progress = "foo"
        Bar = "bar"
        Street = "Bakerstreet"
        Number = 42
    }

type Range =
    {
        From: float
        To: float
    }

let a =
    [|
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    |]
```

### fsharp_newline_between_type_definition_and_members

Adds a new line between a type definition and its first member.
Default = false.

`defaultConfig`

```fsharp
type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From
```

`{ defaultConfig with NewlineBetweenTypeDefinitionAndMembers = true }`

```fsharp
type Range =
    { From: float
      To: float }

    member this.Length = this.To - this.From
```

### fsharp_keep_if_then_in_same_line

**Deprecated setting!**

This setting will be removed in the next major version of Fantomas.
As of 4.4, it has no effect anymore due to a change in the MS F# style guide.

#### Original description:

Bypasses the situation where `if`,`then` and `else` are placed underneath each other.
This will ensure `if` and `then` are kept in the same line.
Default = false.

`defaultConfig`

```fsharp
if System.Char.IsUpper(c)
then sprintf "________%s" (c.ToString().ToLower())
else c.ToString()
```

`{ defaultConfig with KeepIfThenInSameLine = true }`

```fsharp
if System.Char.IsUpper(c) then
    sprintf "________%s" (c.ToString().ToLower())
else
    c.ToString()
```

### fsharp_align_function_signature_to_indentation

When a function signature exceeds the `max_line_length`, Fantomas will put all parameters on separate lines.
This setting also places the equals sign and return type on a new line.
Default = false.

`defaultConfig`

```fsharp
[<FunctionName("FormatCode")>]
let run 
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
```

`{ defaultConfig with AlignFunctionSignatureToIndentation = true }`

```fsharp
[<FunctionName("FormatCode")>]
let run
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse
    =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
```

### fsharp_alternative_long_member_definitions

Provides an alternative way of formatting long member and constructor definitions,
where the difference is mainly in the equal sign and returned type placement.
Default = false.

`defaultConfig`

```fsharp
type C
    (
        aVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
    ) =
    class
    end

type D() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse
        ) : ReturnType =
        42
```

`{ defaultConfig with AlternativeLongMemberDefinitions = true }`

```fsharp
type C
    (
        aVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
    )
    =
    class
    end

type D() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse
        )
        : ReturnType
        =
        42
```

### fsharp_multi_line_lambda_closing_newline

Places the closing parenthesis of a multiline lambda argument on the next line.
Default = false.

`defaultConfig`

```fsharp
let printListWithOffset a list1 =
    List.iter
        (fun { ItemOne = a } ->
            // print
            printfn "%s" a)
        list1

let printListWithOffset a list1 =
    list1
    |> List.iter
        (fun elem ->
            // print stuff
            printfn "%d" (a + elem))
```

`{ defaultConfig with MultiLineLambdaClosingNewline = true }`

```fsharp
let printListWithOffset a list1 =
    List.iter
        (fun { ItemOne = a } ->
            // print
            printfn "%s" a
        )
        list1

let printListWithOffset a list1 =
    list1
    |> List.iter (fun elem ->
        // print stuff
        printfn "%d" (a + elem)
    )
```

### fsharp_keep_indent_in_branch

Breaks the normal indentation flow for the last branch of a pattern match or if/then/else expression.
Only when the pattern match or if/then/else is the return value of a function or member.

*This feature is considered experimental and is subject to change*

`defaultConfig`

```fsharp
let main argv =
    let args = parse argv

    let instructions = Library.foo args

    if args.DryRun = RunMode.Dry then
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    else
        // proceed with main method
        let output = Library.execute instructions
        // do more stuff
        0
```

`{ defaultConfig with KeepIndentInBranch = true }`

```fsharp
let main argv =
    let args = parse argv

    let instructions = Library.foo args

    if args.DryRun = RunMode.Dry then
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    else
    // proceed with main method
    let output = Library.execute instructions
    // do more stuff
    0
```

### fsharp_blank_lines_around_nested_multiline_expressions

Surround **nested** multi-line expressions with blank lines.
Existing blank lines are always preserved (via trivia), with exception when [fsharp_keep_max_number_of_blank_lines](#fsharp_keep_max_number_of_blank_lines) is used.  
Top level expressions will always follow the [2020 blank lines revision](https://github.com/fsprojects/fantomas/blob/master/docs/FormattingConventions.md#2020-revision) principle.
Default = true.

`defaultConfig`

```fsharp
let topLevelFunction () =
    printfn "Something to print"

    try
            nothing ()
    with
    | ex ->
        splash ()
    ()

let secondTopLevelFunction () =
    // ...
    ()
```

`{ defaultConfig with BlankLinesAroundNestedMultilineExpressions = false }`

```fsharp
let topLevelFunction () =
    printfn "Something to print"
    try
            nothing ()
    with
    | ex ->
        splash ()
    ()

let secondTopLevelFunction () =
    // ...
    ()
```

### fsharp_bar_before_discriminated_union_declaration

Always use a `|` before every case in the declaration of a discriminated union. If `false`, a `|` character is used only in multiple-case discriminated unions, and is omitted in short single-case DUs.
Default = false.

```fsharp
type MyDU = Short of int
```

`{ defaultConfig with BarBeforeDiscriminatedUnionDeclaration = true }`

```fsharp
type MyDU = | Short of int
```

### fsharp_keep_max_number_of_blank_lines

Set maximal number of consecutive blank lines to keep from original source. It doesn't change number of new blank lines generated by Fantomas.
Default=100

`defaultConfig`

```fsharp
open Foo


let x = 42
```

`{ defaultConfig with KeepMaxNumberOfBlankLines = 1 }`

```fsharp
open Foo

let x = 42
```


### fsharp_strict_mode

If being set, pretty printing is only done via ASTs. Compiler directives, inline comments and block comments will be ignored.
There are numerous situations when the information in the AST alone cannot restore the original code.
**Please do not use this setting for formatting hand written code!**
Valid use-case of this settings is code generation in projects like [FsAst](https://github.com/ionide/FsAst) and [Myriad](https://github.com/MoiraeSoftware/myriad).
Default = false.

`hand written code`

```fsharp
// some great comment
let add a b =
#if INTERACTIVE
    42
#else
    a + b
#endif
```

`{ defaultConfig with StrictMode = true }`

```fsharp
let add a b = a + b
```

## Ignore Files: .fantomasignore

*starting version 4.1*

To exclude files from formatting, create a `.fantomasignore` file in the root of your project.
`.fantomasignore` uses gitignore syntax (via [MAB.DotIgnore](https://github.com/markashleybell/MAB.DotIgnore)).
Ignored files will be picked up when the [Fantomas cli tool](https://www.nuget.org/packages/fantomas/) or the FAKE helpers (in [Fantomas.Extras](https://www.nuget.org/packages/Fantomas.Extras/)) are used.
Exclusion applies both to formatting and the format checking.

```
# Ignore Fable files
.fable/

# Ignore script files
*.fsx
```

Note that Fantomas only searches for a `.fantomasignore` file in or above its current working directory, if one exists; unlike Git, it does not traverse the filesystem for each input file to find an appropriate ignore file.
(This is not true of the Fantomas daemon. The daemon can't rely on being invoked from the right place, and indeed there may not even be a well-defined notion of "right place" for the formatting tasks the daemon is required to perform, so it does search the filesystem for every file individually.)

## Using the API

See [CodeFormatter.fsi](../src/Fantomas/CodeFormatter.fsi) to view the public API of Fantomas.

## A git pre-commit hook sample

A very elegant and transparent way to use Fantomas is including it in a pre-commit git hook, by creating a `.git/hooks/pre-commit` file with:

```
#!/bin/sh
git diff --cached --name-only --diff-filter=ACM -z | xargs -0 $HOME/.dotnet/tools/fantomas
git diff --cached --name-only --diff-filter=ACM -z | xargs -0 git add
```

<small>This script assumes you have installed Fantomas globally as a [dotnet tool](https://www.nuget.org/packages/fantomas/)</small>

**Please use with caution** as [Fantomas is not without bugs](https://github.com/fsprojects/fantomas/issues?q=is%3Aissue+is%3Aopen+label%3A%22bug+%28soundness%29%22).

## FAKE Helpers

Fantomas also exposes some less official helper functions in [Fantomas.Extras](https://www.nuget.org/packages/Fantomas.Extras/), these will be deprecated in the next major version.
It is advised to run the `fantomas` instead when using FAKE, see [example](../fake-sample/README.md).

## Updating to a new Fantomas version

By default, Fantomas adheres to the Microsoft [F# code formatting guidelines](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting).
If these change, Fantomas will follow accordingly. Due to this reason, the output cannot be guaranteed to remain the same when upgrading to a new minor version.

If you are using Git for your source control, it is recommended to ignore commits where `fantomas` was updated using a [.git-blame-ignore-revs file](https://git-scm.com/docs/git-blame#Documentation/git-blame.txt---ignore-revltrevgt).
Check out this [blogpost](https://www.moxio.com/blog/43/ignoring-bulk-change-commits-with-git-blame) for more details.
