# Fantomas: How to use

## Using the command line tool

---

Create a [.NET tool manifest](https://docs.microsoft.com/en-us/dotnet/core/tools/local-tools-how-to-use) to install tools locally
> dotnet new tool-manifest

Install the command line tool with:
> dotnet tool install fantomas-tool

or install the tool globally with
> dotnet tool install -g fantomas-tool

For the overview how to use the tool, you can type the command

	dotnet fantomas --help

```
USAGE: dotnet fantomas [--help] [--recurse] [--force] [--profile] [--fsi <string>] [--stdin] [--stdout] [--out <string>] [--check] [--version] [<string>]

INPUT:

    <string>              Input path: can be a folder or file with *.fs,*.fsi,*.fsx,*.ml,*.mli extension.

OPTIONS:

    --recurse, -r         Process the input folder recursively.
    --force               Print the source unchanged if it cannot be parsed correctly.
    --profile             Print performance profiling information.
    --fsi <string>        Read F# source from stdin as F# signatures.
    --stdin               Read F# source from standard input.
    --stdout               Write the formatted source code to standard output.
    --out <string>        Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only.
    --check               Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error
                          was an internal error
    --version, -v         Displays the version of Fantomas
    --help                display this list of options.

```

You have to specify an input path and optionally an output path. 
The output path is prompted by `--out` e.g.

	dotnet fantomas ../../../../tests/stackexchange/array.fs --out ../../../../tests/stackexchange_output/array.fs 

Both paths have to be files or folders at the same time. 
If they are folders, the structure of input folder will be reflected in the output one. 
The tool will explore the input folder recursively if you set `--recurse` option (see [Options section](#options)).
If you omit the output path, Fantomas will overwrite the input files.

## Configuration

Fantomas ships with a series of format options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the commandline tool.

A default .editorconfig file would look like
```ini
[*.fs]
indent_size=4
max_line_length=120
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
fsharp_max_array_or_list_width=40
fsharp_max_value_binding_width=40
fsharp_max_function_binding_width=40
fsharp_multiline_block_brackets_on_same_column=false
fsharp_newline_between_type_definition_and_members=false
fsharp_keep_if_then_in_same_line=false
fsharp_max_elmish_width=40
fsharp_single_argument_web_mode=false
fsharp_align_function_signature_to_indentation=false
fsharp_alternative_long_member_definitions=false
fsharp_strict_mode=false
```

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
let value (a:int) = x
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

`{ defaultConfig with SpaceBeforeClassConstructor = true }

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

Add a space before `:`.
Default = false.

`defaultConfig`

```fsharp
type Point = { x: int; y: int }

let update (msg: Msg) (model: Model): Model = model
```

`{ defaultConfig with SpaceBeforeColon = true }`

```fsharp
type Point = { x : int; y : int }

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

> If either cond, e1 or e2 are longer, but not multi-line:
> if cond
> then e1
> else e2

This setting controls what exactly short means in terms of character count.
Default = 40.

`defaultConfig`

```fsharp
if System.Char.IsUpper(c) then sprintf "_%s" (c.ToString().ToLower()) else c.ToString()
```

Neither of the expression is longer than the default 40 here.

`{ defaultConfig with MaxIfThenElseShortWidth = 30 }`

```fsharp
if System.Char.IsUpper(c)
then sprintf "_%s" (c.ToString().ToLower())
else c.ToString()
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

Control the maximum width for which records should be in one line.
Default = 40.

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

### fsharp_max_array_or_list_width

Control the maximum width for which lists and arrays should be in one line.
Default = 40.

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

### fsharp_max_value_binding_width

Control the maximum width for which let and member value bindings should be in one line.
Default = 40.

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

Control the maximum width for which let and member function bindings should be in one line.
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

### fsharp_max_elmish_width

Control the maximum width for which an elmish expression should be in one line.
See [Formatting Elmish style guide](./Formatting-Elmish-code.md) for more information.
Default = 40.

`defaultConfig`

```fsharp
let d = div [] [ p [] [ str "meh" ] ]
```

`{ defaultConfig with MaxElmishWidth = 10 }`

```fsharp
let d =
    div [] [
        p [] [
            str "meh"
        ]
    ]
```

### fsharp_single_argument_web_mode

Applies similar behavior when the elmish expression only contains a single argument.
Default = false.

`defaultConfig`

```fsharp
let a =
    Html.button [ prop.style [ style.marginLeft 5 ]
                  prop.onClick (fun _ -> setCount (count - 1))
                  prop.text "Decrement" ]
```

`{ defaultConfig with SingleArgumentWebMode = true} `

```fsharp
let a =
    Html.button [
        prop.style [ style.marginLeft 5 ]
        prop.onClick (fun _ -> setCount (count - 1))
        prop.text "Decrement"
    ]
```

### fsharp_align_function_signature_to_indentation

When a function signature exceeds the `max_line_length`, Fantomas will put the all but first parameters and the return type on new lines.
These will align with the start column of the first parameter.
This setting will place all parameters on the next line and align with one indent instead.
Default = false.

`defaultConfig`

```fsharp
[<FunctionName("FormatCode")>]
let run ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
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

Provides an alternative way of formatting long member and constructor definitions.
Default = false.

`defaultConfig`

```fsharp
type C(aVeryLongType: AVeryLongTypeThatYouNeedToUse,
       aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
       aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =
    class
    end
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
```

### fsharp_strict_mode

If being set, pretty printing is only done via ASTs. Compiler directives, inline comments and block comments will be ignored.
There are numerous situations when the information in the AST alone cannot restored the original code.
**Please do not use this setting for formatting hand written code!**
Valid us-case of this settings is code generation in projects like [FsAst](https://github.com/ionide/FsAst) and [Myriad](https://github.com/MoiraeSoftware/myriad).
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

`{ defaultConfig with StriceMode = true }`

```fsharp
let add a b = a + b
```

## Using the API

See [CodeFormatter.fsi](../src/Fantomas/CodeFormatter.fsi) to view the public API of Fantomas.

## FAKE Helpers

Fantomas also exposes some less official helper functions when formatting code in FAKE scripts.
Checkout the [FAKE sample](../fake-sample/README.md) for more details.
These functions are not consider to be part of the public API, so they could be improved without the need to bump the major version.