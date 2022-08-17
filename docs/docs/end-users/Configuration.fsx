(**
---
category: End-users
categoryindex: 1
index: 3
---
*)
#r "nuget: Fantomas.Core, 5.0.0-alpha-*"

open Fantomas.Core.FormatConfig
open Fantomas.Core

(**
# Configuration
Fantomas ships with a series of format options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the commandline tool.

Please note that you should only add settings to the `.editorconfig` file when you want to deviate from the default settings.

*)

(**
### indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`

*)

let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1

{ defaultConfig with IdentSize = 2 }

let inline selectRandom (f: _ []) =
  let r = random 1.0

  let rec find =
    function
    | 0 -> fst f.[0]
    | n when r < snd f.[n] -> fst f.[n]
    | n -> find (n - 1)

  find <| f.Length - 1

(**
### max_line_length
`max_line_length` has to be an integer greater or equal to 60.
This preference sets the column where we break F# constructs into new lines.
Default = 120.

`defaultConfig`

*)

match myValue with
| Some foo -> someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"

{ defaultConfig with MaxLineLength = 60 }

match myValue with
| Some foo ->
    someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"

(**
### end_of_line
`end_of_line` determines the newline character, `lf` will add `\n` where `crlf` will add `\r\n`.
`cr` is not supported by the F# language spec.
If not set by the user, the default value is determined by `System.Environment.NewLine`.

### insert_final_newline

Adds a final newline character at the end of the file.
Default = true

`defaultConfig`
*)

let a = 42


{ default with InsertFinalNewline = false }

let a2 = 42

(**
### fsharp_space_before_parameter
Add a space after the name of a function and before the opening parenthesis of the first parameter.
This setting influences function definitions.
Default = true.

`defaultConfig`

*)

let value (a: int) = x
let DumpTrace () = ()

{ defaultConfig with SpaceBeforeParameter = false }

let value(a: int) = x
let DumpTrace() = ()

(**
### fsharp_space_before_lowercase_invocation
Add a space after the name of a lowercased function and before the opening parenthesis of the first argument.
This setting influences function invocation.
Default = true.

`defaultConfig`

*)

value (a, b)
startTimer ()

{ defaultConfig with SpaceBeforeLowercaseInvocation = false }


value(a, b)
startTimer()


(**
### fsharp_space_before_uppercase_invocation
Add a space after the name of a uppercased function and before the opening parenthesis of the first argument.
This setting influences function invocation.
Default = false.

`defaultConfig`

*)

Value(a, b)
person.ToString()


{ defaultConfig with SpaceBeforeUppercaseInvocation = true }


Value (a, b)
person.ToString ()


(**
### fsharp_space_before_class_constructor
Add a space after a type name and before the class constructor.
Default = false.

`defaultConfig`

*)

type Person() =
    class
    end


{ defaultConfig with SpaceBeforeClassConstructor = true }


type Person () =
    class
    end


(**
### fsharp_space_before_member
Add a space after a member name and before the opening parenthesis of the first parameter.
Default = false.

`defaultConfig`

*)

type Person() =
    member this.Walk(distance: int) = ()
    member this.Sleep() = ignore
    member __.singAlong() = ()
    member __.swim(duration: TimeSpan) = ()


{ defaultConfig with SpaceBeforeMember = true }


type Person() =
    member this.Walk (distance: int) = ()
    member this.Sleep () = ignore
    member __.singAlong () = ()
    member __.swim (duration: TimeSpan) = ()


(**
### fsharp_space_before_colon
Add a space before `:`. Please note that not every `:` is controlled by this setting.
Default = false.

`defaultConfig`

*)

type Point = { x: int; y: int }
let myValue: int = 42 // See https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#right-pad-value-and-function-argument-type-annotations
let update (msg: Msg) (model: Model) : Model = model // See https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#surround-return-type-annotations-with-white-space


{ defaultConfig with SpaceBeforeColon = true }


type Point = { x : int; y : int }
let myValue : int = 42
let update (msg : Msg) (model : Model) : Model = model


(**
### fsharp_space_after_comma
Adds a space after `,` in tuples.
Default = true.

`defaultConfig`

*)

myValue.SomeFunction(foo, bar, somethingElse)
(a, b, c)


{ defaultConfig with SpaceAfterComma = false }


myValue.SomeFunction(foo,bar,somethingElse)
(a,b,c)


(**
### fsharp_space_before_semicolon
Adds a space before `;` in records, arrays, lists, etc.
Default = false.

`defaultConfig`

*)

let a = [ 1; 2; 3 ]
let b = [| foo; bar |]
type C = { X: int; Y: int }


{ defaultConfig with SpaceBeforeSemicolon = true }


let a = [ 1 ; 2 ; 3 ]
let b = [| foo ; bar |]
type C = { X: int ; Y: int }


(**
### fsharp_space_after_semicolon

Adds a space after `;` in records, arrays, lists, etc.
Default = true.

`defaultConfig`

*)


let a = [ 1; 2; 3 ]
let b = [| foo; bar |]
type C = { X: int; Y: int }


{ defaultConfig with SpaceAfterSemicolon = false }


let a = [ 1;2;3 ]
let b = [| foo;bar |]
type C = { X: int;Y: int }


(**
### fsharp_space_around_delimiter

Adds a space around delimiters like `[`,`[|`,{`.
Default = true.

`defaultConfig`

*)


let a = [ 1;2;3 ]
let b = [| 4;5;6 |]


{ defaultConfig with SpaceAroundDelimiter = false }


let a = [1;2;3]
let b = [|4;5;6|]

(**
### fsharp_max_if_then_short_width

Control the maximum length for which if/then expression without an else expression can be on one line.  
The [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions) recommends to never write such an expression in one line.
> If the else expression is absent, it is recommended to never to write the entire expression in one line.
Default = 0.

`defaultConfig`

*)


if a then 
    ()


{ defaultConfig with MaxIfThenShortWidth = 15 }


if a then ()


(**
### fsharp_max_if_then_else_short_width

Fantomas by default follows the if/then/else conventions listed in the [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions).
There is stated:

> Indentation of conditionals depends on the size and complexity of the expressions that make them up. Write them on one line when:
> cond, e1, and e2 are short
> e1 and e2 are not if/then/else expressions themselves.

This setting facilitates this by determining the maximum character width where the if/then/else expression stays in one line.
Default = 40.

`defaultConfig`

*)


if myCheck then truth else bogus


{ defaultConfig with MaxIfThenElseShortWidth = 10 }


if myCheck then 
    truth 
else 
    bogus


(**
### fsharp_max_infix_operator_expression

Control the maximum length for which infix expression can be on one line.
Default = 50.

`defaultConfig`

*)


let WebApp =
    route "/ping" >=> authorized >=> text "pong"


{ defaultConfig with MaxInfixOperatorExpression = 20 }


let WebApp =
    route "/ping"
    >=> authorized
    >=> text "pong"


(**
### fsharp_max_record_width

Control the maximum width for which records should be in one line. Default = 40.
Requires `fsharp_record_multiline_formatter` to be `character_width` to take
effect.

`defaultConfig`

*)


type MyRecord = { X: int; Y: int; Length: int }
let myInstance = { X = 10; Y = 20; Length = 90 }


{ defaultConfig with MaxRecordWidth = 20 }


type MyRecord =
    { X: int
      Y: int
      Length: int }

let myInstance =
    { X = 10
      Y = 20
      Length = 90 }


(**
### fsharp_max_record_number_of_items

Control the maximum number of fields for which records should be in one line.
Default = 1. Requires `fsharp_record_multiline_formatter` to be
`number_of_items` to take effect.

`defaultConfig`

*)


type R = { x: int }

type S = { x: int; y: string }

type T = { x: int; y: string; z: float }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' = { r with x = 3; y = "hello" }

let myRecord''' = { r with x = 3; y = "hello"; z = 0.0 }


{ defaultConfig with MaxRecordSize = 2; RecordMultilineFormatter =
MultilineFormatterType.NumberOfItems }


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


(**
### fsharp_record_multiline_formatter

Split records expressions/statements into multiple lines based on the given
condition. `character_width` uses character count of the expression, controlled
by `fsharp_max_record_width`. `number_of_items` uses the number of fields in the
record, controlled by `fsharp_max_record_number_of_items`. Default =
`character_width`. Note that in either case, record expressions/statements are
still governed by `max_line_length`.

`defaultConfig`

*)


type R = { x: int }

type S = { x: int; y: string }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' = { r with x = 3; y = "hello" }


{ defaultConfig with RecordMultilineFormatter =
MultilineFormatterType.NumberOfItems }


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


(**
### fsharp_max_array_or_list_width

Control the maximum width for which lists and arrays can be in one line. Default
= 40. Requires `fsharp_array_or_list_multiline_formatter` to be
`character_width` to take effect.

`defaultConfig`

*)


let myArray = [| one; two; three |]


{ defaultConfig with MaxArrayOrListWidth = 20 }


let myArray =
    [| one
       two
       three |]


(**
### fsharp_max_array_or_list_number_of_items

Control the maximum number of elements for which lists and arrays can be in
one line. Default = 1. Requires `fsharp_array_or_list_multiline_formatter` to be
`number_of_items` to take effect.

`defaultConfig`

*)


let myList = [ one; two ]

let myArray = [| one; two; three |]


{ defaultConfig with MaxArrayOrListNumberOfItems = 2; ArrayOrListMultilineFormatter =
MultilineFormatterType.NumberOfItems }


let myList = [ one; two ]

let myArray =
    [| one
       two
       three |]


(**
### fsharp_array_or_list_multiline_formatter

Split arrays and lists into multiple lines based on the given condition.
`character_width` uses character count of the expression, controlled by
`fsharp_max_array_or_list_width`. `number_of_items` uses the number of elements
in the array or list, controlled by `fsharp_max_array_or_list_number_of_items`.
Default = `character_width`. Note that in either case, list expressions are
still governed by `max_line_length`.

`defaultConfig`

*)


let myArray = [| one; two; three |]


{ defaultConfig with ArrayOrListMultilineFormatter =
MultilineFormatterType.NumberOfItems }


let myArray =
    [| one
       two
       three |]


(**
### fsharp_max_value_binding_width

Control the maximum expression width for which let and member value/property bindings should be in one line.
The width is that of the pattern for the binding plus the implementating expression but not the keywords (e.g. "let").
Default = 80.

`defaultConfig`

*)


let title = "Great title of project"

type MyType() =
    member this.HelpText = "Some help text"


{ defaultConfig with MaxValueBindingWidth = 10 }


let title =
    "Great title of project"

type MyType() =
    member this.HelpText =
        "Some help text"


(**
### fsharp_max_function_binding_width

Control the maximum width for which function and member bindings should be in one line.
Default = 40.

`defaultConfig`

*)


let printScore score total = printfn "%i / %i" score total

type Triangle() =
    member this.CalculateSurface(width: int, height: int) = width * height / 2


{ defaultConfig with MaxFunctionBindingWidth = 10 }


let printScore score total =
    printfn "%i / %i" score total

type Triangle() =
    member this.CalculateSurface(width: int, height: int) =
        width * height / 2


(**
### fsharp_max_dot_get_expression_width

Control the maximum width for which (nested) [SynExpr.DotGet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#DotGet) expressions should be in one line.
Default = 50.

`defaultConfig`

*)


let job =
    JobBuilder
        .UsingJobData(jobDataMap)
        .Create<WrapperJob>()
        .Build()


{ defaultConfig with MaxDotGetExpressionWidth = 100 }


let job =
    JobBuilder.UsingJobData(jobDataMap).Create<WrapperJob>().Build()


(**
### fsharp_multiline_block_brackets_on_same_column

Alternative way of formatting records, arrays and lists. This will align the braces at the same column level.
Default = false.

`defaultConfig`

*)


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


{ defaultConfig with MultilineBlockBracketsOnSameColumn = true }


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


(**
### fsharp_newline_between_type_definition_and_members

Adds a new line between a type definition and its first member.
Default = false.

`defaultConfig`

*)


type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From


{ defaultConfig with NewlineBetweenTypeDefinitionAndMembers = true }


type Range =
    { From: float
      To: float }

    member this.Length = this.To - this.From


(**
### fsharp_align_function_signature_to_indentation

When a function signature exceeds the `max_line_length`, Fantomas will put all parameters on separate lines.
This setting also places the equals sign and return type on a new line.
Default = false.

`defaultConfig`

*)


[<FunctionName("FormatCode")>]
let run 
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req


{ defaultConfig with AlignFunctionSignatureToIndentation = true }


[<FunctionName("FormatCode")>]
let run
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse
    =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req


(**
### fsharp_alternative_long_member_definitions

Provides an alternative way of formatting long member and constructor definitions,
where the difference is mainly in the equal sign and returned type placement.
Default = false.

`defaultConfig`

*)


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


{ defaultConfig with AlternativeLongMemberDefinitions = true }


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


(**
### fsharp_multi_line_lambda_closing_newline

Places the closing parenthesis of a multiline lambda argument on the next line.
Default = false.

`defaultConfig`

*)


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


{ defaultConfig with MultiLineLambdaClosingNewline = true }


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


(**
### fsharp_experimental_keep_indent_in_branch

Breaks the normal indentation flow for the last branch of a pattern match or if/then/else expression.
Only when the pattern match or if/then/else is the return value of a function or member.

*This feature is considered experimental and is subject to change*

`defaultConfig`

*)


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


{ defaultConfig with KeepIndentInBranch = true }


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


(**
### fsharp_blank_lines_around_nested_multiline_expressions

Surround **nested** multi-line expressions with blank lines.
Existing blank lines are always preserved (via trivia), with exception when [fsharp_keep_max_number_of_blank_lines](#fsharp_keep_max_number_of_blank_lines) is used.  
Top level expressions will always follow the [2020 blank lines revision](https://github.com/fsprojects/fantomas/blob/master/docs/FormattingConventions.md#2020-revision) principle.
Default = true.

`defaultConfig`

*)


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


{ defaultConfig with BlankLinesAroundNestedMultilineExpressions = false }


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

(**
### fsharp_bar_before_discriminated_union_declaration

Always use a `|` before every case in the declaration of a discriminated union. If `false`, a `|` character is used only in multiple-case discriminated unions, and is omitted in short single-case DUs.
Default = false.

*)

type MyDU = Short of int


{ defaultConfig with BarBeforeDiscriminatedUnionDeclaration = true }


type MyDU = | Short of int

(**
### fsharp_experimental_stroustrup_style

Please contribute to https://github.com/fsprojects/fantomas/issues/1408.

### fsharp_keep_max_number_of_blank_lines

Set maximal number of consecutive blank lines to keep from original source. It doesn't change number of new blank lines generated by Fantomas.
Default=100

`defaultConfig`
*)

open Foo


let x = 42


{ defaultConfig with KeepMaxNumberOfBlankLines = 1 }


open Foo

let x = 42


(**
### fsharp_strict_mode

If being set, pretty printing is only done via ASTs. Compiler directives, inline comments and block comments will be ignored.
There are numerous situations when the information in the AST alone cannot restore the original code.
**Please do not use this setting for formatting hand written code!**
Valid use-case of this settings is code generation in projects like [FsAst](https://github.com/ionide/FsAst) and [Myriad](https://github.com/MoiraeSoftware/myriad).
Default = false.

`hand written code`
*)

// some great comment
let add a b =
#if INTERACTIVE
    42
#else
    a + b
#endif


{ defaultConfig with StrictMode = true }


let add a b = a + b

(**
<div class="d-flex justify-content-between my-4">
  <a href="./StyleGuide.html">Previous</a>
  <a href="./IgnoreFiles.html">Next</a>
</div>
*)