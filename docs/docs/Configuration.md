---
category: End-user documentation
categoryindex: 1
index: 5
---
# Configuration

Fantomas ships with a series of format options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the commandline tool.

<!-- 
    This index can be auto-generated with VS Code's Markdown All in One extension.
    The ToC will be updated-on-save, or can be generated on command by using
    Ctrl-Shift-P: "Create table of contents". 
    More info: https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one#table-of-contents 
-->
## Index<!-- omit in toc -->
- [Configuration](#configuration)
- [Default `.editorconfig` settings](#default-editorconfig-settings)
- [Each setting explained](#each-setting-explained)
  - [indent\_size](#indent_size)
  - [max\_line\_length](#max_line_length)
  - [end\_of\_line](#end_of_line)
  - [insert\_final\_newline](#insert_final_newline)
  - [fsharp\_space\_before\_parameter](#fsharp_space_before_parameter)
  - [fsharp\_space\_before\_lowercase\_invocation](#fsharp_space_before_lowercase_invocation)
  - [fsharp\_space\_before\_uppercase\_invocation](#fsharp_space_before_uppercase_invocation)
  - [fsharp\_space\_before\_class\_constructor](#fsharp_space_before_class_constructor)
  - [fsharp\_space\_before\_member](#fsharp_space_before_member)
  - [fsharp\_space\_before\_colon](#fsharp_space_before_colon)
  - [fsharp\_space\_after\_comma](#fsharp_space_after_comma)
  - [fsharp\_space\_before\_semicolon](#fsharp_space_before_semicolon)
  - [fsharp\_space\_after\_semicolon](#fsharp_space_after_semicolon)
  - [fsharp\_space\_around\_delimiter](#fsharp_space_around_delimiter)
  - [fsharp\_max\_if\_then\_short\_width](#fsharp_max_if_then_short_width)
  - [fsharp\_max\_if\_then\_else\_short\_width](#fsharp_max_if_then_else_short_width)
  - [fsharp\_max\_infix\_operator\_expression](#fsharp_max_infix_operator_expression)
  - [fsharp\_max\_record\_width](#fsharp_max_record_width)
  - [fsharp\_max\_record\_number\_of\_items](#fsharp_max_record_number_of_items)
  - [fsharp\_record\_multiline\_formatter](#fsharp_record_multiline_formatter)
  - [fsharp\_max\_array\_or\_list\_width](#fsharp_max_array_or_list_width)
  - [fsharp\_max\_array\_or\_list\_number\_of\_items](#fsharp_max_array_or_list_number_of_items)
  - [fsharp\_array\_or\_list\_multiline\_formatter](#fsharp_array_or_list_multiline_formatter)
  - [fsharp\_max\_value\_binding\_width](#fsharp_max_value_binding_width)
  - [fsharp\_max\_function\_binding\_width](#fsharp_max_function_binding_width)
  - [fsharp\_max\_dot\_get\_expression\_width](#fsharp_max_dot_get_expression_width)
  - [fsharp\_multiline\_block\_brackets\_on\_same\_column](#fsharp_multiline_block_brackets_on_same_column)
  - [fsharp\_newline\_between\_type\_definition\_and\_members](#fsharp_newline_between_type_definition_and_members)
  - [fsharp\_align\_function\_signature\_to\_indentation](#fsharp_align_function_signature_to_indentation)
  - [fsharp\_alternative\_long\_member\_definitions](#fsharp_alternative_long_member_definitions)
  - [fsharp\_multi\_line\_lambda\_closing\_newline](#fsharp_multi_line_lambda_closing_newline)
  - [fsharp\_experimental\_keep\_indent\_in\_branch](#fsharp_experimental_keep_indent_in_branch)
  - [fsharp\_blank\_lines\_around\_nested\_multiline\_expressions](#fsharp_blank_lines_around_nested_multiline_expressions)
  - [fsharp\_bar\_before\_discriminated\_union\_declaration](#fsharp_bar_before_discriminated_union_declaration)
  - [fsharp\_experimental\_stroustrup\_style](#fsharp_experimental_stroustrup_style)
  - [fsharp\_keep\_max\_number\_of\_blank\_lines](#fsharp_keep_max_number_of_blank_lines)
  - [fsharp\_strict\_mode](#fsharp_strict_mode)

# Default `.editorconfig` settings

A default .editorconfig file would look like:

```ini
[*.{fs,fsx}]
indent_size=4
max_line_length=120
end_of_line=crlf
insert_final_newline=true
fsharp_space_before_parameter=true
fsharp_space_before_lowercase_invocation=true
fsharp_space_before_uppercase_invocation=false
fsharp_space_before_class_constructor=false
fsharp_space_before_member=false
fsharp_space_before_colon=false
fsharp_space_after_comma=true
fsharp_space_before_semicolon=false
fsharp_space_after_semicolon=true
fsharp_space_around_delimiter=true
fsharp_max_if_then_short_width=0
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
fsharp_align_function_signature_to_indentation=false
fsharp_alternative_long_member_definitions=false
fsharp_multi_line_lambda_closing_newline=false
fsharp_experimental_keep_indent_in_branch=false
fsharp_blank_lines_around_nested_multiline_expressions=true
fsharp_bar_before_discriminated_union_declaration=false
fsharp_experimental_stroustrup_style=false
fsharp_keep_max_number_of_blank_lines=100
fsharp_strict_mode=false
```

Please note that you should only add settings to the `.editorconfig` file when you want to deviate from the default settings.
Copying the entire list above is unnecessary.

# Each setting explained

<a id="indent_size"></a>
## indent\_size

`indent_size` has to be between 1 and 10.

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

<a id="max_line_length"></a>
## max\_line\_length

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

<a id="end_of_line"></a>
## end\_of\_line

`end_of_line` determines the newline character, `lf` will add `\n` where `crlf` will add `\r\n`.
`cr` is not supported by the F# language spec.
If not set by the user, the default value is determined by `System.Environment.NewLine`.

<a id="insert_final_newline"></a>
## insert\_final\_newline

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

<a id="fsharp_space_before_parameter"></a>
## fsharp\_space\_before\_parameter

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

<a id="fsharp_space_before_lowercase_invocation"></a>
## fsharp\_space\_before\_lowercase\_invocation

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

<a id="fsharp_space_before_uppercase_invocation"></a>
## fsharp\_space\_before\_uppercase\_invocation

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

<a id="fsharp_space_before_class_constructor"></a>
## fsharp\_space\_before\_class\_constructor

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

<a id="fsharp_space_before_member"></a>
## fsharp\_space\_before\_member

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

<a id="fsharp_space_before_colon"></a>
## fsharp\_space\_before\_colon

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

<a id="fsharp_space_after_comma"></a>
## fsharp\_space\_after\_comma

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

<a id="fsharp_space_before_semicolon"></a>
## fsharp\_space\_before\_semicolon

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

<a id="fsharp_space_after_semicolon"></a>
## fsharp\_space\_after\_semicolon

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

<a id="fsharp_space_around_delimiter"></a>
## fsharp\_space\_around\_delimiter

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
<a id="fsharp_max_if_then_short_width"></a>
## fsharp\_max\_if\_then\_short\_width

Control the maximum length for which if/then expression without an else expression can be on one line.  
The [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions) recommends to never write such an expression in one line.
> If the else expression is absent, it is recommended to never to write the entire expression in one line.
Default = 0.

`defaultConfig`

```fsharp
if a then 
    ()
```

`{ defaultConfig with MaxIfThenShortWidth = 15 }`

```fsharp
if a then ()
```

<a id="fsharp_max_if_then_else_short_width"></a>
## fsharp\_max\_if\_then\_else\_short\_width

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

<a id="fsharp_max_infix_operator_expression"></a>
## fsharp\_max\_infix\_operator\_expression

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

<a id="fsharp_max_record_width"></a>
## fsharp\_max\_record\_width

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

<a id="fsharp_max_record_number_of_items"></a>
## fsharp\_max\_record\_number\_of\_items

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

<a id="fsharp_record_multiline_formatter"></a>
## fsharp\_record\_multiline\_formatter

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

<a id="fsharp_max_array_or_list_width"></a>
## fsharp\_max\_array\_or\_list\_width

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

<a id="fsharp_max_array_or_list_number_of_items"></a>
## fsharp\_max\_array\_or\_list\_number\_of\_items

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

<a id="fsharp_array_or_list_multiline_formatter"></a>
## fsharp\_array\_or\_list\_multiline\_formatter

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

<a id="fsharp_max_value_binding_width"></a>
## fsharp\_max\_value\_binding\_width

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

<a id="fsharp_max_function_binding_width"></a>
## fsharp\_max\_function\_binding\_width

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

<a id="fsharp_max_dot_get_expression_width"></a>
## fsharp\_max\_dot\_get\_expression\_width

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

<a id="fsharp_multiline_block_brackets_on_same_column"></a>
## fsharp\_multiline\_block\_brackets\_on\_same\_column

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

<a id="fsharp_newline_between_type_definition_and_members"></a>
## fsharp\_newline\_between\_type\_definition\_and\_members

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

<a id="fsharp_align_function_signature_to_indentation"></a>
## fsharp\_align\_function\_signature\_to\_indentation

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

<a id="fsharp_alternative_long_member_definitions"></a>
## fsharp\_alternative\_long\_member\_definitions

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

<a id="fsharp_multi_line_lambda_closing_newline"></a>
## fsharp\_multi\_line\_lambda\_closing\_newline

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

<a id="fsharp_experimental_keep_indent_in_branch"></a>
## fsharp\_experimental\_keep\_indent\_in\_branch

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

<a id="fsharp_blank_lines_around_nested_multiline_expressions"></a>
## fsharp\_blank\_lines\_around\_nested\_multiline\_expressions

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

<a id="fsharp_bar_before_discriminated_union_declaration"></a>
## fsharp\_bar\_before\_discriminated\_union\_declaration

Always use a `|` before every case in the declaration of a discriminated union. If `false`, a `|` character is used only in multiple-case discriminated unions, and is omitted in short single-case DUs.
Default = false.

```fsharp
type MyDU = Short of int
```

`{ defaultConfig with BarBeforeDiscriminatedUnionDeclaration = true }`

```fsharp
type MyDU = | Short of int
```

<a id="fsharp_experimental_stroustrup_style"></a>
## fsharp\_experimental\_stroustrup\_style

Please contribute to https://github.com/fsprojects/fantomas/issues/1408.

<a id="fsharp_keep_max_number_of_blank_lines"></a>
## fsharp\_keep\_max\_number\_of\_blank\_lines

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


<a id="fsharp_strict_mode"></a>
## fsharp\_strict\_mode

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
