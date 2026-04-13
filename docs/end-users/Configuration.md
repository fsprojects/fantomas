<link rel="stylesheet" type="text/css" href="https://fsprojects.github.io/fantomas/content/configuration.css" />
# Configuration

Fantomas ships with a limited series of options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the
commandline.
Your IDE should respect your settings, however the implementation of that is editor specific. Setting the configuration via
UI might be available depending on the IDE.

```
version: 8.0.0-alpha-009+73c57668e2d1ba026a7c418676fad39acd32518e
```

## Usage

Inside .editorconfig you can specify the file extension and code location to be use per config:

```fsharp
[*.fs]
fsharp_space_before_uppercase_invocation = true

#\ Write a comment by starting the line with a '#'
[*.{fs,fsx,fsi}]
fsharp_bar_before_discriminated_union_declaration = true

#\ Apply specific settings for a targeted subfolder
[src/Elmish/View.fs]
fsharp_multiline_bracket_style = stroustrup

```

## Trying your settings via the online tool

You can quickly try your settings via the <a href="https://fsprojects.github.io/fantomas-tools/#/fantomas/preview" target="_blank">online tool</a>.

<img src="https://fsprojects.github.io/fantomas//online_tool_usage.gif" alt="drawing" width="100%"/>
## Settings recommendations

Fantomas ships with a series of settings that you can use freely depending  on your case.
However, there are settings that we do not recommend and generally should not be used.
<p><fantomas-setting green></fantomas-setting><strong>Safe to change:</strong> Settings that aren't attached to any guidelines. Depending on your team or your own preferences, feel free to change these as it's been agreed on the codebase, however, you can always use it's defaults.</p>
<p><fantomas-setting orange></fantomas-setting><strong>Use with caution:</strong> Settings where it is not recommended to change the default value. They might lead to incomplete results.</p>
<p><fantomas-setting red></fantomas-setting><strong>Do not use:</strong> Settings that don't follow any guidelines.</p>
<p><fantomas-setting gr="gr"></fantomas-setting><strong>G-Research:</strong> G-Research styling guide. If you use one of these, for consistency reasons you should use all of them.</p>
<p><copy-to-clipboard></copy-to-clipboard><strong>Copy button:</strong> This copies the `.editorconfig` setting text you need to change the default. ⚠️ The copied text will not contain the default value.

## Auxiliary settings

<fantomas-setting orange></fantomas-setting>
### indent_size

<copy-to-clipboard text="indent_size = 2"></copy-to-clipboard>
`indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4.
The same indentation is ensured to be consistent in a source file.

```
# Default
indent_size = 4
```

```fsharp
formatCode
    """ 
    let inline selectRandom (f: _ []) =
      let r = random 1.0

      let rec find =
          function
          | 0 -> fst f.[0]
          | n when r < snd f.[n] -> fst f.[n]
          | n -> find (n - 1)

      find <| f.Length - 1
    """
    """
indent_size = 2
    """
```

```
let inline selectRandom (f: _[]) =
  let r = random 1.0

  let rec find =
    function
    | 0 -> fst f.[0]
    | n when r < snd f.[n] -> fst f.[n]
    | n -> find (n - 1)

  find <| f.Length - 1
```

<fantomas-setting green></fantomas-setting>
### max_line_length

<copy-to-clipboard text="max_line_length = 100"></copy-to-clipboard>
`max_line_length` has to be an integer greater or equal to 60.
This preference sets the column where we break F# constructs into new lines.

```
# Default
max_line_length = 120
```

```fsharp
formatCode
    """ 
    match myValue with
    | Some foo -> someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
    | None -> printfn "nothing"
    """
    """
max_line_length = 60
    """
```

```
match myValue with
| Some foo ->
    someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
| None -> printfn "nothing"
```

<fantomas-setting green></fantomas-setting>
### end_of_line

<copy-to-clipboard text="end_of_line = lf"></copy-to-clipboard>
`end_of_line` determines the newline character, `lf` will add `\n` where `crlf` will add `\r\n`.
`cr` is not supported by the F# language spec.
If not set by the user, the default value is determined by `System.Environment.NewLine`.

<fantomas-setting orange></fantomas-setting>
### insert_final_newline

<copy-to-clipboard text="insert_final_newline = false"></copy-to-clipboard>
Adds a final newline character at the end of the file.
<a href="https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline" target="_blank">Why should text files end with a newline?</a>

```
# Default
insert_final_newline = true
```

```fsharp
formatCode
    """ 
    let a = 42
    """
    """
insert_final_newline = false
    """
```

```
let a = 42
```

<fantomas-setting orange></fantomas-setting>
### fsharp_space_before_parameter

<copy-to-clipboard text="fsharp_space_before_parameter = false"></copy-to-clipboard>
Add a space after the name of a function and before the opening parenthesis of the first parameter.
This setting influences function definitions.

```
# Default
fsharp_space_before_parameter = true
```

```fsharp
formatCode
    """ 
   let value (a: int) = x
   let DumpTrace() = ()
    """
    """
fsharp_space_before_parameter = false
    """
```

```
let value(a: int) = x
let DumpTrace() = ()
```

<fantomas-setting orange></fantomas-setting>
### fsharp_space_before_lowercase_invocation

<copy-to-clipboard text="fsharp_space_before_lowercase_invocation = false"></copy-to-clipboard>
Add a space after the name of a lowercased function and before the opening parenthesis of the first argument.
This setting influences function invocation in expressions and patterns.

```
# Default
fsharp_space_before_lowercase_invocation = true
```

```fsharp
formatCode
    """ 
value (a, b)
startTimer ()

match x with
| value (a, b) -> ()
    """
    """
fsharp_space_before_lowercase_invocation = false
    """
```

```
value(a, b)
startTimer()

match x with
| value(a, b) -> ()
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_space_before_uppercase_invocation

<copy-to-clipboard text="fsharp_space_before_uppercase_invocation = true"></copy-to-clipboard>
Add a space after the name of a uppercase function and before the opening parenthesis of the first argument.
This setting influences function invocation in expressions and patterns.

```
# Default
fsharp_space_before_uppercase_invocation = false
```

```fsharp
formatCode
    """ 
Value(a, b)
person.ToString()

match x with
| Value(a, b) -> ()
    """
    """
fsharp_space_before_uppercase_invocation = true
    """
```

```
Value (a, b)
person.ToString ()

match x with
| Value (a, b) -> ()
```

<fantomas-setting orange></fantomas-setting>
### fsharp_space_before_class_constructor

<copy-to-clipboard text="fsharp_space_before_class_constructor = true"></copy-to-clipboard>
Add a space after a type name and before the class constructor.

```
# Default
fsharp_space_before_class_constructor = false
```

```fsharp
formatCode
    """ 
    type Person() =
        class
        end
    """
    """
fsharp_space_before_class_constructor = true
    """
```

```
type Person () = class end
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_space_before_member

<copy-to-clipboard text="fsharp_space_before_member = true"></copy-to-clipboard>
Add a space after a member name and before the opening parenthesis of the first parameter.

```
# Default
fsharp_space_before_member = false
```

```fsharp
formatCode
    """ 
    type Person() =
        member this.Walk(distance: int) = ()
        member this.Sleep() = ignore
        member __.singAlong() = ()
        member __.swim(duration: TimeSpan) = ()
    """
    """
fsharp_space_before_member = true
    """
```

```
type Person() =
    member this.Walk (distance: int) = ()
    member this.Sleep () = ignore
    member __.singAlong () = ()
    member __.swim (duration: TimeSpan) = ()
```

<fantomas-setting green></fantomas-setting>
### fsharp_space_before_colon

<copy-to-clipboard text="fsharp_space_before_colon = true"></copy-to-clipboard>
Add a space before `:`. Please note that not every `:` is controlled by this setting.

```
# Default
fsharp_space_before_colon = false
```

```fsharp
formatCode
    """ 
   type Point = { x: int; y: int }
   let myValue: int = 42
   let update (msg: Msg) (model: Model) : Model = model
    """
    """
fsharp_space_before_colon = true
    """
```

```
type Point = { x : int; y : int }
let myValue : int = 42
let update (msg : Msg) (model : Model) : Model = model
```

<fantomas-setting orange></fantomas-setting>
### fsharp_space_after_comma

<copy-to-clipboard text="fsharp_space_after_comma = false"></copy-to-clipboard>
Adds a space after `,` in tuples.

```
# Default
fsharp_space_after_comma = true
```

```fsharp
formatCode
    """ 
    myValue.SomeFunction(foo, bar, somethingElse)
    (a, b, c)
    """
    """
fsharp_space_after_comma = false
    """
```

```
myValue.SomeFunction(foo,bar,somethingElse)
(a,b,c)
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_space_before_semicolon

<copy-to-clipboard text="fsharp_space_before_semicolon = true"></copy-to-clipboard>
Adds a space before `;` in records, arrays, lists, etc.

```
# Default
fsharp_space_before_semicolon = false
```

```fsharp
formatCode
    """ 
    let a = [ 1 ; 2 ; 3 ]
    let b = [| foo ; bar |]
    type C = { X: int ; Y: int }
    """
    """
fsharp_space_before_semicolon = true
    """
```

```
let a = [ 1 ; 2 ; 3 ]
let b = [| foo ; bar |]
type C = { X: int ; Y: int }
```

<fantomas-setting orange></fantomas-setting>
### fsharp_space_after_semicolon

<copy-to-clipboard text="fsharp_space_after_semicolon = false"></copy-to-clipboard>
Adds a space after `;` in records, arrays, lists, etc.

```
# Default
fsharp_space_after_semicolon = true
```

```fsharp
formatCode
    """ 
    let a = [ 1; 2; 3 ]
    let b = [| foo; bar |]
    type C = { X: int; Y: int }
    """
    """
fsharp_space_after_semicolon = false
    """
```

```
let a = [ 1;2;3 ]
let b = [| foo;bar |]
type C = { X: int;Y: int }
```

<fantomas-setting orange></fantomas-setting>
### fsharp_space_around_delimiter

<copy-to-clipboard text="fsharp_space_around_delimiter = false"></copy-to-clipboard>
Adds a space around delimiters like `[`,`[|`,{`.

```
# Default
fsharp_space_around_delimiter = true
```

```fsharp
formatCode
    """ 
    let a = [ 1;2;3 ]
    let b = [| 4;5;6 |]
    """
    """
fsharp_space_around_delimiter = false
    """
```

```
let a = [1; 2; 3]
let b = [|4; 5; 6|]
```

## Maximum width constraints

Settings that control the max width of certain expressions.

<fantomas-setting orange></fantomas-setting>
### fsharp_max_if_then_short_width

<copy-to-clipboard text="fsharp_max_if_then_short_width = 15"></copy-to-clipboard>
Control the maximum length for which if/then expression without an else expression can be on one line.
The [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions) recommends to never write such an expression in one line.

> If the else expression is absent, it is recommended to never to write the entire expression in one line.
> 

```
# Default
fsharp_max_if_then_short_width = 0
```

```fsharp
formatCode
    """ 
    if a then 
        ()
    """
    """
fsharp_max_if_then_short_width = 15
    """
```

```
if a then ()
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_if_then_else_short_width

<copy-to-clipboard text="fsharp_max_if_then_else_short_width = 80"></copy-to-clipboard>
Fantomas by default follows the if/then/else conventions listed in the [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions).
This setting facilitates this by determining the maximum character width where the if/then/else expression stays in one line.

```
# Default
fsharp_max_if_then_else_short_width = 60
```

```fsharp
formatCode
    """ 
    if myCheck then truth else bogus
    """
    """
fsharp_max_if_then_else_short_width = 10
    """
```

```
if myCheck then
    truth
else
    bogus
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_infix_operator_expression

<copy-to-clipboard text="fsharp_max_infix_operator_expression = 100"></copy-to-clipboard>
Control the maximum length for which infix expression can be on one line.

```
# Default
fsharp_max_infix_operator_expression = 80
```

```fsharp
formatCode
    """ 
    let WebApp =
        route "/ping" >=> authorized >=> text "pong"
    """
    """
fsharp_max_infix_operator_expression = 20
    """
```

```
let WebApp =
    route "/ping"
    >=> authorized
    >=> text "pong"
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_record_width

<copy-to-clipboard text="fsharp_max_record_width = 60"></copy-to-clipboard>
Control the maximum width for which records should be in one line.

Requires `fsharp_record_multiline_formatter` to be `character_width` to take effect.

```
# Default
fsharp_max_record_width = 40
```

```fsharp
formatCode
    """ 
    type MyRecord = { X: int; Y: int; Length: int }
    let myInstance = { X = 10; Y = 20; Length = 90 }
    """
    """
fsharp_max_record_width = 20
    """
```

```
type MyRecord =
    {
        X: int
        Y: int
        Length: int
    }

let myInstance =
    {
        X = 10
        Y = 20
        Length = 90
    }
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_record_number_of_items

<copy-to-clipboard text="fsharp_max_record_number_of_items = 2"></copy-to-clipboard>
Control the maximum number of fields for which records should be in one line.

Requires `fsharp_record_multiline_formatter` to be
`number_of_items` to take effect.

```
# Default
fsharp_max_record_number_of_items = 1
```

```fsharp
formatCode
    """ 
    type R = { x: int }

    type S = { x: int; y: string }

    type T = { x: int; y: string; z: float }

    let myRecord = { r = 3 }

    let myRecord' = { r with x = 3 }

    let myRecord'' = { r with x = 3; y = "hello" }

    let myRecord''' = { r with x = 3; y = "hello"; z = 0.0 }
    """
    """
fsharp_record_multiline_formatter = number_of_items
fsharp_max_record_number_of_items = 2
    """
```

```
type R = { x: int }

type S = { x: int; y: string }

type T =
    {
        x: int
        y: string
        z: float
    }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' = { r with x = 3; y = "hello" }

let myRecord''' =
    { r with
        x = 3
        y = "hello"
        z = 0.0
    }
```

<fantomas-setting green></fantomas-setting>
### fsharp_record_multiline_formatter

<copy-to-clipboard text="fsharp_record_multiline_formatter = number_of_items"></copy-to-clipboard>
Split records expressions/statements into multiple lines based on the given condition.
`character_width` uses character count of the expression, controlled by `fsharp_max_record_width`.
`number_of_items` uses the number of fields in the record, controlled by `fsharp_max_record_number_of_items`.

Note that in either case, record expressions/statements are still governed by `max_line_length`.

```
# Default
fsharp_record_multiline_formatter = character_width
```

```fsharp
formatCode
    """ 
    type R = { x: int }

    type S = { x: int; y: string }

    let myRecord = { r = 3 }

    let myRecord' = { r with x = 3 }

    let myRecord'' = { r with x = 3; y = "hello" }
    """
    """
fsharp_record_multiline_formatter = number_of_items
    """
```

```
type R = { x: int }

type S =
    {
        x: int
        y: string
    }

let myRecord = { r = 3 }

let myRecord' = { r with x = 3 }

let myRecord'' =
    { r with
        x = 3
        y = "hello"
    }
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_array_or_list_width

<copy-to-clipboard text="fsharp_max_array_or_list_width = 100"></copy-to-clipboard>
Control the maximum width for which lists and arrays can be in one line.

Requires `fsharp_array_or_list_multiline_formatter` to be `character_width` to take effect

```
# Default
fsharp_max_array_or_list_width = 80
```

```fsharp
formatCode
    """ 
    let myArray = [| one; two; three |]
    """
    """
fsharp_max_array_or_list_width = 20
    """
```

```
let myArray =
    [|
        one
        two
        three
    |]
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_array_or_list_number_of_items

<copy-to-clipboard text="fsharp_max_array_or_list_number_of_items = 2"></copy-to-clipboard>
Control the maximum number of elements for which lists and arrays can be in one line.

Requires `fsharp_array_or_list_multiline_formatter` to be `number_of_items` to take effect.

```
# Default
fsharp_max_array_or_list_number_of_items = 1
```

```fsharp
formatCode
    """ 
    let myList = [ one; two ]
    let myArray = [| one; two; three |]
    """
    """
fsharp_array_or_list_multiline_formatter = number_of_items
fsharp_max_array_or_list_number_of_items = 2
    """
```

```
let myList = [ one; two ]

let myArray =
    [|
        one
        two
        three
    |]
```

<fantomas-setting green></fantomas-setting>
### fsharp_array_or_list_multiline_formatter

<copy-to-clipboard text="fsharp_array_or_list_multiline_formatter = number_of_items"></copy-to-clipboard>
Split arrays and lists into multiple lines based on the given condition.
`character_width` uses character count of the expression, controlled by `fsharp_max_array_or_list_width`.
`number_of_items` uses the number of elements in the array or list, controlled by `fsharp_max_array_or_list_number_of_items`.

Note that in either case, list expressions are still governed by `max_line_length`.

```
# Default
fsharp_array_or_list_multiline_formatter = character_width
```

```fsharp
formatCode
    """ 
    let myArray = [| one; two; three |]
    """
    """
fsharp_array_or_list_multiline_formatter = number_of_items
    """
```

```
let myArray =
    [|
        one
        two
        three
    |]
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_value_binding_width

<copy-to-clipboard text="fsharp_max_value_binding_width = 100"></copy-to-clipboard>
Control the maximum expression width for which let and member value/property bindings should be in one line.
The width is that of the pattern for the binding plus the right-hand expression but not the keywords (e.g. "let").

```
# Default
fsharp_max_value_binding_width = 80
```

```fsharp
formatCode
    """ 
    let title = "Great title of project"
    type MyType() =
        member this.HelpText = "Some help text"
    """
    """
fsharp_max_value_binding_width = 10
    """
```

```
let title =
    "Great title of project"

type MyType() =
    member this.HelpText =
        "Some help text"
```

<fantomas-setting green></fantomas-setting>
### fsharp_max_function_binding_width

<copy-to-clipboard text="fsharp_max_function_binding_width = 40"></copy-to-clipboard>
Control the maximum width for which function and member bindings should be in one line.
In contrast to `fsharp_max_value_binding_width`, only the right-hand side expression of the binding is measured.

```
# Default
fsharp_max_function_binding_width = 40
```

```fsharp
formatCode
    """ 
    let printScore score total = printfn "%i / %i" score total

    type Triangle() =
        member this.CalculateSurface(width: int, height: int) = width * height / 2
    """
    """
fsharp_max_function_binding_width = 10
    """
```

```
let printScore score total =
    printfn "%i / %i" score total

type Triangle() =
    member this.CalculateSurface(width: int, height: int) =
        width * height / 2
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_multiline_bracket_style

<copy-to-clipboard text="fsharp_multiline_bracket_style = stroustrup"></copy-to-clipboard>
`Cramped` Alternative way in F# to format brackets.
`Aligned` The default way of formatting records, arrays and lists. This will align the braces at the same column level.
`Stroustrup` Allow for easier reordering of members and keeping the code succinct.

```
# Default
fsharp_multiline_bracket_style = aligned
```

```fsharp
formatCode
    """ 
    let myRecord =
        { Level = 1
          Progress = "foo"
          Bar = "bar"
          Street = "Bakerstreet"
          Number = 42 }

    type Range =
        { From: float
          To: float
          FileName: string }

    let a =
        [| (1, 2, 3)
           (4, 5, 6)
           (7, 8, 9)
           (10, 11, 12)
           (13, 14, 15)
           (16, 17,18)
           (19, 20, 21) |]
    """
    """
fsharp_multiline_bracket_style = aligned
    """
```

```
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
        FileName: string
    }

let a =
    [|
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
        (10, 11, 12)
        (13, 14, 15)
        (16, 17, 18)
        (19, 20, 21)
    |]
```

```fsharp
formatCode
    """ 
    let myRecord =
        { Level = 1
          Progress = "foo"
          Bar = "bar"
          Street = "Bakerstreet"
          Number = 42 }

    type Range =
        { From: float
          To: float
          FileName: string }

    let a =
        [| (1, 2, 3)
           (4, 5, 6)
           (7, 8, 9)
           (10, 11, 12)
           (13, 14, 15)
           (16, 17,18)
           (19, 20, 21) |]
    """
    """
fsharp_multiline_bracket_style = stroustrup
    """
```

```
let myRecord = {
    Level = 1
    Progress = "foo"
    Bar = "bar"
    Street = "Bakerstreet"
    Number = 42
}

type Range = {
    From: float
    To: float
    FileName: string
}

let a = [|
    (1, 2, 3)
    (4, 5, 6)
    (7, 8, 9)
    (10, 11, 12)
    (13, 14, 15)
    (16, 17, 18)
    (19, 20, 21)
|]
```

<fantomas-setting green></fantomas-setting>
### fsharp_newline_before_multiline_computation_expression

<copy-to-clipboard text="fsharp_newline_before_multiline_computation_expression = false"></copy-to-clipboard>
Insert a newline before a computation expression that spans multiple lines

```
# Default
fsharp_newline_before_multiline_computation_expression = true
```

```fsharp
formatCode
    """ 
    let something =
        task {
            let! thing = otherThing ()
            return 5
        }
    """
    """
fsharp_newline_before_multiline_computation_expression = false
    """
```

```
let something = task {
    let! thing = otherThing ()
    return 5
}
```

## G-Research style

A series of settings requicolor="red" to conform with the [G-Research style guide](https://github.com/G-Research/fsharp-formatting-conventions).
From a consistency point of view, it is recommend to enable all these settings instead of cherry-picking a few.

<fantomas-setting green gr></fantomas-setting>
### fsharp_newline_between_type_definition_and_members

<copy-to-clipboard text="fsharp_newline_between_type_definition_and_members = false"></copy-to-clipboard>
Adds a new line between a type definition and its first member.

```
# Default
fsharp_newline_between_type_definition_and_members = true
```

```fsharp
formatCode
    """ 
type Range =
    { From: float
      To: float }

    member this.Length = this.To - this.From
    """
    """
fsharp_newline_between_type_definition_and_members = false
    """
```

```
type Range =
    {
        From: float
        To: float
    }

    member this.Length = this.To - this.From
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_align_function_signature_to_indentation

<copy-to-clipboard text="fsharp_align_function_signature_to_indentation = true"></copy-to-clipboard>
When a function signature exceeds the `max_line_length`, Fantomas will put all parameters on separate lines.
This setting also places the equals sign and return type on a new line.

```
# Default
fsharp_align_function_signature_to_indentation = false
```

```fsharp
formatCode
    """ 
[<FunctionName("FormatCode")>]
let run 
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
    """
    """
fsharp_align_function_signature_to_indentation = true
    """
```

```
[<FunctionName("FormatCode")>]
let run
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse
    =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_alternative_long_member_definitions

<copy-to-clipboard text="fsharp_alternative_long_member_definitions = true"></copy-to-clipboard>
Provides an alternative way of formatting long member and constructor definitions,
where the difference is mainly in the equal sign and returned type placement.

```
# Default
fsharp_alternative_long_member_definitions = false
```

```fsharp
formatCode
    """ 
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

type E() =
    new
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) = E()
    """
    """
fsharp_alternative_long_member_definitions = true
    """
```

```
type C
    (
        aVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
    )
    = class end

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

type E() =
    new
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        )
        =
        E()
```

<fantomas-setting green gr></fantomas-setting>
### fsharp_multi_line_lambda_closing_newline

<copy-to-clipboard text="fsharp_multi_line_lambda_closing_newline = true"></copy-to-clipboard>
Places the closing parenthesis of a multiline lambda argument on the next line.

```
# Default
fsharp_multi_line_lambda_closing_newline = false
```

```fsharp
formatCode
    """ 
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
    """
    """
fsharp_multi_line_lambda_closing_newline = true
    """
```

```
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

<fantomas-setting orange gr></fantomas-setting>
### fsharp_experimental_keep_indent_in_branch

<copy-to-clipboard text="fsharp_experimental_keep_indent_in_branch = true"></copy-to-clipboard>
Breaks the normal indentation flow for the last branch of a pattern match or if/then/else expression.
Only when the last pattern match or else branch was already at the same level of the entire match or if expression.

**This feature is experimental and is subject to change.**

```
# Default
fsharp_experimental_keep_indent_in_branch = false
```

```fsharp
formatCode
    """ 
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
    """
    """
fsharp_experimental_keep_indent_in_branch = true
    """
```

```
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

<fantomas-setting green gr></fantomas-setting>
### fsharp_bar_before_discriminated_union_declaration

<copy-to-clipboard text="fsharp_bar_before_discriminated_union_declaration = true"></copy-to-clipboard>
Always use a `|` before every case in the declaration of a discriminated union.
If `false`, a `|` character is used only in multiple-case discriminated unions, and is omitted in short single-case DUs.

```
# Default
fsharp_bar_before_discriminated_union_declaration = false
```

```fsharp
formatCode
    """ 
    type MyDU = Short of int
    """
    """
fsharp_bar_before_discriminated_union_declaration = true
    """
```

```
type MyDU = | Short of int
```

## Other

Some additional settings that don't fit into any style guide.

<fantomas-setting green></fantomas-setting>
### fsharp_blank_lines_around_nested_multiline_expressions

<copy-to-clipboard text="fsharp_blank_lines_around_nested_multiline_expressions = false"></copy-to-clipboard>
Surround **nested** multi-line expressions with blank lines.
Existing blank lines are always preserved (via trivia), with exception when [fsharp_keep_max_number_of_blank_lines](#fsharp_keep_max_number_of_blank_lines) is used.
Top level expressions will always follow the [2020 blank lines revision](https://github.com/fsprojects/fantomas/blob/main/docs-old/FormattingConventions.md#2020-revision) principle.

```
# Default
fsharp_blank_lines_around_nested_multiline_expressions = true
```

```fsharp
formatCode
    """ 
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
    """
    """
fsharp_blank_lines_around_nested_multiline_expressions = false
    """
```

```
let topLevelFunction () =
    printfn "Something to print"

    try
        nothing ()
    with ex ->
        splash ()
    ()

let secondTopLevelFunction () =
    // ...
    ()
```

<fantomas-setting green></fantomas-setting>
### fsharp_keep_max_number_of_blank_lines

<copy-to-clipboard text="fsharp_keep_max_number_of_blank_lines = 1"></copy-to-clipboard>
Set maximal number of consecutive blank lines to keep from original source. It doesn't change number of new blank lines generated by Fantomas.

```
# Default
fsharp_keep_max_number_of_blank_lines = 100
```

```fsharp
formatCode
    """ 
    open Foo

    let x = 42
    """
    """
fsharp_keep_max_number_of_blank_lines = 1
    """
```

```
open Foo

let x = 42
```

<fantomas-setting orange></fantomas-setting>
### fsharp_experimental_elmish

<copy-to-clipboard text="fsharp_experimental_elmish = true"></copy-to-clipboard>
Applies the Stroustrup style to the final (two) array or list argument(s) in a function application.
Note that this behaviour is also active when `fsharp_multiline_bracket_style = stroustrup`.

```
# Default
fsharp_experimental_elmish = false
```

```fsharp
formatCode
    """ 
let dualList =
    div
        []
        [
            h1 [] [ str "Some title" ]
            ul
                []
                [
                    for p in model.Points do
                        li [] [ str $"%i{p.X}, %i{p.Y}" ]
                ]
            hr []
        ]

let singleList =
    Html.div
        [
            Html.h1 [ str "Some title" ]
            Html.ul
                [
                    for p in model.Points do
                        Html.li [ str $"%i{p.X}, %i{p.Y}" ]
                ]
        ]
    """
    """
fsharp_experimental_elmish = true
    """
```

```
let dualList =
    div [] [
        h1 [] [ str "Some title" ]
        ul [] [
            for p in model.Points do
                li [] [ str $"%i{p.X}, %i{p.Y}" ]
        ]
        hr []
    ]

let singleList =
    Html.div [
        Html.h1 [ str "Some title" ]
        Html.ul [
            for p in model.Points do
                Html.li [ str $"%i{p.X}, %i{p.Y}" ]
        ]
    ]
```

<fantomas-nav previous="StyleGuide.md" next="IgnoreFiles.md"></fantomas-nav>