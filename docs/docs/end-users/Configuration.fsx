(**
---
category: End-users
categoryindex: 1
index: 3
---
*)

(**
<link rel="stylesheet" type="text/css" href="{{root}}content/configuration.css" />
*)

(**
# Configuration
Fantomas ships with a limited series of options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the
commandline.  
Your IDE should respect your settings, however the implementation of that is editor specific. Setting the configuration via
UI might be available depending on the IDE.
*)

(*** hide ***)
#r "../../../src/Fantomas/bin/Release/net6.0/Fantomas.FCS.dll"
#r "../../../src/Fantomas/bin/Release/net6.0/Fantomas.Core.dll"

printf $"version: {Fantomas.Core.CodeFormatter.GetVersion()}"
(*** include-output  ***)

(**
## Usage
Inside .editorconfig you can specify the file extension and code location to be use per config:
```
[*.fs]
fsharp_space_before_uppercase_invocation = true

#\ Write a comment by starting the line with a '#'
[*.{fs,fsx,fsi}]
fsharp_bar_before_discriminated_union_declaration = true

#\ Apply specific settings for a targeted subfolder
[src/Elmish/View.fs]
fsharp_multiline_bracket_style = stroustrup
```
*)

(**
## Trying your settings via the online tool
You can quickly try your settings via the <a href="https://fsprojects.github.io/fantomas-tools/#/fantomas/preview" target="_blank">online tool</a>.

<img src="{{root}}/online_tool_usage.gif" alt="drawing" width="100%"/>
*)

open Fantomas.Core

let formatCode input configIndent =
    async {
        let! result = CodeFormatter.FormatDocumentAsync(false, input, configIndent)
        printf $"%s{result.Code}"
    }
    |> Async.RunSynchronously

(**
## Settings recommendations
Fantomas ships with a series of settings that you can use freely depending  on your case.  
However, there are settings that we do not recommend and generally should not be used.
<p><fantomas-setting green></fantomas-setting><strong>Safe to change:</strong> Settings that aren't attached to any guidelines. Depending on your team or your own preferences, feel free to change these as it's been agreed on the codebase, however, you can always use it's defaults.</p>
<p><fantomas-setting orange></fantomas-setting><strong>Use with caution:</strong> Settings where it is not recommended to change the default value. They might lead to incomplete results.</p>
<p><fantomas-setting red></fantomas-setting><strong>Do not use:</strong> Settings that don't follow any guidelines.</p>
<p><fantomas-setting gr="gr"></fantomas-setting><strong>G-Research:</strong> G-Research styling guide. If you use one of these, for consistency reasons you should use all of them.</p>
<p><copy-to-clipboard></copy-to-clipboard><strong>Copy button:</strong> This copies the `.editorconfig` setting text you need to change the default. ⚠️ The copied text will not contain the default value.
*)

(**
## Auxiliary settings

<fantomas-setting orange></fantomas-setting>
### indent_size
<copy-to-clipboard text="indent_size = 2"></copy-to-clipboard>

`indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4.  
The same indentation is ensured to be consistent in a source file.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.IndentSize}"
(*** include-output ***)

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
    { FormatConfig.Default with
        IndentSize = 2 }
(*** include-output ***)
(**
<fantomas-setting green></fantomas-setting>
### max_line_length
<copy-to-clipboard text="max_line_length = 100"></copy-to-clipboard>

`max_line_length` has to be an integer greater or equal to 60.  
This preference sets the column where we break F# constructs into new lines.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxLineLength}"
(*** include-output ***)

formatCode
    """ 
    match myValue with
    | Some foo -> someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
    | None -> printfn "nothing"
    """
    { FormatConfig.Default with
        MaxLineLength = 60 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### end_of_line
<copy-to-clipboard text="end_of_line = lf"></copy-to-clipboard>

`end_of_line` determines the newline character, `lf` will add `\n` where `crlf` will add `\r\n`.  
`cr` is not supported by the F# language spec.  
If not set by the user, the default value is determined by `System.Environment.NewLine`.
*)

(**
<fantomas-setting orange></fantomas-setting>
### insert_final_newline
<copy-to-clipboard text="insert_final_newline = false"></copy-to-clipboard>

Adds a final newline character at the end of the file.  
<a href="https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline" target="_blank">Why should text files end with a newline?</a>
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.InsertFinalNewline.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    let a = 42
    """
    { FormatConfig.Default with
        InsertFinalNewline = false }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_space_before_parameter
<copy-to-clipboard text="fsharp_space_before_parameter = false"></copy-to-clipboard>

Add a space after the name of a function and before the opening parenthesis of the first parameter.  
This setting influences function definitions.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeParameter.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
   let value (a: int) = x
   let DumpTrace() = ()
    """
    { FormatConfig.Default with
        SpaceBeforeParameter = false }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_space_before_lowercase_invocation
<copy-to-clipboard text="fsharp_space_before_lowercase_invocation = false"></copy-to-clipboard>

Add a space after the name of a lowercased function and before the opening parenthesis of the first argument.  
This setting influences function invocation in expressions and patterns.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeLowercaseInvocation.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
value (a, b)
startTimer ()

match x with
| value (a, b) -> ()
    """
    { FormatConfig.Default with
        SpaceBeforeLowercaseInvocation = false }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_space_before_uppercase_invocation
<copy-to-clipboard text="fsharp_space_before_uppercase_invocation = true"></copy-to-clipboard>

Add a space after the name of a uppercase function and before the opening parenthesis of the first argument.  
This setting influences function invocation in expressions and patterns.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeUppercaseInvocation.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
Value(a, b)
person.ToString()

match x with
| Value(a, b) -> ()
    """
    { FormatConfig.Default with
        SpaceBeforeUppercaseInvocation = true }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_space_before_class_constructor
<copy-to-clipboard text="fsharp_space_before_class_constructor = true"></copy-to-clipboard>

Add a space after a type name and before the class constructor.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeClassConstructor.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    type Person() =
        class
        end
    """
    { FormatConfig.Default with
        SpaceBeforeClassConstructor = true }

(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_space_before_member
<copy-to-clipboard text="fsharp_space_before_member = true"></copy-to-clipboard>

Add a space after a member name and before the opening parenthesis of the first parameter.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeMember.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    type Person() =
        member this.Walk(distance: int) = ()
        member this.Sleep() = ignore
        member __.singAlong() = ()
        member __.swim(duration: TimeSpan) = ()
    """
    { FormatConfig.Default with
        SpaceBeforeMember = true }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_space_before_colon
<copy-to-clipboard text="fsharp_space_before_colon = true"></copy-to-clipboard>

Add a space before `:`. Please note that not every `:` is controlled by this setting.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeColon.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
   type Point = { x: int; y: int }
   let myValue: int = 42
   let update (msg: Msg) (model: Model) : Model = model
    """
    { FormatConfig.Default with
        SpaceBeforeColon = true }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_space_after_comma
<copy-to-clipboard text="fsharp_space_after_comma = false"></copy-to-clipboard>

Adds a space after `,` in tuples.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceAfterComma.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    myValue.SomeFunction(foo, bar, somethingElse)
    (a, b, c)
    """
    { FormatConfig.Default with
        SpaceAfterComma = false }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_space_before_semicolon
<copy-to-clipboard text="fsharp_space_before_semicolon = true"></copy-to-clipboard>

Adds a space before `;` in records, arrays, lists, etc.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceBeforeSemicolon.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    let a = [ 1 ; 2 ; 3 ]
    let b = [| foo ; bar |]
    type C = { X: int ; Y: int }
    """
    { FormatConfig.Default with
        SpaceBeforeSemicolon = true }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_space_after_semicolon
<copy-to-clipboard text="fsharp_space_after_semicolon = false"></copy-to-clipboard>

Adds a space after `;` in records, arrays, lists, etc.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceAfterSemicolon.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    let a = [ 1; 2; 3 ]
    let b = [| foo; bar |]
    type C = { X: int; Y: int }
    """
    { FormatConfig.Default with
        SpaceAfterSemicolon = false }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_space_around_delimiter
<copy-to-clipboard text="fsharp_space_around_delimiter = false"></copy-to-clipboard>

Adds a space around delimiters like `[`,`[|`,{`.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.SpaceAroundDelimiter.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    let a = [ 1;2;3 ]
    let b = [| 4;5;6 |]
    """
    { FormatConfig.Default with
        SpaceAroundDelimiter = false }
(*** include-output ***)

(**
## Maximum width constraints

Settings that control the max width of certain expressions.

<fantomas-setting orange></fantomas-setting>
### fsharp_max_if_then_short_width
<copy-to-clipboard text="fsharp_max_if_then_short_width = 15"></copy-to-clipboard>

Control the maximum length for which if/then expression without an else expression can be on one line.  
The [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions) recommends to never write such an expression in one line.

> If the else expression is absent, it is recommended to never to write the entire expression in one line.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxIfThenShortWidth}"
(*** include-output ***)

formatCode
    """ 
    if a then 
        ()
    """
    { FormatConfig.Default with
        MaxIfThenShortWidth = 15 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_if_then_else_short_width
<copy-to-clipboard text="fsharp_max_if_then_else_short_width = 80"></copy-to-clipboard>

Fantomas by default follows the if/then/else conventions listed in the [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions).  
This setting facilitates this by determining the maximum character width where the if/then/else expression stays in one line.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxIfThenElseShortWidth}"
(*** include-output ***)

formatCode
    """ 
    if myCheck then truth else bogus
    """
    { FormatConfig.Default with
        MaxIfThenElseShortWidth = 10 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_infix_operator_expression
<copy-to-clipboard text="fsharp_max_infix_operator_expression = 100"></copy-to-clipboard>

Control the maximum length for which infix expression can be on one line.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxInfixOperatorExpression}"
(*** include-output ***)

formatCode
    """ 
    let WebApp =
        route "/ping" >=> authorized >=> text "pong"
    """
    { FormatConfig.Default with
        MaxInfixOperatorExpression = 20 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_record_width
<copy-to-clipboard text="fsharp_max_record_width = 60"></copy-to-clipboard>

Control the maximum width for which records should be in one line.

Requires `fsharp_record_multiline_formatter` to be `character_width` to take effect.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxRecordWidth}"
(*** include-output ***)

formatCode
    """ 
    type MyRecord = { X: int; Y: int; Length: int }
    let myInstance = { X = 10; Y = 20; Length = 90 }
    """
    { FormatConfig.Default with
        MaxRecordWidth = 20 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_record_number_of_items
<copy-to-clipboard text="fsharp_max_record_number_of_items = 2"></copy-to-clipboard>

Control the maximum number of fields for which records should be in one line.

Requires `fsharp_record_multiline_formatter` to be
`number_of_items` to take effect.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxRecordNumberOfItems}"
(*** include-output ***)

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
    { FormatConfig.Default with
        MaxRecordNumberOfItems = 2
        RecordMultilineFormatter = MultilineFormatterType.NumberOfItems }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_record_multiline_formatter
<copy-to-clipboard text="fsharp_record_multiline_formatter = number_of_items"></copy-to-clipboard>

Split records expressions/statements into multiple lines based on the given condition.  
`character_width` uses character count of the expression, controlled by `fsharp_max_record_width`.  
`number_of_items` uses the number of fields in the record, controlled by `fsharp_max_record_number_of_items`.

Note that in either case, record expressions/statements are still governed by `max_line_length`.
*)

(*** hide ***)
printfn $"Default = {MultilineFormatterType.ToConfigString FormatConfig.Default.RecordMultilineFormatter}"
(*** include-output ***)

formatCode
    """ 
    type R = { x: int }

    type S = { x: int; y: string }

    let myRecord = { r = 3 }

    let myRecord' = { r with x = 3 }

    let myRecord'' = { r with x = 3; y = "hello" }
    """
    { FormatConfig.Default with
        RecordMultilineFormatter = MultilineFormatterType.NumberOfItems }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_array_or_list_width
<copy-to-clipboard text="fsharp_max_array_or_list_width = 100"></copy-to-clipboard>

Control the maximum width for which lists and arrays can be in one line. 

Requires `fsharp_array_or_list_multiline_formatter` to be `character_width` to take effect
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxArrayOrListWidth}"
(*** include-output ***)

formatCode
    """ 
    let myArray = [| one; two; three |]
    """
    { FormatConfig.Default with
        MaxArrayOrListWidth = 20 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_array_or_list_number_of_items
<copy-to-clipboard text="fsharp_max_array_or_list_number_of_items = 2"></copy-to-clipboard>

Control the maximum number of elements for which lists and arrays can be in one line.

Requires `fsharp_array_or_list_multiline_formatter` to be `number_of_items` to take effect.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxArrayOrListNumberOfItems}"
(*** include-output ***)

formatCode
    """ 
    let myList = [ one; two ]
    let myArray = [| one; two; three |]
    """
    { FormatConfig.Default with
        MaxArrayOrListNumberOfItems = 2
        ArrayOrListMultilineFormatter = MultilineFormatterType.NumberOfItems }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_array_or_list_multiline_formatter
<copy-to-clipboard text="fsharp_array_or_list_multiline_formatter = number_of_items"></copy-to-clipboard>

Split arrays and lists into multiple lines based on the given condition.  
`character_width` uses character count of the expression, controlled by `fsharp_max_array_or_list_width`.  
`number_of_items` uses the number of elements in the array or list, controlled by `fsharp_max_array_or_list_number_of_items`.

Note that in either case, list expressions are still governed by `max_line_length`.
*)

(*** hide ***)
printfn $"Default = {MultilineFormatterType.ToConfigString FormatConfig.Default.ArrayOrListMultilineFormatter}"
(*** include-output ***)

formatCode
    """ 
    let myArray = [| one; two; three |]
    """
    { FormatConfig.Default with
        ArrayOrListMultilineFormatter = MultilineFormatterType.NumberOfItems }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_value_binding_width
<copy-to-clipboard text="fsharp_max_value_binding_width = 100"></copy-to-clipboard>

Control the maximum expression width for which let and member value/property bindings should be in one line.  
The width is that of the pattern for the binding plus the right-hand expression but not the keywords (e.g. "let").
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxValueBindingWidth}"
(*** include-output ***)

formatCode
    """ 
    let title = "Great title of project"
    type MyType() =
        member this.HelpText = "Some help text"
    """
    { FormatConfig.Default with
        MaxValueBindingWidth = 10 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_function_binding_width
<copy-to-clipboard text="fsharp_max_function_binding_width = 40"></copy-to-clipboard>

Control the maximum width for which function and member bindings should be in one line.  
In contrast to `fsharp_max_value_binding_width`, only the right-hand side expression of the binding is measured.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxFunctionBindingWidth}"
(*** include-output ***)

formatCode
    """ 
    let title = "Great title of project"
    type MyType() =
        member this.HelpText = "Some help text"
    """
    { FormatConfig.Default with
        MaxFunctionBindingWidth = 10 }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_max_dot_get_expression_width
<copy-to-clipboard text="fsharp_max_dot_get_expression_width = 100"></copy-to-clipboard>

Control the maximum width for which (nested) [SynExpr.DotGet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#DotGet) expressions should be in one line.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MaxDotGetExpressionWidth}"
(*** include-output ***)

formatCode
    """ 
   let job = JobBuilder.UsingJobData(jobDataMap).Create<WrapperJob>().Build()
    """
    { FormatConfig.Default with
        MaxDotGetExpressionWidth = 60 }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_multiline_bracket_style
<copy-to-clipboard text="fsharp_multiline_bracket_style = stroustrup"></copy-to-clipboard>

`Cramped` The default way in F# to format brackets.  
`Aligned` Alternative way of formatting records, arrays and lists. This will align the braces at the same column level.  
`Stroustrup` Allow for easier reordering of members and keeping the code succinct. 
*)

(*** hide ***)
printfn $"Default = {MultilineBracketStyle.ToConfigString FormatConfig.Default.MultilineBracketStyle}"
(*** include-output ***)

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
    { FormatConfig.Default with
        MultilineBracketStyle = Aligned }
(*** include-output ***)

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
    { FormatConfig.Default with
        MultilineBracketStyle = Stroustrup }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_newline_before_multiline_computation_expression
<copy-to-clipboard text="fsharp_newline_before_multiline_computation_expression = false"></copy-to-clipboard>

Insert a newline before a computation expression that spans multiple lines
*)

(*** hide ***)
printfn
    $"Default = {FormatConfig.Default.NewlineBeforeMultilineComputationExpression
                     .ToString()
                     .ToLower()}"
(*** include-output ***)

formatCode
    """ 
    let something =
        task {
            let! thing = otherThing ()
            return 5
        }
    """
    { FormatConfig.Default with
        NewlineBeforeMultilineComputationExpression = false }
(*** include-output ***)

(**
## G-Research style

A series of settings requicolor="red" to conform with the [G-Research style guide](https://github.com/G-Research/fsharp-formatting-conventions).  
From a consistency point of view, it is recommend to enable all these settings instead of cherry-picking a few.

<fantomas-setting green gr></fantomas-setting>
### fsharp_newline_between_type_definition_and_members
<copy-to-clipboard text="fsharp_newline_between_type_definition_and_members = false"></copy-to-clipboard>

Adds a new line between a type definition and its first member.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.NewlineBetweenTypeDefinitionAndMembers.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
type Range =
    { From: float
      To: float }

    member this.Length = this.To - this.From
    """
    { FormatConfig.Default with
        NewlineBetweenTypeDefinitionAndMembers = false }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_align_function_signature_to_indentation
<copy-to-clipboard text="fsharp_align_function_signature_to_indentation = true"></copy-to-clipboard>

When a function signature exceeds the `max_line_length`, Fantomas will put all parameters on separate lines.  
This setting also places the equals sign and return type on a new line.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.AlignFunctionSignatureToIndentation.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
[<FunctionName("FormatCode")>]
let run 
    ([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest)
    (log: ILogger)
    : HttpResponse =
    Http.main CodeFormatter.GetVersion format FormatConfig.FormatConfig.Default log req
    """
    { FormatConfig.Default with
        AlignFunctionSignatureToIndentation = true }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_alternative_long_member_definitions
<copy-to-clipboard text="fsharp_alternative_long_member_definitions = true"></copy-to-clipboard>

Provides an alternative way of formatting long member and constructor definitions,
where the difference is mainly in the equal sign and returned type placement.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.AlternativeLongMemberDefinitions.ToString().ToLower()}"
(*** include-output ***)

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
    { FormatConfig.Default with
        AlternativeLongMemberDefinitions = true }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_multi_line_lambda_closing_newline
<copy-to-clipboard text="fsharp_multi_line_lambda_closing_newline = true"></copy-to-clipboard>

Places the closing parenthesis of a multiline lambda argument on the next line.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.MultiLineLambdaClosingNewline.ToString().ToLower()}"
(*** include-output ***)

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
    { FormatConfig.Default with
        MultiLineLambdaClosingNewline = true }
(*** include-output ***)

(**
<fantomas-setting orange gr></fantomas-setting>
### fsharp_experimental_keep_indent_in_branch
<copy-to-clipboard text="fsharp_experimental_keep_indent_in_branch = true"></copy-to-clipboard>

Breaks the normal indentation flow for the last branch of a pattern match or if/then/else expression.  
Only when the last pattern match or else branch was already at the same level of the entire match or if expression.

*This feature is considecolor="red" experimental and is subject to change.*
*)

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
    { FormatConfig.Default with
        ExperimentalKeepIndentInBranch = true }
(*** include-output ***)

(**
<fantomas-setting green gr></fantomas-setting>
### fsharp_bar_before_discriminated_union_declaration
<copy-to-clipboard text="fsharp_bar_before_discriminated_union_declaration = true"></copy-to-clipboard>

Always use a `|` before every case in the declaration of a discriminated union.  
If `false`, a `|` character is used only in multiple-case discriminated unions, and is omitted in short single-case DUs.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.BarBeforeDiscriminatedUnionDeclaration.ToString().ToLower()}"
(*** include-output ***)

formatCode
    """ 
    type MyDU = Short of int
    """
    { FormatConfig.Default with
        BarBeforeDiscriminatedUnionDeclaration = true }

(*** include-output ***)

(**
## Other

Some additional settings that don't fit into any style guide.

<fantomas-setting green></fantomas-setting>
### fsharp_blank_lines_around_nested_multiline_expressions
<copy-to-clipboard text="fsharp_blank_lines_around_nested_multiline_expressions = false"></copy-to-clipboard>

Surround **nested** multi-line expressions with blank lines.  
Existing blank lines are always preserved (via trivia), with exception when [fsharp_keep_max_number_of_blank_lines](#fsharp_keep_max_number_of_blank_lines) is used.  
Top level expressions will always follow the [2020 blank lines revision](https://github.com/fsprojects/fantomas/blob/main/docs-old/FormattingConventions.md#2020-revision) principle.
*)

(*** hide ***)
printfn
    $"Default = {FormatConfig.Default.BlankLinesAroundNestedMultilineExpressions
                     .ToString()
                     .ToLower()}"
(*** include-output ***)

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
    { FormatConfig.Default with
        BlankLinesAroundNestedMultilineExpressions = false }
(*** include-output ***)

(**
<fantomas-setting green></fantomas-setting>
### fsharp_keep_max_number_of_blank_lines
<copy-to-clipboard text="fsharp_keep_max_number_of_blank_lines = 1"></copy-to-clipboard>

Set maximal number of consecutive blank lines to keep from original source. It doesn't change number of new blank lines generated by Fantomas.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.KeepMaxNumberOfBlankLines}"
(*** include-output ***)

formatCode
    """ 
    open Foo

    let x = 42
    """
    { FormatConfig.Default with
        KeepMaxNumberOfBlankLines = 1 }
(*** include-output ***)

(**
<fantomas-setting orange></fantomas-setting>
### fsharp_experimental_elmish
<copy-to-clipboard text="fsharp_experimental_elmish = true"></copy-to-clipboard>

Applies the Stroustrup style to the final (two) array or list argument(s) in a function application.  
Note that this behaviour is also active when `fsharp_multiline_bracket_style = stroustrup`.
*)

(*** hide ***)
printfn $"Default = {FormatConfig.Default.ExperimentalElmish.ToString().ToLower()}"
(*** include-output ***)

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
    { FormatConfig.Default with
        ExperimentalElmish = true }
(*** include-output ***)

(**
<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>

*)
