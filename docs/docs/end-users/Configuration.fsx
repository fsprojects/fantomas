(**
---
category: End-users
categoryindex: 1
index: 3
---
*)

(**
# Configuration
Fantomas ships with a limited series of options.
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the
commandline.  
Your IDE should respect your settings, however the implementation of that is editor specific. Setting the configuration via
UI might be available depending on the IDE.
*)

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
fsharp_multiline_bracket_style = experimental_stroustrup
```
*)

(**
## Trying your settings via the online tool
You can quickly try your settings via the <a href="https://fsprojects.github.io/fantomas-tools/#/fantomas/preview" target="_blank">online tool</a>.

<img src="{{root}}/online_tool_usage.gif" alt="drawing" width="100%"/>
*)

#r "nuget: Fantomas.Core, 5.*"

open Fantomas.Core.FormatConfig
open Fantomas.Core

let formatCode input configIndent =
    CodeFormatter.FormatDocumentAsync(false, input, configIndent)
    |> Async.RunSynchronously

(**
## Settings recommendations
Fantomas ships with a series of settings that you can use freely depending  on your case.  
However, there are settings that we do not recommend and generally should not be used.
<p><fantomas-setting-icon type="green"></fantomas-setting-icon><strong>Safe to change:</strong> Settings that aren't attached to any guidelines. Depending on your team or your own preferences, feel free to change these as it's been agreed on the codebase, however, you can always use it's defaults.</p>
<p><fantomas-setting-icon type="orange"></fantomas-setting-icon><strong>Use with caution:</strong> Settings where it is not recommended to change the default value. They might lead to incomplete results.</p>
<p><fantomas-setting-icon type="red"></fantomas-setting-icon><strong>Do not use:</strong> Settings that don't follow any guidelines.</p>
<p><fantomas-setting-icon type="gr"></fantomas-setting-icon><strong>G-Research:</strong> G-Research styling guide. If you use one of these, for consistency reasons you should use all of them.</p>
*)

(**
## Auxiliary settings

<fantomas-setting name="indent_size" orange></fantomas-setting>

`indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4.  
The same indentation is ensured to be consistent in a source file.

Default = 4.
*)

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
(*** include-it ***)
(**
<fantomas-setting name="max_line_length" green></fantomas-setting>

`max_line_length` has to be an integer greater or equal to 60.  
This preference sets the column where we break F# constructs into new lines.

Default = 120.
*)

formatCode
    """ 
    match myValue with
    | Some foo -> someLongFunctionNameThatWillTakeFooAndReturnsUnit foo
    | None -> printfn "nothing"
    """
    { FormatConfig.Default with
        MaxLineLength = 60 }
(*** include-it ***)

(**
<fantomas-setting name="end_of_line" green></fantomas-setting>

`end_of_line` determines the newline character, `lf` will add `\n` where `crlf` will add `\r\n`.  
`cr` is not supported by the F# language spec.  
If not set by the user, the default value is determined by `System.Environment.NewLine`.
*)

(**
<fantomas-setting name="insert_final_newline" orange></fantomas-setting>

Adds a final newline character at the end of the file.  
<a href="https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline" target="_blank">Why should text files end with a newline?</a>

Default = true
*)

formatCode
    """ 
    let a = 42
    """
    { FormatConfig.Default with
        InsertFinalNewline = false }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_parameter" orange></fantomas-setting>

Add a space after the name of a function and before the opening parenthesis of the first parameter.  
This setting influences function definitions.

Default = true.
*)

formatCode
    """ 
   let value (a: int) = x
   let DumpTrace() = ()
    """
    { FormatConfig.Default with
        SpaceBeforeParameter = false }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_lowercase_invocation" orange></fantomas-setting>

Add a space after the name of a lowercased function and before the opening parenthesis of the first argument.  
This setting influences function invocation in expressions and patterns.

Default = true.
*)

formatCode
    """ 
value (a, b)
startTimer ()

match x with
| value (a, b) -> ()
    """
    { FormatConfig.Default with
        SpaceBeforeLowercaseInvocation = false }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_uppercase_invocation" green gr></fantomas-setting>

Add a space after the name of a uppercase function and before the opening parenthesis of the first argument.  
This setting influences function invocation in expressions and patterns.

Default = false.
*)

formatCode
    """ 
Value(a, b)
person.ToString()

match x with
| Value(a, b) -> ()
    """
    { FormatConfig.Default with
        SpaceBeforeUppercaseInvocation = true }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_class_constructor" orange></fantomas-setting>

Add a space after a type name and before the class constructor.

Default = false.
*)

formatCode
    """ 
    type Person() =
        class
        end
    """
    { FormatConfig.Default with
        SpaceBeforeClassConstructor = true }

(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_member" green gr></fantomas-setting>

Add a space after a member name and before the opening parenthesis of the first parameter.

Default = false.
*)

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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_colon" green></fantomas-setting>

Add a space before `:`. Please note that not every `:` is controlled by this setting.

Default = false.
*)

formatCode
    """ 
   type Point = { x: int; y: int }
   let myValue: int = 42
   let update (msg: Msg) (model: Model) : Model = model
    """
    { FormatConfig.Default with
        SpaceBeforeColon = true }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_after_comma" orange></fantomas-setting>

Adds a space after `,` in tuples.

Default = true.
*)

formatCode
    """ 
    myValue.SomeFunction(foo, bar, somethingElse)
    (a, b, c)
    """
    { FormatConfig.Default with
        SpaceAfterComma = false }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_before_semicolon" green gr></fantomas-setting>

Adds a space before `;` in records, arrays, lists, etc.

Default = false.
*)

formatCode
    """ 
    let a = [ 1 ; 2 ; 3 ]
    let b = [| foo ; bar |]
    type C = { X: int ; Y: int }
    """
    { FormatConfig.Default with
        SpaceBeforeSemicolon = true }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_after_semicolon" orange></fantomas-setting>

Adds a space after `;` in records, arrays, lists, etc.

Default = true.
*)

formatCode
    """ 
    let a = [ 1; 2; 3 ]
    let b = [| foo; bar |]
    type C = { X: int; Y: int }
    """
    { FormatConfig.Default with
        SpaceAfterSemicolon = false }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_space_around_delimiter" orange></fantomas-setting>

Adds a space around delimiters like `[`,`[|`,{`.

Default = true.
*)

formatCode
    """ 
    let a = [ 1;2;3 ]
    let b = [| 4;5;6 |]
    """
    { FormatConfig.Default with
        SpaceAroundDelimiter = false }
(*** include-it ***)

(**
## Maximum width constraints

Settings that control the max width of certain expressions.

<fantomas-setting name="fsharp_max_if_then_short_width" orange></fantomas-setting>

Control the maximum length for which if/then expression without an else expression can be on one line.  
The [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions) recommends to never write such an expression in one line.

> If the else expression is absent, it is recommended to never to write the entire expression in one line.

Default = 0.
*)

formatCode
    """ 
    if a then 
        ()
    """
    { FormatConfig.Default with
        MaxIfThenShortWidth = 15 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_if_then_else_short_width" green></fantomas-setting>

Fantomas by default follows the if/then/else conventions listed in the [Microsoft F# style guide](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions).  
This setting facilitates this by determining the maximum character width where the if/then/else expression stays in one line.

Default = 40.
*)

formatCode
    """ 
    if myCheck then truth else bogus
    """
    { FormatConfig.Default with
        MaxIfThenElseShortWidth = 10 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_infix_operator_expression" green></fantomas-setting>

Control the maximum length for which infix expression can be on one line.

Default = 50.
*)
formatCode
    """ 
    let WebApp =
        route "/ping" >=> authorized >=> text "pong"
    """
    { FormatConfig.Default with
        MaxInfixOperatorExpression = 20 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_record_width" green></fantomas-setting>

Control the maximum width for which records should be in one line.

Default = 40.

Requires `fsharp_record_multiline_formatter` to be `character_width` to take effect.
*)

formatCode
    """ 
    type MyRecord = { X: int; Y: int; Length: int }
    let myInstance = { X = 10; Y = 20; Length = 90 }
    """
    { FormatConfig.Default with
        MaxRecordWidth = 20 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_record_number_of_items" green></fantomas-setting>

Control the maximum number of fields for which records should be in one line.

Default = 1.

Requires `fsharp_record_multiline_formatter` to be
`number_of_items` to take effect.
*)

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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_record_multiline_formatter" green></fantomas-setting>

Split records expressions/statements into multiple lines based on the given condition.  
`character_width` uses character count of the expression, controlled by `fsharp_max_record_width`.  
`number_of_items` uses the number of fields in the record, controlled by `fsharp_max_record_number_of_items`.

Default = `character_width`. 

Note that in either case, record expressions/statements are still governed by `max_line_length`.
*)

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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_array_or_list_width" green></fantomas-setting>

Control the maximum width for which lists and arrays can be in one line. 

Default= 40. 

Requires `fsharp_array_or_list_multiline_formatter` to be `character_width` to take effect
*)

formatCode
    """ 
    let myArray = [| one; two; three |]
    """
    { FormatConfig.Default with
        MaxArrayOrListWidth = 20 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_array_or_list_number_of_items" green></fantomas-setting>

Control the maximum number of elements for which lists and arrays can be in one line.

Default = 1.

Requires `fsharp_array_or_list_multiline_formatter` to be `number_of_items` to take effect.
*)

formatCode
    """ 
    let myList = [ one; two ]
    let myArray = [| one; two; three |]
    """
    { FormatConfig.Default with
        MaxArrayOrListNumberOfItems = 2
        ArrayOrListMultilineFormatter = MultilineFormatterType.NumberOfItems }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_array_or_list_multiline_formatter" green></fantomas-setting>

Split arrays and lists into multiple lines based on the given condition.  
`character_width` uses character count of the expression, controlled by `fsharp_max_array_or_list_width`.  
`number_of_items` uses the number of elements in the array or list, controlled by `fsharp_max_array_or_list_number_of_items`.

Default = `character_width`.

Note that in either case, list expressions are still governed by `max_line_length`.
*)

formatCode
    """ 
    let myArray = [| one; two; three |]
    """
    { FormatConfig.Default with
        ArrayOrListMultilineFormatter = MultilineFormatterType.NumberOfItems }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_value_binding_width" green></fantomas-setting>

Control the maximum expression width for which let and member value/property bindings should be in one line.  
The width is that of the pattern for the binding plus the right-hand expression but not the keywords (e.g. "let").

Default = 80.
*)

formatCode
    """ 
    let title = "Great title of project"
    type MyType() =
        member this.HelpText = "Some help text"
    """
    { FormatConfig.Default with
        MaxValueBindingWidth = 10 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_function_binding_width" green></fantomas-setting>

Control the maximum width for which function and member bindings should be in one line.  
In contrast to `fsharp_max_value_binding_width`, only the right-hand side expression of the binding is measured.

Default = 40
*)

formatCode
    """ 
    let title = "Great title of project"
    type MyType() =
        member this.HelpText = "Some help text"
    """
    { FormatConfig.Default with
        MaxFunctionBindingWidth = 10 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_max_dot_get_expression_width" green></fantomas-setting>

Control the maximum width for which (nested) [SynExpr.DotGet](https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-syntax-synexpr.html#DotGet) expressions should be in one line.

Default = 50.
*)

formatCode
    """ 
   let job =
    JobBuilder
        .UsingJobData(jobDataMap)
        .Create<WrapperJob>()
        .Build()
    """
    { FormatConfig.Default with
        MaxDotGetExpressionWidth = 100 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_multiline_bracket_style" green></fantomas-setting>

How to format bracketted expressions (e.g. records, arrays, lists, etc.) that span multiple lines. 

_This setting replaces the deprecated settings `fsharp_multiline_block_brackets_on_same_column` and `fsharp_experimental_stroustrup_style`._

Possible values:

* `cramped`
* `aligned`
* `experimental_stroustrup`

Default = `cramped`.
*)

(**
**Cramped** - The default way in F# to format brackets.  
*)
formatCode
    """ 
    let band = { Vocals = "John"; Bass = "Paul"; Guitar = "George"; Drums = "Ringo" }
    let songs = [ "Come Together"; "Hey Jude"; "Yesterday"; "Yellow Submarine"; "Here Comes the Sun" ]
    """
    { FormatConfig.Default with
        MultilineBracketStyle = Cramped }
(*** include-it ***)

(**
**Aligned** - Alternative way of formatting brackets. This will align the braces at the same column level.
*)

formatCode
    """ 
    let band = { Vocals = "John"; Bass = "Paul"; Guitar = "George"; Drums = "Ringo" }
    let songs = [ "Come Together"; "Hey Jude"; "Yesterday"; "Yellow Submarine"; "Here Comes the Sun" ]
    """
    { FormatConfig.Default with
        MultilineBracketStyle = Aligned }
(*** include-it ***)

(**
**ExperimentalStroustrup** - Experimental setting. Places the opening brace on the same line as the binding, and the closing brace on its own line.

_Please contribute to [fsprojects/fantomas#1408](https://github.com/fsprojects/fantomas/issues/1408) and engage in [fsharp/fslang-design#706](https://github.com/fsharp/fslang-design/issues/706)._
*)

formatCode
    """ 
    let band = { Vocals = "John"; Bass = "Paul"; Guitar = "George"; Drums = "Ringo" }
    let songs = [ "Come Together"; "Hey Jude"; "Yesterday"; "Yellow Submarine"; "Here Comes the Sun" ]
    """
    { FormatConfig.Default with
        MultilineBracketStyle = ExperimentalStroustrup }
(*** include-it ***)

(**
## G-Research style

A series of settings required to conform with the [G-Research style guide](https://github.com/G-Research/fsharp-formatting-conventions).  
From a consistency point of view, it is recommend to enable all these settings instead of cherry-picking a few.

<fantomas-setting name="fsharp_newline_between_type_definition_and_members" green gr></fantomas-setting>

Adds a new line between a type definition and its first member.

Default = false.
*)

formatCode
    """ 
type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From
    """
    { FormatConfig.Default with
        NewlineBetweenTypeDefinitionAndMembers = true }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_align_function_signature_to_indentation" green gr></fantomas-setting>

When a function signature exceeds the `max_line_length`, Fantomas will put all parameters on separate lines.  
This setting also places the equals sign and return type on a new line.

Default = false.
*)

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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_alternative_long_member_definitions" green gr></fantomas-setting>

Provides an alternative way of formatting long member and constructor definitions,
where the difference is mainly in the equal sign and returned type placement.

Default = false.
*)

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
    """
    { FormatConfig.Default with
        AlternativeLongMemberDefinitions = true }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_multi_line_lambda_closing_newline" green gr></fantomas-setting>

Places the closing parenthesis of a multiline lambda argument on the next line.

Default = false.
*)

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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_experimental_keep_indent_in_branch" orange gr></fantomas-setting>

Breaks the normal indentation flow for the last branch of a pattern match or if/then/else expression.  
Only when the last pattern match or else branch was already at the same level of the entire match or if expression.

*This feature is considered experimental and is subject to change.*
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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_bar_before_discriminated_union_declaration" green gr></fantomas-setting>

Always use a `|` before every case in the declaration of a discriminated union.  
If `false`, a `|` character is used only in multiple-case discriminated unions, and is omitted in short single-case DUs.

Default = false.
*)

formatCode
    """ 
    type MyDU = Short of int
    """
    { FormatConfig.Default with
        BarBeforeDiscriminatedUnionDeclaration = true }

(*** include-it ***)

(**
## Other

Some additional settings that don't fit into any style guide.

<fantomas-setting name="fsharp_blank_lines_around_nested_multiline_expressions" green></fantomas-setting>

Surround **nested** multi-line expressions with blank lines.  
Existing blank lines are always preserved (via trivia), with exception when [fsharp_keep_max_number_of_blank_lines](#fsharp_keep_max_number_of_blank_lines) is used.  
Top level expressions will always follow the [2020 blank lines revision](https://github.com/fsprojects/fantomas/blob/main/docs-old/FormattingConventions.md#2020-revision) principle.

Default = true.
*)

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
(*** include-it ***)

(**
<fantomas-setting name="fsharp_keep_max_number_of_blank_lines" green></fantomas-setting>

Set maximal number of consecutive blank lines to keep from original source. It doesn't change number of new blank lines generated by Fantomas.

Default=100
*)

formatCode
    """ 
    open Foo

    let x = 42
    """
    { FormatConfig.Default with
        KeepMaxNumberOfBlankLines = 1 }
(*** include-it ***)

(**
<fantomas-setting name="fsharp_strict_mode" red></fantomas-setting>

If being set, pretty printing is only done via ASTs. Compiler directives, inline comments and block comments will be ignored.  
There are numerous situations when the information in the AST alone cannot restore the original code.

**Please do not use this setting for formatting hand written code!**

Valid use-case of this settings is code generation in projects like [FsAst](https://github.com/ionide/FsAst) and [Myriad](https://github.com/MoiraeSoftware/myriad).

Default = false.
*)

formatCode
    """ 
    // some great comment
    let add a b =
    #if INTERACTIVE
        42
    #else
        a + b
    #endif
    """
    { FormatConfig.Default with
        StrictMode = true }
(*** include-it ***)
(**
<fantomas-nav previous="./StyleGuide.html" next="./IgnoreFiles.html"></fantomas-nav>

*)
