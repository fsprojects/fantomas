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

# Write a comment by starting the line with a '#'
[*.{fs,fsx,fsi}]
fsharp_bar_before_discriminated_union_declaration = true

# Apply specific settings for a targeted subfolder
[src/Elmish/View.fs]
fsharp_multiline_block_brackets_on_same_column = true
fsharp_experimental_stroustrup_style = true
```
*)

(**
## Trying your settings via the online tool
You can quickly try your settings via the <a href="https://fsprojects.github.io/fantomas-tools/#/fantomas/preview" target="_blank">online tool</a>.  
<img src="{{root}}/online_tool_usage.gif" alt="drawing" width="100%"/>
*)

#r "nuget: Fantomas.Core, 5.0.0-beta-*"

open Fantomas.Core.FormatConfig
open Fantomas.Core
let formatCode input configIndent =
    CodeFormatter.FormatDocumentAsync  (false, input, configIndent)
    |>  Async.RunSynchronously

(**
## Settings recommendations
Fantomas ships with a series of settings that you can use freely depending  on your case. 
However, there are settings that we do not recommend and generally should not be used.   
<p><fantomas-setting-icon type="green"></fantomas-setting-icon><strong>Safe to change:</strong> Settings that aren't attached to any guidelines. Depending on your team or your own preferences, feel free to change these as it's been agreed on the codebase, however, you can always use it's defaults.</p>
<p><fantomas-setting-icon type="red"></fantomas-setting-icon> <strong>Not recommended:</strong> Settings that don't follow any guidelines.</p>
<p><fantomas-setting-icon type="orange"></fantomas-setting-icon> <strong>Do not use:</strong> Settings you should never enable. They might lead to incomplete results.</p>
<p><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch> <strong>G-Research:</strong> G-Research styling guide. If you use one of these, for consistency reasons you should use all of them.</p>
*)

(**
## Auxiliary settings
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon tooltip="Both style guides are based on 4" type="orange"></fantomas-setting-icon>indent_size
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)
(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon>max_line_length
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Recommendation: use lf regardless of OS" type="green"></fantomas-setting-icon>end_of_line
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Recommendation: enable, a file should end on a newline character" type="green"></fantomas-setting-icon>insert_final_newline
 `Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="No guide changes default" type="orange"></fantomas-setting-icon>fsharp_space_before_parameter
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="No guide changes default" type="orange"></fantomas-setting-icon>fsharp_space_before_lowercase_invocation
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Note isn't always respected, in some cases it can lead to invalid code" type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_space_before_uppercase_invocation
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="No guide changes default" type="orange"></fantomas-setting-icon>fsharp_space_before_class_constructor
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_space_before_member
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_space_before_colon
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)


(**
#### <fantomas-setting-icon tooltip="No guide changes default" type="orange"></fantomas-setting-icon>fsharp_space_after_comma
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_space_before_semicolon
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)


(**
#### <fantomas-setting-icon tooltip="No guide changes default" type="orange"></fantomas-setting-icon>fsharp_space_after_semicolon
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="No guide changes default" type="orange"></fantomas-setting-icon>fsharp_space_around_delimiter
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)


(**
## Maximum width constraints
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_if_then_short_width
 `Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_if_then_else_short_width
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_infix_operator_expression
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_record_width
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_record_number_of_items
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_record_multiline_formatter
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_array_or_list_width
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_array_or_list_number_of_items
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_array_or_list_multiline_formatter
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_value_binding_width
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_function_binding_width
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Agree within team" type="green"></fantomas-setting-icon>fsharp_max_dot_get_expression_width
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)



(**
## G-Research style
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_multiline_block_brackets_on_same_column
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_newline_between_type_definition_and_members
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_align_function_signature_to_indentation
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_alternative_long_member_definitions
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_multi_line_lambda_closing_newline
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="Still experimental" type="orange"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_experimental_keep_indent_in_branch
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_blank_lines_around_nested_multiline_expressions
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_bar_before_discriminated_union_declaration
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
## Other
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon tooltip="Not part of any guide" type="orange"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_experimental_stroustrup_style
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
 { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)

(**
#### <fantomas-setting-icon tooltip="I recommend !" type="green"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_keep_max_number_of_blank_lines
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)


(**
#### <fantomas-setting-icon tooltip="Never use this please" type="red"></fantomas-setting-icon><fantomas-setting-icon-gresearch></fantomas-setting-icon-gresearch>fsharp_strict_mode
`Lorep` ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)
