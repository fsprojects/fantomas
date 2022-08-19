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
These can be stored in an [.editorconfig](https://editorconfig.org/) file and will be picked up automatically by the 
commandline. <br>
Your IDE should respect your settings, but the implementation of that is editor specific. Setting the configuration via 
UI might be available depending on the IDE but do note that these features might not be up to 
date nor work properly, therefore it's support and use is out of scope for this project. 
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
You can quickly try your settings via the <a href="https://fsprojects.github.io/fantomas-tools/#/fantomas/preview" target="_blank">online tool</a>. <br>
<img src="{{root}}/online_tool_usage.gif" alt="drawing" width="100%"/>
*)

(**
## Auxiliary settings
### indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`
*)
let input = """ 
let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1
"""
let configIndent = { FormatConfig.Default with IndentSize = 2 }
(**

## Maximum width constraints
### indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`

*)
let input2 = """ 
let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1
"""
let configIndent2 = { FormatConfig.Default with IndentSize = 2 }
(**

## G-Research style
### indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`

*)
let input3 = """ 
let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1
"""
let configIndent3 = { FormatConfig.Default with IndentSize = 2 }
(**

## Other
### indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`

*)
let input4 = """ 
let inline selectRandom (f: _ []) =
    let r = random 1.0

    let rec find =
        function
        | 0 -> fst f.[0]
        | n when r < snd f.[n] -> fst f.[n]
        | n -> find (n - 1)

    find <| f.Length - 1
"""
let configIndent4 = { FormatConfig.Default with IndentSize = 2 }
let formatCode input configIndent =
    CodeFormatter.FormatDocumentAsync  (false, input, configIndent)
    |>  Async.RunSynchronously

formatCode input configIndent