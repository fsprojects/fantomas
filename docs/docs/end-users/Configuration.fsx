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
#### <fantomas-setting-icon type="green"></fantomas-setting-icon>indent_size

This preference sets the indentation.  
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
    { FormatConfig.Default with IndentSize = 2 }
(*** include-it ***)
(**
## Maximum width constraints
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon type="orange"></fantomas-setting-icon>indent_size
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
{ FormatConfig.Default with IndentSize = 2 }

(**
## G-Research style
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon-gresearch ></fantomas-setting-icon-gresearch>indent_size
` indent_size` has to be between 1 and 10.

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
    { FormatConfig.Default with IndentSize = 2 }

(**
## Other
Lorep ipsum dolor sit amet, consectetur adipiscing elit. Donec euismod, nisi vel consectetur interdum, nisi nisi consectetur nisl, eget consectetur nisl nisi vel nisi.
#### <fantomas-setting-icon type="red"></fantomas-setting-icon>indent_size

` indent_size` has to be between 1 and 10.

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
    { FormatConfig.Default with IndentSize = 2 }