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
Fantomas ships with a series of settings that you can use freely depending  on your case. However, there are settings that we do not recommend and generally should not be used.   
<p><i class="bi bi-check-circle-fill green-recommendation fs-4 align-middle" ></i> <strong>Safe to change:</strong> Settings that aren't attached to any guidelines. Depending on your team or your own preferences, feel free to change these as it's been agreed on the codebase, however, you can always use it's defaults.</p>
<p><i class="bi bi-exclamation-circle-fill orange-recommendation fs-4 align-middle"></i> <strong>Not recommended:</strong> Settings that don't follow any guidelines.</p>
<p><i class="bi bi-x-circle-fill red-recommendation fs-4 align-middle"></i> <strong>Do not use:</strong> Settings you should never enable. They might lead to incomplete results.</p>
<p><img class="gresearch-recommendation align-middle" src="{{root}}/gresearch.svg" alt="G-Research logo"/> <strong>G-Research:</strong> G-Research styling guide. If you use one of these, for consistency reasons you should use all of them.</p>
*)

(**
## Auxiliary settings
#### <i class="bi bi-check-circle-fill green-recommendation me-1 fs-5" data-bs-toggle="tooltip" data-bs-custom-class="green-tooltip" data-bs-title="This setting is good to use" ></i>indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation.  
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`
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
#### <i class="bi bi-check-circle-fill orange-recommendation me-1" data-bs-toggle="tooltip" data-bs-custom-class="orange-tooltip" data-bs-title="This setting is not recommended"></i> indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`
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
## <img class="gresearch-recommendation align-middle mb-2" data-bs-toggle="tooltip" data-bs-custom-class="gresearch-tooltip" data-bs-title="If you use one of these you should use all G-Research settings for consistency reasons" data-bs-custom-class="orange-tooltip" src="{{root}}/gresearch.svg" alt="G-Research logo"/> G-Research style
#### <i class="bi bi-check-circle-fill orange-recommendation me-1" data-bs-toggle="tooltip" data-bs-custom-class="orange-tooltip" data-bs-title="This setting is not recommended"></i>indent_size
` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`
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
#### <i class="bi bi-check-circle-fill red-recommendation me-1" data-bs-toggle="tooltip" data-bs-custom-class="red-tooltip" data-bs-title="You shouldn't use this setting"></i>indent_size

` indent_size` has to be between 1 and 10.

This preference sets the indentation
The common values are 2 and 4. 
The same indentation is ensured to be consistent in a source file.
Default = 4.

`defaultConfig`
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