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