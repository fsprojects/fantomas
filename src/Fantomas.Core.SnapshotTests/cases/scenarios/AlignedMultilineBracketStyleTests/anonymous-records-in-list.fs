(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
let configurations =
    [
        {| Build = true; Configuration = "RELEASE"; Defines = ["FOO"] |}
        {| Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] |}
    ]
