(*---
max_line_length = 100
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_align_function_signature_to_indentation = true
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =
    let bar () =
        let saveDir =
            fs.DirectoryInfo.FromDirectoryName(fs.Path.Combine((ThingThing.rootRoot fs thingThing).FullName, "tada!")).EnumerateDirectories()
            |> Seq.exactlyOne
        ()
