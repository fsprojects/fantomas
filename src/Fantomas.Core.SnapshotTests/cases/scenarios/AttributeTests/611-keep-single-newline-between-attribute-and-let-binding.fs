(*---
fsharp_space_before_lowercase_invocation = false
fsharp_space_after_comma = false
fsharp_space_after_semicolon = false
fsharp_space_around_delimiter = false
---*)
open System
open Library

[<EntryPoint>]

let main argv =
    printfn "Nice command-line arguments! Here's what JSON.NET has to say about them:" argv
    |> Array.map getJsonNetJson |> Array.iter (printfn "%s")
    0 // return an integer exit code
