module Fantomas.Logging

[<RequireQualifiedAccess>]
type VerbosityLevel =
    | Normal
    | Detailed

/// log a message
let stdlog (s: Printf.TextWriterFormat<_>) = printfn s

/// log a message to stderr
let elog (s: Printf.TextWriterFormat<_>) = eprintfn s

/// log a message if the verbosity level is >= Detailed
let logGrEqDetailed verbosity s =
    if verbosity = VerbosityLevel.Detailed then
        printfn "%s" s
    else
        ()

// Todo - this doesn't work yet
// let logGrEqDetailedF (verbosity: VerbosityLevel) (s: Printf.TextWriterFormat<_>) =
//     if verbosity = VerbosityLevel.Detailed then
//         printfn s |> ignore
//     else
//         ()

// logGrEqDetailedF VerbosityLevel.Detailed "foo %d" 32
