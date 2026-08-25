module Fantomas.Invocation

open System
open System.IO

let nameOf (processPath: string option) : string =
    // `Environment.ProcessPath` can be absent in single file and AOT scenarios, and the tool's own
    // name is the safe thing to fall back on: it is right for a global install and readable
    // everywhere else.
    match processPath with
    | None -> "fantomas"
    | Some path ->
        match Path.GetFileNameWithoutExtension path with
        | "" -> "fantomas"
        | executable ->
            if String.Equals(executable, "dotnet", StringComparison.OrdinalIgnoreCase) then
                "dotnet fantomas"
            else
                executable

let name () : string =
    nameOf (Option.ofObj Environment.ProcessPath)
