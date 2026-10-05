(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
let updateModuleInImpl (ast: ParsedInput) (mdl: SynModuleOrNamespace) : ParsedInput =
    match ast with
    | ParsedInput.SigFile _ -> ast
    | ParsedInput.ImplFile _ ->
    ParsedImplFileInput(
        fileName,
        isScript,
        qualifiedNameOfFile,
        scopedPragmas,
        hashDirectives,
        [ mdl ],
        isLastAndCompiled
    )
    |> ParsedInput.ImplFile
