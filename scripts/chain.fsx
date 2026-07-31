#load "shared.fsx"

open System.IO
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Shared

// Best-effort: extract a short display name from the member expression of a segment.
let rec exprName (e: Expr) : string =
    match e with
    | Expr.Ident n -> n.Text
    | Expr.OptVar n ->
        n.Identifier.Content
        |> List.choose (function
            | IdentifierOrDot.Ident i -> Some i.Text
            | _ -> None)
        |> String.concat "."
    | Expr.TypeApp n -> $"{exprName n.Identifier}<...>"
    | _ -> e.GetType().Name

let printChain (chain: ExprChain) =
    printfn "Chain:"
    printfn "  Head    : %s" (exprName chain.Head)

    printfn "  Segments: %d" chain.Segments.Length

    chain.Segments
    |> List.iteri (fun i seg ->
        match seg with
        | ChainSegment.DotMember(_, expr) ->
            printfn "    [%02d] simple   .%s" i (exprName expr)
        | ChainSegment.DotApplication(_, expr, ChainCall.Unit _) ->
            printfn "    [%02d] complex  .%s()" i (exprName expr)
        | ChainSegment.DotApplication(_, expr, ChainCall.Paren _) ->
            printfn "    [%02d] complex  .%s(...)" i (exprName expr)
        | ChainSegment.DotIndex(_, idx) ->
            printfn "    [%02d] simple   .[%s]" i (exprName idx))

    let terminalStr =
        match chain.Terminal with
        | ChainTerminal.NoTerminal -> "(none)"
        | ChainTerminal.SpaceAllowed(ChainCall.Unit _) -> "SpaceAllowed ()"
        | ChainTerminal.SpaceAllowed(ChainCall.Paren _) -> "SpaceAllowed (...)"
        | ChainTerminal.NoSpaceAllowed(ChainCall.Unit _) -> "NoSpaceAllowed ()"
        | ChainTerminal.NoSpaceAllowed(ChainCall.Paren _) -> "NoSpaceAllowed (...)"

    printfn "  Terminal: %s" terminalStr

// Recursively collect all ExprChain nodes in the Oak's Node tree.
let rec collectChains (node: Node) : ExprChain list =
    [
        match node with
        | :? ExprChain as chain -> yield chain
        | _ -> ()
        for child in node.Children do
            yield! collectChains child
    ]

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let source, _, _, _ = parseArgs fsi.CommandLineArgs.[1..]
        let oak =
            CodeFormatter.ParseOakAsync(false, source)
            |> Async.RunSynchronously
            |> Array.head
            |> fst

        let chains = collectChains oak

        if chains.IsEmpty then
            printfn "No chain expression found in input."
        else
            printfn "Found %d chain(s):\n" chains.Length
            chains |> List.iteri (fun i chain ->
                printfn "--- Chain #%d ---" (i + 1)
                printChain chain
                printfn "")
| _ ->
    printfn "Usage: dotnet fsi scripts/chain.fsx <input-file>"
