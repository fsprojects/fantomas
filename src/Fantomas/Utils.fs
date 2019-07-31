namespace Fantomas

open System
open FSharp.Compiler.Ast

[<RequireQualifiedAccess>]
module SynExpr =

    let isInParens =
        function | SynExpr.Paren _ -> true | _ -> false

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str : string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

    let normalizeThenSplitNewLine (str : string) =
        (normalizeNewLine str).Split('\n')

    let startsWithOrdinal (prefix : string) (str : string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let removeTrailingSpaces (source:string) =
        source.Split([| Environment.NewLine |], StringSplitOptions.None)
        |> Array.map (fun line -> line.TrimEnd())
        |> fun lines -> String.Join(Environment.NewLine, lines)