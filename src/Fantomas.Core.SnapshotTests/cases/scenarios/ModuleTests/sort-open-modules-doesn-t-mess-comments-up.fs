(*---
fsharp_max_infix_operator_expression = 50
---*)
module internal Fantomas.CodePrinter

// comment0
let x = 0

open System
open System.Collections.Generic
open FSharp.Compiler.SyntaxTree
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceTransformer

// comment1
let sortAndDedup by l =
    // comment2
    l |> Seq.distinctBy by |> Seq.sortBy by |> List.ofSeq