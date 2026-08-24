module Fantomas.Analyzers.PipeBackAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Literal>]
let Code: string = "FANTOMAS-PIPEBACK-001"

[<Literal>]
let Name: string = "PipeBackAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects the backward pipe operator, which reads against the direction the surrounding code is written in."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-pipeback-001"

// The backward pipe operators FSharp.Core defines, under the compiled names the untyped tree
// carries. Writing `<|` produces a `LongIdent` holding `op_PipeLeft`, with the notation the
// author used kept beside it as trivia, so matching the compiled name catches both `f <| x` and
// the far rarer `op_PipeLeft f x`.
let backwardPipes: Set<string> =
    set [ "op_PipeLeft"; "op_PipeLeft2"; "op_PipeLeft3" ]

// Every occurrence of a backward pipe in the file, reported on the operator itself rather than on
// the expression around it, so that the range points at the thing to remove.
let analyze (parsedInput: ParsedInput) : Message list =
    let ranges: ResizeArray<range> = ResizeArray<range>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match expr with
                | SynExpr.Ident ident
                | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ ident ])) when backwardPipes.Contains ident.idText ->
                    ranges.Add ident.idRange
                | _ -> ()
        }

    walkAst walker parsedInput

    ranges
    |> Seq.map (fun (operator: range) ->
        {
            Type = Name
            Message = "Do not use the backward pipe operator. Parenthesise the argument instead."
            Code = Code
            Severity = Severity.Error
            Range = operator
            Fixes = []
        })
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }
