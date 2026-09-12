module Fantomas.Analyzers.SnobMatchAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-SNOBMATCH-001"

[<Literal>]
let Name: string = "SnobMatchAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a two armed match on a boolean, which is an if expression dressed up as pattern matching."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-snobmatch-001"

// Whether a pair of arm patterns is a boolean test written out as two arms.
//
// `true` against `false` is the whole of it, in either order, and `true` or `false` against a
// wildcard says the same thing with the other value left unnamed. A boolean has two values, so
// these cover the scrutinee between them and the second arm is reached exactly when the first is
// not, which is what an `if` means.
let isBooleanTest (first: SynPat) (second: SynPat) : bool =
    match first, second with
    | SynPat.Const(SynConst.Bool _, _), SynPat.Wild _ -> true
    | SynPat.Const(SynConst.Bool firstValue, _), SynPat.Const(SynConst.Bool secondValue, _) -> firstValue <> secondValue
    | _ -> false

// Whether this expression is a match an `if` would say better.
//
// `SynExpr.Match` alone, where the two rules about arm layout reach through `matchClausesOf` for
// `match!` and `function` as well. This rule is about a rewrite rather than about layout, and the
// rewrite differs per form: a `match!` on a boolean needs a `let!` before it can be an `if`, and a
// `function` needs a parameter invented to have something to test. Both are more than the rule is
// worth, so both are left alone.
//
// A `when` guard on either arm means the arms no longer cover the scrutinee between them, so the
// match is asking something the patterns do not say. A conditional directive inside means the two
// arms this reads are not the arms every build sees.
let shouldBeAnIf (directives: range list) (expr: SynExpr) : range option =
    match expr with
    | SynExpr.Match(
        clauses = [ SynMatchClause(pat = first; whenExpr = None); SynMatchClause(pat = second; whenExpr = None) ]
        range = matchRange) when isBooleanTest first second ->

        let holdsADirective: bool =
            directives
            |> List.exists (fun (directive: range) -> Range.rangeContainsRange matchRange directive)

        if holdsADirective then None else Some matchRange
    | _ -> None

// Reported on the whole match expression, because the whole of it is what goes.
let analyze (parsedInput: ParsedInput) : Message list =
    let _, directives = triviaOf parsedInput
    let findings: ResizeArray<range> = ResizeArray<range>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match shouldBeAnIf directives expr with
                | None -> ()
                | Some matchRange -> findings.Add matchRange
        }

    walkAst walker parsedInput

    findings
    |> Seq.map (fun (matchRange: range) ->
        {
            Type = Name
            Message =
                "Write this as an `if`. A `match` is for taking a value apart, and this one only asks whether a boolean is true, which `if ... then ... else ...` says with no pattern in sight and the scrutinee still written once."
            Code = Code
            Severity = Severity.Warning
            Range = matchRange
            Fixes = []
        }
    )
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }
