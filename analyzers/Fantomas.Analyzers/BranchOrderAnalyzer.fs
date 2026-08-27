module Fantomas.Analyzers.BranchOrderAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-BRANCHORDER-001"

[<Literal>]
let Name: string = "BranchOrderAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects an if expression whose long branch comes first, where negating the condition would put the one line branch first instead."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-branchorder-001"

// Whether a condition is one this rule is willing to ask somebody to turn around.
//
// An `if` can always be reversed on paper, but not every reversal reads as well as the original.
// A comparison flips into its opposite and a `not` falls away, and both of those are still one
// thing to read. A condition joined by `&&` or `||` has to grow a `not` and a pair of parentheses
// around the whole of it, which is a worse sentence than the branches were worth, so those are left
// alone. So is anything else: a bare call, a property, a pattern test. Those can only gain a `not`,
// which is fine, and they are the common case.
//
// The operator arrives as a `SynExpr.App` of a `SynExpr.LongIdent` holding the compiled name, which
// is how `op_BooleanAnd` and a spelled out `(&&)` both land here.
let isReversibleCondition (condition: SynExpr) : bool =
    let rec joinsWithBooleanOperator (expr: SynExpr) : bool =
        match expr with
        | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
            joinsWithBooleanOperator funcExpr || joinsWithBooleanOperator argExpr
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = identifiers)) ->
            match List.tryLast identifiers with
            | None -> false
            | Some name -> name.idText = "op_BooleanAnd" || name.idText = "op_BooleanOr"
        | SynExpr.Paren _ -> false
        | _ -> false

    not (joinsWithBooleanOperator condition)

// Whether an if expression has its branches the wrong way around.
//
// Everything here is a reason to stay quiet, and the rule only speaks when none of them applies.
// There has to be an `else`, and no `elif`: a chain has more than two ways through it and no single
// swap that puts the short one first. The `then` branch has to run to more than one line while the
// `else` branch fits on one, which is the same measure `FANTOMAS-ARMORDER-001` uses. And a
// conditional directive inside means the branches this reads are not the branches every build sees.
//
// Unlike a match arm there is nothing to prove about overlap. The two branches of an `if` are
// exclusive by construction, so reversing the condition cannot change which one runs. What it can
// change is how the condition reads, which is what `isReversibleCondition` is about.
let shouldReverse (directives: range list) (expr: SynExpr) : (range * range) option =
    match expr with
    | SynExpr.IfThenElse(
        ifExpr = condition; thenExpr = thenExpr; elseExpr = Some elseExpr; range = ifRange; trivia = { IsElif = false }) ->

        let isChain: bool =
            match elseExpr with
            | SynExpr.IfThenElse _ -> true
            | _ -> false

        let longBranchComesFirst: bool =
            thenExpr.Range.EndLine > thenExpr.Range.StartLine
            && elseExpr.Range.EndLine = elseExpr.Range.StartLine

        let holdsADirective: bool =
            directives
            |> List.exists (fun (directive: range) -> Range.rangeContainsRange ifRange directive)

        if
            isChain
            || holdsADirective
            || not longBranchComesFirst
            || not (isReversibleCondition condition)
        then
            None
        else

        Some(elseExpr.Range, condition.Range)
    | _ -> None

// Reported on the one line branch, which is the one that moves.
let analyze (parsedInput: ParsedInput) : Message list =
    let _, directives = triviaOf parsedInput
    let findings: ResizeArray<range> = ResizeArray<range>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match shouldReverse directives expr with
                | None -> ()
                | Some(elseRange, _) -> findings.Add elseRange
        }

    walkAst walker parsedInput

    findings
    |> Seq.map (fun (elseRange: range) ->
        {
            Type = Name
            Message =
                "Put the shorter branch first. This `else` is a one liner and the `then` above it is not, so negating the condition and swapping the two changes nothing but the reading, and leaves the branch that carries on last where `FANTOMAS-KEEPINDENT-001` can reach it."
            Code = Code
            Severity = Severity.Warning
            Range = elseRange
            Fixes = []
        }
    )
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }
