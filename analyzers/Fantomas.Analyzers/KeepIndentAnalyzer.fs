module Fantomas.Analyzers.KeepIndentAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-KEEPINDENT-001"

[<Literal>]
let Name: string = "KeepIndentAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a last branch whose body is a block indented a level past the expression it belongs to, where that indentation could be kept instead."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-keepindent-001"

// The expression a body really starts with, looking through the operators it is piped into.
//
// `{ ... } |> Some` is an application at the top and a record underneath, and the record is what
// indents things. What is to the right of the operator is a name, so what decides whether the body
// is a block is what stands on the left of the first one. `walkUp` in IgnoreFile.fs is the case that
// showed it, and the same reading covers `{ ... } :: rest`.
//
// A pipeline whose left hand side is itself an application is left where it was: `someCall a b |>
// Some` has nothing under it that a de-indent would save columns for, which is the whole reason a
// plain application is passed over.
let rec bodyHead (expr: SynExpr) : SynExpr =
    match expr with
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; argExpr = leftHandSide)) -> bodyHead leftHandSide
    | _ -> expr

// Whether a body is the kind of expression that goes on to indent things of its own.
//
// This is what the de-indent is for. A block holds bindings, statements, arms or items that each sit
// a level in again, so the four columns it saves are saved for everything inside it, and saved again
// by the next block in. A single application or pipeline has nothing under it to save them for, and
// reads oddly under the blank line the setting writes.
//
// A bracketed body counts as much as a `match` does. `[` holding a `for` loop indents everything in
// the loop twice over, and Fantomas holds it in the column of the bar like anything else. The
// `overruledLines` list in Report.fs is the case that showed it.
let isBlockBody (expr: SynExpr) : bool =
    match bodyHead expr with
    | SynExpr.Sequential _
    | SynExpr.LetOrUse _
    | SynExpr.Match _
    | SynExpr.MatchBang _
    | SynExpr.MatchLambda _
    | SynExpr.IfThenElse _
    | SynExpr.ArrayOrList _
    | SynExpr.ArrayOrListComputed _
    | SynExpr.Record _
    | SynExpr.AnonRecd _
    | SynExpr.ComputationExpr _
    | SynExpr.ObjExpr _ -> true
    | _ -> false

// Whether a range is a single line, which is how both halves of this rule measure a branch that
// gets out of the way. `FANTOMAS-ARMORDER-001` measures a short arm the same way.
let isOneLiner (range: range) : bool = range.EndLine = range.StartLine

// Whether anything follows the expression that keeping the indentation would take into its last
// branch.
//
// This is the one way the reshape changes meaning, and it is a question about the text rather than
// the tree. De-indenting moves the offside line of the body out to the column of the `|` or the
// `else`, so the first thing after the expression that starts in that column, or further right,
// stops following the expression and starts belonging to that branch. What that thing is does not
// matter: a statement of the enclosing block, an operator carrying the expression into a pipeline,
// a comment. Anything further left ends the branch exactly as it ended the expression before.
//
// Only lines after the one the expression ends on are asked about. Whatever shares that last line
// moves with the body and keeps its place, which is how a closing bracket on the same line stays
// out of this.
//
// Reading the source rather than the tree is deliberate, and is the third attempt. Collecting
// `SynExpr.Sequential` pairs missed the `json.WriteEndObject()` after the match in
// `writeDoctorFile`, which then ran for one case out of three. Flattening those sequences properly
// missed the `|> genNode attr` under the match in `genAttributesCore`, which applied to the whole
// match and would have applied to one arm, so everything reached through the other arm lost its
// trivia. Both were shapes to enumerate, and there was always going to be another. The text has
// none.
let followedByContentInColumn (source: ISourceText) (expressionRange: range) (column: int) : bool =
    let mutable line: int = expressionRange.EndLine
    let mutable answer: bool option = None

    while answer.IsNone && line < source.GetLineCount() do
        let text: string = source.GetLineString line
        let content: string = text.TrimStart()

        if content <> "" then
            answer <- Some(text.Length - content.Length >= column)

        line <- line + 1

    defaultArg answer false

// The column a match body has to start in for Fantomas to leave it there.
//
// `genKeepIdentMatchClause` compares the body against the bar, falling back to the pattern where
// there is none, so this is that same column and not a column of this rule's choosing. Only the
// last arm is ever asked, and the arm without a bar is the first one, but the fallback keeps the
// two definitions in step.
let keepIndentColumnOf (clause: SynMatchClause) : int =
    match clause with
    | SynMatchClause(pat = pattern; trivia = trivia) ->

    match trivia.BarRange with
    | Some bar -> bar.StartColumn
    | None -> pattern.Range.StartColumn

// The `then` bodies of an if chain, and its final `else` along with the trivia of the branch that
// owns it.
//
// `elif` nests in the tree: an `if` whose else is another `if` marked `IsElif`. Fantomas prints the
// chain flat and offers the choice to the last `else` alone, so the chain has to be walked to reach
// it, and the branches above are what decides whether the choice is worth taking.
let rec ifChain (expr: SynExpr) : SynExpr list * (SynExpr * SynExprIfThenElseTrivia) option =
    match expr with
    | SynExpr.IfThenElse(thenExpr = thenExpr; elseExpr = elseExpr; trivia = trivia) ->
        match elseExpr with
        | None -> [ thenExpr ], None
        | Some(SynExpr.IfThenElse(trivia = { IsElif = true }) as nested) ->
            let branches, final = ifChain nested
            thenExpr :: branches, final
        | Some elseBody -> [ thenExpr ], Some(elseBody, trivia)
    | _ -> [], None

// A branch this rule could speak about: the range of the whole expression it belongs to, the column
// its body would have to start in, and the body itself.
//
// A match and an if reach different printers and answer the column differently, one from the `|` and
// one from the `else`, but everything after that is the same question of the same shape, so both are
// reduced to this before it gets asked.
//
// The two conditions that belong here rather than there are the ones about the other branches. Every
// branch above the last has to be a one liner, which is the early return shape the setting exists
// for: the branches that decline say so and get out of the way, and what is left is the one path
// that carries on. An expression whose other branches are blocks too is not that shape, and
// de-indenting the last of them alone puts branches of the same kind at two different indentations
// and says the last is special when it is not. And the body has to be on a line of its own already,
// because a branch whose body shares the line of its `->` or its `else` has no indentation to keep.
let keepIndentCandidateOf (expr: SynExpr) : (range * int * SynExpr) option =
    match matchClausesOf expr with
    | Some(matchRange, clauses) ->
        match List.tryLast clauses with
        | Some(SynMatchClause(whenExpr = None; resultExpr = body; trivia = { ArrowRange = Some arrow }) as clause) ->
            let earlier: SynMatchClause list = List.truncate (clauses.Length - 1) clauses

            let othersAreOneLiners: bool =
                earlier |> List.forall (fun (SynMatchClause(range = range)) -> isOneLiner range)

            if othersAreOneLiners && body.Range.StartLine > arrow.EndLine then
                Some(matchRange, keepIndentColumnOf clause, body)
            else
                None
        | _ -> None
    | None ->

    // Only the outermost `if` of a chain is asked. An `elif` is reached again as a node of its own,
    // and answering there as well would report the same `else` twice.
    match expr with
    | SynExpr.IfThenElse(range = ifRange; trivia = { IsElif = false }) ->
        match ifChain expr with
        | branches, Some(elseBody, { ElseKeyword = Some elseKeyword }) ->
            let othersAreOneLiners: bool =
                branches |> List.forall (fun (branch: SynExpr) -> isOneLiner branch.Range)

            if othersAreOneLiners && elseBody.Range.StartLine > elseKeyword.EndLine then
                Some(ifRange, elseKeyword.StartColumn, elseBody)
            else
                None
        | _ -> None
    | _ -> None

// Whether a candidate should keep the indentation of the expression it belongs to.
//
// The body has to span more than the one line it starts on, because a body that fits beside its
// `->` or its `else` is pulled up next to it and never reaches the branch that would keep it. A
// conditional directive inside the expression means the branches this reads are not the branches
// every build sees, so those are left alone.
//
// A `when` guard is already gone by here, filtered out with the rest of the match shape: a multiline
// guard takes a path in `CodePrinter` that indents the body whatever its column, and whether a guard
// prints multiline is a page width question rather than a tree one, so all of them are passed over.
let shouldKeepIndent
    (source: ISourceText)
    (directives: range list)
    (expressionRange: range)
    (column: int)
    (body: SynExpr)
    : bool
    =
    let bodyRange: range = body.Range

    not (isOneLiner bodyRange)
    && bodyRange.StartColumn > column
    && isBlockBody body
    && not (List.exists (fun (directive: range) -> Range.rangeContainsRange expressionRange directive) directives)
    && not (followedByContentInColumn source expressionRange column)

// Reported on the body, which is the part that moves.
let analyze (source: ISourceText) (parsedInput: ParsedInput) : Message list =
    let _, directives = triviaOf parsedInput

    let candidates: ResizeArray<range * int * SynExpr> =
        ResizeArray<range * int * SynExpr>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match keepIndentCandidateOf expr with
                | None -> ()
                | Some candidate -> candidates.Add candidate
        }

    walkAst walker parsedInput

    candidates
    |> Seq.choose (fun (expressionRange: range, column: int, body: SynExpr) ->
        if not (shouldKeepIndent source directives expressionRange column body) then
            None
        else

        Some
            {
                Type = Name
                Message =
                    "Keep the indentation of this expression in its last branch. Every branch above it is a one liner, its own body is a block, and nothing follows the expression in this block, so the body can start in the column of the `|` or the `else` rather than a level in. `fsharp_experimental_keep_indent_in_branch` keeps it there, but only once it is written that way."
                Code = Code
                Severity = Severity.Warning
                Range = body.Range
                Fixes = []
            }
    )
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree }
