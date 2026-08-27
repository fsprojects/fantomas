module Fantomas.Analyzers.KeepIndentAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-KEEPINDENT-001"

[<Literal>]
let Name: string = "KeepIndentAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a last match arm whose body is a block indented a level past the match, where the indentation of the match could be kept instead."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-keepindent-001"

// The arms of everything that prints as a list of match arms, with the range of the expression they
// belong to. `match`, `match!` and `function` all reach the same clause printer in `CodePrinter`, so
// the last arm of each is one `fsharp_experimental_keep_indent_in_branch` can speak about.
let matchClausesOf (expr: SynExpr) : (range * SynMatchClause list) option =
    match expr with
    | SynExpr.Match(clauses = clauses; range = range)
    | SynExpr.MatchBang(clauses = clauses; range = range)
    | SynExpr.MatchLambda(matchClauses = clauses; range = range) -> Some(range, clauses)
    | _ -> None

// The column a body has to start in for Fantomas to leave it there.
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

// Whether a body is the kind of expression that goes on to indent things of its own.
//
// This is what the de-indent is for. A block holds bindings, statements or arms that each sit a
// level in again, so the four columns it saves are saved for everything inside it, and saved again
// by the next block in. A single application or pipeline has nothing under it to save them for, and
// reads oddly under the blank line the setting writes.
let isBlockBody (expr: SynExpr) : bool =
    match expr with
    | SynExpr.Sequential _
    | SynExpr.LetOrUse _
    | SynExpr.Match _
    | SynExpr.MatchBang _
    | SynExpr.MatchLambda _
    | SynExpr.IfThenElse _ -> true
    | _ -> false

// Whether the last arm is the only one that is not a one liner.
//
// The setting is for the early return shape, which is what makes the de-indent read as anything at
// all: the arms that decline say so on one line and get out of the way, and what is left is the one
// path that carries on, written at the indentation it started at. A match whose other arms are
// blocks too is not that shape, and de-indenting the last of them alone puts arms of the same kind
// at two different indentations, saying the last one is special when it is not.
//
// `collectTriviaFromCodeComments` in Trivia.fs is the case that showed it: both of its arms are a
// dozen lines, and de-indenting the second read as a mistake rather than as a happy path.
//
// One liner is measured on the whole clause, which is how `FANTOMAS-ARMORDER-001` measures it, and
// that rule is what puts those arms first to begin with. A match with a single arm, destructuring a
// single case union, qualifies: there is no other arm for it to be out of step with.
let onlyLastArmIsBlock (clauses: SynMatchClause list) : bool =
    match List.rev clauses with
    | [] -> false
    | _ :: earlier ->
        earlier
        |> List.forall (fun (SynMatchClause(range = range)) -> range.EndLine = range.StartLine)

// Whether anything follows the match that keeping the indentation would take into the last arm.
//
// This is the one way the reshape changes meaning, and it is a question about the text rather than
// the tree. De-indenting moves the offside line of the body out to the bar, so the first thing
// after the match that starts in that column, or further right, stops following the match and
// starts belonging to its last arm. What that thing is does not matter: a statement of the
// enclosing block, an operator carrying the match into a pipeline, a comment. Anything further left
// ends the arm exactly as it ended the match before.
//
// Only lines after the one the match ends on are asked about. Whatever shares that last line moves
// with the body and keeps its place, which is how a closing bracket on the same line stays out of
// this.
//
// Reading the source rather than the tree is deliberate, and is the third attempt. Collecting
// `SynExpr.Sequential` pairs missed the `json.WriteEndObject()` after the match in
// `writeDoctorFile`, which then ran for one case out of three. Flattening those sequences properly
// missed the `|> genNode attr` under the match in `genAttributesCore`, which applied to the whole
// match and would have applied to one arm, so every attribute reached through the other arm lost
// its trivia. Both were shapes to enumerate, and there was always going to be another. The text has
// none.
let followedByContentInColumn (source: ISourceText) (matchRange: range) (column: int) : bool =
    let mutable line: int = matchRange.EndLine
    let mutable answer: bool option = None

    while answer.IsNone && line < source.GetLineCount() do
        let text: string = source.GetLineString line
        let content: string = text.TrimStart()

        if content <> "" then
            answer <- Some(text.Length - content.Length >= column)

        line <- line + 1

    defaultArg answer false

// Whether the last arm of a match should keep the indentation of the match.
//
// A `when` guard is passed over: a multiline guard takes a different path in `CodePrinter` that
// indents the body whatever its column, so the rule would be asking for something the formatter
// then undoes. Which guards print multiline is a page width question rather than a tree one, so all
// of them are left alone.
//
// The body has to be on a line of its own already, because an arm whose body shares the line of its
// arrow has no indentation to keep, and it has to span more than that one line, because a body that
// fits beside the arrow is pulled up next to it and never reaches the branch that would keep it.
let shouldKeepIndent
    (source: ISourceText)
    (directives: range list)
    (matchRange: range)
    (clauses: SynMatchClause list)
    (clause: SynMatchClause)
    : bool
    =
    match clause with
    | SynMatchClause(whenExpr = Some _) -> false
    | SynMatchClause(resultExpr = body; trivia = { ArrowRange = Some arrow }) ->
        let column: int = keepIndentColumnOf clause
        let bodyRange: range = body.Range

        onlyLastArmIsBlock clauses
        && bodyRange.StartLine > arrow.EndLine
        && bodyRange.EndLine > bodyRange.StartLine
        && bodyRange.StartColumn > column
        && isBlockBody body
        && not (List.exists (fun (directive: range) -> Range.rangeContainsRange matchRange directive) directives)
        && not (followedByContentInColumn source matchRange column)
    | _ -> false

// Reported on the body, which is the part that moves.
let analyze (source: ISourceText) (parsedInput: ParsedInput) : Message list =
    let _, directives = triviaOf parsedInput

    let candidates: ResizeArray<range * SynMatchClause list> =
        ResizeArray<range * SynMatchClause list>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match matchClausesOf expr with
                | None -> ()
                | Some(matchRange, clauses) -> candidates.Add(matchRange, clauses)
        }

    walkAst walker parsedInput

    candidates
    |> Seq.choose (fun (matchRange: range, clauses: SynMatchClause list) ->
        match List.tryLast clauses with
        | None -> None
        | Some clause ->

        if not (shouldKeepIndent source directives matchRange clauses clause) then
            None
        else

        match clause with
        | SynMatchClause(resultExpr = body) ->
            Some
                {
                    Type = Name
                    Message =
                        "Keep the indentation of the match in this arm. It is the last one, its body is a block, the arms above it are one liners, and nothing follows the match in this block, so the body can start in the column of the `|` rather than a level in. `fsharp_experimental_keep_indent_in_branch` keeps it there, but only once it is written that way."
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
