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

// Where one expression is followed by another in the same block: the range of the first, and the
// column the second starts in.
let followerOf (expr: SynExpr) : (range * int) option =
    match expr with
    | SynExpr.Sequential(expr1 = before; expr2 = after)
    | SynExpr.SequentialOrImplicitYield(expr1 = before; expr2 = after) -> Some(before.Range, after.Range.StartColumn)
    | _ -> None

// Whether keeping the indentation would pull in code that follows the match.
//
// This is the one way the reshape changes meaning, and the only reason the rule needs to look
// further than the arm. De-indenting moves the offside line of the body out to the bar, and code
// that follows the match in the same block starts in that same column. What was the first thing
// past the match becomes the last thing inside the arm: it stops running whatever matched and
// starts running only for that one arm.
//
// So every place one expression is followed by another is collected, and a match sitting inside the
// first of such a pair is left alone. Unless the second starts further left than the bar, which is
// an enclosing block continuing rather than this one, and still ends the arm afterwards.
//
// A following arm of the same match needs no such care. Only the last arm is ever a candidate, so
// there is never one of those left to swallow.
let wouldSwallowFollowingCode (followers: (range * int) list) (matchRange: range) (column: int) : bool =
    followers
    |> List.exists (fun (before: range, followerColumn: int) ->
        followerColumn >= column && Range.rangeContainsRange before matchRange
    )

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
    (directives: range list)
    (followers: (range * int) list)
    (matchRange: range)
    (clause: SynMatchClause)
    : bool
    =
    match clause with
    | SynMatchClause(whenExpr = Some _) -> false
    | SynMatchClause(resultExpr = body; trivia = { ArrowRange = Some arrow }) ->
        let column: int = keepIndentColumnOf clause
        let bodyRange: range = body.Range

        bodyRange.StartLine > arrow.EndLine
        && bodyRange.EndLine > bodyRange.StartLine
        && bodyRange.StartColumn > column
        && isBlockBody body
        && not (List.exists (fun (directive: range) -> Range.rangeContainsRange matchRange directive) directives)
        && not (wouldSwallowFollowingCode followers matchRange column)
    | _ -> false

// Reported on the body, which is the part that moves.
let analyze (parsedInput: ParsedInput) : Message list =
    let _, directives = triviaOf parsedInput
    let followers: ResizeArray<range * int> = ResizeArray<range * int>()

    let candidates: ResizeArray<range * SynMatchClause> =
        ResizeArray<range * SynMatchClause>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match followerOf expr with
                | None -> ()
                | Some follower -> followers.Add follower

                match matchClausesOf expr with
                | None -> ()
                | Some(matchRange, clauses) ->
                    match List.tryLast clauses with
                    | None -> ()
                    | Some clause -> candidates.Add(matchRange, clause)
        }

    walkAst walker parsedInput

    // Judged after the walk rather than during it. A match is reached before the sequence that holds
    // it in some shapes and after it in others, so anything deciding on the followers has to wait
    // until all of them are in.
    let followers: (range * int) list = List.ofSeq followers

    candidates
    |> Seq.choose (fun (matchRange: range, clause: SynMatchClause) ->
        if not (shouldKeepIndent directives followers matchRange clause) then
            None
        else

        match clause with
        | SynMatchClause(resultExpr = body) ->
            Some
                {
                    Type = Name
                    Message =
                        "Keep the indentation of the match in this arm. It is the last one, its body is a block, and nothing follows the match in this block, so the body can start in the column of the `|` rather than a level in. `fsharp_experimental_keep_indent_in_branch` keeps it there, but only once it is written that way."
                    Code = Code
                    Severity = Severity.Warning
                    Range = body.Range
                    Fixes = []
                }
    )
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }
