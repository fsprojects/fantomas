module Fantomas.Analyzers.ArmOrderAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

[<Literal>]
let Code: string = "FANTOMAS-ARMORDER-001"

[<Literal>]
let Name: string = "ArmOrderAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a two armed match whose long arm comes first, where the arms are disjoint and swapping them cannot change meaning."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-armorder-001"

// The comments and the conditional directives of a file, which are the two things that make a
// textual swap of two arms something other than a swap.
let triviaOf (parsedInput: ParsedInput) : range list * range list =
    let comments, directives =
        match parsedInput with
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) -> trivia.CodeComments, trivia.ConditionalDirectives
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) ->
            trivia.CodeComments, trivia.ConditionalDirectives

    let commentRanges: range list =
        comments
        |> List.map (fun (comment: CommentTrivia) ->
            match comment with
            | CommentTrivia.LineComment range -> range
            | CommentTrivia.BlockComment range -> range)

    let directiveRanges: range list =
        directives
        |> List.map (fun (directive: ConditionalDirectiveTrivia) ->
            match directive with
            | ConditionalDirectiveTrivia.Else range -> range
            | ConditionalDirectiveTrivia.EndIf range -> range
            | ConditionalDirectiveTrivia.If(range = range) -> range)

    commentRanges, directiveRanges

// The final identifier of a pattern that heads an arm, when that pattern is one this rule is
// willing to reason about.
//
// Requiring a `SynPat.LongIdent` is what does most of the safety work here. It admits union cases
// and literal patterns, and it excludes `SynPat.Wild` and a bare `SynPat.Named`, which are the two
// patterns that match anything and therefore have to stay last. Nested wildcards are fine, so
// `Some _` still qualifies.
let caseNameOf (pattern: SynPat) : string option =
    match pattern with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = identifiers)) ->
        identifiers |> List.tryLast |> Option.map (fun (ident: Ident) -> ident.idText)
    | _ -> None

// Whether two arms can never match the same value, which is what makes reordering them sound.
// Two distinct case identifiers cannot both match, so comparing the names settles it.
let areDisjoint (first: SynPat) (second: SynPat) : bool =
    match caseNameOf first, caseNameOf second with
    | Some firstName, Some secondName -> firstName <> secondName
    | _ -> false

// Whether a range falls in the gap between two arms.
//
// A comment inside an arm travels with that arm and is no reason to stay quiet. A comment between
// them belongs to neither once they have moved, so it is.
let fallsBetween (before: range) (after: range) (candidate: range) : bool =
    Position.posGeq candidate.Start before.End
    && Position.posGeq after.Start candidate.End

// Whether these two arms should be the other way around.
//
// Everything here is a reason to stay quiet, and the rule only speaks when none of them applies.
// A `when` guard means the first arm can decline and fall through to the second, so the arms are
// not disjoint and the order carries meaning; guarded matches are out for good rather than until
// a later stage. Measuring the whole clause rather than its body keeps "shorter" free of
// argument: a one line pattern with a four line body is not the short arm, and neither is a four
// line pattern with a one line body.
let shouldSwap
    (comments: range list)
    (directives: range list)
    (matchRange: range)
    (first: SynMatchClause)
    (second: SynMatchClause)
    : bool =
    match first, second with
    | SynMatchClause(pat = firstPattern; whenExpr = None; range = firstRange),
      SynMatchClause(pat = secondPattern; whenExpr = None; range = secondRange) ->
        firstRange.EndLine > firstRange.StartLine
        && secondRange.EndLine = secondRange.StartLine
        && areDisjoint firstPattern secondPattern
        && not (List.exists (fun (directive: range) -> Range.rangeContainsRange matchRange directive) directives)
        && not (List.exists (fun (comment: range) -> fallsBetween firstRange secondRange comment) comments)
    | _ -> false

// Reported on the one line arm, which is the one that moves.
let analyze (parsedInput: ParsedInput) : Message list =
    let comments, directives = triviaOf parsedInput
    let findings: ResizeArray<range * string> = ResizeArray<range * string>()

    let walker: SyntaxCollectorBase =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path: SyntaxVisitorPath, expr: SynExpr) : unit =
                match expr with
                | SynExpr.Match(clauses = [ first; second ]; range = matchRange) when
                    shouldSwap comments directives matchRange first second
                    ->
                    match second with
                    | SynMatchClause(pat = pattern; range = clauseRange) ->
                        let name: string = defaultArg (caseNameOf pattern) "this"
                        findings.Add(clauseRange, name)
                | _ -> ()
        }

    walkAst walker parsedInput

    findings
    |> Seq.map (fun (clause: range, name: string) ->
        {
            Type = Name
            Message =
                $"Put the shorter arm first. `%s{name}` is a one liner and the arm above it is not, and the two cannot both match, so swapping them changes nothing but the reading."
            Code = Code
            Severity = Severity.Warning
            Range = clause
            Fixes = []
        })
    |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }

let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return analyze ctx.ParseFileResults.ParseTree }
