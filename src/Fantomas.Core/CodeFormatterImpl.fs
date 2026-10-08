[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open System.Text.RegularExpressions
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open MultipleDefineCombinations

let getSourceText (source: string) : ISourceText = source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<UnderDefines<ParsedInput> array> =
    // First get the syntax tree without any defines
    let baseUntypedTree, baseDiagnostics =
        Fantomas.FCS.Parse.parseFile isSignature source []

    let hashDirectives =
        match baseUntypedTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

    match hashDirectives with
    | [] ->
        async {
            let errors =
                baseDiagnostics
                |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

            if not errors.IsEmpty then
                raise (ParseException baseDiagnostics)

            return
                [|
                    {
                        Defines = DefineCombination.Empty
                        Value = baseUntypedTree
                    }
                |]
        }
    | hashDirectives ->
        let defineCombinations = Defines.getDefineCombination hashDirectives

        async {
            let! results =
                defineCombinations
                |> List.map (fun defineCombination ->
                    async {
                        // The combination without defines was already parsed to find the directives.
                        let untypedTree, diagnostics =
                            if defineCombination.Value.IsEmpty then
                                baseUntypedTree, baseDiagnostics
                            else
                                Fantomas.FCS.Parse.parseFile isSignature source defineCombination.Value

                        let errors =
                            diagnostics
                            |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

                        if not errors.IsEmpty then
                            return Error defineCombination.Value
                        else

                        return
                            Ok
                                {
                                    Defines = defineCombination
                                    Value = untypedTree
                                }
                    }
                )
                |> Async.Parallel

            let failures =
                results
                |> Array.choose (
                    function
                    | Error defines -> Some defines
                    | _ -> None
                )
                |> Array.toList

            if not failures.IsEmpty then
                raise (DefineParseException(failures))

            return
                results
                |> Array.choose (
                    function
                    | Ok result -> Some result
                    | _ -> None
                )
        }

[<NoComparison; NoEquality>]
type FormattedTree =
    {
        Oak: SyntaxOak.Oak
        Trivia: Trivia.RecordedTrivia
        Code: string
        Cursor: pos option
        SecondPass: bool
    }

// One tree formatted: the Oak it was printed from, the comments and directives the parser recorded
// in it, and what was printed.
let formatTree
    (ast: ParsedInput)
    (sourceText: ISourceText option)
    (config: FormatConfig)
    (cursor: pos option)
    : FormattedTree
    =
    let context = Context.Context.Create config

    let oak, recordedTrivia =
        match sourceText with
        | None ->
            ASTTransformer.mkOak None ast,
            {
                Trivia.RecordedTrivia.Comments = []
                Trivia.RecordedTrivia.Directives = []
            }
        | Some sourceText ->

        ASTTransformer.mkOak (Some sourceText) ast
        |> Trivia.enrichTree config sourceText ast

    let oak =
        match cursor with
        | None -> oak
        | Some cursor -> Trivia.insertCursor oak cursor

    let result: FormatResult = context |> CodePrinter.genFile oak |> Context.dump false

    {
        Oak = oak
        Trivia = recordedTrivia
        Code = result.Code
        Cursor = result.Cursor
        SecondPass = false
    }

// What a tree's code is to whoever does not need its Oak: a result that has not been checked.
let unchecked (tree: FormattedTree) : FormatResult =
    {
        Code = tree.Code
        Cursor = tree.Cursor
        Issues = []
    }

let formatAST
    (ast: ParsedInput)
    (sourceText: ISourceText option)
    (config: FormatConfig)
    (cursor: pos option)
    : FormatResult
    =
    formatTree ast sourceText config cursor |> unchecked

// ---- the checks of a formatted result, against what the parser recorded in the source ----

let conditionalDirectivesOf (tree: ParsedInput) : ConditionalDirectiveTrivia list =
    match tree with
    | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
    | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

let defineCombinationsOf (tree: ParsedInput) : string list list =
    match conditionalDirectivesOf tree with
    | [] -> [ [] ]
    | directives -> Defines.getDefineCombination directives |> List.map _.Value

/// `from` without one occurrence of each item of `taken`, matched on `key`, keeping the order of `from`.
let minusBy<'T, 'Key when 'Key: equality> (key: 'T -> 'Key) (from: 'T list) (taken: 'T list) : 'T list =
    taken
    |> List.fold
        (fun (left: 'T list) (item: 'T) ->
            match List.tryFindIndex (fun (candidate: 'T) -> key candidate = key item) left with
            | Some index -> List.removeAt index left
            | None -> left
        )
        from

/// What is left of `before` and of `after` once the most items they have in the same order are
/// paired up on `key`, each keeping its order. Of two pairings with as many pairs, the one with more
/// pairs `alike` wins, which tells apart two items with the same key that the order does not. Left
/// items with the same key on both sides are the same item moved, and are paired up as well.
let unpairedInOrder<'T, 'Key when 'Key: equality>
    (key: 'T -> 'Key)
    (alike: 'T -> 'T -> bool)
    (before: 'T list)
    (after: 'T list)
    : 'T list * 'T list
    =
    let before: 'T array = Array.ofList before
    let after: 'T array = Array.ofList after

    // A pair counts first, and being alike second.
    let pair: int = before.Length + after.Length + 1

    let worth (i: int) (j: int) : int =
        if key before[i] <> key after[j] then 0
        elif alike before[i] after[j] then pair + 1
        else pair

    // What the best pairing of `before` from `i` on and `after` from `j` on is worth. A table of the
    // two lengths, which only the comparison asks for, and only once the comments differ.
    let best: int array2d = Array2D.zeroCreate (before.Length + 1) (after.Length + 1)

    for i in before.Length - 1 .. -1 .. 0 do
        for j in after.Length - 1 .. -1 .. 0 do
            let paired: int =
                match worth i j with
                | 0 -> 0
                | worth -> best[i + 1, j + 1] + worth

            best[i, j] <- max paired (max best[i + 1, j] best[i, j + 1])

    let leftBefore: ResizeArray<'T> = ResizeArray()
    let leftAfter: ResizeArray<'T> = ResizeArray()
    let mutable i: int = 0
    let mutable j: int = 0

    while i < before.Length || j < after.Length do
        if
            i < before.Length
            && j < after.Length
            && worth i j > 0
            && best[i, j] = best[i + 1, j + 1] + worth i j
        then
            i <- i + 1
            j <- j + 1
        elif j = after.Length || i < before.Length && best[i + 1, j] >= best[i, j + 1] then
            leftBefore.Add before[i]
            i <- i + 1
        else
            leftAfter.Add after[j]
            j <- j + 1

    minusBy key (List.ofSeq leftBefore) (List.ofSeq leftAfter),
    minusBy key (List.ofSeq leftAfter) (List.ofSeq leftBefore)

// A comment spanning lines is read out of the source with the platform's line endings, as
// `GetSubTextFromRange` joins its lines with `Environment.NewLine`, and the result has the
// configuration's. Without carriage returns both read alike, and `SourceComment.Text` is the same on
// every platform.
let withoutCarriageReturns (text: string) : string = text.Replace("\r", "")

// What a comment is when comments are compared. A line comment may move between a line of its own
// and the end of a line of code, so both are one kind here. A block comment may not: on a line of its
// own it stays there, and beside code it stays beside code. Trailing whitespace is not printed.
let normalize (content: SyntaxOak.TriviaContent) : SyntaxOak.TriviaContent option =
    match content with
    | SyntaxOak.CommentOnSingleLine text
    | SyntaxOak.CommentOnSingleLineWithLeadingNewlines(_, text)
    | SyntaxOak.LineCommentAfterSourceCode text -> Some(SyntaxOak.CommentOnSingleLine(text.TrimEnd()))
    | SyntaxOak.BlockComment(text, _, _) -> Some(SyntaxOak.BlockComment(text.TrimEnd(), false, false))
    | SyntaxOak.Newline
    | SyntaxOak.Directive _
    | SyntaxOak.Cursor -> None

/// A comment as the checks compare it.
[<NoComparison; NoEquality>]
type ComparedComment =
    {
        /// What it is when comments are compared, see `normalize`.
        Compared: SyntaxOak.TriviaContent
        /// Where it is, with its text.
        Comment: SourceComment
        /// Whether it is on a line of its own rather than beside code. Not compared, as a line comment
        /// may move between the two, and so only telling apart two that are alike otherwise.
        OwnLine: bool
    }

/// The comments of `recordedTrivia`.
let commentsOf (recordedTrivia: Trivia.RecordedTrivia) : ComparedComment list =
    recordedTrivia.Comments
    |> List.choose (fun (trivia: SyntaxOak.TriviaNode) ->
        match normalize trivia.Content with
        | Some(SyntaxOak.CommentOnSingleLine text as content)
        | Some(SyntaxOak.BlockComment(text, _, _) as content) ->
            Some
                {
                    Compared = content
                    Comment =
                        {
                            Range = trivia.Range
                            Text = withoutCarriageReturns text
                        }
                    OwnLine =
                        match trivia.Content with
                        | SyntaxOak.CommentOnSingleLine _
                        | SyntaxOak.CommentOnSingleLineWithLeadingNewlines _ -> true
                        | _ -> false
                }
        | _ -> None
    )

/// The directives of `recordedTrivia`, in their order, as written with their whitespace made
/// single spaces.
let directivesOf (recordedTrivia: Trivia.RecordedTrivia) : string list =
    recordedTrivia.Directives
    |> List.choose (fun (trivia: SyntaxOak.TriviaNode) ->
        match trivia.Content with
        | SyntaxOak.Directive text -> Some(Regex.Replace(text, @"\s+", " ").Trim())
        | _ -> None
    )

let commentSearch (sourceTrivia: Trivia.RecordedTrivia list) (result: string) : ValidationIssue list =
    let result: string = withoutCarriageReturns result

    // Whether only whitespace follows `index` on its line of the result.
    let endsItsLine (index: int) : bool =
        let lineEnd: int =
            match result.IndexOf('\n', index) with
            | -1 -> result.Length
            | lineEnd -> lineEnd

        let mutable position: int = index

        while position < lineEnd && System.Char.IsWhiteSpace result[position] do
            position <- position + 1

        position = lineEnd

    // Whether only whitespace comes before `index` on its line of the result.
    let startsItsLine (index: int) : bool =
        let lineStart: int =
            if index = 0 then
                0
            else
                result.LastIndexOf('\n', index - 1) + 1

        let mutable position: int = lineStart

        while position < index && System.Char.IsWhiteSpace result[position] do
            position <- position + 1

        position = index

    // Where `comment` is in the result, at `from` or after. A line comment runs to the end of its line,
    // so its text counts only where nothing but whitespace follows: there it is the whole comment,
    // and not the start of a longer one, a URL or the inside of a string. A block comment beside code
    // is closed by its own `*)` and may have code after it, so its text inside a string of the result
    // counts too. That can let a lost block comment pass, and never reports one the result has.
    let rec find (content: SyntaxOak.TriviaContent) (comment: SourceComment) (from: int) : int =
        match result.IndexOf(comment.Text, from, System.StringComparison.Ordinal) with
        | -1 -> -1
        | index ->

        match content with
        | SyntaxOak.CommentOnSingleLine _ when not (endsItsLine (index + comment.Text.Length)) ->
            find content comment (index + 1)
        | _ -> index

    // Every place `comment` is in the result.
    let places (content: SyntaxOak.TriviaContent) (comment: SourceComment) : int array =
        [|
            let mutable index: int = find content comment 0

            while index <> -1 do
                yield index
                index <- find content comment (index + 1)
        |]

    // Formatting prints comments in the order of the source, so the search asks whether they are in
    // the result in that order, each at a place of its own: a comment found once cannot stand in for
    // two with the same text. Each is searched for after the one before it was found, which reads
    // `result` once and is all a result that keeps every comment needs.
    let allInOrder (comments: ComparedComment list) : bool =
        let mutable from: int = 0

        comments
        |> List.forall (fun (comment: ComparedComment) ->
            match find comment.Compared comment.Comment from with
            | -1 -> false
            | index ->

            from <- index + comment.Comment.Text.Length
            true
        )

    // Once one is not found, searching on from where the last one was found no longer tells which are
    // missing. A comment that is lost while its text is somewhere further on, a bare `//` or a rule of
    // dashes, would be found there, and every comment in between would count as missing too. So the
    // missing are what is left of the most comments the result has in their order, the longest chain
    // over every place each comment is. Of two chains as long, the one with more comments where they
    // were, on a line of their own or beside code, says which of two with the same text is missing.
    // A comment printed ahead of one it followed breaks the order, and one of the two is reported
    // although both are in the result.
    let missing (combination: Trivia.RecordedTrivia) : SourceComment list =
        let comments: ComparedComment array = commentsOf combination |> Array.ofList

        // A chain counts its comments first and those where they were second.
        let link: int64 = int64 comments.Length + 1L

        // Every chain, as its last comment and the chain it extends, -1 for none.
        let chains: ResizeArray<int * int> = ResizeArray()

        // For each end in the result, the best chain that ends there or before, as a Fenwick tree of
        // prefix maxima: its score, and the chain.
        let scores: int64 array = Array.zeroCreate (result.Length + 1)
        let lasts: int array = Array.create (result.Length + 1) -1

        let bestUntil (position: int) : int64 * int =
            let mutable at: int = position
            let mutable score: int64 = 0L
            let mutable last: int = -1

            while at > 0 do
                if scores[at] > score then
                    score <- scores[at]
                    last <- lasts[at]

                at <- at - (at &&& -at)

            score, last

        let record (ends: int) (score: int64) (chain: int) : unit =
            let mutable at: int = ends

            while at <= result.Length do
                if score > scores[at] then
                    scores[at] <- score
                    lasts[at] <- chain

                at <- at + (at &&& -at)

        for current in 0 .. comments.Length - 1 do
            let comment: ComparedComment = comments[current]

            // Every place is weighed against the chains before this comment, and recorded after, so
            // that no chain has this comment twice.
            let extended: (int * int64 * int) list =
                [
                    for place in places comment.Compared comment.Comment do
                        let score, previous = bestUntil place

                        let kept: int64 = if startsItsLine place = comment.OwnLine then 1L else 0L

                        chains.Add(current, previous)
                        yield place + comment.Comment.Text.Length, score + link + kept, chains.Count - 1
                ]

            for ends, score, chain in extended do
                record ends score chain

        let kept: Set<int> =
            let mutable chain: int = snd (bestUntil result.Length)

            set
                [
                    while chain <> -1 do
                        let comment, previous = chains[chain]
                        yield comment
                        chain <- previous
                ]

        [
            for index, comment in Array.indexed comments do
                if not (kept.Contains index) then
                    yield comment.Comment
        ]

    let notFound (combination: Trivia.RecordedTrivia) : SourceComment list =
        if allInOrder (commentsOf combination) then
            []
        else
            missing combination

    // Once per define combination: the result holds every branch in the order of the source, and a
    // comment in an inactive branch is not a comment under that combination. A comment outside every
    // `#if` is in the tree of every combination, and is told apart from another with the same text by
    // where it is.
    sourceTrivia
    |> List.collect notFound
    |> List.distinctBy (fun (comment: SourceComment) -> comment.Range.Start)
    |> List.sortBy (fun (comment: SourceComment) -> comment.Range.StartLine, comment.Range.StartColumn)
    |> List.map ValidationIssue.MissingComment

let triviaChanges
    (defines: string list)
    (before: Trivia.RecordedTrivia)
    (after: Trivia.RecordedTrivia)
    : ValidationIssue list
    =
    let commentsBefore: ComparedComment list = commentsOf before
    let commentsAfter: ComparedComment list = commentsOf after
    let directivesBefore: string list = directivesOf before
    let directivesAfter: string list = directivesOf after

    [
        // Compared without their order, which formatting is free to change by moving a comment to
        // the other side of a node, and with their number and kind, which it is not. Which of several
        // with the same text is missing is told by the order.
        if
            List.sort (List.map _.Compared commentsBefore)
            <> List.sort (List.map _.Compared commentsAfter)
        then
            let missing, added =
                unpairedInOrder
                    _.Compared
                    (fun (comment: ComparedComment) (other: ComparedComment) -> comment.OwnLine = other.OwnLine)
                    commentsBefore
                    commentsAfter

            ValidationIssue.CommentsChanged(
                defines,
                List.map _.Comment missing,
                List.map (fun (comment: ComparedComment) -> comment.Comment.Text) added
            )

        if directivesBefore <> directivesAfter then
            ValidationIssue.DirectivesChanged(
                defines,
                minusBy id directivesBefore directivesAfter,
                minusBy id directivesAfter directivesBefore
            )
    ]

let hasErrors (diagnostics: FSharpParserDiagnostic list) : bool =
    diagnostics
    |> List.exists (fun (diagnostic: FSharpParserDiagnostic) -> diagnostic.Severity = FSharpDiagnosticSeverity.Error)

// The result parsed, for the checks that read it. It is parsed only for a check that asks, and once
// per combination for all of them. It is valid under its own combinations and compared with the source
// under the source's. Those are the same unless a directive was lost.
[<NoComparison; NoEquality>]
type ParsedResult =
    {
        Text: Lazy<ISourceText>
        Combinations: Lazy<string list list>
        Under: string list -> ParsedInput * FSharpParserDiagnostic list
    }

let parseResult (isSignature: bool) (sourceCombinations: string list list) (result: string) : ParsedResult =
    let resultText: Lazy<ISourceText> = lazy (SourceText.ofString result)

    let resultWithoutDefines: Lazy<ParsedInput * FSharpParserDiagnostic list> =
        lazy (parseFile isSignature resultText.Value [])

    let resultCombinations: Lazy<string list list> =
        lazy (defineCombinationsOf (fst resultWithoutDefines.Value))

    let resultParses: Lazy<Map<string list, Lazy<ParsedInput * FSharpParserDiagnostic list>>> =
        lazy
            (List.distinct (resultCombinations.Value @ sourceCombinations)
             |> List.map (fun (defines: string list) ->
                 defines,
                 lazy
                     (if List.isEmpty defines then
                          resultWithoutDefines.Value
                      else
                          parseFile isSignature resultText.Value defines)
             )
             |> Map.ofList)

    {
        Text = resultText
        Combinations = resultCombinations
        Under = fun (defines: string list) -> resultParses.Value[defines].Value
    }

// The checks that read the result: `CommentSearch`, `Parse` and `TriviaComparison`.
let resultIssues
    (config: FormatConfig)
    (validations: Validations)
    (sourceTrivia: UnderDefines<Trivia.RecordedTrivia> list)
    (parsed: ParsedResult)
    (result: string)
    : ValidationIssue list
    =
    [
        if validations.HasFlag Validations.CommentSearch then
            yield! commentSearch (sourceTrivia |> List.map _.Value) result

        if validations.HasFlag Validations.Parse then
            for defines in parsed.Combinations.Value do
                match Validation.invalidatingDiagnostics (snd (parsed.Under defines)) with
                | [] -> ()
                | diagnostics -> yield ValidationIssue.NotValidFSharp(defines, diagnostics)

        // The result's trivia is read the way formatting reads the source's, through its Oak, which
        // only a tree without parse errors has. Where the result has them, `Parse` says what they are.
        // Reading it can raise where reading the source did not: `mkOak` rejects a tree its model has
        // no place for, and a wrong result is where to expect one. That is reported rather than
        // raised, as formatting itself went fine.
        if validations.HasFlag Validations.TriviaComparison then
            for before in sourceTrivia do
                let defines: string list = before.Defines.Value
                let tree, diagnostics = parsed.Under defines

                if not (hasErrors diagnostics) then
                    let changes: ValidationIssue list =
                        try
                            let _, after =
                                ASTTransformer.mkOak (Some parsed.Text.Value) tree
                                |> Trivia.enrichTree config parsed.Text.Value tree

                            triviaChanges defines before.Value after
                        with error ->
                            [ ValidationIssue.CheckFailed(Validations.TriviaComparison, error) ]

                    yield! changes
    ]

// Without copying either: the check runs on every format, and both can be large.
let sameApartFromTrailingWhitespace (text: string) (other: string) : bool =
    System.MemoryExtensions.SequenceEqual(
        System.MemoryExtensions.TrimEnd(System.MemoryExtensions.AsSpan text),
        System.MemoryExtensions.TrimEnd(System.MemoryExtensions.AsSpan other)
    )

let checkResult
    (formatAgain: string -> Async<string>)
    (config: FormatConfig)
    (isSignature: bool)
    (validations: Validations)
    (source: ISourceText)
    (sourceTrivia: UnderDefines<Trivia.RecordedTrivia> list)
    (result: string)
    : Async<ValidationIssue list>
    =
    async {
        // A result that is the source has nothing formatting did to it to check. Trailing whitespace
        // aside: the source is read without it, and formatting ends the result in a newline.
        if
            validations = Validations.None
            || sameApartFromTrailingWhitespace result (source.GetSubTextString(0, source.Length))
        then
            return []
        else

        let parsed: ParsedResult =
            parseResult
                isSignature
                (sourceTrivia
                 |> List.map (fun (trivia: UnderDefines<Trivia.RecordedTrivia>) -> trivia.Defines.Value))
                result

        let fromResult: ValidationIssue list =
            resultIssues config validations sourceTrivia parsed result

        // Formatting starts from a tree without errors under every combination, so a result that has
        // them is not formatted again: that would only fail on what `Parse` says.
        let parses () : bool =
            parsed.Combinations.Value
            |> List.forall (fun (defines: string list) -> not (hasErrors (snd (parsed.Under defines))))

        let! idempotencyIssues =
            if not (validations.HasFlag Validations.Idempotency) || not (parses ()) then
                async.Return []
            else
                async {
                    match! Async.Catch(formatAgain result) with
                    | Choice2Of2 error -> return [ ValidationIssue.CheckFailed(Validations.Idempotency, error) ]
                    | Choice1Of2 again when again <> result -> return [ ValidationIssue.NotIdempotent again ]
                    | Choice1Of2 _ -> return []
                }

        return fromResult @ idempotencyIssues
    }

let rec formatDocument
    (inspect: UnderDefines<FormattedTree> -> unit)
    (config: FormatConfig)
    (isSignature: bool)
    (source: ISourceText)
    (cursor: pos option)
    (validations: Validations)
    : Async<FormatResult>
    =
    async {
        let! asts = parse isSignature source

        // Each tree is handed to `inspect` in its own task, as soon as it is printed, so its Oak goes
        // when the task does. What is kept is what the merge and the checks need: the code, and the
        // comments and directives the parser recorded.
        let! results =
            asts
            |> Array.map (fun (ast: UnderDefines<ParsedInput>) ->
                async {
                    let tree: FormattedTree = formatTree ast.Value (Some source) config cursor
                    inspect (ast.Map(fun _ -> tree))
                    return ast.Map(fun _ -> tree.Trivia, unchecked tree)
                }
            )
            |> Async.Parallel
            |> Async.map Array.toList

        let merged: FormatResult =
            match results with
            | [] -> failwith "not possible"
            | [ single ] -> snd single.Value
            | all ->

            all
            |> List.map (fun (tree: UnderDefines<Trivia.RecordedTrivia * FormatResult>) -> tree.Map snd)
            |> mergeMultipleFormatResults config

        // The second pass gets the same inspection as the first, its trees marked as its own.
        let formatAgain (code: string) : Async<string> =
            formatDocument
                (fun (tree: UnderDefines<FormattedTree>) ->
                    inspect (tree.Map(fun (formatted: FormattedTree) -> { formatted with SecondPass = true }))
                )
                config
                isSignature
                (getSourceText code)
                None
                Validations.None
            |> Async.map (fun (again: FormatResult) -> again.Code)

        let! issues =
            checkResult
                formatAgain
                config
                isSignature
                validations
                source
                (results
                 |> List.map (fun (tree: UnderDefines<Trivia.RecordedTrivia * FormatResult>) -> tree.Map fst))
                merged.Code

        return { merged with Issues = issues }
    }
