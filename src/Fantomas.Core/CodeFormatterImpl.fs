[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open System
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Text.Range
open Fantomas.Core
open Fantomas.Core.FormatConfig
open Fantomas.Core.SourceParser
open Fantomas.Core.TriviaTypes
open Fantomas.Core.CodePrinter

let getSourceText (source: string) : ISourceText = source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<(ParsedInput * DefineCombination)[]> =
    // First get the syntax tree without any defines
    let baseUntypedTree, baseDiagnostics =
        Fantomas.FCS.Parse.parseFile isSignature source []

    let hashDirectives =
        match baseUntypedTree with
        | ImplFile (ParsedImplFileInput (_, _, directives, _))
        | SigFile (ParsedSigFileInput (_, _, directives, _)) -> directives

    match hashDirectives with
    | [] ->
        async {
            let errors =
                baseDiagnostics
                |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

            if not errors.IsEmpty then
                raise (FormatException $"Parsing failed with errors: %A{baseDiagnostics}\nAnd options: %A{[]}")

            return [| (baseUntypedTree, []) |]
        }
    | hashDirectives ->
        let defineCombinations = Defines.getDefineCombination hashDirectives

        defineCombinations
        |> List.map (fun defineCombination ->
            async {
                let untypedTree, diagnostics =
                    Fantomas.FCS.Parse.parseFile isSignature source defineCombination

                let errors =
                    diagnostics
                    |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

                if not errors.IsEmpty then
                    raise (FormatException $"Parsing failed with errors: %A{diagnostics}\nAnd options: %A{[]}")

                return (untypedTree, defineCombination)
            })
        |> Async.Parallel

let formatWith (sourceAndTokens: ISourceText option) (defineCombination: DefineCombination) ast config =
    let formattedSourceCode =
        let context = Context.Context.Create config sourceAndTokens defineCombination ast

        context
        |> genParsedInput ASTContext.Default ast
        |> Context.dump

    formattedSourceCode

let format (config: FormatConfig) (isSignature: bool) (source: ISourceText) : Async<string> =
    async {
        let! asts = parse isSignature source

        let! results =
            asts
            |> Array.map (fun (ast', defineCombination) ->
                async {
                    let formattedCode = formatWith (Some source) defineCombination ast' config
                    return (defineCombination, formattedCode)
                })
            |> Async.Parallel
            |> Async.map Array.toList

        let merged =
            match results with
            | [] -> failwith "not possible"
            | [ (_, x) ] -> x
            | all ->
                let allInFragments =
                    all
                    |> String.splitInFragments config.EndOfLine.NewLineString

                let allHaveSameFragmentCount =
                    let allWithCount = List.map (fun (_, f: string list) -> f.Length) allInFragments

                    (Set allWithCount).Count = 1

                if not allHaveSameFragmentCount then
                    let chunkReport =
                        allInFragments
                        |> List.map (fun (defines, fragments) ->
                            sprintf "[%s] has %i fragments" (String.concat ", " defines) fragments.Length)
                        |> String.concat config.EndOfLine.NewLineString

                    failwithf
                        """Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back together.
%s
Please raise an issue at https://fsprojects.github.io/fantomas-tools/#/fantomas/preview."""
                        chunkReport

                List.map snd allInFragments
                |> List.reduce String.merge
                |> String.concat config.EndOfLine.NewLineString

        return merged
    }

/// Format a source string using given config
let formatDocument config isSignature source = format config isSignature source

/// Format an abstract syntax tree using given config
let formatAST ast defines sourceAndTokens config =
    formatWith sourceAndTokens defines ast config

/// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
let makeRange fileName startLine startCol endLine endCol =
    mkRange fileName (mkPos startLine startCol) (mkPos endLine endCol)

/// Get first non-whitespace line
let rec getStartLineIndex (lines: _[]) i =
    if i = lines.Length - 1
       || not <| String.IsNullOrWhiteSpace(lines.[i]) then
        i
    else
        getStartLineIndex lines (i + 1)

let rec getEndLineIndex (lines: _[]) i =
    if i = 0
       || not <| String.IsNullOrWhiteSpace(lines.[i]) then
        i
    else
        getEndLineIndex lines (i - 1)

//let isSignificantToken (tok: FSharpTokenInfo) =
//    tok.CharClass <> FSharpTokenCharKind.WhiteSpace
//    && tok.CharClass <> FSharpTokenCharKind.LineComment
//    && tok.CharClass <> FSharpTokenCharKind.Comment
//    && tok.TokenName <> "STRING_TEXT"

// Find out the start token
//let rec getStartCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
//    match tokenizer.ScanToken(!lexState) with
//    | Some tok, state ->
//        if tok.RightColumn >= r.StartColumn
//           && isSignificantToken tok then
//            tok.LeftColumn
//        else
//            lexState := state
//            getStartCol r tokenizer lexState
//    | None, _ -> r.StartColumn

/// Find out the end token
//let rec getEndCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
//    match tokenizer.ScanToken(!lexState) with
//    | Some tok, state ->
//        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)
//
//        if tok.RightColumn >= r.EndColumn
//           && isSignificantToken tok then
//            tok.RightColumn
//        else
//            lexState := state
//            getEndCol r tokenizer lexState
//    | None, _ -> r.EndColumn

//type PatchKind =
//    | TypeMember
//    | RecType
//    | RecLet
//    | Nothing
//
//let startWithMember (sel: string) =
//    [| "member"
//       "abstract"
//       "default"
//       "override"
//       "static"
//       "interface"
//       "new"
//       "val"
//       "inherit" |]
//    |> Array.exists (sel.TrimStart().StartsWith)
//
///// Find the first type declaration or let binding at beginnings of lines
//let private getPatch startCol (lines: string []) =
//    let rec loop i =
//        if i < 0 then
//            Nothing
//        elif Regex.Match(lines.[i], "^[\s]*type").Success then
//            RecType
//        else
//            // Need to compare column to ensure that the let binding is at the same level
//            let m = Regex.Match(lines.[i], "^[\s]*let")
//            let col = m.Index + m.Length
//            // Value 4 accounts for length of "and "
//            if m.Success && col <= startCol + 4 then
//                RecLet
//            else
//                loop (i - 1)
//
//    loop (lines.Length - 1)
//
///// Convert from range to string positions
//let private stringPos (r: Range) (sourceCode: string) =
//    // Assume that content has been normalized (no "\r\n" anymore)
//    let positions =
//        sourceCode.Split('\n')
//        // Skip '\r' as a new line character on Windows
//        |> Seq.map (fun s -> String.length s + 1)
//        |> Seq.scan (+) 0
//        |> Seq.toArray
//
//    let start = positions.[r.StartLine - 1] + r.StartColumn
//    // We can't assume the range is valid, so check string boundary here
//    let finish =
//        let pos = positions.[r.EndLine - 1] + r.EndColumn
//
//        if pos >= sourceCode.Length then
//            sourceCode.Length - 1
//        else
//            pos
//
//    (start, finish)
//
//let private formatRange
//    (checker: FSharpChecker)
//    (parsingOptions: FSharpParsingOptions)
//    returnFormattedContentOnly
//    (range: Range)
//    (lines: _ [])
//    config
//    ({ Source = sourceCode
//       FileName = fileName } as formatContext)
//    =
//    let startLine = range.StartLine
//    let startCol = range.StartColumn
//    let endLine = range.EndLine
//
//    let start, finish = stringPos range sourceCode
//
//    let pre =
//        if start = 0 then
//            String.Empty
//        else
//            sourceCode.[0 .. start - 1].TrimEnd('\r')
//
//    // Prepend selection by an appropriate amount of whitespace
//    let selection, patch =
//        let sel = sourceCode.[start..finish].TrimEnd('\r')
//
//        if startWithMember sel then
//            (String.Join(String.Empty, "type T = ", Environment.NewLine, System.String(' ', startCol), sel), TypeMember)
//        elif String.startsWithOrdinal "and" (sel.TrimStart()) then
//            let p = getPatch startCol lines.[.. startLine - 1]
//
//            let pattern = Regex("and")
//
//            let replacement =
//                match p with
//                | RecType -> "type"
//                | RecLet -> "let rec"
//                | _ -> "and"
//            // Replace "and" by "type" or "let rec"
//            if startLine = endLine then
//                (pattern.Replace(sel, replacement, 1), p)
//            else
//                (System.String(' ', startCol)
//                 + pattern.Replace(sel, replacement, 1),
//                 p)
//        elif startLine = endLine then
//            (sel, Nothing)
//        else
//            (System.String(' ', startCol) + sel, Nothing)
//
//    let post =
//        if finish < sourceCode.Length then
//            let post = sourceCode.[finish + 1 ..]
//
//            if String.startsWithOrdinal "\n" post then
//                Environment.NewLine + post.[1..]
//            else
//                post
//        else
//            String.Empty
//
//    Debug.WriteLine("pre:\n'{0}'", box pre)
//    Debug.WriteLine("selection:\n'{0}'", box selection)
//    Debug.WriteLine("post:\n'{0}'", box post)
//
//    let formatSelection sourceCode config =
//        async {
//            // From this point onwards, we focus on the current selection
//            let formatContext = { formatContext with Source = sourceCode }
//
//            let! formattedSourceCode = format checker parsingOptions config formatContext
//            // If the input is not inline, the output should not be inline as well
//            if
//                sourceCode.EndsWith("\n")
//                && not
//                   <| formattedSourceCode.EndsWith(Environment.NewLine)
//            then
//                return formattedSourceCode + Environment.NewLine
//            elif
//                not <| sourceCode.EndsWith("\n")
//                && formattedSourceCode.EndsWith(Environment.NewLine)
//            then
//                return formattedSourceCode.TrimEnd('\r', '\n')
//            else
//                return formattedSourceCode
//        }
//
//    let reconstructSourceCode startCol formatteds pre post =
//        Debug.WriteLine("Formatted parts: '{0}' at column {1}", sprintf "%A" formatteds, startCol)
//        // Realign results on the correct column
//        Context.Context.Create config [] fileName [] String.Empty None
//        // Mono version of indent text writer behaves differently from .NET one,
//        // So we add an empty string first to regularize it
//        |> if returnFormattedContentOnly then
//               Context.str String.Empty
//           else
//               Context.str pre
//        |> Context.atIndentLevel true startCol (Context.col Context.sepNln formatteds Context.str)
//        |> if returnFormattedContentOnly then
//               Context.str String.Empty
//           else
//               Context.str post
//        |> Context.dump
//
//    async {
//        match patch with
//        | TypeMember ->
//            // Get formatted selection with "type T = \n" patch
//            let! result = formatSelection selection config
//            // Remove the patch
//            let contents = String.normalizeThenSplitNewLine result
//
//            if Array.isEmpty contents then
//                if returnFormattedContentOnly then
//                    return result
//                else
//                    return String.Join(String.Empty, pre, result, post)
//            else
//                // Due to patching, the text has at least two lines
//                let first = contents.[1]
//                let column = first.Length - first.TrimStart().Length
//
//                let formatteds = contents.[1..] |> Seq.map (fun s -> s.[column..])
//
//                return reconstructSourceCode startCol formatteds pre post
//        | RecType
//        | RecLet ->
//            // Get formatted selection with "type" or "let rec" replacement for "and"
//            let! result = formatSelection selection config
//            // Substitute by old contents
//            let pattern =
//                if patch = RecType then
//                    Regex("type")
//                else
//                    Regex("let rec")
//
//            let formatteds =
//                String.normalizeThenSplitNewLine (pattern.Replace(result, "and", 1))
//
//            return reconstructSourceCode startCol formatteds pre post
//        | Nothing ->
//            let! result = formatSelection selection config
//            let formatteds = String.normalizeThenSplitNewLine result
//            return reconstructSourceCode startCol formatteds pre post
//    }
//
///// Format a part of source string using given config, and return the (formatted) selected part only.
///// Beware that the range argument is inclusive. If the range has a trailing newline, it will appear in the formatted result.
//let formatSelection
//    (checker: FSharpChecker)
//    (parsingOptions: FSharpParsingOptions)
//    (range: Range)
//    config
//    ({ Source = sourceCode
//       FileName = fileName } as formatContext)
//    =
//    let lines = String.normalizeThenSplitNewLine sourceCode
//
//    // Move to the section with real contents
//    let contentRange =
//        if range.StartLine = range.EndLine then
//            range
//        else
//            let startLine = getStartLineIndex lines (range.StartLine - 1) + 1
//
//            let endLine = getEndLineIndex lines (range.EndLine - 1) + 1
//
//            Debug.Assert(startLine >= range.StartLine, "Should shrink selections only.")
//            Debug.Assert(endLine <= range.EndLine, "Should shrink selections only.")
//
//            let startCol =
//                if startLine = range.StartLine then
//                    max range.StartColumn 0
//                else
//                    0
//
//            let endCol =
//                if endLine = range.EndLine then
//                    min range.EndColumn (lines.[endLine - 1].Length - 1)
//                else
//                    lines.[endLine - 1].Length - 1
//            // Notice that Line indices start at 1 while Column indices start at 0.
//            makeRange fileName startLine startCol endLine endCol
//
//    let startCol =
//        let line = lines.[contentRange.StartLine - 1].[contentRange.StartColumn ..]
//
//        contentRange.StartColumn + line.Length
//        - line.TrimStart().Length
//
//    let endCol =
//        let line = lines.[contentRange.EndLine - 1].[.. contentRange.EndColumn]
//
//        contentRange.EndColumn - line.Length
//        + line.TrimEnd().Length
//
//    let modifiedRange = makeRange fileName range.StartLine startCol range.EndLine endCol
//
//    Debug.WriteLine(
//        "Original range: {0} --> content range: {1} --> modified range: {2}",
//        sprintf "%O" range,
//        sprintf "%O" contentRange,
//        sprintf "%O" modifiedRange
//    )
//
//    async {
//        let! formatted = formatRange checker parsingOptions true modifiedRange lines config formatContext
//
//        let start, finish = stringPos range sourceCode
//        let newStart, newFinish = stringPos modifiedRange sourceCode
//
//        let pre = sourceCode.[start .. newStart - 1].TrimEnd('\r')
//
//        let post =
//            if newFinish + 1 >= sourceCode.Length
//               || newFinish >= finish then
//                String.Empty
//            else
//                sourceCode.[newFinish + 1 .. finish]
//                    .Replace("\r", "\n")
//
//        Debug.WriteLine(
//            "Original index: {0} --> modified index: {1}",
//            sprintf "%O" (start, finish),
//            sprintf "%O" (newStart, newFinish)
//        )
//
//        Debug.WriteLine("Join '{0}', '{1}' and '{2}'", pre, formatted, post)
//        return String.Join(String.Empty, pre, formatted, post)
//    }

type internal BlockType =
    | List
    | Array
    | SequenceOrRecord
    | Tuple

/// Make a position at (line, col) to denote cursor position
let makePos line col = mkPos line col
