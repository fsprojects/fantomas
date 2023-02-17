module internal Fantomas.Core.MultipleDefineCombinations

open System
open System.Linq
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices
open FSharp.Compiler.Text

/// A CodeFragment represents a chunk of code that is either
///     a single conditional hash directive line,
///     non existing content (for a specific combination of defines) or
///     active content.
///
/// When the code need to be compared, a CustomComparison is used to determine which fragment we are interested in.
[<RequireQualifiedAccess>]
[<CustomEquality; CustomComparison>]
type CodeFragment =
    /// Any line that starts with `#if`, `#else` or `#endif`
    | HashLine of line: string * defines: DefineCombination
    /// Content found between two HashLines
    | Content of code: string * lineCount: int * defines: DefineCombination
    /// When two HashLine follow each other without any content in between.
    | NoContent of defines: DefineCombination

    member x.Defines =
        match x with
        | HashLine(defines = defines)
        | Content(defines = defines)
        | NoContent(defines = defines) -> defines

    member x.LineCount =
        match x with
        | HashLine _ -> 1
        | Content(lineCount = lineCount) -> lineCount
        | NoContent _ -> 0

    override this.Equals _ = false
    override this.GetHashCode() = Int32.MinValue

    interface IComparable with
        member x.CompareTo other =
            match other with
            | :? CodeFragment as other -> (x :> IComparable<_>).CompareTo other
            | _ -> -1

    interface IComparable<CodeFragment> with
        member x.CompareTo y =
            match x, y with
            // When comparing the different results of each format result, the single constant is that all hash lines
            // should exactly match.
            | CodeFragment.HashLine(line = lineX), CodeFragment.HashLine(line = lineY) ->
                assert (lineX = lineY)
                0
            // Pick the other fragment is it has content you don't
            | CodeFragment.NoContent _, CodeFragment.Content _ -> -1
            // Pick our fragment is the other fragment has no code
            | CodeFragment.Content _, CodeFragment.NoContent _ -> 1
            // If both fragments are empty they are equivalent.
            // Keep in mind that we could be comparing more the two fragments at the same time in `traverseFragments`
            | CodeFragment.NoContent _, CodeFragment.NoContent _ -> 0
            // If both fragments have content, we want to take the content with the most lines.
            | CodeFragment.Content(lineCount = ownLineCount; code = ownContent),
              CodeFragment.Content(lineCount = otherLineCount; code = otherContent) ->
                if ownLineCount > otherLineCount then
                    1
                elif ownLineCount < otherLineCount then
                    -1
                elif ownContent = otherContent then
                    0
                else
                    ignore (ownContent, otherContent)
                    failwith "TODO: compare content, pick largest"
            // This is an unexpected situation.
            // You should never enter the case where you need to compare a hash line with something other than a hash line.
            | x, other ->
                // TODO: throw custom exception?
                failwith $"Cannot compare %A{x} with %A{other}"

type FormatResultForDefines =
    { Result: FormatResult
      Defines: DefineCombination
      Fragments: CodeFragment list }

/// Accumulator type used when building up the fragments.
type SplitHashState =
    { CurrentBuilder: StringBuilder
      LinesCollected: int
      LastLineInfo: LastLineInfo }

    static member Zero =
        { CurrentBuilder = StringBuilder()
          LastLineInfo = LastLineInfo.None
          LinesCollected = 0 }

and [<RequireQualifiedAccess>] LastLineInfo =
    | None
    | HashLine
    | Content

/// Accumulator type used when folding over the selected CodeFragments.
type FragmentWeaverState =
    { LastLine: int
      Cursors: Map<DefineCombination, pos>
      ContentBuilder: StringBuilder
      FoundCursor: (DefineCombination * pos) option }

let stringBuilderResult (builder: StringBuilder) = builder.ToString()

let hashRegex = @"^\s*#(if|elseif|else|endif).*"

/// Split the given `source` into the matching `CodeFragments`.
let splitWhenHash (defines: DefineCombination) (newline: string) (source: string) : CodeFragment list =
    let lines = source.Split([| newline |], options = StringSplitOptions.None)
    let mutable fragmentsBuilder = ListCollector<CodeFragment>()

    let closeState (acc: SplitHashState) =
        if acc.LastLineInfo = LastLineInfo.Content then
            let lastFragment = acc.CurrentBuilder.ToString()
            // The last fragment could be a newline after the the last #endif
            fragmentsBuilder.Add(CodeFragment.Content(lastFragment, acc.LinesCollected, defines))

    (SplitHashState.Zero, lines)
    ||> Array.fold (fun acc line ->
        if Regex.IsMatch(line, hashRegex) then
            // Only add the previous fragment if it had content
            if acc.LastLineInfo = LastLineInfo.HashLine then
                fragmentsBuilder.Add(CodeFragment.NoContent defines)
            else
                // Close the previous fragment builder
                let lastFragment = acc.CurrentBuilder.ToString()

                // Check if there is content, the first line of the code could be a hash directive
                if not (String.IsNullOrWhiteSpace(lastFragment)) then
                    fragmentsBuilder.Add(CodeFragment.Content(lastFragment, acc.LinesCollected, defines))

            // Add the hashLine
            fragmentsBuilder.Add(CodeFragment.HashLine(line.TrimStart(), defines))

            // Reset the state
            { CurrentBuilder = StringBuilder()
              LinesCollected = 0
              LastLineInfo = LastLineInfo.HashLine }
        else
            let nextBuilder =
                if acc.LastLineInfo = LastLineInfo.Content then
                    acc.CurrentBuilder.Append(newline)
                else
                    acc.CurrentBuilder

            let nextBuilder = nextBuilder.Append line

            { CurrentBuilder = nextBuilder
              LinesCollected = acc.LinesCollected + 1
              LastLineInfo = LastLineInfo.Content })
    |> closeState

    fragmentsBuilder.Close()

let mergeMultipleFormatResults config (results: (DefineCombination * FormatResult) list) : FormatResult =
    let allInFragments: FormatResultForDefines list =
        results
            .AsParallel()
            .Select(fun (dc, result) ->
                let fragments = splitWhenHash dc config.EndOfLine.NewLineString result.Code

                { Result = result
                  Defines = dc
                  Fragments = fragments })
        |> Seq.toList

    let allHaveSameFragmentCount =
        let allWithCount = List.map (fun { Fragments = f } -> f.Length) allInFragments
        (Set allWithCount).Count = 1

    if not allHaveSameFragmentCount then
        let chunkReport =
            allInFragments
            |> List.map (fun result ->
                sprintf "[%s] has %i fragments" (String.concat ", " result.Defines.Value) result.Fragments.Length)
            |> String.concat config.EndOfLine.NewLineString

        raise (
            FormatException(
                $"""Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back together.
{chunkReport}
Please raise an issue at https://fsprojects.github.io/fantomas-tools/#/fantomas/preview."""
            )
        )

    // Go over each fragment of all results.
    // Compare the fragments one by one and pick the one with most content.
    // See custom comparison for CodeFragment.
    let rec traverseFragments
        (input: CodeFragment list list)
        (continuation: CodeFragment list -> CodeFragment list)
        : CodeFragment list =
        let headItems = List.choose List.tryHead input

        if List.isEmpty headItems then
            continuation []
        else
            let max = List.max headItems
            traverseFragments (List.map List.tail input) (fun xs -> max :: xs |> continuation)

    let selectedFragments: CodeFragment list =
        traverseFragments (allInFragments |> List.map (fun r -> r.Fragments)) id

    let appendNewline (fragment: CodeFragment) (builder: StringBuilder) : StringBuilder =
        match fragment with
        | CodeFragment.NoContent _ -> builder
        | CodeFragment.HashLine _
        | CodeFragment.Content _ -> builder.Append config.EndOfLine.NewLineString

    let appendContent (fragment: CodeFragment) (builder: StringBuilder) : StringBuilder =
        match fragment with
        | CodeFragment.NoContent _ -> builder
        | CodeFragment.HashLine(line = content)
        | CodeFragment.Content(code = content) -> builder.Append content

    let areThereNotCursors =
        results |> List.forall (fun (_, result) -> Option.isNone result.Cursor)

    if areThereNotCursors then
        let code =
            (StringBuilder(), selectedFragments)
            ||> List.foldWithLast
                (fun acc fragment -> (appendContent fragment >> appendNewline fragment) acc)
                (fun acc fragment -> appendContent fragment acc)
            |> stringBuilderResult

        { Code = code; Cursor = None }
    else
        let weaver =
            { LastLine = 1
              FoundCursor = None
              ContentBuilder = StringBuilder()
              Cursors =
                results
                |> List.choose (fun (dc, formatResult) -> formatResult.Cursor |> Option.map (fun cursor -> dc, cursor))
                |> Map.ofList }

        let finalResult =
            let processFragment
                (postContent: CodeFragment -> StringBuilder -> StringBuilder)
                (acc: FragmentWeaverState)
                (fragment: CodeFragment)
                : FragmentWeaverState =
                let nextLastLine = acc.LastLine + fragment.LineCount

                // Try and find a cursor for the current set of defines that falls within the range of the current block.
                match Map.tryFind fragment.Defines acc.Cursors with
                | Some cursor when (acc.LastLine <= cursor.Line && cursor.Line <= nextLastLine) ->
                    { acc with
                        LastLine = acc.LastLine + fragment.LineCount
                        ContentBuilder = (appendContent fragment >> postContent fragment) acc.ContentBuilder
                        FoundCursor = Some(fragment.Defines, cursor) }
                | Some _
                | None ->
                    { acc with
                        LastLine = acc.LastLine + fragment.LineCount
                        ContentBuilder = (appendContent fragment >> postContent fragment) acc.ContentBuilder }

            (weaver, selectedFragments)
            ||> List.foldWithLast (processFragment appendNewline) (processFragment (fun _ sb -> sb))

        { Code = finalResult.ContentBuilder.ToString()
          Cursor = Option.map snd finalResult.FoundCursor }
