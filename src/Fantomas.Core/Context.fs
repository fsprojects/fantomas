module internal Fantomas.Core.Context

open System
open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak

type WriterEvent =
    | Write of string
    | WriteLine
    | WriteLineInsideStringConst
    | WriteBeforeNewline of string
    | WriteLineBecauseOfTrivia
    | WriteLineInsideTrivia
    | IndentBy of int
    | UnIndentBy of int
    | SetIndent of int
    | RestoreIndent of int
    | SetAtColumn of int
    | RestoreAtColumn of int

let (|CommentOrDefineEvent|_|) we =
    match we with
    | Write w when (String.startsWithOrdinal "//" w) -> Some we
    | Write w when (String.startsWithOrdinal "#if" w) -> Some we
    | Write w when (String.startsWithOrdinal "#else" w) -> Some we
    | Write w when (String.startsWithOrdinal "#endif" w) -> Some we
    | Write w when (String.startsWithOrdinal "(*" w) -> Some we
    | _ -> None

type ShortExpressionInfo =
    { MaxWidth: int
      StartColumn: int
      ConfirmedMultiline: bool }

    member x.IsTooLong maxPageWidth currentColumn =
        currentColumn - x.StartColumn > x.MaxWidth // expression is not too long according to MaxWidth
        || (currentColumn > maxPageWidth) // expression at current position is not going over the page width

type Size =
    | CharacterWidth of maxWidth: Num
    | NumberOfItems of items: Num * maxItems: Num

type WriteModelMode =
    | Standard
    | Dummy
    | ShortExpression of ShortExpressionInfo list

type WriterModel =
    {
        /// lines of resulting text, in reverse order (to allow more efficient adding line to end)
        Lines: string list
        /// current indentation
        Indent: int
        /// helper indentation information, if AtColumn > Indent after NewLine, Indent will be set to AtColumn
        AtColumn: int
        /// text to be written before next newline
        WriteBeforeNewline: string
        /// dummy = "fake" writer used in `autoNln`, `autoNlnByFuture`
        Mode: WriteModelMode
        /// current length of last line of output
        Column: int
    }

    member __.IsDummy =
        match __.Mode with
        | Dummy -> true
        | _ -> false

module WriterModel =
    let init =
        { Lines = [ "" ]
          Indent = 0
          AtColumn = 0
          WriteBeforeNewline = ""
          Mode = Standard
          Column = 0 }

    let update maxPageWidth cmd m =
        let doNewline m =
            let m =
                { m with
                    Indent = max m.Indent m.AtColumn }

            let nextLine = String.replicate m.Indent " "
            let currentLine = String.Concat(List.head m.Lines, m.WriteBeforeNewline).TrimEnd()
            let otherLines = List.tail m.Lines

            { m with
                Lines = nextLine :: currentLine :: otherLines
                WriteBeforeNewline = ""
                Column = m.Indent }

        let updateCmd cmd =
            match cmd with
            | WriteLine
            | WriteLineBecauseOfTrivia -> doNewline m
            | WriteLineInsideStringConst ->
                { m with
                    Lines = String.empty :: m.Lines
                    Column = 0 }
            | WriteLineInsideTrivia ->
                let lines =
                    match m.Lines with
                    | [] -> [ String.empty ]
                    | h :: tail -> String.empty :: h :: tail

                { m with Lines = lines; Column = 0 }
            | Write s ->
                { m with
                    Lines = (List.head m.Lines + s) :: (List.tail m.Lines)
                    Column = m.Column + (String.length s) }
            | WriteBeforeNewline s -> { m with WriteBeforeNewline = s }
            | IndentBy x ->
                { m with
                    Indent =
                        if m.AtColumn >= m.Indent + x then
                            m.AtColumn + x
                        else
                            m.Indent + x }
            | UnIndentBy x ->
                { m with
                    Indent = max m.AtColumn <| m.Indent - x }
            | SetAtColumn c -> { m with AtColumn = c }
            | RestoreAtColumn c -> { m with AtColumn = c }
            | SetIndent c -> { m with Indent = c }
            | RestoreIndent c -> { m with Indent = c }

        match m.Mode with
        | Dummy
        | Standard -> updateCmd cmd
        | ShortExpression infos when (List.exists (fun info -> info.ConfirmedMultiline) infos) -> m
        | ShortExpression infos ->
            let nextCmdCausesMultiline =
                match cmd with
                | WriteLine
                | WriteLineBecauseOfTrivia -> true
                | WriteLineInsideStringConst -> true
                | Write _ when (String.isNotNullOrEmpty m.WriteBeforeNewline) -> true
                | _ -> false

            let updatedInfos =
                infos
                |> List.map (fun info ->
                    let tooLong = info.IsTooLong maxPageWidth m.Column

                    { info with
                        ConfirmedMultiline = tooLong || nextCmdCausesMultiline })

            if List.exists (fun i -> i.ConfirmedMultiline) updatedInfos then
                { m with
                    Mode = ShortExpression(updatedInfos) }
            else
                updateCmd cmd

module WriterEvents =
    let normalize ev =
        match ev with
        | Write s when s.Contains("\n") ->
            let writeLine =
                match ev with
                | CommentOrDefineEvent _ -> WriteLineInsideTrivia
                | _ -> WriteLineInsideStringConst

            // Trustworthy multiline string in the original AST can contain \r
            // Internally we process everything with \n and at the end we respect the .editorconfig end_of_line setting.
            s.Replace("\r", "").Split('\n')
            |> Seq.map (fun x -> [ Write x ])
            |> Seq.reduce (fun x y -> x @ [ writeLine ] @ y)
            |> Seq.toList
        | _ -> [ ev ]

    let isMultiline evs =
        evs
        |> Queue.toSeq
        |> Seq.exists (function
            | WriteLine
            | WriteLineBecauseOfTrivia -> true
            | _ -> false)

[<System.Diagnostics.DebuggerDisplay("\"{Dump()}\"")>]
type Context =
    { Config: FormatConfig
      WriterModel: WriterModel
      WriterEvents: Queue<WriterEvent>
      FormattedCursor: pos option }

    /// Initialize with a string writer and use space as delimiter
    static member Default =
        { Config = FormatConfig.Default
          WriterModel = WriterModel.init
          WriterEvents = Queue.empty
          FormattedCursor = None }

    static member Create config : Context =
        { Context.Default with Config = config }

    member x.WithDummy(writerCommands, ?keepPageWidth) =
        let keepPageWidth = keepPageWidth |> Option.defaultValue false

        let mkModel m =
            { m with
                Mode = Dummy
                Lines = [ String.replicate x.WriterModel.Column " " ]
                WriteBeforeNewline = "" }
        // Use infinite column width to encounter worst-case scenario
        let config =
            { x.Config with
                MaxLineLength =
                    if keepPageWidth then
                        x.Config.MaxLineLength
                    else
                        Int32.MaxValue }

        { x with
            WriterModel = mkModel x.WriterModel
            WriterEvents = writerCommands
            Config = config }

    member x.WithShortExpression(maxWidth, ?startColumn) =
        let info =
            { MaxWidth = maxWidth
              StartColumn = Option.defaultValue x.WriterModel.Column startColumn
              ConfirmedMultiline = false }

        match x.WriterModel.Mode with
        | ShortExpression infos ->
            if List.exists (fun i -> i = info) infos then
                x
            else
                { x with
                    WriterModel =
                        { x.WriterModel with
                            Mode = ShortExpression(info :: infos) } }
        | _ ->
            { x with
                WriterModel =
                    { x.WriterModel with
                        Mode = ShortExpression([ info ]) } }

    member x.Column = x.WriterModel.Column

/// This adds a WriterEvent to the Context.
/// One event could potentially be split up into multiple events.
/// The event is also being processed in the WriterModel of the Context.
let writerEvent (e: WriterEvent) (ctx: Context) : Context =
    // One event could contain a multiline string or code comments.
    // These need to be split up in multiple events.
    let evs = WriterEvents.normalize e

    let ctx' =
        { ctx with
            WriterEvents = Queue.append ctx.WriterEvents evs
            WriterModel =
                (ctx.WriterModel, evs)
                ||> Seq.fold (fun m e -> WriterModel.update ctx.Config.MaxLineLength e m) }

    ctx'

let hasWriteBeforeNewlineContent ctx =
    String.isNotNullOrEmpty ctx.WriterModel.WriteBeforeNewline

let finalizeWriterModel (ctx: Context) =
    if hasWriteBeforeNewlineContent ctx then
        writerEvent (Write ctx.WriterModel.WriteBeforeNewline) ctx
    else
        ctx

let dump (isSelection: bool) (ctx: Context) =
    let ctx = finalizeWriterModel ctx

    let code =
        match ctx.WriterModel.Lines with
        | [] -> []
        | h :: tail ->
            // Always trim the last line
            h.TrimEnd() :: tail
        |> List.rev
        |> fun lines ->
            // Don't skip leading newlines when formatting a selection.
            if isSelection then lines else List.skipWhile ((=) "") lines
        |> String.concat ctx.Config.EndOfLine.NewLineString

    { Code = code
      Cursor = ctx.FormattedCursor }

let dumpAndContinue (ctx: Context) =
#if DEBUG
    let m = finalizeWriterModel ctx
    let lines = m.WriterModel.Lines |> List.rev

    let code = String.concat ctx.Config.EndOfLine.NewLineString lines

    printfn $"%s{code}"
#endif
    ctx

type Context with

    member x.FinalizeModel = finalizeWriterModel x

    member x.Dump() =
        let m = finalizeWriterModel x
        let lines = m.WriterModel.Lines |> List.rev

        String.concat x.Config.EndOfLine.NewLineString lines

let writeEventsOnLastLine ctx =
    ctx.WriterEvents
    |> Queue.rev
    |> Seq.takeWhile (function
        | WriteLine
        | WriteLineBecauseOfTrivia
        | WriteLineInsideStringConst -> false
        | _ -> true)
    |> Seq.choose (function
        | Write w when (String.length w > 0) -> Some w
        | _ -> None)

let lastWriteEventIsNewline ctx =
    ctx.WriterEvents
    |> Queue.rev
    |> Seq.skipWhile (function
        | RestoreIndent _
        | RestoreAtColumn _
        | UnIndentBy _
        | Write "" -> true
        | _ -> false)
    |> Seq.tryHead
    |> Option.map (function
        | WriteLineBecauseOfTrivia
        | WriteLine -> true
        | _ -> false)
    |> Option.defaultValue false

let (|EmptyHashDefineBlock|_|) (events: WriterEvent array) =
    match Array.tryHead events, Array.tryLast events with
    | Some(CommentOrDefineEvent _), Some(CommentOrDefineEvent _) ->
        // Check if there is an empty block between hash defines
        // Example:
        // #if FOO
        //
        // #endif
        let emptyLinesInBetween =
            Array.forall
                (function
                | WriteLineInsideStringConst
                | Write "" -> true
                | _ -> false)
                events.[1 .. (events.Length - 2)]

        if emptyLinesInBetween then Some events else None
    | _ -> None

/// Validate if there is a complete blank line between the last write event and the last event
let newlineBetweenLastWriteEvent ctx =
    ctx.WriterEvents
    |> Queue.rev
    |> Seq.takeWhile (function
        | Write ""
        | WriteLine
        | IndentBy _
        | UnIndentBy _
        | SetIndent _
        | RestoreIndent _
        | SetAtColumn _
        | RestoreAtColumn _ -> true
        | _ -> false)
    |> Seq.filter (function
        | WriteLine _ -> true
        | _ -> false)
    |> Seq.length
    |> fun writeLines -> writeLines > 1

let lastWriteEventOnLastLine ctx =
    writeEventsOnLastLine ctx |> Seq.tryHead

// A few utility functions from https://github.com/fsharp/powerpack/blob/master/src/FSharp.Compiler.CodeDom/generator.fs

/// Indent one more level based on configuration
let indent (ctx: Context) =
    // if atColumn is bigger then after indent, then we use atColumn as base for indent
    writerEvent (IndentBy ctx.Config.IndentSize) ctx

/// Unindent one more level based on configuration
let unindent (ctx: Context) =
    writerEvent (UnIndentBy ctx.Config.IndentSize) ctx

/// Apply function f at an absolute indent level (use with care)
let atIndentLevel alsoSetIndent level (f: Context -> Context) (ctx: Context) =
    if level < 0 then
        invalidArg "level" "The indent level cannot be negative."

    let m = ctx.WriterModel
    let oldIndent = m.Indent
    let oldColumn = m.AtColumn

    (writerEvent (SetAtColumn level)
     >> if alsoSetIndent then writerEvent (SetIndent level) else id
     >> f
     >> writerEvent (RestoreAtColumn oldColumn)
     >> writerEvent (RestoreIndent oldIndent))
        ctx

/// Set minimal indentation (`atColumn`) at current column position - next newline will be indented on `max indent atColumn`
/// Example:
/// { X = // indent=0, atColumn=2
///     "some long string" // indent=4, atColumn=2
///   Y = 1 // indent=0, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, but "some long string" have indent only 4, because it is bigger than `atColumn` (2).
let atCurrentColumn (f: _ -> Context) (ctx: Context) = atIndentLevel false ctx.Column f ctx

/// Write everything at current column indentation, set `indent` and `atColumn` on current column position
/// /// Example (same as above):
/// { X = // indent=2, atColumn=2
///       "some long string" // indent=6, atColumn=2
///   Y = 1 // indent=2, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, "some long string" have indent 6, because it is indented from `atCurrentColumn` pos (2).
let atCurrentColumnIndent (f: _ -> Context) (ctx: Context) = atIndentLevel true ctx.Column f ctx

/// Function composition operator
let (+>) (ctx: Context -> Context) (f: _ -> Context) x =
    let y = ctx x

    match y.WriterModel.Mode with
    | ShortExpression infos when infos |> Seq.exists (fun x -> x.ConfirmedMultiline) -> y
    | _ -> f y

let (!-) (str: string) = writerEvent (Write str)

/// Similar to col, and supply index as well
let coli f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()

    while e.MoveNext() do
        if tryPick then tryPick <- false else st <- f' st

        st <- f i e.Current st
        i <- i + 1

    st

/// Process collection - keeps context through the whole processing
/// calls f for every element in sequence and f' between every two elements
/// as a separator. This is a variant that works on typed collections.
let col f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()

    while e.MoveNext() do
        if tryPick then tryPick <- false else st <- f' st
        st <- f e.Current st

    st

// Similar to col but pass the item of 'T to f' as well
let colEx f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()

    while e.MoveNext() do
        if tryPick then tryPick <- false else st <- f' e.Current st
        st <- f e.Current st

    st

/// Similar to col, apply one more function f2 at the end if the input sequence is not empty
let colPost f2 f1 (c: seq<'T>) f (ctx: Context) =
    if Seq.isEmpty c then ctx else f2 (col f1 c f ctx)

/// Similar to col, apply one more function f2 at the beginning if the input sequence is not empty
let colPre f2 f1 (c: seq<'T>) f (ctx: Context) =
    if Seq.isEmpty c then ctx else col f1 c f (f2 ctx)

/// If there is a value, apply f and f' accordingly, otherwise do nothing
let opt (f': Context -> _) o f (ctx: Context) =
    match o with
    | Some x -> f' (f x ctx)
    | None -> ctx

/// similar to opt, only takes a single function f to apply when there is a value
let optSingle f o ctx =
    match o with
    | Some x -> f x ctx
    | None -> ctx

/// Similar to opt, but apply f2 at the beginning if there is a value
let optPre (f2: _ -> Context) (f1: Context -> _) o f (ctx: Context) =
    match o with
    | Some x -> f1 (f x (f2 ctx))
    | None -> ctx

let getListOrArrayExprSize ctx maxWidth xs =
    match ctx.Config.ArrayOrListMultilineFormatter with
    | MultilineFormatterType.CharacterWidth -> Size.CharacterWidth maxWidth
    | MultilineFormatterType.NumberOfItems -> Size.NumberOfItems(List.length xs, ctx.Config.MaxArrayOrListNumberOfItems)

let getRecordSize ctx fields =
    match ctx.Config.RecordMultilineFormatter with
    | MultilineFormatterType.CharacterWidth -> Size.CharacterWidth ctx.Config.MaxRecordWidth
    | MultilineFormatterType.NumberOfItems -> Size.NumberOfItems(List.length fields, ctx.Config.MaxRecordNumberOfItems)

/// b is true, apply f1 otherwise apply f2
let ifElse b (f1: Context -> Context) f2 (ctx: Context) = if b then f1 ctx else f2 ctx

let ifElseCtx cond (f1: Context -> Context) f2 (ctx: Context) = if cond ctx then f1 ctx else f2 ctx

/// apply f only when cond is true
let onlyIf cond f ctx = if cond then f ctx else ctx

let onlyIfCtx cond f ctx = if cond ctx then f ctx else ctx

let onlyIfNot cond f ctx = if cond then ctx else f ctx

let whenShortIndent f ctx =
    onlyIf (ctx.Config.IndentSize < 3) f ctx

/// Repeat application of a function n times
let rep n (f: Context -> Context) (ctx: Context) =
    [ 1..n ] |> List.fold (fun c _ -> f c) ctx

// Separator functions
let sepNone = id
let sepDot = !- "."

let sepSpace (ctx: Context) =
    if ctx.WriterModel.IsDummy then
        (!- " ") ctx
    else
        match lastWriteEventOnLastLine ctx with
        | Some w when (w.EndsWith(" ") || w.EndsWith Environment.NewLine) -> ctx
        | None -> ctx
        | _ -> (!- " ") ctx

// add actual spaces until the target column is reached, regardless of previous content
// use with care
let addFixedSpaces (targetColumn: int) (ctx: Context) : Context =
    let delta = targetColumn - ctx.Column
    onlyIf (delta > 0) (rep delta (!- " ")) ctx

let sepNln = writerEvent WriteLine

// Use a different WriteLine event to indicate that the newline was introduces due to trivia
// This is later useful when checking if an expression was multiline when checking for ColMultilineItem
let sepNlnForTrivia = writerEvent WriteLineBecauseOfTrivia

let sepNlnUnlessLastEventIsNewline (ctx: Context) =
    if lastWriteEventIsNewline ctx then ctx else sepNln ctx

let sepStar = sepSpace +> !- "* "
let sepEq = !- " ="
let sepEqFixed = !- "="
let sepArrow = !- " -> "
let sepArrowRev = !- " <- "
let sepBar = !- "| "

let addSpaceIfSpaceAroundDelimiter (ctx: Context) =
    onlyIf ctx.Config.SpaceAroundDelimiter sepSpace ctx

let addSpaceIfSpaceAfterComma (ctx: Context) =
    onlyIf ctx.Config.SpaceAfterComma sepSpace ctx

/// opening token of list
let sepOpenLFixed = !- "["

/// closing token of list
let sepCloseLFixed = !- "]"

/// opening token of anon record
let sepOpenAnonRecdFixed = !- "{|"
/// opening token of tuple
let sepOpenT = !- "("

/// closing token of tuple
let sepCloseT = !- ")"

let wordAnd = sepSpace +> !- "and "
let wordAndFixed = !- "and"
let wordOf = sepSpace +> !- "of "

let indentSepNlnUnindent f = indent +> sepNln +> f +> unindent

let shortExpressionWithFallback
    (shortExpression: Context -> Context)
    fallbackExpression
    maxWidth
    startColumn
    (ctx: Context)
    =
    // if the context is already inside a ShortExpression mode and tries to figure out if the expression will go over the page width,
    // we should try the shortExpression in this case.
    match ctx.WriterModel.Mode with
    | ShortExpression infos when
        (List.exists (fun info -> info.ConfirmedMultiline || info.IsTooLong ctx.Config.MaxLineLength ctx.Column) infos)
        ->
        ctx
    | _ ->
        // create special context that will process the writer events slightly different
        let shortExpressionContext =
            match startColumn with
            | Some sc -> ctx.WithShortExpression(maxWidth, sc)
            | None -> ctx.WithShortExpression(maxWidth)

        let resultContext = shortExpression shortExpressionContext

        match resultContext.WriterModel.Mode with
        | ShortExpression infos ->
            // verify the expression is not longer than allowed
            if
                List.exists
                    (fun info ->
                        info.ConfirmedMultiline
                        || info.IsTooLong ctx.Config.MaxLineLength resultContext.Column)
                    infos
            then
                fallbackExpression ctx
            else
                { resultContext with
                    WriterModel =
                        { resultContext.WriterModel with
                            Mode = ctx.WriterModel.Mode } }
        | _ ->
            // you should never hit this branch
            fallbackExpression ctx

let isShortExpression maxWidth (shortExpression: Context -> Context) fallbackExpression (ctx: Context) =
    shortExpressionWithFallback shortExpression fallbackExpression maxWidth None ctx

let expressionFitsOnRestOfLine expression fallbackExpression (ctx: Context) =
    shortExpressionWithFallback expression fallbackExpression ctx.Config.MaxLineLength (Some 0) ctx

let isSmallExpression size (smallExpression: Context -> Context) fallbackExpression (ctx: Context) =
    match size with
    | CharacterWidth maxWidth -> isShortExpression maxWidth smallExpression fallbackExpression ctx
    | NumberOfItems(items, maxItems) ->
        if items > maxItems then
            fallbackExpression ctx
        else
            expressionFitsOnRestOfLine smallExpression fallbackExpression ctx

/// provide the line and column before and after the leadingExpression to to the continuation expression
let leadingExpressionResult leadingExpression continuationExpression (ctx: Context) =
    let lineCountBefore, columnBefore =
        List.length ctx.WriterModel.Lines, ctx.WriterModel.Column

    let contextAfterLeading = leadingExpression ctx

    let lineCountAfter, columnAfter =
        List.length contextAfterLeading.WriterModel.Lines, contextAfterLeading.WriterModel.Column

    continuationExpression ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) contextAfterLeading

/// A leading expression is not consider multiline if it has a comment before it.
/// For example
/// let a = 7
/// // foo
/// let b = 8
/// let c = 9
/// The second binding b is not consider multiline.
let leadingExpressionIsMultiline leadingExpression continuationExpression (ctx: Context) =
    let eventCountBeforeExpression = Queue.length ctx.WriterEvents

    let contextAfterLeading = leadingExpression ctx

    let hasWriteLineEventsAfterExpression =
        contextAfterLeading.WriterEvents
        |> Queue.skipExists
            eventCountBeforeExpression
            (function
            | WriteLine _ -> true
            | _ -> false)
            (fun e ->
                match e with
                | [| CommentOrDefineEvent _ |]
                | [| WriteLine |]
                | [| Write "" |]
                | EmptyHashDefineBlock _ -> true
                | _ -> false)

    continuationExpression hasWriteLineEventsAfterExpression contextAfterLeading

let expressionExceedsPageWidth beforeShort afterShort beforeLong afterLong expr (ctx: Context) =
    // if the context is already inside a ShortExpression mode, we should try the shortExpression in this case.
    match ctx.WriterModel.Mode with
    | ShortExpression infos when
        (List.exists (fun info -> info.ConfirmedMultiline || info.IsTooLong ctx.Config.MaxLineLength ctx.Column) infos)
        ->
        ctx
    | ShortExpression _ ->
        // if the context is already inside a ShortExpression mode, we should try the shortExpression in this case.
        (beforeShort +> expr +> afterShort) ctx
    | _ ->
        let shortExpressionContext = ctx.WithShortExpression(ctx.Config.MaxLineLength, 0)

        let resultContext = (beforeShort +> expr +> afterShort) shortExpressionContext

        let fallbackExpression = beforeLong +> expr +> afterLong

        match resultContext.WriterModel.Mode with
        | ShortExpression infos ->
            // verify the expression is not longer than allowed
            if
                List.exists
                    (fun info ->
                        info.ConfirmedMultiline
                        || info.IsTooLong ctx.Config.MaxLineLength resultContext.Column)
                    infos
            then
                fallbackExpression ctx
            else
                { resultContext with
                    WriterModel =
                        { resultContext.WriterModel with
                            Mode = ctx.WriterModel.Mode } }
        | _ ->
            // you should never hit this branch
            fallbackExpression ctx

/// try and write the expression on the remainder of the current line
/// add an indent and newline if the expression is longer
let autoIndentAndNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepNone
        sepNone // before and after for short expressions
        (indent +> sepNln)
        unindent // before and after for long expressions
        expr
        ctx

let sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepSpace
        sepNone // before and after for short expressions
        (indent +> sepNln)
        unindent // before and after for long expressions
        expr
        ctx

let sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepSpace
        sepNone // before and after for short expressions
        (indent +> indent +> sepNln)
        (unindent +> unindent) // before and after for long expressions
        expr
        ctx

let sepSpaceOrIndentAndNlnIfExceedsPageWidthUnlessStroustrup isStroustrup f (node: Node) (ctx: Context) =
    if
        ctx.Config.ExperimentalStroustrupStyle
        && isStroustrup
        && Seq.isEmpty node.ContentBefore
    then
        (sepSpace +> f) ctx
    else
        sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth f ctx

let sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup f (expr: Expr) =
    sepSpaceOrIndentAndNlnIfExceedsPageWidthUnlessStroustrup expr.IsStroustrupStyleExpr (f expr) (Expr.Node expr)

let sepSpaceOrIndentAndNlnIfTypeExceedsPageWidthUnlessStroustrup f (t: Type) =
    sepSpaceOrIndentAndNlnIfExceedsPageWidthUnlessStroustrup t.IsStroustrupStyleType (f t) (Type.Node t)

let autoNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepNone
        sepNone // before and after for short expressions
        sepNln
        sepNone // before and after for long expressions
        expr
        ctx

let autoParenthesisIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionFitsOnRestOfLine expr (sepOpenT +> expr +> sepCloseT) ctx

let futureNlnCheckMem (f, ctx) =
    if ctx.WriterModel.IsDummy then
        (false, false)
    else
        // Create a dummy context to evaluate length of current operation
        let dummyCtx: Context = ctx.WithDummy(Queue.empty, keepPageWidth = true) |> f
        WriterEvents.isMultiline dummyCtx.WriterEvents, dummyCtx.Column > ctx.Config.MaxLineLength

let futureNlnCheck f (ctx: Context) =
    let isMultiLine, isLong = futureNlnCheckMem (f, ctx)
    isMultiLine || isLong

/// similar to futureNlnCheck but validates whether the expression is going over the max page width
/// This functions is does not use any caching
let exceedsWidth maxWidth f (ctx: Context) =
    let dummyCtx: Context = ctx.WithDummy(Queue.empty, keepPageWidth = true)

    let currentLines = dummyCtx.WriterModel.Lines.Length
    let currentColumn = dummyCtx.Column
    let ctxAfter: Context = f dummyCtx
    let linesAfter = ctxAfter.WriterModel.Lines.Length
    let columnAfter = ctxAfter.Column

    linesAfter > currentLines
    || (columnAfter - currentColumn) > maxWidth
    || currentColumn > ctx.Config.MaxLineLength

/// Similar to col, skip auto newline for index 0
let colAutoNlnSkip0i f' (c: seq<'T>) f (ctx: Context) =
    coli
        f'
        c
        (fun i c ->
            if i = 0 then
                f i c
            else
                autoNlnIfExpressionExceedsPageWidth (f i c))
        ctx

/// Similar to col, skip auto newline for index 0
let colAutoNlnSkip0 f' c f = colAutoNlnSkip0i f' c (fun _ -> f)

let sepSpaceBeforeClassConstructor ctx =
    if ctx.Config.SpaceBeforeClassConstructor then
        sepSpace ctx
    else
        ctx

let sepColon (ctx: Context) =
    let defaultExpr = if ctx.Config.SpaceBeforeColon then !- " : " else !- ": "

    if ctx.WriterModel.IsDummy then
        defaultExpr ctx
    else
        match lastWriteEventOnLastLine ctx with
        | Some w when w.EndsWith(" ") -> !- ": " ctx
        | None -> !- ": " ctx
        | _ -> defaultExpr ctx

let sepColonFixed = !- ":"

let sepColonWithSpacesFixed = !- " : "

let sepComma (ctx: Context) =
    if ctx.Config.SpaceAfterComma then
        !- ", " ctx
    else
        !- "," ctx

let sepSemi (ctx: Context) =
    let { Config = { SpaceBeforeSemicolon = before
                     SpaceAfterSemicolon = after } } =
        ctx

    match before, after with
    | false, false -> !- ";"
    | true, false -> !- " ;"
    | false, true -> !- "; "
    | true, true -> !- " ; "
    <| ctx

let ifAlignOrStroustrupBrackets f g =
    ifElseCtx
        (fun ctx ->
            match ctx.Config.MultilineBracketStyle with
            | Aligned
            | ExperimentalStroustrup -> true
            | Cramped -> false)
        f
        g

let sepNlnWhenWriteBeforeNewlineNotEmptyOr fallback (ctx: Context) =
    if hasWriteBeforeNewlineContent ctx then
        sepNln ctx
    else
        fallback ctx

let sepNlnWhenWriteBeforeNewlineNotEmpty =
    sepNlnWhenWriteBeforeNewlineNotEmptyOr sepNone

let sepSpaceUnlessWriteBeforeNewlineNotEmpty (ctx: Context) =
    if hasWriteBeforeNewlineContent ctx then
        ctx
    else
        sepSpace ctx

let autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (f: Context -> Context) (ctx: Context) =
    if hasWriteBeforeNewlineContent ctx then
        indentSepNlnUnindent f ctx
    else
        f ctx

let addParenIfAutoNln expr f =
    let hasParenthesis =
        match expr with
        | Expr.Paren _
        | Expr.ParenLambda _
        | Expr.ParenILEmbedded _
        | Expr.ParenFunctionNameWithStar _
        | Expr.Constant(Constant.Unit _) -> true
        | _ -> false

    let expr = f expr
    expressionFitsOnRestOfLine expr (ifElse hasParenthesis (sepOpenT +> expr +> sepCloseT) expr)

let autoIndentAndNlnExpressUnlessStroustrup (f: Expr -> Context -> Context) (e: Expr) (ctx: Context) =
    let shouldUseStroustrup =
        ctx.Config.ExperimentalStroustrupStyle
        && e.IsStroustrupStyleExpr
        && let node = Expr.Node e in
           Seq.isEmpty node.ContentBefore

    if shouldUseStroustrup then
        f e ctx
    else
        indentSepNlnUnindent (f e) ctx

let autoIndentAndNlnTypeUnlessStroustrup (f: Type -> Context -> Context) (t: Type) (ctx: Context) =
    let shouldUseStroustrup =
        ctx.Config.ExperimentalStroustrupStyle
        && t.IsStroustrupStyleType
        && let node = Type.Node t in
           Seq.isEmpty node.ContentBefore

    if shouldUseStroustrup then
        f t ctx
    else
        autoIndentAndNlnIfExpressionExceedsPageWidth (f t) ctx

let autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup
    (f: Expr -> Context -> Context)
    (e: Expr)
    (ctx: Context)
    =
    let isStroustrup =
        ctx.Config.ExperimentalStroustrupStyle
        && e.IsStroustrupStyleExpr
        && Seq.isEmpty (Expr.Node e).ContentBefore

    if isStroustrup then
        f e ctx
    else
        autoIndentAndNlnIfExpressionExceedsPageWidth (f e) ctx

type ColMultilineItem =
    | ColMultilineItem of
        // current expression
        expr: (Context -> Context) *
        // sepNln of current item
        sepNln: (Context -> Context)

type ColMultilineItemsState =
    { LastBlockMultiline: bool
      Context: Context }

/// Checks if the events of an expression produces multiple lines of by user code.
/// Leading or trailing trivia will not be counted as such.
let isMultilineItem (expr: Context -> Context) (ctx: Context) : bool * Context =
    let previousEventsLength = ctx.WriterEvents.Length
    let nextCtx = expr ctx

    let isExpressionMultiline =
        Queue.skipExists
            previousEventsLength
            (function
            | WriteLine
            | WriteLineInsideStringConst -> true
            | _ -> false)
            (fun events ->
                if events.Length > 0 then
                    // filter leading newlines and trivia
                    match Array.head events with
                    | CommentOrDefineEvent _
                    | WriteLine
                    | WriteLineBecauseOfTrivia -> true
                    | _ -> false
                else
                    false)
            nextCtx.WriterEvents

    isExpressionMultiline, nextCtx

/// This helper function takes a list of expressions and ranges.
/// If the expression is multiline it will add a newline before and after the expression.
/// Unless it is the first expression in the list, that will never have a leading new line.
/// F.ex.
/// let a = AAAA
/// let b =
///     BBBB
///     BBBB
/// let c = CCCC
///
/// will be formatted as:
/// let a = AAAA
///
/// let b =
///     BBBB
///     BBBBB
///
/// let c = CCCC

let colWithNlnWhenItemIsMultiline (items: ColMultilineItem list) (ctx: Context) : Context =
    match items with
    | [] -> ctx
    | [ (ColMultilineItem(expr, _)) ] -> expr ctx
    | ColMultilineItem(initialExpr, _) :: items ->
        let result =
            // The first item can be written as is.
            let initialIsMultiline, initialCtx = isMultilineItem initialExpr ctx

            let itemsState =
                { Context = initialCtx
                  LastBlockMultiline = initialIsMultiline }

            let rec loop (acc: ColMultilineItemsState) (items: ColMultilineItem list) =
                match items with
                | [] -> acc.Context
                | ColMultilineItem(expr, sepNlnItem) :: rest ->
                    // Assume the current item will be multiline or the previous was.
                    // If this is the case, we have already processed the correct stream of event (with additional newline)
                    // It is cheaper to replay the current expression if it (and its predecessor) turned out to be single lines.
                    let ctxAfterNln =
                        (ifElseCtx
                            newlineBetweenLastWriteEvent
                            sepNone // don't add extra newline if there already is a full blank line at the end of the stream.
                            sepNln
                         +> sepNlnItem)
                            acc.Context

                    let isMultiline, nextCtx = isMultilineItem expr ctxAfterNln

                    let nextCtx =
                        if not isMultiline && not acc.LastBlockMultiline then
                            // both the previous and current items are single line expressions
                            // replay the current item as a fallback
                            (sepNlnItem +> expr) acc.Context
                        else
                            nextCtx

                    loop
                        { acc with
                            Context = nextCtx
                            LastBlockMultiline = isMultiline }
                        rest

            loop itemsState items

        result

let colWithNlnWhenItemIsMultilineUsingConfig (items: ColMultilineItem list) (ctx: Context) =
    if ctx.Config.BlankLinesAroundNestedMultilineExpressions then
        colWithNlnWhenItemIsMultiline items ctx
    else
        col sepNln items (fun (ColMultilineItem(expr, _)) -> expr) ctx
