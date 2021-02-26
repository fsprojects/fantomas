module Fantomas.Context

open System
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text.Pos
open FSharp.Compiler.SyntaxTree
open Fantomas
open Fantomas.FormatConfig
open Fantomas.TriviaTypes

type WriterEvent =
    | Write of string
    | WriteLine
    | WriteLineInsideStringConst
    | WriteBeforeNewline of string
    | IndentBy of int
    | UnIndentBy of int
    | SetIndent of int
    | RestoreIndent of int
    | SetAtColumn of int
    | RestoreAtColumn of int

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
      Column: int }
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

            { m with
                  Lines =
                      String.replicate m.Indent " "
                      :: (List.head m.Lines + m.WriteBeforeNewline)
                         :: (List.tail m.Lines)
                  WriteBeforeNewline = ""
                  Column = m.Indent }

        let updateCmd cmd =
            match cmd with
            | WriteLine -> doNewline m
            | WriteLineInsideStringConst ->
                { m with
                      Lines = "" :: m.Lines
                      Column = 0 }
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
                | WriteLine -> true
                | WriteLineInsideStringConst -> true
                | Write _ when (String.isNotNullOrEmpty m.WriteBeforeNewline) -> true
                | _ -> false

            let updatedInfos =
                infos
                |> List.map
                    (fun info ->
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
        | Write s when String.normalizeThenSplitNewLine s |> Array.length > 1 ->
            String.normalizeThenSplitNewLine s
            |> Seq.map (fun x -> [ Write x ])
            |> Seq.reduce (fun x y -> x @ [ WriteLineInsideStringConst ] @ y)
            |> Seq.toList
        | _ -> [ ev ]

    let isMultiline evs =
        evs
        |> Queue.toSeq
        |> Seq.exists
            (function
            | WriteLine -> true
            | _ -> false)

type internal Context =
    { Config: FormatConfig
      WriterModel: WriterModel
      WriterEvents: Queue<WriterEvent>
      BreakLines: bool
      BreakOn: string -> bool
      /// The original source string to query as a last resort
      Content: string
      /// Positions of new lines in the original source string
      Positions: int []
      TriviaMainNodes: Map<FsAstType, TriviaNode list>
      TriviaTokenNodes: Map<FsTokenType, TriviaNode list>
      RecordBraceStart: int list
      FileName: string }

    /// Initialize with a string writer and use space as delimiter
    static member Default =
        { Config = FormatConfig.Default
          WriterModel = WriterModel.init
          WriterEvents = Queue.empty
          BreakLines = true
          BreakOn = (fun _ -> false)
          Content = ""
          Positions = [||]
          TriviaMainNodes = Map.empty
          TriviaTokenNodes = Map.empty
          RecordBraceStart = []
          FileName = String.Empty }

    static member Create
        config
        defines
        (fileName: string)
        (hashTokens: Token list)
        (content: string)
        (maybeAst: ParsedInput option)
        =
        let content = String.normalizeNewLine content

        let positions =
            content.Split('\n')
            |> Seq.map (fun s -> String.length s + 1)
            |> Seq.scan (+) 0
            |> Seq.toArray

        let tokens =
            TokenParser.tokenize defines hashTokens content

        let trivia =
            match maybeAst, config.StrictMode with
            | Some ast, false ->
                let mkRange (startLine, startCol) (endLine, endCol) =
                    mkRange fileName (mkPos startLine startCol) (mkPos endLine endCol)

                Trivia.collectTrivia mkRange tokens ast
            | _ -> []

        let triviaByNodes =
            trivia
            |> List.choose
                (fun tn ->
                    match tn.Type with
                    | MainNode (mn) -> Some(mn, tn)
                    | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (k, g) -> k, List.map snd g)
            |> Map.ofList

        let triviaByTokenNames =
            trivia
            |> List.choose
                (fun tn ->
                    match tn.Type with
                    | Token (tname, _) -> Some(tname, tn)
                    | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (k, g) -> k, List.map snd g)
            |> Map.ofList

        { Context.Default with
              Config = config
              Content = content
              Positions = positions
              TriviaMainNodes = triviaByNodes
              TriviaTokenNodes = triviaByTokenNames
              FileName = fileName }

    member x.WithDummy(writerCommands, ?keepPageWidth) =
        let keepPageWidth =
            keepPageWidth |> Option.defaultValue false

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

    member x.MkRange (startPos: pos) (endPos: pos) = mkRange x.FileName startPos endPos

    member x.MkRangeWith (startLine, startColumn) (endLine, endColumn) =
        x.MkRange(mkPos startLine startColumn) (mkPos endLine endColumn)

let internal writerEvent e ctx =
    let evs = WriterEvents.normalize e

    let ctx' =
        { ctx with
              WriterEvents = Queue.append ctx.WriterEvents evs
              WriterModel =
                  (ctx.WriterModel, evs)
                  ||> Seq.fold (fun m e -> WriterModel.update ctx.Config.MaxLineLength e m) }

    ctx'

let internal finalizeWriterModel (ctx: Context) =
    if ctx.WriterModel.WriteBeforeNewline <> "" then
        writerEvent (Write ctx.WriterModel.WriteBeforeNewline) ctx
    else
        ctx

let internal dump (ctx: Context) =
    let ctx = finalizeWriterModel ctx

    ctx.WriterModel.Lines
    |> List.rev
    |> List.skipWhile ((=) "")
    |> List.map (fun line -> line.TrimEnd())
    |> String.concat ctx.Config.EndOfLine.NewLineString

let internal dumpAndContinue (ctx: Context) =
#if DEBUG
    let m = finalizeWriterModel ctx
    let lines = m.WriterModel.Lines |> List.rev

    let code =
        String.concat ctx.Config.EndOfLine.NewLineString lines

    printfn "%s" code
#endif
    ctx

type Context with
    member x.Column = x.WriterModel.Column
    member x.FinalizeModel = finalizeWriterModel x

let internal writeEventsOnLastLine ctx =
    ctx.WriterEvents
    |> Queue.rev
    |> Seq.takeWhile
        (function
        | WriteLine
        | WriteLineInsideStringConst -> false
        | _ -> true)
    |> Seq.choose
        (function
        | Write w when (String.length w > 0) -> Some w
        | _ -> None)

let internal lastWriteEventIsNewline ctx =
    ctx.WriterEvents
    |> Queue.rev
    |> Seq.skipWhile
        (function
        | RestoreIndent _
        | RestoreAtColumn _
        | Write "" -> true
        | _ -> false)
    |> Seq.tryHead
    |> Option.map
        (function
        | WriteLine -> true
        | _ -> false)
    |> Option.defaultValue false

let private (|CommentOrDefineEvent|_|) we =
    match we with
    | Write (w) when (String.startsWithOrdinal "//" w) -> Some we
    | Write (w) when (String.startsWithOrdinal "#if" w) -> Some we
    | Write (w) when (String.startsWithOrdinal "#else" w) -> Some we
    | Write (w) when (String.startsWithOrdinal "#endif" w) -> Some we
    | Write (w) when (String.startsWithOrdinal "(*" w) -> Some we
    | _ -> None

/// Validate if there is a complete blank line between the last write event and the last event
let internal newlineBetweenLastWriteEvent ctx =
    ctx.WriterEvents
    |> Queue.rev
    |> Seq.takeWhile
        (function
        | Write ""
        | WriteLine
        | IndentBy _
        | UnIndentBy _
        | SetIndent _
        | RestoreIndent _
        | SetAtColumn _
        | RestoreAtColumn _ -> true
        | _ -> false)
    |> Seq.filter
        (function
        | WriteLine _ -> true
        | _ -> false)
    |> Seq.length
    |> fun writeLines -> writeLines > 1

let internal lastWriteEventOnLastLine ctx =
    writeEventsOnLastLine ctx |> Seq.tryHead

let internal forallCharsOnLastLine f ctx =
    writeEventsOnLastLine ctx
    |> Seq.collect id
    |> Seq.forall f

// A few utility functions from https://github.com/fsharp/powerpack/blob/master/src/FSharp.Compiler.CodeDom/generator.fs

/// Indent one more level based on configuration
let internal indent (ctx: Context) =
    // if atColumn is bigger then after indent, then we use atColumn as base for indent
    writerEvent (IndentBy ctx.Config.IndentSize) ctx

/// Unindent one more level based on configuration
let internal unindent (ctx: Context) =
    writerEvent (UnIndentBy ctx.Config.IndentSize) ctx

/// Increase indent by i spaces
let internal incrIndent i (ctx: Context) = writerEvent (IndentBy i) ctx

/// Decrease indent by i spaces
let internal decrIndent i (ctx: Context) = writerEvent (UnIndentBy i) ctx

/// Apply function f at an absolute indent level (use with care)
let internal atIndentLevel alsoSetIndent level (f: Context -> Context) (ctx: Context) =
    if level < 0 then
        invalidArg "level" "The indent level cannot be negative."

    let m = ctx.WriterModel
    let oldIndent = m.Indent
    let oldColumn = m.AtColumn

    (writerEvent (SetAtColumn level)
     >> if alsoSetIndent then
            writerEvent (SetIndent level)
        else
            id
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
let internal atCurrentColumn (f: _ -> Context) (ctx: Context) = atIndentLevel false ctx.Column f ctx

/// Like atCurrentColumn, but use current column after applying prependF
let internal atCurrentColumnWithPrepend (prependF: _ -> Context) (f: _ -> Context) (ctx: Context) =
    let col = ctx.Column
    (prependF >> atIndentLevel false col f) ctx

/// Write everything at current column indentation, set `indent` and `atColumn` on current column position
/// /// Example (same as above):
/// { X = // indent=2, atColumn=2
///       "some long string" // indent=6, atColumn=2
///   Y = 1 // indent=2, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, "some long string" have indent 6, because it is indented from `atCurrentColumn` pos (2).
let internal atCurrentColumnIndent (f: _ -> Context) (ctx: Context) = atIndentLevel true ctx.Column f ctx

/// Function composition operator
let internal (+>) (ctx: Context -> Context) (f: _ -> Context) x =
    let y = ctx x

    match y.WriterModel.Mode with
    | ShortExpression infos when
        infos
        |> Seq.exists (fun x -> x.ConfirmedMultiline) -> y
    | _ -> f y

/// Break-line and append specified string
let internal (++) (ctx: Context -> Context) (str: string) x =
    ctx x
    |> writerEvent WriteLine
    |> writerEvent (Write str)

/// Break-line if config says so
let internal (+-) (ctx: Context -> Context) (str: string) x =
    let c = ctx x

    let c =
        if c.BreakOn str then
            writerEvent WriteLine c
        else
            writerEvent (Write " ") c

    writerEvent (Write str) c

/// Append specified string without line-break
let internal (--) (ctx: Context -> Context) (str: string) x = ctx x |> writerEvent (Write str)

/// Break-line unless we are on empty line
let internal (+~) (ctx: Context -> Context) (str: string) x =
    let addNewline ctx =
        not (forallCharsOnLastLine Char.IsWhiteSpace ctx)

    let c = ctx x

    let c =
        if addNewline c then
            writerEvent WriteLine c
        else
            c

    writerEvent (Write str) c

let internal (!-) (str: string) = id -- str
let internal (!+) (str: string) = id ++ str
let internal (!+-) (str: string) = id +- str
let internal (!+~) (str: string) = id +~ str

/// Print object converted to string
let internal str (o: 'T) (ctx: Context) =
    ctx |> writerEvent (Write(o.ToString()))

/// Similar to col, and supply index as well
let internal coli f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()

    while (e.MoveNext()) do
        if tryPick then
            tryPick <- false
        else
            st <- f' st

        st <- f i (e.Current) st
        i <- i + 1

    st

/// Process collection - keeps context through the whole processing
/// calls f for every element in sequence and f' between every two elements
/// as a separator. This is a variant that works on typed collections.
let internal col f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()

    while (e.MoveNext()) do
        if tryPick then
            tryPick <- false
        else
            st <- f' st

        st <- f (e.Current) st

    st

// Similar to col but pass the item of 'T to f' as well
let internal colEx f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()

    while (e.MoveNext()) do
        if tryPick then
            tryPick <- false
        else
            st <- f' e.Current st

        st <- f (e.Current) st

    st

/// Similar to col, apply one more function f2 at the end if the input sequence is not empty
let internal colPost f2 f1 (c: seq<'T>) f (ctx: Context) =
    if Seq.isEmpty c then
        ctx
    else
        f2 (col f1 c f ctx)

/// Similar to col, apply one more function f2 at the beginning if the input sequence is not empty
let internal colPre f2 f1 (c: seq<'T>) f (ctx: Context) =
    if Seq.isEmpty c then
        ctx
    else
        col f1 c f (f2 ctx)

let internal colPreEx f2 f1 (c: seq<'T>) f (ctx: Context) =
    if Seq.isEmpty c then
        ctx
    else
        colEx f1 c f (f2 ctx)

/// If there is a value, apply f and f' accordingly, otherwise do nothing
let internal opt (f': Context -> _) o f (ctx: Context) =
    match o with
    | Some x -> f' (f x ctx)
    | None -> ctx

/// similar to opt, only takes a single function f to apply when there is a value
let internal optSingle f o ctx =
    match o with
    | Some x -> f x ctx
    | None -> ctx

/// Similar to opt, but apply f2 at the beginning if there is a value
let internal optPre (f2: _ -> Context) (f1: Context -> _) o f (ctx: Context) =
    match o with
    | Some x -> f1 (f x (f2 ctx))
    | None -> ctx

let internal getListOrArrayExprSize ctx maxWidth xs =
    match ctx.Config.ArrayOrListMultilineFormatter with
    | MultilineFormatterType.CharacterWidth -> Size.CharacterWidth maxWidth
    | MultilineFormatterType.NumberOfItems -> Size.NumberOfItems(List.length xs, ctx.Config.MaxArrayOrListNumberOfItems)

let internal getRecordSize ctx fields =
    match ctx.Config.RecordMultilineFormatter with
    | MultilineFormatterType.CharacterWidth -> Size.CharacterWidth ctx.Config.MaxRecordWidth
    | MultilineFormatterType.NumberOfItems -> Size.NumberOfItems(List.length fields, ctx.Config.MaxRecordNumberOfItems)

/// b is true, apply f1 otherwise apply f2
let internal ifElse b (f1: Context -> Context) f2 (ctx: Context) = if b then f1 ctx else f2 ctx

let internal ifElseCtx cond (f1: Context -> Context) f2 (ctx: Context) = if cond ctx then f1 ctx else f2 ctx

/// apply f only when cond is true
let internal onlyIf cond f ctx = if cond then f ctx else ctx

let internal onlyIfNot cond f ctx = if cond then ctx else f ctx

let internal whenShortIndent f ctx =
    onlyIf (ctx.Config.IndentSize < 3) f ctx

/// Repeat application of a function n times
let internal rep n (f: Context -> Context) (ctx: Context) =
    [ 1 .. n ] |> List.fold (fun c _ -> f c) ctx

let internal wordAnd = !- " and "
let internal wordOr = !- " or "
let internal wordOf = !- " of "
// Separator functions
let internal sepNone = id
let internal sepDot = !- "."

let internal sepSpace (ctx: Context) =
    if ctx.WriterModel.IsDummy then
        (!- " ") ctx
    else
        match lastWriteEventOnLastLine ctx with
        | Some w when (w.EndsWith(" ") || w.EndsWith Environment.NewLine) -> ctx
        | None -> ctx
        | _ -> (!- " ") ctx

let internal sepNln = !+ ""

let internal sepNlnUnlessLastEventIsNewline (ctx: Context) =
    if lastWriteEventIsNewline ctx then
        ctx
    else
        sepNln ctx

let internal sepStar = !- " * "
let internal sepStarFixed = !- "* "
let internal sepEq = !- " ="
let internal sepEqFixed = !- "="
let internal sepArrow = !- " -> "
let internal sepArrowFixed = !- "-> "
let internal sepArrowRev = !- " <- "
let internal sepWild = !- "_"

let internal sepBar = !- "| "

/// opening token of list
let internal sepOpenL (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str "[ " ctx
    else
        str "[" ctx

/// closing token of list
let internal sepCloseL (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str " ]" ctx
    else
        str "]" ctx

/// opening token of list
let internal sepOpenLFixed = !- "["

/// closing token of list
let internal sepCloseLFixed = !- "]"

/// opening token of array
let internal sepOpenA (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str "[| " ctx
    else
        str "[|" ctx

/// closing token of array
let internal sepCloseA (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str " |]" ctx
    else
        str "|]" ctx

/// opening token of list
let internal sepOpenAFixed = !- "[|"
/// closing token of list
let internal sepCloseAFixed = !- "|]"

/// opening token of sequence or record
let internal sepOpenS (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str "{ " ctx
    else
        str "{" ctx

/// closing token of sequence or record
let internal sepCloseS (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str " }" ctx
    else
        str "}" ctx

/// opening token of anon record
let internal sepOpenAnonRecd (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str "{| " ctx
    else
        str "{|" ctx

/// closing token of anon record
let internal sepCloseAnonRecd (ctx: Context) =
    if ctx.Config.SpaceAroundDelimiter then
        str " |}" ctx
    else
        str "|}" ctx

/// opening token of anon record
let internal sepOpenAnonRecdFixed = !- "{|"

/// closing token of anon record
let internal sepCloseAnonRecdFixed = !- "|}"

/// opening token of sequence
let internal sepOpenSFixed = !- "{"

/// closing token of sequence
let internal sepCloseSFixed = !- "}"

/// opening token of tuple
let internal sepOpenT = !- "("

/// closing token of tuple
let internal sepCloseT = !- ")"

// we need to make sure each expression in the function application has offset at least greater than
// indentation of the function expression itself
// we replace sepSpace in such case
// remarks: https://github.com/fsprojects/fantomas/issues/545
let internal indentIfNeeded f (ctx: Context) =
    let savedColumn = ctx.WriterModel.AtColumn

    if savedColumn >= ctx.Column then
        // missingSpaces needs to be at least one more than the column
        // of function expression being applied upon, otherwise (as known up to F# 4.7)
        // this would lead to a compile error for the function application
        let missingSpaces =
            (savedColumn - ctx.FinalizeModel.Column + 1)

        atIndentLevel true savedColumn (!-(String.replicate missingSpaces " ")) ctx
    else
        f ctx

let internal eventsWithoutMultilineWrite ctx =
    { ctx with
          WriterEvents =
              ctx.WriterEvents
              |> Queue.toSeq
              |> Seq.filter
                  (function
                  | Write s when s.Contains("\n") -> false
                  | _ -> true)
              |> Queue.ofSeq }

let private shortExpressionWithFallback
    (shortExpression: Context -> Context)
    (fallbackExpression)
    maxWidth
    startColumn
    (ctx: Context)
    =
    // if the context is already inside a ShortExpression mode and tries to figure out if the expression will go over the page width,
    // we should try the shortExpression in this case.
    match ctx.WriterModel.Mode with
    | ShortExpression infos when
        (List.exists
            (fun info ->
                info.ConfirmedMultiline
                || info.IsTooLong ctx.Config.MaxLineLength ctx.Column)
            infos) -> ctx
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
            if List.exists
                (fun info ->
                    info.ConfirmedMultiline
                    || info.IsTooLong ctx.Config.MaxLineLength resultContext.Column)
                infos then
                fallbackExpression ctx
            else
                { resultContext with
                      WriterModel =
                          { resultContext.WriterModel with
                                Mode = ctx.WriterModel.Mode } }
        | _ ->
            // you should never hit this branch
            fallbackExpression ctx

let internal isShortExpression maxWidth (shortExpression: Context -> Context) (fallbackExpression) (ctx: Context) =
    shortExpressionWithFallback shortExpression fallbackExpression maxWidth None ctx

let internal isShortExpressionOrAddIndentAndNewline maxWidth expr (ctx: Context) =
    shortExpressionWithFallback expr (indent +> sepNln +> expr +> unindent) maxWidth None ctx

let internal sepSpaceIfShortExpressionOrAddIndentAndNewline maxWidth expr (ctx: Context) =
    shortExpressionWithFallback (sepSpace +> expr) (indent +> sepNln +> expr +> unindent) maxWidth None ctx

let internal expressionFitsOnRestOfLine expression fallbackExpression (ctx: Context) =
    shortExpressionWithFallback expression fallbackExpression ctx.Config.MaxLineLength (Some 0) ctx

let internal isSmallExpression size (smallExpression: Context -> Context) fallbackExpression (ctx: Context) =
    match size with
    | CharacterWidth maxWidth -> isShortExpression maxWidth smallExpression fallbackExpression ctx
    | NumberOfItems (items, maxItems) ->
        if items > maxItems then
            fallbackExpression ctx
        else
            expressionFitsOnRestOfLine smallExpression fallbackExpression ctx

/// provide the line and column before and after the leadingExpression to to the continuation expression
let internal leadingExpressionResult leadingExpression continuationExpression (ctx: Context) =
    let (lineCountBefore, columnBefore) =
        List.length ctx.WriterModel.Lines, ctx.WriterModel.Column

    let contextAfterLeading = leadingExpression ctx

    let (lineCountAfter, columnAfter) =
        List.length contextAfterLeading.WriterModel.Lines, contextAfterLeading.WriterModel.Column

    continuationExpression ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) contextAfterLeading

/// combines two expression and let the second expression know if the first expression was longer than a given threshold.
let internal leadingExpressionLong threshold leadingExpression continuationExpression (ctx: Context) =
    let (lineCountBefore, columnBefore) =
        List.length ctx.WriterModel.Lines, ctx.WriterModel.Column

    let contextAfterLeading = leadingExpression ctx

    let (lineCountAfter, columnAfter) =
        List.length contextAfterLeading.WriterModel.Lines, contextAfterLeading.WriterModel.Column

    continuationExpression
        (lineCountAfter > lineCountBefore
         || (columnAfter - columnBefore > threshold))
        contextAfterLeading

/// A leading expression is not consider multiline if it has a comment before it.
/// For example
/// let a = 7
/// // foo
/// let b = 8
/// let c = 9
/// The second binding b is not consider multiline.
let internal leadingExpressionIsMultiline leadingExpression continuationExpression (ctx: Context) =
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
                | [| Write "" |] -> true
                | _ -> false)

    continuationExpression hasWriteLineEventsAfterExpression contextAfterLeading

let private expressionExceedsPageWidth beforeShort afterShort beforeLong afterLong expr (ctx: Context) =
    // if the context is already inside a ShortExpression mode, we should try the shortExpression in this case.
    match ctx.WriterModel.Mode with
    | ShortExpression infos when
        (List.exists
            (fun info ->
                info.ConfirmedMultiline
                || info.IsTooLong ctx.Config.MaxLineLength ctx.Column)
            infos) -> ctx
    | ShortExpression _ ->
        // if the context is already inside a ShortExpression mode, we should try the shortExpression in this case.
        (beforeShort +> expr +> afterShort) ctx
    | _ ->
        let shortExpressionContext =
            ctx.WithShortExpression(ctx.Config.MaxLineLength, 0)

        let resultContext =
            (beforeShort +> expr +> afterShort) shortExpressionContext

        let fallbackExpression = beforeLong +> expr +> afterLong

        match resultContext.WriterModel.Mode with
        | ShortExpression infos ->
            // verify the expression is not longer than allowed
            if List.exists
                (fun info ->
                    info.ConfirmedMultiline
                    || info.IsTooLong ctx.Config.MaxLineLength resultContext.Column)
                infos then
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
let internal autoIndentAndNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepNone
        sepNone // before and after for short expressions
        (indent +> sepNln)
        unindent // before and after for long expressions
        expr
        ctx

let internal sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepSpace
        sepNone // before and after for short expressions
        (indent +> sepNln)
        unindent // before and after for long expressions
        expr
        ctx

let internal sepSpaceWhenOrIndentAndNlnIfExpressionExceedsPageWidth (addSpace: Context -> bool) expr (ctx: Context) =
    expressionExceedsPageWidth
        (ifElseCtx addSpace sepSpace sepNone)
        sepNone // before and after for short expressions
        (indent +> sepNln)
        unindent // before and after for long expressions
        expr
        ctx

let internal sepSpaceOrNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepSpace
        sepNone // before and after for short expressions
        sepNln
        unindent // before and after for long expressions
        expr
        ctx

let internal autoNlnIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionExceedsPageWidth
        sepNone
        sepNone // before and after for short expressions
        sepNln
        sepNone // before and after for long expressions
        expr
        ctx

let internal autoParenthesisIfExpressionExceedsPageWidth expr (ctx: Context) =
    expressionFitsOnRestOfLine expr (sepOpenT +> expr +> sepCloseT) ctx

let internal futureNlnCheckMem (f, ctx) =
    if ctx.WriterModel.IsDummy || not ctx.BreakLines then
        (false, false)
    else
        // Create a dummy context to evaluate length of current operation
        let dummyCtx : Context =
            ctx.WithDummy(Queue.empty, keepPageWidth = true)
            |> f

        WriterEvents.isMultiline dummyCtx.WriterEvents, dummyCtx.Column > ctx.Config.MaxLineLength

let internal futureNlnCheck f (ctx: Context) =
    let (isMultiLine, isLong) = futureNlnCheckMem (f, ctx)
    isMultiLine || isLong

/// similar to futureNlnCheck but validates whether the expression is going over the max page width
/// This functions is does not use any caching
let internal exceedsWidth maxWidth f (ctx: Context) =
    let dummyCtx : Context =
        ctx.WithDummy(Queue.empty, keepPageWidth = true)

    let currentColumn = dummyCtx.Column
    let ctxAfter : Context = f dummyCtx
    (ctxAfter.Column - currentColumn) > maxWidth

/// Similar to col, skip auto newline for index 0
let internal colAutoNlnSkip0i f' (c: seq<'T>) f (ctx: Context) =
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
let internal colAutoNlnSkip0 f' c f = colAutoNlnSkip0i f' c (fun _ -> f)

/// Skip all auto-breaking newlines
let internal noNln f (ctx: Context) : Context =
    let res = f { ctx with BreakLines = false }
    { res with BreakLines = ctx.BreakLines }

let internal sepSpaceBeforeClassConstructor ctx =
    if ctx.Config.SpaceBeforeClassConstructor then
        sepSpace ctx
    else
        ctx

let internal sepColon (ctx: Context) =
    let defaultExpr =
        if ctx.Config.SpaceBeforeColon then
            str " : "
        else
            str ": "

    if ctx.WriterModel.IsDummy then
        defaultExpr ctx
    else
        match lastWriteEventOnLastLine ctx with
        | Some w when (w.EndsWith(" ")) -> str ": " ctx
        | None -> str ": " ctx
        | _ -> defaultExpr ctx

let internal sepColonFixed = !- ":"

let internal sepColonWithSpacesFixed = !- " : "

let internal sepComma (ctx: Context) =
    if ctx.Config.SpaceAfterComma then
        str ", " ctx
    else
        str "," ctx

let internal sepCommaFixed = str ","

let internal sepSemi (ctx: Context) =
    let { Config = { SpaceBeforeSemicolon = before
                     SpaceAfterSemicolon = after } } =
        ctx

    match before, after with
    | false, false -> str ";"
    | true, false -> str " ;"
    | false, true -> str "; "
    | true, true -> str " ; "
    <| ctx

let internal sepSemiNln (ctx: Context) =
    // sepNln part is essential to indentation
    if ctx.Config.SemicolonAtEndOfLine then
        (!- ";" +> sepNln) ctx
    else
        sepNln ctx

/// Conditional indentation on with keyword
let internal indentOnWith (ctx: Context) =
    if ctx.Config.IndentOnTryWith then
        indent ctx
    else
        ctx

/// Conditional unindentation on with keyword
let internal unindentOnWith (ctx: Context) =
    if ctx.Config.IndentOnTryWith then
        unindent ctx
    else
        ctx

let internal ifAlignBrackets f g =
    ifElseCtx (fun ctx -> ctx.Config.MultilineBlockBracketsOnSameColumn) f g

/// Don't put space before and after these operators
let internal noSpaceInfixOps = set [ "?" ]

/// Always break into newlines on these operators
let internal newLineInfixOps = set [ "|>"; "||>"; "|||>"; ">>"; ">>=" ]

/// Never break into newlines on these operators
let internal noBreakInfixOps = set [ "="; ">"; "<"; "%" ]

let internal printTriviaContent (c: TriviaContent) (ctx: Context) =
    let currentLastLine = lastWriteEventOnLastLine ctx

    // Some items like #if of Newline should be printed on a newline
    // It is hard to always get this right in CodePrinter, so we detect it based on the current code.
    let addNewline =
        currentLastLine
        |> Option.map (fun line -> line.Length > 0)
        |> Option.defaultValue false

    let addSpace =
        currentLastLine
        |> Option.bind
            (fun line ->
                Seq.tryLast line
                |> Option.map (fun lastChar -> lastChar <> ' '))
        |> Option.defaultValue false

    match c with
    | Comment (LineCommentAfterSourceCode s) ->
        let comment =
            sprintf "%s%s" (if addSpace then " " else String.empty) s

        writerEvent (WriteBeforeNewline comment)
    | Comment (BlockComment (s, before, after)) ->
        ifElse (before && addNewline) sepNln sepNone
        +> sepSpace
        -- s
        +> sepSpace
        +> ifElse after sepNln sepNone
    | Newline -> (ifElse addNewline (sepNln +> sepNln) sepNln)
    | Keyword _
    | Number _
    | StringContent _
    | IdentOperatorAsWord _
    | IdentBetweenTicks _
    | CharContent _ -> sepNone // don't print here but somewhere in CodePrinter
    | Directive (s)
    | Comment (LineCommentOnSingleLine s) ->
        (ifElse addNewline sepNln sepNone)
        +> !-s
        +> sepNln
    <| ctx

let internal printContentBefore triviaNode =
    col sepNone triviaNode.ContentBefore printTriviaContent

let internal printContentAfter triviaNode =
    col sepNone triviaNode.ContentAfter printTriviaContent

let private findTriviaRangeEq nodes (range: Range) =
    nodes
    |> List.tryFind (fun n -> RangeHelpers.rangeEq n.Range range)

let private findTriviaOnStartFromRange nodes (range: Range) =
    nodes
    |> List.tryFind (fun n -> RangeHelpers.rangeStartEq n.Range range)

let internal findTriviaTokenFromName (tokenName: FsTokenType) (range: Range) (ctx: Context) =
    Map.tryFind tokenName ctx.TriviaTokenNodes
    |> Option.defaultValue []
    |> List.tryFind (fun n -> RangeHelpers.``range contains`` range n.Range)

let internal enterNodeTokenByName (range: Range) (tokenName: FsTokenType) (ctx: Context) =
    match findTriviaTokenFromName tokenName range ctx with
    | Some triviaNode -> (printContentBefore triviaNode) ctx
    | None -> ctx

let internal leaveNodeTokenByName (range: Range) (tokenName: FsTokenType) (ctx: Context) =
    match findTriviaTokenFromName tokenName range ctx with
    | Some triviaNode -> (printContentAfter triviaNode) ctx
    | None -> ctx

let internal enterNodeFor (mainNodeName: FsAstType) (range: Range) (ctx: Context) =
    match Map.tryFind mainNodeName ctx.TriviaMainNodes with
    | Some triviaNodes ->
        let tn =
            List.tryFind
                (fun { Range = r; ContentBefore = cb } -> List.isNotEmpty cb && RangeHelpers.rangeEq r range)
                triviaNodes

        match tn with
        | Some triviaNode -> (printContentBefore triviaNode) ctx
        | None -> ctx
    | None -> ctx

let internal leaveNodeFor (mainNodeName: FsAstType) (range: Range) (ctx: Context) =
    match Map.tryFind mainNodeName ctx.TriviaMainNodes with
    | Some triviaNodes ->
        let tn =
            List.tryFind
                (fun { Range = r; ContentAfter = ca } -> List.isNotEmpty ca && RangeHelpers.rangeEq r range)
                triviaNodes

        match tn with
        | Some triviaNode -> (printContentAfter triviaNode) ctx
        | None -> ctx
    | None -> ctx

let internal leaveLeftToken (tokenName: FsTokenType) (range: Range) (ctx: Context) =
    (Map.tryFindOrEmptyList tokenName ctx.TriviaTokenNodes)
    |> List.tryFind
        (fun tn ->
            tn.Range.StartLine = range.StartLine
            && tn.Range.StartColumn = range.StartColumn)
    |> fun tn ->
        match tn with
        | Some ({ ContentAfter = [ TriviaContent.Comment (LineCommentAfterSourceCode (lineComment)) ] }) ->
            !-lineComment +> sepNln
        | _ -> id
    <| ctx

let internal leaveLeftBrace = leaveLeftToken LBRACE
let internal leaveLeftBrack = leaveLeftToken LBRACK
let internal leaveLeftBrackBar = leaveLeftToken LBRACK_BAR

let internal enterRightToken (tokenName: FsTokenType) (range: Range) (ctx: Context) =
    (Map.tryFindOrEmptyList tokenName ctx.TriviaTokenNodes)
    |> List.tryFind
        (fun tn ->
            tn.Range.EndLine = range.EndLine
            && (tn.Range.EndColumn = range.EndColumn
                || tn.Range.EndColumn + 1 = range.EndColumn))
    |> fun tn ->
        match tn with
        | Some ({ ContentBefore = [ TriviaContent.Comment (LineCommentOnSingleLine (lineComment)) ] }) ->
            let spacesBeforeComment =
                let braceSize = if tokenName = RBRACK then 1 else 2

                let spaceAround =
                    if ctx.Config.SpaceAroundDelimiter then
                        1
                    else
                        0

                !-String.Empty.PadLeft(braceSize + spaceAround)

            let spaceAfterNewline =
                if ctx.Config.SpaceAroundDelimiter then
                    sepSpace
                else
                    sepNone

            sepNln
            +> spacesBeforeComment
            +> !-lineComment
            +> sepNln
            +> spaceAfterNewline
        | _ -> id
    <| ctx

let internal enterRightBracket = enterRightToken RBRACK
let internal enterRightBracketBar = enterRightToken BAR_RBRACK

let internal hasPrintableContent (trivia: TriviaContent list) =
    trivia
    |> List.exists
        (fun tn ->
            match tn with
            | Comment _
            | Newline
            | Directive _ -> true
            | _ -> false)

let private sepConsideringTriviaContentBeforeBy
    (findNode: Context -> range -> TriviaNode option)
    (sepF: Context -> Context)
    (range: Range)
    (ctx: Context)
    =
    match findNode ctx range with
    | Some ({ ContentBefore = contentBefore }) when (hasPrintableContent contentBefore) -> ctx
    | _ -> sepF ctx

let internal sepConsideringTriviaContentBefore sepF (key: Choice<FsAstType, FsTokenType>) (range: Range) ctx =
    let findTrivia ctx range =
        match key with
        | Choice1Of2 fsAstKey -> findTriviaRangeEq (Map.tryFindOrEmptyList fsAstKey ctx.TriviaMainNodes) range
        | Choice2Of2 fsTokenKey ->
            findTriviaOnStartFromRange (Map.tryFindOrEmptyList fsTokenKey ctx.TriviaTokenNodes) range

    sepConsideringTriviaContentBeforeBy findTrivia sepF range ctx

let internal sepConsideringTriviaContentBeforeForToken sepF (fsTokenKey: FsTokenType) (range: Range) (ctx: Context) =
    let findTrivia ctx range =
        findTriviaTokenFromName fsTokenKey range ctx

    sepConsideringTriviaContentBeforeBy findTrivia sepF range ctx

let internal sepConsideringTriviaContentBeforeForMainNode sepF (mainNodeName: FsAstType) (range: Range) (ctx: Context) =
    let findNode ctx range =
        Map.tryFind mainNodeName ctx.TriviaMainNodes
        |> Option.defaultValue []
        |> List.tryFind (fun { ContentBefore = cb; Range = r } -> List.isNotEmpty cb && RangeHelpers.rangeEq r range)

    sepConsideringTriviaContentBeforeBy findNode sepF range ctx

let internal sepNlnConsideringTriviaContentBefore (key: Choice<FsAstType, FsTokenType>) (range: Range) =
    sepConsideringTriviaContentBefore sepNln key range

let internal sepNlnConsideringTriviaContentBeforeForToken (fsTokenKey: FsTokenType) (range: Range) =
    sepConsideringTriviaContentBeforeForToken sepNln fsTokenKey range

let internal sepNlnConsideringTriviaContentBeforeForMainNode (mainNode: FsAstType) (range: Range) =
    sepConsideringTriviaContentBeforeForMainNode sepNln mainNode range

let internal sepNlnConsideringTriviaContentBeforeWithAttributesFor
    (mainNode: FsAstType)
    (ownRange: Range)
    (attributeRanges: Range seq)
    (ctx: Context)
    =
    let triviaNode =
        match Map.tryFind mainNode ctx.TriviaMainNodes with
        | Some triviaNodes ->
            List.tryFind
                (fun { Range = r; ContentBefore = cb } ->
                    hasPrintableContent cb
                    && RangeHelpers.rangeEq r ownRange)
                triviaNodes
        | None -> None

    let attributeNode =
        match Map.tryFind SynAttributeList_ ctx.TriviaMainNodes with
        | Some attributeNodes ->
            List.tryFind
                (fun { Range = r; ContentBefore = cb } ->
                    hasPrintableContent cb
                    && Seq.exists (RangeHelpers.rangeEq r) attributeRanges)
                attributeNodes
        | None -> None

    if Option.isSome triviaNode
       || Option.isSome attributeNode then
        ctx
    else
        sepNln ctx

let internal sepNlnForEmptyModule (mainNode: FsAstType) (moduleRange: Range) (ctx: Context) =
    match Map.tryFind mainNode ctx.TriviaMainNodes with
    | Some nodes ->
        if List.exists
            (fun tn ->
                RangeHelpers.rangeEq tn.Range moduleRange
                && (hasPrintableContent tn.ContentBefore
                    || hasPrintableContent tn.ContentAfter))
            nodes then
            ctx
        else
            sepNln ctx
    | _ -> sepNln ctx

let internal sepNlnForEmptyNamespace (namespaceRange: Range) ctx =
    let emptyNamespaceRange =
        mkRange namespaceRange.FileName (mkPos 0 0) namespaceRange.End

    match TriviaHelpers.findInRange (Map.tryFindOrEmptyList Ident_ ctx.TriviaMainNodes) emptyNamespaceRange with
    | Some node when
        hasPrintableContent node.ContentBefore
        || hasPrintableContent node.ContentAfter -> ctx
    | _ -> sepNln ctx

let internal sepNlnTypeAndMembers (firstMemberRange: Range option) (mainNodeType: FsAstType) ctx =
    match firstMemberRange with
    | Some range when (ctx.Config.NewlineBetweenTypeDefinitionAndMembers) ->
        sepNlnConsideringTriviaContentBeforeForMainNode mainNodeType range ctx
    | _ -> ctx

let internal sepNlnWhenWriteBeforeNewlineNotEmpty fallback (ctx: Context) =
    if String.isNotNullOrEmpty ctx.WriterModel.WriteBeforeNewline then
        sepNln ctx
    else
        fallback ctx

let internal autoNlnConsideringTriviaIfExpressionExceedsPageWidth
    sepNlnConsideringTriviaContentBefore
    expr
    (ctx: Context)
    =
    expressionExceedsPageWidth
        sepNone
        sepNone // before and after for short expressions
        sepNlnConsideringTriviaContentBefore
        sepNone // before and after for long expressions
        expr
        ctx

let internal addExtraNewlineIfLeadingWasMultiline leading sepNlnConsideringTriviaContentBefore continuation =
    leadingExpressionIsMultiline
        leading
        (fun ml ->
            sepNln
            +> onlyIf ml sepNlnConsideringTriviaContentBefore
            +> continuation)

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
///
/// The range in the tuple is the range of expression

type internal ColMultilineItem = (Context -> Context) * (Context -> Context) * range

let internal colWithNlnWhenItemIsMultiline (items: ColMultilineItem list) =
    let firstItemRange =
        List.tryHead items
        |> Option.map (fun (_, _, r) -> r)

    let rec impl items =
        match items with
        | (f1, sepNln1, r1) :: (_, sepNln2, _) :: _ ->
            let f1Expr =
                match firstItemRange with
                | Some (fr1) when (fr1 = r1) ->
                    // first expression should always be executed as is.
                    f1
                | _ ->
                    // Maybe the previous statement already introduced a complete blank line.
                    // If not add a new line but consider trivia.
                    ifElseCtx
                        newlineBetweenLastWriteEvent
                        f1
                        (autoNlnConsideringTriviaIfExpressionExceedsPageWidth sepNln1 f1)

            addExtraNewlineIfLeadingWasMultiline f1Expr sepNln2 (impl (List.skip 1 items))
        | [ (f, sepNln, r) ] ->
            match firstItemRange with
            | Some (fr1) when (fr1 = r) ->
                // this can only happen when there is only one item in items
                f
            | _ ->
                ifElseCtx newlineBetweenLastWriteEvent f (autoNlnConsideringTriviaIfExpressionExceedsPageWidth sepNln f)
        | [] -> sepNone

    impl items

let internal genTriviaBeforeClausePipe (rangeOfClause: Range) ctx =
    (Map.tryFindOrEmptyList BAR ctx.TriviaTokenNodes)
    |> List.tryFind
        (fun t ->
            t.Range.StartColumn < rangeOfClause.StartColumn
            && t.Range.StartLine = rangeOfClause.StartLine)
    |> fun trivia ->
        match trivia with
        | Some trivia ->
            let containsOnlyDirectives =
                trivia.ContentBefore
                |> List.forall
                    (fun tn ->
                        match tn with
                        | Directive _ -> true
                        | _ -> false)

            onlyIf containsOnlyDirectives sepNlnUnlessLastEventIsNewline
            +> printContentBefore trivia
        | None -> id
    <| ctx

let internal hasLineCommentAfterInfix (rangePlusInfix: Range) (ctx: Context) =
    match Map.tryFind SynExpr_Ident ctx.TriviaMainNodes with
    | Some triviaNodes ->
        triviaNodes
        |> List.tryFind
            (fun { ContentAfter = ca; Range = r } ->
                List.isNotEmpty ca
                && RangeHelpers.rangeEq r rangePlusInfix)
        |> Option.bind
            (fun trivia ->
                trivia.ContentAfter
                |> List.map
                    (fun ca ->
                        match ca with
                        | TriviaContent.Comment (Comment.LineCommentAfterSourceCode (comment)) -> Some comment
                        | _ -> None)
                |> List.choose id
                |> List.tryHead)
        |> Option.map (fun _ -> true)
        |> Option.defaultValue false
    | _ -> false

let internal lastLineOnlyContains characters (ctx: Context) =
    let lastLine =
        (writeEventsOnLastLine ctx |> String.concat "")
            .Trim(characters)

    let length = String.length lastLine
    length = 0 || length < ctx.Config.IndentSize

// Add a newline when the previous code is only one line above the current location
// For example
// let a = meh
//.
// => The dot is the current point and you want to insert an extra newline in this case
//
// Other example
// let a = foo
//
// .
// => Already two newline character between the dot and the previous code, no need to add an extra newline.
//
// Don't add an extra newline if the previous code ends with `=` or `->`
// For example
// type Foo =
//     .
// => no need for a newline here
let internal sepNlnBeforeMultilineConstruct (mainNode: FsAstType) range rangeOfAttributes ctx =
    let existingNewlines =
        ctx.WriterEvents
        |> Queue.rev
        |> Seq.takeWhile
            (function
            | Write ""
            // for example:
            // type Foo =
            //     static member Bar () = ...
            | IndentBy _
            | WriteLine
            | SetAtColumn _
            | Write " -> "
            | CommentOrDefineEvent _ -> true
            | _ -> false)
        |> Seq.filter
            (function
            | WriteLine
            | IndentBy _
            | Write " -> "
            | CommentOrDefineEvent _ -> true
            | _ -> false)
        |> Seq.length

    if existingNewlines >= 2 then
        ctx // previous construct was multiline so no need to add any extra newlines
    else
        // previous construct was single line so add extra newline
        // sepNlnConsideringTriviaContentBeforeWithAttributes range rangeOfAttributes ctx
        sepNlnConsideringTriviaContentBeforeWithAttributesFor mainNode range rangeOfAttributes ctx
