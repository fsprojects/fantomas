module Fantomas.Context

open System
open System.IO
open System.CodeDom.Compiler
open FSharp.Compiler.Range
open Fantomas.FormatConfig
open Fantomas.TriviaTypes

/// Wrapping IndentedTextWriter with current column position
type ColumnIndentedTextWriter(tw : TextWriter, ?isDummy) =
    let isDummy = isDummy |> Option.defaultValue false
    let indentWriter = new IndentedTextWriter(tw, " ")
    let mutable col = indentWriter.Indent

    // on newline, bigger from Indent and atColumn is selected
    // that way we avoid bigger than indentSpace indentation when indent is used after atCurrentColumn
    let mutable atColumn = 0
    
    let mutable toWriteBeforeNewLine = ""
    
    let applyAtColumn f =
        let newIndent = f atColumn
        indentWriter.Indent <- newIndent

    member __.IsDummy = isDummy
    
    member __.ApplyAtColumn f = applyAtColumn f
    
    member __.Write(s : string) =
        match s.LastIndexOf('\n') with
        | -1 -> col <- col + s.Length
        | i ->
            applyAtColumn (fun x -> max indentWriter.Indent x)
            col <- s.Length - i - 1
        indentWriter.Write(s)

    member __.WriteLine(s : string) =
        applyAtColumn (fun x -> max indentWriter.Indent x)
        col <- indentWriter.Indent
        indentWriter.WriteLine(s + toWriteBeforeNewLine)
        toWriteBeforeNewLine <- ""

    member __.WriteBeforeNextNewLine(s : string) =
        toWriteBeforeNewLine <- s
    
    member __.Dump() =
        indentWriter.InnerWriter.ToString() + toWriteBeforeNewLine
    
    /// Current column of the page in an absolute manner
    member __.Column 
        with get() = col
        and set i = col <- i

    member __.Indent 
        with get() = indentWriter.Indent
        and set i = indentWriter.Indent <- i

    member __.AtColumn 
        with get() = atColumn
        and set i = atColumn <- i    

    member __.InnerWriter = indentWriter.InnerWriter

    interface IDisposable with
        member __.Dispose() =
            indentWriter.Dispose()    

type internal Context = 
    { Config : FormatConfig; 
      Writer : ColumnIndentedTextWriter;
      BreakLines : bool;
      BreakOn : string -> bool;
      /// The original source string to query as a last resort 
      Content : string; 
      /// Positions of new lines in the original source string
      Positions : int []; 
      Trivia : TriviaNode list
      RecordBraceStart: int list }

    /// Initialize with a string writer and use space as delimiter
    static member Default = 
        { Config = FormatConfig.Default
          Writer = new ColumnIndentedTextWriter(new StringWriter())
          BreakLines = true; BreakOn = (fun _ -> false) 
          Content = ""
          Positions = [||]
          Trivia = []
          RecordBraceStart = [] }

    static member create config defines (content : string) maybeAst =
        let content = String.normalizeNewLine content
        let positions = 
            content.Split('\n')
            |> Seq.map (fun s -> String.length s + 1)
            |> Seq.scan (+) 0
            |> Seq.toArray

        let (tokens, lineCount) = TokenParser.tokenize defines content
        let trivia =
            match maybeAst, config.StrictMode with
            | Some ast, false -> Trivia.collectTrivia config tokens lineCount ast
            | _ -> Context.Default.Trivia

        { Context.Default with 
            Config = config
            Content = content
            Positions = positions 
            Trivia = trivia }

    member x.MemoizeProjection = x.Writer.Column, x.Trivia, x.BreakLines, x.RecordBraceStart
    
    member x.With(writer : ColumnIndentedTextWriter, ?keepPageWidth) =
        let keepPageWidth = keepPageWidth |> Option.defaultValue false
        writer.Indent <- x.Writer.Indent
        writer.Column <- x.Writer.Column
        writer.AtColumn <- x.Writer.AtColumn
        // Use infinite column width to encounter worst-case scenario
        let config = { x.Config with PageWidth = if keepPageWidth then x.Config.PageWidth else Int32.MaxValue }
        { x with Writer = writer; Config = config }

let internal dump (ctx: Context) =
    ctx.Writer.Dump()

#if DEBUG
let internal dumpAndContinue (ctx: Context) =
    let code = dump ctx
    printfn "%s" code
    ctx
#endif

// A few utility functions from https://github.com/fsharp/powerpack/blob/master/src/FSharp.Compiler.CodeDom/generator.fs

/// Indent one more level based on configuration
let internal indent (ctx : Context) = 
    ctx.Writer.Indent <- ctx.Writer.Indent + ctx.Config.IndentSpaceNum
    // if atColumn is bigger then after indent, then we use atColumn as base for indent
    ctx.Writer.ApplyAtColumn (fun x -> if x >= ctx.Writer.Indent then x + ctx.Config.IndentSpaceNum else ctx.Writer.Indent)
    ctx

/// Unindent one more level based on configuration
let internal unindent (ctx : Context) = 
    ctx.Writer.Indent <- max ctx.Writer.AtColumn (ctx.Writer.Indent - ctx.Config.IndentSpaceNum)
    ctx

/// Increase indent by i spaces
let internal incrIndent i (ctx : Context) = 
    ctx.Writer.Indent <- ctx.Writer.Indent + i
    ctx

/// Decrease indent by i spaces
let internal decrIndent i (ctx : Context) = 
    ctx.Writer.Indent <- max 0 (ctx.Writer.Indent - i)
    ctx

/// Apply function f at an absolute indent level (use with care)
let internal atIndentLevel alsoSetIndent level (f : Context -> Context) ctx =
    if level < 0 then
        invalidArg "level" "The indent level cannot be negative."
    let oldLevel = ctx.Writer.Indent
    let oldColumn = ctx.Writer.AtColumn
    if alsoSetIndent then ctx.Writer.Indent <- level
    ctx.Writer.AtColumn <- level
    let result = f ctx
    ctx.Writer.AtColumn <- oldColumn
    ctx.Writer.Indent <- oldLevel
    result

/// Set minimal indentation (`atColumn`) at current column position - next newline will be indented on `max indent atColumn`
/// Example:
/// { X = // indent=0, atColumn=2
///     "some long string" // indent=4, atColumn=2
///   Y = 1 // indent=0, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, but "some long string" have indent only 4, because it is bigger than `atColumn` (2).
let internal atCurrentColumn (f : _ -> Context) (ctx : Context) =
    atIndentLevel false ctx.Writer.Column f ctx

/// Write everything at current column indentation, set `indent` and `atColumn` on current column position
/// /// Example (same as above):
/// { X = // indent=2, atColumn=2
///       "some long string" // indent=6, atColumn=2
///   Y = 1 // indent=2, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, "some long string" have indent 6, because it is indented from `atCurrentColumn` pos (2).
let internal atCurrentColumnIndent (f : _ -> Context) (ctx : Context) =
    atIndentLevel true ctx.Writer.Column f ctx

/// Function composition operator
let internal (+>) (ctx : Context -> Context) (f : _ -> Context) x =
    f (ctx x)

/// Break-line and append specified string
let internal (++) (ctx : Context -> Context) (str : string) x =
    let c = ctx x
    c.Writer.WriteLine("")
    c.Writer.Write(str)
    c

/// Break-line if config says so
let internal (+-) (ctx : Context -> Context) (str : string) x =
    let c = ctx x
    if c.BreakOn str then 
        c.Writer.WriteLine("")
    else
        c.Writer.Write(" ")
    c.Writer.Write(str)
    c

/// Append specified string without line-break
let internal (--) (ctx : Context -> Context) (str : string) x =
    let c = ctx x
    c.Writer.Write(str)
    c

/// Break-line unless we are on empty line
let internal (+~) (ctx : Context -> Context) (str : string) x =
    let addNewline ctx =
        dump ctx
        |> String.normalizeThenSplitNewLine
        |> Array.tryLast
        |> Option.map (fun (line:string) -> line.Trim().Length > 1)
        |> Option.defaultValue false
    let c = ctx x
    if addNewline c then 
        c.Writer.WriteLine("")
    c.Writer.Write(str)
    c

let internal (!-) (str : string) = id -- str 
let internal (!+) (str : string) = id ++ str 
let internal (!+-) (str : string) = id +- str 
let internal (!+~) (str : string) = id +~ str 

/// Print object converted to string
let internal str (o : 'T) (ctx : Context) =
    ctx.Writer.Write(o.ToString())
    ctx

/// Similar to col, and supply index as well
let internal coli f' (c : seq<'T>) f (ctx : Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f i (e.Current) st
        i  <- i + 1
    st

/// Process collection - keeps context through the whole processing
/// calls f for every element in sequence and f' between every two elements 
/// as a separator. This is a variant that works on typed collections.
let internal col f' (c : seq<'T>) f (ctx : Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f (e.Current) st
    st

// Similar to col but pass the item of 'T to f' as well
let internal colEx f' (c : seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' e.Current st
        st <- f (e.Current) st
    st

/// Similar to col, apply one more function f2 at the end if the input sequence is not empty
let internal colPost f2 f1 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else f2 (col f1 c f ctx)

/// Similar to col, apply one more function f2 at the beginning if the input sequence is not empty
let internal colPre f2 f1 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else col f1 c f (f2 ctx)

/// If there is a value, apply f and f' accordingly, otherwise do nothing
let internal opt (f' : Context -> _) o f (ctx : Context) =
    match o with
    | Some x -> f' (f x ctx)
    | None -> ctx

/// Similar to opt, but apply f2 at the beginning if there is a value
let internal optPre (f2 : _ -> Context) (f1 : Context -> _) o f (ctx : Context) =
    match o with
    | Some x -> f1 (f x (f2 ctx))
    | None -> ctx

/// b is true, apply f1 otherwise apply f2
let internal ifElse b (f1 : Context -> Context) f2 (ctx : Context) =
    if b then f1 ctx else f2 ctx

let internal ifElseCtx cond (f1 : Context -> Context) f2 (ctx : Context) =
    if cond ctx then f1 ctx else f2 ctx

/// Repeat application of a function n times
let internal rep n (f : Context -> Context) (ctx : Context) =
    [1..n] |> List.fold (fun c _ -> f c) ctx

let internal wordAnd = !- " and "
let internal wordOr = !- " or "
let internal wordOf = !- " of "   

// Separator functions
        
let internal sepDot = !- "."
let internal sepSpace =
    // ignore multiple spaces, space on start of file, after newline
    // TODO: this is inefficient - maybe remember last char written?
    fun (ctx: Context) ->
        if (not ctx.Writer.IsDummy && let s = dump ctx in s = "" || s.EndsWith " " || s.EndsWith Environment.NewLine) then ctx
        else (!- " ") ctx      
let internal sepNln = !+ ""
let internal sepStar = !- " * "
let internal sepEq = !- " ="
let internal sepArrow = !- " -> "
let internal sepWild = !- "_"
let internal sepNone = id
let internal sepBar = !- "| "

/// opening token of list
let internal sepOpenL (ctx : Context) =  
    if ctx.Config.SpaceAroundDelimiter then str "[ " ctx else str "[" ctx 

/// closing token of list
let internal sepCloseL (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str " ]" ctx else str "]" ctx 

/// opening token of list
let internal sepOpenLFixed = !- "["

/// closing token of list
let internal sepCloseLFixed = !- "]"

/// opening token of array
let internal sepOpenA (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str "[| " ctx else str "[|" ctx 

/// closing token of array
let internal sepCloseA (ctx : Context) = 
    if ctx.Config.SpaceAroundDelimiter then str " |]" ctx else str "|]" ctx 

/// opening token of list
let internal sepOpenAFixed = !- "[|"
/// closing token of list
let internal sepCloseAFixed = !- "|]"

/// opening token of sequence or record
let internal sepOpenS (ctx : Context) = 
    if ctx.Config.SpaceAroundDelimiter then str "{ " ctx else str "{" ctx 

/// closing token of sequence or record
let internal sepCloseS (ctx : Context) = 
    if ctx.Config.SpaceAroundDelimiter then str " }" ctx else str "}" ctx

/// opening token of anon record
let internal sepOpenAnonRecd (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str "{| " ctx else str "{|" ctx 

/// closing token of anon record
let internal sepCloseAnonRecd (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str " |}" ctx else str "|}" ctx

/// opening token of sequence
let internal sepOpenSFixed = !- "{"

/// closing token of sequence
let internal sepCloseSFixed = !- "}"

/// opening token of tuple
let internal sepOpenT = !- "("

/// closing token of tuple
let internal sepCloseT = !- ")"

let internal autoNlnCheck f sep (ctx : Context) =
    if not ctx.BreakLines then false else
    // Create a dummy context to evaluate length of current operation
    use colWriter = new ColumnIndentedTextWriter(new StringWriter(), isDummy = true)
    let dummyCtx = ctx.With(colWriter)
    let col = (dummyCtx |> sep |> f).Writer.Column
    // This isn't accurate if we go to new lines
    col > ctx.Config.PageWidth

let internal futureNlnCheckMem = Cache.memoizeBy (fun (f, ctx : Context) -> Cache.LambdaEqByRef f, ctx.MemoizeProjection) <| fun (f, ctx) ->
    if ctx.Writer.IsDummy || not ctx.BreakLines then false else
    // Create a dummy context to evaluate length of current operation
    use colWriter = new ColumnIndentedTextWriter(new StringWriter(), isDummy = true)
    let dummyCtx = ctx.With(colWriter, true)
    let writer = (dummyCtx |> f).Writer
    let str = writer.Dump()
    let withoutStringConst = 
        str.Replace("\\\\", System.String.Empty).Replace("\\\"", System.String.Empty).Split([|'"'|])
        |> Seq.indexed |> Seq.filter (fun (i, _) -> i % 2 = 0) |> Seq.map snd |> String.concat System.String.Empty
    let lines = withoutStringConst.Split([|Environment.NewLine|], StringSplitOptions.None) 

    (lines |> Seq.length) >= 2 || writer.Column > ctx.Config.PageWidth

let internal futureNlnCheck f (ctx : Context) = futureNlnCheckMem (f, ctx)

let internal autoNlnByFuture f = ifElseCtx (futureNlnCheck f) (sepNln +> f) f
let internal autoIndentNlnByFuture f = ifElseCtx (futureNlnCheck f) (indent +> sepNln +> f +> unindent) f

/// Set a checkpoint to break at an appropriate column
let internal autoNlnOrAddSep f sep (ctx : Context) =
    let isNln = autoNlnCheck f sep ctx
    if isNln then
       f (sepNln ctx)
    else
       f (sep ctx)

let internal autoNln f (ctx : Context) = autoNlnOrAddSep f sepNone ctx

let internal autoNlnOrSpace f (ctx : Context) = autoNlnOrAddSep f sepSpace ctx

/// Similar to col, skip auto newline for index 0
let internal colAutoNlnSkip0i f' (c : seq<'T>) f (ctx : Context) = 
    coli f' c (fun i c -> if i = 0 then f i c else autoNln (f i c)) ctx

/// Similar to col, skip auto newline for index 0
let internal colAutoNlnSkip0 f' c f = colAutoNlnSkip0i f' c (fun _ -> f)

/// Skip all auto-breaking newlines
let internal noNln f (ctx : Context) : Context = 
    let res = f { ctx with BreakLines = false }
    { res with BreakLines = ctx.BreakLines }

let internal sepColon (ctx : Context) = 
    if ctx.Config.SpaceBeforeColon then str " : " ctx else str ": " ctx

let internal sepColonFixed = !- ":"

let internal sepColonWithSpacesFixed = !- " : "

let internal sepComma (ctx : Context) = 
    if ctx.Config.SpaceAfterComma then str ", " ctx else str "," ctx

let internal sepSemi (ctx : Context) = 
    if ctx.Config.SpaceAfterSemicolon then str "; " ctx else str ";" ctx

let internal sepSemiNln (ctx : Context) =
    // sepNln part is essential to indentation
    if ctx.Config.SemicolonAtEndOfLine then (!- ";" +> sepNln) ctx else sepNln ctx

let internal sepBeforeArg (ctx : Context) = 
    if ctx.Config.SpaceBeforeArgument then str " " ctx else str "" ctx

/// Conditional indentation on with keyword
let internal indentOnWith (ctx : Context) =
    if ctx.Config.IndentOnTryWith then indent ctx else ctx

/// Conditional unindentation on with keyword
let internal unindentOnWith (ctx : Context) =
    if ctx.Config.IndentOnTryWith then unindent ctx else ctx

let internal sortAndDeduplicate by l (ctx : Context) =
    if ctx.Config.ReorderOpenDeclaration then
        l |> Seq.distinctBy by |> Seq.sortBy by |> List.ofSeq
    else l

/// Don't put space before and after these operators
let internal NoSpaceInfixOps = set ["?"]

/// Always break into newlines on these operators
let internal NewLineInfixOps = set ["|>"; "||>"; "|||>"; ">>"; ">>="]

/// Never break into newlines on these operators
let internal NoBreakInfixOps = set ["="; ">"; "<";]

let internal printTriviaContent (c: TriviaContent) (ctx: Context) =
    // Some items like #if of Newline should be printed on a newline
    // It is hard to always get this right in CodePrinter, so we detect it based on the current code.
    let addNewline =
        dump ctx
        |> String.normalizeThenSplitNewLine
        |> Array.tryLast
        |> Option.map (fun (line:string) -> line.Trim().Length > 1)
        |> Option.defaultValue false

    match c with
    | Comment(LineCommentAfterSourceCode s) -> fun ctx -> ctx.Writer.WriteBeforeNextNewLine (" " + s); ctx
    | Comment(BlockComment(s, before, after)) ->
        ifElse (before && addNewline) sepNln sepNone
        +> sepSpace -- s +> sepSpace
        +> ifElse after sepNln sepNone
    | Newline ->
        (ifElse addNewline (sepNln +> sepNln) sepNln)
    | Keyword _
    | Number _
    | StringContent _
    | IdentOperatorAsWord _
    | IdentBetweenTicks _
    | NewlineAfter
         -> sepNone // don't print here but somewhere in CodePrinter
    | Directive(s)
    | Comment(LineCommentOnSingleLine s) ->
        (ifElse addNewline sepNln sepNone) +> !- s +> sepNln
    <| ctx

let private removeNodeFromContext triviaNode (ctx: Context) =
    let newNodes = List.filter (fun tn -> tn <> triviaNode) ctx.Trivia
    { ctx with Trivia = newNodes }

let internal printContentBefore triviaNode =
    // Make sure content is not being printed twice.
    let removeBeforeContentOfTriviaNode =
        fun (ctx:Context) ->
            let trivia =
                ctx.Trivia
                |> List.map (fun tn ->
                    let contentBefore =
                        tn.ContentBefore
                        |> List.filter(fun cb ->
                            match cb with
                            | Keyword _
                            | Number _
                            | StringContent _
                            | IdentOperatorAsWord _ ->
                                true
                            | _ -> false)
                    if tn = triviaNode then
                        { tn with ContentBefore = contentBefore }
                    else
                        tn
                ) 
            { ctx with Trivia = trivia }
        
    col sepNone triviaNode.ContentBefore printTriviaContent +> removeBeforeContentOfTriviaNode

let internal printContentAfter triviaNode =
    col sepNone triviaNode.ContentAfter printTriviaContent

let private findTriviaMainNodeFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n ->
        Trivia.isMainNode n && n.Range.Start = range.Start && n.Range.End = range.End)

let private findTriviaMainNodeOrTokenOnStartFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n ->
        Trivia.isMainNode n && n.Range.Start = range.Start && n.Range.End = range.End
        || Trivia.isToken n && n.Range.Start = range.Start)

let private findTriviaMainNodeOrTokenOnEndFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n ->
        Trivia.isMainNode n && n.Range.Start = range.Start && n.Range.End = range.End
        || Trivia.isToken n && n.Range.End = range.End)

let private findTriviaTokenFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n -> Trivia.isToken n && n.Range.Start = range.Start && n.Range.End = range.End)

let private findTriviaTokenFromName (range: range) nodes (tokenName:string) =
    nodes
    |> List.tryFind(fun n ->
        match n.Type with
        | Token(tn) when tn.TokenInfo.TokenName = tokenName ->
            (range.Start.Line, range.Start.Column) <= (n.Range.Start.Line, n.Range.Start.Column)
            && (range.End.Line, range.End.Column) >= (n.Range.End.Line, n.Range.End.Column)
        | _ -> false)

let internal enterNodeWith f x (ctx: Context) =
    match f ctx.Trivia x with
    | Some triviaNode ->
        (printContentBefore triviaNode) ctx
    | None -> ctx
let internal enterNode (range: range) (ctx: Context) = enterNodeWith findTriviaMainNodeOrTokenOnStartFromRange range ctx
let internal enterNodeToken (range: range) (ctx: Context) = enterNodeWith findTriviaTokenFromRange range ctx
let internal enterNodeTokenByName (range: range) (tokenName:string) (ctx: Context) = enterNodeWith (findTriviaTokenFromName range) tokenName ctx

let internal leaveNodeWith f x (ctx: Context) =
    match f ctx.Trivia x with
    | Some triviaNode ->
        ((printContentAfter triviaNode) +> (removeNodeFromContext triviaNode)) ctx
    | None -> ctx
let internal leaveNode (range: range) (ctx: Context) = leaveNodeWith findTriviaMainNodeOrTokenOnEndFromRange range ctx
let internal leaveNodeToken (range: range) (ctx: Context) = leaveNodeWith findTriviaTokenFromRange range ctx
let internal leaveNodeTokenByName (range: range) (tokenName:string) (ctx: Context) = leaveNodeWith (findTriviaTokenFromName range) tokenName ctx
    
let internal leaveEqualsToken (range: range) (ctx: Context) =
    ctx.Trivia
    |> List.filter(fun tn ->
        match tn.Type with
        | Token(tok) ->
            tok.TokenInfo.TokenName = "EQUALS" && tn.Range.StartLine = range.StartLine
        | _ -> false
    )
    |> List.tryHead
    |> fun tn ->
        match tn with
        | Some({ ContentAfter = [TriviaContent.Comment(LineCommentAfterSourceCode(lineComment))] } as tn) ->
            sepSpace +> !- lineComment +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal leaveLeftBrace (range: range) (ctx: Context) =
    ctx.Trivia
    |> List.tryFind(fun tn ->
        // Token is a left brace { at the beginning of the range.
        match tn.Type with
        | Token(tok) ->
            tok.TokenInfo.TokenName = "LBRACE" && tn.Range.StartLine = range.StartLine && tn.Range.StartColumn = range.StartColumn
        | _ -> false
    )
    |> fun tn ->
        match tn with
        | Some({ ContentAfter = [TriviaContent.Comment(LineCommentAfterSourceCode(lineComment))] } as tn) ->
            !- lineComment +> sepNln +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal enterRightBracket (range: range) (ctx: Context) =
    ctx.Trivia
    |> List.tryFind(fun tn ->
        // Token is a left brace { at the beginning of the range.
        match tn.Type with
        | Token(tok) ->
            (tok.TokenInfo.TokenName = "RBRACK" || tok.TokenInfo.TokenName = "BAR_RBRACK")
            && tn.Range.EndLine = range.EndLine
            && (tn.Range.EndColumn = range.EndColumn || tn.Range.EndColumn + 1 = range.EndColumn)
        | _ -> false
    )
    |> fun tn ->
        match tn with
        | Some({ ContentBefore = [TriviaContent.Comment(LineCommentOnSingleLine(lineComment))] } as tn) ->
            let spacesBeforeComment =
                let braceSize =
                    match tn.Type with
                    | Token({TokenInfo = {TokenName = "BAR_RBRACK"}}) -> 2
                    | _ -> 1
                let spaceAround = if ctx.Config.SpaceAroundDelimiter then 1 else 0

                !- String.Empty.PadLeft(braceSize + spaceAround)

            let spaceAfterNewline = if ctx.Config.SpaceAroundDelimiter then sepSpace else sepNone
            sepNln +> spacesBeforeComment +> !- lineComment +> sepNln +> spaceAfterNewline +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal hasPrintableContent (trivia: TriviaContent list) =
    trivia
    |> List.filter (fun tn ->
        match tn with
        | Comment(_) -> true
        | Newline -> true
        | _ -> false)
    |> List.isEmpty
    |> not
    
let private hasDirectiveBefore (trivia: TriviaContent list) =
    trivia
    |> List.filter (fun tn ->
        match tn with
        | Directive(_) -> true
        | _ -> false)
    |> List.isEmpty
    |> not

let internal sepNlnConsideringTriviaContentBefore (range:range) ctx =
    match findTriviaMainNodeFromRange ctx.Trivia range with
    | Some({ ContentBefore = (Comment(BlockComment(_,false,_)))::_ }) ->
        sepNln ctx
    | Some({ ContentBefore = contentBefore }) when (hasPrintableContent contentBefore) ->
        ctx
    | _ -> sepNln ctx

let internal sepNlnConsideringTriviaContentBeforeWithAttributes (ownRange:range) (attributeRanges: range seq) ctx =
    seq {
        yield ownRange
        yield! attributeRanges
    }
    |> Seq.choose (findTriviaMainNodeFromRange ctx.Trivia)
    |> Seq.exists (fun ({ ContentBefore = contentBefore }) -> hasPrintableContent contentBefore)
    |> fun hasContentBefore ->
        if hasContentBefore then ctx else sepNln ctx
    
let internal beforeElseKeyword (fullIfRange: range) (elseRange: range) (ctx: Context) =
    ctx.Trivia
    |> List.tryFind(fun tn ->
        match tn.Type with
        | Token(tok) ->
            tok.TokenInfo.TokenName = "ELSE" && (fullIfRange.StartLine < tn.Range.StartLine) && (tn.Range.StartLine >= elseRange.StartLine) 
        | _ -> false
    )
    |> fun tn ->
        match tn with
        | Some({ ContentBefore = [TriviaContent.Comment(LineCommentOnSingleLine(lineComment))] } as tn) ->
            sepNln +> !- lineComment +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal genTriviaBeforeClausePipe (rangeOfClause:range) ctx =
    ctx.Trivia
    |> List.tryFind (fun t ->
        match t.Type with
        | Token({ TokenInfo = { TokenName = bar } }) ->
            bar = "BAR" && t.Range.StartColumn < rangeOfClause.StartColumn && t.Range.StartLine = rangeOfClause.StartLine
        | _ -> false
    )
    |> fun trivia ->
        match trivia with
        | Some trivia ->
            let containsOnlyDirectives =
                trivia.ContentBefore
                |> List.forall (fun tn -> match tn with | Directive(_) -> true | _ -> false)
            
            ifElse containsOnlyDirectives sepNln sepNone
            +> printContentBefore trivia
        | None -> id
    <| ctx
    
let internal genCommentsAfterInfix (rangePlusInfix: range option) (ctx: Context) =
    rangePlusInfix
    |> Option.bind (findTriviaMainNodeFromRange ctx.Trivia)
    |> Option.bind (fun trivia ->
        trivia.ContentAfter
        |> List.map (fun ca ->
            match ca with
            | TriviaContent.Comment(Comment.LineCommentAfterSourceCode(comment)) -> Some comment
            | _ -> None
        )
        |> List.choose id
        |> List.tryHead
    )
    |> Option.map (fun comment -> !- comment +> sepNln)
    |> Option.defaultValue id
    <| ctx
    
// Add a newline if there if trivia content before that requires it
let internal sepNlnIfTriviaBefore (range:range) (ctx:Context) =
    match findTriviaMainNodeFromRange ctx.Trivia range with
    | Some({ ContentBefore = contentBefore }) when (hasDirectiveBefore contentBefore) ->
        sepNln
    | _ -> sepNone
    <| ctx