module internal Fantomas.Core.Context

open Fantomas.FCS.Text
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

type ShortExpressionInfo =
    { MaxWidth: int
      StartColumn: int
      ConfirmedMultiline: bool }

    member IsTooLong: maxPageWidth: int -> currentColumn: int -> bool

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

    member IsDummy: bool

[<System.Diagnostics.DebuggerDisplay("\"{Dump()}\""); NoComparison>]
type Context =
    { Config: FormatConfig
      WriterModel: WriterModel
      WriterEvents: Queue<WriterEvent>
      FormattedCursor: pos option }

    /// Initialize with a string writer and use space as delimiter
    static member Default: Context
    static member Create: config: FormatConfig -> Context
    member WithDummy: writerCommands: Queue<WriterEvent> * ?keepPageWidth: bool -> Context
    member WithShortExpression: maxWidth: int * ?startColumn: int -> Context
    member Column: int

/// This adds a WriterEvent to the Context.
/// One event could potentially be split up into multiple events.
/// The event is also being processed in the WriterModel of the Context.
val writerEvent: e: WriterEvent -> ctx: Context -> Context
val hasWriteBeforeNewlineContent: ctx: Context -> bool
val dump: isSelection: bool -> ctx: Context -> FormatResult
val dumpAndContinue: ctx: Context -> Context
val lastWriteEventIsNewline: ctx: Context -> bool
val lastWriteEventIsDotLambda: ctx: Context -> bool

/// Indent one more level based on configuration
val indent: ctx: Context -> Context
/// Unindent one more level based on configuration
val unindent: ctx: Context -> Context
// /// Apply function f at an absolute indent level (use with care)
val atIndentLevel: alsoSetIndent: bool -> level: int -> f: (Context -> Context) -> ctx: Context -> Context
// /// Set minimal indentation (`atColumn`) at current column position - next newline will be indented on `max indent atColumn`
// /// Example:
// /// { X = // indent=0, atColumn=2
// ///     "some long string" // indent=4, atColumn=2
// ///   Y = 1 // indent=0, atColumn=2
// /// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, but "some long string" have indent only 4, because it is bigger than `atColumn` (2).
val atCurrentColumn: f: (Context -> Context) -> ctx: Context -> Context

/// Write everything at current column indentation, set `indent` and `atColumn` on current column position
/// /// Example (same as above):
/// { X = // indent=2, atColumn=2
///       "some long string" // indent=6, atColumn=2
///   Y = 1 // indent=2, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, "some long string" have indent 6, because it is indented from `atCurrentColumn` pos (2).
val atCurrentColumnIndent: f: (Context -> Context) -> ctx: Context -> Context

/// Function composition operator
val (+>): ctx: (Context -> Context) -> f: (Context -> Context) -> x: Context -> Context
val (!-): str: string -> (Context -> Context)

/// Similar to col, and supply index as well
val coli: f': (Context -> Context) -> c: 'T seq -> f: (int -> 'T -> Context -> Context) -> ctx: Context -> Context

/// Process collection - keeps context through the whole processing
/// calls f for every element in sequence and f' between every two elements
/// as a separator. This is a variant that works on typed collections.
val col: f': (Context -> Context) -> c: 'T seq -> f: ('T -> Context -> Context) -> ctx: Context -> Context
val colEx: f': ('T -> Context -> Context) -> c: 'T seq -> f: ('T -> Context -> Context) -> ctx: Context -> Context

/// Similar to col, apply one more function f2 at the end if the input sequence is not empty
val colPost:
    f2: (Context -> Context) ->
    f1: (Context -> Context) ->
    c: 'T seq ->
    f: ('T -> Context -> Context) ->
    ctx: Context ->
        Context

/// Similar to col, apply one more function f2 at the beginning if the input sequence is not empty
val colPre:
    f2: (Context -> Context) ->
    f1: (Context -> Context) ->
    c: 'T seq ->
    f: ('T -> Context -> Context) ->
    ctx: Context ->
        Context

/// If there is a value, apply f and f' accordingly, otherwise do nothing
val opt: f': (Context -> Context) -> o: 'a option -> f: ('a -> Context -> Context) -> ctx: Context -> Context
/// similar to opt, only takes a single function f to apply when there is a value
val optSingle: f: ('a -> 'b -> 'b) -> o: 'a option -> ctx: 'b -> 'b

/// Similar to opt, but apply f2 at the beginning if there is a value
val optPre:
    f2: (Context -> Context) ->
    f1: (Context -> Context) ->
    o: 'a option ->
    f: ('a -> Context -> Context) ->
    ctx: Context ->
        Context

val getListOrArrayExprSize: ctx: Context -> maxWidth: Num -> xs: 'a list -> Size
val getRecordSize: ctx: Context -> fields: 'a list -> Size
/// b is true, apply f1 otherwise apply f2
val ifElse: b: bool -> f1: (Context -> Context) -> f2: (Context -> Context) -> ctx: Context -> Context

val ifElseCtx:
    cond: (Context -> bool) -> f1: (Context -> Context) -> f2: (Context -> Context) -> ctx: Context -> Context

// /// apply f only when cond is true
val onlyIf: cond: bool -> f: ('a -> 'a) -> ctx: 'a -> 'a
val onlyIfCtx: cond: ('a -> bool) -> f: ('a -> 'a) -> ctx: 'a -> 'a
val onlyIfNot: cond: bool -> f: ('a -> 'a) -> ctx: 'a -> 'a
val whenShortIndent: f: (Context -> Context) -> ctx: Context -> Context
/// Repeat application of a function n times
val rep: n: int -> f: (Context -> Context) -> ctx: Context -> Context
val sepNone: ('a -> 'a)
val sepDot: (Context -> Context)
val sepSpace: ctx: Context -> Context
val addFixedSpaces: targetColumn: int -> ctx: Context -> Context
val sepNln: (Context -> Context)
val sepNlnForTrivia: (Context -> Context)
val sepNlnUnlessLastEventIsNewline: ctx: Context -> Context
val sepStar: (Context -> Context)
val sepEq: (Context -> Context)
val sepEqFixed: (Context -> Context)
val sepArrow: (Context -> Context)
val sepArrowRev: (Context -> Context)
val sepBar: (Context -> Context)
val addSpaceIfSpaceAroundDelimiter: ctx: Context -> Context
val addSpaceIfSpaceAfterComma: ctx: Context -> Context
/// opening token of list
val sepOpenLFixed: (Context -> Context)
/// closing token of list
val sepCloseLFixed: (Context -> Context)
/// opening token of anon record
val sepOpenAnonRecdFixed: (Context -> Context)
/// opening token of tuple
val sepOpenT: (Context -> Context)
/// closing token of tuple
val sepCloseT: (Context -> Context)
val wordAnd: (Context -> Context)
val wordAndFixed: (Context -> Context)
val wordOf: (Context -> Context)
val indentSepNlnUnindent: f: (Context -> Context) -> (Context -> Context)

val isShortExpression:
    maxWidth: int ->
    shortExpression: (Context -> Context) ->
    fallbackExpression: (Context -> Context) ->
    ctx: Context ->
        Context

val expressionFitsOnRestOfLine:
    expression: (Context -> Context) -> fallbackExpression: (Context -> Context) -> ctx: Context -> Context

val isSmallExpression:
    size: Size ->
    smallExpression: (Context -> Context) ->
    fallbackExpression: (Context -> Context) ->
    ctx: Context ->
        Context

/// provide the line and column before and after the leadingExpression to to the continuation expression
val leadingExpressionResult:
    leadingExpression: (Context -> Context) ->
    continuationExpression: ((int * int) * (int * int) -> Context -> 'a) ->
    ctx: Context ->
        'a

/// A leading expression is not consider multiline if it has a comment before it.
/// For example
/// let a = 7
/// // foo
/// let b = 8
/// let c = 9
/// The second binding b is not consider multiline.
val leadingExpressionIsMultiline:
    leadingExpression: (Context -> Context) -> continuationExpression: (bool -> Context -> 'a) -> ctx: Context -> 'a

/// try and write the expression on the remainder of the current line
/// add an indent and newline if the expression is longer
val autoIndentAndNlnIfExpressionExceedsPageWidth: expr: (Context -> Context) -> ctx: Context -> Context
val sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth: expr: (Context -> Context) -> ctx: Context -> Context
val sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth: expr: (Context -> Context) -> ctx: Context -> Context

val sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup:
    f: (Expr -> Context -> Context) -> expr: Expr -> ctx: Context -> Context

val sepSpaceOrIndentAndNlnIfTypeExceedsPageWidthUnlessStroustrup:
    f: (Type -> Context -> Context) -> t: Type -> ctx: Context -> Context

val isStroustrupStyleExpr: config: FormatConfig -> e: Expr -> bool

val autoParenthesisIfExpressionExceedsPageWidth: expr: (Context -> Context) -> ctx: Context -> Context
val futureNlnCheck: f: (Context -> Context) -> ctx: Context -> bool
/// similar to futureNlnCheck but validates whether the expression is going over the max page width
/// This functions is does not use any caching
val exceedsWidth: maxWidth: int -> f: (Context -> Context) -> ctx: Context -> bool

/// Similar to col, skip auto newline for index 0
val colAutoNlnSkip0: f': (Context -> Context) -> c: 'a seq -> f: ('a -> Context -> Context) -> (Context -> Context)
val sepSpaceBeforeClassConstructor: ctx: Context -> Context
val sepColon: ctx: Context -> Context
val sepColonFixed: (Context -> Context)
val sepColonWithSpacesFixed: (Context -> Context)
val sepComma: ctx: Context -> Context
val sepSemi: ctx: Context -> Context
val ifAlignOrStroustrupBrackets: f: (Context -> Context) -> g: (Context -> Context) -> (Context -> Context)
val sepNlnWhenWriteBeforeNewlineNotEmptyOr: fallback: (Context -> Context) -> ctx: Context -> Context
val sepNlnWhenWriteBeforeNewlineNotEmpty: (Context -> Context)
val sepSpaceUnlessWriteBeforeNewlineNotEmpty: ctx: Context -> Context
val autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty: f: (Context -> Context) -> ctx: Context -> Context
val addParenIfAutoNln: expr: Expr -> f: (Expr -> Context -> Context) -> (Context -> Context)

val indentSepNlnUnindentUnlessStroustrup: f: (Expr -> Context -> Context) -> e: Expr -> ctx: Context -> Context

val autoIndentAndNlnTypeUnlessStroustrup: f: (Type -> Context -> Context) -> t: Type -> ctx: Context -> Context

val autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup:
    f: (Expr -> Context -> Context) -> e: Expr -> ctx: Context -> Context

[<NoComparison; NoEqualityAttribute>]
type ColMultilineItem = ColMultilineItem of expr: (Context -> Context) * sepNln: (Context -> Context)

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
val colWithNlnWhenItemIsMultiline: items: ColMultilineItem list -> ctx: Context -> Context
val colWithNlnWhenItemIsMultilineUsingConfig: items: ColMultilineItem list -> ctx: Context -> Context
