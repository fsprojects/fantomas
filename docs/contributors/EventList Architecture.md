# Writer Events and the EventList

## Overview

Fantomas formats code in two phases:

0 **Event generation**: The code printer traverses the Oak tree and appends `WriterEvent` values to an `EventList` — a mutable doubly-linked list. During this phase, only lightweight metadata is tracked (line count, column, indent level). No strings are built.

1 **String materialization**: The `dump` function walks the EventList head-to-tail with a `StringBuilder`, producing the final formatted string.

## EventList

`EventList` (`EventList.fs`) is a mutable doubly-linked list of `EventNode` values. Each node holds a `WriterEvent` and pointers to `Prev`/`Next`.

Key operations:

Operation | Complexity | Used for
--- | --- | ---
`Append` | O(1) | Adding events during formatting
`InsertBefore` / `InsertAfter` | O(1) | Splicing indent/unindent before trivia
`Remove` | O(1) | Removing events (e.g. trailing newline in `addFinalNewline`)
`CreateBackupPoint` | O(1) | Saving the tail position before speculative formatting
`RollbackTo` | O(1) | Discarding events appended after a backup point
`ToSeq` / `ToRevSeq` | O(n) | Iterating forward/backward for inspection
`CurrentLineContent` | O(k) | Walking backward to collect text on the current line


`EventNode` uses `[<AllowNullLiteral>]` instead of `option` for `Prev`/`Next` links because this is a hot path — every formatting operation appends nodes.

## WriterEvent cases

Event | Purpose
--- | ---
`Write` | Append literal code text
`WriteTrivia` | Append trivia text (comments, directives, XML docs). Same as `Write` in output, but allows the engine to distinguish trivia from code without string-prefix checks
`WriteLine` | End current line, start new line at current indentation
`WriteLineBecauseOfTrivia` | Newline introduced by trivia. Distinguished from `WriteLine` so multiline detection can ignore trivia-induced newlines
`WriteLineInsideStringConst` | Raw newline inside a multiline string — no indentation applied
`WriteLineInsideTrivia` | Raw newline inside a trivia block (e.g. block comment)
`WriteBeforeNewline` | Queue text to appear just before the next newline (trailing line comments)
`IndentBy` / `UnIndentBy` | Adjust indentation level. Takes effect on the next newline
`SetIndent` / `RestoreIndent` | Absolute indent control
`SetAtColumn` / `RestoreAtColumn` | Indentation floor (`atCurrentColumn`)
`Start` / `Placeholder` | Position markers for future `colWithNlnWhenItemIsMultiline` rework


## Speculative formatting

Several functions try a short layout and fall back to a long one:

```
CreateBackupPoint  →  try short layout  →  fits?  →  keep events
                                            ↓
                                        RollbackTo  →  run long layout
```

* `expressionFitsOnRestOfLine` / `isShortExpression`: Uses `ShortExpression` mode to detect overflow

* `expressionExceedsPageWidth`: Same, with `LongExpressionLayout` DU for the long path

* `colWithNlnWhenItemIsMultiline`: Optimistic blank-line separator, rolls back if both items are single-line

* `WithDummy`: Encapsulates probe functions — creates backup, runs probe, reads metadata, rolls back automatically

## Trivia-aware indentation

`indentSepNlnUnindent` is the most common formatting pattern (66+ call sites). It indents, adds a newline, runs the content, then unindents:

```
indent +> sepNln +> content +> unindent
```

Both sides are trivia-aware:

* ***`indentSepNlnWithTriviaAwareness`***: If trailing trivia exists before the indent point, splices `IndentBy` before the trivia block so the comment appears at the indented level. The trivia's own newline replaces `sepNln`.

* ***`unindentWithTriviaAwareness`***: If trailing trivia exists after the content, splices `UnIndentBy` before the trailing trivia newline so the newline uses the reduced indent level.

Both use `findTrailingTriviaNewline` which walks backward from the DLL tail, skipping `RestoreIndent`/`RestoreAtColumn`/`UnIndentBy`/`IndentBy`/`WriteLine` events, then verifies a `WriteLineBecauseOfTrivia` preceded by `WriteTrivia`.

## LongExpressionLayout

The `LongExpressionLayout` DU describes how to lay out an expression that doesn't fit on one line:

```fsharp
type LongExpressionLayout =
    | IndentAndUnindent          // indent +> sepNln +> expr +> unindent
    | DoubleIndentAndUnindent    // indent +> indent +> sepNln +> expr +> unindent +> unindent
    | NewlineOnly                // sepNln +> expr
```

`expressionExceedsPageWidthWithLayout` translates the DU to before/after functions, with `unindentWithTriviaAwareness` on the trailing side for indent layouts.

The wrapper functions `autoIndentAndNlnIfExpressionExceedsPageWidth`, `sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth`, etc. all delegate to this.

## WriterModel

`WriterModel` tracks formatting metadata without building strings:

```fsharp
{ LineCount: int          // number of lines produced
  Column: int             // current position on the line
  Indent: int             // current indentation level
  AtColumn: int           // indentation floor (from atCurrentColumn)
  WriteBeforeNewline: string
  Mode: WriteModelMode }  // Standard, Dummy, or ShortExpression
```

`WriterModel.update` processes each event and updates these fields. The same function is used both during normal formatting and when splicing events (to keep the model in sync after an `InsertBefore`).
