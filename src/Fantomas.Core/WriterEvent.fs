namespace Fantomas.Core

/// Represents a single event emitted during the code formatting process.
/// The sequence of writer events captures how the formatter produces its output.
[<Struct>]
type WriterEvent =
    /// Append literal text to the current line.
    | Write of text: string
    /// Emit text that originated from trivia (comments, XML doc lines, or compiler directives).
    /// Behaves identically to Write in dump output, but allows the formatting engine to recognise
    /// trivia events without fragile string-prefix checks (e.g. "starts with //").
    | WriteTrivia of text: string
    /// End the current line and start a new one at the current indentation level.
    | WriteLine
    /// Newline inside a multiline string constant — no indentation is applied.
    | WriteLineInsideStringConst
    /// Queue text to be appended just before the next newline (e.g. trailing line comments).
    | WriteBeforeNewline of text: string
    /// Newline introduced by trivia (comments, directives) rather than by the formatter itself.
    /// Distinguished from WriteLine so colWithNlnWhenItemIsMultiline can ignore trivia-induced newlines.
    | WriteLineBecauseOfTrivia
    /// Newline inside a trivia block (e.g. inside a block comment or directive).
    | WriteLineInsideTrivia
    /// Increase indentation by the given number of spaces. Takes effect on the next newline.
    | IndentBy of amount: int
    /// Decrease indentation by the given number of spaces.
    | UnIndentBy of amount: int
    /// Set indentation to an absolute column position.
    | SetIndent of column: int
    /// Restore indentation to a previously saved value.
    | RestoreIndent of column: int
    /// Set the AtColumn value — the minimum indentation floor for subsequent newlines.
    | SetAtColumn of column: int
    /// Restore AtColumn to a previously saved value.
    | RestoreAtColumn of column: int
    /// Diagnostic marker: beginning of an Oak node. Only emitted when DebugMode is enabled.
    | NodeStart of nodeType: string * range: string
    /// Diagnostic marker: end of an Oak node. Only emitted when DebugMode is enabled.
    | NodeEnd of nodeType: string * range: string
    /// Marks the beginning of a colWithNlnWhenItemIsMultiline block.
    /// No-op during WriterModel.update and dump — used only as a DLL position marker.
    | Start
    /// Placeholder separator between items in a colWithNlnWhenItemIsMultiline block.
    /// After all items are emitted, each Placeholder is inspected to determine if the item
    /// that follows it was multiline, and then replaced with the appropriate separator.
    /// No-op during WriterModel.update and dump.
    | Placeholder
