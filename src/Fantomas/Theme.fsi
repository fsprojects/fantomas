module Fantomas.Theme

/// How much colour a stream will take.
[<RequireQualifiedAccess; Struct>]
type Palette =
    | NoColour
    | FourBit
    | EightBit

/// Which characters the status column is drawn with. `Unicode` is nicer to look at and `Ascii`
/// reaches everywhere, so the fallback carries the same five states rather than dropping them.
[<RequireQualifiedAccess; Struct>]
type GlyphSet =
    | Unicode
    | Ascii

/// What a stream will take, both of it. Whether colour can be used and whether a glyph can be
/// drawn are different questions: `NO_COLOR` says nothing about UTF-8, and a console can do four
/// bit colour without it.
[<Struct>]
type Theme = { Palette: Palette; Glyphs: GlyphSet }

/// What the stream will take, given whether it is redirected. Colour is dropped when the stream
/// is redirected, so piping into a file, a pager or a build log yields plain text.
val detect: redirected: bool -> Theme

/// What standard out will take.
val forOutput: unit -> Theme

/// What standard error will take. Separate from `forOutput` because one can be redirected while
/// the other is not, which is exactly what a shell does when only one of them is captured.
val forError: unit -> Theme

/// Wrap text in a select graphic rendition sequence, choosing between the eight bit code and the
/// four bit fallback, or returning the text untouched when the stream takes no colour.
val decorate: theme: Theme -> eightBit: string -> fallback: string -> text: string -> string

/// Fantomas itself, named as the subject of a sentence.
val title: theme: Theme -> text: string -> string

/// Somewhere the reader can go: a URL, or a file and the position inside it. Most terminals make
/// both clickable, which is why they are the one role.
val link: theme: Theme -> text: string -> string

/// The structure of the page, and the number that answers the question.
val heading: theme: Theme -> text: string -> string

/// Something the reader can type: a flag, or the arguments of a command being suggested.
val flagName: theme: Theme -> text: string -> string

/// A value the reader supplies rather than types verbatim.
val placeholder: theme: Theme -> text: string -> string

/// Scaffolding that carries no information of its own: a gutter, a command name already known.
val muted: theme: Theme -> text: string -> string

/// A run that ended the way exit code 0 does.
val positive: theme: Theme -> text: string -> string

/// Something to act on that did not fail, the way exit code 99 does.
val attention: theme: Theme -> text: string -> string

/// A failure, the way exit code 1 does.
val negative: theme: Theme -> text: string -> string

/// The status column, one character per state, already coloured. Shape carries the meaning on its
/// own, so nobody has to tell red from green and the `Ascii` set loses nothing.
[<NoComparison; NoEquality>]
type StatusGlyphs =
    {
        Formatted: string
        Unchanged: string
        Ignored: string
        NeedsFormatting: string
        Errored: string
    }

val statusGlyphs: theme: Theme -> StatusGlyphs

/// How wide the text is on screen, which is its length once the escape sequences are discounted.
val visibleLength: text: string -> int

/// Write a two column row, the right hand column starting at `column` however much colour the
/// left hand one carries.
val writeRow: write: (string -> unit) -> column: int -> left: string -> right: string -> unit

/// Write a further line of the right hand column of a `writeRow`, under the first.
val writeContinuation: write: (string -> unit) -> column: int -> right: string -> unit
