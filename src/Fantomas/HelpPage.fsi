module Fantomas.HelpPage

/// How much colour the page may use.
[<RequireQualifiedAccess>]
type Palette =
    | NoColour
    | FourBit
    | EightBit

/// What the terminal standard out is attached to will take. Colours are dropped when standard out
/// is redirected, so piping the page into a file or a pager yields plain text.
val detectPalette: unit -> Palette

/// The `fantomas --help` page, as the lines it is made of. Nothing is written, so a caller can
/// look at the page without a console.
val render: palette: Palette -> string list

/// Write the `fantomas --help` page to standard out.
val print: unit -> unit

/// Error handler for Argu. `--help` renders the page above, and an argument error is
/// reported on standard error, followed by a pointer to the page.
/// Argu's own generated usage text is never shown.
val exiter: Argu.IExiter
