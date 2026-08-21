module Fantomas.HelpPage

/// Write the `fantomas --help` page to standard out.
/// Colours are emitted only when standard out is a terminal that wants them,
/// so redirecting the output yields plain text.
val print: unit -> unit

/// Error handler for Argu. `--help` renders the page above, and an argument error is
/// reported on standard error, followed by a pointer to the page.
/// Argu's own generated usage text is never shown.
val exiter: Argu.IExiter
