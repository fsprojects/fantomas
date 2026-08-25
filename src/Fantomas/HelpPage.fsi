module Fantomas.HelpPage

open Fantomas.Theme

/// The `fantomas --help` page, as the lines it is made of. Nothing is written, so a caller can
/// look at the page without a console.
val render: theme: Theme -> string list

/// Write the `fantomas --help` page to standard out.
val print: unit -> unit

/// The line of Argu's message that carries the actual complaint. Argu builds a usage block of its
/// own and hands it over as part of the message; only the first line says what went wrong.
val complaint: message: string -> string

/// Error handler for Argu. `--help` renders the page above, and an argument error is
/// reported on standard error, followed by a pointer to the page.
/// Argu's own generated usage text is never shown.
val exiter: Argu.IExiter
