module Fantomas.HelpPage

open Fantomas.Theme

/// The `fantomas --help` page, as the lines it is made of. Nothing is written, so a caller can
/// look at the page without a console.
val render: theme: Theme -> string list

/// Write the `fantomas --help` page to standard out.
val print: unit -> unit
