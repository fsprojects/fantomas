module Fantomas.HelpPage

open Fantomas.Arguments
open Fantomas.Theme

/// The `--help` page for a command, as the lines it is made of. Nothing is written, so a caller can
/// look at the page without a console.
///
/// A command that names one gets a page about itself, listing only the flags it has any use for
/// and leaving out the paths section when it takes none. Which flags those are is asked of
/// `argumentsRefusedBy`, the same rule that refuses them at run time, so the page cannot come to
/// disagree with the tool.
///
/// Formatting is the command a run gets when it names none, and its page is the overview: every
/// command, every flag, and the examples.
val render: theme: Theme -> invocation: string -> command: Command -> string list

/// Write a command's `--help` page to standard out.
val print: command: Command -> unit
