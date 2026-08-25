module Fantomas.EditorConfigReport

open Serilog
open Fantomas.Core
open Fantomas.EditorConfig

/// Writes what Fantomas could not use out of the `.editorconfig` chain named by the first
/// argument. Each reporter keeps its own record of what it has already written.
type EditorConfigReporter = string -> EditorConfigProblem list -> unit

/// The running version without the commit hash `CodeFormatter.GetVersion` appends, so that the
/// settings a user is pointed at are tied to a version they can act on.
val fantomasVersion: string

/// How far a setting may be from a supported one before the guess is worse than silence. Three
/// edits is roughly a doubled letter, a dropped one and a swapped pair; beyond that naming the
/// other setting is noise rather than help.
///
/// Deliberately looser than the distance at which an unprefixed key is read as a misspelling at
/// all. By the time a suggestion is offered the setting is already known to be a mistake, so a
/// slightly wilder guess costs nothing.
[<Literal>]
val MaximumSuggestionDistance: int = 3

/// The supported setting closest to one Fantomas does not have, when there is a close one.
/// A prefixed spelling of a setting editorconfig itself defines, `fsharp_max_line_length` for
/// `max_line_length`, is answered outright; anything else has to be within a few characters.
val suggestionFor: setting: string -> string option

/// One thing Fantomas could not use, in a sentence, and where a misspelling has an obvious intent,
/// what it looks like it was meant to be. Carries no indentation of its own, so a caller can place
/// it in whatever it is writing.
///
/// Settings and values are quoted the way the rest of the tool quotes what it was given, and for
/// the same reason: both are text someone else wrote. A value can be empty, or carry spaces, or
/// read like prose, and unquoted it runs into the sentence around it.
val describeProblem: problem: EditorConfigProblem -> string

/// What to tell someone about the settings Fantomas could not use out of `origin`, or `None`
/// when there is nothing to tell them. Names every problem, and where a misspelling has an
/// obvious intent, names that too.
///
/// Deliberately does not list the settings Fantomas does support. That list is long, it is the
/// same list every time, and it answers nothing for a setting whose name was right and whose
/// value was not; `describeSupportedSettings` is there for the run that asks for it.
val describe: origin: string -> problems: EditorConfigProblem list -> string option

/// Every setting this build understands, as a block to write once per run.
val describeSupportedSettings: unit -> string

/// A reporter over `log`. Writes each distinct report once, however many files are formatted:
/// the command line reads the same `.editorconfig` again for every one of them, and without
/// this a single typo would be reported once per file. At debug verbosity the first report it
/// writes is followed by the supported settings, once per run; a run with nothing to report says
/// nothing at all.
val createReporter: log: ILogger -> EditorConfigReporter

/// Read the `.editorconfig` that applies to a file, reporting anything in it Fantomas cannot use.
/// The daemon does not go through here: it sends the problems to its client instead, and must not
/// write them to standard error, which `Fantomas.Client` redirects.
val readConfiguration: report: EditorConfigReporter -> fsharpFile: string -> FormatConfig
