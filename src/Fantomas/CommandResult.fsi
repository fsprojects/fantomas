module Fantomas.CommandResult

open Fantomas.FCS.Parse

/// What formatting one file came to.
[<RequireQualifiedAccess; NoComparison>]
type FormatResult =
    | Formatted of filename: string * formattedContent: string
    | Unchanged of filename: string
    | InvalidCode of filename: string * formattedContent: string * diagnostics: FSharpParserDiagnostic list
    | Error of filename: string * formattingError: exn
    | IgnoredFile of filename: string

/// What a `--check` run found. `Formatted` names the files that would change, which is the question
/// the command was asked. `Unchanged` names the ones that would not, which nothing branches on, and
/// is carried so that a report can name every file the run looked at rather than only the ones it
/// has a complaint about.
[<NoComparison>]
type CheckResult =
    {
        Errors: (string * exn) list
        Formatted: string list
        Unchanged: string list
    }

    member HasErrors: bool

    member IsValid: bool

    member NeedsFormatting: bool

/// The failure that a `FormatResult.InvalidCode` stands for. Formatting produced output that
/// Fantomas itself would not accept, which is a bug in Fantomas rather than in the file it was
/// given, and nothing was written.
///
/// It is a type of its own so that a reporter can recognise it, since what it has to say does not
/// fit the one line that every other failure is reduced to. `Diagnostics.invalidOutputExplanation`
/// is the wording, and is what this carries as its message.
type InvalidCodeException =
    inherit Fantomas.Core.FormatException

    new: formattedContent: string * diagnostics: FSharpParserDiagnostic list -> InvalidCodeException

    /// The diagnostics that made that output unacceptable: every error, and every warning Fantomas
    /// does not tolerate. A diagnostic it was willing to overlook is not among them.
    member Diagnostics: FSharpParserDiagnostic list

    /// What Fantomas produced and then would not accept. It is written nowhere, so a report that
    /// wants to show a line of it has nowhere else to read it from.
    member FormattedContent: string

/// A reason the input paths cannot be worked with. Both commands can end this way and both are
/// described from here, which is what keeps their wording from drifting apart.
[<RequireQualifiedAccess; Struct>]
type InputProblem =
    | UnsupportedFileType of path: string
    | NotFound of path: string
    | MultiplePathsWithOut

/// What a format run did. Turning this into text and into an exit code is `Report`'s job, so that
/// a caller can ask what happened without reading the console.
[<RequireQualifiedAccess; NoComparison>]
type FormatCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of results: FormatResult array
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

    /// The exit code the process should end with: 1 when anything failed, 0 otherwise.
    ///
    /// This belongs to the result rather than to a reporter because there is more than one
    /// reporter, and what the process ends with cannot depend on which one printed.
    member ExitCode: int

/// What a `--check` run did. The ignored files are carried alongside, since a run that ignored
/// everything is not the same as a run that found nothing to do.
[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of ignored: string list * result: CheckResult
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

    /// The exit code the process should end with: 0 when every file is already formatted, 99 when
    /// at least one needs formatting, and 1 when something failed. The two failure codes are
    /// distinct on purpose: a pipeline wants to tell "please run the formatter" apart from "this
    /// did not work".
    member ExitCode: int
