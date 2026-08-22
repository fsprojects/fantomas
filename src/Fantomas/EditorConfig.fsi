module Fantomas.EditorConfig

open Serilog
open Fantomas.Core

module Reflection =

    type FSharpRecordField =
        { PropertyName: string
          Category: string option
          DisplayName: string option
          Description: string option }

    val inline getRecordFields: x: 'a -> (FSharpRecordField * obj) array

val toEditorConfigName: value: char seq -> string

/// A setting in an `.editorconfig` that Fantomas read but could not act on.
[<RequireQualifiedAccess>]
type EditorConfigProblem =
    /// A setting carrying the `fsharp_` prefix that this version of Fantomas does not have.
    | UnknownSetting of setting: string
    /// A setting Fantomas has, carrying a value it cannot parse. The default is used instead.
    | UnrecognizedValue of setting: string * value: string

/// What reading the `.editorconfig` chain for one file produced.
[<NoComparison>]
type EditorConfigResult =
    {
        Config: FormatConfig

        /// Absolute paths of the `.editorconfig` files that contributed to `Config`.
        /// Which of them a given problem came from is not knowable: editorconfig merges the whole
        /// chain into one set of properties before Fantomas sees it.
        EditorConfigFiles: string list

        /// The settings that could not be acted on. Not reported anywhere by the time you get
        /// them: `readConfiguration` warns about them, the daemon sends them to its client.
        Problems: EditorConfigProblem list
    }

/// Every .editorconfig setting this build of Fantomas understands, in the order they are worth
/// reading: the settings editorconfig itself defines first, then the ones belonging to Fantomas,
/// each group in ordinal order. The former keep their upstream names and are not prefixed, the
/// latter all carry the `fsharp_` prefix.
val supportedSettings: string list

/// Read a `FormatConfig` from editorconfig properties, falling back to `fallbackConfig` for
/// anything the properties do not set. Keys are matched without regard to case, and when two of
/// them fold onto one the last wins, as editorconfig does. Returns the settings it could not act
/// on alongside the configuration, so the caller can decide whether and how to report them.
val parseOptionsFromEditorConfig:
    fallbackConfig: FormatConfig ->
    editorConfigProperties: System.Collections.Generic.IReadOnlyDictionary<string, string> ->
        FormatConfig * EditorConfigProblem list

val configToEditorConfig: config: FormatConfig -> string

/// Read the `.editorconfig` chain that applies to a file. `None` when no `.editorconfig` sets
/// anything for it.
///
/// Silent: the problems are returned rather than reported. `readConfiguration` warns about them;
/// the daemon sends them to its client instead and must not write them to standard error, which
/// `Fantomas.Client` redirects and never drains.
val tryReadConfiguration: fsharpFile: string -> EditorConfigResult option

/// Read the `.editorconfig` that applies to a file and warn through `log` about anything in it
/// Fantomas cannot use. The daemon does not go through here: it sends the problems to its client
/// instead, and must not write them to standard error.
val readConfiguration: log: ILogger -> fsharpFile: string -> FormatConfig
