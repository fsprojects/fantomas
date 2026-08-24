module Fantomas.EditorConfig

open Fantomas.Core

module Reflection =

    type FSharpRecordField =
        {
            PropertyName: string
            Category: string option
            DisplayName: string option
            Description: string option
        }

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
        /// them: `EditorConfigReport.readConfiguration` warns about them, the daemon sends them
        /// to its client.
        Problems: EditorConfigProblem list
    }

/// Every .editorconfig setting this build of Fantomas understands, in the order they are worth
/// reading: the settings editorconfig itself defines first, then the ones belonging to Fantomas,
/// each group ordered without regard to case. The former keep their upstream names and are not
/// prefixed, the latter all carry the `fsharp_` prefix.
val supportedSettings: string list

/// Whether a setting belongs to Fantomas rather than to editorconfig itself or to another tool.
/// Matched without regard to case, as editorconfig matches keys.
val isFantomasSetting: setting: string -> bool

/// How many single character edits turn one setting name into the other, stopping once the answer
/// is known to be above `limit`.
val editDistance: limit: int -> left: string -> right: string -> int

/// The supported setting closest to `setting`, when one is within `limit` edits of it.
val nearestSetting: limit: int -> setting: string -> string option

/// Read a `FormatConfig` from editorconfig properties, falling back to `fallbackConfig` for
/// anything the properties do not set. Keys and values are both matched without regard to case,
/// as editorconfig defines them, and when two keys fold onto one the last wins. Returns the
/// settings it could not act on alongside the configuration, each named the way it was written,
/// so the caller can decide whether and how to report them.
val parseOptionsFromEditorConfig:
    fallbackConfig: FormatConfig ->
    editorConfigProperties: System.Collections.Generic.IReadOnlyDictionary<string, string> ->
        FormatConfig * EditorConfigProblem list

val configToEditorConfig: config: FormatConfig -> string

/// Read the `.editorconfig` chain that applies to a file. `None` when no `.editorconfig` sets
/// anything for it.
///
/// Silent, and this module writes nothing anywhere: `EditorConfigReport` turns the problems into
/// something a person reads, and the daemon sends them to its client instead.
val tryReadConfiguration: fsharpFile: string -> EditorConfigResult option
