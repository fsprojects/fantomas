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

/// The supported setting closest to `setting`, when one is within `limit` edits of it. Two
/// candidates the same distance away are separated by the order of `supportedSettings`.
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

/// One setting Fantomas has, the value a given file will be formatted with, and where that value
/// came from.
type ResolvedSetting =
    {
        /// The name the setting is written under in an `.editorconfig`.
        Setting: string
        /// The value, spelled the way an `.editorconfig` would carry it.
        Value: string
        /// The absolute path of the `.editorconfig` that set it, or `None` when nothing set it and
        /// the value is the Fantomas default.
        ///
        /// A setting written with a value Fantomas cannot read is `None` as well: the default is
        /// what will be used, so naming the file that wrote it would say the value came from
        /// somewhere it did not. `Problems` is where that is reported.
        SetBy: string option
    }

/// The whole configuration one file will be formatted with, taken apart.
///
/// `tryReadConfiguration` answers what a format run needs, which is the configuration alone. This
/// answers what `doctor` needs, which is the same configuration with each setting's origin
/// attached, and works that out by reading the `.editorconfig` chain one file longer at a time and
/// looking at what each addition changed.
[<NoComparison>]
type ResolvedConfig =
    {
        Config: FormatConfig
        /// Every setting Fantomas has, in the order `supportedSettings` lists them.
        Settings: ResolvedSetting list
        /// The `.editorconfig` files that were read, furthest from the file first, which is the
        /// order they are applied in and so the order in which a later one overrules an earlier.
        EditorConfigFiles: string list
        Problems: EditorConfigProblem list
    }

    /// The settings an `.editorconfig` set, which is the short answer to what makes this file
    /// format differently from one with no configuration around it.
    member FromEditorConfig: ResolvedSetting list

/// Read the `.editorconfig` chain that applies to a file and say where each setting's value came
/// from. Reads the disk, as everything in this module that resolves a chain does.
val resolveConfiguration: fsharpFile: string -> ResolvedConfig

/// A configuration with nothing behind it: every setting at the value it holds, and no
/// `.editorconfig` named as having set any of them. For a caller that has a `FormatConfig` in hand
/// and no chain on disk to resolve, which is what a daemon client and a test both are.
val withoutEditorConfig: config: FormatConfig -> ResolvedConfig

/// Read the `.editorconfig` chain that applies to a file. `None` when no `.editorconfig` sets
/// anything for it.
///
/// Silent, and this module writes nothing anywhere: `EditorConfigReport` turns the problems into
/// something a person reads, and the daemon sends them to its client instead.
val tryReadConfiguration: fsharpFile: string -> EditorConfigResult option
