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

/// Every .editorconfig setting this build of Fantomas understands, in the order they are worth
/// reading: the settings editorconfig itself defines first, then the ones belonging to Fantomas,
/// each group ordered without regard to case. The former keep their upstream names and are not
/// prefixed, the latter all carry the `fsharp_` prefix.
val supportedSettings: string list

/// Whether a setting belongs to Fantomas rather than to editorconfig itself or to another tool.
/// Matched without regard to case, as editorconfig matches keys.
val isFantomasSetting: setting: string -> bool

/// Whether a value is one the editorconfig spec gives a meaning that is not a value: `unset`,
/// `indent_size = tab`, `max_line_length = off`. Fantomas keeps its default for them without
/// reporting a problem.
val isSpecDefinedNonValue: setting: string -> value: string -> bool

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

/// Every setting of a configuration, under the name it is written with and with its value
/// spelled the way an `.editorconfig` would carry it, in record field order.
val settingValues: config: FormatConfig -> (string * string) list

val configToEditorConfig: config: FormatConfig -> string
