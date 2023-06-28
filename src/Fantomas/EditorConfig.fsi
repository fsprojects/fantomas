module Fantomas.EditorConfig

open Fantomas.Core

module Reflection =

    type FSharpRecordField =
        { PropertyName: string
          Category: string option
          DisplayName: string option
          Description: string option }

    val inline getRecordFields: x: 'a -> (FSharpRecordField * obj)[]

val toEditorConfigName: value: seq<char> -> string

val parseOptionsFromEditorConfig:
    fallbackConfig: FormatConfig ->
    editorConfigProperties: System.Collections.Generic.IReadOnlyDictionary<string, string> ->
        FormatConfig

val configToEditorConfig: config: FormatConfig -> string

val tryReadConfiguration: fsharpFile: string -> FormatConfig option

val readConfiguration: fsharpFile: string -> FormatConfig
