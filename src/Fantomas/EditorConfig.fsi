module Fantomas.EditorConfig

module Reflection =

    type FSharpRecordField =
        { PropertyName: string
          Category: string option
          DisplayName: string option
          Description: string option }

    val inline getRecordFields: x: 'a -> (FSharpRecordField * obj)[]

val supportedProperties: string list

val toEditorConfigName: value: seq<char> -> string

val parseOptionsFromEditorConfig:
    fallbackConfig: Core.FormatConfig.FormatConfig ->
    editorConfigProperties: System.Collections.Generic.IReadOnlyDictionary<string, string> ->
        Core.FormatConfig.FormatConfig

val configToEditorConfig: config: Core.FormatConfig.FormatConfig -> string

val tryReadConfiguration: fsharpFile: string -> Core.FormatConfig.FormatConfig option

val readConfiguration: fsharpFile: string -> Core.FormatConfig.FormatConfig
