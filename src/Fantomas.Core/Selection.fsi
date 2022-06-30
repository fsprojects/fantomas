module internal Fantomas.Core.Selection

open FSharp.Compiler.Text
open Fantomas.Core.FormatConfig

val formatSelection:
    config: FormatConfig -> isSignature: bool -> selection: range -> sourceText: ISourceText -> Async<string * range>
