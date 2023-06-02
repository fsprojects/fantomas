module internal Fantomas.Core.Selection

open Fantomas.FCS.Text

val formatSelection:
    config: FormatConfig -> isSignature: bool -> selection: range -> sourceText: ISourceText -> Async<string * range>
