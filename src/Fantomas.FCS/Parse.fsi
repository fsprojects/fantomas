module Fantomas.FCS.Parse

open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text

type FSharpParserDiagnostic =
    { Severity: FSharpDiagnosticSeverity
      SubCategory: string
      Range: range option
      ErrorNumber: int option
      Message: string }

val parseFile:
    isSignature: bool -> sourceText: ISourceText -> defines: string list -> ParsedInput * FSharpParserDiagnostic list
