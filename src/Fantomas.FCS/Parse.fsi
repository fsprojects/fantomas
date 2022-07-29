module Fantomas.FCS.Parse

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type FSharpParserDiagnostic =
    { Severity: FSharpDiagnosticSeverity
      SubCategory: string
      Range: range option
      ErrorNumber: int option
      Message: string }

val parseFile:
    isSignature: bool -> sourceText: ISourceText -> defines: string list -> ParsedInput * FSharpParserDiagnostic list
