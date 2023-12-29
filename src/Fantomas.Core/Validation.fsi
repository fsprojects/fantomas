module internal Fantomas.Core.Validation

open FSharp.Compiler.Diagnostics

val noWarningOrErrorDiagnostics: diagnostics: FSharpDiagnostic list -> bool
/// Check whether an input string is invalid in F# by looking for errors and warnings in the diagnostics.
val isValidFSharpCode: isSignature: bool -> source: string -> Async<bool>
