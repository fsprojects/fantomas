module internal Fantomas.Core.Validation

open Fantomas.FCS.Parse

/// The diagnostics that make source invalid, rather than only whether any of them do: every error,
/// and every warning that is not one of the few `safeToIgnoreWarnings` names. Empty when there is
/// nothing among them Fantomas would refuse.
val invalidatingDiagnostics: diagnostics: FSharpParserDiagnostic list -> FSharpParserDiagnostic list

val noWarningOrErrorDiagnostics: diagnostics: FSharpParserDiagnostic list -> bool

/// Parse an input string and report what about it, if anything, Fantomas will not accept.
val validateFSharpCode: isSignature: bool -> source: string -> Async<ValidationResult>
