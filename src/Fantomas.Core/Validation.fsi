module internal Fantomas.Core.Validation

open Fantomas.FCS.Parse

/// The warning numbers Fantomas formats through. Each is something the compiler will complain
/// about but the parser has fully understood, so the tree is trustworthy: a deprecated construct,
/// a library-only construct, a reserved keyword used as an identifier, an `@` in an identifier,
/// and the interfaces-with-static-abstracts preview warning.
val safeToIgnoreWarnings: Set<int>

/// The diagnostics that make source invalid, rather than only whether any of them do: every error,
/// and every warning that is not one of the few `safeToIgnoreWarnings` numbers. Empty when there is
/// nothing among them Fantomas would refuse.
val invalidatingDiagnostics: diagnostics: FSharpParserDiagnostic list -> FSharpParserDiagnostic list

val noWarningOrErrorDiagnostics: diagnostics: FSharpParserDiagnostic list -> bool

/// Parse an input string and report what about it, if anything, Fantomas will not accept.
/// Source with conditional directives is parsed once per define combination, and the first
/// combination that fails is the one reported. Illustrated in src/Fantomas.Core.Tests/ValidationTests.fs.
val validateFSharpCode: isSignature: bool -> source: string -> Async<ValidationResult>
