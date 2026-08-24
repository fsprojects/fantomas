module Fantomas.Analyzers.Common

open FSharp.Compiler.Syntax

/// Whether the file being analyzed has a signature file in the same project.
///
/// The project's own source list answers this rather than a look at the filesystem. An `.fsi` that
/// is not compiled says nothing about what is visible, and the paths a project reports are already
/// the ones the compiler used.
val hasSignatureFile: fileName: string -> sourceFiles: string list -> bool

/// Whether a binding is a test, judged by the attributes it carries.
///
/// Keying on the attribute rather than on the project is what makes this hold up: there is no list
/// of test projects to keep in step, a test helper living in a product project is treated the same
/// way, and an ordinary binding in a test project is still held to the rule. The name is matched on
/// its final identifier with any `Attribute` suffix removed, so `Test`, `TestAttribute` and
/// `NUnit.Framework.Test` all count as the same thing.
val isTest: attributes: SynAttributes -> bool
