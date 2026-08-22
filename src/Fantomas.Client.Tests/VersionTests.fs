module FantomasClientTests.VersionTests

open NUnit.Framework
open Fantomas.Client.FantomasToolLocator

// Daemons are cached by version string. `fantomas --version` and `dotnet tool list` say the same
// version two different ways, and while they did, the same Fantomas resolved from a tool manifest
// and from the PATH counted as two versions and got two processes.
[<TestCase("Fantomas v8.0.0", "8.0.0")>]
[<TestCase("Fantomas v8.0.0\n", "8.0.0")>]
[<TestCase("Fantomas v8.0.0-alpha-014+4de2c5cf57b9e36bf012c510283e4ce10483811e", "8.0.0-alpha-014")>]
[<TestCase("fantomas 7.0.5", "7.0.5")>]
[<TestCase("8.0.0", "8.0.0")>]
// Folded, because `CompatibleTool` folds the manifest side too. One casing has to win
// and the two producers have to pick the same one.
[<TestCase("Fantomas v8.0.0-Alpha-014", "8.0.0-alpha-014")>]
let ``a printed version reads as the version a tool manifest names`` (printed: string, expected: string) =
    Assert.That(normalizeVersion printed, Is.EqualTo expected)
