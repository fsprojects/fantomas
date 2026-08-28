module FantomasClientTests.VersionTests

open NUnit.Framework
open Fantomas.Client.FantomasToolLocator
open Fantomas.Client.LSPFantomasServiceTypes

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
    Assert.That(string<FantomasVersion>(FantomasVersion.Create printed), Is.EqualTo expected)

// Fantomas added `fantomas daemon` beside `fantomas --daemon` in 8.0.0-alpha-016. Getting this the
// wrong way round does not fail loudly: the client would ask an older Fantomas for a subcommand it
// does not have, be refused, and report that no daemon could be started.
[<TestCase("8.0.0-alpha-016")>]
[<TestCase("8.0.0-alpha-017")>]
// Zero padded to three digits, so these sort the way they read. Semver compares a non numeric
// prerelease identifier character by character, and `alpha-9` would sort above `alpha-10`.
[<TestCase("8.0.0-alpha-099")>]
[<TestCase("8.0.0-alpha-100")>]
[<TestCase("8.0.0-beta-001")>]
[<TestCase("8.0.0-rc-001")>]
[<TestCase("8.0.0")>]
[<TestCase("8.1.0")>]
[<TestCase("9.0.0-beta1")>]
[<TestCase("9.0.0")>]
let ``the release the subcommand landed in, and everything after, asks by command`` (version: string) =
    Assert.That(daemonArgument (FantomasVersion version), Is.EqualTo "daemon")

[<TestCase("4.6.0")>]
[<TestCase("7.0.5")>]
// The alphas before the one it landed in have the flag and nothing else.
[<TestCase("8.0.0-alpha-014")>]
[<TestCase("8.0.0-alpha-015")>]
// A version that cannot be read is not a reason to guess: the flag works on every Fantomas there
// has ever been, including the newest.
[<TestCase("not a version")>]
[<TestCase("")>]
let ``anything older or unreadable asks for the daemon by its flag`` (version: string) =
    Assert.That(daemonArgument (FantomasVersion version), Is.EqualTo "--daemon")
