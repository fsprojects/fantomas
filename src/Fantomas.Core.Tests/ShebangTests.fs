module Fantomas.Core.Tests.ShebangTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``should keep a shebang, 2367`` () =
    let codeSnippet =
        """#!/usr/bin/env -S dotnet fsi
// random licensing stuff

open System

printfn "the best thing we've ever done"
"""

    formatSourceString false codeSnippet config |> should equal codeSnippet

[<Test>]
let ``should work if a shebang is on the first line, 2367`` () =
    let codeSnippet =
        """#!/usr/bin/env -S dotnet fsi
// random licensing stuff

open System

printfn "you can't be waiting on this"
"""

    isValidFSharpCode false codeSnippet |> should equal true

[<Test>]
let ``should fail if a shebang is not on the first line, 2367`` () =
    let codeSnippet =
        """// random licensing stuff
#!/usr/bin/env -S dotnet fsi

open System

printfn "perfect imperfection"
"""

    isValidFSharpCode false codeSnippet |> should equal false
