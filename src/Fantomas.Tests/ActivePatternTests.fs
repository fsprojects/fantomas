module Fantomas.Tests.ActivePatternTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should keep parens around active patterns``() =
    formatSourceString false """let (|Boolean|_|) = Boolean.parse
    """ config
    |> should equal """let (|Boolean|_|) = Boolean.parse
"""

[<Test>]
let ``should keep parens around active patterns in module``() =
    formatSourceString false """module Interpreted =
    let (|Match|_|) = (|Match|_|) RegexOptions.None
    """ config
    |> should equal """module Interpreted = 
    let (|Match|_|) = (|Match|_|) RegexOptions.None
"""

[<Test>]
let ``should keep parens around active patterns in inlined functions``() =
    formatSourceString false """let inline (|Match|_|) x = tryMatchWithOptions x
    """ config
    |> should equal """let inline (|Match|_|) x = tryMatchWithOptions x
"""