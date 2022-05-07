module Fantomas.Core.Tests.KeepMaxEmptyLines

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core.FormatConfig

let config x = { config with KeepMaxBlankLines = x }

[<Test>]
let ``reduce 2 empty lines to 1`` () =
    checkFormat (config 1)
        """
open Foo


let x = 42
"""
        """
open Foo

let x = 42
"""

[<Test>]
let ``reduce 3 empty lines to 2`` () =
    checkFormat (config 2)
        """
open Foo



let x = 42
"""
        """
open Foo


let x = 42
"""

[<Test>]
let ``reduce 3 empty lines to 1`` () =
    checkFormat (config 1)
        """
open Foo



let x = 42
"""
        """
open Foo

let x = 42
"""


[<Test>]
let ``only generated empty lines`` () =
    checkFormat (config 0)
        """
open Foo

open Goo

module M1 =

    let x = 42
module M2 = let y = 42
"""
        """
open Foo
open Goo

module M1 =
    let x = 42

module M2 =
    let y = 42
"""

[<Test>]
let ``dont reduce empty lines in string`` () =
    checkFormat (config 1)
        "
let x = \"\"\"


\"\"\"
"
        "
let x =
    \"\"\"


\"\"\"
"
